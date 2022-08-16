#Install packages
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)

#Read new condo price data
data_dir <- "C:/Users/User/Desktop/Data Science/Impact of COVID-19 on New Condo Prices in Canada/New Condo Price Data (Cleaned).csv"
condo_data <- read.csv(file=data_dir, row.names=1)
#Get vectors of quarters and locations
quarters <- colnames(condo_data)
locations <- rownames(condo_data)
#Convert the data in long format
condo_data <- data.frame(t(condo_data))

#Convert quarters into days
days <- c()
for (quarter in quarters) {
  if (substr(quarter,1,2) == "Q1"){
    day <- as.Date(paste("01-01-",substr(quarter,4,7),sep=""), format="%d-%m-%Y")
  }
  else if (substr(quarter,1,2) == "Q2"){
    day <- as.Date(paste("01-04-",substr(quarter,4,7),sep=""), format="%d-%m-%Y")
  }
  else if (substr(quarter,1,2) == "Q3"){
    day <- as.Date(paste("01-07-",substr(quarter,4,7),sep=""), format="%d-%m-%Y")
  }
  else if (substr(quarter,1,2) == "Q4"){
    day <- as.Date(paste("01-10-",substr(quarter,4,7),sep=""), format="%d-%m-%Y")
  }
  days <- c(days, day)
}
days <- days - days[1]
rownames(condo_data) <- days

#Get number of days of outbreak since 01-01-2017
outbreak_quarter <- "Q1.2020"
outbreak_day <- as.Date("01-01-2020",format="%d-%m-%Y") - as.Date("01-01-2017",format="%d-%m-%Y")
outbreak_index <- which(quarters==outbreak_quarter)

#Plot scatter plots to explore the relationships between condo price and time in each location
for (i in 1:length(locations)){
  print(
    ggplot(condo_data, aes(x=days,y=condo_data[,i]))+
      labs(x="",y="Price Index", title=paste("New Condo Price Trend in",locations[i]))+
      geom_point()+
      geom_smooth()+
      scale_x_continuous(breaks=days,labels=quarters)+
      ylim(min(condo_data), max(condo_data))+
      theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1))+
      geom_vline(xintercept=outbreak_day,linetype="dotted",color="red",size=1.5)+
      annotate(geom="text", x=outbreak_day-200, y=145, label="Outbreak of COVID-19", color="red")
    )
  ggsave(filename=paste(locations[i],".png",sep=""), width=10, height=5)
}

#Perform linear regressions on prices before and after the outbreak of COVID-19 (Q1.2020)
slope_before <- c()
slope_after <- c()
slope_change <- c()
r_sq_before <- c()
r_sq_after <- c()
r_sq_change <- c()
for (i in 1:length(locations)){
  LR_before <- lm(formula=condo_data[1:outbreak_index-1,i]~days[1:outbreak_index-1])
  LR_after <- lm(formula=condo_data[-outbreak_index+1:0,i]~days[-outbreak_index+1:0])
  slope_before <- c(slope_before, summary(LR_before)$coefficient[2,1]*30)
  slope_after <- c(slope_after, summary(LR_after)$coefficient[2,1]*30)
  slope_change <- c(slope_change, tail(slope_after,1)-tail(slope_before,1))
  r_sq_before <- c(r_sq_before, summary(LR_before)$adj.r.squared)
  r_sq_after <- c(r_sq_after, summary(LR_after)$adj.r.squared)
  r_sq_change <- c(r_sq_change, tail(r_sq_after,1)-tail(r_sq_before,1))
}

#Export the results
results <- data.frame(slope_change, r_sq_change, slope_before, r_sq_before, slope_after, r_sq_after, row.names=locations)
colnames(results) <- c('Change in Monthly Index Change', 'Change in R squared','Monthly Index Change (before)', 'R squared (before)', 'Monthly Index Change (after)', 'R squared (after)')
write.csv(results,"../Price Trends.csv", row.names = TRUE)