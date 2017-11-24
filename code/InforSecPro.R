#Author:ATM Golam Bari (bari@mail.usf.edu)
#Created: 11/24/2017

##********************####
# Two packages - openxlsx for reading xlsx files and 
# lubridate for week number extraction - need to be 
# installed at first using the following two instructions

# install.packages("openxlsx")
# library(openxlsx)

# install.packages("lubridate")
# library(lubridate)

# To run the code please follow
# Step 1: source("infoSecPro.R"): compiles the source files
# Step 2: summarize(): write monthly data as summarizedData.data in
# current directory. This is a data matrix of week by user dimension.
# Each week has a fixed samples based on time window. Say time window
# 24 hours, then each week has only 5 samples. So, the total size of 
# the data matrix is #row = 4 weeks * 5 samples for each week = 20,
# #columns = 54.
# Step 3: doStat(): calculates average number of matches for a given
# time frame eg. between week one and week two

##********************####


## Create monthly data for a user
## Monthly data is stored weekly basis
## Weekly samples depends on time window (fractionInADay)

dataChunk <- function(myData) {
  
  weekInAMonth <- 4
  dayInAWeek <- 5
  fractionInADay <- 8
  monthlyData <- list()
  
  for (weekNumber in 1:weekInAMonth) {

    data <- vector(mode = "numeric", length = dayInAWeek * fractionInADay)
    myWeeklyData <- myData[which(myData$WN == weekNumber),]

    if (!is.null(myWeeklyData)) {
    
        for (dayNumber in 1:dayInAWeek) {
      
            myDailyData <- myWeeklyData[which(myWeeklyData$DN == dayNumber),]
        
            if (!is.null(myDailyData)){ 
              
              firstQuarter <- myDailyData[which(myDailyData$HR <=2),]
              secondQuarter <- myDailyData[which(myDailyData$HR >2 & myDailyData$HR <= 5),]
              thirdQuarter <- myDailyData[which(myDailyData$HR >5 & myDailyData$HR <= 8),]
              fourthQuarter <- myDailyData[which(myDailyData$HR >8 & myDailyData$HR <= 11),]
              fifthQuarter <- myDailyData[which(myDailyData$HR >11 & myDailyData$HR <= 14),]
              sixthQuarter <- myDailyData[which(myDailyData$HR >14 & myDailyData$HR <= 17),]
              seventhQuarter <- myDailyData[which(myDailyData$HR >17 & myDailyData$HR <= 20),]
              eighthQuarter <- myDailyData[which(myDailyData$HR >20 & myDailyData$HR <= 23),]
              
              data[(dayNumber - 1)* fractionInADay + 1] <- sum(firstQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 2] <- sum(secondQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 3] <- sum(thirdQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 4] <- sum(fourthQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 5] <- sum(fifthQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 6] <- sum(sixthQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 7] <- sum(seventhQuarter$octPerUnit)
              data[(dayNumber - 1)* fractionInADay + 8] <- sum(eighthQuarter$octPerUnit)
            }
        }
    }
    monthlyData[[weekNumber]] <- data
    #cat("week#: ", weekNumber, "data: ", data, "\n")
    
  }
  return(monthlyData)
}

## discard un-necessary attributes from user-data file
## creates new attributes eg. dayNumber, weekNumber, hours etc.
## call dataChunk and return it to summarize()

dataPreprocessing <- function(fileName) {
  
    flowData <- read.xlsx(fileName, cols = c(4, 6:7, 10), sheet = 1, startRow = 2, colNames = F, rowNames = F)
    colnames(flowData) <- c("octet", "start", "end", "duration")

    flowData <- flowData[!(flowData$duration==0), ] #Excluding row with duration = 0
    flowData$octPerUnit <- flowData$octet/flowData$duration #calculate internet usage data
  
    flowData$start <- 	as.POSIXct(flowData$start/1000, origin="1970-01-01", tz="EST")
    flowData$end <- 	as.POSIXct(flowData$end/1000,  origin="1970-01-01", tz="EST")
    
    flowData$DN <- as.numeric(strftime(as.Date(flowData$end), "%u")) #week days as decimal (1-7, Monday is 1) 
    flowData$WN <- ceiling(day(as.Date(flowData$end))/7)  # weeks in decimals (1, 2, 3 and 4)
    flowData$HR <- as.numeric(strftime(flowData$end, "%H")) #Hours as decimal (0-23) 
    
    aUserData <- dataChunk(flowData)

    uData <- unlist(aUserData)
    return(uData)
}

## Read all the xlsx files and summarize the data matrix.
## Please read the description of summarizedData.data at the beginning

summarize <- function() {
  allFiles <- list.files(pattern = "*.xlsx")
  userData <- list()
  for (i in 1:length(allFiles)) {
    userData[[i]] <- dataPreprocessing(allFiles[i])
  }
  df <- as.data.frame(userData)
  write.table(df , file = "summarizedData.data", row.names = F, col.names = F, sep = " ")
}


## Calculates Z value from the euation given in projects readme file

calcZ <- function(r1a2a, r1a2b, r2a2b, N) {
  rmSquare <- ((r1a2a * r1a2a) + (r1a2b * r1a2b)) * 0.5
  f <- (1 - r2a2b)/(2*(1-rmSquare))
  h <- (1 - f*rmSquare)/(1 - rmSquare)
  z1a2a <- 0.5*log2((1 + r1a2a)/(1 - r1a2a))
  z1a2b <- 0.5*log2((1 + r1a2b)/(1 - r1a2b))
  return((z1a2a - z1a2b)* (sqrt(N - 3)/(2*h*(1 - r2a2b))))
}
## Creates three Spearman's correlation coefficient
getZValue <- function(week_one_me, week_two_me, week_two_other, N) {

  r1 <- cor.test(week_one_me, week_two_me, method = "spearman", alternative = "two.sided")
  r_1a_2a <- r1$estimate
  
  r2 <- cor.test(week_one_me, week_two_other, method = "spearman", alternative = "two.sided")
  r_1a_2b <- r2$estimate
  
  r3 <- cor.test(week_two_me, week_two_other, method = "spearman", alternative = "two.sided")
  r_2a_2b <- r3$estimate
  
  z <- calcZ(r_1a_2a, r_1a_2b, r_2a_2b, N)
  return(z)
}

## Implement the function for Phi (Z) given in readme file
getPhiZ <- function(z) {
  
  p <- 0.3275911;
  a1 <- 0.254829592;
  a2 <- -0.284496736;
  a3 <- 1.421413741;
  a4 <- -1.453152027;
  a5 <- 1.061405429;
  sign <- 0
  
  if (z < 0.0)
    sign = -1
  else
    sign = 1
  x <- abs(z)/sqrt(2)
  t <- 1.0 / (1.0 + p * x);
  erf <- 1.0 - (((((a5 * t + a4) * t) + a3)
                       * t + a2) * t + a1) * t * exp(-x * x)
  return (0.5 * (1.0 + sign * erf))
}

## Calculates average number of matches for a time frame and a time window

doStat <- function() {
  fileName <- "summarizedData-t-window-3-hours.data"
  myData <- read.table(fileName, header = F)
  match <- 0
  N <- 40 #sample size
  i <- 0
  for (me in 1: (ncol(myData) - 2)) {
  
    wholeMonth <- as.vector(myData[, me])
    week_one_me <- wholeMonth[81:120]
    week_one_me <- ifelse(is.na(week_one_me), 0, week_one_me)
    week_two_me <- wholeMonth[121:160]
    week_two_me <- ifelse(is.na(week_two_me), 0, week_two_me)
   
    for (other in (me + 1):(ncol(myData))) {
        
        wholeMonth <- as.vector(myData[, other])
        week_two_other <- wholeMonth[121:160]
        week_two_other <- ifelse(is.na(week_two_other), 0, week_two_other)
        z <- getZValue(week_one_me, week_two_me, week_two_other, N) 
        z <- ifelse(is.na(z), 0, z)
        
        phiZ <- getPhiZ(z)
        p <- 1 - phiZ
        #cat( i, " : ", p, "\n")
        #i <- i + 1
        if (p <= 0.05) 
          match <- match + 1
        #else
        #  cat("match: ", match, "\n")
    }
  }
  cat("#Matches: ", match, "\n")
}