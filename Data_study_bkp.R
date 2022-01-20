######################################
#Importing the dataset
M1834 <- read.csv("marathon_results_2015.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)
ORIGINAL <- M1834
summary(M1834)

#Preprocessing
library(lubridate)
#Translate time to seconds
M1834$X5K <- lubridate::period_to_seconds(hms(M1834$X5K))
M1834$X10K <- lubridate::period_to_seconds(hms(M1834$X10K))
M1834$X15K <- lubridate::period_to_seconds(hms(M1834$X15K))
M1834$X20K <- lubridate::period_to_seconds(hms(M1834$X20))
M1834$X25K <- lubridate::period_to_seconds(hms(M1834$X25K))
M1834$X30K <- lubridate::period_to_seconds(hms(M1834$X30K))
M1834$X35K <- lubridate::period_to_seconds(hms(M1834$X35K))
M1834$X40K <- lubridate::period_to_seconds(hms(M1834$X40K))
M1834$Pace <- lubridate::period_to_seconds(hms(M1834$Pace))
M1834$Official.Time <- lubridate::period_to_seconds(hms(M1834$Official.Time))
M1834$Bib <- as.numeric(M1834$Bib)

#Filtering for our target: EX: Man of 18-34 years
M1834 <- subset(M1834, M1834$M.F == "M" & M1834$Age >= 18 & M1834$Age <= 34,)
#Delete no relevant columns
M1834 <- subset(M1834, select = -c(State,Citizen,X.1,Half,Proj.Time))
summary(M1834)
#Remove the rows that have null values in any column
M1834 <- na.omit(M1834)

distance<-c("X5K","X10K","X15K","X20K","X25K","X30K","X35K","X40K","Official.Time")
for (k in distance) {
  if (k != "X5K") {
    colName <- paste(k,"Parcial", sep = "")
    M1834[,colName] <- M1834[,k] - M1834[,temp]
    if (k == "Official.Time") {
      paceName <- paste(k,"Pace", sep = "")
      M1834[,paceName] <- M1834[,colName]/2
    }
    else {
      paceName <- paste(k,"Pace", sep = "")
      M1834[,paceName] <- M1834[,colName]/5
    }
  }
  else {
    colName <- paste(k,"Parcial", sep = "")
    M1834[,colName] <- M1834[,k]
    paceName <- paste(k,"Pace", sep = "")
    M1834[,paceName] <- M1834[,colName]/5
  }
  temp = k
}
names(M1834) <- make.names(names(M1834))

#Printing the summary and std of all columns in c
distance<-c("X5K","X10K","X15K","X20K","X25K","X30K","X35K","X40K","Official.Time")
for (k in distance) {
  print(k)
  print(summary(M1834[,k]))
  print(sd(M1834[,k]))
}

#Calculating p
d <- sort(M1834$Official.Time)
i <- round(nrow(M1834)*0.1)
print(d[i+2])
TABLEV2 = as.data.frame((with(M1834,table(Pace))))
print(TABLEV2[,"Freq"]/nrow(M1834))
#Calculating cumulative p
TABLEV2$cumsum <- cumsum(TABLEV2[,"Freq"]/nrow(M1834))

#Printing cumulative sums that are important for GPSS
cumsums <- t(TABLEV2$cumsum)
for (k in order(cumsums)) {
  cat(cumsums[k],k+17, sep = ",")
  cat("/")
}

#Define elite and non elite groups
print(mean(M1834$Official.Time))
Elite <- subset(M1834, M1834$Official.Time <= mean(M1834$Official.Time))
NoElite <- subset(M1834, M1834$Official.Time > mean(M1834$Official.Time))
Improvement <- mean(Elite$Pace) / mean(NoElite$Pace)
print(length(M1834$Pace))
print(length(Elite$Pace))
print(length(NoElite$Pace))
ElitePercent <- length(Elite$Pace) / length(M1834$Pace)
print(Improvement)
print(ElitePercent)

######################################
#Model creation
library(lmtest)


lr<-data.frame()
distance<-c("X5KParcial", "X10KParcial","X15KParcial","X20KParcial","X25KParcial","X30KParcial","X35KParcial","X40KParcial")
for (i in distance) {
  f <- paste(i, "~ X30K + X40K")
  model = lm(as.formula(f), M1834)
  #print(model)
  #Verify linear regression model
  # Ensure the linear regression is valid by checking:
  # 1. Normality
  #print(shapiro.test(residuals(model)))
  # 2. Homogeneity
  #print(bptest(model))
  # 3. Independence
  #print(dwtest(model, alternative = "two.sided"))
  
  lr <- rbind(lr, model$coefficients)
}
colnames(lr) <- c('B0','B1','B2')
View(lr)
print(lr)

###################################
#Apply the yates algorithm
library(unrepx)
#(---, +--, -+-, ++-, --+, +-+, -++, +++)
#Compy answer values from GPSS simulation
answer <- c(23135.57,22408.35,22152.81,16823.61,22757.01,22428.15,22540.75,12895.36)
factors <- c("Food", "WC", "Water")
yates(answer, factors)

###################################
#Exporting modified data to use it in simulation
write.csv(M1834,"Marathon_filtered.csv", row.names = FALSE)

