######################################
#Importing the dataset
M1834 <- read.csv("marathon_results_2015.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)
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
#Remove the rows that have null values in any column
M1834 <- na.omit(M1834)

sim <- read.csv("simulation_results.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)

simulation <- sim$simtime

real_data <- M1834$Official.Time



sim_to_analyze = data.frame(x1 = simulation, x2 = "v1")

real_to_analytze = data.frame(x1 = real_data, x2 = "v2")

data = mergeRows(sim_to_analyze, real_to_analytze, common.only = FALSE)

AnovaModel.1 <- aov(V1 ~ x2, data = data)
summary(AnovaModel.1)
Boxplot(V1 ~ x2, data = data, id.method = "y")
