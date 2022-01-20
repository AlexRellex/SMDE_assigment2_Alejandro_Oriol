######################################
#Importing the dataset
marathon_filtered <- read.csv("Marathon_filtered.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)

sim <- read.csv("simulation_results.csv", header = TRUE, fill = TRUE, sep = ",", na.strings = "", dec = ".", strip.white = TRUE)

simulation <- sim$simtime

real_data <- marathon_filtered$Official.Time



sim_to_analyze = data.frame(x1 = simulation, x2 = "v1")

real_to_analytze = data.frame(x1 = real_data, x2 = "v2")

data = mergeRows(sim_to_analyze, real_to_analytze, common.only = FALSE)

AnovaModel.1 <- aov(V1 ~ x2, data = data)
summary(AnovaModel.1)
Boxplot(V1 ~ x2, data = data, id.method = "y")
