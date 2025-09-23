library(readxl)
suppressWarnings(data <- read_excel("CalEnviroScreen4.xlsx"))
data$Education <- as.numeric(gsub("[^0-9.]", "", data$Education))
clean_data <- data[!is.na(data$Traffic) & !is.na(data$Education), ]
education <- clean_data$Education
traffic <- clean_data$Traffic

# scatter plot
par(mfrow = c(1, 1))
plot(education, traffic,
     col = "blue",
     xlab = "Percent of Population (25+) with Less Than High School Education",
     ylab = "Traffic Density (vehicle-km/hr per road length, within 150m)",
     main = "Traffic Density vs. Educational Attainment")

clean_data_no_outlier <- clean_data[clean_data$Traffic <= 40000, ]
education_no_outlier <- clean_data_no_outlier$Education
traffic_no_outlier <- clean_data_no_outlier$Traffic

# histograms
par(mfrow = c(1, 2))  

hist(education_no_outlier,
     main = "Distribution of Educational Attainment",
     xlab = "Percent of Population (25+) with Less Than HS Education",
     col = "lightblue",
     border = "white")

hist(traffic_no_outlier,
     main = "Distribution of Traffic Density",
     xlab = "Traffic Density (vehicle-km/hr per road length, within 150m)",
     col = "lightgreen",
     border = "white")

par(mfrow = c(1, 1))

# linear regression
model <- lm(traffic_no_outlier ~ education_no_outlier)


