library(readxl)
suppressWarnings(data <- read_excel("CalEnviroScreen4.xlsx"))
data$Education <- as.numeric(gsub("[^0-9.]", "", data$Education))
clean_data <- data[!is.na(data$`Tox. Release`) & !is.na(data$Education), ]
education <- clean_data$Education
tox_release <- clean_data$`Tox. Release`
# Define SJSU brand colors
sjsu_blue <- "#0055A2"
sjsu_gold <- "#E5A823"
sjsu_gray <- "#939597"
sjsu_darkgray <- "#53565A"
# Scatter plot
par(mfrow = c(1, 1))
plot(education, tox_release,
     col = sjsu_blue,
     pch = 19,
     xlab = "Percent of Population (25+) with Less Than High School Education",
     ylab = "Toxicity-weighted Concentrations of Chemical Releases",
     main = "Toxic Releases vs. Educational Attainment",
     cex.main = 1.2, cex.lab = 1.1)
clean_data_no_outlier <- clean_data[clean_data$`Tox. Release` <= 40000, ]
education_no_outlier <- clean_data_no_outlier$Education
tox_release_no_outlier <- clean_data_no_outlier$`Tox. Release`
# Histograms side by side
par(mfrow = c(1, 2))
hist(education_no_outlier,
     main = "Distribution of Educational Attainment",
     xlab = "Percent of Population (25+) with Less Than HS Education",
     col = sjsu_blue,
     border = "white")
hist(tox_release_no_outlier,
     main = "Distribution of Toxic Releases",
     xlab = "Toxicity-weighted Concentrations of Chemical Releases",
     col = sjsu_gold,
     border = "white")
# Reset plotting area
par(mfrow = c(1, 1))
# linear regression
model <- lm(tox_release_no_outlier ~ education_no_outlier)