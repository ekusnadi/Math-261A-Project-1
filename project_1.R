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

# Histograms side by side
par(mfrow = c(1, 2))
hist(education,
     main = "Distribution of Educational Attainment",
     xlab = "Percent of Population (25+) with Less Than HS Education",
     col = sjsu_blue,
     border = "white")
hist(tox_release,
     main = "Distribution of Toxic Releases",
     xlab = "Toxicity-weighted Concentrations of Chemical Releases",
     col = sjsu_gold,
     border = "white")

par(mfrow = c(1, 1))

# linear regression
model <- lm(tox_release ~ education)

# scatter plot
plot(education, tox_release,
     col = sjsu_blue,
     pch = 16,
     xlab = "Percent of Population (25+) with Less Than High School Education",
     ylab = "Toxicity-weighted Concentrations of Chemical Releases",
     main = "Toxic Releases vs. Educational Attainment",
     cex.main = 1.2, cex.lab = 1.1)
abline(model, col = "red", lwd = 2)

# residuals vs fitted
fitted_values <- fitted(model)
residuals_values <- residuals(model)

plot(fitted_values, residuals_values,
     col = sjsu_blue,
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 16, )
abline(h = 0)

# residuals histogram
par(mfrow = c(1, 2), mar = c(6, 5, 4, 2) + 0.1)

# Histogram of residuals
hist(residuals_values,
     main = "Distribution of Residuals",
     xlab = "Residual value",
     col = sjsu_blue,
     border = "white")

# qq plot
qqnorm(residuals_values,
       main = "Normal Q-Q Plot of Residuals",
       pch = 19,
       col = sjsu_blue)
qqline(residuals_values, lwd = 2)