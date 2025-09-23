library(readxl)
data <- read_excel("CalEnviroScreen4.xlsx")
clean_data <- data[!is.na(data$Traffic) & !is.na(data$Education), ]
traffic <- data$Traffic
education <- as.numeric(data$Education)

model <- lm(traffic ~ education)

plot(education, traffic)
