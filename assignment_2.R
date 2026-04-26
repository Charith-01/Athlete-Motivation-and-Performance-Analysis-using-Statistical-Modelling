# Load the data in R

getwd()
setwd("C:\\Users\\Wasika\\Desktop\\TPSM\\GROUP ASSIGNMENT")
data <- read.csv("psych_training_sports.csv")
str(data)
summary(data)
dim(data)
names(data)

#  Clean the data

# Check missing values
colSums(is.na(data))


#Check duplicates
sum(duplicated(data))

# Convert categorical variables properly.

data$Gender <- as.factor(data$Gender)
data$Sport_Type <- as.factor(data$Sport_Type)
data$Training_Method <- as.factor(data$Training_Method)
data$Adaptation_Type <- as.factor(data$Adaptation_Type)
data$Intervention_Intensity_Level <- factor(
  data$Intervention_Intensity_Level,
  levels = c("Low", "Medium", "High"),
  ordered = TRUE
)

# Create the main motivation variable
data$Motivation <- (data$Intrinsic_Motivation + data$Extrinsic_Motivation) / 2


# Create motivation groups for group comparison


data$Motivation_Group <- cut(
  data$Motivation,
  breaks = c(-Inf, 50, 70, Inf),
  labels = c("Low", "Medium", "High")
)

# Do descriptive analytics first


#Load packages
library(dplyr)
library(ggplot2)
# Overall summaries
data %>%
  summarise(
    Mean_Motivation = mean(Motivation),
    SD_Motivation = sd(Motivation),
    Mean_Performance = mean(Performance_Score),
    SD_Performance = sd(Performance_Score)
  )
#Group summaries
data %>%
  group_by(Motivation_Group) %>%
  summarise(
    Count = n(),
    Avg_Performance = mean(Performance_Score),
    SD_Performance = sd(Performance_Score)
  )
#Histogram of Motivation
ggplot(data, aes(x = Motivation)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black")
#Histogram of Performance
ggplot(data, aes(x = Performance_Score)) +
  geom_histogram(bins = 30, fill = "orange", color = "black")
#Boxplot of performance by motivation group
ggplot(data, aes(x = Motivation_Group, y = Performance_Score, fill = Motivation_Group)) +
  geom_boxplot()
#Scatterplot of motivation vs performance
ggplot(data, aes(x = Motivation, y = Performance_Score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE)

#  Do correlation analysis

  
cor(data$Motivation, data$Performance_Score)
cor.test(data$Motivation, data$Performance_Score, method = "pearson")


#  Do hypothesis testing properly
# 

#  Fit a simple linear regression model
model1 <- lm(Performance_Score ~ Motivation, data = data)
summary(model1)

#  Compare performance across motivation groups using ANOVA
anova_model <- aov(Performance_Score ~ Motivation_Group, data = data)
summary(anova_model)

# Build a predictive model using train/test split
set.seed(123)

trainID <- sample(1:nrow(data), round(0.8 * nrow(data)))
train <- data[trainID, ]
test <- data[-trainID, ]

#Train simple predictive model:
  
fit_simple <- lm(Performance_Score ~ Motivation, data = train)
summary(fit_simple)

#Predict:
  
y_pred <- predict(fit_simple, test)
y_actual <- test$Performance_Score

MSE <- mean((y_actual - y_pred)^2)
RMSE <- sqrt(MSE)

MSE
RMSE

# Build a stronger multiple regression model

fit_multi <- lm(
  Performance_Score ~ Motivation +
    Self_Confidence_Score +
    Training_Adherence_Rate +
    Resilience_Index +
    Anxiety_Level +
    Experience_Years +
    Intervention_Intensity_Level,
  data = train
)
summary(fit_multi)

y_pred2 <- predict(fit_multi, test)
MSE2 <- mean((y_actual - y_pred2)^2)
RMSE2 <- sqrt(MSE2)

MSE2
RMSE2

#  Check regression assumptions

# Linearity
plot(data$Motivation, data$Performance_Score)
abline(lm(Performance_Score ~ Motivation, data = data), col = "red")

# Homoscedasticity
fit <- lm(Performance_Score ~ Motivation, data = data)
plot(fitted(fit), residuals(fit))
abline(h = 0, col = "red")

# Normality of residuals
qqnorm(residuals(fit))
qqline(residuals(fit), col = "red")
shapiro.test(residuals(fit))

# Independence
install.packages("lmtest")
library(lmtest)
dwtest(fit)

# Multicollinearity for multiple regression
install.packages("car")
library(car)
vif(fit_multi)

