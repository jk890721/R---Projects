```{r}
# Getting the data
# Source came from: https://www.kaggle.com/code/hely333/what-is-the-secret-of-academic-success/input
data = student.mat
# Clean the data, remove columns that are not numeric
new_data <- na.omit(data)
new_data <- subset(new_data, select = -V1)
new_data <- subset(new_data, select = -V4)
new_data <- subset(new_data, select = -V5)
new_data <- subset(new_data, select = -V6)
new_data <- subset(new_data, select = -V9)
new_data <- subset(new_data, select = -V10)
new_data <- subset(new_data, select = -V11)
new_data <- subset(new_data, select = -V12)
new_data <- subset(new_data, select = -V16)
new_data <- subset(new_data, select = -V17)
new_data <- subset(new_data, select = -V18)
new_data <- subset(new_data, select = -V19)
new_data <- subset(new_data, select = -V20)
new_data <- subset(new_data, select = -V21)
new_data <- subset(new_data, select = -V22)
new_data <- subset(new_data, select = -V23)

# Remove the first row of the data because it is the name for that column 
new_data <- new_data[-1,]

# Exchange the name of different column
names(new_data)[names(new_data)=="V2"] <- "gender"
names(new_data)[names(new_data)=="V3"] <- "age"
names(new_data)[names(new_data)=="V14"] <- "study_time"
#For studying time, 1 means less than 2 hours/per week 
#                   2 means 2 to 5 hours/per week
#                   3 means 5 to 10 hours/per week   
#                   4 means more than 10 hours/per week 
names(new_data)[names(new_data)=="V30"] <- "absences"
names(new_data)[names(new_data)=="V33"] <- "grade"
# Setting Female to 0 and Male to 1 
new_data$gender <- ifelse(new_data$gender == "F", 0, 1)

# Remove all the 0s in the grade to remove the outliers and test the assumption again.
new_data = new_data[new_data$grade != 0, ]

# Setting all the variables into numeric
new_data$age = as.numeric(new_data$age)
new_data$V7 = as.numeric(new_data$V7)
new_data$V8 = as.numeric(new_data$V8)
new_data$V13 = as.numeric(new_data$V13)
new_data$study_time= as.numeric(new_data$study_time)
new_data$V15 = as.numeric(new_data$V15)
new_data$V24 = as.numeric(new_data$V24)
new_data$V25 = as.numeric(new_data$V25)
new_data$V26 = as.numeric(new_data$V26)
new_data$V27 = as.numeric(new_data$V27)
new_data$V28 = as.numeric(new_data$V28)
new_data$V29 = as.numeric(new_data$V29)
new_data$absences = as.numeric(new_data$absences)
new_data$V31 = as.numeric(new_data$V31)
new_data$V32 = as.numeric(new_data$V32)
new_data$grade = as.numeric(new_data$grade)

#Test if all variables and numeric
str(new_data)
```

```{r}
# Using histogram to see the numbers of the two gender
library(ggplot2)
ggplot(new_data, aes(x = gender)) +
  geom_bar() + 
  xlab("Female, Male") +
  ylab("Count") +
  ggtitle("Histogram of Gender")
  
# We can see that female has slightly more data than male. 

# Using the boxplot to see study time in different age. 
boxplot(grade ~ study_time, new_data)
stripchart(new_data$grade ~ new_data$study_time, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = 1:length(levels(new_data$study_time)))
# In this graph, we can see that age from 15 to 18 study 2 to 5 hours per week.
# We are not considering the 21 and 22 years old data because there is only one data.

# grade vs absences
boxplot(grade ~ absences, new_data)
absence <- stripchart(new_data$grade ~ new_data$absences, vertical = TRUE, method = "jitter", pch = 19, add = TRUE, col = 1:length(levels(new_data$absences)))
absece <- boxplot(new_data$grade ~ new_data$absences)
abline(absence)
```

```{r}
# In male's perspective
Male <- subset(new_data, new_data$gender == "1")
head(Male)
library(lattice)
xyplot(grade~ age|as.factor(study_time), data = Male,
       layout = c(2,2), type = "o", main = "Male Data", 
       xlab = "study time", ylab = "grade")

Female <- subset(new_data, new_data$gender == "0")
head(Female)
xyplot(grade~ age|as.factor(study_time), data = Female,
       layout = c(2,2), type = "o", main = "Female Data", 
       xlab = "study time", ylab = "grade")


# Use fixed effect to test this data
model0 <- lm(grade ~ study_time, data = Female)
summary(model0)

model.1 <- lm(grade ~ study_time, data = Male)
summary(model.1)

# Use random effect to test
library(lme4)
model.2 <- lmer(grade ~ study_time + age + (1|gender), data = new_data)
summary(model.2)

model.3 <- lmer(grade ~ study_time * age + (1|gender), data = new_data)
summary(model.3)

anova(model.2, model.3)

# We are choosing model 3 because its p-value is smaller than 0.05. 

model.4 <- lmer(grade ~ study_time + age + (1 + age|gender), data = new_data)
summary(model.4)
model.5 <- lmer(grade ~ study_time * age + (1 + age|gender), data = new_data)
summary(model.5)

anova(model.4, model.5)
# We are choosing model.5 because the p-value is smaller than 0.05. 

anova(model.3, model.5)
# We choose model model 3 since the p-value is lager than 0.05. 

# Uncorrelated data
model.6 <- lmer(grade ~ study_time * age + (1|gender) + (0 + age|gender), data = new_data)
summary(model.6)

anova(model.3, model.6)
# We choose model 3 for our final model since the p-value is equal to 1. 
```

```{r}
#test the qq-plot
par(mfrow = c(1,2))
random <- ranef(model.3)$gender[["(Intercept)"]]
qqnorm(random, main = "Random Intercept")
qqline(random)

residual <- residuals(model.3)
qqnorm(residual, main = "Random Intercept")
qqline(residual)
par(mfrow= c(1, 1))

plot(model.3, xlab = "Fitted Vlaues", ylab = "Residual", 
     main = "Constant Variance Assumption - Random Intercept Model")

# All assumption are met. 
```