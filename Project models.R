#----Import and clean data------------------------------------------------------
#Data from https://www.kaggle.com/datasets/rkiattisak/student-performance-in-mathematics
setwd("")
data_exams <- read.csv("exams.csv", header = T)

#Define the variables as factors
data_exams$gender <- as.factor(data_exams$gender)
data_exams$race.ethnicity <- as.factor(data_exams$race.ethnicity)
data_exams$parental.level.of.education <- as.factor(data_exams$parental.level.of.education)
data_exams$lunch <- as.factor(data_exams$lunch)
data_exams$test.preparation.course <- as.factor(data_exams$test.preparation.course)

#Relevel the categorical variables using appropriate baselines
data_exams$test.preparation.course <- relevel(data_exams$test.preparation.course,
                                              "none")
data_exams$lunch <- relevel(data_exams$lunch, "standard")

#----Exploratory analysis-------------------------------------------------------
#Distributions of each test score
hist(data_exams$math.score, main = "Distribution of Math Score",
     xlab = "Math score")
hist(data_exams$reading.score, main = "Distribution of Reading Score",
     xlab = "Reading score")
hist(data_exams$writing.score, main = "Distribution of Writing Score",
     xlab = "Writing score")

#Distribution of each test score by gender
boxplot(math.score ~ gender, data = data_exams, ylab = "Math Score",
        main = "Math Score by Gender")
boxplot(reading.score ~ gender, data = data_exams, ylab = "Reading Score",
        main = "Reading Score by Gender")
boxplot(writing.score ~ gender, data = data_exams, ylab = "Writing Score",
        main = "Writing Score by Gender")

#Distribution of each score by test prep course
boxplot(math.score ~ test.preparation.course, data = data_exams,
        ylab = "Math Score", main = "Math Score by Test Preparation Course")
boxplot(reading.score ~ test.preparation.course, data = data_exams,
        ylab = "Reading Score", main = "Reading Score by Test Preparation Course")
boxplot(writing.score ~ test.preparation.course, data = data_exams,
        ylab = "Writing Score", main = "Writing Score by Test Preparation Course")

#Distribution of each score by lunch program
boxplot(writing.score ~ lunch, data = data_exams, ylab = "Writing Score",
        main = "Writing Score by Lunch Program")
boxplot(reading.score ~ lunch, data = data_exams, ylab = "Reading Score",
        main = "Reading Score by Lunch Program")
boxplot(math.score ~ lunch, data = data_exams, ylab = "Math Score",
        main = "Math Score by Lunch Program")

#----Logistic regression to predict if student will pass/fail exam--------------
#Create a new binary variable to predict pass/fail
#The threshold is arbitrarily set at a certain percentage (here 50%)
passing.score <- 50

data_exams$math.pass <- ifelse(data_exams$math.score <= passing.score, 0, 1)
data_exams$reading.pass <- ifelse(data_exams$reading.score <= passing.score, 0, 1)
data_exams$writing.pass <- ifelse(data_exams$writing.score <= passing.score, 0, 1)

#Logistic regressions for passing/failing each exam
logitMath <- glm(math.pass ~ gender + lunch + test.preparation.course,
                 data = data_exams, family  = binomial())
summary(logitMath)
logitReading <- glm(reading.pass ~ gender + lunch + test.preparation.course,
                    data = data_exams, family  = binomial())
summary(logitReading)
logitWriting <- glm(writing.pass ~ gender + lunch + test.preparation.course,
                    data = data_exams, family  = binomial())
summary(logitWriting)

#Odds ratios + Confidence intervals of each model
library(MASS)
round(exp(logitMath$coeff),2)
round(exp(confint(logitMath)),2)

round(exp(logitReading$coeff),2)
round(exp(confint(logitReading)),2)

round(exp(logitWriting$coeff),2)
round(exp(confint(logitWriting)),2)

#----Multiple regressions to predict score of each exam-------------------------
regMath <- glm(math.score ~ gender + lunch + test.preparation.course,
               data = data_exams)
summary(regMath)
regReading <- lm(reading.score ~ gender + lunch + test.preparation.course,
                  data = data_exams)
summary(regReading)
regWriting <- lm(writing.score ~ gender + lunch + test.preparation.course,
                  data = data_exams)
summary(regWriting)

#Plot residuals of each model to check normality, linearity,
#and equal variance assumptions
plot(regMath)
plot(regReading)
plot(regWriting)