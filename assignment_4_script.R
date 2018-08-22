library(tidyverse)
library(readxl)
library(psych)
library(lattice)
library(car)
library(ppcor)
library(vcd)
library(polycor)

load("data/eye_FR_testdata-1")
load("data/eye_FR_traindata-1")
sa.france <- load("data/SAFRANCEEWdata-1.Rdata")
raw.data <- read_excel("data/Merged_15032016-1.xlsx")
raw.data

##Q3

str(traindata)

traindata %>% 
  count(lineupacc, lineuptpta, exposure)

traindata %>% 
  select(confidence, lineuprt, automatic, facecomparison) %>% 
  describe()

traindata %>% 
  select(confidence, lineuprt, automatic, facecomparison) %>% 
  pairs.panels

ggplot(traindata) +
  geom_bar(mapping = aes(x = lineupacc, fill = exposure), position = "dodge")

ggplot(traindata) +
  geom_bar(mapping = aes(x = lineupacc, fill = lineuptpta), position = "dodge")

ggplot(traindata, aes(confidence, lineupacc, alpha = .4)) +
  geom_point() +
  geom_smooth() +
  geom_jitter(alpha = .4)

ggplot(traindata, aes(lineuprt, lineupacc)) +
  geom_point() +
  geom_smooth() 

ggplot(traindata, aes(lineupacc, confidence)) +
  geom_boxplot() +
  geom_jitter(alpha = .4) +
  facet_grid(~lineupacc)

ggplot(traindata, aes(lineupacc, lineuprt)) +
  geom_boxplot() +
  geom_jitter(alpha = .4) +
  facet_grid(~lineupacc)

ggplot(traindata, aes(lineupacc, facecomparison)) +
  geom_boxplot() +
  geom_jitter(alpha = .4) +
  facet_grid(~lineupacc)

ggplot(traindata, aes(lineupacc, automatic)) +
  geom_boxplot() +
  geom_jitter(alpha = .4) +
  facet_grid(~lineupacc)

traindata %>% 
  dplyr::select(lineupacc, confidence, lineuprt, automatic, facecomparison) %>% 
  describeBy("lineupacc")

biserial(traindata[,5:8], traindata[2:4])

hetcor(traindata[,2:8])

mosaic(~ lineupacc + lineuptpta + exposure, data = traindata, main = "Stats", shade = T, legend = T)

## Question 4

null <- glm(lineupacc ~ lineuptpta + exposure, family = "binomial", data = traindata)
summary(null)


model1.train <- glm(lineupacc ~ lineuptpta*facecomparison + lineuptpta*automatic + exposure + confidence, family = "binomial", data = traindata)
summary(model1.train)

model1.test <- glm(lineupacc ~ lineuptpta*facecomparison + lineuptpta* automatic + exposure + as.numeric(confidence), family = "binomial", data = testdata)
summary(model1.test)

testdata$lineuptpta <- tolower(testdata$lineuptpta)

model2.test <- predict(model1.train, newdata = testdata)
summary(model2.test)

anova(model1.train, null)

1-pchisq(null$deviance,model1.train$df.residual)

str(testdata)

## Question 5

model2 <- glm(lineupacc ~ confidence + lineuptpta, family = "binomial", data = traindata)
summary(model2)

predict.tp <- data.frame(confidence = 80, lineuptpta = "tp")

predict(model2, newdata = predict.tp)

predict(model2, 
        newdata = predict.tp) # logit
predict(model2, 
        newdata = predict.tp, type = "response") # probability
x <- predict(model2, 
             newdata = predict.tp, type="response")
odds.tp <- x/(1-x)  # to get the answer in odds
odds.tp

predict.ta <- data.frame(confidence = 80, lineuptpta = "ta")

predict(model2, newdata = predict.ta)

predict(model2, 
        newdata = predict.ta) # logit
predict(model2, 
        newdata = predict.ta, type = "response") # probability
x <- predict(model2, 
             newdata = predict.ta, type="response")
odds.ta <- x/(1-x)  # to get the answer in odds
odds.ta


exp(coefficients(model2))

install.packages("caret")
library(caret)

confusionMatrix(as.factor(as.numeric(pred.prob >= 0.5)), as.factor(traindata$lineupacc))

confusionMatrix(as.factor(traindata$lineupacc), as.factor(as.numeric(pred.prob >= 0.5)))




## Question 5

model2 <-  glm(lineupacc ~ lineuptpta + confidence, family = "binomial", data = traindata)
model2

dim(traindata)

glm.probs <- predict(model2, type="response")
traindata %>% 
  contrasts(lineupacc)
glm.pred=rep("Down" ,117)
glm.pred[glm.probs >.8]=" Up"
table(glm.pred, lineupacc)
