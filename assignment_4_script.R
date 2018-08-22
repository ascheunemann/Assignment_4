library(tidyverse)
library(readxl)
library(psych)
library(lattice)
library(car)

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


traindata %>% 
  select(lineupacc, confidence, lineuprt, automatic, facecomparison) %>% 
  describeBy("lineupacc")

biserial(traindata[,5:8], traindata[2:4])

install.packages("polycor")
library(polycor)
install.packages("ppcor")
library(ppcor)
library(vcd)
hetcor(traindata[,2:8])


## Question 4

mosaic(~ lineupacc + lineuptpta + exposure, data = traindata, main = "Stats", shade = T, legend = T)

## Question 4

null <- glm(lineupacc ~ lineuptpta + exposure, family = "binomial", data = traindata)
summary(null)


model1.train <- glm(lineupacc ~ lineuptpta*facecomparison + lineuptpta* automatic + exposure + confidence, family = "binomial", data = traindata)
summary(model1.train)

model1.test <- glm(lineupacc ~ lineuptpta*facecomparison + lineuptpta* automatic + exposure + confidence, family = "binomial", data = testdata)
summary(model1.test)

1-pchisq(null$deviance,model1.train$df.residual)

anova(null, model1.train)


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
