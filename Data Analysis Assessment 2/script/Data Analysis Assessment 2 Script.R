# Question 1, 2-factor GLM
## Read in data
data <- read.table("OpsinChap18.csv", header = TRUE, sep = ',')
data

## plot a boxplot and stripchart
boxplot(relativeExpressionOfSWS1~population*waterClarity, data=data)
stripchart(relativeExpressionOfSWS1~population*waterClarity, data=data, 
           add = TRUE, vertical = TRUE, method = "jitter", pch = 21, col = "maroon")

## Analysis of data
data.lm <- lm(relativeExpressionOfSWS1~population+waterClarity, data=data)
plot(data.lm)

attach(data)
interaction.plot(population, waterClarity, relativeExpressionOfSWS1, type = "b", legand = TRUE)
detach(data)

data.lm.ss3 <- lm(relativeExpressionOfSWS1~population*waterClarity, data=data, contrasts = list(population = contr.sum, waterClarity = contr.sum))

summary(data.lm.ss3)
summary (data.lm)
library(car)
Anova(data.lm.ss3, type = 3)

##Post hoc
library(emmeans)
p.emmeans <- emmeans(data.lm.ss3, "population")
p.emmeans

data.pairs <- pairs(p.emmeans)
data.pairs

data.emmeans.btWC <- emmeans(data.lm.ss3, "population", by = "waterClarity")
data.emmeans.btWC
data.pairs.byWC <- pairs(data.emmeans.btWC)
data.pairs.byWC
confint(data.pairs.byWC)

data.emmeans.btP <- emmeans(data.lm.ss3, "waterClarity", by= "population")
data.emmeans.btP
data.pairs.btP <- pairs(data.emmeans.btP)
data.pairs.btP

# Question 2, 1- factor GLM
## read in data
data <- read.table("hairLoss.csv", header = TRUE, sep = ',')
boxplot(hairDensity~Treatment, data = data)

## plot data
data.lm <- lm(log(hairDensity)~Treatment, data=data)
plot(data.lm)

## analyse data
summary(data.lm)
anova(data.lm)

library(emmeans)
data.emmeans <- emmeans(data.lm, "Treatment", type = "response")
data.emmeans

data.pairs <- pairs(data.emmeans)
data.pairs

confint(data.pairs)
