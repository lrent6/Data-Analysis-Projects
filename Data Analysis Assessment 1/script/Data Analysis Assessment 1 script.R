##Question 1
library(readr)
balance <- read_csv("balance.csv")
View(balance)

hist(balance$FBSway[balance$Age=="Young"], col="black", main = "Young Balance", xlab = "FBSway")
hist(balance$FBSway[balance$Age=="Old"], col = "black", main = "Old Balance", xlab = "FBSway")
boxplot(FBSway ~ Age, data=balance)
stripchart(FBSway ~ Age, data = balance, vertical = TRUE, add = TRUE,
           method = "jitter", jitter = 0.3, pch = 20)

nrow(balance)

shapiro.test(balance$FBSway[balance$Age=="Old"])
shapiro.test(balance$FBSway[balance$Age=="Young"])

SE_young <- sd(balance$FBSway[balance$Age=="Young"])/sqrt(30)
SE_young       
SE_old <- sd(balance$FBSway[balance$Age=="Old"])/sqrt(35)
SE_old
mean_young <-mean(balance$FBSway[balance$Age=="Young"], na.rm= FALSE)
sd(mean_young)
median(balance$FBSway[balance$Age=="Old"], na.rm= FALSE)

IQR(balance$FBSway[balance$Age=="Young"], na.rm= FALSE)
IQR(balance$FBSway[balance$Age=="Old"], na.rm= FALSE)

log.young <- log(balance$FBSway[balance$Age=="Young"])
hist(log3.young)
log2.old <- log2(balance$FBSway[balance$Age=="Old"])
hist(log2.old)


wilcox.test(FBSway ~ Age, data = balance)

sd(balance[1,], na.rm = FALSE)

## Question 2
library(readr)
APOE <- read_csv("APOE.csv")
View(APOE)

APOE_2 <- read.table("APOE.csv", header= TRUE, sep = ",", dec = ".", quote = "\"'")
                  

tbAPOE<- table(APOE$Diagnosis, APOE$APOE_Genotype) 
tbAPOE

barplot(tbAPOE[1,]/(tbAPOE[1,]+tbAPOE[2,])*100, ylim = c(0, 100),
        ylab = "% subject with APOE gene")
mosaicplot(tbAPOE)
barplot(tbAPOE,legend.text = c("Alzheimer's", "Control"), ylab = "% of Affected Subjects", xlab = "Genotype", las = 1)

fisher.test(tbAPOE)
sd(tbAPOE[1,])
sd(tbAPOE[2,])

## Question 3 Brains
Brains <- read.table("brains.csv", header=TRUE, sep = ",")
summary(Brains)

plot(Brains, ylab = "Grey Matter (mm3/voxel)")
abline(lm(GreyMatter ~ Proficiency), col = "red")

cor.test(Brains$Proficiency, Brains$GrayMatter)

hist(Brains$Proficiency)
hist(Brains$GrayMatter)


## Question 4 epilepsy
epilepsy_2<- read.csv("epilepsy.csv", header= TRUE, sep = ",", dec = ".", fill = TRUE)

head(epilepsy_2)

summary(epilepsy_2)

stripchart(inhibitory_synapse_density ~ diagnosis, vertical = TRUE, method = "jitter", data = epilepsy_2)

epilepsy_2.lm <- lm(inhibitory_synapse_density ~ diagnosis, data = epilepsy_2)
plot(epilepsy_2.lm)
summary(epilepsy_2.lm)
epilepsy.mean <- tapply(epilepsy_2$inhibitory_synapse_density, epilepsy$diagnosis, mean)

shapiro.test(epilepsy_2$inhibitory_synapse_density[epilepsy_2$diagnosis =="control"])
shapiro.test(epilepsy_2$inhibitory_synapse_density[epilepsy_2$diagnosis == "epilepsy"])

mean(epilepsy_2$inhibitory_synapse_density[epilepsy_2$diagnosis == "epilepsy"])
mean(epilepsy_2$inhibitory_synapse_density[epilepsy_2$diagnosis =="control"])

sd(epilepsy_2$inhibitory_synapse_density[epilepsy_2$diagnosis =="control"])
sd(epilepsy_2$inhibitory_synapse_density[epilepsy_2$diagnosis == "epilepsy"])

sd(epilepsy_2[1-4,2])
sd(epilepsy_2[4-8,2])
boxplot(inhibitory_synapse_density ~ diagnosis, data = epilepsy_2, ylab = "Inhibitory Synapse Density" )
ylab = "% of Affected Subjects", xlab = "Genotype", las = 1
effect.size.20 <- 0.20*epilepsy.mean[2]
summary(effect.size.20)


