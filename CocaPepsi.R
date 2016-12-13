setwd("F:/A Drexel/A5/MKT/Project/")
library(xlsx)
CokePepsi <- read.xlsx("CocaPepsi.xlsx",1)

CP <- CokePepsi

CP$Obs <- NULL # Observation No. are not needed.
CP$Correct <- gsub("T",1, CP$Correct) # If identified correctly, then marked as 1
CP$Correct <- gsub("F",0, CP$Correct) # If identified incorrectly, then marked as 0
CP$Correct <- as.numeric(CP$Correct)

#for (i in 1:(ncol(CP)-1))
#{
#  CP[,i] <- as.numeric((gsub((?:(2|3)),"0",CP[,i])))
#}
#CP$Confidence <- as.numeric(gsub("2","0",as.character(CP$Confidence)))  
  
CP$Like <- as.numeric(gsub(2,0,CP$Like)) # If like the taste makred 1, otherwise 0
CP$Like <- as.numeric(gsub(3,0,CP$Like))
CP$Confidence <- as.numeric(gsub(2,0,CP$Confidence)) # If confident makred 1, otherwise 0
CP$Confidence <- as.numeric(gsub(3,0,CP$Confidence))
CP$Prefer <- as.numeric(gsub(3,0,CP$Prefer)) # If prefer Coca Cola marked 1, otherwise 0
CP$Prefer <- as.numeric(gsub(2,0,CP$Prefer))


# Devided in 3 Groups
G1 <- CP[which(CP$Group == 1),]
G2 <- CP[which(CP$Group == 2),]
G3 <- CP[which(CP$Group == 3),]

# Number of Correct in each Group
G1_Correct <- nrow(CP[which(G1$Correct == 1),])
G2_Correct <- nrow(CP[which(G2$Correct == 1),])
G3_Correct <- nrow(CP[which(G3$Correct == 1),])

# Frequency Table
Group <-c("Group1","Group2","Group3")
Total <-c(30,30,30)
Correct_Num <- c(G1_Correct,G2_Correct,G3_Correct)
Freq <- c(G1_Correct/30,G2_Correct/30,G3_Correct/30)
Freq_tab <- data.frame(Group,Total,Correct_Num,Freq)
Freq_tab

# Group 1 and 2 are significantly different
# Other comparison are not significantly different
prop.test(x = c(G1_Correct,G2_Correct),n=c(30,30),correct = FALSE,conf.level = 0.90)
prop.test(x = c(G1_Correct,G3_Correct),n=c(30,30),correct = FALSE,conf.level = 0.90)
prop.test(x = c(G2_Correct,G3_Correct),n=c(30,30),correct = FALSE,conf.level = 0.90)

# Linear Regression with Interaction
lm <- lm(Correct~Group*Prefer*Confidence*Like*Gender*Age,data = CP)
summary(lm)

# ANOVA 
# Similar with Linear Regression
fit <- aov(Correct ~ Group*Prefer*Confidence*Like*Gender*Age, data=CP)
summary(fit)
plot(fit)
anova(fit)

summary(aov(Correct ~ Group*Prefer*Confidence*Like*Gender*Age,data = CP))


# Logistic Regression
glm_CP <- glm(Correct~Group+Prefer+Confidence+Like+Gender+Age,family = binomial,data = CP)
summary(glm_CP)

glm_G1 <- glm(Correct~Prefer+Confidence+Like+Gender+Age,family = binomial,data = G1)
summary(glm_G1)

glm_G2 <- glm(Correct~Prefer+Confidence+Like+Gender+Age,family = binomial,data = G2)
summary(glm_G2)

glm_G3 <- glm(Correct~Prefer+Confidence+Like+Gender+Age,family = binomial,data = G3)
summary(glm_G3)

#Random Forest
set.seed(765)
samp <- sample(nrow(CP),0.7*nrow(CP))
train <- CP[samp,]
test <- CP[-samp,]

pca <- prcomp(CP)
pca
plot(pca)
