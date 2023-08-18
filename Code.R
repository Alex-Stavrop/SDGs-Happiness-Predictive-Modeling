#DATA PREPARATION
##Read datasets
happinnes <- read.csv(
"C:/Users/alexa/OneDrive/Έ/Idividual DS/643414as_Happiness_2015.csv")
happinnes_expanded <- read.csv(
"C:/Users/alexa/OneDrive/Έ/Idividual DS/643414as_Happiness_2015_expanded.csv")
#Omit X variable
happinnes<- happinnes[-1]
happinnes_expanded<- happinnes_expanded[-1]
#Omit NAs
sum(is.na(happinnes))
happinnes<- na.omit(happinnes)
happinnes_expanded<- na.omit(happinnes_expanded)
library(caret)
library(caretEnsemble)
library(psych)
# Fit Principal Component Analysis to determine how many PC we will use
fitpca <- princomp(happinnes[,5:23], cor = TRUE, scores = T)
screeplot(fitpca)
# Perform a PCA Permutation test
permtestPCA <- function (X, nTests = 100, alpha = 0.05, center.dat
scale.data = TRUE, ...){
n <- nrow(X)
m <- ncol(X)
X <- scale(X, center = center.data, scale = scale.data)
if (scale.data) {a <- 1/(n - 1)} else {a <- 1}
res.X <- prcomp(X)
eigs.X <- res.X$sdev^2
eigs.Xperm <- matrix(0, m, nTests)
Xperm <- matrix(0, n, m)
Xperm[, 1] <- X[, 1];
for (i in 1:nTests){
for (j in 2:m) {
ind <- sort(runif(n), index.return = TRUE)$ix
Xperm[, j] <- X[ind, j]
}
res.Xperm <- prcomp(Xperm)
eigs.Xperm[, i] <- res.Xperm$sdev^2
}
perc.alpha <- matrix(0, m, 2)
for (s in 1:m){
perc.alpha[s,] <- quantile(eigs.Xperm[s,], c(alpha/2, 1 - alpha/2) )
}
plot(1:m, eigs.X, type = "b", col = "red", main = "Permutation test PCA",
xlab = "Component", ylab = "Eigenvalue", ...)
lines(1:m, perc.alpha[, 1], type = "b", col="blue")
lines(1:m, perc.alpha[, 2], type = "b", col="blue")
string1 <- paste("Confidence: ",formatC(alpha/2, digits=3, width=5,
format="f"))
string2 <- paste("Confidence: ",formatC(1-alpha/2, digits=3, width=5,
format="f"))
legend("topright", inset=.05, c("Observed", string1, string2),
lty = c(1, 1, 1), col = c("red", "blue", "blue"),
pch = c("o", "o", "o"))
return(perc.alpha)
}
perm_range <- permtestPCA(happinnes[,5:23])
#Plot loadings of the three first PC
# Change colour of bar plot
c.pc1 <- ifelse(fitpca$loadings[,1] > 0, yes="green2", no="red2")
c.pc2 <- ifelse(fitpca$loadings[,2] > 0, "green2", "red2")
5
c.pc3 <- ifelse(fitpca$loadings[,3] > 0, "green2", "red2")
# Get position for variable names
n.pc1 <- ifelse(fitpca$loadings[,1] > 0, -0.01, fitpca$loadings[,1]-0.01)
n.pc2 <- ifelse(fitpca$loadings[,2] > 0, -0.01, fitpca$loadings[,2]-0.01)
n.pc3 <- ifelse(fitpca$loadings[,3] > 0, -0.01, fitpca$loadings[,3]-0.01)
# Plot
layout(matrix(1:3, ncol=1)) # Set up layout
par(mar=c(1,3,2,1), oma=c(7.5,0,0,0)) # Set up margins
# Plot PC 1
b1 <- barplot(fitpca$loadings[,1], main="PC 1 Loadings Plot", col=c.pc1, las=2,
axisnames=FALSE)
abline(h=0)
# Plot PC 2
b2 <- barplot(fitpca$loadings[,2], main="PC 2 Loadings Plot", col=c.pc2, las=2,
axisnames=FALSE)
b3 <- barplot(fitpca$loadings[,3], main="PC 3 Loadings Plot", col=c.pc3, las=2,
axisnames=FALSE)
abline(h=0)
# Add variable names
text(x=b2, y=ifelse(fitpca$loadings[,3] > 0, -0.01,fitpca$loadings[,3]-0.01),
labels=names(fitpca$loadings[,3]), adj=1, srt=90, xpd=NA)
#Call for bootstrap test
library("boot")
my_boot_pca <- function(x, ind){
res <- princomp(x[ind, ], cor = TRUE)
return(res$sdev^2)
}
fit.boot <- boot(data = happinnes[,5:23], statistic = my_boot_pca, R = 1000)
eigs.boot <- fit.boot$t
head(eigs.boot)
fitpca$sdev^2
par(mfrow =c(1,1), mar = c(5, 4, 4, 1) + 0.1)
#total variance explained by 3 PC
var.expl <- rowSums(eigs.boot[,1:3])/rowSums(eigs.boot)
#Make a histogram and add confidence intervals
hist(var.expl,xlab = "Variance explained", las = 1, col = "blue",
main = "Bootstrap confidence Interval", breaks = 20, border = "white")
perc.alpha <- quantile(var.expl, c(0.025, 1 - 0.025) )
abline(v = perc.alpha, col = "green", lwd = 2)
abline(v = sum(fitpca$sdev[1:3]^2)/sum(fitpca$sdev^2), col = "red", lwd = 2)
6
# Split our train and set data
library(caret)
library(caretEnsemble)
chooseCRANmirror(graphics = FALSE, ind = 10)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pls)
set.seed(100)
sample <- sample.int(n = nrow(happinnes), size = floor(0.86607142858 *nrow(happinnes)), replace = F)
trainData <- happinnes[sample, ]
testData <- happinnes[-sample, ]
x = trainData[,c(5:23)]
y = trainData$Happiness_score
# Call Pcr regression on train data
set.seed(100)
pcr_model <- pcr(data = trainData[,c(5:23)],Happiness_score ~ EG.CFT.ACCS.ZS +
EN.ATM.CO2E.KD.GD + SL.AGR.EMPL.ZS + SL.AGR.EMPL.FE.ZS +
SL.AGR.EMPL.MA.ZS +
SL.SRV.EMPL.ZS + SL.SRV.EMPL.FE.ZS + NY.GDP.MKTP.PP.KD +
NY.GDP.MKTP.PP.CD + NY.GNP.PCAP.KD +
NY.GNP.MKTP.PC.CD + NY.GNP.PCAP.PP.CD + NY.GNP.MKTP.PP.CD +
IT.NET.USER.ZS + NV.IND.MANF.ZS +
NV.IND.MANF.CD + SH.ANM.ALLW.ZS + SP.URB.TOTL,
validation = "CV",
scale = TRUE)
#merge happiness and happinnes expanded data set
data<- merge(happinnes,happinnes_expanded, by = "Happiness_score" )
#Call for lasso regression on the merged data set
y <- as.vector(data$Happiness_score)
x <- model.matrix(Happiness_score ~ EG.CFT.ACCS.ZS +
EN.ATM.CO2E.KD.GD + SL.AGR.EMPL.ZS + SL.AGR.EMPL.FE.ZS +
SL.AGR.EMPL.MA.ZS +
SL.SRV.EMPL.ZS + SL.SRV.EMPL.FE.ZS + NY.GDP.MKTP.PP.KD +
NY.GDP.MKTP.PP.CD + NY.GNP.PCAP.KD +
NY.GNP.MKTP.PC.CD + NY.GNP.PCAP.PP.CD + NY.GNP.MKTP.PP.CD +
IT.NET.USER.ZS + NV.IND.MANF.ZS +
NV.IND.MANF.CD + SH.ANM.ALLW.ZS + SP.URB.TOTL,
data = data)
x <- x[,-1]
set.seed(100)
7
fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 5, verboseIter = TRUE)
lasso <- train(Happiness_score ~ EG.CFT.ACCS.ZS +
EN.ATM.CO2E.KD.GD + SL.AGR.EMPL.ZS + SL.AGR.EMPL.FE.ZS +
SL.AGR.EMPL.MA.ZS +
SL.SRV.EMPL.ZS + SL.SRV.EMPL.FE.ZS + NY.GDP.MKTP.PP.KD +
NY.GDP.MKTP.PP.CD + NY.GNP.PCAP.KD +
NY.GNP.MKTP.PC.CD + NY.GNP.PCAP.PP.CD + NY.GNP.MKTP.PP.CD +
IT.NET.USER.ZS + NV.IND.MANF.ZS +
NV.IND.MANF.CD + SH.ANM.ALLW.ZS + SP.URB.TOTL,
data,
method = 'glmnet',
tuneGrid = expand.grid(alpha = 1,
lambda = seq(0.0001, 1, length =10)),
trControl = fitcontrol)
#comparing models on test data
#Lasso
predictionlassotest <- predict(lasso, testData)
sqrt(mean((testData$Happiness_score-predictionlassotest)^2))
#PCR
predictPCRtest <- predict(pcr_model, testData, ncomp = 3)
sqrt(mean((testData$Happiness_score-predictPCRtest)^2))
#load new prediction data set
happiness_2019 <- read.csv(
"C:/Users/alexa/OneDrive/Έ/Idividual DS/Happiness_2019.csv")
happiness_2019<- happiness_2019[-1]
predictionlassohappiness <- predict(lasso, happiness_2019)
predictPCRhappiness <- predict(pcr_model, happiness_2019, ncomp = 3)
predictionlassohappiness <- round(predictionlassohappiness, digits = 2)
predictPCRhappiness <- round(predictPCRhappiness, digits = 2)
predicts <- cbind("Country_Name" = happiness_2019$Country.Name ,
"Year" = happiness_2019$Time ,"Happines_Lassomodel" =
predictionlassohappiness ,"Happines_PCR_model" =
predictPCRhappiness)
sort(predicts)
library(kableExtra)
knitr::kable(predicts,
caption = "Predicted values for both models") %>%
kable_styling(font_size = 10,full_width
