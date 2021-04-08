library(ggplot2)
library(GGally) # for ggpairs
library(memisc)
library(gridExtra)
library(psych)
library(moments)
library(lmtest)
library(Hmisc)
library(corrplot)
library(car)
library(scales)
library(RColorBrewer)
library(MASS)
library(nnet)
library(brant)
library(mlogit)
library(robustbase)
library(sandwich)
library(dplyr)
library(sjPlot)

winequalitywhite <- read_csv("D:/wine quality selection/winequality_white.csv")
winequalityred <- read_csv("D:/wine quality selection/winequality_red.csv")

winequalitywhite$color<-c(0) #encoding color white
winequalityred$color<-c(1) #encoding color red
winequality<-rbind(winequalitywhite,winequalityred)
#winequality$quality<-factor(winequality$quality)
str(winequality)

summary(winequality)

####Box plot for better visualization of extreme values:
boxplot(winequality$free.sulfur.dioxide, xlab="Free sulfur dioxde")
boxplot(winequality$total.sulfur.dioxide, xlab="Total sulfur dioxde")
boxplot(winequality$residual.sugar, xlab="Residual Sugar")

w<-subset(winequality, winequality$free.sulfur.dioxide>250)
w
w<-subset(winequality, winequality$residual.sugar>60)
w

##Acidity
qplot(x = fixed.cidity, data = winequality) + labs(y="Frequency")
qplot(x = volatile.acidity, data = winequality) + labs(y="Frequency")
qplot(x = citric.acid, data = winequality) + labs(y="Frequency")
##Sugar:
qplot(x = residual.sugar, data = winequality) + labs(y="Frequency")
summary(winequality$residual.sugar)
##alcohol
qplot(x = alcohol, data = winequality) + labs(y="Frequency")
summary(winequality$alcohol)
skewness(winequality$alcohol)
##Quality
qplot(x = quality, data = winequality) + labs(y="Frequency")
unique(winequality$quality)
##Other factors
g5 <- qplot(x = pH, data = winequality) + labs(y="Frequency")
g6 <- qplot(x = chlorides, data = winequality) + labs(y="Frequency")
g7 <- qplot(x = free.sulfur.dioxide, data = winequality) + labs(y="Frequency")
g8 <- qplot(x = total.sulfur.dioxide, data = winequality) + labs(y="Frequency")
g9 <- qplot(x = density, data = winequality) + labs(y="Frequency")
g10 <- qplot(x = sulphates, data = winequality) + labs(y="Frequency")
grid.arrange(g5,g6,g7,g8,g9,g10,ncol=2)

##Factorizing Quality

winequality$quality.cat <- 0

winequality$quality.cat <- ifelse(winequality$quality==7, 'high',winequality$quality.cat)
winequality$quality.cat <- ifelse(winequality$quality==8, 'high',winequality$quality.cat )
winequality$quality.cat <- ifelse(winequality$quality==9, 'high',winequality$quality.cat )
winequality$quality.cat <- ifelse(winequality$quality==3, 'low', winequality$quality.cat)
winequality$quality.cat <- ifelse(winequality$quality==4, 'low', winequality$quality.cat)
winequality$quality.cat <- ifelse(winequality$quality==5, 'medium', winequality$quality.cat)
winequality$quality.cat <- ifelse(winequality$quality==6, 'medium', winequality$quality.cat)

winequality$quality.cat <- factor(winequality$quality.cat, levels = c("low", "medium", "high"))

##Acid and Quality:
unique(winequality$quality.cat)

a1 <- ggplot(winequality, aes(x = fixed.cidity, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1) + ylab("frequency")
a1

a2 <- ggplot(winequality, aes(x = volatile.acidity, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1) +ylab("frequency")
a2

a3 <- ggplot(winequality, aes(x = citric.acid, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1) + ylab("frequency")
a3

grid.arrange(a1,a2,a3,ncol=1)

##Quality with respect to sugar and alcohol

b1 <- ggplot(winequality, aes(x = residual.sugar, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1)+ ylab("frequency")
b1

b2<- ggplot(winequality, aes(x = alcohol, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1) + ylab("frequency")
b2
summary(winequality$residual.sugar)

##other variables and qualiy

c1 <- ggplot(winequality, aes(x = total.sulfur.dioxide , color = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1) +ylab("frequency")

c2 <- ggplot(winequality, aes(x = free.sulfur.dioxide , colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1)+ ylab("frequency")

c3 <- ggplot(winequality, aes(x = pH , colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1)+ ylab("frequency")

c4 <- ggplot(winequality, aes(x = sulphates, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1) +ylab("frequency")

c5 <- ggplot(winequality, aes(x = density, colour = quality.cat)) +
  geom_density(position="identity", fill = NA, size = 1)+ ylab("frequency")
c1
c2
c3
c4
c5
grid.arrange(c1,c2,c3,c4,c5,ncol=2)

##Correlation Matrix (Quality and color not included as they are factors)

w<-winequality
w$quality.cat<-NULL
w$color<-NULL
head(w)

wine.cor = cor(w)
corrplot(wine.cor)

cor(w)

##I am including all the variables in the exhaustive algorithm to see what is the model suggested by the machine!!!
library(leaps)
reg <-
  regsubsets(pH ~ fixed.cidity + volatile.acidity + citric.acid + residual.sugar + chlorides+free.sulfur.dioxide+total.sulfur.dioxide +sulphates+alcohol+density,
             data = winequality,
             nbest = 1,       # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")
reg

s<-summary(reg)
plot(reg, scale = "adjr2", main = "Adjusted R^2")
which.max(s$adjr2)

##After removing our variables due to reasons stated above, we have our first model model 1 as below:

phmodel<-lm(pH~citric.acid+residual.sugar+sulphates, data=winequality)
summary(phmodel)

##Assumption 1: Linearity.
qplot(data=winequality,y=pH,x=citric.acid,color=quality.cat, size=0.2) +geom_smooth(method="lm", size = 1) + scale_y_continuous(breaks = pretty_breaks(n=10)) + scale_x_continuous(breaks = pretty_breaks(n=10)) + xlab("citric.acid")+ylab("pH") + scale_size(range = c(1,1)) + theme_grey(base_size = 10)

qplot(data=winequality,y=pH,x=residual.sugar,color=quality.cat, size=0.2) +geom_smooth(method="lm", size = 1) + scale_y_continuous(breaks = pretty_breaks(n=10)) + scale_x_continuous(breaks = pretty_breaks(n=10)) + xlab("residual.sugar") + ylab("pH") + scale_size(range = c(1,1)) + theme_grey(base_size = 10)

qplot(data=winequality,y=pH,x=sulphates,color=quality.cat, size=0.2) +geom_smooth(method="lm", size = 1) + scale_y_continuous(breaks = pretty_breaks(n=10)) + scale_x_continuous(breaks = pretty_breaks(n=10)) + xlab("sulphates") + ylab("pH") + scale_size(range = c(1,1)) + theme_grey(base_size = 10)
s$which[10,]

##Assumption 2: Multicollinearity
cat("VIF values",vif(phmodel))
cat("\nMean of VIF",mean(vif(phmodel)))
cat("\nReciprocal of VIF values",1/vif(phmodel))

##Assumption 3: Residual assumptions

cat("Mean of residuals",mean(phmodel$residuals))

##Normality of residuals:
plot(phmodel)

##Breush Pagen Test:
bptest(phmodel)

##Correcting heteroscadesticity
cat("Skewness of residual sugar",skewness(winequality$residual.sugar))
cat("\nSkewness of citric acid",skewness(winequality$citric.acid))
cat("\nSkewness of sulphates",skewness(winequality$sulphates))

##Influential case:
winequality$fitted<-phmodel$fitted
winequality$residuals<-phmodel$residuals
winequality$standardizedresiduals<-rstandard(phmodel)
winequality$cooks<-cooks.distance(phmodel)

cat("Maximum value of cook's distance",max(winequality$cooks))

possible.outliers <-subset(winequality, standardizedresiduals < -2|standardizedresiduals > 2)
cat("\nNo.of.possible outliers",nrow(possible.outliers))

phmodelRobust <- lmrob(pH~citric.acid+residual.sugar+sulphates, data=winequality)

summary(phmodelRobust)

##Final model
phfinal<-lm(pH ~volatile.acidity+density, data=winequality)
summary(phfinal)
plot(phfinal)
durbinWatsonTest(phfinal)
bptest(phfinal)

winequality$fitted<-phfinal$fitted
winequality$residuals<-phfinal$residuals
winequality$standardizedresiduals<-rstandard(phfinal)
winequality$cooks<-cooks.distance(phfinal)

cat("Maximum value of cook's distance",max(winequality$cooks))

possible.outliers <-subset(winequality, standardizedresiduals < -2|standardizedresiduals > 2)
cat("\nNoof.outliers",nrow(possible.outliers))

winequality$color.cat <- 0
winequality$color.cat <- ifelse(winequality$color==0, 'red',winequality$color.cat)
winequalitycolor.cat <- ifelse(winequality$color==1, 'white',winequality$color.cat )
winequality$color.cat <- factor(winequality$color.cat, levels = c("red", "white"))

g1 <- winequality[winequality$color==0,]$pH
g2 <- winequality[winequality$color==1,]$pH

i<-t.test(g1,g2, var.equal = FALSE)
i

##Effect size:
t<-i$statistic[[1]]
df<-i$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
cat("The effect size",round(r, 3))