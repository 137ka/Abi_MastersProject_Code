#Script to do multiple linear regression on my data

#clear workspace
rm(list= ls(all= TRUE))

#Set the directory to the NN file
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Near_Neighbours")

#load in relevant packages
library(tidyverse)
library(ggplot2)

# Loading in my data files
PoSp<-read.table(file="POLARSPR.csv",sep=",",h=T)
PoSu<-read.table(file="POLARSUM.csv",sep=",",h=T)
PoA<-read.table(file="POLARAUT.csv",sep=",",h=T)
WeSp<-read.table(file="WESTSPR.csv",sep=",",h=T)
WeSu<-read.table(file="WESTSUM.csv",sep=",",h=T)
WeA<-read.table(file="WESTAUT.csv",sep=",",h=T)
TrSp<-read.table(file="TRADESSPR.csv",sep=",",h=T)
TrSu<-read.table(file="TRADESSUM.csv",sep=",",h=T)
TrA<-read.table(file="TRADESAUT.csv",sep=",",h=T)

#Combining datasets for global analysis
SPRING <- rbind(PoSp,WeSp,TrSp)
SUMMER <- rbind(PoSu,WeSu,TrSu)
AUTUMN <- rbind(PoA,WeA,TrA)

WEST<- rbind(WeSp,WeSu,WeA)
POLAR<- rbind(PoSp,PoSu,PoA)
TRADE<- rbind(TrSp,TrSu,TrA)

GLOBAL <- rbind(SPRING,SUMMER,AUTUMN)

#Making the data into CSVs
write.table(GLOBAL,"GlobalED.csv",col.names=T,row.names=F,sep=',')



# SPRING with NO TEMP - --------------------------------------
#Using the model NT as before
#Making Plots - rough plots (https://www.statmethods.net/stats/regression.html)
#install.packages('ggstatsplot')
#install.packages('visreg')
#library(ggstatsplot)
library(visreg)

SPRING <- na.omit(SPRING)
model1 <- lm(PERID ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model1 <- step(model1, trace = FALSE)
summary(model1)
visreg(model1)

model2 <- lm(FUCO ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model2 <- step(model2, trace = FALSE)
summary(model2)
visreg(model2)


model3 <- lm(BUT ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model3 <- step(model3, trace = FALSE)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model4 <- step(model4, trace = FALSE)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model5 <- step(model5, trace = FALSE)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model6 <- step(model6, trace = FALSE)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = SPRING)
#reduces down the model to the best options!
model7 <- step(model7, trace = FALSE)
summary(model7)
visreg(model7)


# SUMMER with NO TEMP - --------------------------------------
#Using the model NT as before
#Making Plots - rough plots (https://www.statmethods.net/stats/regression.html)


model1 <- lm(PERID ~ TEMP + NO3 + DEPTH + Fe, data = SUMMER)
summary(model1)
visreg(model1)

model2 <- lm(FUCO ~ TEMP + NO3 + DEPTH + Fe, data = SUMMER)
#reduces down the model to the best options!
#model2 <- step(model2, trace = FALSE)
summary(model2)
visreg(model2)


model3 <- lm(BUT ~ TEMP + NO3 + DEPTH + Fe, data = SUMMER)
#reduces down the model to the best options!
model3 <- step(model3, trace = FALSE)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + DEPTH + SALINITY + Fe, data = SUMMER)
#reduces down the model to the best options!
#model4 <- step(model4, trace = FALSE)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + SALINITY + Fe, data = SUMMER)
#reduces down the model to the best options!
#model5 <- step(model5, trace = FALSE)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + DEPTH + SALINITY + Fe, data = SUMMER)
#reduces down the model to the best options!
#model6 <- step(model6, trace = FALSE)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + DEPTH + Fe, data = SUMMER)
#reduces down the model to the best options!
#model7 <- step(model7, trace = FALSE)
summary(model7)
visreg(model7)


# August Models -----------------------------------------------------------
#install.packages('equatiomatic')
library(equatiomatic)
#install.packages('car')
library(car)
Anova(model2)


model1 <- lm(PERID ~ TEMP + NO3, data = AUTUMN)
summary(model1)
visreg(model1)


model2 <- lm(FUCO ~ TEMP + NO3 + SALINITY + Fe, data = AUTUMN)
#reduces down the model to the best options!
model2 <- step(model2, trace = FALSE)
summary(model2)
#visreg(model2)

extract_eq(model2,
           use_coefs = TRUE, # display coefficients
           wrap = TRUE, # multiple lines
           terms_per_line = 2)

model3 <- lm(BUT ~ TEMP + NO3 + DEPTH + SALINITY + Fe, data = AUTUMN)
#reduces down the model to the best options!
model3 <- step(model3, trace = FALSE)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + DEPTH + SALINITY + SALINITY + Fe, data = AUTUMN)
#reduces down the model to the best options!
model4 <- step(model4, trace = FALSE)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + SALINITY + Fe, data = AUTUMN)
#reduces down the model to the best options!
model5 <- step(model5, trace = FALSE)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + DEPTH + SALINITY + Fe, data = AUTUMN)
#reduces down the model to the best options!
model6 <- step(model6, trace = FALSE)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + DEPTH +SALINITY + Fe, data = AUTUMN)
#reduces down the model to the best options!
model7 <- step(model7, trace = FALSE)
summary(model7)
visreg(model7)


# GLOBAL MLR --------------------------------------------------------------

model1 <- lm(PERID ~ NO3 + Fe + Mn + Co + Ni +
               Cu, data = GLOBAL)
#model1 <- step(model1, trace = FALSE)
summary(model1)
visreg(model1)


model2 <- lm(FUCO ~ NO3 + SALINITY + Mn +
               Cu + Cd, data = GLOBAL)
summary(model2)
visreg(model2)

# extract_eq(model2,
#            use_coefs = TRUE, # display coefficients
#            wrap = TRUE, # multiple lines
#            terms_per_line = 2)

model3 <- lm(BUT ~ TEMP + Fe + Mn + Co + Ni +
               Cu + Cd, data = GLOBAL)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + Co, data = GLOBAL)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + SALINITY + Co + Ni +
               Cu + Zn, data = GLOBAL)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + Mn + Co + Ni +
               Cu + Zn + Cd, data = GLOBAL)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + SALINITY + Mn, data = GLOBAL)
summary(model7)
visreg(model7)


# Polar -------------------------------------------------------------------

model1 <- lm(PERID ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
model1 <- step(model1, trace = FALSE)
summary(model1)
visreg(model1)

model2 <- lm(FUCO ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
#reduces down the model to the best options!
model2 <- step(model2, trace = FALSE)
summary(model2)
visreg(model2)


model3 <- lm(BUT ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
#reduces down the model to the best options!
model3 <- step(model3, trace = FALSE)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
#reduces down the model to the best options!
model4 <- step(model4, trace = FALSE)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
#reduces down the model to the best options!
model5 <- step(model5, trace = FALSE)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
#reduces down the model to the best options!
model6 <- step(model6, trace = FALSE)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni +
               Cu + Zn + Cd, data = POLAR)
model7 <- step(model7, trace = FALSE)
summary(model7)
visreg(model7)


# West --------------------------------------------------------------------

model1 <- lm(PERID ~ NO3 + Ni, data = WEST)
#model1 <- step(model1, trace = FALSE)
summary(model1)
visreg(model1)

model2 <- lm(FUCO ~ TEMP + NO3 + Ni, data = WEST)
#reduces down the model to the best options!
#model2 <- step(model2, trace = FALSE)
summary(model2)
visreg(model2)


model3 <- lm(BUT ~ NO3 + Co + Zn, data = WEST)
#reduces down the model to the best options!
#model3 <- step(model3, trace = FALSE)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + SALINITY + Mn + Ni, data = WEST)
#reduces down the model to the best options!
#model4 <- step(model4, trace = FALSE)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + Mn + Ni + Zn + Cd, data = WEST)
#reduces down the model to the best options!
#model5 <- step(model5, trace = FALSE)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + Fe + Mn + Co + Ni + Zn + Cd, data = WEST)
#reduces down the model to the best options!
#model6 <- step(model6, trace = FALSE)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Co, data = WEST)
#model7 <- step(model7, trace = FALSE)
summary(model7)
visreg(model7)

# Trades ------------------------------------------------------------------

TRADE<-subset(TRADE,!(is.na(TRADE["BIO_MONTH"])))

model1 <- lm(PERID ~ TEMP + NO3 + SALINITY + Fe + Co + Ni, data = TRADE)
#model1 <- step(model1, trace = FALSE)
summary(model1)
visreg(model1)

model2 <- lm(FUCO ~ TEMP + NO3 + SALINITY + Co, data = TRADE)
summary(model2)
visreg(model2)


model3 <- lm(BUT ~ TEMP + NO3 + DEPTH + SALINITY + Co, data = TRADE)
#reduces down the model to the best options!
#model3 <- step(model3, trace = FALSE)
summary(model3)
visreg(model3)

model4 <- lm(DIAD ~ TEMP + NO3 + SALINITY + Mn + Co, data = TRADE)
#reduces down the model to the best options!
#model4 <- step(model4, trace = FALSE)
summary(model4)
visreg(model4)

model5 <- lm(ZEA ~ TEMP + NO3 + DEPTH + SALINITY + Fe + Mn + Co + Ni + Cd, data = TRADE)
#reduces down the model to the best options!
#model5 <- step(model5, trace = FALSE)
summary(model5)
visreg(model5)

model6 <- lm(HEX ~ TEMP + NO3 + SALINITY + Mn + Ni + Cd, data = TRADE)
#reduces down the model to the best options!
#model6 <- step(model6, trace = FALSE)
summary(model6)
visreg(model6)

model7 <- lm(DVCHLA ~ TEMP + NO3 + DEPTH + SALINITY + Mn + Ni + Cd, data = TRADE)
#model7 <- step(model7, trace = FALSE)
summary(model7)
visreg(model7)











layout(matrix(c(1,2,3,4),2,2))
plot(modelNT)

#Plot 1 - Assessing Outliers
# install.packages("performance")
# install.packages("see")
library(performance)

check_model(model1)
check_model(modelhex)
