#TARA PLOT

library(tidyverse)
library(reshape2)

library(plotly)
library(ggcorrplot)

#correlation heat map attempt (AAIW factors)

#clear workspace
rm(list=ls(all=TRUE))

#Set path to datasets
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/")

#Read dataset
BF <- read.csv(file="TARA_COMBINED_ENV_SUBSET_LP_20.csv",header =T)

#Create data frame of selected variables and ensure they are numeric
BFdata <- select(BF, YEAR, MONTH, DAY, LAT, LON, TEMP, ZMIN, PERID, FUCO, ALLOX, x19_BUT, x19_HEX, ZEAX, DV_CHLB, DIADIN, DVCHLA, CHLA, SILICATAE,NO3_NO2,Brunt_V_DEPTH,SSD, MLD_1,MLD_2,Z_CHL_MAX,PAR1,PAR2,NPP1,NPP2)

#criteriacheck(BFdata)
Pig.df<-as.data.frame(BFdata)

PERID_N <-Pig.df$PERID
BUTA_N <- Pig.df$x19_BUT
FUCO_N <- Pig.df$FUCO
HEXA_N <- Pig.df$x19_HEX
ZEAX_N <- Pig.df$ZEAX
DIAD_N <- Pig.df$DIADIN
DVCHLA_N <- Pig.df$DVCHLA

df2 <- cbind(PERID_N, BUTA_N, FUCO_N, HEXA_N, ZEAX_N, DIAD_N,DVCHLA_N,Pig.df$CHLA,Pig.df$TEMP,Pig.df$MLD_1,Pig.df$Z_CHL_MAX,Pig.df$NPP1,Pig.df$NO3_NO2,Pig.df$SILICATAE)
colnames(df2)<-c("PERID","BUT19","FUCO","HEX19","ZEAX","DIAD","DVCHLA","CHLA","TEMP","MLD","C_MAX","NPP1","NO3_NO2","SILICATE")

#df2<-subset(df2,!(is.na(Pig.df["FUCO"])))

cormat2 <- round(cor(df2, use = "complete.obs"),2)
head(cormat2)

melted_cormat2 <- melt(cormat2)


# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat2){
  cormat[upper.tri(cormat2)] <- NA
  return(cormat2)}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat2){
  cormat[lower.tri(cormat2)]<- NA
  return(cormat2)}


# Old Plot ----------------------------------------------------------------


# 
# lower_tri <- get_lower_tri(cormat2)
# 
# melted_cormat <- melt(lower_tri, na.rm = TRUE)


ggplot(data = melted_cormat2, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()+
scale_color_continuous(name="Correlation")+
   ggtitle("Tara Correlation heatmap (<20m)")+
   geom_tile(color = "white")+
   scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                        midpoint = 0, limit = c(-1,1), space = "Lab",
                        name="Pearson\nCorrelation") +
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                    size = 12, hjust = 1))+
   coord_fixed()+

 geom_text(aes(Var2, Var1, label = value), color = "black", size = 3)


# New Plot ----------------------------------------------------------------

setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Corr_Plot")

p.mat <- cor_pmat(cormat2)
P_DIMES <-as.data.frame(p.mat)
write.table(P_DIMES,"P_TARA.csv",col.names=T,row.names=T,sep=',')
ggcorrplot(cormat2, outline.color = "white", p.mat = p.mat)
ggcorrplot(cormat2, outline.color = "white", p.mat = p.mat, insig = "blank", lab = T)
ggcorrplot(cormat2, outline.color = "white", lab = T)
