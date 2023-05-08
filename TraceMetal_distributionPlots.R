setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code")


#install.packages("ggplot2")
#install.packages("tidyverse")

library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)


TrcMtls <-read.table(file="TheTRACEMETALFile.csv",sep=",",h=T)
mapdata <-map_data("world")

#install.packages("viridis")  # Install
library("viridis")           # Load

BPLR <- subset(TrcMtls, TrcMtls$Region_mean==0)
BPLR1 <- subset(TrcMtls, TrcMtls$Region_mean==1)
BPLR <- rbind(BPLR, BPLR1)
ARCT <- subset(TrcMtls, TrcMtls$Region_mean==2)
SARC <- subset(TrcMtls, TrcMtls$Region_mean==3)
NADR <- subset(TrcMtls, TrcMtls$Region_mean==4)
GEST <- subset(TrcMtls, TrcMtls$Region_mean==5)
NASW <- subset(TrcMtls, TrcMtls$Region_mean==6)
NATR <- subset(TrcMtls, TrcMtls$Region_mean==7)
WTRA <- subset(TrcMtls, TrcMtls$Region_mean==8)
SATL <- subset(TrcMtls, TrcMtls$Region_mean==10)
NWCS1 <- subset(TrcMtls, TrcMtls$Region_mean==14)
NWCS <- subset(TrcMtls, TrcMtls$Region_mean==15)
NWCS <- rbind(NWCS1, NWCS)
NASE <- subset(TrcMtls, TrcMtls$Region_mean==18)
MONS <- subset(TrcMtls, TrcMtls$Region_mean==30)
ISSG <- subset(TrcMtls, TrcMtls$Region_mean==31)
PSAE <- subset(TrcMtls, TrcMtls$Region_mean==51)
NPTE <- subset(TrcMtls, TrcMtls$Region_mean==55)
NPTW <- subset(TrcMtls, TrcMtls$Region_mean==56)
SPSG <- subset(TrcMtls, TrcMtls$Region_mean==59)
NPTE1 <- subset(TrcMtls, TrcMtls$Region_mean==60)
NPTE <- rbind(NPTE, NPTE1)
PNEC <- subset(TrcMtls, TrcMtls$Region_mean==61)
PEQD <- subset(TrcMtls, TrcMtls$Region_mean==62)
WARM <- subset(TrcMtls, TrcMtls$Region_mean==63)
ARCH <- subset(TrcMtls, TrcMtls$Region_mean==64)
PNEC1 <- subset(TrcMtls, TrcMtls$Region_mean==67)
PNEC <- rbind(PNEC, PNEC1)
SSTC <- subset(TrcMtls, TrcMtls$Region_mean==80)
SANT <- subset(TrcMtls, TrcMtls$Region_mean==81)
ANTA <- subset(TrcMtls, TrcMtls$Region_mean==82)
APLR <- subset(TrcMtls, TrcMtls$Region_mean==83)

TM <- rbind(BPLR, ARCT, SARC, NADR, GEST, NASW, NATR, WTRA, SATL, NWCS, NASE,
            MONS, ISSG, PSAE, NPTW, SPSG, NPTE, PNEC, PEQD, WARM, ARCH, PNEC,
            SSTC, SANT, ANTA, APLR)
TM <- as.data.frame(TM)
#Getting map of world with ggplot2
# map1 <- ggplot(mapdata, aes(x = long, y = lat, group=group)) +
#   geom_polygon()
# map1
# map2 <- ggplot(data=subset(TrcMtls, !is.na(Fe)), aes(x = LON, y = LAT)) +
#   geom_point(aes(color = Fe)) 
# map2

#pdf("TraceMetalPlots.pdf", width = 8, height = 4, onefile = TRUE)
# 
# (low = "#3845F2", high = "#FDF61C", mid = "#1CFD24", 
#   midpoint = 0.1, limit = c(0,0.2), space = "Lab", 
#   name="Pearson\nCorrelation")

# library(scales)
# p + scale_fill_gradientn
# space = "Lab", guide = "colourbar",aesthetics = "colour")


#Plotting figure for LOGFe
FeMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(log(Fe))), aes(x = LON, y = LAT, color = log(Fe)))
print(FeMAP +  scale_color_viridis_c(option = "D") + 
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Fe Distribution and Gradient Concentrations"))

#Plotting figure for Fe
FeMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(Fe)), aes(x = LON, y = LAT, color = Fe))
print(FeMAP +  scale_color_viridis_c(option = "D") + 
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Fe Distribution and Gradient Concentrations"))

#Plotting figure for Mn
MnMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(LOGMn)), aes(x = LON, y = LAT, color = LOGMn))
print(MnMAP +  scale_color_viridis(option = "D") +
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Mn Distribution and Gradient Concentrations"))

#Plotting figure for Co
CoMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(LOGCo)), aes(x = LON, y = LAT, color = LOGCo))
print(CoMAP +  scale_color_viridis(option = "D") +
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Co Distribution and Gradient Concentrations"))

#Plotting figure for Ni
NiMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(LOGNi)), aes(x = LON, y = LAT, color = LOGNi))
print(NiMAP +  scale_color_viridis(option = "D") +
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Ni Distribution and Gradient Concentrations"))


#Plotting figure for Cu
CuMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(LOGCu)), aes(x = LON, y = LAT, color = LOGCu))
print(CuMAP +  scale_color_viridis(option = "D") +
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Cu Distribution and Gradient Concentrations"))

#Plotting figure for Zn
ZnMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(LOGZn)), aes(x = LON, y = LAT, color = LOGZn))
print(ZnMAP +  scale_color_viridis(option = "D") +
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Zn Distribution and Gradient Concentrations"))

#Plotting figure for Cd
CdMAP <- ggplot(NULL, aes(x, y)) +    # Draw ggplot2 plot based on two data frames
  geom_polygon(data = mapdata, aes(x = long, y = lat, group=group)) +
  geom_point(data =subset(TM, !is.na(LOGCd)), aes(x = LON, y = LAT, color = LOGCd))
print(CdMAP +  scale_color_viridis(option = "D") +
        labs(y = "Latitude(\u00B0)", x = "Longitude(\u00B0)") + ggtitle("Cd Distribution and Gradient Concentrations"))


dev.off()



