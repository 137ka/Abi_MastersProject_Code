# Make box and whisker diagrams  for seasonality
#Clear workspace
rm(list=ls(all=TRUE))

library(ggplot2)
library(tidyverse)
library(dplyr)

# Trace metals ------------------------------------------------------------

#Assign NORTHERN Trace Metals Directory
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/ProvincesTraceMetal")

#Load in files and assign variables - Northern Hemisphere + trace metal
arct <- read.csv(file="ARCTtm.csv",sep=",",h=T)
bplr <- read.csv(file="BPLRtm.csv",sep=",",h=T)
gest <- read.csv(file="GESTtm.csv",sep=",",h=T)
nadr <- read.csv(file="NADRtm.csv",sep=",",h=T)
nase <- read.csv(file="NASEtm.csv",sep=",",h=T)
nasw <- read.csv(file="NASWtm.csv",sep=",",h=T)
natr <- read.csv(file="NATRtm.csv",sep=",",h=T)
npte <- read.csv(file="NPTEtm.csv",sep=",",h=T)
nptw <- read.csv(file="NPTWtm.csv",sep=",",h=T)
pnec <- read.csv(file="PNECtm.csv",sep=",",h=T)
psae <- read.csv(file="PSAEtm.csv",sep=",",h=T)
sarc <- read.csv(file="SARCtm.csv",sep=",",h=T)
warm <- read.csv(file="WARMtm.csv",sep=",",h=T)
wtra <- read.csv(file="WTRAtm.csv",sep=",",h=T)

#Assign SOUTHERN Trace Metals Directory
#Load in files and assign variables - Southern Hemisphere + trace metal
anta <- read.csv(file="ANTAtm.csv",sep=",",h=T)
aplr <- read.csv(file="APLRtm.csv",sep=",",h=T)
arch <- read.csv(file="ARCHtm.csv",sep=",",h=T)
issg <- read.csv(file="ISSGtm.csv",sep=",",h=T)
mons <- read.csv(file="MONStm.csv",sep=",",h=T)
peqd <- read.csv(file="PEQDtm.csv",sep=",",h=T)
sant <- read.csv(file="SANTtm.csv",sep=",",h=T)
satl <- read.csv(file="SATLtm.csv",sep=",",h=T)
spsg <- read.csv(file="SPSGtm.csv",sep=",",h=T)
sstc <- read.csv(file="SSTCtm.csv",sep=",",h=T)

#subsetting each province (pgmnts and TMs) by season



# ARCT --------------------------------------------------------------------

arcttmSpr <- subset(arct, arct$SEASON==1)
arcttmSum <- subset(arct, arct$SEASON==2)
arcttmAut <- subset(arct, arct$SEASON==3)
arcttmWin <- subset(arct, arct$SEASON==4)

# Trace Metals
#Spring
arct1mn <- as.data.frame(arcttmSpr$Mn)
arct1fe <- as.data.frame(arcttmSpr$Fe)
arct1co <- as.data.frame(arcttmSpr$Co)
arct1ni <- as.data.frame(arcttmSpr$Ni)
arct1cu <- as.data.frame(arcttmSpr$Cu)
arct1zn <- as.data.frame(arcttmSpr$Zn)
arct1cd <- as.data.frame(arcttmSpr$Cd)
arct1temp <- as.data.frame(arcttmSpr$TEMPERATURE)
arct1no3 <- as.data.frame(arcttmSpr$NITRATE)

#Summer
arct2mn <- as.data.frame(arcttmSum$Mn)
arct2fe <- as.data.frame(arcttmSum$Fe)
arct2co <- as.data.frame(arcttmSum$Co)
arct2ni <- as.data.frame(arcttmSum$Ni)
arct2cu <- as.data.frame(arcttmSum$Cu)
arct2zn <- as.data.frame(arcttmSum$Zn)
arct2cd <- as.data.frame(arcttmSum$Cd)
arct2temp <- as.data.frame(arcttmSum$TEMPERATURE)
arct2no3 <- as.data.frame(arcttmSum$NITRATE)

#Autumn
arct3mn <- as.data.frame(arcttmAut$Mn)
arct3fe <- as.data.frame(arcttmAut$Fe)
arct3co <- as.data.frame(arcttmAut$Co)
arct3ni <- as.data.frame(arcttmAut$Ni)
arct3cu <- as.data.frame(arcttmAut$Cu)
arct3zn <- as.data.frame(arcttmAut$Zn)
arct3cd <- as.data.frame(arcttmAut$Cd)
arct3temp <- as.data.frame(arcttmAut$TEMPERATURE)
arct3no3 <- as.data.frame(arcttmAut$NITRATE)

#Winter
arct4mn <- as.data.frame(arcttmWin$Mn)
arct4fe <- as.data.frame(arcttmWin$Fe)
arct4co <- as.data.frame(arcttmWin$Co)
arct4ni <- as.data.frame(arcttmWin$Ni)
arct4cu <- as.data.frame(arcttmWin$Cu)
arct4zn <- as.data.frame(arcttmWin$Zn)
arct4cd <- as.data.frame(arcttmWin$Cd)
arct4temp <- as.data.frame(arcttmWin$TEMPERATURE)
arct4no3 <- as.data.frame(arcttmWin$NITRATE)


# BPLR --------------------------------------------------------------------

bplrtmSpr <- subset(bplr, bplr$SEASON==1)
bplrtmSum <- subset(bplr, bplr$SEASON==2)
bplrtmAut <- subset(bplr, bplr$SEASON==3)
bplrtmWin <- subset(bplr, bplr$SEASON==4)

# Trace Metals
#Spring
bplr1mn <- as.data.frame(bplrtmSpr$Mn)
bplr1fe <- as.data.frame(bplrtmSpr$Fe)
bplr1co <- as.data.frame(bplrtmSpr$Co)
bplr1ni <- as.data.frame(bplrtmSpr$Ni)
bplr1cu <- as.data.frame(bplrtmSpr$Cu)
bplr1zn <- as.data.frame(bplrtmSpr$Zn)
bplr1cd <- as.data.frame(bplrtmSpr$Cd)
bplr1temp <- as.data.frame(bplrtmSpr$TEMPERATURE)
bplr1no3 <- as.data.frame(bplrtmSpr$NITRATE)

#Summer
bplr2mn <- as.data.frame(bplrtmSum$Mn)
bplr2fe <- as.data.frame(bplrtmSum$Fe)
bplr2co <- as.data.frame(bplrtmSum$Co)
bplr2ni <- as.data.frame(bplrtmSum$Ni)
bplr2cu <- as.data.frame(bplrtmSum$Cu)
bplr2zn <- as.data.frame(bplrtmSum$Zn)
bplr2cd <- as.data.frame(bplrtmSum$Cd)
bplr2temp <- as.data.frame(bplrtmSum$TEMPERATURE)
bplr2no3 <- as.data.frame(bplrtmSum$NITRATE)

#Autumn
bplr3mn <- as.data.frame(bplrtmAut$Mn)
bplr3fe <- as.data.frame(bplrtmAut$Fe)
bplr3co <- as.data.frame(bplrtmAut$Co)
bplr3ni <- as.data.frame(bplrtmAut$Ni)
bplr3cu <- as.data.frame(bplrtmAut$Cu)
bplr3zn <- as.data.frame(bplrtmAut$Zn)
bplr3cd <- as.data.frame(bplrtmAut$Cd)
bplr3temp <- as.data.frame(bplrtmAut$TEMPERATURE)
bplr3no3 <- as.data.frame(bplrtmAut$NITRATE)

#Winter
bplr4mn <- as.data.frame(bplrtmWin$Mn)
bplr4fe <- as.data.frame(bplrtmWin$Fe)
bplr4co <- as.data.frame(bplrtmWin$Co)
bplr4ni <- as.data.frame(bplrtmWin$Ni)
bplr4cu <- as.data.frame(bplrtmWin$Cu)
bplr4zn <- as.data.frame(bplrtmWin$Zn)
bplr4cd <- as.data.frame(bplrtmWin$Cd)
bplr4temp <- as.data.frame(bplrtmWin$TEMPERATURE)
bplr4no3 <- as.data.frame(bplrtmWin$NITRATE)


# SARC --------------------------------------------------------------------

sarctmSpr <- subset(sarc, sarc$SEASON==1)
sarctmSum <- subset(sarc, sarc$SEASON==2)
sarctmAut <- subset(sarc, sarc$SEASON==3)
sarctmWin <- subset(sarc, sarc$SEASON==4)

# Trace Metals
#Spring
sarc1mn <- as.data.frame(sarctmSpr$Mn)
sarc1fe <- as.data.frame(sarctmSpr$Fe)
sarc1co <- as.data.frame(sarctmSpr$Co)
sarc1ni <- as.data.frame(sarctmSpr$Ni)
sarc1cu <- as.data.frame(sarctmSpr$Cu)
sarc1zn <- as.data.frame(sarctmSpr$Zn)
sarc1cd <- as.data.frame(sarctmSpr$Cd)
sarc1temp <- as.data.frame(sarctmSpr$TEMPERATURE)
sarc1no3 <- as.data.frame(sarctmSpr$NITRATE)

#Summer
sarc2mn <- as.data.frame(sarctmSum$Mn)
sarc2fe <- as.data.frame(sarctmSum$Fe)
sarc2co <- as.data.frame(sarctmSum$Co)
sarc2ni <- as.data.frame(sarctmSum$Ni)
sarc2cu <- as.data.frame(sarctmSum$Cu)
sarc2zn <- as.data.frame(sarctmSum$Zn)
sarc2cd <- as.data.frame(sarctmSum$Cd)
sarc2temp <- as.data.frame(sarctmSum$TEMPERATURE)
sarc2no3 <- as.data.frame(sarctmSum$NITRATE)

#Autumn
sarc3mn <- as.data.frame(sarctmAut$Mn)
sarc3fe <- as.data.frame(sarctmAut$Fe)
sarc3co <- as.data.frame(sarctmAut$Co)
sarc3ni <- as.data.frame(sarctmAut$Ni)
sarc3cu <- as.data.frame(sarctmAut$Cu)
sarc3zn <- as.data.frame(sarctmAut$Zn)
sarc3cd <- as.data.frame(sarctmAut$Cd)
sarc3temp <- as.data.frame(sarctmAut$TEMPERATURE)
sarc3no3 <- as.data.frame(sarctmAut$NITRATE)

#Winter
sarc4mn <- as.data.frame(sarctmWin$Mn)
sarc4fe <- as.data.frame(sarctmWin$Fe)
sarc4co <- as.data.frame(sarctmWin$Co)
sarc4ni <- as.data.frame(sarctmWin$Ni)
sarc4cu <- as.data.frame(sarctmWin$Cu)
sarc4zn <- as.data.frame(sarctmWin$Zn)
sarc4cd <- as.data.frame(sarctmWin$Cd)
sarc4temp <- as.data.frame(sarctmWin$TEMPERATURE)
sarc4no3 <- as.data.frame(sarctmWin$NITRATE)


# ANTA --------------------------------------------------------------------

antatmSpr <- subset(anta, anta$SEASON==1)
antatmSum <- subset(anta, anta$SEASON==2)
antatmAut <- subset(anta, anta$SEASON==3)
antatmWin <- subset(anta, anta$SEASON==4)

# Trace Metals
#Spring
anta1mn <- as.data.frame(antatmSpr$Mn)
anta1fe <- as.data.frame(antatmSpr$Fe)
anta1co <- as.data.frame(antatmSpr$Co)
anta1ni <- as.data.frame(antatmSpr$Ni)
anta1cu <- as.data.frame(antatmSpr$Cu)
anta1zn <- as.data.frame(antatmSpr$Zn)
anta1cd <- as.data.frame(antatmSpr$Cd)
anta1temp <- as.data.frame(antatmSpr$TEMPERATURE)
anta1no3 <- as.data.frame(antatmSpr$NITRATE)

#Summer
anta2mn <- as.data.frame(antatmSum$Mn)
anta2fe <- as.data.frame(antatmSum$Fe)
anta2co <- as.data.frame(antatmSum$Co)
anta2ni <- as.data.frame(antatmSum$Ni)
anta2cu <- as.data.frame(antatmSum$Cu)
anta2zn <- as.data.frame(antatmSum$Zn)
anta2cd <- as.data.frame(antatmSum$Cd)
anta2temp <- as.data.frame(antatmSum$TEMPERATURE)
anta2no3 <- as.data.frame(antatmSum$NITRATE)

#Autumn
anta3mn <- as.data.frame(antatmAut$Mn)
anta3fe <- as.data.frame(antatmAut$Fe)
anta3co <- as.data.frame(antatmAut$Co)
anta3ni <- as.data.frame(antatmAut$Ni)
anta3cu <- as.data.frame(antatmAut$Cu)
anta3zn <- as.data.frame(antatmAut$Zn)
anta3cd <- as.data.frame(antatmAut$Cd)
anta3temp <- as.data.frame(antatmAut$TEMPERATURE)
anta3no3 <- as.data.frame(antatmAut$NITRATE)

#Winter
anta4mn <- as.data.frame(antatmWin$Mn)
anta4fe <- as.data.frame(antatmWin$Fe)
anta4co <- as.data.frame(antatmWin$Co)
anta4ni <- as.data.frame(antatmWin$Ni)
anta4cu <- as.data.frame(antatmWin$Cu)
anta4zn <- as.data.frame(antatmWin$Zn)
anta4cd <- as.data.frame(antatmWin$Cd)
anta4temp <- as.data.frame(antatmWin$TEMPERATURE)
anta4no3 <- as.data.frame(antatmWin$NITRATE)


# APLR --------------------------------------------------------------------

aplrtmSpr <- subset(aplr, aplr$SEASON==1)
aplrtmSum <- subset(aplr, aplr$SEASON==2)
aplrtmAut <- subset(aplr, aplr$SEASON==3)
aplrtmWin <- subset(aplr, aplr$SEASON==4)

# Trace Metals
#Spring
aplr1mn <- as.data.frame(aplrtmSpr$Mn)
aplr1fe <- as.data.frame(aplrtmSpr$Fe)
aplr1co <- as.data.frame(aplrtmSpr$Co)
aplr1ni <- as.data.frame(aplrtmSpr$Ni)
aplr1cu <- as.data.frame(aplrtmSpr$Cu)
aplr1zn <- as.data.frame(aplrtmSpr$Zn)
aplr1cd <- as.data.frame(aplrtmSpr$Cd)
aplr1temp <- as.data.frame(aplrtmSpr$TEMPERATURE)
aplr1no3 <- as.data.frame(aplrtmSpr$NITRATE)

#Summer
aplr2mn <- as.data.frame(aplrtmSum$Mn)
aplr2fe <- as.data.frame(aplrtmSum$Fe)
aplr2co <- as.data.frame(aplrtmSum$Co)
aplr2ni <- as.data.frame(aplrtmSum$Ni)
aplr2cu <- as.data.frame(aplrtmSum$Cu)
aplr2zn <- as.data.frame(aplrtmSum$Zn)
aplr2cd <- as.data.frame(aplrtmSum$Cd)
aplr2temp <- as.data.frame(aplrtmSum$TEMPERATURE)
aplr2no3 <- as.data.frame(aplrtmSum$NITRATE)

#Autumn
aplr3mn <- as.data.frame(aplrtmAut$Mn)
aplr3fe <- as.data.frame(aplrtmAut$Fe)
aplr3co <- as.data.frame(aplrtmAut$Co)
aplr3ni <- as.data.frame(aplrtmAut$Ni)
aplr3cu <- as.data.frame(aplrtmAut$Cu)
aplr3zn <- as.data.frame(aplrtmAut$Zn)
aplr3cd <- as.data.frame(aplrtmAut$Cd)
aplr3temp <- as.data.frame(aplrtmAut$TEMPERATURE)
aplr3no3 <- as.data.frame(aplrtmAut$NITRATE)

#Winter
aplr4mn <- as.data.frame(aplrtmWin$Mn)
aplr4fe <- as.data.frame(aplrtmWin$Fe)
aplr4co <- as.data.frame(aplrtmWin$Co)
aplr4ni <- as.data.frame(aplrtmWin$Ni)
aplr4cu <- as.data.frame(aplrtmWin$Cu)
aplr4zn <- as.data.frame(aplrtmWin$Zn)
aplr4cd <- as.data.frame(aplrtmWin$Cd)
aplr4temp <- as.data.frame(aplrtmWin$TEMPERATURE)
aplr4no3 <- as.data.frame(aplrtmWin$NITRATE)


# GEST --------------------------------------------------------------------

gesttmSpr <- subset(gest, gest$SEASON==1)
gesttmSum <- subset(gest, gest$SEASON==2)
gesttmAut <- subset(gest, gest$SEASON==3)
gesttmWin <- subset(gest, gest$SEASON==4)

# Trace Metals
#Spring
gest1mn <- as.data.frame(gesttmSpr$Mn)
gest1fe <- as.data.frame(gesttmSpr$Fe)
gest1co <- as.data.frame(gesttmSpr$Co)
gest1ni <- as.data.frame(gesttmSpr$Ni)
gest1cu <- as.data.frame(gesttmSpr$Cu)
gest1zn <- as.data.frame(gesttmSpr$Zn)
gest1cd <- as.data.frame(gesttmSpr$Cd)
gest1temp <- as.data.frame(gesttmSpr$TEMPERATURE)
gest1no3 <- as.data.frame(gesttmSpr$NITRATE)

#Summer
gest2mn <- as.data.frame(gesttmSum$Mn)
gest2fe <- as.data.frame(gesttmSum$Fe)
gest2co <- as.data.frame(gesttmSum$Co)
gest2ni <- as.data.frame(gesttmSum$Ni)
gest2cu <- as.data.frame(gesttmSum$Cu)
gest2zn <- as.data.frame(gesttmSum$Zn)
gest2cd <- as.data.frame(gesttmSum$Cd)
gest2temp <- as.data.frame(gesttmSum$TEMPERATURE)
gest2no3 <- as.data.frame(gesttmSum$NITRATE)

#Autumn
gest3mn <- as.data.frame(gesttmAut$Mn)
gest3fe <- as.data.frame(gesttmAut$Fe)
gest3co <- as.data.frame(gesttmAut$Co)
gest3ni <- as.data.frame(gesttmAut$Ni)
gest3cu <- as.data.frame(gesttmAut$Cu)
gest3zn <- as.data.frame(gesttmAut$Zn)
gest3cd <- as.data.frame(gesttmAut$Cd)
gest3temp <- as.data.frame(gesttmAut$TEMPERATURE)
gest3no3 <- as.data.frame(gesttmAut$NITRATE)

#Winter
gest4mn <- as.data.frame(gesttmWin$Mn)
gest4fe <- as.data.frame(gesttmWin$Fe)
gest4co <- as.data.frame(gesttmWin$Co)
gest4ni <- as.data.frame(gesttmWin$Ni)
gest4cu <- as.data.frame(gesttmWin$Cu)
gest4zn <- as.data.frame(gesttmWin$Zn)
gest4cd <- as.data.frame(gesttmWin$Cd)
gest4temp <- as.data.frame(gesttmWin$TEMPERATURE)
gest4no3 <- as.data.frame(gesttmWin$NITRATE)


# NADR --------------------------------------------------------------------

nadrtmSpr <- subset(nadr, nadr$SEASON==1)
nadrtmSum <- subset(nadr, nadr$SEASON==2)
nadrtmAut <- subset(nadr, nadr$SEASON==3)
nadrtmWin <- subset(nadr, nadr$SEASON==4)

# Trace Metals
#Spring
nadr1mn <- as.data.frame(nadrtmSpr$Mn)
nadr1fe <- as.data.frame(nadrtmSpr$Fe)
nadr1co <- as.data.frame(nadrtmSpr$Co)
nadr1ni <- as.data.frame(nadrtmSpr$Ni)
nadr1cu <- as.data.frame(nadrtmSpr$Cu)
nadr1zn <- as.data.frame(nadrtmSpr$Zn)
nadr1cd <- as.data.frame(nadrtmSpr$Cd)
nadr1temp <- as.data.frame(nadrtmSpr$TEMPERATURE)
nadr1no3 <- as.data.frame(nadrtmSpr$NITRATE)

#Summer
nadr2mn <- as.data.frame(nadrtmSum$Mn)
nadr2fe <- as.data.frame(nadrtmSum$Fe)
nadr2co <- as.data.frame(nadrtmSum$Co)
nadr2ni <- as.data.frame(nadrtmSum$Ni)
nadr2cu <- as.data.frame(nadrtmSum$Cu)
nadr2zn <- as.data.frame(nadrtmSum$Zn)
nadr2cd <- as.data.frame(nadrtmSum$Cd)
nadr2temp <- as.data.frame(nadrtmSum$TEMPERATURE)
nadr2no3 <- as.data.frame(nadrtmSum$NITRATE)

#Autumn
nadr3mn <- as.data.frame(nadrtmAut$Mn)
nadr3fe <- as.data.frame(nadrtmAut$Fe)
nadr3co <- as.data.frame(nadrtmAut$Co)
nadr3ni <- as.data.frame(nadrtmAut$Ni)
nadr3cu <- as.data.frame(nadrtmAut$Cu)
nadr3zn <- as.data.frame(nadrtmAut$Zn)
nadr3cd <- as.data.frame(nadrtmAut$Cd)
nadr3temp <- as.data.frame(nadrtmAut$TEMPERATURE)
nadr3no3 <- as.data.frame(nadrtmAut$NITRATE)

#Winter
nadr4mn <- as.data.frame(nadrtmWin$Mn)
nadr4fe <- as.data.frame(nadrtmWin$Fe)
nadr4co <- as.data.frame(nadrtmWin$Co)
nadr4ni <- as.data.frame(nadrtmWin$Ni)
nadr4cu <- as.data.frame(nadrtmWin$Cu)
nadr4zn <- as.data.frame(nadrtmWin$Zn)
nadr4cd <- as.data.frame(nadrtmWin$Cd)
nadr4temp <- as.data.frame(nadrtmWin$TEMPERATURE)
nadr4no3 <- as.data.frame(nadrtmWin$NITRATE)


# NASE --------------------------------------------------------------------

nasetmSpr <- subset(nase, nase$SEASON==1)
nasetmSum <- subset(nase, nase$SEASON==2)
nasetmAut <- subset(nase, nase$SEASON==3)
nasetmWin <- subset(nase, nase$SEASON==4)

# Trace Metals
#Spring
nase1mn <- as.data.frame(nasetmSpr$Mn)
nase1fe <- as.data.frame(nasetmSpr$Fe)
nase1co <- as.data.frame(nasetmSpr$Co)
nase1ni <- as.data.frame(nasetmSpr$Ni)
nase1cu <- as.data.frame(nasetmSpr$Cu)
nase1zn <- as.data.frame(nasetmSpr$Zn)
nase1cd <- as.data.frame(nasetmSpr$Cd)
nase1temp <- as.data.frame(nasetmSpr$TEMPERATURE)
nase1no3 <- as.data.frame(nasetmSpr$NITRATE)

#Summer
nase2mn <- as.data.frame(nasetmSum$Mn)
nase2fe <- as.data.frame(nasetmSum$Fe)
nase2co <- as.data.frame(nasetmSum$Co)
nase2ni <- as.data.frame(nasetmSum$Ni)
nase2cu <- as.data.frame(nasetmSum$Cu)
nase2zn <- as.data.frame(nasetmSum$Zn)
nase2cd <- as.data.frame(nasetmSum$Cd)
nase2temp <- as.data.frame(nasetmSum$TEMPERATURE)
nase2no3 <- as.data.frame(nasetmSum$NITRATE)

#Autumn
nase3mn <- as.data.frame(nasetmAut$Mn)
nase3fe <- as.data.frame(nasetmAut$Fe)
nase3co <- as.data.frame(nasetmAut$Co)
nase3ni <- as.data.frame(nasetmAut$Ni)
nase3cu <- as.data.frame(nasetmAut$Cu)
nase3zn <- as.data.frame(nasetmAut$Zn)
nase3cd <- as.data.frame(nasetmAut$Cd)
nase3temp <- as.data.frame(nasetmAut$TEMPERATURE)
nase3no3 <- as.data.frame(nasetmAut$NITRATE)

#Winter
nase4mn <- as.data.frame(nasetmWin$Mn)
nase4fe <- as.data.frame(nasetmWin$Fe)
nase4co <- as.data.frame(nasetmWin$Co)
nase4ni <- as.data.frame(nasetmWin$Ni)
nase4cu <- as.data.frame(nasetmWin$Cu)
nase4zn <- as.data.frame(nasetmWin$Zn)
nase4cd <- as.data.frame(nasetmWin$Cd)
nase4temp <- as.data.frame(nasetmWin$TEMPERATURE)
nase4no3 <- as.data.frame(nasetmWin$NITRATE)


# NASW --------------------------------------------------------------------

naswtmSpr <- subset(nasw, nasw$SEASON==1)
naswtmSum <- subset(nasw, nasw$SEASON==2)
naswtmAut <- subset(nasw, nasw$SEASON==3)
naswtmWin <- subset(nasw, nasw$SEASON==4)

# Trace Metals
#Spring
nasw1mn <- as.data.frame(naswtmSpr$Mn)
nasw1fe <- as.data.frame(naswtmSpr$Fe)
nasw1co <- as.data.frame(naswtmSpr$Co)
nasw1ni <- as.data.frame(naswtmSpr$Ni)
nasw1cu <- as.data.frame(naswtmSpr$Cu)
nasw1zn <- as.data.frame(naswtmSpr$Zn)
nasw1cd <- as.data.frame(naswtmSpr$Cd)
nasw1temp <- as.data.frame(naswtmSpr$TEMPERATURE)
nasw1no3 <- as.data.frame(naswtmSpr$NITRATE)

#Summer
nasw2mn <- as.data.frame(naswtmSum$Mn)
nasw2fe <- as.data.frame(naswtmSum$Fe)
nasw2co <- as.data.frame(naswtmSum$Co)
nasw2ni <- as.data.frame(naswtmSum$Ni)
nasw2cu <- as.data.frame(naswtmSum$Cu)
nasw2zn <- as.data.frame(naswtmSum$Zn)
nasw2cd <- as.data.frame(naswtmSum$Cd)
nasw2temp <- as.data.frame(naswtmSum$TEMPERATURE)
nasw2no3 <- as.data.frame(naswtmSum$NITRATE)

#Autumn
nasw3mn <- as.data.frame(naswtmAut$Mn)
nasw3fe <- as.data.frame(naswtmAut$Fe)
nasw3co <- as.data.frame(naswtmAut$Co)
nasw3ni <- as.data.frame(naswtmAut$Ni)
nasw3cu <- as.data.frame(naswtmAut$Cu)
nasw3zn <- as.data.frame(naswtmAut$Zn)
nasw3cd <- as.data.frame(naswtmAut$Cd)
nasw3temp <- as.data.frame(naswtmAut$TEMPERATURE)
nasw3no3 <- as.data.frame(naswtmAut$NITRATE)

#Winter
nasw4mn <- as.data.frame(naswtmWin$Mn)
nasw4fe <- as.data.frame(naswtmWin$Fe)
nasw4co <- as.data.frame(naswtmWin$Co)
nasw4ni <- as.data.frame(naswtmWin$Ni)
nasw4cu <- as.data.frame(naswtmWin$Cu)
nasw4zn <- as.data.frame(naswtmWin$Zn)
nasw4cd <- as.data.frame(naswtmWin$Cd)
nasw4temp <- as.data.frame(naswtmWin$TEMPERATURE)
nasw4no3 <- as.data.frame(naswtmWin$NITRATE)

# NPTW --------------------------------------------------------------------

nptwtmSpr <- subset(nptw, nptw$SEASON==1)
nptwtmSum <- subset(nptw, nptw$SEASON==2)
nptwtmAut <- subset(nptw, nptw$SEASON==3)
nptwtmWin <- subset(nptw, nptw$SEASON==4)

# Trace Metals
#Spring
nptw1mn <- as.data.frame(nptwtmSpr$Mn)
nptw1fe <- as.data.frame(nptwtmSpr$Fe)
nptw1co <- as.data.frame(nptwtmSpr$Co)
nptw1ni <- as.data.frame(nptwtmSpr$Ni)
nptw1cu <- as.data.frame(nptwtmSpr$Cu)
nptw1zn <- as.data.frame(nptwtmSpr$Zn)
nptw1cd <- as.data.frame(nptwtmSpr$Cd)
nptw1temp <- as.data.frame(nptwtmSpr$TEMPERATURE)
nptw1no3 <- as.data.frame(nptwtmSpr$NITRATE)

#Summer
nptw2mn <- as.data.frame(nptwtmSum$Mn)
nptw2fe <- as.data.frame(nptwtmSum$Fe)
nptw2co <- as.data.frame(nptwtmSum$Co)
nptw2ni <- as.data.frame(nptwtmSum$Ni)
nptw2cu <- as.data.frame(nptwtmSum$Cu)
nptw2zn <- as.data.frame(nptwtmSum$Zn)
nptw2cd <- as.data.frame(nptwtmSum$Cd)
nptw2temp <- as.data.frame(nptwtmSum$TEMPERATURE)
nptw2no3 <- as.data.frame(nptwtmSum$NITRATE)

#Autumn
nptw3mn <- as.data.frame(nptwtmAut$Mn)
nptw3fe <- as.data.frame(nptwtmAut$Fe)
nptw3co <- as.data.frame(nptwtmAut$Co)
nptw3ni <- as.data.frame(nptwtmAut$Ni)
nptw3cu <- as.data.frame(nptwtmAut$Cu)
nptw3zn <- as.data.frame(nptwtmAut$Zn)
nptw3cd <- as.data.frame(nptwtmAut$Cd)
nptw3temp <- as.data.frame(nptwtmAut$TEMPERATURE)
nptw3no3 <- as.data.frame(nptwtmAut$NITRATE)

#Winter
nptw4mn <- as.data.frame(nptwtmWin$Mn)
nptw4fe <- as.data.frame(nptwtmWin$Fe)
nptw4co <- as.data.frame(nptwtmWin$Co)
nptw4ni <- as.data.frame(nptwtmWin$Ni)
nptw4cu <- as.data.frame(nptwtmWin$Cu)
nptw4zn <- as.data.frame(nptwtmWin$Zn)
nptw4cd <- as.data.frame(nptwtmWin$Cd)
nptw4temp <- as.data.frame(nptwtmWin$TEMPERATURE)
nptw4no3 <- as.data.frame(nptwtmWin$NITRATE)


# PSAE --------------------------------------------------------------------

psaetmSpr <- subset(psae, psae$SEASON==1)
psaetmSum <- subset(psae, psae$SEASON==2)
psaetmAut <- subset(psae, psae$SEASON==3)
psaetmWin <- subset(psae, psae$SEASON==4)

# Trace Metals
#Spring
psae1mn <- as.data.frame(psaetmSpr$Mn)
psae1fe <- as.data.frame(psaetmSpr$Fe)
psae1co <- as.data.frame(psaetmSpr$Co)
psae1ni <- as.data.frame(psaetmSpr$Ni)
psae1cu <- as.data.frame(psaetmSpr$Cu)
psae1zn <- as.data.frame(psaetmSpr$Zn)
psae1cd <- as.data.frame(psaetmSpr$Cd)
psae1temp <- as.data.frame(psaetmSpr$TEMPERATURE)
psae1no3 <- as.data.frame(psaetmSpr$NITRATE)

#Summer
psae2mn <- as.data.frame(psaetmSum$Mn)
psae2fe <- as.data.frame(psaetmSum$Fe)
psae2co <- as.data.frame(psaetmSum$Co)
psae2ni <- as.data.frame(psaetmSum$Ni)
psae2cu <- as.data.frame(psaetmSum$Cu)
psae2zn <- as.data.frame(psaetmSum$Zn)
psae2cd <- as.data.frame(psaetmSum$Cd)
psae2temp <- as.data.frame(psaetmSum$TEMPERATURE)
psae2no3 <- as.data.frame(psaetmSum$NITRATE)

#Autumn
psae3mn <- as.data.frame(psaetmAut$Mn)
psae3fe <- as.data.frame(psaetmAut$Fe)
psae3co <- as.data.frame(psaetmAut$Co)
psae3ni <- as.data.frame(psaetmAut$Ni)
psae3cu <- as.data.frame(psaetmAut$Cu)
psae3zn <- as.data.frame(psaetmAut$Zn)
psae3cd <- as.data.frame(psaetmAut$Cd)
psae3temp <- as.data.frame(psaetmAut$TEMPERATURE)
psae3no3 <- as.data.frame(psaetmAut$NITRATE)

#Winter
psae4mn <- as.data.frame(psaetmWin$Mn)
psae4fe <- as.data.frame(psaetmWin$Fe)
psae4co <- as.data.frame(psaetmWin$Co)
psae4ni <- as.data.frame(psaetmWin$Ni)
psae4cu <- as.data.frame(psaetmWin$Cu)
psae4zn <- as.data.frame(psaetmWin$Zn)
psae4cd <- as.data.frame(psaetmWin$Cd)
psae4temp <- as.data.frame(psaetmWin$TEMPERATURE)
psae4no3 <- as.data.frame(psaetmWin$NITRATE)


# SANT --------------------------------------------------------------------

santtmSpr <- subset(sant, sant$SEASON==1)
santtmSum <- subset(sant, sant$SEASON==2)
santtmAut <- subset(sant, sant$SEASON==3)
santtmWin <- subset(sant, sant$SEASON==4)

# Trace Metals
#Spring
sant1mn <- as.data.frame(santtmSpr$Mn)
sant1fe <- as.data.frame(santtmSpr$Fe)
sant1co <- as.data.frame(santtmSpr$Co)
sant1ni <- as.data.frame(santtmSpr$Ni)
sant1cu <- as.data.frame(santtmSpr$Cu)
sant1zn <- as.data.frame(santtmSpr$Zn)
sant1cd <- as.data.frame(santtmSpr$Cd)
sant1temp <- as.data.frame(santtmSpr$TEMPERATURE)
sant1no3 <- as.data.frame(santtmSpr$NITRATE)

#Summer
sant2mn <- as.data.frame(santtmSum$Mn)
sant2fe <- as.data.frame(santtmSum$Fe)
sant2co <- as.data.frame(santtmSum$Co)
sant2ni <- as.data.frame(santtmSum$Ni)
sant2cu <- as.data.frame(santtmSum$Cu)
sant2zn <- as.data.frame(santtmSum$Zn)
sant2cd <- as.data.frame(santtmSum$Cd)
sant2temp <- as.data.frame(santtmSum$TEMPERATURE)
sant2no3 <- as.data.frame(santtmSum$NITRATE)

#Autumn
sant3mn <- as.data.frame(santtmAut$Mn)
sant3fe <- as.data.frame(santtmAut$Fe)
sant3co <- as.data.frame(santtmAut$Co)
sant3ni <- as.data.frame(santtmAut$Ni)
sant3cu <- as.data.frame(santtmAut$Cu)
sant3zn <- as.data.frame(santtmAut$Zn)
sant3cd <- as.data.frame(santtmAut$Cd)
sant3temp <- as.data.frame(santtmAut$TEMPERATURE)
sant3no3 <- as.data.frame(santtmAut$NITRATE)

#Winter
sant4mn <- as.data.frame(santtmWin$Mn)
sant4fe <- as.data.frame(santtmWin$Fe)
sant4co <- as.data.frame(santtmWin$Co)
sant4ni <- as.data.frame(santtmWin$Ni)
sant4cu <- as.data.frame(santtmWin$Cu)
sant4zn <- as.data.frame(santtmWin$Zn)
sant4cd <- as.data.frame(santtmWin$Cd)
sant4temp <- as.data.frame(santtmWin$TEMPERATURE)
sant4no3 <- as.data.frame(santtmWin$NITRATE)


# SPSG --------------------------------------------------------------------

spsgtmSpr <- subset(spsg, spsg$SEASON==1)
spsgtmSum <- subset(spsg, spsg$SEASON==2)
spsgtmAut <- subset(spsg, spsg$SEASON==3)
spsgtmWin <- subset(spsg, spsg$SEASON==4)

# Trace Metals
#Spring
spsg1mn <- as.data.frame(spsgtmSpr$Mn)
spsg1fe <- as.data.frame(spsgtmSpr$Fe)
spsg1co <- as.data.frame(spsgtmSpr$Co)
spsg1ni <- as.data.frame(spsgtmSpr$Ni)
spsg1cu <- as.data.frame(spsgtmSpr$Cu)
spsg1zn <- as.data.frame(spsgtmSpr$Zn)
spsg1cd <- as.data.frame(spsgtmSpr$Cd)
spsg1temp <- as.data.frame(spsgtmSpr$TEMPERATURE)
spsg1no3 <- as.data.frame(spsgtmSpr$NITRATE)

#Summer
spsg2mn <- as.data.frame(spsgtmSum$Mn)
spsg2fe <- as.data.frame(spsgtmSum$Fe)
spsg2co <- as.data.frame(spsgtmSum$Co)
spsg2ni <- as.data.frame(spsgtmSum$Ni)
spsg2cu <- as.data.frame(spsgtmSum$Cu)
spsg2zn <- as.data.frame(spsgtmSum$Zn)
spsg2cd <- as.data.frame(spsgtmSum$Cd)
spsg2temp <- as.data.frame(spsgtmSum$TEMPERATURE)
spsg2no3 <- as.data.frame(spsgtmSum$NITRATE)

#Autumn
spsg3mn <- as.data.frame(spsgtmAut$Mn)
spsg3fe <- as.data.frame(spsgtmAut$Fe)
spsg3co <- as.data.frame(spsgtmAut$Co)
spsg3ni <- as.data.frame(spsgtmAut$Ni)
spsg3cu <- as.data.frame(spsgtmAut$Cu)
spsg3zn <- as.data.frame(spsgtmAut$Zn)
spsg3cd <- as.data.frame(spsgtmAut$Cd)
spsg3temp <- as.data.frame(spsgtmAut$TEMPERATURE)
spsg3no3 <- as.data.frame(spsgtmAut$NITRATE)

#Winter
spsg4mn <- as.data.frame(spsgtmWin$Mn)
spsg4fe <- as.data.frame(spsgtmWin$Fe)
spsg4co <- as.data.frame(spsgtmWin$Co)
spsg4ni <- as.data.frame(spsgtmWin$Ni)
spsg4cu <- as.data.frame(spsgtmWin$Cu)
spsg4zn <- as.data.frame(spsgtmWin$Zn)
spsg4cd <- as.data.frame(spsgtmWin$Cd)
spsg4temp <- as.data.frame(spsgtmWin$TEMPERATURE)
spsg4no3 <- as.data.frame(spsgtmWin$NITRATE)


# SSTC --------------------------------------------------------------------

sstctmSpr <- subset(sstc, sstc$SEASON==1)
sstctmSum <- subset(sstc, sstc$SEASON==2)
sstctmAut <- subset(sstc, sstc$SEASON==3)
sstctmWin <- subset(sstc, sstc$SEASON==4)

# Trace Metals
#Spring
sstc1mn <- as.data.frame(sstctmSpr$Mn)
sstc1fe <- as.data.frame(sstctmSpr$Fe)
sstc1co <- as.data.frame(sstctmSpr$Co)
sstc1ni <- as.data.frame(sstctmSpr$Ni)
sstc1cu <- as.data.frame(sstctmSpr$Cu)
sstc1zn <- as.data.frame(sstctmSpr$Zn)
sstc1cd <- as.data.frame(sstctmSpr$Cd)
sstc1temp <- as.data.frame(sstctmSpr$TEMPERATURE)
sstc1no3 <- as.data.frame(sstctmSpr$NITRATE)

#Summer
sstc2mn <- as.data.frame(sstctmSum$Mn)
sstc2fe <- as.data.frame(sstctmSum$Fe)
sstc2co <- as.data.frame(sstctmSum$Co)
sstc2ni <- as.data.frame(sstctmSum$Ni)
sstc2cu <- as.data.frame(sstctmSum$Cu)
sstc2zn <- as.data.frame(sstctmSum$Zn)
sstc2cd <- as.data.frame(sstctmSum$Cd)
sstc2temp <- as.data.frame(sstctmSum$TEMPERATURE)
sstc2no3 <- as.data.frame(sstctmSum$NITRATE)

#Autumn
sstc3mn <- as.data.frame(sstctmAut$Mn)
sstc3fe <- as.data.frame(sstctmAut$Fe)
sstc3co <- as.data.frame(sstctmAut$Co)
sstc3ni <- as.data.frame(sstctmAut$Ni)
sstc3cu <- as.data.frame(sstctmAut$Cu)
sstc3zn <- as.data.frame(sstctmAut$Zn)
sstc3cd <- as.data.frame(sstctmAut$Cd)
sstc3temp <- as.data.frame(sstctmAut$TEMPERATURE)
sstc3no3 <- as.data.frame(sstctmAut$NITRATE)

#Winter
sstc4mn <- as.data.frame(sstctmWin$Mn)
sstc4fe <- as.data.frame(sstctmWin$Fe)
sstc4co <- as.data.frame(sstctmWin$Co)
sstc4ni <- as.data.frame(sstctmWin$Ni)
sstc4cu <- as.data.frame(sstctmWin$Cu)
sstc4zn <- as.data.frame(sstctmWin$Zn)
sstc4cd <- as.data.frame(sstctmWin$Cd)
sstc4temp <- as.data.frame(sstctmWin$TEMPERATURE)
sstc4no3 <- as.data.frame(sstctmWin$NITRATE)


# ARCH --------------------------------------------------------------------

archtmSpr <- subset(arch, arch$SEASON==1)
archtmSum <- subset(arch, arch$SEASON==2)
archtmAut <- subset(arch, arch$SEASON==3)
archtmWin <- subset(arch, arch$SEASON==4)

# Trace Metals
#Spring
arch1mn <- as.data.frame(archtmSpr$Mn)
arch1fe <- as.data.frame(archtmSpr$Fe)
arch1co <- as.data.frame(archtmSpr$Co)
arch1ni <- as.data.frame(archtmSpr$Ni)
arch1cu <- as.data.frame(archtmSpr$Cu)
arch1zn <- as.data.frame(archtmSpr$Zn)
arch1cd <- as.data.frame(archtmSpr$Cd)
arch1temp <- as.data.frame(archtmSpr$TEMPERATURE)
arch1no3 <- as.data.frame(archtmSpr$NITRATE)

#Summer
arch2mn <- as.data.frame(archtmSum$Mn)
arch2fe <- as.data.frame(archtmSum$Fe)
arch2co <- as.data.frame(archtmSum$Co)
arch2ni <- as.data.frame(archtmSum$Ni)
arch2cu <- as.data.frame(archtmSum$Cu)
arch2zn <- as.data.frame(archtmSum$Zn)
arch2cd <- as.data.frame(archtmSum$Cd)
arch2temp <- as.data.frame(archtmSum$TEMPERATURE)
arch2no3 <- as.data.frame(archtmSum$NITRATE)

#Autumn
arch3mn <- as.data.frame(archtmAut$Mn)
arch3fe <- as.data.frame(archtmAut$Fe)
arch3co <- as.data.frame(archtmAut$Co)
arch3ni <- as.data.frame(archtmAut$Ni)
arch3cu <- as.data.frame(archtmAut$Cu)
arch3zn <- as.data.frame(archtmAut$Zn)
arch3cd <- as.data.frame(archtmAut$Cd)
arch3temp <- as.data.frame(archtmAut$TEMPERATURE)
arch3no3 <- as.data.frame(archtmAut$NITRATE)

#Winter
arch4mn <- as.data.frame(archtmWin$Mn)
arch4fe <- as.data.frame(archtmWin$Fe)
arch4co <- as.data.frame(archtmWin$Co)
arch4ni <- as.data.frame(archtmWin$Ni)
arch4cu <- as.data.frame(archtmWin$Cu)
arch4zn <- as.data.frame(archtmWin$Zn)
arch4cd <- as.data.frame(archtmWin$Cd)
arch4temp <- as.data.frame(archtmWin$TEMPERATURE)
arch4no3 <- as.data.frame(archtmWin$NITRATE)


# ISSG --------------------------------------------------------------------

issgtmSpr <- subset(issg, issg$SEASON==1)
issgtmSum <- subset(issg, issg$SEASON==2)
issgtmAut <- subset(issg, issg$SEASON==3)
issgtmWin <- subset(issg, issg$SEASON==4)

# Trace Metals
#Spring
issg1mn <- as.data.frame(issgtmSpr$Mn)
issg1fe <- as.data.frame(issgtmSpr$Fe)
issg1co <- as.data.frame(issgtmSpr$Co)
issg1ni <- as.data.frame(issgtmSpr$Ni)
issg1cu <- as.data.frame(issgtmSpr$Cu)
issg1zn <- as.data.frame(issgtmSpr$Zn)
issg1cd <- as.data.frame(issgtmSpr$Cd)
issg1temp <- as.data.frame(issgtmSpr$TEMPERATURE)
issg1no3 <- as.data.frame(issgtmSpr$NITRATE)

#Summer
issg2mn <- as.data.frame(issgtmSum$Mn)
issg2fe <- as.data.frame(issgtmSum$Fe)
issg2co <- as.data.frame(issgtmSum$Co)
issg2ni <- as.data.frame(issgtmSum$Ni)
issg2cu <- as.data.frame(issgtmSum$Cu)
issg2zn <- as.data.frame(issgtmSum$Zn)
issg2cd <- as.data.frame(issgtmSum$Cd)
issg2temp <- as.data.frame(issgtmSum$TEMPERATURE)
issg2no3 <- as.data.frame(issgtmSum$NITRATE)

#Autumn
issg3mn <- as.data.frame(issgtmAut$Mn)
issg3fe <- as.data.frame(issgtmAut$Fe)
issg3co <- as.data.frame(issgtmAut$Co)
issg3ni <- as.data.frame(issgtmAut$Ni)
issg3cu <- as.data.frame(issgtmAut$Cu)
issg3zn <- as.data.frame(issgtmAut$Zn)
issg3cd <- as.data.frame(issgtmAut$Cd)
issg3temp <- as.data.frame(issgtmAut$TEMPERATURE)
issg3no3 <- as.data.frame(issgtmAut$NITRATE)

#Winter
issg4mn <- as.data.frame(issgtmWin$Mn)
issg4fe <- as.data.frame(issgtmWin$Fe)
issg4co <- as.data.frame(issgtmWin$Co)
issg4ni <- as.data.frame(issgtmWin$Ni)
issg4cu <- as.data.frame(issgtmWin$Cu)
issg4zn <- as.data.frame(issgtmWin$Zn)
issg4cd <- as.data.frame(issgtmWin$Cd)
issg4temp <- as.data.frame(issgtmWin$TEMPERATURE)
issg4no3 <- as.data.frame(issgtmWin$NITRATE)

# MONS --------------------------------------------------------------------

monstmSpr <- subset(mons, mons$SEASON==1)
monstmSum <- subset(mons, mons$SEASON==2)
monstmAut <- subset(mons, mons$SEASON==3)
monstmWin <- subset(mons, mons$SEASON==4)

# Trace Metals
#Spring
mons1mn <- as.data.frame(monstmSpr$Mn)
mons1fe <- as.data.frame(monstmSpr$Fe)
mons1co <- as.data.frame(monstmSpr$Co)
mons1ni <- as.data.frame(monstmSpr$Ni)
mons1cu <- as.data.frame(monstmSpr$Cu)
mons1zn <- as.data.frame(monstmSpr$Zn)
mons1cd <- as.data.frame(monstmSpr$Cd)
mons1temp <- as.data.frame(monstmSpr$TEMPERATURE)
mons1no3 <- as.data.frame(monstmSpr$NITRATE)

#Summer
mons2mn <- as.data.frame(monstmSum$Mn)
mons2fe <- as.data.frame(monstmSum$Fe)
mons2co <- as.data.frame(monstmSum$Co)
mons2ni <- as.data.frame(monstmSum$Ni)
mons2cu <- as.data.frame(monstmSum$Cu)
mons2zn <- as.data.frame(monstmSum$Zn)
mons2cd <- as.data.frame(monstmSum$Cd)
mons2temp <- as.data.frame(monstmSum$TEMPERATURE)
mons2no3 <- as.data.frame(monstmSum$NITRATE)

#Autumn
mons3mn <- as.data.frame(monstmAut$Mn)
mons3fe <- as.data.frame(monstmAut$Fe)
mons3co <- as.data.frame(monstmAut$Co)
mons3ni <- as.data.frame(monstmAut$Ni)
mons3cu <- as.data.frame(monstmAut$Cu)
mons3zn <- as.data.frame(monstmAut$Zn)
mons3cd <- as.data.frame(monstmAut$Cd)
mons3temp <- as.data.frame(monstmAut$TEMPERATURE)
mons3no3 <- as.data.frame(monstmAut$NITRATE)

#Winter
mons4mn <- as.data.frame(monstmWin$Mn)
mons4fe <- as.data.frame(monstmWin$Fe)
mons4co <- as.data.frame(monstmWin$Co)
mons4ni <- as.data.frame(monstmWin$Ni)
mons4cu <- as.data.frame(monstmWin$Cu)
mons4zn <- as.data.frame(monstmWin$Zn)
mons4cd <- as.data.frame(monstmWin$Cd)
mons4temp <- as.data.frame(monstmWin$TEMPERATURE)
mons4no3 <- as.data.frame(monstmWin$NITRATE)


# NATR --------------------------------------------------------------------

natrtmSpr <- subset(natr, natr$SEASON==1)
natrtmSum <- subset(natr, natr$SEASON==2)
natrtmAut <- subset(natr, natr$SEASON==3)
natrtmWin <- subset(natr, natr$SEASON==4)

# Trace Metals
#Spring
natr1mn <- as.data.frame(natrtmSpr$Mn)
natr1fe <- as.data.frame(natrtmSpr$Fe)
natr1co <- as.data.frame(natrtmSpr$Co)
natr1ni <- as.data.frame(natrtmSpr$Ni)
natr1cu <- as.data.frame(natrtmSpr$Cu)
natr1zn <- as.data.frame(natrtmSpr$Zn)
natr1cd <- as.data.frame(natrtmSpr$Cd)
natr1temp <- as.data.frame(natrtmSpr$TEMPERATURE)
natr1no3 <- as.data.frame(natrtmSpr$NITRATE)

#Summer
natr2mn <- as.data.frame(natrtmSum$Mn)
natr2fe <- as.data.frame(natrtmSum$Fe)
natr2co <- as.data.frame(natrtmSum$Co)
natr2ni <- as.data.frame(natrtmSum$Ni)
natr2cu <- as.data.frame(natrtmSum$Cu)
natr2zn <- as.data.frame(natrtmSum$Zn)
natr2cd <- as.data.frame(natrtmSum$Cd)
natr2temp <- as.data.frame(natrtmSum$TEMPERATURE)
natr2no3 <- as.data.frame(natrtmSum$NITRATE)

#Autumn
natr3mn <- as.data.frame(natrtmAut$Mn)
natr3fe <- as.data.frame(natrtmAut$Fe)
natr3co <- as.data.frame(natrtmAut$Co)
natr3ni <- as.data.frame(natrtmAut$Ni)
natr3cu <- as.data.frame(natrtmAut$Cu)
natr3zn <- as.data.frame(natrtmAut$Zn)
natr3cd <- as.data.frame(natrtmAut$Cd)
natr3temp <- as.data.frame(natrtmAut$TEMPERATURE)
natr3no3 <- as.data.frame(natrtmAut$NITRATE)

#Winter
natr4mn <- as.data.frame(natrtmWin$Mn)
natr4fe <- as.data.frame(natrtmWin$Fe)
natr4co <- as.data.frame(natrtmWin$Co)
natr4ni <- as.data.frame(natrtmWin$Ni)
natr4cu <- as.data.frame(natrtmWin$Cu)
natr4zn <- as.data.frame(natrtmWin$Zn)
natr4cd <- as.data.frame(natrtmWin$Cd)
natr4temp <- as.data.frame(natrtmWin$TEMPERATURE)
natr4no3 <- as.data.frame(natrtmWin$NITRATE)


# NPTE --------------------------------------------------------------------

nptetmSpr <- subset(npte, npte$SEASON==1)
nptetmSum <- subset(npte, npte$SEASON==2)
nptetmAut <- subset(npte, npte$SEASON==3)
nptetmWin <- subset(npte, npte$SEASON==4)

# Trace Metals
#Spring
npte1mn <- as.data.frame(nptetmSpr$Mn)
npte1fe <- as.data.frame(nptetmSpr$Fe)
npte1co <- as.data.frame(nptetmSpr$Co)
npte1ni <- as.data.frame(nptetmSpr$Ni)
npte1cu <- as.data.frame(nptetmSpr$Cu)
npte1zn <- as.data.frame(nptetmSpr$Zn)
npte1cd <- as.data.frame(nptetmSpr$Cd)
npte1temp <- as.data.frame(nptetmSpr$TEMPERATURE)
npte1no3 <- as.data.frame(nptetmSpr$NITRATE)

#Summer
npte2mn <- as.data.frame(nptetmSum$Mn)
npte2fe <- as.data.frame(nptetmSum$Fe)
npte2co <- as.data.frame(nptetmSum$Co)
npte2ni <- as.data.frame(nptetmSum$Ni)
npte2cu <- as.data.frame(nptetmSum$Cu)
npte2zn <- as.data.frame(nptetmSum$Zn)
npte2cd <- as.data.frame(nptetmSum$Cd)
npte2temp <- as.data.frame(nptetmSum$TEMPERATURE)
npte2no3 <- as.data.frame(nptetmSum$NITRATE)

#Autumn
npte3mn <- as.data.frame(nptetmAut$Mn)
npte3fe <- as.data.frame(nptetmAut$Fe)
npte3co <- as.data.frame(nptetmAut$Co)
npte3ni <- as.data.frame(nptetmAut$Ni)
npte3cu <- as.data.frame(nptetmAut$Cu)
npte3zn <- as.data.frame(nptetmAut$Zn)
npte3cd <- as.data.frame(nptetmAut$Cd)
npte3temp <- as.data.frame(nptetmAut$TEMPERATURE)
npte3no3 <- as.data.frame(nptetmAut$NITRATE)

#Winter
npte4mn <- as.data.frame(nptetmWin$Mn)
npte4fe <- as.data.frame(nptetmWin$Fe)
npte4co <- as.data.frame(nptetmWin$Co)
npte4ni <- as.data.frame(nptetmWin$Ni)
npte4cu <- as.data.frame(nptetmWin$Cu)
npte4zn <- as.data.frame(nptetmWin$Zn)
npte4cd <- as.data.frame(nptetmWin$Cd)
npte4temp <- as.data.frame(nptetmWin$TEMPERATURE)
npte4no3 <- as.data.frame(nptetmWin$NITRATE)


# PEQD --------------------------------------------------------------------

peqdtmSpr <- subset(peqd, peqd$SEASON==1)
peqdtmSum <- subset(peqd, peqd$SEASON==2)
peqdtmAut <- subset(peqd, peqd$SEASON==3)
peqdtmWin <- subset(peqd, peqd$SEASON==4)

# Trace Metals
#Spring
peqd1mn <- as.data.frame(peqdtmSpr$Mn)
peqd1fe <- as.data.frame(peqdtmSpr$Fe)
peqd1co <- as.data.frame(peqdtmSpr$Co)
peqd1ni <- as.data.frame(peqdtmSpr$Ni)
peqd1cu <- as.data.frame(peqdtmSpr$Cu)
peqd1zn <- as.data.frame(peqdtmSpr$Zn)
peqd1cd <- as.data.frame(peqdtmSpr$Cd)
peqd1temp <- as.data.frame(peqdtmSpr$TEMPERATURE)
peqd1no3 <- as.data.frame(peqdtmSpr$NITRATE)

#Summer
peqd2mn <- as.data.frame(peqdtmSum$Mn)
peqd2fe <- as.data.frame(peqdtmSum$Fe)
peqd2co <- as.data.frame(peqdtmSum$Co)
peqd2ni <- as.data.frame(peqdtmSum$Ni)
peqd2cu <- as.data.frame(peqdtmSum$Cu)
peqd2zn <- as.data.frame(peqdtmSum$Zn)
peqd2cd <- as.data.frame(peqdtmSum$Cd)
peqd2temp <- as.data.frame(peqdtmSum$TEMPERATURE)
peqd2no3 <- as.data.frame(peqdtmSum$NITRATE)

#Autumn
peqd3mn <- as.data.frame(peqdtmAut$Mn)
peqd3fe <- as.data.frame(peqdtmAut$Fe)
peqd3co <- as.data.frame(peqdtmAut$Co)
peqd3ni <- as.data.frame(peqdtmAut$Ni)
peqd3cu <- as.data.frame(peqdtmAut$Cu)
peqd3zn <- as.data.frame(peqdtmAut$Zn)
peqd3cd <- as.data.frame(peqdtmAut$Cd)
peqd3temp <- as.data.frame(peqdtmAut$TEMPERATURE)
peqd3no3 <- as.data.frame(peqdtmAut$NITRATE)

#Winter
peqd4mn <- as.data.frame(peqdtmWin$Mn)
peqd4fe <- as.data.frame(peqdtmWin$Fe)
peqd4co <- as.data.frame(peqdtmWin$Co)
peqd4ni <- as.data.frame(peqdtmWin$Ni)
peqd4cu <- as.data.frame(peqdtmWin$Cu)
peqd4zn <- as.data.frame(peqdtmWin$Zn)
peqd4cd <- as.data.frame(peqdtmWin$Cd)
peqd4temp <- as.data.frame(peqdtmWin$TEMPERATURE)
peqd4no3 <- as.data.frame(peqdtmWin$NITRATE)


# PNEC --------------------------------------------------------------------

pnectmSpr <- subset(pnec, pnec$SEASON==1)
pnectmSum <- subset(pnec, pnec$SEASON==2)
pnectmAut <- subset(pnec, pnec$SEASON==3)
pnectmWin <- subset(pnec, pnec$SEASON==4)

# Trace Metals
#Spring
pnec1mn <- as.data.frame(pnectmSpr$Mn)
pnec1fe <- as.data.frame(pnectmSpr$Fe)
pnec1co <- as.data.frame(pnectmSpr$Co)
pnec1ni <- as.data.frame(pnectmSpr$Ni)
pnec1cu <- as.data.frame(pnectmSpr$Cu)
pnec1zn <- as.data.frame(pnectmSpr$Zn)
pnec1cd <- as.data.frame(pnectmSpr$Cd)
pnec1temp <- as.data.frame(pnectmSpr$TEMPERATURE)
pnec1no3 <- as.data.frame(pnectmSpr$NITRATE)

#Summer
pnec2mn <- as.data.frame(pnectmSum$Mn)
pnec2fe <- as.data.frame(pnectmSum$Fe)
pnec2co <- as.data.frame(pnectmSum$Co)
pnec2ni <- as.data.frame(pnectmSum$Ni)
pnec2cu <- as.data.frame(pnectmSum$Cu)
pnec2zn <- as.data.frame(pnectmSum$Zn)
pnec2cd <- as.data.frame(pnectmSum$Cd)
pnec2temp <- as.data.frame(pnectmSum$TEMPERATURE)
pnec2no3 <- as.data.frame(pnectmSum$NITRATE)

#Autumn
pnec3mn <- as.data.frame(pnectmAut$Mn)
pnec3fe <- as.data.frame(pnectmAut$Fe)
pnec3co <- as.data.frame(pnectmAut$Co)
pnec3ni <- as.data.frame(pnectmAut$Ni)
pnec3cu <- as.data.frame(pnectmAut$Cu)
pnec3zn <- as.data.frame(pnectmAut$Zn)
pnec3cd <- as.data.frame(pnectmAut$Cd)
pnec3temp <- as.data.frame(pnectmAut$TEMPERATURE)
pnec3no3 <- as.data.frame(pnectmAut$NITRATE)

#Winter
pnec4mn <- as.data.frame(pnectmWin$Mn)
pnec4fe <- as.data.frame(pnectmWin$Fe)
pnec4co <- as.data.frame(pnectmWin$Co)
pnec4ni <- as.data.frame(pnectmWin$Ni)
pnec4cu <- as.data.frame(pnectmWin$Cu)
pnec4zn <- as.data.frame(pnectmWin$Zn)
pnec4cd <- as.data.frame(pnectmWin$Cd)
pnec4temp <- as.data.frame(pnectmWin$TEMPERATURE)
pnec4no3 <- as.data.frame(pnectmWin$NITRATE)


# SATL --------------------------------------------------------------------

satltmSpr <- subset(satl, satl$SEASON==1)
satltmSum <- subset(satl, satl$SEASON==2)
satltmAut <- subset(satl, satl$SEASON==3)
satltmWin <- subset(satl, satl$SEASON==4)

# Trace Metals
#Spring
satl1mn <- as.data.frame(satltmSpr$Mn)
satl1fe <- as.data.frame(satltmSpr$Fe)
satl1co <- as.data.frame(satltmSpr$Co)
satl1ni <- as.data.frame(satltmSpr$Ni)
satl1cu <- as.data.frame(satltmSpr$Cu)
satl1zn <- as.data.frame(satltmSpr$Zn)
satl1cd <- as.data.frame(satltmSpr$Cd)
satl1temp <- as.data.frame(satltmSpr$TEMPERATURE)
satl1no3 <- as.data.frame(satltmSpr$NITRATE)

#Summer
satl2mn <- as.data.frame(satltmSum$Mn)
satl2fe <- as.data.frame(satltmSum$Fe)
satl2co <- as.data.frame(satltmSum$Co)
satl2ni <- as.data.frame(satltmSum$Ni)
satl2cu <- as.data.frame(satltmSum$Cu)
satl2zn <- as.data.frame(satltmSum$Zn)
satl2cd <- as.data.frame(satltmSum$Cd)
satl2temp <- as.data.frame(satltmSum$TEMPERATURE)
satl2no3 <- as.data.frame(satltmSum$NITRATE)

#Autumn
satl3mn <- as.data.frame(satltmAut$Mn)
satl3fe <- as.data.frame(satltmAut$Fe)
satl3co <- as.data.frame(satltmAut$Co)
satl3ni <- as.data.frame(satltmAut$Ni)
satl3cu <- as.data.frame(satltmAut$Cu)
satl3zn <- as.data.frame(satltmAut$Zn)
satl3cd <- as.data.frame(satltmAut$Cd)
satl3temp <- as.data.frame(satltmAut$TEMPERATURE)
satl3no3 <- as.data.frame(satltmAut$NITRATE)

#Winter
satl4mn <- as.data.frame(satltmWin$Mn)
satl4fe <- as.data.frame(satltmWin$Fe)
satl4co <- as.data.frame(satltmWin$Co)
satl4ni <- as.data.frame(satltmWin$Ni)
satl4cu <- as.data.frame(satltmWin$Cu)
satl4zn <- as.data.frame(satltmWin$Zn)
satl4cd <- as.data.frame(satltmWin$Cd)
satl4temp <- as.data.frame(satltmWin$TEMPERATURE)
satl4no3 <- as.data.frame(satltmWin$NITRATE)


# WARM --------------------------------------------------------------------

warmtmSpr <- subset(warm, warm$SEASON==1)
warmtmSum <- subset(warm, warm$SEASON==2)
warmtmAut <- subset(warm, warm$SEASON==3)
warmtmWin <- subset(warm, warm$SEASON==4)

# Trace Metals
#Spring
warm1mn <- as.data.frame(warmtmSpr$Mn)
warm1fe <- as.data.frame(warmtmSpr$Fe)
warm1co <- as.data.frame(warmtmSpr$Co)
warm1ni <- as.data.frame(warmtmSpr$Ni)
warm1cu <- as.data.frame(warmtmSpr$Cu)
warm1zn <- as.data.frame(warmtmSpr$Zn)
warm1cd <- as.data.frame(warmtmSpr$Cd)
warm1temp <- as.data.frame(warmtmSpr$TEMPERATURE)
warm1no3 <- as.data.frame(warmtmSpr$NITRATE)

#Summer
warm2mn <- as.data.frame(warmtmSum$Mn)
warm2fe <- as.data.frame(warmtmSum$Fe)
warm2co <- as.data.frame(warmtmSum$Co)
warm2ni <- as.data.frame(warmtmSum$Ni)
warm2cu <- as.data.frame(warmtmSum$Cu)
warm2zn <- as.data.frame(warmtmSum$Zn)
warm2cd <- as.data.frame(warmtmSum$Cd)
warm2temp <- as.data.frame(warmtmSum$TEMPERATURE)
warm2no3 <- as.data.frame(warmtmSum$NITRATE)

#Autumn
warm3mn <- as.data.frame(warmtmAut$Mn)
warm3fe <- as.data.frame(warmtmAut$Fe)
warm3co <- as.data.frame(warmtmAut$Co)
warm3ni <- as.data.frame(warmtmAut$Ni)
warm3cu <- as.data.frame(warmtmAut$Cu)
warm3zn <- as.data.frame(warmtmAut$Zn)
warm3cd <- as.data.frame(warmtmAut$Cd)
warm3temp <- as.data.frame(warmtmAut$TEMPERATURE)
warm3no3 <- as.data.frame(warmtmAut$NITRATE)

#Winter
warm4mn <- as.data.frame(warmtmWin$Mn)
warm4fe <- as.data.frame(warmtmWin$Fe)
warm4co <- as.data.frame(warmtmWin$Co)
warm4ni <- as.data.frame(warmtmWin$Ni)
warm4cu <- as.data.frame(warmtmWin$Cu)
warm4zn <- as.data.frame(warmtmWin$Zn)
warm4cd <- as.data.frame(warmtmWin$Cd)
warm4temp <- as.data.frame(warmtmWin$TEMPERATURE)
warm4no3 <- as.data.frame(warmtmWin$NITRATE)


# WTRA --------------------------------------------------------------------

wtratmSpr <- subset(wtra, wtra$SEASON==1)
wtratmSum <- subset(wtra, wtra$SEASON==2)
wtratmAut <- subset(wtra, wtra$SEASON==3)
wtratmWin <- subset(wtra, wtra$SEASON==4)

# Trace Metals
#Spring
wtra1mn <- as.data.frame(wtratmSpr$Mn)
wtra1fe <- as.data.frame(wtratmSpr$Fe)
wtra1co <- as.data.frame(wtratmSpr$Co)
wtra1ni <- as.data.frame(wtratmSpr$Ni)
wtra1cu <- as.data.frame(wtratmSpr$Cu)
wtra1zn <- as.data.frame(wtratmSpr$Zn)
wtra1cd <- as.data.frame(wtratmSpr$Cd)
wtra1temp <- as.data.frame(wtratmSpr$TEMPERATURE)
wtra1no3 <- as.data.frame(wtratmSpr$NITRATE)

#Summer
wtra2mn <- as.data.frame(wtratmSum$Mn)
wtra2fe <- as.data.frame(wtratmSum$Fe)
wtra2co <- as.data.frame(wtratmSum$Co)
wtra2ni <- as.data.frame(wtratmSum$Ni)
wtra2cu <- as.data.frame(wtratmSum$Cu)
wtra2zn <- as.data.frame(wtratmSum$Zn)
wtra2cd <- as.data.frame(wtratmSum$Cd)
wtra2temp <- as.data.frame(wtratmSum$TEMPERATURE)
wtra2no3 <- as.data.frame(wtratmSum$NITRATE)

#Autumn
wtra3mn <- as.data.frame(wtratmAut$Mn)
wtra3fe <- as.data.frame(wtratmAut$Fe)
wtra3co <- as.data.frame(wtratmAut$Co)
wtra3ni <- as.data.frame(wtratmAut$Ni)
wtra3cu <- as.data.frame(wtratmAut$Cu)
wtra3zn <- as.data.frame(wtratmAut$Zn)
wtra3cd <- as.data.frame(wtratmAut$Cd)
wtra3temp <- as.data.frame(wtratmAut$TEMPERATURE)
wtra3no3 <- as.data.frame(wtratmAut$NITRATE)

#Winter
wtra4mn <- as.data.frame(wtratmWin$Mn)
wtra4fe <- as.data.frame(wtratmWin$Fe)
wtra4co <- as.data.frame(wtratmWin$Co)
wtra4ni <- as.data.frame(wtratmWin$Ni)
wtra4cu <- as.data.frame(wtratmWin$Cu)
wtra4zn <- as.data.frame(wtratmWin$Zn)
wtra4cd <- as.data.frame(wtratmWin$Cd)
wtra4temp <- as.data.frame(wtratmWin$TEMPERATURE)
wtra4no3 <- as.data.frame(wtratmWin$NITRATE)





# Fe ----------------------------------------------------------------------

setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1fe[!is.na(arct1fe)])
colnames(da)<-c("no")
db<-as.data.frame(arct2fe[!is.na(arct2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3fe[!is.na(arct3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4fe[!is.na(arct4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctfe<-rbind(da,db,dc,dd)
a<-cbind(arctfe,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1fe[!is.na(bplr1fe)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2fe[!is.na(bplr2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3fe[!is.na(bplr3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4fe[!is.na(bplr4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrfe<-rbind(da,db,dc,dd)
b<-cbind(bplrfe,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1fe[!is.na(sarc1fe)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2fe[!is.na(sarc2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3fe[!is.na(sarc3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4fe[!is.na(sarc4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarcfe<-rbind(da,db,dc,dd)
c<-cbind(sarcfe,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1fe[!is.na(anta1fe)])
colnames(da)<-c("no")
db<-as.data.frame(anta2fe[!is.na(anta2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3fe[!is.na(anta3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4fe[!is.na(anta4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antafe<-rbind(da,db,dc,dd)
d<-cbind(antafe,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1fe[!is.na(aplr1fe)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2fe[!is.na(aplr2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3fe[!is.na(aplr3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4fe[!is.na(aplr4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrfe<-rbind(da,db,dc,dd)
e<-cbind(aplrfe,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1fe[!is.na(gest1fe)])
colnames(da)<-c("no")
db<-as.data.frame(gest2fe[!is.na(gest2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3fe[!is.na(gest3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4fe[!is.na(gest4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestfe<-rbind(da,db,dc,dd)
f<-cbind(gestfe,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1fe[!is.na(nadr1fe)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2fe[!is.na(nadr2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3fe[!is.na(nadr3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4fe[!is.na(nadr4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrfe<-rbind(da,db,dc,dd)
g<-cbind(nadrfe,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1fe[!is.na(nase1fe)])
colnames(da)<-c("no")
db<-as.data.frame(nase2fe[!is.na(nase2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3fe[!is.na(nase3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4fe[!is.na(nase4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
nasefe<-rbind(da,db,dc,dd)
h<-cbind(nasefe,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1fe[!is.na(nasw1fe)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2fe[!is.na(nasw2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3fe[!is.na(nasw3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4fe[!is.na(nasw4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswfe<-rbind(da,db,dc,dd)
i<-cbind(naswfe,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1fe[!is.na(nptw1fe)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2fe[!is.na(nptw2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3fe[!is.na(nptw3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4fe[!is.na(nptw4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwfe<-rbind(da,db,dc,dd)
j<-cbind(nptwfe,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1fe[!is.na(psae1fe)])
colnames(da)<-c("no")
db<-as.data.frame(psae2fe[!is.na(psae2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3fe[!is.na(psae3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4fe[!is.na(psae4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaefe<-rbind(da,db,dc,dd)
k<-cbind(psaefe,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1fe[!is.na(sant1fe)])
colnames(da)<-c("no")
db<-as.data.frame(sant2fe[!is.na(sant2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3fe[!is.na(sant3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4fe[!is.na(sant4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santfe<-rbind(da,db,dc,dd)
l<-cbind(santfe,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1fe[!is.na(spsg1fe)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2fe[!is.na(spsg2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3fe[!is.na(spsg3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4fe[!is.na(spsg4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgfe<-rbind(da,db,dc,dd)
m<-cbind(spsgfe,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1fe[!is.na(sstc1fe)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2fe[!is.na(sstc2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3fe[!is.na(sstc3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4fe[!is.na(sstc4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstcfe<-rbind(da,db,dc,dd)
n<-cbind(sstcfe,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1fe[!is.na(arch1fe)])
colnames(da)<-c("no")
db<-as.data.frame(arch2fe[!is.na(arch2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3fe[!is.na(arch3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4fe[!is.na(arch4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archfe<-rbind(da,db,dc,dd)
o<-cbind(archfe,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1fe[!is.na(issg1fe)])
colnames(da)<-c("no")
db<-as.data.frame(issg2fe[!is.na(issg2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3fe[!is.na(issg3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4fe[!is.na(issg4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgfe<-rbind(da,db,dc,dd)
p<-cbind(issgfe,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1fe[!is.na(mons1fe)])
colnames(da)<-c("no")
db<-as.data.frame(mons2fe[!is.na(mons2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3fe[!is.na(mons3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4fe[!is.na(mons4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monsfe<-rbind(da,db,dc,dd)
q<-cbind(monsfe,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1fe[!is.na(natr1fe)])
colnames(da)<-c("no")
db<-as.data.frame(natr2fe[!is.na(natr2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3fe[!is.na(natr3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4fe[!is.na(natr4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrfe<-rbind(da,db,dc,dd)
r<-cbind(natrfe,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1fe[!is.na(npte1fe)])
colnames(da)<-c("no")
db<-as.data.frame(npte2fe[!is.na(npte2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3fe[!is.na(npte3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4fe[!is.na(npte4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
nptefe<-rbind(da,db,dc,dd)
s<-cbind(nptefe,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1fe[!is.na(peqd1fe)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2fe[!is.na(peqd2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3fe[!is.na(peqd3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4fe[!is.na(peqd4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdfe<-rbind(da,db,dc,dd)
t<-cbind(peqdfe,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1fe[!is.na(pnec1fe)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2fe[!is.na(pnec2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3fe[!is.na(pnec3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4fe[!is.na(pnec4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pnecfe<-rbind(da,db,dc,dd)
u<-cbind(pnecfe,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1fe[!is.na(satl1fe)])
colnames(da)<-c("no")
db<-as.data.frame(satl2fe[!is.na(satl2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3fe[!is.na(satl3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4fe[!is.na(satl4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlfe<-rbind(da,db,dc,dd)
v<-cbind(satlfe,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1fe[!is.na(warm1fe)])
colnames(da)<-c("no")
db<-as.data.frame(warm2fe[!is.na(warm2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3fe[!is.na(warm3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4fe[!is.na(warm4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmfe<-rbind(da,db,dc,dd)
w<-cbind(warmfe,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1fe[!is.na(wtra1fe)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2fe[!is.na(wtra2fe)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3fe[!is.na(wtra3fe)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4fe[!is.na(wtra4fe)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtrafe<-rbind(da,db,dc,dd)
x<-cbind(wtrafe,dg)
colnames(x)<-c("SEA", "x")


FINALfe <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALfe <- cbind(FINALfe,col)
colnames(FINALfe)<-c("SEA", "x", "col")

ggplot(data = FINALfe, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Iron Concentrations per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','Sp','Su','A','A','Sp','Su','A',
                                           'Sp','Su','A','Su','Sp','Su','Sp','W','Sp',
                                           'Su','W','Su','Su','A','Su','A','Sp','Su','A',
                                           'W','Sp','Su','A','Sp','Su','A','W','Su','A',
                                           'Sp','Su','A','W','Su','Su','A','Sp','Su',
                                           'Sp','Su','A','Sp','Su','W','Su','A')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name= expression(Fe (mu,M)), limits=c(0, 0.0005)) + theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))


# Mn ----------------------------------------------------------------------
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1mn[!is.na(arct1mn)])
colnames(da)<-c("no")
db<-as.data.frame(arct2mn[!is.na(arct2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3mn[!is.na(arct3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4mn[!is.na(arct4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctmn<-rbind(da,db,dc,dd)
a<-cbind(arctmn,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1mn[!is.na(bplr1mn)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2mn[!is.na(bplr2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3mn[!is.na(bplr3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4mn[!is.na(bplr4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrmn<-rbind(da,db,dc,dd)
b<-cbind(bplrmn,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1mn[!is.na(sarc1mn)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2mn[!is.na(sarc2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3mn[!is.na(sarc3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4mn[!is.na(sarc4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarcmn<-rbind(da,db,dc,dd)
c<-cbind(sarcmn,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1mn[!is.na(anta1mn)])
colnames(da)<-c("no")
db<-as.data.frame(anta2mn[!is.na(anta2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3mn[!is.na(anta3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4mn[!is.na(anta4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antamn<-rbind(da,db,dc,dd)
d<-cbind(antamn,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1mn[!is.na(aplr1mn)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2mn[!is.na(aplr2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3mn[!is.na(aplr3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4mn[!is.na(aplr4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrmn<-rbind(da,db,dc,dd)
e<-cbind(aplrmn,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1mn[!is.na(gest1mn)])
colnames(da)<-c("no")
db<-as.data.frame(gest2mn[!is.na(gest2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3mn[!is.na(gest3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4mn[!is.na(gest4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestmn<-rbind(da,db,dc,dd)
f<-cbind(gestmn,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1mn[!is.na(nadr1mn)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2mn[!is.na(nadr2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3mn[!is.na(nadr3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4mn[!is.na(nadr4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrmn<-rbind(da,db,dc,dd)
g<-cbind(nadrmn,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1mn[!is.na(nase1mn)])
colnames(da)<-c("no")
db<-as.data.frame(nase2mn[!is.na(nase2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3mn[!is.na(nase3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4mn[!is.na(nase4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
nasemn<-rbind(da,db,dc,dd)
h<-cbind(nasemn,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1mn[!is.na(nasw1mn)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2mn[!is.na(nasw2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3mn[!is.na(nasw3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4mn[!is.na(nasw4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswmn<-rbind(da,db,dc,dd)
i<-cbind(naswmn,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1mn[!is.na(nptw1mn)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2mn[!is.na(nptw2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3mn[!is.na(nptw3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4mn[!is.na(nptw4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwmn<-rbind(da,db,dc,dd)
j<-cbind(nptwmn,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1mn[!is.na(psae1mn)])
colnames(da)<-c("no")
db<-as.data.frame(psae2mn[!is.na(psae2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3mn[!is.na(psae3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4mn[!is.na(psae4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaemn<-rbind(da,db,dc,dd)
k<-cbind(psaemn,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1mn[!is.na(sant1mn)])
colnames(da)<-c("no")
db<-as.data.frame(sant2mn[!is.na(sant2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3mn[!is.na(sant3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4mn[!is.na(sant4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santmn<-rbind(da,db,dc,dd)
l<-cbind(santmn,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1mn[!is.na(spsg1mn)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2mn[!is.na(spsg2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3mn[!is.na(spsg3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4mn[!is.na(spsg4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgmn<-rbind(da,db,dc,dd)
m<-cbind(spsgmn,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1mn[!is.na(sstc1mn)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2mn[!is.na(sstc2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3mn[!is.na(sstc3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4mn[!is.na(sstc4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstcmn<-rbind(da,db,dc,dd)
n<-cbind(sstcmn,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1mn[!is.na(arch1mn)])
colnames(da)<-c("no")
db<-as.data.frame(arch2mn[!is.na(arch2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3mn[!is.na(arch3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4mn[!is.na(arch4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archmn<-rbind(da,db,dc,dd)
o<-cbind(archmn,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1mn[!is.na(issg1mn)])
colnames(da)<-c("no")
db<-as.data.frame(issg2mn[!is.na(issg2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3mn[!is.na(issg3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4mn[!is.na(issg4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgmn<-rbind(da,db,dc,dd)
p<-cbind(issgmn,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1mn[!is.na(mons1mn)])
colnames(da)<-c("no")
db<-as.data.frame(mons2mn[!is.na(mons2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3mn[!is.na(mons3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4mn[!is.na(mons4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monsmn<-rbind(da,db,dc,dd)
q<-cbind(monsmn,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1mn[!is.na(natr1mn)])
colnames(da)<-c("no")
db<-as.data.frame(natr2mn[!is.na(natr2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3mn[!is.na(natr3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4mn[!is.na(natr4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrmn<-rbind(da,db,dc,dd)
r<-cbind(natrmn,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1mn[!is.na(npte1mn)])
colnames(da)<-c("no")
db<-as.data.frame(npte2mn[!is.na(npte2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3mn[!is.na(npte3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4mn[!is.na(npte4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
nptemn<-rbind(da,db,dc,dd)
s<-cbind(nptemn,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1mn[!is.na(peqd1mn)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2mn[!is.na(peqd2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3mn[!is.na(peqd3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4mn[!is.na(peqd4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdmn<-rbind(da,db,dc,dd)
t<-cbind(peqdmn,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1mn[!is.na(pnec1mn)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2mn[!is.na(pnec2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3mn[!is.na(pnec3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4mn[!is.na(pnec4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pnecmn<-rbind(da,db,dc,dd)
u<-cbind(pnecmn,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1mn[!is.na(satl1mn)])
colnames(da)<-c("no")
db<-as.data.frame(satl2mn[!is.na(satl2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3mn[!is.na(satl3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4mn[!is.na(satl4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlmn<-rbind(da,db,dc,dd)
v<-cbind(satlmn,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1mn[!is.na(warm1mn)])
colnames(da)<-c("no")
db<-as.data.frame(warm2mn[!is.na(warm2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3mn[!is.na(warm3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4mn[!is.na(warm4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmmn<-rbind(da,db,dc,dd)
w<-cbind(warmmn,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1mn[!is.na(wtra1mn)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2mn[!is.na(wtra2mn)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3mn[!is.na(wtra3mn)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4mn[!is.na(wtra4mn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtramn<-rbind(da,db,dc,dd)
x<-cbind(wtramn,dg)
colnames(x)<-c("SEA", "x")


FINALmn <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALmn <- cbind(FINALmn,col)
colnames(FINALmn)<-c("SEA", "x", "col")

ggplot(data = FINALmn, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Manganese Concentrations per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','Sp','Su','A','A','Sp','Su','A',
                                           'Sp','Su','A','Su','Sp','Su','Sp','Sp','Su',
                                           'Su','A','Su','A','Sp','Su','A','W','Su','A',
                                           'A','W','Su','Sp','Su','A','W','Su','Su','A',
                                           'Sp','Su','Sp','Su','A','Sp','Su','W','Sp','Su')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name= expression(Mn(mu,M)), limits=c(0, 0.006))+ theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                               panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                               panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))

# Ni ----------------------------------------------------------------------
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1ni[!is.na(arct1ni)])
colnames(da)<-c("no")
db<-as.data.frame(arct2ni[!is.na(arct2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3ni[!is.na(arct3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4ni[!is.na(arct4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctni<-rbind(da,db,dc,dd)
a<-cbind(arctni,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1ni[!is.na(bplr1ni)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2ni[!is.na(bplr2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3ni[!is.na(bplr3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4ni[!is.na(bplr4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrni<-rbind(da,db,dc,dd)
b<-cbind(bplrni,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1ni[!is.na(sarc1ni)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2ni[!is.na(sarc2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3ni[!is.na(sarc3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4ni[!is.na(sarc4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarcni<-rbind(da,db,dc,dd)
c<-cbind(sarcni,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1ni[!is.na(anta1ni)])
colnames(da)<-c("no")
db<-as.data.frame(anta2ni[!is.na(anta2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3ni[!is.na(anta3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4ni[!is.na(anta4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antani<-rbind(da,db,dc,dd)
d<-cbind(antani,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1ni[!is.na(aplr1ni)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2ni[!is.na(aplr2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3ni[!is.na(aplr3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4ni[!is.na(aplr4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrni<-rbind(da,db,dc,dd)
e<-cbind(aplrni,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1ni[!is.na(gest1ni)])
colnames(da)<-c("no")
db<-as.data.frame(gest2ni[!is.na(gest2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3ni[!is.na(gest3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4ni[!is.na(gest4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestni<-rbind(da,db,dc,dd)
f<-cbind(gestni,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1ni[!is.na(nadr1ni)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2ni[!is.na(nadr2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3ni[!is.na(nadr3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4ni[!is.na(nadr4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrni<-rbind(da,db,dc,dd)
g<-cbind(nadrni,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1ni[!is.na(nase1ni)])
colnames(da)<-c("no")
db<-as.data.frame(nase2ni[!is.na(nase2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3ni[!is.na(nase3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4ni[!is.na(nase4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
naseni<-rbind(da,db,dc,dd)
h<-cbind(naseni,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1ni[!is.na(nasw1ni)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2ni[!is.na(nasw2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3ni[!is.na(nasw3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4ni[!is.na(nasw4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswni<-rbind(da,db,dc,dd)
i<-cbind(naswni,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1ni[!is.na(nptw1ni)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2ni[!is.na(nptw2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3ni[!is.na(nptw3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4ni[!is.na(nptw4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwni<-rbind(da,db,dc,dd)
j<-cbind(nptwni,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1ni[!is.na(psae1ni)])
colnames(da)<-c("no")
db<-as.data.frame(psae2ni[!is.na(psae2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3ni[!is.na(psae3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4ni[!is.na(psae4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaeni<-rbind(da,db,dc,dd)
k<-cbind(psaeni,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1ni[!is.na(sant1ni)])
colnames(da)<-c("no")
db<-as.data.frame(sant2ni[!is.na(sant2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3ni[!is.na(sant3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4ni[!is.na(sant4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santni<-rbind(da,db,dc,dd)
l<-cbind(santni,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1ni[!is.na(spsg1ni)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2ni[!is.na(spsg2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3ni[!is.na(spsg3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4ni[!is.na(spsg4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgni<-rbind(da,db,dc,dd)
m<-cbind(spsgni,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1ni[!is.na(sstc1ni)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2ni[!is.na(sstc2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3ni[!is.na(sstc3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4ni[!is.na(sstc4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstcni<-rbind(da,db,dc,dd)
n<-cbind(sstcni,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1ni[!is.na(arch1ni)])
colnames(da)<-c("no")
db<-as.data.frame(arch2ni[!is.na(arch2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3ni[!is.na(arch3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4ni[!is.na(arch4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archni<-rbind(da,db,dc,dd)
o<-cbind(archni,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1ni[!is.na(issg1ni)])
colnames(da)<-c("no")
db<-as.data.frame(issg2ni[!is.na(issg2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3ni[!is.na(issg3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4ni[!is.na(issg4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgni<-rbind(da,db,dc,dd)
p<-cbind(issgni,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1ni[!is.na(mons1ni)])
colnames(da)<-c("no")
db<-as.data.frame(mons2ni[!is.na(mons2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3ni[!is.na(mons3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4ni[!is.na(mons4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monsni<-rbind(da,db,dc,dd)
q<-cbind(monsni,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1ni[!is.na(natr1ni)])
colnames(da)<-c("no")
db<-as.data.frame(natr2ni[!is.na(natr2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3ni[!is.na(natr3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4ni[!is.na(natr4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrni<-rbind(da,db,dc,dd)
r<-cbind(natrni,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1ni[!is.na(npte1ni)])
colnames(da)<-c("no")
db<-as.data.frame(npte2ni[!is.na(npte2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3ni[!is.na(npte3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4ni[!is.na(npte4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
npteni<-rbind(da,db,dc,dd)
s<-cbind(npteni,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1ni[!is.na(peqd1ni)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2ni[!is.na(peqd2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3ni[!is.na(peqd3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4ni[!is.na(peqd4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdni<-rbind(da,db,dc,dd)
t<-cbind(peqdni,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1ni[!is.na(pnec1ni)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2ni[!is.na(pnec2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3ni[!is.na(pnec3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4ni[!is.na(pnec4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pnecni<-rbind(da,db,dc,dd)
u<-cbind(pnecni,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1ni[!is.na(satl1ni)])
colnames(da)<-c("no")
db<-as.data.frame(satl2ni[!is.na(satl2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3ni[!is.na(satl3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4ni[!is.na(satl4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlni<-rbind(da,db,dc,dd)
v<-cbind(satlni,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1ni[!is.na(warm1ni)])
colnames(da)<-c("no")
db<-as.data.frame(warm2ni[!is.na(warm2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3ni[!is.na(warm3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4ni[!is.na(warm4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmni<-rbind(da,db,dc,dd)
w<-cbind(warmni,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1ni[!is.na(wtra1ni)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2ni[!is.na(wtra2ni)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3ni[!is.na(wtra3ni)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4ni[!is.na(wtra4ni)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtrani<-rbind(da,db,dc,dd)
x<-cbind(wtrani,dg)
colnames(x)<-c("SEA", "x")


FINALni <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALni <- cbind(FINALni,col)
colnames(FINALni)<-c("SEA", "x", "col")

ggplot(data = FINALni, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Nickel concentrations per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','Sp','Su','A','A','Sp','Su','A',
                                           'Sp','Su','A','Su','Sp','Su','Sp','Sp','Su',
                                           'Su','A','Su','A','Sp','Su','A','W','Su','A',
                                           'A','Su','Sp','Su','A','W','Su','Su','A',
                                           'Sp','Su','Sp','Su','A','Sp','Su','W','Sp','Su')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name= expression(Ni(mu,M)), limits=c(0.0015, 0.008))+ theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                                 panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))


# Zn ----------------------------------------------------------------------
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1zn[!is.na(arct1zn)])
colnames(da)<-c("no")
db<-as.data.frame(arct2zn[!is.na(arct2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3zn[!is.na(arct3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4zn[!is.na(arct4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctzn<-rbind(da,db,dc,dd)
a<-cbind(arctzn,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1zn[!is.na(bplr1zn)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2zn[!is.na(bplr2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3zn[!is.na(bplr3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4zn[!is.na(bplr4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrzn<-rbind(da,db,dc,dd)
b<-cbind(bplrzn,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1zn[!is.na(sarc1zn)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2zn[!is.na(sarc2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3zn[!is.na(sarc3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4zn[!is.na(sarc4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarczn<-rbind(da,db,dc,dd)
c<-cbind(sarczn,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1zn[!is.na(anta1zn)])
colnames(da)<-c("no")
db<-as.data.frame(anta2zn[!is.na(anta2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3zn[!is.na(anta3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4zn[!is.na(anta4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antazn<-rbind(da,db,dc,dd)
d<-cbind(antazn,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1zn[!is.na(aplr1zn)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2zn[!is.na(aplr2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3zn[!is.na(aplr3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4zn[!is.na(aplr4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrzn<-rbind(da,db,dc,dd)
e<-cbind(aplrzn,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1zn[!is.na(gest1zn)])
colnames(da)<-c("no")
db<-as.data.frame(gest2zn[!is.na(gest2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3zn[!is.na(gest3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4zn[!is.na(gest4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestzn<-rbind(da,db,dc,dd)
f<-cbind(gestzn,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1zn[!is.na(nadr1zn)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2zn[!is.na(nadr2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3zn[!is.na(nadr3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4zn[!is.na(nadr4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrzn<-rbind(da,db,dc,dd)
g<-cbind(nadrzn,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1zn[!is.na(nase1zn)])
colnames(da)<-c("no")
db<-as.data.frame(nase2zn[!is.na(nase2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3zn[!is.na(nase3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4zn[!is.na(nase4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
nasezn<-rbind(da,db,dc,dd)
h<-cbind(nasezn,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1zn[!is.na(nasw1zn)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2zn[!is.na(nasw2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3zn[!is.na(nasw3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4zn[!is.na(nasw4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswzn<-rbind(da,db,dc,dd)
i<-cbind(naswzn,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1zn[!is.na(nptw1zn)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2zn[!is.na(nptw2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3zn[!is.na(nptw3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4zn[!is.na(nptw4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwzn<-rbind(da,db,dc,dd)
j<-cbind(nptwzn,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1zn[!is.na(psae1zn)])
colnames(da)<-c("no")
db<-as.data.frame(psae2zn[!is.na(psae2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3zn[!is.na(psae3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4zn[!is.na(psae4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaezn<-rbind(da,db,dc,dd)
k<-cbind(psaezn,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1zn[!is.na(sant1zn)])
colnames(da)<-c("no")
db<-as.data.frame(sant2zn[!is.na(sant2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3zn[!is.na(sant3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4zn[!is.na(sant4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santzn<-rbind(da,db,dc,dd)
l<-cbind(santzn,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1zn[!is.na(spsg1zn)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2zn[!is.na(spsg2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3zn[!is.na(spsg3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4zn[!is.na(spsg4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgzn<-rbind(da,db,dc,dd)
m<-cbind(spsgzn,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1zn[!is.na(sstc1zn)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2zn[!is.na(sstc2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3zn[!is.na(sstc3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4zn[!is.na(sstc4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstczn<-rbind(da,db,dc,dd)
n<-cbind(sstczn,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1zn[!is.na(arch1zn)])
colnames(da)<-c("no")
db<-as.data.frame(arch2zn[!is.na(arch2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3zn[!is.na(arch3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4zn[!is.na(arch4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archzn<-rbind(da,db,dc,dd)
o<-cbind(archzn,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1zn[!is.na(issg1zn)])
colnames(da)<-c("no")
db<-as.data.frame(issg2zn[!is.na(issg2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3zn[!is.na(issg3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4zn[!is.na(issg4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgzn<-rbind(da,db,dc,dd)
p<-cbind(issgzn,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1zn[!is.na(mons1zn)])
colnames(da)<-c("no")
db<-as.data.frame(mons2zn[!is.na(mons2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3zn[!is.na(mons3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4zn[!is.na(mons4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monszn<-rbind(da,db,dc,dd)
q<-cbind(monszn,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1zn[!is.na(natr1zn)])
colnames(da)<-c("no")
db<-as.data.frame(natr2zn[!is.na(natr2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3zn[!is.na(natr3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4zn[!is.na(natr4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrzn<-rbind(da,db,dc,dd)
r<-cbind(natrzn,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1zn[!is.na(npte1zn)])
colnames(da)<-c("no")
db<-as.data.frame(npte2zn[!is.na(npte2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3zn[!is.na(npte3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4zn[!is.na(npte4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
nptezn<-rbind(da,db,dc,dd)
s<-cbind(nptezn,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1zn[!is.na(peqd1zn)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2zn[!is.na(peqd2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3zn[!is.na(peqd3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4zn[!is.na(peqd4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdzn<-rbind(da,db,dc,dd)
t<-cbind(peqdzn,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1zn[!is.na(pnec1zn)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2zn[!is.na(pnec2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3zn[!is.na(pnec3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4zn[!is.na(pnec4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pneczn<-rbind(da,db,dc,dd)
u<-cbind(pneczn,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1zn[!is.na(satl1zn)])
colnames(da)<-c("no")
db<-as.data.frame(satl2zn[!is.na(satl2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3zn[!is.na(satl3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4zn[!is.na(satl4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlzn<-rbind(da,db,dc,dd)
v<-cbind(satlzn,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1zn[!is.na(warm1zn)])
colnames(da)<-c("no")
db<-as.data.frame(warm2zn[!is.na(warm2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3zn[!is.na(warm3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4zn[!is.na(warm4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmzn<-rbind(da,db,dc,dd)
w<-cbind(warmzn,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1zn[!is.na(wtra1zn)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2zn[!is.na(wtra2zn)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3zn[!is.na(wtra3zn)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4zn[!is.na(wtra4zn)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtrazn<-rbind(da,db,dc,dd)
x<-cbind(wtrazn,dg)
colnames(x)<-c("SEA", "x")


FINALzn <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALzn <- cbind(FINALzn,col)
colnames(FINALzn)<-c("SEA", "x", "col")

ggplot(data = FINALzn, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Zinc concentrations per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','Sp','Su','A','A','Sp','Su','A',
                                           'Sp','Su','A','Su','Sp','Su','Sp','Sp','Su',
                                           'Su','A','Sp','Su','A','Sp','Su','A','W','Sp',
                                           'Su','A','Su','A','Su','Sp','Su','A','W','Su','A',
                                           'Sp','Sp','Su','A','Sp','Su','W','Su','A')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name= expression(Zn(mu,M)), limits=c(0.000, 0.006))+ theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                                 panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                                 panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))


# Cu ----------------------------------------------------------------------
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1cu[!is.na(arct1cu)])
colnames(da)<-c("no")
db<-as.data.frame(arct2cu[!is.na(arct2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3cu[!is.na(arct3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4cu[!is.na(arct4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctcu<-rbind(da,db,dc,dd)
a<-cbind(arctcu,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1cu[!is.na(bplr1cu)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2cu[!is.na(bplr2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3cu[!is.na(bplr3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4cu[!is.na(bplr4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrcu<-rbind(da,db,dc,dd)
b<-cbind(bplrcu,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1cu[!is.na(sarc1cu)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2cu[!is.na(sarc2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3cu[!is.na(sarc3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4cu[!is.na(sarc4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarccu<-rbind(da,db,dc,dd)
c<-cbind(sarccu,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1cu[!is.na(anta1cu)])
colnames(da)<-c("no")
db<-as.data.frame(anta2cu[!is.na(anta2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3cu[!is.na(anta3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4cu[!is.na(anta4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antacu<-rbind(da,db,dc,dd)
d<-cbind(antacu,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1cu[!is.na(aplr1cu)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2cu[!is.na(aplr2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3cu[!is.na(aplr3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4cu[!is.na(aplr4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrcu<-rbind(da,db,dc,dd)
e<-cbind(aplrcu,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1cu[!is.na(gest1cu)])
colnames(da)<-c("no")
db<-as.data.frame(gest2cu[!is.na(gest2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3cu[!is.na(gest3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4cu[!is.na(gest4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestcu<-rbind(da,db,dc,dd)
f<-cbind(gestcu,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1cu[!is.na(nadr1cu)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2cu[!is.na(nadr2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3cu[!is.na(nadr3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4cu[!is.na(nadr4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrcu<-rbind(da,db,dc,dd)
g<-cbind(nadrcu,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1cu[!is.na(nase1cu)])
colnames(da)<-c("no")
db<-as.data.frame(nase2cu[!is.na(nase2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3cu[!is.na(nase3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4cu[!is.na(nase4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
nasecu<-rbind(da,db,dc,dd)
h<-cbind(nasecu,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1cu[!is.na(nasw1cu)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2cu[!is.na(nasw2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3cu[!is.na(nasw3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4cu[!is.na(nasw4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswcu<-rbind(da,db,dc,dd)
i<-cbind(naswcu,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1cu[!is.na(nptw1cu)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2cu[!is.na(nptw2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3cu[!is.na(nptw3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4cu[!is.na(nptw4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwcu<-rbind(da,db,dc,dd)
j<-cbind(nptwcu,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1cu[!is.na(psae1cu)])
colnames(da)<-c("no")
db<-as.data.frame(psae2cu[!is.na(psae2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3cu[!is.na(psae3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4cu[!is.na(psae4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaecu<-rbind(da,db,dc,dd)
k<-cbind(psaecu,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1cu[!is.na(sant1cu)])
colnames(da)<-c("no")
db<-as.data.frame(sant2cu[!is.na(sant2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3cu[!is.na(sant3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4cu[!is.na(sant4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santcu<-rbind(da,db,dc,dd)
l<-cbind(santcu,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1cu[!is.na(spsg1cu)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2cu[!is.na(spsg2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3cu[!is.na(spsg3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4cu[!is.na(spsg4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgcu<-rbind(da,db,dc,dd)
m<-cbind(spsgcu,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1cu[!is.na(sstc1cu)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2cu[!is.na(sstc2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3cu[!is.na(sstc3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4cu[!is.na(sstc4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstccu<-rbind(da,db,dc,dd)
n<-cbind(sstccu,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1cu[!is.na(arch1cu)])
colnames(da)<-c("no")
db<-as.data.frame(arch2cu[!is.na(arch2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3cu[!is.na(arch3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4cu[!is.na(arch4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archcu<-rbind(da,db,dc,dd)
o<-cbind(archcu,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1cu[!is.na(issg1cu)])
colnames(da)<-c("no")
db<-as.data.frame(issg2cu[!is.na(issg2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3cu[!is.na(issg3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4cu[!is.na(issg4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgcu<-rbind(da,db,dc,dd)
p<-cbind(issgcu,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1cu[!is.na(mons1cu)])
colnames(da)<-c("no")
db<-as.data.frame(mons2cu[!is.na(mons2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3cu[!is.na(mons3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4cu[!is.na(mons4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monscu<-rbind(da,db,dc,dd)
q<-cbind(monscu,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1cu[!is.na(natr1cu)])
colnames(da)<-c("no")
db<-as.data.frame(natr2cu[!is.na(natr2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3cu[!is.na(natr3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4cu[!is.na(natr4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrcu<-rbind(da,db,dc,dd)
r<-cbind(natrcu,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1cu[!is.na(npte1cu)])
colnames(da)<-c("no")
db<-as.data.frame(npte2cu[!is.na(npte2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3cu[!is.na(npte3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4cu[!is.na(npte4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
nptecu<-rbind(da,db,dc,dd)
s<-cbind(nptecu,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1cu[!is.na(peqd1cu)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2cu[!is.na(peqd2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3cu[!is.na(peqd3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4cu[!is.na(peqd4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdcu<-rbind(da,db,dc,dd)
t<-cbind(peqdcu,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1cu[!is.na(pnec1cu)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2cu[!is.na(pnec2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3cu[!is.na(pnec3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4cu[!is.na(pnec4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pneccu<-rbind(da,db,dc,dd)
u<-cbind(pneccu,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1cu[!is.na(satl1cu)])
colnames(da)<-c("no")
db<-as.data.frame(satl2cu[!is.na(satl2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3cu[!is.na(satl3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4cu[!is.na(satl4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlcu<-rbind(da,db,dc,dd)
v<-cbind(satlcu,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1cu[!is.na(warm1cu)])
colnames(da)<-c("no")
db<-as.data.frame(warm2cu[!is.na(warm2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3cu[!is.na(warm3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4cu[!is.na(warm4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmcu<-rbind(da,db,dc,dd)
w<-cbind(warmcu,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1cu[!is.na(wtra1cu)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2cu[!is.na(wtra2cu)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3cu[!is.na(wtra3cu)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4cu[!is.na(wtra4cu)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtracu<-rbind(da,db,dc,dd)
x<-cbind(wtracu,dg)
colnames(x)<-c("SEA", "x")


FINALcu <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALcu <- cbind(FINALcu,col)
colnames(FINALcu)<-c("SEA", "x", "col")

ggplot(data = FINALcu, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Copper concentrations per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Su','Su','A','A','Sp','Su','A',
                                           'Sp','Su','A','W','Su','Su','A','Su','A',
                                           'Sp','Su','A','W','Su','A','Su','A',
                                           'Su','Sp','Su','A','W','Su','A',
                                           'Sp','Su','Su','Su','W')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C")) +
  scale_y_continuous(name= expression(Cu(mu,M)), limits=c(0.000, 0.006))+ theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                                panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))

# Cd ----------------------------------------------------------------------
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1cd[!is.na(arct1cd)])
colnames(da)<-c("no")
db<-as.data.frame(arct2cd[!is.na(arct2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3cd[!is.na(arct3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4cd[!is.na(arct4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctcd<-rbind(da,db,dc,dd)
a<-cbind(arctcd,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1cd[!is.na(bplr1cd)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2cd[!is.na(bplr2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3cd[!is.na(bplr3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4cd[!is.na(bplr4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrcd<-rbind(da,db,dc,dd)
b<-cbind(bplrcd,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1cd[!is.na(sarc1cd)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2cd[!is.na(sarc2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3cd[!is.na(sarc3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4cd[!is.na(sarc4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarccd<-rbind(da,db,dc,dd)
c<-cbind(sarccd,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1cd[!is.na(anta1cd)])
colnames(da)<-c("no")
db<-as.data.frame(anta2cd[!is.na(anta2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3cd[!is.na(anta3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4cd[!is.na(anta4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antacd<-rbind(da,db,dc,dd)
d<-cbind(antacd,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1cd[!is.na(aplr1cd)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2cd[!is.na(aplr2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3cd[!is.na(aplr3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4cd[!is.na(aplr4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrcd<-rbind(da,db,dc,dd)
e<-cbind(aplrcd,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1cd[!is.na(gest1cd)])
colnames(da)<-c("no")
db<-as.data.frame(gest2cd[!is.na(gest2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3cd[!is.na(gest3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4cd[!is.na(gest4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestcd<-rbind(da,db,dc,dd)
f<-cbind(gestcd,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1cd[!is.na(nadr1cd)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2cd[!is.na(nadr2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3cd[!is.na(nadr3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4cd[!is.na(nadr4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrcd<-rbind(da,db,dc,dd)
g<-cbind(nadrcd,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1cd[!is.na(nase1cd)])
colnames(da)<-c("no")
db<-as.data.frame(nase2cd[!is.na(nase2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3cd[!is.na(nase3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4cd[!is.na(nase4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
nasecd<-rbind(da,db,dc,dd)
h<-cbind(nasecd,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1cd[!is.na(nasw1cd)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2cd[!is.na(nasw2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3cd[!is.na(nasw3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4cd[!is.na(nasw4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswcd<-rbind(da,db,dc,dd)
i<-cbind(naswcd,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1cd[!is.na(nptw1cd)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2cd[!is.na(nptw2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3cd[!is.na(nptw3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4cd[!is.na(nptw4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwcd<-rbind(da,db,dc,dd)
j<-cbind(nptwcd,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1cd[!is.na(psae1cd)])
colnames(da)<-c("no")
db<-as.data.frame(psae2cd[!is.na(psae2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3cd[!is.na(psae3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4cd[!is.na(psae4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaecd<-rbind(da,db,dc,dd)
k<-cbind(psaecd,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1cd[!is.na(sant1cd)])
colnames(da)<-c("no")
db<-as.data.frame(sant2cd[!is.na(sant2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3cd[!is.na(sant3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4cd[!is.na(sant4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santcd<-rbind(da,db,dc,dd)
l<-cbind(santcd,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1cd[!is.na(spsg1cd)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2cd[!is.na(spsg2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3cd[!is.na(spsg3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4cd[!is.na(spsg4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgcd<-rbind(da,db,dc,dd)
m<-cbind(spsgcd,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1cd[!is.na(sstc1cd)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2cd[!is.na(sstc2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3cd[!is.na(sstc3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4cd[!is.na(sstc4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstccd<-rbind(da,db,dc,dd)
n<-cbind(sstccd,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1cd[!is.na(arch1cd)])
colnames(da)<-c("no")
db<-as.data.frame(arch2cd[!is.na(arch2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3cd[!is.na(arch3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4cd[!is.na(arch4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archcd<-rbind(da,db,dc,dd)
o<-cbind(archcd,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1cd[!is.na(issg1cd)])
colnames(da)<-c("no")
db<-as.data.frame(issg2cd[!is.na(issg2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3cd[!is.na(issg3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4cd[!is.na(issg4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgcd<-rbind(da,db,dc,dd)
p<-cbind(issgcd,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1cd[!is.na(mons1cd)])
colnames(da)<-c("no")
db<-as.data.frame(mons2cd[!is.na(mons2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3cd[!is.na(mons3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4cd[!is.na(mons4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monscd<-rbind(da,db,dc,dd)
q<-cbind(monscd,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1cd[!is.na(natr1cd)])
colnames(da)<-c("no")
db<-as.data.frame(natr2cd[!is.na(natr2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3cd[!is.na(natr3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4cd[!is.na(natr4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrcd<-rbind(da,db,dc,dd)
r<-cbind(natrcd,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1cd[!is.na(npte1cd)])
colnames(da)<-c("no")
db<-as.data.frame(npte2cd[!is.na(npte2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3cd[!is.na(npte3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4cd[!is.na(npte4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
nptecd<-rbind(da,db,dc,dd)
s<-cbind(nptecd,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1cd[!is.na(peqd1cd)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2cd[!is.na(peqd2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3cd[!is.na(peqd3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4cd[!is.na(peqd4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdcd<-rbind(da,db,dc,dd)
t<-cbind(peqdcd,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1cd[!is.na(pnec1cd)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2cd[!is.na(pnec2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3cd[!is.na(pnec3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4cd[!is.na(pnec4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pneccd<-rbind(da,db,dc,dd)
u<-cbind(pneccd,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1cd[!is.na(satl1cd)])
colnames(da)<-c("no")
db<-as.data.frame(satl2cd[!is.na(satl2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3cd[!is.na(satl3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4cd[!is.na(satl4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlcd<-rbind(da,db,dc,dd)
v<-cbind(satlcd,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1cd[!is.na(warm1cd)])
colnames(da)<-c("no")
db<-as.data.frame(warm2cd[!is.na(warm2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3cd[!is.na(warm3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4cd[!is.na(warm4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmcd<-rbind(da,db,dc,dd)
w<-cbind(warmcd,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1cd[!is.na(wtra1cd)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2cd[!is.na(wtra2cd)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3cd[!is.na(wtra3cd)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4cd[!is.na(wtra4cd)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtracd<-rbind(da,db,dc,dd)
x<-cbind(wtracd,dg)
colnames(x)<-c("SEA", "x")


FINALcd <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALcd <- cbind(FINALcd,col)
colnames(FINALcd)<-c("SEA", "x", "col")

ggplot(data = FINALcd, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Cadmium concentrations per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','Sp','Su','A','A','Sp','Su','A',
                                           'Sp','Su','A','Su','Sp','Su','Sp','W',
                                           'Sp','Su','Su','Su','A','Su','A','Sp','Su','A','W',
                                           'Su','A','Su','A','Su','Sp','Su','A','W','Su',
                                           'A','Sp','Su','A','Sp','W','Su','A')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name= expression(Cd(mu,M)), limits=c(0.000, 0.00075))+ theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                                panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                                panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))


# Co ----------------------------------------------------------------------
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1co[!is.na(arct1co)])
colnames(da)<-c("no")
db<-as.data.frame(arct2co[!is.na(arct2co)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3co[!is.na(arct3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4co[!is.na(arct4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctco<-rbind(da,db,dc,dd)
a<-cbind(arctco,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1co[!is.na(bplr1co)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2co[!is.na(bplr2co)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3co[!is.na(bplr3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4co[!is.na(bplr4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrco<-rbind(da,db,dc,dd)
b<-cbind(bplrco,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1co[!is.na(sarc1co)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2co[!is.na(sarc2co)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3co[!is.na(sarc3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4co[!is.na(sarc4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarcco<-rbind(da,db,dc,dd)
c<-cbind(sarcco,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1co[!is.na(anta1co)])
colnames(da)<-c("no")
db<-as.data.frame(anta2co[!is.na(anta2co)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3co[!is.na(anta3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4co[!is.na(anta4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antaco<-rbind(da,db,dc,dd)
d<-cbind(antaco,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1co[!is.na(aplr1co)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2co[!is.na(aplr2co)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3co[!is.na(aplr3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4co[!is.na(aplr4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrco<-rbind(da,db,dc,dd)
e<-cbind(aplrco,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1co[!is.na(gest1co)])
colnames(da)<-c("no")
db<-as.data.frame(gest2co[!is.na(gest2co)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3co[!is.na(gest3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4co[!is.na(gest4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestco<-rbind(da,db,dc,dd)
f<-cbind(gestco,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1co[!is.na(nadr1co)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2co[!is.na(nadr2co)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3co[!is.na(nadr3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4co[!is.na(nadr4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrco<-rbind(da,db,dc,dd)
g<-cbind(nadrco,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1co[!is.na(nase1co)])
colnames(da)<-c("no")
db<-as.data.frame(nase2co[!is.na(nase2co)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3co[!is.na(nase3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4co[!is.na(nase4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
naseco<-rbind(da,db,dc,dd)
h<-cbind(naseco,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1co[!is.na(nasw1co)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2co[!is.na(nasw2co)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3co[!is.na(nasw3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4co[!is.na(nasw4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswco<-rbind(da,db,dc,dd)
i<-cbind(naswco,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1co[!is.na(nptw1co)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2co[!is.na(nptw2co)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3co[!is.na(nptw3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4co[!is.na(nptw4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwco<-rbind(da,db,dc,dd)
j<-cbind(nptwco,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1co[!is.na(psae1co)])
colnames(da)<-c("no")
db<-as.data.frame(psae2co[!is.na(psae2co)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3co[!is.na(psae3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4co[!is.na(psae4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaeco<-rbind(da,db,dc,dd)
k<-cbind(psaeco,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1co[!is.na(sant1co)])
colnames(da)<-c("no")
db<-as.data.frame(sant2co[!is.na(sant2co)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3co[!is.na(sant3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4co[!is.na(sant4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santco<-rbind(da,db,dc,dd)
l<-cbind(santco,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1co[!is.na(spsg1co)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2co[!is.na(spsg2co)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3co[!is.na(spsg3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4co[!is.na(spsg4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgco<-rbind(da,db,dc,dd)
m<-cbind(spsgco,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1co[!is.na(sstc1co)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2co[!is.na(sstc2co)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3co[!is.na(sstc3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4co[!is.na(sstc4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstcco<-rbind(da,db,dc,dd)
n<-cbind(sstcco,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1co[!is.na(arch1co)])
colnames(da)<-c("no")
db<-as.data.frame(arch2co[!is.na(arch2co)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3co[!is.na(arch3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4co[!is.na(arch4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archco<-rbind(da,db,dc,dd)
o<-cbind(archco,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1co[!is.na(issg1co)])
colnames(da)<-c("no")
db<-as.data.frame(issg2co[!is.na(issg2co)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3co[!is.na(issg3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4co[!is.na(issg4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgco<-rbind(da,db,dc,dd)
p<-cbind(issgco,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1co[!is.na(mons1co)])
colnames(da)<-c("no")
db<-as.data.frame(mons2co[!is.na(mons2co)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3co[!is.na(mons3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4co[!is.na(mons4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monsco<-rbind(da,db,dc,dd)
q<-cbind(monsco,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1co[!is.na(natr1co)])
colnames(da)<-c("no")
db<-as.data.frame(natr2co[!is.na(natr2co)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3co[!is.na(natr3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4co[!is.na(natr4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrco<-rbind(da,db,dc,dd)
r<-cbind(natrco,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1co[!is.na(npte1co)])
colnames(da)<-c("no")
db<-as.data.frame(npte2co[!is.na(npte2co)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3co[!is.na(npte3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4co[!is.na(npte4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
npteco<-rbind(da,db,dc,dd)
s<-cbind(npteco,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1co[!is.na(peqd1co)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2co[!is.na(peqd2co)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3co[!is.na(peqd3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4co[!is.na(peqd4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdco<-rbind(da,db,dc,dd)
t<-cbind(peqdco,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1co[!is.na(pnec1co)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2co[!is.na(pnec2co)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3co[!is.na(pnec3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4co[!is.na(pnec4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pnecco<-rbind(da,db,dc,dd)
u<-cbind(pnecco,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1co[!is.na(satl1co)])
colnames(da)<-c("no")
db<-as.data.frame(satl2co[!is.na(satl2co)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3co[!is.na(satl3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4co[!is.na(satl4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlco<-rbind(da,db,dc,dd)
v<-cbind(satlco,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1co[!is.na(warm1co)])
colnames(da)<-c("no")
db<-as.data.frame(warm2co[!is.na(warm2co)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3co[!is.na(warm3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4co[!is.na(warm4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmco<-rbind(da,db,dc,dd)
w<-cbind(warmco,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1co[!is.na(wtra1co)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2co[!is.na(wtra2co)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3co[!is.na(wtra3co)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4co[!is.na(wtra4co)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtraco<-rbind(da,db,dc,dd)
x<-cbind(wtraco,dg)
colnames(x)<-c("SEA", "x")


FINALco <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALco <- cbind(FINALco,col)
colnames(FINALco)<-c("SEA", "x", "col")

ggplot(data = FINALco, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Cobalt concentrations per Province") +
  geom_boxplot(alpha = 0.1) +
  scale_x_discrete(name="Season", labels=c('Su','Sp','Su','A','A','Sp','Sp','Su','Su',
                                           'Sp','Sp','Su','A','A','Sp','Sp','Su','A',
                                           'Sp','Su','A','Sp','A','Sp','Su','A','W',
                                           'Su','A','Su','A','Su','Sp','Su','A','Sp',
                                           'Su','A')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#006600","#99ff99","#12A125","#589056","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#006600","#99ff99","#12A125","#589056","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name= expression(Co(mu,M)), limits=c(0.000, 0.00025))+ theme(legend.position = "none",panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                                  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                                  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))




# TEMP --------------------------------------------------------------------

setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1temp[!is.na(arct1temp)])
colnames(da)<-c("no")
db<-as.data.frame(arct2temp[!is.na(arct2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3temp[!is.na(arct3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4temp[!is.na(arct4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arcttemp<-rbind(da,db,dc,dd)
a<-cbind(arcttemp,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1temp[!is.na(bplr1temp)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2temp[!is.na(bplr2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3temp[!is.na(bplr3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4temp[!is.na(bplr4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrtemp<-rbind(da,db,dc,dd)
b<-cbind(bplrtemp,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1temp[!is.na(sarc1temp)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2temp[!is.na(sarc2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3temp[!is.na(sarc3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4temp[!is.na(sarc4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarctemp<-rbind(da,db,dc,dd)
c<-cbind(sarctemp,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1temp[!is.na(anta1temp)])
colnames(da)<-c("no")
db<-as.data.frame(anta2temp[!is.na(anta2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3temp[!is.na(anta3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4temp[!is.na(anta4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antatemp<-rbind(da,db,dc,dd)
d<-cbind(antatemp,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1temp[!is.na(aplr1temp)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2temp[!is.na(aplr2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3temp[!is.na(aplr3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4temp[!is.na(aplr4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrtemp<-rbind(da,db,dc,dd)
e<-cbind(aplrtemp,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1temp[!is.na(gest1temp)])
colnames(da)<-c("no")
db<-as.data.frame(gest2temp[!is.na(gest2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3temp[!is.na(gest3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4temp[!is.na(gest4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gesttemp<-rbind(da,db,dc,dd)
f<-cbind(gesttemp,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1temp[!is.na(nadr1temp)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2temp[!is.na(nadr2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3temp[!is.na(nadr3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4temp[!is.na(nadr4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrtemp<-rbind(da,db,dc,dd)
g<-cbind(nadrtemp,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1temp[!is.na(nase1temp)])
colnames(da)<-c("no")
db<-as.data.frame(nase2temp[!is.na(nase2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3temp[!is.na(nase3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4temp[!is.na(nase4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
nasetemp<-rbind(da,db,dc,dd)
h<-cbind(nasetemp,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1temp[!is.na(nasw1temp)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2temp[!is.na(nasw2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3temp[!is.na(nasw3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4temp[!is.na(nasw4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswtemp<-rbind(da,db,dc,dd)
i<-cbind(naswtemp,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1temp[!is.na(nptw1temp)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2temp[!is.na(nptw2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3temp[!is.na(nptw3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4temp[!is.na(nptw4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwtemp<-rbind(da,db,dc,dd)
j<-cbind(nptwtemp,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1temp[!is.na(psae1temp)])
colnames(da)<-c("no")
db<-as.data.frame(psae2temp[!is.na(psae2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3temp[!is.na(psae3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4temp[!is.na(psae4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaetemp<-rbind(da,db,dc,dd)
k<-cbind(psaetemp,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1temp[!is.na(sant1temp)])
colnames(da)<-c("no")
db<-as.data.frame(sant2temp[!is.na(sant2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3temp[!is.na(sant3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4temp[!is.na(sant4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santtemp<-rbind(da,db,dc,dd)
l<-cbind(santtemp,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1temp[!is.na(spsg1temp)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2temp[!is.na(spsg2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3temp[!is.na(spsg3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4temp[!is.na(spsg4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgtemp<-rbind(da,db,dc,dd)
m<-cbind(spsgtemp,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1temp[!is.na(sstc1temp)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2temp[!is.na(sstc2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3temp[!is.na(sstc3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4temp[!is.na(sstc4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstctemp<-rbind(da,db,dc,dd)
n<-cbind(sstctemp,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1temp[!is.na(arch1temp)])
colnames(da)<-c("no")
db<-as.data.frame(arch2temp[!is.na(arch2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3temp[!is.na(arch3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4temp[!is.na(arch4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archtemp<-rbind(da,db,dc,dd)
o<-cbind(archtemp,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1temp[!is.na(issg1temp)])
colnames(da)<-c("no")
db<-as.data.frame(issg2temp[!is.na(issg2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3temp[!is.na(issg3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4temp[!is.na(issg4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgtemp<-rbind(da,db,dc,dd)
p<-cbind(issgtemp,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1temp[!is.na(mons1temp)])
colnames(da)<-c("no")
db<-as.data.frame(mons2temp[!is.na(mons2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3temp[!is.na(mons3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4temp[!is.na(mons4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monstemp<-rbind(da,db,dc,dd)
q<-cbind(monstemp,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1temp[!is.na(natr1temp)])
colnames(da)<-c("no")
db<-as.data.frame(natr2temp[!is.na(natr2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3temp[!is.na(natr3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4temp[!is.na(natr4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrtemp<-rbind(da,db,dc,dd)
r<-cbind(natrtemp,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1temp[!is.na(npte1temp)])
colnames(da)<-c("no")
db<-as.data.frame(npte2temp[!is.na(npte2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3temp[!is.na(npte3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4temp[!is.na(npte4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
nptetemp<-rbind(da,db,dc,dd)
s<-cbind(nptetemp,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1temp[!is.na(peqd1temp)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2temp[!is.na(peqd2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3temp[!is.na(peqd3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4temp[!is.na(peqd4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdtemp<-rbind(da,db,dc,dd)
t<-cbind(peqdtemp,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1temp[!is.na(pnec1temp)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2temp[!is.na(pnec2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3temp[!is.na(pnec3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4temp[!is.na(pnec4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pnectemp<-rbind(da,db,dc,dd)
u<-cbind(pnectemp,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1temp[!is.na(satl1temp)])
colnames(da)<-c("no")
db<-as.data.frame(satl2temp[!is.na(satl2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3temp[!is.na(satl3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4temp[!is.na(satl4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satltemp<-rbind(da,db,dc,dd)
v<-cbind(satltemp,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1temp[!is.na(warm1temp)])
colnames(da)<-c("no")
db<-as.data.frame(warm2temp[!is.na(warm2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3temp[!is.na(warm3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4temp[!is.na(warm4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmtemp<-rbind(da,db,dc,dd)
w<-cbind(warmtemp,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1temp[!is.na(wtra1temp)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2temp[!is.na(wtra2temp)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3temp[!is.na(wtra3temp)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4temp[!is.na(wtra4temp)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtratemp<-rbind(da,db,dc,dd)
x<-cbind(wtratemp,dg)
colnames(x)<-c("SEA", "x")


FINALtemp <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALtemp <- cbind(FINALtemp,col)
colnames(FINALtemp)<-c("SEA", "x", "col")

ggplot(data = FINALtemp, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal Temperature per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','A','Sp','Su','A','Su','A','Sp','Su','A',
                                           'Sp','Su','A','Su','A','Sp','Su','W','Sp','Su','A','W',
                                           'Sp','Su','A','W','Su','A','Su','A','Sp','Su','A',
                                           'W','Sp','Su','A','W','Sp','Su','A','W','Sp','A','W',
                                           'Su','A','Sp','Su','A','W','Sp','Su','A','W','Sp','Su',
                                           'Sp','A','Su','A','Sp','Su','A','Sp','Su','W','Sp','Su','A','W')) +
  scale_fill_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#71A9E8","#006699","#5012A1","#10C07F","#99FF33", "#CCFF99","#CCCC00","#92D050","#006600","#99ff99","#12A125","#589056","red","orange","#FAD208","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name="Temperature(\u00B0C)", limits=c(-2, 32)) + geom_vline(xintercept=14.5,linetype = 2) + geom_vline(xintercept=43.5,linetype = 2)+
  theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
                                                                               panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
                                                                               panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))


# NO3 ---------------------------------------------------------------------

setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/Box_Plots")

#ARCT
da<-as.data.frame(arct1no3[!is.na(arct1no3)])
colnames(da)<-c("no")
db<-as.data.frame(arct2no3[!is.na(arct2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(arct3no3[!is.na(arct3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(arct4no3[!is.na(arct4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(1,nrow(da)),rep(2,nrow(db)),rep(3,nrow(dc)),rep(4,nrow(dd))))
arctno3<-rbind(da,db,dc,dd)
a<-cbind(arctno3,dg)
colnames(a)<-c("SEA", "x")

#BPLR
da<-as.data.frame(bplr1no3[!is.na(bplr1no3)])
colnames(da)<-c("no")
db<-as.data.frame(bplr2no3[!is.na(bplr2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(bplr3no3[!is.na(bplr3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(bplr4no3[!is.na(bplr4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(5,nrow(da)),rep(6,nrow(db)),rep(7,nrow(dc)),rep(8,nrow(dd))))
bplrno3<-rbind(da,db,dc,dd)
b<-cbind(bplrno3,dg)
colnames(b)<-c("SEA", "x")

#SARC
da<-as.data.frame(sarc1no3[!is.na(sarc1no3)])
colnames(da)<-c("no")
db<-as.data.frame(sarc2no3[!is.na(sarc2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(sarc3no3[!is.na(sarc3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(sarc4no3[!is.na(sarc4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(9,nrow(da)),rep(10,nrow(db)),rep(11,nrow(dc)),rep(12,nrow(dd))))
sarcno3<-rbind(da,db,dc,dd)
c<-cbind(sarcno3,dg)
colnames(c)<-c("SEA", "x")

#ANTA
da<-as.data.frame(anta1no3[!is.na(anta1no3)])
colnames(da)<-c("no")
db<-as.data.frame(anta2no3[!is.na(anta2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(anta3no3[!is.na(anta3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(anta4no3[!is.na(anta4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(13,nrow(da)),rep(14,nrow(db)),rep(15,nrow(dc)),rep(16,nrow(dd))))
antano3<-rbind(da,db,dc,dd)
d<-cbind(antano3,dg)
colnames(d)<-c("SEA", "x")

#APLR
da<-as.data.frame(aplr1no3[!is.na(aplr1no3)])
colnames(da)<-c("no")
db<-as.data.frame(aplr2no3[!is.na(aplr2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(aplr3no3[!is.na(aplr3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(aplr4no3[!is.na(aplr4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(17,nrow(da)),rep(18,nrow(db)),rep(19,nrow(dc)),rep(20,nrow(dd))))
aplrno3<-rbind(da,db,dc,dd)
e<-cbind(aplrno3,dg)
colnames(e)<-c("SEA", "x")

#GEST
da<-as.data.frame(gest1no3[!is.na(gest1no3)])
colnames(da)<-c("no")
db<-as.data.frame(gest2no3[!is.na(gest2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(gest3no3[!is.na(gest3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(gest4no3[!is.na(gest4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(21,nrow(da)),rep(22,nrow(db)),rep(23,nrow(dc)),rep(24,nrow(dd))))
gestno3<-rbind(da,db,dc,dd)
f<-cbind(gestno3,dg)
colnames(f)<-c("SEA", "x")

#NADR
da<-as.data.frame(nadr1no3[!is.na(nadr1no3)])
colnames(da)<-c("no")
db<-as.data.frame(nadr2no3[!is.na(nadr2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(nadr3no3[!is.na(nadr3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(nadr4no3[!is.na(nadr4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(25,nrow(da)),rep(26,nrow(db)),rep(27,nrow(dc)),rep(28,nrow(dd))))
nadrno3<-rbind(da,db,dc,dd)
g<-cbind(nadrno3,dg)
colnames(g)<-c("SEA", "x")

#NASE
da<-as.data.frame(nase1no3[!is.na(nase1no3)])
colnames(da)<-c("no")
db<-as.data.frame(nase2no3[!is.na(nase2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(nase3no3[!is.na(nase3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(nase4no3[!is.na(nase4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(29,nrow(da)),rep(30,nrow(db)),rep(31,nrow(dc)),rep(32,nrow(dd))))
naseno3<-rbind(da,db,dc,dd)
h<-cbind(naseno3,dg)
colnames(h)<-c("SEA", "x")

#NASW
da<-as.data.frame(nasw1no3[!is.na(nasw1no3)])
colnames(da)<-c("no")
db<-as.data.frame(nasw2no3[!is.na(nasw2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(nasw3no3[!is.na(nasw3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(nasw4no3[!is.na(nasw4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(33,nrow(da)),rep(34,nrow(db)),rep(35,nrow(dc)),rep(36,nrow(dd))))
naswno3<-rbind(da,db,dc,dd)
i<-cbind(naswno3,dg)
colnames(i)<-c("SEA", "x")

#NPTW
da<-as.data.frame(nptw1no3[!is.na(nptw1no3)])
colnames(da)<-c("no")
db<-as.data.frame(nptw2no3[!is.na(nptw2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(nptw3no3[!is.na(nptw3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(nptw4no3[!is.na(nptw4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(37,nrow(da)),rep(38,nrow(db)),rep(39,nrow(dc)),rep(40,nrow(dd))))
nptwno3<-rbind(da,db,dc,dd)
j<-cbind(nptwno3,dg)
colnames(j)<-c("SEA", "x")

#PSAE
da<-as.data.frame(psae1no3[!is.na(psae1no3)])
colnames(da)<-c("no")
db<-as.data.frame(psae2no3[!is.na(psae2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(psae3no3[!is.na(psae3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(psae4no3[!is.na(psae4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(41,nrow(da)),rep(42,nrow(db)),rep(43,nrow(dc)),rep(44,nrow(dd))))
psaeno3<-rbind(da,db,dc,dd)
k<-cbind(psaeno3,dg)
colnames(k)<-c("SEA", "x")

# #SANT
da<-as.data.frame(sant1no3[!is.na(sant1no3)])
colnames(da)<-c("no")
db<-as.data.frame(sant2no3[!is.na(sant2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(sant3no3[!is.na(sant3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(sant4no3[!is.na(sant4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(45,nrow(da)),rep(46,nrow(db)),rep(47,nrow(dc)),rep(48,nrow(dd))))
santno3<-rbind(da,db,dc,dd)
l<-cbind(santno3,dg)
colnames(l)<-c("SEA", "x")

#SPSG
da<-as.data.frame(spsg1no3[!is.na(spsg1no3)])
colnames(da)<-c("no")
db<-as.data.frame(spsg2no3[!is.na(spsg2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(spsg3no3[!is.na(spsg3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(spsg4no3[!is.na(spsg4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(49,nrow(da)),rep(50,nrow(db)),rep(51,nrow(dc)),rep(52,nrow(dd))))
spsgno3<-rbind(da,db,dc,dd)
m<-cbind(spsgno3,dg)
colnames(m)<-c("SEA", "x")

#SSTC
da<-as.data.frame(sstc1no3[!is.na(sstc1no3)])
colnames(da)<-c("no")
db<-as.data.frame(sstc2no3[!is.na(sstc2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(sstc3no3[!is.na(sstc3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(sstc4no3[!is.na(sstc4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(53,nrow(da)),rep(54,nrow(db)),rep(55,nrow(dc)),rep(56,nrow(dd))))
sstcno3<-rbind(da,db,dc,dd)
n<-cbind(sstcno3,dg)
colnames(n)<-c("SEA", "x")

#ARCH
da<-as.data.frame(arch1no3[!is.na(arch1no3)])
colnames(da)<-c("no")
db<-as.data.frame(arch2no3[!is.na(arch2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(arch3no3[!is.na(arch3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(arch4no3[!is.na(arch4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(57,nrow(da)),rep(58,nrow(db)),rep(59,nrow(dc)),rep(60,nrow(dd))))
archno3<-rbind(da,db,dc,dd)
o<-cbind(archno3,dg)
colnames(o)<-c("SEA", "x")

#ISSG
da<-as.data.frame(issg1no3[!is.na(issg1no3)])
colnames(da)<-c("no")
db<-as.data.frame(issg2no3[!is.na(issg2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(issg3no3[!is.na(issg3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(issg4no3[!is.na(issg4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(61,nrow(da)),rep(62,nrow(db)),rep(63,nrow(dc)),rep(64,nrow(dd))))
issgno3<-rbind(da,db,dc,dd)
p<-cbind(issgno3,dg)
colnames(p)<-c("SEA", "x")


# #MONS
da<-as.data.frame(mons1no3[!is.na(mons1no3)])
colnames(da)<-c("no")
db<-as.data.frame(mons2no3[!is.na(mons2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(mons3no3[!is.na(mons3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(mons4no3[!is.na(mons4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(65,nrow(da)),rep(66,nrow(db)),rep(67,nrow(dc)),rep(68,nrow(dd))))
monsno3<-rbind(da,db,dc,dd)
q<-cbind(monsno3,dg)
colnames(q)<-c("SEA", "x")



#NATR
da<-as.data.frame(natr1no3[!is.na(natr1no3)])
colnames(da)<-c("no")
db<-as.data.frame(natr2no3[!is.na(natr2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(natr3no3[!is.na(natr3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(natr4no3[!is.na(natr4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(69,nrow(da)),rep(70,nrow(db)),rep(71,nrow(dc)),rep(72,nrow(dd))))
natrno3<-rbind(da,db,dc,dd)
r<-cbind(natrno3,dg)
colnames(r)<-c("SEA", "x")

# #NPTE
da<-as.data.frame(npte1no3[!is.na(npte1no3)])
colnames(da)<-c("no")
db<-as.data.frame(npte2no3[!is.na(npte2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(npte3no3[!is.na(npte3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(npte4no3[!is.na(npte4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(73,nrow(da)),rep(74,nrow(db)),rep(75,nrow(dc)),rep(76,nrow(dd))))
npteno3<-rbind(da,db,dc,dd)
s<-cbind(npteno3,dg)
colnames(s)<-c("SEA", "x")


#PEQD
da<-as.data.frame(peqd1no3[!is.na(peqd1no3)])
colnames(da)<-c("no")
db<-as.data.frame(peqd2no3[!is.na(peqd2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(peqd3no3[!is.na(peqd3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(peqd4no3[!is.na(peqd4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(77,nrow(da)),rep(78,nrow(db)),rep(79,nrow(dc)),rep(80,nrow(dd))))
peqdno3<-rbind(da,db,dc,dd)
t<-cbind(peqdno3,dg)
colnames(t)<-c("SEA", "x")



#PNEC
da<-as.data.frame(pnec1no3[!is.na(pnec1no3)])
colnames(da)<-c("no")
db<-as.data.frame(pnec2no3[!is.na(pnec2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(pnec3no3[!is.na(pnec3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(pnec4no3[!is.na(pnec4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(81,nrow(da)),rep(82,nrow(db)),rep(83,nrow(dc)),rep(84,nrow(dd))))
pnecno3<-rbind(da,db,dc,dd)
u<-cbind(pnecno3,dg)
colnames(u)<-c("SEA", "x")


#SATL
da<-as.data.frame(satl1no3[!is.na(satl1no3)])
colnames(da)<-c("no")
db<-as.data.frame(satl2no3[!is.na(satl2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(satl3no3[!is.na(satl3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(satl4no3[!is.na(satl4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(85,nrow(da)),rep(86,nrow(db)),rep(87,nrow(dc)),rep(88,nrow(dd))))
satlno3<-rbind(da,db,dc,dd)
v<-cbind(satlno3,dg)
colnames(v)<-c("SEA", "x")


#WARM
da<-as.data.frame(warm1no3[!is.na(warm1no3)])
colnames(da)<-c("no")
db<-as.data.frame(warm2no3[!is.na(warm2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(warm3no3[!is.na(warm3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(warm4no3[!is.na(warm4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(89,nrow(da)),rep(90,nrow(db)),rep(91,nrow(dc)),rep(92,nrow(dd))))
warmno3<-rbind(da,db,dc,dd)
w<-cbind(warmno3,dg)
colnames(w)<-c("SEA", "x")


#WTRA
da<-as.data.frame(wtra1no3[!is.na(wtra1no3)])
colnames(da)<-c("no")
db<-as.data.frame(wtra2no3[!is.na(wtra2no3)])
colnames(db)<-c("no")
dc<-as.data.frame(wtra3no3[!is.na(wtra3no3)])
colnames(dc)<-c("no")
dd<-as.data.frame(wtra4no3[!is.na(wtra4no3)])
colnames(dd)<-c("no")
dg <- (col1 = c(rep(93,nrow(da)),rep(94,nrow(db)),rep(95,nrow(dc)),rep(96,nrow(dd))))
wtrano3<-rbind(da,db,dc,dd)
x<-cbind(wtrano3,dg)
colnames(x)<-c("SEA", "x")


FINALno3 <- rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)
col <- c(rep('a',nrow(a)),rep('b',nrow(b)),rep('c',nrow(c)),rep('d',nrow(d)),rep('e',nrow(e)),
         rep('f',nrow(f)),rep('g',nrow(g)),rep('h',nrow(h)),rep('i',nrow(i)),rep('j',nrow(j)),
         rep('k',nrow(k)),rep('l',nrow(l)),rep('m',nrow(m)),rep('n',nrow(n)),rep('o',nrow(o)),
         rep('p',nrow(p)),rep('q',nrow(q)),rep('r',nrow(r)),rep('s',nrow(s)),rep('t',nrow(t)),
         rep('u',nrow(u)),rep('v',nrow(v)),rep('w',nrow(w)),rep('x',nrow(x)))
FINALno3 <- cbind(FINALno3,col)
colnames(FINALno3)<-c("SEA", "x", "col")

ggplot(data = FINALno3, aes(x = factor(x), y = SEA, fill = col, color = col)) + ggtitle("Seasonal NO_3 per Province") +
  geom_boxplot(alpha = 0.1) + 
  scale_x_discrete(name="Season", labels=c('Sp','Su','Sp','Su','A','A','Su',
                                           'Sp','Su','Sp','A','Sp','Su','A','Su','A','A',
                                           'Sp','Su','A','W','Su','A','A','W','Su','A',
                                           'A','Sp','A','A','Su','A','W','Su','A')) +
  scale_fill_manual(values=c("blue","purple","#006699","#10C07F","#99FF33", "#CCFF99","#CCCC00","#006600","#99ff99","#12A125","#589056","red","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) + 
  scale_color_manual(values=c("blue","purple","#006699","#10C07F","#99FF33", "#CCFF99","#CCCC00","#006600","#99ff99","#12A125","#589056","red","#BA0A0A","#D21C43","#FFFF08","#EF892D","#E85A1E","#EC8C9C","#EC0D7D")) +
  scale_y_continuous(name=expression(NO_3(mu,M/kg)), limits=c(-2, 20)) + geom_vline(xintercept=7.5,linetype = 2) + geom_vline(xintercept=25.5,linetype = 2)+
  theme(legend.position = "none", panel.background = element_rect(fill = "white", colour = "#C3C3C3", size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "#F0F0F0"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "#F0F0F0"))

