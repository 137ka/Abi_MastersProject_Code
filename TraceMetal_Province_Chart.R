#Making a large table of Longhurst Provinces for trace metals

#Assigning the directory of the script
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/")
rm(list=ls(all=TRUE))

# Read trace metal data file
pigtab<-read.table(file="TheTRACEMETALFile.csv",sep=",",h=T)
finalData<-subset(pigtab,!(is.na(pigtab["Region_mean"]) | is.na(pigtab["LAT"])))


# Assigining variables to each of the Longhurst Provinces
BPLR <- subset(finalData, finalData$Region_mean==1)
ARCT <- subset(finalData, finalData$Region_mean==2)
SARC <- subset(finalData, finalData$Region_mean==3)
NADR <- subset(finalData, finalData$Region_mean==4)
GEST <- subset(finalData, finalData$Region_mean==5)
NASW <- subset(finalData, finalData$Region_mean==6)
NATR <- subset(finalData, finalData$Region_mean==7)
WTRA <- subset(finalData, finalData$Region_mean==8)
SATL <- subset(finalData, finalData$Region_mean==10)
NASE <- subset(finalData, finalData$Region_mean==18)
MONS <- subset(finalData, finalData$Region_mean==30)
ISSG <- subset(finalData, finalData$Region_mean==31)
PSAE <- subset(finalData, finalData$Region_mean==51)
NPTE <- subset(finalData, finalData$Region_mean==55)
NPTW <- subset(finalData, finalData$Region_mean==56)
SPSG <- subset(finalData, finalData$Region_mean==59)
NPTE1 <- subset(finalData, finalData$Region_mean==60)
PNEC <- subset(finalData, finalData$Region_mean==61)
PEQD <- subset(finalData, finalData$Region_mean==62)
WARM <- subset(finalData, finalData$Region_mean==63)
ARCH <- subset(finalData, finalData$Region_mean==64)
PNEC1 <- subset(finalData, finalData$Region_mean==67)
SSTC <- subset(finalData, finalData$Region_mean==80)
SANT <- subset(finalData, finalData$Region_mean==81)
ANTA <- subset(finalData, finalData$Region_mean==82)
APLR <- subset(finalData, finalData$Region_mean==83)




# Calculating Values for provinces in number order: BPLR

write.table(BPLR,"BPLRtm.csv",col.names=T,row.names=F,sep=',')
obs <- 9374
avg_chla <- mean(BPLR$CHLA, na.rm = TRUE)
SE_chla <- sd(BPLR$CHLA, na.rm = TRUE)/sqrt(length((BPLR$CHLA)))
avg_depth <- mean(BPLR$DEPTH, na.rm = TRUE)
avg_temp <- mean(BPLR$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(BPLR$TEMPERATURE, na.rm = TRUE)/sqrt(length((BPLR$TEMPERATURE)))
avg_oxy <- mean(BPLR$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(BPLR$OXYGEN, na.rm = TRUE)/sqrt(length((BPLR$OXYGEN)))
avg_sal <- mean(BPLR$SALINITY, na.rm = TRUE)
SE_sal <- sd(BPLR$SALINITY, na.rm = TRUE)/sqrt(length((BPLR$SALINITY)))
avg_nn <- mean(BPLR$NITRATE, na.rm = TRUE)
SE_nn <- sd(BPLR$NITRATE, na.rm = TRUE)/sqrt(length((BPLR$NITRATE)))
avg_nh3 <- mean(BPLR$NH3, na.rm = TRUE)
SE_nh3 <- sd(BPLR$NH3, na.rm = TRUE)/sqrt(length((BPLR$NH3)))
avg_phos <- mean(BPLR$PHOS, na.rm = TRUE)
SE_phos <- sd(BPLR$PHOS, na.rm = TRUE)/sqrt(length((BPLR$PHOS)))
avg_sio2 <- mean(BPLR$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(BPLR$SILICATE, na.rm = TRUE)/sqrt(length((BPLR$SILICATE)))
avg_mn <- mean(BPLR$Mn, na.rm = TRUE)
SE_mn <- sd(BPLR$Mn, na.rm = TRUE)/sqrt(length((BPLR$Mn)))
avg_ni <- mean(BPLR$Ni, na.rm = TRUE)
SE_ni <- sd(BPLR$Ni, na.rm = TRUE)/sqrt(length((BPLR$Ni)))
avg_cd <- mean(BPLR$Cd, na.rm = TRUE)
SE_cd <- sd(BPLR$Cd, na.rm = TRUE)/sqrt(length((BPLR$Cd)))
avg_co <- mean(BPLR$Co, na.rm = TRUE)
SE_co <- sd(BPLR$Co, na.rm = TRUE)/sqrt(length((BPLR$Co)))
avg_cu <- mean(BPLR$Cu, na.rm = TRUE)
SE_cu <- sd(BPLR$Cu, na.rm = TRUE)/sqrt(length((BPLR$Cu)))
avg_fe <- mean(BPLR$Fe, na.rm = TRUE)
SE_fe <- sd(BPLR$Fe, na.rm = TRUE)/sqrt(length((BPLR$Fe)))
avg_zn <- mean(BPLR$Zn, na.rm = TRUE)
SE_zn <- sd(BPLR$Zn, na.rm = TRUE)/sqrt(length((BPLR$Zn)))


TM_BPLR<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_BPLR<-as.data.frame(TM_BPLR)
colnames(TM_BPLR)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")
                    


# ARCT --------------------------------------------------------------------


# Calculating Values for provinces in number order: ARCT
write.table(ARCT,"ARCTtm.csv",col.names=T,row.names=F,sep=',')
obs <- 2334
avg_chla <- mean(ARCT$CHLA, na.rm = TRUE)
SE_chla <- sd(ARCT$CHLA, na.rm = TRUE)/sqrt(length((ARCT$CHLA)))
avg_depth <- mean(ARCT$DEPTH, na.rm = TRUE)
avg_temp <- mean(ARCT$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(ARCT$TEMPERATURE, na.rm = TRUE)/sqrt(length((ARCT$TEMPERATURE)))
avg_oxy <- mean(ARCT$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(ARCT$OXYGEN, na.rm = TRUE)/sqrt(length((ARCT$OXYGEN)))
avg_sal <- mean(ARCT$SALINITY, na.rm = TRUE)
SE_sal <- sd(ARCT$SALINITY, na.rm = TRUE)/sqrt(length((ARCT$SALINITY)))
avg_nn <- mean(ARCT$NITRATE, na.rm = TRUE)
SE_nn <- sd(ARCT$NITRATE, na.rm = TRUE)/sqrt(length((ARCT$NITRATE)))
avg_nh3 <- mean(ARCT$NH3, na.rm = TRUE)
SE_nh3 <- sd(ARCT$NH3, na.rm = TRUE)/sqrt(length((ARCT$NH3)))
avg_phos <- mean(ARCT$PHOS, na.rm = TRUE)
SE_phos <- sd(ARCT$PHOS, na.rm = TRUE)/sqrt(length((ARCT$PHOS)))
avg_sio2 <- mean(ARCT$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(ARCT$SILICATE, na.rm = TRUE)/sqrt(length((ARCT$SILICATE)))
avg_mn <- mean(ARCT$Mn, na.rm = TRUE)
SE_mn <- sd(ARCT$Mn, na.rm = TRUE)/sqrt(length((ARCT$Mn)))
avg_ni <- mean(ARCT$Ni, na.rm = TRUE)
SE_ni <- sd(ARCT$Ni, na.rm = TRUE)/sqrt(length((ARCT$Ni)))
avg_cd <- mean(ARCT$Cd, na.rm = TRUE)
SE_cd <- sd(ARCT$Cd, na.rm = TRUE)/sqrt(length((ARCT$Cd)))
avg_co <- mean(ARCT$Co, na.rm = TRUE)
SE_co <- sd(ARCT$Co, na.rm = TRUE)/sqrt(length((ARCT$Co)))
avg_cu <- mean(ARCT$Cu, na.rm = TRUE)
SE_cu <- sd(ARCT$Cu, na.rm = TRUE)/sqrt(length((ARCT$Cu)))
avg_fe <- mean(ARCT$Fe, na.rm = TRUE)
SE_fe <- sd(ARCT$Fe, na.rm = TRUE)/sqrt(length((ARCT$Fe)))
avg_zn <- mean(ARCT$Zn, na.rm = TRUE)
SE_zn <- sd(ARCT$Zn, na.rm = TRUE)/sqrt(length((ARCT$Zn)))


TM_ARCT<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_ARCT<-as.data.frame(TM_ARCT)
colnames(TM_ARCT)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")

# SARC --------------------------------------------------------------------


# Calculating Values for provinces in number order: SARC
write.table(SARC,"SARCtm.csv",col.names=T,row.names=F,sep=',')
obs <- 467
avg_chla <- mean(SARC$CHLA, na.rm = TRUE)
SE_chla <- sd(SARC$CHLA, na.rm = TRUE)/sqrt(length((SARC$CHLA)))
avg_depth <- mean(SARC$DEPTH, na.rm = TRUE)
avg_temp <- mean(SARC$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(SARC$TEMPERATURE, na.rm = TRUE)/sqrt(length((SARC$TEMPERATURE)))
avg_oxy <- mean(SARC$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(SARC$OXYGEN, na.rm = TRUE)/sqrt(length((SARC$OXYGEN)))
avg_sal <- mean(SARC$SALINITY, na.rm = TRUE)
SE_sal <- sd(SARC$SALINITY, na.rm = TRUE)/sqrt(length((SARC$SALINITY)))
avg_nn <- mean(SARC$NITRATE, na.rm = TRUE)
SE_nn <- sd(SARC$NITRATE, na.rm = TRUE)/sqrt(length((SARC$NITRATE)))
avg_nh3 <- mean(SARC$NH3, na.rm = TRUE)
SE_nh3 <- sd(SARC$NH3, na.rm = TRUE)/sqrt(length((SARC$NH3)))
avg_phos <- mean(SARC$PHOS, na.rm = TRUE)
SE_phos <- sd(SARC$PHOS, na.rm = TRUE)/sqrt(length((SARC$PHOS)))
avg_sio2 <- mean(SARC$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(SARC$SILICATE, na.rm = TRUE)/sqrt(length((SARC$SILICATE)))
avg_mn <- mean(SARC$Mn, na.rm = TRUE)
SE_mn <- sd(SARC$Mn, na.rm = TRUE)/sqrt(length((SARC$Mn)))
avg_ni <- mean(SARC$Ni, na.rm = TRUE)
SE_ni <- sd(SARC$Ni, na.rm = TRUE)/sqrt(length((SARC$Ni)))
avg_cd <- mean(SARC$Cd, na.rm = TRUE)
SE_cd <- sd(SARC$Cd, na.rm = TRUE)/sqrt(length((SARC$Cd)))
avg_co <- mean(SARC$Co, na.rm = TRUE)
SE_co <- sd(SARC$Co, na.rm = TRUE)/sqrt(length((SARC$Co)))
avg_cu <- mean(SARC$Cu, na.rm = TRUE)
SE_cu <- sd(SARC$Cu, na.rm = TRUE)/sqrt(length((SARC$Cu)))
avg_fe <- mean(SARC$Fe, na.rm = TRUE)
SE_fe <- sd(SARC$Fe, na.rm = TRUE)/sqrt(length((SARC$Fe)))
avg_zn <- mean(SARC$Zn, na.rm = TRUE)
SE_zn <- sd(SARC$Zn, na.rm = TRUE)/sqrt(length((SARC$Zn)))


TM_SARC<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_SARC<-as.data.frame(TM_SARC)
colnames(TM_SARC)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# NADR --------------------------------------------------------------------

write.table(NADR,"NADRtm.csv",col.names=T,row.names=F,sep=',')
obs <- 603
avg_chla <- mean(NADR$CHLA, na.rm = TRUE)
SE_chla <- sd(NADR$CHLA, na.rm = TRUE)/sqrt(length((NADR$CHLA)))
avg_depth <- mean(NADR$DEPTH, na.rm = TRUE)
avg_temp <- mean(NADR$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(NADR$TEMPERATURE, na.rm = TRUE)/sqrt(length((NADR$TEMPERATURE)))
avg_oxy <- mean(NADR$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(NADR$OXYGEN, na.rm = TRUE)/sqrt(length((NADR$OXYGEN)))
avg_sal <- mean(NADR$SALINITY, na.rm = TRUE)
SE_sal <- sd(NADR$SALINITY, na.rm = TRUE)/sqrt(length((NADR$SALINITY)))
avg_nn <- mean(NADR$NITRATE, na.rm = TRUE)
SE_nn <- sd(NADR$NITRATE, na.rm = TRUE)/sqrt(length((NADR$NITRATE)))
avg_nh3 <- mean(NADR$NH3, na.rm = TRUE)
SE_nh3 <- sd(NADR$NH3, na.rm = TRUE)/sqrt(length((NADR$NH3)))
avg_phos <- mean(NADR$PHOS, na.rm = TRUE)
SE_phos <- sd(NADR$PHOS, na.rm = TRUE)/sqrt(length((NADR$PHOS)))
avg_sio2 <- mean(NADR$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(NADR$SILICATE, na.rm = TRUE)/sqrt(length((NADR$SILICATE)))
avg_mn <- mean(NADR$Mn, na.rm = TRUE)
SE_mn <- sd(NADR$Mn, na.rm = TRUE)/sqrt(length((NADR$Mn)))
avg_ni <- mean(NADR$Ni, na.rm = TRUE)
SE_ni <- sd(NADR$Ni, na.rm = TRUE)/sqrt(length((NADR$Ni)))
avg_cd <- mean(NADR$Cd, na.rm = TRUE)
SE_cd <- sd(NADR$Cd, na.rm = TRUE)/sqrt(length((NADR$Cd)))
avg_co <- mean(NADR$Co, na.rm = TRUE)
SE_co <- sd(NADR$Co, na.rm = TRUE)/sqrt(length((NADR$Co)))
avg_cu <- mean(NADR$Cu, na.rm = TRUE)
SE_cu <- sd(NADR$Cu, na.rm = TRUE)/sqrt(length((NADR$Cu)))
avg_fe <- mean(NADR$Fe, na.rm = TRUE)
SE_fe <- sd(NADR$Fe, na.rm = TRUE)/sqrt(length((NADR$Fe)))
avg_zn <- mean(NADR$Zn, na.rm = TRUE)
SE_zn <- sd(NADR$Zn, na.rm = TRUE)/sqrt(length((NADR$Zn)))


TM_NADR<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_NADR<-as.data.frame(TM_NADR)
colnames(TM_NADR)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# GEST --------------------------------------------------------------------

write.table(GEST,"GESTtm.csv",col.names=T,row.names=F,sep=',')
obs <- 158
avg_chla <- mean(GEST$CHLA, na.rm = TRUE)
SE_chla <- sd(GEST$CHLA, na.rm = TRUE)/sqrt(length((GEST$CHLA)))
avg_depth <- mean(GEST$DEPTH, na.rm = TRUE)
avg_temp <- mean(GEST$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(GEST$TEMPERATURE, na.rm = TRUE)/sqrt(length((GEST$TEMPERATURE)))
avg_oxy <- mean(GEST$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(GEST$OXYGEN, na.rm = TRUE)/sqrt(length((GEST$OXYGEN)))
avg_sal <- mean(GEST$SALINITY, na.rm = TRUE)
SE_sal <- sd(GEST$SALINITY, na.rm = TRUE)/sqrt(length((GEST$SALINITY)))
avg_nn <- mean(GEST$NITRATE, na.rm = TRUE)
SE_nn <- sd(GEST$NITRATE, na.rm = TRUE)/sqrt(length((GEST$NITRATE)))
avg_nh3 <- mean(GEST$NH3, na.rm = TRUE)
SE_nh3 <- sd(GEST$NH3, na.rm = TRUE)/sqrt(length((GEST$NH3)))
avg_phos <- mean(GEST$PHOS, na.rm = TRUE)
SE_phos <- sd(GEST$PHOS, na.rm = TRUE)/sqrt(length((GEST$PHOS)))
avg_sio2 <- mean(GEST$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(GEST$SILICATE, na.rm = TRUE)/sqrt(length((GEST$SILICATE)))
avg_mn <- mean(GEST$Mn, na.rm = TRUE)
SE_mn <- sd(GEST$Mn, na.rm = TRUE)/sqrt(length((GEST$Mn)))
avg_ni <- mean(GEST$Ni, na.rm = TRUE)
SE_ni <- sd(GEST$Ni, na.rm = TRUE)/sqrt(length((GEST$Ni)))
avg_cd <- mean(GEST$Cd, na.rm = TRUE)
SE_cd <- sd(GEST$Cd, na.rm = TRUE)/sqrt(length((GEST$Cd)))
avg_co <- mean(GEST$Co, na.rm = TRUE)
SE_co <- sd(GEST$Co, na.rm = TRUE)/sqrt(length((GEST$Co)))
avg_cu <- mean(GEST$Cu, na.rm = TRUE)
SE_cu <- sd(GEST$Cu, na.rm = TRUE)/sqrt(length((GEST$Cu)))
avg_fe <- mean(GEST$Fe, na.rm = TRUE)
SE_fe <- sd(GEST$Fe, na.rm = TRUE)/sqrt(length((GEST$Fe)))
avg_zn <- mean(GEST$Zn, na.rm = TRUE)
SE_zn <- sd(GEST$Zn, na.rm = TRUE)/sqrt(length((GEST$Zn)))


TM_GEST<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_GEST<-as.data.frame(TM_GEST)
colnames(TM_GEST)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# NASW --------------------------------------------------------------------

write.table(NASW,"NASWtm.csv",col.names=T,row.names=F,sep=',')
obs <- 848
avg_chla <- mean(NASW$CHLA, na.rm = TRUE)
SE_chla <- sd(NASW$CHLA, na.rm = TRUE)/sqrt(length((NASW$CHLA)))
avg_depth <- mean(NASW$DEPTH, na.rm = TRUE)
avg_temp <- mean(NASW$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(NASW$TEMPERATURE, na.rm = TRUE)/sqrt(length((NASW$TEMPERATURE)))
avg_oxy <- mean(NASW$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(NASW$OXYGEN, na.rm = TRUE)/sqrt(length((NASW$OXYGEN)))
avg_sal <- mean(NASW$SALINITY, na.rm = TRUE)
SE_sal <- sd(NASW$SALINITY, na.rm = TRUE)/sqrt(length((NASW$SALINITY)))
avg_nn <- mean(NASW$NITRATE, na.rm = TRUE)
SE_nn <- sd(NASW$NITRATE, na.rm = TRUE)/sqrt(length((NASW$NITRATE)))
avg_nh3 <- mean(NASW$NH3, na.rm = TRUE)
SE_nh3 <- sd(NASW$NH3, na.rm = TRUE)/sqrt(length((NASW$NH3)))
avg_phos <- mean(NASW$PHOS, na.rm = TRUE)
SE_phos <- sd(NASW$PHOS, na.rm = TRUE)/sqrt(length((NASW$PHOS)))
avg_sio2 <- mean(NASW$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(NASW$SILICATE, na.rm = TRUE)/sqrt(length((NASW$SILICATE)))
avg_mn <- mean(NASW$Mn, na.rm = TRUE)
SE_mn <- sd(NASW$Mn, na.rm = TRUE)/sqrt(length((NASW$Mn)))
avg_ni <- mean(NASW$Ni, na.rm = TRUE)
SE_ni <- sd(NASW$Ni, na.rm = TRUE)/sqrt(length((NASW$Ni)))
avg_cd <- mean(NASW$Cd, na.rm = TRUE)
SE_cd <- sd(NASW$Cd, na.rm = TRUE)/sqrt(length((NASW$Cd)))
avg_co <- mean(NASW$Co, na.rm = TRUE)
SE_co <- sd(NASW$Co, na.rm = TRUE)/sqrt(length((NASW$Co)))
avg_cu <- mean(NASW$Cu, na.rm = TRUE)
SE_cu <- sd(NASW$Cu, na.rm = TRUE)/sqrt(length((NASW$Cu)))
avg_fe <- mean(NASW$Fe, na.rm = TRUE)
SE_fe <- sd(NASW$Fe, na.rm = TRUE)/sqrt(length((NASW$Fe)))
avg_zn <- mean(NASW$Zn, na.rm = TRUE)
SE_zn <- sd(NASW$Zn, na.rm = TRUE)/sqrt(length((NASW$Zn)))


TM_NASW<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_NASW<-as.data.frame(TM_NASW)
colnames(TM_NASW)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")



# NATR --------------------------------------------------------------------

write.table(NATR,"NATRtm.csv",col.names=T,row.names=F,sep=',')
obs <- 1171
avg_chla <- mean(NATR$CHLA, na.rm = TRUE)
SE_chla <- sd(NATR$CHLA, na.rm = TRUE)/sqrt(length((NATR$CHLA)))
avg_depth <- mean(NATR$DEPTH, na.rm = TRUE)
avg_temp <- mean(NATR$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(NATR$TEMPERATURE, na.rm = TRUE)/sqrt(length((NATR$TEMPERATURE)))
avg_oxy <- mean(NATR$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(NATR$OXYGEN, na.rm = TRUE)/sqrt(length((NATR$OXYGEN)))
avg_sal <- mean(NATR$SALINITY, na.rm = TRUE)
SE_sal <- sd(NATR$SALINITY, na.rm = TRUE)/sqrt(length((NATR$SALINITY)))
avg_nn <- mean(NATR$NITRATE, na.rm = TRUE)
SE_nn <- sd(NATR$NITRATE, na.rm = TRUE)/sqrt(length((NATR$NITRATE)))
avg_nh3 <- mean(NATR$NH3, na.rm = TRUE)
SE_nh3 <- sd(NATR$NH3, na.rm = TRUE)/sqrt(length((NATR$NH3)))
avg_phos <- mean(NATR$PHOS, na.rm = TRUE)
SE_phos <- sd(NATR$PHOS, na.rm = TRUE)/sqrt(length((NATR$PHOS)))
avg_sio2 <- mean(NATR$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(NATR$SILICATE, na.rm = TRUE)/sqrt(length((NATR$SILICATE)))
avg_mn <- mean(NATR$Mn, na.rm = TRUE)
SE_mn <- sd(NATR$Mn, na.rm = TRUE)/sqrt(length((NATR$Mn)))
avg_ni <- mean(NATR$Ni, na.rm = TRUE)
SE_ni <- sd(NATR$Ni, na.rm = TRUE)/sqrt(length((NATR$Ni)))
avg_cd <- mean(NATR$Cd, na.rm = TRUE)
SE_cd <- sd(NATR$Cd, na.rm = TRUE)/sqrt(length((NATR$Cd)))
avg_co <- mean(NATR$Co, na.rm = TRUE)
SE_co <- sd(NATR$Co, na.rm = TRUE)/sqrt(length((NATR$Co)))
avg_cu <- mean(NATR$Cu, na.rm = TRUE)
SE_cu <- sd(NATR$Cu, na.rm = TRUE)/sqrt(length((NATR$Cu)))
avg_fe <- mean(NATR$Fe, na.rm = TRUE)
SE_fe <- sd(NATR$Fe, na.rm = TRUE)/sqrt(length((NATR$Fe)))
avg_zn <- mean(NATR$Zn, na.rm = TRUE)
SE_zn <- sd(NATR$Zn, na.rm = TRUE)/sqrt(length((NATR$Zn)))


TM_NATR<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_NATR<-as.data.frame(TM_NATR)
colnames(TM_NATR)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")



# WTRA --------------------------------------------------------------------

write.table(WTRA,"WTRAtm.csv",col.names=T,row.names=F,sep=',')
obs <- 675
avg_chla <- mean(WTRA$CHLA, na.rm = TRUE)
SE_chla <- sd(WTRA$CHLA, na.rm = TRUE)/sqrt(length((WTRA$CHLA)))
avg_depth <- mean(WTRA$DEPTH, na.rm = TRUE)
avg_temp <- mean(WTRA$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(WTRA$TEMPERATURE, na.rm = TRUE)/sqrt(length((WTRA$TEMPERATURE)))
avg_oxy <- mean(WTRA$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(WTRA$OXYGEN, na.rm = TRUE)/sqrt(length((WTRA$OXYGEN)))
avg_sal <- mean(WTRA$SALINITY, na.rm = TRUE)
SE_sal <- sd(WTRA$SALINITY, na.rm = TRUE)/sqrt(length((WTRA$SALINITY)))
avg_nn <- mean(WTRA$NITRATE, na.rm = TRUE)
SE_nn <- sd(WTRA$NITRATE, na.rm = TRUE)/sqrt(length((WTRA$NITRATE)))
avg_nh3 <- mean(WTRA$NH3, na.rm = TRUE)
SE_nh3 <- sd(WTRA$NH3, na.rm = TRUE)/sqrt(length((WTRA$NH3)))
avg_phos <- mean(WTRA$PHOS, na.rm = TRUE)
SE_phos <- sd(WTRA$PHOS, na.rm = TRUE)/sqrt(length((WTRA$PHOS)))
avg_sio2 <- mean(WTRA$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(WTRA$SILICATE, na.rm = TRUE)/sqrt(length((WTRA$SILICATE)))
avg_mn <- mean(WTRA$Mn, na.rm = TRUE)
SE_mn <- sd(WTRA$Mn, na.rm = TRUE)/sqrt(length((WTRA$Mn)))
avg_ni <- mean(WTRA$Ni, na.rm = TRUE)
SE_ni <- sd(WTRA$Ni, na.rm = TRUE)/sqrt(length((WTRA$Ni)))
avg_cd <- mean(WTRA$Cd, na.rm = TRUE)
SE_cd <- sd(WTRA$Cd, na.rm = TRUE)/sqrt(length((WTRA$Cd)))
avg_co <- mean(WTRA$Co, na.rm = TRUE)
SE_co <- sd(WTRA$Co, na.rm = TRUE)/sqrt(length((WTRA$Co)))
avg_cu <- mean(WTRA$Cu, na.rm = TRUE)
SE_cu <- sd(WTRA$Cu, na.rm = TRUE)/sqrt(length((WTRA$Cu)))
avg_fe <- mean(WTRA$Fe, na.rm = TRUE)
SE_fe <- sd(WTRA$Fe, na.rm = TRUE)/sqrt(length((WTRA$Fe)))
avg_zn <- mean(WTRA$Zn, na.rm = TRUE)
SE_zn <- sd(WTRA$Zn, na.rm = TRUE)/sqrt(length((WTRA$Zn)))


TM_WTRA<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_WTRA<-as.data.frame(TM_WTRA)
colnames(TM_WTRA)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")

# SATL --------------------------------------------------------------------

write.table(SATL,"SATLtm.csv",col.names=T,row.names=F,sep=',')
obs <- 1492
avg_chla <- mean(SATL$CHLA, na.rm = TRUE)
SE_chla <- sd(SATL$CHLA, na.rm = TRUE)/sqrt(length((SATL$CHLA)))
avg_depth <- mean(SATL$DEPTH, na.rm = TRUE)
avg_temp <- mean(SATL$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(SATL$TEMPERATURE, na.rm = TRUE)/sqrt(length((SATL$TEMPERATURE)))
avg_oxy <- mean(SATL$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(SATL$OXYGEN, na.rm = TRUE)/sqrt(length((SATL$OXYGEN)))
avg_sal <- mean(SATL$SALINITY, na.rm = TRUE)
SE_sal <- sd(SATL$SALINITY, na.rm = TRUE)/sqrt(length((SATL$SALINITY)))
avg_nn <- mean(SATL$NITRATE, na.rm = TRUE)
SE_nn <- sd(SATL$NITRATE, na.rm = TRUE)/sqrt(length((SATL$NITRATE)))
avg_nh3 <- mean(SATL$NH3, na.rm = TRUE)
SE_nh3 <- sd(SATL$NH3, na.rm = TRUE)/sqrt(length((SATL$NH3)))
avg_phos <- mean(SATL$PHOS, na.rm = TRUE)
SE_phos <- sd(SATL$PHOS, na.rm = TRUE)/sqrt(length((SATL$PHOS)))
avg_sio2 <- mean(SATL$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(SATL$SILICATE, na.rm = TRUE)/sqrt(length((SATL$SILICATE)))
avg_mn <- mean(SATL$Mn, na.rm = TRUE)
SE_mn <- sd(SATL$Mn, na.rm = TRUE)/sqrt(length((SATL$Mn)))
avg_ni <- mean(SATL$Ni, na.rm = TRUE)
SE_ni <- sd(SATL$Ni, na.rm = TRUE)/sqrt(length((SATL$Ni)))
avg_cd <- mean(SATL$Cd, na.rm = TRUE)
SE_cd <- sd(SATL$Cd, na.rm = TRUE)/sqrt(length((SATL$Cd)))
avg_co <- mean(SATL$Co, na.rm = TRUE)
SE_co <- sd(SATL$Co, na.rm = TRUE)/sqrt(length((SATL$Co)))
avg_cu <- mean(SATL$Cu, na.rm = TRUE)
SE_cu <- sd(SATL$Cu, na.rm = TRUE)/sqrt(length((SATL$Cu)))
avg_fe <- mean(SATL$Fe, na.rm = TRUE)
SE_fe <- sd(SATL$Fe, na.rm = TRUE)/sqrt(length((SATL$Fe)))
avg_zn <- mean(SATL$Zn, na.rm = TRUE)
SE_zn <- sd(SATL$Zn, na.rm = TRUE)/sqrt(length((SATL$Zn)))


TM_SATL<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_SATL<-as.data.frame(TM_SATL)
colnames(TM_SATL)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")

# NASE --------------------------------------------------------------------

write.table(NASE,"NASEtm.csv",col.names=T,row.names=F,sep=',')
obs <- 651
avg_chla <- mean(NASE$CHLA, na.rm = TRUE)
SE_chla <- sd(NASE$CHLA, na.rm = TRUE)/sqrt(length((NASE$CHLA)))
avg_depth <- mean(NASE$DEPTH, na.rm = TRUE)
avg_temp <- mean(NASE$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(NASE$TEMPERATURE, na.rm = TRUE)/sqrt(length((NASE$TEMPERATURE)))
avg_oxy <- mean(NASE$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(NASE$OXYGEN, na.rm = TRUE)/sqrt(length((NASE$OXYGEN)))
avg_sal <- mean(NASE$SALINITY, na.rm = TRUE)
SE_sal <- sd(NASE$SALINITY, na.rm = TRUE)/sqrt(length((NASE$SALINITY)))
avg_nn <- mean(NASE$NITRATE, na.rm = TRUE)
SE_nn <- sd(NASE$NITRATE, na.rm = TRUE)/sqrt(length((NASE$NITRATE)))
avg_nh3 <- mean(NASE$NH3, na.rm = TRUE)
SE_nh3 <- sd(NASE$NH3, na.rm = TRUE)/sqrt(length((NASE$NH3)))
avg_phos <- mean(NASE$PHOS, na.rm = TRUE)
SE_phos <- sd(NASE$PHOS, na.rm = TRUE)/sqrt(length((NASE$PHOS)))
avg_sio2 <- mean(NASE$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(NASE$SILICATE, na.rm = TRUE)/sqrt(length((NASE$SILICATE)))
avg_mn <- mean(NASE$Mn, na.rm = TRUE)
SE_mn <- sd(NASE$Mn, na.rm = TRUE)/sqrt(length((NASE$Mn)))
avg_ni <- mean(NASE$Ni, na.rm = TRUE)
SE_ni <- sd(NASE$Ni, na.rm = TRUE)/sqrt(length((NASE$Ni)))
avg_cd <- mean(NASE$Cd, na.rm = TRUE)
SE_cd <- sd(NASE$Cd, na.rm = TRUE)/sqrt(length((NASE$Cd)))
avg_co <- mean(NASE$Co, na.rm = TRUE)
SE_co <- sd(NASE$Co, na.rm = TRUE)/sqrt(length((NASE$Co)))
avg_cu <- mean(NASE$Cu, na.rm = TRUE)
SE_cu <- sd(NASE$Cu, na.rm = TRUE)/sqrt(length((NASE$Cu)))
avg_fe <- mean(NASE$Fe, na.rm = TRUE)
SE_fe <- sd(NASE$Fe, na.rm = TRUE)/sqrt(length((NASE$Fe)))
avg_zn <- mean(NASE$Zn, na.rm = TRUE)
SE_zn <- sd(NASE$Zn, na.rm = TRUE)/sqrt(length((NASE$Zn)))


TM_NASE<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_NASE<-as.data.frame(TM_NASE)
colnames(TM_NASE)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# MONS --------------------------------------------------------------------

write.table(MONS,"MONStm.csv",col.names=T,row.names=F,sep=',')
obs <- 506
avg_chla <- mean(MONS$CHLA, na.rm = TRUE)
SE_chla <- sd(MONS$CHLA, na.rm = TRUE)/sqrt(length((MONS$CHLA)))
avg_depth <- mean(MONS$DEPTH, na.rm = TRUE)
avg_temp <- mean(MONS$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(MONS$TEMPERATURE, na.rm = TRUE)/sqrt(length((MONS$TEMPERATURE)))
avg_oxy <- mean(MONS$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(MONS$OXYGEN, na.rm = TRUE)/sqrt(length((MONS$OXYGEN)))
avg_sal <- mean(MONS$SALINITY, na.rm = TRUE)
SE_sal <- sd(MONS$SALINITY, na.rm = TRUE)/sqrt(length((MONS$SALINITY)))
avg_nn <- mean(MONS$NITRATE, na.rm = TRUE)
SE_nn <- sd(MONS$NITRATE, na.rm = TRUE)/sqrt(length((MONS$NITRATE)))
avg_nh3 <- mean(MONS$NH3, na.rm = TRUE)
SE_nh3 <- sd(MONS$NH3, na.rm = TRUE)/sqrt(length((MONS$NH3)))
avg_phos <- mean(MONS$PHOS, na.rm = TRUE)
SE_phos <- sd(MONS$PHOS, na.rm = TRUE)/sqrt(length((MONS$PHOS)))
avg_sio2 <- mean(MONS$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(MONS$SILICATE, na.rm = TRUE)/sqrt(length((MONS$SILICATE)))
avg_mn <- mean(MONS$Mn, na.rm = TRUE)
SE_mn <- sd(MONS$Mn, na.rm = TRUE)/sqrt(length((MONS$Mn)))
avg_ni <- mean(MONS$Ni, na.rm = TRUE)
SE_ni <- sd(MONS$Ni, na.rm = TRUE)/sqrt(length((MONS$Ni)))
avg_cd <- mean(MONS$Cd, na.rm = TRUE)
SE_cd <- sd(MONS$Cd, na.rm = TRUE)/sqrt(length((MONS$Cd)))
avg_co <- mean(MONS$Co, na.rm = TRUE)
SE_co <- sd(MONS$Co, na.rm = TRUE)/sqrt(length((MONS$Co)))
avg_cu <- mean(MONS$Cu, na.rm = TRUE)
SE_cu <- sd(MONS$Cu, na.rm = TRUE)/sqrt(length((MONS$Cu)))
avg_fe <- mean(MONS$Fe, na.rm = TRUE)
SE_fe <- sd(MONS$Fe, na.rm = TRUE)/sqrt(length((MONS$Fe)))
avg_zn <- mean(MONS$Zn, na.rm = TRUE)
SE_zn <- sd(MONS$Zn, na.rm = TRUE)/sqrt(length((MONS$Zn)))


TM_MONS<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_MONS<-as.data.frame(TM_MONS)
colnames(TM_MONS)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# ISSG --------------------------------------------------------------------

write.table(ISSG,"ISSGtm.csv",col.names=T,row.names=F,sep=',')
obs <- 101
avg_chla <- mean(ISSG$CHLA, na.rm = TRUE)
SE_chla <- sd(ISSG$CHLA, na.rm = TRUE)/sqrt(length((ISSG$CHLA)))
avg_depth <- mean(ISSG$DEPTH, na.rm = TRUE)
avg_temp <- mean(ISSG$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(ISSG$TEMPERATURE, na.rm = TRUE)/sqrt(length((ISSG$TEMPERATURE)))
avg_oxy <- mean(ISSG$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(ISSG$OXYGEN, na.rm = TRUE)/sqrt(length((ISSG$OXYGEN)))
avg_sal <- mean(ISSG$SALINITY, na.rm = TRUE)
SE_sal <- sd(ISSG$SALINITY, na.rm = TRUE)/sqrt(length((ISSG$SALINITY)))
avg_nn <- mean(ISSG$NITRATE, na.rm = TRUE)
SE_nn <- sd(ISSG$NITRATE, na.rm = TRUE)/sqrt(length((ISSG$NITRATE)))
avg_nh3 <- mean(ISSG$NH3, na.rm = TRUE)
SE_nh3 <- sd(ISSG$NH3, na.rm = TRUE)/sqrt(length((ISSG$NH3)))
avg_phos <- mean(ISSG$PHOS, na.rm = TRUE)
SE_phos <- sd(ISSG$PHOS, na.rm = TRUE)/sqrt(length((ISSG$PHOS)))
avg_sio2 <- mean(ISSG$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(ISSG$SILICATE, na.rm = TRUE)/sqrt(length((ISSG$SILICATE)))
avg_mn <- mean(ISSG$Mn, na.rm = TRUE)
SE_mn <- sd(ISSG$Mn, na.rm = TRUE)/sqrt(length((ISSG$Mn)))
avg_ni <- mean(ISSG$Ni, na.rm = TRUE)
SE_ni <- sd(ISSG$Ni, na.rm = TRUE)/sqrt(length((ISSG$Ni)))
avg_cd <- mean(ISSG$Cd, na.rm = TRUE)
SE_cd <- sd(ISSG$Cd, na.rm = TRUE)/sqrt(length((ISSG$Cd)))
avg_co <- mean(ISSG$Co, na.rm = TRUE)
SE_co <- sd(ISSG$Co, na.rm = TRUE)/sqrt(length((ISSG$Co)))
avg_cu <- mean(ISSG$Cu, na.rm = TRUE)
SE_cu <- sd(ISSG$Cu, na.rm = TRUE)/sqrt(length((ISSG$Cu)))
avg_fe <- mean(ISSG$Fe, na.rm = TRUE)
SE_fe <- sd(ISSG$Fe, na.rm = TRUE)/sqrt(length((ISSG$Fe)))
avg_zn <- mean(ISSG$Zn, na.rm = TRUE)
SE_zn <- sd(ISSG$Zn, na.rm = TRUE)/sqrt(length((ISSG$Zn)))


TM_ISSG<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_ISSG<-as.data.frame(TM_ISSG)
colnames(TM_ISSG)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# PSAE --------------------------------------------------------------------

write.table(PSAE,"PSAEtm.csv",col.names=T,row.names=F,sep=',')
obs <- 720
avg_chla <- mean(PSAE$CHLA, na.rm = TRUE)
SE_chla <- sd(PSAE$CHLA, na.rm = TRUE)/sqrt(length((PSAE$CHLA)))
avg_depth <- mean(PSAE$DEPTH, na.rm = TRUE)
avg_temp <- mean(PSAE$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(PSAE$TEMPERATURE, na.rm = TRUE)/sqrt(length((PSAE$TEMPERATURE)))
avg_oxy <- mean(PSAE$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(PSAE$OXYGEN, na.rm = TRUE)/sqrt(length((PSAE$OXYGEN)))
avg_sal <- mean(PSAE$SALINITY, na.rm = TRUE)
SE_sal <- sd(PSAE$SALINITY, na.rm = TRUE)/sqrt(length((PSAE$SALINITY)))
avg_nn <- mean(PSAE$NITRATE, na.rm = TRUE)
SE_nn <- sd(PSAE$NITRATE, na.rm = TRUE)/sqrt(length((PSAE$NITRATE)))
avg_nh3 <- mean(PSAE$NH3, na.rm = TRUE)
SE_nh3 <- sd(PSAE$NH3, na.rm = TRUE)/sqrt(length((PSAE$NH3)))
avg_phos <- mean(PSAE$PHOS, na.rm = TRUE)
SE_phos <- sd(PSAE$PHOS, na.rm = TRUE)/sqrt(length((PSAE$PHOS)))
avg_sio2 <- mean(PSAE$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(PSAE$SILICATE, na.rm = TRUE)/sqrt(length((PSAE$SILICATE)))
avg_mn <- mean(PSAE$Mn, na.rm = TRUE)
SE_mn <- sd(PSAE$Mn, na.rm = TRUE)/sqrt(length((PSAE$Mn)))
avg_ni <- mean(PSAE$Ni, na.rm = TRUE)
SE_ni <- sd(PSAE$Ni, na.rm = TRUE)/sqrt(length((PSAE$Ni)))
avg_cd <- mean(PSAE$Cd, na.rm = TRUE)
SE_cd <- sd(PSAE$Cd, na.rm = TRUE)/sqrt(length((PSAE$Cd)))
avg_co <- mean(PSAE$Co, na.rm = TRUE)
SE_co <- sd(PSAE$Co, na.rm = TRUE)/sqrt(length((PSAE$Co)))
avg_cu <- mean(PSAE$Cu, na.rm = TRUE)
SE_cu <- sd(PSAE$Cu, na.rm = TRUE)/sqrt(length((PSAE$Cu)))
avg_fe <- mean(PSAE$Fe, na.rm = TRUE)
SE_fe <- sd(PSAE$Fe, na.rm = TRUE)/sqrt(length((PSAE$Fe)))
avg_zn <- mean(PSAE$Zn, na.rm = TRUE)
SE_zn <- sd(PSAE$Zn, na.rm = TRUE)/sqrt(length((PSAE$Zn)))


TM_PSAE<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_PSAE<-as.data.frame(TM_PSAE)
colnames(TM_PSAE)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")

# NPTE --------------------------------------------------------------------

NPTE = rbind(NPTE, NPTE1)
write.table(NPTE,"NPTEtm.csv",col.names=T,row.names=F,sep=',')
obs <- 701
avg_chla <- mean(NPTE$CHLA, na.rm = TRUE)
SE_chla <- sd(NPTE$CHLA, na.rm = TRUE)/sqrt(length((NPTE$CHLA)))
avg_depth <- mean(NPTE$DEPTH, na.rm = TRUE)
avg_temp <- mean(NPTE$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(NPTE$TEMPERATURE, na.rm = TRUE)/sqrt(length((NPTE$TEMPERATURE)))
avg_oxy <- mean(NPTE$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(NPTE$OXYGEN, na.rm = TRUE)/sqrt(length((NPTE$OXYGEN)))
avg_sal <- mean(NPTE$SALINITY, na.rm = TRUE)
SE_sal <- sd(NPTE$SALINITY, na.rm = TRUE)/sqrt(length((NPTE$SALINITY)))
avg_nn <- mean(NPTE$NITRATE, na.rm = TRUE)
SE_nn <- sd(NPTE$NITRATE, na.rm = TRUE)/sqrt(length((NPTE$NITRATE)))
avg_nh3 <- mean(NPTE$NH3, na.rm = TRUE)
SE_nh3 <- sd(NPTE$NH3, na.rm = TRUE)/sqrt(length((NPTE$NH3)))
avg_phos <- mean(NPTE$PHOS, na.rm = TRUE)
SE_phos <- sd(NPTE$PHOS, na.rm = TRUE)/sqrt(length((NPTE$PHOS)))
avg_sio2 <- mean(NPTE$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(NPTE$SILICATE, na.rm = TRUE)/sqrt(length((NPTE$SILICATE)))
avg_mn <- mean(NPTE$Mn, na.rm = TRUE)
SE_mn <- sd(NPTE$Mn, na.rm = TRUE)/sqrt(length((NPTE$Mn)))
avg_ni <- mean(NPTE$Ni, na.rm = TRUE)
SE_ni <- sd(NPTE$Ni, na.rm = TRUE)/sqrt(length((NPTE$Ni)))
avg_cd <- mean(NPTE$Cd, na.rm = TRUE)
SE_cd <- sd(NPTE$Cd, na.rm = TRUE)/sqrt(length((NPTE$Cd)))
avg_co <- mean(NPTE$Co, na.rm = TRUE)
SE_co <- sd(NPTE$Co, na.rm = TRUE)/sqrt(length((NPTE$Co)))
avg_cu <- mean(NPTE$Cu, na.rm = TRUE)
SE_cu <- sd(NPTE$Cu, na.rm = TRUE)/sqrt(length((NPTE$Cu)))
avg_fe <- mean(NPTE$Fe, na.rm = TRUE)
SE_fe <- sd(NPTE$Fe, na.rm = TRUE)/sqrt(length((NPTE$Fe)))
avg_zn <- mean(NPTE$Zn, na.rm = TRUE)
SE_zn <- sd(NPTE$Zn, na.rm = TRUE)/sqrt(length((NPTE$Zn)))


TM_NPTE<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_NPTE<-as.data.frame(TM_NPTE)
colnames(TM_NPTE)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# NPTW --------------------------------------------------------------------

write.table(NPTW,"NPTWtm.csv",col.names=T,row.names=F,sep=',')
obs <- 200
avg_chla <- mean(NPTW$CHLA, na.rm = TRUE)
SE_chla <- sd(NPTW$CHLA, na.rm = TRUE)/sqrt(length((NPTW$CHLA)))
avg_depth <- mean(NPTW$DEPTH, na.rm = TRUE)
avg_temp <- mean(NPTW$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(NPTW$TEMPERATURE, na.rm = TRUE)/sqrt(length((NPTW$TEMPERATURE)))
avg_oxy <- mean(NPTW$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(NPTW$OXYGEN, na.rm = TRUE)/sqrt(length((NPTW$OXYGEN)))
avg_sal <- mean(NPTW$SALINITY, na.rm = TRUE)
SE_sal <- sd(NPTW$SALINITY, na.rm = TRUE)/sqrt(length((NPTW$SALINITY)))
avg_nn <- mean(NPTW$NITRATE, na.rm = TRUE)
SE_nn <- sd(NPTW$NITRATE, na.rm = TRUE)/sqrt(length((NPTW$NITRATE)))
avg_nh3 <- mean(NPTW$NH3, na.rm = TRUE)
SE_nh3 <- sd(NPTW$NH3, na.rm = TRUE)/sqrt(length((NPTW$NH3)))
avg_phos <- mean(NPTW$PHOS, na.rm = TRUE)
SE_phos <- sd(NPTW$PHOS, na.rm = TRUE)/sqrt(length((NPTW$PHOS)))
avg_sio2 <- mean(NPTW$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(NPTW$SILICATE, na.rm = TRUE)/sqrt(length((NPTW$SILICATE)))
avg_mn <- mean(NPTW$Mn, na.rm = TRUE)
SE_mn <- sd(NPTW$Mn, na.rm = TRUE)/sqrt(length((NPTW$Mn)))
avg_ni <- mean(NPTW$Ni, na.rm = TRUE)
SE_ni <- sd(NPTW$Ni, na.rm = TRUE)/sqrt(length((NPTW$Ni)))
avg_cd <- mean(NPTW$Cd, na.rm = TRUE)
SE_cd <- sd(NPTW$Cd, na.rm = TRUE)/sqrt(length((NPTW$Cd)))
avg_co <- mean(NPTW$Co, na.rm = TRUE)
SE_co <- sd(NPTW$Co, na.rm = TRUE)/sqrt(length((NPTW$Co)))
avg_cu <- mean(NPTW$Cu, na.rm = TRUE)
SE_cu <- sd(NPTW$Cu, na.rm = TRUE)/sqrt(length((NPTW$Cu)))
avg_fe <- mean(NPTW$Fe, na.rm = TRUE)
SE_fe <- sd(NPTW$Fe, na.rm = TRUE)/sqrt(length((NPTW$Fe)))
avg_zn <- mean(NPTW$Zn, na.rm = TRUE)
SE_zn <- sd(NPTW$Zn, na.rm = TRUE)/sqrt(length((NPTW$Zn)))


TM_NPTW<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_NPTW<-as.data.frame(TM_NPTW)
colnames(TM_NPTW)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# SPSG --------------------------------------------------------------------

write.table(SPSG,"SPSGtm.csv",col.names=T,row.names=F,sep=',')
obs <- 2750
avg_chla <- mean(SPSG$CHLA, na.rm = TRUE)
SE_chla <- sd(SPSG$CHLA, na.rm = TRUE)/sqrt(length((SPSG$CHLA)))
avg_depth <- mean(SPSG$DEPTH, na.rm = TRUE)
avg_temp <- mean(SPSG$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(SPSG$TEMPERATURE, na.rm = TRUE)/sqrt(length((SPSG$TEMPERATURE)))
avg_oxy <- mean(SPSG$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(SPSG$OXYGEN, na.rm = TRUE)/sqrt(length((SPSG$OXYGEN)))
avg_sal <- mean(SPSG$SALINITY, na.rm = TRUE)
SE_sal <- sd(SPSG$SALINITY, na.rm = TRUE)/sqrt(length((SPSG$SALINITY)))
avg_nn <- mean(SPSG$NITRATE, na.rm = TRUE)
SE_nn <- sd(SPSG$NITRATE, na.rm = TRUE)/sqrt(length((SPSG$NITRATE)))
avg_nh3 <- mean(SPSG$NH3, na.rm = TRUE)
SE_nh3 <- sd(SPSG$NH3, na.rm = TRUE)/sqrt(length((SPSG$NH3)))
avg_phos <- mean(SPSG$PHOS, na.rm = TRUE)
SE_phos <- sd(SPSG$PHOS, na.rm = TRUE)/sqrt(length((SPSG$PHOS)))
avg_sio2 <- mean(SPSG$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(SPSG$SILICATE, na.rm = TRUE)/sqrt(length((SPSG$SILICATE)))
avg_mn <- mean(SPSG$Mn, na.rm = TRUE)
SE_mn <- sd(SPSG$Mn, na.rm = TRUE)/sqrt(length((SPSG$Mn)))
avg_ni <- mean(SPSG$Ni, na.rm = TRUE)
SE_ni <- sd(SPSG$Ni, na.rm = TRUE)/sqrt(length((SPSG$Ni)))
avg_cd <- mean(SPSG$Cd, na.rm = TRUE)
SE_cd <- sd(SPSG$Cd, na.rm = TRUE)/sqrt(length((SPSG$Cd)))
avg_co <- mean(SPSG$Co, na.rm = TRUE)
SE_co <- sd(SPSG$Co, na.rm = TRUE)/sqrt(length((SPSG$Co)))
avg_cu <- mean(SPSG$Cu, na.rm = TRUE)
SE_cu <- sd(SPSG$Cu, na.rm = TRUE)/sqrt(length((SPSG$Cu)))
avg_fe <- mean(SPSG$Fe, na.rm = TRUE)
SE_fe <- sd(SPSG$Fe, na.rm = TRUE)/sqrt(length((SPSG$Fe)))
avg_zn <- mean(SPSG$Zn, na.rm = TRUE)
SE_zn <- sd(SPSG$Zn, na.rm = TRUE)/sqrt(length((SPSG$Zn)))


TM_SPSG<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_SPSG<-as.data.frame(TM_SPSG)
colnames(TM_SPSG)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# PNEC --------------------------------------------------------------------

PNEC <- rbind(PNEC, PNEC1)
write.table(PNEC,"PNECtm.csv",col.names=T,row.names=F,sep=',')
obs <- 217
avg_chla <- mean(PNEC$CHLA, na.rm = TRUE)
SE_chla <- sd(PNEC$CHLA, na.rm = TRUE)/sqrt(length((PNEC$CHLA)))
avg_depth <- mean(PNEC$DEPTH, na.rm = TRUE)
avg_temp <- mean(PNEC$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(PNEC$TEMPERATURE, na.rm = TRUE)/sqrt(length((PNEC$TEMPERATURE)))
avg_oxy <- mean(PNEC$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(PNEC$OXYGEN, na.rm = TRUE)/sqrt(length((PNEC$OXYGEN)))
avg_sal <- mean(PNEC$SALINITY, na.rm = TRUE)
SE_sal <- sd(PNEC$SALINITY, na.rm = TRUE)/sqrt(length((PNEC$SALINITY)))
avg_nn <- mean(PNEC$NITRATE, na.rm = TRUE)
SE_nn <- sd(PNEC$NITRATE, na.rm = TRUE)/sqrt(length((PNEC$NITRATE)))
avg_nh3 <- mean(PNEC$NH3, na.rm = TRUE)
SE_nh3 <- sd(PNEC$NH3, na.rm = TRUE)/sqrt(length((PNEC$NH3)))
avg_phos <- mean(PNEC$PHOS, na.rm = TRUE)
SE_phos <- sd(PNEC$PHOS, na.rm = TRUE)/sqrt(length((PNEC$PHOS)))
avg_sio2 <- mean(PNEC$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(PNEC$SILICATE, na.rm = TRUE)/sqrt(length((PNEC$SILICATE)))
avg_mn <- mean(PNEC$Mn, na.rm = TRUE)
SE_mn <- sd(PNEC$Mn, na.rm = TRUE)/sqrt(length((PNEC$Mn)))
avg_ni <- mean(PNEC$Ni, na.rm = TRUE)
SE_ni <- sd(PNEC$Ni, na.rm = TRUE)/sqrt(length((PNEC$Ni)))
avg_cd <- mean(PNEC$Cd, na.rm = TRUE)
SE_cd <- sd(PNEC$Cd, na.rm = TRUE)/sqrt(length((PNEC$Cd)))
avg_co <- mean(PNEC$Co, na.rm = TRUE)
SE_co <- sd(PNEC$Co, na.rm = TRUE)/sqrt(length((PNEC$Co)))
avg_cu <- mean(PNEC$Cu, na.rm = TRUE)
SE_cu <- sd(PNEC$Cu, na.rm = TRUE)/sqrt(length((PNEC$Cu)))
avg_fe <- mean(PNEC$Fe, na.rm = TRUE)
SE_fe <- sd(PNEC$Fe, na.rm = TRUE)/sqrt(length((PNEC$Fe)))
avg_zn <- mean(PNEC$Zn, na.rm = TRUE)
SE_zn <- sd(PNEC$Zn, na.rm = TRUE)/sqrt(length((PNEC$Zn)))


TM_PNEC<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_PNEC<-as.data.frame(TM_PNEC)
colnames(TM_PNEC)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# PEQD --------------------------------------------------------------------

write.table(PEQD,"PEQDtm.csv",col.names=T,row.names=F,sep=',')
obs <- 395
avg_chla <- mean(PEQD$CHLA, na.rm = TRUE)
SE_chla <- sd(PEQD$CHLA, na.rm = TRUE)/sqrt(length((PEQD$CHLA)))
avg_depth <- mean(PEQD$DEPTH, na.rm = TRUE)
avg_temp <- mean(PEQD$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(PEQD$TEMPERATURE, na.rm = TRUE)/sqrt(length((PEQD$TEMPERATURE)))
avg_oxy <- mean(PEQD$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(PEQD$OXYGEN, na.rm = TRUE)/sqrt(length((PEQD$OXYGEN)))
avg_sal <- mean(PEQD$SALINITY, na.rm = TRUE)
SE_sal <- sd(PEQD$SALINITY, na.rm = TRUE)/sqrt(length((PEQD$SALINITY)))
avg_nn <- mean(PEQD$NITRATE, na.rm = TRUE)
SE_nn <- sd(PEQD$NITRATE, na.rm = TRUE)/sqrt(length((PEQD$NITRATE)))
avg_nh3 <- mean(PEQD$NH3, na.rm = TRUE)
SE_nh3 <- sd(PEQD$NH3, na.rm = TRUE)/sqrt(length((PEQD$NH3)))
avg_phos <- mean(PEQD$PHOS, na.rm = TRUE)
SE_phos <- sd(PEQD$PHOS, na.rm = TRUE)/sqrt(length((PEQD$PHOS)))
avg_sio2 <- mean(PEQD$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(PEQD$SILICATE, na.rm = TRUE)/sqrt(length((PEQD$SILICATE)))
avg_mn <- mean(PEQD$Mn, na.rm = TRUE)
SE_mn <- sd(PEQD$Mn, na.rm = TRUE)/sqrt(length((PEQD$Mn)))
avg_ni <- mean(PEQD$Ni, na.rm = TRUE)
SE_ni <- sd(PEQD$Ni, na.rm = TRUE)/sqrt(length((PEQD$Ni)))
avg_cd <- mean(PEQD$Cd, na.rm = TRUE)
SE_cd <- sd(PEQD$Cd, na.rm = TRUE)/sqrt(length((PEQD$Cd)))
avg_co <- mean(PEQD$Co, na.rm = TRUE)
SE_co <- sd(PEQD$Co, na.rm = TRUE)/sqrt(length((PEQD$Co)))
avg_cu <- mean(PEQD$Cu, na.rm = TRUE)
SE_cu <- sd(PEQD$Cu, na.rm = TRUE)/sqrt(length((PEQD$Cu)))
avg_fe <- mean(PEQD$Fe, na.rm = TRUE)
SE_fe <- sd(PEQD$Fe, na.rm = TRUE)/sqrt(length((PEQD$Fe)))
avg_zn <- mean(PEQD$Zn, na.rm = TRUE)
SE_zn <- sd(PEQD$Zn, na.rm = TRUE)/sqrt(length((PEQD$Zn)))


TM_PEQD<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_PEQD<-as.data.frame(TM_PEQD)
colnames(TM_PEQD)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# WARM --------------------------------------------------------------------

write.table(WARM,"WARMtm.csv",col.names=T,row.names=F,sep=',')
obs <- 198
avg_chla <- mean(WARM$CHLA, na.rm = TRUE)
SE_chla <- sd(WARM$CHLA, na.rm = TRUE)/sqrt(length((WARM$CHLA)))
avg_depth <- mean(WARM$DEPTH, na.rm = TRUE)
avg_temp <- mean(WARM$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(WARM$TEMPERATURE, na.rm = TRUE)/sqrt(length((WARM$TEMPERATURE)))
avg_oxy <- mean(WARM$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(WARM$OXYGEN, na.rm = TRUE)/sqrt(length((WARM$OXYGEN)))
avg_sal <- mean(WARM$SALINITY, na.rm = TRUE)
SE_sal <- sd(WARM$SALINITY, na.rm = TRUE)/sqrt(length((WARM$SALINITY)))
avg_nn <- mean(WARM$NITRATE, na.rm = TRUE)
SE_nn <- sd(WARM$NITRATE, na.rm = TRUE)/sqrt(length((WARM$NITRATE)))
avg_nh3 <- mean(WARM$NH3, na.rm = TRUE)
SE_nh3 <- sd(WARM$NH3, na.rm = TRUE)/sqrt(length((WARM$NH3)))
avg_phos <- mean(WARM$PHOS, na.rm = TRUE)
SE_phos <- sd(WARM$PHOS, na.rm = TRUE)/sqrt(length((WARM$PHOS)))
avg_sio2 <- mean(WARM$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(WARM$SILICATE, na.rm = TRUE)/sqrt(length((WARM$SILICATE)))
avg_mn <- mean(WARM$Mn, na.rm = TRUE)
SE_mn <- sd(WARM$Mn, na.rm = TRUE)/sqrt(length((WARM$Mn)))
avg_ni <- mean(WARM$Ni, na.rm = TRUE)
SE_ni <- sd(WARM$Ni, na.rm = TRUE)/sqrt(length((WARM$Ni)))
avg_cd <- mean(WARM$Cd, na.rm = TRUE)
SE_cd <- sd(WARM$Cd, na.rm = TRUE)/sqrt(length((WARM$Cd)))
avg_co <- mean(WARM$Co, na.rm = TRUE)
SE_co <- sd(WARM$Co, na.rm = TRUE)/sqrt(length((WARM$Co)))
avg_cu <- mean(WARM$Cu, na.rm = TRUE)
SE_cu <- sd(WARM$Cu, na.rm = TRUE)/sqrt(length((WARM$Cu)))
avg_fe <- mean(WARM$Fe, na.rm = TRUE)
SE_fe <- sd(WARM$Fe, na.rm = TRUE)/sqrt(length((WARM$Fe)))
avg_zn <- mean(WARM$Zn, na.rm = TRUE)
SE_zn <- sd(WARM$Zn, na.rm = TRUE)/sqrt(length((WARM$Zn)))


TM_WARM<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_WARM<-as.data.frame(TM_WARM)
colnames(TM_WARM)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# ARCH --------------------------------------------------------------------

write.table(ARCH,"ARCHtm.csv",col.names=T,row.names=F,sep=',')
obs <- 1414
avg_chla <- mean(ARCH$CHLA, na.rm = TRUE)
SE_chla <- sd(ARCH$CHLA, na.rm = TRUE)/sqrt(length((ARCH$CHLA)))
avg_depth <- mean(ARCH$DEPTH, na.rm = TRUE)
avg_temp <- mean(ARCH$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(ARCH$TEMPERATURE, na.rm = TRUE)/sqrt(length((ARCH$TEMPERATURE)))
avg_oxy <- mean(ARCH$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(ARCH$OXYGEN, na.rm = TRUE)/sqrt(length((ARCH$OXYGEN)))
avg_sal <- mean(ARCH$SALINITY, na.rm = TRUE)
SE_sal <- sd(ARCH$SALINITY, na.rm = TRUE)/sqrt(length((ARCH$SALINITY)))
avg_nn <- mean(ARCH$NITRATE, na.rm = TRUE)
SE_nn <- sd(ARCH$NITRATE, na.rm = TRUE)/sqrt(length((ARCH$NITRATE)))
avg_nh3 <- mean(ARCH$NH3, na.rm = TRUE)
SE_nh3 <- sd(ARCH$NH3, na.rm = TRUE)/sqrt(length((ARCH$NH3)))
avg_phos <- mean(ARCH$PHOS, na.rm = TRUE)
SE_phos <- sd(ARCH$PHOS, na.rm = TRUE)/sqrt(length((ARCH$PHOS)))
avg_sio2 <- mean(ARCH$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(ARCH$SILICATE, na.rm = TRUE)/sqrt(length((ARCH$SILICATE)))
avg_mn <- mean(ARCH$Mn, na.rm = TRUE)
SE_mn <- sd(ARCH$Mn, na.rm = TRUE)/sqrt(length((ARCH$Mn)))
avg_ni <- mean(ARCH$Ni, na.rm = TRUE)
SE_ni <- sd(ARCH$Ni, na.rm = TRUE)/sqrt(length((ARCH$Ni)))
avg_cd <- mean(ARCH$Cd, na.rm = TRUE)
SE_cd <- sd(ARCH$Cd, na.rm = TRUE)/sqrt(length((ARCH$Cd)))
avg_co <- mean(ARCH$Co, na.rm = TRUE)
SE_co <- sd(ARCH$Co, na.rm = TRUE)/sqrt(length((ARCH$Co)))
avg_cu <- mean(ARCH$Cu, na.rm = TRUE)
SE_cu <- sd(ARCH$Cu, na.rm = TRUE)/sqrt(length((ARCH$Cu)))
avg_fe <- mean(ARCH$Fe, na.rm = TRUE)
SE_fe <- sd(ARCH$Fe, na.rm = TRUE)/sqrt(length((ARCH$Fe)))
avg_zn <- mean(ARCH$Zn, na.rm = TRUE)
SE_zn <- sd(ARCH$Zn, na.rm = TRUE)/sqrt(length((ARCH$Zn)))


TM_ARCH<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_ARCH<-as.data.frame(TM_ARCH)
colnames(TM_ARCH)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# SSTC --------------------------------------------------------------------

write.table(SSTC,"SSTCtm.csv",col.names=T,row.names=F,sep=',')
obs <- 1947
avg_chla <- mean(SSTC$CHLA, na.rm = TRUE)
SE_chla <- sd(SSTC$CHLA, na.rm = TRUE)/sqrt(length((SSTC$CHLA)))
avg_depth <- mean(SSTC$DEPTH, na.rm = TRUE)
avg_temp <- mean(SSTC$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(SSTC$TEMPERATURE, na.rm = TRUE)/sqrt(length((SSTC$TEMPERATURE)))
avg_oxy <- mean(SSTC$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(SSTC$OXYGEN, na.rm = TRUE)/sqrt(length((SSTC$OXYGEN)))
avg_sal <- mean(SSTC$SALINITY, na.rm = TRUE)
SE_sal <- sd(SSTC$SALINITY, na.rm = TRUE)/sqrt(length((SSTC$SALINITY)))
avg_nn <- mean(SSTC$NITRATE, na.rm = TRUE)
SE_nn <- sd(SSTC$NITRATE, na.rm = TRUE)/sqrt(length((SSTC$NITRATE)))
avg_nh3 <- mean(SSTC$NH3, na.rm = TRUE)
SE_nh3 <- sd(SSTC$NH3, na.rm = TRUE)/sqrt(length((SSTC$NH3)))
avg_phos <- mean(SSTC$PHOS, na.rm = TRUE)
SE_phos <- sd(SSTC$PHOS, na.rm = TRUE)/sqrt(length((SSTC$PHOS)))
avg_sio2 <- mean(SSTC$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(SSTC$SILICATE, na.rm = TRUE)/sqrt(length((SSTC$SILICATE)))
avg_mn <- mean(SSTC$Mn, na.rm = TRUE)
SE_mn <- sd(SSTC$Mn, na.rm = TRUE)/sqrt(length((SSTC$Mn)))
avg_ni <- mean(SSTC$Ni, na.rm = TRUE)
SE_ni <- sd(SSTC$Ni, na.rm = TRUE)/sqrt(length((SSTC$Ni)))
avg_cd <- mean(SSTC$Cd, na.rm = TRUE)
SE_cd <- sd(SSTC$Cd, na.rm = TRUE)/sqrt(length((SSTC$Cd)))
avg_co <- mean(SSTC$Co, na.rm = TRUE)
SE_co <- sd(SSTC$Co, na.rm = TRUE)/sqrt(length((SSTC$Co)))
avg_cu <- mean(SSTC$Cu, na.rm = TRUE)
SE_cu <- sd(SSTC$Cu, na.rm = TRUE)/sqrt(length((SSTC$Cu)))
avg_fe <- mean(SSTC$Fe, na.rm = TRUE)
SE_fe <- sd(SSTC$Fe, na.rm = TRUE)/sqrt(length((SSTC$Fe)))
avg_zn <- mean(SSTC$Zn, na.rm = TRUE)
SE_zn <- sd(SSTC$Zn, na.rm = TRUE)/sqrt(length((SSTC$Zn)))


TM_SSTC<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_SSTC<-as.data.frame(TM_SSTC)
colnames(TM_SSTC)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# SANT --------------------------------------------------------------------

write.table(SANT,"SANTtm.csv",col.names=T,row.names=F,sep=',')
obs <- 5285
avg_chla <- mean(SANT$CHLA, na.rm = TRUE)
SE_chla <- sd(SANT$CHLA, na.rm = TRUE)/sqrt(length((SANT$CHLA)))
avg_depth <- mean(SANT$DEPTH, na.rm = TRUE)
avg_temp <- mean(SANT$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(SANT$TEMPERATURE, na.rm = TRUE)/sqrt(length((SANT$TEMPERATURE)))
avg_oxy <- mean(SANT$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(SANT$OXYGEN, na.rm = TRUE)/sqrt(length((SANT$OXYGEN)))
avg_sal <- mean(SANT$SALINITY, na.rm = TRUE)
SE_sal <- sd(SANT$SALINITY, na.rm = TRUE)/sqrt(length((SANT$SALINITY)))
avg_nn <- mean(SANT$NITRATE, na.rm = TRUE)
SE_nn <- sd(SANT$NITRATE, na.rm = TRUE)/sqrt(length((SANT$NITRATE)))
avg_nh3 <- mean(SANT$NH3, na.rm = TRUE)
SE_nh3 <- sd(SANT$NH3, na.rm = TRUE)/sqrt(length((SANT$NH3)))
avg_phos <- mean(SANT$PHOS, na.rm = TRUE)
SE_phos <- sd(SANT$PHOS, na.rm = TRUE)/sqrt(length((SANT$PHOS)))
avg_sio2 <- mean(SANT$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(SANT$SILICATE, na.rm = TRUE)/sqrt(length((SANT$SILICATE)))
avg_mn <- mean(SANT$Mn, na.rm = TRUE)
SE_mn <- sd(SANT$Mn, na.rm = TRUE)/sqrt(length((SANT$Mn)))
avg_ni <- mean(SANT$Ni, na.rm = TRUE)
SE_ni <- sd(SANT$Ni, na.rm = TRUE)/sqrt(length((SANT$Ni)))
avg_cd <- mean(SANT$Cd, na.rm = TRUE)
SE_cd <- sd(SANT$Cd, na.rm = TRUE)/sqrt(length((SANT$Cd)))
avg_co <- mean(SANT$Co, na.rm = TRUE)
SE_co <- sd(SANT$Co, na.rm = TRUE)/sqrt(length((SANT$Co)))
avg_cu <- mean(SANT$Cu, na.rm = TRUE)
SE_cu <- sd(SANT$Cu, na.rm = TRUE)/sqrt(length((SANT$Cu)))
avg_fe <- mean(SANT$Fe, na.rm = TRUE)
SE_fe <- sd(SANT$Fe, na.rm = TRUE)/sqrt(length((SANT$Fe)))
avg_zn <- mean(SANT$Zn, na.rm = TRUE)
SE_zn <- sd(SANT$Zn, na.rm = TRUE)/sqrt(length((SANT$Zn)))


TM_SANT<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_SANT<-as.data.frame(TM_SANT)
colnames(TM_SANT)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# ANTA ----------------------------------------------------------------------

write.table(ANTA,"ANTAtm.csv",col.names=T,row.names=F,sep=',')
obs <- 3086
avg_chla <- mean(ANTA$CHLA, na.rm = TRUE)
SE_chla <- sd(ANTA$CHLA, na.rm = TRUE)/sqrt(length((ANTA$CHLA)))
avg_depth <- mean(ANTA$DEPTH, na.rm = TRUE)
avg_temp <- mean(ANTA$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(ANTA$TEMPERATURE, na.rm = TRUE)/sqrt(length((ANTA$TEMPERATURE)))
avg_oxy <- mean(ANTA$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(ANTA$OXYGEN, na.rm = TRUE)/sqrt(length((ANTA$OXYGEN)))
avg_sal <- mean(ANTA$SALINITY, na.rm = TRUE)
SE_sal <- sd(ANTA$SALINITY, na.rm = TRUE)/sqrt(length((ANTA$SALINITY)))
avg_nn <- mean(ANTA$NITRATE, na.rm = TRUE)
SE_nn <- sd(ANTA$NITRATE, na.rm = TRUE)/sqrt(length((ANTA$NITRATE)))
avg_nh3 <- mean(ANTA$NH3, na.rm = TRUE)
SE_nh3 <- sd(ANTA$NH3, na.rm = TRUE)/sqrt(length((ANTA$NH3)))
avg_phos <- mean(ANTA$PHOS, na.rm = TRUE)
SE_phos <- sd(ANTA$PHOS, na.rm = TRUE)/sqrt(length((ANTA$PHOS)))
avg_sio2 <- mean(ANTA$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(ANTA$SILICATE, na.rm = TRUE)/sqrt(length((ANTA$SILICATE)))
avg_mn <- mean(ANTA$Mn, na.rm = TRUE)
SE_mn <- sd(ANTA$Mn, na.rm = TRUE)/sqrt(length((ANTA$Mn)))
avg_ni <- mean(ANTA$Ni, na.rm = TRUE)
SE_ni <- sd(ANTA$Ni, na.rm = TRUE)/sqrt(length((ANTA$Ni)))
avg_cd <- mean(ANTA$Cd, na.rm = TRUE)
SE_cd <- sd(ANTA$Cd, na.rm = TRUE)/sqrt(length((ANTA$Cd)))
avg_co <- mean(ANTA$Co, na.rm = TRUE)
SE_co <- sd(ANTA$Co, na.rm = TRUE)/sqrt(length((ANTA$Co)))
avg_cu <- mean(ANTA$Cu, na.rm = TRUE)
SE_cu <- sd(ANTA$Cu, na.rm = TRUE)/sqrt(length((ANTA$Cu)))
avg_fe <- mean(ANTA$Fe, na.rm = TRUE)
SE_fe <- sd(ANTA$Fe, na.rm = TRUE)/sqrt(length((ANTA$Fe)))
avg_zn <- mean(ANTA$Zn, na.rm = TRUE)
SE_zn <- sd(ANTA$Zn, na.rm = TRUE)/sqrt(length((ANTA$Zn)))


TM_ANTA<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_ANTA<-as.data.frame(TM_ANTA)
colnames(TM_ANTA)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# APLR ----------------------------------------------------------------------

write.table(APLR,"APLRtm.csv",col.names=T,row.names=F,sep=',')
obs <- 1623
avg_chla <- mean(APLR$CHLA, na.rm = TRUE)
SE_chla <- sd(APLR$CHLA, na.rm = TRUE)/sqrt(length((APLR$CHLA)))
avg_depth <- mean(APLR$DEPTH, na.rm = TRUE)
avg_temp <- mean(APLR$TEMPERATURE, na.rm = TRUE)
SE_temp <- sd(APLR$TEMPERATURE, na.rm = TRUE)/sqrt(length((APLR$TEMPERATURE)))
avg_oxy <- mean(APLR$OXYGEN, na.rm = TRUE)
SE_oxy <- sd(APLR$OXYGEN, na.rm = TRUE)/sqrt(length((APLR$OXYGEN)))
avg_sal <- mean(APLR$SALINITY, na.rm = TRUE)
SE_sal <- sd(APLR$SALINITY, na.rm = TRUE)/sqrt(length((APLR$SALINITY)))
avg_nn <- mean(APLR$NITRATE, na.rm = TRUE)
SE_nn <- sd(APLR$NITRATE, na.rm = TRUE)/sqrt(length((APLR$NITRATE)))
avg_nh3 <- mean(APLR$NH3, na.rm = TRUE)
SE_nh3 <- sd(APLR$NH3, na.rm = TRUE)/sqrt(length((APLR$NH3)))
avg_phos <- mean(APLR$PHOS, na.rm = TRUE)
SE_phos <- sd(APLR$PHOS, na.rm = TRUE)/sqrt(length((APLR$PHOS)))
avg_sio2 <- mean(APLR$SILICATE, na.rm = TRUE)
SE_sio2 <- sd(APLR$SILICATE, na.rm = TRUE)/sqrt(length((APLR$SILICATE)))
avg_mn <- mean(APLR$Mn, na.rm = TRUE)
SE_mn <- sd(APLR$Mn, na.rm = TRUE)/sqrt(length((APLR$Mn)))
avg_ni <- mean(APLR$Ni, na.rm = TRUE)
SE_ni <- sd(APLR$Ni, na.rm = TRUE)/sqrt(length((APLR$Ni)))
avg_cd <- mean(APLR$Cd, na.rm = TRUE)
SE_cd <- sd(APLR$Cd, na.rm = TRUE)/sqrt(length((APLR$Cd)))
avg_co <- mean(APLR$Co, na.rm = TRUE)
SE_co <- sd(APLR$Co, na.rm = TRUE)/sqrt(length((APLR$Co)))
avg_cu <- mean(APLR$Cu, na.rm = TRUE)
SE_cu <- sd(APLR$Cu, na.rm = TRUE)/sqrt(length((APLR$Cu)))
avg_fe <- mean(APLR$Fe, na.rm = TRUE)
SE_fe <- sd(APLR$Fe, na.rm = TRUE)/sqrt(length((APLR$Fe)))
avg_zn <- mean(APLR$Zn, na.rm = TRUE)
SE_zn <- sd(APLR$Zn, na.rm = TRUE)/sqrt(length((APLR$Zn)))


TM_APLR<-cbind(obs, avg_depth,avg_temp, SE_temp, avg_oxy,SE_oxy, avg_sal, SE_sal, avg_nn, SE_nn, avg_nh3,SE_nh3, avg_chla,SE_chla,
               avg_phos, SE_phos, avg_sio2, SE_sio2, avg_mn, SE_mn, avg_ni, SE_ni, avg_cd, SE_cd, avg_co, 
               SE_co, avg_cu, SE_cu, avg_fe, SE_fe, avg_zn, SE_zn)

TM_APLR<-as.data.frame(TM_APLR)
colnames(TM_APLR)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                     "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                     "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")


# Form the data into a table
LHprovs<-rbind(TM_BPLR, TM_ARCT, TM_SARC, TM_NADR, TM_GEST, TM_NASW, TM_NATR, TM_WTRA, 
               TM_SATL, TM_NASE, TM_MONS, TM_ISSG, TM_PSAE, TM_NPTE, TM_NPTW, TM_SPSG,
               TM_PNEC, TM_PEQD, TM_WARM, TM_ARCH, TM_SSTC, TM_SANT, TM_ANTA, TM_APLR)
LH_PROVS_CHART<-as.data.frame(LHprovs)
colnames(LH_PROVS_CHART)<-c("OBS","DEPTH","TEMP","TEMP_SE","OXYGEN","OXYGEN_SE","SALINITY","SALINITY_SE","NO3", 
                            "NO3_SE","NH3","NH3_SE","CHLA","CHLA_SE","PHOS","PHOS_SE",
                            "SIO2","SIO2_SE","Mn","Mn_SE","Ni","Ni_SE","Cd","Cd_SE","Co","Co_SE",
                            "Cu","Cu_SE","Fe","Fe_SE","Zn","Zn_SE")
write.table(LH_PROVS_CHART,"TraceMetalsProvChart.csv",col.names=T,row.names=F,sep=',')

# Hemisphere Tables -------------------------------------------------------

# SEA_BPLR<-as.data.frame(ifelse((BPLR$MONTH<=2),4,ifelse((BPLR$MONTH>2&BPLR$MONTH<=5),1,ifelse((BPLR$MONTH>5&BPLR$MONTH<=8),2,ifelse((BPLR$MONTH>8&BPLR$MONTH<=11),3,ifelse((BPLR$MONTH>=12),4,"NA"),)))))
# SEA_ARCT<-as.data.frame(ifelse((ARCT$MONTH<=2),4,ifelse((ARCT$MONTH>2&ARCT$MONTH<=5),1,ifelse((ARCT$MONTH>5&ARCT$MONTH<=8),2,ifelse((ARCT$MONTH>8&ARCT$MONTH<=11),3,ifelse((ARCT$MONTH>=12),4,"NA"))))))
# SEA_SARC<-as.data.frame(ifelse((SARC$MONTH<=2),4,ifelse((SARC$MONTH>2&SARC$MONTH<=5),1,ifelse((SARC$MONTH>5&SARC$MONTH<=8),2,ifelse((SARC$MONTH>8&SARC$MONTH<=11),3,ifelse((SARC$MONTH>=12),4,"NA"))))))
# SEA_PSAE<-as.data.frame(ifelse((PSAE$MONTH<=2),4,ifelse((PSAE$MONTH>2&PSAE$MONTH<=5),1,ifelse((PSAE$MONTH>5&PSAE$MONTH<=8),2,ifelse((PSAE$MONTH>8&PSAE$MONTH<=11),3,ifelse((PSAE$MONTH>=12),4,"NA"))))))
# SEA_NADR<-as.data.frame(ifelse((NADR$MONTH<=2),4,ifelse((NADR$MONTH>2&NADR$MONTH<=5),1,ifelse((NADR$MONTH>5&NADR$MONTH<=8),2,ifelse((NADR$MONTH>8&NADR$MONTH<=11),3,ifelse((NADR$MONTH>=12),4,"NA"))))))
# SEA_GEST<-as.data.frame(ifelse((GEST$MONTH<=2),4,ifelse((GEST$MONTH>2&GEST$MONTH<=5),1,ifelse((GEST$MONTH>5&GEST$MONTH<=8),2,ifelse((GEST$MONTH>8&GEST$MONTH<=11),3,ifelse((GEST$MONTH>=12),4,"NA"))))))
# SEA_NASW<-as.data.frame(ifelse((NASW$MONTH<=2),4,ifelse((NASW$MONTH>2&NASW$MONTH<=5),1,ifelse((NASW$MONTH>5&NASW$MONTH<=8),2,ifelse((NASW$MONTH>8&NASW$MONTH<=11),3,ifelse((NASW$MONTH>=12),4,"NA"))))))
# SEA_NASE<-as.data.frame(ifelse((NASE$MONTH<=2),4,ifelse((NASE$MONTH>2&NASE$MONTH<=5),1,ifelse((NASE$MONTH>5&NASE$MONTH<=8),2,ifelse((NASE$MONTH>8&NASE$MONTH<=11),3,ifelse((NASE$MONTH>=12),4,"NA"))))))
# SEA_NPTE<-as.data.frame(ifelse((NPTE$MONTH<=2),4,ifelse((NPTE$MONTH>2&NPTE$MONTH<=5),1,ifelse((NPTE$MONTH>5&NPTE$MONTH<=8),2,ifelse((NPTE$MONTH>8&NPTE$MONTH<=11),3,ifelse((NPTE$MONTH>=12),4,"NA"))))))
# SEA_NATR<-as.data.frame(ifelse((NATR$MONTH<=2),4,ifelse((NATR$MONTH>2&NATR$MONTH<=5),1,ifelse((NATR$MONTH>5&NATR$MONTH<=8),2,ifelse((NATR$MONTH>8&NATR$MONTH<=11),3,ifelse((NATR$MONTH>=12),4,"NA"))))))
# SEA_PNEC<-as.data.frame(ifelse((PNEC$MONTH<=2),4,ifelse((PNEC$MONTH>2&PNEC$MONTH<=5),1,ifelse((PNEC$MONTH>5&PNEC$MONTH<=8),2,ifelse((PNEC$MONTH>8&PNEC$MONTH<=11),3,ifelse((PNEC$MONTH>=12),4,"NA"))))))
# SEA_WTRA<-as.data.frame(ifelse((WTRA$MONTH<=2),4,ifelse((WTRA$MONTH>2&WTRA$MONTH<=5),1,ifelse((WTRA$MONTH>5&WTRA$MONTH<=8),2,ifelse((WTRA$MONTH>8&WTRA$MONTH<=11),3,ifelse((WTRA$MONTH>=12),4,"NA"))))))
# SEA_WARM<-as.data.frame(ifelse((WARM$MONTH<=2),4,ifelse((WARM$MONTH>2&WARM$MONTH<=5),1,ifelse((WARM$MONTH>5&WARM$MONTH<=8),2,ifelse((WARM$MONTH>8&WARM$MONTH<=11),3,ifelse((WARM$MONTH>=12),4,"NA"))))))
# 
# SEA_SPSG<-as.data.frame(ifelse((SPSG$MONTH<=2),2,ifelse((SPSG$MONTH>2&SPSG$MONTH<=5),3,ifelse((SPSG$MONTH>5&SPSG$MONTH<=8),4,ifelse((SPSG$MONTH>8&SPSG$MONTH<=11),1,ifelse((SPSG$MONTH>=12),2,"NA"))))))
# SEA_PEQD<-as.data.frame(ifelse((PEQD$MONTH<=2),2,ifelse((PEQD$MONTH>2&PEQD$MONTH<=5),3,ifelse((PEQD$MONTH>5&PEQD$MONTH<=8),4,ifelse((PEQD$MONTH>8&PEQD$MONTH<=11),1,ifelse((PEQD$MONTH>=12),2,"NA"))))))
# SEA_SATL<-as.data.frame(ifelse((SATL$MONTH<=2),2,ifelse((SATL$MONTH>2&SATL$MONTH<=5),3,ifelse((SATL$MONTH>5&SATL$MONTH<=8),4,ifelse((SATL$MONTH>8&SATL$MONTH<=11),1,ifelse((SATL$MONTH>=12),2,"NA"))))))
# SEA_MONS<-as.data.frame(ifelse((MONS$MONTH<=2),2,ifelse((MONS$MONTH>2&MONS$MONTH<=5),3,ifelse((MONS$MONTH>5&MONS$MONTH<=8),4,ifelse((MONS$MONTH>8&MONS$MONTH<=11),1,ifelse((MONS$MONTH>=12),2,"NA"))))))
# SEA_ISSG<-as.data.frame(ifelse((ISSG$MONTH<=2),2,ifelse((ISSG$MONTH>2&ISSG$MONTH<=5),3,ifelse((ISSG$MONTH>5&ISSG$MONTH<=8),4,ifelse((ISSG$MONTH>8&ISSG$MONTH<=11),1,ifelse((ISSG$MONTH>=12),2,"NA"))))))
# SEA_SSTC<-as.data.frame(ifelse((SSTC$MONTH<=2),2,ifelse((SSTC$MONTH>2&SSTC$MONTH<=5),3,ifelse((SSTC$MONTH>5&SSTC$MONTH<=8),4,ifelse((SSTC$MONTH>8&SSTC$MONTH<=11),1,ifelse((SSTC$MONTH>=12),2,"NA"))))))
# SEA_SANT<-as.data.frame(ifelse((SANT$MONTH<=2),2,ifelse((SANT$MONTH>2&SANT$MONTH<=5),3,ifelse((SANT$MONTH>5&SANT$MONTH<=8),4,ifelse((SANT$MONTH>8&SANT$MONTH<=11),1,ifelse((SANT$MONTH>=12),2,"NA"))))))
# SEA_ANTA<-as.data.frame(ifelse((ANTA$MONTH<=2),2,ifelse((ANTA$MONTH>2&ANTA$MONTH<=5),3,ifelse((ANTA$MONTH>5&ANTA$MONTH<=8),4,ifelse((ANTA$MONTH>8&ANTA$MONTH<=11),1,ifelse((ANTA$MONTH>=12),2,"NA"))))))
# SEA_APLR<-as.data.frame(ifelse((APLR$MONTH<=2),2,ifelse((APLR$MONTH>2&APLR$MONTH<=5),3,ifelse((APLR$MONTH>5&APLR$MONTH<=8),4,ifelse((APLR$MONTH>8&APLR$MONTH<=11),1,ifelse((APLR$MONTH>=12),2,"NA"))))))
# SEA_ARCH<-as.data.frame(ifelse((ARCH$MONTH<=2),2,ifelse((ARCH$MONTH>2&ARCH$MONTH<=5),3,ifelse((ARCH$MONTH>5&ARCH$MONTH<=8),4,ifelse((ARCH$MONTH>8&ARCH$MONTH<=11),1,ifelse((ARCH$MONTH>=12),2,"NA"))))))

# BPLR <- cbind(BPLR$LAT, BPLR$LON, BPLR$DAYNUM, BPLR$MONTH, BPLR$DEPTH, BPLR$TEMPERATURE, BPLR$SALINITY, BPLR$OXYGEN, BPLR$PHOS, BPLR$SILICATE, BPLR$NITRATE, BPLR$NH3, BPLR$DIC, BPLR$DOC, BPLR$Mn, BPLR$Fe, BPLR$Co, BPLR$Ni, BPLR$Cu, BPLR$Zn, BPLR$Cd, BPLR$LON_PROVS)
# colnames(BPLR)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# ARCT <- cbind(ARCT$LAT, ARCT$LON, ARCT$DAYNUM, ARCT$MONTH, ARCT$DEPTH, ARCT$TEMPERATURE, ARCT$SALINITY, ARCT$OXYGEN,ARCT$PHOS,ARCT$SILICATE, ARCT$NITRATE, ARCT$NH3, ARCT$DIC, ARCT$DOC, ARCT$Mn, ARCT$Fe, ARCT$Co, ARCT$Ni, ARCT$Cu, ARCT$Zn, ARCT$Cd, ARCT$LON_PROVS)
# colnames(ARCT)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# SARC <- cbind(SARC$LAT, SARC$LON, SARC$DAYNUM, SARC$MONTH, SARC$DEPTH, SARC$TEMPERATURE, SARC$SALINITY, SARC$OXYGEN,SARC$PHOS,SARC$SILICATE, SARC$NITRATE, SARC$NH3, SARC$DIC, SARC$DOC, SARC$Mn, SARC$Fe, SARC$Co, SARC$Ni, SARC$Cu, SARC$Zn, SARC$Cd, SARC$LON_PROVS)
# colnames(SARC)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# PSAE <- cbind(PSAE$LAT, PSAE$LON, PSAE$DAYNUM, PSAE$MONTH, PSAE$DEPTH, PSAE$TEMPERATURE, PSAE$SALINITY, PSAE$OXYGEN,PSAE$PHOS,PSAE$SILICATE, PSAE$NITRATE, PSAE$NH3, PSAE$DIC, PSAE$DOC, PSAE$Mn, PSAE$Fe, PSAE$Co, PSAE$Ni, PSAE$Cu, PSAE$Zn, PSAE$Cd, PSAE$LON_PROVS)
# colnames(PSAE)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# NADR <- cbind(NADR$LAT, NADR$LON, NADR$DAYNUM, NADR$MONTH, NADR$DEPTH, NADR$TEMPERATURE, NADR$SALINITY, NADR$OXYGEN,NADR$PHOS,NADR$SILICATE, NADR$NITRATE, NADR$NH3, NADR$DIC, NADR$DOC, NADR$Mn, NADR$Fe, NADR$Co, NADR$Ni, NADR$Cu, NADR$Zn, NADR$Cd, NADR$LON_PROVS)
# colnames(NADR)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# GEST <- cbind(GEST$LAT, GEST$LON, GEST$DAYNUM, GEST$MONTH, GEST$DEPTH, GEST$TEMPERATURE, GEST$SALINITY, GEST$OXYGEN,GEST$PHOS,GEST$SILICATE, GEST$NITRATE, GEST$NH3, GEST$DIC, GEST$DOC, GEST$Mn, GEST$Fe, GEST$Co, GEST$Ni, GEST$Cu, GEST$Zn, GEST$Cd, GEST$LON_PROVS)
# colnames(GEST)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# NASW <- cbind(NASW$LAT, NASW$LON, NASW$DAYNUM, NASW$MONTH, NASW$DEPTH, NASW$TEMPERATURE, NASW$SALINITY, NASW$OXYGEN,NASW$PHOS,NASW$SILICATE, NASW$NITRATE, NASW$NH3, NASW$DIC, NASW$DOC, NASW$Mn, NASW$Fe, NASW$Co, NASW$Ni, NASW$Cu, NASW$Zn, NASW$Cd, NASW$LON_PROVS)
# colnames(NASW)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# NASE <- cbind(NASE$LAT, NASE$LON, NASE$DAYNUM, NASE$MONTH, NASE$DEPTH, NASE$TEMPERATURE, NASE$SALINITY, NASE$OXYGEN,NASE$PHOS,NASE$SILICATE, NASE$NITRATE, NASE$NH3, NASE$DIC, NASE$DOC, NASE$Mn, NASE$Fe, NASE$Co, NASE$Ni, NASE$Cu, NASE$Zn, NASE$Cd, NASE$LON_PROVS)
# colnames(NASE)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# NPTE <- cbind(NPTE$LAT, NPTE$LON, NPTE$DAYNUM, NPTE$MONTH, NPTE$DEPTH, NPTE$TEMPERATURE, NPTE$SALINITY, NPTE$OXYGEN,NPTE$PHOS,NPTE$SILICATE, NPTE$NITRATE, NPTE$NH3, NPTE$DIC, NPTE$DOC, NPTE$Mn, NPTE$Fe, NPTE$Co, NPTE$Ni, NPTE$Cu, NPTE$Zn, NPTE$Cd, NPTE$LON_PROVS)
# colnames(NPTE)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# NATR <- cbind(NATR$LAT, NATR$LON, NATR$DAYNUM, NATR$MONTH, NATR$DEPTH, NATR$TEMPERATURE, NATR$SALINITY, NATR$OXYGEN,NATR$PHOS,NATR$SILICATE, NATR$NITRATE, NATR$NH3, NATR$DIC, NATR$DOC, NATR$Mn, NATR$Fe, NATR$Co, NATR$Ni, NATR$Cu, NATR$Zn, NATR$Cd, NATR$LON_PROVS)
# colnames(NATR)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# PNEC <- cbind(PNEC$LAT, PNEC$LON, PNEC$DAYNUM, PNEC$MONTH, PNEC$DEPTH, PNEC$TEMPERATURE, PNEC$SALINITY, PNEC$OXYGEN,PNEC$PHOS,PNEC$SILICATE, PNEC$NITRATE, PNEC$NH3, PNEC$DIC, PNEC$DOC, PNEC$Mn, PNEC$Fe, PNEC$Co, PNEC$Ni, PNEC$Cu, PNEC$Zn, PNEC$Cd, PNEC$LON_PROVS)
# colnames(PNEC)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# WTRA <- cbind(WTRA$LAT, WTRA$LON, WTRA$DAYNUM, WTRA$MONTH, WTRA$DEPTH, WTRA$TEMPERATURE, WTRA$SALINITY, WTRA$OXYGEN,WTRA$PHOS,WTRA$SILICATE, WTRA$NITRATE, WTRA$NH3, WTRA$DIC, WTRA$DOC, WTRA$Mn, WTRA$Fe, WTRA$Co, WTRA$Ni, WTRA$Cu, WTRA$Zn, WTRA$Cd, WTRA$LON_PROVS)
# colnames(WTRA)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# WARM <- cbind(WARM$LAT, WARM$LON, WARM$DAYNUM, WARM$MONTH, WARM$DEPTH, WARM$TEMPERATURE, WARM$SALINITY, WARM$OXYGEN,WARM$PHOS,WARM$SILICATE, WARM$NITRATE, WARM$NH3, WARM$DIC, WARM$DOC, WARM$Mn, WARM$Fe, WARM$Co, WARM$Ni, WARM$Cu, WARM$Zn, WARM$Cd, WARM$LON_PROVS)
# colnames(WARM)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# SPSG <- cbind(SPSG$LAT, SPSG$LON, SPSG$DAYNUM, SPSG$MONTH, SPSG$DEPTH, SPSG$TEMPERATURE, SPSG$SALINITY, SPSG$OXYGEN,SPSG$PHOS,SPSG$SILICATE, SPSG$NITRATE, SPSG$NH3, SPSG$DIC, SPSG$DOC, SPSG$Mn, SPSG$Fe, SPSG$Co, SPSG$Ni, SPSG$Cu, SPSG$Zn, SPSG$Cd, SPSG$LON_PROVS)
# colnames(SPSG)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# PEQD <- cbind(PEQD$LAT, PEQD$LON, PEQD$DAYNUM, PEQD$MONTH, PEQD$DEPTH, PEQD$TEMPERATURE, PEQD$SALINITY, PEQD$OXYGEN,PEQD$PHOS,PEQD$SILICATE, PEQD$NITRATE, PEQD$NH3, PEQD$DIC, PEQD$DOC, PEQD$Mn, PEQD$Fe, PEQD$Co, PEQD$Ni, PEQD$Cu, PEQD$Zn, PEQD$Cd, PEQD$LON_PROVS)
# colnames(PEQD)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# SATL <- cbind(SATL$LAT, SATL$LON, SATL$DAYNUM, SATL$MONTH, SATL$DEPTH, SATL$TEMPERATURE, SATL$SALINITY, SATL$OXYGEN,SATL$PHOS,SATL$SILICATE, SATL$NITRATE, SATL$NH3, SATL$DIC, SATL$DOC, SATL$Mn, SATL$Fe, SATL$Co, SATL$Ni, SATL$Cu, SATL$Zn, SATL$Cd, SATL$LON_PROVS)
# colnames(SATL)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# MONS <- cbind(MONS$LAT, MONS$LON, MONS$DAYNUM, MONS$MONTH, MONS$DEPTH, MONS$TEMPERATURE, MONS$SALINITY, MONS$OXYGEN,MONS$PHOS,MONS$SILICATE, MONS$NITRATE, MONS$NH3, MONS$DIC, MONS$DOC, MONS$Mn, MONS$Fe, MONS$Co, MONS$Ni, MONS$Cu, MONS$Zn, MONS$Cd, MONS$LON_PROVS)
# colnames(MONS)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# ISSG <- cbind(ISSG$LAT, ISSG$LON, ISSG$DAYNUM, ISSG$MONTH, ISSG$DEPTH, ISSG$TEMPERATURE, ISSG$SALINITY, ISSG$OXYGEN,ISSG$PHOS,ISSG$SILICATE, ISSG$NITRATE, ISSG$NH3, ISSG$DIC, ISSG$DOC, ISSG$Mn, ISSG$Fe, ISSG$Co, ISSG$Ni, ISSG$Cu, ISSG$Zn, ISSG$Cd, ISSG$LON_PROVS)
# colnames(ISSG)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# SSTC <- cbind(SSTC$LAT, SSTC$LON, SSTC$DAYNUM, SSTC$MONTH, SSTC$DEPTH, SSTC$TEMPERATURE, SSTC$SALINITY, SSTC$OXYGEN,SSTC$PHOS,SSTC$SILICATE, SSTC$NITRATE, SSTC$NH3, SSTC$DIC, SSTC$DOC, SSTC$Mn, SSTC$Fe, SSTC$Co, SSTC$Ni, SSTC$Cu, SSTC$Zn, SSTC$Cd, SSTC$LON_PROVS)
# colnames(SSTC)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# SANT <- cbind(SANT$LAT, SANT$LON, SANT$DAYNUM, SANT$MONTH, SANT$DEPTH, SANT$TEMPERATURE, SANT$SALINITY, SANT$OXYGEN,SANT$PHOS,SANT$SILICATE, SANT$NITRATE, SANT$NH3, SANT$DIC, SANT$DOC, SANT$Mn, SANT$Fe, SANT$Co, SANT$Ni, SANT$Cu, SANT$Zn, SANT$Cd, SANT$LON_PROVS)
# colnames(SANT)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# ANTA <- cbind(ANTA$LAT, ANTA$LON, ANTA$DAYNUM, ANTA$MONTH, ANTA$DEPTH, ANTA$TEMPERATURE, ANTA$SALINITY, ANTA$OXYGEN,ANTA$PHOS,ANTA$SILICATE, ANTA$NITRATE, ANTA$NH3, ANTA$DIC, ANTA$DOC, ANTA$Mn, ANTA$Fe, ANTA$Co, ANTA$Ni, ANTA$Cu, ANTA$Zn, ANTA$Cd, ANTA$LON_PROVS)
# colnames(ANTA)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# APLR <- cbind(APLR$LAT, APLR$LON, APLR$DAYNUM, APLR$MONTH, APLR$DEPTH, APLR$TEMPERATURE, APLR$SALINITY, APLR$OXYGEN,APLR$PHOS,APLR$SILICATE, APLR$NITRATE, APLR$NH3, APLR$DIC, APLR$DOC, APLR$Mn, APLR$Fe, APLR$Co, APLR$Ni, APLR$Cu, APLR$Zn, APLR$Cd, APLR$LON_PROVS)
# colnames(APLR)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# ARCH <- cbind(ARCH$LAT, ARCH$LON, ARCH$DAYNUM, ARCH$MONTH, ARCH$DEPTH, ARCH$TEMPERATURE, ARCH$SALINITY, ARCH$OXYGEN,ARCH$PHOS,ARCH$SILICATE, ARCH$NITRATE, ARCH$NH3, ARCH$DIC, ARCH$DOC, ARCH$Mn, ARCH$Fe, ARCH$Co, ARCH$Ni, ARCH$Cu, ARCH$Zn, ARCH$Cd, ARCH$LON_PROVS)
# colnames(ARCH)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# 
# TARAKSJC<-rbind(BPLR, ARCT, SARC, PSAE, NADR, GEST, NASW, NASE, NPTE, NATR, PNEC, WTRA, WARM, SPSG, PEQD, SATL, MONS, ISSG, SSTC, SANT, ANTA, APLR, ARCH)
# TraceMetalTable<-as.data.frame(TARAKSJC)
# colnames(TraceMetalTable)<-c("LAT","LON","DAYNUM","MONTH","DEPTH","TEMPERATURE","SALINITY", "OXYGEN", "PHOS", "SILICATE", "NITRATE","NH3","DIC","DOC","Mn","Fe","Co","Ni","Cu","Zn","Cd","LON_PROVS")
# write.table(TraceMetalTable,"TRACEMETALTABLE_SEASON.csv",col.names=T,row.names=F,sep=',')
