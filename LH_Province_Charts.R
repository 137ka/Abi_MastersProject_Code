#Making a large table of Longhurst Provinces 
#clear workspace
rm(list= ls(all= TRUE))

#Assigning the directory of the script
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/")


# Read merged pigment data file
pigtab<-read.table(file="MAINFILE.csv",sep=",",h=T)
finalData<-subset(pigtab,!(is.na(pigtab["LONG_PROVS"]) | is.na(pigtab["CHLA"] | is.na(pigtab["LAT"]))))


# Assigining variables to each of the Longhurst Provinces
BPLR <- subset(finalData, finalData$LONG_PROVS==1)
ARCT <- subset(finalData, finalData$LONG_PROVS==2)
SARC <- subset(finalData, finalData$LONG_PROVS==3)
NADR <- subset(finalData, finalData$LONG_PROVS==4)
GEST <- subset(finalData, finalData$LONG_PROVS==5)
NASW <- subset(finalData, finalData$LONG_PROVS==6)
NATR <- subset(finalData, finalData$LONG_PROVS==7)
WTRA <- subset(finalData, finalData$LONG_PROVS==8)
SATL <- subset(finalData, finalData$LONG_PROVS==10)
NASE <- subset(finalData, finalData$LONG_PROVS==18)
MONS <- subset(finalData, finalData$LONG_PROVS==30)
ISSG <- subset(finalData, finalData$LONG_PROVS==31)
PSAE <- subset(finalData, finalData$LONG_PROVS==51)
NPTE <- subset(finalData, finalData$LONG_PROVS==55)
NPTW <- subset(finalData, finalData$LONG_PROVS==56)
SPSG <- subset(finalData, finalData$LONG_PROVS==59)
NPTE1 <- subset(finalData, finalData$LONG_PROVS==60)
PNEC <- subset(finalData, finalData$LONG_PROVS==61)
PEQD <- subset(finalData, finalData$LONG_PROVS==62)
WARM <- subset(finalData, finalData$LONG_PROVS==63)
ARCH <- subset(finalData, finalData$LONG_PROVS==64)
PNEC1 <- subset(finalData, finalData$LONG_PROVS==67)
SSTC <- subset(finalData, finalData$LONG_PROVS==80)
SANT <- subset(finalData, finalData$LONG_PROVS==81)
ANTA <- subset(finalData, finalData$LONG_PROVS==82)
APLR <- subset(finalData, finalData$LONG_PROVS==83)



# Calculating Values for provinces in number order: BPLR

write.table(BPLR,"BPLR.csv",col.names=T,row.names=F,sep=',')
obs <- sum(!is.na(BPLR))
avg_chla <- mean(BPLR$CHLA, na.rm = TRUE)
SE_chla <- sd(BPLR$CHLA, na.rm = TRUE)/sqrt(length((BPLR$CHLA)))
avg_depth <- mean(BPLR$DEPTH, na.rm = TRUE)
avg_temp <- mean(BPLR$TEMP, na.rm = TRUE)
SE_Temp <- sd(BPLR$TEMP, na.rm = TRUE)/sqrt(length((BPLR$TEMP)))
avg_sal <- mean(BPLR$SALINITY, na.rm = TRUE)
SE_sal <- sd(BPLR$SALINITY, na.rm = TRUE)/sqrt(length((BPLR$SALINITY)))
avg_no3 <- mean(BPLR$NO3, na.rm = TRUE)
SE_no3 <- sd(BPLR$NO3, na.rm = TRUE)/sqrt(length((BPLR$NO3)))
avg_chlb <- mean((BPLR$CHLB), na.rm = TRUE)
SE_chlb <- sd(((BPLR$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$CHLB)))
avg_chlc <- mean((BPLR$CHLC), na.rm = TRUE)
SE_chlc <- sd(((BPLR$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$CHLB)))
avg_Bcarot <- mean((BPLR$BCARTOENE), na.rm = TRUE)
SE_Bcarot <- sd(((BPLR$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$BCARTOENE)))
avg_butfuc <- mean((BPLR$X19_BUT), na.rm = TRUE)
SE_butfuc <- sd(((BPLR$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$X19_BUT)))
avg_hexfuc <- mean((BPLR$X19_HEX), na.rm = TRUE)
SE_hexfuc <- sd(((BPLR$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$X19_HEX)))
avg_allo <- mean((BPLR$ALLO), na.rm = TRUE)
SE_allo <- sd(((BPLR$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$ALLO)))
avg_diad <- mean((BPLR$DIADINO), na.rm = TRUE)
SE_diad <- sd(((BPLR$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$DIADINO)))
avg_fuco <- mean((BPLR$FUCO), na.rm = TRUE)
SE_fuco <- sd(((BPLR$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$FUCO)))
avg_perid <- mean((BPLR$PERID), na.rm = TRUE)
SE_perid <- sd(((BPLR$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$PERID)))
avg_zea <- mean((BPLR$ZEA), na.rm = TRUE)
SE_zea <- sd(((BPLR$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$ZEA)))
avg_dvchla <- mean((BPLR$DVCHLA), na.rm = TRUE)
SE_dvchla <- sd(((BPLR$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$DVCHLA)))
avg_chlc3 <- mean((BPLR$CHLC3), na.rm = TRUE)
SE_chlc3 <- sd(((BPLR$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$CHLC3)))
avg_lut <- mean((BPLR$LUT), na.rm = TRUE)
SE_lut <- sd(((BPLR$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$LUT)))
avg_violx <- mean((BPLR$VIOLX), na.rm = TRUE)
SE_violx <- sd(((BPLR$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$VIOLX)))
avg_pras <- mean((BPLR$PRAS), na.rm = TRUE)
SE_pras <- sd(((BPLR$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((BPLR$PRAS)))

LP_BPLR<-cbind(obs, avg_depth, avg_temp,SE_Temp, avg_sal,SE_sal, avg_no3,SE_no3, avg_chla,SE_chla,
               avg_chlb, SE_chlb, avg_chlc, SE_chlc, avg_Bcarot, SE_Bcarot, avg_butfuc, SE_butfuc,
               avg_hexfuc, SE_hexfuc, avg_allo, SE_allo, avg_diad, SE_diad, avg_fuco, SE_fuco, avg_perid, 
               SE_perid, avg_zea, SE_zea, avg_dvchla, SE_dvchla,avg_violx, SE_violx, avg_pras, SE_pras)
LP_BPLR<-as.data.frame(LP_BPLR)
colnames(LP_BPLR)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                            "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                            "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                            "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                            "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                            "VIOLX","VIOLX_SE","PRAS",
                            "PRAS_SE")


# ARCT --------------------------------------------------------------------


# Calculating Values for provinces in number order: ARCT
write.table(ARCT,"ARCT.csv",col.names=T,row.names=F,sep=',')
obs2 <- sum(!is.na(ARCT))
avg_chla2 <- mean(ARCT$CHLA, na.rm = TRUE)
SE_chla2 <- sd(ARCT$CHLA, na.rm = TRUE)/sqrt(length((ARCT$CHLA)))
avg_depth2 <- mean(ARCT$DEPTH, na.rm = TRUE)
avg_temp2 <- mean(ARCT$TEMP, na.rm = TRUE)
SE_Temp2 <- sd(ARCT$TEMP, na.rm = TRUE)/sqrt(length((ARCT$TEMP)))
avg_sal2 <- mean(ARCT$SALINITY, na.rm = TRUE)
SE_sal2 <- sd(ARCT$SALINITY, na.rm = TRUE)/sqrt(length((ARCT$SALINITY)))
avg_no32 <- mean(ARCT$NO3, na.rm = TRUE)
SE_no32 <- sd(ARCT$NO3, na.rm = TRUE)/sqrt(length((ARCT$NO3)))
avg_chlb2 <- mean((ARCT$CHLB), na.rm = TRUE)
SE_chlb2 <- sd(((ARCT$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$CHLB)))
avg_chlc2 <- mean((ARCT$CHLC), na.rm = TRUE)
SE_chlc2 <- sd(((ARCT$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$CHLB)))
avg_Bcarot2 <- mean((ARCT$BCARTOENE), na.rm = TRUE)
SE_Bcarot2 <- sd(((ARCT$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$BCARTOENE)))
avg_butfuc2 <- mean((ARCT$X19_BUT), na.rm = TRUE)
SE_butfuc2 <- sd(((ARCT$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$X19_BUT)))
avg_hexfuc2 <- mean((ARCT$X19_HEX), na.rm = TRUE)
SE_hexfuc2 <- sd(((ARCT$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$X19_HEX)))
avg_allo2 <- mean((ARCT$ALLO), na.rm = TRUE)
SE_allo2 <- sd(((ARCT$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$ALLO)))
avg_diad2 <- mean((ARCT$DIADINO), na.rm = TRUE)
SE_diad2 <- sd(((ARCT$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$DIADINO)))
avg_fuco2 <- mean((ARCT$FUCO), na.rm = TRUE)
SE_fuco2 <- sd(((ARCT$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$FUCO)))
avg_perid2 <- mean((ARCT$PERID), na.rm = TRUE)
SE_perid2 <- sd(((ARCT$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$PERID)))
avg_zea2 <- mean((ARCT$ZEA), na.rm = TRUE)
SE_zea2 <- sd(((ARCT$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$ZEA)))
avg_dvchla2 <- mean((ARCT$DVCHLA), na.rm = TRUE)
SE_dvchla2 <- sd(((ARCT$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$DVCHLA)))
avg_chlc32 <- mean((ARCT$CHLC3), na.rm = TRUE)
SE_chlc32 <- sd(((ARCT$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$CHLC3)))
avg_lut2 <- mean((ARCT$LUT), na.rm = TRUE)
SE_lut2 <- sd(((ARCT$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$LUT)))
avg_violx2 <- mean((ARCT$VIOLX), na.rm = TRUE)
SE_violx2 <- sd(((ARCT$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$VIOLX)))
avg_pras2 <- mean((ARCT$PRAS), na.rm = TRUE)
SE_pras2 <- sd(((ARCT$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((ARCT$PRAS)))

LP_ARCT<-cbind(obs2, avg_depth2, avg_temp2,SE_Temp2, avg_sal2,SE_sal2, avg_no32,SE_no32, avg_chla2,SE_chla2,
               avg_chlb2, SE_chlb2, avg_chlc2, SE_chlc2, avg_Bcarot2, SE_Bcarot2, avg_butfuc2, SE_butfuc2,
               avg_hexfuc2, SE_hexfuc2, avg_allo2, SE_allo2, avg_diad2, SE_diad2, avg_fuco2, SE_fuco2, avg_perid2, 
               SE_perid2, avg_zea2, SE_zea2, avg_dvchla2, SE_dvchla2,avg_violx2, SE_violx2, avg_pras2, SE_pras2)
LP_ARCT<-as.data.frame(LP_ARCT)
colnames(LP_ARCT)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# SARC --------------------------------------------------------------------


# Calculating Values for provinces in number order: SARC
write.table(SARC,"SARC.csv",col.names=T,row.names=F,sep=',')
obs3 <- sum(!is.na(SARC))
avg_chla3 <- mean(SARC$CHLA, na.rm = TRUE)
SE_chla3 <- sd(SARC$CHLA, na.rm = TRUE)/sqrt(length((SARC$CHLA)))
avg_depth3 <- mean(SARC$DEPTH, na.rm = TRUE)
avg_temp3 <- mean(SARC$TEMP, na.rm = TRUE)
SE_Temp3 <- sd(SARC$TEMP, na.rm = TRUE)/sqrt(length((SARC$TEMP)))
avg_sal3 <- mean(SARC$SALINITY, na.rm = TRUE)
SE_sal3 <- sd(SARC$SALINITY, na.rm = TRUE)/sqrt(length((SARC$SALINITY)))
avg_no33 <- mean(SARC$NO3, na.rm = TRUE)
SE_no33 <- sd(SARC$NO3, na.rm = TRUE)/sqrt(length((SARC$NO3)))
avg_chlb3 <- mean((SARC$CHLB), na.rm = TRUE)
SE_chlb3 <- sd(((SARC$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$CHLB)))
avg_chlc3 <- mean((SARC$CHLC), na.rm = TRUE)
SE_chlc3 <- sd(((SARC$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$CHLB)))
avg_Bcarot3 <- mean((SARC$BCARTOENE), na.rm = TRUE)
SE_Bcarot3 <- sd(((SARC$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$BCARTOENE)))
avg_butfuc3 <- mean((SARC$X19_BUT), na.rm = TRUE)
SE_butfuc3 <- sd(((SARC$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$X19_BUT)))
avg_hexfuc3 <- mean((SARC$X19_HEX), na.rm = TRUE)
SE_hexfuc3 <- sd(((SARC$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$X19_HEX)))
avg_allo3 <- mean((SARC$ALLO), na.rm = TRUE)
SE_allo3 <- sd(((SARC$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$ALLO)))
avg_diad3 <- mean((SARC$DIADINO), na.rm = TRUE)
SE_diad3 <- sd(((SARC$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$DIADINO)))
avg_fuco3 <- mean((SARC$FUCO), na.rm = TRUE)
SE_fuco3 <- sd(((SARC$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$FUCO)))
avg_perid3 <- mean((SARC$PERID), na.rm = TRUE)
SE_perid3 <- sd(((SARC$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$PERID)))
avg_zea3 <- mean((SARC$ZEA), na.rm = TRUE)
SE_zea3 <- sd(((SARC$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$ZEA)))
avg_dvchla3 <- mean((SARC$DVCHLA), na.rm = TRUE)
SE_dvchla3 <- sd(((SARC$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$DVCHLA)))
avg_chlc33 <- mean((SARC$CHLC3), na.rm = TRUE)
SE_chlc33 <- sd(((SARC$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$CHLC3)))
avg_lut3 <- mean((SARC$LUT), na.rm = TRUE)
SE_lut3 <- sd(((SARC$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$LUT)))
avg_violx3 <- mean((SARC$VIOLX), na.rm = TRUE)
SE_violx3 <- sd(((SARC$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$VIOLX)))
avg_pras3 <- mean((SARC$PRAS), na.rm = TRUE)
SE_pras3 <- sd(((SARC$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((SARC$PRAS)))

LP_SARC<-cbind(obs3, avg_depth3, avg_temp3,SE_Temp3, avg_sal3,SE_sal3, avg_no33,SE_no33, avg_chla3,SE_chla3,
               avg_chlb3, SE_chlb3, avg_chlc3, SE_chlc3, avg_Bcarot3, SE_Bcarot3, avg_butfuc3, SE_butfuc3,
               avg_hexfuc3, SE_hexfuc3, avg_allo3, SE_allo3, avg_diad3, SE_diad3, avg_fuco3, SE_fuco3, avg_perid3, 
               SE_perid3, avg_zea3, SE_zea3, avg_dvchla3, SE_dvchla3,avg_violx3, SE_violx3, avg_pras3, SE_pras3)
LP_SARC<-as.data.frame(LP_SARC)
colnames(LP_SARC)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# NADR --------------------------------------------------------------------

write.table(NADR,"NADR.csv",col.names=T,row.names=F,sep=',')
obs4 <- sum(!is.na(NADR))
avg_chla4 <- mean(NADR$CHLA, na.rm = TRUE)
SE_chla4 <- sd(NADR$CHLA, na.rm = TRUE)/sqrt(length((NADR$CHLA)))
avg_depth4 <- mean(NADR$DEPTH, na.rm = TRUE)
avg_temp4 <- mean(NADR$TEMP, na.rm = TRUE)
SE_Temp4 <- sd(NADR$TEMP, na.rm = TRUE)/sqrt(length((NADR$TEMP)))
avg_sal4 <- mean(NADR$SALINITY, na.rm = TRUE)
SE_sal4 <- sd(NADR$SALINITY, na.rm = TRUE)/sqrt(length((NADR$SALINITY)))
avg_no34 <- mean(NADR$NO3, na.rm = TRUE)
SE_no34 <- sd(NADR$NO3, na.rm = TRUE)/sqrt(length((NADR$NO3)))
avg_chlb4 <- mean((NADR$CHLB), na.rm = TRUE)
SE_chlb4 <- sd(((NADR$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$CHLB)))
avg_chlc4 <- mean((NADR$CHLC), na.rm = TRUE)
SE_chlc4 <- sd(((NADR$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$CHLB)))
avg_Bcarot4 <- mean((NADR$BCARTOENE), na.rm = TRUE)
SE_Bcarot4 <- sd(((NADR$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$BCARTOENE)))
avg_butfuc4 <- mean((NADR$X19_BUT), na.rm = TRUE)
SE_butfuc4 <- sd(((NADR$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$X19_BUT)))
avg_hexfuc4 <- mean((NADR$X19_HEX), na.rm = TRUE)
SE_hexfuc4 <- sd(((NADR$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$X19_HEX)))
avg_allo4 <- mean((NADR$ALLO), na.rm = TRUE)
SE_allo4 <- sd(((NADR$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$ALLO)))
avg_diad4 <- mean((NADR$DIADINO), na.rm = TRUE)
SE_diad4 <- sd(((NADR$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$DIADINO)))
avg_fuco4 <- mean((NADR$FUCO), na.rm = TRUE)
SE_fuco4 <- sd(((NADR$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$FUCO)))
avg_perid4 <- mean((NADR$PERID), na.rm = TRUE)
SE_perid4 <- sd(((NADR$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$PERID)))
avg_zea4 <- mean((NADR$ZEA), na.rm = TRUE)
SE_zea4 <- sd(((NADR$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$ZEA)))
avg_dvchla4 <- mean((NADR$DVCHLA), na.rm = TRUE)
SE_dvchla4 <- sd(((NADR$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$DVCHLA)))
avg_chlc34 <- mean((NADR$CHLC3), na.rm = TRUE)
SE_chlc34 <- sd(((NADR$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$CHLC3)))
avg_lut4 <- mean((NADR$LUT), na.rm = TRUE)
SE_lut4 <- sd(((NADR$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$LUT)))
avg_violx4 <- mean((NADR$VIOLX), na.rm = TRUE)
SE_violx4 <- sd(((NADR$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$VIOLX)))
avg_pras4 <- mean((NADR$PRAS), na.rm = TRUE)
SE_pras4 <- sd(((NADR$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((NADR$PRAS)))

LP_NADR<-cbind(obs4, avg_depth4, avg_temp4,SE_Temp4, avg_sal4,SE_sal4, avg_no34,SE_no34, avg_chla4,SE_chla4,
               avg_chlb4, SE_chlb4, avg_chlc4, SE_chlc4, avg_Bcarot4, SE_Bcarot4, avg_butfuc4, SE_butfuc4,
               avg_hexfuc4, SE_hexfuc4, avg_allo4, SE_allo4, avg_diad4, SE_diad4, avg_fuco4, SE_fuco4, avg_perid4, 
               SE_perid4, avg_zea4, SE_zea4, avg_dvchla4, SE_dvchla4,avg_violx4, SE_violx4, avg_pras4, SE_pras4)
LP_NADR<-as.data.frame(LP_NADR)
colnames(LP_NADR)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# GEST --------------------------------------------------------------------

write.table(GEST,"GEST.csv",col.names=T,row.names=F,sep=',')
obs5 <- sum(!is.na(GEST))
avg_chla5 <- mean(GEST$CHLA, na.rm = TRUE)
SE_chla5 <- sd(GEST$CHLA, na.rm = TRUE)/sqrt(length((GEST$CHLA)))
avg_depth5 <- mean(GEST$DEPTH, na.rm = TRUE)
avg_temp5 <- mean(GEST$TEMP, na.rm = TRUE)
SE_Temp5 <- sd(GEST$TEMP, na.rm = TRUE)/sqrt(length((GEST$TEMP)))
avg_sal5 <- mean(GEST$SALINITY, na.rm = TRUE)
SE_sal5 <- sd(GEST$SALINITY, na.rm = TRUE)/sqrt(length((GEST$SALINITY)))
avg_no35 <- mean(GEST$NO3, na.rm = TRUE)
SE_no35 <- sd(GEST$NO3, na.rm = TRUE)/sqrt(length((GEST$NO3)))
avg_chlb5 <- mean((GEST$CHLB), na.rm = TRUE)
SE_chlb5 <- sd(((GEST$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$CHLB)))
avg_chlc5 <- mean((GEST$CHLC), na.rm = TRUE)
SE_chlc5 <- sd(((GEST$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$CHLB)))
avg_Bcarot5 <- mean((GEST$BCARTOENE), na.rm = TRUE)
SE_Bcarot5 <- sd(((GEST$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$BCARTOENE)))
avg_butfuc5 <- mean((GEST$X19_BUT), na.rm = TRUE)
SE_butfuc5 <- sd(((GEST$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$X19_BUT)))
avg_hexfuc5 <- mean((GEST$X19_HEX), na.rm = TRUE)
SE_hexfuc5 <- sd(((GEST$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$X19_HEX)))
avg_allo5 <- mean((GEST$ALLO), na.rm = TRUE)
SE_allo5 <- sd(((GEST$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$ALLO)))
avg_diad5 <- mean((GEST$DIADINO), na.rm = TRUE)
SE_diad5 <- sd(((GEST$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$DIADINO)))
avg_fuco5 <- mean((GEST$FUCO), na.rm = TRUE)
SE_fuco5 <- sd(((GEST$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$FUCO)))
avg_perid5 <- mean((GEST$PERID), na.rm = TRUE)
SE_perid5 <- sd(((GEST$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$PERID)))
avg_zea5 <- mean((GEST$ZEA), na.rm = TRUE)
SE_zea5 <- sd(((GEST$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$ZEA)))
avg_dvchla5 <- mean((GEST$DVCHLA), na.rm = TRUE)
SE_dvchla5 <- sd(((GEST$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$DVCHLA)))
avg_chlc35 <- mean((GEST$CHLC3), na.rm = TRUE)
SE_chlc35 <- sd(((GEST$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$CHLC3)))
avg_lut5 <- mean((GEST$LUT), na.rm = TRUE)
SE_lut5 <- sd(((GEST$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$LUT)))
avg_violx5 <- mean((GEST$VIOLX), na.rm = TRUE)
SE_violx5 <- sd(((GEST$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$VIOLX)))
avg_pras5 <- mean((GEST$PRAS), na.rm = TRUE)
SE_pras5 <- sd(((GEST$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((GEST$PRAS)))

LP_GEST<-cbind(obs5, avg_depth5, avg_temp5,SE_Temp5, avg_sal5,SE_sal5, avg_no35,SE_no35, avg_chla5,SE_chla5,
               avg_chlb5, SE_chlb5, avg_chlc5, SE_chlc5, avg_Bcarot5, SE_Bcarot5, avg_butfuc5, SE_butfuc5,
               avg_hexfuc5, SE_hexfuc5, avg_allo5, SE_allo5, avg_diad5, SE_diad5, avg_fuco5, SE_fuco5, avg_perid5, 
               SE_perid5, avg_zea5, SE_zea5, avg_dvchla5, SE_dvchla5,avg_violx5, SE_violx5, avg_pras5, SE_pras5)
LP_GEST<-as.data.frame(LP_GEST)
colnames(LP_GEST)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# NASW --------------------------------------------------------------------

write.table(NASW,"NASW.csv",col.names=T,row.names=F,sep=',')
obs6 <- sum(!is.na(NASW))
avg_chla6 <- mean(NASW$CHLA, na.rm = TRUE)
SE_chla6 <- sd(NASW$CHLA, na.rm = TRUE)/sqrt(length((NASW$CHLA)))
avg_depth6 <- mean(NASW$DEPTH, na.rm = TRUE)
avg_temp6 <- mean(NASW$TEMP, na.rm = TRUE)
SE_Temp6 <- sd(NASW$TEMP, na.rm = TRUE)/sqrt(length((NASW$TEMP)))
avg_sal6 <- mean(NASW$SALINITY, na.rm = TRUE)
SE_sal6 <- sd(NASW$SALINITY, na.rm = TRUE)/sqrt(length((NASW$SALINITY)))
avg_no36 <- mean(NASW$NO3, na.rm = TRUE)
SE_no36 <- sd(NASW$NO3, na.rm = TRUE)/sqrt(length((NASW$NO3)))
avg_chlb6 <- mean((NASW$CHLB), na.rm = TRUE)
SE_chlb6 <- sd(((NASW$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$CHLB)))
avg_chlc6 <- mean((NASW$CHLC), na.rm = TRUE)
SE_chlc6 <- sd(((NASW$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$CHLB)))
avg_Bcarot6 <- mean((NASW$BCARTOENE), na.rm = TRUE)
SE_Bcarot6 <- sd(((NASW$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$BCARTOENE)))
avg_butfuc6 <- mean((NASW$X19_BUT), na.rm = TRUE)
SE_butfuc6 <- sd(((NASW$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$X19_BUT)))
avg_hexfuc6 <- mean((NASW$X19_HEX), na.rm = TRUE)
SE_hexfuc6 <- sd(((NASW$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$X19_HEX)))
avg_allo6 <- mean((NASW$ALLO), na.rm = TRUE)
SE_allo6 <- sd(((NASW$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$ALLO)))
avg_diad6 <- mean((NASW$DIADINO), na.rm = TRUE)
SE_diad6 <- sd(((NASW$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$DIADINO)))
avg_fuco6 <- mean((NASW$FUCO), na.rm = TRUE)
SE_fuco6 <- sd(((NASW$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$FUCO)))
avg_perid6 <- mean((NASW$PERID), na.rm = TRUE)
SE_perid6 <- sd(((NASW$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$PERID)))
avg_zea6 <- mean((NASW$ZEA), na.rm = TRUE)
SE_zea6 <- sd(((NASW$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$ZEA)))
avg_dvchla6 <- mean((NASW$DVCHLA), na.rm = TRUE)
SE_dvchla6 <- sd(((NASW$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$DVCHLA)))
avg_chlc36 <- mean((NASW$CHLC3), na.rm = TRUE)
SE_chlc36 <- sd(((NASW$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$CHLC3)))
avg_lut6 <- mean((NASW$LUT), na.rm = TRUE)
SE_lut6 <- sd(((NASW$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$LUT)))
avg_violx6 <- mean((NASW$VIOLX), na.rm = TRUE)
SE_violx6 <- sd(((NASW$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$VIOLX)))
avg_pras6 <- mean((NASW$PRAS), na.rm = TRUE)
SE_pras6 <- sd(((NASW$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((NASW$PRAS)))

LP_NASW<-cbind(obs6, avg_depth6, avg_temp6,SE_Temp6, avg_sal6,SE_sal6, avg_no36,SE_no36, avg_chla6,SE_chla6,
               avg_chlb6, SE_chlb6, avg_chlc6, SE_chlc6, avg_Bcarot6, SE_Bcarot6, avg_butfuc6, SE_butfuc6,
               avg_hexfuc6, SE_hexfuc6, avg_allo6, SE_allo6, avg_diad6, SE_diad6, avg_fuco6, SE_fuco6, avg_perid6, 
               SE_perid6, avg_zea6, SE_zea6, avg_dvchla6, SE_dvchla6,avg_violx6, SE_violx6, avg_pras6, SE_pras6)
LP_NASW<-as.data.frame(LP_NASW)
colnames(LP_NASW)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# NATR --------------------------------------------------------------------

write.table(NATR,"NATR.csv",col.names=T,row.names=F,sep=',')
obs7 <- sum(!is.na(NATR))
avg_chla7 <- mean(NATR$CHLA, na.rm = TRUE)
SE_chla7 <- sd(NATR$CHLA, na.rm = TRUE)/sqrt(length((NATR$CHLA)))
avg_depth7 <- mean(NATR$DEPTH, na.rm = TRUE)
avg_temp7 <- mean(NATR$TEMP, na.rm = TRUE)
SE_Temp7 <- sd(NATR$TEMP, na.rm = TRUE)/sqrt(length((NATR$TEMP)))
avg_sal7 <- mean(NATR$SALINITY, na.rm = TRUE)
SE_sal7 <- sd(NATR$SALINITY, na.rm = TRUE)/sqrt(length((NATR$SALINITY)))
avg_no37 <- mean(NATR$NO3, na.rm = TRUE)
SE_no37 <- sd(NATR$NO3, na.rm = TRUE)/sqrt(length((NATR$NO3)))
avg_chlb7 <- mean((NATR$CHLB), na.rm = TRUE)
SE_chlb7 <- sd(((NATR$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$CHLB)))
avg_chlc7 <- mean((NATR$CHLC), na.rm = TRUE)
SE_chlc7 <- sd(((NATR$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$CHLB)))
avg_Bcarot7 <- mean((NATR$BCARTOENE), na.rm = TRUE)
SE_Bcarot7 <- sd(((NATR$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$BCARTOENE)))
avg_butfuc7 <- mean((NATR$X19_BUT), na.rm = TRUE)
SE_butfuc7 <- sd(((NATR$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$X19_BUT)))
avg_hexfuc7 <- mean((NATR$X19_HEX), na.rm = TRUE)
SE_hexfuc7 <- sd(((NATR$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$X19_HEX)))
avg_allo7 <- mean((NATR$ALLO), na.rm = TRUE)
SE_allo7 <- sd(((NATR$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$ALLO)))
avg_diad7 <- mean((NATR$DIADINO), na.rm = TRUE)
SE_diad7 <- sd(((NATR$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$DIADINO)))
avg_fuco7 <- mean((NATR$FUCO), na.rm = TRUE)
SE_fuco7 <- sd(((NATR$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$FUCO)))
avg_perid7 <- mean((NATR$PERID), na.rm = TRUE)
SE_perid7 <- sd(((NATR$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$PERID)))
avg_zea7 <- mean((NATR$ZEA), na.rm = TRUE)
SE_zea7 <- sd(((NATR$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$ZEA)))
avg_dvchla7 <- mean((NATR$DVCHLA), na.rm = TRUE)
SE_dvchla7 <- sd(((NATR$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$DVCHLA)))
avg_chlc37 <- mean((NATR$CHLC3), na.rm = TRUE)
SE_chlc37 <- sd(((NATR$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$CHLC3)))
avg_lut7 <- mean((NATR$LUT), na.rm = TRUE)
SE_lut7 <- sd(((NATR$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$LUT)))
avg_violx7 <- mean((NATR$VIOLX), na.rm = TRUE)
SE_violx7 <- sd(((NATR$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$VIOLX)))
avg_pras7 <- mean((NATR$PRAS), na.rm = TRUE)
SE_pras7 <- sd(((NATR$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((NATR$PRAS)))

LP_NATR<-cbind(obs7, avg_depth7, avg_temp7,SE_Temp7, avg_sal7,SE_sal7, avg_no37,SE_no37, avg_chla7,SE_chla7,
               avg_chlb7, SE_chlb7, avg_chlc7, SE_chlc7, avg_Bcarot7, SE_Bcarot7, avg_butfuc7, SE_butfuc7,
               avg_hexfuc7, SE_hexfuc7, avg_allo7, SE_allo7, avg_diad7, SE_diad7, avg_fuco7, SE_fuco7, avg_perid7, 
               SE_perid7, avg_zea7, SE_zea7, avg_dvchla7, SE_dvchla7,avg_violx7, SE_violx7, avg_pras7, SE_pras7)
LP_NATR<-as.data.frame(LP_NATR)
colnames(LP_NATR)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# WTRA --------------------------------------------------------------------

write.table(WTRA,"WTRA.csv",col.names=T,row.names=F,sep=',')
obs8 <- sum(!is.na(WTRA))
avg_chla8 <- mean(WTRA$CHLA, na.rm = TRUE)
SE_chla8 <- sd(WTRA$CHLA, na.rm = TRUE)/sqrt(length((WTRA$CHLA)))
avg_depth8 <- mean(WTRA$DEPTH, na.rm = TRUE)
avg_temp8 <- mean(WTRA$TEMP, na.rm = TRUE)
SE_Temp8 <- sd(WTRA$TEMP, na.rm = TRUE)/sqrt(length((WTRA$TEMP)))
avg_sal8 <- mean(WTRA$SALINITY, na.rm = TRUE)
SE_sal8 <- sd(WTRA$SALINITY, na.rm = TRUE)/sqrt(length((WTRA$SALINITY)))
avg_no38 <- mean(WTRA$NO3, na.rm = TRUE)
SE_no38 <- sd(WTRA$NO3, na.rm = TRUE)/sqrt(length((WTRA$NO3)))
avg_chlb8 <- mean((WTRA$CHLB), na.rm = TRUE)
SE_chlb8 <- sd(((WTRA$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$CHLB)))
avg_chlc8 <- mean((WTRA$CHLC), na.rm = TRUE)
SE_chlc8 <- sd(((WTRA$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$CHLB)))
avg_Bcarot8 <- mean((WTRA$BCARTOENE), na.rm = TRUE)
SE_Bcarot8 <- sd(((WTRA$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$BCARTOENE)))
avg_butfuc8 <- mean((WTRA$X19_BUT), na.rm = TRUE)
SE_butfuc8 <- sd(((WTRA$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$X19_BUT)))
avg_hexfuc8 <- mean((WTRA$X19_HEX), na.rm = TRUE)
SE_hexfuc8 <- sd(((WTRA$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$X19_HEX)))
avg_allo8 <- mean((WTRA$ALLO), na.rm = TRUE)
SE_allo8 <- sd(((WTRA$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$ALLO)))
avg_diad8 <- mean((WTRA$DIADINO), na.rm = TRUE)
SE_diad8 <- sd(((WTRA$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$DIADINO)))
avg_fuco8 <- mean((WTRA$FUCO), na.rm = TRUE)
SE_fuco8 <- sd(((WTRA$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$FUCO)))
avg_perid8 <- mean((WTRA$PERID), na.rm = TRUE)
SE_perid8 <- sd(((WTRA$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$PERID)))
avg_zea8 <- mean((WTRA$ZEA), na.rm = TRUE)
SE_zea8 <- sd(((WTRA$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$ZEA)))
avg_dvchla8 <- mean((WTRA$DVCHLA), na.rm = TRUE)
SE_dvchla8 <- sd(((WTRA$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$DVCHLA)))
avg_chlc38 <- mean((WTRA$CHLC3), na.rm = TRUE)
SE_chlc38 <- sd(((WTRA$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$CHLC3)))
avg_lut8 <- mean((WTRA$LUT), na.rm = TRUE)
SE_lut8 <- sd(((WTRA$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$LUT)))
avg_violx8 <- mean((WTRA$VIOLX), na.rm = TRUE)
SE_violx8 <- sd(((WTRA$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$VIOLX)))
avg_pras8 <- mean((WTRA$PRAS), na.rm = TRUE)
SE_pras8 <- sd(((WTRA$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((WTRA$PRAS)))

LP_WTRA<-cbind(obs8, avg_depth8, avg_temp8,SE_Temp8, avg_sal8,SE_sal8, avg_no38,SE_no38, avg_chla8,SE_chla8,
               avg_chlb8, SE_chlb8, avg_chlc8, SE_chlc8, avg_Bcarot8, SE_Bcarot8, avg_butfuc8, SE_butfuc8,
               avg_hexfuc8, SE_hexfuc8, avg_allo8, SE_allo8, avg_diad8, SE_diad8, avg_fuco8, SE_fuco8, avg_perid8, 
               SE_perid8, avg_zea8, SE_zea8, avg_dvchla8, SE_dvchla8,avg_violx8, SE_violx8, avg_pras8, SE_pras8)
LP_WTRA<-as.data.frame(LP_WTRA)
colnames(LP_WTRA)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# SATL --------------------------------------------------------------------

write.table(SATL,"SATL.csv",col.names=T,row.names=F,sep=',')
obs10 <- sum(!is.na(SATL))
avg_chla10 <- mean(SATL$CHLA, na.rm = TRUE)
SE_chla10 <- sd(SATL$CHLA, na.rm = TRUE)/sqrt(length((SATL$CHLA)))
avg_depth10 <- mean(SATL$DEPTH, na.rm = TRUE)
avg_temp10 <- mean(SATL$TEMP, na.rm = TRUE)
SE_Temp10 <- sd(SATL$TEMP, na.rm = TRUE)/sqrt(length((SATL$TEMP)))
avg_sal10 <- mean(SATL$SALINITY, na.rm = TRUE)
SE_sal10 <- sd(SATL$SALINITY, na.rm = TRUE)/sqrt(length((SATL$SALINITY)))
avg_no310 <- mean(SATL$NO3, na.rm = TRUE)
SE_no310 <- sd(SATL$NO3, na.rm = TRUE)/sqrt(length((SATL$NO3)))
avg_chlb10 <- mean((SATL$CHLB), na.rm = TRUE)
SE_chlb10 <- sd(((SATL$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$CHLB)))
avg_chlc10 <- mean((SATL$CHLC), na.rm = TRUE)
SE_chlc10 <- sd(((SATL$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$CHLB)))
avg_Bcarot10 <- mean((SATL$BCARTOENE), na.rm = TRUE)
SE_Bcarot10 <- sd(((SATL$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$BCARTOENE)))
avg_butfuc10 <- mean((SATL$X19_BUT), na.rm = TRUE)
SE_butfuc10 <- sd(((SATL$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$X19_BUT)))
avg_hexfuc10 <- mean((SATL$X19_HEX), na.rm = TRUE)
SE_hexfuc10 <- sd(((SATL$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$X19_HEX)))
avg_allo10 <- mean((SATL$ALLO), na.rm = TRUE)
SE_allo10 <- sd(((SATL$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$ALLO)))
avg_diad10 <- mean((SATL$DIADINO), na.rm = TRUE)
SE_diad10 <- sd(((SATL$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$DIADINO)))
avg_fuco10 <- mean((SATL$FUCO), na.rm = TRUE)
SE_fuco10 <- sd(((SATL$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$FUCO)))
avg_perid10 <- mean((SATL$PERID), na.rm = TRUE)
SE_perid10 <- sd(((SATL$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$PERID)))
avg_zea10 <- mean((SATL$ZEA), na.rm = TRUE)
SE_zea10 <- sd(((SATL$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$ZEA)))
avg_dvchla10 <- mean((SATL$DVCHLA), na.rm = TRUE)
SE_dvchla10 <- sd(((SATL$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$DVCHLA)))
avg_chlc310 <- mean((SATL$CHLC3), na.rm = TRUE)
SE_chlc310 <- sd(((SATL$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$CHLC3)))
avg_lut10 <- mean((SATL$LUT), na.rm = TRUE)
SE_lut10 <- sd(((SATL$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$LUT)))
avg_violx10 <- mean((SATL$VIOLX), na.rm = TRUE)
SE_violx10 <- sd(((SATL$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$VIOLX)))
avg_pras10 <- mean((SATL$PRAS), na.rm = TRUE)
SE_pras10 <- sd(((SATL$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((SATL$PRAS)))

LP_SATL<-cbind(obs10, avg_depth10, avg_temp10,SE_Temp10, avg_sal10,SE_sal10, avg_no310,SE_no310, avg_chla10,SE_chla10,
               avg_chlb10, SE_chlb10, avg_chlc10, SE_chlc10, avg_Bcarot10, SE_Bcarot10, avg_butfuc10, SE_butfuc10,
               avg_hexfuc10, SE_hexfuc10, avg_allo10, SE_allo10, avg_diad10, SE_diad10, avg_fuco10, SE_fuco10, avg_perid10, 
               SE_perid10, avg_zea10, SE_zea10, avg_dvchla10, SE_dvchla10,avg_violx10, SE_violx10, avg_pras10, SE_pras10)
LP_SATL<-as.data.frame(LP_SATL)
colnames(LP_SATL)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# NASE --------------------------------------------------------------------

write.table(NASE,"NASE.csv",col.names=T,row.names=F,sep=',')
obs18 <- sum(!is.na(NASE))
avg_chla18 <- mean(NASE$CHLA, na.rm = TRUE)
SE_chla18 <- sd(NASE$CHLA, na.rm = TRUE)/sqrt(length((NASE$CHLA)))
avg_depth18 <- mean(NASE$DEPTH, na.rm = TRUE)
avg_temp18 <- mean(NASE$TEMP, na.rm = TRUE)
SE_Temp18 <- sd(NASE$TEMP, na.rm = TRUE)/sqrt(length((NASE$TEMP)))
avg_sal18 <- mean(NASE$SALINITY, na.rm = TRUE)
SE_sal18 <- sd(NASE$SALINITY, na.rm = TRUE)/sqrt(length((NASE$SALINITY)))
avg_no318 <- mean(NASE$NO3, na.rm = TRUE)
SE_no318 <- sd(NASE$NO3, na.rm = TRUE)/sqrt(length((NASE$NO3)))
avg_chlb18 <- mean((NASE$CHLB), na.rm = TRUE)
SE_chlb18 <- sd(((NASE$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$CHLB)))
avg_chlc18 <- mean((NASE$CHLC), na.rm = TRUE)
SE_chlc18 <- sd(((NASE$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$CHLB)))
avg_Bcarot18 <- mean((NASE$BCARTOENE), na.rm = TRUE)
SE_Bcarot18 <- sd(((NASE$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$BCARTOENE)))
avg_butfuc18 <- mean((NASE$X19_BUT), na.rm = TRUE)
SE_butfuc18 <- sd(((NASE$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$X19_BUT)))
avg_hexfuc18 <- mean((NASE$X19_HEX), na.rm = TRUE)
SE_hexfuc18 <- sd(((NASE$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$X19_HEX)))
avg_allo18 <- mean((NASE$ALLO), na.rm = TRUE)
SE_allo18 <- sd(((NASE$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$ALLO)))
avg_diad18 <- mean((NASE$DIADINO), na.rm = TRUE)
SE_diad18 <- sd(((NASE$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$DIADINO)))
avg_fuco18 <- mean((NASE$FUCO), na.rm = TRUE)
SE_fuco18 <- sd(((NASE$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$FUCO)))
avg_perid18 <- mean((NASE$PERID), na.rm = TRUE)
SE_perid18 <- sd(((NASE$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$PERID)))
avg_zea18 <- mean((NASE$ZEA), na.rm = TRUE)
SE_zea18 <- sd(((NASE$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$ZEA)))
avg_dvchla18 <- mean((NASE$DVCHLA), na.rm = TRUE)
SE_dvchla18 <- sd(((NASE$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$DVCHLA)))
avg_chlc318 <- mean((NASE$CHLC3), na.rm = TRUE)
SE_chlc318 <- sd(((NASE$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$CHLC3)))
avg_lut18 <- mean((NASE$LUT), na.rm = TRUE)
SE_lut18 <- sd(((NASE$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$LUT)))
avg_violx18 <- mean((NASE$VIOLX), na.rm = TRUE)
SE_violx18 <- sd(((NASE$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$VIOLX)))
avg_pras18 <- mean((NASE$PRAS), na.rm = TRUE)
SE_pras18 <- sd(((NASE$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((NASE$PRAS)))

LP_NASE<-cbind(obs18, avg_depth18, avg_temp18,SE_Temp18, avg_sal18,SE_sal18, avg_no318,SE_no318, avg_chla18,SE_chla18,
               avg_chlb18, SE_chlb18, avg_chlc18, SE_chlc18, avg_Bcarot18, SE_Bcarot18, avg_butfuc18, SE_butfuc18,
               avg_hexfuc18, SE_hexfuc18, avg_allo18, SE_allo18, avg_diad18, SE_diad18, avg_fuco18, SE_fuco18, avg_perid18, 
               SE_perid18, avg_zea18, SE_zea18, avg_dvchla18, SE_dvchla18,avg_violx18, SE_violx18, avg_pras18, SE_pras18)
LP_NASE<-as.data.frame(LP_NASE)
colnames(LP_NASE)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# MONS --------------------------------------------------------------------

write.table(MONS,"MONS.csv",col.names=T,row.names=F,sep=',')
obs22 <- sum(!is.na(MONS))
avg_chla22 <- mean(MONS$CHLA, na.rm = TRUE)
SE_chla22 <- sd(MONS$CHLA, na.rm = TRUE)/sqrt(length((MONS$CHLA)))
avg_depth22 <- mean(MONS$DEPTH, na.rm = TRUE)
avg_temp22 <- mean(MONS$TEMP, na.rm = TRUE)
SE_Temp22 <- sd(MONS$TEMP, na.rm = TRUE)/sqrt(length((MONS$TEMP)))
avg_sal22 <- mean(MONS$SALINITY, na.rm = TRUE)
SE_sal22 <- sd(MONS$SALINITY, na.rm = TRUE)/sqrt(length((MONS$SALINITY)))
avg_no322 <- mean(MONS$NO3, na.rm = TRUE)
SE_no322 <- sd(MONS$NO3, na.rm = TRUE)/sqrt(length((MONS$NO3)))
avg_chlb22 <- mean((MONS$CHLB), na.rm = TRUE)
SE_chlb22 <- sd(((MONS$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$CHLB)))
avg_chlc22 <- mean((MONS$CHLC), na.rm = TRUE)
SE_chlc22 <- sd(((MONS$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$CHLB)))
avg_Bcarot22 <- mean((MONS$BCARTOENE), na.rm = TRUE)
SE_Bcarot22 <- sd(((MONS$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$BCARTOENE)))
avg_butfuc22 <- mean((MONS$X19_BUT), na.rm = TRUE)
SE_butfuc22 <- sd(((MONS$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$X19_BUT)))
avg_hexfuc22 <- mean((MONS$X19_HEX), na.rm = TRUE)
SE_hexfuc22 <- sd(((MONS$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$X19_HEX)))
avg_allo22 <- mean((MONS$ALLO), na.rm = TRUE)
SE_allo22 <- sd(((MONS$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$ALLO)))
avg_diad22 <- mean((MONS$DIADINO), na.rm = TRUE)
SE_diad22 <- sd(((MONS$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$DIADINO)))
avg_fuco22 <- mean((MONS$FUCO), na.rm = TRUE)
SE_fuco22 <- sd(((MONS$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$FUCO)))
avg_perid22 <- mean((MONS$PERID), na.rm = TRUE)
SE_perid22 <- sd(((MONS$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$PERID)))
avg_zea22 <- mean((MONS$ZEA), na.rm = TRUE)
SE_zea22 <- sd(((MONS$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$ZEA)))
avg_dvchla22 <- mean((MONS$DVCHLA), na.rm = TRUE)
SE_dvchla22 <- sd(((MONS$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$DVCHLA)))
avg_chlc322 <- mean((MONS$CHLC3), na.rm = TRUE)
SE_chlc322 <- sd(((MONS$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$CHLC3)))
avg_lut22 <- mean((MONS$LUT), na.rm = TRUE)
SE_lut22 <- sd(((MONS$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$LUT)))
avg_violx22 <- mean((MONS$VIOLX), na.rm = TRUE)
SE_violx22 <- sd(((MONS$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$VIOLX)))
avg_pras22 <- mean((MONS$PRAS), na.rm = TRUE)
SE_pras22 <- sd(((MONS$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((MONS$PRAS)))

LP_MONS<-cbind(obs22, avg_depth22, avg_temp22,SE_Temp22, avg_sal22,SE_sal22, avg_no322,SE_no322, avg_chla22,SE_chla22,
               avg_chlb22, SE_chlb22, avg_chlc22, SE_chlc22, avg_Bcarot22, SE_Bcarot22, avg_butfuc22, SE_butfuc22,
               avg_hexfuc22, SE_hexfuc22, avg_allo22, SE_allo22, avg_diad22, SE_diad22, avg_fuco22, SE_fuco22, avg_perid22, 
               SE_perid22, avg_zea22, SE_zea22, avg_dvchla22, SE_dvchla22,avg_violx22, SE_violx22, avg_pras22, SE_pras22)
LP_MONS<-as.data.frame(LP_MONS)
colnames(LP_MONS)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# ISSG --------------------------------------------------------------------

write.table(ISSG,"ISSG.csv",col.names=T,row.names=F,sep=',')
obs31 <- sum(!is.na(ISSG))
avg_chla31 <- mean(ISSG$CHLA, na.rm = TRUE)
SE_chla31 <- sd(ISSG$CHLA, na.rm = TRUE)/sqrt(length((ISSG$CHLA)))
avg_depth31 <- mean(ISSG$DEPTH, na.rm = TRUE)
avg_temp31 <- mean(ISSG$TEMP, na.rm = TRUE)
SE_Temp31 <- sd(ISSG$TEMP, na.rm = TRUE)/sqrt(length((ISSG$TEMP)))
avg_sal31 <- mean(ISSG$SALINITY, na.rm = TRUE)
SE_sal31 <- sd(ISSG$SALINITY, na.rm = TRUE)/sqrt(length((ISSG$SALINITY)))
avg_no331 <- mean(ISSG$NO3, na.rm = TRUE)
SE_no331 <- sd(ISSG$NO3, na.rm = TRUE)/sqrt(length((ISSG$NO3)))
avg_chlb31 <- mean((ISSG$CHLB), na.rm = TRUE)
SE_chlb31 <- sd(((ISSG$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$CHLB)))
avg_chlc31 <- mean((ISSG$CHLC), na.rm = TRUE)
SE_chlc31 <- sd(((ISSG$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$CHLB)))
avg_Bcarot31 <- mean((ISSG$BCARTOENE), na.rm = TRUE)
SE_Bcarot31 <- sd(((ISSG$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$BCARTOENE)))
avg_butfuc31 <- mean((ISSG$X19_BUT), na.rm = TRUE)
SE_butfuc31 <- sd(((ISSG$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$X19_BUT)))
avg_hexfuc31 <- mean((ISSG$X19_HEX), na.rm = TRUE)
SE_hexfuc31 <- sd(((ISSG$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$X19_HEX)))
avg_allo31 <- mean((ISSG$ALLO), na.rm = TRUE)
SE_allo31 <- sd(((ISSG$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$ALLO)))
avg_diad31 <- mean((ISSG$DIADINO), na.rm = TRUE)
SE_diad31 <- sd(((ISSG$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$DIADINO)))
avg_fuco31 <- mean((ISSG$FUCO), na.rm = TRUE)
SE_fuco31 <- sd(((ISSG$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$FUCO)))
avg_perid31 <- mean((ISSG$PERID), na.rm = TRUE)
SE_perid31 <- sd(((ISSG$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$PERID)))
avg_zea31 <- mean((ISSG$ZEA), na.rm = TRUE)
SE_zea31 <- sd(((ISSG$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$ZEA)))
avg_dvchla31 <- mean((ISSG$DVCHLA), na.rm = TRUE)
SE_dvchla31 <- sd(((ISSG$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$DVCHLA)))
avg_chlc331 <- mean((ISSG$CHLC3), na.rm = TRUE)
SE_chlc331 <- sd(((ISSG$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$CHLC3)))
avg_lut31 <- mean((ISSG$LUT), na.rm = TRUE)
SE_lut31 <- sd(((ISSG$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$LUT)))
avg_violx31 <- mean((ISSG$VIOLX), na.rm = TRUE)
SE_violx31 <- sd(((ISSG$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$VIOLX)))
avg_pras31 <- mean((ISSG$PRAS), na.rm = TRUE)
SE_pras31 <- sd(((ISSG$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((ISSG$PRAS)))

LP_ISSG<-cbind(obs31, avg_depth31, avg_temp31,SE_Temp31, avg_sal31,SE_sal31, avg_no331,SE_no331, avg_chla31,SE_chla31,
               avg_chlb31, SE_chlb31, avg_chlc31, SE_chlc31, avg_Bcarot31, SE_Bcarot31, avg_butfuc31, SE_butfuc31,
               avg_hexfuc31, SE_hexfuc31, avg_allo31, SE_allo31, avg_diad31, SE_diad31, avg_fuco31, SE_fuco31, avg_perid31, 
               SE_perid31, avg_zea31, SE_zea31, avg_dvchla31, SE_dvchla31,avg_violx31, SE_violx31, avg_pras31, SE_pras31)
LP_ISSG<-as.data.frame(LP_ISSG)
colnames(LP_ISSG)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# PSAE --------------------------------------------------------------------

write.table(PSAE,"PSAE.csv",col.names=T,row.names=F,sep=',')
obs51 <- sum(!is.na(PSAE))
avg_chla51 <- mean(PSAE$CHLA, na.rm = TRUE)
SE_chla51 <- sd(PSAE$CHLA, na.rm = TRUE)/sqrt(length((PSAE$CHLA)))
avg_depth51 <- mean(PSAE$DEPTH, na.rm = TRUE)
avg_temp51 <- mean(PSAE$TEMP, na.rm = TRUE)
SE_Temp51 <- sd(PSAE$TEMP, na.rm = TRUE)/sqrt(length((PSAE$TEMP)))
avg_sal51 <- mean(PSAE$SALINITY, na.rm = TRUE)
SE_sal51 <- sd(PSAE$SALINITY, na.rm = TRUE)/sqrt(length((PSAE$SALINITY)))
avg_no351 <- mean(PSAE$NO3, na.rm = TRUE)
SE_no351 <- sd(PSAE$NO3, na.rm = TRUE)/sqrt(length((PSAE$NO3)))
avg_chlb51 <- mean((PSAE$CHLB), na.rm = TRUE)
SE_chlb51 <- sd(((PSAE$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$CHLB)))
avg_chlc51 <- mean((PSAE$CHLC), na.rm = TRUE)
SE_chlc51 <- sd(((PSAE$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$CHLB)))
avg_Bcarot51 <- mean((PSAE$BCARTOENE), na.rm = TRUE)
SE_Bcarot51 <- sd(((PSAE$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$BCARTOENE)))
avg_butfuc51 <- mean((PSAE$X19_BUT), na.rm = TRUE)
SE_butfuc51 <- sd(((PSAE$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$X19_BUT)))
avg_hexfuc51 <- mean((PSAE$X19_HEX), na.rm = TRUE)
SE_hexfuc51 <- sd(((PSAE$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$X19_HEX)))
avg_allo51 <- mean((PSAE$ALLO), na.rm = TRUE)
SE_allo51 <- sd(((PSAE$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$ALLO)))
avg_diad51 <- mean((PSAE$DIADINO), na.rm = TRUE)
SE_diad51 <- sd(((PSAE$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$DIADINO)))
avg_fuco51 <- mean((PSAE$FUCO), na.rm = TRUE)
SE_fuco51 <- sd(((PSAE$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$FUCO)))
avg_perid51 <- mean((PSAE$PERID), na.rm = TRUE)
SE_perid51 <- sd(((PSAE$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$PERID)))
avg_zea51 <- mean((PSAE$ZEA), na.rm = TRUE)
SE_zea51 <- sd(((PSAE$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$ZEA)))
avg_dvchla51 <- mean((PSAE$DVCHLA), na.rm = TRUE)
SE_dvchla51 <- sd(((PSAE$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$DVCHLA)))
avg_chlc513 <- mean((PSAE$CHLC3), na.rm = TRUE)
SE_chlc513 <- sd(((PSAE$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$CHLC3)))
avg_lut51 <- mean((PSAE$LUT), na.rm = TRUE)
SE_lut51 <- sd(((PSAE$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$LUT)))
avg_violx51 <- mean((PSAE$VIOLX), na.rm = TRUE)
SE_violx51 <- sd(((PSAE$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$VIOLX)))
avg_pras51 <- mean((PSAE$PRAS), na.rm = TRUE)
SE_pras51 <- sd(((PSAE$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((PSAE$PRAS)))

LP_PSAE<-cbind(obs51, avg_depth51, avg_temp51,SE_Temp51, avg_sal51,SE_sal51, avg_no351,SE_no351, avg_chla51,SE_chla51,
               avg_chlb51, SE_chlb51, avg_chlc51, SE_chlc51, avg_Bcarot51, SE_Bcarot51, avg_butfuc51, SE_butfuc51,
               avg_hexfuc51, SE_hexfuc51, avg_allo51, SE_allo51, avg_diad51, SE_diad51, avg_fuco51, SE_fuco51, avg_perid51, 
               SE_perid51, avg_zea51, SE_zea51, avg_dvchla51, SE_dvchla51,avg_violx51, SE_violx51, avg_pras51, SE_pras51)
LP_PSAE<-as.data.frame(LP_PSAE)
colnames(LP_PSAE)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")

# NPTE --------------------------------------------------------------------

write.table(NPTE,"NPTE.csv",col.names=T,row.names=F,sep=',')
NPTE = rbind(NPTE, NPTE1)
obs55 <- sum(!is.na(NPTE))
avg_chla55 <- mean(NPTE$CHLA, na.rm = TRUE)
SE_chla55 <- sd(NPTE$CHLA, na.rm = TRUE)/sqrt(length((NPTE$CHLA)))
avg_depth55 <- mean(NPTE$DEPTH, na.rm = TRUE)
avg_temp55 <- mean(NPTE$TEMP, na.rm = TRUE)
SE_Temp55 <- sd(NPTE$TEMP, na.rm = TRUE)/sqrt(length((NPTE$TEMP)))
avg_sal55 <- mean(NPTE$SALINITY, na.rm = TRUE)
SE_sal55 <- sd(NPTE$SALINITY, na.rm = TRUE)/sqrt(length((NPTE$SALINITY)))
avg_no355 <- mean(NPTE$NO3, na.rm = TRUE)
SE_no355 <- sd(NPTE$NO3, na.rm = TRUE)/sqrt(length((NPTE$NO3)))
avg_chlb55 <- mean((NPTE$CHLB), na.rm = TRUE)
SE_chlb55 <- sd(((NPTE$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$CHLB)))
avg_chlc55 <- mean((NPTE$CHLC), na.rm = TRUE)
SE_chlc55 <- sd(((NPTE$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$CHLB)))
avg_Bcarot55 <- mean((NPTE$BCARTOENE), na.rm = TRUE)
SE_Bcarot55 <- sd(((NPTE$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$BCARTOENE)))
avg_butfuc55 <- mean((NPTE$X19_BUT), na.rm = TRUE)
SE_butfuc55 <- sd(((NPTE$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$X19_BUT)))
avg_hexfuc55 <- mean((NPTE$X19_HEX), na.rm = TRUE)
SE_hexfuc55 <- sd(((NPTE$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$X19_HEX)))
avg_allo55 <- mean((NPTE$ALLO), na.rm = TRUE)
SE_allo55 <- sd(((NPTE$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$ALLO)))
avg_diad55 <- mean((NPTE$DIADINO), na.rm = TRUE)
SE_diad55 <- sd(((NPTE$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$DIADINO)))
avg_fuco55 <- mean((NPTE$FUCO), na.rm = TRUE)
SE_fuco55 <- sd(((NPTE$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$FUCO)))
avg_perid55 <- mean((NPTE$PERID), na.rm = TRUE)
SE_perid55 <- sd(((NPTE$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$PERID)))
avg_zea55 <- mean((NPTE$ZEA), na.rm = TRUE)
SE_zea55 <- sd(((NPTE$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$ZEA)))
avg_dvchla55 <- mean((NPTE$DVCHLA), na.rm = TRUE)
SE_dvchla55 <- sd(((NPTE$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$DVCHLA)))
avg_chlc553 <- mean((NPTE$CHLC3), na.rm = TRUE)
SE_chlc553 <- sd(((NPTE$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$CHLC3)))
avg_lut55 <- mean((NPTE$LUT), na.rm = TRUE)
SE_lut55 <- sd(((NPTE$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$LUT)))
avg_violx55 <- mean((NPTE$VIOLX), na.rm = TRUE)
SE_violx55 <- sd(((NPTE$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$VIOLX)))
avg_pras55 <- mean((NPTE$PRAS), na.rm = TRUE)
SE_pras55 <- sd(((NPTE$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((NPTE$PRAS)))

LP_NPTE<-cbind(obs55, avg_depth55, avg_temp55,SE_Temp55, avg_sal55,SE_sal55, avg_no355,SE_no355, avg_chla55,SE_chla55,
               avg_chlb55, SE_chlb55, avg_chlc55, SE_chlc55, avg_Bcarot55, SE_Bcarot55, avg_butfuc55, SE_butfuc55,
               avg_hexfuc55, SE_hexfuc55, avg_allo55, SE_allo55, avg_diad55, SE_diad55, avg_fuco55, SE_fuco55, avg_perid55, 
               SE_perid55, avg_zea55, SE_zea55, avg_dvchla55, SE_dvchla55,avg_violx55, SE_violx55, avg_pras55, SE_pras55)
LP_NPTE<-as.data.frame(LP_NPTE)
colnames(LP_NPTE)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# NPTW --------------------------------------------------------------------

write.table(NPTW,"NPTW.csv",col.names=T,row.names=F,sep=',')
obs56 <- sum(!is.na(NPTW))
avg_chla56 <- mean(NPTW$CHLA, na.rm = TRUE)
SE_chla56 <- sd(NPTW$CHLA, na.rm = TRUE)/sqrt(length((NPTW$CHLA)))
avg_depth56 <- mean(NPTW$DEPTH, na.rm = TRUE)
avg_temp56 <- mean(NPTW$TEMP, na.rm = TRUE)
SE_Temp56 <- sd(NPTW$TEMP, na.rm = TRUE)/sqrt(length((NPTW$TEMP)))
avg_sal56 <- mean(NPTW$SALINITY, na.rm = TRUE)
SE_sal56 <- sd(NPTW$SALINITY, na.rm = TRUE)/sqrt(length((NPTW$SALINITY)))
avg_no356 <- mean(NPTW$NO3, na.rm = TRUE)
SE_no356 <- sd(NPTW$NO3, na.rm = TRUE)/sqrt(length((NPTW$NO3)))
avg_chlb56 <- mean((NPTW$CHLB), na.rm = TRUE)
SE_chlb56 <- sd(((NPTW$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$CHLB)))
avg_chlc56 <- mean((NPTW$CHLC), na.rm = TRUE)
SE_chlc56 <- sd(((NPTW$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$CHLB)))
avg_Bcarot56 <- mean((NPTW$BCARTOENE), na.rm = TRUE)
SE_Bcarot56 <- sd(((NPTW$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$BCARTOENE)))
avg_butfuc56 <- mean((NPTW$X19_BUT), na.rm = TRUE)
SE_butfuc56 <- sd(((NPTW$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$X19_BUT)))
avg_hexfuc56 <- mean((NPTW$X19_HEX), na.rm = TRUE)
SE_hexfuc56 <- sd(((NPTW$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$X19_HEX)))
avg_allo56 <- mean((NPTW$ALLO), na.rm = TRUE)
SE_allo56 <- sd(((NPTW$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$ALLO)))
avg_diad56 <- mean((NPTW$DIADINO), na.rm = TRUE)
SE_diad56 <- sd(((NPTW$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$DIADINO)))
avg_fuco56 <- mean((NPTW$FUCO), na.rm = TRUE)
SE_fuco56 <- sd(((NPTW$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$FUCO)))
avg_perid56 <- mean((NPTW$PERID), na.rm = TRUE)
SE_perid56 <- sd(((NPTW$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$PERID)))
avg_zea56 <- mean((NPTW$ZEA), na.rm = TRUE)
SE_zea56 <- sd(((NPTW$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$ZEA)))
avg_dvchla56 <- mean((NPTW$DVCHLA), na.rm = TRUE)
SE_dvchla56 <- sd(((NPTW$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$DVCHLA)))
avg_chlc563 <- mean((NPTW$CHLC3), na.rm = TRUE)
SE_chlc563 <- sd(((NPTW$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$CHLC3)))
avg_lut56 <- mean((NPTW$LUT), na.rm = TRUE)
SE_lut56 <- sd(((NPTW$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$LUT)))
avg_violx56 <- mean((NPTW$VIOLX), na.rm = TRUE)
SE_violx56 <- sd(((NPTW$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$VIOLX)))
avg_pras56 <- mean((NPTW$PRAS), na.rm = TRUE)
SE_pras56 <- sd(((NPTW$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((NPTW$PRAS)))

LP_NPTW<-cbind(obs56, avg_depth56, avg_temp56,SE_Temp56, avg_sal56,SE_sal56, avg_no356,SE_no356, avg_chla56,SE_chla56,
               avg_chlb56, SE_chlb56, avg_chlc56, SE_chlc56, avg_Bcarot56, SE_Bcarot56, avg_butfuc56, SE_butfuc56,
               avg_hexfuc56, SE_hexfuc56, avg_allo56, SE_allo56, avg_diad56, SE_diad56, avg_fuco56, SE_fuco56, avg_perid56, 
               SE_perid56, avg_zea56, SE_zea56, avg_dvchla56, SE_dvchla56,avg_violx56, SE_violx56, avg_pras56, SE_pras56)
LP_NPTW<-as.data.frame(LP_NPTW)
colnames(LP_NPTW)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# SPSG --------------------------------------------------------------------

write.table(SPSG,"SPSG.csv",col.names=T,row.names=F,sep=',')
obs59 <- sum(!is.na(SPSG))
avg_chla59 <- mean(SPSG$CHLA, na.rm = TRUE)
SE_chla59 <- sd(SPSG$CHLA, na.rm = TRUE)/sqrt(length((SPSG$CHLA)))
avg_depth59 <- mean(SPSG$DEPTH, na.rm = TRUE)
avg_temp59 <- mean(SPSG$TEMP, na.rm = TRUE)
SE_Temp59 <- sd(SPSG$TEMP, na.rm = TRUE)/sqrt(length((SPSG$TEMP)))
avg_sal59 <- mean(SPSG$SALINITY, na.rm = TRUE)
SE_sal59 <- sd(SPSG$SALINITY, na.rm = TRUE)/sqrt(length((SPSG$SALINITY)))
avg_no359 <- mean(SPSG$NO3, na.rm = TRUE)
SE_no359 <- sd(SPSG$NO3, na.rm = TRUE)/sqrt(length((SPSG$NO3)))
avg_chlb59 <- mean((SPSG$CHLB), na.rm = TRUE)
SE_chlb59 <- sd(((SPSG$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$CHLB)))
avg_chlc59 <- mean((SPSG$CHLC), na.rm = TRUE)
SE_chlc59 <- sd(((SPSG$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$CHLB)))
avg_Bcarot59 <- mean((SPSG$BCARTOENE), na.rm = TRUE)
SE_Bcarot59 <- sd(((SPSG$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$BCARTOENE)))
avg_butfuc59 <- mean((SPSG$X19_BUT), na.rm = TRUE)
SE_butfuc59 <- sd(((SPSG$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$X19_BUT)))
avg_hexfuc59 <- mean((SPSG$X19_HEX), na.rm = TRUE)
SE_hexfuc59 <- sd(((SPSG$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$X19_HEX)))
avg_allo59 <- mean((SPSG$ALLO), na.rm = TRUE)
SE_allo59 <- sd(((SPSG$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$ALLO)))
avg_diad59 <- mean((SPSG$DIADINO), na.rm = TRUE)
SE_diad59 <- sd(((SPSG$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$DIADINO)))
avg_fuco59 <- mean((SPSG$FUCO), na.rm = TRUE)
SE_fuco59 <- sd(((SPSG$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$FUCO)))
avg_perid59 <- mean((SPSG$PERID), na.rm = TRUE)
SE_perid59 <- sd(((SPSG$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$PERID)))
avg_zea59 <- mean((SPSG$ZEA), na.rm = TRUE)
SE_zea59 <- sd(((SPSG$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$ZEA)))
avg_dvchla59 <- mean((SPSG$DVCHLA), na.rm = TRUE)
SE_dvchla59 <- sd(((SPSG$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$DVCHLA)))
avg_chlc593 <- mean((SPSG$CHLC3), na.rm = TRUE)
SE_chlc593 <- sd(((SPSG$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$CHLC3)))
avg_lut59 <- mean((SPSG$LUT), na.rm = TRUE)
SE_lut59 <- sd(((SPSG$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$LUT)))
avg_violx59 <- mean((SPSG$VIOLX), na.rm = TRUE)
SE_violx59 <- sd(((SPSG$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$VIOLX)))
avg_pras59 <- mean((SPSG$PRAS), na.rm = TRUE)
SE_pras59 <- sd(((SPSG$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((SPSG$PRAS)))

LP_SPSG<-cbind(obs59, avg_depth59, avg_temp59,SE_Temp59, avg_sal59,SE_sal59, avg_no359,SE_no359, avg_chla59,SE_chla59,
               avg_chlb59, SE_chlb59, avg_chlc59, SE_chlc59, avg_Bcarot59, SE_Bcarot59, avg_butfuc59, SE_butfuc59,
               avg_hexfuc59, SE_hexfuc59, avg_allo59, SE_allo59, avg_diad59, SE_diad59, avg_fuco59, SE_fuco59, avg_perid59, 
               SE_perid59, avg_zea59, SE_zea59, avg_dvchla59, SE_dvchla59,avg_violx59, SE_violx59, avg_pras59, SE_pras59)
LP_SPSG<-as.data.frame(LP_SPSG)
colnames(LP_SPSG)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# PNEC --------------------------------------------------------------------

write.table(PNEC,"PNEC.csv",col.names=T,row.names=F,sep=',')
PNEC <- rbind(PNEC, PNEC1)
obs61 <- sum(!is.na(PNEC))
avg_chla61 <- mean(PNEC$CHLA, na.rm = TRUE)
SE_chla61 <- sd(PNEC$CHLA, na.rm = TRUE)/sqrt(length((PNEC$CHLA)))
avg_depth61 <- mean(PNEC$DEPTH, na.rm = TRUE)
avg_temp61 <- mean(PNEC$TEMP, na.rm = TRUE)
SE_Temp61 <- sd(PNEC$TEMP, na.rm = TRUE)/sqrt(length((PNEC$TEMP)))
avg_sal61 <- mean(PNEC$SALINITY, na.rm = TRUE)
SE_sal61 <- sd(PNEC$SALINITY, na.rm = TRUE)/sqrt(length((PNEC$SALINITY)))
avg_no361 <- mean(PNEC$NO3, na.rm = TRUE)
SE_no361 <- sd(PNEC$NO3, na.rm = TRUE)/sqrt(length((PNEC$NO3)))
avg_chlb61 <- mean((PNEC$CHLB), na.rm = TRUE)
SE_chlb61 <- sd(((PNEC$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$CHLB)))
avg_chlc61 <- mean((PNEC$CHLC), na.rm = TRUE)
SE_chlc61 <- sd(((PNEC$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$CHLB)))
avg_Bcarot61 <- mean((PNEC$BCARTOENE), na.rm = TRUE)
SE_Bcarot61 <- sd(((PNEC$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$BCARTOENE)))
avg_butfuc61 <- mean((PNEC$X19_BUT), na.rm = TRUE)
SE_butfuc61 <- sd(((PNEC$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$X19_BUT)))
avg_hexfuc61 <- mean((PNEC$X19_HEX), na.rm = TRUE)
SE_hexfuc61 <- sd(((PNEC$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$X19_HEX)))
avg_allo61 <- mean((PNEC$ALLO), na.rm = TRUE)
SE_allo61 <- sd(((PNEC$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$ALLO)))
avg_diad61 <- mean((PNEC$DIADINO), na.rm = TRUE)
SE_diad61 <- sd(((PNEC$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$DIADINO)))
avg_fuco61 <- mean((PNEC$FUCO), na.rm = TRUE)
SE_fuco61 <- sd(((PNEC$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$FUCO)))
avg_perid61 <- mean((PNEC$PERID), na.rm = TRUE)
SE_perid61 <- sd(((PNEC$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$PERID)))
avg_zea61 <- mean((PNEC$ZEA), na.rm = TRUE)
SE_zea61 <- sd(((PNEC$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$ZEA)))
avg_dvchla61 <- mean((PNEC$DVCHLA), na.rm = TRUE)
SE_dvchla61 <- sd(((PNEC$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$DVCHLA)))
avg_chlc613 <- mean((PNEC$CHLC3), na.rm = TRUE)
SE_chlc613 <- sd(((PNEC$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$CHLC3)))
avg_lut61 <- mean((PNEC$LUT), na.rm = TRUE)
SE_lut61 <- sd(((PNEC$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$LUT)))
avg_violx61 <- mean((PNEC$VIOLX), na.rm = TRUE)
SE_violx61 <- sd(((PNEC$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$VIOLX)))
avg_pras61 <- mean((PNEC$PRAS), na.rm = TRUE)
SE_pras61 <- sd(((PNEC$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((PNEC$PRAS)))

LP_PNEC<-cbind(obs61, avg_depth61, avg_temp61,SE_Temp61, avg_sal61,SE_sal61, avg_no361,SE_no361, avg_chla61,SE_chla61,
               avg_chlb61, SE_chlb61, avg_chlc61, SE_chlc61, avg_Bcarot61, SE_Bcarot61, avg_butfuc61, SE_butfuc61,
               avg_hexfuc61, SE_hexfuc61, avg_allo61, SE_allo61, avg_diad61, SE_diad61, avg_fuco61, SE_fuco61, avg_perid61, 
               SE_perid61, avg_zea61, SE_zea61, avg_dvchla61, SE_dvchla61, avg_violx61, SE_violx61, avg_pras61, SE_pras61)
LP_PNEC<-as.data.frame(LP_PNEC)
colnames(LP_PNEC)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS","PRAS_SE")


# PEQD --------------------------------------------------------------------

write.table(PEQD,"PEQD.csv",col.names=T,row.names=F,sep=',')
obs62 <- sum(!is.na(PEQD))
avg_chla62 <- mean(PEQD$CHLA, na.rm = TRUE)
SE_chla62 <- sd(PEQD$CHLA, na.rm = TRUE)/sqrt(length((PEQD$CHLA)))
avg_depth62 <- mean(PEQD$DEPTH, na.rm = TRUE)
avg_temp62 <- mean(PEQD$TEMP, na.rm = TRUE)
SE_Temp62 <- sd(PEQD$TEMP, na.rm = TRUE)/sqrt(length((PEQD$TEMP)))
avg_sal62 <- mean(PEQD$SALINITY, na.rm = TRUE)
SE_sal62 <- sd(PEQD$SALINITY, na.rm = TRUE)/sqrt(length((PEQD$SALINITY)))
avg_no362 <- mean(PEQD$NO3, na.rm = TRUE)
SE_no362 <- sd(PEQD$NO3, na.rm = TRUE)/sqrt(length((PEQD$NO3)))
avg_chlb62 <- mean((PEQD$CHLB), na.rm = TRUE)
SE_chlb62 <- sd(((PEQD$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$CHLB)))
avg_chlc62 <- mean((PEQD$CHLC), na.rm = TRUE)
SE_chlc62 <- sd(((PEQD$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$CHLB)))
avg_Bcarot62 <- mean((PEQD$BCARTOENE), na.rm = TRUE)
SE_Bcarot62 <- sd(((PEQD$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$BCARTOENE)))
avg_butfuc62 <- mean((PEQD$X19_BUT), na.rm = TRUE)
SE_butfuc62 <- sd(((PEQD$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$X19_BUT)))
avg_hexfuc62 <- mean((PEQD$X19_HEX), na.rm = TRUE)
SE_hexfuc62 <- sd(((PEQD$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$X19_HEX)))
avg_allo62 <- mean((PEQD$ALLO), na.rm = TRUE)
SE_allo62 <- sd(((PEQD$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$ALLO)))
avg_diad62 <- mean((PEQD$DIADINO), na.rm = TRUE)
SE_diad62 <- sd(((PEQD$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$DIADINO)))
avg_fuco62 <- mean((PEQD$FUCO), na.rm = TRUE)
SE_fuco62 <- sd(((PEQD$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$FUCO)))
avg_perid62 <- mean((PEQD$PERID), na.rm = TRUE)
SE_perid62 <- sd(((PEQD$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$PERID)))
avg_zea62 <- mean((PEQD$ZEA), na.rm = TRUE)
SE_zea62 <- sd(((PEQD$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$ZEA)))
avg_dvchla62 <- mean((PEQD$DVCHLA), na.rm = TRUE)
SE_dvchla62 <- sd(((PEQD$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$DVCHLA)))
avg_chlc623 <- mean((PEQD$CHLC3), na.rm = TRUE)
SE_chlc623 <- sd(((PEQD$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$CHLC3)))
avg_lut62 <- mean((PEQD$LUT), na.rm = TRUE)
SE_lut62 <- sd(((PEQD$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$LUT)))
avg_violx62 <- mean((PEQD$VIOLX), na.rm = TRUE)
SE_violx62 <- sd(((PEQD$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$VIOLX)))
avg_pras62 <- mean((PEQD$PRAS), na.rm = TRUE)
SE_pras62 <- sd(((PEQD$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((PEQD$PRAS)))

LP_PEQD<-cbind(obs62, avg_depth62, avg_temp62,SE_Temp62, avg_sal62,SE_sal62, avg_no362,SE_no362, avg_chla62,SE_chla62,
               avg_chlb62, SE_chlb62, avg_chlc62, SE_chlc62, avg_Bcarot62, SE_Bcarot62, avg_butfuc62, SE_butfuc62,
               avg_hexfuc62, SE_hexfuc62, avg_allo62, SE_allo62, avg_diad62, SE_diad62, avg_fuco62, SE_fuco62, avg_perid62, 
               SE_perid62, avg_zea62, SE_zea62, avg_dvchla62, SE_dvchla62,avg_violx62, SE_violx62, avg_pras62, SE_pras62)
LP_PEQD<-as.data.frame(LP_PEQD)
colnames(LP_PEQD)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# WARM --------------------------------------------------------------------

write.table(WARM,"WARM.csv",col.names=T,row.names=F,sep=',')
obs63 <- sum(!is.na(WARM))
avg_chla63 <- mean(WARM$CHLA, na.rm = TRUE)
SE_chla63 <- sd(WARM$CHLA, na.rm = TRUE)/sqrt(length((WARM$CHLA)))
avg_depth63 <- mean(WARM$DEPTH, na.rm = TRUE)
avg_temp63 <- mean(WARM$TEMP, na.rm = TRUE)
SE_Temp63 <- sd(WARM$TEMP, na.rm = TRUE)/sqrt(length((WARM$TEMP)))
avg_sal63 <- mean(WARM$SALINITY, na.rm = TRUE)
SE_sal63 <- sd(WARM$SALINITY, na.rm = TRUE)/sqrt(length((WARM$SALINITY)))
avg_no363 <- mean(WARM$NO3, na.rm = TRUE)
SE_no363 <- sd(WARM$NO3, na.rm = TRUE)/sqrt(length((WARM$NO3)))
avg_chlb63 <- mean((WARM$CHLB), na.rm = TRUE)
SE_chlb63 <- sd(((WARM$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$CHLB)))
avg_chlc63 <- mean((WARM$CHLC), na.rm = TRUE)
SE_chlc63 <- sd(((WARM$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$CHLB)))
avg_Bcarot63 <- mean((WARM$BCARTOENE), na.rm = TRUE)
SE_Bcarot63 <- sd(((WARM$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$BCARTOENE)))
avg_butfuc63 <- mean((WARM$X19_BUT), na.rm = TRUE)
SE_butfuc63 <- sd(((WARM$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$X19_BUT)))
avg_hexfuc63 <- mean((WARM$X19_HEX), na.rm = TRUE)
SE_hexfuc63 <- sd(((WARM$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$X19_HEX)))
avg_allo63 <- mean((WARM$ALLO), na.rm = TRUE)
SE_allo63 <- sd(((WARM$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$ALLO)))
avg_diad63 <- mean((WARM$DIADINO), na.rm = TRUE)
SE_diad63 <- sd(((WARM$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$DIADINO)))
avg_fuco63 <- mean((WARM$FUCO), na.rm = TRUE)
SE_fuco63 <- sd(((WARM$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$FUCO)))
avg_perid63 <- mean((WARM$PERID), na.rm = TRUE)
SE_perid63 <- sd(((WARM$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$PERID)))
avg_zea63 <- mean((WARM$ZEA), na.rm = TRUE)
SE_zea63 <- sd(((WARM$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$ZEA)))
avg_dvchla63 <- mean((WARM$DVCHLA), na.rm = TRUE)
SE_dvchla63 <- sd(((WARM$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$DVCHLA)))
avg_chlc633 <- mean((WARM$CHLC3), na.rm = TRUE)
SE_chlc633 <- sd(((WARM$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$CHLC3)))
avg_lut63 <- mean((WARM$LUT), na.rm = TRUE)
SE_lut63 <- sd(((WARM$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$LUT)))
avg_violx63 <- mean((WARM$VIOLX), na.rm = TRUE)
SE_violx63 <- sd(((WARM$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$VIOLX)))
avg_pras63 <- mean((WARM$PRAS), na.rm = TRUE)
SE_pras63 <- sd(((WARM$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((WARM$PRAS)))

LP_WARM<-cbind(obs63, avg_depth63, avg_temp63,SE_Temp63, avg_sal63,SE_sal63, avg_no363,SE_no363, avg_chla63,SE_chla63,
               avg_chlb63, SE_chlb63, avg_chlc63, SE_chlc63, avg_Bcarot63, SE_Bcarot63, avg_butfuc63, SE_butfuc63,
               avg_hexfuc63, SE_hexfuc63, avg_allo63, SE_allo63, avg_diad63, SE_diad63, avg_fuco63, SE_fuco63, avg_perid63, 
               SE_perid63, avg_zea63, SE_zea63, avg_dvchla63, SE_dvchla63,avg_violx63, SE_violx63, avg_pras63, SE_pras63)
LP_WARM<-as.data.frame(LP_WARM)
colnames(LP_WARM)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# ARCH --------------------------------------------------------------------

write.table(ARCH,"ARCH.csv",col.names=T,row.names=F,sep=',')
obs64 <- sum(!is.na(ARCH))
avg_chla64 <- mean(ARCH$CHLA, na.rm = TRUE)
SE_chla64 <- sd(ARCH$CHLA, na.rm = TRUE)/sqrt(length((ARCH$CHLA)))
avg_depth64 <- mean(ARCH$DEPTH, na.rm = TRUE)
avg_temp64 <- mean(ARCH$TEMP, na.rm = TRUE)
SE_Temp64 <- sd(ARCH$TEMP, na.rm = TRUE)/sqrt(length((ARCH$TEMP)))
avg_sal64 <- mean(ARCH$SALINITY, na.rm = TRUE)
SE_sal64 <- sd(ARCH$SALINITY, na.rm = TRUE)/sqrt(length((ARCH$SALINITY)))
avg_no364 <- mean(ARCH$NO3, na.rm = TRUE)
SE_no364 <- sd(ARCH$NO3, na.rm = TRUE)/sqrt(length((ARCH$NO3)))
avg_chlb64 <- mean((ARCH$CHLB), na.rm = TRUE)
SE_chlb64 <- sd(((ARCH$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$CHLB)))
avg_chlc64 <- mean((ARCH$CHLC), na.rm = TRUE)
SE_chlc64 <- sd(((ARCH$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$CHLB)))
avg_Bcarot64 <- mean((ARCH$BCARTOENE), na.rm = TRUE)
SE_Bcarot64 <- sd(((ARCH$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$BCARTOENE)))
avg_butfuc64 <- mean((ARCH$X19_BUT), na.rm = TRUE)
SE_butfuc64 <- sd(((ARCH$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$X19_BUT)))
avg_hexfuc64 <- mean((ARCH$X19_HEX), na.rm = TRUE)
SE_hexfuc64 <- sd(((ARCH$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$X19_HEX)))
avg_allo64 <- mean((ARCH$ALLO), na.rm = TRUE)
SE_allo64 <- sd(((ARCH$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$ALLO)))
avg_diad64 <- mean((ARCH$DIADINO), na.rm = TRUE)
SE_diad64 <- sd(((ARCH$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$DIADINO)))
avg_fuco64 <- mean((ARCH$FUCO), na.rm = TRUE)
SE_fuco64 <- sd(((ARCH$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$FUCO)))
avg_perid64 <- mean((ARCH$PERID), na.rm = TRUE)
SE_perid64 <- sd(((ARCH$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$PERID)))
avg_zea64 <- mean((ARCH$ZEA), na.rm = TRUE)
SE_zea64 <- sd(((ARCH$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$ZEA)))
avg_dvchla64 <- mean((ARCH$DVCHLA), na.rm = TRUE)
SE_dvchla64 <- sd(((ARCH$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$DVCHLA)))
avg_chlc643 <- mean((ARCH$CHLC3), na.rm = TRUE)
SE_chlc643 <- sd(((ARCH$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$CHLC3)))
avg_lut64 <- mean((ARCH$LUT), na.rm = TRUE)
SE_lut64 <- sd(((ARCH$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$LUT)))
avg_violx64 <- mean((ARCH$VIOLX), na.rm = TRUE)
SE_violx64 <- sd(((ARCH$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$VIOLX)))
avg_pras64 <- mean((ARCH$PRAS), na.rm = TRUE)
SE_pras64 <- sd(((ARCH$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((ARCH$PRAS)))

LP_ARCH<-cbind(obs64, avg_depth64, avg_temp64,SE_Temp64, avg_sal64,SE_sal64, avg_no364,SE_no364, avg_chla64,SE_chla64,
               avg_chlb64, SE_chlb64, avg_chlc64, SE_chlc64, avg_Bcarot64, SE_Bcarot64, avg_butfuc64, SE_butfuc64,
               avg_hexfuc64, SE_hexfuc64, avg_allo64, SE_allo64, avg_diad64, SE_diad64, avg_fuco64, SE_fuco64, avg_perid64, 
               SE_perid64, avg_zea64, SE_zea64, avg_dvchla64, SE_dvchla64,avg_violx64, SE_violx64, avg_pras64, SE_pras64)
LP_ARCH<-as.data.frame(LP_ARCH)
colnames(LP_ARCH)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# SSTC --------------------------------------------------------------------

write.table(SSTC,"SSTC.csv",col.names=T,row.names=F,sep=',')
obs80 <- sum(!is.na(SSTC))
avg_chla80 <- mean(SSTC$CHLA, na.rm = TRUE)
SE_chla80 <- sd(SSTC$CHLA, na.rm = TRUE)/sqrt(length((SSTC$CHLA)))
avg_depth80 <- mean(SSTC$DEPTH, na.rm = TRUE)
avg_temp80 <- mean(SSTC$TEMP, na.rm = TRUE)
SE_Temp80 <- sd(SSTC$TEMP, na.rm = TRUE)/sqrt(length((SSTC$TEMP)))
avg_sal80 <- mean(SSTC$SALINITY, na.rm = TRUE)
SE_sal80 <- sd(SSTC$SALINITY, na.rm = TRUE)/sqrt(length((SSTC$SALINITY)))
avg_no380 <- mean(SSTC$NO3, na.rm = TRUE)
SE_no380 <- sd(SSTC$NO3, na.rm = TRUE)/sqrt(length((SSTC$NO3)))
avg_chlb80 <- mean((SSTC$CHLB), na.rm = TRUE)
SE_chlb80 <- sd(((SSTC$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$CHLB)))
avg_chlc80 <- mean((SSTC$CHLC), na.rm = TRUE)
SE_chlc80 <- sd(((SSTC$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$CHLB)))
avg_Bcarot80 <- mean((SSTC$BCARTOENE), na.rm = TRUE)
SE_Bcarot80 <- sd(((SSTC$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$BCARTOENE)))
avg_butfuc80 <- mean((SSTC$X19_BUT), na.rm = TRUE)
SE_butfuc80 <- sd(((SSTC$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$X19_BUT)))
avg_hexfuc80 <- mean((SSTC$X19_HEX), na.rm = TRUE)
SE_hexfuc80 <- sd(((SSTC$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$X19_HEX)))
avg_allo80 <- mean((SSTC$ALLO), na.rm = TRUE)
SE_allo80 <- sd(((SSTC$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$ALLO)))
avg_diad80 <- mean((SSTC$DIADINO), na.rm = TRUE)
SE_diad80 <- sd(((SSTC$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$DIADINO)))
avg_fuco80 <- mean((SSTC$FUCO), na.rm = TRUE)
SE_fuco80 <- sd(((SSTC$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$FUCO)))
avg_perid80 <- mean((SSTC$PERID), na.rm = TRUE)
SE_perid80 <- sd(((SSTC$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$PERID)))
avg_zea80 <- mean((SSTC$ZEA), na.rm = TRUE)
SE_zea80 <- sd(((SSTC$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$ZEA)))
avg_dvchla80 <- mean((SSTC$DVCHLA), na.rm = TRUE)
SE_dvchla80 <- sd(((SSTC$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$DVCHLA)))
avg_chlc803 <- mean((SSTC$CHLC3), na.rm = TRUE)
SE_chlc803 <- sd(((SSTC$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$CHLC3)))
avg_lut80 <- mean((SSTC$LUT), na.rm = TRUE)
SE_lut80 <- sd(((SSTC$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$LUT)))
avg_violx80 <- mean((SSTC$VIOLX), na.rm = TRUE)
SE_violx80 <- sd(((SSTC$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$VIOLX)))
avg_pras80 <- mean((SSTC$PRAS), na.rm = TRUE)
SE_pras80 <- sd(((SSTC$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((SSTC$PRAS)))

LP_SSTC<-cbind(obs80, avg_depth80, avg_temp80,SE_Temp80, avg_sal80,SE_sal80, avg_no380,SE_no380, avg_chla80,SE_chla80,
               avg_chlb80, SE_chlb80, avg_chlc80, SE_chlc80, avg_Bcarot80, SE_Bcarot80, avg_butfuc80, SE_butfuc80,
               avg_hexfuc80, SE_hexfuc80, avg_allo80, SE_allo80, avg_diad80, SE_diad80, avg_fuco80, SE_fuco80, avg_perid80, 
               SE_perid80, avg_zea80, SE_zea80, avg_dvchla80, SE_dvchla80,avg_violx80, SE_violx80, avg_pras80, SE_pras80)
LP_SSTC<-as.data.frame(LP_SSTC)
colnames(LP_SSTC)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# SANT --------------------------------------------------------------------

write.table(SANT,"SANT.csv",col.names=T,row.names=F,sep=',')
obs81 <- sum(!is.na(SANT))
avg_chla81 <- mean(SANT$CHLA, na.rm = TRUE)
SE_chla81 <- sd(SANT$CHLA, na.rm = TRUE)/sqrt(length((SANT$CHLA)))
avg_depth81 <- mean(SANT$DEPTH, na.rm = TRUE)
avg_temp81 <- mean(SANT$TEMP, na.rm = TRUE)
SE_Temp81 <- sd(SANT$TEMP, na.rm = TRUE)/sqrt(length((SANT$TEMP)))
avg_sal81 <- mean(SANT$SALINITY, na.rm = TRUE)
SE_sal81 <- sd(SANT$SALINITY, na.rm = TRUE)/sqrt(length((SANT$SALINITY)))
avg_no381 <- mean(SANT$NO3, na.rm = TRUE)
SE_no381 <- sd(SANT$NO3, na.rm = TRUE)/sqrt(length((SANT$NO3)))
avg_chlb81 <- mean((SANT$CHLB), na.rm = TRUE)
SE_chlb81 <- sd(((SANT$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$CHLB)))
avg_chlc81 <- mean((SANT$CHLC), na.rm = TRUE)
SE_chlc81 <- sd(((SANT$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$CHLB)))
avg_Bcarot81 <- mean((SANT$BCARTOENE), na.rm = TRUE)
SE_Bcarot81 <- sd(((SANT$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$BCARTOENE)))
avg_butfuc81 <- mean((SANT$X19_BUT), na.rm = TRUE)
SE_butfuc81 <- sd(((SANT$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$X19_BUT)))
avg_hexfuc81 <- mean((SANT$X19_HEX), na.rm = TRUE)
SE_hexfuc81 <- sd(((SANT$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$X19_HEX)))
avg_allo81 <- mean((SANT$ALLO), na.rm = TRUE)
SE_allo81 <- sd(((SANT$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$ALLO)))
avg_diad81 <- mean((SANT$DIADINO), na.rm = TRUE)
SE_diad81 <- sd(((SANT$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$DIADINO)))
avg_fuco81 <- mean((SANT$FUCO), na.rm = TRUE)
SE_fuco81 <- sd(((SANT$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$FUCO)))
avg_perid81 <- mean((SANT$PERID), na.rm = TRUE)
SE_perid81 <- sd(((SANT$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$PERID)))
avg_zea81 <- mean((SANT$ZEA), na.rm = TRUE)
SE_zea81 <- sd(((SANT$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$ZEA)))
avg_dvchla81 <- mean((SANT$DVCHLA), na.rm = TRUE)
SE_dvchla81 <- sd(((SANT$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$DVCHLA)))
avg_chlc813 <- mean((SANT$CHLC3), na.rm = TRUE)
SE_chlc813 <- sd(((SANT$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$CHLC3)))
avg_lut81 <- mean((SANT$LUT), na.rm = TRUE)
SE_lut81 <- sd(((SANT$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$LUT)))
avg_violx81 <- mean((SANT$VIOLX), na.rm = TRUE)
SE_violx81 <- sd(((SANT$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$VIOLX)))
avg_pras81 <- mean((SANT$PRAS), na.rm = TRUE)
SE_pras81 <- sd(((SANT$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((SANT$PRAS)))

LP_SANT<-cbind(obs81, avg_depth81, avg_temp81,SE_Temp81, avg_sal81,SE_sal81, avg_no381,SE_no381, avg_chla81,SE_chla81,
               avg_chlb81, SE_chlb81, avg_chlc81, SE_chlc81, avg_Bcarot81, SE_Bcarot81, avg_butfuc81, SE_butfuc81,
               avg_hexfuc81, SE_hexfuc81, avg_allo81, SE_allo81, avg_diad81, SE_diad81, avg_fuco81, SE_fuco81, avg_perid81, 
               SE_perid81, avg_zea81, SE_zea81, avg_dvchla81, SE_dvchla81, avg_violx81, SE_violx81, avg_pras81, SE_pras81)
LP_SANT<-as.data.frame(LP_SANT)
colnames(LP_SANT)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# ANTA ----------------------------------------------------------------------

write.table(ANTA,"ANTA.csv",col.names=T,row.names=F,sep=',')
obs82 <- sum(!is.na(ANTA))
avg_chla82 <- mean(ANTA$CHLA, na.rm = TRUE)
SE_chla82 <- sd(ANTA$CHLA, na.rm = TRUE)/sqrt(length((ANTA$CHLA)))
avg_depth82 <- mean(ANTA$DEPTH, na.rm = TRUE)
avg_temp82 <- mean(ANTA$TEMP, na.rm = TRUE)
SE_Temp82 <- sd(ANTA$TEMP, na.rm = TRUE)/sqrt(length((ANTA$TEMP)))
avg_sal82 <- mean(ANTA$SALINITY, na.rm = TRUE)
SE_sal82 <- sd(ANTA$SALINITY, na.rm = TRUE)/sqrt(length((ANTA$SALINITY)))
avg_no382 <- mean(ANTA$NO3, na.rm = TRUE)
SE_no382 <- sd(ANTA$NO3, na.rm = TRUE)/sqrt(length((ANTA$NO3)))
avg_chlb82 <- mean((ANTA$CHLB), na.rm = TRUE)
SE_chlb82 <- sd(((ANTA$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$CHLB)))
avg_chlc82 <- mean((ANTA$CHLC), na.rm = TRUE)
SE_chlc82 <- sd(((ANTA$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$CHLB)))
avg_Bcarot82 <- mean((ANTA$BCARTOENE), na.rm = TRUE)
SE_Bcarot82 <- sd(((ANTA$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$BCARTOENE)))
avg_butfuc82 <- mean((ANTA$X19_BUT), na.rm = TRUE)
SE_butfuc82 <- sd(((ANTA$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$X19_BUT)))
avg_hexfuc82 <- mean((ANTA$X19_HEX), na.rm = TRUE)
SE_hexfuc82 <- sd(((ANTA$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$X19_HEX)))
avg_allo82 <- mean((ANTA$ALLO), na.rm = TRUE)
SE_allo82 <- sd(((ANTA$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$ALLO)))
avg_diad82 <- mean((ANTA$DIADINO), na.rm = TRUE)
SE_diad82 <- sd(((ANTA$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$DIADINO)))
avg_fuco82 <- mean((ANTA$FUCO), na.rm = TRUE)
SE_fuco82 <- sd(((ANTA$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$FUCO)))
avg_perid82 <- mean((ANTA$PERID), na.rm = TRUE)
SE_perid82 <- sd(((ANTA$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$PERID)))
avg_zea82 <- mean((ANTA$ZEA), na.rm = TRUE)
SE_zea82 <- sd(((ANTA$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$ZEA)))
avg_dvchla82 <- mean((ANTA$DVCHLA), na.rm = TRUE)
SE_dvchla82 <- sd(((ANTA$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$DVCHLA)))
avg_chlc823 <- mean((ANTA$CHLC3), na.rm = TRUE)
SE_chlc823 <- sd(((ANTA$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$CHLC3)))
avg_lut82 <- mean((ANTA$LUT), na.rm = TRUE)
SE_lut82 <- sd(((ANTA$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$LUT)))
avg_violx82 <- mean((ANTA$VIOLX), na.rm = TRUE)
SE_violx82 <- sd(((ANTA$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$VIOLX)))
avg_pras82 <- mean((ANTA$PRAS), na.rm = TRUE)
SE_pras82 <- sd(((ANTA$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((ANTA$PRAS)))

LP_ANTA<-cbind(obs82, avg_depth82, avg_temp82,SE_Temp82, avg_sal82,SE_sal82, avg_no382,SE_no382, avg_chla82,SE_chla82,
               avg_chlb82, SE_chlb82, avg_chlc82, SE_chlc82, avg_Bcarot82, SE_Bcarot82, avg_butfuc82, SE_butfuc82,
               avg_hexfuc82, SE_hexfuc82, avg_allo82, SE_allo82, avg_diad82, SE_diad82, avg_fuco82, SE_fuco82, avg_perid82, 
               SE_perid82, avg_zea82, SE_zea82, avg_dvchla82, SE_dvchla82,avg_violx82, SE_violx82, avg_pras82, SE_pras82)
LP_ANTA<-as.data.frame(LP_ANTA)
colnames(LP_ANTA)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS",
                     "PRAS_SE")


# APLR ----------------------------------------------------------------------

write.table(APLR,"APLR.csv",col.names=T,row.names=F,sep=',')
obs83 <- sum(!is.na(APLR))
avg_chla83 <- mean(APLR$CHLA, na.rm = TRUE)
SE_chla83 <- sd(APLR$CHLA, na.rm = TRUE)/sqrt(length((APLR$CHLA)))
avg_depth83 <- mean(APLR$DEPTH, na.rm = TRUE)
avg_temp83 <- mean(APLR$TEMP, na.rm = TRUE)
SE_Temp83 <- sd(APLR$TEMP, na.rm = TRUE)/sqrt(length((APLR$TEMP)))
avg_sal83 <- mean(APLR$SALINITY, na.rm = TRUE)
SE_sal83 <- sd(APLR$SALINITY, na.rm = TRUE)/sqrt(length((APLR$SALINITY)))
avg_no383 <- mean(APLR$NO3, na.rm = TRUE)
SE_no383 <- sd(APLR$NO3, na.rm = TRUE)/sqrt(length((APLR$NO3)))
avg_chlb83 <- mean((APLR$CHLB), na.rm = TRUE)
SE_chlb83 <- sd(((APLR$CHLB)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$CHLB)))
avg_chlc83 <- mean((APLR$CHLC), na.rm = TRUE)
SE_chlc83 <- sd(((APLR$CHLC)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$CHLB)))
avg_Bcarot83 <- mean((APLR$BCARTOENE), na.rm = TRUE)
SE_Bcarot83 <- sd(((APLR$BCARTOENE)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$BCARTOENE)))
avg_butfuc83 <- mean((APLR$X19_BUT), na.rm = TRUE)
SE_butfuc83 <- sd(((APLR$X19_BUT)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$X19_BUT)))
avg_hexfuc83 <- mean((APLR$X19_HEX), na.rm = TRUE)
SE_hexfuc83 <- sd(((APLR$X19_HEX)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$X19_HEX)))
avg_allo83 <- mean((APLR$ALLO), na.rm = TRUE)
SE_allo83 <- sd(((APLR$ALLO)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$ALLO)))
avg_diad83 <- mean((APLR$DIADINO), na.rm = TRUE)
SE_diad83 <- sd(((APLR$DIADINO)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$DIADINO)))
avg_fuco83 <- mean((APLR$FUCO), na.rm = TRUE)
SE_fuco83 <- sd(((APLR$FUCO)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$FUCO)))
avg_perid83 <- mean((APLR$PERID), na.rm = TRUE)
SE_perid83 <- sd(((APLR$PERID)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$PERID)))
avg_zea83 <- mean((APLR$ZEA), na.rm = TRUE)
SE_zea83 <- sd(((APLR$ZEA)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$ZEA)))
avg_dvchla83 <- mean((APLR$DVCHLA), na.rm = TRUE)
SE_dvchla83 <- sd(((APLR$DVCHLA)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$DVCHLA)))
avg_chlc833 <- mean((APLR$CHLC3), na.rm = TRUE)
SE_chlc833 <- sd(((APLR$CHLC3)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$CHLC3)))
avg_lut83 <- mean((APLR$LUT), na.rm = TRUE)
SE_lut83 <- sd(((APLR$LUT)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$LUT)))
avg_violx83 <- mean((APLR$VIOLX), na.rm = TRUE)
SE_violx83 <- sd(((APLR$VIOLX)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$VIOLX)))
avg_pras83 <- mean((APLR$PRAS), na.rm = TRUE)
SE_pras83 <- sd(((APLR$PRAS)/avg_chla), na.rm = TRUE)/sqrt(length((APLR$PRAS)))

LP_APLR<-cbind(obs83, avg_depth83, avg_temp83,SE_Temp83, avg_sal83,SE_sal83, avg_no383,SE_no383, avg_chla83,SE_chla83,
               avg_chlb83, SE_chlb83, avg_chlc83, SE_chlc83, avg_Bcarot83, SE_Bcarot83, avg_butfuc83, SE_butfuc83,
               avg_hexfuc83, SE_hexfuc83, avg_allo83, SE_allo83, avg_diad83, SE_diad83, avg_fuco83, SE_fuco83, avg_perid83, 
               SE_perid83, avg_zea83, SE_zea83, avg_dvchla83, SE_dvchla83, avg_violx83, SE_violx83, avg_pras83, SE_pras83)
LP_APLR<-as.data.frame(LP_APLR)
colnames(LP_APLR)<-c("OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", 
                     "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE",
                     "BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX",
                     "X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO",
                     "FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE",
                     "VIOLX","VIOLX_SE","PRAS","PRAS_SE")


# Form the data into a table
df <- c("BPLR", "ARCT", "SARC","NADR","GFST","NASW","NATR","WTRA",
        "SATL", "NASE", "MONS","ISSG","PSAE","NPTE","NPTW","SPSG",
        "PNEC", "PEQD", "WARM","ARCH","SSTC","SANT","ANTA","APLR")
LHprovs<-rbind(LP_BPLR, LP_ARCT, LP_SARC, LP_NADR, LP_GEST, LP_NASW, LP_NATR, LP_WTRA, 
               LP_SATL, LP_NASE, LP_MONS, LP_ISSG, LP_PSAE, LP_NPTE, LP_NPTW, LP_SPSG, 
               LP_PNEC, LP_PEQD, LP_WARM, LP_ARCH, LP_SSTC, LP_SANT, LP_ANTA, LP_APLR)
LHprovs<-cbind(df,LHprovs)
LH_PROVS_CHART<-as.data.frame(LHprovs)
colnames(LH_PROVS_CHART)<-c("PROVINCES","OBS","DEPTH","TEMP","TEMP_SE","SALINITY","SALINITY_SE","NO3", "NO3_SE","CHLA","CHLA_SE","CHLB","CHLB_SE","CHLC","CHLC_SE","BCARTOENE","BCAROTENE_SE","X19_BUT","X19_BUT_SE","X19_HEX","X19_HEX_SE","ALLO","ALLO_SE","DIADINO","DIADINO_SE","FUCO","FUCO_SE","PERID","PERID_SE","ZEA","ZEA_SE","DVCHLA","DVCHLA_SE","VIOLX","VIOLX_SE","PRAS","PRAS_SE")
write.table(LH_PROVS_CHART,"LonghurstChartUpdated.csv",col.names=T,row.names=F,sep=',')


# # Hemisphere Tables -------------------------------------------------------
# 
# SEA_BPLR<-ifelse((BPLR$MONTH<=2),4,ifelse((BPLR$MONTH>2&BPLR$MONTH<=5),1,ifelse((BPLR$MONTH>5&BPLR$MONTH<=8),2,ifelse((BPLR$MONTH>8&BPLR$MONTH<=11),3,ifelse((BPLR$MONTH>=12),4,"NA")))))
# SEA_ARCT<-ifelse((ARCT$MONTH<=2),4,ifelse((ARCT$MONTH>2&ARCT$MONTH<=5),1,ifelse((ARCT$MONTH>5&ARCT$MONTH<=8),2,ifelse((ARCT$MONTH>8&ARCT$MONTH<=11),3,ifelse((ARCT$MONTH>=12),4,"NA")))))
# SEA_SARC<-ifelse((SARC$MONTH<=2),4,ifelse((SARC$MONTH>2&SARC$MONTH<=5),1,ifelse((SARC$MONTH>5&SARC$MONTH<=8),2,ifelse((SARC$MONTH>8&SARC$MONTH<=11),3,ifelse((SARC$MONTH>=12),4,"NA")))))
# SEA_PSAE<-ifelse((PSAE$MONTH<=2),4,ifelse((PSAE$MONTH>2&PSAE$MONTH<=5),1,ifelse((PSAE$MONTH>5&PSAE$MONTH<=8),2,ifelse((PSAE$MONTH>8&PSAE$MONTH<=11),3,ifelse((PSAE$MONTH>=12),4,"NA")))))
# SEA_NADR<-ifelse((NADR$MONTH<=2),4,ifelse((NADR$MONTH>2&NADR$MONTH<=5),1,ifelse((NADR$MONTH>5&NADR$MONTH<=8),2,ifelse((NADR$MONTH>8&NADR$MONTH<=11),3,ifelse((NADR$MONTH>=12),4,"NA")))))
# SEA_GEST<-ifelse((GEST$MONTH<=2),4,ifelse((GEST$MONTH>2&GEST$MONTH<=5),1,ifelse((GEST$MONTH>5&GEST$MONTH<=8),2,ifelse((GEST$MONTH>8&GEST$MONTH<=11),3,ifelse((GEST$MONTH>=12),4,"NA")))))
# SEA_NASW<-ifelse((NASW$MONTH<=2),4,ifelse((NASW$MONTH>2&NASW$MONTH<=5),1,ifelse((NASW$MONTH>5&NASW$MONTH<=8),2,ifelse((NASW$MONTH>8&NASW$MONTH<=11),3,ifelse((NASW$MONTH>=12),4,"NA")))))
# SEA_NASE<-ifelse((NASE$MONTH<=2),4,ifelse((NASE$MONTH>2&NASE$MONTH<=5),1,ifelse((NASE$MONTH>5&NASE$MONTH<=8),2,ifelse((NASE$MONTH>8&NASE$MONTH<=11),3,ifelse((NASE$MONTH>=12),4,"NA")))))
# SEA_NPTE<-ifelse((NPTE$MONTH<=2),4,ifelse((NPTE$MONTH>2&NPTE$MONTH<=5),1,ifelse((NPTE$MONTH>5&NPTE$MONTH<=8),2,ifelse((NPTE$MONTH>8&NPTE$MONTH<=11),3,ifelse((NPTE$MONTH>=12),4,"NA")))))
# SEA_NATR<-ifelse((NATR$MONTH<=2),4,ifelse((NATR$MONTH>2&NATR$MONTH<=5),1,ifelse((NATR$MONTH>5&NATR$MONTH<=8),2,ifelse((NATR$MONTH>8&NATR$MONTH<=11),3,ifelse((NATR$MONTH>=12),4,"NA")))))
# SEA_PNEC<-ifelse((PNEC$MONTH<=2),4,ifelse((PNEC$MONTH>2&PNEC$MONTH<=5),1,ifelse((PNEC$MONTH>5&PNEC$MONTH<=8),2,ifelse((PNEC$MONTH>8&PNEC$MONTH<=11),3,ifelse((PNEC$MONTH>=12),4,"NA")))))
# SEA_WTRA<-ifelse((WTRA$MONTH<=2),4,ifelse((WTRA$MONTH>2&WTRA$MONTH<=5),1,ifelse((WTRA$MONTH>5&WTRA$MONTH<=8),2,ifelse((WTRA$MONTH>8&WTRA$MONTH<=11),3,ifelse((WTRA$MONTH>=12),4,"NA")))))
# SEA_WARM<-ifelse((WARM$MONTH<=2),4,ifelse((WARM$MONTH>2&WARM$MONTH<=5),1,ifelse((WARM$MONTH>5&WARM$MONTH<=8),2,ifelse((WARM$MONTH>8&WARM$MONTH<=11),3,ifelse((WARM$MONTH>=12),4,"NA")))))
# 
# SEA_SPSG<-ifelse((SPSG$MONTH<=2),2,ifelse((SPSG$MONTH>2&SPSG$MONTH<=5),3,ifelse((SPSG$MONTH>5&SPSG$MONTH<=8),4,ifelse((SPSG$MONTH>8&SPSG$MONTH<=11),1,ifelse((SPSG$MONTH>=12),2,"NA")))))
# SEA_PEQD<-ifelse((PEQD$MONTH<=2),2,ifelse((PEQD$MONTH>2&PEQD$MONTH<=5),3,ifelse((PEQD$MONTH>5&PEQD$MONTH<=8),4,ifelse((PEQD$MONTH>8&PEQD$MONTH<=11),1,ifelse((PEQD$MONTH>=12),2,"NA")))))
# SEA_SATL<-ifelse((SATL$MONTH<=2),2,ifelse((SATL$MONTH>2&SATL$MONTH<=5),3,ifelse((SATL$MONTH>5&SATL$MONTH<=8),4,ifelse((SATL$MONTH>8&SATL$MONTH<=11),1,ifelse((SATL$MONTH>=12),2,"NA")))))
# SEA_MONS<-ifelse((MONS$MONTH<=2),2,ifelse((MONS$MONTH>2&MONS$MONTH<=5),3,ifelse((MONS$MONTH>5&MONS$MONTH<=8),4,ifelse((MONS$MONTH>8&MONS$MONTH<=11),1,ifelse((MONS$MONTH>=12),2,"NA")))))
# SEA_ISSG<-ifelse((ISSG$MONTH<=2),2,ifelse((ISSG$MONTH>2&ISSG$MONTH<=5),3,ifelse((ISSG$MONTH>5&ISSG$MONTH<=8),4,ifelse((ISSG$MONTH>8&ISSG$MONTH<=11),1,ifelse((ISSG$MONTH>=12),2,"NA")))))
# SEA_SSTC<-ifelse((SSTC$MONTH<=2),2,ifelse((SSTC$MONTH>2&SSTC$MONTH<=5),3,ifelse((SSTC$MONTH>5&SSTC$MONTH<=8),4,ifelse((SSTC$MONTH>8&SSTC$MONTH<=11),1,ifelse((SSTC$MONTH>=12),2,"NA")))))
# SEA_SANT<-ifelse((SANT$MONTH<=2),2,ifelse((SANT$MONTH>2&SANT$MONTH<=5),3,ifelse((SANT$MONTH>5&SANT$MONTH<=8),4,ifelse((SANT$MONTH>8&SANT$MONTH<=11),1,ifelse((SANT$MONTH>=12),2,"NA")))))
# SEA_ANTA<-ifelse((ANTA$MONTH<=2),2,ifelse((ANTA$MONTH>2&ANTA$MONTH<=5),3,ifelse((ANTA$MONTH>5&ANTA$MONTH<=8),4,ifelse((ANTA$MONTH>8&ANTA$MONTH<=11),1,ifelse((ANTA$MONTH>=12),2,"NA")))))
# SEA_APLR<-ifelse((APLR$MONTH<=2),2,ifelse((APLR$MONTH>2&APLR$MONTH<=5),3,ifelse((APLR$MONTH>5&APLR$MONTH<=8),4,ifelse((APLR$MONTH>8&APLR$MONTH<=11),1,ifelse((APLR$MONTH>=12),2,"NA")))))
# SEA_ARCH<-ifelse((ARCH$MONTH<=2),2,ifelse((ARCH$MONTH>2&ARCH$MONTH<=5),3,ifelse((ARCH$MONTH>5&ARCH$MONTH<=8),4,ifelse((ARCH$MONTH>8&ARCH$MONTH<=11),1,ifelse((ARCH$MONTH>=12),2,"NA")))))
# 
# BPLR <- cbind(BPLR$LAT, BPLR$LONG, BPLR$DAY, BPLR$MONTH, BPLR$YEAR, SEA_BPLR, BPLR$DEPTH, BPLR$TEMP, BPLR$SALINITY, BPLR$NO3, BPLR$CHLA, BPLR$CHLB, BPLR$CHLC, BPLR$BCARTOENE, BPLR$X19_BUT, BPLR$X19_HEX, BPLR$ALLO, BPLR$DIADINO, BPLR$FUCO, BPLR$PERID, BPLR$ZEA, BPLR$DVCHLA, BPLR$VIOLX, BPLR$PRAS, BPLR$LONG_PROVS)
# colnames(BPLR)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# ARCT <- cbind(ARCT$LAT, ARCT$LONG, ARCT$DAY, ARCT$MONTH, ARCT$YEAR, SEA_ARCT, ARCT$DEPTH, ARCT$TEMP, ARCT$SALINITY, ARCT$NO3, ARCT$CHLA, ARCT$CHLB, ARCT$CHLC, ARCT$BCARTOENE, ARCT$X19_BUT, ARCT$X19_HEX, ARCT$ALLO, ARCT$DIADINO, ARCT$FUCO, ARCT$PERID, ARCT$ZEA, ARCT$DVCHLA, ARCT$VIOLX, ARCT$PRAS, ARCT$LONG_PROVS)
# colnames(ARCT)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# SARC <- cbind(SARC$LAT, SARC$LONG, SARC$DAY, SARC$MONTH, SARC$YEAR, SEA_SARC, SARC$DEPTH, SARC$TEMP, SARC$SALINITY, SARC$NO3, SARC$CHLA, SARC$CHLB, SARC$CHLC, SARC$BCARTOENE, SARC$X19_BUT, SARC$X19_HEX, SARC$ALLO, SARC$DIADINO, SARC$FUCO, SARC$PERID, SARC$ZEA, SARC$DVCHLA, SARC$VIOLX, SARC$PRAS, SARC$LONG_PROVS)
# colnames(SARC)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# PSAE <- cbind(PSAE$LAT, PSAE$LONG, PSAE$DAY, PSAE$MONTH, PSAE$YEAR, SEA_PSAE, PSAE$DEPTH, PSAE$TEMP, PSAE$SALINITY, PSAE$NO3, PSAE$CHLA, PSAE$CHLB, PSAE$CHLC, PSAE$BCARTOENE, PSAE$X19_BUT, PSAE$X19_HEX, PSAE$ALLO, PSAE$DIADINO, PSAE$FUCO, PSAE$PERID, PSAE$ZEA, PSAE$DVCHLA, PSAE$VIOLX, PSAE$PRAS, PSAE$LONG_PROVS)
# colnames(PSAE)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# NADR <- cbind(NADR$LAT, NADR$LONG, NADR$DAY, NADR$MONTH, NADR$YEAR, SEA_NADR, NADR$DEPTH, NADR$TEMP, NADR$SALINITY, NADR$NO3, NADR$CHLA, NADR$CHLB, NADR$CHLC, NADR$BCARTOENE, NADR$X19_BUT, NADR$X19_HEX, NADR$ALLO, NADR$DIADINO, NADR$FUCO, NADR$PERID, NADR$ZEA, NADR$DVCHLA, NADR$VIOLX, NADR$PRAS, NADR$LONG_PROVS)
# colnames(NADR)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# GEST <- cbind(GEST$LAT, GEST$LONG, GEST$DAY, GEST$MONTH, GEST$YEAR, SEA_GEST, GEST$DEPTH, GEST$TEMP, GEST$SALINITY, GEST$NO3, GEST$CHLA, GEST$CHLB, GEST$CHLC, GEST$BCARTOENE, GEST$X19_BUT, GEST$X19_HEX, GEST$ALLO, GEST$DIADINO, GEST$FUCO, GEST$PERID, GEST$ZEA, GEST$DVCHLA, GEST$VIOLX, GEST$PRAS, GEST$LONG_PROVS)
# colnames(GEST)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# NASW <- cbind(NASW$LAT, NASW$LONG, NASW$DAY, NASW$MONTH, NASW$YEAR, SEA_NASW, NASW$DEPTH, NASW$TEMP, NASW$SALINITY, NASW$NO3, NASW$CHLA, NASW$CHLB, NASW$CHLC, NASW$BCARTOENE, NASW$X19_BUT, NASW$X19_HEX, NASW$ALLO, NASW$DIADINO, NASW$FUCO, NASW$PERID, NASW$ZEA, NASW$DVCHLA, NASW$VIOLX, NASW$PRAS, NASW$LONG_PROVS)
# colnames(NASW)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# NASE <- cbind(NASE$LAT, NASE$LONG, NASE$DAY, NASE$MONTH, NASE$YEAR, SEA_NASE, NASE$DEPTH, NASE$TEMP, NASE$SALINITY, NASE$NO3, NASE$CHLA, NASE$CHLB, NASE$CHLC, NASE$BCARTOENE, NASE$X19_BUT, NASE$X19_HEX, NASE$ALLO, NASE$DIADINO, NASE$FUCO, NASE$PERID, NASE$ZEA, NASE$DVCHLA, NASE$VIOLX, NASE$PRAS, NASE$LONG_PROVS)
# colnames(NASE)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# NPTE <- cbind(NPTE$LAT, NPTE$LONG, NPTE$DAY, NPTE$MONTH, NPTE$YEAR, SEA_NPTE, NPTE$DEPTH, NPTE$TEMP, NPTE$SALINITY, NPTE$NO3, NPTE$CHLA, NPTE$CHLB, NPTE$CHLC, NPTE$BCARTOENE, NPTE$X19_BUT, NPTE$X19_HEX, NPTE$ALLO, NPTE$DIADINO, NPTE$FUCO, NPTE$PERID, NPTE$ZEA, NPTE$DVCHLA, NPTE$VIOLX, NPTE$PRAS, NPTE$LONG_PROVS)
# colnames(NPTE)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# NATR <- cbind(NATR$LAT, NATR$LONG, NATR$DAY, NATR$MONTH, NATR$YEAR, SEA_NATR, NATR$DEPTH, NATR$TEMP, NATR$SALINITY, NATR$NO3, NATR$CHLA, NATR$CHLB, NATR$CHLC, NATR$BCARTOENE, NATR$X19_BUT, NATR$X19_HEX, NATR$ALLO, NATR$DIADINO, NATR$FUCO, NATR$PERID, NATR$ZEA, NATR$DVCHLA, NATR$VIOLX, NATR$PRAS, NATR$LONG_PROVS)
# colnames(NATR)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# PNEC <- cbind(PNEC$LAT, PNEC$LONG, PNEC$DAY, PNEC$MONTH, PNEC$YEAR, SEA_PNEC, PNEC$DEPTH, PNEC$TEMP, PNEC$SALINITY, PNEC$NO3, PNEC$CHLA, PNEC$CHLB, PNEC$CHLC, PNEC$BCARTOENE, PNEC$X19_BUT, PNEC$X19_HEX, PNEC$ALLO, PNEC$DIADINO, PNEC$FUCO, PNEC$PERID, PNEC$ZEA, PNEC$DVCHLA, PNEC$VIOLX, PNEC$PRAS, PNEC$LONG_PROVS)
# colnames(PNEC)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# WTRA <- cbind(WTRA$LAT, WTRA$LONG, WTRA$DAY, WTRA$MONTH, WTRA$YEAR, SEA_WTRA, WTRA$DEPTH, WTRA$TEMP, WTRA$SALINITY, WTRA$NO3, WTRA$CHLA, WTRA$CHLB, WTRA$CHLC, WTRA$BCARTOENE, WTRA$X19_BUT, WTRA$X19_HEX, WTRA$ALLO, WTRA$DIADINO, WTRA$FUCO, WTRA$PERID, WTRA$ZEA, WTRA$DVCHLA, WTRA$VIOLX, WTRA$PRAS, WTRA$LONG_PROVS)
# colnames(WTRA)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# WARM <- cbind(WARM$LAT, WARM$LONG, WARM$DAY, WARM$MONTH, WARM$YEAR, SEA_WARM, WARM$DEPTH, WARM$TEMP, WARM$SALINITY, WARM$NO3, WARM$CHLA, WARM$CHLB, WARM$CHLC, WARM$BCARTOENE, WARM$X19_BUT, WARM$X19_HEX, WARM$ALLO, WARM$DIADINO, WARM$FUCO, WARM$PERID, WARM$ZEA, WARM$DVCHLA, WARM$VIOLX, WARM$PRAS, WARM$LONG_PROVS)
# colnames(WARM)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# SPSG <- cbind(SPSG$LAT, SPSG$LONG, SPSG$DAY, SPSG$MONTH, SPSG$YEAR, SEA_SPSG, SPSG$DEPTH, SPSG$TEMP, SPSG$SALINITY, SPSG$NO3, SPSG$CHLA, SPSG$CHLB, SPSG$CHLC, SPSG$BCARTOENE, SPSG$X19_BUT, SPSG$X19_HEX, SPSG$ALLO, SPSG$DIADINO, SPSG$FUCO, SPSG$PERID, SPSG$ZEA, SPSG$DVCHLA, SPSG$VIOLX, SPSG$PRAS, SPSG$LONG_PROVS)
# colnames(SPSG)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# PEQD <- cbind(PEQD$LAT, PEQD$LONG, PEQD$DAY, PEQD$MONTH, PEQD$YEAR, SEA_PEQD, PEQD$DEPTH, PEQD$TEMP, PEQD$SALINITY, PEQD$NO3, PEQD$CHLA, PEQD$CHLB, PEQD$CHLC, PEQD$BCARTOENE, PEQD$X19_BUT, PEQD$X19_HEX, PEQD$ALLO, PEQD$DIADINO, PEQD$FUCO, PEQD$PERID, PEQD$ZEA, PEQD$DVCHLA, PEQD$VIOLX, PEQD$PRAS, PEQD$LONG_PROVS)
# colnames(PEQD)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# SATL <- cbind(SATL$LAT, SATL$LONG, SATL$DAY, SATL$MONTH, SATL$YEAR, SEA_SATL, SATL$DEPTH, SATL$TEMP, SATL$SALINITY, SATL$NO3, SATL$CHLA, SATL$CHLB, SATL$CHLC, SATL$BCARTOENE, SATL$X19_BUT, SATL$X19_HEX, SATL$ALLO, SATL$DIADINO, SATL$FUCO, SATL$PERID, SATL$ZEA, SATL$DVCHLA, SATL$VIOLX, SATL$PRAS, SATL$LONG_PROVS)
# colnames(SATL)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# MONS <- cbind(MONS$LAT, MONS$LONG, MONS$DAY, MONS$MONTH, MONS$YEAR, SEA_MONS, MONS$DEPTH, MONS$TEMP, MONS$SALINITY, MONS$NO3, MONS$CHLA, MONS$CHLB, MONS$CHLC, MONS$BCARTOENE, MONS$X19_BUT, MONS$X19_HEX, MONS$ALLO, MONS$DIADINO, MONS$FUCO, MONS$PERID, MONS$ZEA, MONS$DVCHLA, MONS$VIOLX, MONS$PRAS, MONS$LONG_PROVS)
# colnames(MONS)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# ISSG <- cbind(ISSG$LAT, ISSG$LONG, ISSG$DAY, ISSG$MONTH, ISSG$YEAR, SEA_ISSG, ISSG$DEPTH, ISSG$TEMP, ISSG$SALINITY, ISSG$NO3, ISSG$CHLA, ISSG$CHLB, ISSG$CHLC, ISSG$BCARTOENE, ISSG$X19_BUT, ISSG$X19_HEX, ISSG$ALLO, ISSG$DIADINO, ISSG$FUCO, ISSG$PERID, ISSG$ZEA, ISSG$DVCHLA, ISSG$VIOLX, ISSG$PRAS, ISSG$LONG_PROVS)
# colnames(ISSG)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# SSTC <- cbind(SSTC$LAT, SSTC$LONG, SSTC$DAY, SSTC$MONTH, SSTC$YEAR, SEA_SSTC, SSTC$DEPTH, SSTC$TEMP, SSTC$SALINITY, SSTC$NO3, SSTC$CHLA, SSTC$CHLB, SSTC$CHLC, SSTC$BCARTOENE, SSTC$X19_BUT, SSTC$X19_HEX, SSTC$ALLO, SSTC$DIADINO, SSTC$FUCO, SSTC$PERID, SSTC$ZEA, SSTC$DVCHLA, SSTC$VIOLX, SSTC$PRAS, SSTC$LONG_PROVS)
# colnames(SSTC)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# SANT <- cbind(SANT$LAT, SANT$LONG, SANT$DAY, SANT$MONTH, SANT$YEAR, SEA_SANT, SANT$DEPTH, SANT$TEMP, SANT$SALINITY, SANT$NO3, SANT$CHLA, SANT$CHLB, SANT$CHLC, SANT$BCARTOENE, SANT$X19_BUT, SANT$X19_HEX, SANT$ALLO, SANT$DIADINO, SANT$FUCO, SANT$PERID, SANT$ZEA, SANT$DVCHLA, SANT$VIOLX, SANT$PRAS, SANT$LONG_PROVS)
# colnames(SANT)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# ANTA <- cbind(ANTA$LAT, ANTA$LONG, ANTA$DAY, ANTA$MONTH, ANTA$YEAR, SEA_ANTA, ANTA$DEPTH, ANTA$TEMP, ANTA$SALINITY, ANTA$NO3, ANTA$CHLA, ANTA$CHLB, ANTA$CHLC, ANTA$BCARTOENE, ANTA$X19_BUT, ANTA$X19_HEX, ANTA$ALLO, ANTA$DIADINO, ANTA$FUCO, ANTA$PERID, ANTA$ZEA, ANTA$DVCHLA, ANTA$VIOLX, ANTA$PRAS, ANTA$LONG_PROVS)
# colnames(ANTA)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# APLR <- cbind(APLR$LAT, APLR$LONG, APLR$DAY, APLR$MONTH, APLR$YEAR, SEA_APLR, APLR$DEPTH, APLR$TEMP, APLR$SALINITY, APLR$NO3, APLR$CHLA, APLR$CHLB, APLR$CHLC, APLR$BCARTOENE, APLR$X19_BUT, APLR$X19_HEX, APLR$ALLO, APLR$DIADINO, APLR$FUCO, APLR$PERID, APLR$ZEA, APLR$DVCHLA, APLR$VIOLX, APLR$PRAS, APLR$LONG_PROVS)
# colnames(APLR)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# ARCH <- cbind(ARCH$LAT, ARCH$LONG, ARCH$DAY, ARCH$MONTH, ARCH$YEAR, SEA_ARCH, ARCH$DEPTH, ARCH$TEMP, ARCH$SALINITY, ARCH$NO3, ARCH$CHLA, ARCH$CHLB, ARCH$CHLC, ARCH$BCARTOENE, ARCH$X19_BUT, ARCH$X19_HEX, ARCH$ALLO, ARCH$DIADINO, ARCH$FUCO, ARCH$PERID, ARCH$ZEA, ARCH$DVCHLA, ARCH$VIOLX, ARCH$PRAS, ARCH$LONG_PROVS)
# colnames(ARCH)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# 
# TARAKSJC<-rbind(BPLR, ARCT, SARC, PSAE, NADR, GEST, NASW, NASE, NPTE, NATR, PNEC, WTRA, WARM, SPSG, PEQD, SATL, MONS, ISSG, SSTC, SANT, ANTA, APLR, ARCH)
# PigmentTable<-as.data.frame(TARAKSJC)
# colnames(PigmentTable)<-c("LAT","LONG","DAY","MONTH","YEAR","SEASON","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","VIOLX","PRAS","LONG_PROVS")
# write.table(PigmentTable,"PIGMENTTABLE_SEASON.csv",col.names=T,row.names=F,sep=',')