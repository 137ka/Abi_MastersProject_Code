# #Assigning the directory of the script
  setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/")
  
  #Read, Bottle data, geotraces, Kramer-Siegel and TARA OCEANS Datasets
  TARA<-read.table(file="TARA_COMBINED_ENV_SUBSET_LP_20.csv",sep=",",h=T)
  JC68<-read.table(file="JC068_bottle_summary_20.csv",sep=",",h=T)
  GeoT<-read.table(file="IDP2021_lat_lon.csv",sep=",",h=T)
  Kramsie<-read.table(file="Kramer-Siegel3.csv",sep=",",h=T)
  
  #combining folders to make data frame
  KramerSiegeldf<-cbind(Kramsie$LAT, Kramsie$LON, Kramsie$DATETIME, Kramsie$Depth.water..m...max..1, Kramsie$Temp, Kramsie$Sal, Kramsie$NO3, Kramsie$CHLA, Kramsie$CHLB, Kramsie$CHLC, Kramsie$ABCAR, Kramsie$BUTFUCO, Kramsie$HEXFUCO, Kramsie$ALLO, Kramsie$DIADINO, Kramsie$FUCO, Kramsie$PERID, Kramsie$ZEA, Kramsie$DVCHLA, Kramsie$CHLC3, Kramsie$LUT, Kramsie$VIOLA, Kramsie$PRAS, Kramsie$LONG_PROV)
  JC068bottledf<-cbind(JC68$LAT, JC68$LON, JC68$jday, JC68$udepth, JC68$utemp, JC68$upsal, JC68$Nitrate_Nit, JC68$Chlorophyll.a, JC68$Chlorophyll.b, JC68$Chlorophyll.c2, JC68$B.carotene, JC68$X19.But, JC68$X19.Hex, JC68$Alloxanthin, JC68$Diadinoxanthin, JC68$Fucoxanthin, JC68$Peridinin, JC68$Zeaxanthin, JC68$Divinyl.Chlorophyll, JC68$Chlorophyll.c3, JC68$Lutein, JC68$Violoxanthin, JC68$Prasinoxanthin, JC68$LONG_PROV)
  Taradf<-cbind(TARA$LAT, TARA$LON, TARA$TIMEDATE, TARA$Z_NOM, TARA$TEMP, TARA$SAL, TARA$NO3, TARA$CHLA, TARA$CHL_B, TARA$CHL_C1.2, TARA$CAROT, TARA$X19_BUT, TARA$X19.HEX, TARA$ALLOX, TARA$DIADIN, TARA$FUCO, TARA$PERID, TARA$ZEAX, TARA$DV_CHLA, TARA$CHL_C3, TARA$LUT, TARA$VIOLX, TARA$PRASIN, TARA$LONG_PROV)
  
  #Combining the data frame sinto one big data frame
  TARAKSJC<-rbind(KramerSiegeldf, JC068bottledf, Taradf)
  PigmentTable<-as.data.frame(TARAKSJC)
  colnames(PigmentTable)<-c("LAT","LONG","DATE","DEPTH","TEMP","SALINITY", "NO3","CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX","ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA","CHLC3","LUT","VIOLX","PRAS","LONG_PROVS")
  write.table(PigmentTable,"TTPyd.csv",col.names=T,row.names=T,sep=',')

  #clear workspace
  rm(list=ls(all=TRUE))
  library(maptools)
  data(wrld_simpl)
  
  # Read Abbie's merged pigment data file
  pigtab<-read.table(file="MAINFILE.csv",sep=",",h=T)
  finalData<-subset(pigtab,!(is.na(pigtab["LONG_PROVS"]) | is.na(pigtab["CHLA"] | is.na(pigtab["LAT"]))))

BPLR1 <- subset(finalData, finalData$LONG_PROVS==0)
BPLR <- subset(finalData, finalData$LONG_PROVS==1)
ARCT <- subset(finalData, finalData$LONG_PROVS==2)
SARC <- subset(finalData, finalData$LONG_PROVS==3)
NADR <- subset(finalData, finalData$LONG_PROVS==4)
GEST <- subset(finalData, finalData$LONG_PROVS==5)
NASW <- subset(finalData, finalData$LONG_PROVS==6)
NATR <- subset(finalData, finalData$LONG_PROVS==7)
WTRA <- subset(finalData, finalData$LONG_PROVS==8)
#ETRA <- subset(finalData, finalData$LONG_PROVS==9)
SATL <- subset(finalData, finalData$LONG_PROVS==10)
#NECS <- subset(finalData, finalData$LONG_PROVS==11)
NATR1 <- subset(finalData, finalData$LONG_PROVS==12)
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

# BPLR_MN_CHLA<-mean(BPLR$CHLA)
# BPLR_MN_ZEA<-mean(BPLR$ZEA)

y1 = as.numeric(BPLR$LAT)
x1 = as.numeric(BPLR$LONG)
y2 = as.numeric(ARCT$LAT)
x2 = as.numeric(ARCT$LONG)
y3 = as.numeric(SARC$LAT)
x3 = as.numeric(SARC$LONG)
y4 = as.numeric(NADR$LAT)
x4 = as.numeric(NADR$LONG)
y5 = as.numeric(GEST$LAT)
x5 = as.numeric(GEST$LONG)
y6 = as.numeric(NASW$LAT)
x6 = as.numeric(NASW$LONG)
y7 = as.numeric(NATR$LAT)
x7 = as.numeric(NATR$LONG)
y8 = as.numeric(WTRA$LAT)
x8 = as.numeric(WTRA$LONG)
y10 = as.numeric(SATL$LAT)
x10 = as.numeric(SATL$LONG)
y12 = as.numeric(NATR1$LAT)
x12 = as.numeric(NATR1$LONG)
y18 = as.numeric(NASE$LAT)
x18 = as.numeric(NASE$LONG)
y30 = as.numeric(MONS$LAT)
x30 = as.numeric(MONS$LONG)
y31 = as.numeric(ISSG$LAT)
x31 = as.numeric(ISSG$LONG)
y51 = as.numeric(PSAE$LAT)
x51 = as.numeric(PSAE$LONG)
y55 = as.numeric(NPTE$LAT)
x55 = as.numeric(NPTE$LONG)
y56 = as.numeric(NPTW$LAT)
x56 = as.numeric(NPTW$LONG)
y59 = as.numeric(SPSG$LAT)
x59 = as.numeric(SPSG$LONG)
y60 = as.numeric(NPTE1$LAT)
x60 = as.numeric(NPTE1$LONG)
y61 = as.numeric(PNEC$LAT)
x61 = as.numeric(PNEC$LONG)
y62 = as.numeric(PEQD$LAT)
x62 = as.numeric(PEQD$LONG)
y63 = as.numeric(WARM$LAT)
x63 = as.numeric(WARM$LONG)
y64 = as.numeric(ARCH$LAT)
x64 = as.numeric(ARCH$LONG)
y67 = as.numeric(PNEC1$LAT)
x67 = as.numeric(PNEC1$LONG)
y80 = as.numeric(SSTC$LAT)
x80 = as.numeric(SSTC$LONG)
y81 = as.numeric(SANT$LAT)
x81 = as.numeric(SANT$LONG)
y82 = as.numeric(ANTA$LAT)
x82 = as.numeric(ANTA$LONG)
y83 = as.numeric(APLR$LAT)
x83 = as.numeric(APLR$LONG)


#plotting a simple world map in grey
plot(wrld_simpl, col="gray", border="#979797", bg="white")

#points(y0~x0, cex=0.8, col="purple", pch=21)
points(y1~x1, cex=0.8, col="purple", pch=21)
points(y2~x2, cex=0.8, col="blue", pch=21)
points(y3~x3, cex=0.8, col="#71A9E8", pch=21)#Jean Blue
points(y4~x4, cex=0.8, col="#99FF33", pch=21)#Ice Blue
points(y5~x5, cex=0.8, col="#10C07F", pch=21)#Dark Turquoise
points(y6~x6, cex=0.8, col="#CCCC00", pch=21)
points(y7~x7, cex=0.8, col="#BA0A0A", pch=21)
points(y8~x8, cex=0.8, col="#EC0D7D", pch=21)
#points(y9~x9, cex=0.8, col="#FC540D", pch=21)#Deep orange
points(y10~x10, cex=0.8, col="#E85A1E", pch=21)# Cherry Pink
#points(y11~x11, cex=0.8, col="brown", pch=21)
points(y12~x12, cex=0.8, col="Yellow", pch=21)
# points(y13~x13, cex=0.8, col="#C47E16", pch=21)#Light Brown
# points(y14~x14, cex=0.8, col="cyan", pch=21)
# points(y15~x15, cex=0.8, col="cyan", pch=21)
# points(y16~x16, cex=0.8, col="#E1CB0C", pch=21)#Golden
# points(y17~x17, cex=0.8, col="blue", pch=21)
points(y18~x18, cex=0.8, col="#CCFF99", pch=21)
# points(y20~x20, cex=0.8, col="#5B3701", pch=21)#Dark Brown(S.America)
# points(y21~x21, cex=0.8, col="#C47E16", pch=21)#Light Brown
# points(y22~x22, cex=0.8, col="#C47E16", pch=21)#Light Brown
points(y30~x30, cex=0.8, col="#FAD208", pch=21)#Deep Orange
points(y31~x31, cex=0.8, col="orange", pch=21)#Cherry Pink
# points(y32~x32, cex=0.8, col="#955B03", pch=21)#Chofillate Brown
# points(y33~x33, cex=0.8, col="#E1CB0C", pch=21)#Golden
# points(y34~x34, cex=0.8, col="#5B3701", pch=21)#Dark Brown
# points(y35~x35, cex=0.8, col="cyan", pch=21)
# points(y37~x37, cex=0.8, col="#955B03", pch=21)
# points(y50~x50, cex=0.8, col="blue", pch=21)
points(y51~x51, cex=0.8, col="#006600", pch=21)#Deep Orange
# points(y53~x53, cex=0.8, col="#FC540D", pch=21)#Deep Orange
# points(y54~x54, cex=0.8, col="pink", pch=21)
points(y55~x55, cex=0.8, col="#D21C43", pch=21)#Cherry pink
points(y56~x56, cex=0.8, col="#92D050", pch=21)#Dark Turquoise
# points(y57~x57, cex=0.8, col="blue", pch=21)
# points(y58~x58, cex=0.8, col="pink", pch=21)
points(y59~x59, cex=0.8, col="#12A125", pch=21)#Cherry Pink
points(y60~x60, cex=0.8, col="#D21C43", pch=21)
points(y61~x61, cex=0.8, col="#EF892D", pch=21)#Golden
points(y62~x62, cex=0.8, col="#FFFF08", pch=21)#Deep Red
points(y63~x63, cex=0.8, col="#EC8C9C", pch=21)#Deep Red
points(y64~x64, cex=0.8, col="red", pch=21)#Grey Blue
# points(y65~x65, cex=0.8, col="#5B3701", pch=21)#Deep Brown
# points(y66~x66, cex=0.8, col="#955B03", pch=21)#Chofillate Brown
points(y67~x67, cex=0.8, col="#EF892D", pch=21)#Golden
# points(y68~x68, cex=0.8, col="#955B03", pch=21)#Chofillate Brown
# points(y70~x70, cex=0.8, col="#64DC78", pch=21)
# points(y71~x71, cex=0.8, col="cyan", pch=21)
points(y80~x80, cex=0.8, col="#589056", pch=21)#Evergreen
points(y81~x81, cex=0.8, col="#99ff99", pch=21)#Jean Blue
points(y82~x82, cex=0.8, col="#006699", pch=21)
points(y83~x83, cex=0.8, col="#5012A1", pch=21)

#col="#c00000" 2E70E4