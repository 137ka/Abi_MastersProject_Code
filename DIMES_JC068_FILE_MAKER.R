#JC068 and DIMES

# #Assigning the directory of the script
setwd("C:/Users/abbie/Documents/Oxford/Year_4/4th_Year_Project/Code/")

#Read, Bottle data, geotraces, Kramer-Siegel and TARA OCEANS Datasets
DIM<-read.table(file="DIMES_bottle_file.csv",sep=",",h=T)
JC68<-read.table(file="JC068_bottle_summary_20.csv",sep=",",h=T)

DIMESdf<-cbind(DIM$LAT, DIM$LONG, DIM$YEAR,DIM$MONTH,DIM$DAY, DIM$DEPTH, 
               DIM$SST, DIM$NO3_NO2, DIM$CHLA, DIM$CHLB, DIM$CHL_C2, DIM$BCAROT, 
               DIM$BUT, DIM$HEX, DIM$ALLO, DIM$DIAD, DIM$FUCO, 
               DIM$PERID, DIM$ZEA, DIM$DVCHLA, DIM$LUT, DIM$Violoxanthin, 
               DIM$Prasinoxanthin, DIM$Fe)
JC068bottledf<-cbind(JC68$LAT, JC68$LON, JC68$YEAR,JC68$MONTH,JC68$DAY, JC68$udepth, JC68$utemp, 
                     JC68$Nitrate_Nit, JC68$CHLA,JC68$CHLB,
                     JC68$CHLC2, JC68$CAROT,
                     JC68$but, JC68$hex, JC68$Alloxanthin, 
                     JC68$Diadinoxanthin, JC68$Fucoxanthin, JC68$Peridinin, 
                     JC68$Zeaxanthin, JC68$DVCHLA, 
                     JC68$Lutein, JC68$Violoxanthin, 
                     JC68$Prasinoxanthin, JC68$DFe)

TARAKSJC<-rbind(DIMESdf, JC068bottledf)
PigmentTable<-as.data.frame(TARAKSJC)
colnames(PigmentTable)<-c("LAT","LONG","YEAR","MONTH","DAY","DEPTH","TEMP", "NO3",
                          "CHLA","CHLB","CHLC","BCARTOENE","X19_BUT","X19_HEX",
                          "ALLO","DIADINO","FUCO","PERID","ZEA","DVCHLA",
                          "LUT","VIOLX","PRAS", "Fe")
write.table(PigmentTable,"DIMESJC068.csv",col.names=T,row.names=T,sep=',')