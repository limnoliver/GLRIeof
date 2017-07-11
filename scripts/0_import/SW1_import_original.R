# Workflow to merge and save Rdata file for the GLRI East River (sw1) site
setwd('M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff')

#read in storm event startdate, enddate, estimated, type, frozen, number, peakDisch, runoff amount and loads from file
storm_vol_load <- read.csv("EastRiver_sw1_VolumesLoads.csv",header=T,stringsAsFactors=FALSE)
storm_vol_load$Start <- strptime(storm_vol_load$Start,format="%m/%d/%Y %H:%M")
storm_vol_load$Stop <- strptime(storm_vol_load$Stop,format="%m/%d/%Y %H:%M")
colnames(storm_vol_load) <- c("Start","End","estimated","type","frozen","num","num_split","peakDisch","stormRunoff","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad")

library(Rainmaker)
library(dataRetrieval)
#read in file of precip exported from adaps (data is not all available via NWISWeb)
adaps_precip_in <- read.csv("eastRiver_sw1_Precip2.rdb",header=T,stringsAsFactors=FALSE,sep="\t",comment.char="#")
library(stringr)
adaps_precip_in$pdate <- as.POSIXct(adaps_precip_in$DATETIME,format="%Y%m%d%H%M%S")
df <- adaps_precip_in[,c(3,8)]
colnames(df) <- c("rain","pdate")
df$rain <- as.numeric(df$rain)
###################################################################################################################
# If precip data is available on NWISWeb, can use this section instead of the above
#site_no <- "441624088045601"
#StartDt <- strftime(min(storm_vol_load[which(storm_vol_load$frozen=='N'),]$Start,na.rm=TRUE) - (60*60*24*5),'%Y-%m-%d')
#EndDt <- strftime(max(storm_vol_load[which(storm_vol_load$frozen=='N'),]$End,na.rm=TRUE) + (60*60*24*5),'%Y-%m-%d')
#adaps_precip_in <- readNWISuv(site_no,'00045',StartDt,EndDt,tz="America/Chicago")
#library(stringr)
#colnames(adaps_precip_in) <- c("agency_cd","site_no","pdate","tz_cd","rain","remark")
#df <- adaps_precip_in[,c(5,3)]
#colnames(df) <- c("rain","pdate")
#####################################################################################################################

# run Rainmaker on imported precip data
rainmaker_out <- as.data.frame(RMevents(df,ieHr=.5,rainthresh=0,rain="rain",time="pdate")[1])
colnames(rainmaker_out) <- c("stormnum","StartDate","EndDate","rain")
storm_rainmaker <- RMIntense(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,10,15,30,60))
antecedent_rain <- RMarf(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",days=c(1,3,5,7),varnameout="ARF")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/RMErosivityIndex.R")
erosivity_index <- RMErosivityIndex(df,storm_rainmaker)
storm_rainmaker <- merge(storm_rainmaker,antecedent_rain,by.x="stormnum",by.y="stormnum")
storm_rainmaker <- merge(storm_rainmaker,erosivity_index,by.x="stormnum",by.y="stormnum")

# cutting out storms with estimated QW numbers
storm_vol_load_sub <- storm_vol_load[which(storm_vol_load$estimated=='N'),]

# find minimum start date in storm_vol_load_sub
storm_vol_load_Start <- aggregate(storm_vol_load_sub$Start, list(storm_vol_load_sub$num_split), FUN = min)
colnames(storm_vol_load_Start) <- c("num","Start")
# find maximum end date in storm_vol_load_sub
storm_vol_load_End <- aggregate(storm_vol_load_sub$End, list(storm_vol_load_sub$num_split), FUN = max)
colnames(storm_vol_load_End) <- c("num","End")
# find maximum peak discharge in storm_vol_load_sub
storm_vol_load_peak <- aggregate(storm_vol_load_sub$peakDisch, list(storm_vol_load_sub$num_split), FUN = max)
colnames(storm_vol_load_peak) <- c("num","peakDisch")
# sum load numbers for each constituent for each storm split number
storm_vol_load_load <- aggregate(storm_vol_load_sub[,10:18], list(storm_vol_load_sub$num_split), FUN = sum)
colnames(storm_vol_load_load) <- c("num","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad")
# find unique storm numbers and frozen status
storm_vol_load_est <- unique(storm_vol_load_sub[,c(5,7)])
colnames(storm_vol_load_est) <- c("frozen","num")
# merge all sub data frames based on storm number
storm_vol_load_merge <- merge(storm_vol_load_load, storm_vol_load_Start, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_End, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_peak, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_est, by="num")

# add storm number to storm_rainmaker data frame
storms_list <- storm_vol_load_merge[,c(11,12)]
storms_list$Start <- storms_list$Start - (120*60) # subtract 2 hours from runoff start time to account for rain start time
storms_list$num <- c(1:nrow(storms_list))
norows <- nrow(storm_vol_load_merge)
noreps <- nrow(storm_rainmaker)
storm_rainmaker$stormnum <- -9
for (i in 1:noreps) {
  for (j in 1:norows) {
    storm_rainmaker$stormnum[i] <- ifelse(as.numeric(storm_rainmaker$StartDate.x[i]-storms_list$Start[j])
                                          *as.numeric(storms_list$End[j]-storm_rainmaker$EndDate.x[i])>=0,
                                          storms_list$num[j],storm_rainmaker$stormnum[i])
  }
}

# aggregate data to the storm level, using min start, max end, sum of rain and duration and max of intensities and ARFs. 
storm_rainmaker_agg_startdt <- aggregate(storm_rainmaker$StartDate.x,list(storm_rainmaker$stormnum), min)
storm_rainmaker_agg_enddt <- aggregate(storm_rainmaker$EndDate.x,list(storm_rainmaker$stormnum),max)
storm_rainmaker_agg_sum <- aggregate(storm_rainmaker[,4:5],list(storm_rainmaker$stormnum),sum)
storm_rainmaker_agg <- aggregate(storm_rainmaker[,c(6:11,15:18,29)],list(storm_rainmaker$stormnum),max)
data_merge <- merge(storm_rainmaker_agg,storm_rainmaker_agg_startdt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_enddt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_sum,by.x="Group.1",by.y="Group.1")
storm_vol_load_merge$num <- c(1:nrow(storm_vol_load_merge))
data_merge <- merge(data_merge,storm_vol_load_merge,by.x="Group.1",by.y="num")

# add decimal year, narrow create data_sub with desired columns from data_merge and replace -9 values (NAs) with 0s
data_merge$decYear <- paste(strftime(data_merge$End,"%Y"),".",str_pad(as.POSIXlt(data_merge$End)$yday+1,3,side="left",pad="0"),sep="")
data_sub <- data_merge[,c(2:12,15:30)]
colnames(data_sub) <- c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","EI","rain_amount","duration","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad","Start","End","peakDisch","frozen","decYear")
data_sub$p5max.inches.per.hour <- ifelse(data_sub$p5max.inches.per.hour==-9,0,data_sub$p5max.inches.per.hour)
data_sub$p10max.inches.per.hour <- ifelse(data_sub$p10max.inches.per.hour==-9,0,data_sub$p10max.inches.per.hour)
data_sub$p15max.inches.per.hour <- ifelse(data_sub$p15max.inches.per.hour==-9,0,data_sub$p15max.inches.per.hour)
data_sub$p30max.inches.per.hour <- ifelse(data_sub$p30max.inches.per.hour==-9,0,data_sub$p30max.inches.per.hour)
data_sub$p60max.inches.per.hour <- ifelse(data_sub$p60max.inches.per.hour==-9,0,data_sub$p60max.inches.per.hour)
data_sub$intensity <- ifelse(data_sub$intensity==-9,0,data_sub$intensity)
data_sub$TPLoad <- ifelse(data_sub$TPLoad==-9,0,data_sub$TPLoad)
data_sub$remark <- ""
data_sub$ChlorideLoad <- ifelse(data_sub$ChlorideLoad==-9,0,data_sub$ChlorideLoad)
data_sub$NitrateLoad <- ifelse(data_sub$NitrateLoad==-9,0,data_sub$NitrateLoad)
data_sub$AmmoniumLoad <- ifelse(data_sub$AmmoniumLoad==-9,0,data_sub$AmmoniumLoad)
data_sub$TKNLoad <- ifelse(data_sub$TKNLoad==-9,0,data_sub$TKNLoad)
data_sub$DissPLoad <- ifelse(data_sub$DissPLoad==-9,0,data_sub$DissPLoad)
data_sub$TNLoad <- ifelse(data_sub$TNLoad==-9,0,data_sub$TNLoad)
data_sub$OrgNLoad <- ifelse(data_sub$OrgNLoad==-9,0,data_sub$OrgNLoad)
data_sub$frozen <- ifelse(data_sub$frozen=='Y','2','1')


# save data_sub with merged data ready for regression
save(data_sub,file="M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/dataSubEastRiverAll.RData")

#save data to .csv
write.csv(data_sub, file = "M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/sw1_rainmaker.csv", row.names = FALSE)

#create a series of basic plots to show the precip and storms next to one another
siteName <- "EastRiverAll"
pathToSave <- paste("M:/NonPoint Evaluation/GLRI Edge-of-field/JessicaStuff/GLRI/",siteName[1],sep="")
pdf(paste(pathToSave,"/","PrecipAndStorms.pdf",sep=""))
par(mfrow=c(4,1))
plot(adaps_precip_in$pdate,adaps_precip_in$VALUE,type="l",col="blue",ylab="precip")
plot(storm_vol_load$Start,storm_vol_load$peakDisch,col="red")
plot(storm_vol_load$End,storm_vol_load$peakDisch,col="red")
dev.off()


