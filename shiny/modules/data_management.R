### read data
print(getwd())
load("./../data/calls_Final.RData")
calls_Final$district_name <- levels(calls_Final$district_name)[calls_Final$district_name]
calls_Final['NamePop'] <- paste(calls_Final$region_name, calls_Final$district_name, sep = ' - ')
indexNBN <- (calls_Final$region_name == 'NO BROADER TERM') & !is.na(calls_Final$region_name)
calls_Final$NamePop[indexNBN] <- toupper(calls_Final$district_name[indexNBN])

DF <- data.table(calls_Final)
rm(calls_Final)


### data adjustment
#DF <- DF[2:10000,]
DF <- DF[, Type:=NULL]
setnames(DF, "benefit", "Type")
setnames(DF, "Datetime", "Date")
setnames(DF, "hour24", "Time")
DF[, Time:=Time-1]
setnames(DF, "NamePop", "Place")
DF <- na.omit(DF)
placeDF <- unique(DF[,c("Place", "Population")])


### aggregate
DF <- as.data.table(
  DF[,Calls:=1] 
  %>% group_by(districtID,Type, Date, Time, Place)
  %>% summarise(Calls=sum(Calls))
)
