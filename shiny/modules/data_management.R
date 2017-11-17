### read data
load("./data/calls_Final.RData")
DF <- data.table(calls_Final)
rm(calls_Final)


### data adjustment
#DF <- DF[2:10000,]
DF <- DF[, Type:=NULL]
setnames(DF, "benefit", "Type")
setnames(DF, "Datetime", "Date")
setnames(DF, "hour24", "Time")
DF[, Time:=Time-1]
setnames(DF, "district_name", "Place")
DF <- na.omit(DF)
placeDF <- unique(DF[,c("Place", "Population")])


### aggregate
DF <- as.data.table(
  DF[,Calls:=1] 
  %>% group_by(Type, Date, Time, Place)
  %>% summarise(Calls=sum(Calls), Population=sum(Population))
)
