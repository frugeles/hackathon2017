packages_list=c("knitr","codetools","devtools","data.table","tm","dplyr","SnowballC","xml2","rvest","tidyverse","stringr","magrittr","hexView","httr","jsonlite","pbapply","wordcloud","text2vec","xgboost","LDAvis","topicmodels")
for (pkg in packages_list){
  print(paste0("check: ",pkg))
  if(!require(pkg,character.only = T)){
    print(paste0("need to install: ",pkg))
    install.packages(pkg)
  }
  library(pkg,character.only = T)
}

library(data.table)

#path_to_building_data <- "D:/Hackathon/Day2/call_data.csv"
##JK
##getwd()
##path_to_building_data <- "./data/call_data.csv"
##path_to_data <- gsub("call_data.csv","",path_to_building_data)

call_data <- fread(path_to_building_data)
call_data <- call_data[-1,]
head(call_data,10)
str(call_data)

dim(call_data)

test <- function(v) {
  x <-  summary(is.na(v))
  retain <- x}

na.test <- sapply(call_data, test)

call_data2 <- call_data[!is.na(Latitude) | !is.na(Longitude), ]
call_data.na <- call_data[is.na(Latitude) | is.na(Longitude), ]
dim(call_data2)
dim(call_data.na)
minLat <- min(call_data2$Latitude)
maxLat <- max(call_data2$Latitude)
minLon <- min(call_data2$Longitude)
maxLon <- max(call_data2$Longitude)

geo <- c(minLat, maxLat, minLon, maxLon)
geo

#year  <- substring(call_data$Datetime, 7, 10)
#year <- ifelse(substr(year,2,2)=='-', substr(call_data$Datetime,1,4), year)
#table(year)


# create counting variable
unique(call_data2$Datetime)

call_data2 <- call_data2 %>% mutate(
  cnt=1, 
  Date=substr(Datetime,1,10))



# summarizing the data by type
call_data_summ <- setorder(as.data.table(
  call_data2 %>%
    group_by(Type,Date) %>%  
    summarize(type_summ = sum(cnt)) 
), -type_summ)

head(call_data_summ,50)

call_data_summ1 <- setorder(as.data.table(
  call_data2 %>%
    group_by(Type) %>%  
    summarize(type_summ = sum(cnt)) 
), -type_summ)

head(call_data_summ1,50)

call_data_summ2 <- setorder(as.data.table(
  call_data2 %>%
    group_by(Type) %>%  
    summarize(type_summ = sum(cnt)) 
), Type)

head(call_data_summ2,50)

#call_data$Type <- toupper(call_data$Type)
#call_data <- call_data %>% mutate(
#  Type_GP=ifelse(substr(call_data$Type,1,3)=='Aid','Aid',
#                 ifelse(substr(call_data$Type,1,5)=='Medic','Medic',call_data$Type))
#)

type1 <- strsplit(call_data2$Type, ' ')

type11 <- data.table(unlist(type1))
type11$anz <- 1
class(type11)
names(type11)

table(type11$V1)

typesum <- setorder(type11[, .(Number = sum(anz)), by = V1], -"Number")
names(typesum)
boxplot(typesum$Number)

typesum$cumsum <- cumsum(typesum$Number)
typesum$Prop<- round(100 * typesum$cumsum / sum(typesum$Number, 0.1))

head(typesum, 50)
dim(type11)



sum(typesum$Number)


#Response
test <- data.table(call_data2[grep('Response', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

#Aid
test <- data.table(call_data2[grep('Aid', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

test <- data.table(call_data2[grep('Medic', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

test <- data.table(call_data2[grep('Alarm', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

test <- data.table(call_data2[grep('Reso', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

test <- data.table(call_data2[grep('Trans', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

test <- data.table(call_data2[grep('Motor', call_data2$Type), c("Type", "cnt")])
testsum <- test[ , .(Number = sum(cnt)), by = Type]
testsum

for (i in (1 : 25)) {
  print(i)
  print(typesum[i,1])
  test <- data.table(call_data2[grep(typesum[i,1], call_data2$Type), c("Type", "cnt")])
  testsum <- test[ , .(Number = sum(cnt)), by = Type]
  print(testsum[1:min(10, nrow(testsum))])
}

?regexpr
class(t1)
t1<- c(regexpr(pattern = typesum[1,1], call_data2$Type))
t2<- c(regexpr(pattern = typesum[2,1], call_data2$Type))
t3<- c(regexpr(pattern = typesum[3,1], call_data2$Type))
t4<- c(regexpr(pattern = typesum[4,1], call_data2$Type))
t5<- c(regexpr(pattern = typesum[5,1], call_data2$Type))
t6<- c(regexpr(pattern = typesum[6,1], call_data2$Type))
t7<- c(regexpr(pattern = typesum[7,1], call_data2$Type))
#t8<- c(regexpr(pattern = typesum[8,1], call_data2$Type))
t9<- c(regexpr(pattern = typesum[9,1], call_data2$Type))
t10<- c(regexpr(pattern = typesum[10,1], call_data2$Type))
t11<- c(regexpr(pattern = typesum[11,1], call_data2$Type))
t12<- c(regexpr(pattern = typesum[12,1], call_data2$Type))
t13<- c(regexpr(pattern = typesum[13,1], call_data2$Type))
t14<- c(regexpr(pattern = typesum[14,1], call_data2$Type))
t15<- c(regexpr(pattern = typesum[15,1], call_data2$Type))
t16<- c(regexpr(pattern = typesum[16,1], call_data2$Type))
t17<- c(regexpr(pattern = typesum[17,1], call_data2$Type))
t18<- c(regexpr(pattern = typesum[18,1], call_data2$Type))
t19<- c(regexpr(pattern = typesum[19,1], call_data2$Type))
t20<- c(regexpr(pattern = typesum[20,1], call_data2$Type))
t21<- c(regexpr(pattern = typesum[21,1], call_data2$Type))
t22<- c(regexpr(pattern = typesum[22,1], call_data2$Type))
t23<- c(regexpr(pattern = typesum[23,1], call_data2$Type))
t24<- c(regexpr(pattern = typesum[24,1], call_data2$Type))
t25<- c(regexpr(pattern = typesum[25,1], call_data2$Type))
t30<- c(regexpr(pattern = typesum[30,1], call_data2$Type))

typesum[1:50]

L_Med <- data.table(cbind(t3, t23))
L_Fir <- data.table(cbind(t5))
L_AMR <- data.table(cbind(t7, t9))
L_Mot <- data.table(cbind(t10, t11, t21))
L_Yel <- data.table(cbind(t15))
L_Aid <- data.table(cbind(t2))
L_Ele <- data.table(cbind(t30))
U_Aut <- data.table(cbind(t12, t6))
U_Res <- data.table(cbind(t1, t19, t22))
#U_Aid <- cbind(t2)
U_Alm <- data.table(cbind(t4))

#L_Med
head(L_Med)
L_Med1 <- ifelse(L_Med$t3 > 0 | L_Med$t23 > 0, 1, 0)
table(L_Med1)

#L_Fir
head(L_Fir)
L_Fir1 <- ifelse(L_Fir$t5 > 0 , 1, 0)
table(L_Fir1)

#L_AMR
head(L_AMR)
L_AMR1 <- ifelse(L_AMR$t7 > 0 | L_AMR$t9 > 0, 1, 0)
table(L_AMR1)


#L_Mot
head(L_Mot)
L_Mot1 <- ifelse(L_Mot$t10 > 0 | (L_Mot$t11 > 0  | L_Mot$t21 > 0), 1, 0)
table(L_Mot1)

#L_Yel
head(L_Yel)
L_Yel1 <- ifelse(L_Yel$t15 > 0, 1, 0)
table(L_Yel1)

#L_Ele
head(L_Ele)
L_Ele1 <- ifelse(L_Ele$t30 > 0, 1, 0)
table(L_Ele1)


#L_Aid
head(L_Aid)
L_Aid1 <- ifelse(L_Aid$t2 > 0, 1, 0)
table(L_Aid1)


#U_Aut
head(U_Aut)
U_Aut1 <- ifelse(U_Aut$t6 > 0 | U_Aut$t12 > 0, 1, 0)
table(U_Aut1)

#U_Res
head(U_Res)
U_Res1 <- ifelse(U_Res$t22 > 0 | U_Res$t19 > 0| U_Res$t1 > 0, 1, 0)
table(U_Res1)

#U_Alm
head(U_Alm)
U_Alm1 <- ifelse(U_Alm$t4 > 0, 1, 0)
table(U_Alm1)

call_data3 <- data.table(cbind(call_data2, L_Med1, L_Fir1, L_AMR1, L_Mot1, L_Yel1, L_Aid1, L_Ele1, U_Aut1, U_Res1, U_Alm1))
names(call_data3)

call_data3$benefit <- ifelse(L_Med1 == 1, "Medicin", 
                             ifelse(L_Fir1 == 1, "Fire", 
                                    ifelse(L_Mot1 == 1, "Motor", 
                                           ifelse(L_AMR1 == 1, "AMR",  
                                                  ifelse(L_Ele1 == 1, "RescEle",  
                                                         ifelse(L_Aid1 ==1, "Aid", "Other"
                                                         ))))))
table(call_data3$benefit)
print(cbind(call_data3$benefit, call_data3$Type, call_data3$L_Med1))

call_data3$Alarm <- ifelse(U_Aut1 == 1, "Auto", 
                           ifelse(U_Alm1 == 1, "Alarm", 
                                  ifelse(U_Res1 == 1, "Response", "Other")))
print(cbind(call_data3$Alarm, call_data3$Type, call_data3$U_Aut1))

table(call_data3$Alarm)

restbene <- data.table(call_data3[benefit == "Other"] )
head(restbene)
restsum <- setorder(restbene[, .(Number = sum(cnt)), by = Type], -"Number")
head(restsum, 15)

restAlarm <- data.table(call_data3[benefit == "Other"] )
head(restAlarm)
restsumAlarm <- setorder(restAlarm[, .(Number = sum(cnt)), by = Type], -"Number")
head(restsumAlarm, 10)

# Distribution overv time

head(call_data3$Datetime, 10)  
hour <- substring(call_data3$Datetime, 12,13)
PMAM <- substring(call_data3$Datetime, 21,22)
table(hour)
table(PMAM)
hour <- ifelse(PMAM == '00', 24, hour)
hour24 <- ifelse(PMAM=="PM", as.numeric(hour)+12, as.numeric(hour))
table(hour24)

call_data3$hour24 <- hour24


