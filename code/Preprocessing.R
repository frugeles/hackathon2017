packages_list=c("knitr","codetools","devtools","data.table","tm","dplyr","SnowballC","xml2","rvest","tidyverse","stringr","magrittr","hexView","httr","jsonlite","pbapply","wordcloud","text2vec","xgboost","LDAvis","topicmodels")
for (pkg in packages_list){
  print(paste0("check: ",pkg))
  if(!require(pkg,character.only = T)){
    print(paste0("need to install: ",pkg))
    install.packages(pkg)
  }
  library(pkg,character.only = T)
}



path_to_building_data <- "D:/Hackathon/Day2/call_data.csv"
path_to_data <- gsub("call_data.csv","",path_to_building_data)
call_data <- fread(path_to_building_data)
call_data <- call_data[-1,]
head(call_data,10)
str(call_data)



# create counting variable
unique(call_data$Datetime)

call_data <- call_data %>% mutate(
  cnt=1, 
  Date=substr(Datetime,1,10))

# summarizing the data by type
call_data_summ <- setorder(as.data.table(
  call_data %>%
    group_by(Type,Date) %>%  
    summarize(type_summ = sum(cnt)) 
), -type_summ)

head(call_data_summ,50)

call_data_summ1 <- setorder(as.data.table(
  call_data %>%
    group_by(Type) %>%  
    summarize(type_summ = sum(cnt)) 
), -type_summ)

head(call_data_summ1,50)

call_data_summ2 <- setorder(as.data.table(
  call_data %>%
    group_by(Type) %>%  
    summarize(type_summ = sum(cnt)) 
), Type)

head(call_data_summ2,50)




#call_data$Type <- toupper(call_data$Type)
call_data <- call_data %>% mutate(
  Type_GP=ifelse(substr(call_data$Type,1,3)=='Aid','Aid',
                 ifelse(substr(call_data$Type,1,5)=='Medic','Medic',call_data$Type))
)