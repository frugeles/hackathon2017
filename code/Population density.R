####Set-up####
install.packages("pdftools")
library(stringi)
library(jsonlite)
library(dplyr)
library(pdftools)
library(dplyr)
library(xml2)
library(rvest)

####districts 1-50####
pop <- xml2::read_html("https://statisticalatlas.com/place/Washington/Seattle/Population")
data <- pop %>% html_nodes("title") %>% html_text()

clean_df <- lapply(X = data, FUN = function(x) 
{
  x_split <- unlist(strsplit(x, split = "\r\n"))
  x_split <- gsub("^\\s+|\\s+$", "", x_split)
  x_split <- x_split[x_split != ""]
  x_ret <- data.frame(name     = x_split[1],
                      hours    = x_split[2],
                      location = x_split[3])
}
)

clean_df <- do.call(rbind, clean_df)

neighborhood <- clean_df[2:51,]
pop <- clean_df[52:151,] 

pop <- unique(pop)

pop2 <- data.frame(pop[,-c(2,3)])
neighborhood2 <- data.frame(neighborhood[,-c(2,3)])
pop_density <- cbind(neighborhood2, pop2)
colnames(pop_density) <- c("Neighborhood","Population")
pop_density$Neighborhood <- toupper(pop_density$Neighborhood)
pop_density$Neighborhood <- gsub(" NEIGHBORHOOD", "", pop_density$Neighborhood)
pop_density$Population <- as.numeric(gsub(',','',levels(pop_density$Population)[pop_density$Population]))

####extra districts####
pop_extra <- xml2::read_html("https://statisticalatlas.com/neighborhood/Washington/Seattle/Beacon-Hill/Population#figure/neighborhood-in-seattle")
data <- pop_extra %>% html_nodes("title") %>% html_text()

clean_df <- lapply(X = data, FUN = function(x) 
{
  x_split <- unlist(strsplit(x, split = "\r\n"))
  x_split <- gsub("^\\s+|\\s+$", "", x_split)
  x_split <- x_split[x_split != ""]
  x_ret <- data.frame(name     = x_split[1],
                      hours    = x_split[2],
                      location = x_split[3])
}
)

clean_df <- do.call(rbind, clean_df)

neighborhood <- clean_df[2:44,]
pop_extra <- clean_df[45:130,] 
pop_extra <- unique(pop_extra)

pop_extra2 <- data.frame(pop_extra[,-c(2,3)])
neighborhood2 <- data.frame(neighborhood[,-c(2,3)])
pop_extra_density <- cbind(neighborhood2, pop_extra2)
colnames(pop_extra_density) <- c("Neighborhood","Population")
pop_extra_density$Neighborhood <- toupper(pop_extra_density$Neighborhood)
pop_extra_density$Neighborhood <- gsub(" NEIGHBORHOOD", "", pop_extra_density$Neighborhood)
pop_extra_density$Population <- as.numeric(gsub(',','',levels(pop_extra_density$Population)[pop_extra_density$Population]))
####combine####
final_pop <- unique(rbind(pop_density, pop_extra_density))

####merge district pop####
getwd()
load("./data/calls_district.RData")

colnames(final_pop) <- c("region_name","Population")

calls$region_name <- as.character(levels(calls$region_name)[calls$region_name])
calls_w_pop <- left_join(calls, final_pop)

save.image('./data/calls_Final.RData')



#### Other Processing of data ####

data_path = './data/'

load(paste0(data_path,'calls_Final.RData'))

calls_Final$district_name <- levels(calls_Final$district_name)[calls_Final$district_name]

calls_Final['NamePop'] <- paste(calls_Final$region_name, calls_Final$district_name, sep = ' - ')

indexNBN <- (calls_Final$region_name == 'NO BROADER TERM') & !is.na(calls_Final$region_name)
calls_Final$NamePop[indexNBN] <- toupper(calls_Final$district_name[indexNBN])
