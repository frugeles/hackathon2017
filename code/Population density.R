# Set-up ------------------------------------------------------------------
install.packages("pdftools")
library(stringi)
library(jsonlite)
library(dplyr)
library(pdftools)
library(dplyr)
library(xml2)
library(rvest)

pop <- xml2::read_html("https://statisticalatlas.com/neighborhood/Washington/Seattle/Beacon-Hill/Population#figure/neighborhood-in-seattle")
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

neighborhood <- clean_df[2:44,]
pop <- clean_df[45:130,] 
pop <- unique(pop)

pop2 <- data.frame(pop[,-c(2,3)])
neighborhood2 <- data.frame(neighborhood[,-c(2,3)])
pop_density <- cbind(neighborhood2, pop2)
colnames(pop_density) <- c("Neighborhood","Population")
pop_density$Neighborhood <- toupper(pop_density$Neighborhood)
pop_density$Neighborhood <- gsub(" NEIGHBORHOOD", "", pop_density$Neighborhood)
