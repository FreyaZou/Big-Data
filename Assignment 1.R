# Q2

library(readr)
library(stringr)
library(tidytext)
library(dplyr)
library(rebus)
dataset <- read_csv("~/Documents/coursework/R programming/datacamp/NIHHarvard.csv")

awards_only <- dataset %>%
  filter(!str_detect(dataset$Activity, "^T|^F"))

name <- awards_only$`Contact PI / Project Leader`

split_name1 <- str_split(name,",")

split_name1 <- data.frame(split_name1, row.names = c("lname","fmname"))
#split_name1 <- str_detect(split_name1[1,],"^+\\s")

#split_name1 <- data.frame(matrix(unlist(split_name1), nrow=2, byrow = T))
split_name1 <- data.frame(t(split_name1))
#split_name1 <- t(data.frame(matrix(unlist(split_name1), nrow=2, byrow = T)))

## split last name and middle name

split_name2 <- str_replace_all(split_name1$fmname,"^+\\s|^[[:punct:]]","")
split_name2 <- str_split(split_name2,one_or_more("+\\s"))
split_name2 <- data.frame(split_name2, row.names = c("fname","mname"))
split_name2 <- data.frame(t(split_name2))
## extract only last name
fname <- as.vector(str_replace_all(split_name2$fname," ",""))
split_names <- data.frame(split_name1, fname)
split_names <- split_names %>%
  select(fname,lname)
#uni1 <- split_names[,unique(split_names[c("lname","fname"),])]
uni2 <- unique(split_names)
write_csv(uni2,"uni2.csv")
list <- paste(uni2$fname," ",uni2$lname)