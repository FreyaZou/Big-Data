# Q1
# check packages
pkgs <- c("stringr","httr","rvest", "purrr","dplyr","XML","RCurl","readr","tidytext","rebus")

pkgTest <- function(pkgs){ 
  for (i in pkgs){
    if (!require(i,character.only = TRUE)){
      install.packages(i,dep=TRUE)
    }
  }
}
pkgTest(pkgs)

library(stringr)
library(httr)
library(rvest)
library(purrr)
library(dplyr)
library(XML)
library(RCurl)

## The url that contains S & P 100 list
the_url <- "https://en.wikipedia.org/wiki/S%26P_100#Components"

## better formatting
aa = getURL(the_url)
aa <- readLines(tc <- textConnection(aa)); close(tc)

.opts=curlOptions(followlocation=TRUE)

## Extract the table
companies <- readHTMLTable(aa)[[3]]

## Change the symbol BRK.B to BRK-B

companies <- as.data.frame(companies)

symbols <- companies %>% 
  select(Symbol)

symbols$Symbol <- gsub("BRK.B", "BRK-B", symbols$Symbol)

## Slecet the symbol column

## Create a function that generate url with each company's symbol
## Period of historical price can be specified trough transfering 
## the onset and end into format that url can read. The default setting
## is period between Feb 17, 2017 - Feb 17, 2018


## rvest
library(rvest)

table_scrape <- function(symbol){
  
  baseurl <- "https://finance.yahoo.com/quote/"
  tailurl <- "/history?period1=1487307600&period2=1518843600&interval=1d&filter=history&frequency=1d"
  url <- paste0(baseurl,symbol,tailurl)
  webpage <- read_html(url)
  
  ## Identify the table of interest
  tbls <- html_nodes(webpage, "table")
  
  ## Scrape the table
  tbls_ls <- webpage %>%
    html_nodes("table") %>%
    .[1] %>%
    html_table(fill = TRUE)
  
  ## Reformat
  colnames(tbls_ls[[1]]) <- c("Date",	"Open",	"High",	"Low",	"Close.",	"Adj.Close.",	"Volume")
  company <- rep(symbol,nrow(tbls_ls[[1]]))
  table <- cbind(tbls_ls[[1]],company)
  
  write.csv(table, paste0(symbol,".csv"))
}

## Output a csv file for each company
for (i in 1:length(symbols$Symbol)){
  table_scrape(symbols$Symbol[i])
}

## Concatenate all the csv files together as a large csv file

path <- "stock_price"
df <- multmerge(path)

filenames=list.files(path=path, full.names=TRUE,pattern="*.csv",recursive=FALSE)

all_price <- data.frame()

for (i in 1:length(filenames)) {
  file <- read.csv(filenames[i], header=TRUE, stringsAsFactors = FALSE)
  all_price <- rbind(all_price, file)
}





# Q2

library(readr)
library(tidytext)
library(rebus)
dataset <- read_csv("~/Documents/coursework/R programming/NIHHarvard.csv")

awards_only <- dataset %>%
  filter(!str_detect(dataset$Activity, "^T|^F"))

name <- awards_only$`Contact PI / Project Leader`

## Split names by comma
split_name1 <- str_split(name,",")
split_name1 <- data.frame(split_name1, row.names = c("lname","fmname"))

# Transpose the data
split_name1 <- data.frame(t(split_name1))

## Eliminate the space before last name
split_name2 <- str_replace_all(split_name1$fmname,"^+\\s|^[[:punct:]]","")

## split first name and middle name
split_name2 <- str_split(split_name2,one_or_more("+\\s"))
split_name2 <- data.frame(split_name2, row.names = c("fname","mname"))

## Transpose
split_name2 <- data.frame(t(split_name2))

## Eliminate the space before first name
fname <- as.vector(str_replace_all(split_name2$fname," ",""))
split_names <- data.frame(split_name1, fname)

## Exclude duplicated names
uni2 <- unique(split_names)

## Select first anme and last name then combine
uni2 <- uni2 %>%
  select(fname,lname)

list <- paste(uni2$fname," ",uni2$lname)

num_pubs <- function(author){
  
  url <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=", author,  "[Author] AND Harvard[Affiliation]")
  url <- str_replace_all(url, " ", "%20")
  html = htmlParse(getURL(url),encoding="UTF-8")
  
  ###Converts html to one long string
  page = as(html, "character") 
  page = gsub('\"', "", page)
  
  if (str_detect(page, "No items found") == TRUE) {
    count = 0
  } else {
    zoompage = str_extract(page, pattern = "ncbi_resultcount content=" %R% one_or_more(DIGIT))
    count = as.numeric(str_extract(zoompage, one_or_more(DIGIT)))
  }
  return(count)
  
  Sys.sleep(2+runif(1)*20)
  
}

author_pubs <- sapply(list, numbpubs)

df2 <-  as.data.frame(cbind(Authors = list, N_Publications =  author_pubs))

write.csv(df2, "authorpubs.csv", row.names = FALSE)