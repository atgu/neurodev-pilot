#calculations for figure 1, trio pilot
#Ally Kim
#4/25/22

library(tidyverse)
library(googlesheets4)
source("/Users/heesu/Documents/NeuroDev/scripts/id_parse.R")


setwd = "/Users/heesu/Documents/NeuroDev/trio_pilot"
filepath = "/Users/heesu/Documents/NeuroDev/trio_pilot/data/trio_pilot_297_phenotype_04-06-2022.csv"

#load in data
pilot = read_csv(filepath)
eth = read_sheet("https://docs.google.com/spreadsheets/d/1uBTNQvqTldxvrZORZA0hs1cYquS5tZRnIeY7r05BlmI/edit#gid=1688491931",
                  sheet = "ethnicity", 
                 col_types = "?")

#annotate
pilot$region = id_parse(pilot[[1]], type = "region")
pilot$status = id_parse(pilot[[1]], type = "status")
pilot$family_id = id_parse(pilot[[1]], type = "family")

#subset cases only
cases <- pilot[pilot$status == "case",] #these are 99 cases

#language breakdown
cases[cases$region == "Kenya",]$lang_1


#---------------
#ethnicity breakdown
eth = eth %>% select(contains("subject_id") | starts_with("eth_1"))

#Choose only columns that aren't entirely "0" (i.e. exclude columns where nobody self-identified as that ethnic group)
eth_valid[,-which(colnames(eth) %in% c("sa_eth_detail","eth_other1", "eth_1___555", "eth_1___777", ""))]

eth_valid = eth[-1][, colSums(eth[-1])!= 0] 
eth_valid = cbind(eth[,1], eth_valid)

#list valid ethnicities
colnames(eth_valid)

#Number to ethnicity mapping - change column names
colnames(eth_valid) = c("subject_id", "Arab", "Asian", "Congolese", "European",
                             "Zimbabwean Shona", "Zimbabwean other", "Basotho", "Mixed","Khoi","San", "Indian", "Other African", "Afrikaner", "Amaxhosa", "Amazulu",
                             "Other", "Show More (K)", "Luo", "Mijikenda", "Kamba")
#first add a new column where the ethnic labels will go
eth_valid$eth <- rep(NA)

#next, iterate through the columns to mark each ethnicity, excluding subject id column and the column we are editing
for(i in 2:ncol(eth_valid)-1){
  ethnicity = eth_valid[,i]
  #where ethnicity is 1, put that column's name in the "eth" column. We are ignoring cases where there are multiple 1s at this point
  eth_valid$eth[ethnicity == 1] <- colnames(eth_valid)[i]
}

#now, address multiple ancestries. 
multiple = rowSums(eth_valid[,2:21]) > 1
eth_valid$eth[multiple] <- "Multiple"

#sep by region
eth_valid$region = id_parse(eth_valid$subject_id, type = "region")
table(eth_valid[eth_valid$region == "Kenya",]$eth)
table(eth_valid[eth_valid$region == "South Africa",]$eth)

