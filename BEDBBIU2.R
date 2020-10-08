rm(list = ls())
options(scipen=999)
library(pxweb)
library(tidyverse)
library("devtools")
devtools::install_github("ropengov/pxweb")
library(sqldf)
library(xlsx)

################################################
#
# 
#
#
# Output : list  d2 
# vandringstype         alder           k?n      f?dested            nb             t 
#"factor"     "integer"      "factor"      "factor"     "integer"     "numeric" 
#
#
#
#
#

IN_DIR<-"C:/Users/shbj/Documents/UddModel"


setwd(IN_DIR)



setwd(IN_DIR)
#OFDREAI

## Old Forecast

qlis_ofdrei <- list("migration"=c("*"),
                    "place of birth"=c("*"),
                    "gender"=c("*"),
                    "age"=c("*"),
                    "municipality"=c("*"),  
                    "time"=c("*"))


pxq_ofdrei <- pxweb_query(qlis_ofdrei)



mydata_ofdrei <- pxweb_get(url = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE10/BE50/BEXBBIU2.PX",pxq_ofdrei)

EM_MI<- as.data.frame(mydata_ofdrei,column.name.type = "code", variable.value= "code",stringsAsFactors = FALSE)

## Aggregate towns and settlement and choice type Death

EM_MI<-EM_MI%>%filter(municipality=="Total")
EM_MI<-EM_MI%>%select(-municipality)

Immigrat<-EM_MI%>%filter(migration=="Immigrations") 

# "Greenland"         "Outside Greenland" "Unknown"

d2<-Immigrat%>%transmute(vandringstype=factor(if_else(migration=="Immigrations","Indvandringer","Indvandringer")),alder=as.integer(age),køn=factor(ifelse(gender=="Men","Mænd","Kvinder")),
                         fødested=factor(ifelse(`place of birth`=="Greenland","Grønland",
                                         ifelse(`place of birth`=="Outside Greenland","Udenfor Grønland","Uoplyst"))),
                         nb=as.integer(`Migrations to and from Greenland`),t=as.numeric(time))


#d3<-d2




save(d2, file = paste0(IN_DIR,"/Model_data/Immigration.RData"))




Emmigrat<-EM_MI%>%filter(!migration=="Immigrations") 


d2<-Emmigrat%>%transmute(vandringstype=factor(if_else(migration=="Emigrations","Udvandringer","Udvandringer")),alder=as.integer(age),køn=factor(ifelse(gender=="Men","Mænd","Kvinder")),
                         fødested=factor(ifelse(`place of birth`=="Greenland","Grønland",
                                                ifelse(`place of birth`=="Outside Greenland","Udenfor Grønland","Uoplyst"))),
                         nb=as.integer(`Migrations to and from Greenland`),t=as.numeric(time))


save(d2, file = paste0(IN_DIR,"/Model_data/Emigration.RData"))


#d2<-rbind(d2,d3)


#d2<-d2%>%spread(t,nb)

#write.table(d2, file = "Migration.txt", sep = "\t",
#            row.names = TRUE, col.names = NA)
