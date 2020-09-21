rm(list = ls())
options(scipen=999)
library(pxweb)
library(tidyverse)
library("devtools")
devtools::install_github("ropengov/pxweb")
library(sqldf)
library(xlsx)

######################################################################
#
#  Output: (Death.RData) ...format of file is RData and with variables
#
#   f?dested     alder       køn         t        nb 
#  "factor"     "integer"  "factor" "integer" "numeric"
######################################################################


## drive



IN_DIR<-"X:/OED/Holdbarhedsmodel/Model_udenfor/UddModel"
setwd(IN_DIR)




## Variables in the GS table

list_variable<- list("residence"=c("*"),
                    "place of birth"=c("*"),
                    "gender"=c("*"),
                    "age"=c("*"),
                    "type"=c("*"),
                    "time"=c("*"))

## table


pxq_query <- pxweb_query(list_variable)

## GS name of the table

mydata_ofdrei <- pxweb_get(url = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE10/BE20/BEXBBDM1.PX",pxq_query)

BEDEATH<- as.data.frame(mydata_ofdrei,column.name.type = "code", variable.value= "code",stringsAsFactors = FALSE)

## Aggregate towns and settlement and choice type Death


BEDEATH<-BEDEATH%>%filter(type=="Death")


#d2<-BEDEATH%>%transmute(fødested=factor(ifelse(`place of birth`=="Greenland","Grønland",
#                                               ifelse(`place of birth`=="Outside Greenland","Udenfor Grønland","Uoplyst"))),
#                        alder=as.integer(age),køn=factor(ifelse(gender=="Men","Mænd","Kvinder")),art="Døde",t=as.numeric(time),Døde=Death)


#write.table(d2, file = "DEATH1.txt", sep = "\t",
#            row.names = TRUE, col.names = NA)



FinalBe<-BEDEATH%>%group_by(`place of birth`,age,gender,time)%>%summarise(nb=sum(Death))%>%ungroup()



## Change to danish names but it is available option to withdraw directly the danish names but some technical issue.

d2<-FinalBe%>%transmute(fødested=factor(ifelse(`place of birth`=="Greenland","Grønland",
                                      ifelse(`place of birth`=="Outside Greenland","Udenfor Grønland","Uoplyst"))),
                                      alder=as.integer(age),køn=factor(ifelse(gender=="Men","Mænd","Kvinder")),
                                      nb=as.integer(nb),t=as.numeric(time))





sapply(d2,class)

save(d2, file = paste0(IN_DIR,"/Model_data/Death.RData"))










