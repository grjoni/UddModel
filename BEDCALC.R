rm(list = ls())
options(scipen=999)
library(pxweb)
library(tidyverse)
library("devtools")
devtools::install_github("ropengov/pxweb")
library(sqldf)
library(xlsx)

## Get the OFXKRDS table

IN_DIR<-"C:/Users/shbj/Documents/UddModel"

CREENBANK<-"http://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX"

setwd(IN_DIR)

#OFDREAI

##List of variables in table

list_variable <- list("generation"=c("*"),
                    "place of birth"=c("*"),
                    "gender"=c("*"),
                    "triangles(lexis)"=c("*"),
                    "event"=c("*"),
                    "time"=c("*"))


pxq_ofdrei <- pxweb_query(list_variable)

k <- pxweb(CREENBANK)
k$config$max_values_to_download <-10000 #values per call by default
mydata_ofdrei<- pxweb_get(k,pxq_ofdrei)

#mydata_ofdrei <- pxweb_get(url = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE80/BEXCALC.PX",pxq_ofdrei)
                                                                                          
                                                                                          # Get rid of categorial type  

BebCalc <- as.data.frame(mydata_ofdrei,column.name.type = "code", variable.value= "code",stringsAsFactors = FALSE)

# Filtering the data ...

birthplace    <-c("Greenland","Outside Greenland")
genderChoice  <-c("Woman","Man")
BebCalc1      <-BebCalc%>%filter(`place of birth`%in%birthplace,gender%in%genderChoice)
BebCalcFin    <-BebCalc1%>%transmute(generation,`triangles(lexis)`,event,gender,birth=`place of birth`,time=as.integer(time),nb=`Population Account`)
dd22    <-BebCalcFin%>%filter(time>=1995) 

d1<-dd22%>%transmute(generation=as.integer(generation),
                     `trekant.Lexis.`=factor(if_else(`triangles(lexis)`=="Lower","Type 1","Type 2")),
                     hændelsestype=factor(if_else(event=="Birth","Fødsel",
                      if_else(event=="Death","Dødsfald",
                      if_else(event=="Correction","Korrektioner",
                      if_else(event=="Emigration","Udvandring",
                      if_else(event=="Immigration","Indvandring","Status"
                     ))))))
                    ,køn=factor(if_else(gender=="Man","Mænd","Kvinder")),
                    fødested=factor(if_else(birth=="Greenland","Grønland",
                                    if_else(birth=="Outside Greenland","Udenfor Grønland",
                                    "Uoplyst"))),t=as.numeric(time),nb=as.integer(nb))






# HVad sker her

summary(d1)

tb <- aggregate(nb~hændelsestype+t, data = subset(d1, generation == 1985 & trekant.Lexis. == 'Type 2'),FUN=sum)
xtabs(nb~t+hændelsestype,data=tb)

tb <- aggregate(nb~t+hændelsestype, data = subset(d1,trekant.Lexis. == 'Type 2'), FUN = sum)
xtabs(nb~t+hændelsestype,data=tb)

koen0 <- plyr::mapvalues(d1$køn, from = c('Kvinder','Mænd'), to = c('2','1'))
d1$koen <- koen0


#koen0 <- plyr::mapvalues(d1$koen, from = c('Kvinder','Mænd'), to = c('2','1'))
#d1$koen <- koen0
#names(d1)

d2 <- aggregate(nb~generation+hændelsestype+køn+fødested+t, data = d1, FUN = sum)
summary(d2)



sapply(d2,class)





save(d2, file = paste0(IN_DIR,'/Model_data/bef_regnskab.RData'))

