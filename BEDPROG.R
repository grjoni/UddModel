rm(list = ls())
options(scipen=999)

library(pxweb)
library(tidyverse)
library("devtools")
devtools::install_github("ropengov/pxweb")
library(sqldf)
library(xlsx)
###################################################################################################
# 
#   Based on historical data from 1977 to 2019 and projection from 2020 to 2050
#   where we make cut with 2040 but possible to extend to 2050.
#
#
# output: 
#
#       køn       alder      fødested        nb          t          faar      nb_lag 
#       "factor"  "integer"  "factor"         "integer"  "numeric"  "numeric"  "integer" 
#
#     køn        ->           "kvinder","mænd"                                 : factor   skal ordnes
#     alder      ->            0:99                                            : integer 
#     fødested   ->           "Hele befolkningen","Personer født i Grønland    : factor
#     nb         -> Antal                                                      : integer
#     t          -> År                                                         : numeric
#     faar       -> fødeår    :1878:2040                                       : numeric
#     nb_lag     -> skal tjekkes                                               : integer
#
####################################################################################################
#
#   > head(d2)
# A tibble: 6 x 7
#køn   alder fødested                    nb     t  faar nb_lag
#<fct> <int> <fct>                    <int> <dbl> <dbl>  <int>
#1 Mænd     99 Personer født i Grønland     1  1977  1878     NA
#2 Mænd     98 Personer født i Grønland     0  1977  1879     NA
#3 Mænd     99 Personer født i Grønland     0  1978  1879      0
#4 Mænd     97 Personer født i Grønland     0  1977  1880     NA
#5 Mænd     98 Personer født i Grønland     0  1978  1880      0
#6 Mænd     99 Personer født i Grønland     0  1979  1880      0
#
#####################################################################################################


IN_DIR<-"X:/OED/Holdbarhedsmodel/Model_udenfor/UddModel"
setwd(IN_DIR)

Cutoff   <-2050 
Cutin    <-2020




## GS table of projection done in 2018    (kemr øryggisstadlar)


#BEXP_2018 = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BE0150/BEXP18.px"
BEXP_2020_f = "http://bank.stat.gl/api/v1/da/Greenland/BE/BE01/BE0150/BEXP20.PX"
# Historical data where we get registered to 2019

BEXP_2020_h = "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01/BE0120/BEXST1.PX"


## Varibels in tabels need 2 list due to not same name convention which is lousy discipline by GS


Var_List_2020_f <- list("version"=c("*"),
                    "place of birth"=c("*"),
                    "age"=c("*"),
                    "sex"=c("*"),
                    "time"=c("*"))
############################################################################

Var_List_2020_h <- list("residence"=c("*"),
                 "place of birth"=c("*"),
                 "gender"=c("*"),
                 "age"=c("*"),
                 "time"=c("*"))

##############################################################################

pxq_2020_f <- pxweb_query(Var_List_2020_f)
pxq_2020_h <- pxweb_query(Var_List_2020_h)

##############################################################################

tabel_2020_f <- pxweb_get(url =BEXP_2020_f,pxq_2020_f)
tabel_2020_h <- pxweb_get(url =BEXP_2020_h,pxq_2020_h)

BeProg_2020_f<- as.data.frame(tabel_2020_f,column.name.type = "code", variable.value= "code",stringsAsFactors = FALSE)
BeProg_2020_h<- as.data.frame(tabel_2020_h,column.name.type = "code", variable.value= "code",stringsAsFactors = FALSE)


## We choice 2018 main alternative

BeProg_2020_f<-BeProg_2020_f%>%filter(version=="2020 main alternative")

##  Convert to right format 

FinalBeprog<-BeProg_2020_f%>%filter(time>=Cutin)%>%transmute(sex,alder=as.numeric(age),t=as.numeric(time),nb=`Befolkningsfremskrivning 2020`,birthplace=if_else(`place of birth`=="Born in Greenland","Greenland","Total"))

## Cutoff the forecast

FinalBeprog_2020_f<-FinalBeprog%>%filter(t<=Cutoff)                                                                                     


## Get the historical data

## Take Total
BeProg_2020_h<-BeProg_2020_h%>%filter(residence=="Total")

## Make formatering

BeProg_2020_h<-BeProg_2020_h%>%transmute(sex=gender,alder=as.numeric(age),t=as.numeric(time),nb=`Population January 1st`,birthplace=`place of birth`)
BeProg_2020_h<-BeProg_2020_h%>%filter(sex%in%c("Men","Women"))
FinalBeprog_2020_h<-BeProg_2020_h%>%filter(birthplace%in%c("Greenland","Total"))

## Combine the historical and the 

FinalBeprog<-rbind(FinalBeprog_2020_h,FinalBeprog_2020_f)







FinalBeprog$faar <- FinalBeprog$t - FinalBeprog$alder

FinalBe_2020<-FinalBeprog %>% arrange(sex,birthplace,faar,t) %>% group_by(sex,birthplace,faar, add = F) %>% mutate(nb_lag = lag(nb))%>%ungroup()

## It is possible to extract danish but some issue therefore use the english version and convert to danish 
## names. Here is small letter for women (kvinder)

d2<-FinalBe_2020%>%transmute(køn=factor(if_else(sex=="Men","mænd","kvinder")),alder=as.integer(alder),fødested=
                                               factor(if_else(birthplace=="Total","Hele befolkningen","Personer født i Grønland")),nb=as.integer(nb),t,faar,nb_lag=as.integer(nb_lag))


d2<-FinalBe_2020%>%transmute(køn=factor(if_else(sex=="Men","mænd","kvinder")),alder=as.integer(alder),fødested=
                               factor(if_else(birthplace=="Total","Hele befolkningen","Personer født i Grønland")),nb=as.integer(nb),t,faar,nb_lag=as.integer(nb_lag))




save(d2, file = paste0(IN_DIR,'/Model_data/BEFPROG.RData'))


