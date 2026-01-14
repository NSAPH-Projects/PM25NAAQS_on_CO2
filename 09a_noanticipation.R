################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code reproduces results in Table S8 in the Supplementary Materials. ####
################################################################################

# clean environment
rm(list=ls())

# set the seed
set.seed(42)

# set the working directory
project.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project.dir)

# upload data
load(paste(project.dir,"/final_data.RData",sep=""))

# upload all useful libraries
library(dplyr)
library(tidyr)
library(tidybayes)
library(tidyverse)

library(readxl)
library(readr)
library(flextable)
library(officer)
library(purrr)
library(stringr)

library(ggplot2)
library(scales)
library(ggnewscale)
library(ggh4x)
library(usmap)
library(sf)

library(metafor) #for meta-analysis
library(forcats)

library(CausalArima)

# upload modified script to customize plots of the CausalArima package
source("00_modified_plot.R")
source("00_modified_library.R")

################################################################################
############################### C-ARIMA ########################################
################################################################################

######################## CausalArima preparation ###############################
dates <- seq.Date(from = as.Date("1960-01-01"), by = "years", 
                  length.out = n_until2015)
start<-as.numeric(strftime(as.Date(dates[1], "%Y-%m-%d"), "%u"))

# First analysis: Test if cumulative effect in 2002 is null with intervention 
# date = 1997;

int.date <- as.Date("1997-01-01")
horizon<-as.Date("2002-01-01") # add horizons

# # C-ARIMA model
mytable2002<-NULL
rowtable_names<-NULL

per_boxplot_2002<-matrix(NA,48,1000)

i<-NULL
for(s in unique(final_data$State)){
  i<-sum(i,1)
  data<-final_data[which(final_data$State==s),]
  ce <- CausalArima(y = ts(data$CO2,start = 1, frequency = 1), 
                    xreg = data[,c("Population","Oil_Price","Temperature",
                                   "Precipitation","gdp")],
                    dates = dates, 
                    int.date = int.date, nboot = 1000)
  
  impact_ce<-impact(ce, horizon=horizon)
  rowtable_abs2002<-rbind(impact_ce$impact_boot$`2002-01-01`$effect_cum[3,c(1:3)])
  rowtable_rel2002<-rbind(impact_ce$impact_boot$`2002-01-01`$effect_cum[4,c(1:3)])
  pval_2002<-ce$boot$inf[6,8]
  bi_pval_2002<-ce$boot$inf[6,9]
  rowtable2002<-cbind(rowtable_abs2002,rowtable_rel2002)
  
  per_boxplot_2002[i,]<-(sum(data$CO2[data$Year>=1997&data$Year<=2002]) - 
                           apply(ce$boot$boot.distrib[1:6,],2,sum))/apply(ce$boot$boot.distrib[1:6,],2,sum)
  
  rowtable_names<-rbind(rowtable_names,s)
  mytable2002<-rbind(mytable2002,cbind(rowtable2002,pval_2002,bi_pval_2002))
  
}

df2002<-data.frame(State=rowtable_names,
                   cum_effect=round(mytable2002[,1],3),
                   abs_lb=round(mytable2002[,2],3),
                   abs_up=round(mytable2002[,3],3),
                   rel_cum_effect=round(mytable2002[,4],3),
                   rel_lb=round(mytable2002[,5],3),
                   rel_up=round(mytable2002[,6],3),
                   pval_1=mytable2002[,7]<=0.05,
                   pval_m=mytable2002[,7]<=0.05/48,
                   bi_pval_1=mytable2002[,8]<=0.05,
                   bi_pval_m=mytable2002[,8]<=0.05/48)


df_noanticipation<-data.frame(States=rowtable_names,
                       sign_2002=ifelse(df2002$pval_m==TRUE,1,0),
                       sign_2002_bi=ifelse(df2002$bi_pval_m==TRUE,1,0))

rm(list=setdiff(ls(), list("df_noanticipation","project.dir")))
save.image(paste0(project.dir,"/noanticipation.RData"))

load(paste0(project.dir,"/main_analysis.RData"))

no_anticipation<-which(df_noanticipation$sign_2002_bi==0)
states_no_anticipation<-df_noanticipation$States[which(df_noanticipation$sign_2002_bi==0)]

df2009_noanticipation<-df2009[which(df2009$State%in%states_no_anticipation),]
df2012_noanticipation<-df2012[which(df2012$State%in%states_no_anticipation),]
df2014_noanticipation<-df2014[which(df2014$State%in%states_no_anticipation),]

res_2009_noanticipation <- rma(yi = cum_effect, sei = sd, data = df2009_noanticipation)
s_res_2009_noanticipation <- summary(res_2009_noanticipation)

res_2012_noanticipation <- rma(yi = cum_effect, sei = sd, data = df2012_noanticipation)
s_res_2012_noanticipation <- summary(res_2012_noanticipation)

res_2014_noanticipation <- rma(yi = cum_effect, sei = sd, data = df2014_noanticipation)
s_res_2014_noanticipation <- summary(res_2014_noanticipation)

res_2009_rel_noanticipation <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2009_noanticipation)
s_res_2009_rel_noanticipation <- summary(res_2009_rel_noanticipation)

res_2012_rel_noanticipation <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2012_noanticipation)
s_res_2012_rel_noanticipation <- summary(res_2012_rel_noanticipation)

res_2014_rel_noanticipation <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2014_noanticipation)
s_res_2014_rel_noanticipation <- summary(res_2014_rel_noanticipation)

tables8<-rbind(cbind(paste0(round(res_2009_noanticipation$beta,3),
                            ifelse(res_2009_noanticipation$pval<0.001,"***",
                                   ifelse(res_2009_noanticipation$pval<0.01,"**",
                                          ifelse(res_2009_noanticipation$pval<0.05,"*","")))),
                    paste0(round(res_2009_rel_noanticipation$beta*100,3),
                           ifelse(res_2009_rel_noanticipation$pval<0.001,"***",
                                  ifelse(res_2009_rel_noanticipation$pval<0.01,"**",
                                               ifelse(res_2009_rel$pval<0.05,"*","")))),
                    paste0(round(res_2012_noanticipation$beta,3),
                           ifelse(res_2012_noanticipation$pval<0.001,"***",
                                  ifelse(res_2012_noanticipation$pval<0.01,"**",
                                         ifelse(res_2012_noanticipation$pval<0.05,"*","")))),
                    paste0(round(res_2012_rel_noanticipation$beta*100,3),
                           ifelse(res_2012_rel_noanticipation$pval<0.001,"***",
                                  ifelse(res_2012_rel_noanticipation$pval<0.01,"**",
                                         ifelse(res_2012_rel_noanticipation$pval<0.05,"*","")))),
                    paste0(round(res_2014_noanticipation$beta,3),
                           ifelse(res_2014_noanticipation$pval<0.001,"***",
                                  ifelse(res_2014_noanticipation$pval<0.01,"**",
                                         ifelse(res_2014_noanticipation$pval<0.05,"*","")))),
                    paste0(round(res_2014_rel_noanticipation$beta*100,3),
                           ifelse(res_2014_rel_noanticipation$pval<0.001,"***",
                                  ifelse(res_2014_rel_noanticipation$pval<0.01,"**",
                                         ifelse(res_2014_rel_noanticipation$pval<0.05,"*",""))))),
              cbind(round(res_2009_noanticipation$se,3),round(res_2009_rel_noanticipation$se,3),
                    round(res_2012_noanticipation$se,3),round(res_2012_rel_noanticipation$se,3),
                    round(res_2014_noanticipation$se,3),round(res_2014_rel_noanticipation$se,3)))
colnames(tables8)<-c("2005-2009","2005-2009 rel",
                    "2005-2012","2005-2012 rel",
                    "2005-2014","2005-2014 rel")
rownames(tables8)<-c("effect","S.E.")

saveRDS(tables8, file = paste0(project.dir,"/tables/TableS8.rds"))
