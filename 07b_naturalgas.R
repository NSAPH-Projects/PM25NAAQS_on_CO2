################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code identifies the states for which there is a significant increase in
## CO2 emissions from natural gas; this information in used to test the ########
## assumption that the overall observed reduction in CO2 emissions is not due to
## the new fracking (Supplementary Materials). #################################
################################################################################

# set the seed
set.seed(42)

# set the working directory
project.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project.dir)

# upload data
load(paste(project.dir,"/final_data_naturalgas.RData",sep=""))

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

############################ Main analysis #####################################
int.date <- as.Date("2002-01-01")
horizon<-as.Date(c("2005-01-01","2009-01-01","2012-01-01","2014-01-01")) # add horizons

# # C-ARIMA model
effect_cum_0509<-NULL
effect_cum_0512<-NULL
effect_cum_0514<-NULL

rowtable_names<-NULL

# boot<-list()

per_boxplot_2009<-matrix(NA,48,1000)
per_boxplot_2012<-matrix(NA,48,1000)
per_boxplot_2014<-matrix(NA,48,1000)

i<-NULL

for(s in unique(final_data$State)){
  i<-sum(i,1)
  data<-final_data[which(final_data$State==s),]
  ce <- CausalArima(y = ts(data$CO2,start = 1, frequency = 1), 
                    xreg = data[,c("Population","Oil_Price","Temperature",
                                   "Precipitation",
                                   "gdp")],
                    dates = dates, 
                    int.date = int.date, nboot = 1000)
  
  ce_cum_0509 <- sum(ce$causal.effect[4:8])
  counter_0505 <- sum(ce$forecast[4:8])
  ce_cum_0509_rel <- ce_cum_0509/counter_0505
  
  ce_cum_0512 <- sum(ce$causal.effect[4:11])
  counter_0512 <- sum(ce$forecast[4:11])
  ce_cum_0512_rel <- ce_cum_0512/counter_0512
  
  ce_cum_0514 <- sum(ce$causal.effect[4:13])
  counter_0514 <- sum(ce$forecast[4:13])
  ce_cum_0514_rel <- ce_cum_0514/counter_0514
  
  cum_distr_0509 <- sum(data$CO2[data$Year>=2005&data$Year<=2009]) - 
    apply(ce$boot$boot.distrib[4:8,],2,sum)
  cum_distr_0509_rel <- cum_distr_0509/apply(ce$boot$boot.distrib[4:8,],2,sum)
  ce_cum_0509_int <- quantile(cum_distr_0509,c(0.025,0.975))
  ce_cum_0509_int_rel <- quantile(cum_distr_0509_rel,c(0.025,0.975))
  ce_cum_0509_sd <- sd(cum_distr_0509)
  ce_cum_0509_rel_sd <- sd(cum_distr_0509_rel)
  pval_0509 <- mean(cum_distr_0509>=0)
  bi_pval_0509 <- 2-2*max(mean(cum_distr_0509 < 0), mean(cum_distr_0509 > 0))
  
  cum_distr_0512 <- sum(data$CO2[data$Year>=2005&data$Year<=2012]) - 
    apply(ce$boot$boot.distrib[4:11,],2,sum)
  cum_distr_0512_rel <- cum_distr_0512/apply(ce$boot$boot.distrib[4:11,],2,sum)
  ce_cum_0512_int <- quantile(cum_distr_0512,c(0.025,0.975))
  ce_cum_0512_int_rel <- quantile(cum_distr_0512_rel,c(0.025,0.975))
  ce_cum_0512_sd <- sd(cum_distr_0512)
  ce_cum_0512_rel_sd <- sd(cum_distr_0512_rel)
  pval_0512 <- mean(cum_distr_0512>=0)
  bi_pval_0512 <- 2-2*max(mean(cum_distr_0512 < 0), mean(cum_distr_0512 > 0))
  
  cum_distr_0514 <- sum(data$CO2[data$Year>=2005&data$Year<=2014]) - 
    apply(ce$boot$boot.distrib[4:13,],2,sum)
  cum_distr_0514_rel <- cum_distr_0514/apply(ce$boot$boot.distrib[4:13,],2,sum)
  ce_cum_0514_int <- quantile(cum_distr_0514,c(0.025,0.975))
  ce_cum_0514_int_rel <- quantile(cum_distr_0514_rel,c(0.025,0.975))
  ce_cum_0514_sd <- sd(cum_distr_0514)
  ce_cum_0514_rel_sd <- sd(cum_distr_0514_rel)
  pval_0514 <- mean(cum_distr_0514>=0)
  bi_pval_0514 <- 2-2*max(mean(cum_distr_0514 < 0), mean(cum_distr_0514 > 0))
  
  effect_cum_0509<-rbind(effect_cum_0509,
                         cbind(ce_cum_0509,
                               counter_0505,
                               ce_cum_0509_rel,
                               t(ce_cum_0509_int),
                               t(ce_cum_0509_int_rel),
                               ce_cum_0509_sd,
                               ce_cum_0509_rel_sd,
                               pval_0509,
                               bi_pval_0509))
  
  effect_cum_0512<-rbind(effect_cum_0512,
                         cbind(ce_cum_0512,
                               counter_0512,
                               ce_cum_0512_rel,
                               t(ce_cum_0512_int),
                               t(ce_cum_0512_int_rel),
                               ce_cum_0512_sd,
                               ce_cum_0512_rel_sd,
                               pval_0512,
                               bi_pval_0512))
  
  effect_cum_0514<-rbind(effect_cum_0514,
                         cbind(ce_cum_0514,
                               counter_0514,
                               ce_cum_0514_rel,
                               t(ce_cum_0514_int),
                               t(ce_cum_0514_int_rel),
                               ce_cum_0514_sd,
                               ce_cum_0514_rel_sd,
                               pval_0514,
                               bi_pval_0514))
  
  per_boxplot_2009[i,]<-cum_distr_0509/apply(ce$boot$boot.distrib[4:8,],2,sum)
  per_boxplot_2012[i,]<-cum_distr_0512/apply(ce$boot$boot.distrib[4:11,],2,sum)
  per_boxplot_2014[i,]<-cum_distr_0514/apply(ce$boot$boot.distrib[4:13,],2,sum)
  
  rowtable_names<-rbind(rowtable_names,s)
  
}

df2009<-data.frame(State=rowtable_names,
                   cum_effect=round(effect_cum_0509[,1],3),
                   counter=round(effect_cum_0509[,2],3),
                   cum_effect_rel = round(effect_cum_0509[,3],3),
                   abs_lb=round(effect_cum_0509[,4],3),
                   abs_up=round(effect_cum_0509[,5],3),
                   abs_lb_rel=round(effect_cum_0509[,6],3),
                   abs_up_rel=round(effect_cum_0509[,7],3),
                   sd=round(effect_cum_0509[,8],3),
                   sd_rel=round(effect_cum_0509[,9],3),
                   pval=effect_cum_0509[,10],
                   bi_pval=effect_cum_0509[,11]
)

df2012<-data.frame(State=rowtable_names,
                   cum_effect=round(effect_cum_0512[,1],3),
                   counter=round(effect_cum_0512[,2],3),
                   cum_effect_rel = round(effect_cum_0512[,3],5),
                   abs_lb=round(effect_cum_0512[,4],3),
                   abs_up=round(effect_cum_0512[,5],3),
                   abs_lb_rel=round(effect_cum_0512[,6],3),
                   abs_up_rel=round(effect_cum_0512[,7],3),
                   sd=round(effect_cum_0512[,8],3),
                   sd_rel=round(effect_cum_0512[,9],3),
                   pval=effect_cum_0512[,10],
                   bi_pval=effect_cum_0512[,11]
)

df2014<-data.frame(State=rowtable_names,
                   cum_effect=round(effect_cum_0514[,1],3),
                   counter=round(effect_cum_0514[,2],3),
                   cum_effect_rel = round(effect_cum_0514[,3],3),
                   abs_lb=round(effect_cum_0514[,4],3),
                   abs_up=round(effect_cum_0514[,5],3),
                   abs_lb_rel=round(effect_cum_0514[,6],3),
                   abs_up_rel=round(effect_cum_0514[,7],3),
                   sd=round(effect_cum_0514[,8],3),
                   sd_rel=round(effect_cum_0514[,9],3),
                   pval=effect_cum_0514[,10],
                   bi_pval=effect_cum_0514[,11]
)

res_2009 <- rma(yi = cum_effect, sei = sd, data = df2009, method = "REML")
summary(res_2009)

res_2012 <- rma(yi = cum_effect, sei = sd, data = df2012, method = "REML")
summary(res_2012)

res_2014 <- rma(yi = cum_effect, sei = sd, data = df2014, method = "REML")
summary(res_2014)

res_2009_rel <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2009, method = "REML")
summary(res_2009_rel)

res_2012_rel <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2012, method = "REML")
summary(res_2012_rel)

res_2014_rel <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2014, method = "REML")
summary(res_2014_rel)

table1<-rbind(cbind(paste0(round(res_2009$beta,3),ifelse(res_2009$pval<0.001,"***",ifelse(res_2009$pval<0.01,"**",ifelse(res_2009$pval<0.05,"*","")))),
                    paste0(round(res_2009_rel$beta*100,3),ifelse(res_2009_rel$pval<0.001,"***",ifelse(res_2009_rel$pval<0.01,"**",ifelse(res_2009_rel$pval<0.05,"*","")))),
                    paste0(round(res_2012$beta,3),ifelse(res_2012$pval<0.001,"***",ifelse(res_2012$pval<0.01,"**",ifelse(res_2012$pval<0.05,"*","")))),
                    paste0(round(res_2012_rel$beta*100,3),ifelse(res_2012_rel$pval<0.001,"***",ifelse(res_2012_rel$pval<0.01,"**",ifelse(res_2012_rel$pval<0.05,"*","")))),
                    paste0(round(res_2014$beta,3),ifelse(res_2014$pval<0.001,"***",ifelse(res_2014$pval<0.01,"**",ifelse(res_2014$pval<0.05,"*","")))),
                    paste0(round(res_2014_rel$beta*100,3),ifelse(res_2014_rel$pval<0.001,"***",ifelse(res_2014_rel$pval<0.01,"**",ifelse(res_2014_rel$pval<0.05,"*",""))))),
              cbind(round(res_2009$se,3),round(res_2009_rel$se,3),
                    round(res_2012$se,3),round(res_2012_rel$se,3),
                    round(res_2014$se,3),round(res_2014_rel$se,3)))
colnames(table1)<-c("2005-2009","2005-2009 rel",
                    "2005-2012","2005-2012 rel",
                    "2005-2014","2005-2014 rel")
rownames(table1)<-c("effect","S.E.")

saveRDS(res_2009, file = paste0(project.dir,"/supplementary_results/fracking_20052009.rds"))
saveRDS(res_2012, file = paste0(project.dir,"/supplementary_results/fracking_20052012.rds"))
saveRDS(res_2014, file = paste0(project.dir,"/supplementary_results/fracking_20052014.rds"))
saveRDS(res_2009_rel, file = paste0(project.dir,"/supplementary_results/fracking_rel_20052009.rds"))
saveRDS(res_2012_rel, file = paste0(project.dir,"/supplementary_results/fracking_rel_20052012.rds"))
saveRDS(res_2014_rel, file = paste0(project.dir,"/supplementary_results/fracking_rel_20052014.rds"))
saveRDS(table1, file = paste0(project.dir,"/supplementary_results/Table_naturalgas.rds"))

increase_ng_2009 <- df2009$State[which(df2009$bi_pval<(0.05/48)&df2009$cum_effect>0)]
increase_ng_2012 <- df2012$State[which(df2012$bi_pval<(0.05/48)&df2012$cum_effect>0)]
increase_ng_2014 <- df2014$State[which(df2014$bi_pval<(0.05/48)&df2014$cum_effect>0)]

fracking <- data.frame(State = df2009$State)

fracking$fracking2009 <- ifelse(df2009$State%in%increase_ng_2009,1,0)
fracking$fracking2012 <- ifelse(df2012$State%in%increase_ng_2012,1,0)
fracking$fracking2014 <- ifelse(df2014$State%in%increase_ng_2014,1,0)

write.table(fracking, "increase_naturalgas.txt")
