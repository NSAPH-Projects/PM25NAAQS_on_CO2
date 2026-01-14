################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code reproduces results in Table S9 (line "Precipitation") in the ######
## Supplementary Materials. ####################################################
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
effect_avg_0509<-NULL
effect_avg_0512<-NULL
effect_avg_0514<-NULL

rowtable_names<-NULL

# boot<-list()

per_boxplot_2009<-matrix(NA,48,1000)
per_boxplot_2012<-matrix(NA,48,1000)
per_boxplot_2014<-matrix(NA,48,1000)

i<-NULL

for(s in unique(final_data$State)){
  i<-sum(i,1)
  data<-final_data[which(final_data$State==s),]
  ce <- CausalArima(y = ts(log(data$Precipitation), start = 1, frequency = 1), 
                    dates = dates, 
                    int.date = int.date, nboot = 1000)
  
  ce_avg_0509 <- mean(ce$causal.effect[4:8])
  counter_0505 <- mean(ce$forecast[4:8])
  ce_avg_0509_rel <- ce_avg_0509/counter_0505
  
  ce_avg_0512 <- mean(ce$causal.effect[4:11])
  counter_0512 <- mean(ce$forecast[4:11])
  ce_avg_0512_rel <- ce_avg_0512/counter_0512
  
  ce_avg_0514 <- mean(ce$causal.effect[4:13])
  counter_0514 <- mean(ce$forecast[4:13])
  ce_avg_0514_rel <- ce_avg_0514/counter_0514
  
  avg_distr_0509 <- colMeans(data$Precipitation[data$Year>=2005&data$Year<=2009] - 
                               ce$boot$boot.distrib[4:8,])
  avg_distr_0509_rel <- avg_distr_0509/apply(ce$boot$boot.distrib[4:8,],2,mean)
  ce_avg_0509_int <- quantile(avg_distr_0509,c(0.025,0.975))
  ce_avg_0509_int_rel <- quantile(avg_distr_0509_rel,c(0.025,0.975))
  ce_avg_0509_sd <- sd(avg_distr_0509)
  ce_avg_0509_rel_sd <- sd(avg_distr_0509_rel)
  pval_0509 <- mean(avg_distr_0509>=0)
  bi_pval_0509 <- 2-2*max(mean(avg_distr_0509 < 0), mean(avg_distr_0509 > 0))
  
  avg_distr_0512 <- colMeans(data$Precipitation[data$Year>=2005&data$Year<=2012] - 
                               ce$boot$boot.distrib[4:11,])
  avg_distr_0512_rel <- avg_distr_0512/apply(ce$boot$boot.distrib[4:11,],2,mean)
  ce_avg_0512_int <- quantile(avg_distr_0512,c(0.025,0.975))
  ce_avg_0512_int_rel <- quantile(avg_distr_0512_rel,c(0.025,0.975))
  ce_avg_0512_sd <- sd(avg_distr_0512)
  ce_avg_0512_rel_sd <- sd(avg_distr_0512_rel)
  pval_0512 <- mean(avg_distr_0512>=0)
  bi_pval_0512 <- 2-2*max(mean(avg_distr_0512 < 0), mean(avg_distr_0512 > 0))
  
  avg_distr_0514 <- colMeans(data$Precipitation[data$Year>=2005&data$Year<=2014] - 
                               ce$boot$boot.distrib[4:13,])
  avg_distr_0514_rel <- avg_distr_0514/apply(ce$boot$boot.distrib[4:13,],2,mean)
  ce_avg_0514_int <- quantile(avg_distr_0514,c(0.025,0.975))
  ce_avg_0514_int_rel <- quantile(avg_distr_0514_rel,c(0.025,0.975))
  ce_avg_0514_sd <- sd(avg_distr_0514)
  ce_avg_0514_rel_sd <- sd(avg_distr_0514_rel)
  pval_0514 <- mean(avg_distr_0514>=0)
  bi_pval_0514 <- 2-2*max(mean(avg_distr_0514 < 0), mean(avg_distr_0514 > 0))
  
  effect_avg_0509<-rbind(effect_avg_0509,
                         cbind(ce_avg_0509,
                               counter_0505,
                               ce_avg_0509_rel,
                               t(ce_avg_0509_int),
                               t(ce_avg_0509_int_rel),
                               ce_avg_0509_sd,
                               ce_avg_0509_rel_sd,
                               pval_0509,
                               bi_pval_0509))
  
  effect_avg_0512<-rbind(effect_avg_0512,
                         cbind(ce_avg_0512,
                               counter_0512,
                               ce_avg_0512_rel,
                               t(ce_avg_0512_int),
                               t(ce_avg_0512_int_rel),
                               ce_avg_0512_sd,
                               ce_avg_0512_rel_sd,
                               pval_0512,
                               bi_pval_0512))
  
  effect_avg_0514<-rbind(effect_avg_0514,
                         cbind(ce_avg_0514,
                               counter_0514,
                               ce_avg_0514_rel,
                               t(ce_avg_0514_int),
                               t(ce_avg_0514_int_rel),
                               ce_avg_0514_sd,
                               ce_avg_0514_rel_sd,
                               pval_0514,
                               bi_pval_0514))
  
  per_boxplot_2009[i,]<-avg_distr_0509/apply(ce$boot$boot.distrib[4:8,],2,mean)
  per_boxplot_2012[i,]<-avg_distr_0512/apply(ce$boot$boot.distrib[4:11,],2,mean)
  per_boxplot_2014[i,]<-avg_distr_0514/apply(ce$boot$boot.distrib[4:13,],2,mean)
  
  rowtable_names<-rbind(rowtable_names,s)
  
}

################################ 2009 ##########################################
df2009<-data.frame(State=rowtable_names,
                   avg_effect=round(effect_avg_0509[,1],3),
                   counter=round(effect_avg_0509[,2],3),
                   avg_effect_rel = round(effect_avg_0509[,3],3),
                   abs_lb=round(effect_avg_0509[,4],3),
                   abs_up=round(effect_avg_0509[,5],3),
                   abs_lb_rel=round(effect_avg_0509[,6],3),
                   abs_up_rel=round(effect_avg_0509[,7],3),
                   sd=round(effect_avg_0509[,8],3),
                   sd_rel=round(effect_avg_0509[,9],3),
                   pval=effect_avg_0509[,10],
                   bi_pval=effect_avg_0509[,11]
)

df2012<-data.frame(State=rowtable_names,
                   avg_effect=round(effect_avg_0512[,1],3),
                   counter=round(effect_avg_0512[,2],3),
                   avg_effect_rel = round(effect_avg_0512[,3],5),
                   abs_lb=round(effect_avg_0512[,4],3),
                   abs_up=round(effect_avg_0512[,5],3),
                   abs_lb_rel=round(effect_avg_0512[,6],3),
                   abs_up_rel=round(effect_avg_0512[,7],3),
                   sd=round(effect_avg_0512[,8],3),
                   sd_rel=round(effect_avg_0512[,9],3),
                   pval=effect_avg_0512[,10],
                   bi_pval=effect_avg_0512[,11]
)

df2014<-data.frame(State=rowtable_names,
                   avg_effect=round(effect_avg_0514[,1],3),
                   counter=round(effect_avg_0514[,2],3),
                   avg_effect_rel = round(effect_avg_0514[,3],3),
                   abs_lb=round(effect_avg_0514[,4],3),
                   abs_up=round(effect_avg_0514[,5],3),
                   abs_lb_rel=round(effect_avg_0514[,6],3),
                   abs_up_rel=round(effect_avg_0514[,7],3),
                   sd=round(effect_avg_0514[,8],3),
                   sd_rel=round(effect_avg_0514[,9],3),
                   pval=effect_avg_0514[,10],
                   bi_pval=effect_avg_0514[,11]
)

library(metafor)

res_2009 <- rma(yi = avg_effect, sei = sd, data = df2009)
summary(res_2009)

res_2012 <- rma(yi = avg_effect, sei = sd, data = df2012)
summary(res_2012)

res_2014 <- rma(yi = avg_effect, sei = sd, data = df2014)
summary(res_2014)

res_2009_rel <- rma(yi = avg_effect_rel, sei = sd_rel, data = df2009)
summary(res_2009_rel)

res_2012_rel <- rma(yi = avg_effect_rel, sei = sd_rel, data = df2012)
summary(res_2012_rel)

res_2014_rel <- rma(yi = avg_effect_rel, sei = sd_rel, data = df2014)
summary(res_2014_rel)

tableS9<-rbind(cbind(paste0(round(res_2009$beta,3),ifelse(res_2009$pval<0.001,"***",ifelse(res_2009$pval<0.01,"**",ifelse(res_2009$pval<0.05,"*","")))),
                    paste0(round(res_2009_rel$beta*100,3),ifelse(res_2009_rel$pval<0.001,"***",ifelse(res_2009_rel$pval<0.01,"**",ifelse(res_2009_rel$pval<0.05,"*","")))),
                    paste0(round(res_2012$beta,3),ifelse(res_2012$pval<0.001,"***",ifelse(res_2012$pval<0.01,"**",ifelse(res_2012$pval<0.05,"*","")))),
                    paste0(round(res_2012_rel$beta*100,3),ifelse(res_2012_rel$pval<0.001,"***",ifelse(res_2012_rel$pval<0.01,"**",ifelse(res_2012_rel$pval<0.05,"*","")))),
                    paste0(round(res_2014$beta,3),ifelse(res_2014$pval<0.001,"***",ifelse(res_2014$pval<0.01,"**",ifelse(res_2014$pval<0.05,"*","")))),
                    paste0(round(res_2014_rel$beta*100,3),ifelse(res_2014_rel$pval<0.001,"***",ifelse(res_2014_rel$pval<0.01,"**",ifelse(res_2014_rel$pval<0.05,"*",""))))),
              cbind(round(res_2009$se,3),round(res_2009_rel$se,3),
                    round(res_2012$se,3),round(res_2012_rel$se,3),
                    round(res_2014$se,3),round(res_2014_rel$se,3)))
colnames(tableS9)<-c("2005-2009","2005-2009 rel",
                    "2005-2012","2005-2012 rel",
                    "2005-2014","2005-2014 rel")
rownames(tableS9)<-c("effect","S.E.")

saveRDS(res_2009, file = paste0(project.dir,"/supplementary_results/precipitation_20052009.rds"))
saveRDS(res_2012, file = paste0(project.dir,"/supplementary_results/precipitation_20052012.rds"))
saveRDS(res_2014, file = paste0(project.dir,"/supplementary_results/precipitation_20052014.rds"))
saveRDS(res_2009_rel, file = paste0(project.dir,"/supplementary_results/precipitation_rel_20052009.rds"))
saveRDS(res_2012_rel, file = paste0(project.dir,"/supplementary_results/precipitation_rel_20052012.rds"))
saveRDS(res_2014_rel, file = paste0(project.dir,"/supplementary_results/precipitation_rel_20052014.rds"))
saveRDS(tableS9, file = paste0(project.dir,"/tables/TableS9_Precipitation.rds"))
