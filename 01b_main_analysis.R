################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
# Veronica Ballerini, Marina L. Bottomley, Michelle L. Bell, Francesca Dominici#
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code reproduces Fig. 2, Fig. 3, results of the meta-analysis reported in
## text in the manuscript, Table S1, Table S2 (line "Total") and Fig. S9 in the#
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

# install.packages("devtools")
devtools::install_github("FMenchetti/CausalArima")
library(CausalArima)

# upload modified script to customize plots of the CausalArima package
source("00_modified_plot.R")
source("00_modified_library.R")

# data visualization
source("00_data_visualization.R")

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

per_boxplot_2009<-matrix(NA,48,1000)
per_boxplot_2012<-matrix(NA,48,1000)
per_boxplot_2014<-matrix(NA,48,1000)

summary<-list()

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
  
  summary[[i]]<-impact(ce)
  
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
s_res2009 <- summary(res_2009)

res_2012 <- rma(yi = cum_effect, sei = sd, data = df2012, method = "REML")
s_res2012 <- summary(res_2012)

res_2014 <- rma(yi = cum_effect, sei = sd, data = df2014, method = "REML")
s_res2014 <- summary(res_2014)

res_2009_rel <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2009, method = "REML")
s_res2009_rel <-summary(res_2009_rel)

res_2012_rel <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2012, method = "REML")
s_res2012_rel <- summary(res_2012_rel)

res_2014_rel <- rma(yi = cum_effect_rel, sei = sd_rel, data = df2014, method = "REML")
s_res2014_rel <- summary(res_2014_rel)

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

saveRDS(s_res2009, file = paste0(project.dir,"/main_results/meta_analysis_20052009.rds"))
saveRDS(s_res2012, file = paste0(project.dir,"/main_results/meta_analysis_20052012.rds"))
saveRDS(s_res2014, file = paste0(project.dir,"/main_results/meta_analysis_20052014.rds"))
saveRDS(s_res2009_rel, file = paste0(project.dir,"/main_results/meta_analysis_rel_20052009.rds"))
saveRDS(s_res2012_rel, file = paste0(project.dir,"/main_results/meta_analysis_rel_20052012.rds"))
saveRDS(s_res2014_rel, file = paste0(project.dir,"/main_results/meta_analysis_rel_20052014.rds"))
saveRDS(table1, file = paste0(project.dir,"/tables/TableS2_total.rds"))

### Table for the supplementary material
# Word

make_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "")))
}

# For each year, make columns with stars and actual cell labels (with line breaks)
make_table_columns <- function(df, year) {
  stars_abs <- make_stars(df$bi_pval)
  stars_rel <- make_stars(df$bi_pval)
  
  abs_lab <- paste0(df$cum_effect, stars_abs, "\n(", df$sd, ")")
  rel_lab <- paste0(df$cum_effect_rel, stars_rel, "\n(", df$sd_rel, ")")
  colnames_out <- c(
    paste0("Cum. Effect ", year, " (SE)"),
    paste0("Rel. Cum. Effect ", year, " (SE)")
  )
  out <- data.frame(abs_lab, rel_lab, stringsAsFactors = FALSE)
  colnames(out) <- colnames_out
  out
}

table_2009 <- make_table_columns(df2009, "2009")
table_2012 <- make_table_columns(df2012, "2012")
table_2014 <- make_table_columns(df2014, "2014")

table_full <- data.frame(
  State = df2009$State,
  table_2009,
  table_2012,
  table_2014,
  stringsAsFactors = FALSE
)

ft <- flextable(table_full)
ft <- autofit(ft)

doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "tables/TableS1.docx")

################################# boxplots
states <- rbind(matrix(df2009$State,ncol=1),"U.S.")
df_boxplot<- data.frame(States = rep(states,3), 
                        Period = rep(c("2005-2009","2005-2012","2005-2014"),each=49),
                        rel_effect = rbind(rbind(as.matrix(df2009$cum_effect_rel,ncol=1),as.numeric(res_2009_rel$beta)),
                                           rbind(as.matrix(df2012$cum_effect_rel,ncol=1),as.numeric(res_2012_rel$beta)),
                                           rbind(as.matrix(df2014$cum_effect_rel,ncol=1),as.numeric(res_2014_rel$beta))),
                        sd_rel_effect = rbind(rbind(as.matrix(df2009$sd_rel,ncol=1),as.numeric(res_2009_rel$se)),
                                           rbind(as.matrix(df2012$sd_rel,ncol=1),as.numeric(res_2012_rel$se)),
                                           rbind(as.matrix(df2014$sd_rel,ncol=1),as.numeric(res_2014_rel$se))))


summary_df <- df_boxplot %>%
  rename(State = States, 
         Year = Period, 
         mean = rel_effect, 
         se = sd_rel_effect) %>%
  mutate(
    lower = mean - 1.96 * se,
    upper = mean + 1.96 * se
  )

state_order <- summary_df %>%
  filter(Year == "2005-2012") %>%
  arrange(mean) %>%
  mutate(is_last = State == "U.S.") %>%
  arrange(is_last) %>%
  pull(State)

summary_df$State <- factor(summary_df$State, levels = state_order)

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
               "#CC79A7", "#000000")

jpeg(file = paste0(project.dir,"/plots/Fig3.jpeg"),
     width = 10470, height = 4590, units = "px", res = 1000)
ggplot(summary_df, aes(x = State, y = mean, color = Year, group = Year)) +
  geom_point(position = position_dodge(width = 0.5), size = 1.8) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "",
       y = expression(paste(Delta,"%")),
       color = "Evaluation period") +
  scale_color_manual(values = okabe_ito[1:length(unique(summary_df$Year))]) +
  scale_y_continuous(labels = percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      size  = 12   # bigger x labels
    ),
    axis.text.y = element_text(
      size = 12    # bigger y labels
    ),
    axis.title.y = element_text(size = 14),   # x‑axis label
    legend.position = "bottom"
  )
dev.off()

### Goodness of fit metrics (Fig. S9)

MPE <- NULL
for(i in 1:48){
  MPE<-cbind(MPE,summary[[i]]$arima$accuracy[4])
}

MASE <- NULL
for(i in 1:48){
  MASE<-cbind(MASE,summary[[i]]$arima$accuracy[6])
}

ACF1 <- NULL
for(i in 1:48){
  ACF1<-cbind(ACF1,summary[[i]]$arima$accuracy[7])
}

df_summaries <- data.frame(
  MPE  = as.numeric(MPE),
  MASE = as.numeric(MASE),
  ACF1 = as.numeric(ACF1)
)

df_long <- df_summaries |>
  pivot_longer(
    cols = everything(),
    names_to = "Metric",
    values_to = "Value"
  )

df_long$Metrica <- factor(
  df_long$Metrica,
  levels = c("MPE", "MASE", "ACF1")
)

jpeg(file = paste0(project.dir,"/plots/FigS9.jpeg"),
     width = 10470, height = 4590, units = "px", res = 1000)
ggplot(df_long, aes(x = Metric, y = Value)) +
  geom_boxplot() +
  geom_hline(yintercept = 1, color = "gray", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  theme_bw() +
  theme(
    axis.text.x = element_text(hjust = 1)  
  ) +
  labs(x = NULL, y = NULL)
dev.off()

### Save data
save.image("main_analysis.RData")

