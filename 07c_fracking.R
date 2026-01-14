################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code reproduces Fig. S8 and in text results of the section "Fracking" in
## the Supplementary Materials. ################################################
################################################################################

rm(list = ls())

# set the seed
set.seed(42)

# set the working directory
project.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(project.dir)

# 1. Read data
fracking<-read.table("increase_naturalgas.txt")
load(paste(project.dir,"/main_analysis.RData",sep=""))

# 2. Meta-analysis for each period

df2009$fracking<-ifelse(df2009$State%in%fracking$State[which(fracking$fracking2009==1)],1,0)
df2012$fracking<-ifelse(df2012$State%in%fracking$State[which(fracking$fracking2012==1)],1,0)
df2014$fracking<-ifelse(df2014$State%in%fracking$State[which(fracking$fracking2014==1)],1,0)

reg_2009_fr <- rma(yi = cum_effect, sei = sd,  method = "REML", mods = ~ as.factor(fracking), 
                   data = df2009)
s_reg2009_fr <- summary(reg_2009_fr)

reg_2009_rel_fr <- rma(yi = cum_effect_rel, sei = sd_rel,  method = "REML", mods = ~ as.factor(fracking), 
                   data = df2009)
s_reg2009_rel_fr <- summary(reg_2009_rel_fr)

reg_2012_fr <- rma(yi = cum_effect, sei = sd,  method = "REML", mods = ~ as.factor(fracking), 
                   data = df2012)
s_reg2012_fr <- summary(reg_2012_fr)

reg_2012_rel_fr <- rma(yi = cum_effect_rel, sei = sd_rel,  method = "REML", mods = ~ as.factor(fracking), 
                       data = df2012)
s_reg2012_rel_fr <- summary(reg_2012_rel_fr)

reg_2014_fr <- rma(yi = cum_effect, sei = sd,  method = "REML", mods = ~ as.factor(fracking), 
                   data = df2014)
s_reg2014_fr <- summary(reg_2014_fr)

reg_2014_rel_fr <- rma(yi = cum_effect_rel, sei = sd_rel,  method = "REML", mods = ~ as.factor(fracking), 
                       data = df2014)
s_reg2014_rel_fr <- summary(reg_2014_rel_fr)

saveRDS(s_res2009, file = paste0(project.dir,"/main_results/meta_analysis_20052009_fracking.rds"))
saveRDS(s_res2012, file = paste0(project.dir,"/main_results/meta_analysis_20052012_fracking.rds"))
saveRDS(s_res2014, file = paste0(project.dir,"/main_results/meta_analysis_20052014_fracking.rds"))
saveRDS(s_res2009_rel, file = paste0(project.dir,"/main_results/meta_analysis_rel_20052009_fracking.rds"))
saveRDS(s_res2012_rel, file = paste0(project.dir,"/main_results/meta_analysis_rel_20052012_fracking.rds"))
saveRDS(s_res2014_rel, file = paste0(project.dir,"/main_results/meta_analysis_rel_20052014_fracking.rds"))

# 3. Add year and combine state results for different time frames
df2009$Year <- "2005-2009"
df2012$Year <- "2005-2012"
df2014$Year <- "2005-2014"
df_reg <- rbind(df2009, df2012, df2014)

# 4. Join state-level meta-data
df_fracking <- left_join(df_reg, fracking[, c(1,4)], by = "State")
df_fracking$fracking <- as.factor(df_fracking$fracking2014)
levels(df_fracking$fracking) <- c("No effect", "Significant effect")

# 5. Extract group means

extract_group_meta <- function(m.rma, year_label) {
  cf <- coef(summary(m.rma))
  eff0 <- cf["intrcpt", "estimate"]
  se0  <- cf["intrcpt", "se"]
  ci0  <- c(cf["intrcpt", "ci.lb"], cf["intrcpt", "ci.ub"])
  eff1 <- eff0 + cf[grep("fracking", rownames(cf)), "estimate"]
  # This is a conservative (over-)estimate of SE; for display only
  se1  <- sqrt(se0^2 + cf[grep("fracking", rownames(cf)), "se"]^2)
  ci1  <- c(
    cf["intrcpt", "estimate"] + cf[grep("fracking", rownames(cf)), "ci.lb"],
    cf["intrcpt", "estimate"] + cf[grep("fracking", rownames(cf)), "ci.ub"]
  )
  data.frame(
    State = c("Group mean1","Group mean2"),
    fracking = c("No effect", "Significant effect"),
    mean = c(eff0, eff1),
    lower = c(ci0[1], ci1[1]),
    upper = c(ci0[2], ci1[2]),
    Year = year_label,
    stringsAsFactors = FALSE
  )
}

meta_points <- bind_rows(
  extract_group_meta(reg_2009_fr, "2005-2009"),
  extract_group_meta(reg_2012_fr, "2005-2012"),
  extract_group_meta(reg_2014_fr, "2005-2014")
)

# 6. Construct plot dataset

# Prepare state-level summary for plotting
summary_df <- df_fracking %>%
  mutate(
    lower = cum_effect_rel - 1.96 * sd_rel,
    upper = cum_effect_rel + 1.96 * sd_rel
  ) %>%
  select(State, mean = cum_effect_rel, lower, upper, Year, fracking)

# Bind meta-analytic group-level means as if they were states
plot_data <- summary_df %>%
  filter(State != "U.S.") %>% # Remove US if present (optional)
  bind_rows(meta_points) %>%
  mutate(fracking = factor(fracking, levels = c("No effect","Significant effect")))

# 7. Sorting States (effect 2005-2012)

state_order_table <- plot_data %>%
  filter(Year == "2005-2012", State != "Average effects") %>%
  group_by(fracking) %>%
  arrange(mean) %>%
  summarise(state_levels = list(c(unique(State), "Average effects")), .groups = "drop")

plot_data <- plot_data %>%
  left_join(state_order_table, by = "fracking") %>%
  group_by(fracking) %>%
  mutate(State_ord = factor(State, levels = state_levels[[1]])) %>%
  ungroup() %>%
  select(-state_levels)

plot_data$panel_style <- ifelse(
  plot_data$fracking == "No effect",
  "No effect",
  "Significant effect"
)

# 8. Create a separate third panel for group means

# create a panel variable with 3 panels
plot_data$fracking_panel <- ifelse(
  plot_data$State %in% c("Group mean1","Group mean2"),
  "Average effects",
  as.character(plot_data$fracking)
)

plot_data$fracking_panel <- factor(
  plot_data$fracking_panel,
  levels = c(
    "No effect",
    "Significant effect",
    "Average effects"
  )
)

# 9. Determine state order based on year 2005–2012 (no group means)
state_order_table <- plot_data %>%
  filter(Year == "2005-2012",
         !(State %in% c("Group mean1","Group mean2"))) %>%
  group_by(fracking) %>%
  arrange(mean) %>%
  summarise(state_levels = list(unique(State)), .groups = "drop")

# 10. Apply ordering only to real states
plot_data <- plot_data %>%
  left_join(state_order_table, by = "fracking") %>%
  mutate(
    State_ord =
      ifelse(State %in% c("Group mean1","Group mean2"),
             paste(State, Year),   # keep group means separate
             State),
    
    State_ord = factor(
      State_ord,
      levels = c(
        # State ordering for panel 1 & 2
        unlist(state_order_table$state_levels),
        # Group mean ordering for panel 3
        "Group mean1 2005-2009",
        "Group mean2 2005-2009",
        "Group mean1 2005-2012",
        "Group mean2 2005-2012",
        "Group mean1 2005-2014",
        "Group mean2 2005-2014"
      )
    )
  ) %>%
  select(-state_levels)

# 11. Plot
jpeg(file = paste0(project.dir,"/plots/FigS8.jpeg"),
     width = 12470, height = 4590, units = "px", res = 1000)
ggplot(plot_data, aes(x = State_ord, y = mean, color = Year, group = Year)) +
  geom_point(
    aes(shape = panel_style),
    position = position_dodge(width = 0.5),
    size = 1.8
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper, linetype = panel_style),
    width = 0.25,
    position = position_dodge(width = 0.5),
    linewidth = 0.4,
    show.legend = FALSE     # <--- fixes the mismatched legend
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_shape_manual(
    name = "",
    values = c(
      "No effect" = 15,  # square
      "Significant effect" = 16          # dot
    )
  ) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "",
    y = expression(paste(Delta,"%")),
    color = "Evaluation period"
  ) +
  scale_color_manual(values = okabe_ito[1:length(unique(summary_df$Year))]) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust  = 1,
      size  = 12   # bigger x labels
    ),
    axis.text.y = element_text(size = 12),  # optional: bigger y labels too
    strip.text.x = element_text(size = 11), # optional: bigger facet titles
    legend.text  = element_text(size = 9),  # optional: legend text
    legend.title = element_text(size = 10),
    axis.title.y = element_text(size = 14),   # x‑axis label
    legend.position = "bottom",
    legend.box = "vertical",          # <- stack legends vertically
    # ggh4x.facet = list(
    #   wrap = list(widths = c(1, 1, 0.4))
    # )
  )+
  facet_grid2(
    ~ fracking_panel,
    scales = "free_x",
    space = "free",
    shrink = TRUE,
  )+
  facetted_pos_scales(
    x = list(
      # Panel 1
      scale_x_discrete(
        labels = function(x) {x}
      ),
      # Panel 2
      scale_x_discrete(
        labels = function(x) {x}
      ),
      # Panel 3: no labels for group means
      scale_x_discrete(labels = NULL)
    )
  )
dev.off()
