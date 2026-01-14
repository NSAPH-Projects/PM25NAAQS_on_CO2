################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code reproduces Fig. 5 and meta-regression results' reported in text. ##
################################################################################

# 1. Read data
Regulations <- read.csv("StateBindingClass.csv", sep = ";") 

# load table
table <- read_excel("Science_Table.xlsx")

table$classification <- ifelse(table$Total_meeting_criteria > 0, 1, 0)
table(table$classification)
table$...22 <- NULL
table <- table[-49,]

colnames(table) <- c(
  "State_full", "State", "Total_concurrent",
  "Electric_power_n","Transportation_n",
  "Industrial_n","Residential_n","Commercial_n",
  "Electric_power","Transportation",
  "Industrial","Residential","Commercial",
  "Yes","No",
  "federal_state_acts","house_senate_bills","tax_incentives",
  "grants","state_codes_amendment","executive_orders",
  "classification"
)

head(table)

Regulations<-left_join(Regulations,table[,c("State","Yes","Total_concurrent")],by="State")
Regulations$label <- ifelse(Regulations$Yes==0,Regulations$State,paste(Regulations$State, " (", Regulations$Yes, ")", sep = ""))

# 2. Add year and combine state results for different time frames
df2009$Year <- "2005-2009"
df2012$Year <- "2005-2012"
df2014$Year <- "2005-2014"
df_reg <- rbind(df2009, df2012, df2014)

# 3. Join state-level meta-data
df_regulation <- left_join(df_reg, Regulations[, c(1,2,9)], by = "State")
df_regulation$Binding <- as.factor(df_regulation$Binding)
levels(df_regulation$Binding) <- c("Unconfounded states", "Confounded states")

# --- META-ANALYSIS FOR EACH PERIOD ---

reg_2009 <- rma(yi = cum_effect_rel, sei = sd_rel,  method = "REML", mods = ~ as.factor(Binding), 
                data = filter(df_regulation, Year == "2005-2009"))
s_reg2009 <- summary(reg_2009)

reg_2012 <- rma(yi = cum_effect_rel, sei = sd_rel,  method = "REML", mods = ~ as.factor(Binding), 
                data = filter(df_regulation, Year == "2005-2012"))
s_reg2012 <- summary(reg_2012)

reg_2014 <- rma(yi = cum_effect_rel, sei = sd_rel,  method = "REML", mods = ~ as.factor(Binding), 
                data = filter(df_regulation, Year == "2005-2014"))
s_reg2014 <- summary(reg_2014)

saveRDS(s_reg2009, file = paste0(project.dir,"/main_results/meta_analysis_moderator_20052009.rds"))
saveRDS(s_reg2012, file = paste0(project.dir,"/main_results/meta_analysis_moderator_20052012.rds"))
saveRDS(s_reg2014, file = paste0(project.dir,"/main_results/meta_analysis_moderator_20052014.rds"))

# --- EXTRACT META-ANALYTIC GROUP MEANS ---

extract_group_meta <- function(m.rma, year_label) {
  cf <- coef(summary(m.rma))
  eff0 <- cf["intrcpt", "estimate"]
  se0  <- cf["intrcpt", "se"]
  ci0  <- c(cf["intrcpt", "ci.lb"], cf["intrcpt", "ci.ub"])
  eff1 <- eff0 + cf[grep("Binding", rownames(cf)), "estimate"]
  # This is a conservative (over-)estimate of SE; for display only
  se1  <- sqrt(se0^2 + cf[grep("Binding", rownames(cf)), "se"]^2)
  ci1  <- c(
    cf["intrcpt", "estimate"] + cf[grep("Binding", rownames(cf)), "ci.lb"],
    cf["intrcpt", "estimate"] + cf[grep("Binding", rownames(cf)), "ci.ub"]
  )
  data.frame(
    State = c("Group mean1","Group mean2"),
    Binding = c("Unconfounded states", "Confounded states"),
    mean = c(eff0, eff1),
    lower = c(ci0[1], ci1[1]),
    upper = c(ci0[2], ci1[2]),
    Year = year_label,
    stringsAsFactors = FALSE
  )
}

meta_points <- bind_rows(
  extract_group_meta(reg_2009, "2005-2009"),
  extract_group_meta(reg_2012, "2005-2012"),
  extract_group_meta(reg_2014, "2005-2014")
)

# --- CONSTRUCT PLOT DATASET ---

# Prepare state-level summary for plotting
summary_df <- df_regulation %>%
  mutate(
    lower = cum_effect_rel - 1.96 * sd_rel,
    upper = cum_effect_rel + 1.96 * sd_rel
  ) %>%
  select(State, mean = cum_effect_rel, lower, upper, Year, Binding, label)

# Bind meta-analytic group-level means as if they were states
plot_data <- summary_df %>%
  filter(State != "U.S.") %>% # Remove US if present (optional)
  bind_rows(meta_points) %>%
  mutate(Binding = factor(Binding, levels = c("Unconfounded states","Confounded states")))

# --- ORDINAMENTO STATE PER GRUPPO SULLA BASE DEL 2005-2012 ---

# Calcola l'ordine desiderato per ciascun gruppo
state_order_table <- plot_data %>%
  filter(Year == "2005-2012", State != "Average effects") %>%
  group_by(Binding) %>%
  arrange(mean) %>%
  summarise(state_levels = list(c(unique(State), "Average effects")), .groups = "drop")

plot_data <- plot_data %>%
  left_join(state_order_table, by = "Binding") %>%
  group_by(Binding) %>%
  mutate(State_ord = factor(State, levels = state_levels[[1]])) %>%
  ungroup() %>%
  select(-state_levels)

plot_data$panel_style <- ifelse(
  plot_data$Binding == "Unconfounded states",
  "Unconfounded states",
  "Confounded states"
)

# --- CREATE A SEPARATE THIRD PANEL FOR GROUP MEANS ---

# create a panel variable with 3 panels
plot_data$Binding_panel <- ifelse(
  plot_data$State %in% c("Group mean1","Group mean2"),
  "Average effects",
  as.character(plot_data$Binding)
)

plot_data$Binding_panel <- factor(
  plot_data$Binding_panel,
  levels = c(
    "Unconfounded states",
    "Confounded states",
    "Average effects"
  )
)


# 1. Determine state order based on year 2005–2012 (no group means)
state_order_table <- plot_data %>%
  filter(Year == "2005-2012",
         !(State %in% c("Group mean1","Group mean2"))) %>%
  group_by(Binding) %>%
  arrange(mean) %>%
  summarise(state_levels = list(unique(State)), .groups = "drop")

# 2. Apply ordering only to real states
plot_data <- plot_data %>%
  left_join(state_order_table, by = "Binding") %>%
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

# --- PLOT ---

# Build name → label lookup
lab_map <- Regulations$label
names(lab_map) <- Regulations$State

jpeg(file = paste0(project.dir,"/plots/Fig5.jpeg"),
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
    name = "Alternative policies",
    values = c(
      "Unconfounded states" = 15,  # square
      "Confounded states" = 16          # dot
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
    ~ Binding_panel,
    scales = "free_x",
    space = "free",
    shrink = TRUE,
  )+
  facetted_pos_scales(
    x = list(
      # Panel 1
      scale_x_discrete(
        labels = function(x) {
          # Replace state codes with pretty labels, keep others as-is
          ifelse(x %in% names(lab_map), lab_map[x], x)
        }
      ),
      # Panel 2
      scale_x_discrete(
        labels = function(x) {
          ifelse(x %in% names(lab_map), lab_map[x], x)
        }
      ),
      # Panel 3: no labels for group means
      scale_x_discrete(labels = NULL)
    )
  )
dev.off()

