################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
## This code reproduces Fig. 4 and Fig. S7 (Supplementary Materials). ##########
################################################################################

# load table
table <- read_excel("Science_Table.xlsx")

table$classification <- ifelse(table$Total_meeting_criteria > 0, 1, 0)
table(table$classification)
table$...22 <- NULL
table <- table[-49,]

colnames(table) <- c(
  "State", "code", "Total_concurrent",
  "Electric_power","Transportation",
  "Industrial","Residential","Commercial",
  "Electric_power_pc","Transportation_pc",
  "Industrial_pc","Residential_pc","Commercial_pc",
  "Yes","No",
  "federal_state_acts","house_senate_bills","tax_incentives",
  "grants","state_codes_amendment","executive_orders",
  "classification"
)

head(table)

# descriptive
add_policies <- apply(table[,c(3:22)],2,sum)
saveRDS(add_policies, file = paste0(project.dir,"/main_results/add_policies.rds"))

###
table_long_sectors_pc <- table %>%
  # 1. Sort by Yes, then by No
  arrange(Electric_power, Industrial, Transportation, Residential, Commercial) %>%
  # 2. Lock in this order for code
  mutate(code = factor(code, levels = unique(code))) %>%
  # 3. Reshape for plotting
  select(code, Electric_power, Industrial, Transportation, Residential, Commercial) %>%
  pivot_longer(
    cols = c(Electric_power, Industrial, Transportation, Residential, Commercial),
    names_to = "type",
    values_to = "value"
  ) %>%
  # optional: control legend/order
  mutate(type = factor(type, levels = c("Commercial", "Residential", "Transportation", "Industrial", "Electric_power")))

table_long_sectors_n <- table %>%
  # 1. Sort by Yes, then by No
  arrange(Electric_power, Industrial, Transportation, Residential, Commercial) %>%
  # 2. Lock in this order for code
  mutate(code = factor(code, levels = unique(code))) %>%
  # 3. Reshape for plotting
  select(code, Electric_power, Industrial, Transportation, Residential, Commercial) %>%
  pivot_longer(
    cols = c(Electric_power, Industrial, Transportation, Residential, Commercial),
    names_to = "type",
    values_to = "value"
  ) %>%
  # optional: control legend/order
  mutate(type = factor(type, levels = c("Commercial", "Residential", "Transportation", "Industrial", "Electric_power"))) 

jpeg(file = paste0(project.dir,"/plots/Fig4.jpeg"),
     width = 10470, height = 4590, units = "px", res = 1000)
ggplot(table_long_sectors_n, aes(x = code, y = value, fill = type)) +
  geom_col() +
  scale_fill_manual(
    name   = "Sector",
    values = c("Electric_power" = "#EB713F",   
               "Industrial"  = "#74c476",
               "Transportation" = "#005a32",
               "Residential" = "#3BBFE3",
               "Commercial" = "#162B45")   
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    minor_breaks = NULL
  ) +
  labs(
    x = "",
    y = "Number of examined policies"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.title.y = element_text(size = 14),   # x‑axis label
        legend.text  = element_text(size = 12),  # optional: legend text
        legend.position = "bottom"
  )
dev.off()


### map
#-----------------------------------
# 1. Prepare table and policy_group
#-----------------------------------
table_map <- table %>%
  mutate(
    policy_group = if_else(Yes > 0,
                           "Alternative policies meeting criteria",
                           "No alternative policies meeting criteria"),
    state = State   # full state name, as required by usmap joins
  ) %>%
  filter(!is.na(policy_group))   # remove NA categories

#-----------------------------------
# 2. Get centroids for labels (from all states, then drop AK/HI)
#-----------------------------------
states_sf_all <- usmap::us_map(regions = "states")  # sf object

centroids_sf <- sf::st_centroid(states_sf_all)

centroids <- centroids_sf %>%
  cbind(sf::st_coordinates(.)) %>%  # adds X, Y
  as.data.frame() %>%
  select(abbr, X, Y) %>%
  rename(x = X, y = Y) %>%
  filter(!abbr %in% c("AK", "HI"))  # remove Alaska and Hawaii

label_data <- centroids %>%
  left_join(table_map, by = c("abbr" = "code"))

# Make sure policy_group has only the two levels you want
table_map <- table_map %>%
  filter(!is.na(policy_group)) %>%
  mutate(policy_group = factor(
    policy_group,
    levels = c("Alternative policies meeting criteria",
               "No alternative policies meeting criteria")
  ))

# Nudge labels for small NE states
label_data_adj <- label_data %>%
  mutate(
    x = case_when(
      abbr == "RI" ~ x + 150000,
      abbr == "CT" ~ x + 120000,
      abbr == "MA" ~ x + 150000,
      abbr == "NJ" ~ x + 120000,
      abbr == "DE" ~ x + 120000,
      abbr == "MD" ~ x +  80000,
      TRUE         ~ x
    ),
    y = case_when(
      abbr == "RI" ~ y + 100000,
      abbr == "CT" ~ y +  80000,
      abbr == "MA" ~ y + 120000,
      abbr == "NJ" ~ y -  80000,
      abbr == "DE" ~ y -  80000,
      abbr == "MD" ~ y -  80000,
      TRUE         ~ y
    )
  )

#-----------------------------------
# 3. Build sf map without AK/HI
#-----------------------------------
states_sf <- states_sf_all %>%
  filter(!abbr %in% c("AK", "HI")) %>%          # contiguous 48 only
  left_join(table_map, by = c("abbr" = "code"))  # add policy_group etc.

#-----------------------------------
# 4. Plot and save
#-----------------------------------
p <- ggplot() +
  geom_sf(
    data = states_sf,
    aes(fill = policy_group),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    name   = "",
    values = c(
      "Alternative policies meeting criteria"     = "#74c476",
      "No alternative policies meeting criteria"  = "#d95f02"
    ),
    na.translate = FALSE
  ) +
  geom_label(
    data = label_data_adj,
    aes(x = x, y = y, label = Total_concurrent),
    size       = 3.5,
    color      = "black",
    fill       = "white",
    label.size = 0.2,
    alpha      = 0.8
  ) +
  labs(
    title    = "US States by Presence of Alternative Environmental Policies Meeting Inclusion Criteria",
    subtitle = "Number inside each state = Examined alternative policies",
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text       = element_blank(),
    panel.grid      = element_blank()
  ) +
  coord_sf(expand = FALSE)  # removes extra whitespace around the map

jpeg(file = paste0(project.dir,"/plots/FigS7.jpeg"),
     width = 10470, height = 4590, units = "px", res = 1000)
print(p)
dev.off()
