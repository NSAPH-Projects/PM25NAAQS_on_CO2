################################################################################
## The impact of PM2.5 National Ambient Air Quality Standards on CO2 emissions #
## Veronica Ballerini, Marina Bottomley, Michelle L. Bell, Francesca Dominici ##
################### Code author: Veronica Ballerini ############################
################### Last modified: January 10, 2026 ############################
######## This code reproduces Figure S1 in the Supplementary Materials. ########
################################################################################

# 1) longitudinal data

q1 <- final_data %>%
  filter(Year == 2005, CO2 < 50) %>%
  pull(State)
q2 <- final_data %>%
  filter(Year == 2005, CO2 >= 50 & CO2 < 100) %>%
  pull(State)
q3 <- final_data %>%
  filter(Year == 2005, CO2 >= 100 & CO2 < 200) %>%
  pull(State)
q4 <- final_data %>%
  filter(Year == 2005, CO2 >= 200) %>%
  pull(State)

final_data1 <- final_data %>%
  filter(State %in% q1)
final_data2 <- final_data %>%
  filter(State %in% q2)
final_data3 <- final_data %>%
  filter(State %in% q3)
final_data4 <- final_data %>%
  filter(State %in% q4)

p1 <- ggplot(final_data1, aes(x = Year, y = CO2, color = State)) +
  geom_line() +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  geom_vline(xintercept = 2005, linetype = "dashed", color = "black") +
  theme_minimal()

p2 <- ggplot(final_data2, aes(x = Year, y = CO2, color = State)) +
  geom_line() +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  geom_vline(xintercept = 2005, linetype = "dashed", color = "black") +
  theme_minimal()

p3 <- ggplot(final_data3, aes(x = Year, y = CO2, color = State)) +
  geom_line() +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  geom_vline(xintercept = 2005, linetype = "dashed", color = "black") +
  theme_minimal()

p4 <- ggplot(final_data4, aes(x = Year, y = CO2, color = State)) +
  geom_line() +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  geom_vline(xintercept = 2005, linetype = "dashed", color = "black") +
  theme_minimal()

jpeg(file=paste0(project.dir,"/plots/FigS1.jpeg"),
     width = 8083, height = 8436, units = "px", res = 1000)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()
