library(tidyverse)

df <- read_csv("raw_data.csv")

filtered_df <- df %>%
  filter(
    indic_de == "AGEMOTH",
    geo %in% c("IT", "NL", "EU27_2020"),
    TIME_PERIOD %in% c(2014, 2020, 2023) 
  ) %>%
  
  select(geo, TIME_PERIOD, OBS_VALUE) %>% 
  rename(
    region = geo, 
    year = TIME_PERIOD, 
    average_age = OBS_VALUE
  ) %>%
  
  mutate(
    average_age = as.numeric(average_age)
  )  

line_chart <- ggplot(filtered_df, aes(x = year, y = average_age, color = region, group = region)) +
  geom_line(linewidth = 1.2) + 
  geom_point(size = 3.5, shape = 21, fill = "white", stroke = 1) + 
  labs(
    title = "Average Age at Childbirth: Italy, Netherlands and EU (2014, 2020, 2023)",
    x = "Year",
    y = "Average Age of Women at Childbirth",
    color = "Region" 
  ) +
  scale_x_continuous(breaks = c(2014, 2020, 2023)) + 
  scale_y_continuous(limits = c(30, 33)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10), 
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"), 
    panel.grid.minor = element_blank() 
  )

line_chart

bar_chart <- ggplot(filtered_df, aes(x = factor(year), y = average_age - 28, fill = region)) +
  geom_col(position = "dodge", width = 0.7) + 
  labs(
    title = "Average Age at Childbirth: Italy, Netherlands and EU (2014, 2020, 2023)",
    x = "Year",
    y = "Average Age of Women at Childbirth",
    fill = "Region" 
  ) +
  scale_y_continuous(limits = c(0, 5), labels = function(x) x + 28) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    axis.title = element_text(size = 12), 
    axis.text = element_text(size = 10), 
    legend.position = "bottom", 
    legend.title = element_text(face = "bold"), 
    panel.grid.minor = element_blank() 
  )

bar_chart