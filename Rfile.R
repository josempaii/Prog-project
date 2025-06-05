

###### TIDYVERSE #######

library(tidyverse)

######## IMPORTING DATASET ########

df <- read_csv("rawdata")

##########################
######FILTERING DATA######
##########################

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
  
#######TRANSFORMING AGE INTO NUMERING VALUE#######
  mutate(
    average_age = as.numeric(average_age)
  )  

#########################
##### LINE CHART#########
#########################

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

#########################################
#######BAR CHART COMP 3 REGIONS##########
#########################################



