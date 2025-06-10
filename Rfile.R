
###### TIDYVERSE #######

library(tidyverse)

######## IMPORTING DATASET ########

df <- read_csv("rawdata")
df_fertility <- read_csv("dataset_total_fert.csv")

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
  
  mutate(
    average_age = as.numeric(average_age)
  )  
  
  filtered_fertility_df <- df_fertility %>%
  filter(
    indic_de == "TOTFERRT", # Filter for Total fertility rate
    geo %in% c("IT", "NL", "EA19"), # Use EA19 for Euro Area as EU equivalent in this dataset
    TIME_PERIOD %in% c(2014, 2020, 2023) # Filter for the specific years
  ) %>%
  
  select(geo, TIME_PERIOD, OBS_VALUE) %>% # Select only necessary columns
  rename(
    region = geo,
    year = TIME_PERIOD,
    total_fertility_rate = OBS_VALUE
  ) %>%

mutate(
    total_fertility_rate = as.numeric(total_fertility_rate)
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

  ####Merging data for third graph####
  
  combined_df <- inner_join(filtered_df, filtered_fertility_df, by = c("region", "year"))
  
  ##### Scatter graph ####
  fertility_age_plot <- ggplot(combined_df, aes(x = average_age, y = total_fertility_rate, color = region)) +
    geom_point(size = 4, alpha = 0.8) + # Add points
    geom_text(aes(label = year), vjust = -1, hjust = 0.5, show.legend = FALSE, size = 3.5) + # Add year labels to points
    labs(
      title = "Total Fertility Rate vs. Average Age at Childbirth (2014, 2020, 2023)",
      x = "Average Age of Women at Childbirth",
      y = "Total Fertility Rate",
      color = "Region"
    ) +
    scale_color_manual(values = c( # Customize colors for regions
      "IT" = "red",
      "NL" = "blue",
      "EA19" = "purple" # Using EA19 as the EU equivalent
    )) +
    scale_x_continuous(breaks = c(31.1, 31.5, 32, 32.5)) + # Adjust x-axis breaks based on your data range
    scale_y_continuous(breaks = c(1.2, 1.3, 1.4, 1.5, 1.6, 1.7)) + # Adjust y-axis breaks
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
