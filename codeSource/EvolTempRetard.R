library(dplyr)
library(ggplot2)
library(tidyr)

data2019 <- read.csv("../FlightsInfos/2019.csv", header=TRUE, sep=",")
data2020 <- read.csv("../FlightsInfos/2020.csv", header=TRUE, sep=",")
data2021 <- read.csv("../FlightsInfos/2021.csv", header=TRUE, sep=",")
data2022 <- read.csv("../FlightsInfos/2022.csv", header=TRUE, sep=",")
data2023 <- read.csv("../FlightsInfos/2023.csv", header=TRUE, sep=",")

extractDelayColumns <- function(data) {
  result <- select(data,
                   "FL_MONTH",
                   "FL_YEAR",
                   "DELAY_DUE_CARRIER", 
                   "DELAY_DUE_WEATHER",
                   "DELAY_DUE_NAS",
                   "DELAY_DUE_SECURITY",
                   "DELAY_DUE_LATE_AIRCRAFT"
  )
  return(result)
}

data2019_Delay <- extractDelayColumns(data2019) 
data2020_Delay <- extractDelayColumns(data2020)                   
data2021_Delay <- extractDelayColumns(data2021) 
data2022_Delay <- extractDelayColumns(data2022) 
data2023_Delay <- extractDelayColumns(data2023) 

data_delay <- rbind(data2019_Delay,
                    data2020_Delay,
                    data2021_Delay,
                    data2022_Delay,
                    data2023_Delay)

#Comptage du nombre de retard par mois par an et pour chaque cause
data_delay_binary <- data_delay |>
  mutate(across(starts_with("DELAY_DUE_"), ~ifelse(is.na(.) | . == 0, 0, 1)))

delay_counts_binary <- data_delay_binary |>
  group_by(FL_MONTH, FL_YEAR) |>
  summarize(
    Delay_CARRIER = sum(DELAY_DUE_CARRIER),
    Delay_WEATHER = sum(DELAY_DUE_WEATHER),
    Delay_NAS = sum(DELAY_DUE_NAS),
    Delay_SECURITY = sum(DELAY_DUE_SECURITY),
    Delay_LATE_AIRCRAFT = sum(DELAY_DUE_LATE_AIRCRAFT)
  )

delay_counts_binary_plot <- delay_counts_binary |>
  pivot_longer(cols = starts_with("Delay_"), names_to = "Cause", values_to = "Num_Retards")

ggplot(delay_counts_binary_plot, aes(x = as.Date(paste(FL_YEAR, FL_MONTH, "01", sep = "-")), y = Num_Retards, color = Cause)) +
  geom_line() +
  labs(title = "Nombre de retards par mois pour chaque cause",
       x = "Mois",
       y = "Nombre de retards",
       color = "Cause") +
  theme_minimal()

#Comptage de la durée totale des retards par mois et pour chaque cause
data_delay_total <- data_delay |>
  mutate(across(starts_with("DELAY_DUE_"), ~ifelse(is.na(.) | . == 0, 0, .)))

delay_counts_total <- data_delay_total |>
  group_by(FL_MONTH, FL_YEAR) |>
  summarize(
    Delay_CARRIER = sum(DELAY_DUE_CARRIER),
    Delay_WEATHER = sum(DELAY_DUE_WEATHER),
    Delay_NAS = sum(DELAY_DUE_NAS),
    Delay_SECURITY = sum(DELAY_DUE_SECURITY),
    Delay_LATE_AIRCRAFT = sum(DELAY_DUE_LATE_AIRCRAFT)
  )

delay_counts_total_plot <- delay_counts_total |>
  pivot_longer(cols = starts_with("Delay_"), names_to = "Cause", values_to = "Num_Retards")

ggplot(delay_counts_total_plot, aes(x = as.Date(paste(FL_YEAR, FL_MONTH, "01", sep = "-")), y = Num_Retards, color = Cause)) +
  geom_line() +
  labs(title = "Durée totale des retards par mois pour chaque cause",
       x = "Mois",
       y = "Durée totale des retards",
       color = "Cause") +
  theme_minimal()

