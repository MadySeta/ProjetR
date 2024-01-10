library(ggplot2)
library(dplyr)
library(fmsb)
library(tidyr)

us_airports_type = read.csv("../FlightsInfos/us-airports.csv")
us_airports_type = us_airports_type[-1,]
us_airports_type = us_airports_type[c('iata_code','type')]
data2020 = read.csv("../FlightsInfos/2020.csv")

combined_data <- left_join(data2020, us_airports_type, by = c("ORIGIN" = "iata_code"))
combined_data <- combined_data %>% 
  mutate(type = ifelse(is.na(type), "unknown_type", type))


delay_counts <- combined_data %>%
  group_by(type) %>%
  summarise(
    Count_Carrier_Delay = sum(DELAY_DUE_CARRIER > 0, na.rm = TRUE),
    Count_Weather_Delay = sum(DELAY_DUE_WEATHER > 0, na.rm = TRUE),
    Count_NAS_Delay = sum(DELAY_DUE_NAS > 0, na.rm = TRUE),
    Count_Security_Delay = sum(DELAY_DUE_SECURITY > 0, na.rm = TRUE),
    Count_Late_Aircraft_Delay = sum(DELAY_DUE_LATE_AIRCRAFT > 0, na.rm = TRUE)
  ) %>%
  mutate(
    Total_Delays = Count_Carrier_Delay + Count_Weather_Delay + Count_NAS_Delay + Count_Security_Delay + Count_Late_Aircraft_Delay,
    Ratio_Carrier_Delay = Count_Carrier_Delay / Total_Delays,
    Ratio_Weather_Delay = Count_Weather_Delay / Total_Delays,
    Ratio_NAS_Delay = Count_NAS_Delay / Total_Delays,
    Ratio_Security_Delay = Count_Security_Delay / Total_Delays,
    Ratio_Late_Aircraft_Delay = Count_Late_Aircraft_Delay / Total_Delays
  ) %>%
  gather(key = "Delay_Type", value = "Ratio", -type)


radar_data <- delay_counts %>%
  filter(Ratio <= 1)

ggplot(radar_data, aes(x = type, y = Ratio, fill = Delay_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Diagramme en boîte des retards par type d'aéroport",
       x = "Type d'aéroport",
       y = "Ratio de retard") +
  theme_minimal()



