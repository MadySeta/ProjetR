## Companies to avoid
library(dplyr)
library(ggplot2)
library(tidyr)

airline_code_names = read.csv('../FlightsInfos/AIRLINE_CODE_DICTIONARY.csv')
data2019 <- read.csv("../FlightsInfos/2019.csv", header=TRUE, sep=",")

ratios_by_airline <- data2019 %>%
  group_by(AIRLINE_CODE) %>%
  summarise(
    Total_Flights = n(),
    Total_Delays = sum(ARR_DELAY > 0, na.rm = TRUE),
    Ratio_Delays = Total_Delays / Total_Flights
  ) %>%
  ungroup() %>%
  arrange((Ratio_Delays))

ratios_by_airline <- ratios_by_airline %>%
  left_join(airline_code_names, by = c("AIRLINE_CODE" = "Code"))

ggplot(ratios_by_airline, aes(x = reorder(x = Description, Ratio_Delays), y = Ratio_Delays)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  labs(x = "Compagnie aérienne", y = "Ratio de retards",
       title = "Ratio de retards par compagnie aérienne") +
  theme_minimal()

