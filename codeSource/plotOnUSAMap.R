library(ggplot2)
library(dplyr)
library(maps)


us_airports = read.csv("../FlightsInfos/us-airports.csv")
colnames(us_airports)
us_airports = us_airports[-1,]

select.me <- c('iata_code','latitude_deg','longitude_deg')
us_airports = us_airports[,select.me]


data2019 = read.csv("../FlightsInfos/2019.csv")

delays_ratio_per_airport <- data2019 %>%
  group_by(ORIGIN) %>%
  summarise(
    Total_Flights = n(),
    Total_Delays = sum(ARR_DELAY > 0, na.rm = TRUE),
    Ratio_Delays = Total_Delays / Total_Flights
  )

merged_data <- merge(delays_ratio_per_airport, us_airports, by.x = "ORIGIN", by.y = "iata_code")
merged_data$latitude_deg <- as.numeric(as.character(merged_data$latitude_deg))
merged_data$longitude_deg <- as.numeric(as.character(merged_data$longitude_deg))

usa_map <- map_data("state")


usa_map_plot = ggplot(data = adjusted_usa_map) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data = merged_data, aes(x = longitude_deg, y = latitude_deg, size = Ratio_Delays, color = Ratio_Delays), alpha = 0.5) +
  scale_color_gradientn(colors = c("blue", "orange", "red", "darkred")) +
  theme_minimal() +
  labs(title = "Ratio de retards par aéroport sur la carte des États-Unis",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right") +
  scale_size(range = c(2, 10)) +
  guides(color = guide_legend(title = "Ratio de retards"), size = FALSE) 


usa_map_plot <- usa_map_plot +
  coord_cartesian(xlim = c(-125, -65), ylim = c(25, 50))

print(usa_map_plot)
