library(dplyr)
library(ggplot2)
library(tidyr)
library(openxlsx)

# Lecture des fichiers csv
data2019 <- read.csv("../FlightsInfos/2019.csv", header=TRUE, sep=",")
data2020 <- read.csv("../FlightsInfos/2020.csv", header=TRUE, sep=",")
data2021 <- read.csv("../FlightsInfos/2021.csv", header=TRUE, sep=",")
data2022 <- read.csv("../FlightsInfos/2022.csv", header=TRUE, sep=",")
data2023 <- read.csv("../FlightsInfos/2023.csv", header=TRUE, sep=",")

# Extraction des colonnes pertinentes
extractCancelationsColumns <- function(data) {
  data_cancelation <- data %>%
    select("FL_MONTH", 
           "FL_YEAR", 
           "CANCELLED", 
           "CANCELLATION_CODE") %>%
    filter(CANCELLED == 1)
  
  return(data_cancelation)
}

# Extraction des données d'annulation pour chaque année
data2019_cancelation <- extractCancelationsColumns(data2019) 
data2020_cancelation <- extractCancelationsColumns(data2020) 
data2021_cancelation <- extractCancelationsColumns(data2021) 
data2022_cancelation <- extractCancelationsColumns(data2022)
data2023_cancelation <- extractCancelationsColumns(data2023) 
                               
# Combinaison des données d'annulation
data_cancelation <- rbind(data2019_cancelation,
                          data2020_cancelation,
                          data2021_cancelation,
                          data2022_cancelation,
                          data2023_cancelation)

# Création d'une nouvelle variable de saison
data_cancelation <- data_cancelation %>%
  mutate(Season = case_when(
    FL_MONTH %in% c(3, 4, 5) ~ "Spring",
    FL_MONTH %in% c(6, 7, 8) ~ "Summer",
    FL_MONTH %in% c(9, 10, 11) ~ "Autumn",
    FL_MONTH %in% c(12, 1, 2) ~ "Winter"
  ))

# Correspondance des codes et descriptions
cancelationCode <- data.frame(
  CANCELLATION_CODE = c("A", "B", "C", "D"),
  causeDescription = c("Carrier", "Weather", "National Air System", "Security")
)

# Fusion des données sur les annulations avec les descriptions des causes
# en utilisant la correspondance entre les codes d'annulation et leurs descriptions.
# Groupement par description de cause et par saison pour calculer le ratio.
ratio_data <- data_cancelation %>%
  left_join(cancelationCode, by = "CANCELLATION_CODE") %>%
  group_by(causeDescription, Season) %>%
  summarize(Count = n()) %>%
  group_by(Season) %>%
  mutate(Ratio = Count / sum(Count))

# Création du graphique en camembert pour chaque saison avec les ratios
ggplot(ratio_data, aes(x = "", y = Ratio, fill = causeDescription)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = ""), stat = "identity", position = position_stack(vjust = 0.5)) +
  coord_polar("y") +
  facet_wrap(~ Season) +
  theme_minimal() +
  labs(title = "Ratio des annulations par cause et par saison",
       fill = "Cause d'annulation")

