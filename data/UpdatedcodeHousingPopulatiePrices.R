library(tidyverse)

# Read initial data sets
Aantal_woningen <- read_csv("data/Naamloze spreadsheet - Voorraad_woningen_en_niet_woningen__mutaties__gebruiksfunctie__regio_04062025_144455.csv")
rm(Naamloze_spreadsheet_Voorraad_woningen_en_niet_woningen_mutaties_gebruiksfunctie_regio_04062025_144455)

Verkoopprijzen <- read_csv("data/Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458 - Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458.csv")
rm(Naamloze_spreadsheet_Bevolkingsontwikkeling_regio_per_maand_04062025_144448)

Bevolkingsgroei <- read_csv("data/Naamloze spreadsheet - Bevolkingsontwikkeling__regio_per_maand_04062025_144448.csv")
rm(Naamloze_spreadsheet_Bevolkingsontwikkeling_regio_per_maand_04062025_144448)

#install package tidyverse

require(tidyverse)

#Bevolking data wide naar long

bevolking_long <- Bevolkingsgroei %>%
  pivot_longer(
    cols = c("2012", "2016", "2020", "2024"),
    names_to = "jaar",
    values_to = "waarde"
  )

# Koopwoningen wide naar long
koopwoningen <- read_csv("data/Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458 - Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458.csv")

Verkoopprijzen_long <- Verkoopprijzen %>%
  pivot_longer(
    cols = c("2012", "2016", "2020", "2024"),
    names_to = "jaar",
    values_to = "waarde")

#Rename colums from "jaar" to "perioden"
Aantal_woningen <- Aantal_woningen %>%
  rename(jaar = Perioden)

Aantal_woningen$jaar <- as.character(Aantal_woningen$jaar)

#Samenvoegen datasets "verkoopprijzen_long" en "bevolking_long" tot 1 dataset: "dataframe"
dataframe <- full_join(Verkoopprijzen_long, bevolking_long, by=c("jaar", "Regio's"))

#Samenvoegen datasets "dataframe" en "Aantal_woningen" tot 1 dataset: "df"
df <- full_join(dataframe, Aantal_woningen, by=c("jaar","Regio's"))

#Creating Maindata by removing unnecessary colums from df
Maindata <- remove_missing(df, na.rm = FALSE,
                      vars = c("waarde.x", "waarde.y","Beginstand voorraad", "Nieuwbouw", "Eindstand Voorraad"), 
                      finite = FALSE)
Maindata$Verkoopprijs <- Maindata$waarde.x
Maindata$BevolkGrootte <- Maindata$waarde.y
Maindata$waarde.x <- NULL
Maindata$waarde.y <- NULL
Maindata$Onderwerp.x <- NULL
Maindata$Onderwerp.y <- NULL

#filter per region
plotdataAmersfoort = Maindata %>% filter(`Regio's` == "Amersfoort")

#tijdsvisualisatie
plotdataAmersfoort$jaar <- as.numeric(plotdataAmersfoort$jaar)

ggplot(plotdataAmersfoort, aes(x = jaar, y = Verkoopprijs)) +
  geom_point() +
  xlab("Year") +
  ylab("Price in € (x1000)") +
  ggtitle("Housing prices per year (Amersfoort)") +
  geom_line() +
  geom_vline(xintercept = 2016) +
  scale_y_continuous(
  breaks = seq(300000, 500000, 100000),    
  labels = function(x) x / 1000) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024))


#adding variables to Maindata 
Maindata <- Maindata %>%
  group_by(`Regio's`) %>%
  mutate(GrowthPercentageBevolking = (BevolkGrootte / lag(BevolkGrootte)-1)*100)

Maindata <- Maindata %>%
  group_by(`Regio's`) %>%
  mutate(GrowthPercentageVoorraad = (`Eindstand voorraad` / lag(`Eindstand voorraad`)-1)*100)

#subgroep analyse
data2020 <- subset(Maindata, jaar == 2020)

BevolkMean2020 <- mean(data2020$BevolkGrootte, na.rm = TRUE)

data2020$GrootteBevolking <- 0
data2020$GrootteBevolking <- data2020$GrootteBevolking %>% 
  replace(data2020$BevolkGrootte < BevolkMean2020, "Small")
data2020$GrootteBevolking <- data2020$GrootteBevolking %>% 
  replace(data2020$BevolkGrootte >= BevolkMean2020, "Big")

ggplot(data = data2020, 
       aes(x = GrootteBevolking, y = GrowthPercentageVoorraad)) +
  geom_boxplot() +
  xlab("Population size") +
  ylab("Growth housing supply in percentage") +
  ggtitle("Housing supply growth per subgroup")


#install shapefile
#install.packages("cbsodataR")
#install.packages("sf")
#install.packages("ggplot2")
library(cbsodataR)
library(sf)
library(ggplot2)

#extreme verkoopprijs_gemeentes
minimaleprijs = min(Maindata$Verkoopprijs, na.rm = TRUE)
maximaleprijs = max(Maindata$Verkoopprijs, na.rm = TRUE)
#gemeentes in 2012
gemeente_2012 <- cbs_get_sf("gemeente", 2012)

#create dataset with verkoopprijzen 2012

verkoopprijzen_2012 <- subset(Maindata, jaar == 2012) 

#filter zodat alleen regio, verkoopprijs en jaar overblijft
verkoopprijzen_2012$`Beginstand voorraad` <- NULL
verkoopprijzen_2012$Nieuwbouw <- NULL
verkoopprijzen_2012$`Eindstand voorraad` <- NULL
verkoopprijzen_2012$BevolkGrootte <- NULL
verkoopprijzen_2012$GrowthPercentageBevolking <- NULL
verkoopprijzen_2012$GrowthPercentageVoorraad <- NULL

#merging gemeentedata met verkoopprijsdata
verkoopprijs_gemeentes_2012 <- gemeente_2012 %>% inner_join(verkoopprijzen_2012, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 2012 
ggplot(verkoopprijs_gemeentes_2012, aes(fill = Verkoopprijs)) +
          geom_sf(color = "white", size = 0.1) +
          scale_fill_gradient(limits = c(minimaleprijs, maximaleprijs), low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2012(€)") 

#gemeentes in 2016
gemeente_2016 <- cbs_get_sf("gemeente", 2016)

#create dataset with verkoopprijzen 2016

verkoopprijzen_2016 <- subset(Maindata, jaar == 2016) 

#filter zodat alleen regio, verkoopprijs en jaar overblijft
verkoopprijzen_2016$`Beginstand voorraad` <- NULL
verkoopprijzen_2016$Nieuwbouw <- NULL
verkoopprijzen_2016$`Eindstand voorraad` <- NULL
verkoopprijzen_2016$BevolkGrootte <- NULL
verkoopprijzen_2016$GrowthPercentageBevolking <- NULL
verkoopprijzen_2016$GrowthPercentageVoorraad <- NULL

#merging gemeentedata met verkoopprijsdata in 2016
verkoopprijs_gemeentes_2016 <- gemeente_2016 %>% inner_join(verkoopprijzen_2016, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 
ggplot(verkoopprijs_gemeentes_2016, aes(fill = Verkoopprijs)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimaleprijs, maximaleprijs),low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2016 (€)") 

#gemeentes in 2020
gemeente_2020 <- cbs_get_sf("gemeente", 2020)

#create dataset with verkoopprijzen 2020

verkoopprijzen_2020 <- subset(Maindata, jaar == 2020) 

#filter zodat alleen regio, verkoopprijs en jaar overblijft
verkoopprijzen_2020$`Beginstand voorraad` <- NULL
verkoopprijzen_2020$Nieuwbouw <- NULL
verkoopprijzen_2020$`Eindstand voorraad` <- NULL
verkoopprijzen_2020$BevolkGrootte <- NULL
verkoopprijzen_2020$GrowthPercentageBevolking <- NULL
verkoopprijzen_2020$GrowthPercentageVoorraad <- NULL

#merging gemeentedata met verkoopprijsdata in 2020
verkoopprijs_gemeentes_2020 <- gemeente_2020 %>% inner_join(verkoopprijzen_2020, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 
ggplot(verkoopprijs_gemeentes_2020, aes(fill = Verkoopprijs)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimaleprijs, maximaleprijs), low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2020 (€)") 

#gemeentes in 2024
gemeente_2024 <- cbs_get_sf("gemeente", 2024)

#create dataset with verkoopprijzen 2024

verkoopprijzen_2024 <- subset(Maindata, jaar == 2024) 

#filter zodat alleen regio, verkoopprijs en jaar overblijft
verkoopprijzen_2024$`Beginstand voorraad` <- NULL
verkoopprijzen_2024$Nieuwbouw <- NULL
verkoopprijzen_2024$`Eindstand voorraad` <- NULL
verkoopprijzen_2024$BevolkGrootte <- NULL
verkoopprijzen_2024$GrowthPercentageBevolking <- NULL
verkoopprijzen_2024$GrowthPercentageVoorraad <- NULL

#merging gemeentedata met verkoopprijsdata in 2024
verkoopprijs_gemeentes_2024 <- gemeente_2024 %>% inner_join(verkoopprijzen_2024, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 
ggplot(verkoopprijs_gemeentes_2024, aes(fill = Verkoopprijs)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimaleprijs, maximaleprijs), low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2024 (€)") 
#voor Daves moeder