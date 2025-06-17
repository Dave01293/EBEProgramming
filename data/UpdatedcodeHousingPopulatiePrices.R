library(tidyverse)

Aantal_woningen <- read_csv("data/Naamloze spreadsheet - Voorraad_woningen_en_niet_woningen__mutaties__gebruiksfunctie__regio_04062025_144455.csv")
rm(Naamloze_spreadsheet_Voorraad_woningen_en_niet_woningen_mutaties_gebruiksfunctie_regio_04062025_144455)

Verkoopprijzen <- read_csv("data/Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458 - Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458.csv")
rm(Naamloze_spreadsheet_Bevolkingsontwikkeling_regio_per_maand_04062025_144448)

Bevolkingsgroei <- read_csv("data/Naamloze spreadsheet - Bevolkingsontwikkeling__regio_per_maand_04062025_144448.csv")
rm(Naamloze_spreadsheet_Bevolkingsontwikkeling_regio_per_maand_04062025_144448)

#install.packages("tidyverse")

require(tidyverse)

# === Bevolkingsontwikkeling ===

bevolking_long <- Bevolkingsgroei %>%
  pivot_longer(
    cols = c("2012", "2016", "2020", "2024"),
    names_to = "jaar",
    values_to = "waarde"
  )

# === Koopwoningen ===

#koopwoningen_long <- Aantal_woningen %>%
# pivot_longer(
#  cols = c("2012", "2016", "2020", "2024"),
# names_to = "jaar",
#values_to = "waarde")

# === Koopwoningen ===
koopwoningen <- read_csv("data/Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458 - Bestaande_koopwoningen__gemiddelde_verkoopprijzen__regio_04062025_144458.csv")

Verkoopprijzen_long <- Verkoopprijzen %>%
  pivot_longer(
    cols = c("2012", "2016", "2020", "2024"),
    names_to = "jaar",
    values_to = "waarde")

Aantal_woningen <- Aantal_woningen %>%
  rename(jaar = Perioden)
Aantal_woningen$jaar <- as.character(Aantal_woningen$jaar)

dataframe <- full_join(Verkoopprijzen_long, bevolking_long, by=c("jaar", "Regio's"))

df <- full_join(dataframe, Aantal_woningen, by=c("jaar","Regio's"))

remove_missing(df, na.rm = FALSE, vars = c("waarde.x", "waarde.y","Beginstand voorraad", "Nieuwbouw", "Eindstand Voorraad"), finite = FALSE)

df2 <- remove_missing(df, na.rm = FALSE,
                      vars = c("waarde.x", "waarde.y","Beginstand voorraad", "Nieuwbouw", "Eindstand Voorraad"), 
                      finite = FALSE)
df2$Verkoopprijs <- df2$waarde.x
df2$BevolkGrootte <- df2$waarde.y
df2$waarde.x <- NULL
df2$waarde.y <- NULL
df2$Onderwerp.x <- NULL
df2$Onderwerp.y <- NULL

#filter op regio
plotdata = df2 %>% filter(`Regio's` == "Amersfoort")

#tijdsvisualisatie
plotdata$jaar <- as.numeric(plotdata$jaar)

ggplot(plotdata, aes(x = jaar, y = Verkoopprijs)) +
  geom_point() +
  xlab("Year") +
  ylab("Price in € (x1000)") +
  geom_line() +
  scale_y_continuous(
  breaks = seq(300000, 500000, 100000),    
  labels = function(x) x / 1000) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024))

#adding variables
df2 <- df2 %>%
  group_by(`Regio's`) %>%
  mutate(GrowthPercentageBevolking = (BevolkGrootte / lag(BevolkGrootte)-1)*100)

df2 <- df2 %>%
  group_by(`Regio's`) %>%
  mutate(GrowthPercentageVoorraad = (`Eindstand voorraad` / lag(`Eindstand voorraad`)-1)*100)

#subgroep analyse

data2020 <- subset(df2, jaar == 2020)

BevolkMean2020 <- mean(data2020$BevolkGrootte, na.rm = TRUE)

data2020$GrootteBevolking <- 0
data2020$GrootteBevolking <- data2020$GrootteBevolking %>% 
  replace(data2020$BevolkGrootte < BevolkMean2020, "Klein")
data2020$GrootteBevolking <- data2020$GrootteBevolking %>% 
  replace(data2020$BevolkGrootte >= BevolkMean2020, "Groot")

ggplot(data = data2020, 
       aes(x = GrootteBevolking, y = GrowthPercentageVoorraad)) +
  geom_boxplot()
#BevolkOnderMean <- data2020[data2020$BevolkGrootte < BevolkMean2020]
#BevolkBovenMean <- data2020[data2020$BevolkGrootte > BevolkMean2020]


#voorstel jack voor later
#q1price <- subset(df2, waarde.x >= quartiles[1] & value <= quartiles[2])  # 0–25%
#q2price <- subset(df2, waarde.x >  quartiles[2] & value <= quartiles[3])  # 25–50%
#q3price <- subset(df2, waarde.x >  quartiles[3] & value <= quartiles[4])  # 50–75%
#q4price <- subset(df2, waarde.x >  quartiles[4] & value <= quartiles[5])  # 75–100%

