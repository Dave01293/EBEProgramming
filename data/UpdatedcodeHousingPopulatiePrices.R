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

df2 <- remove_missing(df, na.rm = FALSE, vars = c("waarde.x", "waarde.y","Beginstand voorraad", "Nieuwbouw", "Eindstand Voorraad"), finite = FALSE)