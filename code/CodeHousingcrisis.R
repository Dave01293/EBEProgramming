library(tidyverse)

# Read initial data sets
number_of_houses <- read_csv("data/voorraad_woningen_en_nieuwbouw.csv")

saleprice_houses <- read_csv("data/gemiddelde_verkoopprijzen_koopwoningen.csv")

populationgrowth <- read_csv("data/bevolkingsontwikkeling_per_jaar.csv")

#install package tidyverse

require(tidyverse)

#population data wide to long

population_long <- populationgrowth %>%
  pivot_longer(
    cols = c("2012", "2016", "2020", "2024"),
    names_to = "jaar",
    values_to = "waarde"
  )

# saleprice_houses wide to long

saleprice_houses_long <- saleprice_houses %>%
  pivot_longer(
    cols = c("2012", "2016", "2020", "2024"),
    names_to = "jaar",
    values_to = "waarde")

#Rename colums from "jaar" to "perioden"
number_of_houses <- number_of_houses %>%
  rename(jaar = Perioden)

number_of_houses$jaar <- as.character(number_of_houses$jaar)

#merging datasets "saleprice_houses_long" and "population_long" into 1 dataset: "dataframe"
dataframe <- full_join(saleprice_houses_long, population_long, by=c("jaar", "Regio's"))

#merging datasets "dataframe" and "number_of_houses" into 1 dataset: "df"
df <- full_join(dataframe, number_of_houses, by=c("jaar","Regio's"))

#Creating Maindata by removing unnecessary colums from df
Maindata <- remove_missing(df, na.rm = FALSE,
                           vars = c("waarde.x", "waarde.y","Beginstand voorraad", "nieuwbouw", "eindstand voorraad"), 
                           finite = FALSE)
Maindata$saleprice <- Maindata$waarde.x
Maindata$BevolkGrootte <- Maindata$waarde.y
Maindata$waarde.x <- NULL
Maindata$waarde.y <- NULL
Maindata$Onderwerp.x <- NULL
Maindata$Onderwerp.y <- NULL

#adding variables to Maindata 
Maindata <- Maindata %>%
  group_by(`Regio's`) %>%
  mutate(GrowthPercentagepopulation = (BevolkGrootte / lag(BevolkGrootte)-1)*100)

Maindata <- Maindata %>%
  group_by(`Regio's`) %>%
  mutate(GrowthPercentagesupply = (`Eindstand voorraad` / lag(`Eindstand voorraad`)-1)*100)

#subgroup analyse
data2020 <- subset(Maindata, jaar == 2020)

BevolkMean2020 <- mean(data2020$BevolkGrootte, na.rm = TRUE)

data2020$Groottepopulation <- 0
data2020$Groottepopulation <- data2020$Groottepopulation %>% 
  replace(data2020$BevolkGrootte < BevolkMean2020, "Small")
data2020$Groottepopulation <- data2020$Groottepopulation %>% 
  replace(data2020$BevolkGrootte >= BevolkMean2020, "Big")

ggplot(data = data2020, 
       aes(x = Groottepopulation, y = GrowthPercentagesupply)) +
  geom_boxplot() +
  xlab("population size") +
  ylab("Growth housing supply in percentage") +
  ggtitle("Housing supply growth per subgroup")


#install shapefile
#install.packages("cbsodataR")
#install.packages("sf")
#install.packages("ggplot2")
library(cbsodataR)
library(sf)
library(ggplot2)

#extreme saleprice_municipality
minimumprice = min(Maindata$saleprice, na.rm = TRUE)
maximumprice = max(Maindata$saleprice, na.rm = TRUE)

##########Heatmap Netherlands average salesprice house in 2012###########
#municipality in 2012
municipality_2012 <- cbs_get_sf("gemeente", 2012)

#create dataset with saleprice_houses 2012

saleprice_houses_2012 <- subset(Maindata, jaar == 2012) 

#filter dataset, region, saleprice and year remains
saleprice_houses_2012 <- saleprice_houses_2012 %>%
  select(`Regio's`, jaar, saleprice)

#merging municipalitydata with salepricedata
saleprice_municipality_2012 <- municipality_2012 %>% inner_join(saleprice_houses_2012, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 2012 
ggplot(saleprice_municipality_2012, aes(fill = saleprice)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimumprice, maximumprice), low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2012(€)") 

##########Heatmap Netherlands average salesprice house in 2016###########
#municipality in 2016
municipality_2016 <- cbs_get_sf("gemeente", 2016)

#create dataset with saleprice_houses 2016

saleprice_houses_2016 <- subset(Maindata, jaar == 2016) 

#filter ensuring only region, saleprice and year remains
saleprice_houses_2016 <- saleprice_houses_2016 %>%
  select(`Regio's`, jaar, saleprice)

#merging municipalitydata with salepricedata in 2016
saleprice_municipality_2016 <- municipality_2016 %>% inner_join(saleprice_houses_2016, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 
ggplot(saleprice_municipality_2016, aes(fill = saleprice)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimumprice, maximumprice),low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2016 (€)") 

##########Heatmap Netherlands average salesprice house in 2020###########
#municipality in 2020
municipality_2020 <- cbs_get_sf("gemeente", 2020)

#create dataset with saleprice_houses 2020

saleprice_houses_2020 <- subset(Maindata, jaar == 2020) 

#filter ensuring only region, saleprice and year remains
saleprice_houses_2020 <- saleprice_houses_2020 %>%
  select(`Regio's`, jaar, saleprice)

#merging municipalitydata with salepricedata in 2020
saleprice_municipality_2020 <- municipality_2020 %>% inner_join(saleprice_houses_2020, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 
ggplot(saleprice_municipality_2020, aes(fill = saleprice)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimumprice, maximumprice), low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2020 (€)") 

##########Heatmap Netherlands average salesprice house in 2024###########
#municipality in 2024
municipality_2024 <- cbs_get_sf("gemeente", 2024)

#create dataset with saleprice_houses 2024

saleprice_houses_2024 <- subset(Maindata, jaar == 2024) 

#filter ensuring only region, saleprice and year remains
saleprice_houses_2024 <- saleprice_houses_2024 %>%
  select(`Regio's`, jaar, saleprice)

#merging municipalitydata with salepricedata in 2024
saleprice_municipality_2024 <- municipality_2024 %>% inner_join(saleprice_houses_2024, by = join_by(statnaam == `Regio's`))

#Creating Heatmap 
ggplot(saleprice_municipality_2024, aes(fill = saleprice)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_gradient(limits = c(minimumprice, maximumprice), low = "#00FFFF", high = "red", labels = scales::label_number(big.mark = ".", decimal.mark = ","), name = "Average salesprice house NL in 2024 (€)") 

###########Time visualization weighted average houseprice in 2012#############
#houses filtered on 2012
end_of_year_supply_houses_2012 <- subset (number_of_houses, jaar == 2012)

#filter ensuring only end_of_yearsupply remains
end_of_year_supply_houses_2012$`Beginstand supply`<- NULL
end_of_year_supply_houses_2012$newly_built_housing <- NULL

end_of_year_supply_houses_2012 <- inner_join(end_of_year_supply_houses_2012, saleprice_municipality_2012, by = join_by(`Regio's` == statnaam))
#weighted average 
end_of_year_supply_houses_2012$weighted_mean_salesprice = weighted.mean(end_of_year_supply_houses_2012$saleprice , end_of_year_supply_houses_2012$`Eindstand voorraad`, na.rm = T)


###########Time visualization weighted average houseprice in 2016#############
#houses filtered on 2016
end_of_year_supply_houses_2016 <- subset (number_of_houses, jaar == 2016)

#filter ensuring only end_of_year supply remains
end_of_year_supply_houses_2016$`Beginstand supply`<- NULL
end_of_year_supply_houses_2016$newly_built_housing <- NULL

end_of_year_supply_houses_2016 <- inner_join(end_of_year_supply_houses_2016, saleprice_municipality_2016, by = join_by(`Regio's` == statnaam))
#weighted average 
end_of_year_supply_houses_2016$weighted_mean_salesprice = weighted.mean(end_of_year_supply_houses_2016$saleprice , end_of_year_supply_houses_2016$`Eindstand voorraad`, na.rm = T)

###########Time visualization weighted average houseprice in 2020#############
#houses filtered on 2020
end_of_year_supply_houses_2020 <- subset (number_of_houses, jaar == 2020)

#filter ensuring only end_of_year supply remains
end_of_year_supply_houses_2020$`Beginstand supply`<- NULL
end_of_year_supply_houses_2020$newly_built_housing <- NULL

end_of_year_supply_houses_2020 <- inner_join(end_of_year_supply_houses_2020, saleprice_municipality_2020, by = join_by(`Regio's` == statnaam))
#weighted average 
end_of_year_supply_houses_2020$weighted_mean_salesprice = weighted.mean(end_of_year_supply_houses_2020$saleprice , end_of_year_supply_houses_2020$`Eindstand voorraad`, na.rm = T)

###########Time visualization weighted average houseprice in 2024#############
#houses filtered on 2024
end_of_year_supply_houses_2024 <- subset (number_of_houses, jaar == 2024)

#filter ensuring only end_of_year supply remains
end_of_year_supply_houses_2024$`Beginstand supply`<- NULL
end_of_year_supply_houses_2024$newly_built_housing <- NULL

end_of_year_supply_houses_2024 <- inner_join(end_of_year_supply_houses_2024, saleprice_municipality_2024, by = join_by(`Regio's` == statnaam))
#weighted average 
end_of_year_supply_houses_2024$weighted_mean_salesprice = weighted.mean(end_of_year_supply_houses_2024$saleprice , end_of_year_supply_houses_2024$`Eindstand voorraad`, na.rm = T)

end_of_year_supply_houses <-  end_of_year_supply_houses_2012 
end_of_year_supply_houses = rbind(end_of_year_supply_houses, end_of_year_supply_houses_2016)
end_of_year_supply_houses = rbind(end_of_year_supply_houses, end_of_year_supply_houses_2020)
end_of_year_supply_houses = rbind(end_of_year_supply_houses, end_of_year_supply_houses_2024)

###filter ensuring only 1 region remains
Weighted_mean_houseprices = end_of_year_supply_houses %>% filter(`Regio's` == "Amersfoort")

###filterensuring only Weighted_mean_houseprices remains
Weighted_mean_houseprices = Weighted_mean_houseprices %>%
  select(jaar.x, weighted_mean_salesprice)
Weighted_mean_houseprices = Weighted_mean_houseprices %>% rename(jaar = jaar.x)

#ensuring year will be treated as numeric
Weighted_mean_houseprices$jaar <- as.numeric(Weighted_mean_houseprices$jaar)

#making a plot of Weighted_mean_houseprices
ggplot(Weighted_mean_houseprices, aes(x = jaar, y = weighted_mean_salesprice)) +
  geom_point() +
  xlab("Year") +
  ylab("Price in € (x1000)") +
  ggtitle("Weighted mean houseprice in the Netherlands per year") +
  geom_line() +
  geom_vline(xintercept = 2016) +
  annotate("text", x = 2018, y = 400000, size = 4, label = "\nDecline in mortgage\n interest rates") +
  scale_y_continuous(
    breaks = seq(200000, 500000, 100000),    
    labels = function(x) x / 1000) +
  scale_x_continuous(breaks = c(2012, 2016, 2020, 2024))


