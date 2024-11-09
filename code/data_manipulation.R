library(skimr)
library(rio)
library(tidyverse)

# load data
data <- rio::import("data/happiness.csv")

skim(data)

# add longitude and latitude features to the data
# this is a bit of a hack but it works
# I got the data from https://developers.google.com/public-data/docs/canonical/countries_csv
# and then I manually added the longitude and latitude for the countries that were missing
# I also manually added the longitude and latitude for the countries that were missing

# load countries long and lat
countries <- rio::import("data/countries.csv")

# for consistency rename North Macedonia to Macedonia
data$country[data$country == "North Macedonia"] <- "Macedonia"
# North Cyprus to Northern Cyprus
data$country[data$country == "North Cyprus"] <- "Northern Cyprus"


# add longitude and latitude to data
data <- data %>%
  left_join(countries, by = c("country" = "name"))

# find the countries that are missing
data %>%
  filter(is.na(longitude))

# manually add the longitude and latitude for the countries that are missing
data$longitude[data$country == "Taiwan Province of China"] <- countries$longitude[countries$name == "Taiwan"]
data$latitude[data$country == "Taiwan Province of China"] <- countries$latitude[countries$name == "Taiwan"]
data$latitude[data$country == "Hong Kong S.A.R., China"] <- countries$latitude[countries$name == "Hong Kong"]
data$longitude[data$country == "Hong Kong S.A.R., China"] <- countries$longitude[countries$name == "Hong Kong"]
data$longitude[data$country == "Macedonia"] <- countries$longitude[countries$name == "Macedonia [FYROM]"]
data$latitude[data$country == "Macedonia"] <- countries$latitude[countries$name == "Macedonia [FYROM]"]
data$longitude[data$country == "Myanmar"] <- countries$longitude[countries$name == "Myanmar [Burma]"]
data$latitude[data$country == "Myanmar"] <- countries$latitude[countries$name == "Myanmar [Burma]"]
data$longitude[data$country == "Ivory Coast"] <- countries$longitude[endsWith(countries$name, "d'Ivoire")]
data$latitude[data$country == "Ivory Coast"] <- countries$latitude[endsWith(countries$name, "d'Ivoire")]
data$longitude[data$country == "South Sudan"] <- countries$longitude[countries$name == "Sudan"]
data$latitude[data$country == "South Sudan"] <- countries$latitude[countries$name == "Sudan"]
data$longitude[data$country == "Congo (Kinshasa)"] <- countries$longitude[countries$name == "Congo [DRC]"]
data$latitude[data$country == "Congo (Kinshasa)"] <- countries$latitude[countries$name == "Congo [DRC]"]
data$longitude[data$country == "Congo (Brazzaville)"] <- countries$longitude[countries$name == "Congo [Republic]"]
data$latitude[data$country == "Congo (Brazzaville)"] <- countries$latitude[countries$name == "Congo [Republic]"]
data$longitude[data$country == "Trinidad & Tobago"] <- countries$longitude[countries$name == "Trinidad and Tobago"]
data$latitude[data$country == "Trinidad & Tobago"] <- countries$latitude[countries$name == "Trinidad and Tobago"]
# just use Cyprus for Northern Cyprus
data$longitude[data$country == "Northern Cyprus"] <- countries$longitude[countries$name == "Cyprus"]
data$latitude[data$country == "Northern Cyprus"] <- countries$latitude[countries$name == "Cyprus"]

# add a continent feature
library(countrycode)
country_names <- data$country
country_continents <- countrycode(country_names, "country.name", "continent")
na <- which(is.na(country_continents))
data[na, "country"]
# replace na with Europe
country_continents[na] <- "Europe"
# add to data
data$continent <- country_continents
data$continent <- factor(data$continent)

# remove ID
data <- data %>%
  select(-ID)

# create dummy variables for continent using one hot encoding using fastDummies
library(fastDummies)
data <- data %>%
  dummy_cols(select_columns = "continent", remove_selected_columns = FALSE)

# write world data to csv
rio::export(data, "data/world.csv")
