load(gbif_simple.csv)
gbif_simple <- read.csv("gbif_simple.csv")

#each species separte 

species_list <- split(gbif_simple, gbif_simple$species)
View(species_list)

regions <- read.csv("Kenya_Counties.csv")
aberdare <- subset(regions, regions$NAME == "ABERDARES")
aberdare <- regions[regions$OBJECTID == 45, ]

#with a shapefile 
library(sf)
aberdare_sf <- st_read("Kenya_Counties.shp")
aberdare_only <- aberdare_sf[aberdare_sf$OBJECTID == 45, ] 

species_endangered <- read.csv("species.csv")
species_endangered <- read.csv("species.csv", check.names = TRUE)
species_endangered <- read.csv("species.csv", row.names = NULL)

#fixing 
readr::read_delim("species.csv", delim = NULL)
readLines("species.csv", n = 5)
species_endangered <- read.csv("species.csv", sep = "\t")

library(sf)

species_sf <- st_as_sf(species_endangered, coords = c("decimalLongitude", "decimalLatitude"), 
                       crs = 4326)
aberdare <- st_transform(aberdare, crs = 4326)

species_sf <- st_transform(species_sf, crs = st_crs(aberdare_only))


#data into the polygon 
species_in_aberdare <- species_sf[st_within(species_sf, aberdare_only, sparse = FALSE), ]

#counting the number of species 
library(dplyr)

n_distinct_species <- species_in_aberdare %>% 
  distinct(species) %>% 
  nrow()
species_summary <- species_in_aberdare %>% 
  group_by(kingdom) %>% 
  summarise(n_species = n_distinct(species))

species_by_kingdom <- species_in_aberdare %>% 
  group_by(kingdom) %>% 
  distinct(species)

#species list for ACA 
species_list <- split(species_in_aberdare, species_in_aberdare$species)
view(species_list)

# SIMPLE Balerica data - trial for Maxent 
balearica_data <- species_in_aberdare %>%
  filter(species == "Balearica regulorum") %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  select(longitude, latitude, species)
  
write.csv(balearica_data, "balerica_regolurum.csv", row.names = FALSE)
  
#making my first map - plant species 
plants <- species_in_aberdare %>% filter(kingdom == "Plantae")
class(plants)
plants <- st_as_sf(plants, coords = c("longitude", "latitude"), crs = st_crs(aberdare))

st_geometry(plants)

library(ggplot2)
#This was done when MaxEnt was not working for me, this was not continued. 
#It was also not used in my Thesis 

bbox <- st_bbox(aberdare_only)

#ABERDARE SHAPE FILE 
library(sf)

st_write(aberdare_only, "aberdare_only.shp", delete_layer = TRUE)
st_write(aberdare_only, "aberdare_only.gpkg", delete_layer = TRUE)

#move the balerica data correctly
balearica_data <- balearica_data[, c("species", "longitude", "latitude")]
write.csv(balearica_data, "balerica_regolurum.csv", row.names = FALSE)
balearica_data <- balearica_data[!(balearica_data$longitude == 0 & balearica_data$latitude == 0), ]

install.packages("rmarkdown")
