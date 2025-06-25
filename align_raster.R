library(terra)
library(raster)
ref <- rast("ndvi_copy32.tif")

#load the rest of the rasters 

revi<- rast("evi_copy32.tif")
rele<- rast("ele_copy321.tif")
rbio1 <- rast("extract_bio1")
rbio12 <- rast("extract_bio12")
rbio15 <- rast("extract_bio15")

#align them 

revi_alligned <- resample(revi, ref, method = "bilinear")
rele_alligned <- resample(rele, ref, method = "bilinear")
rbio1_alligned <- resample(rbio1, ref, method = "bilinear")
rbio12_alligned <- resample(rbio12, ref, method = "bilinear")
rbio15_alligned <- resample(rbio15, ref, method = "bilinear")

#into asc 
writeRaster(revi_alligned, "revi_aligned_clea.asc", overwrite = TRUE) 

writeRaster(rele_alligned,
            "rele_aligned.asc", overwrite = TRUE) 

writeRaster(rbio1_alligned, "rbio1_aligned.asc", overwrite = TRUE) 

writeRaster(rbio12_alligned,"rbio12_aligned1.asc" , overwrite = TRUE)

writeRaster(rbio15_alligned,"rbio15_aligned1.asc", overwrite = TRUE) 
writeRaster(ref, "ndvi_aligned2.asc",overwrite = TRUE) 

sessionInfo()
nlyr(rele_alligned)
#also do this for NDVI and then save it as an asc file as well 

#ovverdie the NaNs 
v_evi<-values(revi_alligned)
v_evi[is.nan(v_evi)]<- NA
values(revi_alligned)<- v_evi

NAflag(revi_alligned)<- -9999 
                             
values(ref)[is.nan(values(ref))]<-NA
NAflag(ref)<- -9999 
values(rele_alligned)[is.nan(values(rele_alligned))] <- NA 
NAflag(rele_alligned) <- -9999

values(rbio1_alligned)[is.nan(values(rbio1_alligned))] <- NA
NAflag(rbio1_alligned) <- -9999

values(rbio12_alligned)[is.nan(values(rbio12_alligned))]<-NA
NAflag(rbio12_alligned) <- -9999

values(rbio15_alligned)[is.nan(values(rbio15_alligned))]<- NA
NAflag(rbio15_alligned) <- -9999

#clean versions \please
rbio1_alligned[is.nan(rbio1_alligned)]<-NA
rbio1_alligned_raster <- raster(rbio1_alligned)
writeRaster(rbio1_alligned_raster, "bio1_clean.asc", NAflag = -9999, overwrite = TRUE)

rbio15_alligned[is.nan(rbio15_alligned)]<-NA
rbio15_alligned_raster <-raster(rbio15_alligned)
writeRaster(rbio15_alligned_raster, "bio15_clean.asc", NAflag = -9999, overwrite = TRUE) 

ref[is.nan(ref)]<-NA
ndvi_raster <-raster(ref)
writeRaster(ndvi_raster, "ndvi_clean.asc", NAflag = -9999, overwite = TRUE) 

revi_alligned[is.nan(revi_alligned)]<-NA
revi_alligned_raster<-raster(revi_alligned)
writeRaster(revi_alligned_raster, "evi_clean.asc", NAflag = -9999, overwrite = TRUE)

crs(rbio1_alligned_raster)

balearica<-read.csv("balerica_regolurum.csv")
balearica <-balearica[!(balearica$longitude == 0 & balearica$latitude == 0),]
write.csv(balearica, "balerica_0.csv", row.names = FALSE)

crs(rbio1_alligned_raster)
crs(ndvi_raster)
ndvi_extent<- extent(ndvi_raster)
range(balearica$longitude)
range(balearica$latitude)

#species in the space
br_points <- vect(balearica, geom = c("longitude", "latitude"), crs = "EPSG:32737")
br_points_filtered <- crop(br_points, ndvi_raster)
balerica.filtered <- as.data.frame(br_points_filtered, geom = "XY")

#vsiual 
plot(ndvi_raster)
points(br_clean, col ="red", pch = 16)
crs(br_points)
#fix my CRS 

br_points_reproj <-project(br_points, "EPSG:4326")
df_br <- data.frame(br_points_reproj, geom = "XY")
names(df_br)[names(df_br) == "x"]<- "longitude"
names(df_br)[names(df_br) == "y"]<- "latitude"
head(df_br)
#final try 
coords_br <- crds(br_points_reproj)
br_clean <- cbind(values(br_points_reproj), coords_br)
colnames(br_clean)[ncol(br_clean)-1] <- "longitude"
colnames(br_clean)[ncol(br_clean)]<-"latitude"

br_aberadre <- crop(br_clean, ndvi_extent)
range(br_clean$latitude)
range(br_clean$longitude)
#this is at least long and lat separately 
#pLEASE
br_clean_vect <- vect(br_clean, geom = c("longitude", "latitude"), crs = "EPSG:4326")
br_clipped <- crop(br_clean_vect, ref)
coords_brclip <- crds(br_clipped)
df_filtered <- cbind(values(br_clipped), coords_brclip)
colnames(df_filtered)[ncol(df_filtered)-1] <- "longitude"
colnames(df_filtered)[ncol(df_filtered)]<-"latitude"

species_endangered <- read.csv("species.csv", sep = "\t")
species_vect <- vect(species_endangered, geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
species_aberdare <- crop(species_vect, ref)
coords_species <-crds(species_aberdare)
attribs <- values(species_aberdare)
species_fixed <- cbind(attribs, longitude = coords_species[,1], latitude = coords_species[,2])


species_filtered <- as.data.frame(species_aberdare)
nrow(species_filtered)
unique(species_filtered$species)

species_list <- split(species_filtered, species_filtered$species)
view(species_list)

#Aquila Nipples 
aquila_data <- subset(species_fixed, species == "Aquila nipalensis")
plot(ndvi_raster)
points(x= aquila_data$longitude, y=aquila_clean$latitude, col= "red")

aquila_vect <- vect(aquila_data, geom = c("longitude", "latitude"), crs = "EPSG:4326")
coords_an <- crds(aquila_vect)
aquila_clean <- data.frame(species = aquila_data$species, longitude = coords_an[,1], latitude = coords_an[,2])


#DAY TWO 
#makeing my species only in aberdare 
library(sf)
species_sf <- sf::st_as_sf(species_endangered, coords = c("decimalLongitude", "decimalLatitude")
                           , crs = crs(ndvi_extent))
species_cropped <- species_sf[ndvi_extent]
crs(ndvi_raster)
crs(species_vect)
#raster as a csv file with spexies, long and lat 
data_species <- values(species_vect)
species_complete <- data.frame(species = species_sf$species, longitude = coords_species[,1], latitude = coords_species[,2])
species_df <- as.data.frame(species_vect, geom = "XY")
names(species_df)[names(species_df) == "x"] <- "longitude"
names(species_df)[names(species_df) == "y"] <- "latitude"
#desired columns 
species_df <- species_df[, c("species", "longitude", "latitude")]
write.csv(species_df, "species_cropped.csv", row.names = FALSE)

library(dplyr)
#into animal and plant 
plants_aberadre <- species_endangered |> filter(species_endangered$kingdom == "Plantae") |> pull(species) |> unique()
animals_aberdare <- species_endangered |> filter(kingdom == "Animalia") |> pull(species)|> unique()
species_count1 <- species_df |> count(species, name = "occurences") |> arrange(species, desc(occurences))
range(species_df$latitude)
extent(ndvi_raster)
range(species_df$longitude)
