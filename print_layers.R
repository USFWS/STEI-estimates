library(tidyverse)
library(sf)
################################################################################
# #plot bird obs, note path is relative to older shared folder, Triangle Survey
# layers <- st_layers(dsn = "../Triangle Survey/GIS data/GIS data 2002-2007/abr_survey2002")
# for(i in layers$name){
#   df <- st_read(dsn = "../Triangle Survey/GIS data/GIS data 2002-2007/abr_survey2002", layer = i)
#   p <- ggplot(data = df) + geom_sf() + labs(title = i)
#   print(p)
# }
# folder <- "abr_survey2003"
# layers <- st_layers(dsn = paste0("../GIS data 2002-2007/", folder))
# for(i in layers$name){
#   df <- st_read(dsn = paste0("../GIS data 2002-2007/", folder), layer = i)
#   p <- ggplot(data = df) + geom_sf() + labs(title = i)
#   print(p)
# }
# for(y in 2002:2007){
# folder <- paste0("abr_survey", y)
# layers <- st_layers(dsn = paste0("../GIS data 2002-2007/", folder))
# for(i in layers$name){
#   df <- st_read(dsn = paste0("../GIS data 2002-2007/", folder), layer = i)
#   p <- ggplot(data = df) + geom_sf() + labs(title = i)
#   print(p)
# }}
# ################################################################################
# #plot transects
# folder <- "ABR_aerial_transect_shapefiles"
# layers <- st_layers(dsn = paste0("data/", folder))
# layers
# p <- list()
# for(i in layers$name){
#   df <- st_read(dsn = paste0("data/", folder), layer = i)
#   p[[i]] <- ggplot(data = df) + geom_sf() + labs(title = i)
# }
# ggpubr::ggarrange(plotlist = p, ncol = 2, nrow = 2)
################################################################################
#plot transects and birds obs and color by transect number
# library(readxl)
# df1 <- read_xlsx(path = "data/STEI_obs_1999-2016.xlsx")
#plot the "A lines" as these evidently were flow 1999-2016
trans <-  st_read(dsn = "data/ABR_aerial_transect_shapefiles", 
                  layer = "ABR_BarrowTriangle_STEI_Alines")
#transect end points
endpts <- st_read(dsn = "data/ABR_aerial_transect_shapefiles", 
                  layer = "ABR_BarrowTriangle_STEI_AlineEndPts") |>
  mutate(endpts, Transect = as.numeric(str_sub(NAME, 2, 3))) |>
  mutate(Transect = factor(Transect)) 
# #filter to first 10 transect and plot
# pick <- 41:45
# df1.10 <- df1 |>
#   st_as_sf(coords = c("LongDD83", "LatDD83"), crs = 4269) |>
#   st_transform(crs = 4326) |>
#   mutate(Transect = factor(Transect)) |>
#   filter(Transect %in% pick) |>
#   mutate(Transect = factor(as.numeric(Transect)))
# endpts10 <- endpts |>
#   filter(Transect %in% pick)
#   
# ggplot(data = trans) + geom_sf() + geom_sf(data = df1.10, aes(col = Transect)) + 
#   geom_sf(data = endpts10, aes(col = Transect))
#just endpoints
ggplot(data = endpts) + geom_sf()
#transform endpoints into linestrings
ABRlines <- endpts |> group_by(Transect) |>
  summarize() |>
  st_cast("LINESTRING") |>
  mutate(fTransect = factor(Transect))
plot(st_geometry(ABRlines)) #worked but some weirdness
ggplot(data = ABRlines) + geom_sf(aes(col = fTransect))
ABRlines <- drop_na(ABRlines)
ggplot(data = ABRlines) + geom_sf(aes(col = fTransect))
#view in tmap
library(tmap)
tmap_mode("view")
tm_shape(ABRlines) + tm_lines() #looks good, wait!
#some weirdness in middle transects:
tm_shape(endpts) + tm_dots(popup.vars = "Transect")
#transect 70 and other goes from E to W to center
#try arranging by latitude
ABRlines <- endpts |> 
  drop_na() |>
  group_by(Transect) |>
  summarize(do_union = FALSE) |> #need to set do_union = FALSE, not sure why, see https://stackoverflow.com/questions/57607692/joining-points-together-to-create-single-line-and-mapping-output
  st_cast("LINESTRING") |>
  mutate(fTransect = factor(Transect))
tm_shape(ABRlines) + tm_lines()
#add survey area polygon
triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA") |>
  st_transform(crs = 4326)
ggplot() + geom_sf(data = triangle, col = "red", linewidth = 1) + 
  geom_sf(data = ABRlines)
#looks like transects are on boundary
tm_shape(triangle) + tm_polygons(col = "red", fill = "red") + 
  tm_shape(ABRlines) + tm_lines(popup.vars = "Transect")
#what about the "Blines"
ABRlinesB <- st_read(dsn = "data/ABR_aerial_transect_shapefiles", 
                  layer = "ABR_BarrowTriangle_STEI_BlineEndPts") |>
  mutate(Transect = as.numeric(str_sub(NAME, 2, 3))) |>
  mutate(Transect = factor(Transect)) |>
  drop_na() |>
  group_by(Transect) |>
  summarize(do_union = FALSE) |>
  st_cast("LINESTRING") |>
  mutate(fTransect = factor(Transect))
tm_shape(triangle) + tm_polygons(col = "red", fill = "red") + 
  tm_shape(ABRlinesB) + tm_lines(popup.vars = "Transect")
#plot
tm_shape(triangle) + tm_polygons(col = "red", fill = "red") + 
  tm_shape(ABRlines) + tm_lines(popup.vars = "Transect") + 
  tm_shape(ABRlinesB) + tm_lines(col = "blue", popup.vars = "Transect")
#combine into one 
ABRlines <- ABRlines |> mutate(Panel = "A") |>
  rbind(mutate(ABRlinesB, Panel = "B")) |>
  arrange(Transect, Panel)
tm_shape(ABRlines) + tm_lines(col = "Panel", popup.vars = "Transect")
################################################################################
#Add some bird observations and see how this works out
#There is no A/B panel variable, will have to plot and see
library(readxl)
df1 <- read_xlsx(path = "data/STEI_obs_1999-2016.xlsx") |>
  arrange(Year, Transect) |>
  st_as_sf(coords = c("LongDD83", "LatDD83"), crs = 4269) |>
  st_transform(crs = 4326) |>
  mutate(fTransect = factor(Transect))
#plot
tm_shape(ABRlines) + tm_lines(col = "Panel", popup.vars = "Transect") + 
  tm_shape(df1) + tm_dots()
#see email in data/ABR_aerial_transect_shapefiles/Mail - Osnas, Erik E - Outlook.pdf
# for history of effort. Only A lines where flown 1999-2016
################################################################################
library(tmap)
tmap::tmap_mode("view")
tmap::tm_shape(trans) + tmap::tm_lines() + 
  tmap::tm_shape(df1.10) + tmap::tm_dots(color = "red", size = 0.1) + 
  tmap::tm_basemap(server = "Esri.WorldImagery")

strips <- st_transform(trans, crs = 26904) |> 
  st_buffer( dist = 200, endCapStyle = "SQUARE") |>
  st_transform(crs = 4326)
#2019 decoy tracks
tracks19 <- st_read(dsn = "data/decoy_transects_ABR_2019", 
                    layer = "Utqiagvik_STEI_2019_for_Nathan_Tracks") |>
  st_transform(crs = 4326)
tmap::tm_shape(strips) + tmap::tm_polygons(fill= "orange") + 
  tmap::tm_shape(trans) + tmap::tm_lines() + 
  tmap::tm_shape(tracks19) + tmap::tm_lines(col = "red") + 
  tmap::tm_basemap(server = "Esri.WorldImagery")
################################################################################
#plot gpx transect file received from TimO on 20240822
st_layers(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx")
# Driver: GPX 
# Available layers:
#   layer_name     geometry_type features fields crs_name
# 1    waypoints             Point      421     24   WGS 84
# 2       routes       Line String      141     13   WGS 84
# 3       tracks Multi Line String        0     12   WGS 84
# 4 route_points             Point      421     26   WGS 84
# 5 track_points             Point        0     26   WGS 84
trans <- st_read(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx", 
                 layer = "routes")
ggplot(data = trans) + geom_sf()
#Nice!
tracks <- st_read(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx", 
                  layer = "waypoints")
ggplot(data = tracks) + geom_sf()
#those are waypoints, but nice!

