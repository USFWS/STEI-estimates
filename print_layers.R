library(tidyverse)
library(sf)

layers <- st_layers(dsn = "../GIS data 2002-2007/abr_survey2002")
for(i in layers$name){
  df <- st_read(dsn = "../GIS data 2002-2007/abr_survey2002", layer = i)
  p <- ggplot(data = df) + geom_sf() + labs(title = i)
  print(p)
}
folder <- "abr_survey2003"
layers <- st_layers(dsn = paste0("../GIS data 2002-2007/", folder))
for(i in layers$name){
  df <- st_read(dsn = paste0("../GIS data 2002-2007/", folder), layer = i)
  p <- ggplot(data = df) + geom_sf() + labs(title = i)
  print(p)
}
for(y in 2002:2007){
folder <- paste0("abr_survey", y)
layers <- st_layers(dsn = paste0("../GIS data 2002-2007/", folder))
for(i in layers$name){
  df <- st_read(dsn = paste0("../GIS data 2002-2007/", folder), layer = i)
  p <- ggplot(data = df) + geom_sf() + labs(title = i)
  print(p)
}}
