#this file reads the ABR triangle survey data files and attemps to output usable data for 
#  modelling distribution and abundance.
#
## Erik Osnas, July 2024
## Modified, August, 2024 after getting better data from ABR
# 
# library(tidyverse)
# library(readxl)
# 
# # #read in data from ABR excel files:
# # there are three time periods to read in: (1) 1999-2016, (2) 2017-2018, (3) 2019, 2021-2023
# # There are also double observer data for for 2017 - 2023, but this will not be used now
# #
# # Data types needed are (1) bird observations, (2) transect coverage (surveyed or not),
# #  and (3) GIS files to define transects and survey area (the survey design and/or 
# #  survey effort or flight tracks). I can only find transect coverage from 1999-2016 and a 
# #  polygon shapefile of the triangle area. I cannot find any GIS file of the transects.
# #  This could be as simple as a text file of transect end points and a description of
# #  the flight path (line of latitude or great circle; these are basically equivalent 
# #  in this small area). This might also take the form of flight tracks. 
# #  
# #  For bird observations, each time period seems to be in a different format and 
# #  naming convention. This will need to be reformatted. 
# #
# #1999-2016 bird observations
# df1 <- read_xlsx(path = "data/STEI_obs_1999-2016.xlsx")
# #2019-2023 data
# df3 <- read_xlsx(path = "data/Barrow_STEI_Survey_2019–2023_working_ForFWS.xlsx", 
#                  sheet = "Field data")
# # There were 50 or more warnings (use warnings() to see the first 50)
# # > warnings()
# # Warning messages:
# # 1: Expecting numeric in T2166 / R2166C20: got 'B1F'
# # 2: Expecting numeric in T2167 / R2167C20: got 'B1F'
# # 3: Expecting numeric in T2168 / R2168C20: got 'B1F'
# #mixed type in Transect, need to specify character type for Transect
# #...
# #ISSUE 1.0: non-tidy data. Multiple meaning combined into one variable (I assume).
# #  What does "B" and "F" and all other character codes mean? 
# #  Should these be split into multiple columns?
# 
# #2017
# df2.1 <- read_xlsx(path = "data/Barrow_STEI_2017_forCat_updated7Dec17.xlsx", 
#                           sheet = "Observations")
# #2018
# df2.2 <- read_xlsx(path = "data/Barrow_STEI_Survey_2018_ForCat.xlsx", 
#                    sheet = "Field data")
# #NOTE: sheets are named differently across years
# #
# ## try to combine birds into one data set:
# View(df1)
# df1 <- df1 |> rename(Lat = LatDD83, Lon = LongDD83) |>
#   filter(On_Transect == "Y", Species == "STEI") |>
#   select(Species, Year, Transect, Lat, Lon, Males, Females, Pairs, Flying) 
# #ISSUE 1.2: assuming the variable name is trying to provide GIS metadata. No CRS is found,
# #  so I will assume this is NAD83 due to the file name. This is strange because 
# #  I think GPSs use WGS84. There is essentially little practical difference, I believe. 
# #  I will assume this is EPSG:4269 
# #now try df2
# View(df2.1)
# #Same ISSUE 1.2
# #ISSUE 1.3: Do character "NA"s or NA mean zero observed? I will assume so. Generally,
# #  an affirmation of a numeric zero is preferred. Here in df2.1 character "NA"s are used.
# #  But is this true for all variable? How are zeros and NAs handled by variable. 
# #  I will assume that for "Males", "Females" and "Pairs" "NA" are zeros, bit for other it is not clear.
# df2 <- df2.1 |> rename(Lat = Lat_DD83, Lon = Lon_DD83) |>
#   filter(On_Trans == "Y", Species == "STEI") |>
#   select(Species, Year, Transect, Lat, Lon, Males, Females, Pairs, Flying) |>
#   mutate(Males = as.numeric(replace(Males, Males == "NA", 0)), 
#          Females = as.numeric(replace(Females, Females == "NA", 0)), 
#          Pairs = as.numeric(replace(Pairs, Pairs == "NA", 0)))
# View(df2.2)
# #ISSUE 1.4: Now we have switched coordinate names and presumably CRS. Will assume EPSG:4326
# #ISSUE 1.3.1: Now we are using missing data NAs. I will assume these are zeros, as above.  
# tmp <- df2.2 |> rename(Lat = Lat_WGS84, Lon = Lon_WGS84) |>
#   filter(On_Trans == "Y", Position != "RR", Species == "STEI") |>
#   select(Species, Year, Transect, Lat, Lon, Males, Females, Pairs, Flying) |>
#   mutate(Males = as.numeric(replace(Males, is.na(Males), 0)), 
#          Females = as.numeric(replace(Females, is.na(Females), 0)), 
#          Pairs = as.numeric(replace(Pairs, is.na(Pairs), 0)))
# df2 <- rbind(df2, tmp)
# rm(df2.1, df2.2)
# #NOTE: How to handle flying birds. Delete or use?
# 
# #Are transects with no observation of STEI missing?
# table(df2$Transect, df2$Year)
# #NOTE: Yes, need to fill in zeros
# #ISSUE 1.5: Transect in 2017-2018 is character whereas before it is numeric. Some 
# # transects have leading "D"s, which might mean "decoy transects" but it is unclear 
# # if these are different transects than normal transects. Some transects have trailing "E" or 
# # "W", which I assume indicates direction of flight. This is non-tidy data with transect 
# # indicating at least three different types of information. 
# #
# #df3
# #ISSUE 1.6: New name for Species column. Appears to have been some confusion with 
# # between decoys and real birds, where real birds were thought to be decoys. Some 
# # rules were used to derive a correction and a new Species identification column. I 
# # will use the "Species_final" column as Species and assume this is the intent.  
# tmp <- df3 |> rename(Species = Species_final, Lat = Lat_WGS84, Lon = Lon_WGS84) |>
#   filter(On_Trans == "Y", Position != "RR", Species == "STEI") |>
#   select(Species, Year, Transect, Lat, Lon, Males, Females, Pairs, Flying) |>
# #ISSUE 1.3.1 as above
#   mutate(Males = as.numeric(replace(Males, is.na(Males), 0)), 
#          Females = as.numeric(replace(Females, is.na(Females), 0)), 
#          Pairs = as.numeric(replace(Pairs, is.na(Pairs), 0)))
# df <- rbind(df1, df2, tmp) |>
#    arrange(Year, Transect)
# #finish birds data
# summary(df)
# #ISSUE 1.6.1: Evidently in 2023, "Species_final" was not used, so the "Species" 
# #  field needs to depend on year, at least I assume this is the case. But there are decoys?
# #  Was the data never checked as in previous years for "species_final" determination? Should 
# #  I assume error rates as before? I Will ignore 2023 data for now. 
# ################################################################################
# #Read in 1999-2016 transect effort data
# effort <- read_xlsx(path = "data/Transect_Coverage_1999-2016.xlsx") |>
#   pivot_longer(cols = 3:20, names_to = "Year", values_to = "Surveyed") |>
# #ISSUE 2.0: NAs in effort data instead of 0. Will assume NAs mean "not surveyed". 
# #  Usually a dichotomous factor variable would be used (e.g., 0/1, Yes/No, etc.)
#   mutate(Surveyed = replace(Surveyed, is.na(Surveyed), 0)) |>
#   arrange(Year, Transect)
# 
# group_by(effort, Year) |> summarise(N = sum(Surveyed))
# # # A tibble: 18 × 2
# # Year      N
# # <chr> <dbl>
# #   1 1999     62
# # 2 2000     62
# # 3 2001     62
# # 4 2002     62
# # 5 2003     62
# # 6 2004     62
# # 7 2005     62
# # 8 2006     62
# # 9 2007     31
# # 10 2008     62
# # 11 2009     37
# # 12 2010     31
# # 13 2011     31
# # 14 2012     51
# # 15 2013     31
# # 16 2014     31
# # 17 2015     31
# # 18 2016     31
# #ISSUE 2.1: Odd, in 2012 did all transects until 41, then did every other. 
# #  in 2009, did all to 19, then did not do 20 and did every other one, then 
# #    did none after 55. How to handle this. Will need to stratify in 2012 for any 
# #    design based estimate. Not sure what to do for 2009, might stratify but model 
# #    based estimate might be best here.
# #
# #ISSUE 2.2: I cannot find any effort information for 2017-2023. Except for flight 
# #  tracks in 2019, see below. 
# ################################################################################
# #load some GIS files and visualize:
# library(sf)
# #survey area
# triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA")
# dfgeo <- st_as_sf(df, coords=c("Lon", "Lat"), crs = 4269) 
# #NOTE: need to transform to common crs, right now (above) in both 4269 and 4326, which should be amost identical
# #plot observations
# ggplot(data = triangle) + geom_sf() + geom_sf(data = dfgeo, aes(col = Transect))
# #Small GIS ISSUE 3.0: 2019 ABR report indicates an area of the Triangle to be 2757 km^2. 
# #  I calculate 
# #   > units::set_units(st_area(triangle), "km^2")
# #   2719.499 [km^2]
# #  Probably due to area calculation in different crs. Might be due to different 
# #  polygon definition. Above is provided in EPSG:26904. 
# #  When I transform to EPSG 4326 or 4269, I get 2700.73 km^2:
# #   > units::set_units(st_area(st_transform(triangle, crs = 4326)), "km^2")
# #   2700.73 [km^2]
# #   > units::set_units(st_area(st_transform(triangle, crs = 4269)), "km^2")
# #   2700.73 [km^2]
# #
# #2019 decoy tracks
# tracks19 <- st_read(dsn = "data/decoy_transects_ABR_2019", 
#                     layer = "Utqiagvik_STEI_2019_for_Nathan_Tracks")
# ggplot(data = triangle) + geom_sf() + geom_sf(data = tracks19, aes(col = "red"))
# ggplot(data = tracks19)+geom_sf()
# #NOTE: why the big curve?
# tmap::tmap_mode("view")
# tmap::tm_shape(tracks19) + tmap::tm_lines() + tmap::tm_basemap(server = "Esri.WorldImagery")
# #not clear why the departure from design (no airport or obvious hazard)
# 
# #can we re-con struct the transects from the observations?
# sdf <- st_as_sf(df, coords = c("Lon", "Lat"), crs = 4326) |>
#   st_transform(crs=26904)
# 
# tdf <- cbind(st_drop_geometry(sdf), st_coordinates(sdf)) |>
#   group_by(Transect) |> summarise(mlat = mean(X), sdLat = sd(X)) |>
#   mutate(Transect = as.numeric(Transect)) |>
#   drop_na() |>
#   arrange(Transect) |>
#   mutate(lag1 = (lead(mlat) - mlat)/1000)
# ggplot(data = tdf) + geom_histogram(aes(x = lag1))
# #NO! maybe group by year?
# tdf <- cbind(st_drop_geometry(sdf), st_coordinates(sdf)) |>
#   group_by(Transect, Year) |> summarise(mlat = mean(X), sdLat = sd(X)) |>
#   mutate(Transect = as.numeric(Transect)) |>
#   drop_na() |>
#   arrange(Transect) |>
#   mutate(lag1 = (lead(mlat) - mlat)/1000)
# ggplot(data = tdf) + geom_histogram(aes(x = lag1))
# #No, seem like too many too far apart
# #plot by transect
# trans <- unique(sdf$Transect)
# tmp <- arrange(sdf, Transect)
# for(i in trans){
#   print(ggplot(data = triangle) + geom_sf() + 
#           geom_sf(data = filter(tmp, Transect == i)) + 
#           labs(title = paste0("Transect ", i)))
# }
#I think a different transect numbering system was used each year (or used inconsistently) 
################################################################################
#20240823
#After many email exchanges and one in-person meeting, got what I think are 
# usable files from TimO at ABR
# Let see if they work!
# GPX file of transects with standardized names appears good, see lines in file 
#  print_layers.R
library(tidyverse)
library(readxl)
library(sf)
library(tmap)
#examine bird observations:
birds <- read_xlsx(path = "data/STEI_obs_1999-2023.xlsx", sheet = "STEI Obs 1999-2023") |> 
  rename(Transect = "Standard Transect") |>
  arrange(Year, Transect)
plot(table(birds$Year)) #odd that exactly 50 observation were made in both 1999 and 2000??
#seems OK based on plot, see below, BUT...
#in 2019 SSA the number of observed eider in Table "Appendix 1" of Appendix A is 54 and 48??
effort <- read_xlsx(path = "data/STEI_obs_1999-2023.xlsx", sheet = "Years surveyed") |> 
  pivot_longer(2:26, names_to="Year", values_to="Surveyed") |> 
  drop_na() |>
  arrange(Year, Transect) |>
  mutate(Year = as.numeric(Year)) |>
  filter(Surveyed == "Y") |>
  select(-Surveyed)
plot(table(effort$Year))
#read and plot transects:
trans <- st_read(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx", 
                 layer = "routes") |>
  select(Transect = name)
ggplot(data = trans) + geom_sf()
tmap_mode("view")
tm_shape(trans) + tm_lines() #looks good!
birds.sf <- st_as_sf(birds, coords = c("LongDD83", "LatDD83"), crs = 4269) |>
  st_transform(crs = 4326)
tm_shape(trans) + tm_lines(popup.vars = "Transect", lwd = 1) + 
  tm_shape(birds.sf) + tm_dots(popup.vars = c("Transect", "Year"), size = 0.5)
tm_shape(trans) + tm_lines(popup.vars = "Transect", lwd = 1) + 
  tm_shape(filter(birds.sf, Year <= 2000)) + tm_dots(col ="Year", alpha = 0.5, size = 0.5)

# tm_shape(trans) + tm_lines(popup.vars = "Transect") + 
#   tm_shape(filter(birds.sf, Year == 2023, Unique == "Y")) + tm_dots(popup.vars = "Transect")
#only saw 4!
################################################################################
## Format for analysis: (1) make grid, (2) make sampled transects for each year, 
#  (3) "segmentize"/grid transects for each year, (4) assign observations, 
#  including zeros, to transect segments for each year 
#  
# (1) make grid:
# read in Triangle survey area:
triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA")
grid <- st_intersection(triangle, st_make_grid(x=triangle, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), GridArea = st_area(.)) |>
  select(Sample.Label, GridArea)
ggplot(data = st_transform(grid, crs = 4326)) + geom_sf(alpha = 0) + 
geom_sf(data = trans[10:71,], col = "red")
tm_shape(grid) + tm_polygons() + 
  tm_shape(trans) + tm_lines(col = "red", lwd = 1)

##
# (2) make set of sampled transects:
alltrans <- left_join(effort, trans, by = "Transect") |>
  st_as_sf(sf_column_name="geometry", crs = st_crs(trans)) |>
  st_transform(crs = st_crs(grid))

#loop through each year to make transect segments and combine with bird data to 
# include zeros. Can't figure out how to do this in one tidyverse pipe
source("https://raw.githubusercontent.com/USFWS/ACP-Mapping/main/map_density_functions.R")
# needed to modify get_data function but did not push to GitHub, thus source modified function here
source("get_data2.R")
birds2 <- birds.sf |> cbind(st_coordinates(birds.sf)) |> st_drop_geometry() |>
  rename(Lon = X, Lat = Y, single = Males, pairs = Pairs) |>
  mutate(Observer = "999", Month = 6, Day = 99, Time = 999) |>
  pivot_longer(cols = c("single", "pairs", "Females"), names_to = "Obs_Type", 
               values_to = "Num") |>
  filter(Obs_Type != "Females") |>
  select(Species, Year, Month, Day, Time, Observer, Num, Obs_Type, Lon, Lat)
#need to rename Area in grid
grid2 <- rename(grid, Area = GridArea)
df <- data.frame(NULL)
#no birds observed in 2009! how to deal with that?
#function get_data2 does not deal with no birds observed!
#we will skip 2009 and augment with zeros outside the function to explore how the 
#  GAM/model deals with this
for(y in unique(birds2$Year)){ 
  print(paste0("Year == ", y))
  yeffort <- filter(effort, Year == y) |>
    left_join(trans, by = "Transect") |>
    st_as_sf(sf_column_name="geometry", crs = st_crs(trans)) |>
    st_transform(crs = st_crs(grid))
  segs <- make_segments(x=grid2, y = yeffort, w = 400)
  
  tmp <- get_data2(x = filter(birds2, Year == y), y = segs, area = triangle, 
                    Spp = "STEI", grid = grid2, buff = 0)
  df <- rbind(df, tmp)
}
#now add zeros for 2009
yeffort <- filter(effort, Year == 2009) |>
  left_join(trans, by = "Transect") |>
  st_as_sf(sf_column_name="geometry", crs = st_crs(trans)) |>
  st_transform(crs = st_crs(grid))
#make segments
segs <- make_segments(x=grid2, y = yeffort, w = 400) |>
  mutate(Length = units::drop_units(Length)/1000, 
         Area = units::drop_units(Area)/1000000)
cen <- st_centroid(segs) |> st_coordinates()
tmp <- filter(birds2, Year == 2009) |>
  mutate(Segment.Label = as.character(NA)) |>
  right_join(st_drop_geometry(segs)) |>
  cbind(cen) |>
  mutate(Observer = 999, Count = 0, Year = 2009, 
         logArea = log(Area)) |>
  select(names(df))

df <- rbind(df, tmp) |>
  arrange(Year, Segment.Label, Sample.Label)
  
#plot to check
tmp <- filter(df, Year == 1999) |>
  st_as_sf(coords=c("X", "Y"), crs = st_crs(segs))
ggplot(data = triangle) + geom_sf() + 
  geom_sf(data = tmp, col = "lightgrey", size = 1) + 
  geom_sf(data = filter(tmp, Count > 0))
#looks good!
#write segmentized data to file for later use
#saveRDS(df, file = "data/segmentized_triangle_data_fliers.RDS") #not saved
################################################################################
#Now fit GAM
library(mgcv)
fit0 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)),
            offset = logArea, family = nb, method = "REML", data = df)
fit1 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10),
            offset = logArea, family = nb, method = "REML", data = df)
fit2 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 10) + 
              ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("ds", "cr"), 
                 m=list(c(1,.5),rep(0,0))),
            offset = logArea, family = nb, method = "REML", data = df)
AIC(fit0, fit1, fit2)
summary(fit0)
summary(fit1)
summary(fit2)
gam.check(fit1)
library(DHARMa)
library(mgcViz)
library(gratia)
best <- fit1
appraise(best)
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
#year trend looks too smooth!
b <- getViz(best, nsim = 50)
ck1 <- check2D(b, x1 = "X", x2 = "Y")
ck1 + l_gridCheck2D(gridFun = mean)
testDispersion(best) #looks under dispersed
simulationOutput <- simulateResiduals(fittedModel = best, n = 500)
plot(simulationOutput)
plotResiduals(simulationOutput)
testZeroInflation(simulationOutput) #looks good
#try increasing k for year trends
fit1.1 <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 20),
                  offset = logArea, family = nb, method = "REML", data = df)
summary(fit1.1) #looks like k was too small for year above!
best <- fit1.1
appraise(best)
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
save.image()
df$fYear <- factor(df$Year)
fit1.1.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 20) + 
                   s(fYear, bs = "re"),
              offset = logArea, family = nb, method = "REML", data = df)
AIC(fit1.1, fit1.1.re)
best <- fit1.1.re
appraise(best)
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
fit1.re <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(fYear, bs = "re"),
                 offset = logArea, family = nb, method = "REML", data = df)
best <- fit1.re
appraise(best)
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
AIC(fit0, fit1, fit2, fit1.re, fit1.1, fit1.1.re)
summary(fit1.1) 
best <- fit1.1
appraise(best)
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
b <- getViz(best, nsim = 50)
ck1 <- check2D(b, x1 = "X", x2 = "Y")
ck1 + l_gridCheck2D(gridFun = mean)
testDispersion(best) #looks good
simulationOutput <- simulateResiduals(fittedModel = best, n = 500)
plot(simulationOutput) #q-q plot looks good
plotResiduals(simulationOutput) #not good, not sure how to interpret; 
#  this plot seems to suggest problems
testZeroInflation(simulationOutput) #looks good
testUniformity(simulationOutput)
testOutliers(simulationOutput)
#try other distributions:
#tweedie
fit1.1.tw <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 20),
              offset = logArea, family = tw, method = "REML", data = df)
summary(fit1.1.tw) 
best <- fit1.1.tw
appraise(best)
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
b <- getViz(best, nsim = 50)
ck1 <- check2D(b, x1 = "X", x2 = "Y")
ck1 + l_gridCheck2D(gridFun = mean)
testDispersion(best) #not so good 
simulationOutput <- simulateResiduals(fittedModel = best, n = 500)
plot(simulationOutput) #q-q plot looks good, similar to above, but significant deviation
plotResiduals(simulationOutput) #not good, not sure how to interpret; 
#  this plot seems to suggest problems
testZeroInflation(simulationOutput) #looks good
testUniformity(simulationOutput)
testOutliers(simulationOutput)
#overall, tweedie not as good as NB
#try simple Poisson
fit1.1.po <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 20),
                 offset = logArea, family = poisson, method = "REML", data = df)
summary(fit1.1.po) 
best <- fit1.1.po
appraise(best) #that does not look good, try Darmha
draw(best, select = 1, dist = 0.02, rug = FALSE)
tryCatch(draw(best, select = 2, dist = 0.02, rug = FALSE), error = function(e) e)
b <- getViz(best, nsim = 50)
ck1 <- check2D(b, x1 = "X", x2 = "Y")
ck1 + l_gridCheck2D(gridFun = mean)
testDispersion(best) #not so good!
simulationOutput <- simulateResiduals(fittedModel = best, n = 500)
plot(simulationOutput) #q-q plot looks good, similar to above, but significant deviation
plotResiduals(simulationOutput) #not good, not sure how to interpret; 
#  this plot seems to suggest problems
testZeroInflation(simulationOutput) #not so good!
testUniformity(simulationOutput)
testOutliers(simulationOutput, type = "bootstrap")
#fit1.1 looks the best, but all seem to give similar spatial and temporal patterns
save.image()
###############################
## Fit same model but with no flying birds
rm(list = ls())
load(".RData")
## Make new data frame without fliers
birds2 <- birds.sf |> cbind(st_coordinates(birds.sf)) |> st_drop_geometry() |>
  rename(Lon = X, Lat = Y, single = Males, pairs = Pairs) |>
  mutate(Observer = "999", Month = 6, Day = 99, Time = 999) |>
  pivot_longer(cols = c("single", "pairs", "Females"), names_to = "Obs_Type", 
               values_to = "Num") |>
  filter(Obs_Type != "Females", Flying == "N") |> #REMOVE FLIERs!!!!
  select(Species, Year, Month, Day, Time, Observer, Num, Obs_Type, Lon, Lat)
#need to rename Area in grid
grid2 <- rename(grid, Area = GridArea)
df <- data.frame(NULL)
for(y in unique(birds2$Year)){ 
  print(paste0("Year == ", y))
  yeffort <- filter(effort, Year == y) |>
    left_join(trans, by = "Transect") |>
    st_as_sf(sf_column_name="geometry", crs = st_crs(trans)) |>
    st_transform(crs = st_crs(grid))
  segs <- make_segments(x=grid2, y = yeffort, w = 400)
  
  tmp <- get_data2(x = filter(birds2, Year == y), y = segs, area = triangle, 
                   Spp = "STEI", grid = grid2, buff = 0)
  df <- rbind(df, tmp)
}
#now add zeros for 2009
yeffort <- filter(effort, Year == 2009) |>
  left_join(trans, by = "Transect") |>
  st_as_sf(sf_column_name="geometry", crs = st_crs(trans)) |>
  st_transform(crs = st_crs(grid))
#make segments
segs <- make_segments(x=grid2, y = yeffort, w = 400) |>
  mutate(Length = units::drop_units(Length)/1000, 
         Area = units::drop_units(Area)/1000000)
cen <- st_centroid(segs) |> st_coordinates()
tmp <- filter(birds2, Year == 2009) |>
  mutate(Segment.Label = as.character(NA)) |>
  right_join(st_drop_geometry(segs)) |>
  cbind(cen) |>
  mutate(Observer = 999, Count = 0, Year = 2009, 
         logArea = log(Area)) |>
  select(names(df))

df <- rbind(df, tmp) |>
  arrange(Year, Segment.Label, Sample.Label)
#write segmentized data to file for later use
saveRDS(df, file = "data/segmentized_triangle_data_nofliers.RDS")
######################
## fit model 
library(mgcv)
fit1.1.nofliers <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 20),
              offset = logArea, family = nb, method = "REML", data = df)
saveRDS(fit1.1.nofliers, file = "results/fit1.1.nofliers.RDS")
summary(fit1.1.nofliers)
plot(fit1.1.nofliers)
##Fit a x,y only model for later use in dsims:
df <- readRDS(file = "data/segmentized_triangle_data_nofliers.RDS") |>
  rename(x = X, y = Y)
fit <- gam(Count~s(x, y, bs="ds", k = 200, m=c(1,.5)),
           offset = logArea, family = nb, method = "REML", data = df)
saveRDS(fit, file = "results/fit.nofliers.RDS")
