#this file read the ABR triangle survey data files and outputs usable data for 
#  modelling distribution and abundance.
#
## Erik Osnas, July 2024

library(tidyverse)
library(readxl)

#read in data from ABR excel files:
# there are three time periods to read in: (1) 1999-2016, (2) 2017-2018, (3) 2019, 2021-2023
# There are also double observer data for for 2017 - 2023, but it is unclear how these are related
#
#there are bird observations and transect coverage (surveyed or not) and GIS files to define transect. 
#
#1999-2016 bird observation
df1 <- read_xlsx(path = "../1999_2016ABRData/STEI_obs_1999-2016.xlsx")
#2019-2023 data
df3 <- read_xlsx(path = "../DOSAnalysis/RawData/2023/Barrow_STEI_Survey_2019–2023_working_ForFWS.xlsx", 
                 sheet = "Field data")
# There were 50 or more warnings (use warnings() to see the first 50)
# > warnings()
# Warning messages:
# 1: Expecting numeric in T2166 / R2166C20: got 'B1F'
# 2: Expecting numeric in T2167 / R2167C20: got 'B1F'
# 3: Expecting numeric in T2168 / R2168C20: got 'B1F'
#mixed type in Transect, need to specify character type for Transect
#...

#2017
df2.1 <- read_xlsx(path = "../DOSAnalysis/RawData/2017/Barrow_STEI_2017_forCat_updated7Dec17.xlsx", 
                          sheet = "Observations")
#2018
df2.2 <- read_xlsx(path = "../DOSAnalysis/RawData/2018/Barrow_STEI_Survey_2018_ForCat.xlsx", 
                   sheet = "Field data")
#load some GIS file and visualize:
library(sf)
triangle <- st_read(dsn = "../DOSAnalysis/Data/Barrow_Triangle_STEI_Aerial_SA")
ggplot(data = triangle) + geom_sf()
#2019 decoy tracks
tracks19 <- st_read(dsn = "../decoy_transects_ABR_2019", 
                    layer = "Utqiagvik_STEI_2019_for_Nathan_Tracks")
ggplot(data = triangle) + geom_sf() + geom_sf(data = tracks19, aes(col = "red"))
