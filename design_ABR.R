#Use a design-based estimate for the triangle data
library(tidyverse)
library(sf)
library(units)
library(readxl)
#read in data
triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA")
TriangleArea <- st_area(triangle) |> set_units("km^2") |> drop_units()
trans <- st_read(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx", 
                 layer = "routes") |>
  select(Transect = name)
#add length to transect df
trans <-   mutate(trans, Length = units::set_units(st_length(trans), "km"))
#effort data
effort <- read_xlsx(path = "data/STEI_obs_1999-2023.xlsx", sheet = "Years surveyed") |> 
  pivot_longer(2:26, names_to="Year", values_to="Surveyed") |> 
  drop_na() |>
  arrange(Year, Transect) |>
  mutate(Year = as.numeric(Year)) |>
  filter(Surveyed == "Y") |>
  select(-Surveyed) |>
  left_join(st_drop_geometry(trans))
#read in birds and transform
birds <- read_xlsx(path = "data/STEI_obs_1999-2023.xlsx", sheet = "STEI Obs 1999-2023") |> 
  rename(Transect = "Standard Transect", Lon = "LongDD83", Lat = "LatDD83") |>
  arrange(Year, Transect) |>
  st_as_sf(coords = c("Lon", "Lat"), crs = 4269) |>
  st_transform(crs = 4326)
birds2 <- birds |> cbind(st_coordinates(birds)) |> st_drop_geometry() |>
  rename(Lon = X, Lat = Y, single = Males, pairs = Pairs) |>
  mutate(Observer = "999", Month = 6, Day = 99, Time = 999) |>
  pivot_longer(cols = c("single", "pairs", "Females"), names_to = "Obs_Type", 
               values_to = "Num") |>
  filter(Obs_Type != "Females", On_Transect == "Y") |>
  select(Species, Year, Month, Day, Time, Observer, Transect, Num, Obs_Type, 
         Flying, Lon, Lat) |>
  filter(Flying == "N") #remove flying birds!!
  
birds3 <- birds2 |>  right_join(effort) |> #right join to add zeros
  mutate(Area = Length*set_units(0.4, "km"), 
         Num = replace(Num, is.na(Num), 0)) |> #replace Nas with 0 observations
  group_by(Year, Transect) |>
  #INDICATED BIRDS!!!
  summarise(Num = 2*set_units(sum(Num), "1"), Area = mean(Area)) |>
  select(Year, Transect, Area, Num) 
design.est <- birds3 |> 
  group_by(Year) |>
  summarise(Density = sum(Num)/sum(Area), n = n(), mean_Num = mean(Num), 
            sd_Num = sd(Num), 
            sd_Area = sd(Area), 
            var_Density = sum( (Num - Density*Area)^2 )/(n - 1), 
            Total = drop_units(Density*TriangleArea), 
            sd_Total = drop_units(TriangleArea*sqrt((1 - n/124)*var_Density/n))) |>
  mutate(upper = Total + 2*sd_Total, lower = max(0, Total - 2*sd_Total))
#plot it!
gg <- ggplot(data = design.est) + 
  geom_pointrange(aes(x = Year, y = Total, ymin = lower, ymax = upper))
print(gg)
#look at sum_Num v. sum(area)
ggplot(data = birds3, aes(x = Area, y = Num)) + geom_point() + 
  geom_smooth(method = "lm")
#RATIO MODEL NO GOOD!
#look at mean variance
ggplot(data = design.est, aes(x = mean_Num, y = sd_Num^2)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0)
#Try a simple plot/mean estimate
design.est <- birds3 |> 
  mutate(Area = drop_units(Area), Num = drop_units(Num)) |>
  group_by(Year) |>
  summarise(sum_Num = sum(Num), 
            n = n(),
            Area = sum(Area), 
            Total = sum_Num*TriangleArea/Area,
            sd_Num = sd(Num), 
            sd_Total = (TriangleArea/Area)*sqrt( (1 - Area/TriangleArea)*(n*(sd_Num)^2) )) |>
  mutate(upper = Total + 2*sd_Total, 
         lower = if_else(Total - 2*sd_Total < 0, 0, Total - 2*sd_Total))

ggplot(data = design.est) +  
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Total)) + 
  geom_point(aes(x = Year, y = Total)) +
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Bird Index") +
  labs(title = "Design-based Estimated breeding bird index in Triangle (no detection)")
ggsave("results/trianle_raw_design_ibb_year.png")
# ggplot(data = design.est, aes(x = mean_Density, y = sd_Density^2)) + geom_point() + 
#   geom_smooth(method = "lm")
# #compare to AKaerial estimate
# df <- AKaerial::ACPHistoric$expanded.table |>
#   filter(Species == "STEI") |>
#   group_by(Year, strata) |>
#   summarise(n = n(), Total = mean(ibbtotal.est/2), 
#             diff_Total = max(ibbtotal.est)/2 - min(ibbtotal.est)/2, 
#             sd_Total = sqrt(sum(var.Nib/4)/n + (diff_Total/2)^2)) |>
#   group_by(Year) |>
#   summarise(Total = sum(Total), sd_Total = sqrt(sum(sd_Total^2))) |> 
#   mutate(upper = Total + 2*sd_Total, 
#          lower = if_else(Total - 2*sd_Total < 0, 0, Total - 2*sd_Total), 
#          Type = "Total") |>
#   select(Year, Total, upper, lower, Type)
# ggplot(data = df) + 
#   geom_pointrange(aes(x = Year, y = Total, ymin = lower, ymax = upper))
# df2 <- AKaerial::ACPHistoric$combined |>
#   filter(Species == "STEI") |> 
#   mutate(Total = itotal/2,
#          sd_Total = sqrt(itotal.var/4), 
#          upper = Total + 2*sd_Total, 
#          lower = if_else(Total - 2*sd_Total < 0, 0, Total - 2*sd_Total), 
#          Type = "Observer Mean") |>
#   select(Year, Total, upper, lower, Type)
# ggplot(data = df2) + 
#   geom_pointrange(aes(x = Year, y = Total, ymin = lower, ymax = upper))
# df <- rbind(df, df2)
# ggplot(data = df) + 
#   geom_pointrange(aes(x = Year, y = Total, ymin = lower, ymax = upper, col = Type), 
#                   position = position_dodge2(width = 0.3)) 
# table(AKaerial::ACPHistoric$output.table$Year, AKaerial::ACPHistoric$output.table$Observer)  
