#drop transects out of Triangle survey and look at variance of estimates
# maybe also try bootstrapping
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
#find actual sample size by year
sample.size <- group_by(birds3, Year) |> summarise(n = n())
#calculate estimate as observered
design.est2 <- birds3 |> 
  mutate(Area = drop_units(Area), Num = drop_units(Num)) |>
  group_by(Year) |>
  summarise(sum_Num = sum(Num), 
            n = n(),
            sArea = sum(Area), 
            Total = sum_Num*TriangleArea/sArea,
            sd_Num = sd(Num), 
            sd_Total = (TriangleArea/sArea) * 
              sqrt( (1 - sArea/TriangleArea) * 
                      (sArea*sum(Area*(Num/Area - sum_Num/sArea)^2)/(n - 1) )),
            sd_part = sqrt(sArea*sum(Area*(Num/Area - sum_Num/sArea)^2)/(n - 1)) ) |>
  mutate(upper = Total + 2*sd_Total, 
         lower = if_else(Total - 2*sd_Total < 0, 0, Total - 2*sd_Total))
#plot and add sample sizes
ylim.prim <- c(0, 400)   # in this example, precipitation
ylim.sec <- c(0, 69) 
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]
ggplot(data = design.est2) +  
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Total)) + 
  geom_point(aes(x = Year, y = Total)) +
  geom_line(data = sample.size, aes(x = Year, y = a + b*n), col = "red") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  scale_y_continuous(limits = c(0, 400), 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Sample Size")) + 
  ylab("Estimated Indicated Bird Index") +
  labs(title = "Design-based Estimated breeding bird index in Triangle (no detection)")
#plot estimate of SD with total observed birds
ggplot(data = design.est2) + 
  geom_point(aes(x = Total, y = sd_Total, col = n))
ggplot(data = design.est2) + 
  geom_point(aes(x = Total, y = sd_Total/Total, col = n))
ggplot(data = design.est2) + 
  geom_point(aes(x = sum_Num, y = sd_part/sum_Num, col = n))
################################################################################
#try dsims and the triangle GAM fot object to model sample size?
library(dsims)
fit <- readRDS(file = "results/fit.nofliers.RDS")
trangle <- st_transform(triangle, crs = 3338)
region <- make.region(region.name = "Triangle",
                      shape = triangle)
plot(region)
cover <- make.coverage(region,
                       n.grid.points = 1000)
design <- make.design(region = region, 
                      design = "systematic",
                      samplers = 32,
                      edge.protocol = "minus",
                      design.angle = 88,
                      truncation = 200,
                      coverage.grid = cover)
transects <- generate.transects(design)
transects
plot(region, transects)
density <- make.density(region = region,
                        x.space = 1000,
                        fitted.model = fit)
plot(density, region, scale = 0.001)
pop.desc <- make.population.description(region = region,
                                        density = density,
                                        N = 200,
                                        fixed.N = TRUE)
detect <- make.detectability(key.function = "hn",
                             scale.param = 1000,
                             truncation = 200)
plot(detect, pop.desc)
analyses <- make.ds.analysis(dfmodel = list(~1),
                             key = c("hn"),
                             truncation = 200,
                             er.var = "O2",
                             criteria = "AIC")
sim.parallel <- make.simulation(reps = 100,
                                design = design,
                                population.description = pop.desc,
                                detectability = detect,
                                ds.analysis = analyses)
eg.parallel.survey <- run.survey(sim.parallel)
plot(eg.parallel.survey, region)
sim.parallel <- run.simulation(sim.parallel)
summary(sim.parallel)
histogram.N.ests(sim.parallel)
#set up loop to simulate difference samplers intensities:
sample.size <- seq(62, 14, by = -4) 
pop.size <- c(200, 100, 50)
df <- data.frame(NULL)
for(j in 1:length(pop.size)){
for(i in 1:length(sample.size)){
  design <- make.design(region = region, 
                        design = "systematic",
                        samplers = sample.size[i],
                        edge.protocol = "minus",
                        design.angle = 88,
                        truncation = 200,
                        coverage.grid = cover) 
  pop.desc <- make.population.description(region = region,
                                          density = density,
                                          N = pop.size[j],
                                          fixed.N = TRUE)
  sim.parallel <- make.simulation(reps = 200,
                                  design = design,
                                  population.description = pop.desc,
                                  detectability = detect,
                                  ds.analysis = analyses)
  sim <- run.simulation(sim.parallel)
  df <- rbind(df, summary(sim)@individuals$N)
}}
df <- mutate(df, Samplers = rep(sample.size, times=length(pop.size)), 
             cv = mean.se/Truth)
df2 <- filter(df, mean.se < 1000) |> arrange(Truth, Samplers)
ggplot(data = df2, aes(x = Samplers, y = mean.se, color = Truth, group = Truth)) + 
  geom_line() + geom_point() 
ggplot(data = df2, aes(x = Samplers, y = sd.of.means, color = Truth, group = Truth)) + 
  geom_line() + geom_point()
ggplot(data = df2, aes(x = Samplers, y = percent.bias, color = Truth, group = Truth)) + 
  geom_line() + geom_point()
ggplot(data = df2, aes(x = Samplers, y = RMSE, color = Truth, group = Truth)) + 
  geom_line() + geom_point()
ggplot(data = df2, aes(x = Samplers, y = CI.coverage.prob, color = Truth, group = Truth)) + 
  geom_line() + geom_point()
ggplot(data = df2, aes(x = Samplers, y = cv, color = Truth, group = Truth)) + 
  geom_line() + geom_point() 
ggsave(file = "results/dsims_o2_cv.png")
################################################################################
