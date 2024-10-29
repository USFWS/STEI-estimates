#combine ABR and ACP data set, fit GAM model to combined data and predict
# Then, drop out ABR transects, refit model, and look at estimate precision
#do not have observer data for ABR triangle data, therefore use Observer = "ABR". 
# use only non-flying birds to be consistent with ACP
library(tidyverse)
library(readxl)
library(sf)
################################################################################
#First need segmentized ACP data
#code from head of fit_STEI.R code file in ACP-Mapping:
#  changed cell size to match ABR, 1000m
#bad practice but temporarily change working directory:
wd <- getwd()
setwd("../ACP-Mapping")
source("map_density_functions.R")
#select species
spp <- "STEI"
#read in lines and birds data
lines <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines-Obs-2024-02-15.gpkg")
birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-03-21.csv") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
acp <- st_read(dsn="Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
#loop through all years and fit models
#Do this for SPEI
df <- data.frame(NULL)
years <- unique(birds$Year)
for(i in years){
  trans <- filter(lines, Year == i) %>%
    st_transform(crs=st_crs(3338))
  seg <- make_segments(x = grid, y = trans)
  dat <- filter(birds, Year == i)
  dfy <- get_data(x = dat, y = seg, area = acp, Spp = spp, grid = grid)
  df <- rbind(df, dfy)
}
#change working direct back 
setwd(wd)
#write ACP segmentized data to file:
saveRDS(df, file = "data/segmentized_acp_data.RDS")
#clear workspace
rm(list = ls())
################################################################################
####################
## load ABR data, data object from code in wrangle_ABR.R
abr <- readRDS(file = "data/segmentized_triangle_data_nofliers.RDS")
####################
#load ACP data
acp <- readRDS(file = "data/segmentized_acp_data.RDS")
################################################################################
## set coordinates to same crs, ACP is in 3338, ABR is in 26904
# transform ABR to 3338
tmp <- abr |> st_as_sf(coords = c("X", "Y"), crs = 26904) |>
  st_transform(crs = 3338) |>
  st_coordinates()
abr <- select(abr, -X, -Y) |> cbind(tmp)
df <- rbind(abr, acp)
rm(tmp, abr, acp)
plot(st_geometry(st_as_sf(df, coords = c("X", "Y"), crs = 3338)))
plot(st_geometry(st_as_sf(filter(df, Observer == "999"), coords = c("X", "Y"), crs = 3338)))
tmp <- mutate(df, Survey = ifelse(Observer == "999", "ABR", "ACP"))
ggplot(data = filter(tmp, Survey == "ACP"), aes(x=X, y=Y)) + geom_point() + 
  geom_point(data = filter(tmp, Survey == "ABR"), aes(x=X, y=Y, col = "red"))
rm(tmp)
#looks good
#now fit model
#I fit a model with an Observer effect, because in the ACP data, this model was 
#  essential identical to the best model delta AIC < 1 and I wanted the ABR 
#  Survey (models as observer = "999") to be estimated. 
#  Count ~ s(X, Y, bs = "ds", k = 200, m = c(1, 0.5)) + s(Year, k = 20) + 
#    s(Observer, bs = "re")
library(mgcv)
df$Observer <- factor(df$Observer)
fit <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
      s(Year, k = 20), offset = logArea, family = nb, method = "REML", data = df)
saveRDS(fit, file = "results/fit.RDS")
################################################################################
## predict and map
fit <- readRDS(file = "results/fit.RDS")
#make map, code from ACP-Mapping/map_GAM.R
source("../ACP-Mapping/map_density_functions.R")
acp <- st_read(dsn="../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
newdat <- st_centroid(grid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Observer = "HMW", logArea = 0, Year = 2013) 
# %>%
#   cbind(grid_position)
preds <- predict(fit, newdata = newdat, type = "response", se.fit = TRUE,
                 exclude = "s(Observer)", newdata.guaranteed = TRUE)

plotdat <- cbind(preds, grid) |> st_transform(crs = 4326)
p <- ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
  #geom_sf(data = design[-trim,], fill = NA, alpha = 1, col = "white")  +
  scale_fill_viridis_c(name = "Expected \n density") +
  #breaks = my_breaks, labels = my_breaks) +
  labs(title = "STEI in 2013")
print(p)
ggsave("results/combined_map.png")
#CV
plotdat <- mutate(plotdat, CV = se.fit/fit)
p2 <- ggplot(data = plotdat) + geom_sf(aes(fill=CV), col = NA) +
    scale_fill_viridis_c(name = "CV") + labs(title = "STEI in 2013")
print(p2)
ggsave("results/combined_map_cv.png")
min(plotdat$CV)
# [1] 0.4419038
#zoom to triangle
plotdat <- st_transform(plotdat, crs = 4326)
ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
  scale_fill_viridis_c(name = "Expected \n density") +
  labs(title = "STEI in 2013") + 
  coord_sf(xlim = c(-157.5, -155.5), ylim = c(70.9, 71.4), expand = FALSE)
ggsave("results/combined_map_zoomed.png")
#########################################
# now simulation population total by year
#add Area to newdat
library(units)
newdat$Area <- drop_units(set_units(grid$Grid.Area, "km^2"))
df <- data.frame(NULL)
for(i in 1999:2023){
  df <- rbind(df, mutate(newdat, Year = i))
}
Nsamples <- 250
s <- rbeta(Nsamples, 
           shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
           shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1))
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit, type="lpmatrix", newdata=df, exclude = "s(Observer)",
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit), vcov(fit))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p)) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#summarize
sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                    Mean = apply(post, 2, mean), 
                    sd = apply(post, 2, sd), 
                    median = apply(post, 2, median), 
                    upper = apply(post, 2, quantile, probs = 0.975),
                    lower = apply(post, 2, quantile, probs = 0.025))
#plot
gg <- ggplot(data = sumdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Bird Index") +
  labs(title = "Estimated breeding bird index across ACP (no detection)")
print(gg)
ggsave("results/combined_year_nodetection.png")
#add detection
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p/s[j])) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#summarize
sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                    Mean = apply(post, 2, mean), 
                    sd = apply(post, 2, sd), 
                    median = apply(post, 2, median), 
                    upper = apply(post, 2, quantile, probs = 0.975),
                    lower = apply(post, 2, quantile, probs = 0.025))
#plot
gg <- ggplot(data = sumdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Bird Index") +
  labs(title = "Estimated breeding bird index across ACP (with detection)")
print(gg)
#find long term posterior mean and CI
m = round(mean(apply(post[,-c(1:5)], 1, mean)), 1)
q = round(quantile(apply(post[,-c(1:5)], 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
gg + geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  geom_text(aes(x = 2003, y = 10000, label = "20-year mean", hjust = 0)) + 
  geom_text(aes(x = 2003, y = 9000, hjust = 0,
                label = paste0(m, " (95% CI: ", q[1], ", ", q[4], ")")))
ggsave("results/combined_year_detection.png")
################################################################################
#as above but limit spatial extent to ACP "High" and "Tesh High" strata.
#  above posteriors are very highly skewed to large numbers of eiders
#  this cause concern with eider experts, so trying to understand cause
#  Is it a property of the NB model (yes) or due to the spatial predictions into 
#    the medium and low strata (maybe partly?)
#  Restrict predictions to the High and Tesh High strata to see effect on posterior
fit <- readRDS(file = "results/fit.RDS")
#make map, code from ACP-Mapping/map_GAM.R
source("../ACP-Mapping/map_density_functions.R")
acp <- st_read(dsn="../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = c(1,5)) %>% #select only "High" and "Tesh High"
  st_transform(crs=3338)
plot(st_geometry(acp)) #correct
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
newdat <- st_centroid(grid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Observer = "HMW", logArea = 0, Year = 2013) 
preds <- predict(fit, newdata = newdat, type = "response", se.fit = TRUE,
                 exclude = "s(Observer)", newdata.guaranteed = TRUE)

plotdat <- cbind(preds, grid) |> st_transform(crs = 4326)
p <- ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
  #geom_sf(data = design[-trim,], fill = NA, alpha = 1, col = "white")  +
  scale_fill_viridis_c(name = "Expected \n density") +
  #breaks = my_breaks, labels = my_breaks) +
  labs(title = "STEI in 2013")
print(p)
ggsave("results/combined_map_High.png")
#find posterior by year, here just with detection
library(units)
newdat$Area <- drop_units(set_units(grid$Grid.Area, "km^2"))
df <- data.frame(NULL)
for(i in 1999:2023){
  df <- rbind(df, mutate(newdat, Year = i))
}
Nsamples <- 250
s <- rbeta(Nsamples, 
           shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
           shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1))
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit, type="lpmatrix", newdata=df, exclude = "s(Observer)",
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit), vcov(fit))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p/s[j])) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
  print(j)
}
#summarize
sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                    Mean = apply(post, 2, mean), 
                    sd = apply(post, 2, sd), 
                    median = apply(post, 2, median), 
                    upper = apply(post, 2, quantile, probs = 0.975),
                    lower = apply(post, 2, quantile, probs = 0.025))
#plot
gg <- ggplot(data = sumdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Bird Index") +
  labs(title = "Estimated breeding bird index High and Tesh High Strata (with detection)")
print(gg)
#find long term posterior mean and CI
m = round(mean(apply(post[,-c(1:5)], 1, mean)), 1)
q = round(quantile(apply(post[,-c(1:5)], 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
gg + geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  geom_text(aes(x = 2003, y = 3000, label = "20-year mean", hjust = 0)) + 
  geom_text(aes(x = 2003, y = 2800, hjust = 0,
                label = paste0(m, " (95% CI: ", q[1], ", ", q[4], ")")))
ggsave("results/combined_year_detection_High.png")
