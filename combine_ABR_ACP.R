#combine ABR and ACP data set, fit GAM model to combined data and predict
#do not have observer data for ABR triangle data, therefore use Observer = "ABR". 
# update Dec. 2024: learned that flying birds should be included to be 
#  consistent with ACP
#update below to include flying birds as these are recorded with same protocol 
#  as on ACP
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
lines <- st_read(dsn = "Data/ACP_2023/analysis_output/Lines-Obs-2024-11-12.gpkg")
birds <- read_csv(file = "Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-11-12.csv") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |>
  #Need to replace the "open" STEI observation with "single", do it here for now :(
  mutate(Obs_Type = replace(Obs_Type, Species == "STEI" & Obs_Type == "open", "single"))
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
#fit acp model just to have a model object of know orgin here in this project
#we will fit a Duchon slipe and a tprs model to compare issue with posterior 
# similation and eider density far from coast
# library(mgcv)
# library(gratia)
# df$Observer <- factor(df$Observer)
# fit.ds <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Observer, bs = "re") + 
#              s(Year, k = 14), offset = logArea, family = nb, method = "REML", 
#              data = df)
# draw(fit.ds, select = 1, dist = 0.02, rug = FALSE)
# test <- fit.ds
# test$coefficients <- rmvn(1, coef(fit.ds), vcov(fit.ds))
# draw(test, select = 1, dist = 0.02, rug = FALSE)
# #fit tp
# fit.tp <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re") + 
#                 s(Year, k = 14), offset = logArea, family = nb, method = "REML", 
#               data = df)
# draw(fit.tp, select = 1, dist = 0.02, rug = FALSE)
# test <- fit.tp
# test$coefficients <- rmvn(1, coef(fit.tp), vcov(fit.tp))
# draw(test, select = 1, dist = 0.02, rug = FALSE)
# #clear workspace
# rm(list = ls())
# ################################################################################
####################
rm(list=ls())
## load ABR data, data object from code in wrangle_ABR.R
abr <- readRDS(file = "data/segmentized_triangle_data_fliers.RDS")
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
#I fit a model with an survey effect so that any difference between surveys can 
#  be estimated. An Observer effect could also be used, but as of December 2024,
#  I did not have observer data for the ABR data. 
library(mgcv)
df$Observer <- factor(df$Observer)
df$fYear <- factor(df$Year)
df$survey <- factor(ifelse(df$Observer == "999", "ABR", "ACP"))
fit1 <- gam(Count~survey + s(X, Y, bs="tp", k = 200) + s(Year, k = 20),
            offset = logArea, family = nb, method = "REML", data = df)
saveRDS(fit1, file = "results/fit1.comb.RDS")
fit2 <- gam(Count~survey + s(X, Y, bs="tp", k = 200) + s(Year, k = 20) + 
              ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("tp", "cr")),
            offset = logArea, family = nb, method = "REML", data = df)
saveRDS(fit2, file = "results/fit2.comb.RDS")
fit1 <- readRDS(file = "results/fit1.comb.RDS")
fit2 <- readRDS(file = "results/fit2.comb.RDS")
##plot models
library(gratia)
library(DHARMa)
draw(fit2, select = 1, rug=FALSE)
draw(fit2, select = 2, rug=FALSE)
draw(fit2, select = 3, rug=FALSE)
draw(fit1, select = 1, rug=FALSE)
draw(fit1, select = 2, rug=FALSE)
summary(fit1)
summary(fit2)
AIC(fit1, fit2)
################################################################################
## predict and map
library(mgcv)
fit <- readRDS(file = "results/fit1.comb.RDS")
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
  mutate(Observer = "HMW", survey = "ABR", logArea = 0, Year = 2013) 
# %>%
#   cbind(grid_position)
preds <- predict(fit, newdata = newdat, type = "response", se.fit = TRUE, 
                 newdata.guaranteed = TRUE)

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
#[1] 0.2408627
#zoom to triangle
plotdat <- st_transform(plotdat, crs = 4326)
ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
  scale_fill_viridis_c(name = "Expected \n density (km^-2)") +
  labs(title = "STEI in 2013") + 
  coord_sf(xlim = c(-157.5, -155.5), ylim = c(70.9, 71.4), expand = FALSE)
ggsave("results/combined_map_zoomed.png")
####plot on link scale
draw(fit, select = 1, rug = FALSE) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())
ggsave("results/combined_partial_1.png")
draw(fit, select = 2, rug = FALSE) 
ggsave("results/combined_partial_2.png")
#########################################
# now simulated population total by year
#add Area to newdat
library(units)
newdat$Area <- drop_units(set_units(grid$Grid.Area, "km^2"))
df <- data.frame(NULL)
for(i in 1999:2024){
  df <- rbind(df, mutate(newdat, Year = i))
}
Nsamples <- 500
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit, type="lpmatrix", newdata=df,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
# b <- rmvn(Nsamples, coef(fit), vcov(fit))
# for(j in 1:Nsamples ){
#   p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
#   post[j,] <- cbind(df, p) %>%
#     group_by(Year) %>%
#     summarize( Total = sum(2*p)) %>% #INDICTED BIRDS!!!
#     ungroup() %>%
#     select(Total) %>%
#     unlist()
# }
# #summarize
# #getting some absurd posterior samples, filter?
# post <- post[apply(post, 1, max) < 10000,]
# sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
#                     Mean = apply(post, 2, mean), 
#                     sd = apply(post, 2, sd), 
#                     median = apply(post, 2, median), 
#                     upper = apply(post, 2, quantile, probs = 0.975),
#                     lower = apply(post, 2, quantile, probs = 0.025))
# #plot
# gg <- ggplot(data = sumdf) + 
#   geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
#   geom_line(aes(x = Year, y = Mean)) + 
#   geom_line(aes(x = Year, y = median), linetype = "dashed") + 
#   scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
#   ylab("Indicated Breeding Bird Index")
# print(gg)
#filter above seems to work OK for fit1 but not fit2
#wacky, for fit2, direct MN sampling does not work well, large "blowup" of 
# estimates in first few year. Is this due to posterior sampling or the model?
#ggsave("results/combined_year_noD_filtered.png")
##try MH sampling, need to turn acceptance rate, 0.02 seems to work for fit1
b <- gam.mh(fit, ns=3000, thin = 2, rw.scale = 0.02)
b <- b$bs
plot(b[,1])
plot(b[,2])
plot(b[,200])
hist(b[,1])
b <- b[sample(750:dim(b)[1], Nsamples),] #just to scramble over any autocorreclation, 
#  using 750 as start to avoid a long autocorrelated chains before that section
post <- matrix(0, Nsamples, length(unique(df$Year)))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p)) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#try filtering post for fit2 after the MH sampling!:
# post <- post[apply(post, 1, max) < 10000,]
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
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Indicated Breeding Bird Index")
print(gg)
ggsave("results/combined_year_noD.png")
#that seems to work for fit1
#wow, MH didn't work at all for fit2, must be the s-t interaction in the model
#even filter the posterior after the MH step didn't work. Abandon fit2
################################################################################
#add detection
detdf <- data.frame(Bin = 1:4, p = c(0.514, 0.457, 0.143, 0.114), 
                    lower = c(0.338, 0.217, 0.048, 0.026), 
                    upper=c(0.689, 0.717, 0.306, 0.310))
mP <- mean(detdf$p)
sSE <- sqrt(mean( ((detdf$upper - detdf$lower)/(2*1.96))^2 ))
#method of moments for Beta distribution
shape1 = mP*( mP*(1-mP)/sSE^2 - 1)
shape2 = (1 - mP)*( mP*(1-mP)/sSE^2 - 1)
s <- rbeta(Nsamples, 
           shape1 = shape1, 
           shape2 = shape2)
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
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Indicated Breeding Birds")
print(gg)
ggsave("results/combined_year_withD.png")
saveRDS(post, file = "results/Comb_post.RDS")
#find long term posterior mean and CI
# q = round(quantile(apply(post[,-c(1:5)], 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
################################################################################
#  Calculate population outside Triangle
#read model fit object
library(tidyverse)
library(mgcv)
library(sf)
fit <- readRDS(file = "results/fit1.comb.RDS")
#ACP study area
acp <- st_read(dsn="../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
source("https://raw.githubusercontent.com/USFWS/ACP-Mapping/main/map_density_functions.R")
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
# read in Triangle survey area:
triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA") |>
  st_transform(crs = 3338)
outside <- st_difference(acp, triangle)
ggplot(data = outside) + geom_sf()
#clean up the little areas
outside <- st_cast(outside, "POLYGON") |>
  mutate(ID = factor(row_number())) |>
  select(ID)
outside <- mutate(outside, Area = units::set_units(st_area(outside), "km^2")) |>
  filter(Area > units::set_units(10, "km^2"))
ggplot(data = outside) + geom_sf(aes(fill=ID)) #100 km^2 is right
outside <- filter(outside, Area > units::set_units(100, "km^2"))
ggplot(data = outside) + geom_sf(aes(fill=ID))
#make grid
outside <- select_area(area = outside, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(outside, st_make_grid(x=outside, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
newdat <- st_centroid(grid) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(Observer = "HMW", survey = "ABR", logArea = 0, Year = 2013) 
library(units)
newdat$Area <- drop_units(set_units(grid$Grid.Area, "km^2"))
df <- data.frame(NULL)
for(i in 1999:2024){
  df <- rbind(df, mutate(newdat, Year = i))
}
Nsamples <- 500
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit, type="lpmatrix", newdata=df,
              newdata.guaranteed=TRUE)
b <- gam.mh(fit, ns=3000, thin = 2, rw.scale = 0.02)
b <- b$bs
plot(b[,1])
plot(b[,2])
plot(b[,200])
hist(b[,1])
b <- b[sample(750:dim(b)[1], Nsamples),] #just to scramble over any autocorreclation, 
#  using 750 as start to avoid a long autocorrelated chains before that section
post <- matrix(0, Nsamples, length(unique(df$Year)))
#add detection
detdf <- data.frame(Bin = 1:4, p = c(0.514, 0.457, 0.143, 0.114), 
                    lower = c(0.338, 0.217, 0.048, 0.026), 
                    upper=c(0.689, 0.717, 0.306, 0.310))
mP <- mean(detdf$p)
sSE <- sqrt(mean( ((detdf$upper - detdf$lower)/(2*1.96))^2 ))
#method of moments for Beta distribution
shape1 = mP*( mP*(1-mP)/sSE^2 - 1)
shape2 = (1 - mP)*( mP*(1-mP)/sSE^2 - 1)
s <- rbeta(Nsamples, 
           shape1 = shape1, 
           shape2 = shape2)
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
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Indicated Breeding Birds")
print(gg)
ggsave("results/combined_year_withD_NoTri.png")
saveRDS(post, file = "results/Comb_NoTri_post.RDS")
# ################################################################################
# #as above but limit spatial extent to ACP "High" and "Tesh High" strata.
# #  above posteriors are very highly skewed to large numbers of eiders
# #  this cause concern with eider experts, so trying to understand cause
# #  Is it a property of the NB model (yes) or due to the spatial predictions into 
# #    the medium and low strata (maybe partly?)
# #  Restrict predictions to the High and Tesh High strata to see effect on posterior
# fit <- readRDS(file = "results/fit.RDS")
# #make map, code from ACP-Mapping/map_GAM.R
# source("../ACP-Mapping/map_density_functions.R")
# acp <- st_read(dsn="../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
# #make grid
# acp <- select_area(area = acp, select = c(1,5)) %>% #select only "High" and "Tesh High"
#   st_transform(crs=3338)
# plot(st_geometry(acp)) #correct
# grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
#   mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
# newdat <- st_centroid(grid) %>%
#   st_coordinates() %>%
#   as.data.frame() %>%
#   mutate(Observer = "HMW", logArea = 0, Year = 2013) 
# preds <- predict(fit, newdata = newdat, type = "response", se.fit = TRUE,
#                  exclude = "s(Observer)", newdata.guaranteed = TRUE)
# 
# plotdat <- cbind(preds, grid) |> st_transform(crs = 4326)
# p <- ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
#   #geom_sf(data = design[-trim,], fill = NA, alpha = 1, col = "white")  +
#   scale_fill_viridis_c(name = "Expected \n density") +
#   #breaks = my_breaks, labels = my_breaks) +
#   labs(title = "STEI in 2013")
# print(p)
# ggsave("results/combined_map_High.png")
# #find posterior by year, here just with detection
# library(units)
# newdat$Area <- drop_units(set_units(grid$Grid.Area, "km^2"))
# df <- data.frame(NULL)
# for(i in 1999:2023){
#   df <- rbind(df, mutate(newdat, Year = i))
# }
# Nsamples <- 250
# s <- rbeta(Nsamples, 
#            shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
#            shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1))
# post <- matrix(0, Nsamples, length(unique(df$Year)))
# 
# Xp <- predict(fit, type="lpmatrix", newdata=df, exclude = "s(Observer)",
#               newdata.guaranteed=TRUE)
# #sample from parameter posterior
# b <- rmvn(Nsamples, coef(fit), vcov(fit))
# for(j in 1:Nsamples ){
#   p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
#   post[j,] <- cbind(df, p) %>%
#     group_by(Year) %>%
#     summarize( Total = sum(2*p/s[j])) %>% #INDICTED BIRDS!!!
#     ungroup() %>%
#     select(Total) %>%
#     unlist()
#   print(j)
# }
# #summarize
# sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
#                     Mean = apply(post, 2, mean), 
#                     sd = apply(post, 2, sd), 
#                     median = apply(post, 2, median), 
#                     upper = apply(post, 2, quantile, probs = 0.975),
#                     lower = apply(post, 2, quantile, probs = 0.025))
# #plot
# gg <- ggplot(data = sumdf) + 
#   geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
#   geom_line(aes(x = Year, y = Mean)) + 
#   scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
#   ylab("Estimated Indicated Bird Index") +
#   labs(title = "Estimated breeding bird index High and Tesh High Strata (with detection)")
# print(gg)
# #find long term posterior mean and CI
# m = round(mean(apply(post[,-c(1:5)], 1, mean)), 1)
# q = round(quantile(apply(post[,-c(1:5)], 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
# gg + geom_line(aes(x = Year, y = median), linetype = "dashed") + 
#   geom_text(aes(x = 2003, y = 3000, label = "20-year mean", hjust = 0)) + 
#   geom_text(aes(x = 2003, y = 2800, hjust = 0,
#                 label = paste0(m, " (95% CI: ", q[1], ", ", q[4], ")")))
# ggsave("results/combined_year_detection_High.png")
