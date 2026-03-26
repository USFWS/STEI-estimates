# R code to produce figure in final manuscript version for PLOS
##load packages
library(tidyverse)
library(sf)
library(spData)
library(tmap)
library(grid)
library(units)
library(readxl)
library(cowplot)
library(patchwork)
library(gratia)
library(mgcv)
#Figure 1
path = "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg"
acp <- st_read(dsn = path, quiet = TRUE) #|> st_union()
acp$Stratum[5] <- "Teshekpuk"
path = "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignTrans_QC_2024-11-12.gpkg"
trans.acp <- st_read(dsn = path, quiet = TRUE) |> 
  filter(Year %in% 2007)
trans <- st_read(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx", 
                 layer = "routes", quiet = TRUE) |>
  select(Transect = name) |>
  filter(str_sub(Transect, 1, 1) == "A", row_number() > 10)
triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA", quiet = TRUE) |>
  rename(Stratum = Descriptio) |>
  select(Stratum) |>
  mutate(Stratum = "Triangle") |>
  st_transform(crs = st_crs(acp))
st_geometry(triangle) <- "geom"
acp <- rbind(acp, triangle) |> 
  filter(Stratum != "Not Sampled")
survey.area = tm_shape(acp) +
  tm_fill(fill = "Stratum") +
  tm_borders() + 
  tm_shape(trans.acp) + tm_lines() + 
  tm_layout(legend.outside.position = "bottom",
            legend.orientation = "landscape", 
            frame = FALSE) + 
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0, 100), text.size = 1, 
              position = c("right", "top"))
ak = tm_shape(spData::alaska) + tm_polygons(fill = "white") + 
  tm_borders() + 
  tm_shape(acp) + tm_fill(fill = "Stratum", fill.legend = tm_legend_hide()) + 
  tm_layout(frame = FALSE) + tm_title("Alaska", 
                                      position = tm_pos_in(0.5, 0.1))
#below from from https://r.geocompx.org/adv-map
tiff("Figures/Fig1.tif", res = 600, width = 2250, height = 900, compression = "lzw")
survey.area
print(ak, vp = grid::viewport(0.85, 0.3, width = 0.3, height = 0.3))
dev.off()
###############################################################################
## Figure 2
#need to plot bird locations
path <- "../ACP-Mapping/Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-11-12.csv"
acp.obs <- read_csv(file = path) %>%
  filter(Species == "STEI") |> 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |>
  mutate(Survey = "ACP") |>
  select(Year, Survey, Obs_Type, Num)
abr.obs <- readxl::read_xlsx(path = "data/STEI_obs_1999-2023.xlsx", 
                             sheet = "STEI Obs 1999-2023") |> 
  rename(Transect = "Standard Transect") |>
  arrange(Year, Transect) |>
  st_as_sf(coords = c("LongDD83", "LatDD83"), crs = 4269) |>
  st_transform(crs = 4326) |>
  mutate(Survey = "Triangle") |>
  select(Survey)
df <- rbind(abr.obs, select(acp.obs, Survey))
obs.map = tm_shape(acp) +
  tm_fill() +
  tm_borders() + 
  tm_shape(df) + 
  tm_dots(fill = "Survey", size = 0.25,
          fill.scale = tm_scale_categorical(
            values = c(ACP="darkblue", Triangle="orange"))) + 
  tm_layout(legend.outside.position = "bottom",
            legend.orientation = "landscape", 
            frame = FALSE) + 
  tm_compass(type = "arrow", position = c("left", "top")) +
  tm_scalebar(breaks = c(0, 100), text.size = 1, position = c("right", "top"))
ak = tm_shape(spData::alaska) + tm_polygons(fill = "white") + 
  tm_borders() + 
  tm_shape(acp) + tm_fill(fill = "darkgrey") + 
  tm_layout(frame = FALSE) + tm_title("Alaska", 
                                      position = tm_pos_in(0.5, 0.1))
tiff("Figures/Fig2.tif", res = 600, width = 2250, height = 900, compression = "lzw")
print(obs.map)
print(ak, vp = grid::viewport(0.85, 0.3, width = 0.3, height = 0.3))
dev.off()
#########################################################################
## Figure 3
acp.obs <- read_csv(file = path) %>%
  filter(Species == "STEI") |>
  select(Year) |>
  group_by(Year) |>
  summarize(ACP = n())
abr.obs <- readxl::read_xlsx(path = "data/STEI_obs_1999-2023.xlsx", 
                             sheet = "STEI Obs 1999-2023") |>
  filter(On_Transect == "Y") |>
  select(Year, Flying, Males, Pairs) |>
  group_by(Year) |>
  summarise("Triangle" = sum(Males)+sum(Pairs))
df <- abr.obs |>
  full_join(acp.obs) |> 
  mutate_at(3, ~replace(., Year > 2006 & is.na(.) & Year != 2021, 0)) |>
  rbind(c(2020, NA, NA)) |>
  mutate_at(2:3, ~replace(., Year == 2009 & is.na(.), 0)) |> #no obs eiders on ABR for 2009
  arrange(Year)
df2 <- pivot_longer(df, 2:3, names_to="Survey", values_to = "Number")
fig3 <- ggplot(data = df2, aes(x=Year, y = Number, group = Survey, 
                               col = Survey)) + 
  #geom_line() + 
  geom_segment(aes(xend = Year, y = 0, yend = Number), 
               position = position_nudge(x = c(0, 0.1))) +
  geom_point(size = 2, position = position_nudge(x = c(0, 0.1))) + 
  scale_x_continuous(breaks = seq(1999, 2024, by = 2), 
                     expand = expansion(add = c(1, 1))) + 
  scale_y_continuous(breaks = seq(0, 60, by = 5)) +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
tiff("Figures/Fig3.tif", res = 600, width = 2250, height = 900, compression = "lzw")
print(fig3)
dev.off()
#######################################################################
## Figure 4
#read in data
triangle <- st_read(dsn = "data/Barrow_Triangle_STEI_Aerial_SA", quiet = TRUE)
TriangleArea <- st_area(triangle) |> set_units("km^2") |> drop_units()
trans <- st_read(dsn = "data/Barrow_STEI_standardized_transects_Aug2024.gpx", 
                 layer = "routes", quiet = TRUE) |>
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
         Flying, Lon, Lat) #Did not remove flying birds!

birds3 <- birds2 |>  right_join(effort) |> #right join to add zeros
  mutate(Area = Length*set_units(0.4, "km"), 
         Num = replace(Num, is.na(Num), 0)) |> #replace Nas with 0 observations
  group_by(Year, Transect) |>
  #INDICATED BIRDS!!!
  summarise(Num = 2*set_units(sum(Num), "1"), Area = mean(Area)) |>
  select(Year, Transect, Area, Num) 
abr.est <- birds3 |> 
  mutate(Area = drop_units(Area), Num = drop_units(Num)) |>
  group_by(Year) |>
  summarise(sum_Num = sum(Num), 
            n = n(),
            sArea = sum(Area), 
            Total = sum_Num*TriangleArea/sArea,
            sd_Num = sd(Num), 
            sd_Total = (TriangleArea/sArea) * 
              sqrt( (sArea*sum(Area*(Num/Area - sum_Num/sArea)^2)/(n - 1) )) ) |>
  mutate(upper = Total + 1.96*sd_Total, 
         lower = if_else(Total - 1.96*sd_Total < 0, 0, Total - 1.96*sd_Total)) |>
  select(Year, n, Total, sd_Total, upper, lower)

#ACP
path = "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg"
acp <- st_read(dsn=path, quiet = TRUE)
acp <- mutate(acp, StratumArea = units::set_units(st_area(acp), "km^2"))

path = "../ACP-Mapping/Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-11-12.csv"
birds <- read_csv(file = path) |>
  filter(Species == "STEI") |>
  mutate(Obs_Type = replace(Obs_Type, Obs_Type == "open", "single"))
#table(birds$Year, birds$Obs_Type)
path = "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignTrans_QC_2024-11-12.gpkg"
trans <- st_read(dsn = path, quiet = TRUE)
trans <- mutate(trans, Length = units::set_units(st_length(trans), "km")) |>
  mutate(trans, Year = as.numeric(Year))
birds <- right_join(st_drop_geometry(birds), st_drop_geometry(trans)) |>
  mutate(Area = Length*set_units(0.4, "km"), 
         Num = replace(Num, is.na(Num), 0)) |> #replace Nas with 0 observations
  left_join(st_drop_geometry(acp)) |>
  filter(Stratum != "Not Sampled") |> 
  group_by(Year, Stratum, Transect) |>
  #INDICATED BIRDS!!!
  summarise(Num = 2*sum(Num), Area = units::drop_units(mean(Area)), 
            StratumArea = units::drop_units(mean(StratumArea))) |>
  select(Year, Stratum, Transect, Area, StratumArea, Num) |>
  ungroup()
birds2 <- birds |> 
  group_by(Year, Stratum) |>
  summarise(sum_Num = sum(Num), 
            n = n(),
            sArea = sum(Area), 
            StratumArea = mean(StratumArea), 
            Total = sum_Num*StratumArea/sArea,
            sd_Num = sd(Num), 
            sd_Total = (StratumArea/sArea) * 
              sqrt( (sArea*sum(Area*(Num/Area - sum_Num/sArea)^2)/(n - 1) )) ) |>
  ungroup() |>
  group_by(Year) |>
  summarise(Total = sum(Total), sd_Total = sqrt(sum(sd_Total^2)), 
            upper = Total + 1.96*sd_Total, 
            lower = if_else(Total - 1.96*sd_Total < 0, 0, Total - 1.96*sd_Total))
#make plots
p1 <- ggplot(data = abr.est, aes(group=Year<2020)) +  
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Total)) + 
  geom_point(aes(x = Year, y = Total)) +
  scale_x_continuous(breaks = seq(1999, 2024, by = 2), 
                     expand = expansion(add = c(1, 2))) + 
  scale_y_continuous(limits = c(0, 450)) + 
  ylab("Estimate") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
p2 <- ggplot(data = birds2, aes(x = Year, y=Total, group=Year<2020)) +  
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "orange", alpha = 0.5) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(1999, 2024, by = 2), 
                     expand = expansion(add = c(9, 1))) +  
  scale_y_continuous(limits = c(0, 2000)) + 
  ylab("Estimate") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

tiff("Figures/Fig4.tif", res = 600, width = 2250, height = 900, compression = "lzw")
plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", labels = "AUTO", 
          hjust = -6.5, vjust = 2, label_size = 15)
dev.off()
#######################################################################
## Figure 5
# library(gratia)
# library(mgcv)
#load model results
load("results/abr.RData")
p1 <- draw(fit2, select = 1, dist = 0.02, rug = FALSE, caption = NULL) + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 
tiff("Figures/Fig5A.tif", res = 600, width = 2250, height = 2250, compression = "lzw")
p1
dev.off()
p2 <- draw(fit2, select = 2, dist = 0.02, rug = FALSE, caption = NULL)
tiff("Figures/Fig5B.tif", res = 600, width = 2250, height = 2250, compression = "lzw")
p2
dev.off()

ds3d <- data_slice(fit2, X = evenly(X), Y = evenly(Y), 
                   Year = seq(1999, 2023, length=4))
p3 <- draw(fit2, select = 3, data = ds3d, dist = 0.02, rug = FALSE, 
           caption = NULL) + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())
tiff("Figures/Fig5C.tif", res = 600, width = 2250, height = 2250, compression = "lzw")
p3
dev.off()
#try to put them together
#use patchwork
#library(patchwork)
tiff("Figures/Fig5.tif", res = 300, width = 2250, height = 2250, compression = "lzw")
(p1 + p2 + plot_layout(nrow = 1, ncol = 2, heights = 2)) /
  p3 + plot_layout(heights = 4) + plot_annotation(tag_levels = "A") +
  theme(text = element_text(face = "bold"))
dev.off()
#########################################################################
## Figure 6
#predict over grid for partial effect:
grid.cen <- st_centroid(grid) |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(Year = 1999, Area = drop_units(set_units(st_area(grid), "km^2")))
df <- grid.cen
preds <- predict(fit2, newdata = df, type = "response", 
                 exclude = c("s(Year)", "s(X,Y,Year)"))
df <- cbind(grid.cen, grid, preds) |>
  mutate(Eiders = Area*preds) |>
  st_as_sf(sf_column_name = "geometry", crs = st_crs(grid))
p1 <- ggplot(data = df) + geom_sf(aes(fill = Eiders), col = NA) + 
  scale_fill_viridis_c(name = expression(paste("Eiders (", km^{-2}, ")"))) + 
  theme_minimal() +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 
tiff("Figures/Fig6.tif", res = 600, width = 2250, height = 2250, compression = "lzw")
  p1
dev.off()
###########################################################################
## Figure 7
#define function to make data frame from posterior
post_df <- function(post = NULL, fit = NULL){
  df <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                   Mean = apply(post, 2, mean), 
                   sd = apply(post, 2, sd), 
                   median = apply(post, 2, median), 
                   upper = apply(post, 2, quantile, probs = 0.975),
                   lower = apply(post, 2, quantile, probs = 0.025))
  return(df)
}
#now include year
df <- data.frame(NULL)
for(i in 1999:2023){
  df <- rbind(df, mutate(grid.cen, Year = i))
}
Nsamples <- 500
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit2, type="lpmatrix", newdata=df, exclude = NULL,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit2), vcov(fit2))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p) ) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#summarize
sumdf <- post_df(post = post, fit = fit2)

#add sightability, find posterior
#from Bradely, Table 3
detdf <- data.frame(Bin = 1:4, p = c(0.514, 0.457, 0.143, 0.114), 
                    lower = c(0.338, 0.217, 0.048, 0.026), 
                    upper=c(0.689, 0.717, 0.306, 0.310))
mP <- mean(detdf$p)
sSE <- sqrt(mean( ((detdf$upper - detdf$lower)/(2*1.96))^2 ))
#method of moments for Beta distribution
shape1 = mP*( mP*(1-mP)/sSE^2 - 1)
shape2 = (1 - mP)*( mP*(1-mP)/sSE^2 - 1)
Nsamples <- 500
s <- rbeta(Nsamples, 
           shape1 = shape1, 
           shape2 = shape2)
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit2, type="lpmatrix", newdata=df, exclude = NULL,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit2), vcov(fit2))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p/s[j]) ) %>% #INDICATED BIRDS with sightability!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#saveRDS(post, file = "results/ABR_post.RDS")
postdf <- post_df(post = post, fit = fit2)
#plot
p1 <- ggplot(data = sumdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", 
              alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimate") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
p2 <- ggplot(data = postdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", 
              alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimate") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())

tiff("Figures/Fig7.tif", res = 600, width = 2250, height = 900, compression = "lzw")
plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", labels = "AUTO", 
          hjust = -6.5, vjust = 2, label_size = 15)
dev.off()
############################################################################
# Figure 8
#read in posterior
post <- readRDS(file = "results/ABR_post.RDS")
#find long term posterior mean and CI
m = round(mean(apply(post[,-c(1:5)], 1, mean)), 1)
q = round(quantile(apply(post[,-c(1:5)], 1, mean), 
                   probs=c(0.025, 0.05, 0.5, 0.975)), 1)
#find year-specific summaries for csv output
df <- data.frame(Year = 1999:2023,
                 Mean = apply(post, 2, mean), 
                 sd = apply(post, 2, sd), 
                 median = apply(post, 2, median), 
                 upper = apply(post, 2, quantile, probs = 0.975),
                 lower = apply(post, 2, quantile, probs = 0.025))
#write_csv(df, file = "results/model_est_ABR.csv")

df <- data.frame(NULL)
tyears <- dim(post)[2]
for( t in 1:(tyears-1)){
  trend <- (log(post[,tyears]) - log(post[,tyears-t]))/t
  df <- rbind(df, data.frame(t = t, mtrend = mean(trend), 
                             med = quantile(trend, probs = 0.5), 
                             upper = quantile(trend, probs = 0.975), 
                             lower = quantile(trend, probs = 0.025)))
}

p1 <- ggplot(data = df, aes(x = t, y=mtrend)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line() + 
  geom_line(aes(y = med), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) + 
  scale_y_continuous(breaks = seq(-1, 0.6, by = 0.2)) + 
  ylab("log Trend") + 
  xlab("Lag") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
tiff("Figures/Fig8.tif", res = 600, width = 2250, height = 900, compression = "lzw")
p1
dev.off()
############################################################################
## Figure 9
load("results/ACP.RData")
p1 <- draw(fit0.re, select = 1, dist = 0.02, rug = FALSE, caption = NULL) + 
        theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) 
tiff("Figures/Fig9.tif", res = 600, width = 2250, height = 900, compression = "lzw")
p1
dev.off()
############################################################################
## Figure 10
fit <- fit0.re
df <- data.frame(NULL)
for(i in c(2007:2019, 2022:2024)){
  df <- rbind(df, mutate(grid.cen, fYear = i))
}
Nsamples <- 500
post <- matrix(0, Nsamples, length(unique(df$fYear)))

Xp <- predict(fit, type="lpmatrix", newdata=df, exclude = NULL,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
# try MH sampling of the posterior:
#(tried various rw.scale to get acceptance rate reasonable, 0.25, 0.1, 0.05)
b <- gam.mh(fit, ns=Nsamples*10+1000, thin = 10, rw.scale = 0.05)
b <- b$bs
post.mh <- matrix(0, Nsamples, length(unique(df$fYear)))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post.mh[j,] <- cbind(df, p) %>%
    group_by(fYear) %>%
    summarize( Total = sum(2*p) ) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
sumdf.mh <- data.frame(Year = unique(df$fYear),
                       Mean = apply(post.mh, 2, mean), 
                       sd = apply(post.mh, 2, sd), 
                       median = apply(post.mh, 2, median), 
                       upper = apply(post.mh, 2, quantile, probs = 0.975),
                       lower = apply(post.mh, 2, quantile, probs = 0.025))
#plot
p1 <- ggplot(data = sumdf.mh, aes(group=Year<2020)) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimate") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
p1
## Now do detection: done in Predict_ACP.R, posterior sourced from saved R object
post.mh <- readRDS(post.mh, file = "results/ACP_post.RDS")
sumdf.mh2 <- data.frame(Year = unique(df$fYear),
                       Mean = apply(post.mh, 2, mean), 
                       sd = apply(post.mh, 2, sd), 
                       median = apply(post.mh, 2, median), 
                       upper = apply(post.mh, 2, quantile, probs = 0.975),
                       lower = apply(post.mh, 2, quantile, probs = 0.025))
#plot
p2 <- ggplot(data = sumdf.mh2, aes(group=Year<2020)) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimate") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
p2
tiff("Figures/Fig10.tif", res = 600, width = 2250, height = 900, compression = "lzw")
plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", labels = "AUTO", 
          hjust = -6.5, vjust = 2, label_size = 15)
dev.off()
###########################################################################
## Figure 11
#augment post with NAs for years 2020 and 2021
post <- post.mh
tmp <- matrix(NA, nrow = dim(post)[1], ncol = dim(post)[2]+2)
tmp[,1:13] <- post[,1:13]
tmp[,16:18] <- post[,14:16]
post <- tmp
rm(tmp)
#find long term posterior mean and CI
m = round(mean(apply(post[,], 1, mean, na.rm = TRUE)), 1)
q = round(quantile(apply(post[,], 1, mean, na.rm = TRUE), 
                   probs=c(0.025, 0.05, 0.5, 0.975)), 1)
#find year-specific summaries for csv output
df <- data.frame(Year = 2007:2024,
                 Mean = apply(post, 2, mean, na.rm = TRUE), 
                 sd = apply(post, 2, sd, na.rm = TRUE), 
                 median = apply(post, 2, median, na.rm = TRUE), 
                 upper = apply(post, 2, quantile, probs = 0.975, na.rm = TRUE),
                 lower = apply(post, 2, quantile, probs = 0.025, na.rm = TRUE))
#write_csv(df, file = "results/model_est_ACP.csv")

df <- data.frame(NULL)
tyears <- dim(post)[2]
for( t in 1:(tyears-1)){
  trend <- (log(post[,tyears]) - log(post[,tyears-t]))/t
  df <- rbind(df, data.frame(t = t, mtrend = mean(trend, na.rm = TRUE), 
                             med = quantile(trend, probs = 0.5, na.rm = TRUE), 
                             upper = quantile(trend, probs = 0.975, na.rm = TRUE), 
                             lower = quantile(trend, probs = 0.025, na.rm = TRUE)))
}

p1 <- ggplot(data = df, aes(x = t, y=mtrend)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line() + 
  geom_line(aes(y = med), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) + 
  scale_y_continuous(breaks = seq(-0.5, 2.5, by = 0.25)) + 
  ylab("log Trend") + 
  xlab("Lag") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
tiff("Figures/Fig11.tif", res = 600, width = 2250, height = 900, compression = "lzw")
p1
dev.off()
###############################################################################
## Figure 12
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
#zoom to triangle
p1 <- ggplot(data = plotdat) + geom_sf(aes(fill=fit), col = NA) +
  scale_fill_viridis_c(name = expression(paste("Eiders (", km^{-2}, ")"))) + 
  theme_minimal() +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  coord_sf(xlim = c(-157.5, -155.5), ylim = c(70.9, 71.4), expand = FALSE)
####plot on link scale
p0 <- draw(fit, select = 1, rug = FALSE, caption = NULL) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

p2 <- draw(fit, select = 2, rug = FALSE, caption = NULL) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

tiff("Figures/Fig12.tif", res = 300, width = 2250, height = 2250, compression = "lzw")
p0 + p2 + p1 + plot_layout(nrow = 3, ncol = 1, heights = 2) + 
  plot_annotation(tag_levels = "A") +
  theme(text = element_text(face = "bold"))
dev.off()
###########################################################################
## Figure 13
#model object fit read in above for figure 12
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
##try MH sampling, need to turn acceptance rate, 0.02 seems to work for fit1
b <- gam.mh(fit, ns=3000, thin = 2, rw.scale = 0.02)
b <- b$bs
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
sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                    Mean = apply(post, 2, mean), 
                    sd = apply(post, 2, sd), 
                    median = apply(post, 2, median), 
                    upper = apply(post, 2, quantile, probs = 0.975),
                    lower = apply(post, 2, quantile, probs = 0.025))
#plot
p1 <- ggplot(data = sumdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimate") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
#add detection
#read in posterior data frame saved from file combine_ABR_ACP
post <- readRDS(file = "results/Comb_post.RDS")
#summarize
sumdf2 <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                    Mean = apply(post, 2, mean), 
                    sd = apply(post, 2, sd), 
                    median = apply(post, 2, median), 
                    upper = apply(post, 2, quantile, probs = 0.975),
                    lower = apply(post, 2, quantile, probs = 0.025))
#plot
p2 <- ggplot(data = sumdf2) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimate") + 
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
tiff("Figures/Fig13.tif", res = 600, width = 2250, height = 900, compression = "lzw")
plot_grid(p1, p2, ncol = 1, align = "v", axis = "l", labels = "AUTO", 
          hjust = -6.5, vjust = 2, label_size = 15)
dev.off()
############################################################################
## Figure 14
#find long term posterior mean and CI
m = round(mean(apply(post[,], 1, mean, na.rm = TRUE)), 1)
q = round(quantile(apply(post[,], 1, mean, na.rm = TRUE), 
                   probs=c(0.025, 0.05, 0.5, 0.975)), 1)
#find year-specific summaries for csv output
df <- data.frame(Year = 1999:2024,
                 Mean = apply(post, 2, mean), 
                 sd = apply(post, 2, sd), 
                 median = apply(post, 2, median), 
                 upper = apply(post, 2, quantile, probs = 0.975),
                 lower = apply(post, 2, quantile, probs = 0.025))
#write_csv(df, file = "results/model_est_Comb.csv")

df <- data.frame(NULL)
tyears <- dim(post)[2]
for( t in 1:(tyears-1)){
  trend <- (log(post[,tyears]) - log(post[,tyears-t]))/t
  df <- rbind(df, data.frame(t = t, mtrend = mean(trend, na.rm = TRUE), 
                             med = quantile(trend, probs = 0.5, na.rm = TRUE), 
                             upper = quantile(trend, probs = 0.975, na.rm = TRUE), 
                             lower = quantile(trend, probs = 0.025, na.rm = TRUE)))
}

p1 <- ggplot(data = df, aes(x = t, y=mtrend)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line() + 
  geom_line(aes(y = med), linetype = "dashed") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(2, 24, by = 2)) + 
  scale_y_continuous(breaks = seq(-0.5, 2.5, by = 0.25)) + 
  ylab("log Trend") + 
  xlab("Lag") +
  theme_minimal() + 
  theme(panel.grid.minor = element_blank())
tiff("Figures/Fig14.tif", res = 600, width = 2250, height = 900, compression = "lzw")
p1
dev.off()
#############################################################################
## Figure 15
library(WaveletComp)
set.seed(123)
post <- readr::read_csv(file = "results/model_est_Comb.csv")
invisible(capture.output(my.w <- analyze.wavelet(post, "Mean",
                                                 loess.span = 0,
                                                 dt = 1, dj = 1/250,
                                                 lowerPeriod = 1,
                                                 upperPeriod = 20,
                                                 make.pval = TRUE, n.sim = 500, 
                                                 verbose = FALSE)))
period95 <- my.w$Period[which(my.w$Power.avg.pval < 0.025)]
lower <- dplyr::first(period95)
upper <- dplyr::last(period95)
period.max <- my.w$Period[which(my.w$Power.avg == max(my.w$Power.avg))]
#find lower power ridges and p-values
#first the long period 
long.period <- my.w$Period[my.w$Period > 10]
long.power <- my.w$Power.avg[my.w$Period > 10]
long.p.val <- my.w$Power.avg.pval[my.w$Period > 10]
long.max <- long.period[which(long.power == max(long.power))]
long.max.p <- long.p.val[which(long.power == max(long.power))]
tiff("Figures/Fig15.tif", res = 600, width = 2250, height = 1125, compression = "lzw")
wt.image(my.w, color.key = "quantile", n.levels = 250, timelab = "Year",
         siglvl = 0.025,
         legend.params = list(lab = "Wavelet power levels", mar = 4.7), 
         spec.time.axis = list(at = seq(2, 26, by = 2),
                               labels = seq(2, 26, by = 2)+1998), 
         periodlab = "")
title(ylab = "Period", line = 1.5)
dev.off()
