#based off of code in ACP-Mapping/fit_STEI.R

library(tidyverse)
library(sf)
library(mgcv)
source("../ACP-Mapping/map_density_functions.R")
#select species
spp <- "STEI"
#read in lines and birds data
lines <- st_read(dsn = "../ACP-Mapping/Data/ACP_2023/analysis_output/Lines-Obs-2024-11-12.gpkg")
birds <- read_csv(file = "../ACP-Mapping/Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-11-12.csv") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326) |>
  #Need to replace the "open" STEI observation with "single", do it here for now :(
  mutate(Obs_Type = replace(Obs_Type, Species == "STEI" & Obs_Type == "open", "single"))
acp <- st_read(dsn="../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
#make grid
acp <- select_area(area = acp, select = "all") %>%
  st_transform(crs=3338)
grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 6000)) %>%
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
#fit GAM
fit0 <- gam(Count~s(X, Y, k = 200),
            offset = logArea, family = nb, method = "REML", data = df)
fit1 <- gam(Count~s(X, Y, k = 200) + s(Year, k = 14),
            offset = logArea, family = nb, method = "REML", data = df)
# draw(fit1, select = 1, dist = 0.02, rug = FALSE)
# sm1 <- fit1
# sm1$coefficients <- rmvn(1, coef(fit1), vcov(fit1))
# draw(sm1, select = 1, dist = 0.02, rug = FALSE)
# sm1$coefficients <- rmvn(1, coef(fit1), vcov(fit1))
# draw(sm1, select = 1, dist = 0.02, rug = FALSE)
# sm1$coefficients <- rmvn(1, coef(fit1), vcov(fit1))
# draw(sm1, select = 1, dist = 0.02, rug = FALSE)
fit2 <- gam(Count~s(X, Y, k = 200) + s(Year, k = 14) +
              ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("tp", "cr")),
            offset = logArea, family = nb, method = "REML", data = df)
#fit models with Observer covariate
df$Observer <- factor(df$Observer)
fit0.o <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re"),
              offset = logArea, family = nb, method = "REML", data = df)
fit1.o <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re") + 
                s(Year, k = 14),
              offset = logArea, family = nb, method = "REML", data = df)
fit2.o <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re") + 
                s(Year, k = 14) + 
                ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("tp", "cr")), 
              offset = logArea, family = nb, method = "REML", data = df)
#add random effect of year
df$fYear <- factor(df$Year)
fit0.re <- gam(Count~s(X, Y, k = 200) + s(fYear, bs = "re"),
               offset = logArea, family = nb, method = "REML", data = df)
fit1.re <- gam(Count~s(X, Y, k = 200) + s(Year, k = 14) + 
                 s(fYear, bs = "re"),
               offset = logArea, family = nb, method = "REML", data = df)
fit2.re <- gam(Count~s(X, Y, k = 200) + s(Year, k = 14) + 
                 s(fYear, bs = "re") + 
                 ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("tp", "cr")), 
               offset = logArea, family = nb, method = "REML", data = df)
fit0.o.re <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re") + 
                   s(fYear, bs = "re"),
                 offset = logArea, family = nb, method = "REML", data = df)
fit1.o.re <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re") + 
                   s(fYear, bs = "re") + s(Year, k = 14),
                 offset = logArea, family = nb, method = "REML", data = df)
fit2.o.re <- gam(Count~s(X, Y, k = 200) + s(Observer, bs = "re") + 
                   s(fYear, bs = "re") + s(Year, k = 14) + 
                   ti(X, Y, Year, k = c(50, 5), d=c(2, 1), bs = c("tp", "cr")), 
                 offset = logArea, family = nb, method = "REML", data = df)
aic <- AIC(fit0, fit0.o, fit1, fit1.o, fit2, fit2.o, fit0.re, fit1.re, fit2.re,
           fit0.o.re, fit1.o.re, fit2.o.re)
save.image("results/ACP.RData")
#load("results/ACP.RData")
aic
summary(fit0.re)
summary(fit1.re)
summary(fit2.re)
summary(fit0.o.re)
summary(fit1.o.re)
summary(fit2.o.re)
library(gratia)
draw(fit0.re, rug = FALSE)
draw(fit1.re, rug = FALSE)
draw(fit2.re, rug = FALSE)
draw(fit2.re, select = 4, rug = FALSE)
draw(fit0.o.re, rug = FALSE)
draw(fit1.o.re, rug = FALSE)
draw(fit2.o.re, rug = FALSE)
draw(fit2.o.re, select = 5, rug = FALSE)
aic$DeltaAIC <- aic$AIC - min(aic$AIC)
round(aic, 1)
draw(fit1, rug = FALSE)
# source("STEI_functions.R")
# m=draw_post(fit = fit0.re)
# ggpubr::ggarrange(plotlist = m)
# m=draw_post(fit = fit.ds)
# ggpubr::ggarrange(plotlist = m)
# 
# library(gratia)
# draw(fit1, select = 1, dist = 0.02, rug = FALSE)
# #wow, that is smooth!
# b <- rmvn(1, coef(fit2), vcov(fit2))
# fit.new <- fit2
# fit.new$coefficients <- b
# draw(fit.new, select = 1, dist = 0.02, rug = FALSE)
# draw(readRDS("../ACP-Mapping/Data/ACP_2023/analysis_output/gam/STEI_fit.RDS"), 
#      select = 1, dist = 0.02, rug = FALSE)
# ##OK, so the tprs leads to a much smoother density gradient. 
# #let's save the tprs and ds, and ds.small models and then predict on them to compare
# saveRDS(fit1, file = "results/fit.tp.RDS")
# saveRDS(fit.ds, file = "results/fit.ds.RDS")
# saveRDS(fit.ds.small, file = "results/fit.ds.small.RDS")
# ################################################################################

