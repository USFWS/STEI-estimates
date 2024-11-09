#based off of code in ACP-Mapping/fit_STEI.R

library(tidyverse)
library(sf)
library(mgcv)
source("../ACP-Mapping/map_density_functions.R")
#select species
spp <- "STEI"
#read in lines and birds data
lines <- st_read(dsn = "../ACP-Mapping/Data/ACP_2023/analysis_output/Lines-Obs-2024-02-15.gpkg")
birds <- read_csv(file = "../ACP-Mapping/Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-03-21.csv") %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
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
# fit0 <- gam(Count~s(X, Y, k = 200),
#             offset = logArea, family = nb, method = "REML", data = df)
fit1 <- gam(Count~s(X, Y, k = 200) + s(Year, k = 14),
            offset = logArea, family = nb, method = "REML", data = df)
draw(fit1, select = 1, dist = 0.02, rug = FALSE)
sm1 <- fit1
sm1$coefficients <- rmvn(1, coef(fit1), vcov(fit1))
draw(sm1, select = 1, dist = 0.02, rug = FALSE)
sm1$coefficients <- rmvn(1, coef(fit1), vcov(fit1))
draw(sm1, select = 1, dist = 0.02, rug = FALSE)
sm1$coefficients <- rmvn(1, coef(fit1), vcov(fit1))
draw(sm1, select = 1, dist = 0.02, rug = FALSE)
# fit2 <- gam(Count~s(X, Y, k = 200) + s(Year, k = 14) + 
#               ti(X, Y, Year, k = c(50, 5), d=c(2, 1)),
#             offset = logArea, family = nb, method = "REML", data = df)
fit.ds <- gam(Count~s(X, Y, bs="ds", k = 200, m=c(1,.5)) + s(Year, k = 14), 
              offset = logArea, family = nb, method = "REML", 
              data = df)
fit.ds.small <- gam(Count~s(X, Y, bs="ds", k = 10, m=c(1,.5)) + s(Year, k = 14), 
              offset = logArea, family = nb, method = "REML", 
              data = df)
AIC(fit1, fit.ds, fit.ds.small)
draw(fit.ds, select = 1, dist = 0.02, rug = FALSE)
sm1 <- fit.ds
sm1$coefficients <- rmvn(1, coef(fit.ds), vcov(fit.ds))
draw(sm1, select = 1, dist = 0.02, rug = FALSE)
sm1$coefficients <- rmvn(1, coef(fit.ds), vcov(fit.ds))
draw(sm1, select = 1, dist = 0.02, rug = FALSE)
sm1$coefficients <- rmvn(1, coef(fit.ds), vcov(fit.ds))
draw(sm1, select = 1, dist = 0.02, rug = FALSE)
source("STEI_functions.R")
m=draw_post(fit = fit.ds.small)
ggpubr::ggarrange(plotlist = m)
m=draw_post(fit = fit.ds)
ggpubr::ggarrange(plotlist = m)

library(gratia)
draw(fit1, select = 1, dist = 0.02, rug = FALSE)
#wow, that is smooth!
b <- rmvn(1, coef(fit2), vcov(fit2))
fit.new <- fit2
fit.new$coefficients <- b
draw(fit.new, select = 1, dist = 0.02, rug = FALSE)
draw(readRDS("../ACP-Mapping/Data/ACP_2023/analysis_output/gam/STEI_fit.RDS"), 
     select = 1, dist = 0.02, rug = FALSE)
##OK, so the tprs leads to a much smoother density gradient. 
#let's save the tprs and ds, and ds.small models and then predict on them to compare
saveRDS(fit1, file = "results/fit.tp.RDS")
saveRDS(fit.ds, file = "results/fit.ds.RDS")
saveRDS(fit.ds.small, file = "results/fit.ds.small.RDS")
################################################################################

