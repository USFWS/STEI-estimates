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
#bonus model:
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
#another bonus:
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
round(aic, 2)
draw(fit1, rug = FALSE)
source("STEI_functions.R")
m=draw_post(fit = fit0.re)
ggpubr::ggarrange(plotlist = m)
# m=draw_post(fit = fit.ds)
# ggpubr::ggarrange(plotlist = m)
#looks OK
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
#Predict based on fit0.re
library(units)
grid.cen <- st_centroid(grid) |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(fYear = "2010", Area = drop_units(set_units(st_area(grid), "km^2")))
df <- grid.cen
preds <- predict(fit0.re, newdata = df, type = "response", 
                 exclude = c("s(fYear)"))
df <- cbind(grid.cen, grid, preds) |>
  mutate(Eiders = Area*preds) |>
  st_as_sf(sf_column_name = "x", crs = st_crs(grid))
ggplot(data = df) + geom_sf(aes(fill = Eiders), col = NA) + 
  scale_fill_viridis_c(name = "Eiders (km^-2)") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())
ggsave("results/average_density_ACP.png")
################################################################################
#Posterior sample of time trend
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
b <- rmvn(Nsamples, coef(fit), vcov(fit))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(fYear) %>%
    summarize( Total = sum(2*p) ) %>% #INDICTED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#summarize
sumdf <- data.frame(Year = unique(df$fYear),
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
#OK, a very small proportion of the samples give extremely high value of total eiders
#This could be due to poor approximation of the posterior by the rmv sampling, see 
#the help files for gam.mh or https://gavinsimpson.github.io/gratia/articles/posterior-simulation.html
#this might be due to the large area of zero observation over the survey area, 
#  thus the likelihood is flat
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
gg.mh <- ggplot(data = sumdf.mh, aes(group=Year<2020)) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Indicated Breeding Bird Index") 
print(gg.mh)
hist(post.mh[,6])
#looks much better!
ggsave("results/ACP_noD_ibb_year.png")
################################################################################
##Now add detection
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
post.mh <- matrix(0, Nsamples, length(unique(df$fYear)))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post.mh[j,] <- cbind(df, p) %>%
    group_by(fYear) %>%
    summarize( Total = sum(2*p/s[j]) ) %>% #INDICTED BIRDS!!!
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
gg.mh <- ggplot(data = sumdf.mh, aes(group=Year<2020)) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Indicated Breeding Birds") 
print(gg.mh)
ggsave("results/ACP_withD_ibb_year.png")
saveRDS(post.mh, file = "results/ACP_post.RDS")

