#predict STEI in triangle and other ACP locations
#  Compare Triangle and ACP estimate
#  "Power analysis" of Triangle data to see how much is gained or how much would 
#    be needed to be added to the ACP survey to do equivalent
#
# Initial code written Sep. 2024 for "October "eider meeting"
# Update for SSA and 5-year review, November 2024
library(tidyverse)
library(mgcv)
library(sf)
library(units)
#load model results
load("abr.RData")
#predict over grid for partial effect:
grid.cen <- st_centroid(grid) |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(Year = 1999, Area = drop_units(set_units(st_area(grid), "km^2")))
df <- grid.cen
preds <- predict(fit, newdata = df, type = "response", 
                 exclude = c("s(Year)", "s(X,Y,Year)"))
df <- cbind(grid.cen, grid, preds) |>
  mutate(Eiders = Area*preds) |>
  st_as_sf(sf_column_name = "geometry", crs = st_crs(grid))
ggplot(data = df) + geom_sf(aes(fill = Eiders), col = NA) + 
  scale_fill_viridis_c(name = "Eiders (km^-2)") + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())
ggsave("results/average_density_ABR.png")
################################################################################
#now include year
df <- data.frame(NULL)
for(i in 1999:2023){
  df <- rbind(df, mutate(grid.cen, Year = i))
}
# preds <- predict(fit1.1, newdata = df, type = "response")
# df2 <- cbind(df, preds) |>
#   mutate(Eiders = Area*preds) |>
#   group_by(Year) |>
#   summarise(Eiders = sum(Eiders))
# ggplot(data = df2, aes(x=Year, y=Eiders)) + geom_line() + geom_point()
# #add observed eider number to plot
# obs <- birds2 |> group_by(Year) |>
#   summarise(Eiders = sum(Num)) |>
#   mutate(Type = "Observed") |>
#   rbind(mutate(df2, Type = "Modelled"))
# ggplot(data = obs, aes(x=Year, y=Eiders, col = Type)) + geom_line() + geom_point()
# #simulate posterior
# 
# df <- df |> select(Year, Area, X, Y) |>
#   mutate(Observer = "HMW") #map_total2 requires and observer, should generalize
# #source("../ACP-Mapping/map_density_functions.R")
# #map_total2 does is in pairs, not indicated birds
# #post <- map_total2(gamfit = fit1.1, Nsamples = 250, newdata = df, exclude = NULL)
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
sumdf <- data.frame(Year = min(fit2$model$Year):max(fit2$model$Year),
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
ggsave("results/ABR_noD_ibb_year.png")
################################################################################
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

post_df <- function(post = NULL, fit = NULL){
  df <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
                   Mean = apply(post, 2, mean), 
                   sd = apply(post, 2, sd), 
                   median = apply(post, 2, median), 
                   upper = apply(post, 2, quantile, probs = 0.975),
                   lower = apply(post, 2, quantile, probs = 0.025))
  return(df)
}
postdf <- post_df(post = post, fit = fit2)
gg <- ggplot(data = postdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Indicated Breeding Birds")
print(gg)
ggsave("results/ABR_withD_ibb_year.png")
saveRDS(post, file = "results/ABR_post.RDS")
# #find long term posterior mean and CI
# m = round(mean(apply(post[,-c(1:5)], 1, mean)), 1)
# q = round(quantile(apply(post[,-c(1:5)], 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
# gg + geom_text(aes(x = 2003, y = 1000, label = "20-year mean", hjust = 0)) + 
#   geom_text(aes(x = 2003, y = 950, hjust = 0,
#                 label = paste0(m, " (95% CI: ", q[1], ", ", q[4], ")")))
# ggsave("results/")

