#predict STEI in triangle and other ACP locations
#  Compare Triangle and ACP estimate
#  "Power analysis" of Triangle data to see how much is gained or how much would 
#    be needed to be added to the ACP survey to do equivalent
#
# Initial code written Sep. 2024 for "October "eider meeting"
library(tidyverse)
library(mgcv)
library(sf)
library(units)
#load model results
load(".RData")

# #AIC table
# AIC(fit0, fit1, fit1.1, fit1.re, fit2)
# #fit1.1 is best by above 2-3
# summary(fit1.1)
# plot(fit1.1)
#for no flier need to load fit1.1.noflier, and replace fit1.1 with fit1.1.nofliers
fit1.1 <- readRDS(file = "results/fit1.1.nofliers.RDS")
#predict over grid for each year:
grid.cen <- st_centroid(grid) |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(Year = 1999, Area = drop_units(set_units(st_area(grid), "km^2")))
df <- grid.cen
preds <- predict(fit1.1, newdata = df, type = "response", exclude = "Year")
df <- cbind(grid.cen, grid, preds) |>
  mutate(Eiders = Area*preds) |>
  st_as_sf(sf_column_name = "geometry", crs = st_crs(grid))
ggplot(data = df) + geom_sf(aes(fill = Eiders), col = NA) + 
  scale_fill_viridis_c(name = "Eiders") +
  labs(title="Average density (km^-2) of STEI 1999-2023")
ggsave("results/average_density.png")
sum(df$Eiders)
##above is marginal effect of location,
#now include year
df <- data.frame(NULL)
for(i in 1999:2023){
  df <- rbind(df, mutate(grid.cen, Year = i))
}
preds <- predict(fit1.1, newdata = df, type = "response")
df2 <- cbind(df, preds) |>
  mutate(Eiders = Area*preds) |>
  group_by(Year) |>
  summarise(Eiders = sum(Eiders))
ggplot(data = df2, aes(x=Year, y=Eiders)) + geom_line() + geom_point()
#add observed eider number to plot
obs <- birds2 |> group_by(Year) |>
  summarise(Eiders = sum(Num)) |>
  mutate(Type = "Observed") |>
  rbind(mutate(df2, Type = "Modelled"))
ggplot(data = obs, aes(x=Year, y=Eiders, col = Type)) + geom_line() + geom_point()
#simulate posterior

df <- df |> select(Year, Area, X, Y) |>
  mutate(Observer = "HMW") #map_total2 requires and observer, should generalize
#source("../ACP-Mapping/map_density_functions.R")
#map_total2 does is in pairs, not indicated birds
#post <- map_total2(gamfit = fit1.1, Nsamples = 250, newdata = df, exclude = NULL)
Nsamples <- 250
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit1.1, type="lpmatrix", newdata=df, exclude = NULL,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit1.1), vcov(fit1.1))
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
sumdf <- data.frame(Year = min(fit1.1$model$Year):max(fit1.1$model$Year),
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
  ylab("Estimated Indicated Bird Index") +
  labs(title = "Estimated breeding bird index in Triangle (no detection)")
print(gg)
ggsave("results/trianle_raw_ibb_year.png")
#add observed eiders
# obs <- birds2 |> group_by(Year) |>
#   summarise(Eiders = sum(Num))
# ggplot(data = df) + 
#   geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
#   geom_line(aes(x = Year, y = Mean)) + 
#   scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
#   ylab("Pairs") + 
#   geom_point(data = obs, aes(x=Year, y=Eiders))
#don't like so much with the observed data
#plot posterior for number if "indicated birds" and adjust for detection, 
#  Use interpolated value from 2018 DOS report, Table 3
#  Do not have distance or other covariate values, so will use approximate midpoint 
#    of bins 2 and 3, which is about 100m. 
# calculated as mean = (0.457+0.143)/2 = 0.3
#  mean cv = [((0.717-0.217)/(4*1.96))/0.457 + ((0.306-0.048)/(4*1.96))/0.143]/2
#          = 0.185 or sd ~= 0.3*0.185 = 0.0555
# Use Beta distribution to simulate, Beta(mean = 0.3, sd = 0.0555) = 
#   Beta(shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
#        shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1))
sdf <- data.frame(Sightability = seq(0, 0.75, length = 10000), 
                  Density = dbeta(seq(0, 0.75, length = 10000), 
                                  shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
                                  shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1)))
ggplot(data = sdf, aes(x = Sightability, y = Density)) + geom_area(fill = "orange") + 
  geom_text(aes(x = 0.1, y = 6, label = "Mean = 0.3")) + 
  geom_text(aes(x = 0.1, y = 5.5, label = "SD = 0.0555")) + 
  xlab("Detection") + 
  labs(title = "Detection Prior")
ggsave("results/sightability_prior.png")
#find posterior
Nsamples <- 250
s <- rbeta(Nsamples, 
           shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
           shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1))
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit1.1, type="lpmatrix", newdata=df, exclude = NULL,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit1.1), vcov(fit1.1))
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
postdf <- post_df(post = post, fit = fit1.1)
gg <- ggplot(data = postdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Birds") + 
  labs(title = "Estimated breeding birds in Triangle accounting for detection")
print(gg)
ggsave("results/triangle_ibb_year_1.png")
#find long term posterior mean and CI
m = round(mean(apply(post[,-c(1:5)], 1, mean)), 1)
q = round(quantile(apply(post[,-c(1:5)], 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
gg + geom_text(aes(x = 2003, y = 1000, label = "20-year mean", hjust = 0)) + 
  geom_text(aes(x = 2003, y = 950, hjust = 0,
                label = paste0(m, " (95% CI: ", q[1], ", ", q[4], ")")))
ggsave("results/triangle_ibb_year.png")
################################################################################
#Combine ACP and Triangle estimates:
#first read in ACP STEI model fit object:
path <- "../ACP-Mapping/Data/ACP_2023/analysis_output/gam/STEI_fit.RDS"
fit.acp <- readRDS(file = path)
path <- "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg"
acp <- st_read(dsn = path) |> select_area() 
#plot(st_geometry(acp))
#remove triangle area
acp <- acp |> st_difference(st_transform(triangle, crs = 4326))
plot(st_geometry(acp))
#too many little polygons!
acp <- st_cast(acp, "POLYGON")
areas <- units::set_units(st_area(acp), "km^2")
acp <- mutate(acp, Area = areas) |>
  filter(Area > set_units(100, "km^2"))
plot(st_geometry(acp))
#looks good enough
acp <- st_transform(acp, crs = 3338) #original ACP model was fit in 3338
grid.acp <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
  mutate(Sample.Label = row.names(.), GridArea = units::set_units(st_area(.), "km^2")) |>
  select(Sample.Label, GridArea)
grid.cen <- st_centroid(grid.acp) |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(Observer = "HMW", Area = drop_units(set_units(st_area(grid.acp), "km^2")))
df <- data.frame(NULL)
for(i in 2007:2023){
  df <- rbind(df, mutate(grid.cen, Year = i))
}
#sample posterior
Nsamples <- 250
s <- rbeta(Nsamples, 
           shape1 = 0.3*( 0.3*(1-0.3)/0.0555^2 - 1), 
           shape2 = (1 - 0.3)*( 0.3*(1-0.3)/0.0555^2 - 1))
post <- matrix(0, Nsamples, length(unique(df$Year)))

Xp <- predict(fit.acp, type="lpmatrix", newdata=df, exclude = NULL,
              newdata.guaranteed=TRUE)
#sample from parameter posterior
b <- rmvn(Nsamples, coef(fit.acp), vcov(fit.acp))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p) ) %>% #INDICATED BIRDS!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
#################
postdf <- post_df(post = post, fit = fit.acp)
gg <- ggplot(data = postdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Birds") + 
  labs(title = "Estimated breeding birds in ACP (excluding Triangle, no detection)")
print(gg)
ggsave("results/acp_nodetection_notriangle.png")
###################################################
## apply detection
post <- matrix(0, Nsamples, length(unique(df$Year)))
for(j in 1:Nsamples ){
  p <- exp(Xp%*%b[j,]) * as.vector(df$Area) #replicate of prediction at all points
  post[j,] <- cbind(df, p) %>%
    group_by(Year) %>%
    summarize( Total = sum(2*p/s[j]) ) %>% #INDICATED BIRDS and detection!!!
    ungroup() %>%
    select(Total) %>%
    unlist()
}
postdf <- post_df(post = post, fit = fit.acp)
gg <- ggplot(data = postdf) + 
  geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(x = Year, y = Mean)) + 
  geom_line(aes(x = Year, y = median), linetype = "dashed") + 
  scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
  ylab("Estimated Indicated Birds") + 
  labs(title = "Estimated breeding birds in ACP (excluding Triangle, with detection)")
print(gg)
ggsave("results/acp_withdetection_notriangle.png")
m = round(mean(apply(post, 1, mean)), 1)
q = round(quantile(apply(post, 1, mean), probs=c(0.025, 0.05, 0.5, 0.975)), 1)
gg + geom_text(aes(x = 2015, y = 12000, label = "17-year mean", hjust = 0)) + 
  geom_text(aes(x = 2015, y = 11000, hjust = 0,
                label = paste0(m, " (95% CI: ", q[1], ", ", q[4], ")")))
ggsave("results/acp_withdetection_notriangle.png")
saveRDS(post, file="results/post_ACP_notriangle_withdetection.RDS")
################################################################################
## Make a prediction over whole ACP using "indicated breeding birds"

################################################################################
#combine triangle and ACP estimates
#need to re-create the triangle posterior...
#why not just combine data and re-fit the model?
# post.all <- post[,-c(1:8)] + post.acp
# plot_post(post = post.all, years = 2007:2023)
# post.all2 <- post.all*2/.3
# plot_post(post = post.all2, years = 2007:2023)
# #find long term posteror mean and CI
# mean(apply(post.all2, 1, mean))
# quantile(apply(post.all2, 1, mean), probs=c(0.025, 0.05, 0.5, 0.975))         
# #look at proportion of eider in the triangle area:
# prop <- post2[,-c(1:8)]/post.all2
# plot_post(post = prop, years = 2007:2023)

