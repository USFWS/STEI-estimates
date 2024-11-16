#function for general use in the STEI-estimate project

#draw a GAM response surface after sampling from posterior
#can't figure out how to use the sampling function in gratia, so I wrote my own that seem to work
draw_post <- function(fit = NULL, n_samples = 3, select = 1, dist = 0.02, rug = FALSE){
  library(mgcv)
  library(gratia)
  m <- list()
  m[[1]] <- draw(fit, select = 1, dist = 0.02, rug = FALSE, )
  sm1 <- fit
  for(i in 1:n_samples){
    sm1$coefficients <- rmvn(1, coef(fit), vcov(fit))
    m[[i+1]] <- draw(sm1, select = 1, dist = 0.02, rug = FALSE)
  }
  return(m)
}
#test it
################################################################################
##write a function that accepts a GAM fot object, prediction grid and data,
#  and returns a posterior distribution of the population size

# fit <- fit.ds.small
# acp <- st_read(dsn="../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg")
# #make grid
# acp <- select_area(area = acp, select = c(1,5)) %>%
#   st_transform(crs=3338)
# grid <- st_intersection(acp, st_make_grid(x=acp, cellsize = 1000)) %>%
#   mutate(Sample.Label = row.names(.), Grid.Area = st_area(.))
# newdat <- st_centroid(grid) %>%
#   st_coordinates() %>%
#   as.data.frame() %>%
#   mutate(Observer = "HMW", logArea = 0, Year = 2013) 
# # now simulation population total by year
# #add Area to newdat
# library(units)
# newdat$Area <- drop_units(set_units(grid$Grid.Area, "km^2"))
# df <- data.frame(NULL)
# for(i in 2007:2023){
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
# }
# #summarize
# sumdf <- data.frame(Year = min(fit$model$Year):max(fit$model$Year),
#                     Mean = apply(post, 2, mean), 
#                     sd = apply(post, 2, sd), 
#                     Median = apply(post, 2, median), 
#                     upper = apply(post, 2, quantile, probs = 0.975),
#                     lower = apply(post, 2, quantile, probs = 0.025))
# #plot
# gg <- ggplot(data = sumdf) + 
#   geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
#   geom_line(aes(x = Year, y = Mean)) + 
#   geom_line(aes(x = Year, y = Median), linetype = 2) + 
#   scale_x_continuous(breaks = seq(1999, 2025, by = 2)) + 
#   ylab("Estimated Indicated Bird Index") +
#   labs(title = "Estimated breeding bird index across ACP")
# print(gg)
