# calculate design-based estimate
# df <- AKaerial::ACPHistoric$combined |>
#   filter(Species == "STEI") |>
#   select(Year, ibb, ibb.se) |>
#   mutate(upper = ibb + 2*ibb.se,
#          lower = if_else(ibb - 2*ibb.se < 0, 0, ibb - 2*ibb.se))
# 
# ggplot(data = df, aes(x = Year, y=ibb, group=Year<2020)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper),
#               fill = "orange", alpha = 0.5) +
#   geom_line() +
#   geom_point() +
#   scale_x_continuous(breaks = seq(2006, 2025, by = 2)) +
#   scale_y_continuous(limits = c(0, 1275)) +
#   ylab("Indicated Breeding Bird Index") 
# # +
# #   labs(title = "Design-based Estimated breeding bird index on the ACP (no detection)")
################################################################################
##use ACP-Mapping data and "R3" of Fewster
path = "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignStrata_QC.gpkg"
acp <- st_read(dsn=path)
acp <- mutate(acp, StratumArea = units::set_units(st_area(acp), "km^2"))

path = "../ACP-Mapping/Data/ACP_2023/analysis_output/Bird-QC-Obs-2024-11-12.csv"
birds <- read_csv(file = path) |>
  filter(Species == "STEI") |>
  mutate(Obs_Type = replace(Obs_Type, Obs_Type == "open", "single"))
table(birds$Year, birds$Obs_Type)
path = "../ACP-Mapping/Data/ACP_2023/analysis_output/ACP_DesignTrans_QC_2024-11-12.gpkg"
trans <- st_read(dsn = path)
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
         upper = Total + 2*sd_Total, 
         lower = if_else(Total - 2*sd_Total < 0, 0, Total - 2*sd_Total))

ggplot(data = birds2, aes(x = Year, y=Total, group=Year<2020)) +  
  geom_ribbon(aes(ymin = lower, ymax = upper), 
              fill = "orange", alpha = 0.5) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2)) + 
  scale_y_continuous(limits = c(0, 2000)) + 
  ylab("Indicated Breeding Bird Index")
ggsave("results/acp_design-nodetection.png")
write_csv(birds2, file = "results/design_estimates_ACP.csv")
