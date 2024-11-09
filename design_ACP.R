# calculate design-based estimate and apply a detection correction via simulation
df <- AKaerial::ACPHistoric$combined |>
  filter(Species == "STEI") |>
  select(Year, ibb, ibb.se) |>
  mutate(upper = ibb + 2*ibb.se, lower = if_else(ibb - 2*ibb.se < 0, 0, ibb - 2*ibb.se))

ggplot(data = df) + geom_ribbon(aes(x = Year, ymin = lower, ymax = upper), fill = "orange") + 
  geom_line(aes(x = Year, y = ibb))