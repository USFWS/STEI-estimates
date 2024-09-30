#####
##UPDATED FROM ACP MAPPING to change names of cen2 object to "x". HAve not tested or push commit to ACP Mapping
#function to associate birds observation to transect segments
# then adds effort and covariate data if it exists
# center points of the segments is used as the spatial location
get_data2 <- function(x = NA, y = NA, area = NA, Spp = NA, grid = NA, 
                     buff = 1000, covs = NULL){
  #x is df of bird observation, geographic coordinates must be named "Lon" and "Lat"
  #  and be in geographic coordinates: crs = 4326
  #y is the transect line segmented using make_segments function, 
  #  must have an id for each segment named "Sample.Label"
  #area is polygon of surveyed area
  #Spp is Species to filter and associate
  #grid is the survey area grid
  #buff is a distance to buffer the survey area and clip observation outside this distance
  #covs is an optional df of covariates for each segment, not implemented
  #Must only supply one year at a time
  #
  #Returns a data frame not sf object
  
  library(dplyr)
  library(sf)
  
  Year <- unique(x$Year)
  area <- st_transform(area, crs = st_crs(grid))
  #find center points of the segments
  cen <- y %>%
    st_transform(crs = st_crs(grid)) %>%
    st_centroid() %>%
    #st_join(area, join=st_nearest_feature) %>%
    select(Sample.Label, Segment.Label, Length, Area)
  
  #Need to duplicate segments for each observer in x
  Observer <- unique(x$Observer)
  cen2 <- mutate(cen, Observer = Observer[1])
  if( length(Observer) > 1){
    for(i in 2:length(Observer)){
      cen2 <- mutate(cen, Observer = Observer[i]) %>%
        rbind(cen2)
    }
  }
  st_geometry(cen2) <- "x"
  obs <- x %>%
    filter(Species == Spp) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs=st_crs(4326)) %>%
    st_transform(crs=st_crs(grid)) %>%
    st_filter(st_buffer(area, dist = buff)) %>% #filter out observations out of (buffered) area
    st_join(y, join=st_nearest_feature) %>% # join segments and obs
    select(Year, Month, Day, Time, Observer, Num, Obs_Type, Sample.Label, 
           Segment.Label, Length, Area) %>%
    #mutate(Count = ifelse(Obs_Type == "pair", 2*Num, Num)) %>% #transform to "total birds"
    filter(Obs_Type %in% c("single", "pair")) %>% #retain just the singles ("indicated pairs") and observed pairs
    group_by(Segment.Label, Observer) %>%
    #summarize(Count = sum(Count)) %>% #if type is "total birds", condition on type not implemented
    summarize(Count = sum(Num)) %>% 
    right_join(st_drop_geometry(y)) %>% #join with effort data, right join to retain zeros
    #mutate(Year = Year, Observer = Observer) %>%
    st_drop_geometry() %>%
    right_join(cen2) %>% #add center points of segments, retain Observers
    mutate(Count = replace(Count, is.na(Count), 0),  #replace NAs with zeros
           Year = Year) %>%
    arrange(Segment.Label) %>% 
    relocate(Year) %>% 
    relocate(Segment.Label, .after=Count) %>%
    ungroup() %>%
    st_as_sf(sf_column_name = "x", crs = st_crs(y)) %>%
    mutate(Length = units::drop_units(Length), Area = units::drop_units(Area)) %>%
    mutate(Length = Length/1000, Area = Area/(1000*1000), 
           logArea = log(Area)) #change units and add logArea offset
  
  return( cbind( st_drop_geometry(obs), st_coordinates(obs) ) )
}
#####