library(sf)
library(tidyverse)
Nepal<-read_sf("../Data/Nepal_fug.shp")
Roads<-read_sf("../Data/NepalRoads/hotosm_npl_roads_lines_shp.shp")
View(Roads)




# Ensure both shapefiles use the same CRS (coordinate reference system)
Roads <- st_transform(Roads, crs = st_crs(Nepal))

# Calculate the total area of each jurisdiction
Nepal <- Nepal %>% 
  mutate(area_sq_km = as.numeric(st_area(geometry) / 1e6)) # Convert from square meters to square kilometers

# Clip roads to each jurisdiction and calculate their total length
road_density <- Nepal%>% 
  rowwise() %>% 
  mutate(
    road_length_km = {
      # Clip roads to the current jurisdiction. Use geos firs bc it's much faster
      sel = unlist(geos::geos_intersects_matrix(geometry, Roads))
      clipped_roads<-Roads[sel, ]
      clipped_roads <- st_intersection(clipped_roads, geometry)
      
      # Calculate the total road length in kilometers
      sum(as.numeric(st_length(clipped_roads))) / 1e3
    },
    road_density_km_per_sq_km = road_length_km / area_sq_km # Calculate road density
  )


# Convert back to a regular (non-rowwise) dataframe
road_density2 <- ungroup(road_density)

# Save the results to a new shapefile
st_write(road_density2, "../Data/Covariates/RoadDensity/road_density_by_VDC.shp")




ggplot(road_density2)+
  geom_sf(aes(fill=road_density_km_per_sq_km ),color=alpha("white",alpha=0.01))
