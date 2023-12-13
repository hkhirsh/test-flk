#Test methods for finding spatial overlap 

# 10/5/2023 Runs on NOAA laptop 

library(sf)
library(dplyr)

p <- st_point(x=c(10, 10))
area <- st_buffer(p, 10)
polygons <- st_buffer(st_sample(area, 25), 1)

plot(polygons)

poly_sf=st_as_sf(polygons)

inter <- st_intersection(st_as_sf(polygons))
inter

plot(inter)

# a <- inter |>
#   filter(n.overlaps > 1)
# plot(polygons)
# plot(a, col = "blue", add = TRUE)


poly_diff <-
  st_difference(poly_sf, st_union(st_geometry(inter)))

plot(poly_diff)
plot(poly_sf)
plot(inter)

un=st_union(st_geometry(inter))
plot(un)
