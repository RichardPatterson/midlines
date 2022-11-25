## code to prepare `thames` dataset goes here

library(osmdata)
library(dplyr)
library(sf)
library(units)
library(usethis)


bbox = st_bbox(c(xmin = -0.0590, xmax = 0.0600, ymin = 51.4750, ymax = 51.5175), crs = 4326)

osm = bbox %>%
  opq() %>%
  add_osm_feature("water") %>%
  osmdata_sf()

q1 = osm[6][[1]]
q2 = osm[8][[1]]

q = st_union(st_union(q1, q2, by_feature = TRUE))
#plot(q)

#bbox_poly = st_as_sfc(bbox)

g = st_transform(q, 27700)

#bbox = st_bbox(c(xmin = 534773.3, ymin = 176901.5, xmax = 543162.4, ymax = 181853.3), crs = 27700)
#bbox = st_bbox(c(xmin = 534770, ymin = 177800, xmax = 543160, ymax = 181850), crs = 27700)
bbox = st_bbox(c(xmin = 534770, ymin = 177800, xmax = 542860, ymax = 181850), crs = 27700)


g = st_intersection(g, st_as_sfc(bbox))
plot(g)

#bbox_poly = st_as_sfc(st_bbox(st_transform(bbox_poly, 27700)))

g = st_as_sf(st_cast(st_cast(g, "POLYGON"), "LINESTRING"))

# keep the long one to removed extraneous bodies of water
g = g[lengths(g$x) == max(lengths(g$x)),]

plot(g$x)

lengths(g$x)

g = st_sf(st_line_sample(g, n = 500))

colnames(g)[colnames(g) == "st_line_sample.g..n...500."] = "geometry"
st_geometry(g) <- "geometry"

lengths(g$geometry)


g = st_sf(st_cast(g, "POLYGON"))

thames = g

plot(thames, col = "red")

# Two attempts to remove the R CMD check warning about non-ASCII characters. Neither worked.
# Only a problem for CRAN submission

# 1) remove CRS as package check flag non ASCII characters in the CRS
# st_crs(thames) = NA

# 2) removing does't work so trying this to get rid of the offending symbols https://github.com/r-spatial/sf/issues/1341#issuecomment-1120284345
#st_crs(thames)$wkt <- gsub("°|º", "\\\u00b0", st_crs(thames)$wkt)

save(thames, file = "data-raw/thames.RData")

usethis::use_data(thames, overwrite = TRUE)
