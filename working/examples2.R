#install.packages("ggmap")
#library(osmdata)
#library(ggmap)
library(dplyr)
library(sf)
library(units)
library(devtools)
library(tmap)
load_all()


bbox = st_bbox(c(xmin = 534773.3, ymin = 176901.5, xmax = 543162.4, ymax = 181853.3), crs = 27700)
bbox_poly = st_as_sfc(bbox)
thames_poly = thames

tmap_options(frame = FALSE)

(p1 = tm_shape(thames) + tm_polygons())

p1 + tm_shape(bbox_poly) + tm_borders()

##############################################################################################
osm = bbox %>%
  opq() %>%
  add_osm_feature("water") %>%
  osmdata_sf()

q1 = osm[6][[1]]
names(q1)

plot(q1$geometry)

#q2 = bbox %>%
#  opq() %>%
#  add_osm_feature("water") %>%
#  osmdata_sf()

q2 = osm[8][[1]]
names(q2)

plot(q2$geometry, add = TRUE)

q = st_union(st_union(q1, q2, by_feature = TRUE))
plot(q)

bbox_poly = st_as_sfc(bbox)

g = st_intersection(q, bbox_poly)
plot(g)

g = st_transform(g, 27700)
bbox_poly = st_as_sfc(st_bbox(st_transform(bbox_poly, 27700)))

g = st_as_sf(st_cast(st_cast(g, "POLYGON"), "LINESTRING"))
lengths(g$x)
g = g[33,]

plot(g$x)

lengths(g$x)

g = st_sf(st_line_sample(g, n = 500))

lengths(g$st_line_sample.g..n...500.)


g = st_sf(st_cast(g, "POLYGON"))

plot(g, col = "red")

rm(q, q1, q2)

st_write(g, "working/thames.gpkg", delete_layer = TRUE)
st_write(bbox_poly, "working/bbox_poly.gpkg", delete_layer = TRUE)

rm(list = ls())

g = st_read("working/thames.gpkg")
bbox_poly = st_read("working/bbox_poly.gpkg")



bbox_poly_minus50 = st_buffer(bbox_poly, dist = set_units(-300, "m"))
plot(bbox_poly_minus50, add = TRUE)

bbox_line_minus50 = st_cast(bbox_poly_minus50, "LINESTRING")

bbox = st_bbox(bbox_poly, crs = 27700)

#______________________________________________________________________________#


m = midlines_draw(g, border_line = bbox_line_minus50)

plot(g$geom)
plot(m$geometry, col = m$line_id, add = TRUE)
plot(bbox_poly_minus50$geom, add = TRUE)

m2 = midlines_clean(m, n_removed = 1, border_line = bbox_line_minus50)

plot(g$geom)
plot(m2$geometry, col = m2$removed_flag, add = TRUE)
table(m2$removed_flag)

plot(m2$geometry[m2$removed_flag==0])

m3 = midlines_check(m2, n_removed = 10, border_line = bbox_line)

plot(m3$geometry, col = m3$removed_flag2, add = TRUE)

m2 = filter(m2, m2$removed_flag==0)

m3 = midlines_clean(m2, n_removed = 10, border_line = bbox_line)

#plot(gg$geom)
plot(m3$geometry[m3$removed_flag==0])
table(m3$removed_flag)

plot(m3$geometry)

plot(m3$geometry[m3$removed_flag==0], col = m3$line_id)

m3 = filter(m3, m3$removed_flag==0)


m4 = midlines_clean(m3, n_removed = 10, border_line = bbox_line)
plot(m4$geometry[m4$removed_flag==0], col = "red")

####

#investigate
m2 = filter(m2, m2$removed_flag==0)
m2 = midlines_group(m2)
m2 = st_cast(m2, "LINESTRING")
m2$lines_id = 1:nrow(m2)

plot(m2$geometry, col = m2$lines_id)

m2$length = st_length(m2)

plot(m2$geometry[2])

plot(m$geometry, col = m3$line_id, add = TRUE)
