
# plotting with tmap leave it looking like there is gaps which there arenot

#install.packages("ggmap")
#library(osmdata)
#library(ggmap)
library(dplyr)
library(sf)
library(units)
library(devtools)
library(tmap)
load_all()

#bbox = st_bbox(c(xmin = 534773.3, ymin = 176901.5, xmax = 543162.4, ymax = 181853.3), crs = 27700)

#bbox = st_bbox(c(xmin = 534770, ymin = 177800, xmax = 543160, ymax = 181850), crs = 27700)
#bbox_poly = st_as_sfc(bbox)
thames_poly = thames

bbox_poly = st_as_sfc(st_bbox(c(xmin = 534770, ymin = 177800, xmax = 542860, ymax = 181850), crs = 27700))

plot(thames_poly)


st_bbox(st_buffer(bbox_poly, dist = set_units(-300, "m")))

bbox_poly_small = st_as_sfc(st_bbox(c(xmin = 535070, ymin = 177800, xmax = 542560, ymax = 181550), crs = 27700))
#bbox_poly_small = st_as_sfc(bbox_small)

bbox_line_small = st_cast(bbox_poly_small, "LINESTRING")

plot(bbox_poly_small, add = TRUE)
#______________________________________________________________________________#
m = midlines_draw(thames_poly)

plot(thames_poly)
plot(m$geometry, add = TRUE)

m2 = midlines_clean(m)
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag])

m2 = m2[m2$removed_flag==0, c(1,2)]
m2 = midlines_clean(m2)
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag])

m2 = m2[m2$removed_flag==0, c(1,2)]
m2 = midlines_clean(m2)
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag])

m2 = m2[m2$removed_flag==0, c(1,2)]
m2 = midlines_clean(m2)
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag])

m2 = m2[m2$removed_flag==0, c(1,2)]
m2 = midlines_clean(m2)
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag])

m2 = midlines_clean(m, n_removed = 5)
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag])



plot(m2$geometry[m2$removed_flag==0])




plot(thames_poly$geometry)
plot(m2$geometry[m2$removed_flag== 0], add = TRUE)

###

m3 = midlines_check(m2, n_removed = 10)
table(m3$removed_flag2)

plot(thames_poly)
plot(m3$geometry, col = c("BLUE", "RED")[m3$removed_flag2], add = TRUE)

##

m4 = m3 %>% subset(m3$removed_flag2==0) %>%
  select(line_id, geometry)

m5 = midlines_clean(m4, n_removed = 3)

plot(m5$geometry, col = c("BLUE", "RED")[m5$removed_flag], add = TRUE)

plot(thames_poly)
plot(m5$geometry[m5$removed_flag== 0], add = TRUE)

m6 = m5[m5$removed_flag==0,]
plot(m6$geometry)

##

m7 = midlines_debit(m6, length = set_units(100,"m"))
plot(m7$geometry)

m7 = midlines_debit(m6, length = set_units(1500,"m"))
plot(m7$geometry)

###

st_bbox(st_buffer(bbox_poly, dist = set_units(-300, "m")))

bbox_small = st_bbox(c(xmin = 538350, ymin = 180750, xmax = 539800, ymax = 181900), crs = 27700)
bbox_poly_small = st_as_sfc(bbox_small)

plot(thames)
plot(bbox_poly_small, add = TRUE)

a = st_intersection(thames, bbox_poly_small)
plot(a$geometry)
###

n = midlines_draw(a)

plot(n$geometry, col = n$line_id, add = TRUE)

n2 = midlines_draw(a, dfMaxLength = set_units(85, "m"))
plot(a)
plot(n2$geometry, col = n2$line_id, add = TRUE)

##

n3 = midlines_clean(n2)
plot(n3$geometry[n3$removed_flag==0])

##

n3 = n3[n3$removed_flag==0,]

n4 = midlines_smooth(n3, width = 5 )

plot(n4$geometry)


####
# END
m = midlines_draw(thames_poly)

plot(thames_poly)
plot(m$geometry, add = TRUE, col = "BLUE")

m2 = midlines_clean(m, n_removed = 1)

plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag], add = TRUE)

plot(m2$geometry[m2$removed_flag==0])



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


