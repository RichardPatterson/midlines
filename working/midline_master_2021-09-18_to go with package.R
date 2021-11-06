
# the calculation of lines thing isn't right. need to check this
# also need care checking, currently the comparison of Rue Camille Claudel shows odd removed edges
    #this might be to do with the number of cycles of removal now that lines simplified. tinkering required.


# install.packages(c("sf","maptiles", "units", "tidyverse", "smoothr", "data.table"))
#
# install.packages("sf")
# install.packages("maptiles")
# install.packages("units")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("smoothr")
# install.packages("data.table")
# install.packages("nngeo")

# library(sf)
# library(maptiles)
# library(units)
# #library(tidyverse)
# library(dplyr)
# library(smoothr)
# library(data.table)
# library(lwgeom)
# library(nngeo)
# library(zoo)

# pkgs = c('sf',
# 'maptiles',
# 'units',
# 'dplyr',
# 'smoothr',
# 'data.table',
# 'lwgeom',
# 'nngeo',
# 'zoo')
#
# library('groundhog')
# #set.groundhog.folder("C:/Users/richa/OneDrive - University of Cambridge/other/groundhog")
# groundhog.library(pkgs, "2021-10-29", tolerate.R.version = '4.0.3')

library(devtools)
library(sf)
library(dplyr)
library(units)
library(nngeo)


load_all()


la_nantes = st_read("C:/Users/richa/OneDrive - University of Cambridge/fr_cyclables/data/clean/la_nantes.gpkg")
osm_cycle_infra = st_read("C:/Users/richa/OneDrive - University of Cambridge/fr_cyclables/data/clean/osm_cycle_infra.gpkg")
#osm_nantes_metro_poly = st_read("C:/Users/richa/OneDrive - University of Cambridge/fr_cyclables/data/clean/osm_nantes_metro_poly.gpkg")

#annoyingly st_read names the geometry column and geom and the code won't like that
colnames(la_nantes)[colnames(la_nantes) == "geom"] = "geometry"
st_geometry(la_nantes) <- "geometry"
colnames(osm_cycle_infra)[colnames(osm_cycle_infra) == "geom"] = "geometry"
st_geometry(osm_cycle_infra) <- "geometry"
#colnames(osm_nantes_metro_poly)[colnames(osm_nantes_metro_poly) == "geom"] = "geometry"
#st_geometry(osm_nantes_metro_poly) <- "geometry"


#Boulevard Charles De Gaulle
bbox = sf::st_bbox(la_nantes %>% filter(commune=="ST-HERBLAIN"))
#bbox = sf::st_bbox(la_nantes %>% filter(commune=="ST-HERBLAIN" & nom=="Boulevard Charles De Gaulle")) # this is likely to be needed for the OSMextracts stuff to crop the dataset.

bbox = sf::st_bbox(la_nantes %>% filter(nom=="Rue Camille Claudel"))
bbox = sf::st_bbox(la_nantes %>% filter(nom=="Boulevard Jules Verne"))
bbox = sf::st_bbox(la_nantes %>% filter(nom=="Boulevard des Anglais"))
bbox = sf::st_bbox(la_nantes %>% filter(nom=="Boulevard Charles De Gaulle"))

bbox_as_line = st_cast(st_as_sfc(bbox),"LINESTRING")
bbox_as_poly = st_sfc(st_polygon(bbox_as_line),crs = st_crs(bbox_as_line))

# maybe something like this to more adequate assess the whole area.
# a = st_make_grid(bbox, n = c(5,5))
# st_bbox(a[[1]])


#cycle lane data for the bbox area
la_dat = la_nantes %>% st_intersection(st_as_sfc(bbox))
osm_dat = osm_cycle_infra %>% st_intersection(st_as_sfc(bbox))

plot(la_dat$geometry)


buffer_width = 7.5                # units don't work with square end style not sure why. it was okay for round ends?
max_distance_between_pts = 10     # ensure sufficient points on buffer to allow Voronoi
#near_lanes_distance = set_units(7.5, metres)
#near_buffer_distance = set_units(5, metres)  # to exclude, higher

#merge cycle lane data
dat_both = rbind(osm_dat[,"geometry"],la_dat[,"geometry"])
buffer_pol_union = st_union(st_buffer(dat_both, buffer_width, endCapStyle = "SQUARE", joinStyle = "BEVEL" )) # buffer width defined above (with units)

plot(dat_both$geometry)
plot(buffer_pol_union, add = TRUE)

plot(buffer_pol_union)
buffer_pol_union = st_remove_holes(buffer_pol_union, max_area = 250)
plot(buffer_pol_union)

midlines_all = midlines_draw(buffer_pol_union, max_dist = max_distance_between_pts, boarder_line = bbox_as_line)


plot(buffer_pol_union)
plot(midlines_all$geometry, add = TRUE, col = midlines_all$line_id)



live_deadends = deadends(midlines_all, n_removed = 10, boarder_line = bbox_as_line, boarder_distance = set_units(1,"m"))

#deadend_lines2 = live_deadends2[live_deadends2$removed_flag==1,]
#liveend_lines2 = live_deadends2[live_deadends2$removed_flag==0,]

#plot(liveend_lines$geometry)
#plot(deadend_lines$geometry, col = "red", add = TRUE)

plot(live_deadends$geometry[live_deadends$removed_flag==0])
plot(live_deadends$geometry[live_deadends$removed_flag==1], col = "red", add = TRUE)

processed_lines = process_lines(live_deadends, length = set_units(30,"m"), n_lines = 10, boarder_line = bbox_as_line, boarder_distance = set_units(1,"m"))

#cleaned_lines = processed_lines$cleaned_lines
#removed_multilines = processed_lines$removed_multilines
#cleaned_lines = processed_lines[processed_lines$removed_flag2==0,]
#removed_multilines = processed_lines[processed_lines$removed_flag2==1,] # these are not multilines now


plot(processed_lines$geometry)
plot(processed_lines$geometry[processed_lines$removed_flag==1], add = TRUE, col="red", lwd=3)

plot(processed_lines$geometry)
plot(processed_lines$geometry[processed_lines$removed_flag==1], add = TRUE, col="red", lwd=3)
plot(processed_lines$geometry[processed_lines$added_flag==1], add = TRUE, col="green", lwd=3)


cleaned_lines = processed_lines %>%
   filter(removed_flag2 == 0) %>%
   select(line_id, geometry)
#group the lines so that small bits especially on the margins can be excluded.

debit_lines = midlines_debit(cleaned_lines, set_units(20,"m"))
plot(debit_lines$geometry, col = debit_lines$line_id)

## pick up here. need to include grouping into the smooth (and de-densify)

smoothed_lines = smooth(debit_lines)

plot(debit_lines$geometry)
plot(smoothed_lines$geometry)
#plot(de_densified$geometry)

plot(debit_lines$geometry[4])
plot(smoothed_lines$geometry[4])
#plot(de_densified$geometry[4])

###########

de_densified = de_densify(smoothed_lines, density = set_units(20, "m"))

plot(de_densified$geometry, col = de_densified$line_id)



rm(buffer_pol_union, processed_lines, osm_nantes_ways,
    deadend_lines, deadend_multilines, cleaned_lines,
   live_deadends, liveend_lines, midlines_all,
   removed_multilines, merged_lines, buffer_width,
   max_distance_between_pts, near_buffer_distance,
   near_lanes_distance, group_deadends, m, linestrings,
   smoothed)

#rm(la_dat, osm_dat, bbox_as_line, bbox, merged_lines )

#############################################
#finished here



#st_write(cleaned_gr_multilines2,
#         "data/clean/st_herblain_midlines.csv",
#         layer_options = "GEOMETRY=AS_WKT",
#         delete_layer = TRUE)

#a = st_read("data/clean/st_herblain_midlines.csv")




st_write(cleaned_gr_multilines2,
         "data/clean/st_herblain_midlines.gpkg",
                 delete_layer = TRUE)


b = st_read("data/clean/st_herblain_midlines.gpkg")





#
#
#
#
# view(removed_multilines)
# removed_multilines = removed_multilines[order(removed_multilines$n_lines), ]
# view(removed_multilines)
#
#
# for (i in 1:nrow(removed_multilines)) {
#
#   cent = st_centroid(removed_multilines$geometry[[i]])
#   x_lim  = c(cent[[1]]-20,cent[[1]]+20)
#   y_lim  = c(cent[[2]]-20,cent[[2]]+20)
#
#   plot(removed_multilines$geometry[[i]], xlim = x_lim, ylim = y_lim, col = "Red", lwd = 3)
#   plot(trimmed_lines$geometry, add = TRUE)
#   plot(CdG_both, add = TRUE, col = "Blue")
#   #plot(ff, add = TRUE)
# }
# rm(x_lim, y_lim, cent, i)
#
#
#
# add_back_groups = removed_multilines$group_id[removed_multilines$n_lines >= 10]
#
# add_back = removed_lines[removed_lines$group_id %in% add_back_groups,c("geometry","line_id")]
#
# trimmed_lines = rbind(trimmed_lines,add_back)
#
# removed_lines = removed_lines[!(removed_lines$group_id %in% add_back_groups),]
#
#
# plot(midlines_x$geometry)
# plot(removed_lines$geometry, add = TRUE, col="red", lwd=3)
# plot(add_back$geometry, add = TRUE, col="green", lwd=3)
