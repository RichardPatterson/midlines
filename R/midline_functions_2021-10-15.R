##2021-10-15 change the creation of dead_line in deadends function to use mutate rather than add_count

################################################################################
# 1. midlines_draw uses voronoi polygons to draw the midlines of a polygon

# near_lanes_dist isn't used as I simplified how to id midlines


# dat = buffer_pol_union
# max_dist = max_distance_between_pts
# near_buffer_dist = near_buffer_distance

#' Estimates midlines of sf polygon(s)
#'
#'
#'
#' @export
midlines_draw = function(dat, boarder_line = NULL, max_dist){

  # make linestring and ensure sufficient point density for voronoi
  line_union = sf::st_segmentize(sf::st_union(sf::st_cast(dat,"MULTILINESTRING")), dfMaxLength = max_dist)

  voronoi_edges = sf::st_cast(sf::st_sf(sf::st_sfc(sf::st_voronoi(do.call("c", sf::st_geometry(line_union)),bOnlyEdges = TRUE)),crs = sf::st_crs(dat)),"LINESTRING")
  colnames(voronoi_edges)[colnames(voronoi_edges) == colnames(voronoi_edges)] = "geometry" #this only works cos there is one column
  sf::st_geometry(voronoi_edges) <- "geometry"

  voronoi_edges$line_id = 1:nrow(voronoi_edges)

  # simplified - check the order of these wrt time taken
  # midlines = voronoi_edges[!(sf::st_distance(voronoi_edges, line_union) < near_buffer_dist),]
  # midlines = midlines[!(sf::st_intersects(midlines, line_union, sparse=FALSE)),]
  # midlines = midlines[(sf::st_intersects(midlines, dat, sparse=FALSE)),]

  voronoi_edges = voronoi_edges[unlist(sf::st_contains_properly(dat, voronoi_edges)),]

  if(!(is.null(boarder_line))) {
    boarder_poly = sf::st_sfc(sf::st_polygon(boarder_line), crs = sf::st_crs(boarder_line))
    voronoi_edges = sf::st_intersection(voronoi_edges, boarder_poly)
  }

  voronoi_edges

}




# dat = midlines_all
# n_removed=10
# boarder_line = bbox_as_line
# boarder_distance = set_units(1,"m")
# i=1

#' @export
deadends = function(dat, n_removed=10, boarder_line = NULL, boarder_distance = units::set_units(1,"m")){

  if(!(is.null(boarder_line))) {
    dat$boarder_intersect = as.vector(
      sf::st_is_within_distance(boarder_line, dat$geometry, dist=boarder_distance, sparse=FALSE))
  }

  mid_points = sf::st_cast(dat,"POINT")
  mid_points$point_id = 1:nrow(mid_points)

  removed_mid_points <- data.frame(matrix(ncol = 10, nrow = 0))


  for(i in 1:n_removed) {
    if (i == 1) trimmed_mid_points = mid_points

    #trimmed_mid_points$dead_point = lengths(sf::st_intersects(trimmed_mid_points))==1
    trimmed_mid_points$dead_point = !(
      duplicated(trimmed_mid_points$geometry) |
        duplicated(trimmed_mid_points$geometry, fromLast = TRUE)
    )
    #table(trimmed_mid_points$dead_point)

    if(!(is.null(boarder_line))) {
      trimmed_mid_points$dead_point[trimmed_mid_points$boarder_intersect==TRUE] = FALSE
    }


    ls =trimmed_mid_points$line_id[trimmed_mid_points$dead_point]
    trimmed_mid_points$dead_line = trimmed_mid_points$line_id %in% ls


    trimmed_mid_points$cycle = i

    new_removed_mid_points =trimmed_mid_points[trimmed_mid_points$dead_line == TRUE,]
    removed_mid_points = rbind(removed_mid_points, new_removed_mid_points)

    trimmed_mid_points =trimmed_mid_points[trimmed_mid_points$dead_line == FALSE,]


  }#for loop


    removed_lines = removed_mid_points %>%
        dplyr::group_by(line_id) %>%
        dplyr::summarise(do_union = FALSE) %>%
        sf::st_cast("LINESTRING") %>%
        dplyr::mutate(removed_flag = 1)

    trimmed_lines = trimmed_mid_points %>%
        dplyr::group_by(line_id) %>%
        dplyr::summarise(do_union = FALSE) %>%
        sf::st_cast("LINESTRING") %>%
      dplyr::mutate(removed_flag = 0)

    rbind(removed_lines, trimmed_lines)

  #return = list(removed_lines,trimmed_lines)

  #names(return) = c("deadend_lines", "liveend_lines")
  #return(return)

}

group_id = line_id = geometry = NULL

# # this is the original non-looped version
#   mid_points = st_cast(midlines,"POINT")
#
#   mid_points$dead_point = !(duplicated(mid_points$geometry) | duplicated(mid_points$geometry, fromLast = TRUE))
#   table(mid_points$dead_point)
#
#   mid_points = mid_points %>%
#     group_by(line_id, dead_point) %>%
#     add_count(name = "dead_line")
#
#   trimmed_mid_points = mid_points %>%
#     filter(dead_line == 2)
#
#   removed_mid_points = mid_points %>%
#     filter(dead_line == 1)







#
#
# # 3 grouping deadends - this has been supercedes by functions in group_midlines_new.R
#
# group_deadends = function(dat, line_id = line_id){
#   #l=1
#   removed_lines_2 = dat
#
#   group_index = 1
#   removed_lines_2$group_id = NA
#   #removed_lines_2 = removed_lines_2 %>% arrange(group_id, line_id)
#   data.table::setorder(removed_lines_2, group_id, line_id)
#
#   removed_lines_2$inter = sf::st_intersects(removed_lines_2$geometry, removed_lines_2$geometry)
#
#   removed_lines_2$n_ = 1:nrow(removed_lines_2)
#
#   for(l in 1:nrow(removed_lines_2)) {
#     #print(l)
#     #t = unlist(st_intersects(removed_lines$geometry[l], removed_lines$geometry))
#     #print(t)
#     removed_lines_2$group_id[removed_lines_2$n_ %in% removed_lines_2$inter[[l]]] = group_index
#
#     #removed_lines_2 = removed_lines_2 %>% arrange(group_id)
#     data.table::setorder(removed_lines_2, group_id, na.last = TRUE)
#
#     if(anyNA(removed_lines_2$group_id[l+1])) group_index = group_index +1
#     #print(group_index)
#     #l = l+1
#   }
#
#   removed_multilines_x = removed_lines_2 %>%
#     dplyr::group_by(group_id) %>%
#     dplyr::summarise(do_union = FALSE) %>%
#     sf::st_cast("MULTILINESTRING")
#
#   #list = list(NULL)
#   #list = st_intersects(removed_multilines, removed_multilines)
#
#   removed_multilines_x$n_lines = lengths(removed_multilines_x$geometry)
#
#   removed_multilines_x$length = sf::st_length(removed_multilines_x)
#
#   removed_multilines_x$intersects = sf::st_intersects(removed_multilines_x, removed_multilines_x)
#
#   return  = list(removed_multilines_x,removed_lines_2)
#   names(return) = c("removed_miltilines", "removed_lines")
#
#   return(return)
# }
#


# group_index = 1
# removed_lines$group_id = NA
# removed_lines = removed_lines %>% arrange(group_id, line_id)
#
# for(l in 1:nrow(removed_lines)) {
#   #print(l)
#   t = unlist(st_intersects(removed_lines$geometry[l], removed_lines$geometry))
#   #print(t)
#   removed_lines$group_id[t] = group_index
#
#   removed_lines = removed_lines %>% arrange(group_id)
#
#   if(is.na(removed_lines$group_id[l+1])) group_index = group_index +1
#   #print(group_index)
# }
# rm(l,group_index,t)
#
#
# removed_multilines = removed_lines %>%
#   group_by(group_id) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("MULTILINESTRING")
#
# #list = list(NULL)
# #list = st_intersects(removed_multilines, removed_multilines)
#
# removed_multilines$n_lines = lengths(removed_multilines$geometry)
#
# removed_multilines$length = st_length(removed_multilines)
#
# removed_multilines$intersects = st_intersects(removed_multilines, removed_multilines)


