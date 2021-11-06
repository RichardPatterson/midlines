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
midlines_draw = function(dat, boarder_line = NULL, dfMaxLength = NULL){

  # Input for Voronoi need to be a union of (multi)lines
  line_union = sf::st_union(sf::st_cast(dat,"MULTILINESTRING"))

  # Ensure sufficient points on straight sections of polygon if max_dist specified
  if(!(is.null(dfMaxLength))){
  line_union = sf::st_segmentize(line_union, dfMaxLength = dfMaxLength)
  }

  voronoi_edges = sf::st_cast(sf::st_sf(sf::st_sfc(sf::st_voronoi(do.call("c", sf::st_geometry(line_union)),bOnlyEdges = TRUE)),crs = sf::st_crs(dat)),"LINESTRING")
  colnames(voronoi_edges)[colnames(voronoi_edges) == colnames(voronoi_edges)] = "geometry" #this only works cos there is one column
  sf::st_geometry(voronoi_edges) <- "geometry"

  voronoi_edges$line_id = 1:nrow(voronoi_edges)

  voronoi_edges = voronoi_edges[unlist(sf::st_contains_properly(dat, voronoi_edges)),]

  if(!(is.null(boarder_line))) {
    boarder_poly = sf::st_sfc(sf::st_polygon(boarder_line), crs = sf::st_crs(boarder_line))
    voronoi_edges = sf::st_intersection(voronoi_edges, boarder_poly) # throws a warning about constant attributes
    }

  voronoi_edges

}




# dat = midlines_all
# n_removed=10
# boarder_line = bbox_as_line
# boarder_distance = set_units(1,"m")
# i=1

#' @export
deadends = function(dat, n_removed=10, boarder_line = NULL){

  if(!(is.null(boarder_line))) {

    dat$boarder_intersect = as.vector(sf::st_intersects(dat$geometry, boarder_line , sparse = FALSE))

    #dat$boarder_intersect = as.vector(
    #  sf::st_is_within_distance(boarder_line, dat$geometry, dist=boarder_distance, sparse=FALSE))
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
        dplyr::mutate(removed_flag = factor(1))

    trimmed_lines = trimmed_mid_points %>%
        dplyr::group_by(line_id) %>%
        dplyr::summarise(do_union = FALSE) %>%
        sf::st_cast("LINESTRING") %>%
      dplyr::mutate(removed_flag = factor(0))

    rbind(removed_lines, trimmed_lines)

  #return = list(removed_lines,trimmed_lines)

  #names(return) = c("deadend_lines", "liveend_lines")
  #return(return)

}

group_id = line_id = geometry = NULL



