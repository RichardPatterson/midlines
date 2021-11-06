

# x = buffer_pol_union
# max_dist = max_distance_between_pts
# near_buffer_dist = near_buffer_distance

#' Estimates the midline(s) of sf polygon(s)
#'
#' Uses Voronoi polygons to estimate the midlines of one or more sf polygons
#'
#' Taking an sf polygon or feature collection of polygons, the function uses Voronoi polygons to estimate the polygon midlines. Sufficient density of points are required on the polygon boarder facilitate the Voronoi process. Large gaps between points are possible along straight edges, therefore dfMaxLength used to stipulate the maximum distance between points and add points where required. This argument is passed to \code{\link[sf]{st_segmentize}}.
#'
#' The Voronoi process is likely to lead to extraneous lines which do not form part of the intended midline(s). Additional functions \code{\link{midlines_clean}} and \code{\link{midlines_check}} will hopefully help to deal with these.
#'
#' Where there is a region of interest defined by an sf linestring, e.g. of a bounding box, this can be specified to ensure the midlines do not extend beyond this.
#'
#' @param x an sf polygon within which to estimate the midline
#' @param border_line an sf linestring of exterior border of the area of interest
#' @param dfMaxLength maximum distance between points in polygon x used to generate Voronoi polygons. Argument passed to \code{\link[sf]{st_segmentize}}
#'
#' @examples
#' library(sf)
#' poly = st_buffer(st_linestring(
#'   matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) )
#'   ,0.75)
#' plot(poly, col = "GRAY")
#'
#' ml = midlines_draw(poly, dfMaxLength = 1)
#' plot(ml$geometry, add = TRUE)
#'
#' @export
midlines_draw = function(x, border_line = NULL, dfMaxLength = NULL){

  # Input for Voronoi need to be a union of (multi)lines
  line_union = sf::st_union(sf::st_cast(x,"MULTILINESTRING"))

  # Ensure sufficient points on straight sections of polygon if max_dist specified
  if(!(is.null(dfMaxLength))){
  line_union = sf::st_segmentize(line_union, dfMaxLength = dfMaxLength)
  }

  # Draws the Voronoi edges
  voronoi_edges = sf::st_cast(sf::st_sf(sf::st_sfc(sf::st_voronoi(do.call("c", sf::st_geometry(line_union)),bOnlyEdges = TRUE)),crs = sf::st_crs(x)),"LINESTRING")
  colnames(voronoi_edges)[colnames(voronoi_edges) == colnames(voronoi_edges)] = "geometry" #this only works cos there is one column
  sf::st_geometry(voronoi_edges) <- "geometry"

  # Retain only those that remain within polygon
  voronoi_edges = voronoi_edges[unlist(sf::st_contains_properly(x, voronoi_edges)),]
  voronoi_edges$line_id = 1:nrow(voronoi_edges)

  # If a border is specified, to get rid of anything outside of this
  if(!(is.null(border_line))) {
    border_poly = sf::st_sfc(sf::st_polygon(border_line), crs = sf::st_crs(border_line))
    voronoi_edges = sf::st_intersection(voronoi_edges, border_poly) # throws a warning about constant attributes
    }

  voronoi_edges

}




# x = midlines_all
# n_removed=10
# border_line = bbox_as_line
# border_distance = set_units(1,"m")
# i=1

#' @export
midlines_clean = function(x, n_removed=10, border_line = NULL){

  # Identify those edges that intersect with the borderline (if specified)
  if(!(is.null(border_line))) {
    x$border_intersect = as.vector(sf::st_intersects(x$geometry, border_line , sparse = FALSE))
  }

  mid_points = sf::st_cast(x,"POINT")
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

    if(!(is.null(border_line))) {
      trimmed_mid_points$dead_point[trimmed_mid_points$border_intersect==TRUE] = FALSE
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



