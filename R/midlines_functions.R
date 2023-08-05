

# x = buffer_pol_union
# max_dist = max_distance_between_pts
# near_buffer_dist = near_buffer_distance

#' Estimates the midline(s) of sf polygon(s)
#'
#' Uses Voronoi tessellation to estimate the midlines of one or more sf polygons
#'
#' Taking an sf polygon or feature collection of polygons, the function uses Voronoi tessellation to estimate the polygon midlines. Sufficient density of points are required on the perimeter of the polygon to facilitate the Voronoi tessellation. Large gaps between points can occur where perimeters have straight lines; the dfMaxLength option is used to stipulate the maximum distance between points and add points where required. This argument is passed to \code{\link[sf:geos_unary]{sf::st_segmentize()}}.
#'
#' The Voronoi tessellation is likely to lead to unwanted side branches which do not form part of the intended midline(s). Additional functions \code{\link{midlines_clean}} and \code{\link{midlines_check}} will hopefully help to deal with these.
#'
#' Where there is a region of interest defined by an sf linestring, e.g. of a bounding box, this can be specified to ensur midlines do not extend beyond this.
#'
#' @param x an sf polygon (or feature collection of polygons) within which to estimate the midline(s).
#' @param border_line an sf linestring forming the exterior border of the area of interest.
#' @param dfMaxLength maximum distance between points in polygon x used to generate Voronoi polygons. Argument passed to \code{\link[sf:geos_unary]{sf::st_segmentize()}}.
#'
#' @examples
#' library(sf)
#' poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
#' plot(poly, col = "GRAY")
#'
#' ml = midlines_draw(poly, dfMaxLength = 1)
#' plot(ml$geometry, add = TRUE)
#'
#' @export
midlines_draw = function(x, border_line = NULL, dfMaxLength = NULL){

  # Check input is a valid sfc polygon
  if(!(any(class(sf::st_geometry(x)) == "sfc_POLYGON"))){
    stop("x should be of sfc_polygon")
  }


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
    border_poly = sf::st_sfc(sf::st_polygon(sf::st_union(border_line)), crs = sf::st_crs(border_line))
    voronoi_edges = sf::st_intersection(voronoi_edges, border_poly) # throws a warning about constant attributes
    }

  voronoi_edges

}





#' Aims to identify extraneous lines and the desired midlines
#'
#' Intended for use following \code{\link{midlines_draw}} which uses Voronoi tessellation to estimate polygon midlines. The Voronoi tessellation results in unwanted side chains, in addition to the intended midlines. This function aims to identify those lines.
#'
#' Unwanted short side branches that are produced during midlines estimation can be identified by specifying the number of line segments to be removed from the end of each line, with the `n_removed` option. Some experimentation might be required to identify the number of line segments that will result in the removal of all unwanted side branches. `midlines-check` does not remove line segments but identifies those at line ends with a binary `removed_flag` variable so they can be examined and deleted by the user as required.
#'
#' If necessary, all lines can then be passed to \code{\link{midlines_check}} to unflag wrongly flagged line ends. Depending on the specific use, it may be best to use this function (\code{\link{midlines_check}}) more than once.
#'
#' The border_lines option prevents lines being flagged if they intersect with a boarder defined by an sf linestring. This might be useful if the border intersects with the extremities of the desired midlines to prevent their being flagged for removal.
#'
#' @param x Simple features collection. Intended for use with the output from \code{\link{midlines_draw}}
#'
#' @param n_removed specified the number of cycles of line removal
#'
#' @param border_line an sf linestring forming the exterior border of the area of interest (see below)
#'
#' @examples
#' library(sf)
#' # 1
#' poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
#' plot(poly, col = "GRAY")
#'
#' ml = midlines_draw(poly, dfMaxLength = 1)
#' plot(ml$geometry, add = TRUE)
#'
#' ml_clean = midlines_clean(ml)
#' plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)
#'
#' #2
#' p1 = st_buffer(st_linestring(matrix(c(0,0,30,0),ncol=2, byrow=TRUE) ),0.75)
#' plot(p1)
#' p2 = st_buffer(st_linestring(matrix(c(9,5,9,0,20,0,18,-4),ncol=2, byrow=TRUE) ),0.75)
#' plot(p2, add = TRUE)
#' p3 = st_union(p1, p2)
#' plot(p3, col = "GRAY")
#'
#' bbox_as_line = st_cast(st_as_sfc(st_bbox
#'   (c(xmin = 0, xmax = 30, ymax = -10, ymin = 10))),"LINESTRING")
#' plot(bbox_as_line, add = TRUE)
#'
#' ml = midlines_draw(p3, dfMaxLength = 1)
#' plot(ml$geometry, add = TRUE)
#'
#' ml_clean = midlines_clean(ml, n_removed = 10)
#' plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)
#'
#' ml_clean2 = midlines_clean(ml, n_removed = 10, border_line = bbox_as_line)
#' plot(p3, col = "GRAY")
#' plot(ml_clean2$geometry, col = ml_clean2$removed_flag, add = TRUE)
#'
#' @export
midlines_clean = function(x, n_removed = 1, border_line = NULL){

  # Does the input have a line_id column and does it contain unique values.
  line_id_present = TRUE

  if(!("line_id" %in% colnames(x))) {
    x$line_id = seq_len(nrow(x))
    line_id_present = FALSE
  } else if  (any(duplicated(x$line_id))){
    stop("If x contains a variable named line_id, it must be unique across observations")
  }
  line_id = x$line_id

  mid_points = sf::st_sf(c(lwgeom::st_startpoint(x),
                       lwgeom::st_endpoint(x)))
  sf::st_geometry(mid_points) = "geometry"

  mid_points$line_id = rep(x$line_id, 2)
  mid_points$point_id = 1:nrow(mid_points)

  # Identify those edges that intersect with the borderline (if specified)
  if(!(is.null(border_line))) {
    mid_points$border_intersect = rep(
      border_intersect = as.vector(sf::st_intersects(x$geometry, border_line , sparse = FALSE)),
      2)
  }


  removed_mid_points = vector("list", n_removed)

  for(i in 1:n_removed) {
    if (i == 1) trimmed_mid_points = mid_points

    trimmed_mid_points$dead_point = !(
      duplicated(trimmed_mid_points$geometry) |
        duplicated(trimmed_mid_points$geometry, fromLast = TRUE)
    )

    if(!(is.null(border_line))) {
      trimmed_mid_points$dead_point[trimmed_mid_points$border_intersect==TRUE] = FALSE
    }

    ls =trimmed_mid_points$line_id[trimmed_mid_points$dead_point]
    trimmed_mid_points$dead_line = trimmed_mid_points$line_id %in% ls

    removed_mid_points[[i]] = trimmed_mid_points[trimmed_mid_points$dead_line == TRUE,]

    trimmed_mid_points =trimmed_mid_points[trimmed_mid_points$dead_line == FALSE,]

  }#for loop

  removed_mid_points = dplyr::bind_rows(removed_mid_points)

  removed_mid_points$removed_flag = factor(1)
  trimmed_mid_points$removed_flag = factor(0)

  trimmed_points = rbind(trimmed_mid_points, removed_mid_points)

  rem_flag = stats::aggregate(removed_flag ~ line_id ,
                       sf::st_drop_geometry(trimmed_points),
                       FUN = unique)

  return = dplyr::inner_join(x, rem_flag, by = "line_id")

  # Remove line_id var if it wasn't present in input (x)
  if(!line_id_present){
    return = subset(return, select = -c(line_id))
  }

  return
}


group_id = line_id = geometry = NULL



