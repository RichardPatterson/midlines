# An internal function
# Largely replicates the answer here: https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r

#' Groups line segments into contiguous groups, returns these as multilinestrings.
#'
#' @param x an sf linestring or collection of linestrings
#'
#' @export
midlines_group = function(x) {

  touches = sf::st_touches(x)
  graph = igraph::graph_from_adj_list(touches)

  groups <- igraph::components(graph)$membership

  grouped = stats::aggregate(x, by = list(group_id = groups), FUN = unique)

  grouped$n_lines = lengths(grouped$line_id)
  grouped$length = sf::st_length(grouped)

  #grouped = subset(grouped, select = -c(removed_flag))
  grouped = grouped[, names(grouped) != "removed_flag"]

  return(grouped)
}

# This seems to be okay. It returns a df rather than a tibble, but that's fine because that's what it it passed.
# d = midlines_group(removed)
# e = midlines::midlines_group(removed)
# d
# e
# class(d)
# class(e)
# all.equal(d,e)
# class(removed)



#' Checks removed midlines to identify wrongly flagged segments
#'
#' Polygon midlines estimated by Voronoi tessellation can result in unwanted side branches that need to be removed. Following the flagging of potentially unwanted line segments using \code{\link{midlines_clean}}, this function can be used to filter unwanted line segments from those wrongly flagged. In reality it creates another flag variable (flagged2!), which duplicates the original flagged variable but unflags those deemed to be wrongly flagged.
#'
#' This function is intended to take the output of \code{\link{midlines_clean}}. The unwanted side branches are usually short relative to the desired midlines and so longer groups of line segments are identified as being wrongly flagged. The threshold above which lines are unflagged can be specified as a number of line segments (with the `n_removed` option) or a length in [Units](https://r-quantities.github.io/units/) (`length` option).
#'
#' The `border_line` option can be used along with the option of the same name in \code{\link{midlines_draw}} and \code{\link{midlines_clean}} if an sf linestring specifying an area of interest is available. This ensures flagged line segments which comprise part of a contiguous group intersecting the specified border_line will be un-flagged.
#'
#' @param x an sf linestring collection. Intended to be passed from the output of \code{\link{midlines_clean}}
#'
#' @param n_removed a contiguous group of n_removed or more line segments will be un-flagged
#'
#' @param length a contiguous group of line segment of greater than specified length will be un-flagged
#'
#' @param border_line an sf linestring forming the exterior border of the area of interest (see below). A contiguous group of removed lines intersecting with this line will be un-flagged
#'
#' @examples
#' library(sf)
#' p1 = st_buffer(st_linestring(matrix(c(0,0,30,0),ncol=2, byrow=TRUE) ),0.75)
#' plot(p1)
#' p2 = st_buffer(st_linestring(matrix(c(9,5,9,0,20,0,18,-4),ncol=2, byrow=TRUE) ),0.75)
#' plot(p2, add = TRUE)
#' p3 = st_union(p1, p2)
#' plot(p3, col = "GRAY")
#'
#' ml = midlines_draw(p3, dfMaxLength = 1)
#' plot(ml$geometry, add = TRUE)
#'
#' ml_clean = midlines_clean(ml, n_removed = 15)
#' plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)
#'
#' ml_check = midlines_check(ml_clean, n_removed = 10)
#' plot(p3, col = "GRAY")
#' plot(ml_check$geometry, col = ml_check$removed_flag2, add = TRUE)
#'
#' ml_check2 = midlines_check(ml_clean, length = 5)
#' plot(p3, col = "GRAY")
#' plot(ml_check2$geometry, col = ml_check2$removed_flag2, add = TRUE)
#'
#' bbox_as_line = st_cast(st_as_sfc(st_bbox
#'   (c(xmin = 0, xmax = 30, ymax = -10, ymin = 10))),"LINESTRING")
#' plot(bbox_as_line, add = TRUE)
#'
#' ml_check3 = midlines_check(ml_clean, border_line = bbox_as_line)
#' plot(p3, col = "GRAY")
#' plot(ml_check3$geometry, col = ml_check3$removed_flag2, add = TRUE)
#'
#' @export
midlines_check = function(x, n_removed = NULL, length = NULL, border_line = NULL){

  removed = x[x$removed_flag==1,]
  cleaned = x[x$removed_flag==0,]

  x_multilines = midlines_group(removed)

  # using n_lines as the number of cycles of removing
  if(!(is.null(n_removed))){
    add_back_groups1 = x_multilines$group_id[x_multilines$n_lines >= n_removed]
  } else {
    add_back_groups1 = NULL
  }

  # those greater than length stipulated
  if(!(is.null(length))){
    add_back_groups2 = x_multilines$group_id[x_multilines$length>length]
  } else {
    add_back_groups2 = NULL
  }

  #the first line finds lines touching border and then those removed groups that hit these
  if(!(is.null(border_line))) {

    touch_border = removed[sf::st_intersects(removed, border_line, sparse = FALSE),]
    add_back_groups3 = x_multilines$group_id[sf::st_intersects(x_multilines, sf::st_union(touch_border), sparse = FALSE)]
  } else {
    add_back_groups3 = NULL
  }

  add_back_groups = unique(c(add_back_groups1, add_back_groups2, add_back_groups3))

  add_back_line_ids = unlist(x_multilines$line_id[x_multilines$group_id %in% add_back_groups])

  x$removed_flag2 = x$removed_flag
  x$removed_flag2[x$line_id %in% add_back_line_ids] = 0

  x$added_flag = factor(as.integer(x$removed_flag != x$removed_flag2)) # should probably remove this as its unnecessary

  return(x)
}


