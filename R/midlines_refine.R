# xx version is trying to return original lines not multilines
#2001-10-30 version returns lines i.e. added back original lines not the grouped lines (not multilines)


# 3 grouping deadends

# banks of n_ not existing as a variable name in dataset.

#' Groups lines into multilinestring of clusters of lines which are all contiguous.
#'
#' @param x an sf linestring or collection of linestrings
#'
#' @export
midlines_group = function(x) {

  lines = sf::st_as_sf(sf::st_cast(sf::st_line_merge(sf::st_union(x)), "LINESTRING"))

  colnames(lines)[colnames(lines) == colnames(lines)] = "geometry" #this only works cos there is one column
  sf::st_geometry(lines) <- "geometry"

  lines$group_id = NA
  lines$n_ = 1:nrow(lines)

  inter = sf::st_intersects(lines$geometry, lines$geometry)
  group_index = 1

  for(l in 1:nrow(lines)) {

    lines$group_id[lines$n_ %in%
                     inter[[lines$n_[l]]]] = group_index

    lines = lines[order(lines$group_id, na.last = TRUE),]

    if(anyNA(lines$group_id[l+1])) group_index = group_index +1
    #print(group_index)
    l = l+1
  }

  multilines = lines %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarise(do_union = FALSE) %>%
    sf::st_cast("MULTILINESTRING")

  multilines$n_lines =lengths(lapply(multilines$geometry, unlist))/4
  multilines$length = sf::st_length(multilines)

  #return(list(multilines,lines))
  #}
  grouped_multilinestring = multilines
  ##############


  # this reokaces the variable from the group_lines_a as applying that to that grouped line does something odd.
  grouped_multilinestring$n_lines = (lengths(lapply(grouped_multilinestring$geometry, unlist)) - 2)/ 2

  return(grouped_multilinestring)
}


# a = group_lines(live_deadends[[1]])
#
# a1 = group_lines2(live_deadends[[1]])
#
# all.equal(a,a1)
#
# x = live_deadends[[1]]




##this function uses group lines and cleans them a bit too.
###
# length = set_units(20,"m")
# n_removed = 10
# tolerance = set_units(1,"m")
# border_line = bbox_as_line
# border_distance = units::set_units(1,"m")
# x = live_deadends

#' Checks removed midlines to identify those wrongly removed
#'
#' Polygon midlines estimated by Voronoi tessellation results of extraneous additional lines which need to be removed. Following the flagging of extraneous lines using \code{\link{midlines_clean}}, this function can be used to filter extraneous lines from those wrongly flagged.
#'
#' Intended to take the output of \code{\link{midlines_clean}}, this function will use three criteria to identify lines flagged for removal which might form part of the intended midlines. If n_removed corresponds to the parameter of \code{\link{midlines_clean}} of the same name, it will un-flag lines which formed a contiguous group of >n_removed, i.e. sequential removal of end lines did not result in meeting another branch in the line. Removed based on length is grounded in extraneous lines often being very short and complicated geometries. Contiguous groups of lines longer than the specified length will be un-flagged for removal. The appropriate length will depend on the distance between points on the polygon used to generate the midlines, which in tern can be manipulated with the dfMaxLength option of \code{\link{midlines_draw}}. Trial and error may be required to identify the optimal length. border_line can be used, as with \code{\link{midlines_draw}} and \code{\link{midlines_clean}} to ensure that any lines which formed a contiguous group intersecting the specified border_line will be un-flagged for removal.
#'
#' @param x an sf linestring collection. Intended to be passed from the output of \code{\link{midlines_clean}}
#'
#' @param n_removed a contiguous group of removed lines of this number will be un-flagged for removal
#'
#' @param length a contiguous group of removed lines of more than length will be un-flagged for removal
#'
#' @param border_line an sf linestring forming the exterior border of the area of interest (see below). A contiguous group of removed lines intersecting with this lines will be un-flagged for removal
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

  #moving this within this cleaning function
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
    #touch_border = cleaned[sf::st_is_within_distance(cleaned, border_line, dist = border_distance, sparse = FALSE),]
    touch_border = removed[sf::st_intersects(removed, border_line, sparse = FALSE),]
    add_back_groups3 = x_multilines$group_id[sf::st_intersects(x_multilines, sf::st_union(touch_border), sparse = FALSE)]
  } else {
    add_back_groups3 = NULL
  }

  add_back_groups = unique(c(add_back_groups1, add_back_groups2, add_back_groups3))

  add_back = x_multilines[x_multilines$group_id %in% add_back_groups,"geometry"]

  # identify the lines to add back from the multilines
  add_back_index = lengths(sf::st_covered_by(removed, add_back)) != 0

  add_back_lines = removed[add_back_index,]
  still_removed = removed[!add_back_index,]

  cleaned$added_flag = factor(0)
  still_removed$added_flag = factor(0)
  add_back_lines$added_flag = factor(1)

  cleaned$removed_flag2 = factor(0)
  add_back_lines$removed_flag2 = factor(0)
  still_removed$removed_flag2 = factor(1)

  rbind(cleaned, add_back_lines, still_removed)

}



