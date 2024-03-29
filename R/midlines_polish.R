#' Removes small groups of lines
#'
#' Lines are removed if, when grouped into contiguous groups, the total length of the cluster is less the specified length. The dataset of ungrouped lines is returned.
#'
#' Unlike other functions in this package, `midlines-debit` will remove these line segments, rather than creating a flag variable.
#'
#' @param x a feature collection of sf linestrings. The input is intended to be the output of \code{\link{midlines_clean}}
#' @param length the length below which groups of lines will be removed
#'
#' @export
midlines_debit = function(x, length) {

  merged_lines = midlines_group(x)

  merged_lines = merged_lines[merged_lines$length > length, ]

  x[lengths(sf::st_covered_by(x, merged_lines)) !=0,]

}

#' Uses a rolling mean to smooth midlines
#'
#' Uses \code{zoo{rollapply}} to smooth lines, while keeping nodes where lines meet unchanged. The dataset of ungrouped lines is returned.
#'
#' @param x Feature collection of sf linestrings. The input is intended to be the output of \code{\link{midlines_clean}}
#' @param width option passed to \code{zoo{rollapply}} "specifying the window width (in numbers of observations)" used to calculate the rolling mean
#'
#' @examples
#' library(sf)
#' poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
#' plot(poly, col = "GRAY")
#'
#' ml = midlines_clean(midlines_draw(poly, dfMaxLength = 1))
#' ml = ml[ml$removed_flag==0,]
#' plot(ml$geometry, add = TRUE)
#'
#' ml_smooth = midlines_smooth(ml)
#' plot(poly, col = "GRAY")
#' plot(ml_smooth$geometry, add = TRUE)

#' @export
midlines_smooth = function(x, width = 3){

  dat = midlines_group(x)
  dat = subset(dat, select =  geometry)  # stop warning about repeating attributes
  #dat = sf::st_cast(dat,"LINESTRING")
  dat = sf::st_sf(geometry = sf::st_cast(sf::st_line_merge(sf::st_union(x)), "LINESTRING"))


  s = function(x){
    l = length(dat$geometry[[x]])

    a = zoo::rollmean(dat$geometry[[x]][1:(l/2)], k = width)
    b = zoo::rollmean(dat$geometry[[x]][(l/2+1):l], k = width)

    sx = dat$geometry[[x]][1]
    ex = dat$geometry[[x]][(l/2)]
    sy = dat$geometry[[x]][(l/2+1)]
    ey = dat$geometry[[x]][l]

    sf::st_linestring(cbind(c(sx,a,ex),c(sy,b,ey)))

  }

  smoothed = sf::st_sfc(lapply(seq_len(nrow(dat)), s))

  smoothed = sf::st_sf(geometry =
                          sf::st_collection_extract(
                            lwgeom::st_split(smoothed,
                                             sf::st_union(
                                               sf::st_cast(smoothed, "MULTIPOINT"))), type = "LINESTRING"),
                        crs = sf::st_crs(x))

  smoothed$line_id = seq_len(nrow(smoothed))

  return(smoothed)

  }



#' Removes points on lines between nodes
#'
#' Without modifying the nodes where lines meet, the midlines between these nodes have their number of points reduced (de-densified). This might be useful to reduce the size of the line collection. The intention is that a high density of points can be used to estimate the midlines but this can be reduced if desired. The dataset of ungrouped lines is returned.
#'
#' @param x a feature collection of sf linestrings. The input is intended to be the output of \code{\link{midlines_clean}}
#' @param density is the desired distance between points. This is passed to \code{\link[sf]{st_line_sample}}
#'
#' @examples
#' library(sf)
#' poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
#' plot(poly, col = "GRAY")
#'
#' ml = midlines_clean(midlines_draw(poly, dfMaxLength = 1))
#' ml = ml[ml$removed_flag==0,]
#' plot(ml$geometry, add = TRUE)
#'
#' ml_dedensified = midlines_dedensify(ml, density = 1)
#' plot(poly, col = "GRAY")
#' plot(ml_dedensified$geometry, add = TRUE)

#' @export
midlines_dedensify = function(x, density){

  x = midlines_group(x)
  x = subset(x, select = geometry)
  ls = sf::st_sf(geometry = sf::st_cast(sf::st_line_merge(sf::st_union(x)), "LINESTRING"))

  ls$line_id = 1:nrow(ls)

  de_densified = sf::st_as_sf(sf::st_line_sample(ls, density = density))

  de_densified$s = lwgeom::st_startpoint(ls)
  de_densified$e = lwgeom::st_endpoint(ls)

  #combo = function(x){(sf::st_union(c(x$s,x$x,x$e)))}
  #de_densified = sf::st_cast(sf::st_sf(sf::st_as_sfc((apply(de_densified,1,combo)))),"LINESTRING")

  nrow = nrow(de_densified)
  combo2 = function(x){(sf::st_cast(c(de_densified$s[[x]],de_densified$x[[x]],de_densified$e[[x]]),"LINESTRING"))}
  de_densified = sf::st_sf(sf::st_as_sfc(lapply(1:seq_along(nrow), combo2)))

  colnames(de_densified)[colnames(de_densified) == colnames(de_densified)] = "geometry" #this only works cos there is one column
  sf::st_geometry(de_densified) <- "geometry"

  if (!(is.na(sf::st_crs(x)))) {sf::st_crs(de_densified) = sf::st_crs(x)}

  de_densified$line_id = 1:nrow(de_densified)

  (m = matrix(c(lengths(de_densified$geometry),lengths(ls$geometry)), nrow = nrow(de_densified) ))
  colSums(m)

  de_densified = sf::st_as_sf(
    sf::st_collection_extract(
      lwgeom::st_split(de_densified$geometry,
                       sf::st_union(
                         sf::st_cast(de_densified, "MULTIPOINT"))), type = "LINESTRING"))

  colnames(de_densified)[colnames(de_densified) == colnames(de_densified)] = "geometry" #this only works cos there is one column
  sf::st_geometry(de_densified) <- "geometry"

  de_densified$line_id = 1:nrow(de_densified)

  return(de_densified)

}



