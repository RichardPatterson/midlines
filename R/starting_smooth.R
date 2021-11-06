#' @export
smooth = function(dat, width = 3){

  dat = group_lines(dat)
  dat = dat %>% dplyr::select(geometry)  # stop warning about repeating attributes
  dat = sf::st_cast(dat,"LINESTRING")

  s = function(x){
    l = length(dat$geometry[[x]])

    a = zoo::rollapply(dat$geometry[[x]][1:(l/2)], width = width, mean )
    b = zoo::rollapply(dat$geometry[[x]][(l/2+1):l], width = width, mean )

    sx = dat$geometry[[x]][1]
    ex = dat$geometry[[x]][(l/2)]
    sy = dat$geometry[[x]][(l/2+1)]
    ey = dat$geometry[[x]][l]

    sf::st_linestring(cbind(c(sx,a,ex),c(sy,b,ey)))

  }

  nrow = nrow(dat)

  smoothed = sf::st_as_sf(sf::st_as_sfc(lapply(1:nrow, s)))

  smoothed = sf::st_as_sf(
    sf::st_collection_extract(
      lwgeom::st_split(smoothed$x,
               sf::st_union(
                 sf::st_cast(smoothed, "MULTIPOINT"))), type = "LINESTRING"))

  colnames(smoothed)[colnames(smoothed) == "x"] = "geometry"
  sf::st_geometry(smoothed) <- "geometry"

  smoothed$line_id = 1:nrow(smoothed)

  sf::st_crs(smoothed) = sf::st_crs(dat)

  return(smoothed)
}

#smooth(linestrings)


#' @export
midlines_debit = function(x, length) {

  merged_lines = group_lines(x)

  merged_lines = merged_lines[merged_lines$length > length, ]

  x[lengths(sf::st_covered_by(x, merged_lines)) !=0,]

}
