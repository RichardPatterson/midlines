#' @export
midlines_debit = function(x, length) {

  merged_lines = midlines_group(x)

  merged_lines = merged_lines[merged_lines$length > length, ]

  x[lengths(sf::st_covered_by(x, merged_lines)) !=0,]

}


#' @export
midlines_smooth = function(x, width = 3){

  dat = midlines_group(x)
  dat = x %>% dplyr::select(geometry)  # stop warning about repeating attributes
  dat = sf::st_cast(x,"LINESTRING")

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
midlines_dedensify = function(x, density = units::set_units(20,"m")){

  x = midlines_group(x)
  x = x %>% dplyr::select(geometry)
  ls = sf::st_cast(x,"LINESTRING")
  ls$line_id = 1:nrow(ls)

  de_densified = sf::st_as_sf(sf::st_line_sample(ls, density = density))

  de_densified$s = lwgeom::st_startpoint(ls)
  de_densified$e = lwgeom::st_endpoint(ls)

  combo = function(x){(sf::st_union(c(x$s,x$x,x$e)))}
  de_densified = sf::st_cast(sf::st_sf(sf::st_as_sfc((apply(de_densified,1,combo))), crs = sf::st_crs(x)),"LINESTRING")

  colnames(de_densified)[colnames(de_densified) == colnames(de_densified)] = "geometry" #this only works cos there is one column
  sf::st_geometry(de_densified) <- "geometry"

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



