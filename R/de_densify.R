
#' @export
de_densify = function(x, density = units::set_units(20,"m")){

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

  return(de_densified)

}







