

de_densify = function(x, density = set_units(20,"m")){
  
  ls = st_cast(x,"LINESTRING")
  ls$line_id = 1:nrow(ls)

  de_densified = st_as_sf(st_line_sample(ls, density = density))
  
  de_densified$s = st_startpoint(ls)
  de_densified$e = st_endpoint(ls)
  
  combo = function(x){(st_union(c(x$s,x$x,x$e)))}
  de_densified = st_cast(st_sf(st_as_sfc((apply(de_densified,1,combo))), crs = st_crs(x)),"LINESTRING")
  
  colnames(de_densified)[colnames(de_densified) == colnames(de_densified)] = "geometry" #this only works cos there is one column
  st_geometry(de_densified) <- "geometry"
  
  de_densified$line_id = 1:nrow(de_densified)
  
  (m = matrix(c(lengths(de_densified$geometry),lengths(ls$geometry)), nrow = nrow(de_densified) ))
  colSums(m)
  
  return(de_densified)
    
}







