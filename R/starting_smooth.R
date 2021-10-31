
smooth = function(dat, width = 3){

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

  smoothed = sf::st_as_sf(sf::st_as_sfc(lapply(1:nrow,s)))

  colnames(smoothed)[colnames(smoothed) == colnames(smoothed)] = "geometry" #this only works cos there is one column
  sf::st_geometry(smoothed) <- "geometry"

  smoothed$line_id = 1:nrow(smoothed)

  sf::st_crs(smoothed) = sf::st_crs(dat)

  return(smoothed)
}

#smooth(linestrings)


###################################
#everything else is the working out
#
#
# j = st_sfc(linestrings$geometry)
# j[[1]][1:10]
#
# #this get all coords
# h = function(x){
#   x[1:length(x)]
# }
# lapply(j,h)
#
# #here just the xs
# h2 = function(x){
#   x[1:(length(x)/2)]
# }
# lapply(j,h2)
# linestrings$xs = lapply(j,h2)
#
# #and the ys
# i2 = function(x){
#   x[(length(x)/2+1):length(x)]
# }
# lapply(j,i2)
# linestrings$ys = lapply(j,i2)
#
#
# rollapply(unlist(linestrings$xs[1]), width = 3, mean, partial = TRUE)
#
#
# n = function(x){
#   rollapply(unlist(x$xs), width = 3, mean, partial = TRUE)
# }
#
#
# n = function(x){
#   rollapply(unlist(x), width = 3, mean, partial = TRUE)
# }
#
# xs = linestrings$xs
# linestrings$smooth_x = lapply(xs,n)
# xs2 = lapply(xs,n)
#
# ys = linestrings$ys
# ys2 = lapply(ys,n)
#
#
# unlist(c(linestrings$smooth_x[1], linestrings$smooth_y[1]))
#
# f = function(x){unlist(c(linestrings$smooth_x[x],linestrings$smooth_y[x]))}
#
# linestrings$smoothed = lapply(1:20, f)
#
# ### the above works albeit its not very elegant
# ##this version is better and below in an apply
#
# l = lengths(linestrings$geometry)
#
# a = rollapply(linestrings$geometry[[1]][1:(l[1]/2)], width = 3, mean )
# b = rollapply(linestrings$geometry[[1]][(l[1]/2+1):l[1]], width = 3, mean )
#
# sx = linestrings$geometry[[1]][1]
# ex = linestrings$geometry[[1]][(l[1]/2)]
# sy = linestrings$geometry[[1]][(l[1]/2+1)]
# ey = linestrings$geometry[[1]][l[1]]
#
# st_linestring(cbind(c(sx,a,ex),c(sy,b,ey)))
#
#
# ###
#
# #try it in an apply
# x = 1
# #l = lengths(linestrings$geometry)
# s = function(x){
#   l = length(linestrings$geometry[[x]])
#
#   a = rollapply(linestrings$geometry[[x]][1:(l/2)], width = 3, mean )
#   b = rollapply(linestrings$geometry[[x]][(l/2+1):l], width = 3, mean )
#
#   sx = linestrings$geometry[[x]][1]
#   ex = linestrings$geometry[[x]][(l/2)]
#   sy = linestrings$geometry[[x]][(l/2+1)]
#   ey = linestrings$geometry[[x]][l]
#
#   st_linestring(cbind(c(sx,a,ex),c(sy,b,ey)))
#
# }
#
# smoothed = st_as_sf(st_as_sfc(lapply(1:20,s)))
#
# plot(smoothed$x[1])
# plot(linestrings$geometry[1])
#
# plot(smoothed$x[3])
# plot(linestrings$geometry[3])
#
# ## can i make it work in a function
#
# smooth = function(dat, width = 3){
#
#   s = function(x){
#     l = length(dat$geometry[[x]])
#
#     a = rollapply(dat$geometry[[x]][1:(l/2)], width = 3, mean )
#     b = rollapply(dat$geometry[[x]][(l/2+1):l], width = 3, mean )
#
#     sx = dat$geometry[[x]][1]
#     ex = dat$geometry[[x]][(l/2)]
#     sy = dat$geometry[[x]][(l/2+1)]
#     ey = dat$geometry[[x]][l]
#
#     st_linestring(cbind(c(sx,a,ex),c(sy,b,ey)))
#
#   }
#
#   smoothed = st_as_sf(st_as_sfc(lapply(1:20,s)))
#
#   return(smoothed)
# }
#
# smooth(linestrings)
