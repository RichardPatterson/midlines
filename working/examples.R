


library(devtools)


load_all()

devtools::check()





#############
# midlines_draw
library(sf)
poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
plot(poly, col = "GRAY")

ml = midlines_draw(poly, dfMaxLength = 1)
plot(ml$geometry, add = TRUE)
################
# midlines_clean
library(sf)
# 1
poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
plot(poly, col = "GRAY")

ml = midlines_draw(poly, dfMaxLength = 1)
plot(ml$geometry, add = TRUE)

ml_clean = midlines_clean(ml)
plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)

# 2
p1 = st_buffer(st_linestring(matrix(c(0,0,30,0),ncol=2, byrow=TRUE) ),0.75)
plot(p1)
p2 = st_buffer(st_linestring(matrix(c(9,5,9,0,20,0,18,-4),ncol=2, byrow=TRUE) ),0.75)
plot(p2, add = TRUE)
p3 = st_union(p1, p2)
plot(p3, col = "GRAY")

bbox_as_line = st_cast(st_as_sfc(st_bbox
                                 (c(xmin = 0, xmax = 30, ymax = -10, ymin = 10))),"LINESTRING")
plot(bbox_as_line, add = TRUE)

ml = midlines_draw(p3, dfMaxLength = 1)
plot(ml$geometry, add = TRUE)

ml_clean = midlines_clean(ml, n_removed = 10)
plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)

ml_clean2 = midlines_clean(ml, n_removed = 10, border_line = bbox_as_line)
plot(p3, col = "GRAY")
plot(ml_clean2$geometry, col = ml_clean2$removed_flag, add = TRUE)

#################
# midlines_check
library(sf)
p1 = st_buffer(st_linestring(matrix(c(0,0,30,0),ncol=2, byrow=TRUE) ),0.75)
plot(p1)
p2 = st_buffer(st_linestring(matrix(c(9,5,9,0,20,0,18,-4),ncol=2, byrow=TRUE) ),0.75)
plot(p2, add = TRUE)
p3 = st_union(p1, p2)
plot(p3, col = "GRAY")

ml = midlines_draw(p3, dfMaxLength = 1)
plot(ml$geometry, add = TRUE)

ml_clean = midlines_clean(ml, n_removed = 15)
plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)

ml_check = midlines_check(ml_clean, n_removed = 10)
plot(p3, col = "GRAY")
plot(ml_check$geometry, col = ml_check$removed_flag2, add = TRUE)

ml_check2 = midlines_check(ml_clean, length = 5)
plot(p3, col = "GRAY")
plot(ml_check2$geometry, col = ml_check2$removed_flag2, add = TRUE)

bbox_as_line = st_cast(st_as_sfc(st_bbox
                                 (c(xmin = 0, xmax = 30, ymax = -10, ymin = 10))),"LINESTRING")
plot(bbox_as_line, add = TRUE)

ml_check3 = midlines_check(ml_clean, border_line = bbox_as_line)
plot(p3, col = "GRAY")
plot(ml_check3$geometry, col = ml_check3$removed_flag2, add = TRUE)

##############
# midlines_smooth
library(sf)
poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
plot(poly, col = "GRAY")

ml = midlines_clean(midlines_draw(poly, dfMaxLength = 1))
ml = ml[ml$removed_flag==0,]
plot(ml$geometry, add = TRUE)

ml_smooth = midlines_smooth(ml)
plot(poly, col = "GRAY")
plot(ml_smooth$geometry, add = TRUE)


############################################
##############
# midlines_dedensify
library(sf)
poly = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
plot(poly, col = "GRAY")

ml = midlines_clean(midlines_draw(poly, dfMaxLength = 1))
ml = ml[ml$removed_flag==0,]
plot(ml$geometry, add = TRUE)

ml_dedensified = midlines_dedensify(ml, density = 1)
plot(poly, col = "GRAY")
plot(ml_dedensified$geometry, add = TRUE)

################








g = st_polygon(
  st_difference(
    (st_buffer(st_point(c(5,5)), 3)),
    (st_buffer(st_point(c(5,5)), 2)) ) )
plot(g, col = "red")


outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(2,2,2,8,8,8,8,2,2,2),ncol=2, byrow=TRUE)
#hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
pts = list(outer, hole1)

(pl1 = st_polygon(pts))

plot(pl1, col = "red")










p5_2 = st_buffer(st_point(c(5,10)),1)


p1 = st_buffer(st_linestring(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE) ),0.75)
p2 = st_buffer(st_linestring(matrix(c(5,5,20,5,20,20,5,20,5,5),ncol=2, byrow=TRUE) ),0.75)
p3 = st_polygon( st_difference(
    (st_buffer(st_point(c(6,13)), 11)),
    (st_buffer(st_point(c(6,13)), 9)) ) )
p4 = st_buffer(st_point(c(4.5,20.5)),1.5)
p5 = st_polygon(list(matrix(c(4,9,4,11,6,11,6,9,4,9),ncol=2, byrow=TRUE)))
p6 = st_union(st_union(st_union(p1, p2), p3), p4)
p7 = st_difference(p6, p5)
plot(p7, col = "GRAY")

ml = midlines_draw(poly7, dfMaxLength = 1)
plot(ml$geometry, col = ml$line_id, add = TRUE)

ml_clean = midlines_clean(ml, n_removed = 10)
plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)

ml_checked = midlines_check(ml_clean, n_removed = 10)
plot(ml_checked$geometry, col=ml_checked$removed_flag2,add = TRUE)

ml = ml_checked[ml_checked$removed_flag2==0,]
plot(poly7, col = "GRAY")
plot(ml$geometry, add = TRUE)

ml_clean = midlines_clean(ml, n_removed = 2)
plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)

ml_checked = midlines_check(ml_clean, n_removed = 2)
plot(ml_checked$geometry, col=ml_checked$removed_flag2,add = TRUE)


ml = ml_checked[ml_checked$removed_flag2==0,]
plot(poly7, col = "GRAY")
plot(ml_checked$geometry, add = TRUE)

ml_clean = midlines_clean(ml, n_removed = 1)
plot(ml_clean$geometry, col = ml_clean$removed_flag, add = TRUE)

################################################################################










