# xx version is trying to return original lines not multilines 
  #2001-10-30 version returns lines i.e. added back original lines not the grouped lines (not multilines)


# 3 grouping deadends

# banks of n_ not existing as a variable name in dataset.


group_lines_a = function(x){
  #l=1
  
  lines = x
  
  lines$group_id = NA
  #setorder(lines, group_id, line_id)
  lines$n_ = 1:nrow(lines)
  
  inter = st_intersects(lines$geometry, lines$geometry)
  group_index = 1
  
  
  for(l in 1:nrow(lines)) {
    
    lines$group_id[lines$n_ %in% 
                               inter[[lines$n_[l]]]] = group_index
    
    setorder(lines, group_id, na.last = TRUE)
    
    if(anyNA(lines$group_id[l+1])) group_index = group_index +1
    #print(group_index)
    l = l+1
  }
  
  multilines = lines %>% 
    group_by(group_id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("MULTILINESTRING")
  
  #multilines$n_lines = lengths(multilines$geometry)
  multilines$n_lines =lengths(lapply(multilines$geometry, unlist))/4
  
  
  multilines$length = st_length(multilines)
  
  #multilines$intersects = st_intersects(multilines, multilines)
  
  
  return(list(multilines,lines))
}



group_lines = function(x) {

grouped_linestring = st_as_sf(st_cast(st_line_merge(st_union(x)), "LINESTRING"))

colnames(grouped_linestring)[colnames(grouped_linestring) == colnames(grouped_linestring)] = "geometry" #this only works cos there is one column
st_geometry(grouped_linestring) <- "geometry"

grouped_multilinestring = group_lines_a(grouped_linestring)[[1]]

# this reokaces the variable from the group_lines_a as applying that to that grouped line does something odd.
grouped_multilinestring$n_lines = (lengths(lapply(grouped_multilinestring$geometry, unlist)) - 2)/ 2

return(grouped_multilinestring)
}




# 
# o = group_lines_a(deadend_lines)[[1]]
# o2 = group_lines(deadend_lines)
# 
# 
# library(rbenchmark)
# install.packages("rbenchmark")  
# 
# benchmark(group_lines_a(deadend_lines),
#           group_lines(deadend_lines))
# 
# benchmark(group_lines_a(cleaned_lines),
#           group_lines(cleaned_lines))




##this function uses group lines and cleans them a bit too.
###
#length = set_units(30,"m")
#n_lines = 10
#tolerance = set_units(1,"m")
#x = live_deadends
###
process_lines = function(x, n_lines, length, tolerance){
  
  #moving this within this cleaning function
  x_multilines = group_lines(x[[1]])
  
  
  # using n_lines as the number of cycles of removing
  add_back_groups1 = x_multilines$group_id[x_multilines$n_lines >= n_lines]
  
  #those greater than 20m
  add_back_groups2 = x_multilines$group_id[x_multilines$length>length]
  
  #the first line finds lines touching boarder and then those removed groups that hit these
  jj = live_deadends[[2]][st_is_within_distance(live_deadends[[2]], bbox_as_line, dist = tolerance, sparse = FALSE),]
  add_back_groups3 = x_multilines$group_id[st_intersects(x_multilines, st_union(jj), sparse = FALSE)] 
  
  add_back_groups = unique(c(add_back_groups1, add_back_groups2, add_back_groups3))
  
  add_back = x_multilines[x_multilines$group_id %in% add_back_groups,"geometry"]
  
  ######
  #i'm going to use st_covers with the multilines to id the lines i need to add back.
  add_back$lines = st_covers(add_back, x[[1]])
  u = unlist(unique(add_back$lines))
  add_back_lines = x[[1]][u,]
  #need to add these back and compare with the original
  ######
  
  #i've lost this so added back don't have an id for now
  #add_back$line_id = (max(x[[2]]$line_id)+1):(max(x[[2]]$line_id)+nrow(add_back))
  
  x[[2]]$added_flag = 0
  #add_back$added_flag = 1
  add_back_lines$added_flag = 1
  
  cleaned_lines = rbind(x[[2]],add_back_lines)
  
  #I no longer keep a removed lines
  removed_multilines = x_multilines[!(x_multilines$group_id %in% add_back_groups),]
  
  return = list(cleaned_lines,removed_multilines)
  names(return) = c("cleaned_lines", "removed_multilines")
  
  return(return)
}


#new = process_lines(live_deadends, length = set_units(20,"m"), n_lines = 10, tolerance = set_units(1,"m") )


# processed_lines = process_lines(live_deadends, length = set_units(20,"m"), n_lines = 10, tolerance = set_units(1,"m"))
# 
# cleaned_lines = processed_lines$cleaned_lines
# removed_multilines = processed_lines$removed_multilines
# 
# plot(midlines_all$geometry)
# plot(deadend_lines$geometry, add = TRUE, col="red", lwd=3)
# plot(cleaned_lines$geometry[cleaned_lines$added_flag==1], add = TRUE, col="green", lwd=3)
# 
# 
# processed_lines_new = process_lines_new(live_deadends, length = set_units(20,"m"), n_lines = 10, tolerance = set_units(1,"m"))
# 
# cleaned_lines_new = processed_lines_new$cleaned_lines
# removed_multilines_new = processed_lines_new$removed_multilines
# 
# plot(midlines_all$geometry)
# plot(deadend_lines$geometry, add = TRUE, col="red", lwd=3)
# plot(cleaned_lines_new$geometry[cleaned_lines_new$added_flag==1], add = TRUE, col="green", lwd=3)



