
<!-- README.md is generated from README.Rmd. Please edit that file -->

# midlines

<!-- badges: start -->
<!-- badges: end -->

The goal of `midlines` is to estimate the midline of one or more
polygons, taking inputs in [`sf`](https://github.com/r-spatial/sf)
formats.

`midlines` wraps around several functions from other packages,
predominantly [`sf`](https://github.com/r-spatial/sf). It’s development
has been a learning experience for the author, but hopefully some users
will find it useful. Comments, suggestions and contributions are
welcome.

## Installation

You can install the package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# library(devtools)
devtools::install_github("RichardPatterson/midlines")
```

N.B. to install midlines from GitHub, you will need to install and
attach the devtools package if you have not already done so.

## An example - finding the midline of a river

The package contains an example polygon representing a short section of
the River Thames in central London. This was derived from
[OpenStreetMap](https://www.openstreetmap.org/) data, downloaded using
the [`OSMdata`](https://docs.ropensci.org/osmdata/) package and as such,
copyright is retained by OpenStreetMap contributors.

``` r
# Load libraries
library(midlines)
library(sf)
library(units)

plot(thames)
```

<img src="man/figures/README-thames-1.png" width="100%" />

To estimate the midline of this stretch of the Thames, use the
`midline_draw` function.

``` r
# create a sf collection for the midline
m1 = midlines_draw(thames)

# plot midline with a different colour for each segment (line_id)
plot(m1$geometry, col = m1$line_id, add = TRUE)
```

<img src="man/figures/README-draw-1.png" width="100%" />

We can see the midline has been drawn but that there are many undesired
extraneous lines, especially where the banks of the river are not a
smooth curve. To understand why, we need to know what `midline_draw` has
done. `midlines_draw` used the [`sf`](https://github.com/r-spatial/sf)
function st_voronoi to perform an
[`Voronoi`](https://en.wikipedia.org/wiki/Voronoi_diagram) tessellation
and then removed all line segments not entirely within the polygon.
`midlines` offers a few options to try remove these extraneous lines,
but it is possible you can prevent them by manipulating the location and
density of the points on the original polygon, see sections on
[gaps](zig-zags-and-gaps) and [zig-zagging](#zig-zagging).

## Cleaning the midline

There are several approaches to removing unwanted line segments without
losing any of the desired midline and the specific methods required and
the order of execution will vary according on the nature of the input
polygon and user requirements.

The `midlines_clean` function identifies (flags) potentially extraneous
lines, but does not remove them. This enables the investigation of the
identified lines and complete control over which lines are removed. In
its most basic form `midlines_clean` simply identifies those line
segments which form a dead-end, i.e. which are connected to another line
at only one end.

``` r
# adds a "removed_flag" column to input identifying dead-end line segments
m2 = midlines_clean(m1)

# plot with flagged segments in red and the clean(er) midline in blue
plot(m2$geometry, col = c("BLUE", "RED")[m2$removed_flag], add = TRUE)
```

<img src="man/figures/README-clean1-1.png" width="100%" />

Many of the extraneous lines have been flagged, but not them all. It is
possible to drop the flagged segments and repeat this multiple times to
remove dead-ends one step at a time (and this might be a good idea to
explore your data in the first instance). However, the cycle can be
repeated multiple times using the n_removed option.

``` r
# four cycles of dead-end removal
m2 = midlines_clean(m1, n_removed = 4)

# plot only the clean midline
plot(m2$geometry[m2$removed_flag==0], col = "BLUE")
```

<img src="man/figures/README-clean2-1.png" width="100%" />

This yields a reasonable estimate of the midline of the main channel of
the river, although the side branches (which may or may not be required)
were not consistently handled (see below for more).

## Retaining the ends of the midline

`midlines_clean` removed dead-ends including the extremities of the
desired midline and possibly also those of side branches that might be
of interest. The trade off between removing extraneous line segments and
losing bits of the desired midlines will depend on the specific
requirements, but a few options are available.

### Number of line segments

Using the `n_removed` option to flag multiple cycles of dead-ends allows
the line segments to be grouped with those they intersect with to
identify contiguous groups. The number of line segments in each group
might indicate whether the segments are extraneous. Extraneous side
chains that are substantially shorter than the desired midline can be
removed in this way. For example, if 5 cycles of removal are specified
then the assumption is that groups with 5 or more lines segments are
part of the derired midlines and of interest to the user. To
demonstrate, the example above was satisfactorily cleaned with four
cycles of removal, so if we specify five cycles and use midlines_check
with the n_removed option, it will identify lines of 5 or more segments.

``` r
m2 = midlines_clean(m1, n_removed = 5)
m3 = midlines_check(m2, n_removed = 5)

# plot initially flagged in red and remainder in blue
plot(m3$geometry, col = c("BLUE", "RED")[m3$removed_flag2])

# overlay those initially flagged but identifed as having 5+ segments in green 
plot(m3$geometry[m3$removed_flag==1 & m3$removed_flag2==0], col = "GREEN", add = TRUE)
```

<img src="man/figures/README-n_removed-1.png" width="100%" /> The
`midlines_check` function takes the output from `midlines_clean` and
adds an additional variable unimaginatively named `removed_flag2`. With
both the flag variables, the user can identify those line segments
initially flagged for removal and then unflagged by `midlines_check`.

We can see that now the ends of the main channel midline are restored
but thatthe extraneous lines coming off the midline are still flagged
for removal. This yields potentially unsatisfactory results when the
there is branching at the extremities of the midline, as on the right of
the plot above and also where small bit unconnected to the main channel
are retained (although see [removing unwanted small bits of
midlines](#removing-unwanted-small-bits-of-midlines) for other
approaches to deal with these).

### Length

Groups of segments flagged for removal can also be identified based on
the total length of the group, similar to the use of the number of
segments above. This can be useful especially where the extraneous lines
contain many very small line segments which can be generated with
Voronoi tessellation in small or complex polygons. In the example, a
length of 230 metres results in a similar same configuration as
unflagging those with five or more line segments.

``` r
m2 = midlines_clean(m1, n_removed = 5)
m3 = midlines_check(m2, length = set_units(230,"m"))

# plot initially flagged in red and remainder in blue
plot(m3$geometry, col = c("BLUE", "RED")[m3$removed_flag2])

# overlay those initially flagged but identified as being longer than 230 metres in green 
plot(m3$geometry[m3$removed_flag==1 & m3$removed_flag2==0], col = "GREEN", add = TRUE)
```

<img src="man/figures/README-length-1.png" width="100%" />

Although in this example using length and the number of segments yields
the same results, depending on the shape and size of the polygon the
different approaches might be better used individually or in
combination.

### Border line

Specifying the field, or area, of interest is another way to control
removal of side branches. This can be used to retain line ends that meet
the the border of the area of interest. For example, the Eastern and
Western extremities of our stretch of the river Thames can be identified
so the ends of the desired midline are not removed.

``` r
m2 = midlines_clean(m1, n_removed = 10)

# generate a linestring marking the border
bbox_line = st_cast(st_as_sfc(st_bbox(c(xmin = 535070, ymin = 177800, xmax = 542560, ymax = 181550), crs = 27700)), "LINESTRING")

m3 = midlines_check(m2, border_line = bbox_line)

plot(m3$geometry, col = c("BLUE", "RED")[m3$removed_flag2])
plot(bbox_line, add = TRUE)
```

<img src="man/figures/README-border_line1-1.png" width="100%" />

This works best when the area of interest is smaller than the original
polygon. It is also possible to specify the border_line option with
midlines_draw and midlines_clean, which crops out midline segments
outside the area of interest and safes time by not flagging and
unflagging those lines.

``` r

#bbox_line = st_cast(st_as_sfc(st_bbox(c(xmin = 535070, ymin = 177800, xmax = 542560, ymax = 181550), crs = 27700)), "LINESTRING")

m1 = midlines_draw(thames, border_line = bbox_line)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries

plot(thames)
plot(m1$geometry, col = "BLUE", add = TRUE)
plot(bbox_line, add = TRUE)
```

<img src="man/figures/README-border_line2-1.png" width="100%" />

``` r

m2 = midlines_clean(m1, n_removed = 5, border_line = bbox_line)
m3 = midlines_check(m2, n_removed = 5, border_line = bbox_line)
plot(m3$geometry, col = c("BLUE", "RED")[m3$removed_flag2])
plot(bbox_line, add = TRUE)
```

<img src="man/figures/README-border_line2-2.png" width="100%" />

## Gaps

So far we’ve ignored the side branches north of the the Thames that join
the main channel. Attempts to estimate the midline of the left channel
has been entirely unsuccessful and the right channel midline has a
marked zig-zag pattern. The [zig-zagging](#zig-zagging) section below
covers that issue but here we look more closely at the lack of any
midline in the left of the two side branches.

``` r
# generate a polygon to focus on the left side channel(s)
bbox_poly = st_as_sfc(st_bbox(c(xmin = 536070, ymin = 180700, xmax = 537800, ymax = 181850), crs = 27700))
# a slightly smaller area to use as a border line
bbox_line_s = st_cast(st_as_sfc(st_bbox(c(xmin = 536120, ymin = 180760, xmax = 537750, ymax = 181800), crs = 27700)),"LINESTRING")

plot(thames)
plot(bbox_poly, add = TRUE)
```

<img src="man/figures/README-gaps1-1.png" width="100%" />

``` r
# crop the larger thames polygon to the area we want
side1 = st_intersection(thames, bbox_poly)
plot(side1)


plot(bbox_line_s, add = TRUE)

# estimating midline of the original polygon
m_s1 = midlines_draw(side1, border_line = bbox_line_s)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries
plot(m_s1$geometry, add = TRUE, col = "RED")
```

<img src="man/figures/README-gaps2-1.png" width="100%" />

The lack of lines is caused by the narrowness of the channel, or more
precisely, it is the gaps between points in the polygon perimeter
relative to the width of the polygon in this area. The gap between
points that worked well for the wider main River Thames is insufficient
to allow successful midline estimation in the narrow channel. The
`midlines_draw` function has a `dfMaxLength` option to add additional
points along straight lines at a maximum distance as specified. This
argument is passed along to sf::st_segmentize so see that help file for
more details.

``` r
# some trial and error revealed 8 meters to be the optimal max length between points
plot(side1)
plot(bbox_line_s, add = TRUE)

m_s1 = midlines_draw(side1, dfMaxLength = set_units(8,"m"), border_line = bbox_line_s)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries
#plot(ml2$geometry, add = TRUE, col = "BLUE")

# using the border_line to prevent removal of lines of interest
m_s2 = midlines_clean(m_s1, n_removed = 20, border_line = bbox_line_s)
plot(m_s2$geometry, col = c("BLUE", "RED")[m_s2$removed_flag], add = TRUE)
```

<img src="man/figures/README-gaps3-1.png" width="100%" />

Conversely, if the points on the perimeter are too dense and result in a
very complicated tessellation then sf::st_line_sample might also be
useful.

## Zig-zagging

A zig-zagging midline results from the offset between points on either
side of the polygon when the Voronoi tessellation is done. To illustrate
this, there are a series of midlines drawn with very simple oblong
polygons.

``` r
mat1 = matrix(c(0,0,2,0,4,0,6,0,8,0,10,0,12,0,12,2,11,2,9,2,7,2,5,2,3,2,1,2,0,2,0,0),ncol=2, byrow=TRUE) 
pts1 = st_multipoint(mat1)
pol1 = st_polygon(list(mat1))
ml1 = midlines_draw(pol1)

plot(pol1)
plot(pts1, add = TRUE)
plot(ml1$geometry, col = ml1$line_id, add = TRUE)
```

<img src="man/figures/README-zig-zag1-1.png" width="100%" /> When the
points on the polygon perimeter are offset, this results in the zig-zag
pattern. One way to resolve this is to ensure that points are directly
opposite one another on the polygon.

``` r
mat2 = matrix(c(0,0,2,0,4,0,6,0,8,0,10,0,12,0,12,2,12,2,10,2,8,2,6,2,4,2,2,2,0,2,0,0),ncol=2, byrow=TRUE) 
pts2 = st_multipoint(mat2)
pol2 = st_polygon(list(mat2))
ml2 = midlines_draw(pol2)

plot(pol2)
plot(pts2, add = TRUE)
plot(ml2$geometry, col = ml2$line_id, add = TRUE)
```

<img src="man/figures/README-zig-zag2-1.png" width="100%" /> However,
its not always possible to control the points that make up the polygons
of interest. Another option is to reduce the gap between the points so
even though they are offset the zig-zag is less pronounced.

``` r
mat3 = matrix(c(0,0,1,0,2,0,3,0,4,0,5,0,6,0,7,0,8,0,9,0,10,0,11,0,12,0,
                12,2,11.5,2,10.5,2,9.5,2,8.5,2,7.5,2,6.5,2,5.5,2,4.5,2,3.5,2,2.5,2,1.5,2,0.5,2,0,2,0,0),ncol=2, byrow=TRUE) 
pts3 = st_multipoint(mat3)
pol3 = st_polygon(list(mat3))

ml3 = midlines_draw(pol3)

plot(pol3)
plot(pts3, add = TRUE)
plot(ml3$geometry, col = ml3$line_id, add = TRUE)
```

<img src="man/figures/README-zig-zag3-1.png" width="100%" />

Returning to the river example and looking more closely at the zig-zag
midline of the right side channel.

``` r
# generate a polygon to focus on the right side channel(s)
bbox_poly = st_as_sfc(st_bbox(c(xmin = 538700, ymin = 180750, xmax = 539800, ymax = 181850), crs = 27700))
# a slightly smaller area to use as a border line
bbox_line_s = st_cast(st_as_sfc(st_bbox(c(xmin = 538750, ymin = 180800, xmax = 539750, ymax = 181800), crs = 27700)),"LINESTRING")

plot(thames)
plot(bbox_poly, add = TRUE)
```

<img src="man/figures/README-zig-zag4-1.png" width="100%" />

``` r

# crop the larger thames polygon to the area we want
side2 = st_intersection(thames,bbox_poly)
plot(side2)


plot(bbox_line_s, add = TRUE)

# estimating midline of the original polygon
m_s1 = midlines_draw(side2, border_line = bbox_line_s)
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries
plot(ml1$geometry, add = TRUE, col = "RED")
```

<img src="man/figures/README-zig-zag4-2.png" width="100%" />

One option would be to use the dfMaxLength option with midlines_draw
like we did on the left side channel.

``` r
plot(side2)
m_s1a = midlines_draw(side2, border_line = bbox_line_s, dfMaxLength = set_units(8,"m"))
#> Warning: attribute variables are assumed to be spatially constant throughout all
#> geometries

# clean this to remove extraneous lines
m_s2a = midlines_clean(m_s1a, border_line = bbox_line_s, n_removed = 5)
plot(m_s2a$geometry[m_s2a$removed_flag==0], add = TRUE, col = "RED")
```

<img src="man/figures/README-smooth-1.png" width="100%" />

``` r
#plot(ml2$geometry, add = TRUE, col = "RED")
```

An alternative is to smooth the midline using a rolling average -
basically a wrapper round the rollapply function in the excellent
[Zoo](https://cran.r-project.org/web/packages/zoo/index.html) package.

``` r
# smooth with a rolling average 
m_s2b = midlines_smooth(m_s1)

plot(side2)
plot(m_s2b$geometry, add = TRUE, col = "BLUE")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

`midlines_smooth` will smooth lines in a network of midlines but it does
not move the point where two or more lines join. The `width` option can
be specified for `midlines_smooth`, this is passed to rollapply so see
documentation there for more details. Beware that the wider the window
the greater tenancy to cut the corners and therefore it becomes less
like a midline.

## And finally

### Removing unwanted small bits of midlines

Sometime it might be useful to remove small bits of midline that are not
connected the to main midline. For example, in our earlier example

``` r
plot(m2$geometry)
```

<img src="man/figures/README-debit-1.png" width="100%" />

``` r

# remove the short bits of line
m2 = midlines_debit(m2, length = set_units(500, "m") )

plot(m2$geometry)
```

<img src="man/figures/README-debit-2.png" width="100%" />

### De-densifying midlines

In addition to manipulating the density of points on the perimeter of
the initial polygon, it is sometimes desirable to manipulate the density
of points the form the midline. This results in fewer longer line
segments, rather than many shorter line segments. This can be done using
`midlines_dedensify` either before or after cleaning. Like with the
`midlines_smooth` function, the points where lines join are not affected
but between line joins the number of points can be reduced. This
function calls sf::st_line_sample so see this for the relevant options.

## Voronoi polygons

The estimation of midlines is done using
[`Voronoi`](https://en.wikipedia.org/wiki/Voronoi_diagram) tessellation
via the sf::st_voronoi function. Some understanding of this process can
help to understand how the attributes of the polygon will influence it’s
estimated midline. For a given set of points, Voronoi tessellation
identifies the area around each point, i.e., that are closer to that
point than any other. Below is an demonstration using the points in the
perimeter of the river Thames polygon as the starting point.

<img src="man/figures/README-voronoi-1.png" width="100%" />

## Alternatives

- The very comprehensive R package
  [`cmgo`](https://github.com/AntoniusGolly/cmgo) does many of the same
  things as this package and much more. If your aim to to estimate the
  midline of a river, you should probably start with
  [`cmgo`](https://github.com/AntoniusGolly/cmgo)
- [centerline](https://centerline.readthedocs.io/en/latest/) is a Python
  library with a similar aim
