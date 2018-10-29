# Nicholas Spyrison. 10/10/2018.

#devtools::install_github("milesmcbain/r2vr",    dependencies = TRUE)
#browseURL("https://github.com/MilesMcBain/r2vr")
#aframe is not an R package. #browseURL("https://github.com/aframevr/aframe")

#browseURL("https://github.com/MilesMcBain/r2vr/tree/master/vignettes")
  # Miles McBain's GH vignettes

##### vr_scatterplot #####
  # One of Mile's McBain's GH vignettes.
### Using a community component
#browseURL("https://github.com/MilesMcBain/r2vr/blob/master/vignettes/vr_scatterplot.Rmd")
library("r2vr")
library("jsonlite")
library("ggplot2") # for dimaonds dataset. package also qualified below.

a_scatterplot <- function(json_data, x, y, z, ...){
  ## js sources for scatterplot
  .scatter_source <- "https://cdn.rawgit.com/zcanter/aframe-scatterplot/master/dist/a-scatterplot.min.js"
  .d3_source <- "https://cdnjs.cloudflare.com/ajax/libs/d3/4.4.1/d3.min.js"

  ## Create in-memory asset for JSON data
  ## A regular a_asset could be used that points to a real file
  ## this is necessary in a vignette to avoid CRAN issues.
  json_file_asset <- a_in_mem_asset(id = "scatterdata",
                                    src = "./scatter_data.json",
                                    data = json_data)

  a_entity(tag = "scatterplot",
           src = json_file_asset,
           js_sources = list(.scatter_source, .d3_source),
           x = x,
           y = z,
           z = z, ...)
}

diamonds_json <- jsonlite::toJSON(ggplot2::diamonds)

my_scene <- a_scene(
  template = "empty", children = list(
    a_scatterplot(json_data = diamonds_json,
                  x = "depth", y = "carat", z = "table",
                  xlabel = "depth", ylabel = "carat", zlabel = "table",
                  val = "price", title = "Price of Diamond [$USD]",
                  showFloor = TRUE, ycage = TRUE,
                  pointsize = "10",
                  position = c(0, 0, -2),
                  scale = c(3,3,3)),
    a_pc_control_camera()))

my_scene$serve() #Fire started at 127.0.0.1:8080
writeClipboard("127.0.0.1:8080")
browseURL("https://google.com/")

my_scene$stop()
message("End of part 1: Using a community component. Part 2 starts at ~ line 61.")

### Part 2:
### A Scattleplot from scratch using HTML entities
library("r2vr")
require("purrr")  # also qualified below.
require("tibble") # also qualified below.

a_scatter_ents <- function(x, y, z, colour = rep(1, length(x)), palette_fn = rainbow, sizes = rep(0.1, length(x)), labels, dimensions = c(2,2,2), ...){

  force(sizes)
  x_label <-  deparse(substitute(x))
  y_label <-  deparse(substitute(y))
  z_label <-  deparse(substitute(z))
  legend_label <- deparse(substitute(colour))

  colour_factor <- as.factor(colour)
  ent_colours <- palette_fn(nlevels(colour_factor))[colour_factor]

  range_scale <- function(a) (a - min(a, na.rm=TRUE)) / diff(range(a, na.rm=TRUE))
  x <- range_scale(x)
  y <- range_scale(y)
  z <- range_scale(z)

  positions <- cbind(x,y,z) * dimensions

  entity_data <-
    tibble::tibble(position = purrr::transpose(as.data.frame(positions)),
                   color = ent_colours,
                   radius = sizes,
                   label = labels)

  points <-
    purrr::pmap(entity_data, function(position, color, radius, label){

      id = gsub(" ", "", label)
      point <- a_entity(tag = "sphere", position = unlist(position), color = color,
                        radius = radius,
                        event_set__click =
                          list(`_event`= "click",
                               `_target`= "#labelview",
                               visible = TRUE,
                               value = label),
                        event_set__leave =
                          list(`_event`="mouseleave",
                               `_target`= "#labelview",
                               visible = FALSE,
                               value = label),
                        js_sources = "https://unpkg.com/aframe-event-set-component@^4.0.0/dist/aframe-event-set-component.min.js")
      point
    })

  ## camera entity with cursor
  cursor <- a_entity(tag = "camera", position = c(0,1.6,3),
                     children = list(
                       a_entity(tag = "cursor", position = c(0,0,3)),
                       a_label(id = "labelview",
                               text = "",
                               scale = c(0.4, 0.4, 0.4),
                               position = c(0,-0.4,-1))))
  ## make axis labels
  label_offset <- 0.1 * dimensions

  x_axis_label <- a_label(text = x_label,
                          position = dimensions * c(0.5, 0.1, 0))

  y_axis_label <- a_label(text = y_label,
                          position = dimensions * c(0, 0.5, 0),
                          rotation = c(0, 45, 0))

  z_axis_label <- a_label(text = z_label,
                          position = dimensions * c(0, 0.1, 0.5),
                          rotation = c(0, 90, 0))


  ## make each axis
  x_axis <- a_entity(line = list(start = c(0,0,0),
                                 end = c(dimensions[[1]], 0, 0),
                                 color = "#000000"),
                     children = list(x_axis_label))
  y_axis <- a_entity(line = list(start = c(0,0,0),
                                 end = c(0, dimensions[[2]], 0),
                                 color = "#000000"),
                     children = list(y_axis_label))
  z_axis <- a_entity(line = list(start = c(0,0,0),
                                 end = c(0, 0, dimensions[[3]]),
                                 color = "#000000"),
                     children = list(z_axis_label))
  ## make legend
  legend_levels <- levels(colour_factor)
  legend_colours <- palette_fn(nlevels(colour_factor))
  if(length(legend_levels > 1)){
    box_size = 0.2
    box_spacing = 0.2
    legend_position = c(dimensions[[1]] * 1.1, 0, 0)
    legend_ents <- purrr::imap(legend_levels,
                               function(level, index){
                                 a_entity(tag = "text", value = as.character(level),
                                          position = c(0,
                                                       index * (box_size + box_spacing), 0),
                                          rotation = c(0, 0, 0),
                                          color = "#000000",
                                          align = "right",
                                          anchor = "right",
                                          text = list(xOffset = box_size*2),
                                          geometry= list(primitive = "box",
                                                         width = box_size,
                                                         height = box_size,
                                                         depth = box_size),
                                          material = list(transparent = FALSE,
                                                          color = legend_colours[[index]] )
                                 )
                               })

    ## Legend label
    legend_label <- a_entity(tag = "text", value = legend_label,
                             position = c(0, (box_size + box_spacing) *
                                            (nlevels(colour_factor) + 1), 0),
                             rotation = c(0, 0, 0),
                             color = "#000000",
                             align = "center",
                             geometry= list(primitive = "box",
                                            width = box_size,
                                            height = box_size,
                                            depth = box_size),
                             material = list(transparent = TRUE,
                                             opacity = 0))
    plot_legend <- a_entity(position = legend_position,
                            children = c(legend_ents, legend_label))
  } else {
    plot_legend <- list()
  }

  ## make plot and add points
  plot <- a_entity(position = c(0,0.1,-3),  children = c(x_axis, y_axis,
                                                         z_axis, points,
                                                         plot_legend))

  my_scene <- a_scene(template = "basic",
                      title = "A scattering of cars",
                      children = c(cursor, plot))

  my_scene
}

my_scene <- a_scatter_ents(
  x = mtcars$hp,
  y = mtcars$mpg,
  z = mtcars$wt,
  colour = mtcars$am,
  purrr::partial(rainbow, alpha = NULL), #for RGB -alpha
  labels = row.names(mtcars),
  dimensions = c(3,3,3))

#my_scene$serve("127.0.0.2:8080")
#127.0.0.2:8080 is not a valid IPv4 or IPv6 address.

my_scene$serve() #Fire started at 127.0.0.1:8080
writeClipboard("127.0.0.1:8080")
browseURL("https://google.com/")

my_scene$stop()
message("End of part 2: A Scattleplot from scratch using HTML entities. End of vignette")
