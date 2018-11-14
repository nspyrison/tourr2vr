# Nicholas Spyrison. 10/10/2018.
# some functions for tibble -> json -> vr scatterplot.
# source("./R/points2vr.r")
# Also see Miles McBain's GH vignette:
#browseURL("https://github.com/MilesMcBain/r2vr/blob/master/vignettes/vr_scatterplot.Rmd")

###TODO: something is wrong, json doesn't have the same str as tables.

library("r2vr")
library("jsonlite")

a_scatterplot <- function(json_data, x, y, z, ...){
  ## js sources for scatterplot
  .scatter_source <- "https://cdn.rawgit.com/zcanter/aframe-scatterplot/master/dist/a-scatterplot.min.js"
  .d3_source <- "https://cdnjs.cloudflare.com/ajax/libs/d3/4.4.1/d3.min.js"

  ## Create in-memory asset for JSON data
  ## A regular a_asset could be used that points to a real file
  ## this is necessary in a vignette to avoid CRAN issues.
  json_file_asset <- r2vr::a_in_mem_asset(id = "scatterdata",
                                          src = "./scatter_data.json",
                                          data = json_data)

  return(r2vr::a_entity(tag = "scatterplot",
                        src = json_file_asset,
                        js_sources = list(.scatter_source, .d3_source),
                        x = x,
                        y = z,
                        z = z, ...))
}

# data = flea; rescale = TRUE; cat_col = 7
as_JSON <- function(data,
                    pca = FALSE,
                    rescale = TRUE,
                    cat_col = NULL) { # categorical column number
  if (!is.null(cat_col)) {
    cat  <- data[,  cat_col]
    cat_name <- colnames(data)[cat_col]
    data <- data[, -cat_col]
  }
  if (pca) {
    pca_data <- stats::prcomp(data, center = TRUE, scale. = TRUE)
    data <- pca_data$x
  }
  if (rescale) data <- tourr::rescale(data)
  if (!is.null(cat_col)) {
    data <- cbind(data, cat_name = as.factor(cat) )
  }
  json_data <- jsonlite::toJSON(data)
  return(json_data)
}

# data=flea;pca=T;rescale=T;cat_col=7;x=1;y=2;z=3;title="flea PCA"
vr_point <- function(data,
                     pca = FALSE,
                     rescale = TRUE,
                     cat_col = NULL,
                     x = 1,
                     y = 2,
                     z = 3,
                     title = "") {
  json_data <-
    as_JSON(data = data, pca = pca, rescale = rescale, cat_col = cat_col)
  if(pca) {
    x_name   <- paste0("PC",x)
    y_name   <- paste0("PC",y)
    z_name   <- paste0("PC",z)
    cat_name <- colnames(data)[cat_col]
  } else {
    x_name   <- colnames(data)[x]
    y_name   <- colnames(data)[y]
    z_name   <- colnames(data)[z]
    cat_name <- colnames(data)[cat_col]
  }


  this_scene <- r2vr::a_scene(
    template = "empty",
    children = list(
      a_scatterplot(json_data = json_data,
                    x = x_name,
                    y = y_name,
                    z = z_name,
                    val = cat_name,
                    xlabel = x_name,
                    ylabel = y_name,
                    zlabel = z_name,
                    showFloor = TRUE,
                    ycage = TRUE,
                    title = title,
                    pointsize = "10",
                    position = c(4, -4, -2),
                    # position =~ unit / scale, ~[-y, -z, -x]
                    scale = c(3, 4, 4)
      ), r2vr::a_pc_control_camera()
    )
  )
  this_scene$serve() #Fire started at 127.0.0.1:8080
  browseURL("http://127.0.0.1:8080")
  #this_scene$stop()
  return(this_scene)
}

##Example:
# f_json <- as_JSON(flea, cat_col = 7)
# f_scene <- vr_point(f_json, cat_col = 7)
# f_scene$stop()
