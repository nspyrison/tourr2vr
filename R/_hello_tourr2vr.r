# Nicholas Spyrison. 10/10/2018.

##### hello_tourr2vr #####
  # Gutted from one of Mile's McBain's GH vignettes:
### Using a community component
#browseURL("https://github.com/MilesMcBain/r2vr/blob/master/vignettes/vr_scatterplot.Rmd")
library("r2vr") # other packages qualified in line.

a_scatterplot <- function(json_data, x, y, z, ...){
  ## js sources for scatterplot
  .scatter_source <- "https://cdn.rawgit.com/zcanter/aframe-scatterplot/master/dist/a-scatterplot.min.js"
  .d3_source <- "https://cdnjs.cloudflare.com/ajax/libs/d3/4.4.1/d3.min.js"
  ## Create in-memory asset for JSON data
  ## A regular a_asset could be used that points to a real file
  ## this is necessary in a vignette to avoid CRAN issues.
  json_file_asset <- a_in_mem_asset(id = "scatterdata",
                                    src = "./scatter_data.json",
                                    .data = json_data)
  a_entity(.tag = "scatterplot",
           src = json_file_asset,
           .js_sources = list(.scatter_source, .d3_source),
           x = x, y = z, z = z, ...)
}

f     <- tourr::flea[, 1:6]
f.pca <- stats::prcomp(f, center = TRUE, scale. = TRUE)
f.pca <- tourr::rescale(tibble::as_tibble(f.pca$x) )
f.pca <- cbind(f.pca, "SpeciesInt" = as.integer(as.factor(tourr::flea[,7])) )
f.pca <- tibble::as_tibble(f.pca)

f.pca_json <- jsonlite::toJSON(f.pca)

my_scene <- a_scene(
  .template = "empty", .children = list(
    a_scatterplot(f.pca_json, x = "PC2", y = "PC3", z = "PC1",
                  xlabel = "PC2", ylabel = "PC3", zlabel = "PC1",
                  val = "SpeciesInt", title = "PC1:3 of std. flea",
                  showFloor = TRUE,
                  ycage = TRUE,
                  pointsize = "10",
                  position = c(4, -4, -2),
                    # position =~ unit / scale, ~[-y, -z, -x]
                  scale = c(3, 4, 4)
    ),
    a_pc_control_camera()
  )
)

my_scene$serve() #Fire started at 127.0.0.1:8080
writeClipboard("127.0.0.1:8080")
browseURL("https://google.com/")

my_scene$stop()
