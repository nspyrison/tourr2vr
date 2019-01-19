# Nicholas Spyrison 06/12/2018
# PCA and t-SNE on mtcars, wine, and "1DAY.tsv"

install.packages("rattle.data")
library("rattle.data")

car  <- mtcars
wine <- rattle.data::wine
day1 <- read.table(file = './data/1DAY.tsv', sep = '\t', header = TRUE)

#str(car)
#str(wine)
#str(day1)

### PCA
car_pca  <- stats::prcomp(car, center = TRUE, scale. = TRUE)
wine_pca <- stats::prcomp(wine[, -1], center = TRUE, scale. = TRUE)
# Without Type (of grape)
day1_pca <- stats::prcomp(day1, center = TRUE, scale. = TRUE)

#str(car_pca)

wine_pca$x <- cbind(wine_pca$x, Type = wine[, 1])
# Append Type (of grape) as the last column.

write.csv(car_pca$rotation,  file = "./output/car_pca_rotation.csv")
write.csv(car_pca$x,         file = "./output/car_pca_x.csv")
write.csv(wine_pca$rotation, file = "./output/wine_pca_rotation.csv")
write.csv(wine_pca$x,        file = "./output/wine_pca_x.csv")
write.csv(day1_pca$rotation, file = "./output/day1_pca_rotation.csv")
write.csv(day1_pca$x,        file = "./output/day1_pca_x.csv")


### t-SNE
library(Rtsne)
# t-SNE isn't reproduciable iirc, but
set.seed(07122018)
# for more on perplexity and max_iter (steps) see:
# https://distill.pub/2016/misread-tsne/
# perplexity is ~knn, k nearest neighbors, typically 5-50 -van der Maaten & Hinton
myTSNE <- function (data, dim_out = 2){
  thisTSNE <- Rtsne(data, dims = dim_out, perplexity = 10,
                    verbose=TRUE, max_iter = 500)
  colnames(thisTSNE$Y) <- paste0("tS", 1:dim_out)
  rownames(thisTSNE$Y) <- rownames(data)
  return(thisTSNE)
}

car_tsne  <- myTSNE(car)  # tree: 0sec,    fit: .01sec
wine_tsne <- myTSNE(wine) # tree: .01sec,  fit: .31sec
day1_tsne <- myTSNE(day1) # tree: 33.25sec fit: 2465.42sec

write.csv(car_tsne$Y,  file = "./output/car_tsne.csv")
write.csv(wine_tsne$Y, file = "./output/wine_tsne.csv")
write.csv(day1_tsne$Y, file = "./output/day1_tsne.csv")
