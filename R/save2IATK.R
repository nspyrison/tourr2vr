library(tourr)

f     <- tourr::flea[, 1:6]
f.pca <- stats::prcomp(f, center = TRUE, scale. = TRUE)
f.pca <- tourr::rescale(tibble::as_tibble(f.pca$x) )
f.pca <- cbind(f.pca, "SpeciesInt" = as.integer(as.factor(tourr::flea[,7])) )

write.csv(f.pca, file = "../zSpace/IATK-master/Assets/Datasets/r_output/flea_pca.csv")

flea
write.csv(flea, file = "../zSpace/IATK-master/Assets/Datasets/r_output/flea.csv")
