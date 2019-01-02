# desktop original: "../zSpace/IATK-master/Assets/Datasets/r_output/flea.csv"
library(tourr)

# f     <- tourr::flea[, 1:6]
# f.pca <- stats::prcomp(f, center = TRUE, scale. = TRUE)
# f.pca <- tourr::rescale(tibble::as_tibble(f.pca$x) )
# f.pca <- cbind(f.pca, "SpeciesInt" = as.integer(as.factor(tourr::flea[,7])) )
#
# write.csv(f.pca, file = "../unity/IATK/Assets/Datasets/r_output/flea_pca.csv")

flea
write.csv(flea, file = "../unity/IATK/Assets/Datasets/r_output/flea_pca.csv")

###

# 31/12/2018
# round 2: do holes tour, export result and reference frame.
library(tourr)

# flea_std   <- tourr::rescale(tourr::flea[,1:6])
# fpath      <- tourr::save_history(flea_std, tourr::guided_tour(tourr::holes))
# last_basis <- matrix(as.numeric(fpath[,, dim(fpath)[3]]), ncol = 2)
# last_frame <- flea_std %*% last_basis
message("But this is only d=2, not d=3; go to PCA.")


f     <- tourr::rescale(tourr::flea[,1:6])
f.pca <- stats::prcomp(f, center = TRUE, scale. = TRUE)
basis <- data.frame("var_name" = rownames(f.pca$rotation), f.pca$rotation)
rownames(basis) <- NULL
dat   <- data.frame(f.pca$x, "SpeciesInt" = as.integer(as.factor(tourr::flea[,7])) )

write.csv(dat,   file = "../../Unity/IATK/Assets/Datasets/r_output/flea_pca.csv")
write.csv(basis, file = "../../Unity/IATK/Assets/Datasets/r_output/flea_basis.csv")

###

# Round 3: Export manip_space. 2/01/2019.

library(spinifex)
?create_manip_space

flea_std <- tourr::rescale(tourr::flea[,1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
manip_sp <- create_manip_space(basis = rb, manip_var = 4)

write.csv(dat,      row.names=FALSE, file = "../../Unity/IATK/Assets/Datasets/r_output/flea_std.csv")
write.csv(manip_sp, row.names=FALSE, file = "../../Unity/IATK/Assets/Datasets/r_output/flea_manip_sp.csv")
