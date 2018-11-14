source("./R/points2vr.r") # loads jsonlite, r2vr, makes 3 funcs
# NOT WORKING; see TODO near the top of "./R/points2vr.r"

#data=flea;pca=T;rescale=T;cat_col=7;x=1;y=2;z=3;title="flea PCA"
f_scene <- vr_point(data = flea,
                    pca = T,
                    rescale = T,
                    cat_col = 7,
                    x = 1,
                    y = 2,
                    z = 3,
                    title = "flea PCA")

#f_scene$stop()
