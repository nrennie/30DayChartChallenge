library(tidyverse)
library(rayshader)
library(raster)

# get data
os50_height_dir <- "2022/data/os/"
r <- raster::raster(rgdal::readGDAL(paste0(os50_height_dir, "NN61.asc")))
elmat <- matrix( raster::extract(r, raster::extent(r), buffer = 1000),
                nrow = ncol(r), ncol = nrow(r) )

# find max
which(elmat == max(elmat), arr.ind = TRUE)
ew <- elmat[61, ]
ns <- elmat[, 23]
write.csv(ew, "2022/data/ew.csv")
write.csv(ns, "2022/data/ns.csv")

# figma plot: https://www.figma.com/community/file/1094018694443487961
