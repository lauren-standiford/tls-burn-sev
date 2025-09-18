#testing

library("lidR")
library("tidyverse")
library("glue")
library("sf")
library("doParallel")
library("googledrive")

#install.packages("googledrive")

tls_laz_file <- drive_get("tls_samp.laz")
drive_download(tls_laz_file, path = "tls_samp.laz")

laz <- readLAS('/Volumes/Extreme SSD/c1/c1_tls_p1853_201019_11dot3m.las')
print(laz)
x = decimate_points(laz, random(10))
lidR::plot(x)


