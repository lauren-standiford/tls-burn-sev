## c2 transformations and clipping ##

library(lidR)

tls <- "E:/"
tls <- "/Volumes/tls"

las_files <- list.files(
  '/Volumes/tls',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

c2_files <- str_subset(las_files, "\\bc2\\b")



i = 1
file_i = c2_files[i]

las <- readLAS(file_i)
coords <- cbind(las@data$X, las@data$Y, las@data$Z, 1)

transf <- matrix(c(
  0.999999046326, 0.001372695435, 0.000303271227, 1.318769097328,
  -0.001373481704, 0.999995648861, 0.002608551877, -1.524978637695,
  -0.000299688429, -0.002608966082, 0.999996542931, 32.945327758789,
  0.000000000000, 0.000000000000, 0.000000000000, 1.000000000000
), nrow = 4, byrow = TRUE)

coords_t <- t(transf %*% t(coords))

las@data$X <- coords_t[,1]
las@data$Y <- coords_t[,2]
las@data$Z <- coords_t[,3]

transf_file_name <- str_replace(file_i, "\\.las$", "reg2c1.las")
write_csv(las, transf_file_name)