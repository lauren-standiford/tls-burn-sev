## c2 transformations and clipping ##

library(lidR)
library(stringr)
install.packages(lidRalignment)
install.packages(c('lidR', 'lasR', 'lidRalignment'), repos = 'https://r-lidar.r-universe.dev')
library(lidRalignment)

las_files <- list.files(
  'E:/',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)
c2_files <- str_subset(las_files, "\\bc2\\b")

i = 2
file_i = c2_files[i]
file_i

M <- matrix(c(
  0.999999046326, 0.001372695435, 0.000303271227, 1.318769097328,
  -0.001373481704, 0.999995648861, 0.002608551877, -1.524978637695,
  -0.000299688429, -0.002608966082, 0.999996542931, 32.945327758789,
  0.000000000000, 0.000000000000, 0.000000000000, 1.000000000000
), nrow = 4, byrow = TRUE)

las <- readLAS(file_i)

las_transf <- transform_las(las, M)

las_check = readLAS(las_transf, filter = '-keep_random_fraction 0.0001')
plot(las_check)

transf_file_name <- str_replace(file_i, "\\.las$", "reg2c1.las")
writeLAS(las_transf, transf_file_name)