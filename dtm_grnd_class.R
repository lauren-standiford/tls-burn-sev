library(lidR)
library(stringr)
library(rcsf)

tls <- "/Volumes/tls"

c <- c("c1", "c6")

files <- list.files(
  '/Volumes/tls',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

c1_files <- str_subset(files, "\\bc1\\b")
c6_files <- str_subset(files, "\\bc6\\b")

initial_c_files <- c(c1_files, c6_files)

# file = initial_c_files[1]
# las = readLAS(file, filter = '-keep_random_fraction 0.0001')

for (file in initial_c_files) {

  las <- readLAS(file)

  classified_las <- classify_ground(las, algorithm = csf())

  # classified_file_name <- str_replace(file, "\\.las$", "_grndcls.las")
  # 
  # writeLAS(classified_las, classified_file_name)

  dtm <- rasterize_terrain(classified_las, res = 0.25, algorithm = tin())

  dtm_file_name <- str_replace(file, "\\.las$", "_dtm.tif")

  terra::writeRaster(dtm, dtm_file_name, overwrite = TRUE)

}