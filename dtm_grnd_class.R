library(lidR)
library(stringr)

tls <- "E:/"

c <- c("c1", "c6")

files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

c1_files <- str_subset(files, "\\bc1\\b")
c6_files <- str_subset(files, "\\bc6\\b")

initial_c_files <- c(c1_files, c6_files)

# file = initial_c_files[1]
# las = readLAS(file, filter = '-keep_random_fraction 0.0001')

i = 2

for (file in initial_c_files) {
  
  message('Processing ', file)
  message(i, ' of ', length(initial_c_files))
  tictoc::tic()
  las <- readLAS(file)

  classified_las <- classify_ground(las, algorithm = csf())
  tictoc::toc()

  # classified_file_name <- str_replace(file, "\\.las$", "_grndcls.las")
  # 
  # writeLAS(classified_las, classified_file_name)

  dtm <- rasterize_terrain(classified_las, res = 0.25, algorithm = tin())

  dtm_file_name <- str_replace(file, "\\.las$", "_dtm.tif")

  terra::writeRaster(dtm, dtm_file_name, overwrite = TRUE)
  
  i = i + 1

}
