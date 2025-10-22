library(lidR)
library(stringr)

############ generate dtms ############

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

############ ht normalization ############

tls <- "E:/"

files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = '\\.(tif|las)$'
)

c1_files <- str_subset(files, "\\bc1\\b|\\bc1_dtms\\b")

file_info <- data.frame(
  file = c1_files,
  plot = str_extract(basename(c1_files), "\\d{4}"),
  type = str_extract(c1_files, "\\.(tif|las)$")
)

dtm_files <- subset(file_info, type == ".tif")
las_files <- subset(file_info, type == ".las")
matched <- merge(dtm_files, las_files, by = "plot", suffixes = c(".tif", ".las"))

i = 1

for (i in seq_len(nrow(matched))) {
  las_file <- matched$file.las[i]
  dtm_file <- matched$file.tif[i]
  tictoc::tic()
  las <- readLAS(las_file)
  dtm <- terra::rast(dtm_file)
  tictoc::toc()
  
  tictoc::tic()
  las_norm <- las - dtm
  tictoc::toc()
  # las_check = readLAS(las_norm, filter = '-keep_random_fraction 0.0001')
  # lidR::plot(las_check)
  
  htnorm_file_name <- str_replace(las_file, "\\.las$", "_htnorm.las")
  
  tictoc::tic()
  writeLAS(las_norm, htnorm_file_name)
  tictoc::toc()
  
  i = i + 1
}



############ junk code while i was trying to figure out the ht norm stuff #############

# c6_las <- str_subset(files, "\\bc6\\b")
c1_tif <- str_subset(files, "\\bc1_dtms\\b")
# c6_tif <- str_subset(files, "\\bc6_dtms\\b")

c1_matched <- merge(c1_las, c1_tif, by = "", suffixes = c("tif", "las"))
las_tif_files <- c(c1_las, c6_las, c1_tif, c6_tif)

# file = initial_c_files[1]
# las = readLAS(file, filter = '-keep_random_fraction 0.0001')

###########

dtm_files <- str_subset(files, "\\bc1_dtms\\b")
las_files <- str_subset(files, "\\bc1\\b")

file.info <- data.frame(
  file = 
)

############

i = 2

for (file in las_tif_files) {
  
  message('Processing ', file)
  message(i, ' of ', length(las_tif_files))
  tictoc::tic()
  las <- readLAS(file)
  
  dtm <- terra::rast("")
  tictoc::toc()
  
  # classified_file_name <- str_replace(file, "\\.las$", "_grndcls.las")
  # 
  # writeLAS(classified_las, classified_file_name)
  
  dtm <- rasterize_terrain(classified_las, res = 0.25, algorithm = tin())
  
  htnorm_file_name <- str_replace(file, "\\.las$", "_htnorm.las")
  
  terra::writeRaster(dtm, dtm_file_name, overwrite = TRUE)
  
  i = i + 1
  
}