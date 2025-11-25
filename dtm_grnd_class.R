library(lidR)
library(stringr)

#==============================================================
#                       generate dtms 
#==============================================================

tls <- "E:/"

c <- c("c1", "c6")

files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = '3m.las$'
)

c1_files <- str_subset(files, "\\bc1\\b")
c6_files <- str_subset(files, "\\bc6\\b")

initial_c_files <- c(c1_files, c6_files)

file = c6_files[1]

# file = initial_c_files[1]
# las = readLAS(file, filter = '-keep_random_fraction 0.0001')

i = 1
c6_files = c6_files[i:length(c6_files)]

for (file in c6_files) {
  
  message('Processing ', file)
  message(i, ' of ', length(c6_files))
  tictoc::tic()
  las <- readLAS(file)

  # classified_las <- classify_ground(las, algorithm = csf())

  # classified_file_name <- str_replace(file, "\\.las$", "_grndcls.las")
  # 
  # writeLAS(classified_las, classified_file_name)

  # dtm <- rasterize_terrain(classified_las, res = 0.25, algorithm = tin())
  
  dtm <- rasterize_terrain(las, res = 0.25, algorithm = tin())

  dtm_file_name <- str_replace(file, "\\.las$", "_dtm.tif")

  terra::writeRaster(dtm, dtm_file_name, overwrite = TRUE)
  
  las_norm <- las - dtm
  
  htnorm_file_name <- str_replace(file, "\\.las$", "_htnorm.las")
  
  writeLAS(las_norm, htnorm_file_name)
  
  tictoc::toc()
  
  i = i + 1

}


# ==============================================================
#                          ht normalization 
#==============================================================

tls <- "D:/"

all_files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = '\\.(tif|las)$'
)

# files <- str_subset(all_files, "\\bc10\\b*m\\.las|\\bc6_dtms\\b")
files <- str_subset(all_files, "(c1_dtms|c2.*3m\\.las$)")

file_info <- data.frame(
  file = files,
  plot = str_extract(basename(files), "p\\d{1,4}"),
  type = str_extract(files, "\\.(tif|las)$")
)

dtm_files <- subset(file_info, type == ".tif")
las_files <- subset(file_info, type == ".las")
matched <- merge(dtm_files, las_files, by = "plot", suffixes = c(".tif", ".las"))

i = 1

for (i in seq_len(nrow(matched))) {
  las_file <- matched$file.las[i]
  dtm_file <- matched$file.tif[i]
  
  message('Processing ', las_file)
  message(i, ' of ', nrow(matched))
  
  tictoc::tic()
  las <- readLAS(las_file)
  dtm <- terra::rast(dtm_file)
  
  las_norm <- las - dtm
  
  htnorm_file_name <- str_replace(las_file, "\\.las$", "_htnorm.las")

  writeLAS(las_norm, htnorm_file_name)
  tictoc::toc()
  
  i = i + 1
}

las_check = readLAS(htnorm_file_name, filter = '-keep_random_fraction 0.0001')
plot(las_check)

las_check = readLAS("E:/c6/c6_tls_p11_200817_11dot3m_htnorm.las", filter = '-keep_random_fraction 0.0001')
x = lidR::plot(las_check, color = "Classification")
add_dtm3d(x, dtm)
classified_las = las_check

x = plot(decimate_points(las_norm, random(100)))
add_dtm3d(x, dtm)

#### checking

c6_files <- str_subset(all_files, "\\bc6\\b")
las_norm <- str_subset(c6_files, "norm\\.las$")
file_i <- las_norm[1]

for(file_i in las_norm) {
  las <- readLAS(file_i)
}

