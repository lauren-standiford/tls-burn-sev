library(lidR)
library(stringr)

#==============================================================
#                     ground classification
#==============================================================

files <- list.files("E:/c1/new", full.names = TRUE, pattern = '3m\\.las$')
i = 5
file_i = files[i]

# check ground classified points
for (file_i in files) {
  message('Processing ', file_i)
  message(i, ' of ', length(files))
  las = readLAS(file_i)
  ground_points <- sum(las$Classification == 2, na.rm = TRUE)
  message('Ground classified points: ', ground_points)
   i = i + 1
}

# ground classification
library(RCSF)

file_i <- "E:/c5/no_c1/c5_tls_p1854_reg2c1_200922.las"

for (file_i in files) {
  message('Processing ', file_i)
  message(i, ' of ', length(files))
  tictoc::tic()
  las = readLAS(file_i)
  las <- classify_ground(las, algorithm = csf(), last_returns = TRUE)
  ground_points <- sum(las$Classification == 2, na.rm = TRUE)
  message('Ground classified points: ', ground_points)
  ground_file_name <- str_replace(file_i, "\\.las$", "_ground4.las")
  writeLAS(las, ground_file_name)
  tictoc::toc()
  i = i + 1
}

#==============================================================
#        generate dtms & height normalize initial scans
#==============================================================

files <- list.files("E:/c1/new", full.names = TRUE, pattern = 'ground2\\.las$')
i = 1
file_i = files[i]

file_i <- "E:/c1/c1_tls_p1301_201019_11dot3m.las"

for (file_i in files) {
  message('Processing ', file_i)
  message(i, ' of ', length(files))
  tictoc::tic()
  las <- readLAS(file_i)
  dtm <- rasterize_terrain(las, res = 0.25, algorithm = tin())
  dtm_file_name <- str_replace(file_i, "\\.las$", "_dtm.tif")
  terra::writeRaster(dtm, dtm_file_name, overwrite = TRUE)
  las_norm <- las - dtm
  htnorm_file_name <- str_replace(file_i, "\\.las$", "_htnorm.las")
  writeLAS(las_norm, htnorm_file_name)
  tictoc::toc()
  i = i + 1
}

# =============================================================
#                height normalize subsequent scans
#==============================================================

all_files <- list.files(
  "E:/",
  full.names = T,
  recursive = T,
  pattern = '\\.(tif|las)$'
)

files <- str_subset(all_files, "c1_dtms|c5.*3m\\.las$")

file_info <- data.frame(
  file = files,
  plot = str_extract(basename(files), "p\\d{1,4}"),
  type = str_extract(files, "\\.(tif|las)$")
)

dtm_files <- subset(file_info, type == ".tif")
las_files <- subset(file_info, type == ".las")
matched <- merge(dtm_files, las_files, by = "plot", suffixes = c(".tif", ".las"))

#subset of plots by row
#plot_rows <- c(2, 3, 5, 6, 7, 10, 11)
#matched <- matched[plot_rows, ]

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
