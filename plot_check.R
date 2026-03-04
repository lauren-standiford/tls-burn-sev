library(tidyverse)
library(lidR)

#==============================================================
#             view las files and add to QC df 
#==============================================================
# df = tibble(
#   file_name = c(1),
#   plot = c(1),
#   campaign = c(1),
#   quality = c(1),
#   registered = c(1),
#   clipped = c(1),
#   height_norm = c(1),
# )

df <- read.csv("https://raw.githubusercontent.com/lauren-standiford/tls-burn-sev/refs/heads/main/plot_check.csv")
campaigns <- read.csv("C:/Users/Admin/Desktop/Campaign-Grid view.csv")

df$file_name <- as.character(df$file_name)
df$campaign <- as.character(df$campaign)
df$plot <- as.character(df$plot)

las_files <- list.files(
  'E:/',
  full.names = T,
  recursive = T,
  #pattern = '\\.(tif|las)$'
  pattern = 'las$'
)

file_i = las_files[i]
file_i

las = readLAS("E:/c6/c6_tls_p3_200814_11dot3m_htnorm.las", filter = '-keep_random_fraction 0.0001')

lidR::plot(las)

df = df %>%
  add_row(
    file_name = file_i,
    campaign = str_extract(file_i, "c\\d+"),
    plot = str_match(file_i, "p(\\d+)")[,2],
    quality = 0,
    registered = 1,
    clipped = 1,
    height_norm = 1,
  )

i = i + 1

write_csv(df, "plot_check.csv")

#==============================================================
#                 check ranges/update df value                   
#==============================================================

c15_range_x <- range(las@data$X)
c1_range_y <- range(las@data$Y)
range_z <- range(las@data$Z)

df[104, "quality"] <- 1

#==============================================================
#                         update CRS
#==============================================================

las_files <- list.files("E:/c2", full.names = TRUE, pattern = "c1\\.las$")

i = 1
file_i = las_files[i]

st_crs(las)

for (file_i in las_files) {
  file_i = las_files[i]
  #file_i
  message('Processing ', file_i)
  message(i, ' of ', length(las_files))
  tictoc::tic()
  las = readLAS(file_i)
  st_crs(las) = st_crs("EPSG:26910")
  #st_crs(las) = st_crs(x)
  #projection(las) <- "EPSG:26910"
  #st_crs(las)
  file_name <- str_replace(file_i, "\\.las$", "_crs\\.las")
  writeLAS(las, file_name)
  tictoc::toc()
  
  i = i + 1
}

######################### check crs status ####################

las_files <- list.files("E:/c5", full.names = TRUE, pattern = "3m\\.las$")
las_files
las_ref <- readLAS("E:/c6/c6_tls_p1_200811_11dot3m.las", filter = '-keep_random_fraction 0.0001')
st_crs(las_ref)
i = 1
file_i = las_files[i]

for (file_i in las_files) {
  file_i = las_files[i]
  message('Processing ', file_i)
  message(i, ' of ', length(las_files))
  las = readLAS(file_i, filter = '-keep_random_fraction 0.000001')
  #message(st_crs(las) == st_crs("EPSG:26910"))
  message(st_crs(las) == st_crs(las_ref))
  
  i = i + 1
}

#==============================================================
#                     plot pre/post together 
#==============================================================

library(rgl)

las1 = readLAS("E:/c1/c1_clipped/c1_tls_p1349_201019_11dot3m.las", filter = '-keep_random_fraction 0.001')
las2 = readLAS("E:/c2/c2_tls_p1349_200327_reg2c1_11dot3m.las", filter = '-keep_random_fraction 0.001')

x = plot(las1, pal = "red")
plot(las2, pal = "blue", add = x)

st_crs(las2) == st_crs(las1)

#==============================================================
#                calculate plot centers & radius
#==============================================================

df = tibble(
  file_name = c(1),
  campaign = c(1),
  plot = c(1),
  radius = c(1),
  x_center = c(1),
  y_center = c(1),
)

df$file_name <- as.character(df$file_name)
df$campaign <- as.character(df$campaign)
df$plot <- as.character(df$plot)

c2_files <- list.files("E:/c2", full.names = TRUE, pattern = 'c1\\.las$')
i = 1
file_i = c1_files[i]

for (file_i in c1_files) {
  message('Processing ', file_i)
  message(i, ' of ', length(c1_files))
  las = readLASheader(file_i)
  x = st_bbox(las)
  r = ((x$xmax - x$xmin)/2)
  
  df = df %>%
    add_row(
      file_name = file_i,
      campaign = str_extract(file_i, "c\\d+"),
      plot = str_match(file_i, "p(\\d+)")[,2],
      radius = r,
      x_center = (x$xmin + r),
      y_center = (x$ymin + r)
    )
  
  i = i + 1
}

df$x_center <- as.numeric(df$x_center)
df$y_center <- as.numeric(df$y_center)
df$radius <- as.numeric(df$radius)

write_csv(df, "E:/c2/c2_names_c1_centers.csv")
df = read_csv("E:/c2/c2_names_c1_centers.csv")
df <- df[-c(1, 2), ]

# create df with file names and plots, then match
c2_df <- tibble(
  file_name = c2_files,
  plot = str_match(c2_files, "p(\\d+)")[,2]
)

df <- df %>%
  left_join(c2_df, by = "plot") %>%
  filter(!is.na(file_name.y))

#==============================================================
#                         clip radius
#==============================================================

df = read_csv("E:/c2/c2_names_c1_centers.csv")

i = 1
file_i = c2_files[i]

for (i in seq_len(nrow(df))) {
  file_i = df$c2_file_name[i]
  
  message('Processing ', file_i)
  message(i, ' of ', nrow(df))
  tictoc::tic()
  
  las = readLAS(file_i)
  x_center = df$x_center[i]
  y_center = df$y_center[i]
  new_radius = df$radius[i]
  las = clip_circle(las = las, xcenter = x_center, ycenter = y_center, radius = new_radius)
  
  new_file_name <- str_replace(file_i, "_crs\\.las$", "_11dot3m\\.las")
  writeLAS(las, new_file_name)
  tictoc::toc()
  
  i = i + 1
}

############## check headers? ################
las2@header[["X offset"]]
las1@header[["X offset"]]

las2@header[["X offset"]] <- las1@header[["X offset"]]
las2@header[["Y offset"]] <- las1@header[["Y offset"]]
las2@header[["Z offset"]] <- las1@header[["Z offset"]]

las2@header[["X scale factor"]] <- las1@header[["X scale factor"]]
las2@header[["Y scale factor"]] <- las1@header[["Y scale factor"]]
las2@header[["Z scale factor"]] <- las1@header[["Z scale factor"]]

writeLAS(las2, "D:/c2_clipped/c2_1301_fixedmaybe.las")
