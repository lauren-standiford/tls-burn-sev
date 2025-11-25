library(tidyverse)
library("lidR")

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

# las_files <- list.files(
#   '/Volumes/Extreme SSD',
#   full.names = T,
#   recursive = T,
#   pattern = 'las$'
# )

# on macOS
las_files <- list.files(
  '/Volumes/tls',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

# on windows
las_files <- list.files(
  'D:/',
  full.names = T,
  recursive = T,
  #pattern = '\\.(tif|las)$'
  pattern = 'las$'
)

i = 8

################### view las files and add to QC df ###################

file_i = las_files[i]
file_i

las = readLAS("/Volumes/tls/c6/c6_tls_p3_200814_11dot3m_htnorm.las", filter = '-keep_random_fraction 0.0001')

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

############### write csv ###################

write_csv(df, "plot_check.csv")

############################################################
############# check ranges/update df value #################
############################################################

c15_range_x <- range(las@data$X)
c1_range_y <- range(las@data$Y)
range_z <- range(las@data$Z)

df[104, "quality"] <- 1

############################################################
###################### update CRS ##########################
############################################################

i = 1

file_i = las_files[i]
file_i
# las = readLAS(file_i, filter = '-keep_random_fraction 0.000000000000001')
# st_crs(las)
las = readLAS(file_i)
st_crs(las) = st_crs(x)
st_crs(las)
writeLAS(las, file_i)
i = i + 1

###

c2_files <- str_subset(las_files, "\\bc2_clipped\\b")

#x = readLAS(las_files[i = 27], filter = '-keep_random_fraction 0.00000000001')
x = readLAS("D:/c1/c1_tls_p1301_201019_11dot3m.las", filter = '-keep_random_fraction 0.00000000001')
st_crs(x)

las_check(x)


for ((file_i in c2_files) {
  file_i = c2_files[i]
  #file_i
  message('Processing ', file_i)
  message(i, ' of ', length(c2_files))
  tictoc::tic()
  las = readLAS(file_i)
  st_crs(las) = st_crs(x)
  projection(las) = projection(x)
  projection(las) <- "EPSG:26910"
  #st_crs(las)
  file_name <- str_replace(file_i, "\\.las$", "_maybe\\.las")
  writeLAS(las, file_name)
  tictoc::toc()
  
  i = i + 1
}

las = readLAS(file_i, filter = '-keep_random_fraction 0.000000000000001')
st_crs(las)

############################################################
############### plot pre/post together ###################
############################################################

library(rgl)

i = 84
file_i = las_files[i]
file_i

c2_files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'c2.*3m\\.las$'
)
c1_files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'c1.*3m\\.las$'
)

everything = read_csv("D:/plt_veg_type.csv") 
everything = everything %>%
  group_by(plot)

las1 = readLAS("D:/c1/c1_tls_p1301_201019_11dot3m.las", filter = '-keep_random_fraction 0.001')
las3 = readLAS("D:/c2_clipped/c2_1301_fixedmaybe.las", filter = '-keep_random_fraction 0.001')

x = lidR::plot(las1, pal = "red")

#lidR::plot(las2)

lidR::plot(las_check, pal = "blue", add = x)

crs(las1)
st_crs(las2) <- st_crs(las1)

# 1301 reg worked, changes when clip

range_z <- range(las@data$Z)
range_z2 <- range(las2@data$Z)

st_crs(las2) == st_crs(las1)

st_crs(las1)
st_crs(las2)

############################################################
###################### clip radius #########################
############################################################

df = tibble(
  file_name = c(1),
  campaign = c(1),
  plot = c(1),
  radius = c(1),
  x_center = c(1),
  y_center = c(1),
)

tls <- "D:/"
c2_files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'reg2c1\\.las$'
)

i = 1
file_i = c2_files[i]

for (file_i in c2_files) {
  message('Processing ', file_i)
  message(i, ' of ', length(c2_files))
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

# write_csv(df, "D:/c2/c2_centers.csv")
df = read_csv("D:/c2/c2_centers.csv")

i = 1
file_i = c2_files[i]

for (file_i in c2_files) {
  message('Processing ', file_i)
  message(i, ' of ', length(c2_files))
  tictoc::tic()
  
  las = readLAS(df$file_name[i])
  x_center = df$x_center[i]
  y_center = df$y_center[i]
  new_radius = 11.3
  
  las = clip_circle(las = las, xcenter = x_center, ycenter = y_center, radius = new_radius)
  
  new_file_name <- str_replace(file_i, "\\.las$", "_11dot3m\\.las")
  writeLAS(las, new_file_name)
  tictoc::toc()
  
  i = i + 1
}

las2@header[["X offset"]]
las1@header[["X offset"]]

las2@header[["X offset"]] <- las1@header[["X offset"]]
las2@header[["Y offset"]] <- las1@header[["Y offset"]]
las2@header[["Z offset"]] <- las1@header[["Z offset"]]

las2@header[["X scale factor"]] <- las1@header[["X scale factor"]]
las2@header[["Y scale factor"]] <- las1@header[["Y scale factor"]]
las2@header[["Z scale factor"]] <- las1@header[["Z scale factor"]]

writeLAS(las2, "D:/c2_clipped/c2_1301_fixedmaybe.las")
