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

df$file_name <- as.character(df$file_name)
df$campaign <- as.character(df$campaign)
df$plot <- as.character(df$plot)

las_files <- list.files(
  '/Volumes/Extreme SSD',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

las_files <- list.files(
  '/Volumes/tls',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

i = 8

###################

file_i = las_files[i]
file_i

las = readLAS(file_i, filter = '-keep_random_fraction 0.0001')

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

###############

c15_range_x <- range(las@data$X)
# c1_range_y <- range(las@data$Y)
# c1_range_z <- range(las@data$Z)

# df[66, "clipped"] <- 1

###############

write_csv(df, "plot_check.csv")

###############

# i = 78
# 
# file_i = las_files[i]
# file_i
# las = readLAS(file_i, filter = '-keep_random_fraction 0.000000000000001')
# st_crs(las)
# las = readLAS(file_i)
# st_crs(las) = st_crs(x)
# st_crs(las)
# writeLAS(las, file_i)
# i = i + 1
# 
# x = readLAS(las_files[i = 39], filter = '-keep_random_fraction 0.00000000001')
# st_crs(x)
