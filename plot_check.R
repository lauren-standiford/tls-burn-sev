library(tidyverse)



df = tibble(
  file_name = c(1),
  plot = c(1)
  campaign = c(1),
  quality = c(1),
  registered = c(1),
  clipped = c(1),
  height_norm = c(1),
)

las_files <- list.files(
  '/Volumes/Extreme SSD',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

i = 1



###################


file_i = las_files[i]

file_i
las = readLAS(file_i, filter = '-keep_random_fraction 0.0001')

lidR::plot(las)

file_i = 1

df = df %>%
  add_row(
    file_name = file_i,
    campaign = 1,
    plot = 2,
    quality = 2,
    registered = 1,
    clipped = 1,
    height_norm = 1,
  )

i = i + 1

###############

write_csv(df, "filename.csv")
