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
df$file_name <- as.character(df$file_name)
df$campaign <- as.character(df$campaign)
df$plot <- as.character(df$plot)

df <- read.csv("https://raw.githubusercontent.com/lauren-standiford/tls-burn-sev/refs/heads/main/plot_check.csv")

las_files <- list.files(
  '/Volumes/Extreme SSD',
  full.names = T,
  recursive = T,
  pattern = 'las$'
)

i = 120



###################

file_i = las_files[i]

file_i
las = readLAS(file_i, filter = '-keep_random_fraction 0.0001')

lidR::plot(las)
# x = readLAS(las_files[i-1], filter = '-keep_random_fraction 0.00000000001')

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


# range_x3 <- range(las@data$Z)

# df[66, "clipped"] <- 1

###############

write_csv(df, "plot_check_250925.csv")
