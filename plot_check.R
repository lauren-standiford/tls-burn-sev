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
  'E:/',
  full.names = T,
  recursive = T,
  pattern = '\\.(tif|las)$'
)

i = 8

################### view las files and add to QC df ###################

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

c5_files <- str_subset(las_files, "\\bc5\\b")

x = readLAS(las_files[i = 27], filter = '-keep_random_fraction 0.00000000001')
st_crs(x)


for (fifile_ifor (file_i in c5_files) {
  file_i = c5_files[5]
  file_i
  message('Processing ', file_i)
  message(i, ' of ', length(c5_files))
  tictoc::tic()
  las = readLAS(file_i)
  st_crs(las) = st_crs(x)
  #st_crs(las)
  writeLAS(las, file_i)
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

las1 = readLAS(file_i, filter = '-keep_random_fraction 0.0001')
las2 = readLAS(file_i, filter = '-keep_random_fraction 0.0001')

x = lidR::plot(las1, pal = "red")

lidR::plot(las2, pal = "blue", add = x)

range_z <- range(las@data$Z)
range_z2 <- range(las2@data$Z)

st_crs(las) == st_crs(las2)

st_crs(las)
st_crs(las2)

############################################################
################### missing forest type ####################
############################################################

everything = read_csv("/Volumes/tls/everything.csv")

everything = everything %>%
  # filter(is.na(LF_FOREST)) %>%
  mutate(LF_FOREST = ifelse(is.na(LF_FOREST) & plot %in% c(1, 4, 14), "Hardwood Forest", LF_FOREST),
         LF_FOREST = ifelse(is.na(LF_FOREST) & plot == 17, "Conifer Forest", LF_FOREST))

field_data = readxl::read_xlsx("/Volumes/tls/3DForest_C6SaddleMountain_FieldData.xlsx", sheet = "Tree Data")

field_data = field_data %>%
  filter(Plot %in% c('p1', 'p4', 'p14', 'p17')) %>%
  select(Plot, DBH, Species)

############################################################
##################### missing RBR_NN #######################
############################################################

everything = read_csv("/Volumes/tls/everything.csv")

noRBR = everything %>%
  filter(is.na(RBR_NN)) %>%
  select(plot, campaign, RBR_NN)

RBR_values = read_csv("/Volumes/tls/l83df_burn_severity_veg_type.csv")


