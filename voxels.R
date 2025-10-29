library(lidR)
library(tidyverse)

tls <- "E:/"

all_htnorm <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'htnorm\\.las$'
)

c6_files <- str_subset(all_htnorm, "\\bc6\\b")
c10_files <- str_subset(all_htnorm, "\\bc10\\b")

las <- readLAS("E:/c6/c6_tls_p1_200811_11dot3m_htnorm.las")
voxels <- voxelize_points(las, res = 0.1)
plot(voxels)

vox_met <- voxel_metrics(las, ~list(N = length(Z)), res = 0.5)
plot(vox_met, color="N", pal = heat.colors, size = 0.5, bg = "white", voxel = TRUE)

vox_filtered <- vox_met[vox_met$N >= 200 & vox_met$N <= 4000, ]
plot(vox_filtered, color = "N", pal = heat.colors, size = 0.5, bg = "white", voxel = TRUE)

las_check = readLAS("E:/c6/c6_tls_p1_200811_11dot3m_htnorm.las", filter = '-keep_random_fraction 0.0001')
plot(las_check)

###

las2 <- readLAS("E:/c10/c10_tls_p1_reg2c6_220309_11dot3m_htnorm.las")
vox_met2 <- voxel_metrics(las2, ~list(N2 = length(Z)), res = 0.5)
vox_filtered2 <- vox_met2[vox_met2$N2 >= 500 & vox_met2$N2 <= 4500, ]
plot(vox_filtered2, color = "N2", pal = heat.colors, size = 0.5, bg = "white", voxel = TRUE)

##############################################

c6c10_files <- str_subset(all_htnorm, "\\bc6\\b|\\bc10\\b")

veg_df <- data.frame(
  file = c6c10_files,
  plot = str_extract(basename(c6c10_files), "p\\d{1,4}"),
  veg_density = ""
)

i = 1
file_i = c6c10_files[i]
x = list()

for (file_i in c6c10_files) {
  
  message('Processing ', file_i)
  message(i, ' of ', length(c6c10_files))
  
  tictoc::tic()
  las <- readLAS(file_i)
  # voxels <- voxelize_points(las, res = 0.1)
  vox_met <- voxel_metrics(las, ~list(N = length(Z)), res = 1, all_voxels = TRUE)
  vox_file_name <- str_replace(file_i, "\\.las$", ".csv")
  write_csv(vox_met, vox_file_name)
  
  filled <- vox_met %>%
    group_by(Z) %>%
    summarize(
      n_voxel = n(),
      n_filled = sum(!is.na(N)),
      percentage = n_filled/n_voxel
    ) %>%
    add_column(plot = str_extract(basename(file_i), "p\\d{1,4}"),
               campaign = str_extract(basename(file_i), "c\\d{1,4}")
               )
  
  x[[file_i]] = filled
  
  
  tictoc::toc()
  
  i = i + 1
  
}

things = bind_rows(x)
write_csv(things, "E:/c6/things.csv")

##########################

mtbs <- read_csv("E:/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")

added_sev <- left_join(
  things,
  mtbs,
  by = c("plot" = "Plot"),
  relationship = "many-to-one"
)

plot_info <- added_sev %>%
  group_by(plot, campaign, RBR_NN) %>%
  summarize(
    sum_filled = sum(n_filled),
    total_vox = sum(n_voxel),
    perc = sum_filled/total_vox
  )

plot_info <- plot_info %>%
  ungroup() %>%
  mutate(prepost = case_match(
    campaign,
    'c6' ~ 'Pre',
    'c10' ~ 'Post'
  ) |>
    fct_relevel(
      'Pre',
      'Post'
    ))

ggplot(plot_info, aes(x = prepost, y = perc, color = RBR_NN)) + 
  geom_point() +
    geom_line(aes(group = plot))
