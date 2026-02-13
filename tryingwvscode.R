library(lidR)
library(tidyverse)
library(glue)
library(stringr)

# Get files
all_files <- list.files("E:/c15", full.names = TRUE)
las_files <- all_files[str_detect(all_files, "htnorm\\.las$")]
las_files

# Calculate voxel metrics for each plot and save results
i = 1
file_i = las_files[i]
x = list()
res_values = c(0.05, 0.1, 0.5)

for (file_i in las_files) {
  for (res in res_values) {
  
  message('Processing ', file_i)
  message(i, ' of ', length(las_files))
  message('Voxel size = ', res)
  
  tictoc::tic()
  las <- readLAS(file_i)
  # voxels <- voxelize_points(las, res = 0.1)
  vox_met <- voxel_metrics(las, ~list(N = length(Z)), res = res, all_voxels = TRUE)
  # vox_file_name <- str_replace(file_i, "\\.las$", ".csv")
  
  c <- str_extract(basename(file_i), "c\\d{1,4}")
  p <- str_extract(basename(file_i), "p\\d{1,4}")
                   
  # vox_file_name <- glue("{c}_{p}_{res}vox_metrics.csv")
  # write_csv(vox_met, glue("{c}_{p}_{res}vox_metrics.csv"))
  
  filled <- vox_met %>%
    group_by(Z) %>%
    summarize(
      n_voxel = n(),
      n_filled = sum(!is.na(N)),
      percentage = n_filled/n_voxel
    ) %>%
    add_column(plot = p,
               campaign = c,
               res = res)
  
  # x[[paste0(file_i, "_", res)]] <- filled
  
  write_csv(filled, glue("E:/voxel_results/c15_voxel_results/{c}_{p}_{res}vox_summary.csv"))
  
  tictoc::toc()
  rm(filled, vox_met, las)
  gc()
  }
  i = i + 1
}

# Get files from c15_voxel_results 
vox_files <- list.files("E:/voxel_results/c15_voxel_results", full.names = TRUE)

vox_005_files <- vox_files[str_detect(vox_files, "0\\.05")]
vox_01_files <- vox_files[str_detect(vox_files, "0\\.1")]
vox_05_files <- vox_files[str_detect(vox_files, "0\\.5")]

# Read and combine all files for each voxel size, then save 
vox_005_combined <- map_df(vox_005_files, read_csv)
write_csv(vox_005_combined, "E:/voxel_results/c15_voxel_results/c15_vox_005_combined.csv")

vox_01_combined <- map_df(vox_01_files, read_csv)
write_csv(vox_01_combined, "E:/voxel_results/c15_voxel_results/c15_vox_01_combined.csv")

vox_05_combined <- map_df(vox_05_files, read_csv)
write_csv(vox_05_combined, "E:/voxel_results/c15_voxel_results/c15_vox_05_combined.csv")

# Combine all resolutions for c15
c15_all_res <- bind_rows(vox_005_combined, vox_01_combined, vox_05_combined)
write_csv(c15_all_res, "E:/voxel_results/c6c10c15_vox_all_res.csv")

# Read and combine c6c10 and c15 all resolutions data
c6c10_data <- read_csv("E:/voxel_results/sm_combined_voxel_results/c6c10_vox_data_all_res.csv")
c15_data <- read_csv("E:/voxel_results/sm_combined_voxel_results/c15_vox_005_01_05.csv")

combined_all <- bind_rows(c6c10_data, c15_data)
write_csv(combined_all, "E:/voxel_results/c6c10c15_vox_all_res.csv")

vox_data <- read_csv("E:/voxel_results/c6c10c15_vox_all_res.csv") %>%
  filter(res == 0.05,
  campaign %in% c("c6", "c15"),
  Z >= 0) %>%
  mutate(plot = str_remove(plot, "^p"))
view(vox_data)
# add veg/fire data
veg_fire_data <- read_csv("E:/everything.csv") %>%
  select(plot, campaign, RBR_NN, RBR_3x3avg, prepost, LF_FOREST, sev_class) %>%
  mutate(plot = as.character(plot)) %>%
  distinct(plot, campaign, .keep_all = TRUE)

view(vox_veg_fire)

vox_veg_fire <- vox_data %>%
  left_join(
    veg_fire_data,
    by = c("plot", "campaign")
  ) %>%
  mutate(prepost = if_else(campaign == "c15" & prepost == "post", "12mo_post", prepost))

View(vox_veg_fire)

# ignore what's above this

vox_data <- read_csv("E:/voxel_results/c6c10c15_vox_all_res.csv") %>%
  filter(res == 0.05,
  campaign %in% c("c6", "c15"),
  Z >= 0) %>%
  mutate(plot = str_remove(plot, "^p"))
view(vox_data)

veg_fire_data <- read_csv("E:/all_data_res.05to.5_voxels.csv") %>%
  select(Z, plot, campaign, RBR_NN, RBR_3x3avg, prepost, LF_FOREST, sev_class) %>%
  mutate(plot = as.character(plot)) %>%
  filter(Z == 1) %>%
  distinct(plot, campaign, .keep_all = TRUE)
view(veg_fire_data)

# Join veg_fire_data to vox_data
vox_data <- vox_data %>%
  left_join(
    veg_fire_data %>% select(plot, campaign, RBR_NN, RBR_3x3avg, LF_FOREST, sev_class),
    by = c("plot", "campaign")
  )
view(vox_data)

