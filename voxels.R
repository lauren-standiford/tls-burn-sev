library(lidR)
library(tidyverse)
library(glue)

tls <- "E:/"
tls <- "/Volumes/tls"

all_htnorm <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'htnorm\\.las$'
)

c6_files <- str_subset(all_htnorm, "\\bc6\\b")
c10_files <- str_subset(all_htnorm, "\\bc10\\b")

#==============================================================
#                      visualize voxels 
#==============================================================

las <- readLAS("/Volumes/tls/c1/c1_tls_p1301_201019_11dot3m_htnorm.las", filter = '-keep_random_fraction 0.0001')
las <- readLASheader("/Volumes/tls/c1/c1_tls_p1301_201019_11dot3m_htnorm.las")
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

#==============================================================
#               generate voxels and metrics
#==============================================================

tls <- "E:/"

norm_files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'htnorm\\.las$'
)

las_files <- str_subset(norm_files, "c1_htnorm|c2_htnorm")

# veg_df <- data.frame(
#   file = c6c10_files,
#   plot = str_extract(basename(c6c10_files), "p\\d{1,4}"),
#   veg_density = ""
# )

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
  
  write_csv(filled, glue("D:/c1c2_voxel_results/{c}_{p}_{res}vox_summary.csv"))
  
  tictoc::toc()
  rm(filled, vox_met, las)
  gc()
  }
  i = i + 1
}

things = bind_rows(x)
write_csv(things, "E:/c6/things.csv")
voxel_data = read_csv("E:/things.csv")
voxel_data = read_csv("/Volumes/tls/things.csv")

all_csv_files <- list.files("D:/c1c2_voxel_results/c1_0dot25/", full.names = TRUE)
#one_res <- str_subset(all_csv_files, "\\bc1_0dot1\\b")
combined <- all_csv_files %>%
  lapply(read_csv) %>%
  bind_rows()
write_csv(combined, "D:/c1c2_voxel_results/c2_vox_data_res0dot25.csv")

c1_vox <- read_csv("D:/c1c2_voxel_results/c1_0dot25/c1_vox_data_res0dot25.csv")
c2_vox <- read_csv("D:/c1c2_voxel_results/c2_0dot25/c2_vox_data_res0dot25.csv")
prepost_vox_data <- rbind(c1_vox, c2_vox)
write_csv(prepost_vox_data, "D:/c1c2_voxel_results/c1c2_vox_data_res0dot25.csv")

one_res <- read_csv("D:/c1c2_voxel_results/c1_0dot25/c1_vox_data_res0dot25.csv")

one_res <- one_res %>%
  filter(res == 0.05,
         Z >= 0)

new_df <- one_res %>%
  left_join(
    one_res,
    just_add,
    by = c("plot", "campaign"),
    relationship = "many-to-many"
  )
  
everything <- read_csv("D:/everything.csv")

just_add <- everything %>%
  select(plot, campaign, RBR_NN, RBR_3x3avg, prepost, LF_FOREST, sev_class)
  # distinct()

#==============================================================
#                        add fire sev data 
#==============================================================

mtbs <- read_csv("D:/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")
mtbs <- read_csv("/Volumes/tls/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")

voxel_data = read_csv("D:/c1c2_voxel_results/c1c2_vox_data_res0dot25.csv")

added_sev <- left_join(
  voxel_data,
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

all_vox_data <- all_vox_data %>%
  ungroup() %>%
  mutate(prepost = case_match(
    campaign,
    'c1' ~ 'Pre',
    'c2' ~ 'Post'
  ) |>
    fct_relevel(
      'Pre',
      'Post'
    ))

# add sev classes
all_vox_data <- all_vox_data %>%
  ungroup() %>%
  mutate(sev_class = case_when(
    RBR_NN < 130 ~ 'Low',
    RBR_NN >= 130 & RBR_NN < 298 ~ 'Moderate',
    RBR_NN >= 298 ~ 'High'
  ) |>
    fct_relevel(
      'Low',
      'Moderate',
      'High'
    ))

write_csv(plot_info, "D:/c1c2_voxel_results/c1c2_byplot_wsev.csv")

p <- ggplot(plot_info, aes(x = prepost, y = perc, color = RBR_NN)) + 
  geom_point() +
    geom_line(aes(group = plot)) +
  labs(
    x = "Wildfire status",
    y = "% of filled voxels"
  )

ggsave("D:/figures/c1c2_vvp_v1.png", plot = p)

#==============================================================
#                       add forest type data 
#==============================================================

veg_type <- read_csv("D:/plt_veg_type.csv")
veg_type <- read_csv("/Volumes/tls/plt_veg_type.csv")

added_veg <- left_join(
  added_sev %>% mutate(plot = gsub("^p", "", plot)),
  veg_type %>% mutate(plot = as.character(plot)),
  by = "plot",
  relationship = "many-to-one"
) %>%
  select(Z, n_voxel, n_filled, percentage, plot, campaign, res, RBR_NN, RBR_3x3avg, LF_FOREST)


write_csv(added_veg, "D:/c1c2_voxel_results/c1c2_vox0dot25_sev_veg.csv")
all_stuff = read_csv("E:/prepost_veg_251028.csv")
all_stuff = read_csv("/Volumes/tls/prepost_veg_251028.csv")

#########

added_veg <- read_csv("/Volumes/tls/prepost_veg_251028.csv")

added_veg <- added_veg %>%
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

split_FR_plots <- ggplot(added_veg %>%
         filter(!LF_FOREST == "Mixed Conifer-Hardwood Forest") %>%
         filter(!is.na(LF_FOREST)),
       aes(x = prepost, y = perc, color = RBR_NN)) +
  geom_point() +
  geom_line(aes(group = plot)) +
  facet_wrap(~ LF_FOREST) +
  labs(x = "Wildfire status", y = "% of filled voxels") +
  scale_color_gradient(low = "blue", high = "red", name = "RBR")

print(split_FR_plots)
ggsave("/Volumes/tls/figures/forest_type_split_plot.png", plot = split_FR_plots)

#==============================================================
#                      missing forest type 
#==============================================================

everything = read_csv("D:/c1c2_voxel_results/c1c2_vox0dot25_sev_veg.csv")

everything = everything %>%
  # filter(is.na(LF_FOREST))
  mutate(LF_FOREST = ifelse(is.na(LF_FOREST) & plot == 1312, "Hardwood Forest", LF_FOREST))
         #LF_FOREST = ifelse(is.na(LF_FOREST) & plot == 17, "Conifer Forest", LF_FOREST))

field_data = read_csv("D:/field_data/l83df_burn_severity_veg_type.csv")

field_data = field_data %>%
  filter(Plot %in% c('p1312', 'p1313')) %>%
  select(Plot, DBH, Species)

#==============================================================
#                           missing RBR 
#==============================================================

everything = read_csv("D:/c1c2_voxel_results/c1c2_vox0dot25_sev_veg.csv")
write_csv(all_vox_data, "D:/c1c2_voxel_results/c1c2_vox0dot25_sev_veg.csv")

noRBR = everything %>%
  filter(is.na(RBR_NN)) %>%
  select(plot, campaign) %>%
  distinct()

RBR_values = read_csv("/Volumes/tls/l83df_burn_severity_veg_type.csv")
RBR_values = RBR_values %>%
  select(Plot, RBR, RBR3x3) %>%
  mutate(Plot = gsub("^p", "", Plot)) %>%
  rename(RBR_NN = RBR) %>%
  rename(RBR_3x3avg = RBR3x3) %>%
  subset(Plot %in% c("20", "23")) 

RBR_values$Plot <- as.character(RBR_values$Plot)
added_veg$plot <- as.character(added_veg$plot)

new_added_veg = added_veg %>%
  left_join(RBR_values, by = c("plot" = "Plot"), suffix = c("", "_new")) %>%
  mutate(
    RBR_NN = coalesce(RBR_NN, RBR_NN_new),
    RBR_3x3avg = coalesce(RBR_3x3avg, RBR_3x3avg_new)
  ) %>%
  select(-ends_with("_new"))

write_csv(all_vox_data, "/Volumes/tls/all_data_res.05to.5_voxels.csv")
all_vox_data = read_csv("/Volumes/tls/all_data_res.05to.5_voxels.csv")
