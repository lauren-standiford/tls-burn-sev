library(lidR)
library(tidyverse)

########## get data together for ht layers ##########

tls <- "E:/"
tls <- "/Volumes/tls"

#voxel_data = read_csv("E:/things.csv")
voxel_data = read_csv("/Volumes/tls/things.csv")

#mtbs <- read_csv("E:/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")
mtbs <- read_csv("/Volumes/tls/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")

veg_type <- read_csv("E:/plt_veg_type.csv")
veg_type <- read_csv("/Volumes/tls/plt_veg_type.csv")

# add RBR
added_sev <- left_join(
  voxel_data,
  mtbs,
  by = c("plot" = "Plot"),
  relationship = "many-to-many"
)

# add pre/post
added_sev <- added_sev %>%
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

# add veg type (not all plots have one check that later)
added_veg <- left_join(
  added_sev %>% mutate(plot = gsub("^p", "", plot)),
  veg_type %>% mutate(plot = as.character(plot)),
  by = "plot",
  relationship = "many-to-many"
)

# add sev classes
info_by_ht <- added_veg %>%
  ungroup() %>%
  mutate(sev_class = case_when(
    RBR_NN < 130 ~ 'Low',
    RBR_NN >= 130 & RBR_NN < 298 ~ 'Medium',
    RBR_NN >= 298 ~ 'High'
  ) |>
    fct_relevel(
      'Low',
      'Medium',
      'High'
    ))


write_csv(info_by_ht, "/Volumes/tls/all_data_by_ht.csv")
data_by_ht = read_csv("/Volumes/tls/all_data_by_ht.csv")

#################

