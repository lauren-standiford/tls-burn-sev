library(lidR)
library(tidyverse)

df = tibble(
  perc = runif(10),
  z = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
  type = c('con', 'con', 'con', 'con', 'con', 
           'oak', 'oak', 'oak', 'oak', 'oak')
)

df2 = everything %>%
  filter(!LF_FOREST == "Mixed Conifer-Hardwood Forest",
         !is.na(LF_FOREST),
         Z >= 0) %>%
  group_by(Z, LF_FOREST, sev_class, prepost) %>%
  summarise(
    total_filled = sum(n_filled),
    total_overall = sum(n_voxel),
    perc = total_filled/total_overall) %>%
  # ungroup() %>%
  select(Z, perc, LF_FOREST, sev_class, prepost) %>%
  group_by(Z, LF_FOREST, sev_class, prepost)



df10 = df2 %>%
  rowwise() %>%
  mutate(
    r = map(Z, ~ rep.int(.x, floor(perc * 100))
    )) %>%
  unnest(r) %>%
  ungroup()


p = ggplot() +
  geom_violin(df10,
              mapping = aes(LF_FOREST, Z, color = prepost)) +
  facet_wrap(~ sev_class)

ggsave("/Volumes/tls/figures/violin_plot.png", plot = p)

## old janky stuff ########
########## get data together for ht layers ##########

tls <- "E:/"
tls <- "/Volumes/tls"

#voxel_data = read_csv("E:/things.csv")
voxel_data = read_csv("/Volumes/tls/things.csv")

#mtbs <- read_csv("E:/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")
mtbs <- read_csv("/Volumes/tls/burn_severity_3dforests/kincade_glass_fire_tls_plot_centers_sentinel2a_20m_rbr.csv")

#veg_type <- read_csv("E:/plt_veg_type.csv")
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
everything <- added_veg %>%
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

condense_hts <- data_by_ht %>%
  group_by(Z, prepost, LF_FOREST, sev_class) %>%
  summarize(
    sum_filled = sum(n_filled),
    total_vox = sum(n_voxel),
    perc = sum_filled/total_vox
  )

just_by_hts <- data_by_ht %>%
  group_by(Z) %>%
  summarize(
    sum_filled = sum(n_filled),
    total_vox = sum(n_voxel),
    perc = sum_filled/total_vox
  )

write_csv(condense_hts, "/Volumes/tls/condensed_ht.csv")
condense_hts = read_csv("/Volumes/tls/condensed_ht.csv")

################# okay now to plot ############

ggplot(everything %>%
         filter(!LF_FOREST == "Mixed Conifer-Hardwood Forest") %>%
         filter(!is.na(LF_FOREST)) %>%
         filter(Z >= 0),
       aes(n_filled, Z, fill = prepost, color = prepost)
) +
  geom_violin(scale = "count") +
  facet_wrap(~ interaction(LF_FOREST, prepost))



