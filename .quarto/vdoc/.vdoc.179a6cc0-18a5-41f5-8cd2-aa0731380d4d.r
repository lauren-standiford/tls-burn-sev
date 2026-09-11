#
#
library(tidyverse)
data <- read_csv("E:/voxel_data.csv")
#
#
#
#
# histograms of percentage filled (at each ht) by resolution
data |>
  ggplot(aes(x = percentage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ res, ncol = 3, labeller = label_both) +
  labs(x = "Percentage", y = "Count", title = "Percentage filled (at each ht) by resolution") +
  theme_minimal()

# grouped by 5m height bins, all res
data |>
  mutate(Z_group = cut(
    Z,
    breaks = seq(0, max(Z, na.rm = TRUE) + 5, by = 5),
    right = FALSE,
    include.lowest = TRUE
  )) |>
  ggplot(aes(x = percentage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ Z_group, ncol = 3, labeller = label_both) +
  labs(x = "Percentage filled", y = "Count", title = "Percentage filled (at each ht) by 5m height bins") +
  theme_minimal()

# histogram of sum of percentage filled (by plot), all res
data |>
  group_by(plot, res) |>
  summarise(total_percentage = sum(percentage, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = total_percentage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ res, ncol = 3, labeller = label_both) +
  labs(x = "Total percentage filled (by plot)", y = "Count", title = "Total percentage filled (by plot) by resolution") +
  theme_minimal()

# total percentage filled (by plot), all res, all campaigns
data |>
  group_by(plot, campaign) |>
  summarise(total_percentage = sum(percentage, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = total_percentage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(x = "Total percentage filled (by plot)", y = "Count", title = "Total percentage filled by plot") +
  theme_minimal()

# total percentage filled by plot by campaign, all res
data |>
  group_by(plot, campaign) |>
  summarise(total_percentage = sum(percentage, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = total_percentage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ campaign, ncol = 2, labeller = label_both) +
  labs(x = "Total percentage filled (by plot)", y = "Number of plots (all res)", title = "Total percentage filled by plot by campaign") +
  theme_minimal()

# total percentage filled by plot by campaign, res 0.5
data |>
  filter(res == 0.5) |>
  group_by(plot, campaign) |>
  summarise(total_percentage = sum(percentage, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = total_percentage)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ campaign, ncol = 2, labeller = label_both) +
  labs(x = "Total percentage filled (by plot)", y = "Number of plots", title = "Total percentage filled by plot by campaign") +
  theme_minimal()
#
#
#
#
#
#
data |> 
  group_by(LF_FOREST, sev_class) |>
  summarise(n_plots = n_distinct(plot), .groups = "drop") |>
  mutate(sev_class = fct_relevel(sev_class, 'Low', 'Moderate', 'High')) |>
  arrange(LF_FOREST, sev_class)
#
#
#
data |>
    group_by(LF_FOREST, sev_class) |>
    ggplot(aes(x = LF_FOREST, y = RBR, color = sev_class)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Low" = "green", "Moderate" = "orange", "High" = "red")) +
    labs(x = "Forest Type", y = "RBR", color = "Severity Class") +
    theme_minimal()
#
#
#
#
data |>
    filter(campaign %in% c("c1", "c5", "c6", "c10")) |>
    mutate(site = case_when(
      campaign %in% c("c1", "c5") ~ "pepperwood",
      campaign %in% c("c6", "c10") ~ "saddle mtn"
    )) |>
    group_by(site, campaign) |>
    summarise(total_volume = sum(percentage, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = site, y = total_volume, fill = campaign)) +
    geom_col(width = 0.7, position = position_dodge(width = 0.8)) +
    labs(x = "Site", y = "Volume", fill = "Campaign") +
    theme_minimal()
#
#
#
data |>
    filter(campaign %in% c("c1", "c5", "c6", "c10")) |>
    mutate(site = case_when(
      campaign %in% c("c1", "c5") ~ "pepperwood",
      campaign %in% c("c6", "c10") ~ "saddle mtn"
    )) |>
    group_by(sev_class, site, campaign) |>
    summarise(total_volume = sum(percentage, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = site, y = total_volume, fill = campaign)) +
    geom_col(width = 0.7, position = position_dodge(width = 0.8)) +
    facet_wrap(~ sev_class) +
    labs(x = "Site", y = "Volume", fill = "Campaign") +
    theme_minimal()
#
#
#
data |>
    filter(campaign %in% c("c1", "c5", "c6", "c10")) |>
    group_by(plot, campaign, sev_class, LF_FOREST) |>
    summarise(total_volume = sum(percentage, na.rm = TRUE), .groups = "drop") |>
    group_by(campaign, sev_class, LF_FOREST) |>
    summarise(
      n_plots = n(),
      mean_volume = mean(total_volume, na.rm = TRUE),
      median_volume = median(total_volume, na.rm = TRUE),
      sd_volume = sd(total_volume, na.rm = TRUE),
      q1_volume = quantile(total_volume, 0.25, na.rm = TRUE),
      q3_volume = quantile(total_volume, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(campaign, sev_class, LF_FOREST)
#
#
#
#
data |>
    filter(Z >= 0, res == 0.5, campaign %in% c("c1", "c2", "c6", "c10")) |>
    mutate(prepost = case_when(
        campaign %in% c("c1", "c6") ~ "pre",
        campaign %in% c("c2", "c10") ~ "post")) |>
    mutate(
      Z_group = cut(
        Z,
        breaks = seq(0, max(Z, na.rm = TRUE) + 5, by = 5),
        right = FALSE,
        include.lowest = TRUE
      )
    ) |>
    group_by(Z_group, prepost, sev_class) |>
    summarise(
      mean_percentage = mean(percentage, na.rm = TRUE),
      .groups = "drop"
    ) |>
    ggplot(aes(x = mean_percentage, y = Z_group, color = prepost, shape = prepost)) +
    geom_point(size = 2, position = position_dodge(width = 0.4)) +
    facet_wrap(~ sev_class) +
    labs(
      x = "Mean percentage",
      y = "Z group (5 m bins)",
      color = "Pre/Post"
    )


#
#
#
#
data |>
    #filter(Z >= -1) |>
  group_by(res, campaign, Z) |>
  summarise(
    mean_percentage = mean(percentage, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(res, campaign, Z) |>
  ggplot(aes(x = mean_percentage, y = Z, color = campaign, group = campaign)) +
  geom_point(size = 0.5)
#
#
#
