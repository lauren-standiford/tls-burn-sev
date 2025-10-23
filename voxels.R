library(lidR)
library(stringr)

tls <- "E:/"

all_files <- list.files(
  tls,
  full.names = T,
  recursive = T,
  pattern = 'htnorm\\.las$'
)

c6_files <- str_subset(all_files, "\\bc6\\b")
c10_files <- str_subset(all_files, "\\bc10\\b")

las <- readLAS("E:/c6/c6_tls_p1_200811_11dot3m_htnorm.las")
voxels <- voxelize_points(las, res = 0.1)
plot(voxels)

vox_met <- voxel_metrics(las, ~list(N = length(Z)), res = 0.5)
plot(vox_met, color="N", pal = heat.colors, size = 0.5, bg = "white", voxel = TRUE)
