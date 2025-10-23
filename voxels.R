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

vox_filtered <- vox_met[vox_met$N >= 200 & vox_met$N <= 4000, ]
plot(vox_filtered, color = "N", pal = heat.colors, size = 0.5, bg = "white", voxel = TRUE)

las_check = readLAS("E:/c6/c6_tls_p1_200811_11dot3m_htnorm.las", filter = '-keep_random_fraction 0.0001')
plot(las_check)

###

las2 <- readLAS("E:/c10/c10_tls_p1_reg2c6_220309_11dot3m_htnorm.las")
vox_met2 <- voxel_metrics(las2, ~list(N2 = length(Z)), res = 0.5)
vox_filtered2 <- vox_met2[vox_met2$N2 >= 500 & vox_met2$N2 <= 4500, ]
plot(vox_filtered2, color = "N2", pal = heat.colors, size = 0.5, bg = "white", voxel = TRUE)
