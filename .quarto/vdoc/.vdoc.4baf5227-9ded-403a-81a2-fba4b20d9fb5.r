#
#
#
library(sf)
library(xml2)
library(tidyverse)

scans <- c(1301, 1303, 1304, 1306, 1310, 1312, 1313, 1325, 1329, 1332, 1335, 1337, 1340, 1342, 1349, 1853, 1307, 1309, 1315, 1319, 1854, 1336)
#
#
#
#
# 1. Set your KMZ file path
kmz_filename <- "E://PepperwoodPlotLocations//PepperwoodPlotLocations//2021_plot_locations.kmz"

# 2. Define temporary unzip directory and output name (in same folder as KMZ)
kmz_folder <- dirname(kmz_filename)
unzip_dir <- "extracted_kml_temp"
output_dir <- kmz_folder
output_layer <- "ppw_plots"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 3. R automatically unzips the KMZ to extract the KML data
unzip(kmz_filename, exdir = unzip_dir)

# 4. R finds the newly extracted .kml file automatically
kml_file <- list.files(unzip_dir, pattern = "\\.kml$", full.names = TRUE)

# 5. Parse the KML directly so every SimpleData field is preserved
kml_doc <- read_xml(kml_file[1])
kml_ns <- xml_ns(kml_doc)
placemarks <- xml_find_all(kml_doc, ".//kml:Placemark", kml_ns)

parse_placemark <- function(placemark) {
	simple_data <- xml_find_all(placemark, ".//kml:ExtendedData//kml:SimpleData", kml_ns)
	values <- xml_text(simple_data)
	names(values) <- xml_attr(simple_data, "name")

	coords_text <- xml_text(xml_find_first(
		placemark,
		".//kml:Polygon//kml:outerBoundaryIs//kml:LinearRing//kml:coordinates",
		kml_ns
	))
	coords <- trimws(strsplit(coords_text, "\\s+")[[1]])
	coords <- do.call(rbind, strsplit(coords, ","))
	coords <- matrix(as.numeric(coords[, 1:2, drop = FALSE]), ncol = 2)

	list(
		data = as.data.frame(as.list(values), stringsAsFactors = FALSE),
		geometry = st_polygon(list(coords))
	)
}

parsed <- lapply(placemarks, parse_placemark)
all_fields <- unique(unlist(lapply(parsed, function(x) names(x$data))))
attr_rows <- lapply(parsed, function(x) {
	missing_fields <- setdiff(all_fields, names(x$data))
	for (field_name in missing_fields) {
		x$data[[field_name]] <- NA
	}
	x$data[all_fields]
})
plot_data <- st_sf(
	type.convert(do.call(rbind, attr_rows), as.is = TRUE),
	geometry = st_sfc(lapply(parsed, `[[`, "geometry"), crs = 4326)
)

# 6. Export the preserved attributes and geometry as a shapefile
output_file <- file.path(output_dir, paste0(output_layer, ".shp"))
st_write(plot_data, dsn = output_dir, layer = output_layer, driver = "ESRI Shapefile", delete_layer = TRUE)

shp <- st_read(output_file, quiet = TRUE)
names(shp)
unique(shp$Plot)
#
#
#
#
forest <- read_csv("E:/plots_mtbs_veg.csv") |>
    select(Plot, LF_FOREST, RdNBR, RBR) |>
    mutate(Plot = str_remove_all(Plot, "p"))
ndvi <- read_csv("E:/ndvi/PPW_Landsat_NDVI_4.csv") |>
    select("system:index", Plot, date) |>
	mutate(Plot = str_remove_all(Plot, "PPW")) |>
	#filter(!str_starts(Plot, "15")) |>
	left_join(forest, by = "Plot") |>
    filter(!is.na(LF_FOREST),
    Plot %in% scans)

unique(ndvi$Plot)
view(ndvi)
ndvi <- read_csv("E:/ndvi/PPW_Landsat_NDVI_9.csv")


ndvi3 <- read_csv("E:/ndvi/PPW_Landsat_NDVI_3.csv")
unique(ndvi3$Plot) == unique(ndvi$Plot)

 |>
	filter(Plot == "PPW1307")
view(ndvi3)
summary(ndvi3)

ndvi9 <- read_csv("E:/ndvi/PPW_Landsat_NDVI_9.csv") |>
	filter(Plot == "PPW1307")
view(ndvi9)
summary(ndvi9)

mean(ndvi3$NDVI)
mean(ndvi8$NDVI)

#
#
#
