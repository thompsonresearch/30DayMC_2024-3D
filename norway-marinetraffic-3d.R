if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  geodata,
  sf,
  elevatr,
  terra,
  tidyverse,
  dplyr,
  osmdata,
  ggplot2,
  rnaturalearth,
  giscoR,
  tmap,
  rnaturalearth,
  rnaturalearthhires,
  scales,
  rayshader,
  magick
)

path <- getwd()


# fishing vessel data
ship.url <- "https://datacatalogfiles.worldbank.org/ddh-published/0037580/DR0045403/ShipDensity_Fishing.zip"
destfile.ship <- basename(ship.url)

options(timeout = 999)

download.file(
  url = ship.url,
  destfile = destfile.ship,
  mode = "wb"
)

source("decompress_file.r")

decompress_file(
  directory = getwd(),
  file = destfile.ship
)

rastfile <- gsub(
  ".zip",
  ".tif",
  destfile.ship
)

glob_traffic <- terra::rast(rastfile)

# Bound box for area of interest
# 0.686977,56.751365,32.662466,72.174925

xmin <- 0.686977
ymin <- 56.751365
xmax <- 32.662466
ymax <- 72.174925

bbox <- sf::st_as_sfc(
  sf::st_polygon(
    list(
      cbind(
        c(xmin, xmax, xmax, xmin, xmin),
        c(ymin, ymin, ymax, ymax, ymin)
      )
    )
  ), crs = 4326
)

ship_traffic <- terra::crop(
  x = glob_traffic,
  y = bbox,
  snap = "in"
)

terra::plot(ship_traffic)

ship_traffic_clean <- terra::ifel(
  ship_traffic == 0,
  NA,
  ship_traffic
)

# Nightlight data 

u <- "https://www.youtube.com/redirect?event=video_description&redir_token=QUFFLUhqbkdFYTA0ZGFmYTd0S19EVU1IbkpKY1hvOW1hd3xBQ3Jtc0tuRS14Y1lPV0hWV0hoc0FibDlMOGlycFB0TEJtVWNnaFR3Y0pQR0dneXVzeXB1Qm41M1RRdXU1M1QyMUlDSjZ3VU9wd2FmczN4UFh1WnBsWVJoYjhkek9WVzFIanQ3ckFWQU5fMDJKM0hseDB4S1hxaw&q=https%3A%2F%2Feogdata.mines.edu%2Fnighttime_light%2Fannual%2F&v=k7xWK4rp5pY"

filename <- basename(u)

download.file(
  url = u,
  destfile = filename,
  mode = "wb"
)

nl_path <- list.files(
  path = getwd(),
  pattern = filename,
  full.names = TRUE
)

nl <- terra::rast(
  paste0(
    "/vsigzip/", nl_path
  )
)

nl_region <- terra::crop(
  x = nl,
  y = bbox,
  snap = "in"
)

nl_resampled <- terra::resample(
  x = nl,
  y = ship_traffic_clean,
  method = "bilinear"
)

terra::plot(nl_resampled)

nl_cols <- c(
  "#061c2c",
  "#1f4762",
  "#ffd966",
  "#ffffff"
)

nl_pal <- colorRampPalette(
  nl_cols,
  bias = 12
)(256)

ship_traffic_cols <- hcl.colors(
  n = 5,
  palette = "Blues"
)

scales::show_col(
  ship_traffic_cols,
  ncol = 5,
  labels = TRUE
)

ship_traffic_pal <- colorRampPalette(
  ship_traffic_cols[1:4]
)(256)

nl_df <- as.data.frame(
  nl_resampled,
  xy = TRUE,
  na.rm = TRUE
)

names(
  nl_df
)[3] <- "nightlight_value"

ship_traffic_df <- as.data.frame(
  ship_traffic_clean,
  xy = TRUE,
  na.rm = TRUE
)

head(nl_df)
head(ship_traffic_df)


# DEM elev
# --------------
dem <- elevatr::get_elev_raster(
  locations = sf::st_bbox(bbox),
  z = 8,  # Adjust zoom level based on your resolution needs
  clip = "bbox"
)

# Convert DEM to a raster object
dem_raster <- terra::rast(dem)

# Project DEM to match the CRS of your data
dem_projected <- terra::project(
  dem_raster,
  crs = terra::crs(ship_traffic_clean)
)

# Convert DEM to a matrix for rayshader
dem_matrix <- rayshader::raster_to_matrix(dem_projected)


# Overlay nl and ship traffic
# --------------------------- 

# Resample DEM to match the resolution of nightlights and ship traffic
dem_resampled <- terra::resample(
  x = dem_projected,
  y = nl_resampled,
  method = "bilinear"
)

# Convert DEM to matrix for rendering
dem_matrix <- rayshader::raster_to_matrix(dem_resampled)


# Render 3D Map
# -----------------

# Generate color overlays for nightlights and ship traffic
nl_overlay <- rayshader::generate_surface_overlay(
  heightmap = dem_matrix,
  data = nl_resampled,
  palette = nl_pal
)

ship_traffic_overlay <- rayshader::generate_surface_overlay(
  heightmap = dem_matrix,
  data = ship_traffic_clean,
  palette = ship_traffic_pal
)

# Render the DEM with overlays
dem_matrix %>%
  rayshader::height_shade(texture = rayshader::globe_shade()) %>%
  rayshader::add_overlay(nl_overlay, alphalayer = 0.7) %>%
  rayshader::add_overlay(ship_traffic_overlay, alphalayer = 0.7) %>%
  rayshader::plot_3d(
    heightmap = dem_matrix,
    zscale = 10,  # Adjust z-scale for exaggeration
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 0.5,
    windowsize = c(800, 800),
    zoom = .65,
    phi = 45,
    theta = 0
  )

# Adjust the camera if needed
# rayshader::render_camera(
#  zoom = 0.7,
#  phi = 78,
#  theta = 0
# )


# Save the render
# ----------------------

# Save the high-quality render
file_name <- "dem_nightlight_shiptraffic_3d.png"
hdri_url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/exr/4k/photo_studio_loft_hall_4k.exr"
hdri_file <- basename(hdri_url)

download.file(
  url = hdri_url,
  destfile = hdri_file,
  mode = "wb"
)

rayshader::render_highquality(
  filename = file_name,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1.5,
  interactive = FALSE,
  width = 3000,
  height = 3000
)














