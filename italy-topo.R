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

country <- geodata::gadm(
  country = "ITA",
  level = 1,
  path = path
) %>%
  sf::st_as_sf()

country <- country %>%
  dplyr::filter(NAME_1 == "Valle d'Aosta")

plot(country$geometry)

# Rivers
riv.url <- "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_eu_shp.zip"
destfile <- basename(riv.url)

download.file(
  url = riv.url,
  destfile = destfile,
  mode = "wb"
)

unzip(destfile)

# Load rivers

filename <- list.files(
  path = "HydroRIVERS_v10_eu_shp",
  pattern = ".shp",
  full.names = TRUE
)

country_bbox <- sf::st_bbox(country)
print(country_bbox)

# xmin      ymin      xmax 
# 6.630879 35.492916 18.520695 
# ymax 
# 47.092651 

bbox_wkt <- "POLYGON((
  6.812168 45.465267, 
  6.812168 45.990124,
  7.940472 45.990124,
  7.940472 45.465267,
  6.812168 45.465267
))"

country_rivers <- sf::st_read(
  filename,
  wkt_filter = bbox_wkt
) %>%
  sf::st_intersection(
    country
  )

plot(country_rivers$geometry)

# River width

sort(
  unique(
    country_rivers$ORD_FLOW
  )
)

crs_country <- "+proj=tmerc +lat_0=0 +lon_0=12 +k=0.9985 +x_0=7000000 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"

country_river_width <- country_rivers %>%
  dplyr::mutate(
    width = as.numeric(
      ORD_FLOW
    ),
    width = dplyr::case_when(
      width == 5 ~ 6,
      width == 6 ~ 4,
      width == 7 ~ 2,
      TRUE ~ 0
    )
  ) %>%
  sf::st_as_sf() %>%
  sf::st_transform(crs = crs_country)


# DEM elev

dem <- elevatr::get_elev_raster(
  locations = country,
  z = 8,
  clip = "locations"
)

dem_country <- dem %>%
  terra::rast() %>%
  terra::project(crs_country)

dem_matrix <- rayshader::raster_to_matrix(
  dem_country
)


# Render Scene

dem_matrix %>%
  rayshader::height_shade(
    texture = colorRampPalette(
      c(
        "#fcc69f",
        "#c67847"
      )
    )(128)
  ) %>%
  rayshader::add_overlay(
    rayshader::generate_line_overlay(
      geometry = country_river_width,
      extent = dem_country,
      heightmap = dem_matrix,
      color = "#387b9c",
      linewidth = country_river_width$width,
      data_column_width = "width"
    ), alphalayer = 1
  ) %>%
  rayshader::plot_3d(
    dem_matrix,
    zscale = 20,
    solid = FALSE,
    shadow = TRUE,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(800, 800),
    zoom = .5,
    phi = 84,
    theta = 0
  )

rayshader::render_camera(
  zoom = .65
)

# Render Obj

file_name <- "italy-rivers-3d.png"

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/exr/4k/photo_studio_loft_hall_4k.exr"
hdri_file <- basename(u)

download.file(
  url = u,
  destfile = hdri_file,
  mode = "wb"
)

rayshader::render_highquality(
  filename = file_name,
  preview = TRUE,
  light = FALSE,
  environment_light = hdri_file,
  intensity_env = 1.75,
  interactive = FALSE,
  width = 3000,
  height = 3000
)















