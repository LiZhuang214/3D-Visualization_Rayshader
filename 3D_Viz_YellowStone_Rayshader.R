# notes:
# hold "option" to zoom in and out with toggling
# not exaggerating the plot of the data fov: field of view = 0, 70 - iphone, 90 - other cameras, really world camera feeling

install.packages(c("ggplot2","raster", "rayrender", "spatstat", "spatstat.utils","suncalc","here", "sp","lubridate","rgdal", "magick", "av","xml2", "dplyr"))
install.packages("rayshader")

#macOS: xcrun issue? type "xcode-select --install" into terminal
#macOS imager.so or libx11 errors? Install X11: https://www.xquartz.org

install.packages("remotes")

remotes::install_github("giswqs/whiteboxR")
whitebox::wbt_init()

remotes::install_github("tylermorganwall/rayshader")

options(rgl.useNULL = FALSE)
library(ggplot2)
library(whitebox)
library(rayshader)
library(rayrender)
library(raster)
library(spatstat)
library(spatstat.utils)
library(suncalc)
library(sp)
library(lubridate)
library(rgdal)

setwd(here::here())

##### Hillshading yellowstone lake
# tempfile <- "/Users/lizhuang/Desktop/MasterClass2019_3DMappingAndViz-master/clip_ys2.tif"
yellowstone_tif = raster::raster(tempfile)

# crs(yellowstone_tif)
# newproj <- "+proj=tmerc +lat_0=40.5 +lon_0=-110.0833333333333 +k=0.9999375 +x_0=800000 +y_0=100000 +ellps=GRS80 +units=m +no_defs"
# yellowstone_tif <- projectRaster(yellowstone_tif, crs=newproj)

yellowstone_mat = rayshader::raster_to_matrix(yellowstone_tif)
yellowstone_mat[1:10,1:10]

##### elevation to color mapping using height_shade() function
yellowstone_mat %>%
  height_shade() %>%
  plot_map()

yellowstone_mat %>%
  sphere_shade() %>%
  plot_map()

#### add water feature to the terrain
yellowstone_mat %>%
  sphere_shade() %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.98)) %>%
  plot_map()

##### try different styles 
yellowstone_mat %>%
  sphere_shade(texture = "imhof4") %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.98), color = "imhof4") %>%
  plot_map()

yellowstone_mat %>%
  sphere_shade(texture = "bw") %>%
  add_water(detect_water(yellowstone_mat, cutoff = 1), color = "unicorn") %>%
  plot_map()

##### simulates how light travels across the elevation model
#Default angle: 315 degrees.
#Change sunangle to 225
yellowstone_mat %>%
  sphere_shade(sunangle = 300) %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.98), color = "lightblue") %>%
  add_shadow(ray_shade(yellowstone_mat,zscale = 33, sunaltitude = 3,lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(yellowstone_mat,zscale = 33,sunaltitude = 3), max_darken = 0.5) %>%
  plot_map()

#### with ambient occlusion: add sky view factor to the plot
yellowstone_mat %>%
  sphere_shade() %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.98), color = "lightblue") %>%
  add_shadow(ray_shade(yellowstone_mat, zscale = 33, sunaltitude = 5,lambert = FALSE), 
             max_darken = 0.5) %>%
  add_shadow(lamb_shade(yellowstone_mat,zscale = 33, sunaltitude = 5), max_darken = 0.7) %>%
  add_shadow(ambient_shade(yellowstone_mat), max_darken = 0.1) %>%
  #plot_map() %>%
  save_png("Yellowstone_ambient.png")


##### 3D Mapping
ambientshadows = ambient_shade(yellowstone_mat)

yellowstone_mat %>%
  sphere_shade() %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.89), color = "lightblue") %>%
  add_shadow(ray_shade(yellowstone_mat, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(yellowstone_mat, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  plot_3d(yellowstone_mat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))

render_snapshot()
rgl::rgl.clear()

#######
yellowstone_mat %>%
  sphere_shade() %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.98), color = "lightblue") %>%
  add_shadow(ray_shade(yellowstone_mat, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(yellowstone_mat, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0) %>%
  plot_3d(yellowstone_mat, zscale = 10, fov=0, windowsize = c(1000,1000), 
          phi = 45, theta = 135, zoom = 0.9, 
          background = "#000000", shadowcolor = "grey5",
          soliddepth = -15, shadowdepth = -15)

render_snapshot(title_text = "Yellowstone", 
                title_font = "Helvetica", 
                title_size = 50,
                title_color = "grey90")

render_snapshot(filename = "yellowstone.png")

##### depth of field
yellowstone_mat %>%
  sphere_shade(sunangle = 60) %>%
  add_water(detect_water(yellowstone_mat, cutoff = 0.98), color = "lightblue") %>%
  add_shadow(ray_shade(yellowstone_mat, sunangle = 60, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(yellowstone_mat, sunangle = 60, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambientshadows, max_darken = 0.1) %>%
  plot_3d(yellowstone_mat, zscale = 10,windowsize = c(1000,1000), 
          background = "#000000", shadowcolor = "#273633")  ##edfffc

render_camera(theta = 120, phi = 20, zoom = 0.3, fov = 70)
render_depth(focus = 0.81, preview_focus = TRUE)
render_depth(focus = 0.9, preview_focus = TRUE)
render_depth(focus = 0.81)

render_depth(focus = 0.81, focallength = 200, title_bar_color = "black", vignette = TRUE,
             title_text = "Yellowstone Lake", title_color = "white", title_size = 50)

rgl::rgl.close()

######### make a movie
#Orbit will start with current setting of phi and theta
render_movie(filename = "YellowstoneLake.mp4", 
             phi = 20 , theta = 120, focus = 0.81, fov = 90)

render_movie(filename = "YellowstoneLakemovie.mp4", phi = 30 , theta = -90, type = "oscillate",
             title_text = 'Yellowstone Lake', title_color = "black")
