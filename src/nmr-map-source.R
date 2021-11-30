
# nmr-map-source.R
#
# This script provides part of the source code necessary to render 
# 3D maps of Wytham Woods, Oxford. 
# 
# Copyright (c) Nilo Merino recalde, 2020, except where indicated
# Date Created: 2020-03-23


# --------------------------------------------------------------------------
# REQUIRES
# --------------------------------------------------------------------------

library(tidyverse)
library(rayshader)
library(magick)
library(imager)
library(rayshader)
library(raster)
library(rgdal)

# --------------------------------------------------------------------------
# PATHS
# --------------------------------------------------------------------------

data_path <- file.path(getwd(), "data")
figures_path <- file.path(getwd(), "reports", "figures")

sp40ne <- "https://environment.data.gov.uk/UserDownloads/interactive/fe7178ddc75f4d81ac5e596b9948215498892/LIDARCOMP/LIDAR-DSM-1M-SP40ne.zip"
sp40nw <- "https://environment.data.gov.uk/UserDownloads/interactive/fe7178ddc75f4d81ac5e596b9948215498892/LIDARCOMP/LIDAR-DSM-1M-SP40nw.zip"

sp40ne_zip <- file.path(data_path, "LIDAR-DSM-1M-SP40ne.zip")
sp40nw_zip <- file.path(data_path, "LIDAR-DSM-1M-SP40nw.zip")

if (!dir.exists(file.path(data_path, "raw"))) {
  dir.create(file.path(data_path, "raw"))
}

raw_data_path <- file.path(data_path, "raw")

# --------------------------------------------------------------------------
# PLOT SETTINGS
# --------------------------------------------------------------------------

plot_size <- list(width = 1966, height = 1071)


# --------------------------------------------------------------------------
# FUNCTIONS
# --------------------------------------------------------------------------

#' Reads and combines the .asc LiDAR elevation files in the provided path
#'
#' @param path a string
#' @return a raster object
#' @examples
#' data_path <- "./data/lidar-dsm-1m-wytham/"
#' data <- combine.lidar(data_path)

combine.lidar <- function(path) {
  tmp <- 
    tibble(file = list.files(path, "*.asc$", full.names = T)) %>%
    mutate(raster = map(file, .f = ~ raster(readGDAL(.)))) %>% 
    pull(raster)
  tmp$fun <- mean
  tmp <- do.call(mosaic, tmp)
  return(tmp)
}
  
# --------------------------------------------------------------------------
#' Make and save texture for rayshader
#'
#' @param name a string
#' @param colors see documentation for `create_texture {rayshader}`
#' @return A color array and saves an example plot to the figure path in 
#' the project
#' @examples
#' make.texture(texture_tan, "#f5e5e4", "#1f1c1b", "#49453c", "#9e8e78", 
#' "#b7a690")

make.texture <-
  function(name = "name",
           lightcolor,
           shadowcolor,
           leftcolor,
           rightcolor,
           centercolor) {
    figname <- name
    name <-
      create_texture(lightcolor, shadowcolor, leftcolor, rightcolor, centercolor)
    jpeg(
      file = file.path(figures_path, paste0(figname, ".jpeg")),
      bg = NA,
      width = 200,
      height = 200,
      quality = 100
    )
    plot_map(name)
    position <- list(x=260, y=480)
    text(position,figname)
    dev.off()
    return(name)
  }

# --------------------------------------------------------------------------
# Make textures for this project - this should be moved to a separate
# script if/once the project becomes bigger

texture_tan <-
  make.texture(name = "texture_tan",
               "#ffeed4",
               "#1f1c1b",
               "#4b4f3a",
               "#5e5d4a",
               "#f5d8a9")

texture_green <-
  make.texture(name = "texture_green",
               "#fff3db",
               "#21211f",
               "#c6e88b",
               "#c6e88b",
               "#ffe2b3")

texture_gold <-
  make.texture(name = "texture_gold",
               "#ffe263",
               "#363827",
               "#96b078",
               "#51995b",
               "#fff0a8")

# --------------------------------------------------------------------------
# Camera parameter definitions
# view1 <- render_camera() # use this to capture parameters and later render
# plots programmatically

angle_1 <- c(99.4325894,  6.0341691,  0.25, 95.3149719)
names(angle_1) <- c("theta", "phi", "zoom", "fov")

angle_2 <-c(-7.782936e+00, 1.213982e+01, 9.303043e-04, 1.790000e+02)
names(angle_2) <- c("theta", "phi", "zoom", "fov")

angle_3 <-c(41.0940748, 14.7378151, 0.2046828, 0.0000000)
names(angle_3) <- c("theta", "phi", "zoom", "fov")

angle_4 <-c(235.51426770, 12.44281126, 0.05181892, 133.75991821)
names(angle_4) <- c("theta", "phi", "zoom", "fov")

angle_5 <-c(44.19499594, 5.27746353, 0.05253407, 124.90157318)
names(angle_5) <- c("theta", "phi", "zoom", "fov")

angle_6 <-c(160.11626215, 2.04849474, 0.03404747, 0)
names(angle_6) <- c("theta", "phi", "zoom", "fov")

# --------------------------------------------------------------------------
#' Renders scene in 3D 
#'
#' @param matrix A matrix of elevation data
#' @param zscale values < 1/red exaggerate height
#' @param texture an array with texture (color palette) for plot
#' @param sunaltitude in degrees of arc, from the horizon
#' @param sunangle in degrees of arc. 0 = North
#' @param cam_angle predefined camera parameters
#' @param plotsize c(width, height) of graphic output
#' @param title a string "with your plot title"
#' @return A interactive 3d visualisation (not saved)

render.scene <-
  function(matrix,
           exaggerate_z = TRUE, 
           # this makes everything 40% taller
           texture,
           sunaltitude,
           sunangle,
           windowsize = plot_size) {
    if (length(rgl::rgl.dev.list()) > 0) {
      rgl::rgl.close()
    }
    
    size <- unlist(plot_size)
    
    if (exaggerate_z == TRUE) {
      
      # this is ugly, but will have to do for now
      
      matrix %>%
        sphere_shade(texture = texture,
                     sunangle = sunangle,
                     colorintensity = 2) %>%
        add_shadow(
          ray_shade(
            matrix,
            zscale = 1 / red * 0.4,
            sunaltitude = sunaltitude,
            sunangle = sunangle,
            lambert = FALSE,
            multicore = TRUE,
          ),
          max_darken = 0.1
        ) %>%
        add_shadow(
          lamb_shade(
            matrix,
            zscale = 1 / red * 0.4,
            sunaltitude = sunaltitude,
            sunangle = sunangle
          ),
          max_darken = 0.2
        ) %>%
        # add_shadow(ambient_shade(matrix),
        #             max_darken = 0.3) %>%
        # plot_map() # Uncomment for 2d map
        plot_3d(
          matrix,
          zscale = 1 / red * 0.4,
          windowsize = size,
          baseshape = "circle",
          soliddepth = 30,
          solidcolor = "#080808",
          solidlinecolor = "#000000",
          background = "#3c3d40",
          shadowcolor = "#232324",
          linewidth = 1,
          shadowdepth = -25
        )
      Sys.sleep(2)
      
    } else {
      
      matrix %>%
        sphere_shade(texture = texture,
                     sunangle = sunangle,
                     colorintensity = 2) %>%
        add_shadow(
          ray_shade(
            matrix,
            zscale = 1 / red,
            sunaltitude = sunaltitude,
            sunangle = sunangle,
            lambert = FALSE,
            multicore = TRUE,
          ),
          max_darken = 0.1
        ) %>%
        add_shadow(
          lamb_shade(
            matrix,
            zscale = 1 / red,
            sunaltitude = sunaltitude,
            sunangle = sunangle
          ),
          max_darken = 0.2
        ) %>%
        # add_shadow(ambient_shade(matrix),
        #             max_darken = 0.3) %>%
        # plot_map() # Uncomment for 2d map
        plot_3d(
          matrix,
          zscale = 1 / red,
          windowsize = size,
          baseshape = "circle",
          soliddepth = 30 * 0.4,
          solidcolor = "#080808",
          solidlinecolor = "#000000",
          background = "#3c3d40",
          shadowcolor = "#232324",
          linewidth = 1,
          shadowdepth = -25 * 0.4
        )
      Sys.sleep(2)
    }
  }

# --------------------------------------------------------------------------
#' Histogram equalisation for a colour image
#' This function is based on code from the imager library
#' https://cran.r-project.org/web/packages/imager/vignettes
#' 
#' @param image a cimg object from imgr
#' @return an equalised cimg obje3ct

histEQ <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))
rgb.equalise <- function(image) {
  tmp_list <- imsplit(image,"c")
  tmp_list_eq <- map_il(tmp_list,histEQ)
  imappend(tmp_list_eq,"c")
}

# --------------------------------------------------------------------------
#' Save a snapshot with DOF (Depth Of Field) simulation
#' I (Nilo) have predefined some `cam_angle` values for the Wytham data,
#' add your own!
#'
#' @param cam_angle named numbers (theta, phi, zoom and field of view)
#' @param title character: plot title
#' @param focus number: distance to focal point
#' @param focallength number: focal lenth of virtual lens
#' @param fstop number: the 'aperture' od the lens
#' @param plotsize named number: width and height of saved plot
#' @param preview logical: wether to save focus preview or final plot
#' @return saves a .jpeg file

take.snapshot <- function(cam_angle,
                          title,
                          focus,
                          focallength,
                          fstop,
                          plotsize = plot_size,
                          preview = FALSE) {
  
  do.call(render_camera, as.list(cam_angle))

  if (preview == FALSE) {
    
    figname <- deparse(substitute(cam_angle))
    
    plot_params <-
      c(list(file = file.path(figures_path, paste0(figname, ".png")),
             bg = NA), plot_size)
    
    do.call(png, plot_params)
    
    render_depth(
      focus = focus,
      focallength = focallength,
      fstop = fstop,
      bokehshape = "hex",
      aberration = 0.15,
      vignette = 0.15,
      title_text = title,
      title_color = "#f2f2f2",
      title_size = 30,
      preview_focus = FALSE
    )
    
    dev.off()
    
    noise <-
      imnoise(
        x = plot_params$width,
        y = plot_params$height,
        cc = 1,
        mean = 8,
        sd = 5
      ) %>%
      isoblur(0.5) %>%
      add.colour(simple = TRUE)
    
    img <-
      image_read(path = file.path(figures_path, paste0(figname, ".png"))) %>%
      image_modulate(., brightness = 115, saturation = 110, hue = 99) %>%
      image_normalize() %>%
      magick2cimg()
    
    img_noise <- img  + .003 * noise
    
    cimg2magick(img_noise) %>%
      image_flop() %>%
      image_write(
        path = file.path(figures_path, paste0(figname, ".jpeg")),
        quality = 100,
        format = "jpeg"
      )
    
    file.remove(file.path(figures_path, paste0(figname, ".png")))
    
  } else {
    render_depth(
      focus = focus,
      focallength = focallength,
      fstop = fstop,
      bokehshape = "hex",
      aberration = 0.15,
      vignette = 0.15,
      title_text = title,
      title_color = "#f2f2f2",
      title_size = 30,
      preview_focus = TRUE
    )
  }
}

# --------------------------------------------------------------------------
# This function converts lat-long to desired coordinate system. 
# (C) Tyler Morgan-Wall @tylermorganwall

lat.long.reproject <- function(lat,long, epsg_code) {
  data = data.frame(long=long, lat=lat)
  coordinates(data) <- ~ long+lat
  proj4string(data) <- CRS("+init=epsg:4326")
  xy = data.frame(spTransform(data, CRS(paste0("+init=epsg:", epsg_code))))
  colnames(xy) = c("x","y")
  return(unlist(xy))
}

# --------------------------------------------------------------------------
# Define image size variables from the given bounding box coordinates.
# Adapted from Will Bishop @wcmbishop

get.img.size <- function(boundbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <-
    abs((boundbox@xmin - boundbox@xmax) / (boundbox@ymin - boundbox@ymax))
  # define dimensions
  img_width <-
    ifelse(aspect_ratio > 1, major_dim, major_dim * aspect_ratio) %>% round()
  img_height <-
    ifelse(aspect_ratio < 1, major_dim, major_dim / aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height,
       width = img_width,
       size = size_str)
}
