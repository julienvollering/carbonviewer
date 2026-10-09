#######################################
# DO THE INTERPOLATION OF THE DATASET #
#######################################

compute_volume_slider <- function(peatDepths,
                                  peatlandDelimination,
                                  power){
  
  myGrid <- starsExtra::make_grid(peatlandDelimination, 1)
  myGrid <- sf::st_crop(myGrid, peatlandDelimination)
  
  vol <- gstat::idw(torvdybde_cm ~ 1, peatDepths, 
                             newdata=myGrid, 
                             nmax=30, 
                             idp=power)

  return(sum(vol$var1.pred, na.rm=T))
}

#' Interpolation of peat depths
#'
#' Interpolation of peat depths using Inverse Distance Weighting (IDW). 
#' The optimal power value is determined by minimizing the Mean Absolute Error (MAE) 
#' using cross-validation (IDW vs. kriging).
#' 

#' @param peatDepths peat depth points.
#' @param peatlandDelimination study area polygon.
#' @param powerRange range of power values: Default = 1:6.
#' @param nmax max number of points to use in IDW and kriging. Default = 30.
#'
#' @return A list containing the following elements:
#' - v: volume of peat in the study area.
#' - idweights: A Spatial object containing the interpolated values.
#' - best: The best power value.
#' - gg_out: ggplot showing MAE against power.
#' - gg_out_vol: A ggplot showing relative volume against power.

interpolation <- function(peatDepths,
                          peatlandDelimination,
                          powerRange = 1:6,
                          nmax = 30){
  
  temp <- data.frame(power = powerRange,
                     MAE = as.numeric(NA))
  
  peatDepths <- as(peatDepths, "Spatial")
  
  vol <- NULL
  
  myGrid <- starsExtra::make_grid(peatlandDelimination, 1)
  myGrid <- sf::st_crop(myGrid, peatlandDelimination)
  
  withProgress(message = i18n$t("Beregn volum"), {
    for(i in powerRange){

      # Get the MAE
      temp2 <- gstat::krige.cv(torvdybde_cm ~ 1, peatDepths, set = list(idp=i), nmax = nmax)
      temp$MAE[temp$power==i] <- mean(abs(temp2$residual))
      
      #  Get the volume
      vol_temp <- gstat::idw(torvdybde_cm ~ 1, peatDepths, 
                             newdata=myGrid, 
                             nmax=nmax, 
                             idp=i)
      
      # vol in m3
      vol <- c(vol, sum(vol_temp$var1.pred, na.rm=T))
      
      Sys.sleep(0.5)
      incProgress(1 / length(powerRange))
      }
  })
  
  ifelse(temp$power[which.min(temp$MAE)]<2,
         temp$best <- ifelse(temp$power==2, "best", "not-best"),
         temp$best <- ifelse(temp$MAE==min(temp$MAE), "best", "not-best")
  )
  
  best <- temp %>% filter(best == "best")
  best <- best$power
  

  ###############
  # Plot volume #
  ###############
  vol_df <- data.frame("volume" = vol,
                       "power" = powerRange)
  vol_df$relative_volume <- vol_df$volume/mean(vol_df$volume)*100
  v <- vol_df %>% filter(power == best)
  v <- v$volume
  v_m3 <- v/100  
  
  idweights <- gstat::idw(formula = torvdybde_cm ~ 1, 
             locations = peatDepths, 
             newdata = myGrid, 
             idp=best,
             nmax = nmax)
  
  ######################
  # Plot Power results #
  ######################
  # Plot MAE vs power
  gg_out <- ggplot(temp, aes(x = power, y = MAE,
                            colour = best,
                            shape = best))+
    geom_point(size=6)+
    theme_bw(base_size = 14)+
    scale_x_continuous(breaks = powerRange)+
    guides(colour="none",
          shape = "none")+
    scale_color_manual(values = c("darkgreen","grey"))+
    scale_shape_manual(values = c(18, 19))+
    labs(title = "Comparison of Mean Absolute Error (MAE) for Different Power Values in IDW",
        subtitle = "The optimal power value is visualized by the green diamond.",
        x = "Power",
        y = "Mean Absolute Error (MAE)")

  # Plot relative volume vs power
  gg_out_vol <- ggplot(vol_df, aes(x = factor(power), y = relative_volume))+
    geom_point(size=6)+
    theme_bw(base_size = 14)+
    labs(title = "Impact of Power Parameter on Estimated Peat Volume",
       x = "Power",
       y = "Peat volume as a percentage of\nmean predicted peat volume")

  # Return the interpolation and the result volume
  l_results <- list(v_m3, idweights, best, gg_out, gg_out_vol)
  return(l_results)
}

###############################################
# COMPUTE CARBON STOCKS IF USING BASE DATASET #
###############################################

ccalc_cStocks <- function(volume,
                          perc_SOM_mean,
                          BD_mean){
  temp_stocks <- NULL
  
  for(i in 1:1000){
    temp <-   volume * 
      mean(sample(perc_SOM_mean, replace = T) / 100) * 
      mean(sample(BD_mean, replace = T) * 0.5)
    
    temp_stocks <- c(temp_stocks, temp)
  }
  l_results <- list(mean(temp_stocks), sd(temp_stocks))
  return(l_results)
}

#######################
# SOME ERROR HANDLING #
#######################

# Error message when checking if the .zip file contain the necessary files
print_no_shp <- function(){
  showModal(modalDialog(
    title = "SHP input error",
    "The uploaded .zip file does not contain any .shp file",
    easyClose = TRUE
  ))
}

print_no_prj <- function(){
  showModal(modalDialog(
    title = "SHP input error",
    "The uploaded .zip file does not contain any .prj file containing the 
    information on the Coordinate Reference System",
    easyClose = TRUE
  ))
}

print_no_csv <- function(){
  showModal(modalDialog(
    title = "CSV input error",
    "The uploaded .zip file does not contain any .csv file containing the coordinates and the depth of the sampled sites",
    easyClose = TRUE
  ))
}

print_error_incompatible_file <- function(){
  showModal(modalDialog(
    title = "Input error",
    "Please upload a dataset before clicking on 'load dataset'. 
          The dataset should be a zip file containing both a shapefile with the 
          extent of the area of interest and a csv file containing peat depth measures 
          (in m) taken at the site with coordinates for each measure (given in UTM 33 N, EPSG:25833). 
          For more information refer to the README.",
    easyClose = TRUE
  ))
}

print_error_csv_columns <- function(){
  showModal(modalDialog(
    title = "CSV input error",
    "The CSV file uploaded is not formatted correctly. Please make sure that it contains at least 
    the coordinates (x and y) and the depth of the sampled sites (torvdybde_cm). In any doubts, refer to the README.",
    easyClose = TRUE
  ))
}

print_corrupt_shp <- function(){
  showModal(modalDialog(
    title = "SHP input error",
    "The SHP file uploaded is corrupt. Please make sure that it contains the polygon(s) of the area of interest.
          In any doubts, refer to the README. The session is reloaded.",
    easyClose = TRUE
  ))
}

# Error message for a problem found by read_dataset_zip()
print_dataset_error <- function(error){
  switch(error,
         no_shp = print_no_shp(),
         no_prj = print_no_prj(),
         no_csv = print_no_csv(),
         corrupt_shp = print_corrupt_shp(),
         print_error_csv_columns())
}

# Error message if files are not opened properly

open_shapefile <- function(filename){
  
    #shp <- readOGR(filename) %>% st_as_sf()
    shp <- st_read(filename)  
  
    if (length(shp) == 0){
      showModal(modalDialog(
        title = "SHP input error",
        "The SHP file uploaded is invalid. Please make sure that it contains the polygon(s) of the area of interest. 
            In any doubts, refer to the README.",
        easyClose = TRUE
      ))
    }
    else {
      return(shp)
    }
}

open_csv <- function(filename) {
  tryCatch({
      df <- fread(filename)
      return(df)
    }, error = function(e) {})
  stop("Could not open the file")
}


transform_to_sf <- function(df) {
  coord_columns <- list(c("X", "Y"), c("x", "y"))
  for (cols in coord_columns) {
    tryCatch({
      sf_df <- st_as_sf(df, coords = cols)
      return(sf_df)
    }, error = function(e) {})
  }
  stop("Could not transform the file with any of the following coordinates: ", cols)
}

####################
# READ THE DATASET #
####################

#' Read an uploaded dataset: a zip with the area polygon(s) (.shp + .prj) and a
#' CSV with the peat depth points (x, y and torvdybde_cm or peat_depth_cm)
#'
#' Used by the volume calculation and by the "Contribute data" module, which
#' reads the zip as soon as it is uploaded.
#'
#' @param zip_path path of the zip file (NULL: only read what is already in exdir).
#' @param exdir folder to unzip into.
#' @return list(shape, points), both in EPSG:25833, or list(error = code) where
#'   code is "no_shp", "no_prj", "no_csv", "corrupt_shp" or "bad_csv".
read_dataset_zip <- function(zip_path, exdir, target_crs = 25833){
  if (!is.null(zip_path)) unzip(zip_path, exdir = exdir)
  print(list.files(exdir, recursive = TRUE))

  shp_file <- list.files(exdir, pattern = '.shp$', recursive = TRUE)
  prj_file <- list.files(exdir, pattern = '.prj$', recursive = TRUE)
  csv_file <- list.files(exdir, pattern = '.csv$', recursive = TRUE)

  # Check if there is both a shapefile and a CSV
  if (length(shp_file) == 0) return(list(error = "no_shp"))
  if (length(prj_file) == 0) return(list(error = "no_prj"))
  if (length(csv_file) == 0) return(list(error = "no_csv"))

  shp <- tryCatch(st_read(paste0(exdir, "/", shp_file)), error = function(e) NULL)
  if (is.null(shp)) return(list(error = "corrupt_shp"))

  # If the shapefile has no CRS, we assume it is EPSG:25833 as specified in the
  # README. The points are in the same CRS as the shapefile.
  input_crs <- st_crs(shp)
  if (is.na(input_crs)) {
    print("The shapefile does not contain a projection (CRS)--> assigning the default CRS 'EPSG:25833'")
    shp <- shp %>% st_set_crs(target_crs)
    input_crs <- st_crs(target_crs)
  }
  else {
    print("The shapefile has a projection (CRS) --> Converting to 'EPSG:25833'")
    shp <- shp %>% st_transform(target_crs)
  }

  df <- tryCatch(open_csv(paste0(exdir, "/", csv_file)), error = function(e) NULL)
  if (is.null(df)) return(list(error = "bad_csv"))
  names(df) <- tolower(names(df))

  # If name is peat_depth_cm change to torvdybde_cm
  if ("peat_depth_cm" %in% names(df)){
    names(df)[names(df) == "peat_depth_cm"] <- "torvdybde_cm"
  }

  # Check that the CSV contains the necessary columns (x,y, torvdybde_cm)
  necessary_columns <- c("x","y", "torvdybde_cm")
  if (sum(names(df) %in% necessary_columns) != 3) return(list(error = "bad_csv"))
  print(head(df))

  points <- tryCatch(transform_to_sf(df) %>% st_set_crs(input_crs) %>% st_transform(target_crs),
                     error = function(e) NULL)
  if (is.null(points)) return(list(error = "bad_csv"))

  list(shape = shp, points = points)
}







