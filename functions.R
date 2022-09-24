## HARUKA TAKAGI
## DATE: NOVEMBER 1ST, 2020

# SETUP
setup <- function(){
  library(imager)
  library(tidyverse)
  library(tidymodels)
  library(sp)
  library(scales)
  library(cowplot)
  #devtools::install_github("sharlagelfand/dmc")
  library(dmc)
  set.seed(1234)
}

# CHANGE RESOLUTION FUNCTION
change_resolution <- function(image_df, x_size)
{
  ## change_resolution(image_df, x_size) subsamples an image to produce
  ## a lower resolution image. Any non-coordinate columns in the data
  ## frame are summarized with their most common value in the larger
  ## grid cell.
  ##
  ## Input:
  ## - image_df: A data frame in wide format. The x-coordinate column MUST
  ##             be named 'x' and the y-coordinate column MUST be named 'y'.
  ##             Further columns have no naming restrictions.
  ## - x_size:   The number of cells in the x-direction. The number of cells
  ##             in the vertical direction will be computed to maintain the 
  ##             perspective. There is no guarantee that the exact number
  ##             of cells in the x-direction is x_size
  ##
  ## Output:
  ## - A data frame with the same column names as image_df, but with fewer 
  ##   entries that corresponds to the reduced resolution image.
  ##
  ## Example:
  ##   library(imager)
  ##   library(dplyr)
  ##   fpath <- system.file('extdata/Leonardo_Birds.jpg',package='imager') 
  ##   im <- load.image(fpath)
  ##   im_dat<- as.data.frame(im,wide = "c") %>% rename(R = c.1, G = c.2, B = c.3) %>%
  ##            select(x,y,R,G,B)
  ##   agg_image <- change_resolution(im_dat, 50)
  
  if(!require(sp)) {
    stop("The sp packages must be installed. Run install.packages(\"sp\") and then try again.")
  }
  if(!require(dplyr)) {
    stop("The dplyr packages must be installed. Run install.packages(\"dplyr\") and then try again.")
  }
  
  sp_dat <- image_df 
  gridded(sp_dat) = ~x+y
  
  persp = (gridparameters(sp_dat)$cells.dim[2]/gridparameters(sp_dat)$cells.dim[1])
  y_size = floor(x_size*persp)
  orig_x_size = gridparameters(sp_dat)$cells.dim[1]
  orig_y_size = gridparameters(sp_dat)$cells.dim[2]
  
  x_res = ceiling(orig_x_size/x_size)
  y_res = ceiling(orig_y_size/y_size)
  
  gt = GridTopology(c(0.5,0.5), c(x_res, y_res),
                    c(floor(orig_x_size/x_res), floor(orig_y_size/y_res)))
  SG = SpatialGrid(gt)
  agg = aggregate(sp_dat, SG, function(x) names(which.max(table(x)))[1] )
  agg@grid@cellsize <- c(1,1)
  df <- agg %>% as.data.frame %>% rename(x = s1, y = s2)  %>% select(colnames(image_df))
  
  return(df)
  
}


# FUNCTION ONE: process_image
process_image <- function(image_file_name, k_list)
  {
  ## process_image(image_file_name, k_list) conducts kmeans clustering on the
  ## image file inputted. It will create kmeans meta data neccesary to create
  ## scree plot and visualizations of the clustering.
  ##
  ## Input:
  ## - image_file_name: a string of the file path of the image that will be 
  ##                    clustered. This file path can be shortened if a working
  ##                    directory is set.
  ## - k_list : a numerical value of the number of centers in the clustering.
  ##            This is determined by the human eye, count the number of colors
  ##            in the input image for reference.
  ##
  ## Output:
  ## - A list of the original output of the kclust calls, data on the tidied
  ## clusters, their associated RGB values, and their nearest DMC thread color
  ## information (name, hex, dmc RGB included as well.) Run View(cluster_info)
  ## for more details.
  ##
  ## Examples:
  ##   cluster_info <- process_image('/crossstitchimage.jpeg', 4)
  ##   cluster_info <- process_image('/Users/harukatakagi/Desktop/AS1/crossstitchimage.jpeg', 4)

  setup()
  im <- imager::load.image(image_file_name)
  tidy_dat <- as.data.frame(im, wide = "c") %>% rename(R = c.1, G = c.2, B = c.3)
  dat <- select(tidy_dat,c(-x,-y))
  k <- k_list
  kclust <- kmeans(select(tidy_dat, -x,-y), centers = k, nstart = 20)
  centres <- tidy(kclust)
  centres <- centres %>% mutate(col = rgb(R,G,B))
  
  dmc <- c()
  dmc_name <- c()
  hex <- c()
  dmc_red <- c()
  dmc_green <- c()
  dmc_blue <- c()
  
  for (i in 1:k){
    color_hex <- centres$col[i]
    dmc_output <- dmc(color_hex)
    dmc <- c(dmc, dmc_output[1])
    dmc_name <- c(dmc_name, dmc_output[2])
    hex <- c(hex, dmc_output[3])
    dmc_red <- c(dmc_red, dmc_output[4])
    dmc_green <- c(dmc_green, dmc_output[5])
    dmc_blue <- c(dmc_blue, dmc_output[6])
  }
  
  dmc <- as.numeric(unlist(dmc, use.names=FALSE))
  dmc_name <- unlist(dmc_name, use.names=FALSE)
  hex <- unlist(hex, use.names=FALSE)
  dmc_red <- as.numeric(unlist(dmc_red, use.names=FALSE))
  dmc_green <- as.numeric(unlist(dmc_green, use.names=FALSE))
  dmc_blue <- as.numeric(unlist(dmc_blue, use.names=FALSE))
  
  centres$dmc <- dmc
  centres$dmc_name <- dmc_name
  centres$hex <- hex
  centres$dmc_red <- dmc_red
  centres$dmc_green <- dmc_green
  centres$dmc_blue <- dmc_blue
  
  # combine centres(tidied clusters, RGB values, dmc thread color), kclust and dat output together
  tidied_centres_dmc <- centres
  clustering_output <- c(tidied_centres_dmc, kclust)
  clustering_output[['dat']] <- dat
  clustering_output[['tidy_dat']] <- tidy_dat
  clustering_output[['kclust']] <- kclust
  
  return(clustering_output)
}


# FUNCTION TWO: scree_plot()
scree_plot <- function(cluster_info)
  {
  ## scree_plot(cluster_info) plots two scree plots to determine the most
  ## optimal k values of the image clustering. The first scree plot of the
  ## total within-cluster sum of squares according to the k value. The second
  ## scree plot is the ratio version of the total within-cluster sum of
  ## squares according to the k value.
  ## 
  ## Input:
  ## - cluster_info:A list of the original output of the kclust calls, data on 
  ##                the tidied clusters, their associated RGB values, and their 
  ##                nearest DMC thread color information (name, hex, dmc RGB 
  ##                included as well.) Run View(cluster_info) for more details.
  ##
  ## Output:
  ## - Two scree plots showing the relationship between the total
  ##   within-cluster sum of squares and the k-value. One the plots is a ratio
  ##   version of the other one. 
  ##
  ## Example:
  ##   cluster_info <- process_image('/crossstitchimage.jpeg', 4)
  ##   scree_plots <- scree_plot(cluster_info)
  ##   scree_plots
  
  setup()
  set.seed(1234)
  upper_k <- length(cluster_info$size) + 5
  kclusts <- tibble(k = c(2:upper_k)) %>% mutate(kclust = map(k, ~kmeans(x = cluster_info$dat,
                                                                         centers = .x, nstart=4)),
                                                 glanced = map(kclust, glance),)
  clusterings <- kclusts %>% unnest(cols = c(glanced))
  
  # First scree plot
  first_scree <- ggplot(clusterings, aes(k, tot.withinss)) +
    geom_line() +
    geom_point() +
    ggtitle("Scree Plot One") +
    xlab("Number of clusters (k-value)") + ylab("Total within-cluster sum of squares")
  
  # Second ratio scree plot
  nclust = length(clusterings$k)
  ratio = rep(NA, nclust-1)
  for (kk in 2:nclust) {
    ratio[kk-1] = clusterings$tot.withinss[kk]/clusterings$tot.withinss[kk-1]}
  plot_data <- data.frame(k = clusterings$k[2:nclust],ratio)
  second_scree <- ggplot(plot_data, aes(x=k, y = ratio)) + geom_line() +
    ggtitle("Scree Plot Two") +
    xlab("Number of clusters (k-value)") + ylab("Ratio of Total within-cluster sum of squares")
  
  return(list(first_scree, second_scree))
}


# FUNCTION THREE: color_strip()
color_strips <- function(cluster_info)
  {
  ## color_strips(cluster_info) produces the color strips with the dmc colour
  ## closest to the cluster centre colour. It is a visualization useful when
  ## checking the which colors have been picked up by the kmeans clustering.
  ##
  ## Input:
  ## - cluster_info:A list of the original output of the kclust calls, data on 
  ##                the tidied clusters, their associated RGB values, and their 
  ##                nearest DMC thread color information (name, hex, dmc RGB 
  ##                included as well.) Run View(cluster_info) for more details.
  ##
  ## Output:
  ## - A NULL Object is outputted from this function. A display showing the
  ##   the corresponding dmc thread color hex's and their colors displayed in a 
  ##   grid is also outputted. 
  ##
  ## Example:
  ##   cluster_info <- process_image('/crossstitchimage.jpeg', 4)
  ##   color_strips(cluster_info)
  
  setup()
  set.seed(1234)
  strip <- show_col(cluster_info$col)
  return(strip)
}

# FUNCTION FOUR: make_pattern()
make_pattern <- function(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL)
  {
  ## make_pattern(cluster_info, k, x_size, black_white = FALSE, background_colour = NULL)
  ## produces a cross stitch pattern of the input image, using kmeans clustering. 
  ## Able to produce the cross stitch with color and in black and white, with the 
  ## ability to remove background colors from the plot as well.
  ##
  ## Input:
  ## - cluster_info:A list of the original output of the kclust calls, data on 
  ##                the tidied clusters, their associated RGB values, and their 
  ##                nearest DMC thread color information (name, hex, dmc RGB 
  ##                included as well.) Run View(cluster_info) for more details.
  ## - k : a numerical value indicating the number of kmeans clusterings that 
  ##       should be executed.
  ## - x-size: a numerical value of the total number of possible stitches in
  ##           the horizontal direction.
  ## - black_white: a boolean logical value, if true, print the pattern in
  ##   black and white. If false, print in colour (default value).
  ## - background_colour: A string or a NULL value. The string hex value of the 
  ##                      background that should not be stitched in the
  ##                      pattern. The default value is NULL, this will take
  ##                      into account all colors. 
  ##
  ## Output:
  ## - A ggplot visualization of the image clustering executed on the input 
  ##   image.The visualization is cross-stitch pattern, with the colors and 
  ##   shapes shown on the legend. 
  ##
  ## Example:
  ##   cluster_info <- process_image('/crossstitchimage.jpeg', 4)
  ##   make_pattern(cluster_info, 4, 75)
  ##   make_pattern(cluster_info, 4, 75, black_white = TRUE)
  ##   make_pattern(cluster_info, 4, 75, black_white = FALSE, 
  ##               background_colour = "#C72B3B")
  ##   make_pattern(cluster_info, 4, 75, black_white = TRUE, 
  ##   background_colour = "#C72B3B")
  
  setup()
  set.seed(1234)
  image_df <- cluster_info$tidy_dat
  new_image <- change_resolution(image_df, x_size)
  cluster_info[['low_res_tidy_dat']] <- new_image
  low_res_kclust <- kmeans(select(cluster_info$low_res_tidy_dat, -x,-y), centers = k, nstart = 20)
  cluster_info[['low_res_kclust']] <- low_res_kclust
  tidy_dat2 <- augment(cluster_info$low_res_kclust, cluster_info$low_res_tidy_dat) %>% rename(cluster = .cluster)
  
  hex_vector <- c("#6A859E", "#EED3C4", "#C72B3B", "#1E1108")
  name_vector <- c("Antique Blue - Medium", "Desert Sand - Light", "Red", "Black Brown")
  
  if (is.null(background_colour)){
    i = 0
    hex_vector <- hex_vector
  } else {
    for (i in 1:k){
      if (hex_vector[i] == background_colour){
        hex_vector[i] <- "#FFFFFF"
        name_vector[i] <- "Background NULL"
      }
    }
  }
  
  if (black_white == TRUE){
    i = 0
    for (i in 1:k){
      if (hex_vector[i] == "#FFFFFF"){
        hex_vector[i] <- "#FFFFFF"
      } else {
        hex_vector[i] <- '#000000'
      }
    }
  }
  
  if (black_white == FALSE){
    alpha <- ggplot(tidy_dat2, aes(x=x, y=y)) +
      geom_point(aes(col = factor(cluster),
                     shape = factor(cluster))) +
      scale_colour_manual(name = 'DMC Colors',
                          values = hex_vector,
                          label = name_vector) +
      scale_shape_manual(name = 'DMC Colors',
                         values=c(0, 1, 2, 3),
                         label = name_vector) + 
      scale_y_reverse() + 
      theme_void() +
      background_grid(major = 'xy', color.major = "black")
    return(alpha)
    
  } else {
    beta <- ggplot(tidy_dat2, aes(x=x, y=y)) +
      geom_point(aes(col = factor(cluster), 
                     shape = factor(cluster))) +
      scale_colour_manual(name = 'DMC Colors',
                          values = hex_vector,
                          label = name_vector) +
      scale_shape_manual(name = 'DMC Colors',
                         values=c(0, 1, 2, 3),
                         label = name_vector) + 
      scale_y_reverse() + 
      theme_void() +
      background_grid(major = 'xy', color.major = "black")
    
    return(beta)
  }
}
