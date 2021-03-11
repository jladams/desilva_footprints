

# R script to generate contours from an STL file
# STL files represent a surface with a depression in it, rotated in Utilimaker Cura
#   which is a 3D printing software but useful for rotating and centering STL files around axes

# load the necessary libraries
library(tidyverse)  # tidyverse includes ggplot2
library(rgl)

# create a function that will take an input file and generate contours
# parameters are:  file name and n_bins (to experiment with the number of contour lines)
plot_prints <- function(input_file, n_bins = 30) {
  name <- substr(input_file, 1, nchar(input_file) - 4)
  input_file <- paste0("./data/processed/", input_file)
  output_image <- paste0("./images/", name , ".png")
  
  image_title <- paste0(name, " - Height (mm)")
  
  print(paste("Reading in", input_file))
  
  # load the STL file in to an x/y/z matrix of values
  df <- readSTL(input_file, plot = F)
  
  # move the data in to a "tibble," tidyverse's version of a data frame
  # round the values to 0 decimal places (mutate_all)
  # group the x and y values
  # average z values at overlapping x and y coordinates
  
  footprint <- tibble(x = df[, 1], y = df[, 2], z = df[, 3]) %>%
    mutate_all(round, 0) %>%
    group_by(x, y) %>%
    summarize(z = mean(z, na.rm = TRUE))
  
  # contour lines plot type
  print("Creating contour plot")
  
  p <- ggplot(footprint, aes(x = x, y = y)) +
    geom_contour(aes(z = z, color = stat(-level)), bins = n_bins) +
    scale_colour_distiller(
      type = "seq",
      palette = "Spectral",
      direction = -1,
      values = NULL,
      space = "Lab",
      na.value = "grey50",
      guide = guide_colorbar(reverse = TRUE),
      labels = function(x) {
        return(abs(x))
      },
      aesthetics = "colour"
    ) +
    coord_flip() +
    theme(
      plot.background = element_rect(fill = "black"),
      panel.background = element_rect(fill = "black"),
      panel.grid = element_blank(),
      legend.title = element_blank(),
      axis.text = element_blank()
    )
  
  p <- p + guides(fill = guide_legend("Height (mm)"))
  
  p <- p +
    labs(title = image_title) +
    theme(plot.title = element_text(color = "white"))
  
  # optionally, print to the screen in RStudio
  print(p)
  
  print(paste0("Saving file: ", output_image))
  
  # save the output to a file,
  # and set the image output units in cm, default was saving 8.75 x 6.72 in image
  
  ggsave(output_image,
         width = 24,
         height = 24,
         units = "cm")
  
  return(p)
  
}

# Create the plots for each stl file
plot_prints(input_file = "LaetoliA_Print1.stl", n_bins = 30)
plot_prints(input_file = "LaetoliA_Print2.stl", n_bins = 30)
plot_prints(input_file = "LaetoliA_Print3.stl", n_bins = 30)
plot_prints(input_file = "LaetoliA_Print4a.stl", n_bins = 30)
plot_prints(input_file = "LaetoliA_Print5.stl", n_bins = 30)
plot_prints(input_file = "LaetoliA_Westprint1.stl", n_bins = 30)
plot_prints(input_file = "LaetoliA_Westprint2.stl", n_bins = 30)
