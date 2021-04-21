#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Deal with scientific notation causing issues with instrumentalness:
# Disable scientific notation in double numbers, because instrumentalness otherwise did not test plotting accurately (plotted at zero) and innaccuratelly acts as above 1 grid.max scale.
options(scipen = 999) # This worked well in our EDA notebook but does not seem to be solving the issue here in .R file.
#options(scipen = 100, digits = 4)
#?options
# We don't want to use sprintf because that would change to characters.
#?sprintf
# Let's try this solution below after data load, from https://stackoverflow.com/questions/9397664/force-r-not-to-use-exponential-notation-e-g-e10
#train_sample_10k = format(train_sample_10k,scientific=FALSE)

# Load packages
# Note quotes used in install of some packages below, becuase those otherwise would error and cannot be found and in RStudio packages list
library(shiny)
#library(ggplot2)
# ggradar works on ggplot2 to make a radar chart
#devtools::install_github("ricardo-bion/ggradar")
library("ggradar")
# ggiraph, ggiraphExtra add interactivity for ggradar plots/ggplot2
#install.packages("ggiraph")
#install.packages("ggiraphExtra")
library("ggiraph")
library("ggiraphExtra")
library(scales)
#install.packages("ggiraph")
library("ggiraph")
library("moonBook")
library(sjmisc)
library(reshape2)
#install.packages("Cairo"),
#library(Cairo) # For nicer ggplot2 output when deployed on Linux
library(tidyverse)
#library(dplyr)
#loaded this explicitly in addition to tidyverse on recommendation that sometimes pipe won't work with tidyversy loaded alone, although package is included with tidyverse.
library(lubridate)
library(sf)
library(leaflet)
library(htmltools)
library(DT)
#install.packages("fmsb")
#library(fmsb) #commented out due to rewriting radarchart in ggplot2, ggiraph
library(RColorBrewer)
# Loaded for assigning row name from track_id column:
library(magrittr)

# Load data  
# We'll use only plotted subset of the data set and columns
# so that it prints nicely
# Load data from .csv files.
track_features <- read_csv("../data/raw/track_features/tf_mini.csv")
# For debugging, test load was successful.
track_features

# Remove scientific notation
#track_features = format(track_features,scientific=FALSE)
#track_features

skips <- read_csv("../data/raw/training_set/log_mini.csv")
# counts of number of times each track (by ID) appears in training dataset and sort table by counts descending
skips %>%
  count(track_id_clean) %>%
  arrange(desc(n))
# sample data subset; add percent not skipped
track_plays_and_skips <- skips %>%
  group_by(track_id_clean) %>%
  summarise(
    plays = n(),
    # skip_1 = sum(skip_1),
    # skip_2 = sum(skip_2),
    # skip_3 = sum(skip_3),
    not_skipped = sum(not_skipped)
  ) %>%
  # Take only the tracks played 50 or more times.
  filter(plays >= 50) %>%
  # Add percentage of times each was not skipped of its total times played.
  mutate(not_skipped_pct = not_skipped / plays)
# For debugging, check to see if track_id_clean displays here.
#track_plays_and_skips

# Join features to plays_skips.
track_plays_skips_features <- track_plays_and_skips %>%
  left_join(track_features, by = c("track_id_clean" = "track_id"))
# For debugging, check to see if track_id_clean displays here.
#track_plays_skips_features

# Drop least skipped dataframe to keep only desired column variables.
# Note quotes are required around objects to call columns by name in this format.
# Note: let's keep "not_skipped_pct" which in ggplot2 and ggiraph we can call column as our tooltip aesthetic for hover or on click display in Shiny.
# Note: let's keep "track_id_clean" as our required data_id aesthetic for use with ggplot2 and ggiraph interactivity and because we want this as input for Spotify Developer API engpoints later.
drop <- c("plays",
          #"skip_1",
          #"skip_2",
          #"skip_3",
          # drop not_skipped_pct for now but ideally it could be in a tooltip later.
          "not_skipped_pct", 
          # Drop instrumentalness for now because debugging scientific notation has been unsuccessful for me.
          "instrumentalness", "not_skipped","dyn_range_mean","mode","acoustic_vector_0","acoustic_vector_1","acoustic_vector_2","acoustic_vector_3","acoustic_vector_4","acoustic_vector_5","acoustic_vector_6","acoustic_vector_7",
          # I will also drop the features not on a 0-1 scale for this interactive ggplot ggradar chart since otherwise we would need to rescale them to 0-1 and it would appear inaccurate I think.
          "us_popularity_est", "release_year", "duration", "time_signature", "tempo", "key", "loudness")
track_plays_skips_features1 = track_plays_skips_features[,!(names(track_plays_skips_features) %in% drop)]
# For debugging, check to see if track_id_clean displays here.
#track_plays_skips_features1


# Adjust the columns order for a more visually effective radar chart.
# for now ignore: "not_skipped_pct", #(would like to add back in as a tooltip, but it's erroring out because we're not plotting it on radar) 
# omit dropped columns due to scale besided 0-1 for now.
#"us_popularity_estimate", "release_year", "duration", "time_signature", "tempo", "key", "loudness", 
# Omit instrumentalness for now because debugging scientific notation has been unsuccessful for me. Note "instrumentalness" was between flatness and speechiness in col order.
# "instrumentalness", 
col_order <- c("track_id_clean", "acousticness", "beat_strength", "bounciness", "danceability", "liveness", "mechanism", "organism", "energy", "flatness", "speechiness", "valence")
tracks_plays_skips_radar <- track_plays_skips_features1[, col_order]
# For debugging, check the row names at this point.
#rownames(tracks_plays_skips_radar)
# For debugging, check to see if track_id_clean displays here.
#tracks_plays_skips_radar
# For debugging, check the first column in dataframe. We want it to be the track ID.
#tracks_plays_skips_radar[,1]

# Assign row names from 1st column, which contains "track_id_clean". This will not be plotted on radar chart, and will become group later.
# To resolve error on assigning track_id as rowname, switched to usin gmagrittr package syntax per this answer: https://stackoverflow.com/questions/46165105/error-in-adding-the-first-column-as-the-row-name-in-r
# For debugging reference, error was: Error in `.rowNamesDF<-`(x, value = value) : invalid 'row.names' length /n In addition: Warning message: Setting row names on a tibble is deprecated.
# Successful magritte syntax which first assigns it by column name to rownames, then also drops it from the columns (we overwrite the dataframe variable with this result.):
tracks_plays_skips_radar2 <-tracks_plays_skips_radar %>% data.frame %>% set_rownames(.$track_id_clean) %>% select(-track_id_clean)
# For debugging, view to confirm the result has track_id as rowname, and no longer as a column.
head(tracks_plays_skips_radar2)
# Or, this way: I just want to see the row names quick to confirm.
#row.names(tracks_plays_skips_radar2)
# Same.
#rownames(tracks_plays_skips_radar2)

# Let's try another way to assign rownames as column "group", and do it outside the pipe
tracks_plays_skips_radar2 <- rownames_to_column(.data = tracks_plays_skips_radar2, var = "group")
# For debugging, test this portion before continuing...
tracks_plays_skips_radar2
# For debugging, check group is here at this point.
#tracks_plays_skips_radar2$group

# For debugging, check before rescaling, the values of instrumentalness (Error: 'plot.data' contains value(s) > grid.max):
#tracks_plays_skips_radar2$instrumentalness

#?rescale

# For debugging need to make a reproducible example to get some help with this.
# for_dput <- head(tracks_plays_skips_radar2, 2)
# 
# dput(for_dput, file = "",
#      control = c("keepNA", "keepInteger", "niceNames", "showAttributes"))
# 
# dget(file, keep.source = FALSE)

# Isolate the least skipped tracks.
tracks_least_skipped_radar2 <- tracks_plays_skips_radar2 %>%
  # Commented out due to error (see instead moved above outside pipe as rownames_to_column): Error in UseMethod("tbl_vars") : 
  #no applicable method for 'tbl_vars' applied to an object of class "character"
  #rownames = "group" %>%
  mutate_at(vars(-group), rescale) %>%
  tail() %>%
  select(1:12)
tracks_least_skipped_radar2

# For debugging, check rescaled values of instrumentalness (Error: 'plot.data' contains value(s) > grid.max):
#tracks_least_skipped_radar2$instrumentalness

# Isolate most skipped tracks. 
# Most skipped commented out with fmsb package radarchart in debugging to troubleshoot Warning: Error in writeImpl: Text to be written must be a length-one character vector [No stack trace available]
#tracks_most_skipped_radar <- head(tracks_plays_skips_radar2)
#tracks_most_skipped_features

# set a color palette variable for some greens that will be understood as a relative group (of the least skipped), and remain somewhat visible as distinct tracks individually also.
palette_greens <- brewer.pal(8,"Greens")
# for the fill of polygons, let's add some transparency with alpha
palette_greens_in <- alpha(palette_greens,0.2)

# Define UI for application 
ui <- fluidPage(titlePanel("Sell Out Music Listener"),
  
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 6,
           h4("Points near click"),
           verbatimTextOutput("click_info")
    ),
    column(width = 6,
           h4("Brushed points"),
           verbatimTextOutput("brush_info")
    )
  )
)

# We are not getting interactive points, and need to research ggiraph, ggiraphExtra, girafe.

server <- function(input, output) {
  output$plot1 <- renderPlot({
    # First we will get the least skipped working in ggradar, and then we may return to plot the comparison most skipped beside it
    # Now we will plot the least and most skipped charts beside one another for comparison.
    #opar <- par()
    # Define settings for plotting in a 1x2 (2 column) grid, with appropriate margins:
    #par(mar = rep(.8,4)) #may require testing to get margins between plots looking good
    #par(mfrow = c(1,2)) # 1 row, 2 columns
    # Produce a radar-chart for each desired plot again, least skipped at left; most to right
    # Customized least skipped radar chart
    ggradar(
      tracks_least_skipped_radar2,
      # Add a plot title.
      plot.title = "Features: Least Skipped Songs",
      # Custom labels (removed for now any audio feature columns not on 0-1 scale: "US Popularity est. %", "Release Year", "Duration (ms)", "Time Signature", "Tempo", "Key", "Loudness", )
      # Omit "instrumentalness" for now because column was removed due to unsuccessful scientific notation troubleshooting.
      axis.labels = c("Acousticness", "Beat Strength", "Bounciness", "Danceability", "Liveness", "Mechanism", "Organism", "Energy", "Flatness", "Speechiness", "Valence"),
      axis.label.size = 3.5,
      # We could offset the axis labels further away from the max circle if they appear crowded, measured relative to circle diameter.
      #axis.label.offset = 2 # We'll test the default 1.15 first.
      # Note ggradar does not appear to have an axis color option, as we used "grey30" in fmsb (i.e. axislabcol = "grey30").
      values.radar = c("0", "0.5", "1"),
      # Grid line values (3 lines drawn)
      grid.min = 0, grid.mid = 0.5, grid.max = 1,
      # Polygons
      group.line.width = 0.8,
      group.point.size = 3,
      # Colors the lines of the polygon(s), let's generalize all of these 'good' i.e. least skipped as a similar range of color using a R palette, since we'll compare it to 'bad' most skipped, not among themselves as much.
      group.colours = palette_greens,
      # Adds same colors as fill to polygons with more transparency (not clear yet how to convert this from fmsb to ggradar).
      #pfcol = palette_greens_in, 
      # Background and grid lines (types and colors)
      background.circle.colour = "white", # Note, default is yellow in ggradar.
      grid.line.width = 0.8,
      gridline.min.linetype = "solid",
      gridline.mid.linetype = "solid",
      gridline.max.linetype = "solid", # Note, default is long dashed line in ggradar.
      # Note the min and max lines' color defaults are grey so we don't have to state them.
      gridline.mid.colour = "grey", # Default for the mid point line is blue, which I would find distracting in this case.
      # No legend desired.
      plot.legend = FALSE
      # Legend position (probably don't want a legend showing Track ID; we're concerned with these as a visual group of least skipped with some variation.)
      #legend.position = "bottom"
    )
    
    
    # Customized most skipped radar chart
    # How do we render a plot2? It would be good to show the visual comparison even if we don't program interaction with its points in the same way as with the least skipped plot1.
    # Most skipped commented out in debugging to troubleshoot Warning: Error in writeImpl: Text to be written must be a length-one character vector [No stack trace available]
   
  })
  # How can I supply xvar for my radar chart or otherwise make it interactive?
  output$click_info <- renderPrint({
    # Because radarchart is not a ggplot2 (which would not require us to supply xvar or yvar); if this
    # were a base graphics plot, we'd need those. Radar Chart is made in fmsb so to troubleshoot "Error in nearPoints: nearPoints: not able to automatically infer `xvar` from coordinfo",
    # let's try to to explicitly supply xvar and yvar, which are multiples for many radar axes.
    # Shiny nearpoints docs at: https://shiny.rstudio.com/reference/shiny/0.12.0/nearPoints.html
    nearPoints(tracks_least_skipped_radar2, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(tracks_least_skipped_radar2, input$plot1_brush)
  })
}
# Run the application.
shinyApp(ui, server)
