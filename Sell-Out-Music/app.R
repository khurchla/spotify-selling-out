#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Sell Out Music"),
    library(ggplot2),
#    install.packages("Cairo"),
#    library(Cairo),   # For nicer ggplot2 output when deployed on Linux
    library(tidyverse),
    library(dplyr), #loaded this explicitly in addition to tidyverse on recommendation that sometimes pipe won't work with tidyversy loaded alone, although package is included with tidyverse.
    library(lubridate),
    library(sf),
    library(leaflet),
    library(htmltools),
    library(DT),
    # Note quotes used in install of package below, becuase it otherwise would error and cannot be found and in RStudio packages list
    install.packages("fmsb"),
    library(fmsb),
    library(RColorBrewer),
    
    # We'll use only plotted subset of the data set and columns
    # so that it prints nicely
    # Load data from .csv files.
    track_features <- read_csv("../data/raw/track_features/tf_mini.csv"),
    skips <- read_csv("../data/raw/training_set/log_mini.csv"),
    # counts of number of times each track (by ID) appears in training dataset and sort table by counts descending
    skips %>%
      count(track_id_clean) %>%
      arrange(desc(n)),
    # sample data subset; add percent not skipped
    track_plays_and_skips <- skips %>%
      group_by(track_id_clean) %>%
      summarise(
        plays = n(),
        skip_1 = sum(skip_1),
        skip_2 = sum(skip_2),
        skip_3 = sum(skip_3),
        not_skipped = sum(not_skipped)
      ) %>%
      # Take only the tracks played 50 or more times.
      filter(plays >= 50) %>%
      # Add percentage of times each was not skipped of its total times played.
      mutate(not_skipped_pct = not_skipped / plays),
    # Join features to plays_skips.
    track_plays_skips_features <- track_plays_and_skips %>% 
      left_join(track_features, by = c("track_id_clean" = "track_id")),
    
    # Isolate the least skipped tracks.
    tracks_least_skipped_features <- tail(track_plays_skips_features),
    tracks_least_skipped_features,
    # Drop least skipped dataframe to keep only desired column variables.
    # Note quotes are required around objects to call columns by name in this format.
    drop <- c("not_skipped_pct","track_id_clean","plays","skip_1","skip_2","skip_3","not_skipped","dyn_range_mean","mode","acoustic_vector_0","acoustic_vector_1","acoustic_vector_2","acoustic_vector_3","acoustic_vector_4","acoustic_vector_5","acoustic_vector_6","acoustic_vector_7"),
    tracks_least_skipped_radar = tracks_least_skipped_features[,!(names(tracks_least_skipped_features) %in% drop)],

    # Add min and max to data frame as first 2 rows for range.
    max_min <- data.frame(
      duration = c(2000.0, 0.0), release_year = c(2021, 1950), us_popularity_estimate = c(100.0, 0.0), acousticness = c(1.0, 0.0), beat_strength = c(1.0, 0.0), bounciness =  c(1.0, 0.0), danceability = c(1.0, 0.0), energy = c(1.0, 0.0), flatness = c(1.0, 0.0), instrumentalness = c(1.0, 0.0), key = c(11, 0), liveness = c(1.0, 0.0), loudness = c(-60.0, 0.0), mechanism = c(1.0, 0.0), organism = c(1.0, 0.0), speechiness = c(1.0, 0.0), tempo = c(220.0, 0.0), time_signature = c(5, 0), valence = c(1.0, 0.0)
    ),
    rownames(max_min) <- c("Max", "Min"),
    # Bind the variable ranges to the data.
    tracks_least_skipped_radar <- rbind(max_min, tracks_least_skipped_radar),
    # Display the dataframe result.
    tracks_least_skipped_radar,
    # Adjust the columns order for a more visually effective radar chart.
    col_order <- c("us_popularity_estimate", "release_year", "duration", "time_signature", "tempo", "key", "loudness", "acousticness", "beat_strength", "bounciness", "danceability", "liveness", "mechanism", "organism", "energy", "flatness", "instrumentalness", "speechiness", "valence"),

    tracks_least_skipped_radar2 <- tracks_least_skipped_radar[, col_order],
    tracks_least_skipped_radar2,
    
    # Isolate most skipped tracks.
    tracks_most_skipped_features <- head(track_plays_skips_features),
    tracks_most_skipped_features,
    # Remove unnecessary/non-quantitative columns
    drop <- c("not_skipped_pct","track_id_clean","plays","skip_1","skip_2","skip_3","not_skipped","dyn_range_mean","mode","acoustic_vector_0","acoustic_vector_1","acoustic_vector_2","acoustic_vector_3","acoustic_vector_4","acoustic_vector_5","acoustic_vector_6","acoustic_vector_7"),
    tracks_most_skipped_radar = tracks_most_skipped_features[,!(names(tracks_most_skipped_features) %in% drop)],
    # add the max and min to data frame, same value ranges as in the least skipped tracks
    max_min <- data.frame(
      duration = c(2000.0, 0.0), release_year = c(2021, 1950), us_popularity_estimate = c(100.0, 0.0), acousticness = c(1.0, 0.0), beat_strength = c(1.0, 0.0), bounciness =  c(1.0, 0.0), danceability = c(1.0, 0.0), energy = c(1.0, 0.0), flatness = c(1.0, 0.0), instrumentalness = c(1.0, 0.0), key = c(11, 0), liveness = c(1.0, 0.0), loudness = c(-60.0, 0.0), mechanism = c(1.0, 0.0), organism = c(1.0, 0.0), speechiness = c(1.0, 0.0), tempo = c(220.0, 0.0), time_signature = c(5, 0), valence = c(1.0, 0.0)
    ),
    rownames(max_min) <- c("Max", "Min"),
    # Bind the variable ranges to the data.
    tracks_most_skipped_radar <- rbind(max_min, tracks_most_skipped_radar),
    tracks_most_skipped_radar,
    # Reorder the columns for radar sequence.
    col_order <- c("us_popularity_estimate", "release_year", "duration", "time_signature", "tempo", "key", "loudness", "acousticness", "beat_strength", "bounciness", "danceability", "liveness", "mechanism", "organism", "energy", "flatness", "instrumentalness", "speechiness", "valence"),
    tracks_most_skipped_radar2 <- tracks_most_skipped_radar[, col_order],
    tracks_most_skipped_radar2,
    
    
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

server <- function(input, output) {
  output$plot1 <- renderPlot({
    # Now we will plot the least and most skipped charts beside one another for comparison.
    opar <- par() 
    # Define settings for plotting in a 1x2 (2 column) grid, with appropriate margins:
    par(mar = rep(.8,4)) #may require testing to get margins between plots looking good
    par(mfrow = c(1,2)) # 1 row, 2 columns
    # Produce a radar-chart for each desired plot again, least skipped at left; most to right
    # Customized least skipped radar chart
    radarchart(tracks_least_skipped_radar2, axistype = 3,
               #add a title
               title = "Features of Least Skipped Spotify Tracks",
               #custom polygon, let's generalize all of these 'good' i.e. least skipped as a similar range of color using a R palette, since we'll compare it to 'bad' most skipped, not among themselves as much
               pcol = palette_greens, #colors the lines
               pfcol = palette_greens_in, #adds same colors as fill to polygons
               
               #custom the grid
               cglcol = "grey", cglty = 1, axislabcol = "grey30", cglwd = 0.8,
               
               #custom labels
               vlabels=c("US Popularity est. %", "Release Year", "Duration (ms)", "Time Signature", "Tempo", "Key", "Loudness", "Acousticness", "Beat Strength", "Bounciness", "Danceability", "Liveness", "Mechanism", "Organism", "Energy", "Flatness", "Instrumentalness", "Speechiness", "Valence"),
               vlcex = 0.635,
               #plot just a zero in center because all scales share min as 0
               caxislabels=c("0", "", "", "", "")
    )
    # Customized most skipped radar chart
    radarchart(tracks_most_skipped_radar2, axistype = 3,
               #add a title
               title = "Features of Most Skipped Spotify Tracks",
               #custom polygon, let's generalize all of these 'bad' i.e. most skipped as a similar range of color using a R palette, less emphasis than the greens were for 'good' tracks, since we'll compare it to 'good' least skipped, not among themselves as much.
               pcol = palette_greys, #colors the lines
               pfcol = palette_greys_in, #adds same colors as fill to polygons
               
               #custom the grid; we will test greys on grey grid and adjust if needed.
               cglcol = "grey", cglty = 1, axislabcol = "grey30", cglwd = 0.8,
               
               #custom labels
               vlabels=c("US Popularity est. %", "Release Year", "Duration (ms)", "Time Signature", "Tempo", "Key", "Loudness", "Acousticness", "Beat Strength", "Bounciness", "Danceability", "Liveness", "Mechanism", "Organism", "Energy", "Flatness", "Instrumentalness", "Speechiness", "Valence"),
               vlcex = 0.635,
               #plot just a zero in center because all scales share min as 0
               caxislabels=c("0", "", "", "", "")
    )
    # Restore the standard par() settings
    par <- par(opar) 
  })
  
  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(tracks_least_skipped_radar2, input$plot1_click, addDist = TRUE)
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(tracks_least_skipped_radar2, input$plot1_brush)
  })
}
# Run the application.
shinyApp(ui, server)

