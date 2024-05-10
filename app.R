# Generic multibeam sonar distortion simulator
# 
# Interactive app to simulate the distortion in multibeam sonars
# due to overlap of individual beams
# This script is intended to provide an intuitive display
# of the distortion of generic multibeam sonar systems 
# to improve the interpretation of the info provided by sonars 
#
# Created by Guillermo Boyra to simulate a Simrad SN90 in February 2023
# Generalized for different multibeam sonar systems in May 2023


# load libraries

library(shiny)
# library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

library(sf)


ui <- fluidPage(
  titlePanel("Sonar beam-overlap distortion simulator"), 
  sidebarLayout(
    sidebarPanel(width = 4,
                 tags$h4("Simulation parameters"),
                 tabsetPanel(
                   tabPanel("Sonar", 
                            tags$br(),
                            sliderInput("N",
                                        "Number of beams:",
                                        min = 8,
                                        max = 512,
                                        value = 32, 
                                        step = 8),
                            sliderInput("beamwidth",
                                        "Swath opening (degrees):",
                                        min = 45,
                                        max = 360,
                                        value = 120, 
                                        step = 5),
                            sliderInput("rmax",
                                        "Maximum range (m):",
                                        min = 300,
                                        max = 700,
                                        value = 500),
                            sliderInput("delta.r",
                                        "Along-beam resolution (m):",
                                        min = 1,
                                        max = 10,
                                        value = 5),
                            sliderInput("overlap",
                                        "Degree of overlap (%):",
                                        min = 0,
                                        max = 600,
                                        step = 100,
                                        value = 200)
                   ),  # cierre tabPanel("Sonar")
                   
                   tabPanel("Target", 
                            tags$br(),
                            sliderInput("ycm",
                                        "Target range (m):",
                                        min = 50,
                                        max = 350,
                                        value = 200), 
                            sliderInput("diamx",
                                        "Horizontal diameter (m):",
                                        min = 50,
                                        max = 250,
                                        value = 150), 
                            sliderInput("diamy",
                                        "Vertical diameter (m):",
                                        min = 25,
                                        max = 125,
                                        value = 50), 
                            sliderInput("angle",
                                        "Major axis angle:",
                                        min = -90,
                                        max = 90,
                                        value = 0)
                   ) 
                 )  # cierre tabsetPanel()
    ),  # cierre sidebarPanel()
    mainPanel(width = 8,
              tabsetPanel(
                tabPanel("Simulation results", 
                         plotOutput("sonarPlot"),
                         tags$h5("Interactive app to simulate the distortion caused by beam overlap
                                 in the horizontal swath of a generic multibeam sonar."), 
                         tags$h5("First, define the parameters of the multibeam sonar: 
                                 number of beams, swath opening and percentage of overlap, 
                                 as well as the maximum range and resolution along beam (the size of the samples)."),
                         tags$h5("Then, define the target, an idealized school of elliptical shape: 
                                 you can choose the horizontal and vertical diameters, the angle of the
                                 major axis and the range of the school. Based on these parameters, 
                                 the app simulates the apparent and true target shape 
                                 and calculates the percentage of areal distortion in the apparent target.")
                ),
                tabPanel("About", 
                         tags$br(),
                         tags$h5("The equations used to simulate and correct the distortion are based on the article 
                     `Correction of beam overlap-induced athwart distortion in multibeam sonars`,
                     Boyra, G., Martinez, U., Uranga, J., Moreno, G. and Peña, H.
                     ICES Journal of Marine Science, Volume 80, Issue 1, January 2023, Pages 197–209."), 
                         tags$a(href = "https://academic.oup.com/icesjms/article/80/1/197/6964912", 
                                "https://academic.oup.com/icesjms/article/80/1/197/6964912"),
                         tags$br(),
                         tags$h5("Simulation created by Guillermo Boyra in 2023 under the SONATUN project, funded by the ´Administración del Estado, 
              Ministerio de Agricultura, Pesca y Alimentación, DG del Medio Natural y Política Forestal Ministerio 
              de Medio Ambiente y Medio Rural y Marino, Ministerio de Agricultura y Pesca, Alimentación y Medio Ambiente,
              Secretaría General de Pesca´ as part of the UE NEXT GENERATION Program."), 
                         tags$br()
                         
                )
              )
    )  # cierre mainPanel()
  )      
  
)  # cierre fluidPage()
server <- function(input, output) {
  
  output$sonarPlot <- renderPlot({
    
    # 1. Define the sonar swath ------------
    phi.ang <- input$beamwidth/input$N
    beam <- as.data.frame(expand.grid(
      angle = seq(-input$beamwidth/2 + phi.ang/2, input$beamwidth/2 - phi.ang/2, by = phi.ang), 
      radius = seq(0, input$rmax, by = input$delta.r)))
    beam <- beam  |>  
      mutate(
        value = 0, 
        x = radius*sin(angle*pi/180), 
        y = radius*cos(angle*pi/180)
      ) 
    beam.sf <- sf::st_as_sf(beam, coords = c("x", "y"))
    
    
    # 2. Define the idealized elliptical school --------------
    # plot the beam and an ellipse simulating the school
    # store the ellipse plot into a variable (to extract the points afterwards)
    ellip.ggplot <- ggplot() +
      ggforce::geom_ellipse(aes(x0 = 0, y0 = input$ycm , a = input$diamx/2, 
                                b = input$diamy/2, angle = pi*input$angle/180, 
                                fill = rgb(1, 0, 0, alpha = 0.1)), 
                            color = rgb(1, 0, 0, alpha = 0.1)) 
    
    # access the ggplot data using ggplot_build()
    # store the ellipse points as a polygon in a df
    ellip.df <- ggplot_build(ellip.ggplot)$data[[1]] |>
      select(x, y) 
    
    # close the polygon adding the first row at the end:
    ellip <- ellip.df |> 
      rbind(ellip.df[1,]) |> 
      as.matrix() 
    
    # transform the ellipse into an sf object:
    ellip.sf <- sf::st_polygon(list(ellip))
    
    # 3. Select those points of the swath inside the distorted ellipse ------
    large.beam.sf <- sf::st_within(x = beam.sf, y = ellip.sf)
    # I think that st_contains() would be the proper function, 
    # but st_within() works as well
    
    # Extract the index of the inner points into "index":
    index <- as.data.frame(large.beam.sf)[[1]]
    
    # Identify the inner points in beam.sf in the column "within"
    beam.sf <- beam.sf |>
      mutate(
        i = 1, 
        i = cumsum(i), 
        within = ifelse(i %in% index, T, F)
      )
    
    # 4. Correct overlap distortion -------------

    # Select the swath samples inside the distorted ellipse:
    school.sf <- beam.sf |> 
      filter(within == T) |> 
      mutate(corrected = T) |> 
      select(angle:within, corrected) 

    # Remove increasing layers of beams according to the degree of overlap (DO)
    if (input$overlap > 0) {
      school.sf <- school.sf |> 
        # Delete the last left beam of the within school swath in each radius:
        group_by(radius) |>  
        mutate(
          corrected = if_else(angle == min(angle), F, within)
          # (we must delete one, so could have chosen the right instead the left one)
        ) |> ungroup() 
    } 
    if (input$overlap > 100) {
      school.sf <- school.sf |> 
        # Delete the last right beam of the within school swath in each radius:
        group_by(radius) |>  
        mutate(
          corrected = if_else(angle == max(angle), F, corrected)
        ) |> ungroup() 
    } 
    if (input$overlap > 200) {
      school.sf <- school.sf |> 
        # Delete the second minimum beam of the within school swath in each radius:
        group_by(radius) |>  
        mutate(
          # set the second minimum value to FALSE
          corrected = if_else(angle == head(sort(angle), 2)[2], F, corrected)
        ) |> ungroup() 
    } 
    if (input$overlap > 300) {
      school.sf <- school.sf |> 
        # Delete the second maximum beam of the within school swath in each radius:
        group_by(radius) |>  
        mutate(
          # set the second maximum value to FALSE
          corrected = if_else(angle == tail(sort(angle), 2)[1], F, corrected)
        ) |> ungroup() 
    } 
    if (input$overlap > 400) {
      school.sf <- school.sf |> 
        # Delete the next minimum beam of the within school swath in each radius:
        group_by(radius) |>  
        mutate(
          # set the third minimum value to FALSE
          corrected = if_else(angle == head(sort(angle), 3)[3], F, corrected)
        ) |> ungroup() 
    } 
    if (input$overlap > 500) {
      school.sf <- school.sf |> 
        # Delete the next maximum beam of the within school swath in each radius:
        group_by(radius) |>  
        mutate(
          # set the third maximum value to FALSE
          corrected = if_else(angle == tail(sort(angle), 3)[1], F, corrected)
        ) |> ungroup() 
    } 
    school.sf <- school.sf |> mutate(Distortion = !corrected)
    school.sf$Distortion[is.na(school.sf$Distortion)] <- T
    
    # 5. Estimate the distortion -------------
    
    # Calculate the area of the samples for each radius
    school.sf <- school.sf |> 
      mutate(
        radius.plus = radius + delta.r,
        Area = (phi.ang*pi/360)*(radius.plus^2 - radius^2)
      )
    
    # Filter the distortion-corrected school
    school.cor.sf <- school.sf |> filter(!Distortion)

    # Estimate the area of the distorted school
    Area.dist <- school.sf |> st_set_geometry(NULL) |>  summarise(round(sum(Area))) |> pull()
    # Estimate the area of the distortion-corrected school
    Area.cor <- school.cor.sf |> st_set_geometry(NULL) |>  summarise(round(sum(Area))) |> pull()
    # Percentage of distortion:
    Dist.pctg <- round(100*(Area.dist - Area.cor)/Area.dist)

    # 6. Make the plot -------------

    # Select the manual colors so they work properly in both cases
    if (input$overlap == 0) {colores <- c("blue","red")} else {colores <- c("blue","red")}
    
    # Plots
    ggplot(beam.sf) + 
      ggtitle(paste0("Distortion = ", Dist.pctg, "%"),
              subtitle = paste("Apparent area:", Area.dist, "m2;", 
                               "True area:", Area.cor, "m2")) +
      # plot the swath
      geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
      # plot the school (distorted and correct samples in different colors):
      geom_sf(data = school.sf, aes(geometry = geometry, size = (radius)/10, color = Distortion))  +
      # distorted ellipse
      geom_path(data = ellip.df, aes(x = x, y = y)) +
      scale_color_manual(values =  colores) +
      guides(size = "none")  +
      # guides(color = "none")
      theme(legend.position = c(0.9, 0.15)) 
  })
  
}
shinyApp(ui = ui, server = server)