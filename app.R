# Generic multibeam sonar distortion simulator
# 
# Interactive app to simulate the distortion in multibeam sonars
# due to overlap of individual beams
# This script is intended to provide an intuitive display
# of the distortion of generic multibeam sonar systems 
# to improve the interpretation of the info provided by sonars 
#
# Created by Guillermo Boyra to simulate a Simrad SN90 in February 2023
# Generalized for different multibeam sonar systems in May 2024?


# load libraries

library(shiny)
# library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
# library(ggtext)
library(sf)


ui <- fluidPage(
  titlePanel("Sonar beam-overlap distortion simulation"), 
  sidebarLayout(
    sidebarPanel(width = 4,
                 radioButtons(inputId = "simDir", label = "Simulation type:",
                              choices = c("Forward" = "forward", "Inverse" = "inverse"), 
                              inline = T, selected = "inverse"),
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
                                        max = 900,
                                        value = 500),
                            sliderInput("delta.r",
                                        "Along-beam resolution (m):",
                                        min = 1,
                                        max = 10,
                                        value = 5),
                            sliderInput("overlap",
                                        "Degree of overlap:",
                                        min = 0,
                                        max = 6,
                                        step = 1,
                                        value = 2)
                   ),  # cierre tabPanel("Sonar")
                   
                   tabPanel("Target", 
                            tags$br(),
                            sliderInput("ycm",
                                        "Target range (m):",
                                        min = 50,
                                        max = 800,
                                        value = 200), 
                            sliderInput("diamx",
                                        "Major diameter (m):",
                                        min = 50,
                                        max = 250,
                                        value = 200), 
                            sliderInput("diamy",
                                        "Minor diameter (m):",
                                        min = 25,
                                        max = 125,
                                        value = 75), 
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
                         tags$h5("Interactive app to simulate the distortion caused by beam overlap
                                 on the swath of a generic multibeam sonar."), 
                         tags$h5("The simulation can work forward, distorting the shape of a true school,
                                 or in inverse direction, correcting the shape of a distorted school."), 
                         tags$h5("Define the parameters of (1) the generic multibeam sonar and (2) the target shown on the
                                  sonar echogram, an idealized  elliptical-shape fish school of adjustable size and orientation."),
                         tags$h5("Based on these parameters, the app simulates the true target shape 
                                  after correcting for overlap distortion, and calculates the 
                                  percentage of areal distortion in the apparent target."),
                          verbatimTextOutput("phi.ang"),
                         verbatimTextOutput("phi.bw"),
                         plotOutput("sonarPlot")
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
  output$phi.ang <- renderText({
    paste("Angular separation between beams =", round(input$beamwidth/input$N, 1), "degrees")
  })
  output$phi.bw <- renderText({
    paste("Individual beamwidth =", round((input$beamwidth/input$N)*(input$overlap + 1), 1), "degrees")
  })
  
  
  output$sonarPlot <- renderPlot({
    
    # 1. Define the sonar swath ------------
    #+++++++++++++++++++++++++++++++++++++++

        phi.ang <- input$beamwidth/input$N
    swath <- as.data.frame(expand.grid(
      angle = seq(-input$beamwidth/2 + phi.ang/2, input$beamwidth/2 - phi.ang/2, by = phi.ang), 
      radius = seq(0, input$rmax, by = input$delta.r)))
    swath <- swath  |>  
      mutate(
        value = 0, 
        x = radius*sin(angle*pi/180), 
        y = radius*cos(angle*pi/180)
      ) |> 
      group_by(radius) |> 
      mutate(beam = 1:length(angle)) |> 
      ungroup()
    
    swath.sf <- sf::st_as_sf(swath, coords = c("x", "y"))
    
    
    # 2. Define the idealized elliptical school --------------
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # plot the swath and an ellipse simulating the school
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
    
    # 3. Select swath samples inside the ellipse ------
    #++++++++++++++++++++++++++++++++++++++++++++++++++

    ellipse.swath.sf <- sf::st_within(x = swath.sf, y = ellip.sf)
    # I think that st_contains() would be the proper function, 
    # but st_within() works as well
    
    # Extract the index of the inner points into "index":
    index <- as.data.frame(ellipse.swath.sf)[[1]]
    
    # Identify the inner points in swath.sf in the column "target.ini"
    swath.sf <- swath.sf |>
      mutate(
        i = 1, 
        i = cumsum(i), 
        target.ini = ifelse(i %in% index, T, F)
      )

    # 4. Correct overlap distortion -------------
    #++++++++++++++++++++++++++++++++++++++++++++
    
    ## 4.1 Remove/Add overlapped beams -------------
    #+++++++++++++++++++++++++++++++++++++++++++++++

    ### 4.1.1 Create the target object ---------------
    #+++++++++++++++++++++++++++++++++++++++++++++++++

      target.sf <- swath.sf |> 
      group_by(radius) |> 
      mutate(
        empty.range = sum(target.ini), 
        # identify ranges without detection:
        empty.range = if_else(empty.range == 0, T, F), 
        # identify the minimum and maximum target detecting beams per radius:
        min.beam = beam[min(which(target.ini == T))], 
        max.beam = beam[max(which(target.ini == T))]
      ) |> 
      ungroup() |> 
      # remove empty ranges:
      filter(empty.range == F) |> 
      # initialize reduced and increased targets:
      mutate(
        # reduced target (to use in inverse simulations):
        target.minus = target.ini, 
        # increased target (to use in forward simulations):
        target.plus = target.ini
      ) 

    ### 4.1.2 Incremental addition/removal of distortion beams ------
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    if (input$overlap > 0) {
      target.sf <- target.sf |> 
        mutate(
          # forward - add 1 beam to the left of the minimum beam:
          target.plus = if_else(beam == min.beam - 1, T, target.plus), 
          # inverse - remove the minimum (leftmost) beam:
          target.minus = if_else(beam == min.beam, F, target.minus)
        )
    }
    if (input$overlap > 1) {
      target.sf <- target.sf |> 
        mutate(
          # forward - add 1 beams to the right of the maximum beam:
          target.plus = if_else(beam == max.beam + 1, T, target.plus), 
          # inverse - remove the maximum (rightmost) beam:
          target.minus = if_else(beam == max.beam, F, target.minus)
        )
    }
    if (input$overlap > 2) {
      target.sf <- target.sf |> 
        mutate(
          # forward - add 2 beams to the left of the minimum beam:
          target.plus = if_else(beam == min.beam - 2, T, target.plus), 
          # inverse - remove the 2nd minimum beam:
          target.minus = if_else(beam == min.beam + 1, F, target.minus)
        )
    }
    if (input$overlap > 3) {
      target.sf <- target.sf |> 
        mutate(
          # forward - add 2 beams to the right of the maximum beam:
          target.plus = if_else(beam == max.beam + 2, T, target.plus), 
          # inverse - remove the 2nd maximum beam:
          target.minus = if_else(beam == max.beam - 1, F, target.minus)
        )
    }
    if (input$overlap > 4) {
      target.sf <- target.sf |> 
        mutate(
          # forward - add 3 beams to the left of the minimum beam:
          target.plus = if_else(beam == min.beam - 3, T, target.plus), 
          # inverse - remove the 3rd minimum beam:
          target.minus = if_else(beam == min.beam + 2, F, target.minus)
        )
    }
    if (input$overlap > 5) {
      target.sf <- target.sf |> 
        mutate(
          # forward - add 3 beams to the right of the maximum beam:
          target.plus = if_else(beam == max.beam + 3, T, target.plus), 
          # inverse - remove the 3rd maximum beam:
          target.minus = if_else(beam == max.beam - 2, F, target.minus)
        )
    }

    # Select the increased/reduced target depending on the type of simulation:
    if (input$simDir == "forward") {
      target.sf <- target.sf |> 
        filter(target.plus == T) |> 
        select(angle:geometry, target.ini) |> 
        mutate(Distortion = !target.ini)
    } else {
      target.sf <- target.sf |> 
        filter(target.ini == T) |> 
        select(angle:geometry, target.minus) |> 
        mutate(Distortion = !target.minus)
    }
    
    target.sf$SampleType <- if_else(target.sf$Distortion, "Distortion", "School")

    ## 4.2 Estimate the distortion % -------------
    #+++++++++++++++++++++++++++++++++++++++++++++
    
    # Calculate the area of the samples for each radius
    target.sf <- target.sf |>
      mutate(
        radius.plus = radius + input$delta.r,
        Area = (phi.ang*pi/360)*(radius.plus^2 - radius^2)
      )

    # Filter the distortion-corrected target
    target.cor.sf <- target.sf |> filter(!Distortion)

    # Estimate the area of the distorted ellipse:
    Area.ellipse <- round(st_area(ellip.sf))
    # Estimate the area of the distorted target
    # Area.dist <- target.sf |> st_set_geometry(NULL) |>  summarise(round(sum(Area))) |> pull()
    Area.dist <- target.sf |> st_drop_geometry() |>  summarise(round(sum(Area))) |> pull()
    # Estimate the area of the distortion-corrected target
    # Area.cor <- target.cor.sf |> st_set_geometry(NULL) |>  summarise(round(sum(Area))) |> pull()
    Area.cor <- target.cor.sf |> st_drop_geometry() |>  summarise(round(sum(Area))) |> pull()
    # Percentage of distortion:
    Dist.pctg <- round(100*(Area.dist - Area.cor)/Area.dist)

    # 5. Make the plot -------------
    #+++++++++++++++++++++++++++++++
    
    # Select the manual colors so they work properly in both cases
    if (input$overlap == 0) {colores <- c("blue","red")} else {
      if (length(unique(target.sf$SampleType)) == 2)
      colores <- c("red","blue") else colores <- "red"
      }

    # Plots
    ggplot(swath.sf) +
      ggtitle(paste0("Distortion = ", Dist.pctg, "%"),
              subtitle = paste("Apparent area:", Area.dist, "m^2;",
                               "Ellipse area:", Area.ellipse, "m^2",
                               "True area:", Area.cor, "m^2")) +
      # plot the swath
      geom_sf(data = swath.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
      # plot the target (distorted and correct samples in different colors):
      geom_sf(data = target.sf, aes(geometry = geometry, size = (radius)/10, color = SampleType))  +
      # distorted ellipse
      geom_path(data = ellip.df, aes(x = x, y = y)) +
      scale_color_manual(values =  colores) +
      guides(size = "none")  +
      # guides(color = "none")
      theme(legend.position = c(0.9, 0.15))
  })
  
}
shinyApp(ui = ui, server = server)