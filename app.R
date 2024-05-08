# Generic multibeam sonar distortion simulator
# 
# Interactive app to simulate the distortion in multibeam sonars
# due to overlap of individual beams
# This script is intended to provide an intuitive display
# of the distortion of multibeam sonar systems 
# to improve the interpretation of the info provided by sonars 
#
# Created by Guillermo Boyra in February 2023
# under the project IM-23-SONATUN-NX
# Updated in November 2023
#   To include acknowledgement to sponsors
# Revised in May 2023 
#   Corrected a plot error for DO = 0
#   Expanded for different sonar models
#     Number of beams, beamwidth, along-beam resolution...
#   Rearranged for clarity: 
#     two tabs in the sidebar panel (Sonar definition and School definition) 
#     two tabs in the main panel (Simulation result and About)
#   Correct an error in the distortion correction:
#     it removed twice the number of beams for each degree of overlap
#     to correct it I had to make the corrected school asymmetric for odd numbers of overlaps
#     (with DO 1, 3, 5, etc..., I remove only the leftmost beams of the distorted school)
#     To solve this, I will change the distortion correction in the next version:
#     I will correct the ellipse major and minor radius rather than remove entire beams


# load libraries

library(shiny)
# library(tidyverse)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

library(sf)


ui <- fluidPage(
  titlePanel("Sonar beam overlap simulator"), 
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
                                        max = 1000,
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
                                 in the horizontal swath of a generic multibeam sonar. 
                                 First, you must define the parameters of the multibeam sonar to simulate: 
                                 The number of beams, the swath opening and the percentage of overlap, 
                                 as well as the maximum range and resolution along transect (the number of samples per beam).
                                 Then, you define the target, an idealized school of elliptical shape: 
                                 you can choose the horizontal and vertical diameters, the angle of the
                                 major axis and the range of the school. Based on these parameters, 
                                 the app simulates the true target shape (pink area and dashed-line ellipse) 
                                 after distortion correction with reference to the distorted ellipse 
                                 (red area and continuous line ellipse).")
                         
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
    
    # Define the sonar swath
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
    
    # Select those points of the swath inside the distorted ellipse
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
    
    # 1. Correct overlap distortion -------------
    #++++++++++++++++++++++++++++++++++++++++++++
    
    # 1.1 Correct the ellipse -------------
    #++++++++++++++++++++++++++++++++++++++++++++
    
    a.cor <- input$diamx/2 - 2*(input$overlap/200 + 1/2)*input$ycm*tan(pi*phi.ang/360)*(abs(cos(pi*input$angle/180)))
    b.cor <- input$diamy/2 - 2*(input$overlap/200 + 1/2)*input$ycm*tan(pi*phi.ang/360)*(abs(sin(pi*input$angle/180)))
    
    # plot the ellipse simulating the corrected school
    # store the ellipse plot into a variable (to extract the points afterwards)
    ellip.cor.ggplot <- ggplot() +
      ggforce::geom_ellipse(aes(x0 = 0, y0 = input$ycm , a = a.cor, 
                                b = b.cor, angle = pi*input$angle/180)) 
    
    # access the ggplot data using ggplot_build()
    # store the ellipse points as a polygon in a df
    ellip.cor.df <- ggplot_build(ellip.cor.ggplot)$data[[1]] |>
      select(x, y) 
    
    # close the polygon adding the first row at the end:
    ellip.cor <- ellip.cor.df |>  
      rbind(ellip.cor.df[1,]) |> 
      as.matrix() 
    
    # transform the ellipse into an sf object:
    ellip.cor.sf <- sf::st_polygon(list(ellip.cor))
    
    
    # 1.2 Correct the swath samples -------------
    #++++++++++++++++++++++++++++++++++++++++++++
    
    ## 1.2.1 Remove 1 beam ---------------
    #+++++++++++++++++++++++++++++++++++++
    
    # When the overlap is 100%, we must remove one single beam to correct the school size
    
    # Subset the samples of the beam that are within the school minus one beam
    beam.sf.red1 <- beam.sf |>  
      # Select the samples inside the distorted ellipse:
      filter(within == T) |>  
      # Delete the last left beam of the within school swath in each radius:
      group_by(radius) |>  
      mutate(
        within.red = if_else(
          condition = angle == min(angle), 
          # (we must delete one, so could have chosen the right instead the left one)
          true = F, 
          false = within 
        )  
      ) |> ungroup() |> 
      select(angle:within, within.red) 
    
    # insert the column that identifies the distorted school minus 1 beam in beam.sf
    beam.sf$within.red1 <- F
    beam.sf$within.red1[beam.sf.red1$i] <- beam.sf.red1$within.red
    
    ## 1.2.2 Remove 2 beams -------------
    #++++++++++++++++++++++++++++++++++++
    
    
    # When the overlap is 200%, we must remove TWO beams to correct the school size
    
    # Subset the samples of the beam that are within the school minus two beams
    beam.sf.red2 <- beam.sf |> 
      # For each radius, select the samples inside the distorted ellipse minus two 
      filter(within == T)  |>  
      group_by(radius) |> 
      mutate(
        within.red = if_else(
          # Delete the last left and right beams of the within school swath in each radius:
          condition = angle == max(angle) | angle == min(angle), 
          true = F, 
          false = within 
        )  
      ) |> ungroup() |>  
      select(angle:within, within.red) 
    
    # insert the column that identifies the distorted school minus 2 beams in beam.sf
    beam.sf$within.red2 <- F
    beam.sf$within.red2[beam.sf.red2$i] <- beam.sf.red2$within.red
    
    
    ## 1.2.3 Remove 3 beams -------------
    #+++++++++++++++++++++++++++++++++++++
    
    # When the overlap is 300%, we must remove THREE beams to correct the school size
    
    # Subset the samples of the beam that are within the school minus 3 beams
    beam.sf.red3 <- beam.sf |> 
      filter(within.red2 == T)  |>  
      group_by(radius) |> 
      mutate(
        within.red = if_else(
          # Delete the last left beam of the within school swath in each radius:
          condition = angle == min(angle), 
          true = F, 
          false = within.red2 
        )  
      ) |>  ungroup()  |>  
      select(angle:within, within.red) 
    
    # insert the column that identifies the distorted school minus 3 beams in beam.sf
    beam.sf$within.red3 <- F
    beam.sf$within.red3[beam.sf.red3$i] <- beam.sf.red3$within.red
    
    
    ## 1.2.4 Remove 4 beams -------------
    #++++++++++++++++++++++++++++++++++++
    
    # When the overlap is 400%, we must remove 4 beams to correct the school size
    
    # Subset the samples of the beam that are within the school minus 4 beams
    beam.sf.red4 <- beam.sf |> 
      filter(within.red2 == T)  |>  
      group_by(radius) |> 
      mutate(
        within.red = if_else(
          # Delete the last left and right beams of the within school swath in each radius:
          condition = angle == max(angle) | angle == min(angle), 
          true = F, 
          false = within.red2 
        )  
      ) |>  ungroup() |> 
      select(angle:within, within.red) 
    
    # insert the column that identifies the distorted school minus 4 beams in beam.sf
    beam.sf$within.red4 <- F
    beam.sf$within.red4[beam.sf.red4$i] <- beam.sf.red4$within.red
    
    
    ## 1.2.5 Remove 5 beams-------------
    #+++++++++++++++++++++++++++++++++++
    
    # Subset the samples of the beam that are within the school minus 5 beams
    # delete the last beam of the swath in each radius
    beam.sf.red5 <- beam.sf |> 
      filter(within.red4 == T)  |>  
      group_by(radius) |>  
      mutate(
        within.red = if_else(
          # Delete the last left beam of the within school swath in each radius:
          condition = angle == min(angle), 
          true = F, 
          false = within.red4 
        )  
      )  |>  ungroup() |> 
      select(angle:within, within.red) 
    
    # insert the column that identifies the distorted school minus 5 beams in beam.sf
    beam.sf$within.red5 <- F
    beam.sf$within.red5[beam.sf.red5$i] <- beam.sf.red5$within.red
    
    
    ## 1.2.6 Remove 6 beams -------------
    #++++++++++++++++++++++++++++++++++++
    
    # Subset the samples of the beam that are within the school minus 6 beams
    # delete the last beam of the swath in each radius
    beam.sf.red6 <- beam.sf |> 
      filter(within.red4 == T)  |>  
      group_by(radius) |>  
      mutate(
        within.red = if_else(
          # Delete the last left and right beams of the within school swath in each radius:
          condition = angle == max(angle) | angle == min(angle), 
          true = F, 
          false = within.red4 
        )  
      )  |>  ungroup() |> 
      select(angle:within, within.red) 
    
    # insert the column that identifies the distorted school minus 6 beams in beam.sf
    beam.sf$within.red6 <- F
    beam.sf$within.red6[beam.sf.red6$i] <- beam.sf.red6$within.red
    
    
    ## 1.2.6 Plot  -------------
    #+++++++++++++++++++++++++++
    if (input$overlap == 100) overlap.beam <- beam.sf.red1 
    if (input$overlap == 200) overlap.beam <- beam.sf.red2  
    if (input$overlap == 300) overlap.beam <- beam.sf.red3  
    if (input$overlap == 400) overlap.beam <- beam.sf.red4  
    if (input$overlap == 500) overlap.beam <- beam.sf.red5  
    if (input$overlap == 600) overlap.beam <- beam.sf.red6  
    
    
    
    if (input$overlap == 0) {
      ggplot(beam.sf) + 
        geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
        geom_sf(data = beam.sf.red1, aes(geometry = geometry, size = (radius)/10), color = "pink")  +
        geom_path(data = ellip.df, aes(x = x, y = y)) +
        # scale_color_manual(values = c("red", "blue")) +
        guides(size = "none")
      
    } else {
      ggplot(beam.sf) + 
        # ggtitle(length(beam$angle)) +
        geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
        geom_sf(data = beam.sf.red1, aes(geometry = geometry, size = (radius)/10), color = "red")  +
        # according to the degree of overlap, we have to change the number in the following  line:
        geom_sf(data = overlap.beam, aes(geometry = geometry, size = (radius)/10, color = within.red))  +
        geom_path(data = ellip.df, aes(x = x, y = y)) +
        geom_path(data = ellip.cor.df, aes(x = x, y = y), color = "black", linetype = 2) +
        scale_color_manual(values = c("red", "pink")) +
        guides(size = "none") +
        guides(color = "none")
    }
    
    
    
  })
  
}
shinyApp(ui = ui, server = server)