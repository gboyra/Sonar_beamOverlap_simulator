# Script with the plain code of the app
# without interactivity to make it easier to understand and make changes
# 

library(tidyverse)
# library(dplyr)
# library(ggplot2)
theme_set(theme_bw())

library(sf)


# 1. Define the sonar swath ----------------

# Total sonar beamwidth (degrees):
beamwidth = 120
# Number of beams:
N = 32  
# Maximum sonar range:
rmax = 100
# Along-beam sonar resolution:
delta.r = 5
# Angular distance between consecutive beams:
phi.ang <- beamwidth/N
# Degree of overlap
overlap <- 2


swath <- expand.grid(
  # polar coordinates:
  angle = seq(-beamwidth/2 + phi.ang/2, beamwidth/2 - phi.ang/2, by = phi.ang), 
  radius = seq(0, rmax, by = delta.r)) |>
  as.data.frame() |> 
    mutate(
    value = 0,   
    # Cartesian coordinates:
    x = radius*sin(angle*pi/180), 
    y = radius*cos(angle*pi/180)
  ) |> group_by(radius) |> 
  mutate(beam = 1:length(angle)) |> 
  ungroup()


unique(swath$angle) |> length() == N  # TRUE (cqd)

# convert it into a spatial object:
swath.sf <- sf::st_as_sf(swath, coords = c("x", "y"))
# swath.sf is a df but with the coordinates inside a list column named "geometry" 
str(swath.sf)

# Plot the swath of beams:
ggplot(swath.sf) + 
  # make the size fo the circles proportional to the radius (so they increase with distance)
  geom_sf(data = swath.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
  # remove the legend
  guides(size = "none")

# Plot Figure 3 - :
ggplot(swath.sf) + 
  # make the size fo the circles proportional to the radius (so they increase with distance)
  geom_sf(data = swath.sf, aes(geometry = geometry), color = "black")  +
  # remove the legend
  guides(size = "none") +
  xlab("x") + ylab("y")



# 2. Define the target ----------

# If simDir == "inverse", we define the OBSERVED target
# and the simulator estimates the corrected version that could have produced it

# If simDir == "forward", we define the TRUE target
# and the simulator estimates the distorted version the sonar generates


# Along-beam distance to the target CM (m):
ycm = 60
# Horizontal diameter (m):
diamx = 50
# Vertical diameter (m):
diamy = 25
# Major axis angle (degrees):
angle = 0

# plot the swath and an ellipse simulating the school
# store the ellipse plot into a variable (to extract the points afterwards)
ellip.ggplot <- ggplot() +
  ggforce::geom_ellipse(aes(x0 = 0, y0 = ycm , a = diamx/2, 
                            b = diamy/2, angle = pi*angle/180, 
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

# plot the swath and ellipse together
ggplot(swath.sf) + 
  geom_sf(data = swath.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
  geom_path(data = ellip.df, aes(x = x, y = y), color = "red", linewidth = 2 ) +
  guides(size = "none")


# plot the swath and ellipse together
# Figure 3 - bottom
ggplot(swath.sf) + 
  geom_sf(data = swath.sf, aes(geometry = geometry), color = "black")  +
  geom_path(data = ellip.df, aes(x = x, y = y), color = "red", linewidth = 2 ) +
  guides(size = "none")

# 3. Select the swath samples inside the school ellipse ------------

# Select those points of the swath inside the initial ellipse
ellipse.swath.sf <- sf::st_within(x = swath.sf, y = ellip.sf)
# I think that st_contains() would be the proper function, 
# but st_within() works as well

# Extract the index of the inner points into "index":
index <- as.data.frame(ellipse.swath.sf)[[1]]

# Identify the inner points in swath.sf in the column "target.ini"
swath.sf <- swath.sf  |> 
  mutate(
    i = 1, 
    i = cumsum(i), 
    # this is T when the swath coincides within the distorted target
    target.ini = ifelse(i %in% index, T, F)
  )

# plot the swath and ellipse together
ggplot(swath.sf) + 
  geom_sf(data = swath.sf, aes(geometry = geometry, size = (radius)/10), color = "grey80")  +
  geom_sf(data = swath.sf, aes(geometry = geometry, size = (radius)/10), color = "grey80")  +
  geom_sf(data = swath.sf, aes(geometry = geometry, size = (radius)/10, color = target.ini))  +
  scale_color_manual(values = c("grey80", "pink4")) +
  geom_path(data = ellip.df, aes(x = x, y = y), color = "black", linewidth = 1 ) +
  guides(size = "none")

# Figure 3 
# plot the swath and ellipse together
ggplot(swath.sf) + 
  geom_sf(data = swath.sf, aes(geometry = geometry), color = "black")  +
  geom_sf(data = swath.sf, aes(geometry = geometry), color = "black")  +
  geom_sf(data = swath.sf, aes(geometry = geometry, color = target.ini))  +
  scale_color_manual(values = c("black", "red")) +
  geom_path(data = ellip.df, aes(x = x, y = y), color = "red", linewidth = 2 ) +
  guides(size = "none") +
  # guides(color = "none")
  theme(legend.position = c(0.9, 0.15)) 



# 4. Correct overlap distortion -------------
#++++++++++++++++++++++++++++++++++++++++++++


## 4.1 Correct the ellipse (NOT USED) -------------
#++++++++++++++++++++++++++++++++++++++++++++++++++
#+
# IMP: Intentarlo con st_boundary()

a.cor <- diamx/2 - 2*(overlap + 1/2)*ycm*tan(pi*phi.ang/360)*(abs(cos(pi*angle/180)))
b.cor <- diamy/2 - 2*(overlap + 1/2)*ycm*tan(pi*phi.ang/360)*(abs(sin(pi*angle/180)))

# plot the ellipse simulating the corrected school
# store the ellipse plot into a variable (to extract the points afterwards)
ellip.cor.ggplot <- ggplot() +
  ggforce::geom_ellipse(aes(x0 = 0, y0 = ycm , a = a.cor, 
                            b = b.cor, angle = pi*angle/180)) 

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



## 4.2 Remove/add overlapped beams -------------
#+++++++++++++++++++++++++++++++++++++++++++++++
overlap = 6

target.sf <- swath.sf |> 
  group_by(radius) |> 
  mutate(
    empty.range = sum(target.ini), 
    empty.range = if_else(empty.range == 0, T, F), 
    min.beam = beam[min(which(target.ini == T))], 
    max.beam = beam[max(which(target.ini == T))]
  ) |> 
  ungroup() |> 
  filter(empty.range == F) |> 
  mutate(
    target.minus = target.ini, 
    target.plus = target.ini
  ) 
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 


 if (overlap > 0) {
  target.sf <- target.sf |> 
    mutate(
      target.plus = if_else(beam == min.beam - 1, T, target.plus), 
      target.minus = if_else(beam == min.beam, F, target.minus)
    )
}
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 

if (overlap > 1) {
  target.sf <- target.sf |> 
    mutate(
      target.plus = if_else(beam == max.beam + 1, T, target.plus), 
      target.minus = if_else(beam == max.beam, F, target.minus)
    )
}
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 

if (overlap > 2) {
  target.sf <- target.sf |> 
    mutate(
      target.plus = if_else(beam == min.beam - 2, T, target.plus), 
      target.minus = if_else(beam == min.beam + 1, F, target.minus)
    )
}
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 

if (overlap > 3) {
  target.sf <- target.sf |> 
    mutate(
      target.plus = if_else(beam == max.beam + 2, T, target.plus), 
      target.minus = if_else(beam == max.beam - 1, F, target.minus)
    )
}
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 

if (overlap > 4) {
  target.sf <- target.sf |> 
    mutate(
      target.plus = if_else(beam == min.beam - 3, T, target.plus), 
      target.minus = if_else(beam == min.beam + 2, F, target.minus)
    )
}
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 

if (overlap > 5) {
  target.sf <- target.sf |> 
    mutate(
      target.plus = if_else(beam == max.beam + 3, T, target.plus), 
      target.minus = if_else(beam == max.beam - 2, F, target.minus)
    )
}
target.sf |> st_drop_geometry() |>  summarise(sum(target.ini), sum(target.plus), sum(target.minus)) 

simDir <- "forward"

# Select the increased/reduced target depending on the type of simulation:
if (simDir == "forward") {
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

## 4.3 Calculate percentage of distortion -------------
#++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Calculate the area of the samples for each radius
target.sf <- target.sf |> 
  mutate(
    radius.plus = radius + delta.r,
    Area = (phi.ang*pi/360)*(radius.plus^2 - radius^2)
  )

# Filter the distortion-corrected target
target.cor.sf <- target.sf |> filter(!Distortion)

# Esimate the area of the distorted ellipse:
Area.ellipse <- st_area(ellip.sf)
# Estimate the area of the distorted target
Area.dist <- target.sf |> st_drop_geometry() |>  summarise(round(sum(Area))) |> pull()
# Estimate the area of the distortion-corrected target
Area.cor <- target.cor.sf |> st_drop_geometry() |>  summarise(round(sum(Area))) |> pull()
# Percentage of distortion:
Dist.pctg <- round(100*(Area.dist - Area.cor)/Area.dist)


## 4.4 Make the plot -------------
#+++++++++++++++++++++++++++++++++

# Select the manual colors so they work properly in both cases
if (overlap == 0) {colores <- c("blue","red")} else {
  if (length(unique(target.sf$Distortion)) == 2)
    colores <- c("red","blue") else colores <- "red"
}

# Plots
ggplot(swath.sf) + 
  ggtitle(paste0("Distortion = ", Dist.pctg, "%"),
          subtitle = paste("Apparent area:", Area.dist, "m2;",
                           "True area:", Area.cor, "m2")) +
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


# Figure 3 
# Plots
ggplot(swath.sf) + 
  # plot the swath
  geom_sf(data = swath.sf, aes(geometry = geometry), color = "black")  +
  # plot the target (distorted and correct samples in different colors):
  geom_sf(data = target.sf, aes(geometry = geometry, color = SampleType))  +
  # distorted ellipse
  geom_path(data = ellip.df, aes(x = x, y = y), color = "red", linewidth = 2) +
  scale_color_manual(values =  colores) +
  guides(size = "none")  +
  # guides(color = "none")
  theme(legend.position = c(0.9, 0.15)) 

# Figure 3 
# Plots
ggplot(swath.sf) + 
  ggtitle(paste0("Distortion = ", Dist.pctg, "%"),
          subtitle = paste("Apparent area:", Area.dist, "m2;", 
                           "True area:", Area.cor, "m2")) +
  # plot the swath
  geom_sf(data = swath.sf, aes(geometry = geometry), color = "black")  +
  # plot the target (distorted and correct samples in different colors):
  geom_sf(data = target.sf, aes(geometry = geometry, color = SampleType))  +
  # distorted ellipse
  geom_path(data = ellip.df, aes(x = x, y = y)) +
  scale_color_manual(values =  colores) +
  guides(size = "none")  +
  # guides(color = "none")
  theme(legend.position = c(0.9, 0.15)) 

