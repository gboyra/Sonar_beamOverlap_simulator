# Script with the plain code of the app
# without interactivity to make it easier to understand and make changes
# 

library(tidvyverse)

# 1. Define the sonar swath ----------------

# Total sonar beamwidth (degrees):
beamwidth = 120
# Number of beams:
N = 32  
# Maximum sonar range:
rmax = 500
# Along-beam sonar resolution:
delta.r = 5
# Angular distance between consecutive beams:
phi.ang <- beamwidth/N


beam <- expand.grid(
  # polar coordinates:
  angle = seq(-beamwidth/2 + phi.ang/2, beamwidth/2 - phi.ang/2, by = phi.ang), 
  radius = seq(0, rmax, by = delta.r)) |>
  as.data.frame() |> 
    mutate(
    value = 0,   # what do we need this value for?
    # cartesian coordinates:
    x = radius*sin(angle*pi/180), 
    y = radius*cos(angle*pi/180)
  ) 


unique(beam$angle) |> length() == N  # TRUE (cqd)

# convert it into a spatial object:
beam.sf <- sf::st_as_sf(beam, coords = c("x", "y"))
# beam.sf is a df but with the coordinates inside a list column named "geometry" 
str(beam.sf)

# Plot the swath of beams:
ggplot(beam.sf) + 
  # make the size fo the circles proportional to the radius (so they increase with distance)
  geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
  # remove the legend
  guides(size = "none")




# 2. Define the distorted school ----------

# In this version of the software, we define the distorted school
# and the simulator estimates the corrected version that could have produced it

# Along-beam distance to the school CM (m):
ycm = 200
# Horizontal diameter (m):
diamx = 150
# Vertical diameter (m):
diamy = 50
# Major axis angle (degrees):
angle = 0

# plot the beam and an ellipse simulating the school
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
ggplot(beam.sf) + 
  geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey80")  +
  geom_path(data = ellip.df, aes(x = x, y = y), color = "red", linewidth = 2 ) +
  guides(size = "none")

# 3. Select the swath samples inside the school ellipse ------------

# Select those points of the swath inside the distorted ellipse
large.beam.sf <- sf::st_within(x = beam.sf, y = ellip.sf)
# I think that st_contains() would be the proper function, 
# but st_within() works as well

# Extract the index of the inner points into "index":
index <- as.data.frame(large.beam.sf)[[1]]

# Identify the inner points in beam.sf in the column "within"
beam.sf <- beam.sf  |> 
  mutate(
    i = 1, 
    i = cumsum(i), 
    # this is T when the swath coincides within the distorted school
    within = ifelse(i %in% index, T, F)
  )

# 4. Correct overlap distortion -------------
#++++++++++++++++++++++++++++++++++++++++++++

## 4.1 Remove 1 beam -------------
#+++++++++++++++++++++++++++++++++

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

## 4.2 Remove 2 beams -------------
#++++++++++++++++++++++++++++++++++

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


## 4.3 Remove 3 beams -------------
#+++++++++++++++++++++++++++++++++++

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

## 4.4 Remove 4 beams -------------
#++++++++++++++++++++++++++++++++++

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


## 4.5 Remove 5 beams -------------
#++++++++++++++++++++++++++++++++++

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


## 4.6 Remove 5 beams -------------
#++++++++++++++++++++++++++++++++++

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


## 4.7 Plot  -------------
#++++++++++++++++++++++++++
if (overlap == 100) overlap.beam <- beam.sf.red1 
if (overlap == 200) overlap.beam <- beam.sf.red2  
if (overlap == 300) overlap.beam <- beam.sf.red3  
if (overlap == 400) overlap.beam <- beam.sf.red4  
if (overlap == 500) overlap.beam <- beam.sf.red5  
if (overlap == 600) overlap.beam <- beam.sf.red6  


