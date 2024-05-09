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
# Degree of overlap
overlap <- 200


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

# plot the swath and ellipse together
ggplot(beam.sf) + 
  geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey80")  +
  geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey80")  +
  geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10, color = within))  +
  scale_color_manual(values = c("grey80", "pink4")) +
  geom_path(data = ellip.df, aes(x = x, y = y), color = "black", linewidth = 1 ) +
  guides(size = "none")


# 4. Correct overlap distortion -------------
#++++++++++++++++++++++++++++++++++++++++++++


# 4.1 Correct the ellipse -------------
#++++++++++++++++++++++++++++++++++++++++++++

# 1.1 Correct the ellipse -------------
#++++++++++++++++++++++++++++++++++++++++++++

a.cor <- diamx/2 - 2*(overlap/200 + 1/2)*ycm*tan(pi*phi.ang/360)*(abs(cos(pi*angle/180)))
b.cor <- diamy/2 - 2*(overlap/200 + 1/2)*ycm*tan(pi*phi.ang/360)*(abs(sin(pi*angle/180)))

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


## 4.1 Remove overlapped beams -------------
#+++++++++++++++++++++++++++++++++++++++++++
overlap = 600

# Select the samples inside the distorted ellipse:
school.sf <- beam.sf |> 
  filter(within == T) |> 
  mutate(corrected = T) |> 
  select(angle:within, corrected) 
school.sf$corrected |> sum()

# Now we remove increasing layers of beams according to the DO value set
if (overlap > 0) {
  school.sf <- school.sf |> 
    # Delete the last left beam of the within school swath in each radius:
    group_by(radius) |>  
    mutate(
      corrected = if_else(angle == min(angle), F, within)
      # (we must delete one, so could have chosen the right instead the left one)
    ) |> ungroup() 
} 
school.sf$corrected |> sum()

if (overlap > 100) {
  school.sf <- school.sf |> 
    # Delete the next last beam of the within school swath in each radius:
    group_by(radius) |>  
    mutate(
      corrected = if_else(angle == max(angle), F, corrected)
    ) |> ungroup() 
} 
school.sf$corrected |> sum()

if (overlap > 200) {
  school.sf <- school.sf |> 
    # Delete the next last beam of the within school swath in each radius:
    group_by(radius) |>  
    mutate(
      # set the second minimum value to FALSE
      corrected = if_else(angle == head(sort(angle), 2)[2], F, corrected)
    ) |> ungroup() 
} 
school.sf$corrected |> sum()

if (overlap > 300) {
  school.sf <- school.sf |> 
    # Delete the next last beam of the within school swath in each radius:
    group_by(radius) |>  
    mutate(
      # set the second maximum value to F
      corrected = if_else(angle == tail(sort(angle), 2)[1], F, corrected)
    ) |> ungroup() 
} 
school.sf$corrected |> sum()

if (overlap > 400) {
  school.sf <- school.sf |> 
    # Delete the next last beam of the within school swath in each radius:
    group_by(radius) |>  
    mutate(
      # set the third minimum value to F
      corrected = if_else(angle == head(sort(angle), 3)[3], F, corrected)
    ) |> ungroup() 
} 
school.sf$corrected |> sum()

if (overlap > 500) {
  school.sf <- school.sf |> 
    # Delete the next last beam of the within school swath in each radius:
    group_by(radius) |>  
    mutate(
      # set the third maximum value to F
      corrected = if_else(angle == tail(sort(angle), 3)[1], F, corrected)
    ) |> ungroup() 
} 
school.sf$corrected |> sum()



if (overlap == 0) {colores <- c("pink","red")} else {colores <- c("red","pink")}
ggplot(beam.sf) + 
  # ggtitle(length(beam$angle)) +
  geom_sf(data = beam.sf, aes(geometry = geometry, size = (radius)/10), color = "grey90")  +
  geom_sf(data = beam.sf.red1, aes(geometry = geometry, size = (radius)/10), color = "red")  +
  # according to the degree of overlap, we have to change the number in the following  line:
  geom_sf(data = school.sf, aes(geometry = geometry, size = (radius)/10, color = corrected))  +
  geom_path(data = ellip.df, aes(x = x, y = y)) +
  geom_path(data = ellip.cor.df, aes(x = x, y = y), color = "black", linetype = 2) +
  scale_color_manual(values =  colores) +
  guides(size = "none") +
  guides(color = "none")

