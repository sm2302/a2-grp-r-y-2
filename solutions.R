# Instruction to students: You may clear the code in this file and replace it
# with your own.

library(tidyverse)
library(ggforce)
theme_set(theme_void())





# Bertrand’s Paradox Probability Part

# Setting parameters
nLines = 100; # Number of lines
x0 = 0 # x-coordinate of the center of the circle
y0 = 0 # y-coordinate of the center of the circle
r0 = 1 # Radius of circle
l = sqrt(3) # Length of the side of an equilateral triangle



# Method A
theta1 <- runif(nLines, 0, 2*pi)
theta2 <- runif(nLines, 0, 2*pi) 
# Endpoints of chords on the circumference of the circle
x1 <- r0*cos(theta1) 
y1 <- r0*sin(theta1) 
x2 <- r0*cos(theta2)
y2 <- r0*sin(theta2)

chordA <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
sA <- sum(chordA > l) # Total number of chords longer than the length(l)
pA <- sA/nLines # Probability of Method A
print(pA)
# Answer for Probability of Method A is roughly around 1/3



# Method B
theta3 <- runif(nLines, 0, 2*pi)
# Endpoints of radius on the circumference of the circle
x3 <- r0*cos(theta3)
y3 <- r0*sin(theta3) 

distance1 <- runif(nLines, 0, r0)
chordB <- 2 * sqrt((r0)^2 - (distance1)^2)
sB <- sum(chordB > l) # Total number of chords longer than the length(l)
pB <- sB/nLines # Probability of Method B
print (pB)
# Answer for Probability of Method B is roughly around 1/2



# Method C 
r2 <- r0 * sqrt(runif(nLines))
theta4 <- runif(nLines, 0, 2*pi)

# Midpoints within the circle
x4 <- r2*cos(theta4)
y4 <- r2*sin(theta4)

distance2 <- sqrt((x4 - x0)^2 + (y4 - y0)^2)
chordC <- 2 * sqrt((r0)^2 - (distance2)^2)
sC <- sum(chordC > l) # Total number of chords longer than the length(l)
pC <- sC/nLines # Probability of Method C
print(pC)
# Answer for Probability of Method C is roughly around 1/4





# Bertrand’s Paradox Plotting Part

# Coordinates of equilateral triangle
eqtri_df <- tibble(
  x    = c(0, sqrt(3) / 2, -sqrt(3) / 2),
  y    = c(1, -0.5, -0.5),
  xend = c(sqrt(3) / 2, -sqrt(3) / 2, 0),
  yend = c(-0.5, -0.5, 1)
)



# Method A
rdmchr_df1 <- tibble(
  x    = x1,
  y    = y1,
  xend = x2,
  yend = y2
)

p <- ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r0), col = 'blue', size = 0.5) +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = 'red', size = 0.5) +
  geom_segment(data = rdmchr_df1, aes(x = x, y = y, xend = xend, yend = yend), size = 0.3) +
  coord_equal()

ggsave(p, file = "plotA.png", height = 5, width = 7)



# Method B
x5 <- (distance1/r0) * (x3-x0) 
y5 <- (distance1/r0) * (y3-y0)
# Endpoints of chord on the circumference of the circle
x6 <- 
y6 <- 
x7 <- ((2 * (x5)) - x6)
y7 <- ((2 * (y5)) - y6)

rdmchr_df2 <- tibble(
  x    = x6,
  y    = y6,
  xend = x7,
  yend = y7
)

p <- ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r0), col = 'blue', size = 0.5) +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = 'red', size = 0.5) +
  geom_segment(data = rdmchr_df, aes(x = x0, y = y0, xend = x3, yend = y3), col = 'gray', size = 0.3) +
  geom_point(aes(x5, y5), col = 'gray') +
#  geom_segment(data = rdmchr_df2, aes(x = x, y = y, xend = xend, yend = yend), size = 0.3) +
  coord_equal()

ggsave(p, file = "plotB.png", height = 5, width = 7)



# Method C
# Endpoints of chord on the circumference of the circle
x8 <- 
y8 <- 
x9 <- ((2 * (x4)) - (x8))
y9 <- ((2 * (y4)) - (y8))

rdmchr_df3 <- tibble(
  x    = x8,
  y    = y8,
  xend = x9,
  yend = y9
)

p <- ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r0), col = 'blue', size = 0.5) +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = 'red', size = 0.5) +
  geom_point(aes(x4, y4), col = 'gray') +
#  geom_segment(data = rdmchr_df3, aes(x = x, y = y, xend = xend, yend = yend), size = 0.3) +
  coord_equal()

ggsave(p, file = "plotC.png", height = 5, width = 7)

