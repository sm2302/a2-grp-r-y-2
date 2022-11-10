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
r = 1 # Radius of circle
l = sqrt(3) # Length of the side of an equilateral triangle



# Method A
theta1 <- runif(nLines, 0, 2*pi)
theta2 <- runif(nLines, 0, 2*pi) 
# Endpoints of chords on the circumference of the circle
x1 <- r*cos(theta1) 
y1 <- r*sin(theta1) 
x2 <- r*cos(theta2)
y2 <- r*sin(theta2)

chordA <- sqrt(((x2)-(x1))^2 + ((y2)-(y1))^2)
sA <- sum(chordA > l) # Total number of chords longer than the length(l)
pA <- sA/nLines # Probability of Method A
print(pA)
# Answer for Probability of Method A is roughly around 1/3



# Method B
theta3 <- runif(nLines, 0, 2*pi)
# Endpoints of radius on the circumference of the circle
x3 <- r*cos(theta3)
y3 <- r*sin(theta3) 

distance <- runif(nLines, 0, r)
chordB <- 2 * sqrt((r)^2 - (distance)^2)
sB <- sum(chordB > l) # Total number of chords longer than the length(l)
pB <- sB/nLines # Probability of Method B
print (pB)
# Answer for Probability of Method B is roughly around 1/2



# Method C 
x4 <- runif(nLines, -1, 1)
y4 <- sqrt((r)^2 - (x4)^2)
theta4 <- runif(nLines, 0, 2*pi)

# Endpoints of chords within the circle
x5 <- r*cos(theta4)
y5 <- r*sin(theta4)
x6 <- x4*cos(theta4) + y4*sin(theta4)
y6 <- x4*sin(theta4) - y4*cos(theta4)
x7 <- x4*cos(theta4) - y4*sin(theta4)
y7 <- x4*sin(theta4) + y4*cos(theta4)

chordC <- sqrt(((x7)-(x6))^2 + ((y7)-(y6))^2)
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

rdmchr_df <- tibble(
  x    = x1,
  y    = y1,
  xend = x2,
  yend = y2
)



# Method A
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r), col = 'blue', size = 0.5) +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = 'red', size = 0.5) +
  geom_segment(data = rdmchr_df, aes(x = x, y = y, xend = xend, yend = yend), size = 0.3) +
  coord_equal()

ggsave(p, file = "plotA.png", height = 5, width = 7)



# Method B
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r), col = 'blue', size = 0.5) +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = 'red', size = 0.5) +
  geom_segment(data = rdmchr_df, aes(x = x0, y = y0, xend = x3, yend = y3), size = 0.3) +
  coord_equal()

ggsave(p, file = "plotB.png", height = 5, width = 7)



# Method C
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r), col = 'blue', size = 0.5) +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = 'red', size = 0.5) +
  geom_segment(data = rdmchr_df, aes(x = x6, y = y6, xend = x7, yend = y7), size = 0.3) +
  coord_equal()

ggsave(p, file = "plotC.png", height = 5, width = 7)


