# Instruction to students: You may clear the code in this file and replace it
# with your own.

library(tidyverse)
library(ggforce)
theme_set(theme_void())

# Draw a random chord in a unit circle centred at origin -----------------------

# Coordinates of equilateral triangle
eqtri_df <- tibble(
  x    = c(0, sqrt(3) / 2, -sqrt(3) / 2),
  y    = c(1, -0.5, -0.5),
  xend = c(sqrt(3) / 2, -sqrt(3) / 2, 0),
  yend = c(-0.5, -0.5, 1)
)

# Setting parameters
nLines = 1000; # number of lines
r = 1



# Method A
theta1 <- runif(nLines, 0, 2*pi)
theta2 <- runif(nLines, 0, 2*pi) 
x1 <- r*cos(theta1)
y1 <- r*sin(theta1) 
x2 <- r*cos(theta2)
y2 <- r*sin(theta2)
chordA <- sqrt(((x2)-(x1))^2 + ((y2)-(y1))^2)
sum(chordA > sqrt(3))
pA <- sum(chordA > sqrt(3)) / nLines #probability of Method A

# Method B
theta3 <- runif(nLines, 0, 2*pi)
x3 <- r*cos(theta3)
y3 <- r*sin(theta3) 
distance <- runif(nLines, 0, r)
chordB <- 2 * sqrt((1^2) - (distance)^2)
sum(chordB > sqrt(3))
pB <- sum(chordB > sqrt(3)) / nLines #probability of Method B

# Method C 
theta4 <- runif(nLines, 0, 2*pi)
x4 <- r*cos(theta4)
y4 <- r*sin(theta4)
x5 <- x4*cos(theta4) + y4*sin(theta4)
y5 <- x4*sin(theta4) - y4*cos(theta4)
x6 <- x4*cos(theta4) - y4*sin(theta4)
y6 <- x4*sin(theta4) + y4*cos(theta4)
chordC <- sqrt(((x6)-(x5))^2 + ((y6)-(y5))^2)
sum(chordC > sqrt(3))
pC <- sum(chordC > sqrt(3)) / nLines #probability of Method C

# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)
