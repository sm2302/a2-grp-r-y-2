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



# Method A
x1 <- runif(nLines,-1,1)
y1 <- sample(c(-1,1), size = 1) * sqrt(1 - ((x1)^2))
x2 <- runif(nLines,-1,1)
y2 <- sample(c(-1,1), size = 1) * sqrt(1 - ((x2)^2))
chordA <- sqrt(((x2)-(x1))^2 + ((y2)-(y1))^2)
sum(chordA > sqrt(3))




# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)





# Method B



# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)





# Method C 



# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)
