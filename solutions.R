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
nLines = 100; # number of lines





# Method A


# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)





# Method B

sLines = 0
lLines = 0
for (n in nLines){
  x1 = 0
  x2 = runif(nLines, min = -1, max = 1)
  y1 = 0
  y2 = runif(c(nLines, min = -1, max = 1))
  b1 <- ((y2-y1)^2) + ((x2-x1)^2)
  b2 <- sqrt(b1)
}
if b2 > sqrt(3) {
  lLines = lLines + 1
  }
  elseif b2 < sqrt(3) {
    sLines = sLines + 1
  }
}

p <- sLines/(sLines + lLines)
print(p)

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
