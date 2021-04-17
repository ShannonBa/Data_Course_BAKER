########Ugly Plot###############
library("tidyverse")
library("ggplot2")
library("ggimage")
library("png")

# create a df

set.seed(2017-02-21)
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("gzahn_with_mushroom.PNG",
                                 "weird_turtle_face.PNG"),
                               size=10, replace = TRUE)
)

shark_background <- png::readPNG("gzahn_with_mushroom.PNG")

# plot2
ggplot(d, aes(x, y)) + 
    geom_image(aes(image=sample(c("gzahn_with_mushroom.PNG",
                                   "weird_turtle_face.PNG"),
                                  size=10, replace = TRUE)), size=.05) +
  theme(plot.background = "gzahn_with_mushroom.PNG")


  annotation_custom(rasterGrob(imgage, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
