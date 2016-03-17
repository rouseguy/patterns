#Code obtained from 
# https://aschinchon.wordpress.com/2016/03/14/sunflowers/
#install.packages("deldir")
library(delir)
library(ggplot2)
library(dplyr)

opt = theme(legend.position = "none",
            panel.background = element_rect(fill="red4"),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank())

CreateSunFlower <- function(nob=500, dx= 0, dy = 0){
  data.frame(r = sqrt(1:nob), 
             t = (1:nob)*(3 - sqrt(5))*pi) %>%
               mutate(x=r*cos(t) + dx,
                      y = r*sin(t) + dy)
}

g = seq(from=0, by= 45, length.out=4)
jitter(g, amount=2) %>%
  expand.grid(jitter(g, amount=2)) %>%
  apply(1, function(x) 
    CreateSunFlower(nob=round(jitter(220, factor=15)),
                    dx=x[1],
                    dy=x[2])) %>%
  do.call("rbind", .) %>%
  deldir() %>% 
  .$dirsgs -> sunflowers

ggplot(sunflowers) +
  geom_segment(aes(x= x1, y = y1,
               xend=x2, yend=y2), color ="greenyellow") + 
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) + 
  opt
