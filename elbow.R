library(tidyverse)
library(sf)

#load Dat
dat <- read_csv("./data/dat.csv")
x <- dat$x
y <- dat$y

new_line <- cbind(x = seq(min(x), max(x), length.out = 100), 
                  y = seq(min(y), max(y), length.out = 100))

sf_dat <- st_as_sf(dat, coords = c("x", "y"))

new_line <- st_linestring(new_line)

dat$dist <- st_distance(sf_dat, new_line)

max(dat$dist)
ggplot(dat, aes(x = x, y = dist)) +
  geom_point()

dat$x[which.max(dat$dist)]






#load Dat2 
dat <- read_csv("./data/dat.csv") %>% filter(x > 0)
x <- dat$x
y <- dat$y

new_line <- cbind(x = seq(min(x), max(x), length.out = 100), 
                  y = seq(min(y), max(y), length.out = 100))

sf_dat <- st_as_sf(dat, coords = c("x", "y"))

new_line <- st_linestring(new_line)

dat$dist <- st_distance(sf_dat, new_line)

max(dat$dist)
ggplot(dat, aes(x = x, y = dist)) +
  geom_point()

dat$x[which.max(dat$dist)]


#function

