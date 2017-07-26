

library(geojsonio)
library(ggplot)
library(tidyverse)

# https://rud.is/b/2014/09/26/overcoming-d3-cartographic-envy-with-r-ggplot/

#tj <- topojson_read("https://rawgit.com/jpmarindiaz/geo-collection/master/col/col-depto.topo.json")
tj <- topojson_read(system.file("geo/col/col-depto.topojson",package = "gggeomagic"))

str(tj)
dtj <- fortify(tj) %>% mutate(.id = as.numeric(id)) %>% select(-id)
dat <- tj@data %>% mutate(.id = 0:(nrow(.)-1))

str(dtj)
str(dat)

dd <- left_join(dtj, dat)

centers <- dd %>% select(id, name, lat, long) %>%
  group_by(id, name) %>% summarize(lat = mean(lat), long = mean(long))


gg <- ggplot()
gg <- gg + geom_map(data=dd, map=dd,
                    aes(map_id=id, x=long, y=lat, group=group),
                    color="#ffffff", fill="#bbbbbb", size=0.25)
# gg <- gg + geom_point(data=centers, aes(x=x, y=y))
gg <- gg + geom_text(data=centers, aes(label=name, x=long, y=lat), size=3)
gg <- gg + coord_map() + theme_map()
gg

