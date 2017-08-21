# site map FunCaB
library(ggplot2)
library(maps)
library(mapdata)
library(grid)

#Auds map based on richards code below
library(maps)
library(mapproj)
par(mar = rep(0.1,4))

#force map to cover Norway, Finland and Latvia
map(region = c("Norway", "Finland", "Latvia"), proj = "sinusoidal", type = "n")

# draw coordinates
map.grid(c(0,40, 55, 80), nx = 5, ny = 5, col = "black")
map(xlim = c(-130, 130), col = "grey80", fill = TRUE, proj = "", add = TRUE)


#Richards code
dat <- read.table(header = TRUE, text = "
                  siteID           latitude longitude Temperature Precipitation
                  Alrust           60.8203    8.70466           2             1
                  Arhelleren       60.6652    6.33738           3             3
                  Fauske           61.0355    9.07876           3             1
                  Gudmedalen       60.8328    7.17561           1             3
                  Hogsete          60.876     7.17666           2             2
                  Lavisdalen       60.8231    7.27596           1             2
                  Ovstedal         60.6901    5.96487           3             4
                  Rambera          61.0866    6.63028           2             3
                  Skjellingahaugen 60.9335    6.41504           1             4
                  Ulvhaugen        61.0243    8.12343           1             1
                  Veskre           60.5445    6.51468           2             4
                  Vikesland        60.8803    7.16982           3             2
                  ")
dat$Temperature <- factor(dat$Temperature)
dat$Precipitation <- factor(dat$Precipitation)
precipLab <- c("Very dry", "Dry", "Wet", "Very wet")
tempLab <- c("Alpine", "Intermediate", "Lowland")

xlim <- range(dat$longitude) + c(-1, 0.5)
ylim <- range(dat$latitude) + c(-0.5, 1)

# Low resolution map of Norway for the inset map
norwaymap <- map_data("world", "Norway")

# High resolution map of Norway for the main
norwaymapHires <- map_data("worldHires", "Norway")

a <- ggplot() +
  geom_map(data = norwaymap, aes(x = long, y = lat, map_id = region),
           map = norwaymap, colour = NA, fill = "grey60") +
  geom_rect(data = data.frame(),
            aes(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2]),
            colour = "red", fill = NA) +
  coord_map(xlim = c(3, 33), ylim = c(57, 72)) +
  labs(x = NULL, y = NULL)
a
#print(a)# print this map until you are happy with it.

b <- ggplot(dat, aes(x = longitude, y = latitude, colour = Temperature,
                     shape = Precipitation, fill = Temperature)) +
  geom_map(aes(x = long, y = lat, map_id = region), data = norwaymapHires, map = norwaymapHires, colour = NA, fill = "grey60", inherit.aes = FALSE) +
  geom_point(size = 4) +
  coord_map(xlim = xlim, ylim = ylim) +
  labs(x = NULL, y = NULL) +
  scale_shape_manual(breaks = 1:4, labels = precipLab, values = c(24, 21, 22, 25)) +
  guides(shape = guide_legend(override.aes = list(fill = "black")))+
  scale_fill_manual(values =c("dodgerblue","lightgreen", "red" ),
                    name="Elevation", 
                    breaks=c("1", "2", "3"), 
                    labels = c("ALP", "SUB", "BOR"))+
  scale_colour_manual(values =c("dodgerblue","lightgreen", "red", "#000000"),
                      name="Elevation", 
                      breaks=c("1", "2", "3", "4"), 
                      labels = c("ALP", "SUB", "BOR", "ALL"))
  #scale_fill_brewer(breaks = 1:3, labels = tempLab, palette = "RdBu")
b
#print(b)

maptheme <- theme(
  #axis.text = element_blank(),
  #axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  panel.background = element_blank()
)


grid.newpage()
vp_b <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vp_a <- viewport(width = 0.4, height = 0.4, x = 0.66, y = 0.79)  # the inset in upper left
print(b + maptheme, vp = vp_b)
print(a + maptheme, vp = vp_a)

