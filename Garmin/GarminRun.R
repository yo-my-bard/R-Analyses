library(xml2)
library(lubridate)

#Read in xml file
xmlj4 <- read_xml("july_4.xml")

#First crack at it:

#The track:
track <- xml_child(xml_child(xml_child(xml_child(xmlj4)), search=2), search=9)

#List of all trackpoints along the track, assuming one track
trackpoints <- xml_children(track)

#Trackpoints have 6 elements, we want first 5: Time, Position(Lat (1), Long(2)), Alt(M), Dist(M), HeartBPM
xml_children(trackpoints[i])

#Must call xml_text on each of above elements to capture the respective value. Position has two child elements.
#Data Structures work: opted for a list of lists, with each inner list containing value of elements, in
#same order- [time values, lat values, etc.] then combine to list of 6 lists then to tibble.

test <- vector("list", 1050) #we need to generalize the size using length, but it broke because of NULL values, I think
for (i in 1:1050) {  
  runStats <- xml_children(trackpoints[i])
  time <- xml_text(runStats[1])
  lat <- as.double(xml_text(xml_children(runStats)[1]))
  lon <- as.double(xml_text(xml_children(runStats)[2]))
  alt <- as.double(xml_text(runStats[3]))
  dist <- as.double(xml_text(runStats[4]))
  bpm <- as.double(xml_text(runStats[5]))
  
  test[[i]] <- list(time, lat, lon, alt, dist, bpm)
}

#Random NULL values at the end will make this fail. Check test before running. Will have to explore why this behavior happens.
#tail(test)
#test <- test[-c(1051:1055)]

july4df <- as_tibble(list(time = map_chr(test, 1),
                          lat = map_dbl(test, 2),
                          lon = map_dbl(test, 3),
                          alt = map_dbl(test, 4),
                          dist = map_dbl(test, 5),
                          bpm = map_dbl(test, 6)))

july4df %>%
  ggplot(aes(x=dist, y=bpm)) +
  geom_point(aes(colour=alt)) +
  labs(y = "Heart Rate (BPM)",
       x = "Distance (M)",
       colour = "Altitude (M)",
       title = "Change in Heart Rate Over Distance and Altitude") +
  theme(plot.background = element_rect(fill = "tan"))

#Using lon and lat to plot your route (or you can likely use another map overlay!)
july4df %>%
  ggplot(aes(lon, lat)) +
  geom_point()

