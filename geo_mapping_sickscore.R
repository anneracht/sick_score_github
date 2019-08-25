#geo mapping

ggmap::register_google(key = "https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyDbftIE92IXO3oPX69yAYl9mwr-GNucahw")

p <- ggmap(get_googlemap(center = c(lon = -90, lat = 47.608013),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
p + geom_point(aes(x = Longitude, y = Latitude,  colour = Initial.Type.Group), data = i2, size = 0.5) +
  theme(legend.position="bottom")

#https://www.littlemissdata.com/blog/maps


df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/votes.csv")
print("hello")
