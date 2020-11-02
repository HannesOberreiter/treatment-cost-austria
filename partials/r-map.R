# MAP Libraries ####
# Important loading order to prevent errors! (https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true)
# library( rgeos )
# library( maptools )
# library( rgdal )

# Folder and Name need to be the same
SHAPEFILE.NAME.AUSTRIA <- "VGD_modf"

# Load MAP
SHAPEFILE.FOLDER.AUSTRIA  <- gsub(" ", "", paste(here(), SHAPEFILE.NAME.AUSTRIA, sep = "/"), fixed = TRUE)

# Read MAPS
if(!exists("MAP_AUSTRIA")){
  MAP_AUSTRIA   <- readOGR( dsn = SHAPEFILE.FOLDER.AUSTRIA, layer = SHAPEFILE.NAME.AUSTRIA )
  MF_STATES     <- fortify( MAP_AUSTRIA, region = "BL" )
  MF_DISTRICTS  <- fortify( MAP_AUSTRIA, region = "PB" )
}

rm(SHAPEFILE.FOLDER.AUSTRIA, SHAPEFILE.NAME.AUSTRIA)

MAP1819 <- DATA[DATA$year=="18/19", c( "latitude", "longitude" )] %>% fMapCluster(., 5) %>% add_column(., Survey = "2018/19")
MAP1920 <- DATA[DATA$year=="19/20", c( "latitude", "longitude" )] %>% fMapCluster(., 5) %>% add_column(., Survey = "2019/20")
MAP_POINTS <- rbind(MAP1819, MAP1920)

p <- ggplot() + 
  geom_polygon(
    data = MF_DISTRICTS, aes( x = long, y = lat, group = group ), fill="white", color = "black", size = 0.2
    ) + 
  geom_path(
    data = MF_STATES, aes(x = long, y = lat, group = group), color = "black", size = 0.6
    ) + 
  geom_point(
    data = MAP_POINTS, 
    aes(x = longitude, y = latitude, size = n, fill = Survey), 
    color = "gray", stroke = 0.3, shape = 21, alpha = 0.8
    ) + 
  scale_fill_discrete(type = colorBlindBlack8) +
  coord_quickmap() +
  xlab( "" ) + ylab( "" ) + labs( colour = "Beekeeper (n)", size = "Beekeeper (n)") +
  scale_size_continuous(
    range = c(1, 4), breaks = c(1, 10, round(max(MAP_POINTS$n)/2, digits = 0), max(MAP_POINTS$n))
    ) + 
  ggtitle("") +
  theme_void() +
  theme(
    legend.position="bottom", 
    legend.box = "horizontal",
    plot.title = element_text(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank()
  )
