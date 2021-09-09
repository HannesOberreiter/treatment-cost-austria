# Description -------------------------------------------------------------
# Map Loading and generation
# Important loading order to prevent errors! (https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true)
# library( rgeos )
# library( maptools )
# library( rgdal )
# Now only using sf, dont need the tools above
# library( sf )

# Import ------------------------------------------------------------------
# Folder and Name need to be the same
SHAPEFILE.NAME.AUSTRIA <- "VGD_modf"
# Load MAP
SHAPEFILE.FOLDER.AUSTRIA <- gsub(" ", "", paste(here(), SHAPEFILE.NAME.AUSTRIA, sep = "/"), fixed = TRUE)
# Read MAPS
if (!exists("MF_STATES")) {
  if (file.exists("data/maps.RData")) {
    load("data/maps.RData")
  } else {
    MAP_AUSTRIA <- read_sf(SHAPEFILE.FOLDER.AUSTRIA)
    MF_STATES <- MAP_AUSTRIA %>%
      group_by(BL) %>%
      summarize(
        geometry = st_union(geometry)
      )
    MF_DISTRICTS <- MAP_AUSTRIA %>%
      group_by(PB) %>%
      summarize(
        geometry = st_union(geometry)
      )
    # save R object to prevent loading each time
    save(MAP_AUSTRIA, MF_DISTRICTS, MF_STATES, file = "data/maps.RData")
  }
  # Old Version we now use sf package, as it is quicker
  # MAP_AUSTRIA   <- readOGR( dsn = SHAPEFILE.FOLDER.AUSTRIA, layer = SHAPEFILE.NAME.AUSTRIA )
  # MF_STATES     <- fortify( MAP_AUSTRIA, region = "BL" )
  # MF_DISTRICTS  <- fortify( MAP_AUSTRIA, region = "PB" )
}
rm(SHAPEFILE.FOLDER.AUSTRIA, SHAPEFILE.NAME.AUSTRIA)

# Add Data To Map ---------------------------------------------------------
MAP_POINTS <- dfClean %>%
  select(year, latitude, longitude) %>%
  split(.$year) %>%
  map(~ fMapCluster(., 5)) %>%
  bind_rows(.id = "year") %>%
  mutate(
    Survey = paste0("20", year)
  )

# Plot --------------------------------------------------------------------
p <- ggplot() +
  geom_point(
    data = MAP_POINTS,
    aes(x = longitude, y = latitude, size = n, fill = Survey),
    color = "black", stroke = 0.3, shape = 21, alpha = 0.8
  ) +
  geom_sf(data = MF_STATES, aes(group = BL), color = "black", size = 0.6, fill = "white") +
  geom_sf(data = MF_DISTRICTS, aes(group = PB), fill = NA, color = colorBlindBlack8[1], size = 0.2) +
  # geom_polygon(
  #   data = MF_DISTRICTS, aes( x = long, y = lat, group = group ), fill="white", color = "black", size = 0.2
  #   ) +
  # geom_path(
  #   data = MF_STATES, aes(x = long, y = lat, group = group), color = "black", size = 0.6
  #   ) +

  geom_point(
    data = MAP_POINTS,
    aes(x = longitude, y = latitude, size = n, fill = Survey),
    color = "black", stroke = 0, shape = 21, alpha = 0.8
  ) +
  scale_fill_discrete(type = colorBlindBlack8[c(2, 4, 6)]) +
  coord_sf() +
  xlab("") +
  ylab("") +
  labs(size = "Beekeeper [n]") +
  scale_size_continuous(
    range = c(1, 4), breaks = c(1, 10, round(max(MAP_POINTS$n) / 2, digits = 0), max(MAP_POINTS$n))
  ) +
  ggtitle("") +
  theme_classic() +
  ggplot2::theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_blank()
  )

fSaveImages(p, "map", h = 5)

rm(p, MAP_POINTS, MF_DISTRICTS, MF_STATES)