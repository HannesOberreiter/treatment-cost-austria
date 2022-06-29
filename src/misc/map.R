# Description -------------------------------------------------------------
# library( sf )

# Import ------------------------------------------------------------------
mapDataPath <- glue("{here()}/data/maps.RData")
# Read MAPS
if (!exists("mfStates")) {
  if (file.exists(mapDataPath)) {
    load(mapDataPath)
  } else {
    mapAustria <- read_sf(glue("{here()}/data/map"))
    mfStates <- mapAustria %>%
      group_by(BL) %>%
      summarize(
        geometry = st_union(geometry)
      )
    mfStatesSimplify <- mfStates %>% st_simplify(dTolerance = 0.002)
    mfDistricts <- mapAustria %>%
      group_by(PB) %>%
      summarize(
        geometry = st_union(geometry)
      )
    mfDistrictsSimplify <- mfDistricts %>% st_simplify(dTolerance = 0.002)
    # save R object to prevent loading each time
    save(mapAustria, mfDistricts, mfStates, mfStatesSimplify, mfDistrictsSimplify, file = mapDataPath)
  }
}
rm(mapDataPath)

# Data Cleanup
temp <- dfClean %>%
  # drop_na(longitude, latitude) %>%
  filter(
    # Drop "In mehr as einem Bezirk" because we cannot know which one it belongs to
    district != "In mehr als einem Bezirk"
  ) %>%
  count(year, district) %>%
  group_by(year) %>%
  mutate(
    n = n,
    freq = proportions(n) * 100
  ) %>%
  # join with map source
  left_join(mfDistrictsSimplify, by = c("district" = "PB"))

temp_labels <- temp %>%
  group_by(year) %>%
  summarise(
    n = sum(n),
    n = format(n, big.mark = ".", decimal.mark = ","),
    n = paste0("20", year[[1]], " (n = ", n, ")")
  ) %>%
  pull(n)

names(temp_labels) <- unique(temp$year)
temp_labels <- as_labeller(temp_labels)

p <- temp %>%
  ggplot() +
  geom_sf(
    data = mfStatesSimplify,
    aes(group = BL),
    color = "black",
    size = 0.6,
    fill = "white"
  ) +
  geom_sf(
    aes(group = district, fill = n, geometry = geometry),
    color = colorBlindBlack8[1],
    size = 0.2
  ) +
  coord_sf() +
  xlab("") +
  ylab("") +
  theme(
    legend.position = "bottom",
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_viridis_c(
    option = "inferno",
    direction = -1
  ) +
  labs(fill = "Participants [#]") +
  facet_wrap(
    ~year,
    ncol = 1,
    labeller = temp_labels
  )
fSaveImages(p, "map", h = 10, w = 6.5)