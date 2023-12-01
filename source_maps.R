############################################################
#        HARİTALAR İÇİN BOŞ HARİTA GRAFİĞİ TASLAĞI         #
############################################################
if (!require(rnaturalearth)) {
  install.packages("rnaturalearth",
    dependencies = TRUE
  )
}
if (!require(rnaturalearthdata)) {
  install.packages("rnaturalearthdata",
    dependencies = TRUE
  )
}
if (!require(sf)) install.packages("sf", dependencies = TRUE)

tr <- ne_states(country = "turkey", returnclass = "sf")
trmap <- ggplot(tr) +
  geom_sf() +
  theme_minimal()
dissList <- tr %>%
  select(diss_me, iso_3166_2) %>%
  st_drop_geometry() %>%
  transmute(IK = str_extract(
    string = iso_3166_2,
    pattern = "\\d+"
  ) %>%
    as.integer(), diss_me)
tr <- tr %>% left_join(dissList)
