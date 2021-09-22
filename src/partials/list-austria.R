# Beekeeper and Colonies in Austria based on
# zukunft biene 2
# Anzahl der ImkerInnen und Bienenvölker in Österreich. Daten von \enquote{Biene Österreich}, seit 2017 auf Basis der Meldungen ins Veterinärinformationssystem (VIS) für den jeweiligen Stichtag 31. Oktober.
statsAut <- tibble(
    year = c(2018, 2019, 2020),
    year2 = c("18/19", "19/20", "20/21"),
    beekeeper = c(29745, 30237, 31923),
    colonies = c(372889, 390607, 426121)
)

stateList <- c(
    "Burgenland" = "Burgenland",
    "Wien" = "Vienna",
    "Niederösterreich" = "Lower Austria",
    "Oberösterreich" = "Upper Austria",
    "Steiermark" = "Styria",
    "Salzburg" = "Salzburg",
    "Kärnten" = "Carinthia",
    "Tirol" = "Tyrol",
    "Vorarlberg" = "Vorarlberg"
)
