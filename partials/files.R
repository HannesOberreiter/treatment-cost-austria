D1920 <- tibble::as_tibble(readxl::read_xlsx("data/19-20.xlsx", skip=1)) %>% add_column(year = "19/20")
D1819 <- tibble::as_tibble(readxl::read_xlsx("data/18-19.xlsx", skip=1)) %>% add_column(year = "18/19")

RAW <- rbind(D1920, D1819)
rm(D1920, D1819)
