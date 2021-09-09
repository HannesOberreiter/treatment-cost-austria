# Description -------------------------------------------------------------
# Upset plot to see combination differences
# of our factors of interest

# Data --------------------------------------------------------------------
# Generate DF for ggupset
# only yes/no, no NA's and Hobby Beekeepers < 26 colonies wintered
df_ggupset <- dfClean %>%
  filter(op_migratory_beekeeper != "Unsicher" & op_cert_org_beek != "Unsicher") %>%
  drop_na(op_migratory_beekeeper, op_cert_org_beek) %>%
  mutate(
    up_migratory_beekeeper = ifelse(op_migratory_beekeeper == "Ja", "Migratory Beekeeper", NA),
    up_cert_org_beek = ifelse(op_cert_org_beek == "Ja", "Certified Organic", NA),
    up_hobby = ifelse(hives_winter < 26, "Recreational Beekeeper", NA)
  )
# nest data for plot
df_ggupset <- df_ggupset %>%
  nest(up_nest = c(up_migratory_beekeeper, up_cert_org_beek, up_hobby))

# stats for labels
# percent (18/19 / 19/20) %
label_ggupset <- df_ggupset %>%
  add_count(year, name = "total_n", sort = FALSE) %>%
  count(year, up_nest, total_n, sort = FALSE) %>%
  mutate(np = round(n / total_n * 100, 1)) %>%
  group_by(up_nest) %>%
  summarise(
    label = paste0("", paste0(format(np, nsmall = 1), collapse = " · "), "%"),
    y = sum(n)
  ) %>%
  glimpse()

# Plot --------------------------------------------------------------------
p <- df_ggupset %>%
  mutate(year = paste0("20", year)) %>%
  ggplot(
    aes(x = up_nest, y = ..count.., fill = year)
  ) +
  geom_bar(position = "stack", color = colorBlindBlack8[1]) +
  geom_text(
    data = label_ggupset,
    aes(x = up_nest, y = y, label = label),
    nudge_y = 50,
    size = 2.5, inherit.aes = FALSE
  ) +
  scale_color_manual(
    values = colorBlindBlack8[c(2, 4, 6)], aesthetics = "fill", name = "Survey"
  ) +
  scale_y_continuous(breaks = seq(0, max(nrow(df_ggupset)), 200)) +
  scale_x_upset() +
  ylab("Beekeeper [#]") +
  xlab("") +
  theme(
    legend.position = c(0.93, 0.75),
    axis.title.y = element_text(vjust = -10),
    panel.grid.major.y = element_line()
  )

fSaveImages(p, "distr-groups", w = 9)
rm(p, df_ggupset, label_ggupset)
