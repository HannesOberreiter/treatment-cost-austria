text <- paste0(
  "Max.: ", expenses_table$max, "\n",
  "Min.: ", expenses_table$min, "\n",
  "Mean: ", expenses_table$mean, "\n",
  "Median: ", expenses_table$median, "\n"
)

p <- ggplot(DATA) +
  geom_violin(
    aes(x = year, y = costs, fill = year), 
    color=colorBlindBlack8[1]
  ) +
  geom_boxplot(
    aes(x = year, y = costs), 
    outlier.shape = NA, alpha=0.5, fill=colorBlindBlack8[5]
  ) +
  geom_segment(
    x = 1, y = 45,
    xend = 1, yend = 52,
    lineend = "round",
    linejoin = "round",
    size = 0.5, 
    arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  scale_color_manual(
    values = colorBlindBlack8[c(2,4)], 
    aesthetics = "fill", name="Survey"
  ) +
  xlab("Survey Year") + ylab("Expenses / Colony [Euro]") +
  geom_segment(
    x = 2, y = 45,
    xend = 2, yend = 52,
    lineend = "round",
    linejoin = "round",
    size = 0.5, 
    arrow = arrow(length = unit(0.1, "inches"))
  ) + 
  annotate(
    "text",
    label = text,
    x = c(1.35, 2.35), y = c(40, 40),
    hjust = "right"
  ) + 
  scale_y_continuous(
    limits = c(0,50),
    breaks = c(seq(0,50,5))
  )

fSaveImages("distr-year", p)
rm(text, p)

