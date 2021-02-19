# Description -------------------------------------------------------------
# Upset plot to see combination differences
# of our factors of interest

# Data --------------------------------------------------------------------
# Generate DF for ggupset
# only yes/no, no NA's and Hobby Beekeepers < 21 colonies wintered
df_ggupset <- DATA %>% 
  filter(op_migratory_beekeeper != "Unsicher" & op_cert_org_beek != "Unsicher") %>% 
  drop_na(op_migratory_beekeeper, op_cert_org_beek) %>% 
  mutate(
    up_migratory_beekeeper = ifelse(op_migratory_beekeeper == "Ja", "Migratory", NA),
    up_cert_org_beek = ifelse(op_cert_org_beek == "Ja", "Cert. Organic", NA),
    up_hobby = ifelse(hives_winter < 21, "1-20 Colonies", NA)
  )
# nest data for plot
df_ggupset <- df_ggupset %>% 
  nest(up_nest = c(up_migratory_beekeeper, up_cert_org_beek, up_hobby))

# stats for labels
# percent (18/19 / 19/20) %
label_ggupset <- df_ggupset %>% 
  count(year, up_nest, sort = F) %>% 
  mutate(np = round(n/sum(n)*100,1)) %>% 
  group_by(up_nest) %>% 
  summarise(
    label = paste0("(", paste0(np, collapse = "/"), ")%")
  )

# Plot --------------------------------------------------------------------
p <- df_ggupset %>%
  ggplot(
    aes(x = up_nest, y = ..count.., fill = year)
    ) +
  stat_count(position = "stack", color = colorBlindBlack8[1]) +
  geom_text(
    data = label_ggupset,
    aes(x = up_nest, y = 0, label = label),
    vjust = 1.2, size = 3, inherit.aes = F
  ) +
  scale_color_manual(
    values = colorBlindBlack8[c(2,4)], aesthetics = "fill", name="Survey"
  ) +
  scale_y_continuous( breaks = seq(0,max(nrow(df_ggupset)),100) ) +
  scale_x_upset() + 
  ylab("Beekeeper [#]") + xlab("") + 
  theme(
    axis.title.y = element_text(vjust = -10)
  )

fSaveImages("distr-groups", p)
rm(p, df_ggupset, label_ggupset)
