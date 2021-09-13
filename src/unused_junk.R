# AOV ---------------------------------------------------------------
# we could use the residuals to remove treatment effect
# but linear function is not an good estimate for our data
# aovSize <- DATA %>%
#   split(.$year) %>%
#   map(~aov(costs ~ 0 + c_short_od, data = .x)) %>%
#   map(residuals) %>%
#   map_dfr(~bind_cols("residuals" = .x))
# DATA$residuals <- aovSize$residuals
# rm(aovSize)

# Operation Size ----------------------------------------------------------
# we would except that bigger companies spend less per colony
# we have two ways to group 1-20, 21-50 and > 50
# or 1-20, > 21, both would make sense

## Subset ------------------------------------------------------------------
# If we want to only use treatments which are used by both groups
# Extract Treatment Methods used by defined Factor
STATS_certorg$treatments <- STATS_certorg$subData %>%
  filter(op_cert_org_beek == "Yes") %>%
  select(c_short_od) %>%
  unique() %>%
  pull()
# generate second subset of data
STATS_certorg$subDataTreatment <- STATS_certorg$subData %>%
  filter(c_short_od %in% STATS_certorg$treatments)


# Inactive Code Chunks --------------------------------------------------
## Dunn-Test ---------------------------------------------------------------

# pwc1819 <- DATA %>% filter(year == "18/19") %>%
#   rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>%
#   add_column(year = "18/19")
# pwc1920 <- DATA %>% filter(year == "19/20") %>%
#   rstatix::dunn_test(costs ~ 0 + tri_size, p.adjust.method = "holm") %>%
#   add_column(year = "19/20")
# Binding Position for Plotting of Significant Values
# resPWC <- rbind(pwc1819, pwc1920) %>% rstatix::add_xy_position(x = "tri_size")
# rm(pwc1819, pwc1920)