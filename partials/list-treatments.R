# This file only contains our treatment list, names will be used to connect costs, table column names etc.

treatmentList = tibble(
  tsingle = c(
    "T_vcount_", 
    "T_drone_",
    "T_hyperthermia_",
    "T_biotechnical_",
    "T_formic_short_",
    "T_formic_long_",
    "T_lactic_",
    "T_oxalic_trickle_pure_",
    "T_oxalic_vapo_",
    "T_oxalic_trickle_mix_",
    #"T_oxalic_trickle_",
    "T_thymol_",
    "T_synthetic_",
    "T_other_"
  ),
  ttotal = NA,
  tname = c(
    "Varroa Kontrolle",
    "Drohnenbrutentnahme",
    "Hyperthermie",
    "Andere biotechnische Methode",
    "Ameisensäure - Kurzzeit",
    "Ameisensäure - Langzeit",
    "Milchsäure",
    "Oxalsäure - Pure",
    "Oxalsäure - sub.",
    "Oxalsäure - Mix",
    "Thymol",
    "Anderes chem. Produkt",
    "Andere Methode"
  ),
  tshort = c(
    "V-check",
    "Drohne",
    "Hyp.",
    "Biot.",
    "AS-KZ",
    "AS-LZ",
    "Milchs.",
    "Ox-pure",
    "Ox-sub",
    "Ox-mix",
    "Thy",
    "chem. Pr.",
    "Andere"
  )
)

treatmentList$ttotal <- paste(treatmentList$tsingle, "total", sep="")
