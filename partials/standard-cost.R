source("partials/list-treatments.R")

# Document to calculate "standard" cost values by different internet sources

#### Init ####
# Create Temporary Vector of "Standard" Calculated Costs
# investment cost, will be divided by the number of colonies
# material cost, will be multiplied by the number of colonies, but could be used multiple years
# ongoing costs, will be multiplied by number of colonies (eg. amount of formic acid)
investment_cost <- c(rep(0, 13))
material_cost   <- c(rep(0, 13))
ongoing_cost    <- c(rep(0, 13))

names(investment_cost) <- treatmentList$tname
names(material_cost)   <- treatmentList$tname
names(ongoing_cost)    <- treatmentList$tname

#### Hyperthermie ####

# Varroa Controller, 2475
# https://shop.bienenlieb.at/imker/varroa-controller/
# 05.10.2020
# you can also rent them

# Varroa Kill 2, 799
# https://www.bienenladen.at/de/produktdetails/produkt.varroa-kill.html
# 05.10.2020
investment_cost["Hyperthermie"] <- mean(c(2475, 799))

#### Other Biotechnical Method ####

# Duplex, 23.50
# https://www.bienenladen.at/de/produktdetails/produkt.duplex-wabentaschen-rundstab.html
# Different Frame Cages, 28.90, 33.00
# https://shop.bienenlieb.at/produkte-wabentaschen/
# Sclavini cage 7,25
# https://www.legaitaly.com/en/products/queen-breeding/queen-marking-kit-and-and-square-cages/scalvini-little-cage.808
# Exlucder 12,00
# https://www.legaitaly.com/en/products/hives-and-accessories/queen-excluder/secluder-shaped-brood-frame-lateral.133
# 05.10.2020

material_cost["Andere biotechnische Methode"] <- mean(
  c(
    23.50, 
    12.00,
    7.50
  )
)


#### Formic Acid ####

# Long term ~ 200ml
# Short term ~ 40ml * 3 Repeats
as_st <- 0.12
as_lt <- 0.2

# AMO Varroxal Ameisensäure 85%
# 1000g~0,82l
# 19.70 / 1000g = 16.15 / l
# https://shop.bienenlieb.at/?s=ameisens%C3%A4ure
# https://www.wachs-hoedl.com/AMO-Varroxal-1000g-Ameisensaeure-85-ig
# 05.10.2020

# Formivar 85
# 16.50 / l
# https://shop.garten-bienen.at/fuer-gesunde-bienen/
# 05.10.2020

# Formivar 6ß
# 13.50 / l
# https://shop.garten-bienen.at/fuer-gesunde-bienen/
# 05.10.2020

# Applicators
# Liebig, Nassenheid
# 6.90, 19.90
# https://shop.garten-bienen.at/fuer-gesunde-bienen/
# Universalverdunster
# 8.20
# https://www.wachs-hoedl.com/navi.php?qs=ameisens%E4ure&search=
# 05.10.2020

material_cost["Ameisensäure - Langzeit"] <- mean(
  c(
    6.90, 19.90, 8.20
  )
)

# small amount material cost for short time treatment (eg. cleaning sponge)
material_cost["Ameisensäure - Kurzzeit"] <- mean(
  c(
    1
  )
)

# x 2 because you would do more than 2 times ?
ongoing_cost["Ameisensäure - Langzeit"] <- mean(
  c(
    16.15, 16.50, 13.50
  )*as_lt
)

ongoing_cost["Ameisensäure - Kurzzeit"] <- mean(
  c(
    16.15, 16.50, 13.50
  )*as_st
)

rm(as_lt, as_st)

# Lactic Acid, 15%
# 9,80 / l 
# 16ml per brood frame, 5 brood frames ~ 80ml * 2 (two times application)
# http://www.bienen-gesundheit.com/sortiment/milchsaeure/
# https://www.imkereibedarf-bienenweber.de/Milchsaeure-15-ad-us-vet-1-Liter-Flasche-zur-Varroabekaempfung
# 05.10.2020

ongoing_cost["Milchsäure"] <- mean(9.80*0.160)

#### Oxalic Acid Mixture Pure ####
# Api Bioxal
# 14.50 / 35g
# 500ml (Water Sugar), 50ml / Colony
# https://shop.garten-bienen.at/fuer-gesunde-bienen/api-bioxal-oxalsaeure-zur-varroa-winterbehandlung-35-g.html
# 05.10.2020

# OXUVAR 
# 10.65 / 275g
# Bottle for 15 Colonies
# https://www.andermatt-biovet.de/de_bvi/oxuvar5-7-traeufelbehandlung-gegen-varroa.html
# 05.10.2020

# Oxybee (Sugar + Oxalic Acid)
# 39.00 / 1000g -> 888ml Lösung
# 50ml / Colony
# https://www.holtermann-shop.de/Varroa---Reinigung/Varroabehandlung/Oxybee-9643-9783.html
# 05.10.2020

ongoing_cost["Oxalsäure - Pure"] <- mean(
  14.50/10,
  10.65/15,
  39/888*50,
)

#### Oxalic Acid Sublimation ####
# Api Bioxal
# 14,50 / 35g
# 2,3g / Colony
# https://shop.garten-bienen.at/fuer-gesunde-bienen/api-bioxal-oxalsaeure-zur-varroa-winterbehandlung-35-g.html
# 05.10.2020

# Tolegano (Gas)
# 139
# https://www.weihmayr.com/Oxalsaeure-Verdampfer-gasbetrieben
# Varrox Eddy
# 390
# https://shop.garten-bienen.at/fuer-gesunde-bienen/
# Varrox Verdampfer
# 120
# https://www.andermatt-biovet.de/de_bvi/varrox-verdampfer-gegen-varroa.html
# Sublimox
# 395
# https://www.imkerei-seiringer.com/produkt/sublimox-oxalsaeure-verdampfer/
# https://imkershop-wien.at/produkt/sublimox-oxalsaeureverdampfer/
# 05.10.2020

investment_cost["Oxalsäure - sub."] <- mean(
  139, 390, 120, 395
)

# multiply by 2 becuse in summer its probably used more often and in winter only once
ongoing_cost["Oxalsäure - sub."] <- mean(
  14.50/15 * 2
)

#### Oxal Mixture ####

# VarroMed
# 26,00, 25.80
# 555ml, 45ml / colony
# https://shop.bienenlieb.at/?s=ameisens%C3%A4ure
# https://www.wachs-hoedl.com/VarroMed-555-ml
# 05.10.2020

# Bienenwohl
# 36.90 / liter
# 50ml / colony
# https://imkershop-wien.at/produkt/bienenwohl-10-liter/
# 05.10.2020

ongoing_cost["Oxalsäure - Mix"] <- mean(
  26 / 555 * 45,
  36.90 * 0.05
)


#### Thymol ####

# Apiguard
# 3.50 / gel    2 * gel / colony
# https://de.swienty.com/pi/Apiguard-Varroabehandlung_4003894_164663.aspx?CountryID=2&LanguageId=5&CurrencyId=11
# 05.10.2020

# ApiLifeVar
# 3,00     2 * / colony
# https://www.wachs-hoedl.com/Api-Life-Var-2-Plaettchen
# 05.10.2020

# Thymovar
# 26.50 / 10 Strips -> 2 / Colony
ongoing_cost["Thymol"] <- mean(
  3.50 * 2, 3.00 * 2, 26.5/10*2
)


#### Synthetical ####

# Polyvar 
# 30,00  / 10 Strips
# 1 Strip per Colony
# https://www.medizinfuchs.at/preisvergleich/polyvar-yellow-275-mg-impraeg.str.f.d.bienenstock-10-st-bayer-vital-gmbh-gb-tiergesundheit-pzn-13413376.html
# 05.10.2020

# Apitraz
# 35.00 / 10 Strips
# 2 Strip per Colony
# https://www.soin-et-nature.com/de/apiculture/19370-apitraz-beutel-mit-10-streifen-25-cm-500mg-amitraz-5-bienenstocke.html
# 05.10.2020

# Apivar
# 33.80 / 10 Strips
# 2 Strip per Colony
# https://agrocenter.it/Apivar-500mg-10-Streifen
# 05.10.2020

ongoing_cost["Anderes chem. Produkt"] <- mean(
  30 / 10,
  35 / 5,
  33.80 / 10
)

#### Investment & Material deduction time ####

# estimated years of usage
de_investment = 10
de_material = 5

investment_cost <- investment_cost / de_investment
material_cost   <- material_cost   / de_material

#### Generate DF ####

ESTIMATED_COSTS <- tibble(
  investment  = round(investment_cost, 2),
  material    = round(material_cost, 2),
  consumables = round(ongoing_cost, 2)
)

treatmentList <- cbind(treatmentList, ESTIMATED_COSTS)

rm(de_investment, de_material, material_cost, ongoing_cost, investment_cost, ESTIMATED_COSTS)




