# Description -------------------------------------------------------------
# Calculate an estimate cost for treatment methods
# based on different online resources and standard
# applying practices, eg. from course materials

# Dependent Files ---------------------------------------------------------
source("src/partials/list-treatments.R")

# Costs Calculation --------------------------------------------------------------------
# Create Temporary Vector of "Standard" Calculated Costs
# investment cost, will be divided by the number of colonies
# material cost, will be multiplied by the number of colonies, but could be used multiple years
# ongoing costs, will be multiplied by number of colonies (eg. amount of formic acid)
investmentCost <- c(rep(0, nrow(treatmentList)))
materialCost <- c(rep(0, nrow(treatmentList)))
ongoingCost <- c(rep(0, nrow(treatmentList)))
names(investmentCost) <- treatmentList$tname
names(materialCost) <- treatmentList$tname
names(ongoingCost) <- treatmentList$tname


## Hypertermia -------------------------------------------------------------
# Cost:    Varroa Controller, 2475
# Source:  https://shop.bienenlieb.at/imker/varroa-controller/
# Date:    05.10.2020
# Comment: You can also rent them

# Cost:   Varroa Kill 2, 799
# Source: https://www.bienenladen.at/de/produktdetails/produkt.varroa-kill.html
# Date:   05.10.2020

investmentCost["Hyperthermia"] <- mean(c(2475, 799))

## Other Biotechnical Method -----------------------------------------------
# Cost:   Duplex, 23.50
# Source: https://www.bienenladen.at/de/produktdetails/produkt.duplex-wabentaschen-rundstab.html
# Cost:   Different Frame Cages, 28.90, 33.00
# Source: https://shop.bienenlieb.at/produkte-wabentaschen/
# Cost:   Sclavini cage 7,25
# Source: https://www.legaitaly.com/en/products/queen-breeding/queen-marking-kit-and-and-square-cages/scalvini-little-cage.808
# Cost:   Exlucder 12,00
# Source: https://www.legaitaly.com/en/products/hives-and-accessories/queen-excluder/secluder-shaped-brood-frame-lateral.133
# Date:   05.10.2020

materialCost["Another biotechnical method"] <- mean(
  c(
    mean(c(23.50, 28.90, 33.0)),
    12.00,
    7.25
  )
)

## Formic Acid -------------------------------------------------------------
# Application, Standard Method from Literature and Course Materials
# Long term ~ 200ml
# Short term ~ 40ml * 3 Repeats
as_st <- 0.12
as_lt <- 0.2

# Product: AMO Varroxal Ameisensäure 85%; 1000 g ~ 0.82 l
# Cost:    19.70 / 1000g = 16.15 / l
# Source:  https://shop.bienenlieb.at/?s=ameisens%C3%A4ure
# Source:  https://www.wachs-hoedl.com/AMO-Varroxal-1000g-Ameisensaeure-85-ig
# Date:    05.10.2020

# Product: Formivar 85
# Cost:    16.50 / l
# Source:  https://shop.garten-bienen.at/fuer-gesunde-bienen/
# Date:    05.10.2020

# Product: Formivar 65
# Cost:    13.50 / l
# Source:  https://shop.garten-bienen.at/fuer-gesunde-bienen/
# Date:    05.10.2020

# Product:     Liebig, Nassenheid (Applicator)
# Cost:        6.90, 19.90
# Source:      https://shop.garten-bienen.at/fuer-gesunde-bienen/
# Product:     Universalverdunster (Applicator)
# Cost:        8.20
# Source:      https://www.wachs-hoedl.com/navi.php?qs=ameisens%E4ure&search=
# Date:        05.10.2020

materialCost["Formic acid - long term"] <- mean(
  c(
    6.90, 19.90, 8.20
  )
)
# small amount material cost for short time treatment (eg. cleaning sponge)
materialCost["Formic acid - short term"] <- mean(
  c(
    1
  )
)

ongoingCost["Formic acid - long term"] <- mean(
  c(
    16.15, 16.50, 13.50
  ) * as_lt
)
ongoingCost["Formic acid - short term"] <- mean(
  c(
    16.15, 16.50, 13.50
  ) * as_st
)

rm(as_lt, as_st)

## Lactic Acid -------------------------------------------------------------
# Application, Standard Method from Literature and Course Materials
# 16 ml per brood frame, 5 brood frames ~ 80ml * 2 (two times application)

# Product: Lactic Acid, 15%;
# Cost     9.80 / l
# Source:  http://www.bienen-gesundheit.com/sortiment/milchsaeure/
# Source:  https://www.imkereibedarf-bienenweber.de/Milchsaeure-15-ad-us-vet-1-Liter-Flasche-zur-Varroabekaempfung
# Date:    05.10.2020

ongoingCost["Lactic acid"] <- mean(9.80 * 0.160)

## Oxalic Acid Pure ------------------------------------------------
# Product: Api Bioxal
# Cost:    14.50 / 35g
# Appl.:   500ml (Water Sugar), 50ml / Colony
# Source:  https://shop.garten-bienen.at/fuer-gesunde-bienen/api-bioxal-oxalsaeure-zur-varroa-winterbehandlung-35-g.html
# Date:    05.10.2020

# Product: OXUVAR
# Cost:    10.65 / 275g
# Appl.:   Bottle for 15 Colonies
# Source:  https://www.andermatt-biovet.de/de_bvi/oxuvar5-7-traeufelbehandlung-gegen-varroa.html
# Date:    05.10.2020

# Product: Oxybee (Sugar + Oxalic Acid)
# Cost:    39.00 / 1000g -> 888ml Lösung
# Appl.:   50ml / Colony
# Source:  https://www.holtermann-shop.de/Varroa---Reinigung/Varroabehandlung/Oxybee-9643-9783.html
# Date:    05.10.2020

ongoingCost["Oxal acid - pure"] <- mean(
  14.50 / 10,
  10.65 / 15,
  39 / 888 * 50,
)

## Oxalic Sublimation ------------------------------------------------------
# Product: Api Bioxal
# Cost:    14.50 / 35g
# Appl.:   2.3g / Colony
# Source:  https://shop.garten-bienen.at/fuer-gesunde-bienen/api-bioxal-oxalsaeure-zur-varroa-winterbehandlung-35-g.html
# Date:    05.10.2020

# Investment
# Product: Tolegano (Gas)
# Cost:    139
# Source:  https://www.weihmayr.com/Oxalsaeure-Verdampfer-gasbetrieben
# Product: Varrox Eddy
# Cost:    390
# Source:  https://shop.garten-bienen.at/fuer-gesunde-bienen/
# Product: Varrox Verdampfer
# Cost:    120
# Source:  https://www.andermatt-biovet.de/de_bvi/varrox-verdampfer-gegen-varroa.html
# Product: Sublimox
# Cost:    395
# Source:  https://www.imkerei-seiringer.com/produkt/sublimox-oxalsaeure-verdampfer/
# Source:  https://imkershop-wien.at/produkt/sublimox-oxalsaeureverdampfer/
# Date:    05.10.2020

investmentCost["Oxal acid - sublimation"] <- mean(
  139, 390, 120, 395
)

# multiply by 2 becuse in summer its probably used more often and in winter only once
ongoingCost["Oxal acid - sublimation"] <- mean(
  14.50 / 15 * 2
)

## Oxalic Mixture ----------------------------------------------------------
# Product: VarroMed
# Cost:    26.00, 25.80
# Appl.:   555ml, 45ml / colony
# Source:  https://shop.bienenlieb.at/?s=ameisens%C3%A4ure
# Source:  https://www.wachs-hoedl.com/VarroMed-555-ml
# Date:    05.10.2020

# Product: Bienenwohl
# Cost:    36.90 / liter
# Appl.:   50ml / colony
# Source:  https://imkershop-wien.at/produkt/bienenwohl-10-liter/
# Date:    05.10.2020

ongoingCost["Oxal acid - mixture"] <- mean(
  26 / 555 * 45,
  36.90 * 0.05
)

## Thymol ------------------------------------------------------------------
# Product: Apiguard
# Cost:    3.50 / gel    2 * gel / colony
# Source:  https://de.swienty.com/pi/Apiguard-Varroabehandlung_4003894_164663.aspx?CountryID=2&LanguageId=5&CurrencyId=11
# Date:    05.10.2020

# Product: ApiLifeVar
# Cost:    3.00     2 * / colony
# Source:  https://www.wachs-hoedl.com/Api-Life-Var-2-Plaettchen
# Date:    05.10.2020

# Product: Thymovar
# Cost:    26.50 / 10 Strips -> 2 / Colony

ongoingCost["Thymol"] <- mean(
  3.50 * 2, 3.00 * 2, 26.5 / 10 * 2
)

## Synthetical -------------------------------------------------------------
# Product: Polyvar
# Cost:    30.00  / 10 Strips
# Appl.:   1 Strip per Colony
# Source:  https://www.medizinfuchs.at/preisvergleich/polyvar-yellow-275-mg-impraeg.str.f.d.bienenstock-10-st-bayer-vital-gmbh-gb-tiergesundheit-pzn-13413376.html
# Date:    05.10.2020

# Product: Apitraz
# Cost:    35.00 / 10 Strips
# Appl.:   2 Strip per Colony
# Source:  https://www.soin-et-nature.com/de/apiculture/19370-apitraz-beutel-mit-10-streifen-25-cm-500mg-amitraz-5-bienenstocke.html
# Date:    05.10.2020

# Product: Apivar
# Cost:    33.80 / 10 Strips
# Appl.:   2 Strip per Colony
# Source:  https://agrocenter.it/Apivar-500mg-10-Streifen
# Date:    05.10.2020

ongoingCost["Synthetic methods"] <- mean(
  30 / 10,
  35 / 5,
  33.80 / 10
)


# Investment & Material Deduction Time -------------------------------------
# estimated years of usage
deInvestment <- 10
deMaterial <- 5
investmentCost <- investmentCost / deInvestment
materialCost <- materialCost / deMaterial


# Clean Up ----------------------------------------------------------------
ESTIMATED_COSTS <- tibble(
  investment  = round(investmentCost, 2),
  material    = round(materialCost, 2),
  consumables = round(ongoingCost, 2)
)
treatmentList <- cbind(treatmentList, ESTIMATED_COSTS)

rm(deInvestment, deMaterial, materialCost, ongoingCost, investmentCost, ESTIMATED_COSTS)