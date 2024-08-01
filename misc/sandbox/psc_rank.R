
library(aqp)


# PSC ----
data("soiltexture")
st <- soiltexture$values
st <- st |> 
  within({
    frags = mean(0:14) |> round(2)
    ash   = 0
  })


# skeletal
st_sk <- st
st_sk$frags <- mean(35:89) |> round(2)


# fragmental
st_fg <- st_sk |>
  # subset(texcl == "s") |>
  within({
    # sand = sand + 3
    # silt = silt - 1
    # clay = clay - 2
    frags = round(mean(90:100), 2)
  })


st <- rbind(st, st_sk, st_fg)


# calculate PSC ----
psc <- texture_to_taxpartsize(
  texcl = st$texcl, 
  clay = st$clay, 
  sand = st$sand, 
  fragvoltot = st$frags
  ) |>
  cbind(st, psc = _)

psc <- aggregate(
  cbind(sand, silt, clay, frags) ~ psc, 
  data = psc, 
  function(x) round(mean(x, na.rm = TRUE))
  ) |>
  dplyr::arrange(frags, -clay, sand)


# special classess ----
clayey <- psc |>
  subset(psc %in% c("fine", "very-fine")) |>
  within({psc = "clayey"}) |>
  aggregate(
    cbind(sand, silt, clay, frags) ~ psc, 
    data = _,
    function(x) round(mean(x))
    )

loamy <- psc |>
  subset(psc %in% c("coarse-loamy", "fine-loamy")) |>
  within({psc = "loamy"}) |>
  aggregate(
    cbind(sand, silt, clay, frags) ~ psc, 
    data = _,
    function(x) round(mean(x))
  )


## ashy ----
ashy <- psc |>
  subset(sand >= 30 & frags < 35) |>
  within({
    psc = "ashy"
    tension = mean(0:30)
    ash = mean(30:100)
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags, ash, tension) ~ psc,
    data = _,
    function(x) round(mean(x))
  )

# ashy <- psc |>
#   subset(psc %in% c("coarse-silty")) |>
#   within({
#     psc = "ashy"
#     sand = sand + 30
#     silt = silt - 28
#     clay = clay - 2
#     ash = 60
#   }) |>
#   aggregate(
#     cbind(sand, silt, clay, frags, ash) ~ psc,
#     data = _,
#     function(x) round(mean(x), 2)
#   )

ashy_sk <- ashy |>
  within({
    psc = "ashy-skeletal"
    frags = round(mean(35:89))
  })

ashy_pu <- ashy_sk |>
  within({
    psc = "ashy-pumiceous"
    # sand = sand - 1
    # silt = silt + 0.9
    # clay = clay + 0.1
    frags = round(frags * 2/3)
  })


cindery <- psc |>
  subset((sand + silt) >= 60 & frags < 35) |>
  within({
    psc = "cindery"
    # sand = sand - 59
    # silt = silt + 60
    # clay = clay - 1
    ash  = 80
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags, ash) ~ psc, 
    data = _,
    function(x) round(mean(x))
  ) |>
  within({
    frags = round(mean(60:89))
    tension = NA
  })


pumiceous <- cindery |>
  within({
    psc = "pumiceous"
    # sand = sand - 1
    # silt = silt + 0.5
    # clay = clay + 0.5
    frags = round(frags * 2/3)
  })


## medial ----
medial <- psc |>
  subset(frags < 35) |>
  within({
    psc = "medial"
    tension = mean(30:99)
    # sand = sand + 30
    # silt = silt - 28
    # clay = clay - 2
    # ash = 60
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags, tension) ~ psc,
    data = _,
    function(x) round(mean(x))
  )
medial <- cbind(
  medial[1:5],
  ash = NA,
  medial[6]
)


medial_sk <- medial |>
  within({
    psc = "medial-skeletal"
    frags = round(mean(35:89))
  })

medial_pu <- medial_sk |>
  within({
    psc = "medial-pumiceous"
    frags = round(frags * 2/3)
  })


## hydrous ----
hydrous <- psc |>
  subset(psc %in% c("fine", "very fine")) |>
  within({
    psc = "hydrous"
    # sand = sand - 18
    # silt = silt + 20
    # clay = clay - 2
    # ash = 60
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags) ~ psc,
    data = _,
    function(x) round(mean(x), 2)
  ) |>
  within({
    ash = NA
    tension = 100
  })


hydrous_sk <- hydrous |>
  within({
    psc = "hydrous-skeletal"
    frags = round(mean(35:89))
  })

hydrous_pu <- hydrous_sk |>
  within({
    psc = "hydrous-pumiceous"
    frags = round(frags * 2/3)
  })



## diatomaceous ----
diatomaceous <- data.frame(
    psc = "diatomaceous",
    clay = NA,
    sand = NA,
    silt = NA,
    frags = NA,
    ash  = NA,
    tension = NA,
    limnic = 1,
    gypsum = 0
  )


## gypseous ----
co_gypseous <- psc |>
  subset(sand >= 50 & frags < 35) |>
  within({
    psc = "coarse-gypseous"
    # sand = sand + 40
    # silt = silt - 39
    # clay = clay - 1
    # ash = 60
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags) ~ psc,
    data = _,
    function(x) round(mean(x))
  ) |>
  within({
    ash = NA
    tension = NA
    limnic = 0
    gypsum = mean(40:100)
  })


fi_gypseous <- psc |>
  subset(sand < 50 & frags < 35) |>
  within({
    psc = "fine-gypseous"
    # sand = sand + 40
    # silt = silt - 39.8
    # clay = clay - 0.2
    # ash = 60
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags) ~ psc,
    data = _,
    function(x) round(mean(x), 2)
  ) |>
  within({
    ash = NA
    tension = NA
    limnic = 0
    gypsum = mean(40:100)
  })


gypseous_sk <- rbind(fi_gypseous, co_gypseous) |>
  within({
    psc = "gypseous-skeletal"
    frags = round(mean(35:89), 2)
  }) |>
  aggregate(
    cbind(sand, silt, clay, frags) ~ psc,
    data = _,
    function(x) round(mean(x), 2)
  ) |>
  within({
    ash = NA
    tension = NA
    limnic = 0
    gypsum = mean(40:100)
  })


# combine psc ----
psc_l <- list(
  psc = psc, 
  clayey = clayey, 
  loamy  = loamy, 
  ashy   = ashy, 
  ashy_sk = ashy_sk, 
  ashy_pm = ashy_pu, 
  cindery = cindery, 
  pumiceous = pumiceous,
  medial = medial, 
  medial_sk = medial_sk, 
  medial_pu = medial_pu,
  hydrous = hydrous, 
  hydrous_sk = hydrous_sk, 
  hydrous_pu = hydrous_pu,
  diatomaceous = diatomaceous,
  fi_gypseous = fi_gypseous,
  co_gypseous = co_gypseous,
  gypseous_sk = gypseous_sk
  )
psc_l[1:3]  <- lapply(psc_l[1:3], function(x) cbind(x, ash = NA, tension = NA, limnic = 0, gypsum = NA))
psc_l[4:14]  <- lapply(psc_l[4:14], function(x) cbind(x, limnic = 0, gypsum = NA))
psc0 <- do.call("rbind", psc_l)
psc0 <- psc0[order(-psc0$limnic, psc0$frags, -psc0$clay, -psc0$tension, -psc0$gypsum), ]
row.names(psc0) <- NULL
# # rm(list = names(psc_l)[[-1]])
# 
# psc <- psc[order(psc$frags, -psc$limnic, psc$frags, -psc$clay), ]
# row.names(psc) <- NULL
# 
# psc1 <- psc0
# idx <- which(psc1$frags > 35)
# psc1[idx, 2:4] <- apply(psc1[idx, 2:4], 2, function(x) -100 - x* -1)
# psc1 <- psc1[order(psc1$frags, -psc1$clay), ]
# row.names(psc1) <- NULL

psc1$psc  |> dput()
psc1$clay |> dput()


# ordination
library(cluster)
library(vegan)
library(compositions)

psc_acomp <- acomp(psc[2:4])

# psc_pc <- psc_acomp |>
#   cbind(psc[5]) |>
#   princomp()
# plot(psc_pc$scores, type = "n")
# text(psc_pc$scores)


d <- psc[-1] |>
  daisy() |>
  round(2)

psc_pc <- psc1[c(2, 4, 5)] |>
  princomp()
plot(psc_pc$scores, type = "n")
text(psc_pc$scores)

psc_mds <- metaMDS(
  psc0[, c(2, 3, 4, 5)],
  k = 1,
  distance = "euclidean", 
  autotransform = FALSE, 
  wascores = FALSE
  )
plot(psc_mds, type = "t")

rank <- psc_mds$points[, 1]







