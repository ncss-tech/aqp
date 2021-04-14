## Process spectral reference data from CIE
## D.E. Beaudette
## 2021-04-13




## CIE reference data at 5nm resolution
# http://files.cie.co.at/204.xls
# saved a copy to misc/utils/Munsell/204.zip


# extracted from 204.xls to spectra range in our Munsell spectra
# 380--730nm
spectral.reference <- read.csv('misc/utils/Munsell/spectral-reference-data.csv')

# save to local data
save(spectral.reference, file = 'data/spectral.reference.rda')
