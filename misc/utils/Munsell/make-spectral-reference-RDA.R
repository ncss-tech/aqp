## Process spectral reference data from CIE
## D.E. Beaudette
## 2021-04-13

## CIE reference data at 5nm resolution
# see cie.15.2004.tables.xls
# subset to 380--730nm
spectral.reference <- read.csv('spectral-reference-data.csv')

# save to local data
save(spectral.reference, file = '../../../data/spectral.reference.rda', compress = 'xz')
