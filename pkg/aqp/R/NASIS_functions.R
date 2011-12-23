## consider moving into a new package: NRCS


# place-holder for functions similar to the pedonPC functions

## note that:
## local NASIS DSN must be setup (see instructions)
## local NASIS uses coded values... need to decode in queries
## MS SQL dialect is strange

# get_site_data_from_nasis <- function(dsn, usr, pwd)

## example
# SELECT usiteid, md_drainage.ChoiceName AS drainage_class
# FROM 
# dbo.site JOIN (SELECT * FROM dbo.MetadataDomainDetail WHERE dbo.MetadataDomainDetail.DomainID = 148) AS md_drainage ON md_drainage.ChoiceValue = dbo.site.drainagecl


# get_hz_data_from_nasis <- function(dsn, usr, pwd)
# get_colors_data_from_nasis <- function(dsn, usr, pwd)



