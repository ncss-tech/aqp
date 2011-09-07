
# mixes colors whene there are multiple values per / horizon
# TODO: this is only used by pedonPC functions, and could be generalized
# TODO: seems like this could be faster
mix_and_clean_colors <- function(x)
  {
  # fill missing weights with 1
  x$pct[is.na(x$pct)] <- 1
  
  # skip horizons with a single color
  tab <- table(x$hz_id)
  if(tab > 1)
    {
    r <- with(x, wtd.mean(r, weights=pct))
    g <- with(x, wtd.mean(g, weights=pct))
    b <- with(x, wtd.mean(b, weights=pct))
    # composite
    df <- data.frame(r,g,b)
    }
  else
    df <- x[, c('r','g','b')]
  
  # fill missing colors with white
  df$r[is.na(df$r)] <- 1
  df$g[is.na(df$g)] <- 1
  df$b[is.na(df$b)] <- 1
  
  # done
  return(df)
  }


# get site/pedon aggregate data
# note that we use LEFT joins with the metadata table, in case those fields are NULL
get_site_data_from_pedon_db <- function(dsn)
  {
  q <- "SELECT site.usiteid as site_id, pedon.upedonid as pedon_id, 
  latdegrees + IIF(IsNull(latminutes), 0, latminutes/ 60.0) + IIF(IsNULL(latseconds), 0, latseconds / 60.0 / 60.0) as y,
  -(longdegrees + IIF(IsNull(longminutes), 0, longminutes / 60.0) + IIF(IsNull(longseconds), 0, longseconds / 60.0 / 60.0)) as x,
  dm.choice as datum,
  elev, slope, aspect, plantassocnm,
  bedrckdepth, br.choice_label as bedrock_kind,
  hs.choice AS hillslope_pos,
  descname as describer, soinmassamp as sampled_as, soinmascorr as correlated_as, psctopdepth, pscbotdepth,
  ps.choice_label as part_size_class
  FROM (
  (
  (
  (
  site INNER JOIN pedon ON site.siteiid = pedon.siteiidref)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 1261) AS dm ON site.horizdatnm = dm.choice_id) 
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 517) AS br ON site.bedrckkind = br.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 127) AS ps ON pedon.taxpartsize = ps.choice_id)
  LEFT OUTER JOIN (SELECT * FROM metadata_domain_detail WHERE domain_id = 971) AS hs ON site.hillslopeprof = hs.choice_id
  ORDER BY site.usiteid ;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q)

  # close connection
  odbcClose(channel)
  
  # fix column types
  d$site_id <- as.character(d$site_id)
  d$pedon_id <- as.character(d$pedon_id)
  d$datum <- as.character(d$datum)
  
  # warn if mixed datums
  if(length(unique(na.omit(d$datum))) > 1)
    warning('multiple datums present')
  
  # done
  return(d)
  }


get_hz_data_from_pedon_db <- function(dsn)
  {
  require(RODBC)
  
  # this can be optimized
  # RF calculation should be done in  a sub-query
  q <- "SELECT pedon.upedonid as pedon_id, phorizon.phiid as hz_id,
  phorizon.hzname, phorizon.hzdept, phorizon.hzdepb,
  phorizon.claytotest as clay, phorizon.silttotest as silt, phorizon.sandtotest as sand, phfield,
  Sum(phfrags.fragvol)  AS total_frags_pct
FROM (pedon INNER JOIN phorizon ON (pedon.pedbsidref = phorizon.pedbsidref) AND (pedon.peiid = phorizon.peiidref)) LEFT OUTER JOIN phfrags ON (phorizon.pedbsidref = phfrags.pedbsidref) AND (phorizon.phiid = phfrags.phiidref)
GROUP BY pedon.upedonid, phorizon.phiid, phorizon.hzname, phorizon.hzdept, phorizon.hzdepb, phorizon.claytotest, phorizon.silttotest, phorizon.sandtotest, phfield
ORDER BY pedon.upedonid, phorizon.hzdept ASC ;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q)

  # close connection
  odbcClose(channel)
  
  # fix column types
  d$pedon_id <- as.character(d$pedon_id)
  d$hz_id <- as.character(d$hz_id)
  d$hzname <- as.character(d$hzname)
  
  # done
  return(d)
  }


# this does all of the hard work, dsn is to a file
get_colors_from_pedon_db <- function(dsn)
  {
  require(RODBC)  
  require(plyr)
  require(Hmisc)
  
  # color data... check
  q <- "SELECT pedon.upedonid as pedon_id, phorizon.phiid as hz_id, colormoistst, colorpct as pct, mh.choice AS colorhue, colorvalue, colorchroma
FROM (
(pedon INNER JOIN phorizon ON pedon.peiid = phorizon.peiidref)
INNER JOIN phcolor ON phorizon.phiid = phcolor.phiidref)
INNER JOIN metadata_domain_detail AS mh ON phcolor.colorhue = mh.choice_id
WHERE mh.domain_id = 1242
ORDER BY pedon.upedonid, phiidref, colormoistst;"
  
  # setup connection to our pedon database
  channel <- odbcConnectAccess(dsn, readOnlyOptimize=TRUE)

  # exec query
  cat(paste('fetching from', dsn, '...\n'))
  d <- sqlQuery(channel, q)

  # close connection
  odbcClose(channel)
  
  # convert factor to character
  d$pedon_id <- as.character(d$pedon_id)  
  d$colorhue <- as.character(d$colorhue)

  # convert Munsell to RGB---> note: this is slow
  cat('converting Munsell to RGB ...\n')
  d.rgb <- with(d, munsell2rgb(colorhue, colorvalue, colorchroma, return_triplets=TRUE))

  # re-combine
  d <- cbind(d, d.rgb)

  # split into dry / moist
  dry.colors <- subset(d, d$colormoistst == 1)
  moist.colors <- subset(d, d$colormoistst == 2)
  
  # mix and clean colors
  cat('mixing and cleaning colors ...\n')
  dry.colors.final <- ddply(dry.colors, .(pedon_id, hz_id), mix_and_clean_colors, .progress='text')
  moist.colors.final <- ddply(dry.colors, .(pedon_id, hz_id), mix_and_clean_colors, .progress='text')

  # rename columns
  names(dry.colors.final) <- c('pedon_id','hz_id','d_r','d_g','d_b')
  names(moist.colors.final) <- c('pedon_id','hz_id','m_r','m_g','m_b')

  # merge into single df
  d.final <- join(dry.colors.final, moist.colors.final, type='full')
  
  # clean-up
  rm(d, d.rgb, dry.colors, moist.colors, dry.colors.final, moist.colors.final)
  gc()
  
  # done
  return(d.final)
  }
