library(RODBC)

# setup connection to our pedon database
channel <- odbcConnect('nasis_local', uid='NasisSqlRO', pwd='nasisRe@d0n1y')

# exec query
d <- sqlQuery(channel, "SELECT * FROM MetadataDomainDetail", stringsAsFactors=FALSE)

# close connection
odbcClose(channel)