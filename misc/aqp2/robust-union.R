# remotes::install_github('ncss-tech/aqp@union')

library(aqp)
library(soilDB)

x <- fetchOSD('musick')
a <- fetchOSD('sierra')
b <- fetchOSD('holland')
y <- fetchKSSL("musick")
c <- fetchKSSL("sierra")
d <- fetchKSSL("holland")

# do a sim
f <- perturb(c[1,], n = 6)

# do some perturbation of profiles
horizons(x)$bdy <- 0
horizons(a)$bdy <- 0
horizons(b)$bdy <- 0

s <- perturb(x, n = 6, boundary.attr = 'bdy')
profile_id(s) <- 7:12
u <- perturb(a, n = 6, boundary.attr = 'bdy', new.idname = "uID")
profile_id(u) <- 13:18
v <- perturb(b, n = 6, boundary.attr = 'bdy', new.idname = "vID")
profile_id(v) <- 19:24

# Warning message:
# template profile ID 'idname' exists as a non-unique value in SPC element #X, trying 'alternate'

z <- aqp::combine(list(y, c, x, a, u, b, f, d, s, v)) # 7, 8
z <- aqp::combine(list(b, y, u, c, v, d, x, s, a))    # 3
z <- aqp::combine(list(x, d, a, f, u, c, s, b, y, v)) # 5
z <- aqp::combine(list(s, d, a, f, b, y, x, u, c, v)) # 2, 4

# ok, now, break it ...
c(idname(s),idname(u),idname(v))

# works without warning normally...
z <- aqp::combine(list(s, d, f))

# create site attrs so no way to resolve the existing siteNames / new name conflict
site(s)$pedon_key <- LETTERS[1]
site(s)$.new_id <- letters[1]
site(d)$pID <- LETTERS[2]
site(d)$.new_id <- letters[2]
site(f)$pID <- LETTERS[3]
site(f)$pedon_key <- letters[3]

# this should error after exhausting all options for names, despite unique ID values being available
z <- aqp::combine(list(s, d, f))

# Error: unable to identify a safe existing profile ID name to use for result
# In addition: Warning messages:
# 1: template profile ID '.new_id' exists as a non-unique value in SPC element #1, trying 'pID'
# 2: template profile ID 'pID' exists as a non-unique value in SPC element #2, trying 'pedon_key'
# 3: template profile ID 'pedon_key' exists as a non-unique value in SPC element #3, trying '.new_id'

# this works because "foo" doesn't exist in any SPC
z <- pbindlist(list(s, d, f), new.idname = "foo")
