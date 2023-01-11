
# https://dwheeler.com/sloc/

# COCOMO II model
# http://softwarecost.org/tools/COCOMO/

# Total Physical Source Lines of Code (SLOC)                   = 30152114
# Estimated Development Effort in Person-Years (Person-Months) = 7955.75 (95469)
# (Basic COCOMO model, Person-Months = 2.4 * (KSLOC**1.05))
# Estimated Schedule in Years (Months)                         = 6.53 (78.31)
# (Basic COCOMO model, Months = 2.5 * (person-months**0.38))
# Total Estimated Cost to Develop                              = $ 1074713481
# (average salary = $56286/year, overhead = 2.4).


# https://github.com/sgibb/sgtools
remotes::install_github('sgibb/sgtools')

library(sgtools)

f <- sloc.files('R')
sum(f$sloc)
