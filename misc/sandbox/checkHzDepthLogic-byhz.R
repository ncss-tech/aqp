
library(aqp)

data("jacobs2000")

# making checks by horizon is actually pretty straightforward to implement with existing hzDepthTests

checkHzDepthLogic(jacobs2000)
#>     id valid depthLogic sameDepth missingDepth overlapOrGap
#> 1 92-1  TRUE      FALSE     FALSE        FALSE        FALSE
#> 2 92-2  TRUE      FALSE     FALSE        FALSE        FALSE
#> 3 92-3  TRUE      FALSE     FALSE        FALSE        FALSE
#> 4 92-4  TRUE      FALSE     FALSE        FALSE        FALSE
#> 5 92-5  TRUE      FALSE     FALSE        FALSE        FALSE
#> 6 92-6  TRUE      FALSE     FALSE        FALSE        FALSE
#> 7 92-7  TRUE      FALSE     FALSE        FALSE        FALSE

checkHzDepthLogic(jacobs2000, fast=TRUE)
#>     id valid
#> 1 92-1  TRUE
#> 2 92-2  TRUE
#> 3 92-3  TRUE
#> 4 92-4  TRUE
#> 5 92-5  TRUE
#> 6 92-6  TRUE
#> 7 92-7  TRUE

checkHzDepthLogic(jacobs2000, byhz=TRUE)
#>      id top bottom hzID valid depthLogic sameDepth missingDepth overlapOrGap
#> 1  92-1   0     18    1  TRUE      FALSE     FALSE        FALSE        FALSE
#> 2  92-1  18     43    2  TRUE      FALSE     FALSE        FALSE        FALSE
#> 3  92-1  43     79    3  TRUE      FALSE     FALSE        FALSE        FALSE
#> 4  92-1  79    130    4  TRUE      FALSE     FALSE        FALSE        FALSE
#> 5  92-1 130    153    5  TRUE      FALSE     FALSE        FALSE        FALSE
#> 6  92-1 153    156    6  TRUE      FALSE     FALSE        FALSE        FALSE
#> 7  92-1 156    213    7  TRUE      FALSE     FALSE        FALSE        FALSE
#> 8  92-2   0     18    8  TRUE      FALSE     FALSE        FALSE        FALSE
#> 9  92-2  18     46    9  TRUE      FALSE     FALSE        FALSE        FALSE
#> 10 92-2  46     84   10  TRUE      FALSE     FALSE        FALSE        FALSE
#> 11 92-2  84    122   11  TRUE      FALSE     FALSE        FALSE        FALSE
#> 12 92-2 122    145   12  TRUE      FALSE     FALSE        FALSE        FALSE
#> 13 92-2 145    213   13  TRUE      FALSE     FALSE        FALSE        FALSE
#> 14 92-3   0     15   14  TRUE      FALSE     FALSE        FALSE        FALSE
#> 15 92-3  15     25   15  TRUE      FALSE     FALSE        FALSE        FALSE
#> 16 92-3  25     64   16  TRUE      FALSE     FALSE        FALSE        FALSE
#> 17 92-3  64     84   17  TRUE      FALSE     FALSE        FALSE        FALSE
#> 18 92-3  84    112   18  TRUE      FALSE     FALSE        FALSE        FALSE
#> 19 92-3 112    165   19  TRUE      FALSE     FALSE        FALSE        FALSE
#> 20 92-3 165    175   20  TRUE      FALSE     FALSE        FALSE        FALSE
#> 21 92-4   0     20   21  TRUE      FALSE     FALSE        FALSE        FALSE
#> 22 92-4  20     53   22  TRUE      FALSE     FALSE        FALSE        FALSE
#> 23 92-4  53     79   23  TRUE      FALSE     FALSE        FALSE        FALSE
#> 24 92-4  79    130   24  TRUE      FALSE     FALSE        FALSE        FALSE
#> 25 92-4 130    165   25  TRUE      FALSE     FALSE        FALSE        FALSE
#> 26 92-4 165    185   26  TRUE      FALSE     FALSE        FALSE        FALSE
#> 27 92-4 185    203   27  TRUE      FALSE     FALSE        FALSE        FALSE
#> 28 92-5   0     28   28  TRUE      FALSE     FALSE        FALSE        FALSE
#> 29 92-5  28     61   29  TRUE      FALSE     FALSE        FALSE        FALSE
#> 30 92-5  61    109   30  TRUE      FALSE     FALSE        FALSE        FALSE
#> 31 92-5 109    135   31  TRUE      FALSE     FALSE        FALSE        FALSE
#> 32 92-5 135    183   32  TRUE      FALSE     FALSE        FALSE        FALSE
#> 33 92-6   0     18   33  TRUE      FALSE     FALSE        FALSE        FALSE
#> 34 92-6  18     46   34  TRUE      FALSE     FALSE        FALSE        FALSE
#> 35 92-6  46     76   35  TRUE      FALSE     FALSE        FALSE        FALSE
#> 36 92-6  76    104   36  TRUE      FALSE     FALSE        FALSE        FALSE
#> 37 92-6 104    119   37  TRUE      FALSE     FALSE        FALSE        FALSE
#> 38 92-6 119    168   38  TRUE      FALSE     FALSE        FALSE        FALSE
#> 39 92-7   0     15   39  TRUE      FALSE     FALSE        FALSE        FALSE
#> 40 92-7  15     41   40  TRUE      FALSE     FALSE        FALSE        FALSE
#> 41 92-7  41     48   41  TRUE      FALSE     FALSE        FALSE        FALSE
#> 42 92-7  48     61   42  TRUE      FALSE     FALSE        FALSE        FALSE
#> 43 92-7  61     91   43  TRUE      FALSE     FALSE        FALSE        FALSE
#> 44 92-7  91    132   44  TRUE      FALSE     FALSE        FALSE        FALSE
#> 45 92-7 132    140   45  TRUE      FALSE     FALSE        FALSE        FALSE
#> 46 92-7 140    152   46  TRUE      FALSE     FALSE        FALSE        FALSE

checkHzDepthLogic(jacobs2000, byhz=TRUE, fast=TRUE)
#>      id top bottom hzID valid
#> 1  92-1   0     18    1  TRUE
#> 2  92-1  18     43    2  TRUE
#> 3  92-1  43     79    3  TRUE
#> 4  92-1  79    130    4  TRUE
#> 5  92-1 130    153    5  TRUE
#> 6  92-1 153    156    6  TRUE
#> 7  92-1 156    213    7  TRUE
#> 8  92-2   0     18    8  TRUE
#> 9  92-2  18     46    9  TRUE
#> 10 92-2  46     84   10  TRUE
#> 11 92-2  84    122   11  TRUE
#> 12 92-2 122    145   12  TRUE
#> 13 92-2 145    213   13  TRUE
#> 14 92-3   0     15   14  TRUE
#> 15 92-3  15     25   15  TRUE
#> 16 92-3  25     64   16  TRUE
#> 17 92-3  64     84   17  TRUE
#> 18 92-3  84    112   18  TRUE
#> 19 92-3 112    165   19  TRUE
#> 20 92-3 165    175   20  TRUE
#> 21 92-4   0     20   21  TRUE
#> 22 92-4  20     53   22  TRUE
#> 23 92-4  53     79   23  TRUE
#> 24 92-4  79    130   24  TRUE
#> 25 92-4 130    165   25  TRUE
#> 26 92-4 165    185   26  TRUE
#> 27 92-4 185    203   27  TRUE
#> 28 92-5   0     28   28  TRUE
#> 29 92-5  28     61   29  TRUE
#> 30 92-5  61    109   30  TRUE
#> 31 92-5 109    135   31  TRUE
#> 32 92-5 135    183   32  TRUE
#> 33 92-6   0     18   33  TRUE
#> 34 92-6  18     46   34  TRUE
#> 35 92-6  46     76   35  TRUE
#> 36 92-6  76    104   36  TRUE
#> 37 92-6 104    119   37  TRUE
#> 38 92-6 119    168   38  TRUE
#> 39 92-7   0     15   39  TRUE
#> 40 92-7  15     41   40  TRUE
#> 41 92-7  41     48   41  TRUE
#> 42 92-7  48     61   42  TRUE
#> 43 92-7  61     91   43  TRUE
#> 44 92-7  91    132   44  TRUE
#> 45 92-7 132    140   45  TRUE
#> 46 92-7 140    152   46  TRUE

# but there is a "paradox" of horizon-level "validity" summaries... they only tell a subset of the story

checkHzDepthLogic(glom(jacobs2000[1,], 50, 100, truncate = TRUE, invert = TRUE))
#>     id valid depthLogic sameDepth missingDepth overlapOrGap
#> 1 92-1 FALSE      FALSE     FALSE        FALSE         TRUE

checkHzDepthLogic(glom(jacobs2000[1,], 50, 100, truncate = TRUE, invert = TRUE), byhz=TRUE)
#>     id top bottom hzID valid depthLogic sameDepth missingDepth overlapOrGap
#> 1 92-1   0     18    1  TRUE      FALSE     FALSE        FALSE        FALSE
#> 2 92-1  18     43    2  TRUE      FALSE     FALSE        FALSE        FALSE
#> 3 92-1  43     50    3  TRUE      FALSE     FALSE        FALSE        FALSE
#> 4 92-1 100    130    4  TRUE      FALSE     FALSE        FALSE        FALSE
#> 5 92-1 130    153    5  TRUE      FALSE     FALSE        FALSE        FALSE
#> 6 92-1 153    156    6  TRUE      FALSE     FALSE        FALSE        FALSE
#> 7 92-1 156    213    7  TRUE      FALSE     FALSE        FALSE        FALSE
