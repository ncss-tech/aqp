# Get column of horizon or site data in a SoilProfileCollection

Get the data from a column accessed by name. Column names other than
profile ID are not shared between site and horizons. Bonus: `[[` gives
access to all site and horizon level variables in tab complete for
RStudio using the magrittr pipe operator!

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
x[[i, j]]
```

## Arguments

- x:

  a SoilProfileCollection

- i:

  an expression resolving to a single column name in site or horizon
  table

- j:

  (not used)

## Examples

``` r
data(sp2)
depths(sp2) <- id ~ top + bottom
site(sp2) <- ~ surface

# get with [[
sp2[['surface']]
#>  [1] holocene        lower modesto   lower modesto   upper modesto  
#>  [5] upper modesto   lower riverbank lower riverbank lower riverbank
#>  [9] holocene        lower riverbank lower laguna    lower laguna   
#> [13] holocene        holocene        upper modesto   upper modesto  
#> [17] upper modesto   upper modesto  
#> 6 Levels: holocene lower modesto upper modesto ... lower laguna

# get using "unknown" expression:
#  "2nd + 3rd horizon column names"
for(i in horizonNames(sp2)[2:3])
 print(sp2[[i]])
#>   [1]   0  12  16  25  49  85  95 122 140   0  22  40  86 132 150 205   0  18
#>  [19]  45  94 125 145 179   0  24  33  50  62  86 102 123 157 208 245 266   0
#>  [37]  28  39  59  76 111 126 150 200 260   0  18  24  76 100 160 201 208   0
#>  [55]  19  30  90 113 170 190 240   0   7  24  63  91 126 176 218 246 268 300
#>  [73] 320   0   4  32  61  92 103   0  11  30  60 114 191 223 260   0  11  33
#>  [91]  46  71  93 119 148 197 274 315 473   0  23  43  59  89 120 154 207 270
#> [109] 342 455 505   0   4  16  31  54   0  17  32  49  86 113   0  13  23  54
#> [127]  88 102 141   0  10  22  43  91 118 157   0   8  30  54  88 118 180 230
#> [145] 290 340   0   7  34  57  90 126 191 240
#>   [1]   12   16   25   49   85   95  122  140  170   22   40   86  132  150  205
#>  [16]  225   18   45   94  125  145  179  220   24   33   50   62   86  102  123
#>  [31]  157  208  245  266  301   28   39   59   76  111  126  150  200  260  320
#>  [46]   18   24   76  100  160  201  208  295   19   30   90  113  170  190  240
#>  [61]  300    7   24   63   91  126  176  218  246  268  300  320  410    4   32
#>  [76]   61   92  103  130   11   30   60  114  191  223  260  283   11   33   46
#>  [91]   71   93  119  148  197  274  315  473 1300   23   43   59   89  120  154
#> [106]  207  270  342  455  505 1300    4   16   31   54   78   17   32   49   86
#> [121]  113  147   13   23   54   88  102  141  180   10   22   43   91  118  157
#> [136]  184    8   30   54   88  118  180  230  290  340  370    7   34   57   90
#> [151]  126  191  240  390

data(sp5)

# some column names to work with
rgb.columns <- c("R25","G25","B25")

res <- lapply(rgb.columns, function(x) {

  # [[ allows you to access column names in a loop
  round(sp5[[x]] * 255)

})

# rename scaled results
names(res) <- paste0(rgb.columns,"_scl")

# add horizon ID to results
result <- data.frame(hzID = hzID(sp5), do.call('cbind', res))
head(result)
#>   hzID R25_scl G25_scl B25_scl
#> 1    1     105      97      87
#> 2    2      79      71      64
#> 3    3      79      71      64
#> 4    4      79      71      64
#> 5    5     140     117      92
#> 6    6     110      94      76

# join result back into horizons
horizons(sp5) <- result
```
