# Retrieve horizon data from SoilProfileCollection

Get horizon data from SoilProfileCollection. Result is returned in the
same `data.frame` class used to initially construct the
SoilProfileCollection.

Horizon data in an object inheriting from `data.frame` can easily be
added via merge (LEFT JOIN). There must be one or more same-named
columns (with at least some matching data) on the left and right hand
side to facilitate the join: `horizons(spc) <- newdata`

## Usage

``` r
# S4 method for class 'SoilProfileCollection'
horizons(object)

# S4 method for class 'SoilProfileCollection'
horizons(object) <- value
```

## Arguments

- object:

  A SoilProfileCollection

- value:

  An object inheriting `data.frame`

## Examples

``` r
# load test data
data(sp2)

# promote to SPC
depths(sp2) <- id ~ top + bottom

# assign true to surface horizon
newdata <- data.frame(top = 0,
                      newvalue = TRUE)

# do left join
horizons(sp2) <- newdata

# inspect site table: newvalue TRUE only for horizons
#  with top depth equal to zero
horizons(sp2)
#>     top     id         surface bottom bound_distinct bound_topography  name
#> 1     0  hon-1        holocene     12              A                S     A
#> 2    12  hon-1        holocene     16              A                S   2A1
#> 3    16  hon-1        holocene     25              C                S   2A2
#> 4    25  hon-1        holocene     49              C                S   2AC
#> 5    49  hon-1        holocene     85              C                S    2C
#> 6    85  hon-1        holocene     95              A                S   3C1
#> 7    95  hon-1        holocene    122           <NA>             <NA>   3C2
#> 8   122  hon-1        holocene    140           <NA>             <NA>   4C1
#> 9   140  hon-1        holocene    170           <NA>             <NA>   4C2
#> 10    0 hon-10   lower modesto     22              C                W    Ap
#> 11   22 hon-10   lower modesto     40              G                S    BA
#> 12   40 hon-10   lower modesto     86              G                S   Bt1
#> 13   86 hon-10   lower modesto    132              C                S   Bt2
#> 14  132 hon-10   lower modesto    150              C                S    BC
#> 15  150 hon-10   lower modesto    205              A                W     C
#> 16  205 hon-10   lower modesto    225           <NA>             <NA>    2C
#> 17    0 hon-11   lower modesto     18              A                W    Ap
#> 18   18 hon-11   lower modesto     45              G                S    BA
#> 19   45 hon-11   lower modesto     94              G                S   Bt1
#> 20   94 hon-11   lower modesto    125              C                W   Bt2
#> 21  125 hon-11   lower modesto    145              G                W    BC
#> 22  145 hon-11   lower modesto    179              A                W     C
#> 23  179 hon-11   lower modesto    220           <NA>             <NA>    2C
#> 24    0 hon-13   upper modesto     24              A                S    Ap
#> 25   24 hon-13   upper modesto     33              C                S    BA
#> 26   33 hon-13   upper modesto     50              C                S  BAt1
#> 27   50 hon-13   upper modesto     62              C                S  BAt2
#> 28   62 hon-13   upper modesto     86              C                S   Bt1
#> 29   86 hon-13   upper modesto    102              C                S   Bt2
#> 30  102 hon-13   upper modesto    123              C                S   Bt3
#> 31  123 hon-13   upper modesto    157              G                W   BCt
#> 32  157 hon-13   upper modesto    208           <NA>             <NA> 2BCt1
#> 33  208 hon-13   upper modesto    245              G                W 2BCt2
#> 34  245 hon-13   upper modesto    266              D                W 2BCt3
#> 35  266 hon-13   upper modesto    301           <NA>             <NA>   2CB
#> 36    0 hon-14   upper modesto     28              A                S    Ap
#> 37   28 hon-14   upper modesto     39              C                S    BA
#> 38   39 hon-14   upper modesto     59              C                S  BAt1
#> 39   59 hon-14   upper modesto     76              C                S  BAt2
#> 40   76 hon-14   upper modesto    111              C                W   Bt1
#> 41  111 hon-14   upper modesto    126              C                W   Bt2
#> 42  126 hon-14   upper modesto    150              G                W 2BCt1
#> 43  150 hon-14   upper modesto    200              G                W 2BCt2
#> 44  200 hon-14   upper modesto    260              D                W 2BCt3
#> 45  260 hon-14   upper modesto    320           <NA>             <NA>   2CB
#> 46    0 hon-17 lower riverbank     18              C                W   Ap1
#> 47   18 hon-17 lower riverbank     24              A                W   Ap2
#> 48   24 hon-17 lower riverbank     76              G                W    BA
#> 49   76 hon-17 lower riverbank    100              G                W   Bt1
#> 50  100 hon-17 lower riverbank    160              C                W   Bt2
#> 51  160 hon-17 lower riverbank    201              G                W  2BCt
#> 52  201 hon-17 lower riverbank    208              C                S  3BCt
#> 53  208 hon-17 lower riverbank    295           <NA>             <NA>  4BCt
#> 54    0 hon-18 lower riverbank     19              C                W   Ap1
#> 55   19 hon-18 lower riverbank     30              A                W   Ap2
#> 56   30 hon-18 lower riverbank     90              G                W    BA
#> 57   90 hon-18 lower riverbank    113              C                W   Bt1
#> 58  113 hon-18 lower riverbank    170              G                W   Bt2
#> 59  170 hon-18 lower riverbank    190              C                W  2BCt
#> 60  190 hon-18 lower riverbank    240              C                S  3BCt
#> 61  240 hon-18 lower riverbank    300           <NA>             <NA>  4BCt
#> 62    0 hon-19 lower riverbank      7              C                W   Ap1
#> 63    7 hon-19 lower riverbank     24              A                W   Ap2
#> 64   24 hon-19 lower riverbank     63              G                W    BA
#> 65   63 hon-19 lower riverbank     91              C                W   Bt1
#> 66   91 hon-19 lower riverbank    126              G                W   Bt2
#> 67  126 hon-19 lower riverbank    176              C                W   Bt3
#> 68  176 hon-19 lower riverbank    218              C                S   BCt
#> 69  218 hon-19 lower riverbank    246              G                S 2BCt1
#> 70  246 hon-19 lower riverbank    268              G                S 2BCt2
#> 71  268 hon-19 lower riverbank    300           <NA>             <NA> 2BCt3
#> 72  300 hon-19 lower riverbank    320           <NA>             <NA>  3Cox
#> 73  320 hon-19 lower riverbank    410           <NA>             <NA>  3Cox
#> 74    0  hon-2        holocene      4              A                S    A1
#> 75    4  hon-2        holocene     32              A                W    A2
#> 76   32  hon-2        holocene     61              C                S     C
#> 77   61  hon-2        holocene     92              C                S    2C
#> 78   92  hon-2        holocene    103           <NA>             <NA>    3C
#> 79  103  hon-2        holocene    130           <NA>             <NA>    4C
#> 80    0 hon-20 lower riverbank     11              C                W   Ap1
#> 81   11 hon-20 lower riverbank     30              A                W   Ap2
#> 82   30 hon-20 lower riverbank     60              C                W    BA
#> 83   60 hon-20 lower riverbank    114              C                I   Bt1
#> 84  114 hon-20 lower riverbank    191              C                W   Bt2
#> 85  191 hon-20 lower riverbank    223              G                S 2BCt1
#> 86  223 hon-20 lower riverbank    260              G                S 2BCt2
#> 87  260 hon-20 lower riverbank    283           <NA>             <NA>   2CB
#> 88    0 hon-21    lower laguna     11              C                S     A
#> 89   11 hon-21    lower laguna     33              C                W    AB
#> 90   33 hon-21    lower laguna     46              A                W    BA
#> 91   46 hon-21    lower laguna     71              C                W   Bt1
#> 92   71 hon-21    lower laguna     93              C                W   Bt2
#> 93   93 hon-21    lower laguna    119              C                W   Bt3
#> 94  119 hon-21    lower laguna    148              G                W   Bt4
#> 95  148 hon-21    lower laguna    197              G                W  2Bt1
#> 96  197 hon-21    lower laguna    274              D                W  2Bt2
#> 97  274 hon-21    lower laguna    315              D                W   2BC
#> 98  315 hon-21    lower laguna    473              A                I   3BC
#> 99  473 hon-21    lower laguna   1300           <NA>             <NA>     C
#> 100   0 hon-22    lower laguna     23              C                W     A
#> 101  23 hon-22    lower laguna     43              C                W    AB
#> 102  43 hon-22    lower laguna     59              A                W    BA
#> 103  59 hon-22    lower laguna     89              C                W   Bt1
#> 104  89 hon-22    lower laguna    120              C                W   Bt2
#> 105 120 hon-22    lower laguna    154              G                S   Bt3
#> 106 154 hon-22    lower laguna    207              G                S  2Bt1
#> 107 207 hon-22    lower laguna    270              G                W  2Bt2
#> 108 270 hon-22    lower laguna    342              G                W  2BCt
#> 109 342 hon-22    lower laguna    455              G                W 3BCt1
#> 110 455 hon-22    lower laguna    505              A                W 3BCt2
#> 111 505 hon-22    lower laguna   1300           <NA>             <NA>     C
#> 112   0  hon-3        holocene      4              A                S    A1
#> 113   4  hon-3        holocene     16              G                S    A2
#> 114  16  hon-3        holocene     31              A                W    AC
#> 115  31  hon-3        holocene     54              A                W     C
#> 116  54  hon-3        holocene     78           <NA>             <NA>    2C
#> 117   0  hon-4        holocene     17              A                S     C
#> 118  17  hon-4        holocene     32              C                S    2A
#> 119  32  hon-4        holocene     49              C                S   2AC
#> 120  49  hon-4        holocene     86              G                S    2C
#> 121  86  hon-4        holocene    113              G                S   3C1
#> 122 113  hon-4        holocene    147           <NA>             <NA>   3C2
#> 123   0  hon-5   upper modesto     13              A                W   Ap1
#> 124  13  hon-5   upper modesto     23              C                W   Ap2
#> 125  23  hon-5   upper modesto     54              C                S    BA
#> 126  54  hon-5   upper modesto     88              C                S   Bw1
#> 127  88  hon-5   upper modesto    102              C                S   Bw2
#> 128 102  hon-5   upper modesto    141              G                S   BC1
#> 129 141  hon-5   upper modesto    180           <NA>             <NA>   BC2
#> 130   0  hon-6   upper modesto     10              A                W   Ap1
#> 131  10  hon-6   upper modesto     22              C                W   Ap2
#> 132  22  hon-6   upper modesto     43              C                S    BA
#> 133  43  hon-6   upper modesto     91              C                S   Bw1
#> 134  91  hon-6   upper modesto    118              C                S   Bw2
#> 135 118  hon-6   upper modesto    157              G                S   BC1
#> 136 157  hon-6   upper modesto    184           <NA>             <NA>   BC2
#> 137   0  hon-7   upper modesto      8              C                S   Ap1
#> 138   8  hon-7   upper modesto     30              C                S   Ap2
#> 139  30  hon-7   upper modesto     54              G                S    BA
#> 140  54  hon-7   upper modesto     88              G                S   Bw1
#> 141  88  hon-7   upper modesto    118              G                S   Bw2
#> 142 118  hon-7   upper modesto    180              C                S    BC
#> 143 180  hon-7   upper modesto    230           <NA>             <NA>   2BC
#> 144 230  hon-7   upper modesto    290           <NA>             <NA>  3Cox
#> 145 290  hon-7   upper modesto    340           <NA>             <NA>  4Cox
#> 146 340  hon-7   upper modesto    370           <NA>             <NA>   4Cn
#> 147   0  hon-8   upper modesto      7              A                S  Ap-1
#> 148   7  hon-8   upper modesto     34              C                S  Ap-2
#> 149  34  hon-8   upper modesto     57              C                S    BA
#> 150  57  hon-8   upper modesto     90              C                W  Bw-1
#> 151  90  hon-8   upper modesto    126              G                S  Bw-2
#> 152 126  hon-8   upper modesto    191              G                S   BC1
#> 153 191  hon-8   upper modesto    240           <NA>             <NA>   BC2
#> 154 240  hon-8   upper modesto    390           <NA>             <NA>  2Cox
#>      texture prop structure_grade structure_size structure_type stickiness
#> 1          L   16               1              F             GR         SS
#> 2        sil   17               1              F             GR         SS
#> 3          L   16               1              F             GR         SS
#> 4          L   16               0           <NA>             MA         SS
#> 5          L   13              NA           <NA>             MA         SS
#> 6         sl   11              NA           <NA>             MA         SO
#> 7         sl   12              NA           <NA>             MA         SO
#> 8       <NA>   11              NA           <NA>           <NA>       <NA>
#> 9       <NA>   13              NA           <NA>           <NA>       <NA>
#> 10      sgsl   11               1             CO            SBK         SS
#> 11      sgsl   12              NA           <NA>             MA         SO
#> 12      sgsl   12               2           <NA>             MA         SS
#> 13      sgsl   14               2           <NA>             MA         SS
#> 14      sgsl   11               2           <NA>             MA         SS
#> 15      sgsl    8              NA           <NA>             MA         SO
#> 16       fsl    8              NA           <NA>             MA         SO
#> 17      sgsl   14               1              M            SBK         SS
#> 18      sgsl   12              NA           <NA>             MA         SS
#> 19      sgsl   13               2           <NA>             MA         SS
#> 20      sgsl   15               2           <NA>             MA         MS
#> 21      sgsl   11               2           <NA>             MA         SS
#> 22      sgsl   10              NA           <NA>             MA         SO
#> 23       sil    8              NA           <NA>             MA         SS
#> 24       sil   17              NA           <NA>            CDY         SS
#> 25     sil-l   20              NA           <NA>             MA         SS
#> 26     sil-l   23               2           <NA>             MA         SS
#> 27        l+   26               2              M             PR         MS
#> 28        c-   43               3              M             PR         VS
#> 29       SIC   51               3           <NA>             MA         VS
#> 30      SIC-   42               3              M            ABK         MS
#> 31      sicl   34               3              M            ABK         SS
#> 32      <NA>   26              NA           <NA>           <NA>       <NA>
#> 33        cl   24              NA           <NA>             MA         SS
#> 34      scl-   21              NA           <NA>             MA         SS
#> 35        sl   17              NA           <NA>             MA         SO
#> 36       sil   16              NA           <NA>             MA         SS
#> 37       sil   22              NA           <NA>             MA         SS
#> 38       sil   23               2              M            SBK         SS
#> 39        cl   30               2              F             PR         MS
#> 40       SIC   45               3              M             PR         VS
#> 41        cl   37               3              M            ABK         MS
#> 42        cl   34               3              M            ABK         MS
#> 43   cl-sicl   30               2           <NA>             MA         SS
#> 44       cl-   27              NA           <NA>             MA         SS
#> 45      scl-   23              NA           <NA>             MA         SO
#> 46         l   18              NA           <NA>            CDY         SS
#> 47         l   18              NA           <NA>             MA         SS
#> 48       cl-   27               2              M             MA         MS
#> 49        cl   30               2           <NA>             MA         MS
#> 50        cl   33               2              F             PR         MS
#> 51       scl   25              NA           <NA>             MA         MS
#> 52       cl-   27              NA           <NA>             MA         MS
#> 53     vgscl   21              NA           <NA>             MA         MS
#> 54         l   17               2              M            SBK         SS
#> 55         l   17              NA           <NA>             MA       <NA>
#> 56         L   26               2             VF           <NA>         MS
#> 57        cl   29               2           <NA>             MA         MS
#> 58      sicl   34               3              M             PR         VS
#> 59        CL   28              NA           <NA>             MA         MS
#> 60        CL   29              NA           <NA>             MA         MS
#> 61    GRVSCL   21              NA           <NA>             MA         SS
#> 62         l   16              NA           <NA>            CDY         SO
#> 63         l   19              NA           <NA>             MA         SO
#> 64        l+   24               2              M            SBK         SS
#> 65        l+   24               2           <NA>             MA         MS
#> 66        cl   33               3              M             PR         MS
#> 67        cl   34               3              F             MA         MS
#> 68      scl-   27               2              M            ABK         SS
#> 69      sgsl   20              NA           <NA>             MA         SS
#> 70      sgsl   15              NA           <NA>             MA         SS
#> 71         l   14              NA           <NA>             MA         SO
#> 72      <NA>   11              NA           <NA>           <NA>       <NA>
#> 73      <NA>   13              NA           <NA>           <NA>       <NA>
#> 74       sil   20               2              F             GR         SS
#> 75     sil-1   20              NA           <NA>             MA         SS
#> 76         L   13              NA           <NA>             MA         SO
#> 77      L-sl   13              NA           <NA>             MA         SS
#> 78         L   16              NA           <NA>             MA         SS
#> 79      <NA>   16              NA           <NA>           <NA>       <NA>
#> 80         l   17              NA           <NA>            CDY         SO
#> 81         l   18              NA           <NA>             MA         SO
#> 82        l+   24               2             VF             MA         SS
#> 83        l+   25               2           <NA>             MA         SS
#> 84        cl   33               3              M             PR         MS
#> 85    sgscl-   22               2             CO            ABK         SS
#> 86     sgsl+   18              NA           <NA>            SGR         SS
#> 87        sl   11              NA           <NA>             MA         SO
#> 88       sl+   18               2             CO            SBK         SO
#> 89       scl   25               2             CO            SBK         SO
#> 90  sgscl-cl   30               2           <NA>             MA         SS
#> 91       sgc   51               3           <NA>             MA         MS
#> 92       sgc   55               1              F             PR         MS
#> 93       sgc   51               1              M             PR         MS
#> 94       sgc   48               1              M             PR         MS
#> 95       grc   45               3           <NA>             MA         SS
#> 96      grcl   35               2           <NA>             MA         SS
#> 97      grcl   33               2           <NA>             MA         SS
#> 98   extg l+   26               2              F           <NA>         SO
#> 99        sl   12              NA           <NA>             MA         SO
#> 100     scl-   22               2             CO            SBK         SS
#> 101    sgscl   29               1              M            SBK         SS
#> 102      sgc   44               2              M            SBK         MS
#> 103      sgc   55               3           <NA>             MA         MS
#> 104      sgc   51               3              M           <NA>         MS
#> 105      sgc   51               3           <NA>             MA         MS
#> 106      grc   47               3           <NA>             MA         MS
#> 107   grcl++   39               3              F            ABK         SS
#> 108    vgscl   30               2           <NA>             MA         SS
#> 109   extgcl   23               1              F           <NA>         SS
#> 110    extg+   34               1              F           <NA>         SS
#> 111       sl   18              NA           <NA>             MA         SO
#> 112      sil   24               2              F             GR         SS
#> 113      sil   22               1              F             GR         SS
#> 114      sil   21               1              M           <NA>         SS
#> 115        L   13              NA           <NA>             MA         SO
#> 116        L   14              NA           <NA>             MA         SS
#> 117       sl    9              NA           <NA>            SGR         SO
#> 118        L   16               2              F             GR         SS
#> 119      sil   17              NA           <NA>             MA         SS
#> 120        L   15              NA           <NA>             MA         SS
#> 121       sl   11              NA           <NA>             MA         SO
#> 122       sl   10              NA           <NA>             MA         SO
#> 123      sil   23              NA           <NA>             MA         SS
#> 124      sil   23               2              F             GR         SS
#> 125     sil+   25              NA           <NA>             MA         SS
#> 126      sil   24               2              M           <NA>         SS
#> 127      sil   23               2              M            ABK         SS
#> 128    sil-1   22               2              M            ABK         SS
#> 129    sil-1   22               1              M            ABK         SS
#> 130      sil   23              NA              M            CDY         SS
#> 131      sil   24              NA           <NA>             MA         SS
#> 132      sil   24              NA           <NA>             MA         SS
#> 133        L   22               2              M            ABK         SS
#> 134        L   22               2              M            ABK         SS
#> 135        L   23               2              M            ABK         SS
#> 136        L   22               1              M            ABK         SS
#> 137        L   19              NA           <NA>            CDY         SS
#> 138        L   20              NA           <NA>             MA         SS
#> 139    sil-1   21              NA           <NA>             MA         SS
#> 140        L   23               2           <NA>             MA         SS
#> 141        L   21               2           <NA>             MA         SS
#> 142     <NA>   22               2           <NA>             MA         SS
#> 143    sicl-   28               1              F           <NA>         SS
#> 144     <NA>   17              NA           <NA>           <NA>       <NA>
#> 145     <NA>    8              NA           <NA>           <NA>       <NA>
#> 146     <NA>    6              NA           <NA>           <NA>       <NA>
#> 147        L   19              NA           <NA>            CDY         SO
#> 148        L   20              NA           <NA>             MA         SO
#> 149        L   23               1              M            SBK         SS
#> 150        L   21               2           <NA>             MA         SS
#> 151        L   21               1           <NA>            SGR         SS
#> 152        L   23               2           <NA>             MA         SS
#> 153    L-sil   19               1           <NA>            SGR         SS
#> 154     <NA>    5              NA           <NA>           <NA>       <NA>
#>     plasticity field_ph     hue value chroma         r         g          b
#> 1           PO      6.5    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 2           PO      6.6    10YR   3.0    2.5        NA        NA         NA
#> 3           SP      6.5    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 4           SP      6.6   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 5           PO      6.8   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 6           PO      6.7   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 7           PO      7.0   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 8         <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 9         <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 10          PO      6.8   7.5YR   3.0    2.0 0.3433252 0.2694211 0.21692139
#> 11          PO      6.8   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 12          SP      6.8   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 13          SP      7.2 5-7.5YR   3.0    4.0        NA        NA         NA
#> 14          PO      7.2   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 15          PO      7.2   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 16          PO      7.2    2.5Y   4.0    3.0 0.4428399 0.3717701 0.24837880
#> 17          PO      6.6   7.5YR   3.0    2.0 0.3433252 0.2694211 0.21692139
#> 18          SP      7.0   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 19          SP      7.0   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 20          MP      7.1   7.5YR   3.0    4.0 0.3827022 0.2567391 0.15069361
#> 21          PO      7.2   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 22          PO      7.3   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 23          PO      7.3    2.5Y   5.0    4.0 0.5620372 0.4702857 0.29727835
#> 24          PO      5.5   7.5YR   6.0    6.0 0.7440717 0.5444687 0.34754585
#> 25          SP      6.3     5YR   4.0    4.0 0.5026314 0.3462401 0.24995558
#> 26          SP      6.5     5YR   4.0    4.0 0.5026314 0.3462401 0.24995558
#> 27          MP      6.8     5YR   4.0    5.0 0.5227043 0.3383386 0.21952529
#> 28          VP      6.8     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 29          VP      6.6     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 30          VP      7.1     5YR   4.0    5.0 0.5227043 0.3383386 0.21952529
#> 31          MP      7.1   7.5YR   4.0    6.0 0.5252055 0.3404370 0.15784923
#> 32        <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 33          SP      7.1     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 34          SP      6.9     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 35          SP      6.7     5YR   4.0    5.0 0.5227043 0.3383386 0.21952529
#> 36          PO      5.6   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 37          SP      6.1   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 38          MP      6.5     5YR   4.0    5.0 0.5227043 0.3383386 0.21952529
#> 39          MP      6.7     5YR   4.0    5.0 0.5227043 0.3383386 0.21952529
#> 40          VP      6.7     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 41          VP      7.0     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 42          MP      7.1   7.5YR   5.0    6.0 0.6373120 0.4408493 0.24846998
#> 43          SP      6.9   7.5YR   5.0    6.0 0.6373120 0.4408493 0.24846998
#> 44          SP      6.8     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 45          SP      6.8     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 46          SP      5.7     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 47          SP      5.7     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 48          SP      6.1   2.5YR   3.0    4.0 0.4047002 0.2430202 0.18367986
#> 49          SP      6.4   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 50          MP      6.6   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 51          MP      6.8   2.5YR   5.0    6.0 0.6642703 0.4232155 0.31003272
#> 52          SP      6.9     5YR   4.0    5.0 0.5227043 0.3383386 0.21952529
#> 53          SP      6.8     5YR   5.0    6.0 0.6516906 0.4320947 0.27623886
#> 54          SP      5.5     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 55        <NA>      5.5     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 56          SP      6.1   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 57          SP      6.6   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 58          MP      6.8   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 59          MP      6.9   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 60          MP      6.8     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 61          PO      6.8   2.5YR   4.0    6.0 0.5550868 0.3205433 0.21624665
#> 62          PO      6.3     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 63          SP      6.0     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 64          SP      6.1   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 65          MP      6.2   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 66          MP      6.6   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 67          MP      7.0   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 68          SP      6.9     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 69          SP      7.2     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 70          SP      7.1     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 71          PO      6.9     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 72        <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 73        <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 74          SP      5.6    10YR   3.0    2.5        NA        NA         NA
#> 75          SP      6.2    10YR   3.0    2.5        NA        NA         NA
#> 76          PO      6.8    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 77          PO      7.1    10YR   4.0    4.0 0.4765699 0.3603870 0.21438027
#> 78          SP      7.2    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 79        <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 80          PO      6.2     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 81          SP      6.2     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 82          SP      6.3     5YR   3.0    5.0 0.4134797 0.2413405 0.13580495
#> 83          MP      6.8   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 84          MP      7.1   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 85          MP      6.8     5YR   5.0    6.0 0.6516906 0.4320947 0.27623886
#> 86          SP      6.9     5YR   4.0    6.0 0.5409717 0.3305930 0.18529327
#> 87          PO       NA   7.5YR   5.0    6.0 0.6373120 0.4408493 0.24846998
#> 88          PO      6.2   2.5YR   3.0    4.0 0.4047002 0.2430202 0.18367986
#> 89          SP      6.2   2.5YR   3.0    4.0 0.4047002 0.2430202 0.18367986
#> 90          SP      6.2   2.5YR   3.0    4.0 0.4047002 0.2430202 0.18367986
#> 91          MP      5.7    10YR   3.0    5.0 0.3814261 0.2600469 0.09577498
#> 92          MP      5.3    10YR   3.0    5.0 0.3814261 0.2600469 0.09577498
#> 93          MP      5.0    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 94          MP      4.9    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 95          MP      5.0    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 96          MP      5.3   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 97          PO      5.7   2.5YR   3.0    6.0 0.4468915 0.2206162 0.12794174
#> 98          PO      6.1     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 99          PO      6.2   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 100         SP      6.4   2.5YR   3.0    4.0 0.4047002 0.2430202 0.18367986
#> 101         SP      6.7   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 102         MP      6.7   2.5YR   3.0    5.0 0.4268174 0.2319252 0.15787833
#> 103         MP      6.9    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 104         MP      5.9    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 105         MP      5.4    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 106         MP      5.4    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 107         MP      5.4    10YR   3.0    6.0 0.3934784 0.2558781 0.03398692
#> 108         SP      5.5   2.5YR   3.0    4.0 0.4047002 0.2430202 0.18367986
#> 109         PO      6.1     5YR   3.0    4.0 0.3940324 0.2499977 0.16682669
#> 110         PO      6.0     5YR   4.0    4.0 0.5026314 0.3462401 0.24995558
#> 111         PO      6.9     5YR   4.0    4.0 0.5026314 0.3462401 0.24995558
#> 112         SP      5.9    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 113         SP      5.8    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 114         SP      6.1    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 115         PO      7.1    10YR   4.0    3.0 0.4578366 0.3653270 0.25754623
#> 116         SP      7.0    10YR   4.0    3.0 0.4578366 0.3653270 0.25754623
#> 117         PO      6.5    10YR   4.0    3.0 0.4578366 0.3653270 0.25754623
#> 118         SP      6.9    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 119         SP      6.6    10YR   3.0    3.0 0.3522797 0.2688590 0.17574632
#> 120         SP      6.7    10YR   3.5    3.0        NA        NA         NA
#> 121         PO      6.8    10YR   4.0    3.0 0.4578366 0.3653270 0.25754623
#> 122         PO      6.7    10YR   4.0    3.0 0.4578366 0.3653270 0.25754623
#> 123         SP      6.2   7.5YR   3.0    2.0 0.3433252 0.2694211 0.21692139
#> 124         SP      6.3   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 125         SP      6.7   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 126         SP      7.0   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 127         SP      7.1   7.5YR   3.0    4.0 0.3827022 0.2567391 0.15069361
#> 128         SP      7.2   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 129         SP      7.3   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 130         SP      6.2   7.5YR   3.0    2.0 0.3433252 0.2694211 0.21692139
#> 131         SP      6.4   7.5YR   3.0    2.0 0.3433252 0.2694211 0.21692139
#> 132         SP      6.6   7.5YR   3.0    2.0 0.3433252 0.2694211 0.21692139
#> 133         SP      7.0   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 134         PO      7.0   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 135         SP      7.2   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 136         PO      7.4   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 137         SP      5.6   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 138         SP      5.7   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 139         SP      6.6   7.5YR   3.0    2.5        NA        NA         NA
#> 140         SP      7.0   7.5YR   4.0    6.0 0.5252055 0.3404370 0.15784923
#> 141         SP      7.2   7.5YR   3.0    4.0 0.3827022 0.2567391 0.15069361
#> 142         SP      7.3   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 143         SP      7.2    10YR   4.0    3.0 0.4578366 0.3653270 0.25754623
#> 144       <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 145       <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 146       <NA>       NA    <NA>    NA     NA        NA        NA         NA
#> 147         SP      5.6   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 148         MP      5.8   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 149         SP      6.3   7.5YR   3.0    3.0 0.3644985 0.2628982 0.18559713
#> 150         SP      6.8   7.5YR   3.0    4.0 0.3827022 0.2567391 0.15069361
#> 151         SP      7.1   7.5YR   3.0    4.0 0.3827022 0.2567391 0.15069361
#> 152         SP      7.3   7.5YR   4.0    4.0 0.4923909 0.3523340 0.23133285
#> 153         SP      7.3   7.5YR   4.0    3.0 0.4706220 0.3590875 0.27082863
#> 154       <NA>       NA    <NA>    NA     NA        NA        NA         NA
#>     soil_color hzID newvalue
#> 1    #5A452DFF    1     TRUE
#> 2         <NA>    2       NA
#> 3    #5A452DFF    3       NA
#> 4    #785C45FF    4       NA
#> 5    #785C45FF    5       NA
#> 6    #7E5A3BFF    6       NA
#> 7    #7E5A3BFF    7       NA
#> 8         <NA>    8       NA
#> 9         <NA>    9       NA
#> 10   #584537FF   10     TRUE
#> 11   #5D432FFF   11       NA
#> 12   #785C45FF   12       NA
#> 13        <NA>   13       NA
#> 14   #785C45FF   14       NA
#> 15   #785C45FF   15       NA
#> 16   #715F3FFF   16       NA
#> 17   #584537FF   17     TRUE
#> 18   #5D432FFF   18       NA
#> 19   #785C45FF   19       NA
#> 20   #624126FF   20       NA
#> 21   #7E5A3BFF   21       NA
#> 22   #7E5A3BFF   22       NA
#> 23   #8F784CFF   23       NA
#> 24   #BE8B59FF   24     TRUE
#> 25   #805840FF   25       NA
#> 26   #805840FF   26       NA
#> 27   #855638FF   27       NA
#> 28   #8A542FFF   28       NA
#> 29   #8A542FFF   29       NA
#> 30   #855638FF   30       NA
#> 31   #865728FF   31       NA
#> 32        <NA>   32       NA
#> 33   #8A542FFF   33       NA
#> 34   #8A542FFF   34       NA
#> 35   #855638FF   35       NA
#> 36   #7E5A3BFF   36     TRUE
#> 37   #7E5A3BFF   37       NA
#> 38   #855638FF   38       NA
#> 39   #855638FF   39       NA
#> 40   #8A542FFF   40       NA
#> 41   #8A542FFF   41       NA
#> 42   #A3703FFF   42       NA
#> 43   #A3703FFF   43       NA
#> 44   #8A542FFF   44       NA
#> 45   #8A542FFF   45       NA
#> 46   #64402BFF   46     TRUE
#> 47   #64402BFF   47       NA
#> 48   #673E2FFF   48       NA
#> 49   #6D3B28FF   49       NA
#> 50   #6D3B28FF   50       NA
#> 51   #A96C4FFF   51       NA
#> 52   #855638FF   52       NA
#> 53   #A66E46FF   53       NA
#> 54   #64402BFF   54     TRUE
#> 55   #64402BFF   55       NA
#> 56   #6D3B28FF   56       NA
#> 57   #6D3B28FF   57       NA
#> 58   #723821FF   58       NA
#> 59   #723821FF   59       NA
#> 60   #8A542FFF   60       NA
#> 61   #8E5237FF   61       NA
#> 62   #64402BFF   62     TRUE
#> 63   #64402BFF   63       NA
#> 64   #6D3B28FF   64       NA
#> 65   #6D3B28FF   65       NA
#> 66   #723821FF   66       NA
#> 67   #723821FF   67       NA
#> 68   #8A542FFF   68       NA
#> 69   #8A542FFF   69       NA
#> 70   #8A542FFF   70       NA
#> 71   #8A542FFF   71       NA
#> 72        <NA>   72       NA
#> 73        <NA>   73       NA
#> 74        <NA>   74     TRUE
#> 75        <NA>   75       NA
#> 76   #5A452DFF   76       NA
#> 77   #7A5C37FF   77       NA
#> 78   #5A452DFF   78       NA
#> 79        <NA>   79       NA
#> 80   #64402BFF   80     TRUE
#> 81   #64402BFF   81       NA
#> 82   #693E23FF   82       NA
#> 83   #6D3B28FF   83       NA
#> 84   #723821FF   84       NA
#> 85   #A66E46FF   85       NA
#> 86   #8A542FFF   86       NA
#> 87   #A3703FFF   87       NA
#> 88   #673E2FFF   88     TRUE
#> 89   #673E2FFF   89       NA
#> 90   #673E2FFF   90       NA
#> 91   #614218FF   91       NA
#> 92   #614218FF   92       NA
#> 93   #644109FF   93       NA
#> 94   #644109FF   94       NA
#> 95   #644109FF   95       NA
#> 96   #723821FF   96       NA
#> 97   #723821FF   97       NA
#> 98   #64402BFF   98       NA
#> 99   #785C45FF   99       NA
#> 100  #673E2FFF  100     TRUE
#> 101  #6D3B28FF  101       NA
#> 102  #6D3B28FF  102       NA
#> 103  #644109FF  103       NA
#> 104  #644109FF  104       NA
#> 105  #644109FF  105       NA
#> 106  #644109FF  106       NA
#> 107  #644109FF  107       NA
#> 108  #673E2FFF  108       NA
#> 109  #64402BFF  109       NA
#> 110  #805840FF  110       NA
#> 111  #805840FF  111       NA
#> 112  #5A452DFF  112     TRUE
#> 113  #5A452DFF  113       NA
#> 114  #5A452DFF  114       NA
#> 115  #755D42FF  115       NA
#> 116  #755D42FF  116       NA
#> 117  #755D42FF  117     TRUE
#> 118  #5A452DFF  118       NA
#> 119  #5A452DFF  119       NA
#> 120       <NA>  120       NA
#> 121  #755D42FF  121       NA
#> 122  #755D42FF  122       NA
#> 123  #584537FF  123     TRUE
#> 124  #5D432FFF  124       NA
#> 125  #5D432FFF  125       NA
#> 126  #5D432FFF  126       NA
#> 127  #624126FF  127       NA
#> 128  #7E5A3BFF  128       NA
#> 129  #7E5A3BFF  129       NA
#> 130  #584537FF  130     TRUE
#> 131  #584537FF  131       NA
#> 132  #584537FF  132       NA
#> 133  #5D432FFF  133       NA
#> 134  #5D432FFF  134       NA
#> 135  #7E5A3BFF  135       NA
#> 136  #7E5A3BFF  136       NA
#> 137  #5D432FFF  137     TRUE
#> 138  #5D432FFF  138       NA
#> 139       <NA>  139       NA
#> 140  #865728FF  140       NA
#> 141  #624126FF  141       NA
#> 142  #7E5A3BFF  142       NA
#> 143  #755D42FF  143       NA
#> 144       <NA>  144       NA
#> 145       <NA>  145       NA
#> 146       <NA>  146       NA
#> 147  #5D432FFF  147     TRUE
#> 148  #5D432FFF  148       NA
#> 149  #5D432FFF  149       NA
#> 150  #624126FF  150       NA
#> 151  #624126FF  151       NA
#> 152  #7E5A3BFF  152       NA
#> 153  #785C45FF  153       NA
#> 154       <NA>  154       NA
```
