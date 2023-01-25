* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

set more off

global dir `"C:/Users/marie/Dropbox/NIH2020/SIMAH_workplace/"'

cd "${dir}/demography/1_raw CPS data/"

set more off

clear
quietly infix                     ///
  int     year         1-4        ///
  long    serial       5-9        ///
  byte    month        10-11      ///
  double  hwtfinl      12-21      ///
  double  cpsid        22-35      ///
  byte    asecflag     36-36      ///
  double  asecwth      37-47      ///
  byte    repwt        48-48      ///
  double  repwt1       49-57      ///
  double  repwt2       58-66      ///
  double  repwt3       67-75      ///
  double  repwt4       76-84      ///
  double  repwt5       85-93      ///
  double  repwt6       94-102     ///
  double  repwt7       103-111    ///
  double  repwt8       112-120    ///
  double  repwt9       121-129    ///
  double  repwt10      130-138    ///
  double  repwt11      139-147    ///
  double  repwt12      148-156    ///
  double  repwt13      157-165    ///
  double  repwt14      166-174    ///
  double  repwt15      175-183    ///
  double  repwt16      184-192    ///
  double  repwt17      193-201    ///
  double  repwt18      202-210    ///
  double  repwt19      211-219    ///
  double  repwt20      220-228    ///
  double  repwt21      229-237    ///
  double  repwt22      238-246    ///
  double  repwt23      247-255    ///
  double  repwt24      256-264    ///
  double  repwt25      265-273    ///
  double  repwt26      274-282    ///
  double  repwt27      283-291    ///
  double  repwt28      292-300    ///
  double  repwt29      301-309    ///
  double  repwt30      310-318    ///
  double  repwt31      319-327    ///
  double  repwt32      328-336    ///
  double  repwt33      337-345    ///
  double  repwt34      346-354    ///
  double  repwt35      355-363    ///
  double  repwt36      364-372    ///
  double  repwt37      373-381    ///
  double  repwt38      382-390    ///
  double  repwt39      391-399    ///
  double  repwt40      400-408    ///
  double  repwt41      409-417    ///
  double  repwt42      418-426    ///
  double  repwt43      427-435    ///
  double  repwt44      436-444    ///
  double  repwt45      445-453    ///
  double  repwt46      454-462    ///
  double  repwt47      463-471    ///
  double  repwt48      472-480    ///
  double  repwt49      481-489    ///
  double  repwt50      490-498    ///
  double  repwt51      499-507    ///
  double  repwt52      508-516    ///
  double  repwt53      517-525    ///
  double  repwt54      526-534    ///
  double  repwt55      535-543    ///
  double  repwt56      544-552    ///
  double  repwt57      553-561    ///
  double  repwt58      562-570    ///
  double  repwt59      571-579    ///
  double  repwt60      580-588    ///
  double  repwt61      589-597    ///
  double  repwt62      598-606    ///
  double  repwt63      607-615    ///
  double  repwt64      616-624    ///
  double  repwt65      625-633    ///
  double  repwt66      634-642    ///
  double  repwt67      643-651    ///
  double  repwt68      652-660    ///
  double  repwt69      661-669    ///
  double  repwt70      670-678    ///
  double  repwt71      679-687    ///
  double  repwt72      688-696    ///
  double  repwt73      697-705    ///
  double  repwt74      706-714    ///
  double  repwt75      715-723    ///
  double  repwt76      724-732    ///
  double  repwt77      733-741    ///
  double  repwt78      742-750    ///
  double  repwt79      751-759    ///
  double  repwt80      760-768    ///
  double  repwt81      769-777    ///
  double  repwt82      778-786    ///
  double  repwt83      787-795    ///
  double  repwt84      796-804    ///
  double  repwt85      805-813    ///
  double  repwt86      814-822    ///
  double  repwt87      823-831    ///
  double  repwt88      832-840    ///
  double  repwt89      841-849    ///
  double  repwt90      850-858    ///
  double  repwt91      859-867    ///
  double  repwt92      868-876    ///
  double  repwt93      877-885    ///
  double  repwt94      886-894    ///
  double  repwt95      895-903    ///
  double  repwt96      904-912    ///
  double  repwt97      913-921    ///
  double  repwt98      922-930    ///
  double  repwt99      931-939    ///
  double  repwt100     940-948    ///
  double  repwt101     949-957    ///
  double  repwt102     958-966    ///
  double  repwt103     967-975    ///
  double  repwt104     976-984    ///
  double  repwt105     985-993    ///
  double  repwt106     994-1002   ///
  double  repwt107     1003-1011  ///
  double  repwt108     1012-1020  ///
  double  repwt109     1021-1029  ///
  double  repwt110     1030-1038  ///
  double  repwt111     1039-1047  ///
  double  repwt112     1048-1056  ///
  double  repwt113     1057-1065  ///
  double  repwt114     1066-1074  ///
  double  repwt115     1075-1083  ///
  double  repwt116     1084-1092  ///
  double  repwt117     1093-1101  ///
  double  repwt118     1102-1110  ///
  double  repwt119     1111-1119  ///
  double  repwt120     1120-1128  ///
  double  repwt121     1129-1137  ///
  double  repwt122     1138-1146  ///
  double  repwt123     1147-1155  ///
  double  repwt124     1156-1164  ///
  double  repwt125     1165-1173  ///
  double  repwt126     1174-1182  ///
  double  repwt127     1183-1191  ///
  double  repwt128     1192-1200  ///
  double  repwt129     1201-1209  ///
  double  repwt130     1210-1218  ///
  double  repwt131     1219-1227  ///
  double  repwt132     1228-1236  ///
  double  repwt133     1237-1245  ///
  double  repwt134     1246-1254  ///
  double  repwt135     1255-1263  ///
  double  repwt136     1264-1272  ///
  double  repwt137     1273-1281  ///
  double  repwt138     1282-1290  ///
  double  repwt139     1291-1299  ///
  double  repwt140     1300-1308  ///
  double  repwt141     1309-1317  ///
  double  repwt142     1318-1326  ///
  double  repwt143     1327-1335  ///
  double  repwt144     1336-1344  ///
  double  repwt145     1345-1353  ///
  double  repwt146     1354-1362  ///
  double  repwt147     1363-1371  ///
  double  repwt148     1372-1380  ///
  double  repwt149     1381-1389  ///
  double  repwt150     1390-1398  ///
  double  repwt151     1399-1407  ///
  double  repwt152     1408-1416  ///
  double  repwt153     1417-1425  ///
  double  repwt154     1426-1434  ///
  double  repwt155     1435-1443  ///
  double  repwt156     1444-1452  ///
  double  repwt157     1453-1461  ///
  double  repwt158     1462-1470  ///
  double  repwt159     1471-1479  ///
  double  repwt160     1480-1488  ///
  byte    statefip     1489-1490  ///
  double  hhincome     1491-1498  ///
  byte    fspoor       1499-1500  ///
  byte    pernum       1501-1502  ///
  double  wtfinl       1503-1516  ///
  double  cpsidp       1517-1530  ///
  double  asecwt       1531-1541  ///
  int     relate       1542-1545  ///
  byte    age          1546-1547  ///
  byte    sex          1548-1548  ///
  int     race         1549-1551  ///
  byte    marst        1552-1552  ///
  byte    asian        1553-1554  ///
  byte    famsize      1555-1556  ///
  byte    ftype        1557-1557  ///
  int     hispan       1558-1560  ///
  byte    empstat      1561-1562  ///
  int     occ          1563-1566  ///
  byte    classwkr     1567-1568  ///
  int     educ         1569-1571  ///
  byte    educ99       1572-1573  ///
  byte    repwtp       1574-1574  ///
  double  repwtp1      1575-1584  ///
  double  repwtp2      1585-1594  ///
  double  repwtp3      1595-1604  ///
  double  repwtp4      1605-1614  ///
  double  repwtp5      1615-1624  ///
  double  repwtp6      1625-1634  ///
  double  repwtp7      1635-1644  ///
  double  repwtp8      1645-1654  ///
  double  repwtp9      1655-1664  ///
  double  repwtp10     1665-1674  ///
  double  repwtp11     1675-1684  ///
  double  repwtp12     1685-1694  ///
  double  repwtp13     1695-1704  ///
  double  repwtp14     1705-1714  ///
  double  repwtp15     1715-1724  ///
  double  repwtp16     1725-1734  ///
  double  repwtp17     1735-1744  ///
  double  repwtp18     1745-1754  ///
  double  repwtp19     1755-1764  ///
  double  repwtp20     1765-1774  ///
  double  repwtp21     1775-1784  ///
  double  repwtp22     1785-1794  ///
  double  repwtp23     1795-1804  ///
  double  repwtp24     1805-1814  ///
  double  repwtp25     1815-1824  ///
  double  repwtp26     1825-1834  ///
  double  repwtp27     1835-1844  ///
  double  repwtp28     1845-1854  ///
  double  repwtp29     1855-1864  ///
  double  repwtp30     1865-1874  ///
  double  repwtp31     1875-1884  ///
  double  repwtp32     1885-1894  ///
  double  repwtp33     1895-1904  ///
  double  repwtp34     1905-1914  ///
  double  repwtp35     1915-1924  ///
  double  repwtp36     1925-1934  ///
  double  repwtp37     1935-1944  ///
  double  repwtp38     1945-1954  ///
  double  repwtp39     1955-1964  ///
  double  repwtp40     1965-1974  ///
  double  repwtp41     1975-1984  ///
  double  repwtp42     1985-1994  ///
  double  repwtp43     1995-2004  ///
  double  repwtp44     2005-2014  ///
  double  repwtp45     2015-2024  ///
  double  repwtp46     2025-2034  ///
  double  repwtp47     2035-2044  ///
  double  repwtp48     2045-2054  ///
  double  repwtp49     2055-2064  ///
  double  repwtp50     2065-2074  ///
  double  repwtp51     2075-2084  ///
  double  repwtp52     2085-2094  ///
  double  repwtp53     2095-2104  ///
  double  repwtp54     2105-2114  ///
  double  repwtp55     2115-2124  ///
  double  repwtp56     2125-2134  ///
  double  repwtp57     2135-2144  ///
  double  repwtp58     2145-2154  ///
  double  repwtp59     2155-2164  ///
  double  repwtp60     2165-2174  ///
  double  repwtp61     2175-2184  ///
  double  repwtp62     2185-2194  ///
  double  repwtp63     2195-2204  ///
  double  repwtp64     2205-2214  ///
  double  repwtp65     2215-2224  ///
  double  repwtp66     2225-2234  ///
  double  repwtp67     2235-2244  ///
  double  repwtp68     2245-2254  ///
  double  repwtp69     2255-2264  ///
  double  repwtp70     2265-2274  ///
  double  repwtp71     2275-2284  ///
  double  repwtp72     2285-2294  ///
  double  repwtp73     2295-2304  ///
  double  repwtp74     2305-2314  ///
  double  repwtp75     2315-2324  ///
  double  repwtp76     2325-2334  ///
  double  repwtp77     2335-2344  ///
  double  repwtp78     2345-2354  ///
  double  repwtp79     2355-2364  ///
  double  repwtp80     2365-2374  ///
  double  repwtp81     2375-2384  ///
  double  repwtp82     2385-2394  ///
  double  repwtp83     2395-2404  ///
  double  repwtp84     2405-2414  ///
  double  repwtp85     2415-2424  ///
  double  repwtp86     2425-2434  ///
  double  repwtp87     2435-2444  ///
  double  repwtp88     2445-2454  ///
  double  repwtp89     2455-2464  ///
  double  repwtp90     2465-2474  ///
  double  repwtp91     2475-2484  ///
  double  repwtp92     2485-2494  ///
  double  repwtp93     2495-2504  ///
  double  repwtp94     2505-2514  ///
  double  repwtp95     2515-2524  ///
  double  repwtp96     2525-2534  ///
  double  repwtp97     2535-2544  ///
  double  repwtp98     2545-2554  ///
  double  repwtp99     2555-2564  ///
  double  repwtp100    2565-2574  ///
  double  repwtp101    2575-2584  ///
  double  repwtp102    2585-2594  ///
  double  repwtp103    2595-2604  ///
  double  repwtp104    2605-2614  ///
  double  repwtp105    2615-2624  ///
  double  repwtp106    2625-2634  ///
  double  repwtp107    2635-2644  ///
  double  repwtp108    2645-2654  ///
  double  repwtp109    2655-2664  ///
  double  repwtp110    2665-2674  ///
  double  repwtp111    2675-2684  ///
  double  repwtp112    2685-2694  ///
  double  repwtp113    2695-2704  ///
  double  repwtp114    2705-2714  ///
  double  repwtp115    2715-2724  ///
  double  repwtp116    2725-2734  ///
  double  repwtp117    2735-2744  ///
  double  repwtp118    2745-2754  ///
  double  repwtp119    2755-2764  ///
  double  repwtp120    2765-2774  ///
  double  repwtp121    2775-2784  ///
  double  repwtp122    2785-2794  ///
  double  repwtp123    2795-2804  ///
  double  repwtp124    2805-2814  ///
  double  repwtp125    2815-2824  ///
  double  repwtp126    2825-2834  ///
  double  repwtp127    2835-2844  ///
  double  repwtp128    2845-2854  ///
  double  repwtp129    2855-2864  ///
  double  repwtp130    2865-2874  ///
  double  repwtp131    2875-2884  ///
  double  repwtp132    2885-2894  ///
  double  repwtp133    2895-2904  ///
  double  repwtp134    2905-2914  ///
  double  repwtp135    2915-2924  ///
  double  repwtp136    2925-2934  ///
  double  repwtp137    2935-2944  ///
  double  repwtp138    2945-2954  ///
  double  repwtp139    2955-2964  ///
  double  repwtp140    2965-2974  ///
  double  repwtp141    2975-2984  ///
  double  repwtp142    2985-2994  ///
  double  repwtp143    2995-3004  ///
  double  repwtp144    3005-3014  ///
  double  repwtp145    3015-3024  ///
  double  repwtp146    3025-3034  ///
  double  repwtp147    3035-3044  ///
  double  repwtp148    3045-3054  ///
  double  repwtp149    3055-3064  ///
  double  repwtp150    3065-3074  ///
  double  repwtp151    3075-3084  ///
  double  repwtp152    3085-3094  ///
  double  repwtp153    3095-3104  ///
  double  repwtp154    3105-3114  ///
  double  repwtp155    3115-3124  ///
  double  repwtp156    3125-3134  ///
  double  repwtp157    3135-3144  ///
  double  repwtp158    3145-3154  ///
  double  repwtp159    3155-3164  ///
  double  repwtp160    3165-3174  ///
  byte    classwly     3175-3176  ///
  double  ftotval      3177-3186  ///
  double  inctot       3187-3195  ///
  byte    offpov       3196-3197  ///
  double  offtotval    3198-3207  ///
  long    offcutoff    3208-3213  ///
  byte    poverty      3214-3215  ///
  int     hiufpginc    3216-3219  ///
  int     edgrade      3220-3223  ///
  int     educ_head    3224-3226  ///
  int     educ_mom     3227-3229  ///
  int     educ_mom2    3230-3232  ///
  int     educ_pop     3233-3235  ///
  int     educ_pop2    3236-3238  ///
  int     educ_sp      3239-3241  ///
  byte    educ99_head  3242-3243  ///
  byte    educ99_mom   3244-3245  ///
  byte    educ99_mom2  3246-3247  ///
  byte    educ99_pop   3248-3249  ///
  byte    educ99_pop2  3250-3251  ///
  byte    educ99_sp    3252-3253  ///
  using `"cps_00003.dat"'

replace hwtfinl     = hwtfinl     / 10000
replace asecwth     = asecwth     / 10000
replace repwt1      = repwt1      / 10000
replace repwt2      = repwt2      / 10000
replace repwt3      = repwt3      / 10000
replace repwt4      = repwt4      / 10000
replace repwt5      = repwt5      / 10000
replace repwt6      = repwt6      / 10000
replace repwt7      = repwt7      / 10000
replace repwt8      = repwt8      / 10000
replace repwt9      = repwt9      / 10000
replace repwt10     = repwt10     / 10000
replace repwt11     = repwt11     / 10000
replace repwt12     = repwt12     / 10000
replace repwt13     = repwt13     / 10000
replace repwt14     = repwt14     / 10000
replace repwt15     = repwt15     / 10000
replace repwt16     = repwt16     / 10000
replace repwt17     = repwt17     / 10000
replace repwt18     = repwt18     / 10000
replace repwt19     = repwt19     / 10000
replace repwt20     = repwt20     / 10000
replace repwt21     = repwt21     / 10000
replace repwt22     = repwt22     / 10000
replace repwt23     = repwt23     / 10000
replace repwt24     = repwt24     / 10000
replace repwt25     = repwt25     / 10000
replace repwt26     = repwt26     / 10000
replace repwt27     = repwt27     / 10000
replace repwt28     = repwt28     / 10000
replace repwt29     = repwt29     / 10000
replace repwt30     = repwt30     / 10000
replace repwt31     = repwt31     / 10000
replace repwt32     = repwt32     / 10000
replace repwt33     = repwt33     / 10000
replace repwt34     = repwt34     / 10000
replace repwt35     = repwt35     / 10000
replace repwt36     = repwt36     / 10000
replace repwt37     = repwt37     / 10000
replace repwt38     = repwt38     / 10000
replace repwt39     = repwt39     / 10000
replace repwt40     = repwt40     / 10000
replace repwt41     = repwt41     / 10000
replace repwt42     = repwt42     / 10000
replace repwt43     = repwt43     / 10000
replace repwt44     = repwt44     / 10000
replace repwt45     = repwt45     / 10000
replace repwt46     = repwt46     / 10000
replace repwt47     = repwt47     / 10000
replace repwt48     = repwt48     / 10000
replace repwt49     = repwt49     / 10000
replace repwt50     = repwt50     / 10000
replace repwt51     = repwt51     / 10000
replace repwt52     = repwt52     / 10000
replace repwt53     = repwt53     / 10000
replace repwt54     = repwt54     / 10000
replace repwt55     = repwt55     / 10000
replace repwt56     = repwt56     / 10000
replace repwt57     = repwt57     / 10000
replace repwt58     = repwt58     / 10000
replace repwt59     = repwt59     / 10000
replace repwt60     = repwt60     / 10000
replace repwt61     = repwt61     / 10000
replace repwt62     = repwt62     / 10000
replace repwt63     = repwt63     / 10000
replace repwt64     = repwt64     / 10000
replace repwt65     = repwt65     / 10000
replace repwt66     = repwt66     / 10000
replace repwt67     = repwt67     / 10000
replace repwt68     = repwt68     / 10000
replace repwt69     = repwt69     / 10000
replace repwt70     = repwt70     / 10000
replace repwt71     = repwt71     / 10000
replace repwt72     = repwt72     / 10000
replace repwt73     = repwt73     / 10000
replace repwt74     = repwt74     / 10000
replace repwt75     = repwt75     / 10000
replace repwt76     = repwt76     / 10000
replace repwt77     = repwt77     / 10000
replace repwt78     = repwt78     / 10000
replace repwt79     = repwt79     / 10000
replace repwt80     = repwt80     / 10000
replace repwt81     = repwt81     / 10000
replace repwt82     = repwt82     / 10000
replace repwt83     = repwt83     / 10000
replace repwt84     = repwt84     / 10000
replace repwt85     = repwt85     / 10000
replace repwt86     = repwt86     / 10000
replace repwt87     = repwt87     / 10000
replace repwt88     = repwt88     / 10000
replace repwt89     = repwt89     / 10000
replace repwt90     = repwt90     / 10000
replace repwt91     = repwt91     / 10000
replace repwt92     = repwt92     / 10000
replace repwt93     = repwt93     / 10000
replace repwt94     = repwt94     / 10000
replace repwt95     = repwt95     / 10000
replace repwt96     = repwt96     / 10000
replace repwt97     = repwt97     / 10000
replace repwt98     = repwt98     / 10000
replace repwt99     = repwt99     / 10000
replace repwt100    = repwt100    / 10000
replace repwt101    = repwt101    / 10000
replace repwt102    = repwt102    / 10000
replace repwt103    = repwt103    / 10000
replace repwt104    = repwt104    / 10000
replace repwt105    = repwt105    / 10000
replace repwt106    = repwt106    / 10000
replace repwt107    = repwt107    / 10000
replace repwt108    = repwt108    / 10000
replace repwt109    = repwt109    / 10000
replace repwt110    = repwt110    / 10000
replace repwt111    = repwt111    / 10000
replace repwt112    = repwt112    / 10000
replace repwt113    = repwt113    / 10000
replace repwt114    = repwt114    / 10000
replace repwt115    = repwt115    / 10000
replace repwt116    = repwt116    / 10000
replace repwt117    = repwt117    / 10000
replace repwt118    = repwt118    / 10000
replace repwt119    = repwt119    / 10000
replace repwt120    = repwt120    / 10000
replace repwt121    = repwt121    / 10000
replace repwt122    = repwt122    / 10000
replace repwt123    = repwt123    / 10000
replace repwt124    = repwt124    / 10000
replace repwt125    = repwt125    / 10000
replace repwt126    = repwt126    / 10000
replace repwt127    = repwt127    / 10000
replace repwt128    = repwt128    / 10000
replace repwt129    = repwt129    / 10000
replace repwt130    = repwt130    / 10000
replace repwt131    = repwt131    / 10000
replace repwt132    = repwt132    / 10000
replace repwt133    = repwt133    / 10000
replace repwt134    = repwt134    / 10000
replace repwt135    = repwt135    / 10000
replace repwt136    = repwt136    / 10000
replace repwt137    = repwt137    / 10000
replace repwt138    = repwt138    / 10000
replace repwt139    = repwt139    / 10000
replace repwt140    = repwt140    / 10000
replace repwt141    = repwt141    / 10000
replace repwt142    = repwt142    / 10000
replace repwt143    = repwt143    / 10000
replace repwt144    = repwt144    / 10000
replace repwt145    = repwt145    / 10000
replace repwt146    = repwt146    / 10000
replace repwt147    = repwt147    / 10000
replace repwt148    = repwt148    / 10000
replace repwt149    = repwt149    / 10000
replace repwt150    = repwt150    / 10000
replace repwt151    = repwt151    / 10000
replace repwt152    = repwt152    / 10000
replace repwt153    = repwt153    / 10000
replace repwt154    = repwt154    / 10000
replace repwt155    = repwt155    / 10000
replace repwt156    = repwt156    / 10000
replace repwt157    = repwt157    / 10000
replace repwt158    = repwt158    / 10000
replace repwt159    = repwt159    / 10000
replace repwt160    = repwt160    / 10000
replace wtfinl      = wtfinl      / 10000
replace asecwt      = asecwt      / 10000
replace repwtp1     = repwtp1     / 10000
replace repwtp2     = repwtp2     / 10000
replace repwtp3     = repwtp3     / 10000
replace repwtp4     = repwtp4     / 10000
replace repwtp5     = repwtp5     / 10000
replace repwtp6     = repwtp6     / 10000
replace repwtp7     = repwtp7     / 10000
replace repwtp8     = repwtp8     / 10000
replace repwtp9     = repwtp9     / 10000
replace repwtp10    = repwtp10    / 10000
replace repwtp11    = repwtp11    / 10000
replace repwtp12    = repwtp12    / 10000
replace repwtp13    = repwtp13    / 10000
replace repwtp14    = repwtp14    / 10000
replace repwtp15    = repwtp15    / 10000
replace repwtp16    = repwtp16    / 10000
replace repwtp17    = repwtp17    / 10000
replace repwtp18    = repwtp18    / 10000
replace repwtp19    = repwtp19    / 10000
replace repwtp20    = repwtp20    / 10000
replace repwtp21    = repwtp21    / 10000
replace repwtp22    = repwtp22    / 10000
replace repwtp23    = repwtp23    / 10000
replace repwtp24    = repwtp24    / 10000
replace repwtp25    = repwtp25    / 10000
replace repwtp26    = repwtp26    / 10000
replace repwtp27    = repwtp27    / 10000
replace repwtp28    = repwtp28    / 10000
replace repwtp29    = repwtp29    / 10000
replace repwtp30    = repwtp30    / 10000
replace repwtp31    = repwtp31    / 10000
replace repwtp32    = repwtp32    / 10000
replace repwtp33    = repwtp33    / 10000
replace repwtp34    = repwtp34    / 10000
replace repwtp35    = repwtp35    / 10000
replace repwtp36    = repwtp36    / 10000
replace repwtp37    = repwtp37    / 10000
replace repwtp38    = repwtp38    / 10000
replace repwtp39    = repwtp39    / 10000
replace repwtp40    = repwtp40    / 10000
replace repwtp41    = repwtp41    / 10000
replace repwtp42    = repwtp42    / 10000
replace repwtp43    = repwtp43    / 10000
replace repwtp44    = repwtp44    / 10000
replace repwtp45    = repwtp45    / 10000
replace repwtp46    = repwtp46    / 10000
replace repwtp47    = repwtp47    / 10000
replace repwtp48    = repwtp48    / 10000
replace repwtp49    = repwtp49    / 10000
replace repwtp50    = repwtp50    / 10000
replace repwtp51    = repwtp51    / 10000
replace repwtp52    = repwtp52    / 10000
replace repwtp53    = repwtp53    / 10000
replace repwtp54    = repwtp54    / 10000
replace repwtp55    = repwtp55    / 10000
replace repwtp56    = repwtp56    / 10000
replace repwtp57    = repwtp57    / 10000
replace repwtp58    = repwtp58    / 10000
replace repwtp59    = repwtp59    / 10000
replace repwtp60    = repwtp60    / 10000
replace repwtp61    = repwtp61    / 10000
replace repwtp62    = repwtp62    / 10000
replace repwtp63    = repwtp63    / 10000
replace repwtp64    = repwtp64    / 10000
replace repwtp65    = repwtp65    / 10000
replace repwtp66    = repwtp66    / 10000
replace repwtp67    = repwtp67    / 10000
replace repwtp68    = repwtp68    / 10000
replace repwtp69    = repwtp69    / 10000
replace repwtp70    = repwtp70    / 10000
replace repwtp71    = repwtp71    / 10000
replace repwtp72    = repwtp72    / 10000
replace repwtp73    = repwtp73    / 10000
replace repwtp74    = repwtp74    / 10000
replace repwtp75    = repwtp75    / 10000
replace repwtp76    = repwtp76    / 10000
replace repwtp77    = repwtp77    / 10000
replace repwtp78    = repwtp78    / 10000
replace repwtp79    = repwtp79    / 10000
replace repwtp80    = repwtp80    / 10000
replace repwtp81    = repwtp81    / 10000
replace repwtp82    = repwtp82    / 10000
replace repwtp83    = repwtp83    / 10000
replace repwtp84    = repwtp84    / 10000
replace repwtp85    = repwtp85    / 10000
replace repwtp86    = repwtp86    / 10000
replace repwtp87    = repwtp87    / 10000
replace repwtp88    = repwtp88    / 10000
replace repwtp89    = repwtp89    / 10000
replace repwtp90    = repwtp90    / 10000
replace repwtp91    = repwtp91    / 10000
replace repwtp92    = repwtp92    / 10000
replace repwtp93    = repwtp93    / 10000
replace repwtp94    = repwtp94    / 10000
replace repwtp95    = repwtp95    / 10000
replace repwtp96    = repwtp96    / 10000
replace repwtp97    = repwtp97    / 10000
replace repwtp98    = repwtp98    / 10000
replace repwtp99    = repwtp99    / 10000
replace repwtp100   = repwtp100   / 10000
replace repwtp101   = repwtp101   / 10000
replace repwtp102   = repwtp102   / 10000
replace repwtp103   = repwtp103   / 10000
replace repwtp104   = repwtp104   / 10000
replace repwtp105   = repwtp105   / 10000
replace repwtp106   = repwtp106   / 10000
replace repwtp107   = repwtp107   / 10000
replace repwtp108   = repwtp108   / 10000
replace repwtp109   = repwtp109   / 10000
replace repwtp110   = repwtp110   / 10000
replace repwtp111   = repwtp111   / 10000
replace repwtp112   = repwtp112   / 10000
replace repwtp113   = repwtp113   / 10000
replace repwtp114   = repwtp114   / 10000
replace repwtp115   = repwtp115   / 10000
replace repwtp116   = repwtp116   / 10000
replace repwtp117   = repwtp117   / 10000
replace repwtp118   = repwtp118   / 10000
replace repwtp119   = repwtp119   / 10000
replace repwtp120   = repwtp120   / 10000
replace repwtp121   = repwtp121   / 10000
replace repwtp122   = repwtp122   / 10000
replace repwtp123   = repwtp123   / 10000
replace repwtp124   = repwtp124   / 10000
replace repwtp125   = repwtp125   / 10000
replace repwtp126   = repwtp126   / 10000
replace repwtp127   = repwtp127   / 10000
replace repwtp128   = repwtp128   / 10000
replace repwtp129   = repwtp129   / 10000
replace repwtp130   = repwtp130   / 10000
replace repwtp131   = repwtp131   / 10000
replace repwtp132   = repwtp132   / 10000
replace repwtp133   = repwtp133   / 10000
replace repwtp134   = repwtp134   / 10000
replace repwtp135   = repwtp135   / 10000
replace repwtp136   = repwtp136   / 10000
replace repwtp137   = repwtp137   / 10000
replace repwtp138   = repwtp138   / 10000
replace repwtp139   = repwtp139   / 10000
replace repwtp140   = repwtp140   / 10000
replace repwtp141   = repwtp141   / 10000
replace repwtp142   = repwtp142   / 10000
replace repwtp143   = repwtp143   / 10000
replace repwtp144   = repwtp144   / 10000
replace repwtp145   = repwtp145   / 10000
replace repwtp146   = repwtp146   / 10000
replace repwtp147   = repwtp147   / 10000
replace repwtp148   = repwtp148   / 10000
replace repwtp149   = repwtp149   / 10000
replace repwtp150   = repwtp150   / 10000
replace repwtp151   = repwtp151   / 10000
replace repwtp152   = repwtp152   / 10000
replace repwtp153   = repwtp153   / 10000
replace repwtp154   = repwtp154   / 10000
replace repwtp155   = repwtp155   / 10000
replace repwtp156   = repwtp156   / 10000
replace repwtp157   = repwtp157   / 10000
replace repwtp158   = repwtp158   / 10000
replace repwtp159   = repwtp159   / 10000
replace repwtp160   = repwtp160   / 10000

format hwtfinl     %10.4f
format cpsid       %14.0f
format asecwth     %11.4f
format repwt1      %9.4f
format repwt2      %9.4f
format repwt3      %9.4f
format repwt4      %9.4f
format repwt5      %9.4f
format repwt6      %9.4f
format repwt7      %9.4f
format repwt8      %9.4f
format repwt9      %9.4f
format repwt10     %9.4f
format repwt11     %9.4f
format repwt12     %9.4f
format repwt13     %9.4f
format repwt14     %9.4f
format repwt15     %9.4f
format repwt16     %9.4f
format repwt17     %9.4f
format repwt18     %9.4f
format repwt19     %9.4f
format repwt20     %9.4f
format repwt21     %9.4f
format repwt22     %9.4f
format repwt23     %9.4f
format repwt24     %9.4f
format repwt25     %9.4f
format repwt26     %9.4f
format repwt27     %9.4f
format repwt28     %9.4f
format repwt29     %9.4f
format repwt30     %9.4f
format repwt31     %9.4f
format repwt32     %9.4f
format repwt33     %9.4f
format repwt34     %9.4f
format repwt35     %9.4f
format repwt36     %9.4f
format repwt37     %9.4f
format repwt38     %9.4f
format repwt39     %9.4f
format repwt40     %9.4f
format repwt41     %9.4f
format repwt42     %9.4f
format repwt43     %9.4f
format repwt44     %9.4f
format repwt45     %9.4f
format repwt46     %9.4f
format repwt47     %9.4f
format repwt48     %9.4f
format repwt49     %9.4f
format repwt50     %9.4f
format repwt51     %9.4f
format repwt52     %9.4f
format repwt53     %9.4f
format repwt54     %9.4f
format repwt55     %9.4f
format repwt56     %9.4f
format repwt57     %9.4f
format repwt58     %9.4f
format repwt59     %9.4f
format repwt60     %9.4f
format repwt61     %9.4f
format repwt62     %9.4f
format repwt63     %9.4f
format repwt64     %9.4f
format repwt65     %9.4f
format repwt66     %9.4f
format repwt67     %9.4f
format repwt68     %9.4f
format repwt69     %9.4f
format repwt70     %9.4f
format repwt71     %9.4f
format repwt72     %9.4f
format repwt73     %9.4f
format repwt74     %9.4f
format repwt75     %9.4f
format repwt76     %9.4f
format repwt77     %9.4f
format repwt78     %9.4f
format repwt79     %9.4f
format repwt80     %9.4f
format repwt81     %9.4f
format repwt82     %9.4f
format repwt83     %9.4f
format repwt84     %9.4f
format repwt85     %9.4f
format repwt86     %9.4f
format repwt87     %9.4f
format repwt88     %9.4f
format repwt89     %9.4f
format repwt90     %9.4f
format repwt91     %9.4f
format repwt92     %9.4f
format repwt93     %9.4f
format repwt94     %9.4f
format repwt95     %9.4f
format repwt96     %9.4f
format repwt97     %9.4f
format repwt98     %9.4f
format repwt99     %9.4f
format repwt100    %9.4f
format repwt101    %9.4f
format repwt102    %9.4f
format repwt103    %9.4f
format repwt104    %9.4f
format repwt105    %9.4f
format repwt106    %9.4f
format repwt107    %9.4f
format repwt108    %9.4f
format repwt109    %9.4f
format repwt110    %9.4f
format repwt111    %9.4f
format repwt112    %9.4f
format repwt113    %9.4f
format repwt114    %9.4f
format repwt115    %9.4f
format repwt116    %9.4f
format repwt117    %9.4f
format repwt118    %9.4f
format repwt119    %9.4f
format repwt120    %9.4f
format repwt121    %9.4f
format repwt122    %9.4f
format repwt123    %9.4f
format repwt124    %9.4f
format repwt125    %9.4f
format repwt126    %9.4f
format repwt127    %9.4f
format repwt128    %9.4f
format repwt129    %9.4f
format repwt130    %9.4f
format repwt131    %9.4f
format repwt132    %9.4f
format repwt133    %9.4f
format repwt134    %9.4f
format repwt135    %9.4f
format repwt136    %9.4f
format repwt137    %9.4f
format repwt138    %9.4f
format repwt139    %9.4f
format repwt140    %9.4f
format repwt141    %9.4f
format repwt142    %9.4f
format repwt143    %9.4f
format repwt144    %9.4f
format repwt145    %9.4f
format repwt146    %9.4f
format repwt147    %9.4f
format repwt148    %9.4f
format repwt149    %9.4f
format repwt150    %9.4f
format repwt151    %9.4f
format repwt152    %9.4f
format repwt153    %9.4f
format repwt154    %9.4f
format repwt155    %9.4f
format repwt156    %9.4f
format repwt157    %9.4f
format repwt158    %9.4f
format repwt159    %9.4f
format repwt160    %9.4f
format hhincome    %8.0f
format wtfinl      %14.4f
format cpsidp      %14.0f
format asecwt      %11.4f
format repwtp1     %10.4f
format repwtp2     %10.4f
format repwtp3     %10.4f
format repwtp4     %10.4f
format repwtp5     %10.4f
format repwtp6     %10.4f
format repwtp7     %10.4f
format repwtp8     %10.4f
format repwtp9     %10.4f
format repwtp10    %10.4f
format repwtp11    %10.4f
format repwtp12    %10.4f
format repwtp13    %10.4f
format repwtp14    %10.4f
format repwtp15    %10.4f
format repwtp16    %10.4f
format repwtp17    %10.4f
format repwtp18    %10.4f
format repwtp19    %10.4f
format repwtp20    %10.4f
format repwtp21    %10.4f
format repwtp22    %10.4f
format repwtp23    %10.4f
format repwtp24    %10.4f
format repwtp25    %10.4f
format repwtp26    %10.4f
format repwtp27    %10.4f
format repwtp28    %10.4f
format repwtp29    %10.4f
format repwtp30    %10.4f
format repwtp31    %10.4f
format repwtp32    %10.4f
format repwtp33    %10.4f
format repwtp34    %10.4f
format repwtp35    %10.4f
format repwtp36    %10.4f
format repwtp37    %10.4f
format repwtp38    %10.4f
format repwtp39    %10.4f
format repwtp40    %10.4f
format repwtp41    %10.4f
format repwtp42    %10.4f
format repwtp43    %10.4f
format repwtp44    %10.4f
format repwtp45    %10.4f
format repwtp46    %10.4f
format repwtp47    %10.4f
format repwtp48    %10.4f
format repwtp49    %10.4f
format repwtp50    %10.4f
format repwtp51    %10.4f
format repwtp52    %10.4f
format repwtp53    %10.4f
format repwtp54    %10.4f
format repwtp55    %10.4f
format repwtp56    %10.4f
format repwtp57    %10.4f
format repwtp58    %10.4f
format repwtp59    %10.4f
format repwtp60    %10.4f
format repwtp61    %10.4f
format repwtp62    %10.4f
format repwtp63    %10.4f
format repwtp64    %10.4f
format repwtp65    %10.4f
format repwtp66    %10.4f
format repwtp67    %10.4f
format repwtp68    %10.4f
format repwtp69    %10.4f
format repwtp70    %10.4f
format repwtp71    %10.4f
format repwtp72    %10.4f
format repwtp73    %10.4f
format repwtp74    %10.4f
format repwtp75    %10.4f
format repwtp76    %10.4f
format repwtp77    %10.4f
format repwtp78    %10.4f
format repwtp79    %10.4f
format repwtp80    %10.4f
format repwtp81    %10.4f
format repwtp82    %10.4f
format repwtp83    %10.4f
format repwtp84    %10.4f
format repwtp85    %10.4f
format repwtp86    %10.4f
format repwtp87    %10.4f
format repwtp88    %10.4f
format repwtp89    %10.4f
format repwtp90    %10.4f
format repwtp91    %10.4f
format repwtp92    %10.4f
format repwtp93    %10.4f
format repwtp94    %10.4f
format repwtp95    %10.4f
format repwtp96    %10.4f
format repwtp97    %10.4f
format repwtp98    %10.4f
format repwtp99    %10.4f
format repwtp100   %10.4f
format repwtp101   %10.4f
format repwtp102   %10.4f
format repwtp103   %10.4f
format repwtp104   %10.4f
format repwtp105   %10.4f
format repwtp106   %10.4f
format repwtp107   %10.4f
format repwtp108   %10.4f
format repwtp109   %10.4f
format repwtp110   %10.4f
format repwtp111   %10.4f
format repwtp112   %10.4f
format repwtp113   %10.4f
format repwtp114   %10.4f
format repwtp115   %10.4f
format repwtp116   %10.4f
format repwtp117   %10.4f
format repwtp118   %10.4f
format repwtp119   %10.4f
format repwtp120   %10.4f
format repwtp121   %10.4f
format repwtp122   %10.4f
format repwtp123   %10.4f
format repwtp124   %10.4f
format repwtp125   %10.4f
format repwtp126   %10.4f
format repwtp127   %10.4f
format repwtp128   %10.4f
format repwtp129   %10.4f
format repwtp130   %10.4f
format repwtp131   %10.4f
format repwtp132   %10.4f
format repwtp133   %10.4f
format repwtp134   %10.4f
format repwtp135   %10.4f
format repwtp136   %10.4f
format repwtp137   %10.4f
format repwtp138   %10.4f
format repwtp139   %10.4f
format repwtp140   %10.4f
format repwtp141   %10.4f
format repwtp142   %10.4f
format repwtp143   %10.4f
format repwtp144   %10.4f
format repwtp145   %10.4f
format repwtp146   %10.4f
format repwtp147   %10.4f
format repwtp148   %10.4f
format repwtp149   %10.4f
format repwtp150   %10.4f
format repwtp151   %10.4f
format repwtp152   %10.4f
format repwtp153   %10.4f
format repwtp154   %10.4f
format repwtp155   %10.4f
format repwtp156   %10.4f
format repwtp157   %10.4f
format repwtp158   %10.4f
format repwtp159   %10.4f
format repwtp160   %10.4f
format ftotval     %10.0f
format inctot      %9.0f
format offtotval   %10.0f

label var year        `"Survey year"'
label var serial      `"Household serial number"'
label var month       `"Month"'
label var hwtfinl     `"Household weight, Basic Monthly"'
label var cpsid       `"CPSID, household record"'
label var asecflag    `"Flag for ASEC"'
label var asecwth     `"Annual Social and Economic Supplement Household weight"'
label var repwt       `"Household replicate weights"'
label var repwt1      `"Household replicate weight 1"'
label var repwt2      `"Household replicate weight 2"'
label var repwt3      `"Household replicate weight 3"'
label var repwt4      `"Household replicate weight 4"'
label var repwt5      `"Household replicate weight 5"'
label var repwt6      `"Household replicate weight 6"'
label var repwt7      `"Household replicate weight 7"'
label var repwt8      `"Household replicate weight 8"'
label var repwt9      `"Household replicate weight 9"'
label var repwt10     `"Household replicate weight 10"'
label var repwt11     `"Household replicate weight 11"'
label var repwt12     `"Household replicate weight 12"'
label var repwt13     `"Household replicate weight 13"'
label var repwt14     `"Household replicate weight 14"'
label var repwt15     `"Household replicate weight 15"'
label var repwt16     `"Household replicate weight 16"'
label var repwt17     `"Household replicate weight 17"'
label var repwt18     `"Household replicate weight 18"'
label var repwt19     `"Household replicate weight 19"'
label var repwt20     `"Household replicate weight 20"'
label var repwt21     `"Household replicate weight 21"'
label var repwt22     `"Household replicate weight 22"'
label var repwt23     `"Household replicate weight 23"'
label var repwt24     `"Household replicate weight 24"'
label var repwt25     `"Household replicate weight 25"'
label var repwt26     `"Household replicate weight 26"'
label var repwt27     `"Household replicate weight 27"'
label var repwt28     `"Household replicate weight 28"'
label var repwt29     `"Household replicate weight 29"'
label var repwt30     `"Household replicate weight 30"'
label var repwt31     `"Household replicate weight 31"'
label var repwt32     `"Household replicate weight 32"'
label var repwt33     `"Household replicate weight 33"'
label var repwt34     `"Household replicate weight 34"'
label var repwt35     `"Household replicate weight 35"'
label var repwt36     `"Household replicate weight 36"'
label var repwt37     `"Household replicate weight 37"'
label var repwt38     `"Household replicate weight 38"'
label var repwt39     `"Household replicate weight 39"'
label var repwt40     `"Household replicate weight 40"'
label var repwt41     `"Household replicate weight 41"'
label var repwt42     `"Household replicate weight 42"'
label var repwt43     `"Household replicate weight 43"'
label var repwt44     `"Household replicate weight 44"'
label var repwt45     `"Household replicate weight 45"'
label var repwt46     `"Household replicate weight 46"'
label var repwt47     `"Household replicate weight 47"'
label var repwt48     `"Household replicate weight 48"'
label var repwt49     `"Household replicate weight 49"'
label var repwt50     `"Household replicate weight 50"'
label var repwt51     `"Household replicate weight 51"'
label var repwt52     `"Household replicate weight 52"'
label var repwt53     `"Household replicate weight 53"'
label var repwt54     `"Household replicate weight 54"'
label var repwt55     `"Household replicate weight 55"'
label var repwt56     `"Household replicate weight 56"'
label var repwt57     `"Household replicate weight 57"'
label var repwt58     `"Household replicate weight 58"'
label var repwt59     `"Household replicate weight 59"'
label var repwt60     `"Household replicate weight 60"'
label var repwt61     `"Household replicate weight 61"'
label var repwt62     `"Household replicate weight 62"'
label var repwt63     `"Household replicate weight 63"'
label var repwt64     `"Household replicate weight 64"'
label var repwt65     `"Household replicate weight 65"'
label var repwt66     `"Household replicate weight 66"'
label var repwt67     `"Household replicate weight 67"'
label var repwt68     `"Household replicate weight 68"'
label var repwt69     `"Household replicate weight 69"'
label var repwt70     `"Household replicate weight 70"'
label var repwt71     `"Household replicate weight 71"'
label var repwt72     `"Household replicate weight 72"'
label var repwt73     `"Household replicate weight 73"'
label var repwt74     `"Household replicate weight 74"'
label var repwt75     `"Household replicate weight 75"'
label var repwt76     `"Household replicate weight 76"'
label var repwt77     `"Household replicate weight 77"'
label var repwt78     `"Household replicate weight 78"'
label var repwt79     `"Household replicate weight 79"'
label var repwt80     `"Household replicate weight 80"'
label var repwt81     `"Household replicate weight 81"'
label var repwt82     `"Household replicate weight 82"'
label var repwt83     `"Household replicate weight 83"'
label var repwt84     `"Household replicate weight 84"'
label var repwt85     `"Household replicate weight 85"'
label var repwt86     `"Household replicate weight 86"'
label var repwt87     `"Household replicate weight 87"'
label var repwt88     `"Household replicate weight 88"'
label var repwt89     `"Household replicate weight 89"'
label var repwt90     `"Household replicate weight 90"'
label var repwt91     `"Household replicate weight 91"'
label var repwt92     `"Household replicate weight 92"'
label var repwt93     `"Household replicate weight 93"'
label var repwt94     `"Household replicate weight 94"'
label var repwt95     `"Household replicate weight 95"'
label var repwt96     `"Household replicate weight 96"'
label var repwt97     `"Household replicate weight 97"'
label var repwt98     `"Household replicate weight 98"'
label var repwt99     `"Household replicate weight 99"'
label var repwt100    `"Household replicate weight 100"'
label var repwt101    `"Household replicate weight 101"'
label var repwt102    `"Household replicate weight 102"'
label var repwt103    `"Household replicate weight 103"'
label var repwt104    `"Household replicate weight 104"'
label var repwt105    `"Household replicate weight 105"'
label var repwt106    `"Household replicate weight 106"'
label var repwt107    `"Household replicate weight 107"'
label var repwt108    `"Household replicate weight 108"'
label var repwt109    `"Household replicate weight 109"'
label var repwt110    `"Household replicate weight 110"'
label var repwt111    `"Household replicate weight 111"'
label var repwt112    `"Household replicate weight 112"'
label var repwt113    `"Household replicate weight 113"'
label var repwt114    `"Household replicate weight 114"'
label var repwt115    `"Household replicate weight 115"'
label var repwt116    `"Household replicate weight 116"'
label var repwt117    `"Household replicate weight 117"'
label var repwt118    `"Household replicate weight 118"'
label var repwt119    `"Household replicate weight 119"'
label var repwt120    `"Household replicate weight 120"'
label var repwt121    `"Household replicate weight 121"'
label var repwt122    `"Household replicate weight 122"'
label var repwt123    `"Household replicate weight 123"'
label var repwt124    `"Household replicate weight 124"'
label var repwt125    `"Household replicate weight 125"'
label var repwt126    `"Household replicate weight 126"'
label var repwt127    `"Household replicate weight 127"'
label var repwt128    `"Household replicate weight 128"'
label var repwt129    `"Household replicate weight 129"'
label var repwt130    `"Household replicate weight 130"'
label var repwt131    `"Household replicate weight 131"'
label var repwt132    `"Household replicate weight 132"'
label var repwt133    `"Household replicate weight 133"'
label var repwt134    `"Household replicate weight 134"'
label var repwt135    `"Household replicate weight 135"'
label var repwt136    `"Household replicate weight 136"'
label var repwt137    `"Household replicate weight 137"'
label var repwt138    `"Household replicate weight 138"'
label var repwt139    `"Household replicate weight 139"'
label var repwt140    `"Household replicate weight 140"'
label var repwt141    `"Household replicate weight 141"'
label var repwt142    `"Household replicate weight 142"'
label var repwt143    `"Household replicate weight 143"'
label var repwt144    `"Household replicate weight 144"'
label var repwt145    `"Household replicate weight 145"'
label var repwt146    `"Household replicate weight 146"'
label var repwt147    `"Household replicate weight 147"'
label var repwt148    `"Household replicate weight 148"'
label var repwt149    `"Household replicate weight 149"'
label var repwt150    `"Household replicate weight 150"'
label var repwt151    `"Household replicate weight 151"'
label var repwt152    `"Household replicate weight 152"'
label var repwt153    `"Household replicate weight 153"'
label var repwt154    `"Household replicate weight 154"'
label var repwt155    `"Household replicate weight 155"'
label var repwt156    `"Household replicate weight 156"'
label var repwt157    `"Household replicate weight 157"'
label var repwt158    `"Household replicate weight 158"'
label var repwt159    `"Household replicate weight 159"'
label var repwt160    `"Household replicate weight 160"'
label var statefip    `"State (FIPS code)"'
label var hhincome    `"Total household income"'
label var fspoor      `"Household poverty status"'
label var pernum      `"Person number in sample unit"'
label var wtfinl      `"Final Basic Weight"'
label var cpsidp      `"CPSID, person record"'
label var asecwt      `"Annual Social and Economic Supplement Weight"'
label var relate      `"Relationship to household head"'
label var age         `"Age"'
label var sex         `"Sex"'
label var race        `"Race"'
label var marst       `"Marital status"'
label var asian       `"Asian subgroup"'
label var famsize     `"Number of own family members in hh"'
label var ftype       `"Family Type"'
label var hispan      `"Hispanic origin"'
label var empstat     `"Employment status"'
label var occ         `"Occupation"'
label var classwkr    `"Class of worker "'
label var educ        `"Educational attainment recode"'
label var educ99      `"Educational attainment, 1990"'
label var repwtp      `"Person replicate weights"'
label var repwtp1     `"Person replicate weight 1"'
label var repwtp2     `"Person replicate weight 2"'
label var repwtp3     `"Person replicate weight 3"'
label var repwtp4     `"Person replicate weight 4"'
label var repwtp5     `"Person replicate weight 5"'
label var repwtp6     `"Person replicate weight 6"'
label var repwtp7     `"Person replicate weight 7"'
label var repwtp8     `"Person replicate weight 8"'
label var repwtp9     `"Person replicate weight 9"'
label var repwtp10    `"Person replicate weight 10"'
label var repwtp11    `"Person replicate weight 11"'
label var repwtp12    `"Person replicate weight 12"'
label var repwtp13    `"Person replicate weight 13"'
label var repwtp14    `"Person replicate weight 14"'
label var repwtp15    `"Person replicate weight 15"'
label var repwtp16    `"Person replicate weight 16"'
label var repwtp17    `"Person replicate weight 17"'
label var repwtp18    `"Person replicate weight 18"'
label var repwtp19    `"Person replicate weight 19"'
label var repwtp20    `"Person replicate weight 20"'
label var repwtp21    `"Person replicate weight 21"'
label var repwtp22    `"Person replicate weight 22"'
label var repwtp23    `"Person replicate weight 23"'
label var repwtp24    `"Person replicate weight 24"'
label var repwtp25    `"Person replicate weight 25"'
label var repwtp26    `"Person replicate weight 26"'
label var repwtp27    `"Person replicate weight 27"'
label var repwtp28    `"Person replicate weight 28"'
label var repwtp29    `"Person replicate weight 29"'
label var repwtp30    `"Person replicate weight 30"'
label var repwtp31    `"Person replicate weight 31"'
label var repwtp32    `"Person replicate weight 32"'
label var repwtp33    `"Person replicate weight 33"'
label var repwtp34    `"Person replicate weight 34"'
label var repwtp35    `"Person replicate weight 35"'
label var repwtp36    `"Person replicate weight 36"'
label var repwtp37    `"Person replicate weight 37"'
label var repwtp38    `"Person replicate weight 38"'
label var repwtp39    `"Person replicate weight 39"'
label var repwtp40    `"Person replicate weight 40"'
label var repwtp41    `"Person replicate weight 41"'
label var repwtp42    `"Person replicate weight 42"'
label var repwtp43    `"Person replicate weight 43"'
label var repwtp44    `"Person replicate weight 44"'
label var repwtp45    `"Person replicate weight 45"'
label var repwtp46    `"Person replicate weight 46"'
label var repwtp47    `"Person replicate weight 47"'
label var repwtp48    `"Person replicate weight 48"'
label var repwtp49    `"Person replicate weight 49"'
label var repwtp50    `"Person replicate weight 50"'
label var repwtp51    `"Person replicate weight 51"'
label var repwtp52    `"Person replicate weight 52"'
label var repwtp53    `"Person replicate weight 53"'
label var repwtp54    `"Person replicate weight 54"'
label var repwtp55    `"Person replicate weight 55"'
label var repwtp56    `"Person replicate weight 56"'
label var repwtp57    `"Person replicate weight 57"'
label var repwtp58    `"Person replicate weight 58"'
label var repwtp59    `"Person replicate weight 59"'
label var repwtp60    `"Person replicate weight 60"'
label var repwtp61    `"Person replicate weight 61"'
label var repwtp62    `"Person replicate weight 62"'
label var repwtp63    `"Person replicate weight 63"'
label var repwtp64    `"Person replicate weight 64"'
label var repwtp65    `"Person replicate weight 65"'
label var repwtp66    `"Person replicate weight 66"'
label var repwtp67    `"Person replicate weight 67"'
label var repwtp68    `"Person replicate weight 68"'
label var repwtp69    `"Person replicate weight 69"'
label var repwtp70    `"Person replicate weight 70"'
label var repwtp71    `"Person replicate weight 71"'
label var repwtp72    `"Person replicate weight 72"'
label var repwtp73    `"Person replicate weight 73"'
label var repwtp74    `"Person replicate weight 74"'
label var repwtp75    `"Person replicate weight 75"'
label var repwtp76    `"Person replicate weight 76"'
label var repwtp77    `"Person replicate weight 77"'
label var repwtp78    `"Person replicate weight 78"'
label var repwtp79    `"Person replicate weight 79"'
label var repwtp80    `"Person replicate weight 80"'
label var repwtp81    `"Person replicate weight 81"'
label var repwtp82    `"Person replicate weight 82"'
label var repwtp83    `"Person replicate weight 83"'
label var repwtp84    `"Person replicate weight 84"'
label var repwtp85    `"Person replicate weight 85"'
label var repwtp86    `"Person replicate weight 86"'
label var repwtp87    `"Person replicate weight 87"'
label var repwtp88    `"Person replicate weight 88"'
label var repwtp89    `"Person replicate weight 89"'
label var repwtp90    `"Person replicate weight 90"'
label var repwtp91    `"Person replicate weight 91"'
label var repwtp92    `"Person replicate weight 92"'
label var repwtp93    `"Person replicate weight 93"'
label var repwtp94    `"Person replicate weight 94"'
label var repwtp95    `"Person replicate weight 95"'
label var repwtp96    `"Person replicate weight 96"'
label var repwtp97    `"Person replicate weight 97"'
label var repwtp98    `"Person replicate weight 98"'
label var repwtp99    `"Person replicate weight 99"'
label var repwtp100   `"Person replicate weight 100"'
label var repwtp101   `"Person replicate weight 101"'
label var repwtp102   `"Person replicate weight 102"'
label var repwtp103   `"Person replicate weight 103"'
label var repwtp104   `"Person replicate weight 104"'
label var repwtp105   `"Person replicate weight 105"'
label var repwtp106   `"Person replicate weight 106"'
label var repwtp107   `"Person replicate weight 107"'
label var repwtp108   `"Person replicate weight 108"'
label var repwtp109   `"Person replicate weight 109"'
label var repwtp110   `"Person replicate weight 110"'
label var repwtp111   `"Person replicate weight 111"'
label var repwtp112   `"Person replicate weight 112"'
label var repwtp113   `"Person replicate weight 113"'
label var repwtp114   `"Person replicate weight 114"'
label var repwtp115   `"Person replicate weight 115"'
label var repwtp116   `"Person replicate weight 116"'
label var repwtp117   `"Person replicate weight 117"'
label var repwtp118   `"Person replicate weight 118"'
label var repwtp119   `"Person replicate weight 119"'
label var repwtp120   `"Person replicate weight 120"'
label var repwtp121   `"Person replicate weight 121"'
label var repwtp122   `"Person replicate weight 122"'
label var repwtp123   `"Person replicate weight 123"'
label var repwtp124   `"Person replicate weight 124"'
label var repwtp125   `"Person replicate weight 125"'
label var repwtp126   `"Person replicate weight 126"'
label var repwtp127   `"Person replicate weight 127"'
label var repwtp128   `"Person replicate weight 128"'
label var repwtp129   `"Person replicate weight 129"'
label var repwtp130   `"Person replicate weight 130"'
label var repwtp131   `"Person replicate weight 131"'
label var repwtp132   `"Person replicate weight 132"'
label var repwtp133   `"Person replicate weight 133"'
label var repwtp134   `"Person replicate weight 134"'
label var repwtp135   `"Person replicate weight 135"'
label var repwtp136   `"Person replicate weight 136"'
label var repwtp137   `"Person replicate weight 137"'
label var repwtp138   `"Person replicate weight 138"'
label var repwtp139   `"Person replicate weight 139"'
label var repwtp140   `"Person replicate weight 140"'
label var repwtp141   `"Person replicate weight 141"'
label var repwtp142   `"Person replicate weight 142"'
label var repwtp143   `"Person replicate weight 143"'
label var repwtp144   `"Person replicate weight 144"'
label var repwtp145   `"Person replicate weight 145"'
label var repwtp146   `"Person replicate weight 146"'
label var repwtp147   `"Person replicate weight 147"'
label var repwtp148   `"Person replicate weight 148"'
label var repwtp149   `"Person replicate weight 149"'
label var repwtp150   `"Person replicate weight 150"'
label var repwtp151   `"Person replicate weight 151"'
label var repwtp152   `"Person replicate weight 152"'
label var repwtp153   `"Person replicate weight 153"'
label var repwtp154   `"Person replicate weight 154"'
label var repwtp155   `"Person replicate weight 155"'
label var repwtp156   `"Person replicate weight 156"'
label var repwtp157   `"Person replicate weight 157"'
label var repwtp158   `"Person replicate weight 158"'
label var repwtp159   `"Person replicate weight 159"'
label var repwtp160   `"Person replicate weight 160"'
label var classwly    `"Class of worker last year"'
label var ftotval     `"Total family income"'
label var inctot      `"Total personal income"'
label var offpov      `"Official Poverty Status (IPUMS constructed)"'
label var offtotval   `"Total Family Income for Replicating Official Poverty Rates"'
label var offcutoff   `"Official Poverty Rate Cutoff"'
label var poverty     `"Original poverty status (PUMS original)"'
label var hiufpginc   `"Federal poverty guidelines (increment)"'
label var edgrade     `"Current level of school enrollment"'
label var educ_head   `"Educational attainment recode [of Location of householder]"'
label var educ_mom    `"Educational attainment recode [of Person number of first mother (from programmin"'
label var educ_mom2   `"Educational attainment recode [of Person number of second mother (from programmi"'
label var educ_pop    `"Educational attainment recode [of Person number of first father (from programmin"'
label var educ_pop2   `"Educational attainment recode [of Person number of second father (from programmi"'
label var educ_sp     `"Educational attainment recode [of Person number of spouse (from programming)]"'
label var educ99_head `"Educational attainment, 1990 [of Location of householder]"'
label var educ99_mom  `"Educational attainment, 1990 [of Person number of first mother (from programming"'
label var educ99_mom2 `"Educational attainment, 1990 [of Person number of second mother (from programmin"'
label var educ99_pop  `"Educational attainment, 1990 [of Person number of first father (from programming"'
label var educ99_pop2 `"Educational attainment, 1990 [of Person number of second father (from programmin"'
label var educ99_sp   `"Educational attainment, 1990 [of Person number of spouse (from programming)]"'

label define month_lbl 01 `"January"'
label define month_lbl 02 `"February"', add
label define month_lbl 03 `"March"', add
label define month_lbl 04 `"April"', add
label define month_lbl 05 `"May"', add
label define month_lbl 06 `"June"', add
label define month_lbl 07 `"July"', add
label define month_lbl 08 `"August"', add
label define month_lbl 09 `"September"', add
label define month_lbl 10 `"October"', add
label define month_lbl 11 `"November"', add
label define month_lbl 12 `"December"', add
label values month month_lbl

label define asecflag_lbl 1 `"ASEC"'
label define asecflag_lbl 2 `"March Basic"', add
label values asecflag asecflag_lbl

label define repwt_lbl 1 `"Repwtp available"'
label values repwt repwt_lbl

label define statefip_lbl 01 `"Alabama"'
label define statefip_lbl 02 `"Alaska"', add
label define statefip_lbl 04 `"Arizona"', add
label define statefip_lbl 05 `"Arkansas"', add
label define statefip_lbl 06 `"California"', add
label define statefip_lbl 08 `"Colorado"', add
label define statefip_lbl 09 `"Connecticut"', add
label define statefip_lbl 10 `"Delaware"', add
label define statefip_lbl 11 `"District of Columbia"', add
label define statefip_lbl 12 `"Florida"', add
label define statefip_lbl 13 `"Georgia"', add
label define statefip_lbl 15 `"Hawaii"', add
label define statefip_lbl 16 `"Idaho"', add
label define statefip_lbl 17 `"Illinois"', add
label define statefip_lbl 18 `"Indiana"', add
label define statefip_lbl 19 `"Iowa"', add
label define statefip_lbl 20 `"Kansas"', add
label define statefip_lbl 21 `"Kentucky"', add
label define statefip_lbl 22 `"Louisiana"', add
label define statefip_lbl 23 `"Maine"', add
label define statefip_lbl 24 `"Maryland"', add
label define statefip_lbl 25 `"Massachusetts"', add
label define statefip_lbl 26 `"Michigan"', add
label define statefip_lbl 27 `"Minnesota"', add
label define statefip_lbl 28 `"Mississippi"', add
label define statefip_lbl 29 `"Missouri"', add
label define statefip_lbl 30 `"Montana"', add
label define statefip_lbl 31 `"Nebraska"', add
label define statefip_lbl 32 `"Nevada"', add
label define statefip_lbl 33 `"New Hampshire"', add
label define statefip_lbl 34 `"New Jersey"', add
label define statefip_lbl 35 `"New Mexico"', add
label define statefip_lbl 36 `"New York"', add
label define statefip_lbl 37 `"North Carolina"', add
label define statefip_lbl 38 `"North Dakota"', add
label define statefip_lbl 39 `"Ohio"', add
label define statefip_lbl 40 `"Oklahoma"', add
label define statefip_lbl 41 `"Oregon"', add
label define statefip_lbl 42 `"Pennsylvania"', add
label define statefip_lbl 44 `"Rhode Island"', add
label define statefip_lbl 45 `"South Carolina"', add
label define statefip_lbl 46 `"South Dakota"', add
label define statefip_lbl 47 `"Tennessee"', add
label define statefip_lbl 48 `"Texas"', add
label define statefip_lbl 49 `"Utah"', add
label define statefip_lbl 50 `"Vermont"', add
label define statefip_lbl 51 `"Virginia"', add
label define statefip_lbl 53 `"Washington"', add
label define statefip_lbl 54 `"West Virginia"', add
label define statefip_lbl 55 `"Wisconsin"', add
label define statefip_lbl 56 `"Wyoming"', add
label define statefip_lbl 61 `"Maine-New Hampshire-Vermont"', add
label define statefip_lbl 65 `"Montana-Idaho-Wyoming"', add
label define statefip_lbl 68 `"Alaska-Hawaii"', add
label define statefip_lbl 69 `"Nebraska-North Dakota-South Dakota"', add
label define statefip_lbl 70 `"Maine-Massachusetts-New Hampshire-Rhode Island-Vermont"', add
label define statefip_lbl 71 `"Michigan-Wisconsin"', add
label define statefip_lbl 72 `"Minnesota-Iowa"', add
label define statefip_lbl 73 `"Nebraska-North Dakota-South Dakota-Kansas"', add
label define statefip_lbl 74 `"Delaware-Virginia"', add
label define statefip_lbl 75 `"North Carolina-South Carolina"', add
label define statefip_lbl 76 `"Alabama-Mississippi"', add
label define statefip_lbl 77 `"Arkansas-Oklahoma"', add
label define statefip_lbl 78 `"Arizona-New Mexico-Colorado"', add
label define statefip_lbl 79 `"Idaho-Wyoming-Utah-Montana-Nevada"', add
label define statefip_lbl 80 `"Alaska-Washington-Hawaii"', add
label define statefip_lbl 81 `"New Hampshire-Maine-Vermont-Rhode Island"', add
label define statefip_lbl 83 `"South Carolina-Georgia"', add
label define statefip_lbl 84 `"Kentucky-Tennessee"', add
label define statefip_lbl 85 `"Arkansas-Louisiana-Oklahoma"', add
label define statefip_lbl 87 `"Iowa-N Dakota-S Dakota-Nebraska-Kansas-Minnesota-Missouri"', add
label define statefip_lbl 88 `"Washington-Oregon-Alaska-Hawaii"', add
label define statefip_lbl 89 `"Montana-Wyoming-Colorado-New Mexico-Utah-Nevada-Arizona"', add
label define statefip_lbl 90 `"Delaware-Maryland-Virginia-West Virginia"', add
label define statefip_lbl 99 `"State not identified"', add
label values statefip statefip_lbl

label define fspoor_lbl 01 `"Below 185% poverty"'
label define fspoor_lbl 02 `"Above 185% poverty or income not reported"', add
label define fspoor_lbl 99 `"NIU"', add
label values fspoor fspoor_lbl

label define relate_lbl 0101 `"Head/householder"'
label define relate_lbl 0201 `"Spouse"', add
label define relate_lbl 0202 `"Opposite sex spouse"', add
label define relate_lbl 0203 `"Same sex spouse"', add
label define relate_lbl 0301 `"Child"', add
label define relate_lbl 0303 `"Stepchild"', add
label define relate_lbl 0501 `"Parent"', add
label define relate_lbl 0701 `"Sibling"', add
label define relate_lbl 0901 `"Grandchild"', add
label define relate_lbl 1001 `"Other relatives, n.s."', add
label define relate_lbl 1113 `"Partner/roommate"', add
label define relate_lbl 1114 `"Unmarried partner"', add
label define relate_lbl 1116 `"Opposite sex unmarried partner"', add
label define relate_lbl 1117 `"Same sex unmarried partner"', add
label define relate_lbl 1115 `"Housemate/roomate"', add
label define relate_lbl 1241 `"Roomer/boarder/lodger"', add
label define relate_lbl 1242 `"Foster children"', add
label define relate_lbl 1260 `"Other nonrelatives"', add
label define relate_lbl 9900 `"Relationship unknown"', add
label define relate_lbl 9999 `"NIU"', add
label values relate relate_lbl

label define age_lbl 00 `"Under 1 year"'
label define age_lbl 01 `"1"', add
label define age_lbl 02 `"2"', add
label define age_lbl 03 `"3"', add
label define age_lbl 04 `"4"', add
label define age_lbl 05 `"5"', add
label define age_lbl 06 `"6"', add
label define age_lbl 07 `"7"', add
label define age_lbl 08 `"8"', add
label define age_lbl 09 `"9"', add
label define age_lbl 10 `"10"', add
label define age_lbl 11 `"11"', add
label define age_lbl 12 `"12"', add
label define age_lbl 13 `"13"', add
label define age_lbl 14 `"14"', add
label define age_lbl 15 `"15"', add
label define age_lbl 16 `"16"', add
label define age_lbl 17 `"17"', add
label define age_lbl 18 `"18"', add
label define age_lbl 19 `"19"', add
label define age_lbl 20 `"20"', add
label define age_lbl 21 `"21"', add
label define age_lbl 22 `"22"', add
label define age_lbl 23 `"23"', add
label define age_lbl 24 `"24"', add
label define age_lbl 25 `"25"', add
label define age_lbl 26 `"26"', add
label define age_lbl 27 `"27"', add
label define age_lbl 28 `"28"', add
label define age_lbl 29 `"29"', add
label define age_lbl 30 `"30"', add
label define age_lbl 31 `"31"', add
label define age_lbl 32 `"32"', add
label define age_lbl 33 `"33"', add
label define age_lbl 34 `"34"', add
label define age_lbl 35 `"35"', add
label define age_lbl 36 `"36"', add
label define age_lbl 37 `"37"', add
label define age_lbl 38 `"38"', add
label define age_lbl 39 `"39"', add
label define age_lbl 40 `"40"', add
label define age_lbl 41 `"41"', add
label define age_lbl 42 `"42"', add
label define age_lbl 43 `"43"', add
label define age_lbl 44 `"44"', add
label define age_lbl 45 `"45"', add
label define age_lbl 46 `"46"', add
label define age_lbl 47 `"47"', add
label define age_lbl 48 `"48"', add
label define age_lbl 49 `"49"', add
label define age_lbl 50 `"50"', add
label define age_lbl 51 `"51"', add
label define age_lbl 52 `"52"', add
label define age_lbl 53 `"53"', add
label define age_lbl 54 `"54"', add
label define age_lbl 55 `"55"', add
label define age_lbl 56 `"56"', add
label define age_lbl 57 `"57"', add
label define age_lbl 58 `"58"', add
label define age_lbl 59 `"59"', add
label define age_lbl 60 `"60"', add
label define age_lbl 61 `"61"', add
label define age_lbl 62 `"62"', add
label define age_lbl 63 `"63"', add
label define age_lbl 64 `"64"', add
label define age_lbl 65 `"65"', add
label define age_lbl 66 `"66"', add
label define age_lbl 67 `"67"', add
label define age_lbl 68 `"68"', add
label define age_lbl 69 `"69"', add
label define age_lbl 70 `"70"', add
label define age_lbl 71 `"71"', add
label define age_lbl 72 `"72"', add
label define age_lbl 73 `"73"', add
label define age_lbl 74 `"74"', add
label define age_lbl 75 `"75"', add
label define age_lbl 76 `"76"', add
label define age_lbl 77 `"77"', add
label define age_lbl 78 `"78"', add
label define age_lbl 79 `"79"', add
label define age_lbl 80 `"80"', add
label define age_lbl 81 `"81"', add
label define age_lbl 82 `"82"', add
label define age_lbl 83 `"83"', add
label define age_lbl 84 `"84"', add
label define age_lbl 85 `"85"', add
label define age_lbl 86 `"86"', add
label define age_lbl 87 `"87"', add
label define age_lbl 88 `"88"', add
label define age_lbl 89 `"89"', add
label define age_lbl 90 `"90 (90+, 1988-2002)"', add
label define age_lbl 91 `"91"', add
label define age_lbl 92 `"92"', add
label define age_lbl 93 `"93"', add
label define age_lbl 94 `"94"', add
label define age_lbl 95 `"95"', add
label define age_lbl 96 `"96"', add
label define age_lbl 97 `"97"', add
label define age_lbl 98 `"98"', add
label define age_lbl 99 `"99+"', add
label values age age_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label define sex_lbl 9 `"NIU"', add
label values sex sex_lbl

label define race_lbl 100 `"White"'
label define race_lbl 200 `"Black"', add
label define race_lbl 300 `"American Indian/Aleut/Eskimo"', add
label define race_lbl 650 `"Asian or Pacific Islander"', add
label define race_lbl 651 `"Asian only"', add
label define race_lbl 652 `"Hawaiian/Pacific Islander only"', add
label define race_lbl 700 `"Other (single) race, n.e.c."', add
label define race_lbl 801 `"White-Black"', add
label define race_lbl 802 `"White-American Indian"', add
label define race_lbl 803 `"White-Asian"', add
label define race_lbl 804 `"White-Hawaiian/Pacific Islander"', add
label define race_lbl 805 `"Black-American Indian"', add
label define race_lbl 806 `"Black-Asian"', add
label define race_lbl 807 `"Black-Hawaiian/Pacific Islander"', add
label define race_lbl 808 `"American Indian-Asian"', add
label define race_lbl 809 `"Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 810 `"White-Black-American Indian"', add
label define race_lbl 811 `"White-Black-Asian"', add
label define race_lbl 812 `"White-American Indian-Asian"', add
label define race_lbl 813 `"White-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 814 `"White-Black-American Indian-Asian"', add
label define race_lbl 815 `"American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 816 `"White-Black--Hawaiian/Pacific Islander"', add
label define race_lbl 817 `"White-American Indian-Hawaiian/Pacific Islander"', add
label define race_lbl 818 `"Black-American Indian-Asian"', add
label define race_lbl 819 `"White-American Indian-Asian-Hawaiian/Pacific Islander"', add
label define race_lbl 820 `"Two or three races, unspecified"', add
label define race_lbl 830 `"Four or five races, unspecified"', add
label define race_lbl 999 `"Blank"', add
label values race race_lbl

label define marst_lbl 1 `"Married, spouse present"'
label define marst_lbl 2 `"Married, spouse absent"', add
label define marst_lbl 3 `"Separated"', add
label define marst_lbl 4 `"Divorced"', add
label define marst_lbl 5 `"Widowed"', add
label define marst_lbl 6 `"Never married/single"', add
label define marst_lbl 7 `"Widowed or Divorced"', add
label define marst_lbl 9 `"NIU"', add
label values marst marst_lbl

label define asian_lbl 10 `"Asian Indian"'
label define asian_lbl 20 `"Chinese"', add
label define asian_lbl 30 `"Filipino"', add
label define asian_lbl 40 `"Japanese"', add
label define asian_lbl 50 `"Korean"', add
label define asian_lbl 60 `"Vietnamese"', add
label define asian_lbl 70 `"Other Asian"', add
label define asian_lbl 99 `"NIU"', add
label values asian asian_lbl

label define famsize_lbl 00 `"Missing"'
label define famsize_lbl 01 `"1 family member present"', add
label define famsize_lbl 02 `"2 family members present"', add
label define famsize_lbl 03 `"3 family members present"', add
label define famsize_lbl 04 `"4 family members present"', add
label define famsize_lbl 05 `"5 family members present"', add
label define famsize_lbl 06 `"6 family members present"', add
label define famsize_lbl 07 `"7 family members present"', add
label define famsize_lbl 08 `"8 family members present"', add
label define famsize_lbl 09 `"9 family members present"', add
label define famsize_lbl 10 `"10 family members present"', add
label define famsize_lbl 11 `"11 family members present"', add
label define famsize_lbl 12 `"12 family members present"', add
label define famsize_lbl 13 `"13 family members present"', add
label define famsize_lbl 14 `"14 family members present"', add
label define famsize_lbl 15 `"15 family members present"', add
label define famsize_lbl 16 `"16 family members present"', add
label define famsize_lbl 17 `"17 family members present"', add
label define famsize_lbl 18 `"18 family members present"', add
label define famsize_lbl 19 `"19 family members present"', add
label define famsize_lbl 20 `"20 family members present"', add
label define famsize_lbl 21 `"21 family members present"', add
label define famsize_lbl 22 `"22 family members present"', add
label define famsize_lbl 23 `"23 family members present"', add
label define famsize_lbl 24 `"24 family members present"', add
label define famsize_lbl 25 `"25 family members present"', add
label define famsize_lbl 26 `"26 family members present"', add
label define famsize_lbl 27 `"27 family members present"', add
label define famsize_lbl 28 `"28 family members present"', add
label define famsize_lbl 29 `"29 family members present"', add
label values famsize famsize_lbl

label define ftype_lbl 1 `"Primary family"'
label define ftype_lbl 2 `"Nonfamily householder"', add
label define ftype_lbl 3 `"Related subfamily"', add
label define ftype_lbl 4 `"Unrelated subfamily"', add
label define ftype_lbl 5 `"Secondary individual"', add
label define ftype_lbl 9 `"Missing"', add
label values ftype ftype_lbl

label define hispan_lbl 000 `"Not Hispanic"'
label define hispan_lbl 100 `"Mexican"', add
label define hispan_lbl 102 `"Mexican American"', add
label define hispan_lbl 103 `"Mexicano/Mexicana"', add
label define hispan_lbl 104 `"Chicano/Chicana"', add
label define hispan_lbl 108 `"Mexican (Mexicano)"', add
label define hispan_lbl 109 `"Mexicano/Chicano"', add
label define hispan_lbl 200 `"Puerto Rican"', add
label define hispan_lbl 300 `"Cuban"', add
label define hispan_lbl 400 `"Dominican"', add
label define hispan_lbl 500 `"Salvadoran"', add
label define hispan_lbl 600 `"Other Hispanic"', add
label define hispan_lbl 610 `"Central/South American"', add
label define hispan_lbl 611 `"Central American, (excluding Salvadoran)"', add
label define hispan_lbl 612 `"South American"', add
label define hispan_lbl 901 `"Do not know"', add
label define hispan_lbl 902 `"N/A (and no response 1985-87)"', add
label values hispan hispan_lbl

label define empstat_lbl 00 `"NIU"'
label define empstat_lbl 01 `"Armed Forces"', add
label define empstat_lbl 10 `"At work"', add
label define empstat_lbl 12 `"Has job, not at work last week"', add
label define empstat_lbl 20 `"Unemployed"', add
label define empstat_lbl 21 `"Unemployed, experienced worker"', add
label define empstat_lbl 22 `"Unemployed, new worker"', add
label define empstat_lbl 30 `"Not in labor force"', add
label define empstat_lbl 31 `"NILF, housework"', add
label define empstat_lbl 32 `"NILF, unable to work"', add
label define empstat_lbl 33 `"NILF, school"', add
label define empstat_lbl 34 `"NILF, other"', add
label define empstat_lbl 35 `"NILF, unpaid, lt 15 hours"', add
label define empstat_lbl 36 `"NILF, retired"', add
label values empstat empstat_lbl

label define classwkr_lbl 00 `"NIU"'
label define classwkr_lbl 10 `"Self-employed"', add
label define classwkr_lbl 13 `"Self-employed, not incorporated"', add
label define classwkr_lbl 14 `"Self-employed, incorporated"', add
label define classwkr_lbl 20 `"Works for wages or salary"', add
label define classwkr_lbl 21 `"Wage/salary, private"', add
label define classwkr_lbl 22 `"Private, for profit"', add
label define classwkr_lbl 23 `"Private, nonprofit"', add
label define classwkr_lbl 24 `"Wage/salary, government"', add
label define classwkr_lbl 25 `"Federal government employee"', add
label define classwkr_lbl 26 `"Armed forces"', add
label define classwkr_lbl 27 `"State government employee"', add
label define classwkr_lbl 28 `"Local government employee"', add
label define classwkr_lbl 29 `"Unpaid family worker"', add
label define classwkr_lbl 99 `"Missing/Unknown"', add
label values classwkr classwkr_lbl

label define educ_lbl 000 `"NIU or no schooling"'
label define educ_lbl 001 `"NIU or blank"', add
label define educ_lbl 002 `"None or preschool"', add
label define educ_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_lbl 011 `"Grade 1"', add
label define educ_lbl 012 `"Grade 2"', add
label define educ_lbl 013 `"Grade 3"', add
label define educ_lbl 014 `"Grade 4"', add
label define educ_lbl 020 `"Grades 5 or 6"', add
label define educ_lbl 021 `"Grade 5"', add
label define educ_lbl 022 `"Grade 6"', add
label define educ_lbl 030 `"Grades 7 or 8"', add
label define educ_lbl 031 `"Grade 7"', add
label define educ_lbl 032 `"Grade 8"', add
label define educ_lbl 040 `"Grade 9"', add
label define educ_lbl 050 `"Grade 10"', add
label define educ_lbl 060 `"Grade 11"', add
label define educ_lbl 070 `"Grade 12"', add
label define educ_lbl 071 `"12th grade, no diploma"', add
label define educ_lbl 072 `"12th grade, diploma unclear"', add
label define educ_lbl 073 `"High school diploma or equivalent"', add
label define educ_lbl 080 `"1 year of college"', add
label define educ_lbl 081 `"Some college but no degree"', add
label define educ_lbl 090 `"2 years of college"', add
label define educ_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_lbl 092 `"Associate's degree, academic program"', add
label define educ_lbl 100 `"3 years of college"', add
label define educ_lbl 110 `"4 years of college"', add
label define educ_lbl 111 `"Bachelor's degree"', add
label define educ_lbl 120 `"5+ years of college"', add
label define educ_lbl 121 `"5 years of college"', add
label define educ_lbl 122 `"6+ years of college"', add
label define educ_lbl 123 `"Master's degree"', add
label define educ_lbl 124 `"Professional school degree"', add
label define educ_lbl 125 `"Doctorate degree"', add
label define educ_lbl 999 `"Missing/Unknown"', add
label values educ educ_lbl

label define educ99_lbl 00 `"NIU"'
label define educ99_lbl 01 `"No school completed"', add
label define educ99_lbl 04 `"1st-4th grade"', add
label define educ99_lbl 05 `"5th-8th grade"', add
label define educ99_lbl 06 `"9th grade"', add
label define educ99_lbl 07 `"10th grade"', add
label define educ99_lbl 08 `"11th grade"', add
label define educ99_lbl 09 `"12th grade, no diploma"', add
label define educ99_lbl 10 `"High school graduate, or GED"', add
label define educ99_lbl 11 `"Some college, no degree"', add
label define educ99_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_lbl 13 `"Associate degree, occupational program"', add
label define educ99_lbl 14 `"Associate degree, academic program"', add
label define educ99_lbl 15 `"Bachelors degree"', add
label define educ99_lbl 16 `"Masters degree"', add
label define educ99_lbl 17 `"Professional degree"', add
label define educ99_lbl 18 `"Doctorate degree"', add
label values educ99 educ99_lbl

label define classwly_lbl 00 `"NIU"'
label define classwly_lbl 10 `"Self-employed"', add
label define classwly_lbl 13 `"Self-employed, not incorporated"', add
label define classwly_lbl 14 `"Self-employed, incorporated"', add
label define classwly_lbl 20 `"Works for wages or salary"', add
label define classwly_lbl 22 `"Wage/salary, private"', add
label define classwly_lbl 24 `"Wage/salary, government"', add
label define classwly_lbl 25 `"Federal government employee"', add
label define classwly_lbl 27 `"State government employee"', add
label define classwly_lbl 28 `"Local government employee"', add
label define classwly_lbl 29 `"Unpaid family worker"', add
label define classwly_lbl 99 `"Missing/Unknown"', add
label values classwly classwly_lbl

label define offpov_lbl 01 `"Below Poverty Line"'
label define offpov_lbl 02 `"Above Poverty Line"', add
label define offpov_lbl 99 `"NIU"', add
label values offpov offpov_lbl

label define poverty_lbl 00 `"NIU"'
label define poverty_lbl 10 `"Below poverty"', add
label define poverty_lbl 20 `"Above poverty"', add
label define poverty_lbl 21 `"100-124 percent of the low-income level"', add
label define poverty_lbl 22 `"125-149 percent of the low-income level"', add
label define poverty_lbl 23 `"150 percent and above the low-income level"', add
label values poverty poverty_lbl

label define edgrade_lbl 0011 `"Nursery (pre-school, pre-K) part-day"'
label define edgrade_lbl 0012 `"Nursery (pre-school, pre-K) full-day"', add
label define edgrade_lbl 0021 `"Kindergarten part-day"', add
label define edgrade_lbl 0022 `"Kindergarten full-day"', add
label define edgrade_lbl 0101 `"1st grade"', add
label define edgrade_lbl 0102 `"2nd grade"', add
label define edgrade_lbl 0103 `"3rd grade"', add
label define edgrade_lbl 0104 `"4th grade"', add
label define edgrade_lbl 0105 `"5th grade"', add
label define edgrade_lbl 0106 `"6th grade"', add
label define edgrade_lbl 0107 `"7th grade"', add
label define edgrade_lbl 0108 `"8th grade"', add
label define edgrade_lbl 0201 `"9th grade"', add
label define edgrade_lbl 0202 `"10th grade"', add
label define edgrade_lbl 0203 `"11th grade"', add
label define edgrade_lbl 0204 `"12th grade"', add
label define edgrade_lbl 0301 `"College year 1 (freshman)"', add
label define edgrade_lbl 0302 `"College year 2 (sophomore)"', add
label define edgrade_lbl 0303 `"College year 3 (junior)"', add
label define edgrade_lbl 0304 `"College year 4 (senior)"', add
label define edgrade_lbl 0401 `"Graduate school year 1"', add
label define edgrade_lbl 0402 `"Graduate school year 2+"', add
label define edgrade_lbl 0501 `"Special School"', add
label define edgrade_lbl 9998 `"Not Avaliable"', add
label define edgrade_lbl 9999 `"NIU"', add
label values edgrade edgrade_lbl

label define educ_head_lbl 000 `"NIU or no schooling"'
label define educ_head_lbl 001 `"NIU or blank"', add
label define educ_head_lbl 002 `"None or preschool"', add
label define educ_head_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_head_lbl 011 `"Grade 1"', add
label define educ_head_lbl 012 `"Grade 2"', add
label define educ_head_lbl 013 `"Grade 3"', add
label define educ_head_lbl 014 `"Grade 4"', add
label define educ_head_lbl 020 `"Grades 5 or 6"', add
label define educ_head_lbl 021 `"Grade 5"', add
label define educ_head_lbl 022 `"Grade 6"', add
label define educ_head_lbl 030 `"Grades 7 or 8"', add
label define educ_head_lbl 031 `"Grade 7"', add
label define educ_head_lbl 032 `"Grade 8"', add
label define educ_head_lbl 040 `"Grade 9"', add
label define educ_head_lbl 050 `"Grade 10"', add
label define educ_head_lbl 060 `"Grade 11"', add
label define educ_head_lbl 070 `"Grade 12"', add
label define educ_head_lbl 071 `"12th grade, no diploma"', add
label define educ_head_lbl 072 `"12th grade, diploma unclear"', add
label define educ_head_lbl 073 `"High school diploma or equivalent"', add
label define educ_head_lbl 080 `"1 year of college"', add
label define educ_head_lbl 081 `"Some college but no degree"', add
label define educ_head_lbl 090 `"2 years of college"', add
label define educ_head_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_head_lbl 092 `"Associate's degree, academic program"', add
label define educ_head_lbl 100 `"3 years of college"', add
label define educ_head_lbl 110 `"4 years of college"', add
label define educ_head_lbl 111 `"Bachelor's degree"', add
label define educ_head_lbl 120 `"5+ years of college"', add
label define educ_head_lbl 121 `"5 years of college"', add
label define educ_head_lbl 122 `"6+ years of college"', add
label define educ_head_lbl 123 `"Master's degree"', add
label define educ_head_lbl 124 `"Professional school degree"', add
label define educ_head_lbl 125 `"Doctorate degree"', add
label define educ_head_lbl 999 `"Missing/Unknown"', add
label values educ_head educ_head_lbl

label define educ_mom_lbl 000 `"NIU or no schooling"'
label define educ_mom_lbl 001 `"NIU or blank"', add
label define educ_mom_lbl 002 `"None or preschool"', add
label define educ_mom_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_mom_lbl 011 `"Grade 1"', add
label define educ_mom_lbl 012 `"Grade 2"', add
label define educ_mom_lbl 013 `"Grade 3"', add
label define educ_mom_lbl 014 `"Grade 4"', add
label define educ_mom_lbl 020 `"Grades 5 or 6"', add
label define educ_mom_lbl 021 `"Grade 5"', add
label define educ_mom_lbl 022 `"Grade 6"', add
label define educ_mom_lbl 030 `"Grades 7 or 8"', add
label define educ_mom_lbl 031 `"Grade 7"', add
label define educ_mom_lbl 032 `"Grade 8"', add
label define educ_mom_lbl 040 `"Grade 9"', add
label define educ_mom_lbl 050 `"Grade 10"', add
label define educ_mom_lbl 060 `"Grade 11"', add
label define educ_mom_lbl 070 `"Grade 12"', add
label define educ_mom_lbl 071 `"12th grade, no diploma"', add
label define educ_mom_lbl 072 `"12th grade, diploma unclear"', add
label define educ_mom_lbl 073 `"High school diploma or equivalent"', add
label define educ_mom_lbl 080 `"1 year of college"', add
label define educ_mom_lbl 081 `"Some college but no degree"', add
label define educ_mom_lbl 090 `"2 years of college"', add
label define educ_mom_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_mom_lbl 092 `"Associate's degree, academic program"', add
label define educ_mom_lbl 100 `"3 years of college"', add
label define educ_mom_lbl 110 `"4 years of college"', add
label define educ_mom_lbl 111 `"Bachelor's degree"', add
label define educ_mom_lbl 120 `"5+ years of college"', add
label define educ_mom_lbl 121 `"5 years of college"', add
label define educ_mom_lbl 122 `"6+ years of college"', add
label define educ_mom_lbl 123 `"Master's degree"', add
label define educ_mom_lbl 124 `"Professional school degree"', add
label define educ_mom_lbl 125 `"Doctorate degree"', add
label define educ_mom_lbl 999 `"Missing/Unknown"', add
label values educ_mom educ_mom_lbl

label define educ_mom2_lbl 000 `"NIU or no schooling"'
label define educ_mom2_lbl 001 `"NIU or blank"', add
label define educ_mom2_lbl 002 `"None or preschool"', add
label define educ_mom2_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_mom2_lbl 011 `"Grade 1"', add
label define educ_mom2_lbl 012 `"Grade 2"', add
label define educ_mom2_lbl 013 `"Grade 3"', add
label define educ_mom2_lbl 014 `"Grade 4"', add
label define educ_mom2_lbl 020 `"Grades 5 or 6"', add
label define educ_mom2_lbl 021 `"Grade 5"', add
label define educ_mom2_lbl 022 `"Grade 6"', add
label define educ_mom2_lbl 030 `"Grades 7 or 8"', add
label define educ_mom2_lbl 031 `"Grade 7"', add
label define educ_mom2_lbl 032 `"Grade 8"', add
label define educ_mom2_lbl 040 `"Grade 9"', add
label define educ_mom2_lbl 050 `"Grade 10"', add
label define educ_mom2_lbl 060 `"Grade 11"', add
label define educ_mom2_lbl 070 `"Grade 12"', add
label define educ_mom2_lbl 071 `"12th grade, no diploma"', add
label define educ_mom2_lbl 072 `"12th grade, diploma unclear"', add
label define educ_mom2_lbl 073 `"High school diploma or equivalent"', add
label define educ_mom2_lbl 080 `"1 year of college"', add
label define educ_mom2_lbl 081 `"Some college but no degree"', add
label define educ_mom2_lbl 090 `"2 years of college"', add
label define educ_mom2_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_mom2_lbl 092 `"Associate's degree, academic program"', add
label define educ_mom2_lbl 100 `"3 years of college"', add
label define educ_mom2_lbl 110 `"4 years of college"', add
label define educ_mom2_lbl 111 `"Bachelor's degree"', add
label define educ_mom2_lbl 120 `"5+ years of college"', add
label define educ_mom2_lbl 121 `"5 years of college"', add
label define educ_mom2_lbl 122 `"6+ years of college"', add
label define educ_mom2_lbl 123 `"Master's degree"', add
label define educ_mom2_lbl 124 `"Professional school degree"', add
label define educ_mom2_lbl 125 `"Doctorate degree"', add
label define educ_mom2_lbl 999 `"Missing/Unknown"', add
label values educ_mom2 educ_mom2_lbl

label define educ_pop_lbl 000 `"NIU or no schooling"'
label define educ_pop_lbl 001 `"NIU or blank"', add
label define educ_pop_lbl 002 `"None or preschool"', add
label define educ_pop_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_pop_lbl 011 `"Grade 1"', add
label define educ_pop_lbl 012 `"Grade 2"', add
label define educ_pop_lbl 013 `"Grade 3"', add
label define educ_pop_lbl 014 `"Grade 4"', add
label define educ_pop_lbl 020 `"Grades 5 or 6"', add
label define educ_pop_lbl 021 `"Grade 5"', add
label define educ_pop_lbl 022 `"Grade 6"', add
label define educ_pop_lbl 030 `"Grades 7 or 8"', add
label define educ_pop_lbl 031 `"Grade 7"', add
label define educ_pop_lbl 032 `"Grade 8"', add
label define educ_pop_lbl 040 `"Grade 9"', add
label define educ_pop_lbl 050 `"Grade 10"', add
label define educ_pop_lbl 060 `"Grade 11"', add
label define educ_pop_lbl 070 `"Grade 12"', add
label define educ_pop_lbl 071 `"12th grade, no diploma"', add
label define educ_pop_lbl 072 `"12th grade, diploma unclear"', add
label define educ_pop_lbl 073 `"High school diploma or equivalent"', add
label define educ_pop_lbl 080 `"1 year of college"', add
label define educ_pop_lbl 081 `"Some college but no degree"', add
label define educ_pop_lbl 090 `"2 years of college"', add
label define educ_pop_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_pop_lbl 092 `"Associate's degree, academic program"', add
label define educ_pop_lbl 100 `"3 years of college"', add
label define educ_pop_lbl 110 `"4 years of college"', add
label define educ_pop_lbl 111 `"Bachelor's degree"', add
label define educ_pop_lbl 120 `"5+ years of college"', add
label define educ_pop_lbl 121 `"5 years of college"', add
label define educ_pop_lbl 122 `"6+ years of college"', add
label define educ_pop_lbl 123 `"Master's degree"', add
label define educ_pop_lbl 124 `"Professional school degree"', add
label define educ_pop_lbl 125 `"Doctorate degree"', add
label define educ_pop_lbl 999 `"Missing/Unknown"', add
label values educ_pop educ_pop_lbl

label define educ_pop2_lbl 000 `"NIU or no schooling"'
label define educ_pop2_lbl 001 `"NIU or blank"', add
label define educ_pop2_lbl 002 `"None or preschool"', add
label define educ_pop2_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_pop2_lbl 011 `"Grade 1"', add
label define educ_pop2_lbl 012 `"Grade 2"', add
label define educ_pop2_lbl 013 `"Grade 3"', add
label define educ_pop2_lbl 014 `"Grade 4"', add
label define educ_pop2_lbl 020 `"Grades 5 or 6"', add
label define educ_pop2_lbl 021 `"Grade 5"', add
label define educ_pop2_lbl 022 `"Grade 6"', add
label define educ_pop2_lbl 030 `"Grades 7 or 8"', add
label define educ_pop2_lbl 031 `"Grade 7"', add
label define educ_pop2_lbl 032 `"Grade 8"', add
label define educ_pop2_lbl 040 `"Grade 9"', add
label define educ_pop2_lbl 050 `"Grade 10"', add
label define educ_pop2_lbl 060 `"Grade 11"', add
label define educ_pop2_lbl 070 `"Grade 12"', add
label define educ_pop2_lbl 071 `"12th grade, no diploma"', add
label define educ_pop2_lbl 072 `"12th grade, diploma unclear"', add
label define educ_pop2_lbl 073 `"High school diploma or equivalent"', add
label define educ_pop2_lbl 080 `"1 year of college"', add
label define educ_pop2_lbl 081 `"Some college but no degree"', add
label define educ_pop2_lbl 090 `"2 years of college"', add
label define educ_pop2_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_pop2_lbl 092 `"Associate's degree, academic program"', add
label define educ_pop2_lbl 100 `"3 years of college"', add
label define educ_pop2_lbl 110 `"4 years of college"', add
label define educ_pop2_lbl 111 `"Bachelor's degree"', add
label define educ_pop2_lbl 120 `"5+ years of college"', add
label define educ_pop2_lbl 121 `"5 years of college"', add
label define educ_pop2_lbl 122 `"6+ years of college"', add
label define educ_pop2_lbl 123 `"Master's degree"', add
label define educ_pop2_lbl 124 `"Professional school degree"', add
label define educ_pop2_lbl 125 `"Doctorate degree"', add
label define educ_pop2_lbl 999 `"Missing/Unknown"', add
label values educ_pop2 educ_pop2_lbl

label define educ_sp_lbl 000 `"NIU or no schooling"'
label define educ_sp_lbl 001 `"NIU or blank"', add
label define educ_sp_lbl 002 `"None or preschool"', add
label define educ_sp_lbl 010 `"Grades 1, 2, 3, or 4"', add
label define educ_sp_lbl 011 `"Grade 1"', add
label define educ_sp_lbl 012 `"Grade 2"', add
label define educ_sp_lbl 013 `"Grade 3"', add
label define educ_sp_lbl 014 `"Grade 4"', add
label define educ_sp_lbl 020 `"Grades 5 or 6"', add
label define educ_sp_lbl 021 `"Grade 5"', add
label define educ_sp_lbl 022 `"Grade 6"', add
label define educ_sp_lbl 030 `"Grades 7 or 8"', add
label define educ_sp_lbl 031 `"Grade 7"', add
label define educ_sp_lbl 032 `"Grade 8"', add
label define educ_sp_lbl 040 `"Grade 9"', add
label define educ_sp_lbl 050 `"Grade 10"', add
label define educ_sp_lbl 060 `"Grade 11"', add
label define educ_sp_lbl 070 `"Grade 12"', add
label define educ_sp_lbl 071 `"12th grade, no diploma"', add
label define educ_sp_lbl 072 `"12th grade, diploma unclear"', add
label define educ_sp_lbl 073 `"High school diploma or equivalent"', add
label define educ_sp_lbl 080 `"1 year of college"', add
label define educ_sp_lbl 081 `"Some college but no degree"', add
label define educ_sp_lbl 090 `"2 years of college"', add
label define educ_sp_lbl 091 `"Associate's degree, occupational/vocational program"', add
label define educ_sp_lbl 092 `"Associate's degree, academic program"', add
label define educ_sp_lbl 100 `"3 years of college"', add
label define educ_sp_lbl 110 `"4 years of college"', add
label define educ_sp_lbl 111 `"Bachelor's degree"', add
label define educ_sp_lbl 120 `"5+ years of college"', add
label define educ_sp_lbl 121 `"5 years of college"', add
label define educ_sp_lbl 122 `"6+ years of college"', add
label define educ_sp_lbl 123 `"Master's degree"', add
label define educ_sp_lbl 124 `"Professional school degree"', add
label define educ_sp_lbl 125 `"Doctorate degree"', add
label define educ_sp_lbl 999 `"Missing/Unknown"', add
label values educ_sp educ_sp_lbl

label define educ99_head_lbl 00 `"NIU"'
label define educ99_head_lbl 01 `"No school completed"', add
label define educ99_head_lbl 04 `"1st-4th grade"', add
label define educ99_head_lbl 05 `"5th-8th grade"', add
label define educ99_head_lbl 06 `"9th grade"', add
label define educ99_head_lbl 07 `"10th grade"', add
label define educ99_head_lbl 08 `"11th grade"', add
label define educ99_head_lbl 09 `"12th grade, no diploma"', add
label define educ99_head_lbl 10 `"High school graduate, or GED"', add
label define educ99_head_lbl 11 `"Some college, no degree"', add
label define educ99_head_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_head_lbl 13 `"Associate degree, occupational program"', add
label define educ99_head_lbl 14 `"Associate degree, academic program"', add
label define educ99_head_lbl 15 `"Bachelors degree"', add
label define educ99_head_lbl 16 `"Masters degree"', add
label define educ99_head_lbl 17 `"Professional degree"', add
label define educ99_head_lbl 18 `"Doctorate degree"', add
label values educ99_head educ99_head_lbl

label define educ99_mom_lbl 00 `"NIU"'
label define educ99_mom_lbl 01 `"No school completed"', add
label define educ99_mom_lbl 04 `"1st-4th grade"', add
label define educ99_mom_lbl 05 `"5th-8th grade"', add
label define educ99_mom_lbl 06 `"9th grade"', add
label define educ99_mom_lbl 07 `"10th grade"', add
label define educ99_mom_lbl 08 `"11th grade"', add
label define educ99_mom_lbl 09 `"12th grade, no diploma"', add
label define educ99_mom_lbl 10 `"High school graduate, or GED"', add
label define educ99_mom_lbl 11 `"Some college, no degree"', add
label define educ99_mom_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_mom_lbl 13 `"Associate degree, occupational program"', add
label define educ99_mom_lbl 14 `"Associate degree, academic program"', add
label define educ99_mom_lbl 15 `"Bachelors degree"', add
label define educ99_mom_lbl 16 `"Masters degree"', add
label define educ99_mom_lbl 17 `"Professional degree"', add
label define educ99_mom_lbl 18 `"Doctorate degree"', add
label values educ99_mom educ99_mom_lbl

label define educ99_mom2_lbl 00 `"NIU"'
label define educ99_mom2_lbl 01 `"No school completed"', add
label define educ99_mom2_lbl 04 `"1st-4th grade"', add
label define educ99_mom2_lbl 05 `"5th-8th grade"', add
label define educ99_mom2_lbl 06 `"9th grade"', add
label define educ99_mom2_lbl 07 `"10th grade"', add
label define educ99_mom2_lbl 08 `"11th grade"', add
label define educ99_mom2_lbl 09 `"12th grade, no diploma"', add
label define educ99_mom2_lbl 10 `"High school graduate, or GED"', add
label define educ99_mom2_lbl 11 `"Some college, no degree"', add
label define educ99_mom2_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_mom2_lbl 13 `"Associate degree, occupational program"', add
label define educ99_mom2_lbl 14 `"Associate degree, academic program"', add
label define educ99_mom2_lbl 15 `"Bachelors degree"', add
label define educ99_mom2_lbl 16 `"Masters degree"', add
label define educ99_mom2_lbl 17 `"Professional degree"', add
label define educ99_mom2_lbl 18 `"Doctorate degree"', add
label values educ99_mom2 educ99_mom2_lbl

label define educ99_pop_lbl 00 `"NIU"'
label define educ99_pop_lbl 01 `"No school completed"', add
label define educ99_pop_lbl 04 `"1st-4th grade"', add
label define educ99_pop_lbl 05 `"5th-8th grade"', add
label define educ99_pop_lbl 06 `"9th grade"', add
label define educ99_pop_lbl 07 `"10th grade"', add
label define educ99_pop_lbl 08 `"11th grade"', add
label define educ99_pop_lbl 09 `"12th grade, no diploma"', add
label define educ99_pop_lbl 10 `"High school graduate, or GED"', add
label define educ99_pop_lbl 11 `"Some college, no degree"', add
label define educ99_pop_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_pop_lbl 13 `"Associate degree, occupational program"', add
label define educ99_pop_lbl 14 `"Associate degree, academic program"', add
label define educ99_pop_lbl 15 `"Bachelors degree"', add
label define educ99_pop_lbl 16 `"Masters degree"', add
label define educ99_pop_lbl 17 `"Professional degree"', add
label define educ99_pop_lbl 18 `"Doctorate degree"', add
label values educ99_pop educ99_pop_lbl

label define educ99_pop2_lbl 00 `"NIU"'
label define educ99_pop2_lbl 01 `"No school completed"', add
label define educ99_pop2_lbl 04 `"1st-4th grade"', add
label define educ99_pop2_lbl 05 `"5th-8th grade"', add
label define educ99_pop2_lbl 06 `"9th grade"', add
label define educ99_pop2_lbl 07 `"10th grade"', add
label define educ99_pop2_lbl 08 `"11th grade"', add
label define educ99_pop2_lbl 09 `"12th grade, no diploma"', add
label define educ99_pop2_lbl 10 `"High school graduate, or GED"', add
label define educ99_pop2_lbl 11 `"Some college, no degree"', add
label define educ99_pop2_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_pop2_lbl 13 `"Associate degree, occupational program"', add
label define educ99_pop2_lbl 14 `"Associate degree, academic program"', add
label define educ99_pop2_lbl 15 `"Bachelors degree"', add
label define educ99_pop2_lbl 16 `"Masters degree"', add
label define educ99_pop2_lbl 17 `"Professional degree"', add
label define educ99_pop2_lbl 18 `"Doctorate degree"', add
label values educ99_pop2 educ99_pop2_lbl

label define educ99_sp_lbl 00 `"NIU"'
label define educ99_sp_lbl 01 `"No school completed"', add
label define educ99_sp_lbl 04 `"1st-4th grade"', add
label define educ99_sp_lbl 05 `"5th-8th grade"', add
label define educ99_sp_lbl 06 `"9th grade"', add
label define educ99_sp_lbl 07 `"10th grade"', add
label define educ99_sp_lbl 08 `"11th grade"', add
label define educ99_sp_lbl 09 `"12th grade, no diploma"', add
label define educ99_sp_lbl 10 `"High school graduate, or GED"', add
label define educ99_sp_lbl 11 `"Some college, no degree"', add
label define educ99_sp_lbl 12 `"Associate degree, type of program not specified"', add
label define educ99_sp_lbl 13 `"Associate degree, occupational program"', add
label define educ99_sp_lbl 14 `"Associate degree, academic program"', add
label define educ99_sp_lbl 15 `"Bachelors degree"', add
label define educ99_sp_lbl 16 `"Masters degree"', add
label define educ99_sp_lbl 17 `"Professional degree"', add
label define educ99_sp_lbl 18 `"Doctorate degree"', add
label values educ99_sp educ99_sp_lbl

