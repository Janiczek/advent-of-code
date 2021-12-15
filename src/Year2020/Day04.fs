module AoC.Day04

open FSharpPlus

let exampleInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

let realInput = "hgt:176cm
iyr:2013
hcl:#fffffd ecl:amb
byr:2000
eyr:2034
cid:89 pid:934693255

hcl:#b5c3db ecl:grn hgt:155cm pid:#baec97 iyr:2017
byr:1939
eyr:2020

pid:526669252 eyr:1972
hgt:152cm ecl:dne byr:1960 hcl:z iyr:2023

eyr:2028 hcl:#c0946f hgt:73in byr:1926 ecl:#473aaf iyr:2016 pid:565318180

pid:472686027 ecl:oth iyr:2019
cid:277 byr:1940
eyr:2030 hgt:170cm
hcl:#62e117

ecl:oth
iyr:2017
pid:938461813 hcl:#733820 byr:1959 hgt:159cm eyr:2022

iyr:2011 eyr:2021 hcl:z
ecl:hzl byr:2002 pid:17324328 cid:140
hgt:186cm

byr:2022 pid:3164234967 iyr:1984
hgt:76cm hcl:6b3837
ecl:#fa362b
eyr:2037

hcl:z eyr:1945
pid:9247286687 hgt:75cm
iyr:1934 cid:326 ecl:zzz
byr:2005

byr:2005
ecl:lzr
eyr:2021 pid:152cm
cid:254 iyr:2020 hcl:z hgt:157cm

iyr:2020 eyr:2020 hcl:#18171d ecl:gry pid:914128753 hgt:168cm
byr:2002

hcl:#7d3b0c hgt:160cm eyr:2020 iyr:2015
pid:054067854 ecl:brn byr:2023

hcl:#cfa07d hgt:157cm
byr:1994 eyr:2027 pid:344443856
iyr:2016

pid:762423097
iyr:2014 hcl:#a97842 ecl:brn hgt:180cm byr:1927 eyr:2021

pid:6645616064 hcl:#ceb3a1 byr:2030
eyr:2032 hgt:158cm iyr:2012
ecl:#e9619e

eyr:2022
ecl:brn
byr:1986
hgt:161cm cid:99 pid:288726584 hcl:#6b5442 iyr:2019

cid:75
pid:117771843
hgt:184cm byr:1937 ecl:brn
hcl:#d88fd9
iyr:2015 eyr:2027

iyr:2016 hcl:#fffffd hgt:170cm eyr:2022 ecl:oth pid:629454113
byr:1952

hcl:#c0946f iyr:2018 hgt:189cm
byr:1971 ecl:oth eyr:2029
pid:800207810

eyr:2022 hcl:#7d3b0c pid:969986413
byr:1978 iyr:2020 hgt:186cm
ecl:gry

hgt:171cm byr:1949 hcl:#341e13
ecl:amb eyr:2030 pid:359107274 iyr:2013

pid:839751525 eyr:2024 byr:1921
iyr:2012 ecl:amb hcl:#b0ed6f hgt:154cm

pid:32592758
byr:2009
hgt:107 iyr:2019 hcl:#866857
eyr:2036 ecl:amb

eyr:2040 hcl:#733820 cid:199
byr:2027
pid:7791792988 ecl:blu iyr:2026
hgt:63cm

iyr:2011 cid:119 pid:344693475
ecl:grn hgt:160cm eyr:2029 hcl:#346973 byr:1996

hgt:161in byr:2025 cid:167 iyr:2024 eyr:2040 pid:034804648
hcl:#efcc98 ecl:oth

ecl:#ba14f0 iyr:1935
hgt:60cm
byr:2003 eyr:1987
hcl:8e509b pid:161cm

iyr:2018 pid:620508652 ecl:amb eyr:2023 hgt:183cm hcl:#a97842
byr:1967 cid:117

eyr:2022 ecl:amb
pid:476049089 iyr:2012
hgt:165cm
byr:1955 hcl:#602927

byr:2014 hcl:z iyr:2029 cid:279 pid:28914607 hgt:75cm ecl:xry

hgt:156cm eyr:2023 iyr:2011 ecl:oth hcl:#7d3b0c pid:561313217 byr:1952

iyr:2011 byr:1935
hcl:#cfa07d ecl:oth pid:830614209
eyr:2028 hgt:173cm

iyr:2012 cid:210 eyr:2022
pid:652810130 hcl:#18171d ecl:grn byr:1960 hgt:152cm

eyr:2026 pid:815848563 hgt:75in iyr:2019 ecl:gry byr:1947
hcl:#cfa07d

cid:181 iyr:2012
eyr:2024 byr:1934 hcl:#c0946f
hgt:165cm ecl:oth pid:232944581

cid:135 iyr:2020
byr:1971 hcl:#733820 pid:531877857 hgt:179cm eyr:2027 ecl:amb

byr:1987 hcl:936807 eyr:2032 ecl:#4bec4a pid:605628619 cid:180 hgt:150in
iyr:2015

hcl:b62ef0 ecl:#092141
pid:876635399 byr:1944 hgt:158cm iyr:2017 eyr:1924

iyr:2016 pid:7039815301 byr:2014 hgt:150 eyr:2032 ecl:blu hcl:z

byr:1979 eyr:2030 iyr:1978 hgt:63 pid:1554613758 hcl:z ecl:amb

hgt:70cm hcl:e45897 iyr:2020 eyr:1977 ecl:dne pid:2878189427 byr:1973

iyr:2003
hcl:#cfa07d
pid:260517078
byr:2030 hgt:175cm eyr:2020
ecl:brn

pid:460604681 eyr:2022
cid:138 iyr:2016 hgt:163cm
byr:1922
hcl:#ceb3a1 ecl:oth

hgt:167cm byr:2009 eyr:1975 cid:295 pid:174cm iyr:2029
hcl:z

hgt:67in ecl:grn
eyr:2023
cid:122 pid:281246917 byr:1990 iyr:2011 hcl:#866857

ecl:#ed7ddc byr:1922 cid:234 hcl:e61b1e iyr:1932 eyr:1996 pid:31344005 hgt:62cm

byr:1949
cid:275 iyr:2017 ecl:grn
hgt:164cm eyr:2027 hcl:#18171d
pid:751342937

ecl:blu hgt:162cm
pid:432600613 byr:1923 eyr:2029 iyr:2011 hcl:#623a2f cid:315

iyr:2020
hcl:#b2bb11 pid:055891584 ecl:grn
hgt:67in
eyr:2029 byr:1937

iyr:2012
hcl:#a97842 pid:325640714 ecl:blu hgt:185cm eyr:2024 byr:1971

hcl:#b6652a pid:485327267
ecl:brn hgt:155cm eyr:2028
iyr:2019

pid:902164867 hgt:77 cid:283 eyr:2027
iyr:2020 ecl:hzl byr:1935 hcl:#efcc98

ecl:grn
hcl:#ceb3a1 byr:1977 hgt:165cm
pid:850700221 eyr:2030
iyr:2012

byr:1989 ecl:brn eyr:2026 pid:919138357 iyr:2016
hcl:#623a2f cid:319 hgt:161cm

iyr:2017
byr:1973 pid:293382118 hcl:#341e13 cid:143 ecl:hzl
hgt:166cm eyr:2022

pid:517102798
hcl:f9d9dd
eyr:1933 iyr:2019 hgt:164cm
byr:2017 ecl:utc

eyr:2023 pid:757868802 hcl:#18171d cid:244
hgt:156cm
ecl:blu iyr:2015 byr:1926

eyr:2022
iyr:2020
hgt:158cm ecl:grn
byr:1988
pid:979194751 hcl:#888785

eyr:2039
pid:3867868142 byr:1936 ecl:dne iyr:2022 hcl:4b43b8
hgt:115 cid:241

iyr:2015 eyr:2026
hcl:#ceb3a1 pid:539099924
cid:234
ecl:brn
byr:1920 hgt:163cm

cid:259 iyr:2020
pid:949453818 eyr:2022 hgt:181cm
byr:1997 ecl:blu hcl:#18171d

byr:2016
iyr:2012
ecl:utc
hgt:68in eyr:1993
pid:1542134802 hcl:486699
cid:239

iyr:2018
hgt:154cm ecl:brn byr:1970
eyr:2021 pid:581775861 hcl:#888785

iyr:2012
eyr:2027 hgt:67cm hcl:#efcc98 ecl:zzz pid:312104916 byr:2020

hcl:#b6652a ecl:hzl eyr:2023 iyr:2012 pid:513268492
hgt:159cm

hgt:162in hcl:z
byr:2029
eyr:2023 ecl:#e2e7ab iyr:2016 pid:65979982

cid:84 hgt:71in ecl:blu pid:982719716
eyr:2020 iyr:2014

eyr:2028 hgt:181cm
ecl:hzl pid:255796693 hcl:#341e13 byr:1994 iyr:2011 cid:218

ecl:blu
byr:2029 iyr:2017 pid:468504566 eyr:2020 hcl:z hgt:163cm

hgt:158cm
eyr:2025 ecl:hzl cid:295 pid:601339484
hcl:#7d3b0c byr:1991 iyr:2013

eyr:2028
iyr:2018 pid:2236240873
hgt:172cm
ecl:#0e337e hcl:#b6652a cid:108 byr:1930

ecl:gry hcl:#888785
eyr:2020 pid:442479017 iyr:2016

iyr:2014 ecl:grn
cid:313 eyr:2023
hgt:183cm
byr:1976
pid:499580308 hcl:#53efe6

eyr:2034
cid:235 hcl:8f3cf5
byr:2027
hgt:161in pid:3259965094 ecl:xry iyr:2026

eyr:1978 byr:1925 iyr:2018 hgt:170cm ecl:#0c94e8
pid:562699017 hcl:#816949

eyr:2023 hcl:#866857 hgt:179cm
pid:785862442 iyr:2014 cid:165 ecl:amb byr:1939

hgt:187cm
pid:64469711 ecl:gry eyr:2023 cid:225 hcl:#341e13 iyr:2011 byr:1958

hgt:162cm byr:2028 ecl:#37e345
eyr:2037 hcl:19fb3d
iyr:2021
pid:#87921a

eyr:2027 hcl:#18171d
byr:2002 ecl:gry iyr:2014
pid:561506850 hgt:177cm

hgt:64cm pid:#a92686
eyr:2029 cid:122
byr:2026
iyr:2017 hcl:z ecl:grn

eyr:2028 byr:2007 hgt:155cm ecl:#86fa1b hcl:#733820 pid:562889497
iyr:2019

pid:880698787
byr:1992
hcl:#7d3b0c hgt:163cm ecl:hzl
iyr:2011 eyr:2021

eyr:2020 byr:1994 iyr:2011 hgt:186cm pid:841855425 hcl:#cfa07d ecl:gry

byr:1923 iyr:2015 ecl:amb pid:414655744
hcl:#b6652a
hgt:159cm
eyr:2026

hgt:171cm ecl:amb pid:363065723 iyr:2020
cid:66 hcl:#b6652a eyr:2021
byr:1960

eyr:2002
hcl:2627b2 ecl:#1bf21d pid:168cm byr:2024 iyr:2020
hgt:186in

iyr:2011 byr:1924 eyr:2024
hcl:#b6652a ecl:brn
pid:794477411 hgt:162in

hcl:z hgt:67cm
byr:2025
pid:582569979
iyr:2013
ecl:oth eyr:2025

cid:50 hcl:931e2c
hgt:172in eyr:1994 iyr:2023
ecl:#cd2204
byr:2015
pid:157cm

hgt:173cm eyr:2028
ecl:amb pid:569607283
byr:1942
iyr:2019
cid:228
hcl:#866857

cid:109
ecl:oth eyr:1933 byr:1982 pid:173cm hcl:#b6652a hgt:174cm
iyr:2023

cid:69 hcl:#9ad05b pid:341135641
byr:1968 ecl:brn
iyr:2012 hgt:156cm
eyr:2020

hgt:176cm
byr:1954 ecl:blu
eyr:2020
pid:478462637 iyr:2019
hcl:#888785

iyr:2026 hgt:193in
byr:2018 pid:162cm hcl:605e7f eyr:1948 ecl:utc

byr:1962
eyr:2022 pid:445346117 iyr:2019 hgt:158cm hcl:#623a2f ecl:hzl

cid:278 hgt:187cm eyr:2024 iyr:2016 byr:1964
ecl:grn pid:450739552 hcl:#733820

ecl:grn byr:2000 eyr:2023
pid:344489911 hcl:#7d3b0c iyr:2011 hgt:177cm

iyr:2015 hgt:180cm cid:190 hcl:#a97842 pid:359774842 eyr:2029 byr:2002 ecl:amb

eyr:2027 iyr:2015 ecl:hzl
pid:082733109
byr:1975 hgt:191cm cid:251 hcl:#888785

hcl:#c0946f iyr:2015
hgt:167cm byr:1990 ecl:amb pid:168cm eyr:2023

ecl:gry eyr:2028
byr:1934 iyr:2013 hcl:#6b5442
pid:424412120 hgt:173cm

pid:273352568
eyr:2024
iyr:2013 byr:1926 hcl:#602927
ecl:brn hgt:180cm

hcl:#7d3b0c hgt:70in ecl:amb iyr:2019
byr:1937
eyr:2030 pid:309011548

ecl:grn
hgt:64in pid:796889811 hcl:#18171d
byr:1929 eyr:2027

ecl:amb hcl:#888785
pid:412449028 cid:316 byr:1982
iyr:2019 eyr:2030 hgt:193cm

eyr:1927
hcl:z hgt:158cm byr:1930
ecl:lzr iyr:2018
cid:197
pid:0906120002

ecl:grn byr:1970 hgt:181cm
pid:376212702 eyr:2030 iyr:2017 cid:266 hcl:#f8b0f5

iyr:2018 hgt:73in pid:652356158 hcl:#c0946f
ecl:grn byr:1973

cid:170 hcl:#b6652a byr:2011
ecl:gry iyr:2025 pid:#b6e567 hgt:67cm eyr:2016

hgt:192cm ecl:amb eyr:2026 pid:201824712 hcl:#888785 byr:1966 iyr:2019

iyr:2013 byr:1995 eyr:2028 hcl:#b6652a ecl:brn cid:53 pid:705606447 hgt:176cm

hcl:#341e13 byr:1951
hgt:161cm pid:231973770 iyr:2015 ecl:hzl
eyr:2030

cid:210 ecl:brn iyr:2017 eyr:2030
hgt:176cm hcl:#efcc98
byr:1965

eyr:2020 hcl:#7d3b0c
pid:872088079 ecl:oth iyr:2017 byr:1920
hgt:180cm

hcl:#0b540c iyr:2019
byr:1938
hgt:153cm ecl:gry pid:236785988
eyr:2020

eyr:2020 hgt:184cm iyr:2019
pid:673186642 ecl:oth byr:1977 hcl:#866857

eyr:2025
ecl:gry hcl:#341e13 byr:1970 iyr:2010 pid:972122542 hgt:184cm

ecl:grn byr:1992 hgt:71in
iyr:2014 cid:254 hcl:#fffffd pid:749733013
eyr:2026

cid:98 ecl:amb eyr:2022
hgt:169cm pid:022677680
byr:1937 iyr:2014 hcl:#e62c71

hgt:192cm
iyr:2015
eyr:2028 ecl:oth pid:6000619833 hcl:#c0946f
byr:1930

byr:1938 hcl:#efcc98 hgt:178cm iyr:1953 eyr:2038
ecl:brn pid:#cdc55a

hgt:66in byr:1951 iyr:2016 hcl:#18171d
eyr:2027
ecl:lzr pid:834188980

iyr:2012 eyr:2025
hcl:#7d3b0c pid:330325803 cid:166 hgt:186cm byr:1938
ecl:amb

iyr:2015 hcl:#602927 cid:268 eyr:2021
ecl:amb hgt:186cm pid:318676962

hcl:#3d6f3c iyr:2014 pid:665730784 cid:191 hgt:150cm byr:1981 ecl:oth eyr:2024

ecl:grn hcl:#733820
eyr:2028 iyr:2010
hgt:162cm byr:1944 pid:872962499

eyr:2028 byr:1974
ecl:brn
iyr:2010 hcl:#18171d hgt:160cm

hcl:#602927
byr:1959 eyr:2027 iyr:2016 ecl:brn hgt:169cm pid:078503025

hcl:#623a2f pid:326300051 hgt:153cm
byr:1973 iyr:2012
ecl:gry eyr:2026

hgt:151cm
byr:1966 eyr:2029 pid:026952622 hcl:#18171d ecl:gry iyr:2010

hcl:#7d3b0c byr:1974 pid:444713591 iyr:2017 eyr:2030
hgt:165cm ecl:oth

iyr:2026 pid:184cm
ecl:gmt hcl:z hgt:71cm
eyr:2029

cid:310 hcl:#fffffd byr:1998
pid:450705840 iyr:2015
ecl:grn eyr:2021 hgt:165cm

byr:1939 hcl:#623a2f ecl:gry hgt:69in pid:539812641 eyr:2027 iyr:2013

pid:207645014
iyr:2015
cid:314 ecl:oth
byr:1942
eyr:2027 hgt:186cm hcl:#fffffd

ecl:#fb7e3d eyr:2031 iyr:1956
hgt:188 pid:160cm hcl:z byr:2027

byr:1972 iyr:2020 eyr:2026 hcl:#b6652a pid:289088329 hgt:65in ecl:gry

eyr:2027
hgt:59cm
byr:2022
pid:938063769 ecl:zzz iyr:2028 hcl:23c762

byr:2004 hgt:74 iyr:2017
eyr:2040 ecl:blu pid:4611117799 cid:73 hcl:z

ecl:brn byr:1962 cid:321
iyr:2019 eyr:2026
hgt:159cm
hcl:#667310 pid:439864945

iyr:2026 eyr:2039 pid:633263851 cid:321 ecl:lzr hgt:166cm
byr:2023 hcl:fc3c63

byr:1961 iyr:2010 ecl:blu
eyr:2023 pid:245858010

hgt:193cm pid:821303249 eyr:2020 hcl:#6b5442 cid:130 byr:1946

eyr:2026 ecl:brn
hcl:#733820 byr:1983 hgt:182cm pid:727380954 cid:188 iyr:2015

hgt:152cm cid:206 iyr:2012 byr:1947 hcl:#888785 ecl:gry
pid:720312394 eyr:2023

hgt:150cm ecl:gry pid:863712648
iyr:2019 cid:349 byr:1976 hcl:#602927 eyr:2022

hgt:164in pid:953500867
eyr:2021
iyr:2014
hcl:z cid:343 ecl:amb

byr:1981 pid:529710230 iyr:2013 eyr:2023
hcl:#c0946f ecl:amb
hgt:151cm

pid:706204190 hgt:154cm cid:317
hcl:#602927 byr:1949 ecl:blu iyr:2010 eyr:2028

iyr:2019 hcl:#0219e6
pid:850093151 ecl:gry
eyr:2030
byr:1938 hgt:177cm

ecl:brn hcl:#efcc98 eyr:2029 byr:1963
hgt:185cm pid:611279647 iyr:2011

ecl:blu eyr:2022 byr:1941 hgt:167cm
iyr:2012 hcl:#7d3b0c pid:415739564
cid:193

eyr:2027 ecl:blu byr:1968 pid:479994566
hcl:#733820 hgt:151cm
iyr:2011

pid:263729839 hgt:189cm eyr:2030 ecl:gry byr:2001 hcl:#602927

byr:1985
ecl:amb pid:672663977 cid:139
hgt:159cm hcl:#733820 iyr:2018 eyr:2020

byr:1998
hcl:#cfa07d eyr:2023 pid:255046063 iyr:2011 ecl:blu hgt:177cm

ecl:oth
byr:1980 pid:253747166 eyr:2029
hcl:#6b5442 hgt:186cm

eyr:2030 hcl:#866857
hgt:165cm
ecl:amb
iyr:2017 pid:241240220 cid:164 byr:2001

byr:1994 hcl:#b6652a iyr:2015
pid:753831241
hgt:175cm
eyr:2027 ecl:blu

hcl:#b6652a pid:471594512
byr:1961 ecl:hzl hgt:175cm
iyr:2020 eyr:2025

byr:1987 pid:112366159
eyr:2028 hcl:22b2d7
hgt:64in cid:222
ecl:#b40dca iyr:2019

iyr:2015 hcl:e1ed55 hgt:160in ecl:utc byr:2015 eyr:2036

byr:1935
hcl:#7d3b0c hgt:152cm ecl:gry
pid:160090332 iyr:2020 eyr:2020

pid:552779024 byr:1998 hgt:185cm ecl:gry eyr:2026 iyr:2013 hcl:#d46cd6

ecl:oth pid:311860969
cid:57
hgt:60in
eyr:2026
hcl:#ceb3a1
byr:1961 iyr:2011

eyr:2021 hgt:162cm cid:240
pid:259997995
hcl:#efcc98
ecl:gry byr:1962 iyr:2017

hcl:#866857
iyr:2016
eyr:2029
ecl:blu byr:1927 cid:249 pid:665324615 hgt:65in

byr:1931
cid:331
hgt:66in
ecl:grn iyr:2020 hcl:#efcc98 eyr:2025 pid:175780921

hgt:98
eyr:2040 ecl:blu byr:2029
iyr:1967 hcl:0d76e9
pid:#c9053a cid:296

pid:370918950
hcl:#602927
byr:1938
hgt:178cm iyr:2018 eyr:2030
ecl:oth

hgt:185cm
eyr:1984 ecl:oth pid:851080398
hcl:z byr:2027 iyr:2017

pid:095078224 byr:1957 hcl:#45bcf4 ecl:#f643f9 hgt:63cm eyr:2036 iyr:1978

hcl:z
eyr:2023 ecl:oth hgt:162cm
iyr:2016 byr:1938 pid:#fdcddf

hcl:#341e13 iyr:2013 hgt:189cm
pid:982271041 ecl:brn
byr:1930 eyr:2030

eyr:2026
iyr:2012 hcl:#cfa07d cid:59 pid:105862717 ecl:blu
hgt:159cm byr:1943

ecl:hzl
pid:604172804 iyr:2016 hgt:174cm cid:79 eyr:2025
hcl:#733820 byr:1994

iyr:2011 pid:452628771 ecl:gry hgt:182cm hcl:#623a2f
eyr:2023
byr:1986

hcl:#341e13 iyr:2010 byr:1946 eyr:2021
cid:350 pid:049684494 hgt:180cm
ecl:grn

iyr:2020
hgt:173cm pid:384503937
byr:1986
hcl:#341e13
cid:113
eyr:2025 ecl:amb

hgt:180cm byr:1949
hcl:#733820 iyr:2010 eyr:2030
cid:123 pid:065609606 ecl:oth

iyr:2010 eyr:2028
pid:231750173
hgt:63in ecl:brn
byr:1948 hcl:#18171d

iyr:2020 hcl:#623a2f
ecl:gry
byr:1922 pid:961213634 eyr:2022 hgt:177cm

hcl:#18171d eyr:2020 iyr:2014 byr:1983
pid:183568344 hgt:72in
ecl:gry

eyr:2023 pid:102781246 ecl:brn
hcl:#888785 byr:1929 hgt:167cm iyr:2010

pid:362873066 byr:1994 hcl:#de545f iyr:2018 hgt:177cm ecl:blu cid:86
eyr:2024

hcl:842f2d iyr:1983
byr:1954 eyr:2037
ecl:lzr pid:3915492573 hgt:166cm

ecl:grn
hcl:#fffffd iyr:2014
hgt:173cm
byr:1939
pid:930650489
eyr:2025

eyr:2028 ecl:brn hcl:#7d3b0c hgt:166cm byr:1938 pid:992958531 iyr:2011

pid:101149939 eyr:2024 iyr:2018 hgt:165cm
ecl:hzl
hcl:#ceb3a1 cid:176

cid:62
pid:651390352 hcl:#efcc98
iyr:2018
eyr:2027
ecl:brn
hgt:66in byr:1953

hcl:#623a2f byr:1978
iyr:2013
hgt:180cm eyr:2027 ecl:amb pid:836425641

pid:557464096 hgt:155cm ecl:blu cid:142 byr:1936 iyr:2010
hcl:#cfa07d eyr:2027

ecl:gry iyr:2024 hcl:#341e13 pid:442593279 cid:314 hgt:186cm byr:1960
eyr:2022

cid:123 iyr:2014
byr:2000
pid:878733390 eyr:2021 ecl:hzl hgt:162cm

byr:1959 cid:259
pid:722895016
ecl:brn iyr:2018 eyr:2027 hgt:185cm

pid:163697814 ecl:hzl
iyr:2013 byr:1932
hgt:68in cid:286 eyr:2025 hcl:#efcc98

byr:1927
hgt:72cm ecl:oth
eyr:2021 hcl:#99e959
pid:669724466 iyr:2010

byr:1943 iyr:2011 eyr:2024 pid:384419879 ecl:hzl hcl:#7d3b0c hgt:170cm

pid:137944386 ecl:gry
byr:1953 hcl:#733820 iyr:2013 eyr:2025 hgt:184cm

iyr:2017 eyr:2023 pid:288078785
hgt:179cm
byr:1993 hcl:#602927 ecl:hzl

ecl:brn
hgt:187cm eyr:2024 byr:1971 iyr:2020 hcl:#b6652a pid:622975646
cid:290

pid:371817422 ecl:blu byr:1964
iyr:2018
eyr:2021 cid:176
hgt:153cm hcl:#888785

byr:2002
cid:256 iyr:2014 eyr:2024 ecl:blu hcl:#18171d hgt:187cm
pid:050022911

hgt:178cm pid:211144001 eyr:2027 iyr:2013
byr:1947
hcl:#7d3b0c ecl:grn

eyr:2025 ecl:blu pid:046417901 byr:1950
iyr:2015 hgt:165cm
hcl:#7d3b0c cid:128

ecl:hzl eyr:2029
iyr:2015
hgt:171cm hcl:#341e13
pid:561680375 byr:1997

byr:1948 iyr:2023 pid:17288381 hcl:6a34a3 ecl:#671ece eyr:2001
cid:152

eyr:2036 hgt:141 iyr:1957 byr:1987 hcl:z
pid:86986187 ecl:utc

eyr:2024 hcl:#b6652a iyr:2017 ecl:blu byr:1988 cid:348 hgt:152cm pid:068684272

iyr:2011 pid:989825275
cid:78 hcl:#341e13 byr:1983 ecl:blu hgt:158cm eyr:2020

ecl:grn hgt:187cm eyr:2027 iyr:2015
hcl:#866857 pid:240650427
byr:1940
cid:91

hcl:#888785 cid:185 byr:1925
hgt:155cm iyr:2015 ecl:blu eyr:2027 pid:851697441

iyr:2016 ecl:oth pid:056439470 byr:1985 eyr:2026
hgt:154cm hcl:#282539

ecl:hzl hcl:#a97842
iyr:1944
pid:118846711 eyr:2026 byr:1922 hgt:185cm

iyr:2020 hcl:#733820
pid:854531642 hgt:165cm
ecl:hzl eyr:2022

iyr:2014
byr:1957 hcl:#7fa674 hgt:189cm
eyr:2023 pid:740871941 ecl:brn

ecl:amb cid:349 hgt:170cm
byr:1952 hcl:#ceb3a1 iyr:2020
eyr:2026
pid:730499325

eyr:2027 ecl:amb
byr:1975 pid:985687961
hcl:z hgt:157cm
iyr:2013
cid:133

ecl:blu
hgt:193cm iyr:2015 hcl:#10f2ba byr:1989 pid:939704495 eyr:2021

ecl:oth eyr:2025 hgt:69in iyr:2014 cid:258 pid:477970733 byr:1984 hcl:#b6652a

hcl:z byr:2013
ecl:zzz
pid:1904741884 hgt:180 cid:138 eyr:1985 iyr:1935

eyr:2025
iyr:2026 hgt:190in pid:#43ca33
ecl:#3e1ef1 hcl:#7d3b0c byr:2030

eyr:2029 hgt:191cm
byr:1986 hcl:#ceb3a1 cid:327 pid:795060714 iyr:2012 ecl:hzl

eyr:2025 iyr:2017 ecl:grn
hcl:z
pid:8886398 hgt:174cm byr:2016

hcl:#a97842
eyr:2021 ecl:grn iyr:2013 pid:565234133 byr:1998
hgt:161cm

eyr:2029 hgt:163cm byr:1933 cid:86 iyr:2011
ecl:grn
hcl:#fffffd
pid:818769307

hgt:190cm eyr:2030 hcl:#af5b5d iyr:2011 ecl:brn pid:359524299 byr:1969

ecl:amb iyr:2011 eyr:2022
cid:141
byr:1978 hgt:69in hcl:#fffffd pid:013006109

ecl:blu hgt:164cm iyr:2019 eyr:2027 pid:899103430 hcl:#cfa07d
byr:1976

eyr:1938
ecl:#a03c41 pid:708735698
iyr:2030
hgt:184cm hcl:#b6652a byr:2013

ecl:hzl byr:1997 hcl:#a97842 cid:60 pid:172cm
eyr:2023 hgt:161in iyr:1936

ecl:hzl
byr:1938 pid:094889181
hgt:162cm iyr:2020
eyr:2028
hcl:#623a2f

hgt:162cm cid:86
hcl:#623a2f pid:738174580 ecl:brn byr:1980 eyr:2028 iyr:2014

byr:2007 hgt:150in hcl:z
eyr:2032
ecl:#114f3b
iyr:2030 pid:5129772

ecl:hzl iyr:2017
hcl:#18171d
pid:696283412 byr:1976 hgt:168cm
eyr:2028

eyr:1922 ecl:#84b0d4 byr:2013 hcl:#ceb3a1 pid:150cm iyr:2030
hgt:71cm

hgt:164cm byr:1949 ecl:gry eyr:2026
hcl:#623a2f

ecl:oth
iyr:2013 hgt:166cm hcl:#50e385
pid:478852286
eyr:2030 byr:1930

cid:129
iyr:2020 byr:1978 pid:907580992 eyr:1955
hcl:#602927
ecl:grn hgt:165cm

ecl:blu iyr:2018 byr:1953
hgt:177cm pid:126681706 eyr:2025 hcl:#c0946f

byr:1984 pid:275799917
ecl:oth hcl:#623a2f cid:348 iyr:2020
hgt:189cm eyr:2024

iyr:2016
ecl:hzl byr:1954
hcl:#623a2f pid:810508839 eyr:2026
hgt:185cm

byr:1967
eyr:2021 hcl:#ceb3a1
pid:406634908 hgt:158cm iyr:2018 ecl:hzl

iyr:2019 eyr:2030 pid:995681076 hcl:#341e13
cid:101 hgt:162cm ecl:blu byr:1925

eyr:2026 pid:272513479 hcl:#b6652a byr:1973 iyr:2016 ecl:amb hgt:182cm

pid:298704871 eyr:2024 hcl:#efcc98 byr:1959
iyr:2014 hgt:191cm ecl:grn

hgt:193cm pid:750729809 ecl:oth
cid:324
iyr:2011 hcl:#efcc98 byr:1954 eyr:2020

byr:1966 iyr:2019 eyr:2025 ecl:#2df4b6
hgt:184cm pid:#fc17f4 cid:161 hcl:#602927

byr:1955 hcl:299464 ecl:amb
hgt:157cm iyr:2017 eyr:2021
pid:239450987

hgt:172cm
ecl:hzl
pid:839804598
hcl:#341e13 eyr:2030 byr:1964 iyr:2013

iyr:2018 hgt:152cm byr:1948 hcl:#623a2f pid:400121515
ecl:blu
eyr:2020

cid:296
ecl:grn
byr:1960 iyr:2028 pid:#1f4b95 eyr:2033 hcl:#602927
hgt:66cm

iyr:1933 ecl:#232e20 pid:#d03ca7
eyr:2030 hcl:598ed6 hgt:154in byr:2011

cid:247 ecl:grn iyr:2014
hgt:178cm
byr:1992 hcl:#602927 eyr:2021
pid:678964478

iyr:2010 pid:623705680
ecl:hzl hgt:181cm byr:1980 hcl:#341e13 eyr:2028"

let neededKeys =
    Set.ofList [ "byr"
                 "iyr"
                 "eyr"
                 "hgt"
                 "hcl"
                 "ecl"
                 "pid" (*; "cid"*)  ]

let parse (input: string): Map<string,string> list =
    input.Split "\n\n"
    |> Array.map (fun passport ->
        (passport.Split "\n"
         |> seq
         |> Seq.toList
         |> String.concat " ").Split " "
        |> Array.map (fun field ->
            match field.Split ":" with
            | [| key; value |] -> (key, value)
            | _ -> failwith "bad field?")
        |> Map.ofArray)
    |> Array.toList

let requiredKeysArePresent (keys: Set<string>): bool =
    Set.difference neededKeys keys
    |> Set.isEmpty

let fieldValuesAreOk (passport: Map<string,string>): bool =
    let byr = int32 passport.["byr"]
    let iyr = int32 passport.["iyr"]
    let eyr = int32 passport.["eyr"]
    
    let hgtRaw = passport.["hgt"]
    let hgtLength = hgtRaw.Length
    let hgtNum = if hgtLength < 3 then "0" else hgtRaw.[..hgtLength-3]
    let hgtUnit = hgtRaw.[hgtLength-2..]
    let hgt = int32 hgtNum
    
    let hcl = passport.["hcl"]
    let hclHex = hcl.[1..]
    
    let ecl = passport.["ecl"]
    let pid = passport.["pid"]
    
    List.forall id
        [ byr >= 1920 && byr <= 2002
          iyr >= 2010 && iyr <= 2020
          eyr >= 2020 && eyr <= 2030
          
          if hgtUnit = "cm" then
              hgt >= 150 && hgt <= 193
          else
              hgt >= 59 && hgt <= 76
              
          hcl.[0] = '#'
          String.forall
              (fun c -> System.Char.IsDigit c || (int c >= int 'a' && int c <= int 'f'))
              hclHex
          
          Set.contains ecl (Set.ofList ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"])
          
          String.length pid = 9
          String.forall System.Char.IsDigit pid
        ]
    

let part1 (input: string): int =
    parse input
    |> List.map (Map.keys >> Set.ofSeq)
    |> List.filter requiredKeysArePresent
    |> List.length

let part2 (input: string): int =
    parse input
    |> List.filter (Map.keys >> Set.ofSeq >> requiredKeysArePresent)
    |> List.filter fieldValuesAreOk
    |> List.length

let main = part2 realInput