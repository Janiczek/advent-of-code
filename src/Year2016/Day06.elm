module Year2016.Day06 exposing (..)

import Advent exposing (Test)
import Dict exposing (Dict)


main : Program Never ( Output, Output ) Never
main =
    Advent.program
        { input = input
        , parse1 = parse
        , parse2 = parse
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }


type alias Input =
    List String


type alias Output =
    String


parse : String -> Input
parse input =
    input
        |> String.lines


compute1 : Input -> Output
compute1 input =
    let
        length =
            input
                |> List.head
                |> Advent.unsafeMaybe
                |> String.length
    in
        List.range 0 (length - 1)
            |> List.map (mostFrequentLetter input)
            |> String.join ""


mostFrequentLetter : List String -> Int -> String
mostFrequentLetter words index =
    words
        |> List.map (String.slice index (index + 1))
        |> frequencies
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> Advent.unsafeMaybe
        |> Tuple.first


frequencies : List comparable -> Dict comparable Int
frequencies list =
    list
        |> List.foldl
            (\el counter ->
                Dict.get el counter
                    |> Maybe.withDefault 0
                    |> (\count -> count + 1)
                    |> (\count -> Dict.insert el count counter)
            )
            Dict.empty


compute2 : Input -> Output
compute2 input =
    let
        length =
            input
                |> List.head
                |> Advent.unsafeMaybe
                |> String.length
    in
        List.range 0 (length - 1)
            |> List.map (leastFrequentLetter input)
            |> String.join ""


leastFrequentLetter : List String -> Int -> String
leastFrequentLetter words index =
    words
        |> List.map (String.slice index (index + 1))
        |> frequencies
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.head
        |> Advent.unsafeMaybe
        |> Tuple.first


tests1 : List (Test Input Output)
tests1 =
    [ Test "example"
        """eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"""
        [ "eedadn"
        , "drvtee"
        , "eandsr"
        , "raavrd"
        , "atevrs"
        , "tsrnev"
        , "sdttsa"
        , "rasrtv"
        , "nssdts"
        , "ntnada"
        , "svetve"
        , "tesnvt"
        , "vntsnd"
        , "vrdear"
        , "dvrsen"
        , "enarar"
        ]
        "easter"
    ]


tests2 : List (Test Input Output)
tests2 =
    [ Test "example"
        """eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar"""
        [ "eedadn"
        , "drvtee"
        , "eandsr"
        , "raavrd"
        , "atevrs"
        , "tsrnev"
        , "sdttsa"
        , "rasrtv"
        , "nssdts"
        , "ntnada"
        , "svetve"
        , "tesnvt"
        , "vntsnd"
        , "vrdear"
        , "dvrsen"
        , "enarar"
        ]
        "advent"
    ]


input : String
input =
    """nokeeods
zpmwxpca
jwyhcgzu
stsjmxlq
tehbxqaz
aibnhjna
hglamqmh
rdtbnzgs
iswfycts
bfqvddev
bobrpwzu
ttaijpwn
zsqbxduy
nfxtssou
zakahunf
cucgtuak
ccxstpbr
idvlghrf
ziiyfizd
ilnqkjhr
pspgmvbx
krepxvkh
txnxmsct
eyioumyz
zjfigmii
mjgvdqpx
lzxbjlfz
ytwaguep
wvsifimw
iravmwye
ulndcvsp
nhvpycug
sysylvjj
lpbispqw
eatsmkkl
zuzogruk
dutwoeei
uuogwtao
xlgydojn
byijebmd
lwaxnwgl
yarkhhou
legvqzwd
aowbehqa
mgcnqrao
dzvefekx
nxwtrmsz
muqogaqb
avjyxjuv
sddxjazw
pliriuin
kvbfyxne
fugdyleg
fnxfqrus
qigvixzx
dynhnjfg
kkfacelo
uyutelaf
oteuhuho
howayjbp
nvjimapz
pgruehty
bznxnagk
ackuwwhy
rukhmyzm
xrbfzppf
bmyowesb
eoeevbqq
otqcpxyk
ajebqtop
rvowtdym
tzelxzgg
ozbarteu
qwfmncdd
wjilseki
oexylqve
lbtgnuse
exjuxvtj
ghorhwwe
fqntodqj
faajlwjc
sdaklrsc
vodzbmno
fihrwktu
bzttvcpf
fxxqrnut
plybfmbo
utvvstkq
fiszzikb
tdubqjwe
eaqqhwks
psrzodsk
yenxefmy
xxxnooon
weahqixk
ccrqcwhw
ykecamuc
bcvtssvy
hiatsyta
xtgwhark
tnortovc
kjtmoree
fhrlfwbv
gqobfrvu
sxilzikm
xqahtoem
xxbtbbqp
pmipprgs
dhvhhfzv
ofiglbzk
vunznoab
qkevvjrw
prjestmx
nngivkxb
pvbodgyq
qqllzygh
xmtkadjw
ktbulfqx
dfndvlts
vcxfyxdt
vabmpvrh
kykgsrwg
rngsmulq
yewnysbr
edguzqvd
ehmdsxdg
qgqtbynt
mwyydesw
jgcnhyot
svkzvqsi
tfsdkugy
hohcojbu
nvduapdp
bmtckivn
lkxwtsjv
sjyvxhnn
ldhzxsks
sfvhpjui
uncrecum
xcatthkm
qqzhzbdf
bsdobwkh
bqdprzha
yhszdmef
xkwfdmuk
mgczauqw
esuqatfx
nsmwzkxz
pwvkgdlv
vocqoqga
oqqtglfr
kxuhlbwx
ljelrpbb
vjlfxfaz
xihzikeh
lepiyydq
zbbcletk
cngmhyjr
ydvkjzko
imfnuytd
fccjiacp
ewevykge
ynoxvmig
fzdlexys
oypcsecg
rbpvrcvv
eymcbkxy
mikyujcu
wzgxtsql
vuojkhgq
dcflnifn
xnheoecs
qqqowmea
ascnutyg
nichzgxy
fszhhwlc
fzjnowcu
wmhisugz
getnucis
gnsdlkjd
tdtdqgnl
galitfun
ikqeqxhl
cvywqjlk
obkkpwxy
kwyubstt
eljadxxe
lwpzhfcu
scmkmyqj
txzqjvpv
abeijkpo
itkxnaym
drvekvkd
rqysrauf
akzsohed
xjneqesc
zzhpxfnb
edqfifgh
cwpszcnf
xhhopkua
jcmltvqr
oikwrydj
kewqixur
mgmvolqv
qplmgtkf
prqwcpml
chgizgja
fyqcempn
tfbmvrbh
rlbmwdce
woymburn
pezqonho
fznxkomh
mtmyndpm
xvndjjwo
vjwqeqjv
sxfeahzl
zahyvrlw
amvfzlbd
cofaaqeo
hraodbvu
yqddvddr
ywcbrqxs
yifoffga
ryakycka
aldbnpkc
gzbnjgwx
dbmuryha
kbcfsdcf
tkauzbni
cpsronsl
mptyurop
iapeipuh
glkmcrhy
zjdriwwk
txdacbyg
jpggncvg
nekvihae
rhlowuya
ghpownth
itixvfmb
zdfaycfa
qbetfgnp
zhvgaaue
gmkcuxiy
kvfwuqmj
dfyjiibh
uldjqdik
vayodecd
fcrptbsl
bpkrxnpg
nxceomni
uqdjgsnk
emcqcjvx
ctsebwhz
igzkebwr
jlmnkzro
nflruvsc
dydkyrfm
mwhfwdyo
afnrppxc
ptgculvd
wresmjow
umjxnvgc
kdlcxidd
mspmqgnv
glujmbfs
dxjqbxhn
jbwibcuc
jtufbtyh
zfixtoec
awtnaswy
viedwkop
quyuangc
hgdhkgow
gvisxrle
avxehrsn
nuzxbxmn
xdjxxpjx
cwxhsqpz
jsptklax
hrspgkdr
rghmfkpr
kymmmcab
bbzcujvv
jyyqdcbn
kvjpazrt
wnwfiyjl
jvwrbnbi
jqmjpwnd
fopeyutz
mfeppqte
azcjbavi
uteltaxf
ensdtizg
afupwjas
jsvreklb
xgwcsylf
bzlswqps
kekyroql
wqdjwfir
wbuayqxv
emqnbrqb
cnzqauow
qsmtfbdf
aochxott
nwfwkeae
ijdcshdi
dpwsudgr
oaamnnea
mluzhvlf
fqlpiggq
cytmczdp
unpiijjy
uuudrjkx
ogwjvsjq
jrfhuqfc
zsqluoiv
selafsom
gdkunobm
mzjxifaq
jkeggahz
hxancvri
qaypytsd
oxgydtjv
xntwktcs
zijgqmzj
myqvumje
pbvbpqjq
virtaxeo
glvxjssx
romkjdyz
drwzndbv
rnftqwrn
bovwonoc
wpvgidzh
vujhqizk
ewrzepvh
ahidqlyj
ombljgna
laqxgoor
trsztwtj
okxxzfrs
ydsyalyb
mipyxubw
uhszecbe
tkhrlpin
ynkqgjuf
usxvkzot
ilxgponl
juwgpslt
lupstgdz
liocdvrq
cljsobdt
wrbmyvsy
sfnkwozr
weoseowu
hftgfbvg
gqpixaeq
javqkxck
quxbqioa
msjqizqy
pctywkzv
wwuesfwk
dmzmjfmh
lrolrdqk
ikhfctot
yocwwoqf
vaylqepx
vgagdnau
hvmjlktg
pcirvhlw
wblmhuiy
btyzukto
gxapfhwr
mikkqymd
oturyvbd
jnhonnrl
ljbsbyxn
koktfhfo
cgubhrfo
eycdxbmx
cfgfmnvj
phzbvepd
pvfoudpl
rljpzvrx
kmrzklix
zkixilas
hkyylnmf
gfrijkqm
huzlcysy
zsnsackz
akobdtez
rozkivil
mzgqzylh
llyufhvi
hnboetec
lwjvfbjq
ebxuggjb
erqyjixp
xrorpiax
fplrcbir
pjgaeiyp
nwandnpl
bbbseuhw
xvatmaxr
qyjokwri
dybwsmtp
vqrrpazk
zntcuass
snldpzro
oglioppo
uizpwpbp
imvdquvt
qsbcooii
oaifcspj
rtogzpct
koxwtgoh
wldfgvfv
jcomclml
rnhbrhgy
rheslqiu
icieclfb
spzmzffo
ywfolqsv
ggpkeiaf
ycuosrcz
bjhyzalp
gvlcgmsu
urtfsirp
aczwgpvf
syizzpfm
jeytiogw
baiqzhng
xfoshrln
jalajywc
fbqgnerq
leswwkie
txmkynaa
tozexgmk
tjvdybfs
nguvfhsr
vzulliwb
ejxdnzzw
rzpjbaxz
yhihatyi
cbtrpgtq
hewhjlgj
vecvstym
qcrzjsli
egaglwfe
dpwjmfyn
osxyhciy
bvrlrnrg
ifmjlgvb
tcwiitbj
dvxnlkqd
hbmfpszg
nymiayxn
tlpuagil
xxrzmint
ukosvewb
nfslvmua
ztxjiohi
syzucnhc
lkhkmjeb
irvawvji
ohubcctp
yhsmhiwb
uirlkpdc
umthcmcq
dwnekfwb
sjhwuiwr
lhrumxet
hwodycjm
huokhnxi
ixgnnedb
fuquvhit
ihopkgmj
qkshanae
brcadzaw
gteigmzt
igsptydi
hdadcetj
gpoylltw
kmjgylrl
vcuvwmju
apfjrshd
tfdtgdht
lgifzoct
hrfspexu
pkujmfpy
nqraesca
ojlirtfu
kjacrauy
wmwuvplj
qqjkraoh
zbrgpbfm
cmlpbslo
yofcjzrj
yzrvfgxi
jelbozct
uddcvkkk
pdzymucm
upuphein
rccweles
lgonahnv
hjchxvgg
vzhpjcvj
bxfafcbm
ndrnrxmr
uxnwopde
dmnnntog
rleorqpc
spirduaq
qiyblahq
fqqvqxop
yppeabkq
vsddqzwc
wanqfzmw
sppvdznl
ispmbhkp
rpzuvqcw
sdtajnhp
smsedlfu
zhmewxfu
kunslglz
mkmqorqj
qquztzdg
ddepulmc
pirabmzd
cositrpq
hegtpypm
tpexgfqj
vbhxdezm
wzfkgzrj
drjvuuyz
qbknvxyx
aegkjtil
cklbkfhh
mafufjxv
gpwakxoa
otyjtnxx
nqdosdmm
wungcwuz
wiklkshf"""
