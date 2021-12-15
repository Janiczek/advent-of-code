module Year2015.Day05 exposing (Input1, Input2, Output1, Output2, compute1, compute2, containsLetterTwiceInRow, containsThreeWovels, input_, isNice, main, parse1, parse2, tests1, tests2, wovels)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
          -- , unsafeMaybe
        )
import List.Extra
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List String


type alias Input2 =
    List String


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    String.lines string


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    input
        |> List.filter isNice
        |> List.length


isNice : String -> Bool
isNice string =
    containsThreeWovels string
        && containsLetterTwiceInRow string
        && doesntContainNaughtySubstrings string


wovels : Set Char
wovels =
    Set.fromList
        [ 'a'
        , 'e'
        , 'i'
        , 'o'
        , 'u'
        ]


containsThreeWovels : String -> Bool
containsThreeWovels string =
    (string
        |> String.filter (\c -> Set.member c wovels)
        |> String.length
    )
        >= 3


containsLetterTwiceInRow : String -> Bool
containsLetterTwiceInRow string =
    string
        |> String.toList
        |> List.Extra.groupWhile (==)
        |> List.any (\( _, others ) -> List.length others > 0)


naughtySubstrings : List String
naughtySubstrings =
    [ "ab"
    , "cd"
    , "pq"
    , "xy"
    ]


doesntContainNaughtySubstrings : String -> Bool
doesntContainNaughtySubstrings string =
    List.all
        (\substring -> not (String.contains substring string))
        naughtySubstrings


compute2 : Input2 -> Output2
compute2 input =
    input
        |> List.filter isNice2
        |> List.length


isNice2 string =
    containsPair string
        && containsAlternatedLetter string


containsPair : String -> Bool
containsPair string =
    let
        pairs =
            List.range 0 (String.length string - 2)
                |> List.map (\i -> String.slice i (i + 2) string)
                |> List.indexedMap Tuple.pair
    in
    pairs
        |> List.any
            (\( i, pair ) ->
                pairs
                    |> List.filter (\( ii, _ ) -> (ii /= i) && (ii /= i - 1) && (ii /= i + 1))
                    |> List.any (\( _, pair_ ) -> pair == pair_)
            )


containsAlternatedLetter : String -> Bool
containsAlternatedLetter string =
    List.range 0 (String.length string - 3)
        |> List.any
            (\i ->
                String.slice i (i + 1) string
                    == String.slice (i + 2) (i + 3) string
            )



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        -1
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    []



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
rthkunfaakmwmush
qxlnvjguikqcyfzt
sleaoasjspnjctqt
lactpmehuhmzwfjl
bvggvrdgjcspkkyj
nwaceixfiasuzyoz
hsapdhrxlqoiumqw
lsitcmhlehasgejo
hksifrqlsiqkzyex
dfwuxtexmnvjyxqc
iawwfwylyrcbxwak
mamtkmvvaeeifnve
qiqtuihvsaeebjkd
skerkykytazvbupg
kgnxaylpgbdzedoo
plzkdktirhmumcuf
pexcckdvsrahvbop
jpocepxixeqjpigq
vnsvxizubavwrhtc
lqveclebkwnajppk
ikbzllevuwxscogb
xvfmkozbxzfuezjt
ukeazxczeejwoxli
tvtnlwcmhuezwney
hoamfvwwcarfuqro
wkvnmvqllphnsbnf
kiggbamoppmfhmlf
ughbudqakuskbiik
avccmveveqwhnjdx
llhqxueawluwmygt
mgkgxnkunzbvakiz
fwjbwmfxhkzmwtsq
kzmtudrtznhutukg
gtvnosbfetqiftmf
aoifrnnzufvhcwuy
cldmefgeuwlbxpof
xdqfinwotmffynqz
pajfvqhtlbhmyxai
jkacnevnrxpgxqal
esxqayxzvortsqgz
glfoarwvkzgybqlz
xdjcnevwhdfsnmma
jyjktscromovdchb
pvguwmhdvfxvapmz
iheglsjvxmkzgdbu
lwjioxdbyhqnwekv
zcoguugygkwizryj
ogvnripxxfeqpxdh
hkvajhsbfnzsygbm
cnjqeykecopwabpq
wojjtbcjinoiuhsj
kpwpvgxbyzczdzjq
wrvhylisemlewgzk
uiezkmnhilfzahtm
mucteynnuxpxzmvt
zaiwbgxefusfhmst
apptbogpxivjwink
qryboarjtwjhjgjb
irehxupgyseaahzd
fobstqxguyubggoh
ysriumfghtxtfxwe
auchdmasvfeliptw
mztuhefcrnknyrdl
tyjmkhihbwabjtaa
yquzkdtgsljkaebw
almvdvofjtkyzbmd
emqftiuqqpdwwbrv
hrrhmqfpepvbawvw
atrkgykycvgxbpyb
dhthetnealksbdan
zzqafhgicubptiyo
qdtaieaziwhbttnw
kyskgapdgqrtrefw
edwzlpqztpydmdlr
awszjnlmvlyqsuvl
kcrtmtshtsgixvcp
jtaskgkijivbbkri
mmggfwapsetemiuj
itagrrnjbnmhgppd
uqmbezechbrpbnqq
nnyimvtascflpzsa
knqeimypkdttyudj
vgoiyvtvegwyxjjd
qubzdxsbecktzrho
zehojtvktsbbxijb
xepmjrekwcgoxyoh
bnptxnocbpbqbyeq
sfvynsywscbnymos
dsltfbpcmffbluba
kncrlzlmkikylppa
siwudrvmildgaozv
jhhefbvbvneqzvtc
lqjgztxitbuccqbp
himmwlbhjqednltt
vwognchyertnnfil
eejakhapkbodrntf
qxuijkkhhlskgrba
aankpfxxicfpllog
vuxykvljyqexfhrn
epgygflbxlbwybzq
zuxmwvetmvcszayc
xttwhfqmemgtjnkf
hftwldmivyfunfvl
bejlyxfamzliilrj
zkehazcxyyvtrxti
dsgafehmcfpycvgz
igremmqdojqdvwmb
swnjzvmhcslvkmiw
fchzbfbmtqtxmaef
xwjmyyrlznxrcytq
brwcwzpcvbwdrthl
fvrlridacsiojdmb
mhsturxdlmtxozvy
usxvqyrwywdyvjvz
gwazuslvmarfpnzm
rgkbudaqsnolbcqo
dpxvlbtavdhdedkj
nnqmjzejhodyfgyd
ozoazxkfhujgtzvy
psdgvhzdiwnuaxpl
tznkilxpogbzgijz
wnpytcseirtborhh
lhauurlfsmagfges
oqfbzixnlywkzwwy
yoehapoyjpakziom
vtjftdcsfdzbmtrn
zcshfnodiwixcwqj
wapbxpaxgjvtntkm
qfyypkyvblrtaenh
bsxhbxkovgukhcza
kitdmvpiwzdonoyy
slkbhxmehzavbdsf
dovzjouqkzkcmbkl
qpbigdcqkfnfkxvq
eaiaquhnesvtcdsv
mhbezlhqojdsuryj
dqprkkzxlghkoccx
xqepmorryeivhrhm
frwmrjpezwmjflvf
gjpfgwghodfslwlf
fzyvajisdjbhfthq
pvzxkxdscdbilrdb
mtaxmqcnagmplvnm
rlyafujuuydrqwnc
gvqvrcxwyohufehq
lmrkircgfrfusmfd
ovlpnkxcpimyaspb
xhyjremmqhdqywju
pxfczlhpzbypfarm
utjhprzhtggausyp
utzkkzlnyskjtlqh
cecbcnxpazvkedic
xwvoaggihrbhmijq
krredhmtwlfmyagw
lwfhxgbknhwudkzw
vyczyvuxzmhxmdmn
swcoaosyieqekwxx
waohmlfdftjphpqw
gaclbbfqtiqasijg
ybcyaxhluxmiiagp
xgtxadsytgaznndw
wzqhtjqpaihyxksm
fdwltsowtcsmsyhm
rpoelfbsararhfja
tswgdacgnlhzwcvz
xjgbhdlxllgeigor
ksgthvrewhesuvke
whgooqirdjwsfhgi
toztqrxzavxmjewp
hbkayxxahipxnrtl
lazimkmdnhrtflcu
ndoudnupbotwqgmr
niwuwyhnudxmnnlk
hlmihzlrpnrtwekr
wzkttdudlgbvhqnc
rfyzzgytifkqlxjx
skddrtwxcyvhmjtb
mljspkvjxbuyhari
xwkhozaoancnwaud
nookruxkdffeymdz
oiqfvpxmcplyfgoa
qoxggshmrjlzarex
lsroezewzkrwdchx
nkoonmvdydgzspcl
lygxeqztdqklabov
jempjyzupwboieye
hpdaqkhjiddzybly
cvcizjlnzdjfjlbh
vaaddsbkcgdjhbkj
pjxmtxoyrkmpnenf
ujqdvyqnkbusxlps
miyvzkzqploqaceb
gapcsbkulicvlnmo
xqpcyriqhjhaeqlj
ipumdjwlldzqhmgh
swdstecnzttmehxe
ucmqordmzgioclle
aywgqhmqlrzcxmqx
ptkgyitqanvjocjn
wcesxtmzbzqedgfl
rnetcouciqdesloe
chpnkwfdjikqxwms
onpyrjowcuzdtzfg
tydnqwaqwkskcycz
dhamguhmkjzzeduy
oecllwyrlvsyeeuf
gsukajpoewxhqzft
sgdnffdixtxidkih
pqqzjxzydcvwwkmw
wnjltltufkgnrtgm
hylaicyfrqwolnaq
ovfnugjjwyfjunkm
xknyzsebmqodvhcl
uwfmrjzjvvzoaraw
zaldjvlcnqbessds
zphvjuctrsksouvz
ceqbneqjwyshgyge
wmelhaoylbyxcson
nghuescieaujhgkj
dhjmflwwnskrdpph
exvanqpoofjgiubf
aidkmnongrzjhsvn
mdbtkyjzpthewycc
izctbwnzorqwcqwz
hrvludvulaopcbrv
mrsjyjmjmbxyqbnz
sjdqrffsybmijezd
geozfiuqmentvlci
duzieldieeomrmcg
ehkbsecgugsulotm
cymnfvxkxeatztuq
bacrjsgrnbtmtmdl
kbarcowlijtzvhfb
uwietqeuupewbjav
ypenynjeuhpshdxw
fwwqvpgzquczqgso
wjegagwkzhmxqmdi
vocvrudgxdljwhcz
nnytqwspstuwiqep
axapfrlcanzgkpjs
lklrjiszochmmepj
gxadfpwiovjzsnpi
qidsjxzgwoqdrfie
wgszciclvsdxxoej
kwewlmzxruoojlaq
ywhahockhioribnz
ucbqdveieawzucef
mdyyzmfoaxmzddfv
hsxnabxyqfzceijv
vivruyvbrtaqeebr
jxfeweptjtgvmcjc
mmypqxmpurhculwd
mpiaphksvctnryli
xqzqnuxmuzylkkun
fndmtefjxxcygtji
dnorqlldvzqprird
nutokyajmjpwjaqu
vlupfperqyqkjcaj
dgihjeokrphkpdnk
nvbdyrlheqzixuku
mhrkntnxvsmvrpka
kvhkyanlhhymwljf
fhipumtegqfgeqqw
vpfjgveycdefuabu
kzincljffncylcsf
tsezxymwmjtyegqw
wxhcdrqedkdcwxli
ueihvxviirnooomi
kfelyctfvwyovlyh
horzapuapgtvzizz
iiqkdpmfvhwwzmtj
rsaclclupiicstff
quwkkhrafypkaoum
gyrgkgmwqfkeudfe
noydhbqacwptyfmy
efwwuipzgtkwffhf
suyojcitomdxsduh
lbcxnsykojkufkml
zpglsvoutvzkgdep
usgrufyvgsbsmbpr
katrrwuhwvunjqor
btngwrpcxoyfbgbc
bxjscjdiowjrkpns
nwxvnfrnlkgqxvhf
ikhyqkvljucgdlag
xibnxsjopmxvflkl
mzplumcfivqcjqnz
jqflcxoxzlbwlxry
fcscvmfepdxrshxe
wlpffwunffklzbuc
emvrlqajjgwzfmle
rhaheurtzrfoqkyq
ifuuhpxmadaysfsx
ncyfvleyzqntpcoo
zeogmyaqccmtvokd
jqppbzebppdnpurn
xixarswxsiwjzgni
ezruwzajsoombphs
hmiqfeizyprielxf
jnaoxljnftymsfey
extgzrxzovlsixnf
yhyfmovvlrwoezsv
ffnybaolppuzpjym
pqowimdiusccaagn
jgceiosiihpjsmnu
hkoexeaopebktngx
njhzuvsygymejqav
yjkgcclgtvushcfk
gmbjxhnkkxlihups
pdlwysadiebsidjz
omrwmgzulfoaqros
ofvvgdezwvcffdcy
otytpuklhxcpxhgd
eyfaosxdauumvlux
mvdthjfstrlqlyuo
mdgdchgnlxaxspdm
bakjezmhbwqxzevd
msakswaphdwaodhg
vjcqscgdbnsxdllh
jjywaovewbuzreoj
nqvplhwacylifvwk
lpwmpixbxysmsign
flcvbpxrchcpbgcb
qjpkeuenenwawlok
bnqkflfmdmntctya
fzsgzpoqixvpsneq
icwfdisutoilejld
relchofohnkwbumi
aljalgdaqwhzhfwr
cahkvnwnbwhodpqs
dnrzeunxiattlvdm
nsmkhlrpwlunppjs
mqqsexlwfqnogwub
tfavelkqrtndpait
ooguafrnmprfxcnz
ntynkiordzxtwrqa
rkkyzlxekqqlkvym
ofxcivdnwcmgfnme
ywotqwbrqxlrnobh
nrbbiypwhrqihvev
flqsjixxtydheufs
lcfrfzypstrqctja
hyzbuzawuzjrynny
exfbywcnstebnvmq
vydzwnbmcihvqrnj
qmwqaaylinzrdmiw
lpxpztpvfggspeun
lhxmqqbracsuyrfm
zgkwsrabaseidbrw
yjlmbhbqsqgszsun
mqfzqtbxtuteabtd
izomzdmcqmfrevwd
iqijrlqurdwrkoln
fxhqzpgoxxjkkhql
oulwontmgrjeopnk
edaigfydjexvzzvj
vjhybiklxpxjqpwc
ypxfbfnpbmqmwtte
xzvcsgasztrxdzud
rpulqmobptfarboo
palacmdijxzzykrf
jmllwukplufohiby
dnswayomusiekfmy
sxbrjqtqgzzwhcfo
lylvndsgbnbqiejm
jaxxhoulxnxnaenr
nblissutfazbcpwn
zmlsjszzldvbiacr
kewojtlchfkclqwk
eqvfjasddggvfame
yibzqlvxtraxpdon
dgnbxsbmdrtyvaac
uoxrcxfimhgtxqhy
xfdxalrwcwudlviq
xmtbdklqptoswpwl
zezyopzdztdjerfl
xuzluhjsqvhytgbc
qdjtmeckispmgzki
phakupesplzmmmvc
gpuoqfffumzszybn
bhywxqkrrlwuebbw
ibvwgoyvelzenkzl
ncohvvbmiekbaksa
fzuvqzvxvdbeirrp
lshtzniokucwojjd
punrduvlnrulkium
gnfpikidnfobrrme
vxkvweekmnvkzgyl
rhydssudkcjlqgxn
cjtqvlaahohcgumo
jwzmfyinsfwecgcb
blpeseqhlzfilpuf
jvtpjkyokzcvagon
qjomincbcobjczpe
ugsyzkzgdhxtmsfz
hleaqgwzqjwajcra
coumfghptpnxvvov
hqpnbupnzwpdvgqd
cpouyodqxgviasem
lljvxeyozckifhfd
huqtnvutdyfgwtwa
yenlveuynmlmmymu
ojdyufkomxiwjmbf
spjzgvcwvzgffjkk
vxykmjhyvmhyssbp
tazdeqggfcjfvwwn
uumwcngwcytvpufx
avovuzkrevloneop
owczrtbnrvjfemkt
hzpugcanaxyvaokj
iishlodnxvjtgzyn
qosdonclrnxirham
eonqlnwevahydddg
ryqmnuikftlxuoqy
whqepbcwabzbthha
vekisvnwhgpyemxr
lrwxzoamnvpnlhap
ywepvqthnorfswjv
evqwvsoazmwyypjy
bgwoojddubppmjxf
jypkfrthzgtyeddi
tynabbhfjzkrqsju
adxstbfqheuqbcuk
gqwqiocdyqoiblrx
ybuddlyuskdlegxv
luwynbsmpgyeqsbr
ltyqgqoyljibqndo
jaedpajzphfybajh
epglnrxofptsqvmy
zjdpxkngfkstxbxh
ekegphcwanoickfu
cqvhuucvejqirvfs
uqudnnqumsqcgefo
qnzunermlnpcfflo
ovyxaniqaawzfuxx
djekxcezjowdhopq
bwtwbmdehrhpjnlk
nilsnlacerweikfa
hyrigsrmsrzcyaus
gvmdmgddduylmxic
ewzovdblhmjgjwsk
ojjfsknlonzguzlq
yjgfruvpjvlvrvvq
cyoryodwyhzwprbv
crsjclrurcquqgut
sjhfhobwtojxcmem
ibxfjudilmdeksea
uqbhdbjoeupyhbcz
uqbxigzxuxgmjgnw
jashafmtzrhswirg
dexiolovaucyooka
czjbwwnlwcoqnoiu
ojigosazigfhttjc
zfiqtgrqbmftknzn
dlzbmvmolssbqlzl
sgmchcurrutdtsmw
scdwjqsdohcdrwry
cgtdvecqwplpprxn
iiplenflfczaktwi
wmgnwfxfcjhyeiqg
giihshowtcatecvl
nqhzfincclumvkaz
kxstpzgdfvepionc
agbhxcijxjxerxyi
hmgfqevgdyvisyvs
tthakmvpowpvhtao
ottalcghygpaafbo
aplvozayycremgqg
dbjxlnaouxqtdpfz
peeyallzjsdvpalc
ndtdjyboixuyhfox
llabnbcobexfoldn
cweuvfnfyumbjvxr
ewkhhepaosalnvkk
pivyiwsiqpwhagyx
auzsnwdcerfttawt
grbfrekupciuzkrt
byfwzadtzrbndluf
lluypxjeljzquptk
pskwsnhqanemtfou
sxvrtqqjdjkfhhrm
ulsmqgmshvijyeqh
qigofesfhekoftkf
zhatniakqtqcxyqa
uuczvylgnxkenqee
mlitvtuxknihmisc
srrtrxdvcokpyfmz
osispuucklxcfkeb
vqhazlaulmnpipql
umkiueljberqhdig
knvpbkbvgoqzwprp
nbsocqikhuvsbloj
wjnpepjkzkednqbm
agbhmytsofuyqcor
gvogzhkkpxyfecko
ardafguxifeipxcn
yiajcskbgykyzzkw
sejunbydztyibnpq
dqrgfggwcnxeiygy
xnqqwilzfbhcweel
jjtifhlvmyfxajqi
gwszrpgpmbpiwhek
kydzftzgcidiohfd
efprvslgkhboujic
kecjdfwqimkzuynx
rildnxnexlvrvxts
dlnhjbqjrzpfgjlk
qluoxmzyhkbyvhub
crydevvrjfmsypbi
dosaftwumofnjvix
pwsqxrfwigeffvef
nzyfmnpwqyygjvfx
iccbckrkxlwjsjat
bmputypderxzrwab
bhuakynbwnlreixb
qmrzfyqjiwaawvvk
juvtixbkwyludftn
zapmjxmuvhuqlfol
paiwrqjhpjavuivm
tsepfbiqhhkbyriz
jpprewufiogxoygk
mmapyxbsugcsngef
pduhmgnepnpsshnh
aetndoqjvqyjrwut
fnfvlorhwpkkemhz
gedfidpwvoeazztl
beclvhospgtowaue
wsclsvthxustmczm
tjbxhnpniuikijhe
rhetyhvfcemponeg
mavonujurprbeexi
argbrpomztrdyasa
bzvtffbtygjxmkvh
maqyqkhsqgzfzvve
seeirbiynilkhfcr
wxmanwnozfrlxhwr
dieulypsobhuvswb
nxevassztkpnvxtb
jclxuynjsrezvlcy
xlolzyvgmwjsbmyf
tguzoeybelluxwxc
fkchoysvdoaasykz
cyynwbfcqpqapldf
rhifmzpddjykktuy
ndvufsyusbxcsotm
txutnzvdsorrixgg
qjoczhukbliojneu
ufhwujotncovjjsz
kclsgsdwcrxsycbr
yscwmlrdaueniiic
nxhivrovpkgsmugb
fdxqfyvwwvgeuqkv
femtamfylysohmpr
amsyzslvyxsoribh
nhmqxncwsonhgbcz
uomqsvcbpthlmcue
kxtfapcqrnjkkslj
xtieihonlfubeync
adpcjqxgydulchgj
cjynnzsmmujsxxpd
neeapmzweidordog
szoivgqyqwnyjsnk
uwgrtzaqezgphdcu
ptpgttqxocjwxohi
fhltebsizfwzpgpf
emmsazsidspkhgnh
dxcprkbcjeqxqzgn
tpxzqwxbzwigdtlt
afsmksnmzustfqyt
xyehnftstacyfpit
vcrfqumhjcmnurlw
rrznpjzcjgnugoch
gbxnzkwsjmepvgzk
jwobshgwerborffm
zmuvfkhohoznmifs
buyuwgynbtujtura
bevncenmpxfyzwtf
hqqtcrhzfsrcutjh
kbpzshllpiowepgc
alspewedcukgtvso
xvsvzzdcgjuvutrw
pmwulqraatlbuski
abuzsiinbueowpqn
oedruzahyfuchijk
avhcuhqqjuqkesoq
azqgplkzsawkvnhb
rjyoydogkzohhcvx
aezxwucqvqxuqotb
kxobnsjvzvenyhbu
nnjoiilshoavzwly
aijttlxjrqwaewgk
cvsaujkqfoixarsw
zngtoacpxcsplgal
qhkxliqtokvepcdv
aixihrtdmxkfvcqw
owbgdgdymxhhnoum
tajsagmruwzuakkd
ckrfduwmsodeuebj
alfdhuijuwyufnne
xpchlkijwuftgmnm
rwcrvgphistiihlg
xdaksnorrnkihreq
akeschycpnyyuiug
rgputhzsvngfuovz
lerknhznuxzdhvre
mqiqmyladulbkzve
csnmupielbbpyops
kwgrwgmhfzjbwxxz
npwtvbslvlxvtjsd
zxleuskblzjfmxgf
hexvporkmherrtrn
rhtdhcagicfndmbm
qhnzyuswqwoobuzz
dpvanjuofrbueoza
kjcqujmnhkjdmrrf
gholddsspmxtpybg
jihlvyqdyzkshfsi
zuviqmuqqfmtneur
kzexjowatvkohrtx
wgijnfhibsiruvnl
zevkrkmhsxmicijb
khxrcteqourjvoxa
ylpxlkcnenbxxtta
zrfsvctbojjkpvtw
nlzbudxibnmcrxbt
cqnscphbicqmyrex
ywvdohheukipshcw
riwatbvjqstubssf
idlztqqaxzjiyllu
sdpdgzemlqtizgxn
rjtbovqlgcgojyjx
fnfrfwujmjwdrbdr
osnppzzmrpxmdhtj
ljhwngclvydkwyoe
chwqkrkzrvjwarat
jmydkwpibkvmqlgs
zvhfmbxnlxtujpcz
jsnhsphowlqupqwj
fzhkkbpasthopdev
jerntjdsspdstyhf
gctwmaywbyrzwdxz
xemeaiuzlctijykr
xulrqevtbhplmgxc
yfejfizzsycecqpu
gboxrvvxyzcowtzm
lpvhcxtchwvpgaxp
wdiwucbdyxwnjdqf
qgwoqazzjlvnjrwj
prtlnkakjfqcjngn
fagvxsvjpuvqxniz
xacmxveueaakfbsm
ginvtonnfbnugkpz
qpvggsppewfzvwin
reoqnlzruyyfraxa
kolwtqhifjbbuzor
vrkcywvdhdprztww
ngdvyfmvjqhbzbxt
rooxeoilqzqjunmp
efxmdprtogtxgyqs
qrhjuqndgurcmwgu
ouitjprueefafzpl
kirdwcksqrbwbchp
fpumsmogojuywezo
lgjrgykywugzjees
xigioqcpjabpbdas
ewkhuprpqzikmeop
fgrgxsqeducigxvr
bclkursnqkzmjihl
jozidniwvnqhvsbc
oghcilcyozrmmpta
xbgmaungzcpasapi
iqowypfiayzbcvhv
opdehgwdgkocrgkf
zfzvdjeinlegcjba
vhakxvlcayuzukap
xyradgyiebpevnwe
eamhtflgedwyshkn
igteqdgchjeulfth
kwsfkigxzpbgdxod
vapnpsbdboiewpzp
wbuqhjsngxpqshen
vxxilouxuytitwgm
cpnwlkwnkeanqnet
wdmbtqvvlowftvgb
wjtmcecpyqzwpbqg
jnxmoxdhvsphcdeg
wabxfxpotoywwodn
mwbsoxzlqpqobvvh
coktshbyzjkxnwlt
rzhnggpslwzvyqrp
dgzuqbzarbutlkfx
wunajaiiwgijfvjh
uotdbcgmsvbsfqlb
kxdtlgmqbccjqldb
ngmjzjwvwbegehfr
cvpsabqfpyygwncs
wqluvqlhdhskgmzj
rbveperybfntcfxs
fbmoypqdyyvqyknz
zxpgzwnvmuvkbgov
yexcyzhyrpluxfbj
ltqaihhstpzgyiou
munhsdsfkjebdicd
plecvjctydfbanep
kjrxnnlqrpcieuwx
zbcdtcqakhobuscf
kgovoohchranhmsh
llxufffkyvuxcmfx
tgaswqyzqopfvxtw
kojcqjkdpzvbtjtv
xggdlkmkrsygzcfk
vvitpsnjtdqwyzhh
gcqjuwytlhxsecci
vbsghygcsokphnrg
vejqximdopiztjjm
hudqtwmwkviiuslp
vwswfvpcwwpxlyry
gxmfiehdxptweweq
qjmekjdcedfasopf
pqyxdxtryfnihphf
felnavctjjojdlgp
hbimufguekgdxdac
dhxhtnqgfczywxlr
pssottpdjxkejjrh
edieanguabapxyig
sciinanyqblrbzbb
irxpsorkpcpahiqi
qsxecaykkmtfisei
ivfwlvxlbnrzixff
hqxzzfulfxpmivcw
vvbpaepmhmvqykdg
cetgicjasozykgje
wuetifzdarhwmhji
gaozwhpoickokgby
eldnodziomvdfbuv
favpaqktqaqgixtv
twbcobsayaecyxvu
lzyzjihydpfjgqev
wnurwckqgufskuoh
fxogtycnnmcbgvqz
aetositiahrhzidz
dyklsmlyvgcmtswr
ykaxtdkjqevtttbx
kfmnceyxyhiczzjm
nnizopcndipffpko
yjmznhzyfinpmvkb
sljegcvvbnjhhwdd
zmkeadxlwhfahpwg
rwvcogvegcohcrmx
aguqwrfymwbpscau
vlusytjagzvsnbwe
smvzhburcgvqtklh
rfuprvjkhazrcxpv
megqlnoqmymcrclc
gvldhkewtmlwqvqv
awynhvtyziemnjoa
voprnvtnzspfvpeh
dhlguqwmunbbekih
goayirdhnjrfuiqi
eoghydfykxdslohz
chpippjykogxpbxq
hqbycjweqczwjwgf
pvefsrvwumrlvhmt
eghwdovaynmctktk
crwkxoucibumzawc
bzbtahvhkdigvvtj
bnbptgihhfubxhho
ddqmbwyfmfnjjaro
gvtswqyzazihctif
vmqctjpgadxztqqb
dgnndowtpeooaqqf
sxdvctfdtalufxty
ylgeexosibsmmckw
sxplpyskbpqnojvw
coarhxtsvrontyeg
fyoaurggjupvzvlv
jlyrkqsiwuggvjem
uwbsjoxonreuucyi
gihuqvwxovbgokes
dxzaaxupbcgnxcwf
gidrgmvyrlqqslve
csflmlvqmonoywpx
jkxkpixlythlacnk
ejkarcdkdslldugv
dbzmsusevohhjkmr
cbrqzualjpdtworc
kpgidqlmcbpfmmwu
zwghjuofexfowqam
ncdlxmcrsmsocetz
kfprzqacefifjkbd
swwzivrxulkhvldc
wgqejhigbjwunscp
rsstnwcyybfauqxu
qhngfxyhdqopyfgk
zrndpyyejsmqsiaj
xxknxwpvafxiwwjc
mmaahwgoiwbxloem
tabacndyodmpuovp
yriwomauudscvdce
duvyscvfidmtcugl
mgipxnqlfpjdilge
imeeqcdetjuhfjnw
dvkutrdofpulqkyh
jefvtlktxegpmbya
iyzudqgpvlzjfydh
giohapxnpaqayryd
qheqdprmnqlpztls
rdxhijmzegxkotoq
hdnmaspumdwnrcdz
wafpbgehbuzdgsnc
tbtrfztsferdmhsy
vusndcyjngtkrtmk
ilqblestzxebcifh
urfgjbjgzlrfsdlv
aptcdvpsqwleqttn
bigczjvzokvfofiw
zjnjeufonyqgkbpx
trcdebioegfqrrdi
jrdvdriujlmbqewt
jqrcmuxpwurdhaue
yjlermsgruublkly
zwarvgszuqeesuwq
xthhhqzwvqiyctvs
mzwwaxnbdxhajyyv
nclsozlqrjvqifyi
gcnyqmhezcqvksqw
deuakiskeuwdfxwp
tclkbhqqcydlgrrl
qbpndlfjayowkcrx
apjhkutpoiegnxfx
oaupiimsplsvcsie
sdmxrufyhztxzgmt
ukfoinnlbqrgzdeh
azosvwtcipqzckns
mydyeqsimocdikzn
itfmfjrclmglcrkc
swknpgysfscdrnop
shyyuvvldmqheuiv
tljrjohwhhekyhle
dayinwzuvzimvzjw
qgylixuuervyylur
klqqaiemurawmaaz
hdmzgtxxjabplxvf
xiivzelzdjjtkhnj
ktgplkzblgxwrnvo
gvbpyofzodnknytd
lqhlmnmhakqeffqw
ltzdbngrcxwuxecy
obxnfjeebvovjcjz
zexpwallpocrxpvp
tjpkkmcqbbkxaiak
qiedfixxgvciblih
qcxkhghosuslbyih
gnsfidwhzaxjufgm
xrghwgvyjakkzidw
tftftwedtecglavz
wquqczzkzqrlfngr
twibtkijpvzbsfro
bmplypdsvzuhrjxp
zanrfmestvqpwbuh
zonrhfqowyimcukm
kpvajjfmqpbhrjma
kujzluicngigjbtp
iusguantsrwxdjal
kwxeuylcnszswahw
visdhnkobxnemldu
rogeadmmaicwtabl
pxqycifbgevqudvs
osaiozyvlyddylqr
vffjxrolrpuxcatx
jbmsetccdrywssjd
qgxyhjfpbfifmvgc
npejgalglldxjdhs
mbbtqgmttastrlck
whapaqwdtpkropek
dulbdboxazfyjgkg
xaymnudlozbykgow
lebvqmxeaymkkfoy
bmicnfuubkregouj
dieatyxxxlvhneoj
yglaapcsnsbuvrva
bbpjaslqpzqcwkpk
xehuznbayagrbhnd
ikqmeovaurmqfuvr
ylyokwuzxltvxmgv
hqtfinrkllhqtoiz
pjmhtigznoaejifx
fqdbmowkjtmvvrmx
uvqtqfoulvzozfxv
rpajajukuxtchrjd
sznucejifktvxdre
ufvibsmoushmjbne
xirdqoshngthfvax
iafpkddchsgdqmzl
vmualmlduipvykzh
fnmuahmblwyceejb
ilsaapnswfoymiov
lenvylifraahaclv
cukqxlipuyxedqfh
zgwecslpniqvtvuz
cdcdfpsxuyrhsmag
dszjinhantnxgqra
ioimwotsgnjeacgt
dqcymnvjystbynhp
yibaudyfefbfgunx
cabslcvunjavqkbf
goymzvmgkvlsmugf
zxteiitpthzskjjx
agnxcnaqhjhlurzs
cvmgyxhhnykuxbmb
cgqmjexydmvgwxpp
sygjajofieojiuna
clpvxbrbjvqfbzvu
cbntswqynsdqnhyv
bztpbtwbefiotkfa
pnxccbgajvhyeybu
asyzrvgzumtuissa
facjyblvcqqginxa
rvwnucnbsvberxuv
ghrbeykzrxclasie
ekujtselepgjtaql
krtrzsmduhsifyiw
ticjswvsnyrwhpnt
clmjhsftkfjzwyke
lbxlcixxcztddlam
xhfeekmxgbloguri
azxqwlucwhahtvep
kitdjrwmockhksow
keznwwcusgbtvfrs
ljvzxoywcofgwajj
vebjnhnkcfzbhrcw
eqfcxkavstxcuels
ldattkyawjrvcido
bsqqeilshcwtqyil
foqqsxahfiozcqrw
liswfmuhzfbyzjhf
sulbdcyzmolapfbs
zuggzkelwxjpsgxb
betioxrgtnhpivcw
xmtbixstdipibhgs
ttvurgqmulryyaji
viobnljznzppfmxw
qlzabfopydtxrlet
tusvydegfxhaxolk
thoufvvfjferxhwp
cfyyzppfarjiilbs
jwmhxtgafkkgseqs
pqwuuaxbeklodwpt
vndyveahdiwgkjyx
ssrjgasfhdouwyoh
thbavfcisgvvyekf
yjdvxmubvqadgypa
tlbmcxaelkouhsvu
bonohfnlboxiezzr
rktlxcbkhewyvcjl
rsmoutcbcssodvsc
qszdratuxcrhsvoh
eypyfahpuzqwzwhi
yhkrleqmqlmwdnio
vpnvxusvmngsobmq
hkzyhopvxrsimzys
dblriiwnrvnhxykl
xkriqxkrprjwpncs
rcymltrbszhyhqti
mzbvneplsnpiztzn
vkqtnptgbqefvfoc
nwdtfiaozkcjtlax
crximadpvdaccrsm
lrbajafxwwnxvbei
rbexzesrytpwwmjf
stxwjarildpnzfpg
btamaihdivrhhrrv
acqbucebpaulpotl
dkjhzghxxtxgdpvm
rsbzwsnvlpqzyjir
mizypbwvpgqoiams
nvrslorjpqaasudn
wvexcpzmconqkbvk
rfwfumhjwzrvdzam
eaghdaqorkhdsmth
gtuntmpqaivosewh
nzlsmdgjrigghrmy
dhuvxwobpzbuwjgk
kkcuvbezftvkhebf
aeediumxyljbuyqu
rfkpqeekjezejtjc
wkzasuyckmgwddwy
eixpkpdhsjmynxhi
elrlnndorggmmhmx
ayxwhkxahljoxggy
mtzvvwmwexkberaw
evpktriyydxvdhpx
otznecuqsfagruls
vrdykpyebzyblnut
cnriedolerlhbqjy
uajaprnrrkvggqgx
xdlxuguloojvskjq
mfifrjamczjncuym
otmgvsykuuxrluky
oiuroieurpyejuvm
"""
        |> Advent.removeNewlinesAtEnds


main : Program () ( Output1, Output2 ) Never
main =
    Advent.program
        { input = input_
        , parse1 = parse1
        , parse2 = parse2
        , compute1 = compute1
        , compute2 = compute2
        , tests1 = tests1
        , tests2 = tests2
        }
