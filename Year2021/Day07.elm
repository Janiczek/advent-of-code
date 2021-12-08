module Year2021.Day07 exposing (Input1, Input2, Output1, Output2, compute1, compute2, input_, main, parse1, parse2, tests1, tests2)

import Advent
    exposing
        ( Test
          -- , unsafeToInt
        , unsafeMaybe
        )
import BiDict exposing (BiDict)
import BiDict.Assoc
import List.Cartesian
import List.Extra as List
import Set exposing (Set)



-- 1. TYPES (what is the best representation of the problem?)


type alias Input1 =
    List ( List (Set Char), List (Set Char) )


type alias Input2 =
    List ( List (Set Char), List (Set Char) )


type alias Output1 =
    Int


type alias Output2 =
    Int



-- 2. PARSE (mangle the input string into the representation we decided on)


parse1 : String -> Input1
parse1 string =
    string
        |> String.lines
        |> List.filterMap parseLine


parseLine : String -> Maybe ( List (Set Char), List (Set Char) )
parseLine line =
    case String.split " | " line of
        [ observations, outputs ] ->
            Just
                ( List.map (Set.fromList << String.toList) <| String.split " " observations
                , List.map (Set.fromList << String.toList) <| String.split " " outputs
                )

        _ ->
            Nothing


parse2 : String -> Input2
parse2 string =
    parse1 string



-- 3. COMPUTE (actually solve the problem)


compute1 : Input1 -> Output1
compute1 input =
    let
        uniqueNumbers : Set Int
        uniqueNumbers =
            Set.fromList [ 2, 3, 4, 7 ]
    in
    input
        |> List.concatMap Tuple.second
        |> List.filter (\output -> Set.member (Set.size output) uniqueNumbers)
        |> List.length


type alias State =
    { segmentMappings : BiDict Char Char
    , numberMappings : BiDict.Assoc.BiDict Int (Set Char)
    }


compute2 : Input2 -> Output2
compute2 input =
    input
        |> List.map compute2Line
        |> List.sum


compute2Line : ( List (Set Char), List (Set Char) ) -> Int
compute2Line ( observations, outputs ) =
    let
        all : List (Set Char)
        all =
            observations ++ outputs

        findUnique : Int -> Set Char
        findUnique count =
            case List.uniqueBy Set.toList (List.filter (Set.size >> (==) count) all) of
                [ onlyOne ] ->
                    onlyOne

                _ ->
                    Debug.todo <| "findUnique " ++ String.fromInt count

        hasSize : Int -> Set a -> Bool
        hasSize size set =
            Set.size set == size

        hasSizes : Set Int -> Set a -> Bool
        hasSizes sizes set =
            Set.member (Set.size set) sizes
    in
    let
        shape1 =
            findUnique 2

        shape7 =
            findUnique 3

        shape4 =
            findUnique 4

        shape8 =
            findUnique 7
    in
    let
        a =
            Set.diff shape7 shape1

        shape6 =
            all
                |> List.filter (hasSize 6)
                |> List.filter (\set -> hasSize 1 (Set.diff shape8 set))
                |> List.filter (\set -> hasSize 1 (Set.intersect shape1 set))
                |> List.head
                |> Advent.unsafeMaybe "shape6"

        c =
            Set.diff shape1 shape6

        shape9 =
            all
                |> List.filter (hasSize 6)
                |> List.filter (\set -> hasSize 1 (Set.diff shape8 set))
                |> List.filter (\set -> set /= shape6)
                |> List.filter (\set -> Set.intersect set shape4 == shape4)
                |> List.head
                |> Advent.unsafeMaybe "shape9"

        g =
            shape9
                |> (\set -> Set.diff set shape4)
                |> (\set -> Set.diff set shape7)

        e =
            Set.diff shape8 shape9

        shape5 =
            Set.diff shape6 e

        shape0 =
            all
                |> List.filter (hasSize 6)
                |> List.filter ((/=) shape6)
                |> List.filter ((/=) shape9)
                |> List.head
                |> Advent.unsafeMaybe "shape0"

        d =
            Set.diff shape8 shape0

        b =
            shape4
                |> (\set -> Set.diff set shape7)
                |> (\set -> Set.diff set d)

        f =
            Set.intersect shape1 shape6

        shape3 =
            Set.diff shape9 b

        shape2 =
            shape8
                |> (\set -> Set.diff set b)
                |> (\set -> Set.diff set f)
    in
    let
        numbers : List ( Int, Set Char )
        numbers =
            [ shape0
            , shape1
            , shape2
            , shape3
            , shape4
            , shape5
            , shape6
            , shape7
            , shape8
            , shape9
            ]
                |> List.indexedMap Tuple.pair

        translate : Set Char -> Int
        translate set =
            numbers
                |> List.find (\( n, nSet ) -> set == nSet)
                |> Maybe.map Tuple.first
                |> Advent.unsafeMaybe "translate"
    in
    outputs
        |> List.map translate
        |> List.map String.fromInt
        |> String.join ""
        |> String.toInt
        |> Advent.unsafeMaybe "compute2Line"



-- 4. TESTS (uh-oh, is this problem a hard one?)


tests1 : List (Test Input1 Output1)
tests1 =
    [{- Test "example"
        "input"
        Nothing -- Just "parsed-input"
        -1
     -}
    ]


tests2 : List (Test Input2 Output2)
tests2 =
    [ Test "first line"
        "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
        Nothing
        5353
    , Test "all lines"
        """
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
    """
        Nothing
        61229
    ]



-- BOILERPLATE (shouldn't have to touch this)


input_ : String
input_ =
    """
aecgdbf badcg fbcage gdabce be gaedb bced dfeag adbfgc abe | gedbacf be bdfcga bedfgca
gbeda bf fbgea dgafce fcgedb fgaec bcfa bfg baefgc dbfgace | fb cfba cbedfg afbc
dfegb bcfdae cagbef cedfg cde cadg fcagde dbcefag dc geacf | dc aedcfb ebadgfc cgdef
fgecab ebfd ed acgbd bgadfe dge dbage daefcg gfeba ecdbafg | dfbe fbaeg abegd caefdgb
edbg gd ebfcg cafed dfgecb gdf agcbfde dgfcab fegdc gfbaec | dbge fbaedcg gd dg
fdbagc egbdf dga ebcfad caedgf eadcf dafeg dagfecb gaec ga | fadebc edgaf ga eagc
ed gefacb dfacge fedb cde ecgdfab cgbfe ecdbg edfbgc dbagc | defb edfb ebgadfc efgcb
afgbec adfecg ebfacgd gbfc fb ecdba aebfc debgaf fba gfcea | gadfbe gefdabc dbeac fb
def degfba egfbd ef cabdge gbdfc adbeg eafbdc geaf edacgfb | bfdgae fde fde geadb
fbdcg edafcb dgabef cfedb eadbf ced gcbedfa beca acfgde ce | fcaged eacb bgeadf dbacef
bgcaf fgabd ebfgc cfa dagfce cgefbad gfcbea ca fdbgce aebc | aebc edgcbaf adfgb gbfad
ebgad ea dbfeg bceagdf dgcba aebc edgabc cafdeg abfgdc aeg | dgcba ae ceadgf gbedf
bcdeg gfedab cf cfg aefc fcbdega ebfag cbgadf gbfec acbfge | fc cgf gcf fc
dg adcebg bcdafe dbgf gda bfade adfgbe afedg adefcgb geafc | cgaef dg caegf gfdecab
dgace afbegd bcgdeaf cdgfea ca cdegb cfga egdaf eacfbd cea | eca cfga gafde ca
cfgeab gdfeabc fcbeg fdegba ae ega gcbea dagbc fcgbed feca | degbfca bdfgeca gcbaef caef
cbgfa dfbcg gdb dg fdgbea edcg cfgbde cfdbe cdafbge bdeafc | dg dgb gfdeab bdg
fb bedgfc cbf fegadcb dfacbe bfceg fbgd gdfce dfgaec geabc | adbfce gafcdeb bcf bf
cegfd edagc fadgebc abfegc cf gdefbc aefbgd gedbf gfc cfdb | dcgbfe cdbf fbdc fc
ebcag adbefc defgca fdga gefdacb egfcdb afcge fg gcf edcfa | cgfdbe adefbcg bcfadeg beacg
cebfa begfca efb fe ecbgad bgaec fgecbd fdcgabe gefa dabfc | ef ecabdg egbfcd cgbea
gdae dgebf acgebf dafcgb dg efbgda gfbeacd fdg cbdef efabg | bfcde dg gfdbace dfg
gfcdb fbecad abdce fcdbe dgfeab efca dfe bcadeg ef degbfca | dgabce fdgbc fgdcbae bcgdf
becf ecdafb aefgd gbdcaf cdebafg cdf acbed cafed fc debagc | abdfgc gcbfad cagdeb fc
fabdc fcegdab be deb defbca fbacdg bfade gfcebd beca gdeaf | ebca be bcagdf gcefbd
dacbge cbdae gfaeb gacfbd baged acdebf bdg ecgd gd acfdebg | gd dgb edfbgac dg
bdfecg fgeac agdeb fadb bgeaf bgdfae bgcedaf fb cbadeg gbf | bfeag cabefgd baged fb
ecga cef becdfg bgcdeaf fcdage fedac cdabfg dfagc dbefa ec | ce cdgfbe fec cfbadg
ecbf agbcf beagc dagcf cafgeb bfg dgfbcae egbacd bf adgfeb | fgb fgb cbegfa befc
bdcaefg fdb bdefa edbca cgaedf gefb adfeg fb fcgbad edbafg | fdb bf ebdaf fbeadcg
cba ecgfa bdgc gfabcd bc abfedcg abgfd fabcg bedafc gbfead | cab bc bcgd dgcb
fbeadgc fdacbe abde dagfce cfbag efcda gdefbc bec be feabc | dabe be ecb cefagdb
af eaf debga bcedf ecgfbda acfegb adcf bfgecd ceafdb badef | dfac af fa bfdeac
cdaeg fedcag edfcg fgacedb gadbcf acd egacb dgfceb daef da | begca ad efacgdb cad
cefdg adbg ecbfag cbeafdg geafb bgedf dbf ebcadf fgdeba bd | gedbf cgdaefb dfb gdcef
cdfeg gecafbd bdfea bgfadc gbfaed fcead cdfbae fca ecab ca | fcbgda bcae ecab acf
abdcef gbacdfe fdabc fedac cfbe fde bdagfc eadcg ef edgbaf | efcb adfec fed bcedfa
fcadg efbgc bdf afcdegb bd cfdagb cdgbf bcda gdebfa deafcg | fbcgd agcfdb cgfad db
efg gbdce cgedaf bdfe acbegd egfcdb gbfec ef fdcgbae fbacg | bcdfeg fcgba egf gebcd
cabedf cgfed fdgeb baeg gadfb eb cgafbed edagbf fbe acbgfd | dbfacge edgfabc eb eb
gfecad gebdf egb bdcfe gb gadb ecfgbda egdfa defgab cfabeg | cegafd gafcbde adbg egb
cefbg ceb ce egbdf cagfb bgfedc gcde bdfgae bfdcae gdbfeca | ce gbcefad edgc ce
bacdg cbge ec fdega cafgbd gbecda cea bcdfae fabegdc cedag | eafdcbg cegadb fgdcab eca
gfcadb dfbe dfg egadf gbfaed agbed df dgeacb agefc dfgcabe | fd dbfe dgf bedf
efabg gbcfe bcaefd gc gec gcdb efdbc gfdcae ecgbfd cedbafg | cgfeb cdbfae fdaegbc ecg
dcfgae dcbgfa gfe fgcad afdeg fdbea ge cgdebaf fgcdbe geac | cdgabf dcgabf edfbcg fdebgc
efdacg dbgef defcbg be abgcde dbe cfdge gabfd dafbceg cfeb | fdgbace fdbga bgaedcf dcfeg
cfedgba agdbc gfdcab fg fgab fagdc dfcea becgfd adcebg fgd | fg eafdc gdbacf cadbgf
dgecbf agedfc af dcbae becgfda dbafe bfgdea fbegd gbaf fea | af ebfdg af abefd
afebgd ecbafdg bge bg gcdfe bcdg adcfge ebgcf acefb bedgcf | fecdag fcaeb beg bg
bfgdec de efgca cedabf agcbdf gedb dec gbdcf afbgdec gecdf | dcgfb cde fegca ed
fegcb ecd egda gcbda fadbec edgcafb adgbcf ed gdbec egabcd | cabedfg fgdebac de egad
efcbg cafeb dcafbe ebag gbf bg dgefc ebdafgc bfeacg dbcafg | bg fgecd bfg efgcd
cfbg aedfcb bg gdb ecbdg dbfec adcge aebgdf agcbdfe edbcgf | gbd cegbdf fegdbc gcedb
fcbgae bdacef aedfg gdcafbe fdebc cbda fdaeb ab eab gbdefc | ab dacb agefd gcbefa
gdcfb dfgceb eb feb cfaebg faced fcedgab cbedf gbde fbgacd | feb baecgfd eb eb
fbdaegc efcb bdgaef fbgcde dfc bfedg cbgfd egacdf abgdc fc | dgcba cgdfb bgacd gcafbed
aegfbc gfdbac abfec efb fceg dfbcage bcdae fe eafdbg bafcg | gfedab fe bcfdga ef
fcdbag ecgfabd dgaecb dgcae ed cebd deg egbfda gefac gbacd | de caedg aegbfdc bdcfaeg
adfbc eba bedcga becda efcagdb egad gcdebf cgafbe ea dbegc | efcgdba dage bcgdafe ae
fbgace gafdc adge cfdegab bfedcg fgeacd gd dgf eacfg fcbda | dgcaf bdafc gacef dg
gdecf dge bdcgf fdbgea ebdgfca gfacde cead gecfab de cegfa | edg ed ceda ed
cbedf cbeafg edfagb adec bfade cbgdf ce fdecba ecbdagf fce | cbfed fec aced eacd
gebac ebdcagf facbdg abcgfe fbc bfgde ceaf cgbeda fc bcfeg | bcf bcf fc bfc
facgeb cgfade abc abcdf ceagdfb edbcf ab cdagf abdg bgcdfa | afgdc ba gebfcad dbag
fbadgc defac ga egafd ebdfgca dfecba aegc adfgec defgb gda | ag gaec gdafcb dga
face cag gdafeb dbagce agecbf gbdcf gbcaf fbeadcg egfab ca | cga ca becfga efac
bcgdfa bdgca cgebad fcgb fbedac dgafb dfb faged gbadfec fb | dfbag gdfba fbcg dbf
decabg be gdafebc edgfb ecbdfg ecagfd gcedf dbafg geb cebf | fgebcd dcgebaf gcbdea bfec
dfaecg gdbea cbd bedcfa gcfbde cbfg dfgaceb gdcfe edgbc bc | bc gcbf edgba bc
bgeafc gedfbac dceaf bged fdgbce cgdfb ebc bfecd fcagbd be | cfgbea bec dcgafb adbfgce
fde fcea dgefca dfcag abged ef eadfg cdfgeb fgbacd dcefbag | gdacfe cfea fdcbega cagfbd
gefdca eb bgdfa adegbc edgcf cfabdeg gfdeb bge befc gedfbc | eb fgdecb dbaegc eb
bgadcf aefgb gecdba dcef fgc aegdc cf cafge agdcfe agcfdbe | cf fdcabge fc cgf
dfegb fg fadbe gcdefba baefgd cegdaf dcabfe gcedb gef agbf | fabg gabf aefgdb fg
eadfc defcb fdabcg dabceg gceadf ea eac eagf gbacdfe dgcfa | cae faecbdg ebgdfac eca
adcbgf badfce cfd fdega beafdcg cf caegbd bcgda gdcaf gbfc | cf cfgadb efcbda fabdegc
gcd ecdfb abfcde fgdab bcgfaed gc afedgc gbdcf egdcbf ebcg | cdfbge bcedfg gceb gc
eadbgf cfgbe bdagf decgfa bdac ca bfagc afbcdg cag acfegbd | ca cag edcgaf abdfgec
gcfbea cbeadgf bdcg cd decbga abdce cabge adbfe adgecf cde | gbfeadc cbgd fcedga cbega
fc decf abecgf cdeafg adcfg acdbge agbfd decag cgfeadb cfa | cfa cf bgadf eacgd
bgafe ae afecbg gcaedb bae cfae ebfcgd cgabfed gbadf egbfc | acef egfab bae befgdc
afcdbg aegbd gdbace baecd gd ecgdfab dgb ecgd aebgf baedfc | efdbcag egcd efdacb gbaef
afcgbe gdbc agfde cbdega dc dgbeacf dce afebcd acegd baceg | aecgd cde cdgb dgbc
fabed egab cfgdea fae bafdgc cfbde ae bgdfa gbacfed gedafb | efbad dafeb eaf fagdcbe
dbca agebd gbdcea ebfcadg fgabe bdgcfe eagcdf da gad bgecd | abdc gfbaced gafedc gbfae
cgadb ecfadb agb bdgf dbfac bg dcgea dcgeabf fabegc bfdcga | dgfb gb cbagd gab
bcadf efad ef gebcafd ebf bgedc bfcgad becfd ebfagc cdbefa | cbafd ebf faed dcfab
aegfb gabedcf fbg bf ecfb dbfgca adcgfe afbecg beagd egfac | becf decfga gfb gcafedb
gedbc adgcb da adg feabdg gdaefcb fcad bcagef gbafc gbadcf | dgcebfa da bdgce fgabc
cdgfeab fdeca bcafde cg dgc abdecg fadcg egcf afdbg ceafdg | dgc gdc ecabdfg fdaceg
gdbefa gfacbd dgacfe abcge eacfg dcfe egf gfcad fe gaefbcd | ef edcf egcdfa efgadcb
gacd fabed caebg cdbafeg cd ceabd gbdcae edc dgbefc abcgfe | dec cgda dc cde
bgae agedcb caged dfcaegb gcb dcbag dfaceg cfdab fecdgb bg | abcgd ageb adgcfbe beag
cdbgf dfbagc cfd cegbd bfdag agbefd gcfa fc fbecda fedcabg | gcfdba cfag aegcdbf gfac
gedca efab gcdfab bdeca ecdbagf ab befdc bad abecdf efgcdb | faecgdb ab egacd acged
eaf agebf abfcg edga debfag ae befgd dabfecg dfebca edfgbc | ea aef agde ecdfabg
gdceab dcafgeb ecfgda ef fdbag fbdge fge edgcfb edbcg bcef | gef cdbgfe bedcfg ebcf
cdgbfa fgbec gbdefc acef ebagd ac acbdegf bca ecgba ecbgfa | bcgafd gafecb ca ca
bdaecg cfbg bgdef cfead feabgd gce gc bcdfgea egfcd efcbgd | dceagb befgdc gc cg
dfaecb gbfde gefad gbae bfegdc cafgd ae fae gcfaedb bdagfe | ea gfdaeb bfadeg bfecdga
efa geadf fa bcgafe cagedf dcaf badge cebgdf fcgde fbdaegc | fea afgedc fa cgfde
cebdga de baedgfc cdagfb ecdfg ebgfdc cdgbf dec efbd efcag | egacf bgcdf ed eacfgbd
edac gaebdf afdeg agedbcf acdfge ec fce egfac cebdfg afgbc | fbegcad ce gfcab cdfage
dfaeb edfgb ebg debcgf fedcg gebacd facgde fbgc cadbgfe gb | bfdge gbcf cdgbef fbgc
gc ecgfbd bagfd fgabc gcda gedbfac aefbc bfcagd fadegb gcb | efabc cgda deagfcb gbfedc
gbdea cgefadb gacd ged aebdf gd cgeab eacgbf fbecdg dbacge | dfgbeac dgca faebgcd cbdeag
fdabc bgecf dgab fbaegcd gadbcf fagced cfdabe gaf ga facbg | fdeabc gfbadc gaf cgdbeaf
cbagde cbdefg ad afegb eagdfc fegad dgfce acgebfd dafc agd | fcad eagdbc gda efgda
egcfa adgbe cdfega cebgadf gebcfd bc begac bafc ecb abgfce | ceb egbdcaf edfbcg bafc
dfg gcad dg dcagfbe baefd fdgeac fegac dfbgec cfebag afged | ceafg agcd gadef ecdgbf
eag ea agedbc gafced bcea abgcdef edagb bacgfd dgbac bfegd | acdbeg fagedc aecb abce
eacgf defgcb dfc fagdebc ecgabf fd dcgefa edaf dcgaf bdcag | cegaf cgdaf eadf bcgedf
ecbda ebdf bf cfb fgbcad abedfc abegdc gfeca fbaec dbacefg | fcb egdbac badcge dceba
eagcdfb cfgea cebd cgfdba cbgae eab acbdg eb bceagd gbfeda | abgdce agefdb edfbag fagdbc
decfb gecbafd gefabd gebda efgacd gf egabcd begfd fdg bfga | fg cegabd efbdcga gf
gfce fed febcd ef gacfbd debac adcfebg fdabge gcedbf gbcfd | ef cgbfad fed eabcd
fda gedbf edgca af ceafdb efgda agedcb acgf bgdceaf acedgf | fda fbdace cgfa debfg
gdebca dgcab acgf cf bfdacg fdbegc bfc efbda bgcdafe dafbc | gcebdf cf fcb gebcdf
ecgfdb dbae fagdce fbeag ea afgdceb bfaedg fea bedfg cfbga | eaf dbgef ea bfaegdc
daegfc dfebc cgebf cbaefd dcf ecgdfba dc gadbef aebdf adbc | gedfba gcdabef abedfc dc
edfbag cdeabf decafbg acfgb ac fbcaeg acf gaec fegba cbfdg | cega dbfagec ebgafd cgae
fgabde ecagf fcdae agfbcd ge abfgc gcbe gea dfegcba ebcagf | cfaegdb gebc acfed ge
dgaebc cgb fgbedac debcaf gc dgfc gcbadf afdcb fegab fbacg | gbecda cbfadge gbc fbcdae
egdbc gbcaedf bcf afbg ebfgc bf bagcfe gecadf cefag dafbec | gbcfe dbaecf decfgab fb
befgd caef fgc fdeagc ecgda dcbfag fc acebdg dfceg cgfebda | fc eagdc cf cafe
cgabf acegdf gbadec deaf dcage cfaeg cbgfade egf ef decfgb | cgefdb decabg adceg cgedab
bdce gdfce cbgafd cbdfge facdbeg fce bfaegc fgbdc ce gfeda | agdef ebgfcd fbdcg dfegc
gca bage ga fcedg fbcedga cagbfe cfbae cfbadg facdeb egacf | agbcdef agc gca ga
efgab ce adec ebagcd abdgc abgec bfacgd gbefdc efdacbg ebc | ec egacbdf agfebdc beagf
be fcdeag gcebad adcbe ebcfagd eba ecbg dcfba adegc edgafb | ebgdaf ebgc dgcfea cebadg
febgad geda dagecfb abdfe fbcga dbgaf efgcbd dg dgf cdfabe | gaed adge gdae gaed
fe gbcdaf cfdgae begdf agdbf bdgec bfegad fabgdce bfae gef | gfadbe beaf agfdec egf
gcbef cegabd cgebdf dbcge abgfe cf cdfb fgc gbcdefa agfdec | gcf fgc bedgac dbcf
fcbed dfgeab becdagf egabf fac gafcdb ca cega becfa fceagb | cdbfag cfa bfagcd cfedb
efabgd bgfea ebacdfg afb cfebgd ebdfg af agceb dafceb fagd | fa fbdage afb dafgeb
agb ba fedbgc geabcd fgead fcab fadgceb afbdg cdbfag cdgbf | abcdge ab ab agedcb
bfega feadbg fgbc cgfeabd cfa abcfe afcebg acdeb cf cfaedg | fgbc fac acf dgafce
egfba fdbeag fda ebdf agbfd fd ecfadg bgcfea bacdg fgeabdc | faebgd gfeba eabgf fadgb
decgbfa dcfbag bgf abdg bg ebgfca gfedc bafcd fabcde gbcfd | gb fgecd dgecf gbf
gcbaef cfgead fb acdbfeg acbf gcfea gebcf ebgcd degabf fgb | gaedfbc bf cgefa cbgfead
gfc abcfged edgbfc egafcb fc dbgefa acfb gbeaf aefgc ecagd | fc bgeaf gcf cfba
ecadfb cegbf cedbf egb edbagfc gbfdea gb feagc bdgc dgfbec | bcfed efcbg gb bg
gbfa fa dcbegf dagce bgdface dcafg bdfcg cfa bdgacf fdecab | fgbcda bedcfg cfa gafebdc
afbd gcdeb agecdf dafbeg ebfga cfgaeb fed dbgfe bafgecd fd | df afdb edf gcbed
fgcedb cbg bcfgead agdb dbaegc gcaeb aedcg bg ecbfa fgdeac | bg acgbe bg gb
dgacf bf fbd afbcd afgb dbafcg ebafdgc fadgec dfegcb cdeba | bdf fgab fbd bfga
daecg dabecgf edfagb fcagd cadbgf dabfec bfgc gbadf dcf fc | bafedcg afgcd cdage dcafg
cgafbd beadgc fagbc afb bf gbdca gafce fbdg fgdceba efdbca | cfgbad fab abf bfa
febgd cfdge fcda gbdacfe fc cagde degfac cbedga fabceg fce | ecf befdg cdefg fc
dcaf agecdb dgfbcea cgeadf faegb gecda dgafe fd bedcgf dfg | daegc bcdeag cfad fdgea
dbgea acdeb ecabdg fadce begafc abc dbgc defagcb cb fabged | agdbcfe cagbef dacgbe deacf
fdbge ae caedfg ade agbfcd ebca egadcfb dcabf fedcba daefb | aebc caeb dea bdefg
edab cda fedacb da gbdecf dcgefa fbced acfgb cbafd bacgefd | afegbcd cfebdga ad fgaecd
afgbd cedga cgabd cb bcdfea fgabdc cfbg gcbdefa fdageb bac | efcdabg bfegda cba abdcfe
gfdcab ceg adgbc aefcd gedca cebfga ge ebgd gfaebcd degcab | edfgbca eg gebd gdbe
eafgcb afcbd egbfa dfge fedabg gd fabgd cgbeda dgb aebcdgf | dbg bafeg cegbdaf fdge
cefdg edbfacg cb fabced dgfba bfcgd cdb dcbagf bfadeg acgb | dfeabg abcg bceafgd cbd
bac bagcf acge cbdgf cafedb gafecb fbgae agbecfd ca agefdb | cage cfdgbea fgebca bafgc
bgeacd cbd dgcfeb db bfeac aefdgc fbcedga cfdeb gbdf efgdc | befca dcfeb gedafbc dbc
dbcaefg fadec abfdge gcde ecf ceagbf efadg ec acfdge cfabd | febdag fdage acdefgb cef
badgfc fad eafbc fbdcge fbdge dgae eadbf abedgfc da fbdgea | edbgf gbfcde baecf dfgbce
ce bgefad fecg bedgac dce badcf degafc deagf baegdfc cedfa | ec egfda dce cfeg
gadcbe afebg edgbfc fcad fgcebad fce cf fedcba faceb beacd | cef acegbd dafc adcegb
cagfde dbgcaf baecfd gdebcfa dce cbfde de bgefc bcdfa abde | ebdfagc abgcdf adbe cde
bcaedgf agecb eadgbc egda gbcfed aebcd cdabf ed fgbeca deb | fadcb edcba gcfedab adbegfc
fcgedba cadegb ecdgbf eabcf ga fabdgc cgbed agb dgae abecg | ebdgc abg ceafb cbfae
badcg gdafb dacbgf aegbf fcegadb gcdf afd fd adcbeg defcba | df dacbg cfgd agbedc
bfdc cb gbfad agefbd aegfc gbc agcdbe adfcbg bedcfga cgbaf | bcg dgbaf cb egafdb
bfea aecdb ebdcaf cfb bf gdcfeba cefdg aedgbc bedcf afcbdg | beagdc fb befa ecdab
cabd egfadc cd abdfgec decfb bdgeaf cfd fbdae gcfbe ebfdac | afbcged cd bdafec dc
cgbdafe fdabce cbfgda gb befgca dcagb bfgd acdbf agecd gba | aebgfcd bg dbgac dfgb
gecdafb fe bfdea gbfda bef fceabg adbec degf agebfd afcdgb | bafedg fged fecgabd bgcafe
becadf decga ca adc cgadfeb gadfeb deagb gcba decgf gabdce | cad acd egdca adc
dbaef dagbef cegafb baf gcfbeda fgdb baged efcda bceadg bf | bgdae fb bfcage gdefab
deagbc bedfg gdafcb dcgafeb cg ecdfab cedbg gcea edabc gcd | ecga gabfcde gcea gc
bdfacg cbdag ecdga cebdag geab ea eda begcadf ebfadc gcfed | dea dea dabcg fbgadc
dcga ceafb facbg fgdacb gc fdgecb adgebfc degbaf cgf dbgaf | fcg aefbc gfc dgac
efacgd gabdfe bagd bdfec begacf afgbe da fad ebafd fbgeadc | eafdb agdbef bagd adbgfe
acdgbe eb cfbe gfebca fedagbc bge gefab gcdaef agfdb cfgea | bfcagde fgabd befc afebg
fgedc cfabged bc ebac acbdeg gcedb eabfgd bgc dgabe badcgf | gcb abec efabdcg beca
ebcfgd cbdgaf dbcga facbegd da dga fcad ebcga bcfdg bgdefa | bcgae gcadb da agd
gdeba gbc gc abgec befdga dagcbe bdcagf cbfae cegd bfgaecd | cg dageb egcba gc
cgaeb acgbed cbade abefgcd fdgcba aged edbfc adb da cagfbe | agde aegfbdc da acebd
deb caefgd eb eacb egdca dgcefba dgabfe gdbce cbaegd fgbcd | ecgad egbcda fgbdc ebd
aegfbd bdeagfc facg cgebf abfecd edgcb cef fc agefb gbcefa | cgadfeb fce gcfa fbdega
degafcb cdgbef efdb cbegd fgadce acfgb cfe begcf cdabeg ef | egfacbd bafcg fe caedgf
cbfeadg fceba bcdegf fb ebf eadcf egcab egfcda fedbac fdba | abecg gaceb fgadebc cadefb
efacb cb cgbe afdegb agebf abc cdebafg fcbage cafed abfdcg | bc bceg aedcf cdgbeaf
dabcefg geafb eadg cfedba dgeabf fdgba adf ad fbdcg eabgcf | fgdcb edag bedfca aegbf
gcdbefa bag dbafg aecfbd ag beafgd cfgbd aegf gabedc aedfb | ga deagbc bagfd afedbg
gefdba ea gcdbea adgef abef efcbgd gae egdbf cdgfa cdagbfe | gea feba abfe afbe
bcfag eafdbcg feagc defbgc bdgeaf gbc gbacfd adcb dgfab cb | bc dfagb ceabgfd bacd
cadgfe gefbd bg cdfge ebfdgc bgdafce gdb cgfb fedab gcdeab | bg dgb bedaf dbgfcae
afcdb dcbg eacdf agbfec gbacf bd fdb dcbgeaf befdga gabfcd | bcgaf cbfaeg cfdab bcfad
cebfdag cgafe cdfbe abfec fegabc fdaecg ab eab aegcdb fbag | egcaf eba gbaedc dbgfcea
cbdge gcbdea bdg cbaed defcg ebag gbcdeaf agbdfc bg ecafbd | ageb aebg gb fecbad
dgc gbcad bdafgce cedagf dfbg abcefd dg abgcfd abceg afcdb | gfdb eagbc efdbac dgc
fgdab egdbcaf dbagfc efbadg ge egcdfb cabfe fbage gead geb | gbe adcfgbe ebg beg
bgfd cbafegd dcabe gfcba dagefc gda ecfabg dg bcadfg bcgad | caebd cdgba acgdef gfecbda
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
