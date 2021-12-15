(ns day19
  (:require [instaparse.core :as insta]))

(def syntax "
S = e

<e> = e_to_HF | e_to_NAl | e_to_OMg | <'e'>
<Al> = Al_to_ThF | Al_to_ThRnFAr | <'Al'>
<Ar> = <'Ar'>
<B> = B_to_Ca | B_to_TiB | B_to_TiRnFAr | <'B'>
<C> = <'C'>
<Ca> = Ca_to_CaCa | Ca_to_PB | Ca_to_PRnFAr | Ca_to_SiRnFYFAr | Ca_to_SiRnMgAr | Ca_to_SiTh | <'Ca'>
<F> = F_to_CaF | F_to_PMg | F_to_SiAl | <'F'>
<H> = H_to_CRnAlAr | H_to_CRnFYFYFAr | H_to_CRnFYMgAr | H_to_CRnMgYFAr | H_to_HCa | H_to_NRnFYFAr | H_to_NRnMgAr | H_to_NTh | H_to_OB | H_to_ORnFAr | <'H'>
<Mg> = Mg_to_BF | Mg_to_TiMg | <'Mg'>
<N> = N_to_CRnFYFAr | N_to_HSi | <'N'>
<O> = O_to_CRnFYFAr | O_to_CRnMgAr | O_to_HP | O_to_NRnFAr | O_to_OTi | <'O'>
<P> = P_to_CaP | P_to_PTi | P_to_SiRnFAr | <'P'>
<Rn> = <'Rn'>
<Si> = Si_to_CaSi | <'Si'>
<Th> = Th_to_ThCa | <'Th'>
<Ti> = Ti_to_BP | Ti_to_TiTi | <'Ti'>
<Y> = <'Y'>

e_to_HF = H F
e_to_NAl = N Al
e_to_OMg = O Mg
Al_to_ThF = Th F
Al_to_ThRnFAr = Th Rn F Ar
B_to_Ca = Ca
B_to_TiB = Ti B
B_to_TiRnFAr = Ti Rn F Ar
Ca_to_CaCa = Ca Ca
Ca_to_PB = P B
Ca_to_PRnFAr= P Rn F Ar
Ca_to_SiRnFYFAr = Si Rn F Y F Ar
Ca_to_SiRnMgAr = Si Rn Mg Ar
Ca_to_SiTh = Si Th
F_to_CaF = Ca F
F_to_PMg = P Mg
F_to_SiAl = Si Al
H_to_CRnAlAr = C Rn Al Ar
H_to_CRnFYFYFAr = C Rn F Y F Y F Ar
H_to_CRnFYMgAr = C Rn F Y Mg Ar
H_to_CRnMgYFAr = C Rn Mg Y F Ar
H_to_HCa = H Ca
H_to_NRnFYFAr = N Rn F Y F Ar
H_to_NRnMgAr = N Rn Mg Ar
H_to_NTh = N Th
H_to_OB = O B
H_to_ORnFAr = O Rn F Ar
Mg_to_BF = B F
Mg_to_TiMg = Ti Mg
N_to_CRnFYFAr = C Rn F Y F Ar
N_to_HSi = H Si
O_to_CRnFYFAr = C Rn F Y F Ar
O_to_CRnMgAr = C Rn Mg Ar
O_to_HP = H P
O_to_NRnFAr = N Rn F Ar
O_to_OTi = O Ti
P_to_CaP = Ca P
P_to_PTi = P Ti
P_to_SiRnFAr = Si Rn F Ar
Si_to_CaSi = Ca Si
Th_to_ThCa = Th Ca
Ti_to_BP = B P
Ti_to_TiTi = Ti Ti
")

(def parser (insta/parser syntax))

(def input "ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF")

(defn answer [tree]
  (- (count (flatten tree)) 1))

(defn -main [& args]
  (println
    (reduce (fn [smallest-answer tree]
              (let [current-answer (answer tree)]
                (println
                  "currently minimal: " smallest-answer
                  ", new: " current-answer)
                (min smallest-answer current-answer)))
            9999
            (insta/parses parser input))))
