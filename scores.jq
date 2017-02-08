#!/usr/bin/jq -rMf
def show:
  if type == "number" then if floor == . then tostring else . * 100 | floor | . / 100 | tostring end else
  if type == "null"   then "N/A" else
  tostring end end
;
def scores_by_name:
(
[ "SCORE"
, "AGE"
, "CHANCE"
, "M1.MONN"
, "M1.VALE"
, "M1.TOTA"
, "M2.MONN"
, "M2.VALE"
, "M2.TOTA"
, "MG.MONN"
, "MG.VALE"
, "MG.TOTA"
, "NOM"
] | join("\t")),
(sort_by(.total) | reverse | .[] |
  [ .total
  , .age
  , .init
  , .before.stats.acoins
  , .before.stats.avalues
  , .before.stats.atotals
  , .after.stats.acoins
  , .after.stats.avalues
  , .after.stats.atotals
  , .stats.acoins
  , .stats.avalues
  , .stats.atotals
  , .name
  ] | map(show) | join("\t"))
;

def scores_by_age:
(
[ "AGE"
, "M.MONNA"
, "M.VALEU"
, "M.TOTAL"
] | join("\t")),
(debug |.[] |
  [ .age
  , .acoins
  , .avalues
  , .atotals
  ] | map(show) | join("\t"))
;

(.byname | scores_by_name)
, ""
, "Total valeurs: " + (.byname | map(.values) | add | tostring)
, "Total monnaie: " + (.byname | map(.coins)  | add | tostring)
, "Total points:  " + (.byname | map(.total)  | add | tostring)
# , "",
# (.byage[] | scores_by_age, "\n\n")
