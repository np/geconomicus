#!/usr/bin/jq -rMf
def pad($n): . + " " * ($n - length);
def show:
  if type == "number" then if floor == . then tostring else . * 100 | floor | . / 100 | tostring end else
  if type == "null"   then "N/A" else
  tostring end end
;
def tablerow($n): map(pad($n + 1)) | join("| ") | ("| " + . + "|");

def table($headers; $n):
   ($headers | ( tablerow($n),
                 (map("-" * $n)    | tablerow($n))
               )),
   (.[] | map(show) | tablerow($n));

def scores_by_name:
  sort_by(.total) | reverse |
  map(
    [ .total
    , .name
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
    ]) |
  table(
    [ "SCORE"
    , "NOM"
    , "AGE DEPART"
    , "CHANCE"
    , "M1.MONNAIE"
    , "M1.VALEURS"
    , "M1.TOTAL"
    , "M2.MONNAIE"
    , "M2.VALEURS"
    , "M2.TOTAL"
    , "M.MONNAIE"
    , "M.VALEURS"
    , "M.TOTAL"
    ]; 10)
;

def scores_by_age:
    map(.total = (.dtotals | add))
  | sort_by(.total) | reverse
  | map(
    [ .total
    , .age
    , (.dcoins  | add)
    , (.dvalues | add)
    , .acoins
    , .avalues
    , .atotals
  # , .size
    ])
  | table(
      [ "SCORE"
      , "AGE"
      , "T.MONNAIE"
      , "T.VALEURS"
      , "M.MONNAIE"
      , "M.VALEURS"
      , "M.TOTAL"
    # , "TAILLE"
      ]; 9)
;

  "Résultats de l'équipe"
, "---------------------"
, ""
, "Total valeurs : " + (.byname | map(.values) | add | tostring)
, "Total monnaie : " + (.byname | map(.coins)  | add | tostring)
, "Total points :  " + (.byname | map(.total)  | add | tostring)
, ""
, "Résultats individuels"
, "---------------------"
, ""
, (.byname | scores_by_name)
, ""
, "Résultats par tranche d'age"
, "---------------------------"
, ""
, (.byage | scores_by_age)
