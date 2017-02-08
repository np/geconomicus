#!/usr/bin/jq -Mf
include "nplib";

def span2(p):
    takewhile(p) as $x
  | dropwhile(p)
  | [[$x[], .[0] // empty], .[1:]]
;
def onrounds:
    (map(.coins)  | diff2) as $dcoins
  | (map(.values) | diff2) as $dvalues
  | (map(.coins + .values) | diff2) as $dtotals
  | { dcoins:  $dcoins
    , dvalues: $dvalues
    , acoins:  ($dcoins  | average)
    , avalues: ($dvalues | average)
    , dtotals: $dtotals
    , atotals: ($dtotals | average)
    }
;
def age:
    .rounds
  | length as $max
  | takewhile(.flags | elem("death") | not)
  | length | (2 + $max - .) * 8
;
def stats:
    .[0].coins as $init
  | [(.[] | select(.flags | elem("death"))), .[-1]] as $twolifes
  | ($twolifes | map(.coins)  | add) as $coins
  | ($twolifes | map(.values) | add) as $values
  | span2(.flags | elem("death") | not) as [$before, $after]
  | onrounds as $stats
  | { init:    $init
    , coins:   $coins
    , values:  $values
    , total:   ($coins + $values)
    , before:  { rounds: $before, stats: ($before | onrounds) }
    , after:   { rounds: $after,  stats: ($after | onrounds) }
    , stats:   $stats
    }
;
map( age as $age
   | .age = $age
   | .rounds |= ( . as $rounds
                | to_entries
                | map( .key as $round
                     | .value
                     | .round = $round
                     | .age = (($age + $round * 8) % 80))))
| { byname: map(. + (.rounds | stats))
  , byage: map(.rounds[]) | group_by(.age) | map(onrounds)
  }
