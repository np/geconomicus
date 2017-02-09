#!/usr/bin/jq -Mf

# First round of life at 16yo.
# Time span 8 years.
def gen2age: (2 + .) * 8
;

# include "nplib";
def takewhile(p): [foreach .[] as $i (true; (. and ($i | p)); if . then $i else empty end)];
def dropwhile(p): [foreach .[] as $i (true; (. and ($i | p)); if . then empty else $i end)];
def average: length as $n | if $n == 0 then nan else add / $n end;
def elem($elt): map(select(. == $elt)) | length == 1;

def span2(p):
    takewhile(p) as $x
  | dropwhile(p)
  | [[$x[], .[0] // empty], .[1:]]
;

def onrounds:
    map(.dcoins)  as $dcoins
  | map(.dvalues) as $dvalues
  | map(.dcoins + .dvalues) as $dtotals
  | { dcoins:  $dcoins
    , dvalues: $dvalues
    , acoins:  ($dcoins  | average)
    , avalues: ($dvalues | average)
    , dtotals: $dtotals
    , atotals: ($dtotals | average)
    }
;
def generation:
    .rounds
  | length as $max
  | takewhile(.flags | elem("death") | not)
  | length | ($max - .)
;
def stats:
    .[0].coins as $init
  | [(.[] | select(.flags | elem("death"))), .[-1]] as $twolifes
  | ($twolifes | map(.coins)  | add) as $coins
  | ($twolifes | map(.values) | add) as $values

  # Here we compute $coins and $values by adding the delta.
  # Then we check that they match.
  | (map(.dcoins)  | add) as $coins2
  | (map(.dvalues) | add) as $values2
  | (if $coins == $coins2 then . else {coins: $coins, coins2: $coins2} | debug end) as $anon
  | (if $values == $values2 then . else {values: $values, values2: $values2} | debug end) as $anon

  | span2(.flags | elem("death") | not) as [$before, $after]
  | onrounds as $stats
  | { init:    $init
    , coins:   $coins
    , values:  $values
    , total:   ($coins + $values)
    , before:  { rounds: $before, stats: ($before | onrounds) }
    , after:   { rounds: $after,  stats: ($after  | onrounds) }
    , stats:   $stats
    }
;
map( generation as $gen
   | (.rounds | length) as $maxgen
   | .age = ($gen | gen2age)
   | .rounds |= ( . as $rounds
                | to_entries
                | map( .key as $round
                     | .value
                     | (if $round == 0 then null else $rounds[$round - 1] end) as $prev
                     | ($prev and ($prev.flags | elem("death"))) as $newborn
                     | (if $newborn then null else $prev end) as $prev
                     | .dcoins  = .coins  - ($prev.coins  // 0)
                     | .dvalues = .values - ($prev.values // 0)
                     | .round   = $round
                     | .age     = (($gen + $round) % $maxgen | gen2age)
                     )
                )
   )
| { byname: map(. + (.rounds | stats))
  , byage: map(.rounds[]) | group_by(.age)
         | map(.[0].age as $age | length as $size | onrounds | .age = $age | .size = $size)
  }
