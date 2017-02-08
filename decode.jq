#!/usr/bin/jq -RsMf
def decoderound:
  select(. != "") | split(" ")
  | {coins: .[0] | tonumber, values: .[1] | tonumber, flags: .[2:]}
;
def decodeplayer:
  split("\n")
  | { name: .[0]
    , rounds: .[1:] | map(decoderound)
    }
;
split("\n\n") | map(decodeplayer)
