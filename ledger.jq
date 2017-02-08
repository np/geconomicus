#!/usr/bin/jq -rMf
.[] | .name as $name | .rounds | to_entries[] | .key as $round | .value |
"2017/01/\($round + 1)\n  (\($name))  = \(.coins) C\n  (\($name))  = \(.values) V\n"
