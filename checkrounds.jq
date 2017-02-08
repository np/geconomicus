#!/usr/bin/jq -Mf
(.[0].rounds | length) as $n |
{result:   $n
,outliers: map({name,rounds: .rounds | length | select($n != .)})
}
