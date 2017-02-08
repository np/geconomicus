def takewhile(p): [foreach .[] as $i (true; (. and ($i | p)); if . then $i else empty end)];
def dropwhile(p): [foreach .[] as $i (true; (. and ($i | p)); if . then empty else $i end)];
def span(p): [takewhile(p), dropwhile(p)];
def map2(f): . as $arr | to_entries
      | map( (if .key == 0 then null else $arr[.key - 1] end) as $prev
           | { curr: .value, prev: $prev} | f);
def diff2: map2(.curr - (.prev // 0));
def average: length as $n | if $n == 0 then nan else add / $n end;
def elem($elt): map(select(. == $elt)) | length == 1;
