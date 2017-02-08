def takewhile(p): [foreach .[] as $i (true; (. and ($i | p)); if . then $i else empty end)];
def dropwhile(p): [foreach .[] as $i (true; (. and ($i | p)); if . then empty else $i end)];
def span(p): [takewhile(p), dropwhile(p)];
def diff2: . as $arr | to_entries | .[1:] | map(.value - $arr[.key - 1]);
def average: length as $n | if $n == 0 then nan else add / $n end;
def elem($elt): map(select(. == $elt)) | length == 1;
