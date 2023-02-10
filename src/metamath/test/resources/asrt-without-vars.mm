$c 0 + = -> ( ) term wff & => |- [ { ] } <. .> T. $.
$v t r s P Q $.
tt $f term t $.
tr $f term r $.
ts $f term s $.
wp $f wff P $.
wq $f wff Q $.
tze $a term 0 $.
tpl $a term ( t + r ) $.
weq $a wff t = r $.
wim $a wff ( P -> Q ) $.
a1 $a |- ( t = r -> ( t = s -> r = s ) ) $.
a2 $a |- ( t + 0 ) = t $.
${
   min $e |- P $.
   maj $e |- ( P -> Q ) $.
   mp  $a |- Q $.
$}
th1 $p |- t = t $=
   tt tze tpl tt weq tt tt weq tt a2 tt tze tpl
   tt weq tt tze tpl tt weq tt tt weq wim tt a2
   tt tze tpl tt tt a1 mp mp
 $.

${
   h1 $e |- ( t = r ) $.
   h2 $e |- ( r = s ) $.
   asrt-without-vars  $a |- T. $.
$}