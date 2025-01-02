const mmpText1 = `
$( <MM> <PROOF_ASST> THEOREM=syllogism LOC_AFTER=

hd1::syllogism.1 |- ( ph -> ps )
hd2::syllogism.2 |- ( ps -> ch )

* !              |- ( ph -> ( ps -> ch ) )
* !              |- ( ( ph -> ps ) -> ( ph -> ch ) )
!d3::              |- &W1
!d5::              |- &W2
!d6::ax-2              |- ( &W2 -> ( &W1 -> ( ph -> ch ) ) )
d4:d5,d6:ax-mp          |- ( &W1 -> ( ph -> ch ) )
qed:d3,d4:ax-mp     |- ( ph -> ch )

$)
`

const parsed = parseMmp(mmpText1)
console.log("parsed", parsed)

await loadMmpTextToEditor(mmpText1)
await mmj2Unify()
