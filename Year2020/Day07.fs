module AoC.Day07

open FSharpx.Collections
open FSharpPlus

let exampleInput = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."

let realInput = "shiny plum bags contain no other bags.
clear crimson bags contain 3 pale aqua bags, 4 plaid magenta bags, 3 dotted beige bags, 3 dotted black bags.
dim violet bags contain 5 bright brown bags.
mirrored tomato bags contain 3 faded maroon bags, 3 dark green bags.
muted salmon bags contain 1 posh yellow bag.
posh lime bags contain 1 dim lavender bag.
light fuchsia bags contain 5 faded coral bags.
plaid lime bags contain 1 dull brown bag, 4 clear black bags, 3 dotted coral bags.
dim crimson bags contain 2 striped blue bags.
drab salmon bags contain 3 plaid fuchsia bags, 1 mirrored teal bag, 4 posh aqua bags.
dark red bags contain 1 bright magenta bag, 1 posh lavender bag, 2 dark gray bags, 1 wavy lime bag.
striped indigo bags contain 2 drab brown bags.
vibrant beige bags contain 3 drab gray bags, 4 shiny gold bags, 4 dull white bags, 3 bright lavender bags.
pale maroon bags contain 1 pale crimson bag, 2 mirrored magenta bags.
dull cyan bags contain 4 mirrored green bags, 2 striped red bags, 1 clear blue bag, 5 muted gold bags.
clear brown bags contain 3 light orange bags, 2 striped red bags.
wavy white bags contain 3 bright purple bags, 2 posh lime bags, 1 faded crimson bag.
shiny green bags contain 4 dim red bags, 3 vibrant blue bags, 2 dotted plum bags.
dotted indigo bags contain 5 muted lime bags, 2 drab maroon bags, 2 bright tomato bags.
muted purple bags contain 3 pale chartreuse bags, 2 dim plum bags, 2 striped blue bags.
dotted magenta bags contain 1 mirrored maroon bag, 3 shiny red bags, 2 faded blue bags, 2 mirrored purple bags.
posh maroon bags contain 4 dull olive bags, 3 dark blue bags.
pale teal bags contain 4 posh lavender bags, 5 light lavender bags, 5 clear violet bags.
faded red bags contain 2 dotted lime bags.
bright lime bags contain 1 dim blue bag.
vibrant gold bags contain 4 dark violet bags, 1 faded crimson bag.
shiny cyan bags contain 5 clear beige bags, 1 wavy cyan bag.
shiny crimson bags contain 4 posh salmon bags, 5 dim fuchsia bags.
striped salmon bags contain 1 striped lime bag.
faded silver bags contain 5 dull blue bags.
dull crimson bags contain 1 dark bronze bag.
dull silver bags contain 5 light purple bags, 2 dim crimson bags, 2 plaid red bags.
vibrant plum bags contain 2 mirrored indigo bags, 4 pale chartreuse bags, 2 muted violet bags.
muted green bags contain 2 dull black bags, 1 mirrored green bag.
vibrant crimson bags contain 5 dark beige bags, 5 dull maroon bags, 5 drab lavender bags.
drab orange bags contain 2 posh silver bags, 2 dim olive bags, 1 plaid green bag.
striped silver bags contain 3 shiny indigo bags.
plaid bronze bags contain 2 dull silver bags.
striped yellow bags contain 4 dim tomato bags.
plaid beige bags contain 1 striped black bag, 2 wavy purple bags, 4 striped blue bags.
mirrored blue bags contain 1 dim green bag, 5 dark maroon bags, 5 plaid plum bags.
pale green bags contain 1 mirrored aqua bag, 2 mirrored indigo bags, 4 vibrant red bags.
plaid gold bags contain 4 wavy magenta bags.
vibrant olive bags contain 5 mirrored magenta bags, 1 plaid salmon bag, 3 bright white bags.
bright teal bags contain 5 bright fuchsia bags.
drab purple bags contain 3 muted lavender bags, 2 plaid gold bags, 5 muted green bags, 3 drab gold bags.
mirrored lime bags contain 3 light orange bags, 3 dim chartreuse bags, 5 shiny brown bags.
faded plum bags contain 3 light orange bags, 5 dotted orange bags, 2 striped bronze bags, 3 light aqua bags.
plaid indigo bags contain 5 bright cyan bags.
shiny blue bags contain 4 drab turquoise bags.
bright crimson bags contain 5 clear cyan bags, 2 pale maroon bags, 3 muted lavender bags.
dark purple bags contain 2 shiny brown bags, 1 posh aqua bag, 2 wavy gold bags, 4 mirrored teal bags.
striped crimson bags contain 3 shiny brown bags.
light crimson bags contain 5 dark white bags, 2 shiny lavender bags, 1 muted white bag.
dark violet bags contain 3 muted violet bags, 1 bright green bag, 2 dotted maroon bags.
striped white bags contain 1 light coral bag, 2 light brown bags.
dotted beige bags contain 5 light coral bags, 3 plaid black bags, 1 bright lavender bag, 5 posh green bags.
plaid plum bags contain no other bags.
striped red bags contain 3 plaid green bags.
light white bags contain 5 plaid teal bags, 5 faded tan bags.
clear purple bags contain 1 drab cyan bag, 2 shiny fuchsia bags, 4 dull beige bags.
clear beige bags contain 4 dim cyan bags, 4 clear gold bags.
clear indigo bags contain 2 faded beige bags, 5 shiny gold bags, 1 dark brown bag.
plaid coral bags contain 1 striped black bag.
wavy cyan bags contain 1 posh coral bag, 2 shiny black bags.
striped green bags contain 1 pale green bag, 1 striped red bag, 5 striped tomato bags, 4 clear tomato bags.
shiny salmon bags contain 1 bright silver bag, 1 faded gray bag, 1 muted lime bag, 5 vibrant chartreuse bags.
pale gray bags contain 1 drab gray bag.
vibrant tomato bags contain 5 dim turquoise bags, 1 pale blue bag, 2 striped brown bags, 3 plaid red bags.
dotted red bags contain 4 plaid black bags, 3 dotted blue bags.
faded aqua bags contain 1 striped turquoise bag, 1 dark tan bag.
wavy silver bags contain 5 pale cyan bags.
faded salmon bags contain 2 clear salmon bags, 1 plaid green bag, 2 shiny white bags, 1 pale chartreuse bag.
dull turquoise bags contain 1 clear violet bag.
plaid magenta bags contain 4 wavy cyan bags.
vibrant maroon bags contain 3 plaid plum bags.
striped turquoise bags contain 2 shiny lavender bags, 2 light aqua bags, 5 drab magenta bags.
wavy crimson bags contain 4 posh coral bags, 1 wavy lime bag, 1 plaid plum bag, 4 dull maroon bags.
wavy red bags contain 5 vibrant blue bags.
plaid silver bags contain 4 light salmon bags, 5 faded indigo bags, 3 clear magenta bags.
wavy salmon bags contain 5 dim olive bags, 3 posh magenta bags, 4 dark turquoise bags, 5 drab teal bags.
dark yellow bags contain 2 drab silver bags, 3 dim cyan bags, 3 clear olive bags, 3 dotted crimson bags.
striped brown bags contain 5 clear gray bags, 3 wavy salmon bags.
posh lavender bags contain 4 light teal bags, 4 wavy turquoise bags, 1 dim yellow bag.
muted beige bags contain 4 dull teal bags.
vibrant purple bags contain 5 dark silver bags, 2 striped gold bags.
bright yellow bags contain 2 dark blue bags, 2 bright brown bags.
dim tan bags contain 3 striped lime bags, 4 posh silver bags, 3 drab teal bags, 4 mirrored magenta bags.
drab chartreuse bags contain 3 shiny brown bags, 5 dark silver bags, 4 muted olive bags.
clear maroon bags contain 4 clear indigo bags.
posh orange bags contain 3 light silver bags, 3 clear black bags, 1 faded maroon bag, 5 wavy red bags.
plaid chartreuse bags contain 2 vibrant beige bags, 1 dull aqua bag, 3 clear blue bags, 2 wavy fuchsia bags.
dull beige bags contain 3 mirrored black bags, 4 drab gray bags.
clear fuchsia bags contain 1 posh olive bag, 4 wavy silver bags, 1 faded beige bag.
dim blue bags contain 4 muted lavender bags, 2 mirrored black bags, 5 dull white bags.
faded coral bags contain 5 drab teal bags, 2 plaid green bags.
shiny fuchsia bags contain 2 striped coral bags.
mirrored green bags contain 5 bright purple bags, 1 dim olive bag, 1 dark green bag.
muted gray bags contain 2 bright white bags, 4 mirrored turquoise bags, 4 plaid teal bags.
plaid blue bags contain 3 mirrored indigo bags.
bright maroon bags contain 5 vibrant aqua bags.
dark magenta bags contain 1 dull crimson bag, 3 clear orange bags, 2 plaid chartreuse bags.
dark coral bags contain 4 dull green bags.
wavy plum bags contain 3 plaid plum bags, 5 drab lavender bags.
faded indigo bags contain 4 shiny brown bags, 5 dotted salmon bags, 3 vibrant aqua bags.
dotted tomato bags contain 1 vibrant plum bag.
bright violet bags contain 4 dim yellow bags, 3 dark silver bags, 5 posh beige bags, 5 wavy lavender bags.
shiny chartreuse bags contain 3 bright brown bags, 2 dim cyan bags, 4 shiny brown bags, 1 clear black bag.
dim chartreuse bags contain no other bags.
bright turquoise bags contain 3 clear lime bags, 3 clear violet bags, 2 dotted maroon bags, 1 dark cyan bag.
plaid salmon bags contain 3 dark tomato bags, 5 light maroon bags.
drab tan bags contain 2 light tomato bags, 4 clear maroon bags, 1 dim olive bag, 5 dark teal bags.
dim green bags contain 3 muted aqua bags, 3 mirrored aqua bags.
drab yellow bags contain 1 clear chartreuse bag.
clear lavender bags contain 1 wavy salmon bag, 3 dull tan bags, 5 plaid magenta bags.
mirrored magenta bags contain 5 dark violet bags.
dotted white bags contain 2 muted blue bags, 1 light brown bag, 1 bright red bag, 3 posh aqua bags.
faded black bags contain 1 light violet bag, 5 muted aqua bags, 4 striped blue bags, 2 dull gray bags.
bright plum bags contain 3 dull white bags, 3 wavy maroon bags.
light gray bags contain 1 posh magenta bag.
dull orange bags contain 4 dotted chartreuse bags, 2 clear lavender bags, 4 pale silver bags, 5 shiny blue bags.
wavy purple bags contain 3 striped orange bags, 2 light aqua bags, 5 dull blue bags, 3 striped lime bags.
plaid green bags contain 1 dotted blue bag.
plaid purple bags contain 1 drab aqua bag, 4 dark bronze bags, 1 vibrant olive bag.
bright green bags contain 4 dim green bags, 2 dull aqua bags, 1 striped orange bag, 3 light teal bags.
posh cyan bags contain 5 pale orange bags, 5 faded chartreuse bags.
posh white bags contain 1 dark cyan bag, 1 dark magenta bag, 2 pale plum bags, 2 striped teal bags.
mirrored red bags contain 1 dotted violet bag, 4 dotted white bags, 4 faded tan bags, 4 wavy maroon bags.
dim turquoise bags contain 2 dark brown bags.
vibrant fuchsia bags contain 4 muted aqua bags, 1 light maroon bag.
light turquoise bags contain 5 bright cyan bags, 2 pale cyan bags.
striped coral bags contain 3 striped turquoise bags, 1 posh green bag, 1 dark brown bag.
dim black bags contain 2 posh coral bags.
mirrored orange bags contain 3 dull maroon bags, 5 bright purple bags, 2 striped turquoise bags.
clear cyan bags contain 5 wavy green bags, 4 faded coral bags, 4 muted purple bags.
muted aqua bags contain no other bags.
drab maroon bags contain 1 vibrant orange bag, 5 dotted white bags.
dim brown bags contain 1 dark plum bag, 5 light aqua bags, 5 striped orange bags, 3 vibrant aqua bags.
dim teal bags contain 3 pale gold bags, 4 dark teal bags.
pale white bags contain 2 shiny lavender bags, 2 clear gray bags, 3 pale purple bags, 5 striped yellow bags.
wavy black bags contain 5 wavy red bags, 2 vibrant bronze bags.
posh brown bags contain 4 shiny cyan bags.
bright bronze bags contain 2 plaid black bags, 3 mirrored gold bags, 4 drab silver bags, 4 striped orange bags.
shiny silver bags contain 5 muted gold bags, 4 light blue bags.
bright indigo bags contain 2 dotted gold bags, 5 vibrant red bags, 5 faded olive bags, 4 mirrored purple bags.
shiny violet bags contain 3 mirrored black bags, 2 bright maroon bags, 2 vibrant gold bags.
plaid crimson bags contain 1 plaid beige bag.
pale tomato bags contain 2 dark tomato bags.
striped bronze bags contain 5 bright magenta bags.
wavy violet bags contain 4 drab gray bags.
faded crimson bags contain 2 plaid plum bags, 5 vibrant aqua bags, 5 posh yellow bags, 4 bright lavender bags.
shiny aqua bags contain 4 faded silver bags, 2 dark maroon bags.
light lavender bags contain 3 dim chartreuse bags.
wavy orange bags contain 1 striped lime bag, 3 mirrored indigo bags, 2 vibrant plum bags, 4 dull tomato bags.
posh aqua bags contain 2 posh magenta bags.
pale black bags contain 2 drab green bags, 4 wavy olive bags, 4 plaid teal bags, 3 posh silver bags.
dotted crimson bags contain 1 pale gold bag, 5 dark brown bags, 4 dull aqua bags.
light orange bags contain 4 plaid green bags.
drab silver bags contain 5 wavy tan bags, 5 plaid tomato bags, 2 vibrant violet bags, 3 pale chartreuse bags.
vibrant red bags contain 4 light aqua bags, 4 striped orange bags, 5 dark blue bags, 3 faded green bags.
wavy olive bags contain 3 dull beige bags, 2 dim lavender bags, 1 striped gold bag.
dull tomato bags contain 3 vibrant violet bags, 1 shiny chartreuse bag, 4 plaid beige bags, 2 clear indigo bags.
dull coral bags contain 3 bright green bags, 4 dim purple bags.
faded magenta bags contain 3 drab olive bags, 2 faded maroon bags, 3 striped blue bags.
plaid yellow bags contain 1 faded brown bag, 1 faded gold bag, 5 drab fuchsia bags.
pale crimson bags contain 5 drab silver bags, 2 striped crimson bags.
plaid red bags contain 4 vibrant aqua bags.
vibrant bronze bags contain 4 shiny aqua bags.
muted crimson bags contain 3 vibrant chartreuse bags, 3 shiny fuchsia bags, 2 dull fuchsia bags, 4 striped brown bags.
bright lavender bags contain 4 muted aqua bags, 3 dim green bags.
dotted silver bags contain 1 striped white bag, 5 dark magenta bags, 2 clear green bags, 3 dim silver bags.
mirrored olive bags contain 5 drab turquoise bags, 2 dim orange bags, 5 dark aqua bags, 4 posh plum bags.
pale blue bags contain 2 posh green bags, 5 shiny lavender bags, 1 dim brown bag, 5 drab magenta bags.
striped purple bags contain 5 vibrant beige bags, 3 vibrant bronze bags.
wavy green bags contain 5 plaid plum bags, 2 muted blue bags, 5 drab gray bags, 2 posh magenta bags.
clear aqua bags contain 5 wavy green bags, 5 wavy maroon bags, 3 plaid salmon bags, 4 dark salmon bags.
striped aqua bags contain 4 mirrored purple bags.
pale lime bags contain 3 dull blue bags.
vibrant orange bags contain 5 light brown bags, 4 posh silver bags.
pale brown bags contain 4 striped yellow bags, 1 light salmon bag, 2 dark blue bags.
bright magenta bags contain 2 wavy cyan bags.
clear tan bags contain 5 dark gold bags.
pale beige bags contain 4 vibrant orange bags, 2 posh tomato bags.
mirrored gray bags contain 4 dull salmon bags.
faded lime bags contain 3 muted purple bags, 4 clear lime bags.
dark fuchsia bags contain 4 dull crimson bags, 1 vibrant cyan bag, 2 light lavender bags, 1 dark tomato bag.
shiny teal bags contain 3 mirrored tomato bags, 3 plaid coral bags, 2 shiny coral bags.
wavy magenta bags contain 1 vibrant cyan bag, 1 posh green bag, 4 vibrant aqua bags.
dotted bronze bags contain 1 clear orange bag, 2 dull lavender bags, 2 clear salmon bags.
plaid gray bags contain 1 shiny brown bag, 1 plaid turquoise bag, 3 faded silver bags, 2 mirrored white bags.
dark tan bags contain 1 light lavender bag.
bright orange bags contain 2 wavy gray bags.
light yellow bags contain 4 light blue bags, 3 muted blue bags, 1 plaid red bag, 3 mirrored aqua bags.
faded maroon bags contain 2 shiny brown bags, 4 drab magenta bags, 2 dotted maroon bags, 5 mirrored indigo bags.
dark aqua bags contain 2 bright gold bags, 3 plaid tomato bags.
striped gray bags contain 4 dark tomato bags.
bright olive bags contain 1 light gold bag, 4 faded coral bags, 5 dark brown bags, 4 faded maroon bags.
mirrored yellow bags contain 5 shiny silver bags, 5 dull violet bags, 5 drab silver bags, 5 pale lavender bags.
shiny gold bags contain 3 posh green bags, 2 dull white bags.
pale aqua bags contain 1 vibrant cyan bag, 2 posh gray bags, 3 faded beige bags, 2 dark gold bags.
light bronze bags contain 4 dotted black bags, 4 bright lavender bags, 2 plaid maroon bags.
plaid cyan bags contain 3 vibrant turquoise bags.
dull black bags contain 3 muted violet bags, 2 shiny brown bags, 4 dim chartreuse bags, 1 light lavender bag.
dotted brown bags contain 1 shiny white bag, 5 muted blue bags, 5 pale white bags, 3 bright gray bags.
clear turquoise bags contain 4 dark blue bags, 3 drab gold bags.
mirrored salmon bags contain 1 posh aqua bag, 2 dark gold bags, 4 dull black bags.
drab blue bags contain 2 plaid red bags, 3 wavy chartreuse bags, 3 posh salmon bags.
pale purple bags contain 1 wavy tan bag, 5 shiny lavender bags, 4 faded beige bags.
shiny lavender bags contain 2 dim olive bags, 3 vibrant aqua bags, 1 shiny plum bag, 1 dim cyan bag.
plaid brown bags contain 3 faded black bags, 5 wavy violet bags, 5 faded white bags.
faded tan bags contain 4 clear tan bags, 4 clear gold bags.
dim cyan bags contain 4 vibrant lime bags, 5 faded silver bags, 4 pale lime bags, 2 dim chartreuse bags.
vibrant green bags contain 5 plaid blue bags, 3 shiny maroon bags, 4 dotted violet bags.
drab turquoise bags contain 2 drab gray bags, 5 clear magenta bags.
bright coral bags contain 4 clear gold bags, 4 light coral bags.
shiny yellow bags contain 5 shiny chartreuse bags, 2 wavy green bags, 1 clear beige bag.
dim coral bags contain 2 shiny gray bags, 5 clear indigo bags, 2 vibrant plum bags.
pale orange bags contain 2 plaid tomato bags.
mirrored bronze bags contain 4 striped gray bags, 1 posh lavender bag, 2 wavy turquoise bags.
dim magenta bags contain 5 vibrant maroon bags, 5 mirrored fuchsia bags, 5 pale bronze bags, 2 dim brown bags.
plaid aqua bags contain 3 mirrored green bags.
mirrored indigo bags contain 2 vibrant lime bags, 2 clear salmon bags, 4 wavy magenta bags.
pale violet bags contain 1 clear salmon bag, 5 posh maroon bags, 4 posh plum bags.
bright white bags contain 1 muted blue bag, 2 wavy chartreuse bags, 2 pale turquoise bags, 5 plaid red bags.
faded lavender bags contain 2 light gold bags.
pale salmon bags contain 3 pale turquoise bags, 2 faded black bags, 5 wavy green bags.
vibrant teal bags contain 5 vibrant red bags, 1 dark silver bag, 2 pale white bags.
dark teal bags contain 4 dim plum bags, 4 mirrored white bags, 1 wavy gold bag.
dotted violet bags contain 1 clear beige bag.
vibrant black bags contain 5 dim violet bags.
bright gray bags contain 1 dull gray bag, 1 dark plum bag, 4 bright silver bags, 4 pale chartreuse bags.
light black bags contain 1 faded chartreuse bag.
muted coral bags contain 2 striped gray bags, 3 clear beige bags.
dark orange bags contain 3 mirrored teal bags, 5 dotted blue bags, 1 vibrant lime bag.
muted yellow bags contain 2 dim aqua bags, 4 vibrant indigo bags.
posh chartreuse bags contain 5 light blue bags, 4 faded chartreuse bags, 4 shiny black bags, 1 dim violet bag.
wavy maroon bags contain 5 muted gold bags, 4 posh yellow bags.
dim orange bags contain 1 faded gold bag.
dim aqua bags contain 4 wavy purple bags.
faded turquoise bags contain 5 bright violet bags, 3 pale purple bags, 4 faded maroon bags.
posh bronze bags contain 2 dim orange bags, 1 posh lavender bag.
vibrant white bags contain 5 muted aqua bags, 5 shiny turquoise bags.
clear blue bags contain 1 mirrored lavender bag, 2 dull violet bags.
striped teal bags contain 2 vibrant cyan bags.
striped tomato bags contain 1 dotted violet bag, 3 vibrant violet bags, 1 light beige bag.
muted maroon bags contain 2 clear red bags, 2 plaid chartreuse bags, 2 posh tomato bags.
dark bronze bags contain 5 dull white bags, 3 clear violet bags, 4 dark olive bags, 4 pale violet bags.
light blue bags contain 1 muted violet bag, 4 dark gold bags, 3 pale blue bags.
plaid white bags contain 3 striped orange bags, 3 light coral bags, 5 drab aqua bags.
vibrant cyan bags contain 4 dim fuchsia bags, 5 dull blue bags.
faded tomato bags contain 5 dim violet bags, 4 bright green bags, 3 bright teal bags.
wavy fuchsia bags contain 5 striped coral bags, 3 dark maroon bags, 5 muted aqua bags.
drab brown bags contain 5 wavy orange bags, 4 clear violet bags.
shiny olive bags contain 5 pale red bags, 1 bright purple bag, 2 dark plum bags.
mirrored fuchsia bags contain 4 dark violet bags, 2 faded crimson bags, 4 striped black bags.
clear olive bags contain 2 wavy magenta bags, 1 striped black bag, 5 pale fuchsia bags, 4 drab red bags.
dim yellow bags contain 2 faded blue bags, 2 shiny lavender bags, 5 shiny silver bags.
dark silver bags contain 4 light aqua bags.
plaid tomato bags contain 2 posh aqua bags, 2 striped turquoise bags, 3 plaid plum bags.
clear magenta bags contain 2 muted violet bags.
dotted orange bags contain 3 striped turquoise bags.
striped fuchsia bags contain 4 clear beige bags, 4 shiny crimson bags, 1 striped red bag, 4 shiny lavender bags.
clear gray bags contain 5 vibrant aqua bags, 1 light teal bag, 2 striped lime bags, 3 vibrant cyan bags.
dotted gold bags contain 4 drab gold bags, 2 faded tomato bags, 1 pale gray bag.
clear orange bags contain 3 mirrored plum bags, 1 dim aqua bag, 1 drab bronze bag.
vibrant blue bags contain 1 shiny brown bag, 5 shiny crimson bags.
pale plum bags contain 3 wavy olive bags, 5 pale lime bags, 3 plaid gold bags, 1 dim gold bag.
dim lavender bags contain 5 striped black bags, 2 vibrant lime bags, 4 bright red bags.
dull purple bags contain 1 dark tomato bag, 5 faded crimson bags.
vibrant tan bags contain 4 dim tomato bags, 4 vibrant violet bags, 5 pale olive bags, 2 posh aqua bags.
dull magenta bags contain 4 bright gray bags, 5 faded gold bags, 3 dotted yellow bags, 3 bright silver bags.
posh violet bags contain 5 vibrant indigo bags, 5 pale chartreuse bags, 2 dark green bags, 3 light blue bags.
dotted green bags contain 4 clear red bags, 5 drab aqua bags, 3 light black bags.
pale magenta bags contain 5 dark maroon bags, 3 mirrored aqua bags.
pale indigo bags contain 3 drab turquoise bags, 5 light violet bags, 5 clear magenta bags, 1 striped blue bag.
dotted lime bags contain 4 dull tomato bags, 5 dull yellow bags, 4 shiny gold bags.
posh fuchsia bags contain 2 pale orange bags, 4 posh coral bags, 1 drab brown bag.
light teal bags contain 3 faded green bags.
shiny lime bags contain 4 dotted blue bags, 5 light coral bags.
dull blue bags contain no other bags.
pale turquoise bags contain 2 pale blue bags, 5 dotted purple bags.
striped gold bags contain 2 wavy silver bags, 3 light purple bags, 3 dull gold bags, 1 dark coral bag.
vibrant chartreuse bags contain 5 mirrored tan bags, 4 vibrant blue bags, 1 clear teal bag, 2 dull indigo bags.
muted silver bags contain 1 dark beige bag.
shiny red bags contain 3 dim fuchsia bags, 3 wavy gold bags, 3 posh violet bags, 3 shiny silver bags.
mirrored chartreuse bags contain 1 wavy white bag.
light red bags contain 4 mirrored gold bags.
pale chartreuse bags contain 1 pale lime bag, 4 dim cyan bags.
bright aqua bags contain 5 bright yellow bags, 1 drab orange bag.
wavy lavender bags contain 5 dark white bags, 3 muted blue bags, 1 dotted salmon bag, 2 dull silver bags.
dotted purple bags contain 5 light aqua bags.
drab red bags contain 4 wavy green bags.
dull indigo bags contain 2 dark teal bags, 5 drab turquoise bags.
striped lime bags contain 3 dull blue bags, 2 shiny lavender bags, 2 muted aqua bags, 3 posh silver bags.
clear red bags contain 3 shiny fuchsia bags.
mirrored plum bags contain 1 muted fuchsia bag.
light chartreuse bags contain 3 mirrored salmon bags, 3 clear indigo bags, 1 striped coral bag, 1 plaid blue bag.
striped plum bags contain 3 pale violet bags.
light gold bags contain 2 dim fuchsia bags.
shiny white bags contain 5 dark indigo bags, 2 dim aqua bags, 5 vibrant aqua bags.
faded bronze bags contain 5 dim cyan bags.
pale red bags contain 2 mirrored magenta bags, 1 bright cyan bag, 2 vibrant lime bags.
muted chartreuse bags contain 2 bright chartreuse bags, 1 wavy gray bag, 1 pale lime bag, 5 light teal bags.
wavy chartreuse bags contain 4 bright fuchsia bags, 3 vibrant violet bags, 2 dull aqua bags.
dull lime bags contain 5 shiny lavender bags, 3 posh aqua bags.
vibrant magenta bags contain 5 striped yellow bags, 2 light tan bags, 5 shiny brown bags, 2 muted yellow bags.
muted blue bags contain 3 vibrant aqua bags, 2 dim fuchsia bags.
drab aqua bags contain 1 plaid plum bag, 1 posh yellow bag, 1 muted fuchsia bag, 4 muted indigo bags.
mirrored beige bags contain 5 wavy brown bags, 2 clear crimson bags, 2 dim gold bags.
light tan bags contain 5 light violet bags, 5 dim brown bags, 5 wavy turquoise bags.
faded fuchsia bags contain 5 drab brown bags, 2 light aqua bags.
dim salmon bags contain 4 shiny cyan bags, 4 faded olive bags, 3 dark maroon bags.
drab lavender bags contain 2 drab gray bags, 5 clear black bags, 1 shiny plum bag.
mirrored white bags contain 3 plaid plum bags, 5 muted coral bags, 1 clear gold bag.
dull aqua bags contain 3 shiny lavender bags, 1 muted aqua bag, 4 light purple bags, 4 shiny brown bags.
plaid maroon bags contain 4 clear lime bags, 1 muted violet bag, 4 vibrant teal bags.
clear salmon bags contain 2 striped blue bags, 1 dim chartreuse bag, 3 light purple bags, 2 posh silver bags.
dotted lavender bags contain 1 dark tomato bag, 2 striped turquoise bags, 3 dull gray bags.
light green bags contain 4 bright silver bags, 1 dim plum bag, 5 dark indigo bags, 5 dark blue bags.
plaid black bags contain 4 muted lavender bags, 5 muted violet bags, 3 dim olive bags, 5 bright maroon bags.
dull brown bags contain 3 dull green bags.
dull fuchsia bags contain 2 dotted blue bags, 4 vibrant bronze bags, 5 striped red bags.
light olive bags contain 3 clear beige bags, 3 bright maroon bags, 1 dim green bag.
faded beige bags contain 2 striped black bags, 5 light coral bags.
light coral bags contain 3 clear gold bags, 2 drab magenta bags, 2 pale lime bags.
shiny turquoise bags contain 4 dull olive bags, 1 pale purple bag, 5 striped bronze bags.
dark chartreuse bags contain 3 dotted beige bags, 1 dull silver bag, 3 posh lavender bags, 5 dotted blue bags.
shiny brown bags contain 5 plaid plum bags, 3 vibrant lime bags, 1 posh silver bag, 5 muted aqua bags.
dim gold bags contain 4 wavy magenta bags, 1 plaid turquoise bag, 3 drab maroon bags, 3 dark coral bags.
faded teal bags contain 2 dim turquoise bags, 4 faded beige bags.
dull lavender bags contain 3 shiny chartreuse bags, 4 posh salmon bags.
mirrored brown bags contain 4 vibrant cyan bags.
striped black bags contain 2 dull blue bags, 1 vibrant aqua bag, 1 dark maroon bag.
mirrored violet bags contain 3 vibrant crimson bags, 1 posh violet bag.
dark beige bags contain 3 mirrored gold bags.
clear white bags contain 2 striped gray bags.
dull tan bags contain 3 mirrored red bags, 2 plaid indigo bags, 3 bright gray bags.
dim tomato bags contain 1 faded beige bag, 2 dotted beige bags.
dark black bags contain 2 clear silver bags.
shiny magenta bags contain 5 plaid blue bags, 5 shiny aqua bags, 1 dull aqua bag.
light plum bags contain 1 dim black bag, 3 faded olive bags.
shiny beige bags contain 5 vibrant plum bags, 5 light blue bags, 2 light salmon bags, 3 wavy tan bags.
wavy gold bags contain 1 drab aqua bag.
vibrant lime bags contain no other bags.
vibrant turquoise bags contain 1 vibrant blue bag, 4 striped teal bags, 5 striped white bags.
dim maroon bags contain 4 striped lime bags, 2 light orange bags, 2 vibrant maroon bags.
posh olive bags contain 5 plaid tomato bags, 4 dark magenta bags, 4 faded chartreuse bags.
dim white bags contain 3 striped blue bags.
faded purple bags contain 1 dim brown bag, 3 dark orange bags, 2 posh silver bags, 5 muted lavender bags.
faded brown bags contain 5 faded coral bags, 1 striped turquoise bag.
posh beige bags contain 4 pale lime bags, 4 light violet bags.
shiny gray bags contain 2 shiny black bags, 5 striped white bags.
dotted salmon bags contain 3 pale lime bags, 3 muted lavender bags, 3 vibrant red bags.
vibrant brown bags contain 5 light gold bags, 3 light purple bags, 4 light blue bags.
faded white bags contain 1 dotted black bag.
wavy turquoise bags contain 4 dim crimson bags, 3 bright maroon bags, 3 pale cyan bags.
drab cyan bags contain 4 drab olive bags, 3 dim tomato bags, 2 muted indigo bags.
drab indigo bags contain 5 shiny bronze bags, 4 striped crimson bags, 5 light gold bags.
dim red bags contain 2 dull aqua bags, 3 mirrored aqua bags, 1 wavy red bag, 2 shiny crimson bags.
wavy coral bags contain 3 bright tan bags, 1 bright coral bag, 4 dull gold bags.
pale tan bags contain 2 clear blue bags, 3 dark bronze bags, 2 plaid coral bags, 3 vibrant magenta bags.
wavy lime bags contain 4 dull silver bags.
muted indigo bags contain 5 light purple bags.
clear bronze bags contain 4 clear silver bags, 3 shiny tan bags.
dull maroon bags contain 4 dark blue bags, 4 mirrored indigo bags.
clear yellow bags contain 3 plaid coral bags, 3 drab lime bags, 3 faded indigo bags.
drab green bags contain 5 faded purple bags.
plaid olive bags contain 5 faded silver bags, 4 dull purple bags, 4 dull yellow bags, 1 plaid salmon bag.
dark brown bags contain 1 clear gold bag, 5 light coral bags.
wavy tomato bags contain 4 striped teal bags.
dotted maroon bags contain 1 posh silver bag, 1 dark turquoise bag.
posh plum bags contain 2 bright red bags, 3 shiny maroon bags.
faded violet bags contain 1 faded green bag.
bright tan bags contain 3 shiny blue bags, 1 mirrored lime bag, 2 vibrant plum bags.
dim beige bags contain 1 light tan bag, 1 pale silver bag, 5 plaid silver bags.
clear plum bags contain 1 dark silver bag, 4 dull green bags, 3 shiny gray bags.
muted lavender bags contain 5 dim olive bags, 1 pale lime bag.
faded gold bags contain 1 striped coral bag, 3 light aqua bags.
faded orange bags contain 4 faded beige bags.
dim fuchsia bags contain no other bags.
dull green bags contain 5 dull purple bags, 3 bright maroon bags, 1 dark plum bag.
muted turquoise bags contain 5 shiny chartreuse bags, 1 mirrored lime bag.
striped cyan bags contain 3 striped gray bags, 4 dark brown bags.
faded cyan bags contain 4 striped purple bags.
shiny maroon bags contain 5 faded silver bags, 5 dark purple bags, 5 pale gold bags.
wavy tan bags contain 3 dim brown bags.
clear silver bags contain 5 clear blue bags, 1 dim chartreuse bag, 2 clear orange bags.
dull gold bags contain 3 drab aqua bags, 1 dim green bag.
wavy gray bags contain 2 striped bronze bags.
shiny purple bags contain 3 wavy lavender bags, 2 striped yellow bags.
posh salmon bags contain 1 dull black bag, 3 muted aqua bags, 4 muted fuchsia bags, 5 bright coral bags.
faded gray bags contain 2 faded orange bags, 1 striped brown bag.
shiny indigo bags contain 5 posh beige bags.
vibrant gray bags contain 3 pale brown bags, 2 shiny tomato bags, 5 mirrored silver bags, 3 striped tomato bags.
posh gray bags contain 4 striped black bags, 3 muted aqua bags, 4 mirrored gold bags.
mirrored cyan bags contain 1 muted lavender bag, 4 striped lime bags, 3 mirrored blue bags.
dull yellow bags contain 5 shiny brown bags, 5 clear maroon bags, 4 dim cyan bags.
mirrored black bags contain 2 mirrored aqua bags.
dim purple bags contain 4 vibrant teal bags, 5 shiny silver bags, 3 shiny brown bags.
drab beige bags contain 1 dim yellow bag, 1 vibrant lime bag, 2 muted plum bags, 5 posh violet bags.
striped chartreuse bags contain 2 posh gold bags, 1 striped lime bag.
dull chartreuse bags contain 1 plaid plum bag.
bright red bags contain 2 light lavender bags, 1 drab magenta bag.
drab crimson bags contain 2 pale chartreuse bags, 1 muted black bag, 4 striped yellow bags, 4 striped black bags.
wavy aqua bags contain 4 light tan bags, 3 dim maroon bags, 1 bright fuchsia bag.
clear gold bags contain no other bags.
dim lime bags contain 1 dull crimson bag, 1 mirrored orange bag, 1 light yellow bag, 1 muted fuchsia bag.
pale cyan bags contain 5 dull blue bags, 5 dark blue bags.
dim silver bags contain 3 bright aqua bags.
dark maroon bags contain 4 striped turquoise bags, 4 faded green bags, 3 dim fuchsia bags.
striped tan bags contain 2 dark violet bags, 2 muted indigo bags.
posh tomato bags contain 3 dark purple bags, 3 dim olive bags, 2 dotted white bags, 3 mirrored cyan bags.
light silver bags contain 1 plaid orange bag, 3 wavy salmon bags.
muted brown bags contain 1 plaid orange bag.
dim indigo bags contain 2 dark teal bags, 5 faded beige bags, 1 drab gray bag, 4 muted gold bags.
wavy brown bags contain 3 dotted salmon bags.
posh purple bags contain 2 wavy salmon bags, 1 faded tomato bag, 5 dark tan bags.
muted fuchsia bags contain 3 muted violet bags, 5 light purple bags, 4 dim green bags.
muted lime bags contain 2 vibrant beige bags.
muted black bags contain 5 pale blue bags, 1 vibrant beige bag, 4 pale lime bags, 2 vibrant cyan bags.
drab white bags contain 2 dim magenta bags, 5 vibrant bronze bags, 3 bright magenta bags.
bright chartreuse bags contain 5 dark white bags, 3 dotted olive bags.
wavy yellow bags contain 2 dotted salmon bags.
posh green bags contain 5 posh magenta bags, 2 light aqua bags, 3 wavy purple bags.
clear violet bags contain 3 mirrored salmon bags.
dull plum bags contain 4 bright coral bags.
posh yellow bags contain 4 dim aqua bags, 1 shiny brown bag, 3 striped orange bags.
dark salmon bags contain 4 light gold bags, 3 dotted white bags, 5 drab gray bags, 4 vibrant cyan bags.
posh blue bags contain 4 vibrant salmon bags, 2 clear tan bags, 5 light tomato bags, 1 wavy maroon bag.
drab bronze bags contain 5 plaid green bags, 4 striped turquoise bags, 2 shiny aqua bags, 3 bright plum bags.
dotted aqua bags contain 3 wavy olive bags.
dark lavender bags contain 2 dotted plum bags.
shiny tan bags contain 4 dotted turquoise bags, 4 pale violet bags, 3 plaid salmon bags, 1 striped gold bag.
shiny orange bags contain 3 dull purple bags, 1 clear green bag.
muted olive bags contain 2 dark orange bags.
dark tomato bags contain 5 vibrant lime bags.
vibrant silver bags contain 4 striped blue bags, 2 plaid gold bags.
dull bronze bags contain 3 faded coral bags, 1 clear chartreuse bag, 2 muted aqua bags, 3 wavy teal bags.
light tomato bags contain 3 dark blue bags, 5 mirrored salmon bags.
dotted cyan bags contain 2 dark gold bags, 4 clear gray bags, 2 dull aqua bags.
bright purple bags contain 4 striped coral bags.
dark white bags contain 3 dim brown bags, 1 mirrored gold bag, 1 striped white bag, 4 plaid black bags.
light magenta bags contain 4 vibrant olive bags, 5 clear lavender bags, 5 faded yellow bags.
drab violet bags contain 3 dotted blue bags, 2 dark plum bags, 3 dim silver bags, 5 vibrant olive bags.
posh turquoise bags contain 2 muted indigo bags, 2 striped white bags, 3 drab bronze bags, 4 dotted black bags.
plaid tan bags contain 3 vibrant bronze bags, 5 dull purple bags, 2 posh turquoise bags.
dull violet bags contain 4 vibrant cyan bags.
drab gray bags contain 3 pale lime bags, 3 bright green bags, 3 light lavender bags, 5 dull gray bags.
faded blue bags contain 5 muted lavender bags, 2 dim fuchsia bags, 3 clear salmon bags, 4 striped blue bags.
mirrored turquoise bags contain 5 faded purple bags.
wavy bronze bags contain 3 dim green bags, 2 muted indigo bags, 5 dotted tan bags.
light brown bags contain 3 bright purple bags, 4 vibrant lime bags.
bright brown bags contain 4 light coral bags, 3 clear gold bags, 2 striped turquoise bags, 4 dim fuchsia bags.
dotted turquoise bags contain 1 striped white bag, 4 dark magenta bags.
wavy teal bags contain 5 shiny lime bags, 2 dull cyan bags.
dark gold bags contain 5 dim green bags, 2 plaid red bags, 2 pale chartreuse bags.
dotted black bags contain 3 drab olive bags, 3 light teal bags.
wavy blue bags contain 4 clear red bags.
posh red bags contain 3 muted tan bags.
shiny black bags contain 5 light maroon bags, 4 vibrant cyan bags, 2 mirrored indigo bags.
plaid fuchsia bags contain 3 wavy magenta bags, 4 posh aqua bags, 3 posh salmon bags.
dotted olive bags contain 5 bright brown bags, 1 dotted salmon bag, 4 striped turquoise bags.
dull teal bags contain 2 pale purple bags.
posh indigo bags contain 1 shiny salmon bag.
clear coral bags contain 2 muted violet bags.
pale silver bags contain 2 drab olive bags, 5 wavy red bags.
light beige bags contain 2 muted lime bags.
striped orange bags contain no other bags.
dark gray bags contain 4 pale white bags, 3 pale blue bags, 5 dotted beige bags.
striped lavender bags contain 2 dull brown bags, 4 vibrant lavender bags, 1 vibrant aqua bag, 5 dull gold bags.
dark blue bags contain 5 clear gold bags, 5 faded silver bags.
plaid orange bags contain 1 shiny tomato bag, 1 light tomato bag.
muted bronze bags contain 2 clear gray bags, 5 shiny black bags, 5 shiny red bags, 2 muted blue bags.
light cyan bags contain 4 wavy crimson bags, 4 muted bronze bags, 4 clear lime bags, 3 dull yellow bags.
light salmon bags contain 4 muted aqua bags, 5 vibrant lime bags, 4 light aqua bags, 4 dim fuchsia bags.
dotted fuchsia bags contain 1 mirrored aqua bag.
drab tomato bags contain 2 vibrant blue bags, 1 pale chartreuse bag, 4 shiny black bags, 5 drab silver bags.
drab black bags contain 3 mirrored teal bags, 5 dull maroon bags.
posh black bags contain 4 pale violet bags, 5 plaid violet bags, 2 posh magenta bags.
vibrant aqua bags contain 1 faded silver bag.
mirrored tan bags contain 3 dotted white bags.
mirrored teal bags contain 3 shiny plum bags, 3 shiny brown bags, 3 striped turquoise bags, 2 bright red bags.
muted white bags contain 5 drab gray bags, 5 faded white bags, 3 vibrant beige bags.
bright beige bags contain 3 vibrant red bags, 4 posh silver bags.
dark indigo bags contain 2 dark violet bags, 2 dim plum bags, 1 mirrored indigo bag.
striped maroon bags contain 5 faded blue bags.
clear chartreuse bags contain 5 plaid plum bags, 1 plaid maroon bag, 1 dark crimson bag, 4 drab bronze bags.
dark olive bags contain 2 wavy purple bags, 4 shiny lime bags.
dim olive bags contain 5 faded silver bags, 5 shiny plum bags.
light indigo bags contain 5 dotted tomato bags, 1 dim orange bag, 3 mirrored orange bags, 3 pale cyan bags.
faded yellow bags contain 4 light red bags, 5 clear black bags, 2 dotted gold bags.
clear teal bags contain 2 light maroon bags.
drab lime bags contain 4 dim blue bags, 3 muted gold bags, 3 faded crimson bags.
dotted chartreuse bags contain 1 vibrant chartreuse bag, 3 posh red bags, 5 muted bronze bags, 4 dark brown bags.
drab olive bags contain 1 striped orange bag, 3 drab brown bags.
posh gold bags contain 3 plaid beige bags, 4 dim crimson bags, 2 dull black bags.
wavy indigo bags contain 3 bright brown bags, 3 pale cyan bags, 4 mirrored orange bags, 1 clear cyan bag.
bright black bags contain 1 muted crimson bag.
pale fuchsia bags contain 5 dull yellow bags, 4 bright chartreuse bags.
shiny coral bags contain 1 muted lavender bag, 5 muted purple bags, 1 striped orange bag.
faded chartreuse bags contain 4 striped turquoise bags, 1 wavy beige bag.
bright blue bags contain 3 vibrant olive bags.
faded olive bags contain 3 vibrant maroon bags, 4 wavy red bags, 2 shiny cyan bags, 4 wavy salmon bags.
clear green bags contain 5 dark turquoise bags, 4 posh cyan bags, 5 pale orange bags.
dim bronze bags contain 4 wavy orange bags, 2 bright magenta bags, 3 striped brown bags.
pale gold bags contain 3 dark purple bags.
wavy beige bags contain 2 wavy plum bags, 1 muted purple bag, 4 striped turquoise bags, 4 dull green bags.
plaid lavender bags contain 3 striped plum bags, 3 plaid orange bags.
bright silver bags contain 2 muted aqua bags.
muted magenta bags contain 5 faded olive bags.
drab magenta bags contain 4 dim chartreuse bags.
pale yellow bags contain 2 wavy tan bags, 4 striped cyan bags, 1 wavy salmon bag.
clear tomato bags contain 1 clear blue bag, 5 vibrant orange bags, 3 drab silver bags, 2 dim green bags.
dull gray bags contain 2 bright red bags, 3 striped lime bags.
dim gray bags contain 4 dark purple bags, 2 dim tomato bags.
mirrored crimson bags contain 4 pale gray bags, 1 muted chartreuse bag, 2 dotted orange bags.
drab gold bags contain 4 drab lavender bags, 3 light coral bags.
posh tan bags contain 5 vibrant gold bags, 1 dotted purple bag.
drab teal bags contain 3 shiny gold bags, 2 muted blue bags, 2 posh coral bags, 3 bright lavender bags.
pale olive bags contain 2 dull violet bags, 5 shiny maroon bags, 4 light red bags, 2 wavy cyan bags.
dotted coral bags contain 1 light blue bag, 2 plaid black bags.
light aqua bags contain 1 vibrant lime bag, 3 clear gold bags, 1 plaid plum bag, 5 shiny plum bags.
posh magenta bags contain 4 plaid plum bags, 2 vibrant lime bags, 5 light aqua bags, 2 dull blue bags.
posh coral bags contain 3 dotted salmon bags, 2 dim lavender bags, 4 wavy purple bags.
dark plum bags contain 2 vibrant aqua bags, 2 dim fuchsia bags, 4 dull blue bags.
posh teal bags contain 4 mirrored indigo bags, 3 striped purple bags, 5 dim cyan bags, 4 plaid silver bags.
vibrant salmon bags contain 3 faded lime bags.
vibrant violet bags contain 5 bright red bags, 3 shiny brown bags, 3 vibrant cyan bags.
plaid violet bags contain 2 vibrant violet bags, 4 pale brown bags.
dull red bags contain 1 dim aqua bag, 5 dotted magenta bags, 1 dotted gold bag, 2 shiny chartreuse bags.
vibrant coral bags contain 2 bright green bags.
muted cyan bags contain 4 dull white bags, 3 muted gray bags, 1 mirrored brown bag, 5 light maroon bags.
dotted tan bags contain 4 shiny indigo bags, 3 clear gold bags.
muted tomato bags contain 1 faded orange bag.
clear black bags contain 2 light aqua bags, 5 dull white bags.
bright fuchsia bags contain 5 dim brown bags.
striped olive bags contain 4 dim tomato bags.
dotted yellow bags contain 4 mirrored aqua bags, 4 faded indigo bags, 2 faded green bags.
pale bronze bags contain 3 plaid teal bags, 2 posh aqua bags, 2 dotted lime bags.
striped blue bags contain 4 dark tomato bags, 2 dim aqua bags, 1 dull olive bag.
dull salmon bags contain 1 light yellow bag.
muted tan bags contain 2 dim purple bags, 2 shiny coral bags, 2 drab bronze bags.
bright gold bags contain 5 shiny lavender bags, 4 dark maroon bags.
vibrant lavender bags contain 3 vibrant lime bags, 1 dark tomato bag, 2 dim fuchsia bags, 4 clear black bags.
bright tomato bags contain 4 clear blue bags, 2 wavy beige bags, 5 faded lime bags.
drab plum bags contain 4 bright olive bags, 1 posh lavender bag, 3 pale white bags, 2 dim green bags.
muted gold bags contain 3 posh magenta bags.
mirrored coral bags contain 3 dotted beige bags, 1 light magenta bag, 4 wavy turquoise bags.
mirrored purple bags contain 1 mirrored plum bag, 4 faded black bags, 3 bright violet bags, 1 vibrant yellow bag.
mirrored lavender bags contain 4 shiny violet bags, 4 dark violet bags, 3 drab gray bags, 3 plaid salmon bags.
muted teal bags contain 1 plaid turquoise bag, 5 light tomato bags.
plaid turquoise bags contain 2 posh aqua bags, 3 wavy plum bags, 3 dotted salmon bags.
pale coral bags contain 4 wavy lavender bags, 5 striped gray bags, 2 dotted turquoise bags, 4 striped violet bags.
pale lavender bags contain 4 vibrant lime bags, 1 dim plum bag, 1 posh salmon bag.
dark lime bags contain 4 dotted beige bags, 4 faded gold bags.
dotted teal bags contain 3 posh cyan bags.
striped magenta bags contain 4 light maroon bags.
light lime bags contain 2 light tomato bags, 2 bright cyan bags, 1 dotted white bag, 5 dark turquoise bags.
plaid teal bags contain 4 dim cyan bags, 2 muted black bags, 1 dark silver bag, 4 drab lavender bags.
vibrant yellow bags contain 4 mirrored teal bags, 2 shiny lime bags, 1 striped purple bag, 2 dotted beige bags.
dotted plum bags contain 1 drab lavender bag.
dotted blue bags contain 4 dull white bags, 5 dull olive bags.
posh crimson bags contain 1 wavy plum bag, 4 dim bronze bags.
muted orange bags contain 5 faded tomato bags, 1 dull magenta bag.
mirrored gold bags contain 4 dim fuchsia bags, 3 dull black bags, 5 shiny lavender bags, 5 dull gray bags.
bright cyan bags contain 4 pale blue bags.
dark cyan bags contain 2 dim olive bags, 2 faded crimson bags, 2 pale chartreuse bags.
striped beige bags contain 5 drab lavender bags.
dull olive bags contain 4 dark brown bags, 5 muted lavender bags, 4 plaid red bags, 1 dim green bag.
faded green bags contain 5 light aqua bags, 1 vibrant cyan bag, 5 striped orange bags.
shiny bronze bags contain 1 shiny purple bag, 5 striped indigo bags, 5 bright indigo bags, 5 striped yellow bags.
mirrored silver bags contain 2 pale tan bags.
dark green bags contain 4 striped white bags, 2 vibrant beige bags, 4 shiny aqua bags, 2 drab gray bags.
dotted gray bags contain 3 dotted violet bags, 5 muted beige bags, 4 posh yellow bags.
clear lime bags contain 5 faded crimson bags, 5 dark brown bags, 1 dim chartreuse bag, 5 bright fuchsia bags.
light maroon bags contain 5 light aqua bags.
striped violet bags contain 2 clear violet bags, 1 striped yellow bag, 5 dark lime bags.
mirrored maroon bags contain 2 dull purple bags, 3 clear black bags.
light purple bags contain 1 vibrant aqua bag, 4 striped turquoise bags, 4 dark blue bags, 3 dark maroon bags.
dark crimson bags contain 5 dark blue bags, 1 dim coral bag.
muted violet bags contain 3 striped turquoise bags, 3 vibrant lime bags.
shiny tomato bags contain 4 shiny plum bags.
drab fuchsia bags contain 2 faded beige bags, 3 light lavender bags.
dull white bags contain 5 wavy purple bags, 4 shiny lavender bags.
dim plum bags contain 4 dark brown bags, 3 shiny brown bags, 4 dim brown bags, 5 light maroon bags.
muted red bags contain 3 bright chartreuse bags, 2 shiny lime bags, 1 dotted olive bag, 3 shiny plum bags.
posh silver bags contain no other bags.
drab coral bags contain 4 faded white bags, 5 mirrored plum bags, 5 striped blue bags.
mirrored aqua bags contain 1 dark tomato bag, 2 dark brown bags.
vibrant indigo bags contain 5 dark silver bags, 3 clear lime bags, 1 dim gray bag.
bright salmon bags contain 4 bright crimson bags.
muted plum bags contain 4 mirrored lavender bags.
dark turquoise bags contain 5 striped turquoise bags, 4 dark blue bags, 5 posh yellow bags, 4 wavy purple bags.
light violet bags contain 3 clear black bags, 3 mirrored indigo bags, 5 striped coral bags, 2 dim crimson bags."

type BagColor = string

let parseBag (bag: string): BagColor * int =
    let withoutBags =
        if String.endsWith " bags" bag then
            bag.[..bag.Length - 6]
        else
            bag.[..bag.Length - 5]

    let firstSpaceIndex =
        String.findIndex (fun c -> c = ' ') withoutBags

    let numString = withoutBags.[..firstSpaceIndex]
    let num = int32 numString
    let color = withoutBags.[firstSpaceIndex + 1..]
    (color, num)


let parseLine (line: string): (BagColor * (BagColor * int) list) =
    let bagsFirstIndex = String.findSliceIndex " bags" line - 1
    let keyColor = line.[..bagsFirstIndex]
    let afterContainIndex = String.findSliceIndex "contain" line + 8

    let afterContain =
        line.[afterContainIndex..line.Length - 2]

    let items: (BagColor * int) list =
        if afterContain = "no other bags" then
            []
        else
            afterContain.Split ", "
            |> Seq.map parseBag
            |> List.ofSeq

    (keyColor, items)

type Rules = Map<BagColor, (BagColor * int) list>

let parse (input: string): Rules =
    input.Split "\n" |> Seq.map parseLine |> Map.ofSeq

let part1 (rules: Rules): int =
    let replaceWithContentUntilSG (rules: Rules): Rules =
        rules
        |> Map.map (fun parentColor replacements ->
            if parentColor = "shiny gold" then
                replacements
            else if List.exists (fun (childColor, _) ->
                        childColor = "shiny gold") replacements then
                replacements
            else
                replacements
                |> List.map (fun (childColor, n) ->
                    rules.[childColor]
                    |> List.map (fun (grandchildColor, nn) ->
                        (grandchildColor, n * nn)))
                |> List.concat)

    let rec replaceUntilFix (rules: Rules): Rules =
        let newRules = replaceWithContentUntilSG rules
        if rules = newRules then newRules else replaceUntilFix newRules

    replaceUntilFix rules
    |> Map.filter (fun k vs ->
        (k <> "shiny gold")
        && (List.exists (fun (child, _) -> child = "shiny gold") vs))
    |> Map.count

let part2 (rules: Rules): int =
    let rec go (destroyed: int) (bags: (BagColor * int) list): int =
        if List.isEmpty bags then
            destroyed
        else
            let (newDestroyed, newBags) =
                bags
                |> List.map (fun (bag, nBags) ->
                    let children = rules.[bag]
                    (nBags,
                     children
                     |> List.map (fun (childColor, childN) ->
                         (childColor, childN * nBags))))
                |> List.fold (fun (accD, accChildren) (newD, newChild) ->
                    (accD + newD, newChild ++ accChildren)) (destroyed, [])

            go newDestroyed newBags

    go 0 rules.["shiny gold"]


let main = realInput |> parse |> part2
