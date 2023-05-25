# User guide for metamath-lamp

Metamath-lamp (Lite Assistant for Metamath Proofs) is
a proof assistant for creating formal
mathematical proofs in the [Metamath system](https://us.metamath.org/).
Unlike most other Metamath proof systems
(such as mmj2 or original metamath-exe),
users can use this proof assistant without installing anything;
you can simply run it directly using your web browser.
More information on metamath-lamp is available at the
[metamath-lamp source code repository](https://github.com/expln/metamath-lamp).

This is a user guide for metamath-lamp.
We'll begin with how to start metamath-lamp and a "quick start"
of the basics of using it.
This will be followed by a brief example (proving that two plus two
equals four).
After that we'll cover the various parts of the user interface
(providing a basic reference guide).
In the future we hope to follow this information with more examples.

Note that metamath-lamp changes over time, so some of this guide
may not exactly match what you see. If you see a difference, please
let us know so we can fix this user guide.

## Starting metamath-lamp

You don't need to install anything to run metamath-lamp.
Just use you web browser and view the
**[Metamath-lamp web site](https://expln.github.io/lamp/latest/index.html)**.

## Quickstart

To use metamath-lamp, you:

* Load the proof context (the databases you'll use and their scope).
* Set the fundamental proof information (its
  description, variables, and disjoints).
* Add the goal ("qed") and any hypotheses to the list of statements.
* Now create the proof.
  To do this, you add other statements and repeatedly unify them
  until the goal is completely proven.
  You can do it bottom-up, top-down, middle-out, whatever makes sense to you.
* Copy the compressed proof of the goal into the clipboard.
  You can do this by selecting the green checkmark next to the goal
  to show the compressed proof, then press copy.
  You'd typically copy that compressed proof into a Metamath database
  (text file).

Throughout metamath-lamp there are various tooltips.
So if you hover an iteractive item, in most cases the tool will provide a brief
explanation of what that item does.
You don't need to memorize this user guide!

This software works on many different systems which have different conventions.
On some Macintosh keyboards the "Enter" key is instead labelled "Return" and
the "Alt" key is instead labelled "Opt" or "Option".
On a touchscreen (like a smartphone), a quick touch and release on a control
is the same as a left click.
Metamath-lamp has some actions that are quickly accessed using Alt+left click
(hold down the Alt aka Opt key, and while holding it use left click)..

## Simple demo: Proving that 2 + 2 = 4

Let's first show using metamath-lamp to create a simple proof, namely,
that 2 + 2 = 4.

We first need to decide on the proof context, that is, the database(s)
of axioms and proven theorems we'll use. In this case we'll use the most
common metamath database, `set.mm`. This database uses the very common starting
points of classical first-order logic and ZFC set theory.
We'll also tell it to *stop* using the database just before its
proof of 2 + 2 = 4, which in this database is named `2p2e4`.
If we included the existing proof, the prover
would simply reuse that existing proof.

> Select Source type "Web", Alias "set.mm:latest"; after confirmation this
> loads the given database.
> Now under scope select "Stop before" and enter the label "2p2e4".
> Finally, apply changes to the context.

For this example we'll leave the proof description, variables, and disjoints
blank. We do need to tell metamath-lamp our goal.

> In the Editor select "+"  (add new statement). Enter
> `|- ( 2 + 2 ) = 4`
> and press Enter (Return) to save the result.
> Be sure to surround each symbol by at least one space, and the
> parentheses are not optional.

For our purposes, we're going to rename our goal with the label "qed".
Renaming the target statement (as well as any other statement) is 
not required; you could just continue using the
label metamath-lamp suggested. But renaming some of the statements 
(especially the target one) makes them easier to distinguish for you
and eventually this name will appear in the final proof.

> Select the statement number (1) using the checkbox to the left of it.
> Change its name to "qed" and press Enter (Return).

Now we need to figure out how to prove this.
Metamath-lamp can actually some things automatically, but we will
*intentionally* avoid some of those automations to see how to
prove something in cases where the automations can't do enough.

There are many different ways to create a proof, including top-down,
bottom-up, or even middle-out. Metamath-lamp supports them all.
Also, note that there are often many proofs for the same true statement.
Here we're going to show one way to do it, as an example.

In many cases we can prove a statement by identifying definitions of
what we want to prove, finding their expansions, and repeatedly
expanding and simplifying the
results to show that what we want to prove is correct.

In this case, we want to prove that something is 4, so the definition
of 4 would probably be useful.
We'll add a statement, and search for the definition of 4.

> Select the magnifying glass (search) icon; under pattern enter
> `4 =` and click on Search.
> Select the statement labelled `df-4` and press "Choose Selected".
> You will now have a new statement:
> `|- 4 = ( 3 + 1 )`

This definition of 4 depends on the definition of 3, so let's add
the definition of 3 as well.
Note that `df-4` is the definition of 4; this suggests a naming convention,
so we can probably just use the naming convention to find it.

> Select the magnifying glass (search) icon; in the "label" field
> enter `df-3` and click on Search.
> Select the statement labelled `df-3` and press "Choose Selected".
> You will now have a new statement:
> `|- 3 = ( 2 + 1 )`

We can connect the definition of 4 using the definition 3 by simply
adding 1 to both sides of the definition of 3.
We can simply add this statement as claim and see if metamath-lamp
can find a statement that proves this correct (in this case it can).
We don't want to add this statement as the *last* statement,
so we'll select the last statement before adding it (so we'll
insert that statement before it).

> Select the checkbox to the left of the "qed" statement.
> Then select "+" (add new statement).
> Notice that because a statement was selected, the new statement will
> be inserted before "qed".
> Enter the new statement
> `|- ( 3 + 1 ) = ( ( 2 + 1 ) + 1 )`
> and press Enter (Return).
> Unselect the checkbox to the left of the "qed" statement.
> Now press unify (the multiple-connected dots symbol); since there
> was no specific statement selected, it will try to justify all statements.
> Metamath-lamp will succeed in finding a justification for our new statement,
> so it will show a green checkmark next to our new statement.

We could later on connect this proof of `( 3 + 1 )` to the number 4.
However, in a more complex proof we might forget that we were trying
to prove an expansion of a value in the goal (4 in this case).
So let's "clean up" now by directly proving that this term is an
expansion of a symbol in the goal. Instead of typing it all in, we'll
use the "duplicate" command to get us started:

> Select the checkbox to the left of the new statement
> `|- ( 3 + 1 ) = ( ( 2 + 1 ) + 1 )`
> and press the "duplicate" icon (double circles behind a "=").
> This will create a duplicate statement below the current one.
> Click on the new statement text, and change `( 3 + 1 )` to 4; once you have
> `|- 4 = ( ( 2 + 1 ) + 1 )`
> press Enter (Return).
> Press unify, which will produce a green checkmark next to all the statements
> except our final "qed" statement.

Our goal involves showing that the symbol `2` and `4` have some kind
of relationship. A common technique to create proofs is to expand
the definitions of terms and then show that their expansions are equivalent.
We've already expanded `4`, let's now expand `2`.

> Select the magnifying glass (search) icon; in the "label" field
> enter `df-2` and click on Search.
> Select the statement labelled `df-2` and press "Choose Selected".
> You will now have a new statement:
> `|- 2 = ( 1 + 1 )`

This definition of `2` is similar to the value we expanded for `4`.
Both have a `1` followed by another `1` at their end.
We can take the definition of `2` and add `2` to both sides, at the
beginning of each side, to produce a very similar expression.
Let's try that.

> Select the checkbox to the left of the "qed" statement.
> Then select "+" (add new statement).
> Enter the new statement
> `|- ( 2 + 2 ) = ( 2 + ( 1 + 1 ) )`
> and press Enter (Return).
> Unselect the checkbox to the left of the "qed" statement.
> Now press unify (the multiple-connected dots symbol); since there
> was no specific statement selected, it will try to justify all statements.
> Metamath-lamp will succeed in finding a justification for our new statement,
> so it will show a green checkmark next to our new statement.

At this point we've shown that `4` and `( 2 + 2 )` are separately
equal to very similar expressions. If we could prove that those expressions
are equal to each other, we could trivially prove our goal.
Let's try to do that.

> Select the checkbox to the left of the "qed" statement.
> Select "+" (add new statement). Enter the new statement
> `|- ( ( 2 + 1 ) + 1 ) = ( 2 + ( 1 + 1 ) )`
> and press Enter (Return).
> Unselect the "qed" statement.
> As an experiment, select Unify with no statement selected;
> you'll see that in this case nothing happens.

It's actually true that
`( ( 2 + 1 ) + 1 )' is equal to `( 2 + ( 1 + 1 ) )`.
That's because addition is associative
(you can do the first or second addition first and the result is the same).
The Metamath database in this context already has a proof that
addition is associative, too.

However, when you press "unify" without selecting any statements,
metamath-lamp cannot automatically prove this new statement.
The problem isn't that the database doesn't have this already proven.
In fact, the Metamath database in this context
*does* have a proof that addition is associative.
However, the rule in this Metamath
database requires some preconditions we haven't included in our proof.

So we'll instead use a bottom-up search, which will try to find and
prove other any other statements necessary to apply a relevant existing proof.

> Select the checkbox next to our latest statement
> `|- ( ( 2 + 1 ) + 1 ) = ( 2 + ( 1 + 1 ) )`
> and press "Unify".
> A new dialogue will display titled "Proving bottom-up".
> These options control how metamath-lamp will search for a proof
> of ths statement. For now, we'll just accept the defaults and press the
> "Prove" button at the bottom of the dialogue.
> After a moment it will present a list, and one of the first options
> (probably the first one) should use "addassi".
> The theorem "addassi" is a pre-existing theorem showing that
> addition is associative.
> This requires multiple lines, because using this associativity
> theorem requires showing that `1 and `2` are complex numbers.
> Use the checkbox to its
> left to select that one, then press the "Apply Selected" button.

Suddenly a lot has happened. Notice that we now have a green checkmark
next to our new statement, because we've proven it.
We now have new statements that have been automatically added to our proof,
namely that `1 e. CC` (`1` is a complex number) and `2 e. CC`
(`2` is a complex number).

Most importantly, the final statement "qed" has a green checkmark, which
means we have proven our goal. This happened because after we applied the
new statement, metamath-lamp automatically unified all the statements,
and was able to complete the rest of the proof.

We can now show the compressed proof.

> Select the green checkmark (*not* "P") on the last ("qed") statement.
> You can select "Copy" to copy the compressed proof into the clipboard.

Now that we've seen a simple demo of metamath-lamp, let's
walk through its entire user interface.

## Loading source Metamath databases to create the proof context

Before creating a mathematical proof using metamath-lamp, you must
first load at least one Metamath database and decide how much of those
database(s) to use. This creates the context of a proof - that is,
the set of axioms, proven theorems, and so on that the proof is allowed to use.

One of the powerful benefits of the Metamath system is that it
impose any particular set of axioms (such as those of logic or set theory).
However, to create a proof, you need to start with *some* set of axioms,
and typically you'll want to build on other proofs that use those axioms.
So we must first tell metamath-lamp what this proof is allowed to use.

When metamath-lamp starts, it will tell you that no database/context is loaded.
Select the "source type" of the database, which is:

* "web" - load from the world wide web.
   You then need to pick an alias (which database on the web).
* "local" - load from a local file.
   You then need to choose the file.

Most users will just choose "web" and use "set.mm:latest". This is the
current version of set.mm (aka the Metamath Proof Explorer), which is based
on classical logic and ZFC set theory. This is the largest Metamath database.

Confirm as necessary. Once it's loaded, you'll need to pick a scope.
A metamath database is a sequence of statements, and metamath-lamp
will *only* let you use statements that are in scope. The scope options are:

* "Read all" - use all statements in the source.
* "Stop before" (label) - use all statements up to but *not* including
  the given label. If you want to practice re-proving some statement, use
  this and give the label of what you want to prove yourself.
  Trying to prove something *already* proven is a great way to learn how
  to use metamath-lamp.
* "Stop after" (label) - use all statements up to and *including* the
  given label. If you want to use statements up to some label and not beyond,
  this is how to do that.

You can select "+" to load another source, and the adjacent trash can
to remove a source. In most cases you won't load another source.
A common use for adding another source
is if you're using a public Metamath database as a starting
point, but have your own private collection of definitions and proofs
in your local file storage.

Once you've selected all sources, select "Apply changes" to process these
source databases. After it's applied, the source selection section
is hidden and you can start creating a proof with the proof editor.

## Main tabs: Settings and Editor

Once you've loaded the context,
at the top there is a tab bar with two tabs, "Settings" and "Editor".

The "Editor" tab is the main view that lets you see and edit a proof.
The "Settings" tab lets you change the editor configuration to your liking,
We'll cover the Settings tab later; let's focus on the Editor tab.

## Editor tab

The Editor tab lets you edit a proof; it starts empty.
You will create a list of statements in the editor that will eventually
result in a proof.

### How to state the goal and hypotheses

To prove something, you must first tell the system what to prove and
any special hypotheses to use. To do that:

* Under the "Editor" tab", press the "+" in the editor command tab bar
  to create a new statement. Enter the goal of the proof.
  Typically the goal will begin with the symbol "|-" which means
  "it is true that".
  Click on its step number (1) if you want to rename the step name (typically
  this is named "qed").
* If there are hypotheses, press "+" to enter each one, and select the "P"
  using Alt+left click (or Opt+left click) to change "P" (provable assertion)
  into "H" (hypothesis). Put them in the intended order by selecting them
  (the box on the left) and selecting the up and down icons.
  You generally want the goal last.

You're now ready to create a proof. Let's first look at the editor command's
tab bar.

### Editor command tab bar

The Editor tab has another tab bar with a variety of icons for commands.
You can hover over an icon to see what the command does. Here are their
icons and meanings:

* Box: Select or deselect all current statements.
* Up and Down: Move the selected statements up or down in their list.
* "+": Add a statement (which you then type in).
* Trash can: Delete the selected statement(s).
* Duplicated "+": Copy the selected statement.
* Merge: Merge the selected statements (they must be similar).
* Magnifying glass: search for a statement pattern in the current context.
  The selected pattern one (if any) will be added as a new statement.
  See below for more about search patterns.
* A arrow: Apply a substitution to all selected statements.
* Network: Unify. If no statements are selected, it will attempt to unify
  all statements to create a proof. If a statement is selected, it will
  open a dialogue to start a bottom-up search for a proof.

Under the editor command tab bar is basic information about the proof
(such as its description).

### Basic information about the proof

The basic information about the proof are the proof's description,
variables, and disjoints. Click on the *section name* to edit this
information. You can also select the editable fields, but
description is odd - by default, you have to use Alt+left click to edit it,
while just left click selects part of its text.

Here's some about those fields:

* Description: The description of the proof.
  Note: This description is *not* currently generated as a comment to be
  inserted into a database.
* Variables: A list of variables.
* Disjoints; A list of variables that are disjoint.

<!--
Omitted, since description is currently not generated as a comment:

  in the final Metamath database just before the proof.
  If you are following the conventions of `set.mm`, the first sentence
  should be an English description of what is proved. Surround
  Metamath statements with backquotes (so they can be typographically formatted)
  and precede references to another with an isolated "~".
  Conventionally this includes, at its end, a statement like
  "(Contributed by NAME, DD-MMM-YYYY)" where DD-MMM-YYYY is the date
  the proof was completed and MMM is the 3-letter English name
  of the month.
-->

Under the basic information about the proof
are a list of statements in the proof.

### List of statements in the proof

The list of statements (aka steps) of the proof follows the basic information
about the proof.

By default, when the tool begins there will be no statements.
Typically the first statement to be added is the statement
to be proved (aka the *goal*).
Use "+" in the editor command bar to add the goal.
Usually the goal is the last statement, though metamath-lamp does
not enforce this.

Each statement is presented in the following left-to-right order:

* Box (selector): Select this box to select or unselect this statement.
  Many commands work on the "currently selected statement(s)",
  so it's important to be able to select them al.
  Use the box in the editor command bar to select or deselect all statements.
* Green Checkmark (if present): If there's a green checkmark following the
  selector box, a recent unification has
  confirmed that this statement is proven given its context and its
  previous statements. Any modification of a proof removes the checkmarks.
  To regenerate the checkmarks,
  select "unify" (without selecting any particular statement), which will
  re-verify the statements and show checkmarks for the
  statements that are proven.
  Once you see a checkmark, you can see a compressed metamath proof of
  that step by selecting its checkmark (generally you would do this on
  the goal step). Once there, you can show or hide the proof table,
  as well as showing only essential steps.
  Non-essential steps are the steps showing how to create syntactic structures
  and show that they are of the correct types.
* Id: This is the id for this statement.
  You should give your proof's goal the id of what you intend to name it.
  Consider naming your goal "qed" if you don't know what name to use.
  Each hypothesis needs to have a unique id that isn't already in the
  database are using.
  If you're following the conventions of set.mm, the name of each hypothesis
  is the goal name followed by a period and an integer (starting with 1).
  For example, the proof of "mp3an3an" might have hypotheses
  with ids "mp3an3an.1" and "mp3an3an.2". Note that this is different
  from the convention of the mmj2 tool,
   where hypotheses have id names of "h1" and so on.
  All other statements are typically
  consecutive integers starting with 1, though they don't need to be;
  you can use any sequence of alphanumerics for an id as long as it's
  not already in the database.
  The point of the id is to provide a simple way to refer to a statement.
* P/H: This is "P" if it's a statement to be proven, and
  "H" if the statement is a hypothesis, Typically all hypothesis are listed
  first. By default, left-clicking on a "P" will
  reveal or hide the specific justification
  for the proved step (if any). By default, using Alt+left click will show a
  dialogue to let you select if this is a "P" or "H" statement type.
* Statement: This is the actual statement. In most cases this will start
  with "|-" (meaning "it is true that..."), followed by a space-separated
  sequence of symbols of the statement to be proved. An example of a statement
  is `|- ( 2 + 2 ) = 4` (two plus two equals four).
  You can edit the statement. By default, you can do this by clicking
  on the text with the mouse left button or by touching it
  using a touchscreen.
  Once you're done, press the disk icon to use the edited statement, or
  the cancel icon to not change the statement.
  You can also select *parts* of a statement; by default you can do this
  by using Alt+left click ("alt" is sometimes labelled "opt").
  For more about selecting parts of a statement, see the next section.

### Selecting parts of a statement

What we've shown so far is enough to create any proof.
However, it's very common when creating a proof to want to copy
*part* of a statement. Therefore, metamath-lamp has mechanisms to
make selecting *parts* of a statement very easy, especially in the presence
of parentheses-like constructs.

By default, Alt+left click enables selecting part of a statement
(on some Mac keyboards "alt" is labelled "opt").
If you'd prefer a different mechanism, use the settings tab to change this.

Once you've done this, a selector dialogue will appear under the statement.
You can easily select a part of the statement and modify the selection.
In particular,
you can select parentheses-like characters to select the expression
begun or ended with them.

You can the use the selector dialogue as follows:
* Expand selection: Expand the selection to the next largest syntactic unit.
* Shrink selection: Reduce the selection to the next smallest syntactic unit.
* Add new statement above: Create a new statement above the current statment,
  and make its contents the selected statement.
* Add new statement below: Create a new statement below the current statment,
  and make its contents the selected statement.
* Copy to clipboard. Note that you can later create or edit statement,
  and copy from the clipboard.
* Edit: Start editing with the current text selected.
* Close: Close this statement part selection dialogue box.

### Search patterns

The "magnifying glass" icon enables you to search for a statement
that matches a given pattern.

The search pattern language is very simple,
Note that search will *only* match on the conclusion part
of an axiom or theorem (there is currently no mechanism to search hypotheses).
A pattern should consist of a space-separated sequence of one or more constants.
Statements will only be considered matches if their conclusion part has
the same constants in the same order, with optionally 1 or more other
symbols before the pattern, between the requested constants,
and after the pattern.

Therefore, a search for `0 ->` will match the conclusion
`|- ( ( 0 <_ A /\ 0 <_ B ) -> 0 <_ ( A + B ) )`
because the conclusion has a `0` constant which is later followed by a
`->` constant.

### Replacement

TODO

### Proving bottom-up

If you select one statement and then select unify, you'll enter a
"proving bottom-up" dialogue. This lets you search for a way to prove
the selected statement bottom-up using the current context.
You can change the search options then select "prove" to start the search
(or select cancel to cancel). Proving bottom-up can take a long time,
depending the problem and the speed of the computer.
It essentially works backwards to find a match, first with a single level,
then with 2 levels (a statement that depends on another that also requires
proving), and so on.

This dialogue has the following options:

Root statements ("first level" and "other levels"):

Label: If set, this is the sole ("root") statement to use as a starting
point. Note that this is set if the system previously found a justification
for this statement using this justification.

Search depth: How deep the search is to go.
A single statement is depth 1, a statement that requires 1 other
statement to be also proved is depth 2, and so on. 
The default search depth value is 4. Larger numbers enable more automation
but generally take exponentially more time.

Length restriction: This controls the interpretation of search depth.

Checkbox Allow new disjoints:

Checkbox Allow new statements:

Checkbox Allow new variables:

Logging level (0..2):
If the logging level is more than 1, you may enter the maximum number
of branches.

You can speed up searches by not allowing new disjoints, new statements,
and/or new variables, but in some cases this may mean a proof won't be
found.

TODO

## Settings

The "Settings" tab lets you configure metamath-lamp to your liking.

It's important to remember that any changes you make in the
Settings tab are *not* applied until you select
"Apply Changes"; if you want to discard changes, select "Discard Changes".

TODO

## More examples

In future versions of this user guide we hope to have more examples.

TODO

## Conclusion

Metamath-lamp is intended to be an easy-to-use proof assistant.
We'd love contributions and feedback.
