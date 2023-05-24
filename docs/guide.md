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
This guide provides a brief explanation of
how to use metamath-lamp, focusing on the various actions you can take.

## Starting metamath-lamp

You don't need to install anything to run metamath-lamp.
Just use you web browser and view the
**[Metamath-lamp web site](https://expln.github.io/lamp/latest/index.html)**.

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

At the top there is a tab bar with two tabs, "Settings" and "Editor".
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
  using Alt-left click (or Opt-left click) to change "P" (provable assertion)
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
* Magnifying glass: search for an existing statement in the current context;
  the selected one (if any) will be added as a new statement.
* A arrow: Apply a substitution to all selected statements.
* Network: Unify. If no statements are selected, it will attempt to unify
  all statements to create a proof. If a statement is selected, it will
  open a dialogue to start a bottom-up search for a proof.

