### [DEV Version](https://expln.github.io/lamp/dev/index.html)
* Change the default value of "Allowed statements: first level" to None in the bottom-up prover dialog.
* Allow specifying steps for the "Allowed statements: first level" option of the bottom-up prover by selecting steps in the editor before invoking the bottom-up prover dialog.
* Add pagination in the Editor to simplify work with long proofs. Number of steps per page is configurable in the "View options" menu.
* Add possibility to bookmark steps in the Editor. This allows to show only bookmarked steps.
* In the bottom-up prover dialog, add possibility to set "Allowed statements: first level" equal to "Allowed statements: other levels".
* Changes in the API of the Fragment Transform feature.
* Add a new transform "Extract: X ⇒ ( ph -> X )" which extracts a part of a step in a deduction proof.
* Add possibility to "Delete unrelated steps and hypotheses" in the Editor.
* Make it possible to run a macro from the UI.
* Implement the "Inline proof" in the editor.
* Make it possible to move all selected steps to the top or bottom in the Editor.
* Bug fix: Use spaces instead of commas to separate variables in the disjoints section of the editor. https://github.com/expln/metamath-lamp/issues/199
* Add filtering by description in the Explorer. https://github.com/expln/metamath-lamp/issues/178
* Migrate to ReScript 11. No functional changes.
* Make it possible to add assertions from the Explorer tab to the Editor. This is an equivalent of using the Search button in the Editor, but more handy in some cases.
* Extend the search by pattern functionality: match assertions and hypotheses simultaneously; match assertion and hypotheses separately; 
  adjacent symbols (https://github.com/expln/metamath-lamp/issues/57); exact match.
* Allow opening a new Explorer tab manually.
* Bug fix: An assertion tab crashes if the assertion proof contains errors. https://github.com/expln/metamath-lamp/issues/184
* Implement pagination in the Proof Explorer.
* Bug fix: Renumber fails if the goal label is a number. https://github.com/expln/metamath-lamp/issues/207
* Add "About this application" button on the Settings tab. https://github.com/expln/metamath-lamp/issues/72
### Version 24
* Add a new validation in the Editor: "Any statement must begin with a constant".
### Version 23
* Implement the feature of Macros in the experimental mode (this allows to automate certain actions in Metamath-lamp using custom JavaScript code). 
  In this version it is not possible to run a macro from the UI.
* Add more info to the Merge dialog to understand easier what statement to use https://github.com/expln/metamath-lamp/issues/188
### Version 22
* Implement the "Delete unrelated steps" feature in the Editor.
* Implement the "Rename hypotheses" feature in the Editor.
* Implement the "Auto unify all" feature in the Editor.
* Usability improvement: Add empty space at the bottom of the editor for longer proofs.
* Usability improvement: Allow sorting by "Number of new steps" in the Bottom-up prover dialog.
* Support variables in the pattern search https://github.com/expln/metamath-lamp/issues/13
* Set the default value of "Allow new variables" in the bottom-up prover to False https://github.com/expln/metamath-lamp/issues/180
### Version 21
* Bug fix: Unify hangs for certain statements/theorems https://github.com/expln/metamath-lamp/issues/175
### Version 20
* Allow opening multiple Explorer tabs by clicking the "search" button on a selected fragment.
* Always sort variables in disjoints according to the specified order https://github.com/expln/metamath-lamp/issues/172
### Version 19
* Bug fix: Bottom-up prover adds unnecessary disjoints https://github.com/expln/metamath-lamp/issues/166
* Change the order in which assertions are selected by the prover https://github.com/expln/metamath-lamp/issues/163
* Usability improvement: Make it possible to manually add disjoints on mobile https://github.com/expln/metamath-lamp/issues/164
### Version 18
* ["Fragment Transformers"](https://lamp-guide.metamath.org/#transformers-more-than-meets-the-eye) https://github.com/expln/metamath-lamp/issues/121 
### Version 17
* Add "Duplicate up" button in the Editor https://github.com/expln/metamath-lamp/issues/154
* Usability improvement: When substituting, always use the most recent selections https://github.com/expln/metamath-lamp/issues/155
* In explorer allow selection of final type https://github.com/expln/metamath-lamp/issues/111
* Implement rules how to work with `discouraged`/`deprecated`/`transitively deprecated` assertions https://github.com/expln/metamath-lamp/issues/31 https://github.com/expln/metamath-lamp/issues/111
### Version 16
* For goal labels, apply same strict label validation rules as for hypotheses https://github.com/expln/metamath-lamp/issues/81
* Add a "Reset editor content" menu item in the Editor https://github.com/expln/metamath-lamp/issues/21
* Usability improvement: Replacement fields should use selections consistently in the Substitution dialog https://github.com/expln/metamath-lamp/issues/82
* Make it more obvious how to get a completed proof https://github.com/expln/metamath-lamp/issues/11
* Allow merging multiple statements with one merge button click https://github.com/expln/metamath-lamp/issues/149
* Preload existing proofs into the editor https://github.com/expln/metamath-lamp/issues/8
* Automatic merge of duplicated statements https://github.com/expln/metamath-lamp/issues/47
* Full unification in the Substitution dialog https://github.com/expln/metamath-lamp/pull/147
### Version 15
* Make it possible to switch on/off parentheses autocomplete https://github.com/expln/metamath-lamp/issues/64
* Automate creating of labels for hypotheses https://github.com/expln/metamath-lamp/issues/32
* Usability improvement: Open proof explorer by clicking refs in justifications https://github.com/expln/metamath-lamp/issues/99
* Usability improvement: Add delete (trash can) when editing Description, Variables, and Disjoints https://github.com/expln/metamath-lamp/issues/123 
* Usability improvement: Allow shrinking to a single symbol in fragment selector https://github.com/expln/metamath-lamp/issues/112
* UI improvement: In visualizations, don't repeat if conclusion is all constants https://github.com/expln/metamath-lamp/issues/115
* Bug fix: Alt+click doesn't edit label and step type when "long click" is enabled https://github.com/expln/metamath-lamp/issues/138
### Version 14
* The undo/redo feature in the editor https://github.com/expln/metamath-lamp/pull/133
### Version 13
* Add cancel button for modifying step type https://github.com/expln/metamath-lamp/pull/132
* "paste" command for statement fragments https://github.com/expln/metamath-lamp/pull/130
