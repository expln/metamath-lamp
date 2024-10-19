### [DEV Version](https://expln.github.io/lamp/dev/index.html)
### Version 24
* 
### Version 23
* Implement the feature of Macros in the experimental mode (this allows to automate certain actions in mm-lamp using custom JS code). 
* Add more info to the Merge dialog to understand easier what statement to use https://github.com/expln/metamath-lamp/issues/188
### Version 22
* Implement the "Delete unrelated steps" feature in the Editor.
* Implement the "Rename hypotheses" feature in the Editor.
* Implement the "Auto unify all" feature in the Editor.
* Usability improvement: Add empty space at the bottom of the editor for longer proofs.
* Usability improvement: Allow sorting by "Number of new steps" in the Bottom-up prover dialog.
* Support variables in the pattern search https://github.com/expln/metamath-lamp/issues/13
* Set the default value of "Allow new variables" in the bottom-up prover to false https://github.com/expln/metamath-lamp/issues/180
### Version 21
* Bug fix: Unify hangs for certain statements/theorems https://github.com/expln/metamath-lamp/issues/175
### Version 20
* Allow opening multiple Explorer tabs by clicking a "search" button on a selected fragment.
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
