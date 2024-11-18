### [DEV Version](https://expln.github.io/lamp/dev/index.html)
### Version 26
* Enable automatic substitution search to minimize presses of the "Find Substitution" button.
* Add "Show unproved only" and "Expand all sources" buttons to the Proof Tree dialog.
* Bug fix: scrolling position changes "randomly" when switching between tabs.
* Allow selecting multiple steps in the editor by holding the Shift key.
* Add the option to include all hypotheses (even unused ones) in the compressed proof.
* Add the "Apply a substitution to all steps" button to the fragment selection actions.
* Add an additional pagination UI element at the bottom of the Explorer page.
* Make it possible to change default parameter values for the bottom-up prover.
* Fix a bug in "Find substitution by unification".
* Add a "Restore default settings" button to the Settings tab.
### Version 25
* Bug fix: Use spaces instead of commas to separate variables in the disjoints section of the editor. https://github.com/expln/metamath-lamp/issues/199
* Extend the [pattern search functionality](https://github.com/expln/metamath-lamp-docs/blob/master/explorer/search_by_pattern.md):
match assertions and hypotheses simultaneously; match assertion and hypotheses separately;
  adjacent symbols (https://github.com/expln/metamath-lamp/issues/57); exact match.
* Allow [opening a new Explorer tab](https://github.com/expln/metamath-lamp-docs/blob/master/explorer/multiple_explorer_tabs.md)
with empty search filters.
* Add possibility to [bookmark steps in the Editor](https://github.com/expln/metamath-lamp-docs/blob/master/editor/bookmark_steps.md). 
This allows to show only bookmarked steps.
* Add pagination in the Editor to simplify work with long proofs. Number of steps per page is configurable in the "View options" menu.
* Add a new [transform "Extract: X ⇒ ( ph -> X )"](https://github.com/expln/metamath-lamp-docs/blob/master/editor/transform_extract_for_deduction.md) 
which extracts a part of a step in a deduction proof.
* Add possibility to "[Delete unrelated steps and hypotheses](https://github.com/expln/metamath-lamp-docs/blob/master/editor/delete_unrelated_steps.md)" in the Editor.
* Allow [specifying steps for the "Allowed statements: first level"](https://github.com/expln/metamath-lamp-docs/blob/master/editor/steps_in_bottom_up_prover.md)
option of the bottom-up prover by selecting steps in the editor before invoking the bottom-up prover dialog.
* Implement the "[Inline proof](https://github.com/expln/metamath-lamp-docs/blob/master/editor/inline_proof.md)" in the editor.
* Make it possible to [move all selected steps](https://github.com/expln/metamath-lamp-docs/blob/master/editor/move_multiple_steps.md)
to the top or bottom in the Editor.
* Make it possible to [add assertions from the Explorer tab to the Editor](https://github.com/expln/metamath-lamp-docs/blob/master/explorer/add_assertions_to_editor_from_explorer.md). 
This is an equivalent of using the Search button in the Editor, but more handy in some cases.
* Add filtering by description in the Explorer. https://github.com/expln/metamath-lamp/issues/178
* Bug fix: An assertion tab crashes if the assertion proof contains errors. https://github.com/expln/metamath-lamp/issues/184
* Bug fix: Renumber fails if the goal label is a number. https://github.com/expln/metamath-lamp/issues/207
* Implement pagination in the Proof Explorer.
* Add "About this application" button on the Settings tab. https://github.com/expln/metamath-lamp/issues/72
* In the bottom-up prover dialog, add possibility to set "Allowed statements: other levels" equal to "Allowed statements: first level".
* Change the default value of "Allowed statements: first level" to None in the bottom-up prover dialog.
* Changes in the API of the Fragment Transform feature.
* Make it possible to run a macro from the UI.
* Migrate to ReScript 11. No functional changes.
### Version 24
* Add a new validation in the Editor: "Any statement must begin with a constant".
### Version 23
* Add more info to the [Merge dialog](https://github.com/expln/metamath-lamp-docs/blob/master/editor/merge_similar_steps.md) to understand easier what statement to use https://github.com/expln/metamath-lamp/issues/188
* Implement the feature of Macros in the experimental mode (this allows to automate certain actions in Metamath-lamp using custom JavaScript code). 
  In this version it is not possible to run a macro from the UI.
### Version 22
* Support variables in the [pattern search](https://github.com/expln/metamath-lamp-docs/blob/master/explorer/search_by_pattern.md) (https://github.com/expln/metamath-lamp/issues/13)
* Implement the "[Delete unrelated steps](https://github.com/expln/metamath-lamp-docs/blob/master/editor/delete_unrelated_steps.md)" feature in the Editor.
* Implement the "[Rename hypotheses](https://github.com/expln/metamath-lamp-docs/blob/master/editor/labels_for_hypotheses.md)" 
feature in the Editor.
* Implement the "Auto unify all" feature in the Editor.
* Usability improvement: Add empty space at the bottom of the editor for longer proofs.
* Usability improvement: Allow sorting by "Number of new steps" in the Bottom-up prover dialog.
* Set the default value of "Allow new variables" in the bottom-up prover to False https://github.com/expln/metamath-lamp/issues/180
### Version 21
* Bug fix: Unify hangs for certain statements/theorems https://github.com/expln/metamath-lamp/issues/175
### Version 20
* Always sort variables in disjoints according to the specified order https://github.com/expln/metamath-lamp/issues/172
* Allow opening [multiple Explorer tabs](https://github.com/expln/metamath-lamp-docs/blob/master/explorer/multiple_explorer_tabs.md) 
by clicking the "search" button on a selected fragment.
### Version 19
* Change the order in which assertions are selected by the prover https://github.com/expln/metamath-lamp/issues/163
* Bug fix: Bottom-up prover adds unnecessary disjoints https://github.com/expln/metamath-lamp/issues/166
* Usability improvement: Make it possible to manually add disjoints on mobile https://github.com/expln/metamath-lamp/issues/164
### Version 18
* Implement ["Fragment Transformers"](https://lamp-guide.metamath.org/#transformers-more-than-meets-the-eye) (https://github.com/expln/metamath-lamp/issues/121) 
### Version 17
* Implement rules how to work with `discouraged`/`deprecated`/`transitively deprecated` assertions https://github.com/expln/metamath-lamp/issues/31 https://github.com/expln/metamath-lamp/issues/111
* Add "Duplicate up" button in the Editor https://github.com/expln/metamath-lamp/issues/154
* Usability improvement: When substituting, always use the most recent selections https://github.com/expln/metamath-lamp/issues/155
* In the Explorer allow selection of final type https://github.com/expln/metamath-lamp/issues/111
### Version 16
* Preload existing proofs into the editor https://github.com/expln/metamath-lamp/issues/8
* For goal labels, apply same strict label validation rules as for hypotheses https://github.com/expln/metamath-lamp/issues/81
* Make it more obvious how to get a completed proof https://github.com/expln/metamath-lamp/issues/11
* Allow [merging multiple statements](https://github.com/expln/metamath-lamp-docs/blob/master/editor/merge_similar_steps.md) 
with one merge button click https://github.com/expln/metamath-lamp/issues/149
* Automatic [merge of duplicated statements](https://github.com/expln/metamath-lamp-docs/blob/master/editor/merge_similar_steps.md) (https://github.com/expln/metamath-lamp/issues/47)
* Usability improvement: Replacement fields should use selections consistently in the Substitution dialog https://github.com/expln/metamath-lamp/issues/82
* Full unification in the Substitution dialog https://github.com/expln/metamath-lamp/pull/147
* Add a "Reset editor content" menu item in the Editor https://github.com/expln/metamath-lamp/issues/21
### Version 15
* Automate creating of [labels for hypotheses](https://github.com/expln/metamath-lamp-docs/blob/master/editor/labels_for_hypotheses.md) (https://github.com/expln/metamath-lamp/issues/32)
* Make it possible to switch on/off parentheses autocomplete https://github.com/expln/metamath-lamp/issues/64
* Usability improvement: Open proof explorer by clicking refs in justifications https://github.com/expln/metamath-lamp/issues/99
* Usability improvement: Add delete (trash can) when editing Description, Variables, and Disjoints https://github.com/expln/metamath-lamp/issues/123 
* Usability improvement: Allow shrinking to a single symbol in fragment selector https://github.com/expln/metamath-lamp/issues/112
* UI improvement: In visualizations, don't repeat if conclusion is all constants https://github.com/expln/metamath-lamp/issues/115
* Bug fix: Alt+click doesn't edit label and step type when "long click" is enabled https://github.com/expln/metamath-lamp/issues/138
### Version 14
* The undo/redo feature in the editor https://github.com/expln/metamath-lamp/pull/133
### Version 13
* "paste" command for statement fragments https://github.com/expln/metamath-lamp/pull/130
* Add cancel button for modifying step type https://github.com/expln/metamath-lamp/pull/132
