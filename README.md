# emacs-playground
A playground for emacs editor.

## Usage
`git clone <project-url> ~/.emacs.d`  
Then run emacs.

Prepare markdown package:  
https://jblevins.org/projects/markdown-mode/markdown-mode.el  
`sudo apt install pandoc`  
Check documentation for windows: https://pandoc.org/installing.html#windows  

Prepare zenburn theme:  
https://github.com/bbatsov/zenburn-emacs

## Design

### Rules:
1. All commands are prefixed with C-c.  
   In this description, prefix is omitted. So [c] means [C-c c] actually.
1. All file editing commands are single alphabets.  
   Advanced (not so frequently used) file editing commands could be uppercase.  
   [C-j] is an exception because it is used as a replacement of [RET] in emacs default.
1. All file operation (open, close, create, etc.) are [C-<alphabet>].
1. All layout, window, buffer operation are numbers or special symbols (-, +, |, /, DEL, RET, etc.).

### Naming conventions:
* Function: prefixed with f7-
* Command: prefixed with c6-
* Variable: global ones prefixed with g5-  
            local ones prefixed with l4-
* Macro: prefixed with m4-

## Work log
### TODO
* Update opened-buffers-record but not overwrite.
* Conveniently open buffer throw opened-buffers-record.
* Implement the project concept.
  1. Make the qxf-mic-array-root (actually is work-root) changeable.
  1. Way for search the project root, perhaps a hidden config file.
* Sidebar for available buffers.
  1. Side-bar content save and load.
  1. Show opened file buffers.
  1. Add [C-c <down>] and [C-c <up>] for editor switch.
  1. Perhaps use blur hook of editor window to referesh the side-bar.
* Varialble rename.
* Made outline in function, macro and variable groups.
* Investigate the batch mode.
* Implement [C-c s] and [C-c r], convenient search, extract keyword at current point.
* Auto jump to definition/header/declaration.

### Done
* Hide the menu bar in qxf-general.el.
* Create copy and paste logic.
* Implement line movement.
* Implement snippet insertion. e.g. command definition.
* Make the editor 120+<number-columns>. {[function] count-lines start end}
* Add a [C-c a] to jump to the line beginning.
* Add a cmake command.
* Make line copy. [C-c d]
* Implement append-to-side-bar.
* Assign [C-c 0] to 3 panel mode, but not jump to mic-array-root.
* Assign [C-c 9] to 2 panel mode.
* Know how to make data structure.
  * property list,
    * {[Function] plist-get plist property}
    * {[Function] plist-put plist property value}
    * {[Function] plist-member plist property}
  * associate list,
    * {[Function] assoc key alist &optional testfn}
    * {[Function] rassoc value alist}
    * {[Function] assq key alist}
  * record, is a vector, first solt is value to use by {type-of}.
    * {[Function] record type &rest objects}
    * {[Function] make-record type length object}
* Extract print-to-buffer.
* Jump to nearest outmost bracket. [C-c b] and [C-c f].
* Make the indent-sexp as I like: a brackets pair is not in same line have to be in same column. [C-c q]
* Implement [C-c (] to auto insert ().
* Implement [<backtab>]. Trim inner spaces(    ).
* Implement [C-c j] to break with auto-format.
* Take a look at package chapter. Extract private functions into qxf-utils.
* Assign [C-c i] to quick insertion.
  * template Load from file.
* Show more information for entries in sidebar.
* Implement [C-c "], [C-c [] and [C-c {] inputing pair shortcut.
* Provide a function for retrieving lines(without \n) from string.
* Implement file outline. List functions with sort and line numbers. [C-c |]
* Implement function template.
* Design naming convention for function, command, variable and macro.

### Fixed
* [C-c q] Spaces at line-end.
* [C-c f] printed a lot things.
* [C-c q] Error on line with only empty string.

### Cancelled
* Implement point history. [C-c .] and [C-c ,] to jump.
* Use hook and mode to deal with confliction with c mode.

