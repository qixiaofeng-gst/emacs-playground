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
Design command rules:  
"
All commands are prefixed with C-c.
In this description, prefix is omitted. So [c] means [C-c c] actually.
Rules:
1. All file editing commands are single alphabets.
   Advanced (not so frequently used) file editing commands could be uppercase.
   [C-j] is an exception because it is used as a replacement of [RET] in emacs default.
2. All file operation (open, close, create, etc.) are [C-<alphabet>].
3. All layout, window, buffer operation are numbers or special symbols (-, +, |, /, DEL, RET, etc.).
"
; Naming conventions:
"
Function: prefixed with f7-
Command: prefixed with c6-
Variable: global ones prefixed with g5-
          local ones prefixed with l4-
Macro: prefixed with m4-
"

## Work log
; TODO Implement the project concept.
;      1. Make the qxf-mic-array-root (actually is work-root) changeable.
;      2. Way for search the project root, perhaps a hidden config file.
; TODO Sidebar for available buffers.
;      *. Side-bar content save and load.
;      1. Show opened file buffers.
;      2. Add [C-c <down>] and [C-c <up>] for editor switch.
;      3. Perhaps use blur hook of editor window to referesh the side-bar.
; TODO Varialble rename.
; TODO Made outline in function, macro and variable groups.
; TODO Investigate the batch mode.
; TODO Implement [C-c s] and [C-c r], convenient search, extract keyword at current point.
; TODO Auto jump to definition/header/declaration.

; DONE Hide the menu bar in qxf-general.el.
; DONE Create copy and paste logic.
; DONE Implement line movement.
; DONE Implement snippet insertion. e.g. command definition.
; DONE Make the editor 120+<number-columns>. {[function] count-lines start end}
; DONE Add a [C-c a] to jump to the line beginning.
; DONE Add a cmake command.
; DONE Make line copy. [C-c d]
; DONE Implement append-to-side-bar.
; DONE Assign [C-c 0] to 3 panel mode, but not jump to mic-array-root.
; DONE Assign [C-c 9] to 2 panel mode.
; DONE Know how to make data structure.
;      * property list,
;        * {[Function] plist-get plist property}
;        * {[Function] plist-put plist property value}
;        * {[Function] plist-member plist property}
;      * associate list,
;        * {[Function] assoc key alist &optional testfn}
;        * {[Function] rassoc value alist}
;        * {[Function] assq key alist}
;      * record, is a vector, first solt is value to use by {type-of}.
;        * {[Function] record type &rest objects}
;        * {[Function] make-record type length object}
; DONE Extract print-to-buffer.
; DONE Jump to nearest outmost bracket. [C-c b] and [C-c f].
; DONE Make the indent-sexp as I like: a brackets pair is not in same line have to be in same column. [C-c q]
; DONE Implement [C-c (] to auto insert ().
; DONE Implement [<backtab>]. Trim inner spaces(    ).
; DONE Implement [C-c j] to break with auto-format.
; DONE Take a look at package chapter. Extract private functions into qxf-utils.
; DONE Assign [C-c i] to quick insertion.
;      * template Load from file.
; DONE Show more information for entries in sidebar.
; DONE Implement [C-c "], [C-c [] and [C-c {] inputing pair shortcut.
; DONE Provide a function for retrieving lines(without \n) from string.
; DONE Implement file outline. List functions with sort and line numbers. [C-c |]
; DONE Implement function template.
; DONE Design naming convention for function, command, variable and macro.

; FIXED [C-c q] Spaces at line-end.
; FIXED [C-c f] printed a lot things.
; FIXED [C-c q] Error on line with only empty string.

; CANCELED Implement point history. [C-c .] and [C-c ,] to jump.
; CANCELED Use hook and mode to deal with confliction with c mode.


