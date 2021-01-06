(print 'world)			  ; Perhaps we only have line comment?

(list 1 2 3)
(list nil t)				;
(list 'a 'b)
(list a b)
(cons 'foo '(4 5 6))
(setq foo nil)
(setq a 'a)
(setq b 'b)
(list foo a b)
(list 'foo)
(progn
  (setq first-list '(1 2 3))
  (length first-list)
)
(car first-list)
(setq second-list (list 1 2 3))
(car second-list)
(booleanp second-list)
(booleanp foo)
(prin1 'foo)
(princ "hello world")
(+ 1 2 3 4)
(- 1 2 3 4)

(emacs-version)
(eval 'foo)
(eval 'emacs-build-time)
(list emacs-version)

(eval "Hello world")
(eval -1)
(eval #$)
(fixnump 1)
(bignump 1)
(eq 1 1)
(eval ?Q)
(eval ?a)
(eval ?\a)
(eval ?\N{LATIN SMALL LETTER A WITH GRAVE})
(+ ?a ?b)
(length "Soulmate")
(eval "\xe0\ ")
(eval [1 2 3])

; Access symbol slots:
(symbol-function 'car)
(symbol-name 'cons)

(type-of 'car)
(type-of 1)
(type-of [1 2 3])
(type-of (symbol-function 'car))
(eval :test)

; Simplest demo for function definition:
(/ 1.0 2)
(defun qxf-sqrt
    (a)
  (progn
    (setq a (/ a 2.0))
    (/ a 2.0)))
(qxf-sqrt 1)

; Simplest demo for flat workflow:
(progn
  (setq x '(#1=(a) b #1#))
  (eq (nth 0 x) (nth 2 x)))

(progn
  (setq x '((a) b (a)))
  (eq (nth 0 x) (nth 2 x)))

(progn
  (setq x '#1=(a #1#))
  (eq x (cadr x))
  x)

; Type predictors:
(atom x)
(arrayp x)
(bool-vector-p x)
(booleanp x)
(bufferp x)
(byte-code-function-p x)
(case-table-p x)
(char-or-string-p x)
(char-table-p x)
(commandp x)
(condition-variable-p x)
(consp x)
(custom-variable-p x)
(floatp x)
(fontp x)
(frame-configuration-p x)
(frame-live-p x)
(framep x)
(functionp x)
(hash-table-p x)
(integer-or-marker-p x)
(integerp x)
(keymapp x)
(keywordp x)
(listp x)
(markerp x)
(mutexp x)
(nlistp x)
(number-or-marker-p x)
(numberp x)
(overlayp x)
(processp x)
(recordp x)
(sequencep x)
(string-or-null-p x)
(stringp x)
(subrp x)
(symbolp x)
(syntax-table-p x)
(threadp x)
(vectorp x)
(wholenump x)
(window-configuration-p x)
(window-live-p x)
(windowp x)
(zerop 0)
(eq (type-of x) 'cons)

; Special type predictor:
(null nil)
(atom nil)

; Demonstration for 'let' special form:
(let ((x 1) (y 2) (z 3))
  (list x y z)
  (type-of x)
  (type-of '(x y z)))

(= 12 12.0)
(defvar hello-var 1e-6)
(defun apx-equal (x y)
  (or (= x y) (< (abs (- x y)) hello-var)))
(apx-equal 1 1.00000001)
(defun qxf-sqrt (x)
  (let ((result 0) (top x) (bot 0))
    (if (= result 0) (prin1 top) (prin1 bot))
    result))
(list (sqrt 2) (qxf-sqrt 2))
(format "%%%d %6d %s" 1.1 20 ?a)

(let ((the-ring (make-ring 10)))
  (dotimes (i 10) (ring-insert the-ring (format "item-%d" i)))
  the-ring)

; Demonstration for 'dolist' loop:
(dolist (i '(1 3 5 7 13 35 57 713 1335 3557 57713 7131335))
  (prin1 i))

; Demonstration for 'while' loop:
(let ((i 0)) (while (< i 10) (print i) (setq i (1+ i))))

; Access system time:
(float-time)
(symbol-plist 'defun)

; Package basic (load-path):
(eval 'load-path)
(nth 1 load-path)
(safe-length load-path)
(let ((num 0)) (while (< num (safe-length load-path))
		   (print (nth num load-path)) (setq num (1+ num)))
    num)

; Learn the two special escaped keys (\C- and \M-):
(print "\M-x | Cannot print the special keys.")
(print "\C-x | ^x")
(print "\S-x | Yeah it is a uppercase X.")
(print (kbd "C-x"))
(print "Modifier Keys: Control-modified is case-insensitive.")
(print "Modifier Keys: All other modifiers are case-sensitive.")
(print "Modifier Keys: Meta-modifier.")
(print "Modifier Keys: Hyper, Super and Alt-modifiers are rare on keyboard.")
(print "Modifier Keys: Use C-x @ s/h/a to prefix Hyper, Super and Alt modifiers.")

; Overwrite a key-bind:
(defun qxf-print () (interactive) (print "Dummy print."))
(define-key global-map (kbd "C-x 1") 'qxf-print)

(nth 0 (current-active-maps))
(eval 'global-map)

; Create a window:
; frame-selected-window &optional frame [Function]
; split-window &optional window size side pixelwise [Function]
; TODO Create IDE layout:
;      1. Kill all other windows, leave only one window.
;      2. Create all required windows and buffers, store them all in a list.
(split-window nil 10 'right)
; TODO Implement history for emacs.
;      1. Save all opened buffers(user's files) into a config file.
;      2. Load and auto open at emacs startup.
; TODO Insert text programmatically.
;      1. Create buffer.
;      2. Output something to buffer.
; TODO Save to file.
; TODO Auto load (create buffers and windows) at startup.

