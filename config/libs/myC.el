(require 'treesit)
(require 'c-ts-mode)

; Remap c-mode to c-ts-mode.
(setq major-mode-remap-defaults (delete '(c-or-c++-mode) major-mode-remap-defaults))
(setq major-mode-remap-defaults (delete '(c-mode) major-mode-remap-defaults))
(setq major-mode-remap-defaults (delete '(c++-mode) major-mode-remap-defaults))

(setq c-ts-mode-indent-offset standard-indent)
(setq treesit-font-lock-level 4)

; For now just make it possible to use c-mode.
;(setcdr c-mode-base-map nil)
;(define-key c-mode-map [?\C-c] nil t)
;(setcdr c++-mode-map nil)
(setcdr c-ts-base-mode-map nil)

;(setcdr (assq 'other c-default-style) "my")

;(setq c-style-alist
;	(cons
;		(cons
;			"my"
;			`(
;				(c-basic-offset . ,standard-indent)
;				(c-comment-only-line-offset . 0)
;				(c-offsets-alist
;					; Anchor pos: Beg of previous line.
;					(string . c-lineup-dont-change)
;					; Example:
;					; if () {
;					;     /*
;					;     |
;					;     */
;					; }
;					; Anchor pos: Beg of the comment.
;					(c . 0)
;					; Anchor pos: When inside a class: Boi at the func decl start.
;					; When at top level: Bol at the func decl start. When inside
;					; a code block (only possible in Pike): At the func decl start(*).
;					(defun-open . 0)
;					; Anchor pos: At the defun block open if it's at boi,
;					; otherwise boi at the func decl start.
;					(defun-close . 0)
;					; Anchor pos: At the block open(*).
;					(defun-block-intro . +)
;					; Anchor pos: Boi at the class decl start.
;					(class-open . 0)
;					; Anchor pos: Boi at the class decl start.
;					(class-close . 0)
;
;					; This will be added to everything in class body, so access keywords
;					; like private: must have '-' instead of 0, even though they are
;					; anchored at boi at the class decl start.
;					; Anchor pos: At the class open brace if it's at boi,
;					; otherwise boi at the class decl start.
;					(inclass . +)
;
;					; class Foo {
;					;     int foo(int x)
;					;     {
;					; Anchor pos: None for functions (inclass got the relpos
;					; then), boi at the lambda start for lambdas.
;					(inline-open . 0)
;					; Anchor pos: Inexpr functions: At the lambda block open if
;					; it's at boi, else at the statement(*) at boi of the start of
;					; the lambda construct. Otherwise: At the inline block open
;					; if it's at boi, otherwise boi at the func decl start.
;					(inline-close . 0)
;					; Anchor pos: Boi at the func decl start.
;					(func-decl-cont . +)
;					; Anchor pos: Boi at the topmost intro line.
;					(knr-argdecl-intro . +)
;					; Anchor pos: At the beginning of the first K&R argdecl.
;					(knr-argdecl . 0)
;					; Anchor pos: Bol at the last line of previous construct.
;					(topmost-intro . 0)
;					; Anchor pos: Bol at the topmost annotation line
;					(topmost-intro-cont . c-lineup-topmost-intro-cont)
;					; Anchor pos: Bol at the topmost annotation line
;					(annotation-top-cont . 0)
;					; I guess something like this:
;					; double asd,
;					;     zxc,
;					; Anchor pos: Boi at the topmost intro line.
;					(annotation-var-cont . +)
;					; Like this:
;					; Foo(int x) :
;					;         x(x),
;					; Anchor pos: Boi at the func decl arglist open.
;					(member-init-intro . ++)
;					; Anchor pos: Beg of the first member init.
;					(member-init-cont . c-lineup-multi-inher)
;					; Like this:
;					; class Foo :
;					;         BaseClass,
;					; Anchor pos: Boi at the class decl start.
;					(inher-intro . ++)
;					; Anchor pos: Java: At the implements/extends keyword start.
;					; Otherwise: At the inher start colon, or boi at the class
;					; decl start if the first inherit clause hangs and it's not a
;					; func-local inherit clause (when does that occur?).
;					(inher-cont . c-lineup-multi-inher)
;					; Anchor pos: Inexpr statement: At the statement(*) at boi of
;					; the start of the inexpr construct. Otherwise: None.
;					(block-open . 0)
;					; Anchor pos: Inexpr statement: At the inexpr block open if
;					; it's at boi, else at the statement(*) at boi of the start of
;					; the inexpr construct. Block hanging on a case/default
;					; label: At the closest preceding label that starts at boi.
;					; Otherwise: At the block open(*).
;					(block-close . 0)
;
;					; I guess 4 options below will result in something like this:
;					; int array[3]
;					; {
;					;     1,
;					;     2,
;					;     3
;					; }
;
;					; Anchor pos: Boi at the brace list decl start, but a starting
;					; "typedef" token is ignored.
;					(brace-list-open . 0)
;					; Anchor pos: At the brace list decl start(*).
;					(brace-list-close . 0)
;					; Anchor pos: At the brace list decl start(*).
;					(brace-list-intro . +)
;					; Anchor pos: At the first non-ws char after the open paren if
;					; the first token is on the same line, otherwise boi at that token.
;					(brace-list-entry . 0)
;					; Anchor pos: Same as brace-list-entry.
;					(brace-entry-open . 0)
;					; Anchor pos: After a ';' in the condition clause of a for
;					; statement: At the first token after the starting paren.
;					; Otherwise: At the preceding statement(*).
;					(statement . 0)
;					; Anchor pos: After the first token in the condition clause of
;					; a for statement: At the first token after the starting
;					; paren. Otherwise: At the containing statement(*).
;					(statement-cont . +)
;					; Anchor pos: In inexpr statement block: At the inexpr block
;					; open if it's at boi, else at the statement(*) at boi of the
;					; start of the inexpr construct. In a block hanging on a
;					; case/default label: At the closest preceding label that
;					; starts at boi. Otherwise: At the start of the containing
;					; block(*).
;					(statement-block-intro . +)
;					; Anchor pos: At the case/default label(*).
;					(statement-case-intro . +)
;					; Anchor pos: At the case/default label(*).
;					(statement-case-open . 0)
;					; Anchor pos: At the containing statement(*).
;					(substatement . +)
;					; Like this:
;					; if ()
;					; {
;					; Anchor pos: At the containing statement(*).
;					(substatement-open . 0)
;					; Anchor pos: At the containing statement(*).
;					(substatement-label . +)
;					; Anchor pos: At the start of the switch block(*).
;					(case-label . +)
;					; Anchor pos: Same as inclass.
;					(access-label . -)
;					; Anchor pos: At the start of the containing block(*).
;					(label . +)
;					; Anchor pos: At the corresponding while statement(*).
;					(do-while-closure . 0)
;					; Anchor pos: At the corresponding if statement(*).
;					(else-clause . 0)
;					; Anchor pos: At the previous try or catch statement clause(*).
;					(catch-clause . 0)
;					; c-lineup-comment preserves comment alignment, like here:
;					; int foo() {
;					;     int x;
;					;                   //
;					;                   // <- indent-line here won't do anything.
;					; Anchor pos: None.
;					(comment-intro c-lineup-knr-region-comment c-lineup-comment)
;					; Anchor pos: At the containing statement(*).
;					; 2nd pos: At the open paren.
;					(arglist-intro . +)
;					; Anchor pos: At the first token after the open paren.
;					(arglist-cont . 0)
;					; Anchor pos: At the containing statement(*).
;					; 2nd pos: At the open paren.
;					(arglist-cont-nonempty . c-lineup-arglist)
;					; Anchor pos: At the containing statement(*).
;					; 2nd pos: At the open paren.
;					(arglist-close . 0)
;					; Anchor pos: Boi at the first stream op in the statement.
;					(stream-op . c-lineup-streamop)
;					; Anchor pos: None.
;					(cpp-macro . [0]) ; [0] to be always on absolute 0 column.
;					; Anchor pos: At the macro start (always at boi).
;					(cpp-macro-cont . +)
;					; Some macro continuation line alignment.
;					; Anchor pos: None.
;					(cpp-define-intro c-lineup-cpp-define +)
;					; Anchor pos: None.
;					(friend . 0)
;					; Anchor pos: Boi.
;					(objc-method-intro . [0])
;					; Anchor pos: At the method start (always at boi).
;					(objc-method-args-cont . c-lineup-ObjC-method-args)
;					; Anchor pos: At the open bracket.
;					(objc-method-call-cont
;						c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
;					(extern-lang-open . 0)
;					(namespace-open . 0)
;					(module-open . 0)
;					; Anchor pos: Boi at the extern/namespace/etc keyword.
;					(composition-open . 0)
;					(extern-lang-close . 0)
;					(namespace-close . 0)
;					(module-close . 0)
;					; Anchor pos: Boi at the corresponding extern/namespace/etc keyword.
;					(composition-close . 0)
;					; Body indentation, like in inclass.
;					(inextern-lang . +)
;					(innamespace . +)
;					(inmodule . +)
;					; Anchor pos: At the extern/namespace/etc block open brace if
;					; it's at boi, otherwise boi at the keyword.
;					(incomposition . +)
;					; Anchor pos: Boi at the decl start. This might be changed;
;					; the logical position is clearly the opening '<'.
;					(template-args-cont c-lineup-template-args +)
;					; Anchor pos: None.
;					(inlambda . 0)
;					; Anchor pos: Boi at the lambda start.
;					(lambda-intro-cont . +)
;					; Anchor pos: None.
;					(inexpr-statement . +)
;					; Anchor pos: None.
;					(inexpr-class . +))
;			))
;		c-style-alist))

(provide 'myC)
