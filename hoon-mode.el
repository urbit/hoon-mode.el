;;; hoon-mode.el --- Major mode for editing hoon files for urbit

;; Copyright (C) 2014–2016 Urbit

;; Author:
;;    * Adam Bliss      https://github.com/abliss          <abliss@gmail.com>
;; Contributors:
;;    * N Gvrnd         https://github.com/ngvrnd
;;    * TJamesCorcoran  https://github.com/TJamesCorcoran <jamescorcoran@gmail.com>
;;    * Rastus Vernon   https://github.com/rastus-vernon  <rastus.vernon@protonmail.ch>
;;    * Elliot Glaysher https://github.com/eglaysher      <erg@google.com>
;;
;; URL: https://github.com/urbit/hoon-mode.el
;; Version: 0.1
;; Keywords: extensions, hoon, nock, urbit, Mars

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is my first Major Mode, so don't expect much. It's heavily based on
;; SampleMode from the emacs wiki.

;;; Code:

(require 'cl-lib)

(defvar hoon-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Basic quoting support
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\\ "\\" st)
    ;; Hoon comments. Also mark ':' as a normal punctuation character.
    (modify-syntax-entry ?: ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; todo: i don't understand why this is here.
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?\; "." st)
    st)
  "Syntax table for `hoon-mode'.")

(eval-and-compile
  (defconst hoon-rx-constituents
    `((gap . ,(rx (and space (one-or-more space))))
      (identifier . ,(rx (and lower (zero-or-more (or lower digit "-")))))
      (mold . ,(rx (or "*"
                       "?"
                       "^"
                       (and "@" (zero-or-more word))
                       (and (opt "$-")
                            "("
                            (one-or-more
                             (or (or alphanumeric "(" ")" "*" "?" "@" "-" ":"
                                     "^")
                                 ;; Spaces must be single.
                                 (and space (or alphanumeric "(" ")" "*" "?"
                                                "@" "-" ":" "^"))))
                            ")")
                       (and lower (one-or-more (or lower digit "-" ":" "^")))
                       "$-"
                       )))
      (wing . ,(rx (one-or-more (or "." lower digit "-" "+" "<" ">"))))
      )
    "Common patterns used in font locking hoon code.")

  (defmacro hoon-rx (&rest regexps)
    "Hoon mode specialized rx macro."
    (let ((rx-constituents (append hoon-rx-constituents rx-constituents)))
      (cond ((null regexps)
             (error "No regexp"))
            ((cdr regexps)
             (rx-to-string `(and ,@regexps) t))
            (t
             (rx-to-string (car regexps) t))))))



(defconst hoon-font-lock-arm-declarations-rx
  (hoon-rx (and (group "+" (or "+" "-")) gap
                (group (or "$" identifier))))
  "Regexp of declarations")

(defconst hoon-font-lock-face-mold-rx
  (hoon-rx
   (and (group word (zero-or-more (or word "-")))
        "/"
        (group mold)))
  "Regexp to match name/mold in declarations.")

(defconst hoon-font-lock-kethep-rx
  (hoon-rx (and "^-  "
                (opt "{")
                (group (or mold) (zero-or-more space (or mold)))
                (opt "}")))
  "Regexp to match ^- in long form. Note the `or' around
  `mold'. We need to wrap the imported stuff in that context.")

(defconst hoon-font-lock-kethep-irregular-rx
  (hoon-rx (and "`" (group mold) "`")))

(defconst hoon-font-lock-kettis-rx
  (hoon-rx (and "^=" gap (group identifier))))

(defconst hoon-font-lock-kettis-irregular-rx
  (hoon-rx (and (group identifier) "="))
  "Regexp of faces.")

(defconst hoon-font-lock-tis-wing-rx
  (hoon-rx (and (or "=." "=/" "=?" "=*") gap (group wing)))
  "Several runes start with <rune> <gap> term/wing. Combine these into one
regexp. Because of =/, this rule must run after the normal mold rule.")

(defconst hoon-font-lock-tisket-rx
  (hoon-rx (and "=^" gap (group wing) gap (group wing))))

(defconst hoon-font-lock-symbols-rx
  (rx (and "%" (or (and word (zero-or-more (any word "-")))
                   "|" "&" "$" ".n" ".y")))
  "Regexp of symbols. This must be run before runes, or %.n and %.y will
 partially be highlighted as runes.")

(defconst hoon-font-lock-runes-rx
  ;; This could be `regexp-opt' and added statically for more speed
  (rx (or
       "$@" "$_" "$:" "$%" "$-" "$^" "$?" "$=" "$|" "$," "$&" "$+"
       "|_" "|:" "|%" "|." "|^" "|-" "|~" "|*" "|=" "|?"
       ":_" ":^" ":-" ":+" ":~" ":*"
       "%_" "%." "%-" "%*" "%^" "%+" "%~" "%="
       ".^" ".+" ".*" ".=" ".?"
       "^|" "^." "^+" "^-" "^&" "^~" "^=" "^?"
       "~|" "~_" "~%" "~/" "~<" "~>" "~$" "~+" "~&" "~=" "~?" "~!"
       ";:" ";/" ";~" ";;"
       "=|" "=:" "=/" "=;" "=." "=?" "=<" "=-" "=>" "=^" "=+" "=~" "=*" "=,"
       "?|" "?-" "?:" "?." "?^" "?<" "?>" "?+" "?&" "?@" "?~" "?=" "?!"
       "!," "!>" "!;" "!=" "!?" "!^" "!:"
       ;; Not technically runes, but we highlight them like that.
       "=="
       "--"
       ))
  "Regexp of runes.")

(defconst hoon-font-lock-preprocessor-rx
  (rx (or "/?" "/-" "/+" "//" "/="))
  "Ford preprocessor 'runes'.")

(defconst hoon-font-lock-zapzap-rx
  (rx "!!")
  "Highlight the crash rune in red.")

(defun hoon-font-match-comment-code-matcher (end)
  "Search for embedded `markdown code` in string types which
should be highlighted. This check ensures that both the ` marks
occur inside some sort of string."
  (let ((pos 0)
        (end-pos 0))
    (cond ((and (setq pos (search-forward "`" end t))
                (nth 3 (syntax-ppss pos)))
           (let ((beg (match-beginning 0)))
             (cond ((and (setq end-pos (search-forward "`" end t))
                         (nth 3 (syntax-ppss end-pos)))
                    (set-match-data (list beg (point)))
                    t)
                   (t nil))))
          (t nil))))

(defconst hoon-font-lock-numbers-rx
  ;; Numbers are in decimal, binary, hex, base32, or base64, and they must
  ;; contain dots (optionally followed by whitespace), as in the German manner.
  (rx (or
       (and "0w"
            (repeat 1 5 (in "-~0-9a-zA-Z"))
            (zero-or-more "." (repeat 5 (in "-~0-9a-zA-Z"))))
       (and "0v"
            (repeat 1 5 (in "0-9a-v"))
            (zero-or-more "." (repeat 5 (in "0-9a-v"))))
       (and "0b"
            (repeat 1 4 (in "0-1"))
            (zero-or-more "." (repeat 4 (in "0-1"))))
       (and "0x"
            (repeat 1 4 hex)
            (zero-or-more "." (repeat 4 hex)))
       (and (repeat 1 3 digit)
            (zero-or-more "." (repeat 3 digit)))
       ))
  "Regexp of numbers")

(defconst hoon-font-lock-todos-rx
  (rx (or "XX" "XXX" "TODO" "FIXME"))
  "Regexp of todo notes.")

;; This is a start, but we still occasionally miss some complex mold declarations.
(defvar hoon-font-lock-keywords
  `(
    (,hoon-font-lock-arm-declarations-rx ;; "++  arm"
     (1 font-lock-constant-face)
     (2 font-lock-function-name-face))
    (,hoon-font-lock-face-mold-rx        ;; {name/mold}
     (1 font-lock-variable-name-face)
     (2 font-lock-type-face))
    (,hoon-font-lock-kethep-rx           ;; ^-  mold
     (1 font-lock-type-face))
    (,hoon-font-lock-kethep-irregular-rx ;; `mold`
     (1 font-lock-type-face))
    (,hoon-font-lock-kettis-rx           ;; ^=  face
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-kettis-irregular-rx ;; face=
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-tis-wing-rx         ;; (=. =/ =?)  wing
     (1 font-lock-variable-name-face))
    (,hoon-font-lock-tisket-rx           ;; =^  wing  wing
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face))

    (,hoon-font-lock-symbols-rx . font-lock-keyword-face)

    ;; Highlights all other runes in other contexts.
    (,hoon-font-lock-runes-rx . font-lock-constant-face)
    (,hoon-font-lock-preprocessor-rx . font-lock-preprocessor-face)
    (,hoon-font-lock-zapzap-rx . font-lock-warning-face)

    ;; Highlight mini-markdown.
    (hoon-font-match-comment-code-matcher 0 font-lock-constant-face t)

    ;; Highlight any auras in any other contexts. This must happen after all
    ;; the above because it would otherwise stop the previous rules' execution.
    ;; TODO: This rule causes false positives, highlighting ^ in contexts where
    ;; it's used to reach up one namespace instead of being a mold.
    ("\\(@\\w*\\)\\|\\^" . font-lock-type-face)

    ;; These highlights don't have any issues.
    (,hoon-font-lock-numbers-rx . font-lock-constant-face)
    (,hoon-font-lock-todos-rx . font-lock-warning-face))
  "Keyword highlighting specification for `hoon-mode'.")

(defvar hoon-imenu-generic-expression ".*")

(defvar hoon-outline-regexp ":::")

(defun hoon-info-docstring-p (state)
  "Return non-nil if point is in a docstring."
  (and (nth 3 state)
       (nth 8 state)
       (string=
        (buffer-substring-no-properties (nth 8 state) (+ (nth 8 state) 4))
        "''':")))

(defun hoon-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      (if (hoon-info-docstring-p state)
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(defun hoon-syntax-stringify ()
  "Put `syntax-table' property correctly on doccords. Adapted
from `python-syntax-stringify', which does a similar trick."
  (let* ((num-quotes (length (match-string-no-properties 1)))
         (ppss (prog2
                   (backward-char num-quotes)
                   (syntax-ppss)
                 (forward-char num-quotes)))
         (string-start (and (not (nth 4 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) num-quotes))
         (quote-ending-pos (point)))
    (if (not string-start)
        ;; This set of quotes delimit the start of a string.
        (put-text-property quote-starting-pos (1+ quote-starting-pos)
                           'syntax-table (string-to-syntax "|"))
      ;; This set of quotes delimit the end of a string.
      (put-text-property (1- quote-ending-pos) quote-ending-pos
                         'syntax-table (string-to-syntax "|")))))

(defconst hoon-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (group "''':"))
    (0 (ignore (hoon-syntax-stringify)))))
  "Modify the syntax table so we deal with multiline doccords.")

;;;###autoload
(define-derived-mode hoon-mode prog-mode "Hoon"
  "A major mode for editing Hoon files."
  :syntax-table hoon-mode-syntax-table
  (set (make-local-variable 'comment-start) "::")
  (set (make-local-variable 'comment-padding) 2)
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 56)   ;; zero based columns
  (set (make-local-variable 'comment-use-syntax) t)
  (set (make-local-variable 'comment-start-skip) "\\(::+\\)\\s-*")
  (set (make-local-variable 'font-lock-defaults)
       '(hoon-font-lock-keywords
         nil nil nil nil
         (font-lock-syntactic-face-function
          . hoon-font-lock-syntactic-face-function)))
  (set (make-local-variable 'indent-tabs-mode) nil) ;; tabs zutiefst verboten
  (set (make-local-variable 'indent-line-function) 'indent-relative)
  (set (make-local-variable 'fill-paragraph-function) 'hoon-fill-paragraph)
  (set (make-local-variable 'imenu-generic-expression)
       hoon-imenu-generic-expression)
  (set (make-local-variable 'outline-regexp) hoon-outline-regexp)
  (set (make-local-variable 'syntax-propertize-function)
       hoon-syntax-propertize-function)

  ;; Hoon files often have the same file name in different
  ;; directories. Previously, this was manually handled by hoon-mode instead of
  ;; just setting the right variables and letting Emacs handle it.
  (set (make-local-variable 'uniquify-buffer-name-style) 'forward)
  (set (make-local-variable 'uniquify-strip-common-suffix) nil))

(defun hoon-fill-paragraph (&optional justify)
  "Only fill inside comments. (It might be neat to auto-convert short to long
form syntax, but that would take parsing.)"
  (interactive "P")
  (or (fill-comment-paragraph justify)
      ;; Never return nil; `fill-paragraph' will perform its default behavior
      ;; if we do.
      t))

;;; Indentation

(defun hoon-indent-line ()
  "Indent current line of Hoon code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (hoon-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun hoon-calculate-indentation ()
  "Return the column to which the current line should be indented."
  0) ;;TODO

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hoon$" . hoon-mode))

(provide 'hoon-mode)
;;; hoon-mode.el ends here
