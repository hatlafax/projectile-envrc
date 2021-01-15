;;; projectile-envrc.el --- Extension for the projectile-mode

;; Copyright (C) 2021 Johannes Brunen (hatlafax)

;; Author:            Johannes Brunen <hatlafax@gmx.de>
;; URL:               -
;; Version:           0.1.0
;; Keywords:          projectile
;; Package-Requires:  ((emacs "27.1") (projectile "0.12.0") (cl-lib) (dash "2.17.0") (s "1.12.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;      This simple extension allows you to add a simplified direnv
;;      functionality to projectile. 
;;
;;      I was not able to get the available packages working in my
;;      Emacs configuration on a windows machine.
;;
;;      Therefore I decided to write a simplified (naive) version
;;      that reads a '.projectile-envrc' file in the pojectile
;;      project root directory on execution of commands that might 
;;      start external processes.
;;
;;      The following commands are wrapped by this package with
;;      a function that temporarily modifies the 'process-environment'
;;      according to the content of file '.projectile-envrc'. 
;;
;;      projectile-run-command-in-root
;;      projectile-run-shell-command-in-root
;;      projectile-run-async-shell-command-in-root
;;      projectile-run-gdb
;;      projectile-run-shell
;;      projectile-run-eshell
;;      projectile-run-ielm
;;      projectile-run-term
;;      projectile-run-vterm
;;      projectile-configure-project
;;      projectile-compile-project
;;      projectile-install-project
;;      projectile-package-project
;;
;;      The file format for '.projectile-envrc' is quite simple:
;;      Only lines that begin with '+', '-', '*:' or ':*' are
;;      considered. Everything else is ignored.
;;
;;      '+'  Add the following variable to the process environent
;;      '-'  Remove the following variable from the process environent
;;      '*:' Prepend to the process environment variable
;;      ':*' Append to the process environment variable
;;
;;      Example:
;;      #
;;      # Hi there this is ignored
;;      #
;;      
;;      
;;      +TIK=Hallo World
;;      +TAK="A small test"
;;      *:PATH=c:/utils/testA
;;      *:PATH=c:/utils/testB
;;      :*PATH=c:/utils/testC
;;      :*PATH=c:/utils/testD
;;      +TOK=c:/utils/testE
;;      -OSG_LOG_FILE
;;      +TOE=c:/utils/testF
;;      
;;      Usage: after configuration of projectile load the package:
;;
;;      (require 'projectile-envrc)
;;
;;; Code:

(require 's)
(require 'dash)
(require 'projectile)

(defcustom projectile-envrc-silenced t
  "On default the modification of the process envronment is not printed to the message buffer."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-envrc-verbose t
  "On default the modification of the process envronment is not printed to the message buffer."
  :group 'projectile
  :type 'boolean)

(defun projectile-envrc-set-env-var (pair)
    "Sets an environment variable. Expects a pair of (VARNAME . VALUE)"
    (setenv (car pair) (cdr pair))
    (when (and  (not projectile-envrc-silenced) projectile-envrc-verbose)
        (message "      added: %s=%s" (car pair) (getenv (car pair))))
)

(defun projectile-envrc-unset-env-var (varname)
    "Unset an environment variable. Expects a VARNAME."
    (setenv varname nil)
    (when (and  (not projectile-envrc-silenced) projectile-envrc-verbose)
        (message "    removed: %s=%s" varname (getenv varname)))
)

(defun projectile-envrc-prepend-env-var (pair)
    "Prepend VALUE to an environment variable VARNAME. Expects a pair of (VARNAME . VALUE)"
    (setenv (car pair) (concat (cdr pair) path-separator (getenv (car pair))))
    (when (and  (not projectile-envrc-silenced) projectile-envrc-verbose)
        (message "  prepended: %s=%s" (car pair) (getenv (car pair))))
)

(defun projectile-envrc-append-env-var (pair)
    "Append VALUE to an environment variable VARNAME. Expects a pair of (VARNAME . VALUE)"
    (setenv (car pair) (concat (getenv (car pair)) path-separator (cdr pair)))
    (when (and  (not projectile-envrc-silenced) projectile-envrc-verbose)
        (message "   appended:  %s=%s" (car pair) (getenv (car pair))))
)

(defun projectile-envrc-read-file-as-string (filename)
    "Returns the file content as a string"
    (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string)))

(defun projectile-envrc-parse-add (line)
    "Parses a single line of the form export VAR=VAL into a cons
cell where the car is the var name and the cdr is its value."
    ;(message line)
    (let* ( (parts (s-split-up-to "=" (s-chop-prefix "+" (s-trim line)) 1))
            (varname (s-trim (car parts)))
            (varval (s-trim (car (last parts)))) )
        (cons varname varval)
    ))

(defun projectile-envrc-parse-remove (line)
    "Parses a single line of the form export VAR into a cons
cell where the car is the var name and the cdr is its value."
    ;(message line)
    (let* ( (varname (s-chop-prefix "-" (s-trim line))) )
        varname))

(defun projectile-envrc-parse-prepend (line)
    "Parses a single line of the form export VAR=VAL into a cons
cell where the car is the var name and the cdr is its value."
    ;(message line)
    (let* ( (parts (s-split-up-to "=" (s-chop-prefix "*:" (s-trim line)) 1))
            (varname (s-trim (car parts)))
            (varval (s-trim (car (last parts)))) )
        (cons varname varval)
    ))

(defun projectile-envrc-parse-append (line)
    "Parses a single line of the form export VAR=VAL into a cons
cell where the car is the var name and the cdr is its value."
    ;(message line)
    (let* ( (parts (s-split-up-to "=" (s-chop-prefix ":*" (s-trim line)) 1))
            (varname (s-trim (car parts)))
            (varval (s-trim (car (last parts)))) )
        (cons varname varval)
    ))

(defun projectile-envrc-export-variables ()
    "Reads and evaluates a .projectile-envrc file if found in the projectile project root."
    (interactive)
    (when (projectile-project-p)
        (let ( (envrc (expand-file-name ".projectile-envrc" (projectile-project-root))) )
            (when (file-exists-p envrc)
                (unless projectile-envrc-silenced
                    (message "projectile-envr temporarily modified the process environment:"))

                (let* ((contents (projectile-envrc-read-file-as-string envrc))
                       (lines (s-split "\n" contents))

                       (exports-add    (-filter (lambda (l) (s-starts-with? "+"  l)) lines))
                       (exports-remove (-filter (lambda (l) (s-starts-with? "-"  l)) lines))
                       (exports-prepend(-filter (lambda (l) (s-starts-with? "*:" l)) lines))
                       (exports-append (-filter (lambda (l) (s-starts-with? ":*" l)) lines))

                       (envvars-add    (-map #'projectile-envrc-parse-add     exports-add))
                       (envvars-remove (-map #'projectile-envrc-parse-remove  exports-remove))
                       (envvars-prepend(-map #'projectile-envrc-parse-prepend exports-prepend))
                       (envvars-append (-map #'projectile-envrc-parse-append  exports-append))
                      )
                   (-each envvars-add     #'projectile-envrc-set-env-var)
                   (-each envvars-remove  #'projectile-envrc-unset-env-var)
                   (-each envvars-prepend #'projectile-envrc-prepend-env-var)
                   (-each envvars-append  #'projectile-envrc-append-env-var)
                )))))

(defun projectile-envrc-run (org-fun &rest args)
    "Advised function changing the process environment before calling the original function."
    ;(message "Greetings from projectile-envrc-run...")
    (let ( (process-environment (cl-copy-list process-environment) ) )
        (projectile-envrc-export-variables)
        (apply org-fun args)
    ))

(advice-add 'projectile-run-command-in-root             :around #'projectile-envrc-run)
(advice-add 'projectile-run-shell-command-in-root       :around #'projectile-envrc-run)
(advice-add 'projectile-run-async-shell-command-in-root :around #'projectile-envrc-run)
(advice-add 'projectile-run-gdb                         :around #'projectile-envrc-run)
(advice-add 'projectile-run-shell                       :around #'projectile-envrc-run)
(advice-add 'projectile-run-eshell                      :around #'projectile-envrc-run)
(advice-add 'projectile-run-ielm                        :around #'projectile-envrc-run)
(advice-add 'projectile-run-term                        :around #'projectile-envrc-run)
(advice-add 'projectile-run-vterm                       :around #'projectile-envrc-run)
(advice-add 'projectile-configure-project               :around #'projectile-envrc-run)
(advice-add 'projectile-compile-project                 :around #'projectile-envrc-run)
(advice-add 'projectile-install-project                 :around #'projectile-envrc-run)
(advice-add 'projectile-package-project                 :around #'projectile-envrc-run)

(provide 'projectile-envrc)
;;; projectile-envrc.el ends here