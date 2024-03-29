;;; groot.el --- Support for Org links to Git repo & relative path.  -*- lexical-binding:t -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-06-06
;; Timestamp:  2023-09-29
;; Homepage:   https://github.com/cole-brown/groot
;; Keywords:   hypermedia vc
;;
;; TODO:pkg: Add back in.
;; TODO:pkg: TODO-Package-Version: 1.9.0
;; TODO:pkg: TODO-Package-Requires: (
;;     TODO:pkg: What version of emacs does this actually require?
;;     (emacs "28.1")
;;     TODO:pkg: Does it really need these and what version?
;;     (compat "29.1.4.1")
;;     (magit "3.3.0")
;;     (org "9.6.5"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This package defines the Org link type `groot', which can be used to store
;; relative path links to local git repo files.
;;
;; Use the command `org-store-link' in a file-backed buffer to store a link.
;; Later you can insert that into an Org buffer using the command
;; `org-insert-link'.
;;
;; Format
;; ------
;;
;; The link type defined here take these forms:
;;
;;    groot:repo-name:/relative/path/file.ext
;;
;; Export
;; ------
;;
;; TODO: Currently nothing special supported.
;;
;; TODO: COMMENTARY FOR GROOT EXPORT!
;;
;; When an Org file containing such links is exported, then the url of
;; the remote configured with `groot-remote' is used to generate a web
;; url according to `groot-export-alist'.
;;
;; Both the remote to be considered the public remote, as well as the
;; actual web urls can be defined in individual repositories using Git
;; variables.
;;
;; To use a remote different from `groot-remote' but still use
;; `groot-export-alist' to generate the web urls, use:
;;
;;    git config groot.remote REMOTE-NAME
;;
;; To explicitly define the web urls, use something like:
;;
;;    git config groot.status http://example.com/repo/overview
;;    git config groot.rev http://example.com/repo/revision/%r
;;    git config groot.log http://example.com/repo/history/%r
;;
;;; Code:


;;------------------------------------------------------------------------------
;; Prereqs & Compatibility
;;------------------------------------------------------------------------------

(require 'cl-lib)
(require 'compat)
(require 'format-spec)
(require 'magit)
(require 'org)

;; Compatibility with Org <9.3 (released 2019-12-03).
(unless (fboundp 'org-link-store-props)
  (defalias 'org-link-store-props 'org-store-link-props))

(eval-when-compile (require 'subr-x))


;;--------------------------------------------------------------------------------
;; Options
;;--------------------------------------------------------------------------------

;; TODO: Delete whatever we're not using.

(defgroup groot nil
  "Org links to Magit buffers."
  :group 'magit-extensions
  :group 'org-link)


(defcustom groot-repositories nil
  "Alist of Git repositories.

Each element has the form (NAME . PATH)
  - NAME: string - repository's name (usually the same as the repo dir name)
  - PATH: string - absolute path to the repo's root directory

NOTE: `groot-path-repositories' is a helpful function that will find all Git
repos directly under a dir. However, it cannot be used in `use-package'
`:custom' section, so you'd have to wait for the `:config':
  (use-package groot
     ...
     :config
     (customize-set-variable 'groot-repositories
                             (list (groot-path-repositories \"~/.config\")
                                   (groot-path-repositories \"~/repos\")))"
  :group 'groot
  :type '(alist :key-type string :value-type directory))


(defcustom groot-ignore-major-modes '(org-mode)
  "List of major mode symbols we should _not_ take responsibility for org links.
This allows other org link types to make the link instead.

NOTE: `major-mode' is compared using `derived-mode-p', so these and all derived
modes are ignored."
  :group 'groot
  :type '(repeat symbol))


(defcustom groot-ignore-predicates nil
  "List of predicates for whether to _not_ take responsibility an org link.
This allows other org link types to make the link instead.

Predicates take a buffer-object and return non-nil for \"ignore this buffer\".

See `groot-link--store?' for default ignores."
  :group 'groot
  :type '(repeat function))


;; TODO: export functionality!
;; (defcustom groot-export-alist
;;   `(("github.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
;;      "https://github.com/%n"
;;      "https://github.com/%n/commits/%r"
;;      "https://github.com/%n/commit/%r")
;;     ("gitlab.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
;;      "https://gitlab.com/%n"
;;      "https://gitlab.com/%n/commits/%r"
;;      "https://gitlab.com/%n/commit/%r")
;;     ("git.sr.ht[:/]\\(.+?\\)\\(?:\\.git\\)?$"
;;      "https://git.sr.ht/%n"
;;      "https://git.sr.ht/%n/log/%r"
;;      "https://git.sr.ht/%n/commit/%r")
;;     ("bitbucket.org[:/]\\(.+?\\)\\(?:\\.git\\)?$"
;;      "https://bitbucket.org/%n"
;;      "https://bitbucket.org/%n/commits/branch/%r"
;;      "https://bitbucket.org/%n/commits/%r")
;;     ("code.orgmode.org[:/]\\(.+\\)$"
;;      "https://code.orgmode.org/cgit.cgi/%n"
;;      "https://code.orgmode.org/cgit.cgi/%n/commits/%r"
;;      "https://code.orgmode.org/cgit.cgi/%n/commit/%r")
;;     ("git.kernel.org/pub/scm[:/]\\(.+\\)$"
;;      "https://git.kernel.org/cgit/%n"
;;      "https://git.kernel.org/cgit/%n/log/?h=%r"
;;      "https://git.kernel.org/cgit/%n/commit/?id=%r"))
;;   "Alist used to translate Git urls to web urls when exporting links.
;;
;; Each entry has the form (REMOTE-REGEXP STATUS LOG REVISION).  If
;; a REMOTE-REGEXP matches the url of the chosen remote then one of
;; the corresponding format strings STATUS, LOG or REVISION is used
;; according to the major mode of the buffer being linked to.
;;
;; The first submatch of REMOTE-REGEXP has to match the repository
;; identifier (which usually consists of the username and repository
;; name).  The %n in the format string is replaced with that match.
;; LOG and REVISION additionally have to contain %r which is
;; replaced with the appropriate revision.
;;
;; This can be overwritten in individual repositories using the Git
;; variables `groot.status', `groot.log' and `groot.commit'. The
;; values of these variables must not contain %n, but in case of the
;; latter two variables they must contain %r.  When these variables
;; are defined then `groot-remote' and `groot.remote' have no effect."
;;   :group 'groot
;;   :type '(repeat (list :tag "Remote template"
;;                        (regexp :tag "Remote regexp")
;;                        (string :tag "Status format")
;;                        (string :tag "Log format" :format "%{%t%}:    %v")
;;                        (string :tag "Revision format"))))
;;
;;
;; (defcustom groot-remote "origin"
;;   "Default remote used when exporting links.
;;
;; If there exists but one remote, then that is used unconditionally.
;; Otherwise if the Git variable `groot.remote' is defined and that
;; remote exists, then that is used.  Finally the value of this
;; variable is used, provided it does exist in the given repository.
;; If all of the above fails then `groot-export' raises an error."
;;   :group 'groot
;;   :type 'string)


;;--------------------------------------------------------------------------------
;; Error Signalling
;;--------------------------------------------------------------------------------

(define-error 'groot-error              "Groot Error")
(define-error 'groot-error--path        "Bad Path"               'groot-error)
(define-error 'groot-error--repo        "Repository Error"       'groot-error)
(define-error 'groot-error--not-in-repo "Not in a Repository"    'groot-error--repo)
(define-error 'groot-error--repo-path   "Bad Repository Path"    '(groot-error--path groot-error--repo))
(define-error 'groot-error--mode        "Unsupported Major Mode" 'groot-error)
(define-error 'groot-error--link        "Bad `groot' Link"       'groot-error)
;; (signal 'groot-error--not-in-repo '(x y))
;; (error "hi")


(defvar groot--org-api-warn-on-error nil
  "Should errors during `org' API usage be noted?

`groot' eats all errors in the `org-link-parameters' API functions (e.g.
`groot-link--store') as otherwise it breaks e.g. `org-store-link', and not just
for the groot links. It will not eat the errors when `debug-on-error' is true.

This setting is for whether to `message' information about consumed errors.

Probably don't need to enable this. Mainly for the dev.")


(defun groot--error (type message &rest args)
  "Signal an error of TYPE with a formatted message, like function `error'.

TYPE should be `groot-error' or a child error type.

MESSAGE should be a (formatting) string.

ARGS should match up to formatting fields in MESSAGE."
  (declare (indent 1))
  ;; NOTE: Do not downgrade from error to warning here; ignore
  ;; `groot--org-api-warn-on-error'. That is for `groot-link' functions to
  ;; handle.
  (signal type
          ;; NOTE: Function `error' is (signal 'error '("formatted message")),
          ;; basically. So imitate that?
          (list (apply #'format message args))))
;; (groot--error 'groot-error--not-in-repo "hello %S" 'there)
;; (error "hello %S" 'there)


(defun groot--error-or-nil (error? type message &rest args)
  "Signal TYPE error or ignore and return nil.

If ERROR? is non-nil, signal an error of TYPE with a formatted message, like
function `error'. Else do nothing and return nil, like `ignore'.

TYPE should be `groot-error' or a child error type.

MESSAGE should be a (formatting) string.

ARGS should match up to formatting fields in MESSAGE."
  (declare (indent 2))
  (when error?
    (apply #'groot--error type message args)))


(defun groot--org-api--handle-error (caller error-value)
  "Handle ERROR-VALUE for the `org' API functions, which cannot raise errors.

If `groot--org-api-warn-on-error' is non-nil, convert ERROR-VALUE's message
to a warning message. Else drop ERROR-VALUE and return nil.

CALLER should be the calling function's name, for the +error+ warning message.

Warning message format is:
  \"[ERROR] $CALLER: caught error '$ERROR-TYPE': $ERROR-MESSAGE\""
  (when groot--org-api-warn-on-error
    (warn "[ERROR] %s: caught error '%S': %s"
          caller
          (car error-value)
          (error-message-string error-value))))


;;--------------------------------------------------------------------------------
;; Validation
;;--------------------------------------------------------------------------------

(cl-defun groot--path-assert (context path &key error? dir?)
  "Ensure PATH is valid.

CONTEXT should be a string suitable for displaying in an error to the user.
CONTEXT will be prepended to error messages:
  \"CONTEXT: <error message>\"

PATH is always required to:
  - be a string
  - be an existing path

PATH is optionally required to:
  - DIR? non-nil: be an existing directory

If ERROR? is non-nil, raise an error signal with details on why it is invalid.
If ERROR? is nil, just return nil on invalid PATH.

Return PATH, nil, or raise error signal."
  ;;------------------------------
  ;; Invalid
  ;;------------------------------
  (cond ((not (stringp path))
         (groot--error-or-nil error? 'groot-error--path
           "%s: PATH must be a string or nil, got '%s': '%S'"
           context
           (type-of path)
           path))

        ((not (file-exists-p path))
         (groot--error-or-nil error? 'groot-error--path
           "%s: PATH does not exist? '%s'"
           context
           path))

        ((and dir?
              (not (file-directory-p path)))
         (groot--error-or-nil error? 'groot-error--path
           "%s: PATH is not a directory? '%s'"
           context
           path))

        ;;------------------------------
        ;; Valid
        ;;------------------------------
        ;; Got past all the requirements, so return non-nil for success.
        (t
         path)))
;; (groot--path-assert "test 1" (buffer-file-name) :error? t)
;; (groot--path-assert "test 2" (buffer-file-name) :error? t :dir? t)
;; (groot--path-assert "test 3" (buffer-file-name) :dir? t)


(cl-defun groot--name-assert (context name &key error?)
  "Ensure NAME is valid.

CONTEXT should be a string suitable for displaying in an error to the user.
CONTEXT will be prepended to error messages:
  \"CONTEXT: <error message>\"

NAME is always required to:
  - be a string

If ERROR? is non-nil, raise an error signal with details on why it is invalid.
If ERROR? is nil, just return nil on invalid NAME.

Return NAME, nil, or raise error signal."
  ;;------------------------------
  ;; Invalid
  ;;------------------------------
  (cond ((not (stringp name))
         (groot--error-or-nil error? 'groot-error
           "%s: NAME must be a string, got '%s': '%S'"
           context
           (type-of name)
           name))

        ;;------------------------------
        ;; Valid
        ;;------------------------------
        ;; Got past all the requirements, so return non-nil for success.
        (t
         name)))
;; (groot--name-assert "test 1" "name" :error? t)
;; (groot--name-assert "test 2" nil :error? t)
;; (groot--name-assert "test 3" :name :error? t)


;;--------------------------------------------------------------------------------
;; Getters & Setters
;;--------------------------------------------------------------------------------

(defun groot-repositories (name)
  "Getter for alist `groot-repositories'.

NAME should be a string.

Allows for cons or list entries:
  '((NAME-0 . PATH-0)
    (NAME-1 PATH-1))

Return normalized path or nil."
  (when-let ((value (alist-get name groot-repositories nil nil #'string=)))
    (groot--path-normalize :type 'dir
                           :path (if (listp value)
                                     (nth 0 value)
                                   value))))
;; (pp groot-repositories)
;; (groot-repositories "groot")
;; (groot-repositories "emacs-sn004")
;; (setq groot-repositories '((".emacs.d" . "~/.config/emacs-sn004") ("personal" "~/.config/personal")))
;; (groot-repositories ".emacs.d")
;; (groot-repositories "personal")
;; (groot-repositories "does-not-exist1")


(defun groot-repositories-append (&rest args)
  "Setter for `groot-repositories'.

Each arg in ARGS should be:
  - A path.
  - A cons of '(REPO-NAME . REPO-PATH)
  - An alist of such conses (e.g. from `groot-path-repositories')."
  ;; Add each arg to `groot-repositories'.
  (dolist (arg args)
    (cond
     ;;------------------------------
     ;; `string': Add as a repo.
     ;;------------------------------
     ((stringp arg)
      (let* ((path (groot--repository-path-normalize arg))
             (name (groot--name-normalize path)))
        (push (cons name path) groot-repositories)))

     ;;------------------------------
     ;; `cons': Normalize and add.
     ;;------------------------------
     ;; "cons but not list" check:
     ((and (cdr arg)
           (atom (cdr arg)))
      (push (cons (car arg) ; Dunno how to normalize the name, so just leave it as-is.
                  (groot--repository-path-normalize (cdr arg)))
            groot-repositories))

     ;;------------------------------
     ;; `list': Recurse!
     ;;------------------------------
     ;; `proper-list-p' to avoid circular lists and dotted lists.
     ((proper-list-p arg)
      ;; Easy: Just ask ourselves to to our work for us.
      (apply #'groot-repositories-append arg))

     ;;------------------------------
     ;; Fallthrough: Error!
     ;;------------------------------
     (t
      (error "groot-repositories-append: Don't know how to deal with arg: %S"
             arg)))))
;; (groot-repositories-append (buffer-file-name))
;; (apply #'groot-repositories-append (groot-path-repositories "~/.config"))


(defun groot--repository-name-to-path (name)
  "Get path to repository NAME.

NAME should be a string.

Return absolute path string to repository root, or nil if not found.

NOTE: No error checking of NAME! Use `groot--name-assert' if desired."
  (or
   ;;------------------------------
   ;; Seach Groot Repo List
   ;;------------------------------
   (groot-repositories name)

   ;;------------------------------
   ;; Optional: Search Autogit Commit Repo List?
   ;;------------------------------
   (when (bound-and-true-p autogit:repos:path/commit)
     (let (path-found
           (repo-list autogit:repos:path/commit)) ; list of path strings
       (while (and (null path-found)
                   repo-list)
         (when-let* ((repo-path (pop repo-list)))
           ;; Ensure path is a repo root.
           (setq repo-path (groot--repository-path-normalize repo-path))
           (when (string= name
                          (groot--name-normalize repo-path))
             ;; Found it!
             (setq path-found repo-path))))

       ;; Did we find anything?
       path-found))

   ;;------------------------------
   ;; Optional: Search Autogit Watch Repo List?
   ;;------------------------------
   (when (bound-and-true-p autogit:repos:path/watch)
     (let (path-found
           (repo-list autogit:repos:path/watch)) ; list of path strings
       (while (and (null path-found)
                   repo-list)
         (when-let* ((repo-path (pop repo-list)))
           ;; Ensure path is a repo root.
           (setq repo-path (groot--repository-path-normalize repo-path))
           (when (string= name
                          (groot--name-normalize repo-path))
             ;; Found it!
             (setq path-found repo-path))))

       ;; Did we find anything?
       path-found))

   ;;------------------------------
   ;; Optional: Search Magit Repo List?
   ;;------------------------------
   (when (bound-and-true-p magit-repository-directories)
     ;; NOTE: `magit-repository-directories' is an alist of dirs that are _OR THAT MAY CONTAIN_ git
     ;; repos. Convert into actual git repos with `magit-repos-alist'. We'll end up with an alist
     ;; with entries: (REPO-DIR-NAME . REPO-PATH)
     (alist-get name (magit-repos-alist) nil nil #'string=))))
;; (setq groot-repositories '((".emacs.d" . "~/.config/emacs-sn004") ("personal" "~/.config/personal")))
;; (groot--repository-name-to-path ".emacs.d")
;; (groot--repository-name-to-path "personal")
;; (groot--repository-name-to-path "lily.d")
;; (groot--repository-name-to-path "smudge")
;; (groot--repository-name-to-path "openiddict-ui")
;;
;; (groot--repository-name-to-path "does-not-exist")
;; (groot--repository-name-to-path nil)


;;--------------------------------------------------------------------------------
;; Normalization
;;--------------------------------------------------------------------------------

(cl-defun groot--path-normalize (&key path type)
  "Normalize PATH string.

TYPE should be one of: `file', `dir'

If PATH is not a string, normalize to nil.
Else return PATH that is:
  - absolute
  - abbreviated (e.g. \"~\" instead of \"/home/username\")
  - TYPE:
    - `file' : file path (cannot end in dir separator)
    - `dir'  : directory path (must end in dir separator)

NOTE: No error checking! Use `groot--path-assert' if desired."

  ;;------------------------------
  ;; Error Checks
  ;;------------------------------
  (cond ((or (not path)
             (not (stringp path)))
         ;; TODO: error out or always just nil?
         nil)
        ((or (not type)
             (not (symbolp type))
             (not (memq type '(file dir))))
         ;; TODO: error out or always just nil?
         nil)

        ;;------------------------------
        ;; Normalize
        ;;------------------------------
        (t
         (abbreviate-file-name ; path abbreviations like "~"
          (funcall
           (pcase type
             ('file
              #'directory-file-name) ; as file path
             ('dir
              #'file-name-as-directory) ; as directory path
             (_
              ;; TODO: error out or always just nil? Was error checked above...
              nil))
           ;; absolute path
           (expand-file-name path))))))
;; (groot--path-normalize :type 'file :path "~/.config")
;; (groot--path-normalize :type 'dir :path "~/.config")
;; (groot--path-normalize :type 'file :path "/home/work/.config")
;; (groot--path-normalize :type 'file :path "actual-package-stuff")
;; (groot--path-normalize :type 'dir :path default-directory)
;; (groot--path-normalize :type 'file :path (buffer-file-name (buffer-base-buffer)))
;; (groot--path-normalize :type 'file :path (buffer-file-name (buffer-base-buffer)))
;; bad -> nil:
;; (groot--path-normalize :path 'hi)
;; (groot--path-normalize :type 'file :path 'hi)


(defun groot--name-normalize (path)
  "Get the name of the final directory in PATH.

PATH should be a directory path string.
If PATH is not a string, normalize name to nil.

NOTE: No error checking! Use `groot--path-assert' if desired."
  (when (stringp path)
    (file-name-nondirectory ; Get dir name from filepath.
     (groot--path-normalize :type 'file :path path))))
;; (groot--name-normalize default-directory)
;; (groot--name-normalize (buffer-file-name))
;; (groot--name-normalize 'hi)


(defun groot--repository-path-normalize (path)
  "Get absolute path to Git repository root of PATH.

PATH should be a path string.

Return path that is:
  - absolute
  - abbreviated (e.g. \"~\" instead of \"/home/username\")
  - directory (i.e. ends in dir separator)

NOTE: No error checking of PATH! Use `groot--path-assert' if desired."
  (when (stringp path)
    ;; `magit' does the heavy lifting.
    (groot--path-normalize :type 'dir
                           :path (magit-toplevel path))))
;; (groot--repository-path-normalize "~/.config/emacs-sn004/mantle/config")
;; (groot--repository-path-normalize nil)


(defun groot--repository-name-normalize (path)
  "Get the name of the git repo's root directory from PATH.

PATH should be a path string.

Usually name is equivalent to the git repo root directory's name, except when
an alias name is provided in alist `groot-repositories'.

Signal an error if path is invaild or not in a Git repository, or if the current
Git repo isn't known enough for later link following, signal an error."
  (when-let ((path-normal (groot--repository-path-normalize path)))
    (or
     ;;------------------------------
     ;; Search for an alias first.
     ;;------------------------------
     (let ((aliases groot-repositories)
           alias)
       (while (and (null alias)
                   aliases)
         (if-let* ((entry (pop aliases))
                   (entry-alias (car entry))
                   (entry-path (if (listp (cdr entry))
                                   (nth 1 entry)
                                 (cdr entry))))
             ;; NOTE: Paths in `groot-repositories' not guarenteed to be normalized!
             (when (string= (groot--path-normalize :type 'dir :path entry-path)
                            path-normal)
               (setq alias entry-alias))))
       ;; Did we find an alias?
       alias)

     ;;------------------------------
     ;; Fallback
     ;;------------------------------
     ;; No alias? Just use the dir name.
     (groot--name-normalize path-normal))))
;; (groot--repository-name-normalize default-directory)


(defun groot--repository-path-rooted (root path)
  "Get PATH relative to ROOT.

PATH should be a path string.
ROOT should be a path string that is a parent of PATH.

NOTE: No error checking of parameters! Use `groot--path-assert' if desired."
  (string-trim-left path (regexp-quote root)))
;; (groot--repository-path-rooted (groot--repository-current-path) (groot--path-normalize :type 'file :path (buffer-file-name (buffer-base-buffer))))


;;--------------------------------------------------------------------------------
;; Current Repository / Path
;;--------------------------------------------------------------------------------

(cl-defun groot--repository-current-path (&key (error? t))
  "Get absolute path to current Git repository according to `default-directory'.

If ERROR? is nil, eat error signals and return nil instead.

Return absolute directory (ends in dir separator) path."
  (let ((path (groot--path-normalize :type 'dir :path default-directory)))
    ;; Require `default-directory' exist as a directory.
    (if (not (groot--path-assert "repository-current-path"
                                 path
                                 :error? error?
                                 :dir? t))
        ;; `path' is invalid but we didn't want an error signal, so just
        ;; make sure to not continue on doing stuff.
        nil

      ;; Figure out repo's root path.
      (let ((repo (groot--repository-path-normalize path)))
        ;; ...Are we _actually in a repo?
        (if (not repo)
            (groot--error-or-nil error? 'groot-error--not-in-repo
              "groot--repository-current-path: Not currently in a repository! Current Path: %S"
              path)
          ;; `repo' is either the repo root path or nil (and they didn't want an error), so return it.
          repo)))))
;; (groot--repository-current-path)
;; (let (default-directory) (groot--repository-current-path))
;; (let (default-directory) (groot--repository-current-path :error? t))
;; (let (default-directory) (groot--repository-current-path :error? nil))


(cl-defun groot--repository-current-name (&key (error? t))
  "Get the name of current git repo according to `default-directory'.

Usually name is equivalent to the git repo root directory's name, except when
an alias name is provided in alist `groot-repositories', or maybe
`magit-repos-alist'.

If ERROR? is nil, eat error signals and return nil instead.

Return a string, nil, or raise an error signal."
  (groot--repository-name-normalize (groot--repository-current-path :error? error?)))
;; (groot--repository-current-name)
;; (let (default-directory) (groot--repository-current-name))
;; (let (default-directory) (groot--repository-current-name :error? nil))


(cl-defun groot--path-current (&key (error? t) relative?)
  "Get absolute path to current file-backed buffer.

If ERROR? is nil, eat error signals and return nil instead.


If RELATIVE? is nil, return absolute filepath.
If RELATIVE? is non-nil, return filepath relative to repo root."
  ;;------------------------------
  ;; Normalize & Assert the Path
  ;;------------------------------
  ;; Get filename of current buffer (if indirect, get filename of base buffer).
  (let ((path (groot--path-normalize :type 'file
                                     :path (buffer-file-name (buffer-base-buffer)))))
    ;; Require the file actually exist? If that's annoying for some reason, can
    ;; remove or make another kwarg...
    (if (not (groot--path-assert "path-current"
                                 path
                                 :error? error?
                                 :dir? nil))
        ;; `path' is invalid but we didn't want an error signal, so just
        ;; make sure to not continue on doing stuff.
        nil

      ;;------------------------------
      ;; Return the Path
      ;;------------------------------
      (if relative?
          ;; Relative Path
          (groot--repository-path-rooted (groot--repository-current-path :error? error?)
                                         path)

        ;; Absolute Path
        path))))
;; (groot--path-current)
;; (let (default-directory) (groot--path-current))
;; (let (default-directory) (groot--path-current :error? nil))


;;--------------------------------------------------------------------------------
;; Org Link API
;;--------------------------------------------------------------------------------

(defun groot-link--store? (buffer repo-name)
  "Should `groot' be the link type to store this org link?

Annoyingly, we can't have a low priority of any sort, and,
actually, being added via `org-link-set-parameters' makes it the
highest priority until something else is added. So instead look
at major modes and stuff and quit early if we thing someone else
can do a better job of this.

BUFFER should be the buffer that org is asking for a link to.

REPO-NAME should be the repository that the buffer exists in."
  (cond
   ;;------------------------------
   ;; Do Not Store Link!
   ;;------------------------------
   ;; Check for disallowed major-mode - e.g. `org-mode' we currently want the
   ;; normal 'file:' link handler to do its thing with the current headline.
   ((apply #'derived-mode-p groot-ignore-major-modes)
    nil)

   ;; Check with any predicates.
   (groot-ignore-predicates
    (let ((predicates groot-ignore-predicates)
          ignore?)
      (while (and (not ignore?)
                  predicates)
        (setq ignore? (funcall (pop predicates) buffer)))
      ignore?))

   ;; Ignore buffers without backing files.
   ((not (buffer-file-name (buffer-base-buffer)))
    nil)

   ;; Check if `groot' knows about this repository.
   ((not (groot--repository-name-to-path repo-name))
    nil)

   ;;------------------------------
   ;; ...Maybe Store Link?
   ;;------------------------------
   ;; Otherwise, make an attempt to store this link. Can still fail if
   ;; `groot-link--store' returns nil for whatever reason.
   (t)))
;; (groot-link--store? (current-buffer))


;; TODO: option for this type of location versus others (e.g. revision, line, column)
(defun groot-link--store--file-location ()
  "Get a file location string for `groot-link--store'.

Like 'file'-type links: `file:<path>::<location-string>'

Expect point/region in `current-buffer' to be the desired location.

Return a string or nil."
  ;;------------------------------
  ;; `org-create-file-search-functions'?
  ;;------------------------------
  ;; If a hook func succeeds, just return what it returns.
  (cond ((and org-create-file-search-functions
              (run-hook-with-args-until-success 'org-create-file-search-functions)))

        ;;------------------------------
        ;; `org-mode' file?
        ;;------------------------------
        ;; Wish I could just call the function org uses, but it's all stuffed
        ;; into the top-level, interactive mega-function `org-store-link'...
        ;; So for now, just assume we don't get `org-mode' files?
        ;; If we start working in `org-mode', see for all the shenanigans necessary:
        ;;   [[file:/usr/share/emacs/28.1/lisp/org/ol.el.gz::and (buffer-file-name (buffer-base-buffer)) (derived-mode-p 'org-mode]]
        ((and (buffer-file-name (buffer-base-buffer))
              (derived-mode-p 'org-mode))
         (groot--error 'groot-error--mode
           "%s: Major mode '%S' is not supported due to being (derived from) '%S'!"
           "groot-link--store--file-location"
           major-mode
           (derived-mode-p 'org-mode)))

        ;;------------------------------
        ;; Context Search String
        ;;------------------------------
        ((buffer-file-name (buffer-base-buffer))
         ;; Obey `org-store-link' expectations: `org-link-context-for-files'
         ;; is the setting and a prefix arg can negate it.
         (when (org-xor org-link-context-for-files
                        (equal current-prefix-arg '(4)))
           ;; NOTE: Entire region should be stored if region is active. See `org-link-context-for-files'.
           (let ((context (org-link--normalize-string (or (org-link--context-from-region)
                                                          (org-current-line-string))
                                                      t)))
             ;; Did we get anything?
             ;; `org-string-nw-p' is a terrible name.
             ;; Think `org-string-not-null-or-whitespace-p'.
             (when (org-string-nw-p context)
               ;; Ok; return context string.
               context))))

        ;;------------------------------
        ;; Fallback
        ;;------------------------------
        (t
         nil)))


;;;###autoload
(defun groot-link--store ()
  "Store a link to a file in a Git repository.

Link will be in format:
  - groot:repo-name:/path/rooted/in/repo.ext"
  ;; Eat errors and return nil so users don't get their `org-store-link' broken.
  ;; Do not eat errors when `debug-on-error' is true.
  (condition-case-unless-debug error
      (let ((repo-name (groot--repository-current-name :error? t)))
        ;;------------------------------
        ;; Should We Make This Link?
        ;;------------------------------
        (when (groot-link--store? (current-buffer) repo-name)
          ;;------------------------------
          ;; Sanity Checks
          ;;------------------------------
          (let ((repo-path (groot--repository-current-path :error? t))
                (buffer-path (groot--path-normalize :type 'file
                                                    :path (buffer-file-name (buffer-base-buffer)))))
            (groot--path-assert "groot-link--store"
                                buffer-path
                                :error? t
                                :dir? nil)

            ;;------------------------------
            ;; Create the Link
            ;;------------------------------
            ;; Convert full path to relative/rooted path.
            (let ((link-path (groot--repository-path-rooted repo-path buffer-path)))
              (if-let ((link-location (groot-link--store--file-location)))
                  (org-link-store-props :type "groot"
                                        :link (format "groot:%s:/%s::%s"
                                                      repo-name
                                                      link-path
                                                      link-location)
                                        ;; No description for now?
                                        ;; :description (format "%s:/%s"
                                        ;;                      repo-name
                                        ;;                      (string-trim-left buffer-path repo-path))
                                        )
                (org-link-store-props :type "groot"
                                      :link (format "groot:%s:/%s"
                                                    repo-name
                                                    link-path))
                ;; No description for now?
                ;; :description (format "%s:/%s"
                ;;                      repo-name
                ;;                      (string-trim-left buffer-path repo-path))
                )))))

    ;;------------------------------
    ;; Error Handling
    ;;------------------------------
    ;; If `debug-on-error' is enabled, errors will happen as usual.
    ;; Otherwise, downgrade to warning or just quietly return nil.
    ;;---
    ;; Our Error Types
    ;;---
    (groot-error
     (groot--org-api--handle-error "groot-link--store" error)
     ;; Always return nil so that org doesn't think we did something.
     nil)
    ;;---
    ;; Any Other Error Types
    ;;---
    (error
     (groot--org-api--handle-error "groot-link--store" error)
     ;; Always return nil so that org doesn't think we did something.
     nil)))
;; (groot-link--store)


(defconst groot-link--regex
  (rx-to-string `(sequence
                  string-start
                  ;;---
                  ;; Repo Name (Dir/Filename, basically?)
                  ;;---
                  (group
                   (one-or-more
                    (not (any ":"      ; No colon (we need to be a field separator)
                              "\t\r\n" ; No whitespace (except space is fine)
                              ?\\      ; No literal backslash
                              ?/       ; No literal forward slash
                              ?\"      ; No literal double quote
                              ;; No... various other symbols.
                              ;; NOTE: Had bad luck with the regex when these were just in one string?!
                              ;; It wouldn't match e.g. "repo.name" which means period was sneaking in somewhere...
                              ;; That was back when a hyphen was also in here though...
                              "{}" "<>" "#" "%" "&" "*" "?" "$" "!" "@" "`" "|" "="))))
                  ;;---
                  ;; Repo/Path seperator token
                  ;;---
                  ":/"
                  ;;---
                  ;; Path
                  ;;---
                  (group
                   (one-or-more
                    ;; Similar to repo name, but must allow slash (*nix & windows) and backslash (windows).
                    (not (any ":"      ; No colon (we need to be a field separator)
                              "\t\r\n" ; No whitespace (except space is fine)
                              ?\"      ; No literal double quote
                              ;; No... various other symbols.
                              ;; NOTE: Had bad luck with the regex when these were just in one string?!
                              ;; It wouldn't match e.g. "repo.name" which means period was sneaking in somewhere...
                              ;; That was back when a hyphen was also in here though...
                              "{}" "<>" "#" "%" "&" "*" "?" "$" "!" "@" "`" "|" "="))))
                  ;;---
                  ;; Optional: Path/Location separator token
                  ;;---
                  (zero-or-one
                   "::"
                   ;;---
                   ;; Location
                   ;;---
                   (group (zero-or-more ; /Should/ always exist if "::" exists but be forgiving.
                           ;; Have to include newlines too, for linking to a region of the file...
                           ;; So... be quite permissive.
                           anything)))
                  string-end)
                :no-group)
  "Regex for parsing a `groot' link.

Expect the link format provided to `groot-link--open', which does not have the
link type included.

That is, expect:
  \"repo:/path/to/file.el\"
instead of:
  \"groot:repo:/path/to/file.el\".")
;; (let* ((str "repo.name-field:/path/to/some-file.el::;aoeu\nasdf()a){[]!@(&#")
;;        (match? (string-match groot-link--regex str)))
;;   (list :match? match?
;;         :match-0 (when match? (match-string 0 str))
;;         :match-1 (when match? (match-string 1 str))
;;         :match-2 (when match? (match-string 2 str))
;;         :match-3 (when match? (match-string 3 str))))


(defun groot-link--parse (link)
  "Parse LINK into its constituent parts.

LINK should be in format:
  - repo-name:/path/rooted/in/repo.ext
  - repo-name:/path/rooted/in/repo.ext::file location string

Return plist of key values:
  - `:repository' : string: repository name
  - `:path'       : string: relative path, rooted in repository
  - `:location'   : string or nil: place in file"
  (when (string-match groot-link--regex link)
    (list :repository (match-string 1 link)
          :path       (match-string 2 link)
          :location   (match-string 3 link))))
;; (groot-link--parse "groot:/groot.el")
;; (groot-link--parse "groot:/groot.el::")
;; (groot-link--parse "repo.name-field:/path/to/some-file.el::;aoeu\nasdf()a){[]!@(&#")


;;;###autoload
(defun groot-link--open (link)
  "Open a groot link.

LINK should be in format:
  - repo-name:/path/rooted/in/repo.ext
  - repo-name:/path/rooted/in/repo.ext::file location string

Just open the file if no location string.
Else open the file and then search for location string with `org-link-search'."
  (let ((func-name "groot-link--open"))
    ;; Eat errors and return nil so users don't get their `org-store-link' broken.
    ;; Do not eat errors when `debug-on-error' is true.
    (condition-case-unless-debug error
        (if-let ((link-parts (groot-link--parse link)))
            (let* ((link-repo (plist-get link-parts :repository))
                   (link-path (plist-get link-parts :path))
                   (link-location (plist-get link-parts :location)) ; optional
                   (repo-path (groot--repository-name-to-path link-repo)))
              ;;------------------------------
              ;; Error Checks
              ;;------------------------------
              (groot--name-assert (format "%s (link-repo)" func-name) link-repo :error? t)
              (if repo-path
                  (groot--path-assert (format "%s (repo-path)" func-name) repo-path :error? t :dir? t)
                (groot--error 'groot-error--repo
                  (concat "%s: "
                          "Don't know where repository '%s' is located! "
                          "Add it to: %s%s%s.")
                  func-name
                  link-repo
                  ;; Add it to:
                  "`groot-repositories'"
                  (if (boundp 'autogit:repos:path/commit)
                      ", `autogit:repos:path/commit', `autogit:repos:path/watch'"
                    "")
                  ", or `magit-repository-directories'"))

              ;; `repo-path' is guaranteed to end in dir separator so we can just concat.
              (let ((path (groot--path-normalize :type 'file
                                                 :path (concat repo-path link-path))))
                (groot--path-assert (format "%s (path)" func-name) path :error? t :dir? nil)

                ;;------------------------------
                ;; Follow Link
                ;;------------------------------
                ;; Open the file...
                (find-file-other-window path)
                ;; ...and find the place in the file, if appropriate.
                (when (and (stringp link-location)
                           (not (string-empty-p link-location)))
                  (org-link-search link-location))

                ;; TODO: Search to location if we have one.
                ))

          ;;------------------------------
          ;; ERROR: Bad LINK
          ;;------------------------------
          (groot--error 'groot-error--link
            (concat "%s: "
                    "Invalid `groot' link: \"groot:%s\"! "
                    "Expected format of: \"repo-name:/relative/path/to/file.ext\". "
                    "Got repo-name: %S, path: %S")
            func-name
            link
            link-repo
            link-path))

      ;;------------------------------
      ;; Error Handling
      ;;------------------------------
      ;; If `debug-on-error' is enabled, errors will happen as usual.
      ;; Otherwise, downgrade to warning or just quietly return nil.
      ;;---
      ;; Our Error Types
      ;;---
      (groot-error (groot--org-api--handle-error "groot-link--open" error))
      ;;---
      ;; Any Other Error Types
      ;;---
      (error (groot--org-api--handle-error "groot-link--open" error)))))
;; groot-repositories
;; (push (cons "groot" (groot--repository-current-path :error? t)) groot-repositories)


;;------------------------------
;; TODO: More org link functionality!
;;------------------------------

;; ;;;###autoload
;; (defun groot-link--export (path desc format)
;;   (groot-export path desc format "status" 1))


;; ;;;###autoload
;; (defun groot-link--complete-link (&optional arg)
;;   (let ((default-directory (magit-read-repository arg)))
;;     (concat "groot:" (groot--repository-current-path))))


;; ;;; Export
;;
;; (defun groot-export (path desc format gitvar idx)
;;   (pcase-let* ((`(,dir ,rev) (split-string path "::"))
;;                (dir (groot--repository-name-to-path dir)))
;;     (if (file-exists-p dir)
;;         (let* ((default-directory dir)
;;                (remotes (magit-git-lines "remote"))
;;                (remote  (magit-get "groot.remote"))
;;                (remote  (cond ((length= remotes 1) (car remotes))
;;                               ((member remote remotes) remote)
;;                               ((member groot-remote remotes) groot-remote))))
;;           (if remote
;;               (if-let ((link
;;                         (or (and-let* ((url (magit-get "groot" gitvar)))
;;                               (format-spec url `((?r . ,rev))))
;;                             (and-let* ((url (magit-get "remote" remote "url"))
;;                                        (format (cl-find-if
;;                                                 (lambda (elt)
;;                                                   (string-match (car elt) url))
;;                                                 groot-export-alist)))
;;                               (format-spec (nth idx format)
;;                                            `((?n . ,(match-string 1 url))
;;                                              (?r . ,rev)))))))
;;                   (groot--format-export link desc format)
;;                 (signal 'org-link-broken
;;                         (list (format "Cannot determine public url for %s"
;;                                       path))))
;;             (signal 'org-link-broken
;;                     (list (format "Cannot determine public remote for %s"
;;                                   default-directory)))))
;;       (signal 'org-link-broken
;;               (list (format "Cannot determine public url for %s %s"
;;                             path "(which itself does not exist)"))))))
;;
;; (defun groot--format-export (link desc format)
;;   (pcase format
;;     ('html  (format "<a href=\"%s\">%s</a>" link desc))
;;     ('latex (format "\\href{%s}{%s}" link desc))
;;     ('ascii link)
;;     (_      link)))


;;--------------------------------------------------------------------------------
;; Config API
;;--------------------------------------------------------------------------------

;;;###autoload
(defun groot-path-in-repository? (path)
  "Return non-nil if PATH is or is in git repository."
  ;; 1. PATH must exist.
  (when-let* ((exists? (file-exists-p path))
              ;; 2. PATH must be in a git repository.
              (root (magit-toplevel path)))
    ;; 3. Top-level must be the ancestor of PATH or the PATH itself.
    (or (groot--path-descendant? path root)
        (string= (groot--path-normalize :type 'file :path root)
                 (groot--path-normalize :type 'file :path path)))))
;; (groot-path-in-repository? (buffer-file-name))
;; (groot-path-in-repository? (path:parent (buffer-file-name)))
;; (groot-path-repositories "~/.config/emacs-sn004/packages/user")


(defconst groot--path-rx-filename-ignore
  (list
   ;; Ignore "." and ".." entries.
   (rx-to-string '(sequence
                   string-start
                   (repeat 1 2 ".")
                   string-end)
                 :no-group))
  "What to ignore when traversing paths, getting children, etc..

NOTE: These should be compiled regex strings.")


(defun groot--path-ignore? (path regexes)
  "Return non-nil if PATH matches any of the REGEXES."
  (let ((func-name "groot--path-ignore?")
        (regex-list regexes) ;; Shallow copy so we can pop without possibly changing caller's list.
        ignore?)
    (unless (stringp path)
      (groot--error 'groot-error--path
        "%s: PATH must be a string! Got: %S"
        func-name
        path))
    (unless (listp regexes)
      (groot--error 'groot-error
        "%s: REGEXES must be a list of regex strings! Got: %S"
        func-name
        regexes))

    (while (and (not ignore?)
                regex-list)
      (let ((regex (pop regex-list)))
        (unless (stringp regex)
          (groot--error 'groot-error
            "%s: Regex must be a string! Got %s: %S"
            func-name
            (type-of regex)
            regex))

        (when (string-match regex path)
          ;; Set our return & stop-looping-early value.
          (setq ignore? t))))

    ignore?))
;; (groot--path-ignore? "x.y" groot--path-rx-filename-ignore)
;; (groot--path-ignore? "." groot--path-rx-filename-ignore)
;; (groot--path-ignore? ".." groot--path-rx-filename-ignore)
;; (groot--path-ignore? "..." groot--path-rx-filename-ignore)


(defun groot--path-children (parent-path)
  "Return immediate children directories of PARENT-PATH directory."
  (when (and (stringp parent-path)
             (file-directory-p parent-path))
    (let (child-dirs
          ;; Guarentee ourselves a dirpath.
          (parent-path (groot--path-normalize :type 'dir :path parent-path)))
      ;; Find all children files...
      (dolist (child (directory-files-and-attributes parent-path))
        (let* ((child-name  (car child))
               (child-path  (concat parent-path child-name))
               (child-attrs (cdr child)))
          ;;------------------------------
          ;; Save / Ignore Child
          ;;------------------------------
          ;; Explicitly ignore?
          (cond ((groot--path-ignore? child-name groot--path-rx-filename-ignore)
                 nil)

                ;;---
                ;; Save / Ignore by Type
                ;;---
                ;; We only care about directories, so check the attributes and ignore everything else.
                ((eq (file-attribute-type child-attrs) t) ; t is the attr type for directories.
                 ;; Save to results list.
                 (push child-path child-dirs))

                ((eq (file-attribute-type child-attrs) nil) ; nil is the attr type for files.
                 ;; Just ignore.
                 nil)

                ((stringp (file-attribute-type child-attrs)) ; The attr type for symlinks is a string of the path they point to.
                 ;; Just ignore.
                 nil)

                ;;---
                ;; Error: How did you get here?
                ;;---
                ;; ...are there any other attr types? There are other file types, like sockets.
                (t
                 (groot--error 'groot-error--path
                   "%s: '%s': Unhandled file-attribute-type: %S"
                   "groot--path-children"
                   child-path
                   (file-attribute-type child-attrs))))))

      ;;------------------------------
      ;; Done
      ;;------------------------------
      child-dirs)))
;; (groot--path-children (path:parent buffer-file-name))
;; (groot-path-repositories "~/.config/emacs-sn004/packages/user")


(defun groot--path-parent (path)
  "Return the parent directory of PATH.

Special Cases:
  1. If PATH is the root of the file system, return the root.
     (groot--path-parent \"/\")
       -> \"/\"
  2. If PATH is the top level of a relative path (e.g. \"this-dir\"), return
     empty string.
     (groot--path-parent \"this-dir\")
       -> \"\""
  (directory-file-name
   ;; `file-name-directory' returns nil for parent of top-level relative path:
   ;;   (file-name-directory "relative")
   ;;     -> nil
   ;; Avoid that; use empty string instead of nil.
   (or (file-name-directory
        ;; But first make sure PATH doesn't have a trailing slash.
        ;; Otherwise all we do is strip the slash.
        ;; Example: "/path/to/foo/" should have a parent of:
        ;; "/path/to" aka "/path/to/", not "/path/to/foo".
        (directory-file-name path))
       "")))
;; (groot--path-parent "/path/to/foo")
;; (groot--path-parent "/path/to/foo/")
;; Special Cases:
;; (groot--path-parent "/")
;; (groot--path-parent "relative-path")


(defun groot--path-child? (child parent)
  "Return non-nil if CHILD path is a direct child of PARENT path.

CHILD and PARENT cannot be the same path.

NOTE: CHILD and PARENT are normalized before comparing."
  (let ((child (groot--path-normalize :type 'file :path child))
        (parent (groot--path-normalize :type 'file :path parent)))
    (unless (string= child parent) ;; "/" is not a child of itself.
      (string= (groot--path-parent child)
               parent))))
;; (groot--path-child? "/path/to/foo/file.txt" "/path/to/foo")
;; (groot--path-child? "/path/to/foo/" "/path/to/foo/child/")


(defun groot--path-descendant? (descendant ancestor)
  "Return non-nil if DESCENDANT path is a descendant of ANCESTOR path.

DESCENDANT and ANCESTOR must be strings.
DESCENDANT and ANCESTOR cannot be the same path.

Will normalize the paths before comparing."
  (let ((descendant (groot--path-normalize :type 'file :path descendant))
        (ancestor (groot--path-normalize :type 'file :path ancestor)))
    (unless (string= descendant ancestor)
      (string-prefix-p ancestor
                       descendant))))
;; Yes:
;; (groot--path-descendant? "/foo/bar" "/foo")
;; (groot--path-descendant? "/foo/bar/" "/foo")
;; (groot--path-descendant? "/foo/bar" "/foo/")
;; (groot--path-descendant? "/foo/bar/" "/foo/")
;; No:
;; (groot--path-descendant? "/path/to/foo/" "/path/to/foo/child")


;;;###autoload
(defun groot-path-repositories (path)
  "Get all git repositories that are direct children of PATH.

Return list of absolute paths or nil."
  ;;------------------------------
  ;; Error Checking & Normalization
  ;;------------------------------
  (unless (stringp path)
    (groot--error 'groot-error--repo-path
      "groot-path-repositories: PATH must be a string! Got: %S"
      path))

  (setq path (groot--path-normalize :type 'file :path path))

  (unless (file-exists-p path)
    (groot--error 'groot-error--repo-path
      "groot-path-repositories: PATH does not exist! %s"
      path))
  (unless (file-directory-p path)
    (groot--error 'groot-error--repo-path
      "groot-path-repositories: PATH is not a directory! %s"
      path))

  ;;------------------------------
  ;; Find Git Repos.
  ;;------------------------------
  (let (repos)
    ;;---
    ;; First: Does the supplied directory contain repositories?
    ;;---
    ;; Check this first so we can actually find repos inside of other repos.
    (dolist (path-child (groot--path-children path))
      ;; Now... _What_ repo is this? Could have parent paths in this list if
      ;; we're inside a repo looking for other repos (e.g. in a git ignored
      ;; dir that contains `straight' package repos).
      (when (and (groot-path-in-repository? path-child) ; inside of some repo
                 (groot--path-descendant? (groot--repository-path-normalize path-child)
                                          path)) ; under the original path
        (push (groot--repository-path-normalize path-child) repos)))

    ;; If child repos were found, return the list of absolute paths to said repos.
    (cond (repos)
          ;;---
          ;; Second: Is the path itself (in) a repo?
          ;;---
          ;; If no children repos, check the path itself.
          ((groot-path-in-repository? path)
           ;; Yes; return the root of the repo.
           (list (groot--repository-path-normalize path))))))
;; (groot-path-repositories "~/.config")
;; (groot-path-repositories "~/")
;; (groot-path-repositories "~/.config/emacs-sn004/packages/user")

;;--------------------------------------------------------------------------------
;; Commands
;;--------------------------------------------------------------------------------

;;;###autoload
(defun groot-cmd-register (&optional arg)
  "Register `groot' with `org' as link type \"groot:\".

ARG can be:
  - nil        : register
  - prefix arg : unregister
  - non-nil    : unregister"
  (interactive "P")
  ;; Translate ARG into bool `register?'.
  (let ((register? (not arg)))
    (with-eval-after-load 'org
      ;; Do we need to wait for magit, or not? Assume not until proven so.

      ;; NOTE: See docstring for `org-link-parameters' for details of what
      ;; `org-link-set-parameters' takes as kwargs.
      ;;
      ;; NOTE: Manually unregister by setting values to nil:
      ;;   (org-link-set-parameters "groot" :store nil :follow nil :export nil :complete nil)
      (org-link-set-parameters "groot"
                               :store    (when register? #'groot-link--store)
                               :follow   (when register? #'groot-link--open)
                               ;; TODO: More org link functionality!
                               ;; :export   (when register? #'groot-link--export)
                               ;; :complete (when register? #'groot-link--complete-link)
                               ))

    (when (called-interactively-p)
      (message "groot %s org-link" (if register? "registered with" "unregistered from")))))


(defun groot-cmd-warn-on-error (&optional arg)
  "Enable, disable, or toggle `groot--org-api-warn-on-error', depending on ARG.

ARG should be:
  - nil, `:toggle', `toggle' : toggle
  - `:enable', `enable'      : enable
  - `:disable', `disable'    : disable"
  (interactive)

  (setq groot--org-api-warn-on-error
        (pcase arg
          ((or 'nil :toggle 'toggle)
           (not groot--org-api-warn-on-error))
          ((or :enable 'enable)
           t)
          ((or :disable 'disable)
           nil)))

  (when (called-interactively-p)
    (message "groot: warn-on-error %s"
             (if groot--org-api-warn-on-error "enabled" "disabled"))))


;;--------------------------------------------------------------------------------
;; Register with Org on Load
;;--------------------------------------------------------------------------------

;;;###autoload
(groot-cmd-register)


;;--------------------------------------------------------------------------------
;; The End.
;;--------------------------------------------------------------------------------
(provide 'groot)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; groot.el ends here
