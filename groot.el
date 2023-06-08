;;; groot.el --- Support for Org links to Git repo & relative path.  -*- lexical-binding:t -*-
;;
;; Author:     Cole Brown <http://github/cole-brown>
;; Maintainer: Cole Brown <code@brown.dev>
;; Created:    2023-06-06
;; Modified:   2023-06-06
;; Homepage:   https://github.com/cole-brown/groot
;; Keywords:   hypermedia vc
;;
;; TODO:pkg: Add back in.
;; TODO:pkg: TODO-Package-Version: 1.9.0
;; TODO:pkg: TODO-Package-Requires: (
;;     (emacs "25.1")
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
  - PATH: string - absolute path to the repo's root directory"
  :group 'groot
  :type '(alist :key-type string :value-type directory))


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
         (if error?
             (error "%s: PATH must be a string or nil, got '%s': '%S'"
                    context
                    (type-of path)
                    path)
           nil))

        ((not (file-exists-p path))
         (if error?
             (error "%s: PATH does not exist? '%s'"
                    context
                    path)
           nil))

        ((and dir?
              (not (file-directory-p path)))
         (if error?
             (error "%s: PATH is not a directory? '%s'"
                    context
                    path)
           nil))

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
         (if error?
             (error "%s: NAME must be a string, got '%s': '%S'"
                    context
                    (type-of name)
                    name)
           nil))

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
    (groot--path-normalize (if (listp value)
                               (nth 0 value)
                             value))))
;; (setq groot-repositories '((".emacs.d" . "~/.config/emacs-sn004") ("personal" "~/.config/personal")))
;; (groot-repositories ".emacs.d")
;; (groot-repositories "personal")
;; (groot-repositories "does-not-exist1")


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

(cl-defun groot--path-normalize (path &key (file? nil))
  "Normalize PATH string.

If PATH is not a string, normalize to nil.
Else return PATH that is:
  - absolute
  - abbreviated (e.g. \"~\" instead of \"/home/username\")
  - FILE?:
    - nil    : directory path (i.e. must end in dir separator)
    - non-nil: file path (cannot end in dir separator)

NOTE: No error checking! Use `groot--path-assert' if desired."
  (when (stringp path)
    (abbreviate-file-name ; path abbreviations like "~"
     (funcall
      (if file?
          #'directory-file-name ; as file path
        #'file-name-as-directory) ; as directory path
      ;; absolute path
      (expand-file-name path)))))
;; (groot--path-normalize "~/.config")
;; (groot--path-normalize "/home/work/.config")
;; (groot--path-normalize "actual-package-stuff")
;; (groot--path-normalize default-directory)
;; (groot--path-normalize (buffer-file-name (buffer-base-buffer)))
;; (groot--path-normalize (buffer-file-name (buffer-base-buffer)) :file? t)
;; (groot--path-normalize 'hi)


(defun groot--name-normalize (path)
  "Get the name of the final directory in PATH.

PATH should be a directory path string.
If PATH is not a string, normalize name to nil.

NOTE: No error checking! Use `groot--path-assert' if desired."
  (when (stringp path)
    (file-name-nondirectory ; Get dir name from filepath.
     (directory-file-name ; Convert dirpath to filepath.
      (groot--path-normalize path)))))
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
    (groot--path-normalize (magit-toplevel path))))
;; (groot--repository-path-normalize nil)
;; (groot--repository-path-normalize "~/.config/emacs-sn004/mantle/config")


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
             (when (string= (groot--path-normalize entry-path)
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
;; (groot--repository-path-rooted (groot--repository-current-path) (groot--path-normalize (buffer-file-name (buffer-base-buffer)) :file? t))


;;--------------------------------------------------------------------------------
;; Current Repository / Path
;;--------------------------------------------------------------------------------

(cl-defun groot--repository-current-path (&key (error? t))
  "Get absolute path to current Git repository according to `default-directory'.

If ERROR? is nil, eat error signals and return nil instead.

Return absolute directory (ends in dir separator) path."
  (let ((path (groot--path-normalize default-directory)))
    ;; Require `default-directory' exist as a directory.
    (if (not (groot--path-assert "repository-current-path"
                                 path
                                 :error? error?
                                 :dir? t))
        ;; `path' is invalid but we didn't want an error signal, so just
        ;; make sure to not continue on doing stuff.
        nil

      ;; Figure out repo's root path.
      (groot--repository-path-normalize path))))
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


(cl-defun groot--path-current (&key (error? t))
  "Get absolute path to current file-backed buffer.

If ERROR? is nil, eat error signals and return nil instead.

Return absolute filepath."
  ;; Get filename of current buffer (if indirect, get filename of base buffer).
  (let ((path (groot--path-normalize (buffer-file-name (buffer-base-buffer)))))
    ;; Require the file actually exist? If that's annoying for some reason, can
    ;; remove or make another kwarg...
    (if (not (groot--path-assert "path-current"
                                 path
                                 :error? error?
                                 :dir? nil))
        ;; `path' is invalid but we didn't want an error signal, so just
        ;; make sure to not continue on doing stuff.
        nil

      ;; Nothing more to do.
      path)))
;; (groot--path-current)
;; (let (default-directory) (groot--path-current))
;; (let (default-directory) (groot--path-current :error? nil))


;;--------------------------------------------------------------------------------
;; Git-Rooted Paths
;;--------------------------------------------------------------------------------

;;;###autoload
(with-eval-after-load 'org
  ;; Do we need to wait for magit, or not? Assume not until proven so.
  ;; (with-eval-after-load 'magit
  (org-link-set-parameters "groot"
                           :store    #'groot-link--store
                           :follow   #'groot-link--open
                           ;; TODO: More org link functionality!
                           ;; :export   #'groot-link--export
                           ;; :complete #'groot-link--complete-link
                           )

  ;; NOTE: Unregister like so:
  ;;   (org-link-set-parameters "groot" :store nil :follow nil :export nil :complete nil)
  )


;;;###autoload
(defun groot-link--store ()
  "Store a link to a file in a Git repository.

Link will be in format:
  - groot:repo-name:/path/rooted/in/repo.ext"
  ;;------------------------------
  ;; Sanity Checks
  ;;------------------------------
  (let ((repo-path (groot--repository-current-path :error? t))
        (repo-name (groot--repository-current-name :error? t))
        (buffer-path (groot--path-normalize (buffer-file-name (buffer-base-buffer))
                                            :file? t)))
    (groot--path-assert "groot-link-store"
                        buffer-path
                        :error? t
                        :dir? nil)

    ;;------------------------------
    ;; Create the Link
    ;;------------------------------
    (org-link-store-props
     :type        "groot"
     :link        (format "groot:%s:/%s"
                          repo-name
                          ;; Convert full path to relative/rooted path.
                          (groot--repository-path-rooted repo-path buffer-path))
     ;; No description for now?
     ;; :description (format "groot:%s:/%s"
     ;;                      repo-name
     ;;                      (string-trim-left buffer-path repo-path))
     )))
;; (groot-link--store)


;;;###autoload
(defun groot-link--open (link)
  "Open a groot link.

LINK should be in format:
  - repo-name:/path/rooted/in/repo.ext"
  (if-let* ((link-parts (split-string link ":/"))
            (link-name (nth 0 link-parts))
            (link-path (nth 1 link-parts)))
      (let ((repo-path (groot--repository-name-to-path link-name)))
        ;;------------------------------
        ;; Error Checks
        ;;------------------------------
        (groot--name-assert "groot-link-open (link-name)" link-name :error? t)
        (if repo-path
            (groot--path-assert "groot-link-open (repo-path)" repo-path :error? t :dir? t)
          (error (concat "%s: "
                         "Don't know where repository '%s' is located! "
                         "Add it to: %s%s%s.")
                 "groot-link-open"
                 link-name
                 ;; Add it to:
                 "`groot-repositories'"
                 (if (boundp 'autogit:repos:path/commit)
                     ", `autogit:repos:path/commit', `autogit:repos:path/watch'"
                   "")
                 ", or `magit-repository-directories'"))

        ;; `repo-path' is guaranteed to end in dir separator.
        (let ((path (groot--path-normalize (concat repo-path link-path)
                                           :file? t)))
          (groot--path-assert "groot-link-open (path)" path :error? t :dir? nil)
          (groot--path-assert "groot-link-open (path)" path :error? t :dir? nil)

          ;;------------------------------
          ;; Follow Link
          ;;------------------------------
          (find-file-other-window path)))

    ;;------------------------------
    ;; ERROR: Bad LINK
    ;;------------------------------
    (error (concat "%s: "
                   "Invalid `groot' link: \"groot:%s\"! "
                   "Expected format of: \"repo-name:/relative/path/to/file.ext\". "
                   "Got repo-name: %S, path: %S")
           "groot-link-open"
           link
           link-name
           link-path)))
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
;; Commands
;;--------------------------------------------------------------------------------

;; NOTE: ...org doesn't make this easy... See function `org-store-link'.
;; TL;DR: Big fucking mess of a function. No interface for others to do
;; anything like what I wanted to do here?!
;; So... Oh well?
;; Just use `org-store-link', you pleb?
;;
;; ;;;###autoload
;; (defun groot-store-link (_prefix)
;;   "Store an org link of type 'groot'.
;;
;; Like `org-store-link' but specific to 'groot'. In case you don't want 'groot'
;; taking over all Git repo links?"
;;   (interactive "P")
;;   (groot-link--store))



;;--------------------------------------------------------------------------------
;; The End.
;;--------------------------------------------------------------------------------
(provide 'groot)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; groot.el ends here
