# support for accessing emacs bookmarks in bash


# eval elisp code from stdin
cde_elisp_eval () {
    local temp_fname=$(mktemp)
    cat > $temp_fname
    emacs --no-init-file --no-site-file --batch --load $temp_fname
    rm -f $temp_fname;
}

CDE_ELISP_COMMON="
(require 'cl)
(require 'bookmark)
(defun dir-p (location)
  (char-equal (elt location (1- (length location))) ?/))
(defun bookmark-to-line (name)
  (let ((bookmark (bookmark-get-bookmark name)))
    (if bookmark
	(format \"\\\"%s\\\" \\\"%s\\\"\" name (bookmark-get-filename bookmark)))))
"


# print all bookmarks: one bookmark per line in such format: "<name>" "<path>"
cde_all_bookmarks () {
    cde_elisp_eval <<EOF
$CDE_ELISP_COMMON
(dolist (name (remove-if-not (lambda (name) (dir-p (bookmark-location name)))
			     (bookmark-all-names)))
  (princ (format "%s\n" (bookmark-to-line name))))
EOF
}

# print the path for a bookmark or empty if a bookmark is not found
cde_get_bookmark () {
    local name="$1"
    cde_elisp_eval <<EOF
$CDE_ELISP_COMMON
(let ((location (bookmark-location "$name")))
  (if location (princ location)))
EOF
}

# cd to bookmark
cde () {
    local name="$1"
    local path=$(cde_get_bookmark $name)
    eval path=$path
    [ -n "$path" ] && cd $path
}

_cde () {
    [ $COMP_CWORD -eq 1 ] && COMPREPLY=($(compgen -W "$(cde_all_bookmarks|cut -d' ' -f1)" ${COMP_WORDS[$COMP_CWORD]}))
}
complete -o nospace -F _cde cde

# FIXME: maybe someday I will make cd to accept normal directories and bookmarks.
_cde_cd () {
    local merged=()
    _cde
    merged=$COMPREPLY
    _cd
    merged=(${COMPREPLY[@]} ${merged[@]})
    COMPREPLY=($(compgen -W "${merged[*]}" "$2"))
}
#complete -o nospace -F _cde_cd cd
