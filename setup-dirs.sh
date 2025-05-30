#!/usr/bin/env bash

# Define the base directory (change if not using ~/.emacs.d/)
EMACS_DIR="${HOME}/.emacs.default"
SNIPPET_DIR="${EMACS_DIR}/snippets"
TEMPLATE_DIR="${EMACS_DIR}/templates"
HTML_DIR="${EMACS_DIR}/html"

# Create directories
mkdir -p "${SNIPPET_DIR}/emacs-lisp-mode"
mkdir -p "${SNIPPET_DIR}/org-mode"
mkdir -p "${TEMPLATE_DIR}"
mkdir -p "${HTML_DIR}/static"
mkdir -p "${HTML_DIR}/docs"

# Create Emacs Lisp mode snippet
cat << 'EOF' > "${SNIPPET_DIR}/emacs-lisp-mode/hello-world"
# -*- mode: snippet -*-
# name: Hello World
# key: hw
# --
(message "Hello, world!")
EOF

# Create Org mode snippet
cat << 'EOF' > "${SNIPPET_DIR}/org-mode/todo-item"
# -*- mode: snippet -*-
# name: Org TODO Item
# key: todo
# --
* TODO ${1:Task description}
  DEADLINE: ${2:`(org-insert-time-stamp (current-time))`}
$0
EOF

# Create Tempel template
cat << 'EOF' > "${TEMPLATE_DIR}/tempel.eld"
((hello
  "Hello, " (p "name") "!")
 (defun
  "(defun " (p "name") " (" (p "args") ")\n  " (p "body") "\n)")
 (org-block
  "#+begin_src " (p "lang") "\n" r "\n#+end_src"))
EOF

# Confirm success
echo "✅ Snippet and template directories created and populated in:"
echo "   ${SNIPPET_DIR} and ${TEMPLATE_DIR}"
