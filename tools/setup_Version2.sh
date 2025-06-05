#!/bin/bash

# Create base directory
mkdir -p ~/.emacs.default/{elisp,literate,straight}
chmod 755 ~/.emacs.default
chmod 755 ~/.emacs.default/{elisp,literate,straight}

# Create early-init.el if it doesn't exist
if [ ! -f ~/.emacs.default/early-init.el ]; then
    touch ~/.emacs.default/early-init.el
    chmod 644 ~/.emacs.default/early-init.el
fi

# Create init.el if it doesn't exist
if [ ! -f ~/.emacs.default/init.el ]; then
    touch ~/.emacs.default/init.el
    chmod 644 ~/.emacs.default/init.el
fi

# Create custom.el if it doesn't exist
if [ ! -f ~/.emacs.default/custom.el ]; then
    touch ~/.emacs.default/custom.el
    chmod 644 ~/.emacs.default/custom.el
fi

# Move any existing .org files to literate directory
for f in ~/.emacs.default/*.org; do
    if [ -f "$f" ]; then
        mv "$f" ~/.emacs.default/literate/
        chmod 644 ~/.emacs.default/literate/"$(basename "$f")"
    fi
done

echo "Setup complete. Directory structure:"
tree ~/.emacs.default