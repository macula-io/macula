#!/bin/bash
# Fix XML-unsafe characters in Erlang @doc comments
#
# EDoc/ExDoc uses XML parsing, so characters like < and > need to be escaped
# in documentation comments. This script escapes << and >> in comment lines.

set -e

cd "$(dirname "$0")/.."

echo "Fixing XML-unsafe characters in @doc comments..."

# Find all doc comment lines with << or >> and escape them
# In EDoc/ExDoc, we should use &lt;&lt; and &gt;&gt; for binary syntax

for file in src/*.erl; do
    if [ -f "$file" ]; then
        # Create backup
        cp "$file" "$file.bak"

        # Replace << with &lt;&lt; and >> with &gt;&gt; in ALL comment lines (starting with %%)
        # Also escape standalone & as &amp;
        perl -i -pe 's/(^\%\%.*?)<<(.*?)>>/$1\&lt;\&lt;$2\&gt;\&gt;/g' "$file"
        perl -i -pe 's/(^\%\%.*?) & /$1 \&amp; /g' "$file"

        echo "  Fixed: $(basename $file)"
    fi
done

echo ""
echo "Done! Backups saved as .bak files"
echo ""
echo "To test the documentation generation:"
echo "  rebar3 ex_doc"
echo ""
echo "To revert changes if needed:"
echo "  for f in src/*.erl.bak; do mv \"\$f\" \"\${f%.bak}\"; done"
