for f in BiocharAG/R/*.R; do echo "### Content of file $f ###"; cat "$f"; echo ""; done > BiocharAG_complete.R
for f in scripts/*.R;     do echo "### Content of file $f ###"; cat "$f"; echo ""; done > combined_scripts.R
