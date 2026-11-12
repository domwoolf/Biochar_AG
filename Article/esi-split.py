"""Quarto post-render step for PDF output: split the rendered PDF into the article and the ESI.

The manuscript and its Electronic Supplementary Information are rendered as one document, so that
cross-references between them resolve. This writes <name>-article.pdf (pages before the ESI) and
<name>-ESI.pdf (from the ESI heading on), locating the ESI by its entry in the PDF outline.
The combined PDF is kept.
"""
import os
import sys

from pypdf import PdfReader, PdfWriter

ESI_TITLE = "Electronic Supplementary Information"


def esi_start(reader):
    """0-based index of the page with the ESI heading, from the PDF outline (top level)."""
    for item in reader.outline:
        if not isinstance(item, list) and item.title.strip().startswith(ESI_TITLE):
            return reader.get_destination_page_number(item)
    return None


def split(pdf):
    reader = PdfReader(pdf)
    start = esi_start(reader)
    if start is None:
        print(f"esi-split: no '{ESI_TITLE}' entry in the outline of {pdf}; not split", file=sys.stderr)
        return
    stem = pdf[:-4]
    for suffix, pages in (("article", range(0, start)), ("ESI", range(start, len(reader.pages)))):
        w = PdfWriter()
        for i in pages:
            w.add_page(reader.pages[i])
        with open(f"{stem}-{suffix}.pdf", "wb") as f:
            w.write(f)
    print(f"esi-split: {stem}-article.pdf (pages 1-{start}), {stem}-ESI.pdf (pages {start + 1}-{len(reader.pages)})")


if __name__ == "__main__":
    outputs = os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split()
    for f in outputs or ["BiomassTEA.pdf"]:
        if f.endswith(".pdf") and not f.endswith(("-article.pdf", "-ESI.pdf")) and os.path.exists(f):
            split(f)
