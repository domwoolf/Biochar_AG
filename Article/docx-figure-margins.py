"""Quarto post-render step for Word output.

Quarto places each labelled figure in a one-cell layout table. The "Table" style gives that cell
108-twip left and right margins, so an image at 100% of the text width overflows the cell
and Quarto writes a fixed column width of 5.5 in (7920 twips) whatever the text width. Word and
LibreOffice honor the 100% table width, but Google Docs uses the fixed column width and crops the
image. For figure tables only, this sets the column and table widths to the image width and zeroes
the left/right cell margins; data tables are unchanged.
"""
import os
import re
import shutil
import sys
import tempfile
import zipfile

ZERO_MAR = ('<w:tblCellMar><w:left w:type="dxa" w:w="0" />'
            '<w:right w:type="dxa" w:w="0" /></w:tblCellMar>')


# Order of tblPr children required by the WordprocessingML schema (CT_TblPr). Word desktop tolerates
# other orders; Word Online reports the document as corrupt (e.g. flextable writes jc after tblLayout).
TBLPR_ORDER = ["tblStyle", "tblpPr", "tblOverlap", "bidiVisual", "tblStyleRowBandSize", "tblStyleColBandSize",
               "tblW", "jc", "tblCellSpacing", "tblInd", "tblBorders", "shd", "tblLayout", "tblCellMar", "tblLook",
               "tblCaption", "tblDescription", "tblPrChange"]


def _children(inner):
    """Split the content of an element into its top-level child elements (strings)."""
    out, pos = [], 0
    while True:
        m = re.compile(r"<w:(\w+)\b").search(inner, pos)
        if not m:
            return out
        name, i = m.group(1), m.start()
        tag_end = inner.index(">", i)
        if inner[tag_end - 1] == "/":
            end = tag_end + 1
        else:  # find the matching close tag, allowing nested elements of the same name
            depth, j = 1, tag_end + 1
            while depth:
                n = re.compile(r"<(/?)w:" + name + r"\b[^>]*?(/?)>").search(inner, j)
                if n.group(1):
                    depth -= 1
                elif not n.group(2):
                    depth += 1
                j = n.end()
            end = j
        out.append((name, inner[i:end]))
        pos = end


def fix_order(xml):
    """Put the children of every tblPr in schema order."""
    def reorder(m):
        kids = _children(m.group(1))
        rank = {n: k for k, n in enumerate(TBLPR_ORDER)}
        kids.sort(key=lambda c: rank.get(c[0], len(TBLPR_ORDER)))
        return "<w:tblPr>" + "".join(c[1] for c in kids) + "</w:tblPr>"
    return re.sub(r"<w:tblPr>(.*?)</w:tblPr>", reorder, xml, flags=re.S)


def fix_xml(xml):
    out, pos, n = [], 0, 0
    for m in re.finditer(r"<w:tbl>", xml):
        start = m.end()
        # content of this table up to the next nested table or its end
        nxt = re.search(r"<w:tbl>|</w:tbl>", xml[start:])
        body = xml[start:start + nxt.start()] if nxt else xml[start:]
        grid = re.search(r"<w:tblGrid>(.*?)</w:tblGrid>", body)
        if not (grid and grid.group(1).count("<w:gridCol") == 1 and "<w:drawing" in body):
            continue
        ext = re.search(r'<wp:extent cx="(\d+)"', body)
        if not ext:
            continue
        twips = round(int(ext.group(1)) / 635)  # 635 EMU per twip
        end_pr = xml.find("</w:tblPr>", start)
        tblpr = re.sub(r'<w:tblW [^>]*/>', f'<w:tblW w:type="dxa" w:w="{twips}" />', xml[start:end_pr])
        if "<w:tblCellMar>" not in tblpr:
            # The schema fixes the order of tblPr children: tblCellMar precedes tblLook, tblCaption and
            # tblDescription. Word Online reports the file as corrupt if it follows them.
            later = re.search(r"<w:(tblLook|tblCaption|tblDescription)\b", tblpr)
            at = later.start() if later else len(tblpr)
            tblpr = tblpr[:at] + ZERO_MAR + tblpr[at:]
        grid_start = start + body.index(grid.group(0))
        grid_new = re.sub(r'w:w="\d+"', f'w:w="{twips}"', grid.group(0))
        out += [xml[pos:start], tblpr, xml[end_pr:grid_start], grid_new]
        pos = grid_start + len(grid.group(0))
        n += 1
    out.append(xml[pos:])
    return fix_order("".join(out)), n


def fix_docx(path):
    tmp = tempfile.NamedTemporaryFile(delete=False, suffix=".docx", dir=os.path.dirname(path) or ".")
    tmp.close()
    with zipfile.ZipFile(path) as zin, zipfile.ZipFile(tmp.name, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "word/document.xml":
                xml, n = fix_xml(data.decode("utf-8"))
                data = xml.encode("utf-8")
            zout.writestr(item, data)
    shutil.move(tmp.name, path)
    print(f"docx-figure-margins: set widths and margins of {n} figure tables in {path}")


if __name__ == "__main__":
    files = sys.argv[1:] or os.environ.get("QUARTO_PROJECT_OUTPUT_FILES", "").split("\n")
    for f in files:
        if f.endswith(".docx") and os.path.exists(f):
            fix_docx(f)
