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
            tblpr += ZERO_MAR
        grid_start = start + body.index(grid.group(0))
        grid_new = re.sub(r'w:w="\d+"', f'w:w="{twips}"', grid.group(0))
        out += [xml[pos:start], tblpr, xml[end_pr:grid_start], grid_new]
        pos = grid_start + len(grid.group(0))
        n += 1
    out.append(xml[pos:])
    return "".join(out), n


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
