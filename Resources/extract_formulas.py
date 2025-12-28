
import xml.etree.ElementTree as ET
import os

base_dir = "temp_xl/xl"
workbook_path = os.path.join(base_dir, "workbook.xml")

# 1. Parse workbook.xml to map names to sheet IDs
ns = {'cols': 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'}
tree = ET.parse(workbook_path)
root = tree.getroot()

sheets = {}
for sheet in root.findall(".//{http://schemas.openxmlformats.org/spreadsheetml/2006/main}sheet"):
    name = sheet.get("name")
    sheet_id = sheet.get("sheetId") # internal ID
    r_id = sheet.get("{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id")
    sheets[name] = r_id

print("Sheet mapping:", sheets)

# 2. Parse relationships to find filenames
rel_path = os.path.join(base_dir, "_rels/workbook.xml.rels")
rel_tree = ET.parse(rel_path)
rel_root = rel_tree.getroot()

sheet_files = {}
for rel in rel_root.findall(".//{http://schemas.openxmlformats.org/package/2006/relationships}Relationship"):
    r_id = rel.get("Id")
    target = rel.get("Target")
    if r_id in sheets.values():
        # find the sheet name for this r_id
        for name, rid in sheets.items():
            if rid == r_id:
                sheet_files[name] = target

print("Sheet files:", sheet_files)

# 3. Define function to extract formulas from a sheet
def extract_formulas(sheet_name):
    filename = sheet_files.get(sheet_name)
    if not filename:
        print(f"Sheet {sheet_name} not found.")
        return

    # Target is relative to xl/, e.g., "worksheets/sheet1.xml"
    full_path = os.path.join(base_dir, filename)

    print(f"\n--- Formulas in {sheet_name} ({filename}) ---")
    try:
        stree = ET.parse(full_path)
        sroot = stree.getroot()
        # Namespace usually required
        rows = sroot.findall(".//{http://schemas.openxmlformats.org/spreadsheetml/2006/main}row")

        for row in rows[:20]: # Check first 20 rows
            r_idx = row.get("r")
            cells = row.findall("{http://schemas.openxmlformats.org/spreadsheetml/2006/main}c")
            for cell in cells:
                ref = cell.get("r")
                f = cell.find("{http://schemas.openxmlformats.org/spreadsheetml/2006/main}f")
                v = cell.find("{http://schemas.openxmlformats.org/spreadsheetml/2006/main}v")

                if f is not None and f.text:
                   val = v.text if v is not None else "(no val)"
                   print(f"Cell {ref}: = {f.text} (Value: {val})")

    except Exception as e:
        print(f"Error reading {sheet_name}: {e}")

# Extract for relevant sheets
extract_formulas("bes")
extract_formulas("beccs")
extract_formulas("bebcs")
