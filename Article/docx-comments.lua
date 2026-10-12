-- docx-comments.lua
-- Word output only: HTML comments (<!-- TODO ... -->, <!-- STALE ... -->, <!--# note -->) are normally dropped
-- from .docx. This filter shows them to co-authors as yellow-highlighted text in square brackets.
-- Other formats are left unchanged.

if not FORMAT:match("docx") then
  return {}
end

local function xml_escape(s)
  return (s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;"))
end

-- Text of an HTML comment, or nil if the raw HTML is not a comment
local function comment_text(raw)
  local body = raw:match("^%s*<!%-%-(.-)%-%->%s*$")
  if not body then return nil end
  body = body:gsub("^#", ""):gsub("%s+", " "):gsub("^%s+", ""):gsub("%s+$", "")
  return body
end

local function highlighted_run(text)
  return '<w:r><w:rPr><w:highlight w:val="yellow"/></w:rPr><w:t xml:space="preserve">[' ..
    xml_escape(text) .. ']</w:t></w:r>'
end

function RawInline(el)
  if el.format ~= "html" then return nil end
  local text = comment_text(el.text)
  if not text then return nil end
  return pandoc.RawInline("openxml", highlighted_run(text))
end

function RawBlock(el)
  if el.format ~= "html" then return nil end
  local text = comment_text(el.text)
  if not text then return nil end
  return pandoc.RawBlock("openxml", "<w:p>" .. highlighted_run(text) .. "</w:p>")
end
