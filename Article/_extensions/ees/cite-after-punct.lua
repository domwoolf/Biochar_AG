-- Citation placement for RSC numbered superscript references. Runs before citeproc.
--
-- 1. A bracketed citation directly followed by . , ; or : is moved after that mark, and the space
--    before it is dropped ("text [@a]." -> "text.¹").
-- 2. An author-in-text citation ("@smith2020 found") is written as the author name followed by the
--    superscript number ("Smith et al.¹ found"), because a numeric style otherwise prints the number
--    alone. Names follow RSC: one or two authors by surname, three or more as "First et al.";
--    an organizational author by its full name.

local stringify = pandoc.utils.stringify
local PUNCT = "^([%.,;:])(.*)$"

local function move_after_punct(inl)
  local i = 1
  while i <= #inl do
    local c = inl[i]
    local nxt = inl[i + 1]
    if c.t == "Cite" and c.citations[1].mode == "NormalCitation" and nxt and nxt.t == "Str" then
      local p, rest = nxt.text:match(PUNCT)
      if p then
        local prev = inl[i - 1]
        if prev and (prev.t == "Space" or prev.t == "SoftBreak") then
          inl:remove(i - 1)
          i = i - 1
        end
        inl[i] = pandoc.Str(p)
        inl[i + 1] = c
        if rest ~= "" then
          inl:insert(i + 2, pandoc.Str(rest))
        end
        i = i + 1
      end
    end
    i = i + 1
  end
  return inl
end

local function author_label(ref)
  local names = ref.author or ref.editor
  if not names or #names == 0 then
    return nil
  end
  local function name(a)
    if a.literal then
      return stringify(a.literal)
    end
    local fam = stringify(a.family or "")
    local part = a["non-dropping-particle"] and stringify(a["non-dropping-particle"])
    return (part and part ~= "") and (part .. " " .. fam) or fam
  end
  if #names == 1 then
    return name(names[1])
  elseif #names == 2 then
    return name(names[1]) .. " and " .. name(names[2])
  else
    return name(names[1]) .. " et al."
  end
end

function Pandoc(doc)
  local labels = {}
  for _, ref in ipairs(pandoc.utils.references(doc)) do
    labels[stringify(ref.id)] = author_label(ref)
  end
  doc = doc:walk({ Inlines = move_after_punct })
  return doc:walk({
    Cite = function(c)
      if #c.citations ~= 1 or c.citations[1].mode ~= "AuthorInText" then
        return nil
      end
      local label = labels[c.citations[1].id]
      if not label then
        return nil
      end
      local cit = c.citations[1]
      cit.mode = "NormalCitation"
      local out = pandoc.Inlines(pandoc.read(label, "markdown").blocks[1].content)
      out:insert(pandoc.Cite({}, { cit }))
      return out
    end,
  })
end
