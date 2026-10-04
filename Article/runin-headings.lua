-- Science Advances style: fourth-level headings are italic, end in a period and run into the text.
-- Markdown stays "#### Heading"; this filter merges each level-4 heading into the paragraph after it.
function Blocks(blocks)
  local out = pandoc.Blocks({})
  local i = 1
  while i <= #blocks do
    local b, nxt = blocks[i], blocks[i + 1]
    if b.t == "Header" and b.level == 4 and nxt and nxt.t == "Para" then
      local lead = b.content:clone()
      local last = lead[#lead]
      if not (last and last.t == "Str" and last.text:match("%.$")) then lead:insert(pandoc.Str(".")) end
      local inl = pandoc.Inlines({pandoc.Emph(lead), pandoc.Space()})
      inl:extend(nxt.content)
      out:insert(pandoc.Para(inl))
      i = i + 2
    else
      out:insert(b)
      i = i + 1
    end
  end
  return out
end
