-- Jupyter export only: collapsible callouts do not exist in .ipynb, so their
-- content (including code cells and figures) is shown directly, under a
-- small heading with the callout title.
function Callout(el)
  local blocks = pandoc.List()
  if el.title then
    blocks:insert(pandoc.Header(4, pandoc.utils.stringify(el.title)))
  end
  blocks:extend(el.content)
  return blocks
end
