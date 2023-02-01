local ls = require 'luasnip'

local s = ls.snippet -- s(context, nodes, opts) -> snippet

local t = ls.text_node -- static text
local i = ls.insert_node
local f = ls.function_node -- be generated from the contents of other node, return text
local c = ls.choice_node
-- local r = ls.restore_node
-- local d = ls.dynamic_node -- return a snippetNode
local sn = ls.snippet_node
-- local isn = ls.indent_snippet_node

-- local ai = require("luasnip.nodes.absolute_indexer")

-- local rep = require("luasnip.extras").rep
local p = require('luasnip.extras').partial
local fmt = require('luasnip.extras.fmt').fmt
local fmta = require('luasnip.extras.fmt').fmta

local postfix = require('luasnip.extras.postfix').postfix

local function bash(_, snip)
  local file = io.popen(snip.trigger, 'r')
  local res = {}
  for line in file:lines() do
    table.insert(res, line)
  end
  return res
end

local calculate_comment_string = require('Comment.ft').calculate
local region = require('Comment.utils').get_region

local get_cstring = function(ctype)
  local cstring = calculate_comment_string { ctype = ctype, range = region() } or ''
  local cstring_table = vim.split(cstring, '%s', { plain = true, trimempty = true })
  if #cstring_table == 0 then
    return { '', '' } -- default
  end
  return #cstring_table == 1 and { cstring_table[1], '' } or { cstring_table[1], cstring_table[2] }
end

local marks = {
  signature = function()
    return fmt('<{}>', i(1, store.luasnip.username))
  end,
  signature_with_email = function()
    return fmt('<{}{}>', { i(1, store.luasnip.username), i(2, ' ' .. store.luasnip.email) })
  end,
  date_signature_with_email = function()
    return fmt(
      '<{}{}{}>',
      { i(1, os.date '%d-%m-%y'), i(2, ', ' .. store.luasnip.username), i(3, ' ' .. store.luasnip.email) }
    )
  end,
  date_signature = function()
    return fmt('<{}{}>', { i(1, os.date '%d-%m-%y'), i(2, ', ' .. store.luasnip.username) })
  end,
  date = function()
    return fmt('<{}>', i(1, os.date '%d-%m-%y'))
  end,
  empty = function()
    return t ''
  end,
}

local todo_snippet_nodes = function(aliases, opts)
  local aliases_nodes = vim.tbl_map(function(alias)
    return i(nil, alias) -- generate choices for [name-of-comment]
  end, aliases)
  local sigmark_nodes = {} -- choices for [comment-mark]
  for _, mark in pairs(marks) do
    table.insert(sigmark_nodes, mark())
  end
  -- format them into the actual snippet
  local comment_node = fmta('<> <> <> <> <><>', {
    f(function()
      return get_cstring(opts.ctype)[1] -- get <comment-string[1]>
    end),
    c(1, aliases_nodes), -- [name-of-comment]
    i(3), -- {comment-text}
    c(2, sigmark_nodes), -- [comment-mark]
    f(function()
      return get_cstring(opts.ctype)[2] -- get <comment-string[2]>
    end),
    i(0),
  })
  return comment_node
end

local todo_snippet = function(context, aliases, opts)
  opts = opts or {}
  aliases = type(aliases) == 'string' and { aliases } or aliases -- if we do not have aliases, be smart about the function parameters
  context = context or {}
  if not context.trig then
    return error("context doesn't include a `trig` key which is mandatory", 2) -- all we need from the context is the trigger
  end
  opts.ctype = opts.ctype or 1 -- comment type can be passed in the `opts` table, but if it is not, we have to ensure, it is defined
  local alias_string = table.concat(aliases, '|') -- `choice_node` documentation
  context.name = context.name or (alias_string .. ' comment') -- generate the `name` of the snippet if not defined
  context.dscr = context.dscr or (alias_string .. ' comment with a signature-mark') -- generate the `dscr` if not defined
  context.docstring = context.docstring or (' {1:' .. alias_string .. '}: {3} <{2:mark}>{0} ') -- generate the `docstring` if not defined
  local comment_node = todo_snippet_nodes(aliases, opts) -- nodes from the previously defined function for their generation
  return s(context, comment_node, opts) -- the final todo-snippet constructed from our parameters
end

local todo_snippet_specs = {
  { { trig = 'todo' }, 'TODO' },
  { { trig = 'fix' }, { 'FIX', 'BUG', 'ISSUE', 'FIXIT' } },
  { { trig = 'hack' }, 'HACK' },
  { { trig = 'warn' }, { 'WARN', 'WARNING', 'XXX' } },
  { { trig = 'perf' }, { 'PERF', 'PERFORMANCE', 'OPTIM', 'OPTIMIZE' } },
  { { trig = 'note' }, { 'NOTE', 'INFO' } },
  -- NOTE: Block commented todo-comments
  { { trig = 'todob' }, 'TODO', { ctype = 2 } },
  { { trig = 'fixb' }, { 'FIX', 'BUG', 'ISSUE', 'FIXIT' }, { ctype = 2 } },
  { { trig = 'hackb' }, 'HACK', { ctype = 2 } },
  { { trig = 'warnb' }, { 'WARN', 'WARNING', 'XXX' }, { ctype = 2 } },
  { { trig = 'perfb' }, { 'PERF', 'PERFORMANCE', 'OPTIM', 'OPTIMIZE' }, { ctype = 2 } },
  { { trig = 'noteb' }, { 'NOTE', 'INFO' }, { ctype = 2 } },
}

local todo_comment_snippets = {}
for _, v in ipairs(todo_snippet_specs) do
  table.insert(todo_comment_snippets, todo_snippet(v[1], v[2], v[3]))
end

ls.add_snippets('all', todo_comment_snippets, { type = 'snippets', key = 'todo_comments' })

---------------------------- Snippets  ------------------------------------------------
local snippets = {
  s('yy', p(os.date, '%Y')),

  s({ trig = 'ymd', name = 'Current date', dscr = 'Insert the current date' }, {
    p(os.date, '%Y-%m-%d'),
  }),

  s(
    'dt',
    f(function()
      return os.date '%D - %H:%M'
    end)
  ),

  s({ trig = 'pwd' }, { f(bash, {}) }),

  postfix('!', {
    f(function(_, parent)
      return '!' .. parent.snippet.env.POSTFIX_MATCH
    end, {}),
  }),
  postfix('&', {
    f(function(_, parent)
      return '&' .. parent.snippet.env.POSTFIX_MATCH
    end, {}),
  }),
  postfix('*', {
    f(function(_, parent)
      return '&' .. parent.snippet.env.POSTFIX_MATCH
    end, {}),
  }),
}

return snippets
