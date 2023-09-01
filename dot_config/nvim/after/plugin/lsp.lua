local lsp = require('lsp-zero')

lsp.preset('recommended')
lsp.ensure_installed({
	-- 'tsserver',
	-- 'eslint',
	'lua_ls',
  -- 'rust_analyzer',
	-- 'pyright',
})

lsp.setup()
