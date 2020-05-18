


syn keyword Function def
syn match Identifier /\v[a-zA-Z_][a-zA-Z_]+/
syn match Statement /\v([{\n }[a-zA-Z][}\n ])|[{}]/
syn match Number /\v[0-9]+/

highlight Function ctermfg=214
