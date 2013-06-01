" Vim syntax support file
" Maintainer:	Bram Moolenaar <Bram@vim.org>
" Last Change:	2001 Sep 12

" This file sets up the default methods for highlighting.
" It is loaded from "synload.vim" and from Vim for ":syntax reset".
" Also used from init_highlight().

if !exists("syntax_cmd") || syntax_cmd == "on"
  " ":syntax on" works like in Vim 5.7: set colors but keep links
  command -nargs=* SynColor hi <args>
  command -nargs=* SynLink hi link <args>
else
  if syntax_cmd == "enable"
    " ":syntax enable" keeps any existing colors
    command -nargs=* SynColor hi def <args>
    command -nargs=* SynLink hi def link <args>
  elseif syntax_cmd == "reset"
    " ":syntax reset" resets all colors to the default
    command -nargs=* SynColor hi <args>
    command -nargs=* SynLink hi! link <args>
  else
    " User defined syncolor file has already set the colors.
    finish
  endif
endif

" Many terminals can only use six different colors (plus black and white).
" Therefore the number of colors used is kept low. It doesn't look nice with
" too many colors anyway.
" Careful with "it changes the color to bright for some terminals.
" There are two sets of defaults: for a dark and a light background.
if &background == "dark"
  SynColor Comment	gui=NONE guifg=#80a0ff guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=232
  SynColor Constant	gui=NONE guifg=#ffa0a0 guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=233
  SynColor Special	gui=NONE guifg=Orange guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=234
  SynColor Identifier	gui=NONE guifg=#40ffff guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=235
  SynColor Statement	gui=bold guifg=#ffff60 guibg=NONE                              cterm=bold ctermbg=NONE ctermfg=236
  SynColor PreProc	gui=NONE guifg=#ff80ff guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=237
  SynColor Type		gui=bold guifg=#60ff60 guibg=NONE                              cterm=bold ctermbg=NONE ctermfg=238
  SynColor Underlined	gui=underline guifg=#80a0ff                              cterm=underline ctermfg=232
  SynColor Ignore	gui=NONE guifg=bg guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=NONE
else
  SynColor Comment	gui=NONE guifg=Blue guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=21
  SynColor Constant	gui=NONE guifg=Magenta guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=13
  SynColor Special	gui=NONE guifg=SlateBlue guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=239
  SynColor Identifier	gui=NONE guifg=DarkCyan guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=240
  SynColor Statement	gui=bold guifg=Brown guibg=NONE                              cterm=bold ctermbg=NONE ctermfg=241
  SynColor PreProc	gui=NONE guifg=Purple guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=242
  SynColor Type		gui=bold guifg=SeaGreen guibg=NONE                              cterm=bold ctermbg=NONE ctermfg=243
  SynColor Underlined	gui=underline guifg=SlateBlue                              cterm=underline ctermfg=239
  SynColor Ignore	gui=NONE guifg=bg guibg=NONE                              cterm=NONE ctermbg=NONE ctermfg=NONE
  SynColor Normal guibg=grey90               ctermbg=7
  SynColor Cursor guibg=Green guifg=NONE                             ctermbg=10 ctermfg=NONE
  SynColor lCursor guibg=Cyan guifg=NONE                             ctermbg=14 ctermfg=NONE
  SynColor NonText guibg=grey80                             ctermbg=244
  SynColor Constant gui=NONE guibg=grey95                             cterm=NONE ctermbg=245
  SynColor Special gui=NONE guibg=grey95                             cterm=NONE ctermbg=245
endif
SynColor Error		gui=NONE guifg=White guibg=Red                              cterm=NONE ctermbg=9 ctermfg=15
SynColor Todo		gui=NONE guifg=Blue guibg=Yellow                              cterm=NONE ctermbg=11 ctermfg=21

" Common groups that link to default highlighting.
" You can specify other highlighting easily.
SynLink String		Constant
SynLink Character	Constant
SynLink Number		Constant
SynLink Boolean		Constant
SynLink Float		Number
SynLink Function	Identifier
SynLink Conditional	Statement
SynLink Repeat		Statement
SynLink Label		Statement
SynLink Operator	Statement
SynLink Keyword		Statement
SynLink Exception	Statement
SynLink Include		PreProc
SynLink Define		PreProc
SynLink Macro		PreProc
SynLink PreCondit	PreProc
SynLink StorageClass	Type
SynLink Structure	Type
SynLink Typedef		Type
SynLink Tag		Special
SynLink SpecialChar	Special
SynLink Delimiter	Special
SynLink SpecialComment	Special
SynLink Debug		Special

highlight Visual cterm=reverse

delcommand SynColor
delcommand SynLink
