

" Get the meta keys working for when meta sends esc followed
" by the character

"set timeoutlen=180
"set ttimeoutlen=200

set shellpipe=2>&1\|tee


if &term =~ "vt100"
  if has("terminfo")
    set t_Co=8
    set t_Sf=[3%p1%dm
    set t_Sb=[4%p1%dm
  else
    set t_Co=8
    set t_Sf=[3%dm
    set t_Sb=[4%dm
  endif
endif

if &term =~ "xterm" || &term =~ "screen"
  if has("terminfo")
    set t_Co=256
    set t_Sf=[3%p1%dm
    set t_Sb=[4%p1%dm
  else
    set t_Co=256
    set t_Sf=[3%dm
    set t_Sb=[4%dm
  endif
endif

if &term =~ "linux"
  source ~/myconfig/vim-config/vimrc-meta-esc.vim
  set ttimeout ttimeoutlen=20
endif
    

