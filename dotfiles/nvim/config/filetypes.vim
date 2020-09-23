
"-------------------------------------------------
" FILE TYPES
"-------------------------------------------------

augroup TYPESRC

	au!

	" Reload vim config automatically
	au BufWritePost *.vim nested
		\ source $MYVIMRC | redraw

	" Reload vim script automatically if setlocal autoread
	au BufWritePost,FileWritePost *.vim nested
		\ if &l:autoread > 0 | source <afile> |
		\   echo 'source ' . bufname('%') |
		\ endif

	" Automatically set read-only for files being edited elsewhere
	au SwapExists * nested let v:swapchoice = 'o'

	" Update diff comparison once leaving insert mode
	au InsertLeave * if &l:diff | diffupdate | endif

	" Equalize window dimensions when resizing vim window
	au VimResized * tabdo wincmd =

	" Force write shada on leaving Neovim
	au VimLeave * if has('nvim') | wshada! | else | wviminfo! | endif

	" Check if file changed when its window is focus, more eager than 'autoread'
	au FocusGained * checktime

	au Syntax * if line('$') > 5000 | syntax sync minlines=200 | endif

	" Update filetype on save if empty
	au BufWritePost * nested
		\ if &l:filetype ==# '' || exists('b:ftdetect')
		\ |   unlet! b:ftdetect
		\ |   filetype detect
		\ | endif


	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	au BufReadPost *
		\ if &ft !~# 'commit' && ! &diff &&
		\      line("'\"") >= 1 && line("'\"") <= line("$")
		\|   execute 'normal! g`"zvzz'
		\| endif

	au FileType crontab setlocal nobackup nowritebackup

	au FileType yaml.docker-compose setlocal expandtab

	au FileType gitcommit setlocal spell

	au FileType gitcommit,qfreplace setlocal nofoldenable

	" https://webpack.github.io/docs/webpack-dev-server.html#working-with-editors-ides-supporting-safe-write
	au FileType css,javascript,javascriptreact setlocal backupcopy=yes

	au FileType php
		\ setlocal matchpairs-=<:> iskeyword+=\\ path+=/usr/local/share/pear

	au FileType python
		\ setlocal expandtab smarttab nosmartindent
		\ | setlocal tabstop=4 softtabstop=4 shiftwidth=4 textwidth=80

	au FileType html setlocal path+=./;/

	au FileType markdown
		\ setlocal expandtab conceallevel=0
		\ | setlocal autoindent formatoptions=tcroqn2 comments=n:>

	au FileType apache setlocal path+=./;/


augroup END

