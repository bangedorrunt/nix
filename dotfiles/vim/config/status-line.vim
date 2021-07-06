" status line
" -----------

" - you have to escape spaces with \
" - everything between %{…} will be evaluated
" - you can set colours with %#your_colour_name#
" - everything you need to know is in :help statusline

set statusline=
set statusline+=%#DiffAdd#%{(mode()=='n')?'\ \ NORMAL\ ':''}
set statusline+=%#DiffText#%{(mode()=='i')?'\ \ INSERT\ ':''}
set statusline+=%#DiffDelete#%{(mode()=='r')?'\ \ RPLACE\ ':''}
set statusline+=%#DiffDelete#%{(mode()=='v')?'\ \ VISUAL\ ':''}
set statusline+=%1*                           " user1 highlight
set statusline+=\ [%n]                        " buffer number
" set statusline+=\ %{GetGitBranchName()}     " git branch name
set statusline+=\ [%f]                        " file path
set statusline+=%m                            " modified flag
set statusline+=%r                            " readonly flag
set statusline+=%h                            " help file flag
set statusline+=%w                            " preview window flag
set statusline+=%y                            " file type
set statusline+=[
set statusline+=%{&ff}                        " file format
set statusline+=:
set statusline+=%{strlen(&fenc)?&fenc:'none'} " file encoding
set statusline+=]
set statusline+=%=                            " left/right separator
set statusline+=%c                            " file encoding
set statusline+=,
set statusline+=%l                            " current line number
set statusline+=/
set statusline+=%L                            " total number of lines
set statusline+=\ (%P)\                       " percent through file

" example result:
"
"  [1] [master] [vim/vimrc][vim][unix:utf-8]            17,238/381 (59%)

" disable tabline.
" set showtabline=0
