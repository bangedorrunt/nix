"" ---------------------
"" ESSENTIAL KEYBINDINGS FOR VSCODE
"" ---------------------

" Map leader to space
nmap <space> <nop>
let mapleader="\<space>"
let maplocalleader="\<space>"

" Use <tab> and <s-tab> to jump to buffers
" nmap <tab> :<c-u>Tabnext<cr>
" nmap <s-tab> :<c-u>Tabprev<cr>
nmap <tab> gt
nmap <s-tab> gT

" Break line in normal mode
nmap o o<esc>
nmap O O<esc>

" Make j and k move to next line, not a file line
nnoremap j gj
nnoremap k gk

" Prevent x from overriding what's in the clipboard
nnoremap x "_x
nnoremap X "_x

" Copy/Paste/Cut
nmap YY "+y<cr>
nmap P "+gP<cr>
nmap XX "+x<cr>

" Prevent selecting and pasting from overwritting what you originally copied
xnoremap p pgvy

" Keep cursor at the bottom of the visual selection after you yank it
vmap y ygv<esc>

" Vmap for maintain visual mode after shifting > and <
vmap < <gv
vmap > >gv

nmap >> >>_
nmap << <<_

" Better comments
xmap gc  <plug>VSCodeCommentary
nmap gc  <plug>VSCodeCommentary
omap gc  <plug>VSCodeCommentary
nmap gcc <plug>VSCodeCommentaryLine

" VSCode keybindings
nnoremap <leader><space> :<c-u>call VSCodeNotify('workbench.action.showCommands')<cr>
vnoremap <leader><space> :<c-u>call VSCodeNotify('workbench.action.showCommands')<cr>
nnoremap <silent> <leader>; :<c-u>call VSCodeNotify('workbench.action.tasks.runTask', 'ripgrep')<cr>
vnoremap <silent> <leader>; :<c-u>call VSCodeNotify('workbench.action.tasks.runTask', 'ripgrep-cword')<cr>
nnoremap <leader>1 :<c-u>call VSCodeNotify('workbench.action.focusFirstEditorGroup')<cr>
nnoremap <leader>2 :<c-u>call VSCodeNotify('workbench.action.focusSecondEditorGroup')<cr>
nnoremap <leader>3 :<c-u>call VSCodeNotify('workbench.action.focusThirdEditorGroup')<cr>
nnoremap <leader>4 :<c-u>call VSCodeNotify('workbench.action.focusFourthEditorGroup')<cr>


" File manager
nnoremap <leader>bb :<c-u>call VSCodeNotify('workbench.action.showAllEditors')<cr>
nnoremap <leader>bd :<c-u>call VSCodeNotify('workbench.action.closeActiveEditor')<cr>
nnoremap <leader>bn :<c-u>call VSCodeNotify('workbench.action.nextEditor')<cr>
nnoremap <leader>bp :<c-u>call VSCodeNotify('workbench.action.previousEditor')<cr>
nnoremap <leader>be :<c-u>call VSCodeNotify('workbench.action.files.newUntitledFile')<cr>
nnoremap <leader>bs :<c-u>call VSCodeNotify('workbench.action.files.save')<cr>

nnoremap <leader>ff :<c-u>call VSCodeNotify('workbench.action.files.openFileFolder')<cr>
nnoremap <leader>fd :<c-u>call VSCodeNotify('fileutils.removeFile')<cr>
nnoremap <leader>fn :<c-u>call VSCodeNotify('fileutils.newFile')<cr>
nnoremap <leader>fm :<c-u>call VSCodeNotify('fileutils.moveFile')<cr>
nnoremap <leader>fr :<c-u>call VSCodeNotify('fileutils.renameFile')<cr>
nnoremap <leader>fy :<c-u>call VSCodeNotify('workbench.action.files.copyPathOfActiveFile')<cr>
nnoremap <leader>fe :<c-u>call VSCodeNotify('fileutils.newFile')<cr>
nnoremap <leader>fs :<c-u>call VSCodeNotify('workbench.action.files.save')<cr>

" Project manager
nnoremap <leader>pf :<c-u>call VSCodeNotify('workbench.action.quickOpen')<cr>
nnoremap <leader>pe :<c-u>call VSCodeNotify('projectManager.editProjects')<cr>
nnoremap <leader>pp :<c-u>call VSCodeNotify('projectManager.listProjects')<cr>
nnoremap <leader>ps :<c-u>call VSCodeNotify('projectManager.saveProject')<cr>

" Toggle settings
nnoremap <leader>tt :<c-u>call VSCodeNotify('workbench.action.toggleSidebarVisibility')<cr>
nnoremap <leader>ts :<c-u>call VSCodeNotify('workbench.action.selectTheme')<cr>
nnoremap <leader>te :<c-u>call VSCodeNotify('workbench.view.explorer')<cr>
nnoremap <leader>tx :<c-u>call VSCodeNotify('workbench.view.extensions')<cr>
nnoremap <leader>tK :<c-u>call VSCodeNotify('workbench.action.openDefaultKeybindingsFile')<cr>
nnoremap <leader>tk :<c-u>call VSCodeNotify('workbench.action.openGlobalKeybindingsFile')<cr>

" Windows navigation
nnoremap <leader>wc :<c-u>call VSCodeNotify('workbench.action.closeEditorsInGroup')<cr>
xnoremap <leader>wc :<c-u>call VSCodeNotify('workbench.action.closeEditorsInGroup')<cr>
nnoremap <leader>wd :<c-u>call VSCodeNotify('workbench.action.closeEditorsInGroup')<cr>
xnoremap <leader>wd :<c-u>call VSCodeNotify('workbench.action.closeEditorsInGroup')<cr>
nnoremap <leader>wh :<c-u>call VSCodeNotify('workbench.action.focusPreviousGroup')<cr>
xnoremap <leader>wh :<c-u>call VSCodeNotify('workbench.action.focusPreviousGroup')<cr>
nnoremap <leader>wH :<c-u>call VSCodeNotify('workbench.action.moveActiveEditorGroupLeft')<cr>
xnoremap <leader>wH :<c-u>call VSCodeNotify('workbench.action.moveActiveEditorGroupLeft')<cr>
nnoremap <leader>wl :<c-u>call VSCodeNotify('workbench.action.focusNextGroup')<cr>
xnoremap <leader>wl :<c-u>call VSCodeNotify('workbench.action.focusNextGroup')<cr>
nnoremap <leader>wL :<c-u>call VSCodeNotify('workbench.action.moveActiveEditorGroupRight')<cr>
xnoremap <leader>wL :<c-u>call VSCodeNotify('workbench.action.moveActiveEditorGroupRight')<cr>
nnoremap <leader>wm :<c-u>call VSCodeNotify('workbench.action.maximizeEditor')<cr>
xnoremap <leader>wm :<c-u>call VSCodeNotify('workbench.action.maximizeEditor')<cr>
nnoremap <leader>wv :<c-u>call VSCodeNotify('workbench.action.splitEditor')<cr>
xnoremap <leader>wv :<c-u>call VSCodeNotify('workbench.action.splitEditor')<cr>
nnoremap <leader>ws :<c-u>call VSCodeNotify('workbench.action.splitEditorOrthogonal')<cr>
xnoremap <leader>ws :<c-u>call VSCodeNotify('workbench.action.splitEditorOrthogonal')<cr>
nnoremap <leader>ww :<c-u>call VSCodeNotify('workbench.action.focusNextGroup')<cr>
xnoremap <leader>ww :<c-u>call VSCodeNotify('workbench.action.focusNextGroup')<cr>
nnoremap <leader>wW :<c-u>call VSCodeNotify('workbench.action.focusPreviousGroup')<cr>
xnoremap <leader>wW :<c-u>call VSCodeNotify('workbench.action.focusPreviousGroup')<cr>
nnoremap <leader>w= :<c-u>call VSCodeNotify('workbench.action.evenEditorWidths')<cr>
xnoremap <leader>w= :<c-u>call VSCodeNotify('workbench.action.evenEditorWidths')<cr>

" Use arrow keys to resize windows
" Both width and height are resized due to VSCode API limitation
function! s:manageEditorSize(...)
  let count = a:1
  let to = a:2
  for i in range(1, count ? count : 1)
    call VSCodeNotify(to ==# 'increase' ? 'workbench.action.increaseViewSize' : 'workbench.action.decreaseViewSize')
  endfor
endfunction

nnoremap <silent> <right> :<c-u>call <sid>manageEditorSize(v:count, 'increase')<cr>
xnoremap <silent> <right> :<c-u>call <sid>manageEditorSize(v:count, 'increase')<cr>
nnoremap <silent> <left> :<c-u>call <sid>manageEditorSize(v:count, 'decrease')<cr>
xnoremap <silent> <left> :<c-u>call <sid>manageEditorSize(v:count, 'decrease')<cr>
