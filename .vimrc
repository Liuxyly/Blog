set mouse=a
let Tlist_Show_One_File=0
let Tlist_Exit_OnlyWindow=1
let Tlist_Use_Right_Window=1
let Tilists_File_Fold_Auto_Close=1
map <F4> :TlistToggle<CR>
noremap <F6> :make<CR>
noremap <S-F6> :make clean; make<CR>
noremap <F7> :Tlist<CR>
noremap <S-F7> :TlistSync<CR>
noremap <F3> :!./vitags.sh<CR>:cs reset<CR>
noremap <S-F3> :!cvs up<CR>
noremap <space> @=((foldclosed(line(''.)) < 0) 'zc' : 'zo')<CR>
if has("multi_byte")
	set encoding=utf-8
	set fileencoding=chinese
	"	set fileencoding=ucs-bom,utf-8,chinese
endif
set wrap
set hlsearch
filetype plugin on
colorscheme elflord
syntax on
set nocp
filetype plugin on
filetype indent on
if has("cscope")
	set cscopeverbose
	set csprg=/usr/bin/cscope
	set csto=1
	set cst
	set nocsverb
	if filereadable("cscope.out")
		cs add cscope.out
	elseif $CSCOPE_DB
		cs add $CSCOPE
	endif
	set csverb
	set cscopetag
	nmap <C-/>s :cs find s <C-R>=expand("<cword>")<CR><CR>
	nmap <C-/>g :cs find g <C-R>=expand("<cword>")<CR><CR>
	nmap <C-/>c :cs find c <C-R>=expand("<cword>")<CR><CR>
	nmap <C-/>t :cs find t <C-R>=expand("<cword>")<CR><CR>
	nmap <C-/>e :cs find e <C-R>=expand("<cword>")<CR><CR>
	nmap <C-/>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
	nmap <C-/>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
	nmap <C-/>d :cs find d <C-R>=expand("<cword>")<CR><CR>
endif
set nu
set ts=4
set sw=4
set ru
set hls
set is
set sm
set cin
set cino=:0g0t0(sus
set autoread
set incsearch
set nowrap
set nobackup
map <C-F12> :!ctag -R --c-kinds=+p --fields=+iaS --extra=+q .<CR>
map <C-F11> :!ctag -R .<CR>

if has("ctag")
	if fileradeable("tags")
		set tags=tags;
		set autochdir
	endif
endif
