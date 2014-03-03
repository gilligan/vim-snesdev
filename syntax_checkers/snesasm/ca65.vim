if exists("g:loaded_syntastic_snesasm_checker")
    finish
endif
let g:loaded_syntastic_snesasm_ca65_checker = 1

if exists('g:syntastic_extra_filetypes')
    call add(g:syntastic_extra_filetypes, 'snesasm')
else
    let g:syntastic_extra_filetypes = ['snesasm']
endif
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_snesasm_ca65_GetLocList() dict
    let makeprg = self.makeprgBuild({
                \ 'args': '--cpu 65816 --auto-import',
                \ 'args_after': '-I/Users/gilligan/Development/sfcdev/sfckit/include -I/Users/gilligan/Development/sfcdev/sfckit/lib' })
    let errorformat = '%f(%l): Error: %m'
    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
            \ 'filetype': 'snesasm',
            \ 'name': 'ca65',
            \ 'exec': 'ca65' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
