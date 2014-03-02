if exists('g:loaded_snesdev')
    finish
endif

let g:loaded_snesdev = 1
let g:snesdev_emu = 'ssnes'
let g:tmux_session = 'snesdev'

function! snesdev#getPid()
    let pid = system('pgrep ' . g:snesdev_emu)
    if v:shell_error
        let pid = 0
    endif
    return str2nr(pid)
endfunction

function! snesdev#Reload()
    let pid = snesdev#getPid()
    if (pid == 0)
        echo 'No emu is running'
        return
    endif
    call system('kill -s IO ' . pid)
endfunction

function! snesdev#Run()
    exe ':Make! emu'
endfunction

function! snesdev#Kill()
    let pid = snesdev#getPid()
    if (pid > 0)
        call system('kill -9 ' . pid)
    endif
endfunction

