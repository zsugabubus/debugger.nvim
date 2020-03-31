" https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI.html

" https://sourceware.org/gdb/onlinedocs/gdb/Context-management.html#Context-management
" https://sourceware.org/gdb/onlinedocs/gdb/Thread-groups.html#Thread-groups
" MI cmd --thread xxx --frame xxx --process xxx
"
function s:parseany(input) abort
  try
    return s:parsestr(a:input)
  catch
  endtry

  try
    return s:parse_list(a:input)
  catch
  endtry

  try
    return s:parseobj(a:input)
  endtry

  throw 'cannot parse: ' . a:input
endfunction

function s:parsestr(input) abort
  let input = a:input
  let [_, ret, input; _] = matchlist(input, '\v^"(%([^"\\]*|\\.)*)"(.*)')
  " let ret = substitute(ret, '\V\\\"', '\"', 'g')
  " let ret = substitute(ret, '\v\\(U[0-9a-zA-Z]{4}|u[0-9a-zA-Z]{4}|x[0-9a-zA-Z]{2}|.)', {m-> m.'x'}, '')
  return [ret, input]
endfunction

function s:parse_list(input) abort
  let input = a:input
  let ret = []
  let [_, input; _] = matchlist(input, '\v^\[(.*)')
  while 1
    try
      let [_, input; _] = matchlist(input, '\v^\](.*)')
      break
    catch
      if !empty(ret)
        let [_, input; _] = matchlist(input, '\v^,(.*)')
      endif
    endtry
    let [elem, input] = s:parseany(input)
    call add(ret, elem)
  endwhile
  return [ret, input]
endfunction

function s:parseobj(input) abort
  let input = a:input
  let ret = {}
  try
    let [_, input; _] = matchlist(input, '\v^\{(.*)')
    let end = '\v^\}(.*)'
  catch
    let end = '\v^%(\\n)?$(.*)'
  endtry

  while 1
    try
      let [_, input; _] = matchlist(input, end)
      break
    catch
      if !empty(ret)
        let [_, input; _] = matchlist(input, '\v^,(.*)')
      endif
    endtry
    let [_, key, input; _] = matchlist(input, '\v^([a-z\-_]*)\=(.*)')
    let [value, input] = s:parseany(input)
    let ret[key] = value
  endwhile

  return [ret, input]
endfunction

function s:parsedata(input, action) abort
  let input = a:input
  if a:action
    try
      let [_, name; _] = matchlist(input, '\v^([a-z\-]*)$')
      return [name, {}]
    catch
      let [_, name, input; _] = matchlist(input, '\v^([a-z\-]*),(.*)')
    endtry
  endif
  let r = s:parseany(input)
  let [data, input] = r
  if !empty(input)
    throw 'parse error: ' . input
  endif
  return a:action ? [name, data] : data
endfunction

function s:on_stdout(job_id, data, event) dict abort
  let self.lines[-1] .= a:data[0]
  call extend(self.lines, a:data[1:])
  for line in self.lines[:-2]
    let [line, token, prefix, data; _] = matchlist(line, '\v(\d*)(.)([^\r]*)')
    if prefix ==# '^'
      let Cb = s:Commands[token]
      unlet s:Commands[token]
      let result = s:parsedata(data, 1)

      if result[0] ==# 'done' || result[0] ==# 'running'
        " FUCK: gdb
        call Cb(result[1])
      elseif result[0] ==# 'exit'
        " Ignore
      elseif result[0] ==# 'error'
        echohl Error
        echo printf('debugger.nvim: gdb: %s', result[1].msg)
        echohl None
      else
        echohl Error
        echo printf('debugger.nvim: Unknown response: %s.', string(result))
        echohl None
      endif
    elseif prefix ==# '+'
      echom  '+: ' . line[1:]
    elseif prefix ==# '*'
      " echom '<-' . line
      let [action, data] = s:parsedata(data, 1)
      " echom  '*: ' . string([action, data])
      if action ==# 'stopped'
        " call s:send_command('-data-list-register-values r', function('s:registers_info_cb'))
        call s:send_command('-data-list-changed-registers', function('s:list_chaged_registers_cb'))
        call s:send_command('-thread-info', function('s:thread_info_cb'))

        if exists('data.frame.line')
          let frame = data.frame
          let buffer = bufnr(frame.fullname, 1)

          if !has_key(b:, 'debug')
            for map in g:debugger_maps
              execute map
            endfor
            let b:debug = 1
          endif

          execute 'buffer!' buffer
          call sign_unplace('debugger', { 'buffer': buffer, 'id': 111 })
          call sign_place(111, 'debugger', 'DebuggerCurrent', buffer, {
          \  'lnum': str2nr(frame.line),
          \  'priority': 99
          \})
          if exists('frame.args')
            call s:send_command(printf('-symbol-info-functions --name %s', frame.func), function('s:show_frame_args_cb', [frame]))
          endif
        endif

        if exists('data.reason')
          if data.reason ==# 'breakpoint-hit'
            echohl DiffDelete
            echon 'STOPPED'
            echohl None
            echon printf(' Breakpoint %d was hit by thread %d; thread %s stopped.', data.bkptno, data['thread-id'], data['stopped-threads'])
          elseif data.reason ==# 'function-finished'
            echo printf('Function finished on thread %d.', data.bkptno, data['thread-id'], data['stopped-threads'])
          elseif data.reason ==# 'exited-normally'
            echo 'Program exited.'
          endif
        else
          echo printf('Program stopped at .')
        endif
      elseif action ==# 'running'
        call sign_unplace('debugger', { 'id': 111 })
        echohl DiffAdd
        echom 'RUNNING'
        echohl None
      endif
    elseif prefix ==# '='
      echom line string(s:parsedata(data, 1))
      let [option, data] = s:parsedata(data, 1)
      if option ==# 'breakpoint-created'
        call call('s:breakpoint_add', [], data.bkpt)
      elseif option ==# 'breakpoint-modified'
        call call('s:breakpoint_mod', [], data.bkpt)
      elseif option ==# 'breakpoint-deleted'
        call call('s:breakpoint_del', [], data.bkpt)
      else
        echom  '=: ' . option ' = ' string(data)
      endif
    elseif prefix ==# '~'
      " echom  '~: ' . string(s:parsedata(data, 0))
    elseif prefix ==# '@'
    elseif prefix ==# '&'
      echom  'debugger.nvim: gdb: ' . s:parsedata(data, 0)
    elseif prefix ==# "(" && data ==# 'gdb) '
    else
      " throw line
    endif
  endfor
  let self.lines = self.lines[-1:]
endfunction

  " call s:send_command(printf('save breakpoints %s', fnameescape('/tmp/bkpts')))
function s:breakpoint_add() dict
  let s:breakpoints[self.number] = self
  call call('s:breakpoint_update', [], self)
endfunction

function s:breakpoint_mod() dict
  let s:breakpoints[self.number] = self
  call call('s:breakpoint_update', [], self)
endfunction

function s:breakpoint_del() dict
  call call('s:breakpoint_unplace', [], s:breakpoints[data.id])
  call filter(s:breakpoints, {_, bp-> bp.number != data.id})
  unlet s:breakpoints[data.id]
endfunction

function s:on_exit(job_id, data, evenet)
  echom printf('job %d finished', a:job_id)
endfunction

let s:threadsbuf = -1
let s:regsbuf = -1
let s:regnames = []
let s:regnamescn = -1
let s:regpos = {}
let s:regchmatches = []

function s:list_chaged_registers_cb(result)
  call s:send_command(printf('-data-list-register-values --skip-unavailable x %s', join(a:result['changed-registers'], ' ')), function('s:registers_info_cb'))
endfunction

highlight default link DebuggerRegisterChanged DiffChange

function s:registers_info_cb(result)
  if s:regsbuf ==# -1
    return
  endif

  let cbuf = bufnr()
  let addnew = nvim_buf_get_lines(s:regsbuf, -2, -1, 0)[0] !=# '.'

  try
    silent! execute bufwinnr(s:regsbuf) .'windo for match in s:regchmatches | call matchdelete(match) | endfor'
    let view = winsaveview()

    let s:regchmatches = []

    let cn = changenr()

    for reg in a:result['register-values']
      let reg.name = s:regnames[reg.number]
      let [lnum, col, len] = get(s:regpos, reg.number, [0, 0, 0])
      if cn ==# s:regnamescn && lnum ># 0
        let valcol = col
        let text = reg.value
        let newlen = len(text)
        let text = text.repeat(' ', len - newlen)
        let s:regpos[reg.number][2] = newlen
        let new = 0
        let len = newlen
      else
        let [lnum, col] = searchpos('\V\<'.reg.name.'=\zs')
        if lnum ==# 0
          if !addnew
            continue
          endif
          let lnum = line('$')
          let col = 1
          let new = 1
          let valcol = col + len(reg.name) + 1
          let text = reg.name.'='.reg.value
          let len = len(reg.value)
        else
          let valcol = col
          let text = reg.value
          let newlen = len(text)
          let text = text.repeat(' ', len - newlen)
          let s:regpos[reg.number][2] = newlen
          let new = 0
          let len = newlen
        endif
        let lnum -= 1
        let s:regpos[reg.number] = [lnum, valcol, len]
      endif
      try
        let line = nvim_buf_get_lines(s:regsbuf, lnum, lnum + 1, 1)[0]
      catch
        let line = ''
      endtry
      call nvim_buf_set_lines(s:regsbuf, lnum, lnum + 1, 0, [matchstr(strpart(line, 0, col - 1).text.strpart(line, col - 1 + len(text)), '\v^.{-}\ze\s*$')] + (new ? [''] : []))

      call add(s:regchmatches, matchaddpos('DebuggerRegisterChanged', [[lnum + 1, valcol, len]], 60, -1, { 'window': bufwinnr(s:regsbuf) }))
    endfor
    let s:regnamescn = cn

    call winrestview(view)
  finally
    redraw
    silent! execute bufwinnr(cbuf).'windo normal'
  endtry
endfunction

function s:thread_info_cb(result)
  if s:threadsbuf ==# -1
    return
  endif

  " TODO: https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI-Stack-Manipulation.html#GDB_002fMI-Stack-Manipulation
  let line = 0
  for thread in a:result.threads
    call nvim_buf_set_lines(s:threadsbuf, line, line + 2, 0, [
    \  printf('%1s%d %s %s:', thread.id == a:result['current-thread-id'] ? '*' : '', thread.id, thread['target-id'], thread.state),
    \  printf('  #%-2d %s in %s (%s) at %s', 0, thread.frame.addr, thread.frame.func, join(map(thread.frame.args, {_, arg-> printf('%s=%s', arg.name, arg.value)}), ', '), (has_key(thread.frame, 'line') ? printf('%s:%d', thread.frame.file, str2nr(thread.frame.line)) : thread.frame.from))
    \])
    let line += 2
  endfor
  call nvim_buf_set_lines(s:threadsbuf, line, 9999, 0, [])
  redraw
endfunction

function s:show_frame_args_cb(frame, result)
  let buffer = bufnr(a:frame.fullname)
  if buffer ==# -1
    return
  endif

  for file in a:result.symbols.debug
    if file.fullname !=# a:frame.fullname
      continue
    endif

    let symbol = file.symbols[0]

    call nvim_buf_set_virtual_text(buffer, 5, str2nr(symbol.line) - 1, [
    \  ['  '.join(map(a:frame.args, {_, arg-> printf('%s=%s', arg.name, arg.value)}), ', '), 'Normal']
    \], {})

    return
  endfor

  throw 'no such function: ' . string(a:frame)
endfunction

let s:currtoken = 0
let s:Commands = {}
let s:job = jobstart(['gdb', '--interpreter=mi3', '--quiet', '-fullname', '-nh'], {
\  'on_stdout': function('s:on_stdout'),
\  'on_stderr': function('s:on_stdout'),
\  'on_exit': function('s:on_exit'),
\  'lines': [''],
\  'pty': v:true,
\})

function s:send_command(cmd, ...) abort
  let s:currtoken += 1
  let s:Commands[s:currtoken] = get(a:000, 0, function('s:noop'))
  let msg = printf("%d%s\n", s:currtoken, a:cmd)
  " if &verbose >= 12
    " echom '->' . msg
  " endif
  call chansend(s:job, msg)
endfunction

function s:breakpoint_update() dict abort
  let buffer = bufnr(self.fullname)
  if buffer ==# -1
    return
  endif

  call call('s:breakpoint_unplace', [buffer], self)
  " echom 'add bp: ' . string(self)
  let self.debugger_sign = sign_place(0, 'debugger', 'DebuggerBreakpoint' . (self.enabled ==# 'y' ? '' : 'Disabled'), buffer, { 'lnum': str2nr(self.line), 'priority': 89 })
  let self.debugger_signns = nvim_buf_set_virtual_text(buffer, 4, str2nr(self.line) - 1, [[printf('  hits=%d', self.times), 'Normal']], {})
endfunction

function s:breakpoint_unplace(buffer) dict abort
  if has_key(self, 'debugger_sign')
    call sign_unplace('debugger', { 'id': self.debugger_sign })
    call nvim_buf_clear_namespace(a:buffer, 0, str2nr(self.line) - 1, str2nr(self.line))
    unlet self.debugger_sign
  endif
endfunction

function s:break_list_cb(result) abort
  for entry in a:result['BreakpointTable'].body
    let bkpt = entry.bkpt
    call call('s:breakpoint_update', [], bkpt)
  endfor
endfunction

function s:setup_breakpoints() abort

  let path = s:getcfile()
  for [_, bkpt] in items(s:breakpoints)
    if bkpt.fullname ==# path
      call call('s:breakpoint_update', [], bkpt)
    endif
  endfor
endfunction

highlight DebuggerBreakpoint         cterm=NONE ctermfg=160
highlight DebuggerBreakpointDisabled cterm=NONE ctermfg=160
call sign_define('DebuggerBreakpoint', {
\  'text': ' ●',
\  'texthl': 'DebuggerBreakpoint',
\})
call sign_define('DebuggerBreakpointDisabled', {
\  'text': ' ○',
\  'texthl': 'DebuggerBreakpointDisabled',
\})

highlight DebuggerCurrent cterm=bold ctermfg=236 ctermbg=226
highlight DebuggerCurrentLine ctermbg=226
call sign_define('DebuggerCurrent', {
\  'text': '#0',
\  'texthl': 'DebuggerCurrent',
\  'linehl': 'DebuggerCurrentLine'
\})

highlight DebuggerFrame cterm=bold ctermfg=236 ctermbg=226
highlight DebuggerFrameLine ctermbg=226
call sign_define('DebuggerFrame', {
\  'text': '#x',
\  'texthl': 'DebuggerFrame',
\  'linehl': 'DebuggerFrameLine'
\})

function s:noop(result) abort
endfunction

function s:break_insert_cb(result) abort
  call call('s:breakpoint_add', [], a:result.bkpt)
endfunction

function s:debugger_connected(result) abort

  " call s:send_command(printf('source %s', fnameescape('/tmp/bkpts')), function('s:startup'))

  call s:send_command('-data-list-register-names', function('s:list_reg_names_cb'))

  let s:threadsbuf = bufnr('debugger://threads', 1)
  execute 'split | buffer' s:threadsbuf ' | resize 4'
  call setbufvar(s:threadsbuf, '&buftype', 'nofile')
  call setbufvar(s:threadsbuf, '&bufhidden', 'unload')
  call setbufvar(s:threadsbuf, '&swapfile', 0)
  call setbufvar(s:threadsbuf, '&number', 0)
  call setbufvar(s:threadsbuf, '&relativenumber', 0)
  call setbufvar(s:threadsbuf, '&filetype', 'backtrace')

  let s:breakpoints = {}

  augroup DebuggerBreakpoints
    autocmd!
    autocmd BufEnter * :call <SID>setup_breakpoints()
  augroup END

  echohl DiffAdd
  echo 'CONNECTED'
  echohl None

endfunction

function s:list_reg_names_cb(result)
  let s:regnames = a:result['register-names']

  let s:regsbuf = bufnr('debugger://registers', 1)
  execute 'vsplit | buffer' s:regsbuf ' | resize 6'
  call setbufvar(s:regsbuf, '&buftype', 'nofile')
  call setbufvar(s:regsbuf, '&bufhidden', 'unload')
  call setbufvar(s:regsbuf, '&swapfile', 0)
  call setbufvar(s:regsbuf, '&number', 0)
  call setbufvar(s:regsbuf, '&relativenumber', 0)
  call setbufvar(s:regsbuf, '&filetype', 'debugger-registers')
endfunction

function s:read_remote(buffer) abort
  echom 'read'
  call setbufvar(a:buffer, '&buftype', 'nofile')
  call setbufvar(a:buffer, '&swapfile', 0)
  call setbufvar(a:buffer, '&readonly', 1)
  call setbufvar(a:buffer, '&modifiable', 0)

  let localfile = tempname()
  echom bufname(a:buffer)
  call s:send_command(printf('-target-file-get %s %s', matchstr(bufname(a:buffer), '\m://\zs.*'), localfile), function('s:target_file_get_cb', [a:buffer, localfile]))
endfunction

function s:target_file_get_cb(buffer, localfile, result) abort
  echoe string(a:result)
  " execute printf('normal %dbufdo :0read %s', a:buffer, fnameescape(a:localfile))
  call setbufvar(a:buffer, '&buftype', 'acwrite')
  call setbufvar(a:buffer, '&modifiable', 1)
  call setbufvar(a:buffer, '&readonly', 0)
  call nvim_buf_set_lines(a:buffer, 0, -1, 1, readfile(a:localfile))
  call setbufvar(a:buffer, '&modified', 0)
  call delete(a:localfile)
endfunction

function s:write_remote(buffer) abort
  call setbufvar(a:buffer, '&modifiable', 0)

  let localfile = tempname()
  silent execute printf('write %s', fnameescape(localfile))
  call s:send_command(printf('-target-file-put %s %s', localfile, matchstr(bufname(''), '\m://\zs.*') . '.gdb'), function('s:target_file_put_cb', [a:buffer, localfile]))
endfunction

function s:target_file_put_cb(buffer, localfile, result) abort
  call setbufvar(a:buffer, '&modifiable', 1)
  call setbufvar(a:buffer, '&modified', 0)
  call delete(a:localfile)
endfunction

augroup DebuggerRemote
  autocmd!
  autocmd BufEnter debugger-target://* :call <SID>read_remote(bufnr())
  autocmd BufWriteCmd debugger-target://* :call <SID>write_remote(bufnr())
augroup END

function s:startup(result)
  " call s:send_command(printf('source %s', fnameescape('/tmp/bkpts')), function('s:startup'))
  call s:send_command('-break-list', function('s:break_list_cb'))
endfunction

function s:show_pwd_cb(result) abort
  redraw
  echo a:result.cwd
endfunction

function s:getcfile() abort
  return matchstr(expand('%:p'), '\m^\%(debugger-target://\)\?\zs.*')
endfunction

command -range -nargs=? -count=1 DebuggerBreakpointAdd call <SID>send_command(empty(<q-args>) ? printf('-break-insert %s:%d', s:getcfile(), line('.')) : printf('-break-insert %s', <q-args>), function('<SID>break_insert_cb'))
command -range -nargs=? -count=1 DebuggerBreakpointDisable call <SID>send_command(empty(<q-args>) ? printf('-break-insert %s:%d', s:getcfile(), line('.')) : printf('-break-insert %s', <q-args>), function('<SID>break_insert_cb'))
command -range -nargs=+ -count=1 DebuggerBreakpointAddIf call <SID>send_command(printf('-break-insert %s:%d -c %s', s:getcfile(), line('.'), <q-args>), function('<SID>break_insert_cb'))
command -count=1 DebuggerNext call <SID>send_command('-exec-next <count>')
command -count=1 DebuggerNextInstruction call <SID>send_command('-exec-next-instruction <count>')
command -count=1 DebuggerPrev call <SID>send_command('-exec-next --reverse <count>')
command -count=1 DebuggerPrevInstruction call <SID>send_command('-exec-next-instruction --reverse <count>')
command -nargs=? DebuggerRun call <SID>send_command('-exec-run <args>')
command -nargs=* DebuggerArgs call <SID>send_command('-exec-arguments <args>')
command -range DebuggerJump call <SID>send_command('-exec-jump <count>')
command -count=1 DebuggerReturn call <SID>send_command('-exec-return')
command -count=1 DebuggerStep call <SID>send_command('-exec-step <count>')
command -count=1 DebuggerExecUntil call <SID>send_command(printf('-exec-until %s:%d', s:getcfile(), line('.')))
command -count=1 DebuggerStepInstruction call <SID>send_command('-exec-step-instruction <count>')
command -count=0 DebuggerContinue call <SID>send_command('-exec-continue')
command DebuggerStepOut call <SID>send_command('-exec-finish')
command DebuggerInterrupt call <SID>send_command('-exec-interrupt+)
command DebuggerInterruptAll call <SID>send_command('-exec-interrupt --all')
command DebuggerExit call <SID>send_command('-exit', function('<SID>expect_exit_cb'))
command DebuggerPwd call <SID>send_command('-environment-pwd', function('<SID>show_pwd_cb'))
command -nargs=+ DebuggerCd call <SID>send_command('-environment-cd <args>')

let g:debugger_maps = []

command -nargs=+ DebugMap :call add(g:debugger_maps, <q-args>)

nnoremap <silent><buffer> <Leader><Leader>b :<C-U>DebuggerBreakpointAdd<CR>
nnoremap <silent><buffer> <Leader><Leader>c :<C-U>DebuggerContinue<CR>

" DebugMap nnoremap <silent> W  :<C-U>call send_command(printf(\"watch %s\", input('watch expr ')))<CR>
" DebugMap nnoremap <silent> B  :<C-U>call send_command(printf(\"break %s\", input('break ')))<CR>
" DebugMap nnoremap <silent> db :<C-U>call send_command(printf(\"delete break %d\", line('.')))<CR>
DebugMap nnoremap <silent><buffer> bb :<C-U>DebuggerBreakpointAdd<CR>
DebugMap nnoremap <silent><buffer> * :<C-U>DebuggerBreakpointAdd<CR>
DebugMap nnoremap <silent><buffer> bi :<C-U>DebuggerBreakpointAdd<CR>
DebugMap nnoremap <silent><buffer> bp :<C-U>DebuggerBreakpoint<CR>
DebugMap nnoremap <silent><buffer> bd :DebuggerBreakpointDisable<CR>
DebugMap nnoremap <silent><buffer> bD :DebuggerBreakpointDelete<CR>
" DebugMap nnoremap <silent> bi :<C-U>call send_command(printf(\"break %d if %s\", line('.'), input('break if ')))<CR>
DebugMap nnoremap <silent><buffer> ]b " prev by line
DebugMap nnoremap <silent><buffer> [b " prev by line
DebugMap nnoremap <silent><buffer> ]B " prev by id
DebugMap nnoremap <silent><buffer> [B " prev by id
DebugMap nnoremap <silent><buffer> p :<C-U>DebuggerInterrupt<CR>
DebugMap nnoremap <silent><buffer> P :<C-U>DebuggerInterruptAll<CR>
DebugMap nnoremap <silent><buffer> c :<C-U>DebuggerContinue<CR>
DebugMap nnoremap <silent><buffer> n :<C-U>DebuggerNext<CR>
DebugMap nnoremap <silent><buffer> N :<C-U>DebuggerPrev<CR>
DebugMap nnoremap <silent><buffer> t :<C-U>DebuggerNextInstruction<CR>
DebugMap nnoremap <silent><buffer> T :<C-U>DebuggerPrevInstruction<CR>
DebugMap nnoremap <silent><buffer> s :<C-U>DebuggerStep<CR>
DebugMap nnoremap <silent><buffer> f :<C-U>DebuggerStepOut<CR>
" DebugMap nnoremap <silent> S  :<C-U>call send_command(\"stepi \" . v:count1)<CR>
DebugMap nnoremap <expr><silent><buffer> r  ':<C-U>DebuggerReturn'.input('return ')."\<lt>CR>"
" DebugMap nnorema<silent> p j :call send_command(\"jump 0x0\n\")<CR>
" DebugMap nnoremap <silent> F  :<C-U>call send_command(\"frame \" . v:count)<CR>
" DebugMap nnoremap <silent> +  :<C-U>call send_command(\"up \" . v:count)<CR>
" DebugMap nnoremap <silent> -  :<C-U>call send_command(\"down \" . v:count)<CR>
DebugMap nnoremap <silent><buffer> u  :<C-U>DebuggerExecUntil<CR>
" DebugMap nnoremap <silent> U  :<C-U>call send_command(\"advance \" . v:count)<CR>
DebugMap nnoremap <buffer> <C-c> :call jobstop(id)<CR>

call s:send_command('define hook-stop\nframe\nbacktrace\nend')
call s:send_command('-gdb-set mi-async on')
call s:send_command('target remote localhost:2000', function('s:debugger_connected'))

" vi: ts=2 sw=2 sts=2 et
