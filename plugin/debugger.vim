" https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI.html
" https://sourceware.org/gdb/current/onlinedocs/gdb/GDB_002fMI-Stack-Manipulation.html
" https://www.zeuthen.desy.de/dv/documentation/unixguide/infohtml/gdb/GDB_002fMI-Thread-Commands.html
" https://sourceware.org/gdb/onlinedocs/gdb/GDB_002fMI-Stack-Manipulation.html#GDB_002fMI-Stack-Manipulation

" https://sourceware.org/gdb/onlinedocs/gdb/Context-management.html#Context-management
" https://sourceware.org/gdb/onlinedocs/gdb/Thread-groups.html#Thread-groups
" MI cmd --thread xxx --frame xxx --process xxx

if get(g:, 'loaded_debugger', 0)
  finish
endif
let g:loaded_debugger = 1

let s:program = {'threads': {}}
let s:debugger_ns = nvim_create_namespace('debugger.nvim')

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
    " Braindead list: [frame={},frame={},frame={}].
    " Ignore key from the key-value pair.
    let [_, input; _] = matchlist(input, '\v^%([a-z\-_]*\=)?(.*)')

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

function s:need_update_panel(name) abort
  let buf = s:get_panel(a:name)
  if buf && !getbufvar(buf, 'debugger_no_update', 0)
    call setbufvar(buf, 'debugger_no_update', 1)
    return 1
  else
    return 0
  endif
endfunction

function s:update_panels() abort
  for win in range(1, winnr('$'))
    let buf = winbufnr(win)
    let bufname = bufname(buf)
    let name = matchstr(bufname, '\m^debugger://\zs.*')
    if empty(name)
      continue
    endif

    try
      call s:update_threads_panel(buf, 0)
    finally
      call setbufvar(buf, '&modified', 0)
    endtry
  endfor

    " call s:send_command('-data-list-register-values r', function('s:on_registers_info'))
    " call s:send_command('-data-list-changed-registers'.s:thread_frame(), function('s:on_list_changed_registers', [s:program.current_thread]))
endfunction

function s:go_frame(rel)
  let thread = s:program.threads[s:program.current_thread]
  let thread.current_frame += a:rel
  let thread.current_frame = max([min([get(thread, 'current_frame', 0), len(thread.stack) - 1]), 0])
  call s:update_current()
endfunction

function s:update_current() abort
  let thread = s:program.threads[s:program.current_thread]
  if has_key(thread, 'stack')
    let frame = thread.stack[thread.current_frame]

    " Remove previous mark.
    call sign_unplace('debugger', { 'id': 111 })

    if has_key(frame, 'line')
      let buf = bufnr(frame.fullname, 1)
      let win = bufwinnr(buf)
      if win ==# -1
        if bufname() =~# '\m^debugger://'
          split
        endif
        execute 'buffer' buf
      else
        silent! execute win 'windo normal'
      endif

      call sign_place(111, 'debugger', 'DebuggerCurrentFrame'.frame.level, buf, {
      \  'lnum': frame.line,
      \  'priority': 99
      \})
    endif
  endif
endfunction

function s:on_stdout(job_id, data, event) dict abort
  let self.lines[-1] .= a:data[0]
  call extend(self.lines, a:data[1:])
  for line in self.lines[:-2]
    let [line, token, prefix, data; _] = matchlist(line, '\v(\d*)(.)([^\r]*)')
    echom line
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
        echohl DebuggerError|echom printf('debugger.nvim: gdb: %s', result[1].msg)|echohl None
      else
        echohl DebuggerError|echom printf('debugger.nvim: Unknown response: %s.', string(result))|echohl None
      endif

    elseif prefix ==# '+'
      " echom  '+: ' . line[1:]

    elseif prefix ==# '*'
      let [action, data] = s:parsedata(data, 1)
      " echom  '*: ' . string([action, data])
      "
      if action ==# 'stopped'
          " if exists('frame.args')
          "   call s:send_command(printf('-symbol-info-functions --name %s', frame.func), function('s:show_frame_args_cb', [frame]))
          " endif

        call s:send_command('-thread-info', function('s:on_thread_info', [0]))

        " Append message history and update status line. We save and show
        " slightly different messages.
        let data.core = get(data, 'core', '?')
        if !has_key(data, 'frame')
          let data.frame = {}
        endif
        let data.frame.file = get(data, 'file', '?')
        let data.frame.addr = get(data, 'addr', '???')

        let message = ''
        if has_key(data, 'reason')
          if data.reason ==# 'breakpoint-hit'
            let action = printf('Breakpoint %d hit', data.bkptno)
            let message = printf('by thread %d (core %s); thread %s stopped', data['thread-id'], data.core, data['stopped-threads'])
          elseif data.reason ==# 'function-finished'
            let action = printf('Function exited')
            let message = printf('on thread %d (core %s); thread %s stopped', data['thread-id'], data.core, data['stopped-threads'])

            let thread = s:program.threads[data['thread-id']]
            " We were inside stack[0]. When function finishes we will be at
            " stack[1]. It contains the previous state of the stack so it will
            " exactly give us where stack[0] has been called.
            let frame = thread.stack[1]
            let buf = bufnr(frame.fullname, 0)
            if buf !=# -1
              call nvim_buf_set_virtual_text(buf, s:debugger_ns, str2nr(frame.line) - 1, [
              \  s:vthead, ['return', 'Keyword'], [' '], [get(data, 'return-value', 'void'), 'Normal'], s:vttail
              \], {})
            endif

          elseif data.reason ==# 'end-stepping-range'
            let action = 'Stepped'
            let message = printf('on thread %d (core %d); thread %s stopped', data['thread-id'], data.core, data['stopped-threads'])
          elseif data.reason ==# 'exited-normally'
            let action = 'Program terminated.'
          else
            let action = data.reason
          endif
        elseif has_key(data, 'signal-name')
          let action = printf('Received %s', data['signal-name'])
          let message = printf(', %s', data['signal-meaning'])
        else
          let action = printf('At %s: %s(%s)', get(data.frame, 'from', '??'), data.frame.func, data.frame.addr)
        endif

        " This one goes to message history.
        echom printf('%s(%s): %s %s', data.frame.func, data.frame.addr, action, message)

        " And this one is displayed for the user.
        echohl DebuggerProgramStop
        echo 'STOPPED'
        echohl DebuggerStatusHighlight
        echon ' '.action.' '
        echohl None
        echon message

      elseif action ==# 'running'
        call sign_unplace('debugger', { 'id': 111 })
        echohl DebuggerProgramStart|echon 'RUNNING'|echohl None
      endif
    elseif prefix ==# '='
      " echom line string(s:parsedata(data, 1))
      let [option, data] = s:parsedata(data, 1)
      if option ==# 'breakpoint-created'
        call call('s:breakpoint_add', [], data.bkpt)
      elseif option ==# 'breakpoint-modified'
        call call('s:breakpoint_mod', [], data.bkpt)
      elseif option ==# 'breakpoint-deleted'
        call call('s:breakpoint_del', [], data.bkpt)
      else
        " echom  '=: ' . option ' = ' string(data)
      endif

    elseif prefix ==# '~'
      " echom string(s:parsedata(data, 0))
      " echom  '~: ' . 
    elseif prefix ==# '@'

    elseif prefix ==# '&'
      echohl DebuggerError|echom 'debugger.nvim: gdb: '.s:parsedata(data, 0)|echohl None

    elseif prefix ==# "(" && data ==# 'gdb) '
    else
      " Drop line.
    endif
  endfor
  let self.lines = self.lines[-1:]
endfunction

  " call s:send_command(printf('save breakpoints %s', fnameescape('.gdb_breakpoints')))
function s:breakpoint_add() dict
  let s:program.breakpoints[self.number] = self
  call call('s:breakpoint_update', [], self)
endfunction

function s:breakpoint_mod() dict
  let s:program.breakpoints[self.number] = self
  call call('s:breakpoint_update', [], self)
endfunction

function s:breakpoint_del() dict
  call call('s:breakpoint_unplace', [], s:program.breakpoints[data.id])
  call filter(s:program.breakpoints, {_, bp-> bp.number != data.id})
  unlet s:program.breakpoints[data.id]
endfunction

function s:on_break_info(data)

endfunction

function s:on_break_changed(bkpt_id, data)
  call s:send_command('-break-info '.a:bkpt_id, function('s:on_break_table'))
endfunction

function! DebuggerDebugging()
  return has_key(s:, 'job')
endfunction

function s:on_exit(job_id, data, evenet)
  unlet! s:job

  call s:map_restore()
  doautocmd User DebuggerLeave

  augroup DebuggerBreakpoints
    autocmd!
  augroup END

endfunction

augroup DebuggerDefaults
  autocmd!
  autocmd User DebuggerEnter call <SID>map_default()
augroup END

function s:map_restore() abort
  for keymap in s:saved_keymaps.n
    call nvim_set_keymap('n', keymap.lhs, keymap.rhs, { 'script': keymap.script ? v:true : v:false, 'expr': keymap.expr ? v:true : v:false, 'noremap': keymap.noremap ? v:true : v:false, 'nowait': keymap.nowait ? v:true : v:false, 'silent': keymap.silent ? v:true : v:false })
  endfor

  unlet s:saved_keymaps
endfunction
 
let s:regnames = []
let s:regnamescn = -1
let s:regpos = {}

function s:get_panel(name) abort
  let buf = bufnr('debugger://'.a:name)
  return bufwinnr(buf) !=# -1 ? buf : 0
endfunction

" #0  m4_traceon (obs=0x24eb0, argc=1, argv=0x2b8c8)
"     at builtin.c:993
" #1  0x6e38 in expand_macro (sym=<optimized out>) at macro.c:242
" #2  0x6840 in expand_token (obs=0x0, t=<optimized out>, td=0xf7fffb08)
"     at macro.c:71

function s:on_stack_list_frames(thread_id, data) abort
  let thread = s:program.threads[a:thread_id]
  let thread.stack = a:data.stack
  let thread.current_frame = min([thread.current_frame, len(get(thread, 'stack', [])) - 1])

  if s:program.current_thread ==# a:thread_id
    call s:update_current()
  endif

  call s:update_panels()
endfunction

function s:on_stack_list_arguments(thread_id, data) abort
  let thread = s:program.threads[a:thread_id]
  let stack_args = a:data['stack-args']
  if len(stack_args) !=# len(thread.stack)
    " Out-of-sync and there is no way to make it right since we do not know
    " what mostthe bottom (highest number) stack index at the moment
    " stack-list-arguments was queried.
    return
  endif
  for args in stack_args
    let thread.stack[args.level].args = args.args
  endfor

  call s:update_panels()
endfunction

function s:on_stack_list_variables(thread_id, frame_id, data) abort
  let thread = s:program.threads[a:thread_id]
  let frame = thread.stack[a:frame_id]
  let frame.variables = a:data.variables

  call s:update_panels()
endfunction

function s:on_list_register_values(data)
  let thread = s:program.threads[s:program.current_thread]
  echoe 'eu'
  let thread.regs = a:data
endfunction

function s:on_thread_info(thread_id, data) abort
  " a:thread_id := 0 => list all threads
  if !a:thread_id
    for thread in values(s:program.threads)
      let thread.delete = 1
    endfor
  endif

  for thread in a:data.threads
    let program_thread = get(get(s:program, 'threads', {}), thread.id, {})
    let thread.current_frame = get(program_thread, 'current_frame', 0)
    " if !has_key(a:data, 'id')
      call s:send_command('-stack-list-frames --thread '.thread.id, function('s:on_stack_list_frames', [thread.id]))
      call s:send_command('-stack-list-arguments --thread '.thread.id.' 2', function('s:on_stack_list_arguments', [thread.id]))
    " endif

    call s:send_command('-stack-list-variables --thread '.thread.id.' --frame 0 2', function('s:on_stack_list_variables', [thread.id, 0]))
    if has_key(program_thread, 'stack')
      let thread.stack = program_thread.stack
    endif
    " let a:data.stack[a:data.frame.level] = a:data.frame
    " unlet a:data.frame
    let s:program.threads[thread.id] = thread
  endfor

  if !a:thread_id
    for [thread_id, thread] in items(s:program.threads)
      if get(thread, 'delete', 0) ==# 1
        if s:program.current_thread ==# thread_id
          unlet! s:program.current_thread
        endif
        unlet! s:program.threads[thread_id]
      endif
    endfor
  endif

  if !has_key(s:program, 'current_thread') && !empty(s:program.threads)
    let thread = values(s:program.threads)[0]
    let s:program.current_thread = thread.id
    call s:update_current()
  endif

  call s:update_panels()
endfunction

function s:on_list_changed_registers(thread_id, result) abort
  call s:send_command('-data-list-register-values --thread '.thread_id.' --skip-unavailable x '.join(a:result['changed-registers'], ' '), function('s:on_list_register_values'))
endfunction

function s:xx(result) abort
  let s:frames = a:result.stack

  let buf = s:get_panel('stack')
  if !buf
    return
  endif

  try
    let line = 0
    call nvim_buf_set_lines(buf, line, -1, 0, [])
    for frame in s:frames
      call nvim_buf_set_lines(buf, line, line + 1, 0, [
      \  printf('%1s#%-2d %s() at %s', frame.level ==# s:selected_frame ? '*' : '', frame.level, frame.func, has_key(frame, 'file') ? frame.file.':'.get(frame, 'line', '?') : frame.addr),
      \])
      let line += 1
    endfor
  finally
    call s:finish_pane_update(buf)
  endtry
endfunction

highlight default link DebuggerRegisterChange DiffChange
highlight link DebuggerError Error
highlight default DebuggerStatusHighlight gui=bold
highlight default DebuggerProgramStart gui=bold guibg=#a0df2f guifg=#fefefe
highlight default DebuggerProgramStop gui=bold guibg=#f40000 guifg=#fefefe
highlight default DebuggerProgramConnect guibg=#fde74c guifg=#000000

function s:finish_pane_update(buf) abort
  " redraw
endfunction

function s:aeu(result)
  let buf = s:get_panel('registers')
  if !buf
    return
  endif

  let cbuf = bufnr()
  try
    let addnew = nvim_buf_get_lines(buf, -2, -1, 0)[0] !=# '.'
  catch
    let addnew = 1
  endtry

  try
    call nvim_buf_clear_namespace(buf, s:debugger_ns, 0, -1)

    let view = winsaveview()

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
          let valcol = col + len(reg.name)
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
        let line = nvim_buf_get_lines(buf, lnum, lnum + 1, 1)[0]
      catch
        let line = ''
      endtry
      call nvim_buf_set_lines(buf, lnum, lnum + 1, 0, [matchstr(strpart(line, 0, col - 1).text.strpart(line, col - 1 + len(text)), '\v^.{-}\ze\s*$')] + (new ? [''] : []))

      call nvim_buf_add_highlight(buf, s:debugger_ns, 'DebuggerRegisterChange', lnum, valcol, valcol + len)
    endfor
    let s:regnamescn = cn

    call winrestview(view)
  finally
    silent! execute bufwinnr(cbuf) 'windo normal'
    call s:finish_pane_update(buf)
  endtry
endfunction

function s:update_threads_panel(buf, thread_id) abort
  let lines = []
  for thread in sort(values(s:program.threads), {x,y-> x.id - y.id})
    call add(lines, printf('%s %d %s %s:', thread.id == s:program.current_thread ? '*' : ' ', thread.id, thread['target-id'], thread.state))
    for frame in get(thread, 'stack', [])
      call add(lines, printf('  #%-2d %s in %s (%s) at %s', frame.level, frame.addr, frame.func, join(map(copy(get(frame, 'args', [])), {_, arg-> printf('%s=(%s)%s', arg.name, arg.type, arg.value)}), ', '), (has_key(frame, 'line') ? printf('%s:%d', frame.file, str2nr(frame.line)) : get(frame, 'from', '??'))))
      for var in get(frame, 'variables', [])
        if !get(var, 'arg', 0)
          call add(lines, printf('    %s=(%s)%s', var.name, var.type, get(var, 'value', 'void')))
        endif
      endfor
    endfor
  endfor
  call nvim_buf_set_lines(a:buf, 0, -1, 0, lines)
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

    call nvim_buf_set_virtual_text(buffer, s:debugger_ns, str2nr(symbol.line) - 1, [
    \  s:vthead, ['  '.join(map(a:frame.args, {_, arg-> printf('%s=%s', arg.name, arg.value)}), ', '), 'Normal'], s:vttail
    \], {})

    return
  endfor

  throw 'no such function: ' . string(a:frame)
endfunction

function s:start() abort
  if has_key(s:, 'job')
    return
  endif

  let s:currtoken = 0
  let s:Commands = {}
  let s:job = jobstart(['gdb', '--interpreter=mi3', '--quiet', '-fullname', '-nh'], {
  \  'on_stdout': function('s:on_stdout'),
  \  'on_stderr': function('s:on_stdout'),
  \  'on_exit': function('s:on_exit'),
  \  'lines': [''],
  \  'pty': v:true,
  \})
  " We are async.
  call s:send_command('-gdb-set mi-async on')
  " Enable Python-base frame filters.
  call s:send_command('-enable-frame-filters')
  " call s:send_command(\"define hook-stop\nframe\nbacktrace\nend\")
  " hook[post]-(run|continue|{command-name})
endfunction

function g:DebuggerStatus() abort

endfunction

let s:vthead = [' /* ', 'Comment']
let s:vttail = [' */ ', 'Comment']

function s:send_command(cmd, ...) abort
  if !exists('s:job')
    echohl DebuggerError|echon 'debugger.nvim: No debugger attached'|echohl None
    return
  endif

  let s:currtoken += 1
  let s:Commands[s:currtoken] = get(a:000, 0, function('s:noop'))
  let msg = printf("%d%s\n", s:currtoken, a:cmd)
  " echon '->' . msg
  call chansend(s:job, msg)
endfunction

function s:breakpoint_update() dict abort
  let buf = bufnr(self.fullname)
  if buf ==# -1
    return
  endif

  call call('s:breakpoint_unplace', [buf], self)
  let self.debugger_sign = sign_place(0, 'debugger', 'DebuggerBreakpoint'.(self.enabled ==# 'y' ? (self.disp ==# 'keep' ? '' : 'Once') : 'Disabled'), buf, { 'lnum': str2nr(self.line), 'priority': 89 })
  let self.debugger_signns = nvim_buf_set_virtual_text(buf, s:debugger_ns, self.line - 1, [
  \  s:vthead, [(0 <# get(self, 'ignore', 0) ? 'ignore='.(self.ignore).' ' : '').'times='.(self.times).(has_key(self, 'cond') ? ' if='.self.cond : '').(has_key(self, 'thread') ? ' thread='.self.thread : '').(' id='.self.number. ' '.get(self, 'pass', '?')), 'Normal'], s:vttail
  \], {})
endfunction

function s:breakpoint_unplace(buffer) dict abort
  if has_key(self, 'debugger_sign')
    call sign_unplace('debugger', { 'id': self.debugger_sign })
    call nvim_buf_clear_namespace(a:buffer, 0, str2nr(self.line) - 1, str2nr(self.line))
    unlet self.debugger_sign
  endif
endfunction

function s:on_break_table(result) abort
  for bkpt in a:result['BreakpointTable'].body
    call call('s:breakpoint_update', [], bkpt)
  endfor
endfunction

function s:setup_breakpoints() abort
  let path = s:getcfile()
  for [_, bkpt] in items(s:program.breakpoints)
    if bkpt.fullname ==# path
      call call('s:breakpoint_update', [], bkpt)
    endif
  endfor
endfunction

highlight default DebuggerBreakpoint      cterm=NONE guifg=#ee3000 gui=bold ctermfg=160
highlight link DebuggerBreakpointDisabled DebuggerBreakpoint
call sign_define('DebuggerBreakpointOnce', {
\  'text': '?B',
\  'texthl': 'DebuggerBreakpoint',
\})
call sign_define('DebuggerBreakpoint', {
\  'text': ' B',
\  'texthl': 'DebuggerBreakpoint',
\})
call sign_define('DebuggerBreakpointDisabled', {
\  'text': ' b',
\  'texthl': 'DebuggerBreakpointDisabled',
\})
call sign_define('DebuggerHardwareBreakpoint', {
\  'text': ' H',
\  'texthl': 'DebuggerHardwareBreakpoint',
\})
call sign_define('DebuggerHardwareBreakpointDisabled', {
\  'text': ' h',
\  'texthl': 'DebuggerHardwareBreakpointDisabled',
\})

function s:generate_frame_highlights(depth) 
  if a:depth ==# 0
    highlight DebuggerFrame0 gui=NONE guibg=#cfcfcf
    highlight DebuggerCurrentFrame0 gui=NONE guibg=#fcfc00
  else
    execute printf('highlight link DebuggerFrame%d DebuggerFrame%d', a:depth, a:depth - 1)
    execute printf('highlight link DebuggerCurrentFrame%d DebuggerCurrentFrame%d', a:depth, a:depth - 1)
  endif

  execute printf('highlight link DebuggerFrameSign%d DebuggerFrame%d', a:depth, a:depth)
  call sign_define('DebuggerFrame'.a:depth, {
  \  'text': (a:depth.'>')[:1],
  \  'texthl': 'DebuggerFrameSign'.a:depth,
  \  'linehl': 'DebuggerFrame'.a:depth,
  \  'numhl': 'DebuggerFrame'.a:depth
  \})

  execute printf('highlight link DebuggerCurrentFrameSign%d DebuggerCurrentFrame%d', a:depth, a:depth)
  call sign_define('DebuggerCurrentFrame'.a:depth, {
  \  'text': (a:depth.'>')[:1],
  \  'texthl': 'DebuggerCurrentFrameSign'.a:depth,
  \  'linehl': 'DebuggerCurrentFrame'.a:depth,
  \  'numhl': 'DebuggerCurrentFrame'.a:depth
  \})
endfunction

for depth in range(0, 10)
  call s:generate_frame_highlights(depth)
endfor

function s:noop(result) abort
endfunction

function s:break_insert_cb(result) abort
  call call('s:breakpoint_add', [], a:result.bkpt)
endfunction

function s:on_debugger_connected(result) abort
  let s:program.breakpoints = {}

  " call s:send_command(printf('source %s', fnameescape('/tmp/bkpts')), function('s:startup'))

  " call s:send_command('-data-list-register-names', function('s:list_reg_names_cb'))

  augroup DebuggerBreakpoints
    autocmd!
    autocmd BufEnter * :call <SID>setup_breakpoints()
  augroup END

  let s:saved_keymaps = { 'n': nvim_get_keymap('n') }
  doautocmd User DebuggerEnter

endfunction

function s:list_reg_names_cb(result)
  let s:regnames = a:result['register-names']

  let regsbuf = bufnr('debugger://registers', 1)
  execute 'vsplit | buffer' regsbuf ' | resize 6'
endfunction

autocmd BufWinEnter debugger://* ++nested
  \ setlocal buftype=nofile bufhidden=wipe noswapfile undolevels=-1 nonumber norelativenumber|
  \ let &l:filetype = substitute(expand("<amatch>"), '\V://', '-', '')|
  \ let b:did_filetype = 1|
  \ call s:update_panels()

function s:read_remote(buffer) abort
  echom 'read'
  call setbufvar(a:buffer, '&buftype', 'nofile')
  call setbufvar(a:buffer, '&swapfile', 0)
  call setbufvar(a:buffer, '&modifiable', 0)

  let localfile = tempname()
  echom bufname(a:buffer)
  call s:send_command(printf('-target-file-get %s %s', matchstr(bufname(a:buffer), '\m://\zs.*'), localfile), function('s:target_file_get_cb', [a:buffer, localfile]))
endfunction

function s:target_file_get_cb(buffer, localfile, result) abort
  " execute printf('normal %dbufdo :0read %s', a:buffer, fnameescape(a:localfile))
  call setbufvar(a:buffer, '&buftype', 'acwrite')
  " call nvim_buf_set_lines(a:buffer, 0, -1, 1, readfile(a:localfile))
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
  autocmd BufEnter debugger-target://* call <SID>read_remote(bufnr())
  autocmd BufWriteCmd debugger-target://* call <SID>write_remote(bufnr())
augroup END

function s:startup(result)
  " call s:send_command(printf('source %s', fnameescape('.gdb_breakpoints')), function('s:startup'))
  call s:send_command('-break-list', function('s:on_break_table'))
endfunction

function s:show_pwd_cb(result) abort
  redraw
  echo a:result.cwd
endfunction

function s:getcfile() abort
  return matchstr(expand('%:p'), '\m^\%(debugger-target://\)\?\zs.*')
endfunction

command! -range -nargs=? -count=1 DebuggerBreakpointAdd call <SID>send_command(empty(<q-args>) ? printf('-break-insert %s:%d', s:getcfile(), line('.')) : printf('-break-insert %s', <q-args>), function('<SID>break_insert_cb'))
command! -range -nargs=? -count=1 DebuggerBreakpointDisable call <SID>send_command(empty(<q-args>) ? printf('-break-insert %s:%d', s:getcfile(), line('.')) : printf('-break-insert %s', <q-args>), function('<SID>break_insert_cb'))
command! -range -nargs=+ -count=1 DebuggerBreakpointAddIf call <SID>send_command(printf('-break-insert %s:%d -c %s', s:getcfile(), line('.'), <q-args>), function('<SID>break_insert_cb'))
command! -count=1 DebuggerNext call <SID>send_command('-exec-next <count>')
command! -count=1 DebuggerNextInstruction call <SID>send_command('-exec-next-instruction <count>')
command! -count=1 DebuggerPrev call <SID>send_command('-exec-next --reverse <count>')
command! -count=1 DebuggerPrevInstruction call <SID>send_command('-exec-next-instruction --reverse <count>')
command! -nargs=? DebuggerRun call <SID>send_command('-exec-run <args>')
command! -nargs=* DebuggerArgs call <SID>send_command('-exec-arguments <args>')
command! -range DebuggerJump call <SID>send_command('-exec-jump <count>')
command! -count=1 DebuggerReturn call <SID>send_command('-exec-return')
command! -count=1 DebuggerStep call <SID>send_command('-exec-step <count>')
command! -count=1 DebuggerExecUntil call <SID>send_command(printf('-exec-until %s:%d', s:getcfile(), line('.')))
command! -count=1 DebuggerStepInstruction call <SID>send_command('-exec-step-instruction <count>')
command! -count=0 DebuggerContinue call <SID>send_command('-exec-continue')
command! DebuggerStepOut call <SID>send_command('-exec-finish')
command! DebuggerInterrupt call <SID>send_command('-exec-interrupt')
command! DebuggerInterruptAll call <SID>send_command('-exec-interrupt --all')
command! DebuggerExit call <SID>send_command('-exit', function('<SID>expect_exit_cb'))
command! DebuggerPwd call <SID>send_command('-environment-pwd', function('<SID>show_pwd_cb'))
command! -nargs=+ DebuggerCd call <SID>send_command('-environment-cd <args>')
command! DebuggerFrameDown call <SID>go_frame(-1)
command! DebuggerFrameUp call <SID>go_frame(1)
command! DebuggerKill call jobstop(s:job)

nnoremap <silent><buffer> <Leader><Leader>b :<C-U>DebuggerBreakpointAdd<CR>
nnoremap <silent><buffer> <Leader><Leader>c :<C-U>DebuggerContinue<CR>

function s:map_default()
  nnoremap <silent> x :call <SID>update_panels()<CR>
  nnoremap <silent> ot :15split debugger://threads<CR>
  nnoremap <silent> or :15split debugger://registers<CR>
  nnoremap <silent> os :15split debugger://stack<CR>
  nnoremap <silent> Ot :15vsplit debugger://threads<CR>
  nnoremap <silent> Or :15vsplit debugger://registers<CR>
  nnoremap <silent> Os :15vsplit debugger://stack<CR>

  nnoremap <silent> bi :call <SID>send_command('-break-after 1 2', function('<SID>on_break_changed', [1]))<CR>
  nnoremap <silent> bb :<C-U>DebuggerBreakpointAdd<CR>
  nnoremap <silent> * :<C-U>DebuggerBreakpointAdd<CR>
  nnoremap <silent> bp :<C-U>DebuggerBreakpoint<CR>
  nnoremap <silent> bd :DebuggerBreakpointDisable<CR>
  nnoremap <silent> bD :DebuggerBreakpointDelete<CR>
  nnoremap B :DebuggerBreakpointAdd 
  nnoremap <silent> ]b " prev by line
  nnoremap <silent> [b " prev by line
  nnoremap <silent> ]B " prev by id
  nnoremap <silent> [B " prev by id
  nnoremap <silent> p :DebuggerInterrupt<CR>
  nnoremap <silent> P :DebuggerInterruptAll<CR>
  nnoremap <C-c> :DebuggerInterrupt<CR>
  nnoremap <silent> r :DebuggerRun<CR>
  nnoremap <silent> c :DebuggerContinue<CR>
  nnoremap <silent> CC :DebuggerJump<CR>
  nnoremap <silent> J :DebuggerFrameDown<CR>
  nnoremap <silent> K :DebuggerFrameUp<CR>
  nnoremap <silent> n :DebuggerNext<CR>
  nnoremap <silent> N :DebuggerPrev<CR>
  nnoremap <silent> t :DebuggerNextInstruction<CR>
  nnoremap <silent> T :DebuggerPrevInstruction<CR>
  nnoremap <silent> s :DebuggerStep<CR>
  nnoremap <silent> f :DebuggerStepOut<CR>
  nnoremap <silent> <expr><silent><buffer> r  ':<C-U>DebuggerReturn'.input('return ')."\<lt>CR>"
  nnoremap <silent> u  :DebuggerExecUntil<CR>
  nnoremap <C-c><C-c> :DebuggerKill<CR>
endfunction

let s:save_keymaps = {}

" DebugMap nnoremap <silent> W  :<C-U>call send_command(printf(\"watch %s\", input('watch expr ')))<CR>
" DebugMap nnoremap <silent> B  :<C-U>call send_command(printf(\"break %s\", input('break ')))<CR>
" DebugMap nnoremap <silent> db :<C-U>call send_command(printf(\"delete break %d\", line('.')))<CR>
" DebugMap nnoremap <silent> bi :<C-U>call send_command(printf(\"break %d if %s\", line('.'), input('break if ')))<CR>
" DebugMap nnoremap <silent> S  :<C-U>call send_command(\"stepi \" . v:count1)<CR>
" DebugMap nnorema<silent> p j :call send_command(\"jump 0x0\n\")<CR>
" DebugMap nnoremap <silent> F  :<C-U>call send_command(\"frame \" . v:count)<CR>
" DebugMap nnoremap <silent> +  :<C-U>call send_command(\"up \" . v:count)<CR>
" DebugMap nnoremap <silent> -  :<C-U>call send_command(\"down \" . v:count)<CR>
" DebugMap nnoremap <silent> U  :<C-U>call send_command(\"advance \" . v:count)<CR>

let s:address = '127.0.0.1:20001'
function s:debugger_connect_remote(...) abort
  call s:start()

  let s:address = get(a:000, 1, s:address)
  echom printf('Connecting to %s...', s:address)
  call s:send_command('set sysroot /')
  call s:send_command(printf('target remote %s', s:address), function('s:on_debugger_connected'))
endfunction

command! -nargs=* DC call s:debugger_connect_remote(<args>)

" vi: ts=2 sw=2 sts=2 et
