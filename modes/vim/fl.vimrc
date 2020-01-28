
:if version >= 500

" for fl
:let $fl_inp = "/tmp/fl_cseger_inp_" . strftime("%H_%M_%S")
:let $fl_out = "/tmp/fl_cseger_out_" . strftime("%H_%M_%S")

" --------- Starting fl ---------
" Normal fl
map <silent> @f :!fl --read_input_from_file $fl_inp -use_stdout >& $fl_out & <CR><CR>
map <silent> @F :!fl --read_input_from_file $fl_inp -use_stdout >& $fl_out & <CR><CR>

" --------- Killing fl ---------
map <silent> @q :! echo 'quit;' >> $fl_inp <CR><CR>
map <silent> @Q :! echo 'quit;' >> $fl_inp <CR><CR>

" --------- Help information ---------
" Get help on word or symbol under cursor (search back and forward to get id)
:function Select_help()
:  if ( match(strpart(getline(line(".")), col(".") -1, 1), "[a-zA-Z_]") == 0 )
:     exe ":! echo 'print (help " . '"' . @k . '");' . "'" . " >>" . $fl_inp
:  else
:     exe ":! echo 'print (help " . '"' . @l . '");' . "'" . " >>" . $fl_inp
:  endif
:endfunction
map <silent> @? mK/[^a-z0-9A-Z_']<CR>mL`K"ky`L`K/[a-zA-Z0-9()   "]<CR>mL`K"ly`L`K:call Select_help()<CR><CR>


" --------- Open a window in vim to see stdout from fl ---------
function! See_stdout()
    exe "new " . $fl_out
    setlocal autoread
endfunction
map <silent> @O   :silent call See_stdout()<CR>G
map <silent> @o   :silent call See_stdout()<CR>G


" --------- Function to append text to the fl_inp file ---------
function! Send_fl_lines() range
    exe "redir >> " . $fl_inp
    silent echon "set_file_name \"" . expand('%:t') . "\";"
    silent echon "set_line_number " . a:firstline . ";"
    silent exe ": " a:firstline "," a:lastline "p"
    redir END
endf

" --------- Send regions ---------
" Single line
map @@ :call Send_fl_lines()<CR>
" To end of paragraph
map @} }:silent '',. call Send_fl_lines()<CR>
" From beginning of paragraph
map <silent> @{ {@}
" To end of file
map @G G:silent '',. call Send_fl_lines()<CR>G
" Everything between 'a and 'z marks
map @A :silent 'a,'z call Send_fl_lines()<CR><CR>'z
" From current to 'z mark
map @Z 'z:silent '',. call Send_fl_lines()<CR><CR>'z
" All of buffer
map <silent> @B :w! ./.fl_tmp<CR>:! echo 'load "./.fl_tmp";' >> $fl_inp<CR><CR><CR><CR>

:endif

