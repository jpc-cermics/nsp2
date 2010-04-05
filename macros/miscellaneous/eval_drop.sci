function eval_drop(uri)
  ext= file("extension",uri);
  fname= file('split',uri) 
  if fname(1) <> "file:" then return;end 
  fname = file('join',['/';fname(2:$)]);  
  if ext == '.sce' || ext == '.sci'
    exec(fname);
  elseif ext == '.cos' || ext == '.cosf' then 
      scicos(fname);
  end
endfunction
