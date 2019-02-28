// How to communicate values between 2 nsp 
// one being spawned by the other 

function val=test()
  S=spawn(['nsp','-nw'],'-nsp->');
  S.send['x=1:7;\n'];
  val=spawn_receive(S,'x');
endfunction 

function spawn_send(x,str)
  s=serialize(x);
  s.compress[];
  s=s.tobase64[70];
  print(s,as_read=%t,name=str);
endfunction

function val=spawn_receive(S,str)
  S.send['exec(''transmit.sce'');\n'];
  str=S.send[sprintf('spawn_send(%s,""val"");\n',str)];
  ok = execstr(str,errcatch=%t);
  if ~ok then
    val =%nan;
    return;
  else
    val= base64toserial(val);
    val = val.unserialize[];
  end
endfunction


  
