while %t 
  a=pvm_recv(pvm_parent(),0);
  if a=='exit' then break;end
  pvm_send(pvm_parent(),a,0);
end
quit;



