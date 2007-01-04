
function Lc=premia_var_to_xchoices(L)
// convert a premia var list to a format suitable for x_choices 
  Lc=list();
  for i=1:length(L)
    if type(L(i)(2),'short')=='l' then 
      Lc(i)=list('button',L(i)(1),0,premia_var_to_xchoices(L(i)(2)));
    else 
      if L(i)(3)== %t then tag='entry'; else tag = 'ignore'; end
      str=sprint(L(i)(2),as_read=%t);
      // first line is a header 
      str=str(2);      
      Lc(i)=list(tag,L(i)(1),0,str);
    end
  end
endfunction

function L=x_choices_to_premia(Lc)
// back-convert an x_choice format to premia var description.
//  
  L=list()
  for i=1:length(Lc)
    select Lc(i)(1)
     case 'entry' then  L(i)=list(Lc(i)(2),evstr(Lc(i)(4)),%t);
     case 'ignore' then L(i)=list(Lc(i)(2),evstr(Lc(i)(4)),%f);
     case 'button' then L(i)=list(Lc(i)(2), ...
				  x_choices_to_premia(Lc(i)(4)));
    end
  end
endfunction

function ok=premia_check_basic(L,Lold)
// check that values in L matches types in Lold 
// we assume here that L and Lold have the same shapes 
  ok=%t;
  for i=1:length(L)
    if type(L(i)(2),'short')=='l' then 
      ok = premia_check_basic(L(i)(2),Lold(i)(2));
      if ok== %f then 
	return;
      end 
    else 
      if L(i)(3)== %t then 
	// we have to check here that values matches 
	if type(L(i)(2),'short') <> 'm' then 
	  x_message(L(i)(1)+' should be of Matrix type');
	  ok=%f;
	  return;
	end
	if length(L(i)(2))<>length(Lold(i)(2)) then 
	  x_message(L(i)(1)+' should be of length '+string(length(Lold(i)(2))));
	  ok=%f;
	  return;
	end
      end
    end
  end
endfunction;
  
function premia_model_values(M) 
// interactively set model parameters 
  B=M.get_model_values[];
  Bx=premia_var_to_xchoices(B);
  while %t then 
  [Lres,Bx]=x_choices('Choose model '+M.get_model[]+' parameters',Bx,%t);
  Bn=x_choices_to_premia(Bx);
  if premia_check_basic(Bn,B) then 
    // Now we have to check if model accept parameters 
    M.set_model_values[Bn];
    S=M.model_check[];
    if isempty(S) then 
      break;
    else
      M.set_model_values[B];
      x_message(S(1)+': '+S(2));
    end
  end
end
