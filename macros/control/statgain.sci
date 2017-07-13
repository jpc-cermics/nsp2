function st=statgain(sl)
  select type(sl,'short')
    case 'linearsys' then
     dom=sl(7);
     [m,p]=size(sl(2));
     if dom=='c' then
       if rank(sl(2)) <> m then
         error('Error: singular A matrix');
       end
       st=sl(5)-sl(4)*inv(sl(2))*sl(3);
     else
       if rank(eye(m,m)-sl(2)) <> m then
         error('Error: singular eye-a matrix');
       end
       st=sl(5)+sl(4)*inv(eye(m,m)-sl(2))*sl(3);
     end
    case 'r' then
     dom=sl.dt;
     if dom=='c' then
       st=freq(sl.num,sl.den,0)
     else
       st=freq(sl.num,sl.den,1)
     end
    else
      error("Error: first argument must be a linear system or transfer matrix");
  end
endfunction
