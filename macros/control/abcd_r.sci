function [A,B,C,D]=abcd_r(sl)
  [A,B,C,D]=abcd(tf2ss(sl));
endfunction
