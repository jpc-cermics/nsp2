// obsolete function 
function [V] = variance_m(x, varargin, varargopt)
   printf("\nWARNING: variance is obsolete, use var instead\n")
   V = var(x, varargin(:), varargopt(:))
endfunction
