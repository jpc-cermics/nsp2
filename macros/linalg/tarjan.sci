function L=tarjan(Adj)
// Tarjan's strongly connected components algorithm
// Adj is an adjacency matrix i.e Adj(i,j) is non null when 
// there is a link i->j
// the non nul values must be greater than %eps
// The function can therefore be used with Markov transition matrices 
 
  if nargin == 0 then 
    M=genmarkov([1,2,3],4)
    L=tarjan(M);
    return;
  end
      
  function [Vindex,Vlowlink,Vonstack,index,S,L]=strongconnect(v,Vindex,Vlowlink,Vonstack,...
						  index,S,L)
  // Set the depth index for v to the smallest unused index
    Vindex(v) = index;
    Vlowlink(v) = index
    index = index + 1;
    S = [v,S];
    Vonstack(v) = %t;
    // Consider successors of v
    next = find(full(Adj(v,:)) > 10*%eps )
    for w = next do
      if isnan(Vindex(w)) then
	// Successor w has not yet been visited; recurse on it
	[Vindex,Vlowlink,Vonstack,index,S,L]=strongconnect(w,Vindex,Vlowlink,Vonstack,...
						  index,S,L);
	Vlowlink(v) = min(Vlowlink(v), Vlowlink(w))
      elseif Vonstack(w) then
	// Successor w is in stack S and hence in the current SCC
	Vlowlink(v)  = min(Vlowlink(v), Vindex(w))
      end
    end
    // If v is a root node, pop the stack and generate an SCC
    if Vlowlink(v) == Vindex(v) then
      // start a new strongly connected component
      SCC=[v];
      while %t do
	w = S(1); S(1)=[];
	Vonstack(w) = %f;
	// add w to current strongly connected component
	if w == v then break;end 
	SCC($+1)=w;
      end
      L{$+1}=gsort(SCC,"g","i");
    end
  endfunction
    
  index = 0
  S = [];
  n=size(Adj,1);
  V = 1:n
  Vindex= %nan*(1:n);
  Vlowlink= %nan*(1:n);
  Vonstack= m2b(zeros(1,n));
  S=[];
  L={};
  for v = V do
    if isnan(Vindex(v)) then 
      [Vindex,Vlowlink,Vonstack,index,S,L]=strongconnect(v,Vindex,Vlowlink, Vonstack,index,...
						  S,L);
    end
  end
endfunction

