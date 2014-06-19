// Attention agclose fait que deux exec 
// successifs plantent. 
// à revoir donc 

// un graphe 
G=agraph_create();
G.add_nodes[string(1:20)]
A=grand(60,2,'uin',1,20);
G.add_edges[string(A)];
G.layout['neato'];
G.render['pdf','pipo.pdf'];
G.render['tk','pipo.tk'];

// find a node by its name 
if ~execstr('n=G.findnode["" 1""]',errcatch=%t) then pause;end
// check the name 

// loop on nodes 
if ~execstr('n=G.fstnode[]',errcatch=%t) then pause;end
if ~execstr('n=G.nxtnode[n]',errcatch=%t) then pause;end
if ~execstr('n=G.lstnode[]',errcatch=%t) then pause;end
if ~execstr('n=G.prvnode[n]',errcatch=%t) then pause;end

// get parent and root graph 
// here parent should fail.

G1=G.root[];
if ~G1.equal[G] then pause;end 

// check names 
if G.nameof[] <> "G" then pause;end
n=G.fstnode[]; 
if n.nameof[] <> " 1" then pause;end 

// get graph from node 

G1=n.agraphof[];
if ~G1.equal[G] then pause;end 


