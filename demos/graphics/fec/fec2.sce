// -*- Mode: scilab -*- 
// An example of visualisation of a function linear on triangles
// Mesh of amdba type 
// Copyright Enpc 

if ~new_graphics() then switch_graphics();end 
path='NSP/demos/graphics/fec/';
exec(path+'macros.sci')
amdbaR(path+'MESH');
meshvisu()
xclick();xclear()
xset('colormap',jetcolormap(32));
emc2C(1,6,path+'MESH.VAL')
xclick();xclear()
xset('colormap',jetcolormap(32));
emc2C(1,6,path+'MESH.VAL',[-2,-2,2,2])
xclick();xclear();
xset('colormap',jetcolormap(32));
emc2V(2,3,6,20,path+'MESH.VAL',[-0.5,-0.5,2,0.5])

