exec('macros/xdess/hotcolormap.sci')
exec('macros/xdess/greencolormap.sci')

xsetech(wrect=[0,0,0.5,0.5])
plot3d1()
xsetech(wrect=[0.5,0,0.5,0.5])
xset('colormap',hotcolormap(45));
plot3d1()
xsetech(wrect=[0,0.5,0.5,0.5])
xset('default_colormap');
plot3d1()
xsetech(wrect=[0.5,0.5,0.5,0.5])
xset('colormap',greencolormap(34));
plot3d1()
