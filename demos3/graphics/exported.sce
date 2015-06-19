xclear()
xset('font size',3);
x=0:0.1:2*%pi;plot2d([x]',[sin(x)]',leg='sin(x)',strf='132')
for A=[0:30:360];xstring(4.5+0.5*cos(-%pi*A/180),0.5+0.5*sin(-%pi*A/180),'Nsp',A,0);end
for A=[0:30:360];xsegs([4.5;4.5+0.5*cos(-%pi*A/180)],[0.5;0.5+0.5*sin(-%pi*A/180)]);end
gtk_logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
gtk_logo_pixbuf = gdk_pixbuf_new_from_file(gtk_logo);
xdraw_pixbuf(0,gtk_logo_pixbuf,0,0,2,2,1,1)
xstring(1,-2,'aeu grave : аищ, chapeau вкы')
//xdraw_pixbuf(0,gtk_logo_pixbuf,0,0,0.5,0,2,2)
xtitle('nsp cairo export','Time','Sin(t)')


