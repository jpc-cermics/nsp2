// More stuffs 
//-------------

graphic_demo_list(13)=[contour();
		    title=["contour "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(14)=[fcontour(); 
		    title=["fcontour "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(15)=[champ();
		    title=["champ "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(16)=[fchamp();
		    title=["fchamp "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(17)=[grayplot();
		    title=["grayplot "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(18)=[fgrayplot();
		    title=["fgrayplot "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(19)=[errbar();
		    title=["errbar "];
		    xtitle(title," "," ");
		    endfunction

graphic_demo_list(20)=chart();
graphic_demo_list(21)=zgrid();
graphic_demo_list(22)=[r=(%pi):-0.01:0;x=r.*cos(10*r);y=r.*sin(10*r);
          function [z]=surf(x,y); z=sin(x)*cos(y);endfunction;
          t=%pi*(-10:10)./10;
          fplot3d(t,t,surf,35,45,"X@Y@Z",[6,2,3]);
                  z=sin(x).*cos(y);
          write(%io(2),"Adding 2d graphics on 3d graphic");
          z=sin(x).*cos(y);
          [x1,y1]=geom3d(x,y,z);
          xpoly(x1,y1,"lines");
          [x1,y1]=geom3d([0,0],[0,0],[5,0]);
          xsegs(x1,y1);
          xstring(x1(1),y1(1)," The point (0,0,0)");
          title=["plot3d and use of xgeom "];
          xtitle(title," "," ");
		    endfunction

graphic_demo_list(23)=[r=(%pi):-0.01:0;x=r.*cos(10*r);y=r.*sin(10*r);
          function [z]=surf(x,y); z=sin(x)*cos(y);endfunction;
          t=%pi*(-10:10)./10;
          fplot3d(t,t,surf,35,45,"X@Y@Z",[-12,2,3]);
                  z=sin(x).*cos(y);
          write(%io(2),"Adding 2d graphics on 3d graphic");
          z=sin(x).*cos(y);
          [x1,y1]=geom3d(x,y,z);
          xpoly(x1,y1,"lines");
          [x1,y1]=geom3d([0,0],[0,0],[5,0]);
          xsegs(x1,y1);
          xstring(x1(1),y1(1)," The point (0,0,0)");
          title=["plot3d and use of xgeom "];
          xtitle(title," "," ");
		    endfunction

graphic_demo_list(24)=[t=%pi*(-10:10)./10;
          function [z]=surf(x,y); z=sin(x)*cos(y);endfunction;
          rect=[-%pi,%pi,-%pi,%pi,-5,1];
          z=feval(t,t,surf);
          contour(t,t,z,10,35,45,"X@Y@Z",[1,1,0],rect,-5);
          plot3d(t,t,z,35,45,"X@Y@Z",[2,1,3],rect);
          title=["plot3d and contour "];
          xtitle(title," "," ");
		    endfunction

graphic_demo_list(25)=[t=%pi*(-10:10)./10;
          function [z]=surf(x,y); z=sin(x)*cos(y);endfunction;
          rect=[-%pi,%pi,-%pi,%pi,-1,1];
          z=feval(t,t,surf);
          plot3d(t,t,z,35,45,"X@Y@Z",[-1,1,3],rect);
          contour(t,t,z,10,35,45,"X@Y@Z",[0,1,0],rect);
          title=["plot3d and contour "];
          xtitle(title," "," ");
		    endfunction

graphic_demo_list(26)=[t=%pi*(-10:10)./10;
          function [z]=surf(x,y); z=sin(x)*cos(y);endfunction;
          rect=[-%pi,%pi,-%pi,%pi,-1,1];
          z=feval(t,t,surf);
          plot3d(t,t,z,35,45,"X@Y@Z",[-20,1,3],rect);
          xset("alufunction",0);
          contour(t,t,z,10,35,45,"X@Y@Z",[0,1,0],rect);
          title=["plot3d and contour X11 only"];
          xtitle(title," "," ");
          xset("default");
		    endfunction

graphic_demo_list(27)=[exec("SCI/demos/graphics/sd.sav");
          plot2d();gr_menu(sd_1,1,1);
          title=["plot2d and gr_menu "];
          xtitle(title," "," ");
		    endfunction

graphic_demo_list(28)=[exec("SCI/demos/graphics/sd.sav");
           plot3d();gr_menu(sd_2,1,1);
          title=["plot3d and gr_menu "];
          xtitle(title," "," " );
		    endfunction

graphic_demo_list(29)=[xset("font",2,0);xsetech([0,0,0.5,0.5]);plot3d();
	xsetech([0.5,0,0.5,0.5]);plot2d();
	xsetech([0.5,0.5,0.5,0.5]);grayplot();
	xsetech([0,0.5,0.5,0.5]);histplot();
	xsetech([0,0,1,1]);
	xset("default");
		    endfunction

graphic_demo_list(30)=[fac3d();
		    endfunction

graphic_demo_list(31)=[fac3d1();             ];


