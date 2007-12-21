function [X,Y,Z] = slice_parallelepiped(dir, val, ebox, nx, ny, nz)
   //
   //   ebox = [xmin xmax ymin ymax zmin zmax]
   //   dir : string "x=", val
   //   nx, ny, nz les discretisations
   //
   select dir
     case "x=" then
       if val < ebox(1)  |  ebox(2) < val then
	  error("bad slice choosen")
       end
       y = linspace(ebox(3),ebox(4),ny)
       z = linspace(ebox(5),ebox(6),nz)
       [Z,Y] = ndgrid(z,y)
       X = val*ones_new(size(Y));
     case "y=" then
       if val < ebox(3)  |  ebox(4) < val then
	  error("bad slice choosen")
       end
       x = linspace(ebox(1),ebox(2),nx)
       z = linspace(ebox(5),ebox(6),nz)
       [X,Z] = ndgrid(x,z)
       Y = val*ones_new(size(X));
     case "z=" then
       if val < ebox(5)  |  ebox(6) < val then
	  error("bad slice choosen")
       end
       x = linspace(ebox(1),ebox(2),nx)
       y = linspace(ebox(3),ebox(4),ny)
       [Y,X] = ndgrid(y,x)
       Z = val*ones_new(size(X));
   else
      error("bad arg dir")
   end // select
endfunction
