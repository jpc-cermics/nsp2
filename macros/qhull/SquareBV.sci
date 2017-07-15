function [CellArea,V,C]=SquareBV(x,y,toggleplot=%f,rect=[0,1,0,1])
// SQUAREBV   Arbitrary Square Bounded Voronoi Diagram
// This function compute the individual Voronoi cell area of point sets
// bounded in an arbitrary square

// Written by / send comments or suggestions to :
// Meng Sang, Ong (ongdotmengdotsangatengdotmonashdotedudotmy) 
// & Ye Chow, Kuang (kuangdotyedotchowatmonashdotedu)
// Photonic, Semiconductor & Communication Research Group
// School of Engineering
// Monash University Sunway campus
// October 2010
//  Inputs:
//  x             : M x 1 array of x-coordinates
//  y             : M x 1 array of y-coordinates
//  toggleplot    : 1 to turn on figures (may take longer time for large samples)
//                  0 to turn off figures;
//  [x1,x2,y1,y2] : 1 x 4 array of square edges
//  Outputs:
//  CellArea      : M x 1 array of Voronoi cell area bounded in an arbitrary square
//  Usage:
//  CellArea=SquareBV(x,y,toggleplot,[x1,x2,y1,y2])
//  CellArea=SquareBV(x,y,toggleplot)

  if nargin <= 0 then 
    // Define square edges
    x1 = 0;  x2 = 1;
    y1 = 0;  y2 = 1;
    // Generate test point set in a unit square
    M = 10;
    x = (x2-x1)*rand(M,1) + x1;
    y = (y2-y1)*rand(M,1) + y1;
    // Calculate individual cell area of each test point
    [CellArea,V,C]=SquareBV(x,y,toggleplot=%t,rect=[x1,x2,y1,y2]);
    // CellArea=SquareBV(x,y,0);
    return;
  end
    
  function CellArea = CalcArea_UnitSquare(VSet, V1, V2, ptS)

    global x_VoronoiGlobal y_VoronoiGlobal
    global LeftEdge RightEdge BtmEdge TopEdge

    x = x_VoronoiGlobal;
    y = y_VoronoiGlobal;

    S               = [x(ptS),y(ptS)];
    xOther          = x;
    xOther(ptS)     = [];
    yOther          = y;
    yOther(ptS)     = [];

    VBoundary       = [V1;V2];
    VAll            = [VSet;VBoundary];
    Centre          = mean(VAll,1);
    // SAngle          = atan2(S(2)-Centre(2),S(1)-Centre(1));
    Angle           = atan(VBoundary(:,2)-Centre(2),VBoundary(:,1)-Centre(1));
    // Angle           = atan2([VBoundary(:,2)],[VBoundary(:,1)]);
    [Angle,index]   = sort(Angle,'g','i');
    VBoundary       = VBoundary(index,:);

    ptNext      = [2:size(VBoundary,1),1];
    flagBtmLeft  = %f;
    flagTopLeft  = %f;
    flagTopRight = %f;
    flagBtmRight = %f;

    for index = 1 : size(VBoundary,1)
      // Determine if there is a point inside the Voronoi cell
      dx = VBoundary(index,1)-VBoundary(ptNext(index),1);
      dy = VBoundary(index,2)-VBoundary(ptNext(index),2);
      
      Side            = sign((xOther-VBoundary(index,1))*dy - (yOther-VBoundary(index,2))*dx);
      RefSideBtmLeft  = sign((LeftEdge-VBoundary(index,1))*dy  - (BtmEdge-VBoundary(index,2))*dx);
      RefSideTopLeft  = sign((LeftEdge-VBoundary(index,1))*dy  - (TopEdge-VBoundary(index,2))*dx);
      RefSideTopRight = sign((RightEdge-VBoundary(index,1))*dy - (TopEdge-VBoundary(index,2))*dx);
      RefSideBtmRight = sign((RightEdge-VBoundary(index,1))*dy - (BtmEdge-VBoundary(index,2))*dx);
      
      // Append square edges to the corresponding Voronoi cell
      if (~any(Side==RefSideBtmLeft) && (RefSideBtmLeft ~= 0))
	if ~flagBtmLeft
	  VAll = [VAll; [LeftEdge, BtmEdge]];
	  flagBtmLeft = %t;
	end
      end
      
      if (~any(Side==RefSideTopLeft) && (RefSideTopLeft ~= 0))
	if ~flagTopLeft
	  VAll = [VAll; [LeftEdge, TopEdge]];
	  flagTopLeft = %t;
	end
      end
      
      if (~any(Side==RefSideTopRight) && (RefSideTopRight ~= 0))
	if ~flagTopRight
	  VAll = [VAll; [RightEdge, TopEdge]];
	  flagTopRight = %t;
	end
      end
      
      if (~any(Side==RefSideBtmRight) && (RefSideBtmRight ~= 0))
	if ~flagBtmRight
	  VAll = [VAll; [RightEdge, BtmEdge]];
	  flagBtmRight = %t;
	end
      end
      
    end

    // Calculate Voronoi Cell Area
    vec             = VAll - repmat(Centre, size(VAll,1),1);
    Angle           = atan(vec(:,2),vec(:,1));
    // attention 
    [sorted,index]  = sort(Angle,'g','i'); //#ok<ASGLU>
    VAll            = VAll(index,:);
    CellArea        = polyarea(VAll(:,1), VAll(:,2));
    if size(CellArea,'*')==0 then CellArea=0;end 
  endfunction 
  
  function [xc, yc] = BoundaryIntersect_UnitSquare(P1, P2, Mode)

    global LeftEdge RightEdge BtmEdge TopEdge
    x1 = P1(:,1);
    y1 = P1(:,2);
    x2 = P2(:,1);
    y2 = P2(:,2);
    dx = x1 - x2;
    dy = y1 - y2;
    bNonsingular = (dx~=0);
    bSingular    = ~bNonsingular;
    
    select Mode
     case 1
      // One point outside of square while the other inside
      // Non-singular case
      if (bNonsingular)    // Regular equation of line
	m = dy./dx;
	// Calculate square intersect
	yc1 = m.*(LeftEdge-x1) + y1;
	yc2 = m.*(RightEdge-x1) + y1;
	xc1 = 1./m.*(BtmEdge-y1) + x1;
	xc2 = 1./m.*(TopEdge-y1) + x1;
	ptIntersect = [];
	// Check for valid intersects
	if (yc1 >= BtmEdge && yc1 <= TopEdge) then
	  ptIntersect = [ptIntersect; [LeftEdge, yc1]];
	end;
	if (yc2 >= BtmEdge && yc2 <= TopEdge)  then
	  ptIntersect = [ptIntersect; [RightEdge, yc2]]; 
	end
	if (xc1 >= LeftEdge && xc1 <= RightEdge) then
	  ptIntersect = [ptIntersect; [xc1, BtmEdge]]; 
	end
	if (xc2 >= LeftEdge && xc2 <= RightEdge) then 
	  ptIntersect = [ptIntersect; [xc2, TopEdge]]; 
	end
	intersect1 = ptIntersect(1,:);
	intersect2 = ptIntersect(2,:);
      end
      // Singular case
      if (bSingular) then  // vertical line
	// Calculate square intersect
	intersect1 = [x1, TopEdge];
	intersect2 = [x1, BtmEdge];
      end
      // The dot product of (p1-intersect).(p2-intersect) is negative 
      // if the intersection point
      // lie between two points, as the two vectors take on different direction
      // Due to the point configuration, only one "intersect" test is
      // needed (the other one is logial compliment)
      v1 = sum((P1-intersect1).*(P2-intersect1),2);
      // v2 = sum((P1-intersect2).*(P2-intersect2),2);
      v1neg = v1<0;
      v2neg = ~v1neg;
      xc(v1neg) = intersect1(v1neg,1);
      xc(v2neg) = intersect2(v2neg,1);
      yc(v1neg) = intersect1(v1neg,2);
      yc(v2neg) = intersect2(v2neg,2);
     case 2
      // Both points are outside of unit circle
      if (bNonsingular)    // Regular equation of line
	m = dy./dx;
	// Calculate square intersect
	yc1 = m.*(LeftEdge-x1) + y1;
	yc2 = m.*(RightEdge-x1) + y1;
	xc1 = 1./m.*(BtmEdge-y1) + x1;
	xc2 = 1./m.*(TopEdge-y1) + x1;
	ptIntersect = [];
	// Check for valid intersects
	if (yc1 >= BtmEdge && yc1 <= TopEdge) then
	  ptIntersect = [ptIntersect; [LeftEdge, yc1]]; 
	end
	if (yc2 >= BtmEdge && yc2 <= TopEdge) then
	  ptIntersect = [ptIntersect; [RightEdge, yc2]]; 
	end
	if (xc1 >= LeftEdge && xc1 <= RightEdge) then 
	  ptIntersect = [ptIntersect; [xc1, BtmEdge]]; 
	end
	if (xc2 >= LeftEdge && xc2 <= RightEdge) then 
	  ptIntersect = [ptIntersect; [xc2, TopEdge]]; 
	end
	if (isempty(ptIntersect))
	  intersect1 = %nan([1;1])';//nan(1,2);
	  intersect2 = %nan([1;1])';// nan(1,2);
	else
	  intersect1 = ptIntersect(1,:);
	  intersect2 = ptIntersect(2,:);
	end
	
	// Ensures that [xc,yc] is between the two voronoi vertices.
	v1 = sum((P1-intersect1).*(P2-intersect1),2);
	v2 = sum((P1-intersect2).*(P2-intersect2),2);
	
	// Check that the line connecting two voronoi vertices 
	// intersects the square
	if (v1<0 && v2<0) 
	  xc = [intersect1(1),intersect2(1)];
	  yc = [intersect1(2),intersect2(2)];
	else
	  xc = [%nan,%nan];
	  yc = [%nan,%nan];
	end
      end
      if (bSingular)   // vertical line
	if (x1>=LeftEdge && x1<= RightEdge)
	  xc = [x1; x1];
	  // yc = [LeftEdge; RightEdge]; 
	  yc = [BtmEdge; TopEdge];
	else
	  xc = [%nan,%nan];
	  yc = [%nan,%nan];
	end
      end    
    end
  endfunction 

  function [V, C] = BoundVoronoin_UnitSquare(V, C,toggleplot=%f)
    
    global x_VoronoiGlobal y_VoronoiGlobal
    global LeftEdge RightEdge BtmEdge TopEdge

    function y=realsqrt(x) ; y=sqrt(x);endfunction 
    
  // An arbitrarily selected small number for distance comparison
    Epsilon = 1e-14;

    x = x_VoronoiGlobal;
    y = y_VoronoiGlobal;

    // Identify points with unbounded cell
    
    LenC = size(C,'*'); // equals to M
    
    ptUnbound = [];
    for index = 1 : LenC
      if C{index}.has[1]  // Unbounded cell was ismember(1,C{index}) in matlab
	ptUnbound = [ptUnbound;index]; //#ok<AGROW>
      end
    end
    // List the unbounded cells in counter-clockwise direction
    UnboundCentre = mean([x(ptUnbound),y(ptUnbound)],1);
    // Angle =
    // atan2(y(ptUnbound)-UnboundCentre(2),x(ptUnbound)-UnboundCentre(1));
    Angle = atan(y(ptUnbound)-UnboundCentre(2),x(ptUnbound)-UnboundCentre(1));
    [Angle,ptptUnbound] = sort(Angle,'g','i'); //#ok<ASGLU>
    ptUnbound = ptUnbound(ptptUnbound);
    Orphan_x = x(ptUnbound);
    Orphan_y = y(ptUnbound);

    rect=[LeftEdge,BtmEdge,RightEdge,TopEdge];
    of=1;
    rectp=[LeftEdge-of,BtmEdge-of,RightEdge+of,TopEdge+of];
    
    if toggleplot
      // attention à revoir.
      [vx,vy] = voronoi(x,y);
      xset('window',1);
      plot2d([],[],rect=rectp);
      //hold on
      plot2d(vx,vy,style=ones(1,size(vx,2)),axesflag=0,rect=rectp);
      //plot2d(V(2:$,1),V(2:$,2),style=-3,axesflag=0,rect=rectp)
      plot2d(x,y,style=0,axesflag=0,rect=rectp);
      plot2d(Orphan_x,Orphan_y,style=-2,axesflag=0,rect=rectp)
      xrect(LeftEdge,BtmEdge+TopEdge,RightEdge,TopEdge)
    end
    
    //// Find suitable replacement for infinity points
    // Pointer to the next(circular) unbounded cell
    ptNext = [2:length(ptUnbound),1];
    for index = 1 : length(ptUnbound)
      ptV = C{ptUnbound(index)};
      // =====================================================================
      if toggleplot
	// Display voronoi diagram
	xset('window',2);
	plot2d([],[],rect=rectp);
	plot2d(Orphan_x,Orphan_y,style=-2,axesflag=0,rect=rectp);
	plot2d(x(ptUnbound(index)),y(ptUnbound(index)),style=-3,...
	       axesflag=0,rect=rectp);
	plot2d(vx,vy,style=ones(1,size(vx,2)),axesflag=0,rect=rectp);
	for jndex = 1 : length(ptV)
	  if ptV(jndex) ~= 1
	    plot2d(V(ptV(jndex),1),V(ptV(jndex),2),style=-4,axesflag=0,rect=rectp);
	  end
	end
      end
      // =====================================================================
      P1x         = Orphan_x(index);
      P1y         = Orphan_y(index);
      P2x_Next    = Orphan_x(ptNext(index));
      P2y_Next    = Orphan_y(ptNext(index));
      ptVFinite   = setdiff(ptV,[1]);
      // Identify voronoi vertex that is closest to the proposed perpendicular bisector
      Dist  = abs(V(ptVFinite,1)*(P1x-P2x_Next) + ... 
		  V(ptVFinite,2)*(P1y-P2y_Next) ...
		  -(P1y^2-P2y_Next^2+P1x^2-P2x_Next^2)/2);
      [MinDist,jndex] = min(Dist); //#ok<ASGLU>
      // NormVoronoiPoint = realsqrt(sum(V(ptVFinite(jndex),:).^2));
      // NormVoronoiPoint = V(ptVFinite(jndex),1)<= RightEdge & V(ptVFinite(jndex),1)>= LeftEdge & V(ptVFinite(jndex),2)<= TopEdge & V(ptVFinite(jndex),2)>= BtmEdge;
      // Identifies the points that uses ptVFinite(jndex) as voronoi vertice
      ptP = [];    bContinue = %t;   Counter=0;      PCounter=0;
      while bContinue
	Counter = Counter + 1;
	// if ismember(ptVFinite(jndex),C{Counter})
	if C{Counter}.has[ptVFinite(jndex)] then 
	  PCounter = PCounter + 1;
	  ptP(PCounter) = Counter;
	  if PCounter>=3
	    bContinue = %f;
	  end
	end
	if Counter >= LenC
	  bContinue = %f;
	end
      end
      
      // Find the bisector and append the new point to appropriate entries in C
      for Counter1 = 1 : PCounter-1
	for Counter2 = Counter1 + 1 : PCounter
	  ptLocalVSet = setdiff(union(C{ptP(Counter1)},C{ptP(Counter2)}),[1,ptVFinite(jndex)]);
	  // Equation of bisector (y-y0)(dy)+(x-x0)(dx)=0
	  MidPoint    = 0.5*[x(ptP(Counter1))+x(ptP(Counter2)),y(ptP(Counter1))+y(ptP(Counter2))];
	  dx          = x(ptP(Counter1))-x(ptP(Counter2));
	  dy          = y(ptP(Counter1))-y(ptP(Counter2));
	  VecLen      = realsqrt(dx^2+dy^2);
	  dx          = dx/VecLen;
	  dy          = dy/VecLen;
	  DistBisect  = abs( (V(ptLocalVSet,1)-MidPoint(1))*dx + (V(ptLocalVSet,2)-MidPoint(2))*dy );
	  bNoHit      = DistBisect > Epsilon;
	  if all(bNoHit)
	    
	    // Can't find a finite voronoi vertice to complement ptVFinite(jndex)
	    // Create a new point outside the rectangle
	    Lambda = 2*realsqrt((V(ptVFinite(jndex),1)-x(ptP(Counter1))).^2+(V(ptVFinite(jndex),2)-y(ptP(Counter1))).^2);
	    
	    P_ex1 = V(ptVFinite(jndex),:) + Lambda*[dy,-dx];
	    P_ex2 = V(ptVFinite(jndex),:) + Lambda*[-dy,dx];
	    // attention a sort XXX 
	    Dist1 = sort(realsqrt((P_ex1(1)-x(ptP)).^2+(P_ex1(2)-y(ptP)).^2));
	    Dist2 = sort(realsqrt((P_ex2(1)-x(ptP)).^2+(P_ex2(2)-y(ptP)).^2));
	    
	    Direction = [];
	    if (Dist1(3)-Dist1(2) > 0) && (abs(Dist1(2)-Dist1(1))<Epsilon)
	      Direction = 1;
	    elseif (Dist2(3)-Dist2(2) > 0) && (abs(Dist2(2)-Dist2(1))<Epsilon)
	      Direction = 2;
	    end
	    if ~isempty(Direction)
	      bContinue = %t;
	      while bContinue
		if Direction == 1
		  P_ex = V(ptVFinite(jndex),:) + Lambda*[dy,-dx];
		elseif Direction == 2
		  P_ex = V(ptVFinite(jndex),:) + Lambda*[-dy,dx];
		end
		if ~(P_ex(1)<= RightEdge && P_ex(1)>= LeftEdge && P_ex(2)<= TopEdge && P_ex(2)>= BtmEdge)
		  bContinue = %f;
		  V = [V;P_ex]; //#ok<AGROW>
		  C{ptP(Counter1)} = [C{ptP(Counter1)},size(V,1)];
		  C{ptP(Counter2)} = [C{ptP(Counter2)},size(V,1)];
		  if toggleplot
		    plot2d(V($,1),V($,2),style=-2,axesflag=0,rect=rectp);
		  end
		else
		  Lambda = 2*Lambda;
		end
	      end
	    end
	  end
	  
	end // End of Counter 2
      end // End of Counter 1
      
      if toggleplot
	// hold off
      end
    end

    //// Sort angle, all vertices in anti-clockwise direction

    for index = 1 : LenC
      ptVFinite = setdiff(C{index},[1]);
      if toggleplot
	xset('window',3);
	plot2d([],[],rect=rectp);
	plot2d(Orphan_x,Orphan_y,style=-2,axesflag=0,rect=rectp);
	plot2d(x(index),y(index),style=-3,axesflag=0,rect=rectp);
	plot2d(vx,vy,style=ones(1,size(vx,2)),axesflag=0,rect=rectp);
	plot2d(V(ptVFinite,1),V(ptVFinite,2),style=-4,axesflag=0,rect=rectp);
      end
      Centre = mean(V(ptVFinite,:),1);
      // Angle = atan2(V(ptVFinite,2)-Centre(2),V(ptVFinite,1)-Centre(1));
      Angle = atan(V(ptVFinite,2)-Centre(2),V(ptVFinite,1)-Centre(1));
      [Angle,jndex] = sort(Angle,'g','i'); //#ok<ASGLU>
      C{index} = ptVFinite(jndex);   // Sorted Voronoi vertices
      if toggleplot
	// hold off
      end
    end
  endfunction 
  
  if any(size(x) ~= size(y)) then 
    error('x,y dimensions are not consistent')
  end
  x=x(:); y=y(:);
  clearglobal x_VoronoiGlobal y_VoronoiGlobal
  clear LeftEdge RightEdge BtmEdge TopEdge
  global x_VoronoiGlobal y_VoronoiGlobal
  global LeftEdge RightEdge BtmEdge TopEdge
  if toggleplot
    //plottools('on')
    //propertyeditor('off')
    //figure(1);figure(2);figure(3);figure(4);figure(5);
    //set([1:5],'WindowStyle','docked');
  end
  // Check input
  LeftEdge = rect(1);
  RightEdge = rect(2);
  BtmEdge = rect(3);
  TopEdge = rect(4);
  x_VoronoiGlobal = x;
  y_VoronoiGlobal = y;
  // Voronoi tesselation using voronoin()
  [V, C] = voronoin([x,y]);
  [vx,vy] = voronoi(x,y);
  // Force all voronoi vertices to be finite
  [V,C] = BoundVoronoin_UnitSquare(V, C,toggleplot=toggleplot);

  // ---------------------------------------------------------------------
  // Find boundary intersection points and each voronoi cell areas
  // NormV = realsqrt(sum(V.^2,2));
  NormV = V(:,1) <= RightEdge & V(:,1) >= LeftEdge ...
	  & V(:,2) <= TopEdge & V(:,2) >= BtmEdge;
  LenC  = max(size(C)); // length(C) in octave 
  CellArea = zeros(LenC,1);
  for index = 1 : LenC
    ptV = C{index};
    VSet = V(ptV,:);
    NormVSet = NormV(ptV);
    // =================================================================
    if toggleplot
      rect=[LeftEdge,BtmEdge,RightEdge,TopEdge];
      of=1;
      rectp=[LeftEdge-of,BtmEdge-of,RightEdge+of,TopEdge+of];
      xset('window',4);
      plot2d([],[],rect=rectp);
      plot2d(x,y,style=-2,axesflag=0,rect=rectp);
      plot2d(x(index),y(index),style=-3,axesflag=0,rect=rectp);
      plot2d(vx,vy,style=ones(1,size(vx,2)),axesflag=0,rect=rectp);
      for jndex = 1 : size(ptV,2);
	if ptV(jndex) ~= 1
	  plot2d(V(ptV(jndex),1),V(ptV(jndex),2),style=-4,axesflag=0,rect=rectp);	end
      end
    end
    // =================================================================
    // Identify the vertice furthest from the origin to be the first scanning vertice
    // if max(NormVSet) < 1
    if all(NormVSet)
      // All points inside unit square, calculate area
      CellArea(index) = polyarea(V(ptV,1),V(ptV,2));
    else
      // Find points outside unit Square
      // bOutside = NormVSet>1;
      bOutside = ~NormVSet;
      bTestStartPoint = bOutside;
      // bTestStartPoint = or(bTestStartPoint,circshift(bTestStartPoint,-1));
      bTestStartPoint = or(bTestStartPoint,bTestStartPoint([2:$,1])) 
      ptComplimentaryPoint = mod(find(bTestStartPoint),size(VSet,1))+1;
      // Find intersection points (if exist)
      TestPoint1 = VSet(bTestStartPoint,:);
      TestPoint2 = VSet(ptComplimentaryPoint,:);
      if size(VSet,1) == 2
	TestPoint1(2,:) = [];
	TestPoint2(2,:) = [];
      end
      // =============================================================
      if toggleplot
	plot2d([TestPoint1(:,1),TestPoint2(:,1)]',[TestPoint1(:,2),TestPoint2(:,2)]',rect=rectp,axesflag=0)
      end
      // =============================================================
      Norm1 = NormVSet(bTestStartPoint);
      Norm2 = NormVSet(ptComplimentaryPoint);
      if size(VSet,1) == 2
	Norm1(2,:) = [];
	Norm2(2,:) = [];
      end
      // bOneInOneOut= xor(~Norm1, ~Norm2);
      bOneInOneOut= ~Norm1 <> ~Norm2 // gives xor 
      count = 0; xc1 = []; yc1 = [];
      for hndex = 1:size(bOneInOneOut,1)
	if bOneInOneOut(hndex)
	  count = count + 1;
	  // [xc1, yc1]  = BoundaryIntersect_UnitSquare(TestPoint1(bOneInOneOut,:),TestPoint2(bOneInOneOut,:),1);
	  [xc1(count), yc1(count)]  = ...
	      BoundaryIntersect_UnitSquare(TestPoint1(hndex,:),TestPoint2(hndex,:),1);
	end
      end
      xc1 = reshape(xc1,numel(xc1),1);
      yc1 = reshape(yc1,numel(yc1),1);
      // bTwoOut= and(Norm1>1,Norm2>1);
      bTwoOut     = and(~Norm1, ~Norm2);
      count = 0; xc2 = []; yc2 = [];
      for kndex = 1:size(bTwoOut,1)
	if bTwoOut(kndex)
	  count = count + 2;
	  [xc2(count-1:count), yc2(count-1:count)]  = ...
	      BoundaryIntersect_UnitSquare(TestPoint1(kndex,:),TestPoint2(kndex,:),2);
	end
      end
      xc2 = reshape(xc2,numel(xc2),1);
      yc2 = reshape(yc2,numel(yc2),1);
      xc2(isnan(xc2)) = [];
      yc2(isnan(yc2)) = [];
      // =============================================================
      if toggleplot
	if ~isempty(xc1)
	  plot2d(xc1,yc1,	rect=rectp,axesflag=0)
	end
	if ~isempty(xc2)
	  plot2d(xc2,yc2,	rect=rectp,axesflag=0)
	end
      end
      // =============================================================
      // Calculate Voronoi cell area
      CellArea(index) = CalcArea_UnitSquare(VSet(NormVSet,:), [xc1,yc1], [xc2,yc2], index);
    end
    // =================================================================
    if toggleplot
      // hold off
      // pause(0.8)
    end
    // =================================================================
  end
  TotalArea = sum(CellArea);
  Err = TotalArea - abs((RightEdge - LeftEdge)*(TopEdge-BtmEdge));
  fprintf('Total Area = %.3e\n',TotalArea);
  fprintf('Mean absolute Error = %.3e\n',mean(abs(Err)))
  fprintf('Largest absolute Error = %.3e\n',max(abs(Err)))
  clearglobal x_VoronoiGlobal y_VoronoiGlobal
  clear LeftEdge RightEdge BtmEdge TopEdge
endfunction 
