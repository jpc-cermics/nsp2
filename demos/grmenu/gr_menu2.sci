// test program 
// a new scicos editor 
// this works with objects of src/gobjects 

function w=create_midle_menu (win,xc,yc)
// midle button menu construction 
  global('GF');
  s_win='win'+string(win);
  [is_sel,ov]= GF(s_win).get_selection[];
  if ~is_sel then 
    // no selection try to find one 
    rep=GF(s_win).select_and_hilite[[xc,yc]];
    if rep then 
      GF(s_win).draw[];
      [is_sel,ov]= GF(s_win).get_selection[];
    end
  end

  menu = gtkmenu_new ();
  // title of the sub menu 
  if is_sel then 
    name = type(ov,'string')
    menuitem = gtkmenuitem_new(label=name+ ' Menu' );
    menu.append[menuitem]
    menuitem.show[];
    menuitem = gtkseparatormenuitem_new()
    menu.append[menuitem]
    menuitem.show[];
  else
    name = 'void';
  end
  
  //
  menuitem = gtkimagemenuitem_new(stock_id="gtk-delete");
  if ~is_sel then 
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",midle_menuitem_response,list(1,win)];
  menu.append[menuitem]
  menuitem.show[];
  //
  if name == 'Link' then 
    menuitem = gtkmenuitem_new(label="add control point");
    menuitem.connect["activate",midle_menuitem_response,list(2,xc,yc,win)];
    menu.append[menuitem]
    menuitem.show[];
    //
    menuitem = gtkmenuitem_new(label="remove control point");
    menuitem.connect["activate",midle_menuitem_response,list(3,xc,yc,win)];
    menu.append[menuitem]
    menuitem.show[];
  end
  //-- copy 
  menuitem = gtkimagemenuitem_new(stock_id="gtk-copy");
  if ~is_sel then 
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",midle_menuitem_response,list(4,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  // sensitive or not 
  // menuitem.set_sensitive[%f];
  //-- paste 
  menuitem = gtkimagemenuitem_new(stock_id="gtk-paste");
  if ~( GF.iskey['clipboard'] && length(GF('clipboard')) ==  1) then 
    // nothing to paste 
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",midle_menuitem_response,list(5,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  //  
  w=menu;
endfunction 

function midle_menuitem_response(w,args) 
// midle button menu activation 
// the midle menu should be a by object menu 
  global('GF');
  printf("Menu item [%d] activated for win=%d\n",args(1),args($));
  win='win'+string(args($))
  select args(1) 
   case 1 then  GF(win).delete_hilited[] ; GF(win).draw[];
   case 2 then  
    GF(win).hilite_near_pt[[args(2),args(3)]];
    GF(win).select_link_and_add_control[[args(2),args(3)]];
   case 3 then  
    GF(win).hilite_near_pt[[args(2),args(3)]];
    GF(win).select_link_and_remove_control[[args(2),args(3)]];
   case 4 then 
    [test,obj]= GF(win).get_selection_copy[];
    if test then GF('clipboard') = list(obj); 
    else x_message('No selection');end 
   case 5 then 
    // attention ici il faut que l'on insere des 
    // copies car on veut pouvoir inserer plusieurs 
    // fois 
    if GF.iskey['clipboard'] then 
      if length(GF('clipboard'))<> 0 then
	GF('clipboard')(1).set_pos[[args(2),args(3)]];
	GF(win).insert[GF('clipboard')(1)];
      else 
	x_message('Clipboard is empty');end 
    end
    GF('clipboard') = list();
  end
  GF(win).draw[]
endfunction

function menu=create_right_menu (win)
// midle button menu construction 
// midle button is for the current selected object 
// if more than one object is selected this menu is deactivated.
  tearoff=%t;
  menu = gtkmenu_new ();
  if tearoff then 
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  // new
  tags = ['new link';'new block';'new_connector']
  for i=1:size(tags,'*')
    // BUG: mnemonic and label are not active ?
    // menuitem = gtkimagemenuitem_new(stock_id="gtk-new",mnemonic=tags(i),label=tags(i));
    menuitem = gtkmenuitem_new(label=tags(i));
    menuitem.connect["activate",menuitem_response,list(i,win)];
    menu.append[menuitem]
    menuitem.show[];
  end
  // separator 
  menuitem = gtkseparatormenuitem_new()
  menu.append[menuitem]
  menuitem.show[];
  // save to file 
  menuitem = gtkimagemenuitem_new(stock_id="gtk-save-as");
  menuitem.connect["activate",menuitem_response,list(4,win)];
  menu.append[menuitem]
  menuitem.show[];
  // load file 
  menuitem = gtkimagemenuitem_new(stock_id="gtk-open");
  menuitem.connect["activate",menuitem_response,list(5,win)];
  menu.append[menuitem]
  menuitem.show[];
endfunction 


function menuitem_response(w,args) 
// midle button menu activation 
  global('GF');
  printf("Menu item [%d] activated \n",args(1));
  win='win'+string(args($));
  select args(1) 
   case 1 then  GF(win).new_link[];
   case 2 then  
    // print(GF(win));
    GF(win).new_block[];
    // print(GF(win));
   case 3 then  GF(win).new_connector[] ;
   case 4 then  
    fname = xgetfile();
    if fname <> "" then 
      save(fname,diagram=GF(win));
    end
   case 5 then 
    fname = xgetfile();
    if fname <> "" then 
      load(fname);
      if exists('diagram') then 
	diagram.attach_to_window[args($)];
	GF(win)=diagram;
      end
    end
   case 6 then 
  end
  GF(win).draw[]
endfunction

function w=create_menu (depth, length, tearoff)
// provisoire 
  if depth < 1 ;  w=[] ;return; end 
  menu = gtkmenu_new ();
  if tearoff then 
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  menuitem = gtkimagemenuitem_new(stock_id="gtk-open");
  menu.append[menuitem]
  menuitem.show[];
    
  for i = 1:length 
    buf = sprintf("radio menu item %2d - %d", depth,i);
    if i==1 then 
      menuitem = gtkradiomenuitem_new(label=buf);
      group = menuitem;
    else
      menuitem = gtkradiomenuitem_new(group=group,label=buf);
    end
    // callback 
    menuitem.connect["activate",menuitem_response,list(buf)];
    menu.append[menuitem]
    menuitem.show[];
    if i == 3 then menuitem.set_sensitive[%f]; end 
    if i == 4 then menuitem.set_inconsistent[ %f]; end 
    if i < 3 then 
      if depth > 1 then 
	menuitem.set_submenu[create_menu(depth - 1,5,%t)];
      end
    end
  end
  w=menu;
endfunction 


function my_eventhandler(win,x,y,ibut)
  global('gr_objects');
  global('GF');
  global('count');
  winid= 'win'+string(win);
  if ~GF.iskey[winid];then
    GF(winid)=%types.GFrame.new[[0,0,100,100],[0,0,100,100],win];
    GF(winid).draw[];// this will fix the initial scale 
  end
  if count == 1 then 
    printf("event handler aborted =%d\n",count)
    return
  end
  count = 1;
  if ibut == -100 then 
    printf('window killed ')
  elseif ibut==-1 then 
    [xc,yc]=xchange(x,y,'i2f')
    xinfo('Mouse position is ('+string(xc)+','+string(yc)+')')
  elseif ibut==0 then 
    // left press 
    [xc,yc]=xchange(x,y,'i2f')
    GF(winid).select_and_move[[xc,yc]];
  elseif ibut==1 then 
    // midle press 
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_midle_menu (win,xc,yc)
    popup_m.popup[button=1,activate_time=0]; //event.time]; 
  elseif ibut==2 then 
    // right press
    [xc,yc]=xchange(x,y,'i2f')
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_right_menu (win)
    popup_m.popup[button=2,activate_time=0]; //event.time]; 
    //popup_menu=create_menu(2,8,%f); 
    //popup_menu.popup[button=3,activate_time=0]; //event.time]; 
  elseif ibut==3 then 
    // a double click 
    x_message('double click');
    getcolor();
  elseif ibut==100 
    x_message('100');
    x_message('click on d: 99');
    //gr_delete();
    //gr_draw(win);
  elseif ibut==99
    x_message('click on c: 99');
    //gr_copy();
    //gr_draw(win);
  else
    xinfo('Mouse action: ['+string(ibut)+']');
    // test a popup menu 
    //
  end
  count=0;
endfunction

// plusieurs fonctions de test pour draw_vanne 

function draw_vanne(rect)
// test function for block drawing 
  orig=[rect(1),rect(2)-rect(4)];
  sz=[rect(3),rect(4)];
  // take car that for Opengl 
  // polygone are to be convex when filled 
  //xfpolys(orig(1)+[0;5;7;3;5;10;10;0;0]*sz(1)/10,...
  //      orig(2)+[4;2;7;7;2;0;4;0;4]*sz(2)/10,15);
  // thus we draw 3 polygons.
  xfpolys(orig(1)+[5,5,5;10,7,0;10,3,0]*sz(1)/10,...
	  orig(2)+[2,2,2;4,7,0;0,7,4]*sz(2)/10,[15,15,15]);
  
  xfarcs([orig(1)+3*sz(1)/10;orig(2)+sz(2);4*sz(1)/10;6*sz(2)/10;0;180*64],...
	 15)
  xarcs([orig(1)+3*sz(1)/10;orig(2)+sz(2);4*sz(1)/10;6*sz(2)/10;0;180*64],...
	1);
  xset('font',2,6);
  xstringb(orig(1),orig(2),'String',sz(1),sz(2));
endfunction;



function draw_plot2d(rect)
// test function for block drawing 
  print(rect)
  [wrect,frect,vv,arect]=xgetech();
  wx= (rect(1)-frect(1))/(frect(3)-frect(1));
  wy= abs(rect(2)-frect(4))/(frect(4)-frect(2));
  ww= rect(3)/(frect(3)-frect(1));
  wh= rect(4)/(frect(4)-frect(2));
  xsetech(wrect=[wx,wy,ww,wh],arect=ones(1,4)/8);
  plot3d();
  xsetech(wrect=wrect,frect=frect,arect=arect);
endfunction;


function draw_scope(rect)
// test function for block drawing 
  orig=[rect(1),rect(2)-rect(4)];
  sz=[rect(3),rect(4)];
  //B=CLOCK_f('define');
  B=SCOPE_f('define');
  str=B.graphics.gr_i(1);
  execstr(str);
endfunction;

function y=scs_color(i);y=i;endfunction

function F= diagram()
// build a diagram non interactively 
  F=%types.GFrame.new[[0,0,100,100],[0,0,100,100],-1];
  B1=%types.Block.new[[10,80,10,10],color=6,background=7];
  F.insert[B1];
  B2=%types.Block.new[[25,80,10,10],color=6,background=7];
  F.insert[B2];
  B3=%types.Block.new[[40,80,10,10],color=6,background=7];
  F.insert[B3];
  // we fix the lock points 
  B4=%types.Block.new[[40,80,10,10],color=6,background=7];
  // ior(dir,ishift(type,4))
  // with :
  // dir = LD_NORTH=0, LD_SOUTH=1, LD_EAST=2, LD_WEST=3, LD_ANY=4;
  // type = L_IN=0 ,L_OUT=1 ,L_EVIN=2 ,L_EVOUT=3 , L_SQP=4 , L_SQM=5 ;
  B4.set_locks_pos[[0.80;0.0;ior(4,ishift(2,4))]]
  F.insert[B4];
  
  L=%types.Link.new[[0,10;0,10]];
  L.connect[0,B1,1];  L.connect[1,B2,1];
  F.insert[L];
  L=%types.Link.new[[0,10;0,10]];
  L.connect[0,B2,2];  L.connect[1,B3,2];
  F.insert[L];
endfunction;
  
global('GF');
GF=hcreate(6);

xinit(name='My diagram',dim=[1000,1000],popup_dim=[600,400])
xset('recording',0)
xsetech(arect=[0,0,0,0]);

seteventhandler('my_eventhandler');

xinit(name='My second diagram opengl=%t',opengl=%t,dim=[1000,1000],popup_dim=[600,400])
xset('recording',0)
xsetech(arect=[0,0,0,0]);
seteventhandler('my_eventhandler');




