// test program for a scicos-like editor
// this works with objects of src/gobjects

function w=create_object_menu (win,xc,yc)
// midle button menu construction
// version where selection is a list

  global('GF');
  s_win='win'+string(win);

  // check if pointer is over an object

  [k,hilited] = GF(winid).check_pointer[[xc,yc]];
  if k == %f then
    w=create_right_menu (win,xc,yc)
    return
  end

  // if pointer is over an object
  // we have to check if object is hilited
  // if it is then ok
  // if it is not then we have to select it unselect
  // others then continue

  if ~hilited  then
    rep=GF(s_win).select_and_hilite[[xc,yc]];
    GF(s_win).draw[];
  end

  // now selection should not be empty

  L= GF(s_win).get_selection_as_gframe[];

  menu = gtkmenu_new ();
  // title of the sub menu

  if L.nobjs[]<>0 then
    if L.nobjs[]==1 then
      [ok, Lo] = GF(s_win).get_selection[];
      name = type(Lo,'string')
    else
      name = 'Agregation '
    end
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
  if L.nobjs[]==0 then
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",objet_menuitem_response,list(1,win)];
  menu.append[menuitem]
  menuitem.show[];
  //
  if name == 'Link' then
    menuitem = gtkmenuitem_new(label="add control point");
    menuitem.connect["activate",objet_menuitem_response,list(2,xc,yc,win)];
    menu.append[menuitem]
    menuitem.show[];
    //
    menuitem = gtkmenuitem_new(label="remove control point");
    menuitem.connect["activate",objet_menuitem_response,list(3,xc,yc,win)];
    menu.append[menuitem]
    menuitem.show[];
  end
  if name == 'GridBlock' then
    menuitem = gtkmenuitem_new(label="edit super block");
    menuitem.connect["activate",objet_menuitem_response,list(6,xc,yc,win)];
    menu.append[menuitem]
    menuitem.show[];
  end

  //-- copy
  menuitem = gtkimagemenuitem_new(stock_id="gtk-copy");
  if  L.nobjs[]==0 then
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",objet_menuitem_response,list(4,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  // sensitive or not
  // menuitem.set_sensitive[%f];
  //-- paste
  if %f then
    menuitem = gtkimagemenuitem_new(stock_id="gtk-paste");
    if ~( GF.iskey['clipboard'] && length(GF('clipboard')) ==  1) then
      // nothing to paste
      menuitem.set_sensitive[%f];
    end
    menuitem.connect["activate",objet_menuitem_response,list(5,xc,yc,win)];
    menu.append[menuitem]
    menuitem.show[];
  end
  //
  w=menu;
endfunction

function objet_menuitem_response(w,args)
// This function is the activated handler
// when an object menu is activated
//
  global('GF');
  // printf("Menu item [%d] activated for win=%d\n",args(1),args($));
  win='win'+string(args($))
  select args(1)
   case 1 then
    // delete hilited objects
    GF(win).delete_hilited[] ;
    GF(win).draw[];
   case 2 then
    // add a control to a link
    GF(win).hilite_near_pt[[args(2),args(3)]];
    GF(win).select_link_and_add_control[[args(2),args(3)]];
   case 3 then
    // remove a link control
    GF(win).hilite_near_pt[[args(2),args(3)]];
    GF(win).select_link_and_remove_control[[args(2),args(3)]];
   case 4 then
    // copy selection into the clipboard
    L= GF(win).get_selection_as_gframe[];
    // pause
    if ~isempty(L) then GF('clipboard') = list(L.copy[]);
    else x_message('No selection');end
   case 5 then
    // paste selection
    if GF.iskey['clipboard'] then
      if length(GF('clipboard'))<> 0 then
	L= GF('clipboard')(1);
	GF(win).insert_gframe[L,[args(2),args(3)]];
	//x_message('Paste multiple');
      else
	x_message('Clipboard is empty');end
    end
    // GF('clipboard') = list();
   case 6 then
    // edit a super block
    // printf("enter super block edit\n");
    [test,obj]= GF(win).get_selection[];
    xinit(name='Super Block',dim=[1000,1000],popup_dim=[600,400])
    newwin= xget('window');
    xset('recording',0)
    xsetech(arect=[0,0,0,0]);
    newgf=obj.edit[];
    winid= 'win'+string(newwin);
    GF(winid)= newgf;
    GF(winid).attach_to_window[newwin];
    GF(winid).draw[];
    seteventhandler('my_eventhandler');
  end
  GF(win).draw[]
endfunction

function menu=create_right_menu (win,xc,yc)
// right button menu construction
  global('GF');
  s_win='win'+string(win);

  tearoff=%t;
  menu = gtkmenu_new ();
  if tearoff then
    menuitem = gtktearoffmenuitem_new ();
    menu.append[  menuitem]
    menuitem.show[];
  end
  //--  new
  tags = ['link';'block';'connector';'empty super block';'selection to super block']
  for i=1:size(tags,'*')
    // BUG: mnemonic and label are not active ?
    // menuitem = gtkimagemenuitem_new(stock_id="gtk-new",mnemonic=tags(i),label=tags(i));
    menuitem = gtkmenuitem_new(label=tags(i));
    menuitem.connect["activate",menuitem_response,list(i,win)];
    menu.append[menuitem]
    menuitem.show[];
  end
  // check if there's an hilited object
  // if not unset the last menuitem
  [test,obj]= GF(s_win).get_selection[];
  if ~test  then
    menuitem.set_sensitive[%f];
  end
  // separator
  menuitem = gtkseparatormenuitem_new()
  menu.append[menuitem]
  menuitem.show[];
  //-- copy
  menuitem = gtkimagemenuitem_new(stock_id="gtk-copy");
  [ok, Lo] = GF(s_win).get_selection[];
  if  ~ok then
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",menuitem_response,list(9,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  //-- paste
  menuitem = gtkimagemenuitem_new(stock_id="gtk-paste");
  if ~( GF.iskey['clipboard'] && length(GF('clipboard')) ==  1) then
    // nothing to paste
    menuitem.set_sensitive[%f];
  end
  menuitem.connect["activate",menuitem_response,list(6,xc,yc,win)];
  menu.append[menuitem]
  menuitem.show[];
  // separator
  menuitem = gtkseparatormenuitem_new()
  menu.append[menuitem]
  menuitem.show[];
  //--  save to file
  menuitem = gtkimagemenuitem_new(stock_id="gtk-save-as");
  menuitem.connect["activate",menuitem_response,list(7,win)];
  menu.append[menuitem]
  menuitem.show[];
  //--  load file
  menuitem = gtkimagemenuitem_new(stock_id="gtk-open");
  menuitem.connect["activate",menuitem_response,list(8,win)];
  menu.append[menuitem]
  menuitem.show[];
endfunction


function menuitem_response(w,args)
// right button menu activation
  global('GF');
  //printf("Menu item [%d] activated \n",args(1));
  win='win'+string(args($));
  select args(1)
   case 1 then  GF(win).new_link[];
   case 2 then  GF(win).new_block[];
   case 3 then  GF(win).new_connector[] ;
   case 4 then  GF(win).new_gridblock[] ;
   case 5 then  GF(win).new_gridblock_from_selection[] ;
   case 6 then
    // paste selection
    if GF.iskey['clipboard'] then
      if length(GF('clipboard'))<> 0 then
	L= GF('clipboard')(1);
	GF(win).insert_gframe[L,[args(2),args(3)]];
      else
	x_message('Clipboard is empty');end
    end
    //GF('clipboard') = list();
   case 7 then
    fname = xgetfile();
    if fname <> "" then
      save(fname,diagram=GF(win));
    end
   case 8 then
    fname = xgetfile();
    if fname <> "" then
      load(fname);
      if exists('diagram') then
	diagram.attach_to_window[args($)];
	GF(win)=diagram;
      end
    end
   case 9 then
    // copy selection into the clipboard
    L= GF(win).get_selection_as_gframe[];
    // pause
    if ~isempty(L) then GF('clipboard') = list(L.copy[]);
    else x_message('No selection');end
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


function my_eventhandler(win,x,y,ibut,imask)
  global('gr_objects');
  global('GF');
  global('count');
  // keys are described in src/gtk3/codegen/keysyms.sce
  // mask in gdk-types.defs are accessible in GDK hash table.
  // shift-mask GDK.SHIFT_MASK
  // control-mask GDK_CONTROL_MASK
  // if ibut<>-1 then printf("ibutton = %d, mask = %d\n",ibut,imask);end
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
    if iand(imask,GDK.SHIFT_MASK)
      // add to selection and move the whole stuff
      [xc,yc]=xchange(x,y,'i2f')
      GF(winid).select_and_move_list[[xc,yc]];
    elseif iand(imask,GDK.CONTROL_MASK)
	// toggle the selection
	// printf("control -press \n");
	[xc,yc]=xchange(x,y,'i2f')
	GF(winid).select_and_toggle_hilite[[xc,yc]];
	GF(winid).draw[];
    else
      // select the new, unhilite others and move selected
      [xc,yc]=xchange(x,y,'i2f')
      GF(winid).select_and_move[[xc,yc]];
    end
  elseif ibut==1 then
    // midle press
  elseif ibut==2 then
    // right press
    [xc,yc]=xchange(x,y,'i2f')
    popup_m=create_object_menu (win)
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
  elseif ibut == 65288 || ibut == 65535
    // Delete and supr keys -> delete hilited objects
    GF(winid).delete_hilited[] ;
    GF(winid).draw[];
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

function draw_plot3d(rect)
// test function for block drawing
  print(rect)
  [wrect,frect,vv,arect]=xgetech();
  wx= (rect(1)-frect(1))/(frect(3)-frect(1));
  wy= abs(rect(2)-frect(4))/(frect(4)-frect(2));
  ww= rect(3)/(frect(3)-frect(1));
  wh= rect(4)/(frect(4)-frect(2));
  xsetech(wrect=[wx,wy,ww,wh],arect=ones_new(1,4)/8);
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

gtk_logo = getenv('NSP')+'/demos3/gtk3/libbase/demo_images/gtk-logo-rgb.gif';
global gtk_logo_pixbuf;
gtk_logo_pixbuf = gdk_pixbuf_new_from_file(gtk_logo);

function draw_gtk_logo(rect)
// test function for block drawing
  global gtk_logo_pixbuf;
  xdraw_pixbuf(0,gtk_logo_pixbuf,0,0,rect(1),rect(2),rect(3),rect(4));
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
GF=hash_create(6);

xinit(name='My diagram',dim=[1000,1000],popup_dim=[600,400])
xset('recording',0)
xsetech(arect=[0,0,0,0]);

if %f then
  seteventhandler('my_eventhandler');
  xinit(name='My second diagram opengl=%t',opengl=%t,dim=[1000,1000],popup_dim=[600,400])
  xset('recording',0)
  xsetech(arect=[0,0,0,0]);
  seteventhandler('my_eventhandler');
else
  F= diagram();
  pause;
  F.attach_to_window[0];
  F.draw[];
end
