// Toolbar 
// --------------------------------- 


function set_toolbar_horizontal(_b,args)
  args(1).set_orientation[GTK.ORIENTATION_HORIZONTAL]
endfunction

function set_toolbar_vertical(_b,args)
  args(1).set_orientation[GTK.ORIENTATION_VERTICAL]
endfunction
function set_toolbar_icons(_b,args) 
  args(1).set_style[TOOLBAR_ICONS]
endfunction
function set_toolbar_text(_b,args)
  args(1).set_style[TOOLBAR_TEXT]
endfunction
function set_toolbar_both(_b,args)
  args(1).set_style[TOOLBAR_BOTH]
endfunction
function set_toolbar_small_space(_b,args)
  args(1).set_space_size[5]
endfunction
function set_toolbar_big_space(_b,args)
  args(1).set_space_size[10]
endfunction
function set_toolbar_enable(_b,args)
  args(1).set_tooltips[%t]
endfunction
function set_toolbar_disable(_b,args)
  args(1).set_tooltips[%f]
endfunction

function [toolbar]=make_toolbar(win)
  toolbar = gtktoolbar_new();
  win.realize[];
  // FIXME: crashes if the window is not realized 
  style = win.style.get_bg_gc[GTK.STATE_NORMAL]
  //[1] - first possibility 
  //pixmap=gtk_pixmap_new_from_xpm(win, "test.xpm")
  //[2] - second possibility
  gdkwin = win.window;
  [pix, mask] = gdk_pixmap_create_from_xpm(gdkwin, none_create(),getenv('NSP')+"/demos/gtk2/libbase/test.xpm")
  //[3] - third possibility
  //cmap =  win.get_colormap[];
  //[pix,mask]=gdk_pixmap_colormap_create_from_xpm([],cmap,[],"test.xpm")
  // toolbar.append_item
  //  GtkPixmap(pix, mask),
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Horizontal", "Horizontal toolbar layout","Horizontal toolbar layout",pixmap,  set_toolbar_horizontal]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Vertical", "Vertical toolbar layout", "Vertical toolbar layout", pixmap, set_toolbar_vertical]
  toolbar.append_space[]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Icons", "Only show toolbar icons","Only Icons",pixmap,set_toolbar_icons]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Text", "Only show toolbar text","Only Text", pixmap,  set_toolbar_text]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Both", "Show toolbar icons and text","Both Text and Icons",pixmap,set_toolbar_both ]
  toolbar.append_space[]
  entry = gtk_entry_new()
  entry.show[]
  toolbar.append_widget[entry, "", ""]
  toolbar.append_space[]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Small", "Use small spaces","Small spaces",pixmap,set_toolbar_small_space]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Big", "Use big spaces","Big spaces",pixmap,set_toolbar_big_space]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Enable", "Enable tooltips","Enable tooltips",pixmap,set_toolbar_enable]
  pixmap = gtk_pixmap_new(pix,mask);
  toolbar.append_item["Disable", "Disable tooltips","Disable tooltips",pixmap,set_toolbar_disable]
endfunction 

function demo_toolbar()
  win = gtkwindow_new()
  win.set_title["Toolbar test"];
  //win.set_policy[%f, %t, %t];
  win.connect["delete_event", hide];
  toolbar = make_toolbar(win)
  win.add[toolbar]
  toolbar.show[]
  win.show[]
endfunction 
