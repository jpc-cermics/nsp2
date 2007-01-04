// Toolbar 
// --------------------------------- 


function set_toolbar_horizontal(_b,args)
  args(1).set_orientation[GTK.ORIENTATION_HORIZONTAL]
endfunction
function set_toolbar_vertical(_b,args)
  args(1).set_orientation[GTK.ORIENTATION_VERTICAL]
endfunction
function set_toolbar_icons(_b,args) 
  args(1).set_style[GTK.TOOLBAR_ICONS]
endfunction
function set_toolbar_text(_b,args)
  args(1).set_style[GTK.TOOLBAR_TEXT]
endfunction
function set_toolbar_both(_b,args)
  args(1).set_style[GTK.TOOLBAR_BOTH]
endfunction
function set_toolbar_small_space(_b,args)
  args(1).insert_space[5]
endfunction
function set_toolbar_big_space(_b,args)
  args(1).insert_space[10]
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
  gdkwin = win.window;
  [pix, mask] = gdk_pixmap_create_from_xpm(gdkwin, none_create(),getenv('NSP')+"/demos/gtk2/libbase/test.xpm")
  pixmap = gtkimage_new("pixmap",pix,mask);
 
  toolbar.append_item[text="Horizontal",tooltip_text= "Horizontal toolbar layout",...
		      tooltip_private_text="Horizontal toolbar layout",...
		      icon=pixmap,callback=set_toolbar_horizontal,params=list(toolbar)]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Vertical",tooltip_text= "Vertical toolbar layout",...
		      tooltip_private_text= "Vertical toolbar layout",icon= pixmap,callback= set_toolbar_vertical,params=list(toolbar)]
  toolbar.append_space[]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Icons",tooltip_text= "Only show toolbar icons",...
		      tooltip_private_text="Only Icons",icon=pixmap,callback=set_toolbar_icons,params=list(toolbar)]
    pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Text",tooltip_text= "Only show toolbar text",..
		      tooltip_private_text="Only Text",icon= pixmap,callback=  set_toolbar_text,params=list(toolbar)]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Both",tooltip_text= "Show toolbar icons and text",...
		      tooltip_private_text="Both Text and Icons",icon=pixmap,callback=set_toolbar_both,params=list(toolbar) ]
  toolbar.append_space[]
  entry = gtkentry_new()
  entry.show[]
  toolbar.append_widget[entry, "", ""]
  toolbar.append_space[]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Small",tooltip_text= "Use small spaces",tooltip_private_text="Small spaces",...
		      icon=pixmap,callback=set_toolbar_small_space,params=list(toolbar)]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Big",tooltip_text= "Use big spaces",tooltip_private_text="Big spaces",...
		      icon=pixmap,callback=set_toolbar_big_space,params=list(toolbar)]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Enable",tooltip_text= "Enable tooltips",tooltip_private_text="Enable tooltips",...
		      icon=pixmap,callback=set_toolbar_enable,params=list(toolbar)]
  pixmap = gtkimage_new("pixmap",pix,mask);
  toolbar.append_item[text="Disable",tooltip_text= "Disable tooltips",tooltip_private_text="Disable tooltips",...
		      icon=pixmap,callback=set_toolbar_disable,params=list(toolbar)]
endfunction 

function demo_toolbar()
  win = gtkwindow_new()
  win.set_title["Toolbar test"];
  //win.set_policy[%f, %t, %t];
  win.connect["delete_event", demo_delete];
  toolbar = make_toolbar(win)
  win.add[toolbar]
  toolbar.show[]
  win.show[]
endfunction 
