// OK  demo_item_factory 

function demo_item_factory () 

  function gtk_ifactory_cb (callback_data, callback_action,widget)
  // printf("ItemFactory: activated ""%s""", widget.path_from_widget[])
  endfunction 
  function dump_accels (callback_data, callback_action,widget)
  //  gtk_accel_map_save_fd (1 /* stdout */]
  endfunction 
  
  //  list( "/File/\\/Test__Escaping/And\\/\n\tWei\\\\rdly",    "",        gtk_ifactory_cb,       none ),
  none=none_create(); 
  apple="" // FIXME: XXXXX to be done 
	   // list( "/_Preferences/Shape/_Image",       "", gtk_ifactory_cb, 0, "<ImageItem>", apple ),
  
  menu_items = list(
    list( "/_File",                    "",         none,                  0, "<Branch>", "" ),
    list( "/File/tearoff1",            "",         gtk_ifactory_cb,       0, "<Tearoff>","" ),
    list( "/File/_New",                "",         gtk_ifactory_cb,       0, "<StockItem>", "gtk-new" ),
    list( "/File/_Open",               "",         gtk_ifactory_cb,       0, "<StockItem>", "gtk-open" ),
    list( "/File/_Save",               "",         gtk_ifactory_cb,       0, "<StockItem>", "gtk-save" ),
    list( "/File/Save _As...",         "<control>A", gtk_ifactory_cb,     0, "<StockItem>", "gtk-save" ),
    list( "/File/_Dump ""_Accels""",   "",        dump_accels,            0, "","" ),
    list( "/File/sep1",                "",               gtk_ifactory_cb, 0, "<Separator>","" ),
    list( "/File/_Quit",               "",               gtk_ifactory_cb, 0, "<StockItem>", "gtk-quit" ),
    list( "/_Preferences",     	     "",        none,                   0, "<Branch>" ,""),
    list( "/_Preferences/_Color",      "",        none,               0, "<Branch>","" ),
    list( "/_Preferences/Color/_Red",  "",        gtk_ifactory_cb, 0, "<RadioItem>","" ),
    list( "/_Preferences/Color/_Green","",        gtk_ifactory_cb, 0, "/Preferences/Color/Red","" ),
    list( "/_Preferences/Color/_Blue", "",        gtk_ifactory_cb, 0, "/Preferences/Color/Red","" ),
    list( "/_Preferences/_Shape",      "",        none,               0, "<Branch>","" ),
    list( "/_Preferences/Shape/_Square",      "", gtk_ifactory_cb, 0, "<RadioItem>","" ),
    list( "/_Preferences/Shape/_Rectangle",   "", gtk_ifactory_cb, 0, "/Preferences/Shape/Square","" ),
    list( "/_Preferences/Shape/_Oval",        "", gtk_ifactory_cb, 0, "/Preferences/Shape/Rectangle","" ),
    list( "/_Preferences/Shape/_Rectangle",   "", gtk_ifactory_cb, 0, "/Preferences/Shape/Square","" ),
    list( "/_Preferences/Shape/_Oval",        "", gtk_ifactory_cb, 0, "/Preferences/Shape/Rectangle","" ),
    list( "/_Preferences/Coffee",                  "", gtk_ifactory_cb, 0, "<CheckItem>","" ),
    list( "/_Preferences/Toast",                   "", gtk_ifactory_cb, 0, "<CheckItem>","" ),
    list( "/_Preferences/Marshmallow Froot Loops", "", gtk_ifactory_cb, 0, "<CheckItem>","" ),
    list( "/_Preferences/Should_NotAppear",          "", none,               0, "<Branch>","" ),
    list( "/Preferences/ShouldNotAppear/SubItem1",   "", gtk_ifactory_cb, 0 ,"",""),
    list( "/Preferences/ShouldNotAppear/SubItem2",   "", gtk_ifactory_cb, 0 ,"",""),
    list( "/_Help",            "",         none,                     0, "<LastBranch>" ,""),
    list( "/Help/_Help",       "",         gtk_ifactory_cb,       0, "<StockItem>", "gtk-help"),
    list( "/Help/_About",      "",         gtk_ifactory_cb,       0,"","" ));
  
  window = gtkwindow_new();
  // window.set_screen[  widget.get_screen[]]
  // window.connect[  "destroy",...];
  // window.connect[  "delete-event",...]
      
  accel_group = gtkaccelgroup_new ()
  item_factory = gtkitemfactory_new ("menu_bar", "<main>", accel_group= accel_group)
  window.set_data[main = item_factory];
  window.add_accel_group[  accel_group]
  window.set_title[  "Item Factory"]
  window.set_border_width[  0]
  item_factory.create_items[ menu_items,list()];

  // preselect /Preferences/Shape/Oval over the other radios

  item= item_factory.get_item["/Preferences/Shape/Oval"];
  item.set_active[%t]; 

  //  preselect /Preferences/Coffee
  item= item_factory.get_item["/Preferences/Coffee"]; 
  item.set_active[%t]; 

  // preselect /Preferences/Marshmallow Froot Loops and set it insensitive
  item= item_factory.get_item["/Preferences/Marshmallow Froot Loops"]; 
  item.set_active[%t]; 
       
  //  Test how tooltips (ugh) work on menu items
  
  tooltips = gtktooltips_new ();
  window.set_data[testgtk_tooltips=tooltips];
  
  tooltips.set_tip[ item_factory.get_item[ "/File/New"],  "Create a new file"];
  tooltips.set_tip[ item_factory.get_item[ "/File/Open"], "Open a file"];
  tooltips.set_tip[ item_factory.get_item[ "/File/Save"], "Safe file"];
  tooltips.set_tip[ item_factory.get_item[ "/Preferences/Color"],  "Modify color"];
  
  box1 = gtkvbox_new(homogeneous=%f,spacing=0);
  window.add[  box1]
  
  box1.pack_start[  item_factory.get_widget["<main>"]];

  label = gtklabel_new(str="Type\n<alt>\nto start");
  label.set_size_request[  200, 200]
  label.set_alignment[  0.5, 0.5]
  box1.pack_start[ label,expand=%t,fill=%t,padding=0]

  separator = gtkhseparator_new ();
  box1.pack_start[ separator,expand=%f,fill=%t,padding=0]

  box2 = gtkvbox_new(homogeneous=%f,spacing=10);
  box2.set_border_width[  10]
  box1.pack_start[ box2,expand=%f,fill=%t,padding=0]

  button = gtkbutton_new(label="close");
  button.connect[ "clicked",button_destroy_win,list(window)];
  box2.pack_start[ button,expand=%t,fill=%t,padding=0]
  button.set_flags[GTK.CAN_DEFAULT];
  button.grab_default[];
  
  item_factory.delete_item["/Preferences/ShouldNotAppear"];
  window.show_all[];
endfunction 

