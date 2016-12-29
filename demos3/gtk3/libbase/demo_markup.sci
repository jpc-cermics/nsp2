// Text View/Markup
//
// GtkTextBuffer lets you define your own tags that can influence
// text formatting in a variety of ways. In this example, we show
// that GtkTextBuffer can load Pango markup and automatically generate
// suitable tags.

function window = demo_markup (do_widget)

  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  if nargin >= 1 then window.set_screen[ do_widget.get_screen []];end 
  window.set_default_size[450, 450];
  //window.connect[ "destroy",  gtk_widget_destroyed, &window);

  window.set_title[ "Markup"];

  view = gtk_text_view_new ();
  view.set_wrap_mode[GTK.WRAP_WORD];
  view.set_left_margin[10];
  view.set_right_margin[10];

  sw = gtk_scrolled_window_new ();
  sw.set_policy[ GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC];
  window.add[sw];
  sw.add[view];
  
  buffer = view.get_buffer [];
  iter = buffer.get_start_iter[];

  S=getfile(getenv('NSP')+"/demos3/gtk3/libbase/demo_markup.txt");
  S=strsubst(S,'NSP',getenv('NSP'));
    
  buffer.insert_markup[iter, catenate(S,sep='\n'),-1];
  sw.show_all[];
  window.show[];
endfunction



