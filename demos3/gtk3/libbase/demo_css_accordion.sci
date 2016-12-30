//  Theming/CSS Accordion
//
//  A simple accordion demo written using CSS transitions and multiple backgrounds
  
function window= demo_css_accordion (do_widget)

 
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_title [ "CSS Accordion"];
  //window.set_transient_for[do_widget];
  window.set_default_size[600, 300];
  // window.connect[ "destroy",  gtk_widget_destroyed, &window];
  
  container = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=0);
  container.set_halign[GTK.ALIGN_CENTER];
  container.set_valign[GTK.ALIGN_CENTER];
  window.add[container];
  
  labels=["This","Is","A","CSS","Accordion",":-)"];
  for i=1:size(labels,'*')
    child = gtk_button_new(label=labels(i));
    container.add[child];
  end
  
  // provider = gtk_css_provider_new ();
  // provider.load_from_path[getenv('NSP')+"/demos3/gtk3/libbase/demo_css_accordion/demo_css_accordion.css"]
  
  S=getfile(getenv('NSP')+"/demos3/gtk3/libbase/demo_css_accordion/demo_css_accordion.css");
  S=strsubst(S,'NSP',getenv('NSP'));
  text = catenate(S,sep='\n');
  provider = gtk_css_provider_new ();
  ok=execstr('provider.load_from_data[text, -1];',errcatch=%t);
  if ~ok then printf("Error: failed to load css files\n");lasterror();end
  
  css_apply_css (window, list(provider));
  window.show_all[];
endfunction 

