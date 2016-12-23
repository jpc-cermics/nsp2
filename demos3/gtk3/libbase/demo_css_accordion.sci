//  Theming/CSS Accordion
//  A simple accordion demo written using CSS transitions and multiple backgrounds
//
  
function demo_css_accordion (do_widget)

  function apply_css (widget, L)
    provider=L(1);
    context = widget.get_style_context[];
    context.add_provider[ provider, i2m(intmax(1u))];
    if is(widget,%types.GtkContainer) then 
      widget.forall[ apply_css, L];
    end
  endfunction 
  
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
  
  provider = gtk_css_provider_new ();
  if %f then 
    // if resources are present: 
    provider.load_from_resource["/css_accordion/css_accordion.css"];
  else
    // load css from path 
    provider.load_from_path[getenv('NSP')+"/demos3/gtk3/libbase/css_accordion.css"]
  end
  apply_css (window, list(provider));
  window.show_all[];
endfunction 

