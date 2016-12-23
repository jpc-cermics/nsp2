// Flow Box
//
// GtkFlowBox allows flexible and responsive grids which reflow
// as needed and support sorting and filtering.
// The children of a GtkFlowBox are regular widgets

function window=demo_flowbox (do_widget)

  function button= color_swatch_new (color)

    function y= draw_color (drawingarea, cr, color)
      r=color(1);g=color(2);b=color(3);
      ok = execstr(sprintf('color= gdk_rgba_new(''rgb(%d,%d,%d)'');',r,g,b),errcatch=%t);
      if ok then 
	cairo_set_source_rgba(cr,color.red,color.green,color.blue,color.alpha);
	cairo_paint (cr);
      else
	printf("%s",catenate(lasterror()));
      end
      y=%f;
    endfunction 
  
    button = gtk_button_new ();
    area = gtk_drawing_area_new ();
    area.connect[ "draw", draw_color, color];
    area.set_size_request[24, 24];
    button.add[area];
    button.show_all[];
  endfunction 
  
  colors=hotcolormap(320);
  
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  //window.set_screen[ do_widget.get_screen []];
  window.set_title[ "Flow Box"];
  window.set_default_size[400, 600];
  //window.connect[ "destroy",gtk_widget_destroyed, &window];
  scrolled = gtk_scrolled_window_new ();
  scrolled.set_policy[GTK.POLICY_NEVER, GTK.POLICY_AUTOMATIC];
  flowbox = gtk_flow_box_new ();
  flowbox.set_valign[GTK.ALIGN_START];
  flowbox.set_max_children_per_line[30];
  flowbox.set_selection_mode[GTK.SELECTION_NONE];

  scrolled.add[flowbox];
  window.add[scrolled];

  for i =1:size(colors,'r')
    flowbox.add[color_swatch_new (256*colors(i,:))];
  end
  scrolled.show_all[];
  window.show[];
endfunction 
