//  Pango/Font Features
//
// This demonstrates support for OpenType font features with
// Pango attributes. The attributes can be used manually or
// via Pango markup.

function update (button,paramspec, data) 
  
  toggles = data(2);
  entry=data(1);

  text = entry.get_text [];
  //font_desc = font.get_font [];
  font_desc = "pipo";
  s="";
  has_feature = %f;
  for i =1:size(toggles,'*')
    if ~ toggles{i}.is_sensitive[] then continue;end
    if is(toggles{i},%types.GtkRadioButton) then 
      if toggles{i}.get_active[] then 
	if has_feature then 
	  s = s +  ", ";
	end
	s = s+ 	toggles{i}.get_name[];
	s = s+ " 1";
	has_feature = %t;
      end
    else
      if has_feature then s = s + ", ";end
      s = s  + 	toggles{i}.get_name[];
      if  toggles{i}.get_active[] then 
	s = s + " 1";
      else
	s = s +  " 0";
      end
      has_feature = %t;
    end
  end
  
  font_settings = s;
  settings=data(3);
  settings.set_text[font_settings];
  s = sprintf("<span font_desc=''%s'' font_features=''%s''>%s</span>", ...
	      font_desc, font_settings, text);
  label.set_markup[ s];
endfunction 

function reset (toggles)

  numcasedefault.set_active[%t];
  numspacedefault.set_active[%t];
  fractiondefault.set_active[%t];
  for i = 1:size(toggles,'*')
    toggle = toggles{i}
    if is(toggle,%types.GtkRadioButton) then 
      toggle.set_active[%f];
      toggle.set_sensitive[%f];
    end
  end
endfunction

function switch_to_entry (void)
  text = entry.get_text [];
  stack.set_visible_child_name[ "entry"];
endfunction

function switch_to_label (toggles)
  text = "";
  stack.set_visible_child_name[ "label"];
  pause switch_to_label
  update (toggles);
endfunction

function y= entry_key_press (entry, event,toggles)
  if (event.keyval == 0xff1b ) then 
    // GDK.KEY_Escape)
    entry.set_text[text];
    switch_to_label (toggles);
    y = GDK.EVENT_STOP;
    return 
  end
  y = GDK.EVENT_PROPAGATE;
endfunction

function window = demo_font_features (do_widget)

  fname = getenv('NSP')+ "/demos3/gtk3/libbase/demo_font-features/font-features.ui";
  builder = gtk_builder_new_from_file(fname)

  toggles={ builder.get_object[ "kern"];
	    builder.get_object[ "liga"];
	    builder.get_object[ "dlig"];
	    builder.get_object[ "hlig"];
	    builder.get_object[ "clig"];
	    builder.get_object[ "smcp"];
	    builder.get_object[ "c2sc"];
	    builder.get_object[ "lnum"];
	    builder.get_object[ "onum"];
	    builder.get_object[ "pnum"];
	    builder.get_object[ "tnum"];
	    builder.get_object[ "frac"];
	    builder.get_object[ "afrc"];
	    builder.get_object[ "zero"];
	    builder.get_object[ "nalt"];
	    builder.get_object[ "swsh"];
	    builder.get_object[ "calt"];
	    builder.get_object[ "hist"];
	    builder.get_object[ "salt"];
	    builder.get_object[ "ss01"];
	    builder.get_object[ "ss02"];
	    builder.get_object[ "ss03"];
	    builder.get_object[ "ss04"];
	    builder.get_object[ "ss05"]};
  // handlers 
  H=hash(10);
  H.update= update;
  H.reset= reset;
  H.switch_to_entry = switch_to_entry;
  H.switch_to_label = switch_to_label;
  H.entry_key_press = entry_key_press;
  
  window = builder.get_object[ "window"];
  label = builder.get_object[ "label"];
  settings = builder.get_object[ "settings"];
  resetbutton = builder.get_object[ "reset"];
  font = builder.get_object[ "font"];
  numcasedefault = builder.get_object[ "numcasedefault"];
  numspacedefault = builder.get_object[ "numspacedefault"];
  fractiondefault = builder.get_object[ "fractiondefault"];
  stack = builder.get_object[ "stack"];
  entry = builder.get_object[ "entry"];

  L= list(entry,toggles,settings);
  builder.connect_signals[H];
    
  update ("","",L);

  // window.connect[ "destroy",gtk_widget_destroyed, &window);
  // gtk_window_present (window);
  window.show_all[];
endfunction

