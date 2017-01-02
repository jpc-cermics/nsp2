//  Pango/Font Features
//
// This demonstrates support for OpenType font features with
// Pango attributes. The attributes can be used manually or
// via Pango markup.

function window = demo_font_features (do_widget)
  
  function switch_to_entry (eventbox,event,data)
    entry=data(1);
    stack=data(5)
    stack.set_visible_child_name[ "entry"];
  endfunction

  function switch_to_label (eventbox,event,data)
    data(5).set_visible_child_name[ "label"];
    data(10)("","",data);
  endfunction

  function y= entry_key_press (entry, event,data)
    if (event.keyval == 0xff1b ) then 
      // 0xff1b is GDK_KEY_Escape 
      data(5).set_visible_child_name[ "label"];
      data(10)("","",data);
      y = %t;// GDK.EVENT_STOP;
    else
      y = %f;// GDK.EVENT_PROPAGATE;
    end
  endfunction
  
  
  function update (button,paramspec, data) 
    if nargin == 2 then data=paramspec;end
    toggles = data(2);
    entry=data(1);
    label = data(4);
    font = data(9);
    text = entry.get_text [];
    font_desc = font.get_font_name[];
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
    label.set_markup[s];
    
  endfunction 

  function reset (button, data)

    toggles = data(2);
    entry=data(1);
    label = data(4);
    numcasedefault=data(6);
    numspacedefault=data(7);
    fractiondefault=data(8);
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
  
  // get objects 
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
  
  // prepare data for handlers 
  L= list(entry,toggles,settings,label,stack,numcasedefault,numspacedefault,...
	  fractiondefault,font,update);
  
  builder.connect_signals[H,L];
  
  update ("","",L);
  
  window.show_all[];
endfunction

