//
// Label Demo
// FIXME : selectable is not working ? 

function sensitivity_toggled (toggle,args)
  args(1).set_sensitive[toggle.get_active[]]  
endfunction 


function button=create_sensitivity_control (widget)
  button = gtktogglebutton_new(label="Sensitive");  
  button.set_active[widget.get_property['sensitive']];
  button.connect["toggled", sensitivity_toggled,list(widget)]
  button.show_all[];
endfunction 

function set_selectable_recursive(widget, setting)
  if is(widget, %types.GtkContainer) 
    children = widget.get_children[];
    for ch=children 
      set_selectable_recursive(ch, setting);
    end
  elseif is(widget,%types.GtkLabel) 
    widget.set_selectable[setting]
  else 
    printf("Not a label or a container ");
  end
endfunction

function selectable_toggled (toggle,args)
  set_selectable_recursive (args(1), toggle.get_active[]);
endfunction 

function button=create_selectable_control (widget)
  button = gtktogglebutton_new(label="Selectable");  
  button.set_active[ %f]
  button.connect[ "toggled", selectable_toggled,list(widget)]
  button.show_all[]
endfunction 

function demo_labels ()
  window = gtkwindow_new();
  // window.connect[  "destroy", hide];
  window.set_title[  "Label"]
  
  vbox = gtkvbox_new(homogeneous=%f,spacing=5);
  hbox = gtkhbox_new(homogeneous=%f,spacing=5);
  window.add[  vbox]
  vbox.pack_end[hbox];

  button = create_sensitivity_control (hbox);

  vbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  button = create_selectable_control (hbox);

  vbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  vbox = gtkvbox_new(homogeneous=%f,spacing=5);
      
  hbox.pack_start[ vbox,expand=%f,fill=%f,padding=0]
  window.set_border_width[  5]

  frame = gtkframe_new(label="Normal Label");
  label = gtklabel_new(str="This is a Normal label");
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]
  
  frame = gtkframe_new(label="Multi-line Label");
  label = gtklabel_new(str="This is a Multi-line label.\nSecond line\nThird line");
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]

  frame = gtkframe_new(label="Left Justified Label");
  label = gtklabel_new(str="This is a Left-Justified\nMulti-line label.\nThird      line");
  label.set_justify[  GTK.JUSTIFY_LEFT]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]

  frame = gtkframe_new(label="Right Justified Label");
  label = gtklabel_new(str="This is a Right-Justified\nMulti-line label.\nFourth line, (j/k)");
  label.set_justify[  GTK.JUSTIFY_RIGHT]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]

  frame = gtkframe_new(label="Internationalized Label");
  label = gtklabel_new();
  str=[ "French (Fran\303\247ais) Bonjour, Salut\n"
	"Korean (\355\225\234\352\270\200)   \354\225\210\353\205\225\355\225\230\354\204\270\354\232\224, \354\225\210\353\205\225\355\225\230\354\213\255\353\213\210\352\271\214\n"
	"Russian (\320\240\321\203\321\201\321\201\320\272\320\270\320\271) \320\227\320\264\321\200\320\260\320\262\321\201\321\202\320\262\321\203\320\271\321\202\320\265!\n"
	"Chinese (Simplified) <span lang=""zh-cn"">\345\205\203\346\260\224	\345\274\200\345\217\221</span>\n"
	"Chinese (Traditional) <span lang=""zh-tw"">\345\205\203\346\260\243	\351\226\213\347\231\274</span>\n"
	"Japanese <span lang=""ja"">\345\205\203\346\260\227	\351\226\213\347\231\272</span>"]
  label.set_markup[ catenate(str)];
  label.set_justify[  GTK.JUSTIFY_LEFT]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]
  frame = gtkframe_new(label="Bidirection Label");
  str = ["Arabic	\330\247\331\204\330\263\331\204\330\247\331\205\330\271\331\204\331\212\331\203\331\205\n"
	 "Hebrew	\327\251\327\234\327\225\327\235"];	 
  label = gtklabel_new(str=catenate(str));
  label.set_direction[  GTK.TEXT_DIR_RTL]
  label.set_justify[  GTK.JUSTIFY_RIGHT]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]

  vbox = gtkvbox_new(homogeneous=%f,spacing=5);
  hbox.pack_start[ vbox,expand=%f,fill=%f,padding=0]
  frame = gtkframe_new(label="Line wrapped label");
  str = [ "This is an example of a line-wrapped label.  It should not be taking "
	  "up the entire             "
	  "width allocated to it, but automatically wraps the words to fit.  "
	  "The time has come, for all good men, to come to the aid of their party.  "
	  "The sixth sheik''s six sheep''s sick.\n"
	  "     It supports multiple paragraphs correctly, and  correctly   adds "
	  "many          extra  spaces. "];
  label = gtklabel_new(str=catenate(str));

  label.set_line_wrap[  %t]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]
  
  frame = gtkframe_new(label="Filled, wrapped label");
  str = ["This is an example of a line-wrapped, filled label.  It should be taking "
	 "up the entire              width allocated to it.  Here is a seneance to prove "
	 "my point.  Here is another sentence. "
	 "Here comes the sun, do de do de do.\n"
	 "    This is a new paragraph.\n"
	 "    This is another newer, longer, better paragraph.  It is coming to an end, "
	 "unfortunately."];
			 
  label = gtklabel_new(str=catenate(str));
  label.set_justify[  GTK.JUSTIFY_FILL]
  label.set_line_wrap[  %t]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]
  
  frame = gtkframe_new(label="Underlined label");
  str=["This label is underlined!\n"
       "This one is underlined (\343\201\223\343\202\223\343\201\253\343\201\241\343\201\257) in quite a funky fashion"];
  label = gtklabel_new(str=catenate(str));
  label.set_justify[  GTK.JUSTIFY_LEFT]
  label.set_pattern[  "_________________________ _ _________ _ _____ _ __ __  ___ ____ _____"]
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]

  frame = gtkframe_new(label="Markup label");
  label = gtklabel_new()

  // There's also a gtk_label_set_markup() without accel if you
  // don't have an accelerator key
  str=["This <span foreground=""blue"" background=""orange"">label</span> has "
       "<b>markup</b> _such as "
       "<big><i>Big Italics</i></big>\n"
       "<tt>Monospace font</tt>\n"
       "<u>Underline!</u>\n"
       "foo\n"
       "<span foreground=""green"" background=""red"">Ugly colors</span>\n"
       "and nothing on this line,\n"
       "or this.\n"
       "or this either\n"
       "or even on this one\n"
       "la <big>la <big>la <big>la <big>la</big></big></big></big>\n"
       "but this _word is <span foreground=""purple""><big>purple</big></span>\n"
       "<span underline=""double"">We like <sup>superscript</sup> and <sub>subscript</sub> too</span>"];

  label.set_markup_with_mnemonic[ catenate(str)]; 

  //g_assert (label.get_mnemonic_keyval[] == GDK_s);
      
  frame.add[  label]
  vbox.pack_start[ frame,expand=%f,fill=%f,padding=0]
  window.show_all[];
endfunction

