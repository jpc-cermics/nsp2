// Text Widget
//
//  The GtkTextView widget displays a GtkTextBuffer. One GtkTextBuffer
//  can be displayed by multiple GtkTextViews. This demo has two views
//  displaying a single buffer, and shows off the widget's text
//  formatting features.
// 
// FIXME: unfinished ...  reste du boulot !!!! 

function create_tags (buffer)
// Create a bunch of tags. Note that it's also possible to
// create tags with gtktexttag_new() then add them to the
// tag table for the buffer, gtk_text_buffer_create_tag() is
// just a convenience function. Also note that you don't have
// to give tags a name; pass NULL for the name to create an
// anonymous tag.
//
// In any real app, another useful optimization would be to create
// a GtkTextTagTable in advance, and reuse the same tag table for
// all the buffers with the same tag set, instead of creating
// new copies of the same tags for every buffer.
//
// Tags are assigned default priorities in order of addition to the
// tag table.	 That is, tags created later that affect the same text
// property affected by an earlier tag will override the earlier
// tag.  You can modify tag priorities with
// gtk_text_tag_set_priority().
// XXXXX reste a regler le pb de size qui est pas clair 
// d'autre part create_tag doit renvoyer un tag 
// comment utilise-t-on les tags ensuite ? 

  buffer.create_tag[ "heading", weight= PANGO.WEIGHT_BOLD, scale= PANGO.SCALE_XX_LARGE];
  buffer.create_tag[ "italic",  style= PANGO.STYLE_ITALIC ];
  buffer.create_tag[ "bold",    weight= PANGO.WEIGHT_BOLD ];  
  buffer.create_tag[ "big",     size= 20 * PANGO.SCALE_MEDIUM ];
  buffer.create_tag[ "xx-small",scale= PANGO.SCALE_XX_SMALL ];
  buffer.create_tag[ "x-large", scale= PANGO.SCALE_X_LARGE ];
  buffer.create_tag[ "monospace",family= "monospace" ];
  buffer.create_tag[ "blue_foreground", foreground= "blue" ];  
  buffer.create_tag[ "red_background",  background= "red"];
  //stipple = gdk_bitmap_create_from_data (],gray50_bits, gray50_width,gray50_height);
  //buffer.create_tag[ "background_stipple", background_stipple= stipple ];
  //buffer.create_tag[ "foreground_stipple", foreground_stipple= stipple ];
  //g_object_unref (stipple);
  buffer.create_tag[ "big_gap_before_line", pixels_above_lines= 30 ];
  buffer.create_tag[ "big_gap_after_line", pixels_below_lines= 30 ];
  buffer.create_tag[ "double_spaced_line", pixels_inside_wrap= 10 ];
  buffer.create_tag[ "not_editable", editable= %f];
  buffer.create_tag[ "word_wrap", wrap_mode= GTK.WRAP_WORD ];
  buffer.create_tag[ "char_wrap", wrap_mode= GTK.WRAP_CHAR ];
  buffer.create_tag[ "no_wrap",   wrap_mode= GTK.WRAP_NONE ];
  buffer.create_tag[ "center",    justification= GTK.JUSTIFY_CENTER];
  buffer.create_tag[ "right_justify", justification= GTK.JUSTIFY_RIGHT ];
  buffer.create_tag[ "wide_margins", left_margin= 50, right_margin= 50  ];
  buffer.create_tag[ "strikethrough", strikethrough= %t ];
  buffer.create_tag[ "underline", underline= PANGO.UNDERLINE_SINGLE ];
  buffer.create_tag[ "double_underline", underline= PANGO.UNDERLINE_DOUBLE ];
  buffer.create_tag[ "superscript", rise= 10 * PANGO.SCALE_MEDIUM, size= 8 * PANGO.SCALE_MEDIUM];
  buffer.create_tag[ "subscript", rise= -10 * PANGO.SCALE_MEDIUM,  size= 8 * PANGO.SCALE_MEDIUM];
  buffer.create_tag[ "rtl_quote", wrap_mode= GTK.WRAP_WORD, direction= GTK.TEXT_DIR_RTL,
		     indent= 30,  left_margin= 20, right_margin= 20];
endfunction 

function small_insert(buffer) 
  iter = buffer.get_iter_at_offset [0];
  buffer.insert_with_tags_by_name [iter, "The text widget can display text with all kinds of nifty attributes. It also supports multiple views of the same buffer; this demo is showing the same buffer in two places.\n\n","char_wrap"];
endfunction 

function insert_text (buffer)
// XXXX attention a metre dans gtkpixbuf  
  gtk_logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif';
  pix=gdk_pixbuf_new_from_file(gtk_logo);
  pix = pix.scale_simple[32,32, GDK.INTERP_BILINEAR];
  
  // get start of buffer; each insertion will revalidate the
  // iterator to point to just after the inserted text.
  iter = buffer.get_iter_at_offset [0];
  buffer.insert [iter, "The text widget can display text with all kinds of nifty attributes. It also supports multiple views of the same buffer; this demo is showing the same buffer in two places.\n\n"];
  buffer.insert_with_tags_by_name [iter, "Font styles. ",   "heading"];
  buffer.insert [iter, "For example, you can have "];
  buffer.insert_with_tags_by_name [iter,  "italic",   "italic"];
  buffer.insert [iter, ", "];  
  buffer.insert_with_tags_by_name [iter,  "bold",   "bold"];
  buffer.insert [iter, ", or "];
  buffer.insert_with_tags_by_name [iter,  "monospace (typewriter)",   "monospace"];
  buffer.insert [iter, ", or "];
  buffer.insert_with_tags_by_name [iter,  "big", "big"];
  buffer.insert [iter, " text. "];
  buffer.insert [iter, "It''s best not to hardcode specific text sizes; you can use relative sizes as with CSS, such as "];
  buffer.insert_with_tags_by_name [iter,  "xx-small",  "xx-small"];
  buffer.insert [iter, " or "];
  buffer.insert_with_tags_by_name [iter,  "x-large",   "x-large"];
  buffer.insert [iter, " to ensure that your program properly adapts if the user changes the default font size.\n\n"];
  buffer.insert_with_tags_by_name [iter, "Colors. ",   "heading"];
  buffer.insert [iter, "Colors such as "];  
  buffer.insert_with_tags_by_name [iter,  "a blue foreground",  "blue_foreground"];
  buffer.insert [iter, " or "];  
  buffer.insert_with_tags_by_name [iter,  "a red background",  "red_background"];
  buffer.insert [iter, " or even "];  
  buffer.insert_with_tags_by_name [iter,  "a stippled red background",  "red_background", "background_stipple"];
  buffer.insert [iter, " or "];  
  buffer.insert_with_tags_by_name [iter, "a stippled blue foreground on solid red background", 
		    "blue_foreground", "red_background", "foreground_stipple"]
  buffer.insert [iter, " (select that to read it) can be used.\n\n"];  
  buffer.insert_with_tags_by_name [iter, "Underline, strikethrough, and rise. ",  "heading"];
  buffer.insert_with_tags_by_name [iter, "Strikethrough",  "strikethrough"];
  buffer.insert [iter, ", "];
  buffer.insert_with_tags_by_name [iter,  "underline",  "underline"];
  buffer.insert [iter, ", "];
  buffer.insert_with_tags_by_name [iter,  "double underline",  "double_underline"];
  buffer.insert [iter, ", "];
  buffer.insert_with_tags_by_name [iter,  "superscript",  "superscript"];
  buffer.insert [iter, ", and "];
  buffer.insert_with_tags_by_name [iter,  "subscript",  "subscript"];
  buffer.insert [iter, " are all supported.\n\n"];
  buffer.insert_with_tags_by_name [iter, "Images. ",  "heading"];
  buffer.insert [iter, "The buffer can have images in it: "];
  buffer.insert_pixbuf [iter, pix];
  buffer.insert_pixbuf [iter, pix];
  buffer.insert_pixbuf [iter, pix];
  buffer.insert [iter, " for example.\n\n"];
  buffer.insert_with_tags_by_name [iter, "Spacing. ",  "heading"];
  buffer.insert [iter, "You can adjust the amount of space before each line.\n"];
  buffer.insert_with_tags_by_name [iter, "This line has a whole lot of space before it.\n", "big_gap_before_line", "wide_margins"];
  buffer.insert_with_tags_by_name [iter, "You can also adjust the amount of space after each line; this line has a whole lot of space after it.\n", "big_gap_after_line", "wide_margins"];
  buffer.insert_with_tags_by_name [iter,  "You can also adjust the amount of space between wrapped lines; this line has extra space between each wrapped line in the same paragraph. To show off wrapping, some filler text: the quick brown fox jumped over the lazy dog. Blah blah blah blah blah blah blah blah blah.\n",   "double_spaced_line", "wide_margins"];
  buffer.insert [iter, "Also note that those lines have extra-wide margins.\n\n"];
  buffer.insert_with_tags_by_name [iter, "Editability. ",    "heading"];
  buffer.insert_with_tags_by_name [iter, "This line is ''locked down'' and can''t be edited by the user - just try it! You can''t delete this line.\n\n", "not_editable"];
  buffer.insert_with_tags_by_name [iter, "Wrapping. ",  "heading"];
  buffer.insert [iter, "This line (and most of the others in this buffer) is word-wrapped, using the proper Unicode algorithm. Word wrap should work in all scripts and languages that GTK+ supports. Let''s make this a long paragraph to demonstrate: blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah\n\n"];  
  buffer.insert_with_tags_by_name [iter, "This line has character-based wrapping, and can wrap between any two character glyphs. Let''s make this a long paragraph to demonstrate: blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah blah\n\n", 
		    "char_wrap"];
  
  buffer.insert_with_tags_by_name [iter, "This line has all wrapping turned off, so it makes the horizontal scrollbar appear.\n\n\n","no_wrap"];
  buffer.insert_with_tags_by_name [iter, "Justification. ",   "heading"];  
  buffer.insert_with_tags_by_name [iter,  "\nThis line has center justification.\n",  "center"];
  buffer.insert_with_tags_by_name [iter,  "This line has right justification.\n",  "right_justify"];
  buffer.insert_with_tags_by_name [iter,  "\nThis line has big wide margins. Text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text text.\n", 
		    "wide_margins"];  
  buffer.insert_with_tags_by_name [iter, "Internationalization. ",   "heading"];
  buffer.insert [iter,  "You can put all sorts of Unicode text in the buffer.\n\nGerman (Deutsch S\303\274d) Gr\303\274\303\237 Gott\nGreek (\316\225\316\273\316\273\316\267\316\275\316\271\316\272\316\254) \316\223\316\265\316\271\316\254 \317\203\316\261\317\202\nHebrew	\327\251\327\234\327\225\327\235\nJapanese (\346\227\245\346\234\254\350\252\236)\n\nThe widget properly handles bidirectional text, word wrapping, DOS/UNIX/Unicode paragraph separators, grapheme boundaries, and so on using the Pango internationalization framework.\n"];  

  buffer.insert [iter, "Here''s a word-wrapped quote in a right-to-left language:\n"];
  //  buffer.insert_with_tags_by_name [iter, "\331\210\331\202\330\257 \330\250\330\257 \330\250\331\210\331\204\331\212\331\201\331\212\330\247.\n\n", "rtl_quote"];
  
  buffer.insert [iter, "You can put widgets in the buffer: Here''s a button: "];
  anchor = buffer.create_child_anchor [iter];
  buffer.insert [iter, " and a menu: "];
  anchor = buffer.create_child_anchor [iter];
  buffer.insert [iter, " and a scale: "];
  anchor = buffer.create_child_anchor [iter];
  buffer.insert [iter, " and an animation: "];
  anchor = buffer.create_child_anchor [iter];
  buffer.insert [iter, " finally a text entry: "];
  anchor = buffer.create_child_anchor [iter];
  buffer.insert [iter, ".\n"];
  
  buffer.insert [iter, "\n\nThis demo doesn''t demonstrate all the GtkTextBuffer features; it leaves out, for example: invisible/hidden text (doesn''t work in GTK 2, but planned), tab stops, application-drawn areas on the sides of the widget for displaying breakpoints and such..."];

  // Apply word_wrap tag to whole buffer */
  // XXXX  [it_start,it_end]=buffer.get_bounds [];
  // XXXX  buffer.apply_tag_by_name["word_wrap", it_start, it_end];
endfunction

function rep= find_anchor (iter)
  rep=%f;
  while iter.forward_char[]
    if iter.get_child_anchor[] then rep=%t;break;end
  end 
endfunction 

function attach_widgets(text_view)
  buffer = text_view.get_buffer[];
  iter = buffer.get_start_iter[];
  i = 0;
  while iter.find_anchor[]
    anchor = iter.get_child_anchor[];
    if i == 0  then 
      widget = gtkbutton_new(label="Click Me");
      widget.connect["clicked",easter_egg_callback];
    elseif i == 1
      menu = gtkmenu_new ();
      widget = gtkoptionmenu_new ();
      menu_item = gtkmenuitem_new(label="Option 1");
      menu.append[menu_item];
      menu_item = gtkmenuitem_new(label="Option 2");
      menu.append[menu_item];
      menu_item = gtkmenuitem_new(label="Option 3");
      menu.append[menu_item];
      widget.set_menu[menu];
    elseif i == 2
      widget = gtkhscale_new (NULL);
      widget.set_range[ 0, 100];
      widget.set_size_request[70, -1];
    elseif i == 3
      flop = getenv('NSP')+'/demos/gtk2/libplus/floppybuddy.gif";
      widget = gtkimage_new('file',flop);
    elseif i == 4
      widget = gtkentry_new ();
    end
    text_view.add_child_at_anchor[ widget, anchor];
    widget.show_all[];
    i=i+1;
  end 
endfunction 

function demo_textview ()
  window = gtkwindow_new();
  window.set_default_size[450, 450];
  // XXXXX window.connect["destroy",gtk_widget_destroyed];
  window.set_title["TextView"];
  window.set_border_width[0];
  vpaned = gtkvpaned_new ();
  vpaned.set_border_width[5];
  window.add[vpaned];

  // For convenience, we just use the autocreated buffer from
  // the first text view; you could also create the buffer
  // by itself with gtk_text_buffer_new(), then later create
  // a view widget.

  view1 = gtktextview_new ();
  buffer = view1.get_buffer[];
  view2 = gtktextview_new(buffer=buffer);
  
  sw = gtkscrolledwindow_new();
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  vpaned.add1[sw];
  sw.add[view1];

  sw = gtkscrolledwindow_new();
  sw.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  vpaned.add2[sw];
  sw.add[view2];

  create_tags (buffer);
  insert_text (buffer);
  //small_insert(buffer);
  //XXXXX attach_widgets (view1);
  //XXXXX attach_widgets (view2);
  vpaned.show_all[];
  window.show[];
endfunction 


function recursive_attach_view (depth,view,anchor)
  if depth > 4 then return; end ;
  
  child_view = gtktextview_new(buffer=view.get_buffer[]);

  // Event box is to add a black border around each child view */
  event_box = gtkeventbox_new ();
  color= gdk_color_parse ("black");
  gtk_widget_modify_bg (event_box, GTK_STATE_NORMAL, color);

  align = gtkalignment_new (xalign=0.5,yalign=0.5,xscale=1,yscale=1);

  align.set_border_width[1];
  event_box.add[align];
  align.add[child_view];
  view.add_child_at_anchor[event_box, anchor];
  recursive_attach_view (depth + 1, child_view, anchor);
endfunction 


function easter_egg_callback (button, data)
  buffer = gtktextbuffer_new ();
  iter= buffer.get_start_iter[];
  buffer.insert[iter,"This buffer is shared by a set of nested text views.\n Nested view:\n"];
  anchor = buffer.create_child_anchor[iter];
  buffer.insert[iter,"\nDon''t do this in real applications, please.\n"];
  view = gtktextview_new(buffer=buffer);
  recursive_attach_view (0,view, anchor);
  window = gtkwindow_new();
  sw = gtkscrolledwindow_new();
  sw.set_policy[ GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  window.add[sw]
  sw.add[view];
  window.set_default_size[300,400];
  window.show_all[];
endfunction 


