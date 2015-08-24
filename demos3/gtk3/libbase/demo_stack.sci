
function set_visible_child (button, data)
  stack.set_visible_child[data(1)];
endfunction

function set_visible_child_name (button, data)
  stack.set_visible_child_name[data(1)];
endfunction

function toggle_hhomogeneous (button, data)
  active = button.get_active[];
  stack.set_hhomogeneous[active];
endfunction

function toggle_vhomogeneous (button, data)
  active = button.get_active[];
  stack.set_vhomogeneous[active];
endfunction

function toggle_icon_name (button, data)
  active = button.get_active[];
  if active then
    stack.child_set[w1, "icon-name"=  "edit-find-symbolic" ];
  end
endfunction

function toggle_transitions (combo, data)
  stack=data(1);
  id = combo.get_active[];
  stack.set_transition_type[id];
endfunction

function on_back_button_clicked (button, stack)
  stack=data(1);
  seq= [ "1", "2", "3"];
  vis = stack.get_visible_child_name[];
  for i = 1:size(seq,'*')
    if vis.equal[seq(i)] then
      stack.set_visible_child_full[ seq(i-1),
		    GTK.STACK_TRANSITION_TYPE_SLIDE_RIGHT];
      return;
    end
  end
endfunction

function on_forward_button_clicked (button, stack)
  stack=data(1);
  seq = [ "1", "2", "3"];
  vis = stack.get_visible_child_name[];
  for i = 1:size(seq,'*')
    if vis.equal[seq(i)] then
      stack.set_visible_child_full[ seq(i + 1),
		    GTK.STACK_TRANSITION_TYPE_SLIDE_LEFT];
      return;
    end
  end
endfunction

function update_back_button_sensitivity (stack, pspec, button)
  vis = stack.get_visible_child_name[];
  button.set_sensitive[ ~vis.equal["1"]];
endfunction

function update_forward_button_sensitivity (stack, pspec, button)
  vis = stack.get_visible_child_name[];
  button.set_sensitive [ ~vis.equal["3"]];
endfunction

function demo_stack()
  window = gtk_window_new (type=GTK.WINDOW_TOPLEVEL);
  window.set_size_request[300, 300];

  box = gtk_box_new (GTK.ORIENTATION_VERTICAL,spacing= 0);
  window.add[box];

  switcher = gtk_stack_switcher_new ();
  box.pack_start[switcher,expand=%f, fill=%f,padding=0];

  stack = gtk_stack_new ();

  //  Make transitions longer so we can see that they work
  stack.set_transition_duration[1500];

  stack.set_halign[GTK.ALIGN_START];

  //  Add sidebar before stack

  // sidebar = gtk_stack_sidebar_new ();
  // sidebar.set_stack[stack];

  // layout = gtk_box_new (GTK.ORIENTATION_HORIZONTAL, spacing=0);
  // layout.pack_start[sidebar,expand=%f, fill=%f,padding=0];
  // layout.pack_start[stack, expand=%t, fill=%t,padding=0];

  //  box.add[layout];

  switcher.set_stack[stack];

  w1= gtk_text_view_new()
  text_buffer= w1.get_buffer[];
  text_buffer.set_text[ "This is a\nTest\nBalh!"];
  stack.add_with_properties[w1,name="1",title="1"];

  w2 = gtk_button_new(label="Gazoooooooooooooooonk");
  stack.add[w2];
  stack.child_set[w2, name= "2",title= "2"];// "needs-attention"= %t];

  scrolled_win = gtk_scrolled_window_new ();
  scrolled_win.set_policy[GTK.POLICY_AUTOMATIC, GTK.POLICY_AUTOMATIC];
  scrolled_win.set_size_request[100, 200];

  store = gtk_list_store_new( list(""),%f);

  for i = 0:39 do
    // store.insert_with_values[&iter, i, 0,  "Testvalule"];
  end;

  tree_view = gtk_tree_view_new_with_model (store);
  scrolled_win.add[tree_view];
  w3 = scrolled_win;

  renderer = gtk_cell_renderer_text_new ();
  //column = gtk_tree_view_column_new_with_attributes ("Target", renderer, "text"= 0);
  column = gtk_tree_view_column_new ();
  column.set_title [ "Target"];
  column.pack_start [ renderer, %f];
  column.add_attribute[renderer,"text",0]
  tree_view.append_column[ column];

  stack.add_titled[w3, "3", "3"];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 0);
  box.add[hbox];

  button = gtk_button_new(label="1");
  hbox.add[button];
  button.connect[ "clicked",  set_visible_child, list(w1)];

  button = gtk_button_new(label="2");
  hbox.add[button];
  button.connect[ "clicked",  set_visible_child,list( w2)];

  button = gtk_button_new(label="3");
  hbox.add[button];
  button.connect[ "clicked",  set_visible_child,list( w3)];

  button = gtk_button_new(label="1");
  hbox.add[button];
  button.connect[ "clicked",  set_visible_child_name, list( "1")];

  button = gtk_button_new(label="2");
  hbox.add[button];
  button.connect[ "clicked",  set_visible_child_name, list( "2")];

  button = gtk_button_new(label="3");
  hbox.add[button];
  button.connect [ "clicked", set_visible_child_name,list( "3")];

  // button = gtk_check_button_new ();
  // button.set_active[ stack.get_hhomogeneous[]];
  // hbox.add[button];
  // button.connect[ "clicked", toggle_hhomogeneous];

  // button = gtk_check_button_new_with_label ("homogeneous");
  // button.set_active[ stack.get_vhomogeneous[]];
  // hbox.add[button];
  // button.connect["clicked",  toggle_vhomogeneous];

  button = gtk_toggle_button_new(label="Add icon");
  button.connect[ "toggled",  toggle_icon_name];
  hbox.add[button];

  combo = gtk_combo_box_text_new ();

  // XXXX: could be interesting to get these informations
  // class = g_type_class_ref (GTK.TYPE_STACK_TRANSITION_TYPE);
  // Obtain the possible values of an enum or flags ?
  B=strstr(GTK.__keys,"STACK_TRANSITION_TYPE");
  values= tolower(GTK.__keys(B<>0));
  values= strsubst(values,'stack_transition_type_",'')
  for i = 1:size(values,'*')
    combo.append_text[values(i)];
  end

  hbox.add[combo];
  combo.connect[ "changed",  toggle_transitions,list(stack)];
  combo.set_active[0];

  hbox = gtk_box_new (GTK.ORIENTATION_HORIZONTAL,spacing= 0);
  box.add[hbox];

  button = gtk_button_new(label="<");
  button.connect[ "clicked",  on_back_button_clicked, list(stack)];
  stack.connect[ "notify::visible-child-name", update_back_button_sensitivity,list( button)];
  hbox.add[button];

  button = gtk_button_new(label=">");
  hbox.add[button];
  button.connect[ "clicked",  on_forward_button_clicked, list(stack)];
  stack.connect [ "notify::visible-child-name", update_forward_button_sensitivity,list( button)];
  window.show_all[];
endfunction
