// Focus test

function demo_focus()

  function [l,table]=make_focus_table(flag)
    table = gtk_grid_new();
    tf =  ior(GTK.EXPAND,GTK.FILL);
    count=1
    l= list();
    for i=0:2
      for j=0:2
	if flag then pos= j+3*i;else pos=  i+3*j; end
	if modulo((i + j),2)==0
          widget = gtk_entry_new ();
	  widget.set_text[sprintf("Foo %d",pos+1)];
	else
          widget = gtk_button_new(label=sprintf("Foo %d",pos+1));
	end
	l($+1) =  widget
	table.attach[widget, i, j, 1,1];
      end
    end
  endfunction

  dialog = gtk_dialog_new(title="Keyboard focus navigation");
  //dialog.connect["destroy",hide]
  //dialog.connect["response",hide ]
  dialog.set_title["Keyboard Focus Navigation"]
  frame = gtk_frame_new(label="Weird tab focus chain");
  dialog_vbox = dialog.get_content_area[];
  dialog_vbox.pack_start[ frame,expand=%t,fill=%t,padding=0];
  [l,table] = make_focus_table(%t);
  frame.add[  table]
  table.set_focus_chain[ l]
  frame = gtk_frame_new(label="Default tab focus chain");
  dialog_vbox.pack_start[ frame,expand=%t,fill=%t,padding=0];
  [l,table] = make_focus_table(%f);
  frame.add[ table]

  dialog.add_button["Close",10];
  dialog.show_all[];
  response = dialog.run[]
  if response == 10 // Close button
    dialog.destroy[];
  end

endfunction
