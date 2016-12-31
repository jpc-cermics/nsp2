function [y]=demo_idle(_button)
  y=1
  dialog = gtk_dialog_new()
  dialog.connect["delete_event", demo_delete];
  dialog.set_title["Idle Test"];
  label = gtk_label_new(str="count: 0")
  dialog_vbox = dialog.get_content_area[];
  dialog_vbox.pack_start[label]
  label.show[]

  // function to be executed when idle
  function [y]=idle_test(args)
    y=%t
    count=args(1).get_data['idle_count'];
    count = count + 1;
    args(1).set_data[idle_count=count];
    args(2).set_text["count: " + m2s(count,"%5.0f")];
  endfunction

  // starts the idle handler
  function [y]=start_idle_test(dialog,label)
    y=1
    if dialog.get_data['idle_id'] == 0 then
      id = gtk_idle_add(200,dialog.get_data['idle_f'],list(dialog,label))
      dialog.set_data[idle_id=id ]
    end
  endfunction

  // stops  the idle handler
  function [y]=stop_idle_test(dialog,label)
    y=1
    id = dialog.get_data['idle_id'];
    if id <> 0 then
      g_source_remove(id);
      dialog.set_data[idle_id=0];
    end
  endfunction

  // attach data to the main windget
  // since they must be shared between handlers
  // and exists when returning from this function.
  dialog.set_data[idle_f = idle_test];
  dialog.set_data[idle_id = 0];
  dialog.set_data[idle_count = 0];

  dialog.add_button["Close",1];
  dialog.add_button["Start",2];
  dialog.add_button["Stop",3];
  dialog.show_all[];

  while %t
    response = dialog.run[]
    select response
     case 1 then
      // Close button
      dialog.destroy[];
      return;
     case 2 then
      // Start
      start_idle_test(dialog,label);
     case 3 then
      // Stop
      stop_idle_test(dialog,label);
    end
  end
endfunction
