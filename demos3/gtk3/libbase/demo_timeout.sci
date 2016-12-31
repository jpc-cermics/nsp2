// timeout work properly with gtk_main()
// but when we are at the nsp prompt
// timeout are not properly activated
// (i.e they are losing activation time)

function demo_timeout(_button,parent)

  dialog = gtk_dialog_new(parent=parent)
  dialog.connect["delete_event", demo_delete];
  dialog.set_title["Timeout Test"];
  label = gtk_label_new(str="count: 0")
  //label.set_padding[10,10]
  dialog_vbox = dialog.get_content_area[];
  dialog_vbox.pack_start[label];
  label.show[]

  // function to be executed when timeout
  function [y]=timeout_test(args)
    y=%t
    count=args(1).get_data['timeout_count'];
    count = count + 1;
    args(1).set_data[timeout_count=count];
    //label.set_text("count: " + str(count[0]))
    args(2).set_text["count: " + m2s(count,"%5.0f")];
  endfunction

  // starts the timeout handler
  function [y]=start_timeout_test(dialog,label)
    y=1
    if dialog.get_data['timeout_id'] == 0 then
      id = gtk_timeout_add(100,dialog.get_data['timeout_f'],list(dialog,label));
      dialog.set_data[timeout_id=id ]
    end
  endfunction

  // stops  the timeout handler
  function [y]=stop_timeout_test(dialog,label)
    y=1
    id = dialog.get_data['timeout_id'];
    if id <> 0 then
      g_source_remove(id);
      dialog.set_data[timeout_id=0];
    end
  endfunction

  // attach data to the dialog
  // since they must be shared between handlers
  // and exists when returning from this function.

  dialog.set_data[timeout_f = timeout_test];
  dialog.set_data[timeout_id = 0];
  dialog.set_data[timeout_count = 0];

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
      start_timeout_test(dialog,label);
     case 3 then
      // Stop
      stop_timeout_test(dialog,label);
    end
  end
endfunction
