// GtkStatusBar

function []=demo_statusbar()
//
  dialog = gtk_dialog_new()
  dialog.connect[ "delete_event", demo_delete];
  dialog.set_title["Status Bar Test"];

  statusbar = gtk_statusbar_new()
  vbox = dialog.get_content_area[];
  vbox.add[statusbar];
  statusbar.show[]
  c = statusbar.get_context_id["test"];
  statusbar.push[c,"First message"]
  statusbar.set_data[statusbar_data=0];

  function []=pop_statusbar_test(statusbar,arg)
    statusbar.pop[arg];
  endfunction

  function []=push_statusbar_test(statusbar,arg)
    count= statusbar.get_data['statusbar_data'];
    statusbar.set_data[statusbar_data=count+1];
    statusbar.push[arg,"Message "+m2s(count,"%5.0f")];
  endfunction

  dialog.add_button["Pop",1];
  dialog.add_button["Push",2];
  dialog.add_button["Close",3];
  dialog.show[];

  while %t then
    response = dialog.run[]
    select response
     case 1 then pop_statusbar_test(statusbar,c);
     case 2 then push_statusbar_test(statusbar,c);
     case 3 then; dialog.destroy[];break;
    end
  end

endfunction
