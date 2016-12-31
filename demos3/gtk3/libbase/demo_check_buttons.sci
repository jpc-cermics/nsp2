// GtkChekButton OK
//-----------------------------------------------

function []=demo_check_buttons()

  function grid = create_widget_grid ()
    grid = gtk_grid_new ();
    for i=1:5
      for j=1:5
	if i == 1 && j == 1 then
	  widget=%f
	elseif i == 1
	  tmp = sprintf("%d",j);
	  widget = gtk_label_new (str=tmp);
	elseif j == 1
	  tmp = sprintf("%c", ascii(ascii('A') + i ));
	  widget = gtk_label_new (str=tmp);
	else
	  widget = gtk_check_button_new();
	end
	if ~widget.equal[%f] then
	  widget.show[];
	  grid.attach[widget, i, j, 1, 1];
	end
      end
    end
    grid.show[];
  endfunction

  win = gtk_window_new()
  win.connect["delete_event", demo_delete];
  win.set_name["check buttons"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2]
  box2.show[]
  for i = 0:3
    button = gtk_check_button_new(label="button" +  m2s(i,expand="%0.f"))
    box2.pack_start[button]
    button.show[]
  end
  button = gtk_check_button_new(label="inconsistent");
  button.set_inconsistent[%t];
  box2.pack_start[button]
  button.show[]

  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand=%f,fill=%t,padding=0]
  separator.show[]

  table = create_widget_grid ();
  table.set_border_width[10];
  table.show[];
  box1.pack_start[table,expand=%f,fill=%t,padding=0];

  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand=%f,fill=%t,padding=0]
  separator.show[]

  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand=%f,fill=%t,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked", button_destroy_win,list(win)];
  box2.pack_start[button]
  //button.set_flags[GTK.CAN_DEFAULT]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction
