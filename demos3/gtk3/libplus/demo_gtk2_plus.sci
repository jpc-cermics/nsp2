
function demo_gtk2_plus()
  B = hash_create(20);
  B('display and screen')=demo_display_screen,
  B('image from drawable')=demo_image_from_drawable,
  B('images1')=demo_images1,
  B('images2')=demo_images2,
  B('images3')=demo_images3,
  B('labels')=demo_labels,
  B('nsp graphic widget 1')=demo_nsp_graphic_widget,
  B('nsp graphic widget 2')=demo_dyn_graph,
  
  win = gtk_window_new()
  win.set_title["main window"]
  win.set_size_request[200, 400]
  //win.set_uposition[20,20]
  //win.connect["destroy",hide];
  win.connect["delete_event", demo_delete];
  win.set_title["gtktest"];
  box1 = gtk_box_new("vertical",spacing=0)
  win.add[box1]
  box1.show[]
  scrolled_window = gtk_scrolled_window_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  box1.pack_start[scrolled_window, expand=%t,fill=%t,padding=0]
  scrolled_window.show[]
  box2 = gtk_box_new("vertical",spacing=0)
  box2.set_border_width[0]
  scrolled_window.add[box2];
  box2.show[]
  k = B.__keys;
  Bk = sort(k,'g','i');
  for i1 = 1:size(Bk,1)
    button = gtk_button_new(label=Bk(i1,1))
    func= B.find[Bk(i1,1)]
    if is(func,%types.PList) then
      button.connect["clicked",func];
    else
      button.set_sensitive[%f]
    end
    box2.pack_start[button]
    button.show[]
  end
  separator = gtk_separator_new("horizontal")
  box1.pack_start[separator,expand= %f,fill= %f,padding=0]
  separator.show[]
  box2 = gtk_box_new("vertical",spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,expand=%f,fill=%f,padding=0]
  box2.show[]
  button = gtk_button_new(label="close")
  button.connect["clicked",button_destroy_win,list(win)];
  //button.set_flags[GTK.CAN_DEFAULT]
  box2.pack_start[button]
  button.set_can_default[%t]
  button.grab_default[]
  button.show[]
  win.show[]
  //gtk_main()
endfunction

function [y]=demo_delete(win, event)
  // used when
  y=%f; // if false then destroy is performed
        // if true then destroy is not done
  if y==%t then
    //printf("in delete returning true (no destroy)\n");
  else
    //    printf("in delete returning false (destroy)\n");
  end
endfunction

function destroy(win, event)
// called when window is destroyed
// printf("in destroy \n");
endfunction

function button_destroy_win(but,args)
  // button handler which destroy args(1)
  //printf("in win destroy \n");
  args(1).destroy[];
endfunction
