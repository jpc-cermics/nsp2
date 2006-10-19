
function demo_gtk2_plus()
  B = hash_create(20);
  B('colorsel')=demo_colorsel,
  B('display and screen')=demo_display_screen,
  B('image from drawable')=demo_image_from_drawable,
  B('images1')=demo_images1,
  B('images')=demo_images,
  B('item factory')=demo_item_factory,
  B('labels')=demo_labels,
  B('liststore1')=demo_liststore1,
  B('liststore')=demo_liststore,
  B('menus1')=demo_menus1,
  B('nsp graphic widget 1')=demo_nsp_graphic_widget,
  B('nsp graphic widget 2')=demo_dyn_graph,
  B('sizegroup')=demo_sizegroup,
  B('stock browser')=demo_stock_browser,
  B('textview')=demo_textview,
  B('treestore1')=demo_treestore1,
  B('treestore')=demo_treestore,
  B('treeview editable')=demo_treeview_editable,
  
  win = gtkwindow_new()
  win.set_title["main window"]
  win.set_size_request[200, 400]	
  //win.set_uposition[20,20]
  //win.connect["destroy",hide];
  win.connect["delete_event", demo_delete];
  win.set_title["gtktest"];
  box1 = gtkvbox_new(homogeneous=%f,spacing=0)
  win.add[box1]
  box1.show[]
  scrolled_window = gtkscrolledwindow_new()
  scrolled_window.set_border_width[10]
  scrolled_window.set_policy[GTK.POLICY_AUTOMATIC,GTK.POLICY_AUTOMATIC];
  box1.pack_start[scrolled_window, expand=%t,fill=%t,padding=0] 	
  scrolled_window.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=0)
  box2.set_border_width[0]
  scrolled_window.add_with_viewport[box2];
  box2.show[]
  k = B.__keys;
  Bk = sort(k,'g','i');
  for i1 = 1:size(Bk,1)
    button = gtkbutton_new(label=Bk(i1,1))
    func= B.find[Bk(i1,1)]
    if is(func,%types.PList) then 
      button.connect["clicked",func];
    else
      button.set_sensitive[%f]
    end 
    box2.pack_start[button]
    button.show[]
  end 
  separator = gtkhseparator_new()
  box1.pack_start[separator,expand= %f,fill= %f,padding=0]
  separator.show[]
  box2 = gtkvbox_new(homogeneous=%f,spacing=10)
  box2.set_border_width[10]
  box1.pack_start[box2,expand= %f,expand=%f,fill=%f,padding=0]
  box2.show[]
  button = gtkbutton_new(label="close")
  button.connect["clicked",button_destroy_win,list(win)];
  button.set_flags[GTK.CAN_DEFAULT]
  box2.pack_start[button]
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
     printf("in delete returning true (no destroy)\n");
  else
     printf("in delete returning false (destroy)\n");
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







