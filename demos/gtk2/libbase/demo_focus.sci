// Focus test 

function [l,table]=make_focus_table(flag)
  table = gtktable_new(rows=5,columns=5,homogeneous=%f);
  tf =  ior(GTK.EXPAND,GTK.FILL); 
  count=1
  l= list();
  for i=0:2 
    for j=0:2 
       if flag then pos= j+3*i;else pos=  i+3*j; end 
       if modulo((i + j),2)==0 
          widget = gtkentry_new ();
	  widget.set_text[sprintf("Foo %d",pos+1)];
       else
          widget = gtkbutton_new(label=sprintf("Foo %d",pos+1));
       end 
       l($+1) =  widget
       table.attach[widget, i, i + 1, j, j + 1,xoptions=tf,yoptions=tf,xpadding=5,ypadding=5];
    end
  end 
endfunction 

function demo_focus() 
  window = gtkdialog_new(title="Keyboard focus navigation", 
  buttons= ["gtk-close"]);
  window.connect["destroy",hide]
  window.connect[  "response",hide ]
  window.set_title[  "Keyboard Focus Navigation"]
  
  frame = gtkframe_new(label="Weird tab focus chain");
  window.vbox.pack_start[ frame,expand=%t,fill=%t,padding=0];
  [l,table] = make_focus_table(%t);
  frame.add[  table]
  table.set_focus_chain[ l]
  frame = gtkframe_new(label="Default tab focus chain");
  window.vbox.pack_start[ frame,expand=%t,fill=%t,padding=0];
  [l,table] = make_focus_table(%f);
  frame.add[ table]      
  window.show_all[];
endfunction 

