// Simple Hello World example in Nsp 

function hello(arg) 
  print('In hello handler');
endfunction 

function destroy(arg) 
  print('In destroy handler');
endfunction

function [y]=delete_event(win, event) 
  print('In destroy handler');
  // don't destroy window 
  y=%t;
  //win.hide()
endfunction


// this block creates our main application window

window = gtkwindow_new();
window.connect["destroy", destroy]
window.connect["delete_event", delete_event]
window.set_border_width[10]

// this block creates our button and places it within the window
button = gtkbutton_new(mnemonic="Hello World")
//button = gtkbutton_new(stock="gtk-ok")
// connects the 'hello' function to the clicked signal from the button
button.connect["clicked", hello];
window.add[button]
button.show[]
//  as the button is within the window this also shows the window 
window.show_all[]


