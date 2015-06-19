
// Gamma Curve 

function []=demo_gamma_curve()
  win = gtkwindow_new()
  //win.connect[ "destroy", hide];
  win.connect["delete_event", demo_delete];
  win.set_title["test"];
  win.set_size_request[200, 150]
  curve = gtkgammacurve_new()
  win.add[curve]
  curve.show[]
  win.show[]
  //gtk_main()
endfunction

