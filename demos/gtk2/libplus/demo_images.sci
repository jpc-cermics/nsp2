
function progressive_prepared_callback (loader,args)
  image = args(1)
  // pixbuf = gdk_pixbuf_loader_get_pixbuf (loader);
  pixbuf = loader.get_pixbuf[];
  // Avoid displaying random memory contents, since the pixbuf
  // isn't filled in yet.
  // gdk_pixbuf_fill (pixbuf, 0xaaaaaaff);
  //gtk_image_set_from_pixbuf (GTK.IMAGE (image), pixbuf);
  image.set_from_pixbuf[pixbuf];
endfunction

function progressive_updated_callback(loader,x,y, width,height, data)
// image = GTK.WIDGET (data);
  image = args(1);
  // We know the pixbuf inside the GtkImage has changed, but the image
  // itself doesn't know this; so queue a redraw.  If we wanted to be
  // really efficient, we could use a drawing area or something
  // instead of a GtkImage, so we could control the exact position of
  // the pixbuf on the display, then we could queue a draw for only
  // the updated area of the image.
  //
  image.queue_draw[];
endfunction 


// function progressive_timeout(data)
// To be done 
// // image = GTK.WIDGET (data);
//   image = args(1);
//   // This shows off fully-paranoid error handling, so looks scary.
//   // You could factor out the error handling code into a nice separate
//   // function to make things nicer.
//   //
  
//   if (image_stream)
//     {
//       size_t bytes_read;
//       guchar buf[256];
//       GError *error = NULL;
      
//       bytes_read = fread (buf, 1, 256, image_stream);

//       if (ferror (image_stream))
// 	{
// 	  GtkWidget *dialog;
	  
// 	  dialog = gtkmessagedialog_new (GTK.WINDOW (window),
// 					   GTK.DIALOG_DESTROY_WITH_PARENT,
// 					   GTK.MESSAGE_ERROR,
// 					   GTK.BUTTONS_CLOSE,
// 					   "Failure reading image file 'alphatest.png': %s",
// 					   g_strerror (errno));

// 	  dialog.connect[  "response",
// 			    gtk_widget_destroy, NULL]

// 	  fclose (image_stream);
// 	  image_stream = NULL;

// 	  dialog.show[];
	  
// 	  load_timeout = 0;

// 	  return %f; /* uninstall the timeout */
// 	}

//       if (!gdk_pixbuf_loader_write (pixbuf_loader,
// 				    buf, bytes_read,
// 				    &error))
// 	{
// 	  GtkWidget *dialog;
	  
// 	  dialog = gtkmessagedialog_new (GTK.WINDOW (window),
// 					   GTK.DIALOG_DESTROY_WITH_PARENT,
// 					   GTK.MESSAGE_ERROR,
// 					   GTK.BUTTONS_CLOSE,
// 					   "Failed to load image: %s",
// 					   error->message);

// 	  g_error_free (error);
	  
// 	  dialog.connect[  "response",
// 			    gtk_widget_destroy, NULL]

// 	  fclose (image_stream);
// 	  image_stream = NULL;
	  
// 	  dialog.show[];

// 	  load_timeout = 0;

// 	  return %f; /* uninstall the timeout */
// 	}

//       if (feof (image_stream))
// 	{
// 	  fclose (image_stream);
// 	  image_stream = NULL;

// 	  /* Errors can happen on close, e.g. if the image
// 	   * file was truncated we'll know on close that
// 	   * it was incomplete.
// 	   */
// 	  error = NULL;
// 	  if (!gdk_pixbuf_loader_close (pixbuf_loader,
// 					&error))
// 	    {
// 	      GtkWidget *dialog;
	      
// 	      dialog = gtkmessagedialog_new (GTK.WINDOW (window),
// 					       GTK.DIALOG_DESTROY_WITH_PARENT,
// 					       GTK.MESSAGE_ERROR,
// 					       GTK.BUTTONS_CLOSE,
// 					       "Failed to load image: %s",
// 					       error->message);
	      
// 	      g_error_free (error);
	      
// 	      dialog.connect[  "response",
// 				gtk_widget_destroy, NULL]
	      
// 	      dialog.show[];

// 	      g_object_unref (pixbuf_loader);
// 	      pixbuf_loader = NULL;
	      
// 	      load_timeout = 0;
	      
// 	      return %f; /* uninstall the timeout */
// 	    }
	  
// 	  g_object_unref (pixbuf_loader);
// 	  pixbuf_loader = NULL;
// 	}
//     }
//   else
//     {
//       gchar *filename;
//       gchar *error_message = NULL;
//       GError *error = NULL; 

//       /* demo_find_file() looks in the the current directory first,
//        * so you can run gtk-demo without installing GTK, then looks
//        * in the location where the file is installed.
//        */
//       filename = demo_find_file ("alphatest.png", &error);
//       if (error)
// 	{
// 	  error_message = g_strdup (error->message);
// 	  g_error_free (error);
// 	}
//       else
// 	{
// 	  image_stream = fopen (filename, "r");
// 	  g_free (filename);

// 	  if (!image_stream)
// 	    error_message = g_strdup_printf ("Unable to open image file 'alphatest.png': %s",
// 					     g_strerror (errno));
// 	}

//       if (image_stream == NULL)
// 	{
// 	  GtkWidget *dialog;
	  
// 	  dialog = gtkmessagedialog_new (GTK.WINDOW (window),
// 					   GTK.DIALOG_DESTROY_WITH_PARENT,
// 					   GTK.MESSAGE_ERROR,
// 					   GTK.BUTTONS_CLOSE,
// 					   "%s", error_message);
// 	  g_free (error_message);

// 	  dialog.connect[  "response",
// 			    gtk_widget_destroy, NULL]
	  
// 	  dialog.show[];

// 	  load_timeout = 0;

// 	  return %f; /* uninstall the timeout */
// 	}

//       if (pixbuf_loader)
// 	{
// 	    gdk_pixbuf_loader_close (pixbuf_loader, NULL);
// 	    g_object_unref (pixbuf_loader);
// 	    pixbuf_loader = NULL;
// 	}
      
//       pixbuf_loader = gdk_pixbuf_loader_new ();
      
//       pixbuf_loader.connect[  "area_prepared",
// 			progressive_prepared_callback, image]
      
//       pixbuf_loader.connect[  "area_updated",
// 			progressive_updated_callback, image]
//     }

//   /* leave timeout installed */
//   return %t;
// }

// static void
// start_progressive_loading (GtkWidget *image)
// {
//   /* This is obviously totally contrived (we slow down loading
//    * on purpose to show how incremental loading works).
//    * The real purpose of incremental loading is the case where
//    * you are reading data from a slow source such as the network.
//    * The timeout simply simulates a slow data source by inserting
//    * pauses in the reading process.
//    */
//   load_timeout = g_timeout_add (150,
// 				progressive_timeout,
// 				image);
// }

// static void
// cleanup_callback (GtkObject *object,
// 		  gpointer   data)
// {
//   if (load_timeout)
//     {
//       g_source_remove (load_timeout);
//       load_timeout = 0;
//     }
  
//   if (pixbuf_loader)
//     {
//       gdk_pixbuf_loader_close (pixbuf_loader, NULL);
//       g_object_unref (pixbuf_loader);
//       pixbuf_loader = NULL;
//     }

//   if (image_stream)
//     fclose (image_stream);
//   image_stream = NULL;
// }



function toggle_sensitivity_callback(togglebutton,args)
  container= args(1);
  ch = container.get_children[];
  flag= ~ togglebutton.get_active[];
  for child=ch do 
    child.set_sensitive[flag];
  end
  togglebutton.set_sensitive[%t]
endfunction

  

function demo_images() 
  window = gtkwindow_new()
  window.connect[ "delete_event", demo_delete];
  window.set_title["Images"];
  //window.connect[  "destroy",  gtk_widget_destroyed, &window]
  //window.connect[  "destroy",	cleanup_callback, NULL]
  window.set_border_width[  8]
  vbox = gtkvbox_new(homogeneous=%f,spacing=8);
  vbox.set_border_width[  8]
  window.add[  vbox]

  label = gtklabel_new();
  label.set_markup[  "<u>Image loaded from a file</u>"]
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
      
  frame = gtkframe_new();
  frame.set_shadow_type[GTK.SHADOW_IN];
  // The alignment keeps the frame from growing when users resize
  //the window
  align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
  align.add[  frame]
  vbox.pack_start[ align,expand=%f,fill=%f,padding=0]
  logo = getenv('NSP')+'/demos/gtk2/libplus/gtk-logo-rgb.gif";
  pixbuf = gdk_pixbuf_new_from_file(logo);
  image = gtkimage_new("pixbuf",pixbuf);
  frame.add[image]
  // Animation 
      
  label = gtklabel_new();
  label.set_markup[ "<u>Animation loaded from a file</u>"]
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
    
  frame = gtkframe_new();
  frame.set_shadow_type[GTK.SHADOW_IN];
  // The alignment keeps the frame from growing when users resize
  // the window
  align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
  align.add[  frame]
  vbox.pack_start[ align,expand=%f,fill=%f,padding=0]
  flop = getenv('NSP')+'/demos/gtk2/libplus/floppybuddy.gif';
  image = gtkimage_new("file",flop);
  frame.add[  image]
  
  // Progressive 
            
  label = gtklabel_new()
  label.set_markup[ "<u>Progressive image loading</u>"]
  vbox.pack_start[ label,expand=%f,fill=%f,padding=0]
      
  frame = gtkframe_new();
  frame.set_shadow_type[GTK.SHADOW_IN];
  // The alignment keeps the frame from growing when users resize
  // the window
  align = gtkalignment_new(xalign=0.5,yalign=0.5,xscale=0,yscale=0);
  align.add[  frame]
  vbox.pack_start[ align,expand=%f,fill=%f,padding=0]
    
  // Create an empty image for now; the progressive loader
  // will create the pixbuf and fill it in.
  image = gtkimage_new("pixbuf");
  frame.add[  image]
  // FIXME: XXXXX  start_progressive_loading(image);
  // Sensitivity control */
  button = gtktogglebutton_new(mnemonic="_Insensitive");
  vbox.pack_start[ button,expand=%f,fill=%f,padding=0]
  button.connect["toggled",  toggle_sensitivity_callback,list(vbox)]
  window.show_all[]
endfunction
