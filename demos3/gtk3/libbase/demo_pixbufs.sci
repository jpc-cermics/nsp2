// Pixbufs
//
// A GdkPixbuf represents an image, normally in RGB or RGBA format.
// Pixbufs are normally used to load files from disk and perform
// image scaling.
//
// This demo is not all that educational, but looks cool. It was written
// by Extreme Pixbuf Hacker Federico Mena Quintero. It also shows
// off how to use GtkDrawingArea to do a simple animation.
//
// Look at the Image demo for additional pixbuf usage examples.
//

//  Loads the images for the demo and returns whether the operation succeeded  


function window = demo_pixbufs (do_widget)

  function [images,background]=load_pixbufs()

    dir = getenv('NSP')+"/demos3/gtk3/libbase/demo_pixbufs/";
    background = gdk_pixbuf_new_from_file (dir + "background.jpg")
    
    back_width = background.get_width[];
    back_height = background.get_height[];
    
    image_names =[ "apple-red.png",
		   "gnome-applets.png",
		   "gnome-calendar.png",
		   "gnome-foot.png",
		   "gnome-gmush.png",
		   "gnome-gimp.png",
		   "gnome-gsame.png",
		   "gnu-keys.png"];
    images={}
    for i=1:size(image_names,'*') 
      images{i} = gdk_pixbuf_new_from_file (dir + image_names[i]);
    end
  endfunction

  //  Expose callback for the drawing area  

  function y= draw_cb (widget, cr, data)
    frame=data;
    gdk_cairo_set_source_pixbuf (cr, frame, 0, 0);
    cairo_paint (cr);
    y=%t;
  endfunction 

  //  Handler to regenerate the frame  
  function y = on_tick (widget, frame_clock, data)
    background= data(1);
    images = data(2)
    frame = data(3)
    start_time = frame.get_data['start_time'];
    
    CYCLE_TIME=3000000 //  3 seconds  

    back_width = background.get_width[];
    back_height = background.get_height[];

    background.copy_area[ 0, 0, back_width, back_height, frame, 0, 0];
    
    if start_time == 0 then 
      start_time = frame_clock.get_frame_time[];
      frame.set_data[start_time=start_time];
    end
    
    current_time = frame_clock.get_frame_time[];
    
    f = mod(current_time - start_time, CYCLE_TIME) / CYCLE_TIME;
    
    xmid = back_width / 2.0;
    ymid = back_height / 2.0;

    radius = min (xmid, ymid) / 2.0;
    
    for i = 1:size(images,'*')
      
      ang = 2.0 * %pi * i / size(images,'*') - f * 2.0 * %pi;
      image = images{i};
      iw = image.get_width[];
      ih = image.get_height[];
      
      r = radius + (radius / 3.0) * sin (f * 2.0 * %pi);

      xpos = floor (xmid + r * cos (ang) - iw / 2.0 + 0.5);
      ypos = floor (ymid + r * sin (ang) - ih / 2.0 + 0.5);

      k = (i == 1)*sin (f * 2.0 * %pi)+ (i<>1)*cos (f * 2.0 * %pi);
      k = 2.0 * k * k;
      k = max (0.25, k);

      r1 = gdk_rectangle_new([xpos,  ypos, iw * k,  ih * k]);
      r2 = gdk_rectangle_new([0,  0, back_width, back_height]);
      [ok ,dest]= r1.intersect[r2];
      if  ok then 
	pos= (i == 1)* max (127, abs (255 * sin (f * 2.0 * %pi))) + ...
	     (i <> 1)* max(127, abs (255 * cos (f * 2.0 * %pi)));
	image.composite[frame, dest.x, dest.y, dest.width, dest.height,...
			xpos, ypos, k, k, GDK.INTERP_NEAREST, pos];
      end
    end
    widget.queue_draw[];
    y = %t; // G_SOURCE_CONTINUE;
  endfunction 

  window = gtk_window_new (type= GTK.WINDOW_TOPLEVEL);
  if nargin >=1 then 
    window.set_screen[ do_widget.get_screen []];
  end
  window.set_title[ "Pixbufs"];
  window.set_resizable[%f];

  // window.connect[ "destroy", gtk_widget_destroyed, &window);
  
  [images,background]= load_pixbufs();
  back_width = background.get_width[];
  back_height = background.get_height[];
  
  window.set_size_request[back_width, back_height];
  
  frame = gdk_pixbuf_new (GDK.COLORSPACE_RGB, %f, 8, back_width, back_height);
  frame.set_data[start_time=0];
  da = gtk_drawing_area_new ();
  da.connect[ "draw", draw_cb,frame];
  window.add[da];
  da.add_tick_callback[on_tick,list(background,images,frame)];
  window.show_all[];
endfunction


