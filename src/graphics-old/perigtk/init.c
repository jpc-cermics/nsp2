/* Nsp
 * Copyright (C) 1998-2008 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Graphic library
 * jpc@cermics.enpc.fr
 *
 * Initialize a graphic window
 * Common code for Gtk/OpenGL and Cairo 
 *--------------------------------------------------------------------------*/

/*
 * initgraphic : initialize graphic window
 * If v2 is not a nul pointer *v2 is the window number to create 
 * EntryCounter is used to check for first Entry + to know the next 
 * available window number 
 */

static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box,int *v2,
			    int *wdim,int *wpdim,double *viewport_pos,int *wpos);


static void initgraphic(char *string, int *v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos,char mode)
{ 
  nsp_initgraphic(string,NULL,NULL,v2,wdim,wpdim,viewport_pos,wpos);
}

/* used when a graphic window is to be inserted in a more complex 
 * widget hierarchy 
 */

#ifdef PERIGL 
#define nsp_graphic_new nsp_graphic_new_gl
#endif 
#ifdef PERICAIRO 
#define nsp_graphic_new nsp_graphic_new_cairo
#endif 

int nsp_graphic_new(GtkWidget *win,GtkWidget *box, int v2,int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{ 
  nsp_initgraphic("",win,box,&v2,wdim,wpdim,viewport_pos,wpos);
  return  nsp_get_win_counter()-1;
}

#ifdef PERIGTK 
/* this should be  moved in windows: keep track of window ids
 */
static int EntryCounter = 0;
int nsp_get_win_counter() { return EntryCounter;};
void nsp_set_win_counter(int n) {  EntryCounter=Max(EntryCounter,n); EntryCounter++;}
#endif 

static void nsp_initgraphic(char *string,GtkWidget *win,GtkWidget *box,int *v2,
			    int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  int i;
  static int first = 0;
  GdkColor white={0,0,0,0};
  GdkColor black={0,65535,65535,65535};
  BCG *NewXgc ;
  /* Attention ici on peut faire deux fenetre de meme numéro à régler ? XXXXX */
  int WinNum = ( v2 != (int *) NULL && *v2 != -1 ) ? *v2 : nsp_get_win_counter();
  gui_private *private ; 
  if ( ( private = MALLOC(sizeof(gui_private)))== NULL) 
    {
      Sciprintf("initgraphics: running out of memory \n");
      return;
    }
  /* default values  */
  private->colors=NULL;
  private->colormap=NULL;
  private->window=NULL;		
  private->drawing=NULL;           
  private->scrolled=NULL;          
  private->CinfoW =NULL;           
  private->vbox=NULL;              
  private->menubar=NULL;
  private->menu_entries=NULL;
  private->pixmap=NULL;       
  private->extra_pixmap=NULL;
  private->drawable=NULL;
  private->wgc=NULL;
  private->stdgc=NULL;
  private->gcursor=NULL;      
  private->ccursor=NULL;      
  private->font=NULL;
  private->resize = 0; /* do not remove !! */
  private->in_expose= FALSE;
  private->protect= FALSE;
  private->draw= FALSE;
  private->layout  = NULL;
  private->mark_layout  = NULL;
  private->context = NULL;
  private->desc = NULL;
  private->mark_desc = NULL;
  private->gcol_bg = white;
  private->gcol_fg = black;
#ifdef PERIGL 
  private->ft2_context = NULL;
  private->gdk_only= FALSE;
  private->gl_only= FALSE;
  private->gldrawable= NULL;
  private->glcontext = NULL;
#endif 
#ifdef PERICAIRO 
  private->cairo_cr = NULL;
#endif 

  if (( NewXgc = window_list_new(private) ) == (BCG *) 0) 
    {
      Sciprintf("initgraphics: unable to alloc\n");
      return;
    }

  NewXgc->CurWindow = WinNum;
  NewXgc->record_flag = TRUE; /* default mode is to record plots */
  NewXgc->plots = NULL;
  NewXgc->last_plot = NULL;
  /* the graphic engine associated to this graphic window */  
#ifdef PERIGL 
  NewXgc->graphic_engine = &GL_gengine ;
#else 
#ifdef PERICAIRO 
  NewXgc->graphic_engine = &Cairo_gengine;
#else   
  NewXgc->graphic_engine = &Gtk_gengine;
#endif 
#endif 
  start_sci_gtk(); /* be sure that gtk is started */

  if (first == 0)
    {
      maxcol = 1 << 16; /* FIXME XXXXX : to be changed */
      first++;
    }

  /* recheck with valgrind 
   * valgrind detecte des variables non initialisees dans 
   * initialize a cause d'initialisation croisées 
   * d'ou des valeurs par defaut ...
   * A tester pour faire les choses dans l'ordre dans initialize 
   */
  NewXgc->fontId=0 ;
  NewXgc->fontSize=0 ;
  NewXgc->CurHardSymb=0;
  NewXgc->CurHardSymbSize=0;
  NewXgc->CurLineWidth=0;
  NewXgc->CurPattern=0;
  NewXgc->CurColor=0;
  NewXgc->CurPixmapStatus=0;
  NewXgc->CurVectorStyle=0;
  NewXgc->CurDrawFunction=0;
  NewXgc->ClipRegionSet=0;
  NewXgc->CurDashStyle=0;
  NewXgc->IDLastPattern=0;
  NewXgc->Numcolors=0; 
  NewXgc->NumBackground=0;
  NewXgc->NumForeground=0;
  NewXgc->NumHidden3d=0; 
  NewXgc->Autoclear=0;
  /* default colormap not instaled */
  NewXgc->CmapFlag = -1; 
  /* default resize not yet defined */
  NewXgc->CurResizeStatus = -1; /* to be sure that next will initialize */
  NewXgc->CurColorStatus = -1;  /* to be sure that next will initialize */
  for ( i = 0 ; i < 4 ; i++) NewXgc->zrect[i]=0;

  if ( win != NULL )
    {
      gtk_nsp_graphic_window(FALSE,NewXgc,"unix:0",win,box,wdim,wpdim,viewport_pos,wpos);
    }
  else 
    {
      gtk_nsp_graphic_window(TRUE,NewXgc,"unix:0",NULL,NULL,wdim,wpdim,viewport_pos,wpos);
    }

  

  /* next values are to be set since initialize_gc 
   * action depend on the current state defined by these 
   * variables. For pixmap, resizestatus and colorstatus 
   * initialize performs a switch from old value to new value 
   */
  /* Default value is without Pixmap */
  NewXgc->CurPixmapStatus = 0; 
#ifdef PERIGL 
  NewXgc->private->drawable = (GdkDrawable *) NewXgc->private->drawing->window;  
#endif 

  nsp_fonts_initialize(NewXgc);/* initialize a pango_layout */

  NewXgc->graphic_engine->scale->initialize_gc(NewXgc);
  /* Attention ce qui est ici doit pas etre rejoué 
   * on l'enleve donc de initialize_gc
   */
  NewXgc->graphic_engine->xset_pixmapOn(NewXgc,0);
  NewXgc->graphic_engine->xset_wresize(NewXgc,1);
  /* now initialize the scale list : already performed in window_list_new */
  /* NewXgc->scales = NULL; xgc_add_default_scale(NewXgc);*/
  nsp_set_win_counter(WinNum);
  gdk_flush();
}

/*
 * partial or full creation of a graphic nsp widget 
 * if is_top == FALSE a partial widget (vbox) is created 
 */

/* FIXME: for cairo 
 *        gint gtk_cairo_set_x11_cr(GtkCairo *gtkcairo,gint width,gint height)
 *        is added by me on my local gtkcairo installation. 
 *        the code should be updated and tested with a more recent version 
 *        of gtk which includes cairo.
 *        this function must be called when size is changed. I use this function 
 *        because I do not use the paint method.
 */

static void gtk_nsp_graphic_window(int is_top, BCG *dd, char *dsp,GtkWidget *win,GtkWidget *box,
				   int *wdim,int *wpdim,double *viewport_pos,int *wpos)
{
  static char gwin_name[100];
  gint iw, ih;
#ifdef PERIGL
  guint mode = GDK_GL_MODE_RGB | GDK_GL_MODE_DEPTH | GDK_GL_MODE_STENCIL;
  GdkGLConfig *glconfig;
#endif 
  GtkWidget *scrolled_window;
  GtkWidget *vbox;

#ifdef PERIGL
  glconfig = gdk_gl_config_new_by_mode (mode  | GDK_GL_MODE_DOUBLE) ;
  if (glconfig == NULL)
    {
      glconfig = gdk_gl_config_new_by_mode (mode);
      if (glconfig == NULL)
	{
	  /* FIXME XXX */
	  g_print ("*** No appropriate OpenGL-capable visual found.\n");
	  return;
	}
    }
  /* examine_gl_config_attrib(glconfig); */
#endif 

  /* initialise pointers */
  dd->private->drawing = NULL;
  dd->private->wgc = NULL;
  dd->private->gcursor = NULL;
  dd->private->ccursor = NULL;
  /* gdk_rgb_init(); */
  /* gtk_widget_push_visual(gdk_rgb_get_visual()); */
  /* gtk_widget_push_colormap(gdk_rgb_get_cmap()); */

  /* create window etc */
  if ( wdim != NULL ) 
    {
      dd->CWindowWidth = iw = wdim[0] ; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = wdim[1]; /*  pixelHeight(); */
    }
  else 
    {
      dd->CWindowWidth = iw = 600 ; /*  / pixelWidth(); */
      dd->CWindowHeight = ih = 400; /*  pixelHeight(); */
    }

  if ( is_top == TRUE ) 
    {
      dd->private->window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
      sprintf( gwin_name, "Graphic Window %d", dd->CurWindow );
      gtk_window_set_title (GTK_WINDOW (dd->private->window),  gwin_name);
      /* gtk_window_set_policy(GTK_WINDOW(dd->private->window), TRUE, TRUE, FALSE); */
      gtk_window_set_resizable(GTK_WINDOW(dd->private->window), TRUE);
      gtk_widget_realize(dd->private->window);
      vbox = gtk_vbox_new (FALSE, 0);
      gtk_container_add (GTK_CONTAINER (dd->private->window), vbox);
    }
  else 
    {
      dd->private->window = win ;
      sprintf( gwin_name, "Graphic Window %d", dd->CurWindow );
      gtk_window_set_title (GTK_WINDOW (dd->private->window),  gwin_name);
      /*  gtk_window_set_policy(GTK_WINDOW(dd->private->window), TRUE, TRUE, FALSE); */
      gtk_window_set_resizable(GTK_WINDOW(dd->private->window), TRUE);
      /* gtk_widget_realize(dd->private->window);*/
      vbox = gtk_vbox_new (FALSE, 0);
      gtk_container_add (GTK_CONTAINER(box) , vbox);
    }

  /* gtk_widget_show (vbox); */

  dd->private->vbox =  gtk_vbox_new (FALSE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), dd->private->vbox, FALSE, TRUE, 0);

  dd->private->menu_entries = graphic_initial_menu(dd->CurWindow );
  dd->private->menubar = NULL;
  create_graphic_window_menu(dd);

  dd->private->CinfoW = gtk_statusbar_new ();
  gtk_box_pack_start (GTK_BOX (vbox), dd->private->CinfoW, FALSE, TRUE, 0);

  /* create a new scrolled window. */
  dd->private->scrolled = scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_set_border_width (GTK_CONTAINER (scrolled_window),0);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  /* fix min size of the scrolled window */
  if ( wpdim != NULL) 
    gtk_widget_set_size_request (scrolled_window,wpdim[0],wpdim[1]);
  else
    gtk_widget_set_size_request (scrolled_window,iw+10,ih+10);

  /* place and realize the scrolled window  */

  gtk_box_pack_start (GTK_BOX (vbox), scrolled_window, TRUE, TRUE, 0);

  if ( is_top == TRUE ) 
    gtk_widget_realize(scrolled_window);
  else
    gtk_widget_show(scrolled_window);

  if ( viewport_pos != NULL )
    {
      gtk_adjustment_set_value( gtk_scrolled_window_get_hadjustment (GTK_SCROLLED_WINDOW (scrolled_window)),
				(gfloat) viewport_pos[0]);
      gtk_adjustment_set_value( gtk_scrolled_window_get_vadjustment (GTK_SCROLLED_WINDOW (scrolled_window)),
				(gfloat) viewport_pos[1]);      
    }

/* new pericairo mode for 2.8 */
#ifdef PERICAIROXX
  /* drawing is here a cairo_drawing */
  dd->private->cairo_drawing = gtk_cairo_new (); 
  /* XXX removing default Double buffering 
   * since during expose we draw on our own surface 
   * which is attached to the graphic window. 
   */
  gtk_widget_set_double_buffered (dd->private->cairo_drawing ,FALSE);
  dd->private->drawing =   dd->private->cairo_drawing ;
#else 
  /* create private->drawingarea */
  dd->private->drawing = gtk_drawing_area_new();
#ifdef PERIGL
  /* Set OpenGL-capability to the widget */
  gtk_widget_set_gl_capability (dd->private->drawing,
				glconfig,
				NULL,
				TRUE,
				GDK_GL_RGBA_TYPE);
#else
  /* we use our own double buffer */
  gtk_widget_set_double_buffered (dd->private->drawing ,FALSE);
#endif /* PERIGL */
#endif /* PERICAIRO */

  /* gtk_widget_set_usize (GTK_WIDGET (dd->private->cairo_drawing),600,400); */

  g_signal_connect(GTK_OBJECT(dd->private->drawing), "button-press-event",
		   G_CALLBACK(locator_button_press), (gpointer) dd);
  g_signal_connect(GTK_OBJECT(dd->private->drawing), "button-release-event",
		   G_CALLBACK(locator_button_release), (gpointer) dd);
  g_signal_connect(GTK_OBJECT(dd->private->drawing), "motion-notify-event",
		   G_CALLBACK(locator_button_motion), (gpointer) dd);
  g_signal_connect(GTK_OBJECT(dd->private->drawing), "realize",
		   G_CALLBACK(realize_event), (gpointer) dd);

  gtk_widget_set_events(dd->private->drawing, GDK_EXPOSURE_MASK 
			| GDK_BUTTON_PRESS_MASK 
			| GDK_BUTTON_RELEASE_MASK
			| GDK_POINTER_MOTION_MASK
			| GDK_POINTER_MOTION_HINT_MASK
			| GDK_LEAVE_NOTIFY_MASK );

  /* private->drawingarea properties */
  /* min size of the graphic window */
  gtk_widget_set_size_request(GTK_WIDGET (dd->private->drawing), iw, ih);

  /* place and realize the private->drawing area */
  gtk_scrolled_window_add_with_viewport ( GTK_SCROLLED_WINDOW (scrolled_window),
					  GTK_WIDGET (dd->private->drawing));

  if ( is_top == TRUE )
    gtk_widget_realize(dd->private->drawing);
  else
    gtk_widget_show(dd->private->drawing);

  /* connect to signal handlers, etc */
  g_signal_connect(GTK_OBJECT(dd->private->drawing), "configure_event",
		   G_CALLBACK(configure_event), (gpointer) dd);

  g_signal_connect(GTK_OBJECT(dd->private->drawing), "expose_event",
		   G_CALLBACK(expose_event), (gpointer) dd);
  
  /* 
   *  g_signal_connect (G_OBJECT (dd->private->cairo_drawing), "paint", 
   *  G_CALLBACK (cairo_paint),(gpointer) dd );
   */

  g_signal_connect(GTK_OBJECT(dd->private->window), "destroy",
		   G_CALLBACK(sci_destroy_window), (gpointer) dd);
  
  g_signal_connect(GTK_OBJECT(dd->private->window), "delete_event",
		   G_CALLBACK(sci_delete_window), (gpointer) dd);

  g_signal_connect (GTK_OBJECT(dd->private->window), "key_press_event",
		    G_CALLBACK(key_press_event), (gpointer) dd);

  /* show everything */

  if ( is_top == TRUE ) 
    {
      /* create offscreen drawable : Already done in the realize_event 
       */
      if ( wpos != NULL ) 
	gtk_window_move (GTK_WINDOW(dd->private->window),wpos[0],wpos[1]);
      gtk_widget_realize(dd->private->window);
      gtk_widget_show_all(dd->private->window);
    }
  else 
    {
      /* we need here to realize the dd->private->drawing 
       * this will create offscreen drawable : in realize_event
       * and the initialize_gc 
       */
      gtk_widget_realize(dd->private->drawing);
    }

  /* let other widgets use the default colour settings */
  /* gtk_widget_pop_visual(); */
  /* gtk_widget_pop_colormap(); */
}

