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
 * store a list of graphic contexts 
 * this is gui independant the gui dependant part is 
 * delegated to private member of winxgc  
 *--------------------------------------------------------------------------*/

#include "nsp/graphics/Graphics.h" 

#ifdef  WITH_GTKGLEXT 
extern Gengine GL_gengine;
#endif 

extern int window_list_check_top(BCG *,void *) ;

/**
 * WindowList: 
 * @winxgc: graphic context #BCG
 * @next: next window. 
 * @prev: previous window.
 *
 * A doubly linked list of graphic context #BCG of graphic windows 
 * 
 */

typedef struct _window_list WindowList;

struct  _window_list
{
  BCG winxgc;
  WindowList *next;
  WindowList *prev;
} ;

static WindowList *The_List  = (WindowList *) NULL; 

static int set_window_scale(int win,window_scale_list *scale);
static window_scale_list *check_subwin_wcscale( window_scale_list *listptr, double subwin_rect[4]);
static window_scale_list *new_wcscale ( window_scale_list *val);
static WindowList  *window_list_search_w_int(WindowList *listptr, int i);
static int window_list_search_topelevel_int(WindowList *,void *,int);
static void scale_copy (window_scale_list *s1, window_scale_list *s2);
static window_scale_list *new_wcscale ( window_scale_list *val);
static int same_subwin (double lsubwin_rect[4],double subwin_rect[4]);

/**
 * check_graphic_window
 * 
 * returns the BCG structure associated to 
 * the current graphic window. If no current graphic 
 * window exists, one is created 
 * 
 * Return value: the current BCG to be used 
 **/

extern Gengine Gtk_gengine ; /* XXXXX */

BCG *check_graphic_window(void)
{
  BCG *loc =  window_list_get_first();
  if ( loc == NULL ) 
    {
#ifdef THREAD_VERSION
      gdk_threads_enter();
#endif
      Gtk_gengine.xset_curwin(0,TRUE);
#ifdef THREAD_VERSION
      gdk_threads_leave();
#endif
    }
  return  window_list_get_first();
} 


/**
 * set_graphic_window:
 * @num: an integer 
 * 
 * The current graphic window is set to window @num 
 * If the window does not exist it is created.
 * 
 * Return value: the new graphic context associated to window @num.
 **/

BCG *set_graphic_window(int num) 
{
  Gtk_gengine.xset_curwin(Max(0,num),TRUE);
  return  window_list_get_first();
}


/**
 * window_list_new:
 * @private: extra information for #BCG 
 * 
 * Creates a new graphix context. The scales are also 
 * initialized with default scale.
 * 
 * Return value: a new graphic context 
 **/

BCG *window_list_new(void *private)
{
  WindowList *loc =  (WindowList *) MALLOC (sizeof(WindowList));
  if ( loc == NULL) 
    {
      Sciprintf("window_list_new_entry_int: running out of memory\n");
      return NULL;
    }
  else 
    { 
      loc->winxgc.scales = NULL;
      if ( xgc_add_default_scale(&loc->winxgc) == FAIL)
	{
	  FREE(loc);
	  Sciprintf("window_list_new_entry_int: running out of memory\n");
	  return NULL;
	}
      loc->winxgc.private = private ;
      loc->winxgc.CurWindow = 0;
      loc->winxgc.CmapFlag  = 1;
      loc->winxgc.EventHandler[0] = '\0';
      loc->winxgc.queue.in=0;
      loc->winxgc.queue.out=0;
      loc->winxgc.queue.size=MaxCB;
      loc->next = The_List ;
      if ( The_List != NULL) The_List->prev = loc;
      loc->prev = NULL;
      The_List = loc ;
    }
  return &loc->winxgc; 
}

/**
 * window_list_win_to_front:
 * @win: 
 * 
 * move the graphic context associated to window @win to the front of 
 * the window list. Which is the place where information about the 
 * current window is stored.
 * 
 **/

BCG * window_list_win_to_front(int win)
{ 
  WindowList *loc  =  window_list_search_w_int(The_List,win);
  if ( loc != NULL) 
    {
      if ( loc->prev != NULL) 
	(loc->prev)->next = loc->next ;
      if ( loc->next != NULL) (loc->next)->prev = loc->prev ;
      /* reinsert loc on top */
      loc->next = The_List ;
      loc->next->prev = loc ;
      loc->prev = NULL;
      The_List = loc;
      return &loc->winxgc;
    }
  else 
    return NULL;
}

/**
 * window_list_remove:
 * @num: 
 * 
 * Free the entry in window list for window number @num. 
 * note that scales are destroyed here and private data 
 * are destroyed elsewhere 
 * 
 **/

void window_list_remove(int num)
{
  WindowList *L1= The_List ;
  window_scale_list *loc;
  while ( L1 != (WindowList *) NULL)
    {
      if ( L1->winxgc.CurWindow == num )
	{
	  if ( L1->prev != NULL) 
	    {
	      if ( L1->next != NULL) L1->next->prev = L1->prev;
	      L1->prev->next = L1->next ; 
	    }
	  else 
	    {
	      The_List = (WindowList *) L1->next ;
	      if ( L1->next != NULL) L1->next->prev = NULL;
	    }
	  /* now L1 is to be freed */
	  loc = L1->winxgc.scales ; 
	  while ( loc != NULL)
	    {
	      window_scale_list *next = loc->next;
	      FREE(loc);
	      loc = next;
	    }
	  FREE(L1);
	  break;
	}
      L1 = (WindowList *) L1->next;
    }
  return;
}

/**
 * window_list_search:
 * @winnum: 
 * 
 * Search a graphic context for window @winnum 
 * 
 * 
 * Return value: %NULL or a graphic context 
 **/

BCG *window_list_search(int winnum)
{ 
  WindowList *loc = window_list_search_w_int(The_List,(Max(0,winnum)));
  if ( loc == NULL) return NULL;
  return  &(loc->winxgc);
}

static WindowList  *window_list_search_w_int(WindowList *listptr, int i)
{
  while ( listptr != NULL)
    {
      if ((listptr->winxgc.CurWindow) == i) return( listptr);
      listptr = listptr->next;
    }
  return NULL;
}

/**
 * window_list_search_toplevel:
 * @win: a void pointer  
 * 
 * returns the number of graphic windows which share
 * the same toplevel widget 
 * 
 * Return value: an integer 
 **/

int window_list_search_toplevel(void *win)
{ 
  return window_list_search_topelevel_int(The_List,win,0);
}

static int window_list_search_topelevel_int(WindowList *listptr,void *win, int count)
{
  if (listptr == (WindowList  *) NULL)
    {
      return count;
    }
  else 
    { 
      if ( window_list_check_top(&listptr->winxgc,win) == TRUE) count++;
      return window_list_search_topelevel_int((WindowList *) listptr->next,win,count);
    }
}


/**
 * window_list_get_first:
 * @void: 
 * 
 * returns the first graphic context found in the window list 
 * 
 * Return value: %NULL or a graphic context 
 **/

BCG *window_list_get_first(void)
{
  return ( The_List == (WindowList *) NULL) ?  (BCG *) 0 :  &(The_List->winxgc);
}


/**
 * window_list_get_ids:
 * @Num: int pointer 
 * @Ids: pointer to an array of integers 
 * @flag: 0 or 1 
 * 
 * If @flag is equal to zero then returns in @Num the number 
 * of graphic windows in the graphic window list.
 * If @flag is equal to one then @Ids must be a pointer to 
 * an allocated array of proper size (size returned by the call with @flag =0)
 * which is then filled with the ids of the graphic windows. 
 * 
 **/

void window_list_get_ids(int *Num, int *Ids, int flag)
{
  WindowList *listptr = The_List;
  *Num = 0;
  if ( flag == 0 )
    {
      while ( listptr != (WindowList  *) 0 ) 
	{
	  (*Num)++;
	  listptr = (WindowList *) listptr->next;
	}
    }
  else 
    {
      while ( listptr != (WindowList  *) 0 ) 
	{
	  Ids[*Num] = listptr->winxgc.CurWindow;
	  listptr =  (WindowList *)listptr->next;
	  (*Num)++;
	}
    }
}


/**
 * window_list_get_max_id:
 * 
 * returns the highest id of graphic windows or -1 if no 
 * graphic windows.
 * 
 * Returns: an integer 
 **/

int window_list_get_max_id(void)
{
  WindowList *listptr = The_List;
  int Num = -1;
  while ( listptr != (WindowList  *) 0 ) 
    {
      Num = Max(listptr->winxgc.CurWindow,Num);
      listptr =  (WindowList *)listptr->next;
    }
  /* Sciprintf("Max Id : %d \n",Num); */
  return(Num);
}

/*
 * Graphic scales 
 */

/*
 * default values 
 * 
 */

static window_scale_list  default_scale = 
  { 
    0,
    0,
    {600,400},
    {0.0,0.0,1.0,1.0},
    {0.0,0.0,1.0,1.0},
    {0.0,0.0},
    {1/8.0,1/8.0,1/8.0,1/8.0},
    {0.0,1.0,0.0,10},
    {0.0,1.0,0.0,10},
    '5',
    75.0,53.0,450.0,318.0,
    {'n','n'},
    {75,53,450,318},
    {2,10,2,10},
    {{1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0}},
    {-1.0,1.0,-1.0,1.0,-1.0,1.0},
    {0.0,0.0,0.0},
    35.0,45.0,
    1,                 /* added by es */
    1.0,0.0,           /* test: cosa, sina */
    (window_scale_list *) 0, /*unused */
    (window_scale_list *) 0 /*unused */
  };


/**
 * set_window_scale_with_default:
 * @win: an integer 
 * 
 * fills the current scale of window @win with default values.
 **/

void set_window_scale_with_default(int win) 
{ 
  set_window_scale(win,&default_scale);
} 



/**
 * move_subwindow_scale_to_front:
 * @Xgc: a graphic context 
 * @subwin: a pointer to an array of doubles specifying a subwindows.
 * 
 *   if subwin == NULL : returns OK 
 *   if subwin != NULL : search for existing scale values for
 *      subwindow subwin of window graphics context Xgc.
 *   If no scale are found we do nothing and return FAIL
 *   else the scale is moved on the top of the scale list and we return OK
 * 
 * 
 * Returns: %OK or %FAIL
 **/

int move_subwindow_scale_to_front(BCG *Xgc,double *subwin)
{ 
  window_scale_list *loc;
  if ( Xgc == NULL) return FAIL;
  if ( Xgc->scales == NULL ) return FAIL;
  loc = check_subwin_wcscale(Xgc->scales,subwin);
  if ( loc != NULL) 
    {
      /* we move loc to the top of the list */
      if ( loc != Xgc->scales )
	{
	  /* remove loc */
	  (loc->prev)->next = loc->next ;
	  if ( loc->next != NULL) (loc->next)->prev = loc->prev ;
	  /* reinsert loc on top */
	  loc->next = Xgc->scales;
	  loc->next->prev = loc ;
	  Xgc->scales = loc;
	}
      return OK;
    }
  return FAIL;
}

/**
 * xgc_add_scale:
 * @Xgc: a graphic context 
 * @scale: a #window_scale_list pointer 
 * 
 * adds a copy of @scale at the begining of the window scale list  
 * of graphic context @Xgc.
 * 
 * Returns:  %OK or %FAIL.
 **/

static int xgc_add_scale(BCG *Xgc,window_scale_list *scale)
{ 
  window_scale_list *loc;
  if ( Xgc == NULL) return FAIL;
  if (( loc = new_wcscale(scale))== NULL)
    {
      Scistring("set_window_scale: running out of memory\n");
      return FAIL;
    }
  else 
    {
      loc->next = Xgc->scales;
      if (loc->next != NULL) loc->next->prev = loc;
      Xgc->scales = loc;
    }
  return OK;
}


/**
 * xgc_add_default_scale:
 * @Xgc:  a graphic context 
 * 
 * adds a copy of default scale at the begining of the window scale list 
 * of graphic context @Xgc.
 *
 * Returns: %OK or %FAIL.
 **/

int xgc_add_default_scale(BCG *Xgc)
{
  return xgc_add_scale(Xgc,&default_scale);
}

/**
 * xgc_reset_scales_to_default:
 * @Xgc:  a graphic context 
 * 
 * remove the scales in the graphic context @Xgc and 
 * stores a copy of default scale in the window scale list
 * 
 * Returns: %OK or %FAIL.
 **/

int xgc_reset_scales_to_default(BCG *Xgc)
{
  window_scale_list *loc = Xgc->scales;
  while ( loc != NULL)
    {
      window_scale_list *next = loc->next;
      FREE(loc);
      loc = next;
    }
  Xgc->scales = NULL;
  /* now reinsert default scale */
  return xgc_add_default_scale(Xgc);
}


/**
 * set_window_scale:
 * @win: an integer 
 * @scale: a scale (#window_scale_list pointer).
 * 
 * if a subwin given by @scale is already in the scale list 
 * the scale list item is updated using @scale 
 * and moved at the begining of window @win scale list 
 * if not, a copy of @scale is added at the begining of the scale list 
 * of window @win.
 * 
 * Return value: %OK or %FAIL
 **/

static int set_window_scale(int win,window_scale_list *scale)
{ 
  window_scale_list *loc;
  BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL) return FAIL;
  if ( winxgc->scales ==  NULL)
    {
      winxgc->scales = new_wcscale(scale);
      if ( winxgc->scales == 0) 
	{
	  Scistring("set_window_scale: running out of memory\n");
	  return FAIL;
	}
      return OK;
    }
  loc = check_subwin_wcscale(winxgc->scales,scale->subwin_rect); 
  if ( loc != NULL) 
    {
      /* subwin exists we update it with data from scale
       * loc->next and loc->previous are not changed !
       */
      scale_copy(loc,scale);
      /* we move listptr to the top of the list */
      if ( loc != winxgc->scales )
	{
	  /* remove loc */
	  (loc->prev)->next = loc->next ;
	  if ( loc->next != NULL) (loc->next)->prev = loc->prev ;
	  /* reinsert loc on top */
	  loc->next = winxgc->scales;
	  loc->next->prev = loc ;
	  winxgc->scales = loc;
	}      
    }
  else 
    {
      /* subwin does not exists we add it a the begining of the list */
      if (( loc = new_wcscale(scale))== NULL)
	{
	  Scistring("set_window_scale: running out of memory\n");
	  return FAIL;
	}
      else 
	{
	  loc->next = winxgc->scales;
	  if (loc->next != NULL) loc->next->prev = loc;
	  winxgc->scales = loc;
	}
    }
  return OK;
}

/**
 * new_wcscale:
 * @val: the scale to use for copy 
 * 
 * returns a new window_scale_list which is a copy of val.
 * 
 * Return value: a new #window_scale_list or %NULL
 **/

static window_scale_list *new_wcscale(window_scale_list *val) 
{
  window_scale_list *new ;
  if ((new = MALLOC(sizeof(window_scale_list))) == NULL) return NULL;
  new->next = (window_scale_list *) 0;
  new->prev = (window_scale_list *) 0;
  scale_copy(new,val);
  return new;
}

/**
 * check_subwin_wcscale:
 * @listptr: a #window_scale_list
 * @subwin_rect: array pointer 
 * 
 * Check is a #window_scale_list for a subwindow specified by @subwin_rect 
 * is found in the scale list @listptr and returns it if found. 
 * 
 * Returns: %NULL or a #window_scale_list
 **/

static window_scale_list *check_subwin_wcscale( window_scale_list *listptr, double subwin_rect[4])
{
  int count = 0;
  window_scale_list *listptr1 = listptr;
  while ( listptr1 != NULL) 
    {
      count++; listptr1 = listptr1->next;
    }
  /* Sciprintf("Number of scales : %d\n",count); */
  while ( listptr != NULL) 
    {
      if ( same_subwin( listptr->subwin_rect,subwin_rect)) return listptr;
      listptr = listptr->next;
    }
  return NULL;
}

static int same_subwin( double lsubwin_rect[4],double subwin_rect[4])
{
  if ( Abs(lsubwin_rect[0] - subwin_rect[0]) < 1.e-8
       && Abs(lsubwin_rect[1] - subwin_rect[1]) < 1.e-8
       && Abs(lsubwin_rect[2] - subwin_rect[2]) < 1.e-8
       && Abs(lsubwin_rect[3] - subwin_rect[3]) < 1.e-8 ) 
    return 1;
  else 
    return 0;
}

/*------------------------------------------------------------
 * delete the scales associated to window i 
 *-------------------------------------------------------------*/

/**
 * window_scale_delete:
 * @win: an integer 
 * 
 * delete the scales associated to window @win. 
 * 
 **/

void window_scale_delete(int win)
{ 

  window_scale_list *loc;
  BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL) return ; 
  if ( winxgc->scales ==  NULL) return ;
  loc = winxgc->scales ;
  while ( loc != NULL)
    {
      window_scale_list *next = loc->next;
      FREE(loc);
      loc = next;
    }
  winxgc->scales = NULL;
}


/**
 * scale_copy:
 * @s1: a #window_scale_list
 * @s2: a #window_scale_list
 * 
 * Uses @s2 to update @s1 
 * 
 **/

static void scale_copy( window_scale_list *s1,window_scale_list *s2)
{
  int i,j;
  s1->scale_flag=s2->scale_flag;
  s1->scale_flag3d=s2->scale_flag3d;
  s1->wdim[0]=  s2->wdim[0];
  s1->wdim[1]=  s2->wdim[1];
  for (i=0; i< 4; i++) 
    {
      s1->subwin_rect[i]=s2->subwin_rect[i];
      s1->frect[i]=s2->frect[i];
      s1->WIRect1[i]=s2->WIRect1[i];
      s1->Waaint1[i]=s2->Waaint1[i];
      s1->xtics[i]=s2->xtics[i];
      s1->ytics[i]=s2->ytics[i];
      s1->axis[i]=s2->axis[i];
    }
  s1->strflag = s2->strflag;
  for (i=0; i< 2; i++) s1->zfrect[i]=s2->zfrect[i]; 
  for (i=0; i< 3; i++) 
    for (j=0; i< 3; i++) 
      s1->m[i][j]=s2->m[i][j];
  for (i=0; i< 6; i++) 
    s1->bbox1[i]=s2->bbox1[i] ;
  s1->Wxofset1=s2->Wxofset1;
  s1->Wyofset1=s2->Wyofset1;
  s1->Wscx1=s2->Wscx1;
  s1->Wscy1=s2->Wscy1;
  s1->logflag[0] = s2->logflag[0];
  s1->logflag[1] = s2->logflag[1];
  s1->cosa = s2->cosa;
  s1->sina = s2->sina;
}


/**
 * show_scales:
 * @Xgc: a graphic context 
 * 
 * display on the current output stream the subwindows 
 * associated to graphic context @Xgc.
 * 
 **/

void show_scales(BCG *Xgc)
{
  window_scale_list *loc;
  if ( Xgc == NULL ||  Xgc->scales ==  NULL) 
    {
      Sciprintf("No scales for window %d\r\n",Xgc->CurWindow);
      return ;
    }
  Sciprintf("scales for window %d\r\n",Xgc->CurWindow);
  loc = Xgc->scales ;
  while ( loc != NULL)
    {
      Sciprintf("\tsubwin=[%5.2f,%5.2f,%5.2f,%5.2f], flag=%d\r\n",
	       loc->subwin_rect[0],loc->subwin_rect[1],loc->subwin_rect[2],loc->subwin_rect[3],
	       loc->scale_flag);      
      loc = loc->next;
    }
}

/**
 * setscale2d:
 * @Xgc: a graphic context 
 * @WRect: 
 * @FRect: 
 * @logscale: 
 * 
 * SHOULD NOT BE USED.
 * 
 * uses @WRect,@FRect,@logscale to update the graphic scales 
 * contained in @Xgc. @WRect gives the subwindow to use 
 * @FRect gives the bounds. 
 * @WRect=[<x-upperleft>,<y-upperleft>,largeur,hauteur]
 * example WRect=[0,0,1.0,1.0] we use all the window 
 *         WRect=[0.5,0.5,0.5,0.5] we use the down right 
 *         quarter of the window 
 *
 * Return value: unused.
 **/

int setscale2d(BCG *Xgc,double WRect[4],double FRect[4],char *logscale)
{
  static int aaint[]={2,10,2,10};
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) store_Ech(Xgc,WRect,FRect,logscale);
  if (logscale[0]=='l') 
    {
      FRect[0]=log10(FRect[0]);
      FRect[2]=log10(FRect[2]);
    }
  if (logscale[1]=='l') 
    {
      FRect[1]=log10(FRect[1]);
      FRect[3]=log10(FRect[3]);
    }
  set_scale(Xgc,"tttftf",WRect,FRect,aaint,logscale,NULL);
  return(0);
}


/*-------------------------------------------
 * setscale2d 
 *  
 *-------------------------------------------*/

/**
 * Nsetscale2d:
 * @Xgc: a graphic context 
 * @WRect: subwindow 
 * @FRect: scales 
 * @logscale: a string. 
 * 
 * uses @WRect,@FRect,@logscale to update the graphic scales 
 * contained in @Xgc. 
 *  @WRect gives the subwindow to use 
 *  @ARect gives the axis rectangle 
 *  @FRect gives the bounds 
 *  WRect=[<x-upperleft>,<y-upperleft>,largeur,hauteur]
 *  example WRect=[0,0,1.0,1.0] we use all the window 
 *          WRect=[0.5,0.5,0.5,0.5] we use the down right 
 *  quarter of the window
 *  @logscale : gives xy log axis flags 
 *  each argument can be a null pointer if they are 
 *  not to be changed from their current value 
 * 
 *  Each window can have a set of scales : one for each specified 
 *  subwindow. This routine must take care of properly 
 *  switching from one scale to an other.
 * 
 * 
 * Returns: 0 
 **/

int Nsetscale2d(BCG *Xgc,double WRect[4],double ARect[4],double FRect[4],char *logscale)
{
  /* if some arguments are null pointer we set them to 
   * the corresponding current_scale value. 
   * this is only important for store_NEch which does not work with null arguments 
   */ 
  /* char flag[] = "tfffff";*/ /* flag to specify which arguments have changed*/
  char flag[7];
  strcpy(flag,"tfffff");
  if ( WRect != NULL) 
    {
      flag[1]='t';
      /* a subwindow is specified 
       * try to get its scale values as default values 
       */
      if (  move_subwindow_scale_to_front(Xgc,WRect) == FAIL)
	{
	  /* new subwindow */
	  if ( xgc_add_scale(Xgc,&default_scale)== FAIL)   return 0;
	  memcpy(Xgc->scales->subwin_rect, WRect,4*sizeof(double));
	  Xgc->scales->scale_flag = 0;
	  Xgc->scales->scale_flag3d = 0;
	}
    }
  else 
    {
      /* set WRect to default value */
      WRect = Xgc->scales->subwin_rect;
    }
  if ( FRect != NULL) flag[2]='t'; else FRect = Xgc->scales->frect;
  if ( ARect != NULL) flag[5]='t'; else ARect = Xgc->scales->axis;
  if ( logscale != NULL) flag[4] ='t'; else logscale = Xgc->scales->logflag;
  if ( flag[4] == 't' && flag[2] == 't' ) 
    {
      if (logscale[0]=='l') 
	{
	  if ( FRect[0] <= 0 || FRect[2] <= 0 ) 
	    {
	      Scistring("Warning: negative boundaries on x scale with a log scale \n");
	      FRect[0]=1.e-8;FRect[2]=1.e+8;
	    } 
	  FRect[0]=log10(FRect[0]);
	  FRect[2]=log10(FRect[2]);
	}
      if (logscale[1]=='l') 
	{
	  if ( FRect[1] <= 0 || FRect[3] <= 0 ) 
	    {
	      Scistring("Warning: negative boundaries on y scale with a log scale \n");
	      FRect[1]=1.e-8;FRect[3]=1.e+8;
	    } 
	  FRect[1]=log10(FRect[1]);
	  FRect[3]=log10(FRect[3]);
	}
    }
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) 
    store_NEch(Xgc,flag,WRect,ARect,FRect,logscale);
  set_scale(Xgc,flag,WRect,FRect,NULL,logscale,ARect);
  return(0);
}

/**
 * getscale2d:
 * @Xgc: 
 * @WRect: 
 * @FREct: 
 * @logscale: 
 * @ARect: 
 * 
 * get current scale values.
 * 
 * Returns: 0 
 **/

int getscale2d(BCG *Xgc,double WRect[4],double FRect[4],char *logscale,double ARect[4])
{
  int i;
  static double ten=10.0;
  logscale[0] = Xgc->scales->logflag[0];
  logscale[1] = Xgc->scales->logflag[1];
  for ( i=0; i < 4 ; i++) 
    {
      WRect[i]=Xgc->scales->subwin_rect[i];
      FRect[i]=Xgc->scales->frect[i];
      ARect[i]=Xgc->scales->axis[i];
    }
  if (logscale[0]=='l') 
    {
      FRect[0]=pow(ten,FRect[0]);
      FRect[2]=pow(ten,FRect[2]);
    }
  if (logscale[1]=='l') 
    {
      FRect[1]=pow(ten,FRect[1]);
      FRect[3]=pow(ten,FRect[3]);
    }
  return(0);
}


/**
 * set_scale:
 * @Xgc: a graphic context 
 * @flag: flag to indicate which arguments to use 
 * @subwin: subwindow specification
 * @frame_values: array with xmin,ymin,xmax,ymax
 * @aaint:  array with xint,x_subint,y_int,y_subint
 * @logflag: xlogflag,ylogflag
 * @axis_values: mfact_xl, mfact_xr,mfact_yu,mfact_yd
 * 
 * changes selected items in Xgc scale 
 * flag gives which component must be used for 
 *      upgrading or setting the current scale 
 * flag[0]   : used for window dim upgrade t or f 
 * flag[1:5] : subwin,frame_values,aaint,logflag,axis_values
 *           for subwin the values can be t,T,f 
 * Warning : frame_values[i] must be log10(val[i]) 
 *           when using log scales 
 * 
 **/

void set_scale(BCG *Xgc,  char flag[6],double  subwin[4],double  frame_values[4],
	       int aaint[4], char logflag[2],   double axis_values[4])
{
  char wdim_changed= 'f',subwin_changed='f';
  char frame_values_changed='f',aaint_changed='f';
  char logflag_changed='f';
  char axis_changed = 'f';
  int wdim[2];
  int i;
  
  if ( flag[1] == 'T') 
    {
      /* we directly change the current scale without searching 
       * a subwindow scale 
       */
      memcpy(Xgc->scales->subwin_rect, subwin ,4*sizeof(double));
      subwin_changed='t' ;
    }
  else if ( flag[1] == 't' ) 
    {
      if ( ! same_subwin( subwin,Xgc->scales->subwin_rect)) 
	{
	  subwin_changed='t' ;
	  if ( move_subwindow_scale_to_front(Xgc,subwin)==FAIL);
	  if ( xgc_add_scale(Xgc,&default_scale)== FAIL)
	    return;
	}
    }

  if ( flag[0] == 't'  ) 
    {
      Xgc->graphic_engine->xget_windowdim(Xgc,wdim,wdim+1);
      if ( Xgc->scales->wdim[0] != wdim[0] || Xgc->scales->wdim[1] != wdim[1]) 
	{ 
	  Xgc->scales->wdim[0] = wdim[0]; Xgc->scales->wdim[1] = wdim[1]; 
	  wdim_changed='t';
	}
    }
  if ( flag[2] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( frame_values[i] != Xgc->scales->frect[i]) { frame_values_changed='t' ; break;}
      /* if no scales were present and the values given are the same as the 
       * default frect values we must register that we are setting a scale 
       */
      if ( Xgc->scales->scale_flag == 0)  frame_values_changed='t' ;

    }
  if ( flag[3] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( aaint[i] != Xgc->scales->Waaint1[i]) { aaint_changed='t' ; break;}
    }
  if ( flag[4] == 't' ) 
    {
      for (i=0; i < 2 ; i++ ) 
	if ( logflag[i] != Xgc->scales->logflag[i]) { logflag_changed='t' ; break;}
    }
  if ( flag[5] == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) 
	if ( axis_values[i] != Xgc->scales->axis[i]) { axis_changed='t' ; break;}
    }
  if ( axis_changed == 't') 
    {
      for (i=0; i < 4 ; i++ ) Xgc->scales->axis[i] = axis_values[i];
    }

  if ( subwin_changed == 't' ) 
    {
      for (i=0; i < 4 ; i++ ) Xgc->scales->subwin_rect[i] = subwin[i];
    }
  if ( frame_values_changed == 't' ) 
    {
      for (i=0; i < 4 ; i++) Xgc->scales->frect[i]=frame_values[i]; 
      /* the scale is no more a default scale */
      Xgc->scales->scale_flag = 1;
    }

  if ( wdim_changed == 't' || subwin_changed == 't' || frame_values_changed == 't' 
       ||  axis_changed == 't' )
    {
      /* Upgrading constants for 2D transformation */
      double scx,scy,xoff,yoff,val;
      scx =  ((double) Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[2]-Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[2]
	      *(Xgc->scales->axis[0]+Xgc->scales->axis[1]));
      scy =  ((double) Xgc->scales->wdim[1]*Xgc->scales->subwin_rect[3]-Xgc->scales->wdim[1]*Xgc->scales->subwin_rect[3]
	      *(Xgc->scales->axis[2]+Xgc->scales->axis[3]));
      xoff= ((double) Xgc->scales->wdim[0]*Xgc->scales->subwin_rect[2])*(Xgc->scales->axis[0]);
      yoff= ((double) Xgc->scales->wdim[1]*Xgc->scales->subwin_rect[3])*(Xgc->scales->axis[2]);
      
      Xgc->scales->Wxofset1= xoff+Xgc->scales->subwin_rect[0]*((double)Xgc->scales->wdim[0]);
      Xgc->scales->Wyofset1= yoff+Xgc->scales->subwin_rect[1]*((double)Xgc->scales->wdim[1]);
      Xgc->scales->Wscx1   = scx;
      Xgc->scales->Wscy1   = scy;

      val = Abs(Xgc->scales->frect[0]- Xgc->scales->frect[2]);
      Xgc->scales->Wscx1 = (val <=SMDOUBLE) ? Xgc->scales->Wscx1/SMDOUBLE : Xgc->scales->Wscx1/val; 
      val = Abs(Xgc->scales->frect[1]- Xgc->scales->frect[3]);
      Xgc->scales->Wscy1 = (val <=SMDOUBLE) ? Xgc->scales->Wscy1/SMDOUBLE : Xgc->scales->Wscy1/val;

      Xgc->scales->WIRect1[0] = XScale( Xgc->scales->frect[0]);
      Xgc->scales->WIRect1[1] = YScale( Xgc->scales->frect[3]);
      Xgc->scales->WIRect1[2] = Abs(XScale( Xgc->scales->frect[2]) -  XScale( Xgc->scales->frect[0]));
      Xgc->scales->WIRect1[3] = Abs(YScale( Xgc->scales->frect[3]) -  YScale( Xgc->scales->frect[1]));
#ifdef WITH_GTKGLEXT 
      /* transmit info to opengl */
      if ( Xgc->graphic_engine == &GL_gengine ) 
	{
	  nsp_ogl_set_view(Xgc);
	}
#endif

    }
  if (  aaint_changed== 't' ) 
    {
      for (i=0; i < 4 ; i++) Xgc->scales->Waaint1[i]=aaint[i];
    }
  if ( logflag_changed== 't' ) 
    {
      Xgc->scales->logflag[0]=logflag[0];      Xgc->scales->logflag[1]=logflag[1];
    }
  if (  aaint_changed== 't' || frame_values_changed == 't')
    {
      Xgc->scales->xtics[0] = Xgc->scales->frect[0];
      Xgc->scales->xtics[1] = Xgc->scales->frect[2];
      Xgc->scales->ytics[0] = Xgc->scales->frect[1];
      Xgc->scales->ytics[1] = Xgc->scales->frect[3];
      Xgc->scales->xtics[2] = 0.0;
      Xgc->scales->ytics[2] = 0.0;
      Xgc->scales->xtics[3] = Xgc->scales->Waaint1[1];
      Xgc->scales->ytics[3] = Xgc->scales->Waaint1[3];
    }
}

/*--------------------------------------------------------------------
 * Get the current window dimensions.
 *--------------------------------------------------------------------*/

/**
 * get_cwindow_dims:
 * @: 
 * 
 * 
 **/
void get_cwindow_dims( int wdims[2])
{
  BCG *Xgc = check_graphic_window();
  Xgc->graphic_engine->xget_windowdim(Xgc,wdims,wdims+1);
}

/*--------------------------------------------------------------------
 * use current scale to set the clipping rectangle 
 *--------------------------------------------------------------------*/

void frame_clip_on(BCG *Xgc)
{
  Xgc->graphic_engine->xset_clip(Xgc,Xgc->scales->WIRect1);
}

void frame_clip_off(BCG *Xgc)
{
  Xgc->graphic_engine->xset_unclip(Xgc);
}

/* checks if there's an already stored event in a 
 * graphic window queue.
 * if Xgc is NULL all the windows are searched 
 * else only Xgc is searched 
 */

/**
 * window_list_check_queue:
 * @Xgc: 
 * @ev: 
 * 
 * 
 * 
 * Returns: 
 **/
int window_list_check_queue(BCG *Xgc,nsp_gwin_event *ev)
{
  WindowList *L1= The_List ;
  if ( Xgc == NULL )
    {
      /* search the whole list */
      while ( L1 != (WindowList *) NULL)
	{
	  if ( nsp_queue_empty(&L1->winxgc.queue) == FALSE )
	    {
	      *ev= nsp_dequeue(&L1->winxgc.queue);
	      return OK;
	    }
	  L1 = (WindowList *) L1->next;
	}
    }
  else
    {
      if ( nsp_queue_empty(&Xgc->queue) == FALSE ) 
	{
	  *ev= nsp_dequeue(&Xgc->queue);
	  return OK;
	}
    }
  return FAIL;
}

/**
 * window_list_clear_queue:
 * @Xgc: 
 * 
 * 
 * 
 * Returns: 
 **/
int window_list_clear_queue(BCG *Xgc)
{
  WindowList *L1= The_List ;
  if ( Xgc == NULL )
    {
      /* clear all queues */
      while ( L1 != (WindowList *) NULL)
	{
	  nsp_clear_queue(&L1->winxgc.queue);
	  L1 = (WindowList *) L1->next;
	}
    }
  else
    {
      nsp_clear_queue(&Xgc->queue);
    }
  return FAIL;
}
