/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 2001 Enpc/Jean-Philippe Chancelier
 *    jpc@cermics.enpc.fr 
 --------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------
 * store a list of graphic contexts 
 * this is gui independant the gui dependant part is 
 * delegated to private member of winxgc  
 *--------------------------------------------------------------------------*/

#include "nsp/graphics/Graphics.h" 

/*
 * structure for Window List 
 */

typedef struct _window_list WindowList;

struct  _window_list
{
  BCG winxgc;
  WindowList *next;
  WindowList *prev;
} ;

static WindowList *The_List  = (WindowList *) NULL;  /* list of windows */

static int set_window_scale(int win,window_scale_list *scale);
static window_scale_list *check_subwin_wcscale( window_scale_list *listptr, double subwin_rect[4]);
static void scale_copy (window_scale_list *s1, window_scale_list *s2);
static window_scale_list *new_wcscale ( window_scale_list *val);
static int same_subwin (double lsubwin_rect[4],double subwin_rect[4]);
static WindowList *window_list_search_w(int winnum);

/*-------------------------------------------------------------------------
 * Adds a new entry at the  start  of the Window List 
 * and returns a pointer to that entry 
 *-------------------------------------------------------------------------*/

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
 * move the Xgc associated to window win to front of 
 * window list. This is were we store the current 
 * window.
 * 
 **/

BCG * window_list_win_to_front(int win)
{ 
  WindowList *loc  = window_list_search_w(win);
  if ( loc != NULL) 
    {
      if ( loc->prev != NULL) 
	(loc->prev)->next = loc->next ;
      if ( loc->next != NULL) (loc->next)->prev = loc->prev ;
      /* reinsert loc on top */
      loc->next = The_List ;
      loc->next->prev = loc ;
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
 * Free the entry in window list for window number num 
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
	    L1->prev->next = L1->next ; 
	  else 
	    The_List = (WindowList *) L1->next ;
	  break;
	}
      L1 = (WindowList *) L1->next;
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
  return;
}

/**
 * window_list_search:
 * @winnum: 
 * 
 * Search a graphic context for window #winnum 
 * 
 * 
 * Return value: %NULL or a grapci context 
 **/

BCG *window_list_search(int winnum)
{ 
  WindowList *loc = window_list_search_w(Max(0,winnum));
  if ( loc == NULL) return NULL;
  return  &(loc->winxgc);
}

static WindowList *window_list_search_w_int (WindowList *,int);

static WindowList *window_list_search_w(int winnum)
{ 
  return window_list_search_w_int(The_List,Max(0,winnum));
}

static WindowList  *window_list_search_w_int(WindowList *listptr, int i)
{
  if (listptr == (WindowList  *) NULL) return NULL;
  if ((listptr->winxgc.CurWindow) == i)
    return( listptr);
  else 
    return(window_list_search_w_int((WindowList *) listptr->next,i));
}

/*---------------------------------------------------
 * returns number of graphic windows which shares the same toplevel widget 
 *---------------------------------------------------*/

static int window_list_search_topelevel_int(WindowList *,void *,int);
extern int window_list_check_top(BCG *,void *) ;

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
 * @: 
 * 
 * returns the first graphic context found in the window list 
 * 
 * Return value: %NULL or a graphic context 
 **/

BCG * window_list_get_first()
{
  return ( The_List == (WindowList *) NULL) ?  (BCG *) 0 :  &(The_List->winxgc);
}


/*----------------------------------------------------------------------
 * get ids of scilab windows in array Ids,
 * Num gives the number of windows
 * flag == 1 ==> get the Ids 
 * flag == 0 ==> just get the Number Num 
 *----------------------------------------------------------------------*/

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

/*---------------------------------------------------------------------------
 * get the highest id of scilab windows
 * or -1 if no windows 
 *---------------------------------------------------------------------------*/

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

/*------------------------------------------------------------------------
 * Graphic scales 
 *--------------------------------------------------------------------------*/

static void scale_copy (window_scale_list *s1, window_scale_list *s2);
static window_scale_list *new_wcscale ( window_scale_list *val);
static int same_subwin (double lsubwin_rect[4],double subwin_rect[4]);

/* Current Scale XXXXXX to be removed */

window_scale_list current_scale = 
{ 
  0,
  {600,400},
  {0.0,0.0,1.0,1.0},
  {0.0,0.0,1.0,1.0},
  {1/8.0,1/8.0,1/8.0,1/8.0},
  {0.0,1.0,0.0,10},
  {0.0,1.0,0.0,10},
  75.0,53.0,450.0,318.0,
  {'n','n'},
  {75,53,450,318},
  {2,10,2,10},
  {{1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0}},
  {0.0,1.0,0.0,1.0,0.0,1.0},
  35.0,45.0,
  1,                 /* added by es */
  (window_scale_list *) 0, /*unused */
  (window_scale_list *) 0 /*unused */
};

/*
 * default values 
 * 
 */

static window_scale_list  default_scale = 
{ 
  0,
  {600,400},
  {0.0,0.0,1.0,1.0},
  {0.0,0.0,1.0,1.0},
  {1/8.0,1/8.0,1/8.0,1/8.0},
  {0.0,1.0,0.0,10},
  {0.0,1.0,0.0,10},
  75.0,53.0,450.0,318.0,
  {'n','n'},
  {75,53,450,318},
  {2,10,2,10},
  {{1.0,0.0,0.0},{0.0,1.0,0.0},{0.0,0.0,1.0}},
  {0.0,1.0,0.0,1.0,0.0,1.0},
  35.0,45.0,
  1,                 /* added by es */
  (window_scale_list *) 0, /*unused */
  (window_scale_list *) 0 /*unused */
};

/*----------------------------------------------------------
 * Back to defaults values : fills current scale (Scale)
 * and curwin() scale with default scale.
 *----------------------------------------------------------*/
/* XXXX 
void current_scale2default()
{
  scale_copy(&current_scale,&default_scale);  
  set_window_scale(curwin(),&current_scale);
}
*/

void set_window_scale_with_default(int win) 
{ 
  set_window_scale(win,&default_scale);
} 

/*------------------------------------------------------------
 * void get_window_scale(Xgc,subwin)
 *   if subwin == NULL : do nothing 
 *   if subwin != NULL : search for existing scale values of subwindow subwin of window i.
 *   If no scale are found we do nothing and return 0
 *   else the scale is moved on front
 *   a subwindow is described by an array of four doubles 
 *-------------------------------------------------------------*/

int get_window_scale(int win,double *subwin)
{ 
  window_scale_list *listptr;
  BCG * winxgc = window_list_search(win);
  if ( winxgc == NULL) return FAIL;
  listptr = winxgc->scales;
  if ( subwin == NULL) 
    {
      scale_copy(&current_scale,listptr);
      return OK;
    }
  else
    {
      window_scale_list *loc = check_subwin_wcscale(listptr,subwin);
      if ( loc != NULL) 
	{
	  scale_copy(&current_scale,loc);
	  return OK;
	}
    }
  return FAIL;
}

/*------------------------------------------------------------
 * int move_subwindow_scale_to_front(Xgc,subwin)
 *   if subwin == NULL : returns OK 
 *   if subwin != NULL : search for existing scale values for
 *      subwindow subwin of window graphics context Xgc.
 *      If no scale are found we do nothing and return FAIL
 *   else the scale is moved on the top of the scale list and we return OK
 *-------------------------------------------------------------*/

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
 * add a copy of scale at the begining of the window scale list 
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


int xgc_add_default_scale(BCG *Xgc)
{
  return xgc_add_scale(Xgc,&default_scale);
}

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
 * @win: 
 * @scale: 
 * 
 * if a subwin given by scale is already in the scale list 
 * the scale list item  is updates using #scale 
 * and moved at the begining of window #win scale list 
 * if not a new scale is added atthe begining of the scale list 
 * of window #win.
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

static window_scale_list *check_subwin_wcscale( window_scale_list *listptr, double subwin_rect[4])
{
  if ( listptr == (window_scale_list  *) NULL)  return NULL;
  if ( same_subwin( listptr->subwin_rect,subwin_rect)) 
    return listptr;
  else 
    return check_subwin_wcscale(listptr->next,subwin_rect);
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
 * @s1: 
 * @s2: 
 * 
 * Uses s2 to update s1 
 * 
 **/

static void scale_copy( window_scale_list *s1,window_scale_list *s2)
{
  int i,j;
  s1->flag=s2->flag;
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
}


/*-------------------------------------------
 * show the recorded scales for window i 
 *-------------------------------------------*/

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
      sciprint("\tsubwin=[%5.2f,%5.2f,%5.2f,%5.2f], flag=%d\r\n",
	       loc->subwin_rect[0],loc->subwin_rect[1],loc->subwin_rect[2],loc->subwin_rect[3],
	       loc->flag);      
      loc = loc->next;
    }
}


/*-------------------------------------------
 * setscale2d 
 * uses WRect,FRect,logscale to update 
 * current scale (current_scale) 
 *  WRect gives the subwindow to use 
 *  FRect gives the bounds 
 *  WRect=[<x-upperleft>,<y-upperleft>,largeur,hauteur]
 *    example WRect=[0,0,1.0,1.0] we use all the window 
 *            WRect=[0.5,0.5,0.5,0.5] we use the down right 
 *            quarter of the window 
 *-------------------------------------------*/

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
 * uses WRect,ARect,FRect,logscale to update 
 * current scale (current_scale) 
 *  WRect gives the subwindow to use 
 *  ARect gives the axis rectangle 
 *  FRect gives the bounds 
 *  WRect=[<x-upperleft>,<y-upperleft>,largeur,hauteur]
 *    example WRect=[0,0,1.0,1.0] we use all the window 
 *            WRect=[0.5,0.5,0.5,0.5] we use the down right 
 *            quarter of the window
 *  logscale : gives xy log axis flags 
 *  each argument can be a null pointer if they are 
 *  not to be changed from their current value 
 * 
 *  Each window can have a set of scales : one for each specified 
 *  subwindow. This routine must take care of properly 
 *  switching from one scale to an other.
 *  
 *-------------------------------------------*/

int Nsetscale2d(BCG *Xgc,double WRect[4],double ARect[4],double FRect[4],char *logscale)
{
  /* if some arguments are null pointer we set them to 
   * the corresponding current_scale value. 
   * this is only important for store_NEch which do not work with null arguments 
   */ 
  /* char flag[] = "tfffff";*/ /* flag to specify which arguments have changed*/
  char flag[7];
  strcpy(flag,"tfffff");
  if ( WRect != NULL) 
    {
      flag[1]='t';
      /* a subwindow is specified */
      if ( ! same_subwin(WRect,Xgc->scales->subwin_rect)) 
	{
	  if ( move_subwindow_scale_to_front(Xgc,WRect)==FAIL);
	  if ( xgc_add_scale(Xgc,&default_scale)== FAIL)
	    return 0;
	  else 
	    Xgc->scales->flag = 0;
	}
    }
  else WRect = Xgc->scales->subwin_rect;
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
  if (Xgc->graphic_engine->xget_recording(Xgc) == TRUE) store_NEch(Xgc,flag,WRect,ARect,FRect,logscale);
  set_scale(Xgc,flag,WRect,FRect,NULL,logscale,ARect);
  return(0);
}

/* used to send values to Scilab */

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

/*-------------------------------------------
 * changes selected items in the current scale 
 * flag gives which component must be used for 
 *      upgrading or setting the current scale 
 * flag[0]   : used for window dim upgrade 
 * flag[1:5] : subwin,frame_values,aaint,logflag,axis_values
 * Result: current_scale is changed 
 * Warning : frame_values[i] must be log10(val[i]) 
 *           when using log scales 
 *-------------------------------------------*/

void set_scale(BCG *Xgc,
	       char flag[6],            /* flag[i] = 't' or 'f' */
	       double  subwin[4],       /* subwindow specification */
	       double  frame_values[4], /* [xmin,ymin,xmax,ymax] */
	       int aaint[4],        /* [xint,x_subint,y_int,y_subint]*/
	       char logflag[2],        /* [xlogflag,ylogflag] */
	       double axis_values[4])   /* [mfact_xl, mfact_xr,mfact_yu,mfact_yd]; */
{
  char wdim_changed= 'f',subwin_changed='f';
  char frame_values_changed='f',aaint_changed='f';
  char logflag_changed='f';
  char axis_changed = 'f';
  int wdim[2];
  int i;

  if ( flag[1] == 't' ) 
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
      Xgc->scales->flag = 1;
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

void get_cwindow_dims( int wdims[2])
{
  BCG *Xgc;
  Xgc=check_graphic_window();
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






