/* Nsp
 * Copyright (C) 1998-2019 Jean-Philippe Chancelier Enpc/Cermics
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
 * A set of predefined gtk menus.
 *
 */

#include <nsp/nsp.h>
#include <nsp/graphics-new/Graphics.h>
#include <nsp/menus.h>
#include <nsp/stack.h>
#include <nsp/interf.h> 
#include <nsp/system.h>
#include <nsp/gtksci.h>
#include <nsp/smatrix.h>
#include <nsp/nspthreads.h>

/*
 * Now the interfaced function for basic menus 
 */

/*
 * interface for x_message 
 */

static int int_x_message(Stack stack, int rhs, int opt, int lhs)
{
  menu_answer rep;
  int nrep;
  NspObject *Obj;
  NspSMatrix *Message;
  NspSMatrix *Buttons=NULLSMAT;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((Message = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((Buttons = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
    }
  rep= nsp_message(Message,Buttons,&nrep);
  switch (rep) 
    {
    case menu_bad_argument:
      Scierror("Error: bad argument given to %s \n",NspFname(stack));
      return RET_BUG;
    case menu_fail :
      Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
      return RET_BUG;
    case menu_cancel: 
      nrep=0;
    case menu_ok :
      if (( Obj =nsp_create_object_from_double(NVOID,nrep)) == NULLOBJ ) return RET_BUG;
      MoveObj(stack,1,Obj);
      return 1;
    }
  return 0;
}

/*
 * interface for modeless message 
 */

static int int_x_message_modeless(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *Message;
  NspSMatrix *Buttons=NULLSMAT;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((Message = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((Buttons = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
    }
  if ( nsp_message_modeless(Message,Buttons) == menu_fail ) return RET_BUG;
  return 0;
}

/*
 * x_choose 
 */

static int int_x_choose(Stack stack, int rhs, int opt, int lhs)
{
  menu_answer rep;
  int nrep;
  NspObject *Obj;
  NspSMatrix *Items;
  NspSMatrix *Title;
  NspSMatrix *button = NULL;
  CheckRhs(2,3);
  CheckLhs(0,1);
  if ((Items = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Title = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((button = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
    }
  rep = nsp_choose(Items,Title,button,&nrep);
  switch (rep) 
    {
    case menu_bad_argument:
      Scierror("Error: bad argument given to %s \n",NspFname(stack));
      return RET_BUG;
    case menu_fail :
      Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
      return RET_BUG;
    case menu_cancel: 
      nrep=0;
    case menu_ok :
      if (( Obj =nsp_create_object_from_double(NVOID,nrep)) == NULLOBJ ) return RET_BUG;
      MoveObj(stack,1,Obj);
      return 1;
    }
  return 0;
}

/*
 * x_dialog
 */

static int int_x_dialog(Stack stack, int rhs, int opt, int lhs)
{
  menu_answer rep;
  NspObject *Obj;
  NspSMatrix *Init;
  NspSMatrix *Title;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Title = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Init  = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
  rep= nsp_dialog(Title,Init,&Obj);
  switch (rep) 
    {
    case menu_bad_argument:
      Scierror("Error: bad argument given to %s \n",NspFname(stack));
      return RET_BUG;
    case menu_fail :
      Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
      return RET_BUG;
    case menu_cancel: 
      if ((Obj = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0))== NULLOBJ ) return RET_BUG;
      MoveObj(stack,1,Obj);
      return 1;
    case menu_ok :
      MoveObj(stack,1,Obj);
      return 1;
    }
  return 0;
}

/*
 * x_mdialog
 */

static int int_x_mdialog(Stack stack, int rhs, int opt, int lhs)
{
  menu_answer rep;
  NspObject *O1;
  NspSMatrix *Title,*Labels,*Init_values;
  NspSMatrix *Labels_v, *Labels_h,*Init_matrix;
  CheckStdRhs(3,4);
  CheckLhs(0,1);
  if ((Title = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((Labels  = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
      if ((Init_values  = GetSMatCopyUtf8(stack,3)) == NULLSMAT) return RET_BUG;
      Init_values->m =  Init_values->mn; 
      Init_values->n =  1;
      if ( Labels->mn != Init_values->mn ) 
	{
	  Scierror("%s: second and third argument should be of equal size\n",NspFname(stack));
	  return RET_BUG;
	}
      if ( Labels->mn == 0 ) 
	{
	  /* return empty string */
	  Scierror("%s: second and third argument are of size 0\n",NspFname(stack));
	  return RET_BUG;
	}
      rep = nsp_multi_dialog(Title,Labels,Init_values);
      switch (rep)
	{
	case menu_ok :
	  NSP_OBJECT(Init_values)->ret_pos = 1;
	  break;
	case menu_cancel :
	  if ((O1=(NspObject *) nsp_smatrix_create(NVOID,0,0,"v",0))== NULLOBJ) return RET_BUG; 
	  MoveObj(stack,1,O1);
	  break;
	case menu_bad_argument:
	  Scierror("Error: bad argument given to %s \n",NspFname(stack));
	  return RET_BUG;
	case menu_fail: 
	  Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
	  return RET_BUG;
	}
    }
  else 
    {
      int entry=FALSE;
      int entry_size=20;
      nsp_option opts[] ={{ "entry",s_bool,NULLOBJ,-1},
			  { "entry_size",s_int,NULLOBJ,-1},
			  { NULL,t_end,NULLOBJ,-1}};
      if ((Labels_v  = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
      if ((Labels_h  = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
      if ((Init_matrix  = GetSMatCopyUtf8(stack,4)) == NULLSMAT) return RET_BUG;
      if ( get_optional_args(stack,rhs,opt,opts,&entry,&entry_size) == FAIL) return RET_BUG;

      if ( Labels_v->mn != 0 && Labels_v->mn != Init_matrix->m ) 
	{
	  Scierror("%s: second and fourth argument have incompatible sizes\n",NspFname(stack));
	  return RET_BUG;
	}
      if ( Labels_h->mn != 0 &&  Labels_h->mn != Init_matrix->n ) 
	{
	  Scierror("%s: third and fourth argument have incompatible sizes\n",NspFname(stack));
	  return RET_BUG;
	}
      rep = nsp_matrix_dialog(Title,Labels_v->mn != 0 ? Labels_v : NULL,
			      Labels_h->mn != 0 ? Labels_h : NULL,Init_matrix,entry,entry_size);
      switch (rep)
	{
	case menu_ok :
	  NSP_OBJECT(Init_matrix)->ret_pos = 1;
	  break;
	case menu_cancel :
	  if ((O1=(NspObject *) nsp_smatrix_create(NVOID,0,0,"v",0))== NULLOBJ) return RET_BUG; 
	  MoveObj(stack,1,O1);
	  break;
	case menu_bad_argument:
	  Scierror("Error: bad argument given to %s \n",NspFname(stack));
	  return RET_BUG;
	case menu_fail: 
	  Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
	  return RET_BUG;
	}
    }
  return 1;
}


/**
 * int_xgetfile:
 * @stack: 
 * @rhs: 
 * @opt: 
 * @lhs: 
 * 
 * A unique function used to open a dialog widget with 
 * a GtkFileChooserDialog (open or save) or a 
 * GtkFileSelection.
 * 
 * Return value: %RET_BUG or 1 
 **/
 
static int int_xgetfile(Stack stack, int rhs, int opt, int lhs)
{
  int action=FALSE,save=FALSE,open=FALSE,folder=FALSE,free_f=0;
  NspObject *Rep;
  NspSMatrix *Masks=NULL;
  char *dirname = NULL,*filename=NULL,dir_expanded[FSIZE+1];
  char *title = "Choose file name", *title_utf8=NULL;
  char *res = NULL;
  char def_res[] = "";
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "dir",string,NULLOBJ,-1},
		      { "file",string,NULLOBJ,-1},
		      { "masks",smat,NULLOBJ,-1},
		      { "title",string,NULLOBJ,-1},
		      { "action",s_bool,NULLOBJ,-1},
		      { "save",s_bool,NULLOBJ,-1},
		      { "open",s_bool,NULLOBJ,-1},
		      { "folder",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&dirname,&filename,&Masks,&title,&action,&save,&open,&folder) == FAIL) 
    return RET_BUG;
  if ( save == TRUE && open == TRUE ) 
    {
      Scierror("Error: save and open cannot be both set to %t\n");
      return RET_BUG; 
    }

  if ( dirname != NULL ) 
    {
      char *dirname_utf8;
      /* expand keys in path name result in buf */
      nsp_expand_file_with_exec_dir(&stack,dirname,dir_expanded);

      if ((dirname_utf8= nsp_string_to_utf8(dir_expanded)) == NULL) {
	Scierror("Error: cannot convert dir to utf8\n");
	return RET_BUG;
      }
      if ( dirname_utf8 != dir_expanded ) 
	{
	  strncpy(dir_expanded,dirname_utf8,FSIZE);	  
	  g_free (dirname_utf8);
	}
      dirname = dir_expanded;
    }

  if ((title_utf8= nsp_string_to_utf8(title)) == NULL) {
    Scierror("Error: cannot convert title to utf8\n");
    return RET_BUG;
  }

#if defined(GTK_DISABLE_DEPRECATED)
  if ( save == FALSE && open == FALSE && folder == FALSE ) 
    {
      Sciprintf("Warning: gtk_file_selection is deprecated, you have to use save, or open, or folder flag in %s\n",
		NspFname(stack));
      return RET_BUG;
    }
#endif 

  if ( save == TRUE ) 
    {
      char **masks= NULL;
      /* specific dialog for saving */
      if ( Masks != NULL) 
	{
	  if ( Masks->mn %2 != 0) {
	    Scierror("masks should be given as a 2xm sized matrix\n");
	    return RET_BUG;
	  }
	  masks = Masks->S;
	}
      if (( res= nsp_get_filename_save(title_utf8,dirname,filename,masks)) != NULL)
	  free_f= 1;
      else 
	res = def_res;
    }
  else if ( open == TRUE )
    {
      char **masks= NULL;
      /* specific dialog for opening */
      if ( Masks != NULL) 
	{
	  if ( Masks->mn %2 != 0) {
	    Scierror("masks should be given as a 2xm sized matrix\n");
	    return RET_BUG;
	  }
	  masks = Masks->S;
	}
      if ((res = nsp_get_filename_open(title_utf8,dirname,masks)) != NULL)
	free_f= 1;
      else 
	res = def_res;
    }
  else if (folder == TRUE )
    {
      /* specific dialog for saving */
      if (( res= nsp_get_filename_folder(title_utf8,dirname)) != NULL)
	free_f= 1;
      else 
	res = def_res;
    }
#if !defined(GTK_DISABLE_DEPRECATED)
  else 
    {
      menu_answer rep= nsp_get_file_window(title_utf8,dirname,action,&res);
      switch (rep)
	{
	case menu_cancel : res = def_res;break;
	case menu_ok : 	free_f=2;break;
	case menu_bad_argument:
	  Scierror("Error: bad argument given to %s \n",NspFname(stack));
	  return RET_BUG;
	case menu_fail : 
	  Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
	  return RET_BUG;
	}
    }
#endif 
  if (( Rep =nsp_create_object_from_str(NVOID,res))==NULLOBJ ) goto ret_bug;
  MoveObj(stack,1,Rep);
  if (title_utf8 != NULL&& title_utf8 != title ) g_free (title_utf8);
  return 1;
 ret_bug: 
  switch (free_f)
    {
    case 1: g_free(res);break;
    case 2: FREE(res);break;
    }
  if ( title_utf8 != NULL && title_utf8 != title ) g_free (title_utf8);
  return RET_BUG;
}  


/*
 * choices 
 */

static int nsp_check_choice_list(Stack stack,NspList *L);

static int int_x_choices(Stack stack, int rhs, int opt, int lhs)
{
  menu_answer rep;
  Cell *Loc;
  int count=0,m,use_table = FALSE;
  NspMatrix *M=NULL;
  NspSMatrix *Title=NULL;
  NspList *ListItems=NULL,*Res=NULL ; 
  nsp_string title=NULL; 
  CheckRhs(2,3);
  CheckLhs(0,3);
  if ((Title = GetSMatUtf8(stack,1)) == NULLSMAT) goto err;
  if ((ListItems  = GetListCopy(stack,2)) == NULLLIST) goto err;
  if ( rhs == 3 ) 
    {
     if ( GetScalarBool (stack,3,&use_table) == FAIL) goto err;
    }
  if ((title =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1))== NULL) goto err;

  /* walk throught list, check if its OK and convert to Utf8 */

  if (  nsp_check_choice_list(stack,ListItems) == FAIL) goto err;

  /* run the widget */

  rep = nsp_choices_with_combobox(title,ListItems,&Res,use_table);
  switch (rep)
    {
    case menu_ok :
      /* walk throught list and collect results */
      m =nsp_list_length(ListItems);
      if ((M= nsp_matrix_create(NVOID,'r',m,1))== NULLMAT) goto err; 
      /* walk though the list */ 
      Loc = ListItems->first;
      count = 0;
      while (Loc != NULL) 
	{
	  NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
	  M->R[count]= active_field->R[0];
	  Loc= Loc->next;
	  count++;
	}
      break;
    case menu_cancel :
      if ((M= nsp_matrix_create(NVOID,'r',0,0))== NULLMAT) goto err; 
      break;
    case menu_fail: 
      Scierror("Error: lack of memory or internal error in %s \n",NspFname(stack));
      goto err;
    case menu_bad_argument:
      Scierror("Error: bad argument given to %s \n",NspFname(stack));
      goto err;
    }
  /* first returns the list */
  nsp_string_destroy(&title);
  MoveObj(stack,1,NSP_OBJECT(Res));
  if ( lhs >= 2 ) 
    {
      NSP_OBJECT(ListItems)->ret_pos = 2;
    }
  if ( lhs == 3 )
    {
      MoveObj(stack,3,NSP_OBJECT(M));
    }
  else 
    {
      nsp_matrix_destroy(M);
    }
  return Max(lhs,1); 
 err:
  if (title != NULL) nsp_string_destroy(&title);
  nsp_matrix_destroy(M);
  nsp_list_destroy(Res);
  return RET_BUG;
  
}

/*
 * checks that a list is of the form 
 * list(string,string,scalar,smat)
 * and convert strings to utf8 
 *
 */

static int nsp_check_choice(Stack stack,NspList *L)
{
  NspSMatrix *MS1=NULL,*MS2=NULL;
  NspObject *M3=NULL;
  int i;
  int_types Tc1[]={ smat,smat,s_int,obj ,list_end} ;
  
  if ( GetListArgs(L,1,Tc1,&MS1,&MS2,&i,&M3) == FAIL) return FAIL;

  if ( MS1->mn >= 1 &&  
       ( strcmp(MS1->S[0],"spin") == 0 || strcmp(MS1->S[0],"range") == 0) ) 
    {
      if (  nsp_smatrix_to_utf8(MS1) == FAIL 
	    || nsp_smatrix_to_utf8(MS2) == FAIL )
	{
	  Scierror("Error: in %s conversion to utf8 failed\n",NspFname(stack));
	  return FAIL;
	}
      if ( !( IsMat(M3) && ((NspMatrix *)M3)->mn==8) ) 
	{
	  Scierror("Error: Argument should be a Matrix of size 8\n");
	  Scierror("       while parsing a list argument (element 4)\n");
	  return FAIL;
	}
    }
  else if ( MS1->mn >= 1 && ( strcmp(MS1->S[0],"button") == 0 ))
    {
      if ( ! IsList(M3)) 
	{
	  Scierror("Error: Argument should be a List\n");
	  Scierror("       while parsing a list argument (element 4)\n");
	  return FAIL;
	}
      if ( nsp_check_choice_list(stack,(NspList *)M3) != OK ) 
	{
	  Scierror("Error: Argument list is incorrect\n");
	  Scierror("       while parsing a list argument (element 4)\n");
	  return FAIL;
	}
    }
  else
    {
      if ( !( IsSMat(M3)))
	{
	  Scierror("Error: Argument should be a SMat\n");
	  Scierror("       while parsing a list argument (element 4)\n");
	  return FAIL;
	}
      if (  nsp_smatrix_to_utf8(MS1) == FAIL 
	    || nsp_smatrix_to_utf8(MS2) == FAIL 
	    || nsp_smatrix_to_utf8((NspSMatrix *)M3) == FAIL )
	{
	  Scierror("Error: in %s conversion to utf8 failed\n",NspFname(stack));
	  return FAIL;
	}
    }
  return OK; 
}

static int nsp_check_choice_list(Stack stack,NspList *L)
{
  Cell *Loc = L->first;
  int count=1;
  while (Loc != NULL) 
    {
      if (Loc->O != NULL && IsList(Loc->O)) 
	{
	  if ( nsp_check_choice(stack,(NspList *) Loc->O)==FAIL)
	    {
	      const char *arg =nsp_object_get_name((NthObj(2)));
	      if (  strcmp(arg ,NVOID) != 0)
		{
		  Scierror("Error: second argument of function %s has a wrong element %s(%d) \n",
			   NspFname(stack),arg,count);
		}
	      else 
		{
		  Scierror("Error: element %d of second argument of function %s is wrong\n",count,NspFname(stack));
		}
	      return FAIL;
	    }
	}
      else
	{
	  const char *arg =nsp_object_get_name((NthObj(2)));
	  if (  strcmp(arg ,NVOID) != 0)
	    Scierror("Error: second argument of function %s, %s(%d) is not a list\n",
		     NspFname(stack),arg,count);
	  else 
	    Scierror("Error: element %d of second argument of function %s is not a list\n",
		     count,NspFname(stack));
	  return FAIL;
	}
      Loc= Loc->next;
      count++;
    }
  return OK;
}


/* 
 * creates a combo box for choosing colors and 
 * return the combo box as a nsp encapsulated gtk widget
 * (see demo) 
 */

static int int_nsp_gtkcombobox_colormap_new(Stack stack, int rhs, int opt, int lhs)
{
#if 0
  BCG *Xgc;
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);

  Xgc=check_graphic_window();
  if ((ret = (GObject *) nsp_gtkcombobox_colormap_new(Xgc,-1))== NULL) return RET_BUG;
  nsp_type_gtkcombobox = new_type_gtkcombobox(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gtkcombobox );
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
#endif 
  return RET_BUG;
}

/* 
 * menu to get a color id for the current window 
 */

static int int_nsp_choose_color(Stack stack, int rhs, int opt, int lhs)
{
  int col,init=-1,window=-1;
  NspMatrix *colormap=NULL;
  BCG *Xgc;
  NspObject *O1;
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { "colormap",realmat,NULLOBJ,-1},
		      { "window",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&opts,&init,&colormap,&window) == FAIL) 
    return RET_BUG;
  init = Max(init,1);
  if ( window != -1 )
    {
      /* we want the colormap of  a specific graphic window 
       * note that the window is created if it does not exists.
       */
      Xgc = window_list_search(window);
      if ( Xgc == NULL) Xgc= set_graphic_window(Max(window,0)); 
      col = gtkcombobox_select_color(Xgc,init); 
    }
  else
    {
      if ( colormap == NULL )
	{
	  Xgc=check_graphic_window();
	  col = gtkcombobox_select_color(Xgc,init-1); 
	}
      else
	{
	  if ( colormap->n != 3 ) 
	    {
	      Scierror("%s:optional argument colormap should be of size mx3\n",NspFname(stack));
	      return RET_BUG;
	    }
	  /* No use to open a graphic window we use the 
	   * provided colormap 
	   */
	  col = gtkcombobox_select_color_in_table(colormap,init-1);
	}
    }
  if (( O1 =nsp_create_object_from_double(NVOID,col)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,O1);
  return 1;
}


/*
 * interface for set/unset menus 
 */

typedef void men_f(int win_num, const char *button_name, int ne);

static int int_set_unset_menu(Stack stack, int rhs, int opt, int lhs, men_f *F)
{
  int gwin=-1,nsub=0,ierr=0;
  char *button;

  if ( nsp_is_gtk_window() == FALSE) return 0;

  CheckRhs(1,3);
  
  if (IsMatObj(stack,1)) 
    {
      /* setmenu(gwin,button [,nsub]) */
      if ( GetScalarInt(stack,1,&gwin) == FAIL) return RET_BUG;
      if ((button = GetStringUtf8(stack,2)) == (char*)0) return RET_BUG;
      if ( rhs == 3 ) 
	{
	  if ( GetScalarInt(stack,3,&nsub) == FAIL) return RET_BUG;
	}
    }
  else 
    {
      if ((button = GetStringUtf8(stack,1)) == (char*)0) return RET_BUG;
      if ( rhs == 2 ) 
	{
	  if ( GetScalarInt(stack,2,&nsub) == FAIL) return RET_BUG;
	}
    }
  (*F)(gwin,button,nsub);
  if ( ierr != 0 ) 
    {
      Scierror("%s: Error\n",NspFname(stack));
      return RET_BUG;
    }
  return 0;
}

static int int_set_menu(Stack stack, int rhs, int opt, int lhs)
{
  return int_set_unset_menu(stack,rhs,opt,lhs, nsp_menus_set);
}

static int int_unset_menu(Stack stack, int rhs, int opt, int lhs)
{
  return int_set_unset_menu(stack, rhs, opt, lhs, nsp_menus_unset);

}

/*
 * addmenu 
 */

static int int_add_menu(Stack stack, int rhs, int opt, int lhs)
{
  char *button=NULL,*mname=NULL;
  int zero=0,ierr=0, typ=0,gwin=-1;
  NspSMatrix *SubMenus = NULL;

  CheckRhs(1,4);
  CheckLhs(0,1);
  if (IsListObj(stack,rhs)) 
    {
      NspList *L;
      NspObject *O1;
      /* last variable is list(typ,mname) */
      if ((L = GetList(stack,rhs)) == NULLLIST) return RET_BUG;
      if (nsp_list_length(L) != 2 ) 
	{
	  Scierror("%s: last argument should be a list of length 2\n",NspFname(stack));
	  return RET_BUG;
	}
      if ((O1 =nsp_list_get_element(L, 1))== NULLOBJ) return RET_BUG;
      if ( IsMat(O1) && ((NspMatrix *) O1)->mn == 1 && ((NspMatrix *) O1)->rc_type == 'r') 
	{
	  typ = (int) ((NspMatrix *) O1)->R[0];
	}
      else 
	{
	  Scierror("%s: last argument should be a list of length 2 = (scalar,string) \n",NspFname(stack));
	  return RET_BUG;
	}
      if ((O1 =nsp_list_get_element(L,2))== NULLOBJ) return RET_BUG;
      if ( IsSMat(O1) && ((NspSMatrix *) O1)->mn == 1 ) 
	{
	  mname = ((NspSMatrix *) O1)->S[0];
	}
      else 
	{
	  Scierror("%s: last argument should be a list of length 2 = (scalar,string) \n",NspFname(stack));
	  return RET_BUG;
	}
    }
      
  /* first argument */
  
  if (IsMatObj(stack,1)) 
    {
      if ( GetScalarInt(stack,1,&gwin) == FAIL) return RET_BUG;
      if ((button = GetStringUtf8(stack,2)) == (char*)0) return RET_BUG;
      if ( rhs >= 3 && IsSMatObj(stack,3))
	{
	  if ((SubMenus = GetSMatUtf8(stack,3))== NULLSMAT) return RET_BUG;
	}
    }
  else 
    {
      if ((button= GetStringUtf8(stack,1)) == (char*)0) return RET_BUG;
      if ( nsp_is_gtk_window() == FALSE) 
	{
	  /* we are in -nw mod we ignore add menu  ....*/ 
	  return 0;
	}
      if ( rhs >= 2 && IsSMatObj(stack,2))
	{
	  if ((SubMenus = GetSMatUtf8(stack,2))== NULLSMAT) return RET_BUG;
	}
    }
  
  if ( gwin != -1 ) 
    {
      nsp_gr_change(gwin);
    } 

  if ( mname == NULL) mname = button;

  if ( SubMenus != NULLSMAT)
    ierr= nsp_menus_add(gwin,button,SubMenus->S,SubMenus->mn,typ,mname);
  else 
    ierr= nsp_menus_add(gwin,button,NULL,zero,typ,mname);

  if ( gwin != -1 ) 
    {
      nsp_gr_change(gwin);
    } 

  if (ierr == FAIL ) 
    {
      Scierror("%s: error \n",NspFname(stack));
      return RET_BUG;
    }
  return(0);
}


/*
 * delmenu 
 */

static int int_delmenu(Stack stack, int rhs, int opt, int lhs)
{
  int gwin=-1;
  char *button;

  if ( nsp_is_gtk_window() == FALSE) return 0;

  CheckRhs(1,2);
  
  if (IsMatObj(stack,1)) 
    {
      /* setmenu(gwin,button [,nsub]) */
      if ( GetScalarInt(stack,1,&gwin) == FAIL) return RET_BUG;
      if ((button = GetStringUtf8(stack,2)) == (char*)0) return RET_BUG;
    }
  else 
    {
      if ((button = GetStringUtf8(stack,1)) == (char*)0) return RET_BUG;
    }
  nsp_menus_delete_button(gwin,button);
  return 0;
}


/*
 * The Interface for dialogs and menus operations
 */

static OpTab Menus_func[]={
  {"x_message",int_x_message},
  {"x_message_modeless",int_x_message_modeless},
  {"x_choose",int_x_choose},
  {"x_dialog",int_x_dialog},
  {"x_choices",int_x_choices},
  {"x_mdialog",int_x_mdialog},
  {"setmenu", int_set_menu},
  {"addmenu", int_add_menu},
  {"unsetmenu",int_unset_menu},
  {"delmenu",int_delmenu},
  {"xgetfile", int_xgetfile},
  {"gtk_combo_colormap_new",int_nsp_gtkcombobox_colormap_new},
  {"choose_color",int_nsp_choose_color},
  {(char *) 0, NULL}
};

int Menus_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,Menus_func[i].fonc,
					       &stack,rhs,opt,lhs);
#else 
  return (*(Menus_func[i].fonc))(stack,rhs,opt,lhs);
#endif 
}

/* used to walk through the interface table 
 *  (for adding or removing functions) 
 */

void Menus_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Menus_func[i].name;
  *f = Menus_func[i].fonc;
}






