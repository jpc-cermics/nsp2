/* Nsp
 * Copyright (C) 1998-2005 Jean-Philippe Chancelier Enpc/Cermics
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

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "nsp/interf.h"
#include "nsp/menus.h" 
#include "../system/files.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/gtksci.h"

/*
 * Now the interfaced function for basic menus 
 */

/* get an utf8 string matrix 
 * an eventual copy is performed if the string is 
 * to be converted.
 */

/* FIXME */
extern int nsp_smatrix_to_utf8(NspSMatrix *A);
extern int nsp_smatrix_utf8_validate(NspSMatrix *A);

static NspSMatrix *GetSMatUtf8(Stack stack,int pos)
{
  NspSMatrix *Sm;
  if ((Sm = GetSMat(stack,pos)) == NULLSMAT) return NULLSMAT;
  if ( nsp_smatrix_utf8_validate(Sm) == FALSE )
    {
      /* need to copy first */
      if ((Sm = GetSMatCopy(stack,pos)) == NULLSMAT) return NULLSMAT;
      if ( nsp_smatrix_to_utf8(Sm) == FAIL) 
	{
	  Scierror("%s: failed to convert %s to utf8\n",stack.fname,ArgPosition(pos));
	  return NULLSMAT;
	}
    }
  return Sm;
}

/* get an utf8 string 
 * an eventual copy is performed if the string is 
 * to be converted.
 */

static char *GetStringUtf8(Stack stack,int pos)
{
  NspSMatrix *Sm;
  if ((Sm = GetSMatUtf8(stack,pos)) == NULLSMAT) return NULL;
  if ( Sm->mn != 1 ) 
    {
      Scierror("%s: %s should be a string\n",stack.fname,ArgPosition(pos));
      return NULL;
    }
  return Sm->S[0];
}

/* get a copy of a string matrix converted to utf8 
 */

static NspSMatrix *GetSMatCopyUtf8(Stack stack,int pos)
{
  NspSMatrix *Sm;
  if ((Sm = GetSMatCopy(stack,pos)) == NULLSMAT) return NULLSMAT;
  if ( nsp_smatrix_utf8_validate(Sm) == FALSE )
    {
      if ( nsp_smatrix_to_utf8(Sm) == FAIL) 
	{
	  Scierror("%s: failed to convert %s to utf8\n",stack.fname,ArgPosition(pos));
	  return NULLSMAT;
	}
    }
  return Sm;
}

/*
 * interface for x_message 
 */

int int_x_message(Stack stack, int rhs, int opt, int lhs)
{
  integer nrep;
  NspSMatrix *Message;
  NspSMatrix *Buttons=NULLSMAT;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((Message = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((Buttons = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
      if ( Buttons->mn != 1 &&Buttons->mn != 2) 
	{
	  Scierror("%s: second argument should be of size 1 or 2 \n",stack.fname);
	  return RET_BUG;
	}
    }
  if ( nsp_message(Message,Buttons,&nrep) == FAIL) return RET_BUG;
  if ( rhs == 1) return 0;
  if ( rhs == 2) 
    {
      NspObject *O1;
      if ( Buttons->mn != 2 ) return 0;
      if (( O1 =nsp_create_object_from_double(NVOID,nrep)) == NULLOBJ ) return RET_BUG;
      MoveObj(stack,1,O1);
      return 1;
    }
  return 0;
}

/*
 * interface for modeless message 
 */

int int_x_message_modeless(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *Message;
  NspSMatrix *Buttons=NULLSMAT;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((Message = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((Buttons = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
      if ( Buttons->mn != 1 ) 
	{
	  Scierror("%s: second argument should be of size 1 \n",stack.fname);
	  return RET_BUG;
	}
    }
  if ( nsp_message_modeless(Message,Buttons) == FAIL) return RET_BUG;
  return 0;
}

/*
 * x_choose 
 */

int int_x_choose(Stack stack, int rhs, int opt, int lhs)
{
  int nrep;
  NspObject *O1;
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
  if ( nsp_choose(Items,Title,button,&nrep) == FAIL) return RET_BUG;
  if (( O1 =nsp_create_object_from_double(NVOID,nrep)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,O1);
  return 1;
}

/*
 * x_dialog
 */

int int_x_dialog(Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  NspObject *O1;
  NspSMatrix *Init;
  NspSMatrix *Title;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Title = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Init  = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
  rep= nsp_dialog(Title,Init,&O1);
  switch (rep) 
    {
    case  FAIL : 
      /* cancel or dialog deleted */
      if ((O1 = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0))== NULLOBJ ) return RET_BUG;
      break;
    case OK: 
      if ( O1 == NULLOBJ) 
	{
	  /* memory failure */
	  return RET_BUG;
	}
      break;
    default :
      Scierror("Error: internal error nsp_dialog returns a wrong result\n");
      return RET_BUG;
      break;
    }
  MoveObj(stack,1,O1);
  return 1;
}

/*
 * x_mdialog
 */

int int_x_mdialog(Stack stack, int rhs, int opt, int lhs)
{
  int cancel;
  NspObject *O1;
  NspSMatrix *Title,*Labels,*Init_values;
  NspSMatrix *Labels_v, *Labels_h,*Init_matrix;
  CheckRhs(3,4);
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
	  Scierror("%s: second and third argument should be of equal size\n",stack.fname);
	  return RET_BUG;
	}
      if ( Labels->mn == 0 ) 
	{
	  /* return empty string */
	  Scierror("%s: second and third argument are of size 0\n",stack.fname);
	  return RET_BUG;
	}
      if ( nsp_multi_dialog(Title,Labels,Init_values,&cancel) == FAIL) return RET_BUG;
      if ( cancel == 1 ) 
	{
	  if ((O1=(NspObject *) nsp_smatrix_create(NVOID,0,0,"v",0))== NULLOBJ) return RET_BUG; 
	  MoveObj(stack,1,O1);
	}
      else 
	{
	  NSP_OBJECT(Init_values)->ret_pos = 1;
	} 
    }
  else 
    {
      if ((Labels_v  = GetSMatUtf8(stack,2)) == NULLSMAT) return RET_BUG;
      if ((Labels_h  = GetSMatUtf8(stack,3)) == NULLSMAT) return RET_BUG;
      if ((Init_matrix  = GetSMatCopyUtf8(stack,4)) == NULLSMAT) return RET_BUG;
      if ( nsp_matrix_dialog(Title,Labels_v,Labels_h,Init_matrix,&cancel) == FAIL) return RET_BUG;
      if ( cancel == 1 ) 
	{
	  if ((O1=(NspObject *) nsp_smatrix_create(NVOID,0,0,"v",0))== NULLOBJ) return RET_BUG; 
	  MoveObj(stack,1,O1);
	}
      else 
	{
	  NSP_OBJECT(Init_matrix)->ret_pos = 1;
	}
    }
  return 1;
}


/*
 * interface for set/unset menus 
 */
typedef void men_f(int win_num, const char *button_name, int ne);

int int_set_unset_menu(Stack stack, int rhs, int opt, int lhs, men_f *F)
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
      Scierror("%s: Error\n",stack.fname);
      return RET_BUG;
    }
  return 0;
}

int int_set_menu(Stack stack, int rhs, int opt, int lhs)
{
  return int_set_unset_menu(stack,rhs,opt,lhs, nsp_menus_set);
}

int int_unset_menu(Stack stack, int rhs, int opt, int lhs)
{
  return int_set_unset_menu(stack, rhs, opt, lhs, nsp_menus_unset);

}

/*
 * addmenu 
 */

int int_add_menu(Stack stack, int rhs, int opt, int lhs)
{
  char *button=NULL,*mname=NULL;
  integer win,zero=0,ierr=0, typ=0,gwin=-1;
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
	  Scierror("%s: last argument should be a list of length 2\n",stack.fname);
	  return RET_BUG;
	}
      if ((O1 =nsp_list_get_element(L, 1))== NULLOBJ) return RET_BUG;
      if ( IsMat(O1) && ((NspMatrix *) O1)->mn == 1 && ((NspMatrix *) O1)->rc_type == 'r') 
	{
	  typ = (int) ((NspMatrix *) O1)->R[0];
	}
      else 
	{
	  Scierror("%s: last argument should be a list of length 2 = (scalar,string) \n",stack.fname);
	  return RET_BUG;
	}
      if ((O1 =nsp_list_get_element(L,2))== NULLOBJ) return RET_BUG;
      if ( IsSMat(O1) && ((NspSMatrix *) O1)->mn == 1 ) 
	{
	  mname = ((NspSMatrix *) O1)->S[0];
	}
      else 
	{
	  Scierror("%s: last argument should be a list of length 2 = (scalar,string) \n",stack.fname);
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
      win = scig_change(gwin);
    } 

  if ( mname == NULL) mname = button;

  if ( SubMenus != NULLSMAT)
    ierr= nsp_menus_add(gwin,button,SubMenus->S,SubMenus->mn,typ,mname);
  else 
    ierr= nsp_menus_add(gwin,button,NULL,zero,typ,mname);

  if ( gwin != -1 ) 
    {
      win = scig_change(gwin);
    } 

  if (ierr == FAIL ) 
    {
      Scierror("%s: error \n",stack.fname);
      return RET_BUG;
    }
  return(0);
}


/*
 * delmenu 
 */

int int_delmenu(Stack stack, int rhs, int opt, int lhs)
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
 
int int_xgetfile(Stack stack, int rhs, int opt, int lhs)
{
  int ierr=0,rep,action=FALSE,save=FALSE,open=FALSE,free_f=0;
  NspObject *Rep;
  NspSMatrix *Masks=NULL;
  char *dirname = NULL,dir_expanded[FSIZE+1];
  char *title = "Choose file name", *title_utf8=NULL;
  char *res = NULL;
  char def_res[] = "";
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "dir",string,NULLOBJ,-1},
		      { "masks",smat,NULLOBJ,-1},
		      { "title",string,NULLOBJ,-1},
		      { "action",s_bool,NULLOBJ,-1},
		      { "save",s_bool,NULLOBJ,-1},
		      { "open",s_bool,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&dirname,&Masks,&title,&action,&save,&open) == FAIL) 
    return RET_BUG;

  if ( dirname != NULL ) 
    {
      char *dirname_utf8;
      if ((dirname_utf8= nsp_string_to_utf8(dirname)) == NULL) {
	Scierror("Error: cannot convert dir to utf8\n");
	return RET_BUG;
      }
      nsp_path_expand(dirname_utf8,dir_expanded,FSIZE);
      if ( dirname_utf8 != dirname ) g_free (dirname_utf8);
      dirname = dir_expanded;
    }

  if ((title_utf8= nsp_string_to_utf8(title)) == NULL) {
    Scierror("Error: cannot convert title to utf8\n");
    return RET_BUG;
  }

  if ( save == TRUE ) 
    {
      /* specific dialog for saving */
      if (( res= nsp_get_filename_save(title_utf8,dirname)) != NULL)
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
  else 
    {
      rep= nsp_get_file_window(title_utf8,dirname,action,&res,&ierr);
      if ( ierr != 0) return RET_BUG; 
      if ( rep == FALSE )
	res = def_res;
      else 
	free_f=2;
    }
  if (( Rep =nsp_create_object_from_str(res))==NULLOBJ ) goto ret_bug;
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

int int_x_choices(Stack stack, int rhs, int opt, int lhs)
{
  Cell *Loc;
  int count=0,m,use_table = FALSE;
  NspMatrix *M;
  NspSMatrix *Title;
  NspList *ListItems ; 
  nsp_string title; 
  CheckRhs(2,3);
  CheckLhs(0,2);
  if ((Title = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ((ListItems  = GetListCopy(stack,2)) == NULLLIST) return RET_BUG;
  if ( rhs == 3 ) 
    {
     if ( GetScalarBool (stack,3,&use_table) == FAIL) return RET_BUG;
    }
  if ((title =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1))== NULL) return RET_BUG;

  /* walk throught list, check if its OK and convert to Utf8 */

  if (  nsp_check_choice_list(stack,ListItems) == FAIL) return RET_BUG;

  /* run the widget */

  if ( nsp_choices_with_combobox(title,ListItems,use_table) == FAIL) 
    {
      if ((M= nsp_matrix_create(NVOID,'r',0,0))== NULLMAT) return RET_BUG; 
    }
  else 
    {
      /* walk throught list and collect results */
      m =nsp_list_length(ListItems);
      if ((M= nsp_matrix_create(NVOID,'r',m,1))== NULLMAT) return RET_BUG; 
      /* walk though the list */ 
      Loc = ListItems->first;
      count = 0;

      while (Loc != NULL) 
	{
	  NspMatrix *active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->O);
	  M->R[count]= active_field->R[0]+1;
	  Loc= Loc->next;
	  count++;
	}
    }
  MoveObj(stack,1,(NspObject *) M);
  if ( lhs == 2 ) NSP_OBJECT(ListItems)->ret_pos = 2;
  return Max(lhs,1); 
}

/*
 * checks that a list is of the form 
 * list(string,string,scalar,smat)
 * and convert strings to utf8 
 *
 */

static int nsp_check_choice(Stack stack,NspList *L)
{
  NspSMatrix *MS1,*MS2,*MS3;
  int i;
  int_types Tc[]={ smat,smat,s_int,smat ,list_end} ;
  if ( GetListArgs(L,1,Tc,&MS1,&MS2,&i,&MS3) == FAIL) return FAIL;
  if (  nsp_smatrix_to_utf8(MS1) == FAIL 
	|| nsp_smatrix_to_utf8(MS2) == FAIL
	|| nsp_smatrix_to_utf8(MS3) == FAIL )
    {
      Scierror("Error: in %s conversion to utf8 failed\n",stack.fname);
      return FAIL;
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
	      char *arg =nsp_object_get_name((NthObj(2)));
	      if (  strcmp(arg ,NVOID) != 0)
		{
		  Scierror("Error: second argument of function %s has a wrong element %s(%d) \n",
			   stack.fname,arg,count);
		}
	      else 
		{
		  Scierror("Error: element %d of second argument of function %s is wrong\n",count,stack.fname);
		}
	      return FAIL;
	    }
	}
      else
	{
	  char *arg =nsp_object_get_name((NthObj(2)));
	  if (  strcmp(arg ,NVOID) != 0)
	    Scierror("Error: second argument of function %s, %s(%d) is not a list\n",
		     stack.fname,arg,count);
	  else 
	    Scierror("Error: element %d of second argument of function %s is not a list\n",
		     count,stack.fname);
	  return FAIL;
	}
      Loc= Loc->next;
      count++;
    }
  return OK;
}

/* just a test 
 *
 */

static int int_print_menu(Stack stack, int rhs, int opt, int lhs)
{
  static NspList *L=NULL;
  char *format[] = {"Postscript", "Postscript No Preamble",  "Postscript-Latex",
		    "Xfig",  "Gif", "PPM",  NULL };
  char *printer[]={ "lpr ", NULL};
  char *color[]={ "color", "black and white",NULL};
  char *modes[]={"landscape", "portrait", "keep size",NULL };
  char *save[]={"",NULL};
  NspSMatrix *S;
  NspList *L1,*L2,*L3,*L4,*L5;
  NspObject *Obj;
  int_types Ret[]={ string ,string, s_int ,smatcopy , t_end};
  int_types Ret1[]={ obj,obj,obj,obj,obj, t_end};
  CheckRhs(0,0); 
  /* test the list builder **/
  if ( L == NULL) 
    {
      if (( S = nsp_smatrix_create_from_table(modes)) == NULL) return RET_BUG;
      if (( L1 = BuildListFromArgs(Ret,"combo","mode",1,S)) == NULL ) return RET_BUG;
      if (( S = nsp_smatrix_create_from_table(color)) == NULL) return RET_BUG;
      if (( L2 = BuildListFromArgs(Ret,"combo","color",0,S)) == NULL ) return RET_BUG;
      if (( S = nsp_smatrix_create_from_table(format)) == NULL) return RET_BUG;
      if (( L3 = BuildListFromArgs(Ret,"combo","format",0,S)) == NULL ) return RET_BUG;
      if (( S = nsp_smatrix_create_from_table(printer)) == NULL) return RET_BUG;
      if (( L4 = BuildListFromArgs(Ret,"entry","print command",0,S)) == NULL ) return RET_BUG;
      if (( S = nsp_smatrix_create_from_table(save)) == NULL) return RET_BUG;
      if (( L5 = BuildListFromArgs(Ret,"save","file name",0,S)) == NULL ) return RET_BUG;
      if (( L = BuildListFromArgs(Ret1,L4,L5,L3,L2,L1))== NULL) return RET_BUG;
    }
  if ( nsp_choices_with_combobox("Print dialog",L,TRUE) == FAIL)
    return RET_BUG;
  if ((Obj=nsp_object_copy(NSP_OBJECT(L)))== NULLOBJ) return RET_BUG;
  MoveObj(stack,1,Obj);
  return 1;
}



/* 
 * creates a combo box for choosing colors and 
 * return the combo box 
 * (see demo) 
 */

static int int_nsp_gtkcombobox_colormap_new(Stack stack, int rhs, int opt, int lhs)
{
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
}

/* 
 * menu to get a color id for the current window 
 */

static int int_nsp_choose_color(Stack stack, int rhs, int opt, int lhs)
{
  int col,init=-1;
  BCG *Xgc;
  NspObject *O1;
  CheckRhs(0,1);
  if ( rhs == 1) 
    {
      if ( GetScalarInt(stack,1,&init) == FAIL) return RET_BUG;
      init = Max(init,0);
    }
  Xgc=check_graphic_window();
  col = gtkcombobox_select_color(Xgc,init);
  if (( O1 =nsp_create_object_from_double(NVOID,col)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,O1);
  return 1;
}

/*
 * The Interface for basic matrices operation 
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
  {"x_choices",int_x_choices},
  {"gtk_combo_colormap_new",int_nsp_gtkcombobox_colormap_new},
  {"choose_color",int_nsp_choose_color},
  {"print_menu",int_print_menu},
  {(char *) 0, NULL}
};

int Menus_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Menus_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Menus_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Menus_func[i].name;
  *f = Menus_func[i].fonc;
}






