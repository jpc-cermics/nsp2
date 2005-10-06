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
  if (rep == FAIL)
    {
      /* cancel */
      if ((O1 = (NspObject *) nsp_smatrix_create(NVOID,0,0,NULL,0))== NULLOBJ ) return RET_BUG;
    }
  else 
    {
      /* lack of memory */
      if ( O1 == NULLOBJ) return RET_BUG;
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
    nsp_menus_add(&gwin,button,SubMenus->S,&SubMenus->mn,&typ,mname,&ierr);
  else 
    nsp_menus_add(&gwin,button,NULL,&zero,&typ,mname,&ierr);

  if ( gwin != -1 ) 
    {
      win = scig_change(gwin);
    } 

  if (ierr != 0 ) 
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
  nsp_menus_delete_button(&gwin,button);
  return 0;
}

/*
 * xgetfile 
 */
     


int int_xgetfile(Stack stack, int rhs, int opt, int lhs)
{
  int ierr=0,rep,flag=0;
  NspObject *Rep;
  char *dir = NULL, dir_expanded[FSIZE+1];
  char *title = "Choose file name";
  char *filemask = "*";
  char *res = NULL; 
  int_types T[] = {new_opts, t_end} ;

  nsp_option opts[] ={{ "dir",string,NULLOBJ,-1},
		      { "mask",string,NULLOBJ,-1},
		      { "title",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  if ( GetArgs(stack,rhs,opt,T,&opts,&dir,&filemask,&title) == FAIL) return RET_BUG;

  if ( dir != NULL ) 
    {
      flag = 1 ;
      nsp_path_expand(dir,dir_expanded,FSIZE);
    }

  rep= nsp_get_file_window(filemask,&res,dir_expanded,flag,0,&ierr,title); 
  if ( ierr != 0) return RET_BUG; 
  if ( rep == FALSE ) 
    {
      if (( Rep =nsp_create_object_from_str(""))==NULLOBJ ) return RET_BUG;
    }
  else
    {
      if (( Rep =nsp_create_object_from_str(res))==NULLOBJ ) return RET_BUG;
      FREE(res);
    }
  MoveObj(stack,1,Rep);
  return 1;
}  


/*************************************************************
 * choices 
 *************************************************************/

static int check_sub_list(Stack stack,NspList *L,int count,NspSMatrix **Item,int *def,NspSMatrix **values)
{
  Cell *Loc;
  Loc = L->first;
  if ( Loc == NULLCELL) 
    {
      Scierror("%s: list item(%d) is null\n",stack.fname,count);
      return FAIL;
    }
  if (Loc->O == NULLOBJ) 
    {
      Scierror("%s: list item(%d,1) is null\n",stack.fname,count);
      return FAIL;
    }
  if ( ! IsSMat(Loc->O) ) 
    {
      Scierror("%s: list item(%d,1) is not a string \n",stack.fname,count);
      return FAIL;
    }
  *Item = (NspSMatrix *) Loc->O;
  if ( (*Item)->mn != 1 ) 
    {
      Scierror("%s: list item(%d,1) is not a string \n",stack.fname,count);
      return FAIL;
    }
  /* since we are working on a copy we can convert on place */
  if (  nsp_smatrix_to_utf8(*Item) == FAIL) 
    {
      Scierror("%s: list item(%d,1) conversion to utf8 failed\n",stack.fname,count);
    }
  Loc = Loc->next ; 
  /* a scalar */ 
  if ( Loc == NULLCELL) 
    {
      Scierror("%s: list item(%d,2) is null\n",stack.fname,count);
      return FAIL;
    }
  if (Loc->O == NULLOBJ) 
    {
      Scierror("%s: list item(%d,2) is null\n",stack.fname,count);
      return FAIL;
    }
  if ( ! IsMat(Loc->O) ) 
    {
      Scierror("%s: list item(%d,2) is not a scalar \n",stack.fname,count);
      return FAIL;
    }
  if ( ((NspMatrix *) Loc->O)->mn != 1 || ((NspMatrix *) Loc->O)->rc_type != 'r')
    {
      Scierror("%s: list item(%d,1) is not a real scalar \n",stack.fname,count);
      return FAIL;
    }
  *def = (int) ((NspMatrix *) Loc->O)->R[0];
  /* A string Matrix */ 
  Loc = Loc->next ; 
  if ( Loc == NULLCELL) 
    {
      Scierror("%s: list item(%d,2) is null\n",stack.fname,count);
      return FAIL;
    }
  if (Loc->O == NULLOBJ) 
    {
      Scierror("%s: list item(%d,2) is null\n",stack.fname,count);
      return FAIL;
    }
  if ( ! IsSMat(Loc->O) ) 
    {
      Scierror("%s: list item(%d,2) is not a string matrix \n",stack.fname,count);
      return FAIL;
    }
  *values = (NspSMatrix *) Loc->O; 
  /* since we are working on a copy we can convert on place */
  if (  nsp_smatrix_to_utf8(*values) == FAIL) 
    {
      Scierror("%s: list item(%d,1) conversion to utf8 failed\n",stack.fname,count);
    }

  return OK;
}

/* XXXX should improve the cleaning process in case of problems */ 

int int_x_choices(Stack stack, int rhs, int opt, int lhs)
{
  Cell *Loc;
  int *def,count=0,m;
  NspObject *Sep;
  NspSMatrix *Title,*Item,*Values,*Items=NULL;
  NspMatrix *M;
  NspList *ListItems ; 
  nsp_string title; 
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Title = GetSMatUtf8(stack,1)) == NULLSMAT) return RET_BUG;
  if ((ListItems  = GetListCopy(stack,2)) == NULLLIST) return RET_BUG;
  if ((title =nsp_smatrix_elts_concat(Title,"\n",1,"\n",1))== NULL) return RET_BUG;
  /* walk throught list and build arguments */
  m =nsp_list_length(ListItems);
  if ((M= nsp_matrix_create(NVOID,'r',m,1))== NULLMAT) return RET_BUG; 
  if ( m == 0 ) 
    {
      MoveObj(stack,1,(NspObject *) M);
      return 1;
    }

  def = (int *) M->R;
  M->convert = 'i';

  if ( (Sep =nsp_create_object_from_str(""))== NULLOBJ) return RET_BUG;

  /* walk though the list */ 
  Loc = ListItems->first;
  while ( Loc != NULLCELL) 
    {
      int idef;
      NspObject *lsub= Loc->O;
      count++; 
      if (lsub == NULLOBJ) 
	{
	  Scierror("%s: list item %d is null\n",stack.fname,count);
	  return RET_BUG;
	}
      if ( ! IsList(lsub) ) 
	{
	  Scierror("%s: list item %d is not a list \n",stack.fname,count);
	  return RET_BUG;
	}
      if (nsp_list_length((NspList *) lsub) != 3 ) 
	{
	  Scierror("%s: list item %d is not a list \n",stack.fname,count);
	  return RET_BUG;
	}
      if ( check_sub_list(stack,(NspList *)lsub,count,&Item,&idef,&Values) == FAIL) 
	{
	  return RET_BUG;
	}
      def[count-1]=idef;
      /* store Item and Values */ 
      if ( count == 1) 
	{
	  if ((Items =nsp_smatrix_copy(Item)) == NULLSMAT) return RET_BUG;
	  Values->n = Values->mn;  Values->m = 1;
	  if ( nsp_smatrix_concat_right(Items,Values) == FAIL) return RET_BUG;
	}
      else 
	{
	  if ( nsp_smatrix_concat_right(Items,(NspSMatrix *)Sep) == FAIL) return RET_BUG;
	  /* SciChoices use NULL as sep */ 
	  nsp_string_destroy(&(Items->S[Items->mn-1]));
	  Items->S[Items->mn-1]= NULL;
	  if ( nsp_smatrix_concat_right(Items,Item) == FAIL) return RET_BUG;
	  Values->n = Values->mn;  Values->m = 1;
	  if ( nsp_smatrix_concat_right(Items,Values) == FAIL) return RET_BUG;
	}
      Loc = Loc->next;
    }
  if ( nsp_choices(title,Items->S, def, count) == FAIL)  return RET_BUG;
  if ( def[0] == -1 ) 
    {
      /* this is a cancel */
      if ( nsp_matrix_resize(M,0,0) == FAIL) return RET_BUG;
    }
  nsp_smatrix_destroy(Items);
  nsp_string_destroy(&title);
  MoveObj(stack,1,(NspObject *) M);
  return 1;
}


/*************************************************************
 * The Interface for basic matrices operation 
 *************************************************************/

static OpTab Menus_func[]={
#include "men-IN.nam"
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






