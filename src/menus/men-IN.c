 /*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include <math.h>
#include <stdio.h>
#include <string.h>

#include "nsp/interf.h"
#include "nsp/menus.h" 
#include "nsp/menus.h" 
#include "../system/files.h"
#include "nsp/graphics/Graphics.h"
#include "nsp/gtksci.h" 

/***************************************************
 * Now the interfaced function for basic menus 
 ***************************************************/

int int_x_message(Stack stack, int rhs, int opt, int lhs)
{
  integer nrep;
  NspSMatrix *Message;
  NspSMatrix *Buttons=NULLSMAT;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((Message = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((Buttons = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
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

int int_x_message_modeless(Stack stack, int rhs, int opt, int lhs)
{
  NspSMatrix *Message;
  NspSMatrix *Buttons=NULLSMAT;
  CheckRhs(1,2);
  CheckLhs(0,1);
  if ((Message = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 2) 
    {
      if ((Buttons = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
      if ( Buttons->mn != 1 ) 
	{
	  Scierror("%s: second argument should be of size 1 \n",stack.fname);
	  return RET_BUG;
	}
    }
  if ( nsp_message_modeless(Message,Buttons) == FAIL) return RET_BUG;
  return 0;
}

/*************************************************************
 * x_choose 
 *************************************************************/

int int_x_choose(Stack stack, int rhs, int opt, int lhs)
{
  int nrep;
  NspObject *O1;
  NspSMatrix *Items;
  NspSMatrix *Title;
  NspSMatrix *button = NULL;
  CheckRhs(2,3);
  CheckLhs(0,1);
  if ((Items = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Title = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((button = GetSMat(stack,3)) == NULLSMAT) return RET_BUG;
    }
  if ( nsp_choose(Items,Title,button,&nrep) == FAIL) return RET_BUG;
  if (( O1 =nsp_create_object_from_double(NVOID,nrep)) == NULLOBJ ) return RET_BUG;
  MoveObj(stack,1,O1);
  return 1;
}

/*************************************************************
 * x_dialog
 *************************************************************/

int int_x_dialog(Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  NspObject *O1;
  NspSMatrix *Init;
  NspSMatrix *Title;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Title = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((Init  = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
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

/*************************************************************
 * x_mdialog
 *************************************************************/

int int_x_mdialog(Stack stack, int rhs, int opt, int lhs)
{
  int cancel;
  NspObject *O1;
  NspSMatrix *Title,*Labels,*Init_values;
  NspSMatrix *Labels_v, *Labels_h,*Init_matrix;
  CheckRhs(3,4);
  CheckLhs(0,1);
  if ((Title = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ( rhs == 3 ) 
    {
      if ((Labels  = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
      if ((Init_values  = GetSMatCopy(stack,3)) == NULLSMAT) return RET_BUG;
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
      if ((Labels_v  = GetSMat(stack,2)) == NULLSMAT) return RET_BUG;
      if ((Labels_h  = GetSMat(stack,3)) == NULLSMAT) return RET_BUG;
      if ((Init_matrix  = GetSMatCopy(stack,4)) == NULLSMAT) return RET_BUG;
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


/*************************************************************
 * setmenu 
 *************************************************************/

typedef int men_f(int *win_num, char *button_name, int *entries, int *ptrentries, int *ne, int *ierr);

extern men_f nsp_menus_set;
extern men_f nsp_menus_unset;

int int_set_unset_menu(Stack stack, int rhs, int opt, int lhs, men_f *F)
{
  int zero=0;
  int gwin=-1,nsub=0,ierr=0;
  char *button;

  if ( nsp_is_gtk_window() == FALSE) return 0;

  CheckRhs(1,3);
  
  if (IsMatObj(stack,1)) 
    {
      /* setmenu(gwin,button [,nsub]) */
      if ( GetScalarInt(stack,1,&gwin) == FAIL) return RET_BUG;
      if ((button = GetString(stack,2)) == (char*)0) return RET_BUG;
      if ( rhs == 3 ) 
	{
	  if ( GetScalarInt(stack,3,&nsub) == FAIL) return RET_BUG;
	}
    }
  else 
    {
      if ((button = GetString(stack,1)) == (char*)0) return RET_BUG;
      if ( rhs == 2 ) 
	{
	  if ( GetScalarInt(stack,2,&nsub) == FAIL) return RET_BUG;
	}
    }
  (*F)(&gwin,button,&zero,&zero,&nsub,&ierr);
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


/*************************************************************
 * addmenu 
 *************************************************************/

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
      if ((button = GetString(stack,2)) == (char*)0) return RET_BUG;
      if ( rhs >= 3 && IsSMatObj(stack,3))
	{
	  if ((SubMenus = GetSMat(stack,3))== NULLSMAT) return RET_BUG;
	}
    }
  else 
    {
      if ((button= GetString(stack,1)) == (char*)0) return RET_BUG;
      if ( nsp_is_gtk_window() == FALSE) 
	{
	  /* we are in -nw mod we ignore add menu  ....*/ 
	  return 0;
	}
      if ( rhs >= 2 && IsSMatObj(stack,2))
	{
	  if ((SubMenus = GetSMat(stack,2))== NULLSMAT) return RET_BUG;
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


/*************************************************************
 * delmenu 
 *************************************************************/

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
      if ((button = GetString(stack,2)) == (char*)0) return RET_BUG;
    }
  else 
    {
      if ((button = GetString(stack,1)) == (char*)0) return RET_BUG;
    }
  nsp_menus_delete_button(&gwin,button);
  return 0;
}

/*************************************************************
 * xgetfile 
 *************************************************************/
     
static int get_file(char *filemask, char *dirname,char *title,char **res)
{
  static char def_filemask[]="*";
  static char def_title[]="Choose file name";
  static char dir_expanded[FSIZE+1];
  int flag=0,rep,ierr=0;
  if ( dirname != NULL ) 
    {
      flag =1 ;
      nsp_path_expand(dirname,dir_expanded,FSIZE);
    }
  if ( title == NULL) title = def_title;
  if ( filemask == NULL) filemask = def_filemask;
  rep = nsp_get_file_window(filemask,res,dir_expanded,flag,0,&ierr,title);
  if ( ierr >= 1 || rep == FALSE )  return FAIL;
  return OK;
}


int int_xgetfile(Stack stack, int rhs, int opt, int lhs)
{
  NspObject *Rep;
  char *dir = NULL;
  char *title = NULL;
  char *filemask = NULL;
  char *res; 
  int_types T[] = {opts, t_end} ;
  /* 3 optional named arguments */
  /* names of optional arguments: must be NULL terminated*/
  char *Names[]={"dir","mask","title",NULL};
  /* types of optional arguments */
  int_types Topt[]={string,string,string, t_end} ;
  /* table to store optional arguments */ 
  NspObject *Tab[3]; 
  /* table to store optional arguments position */ 
  int posi[3];
  /* structure for optional arguments */
  named_opts N = {3, Names, Topt,Tab, posi};
  /* N.n =  4 ; N.names= Names, N.types = Topt, N.objs = Tab; */
  if ( GetArgs(stack,rhs,opt,T,&N,&dir,&filemask,&title) == FAIL) return RET_BUG;
  if ( get_file(filemask,dir,title,&res) == FAIL) return RET_BUG;
  if (( Rep =nsp_create_object_from_str(res))==NULLOBJ ) return RET_BUG;
  FREE(res);
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
  String *title; 
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((Title = GetSMat(stack,1)) == NULLSMAT) return RET_BUG;
  if ((ListItems  = GetList(stack,2)) == NULLLIST) return RET_BUG;
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
	  StringDestroy(&(Items->S[Items->mn-1]));
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
  StringDestroy(&title);
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






