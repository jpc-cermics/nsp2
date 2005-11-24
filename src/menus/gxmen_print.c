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
 * menu for graphic print and export
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"
#include "nsp/interf.h"

int nsp_print_dialog(char **print_command,int *type,int *orientation,int *format)
{
  static NspList *L=NULL;
  char *title="Print dialog";
  char *formats[] = {"Postscript", "Postscript No Preamble",  
		    "Postscript-Latex","Xfig","Gif","PPM", NULL };
  char *printer[]={ "lpr ", NULL};
  char *types[]={ "color", "black and white",NULL};
  char *orientations[]={"landscape", "portrait", "keep size",NULL };
  NspSMatrix *S;
  NspList *L1,*L2,*L3,*L4;
  int_types Ret[]={ string ,string, s_int ,smatcopy , t_end};
  int_types Ret1[]={ obj,obj,obj,obj, t_end};
  /* test the list builder **/
  if ( L == NULL) 
    {
      if (( S = nsp_smatrix_create_from_table(formats)) == NULL) return FAIL;
      if (( L1 = BuildListFromArgs(Ret,"combo","Format",1,S)) == NULL ) return FAIL;
      if (( S = nsp_smatrix_create_from_table(types)) == NULL) return FAIL;
      if (( L2 = BuildListFromArgs(Ret,"combo","Type",0,S)) == NULL ) return FAIL;
      if (( S = nsp_smatrix_create_from_table(orientations)) == NULL) return FAIL;
      if (( L3 = BuildListFromArgs(Ret,"combo","Orientation",0,S)) == NULL ) return FAIL;
      if (( S = nsp_smatrix_create_from_table(printer)) == NULL) return FAIL;
      if (( L4 = BuildListFromArgs(Ret,"entry","print command",0,S)) == NULL ) return FAIL;
      if (( L = BuildListFromArgs(Ret1,L4,L3,L2,L1))== NULL) return FAIL;
    }
  if ( nsp_choices_with_combobox(title,L,TRUE) == FAIL) return FAIL;
  {
    NspMatrix *active_field;
    NspSMatrix *Ms;
    /* L4 is an entry */
    Cell *Loc= L->first;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    Ms = ((NspSMatrix *) ((NspList *) Loc->O)->first->next->next->next->O);
    if (( *print_command =new_nsp_string(Ms->S[0])) == NULLSTRING) return FAIL;
    /* L3 is a combo */
    Loc= Loc->next;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    *orientation= (int) active_field->R[0];
    /* L2 is a combo */
    Loc= Loc->next;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    *type = (int)active_field->R[0];
    /* L1 is a combo */
    Loc= Loc->next;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    *format = (int)active_field->R[0];
  }
  return OK;
}

int nsp_export_dialog(char **file,int *type,int *orientation,int *format)
{
  static NspList *L=NULL;
  char *title="Export dialog";
  char *formats[] = {"Postscript", "Postscript No Preamble",  
		    "Postscript-Latex","Xfig","Gif","PPM", NULL };
  char *types[]={ "color", "black and white",NULL};
  char *orientations[]={"landscape", "portrait", "keep size",NULL };
  char *save[]={"Untitled.eps",NULL};
  NspSMatrix *S;
  NspList *L1,*L2,*L3,*L4;
  int_types Ret[]={ string ,string, s_int ,smatcopy , t_end};
  int_types Ret1[]={ obj,obj,obj,obj, t_end};
  /* test the list builder **/
  if ( L == NULL) 
    {
      if (( S = nsp_smatrix_create_from_table(formats)) == NULL) return FAIL;
      if (( L1 = BuildListFromArgs(Ret,"combo","Format",1,S)) == NULL ) return FAIL;
      if (( S = nsp_smatrix_create_from_table(types)) == NULL) return FAIL;
      if (( L2 = BuildListFromArgs(Ret,"combo","Type",0,S)) == NULL ) return FAIL;
      if (( S = nsp_smatrix_create_from_table(orientations)) == NULL) return FAIL;
      if (( L3 = BuildListFromArgs(Ret,"combo","Orientation",0,S)) == NULL ) return FAIL;
      if (( S = nsp_smatrix_create_from_table(save)) == NULL) return FAIL;
      if (( L4 = BuildListFromArgs(Ret,"save","file name",0,S)) == NULL ) return FAIL;
      if (( L = BuildListFromArgs(Ret1,L4,L3,L2,L1))== NULL) return FAIL;
    }
  if ( nsp_choices_with_combobox(title,L,TRUE) == FAIL) return FAIL;
  {
    NspMatrix *active_field;
    NspSMatrix *Ms;
    /* L4 is an entry */
    Cell *Loc= L->first;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    Ms = ((NspSMatrix *) ((NspList *) Loc->O)->first->next->next->next->O);
    if (( *file =new_nsp_string(Ms->S[0])) == NULLSTRING) return FAIL;
    /* L3 is a combo */
    Loc= Loc->next;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    *orientation= (int) active_field->R[0];
    /* L2 is a combo */
    Loc= Loc->next;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    *type = (int)active_field->R[0];
    /* L1 is a combo */
    Loc= Loc->next;
    active_field = ((NspMatrix *) ((NspList *) Loc->O)->first->next->next->O);
    *format = (int)active_field->R[0];
  }
  return OK;
}
