/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 */





#line 30 "codegen/spolyhedron.override"
#include <nsp/object.h>
#include <nsp/spolyhedron.h>
#include <nsp/polyhedron.h>
#include <nsp/figuredata.h> 
#include <nsp/figure.h> 
#include "../graphics-new/Plo3dObj.h"
#include <nsp/grcommon.h>

#line 37 "spolyhedron.c"

/* ----------- NspSPolyhedron ----------- */


#define  NspSPolyhedron_Private 
#include <nsp/object.h>
#include <nsp/spolyhedron.h>
#include <nsp/interf.h>

/* 
 * NspSPolyhedron inherits from Graphic 
 */

int nsp_type_spolyhedron_id=0;
NspTypeSPolyhedron *nsp_type_spolyhedron=NULL;

/*
 * Type object for NspSPolyhedron 
 * all the instance of NspTypeSPolyhedron share the same id. 
 * nsp_type_spolyhedron: is an instance of NspTypeSPolyhedron 
 *    used for objects of NspSPolyhedron type (i.e built with new_spolyhedron) 
 * other instances are used for derived classes 
 */
NspTypeSPolyhedron *new_type_spolyhedron(type_mode mode)
{
  NspTypeSPolyhedron *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_spolyhedron != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_spolyhedron;
    }
  if (( type =  malloc(sizeof(NspTypeSPolyhedron))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_graphic(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = spolyhedron_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = spolyhedron_get_methods;
  type->new = (new_func *) new_spolyhedron;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for spolyhedron */ 

  top->pr = (print_func *) nsp_spolyhedron_print;
  top->dealloc = (dealloc_func *) nsp_spolyhedron_destroy;
  top->copy  =  (copy_func *) nsp_spolyhedron_copy;
  top->size  = (size_func *) nsp_spolyhedron_size;
  top->s_type =  (s_type_func *) nsp_spolyhedron_type_as_string;
  top->sh_type = (sh_type_func *) nsp_spolyhedron_type_short_string;
  top->info = (info_func *) nsp_spolyhedron_info;
  /* top->is_true = (is_true_func  *) nsp_spolyhedron_is_true; */
  /* top->loop =(loop_func *) nsp_spolyhedron_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_spolyhedron_object;
  top->eq  = (eq_func *) nsp_spolyhedron_eq;
  top->neq  = (eq_func *) nsp_spolyhedron_neq;
  top->save  = (save_func *) nsp_spolyhedron_xdr_save;
  top->load  = (load_func *) nsp_spolyhedron_xdr_load;
  top->create = (create_func*) int_spolyhedron_create;
  top->latex = (print_func *) nsp_spolyhedron_latex;
  top->full_copy = (copy_func *) nsp_spolyhedron_full_copy;

  /* specific methods for spolyhedron */

  type->init = (init_func *) init_spolyhedron;

#line 45 "codegen/spolyhedron.override"
  /* inserted verbatim in the type definition 
   * here we override the method og its father class i.e Graphic
   */
  ((NspTypeGraphic *) type->surtype)->draw = nsp_draw_spolyhedron;
  ((NspTypeGraphic *) type->surtype)->translate =nsp_translate_spolyhedron ;
  ((NspTypeGraphic *) type->surtype)->rotate =nsp_rotate_spolyhedron  ;
  ((NspTypeGraphic *) type->surtype)->scale =nsp_scale_spolyhedron  ;
  ((NspTypeGraphic *) type->surtype)->bounds =nsp_getbounds_spolyhedron  ;
  /* next method are defined in NspGraphic and need not be chnaged here for SPolyhedron */
  /* ((NspTypeGraphic *) type->surtype)->link_figure = nsp_graphic_link_figure; */ 
  /* ((NspTypeGraphic *) type->surtype)->unlink_figure = nsp_graphic_unlink_figure; */ 
  ((NspTypeGraphic *) type->surtype)->zmean = nsp_spolyhedron_zmean;
  ((NspTypeGraphic *) type->surtype)->n_faces = nsp_spolyhedron_n_faces;

#line 124 "spolyhedron.c"
  /* 
   * NspSPolyhedron interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_spolyhedron_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeSPolyhedron called nsp_type_spolyhedron
       */
      type->id =  nsp_type_spolyhedron_id = nsp_new_type_id();
      nsp_type_spolyhedron = type;
      if ( nsp_register_type(nsp_type_spolyhedron) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_spolyhedron(mode);
    }
  else 
    {
      type->id = nsp_type_spolyhedron_id;
      return type;
    }
}

/*
 * initialize NspSPolyhedron instances 
 * locally and by calling initializer on parent class 
 */

static int init_spolyhedron(NspSPolyhedron *Obj,NspTypeSPolyhedron *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  Obj->obj = NULL;
  return OK;
}

/*
 * new instance of NspSPolyhedron 
 */

NspSPolyhedron *new_spolyhedron() 
{
  NspSPolyhedron *loc;
  /* type must exists */
  nsp_type_spolyhedron = new_type_spolyhedron(T_BASE);
  if ( (loc = malloc(sizeof(NspSPolyhedron)))== NULLSPOLYHEDRON) return loc;
  /* initialize object */
  if ( init_spolyhedron(loc,nsp_type_spolyhedron) == FAIL) return NULLSPOLYHEDRON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspSPolyhedron 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_spolyhedron_size(NspSPolyhedron *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char spolyhedron_type_name[]="SPolyhedron";
static char spolyhedron_short_type_name[]="spolyhedron";

static char *nsp_spolyhedron_type_as_string(void)
{
  return(spolyhedron_type_name);
}

static char *nsp_spolyhedron_type_short_string(NspObject *v)
{
  return(spolyhedron_short_type_name);
}

/*
 * A == B 
 */

static int nsp_spolyhedron_eq(NspSPolyhedron *A, NspObject *B)
{
  NspSPolyhedron *loc = (NspSPolyhedron *) B;
  if ( check_cast(B,nsp_type_spolyhedron_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( NSP_OBJECT(A->obj->Mcoord)->type->eq(A->obj->Mcoord,loc->obj->Mcoord) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->Mface)->type->eq(A->obj->Mface,loc->obj->Mface) == FALSE ) return FALSE;
  if ( NSP_OBJECT(A->obj->Mval)->type->eq(A->obj->Mval,loc->obj->Mval) == FALSE ) return FALSE;
  if ( A->obj->vmin != loc->obj->vmin) return FALSE;
  if ( A->obj->vmax != loc->obj->vmax) return FALSE;
  if ( A->obj->colmin != loc->obj->colmin) return FALSE;
  if ( A->obj->colmax != loc->obj->colmax) return FALSE;
  if ( A->obj->coloutmin != loc->obj->coloutmin) return FALSE;
  if ( A->obj->coloutmax != loc->obj->coloutmax) return FALSE;
  if ( A->obj->mesh != loc->obj->mesh) return FALSE;
  if ( A->obj->back_color != loc->obj->back_color) return FALSE;
  if ( A->obj->shade != loc->obj->shade) return FALSE;
  if ( A->obj->Mcoord_l != loc->obj->Mcoord_l) return FALSE;
  {int i;
    for ( i = 0 ; i < A->obj->pos_length ; i++)
      if ( A->obj->pos[i] != loc->obj->pos[i]) return FALSE;
  }
  {int i;
    for ( i = 0 ; i < A->obj->fill_length ; i++)
      if ( A->obj->fill[i] != loc->obj->fill[i]) return FALSE;
  }
  {int i;
    for ( i = 0 ; i < A->obj->vlevel_length ; i++)
      if ( A->obj->vlevel[i] != loc->obj->vlevel[i]) return FALSE;
  }
  if ( A->obj->coldef != loc->obj->coldef) return FALSE;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_spolyhedron_neq(NspSPolyhedron *A, NspObject *B)
{
  return ( nsp_spolyhedron_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_spolyhedron_xdr_save(XDR *xdrs, NspSPolyhedron *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_spolyhedron)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mcoord)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mface)) == FAIL) return FAIL;
  if (nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->Mval)) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->vmin) == FAIL) return FAIL;
  if (nsp_xdr_save_d(xdrs, M->obj->vmax) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->colmin) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->colmax) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->coloutmin) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->coloutmax) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->mesh) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->back_color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->shade) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, M->obj->coldef) == FAIL) return FAIL;
  if ( nsp_graphic_xdr_save(xdrs, (NspGraphic *) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspSPolyhedron  *nsp_spolyhedron_xdr_load_partial(XDR *xdrs, NspSPolyhedron *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if ((M->obj->Mcoord =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->Mface =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if ((M->obj->Mval =(NspMatrix *) nsp_object_xdr_load(xdrs))== NULLMAT) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->vmin) == FAIL) return NULL;
  if (nsp_xdr_load_d(xdrs, &M->obj->vmax) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->colmin) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->colmax) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->coloutmin) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->coloutmax) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->mesh) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->back_color) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->shade) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &M->obj->coldef) == FAIL) return NULL;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_graphic_xdr_load_partial(xdrs,(NspGraphic *)M) == NULL) return NULL;
 return M;
}

static NspSPolyhedron  *nsp_spolyhedron_xdr_load(XDR *xdrs)
{
  NspSPolyhedron *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLSPOLYHEDRON;
  if ((H  = nsp_spolyhedron_create_void(name,(NspTypeBase *) nsp_type_spolyhedron))== NULLSPOLYHEDRON) return H;
  if ( nsp_spolyhedron_create_partial(H) == FAIL) return NULLSPOLYHEDRON;
  if ((H  = nsp_spolyhedron_xdr_load_partial(xdrs,H))== NULLSPOLYHEDRON) return H;
  if ( nsp_spolyhedron_check_values(H) == FAIL) return NULLSPOLYHEDRON;
#line 68 "codegen/spolyhedron.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_spolyhedron(NULL,H)== FAIL) return NULL; 

#line 329 "spolyhedron.c"
  return H;
}

/*
 * delete 
 */

void nsp_spolyhedron_destroy_partial(NspSPolyhedron *H)
{
  nsp_graphic_destroy_partial((NspGraphic *) H);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
#line 73 "codegen/spolyhedron.override"
  /* verbatim in destroy */
  nsp_matrix_destroy(H->obj->Mcoord_l);

#line 347 "spolyhedron.c"
    if ( H->obj->Mcoord != NULL ) 
      nsp_matrix_destroy(H->obj->Mcoord);
    if ( H->obj->Mface != NULL ) 
      nsp_matrix_destroy(H->obj->Mface);
    if ( H->obj->Mval != NULL ) 
      nsp_matrix_destroy(H->obj->Mval);
    FREE(H->obj->pos);
    FREE(H->obj->fill);
    FREE(H->obj->vlevel);
    FREE(H->obj);
   }
}

void nsp_spolyhedron_destroy(NspSPolyhedron *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_spolyhedron_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_spolyhedron_info(NspSPolyhedron *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLSPOLYHEDRON) 
    {
      Sciprintf("Null Pointer NspSPolyhedron \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_spolyhedron_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_spolyhedron_print(NspSPolyhedron *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLSPOLYHEDRON) 
    {
      Sciprintf("Null Pointer NspSPolyhedron \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=TO_BE_DONE();\n",pname);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_spolyhedron_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_spolyhedron_type_short_string(NSP_OBJECT(M)) ,M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mface != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mface),indent+2,"Mface",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mval != NULL)
    { if ( nsp_object_print(NSP_OBJECT(M->obj->Mval),indent+2,"Mval",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"vmin=%f\n",M->obj->vmin);
  Sciprintf1(indent+2,"vmax=%f\n",M->obj->vmax);
  Sciprintf1(indent+2,"colmin=%d\n",M->obj->colmin);
  Sciprintf1(indent+2,"colmax=%d\n",M->obj->colmax);
  Sciprintf1(indent+2,"coloutmin=%d\n",M->obj->coloutmin);
  Sciprintf1(indent+2,"coloutmax=%d\n",M->obj->coloutmax);
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"back_color=%d\n",M->obj->back_color);
  Sciprintf1(indent+2,"shade	= %s\n", ( M->obj->shade == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  Sciprintf1(indent+2,"coldef=%d\n",M->obj->coldef);
  nsp_graphic_print((NspGraphic *) M,indent+2,NULL,rec_level);
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_spolyhedron_latex(NspSPolyhedron *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_spolyhedron_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  if ( M->obj->Mcoord != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mcoord),indent+2,"Mcoord",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mface != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mface),indent+2,"Mface",rec_level+1)== FALSE ) return FALSE ;
    }
  if ( M->obj->Mval != NULL)
    { if ( nsp_object_latex(NSP_OBJECT(M->obj->Mval),indent+2,"Mval",rec_level+1)== FALSE ) return FALSE ;
    }
  Sciprintf1(indent+2,"vmin=%f\n",M->obj->vmin);
  Sciprintf1(indent+2,"vmax=%f\n",M->obj->vmax);
  Sciprintf1(indent+2,"colmin=%d\n",M->obj->colmin);
  Sciprintf1(indent+2,"colmax=%d\n",M->obj->colmax);
  Sciprintf1(indent+2,"coloutmin=%d\n",M->obj->coloutmin);
  Sciprintf1(indent+2,"coloutmax=%d\n",M->obj->coloutmax);
  Sciprintf1(indent+2,"mesh	= %s\n", ( M->obj->mesh == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"back_color=%d\n",M->obj->back_color);
  Sciprintf1(indent+2,"shade	= %s\n", ( M->obj->shade == TRUE) ? "T" : "F" );
  Sciprintf1(indent+2,"Mcoord_l=%xl\n",M->obj->Mcoord_l);
  Sciprintf1(indent+2,"coldef=%d\n",M->obj->coldef);
  nsp_graphic_latex((NspGraphic *) M,indent+2,NULL,rec_level);
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspSPolyhedron objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspSPolyhedron   *nsp_spolyhedron_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_spolyhedron_id) == TRUE ) return ((NspSPolyhedron *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_spolyhedron));
  return NULL;
}

int IsSPolyhedronObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_spolyhedron_id);
}

int IsSPolyhedron(NspObject *O)
{
  return nsp_object_type(O,nsp_type_spolyhedron_id);
}

NspSPolyhedron  *GetSPolyhedronCopy(Stack stack, int i)
{
  if (  GetSPolyhedron(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspSPolyhedron  *GetSPolyhedron(Stack stack, int i)
{
  NspSPolyhedron *M;
  if (( M = nsp_spolyhedron_object(NthObj(i))) == NULLSPOLYHEDRON)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspSPolyhedron instance 
 *-----------------------------------------------------*/

static NspSPolyhedron *nsp_spolyhedron_create_void(char *name,NspTypeBase *type)
{
 NspSPolyhedron *H  = (type == NULL) ? new_spolyhedron() : type->new();
 if ( H ==  NULLSPOLYHEDRON)
  {
   Sciprintf("No more memory\n");
   return NULLSPOLYHEDRON;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLSPOLYHEDRON;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_spolyhedron_create_partial(NspSPolyhedron *H)
{
  if ( nsp_graphic_create_partial((NspGraphic *) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_spolyhedron)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->Mcoord = NULLMAT;
  H->obj->Mface = NULLMAT;
  H->obj->Mval = NULLMAT;
  H->obj->vmin = 0.0;
  H->obj->vmax = 0.0;
  H->obj->colmin = -1;
  H->obj->colmax = -1;
  H->obj->coloutmin = -1;
  H->obj->coloutmax = -1;
  H->obj->mesh = TRUE;
  H->obj->back_color = 4;
  H->obj->shade = FALSE;
  H->obj->Mcoord_l = NULL;
  H->obj->pos = NULL; H->obj->pos_length = 0; 
  H->obj->fill = NULL; H->obj->fill_length = 0; 
  H->obj->vlevel = NULL; H->obj->vlevel_length = 0; 
  H->obj->coldef = 0;
  return OK;
}

int nsp_spolyhedron_check_values(NspSPolyhedron *H)
{
  if ( H->obj->Mcoord == NULLMAT) 
    {
       if (( H->obj->Mcoord = nsp_matrix_create("Mcoord",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->Mface == NULLMAT) 
    {
       if (( H->obj->Mface = nsp_matrix_create("Mface",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  if ( H->obj->Mval == NULLMAT) 
    {
       if (( H->obj->Mval = nsp_matrix_create("Mval",'r',0,0)) == NULLMAT)
       return FAIL;

    }
  nsp_graphic_check_values((NspGraphic *) H);
  return OK;
}

NspSPolyhedron *nsp_spolyhedron_create(char *name,NspMatrix* Mcoord,NspMatrix* Mface,NspMatrix* Mval,double vmin,double vmax,int colmin,int colmax,int coloutmin,int coloutmax,gboolean mesh,int back_color,gboolean shade,void* Mcoord_l,int* pos, int pos_length,int* fill, int fill_length,double* vlevel, int vlevel_length,int coldef,NspTypeBase *type)
{
 NspSPolyhedron *H  = nsp_spolyhedron_create_void(name,type);
 if ( H ==  NULLSPOLYHEDRON) return NULLSPOLYHEDRON;
  if ( nsp_spolyhedron_create_partial(H) == FAIL) return NULLSPOLYHEDRON;
  H->obj->Mcoord= Mcoord;
  H->obj->Mface= Mface;
  H->obj->Mval= Mval;
  H->obj->vmin=vmin;
  H->obj->vmax=vmax;
  H->obj->colmin=colmin;
  H->obj->colmax=colmax;
  H->obj->coloutmin=coloutmin;
  H->obj->coloutmax=coloutmax;
  H->obj->mesh=mesh;
  H->obj->back_color=back_color;
  H->obj->shade=shade;
  H->obj->Mcoord_l = Mcoord_l;
  H->obj->pos = pos;
  H->obj->pos_length = pos_length;
  H->obj->fill = fill;
  H->obj->fill_length = fill_length;
  H->obj->vlevel = vlevel;
  H->obj->vlevel_length = vlevel_length;
  H->obj->coldef=coldef;
 if ( nsp_spolyhedron_check_values(H) == FAIL) return NULLSPOLYHEDRON;
 return H;
}


NspSPolyhedron *nsp_spolyhedron_create_default(char *name)
{
 NspSPolyhedron *H  = nsp_spolyhedron_create_void(name,NULL);
 if ( H ==  NULLSPOLYHEDRON) return NULLSPOLYHEDRON;
  if ( nsp_spolyhedron_create_partial(H) == FAIL) return NULLSPOLYHEDRON;
 if ( nsp_spolyhedron_check_values(H) == FAIL) return NULLSPOLYHEDRON;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspSPolyhedron *nsp_spolyhedron_copy_partial(NspSPolyhedron *H,NspSPolyhedron *self)
{
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspSPolyhedron *nsp_spolyhedron_copy(NspSPolyhedron *self)
{
  NspSPolyhedron *H  =nsp_spolyhedron_create_void(NVOID,(NspTypeBase *) nsp_type_spolyhedron);
  if ( H ==  NULLSPOLYHEDRON) return NULLSPOLYHEDRON;
  if ( nsp_graphic_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSPOLYHEDRON;
  if ( nsp_spolyhedron_copy_partial(H,self)== NULL) return NULLSPOLYHEDRON;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspSPolyhedron *nsp_spolyhedron_full_copy_partial(NspSPolyhedron *H,NspSPolyhedron *self)
{
  if ((H->obj = calloc(1,sizeof(nsp_spolyhedron))) == NULL) return NULLSPOLYHEDRON;
  H->obj->ref_count=1;
  if ( self->obj->Mcoord == NULL )
    { H->obj->Mcoord = NULL;}
  else
    {
      if ((H->obj->Mcoord = (NspMatrix *) nsp_object_full_copy_and_name("Mcoord",NSP_OBJECT(self->obj->Mcoord))) == NULLMAT) return NULL;
    }
  if ( self->obj->Mface == NULL )
    { H->obj->Mface = NULL;}
  else
    {
      if ((H->obj->Mface = (NspMatrix *) nsp_object_full_copy_and_name("Mface",NSP_OBJECT(self->obj->Mface))) == NULLMAT) return NULL;
    }
  if ( self->obj->Mval == NULL )
    { H->obj->Mval = NULL;}
  else
    {
      if ((H->obj->Mval = (NspMatrix *) nsp_object_full_copy_and_name("Mval",NSP_OBJECT(self->obj->Mval))) == NULLMAT) return NULL;
    }
  H->obj->vmin=self->obj->vmin;
  H->obj->vmax=self->obj->vmax;
  H->obj->colmin=self->obj->colmin;
  H->obj->colmax=self->obj->colmax;
  H->obj->coloutmin=self->obj->coloutmin;
  H->obj->coloutmax=self->obj->coloutmax;
  H->obj->mesh=self->obj->mesh;
  H->obj->back_color=self->obj->back_color;
  H->obj->shade=self->obj->shade;
  H->obj->Mcoord_l = self->obj->Mcoord_l;
  if ((H->obj->pos = malloc(self->obj->pos_length*sizeof(int)))== NULL) return NULL;
  H->obj->pos_length = self->obj->pos_length;
  memcpy(H->obj->pos,self->obj->pos,self->obj->pos_length*sizeof(int));
  if ((H->obj->fill = malloc(self->obj->fill_length*sizeof(int)))== NULL) return NULL;
  H->obj->fill_length = self->obj->fill_length;
  memcpy(H->obj->fill,self->obj->fill,self->obj->fill_length*sizeof(int));
  if ((H->obj->vlevel = malloc(self->obj->vlevel_length*sizeof(double)))== NULL) return NULL;
  H->obj->vlevel_length = self->obj->vlevel_length;
  memcpy(H->obj->vlevel,self->obj->vlevel,self->obj->vlevel_length*sizeof(double));
  H->obj->coldef=self->obj->coldef;
  return H;
}

NspSPolyhedron *nsp_spolyhedron_full_copy(NspSPolyhedron *self)
{
  NspSPolyhedron *H  =nsp_spolyhedron_create_void(NVOID,(NspTypeBase *) nsp_type_spolyhedron);
  if ( H ==  NULLSPOLYHEDRON) return NULLSPOLYHEDRON;
  if ( nsp_graphic_full_copy_partial((NspGraphic *) H,(NspGraphic *) self ) == NULL) return NULLSPOLYHEDRON;
  if ( nsp_spolyhedron_full_copy_partial(H,self)== NULL) return NULLSPOLYHEDRON;
#line 68 "codegen/spolyhedron.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_spolyhedron(NULL,H)== FAIL) return NULL; 

#line 698 "spolyhedron.c"
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspSPolyhedron
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_spolyhedron_create(Stack stack, int rhs, int opt, int lhs)
{
  NspSPolyhedron *H;
  CheckStdRhs(0,0);
  /* want to be sure that type spolyhedron is initialized */
  nsp_type_spolyhedron = new_type_spolyhedron(T_BASE);
  if(( H = nsp_spolyhedron_create_void(NVOID,(NspTypeBase *) nsp_type_spolyhedron)) == NULLSPOLYHEDRON) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( nsp_spolyhedron_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_spolyhedron_check_values(H) == FAIL) return RET_BUG;
#line 68 "codegen/spolyhedron.override"
  /* verbatim in create/load/copy interface  */
  if ( nsp_check_spolyhedron(NULL,H)== FAIL) return RET_BUG; 

#line 722 "spolyhedron.c"
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *spolyhedron_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_spolyhedron_get_Mcoord(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspSPolyhedron *) self)->obj->Mcoord;
  return (NspObject *) ret;
}

static NspObject *_wrap_spolyhedron_get_obj_Mcoord(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSPolyhedron *) self)->obj->Mcoord);
  return (NspObject *) ret;
}

static int _wrap_spolyhedron_set_Mcoord(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mcoord;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mcoord = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSPolyhedron *) self)->obj->Mcoord != NULL ) 
    nsp_matrix_destroy(((NspSPolyhedron *) self)->obj->Mcoord);
  ((NspSPolyhedron *) self)->obj->Mcoord= Mcoord;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_Mface(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspSPolyhedron *) self)->obj->Mface;
  return (NspObject *) ret;
}

static NspObject *_wrap_spolyhedron_get_obj_Mface(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSPolyhedron *) self)->obj->Mface);
  return (NspObject *) ret;
}

static int _wrap_spolyhedron_set_Mface(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mface;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mface = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSPolyhedron *) self)->obj->Mface != NULL ) 
    nsp_matrix_destroy(((NspSPolyhedron *) self)->obj->Mface);
  ((NspSPolyhedron *) self)->obj->Mface= Mface;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_Mval(void *self,const char *attr)
{
  NspMatrix *ret;

  ret = ((NspSPolyhedron *) self)->obj->Mval;
  return (NspObject *) ret;
}

static NspObject *_wrap_spolyhedron_get_obj_Mval(void *self,const char *attr, int *copy)
{
  NspMatrix *ret;

  *copy = FALSE;
  ret = ((NspMatrix*) ((NspSPolyhedron *) self)->obj->Mval);
  return (NspObject *) ret;
}

static int _wrap_spolyhedron_set_Mval(void *self,const char *attr, NspObject *O)
{
  NspMatrix *Mval;

  if ( ! IsMat(O) ) return FAIL;
  if ((Mval = (NspMatrix *) nsp_object_copy_and_name(attr,O)) == NULLMAT) return FAIL;
  if (((NspSPolyhedron *) self)->obj->Mval != NULL ) 
    nsp_matrix_destroy(((NspSPolyhedron *) self)->obj->Mval);
  ((NspSPolyhedron *) self)->obj->Mval= Mval;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_vmin(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspSPolyhedron *) self)->obj->vmin;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_spolyhedron_set_vmin(void *self,const char *attr, NspObject *O)
{
  double vmin;

  if ( DoubleScalar(O,&vmin) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->vmin= vmin;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_vmax(void *self,const char *attr)
{
  double ret;
  NspObject *nsp_ret;

  ret = ((NspSPolyhedron *) self)->obj->vmax;
  nsp_ret=nsp_create_object_from_double(NVOID,(double) ret);
  return nsp_ret;
}

static int _wrap_spolyhedron_set_vmax(void *self,const char *attr, NspObject *O)
{
  double vmax;

  if ( DoubleScalar(O,&vmax) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->vmax= vmax;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_colmin(void *self,const char *attr)
{
  int ret;

  ret = ((NspSPolyhedron *) self)->obj->colmin;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_spolyhedron_set_colmin(void *self,const char *attr, NspObject *O)
{
  int colmin;

  if ( IntScalar(O,&colmin) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->colmin= colmin;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_colmax(void *self,const char *attr)
{
  int ret;

  ret = ((NspSPolyhedron *) self)->obj->colmax;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_spolyhedron_set_colmax(void *self,const char *attr, NspObject *O)
{
  int colmax;

  if ( IntScalar(O,&colmax) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->colmax= colmax;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_coloutmin(void *self,const char *attr)
{
  int ret;

  ret = ((NspSPolyhedron *) self)->obj->coloutmin;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_spolyhedron_set_coloutmin(void *self,const char *attr, NspObject *O)
{
  int coloutmin;

  if ( IntScalar(O,&coloutmin) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->coloutmin= coloutmin;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_coloutmax(void *self,const char *attr)
{
  int ret;

  ret = ((NspSPolyhedron *) self)->obj->coloutmax;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_spolyhedron_set_coloutmax(void *self,const char *attr, NspObject *O)
{
  int coloutmax;

  if ( IntScalar(O,&coloutmax) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->coloutmax= coloutmax;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_mesh(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspSPolyhedron *) self)->obj->mesh;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_spolyhedron_set_mesh(void *self,const char *attr, NspObject *O)
{
  int mesh;

  if ( BoolScalar(O,&mesh) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->mesh= mesh;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_back_color(void *self,const char *attr)
{
  int ret;

  ret = ((NspSPolyhedron *) self)->obj->back_color;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_spolyhedron_set_back_color(void *self,const char *attr, NspObject *O)
{
  int back_color;

  if ( IntScalar(O,&back_color) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->back_color= back_color;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_shade(void *self,const char *attr)
{
  int ret;
  NspObject *nsp_ret;

  ret = ((NspSPolyhedron *) self)->obj->shade;
  nsp_ret= (ret == TRUE) ? nsp_create_true_object(NVOID) : nsp_create_false_object(NVOID);
  return nsp_ret;
}

static int _wrap_spolyhedron_set_shade(void *self,const char *attr, NspObject *O)
{
  int shade;

  if ( BoolScalar(O,&shade) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->shade= shade;
  return OK;
}

static NspObject *_wrap_spolyhedron_get_coldef(void *self,const char *attr)
{
  int ret;

  ret = ((NspSPolyhedron *) self)->obj->coldef;
  return nsp_new_double_obj((double) ret);
}

static int _wrap_spolyhedron_set_coldef(void *self,const char *attr, NspObject *O)
{
  int coldef;

  if ( IntScalar(O,&coldef) == FAIL) return FAIL;
  ((NspSPolyhedron *) self)->obj->coldef= coldef;
  return OK;
}

static AttrTab spolyhedron_attrs[] = {
  { "Mcoord", (attr_get_function *)_wrap_spolyhedron_get_Mcoord, (attr_set_function *)_wrap_spolyhedron_set_Mcoord,(attr_get_object_function *)_wrap_spolyhedron_get_obj_Mcoord, (attr_set_object_function *)int_set_object_failed },
  { "Mface", (attr_get_function *)_wrap_spolyhedron_get_Mface, (attr_set_function *)_wrap_spolyhedron_set_Mface,(attr_get_object_function *)_wrap_spolyhedron_get_obj_Mface, (attr_set_object_function *)int_set_object_failed },
  { "Mval", (attr_get_function *)_wrap_spolyhedron_get_Mval, (attr_set_function *)_wrap_spolyhedron_set_Mval,(attr_get_object_function *)_wrap_spolyhedron_get_obj_Mval, (attr_set_object_function *)int_set_object_failed },
  { "vmin", (attr_get_function *)_wrap_spolyhedron_get_vmin, (attr_set_function *)_wrap_spolyhedron_set_vmin,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "vmax", (attr_get_function *)_wrap_spolyhedron_get_vmax, (attr_set_function *)_wrap_spolyhedron_set_vmax,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "colmin", (attr_get_function *)_wrap_spolyhedron_get_colmin, (attr_set_function *)_wrap_spolyhedron_set_colmin,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "colmax", (attr_get_function *)_wrap_spolyhedron_get_colmax, (attr_set_function *)_wrap_spolyhedron_set_colmax,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "coloutmin", (attr_get_function *)_wrap_spolyhedron_get_coloutmin, (attr_set_function *)_wrap_spolyhedron_set_coloutmin,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "coloutmax", (attr_get_function *)_wrap_spolyhedron_get_coloutmax, (attr_set_function *)_wrap_spolyhedron_set_coloutmax,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "mesh", (attr_get_function *)_wrap_spolyhedron_get_mesh, (attr_set_function *)_wrap_spolyhedron_set_mesh,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "back_color", (attr_get_function *)_wrap_spolyhedron_get_back_color, (attr_set_function *)_wrap_spolyhedron_set_back_color,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "shade", (attr_get_function *)_wrap_spolyhedron_get_shade, (attr_set_function *)_wrap_spolyhedron_set_shade,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { "coldef", (attr_get_function *)_wrap_spolyhedron_get_coldef, (attr_set_function *)_wrap_spolyhedron_set_coldef,(attr_get_object_function *)int_get_object_failed, (attr_set_object_function *)int_set_object_failed },
  { NULL,NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
#line 78 "codegen/spolyhedron.override"

extern function int_nspgraphic_extract;

int _wrap_nsp_extractelts_spolyhedron(Stack stack, int rhs, int opt, int lhs) 
{
  return int_nspgraphic_extract(stack,rhs,opt,lhs);
}

#line 1030 "spolyhedron.c"


#line 88 "codegen/spolyhedron.override"

extern function int_graphic_set_attribute;

int _wrap_nsp_setrowscols_spolyhedron(Stack stack, int rhs, int opt, int lhs) 
{
  return int_graphic_set_attribute(stack,rhs,opt,lhs);
}

#line 1042 "spolyhedron.c"


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab SPolyhedron_func[]={
  {"extractelts_spolyhedron", _wrap_nsp_extractelts_spolyhedron},
  {"setrowscols_spolyhedron", _wrap_nsp_setrowscols_spolyhedron},
  { "spolyhedron_create", int_spolyhedron_create},
  { NULL, NULL}
};

/* call ith function in the SPolyhedron interface */

int SPolyhedron_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(SPolyhedron_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void SPolyhedron_Interf_Info(int i, char **fname, function (**f))
{
  *fname = SPolyhedron_func[i].name;
  *f = SPolyhedron_func[i].fonc;
}

#line 98 "codegen/spolyhedron.override"

/* inserted verbatim at the end */

static void nsp_draw_spolyhedron(BCG *Xgc,NspGraphic *Obj, void *data)
{
  int face; 
  if ( Obj->obj->hidden == TRUE ) return ;
  nsp_check_spolyhedron(Xgc,(NspSPolyhedron *) Obj);
#ifdef  WITH_GTKGLEXT 
  if ( Xgc->graphic_engine == &GL_gengine ) 
    {
      /* if we are using OpenGl we make a full draw of 
       * object and return 
       */
      draw_spolyhedron_ogl(Xgc,Obj);
      nsp_ogl_set_2dview(Xgc); 
      return; 
    }
#endif 
  if ( data != NULL) 
    {
      face = *((int *) data);
      draw_spolyhedron_face(Xgc,Obj,face);
    }
  else 
    {
      int i;
      /* draw all the faces: this is not really used  
       * since the face order is computed and sequenced in upper object.
       */
      for ( i= 0 ; i < ((NspSPolyhedron*) Obj)->obj->Mface->n ; i++) 
	draw_spolyhedron_face(Xgc,Obj,i);
    }
}


static void nsp_translate_spolyhedron(NspGraphic *Obj,const double *tr)
{
  nsp_figure_force_redraw(Obj->obj->Fig,NULL);

}

static void nsp_rotate_spolyhedron(NspGraphic *Obj,double *R)
{
  nsp_figure_force_redraw(Obj->obj->Fig,NULL);
}

static void nsp_scale_spolyhedron(NspGraphic *Obj,double *alpha)
{
  nsp_figure_force_redraw(Obj->obj->Fig,NULL);
}

/* compute in bounds the enclosing rectangle of spolyhedron 
 *
 */

extern void nsp_gr_bounds_min_max(int n,double *A,int incr,double *Amin, double *Amax) ;

static int nsp_getbounds_spolyhedron(NspGraphic *Obj,double *bounds)
{
  int i;
  /* this should be stored in a cache and recomputed when necessary 
   *
   */
  nsp_spolyhedron *Q= ((NspSPolyhedron *) Obj)->obj;
  nsp_check_spolyhedron(NULL,(NspSPolyhedron *) Obj);
  if ( Q->Mcoord->mn == 0) return FALSE;
  for ( i = 0 ; i < Q->Mcoord->n ; i++) 
    nsp_gr_bounds_min_max(Q->Mcoord->m,Q->Mcoord->R+i*Q->Mcoord->m,1,&bounds[2*i],&bounds[2*i+1]);
  return TRUE;
}



int nsp_check_spolyhedron(BCG *Xgc, NspSPolyhedron *P)
{
  nsp_spolyhedron *Q = P->obj;
  /* aliases */
  int Q_nb_coords = Q->Mcoord->m;
  /* 
     double *Q_coord = Q->Mcoord->R; 
     int Q_nb_vertices_per_face = Q->Mface->m;
     int Q_nb_faces = Q->Mface->n;
     int *Q_face = Q->Mface->I; 
     double *Q_val = Q->Mval->R;
  */
  int Q_nb_levels;
  double dv;
  int i;

  if (  Q->colmax == -1 &&  Q->colmin == -1) Q->coldef=1;

  /* give default values to 
   * colmin and colmax using the registered colormap
   */
  if ( Xgc != NULL && Q->coldef==1)
    {
      /* adapt colmin and colmax to min max colors */
      Q->colmin = 1;
      Q->colmax = Xgc->Numcolors-1;
      if ( Q->coloutmin == -1) Q->coloutmin=1;
      if ( Q->coloutmax == -1) Q->coloutmax=Q->colmax;
    }
  
  Q_nb_levels = Q->colmax - Q->colmin + 1;

  if ( Q->Mcoord->n != 3 ) 
    {
      Scierror("Error: bad coord for spolyhedron, second dimension should be 3\n");
      return FAIL;
    }
  
  if ( Q->Mface->m < 3 ) 
    {
      Scierror("Error: bad face for spolyhedron, first dimension should be >= 3 %d\n",Q->Mface->m);
      return FAIL;
    }

  Q->Mface = Mat2int(Q->Mface);

  if ( Q->Mval->mn != Q_nb_coords && Q->Mval->mn != Q->Mface->n  ) 
    {
      Scierror("Error: bad dimensions for val, mxn should be equal to %d or %d\n",
	       Q_nb_coords, Q->Mface->n );
      return FAIL;
    }
  
  if ( Q_nb_levels < 1 )
    {
      Scierror("Error: bad colmin and colmax fields for polyhedron\n");
      return FAIL;
    }

  /* a faire si changements */

  if ( Q->vlevel == NULL) 
    {
      Q->vlevel = malloc( (1 + Q_nb_levels) * sizeof(double));
      if ( Q->vlevel == NULL)  return FAIL;
      Q->vlevel_length = (1 + Q_nb_levels);
    }
  else if ( Q->vlevel_length != (1 + Q_nb_levels))
    {
      Q->vlevel = realloc(Q->vlevel, (1 + Q_nb_levels) * sizeof(double));
      if ( Q->vlevel == NULL)  return FAIL;
      Q->vlevel_length = (1 + Q_nb_levels);
    }

  if ( Q->fill == NULL) 
    {
      Q->fill = malloc( (2 + Q_nb_levels) * sizeof(int));
      if ( Q->fill == NULL)  return FAIL;
      Q->fill_length = (2 + Q_nb_levels);
    }
  else if ( Q->fill_length != (2 + Q_nb_levels))
    {
      Q->fill = realloc(Q->fill, (2 + Q_nb_levels) * sizeof(int));
      if ( Q->fill == NULL)  return FAIL;
      Q->fill_length = (2 + Q_nb_levels);
    }

  if ( Xgc != NULL && Q->vmax == Q->vmin && Q->vmax == 0.0)
    {
      /* we assume here that the Q_val values 
       * are graduated as colors 
       *
       */
      Q->vmin = 1;
      Q->vmax =  Xgc->Numcolors-1;
    }
  
  dv = (Q->vmax - Q->vmin)/Q_nb_levels;
  Q->vlevel[0] = Q->vmin;
  for ( i = 1 ; i < Q_nb_levels ; i++ ) Q->vlevel[i] = Q->vmin + i*dv;
  Q->vlevel[Q_nb_levels] = Q->vmax;
  
  Q->fill[0] = Q->coloutmin;
  Q->fill[1] = Q->colmin;
  for ( i = 2 ; i <= Q_nb_levels ; i++ )  Q->fill[i] = Q->fill[i-1] + 1;
  Q->fill[Q_nb_levels+1] = Q->coloutmax;

  if ( Q->pos == NULL) 
    {
      Q->pos = malloc( Q_nb_coords * sizeof(VisionPos));
      if ( Q->pos == NULL) return FAIL;
      Q->pos_length = Q_nb_coords;
    }

  /* create extra data for Mcoord_l declared void* */
  if ( Q->Mcoord_l == NULL) 
    {
      Q->Mcoord_l = nsp_matrix_create("local",'r',Q->Mcoord->m, Q->Mcoord->n);
    }
  return OK;
}



static void draw_spolyhedron_face(BCG *Xgc,NspGraphic *Ob, int j)
{
  nsp_spolyhedron *Q = ((NspSPolyhedron *) Ob)->obj;
  int i, k, np=1, m, zero=0;
  int x_def[12], y_def[12];
  int *x=x_def, *y=y_def;
  int nbtri; 
  int zxy[3], sx[3], sy[3];
  int numpt, *current_vertex, color, orient;
  double v[3], val_mean=0.0;
  int one_face_color = FALSE;
  int Q_nb_coords = Q->Mcoord->m;
  int foreground_color = 1; /* XX should be shared */
  double * Q_coord = ((NspMatrix *) Q->Mcoord_l)->R;
  int Q_nb_vertices_per_face = Q->Mface->m;
  int *Q_face = Q->Mface->I; 
  double *Q_val = Q->Mval->R;
  int Q_nb_levels = Q->colmax - Q->colmin + 1;

  int display_mode = (Q->shade == TRUE) ? INTERP : FLAT;

  if ( Q->Mval->mn != Q->Mcoord->m && Q->Mval->mn == Q->Mface->n) 
    {
      /* when we just have one color per face INTERP is useless */
      display_mode = FLAT;
      one_face_color = TRUE;
    }

  m = Q_nb_vertices_per_face;
  current_vertex = &(Q_face[m*j]);

  if ( m > 12 ) 
    {
      x = graphic_alloc(0,m,sizeof(int));
      y = graphic_alloc(1,m,sizeof(int));
    }

  for (i = 0 ; i < m ; i++)
    {
      numpt = current_vertex[i]-1;
      x[i] = XScale(Xgc->scales,Q_coord[numpt]);
      y[i] = YScale(Xgc->scales,Q_coord[numpt+Q_nb_coords]);
      val_mean += (one_face_color) ? Q_val[j] : Q_val[numpt];
    }

  val_mean /=  m;
  
  if ( ISNAN(val_mean) || isinf(val_mean)) return;

  orient = nsp_obj3d_orientation(x, y, m);
  
  Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);
  
  if ( m > 12 || ( display_mode == FLAT  || ( orient == 1 && Q->back_color >= 0 ) ))
    {
      if ( orient == 1  && Q->back_color >= 0 )
	{
	  color = - Q->back_color;
	  if ( Q->mesh ) color= Abs(color);
	  Xgc->graphic_engine->fillpolylines(Xgc, x, y, &color, np, m);
	}
      else
	{
	  color = -Q->fill[zone(val_mean, Q->vmin, Q->vmax, Q_nb_levels)];
	  if ( Q->mesh ) color= Abs(color);
	  Xgc->graphic_engine->fillpolylines(Xgc, x, y, &color, np, m);
	}
    }
  else
    {
      nbtri = m - 2;
      for ( k = 0 ; k < nbtri ; k++ )
	{
	  int l;
	  int triangle[]= { 0, k+1,k+2};
	  for ( l = 0 ; l < 3 ; l++ )
	    {
	      i = triangle[l];
	      if ( one_face_color ) 
		v[l] = Q_val[j]; /* one color for each facet */
	      else	
		v[l] = Q_val[current_vertex[i]-1]; /* one color for each vertex */
	      zxy[l] = zone(v[l], Q->vmin, Q->vmax, Q_nb_levels);
	      sx[l] = x[i]; sy[l] = y[i];
	    }
	  /* (sx,sy,v) : triangle et valeur en chaque sommet
	   * zxy : vecteur indiquant que la couleur du sommet i vaut - Q->fill[zxy[i]]
	   * (Q->vlevel,Q->fill): donne une couleur en fonction de z. 
	   * Q->vlevel[i]: donne le z quantifié qui porte la couleur i 
	   */
	  interp_color_triangle (Xgc,sx, sy, v, zxy, Q->vlevel, Q->fill);  
	}
      if ( Q->mesh  ) 
	Xgc->graphic_engine->fillpolylines(Xgc, x, y, &zero, np, m);
    }
}

static void draw_spolyhedron_ogl(BCG *Xgc,void *Ob)
{
#ifdef  WITH_GTKGLEXT 
  int one_face_color = FALSE;
  int foreground_color = 1; /* XX should be shared */
  nsp_spolyhedron *Q = ((NspSPolyhedron *) Ob)->obj;
  int i,j, np=1, m,colors[128];
  int numpt, *current_vertex, color;
  double val_mean=0.0;
  double x[128], y[128], z[128], v[128];

  int Q_nb_coords = Q->Mcoord->m;  
  double *Q_coord = Q->Mcoord->R; 
  int Q_nb_vertices_per_face = Q->Mface->m;
  int Q_nb_faces = Q->Mface->n;
  int *Q_face = Q->Mface->I; 
  double *Q_val = Q->Mval->R;
  int Q_nb_levels = Q->colmax - Q->colmin + 1;
  int display_mode = (Q->shade == TRUE) ? INTERP : FLAT;

  if ( Q->Mval->mn != Q->Mcoord->m && Q->Mval->mn == Q->Mface->n) 
    {
      /* when we just have on color per face INTERP is useless */
      display_mode = FLAT;
      one_face_color = TRUE;
    }
  
  for ( j = 0 ; j < Q_nb_faces ; j++) 
    {
      val_mean = 0;
      m = Min(Q_nb_vertices_per_face,128) ; /* 128 à gerer XXX */
      current_vertex = &(Q_face[m*j]);
      for (i = 0 ; i < m ; i++)
	{
	  numpt = current_vertex[i]-1;
	  x[i] = Q_coord[numpt];
	  y[i] = Q_coord[numpt+Q_nb_coords];
	  z[i] = Q_coord[numpt+2*Q_nb_coords];
	  v[i] = (one_face_color) ? Q_val[j] : Q_val[numpt];
	  val_mean += v[i];
	}
      val_mean /=  m;
      Xgc->graphic_engine->xset_pattern(Xgc,foreground_color);

      if ( display_mode == FLAT  )
	{
	  color = Q->fill[zone(val_mean, Q->vmin, Q->vmax, Q_nb_levels)];
	  fillpolylines3D(Xgc, x, y,z, &color, np, m);
	}
      else
	{
	  for (i = 0 ; i < m ; i++) 
	    colors[i] = Q->fill[zone(v[i], Q->vmin, Q->vmax, Q_nb_levels)];
	  /* colors are given by cvect of size (*p) times (*q) */
	  fillpolylines3D_shade(Xgc,x,y,z,colors, np,m);
	}
    }
#endif
}


static void zmean_faces_for_SPolyhedron(void *Obj, double z[], HFstruct HF[], int *n, int k)
{
  nsp_spolyhedron *Q = ((NspSPolyhedron *) Obj)->obj;
  int m, i, j, *current_vertex;
  VisionPos pos_face, pos_vertex;
  double coef, zmean;

  int Q_nb_coords = Q->Mcoord->m;
  double * Q_coord = ((NspMatrix *) Q->Mcoord_l)->R;
  int Q_nb_vertices_per_face = Q->Mface->m;
  int Q_nb_faces = Q->Mface->n;
  int *Q_face = Q->Mface->I; 

  m = Q_nb_vertices_per_face; 
  coef = 1.0/m;
  current_vertex = Q_face;
  for ( j = 0 ; j < Q_nb_faces ; j++ )
    {
      zmean = 0.0; pos_face = OUT_XY;
      /* Une face rentre dans le calcul des faces cachées si :
       *     1/ aucun point n'est en position OUT_Z
       *     2/ au moins un point est IN (les autres étant alors soit
       *        IN soit OUT_XY)
       * On pourra par la suite détailler un peu plus car si tous les
       * sommets de la face sont IN aucun clippling n'est à effectuer.
       * Faire ce clipping moi-même ?
       */
      for ( i = 0 ; i < m ; i++ )
	{
	  zmean += Q_coord[(*current_vertex-1)+2*Q_nb_coords];
	  pos_vertex = Q->pos[*current_vertex-1];
	  if (pos_vertex == OUT_Z)
	    pos_face = OUT_Z;
	  else if (pos_vertex == VIN && pos_face != OUT_Z)
	    pos_face = VIN;
	  current_vertex++;
	}
      if (pos_face == VIN) 
	{
	  z[*n] = coef*zmean;
	  HF[*n].num_obj = k;
	  HF[*n].num_in_obj = j;
	  (*n)++; 
	}
    }
}

/*
 * requested method for 3d objects.
 */

static void nsp_spolyhedron_zmean(BCG *Xgc,NspGraphic *Obj, double *z, void *HF, int *n, int k, double *lim)
{
  nsp_spolyhedron *Q= ((NspSPolyhedron *) Obj)->obj;
  apply_transforms_new(Xgc,((NspMatrix *) Q->Mcoord_l)->R,Q->Mcoord->R,Q->pos, lim, Q->Mcoord->m);
  zmean_faces_for_SPolyhedron(Obj, z,  HF, n, k);
}

/* requested method for 3d objects.
 *
 */

static int nsp_spolyhedron_n_faces(BCG *Xgc,NspGraphic *Obj)
{
  return ((NspSPolyhedron *) Obj)->obj->Mface->n;
}


/* utility 
 *
 */

static int zone(double val, double valmin, double valmax, int nv)
{
  int z;
  if ( val > valmax )
    return (nv+1);
  else if ( val < valmin )
    return (0);
  else
    {
      z = 1 + (int) floor( nv*((val - valmin)/(valmax-valmin)) );
      if ( z > nv ) z = nv;
      return (z);
    }
}

static void interp_color_triangle(BCG *Xgc,int *x, int *y, double *v, int *z, double *zlevel, int *fill)
{
  int sx[3], sy[3], zxy[3], perm[3];
  double fxy[3];

  int i, nb0, edge, izone, color;
  int nr, resx[5],resy[5];
  int xEdge2, yEdge2, xEdge, yEdge; 

  permut_of_sort(z, perm);
  for ( i = 0 ; i < 3 ; i++)
    {
      sx[i] = x[perm[i]];
      sy[i] = y[perm[i]];
      zxy[i] = z[perm[i]];
      fxy[i] = v[perm[i]];
    }

  if ( zxy[0] == zxy[2] )   /*  case of only one color for the triangle : */
    {
      resx[0] = sx[0]; resx[1] = sx[1]; resx[2] = sx[2];
      resy[0] = sy[0]; resy[1] = sy[1]; resy[2] = sy[2];
      color = - Abs(fill[zxy[0]]); nr = 3;
      if ( color != 0 )
	Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr); 
      return; 
    }
  
  /* 
   *  at least 2 colors for painting the triangle : it is divided in elementary
   *  polygons. The number of polygons is npolys = zxy[2]-zxy[0]+1.
   * 
   *                          P2           as zxy[0] <= zxy[1] <  zxy[2] or 
   *  Notations/Hints :       /\              zxy[0] <  zxy[1] <= zxy[2]
   *                  edge2  /  \ edge1    from a previus sort. All the polygons
   *                        /    \         have 2 points on edge2, the others points
   *                       /______\        are on edge0 and/or edge1. I name the 2 ends
   *                     P0        P1      points on each poly PEdge2 and Pedge, they are 
   *                         edge0         the 2 first points of the next poly. I start
   *  from P0 to form the first poly (a triangle or
   *  a 4 sides depending if zxy[0]=zxy[1]), then the 2, 3, .., npolys - 1 (if they exist)
   *  and finally the last one which comprise the P2 vertex.  In some special cases
   *  we can have a degenerate poly but it doesn't matter ! 				  
   */
  
  nb0 = zxy[1]-zxy[0]; /* number of intersection points on edge 0 */

  /*
   *   compute the first poly 
   */
  
  resx[0] = sx[0]; resy[0] = sy[0]; nr = 1; edge = 0;
  if ( nb0 == 0 )  /* the intersection point is on Edge1 but */
    {              /* the next point of the poly is P1 */  
      resx[1] = sx[1]; resy[1] = sy[1]; nr++;
      edge = 1;    /* the next intersection points will be on edge1 */
    } 
  else 
    nb0--;
  /* the intersection point on edge (0 or 1) : */
  find_intersection(sx, sy, fxy, zlevel[zxy[0]], edge, edge+1, &xEdge, &yEdge);
  resx[nr] = xEdge; resy[nr] = yEdge; nr++;
  /* the last point of the first poly (edge 2) : */
  find_intersection(sx, sy, fxy, zlevel[zxy[0]], 0, 2, &xEdge2, &yEdge2);
  resx[nr] = xEdge2; resy[nr] = yEdge2; nr++;
  color = - Abs(fill[zxy[0]]);
  if ( color != 0 )
    Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
  /*
   * compute the intermediary polygon(s) 
   */

  for ( izone = zxy[0]+1 ; izone < zxy[2] ; izone++ ) 
    {
      resx[0] = xEdge2; resy[0] = yEdge2;          /* the 2 first points are known */
      resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
      if ( edge == 0 )   /* the intersection point is perhaps on edge 0 */
	{
	  if (nb0 == 0 )  /* no it is on edge 1 but the next point of the poly is P1 */
	    {
	      resx[2] = sx[1]; resy[2] = sy[1]; nr++;
	      edge = 1;          /* the next intersection points will be on edge1 */
	    } 
	  else 
	    nb0--;
	};
      /* the intersection point on edge (0 or 1) : */
      find_intersection(sx, sy, fxy, zlevel[izone], edge, edge+1, &xEdge, &yEdge);
      resx[nr] = xEdge; resy[nr] = yEdge; nr++;
      /* the last point of the first poly (edge 2) : */
      find_intersection(sx, sy, fxy, zlevel[izone], 0, 2, &xEdge2, &yEdge2);
      resx[nr] = xEdge2; resy[nr] = yEdge2; nr++;
      color = - Abs(fill[izone]);
      if ( color != 0 )
	Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr);
      
      
    };

  /*
   * compute the last poly  
   */
  
  resx[0] = xEdge2; resy[0] = yEdge2;         /* the 2 first points are known */
  resx[1] = xEdge;  resy[1] = yEdge; nr = 2;
  if ( edge == 0 )  /* the next point of the poly is P1 */
    {                         
      resx[2] = sx[1]; resy[2] = sy[1]; nr++;
    };
  /* the last point is P2 */
  resx[nr] = sx[2]; resy[nr] = sy[2]; nr++;
  color = - Abs(fill[zxy[2]]);
  
  if ( color != 0 )
     Xgc->graphic_engine->fillpolylines(Xgc,resx,resy,&color,1,nr); 
  
  
}


static void permut_of_sort(int *tab, int *perm)
{
  /* 
   *   get the permutation perm(0:2) which sort the array tab(0:2) in increasing order 
   */
  perm[0]=0; perm[1] = 1; perm[2] = 2;
  if ( tab[1] < tab[0] ) 
    {
      perm[1]=0 ; perm[0] = 1;
    }
  if ( tab[2] < tab[perm[1]] ) 
    {   /* sort not finish */
      if ( tab[2] < tab[perm[0]] ) 
	{
	  perm[2] = perm[1]; perm[1] = perm[0]; perm[0] = 2; 
	}
      else 
	{
	  perm[2] = perm[1] ; perm[1] = 2;
	}
    }
}

static void find_intersection(int *sx, int *sy, double *fxy, double z, 
			      int inda, int indb, int *xint, int *yint)
{ 
  double alpha;
  alpha = (z - fxy[inda])/(fxy[indb] - fxy[inda]);
  *xint = (int) ((1.0 - alpha)*sx[inda] + alpha*sx[indb]);
  *yint = (int) ((1.0 - alpha)*sy[inda] + alpha*sy[indb]);
} 


/* Utilities 
 * 
 */

/**
 * nsp_spolyhedron_create_from_triplet:
 * @name: name to give to new object 
 * @x: array of size @m
 * @y: array of size @n
 * @z: array of size @mx@n
 * @m: size of @x array  
 * @n: size of @y array
 * 
 * creates a #NspPolyhedron from a triplet describing a 
 * surface. 
 * 
 * Returns: a new #NspPolyhedron or %NULL 
 **/

NspSPolyhedron *nsp_spolyhedron_create_from_triplet(char *name,double *x,double *y,double *z,int m,int n, double *col,int ncol)
{
  double vmin=0.0,vmax=0.0;
  NspSPolyhedron *pol;
  NspMatrix *C=NULL,*F=NULL,*Val = NULL;
  if ((C=nsp_surf_to_coords("c",x,y,z,m,n))==NULL) goto bug;
  if ((F=nsp_surf_to_faces("f",x,m,y,n) )==NULL) goto bug;
  if ((Val = nsp_matrix_create("v",'r',C->m,1)) == NULLMAT) goto bug; 

  if ( col == NULL) 
    {
      /* colors are selected according to z values */
      memcpy(Val->R,C->R+2*C->m,C->m*sizeof(double));
    }
  else if ( ncol == C->m ) 
    {
      /* colors are selected accordind to col array */
      memcpy(col,C->R+2*C->m,C->m*sizeof(double));
    }
  else if ( ncol == m ) 
    {
      /* one color by face from col array XXXXX */
      memcpy(Val->R,C->R+2*C->m,C->m*sizeof(double));
    }
  
  
  /* use VMiniMaxi but change it to extern */
  if ( Val->mn != 0) 
    {
      int i=0,j;
      while ( ISNAN(Val->R[i])) i++;
      vmin = vmax = Val->R[i];
      for ( j = i+1 ; j < Val->mn ; j++) 
	{
	  if ( ISNAN(Val->R[j])) continue;
	  if ( Val->R[j] < vmin) vmin = Val->R[j];
	  if ( Val->R[j] > vmax) vmax = Val->R[j];
	}
    }
  if ((pol = nsp_spolyhedron_create(name,C,F,Val,vmin,vmax,-1,-1,-1,-1,
				    TRUE,4,TRUE,NULL,NULL,0,NULL,0,NULL,0,0,NULL))==NULL)
    goto bug;
  if ( nsp_check_spolyhedron(NULL,pol)== FAIL) goto bug;
  return pol;
 bug:
  if ( C != NULL) nsp_matrix_destroy(C);
  if ( F != NULL) nsp_matrix_destroy(F);
if ( Val != NULL) nsp_matrix_destroy(Val);
  return NULL;
}

NspSPolyhedron *nsp_spolyhedron_create_from_facets(char *name,double *xx,double *yy,double *zz,int m,int n,int *colors, int ncol ,int cmap_ncol )
{
  int bc;
  double vmin=1,vmax=cmap_ncol -1 ;
  NspSPolyhedron *pol;
  NspMatrix *C=NULL,*F=NULL, *Val=NULL;
  
  if ( nsp_facets_to_faces(xx,yy,zz,colors,ncol,m,n,&C,&F,&Val)== FAIL) goto bug;

  if ( colors == NULL ) 
    {
      /* when colors is not given zz is used and mapped to colors we have 
       * to compute vmin and vmax in that case to 
       * properly remap zz to the range of the colormap 
       */
      int i=0,j;
      while ( ISNAN(Val->R[i])) i++;
      vmin = vmax = Val->R[i];
      for ( j = i+1 ; j < Val->mn ; j++) 
	{
	  if ( ISNAN(Val->R[j])) continue;
	  if ( Val->R[j] < vmin) vmin = Val->R[j];
	  if ( Val->R[j] > vmax) vmax = Val->R[j];
	}
    }
  
  bc = 4; /* XXX the color for hidden faces
	   */
  if ((pol = nsp_spolyhedron_create(name,C,F,Val,vmin,vmax,-1,-1,-1,-1,
				    TRUE,bc,TRUE,NULL,NULL,0,NULL,0,NULL,0,0,NULL))==NULL)
    goto bug;

  if ( nsp_check_spolyhedron(NULL,pol)== FAIL) goto bug;
  return pol;
  
 bug:
  if ( C != NULL) nsp_matrix_destroy(C);
  if ( F != NULL) nsp_matrix_destroy(F);
  if ( Val != NULL) nsp_matrix_destroy(Val);
  return NULL;
}


#line 1783 "spolyhedron.c"
