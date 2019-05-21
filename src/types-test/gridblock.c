/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
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
 */





#line 35 "codegen/gridblock.override"
#include <nsp/objects.h>
#include <nsp/graphics-new/Graphics.h> 
#include "nsp/link.h"
#include "nsp/block.h"
#include "nsp/axes.h"
#include "nsp/grimage.h"

#line 36 "gridblock.c"

/* -----------NspGridBlock ----------- */


#define  NspGridBlock_Private 
#include <nsp/objects.h>
#include <nsp/gridblock.h>
#include <nsp/interf.h>

/* 
 * NspGridBlock inherits from Block 
 * and implements GRint
 */

int nsp_type_gridblock_id=0;
NspTypeGridBlock *nsp_type_gridblock=NULL;

/*
 * Type object for NspGridBlock 
 * all the instance of NspTypeGridBlock share the same id. 
 * nsp_type_gridblock: is an instance of NspTypeGridBlock 
 *    used for objects of NspGridBlock type (i.e built with new_gridblock) 
 * other instances are used for derived classes 
 */
NspTypeGridBlock *new_type_gridblock(type_mode mode)
{
  NspTypeGRint *t_grint;
  NspTypeGridBlock *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gridblock != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gridblock;
    }
  if (( type =  malloc(sizeof(NspTypeGridBlock))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_block(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gridblock_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gridblock_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_gridblock;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gridblock */ 

  top->pr = (print_func *) nsp_gridblock_print;
  top->dealloc = (dealloc_func *) nsp_gridblock_destroy;
  top->copy  =  (copy_func *) nsp_gridblock_copy;
  top->size  = (size_func *) nsp_gridblock_size;
  top->s_type =  (s_type_func *) nsp_gridblock_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gridblock_type_short_string;
  top->info = (info_func *) nsp_gridblock_info;
  /* top->is_true = (is_true_func  *) nsp_gridblock_is_true; */
  /* top->loop =(loop_func *) nsp_gridblock_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_gridblock_object;
  top->eq  = (eq_func *) nsp_gridblock_eq;
  top->neq  = (eq_func *) nsp_gridblock_neq;
  top->save  = (save_func *) nsp_gridblock_xdr_save;
  top->load  = (load_func *) nsp_gridblock_xdr_load;
  top->create = (create_func*) int_gridblock_create;
  top->latex = (print_func *) nsp_gridblock_latex;
  top->full_copy = (copy_func *) nsp_gridblock_full_copy;

  /* specific methods for gridblock */

  type->init = (init_func *) init_gridblock;

#line 49 "codegen/gridblock.override"
  /* inserted verbatim in the type definition */
  type->gtk_methods = TRUE;

#line 115 "gridblock.c"
  /* 
   * NspGridBlock interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_grint = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase * ) t_grint;
#line 54 "codegen/gridblock.override"


  t_grint->get_hilited 	= ((NspTypeGRint *) type->surtype->interface)->get_hilited;
  t_grint->set_hilited 	= ((NspTypeGRint *) type->surtype->interface)->set_hilited;
  t_grint->get_show    	= ((NspTypeGRint *) type->surtype->interface)->get_show;
  t_grint->set_show		= ((NspTypeGRint *) type->surtype->interface)->set_show;
  t_grint->set_pos  	= ((NspTypeGRint *) type->surtype->interface)->set_pos;
  t_grint->get_pos  	= ((NspTypeGRint *) type->surtype->interface)->get_pos;
  t_grint->resize 		= ((NspTypeGRint *) type->surtype->interface)->resize;
  t_grint->update_locks 	= ((NspTypeGRint *) type->surtype->interface)->update_locks;
  t_grint->contains_pt 	= ((NspTypeGRint *) type->surtype->interface)->contains_pt;
  t_grint->control_near_pt 	= ((NspTypeGRint *) type->surtype->interface)->control_near_pt;
  t_grint->lock_near_pt 	= ((NspTypeGRint *) type->surtype->interface)->lock_near_pt;
  t_grint->move_control_init= ((NspTypeGRint *) type->surtype->interface)->move_control_init;
  t_grint->move_control 	= ((NspTypeGRint *) type->surtype->interface)->move_control;

  t_grint->get_number_of_locks = ((NspTypeGRint *) type->surtype->interface)->get_number_of_locks;
  t_grint->get_number_of_ports = ((NspTypeGRint *) type->surtype->interface)->get_number_of_ports;
  t_grint->get_lock_connection = ((NspTypeGRint *) type->surtype->interface)->get_lock_connection;
  t_grint->get_lock_pos = ((NspTypeGRint *) type->surtype->interface)->get_lock_pos;
  t_grint->get_lock_dir = ((NspTypeGRint *) type->surtype->interface)->get_lock_dir;
  t_grint->set_lock_connection = ((NspTypeGRint *) type->surtype->interface)->set_lock_connection;
  t_grint->unset_lock_connection = ((NspTypeGRint *) type->surtype->interface)->unset_lock_connection;
  t_grint->is_lock_connectable = ((NspTypeGRint *) type->surtype->interface)->is_lock_connectable;
  t_grint->is_lock_connected = ((NspTypeGRint *) type->surtype->interface)->is_lock_connected;
  t_grint->set_lock_pos = ((NspTypeGRint *) type->surtype->interface)->set_lock_pos;
  t_grint->unlock = ((NspTypeGRint *) type->surtype->interface)->unlock;

#line 153 "gridblock.c"
  if ( nsp_type_gridblock_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGridBlock called nsp_type_gridblock
       */
      type->id =  nsp_type_gridblock_id = nsp_new_type_id();
      nsp_type_gridblock = type;
      if ( nsp_register_type(nsp_type_gridblock) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gridblock(mode);
    }
  else 
    {
      type->id = nsp_type_gridblock_id;
      return type;
    }
}

/*
 * initialize NspGridBlock instances 
 * locally and by calling initializer on parent class 
 */

static int init_gridblock(NspGridBlock *Obj,NspTypeGridBlock *type)
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
 * new instance of NspGridBlock 
 */

NspGridBlock *new_gridblock() 
{
  NspGridBlock *loc;
  /* type must exists */
  nsp_type_gridblock = new_type_gridblock(T_BASE);
  if ( (loc = malloc(sizeof(NspGridBlock)))== NULLGRIDBLOCK) return loc;
  /* initialize object */
  if ( init_gridblock(loc,nsp_type_gridblock) == FAIL) return NULLGRIDBLOCK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGridBlock 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gridblock_size(NspGridBlock *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char gridblock_type_name[]="GridBlock";
static char gridblock_short_type_name[]="gridblock";

static char *nsp_gridblock_type_as_string(void)
{
  return(gridblock_type_name);
}

static char *nsp_gridblock_type_short_string(NspObject *v)
{
  return(gridblock_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gridblock_eq(NspGridBlock *A, NspObject *B)
{
  NspGridBlock *loc = (NspGridBlock *) B;
  if ( check_cast(B,nsp_type_gridblock_id) == FALSE) return FALSE ;
  if ( A->obj == loc->obj ) return TRUE;
  if ( A->obj->diagram != loc->obj->diagram) return FALSE;
   return TRUE;
}

/*
 * A != B 
 */

static int nsp_gridblock_neq(NspGridBlock *A, NspObject *B)
{
  return ( nsp_gridblock_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_gridblock_xdr_save(XDR *xdrs, NspGridBlock *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gridblock)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if ( nsp_block_xdr_save(xdrs, (NspBlock * ) M)== FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspGridBlock  *nsp_gridblock_xdr_load_partial(XDR *xdrs, NspGridBlock *M)
{
  int fid;
  char name[NAME_MAXL];
  M->obj->ref_count=1;
  if (nsp_xdr_load_i(xdrs, &fid) == FAIL) return NULL;
  if ( fid == nsp_dynamic_id)
    {
     if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
    }
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULL;
  if ( nsp_block_xdr_load_partial(xdrs,(NspBlock * )M) == NULL) return NULL;
 return M;
}

static NspGridBlock  *nsp_gridblock_xdr_load(XDR *xdrs)
{
  NspGridBlock *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRIDBLOCK;
  if ((H  = nsp_gridblock_create_void(name,(NspTypeBase *) nsp_type_gridblock))== NULLGRIDBLOCK) return H;
  if ( nsp_gridblock_create_partial(H) == FAIL) return NULLGRIDBLOCK;
  if ((H  = nsp_gridblock_xdr_load_partial(xdrs,H))== NULLGRIDBLOCK) return H;
  if ( nsp_gridblock_check_values(H) == FAIL) return NULLGRIDBLOCK;
  return H;
}

/*
 * delete 
 */

void nsp_gridblock_destroy_partial(NspGridBlock *H)
{
  nsp_block_destroy_partial((NspBlock * ) H);
   H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
    FREE(H->obj);
   }
}

void nsp_gridblock_destroy(NspGridBlock *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_gridblock_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_gridblock_info(NspGridBlock *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGRIDBLOCK) 
    {
      Sciprintf("Null Pointer NspGridBlock \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_gridblock_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_gridblock_print(NspGridBlock *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGRIDBLOCK) 
    {
      Sciprintf("Null Pointer NspGridBlock \n");
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
          nsp_gridblock_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s (nref=%d)\n",pname, nsp_gridblock_type_short_string(NSP_OBJECT(M)), M->obj->ref_count);
      Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+2,"diagram=0x%x\n", M->obj->diagram);
  nsp_block_print((NspBlock * ) M, indent+2,NULL,rec_level);
    Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gridblock_latex(NspGridBlock *M, int use_math,const char *name, int rec_level)
{
  int indent=2;
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( use_math ) Sciprintf("\\begin{equation*}\n");

  if ( name != NULL || strcmp(NSP_OBJECT(M)->name,NVOID) != 0)
    Sciprintf("\\verb|%s| = \\left\\{\n", pname);

  else 
    Sciprintf("\\left\{\n");

  // Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_gridblock_type_short_string(NSP_OBJECT(M)));
  Sciprintf("\\begin{array}{l}");

  Sciprintf1(indent+2,"\\verb|diagram|= \\verb@0x%x@\n",M->obj->diagram);
  Sciprintf1(2,"\\\\\n");
  nsp_block_latex((NspBlock * ) M, FALSE,NULL,rec_level);
  Sciprintf1(indent+1,"\n");
  Sciprintf("\\end{array}\n");

  Sciprintf("\\right.\n");

  if ( use_math ) Sciprintf("\\end{equation*}\n");

  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGridBlock objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGridBlock   *nsp_gridblock_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gridblock_id)  == TRUE  ) return ((NspGridBlock *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gridblock));
  return NULL;
}

int IsGridBlockObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gridblock_id);
}

int IsGridBlock(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gridblock_id);
}

NspGridBlock  *GetGridBlockCopy(Stack stack, int i)
{
  if (  GetGridBlock(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGridBlock  *GetGridBlock(Stack stack, int i)
{
  NspGridBlock *M;
  if (( M = nsp_gridblock_object(NthObj(i))) == NULLGRIDBLOCK)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspGridBlock instance 
 *-----------------------------------------------------*/

static NspGridBlock *nsp_gridblock_create_void(const char *name,NspTypeBase *type)
{
 NspGridBlock *H  = (type == NULL) ? new_gridblock() : type->new();
 if ( H ==  NULLGRIDBLOCK)
  {
   Sciprintf("No more memory\n");
   return NULLGRIDBLOCK;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGRIDBLOCK;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_gridblock_create_partial(NspGridBlock *H)
{
  if ( nsp_block_create_partial((NspBlock * ) H)== FAIL) return FAIL;
  if((H->obj = calloc(1,sizeof(nsp_gridblock)))== NULL ) return FAIL;
  H->obj->ref_count=1;
  H->obj->diagram = NULL;
  return OK;
}

int nsp_gridblock_check_values(NspGridBlock *H)
{
  nsp_block_check_values((NspBlock * ) H);
  return OK;
}

NspGridBlock *nsp_gridblock_create(const char *name,void* diagram,NspTypeBase *type)
{
  NspGridBlock *H  = nsp_gridblock_create_void(name,type);
  if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  if ( nsp_gridblock_create_partial(H) == FAIL) return NULLGRIDBLOCK;
  H->obj->diagram = diagram;
  if ( nsp_gridblock_check_values(H) == FAIL) return NULLGRIDBLOCK;
  return H;
}


NspGridBlock *nsp_gridblock_create_default(const char *name)
{
 NspGridBlock *H  = nsp_gridblock_create_void(name,NULL);
 if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  if ( nsp_gridblock_create_partial(H) == FAIL) return NULLGRIDBLOCK;
  if ( nsp_gridblock_check_values(H) == FAIL) return NULLGRIDBLOCK;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspGridBlock *nsp_gridblock_copy_partial(NspGridBlock *H,NspGridBlock *self)
{
  if ( nsp_block_copy_partial((NspBlock *) H,(NspBlock * ) self ) == NULL) return NULLGRIDBLOCK;
  H->obj = self->obj; self->obj->ref_count++;
  return H;
}

NspGridBlock *nsp_gridblock_copy(NspGridBlock *self)
{
  NspGridBlock *H  =nsp_gridblock_create_void(NVOID,(NspTypeBase *) nsp_type_gridblock);
  if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  if ( nsp_gridblock_copy_partial(H,self)== NULL) return NULLGRIDBLOCK;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspGridBlock *nsp_gridblock_full_copy_partial(NspGridBlock *H,NspGridBlock *self)
{
  if ( nsp_block_full_copy_partial((NspBlock *) H,(NspBlock * ) self ) == NULL) return NULLGRIDBLOCK;
  if ((H->obj = calloc(1,sizeof(nsp_gridblock))) == NULL) return NULLGRIDBLOCK;
  H->obj->ref_count=1;
  H->obj->diagram = self->obj->diagram;
  return H;
}

NspGridBlock *nsp_gridblock_full_copy(NspGridBlock *self)
{
  NspGridBlock *H  =nsp_gridblock_create_void(NVOID,(NspTypeBase *) nsp_type_gridblock);
  if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  if ( nsp_gridblock_full_copy_partial(H,self)== NULL) return NULLGRIDBLOCK;
  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspGridBlock
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_gridblock_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGridBlock *H;
  CheckStdRhs(0,0);
  /* want to be sure that type gridblock is initialized */
  nsp_type_gridblock = new_type_gridblock(T_BASE);
  if(( H = nsp_gridblock_create_void(NVOID,(NspTypeBase *) nsp_type_gridblock)) == NULLGRIDBLOCK) return RET_BUG;
  /* then we use optional arguments to fill attributes */
    if ( nsp_gridblock_create_partial(H) == FAIL) return RET_BUG;
  if ( int_create_with_attributes((NspObject  * ) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_gridblock_check_values(H) == FAIL) return RET_BUG;
    MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 88 "codegen/gridblock.override"

int _wrap_gridblock_edit(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspDiagram *D; 
  if ((D = nsp_diagram_from_gridblock(NVOID,self)) == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(D));
  return 1;
}


#line 573 "gridblock.c"


static NspMethods gridblock_methods[] = {
  {"edit",(nsp_method *) _wrap_gridblock_edit},
  { NULL, NULL}
};

static NspMethods *gridblock_get_methods(void) { return gridblock_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gridblock_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab GridBlock_func[]={
  { "gridblock_create", int_gridblock_create},
  { NULL, NULL}
};

/* call ith function in the GridBlock interface */

int GridBlock_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return ( *(GridBlock_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GridBlock_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = GridBlock_func[i].name;
  *f = GridBlock_func[i].fonc;
}
void nsp_initialize_GridBlock_types(void)
{
  new_type_gridblock(T_BASE);
}

#line 100 "codegen/gridblock.override"

/* inserted verbatim at the end */

NspGridBlock *nsp_gridblock_create_override(char *name,double *rect,int color,int thickness,int background)
{
  NspDiagram *D;
  NspBlock *B;
  NspGridBlock *H;
  /* want to be sure that type gridblock is initialized */
  nsp_type_gridblock = new_type_gridblock(T_BASE);
  if(( H = nsp_gridblock_create_void(name,(NspTypeBase *) nsp_type_gridblock)) == NULLGRIDBLOCK)
    return NULL;
  if ( nsp_gridblock_create_partial(H) == FAIL) return NULL;
  B = (NspBlock *) H;
  /* create the part from father */
  memcpy(B->obj->r,rect,4*sizeof(double));
  B->obj->color = color;
  B->obj->thickness = thickness;
  B->obj->background = background;
  B->obj->n_locks = 0;
  /* create the own part */
  if ((D = nsp_diagram_create_default("dg")) == NULL) return NULLGRIDBLOCK;
  H->obj->diagram = D;
  /* to prevent destruction of obj */
  D->obj->ref_count++;
#if 0
  {
    /* insert a first object in the frame */
    int color=4,thickness=1, background=9;
    double rect[]={0,100,25,25};
    NspBlock *B;
    B = nsp_block_create("fe",NULL,rect,color,thickness,background,0,NULL,NULL,0,NULL);
    if ( B == NULLBLOCK) return NULLGRIDBLOCK;
    nsp_list_end_insert(D->obj->children,(NspObject  *) B);
  }
#endif 
  return H;
}

/* create a NspGridBlock filled with a full copy of objects from @F
 *
 */

NspGridBlock *nsp_gridblock_create_from_nsp_diagram(char *name,double *rect,int color,int thickness,int background, NspDiagram *D) 
{
  NspGraphic *G;
  NspBlock *B;
  NspGridBlock *H;
  double rect1[]={0,100,25,25};
  /* want to be sure that type gridblock is initialized */
  nsp_type_gridblock = new_type_gridblock(T_BASE);
  if(( H = nsp_gridblock_create_void(name,(NspTypeBase *) nsp_type_gridblock)) == NULLGRIDBLOCK)
    return NULL;
  if ( nsp_gridblock_create_partial(H) == FAIL) return NULL;
  B = (NspBlock *) H;
  /* create the part from father */
  B->obj->color = color;
  B->obj->thickness = thickness;
  B->obj->background = background;
  B->obj->n_locks = 0;
  B->obj->icon = NULL;
  B->obj->locks = NULL;
  B->obj->draw_mode = 0;
  memcpy(B->obj->r,rect1,4*sizeof(double));
  /* create the own part, the Diagram given as argument must be a copy */
  H->obj->diagram = D;
  /* to prevent destruction of obj */
  D->obj->ref_count++;
  G = (NspGraphic *)D;
  G->type->unlink_figure(G, ((NspGraphic *) D)->obj->Fig);
  return H;
}



#if 0

/* #define DRAW_INSIDE */

void gridblock_draw(NspGridBlock *B)
{
#ifdef DRAW_INSIDE
  double WRect[4],WRect1[4], FRect[4], ARect[4];
  char logscale[2];
#endif 
  NspBlock *Bl = (NspBlock *) B;
  /* take care of the fact that str1 must be writable */
  BCG *Xgc;
  int cpat, cwidth;
  /* only draw gridblock which are in a frame */
  if ( Bl->obj->frame == NULL) return;
  /* check the show attribute */
  if ( Bl->obj->show == FALSE ) return ;
  Xgc=Bl->obj->frame->Xgc;
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
#ifdef DRAW_INSIDE
  /* Draw the super block inside the block ! 
   */
  /* set the scale for drawing inside the frame */
  getscale2d(Xgc,WRect,FRect,logscale,ARect);
  /* use gf->r to modify the scales */
  WRect1[0]= (B->obj->r[0]-FRect[0])/(FRect[2]-FRect[0]);
  WRect1[1]= 1- (B->obj->r[1]-FRect[1])/(FRect[3]-FRect[1]);
  WRect1[2]= B->obj->r[2]/FRect[2];
  WRect1[3]= B->obj->r[3]/FRect[3];
  /* we directly change the default scale because we do not want 
   * to register all the scales that will be generated by set_scale 
   * thus we use T in flag[1].
   */
  set_scale(Xgc,"fTtfff",WRect1,B->obj->scale,NULL,NULL,NULL);
  nspgframe_draw(B->obj);
  /* scale back */
  set_scale(Xgc,"fTtfff",WRect,FRect,NULL,NULL,NULL);
#endif 
  /* call the father draw */
  GR_INT(Bl->type->interface)->draw(Bl);
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}
#endif 


NspDiagram *nsp_gridblock_get_diagram(void *B1)
{
  ((NspGridBlock *) B1)->obj->ref_count++;
  return ((NspGridBlock *) B1)->obj->diagram;
}

#line 751 "gridblock.c"
