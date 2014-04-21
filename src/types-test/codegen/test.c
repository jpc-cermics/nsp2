/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2014 Jean-Philippe Chancelier Enpc/Cermics
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






/* ----------- NspColormap ----------- */


#define  NspColormap_Private 
#include <nsp/objects.h>
#include <nsp/colormap.h>
#include <nsp/interf.h>

/* 
 * NspColormap inherits from GObject 
 */

int nsp_type_colormap_id=0;
NspTypeColormap *nsp_type_colormap=NULL;

/*
 * Type object for NspColormap 
 * all the instance of NspTypeColormap share the same id. 
 * nsp_type_colormap: is an instance of NspTypeColormap 
 *    used for objects of NspColormap type (i.e built with new_colormap) 
 * other instances are used for derived classes 
 */
NspTypeColormap *new_type_colormap(type_mode mode)
{
  NspTypeColormap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_colormap != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_colormap;
    }
  if (( type =  malloc(sizeof(NspTypeColormap))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = colormap_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = colormap_get_methods;
  type->gtk_methods = FALSE;
  type->new = (new_func *) new_colormap;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for colormap */ 

  top->pr = (print_func *) nsp_colormap_print;
  top->dealloc = (dealloc_func *) nsp_colormap_destroy;
  top->copy  =  (copy_func *) nsp_colormap_copy;
  top->size  = (size_func *) nsp_colormap_size;
  top->s_type =  (s_type_func *) nsp_colormap_type_as_string;
  top->sh_type = (sh_type_func *) nsp_colormap_type_short_string;
  top->info = (info_func *) nsp_colormap_info;
  /* top->is_true = (is_true_func  *) nsp_colormap_is_true; */
  /* top->loop =(loop_func *) nsp_colormap_loop;*/
  top->path_extract = (path_func *)  object_path_extract;
  top->get_from_obj = (get_from_obj_func *) nsp_colormap_object;
  top->eq  = (eq_func *) nsp_colormap_eq;
  top->neq  = (eq_func *) nsp_colormap_neq;
  top->save  = (save_func *) nsp_colormap_xdr_save;
  top->load  = (load_func *) nsp_colormap_xdr_load;
  top->create = (create_func*) int_colormap_create;
  top->latex = (print_func *) nsp_colormap_latex;
  top->full_copy = (copy_func *) nsp_colormap_full_copy;

  /* specific methods for colormap */

  type->init = (init_func *) init_colormap;

  /* 
   * NspColormap interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_colormap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeColormap called nsp_type_colormap
       */
      type->id =  nsp_type_colormap_id = nsp_new_type_id();
      nsp_type_colormap = type;
      if ( nsp_register_type(nsp_type_colormap) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_colormap(mode);
    }
  else 
    {
      type->id = nsp_type_colormap_id;
      return type;
    }
}

/*
 * initialize NspColormap instances 
 * locally and by calling initializer on parent class 
 */

static int init_colormap(NspColormap *Obj,NspTypeColormap *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of NspColormap 
 */

NspColormap *new_colormap() 
{
  NspColormap *loc;
  /* type must exists */
  nsp_type_colormap = new_type_colormap(T_BASE);
  if ( (loc = malloc(sizeof(NspColormap)))== NULLCOLORMAP) return loc;
  /* initialize object */
  if ( init_colormap(loc,nsp_type_colormap) == FAIL) return NULLCOLORMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspColormap 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_colormap_size(NspColormap *Mat, int flag)
{
  return 1;
}

/*
 * type as string 
 */

static char colormap_type_name[]="Colormap";
static char colormap_short_type_name[]="colormap";

static char *nsp_colormap_type_as_string(void)
{
  return(colormap_type_name);
}

static char *nsp_colormap_type_short_string(NspObject *v)
{
  return(colormap_short_type_name);
}

/*
 * A == B 
 */

static int nsp_colormap_eq(NspColormap *A, NspObject *B)
{
  NspColormap *loc = (NspColormap *) B;
  if ( check_cast(B,nsp_type_colormap_id) == FALSE) return FALSE ;
  return TRUE;
}

/*
 * A != B 
 */

static int nsp_colormap_neq(NspColormap *A, NspObject *B)
{
  return ( nsp_colormap_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

int nsp_colormap_xdr_save(XDR *xdrs, NspColormap *M)
{
  /* if (nsp_xdr_save_id(xdrs,NSP_OBJECT(M)) == FAIL) return FAIL;*/
  /* if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL; */ 
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_colormap)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

NspColormap  *nsp_colormap_xdr_load_partial(XDR *xdrs, NspColormap *M)
{
  int fid;
  char name[NAME_MAXL];
 return M;
}

static NspColormap  *nsp_colormap_xdr_load(XDR *xdrs)
{
  NspColormap *H = NULL;
  char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLCOLORMAP;
  if ((H  = nsp_colormap_create_void(name,(NspTypeBase *) nsp_type_colormap))== NULLCOLORMAP) return H;
  if ( nsp_colormap_create_partial(H) == FAIL) return NULLCOLORMAP;
  if ((H  = nsp_colormap_xdr_load_partial(xdrs,H))== NULLCOLORMAP) return H;
  if ( nsp_colormap_check_values(H) == FAIL) return NULLCOLORMAP;
  return H;
}

/*
 * delete 
 */

void nsp_colormap_destroy_partial(NspColormap *H)
{
  nsp_gobject_destroy_partial((NspGObject *) H);
}

void nsp_colormap_destroy(NspColormap *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  nsp_colormap_destroy_partial(H);
  FREE(H);
}

/*
 * info 
 */

int nsp_colormap_info(NspColormap *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLCOLORMAP) 
    {
      Sciprintf("Null Pointer NspColormap \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=\t\t%s\n", (pname==NULL) ? "" : pname,
             nsp_colormap_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}

/*
 * print 
 */

int nsp_colormap_print(NspColormap *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLCOLORMAP) 
    {
      Sciprintf("Null Pointer NspColormap \n");
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
          nsp_colormap_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=\t\t%s \n",pname, nsp_colormap_type_short_string(NSP_OBJECT(M)) );
      Sciprintf1(indent+1,"{\n");
      Sciprintf1(indent+1,"}\n");
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_colormap_latex(NspColormap *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=\t\t%s\n",pname, nsp_colormap_type_short_string(NSP_OBJECT(M)));
  Sciprintf1(indent+1,"{\n");
  Sciprintf1(indent+1,"}\n");
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}
/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspColormap objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspColormap   *nsp_colormap_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_colormap_id) == TRUE ) return ((NspColormap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_colormap));
  return NULL;
}

int IsColormapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_colormap_id);
}

int IsColormap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_colormap_id);
}

NspColormap  *GetColormapCopy(Stack stack, int i)
{
  if (  GetColormap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspColormap  *GetColormap(Stack stack, int i)
{
  NspColormap *M;
  if (( M = nsp_colormap_object(NthObj(i))) == NULLCOLORMAP)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspColormap instance 
 *-----------------------------------------------------*/

static NspColormap *nsp_colormap_create_void(const char *name,NspTypeBase *type)
{
 NspColormap *H  = (type == NULL) ? new_colormap() : type->new();
 if ( H ==  NULLCOLORMAP)
  {
   Sciprintf("No more memory\n");
   return NULLCOLORMAP;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLCOLORMAP;
 NSP_OBJECT(H)->ret_pos = -1 ;
 return H;
}

int nsp_colormap_create_partial(NspColormap *H)
{
  return OK;
}

int nsp_colormap_check_values(NspColormap *H)
{
  return OK;
}

NspColormap *nsp_colormap_create(const char *name,,NspTypeBase *type)
{
  NspColormap *H  = nsp_colormap_create_void(name,type);
  if ( H ==  NULLCOLORMAP) return NULLCOLORMAP;
  if ( nsp_colormap_check_values(H) == FAIL) return NULLCOLORMAP;
  return H;
}


NspColormap *nsp_colormap_create_default(const char *name)
{
 NspColormap *H  = nsp_colormap_create_void(name,NULL);
 if ( H ==  NULLCOLORMAP) return NULLCOLORMAP;
 if ( nsp_colormap_check_values(H) == FAIL) return NULLCOLORMAP;
 return H;
}

/*
 * copy for gobject derived class  
 */

NspColormap *nsp_colormap_copy_partial(NspColormap *H,NspColormap *self)
{
  return H;
}

NspColormap *nsp_colormap_copy(NspColormap *self)
{
  NspColormap *H  =nsp_colormap_create_void(NVOID,(NspTypeBase *) nsp_type_colormap);
  if ( H ==  NULLCOLORMAP) return NULLCOLORMAP;
  if ( nsp_gobject_copy_partial((NspGObject *) H,(NspGObject *) self ) == NULL) return NULLCOLORMAP;
  if ( nsp_colormap_copy_partial(H,self)== NULL) return NULLCOLORMAP;

  return H;
}
/*
 * full copy for gobject derived class
 */

NspColormap *nsp_colormap_full_copy_partial(NspColormap *H,NspColormap *self)
{
  return H;
}

NspColormap *nsp_colormap_full_copy(NspColormap *self)
{
  NspColormap *H  =nsp_colormap_create_void(NVOID,(NspTypeBase *) nsp_type_colormap);
  if ( H ==  NULLCOLORMAP) return NULLCOLORMAP;
  if ( nsp_gobject_full_copy_partial((NspGObject *) H,(NspGObject *) self ) == NULL) return NULLCOLORMAP;
  if ( nsp_colormap_full_copy_partial(H,self)== NULL) return NULLCOLORMAP;

  return H;
}

/*-------------------------------------------------------------------
 * wrappers for the NspColormap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

int int_colormap_create(Stack stack, int rhs, int opt, int lhs)
{
  NspColormap *H;
  CheckStdRhs(0,0);
  /* want to be sure that type colormap is initialized */
  nsp_type_colormap = new_type_colormap(T_BASE);
  if(( H = nsp_colormap_create_void(NVOID,(NspTypeBase *) nsp_type_colormap)) == NULLCOLORMAP) return RET_BUG;
  /* then we use optional arguments to fill attributes */
  if ( int_create_with_attributes((NspObject  *) H,stack,rhs,opt,lhs) == RET_BUG)  return RET_BUG;
 if ( nsp_colormap_check_values(H) == FAIL) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *colormap_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab colormap_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_Ctestzzz(Stack stack, int rhs, int opt, int lhs) /* testzzz */
{
  int_types T[] = {s_double, s_int, s_int, s_int,t_end};
  double zz;
  int xx1, xx2, xx3, ret;
  if ( GetArgs(stack,rhs,opt,T,&zz, &xx1, &xx2, &xx3) == FAIL) return RET_BUG;
    ret = Ctestzzz(zz, xx1, xx2, xx3);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_Ctestxxx(Stack stack, int rhs, int opt, int lhs) /* testxxx */
{
  int_types T[] = {s_double, s_double, s_double,t_end};
  double zz1, zz2, zz3;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&zz1, &zz2, &zz3) == FAIL) return RET_BUG;
    ret = Ctestxxx(zz1, zz2, zz3);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab test_func[]={
  {"testzzz", _wrap_Ctestzzz},
  {"testxxx", _wrap_Ctestxxx},
  { "test_create", int_test_create},
  { NULL, NULL}
};

/* call ith function in the test interface */

int test_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(test_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void test_Interf_Info(int i, char **fname, function (**f))
{
  *fname = test_func[i].name;
  *f = test_func[i].fonc;
}

#line 524 "test.c"
