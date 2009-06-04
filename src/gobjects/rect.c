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

#define  Rect_Private 
#include "nsp/object.h"

#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include <nsp/graphics/Graphics.h>

/* graphic rectangle 
 * NspRect inherits from NspObject 
 */

int nsp_type_rect_id=0;
NspTypeRect *nsp_type_rect=NULL;

NspTypeRect *new_type_rect(type_mode mode)
{
  NspTypeRect *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_rect != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_rect;
    }
  
  if ((type =  malloc(sizeof(NspTypeRect))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  rect_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  rect_get_methods;
  type->new = (new_func *) new_rect;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for rect */ 

  top->pr = (print_func *) rect_print;                    
  top->dealloc = (dealloc_func *) rect_destroy;            
  top->copy  =  (copy_func *) rect_copy;
  top->size  = (size_func *) rect_size;                  
  top->s_type =  (s_type_func *) rect_type_as_string;    
  top->sh_type = (sh_type_func *) rect_type_short_string;
  top->info = (info_func *) rect_info ;                    
  /* top->is_true = (is_true_func  *) rect_is_true; */
  /* top->loop =(loop_func *) rect_loop_extract ; */
  top->path_extract = (path_func *)  object_path_extract ;
  top->get_from_obj = (get_from_obj_func *) rect_object;
  top->eq  = (eq_func *) rect_eq;
  top->neq  = (eq_func *) rect_neq;
  top->save  = (save_func *) rect_xdr_save;
  top->load  = (load_func *) rect_xdr_load;

  /* specific methods for rect */
      
  type->init = (init_func *) init_rect;
      
  /* 
   * Rect interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_rect_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeRect called nsp_type_rect
       */
      type->id =  nsp_type_rect_id = nsp_new_type_id();
      nsp_type_rect = type;
      if ( nsp_register_type(nsp_type_rect) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_rect(mode);
    }
  else 
    {
      type->id = nsp_type_rect_id;
      return type;
    }
}

/*
 * initialize Rect instances 
 * locally and by calling initializer on parent class 
 */

static int init_rect(NspRect *o,NspTypeRect *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Rect 
 */

NspRect *new_rect() 
{
  NspRect *loc; 
  /* type must exists */
  nsp_type_rect = new_type_rect(T_BASE);
  if ( (loc = malloc(sizeof(NspRect)))== NULLRECT) return loc;
  /* initialize object */
  if ( init_rect(loc,nsp_type_rect) == FAIL) return NULLRECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Rect 
 *-----------------------------------------------*/

 /*
  * size 
  */

static int rect_size(NspRect *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char rect_type_name[]="GRect";
static char rect_short_type_name[]="gr";

static char *rect_type_as_string(void)
{
  return(rect_type_name);
}

static char *rect_type_short_string(NspObject *v)
{
  return(rect_short_type_name);
}



static int rect_compare(NspRect * A,NspRect * B)
{
  int i;
  for ( i = 0 ; i < 4 ; i++) 
    if ( A->r[i] != B->r[i]) return FALSE;
  if ( A->color != B->color) return FALSE;
  if ( A->thickness != B->thickness) return FALSE;
  if ( A->background != B->background) return FALSE;
  return TRUE;
}


static int rect_eq(NspRect *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_rect_id) == FALSE) return FALSE ;
  return rect_compare(A,(NspRect *) B);
}

static int rect_neq(NspRect *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_rect_id) == FALSE) return TRUE;
  return rect_compare(A,(NspRect *) B);
}

/*
 * save 
 */

static int rect_xdr_save(XDR  *xdrs, NspRect *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->color) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->thickness) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs,M->background) == FAIL) return FAIL;
  if (nsp_xdr_save_array_d(xdrs,M->r,4) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspRect  *rect_xdr_load(XDR  *xdrs)
{
  int color,thickness,background;
  NspRect *M;
  char name[NAME_MAXL];
  double rect[4];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLRECT;
  if (nsp_xdr_load_i(xdrs,&color) == FAIL) return NULLRECT;
  if (nsp_xdr_load_i(xdrs,&thickness) == FAIL) return NULLRECT;
  if (nsp_xdr_load_i(xdrs,&background) == FAIL) return NULLRECT;
  if (nsp_xdr_load_array_d(xdrs,rect,4) == FAIL) return NULLRECT;
  if (( M= rect_create(name,NULL,rect,color,thickness,background,NULL)) == NULLRECT ) return NULLRECT;
  return M;
}

/*
 * delete
 */


static void rect_destroy(NspRect *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  FREE(H);
}

/*
 * info
 */
 


static int rect_info(NspRect *H, int indent,char *name,int rec_level)
{
  int i;
  if ( H == NULLRECT) 
    {
      Sciprintf("Null Pointer Rect \n");
      return TRUE;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[Rect %s, r=[%5.2f,%5.2f,%5.2f,%5.2f] co=%d, th=%d bg=%d]\n",
	    NSP_OBJECT(H)->name,H->r[0],H->r[1],H->r[2],H->r[3],H->color,H->thickness,H->background);
  return TRUE;
}

/*
 * print
 */

static int rect_print(NspRect *H, int indent,char *name, int rec_level)
{
  rect_info(H,indent,NULL,0);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces
 *-----------------------------------------------------*/

NspRect   *rect_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast(O,nsp_type_rect_id) == TRUE) return ((NspRect *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_rect));
  return(NULL);
}

int IsRectObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_rect_id);
}

int IsRect(NspObject *O)
{
  return nsp_object_type(O , nsp_type_rect_id);
}

NspRect  *GetRectCopy(Stack stack, int i)
{
  if (  GetRect(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspRect  *GetRect(Stack stack, int i)
{
  NspRect *M;
  if (( M = rect_object(NthObj(i))) == NULLRECT)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor
 * if type is non NULL it is a subtype which can be used to
 * create a NspClassA instance
 *-----------------------------------------------------*/

NspRect *rect_create(char *name,BCG *Xgc,double rect[],int color,int thickness,int background,NspTypeBase *type)
{
  int i;
  NspRect *H = (type == NULL) ? new_rect(): type->new();
  if ( H ==  NULLRECT)
    {
      Sciprintf("No more memory\n");
      return NULLRECT;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return(NULLRECT);
  NSP_OBJECT(H)->ret_pos = -1 ;
  for ( i=0; i < 4 ; i++) H->r[i]=rect[i];
  H->color = color;
  H->thickness = thickness;
  H->background = background;
  H->Xgc = Xgc;
  return H;
}

/*
 * copy 
 */

static NspRect *rect_copy(NspRect *H)
{
  return rect_create(NVOID,H->Xgc,H->r,H->color,H->thickness,H->background,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the ClassA
 * i.e functions at Nsp level
 *-------------------------------------------------------------------*/

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);

int int_grcreate(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspRect *H;
  double *val=NULL;
  int back=-1,color=-1,thickness=-1;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(2,8); /* first is ignored */

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&thickness) == FAIL) return RET_BUG;
  Xgc= check_graphic_window();
  if ( back <= 0 )  back  = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( color <= 0 ) color = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( thickness < 0 ) thickness = Xgc->graphic_engine->xget_thickness(Xgc);
  if(( H = rect_create(NVOID,Xgc,val,color,thickness,back,NULL)) == NULLRECT) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val)
{
  NspMatrix *M1;
  int i;
  static double l[4];
  switch ( rhs -opt ) 
    {
    case 2 :
      if ((M1=GetRealMat(stack,2)) == NULLMAT ) return FAIL;
      CheckLength(NspFname(stack),1,M1,4);
      *val = M1->R;
      break;
    case 5 :
      for ( i = 2 ; i <= 5 ; i++) 
	{
	  if (GetScalarDouble(stack,i,l+i-1) == FAIL) return FAIL;
	}
      *val = l;
      break;
    default :
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 4\r\n",NspFname(stack),rhs);
      return FAIL;
    }
  return OK;
}

/*------------------------------------------------------
 * attributes  (set/get methods)
 *------------------------------------------------------*/

static NspObject * int_grect_get_color(void *Hv,const char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspRect *) Hv)->color);
}
                                                                                                      
static int int_grect_set_color(void *Hv,const  char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspRect *)Hv)->color = color;
  return OK ;
}

static NspObject * int_grect_get_thickness(void *Hv,const char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspRect *) Hv)->thickness);
}
                                                                                                      
static int int_grect_set_thickness(void *Hv,const char *attr, NspObject *O)
{
  int thickness;
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspRect *)Hv)->thickness = thickness;
  return OK ;
}

static NspObject * int_grect_get_background(void *Hv,const char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspRect *) Hv)->background);
}
                                                                                                      
static int int_grect_set_background(void *Hv,const  char *attr, NspObject *O)
{
  int background;
  if (  IntScalar(O,&background) == FAIL) return FAIL;
  ((NspRect *)Hv)->background = background;
  return OK ;
}

static AttrTab rect_attrs[] = {
  { "color",        int_grect_get_color ,     int_grect_set_color ,     NULL, NULL  },
  { "thickness",    int_grect_get_thickness,  int_grect_set_thickness,  NULL, NULL  },
  { "background",   int_grect_get_background, int_grect_set_background, NULL, NULL  },
  { (char *) 0, NULL, NULL , NULL }
};

/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/

/* draw */

int int_grect_draw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  RectDraw((NspRect *) self);
  return 0;
}

/* translate */

int int_grect_translate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),2,M,2);
  RectTranslate((NspRect *) self,M->R);
  NSP_OBJECT(self)->ret_pos = 1;
  return 1;
}

/* resize */ 

int int_grect_resize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),2,M,2);
  RectResize((NspRect *) self,M->R);
  NSP_OBJECT(self)->ret_pos = 1;
  return 1;
}


static NspMethods rect_methods[] = {
  { "translate", int_grect_translate},
  { "resize",   int_grect_resize},
  { "draw",   int_grect_draw},
  { (char *) 0, NULL}
};
                                                                                                      
static NspMethods *rect_get_methods(void) { return rect_methods;};
                                                                                                      
                                                                                                 
/*----------------------------------------------------
 * Interface
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/


/******************************************************
 *  Interface 
 ******************************************************/

static OpTab Rect_func[]={
  {"setrowscols_gr",int_set_attributes},
  {"gr_create", int_grcreate},
  {(char *) 0, NULL}
};

/* call ith function in the Rect interface */

int Rect_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Rect_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void Rect_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Rect_func[i].name;
  *f = Rect_func[i].fonc;
}

/*********************************************************************
 * Rect Object in Scilab : a graphic Rectangle 
 *********************************************************************/

/*
 * Draw 
 */

void RectDraw(NspRect *R)
{
  BCG *Xgc;
  int cpat, cwidth;
  Xgc=check_graphic_window();
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
  Xgc->graphic_engine->xset_pattern(Xgc,R->background);
  Xgc->graphic_engine->scale->fillrectangle(Xgc,R->r);
  Xgc->graphic_engine->xset_pattern(Xgc,R->color);
  Xgc->graphic_engine->scale->drawrectangle(Xgc,R->r);
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}

/*
 * translate 
 */

void RectTranslate(NspRect *R,const double pt[2])
{
  R->r[0] += pt[0] ;
  R->r[1] += pt[1] ;
}

/*
 * resize 
 */

void RectResize(NspRect *R,const double size[2])
{
  R->r[2] = size[0] ;
  R->r[3] = size[1] ;
}




