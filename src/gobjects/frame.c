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
 */

#define  GFrame_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/graphics/Graphics.h"

/* graphic frame 
 * NspGFrame inherits from NspObject 
 */

int nsp_type_gframe_id=0;
NspTypeGFrame *nsp_type_gframe=NULL;

NspTypeGFrame *new_type_gframe(type_mode mode)
{
  NspTypeGFrame *type = NULL;
  NspTypeObject *top;
  if ( nsp_type_gframe != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gframe;
    }
  if ((type =  malloc(sizeof(NspTypeGFrame)))== NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gframe_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods = gframe_get_methods;
  type->new = (new_func *) new_gframe;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gframe */ 

  top->pr = (print_func *) gframe_print;                    
  top->dealloc = (dealloc_func *) gframe_destroy;
  top->copy  =  (copy_func *) gframe_copy;                   
  top->size  = (size_func *) gframe_size;                  
  top->s_type =  (s_type_func *) gframe_type_as_string;    
  top->sh_type = (sh_type_func *) gframe_type_short_string;
  top->info = (info_func *) gframe_info;                    
  /* top->is_true = (is_true_func  *) GFrameIsTrue;            */
  /*top->loop =(loop_func *) gframe_loop;*/
  top->path_extract = (path_func *) gframe_path_extract;
  top->get_from_obj = (get_from_obj_func *) gframe_object;  
  top->eq  = (eq_func *) gframe_eq;
  top->neq  = (eq_func *) gframe_neq;
  top->save  = (save_func *) gframe_xdr_save;
  top->load  = (load_func *) gframe_xdr_load;
  top->create = (create_func*) int_gframe_create;

  /* specific methods for gframe */

  type->init = (init_func *) init_gframe;
  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  
  if ( nsp_type_gframe_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_gframe
       */
      type->id =  nsp_type_gframe_id = nsp_new_type_id();
      nsp_type_gframe = type;
      if ( nsp_register_type(nsp_type_gframe) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gframe(mode);
    }
  else 
    {
      type->id = nsp_type_gframe_id;
      return type;
    }
}
/*
 * initialize Gframe instances 
 * locally and by calling initializer on parent class 
 */

static int init_gframe(NspGFrame *o,NspTypeGFrame *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GFrame 
 */

NspGFrame *new_gframe() 
{
  NspGFrame *loc; 
  /* type must exists */
  nsp_type_gframe = new_type_gframe(T_BASE);
  if ( (loc = malloc(sizeof(NspGFrame)))== NULLGFRAME) return loc;
  /* initialize object */
  if ( init_gframe(loc,nsp_type_gframe) == FAIL) return NULLGFRAME;
  return loc;
}


/*----------------------------------------------
 * Object method redefined for GFrame 
 *-----------------------------------------------*/

 /*
  * size 
  */

static int gframe_size(NspGFrame *o, int flag)
{
  return nsp_list_length(o->obj->objs);
}

/*
 * type as string 
 */

static char gframe_type_name[]="GFrame";
static char gframe_short_type_name[]="gf";

static char *gframe_type_as_string(void)
{
  return(gframe_type_name);
}

static char *gframe_type_short_string(NspObject *v)
{
  return(gframe_short_type_name);
}

static NspObject *gframe_path_extract(NspGFrame *H,int n, NspObject **Objs)
{
  int i;
  if ( n != 1 ) return NULLOBJ;
  if ( IntScalar(*Objs,&i) == FAIL) return NULLOBJ ;
  if ( i >= 1 && i <=nsp_list_length(H->obj->objs))
    return nsp_list_get_element(H->obj->objs,i);
  else
    return NULLOBJ; 
}


static int gframe_eq(NspGFrame *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gframe_id) == FALSE) return FALSE ;
  if ( A->obj == ((NspGFrame *) B)->obj ) return TRUE ;
  return FALSE;
}

static int gframe_neq(NspGFrame *A, NspObject *B)
{
  return gframe_eq(A,B)== TRUE ? FALSE : TRUE ;
}

/*
 * save 
 */

static int gframe_xdr_save(XDR  *xdrs, NspGFrame *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;

  if ( nsp_xdr_save_array_d(xdrs,M->obj->scale,4) == FAIL) return FAIL;
  if ( nsp_xdr_save_array_d(xdrs,M->obj->r,4) == FAIL) return FAIL;
  if ( nsp_object_xdr_save(xdrs,NSP_OBJECT(M->obj->objs))== FAIL)  return FAIL;
  return OK;
}

/*
 * load 
 */

static void gframe_recompute_pointers(NspGFrame *F);

static NspGFrame  *gframe_xdr_load(XDR  *xdrs)
{
  NspGFrame *M=NULLGFRAME;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGFRAME;
  if (( M = gframe_create(name,NULL,FALSE,NULL,NULL,NULL)) == NULLGFRAME) return NULLGFRAME;
  if ( nsp_xdr_load_array_d(xdrs,M->obj->scale,4) == FAIL) return NULLGFRAME;
  if ( nsp_xdr_load_array_d(xdrs,M->obj->r,4) == FAIL) return NULLGFRAME;
  if ((M->obj->objs =(NspList *) nsp_object_xdr_load(xdrs))== NULL)  return NULLGFRAME;
  /* restore lost pointers */
  gframe_set_frame_field(M);
  /*
   * restore interconnections 
   */
  gframe_recompute_pointers(M);
  return M;
}

static void gframe_destroy(NspGFrame *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     nsp_list_destroy(H->obj->objs);
     FREE(H->obj);
   }
  FREE(H);
}

static void gframe_info(NspGFrame *H, int indent,char *name,int rec_level)
{
  int i;
  if ( H == NULLGFRAME) 
    {
      Sciprintf("Null Pointer GFrame \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t=\t\t%s (%d) [0x%d,count=%d]\n",NSP_OBJECT(H)->name,
	    gframe_type_short_string(NSP_OBJECT(H)), nsp_list_length(H->obj->objs),
	    H->obj,H->obj->ref_count );
}

static void gframe_print(NspGFrame *H, int indent,char *name, int rec_level)
{
  gframe_info(H,indent,NULL,0);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGFrame   *gframe_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast(O,nsp_type_gframe_id) == TRUE) return ((NspGFrame *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gframe));
  return(NULL);
}

int IsGFrameObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gframe_id);
}

int IsGFrame(NspObject *O)
{
  return nsp_object_type(O , nsp_type_gframe_id);
}

NspGFrame  *GetGFrameCopy(Stack stack, int i)
{
  if (  GetGFrame(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFrame  *GetGFrame(Stack stack, int i)
{
  NspGFrame *M;
  if (( M = gframe_object(NthObj(i))) == NULLGFRAME)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

static NspGFrame *gframe_create_void(char *name,NspTypeBase *type)
{
  NspGFrame *H  = (type == NULL) ? new_gframe() : type->new();
  if ( H ==  NULLGFRAME )
    {
      Sciprintf("No more memory\n");
      return NULLGFRAME;
    }
  if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
    return NULLGFRAME;
  NSP_OBJECT(H)->ret_pos = -1 ;
  H->obj = NULL;
  return H;
}

NspGFrame *gframe_create(char *name,BCG *Xgc,int init_objs,const double *scale,
			 double *r,NspTypeBase *type)
{
  int i;
  NspGFrame *H  = gframe_create_void(name,type);
  if ( H ==  NULLGFRAME ) return NULLGFRAME;
  if ((H->obj = malloc(sizeof(nsp_gframe))) == NULL) return NULL;
  H->obj->ref_count=1;
  if ( r!= NULL)   for ( i=0; i < 4 ; i++) H->obj->r[i]=r[i];
  if ( scale != NULL) for ( i=0; i < 4 ; i++) H->obj->scale[i]=scale[i];
  if ( init_objs ) 
    {
      if ( ( H->obj->objs =nsp_list_create("objs"))== NULLLIST) return NULLGFRAME;
    }
  else 
    {
      H->obj->objs = NULLLIST;
    }
  H->obj->Xgc = Xgc;
  return H;
}

NspGFrame *gframe_copy(NspGFrame *self)
{
  NspGFrame *H  = gframe_create_void(NVOID,(NspTypeBase *) nsp_type_gframe);
  if ( H ==  NULLGFRAME) return NULLGFRAME;
  H->obj = self->obj;
  self->obj->ref_count++;
  return H;
}


/*-------------------------------------------------------------------
 * wrappers for the GFrame class 
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_gframe_create(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc=NULL;
  int i,winid;
  NspGFrame *H;
  NspMatrix *scale,*r;

  CheckRhs(3,1000);

  if ( opt != 0 ) 
    { 
      Scierror("Error: optional arguments are not expected\n");
      return RET_BUG;
    }
  if ((scale =GetRealMat(stack,1)) == NULLMAT ) return FAIL;
  CheckLength(NspFname(stack),1,scale,4);
  if ((r =GetRealMat(stack,2)) == NULLMAT ) return FAIL;
  CheckLength(NspFname(stack),2,r,4);
  if ( GetScalarInt(stack,3,&winid) == FAIL) return RET_BUG;
  if (winid != -1) 
    {
      Xgc =  window_list_search(winid);
      if ( Xgc == NULL ) Xgc= check_graphic_window();
    }
  
  if(( H = gframe_create(NVOID,Xgc,TRUE,scale->R,r->R,NULL)) == NULLGFRAME) return RET_BUG;
  for ( i = 4 ; i <= rhs ; i++ )
    {
      if ( MaybeObjCopy(&NthObj(i)) == NULL)  return RET_BUG;
      if ( nsp_object_set_name(NthObj(i),"lel") == FAIL) return RET_BUG;
      /* change according to object type */
      if ( IsBlock(NthObj(i)) ||  IsConnector(NthObj(i)))
	{
	  if (nsp_list_end_insert(H->obj->objs,NthObj(i)) == FAIL ) return RET_BUG;
	}
      else if ( IsLink(NthObj(i))) 
	{ 
	  if (nsp_list_insert(H->obj->objs,NthObj(i),0) == FAIL ) return RET_BUG;
	}
      /* If NthObj(i) is not copied it is inserted in the list 
       * we must set then NthObj(i) to NULLOBJ 
       * to prevent the cleaning process to clean the object 
       * that we have inserted in our list */
      NthObj(i) = NULLOBJ ;
    }
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

/* generic function for scalar attribute */
#ifdef NOK
#define INT_ATTRIBUTE_GET_SCALAR(name,path)	\
  static NspObject *CNAME(int_attribute_get_,name)(void *Hv,char *attr) { \
    return nsp_create_object_from_double(NVOID,(path)->name); } 

INT_ATTRIBUTE_GET_SCALAR(mycolor,((NspBlock *) Hv)->obj);

static NspObject * int_gframe_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspGFrame *) Hv)->obj->color);
}

static int int_gframe_set_color(void *Hv, char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGFrame *)Hv)->obj->color = color;
  return OK ;
}

static NspObject * int_gframe_get_thickness(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspGFrame *) Hv)->thickness);
}
                           
static int int_gframe_set_thickness(void *Hv, char *attr, NspObject *O)
{
  int thickness;
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspGFrame *)Hv)->thickness = thickness;
  return OK ;
}

static NspObject * int_gframe_get_background(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspGFrame *) Hv)->background);
}
 
static int int_gframe_set_background(void *Hv, char *attr, NspObject *O)
{
  int background;
  if (  IntScalar(O,&background) == FAIL) return FAIL;
  ((NspGFrame *)Hv)->background = background;
  return OK ;
}

#endif 

static NspObject *int_gframe_get_scale(void *self,char *attr)
{
  return (NspObject *) nsp_matrix_create_from_array(NVOID,1,4,((NspGFrame *)self)->obj->r,NULL);
}

static int int_gframe_set_scale(void *self,const  char *attr, NspObject *O)
{
  int i;
  NspMatrix *A; 
  if ((A = matrix_object (O)) == NULLMAT ) return RET_BUG;
  if ( A->mn != 4 && A->rc_type == 'r' ) return RET_BUG;
  for ( i= 0 ; i < 4 ; i++) 
    ((NspGFrame *) self)->obj->r[i]= A->R[i];
  return OK;
}

static AttrTab gframe_attrs[] = {
#ifdef NOK
  { "color",        int_gframe_get_color ,     int_gframe_set_color ,     NULL },
  { "background",    int_gframe_get_background,  int_gframe_set_background,  NULL },
  { "thickness",    int_gframe_get_thickness,  int_gframe_set_thickness,  NULL },
#endif
  { "scale", int_gframe_get_scale, int_gframe_set_scale, NULL},
  { (char *) 0, NULL}

};

/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/

/* draw */

int int_gfdraw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  gframe_draw(self);
  MoveObj(stack,1,self);
  return 1;
}

/* select_and_move */

int int_gf_select_and_move(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  gframe_select_and_move(((NspGFrame *) self),pt->R);
  return 0;
}
/* select_and_move */

int int_gf_select_and_hilite(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep;
  NspObject *bool;
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  rep= gframe_select_and_hilite(((NspGFrame *) self),pt->R);
  if ((bool = nsp_create_boolean_object(NVOID,(rep == OK) ? TRUE : FALSE))
      == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,bool);
  return 1;
}

/* split link */

int int_gf_select_and_split(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  gframe_select_and_split(((NspGFrame *) self),pt->R);
  return 0;
}

/* split link */

int int_gf_select_link_and_add_control(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  gframe_select_link_and_add_control(((NspGFrame *) self),pt->R);
  return 0;
}

/* shorten link */

int int_gf_select_link_and_remove_control(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  gframe_select_link_and_remove_control(((NspGFrame *) self),pt->R);
  return 0;
}


int int_gf_hilite_near_pt(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(NspFname(stack),1,pt,2);
  gframe_hilite_near_pt(((NspGFrame *) self),pt->R);
  return 0;
}

int int_gf_new_block(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj = gframe_create_new_block(((NspGFrame *) self)))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

int int_gf_new_connector(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj = gframe_create_new_connector(((NspGFrame *) self)))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

int int_gf_new_rect(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  gframe_create_new_rect(((NspGFrame *) self));
  return 0;
}

int int_gf_new_link(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj;
  CheckRhs(0,0);
  CheckLhs(-1,1);
  if ((obj =   gframe_create_new_link(((NspGFrame *) self)))== NULL) return RET_BUG;
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  MoveObj(stack,1,obj);
  return 1;
}

int int_gf_delete_hilited(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  gframe_delete_hilited(((NspGFrame *) self));
  return 0;
}

int int_gf_get_selection(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj,*bool;
  CheckRhs(0,0);
  CheckLhs(2,2);
  if ((obj = gframe_get_hilited(((NspGFrame *) self)))== NULL) 
    {
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,1,bool);
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,2,bool);
      return 2;
    }
  /* since obj is kept on the frame we must return a copy */
  if ((obj=nsp_object_copy(obj)) == NULLOBJ) return RET_BUG;
  if ((bool = nsp_create_boolean_object(NVOID,TRUE)));
  MoveObj(stack,1,bool);
  MoveObj(stack,2,obj);
  return 2;
}

int int_gf_get_selection_copy(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspObject *obj,*bool;
  NspTypeGRint *bf;
  CheckRhs(0,0);
  CheckLhs(2,2);
  if ((obj = gframe_get_hilited(((NspGFrame *) self)))== NULL) 
    {
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,1,bool);
      if ((bool = nsp_create_boolean_object(NVOID,FALSE)));
      MoveObj(stack,2,bool);
      return 2;
    }
  /* we want here a full copy 
   * we assume that obj implements grint interface 
   */
  bf = GR_INT(obj->basetype->interface);
  if ((obj = bf->full_copy(obj))== NULLOBJ)  return RET_BUG;
  if ((bool = nsp_create_boolean_object(NVOID,TRUE)));
  MoveObj(stack,1,bool);
  MoveObj(stack,2,obj);
  return 2;
}


int int_gf_insert(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  CheckLhs(-1,0);
  NspBlock  *B; 
  NspLink *L;
  NspConnector *C;
  NspObject *obj=NULL;
  int flag = TRUE;
  if ( IsBlockObj(stack,1) )
    { 
      if ((B=GetBlockCopy(stack,1)) == NULLBLOCK) return RET_BUG;
      B->obj->frame = ((NspGFrame *) self)->obj;
      obj = NSP_OBJECT(B);
    }
  else if ( IsLinkObj(stack,1))
    { 
      if ((L=GetLinkCopy(stack,1)) == NULLLINK) return RET_BUG;
      L->obj->frame = ((NspGFrame *) self)->obj;
      obj = NSP_OBJECT(L);
      flag = FALSE;
    }
  else if ( IsConnectorObj(stack,1))
    { 
      if ((C=GetConnectorCopy(stack,1)) == NULLCONNECTOR) return RET_BUG;
      C->obj->frame = ((NspGFrame *) self)->obj;
      obj = NSP_OBJECT(C);
    }

  if (nsp_object_set_name(obj,"lel") == FAIL) return RET_BUG;
  if ( flag ) 
    {
      if (nsp_list_end_insert(((NspGFrame *) self)->obj->objs,obj) == FAIL ) return RET_BUG;
    }
  else 
    {
      if (nsp_list_insert(((NspGFrame *) self)->obj->objs,obj,0) == FAIL ) return RET_BUG;
    }
  return 0;
}


int int_gf_attach_to_window(void *self,Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int winid;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ( GetScalarInt(stack,1,&winid) == FAIL) return RET_BUG;
  Xgc =  window_list_search(winid);
  if ( Xgc == NULL ) 
    {
      Scierror("Error: Graphic window %d does not exists\n",winid);
      return RET_BUG;
    }
  ((NspGFrame *) self)->obj->Xgc = Xgc;
  
  return 0;
}

int int_gf_full_copy(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspGFrame *F;
  CheckRhs(0,0);
  CheckLhs(0,1);
  if ((F = frame_full_copy((NspGFrame *) self))== NULLGFRAME) 
    {
      Scierror("Error: copy failed\n");
      return RET_BUG;
    }
  MoveObj(stack,1,NSP_OBJECT(F));
  return 1;
}

void gframe_tops(NspGFrame *R,char *fname);

int int_gf_tops(void *self,Stack stack, int rhs, int opt, int lhs)
{
  int rep=1,color=-1;
  char *filename= NULL, *mode = NULL;
  static char *Table[] = {"d", "l", "n", "p", "k", NULL};
  int_types T[] = {string, new_opts, t_end} ;
  nsp_option opts[] ={{ "color",s_bool,NULLOBJ,-1},
		      { "mode",string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  if ( GetArgs(stack,rhs,opt,T,&filename,&opts,&color,&mode) == FAIL) return RET_BUG;
  if ( mode != NULL) 
    {
      rep = is_string_in_array(mode,Table,1);
      if ( rep < 0 ) 
	{
	  string_not_in_array(stack,mode,Table,"optional argument mode");
	  return RET_BUG;
	}
    }
  /* XXXXXXXXX gframe_tops(self,filename); */
  return 0;
}


static NspMethods gframe_methods[] = {
  { "draw",   int_gfdraw},
  { "tops",   int_gf_tops},
  { "new_link", int_gf_new_link },
  { "new_block", int_gf_new_block },
  { "new_connector", int_gf_new_connector },
  { "new_rect", int_gf_new_rect },
  { "hilite_near_pt", int_gf_hilite_near_pt },
  { "select_and_move", int_gf_select_and_move},
  { "select_and_hilite", int_gf_select_and_hilite},
  { "select_and_split", int_gf_select_and_split},
  { "select_link_and_add_control", int_gf_select_link_and_add_control},
  { "select_link_and_remove_control", int_gf_select_link_and_remove_control},
  { "delete_hilited", int_gf_delete_hilited },
  { "insert",int_gf_insert},
  { "get_selection",int_gf_get_selection},
  { "get_selection_copy",int_gf_get_selection_copy},
  { "attach_to_window",int_gf_attach_to_window},
  { "copy",int_gf_full_copy},
  { (char *) 0, NULL}
};

static NspMethods *gframe_get_methods(void) { return gframe_methods;};


/******************************************************
 *  Interface 
 ******************************************************/

static OpTab GFrame_func[]={
  {"setrowscols_gf",int_set_attributes}, 
  {(char *) 0, NULL}
};

/* call ith function in the GFrame interface */

int GFrame_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GFrame_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GFrame_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GFrame_func[i].name;
  *f = GFrame_func[i].fonc;
}

/*********************************************************************
 * GFrame Object in Scilab : a graphic GFrameangle 
 *********************************************************************/

static int pixmap = FALSE ; /* XXXXX */

/**
 * gframe_draw:
 * @R: a graphic frame  
 * 
 * draw the objects contained in frame @R.
 * 
 **/

void gframe_draw(NspGFrame *R)
{
  BCG *Xgc = R->obj->Xgc;
  Cell *C = R->obj->objs->first;
  if ( Xgc == NULL) return;
  /* using current values */
  Nsetscale2d(Xgc,NULL,NULL,R->obj->scale,"nn");
  Xgc->graphic_engine->clearwindow(Xgc);
  /* XXX xtape('replay',win); */
  /* 
   * XXX A GFrame could be casted info a Rect 
   * for drawing its boundaries 
   */
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* assuming all objects here implements grint */
	  GR_INT(C->O->basetype->interface)->draw(C->O);
	}
      C = C->next ;
    }
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
}

/**
 * gframe_draw:
 * @R: a graphic frame  
 * 
 * draw the objects contained in frame @R.
 * 
 **/

extern BCG ScilabGCPos ; /* Postscript */
extern Gengine Pos_gengine;

void gframe_tops(NspGFrame *R,char *fname)
{
  int wdim[2],*wdim_p=NULL;
  int zero=0,un=1,colored=TRUE;
  BCG *Xgc = R->obj->Xgc;
  if ( Xgc == NULL) return;
  R->obj->Xgc->graphic_engine->xget_windowdim(R->obj->Xgc,wdim,wdim+1);
  wdim_p = wdim;
  ScilabGCPos.graphic_engine = &Pos_gengine;
  ScilabGCPos.graphic_engine->initgraphic(fname,&Xgc->CurWindow,wdim_p,NULL,NULL,NULL,'k');
  if (colored == TRUE ) 
    ScilabGCPos.graphic_engine->xset_usecolor(&ScilabGCPos,un);
  else
    ScilabGCPos.graphic_engine->xset_usecolor(&ScilabGCPos,zero);
  R->obj->Xgc = &ScilabGCPos;
  gframe_draw(R);
  ScilabGCPos.graphic_engine->xend(&ScilabGCPos);
  R->obj->Xgc = Xgc;
}


/**
 * gframe_select_obj:
 * @R: a graphic frame  
 * @pt: point coordinates.
 * @Objs: an array of objects.
 * @exclude: an object to be excluded from search.
 * 
 * selects the first object of @R which contains the point @pt and returns 
 * the result in @Objs. @exclude can be used to exclude an object from the search.
 * 
 * Return value: 0 or the position of the object found in the list.
 *
 **/

int gframe_select_obj(NspGFrame *R,const double pt[2], NspObject **Objs, NspObject *exclude) 
{
  int count = 1;
  Cell *C = R->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && C->O != exclude )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->contains_pt(C->O,pt) ) 
	    {
	      *Objs = C->O;
	      return count;
	    }
	}
      C = C->next ;
      count++;
    }
  return 0;
}


/**
 * gframe_select_lock:
 * @F: a graphic frame  
 * @pt: point coordinates.
 * @O: an object 
 * @cp: lock point id 
 * @lock_c: is lock connectable.
 *
 * If @pt is close enough to an object lock point, then the object is 
 * returned in @O, the lock point id in @cp and the connectable status in @lock_c.
 *  
 * return value: a non null integer in case of success
 **/


int gframe_select_lock(NspGFrame *F,double pt[2], NspObject **O, int *cp, int *lock_c) 
{
  int count = 1;
  Cell *C = F->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->lock_near_pt(C->O,pt,cp) == TRUE ) 
	    {
	      *O = C->O;
	      *lock_c = bf->is_lock_connectable(C->O,*cp);
	      return count;
	    }
	}
      C = C->next ;
      count++;
    }
  return 0;
}

/*
 * set the frame field for all objects 
 * Note that all object can be casted to block for accessing that field
 * XXXX this should be turned into o GR_INT method 
 **/

void gframe_set_frame_field(NspGFrame *F)
{
  int count = 1;
  Cell *C = F->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  NspBlock *B = (NspBlock *) C->O; 
	  B->obj->frame = F->obj;
	}
      C = C->next ;
      count++;
    }
}

/*
 * set the frame field for all objects 
 * Note that all object can be casted to block for accessing that field
 * XXXX this should be turned into o GR_INT method 
 **/

void * gframe_get_adress(NspGFrame *F,void *old )
{
  int count = 1;
  Cell *C = F->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  NspBlock *B = (NspBlock *) C->O; 
	  if ( B->obj->object_sid  == old) 
	    return C->O;
	}
      C = C->next ;
      count++;
    }
  return NULL;
}


/**
 * gframe_locks_set_show:
 * @F:  a #NspGFrame 
 * @O: a #NspObject 
 * @val: a flag as an integer
 * 
 * sets the show attribute to value @val for all the objects which 
 * are connected to object @O by lock connections.
 **/

static void gframe_locks_set_show(NspGFrame *F,NspObject *O,int val)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK && p.object_id != NULL) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  bf1->set_show(O1,val);
		}
	    }
	}
    }
}

/**
 * gframe_select_and_move:
 * @R: a #NspGFrame 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and move it with the mouse.
 * 
 * Return value: %OK or %FAIL
 **/

int gframe_select_and_move(NspGFrame *R,const double pt[2])
{
  BCG *Xgc;
  int k1, cp;
  NspTypeGRint *bf;
  NspObject *O;
  int k = gframe_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  /* are we inside a control point ? */
  bf = GR_INT(O->basetype->interface);
  k1 = bf->control_near_pt(O,pt,&cp);
  /* hide the moving object and its locked objects */
  bf->set_show(O,FALSE);
  if ( IsBlock(O)|| IsConnector(O) )  gframe_locks_set_show(R,O,FALSE);
  gframe_unhilite_objs(R,FALSE);
  bf->set_hilited(O,TRUE);
  if (0) 
    {
      /* global draw of all but the moving object 
       * we record the state to redraw faster. 
       * Pb this will reset scales to default and we do not want this XXX
       */
      Xgc = R->obj->Xgc;
      Xgc->graphic_engine->clearwindow(Xgc);
      Xgc->graphic_engine->xset_recording(Xgc,TRUE);
      Xgc->graphic_engine->tape_clean_plots(Xgc,Xgc->CurWindow);
      gframe_draw(R);
      Xgc->graphic_engine->xset_recording(Xgc,FALSE);
    }
  else
    {
      gframe_draw(R);
    }
  /* */
  bf->set_show(O,TRUE);
  if ( IsBlock(O) || IsConnector(O) )  gframe_locks_set_show(R,O,TRUE);
  if ( k1 == FALSE ) 
    {
      if ( gframe_move_obj(R,O, pt, -5,cp,MOVE ) == -100) 
	return OK;
    }
  else
    {
      if ( gframe_move_obj(R,O, pt, -5,cp,MOVE_CONTROL ) == -100) 
	return OK;
    }
  gframe_draw(R);
  return OK;
}



/**
 * gframe_select_and_hilite:
 * @R: a #NspGFrame 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and hilite the object. Other hilite objects are 
 * unhilited.
 * 
 * Return value: %OK or %FAIL
 **/

int gframe_select_and_hilite(NspGFrame *R,const double pt[2])
{
  NspTypeGRint *bf;
  NspObject *O;
  int k = gframe_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  bf = GR_INT(O->basetype->interface);
  gframe_unhilite_objs(R,FALSE);
  bf->set_hilited(O,TRUE);
  return OK;
}



/**
 * gframe_select_and_split:
 * @R: a #NspGFrame 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and if this object is a link the link is splited.
 * 
 * Return value: %OK or %FAIL
 **/

int gframe_select_and_split(NspGFrame *R,const double pt[2])
{
  int rep=OK;
  NspObject *Ob;
  int k = gframe_select_obj(R,pt,&Ob,NULL);
  if ( k==0 ) return FAIL;
  if ( IsLink(Ob) ) 
    {
      NspLink *link;
      rep= link_split(R,(NspLink *) Ob,&link,pt);
      gframe_draw(R);
    }
  return rep;
}

/**
 * gframe_select_link_and_add_control:
 * @R: a #NspGFrame 
 * @pt: a point position 
 * 
 * selects the  object which is near the point @pt 
 * and if this object is a link a control point is added to the link.
 * 
 * Return value: %OK or %FAIL
 * FIXME: are we also supposed to highlight the link ? 
 **/

int gframe_select_link_and_add_control(NspGFrame *R,const double pt[2])
{
  int rep=OK;
  NspObject *O;
  int k = gframe_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  if ( IsLink(O) ) 
    {
      rep= link_add_control((NspLink *)O,pt);
      gframe_draw(R);
    }
  return rep;
}

/**
 * gframe_select_link_and_remove_control:
 * @R: a #NspGFrame 
 * @pt: a point position 
 * 
 * selects the object which is near the point @pt 
 * and if this object is a link a control point is added to the link.
 * 
 * Return value: %OK or %FAIL
 **/

int gframe_select_link_and_remove_control(NspGFrame *R,const double pt[2])
{
  int rep=OK;
  NspObject *O;
  int k = gframe_select_obj(R,pt,&O,NULL);
  if ( k==0 ) return FAIL;
  if ( IsLink(O) ) 
    {
      rep= link_remove_control((NspLink *)O,pt);
      gframe_draw(R);
    }
  return rep;
}

/**
 * gframe_hilite_near_pt:
 * @R: a #NspGFrame 
 * @pt: a point position 
 * 
 * highlights the object which is near the point @pt.
 * 
 * Return value: 
 **/

int  gframe_hilite_near_pt(NspGFrame *R,const double pt[2])
{
  NspObject *O;
  int k = gframe_select_obj(R,pt,&O,NULL);
  if ( k==0 )
    {
      gframe_unhilite_objs(R,TRUE);
    }
  else 
    {
      NspTypeGRint *bf = GR_INT(O->basetype->interface);
      gframe_unhilite_objs(R,FALSE);
      bf->set_hilited(O,TRUE);
      gframe_draw(R);
    }
  return OK;
}

/**
 * gframe_locks_draw:
 * @R: a #NspGFrame 
 * @O: a #NspObject. 
 * 
 * calls the draw method on the objects which are connected
 * to object @O by lock points.
 **/

static void gframe_locks_draw(NspGFrame *R,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  /* Scierror("XXX gframe_locks_draw lock= %d ports=%d\n",i,np); */
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK && p.object_id != NULL) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  bf1->draw(O1);
		}
	    }
	}
    }
}


/**
 * gframe_locks_update:
 * @R: a #NspGFrame 
 * @O: a #NspObject. 
 *
 * Updates the position of the control points of 
 * objects which are locked to object @O. 
 * this is usefull when moving  block to update links 
 * positions.
 * 
 **/

void gframe_locks_update(NspGFrame *R,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      lock_dir dir;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK && p.object_id != NULL) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  double pt[2];
		  bf->get_lock_pos(O,i,pt);
		  /* updates links acordingly and try to keep 
		   * prefered direction 
		   */
		  dir = bf->get_lock_dir(O,i);		  
		  bf1->set_lock_pos(O1,p.lock,pt,TRUE,dir);
		}
	    }
	}
    }

}

/**
 * ZZZZ
 **/

static void gframe_recompute_obj_pointers(NspGFrame *R,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  for ( i = 0 ; i < n ; i++) 
    {
      int np = bf->get_number_of_ports(O,i);
      int j;
      for ( j= 0 ; j < np ; j++) 
	{
	  gr_port p;
	  if ( bf->get_lock_connection(O,i,j,&p)== OK ) 
	    {
	      if ( p.object_sid != NULL) 
		{
		  void *new= gframe_get_adress(R,p.object_sid );
		  p.object_id = new;
		  p.object_sid = NULL;
		  /* A uniformiser */
		  bf->set_lock_connection(O,i,j,&p);
		}
	    }
	}
    }
}


/*
 *
 **/

static void gframe_recompute_pointers(NspGFrame *F)
{
  int count = 1;
  Cell *C = F->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  gframe_recompute_obj_pointers(F,C->O);
	}
      C = C->next ;
      count++;
    }
}


/**
 * gframe_move_obj:
 * @F: : a #NspGFrame 
 * @O: the #NspObject to be moved. 
 * @pt: the initial position of the mouse.
 * @stop: an integer giving the mouse code to accept for ending the move 
 * @cp: the id of the control point to be moved
 * @action: %MOVE or %MOVE_CONTROL
 * 
 * 
 * 
 * Return value: 
 **/

int gframe_move_obj(NspGFrame *F,NspObject *O,const double pt[2],int stop,int cp,move_action action)
{
  int record,rep;
  BCG *Xgc = F->obj->Xgc;
  int alumode = Xgc->graphic_engine->xget_alufunction(Xgc);
  int wstop = 0, ibutton, iwait=FALSE;
  double mpt[2],pt1[2]= {pt[0],pt[1]},ptwork[2];
  NspTypeGRint *bf = GR_INT(O->basetype->interface);

  record = Xgc->graphic_engine->xget_recording(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
  
  if ( action == MOVE_CONTROL) 
    {
      bf->move_control_init(O,cp,ptwork);
    }
  /*
   * mpt is the mouse position, 
   * ptwork is the control point position 
   */

  while ( wstop==0 ) 
    {
      if (0)
	{
	  /* draw the rest of the world : using recorded state */
	  Xgc->graphic_engine->clearwindow(Xgc);
	  Xgc->graphic_engine->xset_recording(Xgc,TRUE);
	  Xgc->graphic_engine->tape_replay(Xgc,Xgc->CurWindow);
	  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
	  /* draw the moving block */
	  bf->draw(O);
	  if ( IsBlock(O)  || IsConnector(O))  gframe_locks_draw(F,O);
	}
      else 
	{
	  /* full redraw for fast graphics */
	  gframe_draw(F);
	}
      if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
      /* get new mouse position */
      Xgc->graphic_engine->scale->xgetmouse(Xgc,"one",&ibutton,mpt,mpt+1,iwait,TRUE,TRUE,FALSE);
      if ( ibutton == -100 ) 
	{
	  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
	  Xgc->graphic_engine->xset_recording(Xgc,record);
	  return ibutton;
	}
      if ( ibutton == stop ) wstop= 1;
      Xgc->graphic_engine->xinfo(Xgc,"ibutton=%d",ibutton);
      /* clear block shape using redraw */
      /* if ( pixmap ) Xgc->graphic_engine->xset_show(); */
      /* move object */
      switch ( action ) 
	{
	case MOVE : 
	  rep= bf->translate(O,(pt1[0]= mpt[0] -pt1[0],pt1[1]=mpt[1] -pt1[1],pt1));
	  if ( rep == FAIL) wstop=1; /* quit untranslatable objects */
	  break;
	case MOVE_CONTROL :
	  bf->move_control(F,O,mpt,cp, ptwork);
	}
      /* update locks positions for objects locked to objects  */ 
      gframe_locks_update(F,O);
      pt1[0] = mpt[0];
      pt1[1] = mpt[1];
    }
  if ( IsLink(O)) link_check(F,(NspLink *)O);

  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
  Xgc->graphic_engine->xset_recording(Xgc,record);
  return ibutton;
}


/**
 * gframe_unhilite_objs:
 * @R:  a #NspGFrame 
 * @draw: an integer 
 * 
 * unhighlight the highlighted objects of @R if 
 * @draw is equal to %TRUE the objects are redrawn.
 * 
 **/

void gframe_unhilite_objs(NspGFrame *R,int draw )
{
  int ok = FALSE;
  Cell *C = R->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE) 
	    {
	      bf->set_hilited(C->O,FALSE);
	      ok = TRUE;
	    }
	}
      C = C->next ;
    }
  if ( ok == TRUE && draw == TRUE )  gframe_draw(R);
}

/**
 * gframe_delete_hilited:
 * @R: : a #NspGFrame 
 * 
 * delete hilited objects of @F.
 **/

void gframe_delete_hilited(NspGFrame *R) 
{
  Cell *C = R->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* grint interface */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE ) 
	    {
	      nsp_object_destroy(&C->O);
	      C->O = NULLOBJ;
	    }
	}
      C = C->next ;
    }
  /* 
   * here we must compact the list which can have holes 
   * but it implies that lock number are to be properly updated 
   * FIXME ? 
   */
}
/**
 * gframe_get_hilited:
 * @R: : a #NspGFrame 
 * 
 * return the first hilited object of the list of objects 
 * contained in @R.
 * 
 **/

NspObject * gframe_get_hilited(NspGFrame *R) 
{
  Cell *C = R->obj->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* grint interface */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE ) 
	    {
	      return C->O;
	    }
	}
      C = C->next ;
    }
  return NULL;
}

/**
 * gframe_create_new_block:
 * @F: a #NspGFrame 
 * 
 * creates a new block which is positioned interactively and 
 * inserted in @F.
 * 
 * Return value: %OK or %FALSE.
 **/

NspObject * gframe_create_new_block(NspGFrame *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,10,10}, pt[]={0,100};
  NspBlock *B;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  B=block_create("fe",rect,color,thickness,background,NULL);
  if ( B == NULLBLOCK) return NULLOBJ;
  B->obj->frame = F->obj;
  B->obj->hilited = TRUE;
  if (nsp_list_end_insert(F->obj->objs,(NspObject  *) B) == FAIL) return NULLOBJ;
  rep= gframe_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  if ( pixmap ) F->obj->Xgc->graphic_engine->xset_show(F->obj->Xgc);
  return NSP_OBJECT(B);
}

/**
 * gframe_create_new_connector:
 * @F: a #NspGFrame 
 * 
 * create a new connector which is positioned interactively and 
 * inserted in @F.
 * 
 * Return value: %OK or %FALSE
 **/

NspObject * gframe_create_new_connector(NspGFrame *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,4,4}, pt[]={0,100};
  NspConnector *B;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  B=connector_create("fe",rect,color,thickness,background,NULL);
  if ( B == NULL) return NULLOBJ;
  B->obj->frame = F->obj;
  B->obj->hilited = TRUE;
  if (nsp_list_end_insert(F->obj->objs,(NspObject  *) B) == FAIL) return NULLOBJ;
  rep= gframe_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return NULLOBJ;
  /* XXXX block_draw(B); */
  if ( pixmap ) F->obj->Xgc->graphic_engine->xset_show(F->obj->Xgc);
  return NSP_OBJECT(B);
}

/**
 * gframe_create_new_rect:
 * @F: a #NspGFrame 
 * 
 * create a new rectangle which is positioned interactively and 
 * inserted in @F.
 * 
 * Return value: %OK or %FALSE
 **/

int gframe_create_new_rect(NspGFrame *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,4,4}, pt[]={0,100};
  NspRect *B;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  B=rect_create("fe",F->obj->Xgc,rect,color,thickness,background,NULL);
  if ( B == NULL) return FAIL;
  if (nsp_list_end_insert(F->obj->objs,(NspObject  *) B) == FAIL) return FAIL;
  rep= gframe_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return FAIL;
  /* XXXX block_draw(B); */
  if ( pixmap ) F->obj->Xgc->graphic_engine->xset_show(F->obj->Xgc);
  return OK;
}


/**
 * gframe_create_new_link:
 * @F: a #NspGFrame 
 * 
 * Interactively creates a new link (#NspLink) and inserts 
 * the link in object @F.
 * 
 * Return value: %FAIL or %OK.
 *
 **/

static double lock_size=1;

NspObject * gframe_create_new_link(NspGFrame *F)
{
  BCG *Xgc= F->obj->Xgc;
  NspObject *Ob;
  int cp1,record;
  double mpt[2],pt[2];
  int alumode = Xgc->graphic_engine->xget_alufunction(Xgc), wstop = 0,stop=2, ibutton, iwait=FALSE;
  int color=4,thickness=1,hvfactor,count=0;
  NspLink *L;
  NspTypeGRint *bf;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  hvfactor=lock_size*2;/* magnetism toward horizontal and vertical line  */
  Xgc->graphic_engine->xinfo(Xgc,"Enter polyline, Right click to stop");

  record = Xgc->graphic_engine->xget_recording(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
  
  /* prepare a link with 1 points */
  L= link_create_n("fe",1,color,thickness);
  L->obj->frame = F->obj;

  bf = GR_INT(((NspObject *) L)->basetype->interface);

  if ( L == NULLLINK) return NULLOBJ;
  L->obj->hilited = TRUE;
  L->obj->poly->R[0]=mpt[0];
  L->obj->poly->R[1]=mpt[0];
  while ( wstop==0 ) 
    {
      gframe_draw(F);
      /* draw the link */
      bf->draw(L);
      if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
      /* get new mouse position */
      Xgc->graphic_engine->scale->xgetmouse(Xgc,"one",&ibutton,mpt,mpt+1,iwait,TRUE,TRUE,FALSE);
      if ( ibutton == -100 ) 
	{
	  /* we stop : window was killed */
	  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
	  Xgc->graphic_engine->xset_recording(Xgc,record);
	  return NULLOBJ;
	}
      if ( ibutton == stop ) 
	{
	  /* here we stop with a right click 
	   */
	  if (  count >= 2  ) 
	    {
	      double *x = L->obj->poly->R, *y = L->obj->poly->R + L->obj->poly->m;
	      /* try to improve angles before quit */
	      /*  magnetism toward horizontal or vertival lines */
	      if ( Abs( x[count] - x[count-1] ) < hvfactor ) x[count-1] = x[count];
	      if ( Abs( y[count] - y[count-1] ) < hvfactor ) y[count-1] = y[count];
	    }              
	}
      /* clear link shape using redraw */
      /* bf->draw(L); */
      
      if ( ibutton == stop ) 
	{
	  break; 
	}
      else if ( ibutton == 0 ) 
	{
	  /* this is a left click click 
	   * If the left click is near a lock point we stop 
	   * 
	   */
	  int lock_c, rep;
	  /* are we near a lock point ? if true mpt is changed  */
	  pt[0]=mpt[0]; pt[1]=mpt[1];
	  rep = gframe_select_lock(F,mpt, &Ob, &cp1,&lock_c) ;
	  /* link_check will check if the lock point is already in use */
	  if ( rep != 0 )
	    {
	      /* set last point to lock position and stop if it's not the first point*/
	      L->obj->poly->R[count]= mpt[0];
	      L->obj->poly->R[count+L->obj->poly->m]= mpt[1];
	      if ( count != 0) 
		{
		  if ( count >= 2 ) 
		    {
		      double *x = L->obj->poly->R, *y = L->obj->poly->R + L->obj->poly->m;
		      /* try to improve angles before quit */
		      /*  magnetism toward horizontal or vertival lines */
		      if ( Abs( x[count] - x[count-1] ) < hvfactor ) x[count-1] = x[count];
		      if ( Abs( y[count] - y[count-1] ) < hvfactor ) y[count-1] = y[count];
		    }		  
		  /* we have finished */
		  break;
		}
	    }
	  if ( nsp_matrix_add_rows(L->obj->poly,1)== FAIL ) return NULLOBJ;	  
	  count ++;
	  L->obj->poly->R[count]= mpt[0];
	  L->obj->poly->R[count+L->obj->poly->m]= mpt[1];
	}
      else 
	{
	  int lock_c;
	  /* just moving */
	  /* are we near a lock point ? if true mpt is changed  */
	  int rep = gframe_select_lock(F,mpt, &Ob, &cp1,&lock_c) ;
	  if ( rep == 0 && count != 0 ) 
	    {
	      /*  try to keep horizontal and vertical lines */
	      if ( Abs( L->obj->poly->R[count-1] - mpt[0]) < hvfactor ) mpt[0]=L->obj->poly->R[count-1];
	      if ( Abs( L->obj->poly->R[count-1+L->obj->poly->m] - mpt[1]) < hvfactor ) 
		mpt[1]=L->obj->poly->R[count-1+L->obj->poly->m];
	    }              
	  L->obj->poly->R[count]= mpt[0];
	  L->obj->poly->R[count+L->obj->poly->m]= mpt[1];
	}
    }
  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
  /* insert link in frame 
   * at the start 
   */
  if (nsp_list_insert(F->obj->objs,(NspObject  *) L,0) == FAIL) return NULLOBJ;
  /* check if first and last points are locked 
   * if true update locks 
   */
  mpt[0]=L->obj->poly->R[0];
  mpt[1]=L->obj->poly->R[L->obj->poly->m];
  link_lock_update(F,L,0,mpt);
  mpt[0]=L->obj->poly->R[L->obj->poly->m-1];
  mpt[1]=L->obj->poly->R[2*L->obj->poly->m-1];
  link_lock_update(F,L,1,mpt);
  link_check(F,L);
  bf->draw(L);
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
  Xgc->graphic_engine->xset_recording(Xgc,record);
  return NSP_OBJECT(L);
}

/*
 * Make a full copy of object B
 * this is to be inserted in grint 
 */

static NspList * nsp_list_full_copy(NspList *L);

static NspGFrame *frame_full_copy( NspGFrame *F)
{
  NspGFrame *M=NULLGFRAME;
  if (( M = gframe_create(NVOID,NULL,FALSE,F->obj->scale,F->obj->r,NULL)) == NULLGFRAME) 
    return NULLGFRAME;
  if ((M->obj->objs = nsp_list_full_copy(F->obj->objs))== NULL)  return NULLGFRAME;
  /* restore lost pointers */
  gframe_set_frame_field(M);
  /*
   * restore interconnections 
   */
  gframe_recompute_pointers(M);
  return M;
}


static NspList * nsp_list_full_copy(NspList *L)
{
  NspObject *obj=NULL;
  NspList *Loc;
  Cell *cloc;
  if ( (Loc = nsp_list_create(NVOID)) == NULLLIST ) return NULLLIST;
  cloc = L->first ;
  while ( cloc != NULLCELL ) 
    {
      if ( cloc->O != NULLOBJ ) 
	{
	  NspTypeGRint *bf= GR_INT(cloc->O->basetype->interface);
	  if ( (obj = bf->full_copy(cloc->O)) == NULLOBJ )  goto err;
	  if ( nsp_object_set_name(obj,nsp_object_get_name(cloc->O)) == FAIL ) goto err;
	}
      if ( nsp_list_end_insert(Loc, obj) == FAIL ) goto err;
      cloc = cloc->next;
    }
  return Loc;

 err:
  nsp_list_destroy(Loc);
  return NULLLIST;
} 


