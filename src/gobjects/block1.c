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
 *
 * An example of an Object which inherits from NspBlock 
 * and which performs a MatPlot. 
 */

#define  GridBlock_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/graphics-old/Graphics.h"
#include "nsp/parse.h"
#include "gridblock.h" 

/* 
 * NspGridBlock inherits from NspBlock. 
 * A NspGridBlock, is a NspBlock with 
 * a list of sub-blocks in a nsp_gframe. 
 * It is used to implement a super block. 
 */

int nsp_type_gridblock_id=0;
NspTypeGridBlock *nsp_type_gridblock=NULL;

NspTypeGridBlock *new_type_gridblock(type_mode mode)
{
  NspTypeGridBlock *type = NULL;
  NspTypeObject *top;
  NspTypeGRint *gri;

  if ( nsp_type_gridblock != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gridblock;
    }
  if (( type =  malloc(sizeof(NspTypeGridBlock))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_block(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  gridblock_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  gridblock_get_methods; 
  type->new = (new_func *) new_gridblock;
  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gridblock */ 

  top->pr = (print_func *) gridblock_print;                    
  top->dealloc = (dealloc_func *) gridblock_destroy;            
  top->copy  =  (copy_func *) gridblock_copy;                   
  top->size  = (size_func *) gridblock_size;                  
  top->s_type =  (s_type_func *) gridblock_type_as_string;    
  top->sh_type = (sh_type_func *) gridblock_type_short_string;
  top->info = (info_func *) gridblock_info;                    
  /* top->is_true = (is_true_func  *) GridBlockIsTrue;           */
  /* top->loop =(loop_func *) gridblock_loop; */
  top->path_extract =    (path_func *)  object_path_extract ;       
  top->get_from_obj = (get_from_obj_func *) gridblock_object  ;  
  top->eq  = (eq_func *) gridblock_eq;
  top->neq  = (eq_func *) gridblock_neq;
  top->save  = (save_func *) gridblock_xdr_save;
  top->load  = (load_func *) gridblock_xdr_load;
  top->create = (create_func*) int_gridblock_create;
  
  /* specific methods for gridblock */

  type->init =  (init_func *) init_gridblock;

  /* GridBlock implement grint interface */
  /* first get it from father */

  gri = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) gri;

  /* copy from father interface */
  *gri = *((NspTypeGRint *) type->surtype->interface);
  
  /* change localy */
  gri->draw  =(gr_draw *) gridblock_draw;
  gri->full_copy =(gr_full_copy *) gridblock_full_copy;
  gri->translate = (gr_translate *) gridblock_translate;
  gri->set_pos = (gr_set_pos *) gridblock_set_pos;
  gri->get_pos = (gr_get_pos *) gridblock_get_pos;
  gri->get_rect = (gr_get_rect *) gridblock_get_rect;
  gri->resize = (gr_resize *) gridblock_resize;
  gri->move_control = ( gr_move_control *) gridblock_move_control;

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
 * initialize GridBlock instances 
 * locally and by calling initializer on parent class 
 */

static int init_gridblock(NspGridBlock *o,NspTypeGridBlock *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GridBlock 
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
 * Object method redefined for GridBlock 
 *-----------------------------------------------*/

 /*
  * size 
  */

static int gridblock_size(NspGridBlock *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gridblock_type_name[]="GridBlock";
static char gridblock_short_type_name[]="grbl";

static char *gridblock_type_as_string(void)
{
  return(gridblock_type_name);
}

static char *gridblock_type_short_string(NspObject *v)
{
  return(gridblock_short_type_name);
}

/* used in for x=y where y is a GridBlock */

static int gridblock_eq(NspGridBlock *A, NspObject *B)
{
  if ( check_cast(B,nsp_type_gridblock_id) == FALSE) return FALSE ;
  if ( A->obj == ((NspGridBlock *) B)->obj ) return TRUE ;
  return FALSE;
}

static int gridblock_neq(NspGridBlock *A, NspObject *B)
{
  return gridblock_eq(A,B)== TRUE ? FALSE : TRUE ;
}

/*
 * save 
 */

static int gridblock_xdr_save(XDR  *xdrs, NspGridBlock *M)
{
  NspBlock *G = (NspBlock *) M;
  int i;
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  /* the gridblock */
  if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(M)) == FAIL) return FAIL;
  if ( nsp_xdr_save_array_d(xdrs,G->obj->r,4) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,G->obj->color) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,G->obj->thickness) == FAIL) return FAIL;
  if ( nsp_xdr_save_i(xdrs,G->obj->background) == FAIL) return FAIL;
  /* the lock points */
  if ( nsp_xdr_save_i(xdrs,G->obj->n_locks) == FAIL) return FAIL;
  for ( i = 0 ; i < G->obj->n_locks ; i++) 
    {
      grb_lock *lock= G->obj->locks+i;
      if ( nsp_xdr_save_array_d(xdrs,lock->pt,2) == FAIL) return FAIL;
      if ( nsp_xdr_save_array_d(xdrs,lock->ptr,2) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,lock->type) == FAIL) return FAIL;
      /* the port */
      if ( nsp_xdr_save_i(xdrs,NSP_POINTER_TO_INT(lock->port.object_id)) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,lock->port.lock) == FAIL) return FAIL;
      if ( nsp_xdr_save_i(xdrs,lock->port.port) == FAIL) return FAIL;
    }
  return OK;
}

/*
 * load 
 */

static NspGridBlock  *gridblock_xdr_load(XDR  *xdrs)
{
  double r[4];
  int i,id,color,thickness,background;
  NspGridBlock *M=NULLGRIDBLOCK;
  NspBlock *B ;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGRIDBLOCK;
  /* the gridblock */
  if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  NULLGRIDBLOCK;
  if ( nsp_xdr_load_array_d(xdrs,r,4) == FAIL) return NULLGRIDBLOCK;
  if ( nsp_xdr_load_i(xdrs,&color) == FAIL) return NULLGRIDBLOCK;
  if ( nsp_xdr_load_i(xdrs,&thickness) == FAIL) return NULLGRIDBLOCK;
  if ( nsp_xdr_load_i(xdrs,&background) == FAIL) return NULLGRIDBLOCK;
  if (( M = gridblock_create(name,r,color,thickness,background,NULL)) == NULLGRIDBLOCK)
    return NULLGRIDBLOCK;
  B= (NspBlock *) M;
  B->obj->object_sid = NSP_INT_TO_POINTER(id);
  /* the lock points */
  if ( nsp_xdr_load_i(xdrs,&B->obj->n_locks) == FAIL) return NULLGRIDBLOCK;
  if ( B->obj->locks != NULL) FREE(B->obj->locks);
  if (( B->obj->locks = malloc(B->obj->n_locks*sizeof(grb_lock))) == NULL ) 
    return NULLGRIDBLOCK;
  for ( i = 0 ; i < B->obj->n_locks ; i++) 
    {
      grb_lock *lock= B->obj->locks+i;
      if ( nsp_xdr_load_array_d(xdrs,lock->pt,2) == FAIL) return NULLGRIDBLOCK;
      if ( nsp_xdr_load_array_d(xdrs,lock->ptr,2) == FAIL) return NULLGRIDBLOCK;
      if ( nsp_xdr_load_i(xdrs,&lock->type) == FAIL) return NULLGRIDBLOCK;
      /* the port */
      lock->port.object_id = NULLOBJ;
      if ( nsp_xdr_load_i(xdrs,&id) == FAIL) return  NULLGRIDBLOCK;
      lock->port.object_sid = NSP_INT_TO_POINTER(id);
      if ( nsp_xdr_load_i(xdrs,&lock->port.lock) == FAIL) return NULLGRIDBLOCK;
      if ( nsp_xdr_load_i(xdrs,&lock->port.port) == FAIL) return NULLGRIDBLOCK;
    }
  return M;
}

static void gridblock_destroy(NspGridBlock *H)
{
  NspBlock *B = (NspBlock *) H;
  nsp_object_destroy_name(NSP_OBJECT(H));
  H->obj->ref_count--;
  B->obj->ref_count--; 
  if ( B->obj->ref_count == 0 )
   {
     int i;
     for ( i = 0 ; i <  B->obj->n_locks ;i++)
       {
	 GR_INT(NSP_OBJECT(B)->basetype->interface)->unlock(B,i);
       }
     FREE(B->obj->locks);
     FREE(B->obj);
   }
  if ( H->obj->ref_count == 0 )
    {
      nsp_list_destroy(H->obj->objs);
      FREE(H->obj);
    }
  FREE(H);
}


static void gridblock_info(NspGridBlock *H, int indent,char *name,int rec_level)
{
  int i;
  if ( H == NULLGRIDBLOCK) 
    {
      Sciprintf("Null Pointer GridBlock \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t=\t\t%s (1) [0x%d,count=%d]\n",NSP_OBJECT(H)->name,
	    gridblock_type_short_string(NSP_OBJECT(H)), H->obj,H->obj->ref_count );
}

static int gridblock_print(NspGridBlock *H, int indent,char *name, int rec_level)
{
  gridblock_info(H,indent,NULL,0);
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGridBlock *gridblock_object(NspObject *O)
{
  /** Follow pointer **/
  HOBJ_GET_OBJECT(O,NULL);
  /** Check type **/
  if ( check_cast(O,nsp_type_gridblock_id) == TRUE) return ((NspGridBlock *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_gridblock));
  return(NULL);
}

int IsGridBlockObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gridblock_id);
}

int IsGridBlock(NspObject *O)
{
  return nsp_object_type(O , nsp_type_gridblock_id);
}

NspGridBlock  *GetGridBlockCopy(Stack stack, int i)
{
  if (  GetGridBlock(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGridBlock  *GetGridBlock(Stack stack, int i)
{
  NspGridBlock *M;
  if (( M = gridblock_object(NthObj(i))) == NULLGRIDBLOCK)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

static NspGridBlock *gridblock_create_void(char *name,NspTypeBase *type)
{
 NspGridBlock *H  = (type == NULL) ? new_gridblock() : type->new();
 if ( H ==  NULLGRIDBLOCK)
  {
   Sciprintf("No more memory\n");
   return NULLGRIDBLOCK;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULL)
   return NULLGRIDBLOCK;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}


NspGridBlock *gridblock_create(char *name,double *rect,int color,int thickness,int background,
			       NspTypeBase *type )
{
  NspGFrame *Gf;
  double gf_scale[]={0,0,100,100};
  double gf_rect[]={0,0,100,100};
  NspBlock *B;
  NspGridBlock *H  = gridblock_create_void(name,type);
  if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  B = (NspBlock *) H;
  /* create the part from father */
  if ( nsp_block_create(B,rect,color,thickness,background) == NULL) return NULLGRIDBLOCK;
  /* create the own part */
  if ((Gf = nsp_gframe_create("gf",NULL,TRUE,gf_scale,gf_rect,NULL)) == NULL) return NULLGRIDBLOCK;
  Gf->obj->top = FALSE;
  H->obj = Gf->obj;
  /* to prevent destruction of obj */
  Gf->obj->ref_count++;
  /* delete unused Gf */
  nsp_gframe_destroy(Gf);
  /* insert a first object in the frame */
  {
    int color=4,thickness=1, background=9;
    double rect[]={0,100,25,25};
    double rect1[]={50,50,25,25};
    NspBlock *B;
    B=block_create("fe",rect,color,thickness,background,NULL);
    if ( B == NULLBLOCK) return NULLGRIDBLOCK;
    B->obj->frame = H->obj;
    nsp_list_end_insert(H->obj->objs,(NspObject  *) B);
    B=block_create("fe",rect1,color,thickness,background,NULL);
    if ( B == NULLBLOCK) return NULLGRIDBLOCK;
    B->obj->frame = H->obj;
    nsp_list_end_insert(H->obj->objs,(NspObject  *) B);
  }
  return H;
}

/* create a NspGridBlock filled with a full copy of objects from @F
 *
 */

NspGridBlock *gridblock_create_from_nsp_gframe(char *name,double *rect,int color,int thickness,int background, NspGFrame *F) 
{
  int i;
  NspGFrame *Gf;
  double gf_scale[]={0,0,100,100};
  double gf_rect[]={0,0,100,100};
  NspBlock *B;
  NspGridBlock *H  = gridblock_create_void(name,NULL);
  if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  B = (NspBlock *) H;
  /* create the part from father */
  if ( nsp_block_create(B,rect,color,thickness,background) == NULL) return NULLGRIDBLOCK;
  /* create the own part */
  if ((Gf = nsp_gframe_full_copy(F))== NULLGFRAME) return NULLGRIDBLOCK; 
  Gf->obj->top = FALSE; /* not a top frame */
  H->obj = Gf->obj;
  /* to prevent destruction of obj */
  Gf->obj->ref_count++;
  /* XXXXX  delete unused Gf */
  /* gframe_destroy(Gf);*/
  for ( i=0; i < 4 ; i++) H->obj->r[i]=gf_rect[i];
  for ( i=0; i < 4 ; i++) H->obj->scale[i]=gf_scale[i];
  return H;
}

/*
 * copy for gobject derived class  
 */

NspGridBlock *gridblock_copy(NspGridBlock *self)
{
  NspGridBlock *H  =gridblock_create_void(NVOID,(NspTypeBase *) nsp_type_gridblock);
  if ( H ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  /* copy herited part */
  ((NspBlock *) H)->obj =   ((NspBlock *) self)->obj ;
  ((NspBlock *) H)->obj->ref_count++;
  /* copy self part */
  H->obj = self->obj;
  self->obj->ref_count++;
  return H;
}

/*-------------------------------------------------------------------
 * constructor at nsp level 
 * %types.GridBlock(...) or %types.GridBlock.new[]
 *-------------------------------------------------------------------*/

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);

static int int_gridblock_create(Stack stack, int rhs, int opt, int lhs)
{
  NspGridBlock *H;
  double *val=NULL;
  int back=-1,color=-1,thickness=-1;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = gridblock_create(NVOID,val,color,thickness,back,NULL)) == NULLGRIDBLOCK) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val)
{
  NspMatrix *M1;
  int i;
  static double l[4];
  switch ( rhs - opt ) 
    {
    case 1 :
      if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
      CheckLength(NspFname(stack),1,M1,4);
      *val = M1->R;
      break;
    case 4 :
      for ( i = 1 ; i <= 4 ; i++) 
	{
	  if (GetScalarDouble(stack,i,l+i-1) == FAIL) return FAIL;
	}
      *val = l;
      break;
    default :
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 4\r\n",NspFname(stack),rhs-opt);
      return FAIL;
    }
  return OK;
}

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static AttrTab gridblock_attrs[] = {
  { (char *) 0, NULL}
};

/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/



int int_gridblock_edit(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspGFrame *gf; 
  if ((gf = nsp_gframe_from_nspgframe(NVOID,NULL,((NspGridBlock *) self)->obj ))== NULLGFRAME) 
    return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(gf));
  return 1;
}

static NspMethods gridblock_methods[] = {
  { "edit", int_gridblock_edit},
  { (char *) 0, NULL}
};

static NspMethods *gridblock_get_methods(void) { return gridblock_methods;};

/*------------------------------------------------------
 *  Interface 
 *---------------------------------------------------*/

static OpTab GridBlock_func[]={
  {"setrowscols_gbl",int_set_attributes}, 
  {(char *) 0, NULL}
};

/* call ith function in the GridBlock interface */

int GridBlock_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GridBlock_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void GridBlock_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GridBlock_func[i].name;
  *f = GridBlock_func[i].fonc;
}


/*
 * implementation of the GRint interface 
 * for a gridblock 
 */ 

/**
 * gridblock_draw:
 * @B: a gridblock 
 *
 * Draws a gridblock given the current graphic driver. 
 *
 **/

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


/*
 * Make a full copy of object B
 * this is to be inserted in grint 
 */

static NspGridBlock * gridblock_full_copy( NspGridBlock *B)
{
  int i;
  double gf_scale[]={0,0,100,100};
  double gf_rect[]={0,0,100,100};
  NspBlock *Blnew, *Bl =((NspBlock *) B) ;
  NspGridBlock *Bnew  = gridblock_create_void(NVOID,NULL);
  if ( Bnew ==  NULLGRIDBLOCK) return NULLGRIDBLOCK;
  Blnew = (NspBlock *) Bnew;
  /* create the part from father */
  if ( nsp_block_create(Blnew, Bl->obj->r,Bl->obj->color, Bl->obj->thickness,Bl->obj->background) == NULL) 
    return NULLGRIDBLOCK;
  /* create the own part */
  if ((Bnew->obj = nspgframe_full_copy(B->obj,FALSE))  == NULL) 
    return NULLGRIDBLOCK;
  Bnew->obj->top = FALSE;
  for ( i=0; i < 4 ; i++) Bnew->obj->r[i]= gf_rect[i];
  for ( i=0; i < 4 ; i++) Bnew->obj->scale[i]= gf_scale[i];
  return Bnew;
}


/**
 * gridblock_tranlate:
 * @B: a gridblock 
 * @tr: translation vector 
 * 
 * Tranlates the gridblock origin (upper left point) using the 
 * value of vector @tr. 
 *
 **/

int gridblock_translate(NspGridBlock *B,const double tr[2])
{
  NspBlock *Bl = (NspBlock *) B;
  GR_INT(Bl->type->interface)->translate(Bl,tr);
  /* we want here to update the position of the frame inside its parent frame */
  memcpy(B->obj->r,Bl->obj->r,4*sizeof(double));
  return OK;
}

int gridblock_set_pos(NspGridBlock *B,const double tr[2])
{
  NspBlock *Bl = (NspBlock *) B;
  GR_INT(Bl->type->interface)->set_pos(Bl,tr);
  /* we want here to update the position of the frame inside its parent frame */
  memcpy(B->obj->r,Bl->obj->r,4*sizeof(double));
  Sciprintf("Set position of a gridblock\n");
  return OK;
}

void gridblock_get_pos(NspGridBlock *B, double tr[2])
{
  NspBlock *Bl = (NspBlock *) B;
  GR_INT(Bl->type->interface)->get_pos(Bl,tr);
}

void gridblock_get_rect(NspGridBlock *B, double r[4])
{
  NspBlock *Bl = (NspBlock *) B;
  GR_INT(Bl->type->interface)->get_rect(Bl,r);
}


/**
 * gridblock_resize: 
 * @B: a gridblock 
 * @size: new width and height of the gridblock given in an array of double.
 * 
 * Resize the gridblock using the value of vector @size. 
 *
 **/

void gridblock_resize(NspGridBlock *B,const double size[2])
{
  NspBlock *Bl = (NspBlock *) B;
  GR_INT(Bl->type->interface)->resize(Bl,size);
  /* we want here to update the position of the frame inside its parent frame */
  memcpy(B->obj->r,Bl->obj->r,4*sizeof(double));
}


/**
 * block_move_control:
 * @F: a graphic frame 
 * @B: a block 
 * mpt: point coordinates 
 * @ct: a control point id
 * @ptc: point coordinates 
 * 
 * Updates the block structure when a control point (there's just one control point 
 * for blocks at down right corner) is moved.
 * @mpt gives the mouse position where the control point is to be moved.translation vector which is to be applied to the control point.
 **/

void gridblock_move_control(NspGFrame *F, NspGridBlock *B,const double mpt[2], int cp,double ptc[2])
{
  ptc[0]  =  Max(  mpt[0] - B->obj->r[0] ,0);
  ptc[1]  =  Max(  B->obj->r[1] -mpt[1] ,0);
  gridblock_resize(B,ptc);
  /* return a copy of mpt */
  ptc[0]=mpt[0];
  ptc[1]=mpt[1];
}


void gridblock_set_diagram(NspGridBlock *B, NspGFrame *F)
{
  /* ZZZ */

}

