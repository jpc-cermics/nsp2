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


#define  Block_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "nsp/matutil.h"
#include "nsp/graphics/Graphics.h"

#include "nsp/parse.h"

/*
 * NspBlock inherits from NspObject and implements GRint 
 * graphic blocks 
 */

int nsp_type_block_id=0;
NspTypeBlock *nsp_type_block=NULL;

NspTypeBlock *new_type_block(type_mode mode)
{
  NspTypeBlock *type = NULL;
  NspTypeObject *top;
  NspTypeGRint *gri;

  if ( nsp_type_block != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_block;
    }
  if (( type =  malloc(sizeof(NspTypeBlock))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  block_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  block_get_methods; 
  type->new = (new_func *) new_block;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for block */ 

  top->pr = (print_func *) block_print;                    
  top->dealloc = (dealloc_func *) block_destroy;            
  top->copy  =  (copy_func *) block_copy;                   
  top->size  = (size_func *) block_size;                  
  top->s_type =  (s_type_func *) block_type_as_string;    
  top->sh_type = (sh_type_func *) block_type_short_string;
  top->info = (info_func *) block_info;                    
  /* top->is_true = (is_true_func  *) BlockIsTrue;           */
  /* top->loop =(loop_func *) block_loop; */
  top->path_extract =    (path_func *)  object_path_extract ;       
  top->get_from_obj = (get_from_obj_func *) block_object  ;  
  top->eq  = (eq_func *) block_eq;
  top->neq  = (eq_func *) block_neq;
  top->save  = (save_func *) block_xdr_save;
  top->load  = (load_func *) block_xdr_load;
  top->create = (create_func*) int_block_create;

  /* specific methods for block */

  type->init =  (init_func *) init_block;

  /* Block implement grint interface */

  gri = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) gri;
  
  gri->get_hilited 	=(gr_get_hilited *) block_get_hilited;
  gri->set_hilited 	=(gr_set_hilited *) block_set_hilited;
  gri->get_show    	=(gr_get_show *) block_get_show;
  gri->set_show		=(gr_set_show *) block_set_show;
  gri->draw    		=(gr_draw *) block_draw;
  gri->translate 	=(gr_translate *) block_translate;
  gri->resize 		=(gr_resize *) block_resize;
  gri->update_locks 	=(gr_update_locks *) block_update_locks;
  gri->contains_pt 	=(gr_contains_pt *) block_contains_pt;
  gri->control_near_pt 	=(gr_control_near_pt *) block_control_near_pt;
  gri->lock_near_pt 	=(gr_lock_near_pt *) block_lock_near_pt;
  gri->move_control_init=(gr_move_control_init *) block_move_control_init;
  gri->move_control 	=(gr_move_control *) block_move_control;

  gri->get_number_of_locks =(gr_get_number_of_locks *) block_get_number_of_locks;
  gri->get_number_of_ports =(gr_get_number_of_ports *) block_get_number_of_ports;
  gri->get_lock_connection =(gr_get_lock_connection *) block_get_lock_connection;
  gri->get_lock_pos =(gr_get_lock_pos *) block_get_lock_pos;
  gri->set_lock_connection =(gr_set_lock_connection *) block_set_lock_connection;
  gri->unset_lock_connection =(gr_unset_lock_connection *) block_unset_lock_connection;
  gri->is_lock_connectable =(gr_is_lock_connectable *) block_is_lock_connectable;
  gri->is_lock_connected =(gr_is_lock_connected *) block_is_lock_connected;
  gri->set_lock_pos =(gr_set_lock_pos *) block_set_lock_pos;
  
  if ( nsp_type_block_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_block
       */
      type->id =  nsp_type_block_id = nsp_new_type_id();
      nsp_type_block = type;
      if ( nsp_register_type(nsp_type_block) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_block(mode);
    }
  else 
    {
      type->id = nsp_type_block_id;
      return type;
    }
}

/*
 * initialize Block instances 
 * locally and by calling initializer on parent class 
 */

static int init_block(NspBlock *o,NspTypeBlock *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Block 
 */

NspBlock *new_block() 
{
  NspBlock *loc; 
  /* type must exists */
  nsp_type_block = new_type_block(T_BASE);
  if ( (loc = malloc(sizeof(NspBlock)))== NULLBLOCK) return loc;
  /* initialize object */
  if ( init_block(loc,nsp_type_block) == FAIL) return NULLBLOCK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for Block 
 *-----------------------------------------------*/

 /*
  * size 
  */

static int block_size(NspBlock *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char block_type_name[]="Block";
static char block_short_type_name[]="gbl";

static char *block_type_as_string(void)
{
  return(block_type_name);
}

static char *block_type_short_string(void)
{
  return(block_short_type_name);
}

/** used in for x=y where y is a Block **/

int BlockFullComp(NspBlock * A,NspBlock * B,char *op,int *err)
{
  Scierror("BlockFullComp: to be implemented \n");
  return FALSE;
}

static int block_eq(NspBlock *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_block_id) == FALSE) return FALSE ;
  rep = BlockFullComp(A,(NspBlock *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int block_neq(NspBlock *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_block_id) == FALSE) return TRUE;
  rep = BlockFullComp(A,(NspBlock *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int block_xdr_save(XDR  *xdrs, NspBlock *M)
{
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("block_xdr_save: to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspBlock  *block_xdr_load(XDR  *xdrs)
{
  NspBlock *M=NULLBLOCK;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLBLOCK;
  Scierror("block_xdr_load: to be implemented \n");
  return M;
}

static void block_destroy(NspBlock *H)
{
  FREE(NSP_OBJECT(H)->name);
  H->obj->ref_count--;
  if ( H->obj->ref_count == 0 )
   {
     FREE(H->obj->locks);
     FREE(H->obj);
   }
  FREE(H);
}

static void block_info(NspBlock *H, int indent)
{
  int i;
  if ( H == NULLBLOCK) 
    {
      Sciprintf("Null Pointer Block \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t = Block r=[%5.2f,%5.2f,%5.2f,%5.2f] co=%d, th=%d bg=%d\n",
	    NSP_OBJECT(H)->name,H->obj->r[0],H->obj->r[1],H->obj->r[2],H->obj->r[3],
	    H->obj->color,H->obj->thickness,H->obj->background);
}

static void block_print(NspBlock *H, int indent)
{
  block_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspBlock *block_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_block_id) == TRUE) return ((NspBlock *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_block));
  return(NULL);
}

int IsBlockObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_block_id);
}

int IsBlock(NspObject *O)
{
  return nsp_object_type(O , nsp_type_block_id);
}

NspBlock  *GetBlockCopy(Stack stack, int i)
{
  if (  GetBlock(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspBlock  *GetBlock(Stack stack, int i)
{
  NspBlock *M;
  if (( M = block_object(NthObj(i))) == NULLBLOCK)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

static NspBlock *block_create_void(char *name,NspTypeBase *type)
{
 NspBlock *H  = (type == NULL) ? new_block() : type->new();
 if ( H ==  NULLBLOCK)
  {
   Sciprintf("No more memory\n");
   return NULLBLOCK;
  }
 if ( ( NSP_OBJECT(H)->name =new_nsp_string(name)) == NULLSTRING) return NULLBLOCK;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->obj = NULL;
 return H;
}

NspBlock *block_create(char *name,double rect[],int color,int thickness,int background,
		       NspTypeBase *type )
{
  int i;
  NspBlock *H  = block_create_void(name,type);
  if ( H ==  NULLBLOCK) return NULLBLOCK;
  if ((H->obj = malloc(sizeof(nsp_block))) == NULL) return NULL;
  H->obj->ref_count=1;
  H->obj->frame = NULL; 
  /* fields */
  for ( i=0; i < 4 ; i++) H->obj->r[i]=rect[i];
  H->obj->color = color;
  H->obj->thickness = thickness;
  H->obj->background = background;
  H->obj->hilited = FALSE ; 
  H->obj->show = TRUE   ; 
  H->obj->n_locks = 4 ; /* a changer  XXXXX */
  if (( H->obj->locks = malloc(H->obj->n_locks*sizeof(grb_lock))) == NULL ) return NULLBLOCK;
  for (i=0; i < H->obj->n_locks ; i++) 
    H->obj->locks[i].port.object_id = NULL; 
  block_update_locks(H);
  return H;
}

/*
 * copy for gobject derived class  
 */

NspBlock *block_copy(NspBlock *self)
{
  NspBlock *H  =block_create_void(NVOID,(NspTypeBase *) nsp_type_block);
  if ( H ==  NULLBLOCK) return NULLBLOCK;
  H->obj = self->obj;
  self->obj->ref_count++;
  return H;
}

/*-------------------------------------------------------------------
 * constructor at nsp level 
 * %types.Block(...) or %types.Block.new[]
 *-------------------------------------------------------------------*/

static int get_rect(Stack stack, int rhs, int opt, int lhs,double **val);

static int int_block_create(Stack stack, int rhs, int opt, int lhs)
{
  NspBlock *H;
  double *val;
  int back=-1,color=-1,thickness=-1;

  nsp_option opts[] ={{ "background",s_int,NULLOBJ,-1},
		      { "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ( get_rect(stack,rhs,opt,lhs,&val)==FAIL) return RET_BUG;
  if ( get_optional_args(stack,rhs,opt,opts,&back,&color,&thickness) == FAIL) return RET_BUG;
  if(( H = block_create(NVOID,val,color,thickness,back,NULL)) == NULLBLOCK) return RET_BUG;
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
      CheckLength(stack.fname,1,M1,4);
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
      Scierror("%s: wrong number of rhs argumens (%d), rhs must be 1 or 4\r\n",stack.fname,rhs-opt);
      return FAIL;
    }
  return OK;
}

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_gblock_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspBlock *) Hv)->obj->color);
}

static int int_gblock_set_color(void *Hv, char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspBlock *)Hv)->obj->color = color;
  return OK ;
}

static NspObject * int_gblock_get_thickness(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspBlock *) Hv)->obj->thickness);
}
                                                                                                      
static int int_gblock_set_thickness(void *Hv, char *attr, NspObject *O)
{
  int thickness;
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspBlock *)Hv)->obj->thickness = thickness;
  return OK ;
}

static NspObject * int_gblock_get_background(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspBlock *) Hv)->obj->background);
}
                                                                                                      
static int int_gblock_set_background(void *Hv, char *attr, NspObject *O)
{
  int background;
  if (  IntScalar(O,&background) == FAIL) return FAIL;
  ((NspBlock *)Hv)->obj->background = background;
  return OK ;
}

static NspObject * int_gblock_get_hilited(void *Hv,char *attr)
{
  return nsp_new_boolean_obj(((NspBlock *) Hv)->obj->hilited);
}
                                                                                                      
static int int_gblock_set_hilited(void *Hv, char *attr, NspObject *O)
{
  int hilited;
  if (  BoolScalar(O,&hilited) == FAIL) return FAIL;
  ((NspBlock *)Hv)->obj->hilited = hilited;
  return OK ;
}

static NspObject * int_gblock_get_show(void *Hv,char *attr)
{
  return nsp_new_boolean_obj(((NspBlock *) Hv)->obj->show);
}
                                                                                                      
static int int_gblock_set_show(void *Hv, char *attr, NspObject *O)
{
  int show;
  if (  BoolScalar(O,&show) == FAIL) return FAIL;
  ((NspBlock *)Hv)->obj->show = show;
  return OK ;
}

static AttrTab block_attrs[] = {
  { "color",        int_gblock_get_color ,     int_gblock_set_color ,     NULL },
  { "background",    int_gblock_get_background,  int_gblock_set_background,  NULL },
  { "thickness",    int_gblock_get_thickness,  int_gblock_set_thickness,  NULL },
  { "hilited",   int_gblock_get_hilited, int_gblock_set_hilited, NULL },
  { "show",   int_gblock_get_show, int_gblock_set_show, NULL },
  { (char *) 0, NULL}
};

/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/

/* draw */

static int int_gblock_draw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  block_draw(self);
  return 0;
}

/* translate */

static int int_gblock_translate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,M,2);
  block_translate(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

/* resize */ 

static int int_gblock_resize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,M,2);
  block_resize(self,M->R);
  MoveObj(stack,1,self);
  return 1;
}

static NspMethods block_methods[] = {
  { "translate", int_gblock_translate},
  { "resize",   int_gblock_resize},
  { "draw",   int_gblock_draw},
  { (char *) 0, NULL}
};

static NspMethods *block_get_methods(void) { return block_methods;};

/*------------------------------------------------------
 *  Interface 
 *---------------------------------------------------*/

static OpTab Block_func[]={
  {"setrowscols_gbl",int_set_attributes}, 
  {(char *) 0, NULL}
};

/** call ith function in the Block interface **/

int Block_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Block_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Block_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Block_func[i].name;
  *f = Block_func[i].fonc;
}


/*
 * implementation of the GRint interface 
 * for a block 
 */ 

/**
 * block_get_hilited:
 * @B: a block 
 *
 * Returns the value of the hilited attribute of object @B.
 *
 **/

int block_get_hilited(NspBlock *B) {  return B->obj->hilited; } 

/**
 * block_set_hilited:
 * @B: a block 
 * @val: %True or %False. 
 * 
 * Sets the hilited status of the block @B.
 *
 **/

void block_set_hilited(NspBlock *B,int val) {  B->obj->hilited = val; } 

/**
 * block_get_show:
 * @B: a block 
 * @val:  %True or %False. 
 * 
 * Returns the value of the show attribute of object @B.
 *
 **/

int block_get_show(NspBlock *B) {  return B->obj->show; } 

/**
 * block_set_show:
 * @B: a block 
 *
 * Sets the show status of the given Block.
 *
 **/

void block_set_show(NspBlock *B,int val) {  B->obj->show = val; } 

/**
 * block_draw:
 * @B: a block 
 *
 * Draws a block given the current graphic driver. 
 *
 **/

static int lock_size=2; /*  XXX a factoriser quelque part ... */ 
static int lock_color=10;

void block_draw(NspBlock *B)
{
  BCG *Xgc;
  char str[256];
  double loc[4];
  int cpat, cwidth,i, draw_script ; 
  /* only draw block which are in a frame */
  if ( B->obj->frame == NULL) return;
  /* check the show attribute */
  if ( B->obj->show == FALSE ) return ;

  Xgc=B->obj->frame->Xgc;
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);

  /* first draw inside */
  /* just a test we draw a Matrix inside the block */
  draw_script = 0;
  switch (draw_script)
    {
    case 0: 
      sprintf(str,"Matplot1(rand(10,10)*32,[%5.2f,%5.2f,%5.2f,%5.2f]);",B->obj->r[0],B->obj->r[1]-B->obj->r[3],
	      B->obj->r[0]+B->obj->r[2],B->obj->r[1]);
      nsp_parse_eval_from_string(str,FALSE,FALSE,FALSE,TRUE);
      break;
    case 1: 
      sprintf(str,"draw_inside([%5.2f,%5.2f,%5.2f,%5.2f]);",B->obj->r[0],B->obj->r[1],B->obj->r[2],B->obj->r[3]);
      nsp_parse_eval_from_string(str,FALSE,FALSE,FALSE,TRUE);
      break;
    default: 
      /* fill rectangle */
      Xgc->graphic_engine->xset_pattern(Xgc,B->obj->background);
      Xgc->graphic_engine->scale->fillrectangle(Xgc,B->obj->r);
      break;
    }
  /* draw rectangle */
  Xgc->graphic_engine->xset_pattern(Xgc,B->obj->color);
  Xgc->graphic_engine->scale->drawrectangle(Xgc,B->obj->r);
  /* add hilited */ 
  Xgc->graphic_engine->xset_pattern(Xgc,lock_color);
  if ( B->obj->hilited == TRUE ) 
    {
      loc[0]=B->obj->r[0]; loc[1]=B->obj->r[1];loc[2]=loc[3]= lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
      loc[0]+= B->obj->r[2] -2; loc[1] -= B->obj->r[3] -2;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
    }
  for ( i=0 ; i < B->obj->n_locks  ; i++ ) 
    {
      if ( block_is_lock_connected(B,i)== TRUE)
	Xgc->graphic_engine->xset_pattern(Xgc,lock_color); 
      else 
	Xgc->graphic_engine->xset_pattern(Xgc,1); 
      block_get_lock_pos(B,i,loc);
      loc[0] += -1; loc[1] += 1;loc[2]=loc[3]= lock_size;
      Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
    }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}

/**
 * block_tranlate:
 * @B: a block 
 * @tr: translation vector 
 * 
 * Tranlates the block origin (upper left point) using the 
 * value of vector @tr. 
 *
 **/

void block_translate(NspBlock *B,const double tr[2])
{
  B->obj->r[0] += tr[0] ;
  B->obj->r[1] += tr[1] ;
  block_update_locks(B);
}

/**
 * block_resize: 
 * @B: a block 
 * @size: new width and height of the block given in an array of double.
 * 
 * Resize the block using the value of vector @size. 
 *
 **/

void block_resize(NspBlock *B,const double size[2])
{
  B->obj->r[2] = size[0] ;
  B->obj->r[3] = size[1] ;
  block_update_locks(B);
}


/**
 * block_update_locks:
 * @B: a block 
 * 
 * Recomputes the positions of the locks point of the block.
 *
 **/

void block_update_locks(NspBlock *B)
{
  B->obj->locks[0].pt[0]=B->obj->r[0]+B->obj->r[2]/2; 
  B->obj->locks[0].pt[1]=B->obj->r[1];
  B->obj->locks[1].pt[0]=B->obj->r[0]+B->obj->r[2]/2; 
  B->obj->locks[1].pt[1]=B->obj->r[1]-B->obj->r[3];
  B->obj->locks[2].pt[0]=B->obj->r[0];        
  B->obj->locks[2].pt[1]=B->obj->r[1]-B->obj->r[3]/2;
  B->obj->locks[3].pt[0]=B->obj->r[0]+B->obj->r[2];   
  B->obj->locks[3].pt[1]=B->obj->r[1]-B->obj->r[3]/2;
}

/**
 * block_contains_pt
 * @B: a block 
 * @pt: a point position 
 * 
 * Checks if the given point in inside the block enclosing rectangle.
 * 
 * Return value: %True or %False.
 **/

int block_contains_pt(const NspBlock *B,const double pt[2])
{
  return B->obj->r[0] <= pt[0] && B->obj->r[1] >= pt[1] && B->obj->r[0]+B->obj->r[2] >= pt[0] && B->obj->r[1]- B->obj->r[3] <= pt[1];
}

/**
 * block_control_near_pt:
 * @B: a block 
 * @pt: a point position 
 * @cp: the control point id in case of success.
 * 
 * Checks if the given point is near a block control point. 
 * 
 * Return value: %True or %False.
 **/

int block_control_near_pt(const NspBlock *B,const double pt[2], int *cp)
{
  double d = Max(Abs( B->obj->r[0]+B->obj->r[2] -pt[0]),Abs( B->obj->r[1]-B->obj->r[3] -pt[1])) ;
  if ( d < lock_size ) 
    { 
      *cp = 0 ;
      return TRUE;
    }
  return FALSE;
}

/**
 * block_lock_near_pt:
 * @B: a block 
 * @pt: a point position 
 * @cp: the control point id in case of success.
 * 
 * Checks if the given point is near a block lock point. 
 * If %True the given point is changed so as to contains 
 * the lock point coordinates and @cp is filled with the control point id. 
 * 
 * Return value: %True or %False.
 **/

int block_lock_near_pt(const NspBlock *B,double pt[2], int *cp)
{
  int i;
  for ( i=0 ; i < B->obj->n_locks ; i++ ) 
    {
      double d= Max(Abs( B->obj->locks[i].pt[0] -pt[0]),Abs( B->obj->locks[i].pt[1] -pt[1])) ;
      if ( d < lock_size ) 
	{ 
	  *cp = i;
	  pt[0]=B->obj->locks[i].pt[0];
	  pt[1]=B->obj->locks[i].pt[1];
	  return TRUE;
	}
    }
  return FALSE;
}

/**
 * block_move_control_init:
 * @B: a block 
 * @ct: a control point id
 * @pts: point coordinates 
 * 
 * Used to init a control point interactive move. 
 * This function is empty for Blocks. 
 **/

void block_move_control_init( NspBlock *B,int cp,double ptc[2])
{
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
 * for blocks atdown right corner) is moved.
 * @mpt gives the mouse position where the control point is to be moved.translation vector which is to be applied to the control point.
 **/

void block_move_control(NspGFrame *F, NspBlock *B,const double mpt[2], int cp,double ptc[2])
{
  B->obj->r[2] =  Max(  mpt[0] - B->obj->r[0] ,0);
  B->obj->r[3] =  Max(  B->obj->r[1] -mpt[1] ,0);
  ptc[0]=mpt[0];
  ptc[1]=mpt[1];
  block_update_locks(B);
}

/**
 * block_get_number_of_locks: 
 * @B: a block 
 * 
 * Returns the number of lock points of the block 
 * 
 * Return value: the number of lock points
 **/

int block_get_number_of_locks(const NspBlock *B) 
{
  return B->obj->n_locks;
}

/**
 * block_get_number_of_ports: 
 * @B: a block 
 * @lp: a lock point 
 * 
 * Returns the number of ports of lock points lp;
 * 
 * Return value: the number of ports
 **/

int block_get_number_of_ports(const NspBlock *B,int lp) 
{
  return 1;
}

/**
 * block_get_lock_connection: 
 * @B: a block 
 * @i: a lock point id. 
 * @port: a port of the lock point @i;
 * 
 * Returns in a gr_port structure information about the object 
 * connected to the port @port of lock point @i. 
 * 
 * Return value: #TRUE if lock point and port number exists or #FALSE. 
 **/

int block_get_lock_connection(const NspBlock *B,int i,int port, gr_port *p )
{
  if ( i >= 0 && i < B->obj->n_locks && port == 0  ) 
    {
      gr_port *gport= &B->obj->locks[i].port; 
      p->object_id = gport->object_id;
      p->lock = gport->lock;
      p->port = gport->port;
      return OK;
    }
  return FAIL;
}

/**
 * block_get_lock_pos:
 * @B: a block 
 * @i: a lock point id. 
 * @pt: point coordinates.
 *
 * Returns in @pt the position of lock point @i. 
 **/

void block_get_lock_pos(const NspBlock *B, int i,double pt[])
{
  if ( i >=  0 && i < B->obj->n_locks )
    {
      pt[0]= B->obj->locks[i].pt[0];
      pt[1]= B->obj->locks[i].pt[1];
    }
}

/**
 * block_set_lock_connection: 
 * @B: a block 
 * @i: a lock point id. 
 * @p: information to be connected to one port of lock point i;
 * 
 * the port described by @p is connected to a port of lock point i; 
 * return value: -1 or the port number used for connection.
 **/

int block_set_lock_connection(NspBlock *B,int i,const gr_port *p)
{
  if ( i >=  0  && i < B->obj->n_locks ) 
    {
      gr_port *port= &B->obj->locks[i].port;
      if ( port->object_id != NULL) return FALSE; 
      port->object_id = p->object_id;
      port->lock = p->lock;
      port->port= p->port; 
      return 0;
    }
  return -1;
}

/**
 * block_unset_lock_connection: 
 * @B: a block 
 * @i: a lock point id. 
 * @prt: a lock point port. 
 * 
 * unconect the object locked to port @port of lock point @i.
 * 
 **/

void block_unset_lock_connection(NspBlock *B,int i,int port)
{
  if ( i >= 0 && i < B->obj->n_locks && port == 0)
    {
      B->obj->locks[i].port.object_id = NULL;
    }
}

/**
 * block_is_lock_connectable: 
 * @B: a block 
 * @i: a lock point id. 
 * 
 * Checks if there's a free port in lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int block_is_lock_connectable(NspBlock *B,int i)
{
  if ( i >=  0 && i < B->obj->n_locks )
    {
      if ( B->obj->locks[i].port.object_id == NULL) return TRUE; 
    }
  return FALSE;
}

/**
 * block_is_lock_connected 
 * @B: a block 
 * @i: a lock point id. 
 * 
 * Checks if there's a locked port for lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int block_is_lock_connected(NspBlock *B,int i)
{
  if ( i >=  0  && i < B->obj->n_locks ) 
    {
      if ( B->obj->locks[i].port.object_id != NULL) return TRUE; 
    }
  return FALSE;
}

/**
 * block_set_lock_pos: 
 * @B: a block 
 * @i: a lock point id. 
 * @pt: a point coordinates 
 * 
 * Sets the lock point @i poistion to @pt. 
 **/

void block_set_lock_pos(NspBlock *B, int i,const double pt[])
{
  if ( i >= 0 && i < B->obj->n_locks )
    {
      B->obj->locks[i].pt[0] = pt[0];
      B->obj->locks[i].pt[1] = pt[1];
    }
}








