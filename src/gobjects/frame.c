/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998 )                          
 * Jean-Philippe Chancelier Enpc/Cergrene                            
 *-------------------------------------------------------------------*/

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
  return nsp_list_length(o->objs);
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

static char *gframe_type_short_string(void)
{
  return(gframe_short_type_name);
}

static NspObject *gframe_path_extract(NspGFrame *H, NspObject *O)
{
  int i;
  if ( IntScalar(O,&i) == FAIL) return NULLOBJ ;
  if ( i >= 1 && i <=nsp_list_length(H->objs))
    return nsp_list_get_element(H->objs,i);
  else
    return NULLOBJ; 
}

int GFrameFullComp(NspGFrame * A,NspGFrame * B,char *op,int *err)
{
  Scierror("GFrameFullComp: to be implemented \n");
  return FALSE;
}

static int gframe_eq(NspGFrame *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_gframe_id) == FALSE) return FALSE ;
  rep = GFrameFullComp(A,(NspGFrame *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int gframe_neq(NspGFrame *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_gframe_id) == FALSE) return TRUE;
  rep = GFrameFullComp(A,(NspGFrame *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int gframe_xdr_save(NspFile  *F, NspGFrame *M)
{
  if ( XdrSaveI(F,M->type->id) == FAIL) return FAIL;
  if ( XdrSaveString(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  Scierror("gframe_xdr_save  to be implemented \n");
  return OK;
}

/*
 * load 
 */

static NspGFrame  *gframe_xdr_load(NspFile  *F)
{
  NspGFrame *M;
  static char name[NAME_MAXL];
  if ( XdrLoadString(F,name,NAME_MAXL) == FAIL) return NULLGFRAME;
  Scierror("gframe_xdr_load  to be implemented \n");
  return M;
}

static void gframe_destroy(NspGFrame *H)
{
  FREE(NSP_OBJECT(H)->name);
 nsp_list_destroy(H->objs);
  FREE(H);
}

static void gframe_info(NspGFrame *H, int indent)
{
  int i;
  if ( H == NULLGFRAME) 
    {
      Sciprintf("Null Pointer GFrame \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("[GFrame %s, scale=[%5.2f,%5.2f,%5.2f,%5.2f] r=[%5.2f,%5.2f,%5.2f,%5.2f]\n",
	    NSP_OBJECT(H)->name,H->scale[0],H->scale[1],H->scale[2],H->scale[3],
	    H->scale[0],H->scale[1],H->scale[2],H->scale[3]);
  /* Faire un Map sur les elts XXXXX */
}

static void gframe_print(NspGFrame *H, int indent)
{
  gframe_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGFrame   *gframe_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
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

NspGFrame *gframe_create(char *name,BCG *Xgc,const double scale[],double r[],NspTypeBase *type)
{
  int i;
  NspGFrame *H = (type == NULL) ? new_gframe(): type->new();
  if ( H == NULLGFRAME)
    {
      Sciprintf("No more memory\n");
      return NULLGFRAME;
    }
  if ((NSP_OBJECT(H)->name = NewString(name))== NULLSTRING) return NULLGFRAME;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  for ( i=0; i < 4 ; i++) H->r[i]=r[i];
  for ( i=0; i < 4 ; i++) H->scale[i]=scale[i];
  if ( ( H->objs =nsp_list_create(NVOID,NULL))== NULLLIST) return NULLGFRAME;
  H->Xgc = Xgc;
  return H;
}

NspGFrame *gframe_copy(NspGFrame *H)
{
  NspGFrame *loc= gframe_create(NVOID,H->Xgc,H->scale,H->r,NULL);
  if ( loc == NULLGFRAME ) return  NULLGFRAME;
  loc->objs =nsp_list_copy(H->objs);
  if ( loc->objs == NULLLIST) return NULLGFRAME;
  return loc;
}

/*-------------------------------------------------------------------
 * wrappers for the ClassA
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/


static int int_gframe_create(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  int i;
  NspGFrame *H;
  NspMatrix *scale,*r;

  CheckRhs(2,1000);

  if ( opt != 0 ) 
    { 
      Scierror("Error: optional arguments are not expected\n");
      return RET_BUG;
    }
  Xgc= check_graphic_window();
  if ((scale =GetRealMat(stack,1)) == NULLMAT ) return FAIL;
  CheckLength(stack.fname,1,scale,4);
  if ((r =GetRealMat(stack,2)) == NULLMAT ) return FAIL;
  CheckLength(stack.fname,2,r,4);
  
  if(( H = gframe_create(NVOID,Xgc,scale->R,r->R,NULL)) == NULLGFRAME) return RET_BUG;
  for ( i = 3 ; i <= rhs ; i++ )
    {
      if ( MaybeObjCopy(&NthObj(i)) == NULL)  return RET_BUG;
      if (nsp_object_set_name(NthObj(i),"lel") == FAIL) return RET_BUG;
      if (nsp_list_end_insert(H->objs,NthObj(i)) == FAIL ) return RET_BUG;
      /** If NthObj(i) is not copied it is inserted in the list 
      we must set then NthObj(i) to NULLOBJ 
      to prevent the cleaning process to clean the object 
      that we have inserted in our list **/
      NthObj(i) = NULLOBJ ;
    }
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

#ifdef NOK
static NspObject * int_gframe_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspGFrame *) Hv)->color);
}

static int int_gframe_set_color(void *Hv, char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspGFrame *)Hv)->color = color;
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


static AttrTab gframe_attrs[] = {
#ifdef NOK
  { "color",        int_gframe_get_color ,     int_gframe_set_color ,     NULL },
  { "background",    int_gframe_get_background,  int_gframe_set_background,  NULL },
  { "thickness",    int_gframe_get_thickness,  int_gframe_set_thickness,  NULL },
#endif
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
  CheckLength(stack.fname,1,pt,2);
  gframe_select_and_move(((NspGFrame *) self),pt->R);
  return 0;
}

/* split link */

int int_gf_select_and_split(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,0);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,pt,2);
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
  CheckLength(stack.fname,1,pt,2);
  gframe_select_link_and_add_control(((NspGFrame *) self),pt->R);
  return 0;
}


int int_gf_hilite_near_pt(void *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *pt;
  CheckRhs(1,1);
  CheckLhs(-1,1);
  if ((pt = GetRealMat(stack,1)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,1,pt,2);
  gframe_hilite_near_pt(((NspGFrame *) self),pt->R);
  return 0;
}

int int_gf_new_block(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  gframe_create_new_block(((NspGFrame *) self));
  return 0;
}

int int_gf_new_connector(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  gframe_create_new_connector(((NspGFrame *) self));
  return 0;
}

int int_gf_new_link(void *self,Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(0,0);
  CheckLhs(-1,1);
  gframe_create_new_link(((NspGFrame *) self));
  return 0;
}

static NspMethods gframe_methods[] = {
  { "draw",   int_gfdraw},
  { "new_link", int_gf_new_link },
  { "new_block", int_gf_new_block },
  { "new_connector", int_gf_new_connector },
  { "hilite_near_pt", int_gf_hilite_near_pt },
  { "select_and_move", int_gf_select_and_move},
  { "select_and_split", int_gf_select_and_split},
  { "select_link_and_add_control", int_gf_select_link_and_add_control},
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

/** call ith function in the GFrame interface **/

int GFrame_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(GFrame_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void GFrame_Interf_Info(int i, char **fname, function (**f))
{
  *fname = GFrame_func[i].name;
  *f = GFrame_func[i].fonc;
}

/*********************************************************************
 * GFrame Object in Scilab : a graphic GFrameangle 
 *********************************************************************/

static int pixmap = TRUE ; /* XXXXX */

/**************************************************
 * Draw 
 **************************************************/

void gframe_draw(NspGFrame *R)
{
  BCG *Xgc;
  Cell *C = R->objs->first;
  Xgc=check_graphic_window();
  /* using current values */
  Nsetscale2d(Xgc,NULL,NULL,R->scale,"nn");
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


/**************************************************
 * frame_select_obj 
 **************************************************/

int gframe_select_obj(NspGFrame *R,const double pt[2], NspObject **O, NspObject *exclude) 
{
  int count = 1;
  Cell *C = R->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ && C->O != exclude )
	{
	  /* cast to a BlockFType */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->contains_pt(C->O,pt) ) 
	    {
	      *O = C->O;
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
 * @lp: lock point id 
 * @lock_c: is lock connectable.
 *
 * If @pt is close enough to an object lock point, then the object is 
 * returned in @O, the lock point id in @lp and the connectable status in @lock_c.
 *  
 * return value: a non null integer in case of success
 **/


int gframe_select_lock(NspGFrame *F,double pt[2], NspObject **O, int *cp, int *lock_c) 
{
  int count = 1;
  Cell *C = F->objs->first;
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

/**************************************************
 * select_and_move
 **************************************************/

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
	      if ( bf->get_lock_connection(O,i,j,&p)== OK) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  bf1->set_show(O1,val);
		}
	    }
	}
    }
}

int gframe_select_and_move(NspGFrame *R,const double pt[2])
{
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
  /* global draw to hide current object */
  gframe_draw(R);
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
 * XXXX split a link 
 */

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
 * faut il en profiter pour hiliter ici ? 
 * ou faire appel avant a une autre fonction ? 
 */

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

/**************************************************
 * hilite nearest object 
 **************************************************/

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




/**************************************************
 * move obj 
 **************************************************/

static void gframe_locks_draw(NspGFrame *R,NspObject *O)
{
  NspTypeGRint *bf = GR_INT(O->basetype->interface);
  int   n = bf->get_number_of_locks(O), i;
  Scierror("XXX gframe_locks_draw n_locks = %d\n",n);
  for ( i = 0 ; i < n ; i++) 
    {
      if ( bf->is_lock_connected(O,i) == TRUE) 
	{
	  int np = bf->get_number_of_ports(O,i);
	  int j;
	  Scierror("XXX gframe_locks_draw lock= %d ports=%d\n",i,np);
	  for ( j= 0 ; j < np ; j++) 
	    {
	      gr_port p;
	      if ( bf->get_lock_connection(O,i,j,&p)== OK) 
		{
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  Scierror("XXX gframe_locks_draw lock= %d port=%d is connected 0x%lx\n",i,j,(long) O1);
		  bf1->draw(O1);
		}
	    }
	}
    }
}


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
	      if ( bf->get_lock_connection(O,i,j,&p)== OK) 
		{
		  
		  NspObject *O1 = p.object_id; 
		  NspTypeGRint *bf1 = GR_INT(O1->basetype->interface);
		  double pt[2];
		  bf->get_lock_pos(O,i,pt);
		  bf1->set_lock_pos(O1,p.lock,pt);
		}
	    }
	}
    }

}



int gframe_move_obj(NspGFrame *F,NspObject *O,const double pt[2],int stop,int cp,move_action action)
{
  int record;
  BCG *Xgc = F->Xgc;
  int alumode = Xgc->graphic_engine->xget_alufunction(Xgc), wstop = 0, ibutton, iwait=FALSE;
  double mpt[2],pt1[2]= {pt[0],pt[1]},ptwork[2];

  NspTypeGRint *bf = GR_INT(O->basetype->interface);

  record = Xgc->graphic_engine->xget_recording(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
  Xgc->graphic_engine->xset_alufunction1(Xgc,6);
  
  while ( wstop==0 ) 
    {
      bf->draw(O);
      if ( IsBlock(O)  || IsConnector(O))  gframe_locks_draw(F,O);
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
      bf->draw(O);
      if ( IsBlock(O)|| IsConnector(O) ) gframe_locks_draw(F,O);
      /* if ( pixmap ) Xgc->graphic_engine->xset_show(); */

      /* move object */
      switch ( action ) 
	{
	case MOVE : 
	  bf->translate(O,(pt1[0]= mpt[0] -pt1[0],pt1[1]=mpt[1] -pt1[1],pt1));
	  break;
	case MOVE_CONTROL :
	  bf->move_control(F,O,(pt1[0]= mpt[0] -pt1[0],pt1[1]=mpt[1] -pt1[1],pt1),cp, ptwork);
	}
      /* update locks positions int locked objects  */ 
      gframe_locks_update(F,O);
      pt1[0] = mpt[0];
      pt1[1] = mpt[1];
    }
  if ( IsLink(O)) link_check(F,(NspLink *)O);

  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
  Xgc->graphic_engine->xset_recording(Xgc,record);
  return ibutton;
}



/**************************************************
 * unhilite objects and redraw
 **************************************************/

void gframe_unhilite_objs(NspGFrame *R,int draw )
{
  int ok = FALSE;
  Cell *C = R->objs->first;
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



/**************************************************
 * delete hilited objects 
 **************************************************/

void gframe_delete_hilited(NspGFrame *R) 
{
  Cell *C = R->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O != NULLOBJ )
	{
	  /* grint interface */
	  NspTypeGRint *bf = GR_INT(C->O->basetype->interface);
	  if ( bf->get_hilited(C->O) == TRUE ) 
	    {
	      /* XXX detruire l'object O */
	      C->O = NULLOBJ;
	    }
	}
      C = C->next ;
    }
  /* 
   * here we must compact the list zhich can have holes 
   * but it implies that lock number are to be properly updated 
   * XXXXXXXXXXXXX 
   */
}

/**************************************************
 * using move to position a new block with default size 
 **************************************************/

int gframe_create_new_block(NspGFrame *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,10,10}, pt[]={0,100};
  NspBlock *B;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  B=block_create(NVOID,rect,color,thickness,background,NULL);
  if ( B == NULLBLOCK) return FAIL;
  B->hilited = TRUE;
  if (nsp_list_end_insert(F->objs,(NspObject  *) B) == FAIL) return FAIL;
  rep= gframe_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return FAIL;
  /* XXXX block_draw(B); */
  if ( pixmap ) F->Xgc->graphic_engine->xset_show(F->Xgc);
  return OK;
}

int gframe_create_new_connector(NspGFrame *F)
{
  int color=4,thickness=1, background=9,rep;
  double rect[]={0,100,4,4}, pt[]={0,100};
  NspConnector *B;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  B=connector_create(NVOID,rect,color,thickness,background,NULL);
  if ( B == NULL) return FAIL;
  B->hilited = TRUE;
  if (nsp_list_end_insert(F->objs,(NspObject  *) B) == FAIL) return FAIL;
  rep= gframe_move_obj(F,(NspObject  *) B,pt,-5,0,MOVE);
  if ( rep== -100 )  return FAIL;
  /* XXXX block_draw(B); */
  if ( pixmap ) F->Xgc->graphic_engine->xset_show(F->Xgc);
  return OK;
}


int gframe_create_new_link(NspGFrame *F)
{
  BCG *Xgc= F->Xgc;
  NspObject *Ob;
  int cp1,record;
  double mpt[2],pt[2];
  int alumode = Xgc->graphic_engine->xget_alufunction(Xgc), wstop = 0,stop=2, ibutton, iwait=FALSE;
  int color=4,thickness=1,hvfactor,count=0;
  NspLink *L;
  NspTypeGRint *bf;
  /* unhilite all */
  gframe_unhilite_objs(F,FALSE);
  hvfactor=5;/* magnetism toward horizontal and vertical line  */
  Xgc->graphic_engine->xinfo(Xgc,"Enter polyline, Right click to stop");

  record = Xgc->graphic_engine->xget_recording(Xgc);
  Xgc->graphic_engine->xset_recording(Xgc,FALSE);
  Xgc->graphic_engine->xset_alufunction1(Xgc,6);
  /* prepare a link with 1 points */
  L= LinkCreateN(NVOID,1,color,thickness);
  bf = GR_INT(((NspObject *) L)->basetype->interface);

  if ( L == NULLLINK) return FAIL;
  L->hilited = TRUE;
  L->poly->R[0]=mpt[0];
  L->poly->R[1]=mpt[0];
  while ( wstop==0 ) 
    {
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
	  return ibutton;
	}
      if ( ibutton == stop ) wstop= 1;
      /* clear link shape using redraw */
      bf->draw(L);
      if ( ibutton == stop ) 
	{
	  break; 
	}
      else if ( ibutton == 0 ) 
	{
	  /* click */
	  int lock_c, rep;
	  /* are we near a lock point ? if true mpt is changed  */
	  pt[0]=mpt[0]; pt[1]=mpt[1];
	  rep = gframe_select_lock(F,mpt, &Ob, &cp1,&lock_c) ;
	  /* link_check will check if the lock point is already in use */
	  if ( rep != 0 )
	    {
	      /* set last point to lock position and stop if it's not the first point*/
	      L->poly->R[count]= mpt[0];
	      L->poly->R[count+L->poly->m]= mpt[1];
	      if ( count != 0) break;
	    }
	  if ( nsp_matrix_add_rows(L->poly,1)== FAIL ) return FAIL;	  
	  count ++;
	  L->poly->R[count]= mpt[0];
	  L->poly->R[count+L->poly->m]= mpt[1];
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
	      if ( Abs( L->poly->R[count-1] - mpt[0]) < hvfactor ) mpt[0]=L->poly->R[count-1];
	      if ( Abs( L->poly->R[count-1+L->poly->m] - mpt[1]) < hvfactor ) 
		mpt[1]=L->poly->R[count-1+L->poly->m];
	    }              
	  L->poly->R[count]= mpt[0];
	  L->poly->R[count+L->poly->m]= mpt[1];
	}
    }
  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
  /* insert link in frame */
  if (nsp_list_end_insert(F->objs,(NspObject  *) L) == FAIL) return FAIL;
  /* check if first and last points are locked 
   * if true update locks 
   */
  mpt[0]=L->poly->R[0];
  mpt[1]=L->poly->R[L->poly->m];
  link_lock_update(F,L,0,mpt);
  mpt[0]=L->poly->R[L->poly->m-1];
  mpt[1]=L->poly->R[2*L->poly->m-1];
  link_lock_update(F,L,1,mpt);
  link_check(F,L);
  bf->draw(L);
  if ( pixmap ) Xgc->graphic_engine->xset_show(Xgc);
  Xgc->graphic_engine->xset_alufunction1(Xgc,alumode);
  Xgc->graphic_engine->xset_recording(Xgc,record);
  return ibutton;
}



