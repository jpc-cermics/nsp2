/*-------------------------------------------------------------------
 * This Software is ( Copyright ENPC 1998 )                          
 * Jean-Philippe Chancelier Enpc/Cergrene                            
 *-------------------------------------------------------------------*/

#define  Link_Private 
#include "nsp/object.h"
#include "nsp/pr-output.h" 
#include "nsp/interf.h"
#include "../zcalelm/convert.h"
#include "nsp/grint.h" /* interface definition */
#include "nsp/graphics/Graphics.h"

/* 
 * NspLink inherits from NspObject and implements GRint 
 * graphic links 
 */

int nsp_type_link_id=0;
NspTypeLink *nsp_type_link=NULL;



NspTypeLink *new_type_link(type_mode mode)
{
  NspTypeLink *type = NULL;
  NspTypeObject *top;
  NspTypeGRint *gri;
  if ( nsp_type_link != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_link;
    }
  if ((type =  malloc(sizeof(NspTypeLink))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype =(NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs =  link_attrs ;
  type->get_attrs = (attrs_func *) int_get_attribute; 
  type->set_attrs = (attrs_func *) int_set_attribute; 
  type->methods =  link_get_methods;
  type->new = (new_func *) new_link;

  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for link */ 

  top->pr = (print_func *) link_print;                    
  top->dealloc = (dealloc_func *) link_destroy;            
  top->copy  =  (copy_func *) link_copy;
  top->size  = (size_func *) link_size;                  
  top->s_type =  (s_type_func *) link_type_as_string;    
  top->sh_type = (sh_type_func *) link_type_short_string;
  top->info = (info_func *) link_info;                    
  /* top->is_true = (is_true_func  *) LinkIsTrue;           */
  /* top->loop =(loop_func *) link_loop; */
  top->path_extract =   (path_func *)  object_path_extract ;
  top->get_from_obj = (get_from_obj_func *)  link_object  ;  
  top->eq  = (eq_func *) link_eq;
  top->neq  = (eq_func *) link_neq;
  top->save  = (save_func *) link_xdr_save;
  top->load  = (load_func *) link_xdr_load;
  top->create = (create_func*) int_link_create;
  /* specific methods for link */

  type->init = (init_func *) init_link;


  /* 
   * interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */

  /* Link  implement grint interface */
  gri = new_type_grint(T_DERIVED);
  type->interface = (NspTypeBase *) gri;
  
  gri->get_hilited 	=(gr_get_hilited *) link_get_hilited;
  gri->set_hilited 	=(gr_set_hilited *) link_set_hilited;
  gri->get_show    	=(gr_get_show *) link_get_show;
  gri->set_show		=(gr_set_show *) link_set_show;
  gri->draw    		=(gr_draw *) link_draw;
  gri->translate 	=(gr_translate *) link_translate;
  gri->resize 		=(gr_resize *) link_resize;
  gri->update_locks 	=(gr_update_locks *) link_update_locks;
  gri->contains_pt 	=(gr_contains_pt *) link_contains_pt;
  gri->control_near_pt 	=(gr_control_near_pt *) link_control_near_pt;
  gri->lock_near_pt 	=(gr_lock_near_pt *) link_lock_near_pt;
  gri->move_control_init=(gr_move_control_init *) link_move_control_init;
  gri->move_control 	=(gr_move_control *) link_move_control;

  gri->get_number_of_locks =(gr_get_number_of_locks *) link_get_number_of_locks;
  gri->get_number_of_ports =(gr_get_number_of_ports *) link_get_number_of_ports;
  gri->get_lock_connection =(gr_get_lock_connection *) link_get_lock_connection;
  gri->get_lock_pos =(gr_get_lock_pos *) link_get_lock_pos;
  gri->set_lock_connection =(gr_set_lock_connection *) link_set_lock_connection;
  gri->unset_lock_connection =(gr_unset_lock_connection *) link_unset_lock_connection;
  gri->is_lock_connectable =(gr_is_lock_connectable *) link_is_lock_connectable;
  gri->is_lock_connected =(gr_is_lock_connected *) link_is_lock_connected;
  gri->set_lock_pos =(gr_set_lock_pos *) link_set_lock_pos;
  
  if ( nsp_type_link_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeMatrix called nsp_type_link
       */
      type->id =  nsp_type_link_id = nsp_new_type_id();
      nsp_type_link = type;
      if ( nsp_register_type(nsp_type_link) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_link(mode);
    }
  else 
    {
      type->id = nsp_type_link_id;
      return type;
    }
}
/*
 * initialize Link instances 
 * locally and by calling initializer on parent class 
 */

static int init_link(NspLink *o,NspTypeLink *type)
{
  /* to be done always */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of Link 
 */

NspLink *new_link() 
{
  NspLink *loc; 
  /* type must exists */
  nsp_type_link = new_type_link(T_BASE);
  if ( (loc = malloc(sizeof(NspLink)))== NULLLINK) return loc;
  /* initialize object */
  if ( init_link(loc,nsp_type_link) == FAIL) return NULLLINK;
  return loc;
}


/*----------------------------------------------
 * Object method redefined for Link 
 *-----------------------------------------------*/

 /*
  * size 
  */

static int link_size(NspLink *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char link_type_name[]="Link";
static char link_short_type_name[]="gl";

static char *link_type_as_string(void)
{
  return(link_type_name);
}

static char *link_type_short_string(void)
{
  return(link_short_type_name);
}

/** used in for x=y where y is a Link **/

int LinkFullComp(NspLink * A,NspLink * B,char *op,int *err)
{
  Scierror("LinkFullComp: to be implemented \n");
  return FALSE;
}

static int link_eq(NspLink *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_link_id) == FALSE) return FALSE ;
  rep = LinkFullComp(A,(NspLink *) B,"==",&err);
  if ( err == 1) return FALSE ; 
  return rep;
}

static int link_neq(NspLink *A, NspObject *B)
{
  int err,rep;
  if ( check_cast(B,nsp_type_link_id) == FALSE) return TRUE;
  rep = LinkFullComp(A,(NspLink *) B,"<>",&err);
  if ( err == 1) return TRUE ; 
  return rep;
}

/*
 * save 
 */

static int link_xdr_save(NspFile  *F, NspLink *M)
{
  Scierror("link_xdr_save to be implemented \n");
  if ( XdrSaveI(F,M->type->id) == FAIL) return FAIL;
  if ( XdrSaveString(F, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspLink  *link_xdr_load(NspFile  *F)
{
  NspLink *M;
  static char name[NAME_MAXL];
  Scierror("link_xdr_load to be implemented \n");
  if ( XdrLoadString(F,name,NAME_MAXL) == FAIL) return NULLLINK;
  return M;
}

/*
 * delete 
 */


static void link_destroy(NspLink *H)
{
  FREE(NSP_OBJECT(H)->name);
  nsp_matrix_destroy(H->poly);
  FREE(H);
}

/*
 * info 
 */

static void link_info(NspLink *H, int indent)
{
  int i;
  if ( H == NULLLINK) 
    {
      Sciprintf("Null Pointer Link \n");
      return;
    }
  for ( i=0 ; i < indent ; i++) Sciprintf(" ");
  Sciprintf("%s\t =  Link co=%d, th=%d\n",
	    NSP_OBJECT(H)->name,H->color,H->thickness);
}

static void link_print(NspLink *H, int indent)
{
  link_info(H,indent);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for ClassA objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspLink   *link_object(NspObject *O)
{
  /** Follow pointer **/
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /** Check type **/
  if ( check_cast(O,nsp_type_link_id) == TRUE) return ((NspLink *) O);
  else 
    Scierror("Error:\tArgument should be a %s\n",type_get_name(nsp_type_link));
  return(NULL);
}

int IsLinkObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_link_id);
}

int IsLink(NspObject *O)
{
  return nsp_object_type(O , nsp_type_link_id);
}

NspLink  *GetLinkCopy(Stack stack, int i)
{
  if (  GetLink(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspLink  *GetLink(Stack stack, int i)
{
  NspLink *M;
  if (( M = link_object(NthObj(i))) == NULLLINK)
     ArgMessage(stack,i);
  return M;
}


/*-----------------------------------------------------
 * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassA instance 
 *-----------------------------------------------------*/

NspLink *link_create(char *name,NspMatrix *D,int color,int thickness,
		     NspTypeBase *type)
{
  NspLink *H = (type == NULL) ? new_link() : type->new();
  if ( H == NULLLINK)
    {
      Sciprintf("No more memory\n");
      return NULLLINK;
    }
  if ((NSP_OBJECT(H)->name = NewString(name))== NULLSTRING) return NULLLINK;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 
  if (( H->poly = nsp_matrix_copy(D))== NULLMAT) return NULLLINK;
  H->color = color;
  H->thickness = thickness;
  H->hilited = FALSE ; 
  H->show = TRUE   ; 
  H->locks[0].port.object_id = NULL;
  H->locks[1].port.object_id = NULL;
  return H;
}

NspLink *link_copy(NspLink *H)
{
  return link_create(NVOID,H->poly,H->color,H->thickness,NULL);
}

/*-------------------------------------------------------------------
 * wrappers for the ClassA
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

static int int_link_create(Stack stack, int rhs, int opt, int lhs)
{
  BCG *Xgc;
  NspLink *H;
  NspMatrix *M1;
  int color=-1,thickness=-1;

  nsp_option opts[] ={{ "color",s_int,NULLOBJ,-1},
		      { "thickness",s_int,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};

  CheckRhs(1,7);

  if ((M1=GetRealMat(stack,1)) == NULLMAT ) return FAIL;
  CheckCols(stack.fname,1,M1,2);
  if ( get_optional_args(stack,rhs,opt,opts,&color,&thickness) == FAIL) return RET_BUG;

  Xgc=  check_graphic_window();
  if ( color <= 0 ) color = Xgc->graphic_engine->xget_pattern(Xgc);
  if ( thickness < 0 ) thickness = Xgc->graphic_engine->xget_thickness(Xgc);
  if(( H = link_create(NVOID,M1,color,thickness,NULL)) == NULLLINK) return RET_BUG;
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

/*------------------------------------------------------
 * attributes  (set/get methods) 
 *------------------------------------------------------*/

static NspObject * int_glink_get_color(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspLink *) Hv)->color);
}

static int int_glink_set_color(void *Hv, char *attr, NspObject *O)
{
  int color;
  if (  IntScalar(O,&color) == FAIL) return FAIL;
  ((NspLink *)Hv)->color = color;
  return OK ;
}

static NspObject * int_glink_get_thickness(void *Hv,char *attr)
{
  return nsp_create_object_from_double(NVOID,((NspLink *) Hv)->thickness);
}
                                                                                                      
static int int_glink_set_thickness(void *Hv, char *attr, NspObject *O)
{
  int thickness;
  if (  IntScalar(O,&thickness) == FAIL) return FAIL;
  ((NspLink *)Hv)->thickness = thickness;
  return OK ;
}

static NspObject * int_glink_get_hilited(void *Hv,char *attr)
{
  return nsp_new_boolean_obj(((NspLink *) Hv)->hilited);
}
                                                                                                      
static int int_glink_set_hilited(void *Hv, char *attr, NspObject *O)
{
  int hilited;
  if (  BoolScalar(O,&hilited) == FAIL) return FAIL;
  ((NspLink *)Hv)->hilited = hilited;
  return OK ;
}

static NspObject * int_glink_get_show(void *Hv,char *attr)
{
  return nsp_new_boolean_obj(((NspLink *) Hv)->show);
}
                                                                                                      
static int int_glink_set_show(void *Hv, char *attr, NspObject *O)
{
  int show;
  if (  BoolScalar(O,&show) == FAIL) return FAIL;
  ((NspLink *)Hv)->show = show;
  return OK ;
}

static AttrTab link_attrs[] = {
  { "color",        int_glink_get_color ,     int_glink_set_color ,     NULL },
  { "thickness",    int_glink_get_thickness,  int_glink_set_thickness,  NULL },
  { "hilited",   int_glink_get_hilited, int_glink_set_hilited, NULL },
  { "show",   int_glink_get_show, int_glink_set_show, NULL },
  { (char *) 0, NULL}
};


/*------------------------------------------------------
 * methods
 *------------------------------------------------------*/

/* draw */

int int_gldraw(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  CheckRhs(1,1);
  link_draw(self);
  NSP_OBJECT(self)->ret_pos = 1;
  return 1;
}

/* translate */

int int_gltranslate(void  *self,Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(0,1);
  if ((M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,2,M,2);
  link_translate(self,M->R);
  NSP_OBJECT(self)->ret_pos = 1;
  return 1;
}

/* resize */ 

int int_glresize(void  *self, Stack stack, int rhs, int opt, int lhs)
{
  NspMatrix *M;
  CheckRhs(2,2);
  CheckLhs(-1,1);
  if ((M = GetRealMat(stack,2)) == NULLMAT ) return RET_BUG;
  CheckLength(stack.fname,2,M,2);
  link_resize(self,M->R);
  NSP_OBJECT(self)->ret_pos = 1;
  return 1;
}


static NspMethods link_methods[] = {
  { "translate", int_gltranslate},
  { "resize",   int_glresize},
  { "draw",   int_gldraw},
  { (char *) 0, NULL}
};
                                                                                                      
static NspMethods *link_get_methods(void) { return link_methods;};
  
/******************************************************
 *  Interface 
 ******************************************************/

static OpTab Link_func[]={
  {"setrowscols_gl",int_set_attributes}, 
  {(char *) 0, NULL}
};

/** call ith function in the Link interface **/

int Link_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(Link_func[i].fonc))(stack,rhs,opt,lhs);
}

/** used to walk through the interface table 
    (for adding or removing functions) **/

void Link_Interf_Info(int i, char **fname, function (**f))
{
  *fname = Link_func[i].name;
  *f = Link_func[i].fonc;
}


static void dist_2_polyline(const NspMatrix *poly,const double pt[2],
			    double pt_proj[2],int *kmin,double *pmin,double *d);

/*********************************************************************
 * Create a graphic link
 *********************************************************************/

NspLink *LinkCreateN(char *name,int n,int color,int thickness)
{
  NspLink *H = new_link() ;
  if ( H == NULLLINK)
    {
      Sciprintf("No more memory\n");
      return NULLLINK;
    }
  if ((NSP_OBJECT(H)->name = NewString(name))== NULLSTRING) return NULLLINK;
  NSP_OBJECT(H)->ret_pos = -1 ; /* XXXX must be added to all data types */ 

  if (( H->poly = MatZeros(n,2))== NULLMAT) return NULLLINK;
  H->color = color;
  H->thickness = thickness;
  H->hilited = FALSE ; 
  H->show = TRUE   ; 
  H->locks[0].port.object_id = NULL;
  H->locks[1].port.object_id = NULL;
  return H;
}

/*---------------------------------------------------------
 * GRint interface implementation 
 *---------------------------------------------------------*/

/**************************************************
 * change or get attributes 
 **************************************************/

int link_get_hilited(NspLink *B) {  return B->hilited; } 
void link_set_hilited(NspLink *B,int val) {  B->hilited = val; } 
int link_get_show(NspLink *B) {  return B->show; } 
void link_set_show(NspLink *B,int val) {  B->show = val; } 

/**************************************************
 * Draw 
 **************************************************/

static int lock_size=2;
static int lock_color=10;
static int link_unconnected_color=10;

void link_draw(NspLink *L)
{
  BCG *Xgc;
  double loc[4];
  int cpat, cwidth;

  if ( L->show == FALSE ) return ;

  Xgc=check_graphic_window();
  cpat = Xgc->graphic_engine->xget_pattern(Xgc);
  cwidth = Xgc->graphic_engine->xget_thickness(Xgc);
  /* draw polyline */
  if ( link_is_lock_connected(L,0)== TRUE && 
       link_is_lock_connected(L,1)== TRUE ) 
    Xgc->graphic_engine->xset_pattern(Xgc,L->color);
  else 
    Xgc->graphic_engine->xset_pattern(Xgc,link_unconnected_color);
  Xgc->graphic_engine->scale->drawpolyline(Xgc,L->poly->R, L->poly->R + L->poly->m,
					   L->poly->m,0);
  /* add hilited */ 
  Xgc->graphic_engine->xset_pattern(Xgc,lock_color);
  if ( L->hilited == TRUE ) 
    {
      int i,m= L->poly->m;
      double *x= L->poly->R, *y = x + m; 
      /* link points except first and last */
      for ( i=1 ; i < m -1; i++) 
	{
	  loc[0]=x[i]-1; loc[1]=y[i]+1;loc[2]=loc[3]= lock_size;
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
	}
      /* firts and last link points which are lock points */
      for ( i=0 ; i <= 1; i++) 
	{
	  if ( i== 1 && m == 1) break; /* just one point in the link */
	  if ( link_is_lock_connected(L,i)== TRUE)
	    Xgc->graphic_engine->xset_pattern(Xgc,lock_color); 
	  else 
	    Xgc->graphic_engine->xset_pattern(Xgc,1); 
	  link_get_lock_pos(L,i,loc);
	  loc[0] += -1; loc[1] += 1;loc[2]=loc[3]= lock_size;
	  Xgc->graphic_engine->scale->fillrectangle(Xgc,loc);
	}
    }
  Xgc->graphic_engine->xset_pattern(Xgc,cpat);
  Xgc->graphic_engine->xset_thickness(Xgc,cwidth);
}

/**************************************************
 * translate 
 **************************************************/

void link_translate(NspLink *L,const double pt[2])
{
  int i,m= L->poly->m;
  double *x= L->poly->R, *y = x + m; 
  /* cannot translate locked link */
  if ( link_is_lock_connected(L,0)) return; 
  if ( link_is_lock_connected(L,1)) return; 
  for ( i= 0 ; i < m ; i++) 
    {
      x[i] += pt[0] ;
      y[i] += pt[1] ;
    }
}

/**************************************************
 * resize 
 **************************************************/

void link_resize(NspLink *R,const double size[2])
{
  
}

/**************************************************
 * Compute locks points and update R
 **************************************************/

void link_update_locks(NspLink *R)
{
  
}


/**************************************************
 * pt is inside NspLink 
 **************************************************/

int link_contains_pt(const NspLink *B,const double pt[2])
{
  double pt_proj[2],pmin,d;
  int kmin=-1;
  dist_2_polyline(B->poly,pt,pt_proj,&kmin,&pmin,&d);
  if ( kmin != -1 && d <  lock_size ) 
    return TRUE;
  else 
    return FALSE;
}
    


/**************************************************
 * utility function 
 * distance from a point to a polyline 
 * the point is on the segment [kmin,kmin+1] (note that 
 * kmin is < size(xp,'*'))
 * and its projection is at point 
 * pt = [ xp(kmin)+ pmin*(xp(kmin+1)-xp(kmin)) ;
 *        yp(kmin)+ pmin*(yp(kmin+1)-yp(kmin)) 
 * the distance is dmin 
 * Copyright ENPC
 **************************************************/

static void dist_2_polyline(const NspMatrix *poly,const double pt[2],
			    double pt_proj[2],int *kmin,double *pmin,
			    double *dmin)
{
  double ux,uy,wx,wy,un,gx,gy;
  int n= poly->m;
  double *xp= poly->R;
  double *yp= poly->R + n;
  double p,d;
  int i;
  *dmin = 1.0+10; /* XXXXX max_double */
  for ( i = 0 ; i < n-1 ; i++) 
    {
      ux = xp[i+1]-xp[i];
      uy = yp[i+1]-yp[i];
      wx = pt[0] - xp[i];
      wy = pt[1] - yp[i];
      un= Max(ux*ux + uy*uy,1.e-10); /* XXXX */
      p = Max(Min((ux*wx+ uy*wy)/un,1),0);
      /* the projection of pt on each segment */
      gx= wx -  p * ux;
      gy= wy -  p * uy;
      d = Max(Abs(gx),Abs(gy));
      if ( d < *dmin ) 
	{
	  *dmin = d;
	  *pmin = p;
	  *kmin = i;
	  pt_proj[0]= xp[i]+ p*ux;
	  pt_proj[1]= yp[i]+ p*uy;
	}
    }
}

/**************************************************
 * pt is inside a control point 
 **************************************************/

int link_control_near_pt(const NspLink *B,const double pt[2], int *cp)
{
  int n= B->poly->m;
  double *xp= B->poly->R;
  double *yp= B->poly->R + n;
  double d; 
  int i;
  for ( i = 0 ; i < n ; i++) 
    {
      d = Max(Abs( xp[i] -pt[0]),Abs( yp[i] -pt[1])) ;
      if ( d < lock_size ) 
	{ 
	  *cp = i ;
	  return TRUE;
	}
    }
  return FALSE;
}

/**************************************************
 * pt is inside a lock point 
 **************************************************/

int link_lock_near_pt(const NspLink *B,const double pt[2], int *cp)
{
  return FALSE;
}

/**************************************************
 * used when moving a control point 
 **************************************************/

void link_move_control_init( NspLink *B,int cp,double pt[2])
{
  pt[0]= B->poly->R[cp];
  pt[1]= B->poly->R[cp + B->poly->m];
}

/*
 * link_get_number: unused 
 */

int link_get_number(NspGFrame *F, NspLink *B) 
{
  int count = 1;
  Cell *C = F->objs->first;
  while ( C != NULLCELL) 
    {
      if ( C->O == (NspObject  *) B) return count;
      C = C->next ;
      count++;
    }
  return 0;
}

/* unlock, lock point lp = 0 or 1 */

static void link_unlock(NspGFrame *F, NspLink *L,int lp) 
{
  NspObject *O1;
  gr_port p; 
  /* just test if unlock is necessary */
  if ( link_is_lock_connected(L,lp)==FALSE ) return; 
  if ( link_get_lock_connection(L,lp,0,&p)==FAIL) return;
  /* we are locked to a block, unlock it */
  O1 = p.object_id;
  if ( O1 != NULLOBJ ) 
    {
      /* propagate unlock to the locked object */
      GR_INT(O1->basetype->interface)->unset_lock_connection(O1,p.lock,p.port);
    }
  /* unset the lock on the link */
  link_unset_lock_connection(L,lp,0);
}

/**
 * link_lock: 
 * @F: a graphic frame  
 * @L: a link 
 * @lp: lock point of the link ie 0 or 1 
 * @p: an object to be connected to lp 
 * 
 * This function first unlock the lock point of Link @L. 
 * Then the lock point of the link is connected to the object/lock point 
 * stored in @p. @p is thus updated to contain a valid port number. 
 * Then @lp lock point of @L is locked to @p.
 * When calling this function, we assume that the object/lock point 
 * described by @p has an available port for connection. 
 * 
 **/

static void link_lock(NspGFrame *F, NspLink *L,int lp,gr_port *p)
{
  int port;
  NspObject *O1;
  gr_port p1 = {(NspObject *) L,lp,0};
  link_unlock(F, L,lp); /* unlock lp */
  /* we first update the Object we want to lock */
  O1 = p->object_id;
  port=GR_INT(O1->basetype->interface)->set_lock_connection(O1,p->lock,&p1);
  /* should not get through port == -1 */
  if ( port == -1 ) Scierror("Unable to connect link to object \n");
  p->port = port; 
  /* Now we lock the link loc to p */
  link_set_lock_connection(L,lp,p);
}

/**
 * link_lock_update: 
 * @F: a graphic frame  
 * @L: a link 
 * @lp: lock point of the link ie 0 or 1 
 * @ptnew: point coordinates.
 * 
 * If @ptnew is near an object lock point and the lock point has an available port, 
 * then the link lock point @lp is unlocked and locked to this new lock point. 
 * @ptnew is also changed. 
 * 
 **/

void link_lock_update(NspGFrame *F, NspLink *L,int lp,double ptnew[2]) 
{
  int lock_c;
  /* double pt[]={ptnew[0],ptnew[1]};*/
  if ( F != NULLGFRAME) 
    {
      NspObject *Ob;
      int cp1;
      int rep = gframe_select_lock(F,ptnew, &Ob, &cp1,&lock_c) ;
      if ( rep != 0 ) 
	{
	  if ( lock_c == TRUE)
	    {
	      gr_port p ={ Ob,cp1,0};/* link_lock will find the available port */
	      link_lock(F,L,lp,&p);
	    }
	}
      else 
	{
	  link_unlock(F,L,lp);
	}
    }
}


void link_move_control(NspGFrame *F, NspLink *L,const double pt[2], int cp,double ptc[2])
{
  double ptnew[2],ptb[2],ptn[2];
  /* move a control point : here the point cp of the polyline */
  int n = L->poly->m;
  double *xp= L->poly->R;
  double *yp= L->poly->R + n;
  /* we keep in ptc the current point position 
   * since magnetism can move us to a new position 
   */
  ptnew[0]  = ptc[0]+pt[0];
  ptnew[1]  = ptc[1]+pt[1];
  ptc[0] = ptnew[0]; ptc[1] = ptnew[1];

  if ( cp >= 1 && cp < n -1 ) 
    {
      int hvfactor=5;
      /*  magnetism toward horizontal or vertival lines */
      ptb[0] = xp[cp-1]; ptb[1] = yp[cp-1];
      ptn[0] = xp[cp+1]; ptn[1] = yp[cp+1];
      if ( Abs( ptb[0] - ptnew[0]) < hvfactor ) ptnew[0]= ptb[0];
      if ( Abs( ptn[0] - ptnew[0]) < hvfactor ) ptnew[0]= ptn[0];
      if ( Abs( ptb[1] - ptnew[1]) < hvfactor ) ptnew[1]= ptb[1];
      if ( Abs( ptn[1] - ptnew[1]) < hvfactor ) ptnew[1]= ptn[1];
    }
  else if ( cp == 0 || cp == n-1 )
    {
      /* try to check if we are in the vivinity of 
       * a lock point lock of a Block 
       * if true this will change ptnew 
       */
      int lp= (cp == 0) ? 0 : 1 ;
      link_lock_update(F,L,lp,ptnew);
    }
  xp[cp] = ptnew[0];
  yp[cp] = ptnew[1];
}

/** 
 * link_split: 
 * 
 * XXXX 
 * 
 */

int link_split(NspGFrame *F,NspLink *L,NspLink **L1,const double pt[2])
{
  int kmin,i,n,n1;
  gr_port p;
  double proj[2], pmin,dmin;
  dist_2_polyline(L->poly,pt,proj,&kmin,&pmin,&dmin);
  n = L->poly->m;
  /* for proj to end of link */
  n1 = L->poly->m-kmin;
  if ((*L1= LinkCreateN(NVOID,n1,L->color,L->thickness))==NULL) return FAIL;
  (*L1)->poly->R[0]=proj[0];
  (*L1)->poly->R[n1]=proj[1];
  for ( i=1; i < n1 ; i++) 
    {
      (*L1)->poly->R[i]= L->poly->R[i+kmin];
      (*L1)->poly->R[i+n1]=L->poly->R[i+kmin+n];
    }
  /* change L */ 
  L->poly->R[kmin+1]=proj[0];
  L->poly->R[kmin+1+n]= proj[1];
  for ( i=0; i < kmin +2 ; i++) 
    {
      L->poly->R[i+ kmin+2]= L->poly->R[i+n];
    }
  if (  nsp_matrix_resize(L->poly,kmin+2,2)== FAIL) return FAIL;
  /* now change links */ 
  if ( link_is_lock_connected(L,1)== TRUE)
    {
      link_get_lock_connection(L,1,0,&p);
      link_unlock(F,L,1);  
      link_lock(F,(*L1),1,&p); 
    }
  /* add L1 in the frame */ 
  if (nsp_list_end_insert(F->objs,(NspObject  *) (*L1)) == FAIL) return FAIL;
  return OK;
}



/** 
 * link_add_control_point: 
 * 
 * XXXX 
 * 
 */

int link_add_control(NspLink *L,const double pt[2])
{
  int kmin,i,n;
  double proj[2], pmin,dmin;
  n = L->poly->m;
  /* the point is to be inserted after kmin */
  dist_2_polyline(L->poly,pt,proj,&kmin,&pmin,&dmin);
  if (  nsp_matrix_resize(L->poly,n+1,L->poly->n)== FAIL) return FAIL;
  /* insert point in resized matrix */
  for ( i= L->poly->mn -1 ; i > L->poly->m+kmin+1 ; i--) 
    L->poly->R[i]= L->poly->R[i-2];
  L->poly->R[L->poly->m+kmin+1]=pt[1];
  for ( i= L->poly->m+kmin ; i> kmin+1 ; i--) 
    L->poly->R[i]= L->poly->R[i-1];
  L->poly->R[kmin+1]=pt[0];
  return OK;
}

/** 
 * link_check: 
 * 
 * Checks lock points 
 * 
 */

void link_check(NspGFrame *F,NspLink *L)
{
  NspObject *obj;
  double pt[2];
  int i,cp,lock_c;
  /* checks if the lock point are not over an other object lock point 
   * and lock is not set 
   */
  for ( i=0; i < 2 ; i++) 
    {
      link_get_lock_pos(L,i,pt);
      /* checks if pt is a lock point of */
      if ( gframe_select_lock(F,pt,&obj,&cp,&lock_c) != 0) 
	{
	  /* pt is near a lock point */ 
	  if ( link_is_lock_connected(L,i)== FALSE) 
	    {
	      Scierror("Link lock point is over an object lock and lock is not active\n");
	      pt[0]+= lock_size*2;
	      link_set_lock_pos(L,i,pt);
	    }
	}
      /* checks if lock point is over a link */
      if ( gframe_select_obj(F,pt,&obj,(NspObject *)L) != 0) 
	{
	  if ( link_is_lock_connected(L,i)== FALSE) 
	    {
	      Scierror("Link lock point [%5.2f,%5.2f]is over an object \n",pt[0],pt[1]);
	      if ( (obj != (NspObject *) L) && IsLink(obj))
		{
		  NspLink *link;
		  Scierror("I split the link \n");
		  if ( link_split(F,(NspLink *)obj,&link,pt) == OK)
		    {
		      /* create a connector */
		      int color=4,thickness=1, background=9;
		      double rect[]={pt[0]-lock_size,pt[1]+lock_size,lock_size*3,lock_size*3}; 
		      NspConnector *C;
		      gr_port p;
		      C=connector_create(NVOID,rect,color,thickness,background,NULL);
		      if ( C == NULL) return;
		      if (nsp_list_end_insert(F->objs,NSP_OBJECT(C)) == FAIL) return ; 
		      /* and link obj,link and L to the connector */
		      p.object_id =NSP_OBJECT(C); 
		      p.lock = 0; 
		      p.port = 0; /* not used */
		      link_lock(F,(NspLink *)obj,1,&p); 
		      link_lock(F,link,0,&p); 
		      link_lock(F,L,i,&p); 
		      gframe_locks_update(F,NSP_OBJECT(C));/* align the locks */
		      GR_INT(((NspObject *)C)->basetype->interface)->draw(C);

		    }
		}
	    }
	}
    }

  /* 
   * checks if the link polyline control points are not over lock points 
   */
  for ( i=1; i <  L->poly->m-1 ; i++) 
    {
      pt[0]= L->poly->R[i]; 
      pt[1]= L->poly->R[i+L->poly->m]; 
      if ( gframe_select_lock(F,pt,&obj,&cp,&lock_c) != 0) 
	{
	  L->poly->R[i]+= 2*lock_size;
	}
    }

}

/*----------------------------------------------------
 * methods of GRint 
 *----------------------------------------------------*/

/**
 * link_get_number_of_locks: 
 * @B: a link 
 * 
 * Returns the number of lock points of the link 
 * 
 * Return value: the number of lock points
 **/

int link_get_number_of_locks(const NspLink *B) 
{
  return 2;
}

/**
 * link_get_number_of_ports: 
 * @B: a link 
 * @lp: a lock point 
 * 
 * Returns the number of ports of lock points lp;
 * 
 * Return value: the number of ports
 **/

int link_get_number_of_ports(const NspLink *B,int lp) 
{
  return 1;
}


/**
 * link_get_lock_connection: 
 * @B: a link 
 * @i: a lock point id. 
 * @port: a port of the lock point @i;
 * 
 * Returns in a gr_port structure information about the object 
 * connected to the port @port of lock point @i. 
 * 
 * Return value: #TRUE if lock point and port number exists or #FALSE. 
 **/

int link_get_lock_connection(const NspLink *B,int i,int port, gr_port *p )
{
  if (( i == 0 || i == 1 ) && port == 0)
    {
      const gr_port *gport= &B->locks[i].port; 
      p->object_id = gport->object_id;
      p->lock = gport->lock;
      p->port = gport->port;
      return OK;
    }
  return FAIL;
}

/**
 * link_get_lock_pos:
 * @B: a link 
 * @i: a lock point id. 
 * @pt: point coordinates.
 *
 * Returns in @pt the position of lock point @i. 
 **/

void link_get_lock_pos(const NspLink *B, int i,double pt[])
{
  if ( i ==  0 ) 
    {
      pt[0]= B->poly->R[0];
      pt[1]= B->poly->R[B->poly->m];
    }
  else if ( i == 1 ) 
    {
      pt[0]= B->poly->R[B->poly->m-1];
      pt[1]= B->poly->R[2*B->poly->m-1];

    }
}

/**
 * link_set_lock_connection: 
 * @B: a link 
 * @i: a lock point id. 
 * @p: information to be connected to one port of lock point i;
 * 
 * the port described by @p is connected to a port of lock point i; 
 * return value: -1 or the port number used for connection.
 **/

int link_set_lock_connection(NspLink *B,int i,const gr_port *p)
{
  if ( i ==  0 || i == 1 ) 
    {
      gr_port *port= &B->locks[i].port;
      if ( port->object_id != NULL) return FALSE; 
      port->object_id = p->object_id;
      port->lock = p->lock;
      port->port= p->port; 
      return 0;
    }
  return -1;
}

/**
 * link_unset_lock_connection: 
 * @B: a link 
 * @i: a lock point id. 
 * @prt: a lock point port. 
 * 
 * unconect the object locked to port @port of lock point @i.
 * 
 **/

void link_unset_lock_connection(NspLink *B,int i,int port)
{
  if (( i == 0 || i ==1) && port == 0)
    {
      /* XXXX : faut-il aussi propager l'info sur l'object locké ? */
      B->locks[i].port.object_id = NULL;
    }
}

/**
 * link_is_lock_connectable
 * @B: a link 
 * @i: a lock point id. 
 * 
 * Checks if there's a free port in lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int link_is_lock_connectable(NspLink *B,int i)
{
  if (  i == 0 || i ==1 )
    {
      if ( B->locks[i].port.object_id == NULL) return TRUE; 
    }
  return FALSE;
}

/**
 * link_is_lock_connected 
 * @B: a link 
 * @i: a lock point id. 
 * 
 * Checks if there's a locked port for lock point @i.
 * 
 * return value: #TRUE or #FALSE.
 **/

int link_is_lock_connected(NspLink *B,int i)
{
  if ( i == 0 || i ==1)
    {
      if ( B->locks[i].port.object_id != NULL) return TRUE; 
    }
  return FALSE;
}

/**
 * link_set_lock_pos: 
 * @B: a link 
 * @i: a lock point id. 
 * @pt: a point coordinates 
 * 
 * Sets the lock point @i poistion to @pt. 
 **/

void link_set_lock_pos(NspLink *B, int i,const double pt[])
{

  if ( i ==  0 ) 
    {
      B->poly->R[0]=pt[0];
      B->poly->R[B->poly->m]=pt[1];
    }
  else if ( i == 1 ) 
    {
      B->poly->R[B->poly->m-1]=pt[0];
      B->poly->R[2*B->poly->m-1]=pt[1];
    }
}







