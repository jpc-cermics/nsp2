/* Nsp
 * Copyright (C) 1998-2011 Jean-Philippe Chancelier Enpc/Cermics
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
 * Encapsulat GDateTime from glib in a class 
 *
 */

#include <gtk/gtk.h>

#define  GDateTime_Private 
#include <nsp/object.h>
#include <nsp/smatrix.h>
#include <nsp/matrix.h>
#include <nsp/imatrix.h>
#include <nsp/list.h>
#include <nsp/hash.h>
#include <nsp/hobj.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gdatetime.h> 
#include <nsp/gtk/gdate.h> 
#include <nsp/interf.h>

/* 
 * NspGDateTime inherits from NspObject 
 */

int nsp_type_gdate_time_id=0;
NspTypeGDateTime *nsp_type_gdate_time =NULL;

/*
 * Type object for GDateTime 
 * all the instance of NspTypeGDateTime share the same id. 
 * nsp_type_gdate: is an instance of NspTypeGDateTime 
 *    used for objects of NspGDateTime type (i.e built with new_gdate) 
 * other instances are used for derived classes 
 */
NspTypeGDateTime *new_type_gdate_time(type_mode mode)
{
  NspTypeGDateTime *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdate_time != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdate_time;
    }
  if ((type =  malloc(sizeof(NspTypeGDateTime))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdate_time_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdate_time_get_methods; 
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdate_time;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdate */ 

  top->pr = (print_func *) nsp_gdate_time_print;                  
  top->dealloc = (dealloc_func *) nsp_gdate_time_destroy;
  top->copy  =  (copy_func *) nsp_gdate_time_copy;                 
  top->size  = (size_func *) nsp_gdate_time_size;                
  top->s_type =  (s_type_func *) nsp_gdate_time_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_gdate_time_type_short_string;
  top->info = (info_func *) nsp_gdate_time_info ;                  
  /* top->is_true = (is_true_func  *) nsp_gdate_time_is_true; */
  /* top->loop =(loop_func *) nsp_gdate_time_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_gdate_time_object;
  top->eq  = (eq_func *) nsp_gdate_time_eq;
  top->neq  = (eq_func *) nsp_gdate_time_neq;
  top->save  = (save_func *) nsp_gdate_time_xdr_save;
  top->load  = (load_func *) nsp_gdate_time_xdr_load;
  top->create = (create_func*) int_gdate_time_create;
  top->latex = (print_func *) nsp_gdate_time_latex_print;
  
  /* specific methods for gdate */
      
  type->init = (init_func *) init_gdate_time;

/* 
 * GDateTime interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_gdate_time_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDateTime called nsp_type_gdate_time
       */
      type->id =  nsp_type_gdate_time_id = nsp_new_type_id();
      nsp_type_gdate_time = type;
      if ( nsp_register_type(nsp_type_gdate_time) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gdate_time(mode);
    }
  else 
    {
       type->id = nsp_type_gdate_time_id;
       return type;
    }
}

/*
 * initialize GDateTime instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdate_time(NspGDateTime *o,NspTypeGDateTime *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GDateTime 
 */

NspGDateTime *new_gdate_time() 
{
  NspGDateTime *loc; 
  /* type must exists */
  nsp_type_gdate_time = new_type_gdate_time(T_BASE);
  if ( (loc = malloc(sizeof(NspGDateTime)))== NULLGDATETIME) return loc;
  /* initialize object */
  if ( init_gdate_time(loc,nsp_type_gdate_time) == FAIL) return NULLGDATETIME;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GDateTime 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gdate_time_size(NspGDateTime *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gdate_time_type_name[]="GDateTime";
static char gdate_time_short_type_name[]="gdatetime";

static char *nsp_gdate_time_type_as_string(void)
{
  return(gdate_time_type_name);
}

static char *nsp_gdate_time_type_short_string(NspObject *v)
{
  return(gdate_time_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gdate_time_eq(NspGDateTime *A, NspObject *B)
{
  NspGDateTime *loc = (NspGDateTime *) B;
  if ( check_cast(B,nsp_type_gdate_time_id) == FALSE) return FALSE ;
  if ( g_date_time_compare( A->gdate , loc->gdate ) == 0) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int nsp_gdate_time_neq(NspGDateTime *A, NspObject *B)
{
  return ( nsp_gdate_time_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_gdate_time_xdr_save(XDR *xdrs, NspGDateTime *M)
{
  GDateTime *Dt;
  gint64 gdt;
  Dt = g_date_time_to_utc(M->gdate);
  gdt= g_date_time_to_unix(Dt);
  if (nsp_xdr_save_i(xdrs,nsp_dynamic_id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs,type_get_name(nsp_type_gdate_time)) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_array_ixx(xdrs,&gdt,nsp_gint64,1) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */


static NspGDateTime  *nsp_gdate_time_xdr_load(XDR *xdrs)
{
  gint64 gdt;
  NspGDateTime *M = NULL;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGDATETIME;
  if ((M  = gdate_time_create_void(name,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return M;
  if (nsp_xdr_load_array_ixx(xdrs,&gdt,nsp_gint64,1) == FAIL) return  NULLGDATETIME;
  M->gdate = g_date_time_new_from_unix_utc(gdt);
  return M;
}

/*
 * delete 
 */

void nsp_gdate_time_destroy(NspGDateTime *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  g_date_time_unref(H->gdate);
  FREE(H);
}

/*
 * info 
 */

int nsp_gdate_time_info(NspGDateTime *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGDATETIME) 
    {
      Sciprintf("Null Pointer GDateTime \n");
      return TRUE;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  Sciprintf1(indent,"%s\t=[%d,%d,%d,%d,%d,%d]\t\t%s \n",pname,
	     g_date_time_get_year (M->gdate),
	     g_date_time_get_month(M->gdate),
	     g_date_time_get_day_of_month (M->gdate),
	     g_date_time_get_hour(M->gdate),            
	     g_date_time_get_minute(M->gdate),
	     g_date_time_get_second(M->gdate),
	     nsp_gdate_time_type_short_string(NSP_OBJECT(M)));
  return TRUE;
}


/*
 * print 
 */

int nsp_gdate_time_print(NspGDateTime *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGDATETIME) 
    {
      Sciprintf("Null Pointer GDateTime \n");
      return TRUE;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"XXXXXX%s=gdate_time_new(%d);\n",pname, 
		 100);
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_gdate_time_info(M,indent,pname,rec_level);
          return TRUE;
        }
      Sciprintf1(indent,"%s\t=[%d,%d,%d,%d,%d,%d]\t\t%s \n",pname,
		 g_date_time_get_year (M->gdate),
		 g_date_time_get_month(M->gdate),
		 g_date_time_get_day_of_month (M->gdate),
		 g_date_time_get_hour(M->gdate),            
		 g_date_time_get_minute(M->gdate),
		 g_date_time_get_second(M->gdate),
		 nsp_gdate_time_type_short_string(NSP_OBJECT(M)));
    }
  return TRUE;
}

/*
 * latex print 
 */

int nsp_gdate_time_latex_print(NspGDateTime *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  Sciprintf1(indent,"%s\t=[%d,%d,%d,%d,%d,%d]\t\t%s \n",pname,
	     g_date_time_get_year (M->gdate),
	     g_date_time_get_month(M->gdate),
	     g_date_time_get_day_of_month (M->gdate),
	     g_date_time_get_hour(M->gdate),            
	     g_date_time_get_minute(M->gdate),
	     g_date_time_get_second(M->gdate),
	     nsp_gdate_time_type_short_string(NSP_OBJECT(M)));
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
  return TRUE;
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GDateTime objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGDateTime   *nsp_gdate_time_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gdate_time_id) == TRUE ) return ((NspGDateTime *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdate_time));
  return NULL;
}

int IsGDateTimeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdate_time_id);
}

int IsGDateTime(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdate_time_id);
}

NspGDateTime  *GetGDateTimeCopy(Stack stack, int i)
{
  if (  GetGDateTime(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDateTime  *GetGDateTime(Stack stack, int i)
{
  NspGDateTime *M;
  if (( M = nsp_gdate_time_object(NthObj(i))) == NULLGDATETIME)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspGDateTime *gdate_time_create_void(char *name,NspTypeBase *type)
{
 NspGDateTime *H  = (type == NULL) ? new_gdate_time() : type->new();
 if ( H ==  NULLGDATETIME)
  {
   Sciprintf("No more memory\n");
   return NULLGDATETIME;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGDATETIME;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->gdate = NULL;
 return H;
}

NspGDateTime *gdate_time_create_full(char *name,gint year,gint month,gint day,
				     gint hour, gint minute, gint second,
				     NspTypeBase *type)
{
  NspGDateTime *H; 
  H = gdate_time_create_void(name,type);
  if ( H ==  NULLGDATETIME) return NULLGDATETIME;
  H->gdate = g_date_time_new_local (year, month, day, hour, minute,second);
  if ( H->gdate == NULL) 
    {
      Scierror("Error: [%d,%d,%d,%d,%d,%d] is not a valid local time\n",day,month,year,
	       hour, minute,second);
      return NULL;
    }
  return H;
}


/*
 * copy for gobject derived class  
 */

NspGDateTime *nsp_gdate_time_copy(NspGDateTime *self)
{
  gint year=g_date_time_get_year (self->gdate);
  gint month=g_date_time_get_month(self->gdate);
  gint day=g_date_time_get_day_of_month (self->gdate);
  gint hour=g_date_time_get_hour(self->gdate);
  gint minute=g_date_time_get_minute(self->gdate);
  gint second=g_date_time_get_second(self->gdate);
  NspGDateTime *H  =gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time);
  if ( H ==  NULLGDATETIME) return NULLGDATETIME;
  if (( H->gdate = g_date_time_new_local (year, month, day, hour, minute,second)) == NULL) 
    {
      Scierror("Error: failed to copy gdate_time\n");
      return NULL;
    }
  return H;
}


/*-------------------------------------------------------------------
 * wrappers for the GDateTime
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* create a gdate from 
 * gdate_time_new(): current date 
 * gdate_time_new(d,m,y)
 * gdate_time_new(str)
 * gdate_time_new(julian) 
 * 
 */

static int int_gdate_time_create(Stack stack, int rhs, int opt, int lhs)
{
  double d_day,d_month,d_year;
  NspGDateTime *H;
  CheckStdRhs(0,6);
  /* want to be sure that type gdate is initialized */
  nsp_type_gdate_time = new_type_gdate_time(T_BASE);
  if ( rhs == 6 )
    {
      double d_hour, d_minute, d_second;
      if (GetScalarDouble (stack,1, &d_year) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,2, &d_month) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,3, &d_day) == FAIL) return RET_BUG;  
      if (GetScalarDouble (stack,4, &d_hour) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,5, &d_minute) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,6, &d_second) == FAIL) return RET_BUG;  
      if(( H = gdate_time_create_full(NVOID,d_year, d_month, d_day, d_hour, d_minute, d_second,
				      (NspTypeBase *) nsp_type_gdate_time)) == NULLGDATETIME)
	return RET_BUG;
    }
  else 
    {
      Scierror("Error: rhs should be one or three\n");
      return RET_BUG;
    }
  MoveObj(stack,1,(NspObject  *) H);
  return 1;
} 

static int _wrap_gdate_time_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_gdate_time_create(stack,rhs,opt,lhs);
}

static int _wrap_g_date_time_get_day_of_week(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret = g_date_time_get_day_of_week(self->gdate);
  CheckLhs(0,1);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_get_day_of_month(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret = g_date_time_get_day_of_month(self->gdate);
  CheckLhs(0,1);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_get_day_of_year(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret = g_date_time_get_day_of_year(self->gdate);
  CheckLhs(0,1);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_get_ymd(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int d,m,y;
  CheckLhs(0,3);
  g_date_time_get_ymd(self->gdate,&y,&m,&d);
  if ( nsp_move_double(stack,1,(double) y)==FAIL) return RET_BUG;
  if ( lhs >= 2 ) 
    {
      if ( nsp_move_double(stack,1,(double) m)==FAIL) return RET_BUG;
    }
  if ( lhs >= 3 ) 
    {
      if ( nsp_move_double(stack,1,(double) d)==FAIL) return RET_BUG;
    }
  return Max(lhs,1);
}

static int _wrap_g_date_time_get_hour(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  ret = g_date_time_get_hour(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_g_date_time_get_minute(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret= g_date_time_get_minute(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_g_date_time_get_month(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  ret = g_date_time_get_month(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_get_second(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  ret = g_date_time_get_second(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_get_year(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  ret = g_date_time_get_year(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_g_date_time_is_daylight_savings(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret= g_date_time_is_daylight_savings(self->gdate);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_add_years(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;

  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g= g_date_time_add_years(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_add_months(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;

  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g=g_date_time_add_months(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_add_weeks(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;

  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g=g_date_time_add_weeks(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_add_days(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;

  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g=g_date_time_add_days(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_add_hours(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;

  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g=g_date_time_add_hours(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_add_seconds(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;
  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g=g_date_time_add_seconds(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}


static int _wrap_g_date_time_add_minutes(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;
  int_types T[] = {s_int,t_end};
  int n;
  if ( GetArgs(stack,rhs,opt,T,&n) == FAIL) return RET_BUG;
  g=g_date_time_add_minutes(self->gdate, n);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_add_full(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GDateTime *g;
  NspGDateTime *M = NULL;
  
  int_types T[] = {s_int,s_int,s_int,s_int,s_int,s_int,t_end};
  int y,m,d,h,mi,s;
  if ( GetArgs(stack,rhs,opt,T,&y,&m,&d,&h,&mi,&s) == FAIL) return RET_BUG;
  g= g_date_time_add_full(self->gdate,y,m,d,h,mi,s);
  if ( g == NULL) 
    {
      Scierror("Error: g_date_time_add_full failed\n");
      return RET_BUG;
    }
  if ((M  = gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
    return RET_BUG;
  M->gdate = g ;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}


static NspMethods gdate_time_methods[] = {
  {"get_month",(nsp_method *) _wrap_g_date_time_get_month},
  {"get_year",(nsp_method *) _wrap_g_date_time_get_year},
  {"get_day_of_year",(nsp_method *) _wrap_g_date_time_get_day_of_year},
  {"add_days",(nsp_method *) _wrap_g_date_time_add_days},
  {"add_months",(nsp_method *) _wrap_g_date_time_add_months},
  {"add_years",(nsp_method *) _wrap_g_date_time_add_years},
  {"get_day_of_week",(nsp_method *)_wrap_g_date_time_get_day_of_week},
  {"get_day_of_month",(nsp_method *)_wrap_g_date_time_get_day_of_month},
  {"get_ymd",(nsp_method *)_wrap_g_date_time_get_ymd},
  {"get_hour",(nsp_method *)_wrap_g_date_time_get_hour},
  {"get_minute",(nsp_method *)_wrap_g_date_time_get_minute},
  {"get_second",(nsp_method *)_wrap_g_date_time_get_second},
  {"is_daylight_savings",(nsp_method *)_wrap_g_date_time_is_daylight_savings},
  {"add_weeks",(nsp_method *)_wrap_g_date_time_add_weeks},
  {"add_hours",(nsp_method *)_wrap_g_date_time_add_hours},
  {"add_seconds",(nsp_method *)_wrap_g_date_time_add_seconds},
  {"add_minutes",(nsp_method *)_wrap_g_date_time_add_minutes},
  {"add_full",(nsp_method *)_wrap_g_date_time_add_full},
  { NULL, NULL}
};

static NspMethods *gdate_time_get_methods(void) { return gdate_time_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

#if 0 
static NspObject *_wrap_gdate_time_get_obj(void *self,char *attr)
{
  int ret;
  ret = ((int) ((NspGDateTime *) self)->gdate);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_gdate_time_set_obj(void *self, char *attr, NspObject *O)
{
  /* if ( IntScalar(O,&obj) == FAIL) return FAIL;
  ((NspGDateTime *) self)->gdate = obj; 
  */
  return OK;
}
#endif 

static AttrTab gdate_time_attrs[] = {
#if 0 
  { "obj", (attr_get_function *)_wrap_gdate_time_get_obj, (attr_set_function *)_wrap_gdate_time_set_obj,(attr_get_object_function *)int_get_object_failed,(attr_set_object_function *)int_set_object_failed },
#endif 
  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gdate_time_func[]={
  {"gdate_time_new", _wrap_gdate_time_new},
  {"gdate_time_create", int_gdate_time_create},
  { NULL, NULL}
};

/* call ith function in the gdate interface */

int gdate_time_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(gdate_time_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gdate_time_Interf_Info(int i, char **fname, function (**f))
{
  *fname = gdate_time_func[i].name;
  *f = gdate_time_func[i].fonc;
}

/* ----------- enums and flags ----------- */

/* unused 
 *
 */

void
gdate_time_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash *) module, G_TYPE_DATE_WEEKDAY, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, G_TYPE_DATE_MONTH, strip_prefix);
}

#if  GTK_CHECK_VERSION(2,8,0)

static GType g_date_month_get_type(void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { G_DATE_BAD_MONTH, "G_DATE_BAD_MONTH", "bad-month" },
      { G_DATE_JANUARY, "G_DATE_JANUARY", "january" },
      { G_DATE_FEBRUARY, "G_DATE_FEBRUARY", "february" },
      { G_DATE_MARCH, "G_DATE_MARCH", "march" },
      { G_DATE_APRIL, "G_DATE_APRIL", "april" },
      { G_DATE_MAY, "G_DATE_MAY", "may" },
      { G_DATE_JUNE, "G_DATE_JUNE", "june" },
      { G_DATE_JULY, "G_DATE_JULY", "july" },
      { G_DATE_AUGUST, "G_DATE_AUGUST", "august" },
      { G_DATE_SEPTEMBER, "G_DATE_SEPTEMBER", "september" },
      { G_DATE_OCTOBER, "G_DATE_OCTOBER", "october" },
      { G_DATE_NOVEMBER, "G_DATE_NOVEMBER", "november" },
      { G_DATE_DECEMBER, "G_DATE_DECEMBER", "december" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("GDateTimeMonth"), values);
  }
  return etype;
}

static GType g_date_weekday_get_type(void)
{
  static GType etype = 0;
  if (etype == 0) {
    static const GEnumValue values[] = {
      { G_DATE_BAD_WEEKDAY, "G_DATE_BAD_WEEKDAY", "bad-weekday" },
      { G_DATE_MONDAY, "G_DATE_MONDAY", "monday" },
      { G_DATE_TUESDAY, "G_DATE_TUESDAY", "tuesday" },
      { G_DATE_WEDNESDAY, "G_DATE_WEDNESDAY", "wednesday" },
      { G_DATE_THURSDAY, "G_DATE_THURSDAY", "thursday" },
      { G_DATE_FRIDAY, "G_DATE_FRIDAY", "friday" },
      { G_DATE_SATURDAY, "G_DATE_SATURDAY", "saturday" },
      { G_DATE_SUNDAY, "G_DATE_SUNDAY", "sunday" },
      { 0, NULL, NULL }
    };
    etype = g_enum_register_static (g_intern_static_string ("GDateTimeMonth"), values);
  }
  return etype;
}

#endif 

