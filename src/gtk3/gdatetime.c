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
 *
 * Encapsulat GDateTime from glib in a class 
 *
 */

#include <gtk/gtk.h>

#if GLIB_CHECK_VERSION(2, 26, 0) 

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
  if ((M  = nsp_gdate_time_create_void(name,(NspTypeBase *) nsp_type_gdate_time))== NULLGDATETIME) 
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
 * constructor for NspGDateTime instance or instances 
 * of subtypes depending on the value of type.
 *-----------------------------------------------------*/

static NspGDateTime *nsp_gdate_time_create_void(const char *name,NspTypeBase *type)
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

/* create a NspGDateTime from a GDateTime @gdt */

static NspGDateTime *nsp_gdate_time_create(const char *name, GDateTime *gdt)
{
  NspGDateTime *Gdt;
  /* want to be sure that type gdate is initialized */
  nsp_type_gdate_time = new_type_gdate_time(T_BASE);
  if ((Gdt  = nsp_gdate_time_create_void(name,(NspTypeBase *) nsp_type_gdate_time)) 
      == NULLGDATETIME) 
    return NULLGDATETIME;
  Gdt->gdate = gdt; 
  return Gdt;
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
  NspGDateTime *H  = nsp_gdate_time_create_void(NVOID,(NspTypeBase *) nsp_type_gdate_time);
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

/* Creates a NspGDateTime corresponding to this exact instant in the given time zone */

NspGDateTime *nsp_gdate_time_new_now(const gchar *name,const gchar *timezone)
{
  GDateTime *gdt;
  GTimeZone *Tz = g_time_zone_new(timezone);
  gdt = g_date_time_new_now (Tz);
  g_time_zone_unref(Tz);
  if ( gdt == NULL ) return NULL;
  return nsp_gdate_time_create(name,gdt);
}

/* Creates a GDateTime corresponding to this exact instant in the local time zone. */

NspGDateTime *nsp_gdate_time_new_now_local(const gchar *name)
{
  GDateTime *gdt = g_date_time_new_now_local();
  if ( gdt == NULL ) return NULL;
  return nsp_gdate_time_create(name,gdt);
}

/* Creates a GDateTime corresponding to this exact instant in UTC. */

NspGDateTime *nsp_gdate_time_new_now_utc(const gchar *name)
{
  GDateTime *gdt = g_date_time_new_now_utc();
  if ( gdt == NULL ) return NULL;
  return nsp_gdate_time_create(name,gdt);
}

/* Creates a GDateTime corresponding to the given Unix time t in the local time zone.
 * Unix time is the number of seconds that have elapsed since 1970-01-01 00:00:00 UTC, 
 *  regardless of the local time offset.
 */

NspGDateTime *nsp_gdate_time_new_from_unix_local(const char *name,gint64 t)
{
  GDateTime *gdt = g_date_time_new_from_unix_local(t);
  if ( gdt == NULL ) return NULL;
  return nsp_gdate_time_create(name,gdt);
}


/* Creates a GDateTime corresponding to the given Unix time t in UTC.
 * Unix time is the number of seconds that have elapsed since 1970-01-01 00:00:00 UTC.
 */

NspGDateTime *nsp_gdate_time_new_from_unix_utc(const char *name,gint64 t)
{
  GDateTime *gdt = g_date_time_new_from_unix_utc(t);
  if ( gdt == NULL ) return NULL;
  return nsp_gdate_time_create(name,gdt);
}

/* 
   g_date_time_new_from_timeval_local ()
   GDateTime *g_date_time_new_from_timeval_local  (const GTimeVal *tv);
   Creates a GDateTime corresponding to the given GTimeVal tv in the local time zone.
   The time contained in a GTimeVal is always stored in the form of seconds elapsed 
   since 1970-01-01 00:00:00 UTC, regardless of the local time offset.
*/

/* 
   g_date_time_new_from_timeval_utc ()
   GDateTime *g_date_time_new_from_timeval_utc    (const GTimeVal *tv);
   Creates a GDateTime corresponding to the given GTimeVal tv in UTC.
   The time contained in a GTimeVal is always stored in the form of 
   seconds elapsed since 1970-01-01 00:00:00 UTC.
*/

/*  Creates a new GDateTime corresponding to the given date and time in the time zone tz.
 *  The year must be between 1 and 9999, 
 *  month between 1 and 12 and day between 1 and 28, 29, 30 or 31 depending on the month and the year.
 *  hour must be between 0 and 23 and minute must be between 0 and 59.
 *  seconds must be at least 0.0 and must be strictly less than 60.0. 
 *  It will be rounded down to the nearest microsecond.
 * 
 */

NspGDateTime *nsp_gdate_time_new(const char *name,gint year,gint month,gint day,
				 gint hour, gint minute, gint second,
				 const char *timezone)
{
  GTimeZone *Tz = g_time_zone_new(timezone);
  GDateTime *gdt; 
  if ( Tz == NULL) 
    {
      Scierror("Error: invalid time zone %s \n",timezone);
      return NULL;
    }
  gdt = g_date_time_new(Tz,year, month, day, hour, minute,second);
  g_time_zone_unref(Tz);
  if ( gdt == NULL) 
    {
      Scierror("Error: [%d,%d,%d,%d,%d,%d] is not a valid %s time\n",day,month,year,
	       hour, minute,second,timezone);
      return NULL;
    }
  return nsp_gdate_time_create(name,gdt);
}

NspGDateTime *nsp_gdate_time_new_local(const char *name,gint year,gint month,gint day,
					      gint hour, gint minute, gint second)
{
  GDateTime *gdt = g_date_time_new_local(year, month, day, hour, minute,second);
  if ( gdt == NULL) 
    {
      Scierror("Error: [%d,%d,%d,%d,%d,%d] is not a valid local time\n",day,month,year,
	       hour, minute,second);
      return NULL;
    }
  return nsp_gdate_time_create(name,gdt);
}

NspGDateTime *nsp_gdate_time_new_utc(const char *name,gint year,gint month,gint day,
					    gint hour, gint minute, gint second)
{
  GDateTime *gdt = g_date_time_new_local(year, month, day, hour, minute,second);
  if ( gdt == NULL) 
    {
      Scierror("Error: [%d,%d,%d,%d,%d,%d] is not a valid utc time\n",day,month,year,
	       hour, minute,second);
      return NULL;
    }
  return nsp_gdate_time_create(name,gdt);
}

/* interface to create a NspGDateTime object */

int int_gdate_time_create(Stack stack, int rhs, int opt, int lhs)
{
  char *tz = NULL;
  double d_day,d_month,d_year, d_hour, d_minute, d_second;
  NspGDateTime *H;
  Boolean utc= FALSE; /* default is local */
  nsp_option opts[] ={{ "utc", s_bool,NULLOBJ,-1},
		      { "tz", string,NULLOBJ,-1},
		      { NULL,t_end,NULLOBJ,-1}};
  CheckStdRhs(0,6);
  if ( get_optional_args(stack, rhs, opt, opts, &utc,&tz) == FAIL )
    return RET_BUG;
  
  if ( rhs - opt == 0 )
    {
      if ( tz != NULL) 
	{
	  H = nsp_gdate_time_new_now(NVOID,tz);
	}
      else if ( utc == TRUE )
	{
	  H = nsp_gdate_time_new_now_utc(NVOID);
	}
      else
	{
	  H = nsp_gdate_time_new_now_local(NVOID);
	}
    }
  else if ( rhs -opt  == 1 )
    {
      NspIMatrix  *IA=NULL;
      gint64 t;
      if (( IA = GetIMat(stack,1))  == NULLIMAT) return RET_BUG;
      if ( IA->itype != nsp_gint64) 
	{
	  Scierror ("Error: in %s, integer should be of int64 subtype\n", NspFname(stack));
	  return RET_BUG;
	}
      if ( IA->mn != 1 )
	{
	  Scierror ("Error: in %s, argument sould be a 1x1 int64 \n", NspFname(stack));
	  return RET_BUG;
	}
      t = IA->Gint64[0];
      if ( tz != NULL) 
	{
	  Scierror("Error: a time zone cannot be given when datetime is given with unix time\n");
	  return RET_BUG;
	}
      else if ( utc == TRUE )
	{
	  H = nsp_gdate_time_new_from_unix_utc(NVOID,t);
	}
      else
	{
	  H = nsp_gdate_time_new_from_unix_local(NVOID,t);
	}
    }
  else if ( rhs -opt  == 6 )
    {
      if (GetScalarDouble (stack,1, &d_year) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,2, &d_month) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,3, &d_day) == FAIL) return RET_BUG;  
      if (GetScalarDouble (stack,4, &d_hour) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,5, &d_minute) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,6, &d_second) == FAIL) return RET_BUG;  
      if ( tz != NULL )
	{
	  H = nsp_gdate_time_new(NVOID,d_year, d_month, d_day, d_hour, d_minute, d_second,tz);
	}
      else if ( utc == TRUE ) 
	{
	  H = nsp_gdate_time_new_utc(NVOID,d_year, d_month, d_day, d_hour, d_minute, d_second);
	}
      else
	{
	  H = nsp_gdate_time_new_local(NVOID,d_year, d_month, d_day, d_hour, d_minute, d_second);
	}
    }
  else 
    { 
      Scierror("Error: rhs should be 0 or one or 6 followed by optionals utc=%%t | %%f and tz=string\n");
      return RET_BUG;
    }
  if ( H == NULL) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(H));
  return 1;
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
      if ( nsp_move_double(stack,2,(double) m)==FAIL) return RET_BUG;
    }
  if ( lhs >= 3 ) 
    {
      if ( nsp_move_double(stack,3,(double) d)==FAIL) return RET_BUG;
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
      Scierror("Error: method add_years failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_months failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_weeks failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_days failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_hours failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_seconds failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_minutes failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
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
      Scierror("Error: method add_full failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_to_unix(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  NspIMatrix *Im;
  gint64 t;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  t = g_date_time_to_unix(self->gdate);
  if (( Im =nsp_imatrix_create(NVOID,1,1,nsp_gint64)) == NULLIMAT) 
    return RET_BUG;
  Im->Gint64[0] = t;
  MoveObj(stack,1,NSP_OBJECT(Im));
  return 1;
}

static int _wrap_g_date_time_to_local(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  NspGDateTime *M = NULL;
  GDateTime *g;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  g = g_date_time_to_local(self->gdate);
  if ( g == NULL) 
    {
      Scierror("Error: method to_local failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_to_utc(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  NspGDateTime *M = NULL;
  GDateTime *g;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  g = g_date_time_to_utc(self->gdate);
  if ( g == NULL) 
    {
      Scierror("Error: method to_utc failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_to_timezone(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  NspGDateTime *M = NULL;
  GTimeZone *Tz; 
  GDateTime *g;
  char *timezone;

  CheckStdRhs(1,1);
  CheckLhs(0,1);

  if ((timezone = GetString(stack,1))== NULL) 
    return RET_BUG;

  if ((Tz = g_time_zone_new(timezone)) == NULL) 
    {
      Scierror("Error: invalid time zone %s \n",timezone);
      return RET_BUG;
    }
  g = g_date_time_to_timezone(self->gdate,Tz);
  g_time_zone_unref(Tz);
  if ( g == NULL) 
    {
      Scierror("Error: method to_utc failed\n");
      return RET_BUG;
    }
  if ((M = nsp_gdate_time_create(NVOID,g)) == NULLGDATETIME) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(M));
  return 1;
}

static int _wrap_g_date_time_format(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  char *format,*str;
  int rep;
  CheckStdRhs(1,1);
  CheckLhs(0,1);
  
  if ((format = GetString(stack,1))== NULL) 
    return RET_BUG;
  str = g_date_time_format(self->gdate,format);
  if ( str == NULL) 
    {
      Scierror("Error: method format failed\n");
      return RET_BUG;
    }
  rep = nsp_move_string(stack,1, str ,-1);
  g_free(str);
  if ( rep == FAIL ) return RET_BUG;
  return 1;
}

static int _wrap_g_date_time_get_timezone_abbreviation(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  const char *str;
  int rep;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  str = g_date_time_get_timezone_abbreviation(self->gdate);
  if ( str == NULL) 
    {
      Scierror("Error: method get_timezone_abbreviation failed\n");
      return RET_BUG;
    }
  rep = nsp_move_string(stack,1, str ,-1);
  if ( rep == FAIL ) return RET_BUG;
  return 1;
}


static int _wrap_g_date_time_get_utc_offset(NspGDateTime *self,Stack stack,int rhs,int opt,int lhs)
{
  GTimeSpan ts= g_date_time_get_utc_offset(self->gdate);
  NspIMatrix *Im;
  CheckStdRhs(0,0);
  CheckLhs(0,1);
  if (( Im =nsp_imatrix_create(NVOID,1,1,nsp_gint64)) == NULLIMAT) 
    return RET_BUG;
  Im->Gint64[0] = ts;
  MoveObj(stack,1,NSP_OBJECT(Im));
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
  {"to_unix",(nsp_method *) _wrap_g_date_time_to_unix},
  {"to_utc",(nsp_method *) _wrap_g_date_time_to_utc},
  {"to_local",(nsp_method *) _wrap_g_date_time_to_local},
  {"to_timezone", (nsp_method *) _wrap_g_date_time_to_timezone}, 
  {"format", (nsp_method *) _wrap_g_date_time_format}, 
  {"get_timezone_abbreviation", (nsp_method *) _wrap_g_date_time_get_timezone_abbreviation},
  {"get_utc_offset",  (nsp_method *) _wrap_g_date_time_get_utc_offset},
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
  {"gdate_time_new", int_gdate_time_create},
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

#else 


#endif 
