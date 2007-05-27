/* Nsp
 * Copyright (C) 1998-2007 Jean-Philippe Chancelier Enpc/Cermics
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
 * Encapsulat GDate from glib in a class 
 *
 */

#define  GDate_Private 
#include <nsp/object.h>
#include <gtk/gtk.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gdate.h> 
#include <nsp/interf.h>

/* 
 * NspGDate inherits from NspObject 
 */

int nsp_type_gdate_id=0;
NspTypeGDate *nsp_type_gdate=NULL;

/*
 * Type object for GDate 
 * all the instance of NspTypeGDate share the same id. 
 * nsp_type_gdate: is an instance of NspTypeGDate 
 *    used for objects of NspGDate type (i.e built with new_gdate) 
 * other instances are used for derived classes 
 */
NspTypeGDate *new_type_gdate(type_mode mode)
{
  NspTypeGDate *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdate != 0 && mode == T_BASE ) 
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdate;
    }
  if ((type =  malloc(sizeof(NspTypeGDate))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_object(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdate_attrs ; 
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdate_get_methods; 
  type->new = (new_func *) new_gdate;

  
  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);
  
  /* object methods redefined for gdate */ 

  top->pr = (print_func *) nsp_gdate_print;                  
  top->dealloc = (dealloc_func *) nsp_gdate_destroy;
  top->copy  =  (copy_func *) nsp_gdate_copy;                 
  top->size  = (size_func *) nsp_gdate_size;                
  top->s_type =  (s_type_func *) nsp_gdate_type_as_string;  
  top->sh_type = (sh_type_func *) nsp_gdate_type_short_string;
  top->info = (info_func *) nsp_gdate_info ;                  
  /* top->is_true = (is_true_func  *) nsp_gdate_is_true; */
  /* top->loop =(loop_func *) nsp_gdate_loop;*/
  top->path_extract = (path_func *)  object_path_extract; 
  top->get_from_obj = (get_from_obj_func *) nsp_gdate_object;
  top->eq  = (eq_func *) nsp_gdate_eq;
  top->neq  = (eq_func *) nsp_gdate_neq;
  top->save  = (save_func *) nsp_gdate_xdr_save;
  top->load  = (load_func *) nsp_gdate_xdr_load;
  top->create = (create_func*) int_gdate_create;
  top->latex = (print_func *) nsp_gdate_latex_print;
  
  /* specific methods for gdate */
      
  type->init = (init_func *) init_gdate;

/* 
 * GDate interfaces can be added here 
 * type->interface = (NspTypeBase *) new_type_b();
 * type->interface->interface = (NspTypeBase *) new_type_C()
 * ....
 */
  if ( nsp_type_gdate_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDate called nsp_type_gdate
       */
      type->id =  nsp_type_gdate_id = nsp_new_type_id();
      nsp_type_gdate = type;
      if ( nsp_register_type(nsp_type_gdate) == FALSE) return NULL;
      return ( mode == T_BASE ) ? type : new_type_gdate(mode);
    }
  else 
    {
       type->id = nsp_type_gdate_id;
       return type;
    }
}

/*
 * initialize GDate instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdate(NspGDate *o,NspTypeGDate *type)
{
  /* jump the first surtype */ 
  if ( type->surtype->init(&o->father,type->surtype) == FAIL) return FAIL;
  o->type = type; 
  NSP_OBJECT(o)->basetype = (NspTypeBase *)type;
  /* specific */
  return OK;
}

/*
 * new instance of GDate 
 */

NspGDate *new_gdate() 
{
  NspGDate *loc; 
  /* type must exists */
  nsp_type_gdate = new_type_gdate(T_BASE);
  if ( (loc = malloc(sizeof(NspGDate)))== NULLGDATE) return loc;
  /* initialize object */
  if ( init_gdate(loc,nsp_type_gdate) == FAIL) return NULLGDATE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for GDate 
 *-----------------------------------------------*/
/*
 * size 
 */

static int nsp_gdate_size(NspGDate *Mat, int flag)
{
  return 0;
}

/*
 * type as string 
 */

static char gdate_type_name[]="GDate";
static char gdate_short_type_name[]="gdate";

static char *nsp_gdate_type_as_string(void)
{
  return(gdate_type_name);
}

static char *nsp_gdate_type_short_string(NspObject *v)
{
  return(gdate_short_type_name);
}

/*
 * A == B 
 */

static int nsp_gdate_eq(NspGDate *A, NspObject *B)
{
  NspGDate *loc = (NspGDate *) B;
  if ( check_cast(B,nsp_type_gdate_id) == FALSE) return FALSE ;
  if ( g_date_compare( A->gdate , loc->gdate ) == 0) return TRUE;
  return FALSE;
}

/*
 * A != B 
 */

static int nsp_gdate_neq(NspGDate *A, NspObject *B)
{
  return ( nsp_gdate_eq(A,B) == TRUE ) ? FALSE : TRUE;
}

/*
 * save 
 */

static int nsp_gdate_xdr_save(XDR *xdrs, NspGDate *M)
{
  int julian=0;
  if (g_date_valid(M->gdate) == TRUE) 
    julian = g_date_get_julian(M->gdate);
  if (nsp_xdr_save_i(xdrs,M->type->id) == FAIL) return FAIL;
  if (nsp_xdr_save_string(xdrs, NSP_OBJECT(M)->name) == FAIL) return FAIL;
  if (nsp_xdr_save_i(xdrs, julian) == FAIL) return FAIL;
  return OK;
}

/*
 * load 
 */

static NspGDate  *nsp_gdate_xdr_load(XDR *xdrs)
{
  NspGDate *M = NULL;
  int julian;
  static char name[NAME_MAXL];
  if (nsp_xdr_load_string(xdrs,name,NAME_MAXL) == FAIL) return NULLGDATE;
  if ((M  = gdate_create_void(name,(NspTypeBase *) nsp_type_gdate))== NULLGDATE) return M;
  if (nsp_xdr_load_i(xdrs, &julian) == FAIL) return NULL;
  if ( g_date_valid_julian(julian) == TRUE )
    M->gdate= g_date_new_julian(julian);
  else 
    M->gdate= g_date_new ();
  return M;
}

/*
 * delete 
 */

void nsp_gdate_destroy(NspGDate *H)
{
  nsp_object_destroy_name(NSP_OBJECT(H));
  g_date_free(H->gdate);
  FREE(H);
}

/*
 * info 
 */

void nsp_gdate_info(NspGDate *M,int indent,const char *name,int rec_level)
{
  const char *pname;
  if ( M == NULLGDATE) 
    {
      Sciprintf("Null Pointer GDate \n");
      return;
    }
  pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( g_date_valid(M->gdate) == TRUE ) 
    Sciprintf1(indent,"%s\t=[%d,%d,%d]\t\t%s \n",pname,
	       g_date_get_day(M->gdate),
	       g_date_get_month(M->gdate),
	       g_date_get_year(M->gdate),
	       nsp_gdate_type_short_string(NSP_OBJECT(M)));
  else 
    Sciprintf1(indent,"%s\t=[invalid]\t\t%s \n",pname,
	       nsp_gdate_type_short_string(NSP_OBJECT(M)));
  
}


/*
 * print 
 */

void nsp_gdate_print(NspGDate *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( M == NULLGDATE) 
    {
      Sciprintf("Null Pointer GDate \n");
      return;
    }
  if (user_pref.pr_as_read_syntax) 
    { 
      Sciprintf1(indent,"%s=gdate_new(%d);\n",pname, 
		 g_date_get_julian(M->gdate));
    } 
  else 
    { 
      if ( user_pref.pr_depth  <= rec_level -1 ) 
        {
          nsp_gdate_info(M,indent,pname,rec_level);
          return;
        }
      if ( g_date_valid(M->gdate) == TRUE ) 
	Sciprintf1(indent,"%s\t=[%d,%d,%d]\t\t%s \n",pname,
		   g_date_get_day(M->gdate),
		   g_date_get_month(M->gdate),
		   g_date_get_year(M->gdate),
		   nsp_gdate_type_short_string(NSP_OBJECT(M)));
      else 
	Sciprintf1(indent,"%s\t=[invalid]\t\t%s \n",pname,
		   nsp_gdate_type_short_string(NSP_OBJECT(M)));
	
    }
}

/*
 * latex print 
 */

void nsp_gdate_latex_print(NspGDate *M, int indent,const char *name, int rec_level)
{
  const char *pname = (name != NULL) ? name : NSP_OBJECT(M)->name;
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\002latex:\\[");
  if ( g_date_valid(M->gdate) == TRUE ) 
    Sciprintf1(indent,"%s\t=[%d,%d,%d]\t\t%s \n",pname,
	       g_date_get_day(M->gdate),
	       g_date_get_month(M->gdate),
	       g_date_get_year(M->gdate),
	       nsp_gdate_type_short_string(NSP_OBJECT(M)));
  else 
    Sciprintf1(indent,"%s\t=[invalid]\t\t%s \n",pname,
	       nsp_gdate_type_short_string(NSP_OBJECT(M)));
  if ( nsp_from_texmacs() == TRUE ) Sciprintf("\\]\005");
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for GDate objects 
 * Note that some of these functions could become MACROS XXXXX 
 *-----------------------------------------------------*/

NspGDate   *nsp_gdate_object(NspObject *O)
{
  /* Follow pointer */
  if ( check_cast(O,nsp_type_hobj_id) == TRUE)  O = ((NspHobj *) O)->O ;
  /* Check type */
  if ( check_cast (O,nsp_type_gdate_id) == TRUE ) return ((NspGDate *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdate));
  return NULL;
}

int IsGDateObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i) , nsp_type_gdate_id);
}

int IsGDate(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdate_id);
}

NspGDate  *GetGDateCopy(Stack stack, int i)
{
  if (  GetGDate(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDate  *GetGDate(Stack stack, int i)
{
  NspGDate *M;
  if (( M = nsp_gdate_object(NthObj(i))) == NULLGDATE)
     ArgMessage(stack,i);
  return M;
}

/*-----------------------------------------------------
  * constructor 
 * if type is non NULL it is a subtype which can be used to 
 * create a NspClassB instance 
 *-----------------------------------------------------*/

static NspGDate *gdate_create_void(char *name,NspTypeBase *type)
{
 NspGDate *H  = (type == NULL) ? new_gdate() : type->new();
 if ( H ==  NULLGDATE)
  {
   Sciprintf("No more memory\n");
   return NULLGDATE;
  }
 if ( nsp_object_set_initial_name(NSP_OBJECT(H),name) == NULLSTRING) return NULLGDATE;
 NSP_OBJECT(H)->ret_pos = -1 ;
 H->gdate = NULL;
 return H;
}

NspGDate *gdate_create_dmy(char *name,guint day,guint month,guint year ,NspTypeBase *type)
{
  NspGDate *H; 
  if ( g_date_valid_dmy(day, month, year) ==FALSE) 
    {
      Scierror("Error: [%u,%u,%u] is not a valid dmy\n",day,month,year);
      return NULL;
    }
  H = gdate_create_void(name,type);
  if ( H ==  NULLGDATE) return NULLGDATE;
  H->gdate= g_date_new_dmy(day, month, year);
  return H;
}

NspGDate *gdate_create_julian(char *name,guint32 julian ,NspTypeBase *type)
{
  NspGDate *H; 
  if ( g_date_valid_julian(julian) ==FALSE) 
    {
      Scierror("Error: %u is not a valid julian\n",julian);
      return NULL;
    }
  H= gdate_create_void(name,type);
  if ( H ==  NULLGDATE) return NULLGDATE;
  H->gdate= g_date_new_julian(julian);
  return H;
}

NspGDate *gdate_create_str(char *name,const char *str ,NspTypeBase *type)
{
  NspGDate *H; 
  H= gdate_create_void(name,type);
  if ( H ==  NULLGDATE) return NULLGDATE;
  H->gdate=g_date_new();
  g_date_set_parse(H->gdate,str);
  if ( g_date_valid(H->gdate) == FALSE)
    {
      nsp_gdate_destroy(H);
      return NULL;
    }
  return H;
}


/*
 * copy for gobject derived class  
 */

NspGDate *nsp_gdate_copy(NspGDate *self)
{
  int julian; 
  NspGDate *H  =gdate_create_void(NVOID,(NspTypeBase *) nsp_type_gdate);
  if ( H ==  NULLGDATE) return NULLGDATE;
  H->gdate=g_date_new();
  julian = g_date_get_julian(self->gdate);
  if ( g_date_valid_julian(julian) == TRUE) 
    g_date_set_julian(H->gdate,julian );
 return H;
}

/*-------------------------------------------------------------------
 * wrappers for the GDate
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/

/* create a gdate from 
 * gdate_new(): current date 
 * gdate_new(d,m,y)
 * gdate_new(str)
 * gdate_new(julian) 
 * 
 */

static int int_gdate_create(Stack stack, int rhs, int opt, int lhs)
{
  double djulian, d_day,d_month,d_year;
  NspGDate *H;
  CheckStdRhs(0,3);
  /* want to be sure that type gdate is initialized */
  nsp_type_gdate = new_type_gdate(T_BASE);
  if ( rhs == 0 )
    {
      GTimeVal result;
      /* use current date */
      if(( H = gdate_create_void(NVOID, (NspTypeBase *) nsp_type_gdate)) == NULLGDATE)
	return RET_BUG;
      H->gdate = g_date_new();
      g_get_current_time(&result);
      g_date_set_time_val(H->gdate,&result);
    }
  else if ( rhs == 1 ) 
    {
      if ( IsMatObj(stack,1) )
	{
	  if (GetScalarDouble (stack,1, &djulian) == FAIL) return RET_BUG;
	  if(( H = gdate_create_julian(NVOID,(guint32) djulian, (NspTypeBase *) nsp_type_gdate)) == NULLGDATE)
	    return RET_BUG;
	}
      else if ( IsSMatObj(stack,1) )
	{
	  const char *str;
	  if ((str = GetString(stack,1)) == (char*)0) return RET_BUG;
	  if(( H = gdate_create_str(NVOID,str, (NspTypeBase *) nsp_type_gdate)) == NULLGDATE)
	    {
	      Scierror("Error: cannot parse a date from string %s\n",str);
	      return RET_BUG;
	    }
	}
      else
	{
	  Scierror("Error: first argument must be a string or an int\n");
	  return RET_BUG;
	}
    }
  else if ( rhs == 3) 
    {
      if (GetScalarDouble (stack,1, &d_day) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,2, &d_month) == FAIL) return RET_BUG;
      if (GetScalarDouble (stack,3, &d_year) == FAIL) return RET_BUG;    
      if(( H = gdate_create_dmy(NVOID,(guint) d_day,(guint)d_month,(guint)d_year,
				(NspTypeBase *) nsp_type_gdate)) == NULLGDATE)
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

static int _wrap_gdate_new(Stack stack, int rhs, int opt, int lhs)
{
  return int_gdate_create(stack,rhs,opt,lhs);
}


static int _wrap_g_date_valid(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;

  ret = g_date_valid(self->gdate);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_weekday(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;

  ret = g_date_get_weekday(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_month(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_month(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_year(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_year(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_day(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_day(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_julian(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  gulong ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_julian(self->gdate);
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_day_of_year(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_day_of_year(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_monday_week_of_year(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_monday_week_of_year(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_sunday_week_of_year(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_sunday_week_of_year(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_get_iso8601_week_of_year(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret=0;
  if ( g_date_valid(self->gdate) )
    ret = g_date_get_iso8601_week_of_year(self->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


static int _wrap_g_date_set_parse(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,t_end};
  char *str;
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
  g_date_set_parse(self->gdate, str);
  return 0;
}

static int _wrap_g_date_set_time_t(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  time_t timet;
  if ( GetArgs(stack,rhs,opt,T,&timet) == FAIL) return RET_BUG;
  g_date_set_time_t(self->gdate, timet);
  return 0;
}

/* XXXXXX a faire */


static int _wrap_g_date_set_month(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  GDateMonth month;
  NspObject *nsp_month = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_month) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_MONTH, nsp_month, &month)== FAIL)
    return RET_BUG;
  if ( g_date_valid_month(month)) 
    g_date_set_month(self->gdate, month);
  else 
    {
      Scierror("Error: invalid month\n");
      return RET_BUG;
    }
  return 0;
}

static int _wrap_g_date_set_day(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,t_end};
  NspObject *nsp_weekday = NULL;
  GDateWeekday weekday;
  if ( GetArgs(stack,rhs,opt,T,&nsp_weekday) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_WEEKDAY, nsp_weekday, &weekday)== FAIL)
    return RET_BUG;
  if ( g_date_valid_day(weekday)) 
    g_date_set_day(self->gdate, weekday);
  else 
    {
      Scierror("Error: invalid day\n");
      return RET_BUG;
    }
  return 0;
}

static int _wrap_g_date_set_year(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int year;

  if ( GetArgs(stack,rhs,opt,T,&year) == FAIL) return RET_BUG;
  if ( g_date_valid_year(year) ) 
    g_date_set_year(self->gdate, year);
  else 
    {
      Scierror("Error: invalid year\n");
      return RET_BUG;
    }
  return 0;
}

static int _wrap_g_date_set_dmy(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, s_int , s_int,t_end};
  int day, year, month; 
  if ( GetArgs(stack,rhs,opt,T,&day, &month, &year) == FAIL) return RET_BUG;
  if ( g_date_valid_dmy(day, month, year)) 
    g_date_set_dmy(self->gdate, day, month, year);
  else 
    {
      Scierror("Error: invalid dmy\n");
      return RET_BUG;

    }
  return 0;
}

static int _wrap_g_date_set_julian(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  gulong julian_date;
  if ( GetArgs(stack,rhs,opt,T,&julian_date) == FAIL) return RET_BUG;
  if ( g_date_valid_julian(julian_date))
    g_date_set_julian(self->gdate, julian_date);
  else 
    {
      Scierror("Error: invalid julian\n");
      return RET_BUG;
    }
  return 0;
}

static int _wrap_g_date_is_first_of_month(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  ret = g_date_is_first_of_month(self->gdate);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_is_last_of_month(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  ret = g_date_is_last_of_month(self->gdate);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_add_days(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int n_days;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  if ( GetArgs(stack,rhs,opt,T,&n_days) == FAIL) return RET_BUG;
  g_date_add_days(self->gdate, n_days);
  return 0;
}

static int _wrap_g_date_subtract_days(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int n_days;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  if ( GetArgs(stack,rhs,opt,T,&n_days) == FAIL) return RET_BUG;
  g_date_subtract_days(self->gdate, n_days);
  return 0;
}

static int _wrap_g_date_add_months(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int n_months;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }

  if ( GetArgs(stack,rhs,opt,T,&n_months) == FAIL) return RET_BUG;
  g_date_add_months(self->gdate, n_months);
  return 0;
}

static int _wrap_g_date_subtract_months(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int n_months;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }

  if ( GetArgs(stack,rhs,opt,T,&n_months) == FAIL) return RET_BUG;
  g_date_subtract_months(self->gdate, n_months);
  return 0;
}

static int _wrap_g_date_add_years(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int n_years;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }

  if ( GetArgs(stack,rhs,opt,T,&n_years) == FAIL) return RET_BUG;
  g_date_add_years(self->gdate, n_years);
  return 0;
}

static int _wrap_g_date_subtract_years(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,t_end};
  int n_years;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }

  if ( GetArgs(stack,rhs,opt,T,&n_years) == FAIL) return RET_BUG;
  g_date_subtract_years(self->gdate, n_years);
  return 0;
}

static int _wrap_g_date_days_between(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  int ret;
  NspGObject *date2;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdate, &date2) == FAIL) return RET_BUG;
  if ( g_date_valid(((NspGDate *) date2)->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }

  ret = g_date_days_between(self->gdate,((NspGDate *) date2)->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_compare(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  int ret;
  NspGObject *rhsd;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdate, &rhsd) == FAIL) return RET_BUG;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  if ( g_date_valid(((NspGDate *) rhsd)->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  ret = g_date_compare(self->gdate, ((NspGDate *) rhsd)->gdate);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_date_clamp(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, obj_check,t_end};
  NspGObject *min_date, *max_date;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdate, &min_date, &nsp_type_gdate, &max_date) == FAIL)
    return RET_BUG;
  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  if ( g_date_valid(((NspGDate *) min_date)->gdate) == FALSE 
       ||  g_date_valid(((NspGDate *) max_date)->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  g_date_clamp(self->gdate,((NspGDate *)min_date)->gdate,((NspGDate *)max_date)->gdate);
  return 0;
}

static int _wrap_g_date_order(NspGDate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,t_end};
  NspGObject *date2;

  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdate, &date2) == FAIL) return RET_BUG;

  if ( g_date_valid(self->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }
  if ( g_date_valid(((NspGDate *) date2)->gdate) == FALSE )
    {
      Scierror("Error: invalid date\n");
      return RET_BUG;
    }


  g_date_order(self->gdate, ((NspGDate *)date2)->gdate);
  return 0;
}

static int _wrap_g_date_strftime(NspGDate *self,Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,t_end};
  char s[128], *format;
  int slen=128, ret;
  if ( GetArgs(stack,rhs,opt,T,&format) == FAIL) return RET_BUG;
  ret = g_date_strftime(s, slen, format,self->gdate);
  if ( ret == 0) 
    {
      Scierror("Error: buffer too small in strftime\n");
      return RET_BUG;
    }
  if ( nsp_move_string(stack,1,s,-1) ==FAIL) 
    return RET_BUG;
  return 1;
}

static NspMethods gdate_methods[] = {
  {"valid",(nsp_method *) _wrap_g_date_valid},
  {"get_weekday",(nsp_method *) _wrap_g_date_get_weekday},
  {"get_month",(nsp_method *) _wrap_g_date_get_month},
  {"get_year",(nsp_method *) _wrap_g_date_get_year},
  {"get_day",(nsp_method *) _wrap_g_date_get_day},
  {"get_julian",(nsp_method *) _wrap_g_date_get_julian},
  {"get_day_of_year",(nsp_method *) _wrap_g_date_get_day_of_year},
  {"get_monday_week_of_year",(nsp_method *) _wrap_g_date_get_monday_week_of_year},
  {"get_sunday_week_of_year",(nsp_method *) _wrap_g_date_get_sunday_week_of_year},
  {"get_iso8601_week_of_year",(nsp_method *) _wrap_g_date_get_iso8601_week_of_year},
  {"set_parse",(nsp_method *) _wrap_g_date_set_parse},
  {"set_time_t",(nsp_method *) _wrap_g_date_set_time_t},
  {"set_month",(nsp_method *) _wrap_g_date_set_month},
  {"set_day",(nsp_method *) _wrap_g_date_set_day},
  {"set_year",(nsp_method *) _wrap_g_date_set_year},
  {"set_dmy",(nsp_method *) _wrap_g_date_set_dmy},
  {"set_julian",(nsp_method *) _wrap_g_date_set_julian},
  {"is_first_of_month",(nsp_method *) _wrap_g_date_is_first_of_month},
  {"is_last_of_month",(nsp_method *) _wrap_g_date_is_last_of_month},
  {"add_days",(nsp_method *) _wrap_g_date_add_days},
  {"subtract_days",(nsp_method *) _wrap_g_date_subtract_days},
  {"add_months",(nsp_method *) _wrap_g_date_add_months},
  {"subtract_months",(nsp_method *) _wrap_g_date_subtract_months},
  {"add_years",(nsp_method *) _wrap_g_date_add_years},
  {"subtract_years",(nsp_method *) _wrap_g_date_subtract_years},
  {"days_between",(nsp_method *) _wrap_g_date_days_between},
  {"compare",(nsp_method *) _wrap_g_date_compare},
  {"clamp",(nsp_method *) _wrap_g_date_clamp},
  {"order",(nsp_method *) _wrap_g_date_order},
  {"strftime",(nsp_method *) _wrap_g_date_strftime},
  { NULL, NULL}
};

static NspMethods *gdate_get_methods(void) { return gdate_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static NspObject *_wrap_gdate_get_obj(void *self,char *attr)
{
  int ret;
  ret = ((int) ((NspGDate *) self)->gdate);
  return nsp_new_double_obj((double) ret);
}

static int _wrap_gdate_set_obj(void *self, char *attr, NspObject *O)
{
  /* if ( IntScalar(O,&obj) == FAIL) return FAIL;
  ((NspGDate *) self)->gdate = obj; 
  */
  return OK;
}

static AttrTab gdate_attrs[] = {
  { "obj", (attr_get_function *)_wrap_gdate_get_obj, (attr_set_function *)_wrap_gdate_set_obj,(attr_get_object_function *)int_get_object_failed },
  { NULL,NULL,NULL,NULL },
};


/*-------------------------------------------
 * functions 
 *-------------------------------------------*/

int _wrap_g_date_new_dmy(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, obj, s_int,t_end};
  int day, year;
  GDateMonth month;
  NspObject *nsp_month = NULL, *nsp_ret;
  GDate *ret;

  if ( GetArgs(stack,rhs,opt,T,&day, &nsp_month, &year) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_MONTH, nsp_month, &month)== FAIL)
      return RET_BUG;
    ret = g_date_new_dmy(day, month, year);
  nsp_type_gdate = new_type_gdate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_date_new_julian(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  NspObject *nsp_ret;
  gulong julian_day;
  GDate *ret;

  if ( GetArgs(stack,rhs,opt,T,&julian_day) == FAIL) return RET_BUG;
    ret = g_date_new_julian(julian_day);
  nsp_type_gdate = new_type_gdate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_date_valid_day(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int day, ret;

  if ( GetArgs(stack,rhs,opt,T,&day) == FAIL) return RET_BUG;
    ret = g_date_valid_day(day);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_valid_month(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,t_end};
  int ret;
  GDateMonth month;
  NspObject *nsp_month = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_month) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_MONTH, nsp_month, &month)== FAIL)
      return RET_BUG;
    ret = g_date_valid_month(month);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_valid_year(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int year, ret;

  if ( GetArgs(stack,rhs,opt,T,&year) == FAIL) return RET_BUG;
    ret = g_date_valid_year(year);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_valid_weekday(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,t_end};
  int ret;
  NspObject *nsp_weekday = NULL;
  GDateWeekday weekday;

  if ( GetArgs(stack,rhs,opt,T,&nsp_weekday) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_WEEKDAY, nsp_weekday, &weekday)== FAIL)
      return RET_BUG;
    ret = g_date_valid_weekday(weekday);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_valid_julian(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int ret;
  gulong julian_date;

  if ( GetArgs(stack,rhs,opt,T,&julian_date) == FAIL) return RET_BUG;
    ret = g_date_valid_julian(julian_date);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_valid_dmy(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, obj, s_int,t_end};
  int day, year, ret;
  GDateMonth month;
  NspObject *nsp_month = NULL;

  if ( GetArgs(stack,rhs,opt,T,&day, &nsp_month, &year) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_MONTH, nsp_month, &month)== FAIL)
      return RET_BUG;
    ret = g_date_valid_dmy(day, month, year);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_is_leap_year(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int year, ret;

  if ( GetArgs(stack,rhs,opt,T,&year) == FAIL) return RET_BUG;
    ret = g_date_is_leap_year(year);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_get_days_in_month(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, s_int,t_end};
  int year, ret;
  GDateMonth month;
  NspObject *nsp_month = NULL;

  if ( GetArgs(stack,rhs,opt,T,&nsp_month, &year) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATE_MONTH, nsp_month, &month)== FAIL)
      return RET_BUG;
    ret = g_date_get_days_in_month(month, year);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_get_monday_weeks_in_year(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int year, ret;

  if ( GetArgs(stack,rhs,opt,T,&year) == FAIL) return RET_BUG;
    ret = g_date_get_monday_weeks_in_year(year);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_date_get_sunday_weeks_in_year(Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int,t_end};
  int year, ret;

  if ( GetArgs(stack,rhs,opt,T,&year) == FAIL) return RET_BUG;
    ret = g_date_get_sunday_weeks_in_year(year);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}


/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gdate_func[]={
  {"gdate_new", _wrap_gdate_new},
  {"gdate_create", int_gdate_create},
  {"g_date_valid_day", _wrap_g_date_valid_day},
  {"g_date_valid_month", _wrap_g_date_valid_month},
  {"g_date_valid_year", _wrap_g_date_valid_year},
  {"g_date_valid_weekday", _wrap_g_date_valid_weekday},
  {"g_date_valid_julian", _wrap_g_date_valid_julian},
  {"g_date_valid_dmy", _wrap_g_date_valid_dmy},
  {"g_date_is_leap_year", _wrap_g_date_is_leap_year},
  {"g_date_get_days_in_month", _wrap_g_date_get_days_in_month},
  {"g_date_get_monday_weeks_in_year", _wrap_g_date_get_monday_weeks_in_year},
  {"g_date_get_sunday_weeks_in_year", _wrap_g_date_get_sunday_weeks_in_year},
  { NULL, NULL}
};

/* call ith function in the gdate interface */

int gdate_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
  return (*(gdate_func[i].fonc))(stack,rhs,opt,lhs);
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gdate_Interf_Info(int i, char **fname, function (**f))
{
  *fname = gdate_func[i].name;
  *f = gdate_func[i].fonc;
}

/* ----------- enums and flags ----------- */


void
gdate_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash *) module, G_TYPE_DATE_WEEKDAY, strip_prefix);
  nsp_enum_add_constants((NspHash *) module, G_TYPE_DATE_MONTH, strip_prefix);
}


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
    etype = g_enum_register_static (g_intern_static_string ("GDateMonth"), values);
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
    etype = g_enum_register_static (g_intern_static_string ("GDateMonth"), values);
  }
  return etype;
}
