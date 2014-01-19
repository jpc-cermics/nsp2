/* -*- Mode: C -*- */
#ifndef NSP_INC_GDateTime
#define NSP_INC_GDateTime

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* GDateTime */

#include "nsp/object.h"

/*
 * NspGDateTime inherits from NspObject
 */

typedef struct _NspGDateTime NspGDateTime ;
typedef struct _NspTypeGDateTime NspTypeGDateTime ;

struct _NspTypeGDateTime {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspGDateTime {
  /*< private >*/
  NspObject father;
  NspTypeGDateTime*type;
  /*< public >*/
  GDateTime *gdate;
};

extern int nsp_type_gdate_time_id;
extern NspTypeGDateTime *nsp_type_gdate_time;

/* type instances for object */

NspTypeGDateTime *new_type_gdate_time(type_mode mode);

/* instance for GDateTime */

NspGDateTime *new_gdate_time();

/*
* Object methods redefined for gdate 
*/


#define NULLGDATETIME (NspGDateTime*) 0

extern NspGDateTime *gdate_time_create(char *name,GDateTime *gdate ,NspTypeBase *type);


/* from GDateTimeObj.c */

extern NspGDateTime *nsp_gdate_time_copy(NspGDateTime *H);
extern void nsp_gdate_time_destroy(NspGDateTime *H);
extern int nsp_gdate_time_info(NspGDateTime *H, int indent,const char *name, int rec_level);
extern int nsp_gdate_time_print(NspGDateTime *H, int indent,const char *name, int rec_level);
extern int nsp_gdate_time_latex_print(NspGDateTime *H, int indent,const char *name, int rec_level);
extern NspGDateTime *nsp_gdate_time_object (NspObject *O); 
extern int IsGDateTimeObj (Stack stack, int i); 
extern int IsGDateTime(NspObject *O);
extern NspGDateTime *GetGDateTimeCopy (Stack stack, int i); 
extern NspGDateTime *GetGDateTime (Stack stack, int i); 

#endif /* NSP_INC_GDateTime */ 

#ifdef GDateTime_Private 
static int init_gdate_time(NspGDateTime *o,NspTypeGDateTime *type);
static int nsp_gdate_time_size(NspGDateTime *Mat, int flag);
static char *nsp_gdate_time_type_as_string(void);
static char *nsp_gdate_time_type_short_string(NspObject *v);
static int nsp_gdate_time_eq(NspGDateTime *A, NspObject *B);
static int nsp_gdate_time_neq(NspGDateTime *A, NspObject *B);
static int nsp_gdate_time_xdr_save(XDR  *xdrs, NspGDateTime *M);
static NspGDateTime *nsp_gdate_time_xdr_load(XDR *xdrs);
static AttrTab gdate_time_attrs[];
static NspMethods *gdate_time_get_methods(void);
static int int_gdate_time_create(Stack stack, int rhs, int opt, int lhs);
static NspGDateTime *gdate_time_create_void(char *name,NspTypeBase *type);
#if  GTK_CHECK_VERSION(2,8,0)
#define G_TYPE_DATE_MONTH (g_date_month_get_type())
static GType g_date_month_get_type(void);
#define G_TYPE_DATE_WEEKDAY (g_date_weekday_get_type())
static GType g_date_weekday_get_type(void);
#else 
#define G_TYPE_DATE_MONTH G_TYPE_NONE
#define G_TYPE_DATE_WEEKDAY G_TYPE_NONE
#endif 

#endif /* GDateTime_Private */

