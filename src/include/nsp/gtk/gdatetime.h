/* -*- Mode: C -*- */
#ifndef NSP_INC_GDateTime
#define NSP_INC_GDateTime

/*
 * This Software is GPL (Copyright ENPC 1998-2019) 
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
extern int int_gdate_time_create(Stack stack, int rhs, int opt, int lhs);
extern NspGDateTime *nsp_gdate_time_new_now(const gchar *name,const gchar *timezone);
extern NspGDateTime *nsp_gdate_time_new_now_local(const gchar *name);
extern NspGDateTime *nsp_gdate_time_new_now_utc(const gchar *name);
extern NspGDateTime *nsp_gdate_time_new_from_unix_local(const char *name,gint64 t);
extern NspGDateTime *nsp_gdate_time_new_from_unix_utc(const char *name,gint64 t);
extern NspGDateTime *nsp_gdate_time_new_local(const char *name,gint year,gint month,gint day,
					      gint hour, gint minute, gint second);
extern NspGDateTime *nsp_gdate_time_new_utc(const char *name,gint year,gint month,gint day,
					    gint hour, gint minute, gint second);

extern NspGDateTime *nsp_gdate_time_new(const char *name,gint year,gint month,gint day,
					gint hour, gint minute, gint second,
					const char *timezone);

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
static NspGDateTime *nsp_gdate_time_create_void(const char *name,NspTypeBase *type);
static NspGDateTime *nsp_gdate_time_create(const char *name,GDateTime *gdate);
#endif /* GDateTime_Private */

