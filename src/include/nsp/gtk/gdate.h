/* -*- Mode: C -*- */
#ifndef NSP_INC_GDate
#define NSP_INC_GDate

/*
 * This Software is GPL (Copyright ENPC 1998-2007) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* GDate */

#include "nsp/object.h"

/*
 * NspGDate inherits from NspObject
 */

typedef struct _NspGDate NspGDate ;
typedef struct _NspTypeGDate NspTypeGDate ;

struct _NspTypeGDate {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

struct _NspGDate {
  /*< private >*/
  NspObject father;
  NspTypeGDate*type;
  /*< public >*/
  GDate *gdate;
};

extern int nsp_type_gdate_id;
extern NspTypeGDate *nsp_type_gdate;

/* type instances for object */

NspTypeGDate *new_type_gdate(type_mode mode);

/* instance for GDate */

NspGDate *new_gdate();

/*
* Object methods redefined for gdate 
*/


#define NULLGDATE (NspGDate*) 0

extern NspGDate *gdate_create(char *name,GDate *gdate ,NspTypeBase *type);


/* from GDateObj.c */

extern NspGDate *nsp_gdate_copy(NspGDate *H);
extern void nsp_gdate_destroy(NspGDate *H);
extern int nsp_gdate_info(NspGDate *H, int indent,const char *name, int rec_level);
extern int nsp_gdate_print(NspGDate *H, int indent,const char *name, int rec_level);
extern int nsp_gdate_latex_print(NspGDate *H, int indent,const char *name, int rec_level);
extern NspGDate *nsp_gdate_object (NspObject *O); 
extern int IsGDateObj (Stack stack, int i); 
extern int IsGDate(NspObject *O);
extern NspGDate *GetGDateCopy (Stack stack, int i); 
extern NspGDate *GetGDate (Stack stack, int i); 

#endif /* NSP_INC_GDate */ 

#ifdef GDate_Private 
static int init_gdate(NspGDate *o,NspTypeGDate *type);
static int nsp_gdate_size(NspGDate *Mat, int flag);
static char *nsp_gdate_type_as_string(void);
static char *nsp_gdate_type_short_string(NspObject *v);
static int nsp_gdate_eq(NspGDate *A, NspObject *B);
static int nsp_gdate_neq(NspGDate *A, NspObject *B);
static int nsp_gdate_xdr_save(XDR  *xdrs, NspGDate *M);
static NspGDate *nsp_gdate_xdr_load(XDR *xdrs);
static AttrTab gdate_attrs[];
static NspMethods *gdate_get_methods(void);
static int int_gdate_create(Stack stack, int rhs, int opt, int lhs);
static NspGDate *gdate_create_void(char *name,NspTypeBase *type);
#if  GTK_CHECK_VERSION(2,8,0)
#define G_TYPE_DATE_MONTH (g_date_month_get_type())
static GType g_date_month_get_type(void);
#define G_TYPE_DATE_WEEKDAY (g_date_weekday_get_type())
static GType g_date_weekday_get_type(void);
#else 
#define G_TYPE_DATE_MONTH G_TYPE_NONE
#define G_TYPE_DATE_WEEKDAY G_TYPE_NONE
#endif 

#endif /* GDate_Private */

