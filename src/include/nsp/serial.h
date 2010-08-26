/* -*- Mode: C -*- */
#ifndef NSP_INC_Serial
#define NSP_INC_Serial

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* Serial */

#include "nsp/object.h"

/*
 * NspSerial inherits from NspObject
 */

/**
 * NspSerial: 
 * @val: character array used to store the serialized object
 * @nbytes: size of the serialized object.
 * 
 * inherits from #NspObject, used to store serialized objects.
 */


/* typedef struct _NspSerial NspSerial; */
typedef struct _NspTypeSerial NspTypeSerial;

typedef int (*serial_save) (XDR  *xdrs, NspSerial *M);

struct _NspTypeSerial { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

struct _NspSerial {
  /*< private >*/
  NspObject father; 
  NspTypeSerial *type; 
  /*< public >*/
  char *val;
  int nbytes;
};

extern const char nsp_serial_header[];
extern const char nsp_zserial_header[];

extern int nsp_type_serial_id;
extern NspTypeSerial *nsp_type_serial;

/* type instances for classa */

NspTypeSerial *new_type_serial(type_mode mode);

/* instance for Serial */

NspSerial *new_serial();

/*
 * Object methods redefined for serial 
 */

#define NULLSERIAL (NspSerial*) 0

extern NspSerial *nsp_serial_create(const char *name,const char *buf,int nbytes);
extern NspSerial *nsp_serial_copy(const NspSerial *H);
extern void nsp_serial_destroy(NspSerial *H);
extern int nsp_serial_info(NspSerial *H, int indent,const char *name, int rec_level);
extern int nsp_serial_print(NspSerial *H, int indent,const char *name, int rec_level);

extern NspSerial *nsp_serial_object (NspObject *O); 
extern int IsSerialObj (Stack stack, int i); 
extern int IsSerial(NspObject *O);
extern NspSerial *GetSerialCopy (Stack stack, int i); 
extern NspSerial *GetSerial (Stack stack, int i); 

extern NspObject *nsp_object_unserialize(const NspSerial *S);
extern NspObject * nsp_object_serialize(const NspObject *O);

extern NspMatrix *nsp_serial_to_matrix(const NspSerial *S);
extern NspSerial *nsp_matrix_to_serial(const NspMatrix *A);

extern NspSerial *nsp_serial_compress(const NspSerial *S);
extern NspSerial *nsp_serial_uncompress(const NspSerial *Sz);


#endif 

/* private part */

#ifdef Serial_Private 
static NspSerial *_nsp_serial_create(const char *name,const char *buf,int nbytes,NspTypeBase *type,int zflag);
static int init_serial(NspSerial *o,NspTypeSerial *type);
static int nsp_serial_size(NspSerial *Mat, int flag);
static char *nsp_serial_type_as_string(void);
static char *nsp_serial_type_short_string(NspObject *v);
static int nsp_serial_eq(NspSerial *A, NspObject *B);
static int nsp_serial_neq(NspSerial *A, NspObject *B);
static int nsp_serial_xdr_save(XDR *xdrs, NspSerial *M);
static NspSerial  *nsp_serial_xdr_load(XDR *xdrs);
static AttrTab serial_attrs[];
static NspMethods *serial_get_methods(void); 
/* static int int_serial_create(Stack stack, int rhs, int opt, int lhs); */
#endif /* Serial_Private */
