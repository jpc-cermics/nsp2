#ifndef INC_NSP_Hash
#define INC_NSP_Hash

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/
  
/* class Hash */

#include <stdio.h>   /** for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"


/*
 * We need a local static variable which contains the pointer to the
 * allocated memory for the hash table. An entry in this table contains
 * an ENTRY and a flag for usage.
 */

/** Element stored in the hashtable **/

typedef struct { 
  unsigned int   used; /* used to detect if data is present 
			* we could remove this and use data == NULL 
			*/ 
  NspObject *data;  
} Hash_Entry ;


/*
 * NspHash inherits from NspObject 
 */

typedef struct _nsp_hash NspHash;

typedef int (*hash_save) (NspFile  *F, NspHash *M);

typedef struct _nsp_type_Hash { 
  NSP_TYPE_OBJECT__ 
  /* rajouts */
} NspTypeHash;

struct _nsp_hash {
  NspObject father; 
  NspTypeHash *type; 
  unsigned int hsize,filled;
  void  *htable;
};

extern int nsp_type_hash_id;
extern NspTypeHash *nsp_type_hash;

int nsp_type_hash_init();

/* only useful when building a new class derived from hash */

NspTypeHash *new_type_hash(type_mode mode);

/* initialize type for Object */

NspHash *new_hash();

/*
 * Object methods redefined for hash 
 */

#ifdef Hash_Private 
static int init_hash(NspHash *ob,NspTypeHash *type);
static int hash_size(NspHash *Mat, int flag);
static char *hash_type_as_string(void);
static char *hash_type_short_string(void);
static int hash_eq(NspHash *A, NspObject *B);
static int hash_neq(NspHash *A, NspObject *B);
static int hash_xdr_save(NspFile  *F, NspHash *M);
static NspHash  *hash_xdr_load(NspFile  *F);
static NspObject *hash_path_extract(NspHash *A, NspObject *O);
static AttrTab hash_attrs[]; 
static int int_hash_get_attribute(Stack stack, int rhs, int opt, int lhs);
static int int_hash_set_attribute(Stack stack, int rhs, int opt, int lhs);
static NspMethods *hash_get_methods(void); 
#endif /* Hash_Private */

#define NULLHASH (NspHash*) 0

NspHash *nsp_hash_create(char *name, unsigned int size);
NspHash *hash_copy(NspHash *H);
void nsp_hash_destroy(NspHash *H);
void hash_info(NspHash *H, int indent);
void hash_print(NspHash *H, int indent);

/* from HashObj.c */

extern NspHash *hash_object (NspObject *O); 
extern int IsHashObj (Stack stack, int i); 
extern int IsHash(NspObject *O);
extern NspHash *GetHashCopy (Stack stack, int i); 
extern NspHash *GetHash (Stack stack, int i); 

/* from file NspHash.c */ 

extern NspHash *HashCreate (char *name, unsigned int size); 
extern NspHash *HashCopy (NspHash *H); 
extern int nsp_hash_resize(NspHash *H, unsigned int new_size); 
extern int nsp_hash_merge(NspHash *H1, NspHash *H2); 
extern void HashDestroy (NspHash *H); 
extern void HashInfo (NspHash *H, int indent); 
extern void HashPrint (NspHash *H, int indent); 
extern int nsp_hash_get_next_object(NspHash *H, int *i, NspObject **O); 
extern int nsp_hash_enter_copy(NspHash *H, NspObject *O); 
extern int nsp_hash_enter(NspHash *H, NspObject *O); 
extern void nsp_hash_remove(NspHash *H, char *str); 
extern int nsp_hash_find_and_copy(NspHash *H, char *str, NspObject **O); 
extern int nsp_hash_find(NspHash *H, char *str, NspObject **O);
extern NspBMatrix  *nsp_hash_equal(NspHash *L1, NspHash *L2);
extern NspBMatrix  *nsp_hash_not_equal(NspHash *L1, NspHash *L2);
extern int nsp_hash_full_equal(NspHash *L1, NspHash *L2);
extern int nsp_hash_full_not_equal(NspHash *L1, NspHash *L2);


/* from file HashObj.c */ 

extern NspObject *HashPathExtract (NspHash *H, NspObject *O); 
extern NspObject *HashLoopExtract (char *str, NspObject *O, NspObject *O1, int i, int *rep); 
/* for Hash.c */

typedef enum {
  H_FIND,H_FIND_COPY,H_ENTER,H_ENTER_COPY,H_REMOVE
} HashOperation;
 
extern int nsp_hsearch (NspHash *H,char *key, NspObject **data,HashOperation);
extern NspHash *nsp_hcreate (char *str,  unsigned nel);
extern void nsp_hdestroy (NspHash *H);



#endif

