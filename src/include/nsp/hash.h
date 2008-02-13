#ifndef NSP_INC_Hash
#define NSP_INC_Hash

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* class Hash */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"

/*
 * We need a local static variable which contains the pointer to the
 * allocated memory for the hash table. An entry in this table contains
 * an ENTRY and a flag for usage.
 */

/**
 * Hash_Entry: 
 * @used: integer used to detect used locations
 * @data: #NspObject 
 * 
 * Element stored in #NspHash  
 */

typedef struct _Hash_Entry  Hash_Entry; 

struct _Hash_Entry { 
  unsigned int used; /* used to detect if data is present */
  NspObject *data;  
};

/*
 * NspHash inherits from NspObject 
 */

typedef struct _NspTypeHash NspTypeHash ;

struct _NspTypeHash { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

/**
 * NspHash: 
 * @hsize: size of hash table
 * @filled:  number of present entries
 * @htable: an array of size hsize+1.
 * 
 * inherits from #NspObject, used to store hash tables.
 */

typedef struct _NspHash NspHash;

struct _NspHash {
  /*< private >*/
  NspObject father; 
  NspTypeHash *type; 
  /*< public >*/
  unsigned int hsize,filled; /* size of hash table and number of present entries */
  void  *htable; /* an array of size hsize+1 */
};

extern int nsp_type_hash_id;
extern NspTypeHash *nsp_type_hash;

int nsp_type_hash_init();

/* only useful when building a new class derived from hash */

NspTypeHash *new_type_hash(type_mode mode);

/* initialize type for Object */

NspHash *new_hash();

/**
 * NULLHASH:
 **/

#define NULLHASH (NspHash*) 0

extern NspHash *nsp_hash_create(char *name, unsigned int size);
extern NspHash *nsp_hash_copy(NspHash *H);
extern void nsp_hash_destroy(NspHash *H);
extern int nsp_hash_info(NspHash *H, int indent,char *name, int rec_level);
extern int nsp_hash_print(NspHash *H, int indent,char *name, int rec_level);

/* from HashObj.c */

extern NspHash *nsp_hash_object (NspObject *O); 
extern int IsHashObj (Stack stack, int i); 
extern int IsHash(const NspObject *O);
extern NspHash *GetHashCopy (Stack stack, int i); 
extern NspHash *GetHash (Stack stack, int i); 

/* from file NspHash.c */ 

extern int nsp_hash_resize(NspHash *H, unsigned int new_size); 
extern int nsp_hash_merge(NspHash *H1, NspHash *H2); 
extern int nsp_hash_get_next_object(NspHash *H, int *i, NspObject **O); 
extern int nsp_hash_enter_copy(NspHash *H, NspObject *O); 
extern int nsp_hash_enter(NspHash *H, NspObject *O); 
extern void nsp_hash_remove(NspHash *H, char *str); 
extern int nsp_hash_find_and_copy(NspHash *H, char *str, NspObject **O); 
extern int nsp_hash_find(NspHash *H,const char *str, NspObject **O);
extern NspBMatrix  *nsp_hash_equal(NspHash *L1, NspHash *L2);
extern NspBMatrix  *nsp_hash_not_equal(NspHash *L1, NspHash *L2);
extern int nsp_hash_full_equal(NspHash *L1, NspHash *L2);
extern int nsp_hash_full_not_equal(NspHash *L1, NspHash *L2);

/**
 * HashOperation:
 * @H_FIND: find object 
 * @H_FIND_COPY: find object and return a copy 
 * @H_ENTER: enter an object 
 * @H_ENTER_COPY: enter a copyof an object 
 * @H_REMOVE: remove object
 *
 * used to select an operation for nsp_hsearch()
 **/

typedef enum {
  H_FIND,
  H_FIND_COPY,
  H_ENTER,
  H_ENTER_COPY,
  H_REMOVE 
} HashOperation;

extern int nsp_hsearch (NspHash *H,const char *key, NspObject **data,HashOperation action);
extern NspHash *nsp_hcreate_from_list(char *name,unsigned int nel, NspList *L);
NspHash *nsp_hcreate(char *name, unsigned int nel);
extern void nsp_hdestroy (NspHash *H);
extern NspHash *nsp_current_frame_to_hash(void);
extern NspSMatrix * nsp_hash_get_keys(NspHash *H);

#endif

#ifdef Hash_Private 
/*
 * Object methods redefined for hash 
 */
static int init_hash(NspHash *ob,NspTypeHash *type);
static int hash_size(NspHash *Mat, int flag);
static char *hash_type_as_string(void);
static char *hash_type_short_string(NspObject *v);
static int hash_eq(NspHash *A, NspObject *B);
static int hash_neq(NspHash *A, NspObject *B);
static int hash_xdr_save(XDR  *F, NspHash *M);
static NspHash  *hash_xdr_load(XDR  *F);
static NspObject *hash_path_extract(NspHash *A,int n, NspObject **Objs, int *copy);
static AttrTab hash_attrs[]; 
static int int_hash_get_attribute(Stack stack, int rhs, int opt, int lhs);
static int int_hash_set_attribute(Stack stack, int rhs, int opt, int lhs);
static NspMethods *hash_get_methods(void); 
#endif /* Hash_Private */
