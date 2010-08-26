#ifndef NSP_INC_BHash
#define NSP_INC_BHash

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
  
/* class BHash */

#include <stdio.h>   /* for file declaration **/
#include "nsp/sciio.h" 
#include "nsp/object.h"

/*
 * We need a local static variable which contains the pointer to the
 * allocated memory for the bhash table. An entry in this table contains
 * an ENTRY and a flag for usage.
 */

/* Element stored in the bhashtable */

typedef struct _BHash_Entry  BHash_Entry; 

struct _BHash_Entry { 
  unsigned int used; /* used to detect if data is present */
  char *key;
  int val;
};

/*
 * NspBHash inherits from NspObject 
 */

typedef struct _NspTypeBHash NspTypeBHash ;

struct _NspTypeBHash { 
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
};

/**
 * NspBHash: 
 * @hsize: size of hashtable 
 * @filled: number of elements present in the hash table 
 * @htable: array in which (key,value) pairs are stored.
 *
 * inherits from #NspObject, a basic hash table for which keys are 
 * strings and values are integers.
 */

/* typedef struct _NspBHash NspBHash; */

struct _NspBHash {
  /*< private >*/
  NspObject father; 
  NspTypeBHash *type; 
  /*< public >*/
  unsigned int hsize,filled; /* size of bhash table and number of present entries */
  BHash_Entry *htable; /* an array of size hsize+1 */
};

extern int nsp_type_bhash_id;
extern NspTypeBHash *nsp_type_bhash;

int nsp_type_bhash_init();

/* only useful when building a new class derived from bhash */

NspTypeBHash *new_type_bhash(type_mode mode);

/* initialize type for Object */

NspBHash *new_bhash();

/**
 * NULLBHASH:
 **/

#define NULLBHASH (NspBHash*) 0

extern NspBHash *nsp_bhash_create(const char *name, unsigned int size);
extern NspBHash *nsp_bhash_copy(const NspBHash *H);
extern void nsp_bhash_destroy(NspBHash *H);
extern int nsp_bhash_info(NspBHash *H, int indent,char *name, int rec_level);
extern int nsp_bhash_print(NspBHash *H, int indent,char *name, int rec_level);

/* from BHashObj.c */

extern NspBHash *nsp_bhash_object (NspObject *O); 
extern int IsBHashObj (Stack stack, int i); 
extern int IsBHash(const NspObject *O);
extern NspBHash *GetBHashCopy (Stack stack, int i); 
extern NspBHash *GetBHash (Stack stack, int i); 

/* from file NspBHash.c */ 

extern int nsp_bhash_resize(NspBHash *H, unsigned int new_size); 
extern int nsp_bhash_merge(NspBHash *H1, NspBHash *H2); 
extern int nsp_bhash_get_next_object(NspBHash *H,int *i,char **str,int *val);
extern void nsp_bhash_enter_pos_i(NspBHash *H, int i,int val);
extern int nsp_bhash_enter_copy(NspBHash *H, NspObject *O); 
extern int nsp_bhash_enter(NspBHash *H,const char *str,int val);
extern void nsp_bhash_remove(NspBHash *H, char *str); 
extern int nsp_bhash_find_and_copy(NspBHash *H, char *str, NspObject **O); 
extern int nsp_bhash_find(NspBHash *H,const char *str,int *val);
extern NspBMatrix  *nsp_bhash_equal(NspBHash *L1, NspBHash *L2);
extern NspBMatrix  *nsp_bhash_not_equal(NspBHash *L1, NspBHash *L2);
extern int nsp_bhash_full_equal(NspBHash *L1, NspBHash *L2);
extern int nsp_bhash_full_not_equal(NspBHash *L1, NspBHash *L2);
extern NspSMatrix * nsp_bhash_get_keys(const char *name,NspBHash *Hv);


/**
 * BHashOperation:
 * @H_FIND: find object 
 * @H_FIND_COPY: find object and return a copy 
 * @H_ENTER: enter an object 
 * @H_ENTER_COPY: enter a copyof an object 
 * @H_REMOVE: remove object
 *
 * used to select an operation for nsp_bhsearch)
 **/

typedef enum {
  BH_FIND,
  BH_ENTER,
  BH_REMOVE 
} BHashOperation;

/* extern int nsp_bhsearch (NspBHash *H, char *key,int *val,BHashOperation action); */
extern NspBHash *nsp_bhcreate_from_list(char *name,unsigned int nel, NspList *L);
NspBHash *nsp_bhcreate(const char *name, unsigned int nel);
extern void nsp_bhdestroy (NspBHash *H);

#endif

#ifdef BHash_Private 
/*
 * Object methods redefined for bhash 
 */
static int init_bhash(NspBHash *ob,NspTypeBHash *type);
static int bhash_size(NspBHash *Mat, int flag);
static char *bhash_type_as_string(void);
static char *bhash_type_short_string(NspObject *v);
static int bhash_eq(NspBHash *A, NspObject *B);
static int bhash_neq(NspBHash *A, NspObject *B);
static int bhash_xdr_save(XDR  *F, NspBHash *M);
static NspBHash  *bhash_xdr_load(XDR  *F);
static NspObject *bhash_path_extract(NspBHash *A,int n, NspObject **Objs, int *copy);
static AttrTab bhash_attrs[]; 
static int int_bhash_get_attribute(Stack stack, int rhs, int opt, int lhs);
static int int_bhash_set_attribute(Stack stack, int rhs, int opt, int lhs);
static NspMethods *bhash_get_methods(void); 
#endif /* BHash_Private */
