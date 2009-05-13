/* -*- Mode: C -*- */
#ifndef NSP_INC_Spawn
#define NSP_INC_Spawn

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/* Spawn */

#include "nsp/object.h"

/*
 * NspSpawn inherits from NspObject
 */

typedef struct _NspSpawn NspSpawn ;
typedef struct _NspTypeSpawn NspTypeSpawn ;

struct _NspTypeSpawn {
  /*< private >*/
  NSP_TYPE_OBJECT__
  /*< public >*/
};

typedef struct _nsp_spawn nsp_spawn;
struct _nsp_spawn {
  int pid,active,out_smat;
  NspSMatrix *out;
  int ref_count;
  nsp_string prog;
  nsp_string prompt_check;
  void *channel_in;
  
};

struct _NspSpawn {
  /*< private >*/
  NspObject father;
  NspTypeSpawn*type;
  /*< public >*/
  nsp_spawn *obj;
};

extern int nsp_type_spawn_id;
extern NspTypeSpawn *nsp_type_spawn;

/* type instances for object */

NspTypeSpawn *new_type_spawn(type_mode mode);

/* instance for Spawn */

NspSpawn *new_spawn();

/*
* Object methods redefined for spawn 
*/


#define NULLSPAWN (NspSpawn*) 0

extern NspSpawn *spawn_create(char *name,NspTypeBase *type);

/* from SpawnObj.c */

extern NspSpawn *nsp_spawn_copy(NspSpawn *H);
extern void nsp_spawn_destroy(NspSpawn *H);
extern void nsp_spawn_info(NspSpawn *H, int indent);
extern int nsp_spawn_print(NspSpawn *H, int indent);
extern NspSpawn *nsp_spawn_object (NspObject *O); 
extern int IsSpawnObj (Stack stack, int i); 
extern int IsSpawn(NspObject *O);
extern NspSpawn *GetSpawnCopy (Stack stack, int i); 
extern NspSpawn *GetSpawn (Stack stack, int i); 
int int_spawn_create(Stack stack, int rhs, int opt, int lhs);

#endif /* NSP_INC_Spawn */ 

#ifdef Spawn_Private 
static int init_spawn(NspSpawn *o,NspTypeSpawn *type);
static int nsp_spawn_size(NspSpawn *Mat, int flag);
static char *nsp_spawn_type_as_string(void);
static char *nsp_spawn_type_short_string(NspObject *v);
static int nsp_spawn_eq(NspSpawn *A, NspObject *B);
static int nsp_spawn_neq(NspSpawn *A, NspObject *B);
static AttrTab spawn_attrs[];
static NspMethods *spawn_get_methods(void);
static NspSpawn *spawn_create_void(char *name,NspTypeBase *type);
#endif /* Spawn_Private */

