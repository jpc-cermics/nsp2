static int block_get_n_locks(const NspBlock *B) ;
static int block_get_lock_status(const NspBlock *B, int i);
static int block_get_lock_link(const NspBlock *B, int i);
static void block_get_lock_pos(const NspBlock *B, int i,double pt[]);
static int block_get_lock_data(const NspBlock *B, int i) ;
static void block_set_lock_status(NspBlock *B, int i, int val);
static void  block_set_lock_link(NspBlock *B, int i,int val);
static void block_set_lock_pos(NspBlock *B, int i,const double pt[]);
static void block_set_lock_data(NspBlock *B, int i, int val);

/* 
 * n locks points 
 * for each lock point port 
 *     id of locked object (could be a pointer) 
 *     lock point id of locked object 
 *     port used for the lock point 
 *     other data ? 
 *
 * Question: faut-il une structure { object, locked_point, port, etc....}
 *     ou faut-il utiliser une structure de type matrice ? 
 *     
 *
 *
 */ 

