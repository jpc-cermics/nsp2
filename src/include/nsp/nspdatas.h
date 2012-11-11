#ifndef NSP_INC_NSPDATAS_H
#define NSP_INC_NSPDATAS_H 

#include <nsp/nspthreads.h> 

/*
 * Used to store the calling frames for function invocation 
 * It is a list of list each list is called a frame here 
 */

typedef struct _nsp_datas nsp_datas; 
struct _nsp_datas {
  NspList   *L;
  NspObject *Reserved;      /* used to fill stack with non empty object */
  NspObject *Null;          /* Direct access to %null */
  NspFrame  *GlobalFrame;   /* Direct access to GlobalFrame */
  NspFrame  *ConstantFrame; /* Direct access to constants */
  NspFrame  *TopFrame;      /* Direct access to top */
};

#ifndef NSP_WITH_MAIN_GTK_THREAD
extern NspList   *Datas; /* contains the list of frames */
extern NspObject *Reserved;/* used to fill stack with non empty object */
extern NspObject *Null;   /* Direct access to %null */
extern NspFrame  *GlobalFrame; /* Direct access to GlobalFrame */
extern NspFrame  *ConstantFrame; /* Direct access to constants */
extern NspFrame  *TopFrame; /* Direct access to top */
#endif

#endif
