#ifndef NSP_INC_ACCELERATEDTAB
#define NSP_INC_ACCELERATEDTAB

#include <nsp/nsp.h>  
#include <nsp/interf.h>   /* need the definition of function */

/* enum codes for accelerated ops: 
 * these values should be shorter than NOTCODE_OP 
 * defined in plisttoken.h 
 */

typedef enum _accelerated_ops accelerated_ops;

enum _accelerated_ops { 
  undef_tab=-1,
  concatr_tab=0, 
  concatd_tab=1,
  extract_tab=2,
  extractelts_tab=3,
  extractcols_tab=4,
  extractrows_tab=5,
  resize2vect_tab=6,
  deleteelts_tab=7,
  deletecols_tab=8,
  deleterows_tab=9,
  tozero_tab=10,
  setrowscols_tab=11};

/**
 * AcceleratedTab: 
 * @ops_id:  this should be equal to the indice in table
 * @opname: operator name 
 * @arity: operator arity 
 * @accelerated_types: table of types for which an accelerated function exists
 * @length: table lenght
 * @func: table of functions
 * 
 * structure used to store functions which code an operator for specific data types
 */

typedef struct _AcceleratedTab AcceleratedTab;

struct _AcceleratedTab
{
  const int ops_id; /* this should be equal to the indice in table */
  const char *opname;
  const int arity;
  const char **accelerated_types;
  int length;
  function **func;
} ;

extern int nsp_init_accelerated_tabs(void);
extern function *nsp_get_fast_function(const AcceleratedTab *tab, int type_id);
extern const AcceleratedTab *nsp_get_accelerated_tab(accelerated_ops tab_id);

#endif 
