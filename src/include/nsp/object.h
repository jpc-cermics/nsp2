#ifndef NSP_INC_TYPE_OBJECT 
#define NSP_INC_TYPE_OBJECT 

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

/*
 * base type (NspTypeObject) 
 */

#include "nsp/math.h" 

typedef struct  _NspObject  NspObject; 
typedef struct  _AttrTab AttrTab;
typedef struct  _NspMethods NspMethods;

typedef void (*print_fun)(void *); 
typedef unsigned int NspTypeId ;   
typedef void  (print_func) (void *,int,int);
typedef void  (dealloc_func) (void *);
typedef void* (copy_func) (void *);
typedef int   (size_func) (void *,int );
typedef char* (s_type_func) (void);
typedef char* (sh_type_func) (void *);
typedef void* (info_func) (void *,int );
typedef char* (set_name_func) (void *,char *);
typedef char* (get_name_func) (void *);
typedef int   (is_true_func)  (void *);
typedef void *(loop_func) (char *str,void *,void *,int i,int *rep);
typedef void *(path_func) (void *,void *);
typedef void *(get_from_obj_func) (void *);
typedef int (eq_func) (void *,void *);
typedef int (save_func) (void *,void *);
typedef NspObject *(load_func) (void *);
typedef int (init_func) (void *,void *);
/* typedef AttrTab *(attrs_func) (void );*/
typedef NspMethods *(methods_func) (void );
typedef void *(new_func) (void);
typedef void *(attrs_func) (void);
typedef void *(create_func) (void);

typedef struct _NspTypeBase  NspTypeBase ;

#define NSP_TYPE_OBJECT__ \
  NspTypeId id ;                      /* each type has a unique id */ \
  NspTypeBase *surtype;               /* type of parent */ \
  NspTypeBase *interface ;  	      /* chained types for interfaces */ \
  init_func *init ;		      /* initializer */ \
  new_func *new ;		      /* allocations */ \
  AttrTab *attrs; 		      /* attribute table */ \
  attrs_func *get_attrs;	      /* get attribute wrapper */ \
  attrs_func *set_attrs;	      /* get attribute wrapper */ \
  methods_func *methods;	      /* methods */ \

struct _NspTypeBase {
  NSP_TYPE_OBJECT__ 
} ;

typedef struct _NspTypeObject NspTypeObject;

struct _NspTypeObject {
  /*< private >*/
  NSP_TYPE_OBJECT__ 
  /*< public >*/
  print_func *pr ;		      /* printing*/   
  dealloc_func *dealloc;              /* dealloc */  
  copy_func *copy ;                   /* copy object */  
  size_func *size ;                   /* m,n or m*n  */  
  s_type_func *s_type;                /* type as a String */  
  sh_type_func *sh_type;              /* type as a short string */  
  info_func *info;                    /* info */  
  set_name_func *set_name;            /* set name */  
  get_name_func *get_name;            /* get name */  
  is_true_func  *is_true;             /* check if object can be considered as true */  
  loop_func     *loop;                /* for loops */  
  path_func     *path_extract;        /* used for x(1)(2)(...) */  
  get_from_obj_func *get_from_obj;    /* get object stored in SciObj */  
  eq_func *eq ;                       /* equality check */  
  eq_func *neq ;                      /* non-equality check */
  save_func *save;                    /* file save */
  load_func *load;                    /* file load */
  create_func *create;	      	      /* creates a new object  */ 
};


/* cast a type instance to base type */

#define NSP_TYPE_OBJECT(t) ((NspTypeObject *) t) 
#define NSP_TYPE_BASE(t) ((NspTypeBase *) t) 

/* checking the type of objects is done through 
 * unique id; we keep a list of type to get type 
 * from type id 
 */

extern NspTypeId  nsp_new_type_id(void);

typedef struct _registered_types registered_types ;

struct _registered_types {
  NspTypeObject  *type;
  struct _registered_types *next;
};

extern registered_types *nsp_types;
extern int nsp_register_type(void *type);
extern int nsp_no_type_id; /* this can no be a type id : used in save/load */
extern void *nsp_get_type_from_id(NspTypeId id); 

/* used in type constructors */ 

typedef enum { T_BASE, T_DERIVED } type_mode;

/*-------------------------------------------------------
 * base Objet (NspObject): 
 *------------------------------------------------------*/

/* typedef struct  _nsp_object  NspObject; */

struct  _NspObject {
  char *name;			/* object name: must be first */
  NspTypeObject *type;
  NspTypeBase *basetype;        /* type of base child  */
  int  ret_pos ;                /* used to store return position from an interface */ 
};

extern int nsp_type_object_id ;
extern NspTypeObject  *nsp_type_object;
int nsp_type_object_init();

/* cast to the top of hierarchy */

#define NSP_OBJECT(o) ((NspObject *) o) 

/* return a new instance of struct NspTypeObject. This is used 
 * when building a type for objects derived from NspObject 
 * nsp_type_object_id when # 0 contains the unique id of NspTypeObject 
 * nsp_type_object is a special instance of NspTypeObject which is 
 * found in the type filed of a NspObject. 
 */

NspTypeObject *new_type_object(type_mode mode);

/* utility function used for inheritance  */

void nsp_type_object_set(NspObject *o,NspTypeObject *type);

/* new object */ 

NspObject  *new_object(void);

/* initialize type for Object */

int nsp_type_object_init(void); 

/* is it safe to cast instance o to object with id as type id */

int check_cast(void *obj,NspTypeId id); 

/* instance o implements interface with id as type */

NspTypeBase *check_implements(void *obj,NspTypeId id); 

/*-----------------------------------------------------------
 * An array of Object
 * we want to pass Stack by value 
 *-----------------------------------------------------------*/

typedef struct _Stack Stack;

typedef void stack_error(Stack *S,char *fmt,...);

struct _Stack {
  char *fname; /* function currently evaluated **/
  char *file_name ; /* current evaluated file **/
  int first;   /* position of first argument to be used **/
  NspObject **D;     /* D is dynamically changed so that D[1] is the first Objet used XXX **/
  NspObject **L;     /* Last position **/
  NspObject **S;     /* points to the whole stack **/
  NspObject *error_msg; 
  stack_error *error;
  int errcatch; 
  int pause; 
} ;

#define STACK_SIZE 50000
extern NspObject *S[STACK_SIZE];    /* this could be malloced XXX **/
extern Stack SciStack ;

extern void StackInfo (void); 
extern void InitStack (void); 
extern void nsp_init_stack(Stack *stack,NspObject **S);

/*-----------------------------------------------------------
 * Object attributes 
 *-----------------------------------------------------------*/

typedef NspObject * (attr_get_function) (void *o, char *attr);
typedef NspObject * (attr_get_object_function) (void *o,char *str);
typedef int (attr_set_function) (void *Hv,char *attr, NspObject *val);

extern NspObject * int_get_failed(NspObject *self, char *attr);
extern NspObject * int_get_object_failed(NspObject *self, char *attr);
extern int int_set_failed(NspObject *self,char *attr, NspObject *val);

struct _AttrTab {
  char *name;
  attr_get_function *get;
  attr_set_function *set;
  attr_get_object_function *get_object;
};

/* utility function for attributes */

extern int attr_search (char *key,AttrTab Table[]);
extern int int_check_attr (char *key,AttrTab attrs[],Stack stack,int rhs,int opt,int lhs);
extern int attrs_to_stack (char *key,AttrTab attrs[],Stack stack,int pos);

extern int set_attribute_util(NspObject *ob, NspTypeBase *type, char *attr,NspObject *val);
extern int int_set_attribute(Stack stack, int rhs, int opt, int lhs);
extern int int_set_attributes(Stack stack, int rhs, int opt, int lhs);
extern int int_set_attributes1(void *Ob,Stack stack, int rhs, int opt, int lhs);
extern int int_create_with_attributes(NspObject *ob,Stack stack, int rhs, int opt, int lhs);
extern int nsp_set_attribute_util(NspObject *ob, NspTypeBase *type, char *attr,NspObject *val);
extern int int_get_attribute(Stack stack, int rhs, int opt, int lhs);
extern NspObject *nsp_get_attribute_util(NspObject *ob,NspTypeBase *type,char *attr) ;
extern NspObject *nsp_get_attribute_object(NspObject *ob,NspTypeBase *type, char *attr) ;
extern NspObject *object_path_extract(NspObject *a, NspObject *ob);


/*-----------------------------------------------------------
 * Object methods 
 *-----------------------------------------------------------*/

typedef int nsp_method(void *o,Stack stack,int rhs,int opt,int lhs);

struct _NspMethods {
  char *name;
  nsp_method *meth; 
};

extern int method_search(char *key, NspMethods *Table);
int nsp_exec_method_util(NspObject *ob,NspTypeBase *type,char *method, Stack stack, int rhs, int opt, int lhs);

/*----------------------------------------------------------
 NOOBJ     : used for load/save 
 LIST      : Scilab Lists 
 MATRIX    : usual matrices 
 SMATRIX   : matrix of string 
 BMATRIX   : boolean matrix 
 LIB       : library 
 PMATRIX   : Matrix of polynoms 
 SPMATRIX  : Sparse Matrix 
 P_PLIST   : Scilab Parsed Expressions 
 HASH      : Scilab Hash Table  
 HOBJ     : Pointer to a Scilab Obj 
 FUNC     : Scilab internal function i.e inside an interface 
 IVECT    : Scilab implicit vectors 
 SCIFILE  : file 
 GTKE     : Gtk/gdk object 
 VOIDPT   : void * to a C object 
 ARRAY  : 
 MODULE :   internal use for modules 
 ME     :   internal use for modules elts 
 LMO    :   internal usr list of modules 
 *----------------------------------------------------------*/
/* 

typedef enum { NOOBJ,LIST,MATRIX,SMATRIX,BMATRIX,LIB,
	       PMATRIX,SPMATRIX,P_PLIST,HASH,HOBJ,FUNC,
	       IVECT,SCIFILE,GTKE,VOIDPT,ARRAY,MODULE,ME,LMO,RECT,GFRAME,BLOCK,LINK
} SciType;
*/

#include "nsp/plisttoken.h"
#include "nsp/file.h"
#include "nsp/matrix.h"
#include "nsp/smatrix.h"
#include "nsp/spmatrix.h"
#include "nsp/pmatrix.h"
#include "nsp/bmatrix.h"
#include "nsp/plist.h"
#include "nsp/list.h"
#include "nsp/hash.h"
#include "nsp/hobj.h"
#include "nsp/function.h"
#include "nsp/ivect.h"
#include "nsp/mod.h" 
#include "nsp/me.h" 
#include "nsp/lmo.h" 
#include "nsp/rect.h" 
#include "nsp/block.h" 
#include "nsp/connector.h" 
#include "nsp/gframe.h" 
#include "nsp/link.h" 
#include "nsp/none.h" 
#include "nsp/type.h" 
#include "nsp/module.h"
#include "nsp/modulelt.h"
#include "nsp/classa.h"
#include "nsp/classb.h"
#include "nsp/cells.h"

/*----------------------------------------------------------
 * A set of prototypes 
 *----------------------------------------------------------*/

void nsp_void_object_destroy(NspObject **O);

#define Ocheckname(x,y) ( strcmp( NSP_OBJECT(x)->name,y)==0 ) 

NspObject *nsp_create_empty_matrix_object(char *str);
NspObject *nsp_create_object_from_doubles( int m,int n,int it,double *rtab,double *itab,char *name);
void *MaybeObjCopy (NspObject **O);
NspObject *nsp_object_copy_and_name(char *name,NspObject *O);

int nsp_object_xdr_save(NspFile *F, NspObject *O);
NspObject *nsp_object_xdr_load(NspFile *F); 
extern void nsp_object_destroy(NspObject **O); 
extern void nsp_void_object_destroy(NspObject **O); 
extern NspObject *nsp_object_copy(NspObject *O); 
extern int nsp_object_get_size(NspObject *O, int j); 
extern NspObject *nsp_object_copy_with_name(NspObject *O); 
extern NspObject *nsp_object_copy_and_name(char *name, NspObject *O); 
extern char *nsp_object_type_as_string(NspObject *O); 
extern char *nsp_object_type_short(NspObject *O); 
extern int nsp_object_type(NspObject *O, NspTypeId id); 
extern int nsp_object_implements(NspObject *O, NspTypeId id); 
extern void nsp_object_info(NspObject *O, int indent); 
extern void nsp_object_print(NspObject *O, int indent); 
extern int nsp_object_is_true(NspObject *O); 
extern NspObject *nsp_object_loop_extract(char *str, NspObject *O, NspObject *O1, int i, int *rep); 
extern NspObject *def_loop (char *str, NspObject *O, NspObject *O1, int i, int *rep); 
extern NspObject *nsp_get_object(Stack stack, int i); 
extern NspObject *nsp_get_object_copy(Stack stack, int i);
extern NspObject *nsp_create_object_from_double(char *str, double dval); 
extern NspObject *nsp_create_object_from_int(char *str, int ival); 
extern NspObject *nsp_complexi_object_(char *str); 
extern NspObject *nsp_create_object_from_str(char *str); 
extern NspObject *nsp_create_object_from_str_and_size(char *str, int lstr); 
extern NspObject *nsp_create_object_from_doubles(int m, int n, int it, double *rtab, double *itab, char *name); 
extern NspObject *nsp_create_empty_matrix_object(char *str); 
extern NspObject *nsp_create_true_object(char *str); 
extern NspObject *nsp_create_boolean_object(char *str,int val);
extern NspObject *nsp_create_false_object(char *str); 
extern char *nsp_object_get_name(NspObject *O); 
extern int nsp_object_set_name(NspObject *O, char *str); 
extern int print_count_rows(Stack stack,int first_arg,int last_arg);

#endif /*  NSP_TYPE_OBJECT  */









  
  
  

  




