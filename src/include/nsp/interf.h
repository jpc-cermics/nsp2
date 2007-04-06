#ifndef NSP_INC_INTERF
#define NSP_INC_INTERF

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include "nsp/object.h"

enum { RET_OK=0, RET_ENDFOR=-1,  RET_BREAK=-2,   RET_QUIT=-3,  RET_EOF= -4 ,  
       RET_CTRLC=-5, RET_RETURN=-6, RET_ABORT=-7, RET_CONTINUE = -8 , 
       RET_ERROR_RAISED=-9, RET_BUG=-100};

/* structure used to store the functions defined by an interface */

typedef int (function) (Stack S,int rhs,int opt,int lhs);
typedef int (function_wrapper) (Stack S,int rhs,int opt,int lhs,function *f);

typedef struct _OpTab OpTab;

struct _OpTab  {
  char *name;
  function *fonc;
};

typedef struct _OpWrapTab OpWrapTab;

struct _OpWrapTab  {
  char *name;
  function *fonc;
  function_wrapper *wrapper;
};

/* a metre ailleurs XXXXX */

extern NspSMatrix *SMatCreateFromAttrsTable (AttrTab *T); 

#define NthObj(x) (stack.val->S[stack.first+x-1])
/* current fonction name in use */
/* #define NspFname(x) (x).val->fname */
#define NspFname(x) (x).fname 
#define NspFnameH(x) (x)->fname 
/* current file name in use */
#define NspFileName(x) (x).file_name

/**
 *CheckRhs:
 * @x: an integer 
 * @y: an integer
 * 
 * macro used in interfaces to check that the number of given arguments (standards and named optionals) 
 * is in the range @x,@y
 */

#define CheckRhs(x,y) if ( rhs < x || rhs > y )				\
    { Scierror("Error: %d arguments is incorrect for function %s\n",rhs,stack.fname);return RET_BUG;} 

/**
 *CheckLhs:
 * @x: an integer 
 * @y: an integer
 * 
 * macro used in interfaces to check that the number of expected returned arguments
 * is in the range @x,@y
 */


#define CheckLhs(x,y) if ( lhs != -1 && ( lhs < x || lhs > y ))		\
    { Scierror("Error %d returned values is incorrect for function %s\n",lhs,stack.fname);return RET_BUG;}

/**
 *CheckStdRhs:
 * @x: an integer 
 * @y: an integer
 * 
 * macro used in interfaces to check that the number of given standard arguments 
 * is in the range @x,@y
 */


#define CheckStdRhs(x,y) if ( rhs -opt < x || rhs -opt > y )		\
  { Scierror("Error: %d arguments is incorrect for function %s\n",rhs-opt,stack.fname);return RET_BUG;} 

/**
 *CheckOptRhs:
 * @x: an integer 
 * @y: an integer
 * 
 * macro used in interfaces to check that the number of named optional arguments
 * is in the range @x,@y
 */

#define CheckOptRhs(x,y) if ( opt < x || opt > y )			\
  { Scierror("Error: %d optional arguments is incorrect for function %s\n",opt,stack.fname);return RET_BUG;} 

/* Used by GetArgs to decode argument list */

/**
 * int_types
 * @s_int:         scalar int 
 * @s_double:      scalar double 
 * @s_bool:        scalar bool   
 * @string:        string 
 * @stringcopy:    string copy 
 * @mat:           matrix 
 * @matcopy:       copy of matrix 
 * @realmat:       real matrix 
 * @realmatcopy:   copy of real matrix 
 * @smat:          string  matrix 
 * @smatcopy:      copy of string matrix 
 * @bmat:          boolean  matrix 
 * @bmatcopy:      copy of boolean matrix 
 * @mat_int:       matrix converted to int 
 * @matcopy_int:   copy of a matrix + int conversion 
 * @list:          a list 
 * @list_begin:    used to start a list description 
 * @list_end:      used to stop  a list description 
 * @obj:           an object 
 * @objcopy:       copy of object 
 * @obj_check:     an object preceeded by its type 
 * @hash:          a hash table 
 * @hashcopy:      a hash table copy
 * @opts:          optional arguments follow 
 * @new_opts:      optional arguments follow 
 * @t_end          end of type table  
 *
 * #int_types are used in #nsp_option to specify the 
 * expected types of arguments.
 *
 */

typedef enum _int_types{ 
  s_int,        /* scalar int */
  s_double,     /* scalar double */
  s_bool,       /* scalar bool   */
  string,       /* string */
  stringcopy,   /* string copy */
  mat,          /* matrix */
  matcopy,      /* copy of matrix */
  realmat,      /* real matrix */
  realmatcopy,  /* copy of real matrix */
  smat,         /* string  matrix */
  smatcopy,     /* copy of string matrix */
  bmat,         /* boolean  matrix */
  bmatcopy,     /* copy of boolean matrix */
  mat_int,      /* matrix converted to int */
  matcopy_int,  /* copy of a matrix + int conversion */
  list,         /* a list */
  list_begin,   /* used to start a list description */
  list_end,     /* used to stop  a list description */
  obj,          /* an object */
  objcopy,      /* copy of object */
  obj_check,    /* an object preceeded by its type */
  hash,         /* a hash table */
  hashcopy,     /* a hash table copy*/
  opts,         /* optional arguments follow */ 
  new_opts,         /* optional arguments follow */ 
  t_end         /* end of type table  */ 
} int_types;

/*  structure used for optional arguments */

/**
 * nsp_option:
 * @name: name of optional argument
 * @type: type of optional argument as a #int_types
 * @obj:   the optional argument when found
 * @position: the position in the calling stack of the optional argument when found
 * 
 * #nsp_option are used to specify types of optional arguments in 
 * interfaces.
 */

typedef struct _nsp_option nsp_option;

struct _nsp_option { 
  char *name;  /* options names */
  int_types type; /* types associated to each optional name */
  NspObject *obj;    /* array to store pointers to selected optional arguments 
		  * (set to NULL) if optional argument is not found */
  int  position;     /* stack position of arguments */
};

/* functions used in interfaces */

extern int RetArgs(Stack stack,int lhs,int_types *T,...) ;
extern int  GetArgs (Stack stack,int rhs,int opt,int_types *T,...);
extern int  GetListArgs (NspList *L,int pos,int_types *T,...); 
/* List *BuildListFromArgs_1 (int_types *T,va_list *ap); */
extern NspList *BuildListFromArgs (const char *name,int_types *T,...);
extern int  GetFromTable (NspObject **Objs,int_types *T,...);
extern int  get_optional_args(Stack stack,int rhs,int opt,nsp_option opts[],...);
extern int  get_optional_args_from_hash(Stack stack,NspHash *H,nsp_option opts[],...);
extern int  get_args_from_hash(Stack stack,NspHash *H,nsp_option opts[],...);

extern void PutLhsObj (Stack stack,int nv,int ind[]);

extern void OptCheck (NspObject **Os,NspObject **DefO,char **Names,int n,Stack ,int nopt);
extern char *ArgPosition (int i);
extern void ArgName (Stack stack, int i );

extern void SwapObjs (Stack stack,int i,int j);
extern void MoveObj  (Stack stack ,int j,NspObject *O);
extern void StackStore(Stack stack,NspObject * o,int pos);

extern int nsp_move_string(Stack stack,int n,const char *bytes,int length);
extern NspObject *nsp_new_string_obj(const char *name,const char *bytes,int length);
extern int nsp_move_double(Stack stack,int n,double d);
extern int nsp_move_doubles(Stack stack,int pos, int m,int n,...);
extern NspObject *nsp_new_double_obj(double d);
extern int nsp_move_boolean(Stack stack,int n,int ival);
extern NspObject *nsp_new_boolean_obj(int ival);

extern void ObjConvert (NspObject *O);

extern NspMod  *ModObj  ( NspObject *O);
extern NspMod *GetModCopy  (Stack S,int i);
extern NspMod *GetMod  (Stack S,int i);

extern NspMe *GetMe (Stack stack,int i);
extern NspMe *GetMeCopy (Stack stack,int i);

extern NspLmo *GetLmoCopy (Stack S,int  i);
extern NspLmo *GetLmo (Stack S,int i);

extern int GetDimArg(Stack stack, int pos, int *dim);


/**
 * CheckSameDims:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the first object in the calling stack
 * @pos2: position of the second object in the calling stack
 * @o1: a matrix 
 * @o2: a matrix 
 * 
 * Checks that both given matrices share the same dimensions
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckSameDims(fname,pos1,pos2,o1,o2) if ( o1->m != o2->m || o1->n != o2->n ) \
   { Scierror("%s: arguments %d and %d should have the same size\n",fname,pos1,pos2); \
     return RET_BUG;} 

/**
 * CheckDims:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a matrix 
 * @m1: number of expected rows 
 * @n1: number of expected columns 
 * 
 * Checks that given matrix have the expected @m1 x @n1 size.
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckDims(fname,pos1,o1,m1,n1) if ( o1->m != m1 || o1->n != n1 ) \
   { Scierror("%s: arguments %d should be of size %dx%d\n",fname,pos1,m1,n1); \
     return RET_BUG;} 

/**
 * CheckDimProp:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the first object in the calling stack
 * @pos2: position of the second object in the calling stack
 * @log_exp: an expression which 
 * 
 * Checks that @log_exp is true.
 * 
 * Returns: returns from interface with %RET_BUG if the expression is not true.
 **/

#define CheckDimProp(fname,pos1,pos2,log_exp) if ( log_exp )		\
    { Scierror("%s: arguments %d and %d have incompatible size\n",fname,pos1,pos2); \
      return RET_BUG;} 

/**
 * CheckSquare:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a matrix 
 * 
 * Checks that the given matrix is square.
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckSquare(fname,pos1,o1) if ( o1->m != o1->n ) \
   { Scierror("%s: arguments %d should be square\n",fname,pos1); \
     return RET_BUG;} 

/**
 * CheckRows:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * @value: an integer 
 * 
 * Checks that the given matrix has @value rows.
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/


#define CheckRows(fname,pos1,o1,value) if ( o1->m != value ) \
   { Scierror("%s: arguments %d has an incorrect row dimension\n",fname,pos1); \
     return RET_BUG;} 

/**
 * CheckCols:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * @value: an integer 
 * 
 * Checks that the given matrix has @value columns.
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckCols(fname,pos1,o1,value) if ( o1->n != value ) \
   { Scierror("%s: arguments %d has an incorrect col dimension\n",fname,pos1); \
     return RET_BUG;} 

/**
 * CheckLength:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * @value: an integer 
 * 
 * Checks that the given matrix has a length equal to @value.
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckLength(fname,pos1,o1,value) if ( o1->mn != value ) \
   { Scierror("%s: arguments %d should be of length %d\n",fname,pos1,value); \
     return RET_BUG;} 

/**
 * CheckScalar:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * 
 * Checks that the given matrix is a 1x1 matrix
 * 
 * Returns: 
 **/

#define CheckScalar(fname,pos1,o1) if ( o1->mn != 1 ) \
   { Scierror("%s: arguments %d should be scalar\n",fname,pos1); \
     return RET_BUG;} 


/**
 * CheckVector:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos1: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * 
 * Checks that the given matrix is a vector
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckVector(fname,pos1,o1) if ( o1->mn != 0 && o1->m != 1 && o1->n != 1 ) \
   { Scierror("%s: arguments %d should be a vector \n",fname,pos1); \
     return RET_BUG;} 

/**
 * CheckReal:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * 
 * Checks that the given matrix is real.
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/


#define CheckReal(fname,pos,o1) if ( o1->rc_type != 'r' ) \
   { Scierror("%s: arguments %d should be a real matrix\n",fname,pos1); \
     return RET_BUG;} 

/**
 * CheckComplex:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @pos: position of the object in the calling stack
 * @o1: a #NspMatrix 
 * 
 * Checks that the given matrix is complex
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckComplex(fname,pos,o1) if ( o1->rc_type != 'r' ) \
   { Scierror("%s: arguments %d should be a complex  matrix\n",fname,pos1); \
     return RET_BUG;} 


/**
 * CheckNonNegative:
 * @fname: name of the interfaced function (can use NspFname(stack))
 * @k: a value. 
 * @pos1: position of the object in the calling stack
 * 
 * Checks that the given value @k is non negative. 
 * 
 * Returns: returns from interface with %RET_BUG if the condition is not satisfied.
 **/

#define CheckNonNegative(fname, k, pos1) if ( k < 0 ) \
   { Scierror("%s: argument %d must be non negative\n",fname,pos1); \
     return RET_BUG;} 

extern int call_interf(function *f, Stack stack, int rhs, int opt, int lhs); 
extern int AllInterf(int i, int num, Stack stack, int rhs, int opt, int lhs);




#endif 

