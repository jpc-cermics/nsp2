#ifndef NSP_INC_USER_PREFS
#define NSP_INC_USER_PREFS

/* FIXME: 
 *   should be moved to scilab data 
 */

typedef enum { print_std, print_latex, print_latextab } nsp_print_mode;

typedef struct _user_preferences  user_preferences;

struct _user_preferences
{
  int output_max_field_width;	/* */
  int output_precision;		/* */
  int split_long_rows;		/* */
  int print_empty_dimensions;	/* */
  int pr_as_read_syntax;	/* print in read syntax */
  int pr_depth ;        /* stop printing at given depth for recursive objects */
  int list_as_tree;     /* flag for list display */
  nsp_print_mode mode;  /* std, latex or latex tables */
  int active ;          /* only print if active is true */
};

extern user_preferences user_pref;

#endif 

