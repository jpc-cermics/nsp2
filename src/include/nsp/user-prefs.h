#ifndef NSP_INC_USER_PREFS
#define NSP_INC_USER_PREFS

/* FIXME: 
 *   should be moved to scilab data 
 */

struct user_preferences
{
  int output_max_field_width;	/* */
  int output_precision;		/* */
  int split_long_rows;		/* */
  int print_empty_dimensions;	/* */
  int pr_as_read_syntax;	/* print in read syntax */
  int pr_depth ;        /* stop printing at given depth for recursive objects */
  int list_as_tree;     /* flag for list display */
};

extern struct user_preferences user_pref;

#endif 

