#ifndef NSP_INC_USER_PREFS
#define NSP_INC_USER_PREFS

/* FIXME: 
 *   should be moved to scilab data 
 */

struct user_preferences
{
  int output_max_field_width;
  int output_precision;
  int split_long_rows;
  int print_empty_dimensions;
  int pr_as_read_syntax;
};

extern struct user_preferences user_pref;

#endif 

