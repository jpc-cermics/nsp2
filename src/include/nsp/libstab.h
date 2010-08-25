#ifndef NSP_MACROS_H
#define NSP_MACROS_H

extern const char *nsp_get_libdir(int num);
extern int nsp_enter_macros(const char *dir_name,int recursive,int compile);
extern int nsp_delete_macros(const char *dirname);
extern NspObject *nsp_find_macro(char *str);
extern void nsp_init_macro_table(void);
extern void nsp_macro_table_reset_cache(void);
extern int nsp_enter_function(const char *str, int Int, int Num);
extern void nsp_delete_function(const char *str);
extern int nsp_find_function(const char *str, int *Int, int *Num);
extern void nsp_init_function_table(void);
extern int nsp_find_function_by_id(char *key, int Int, int Num);
extern void nsp_print_function_table(void);
extern void nsp_delete_interface_functions(int Int);

#endif
