#ifndef SCI_MACROTAB
#define SCI_MACROTAB

extern int nsp_enter_macros(const char *dirname,int recursive,int compile);
extern int nsp_delete_macros(const char *Dir);
extern NspObject *nsp_find_macro(const char *str);
extern void nsp_init_macro_table(void) ;
extern void nsp_macro_table_reset_cache(void);

#endif
