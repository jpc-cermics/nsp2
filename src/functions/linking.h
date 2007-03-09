#ifndef SCI_LINK 
#define SCI_LINK 

/*********************************************************************
 * This Software is ( Copyright INRIA/ENPC 1998 )                    *
 *********************************************************************/

extern void GetDynFunc_obsolete ( int ii, int (**realop)());
extern int SearchInDynLinks(nsp_const_string op, int (**realop) ());
extern void  ShowDynLinks (void);

extern int nsp_is_linked(nsp_const_string name,int *ilib);
extern void SciLink(int iflag, int *rhs,int *ilib,nsp_const_string shared_path, char **en_names, char strf);
extern void SciDynLoad(nsp_const_string shared_path,char **en_names,char strf,
		       int *ilib, int iflag, int *rhs);

extern NspHash *nsp_get_dlsymbols();
extern void nsp_unlink_shared(int ilib);
extern void nsp_remove_interface(int Nshared);
extern void nsp_link_initialize(void);
extern void nsp_delete_interface_functions(int Int);

#endif 
