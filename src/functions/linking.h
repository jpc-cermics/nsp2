
#ifndef SCI_LINK 
#define SCI_LINK 

/*********************************************************************
 * This Software is ( Copyright INRIA/ENPC 1998 )                    *
 *********************************************************************/

extern void GetDynFunc ( int ii, int (**realop)());
extern int SearchInDynLinks (char *op, int (**realop)());
extern void SciLinkInit (void);
extern void  ShowDynLinks (void);
extern void SciLink (int iflag,int *rhs,int *ilib,char *files[],
			     char *en_names[],char *strf);

extern void SciDynLoad (char **files,char **en_names,char *strf,
				int *ilib,int iflag,int *rhs);

extern void C2F(iislink) (   char *buf,   integer *irep);
extern void C2F(isciulink) ( integer *i);
extern int LinkStatus  (void);

void AddInter (char **files, char *iname, char **enames, int *err);
int BlankInterface (int i, char *fname, int first, int rhs, int opt, int lhs);
void RemoveInterf (int Nshared);

#endif 
