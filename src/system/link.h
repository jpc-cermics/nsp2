/* Copyright INRIA */

extern void GetDynFunc ( int ii, void (**realop)());
extern int SearchInDynLinks (char *op, void (**realop)());
extern void SciLinkInit (void);
extern void  ShowDynLinks (void);
extern void RemoveInterf (int);
extern void SciLink (int iflag,int *rhs,int *ilib,char *files[],
			     char *en_names[],char *strf);

extern void C2F(iislink) (   char *buf,   integer *irep);
