#ifndef OBJXDR_SCI 
#define OBJXDR_SCI 

int MatXdrSave (NspFile *F,NspMatrix *M);
NspMatrix *MatXdrLoad (NspFile *F);
int NspPListXdrSave (NspFile *F,NspPList *M);
NspPList *NspPListXdrLoad (NspFile *F);
int PListXdrSave (NspFile *F, PList L);
int PListXdrLoad (NspFile *F, PList *plist);
int ObjXdrSave (NspFile *F, NspObject *O);
NspObject *ObjXdrLoad (NspFile *F); 

#endif
