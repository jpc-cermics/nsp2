#ifndef SCI_DATAS 
#define SCI_DATAS 

extern NspList *Datas ;

extern int NewFrame(void);
extern void DeleteFrame(void);
extern void FramesInfo(void);
extern void FrameInfo(void);
extern void FramesPrint(void);
extern void FramePrint(void);
extern int  FrameReplaceObj(NspObject *A);
extern int  GlobalFrameReplaceObj(NspObject *A);
extern NspObject *FramesSearchObj(char *str);
extern NspObject *FrameSearchObj(String *str);
extern NspObject *GlobalFrameSearchObj(String *str);
extern void GlobalFrameRemoveObj(String *str);
extern NspObject *FrameSearchObjAndRemove(String *str);
extern void FrameObjRemove(String *str);
extern int SearchObjAndMoveToNextFrame(String *str);
extern int MoveObjToNextFrame(NspObject *O);
extern int declare_global(char *name) ;

#endif



