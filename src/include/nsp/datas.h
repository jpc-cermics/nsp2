#ifndef SCI_DATAS 
#define SCI_DATAS 

extern NspList *Datas ;


extern int nsp_new_frame(void);
extern void nsp_frame_delete(void);
extern void nsp_frames_info(void);
extern void nsp_frame_info(void);
extern void nsp_frames_print(void);
extern void nsp_frame_print(void);
extern int nsp_frame_replace_object(NspObject *A);
extern int nsp_global_frame_replace_object(NspObject *A);
extern NspObject *nsp_frames_search_object(char *str);
extern NspObject *nsp_frame_search_object(String *str);
extern NspObject *nsp_global_frame_search_object(String *str);
extern void nsp_global_frame_remove_object(String *str);
extern NspObject *nsp_frame_search_and_remove_object(String *str);
extern void nsp_frame_remove_object(String *str);
extern int nsp_frame_search_and_move_up_object(String *str);
extern int nsp_frame_move_up_object(NspObject *O);
extern int nsp_declare_global(char *name) ;

#endif



