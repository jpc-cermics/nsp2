#ifndef SCI_SOX 
#define SCI_SOX 

#include "wav.h" 

/* Allan CORNET 18/01/2004 */
int C2F(playsound) (char *filename,unsigned long fname_len);
int C2F(loadwave) (char *filename, double *res, int *size_res, int flag,WavInfo *, int *ierr);
int C2F(savewave) (char *filename, double *res, int *rate, int *size_res, int *ierr);

void C2F(mopen) (int *fd, char *file, char *status, int *f_swap, double *res, int *error);
void C2F(mclose) (int *fd, double *res);
void C2F(meof) (int *fd, double *res);
void C2F(merror) (int *fd, double *res);
void C2F(mclearerr) (int *fd);
void C2F(mseek) (int *fd, int *offset, char *flag, int *err);
void C2F(mtell) (int *fd, double *offset, int *err);
void C2F(mput) (int *fd, double *res, int *n, char *type, int *ierr);
void C2F(mget) (int *fd, double *res, int *n, char *type, int *ierr);
void C2F(mgetstr) (int *fd, char **start, int *n, int *ierr);
void C2F(mgetstr1) (int *fd, char *start, int *n, int *ierr);
void C2F(mputstr) (int *fd, char *str, double *res, int *ierr);


#endif
