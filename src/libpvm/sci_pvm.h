#ifndef NSP_H__PVM 
#define NSP_H__PVM 

extern int nsp_pvm_send(int *tids,int ntids,NspObject *M,int msgtag);
extern int nsp_pvm_recv(int tid,int tag,NspObject **M);
extern int  nsp_pvm_pkmatrix(NspMatrix *M);
extern int nsp_pvm_upkmatrix(NspMatrix **M);
extern int nsp_pvm_start(char *hostfile);
extern int nsp_pvm_halt(void);

#endif 
