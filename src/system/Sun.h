/* Copyright INRIA */
#ifndef SUNSCI_PROTO
#define  SUNSCI_PROTO

#include "nsp/machine.h" 

typedef long int ftnlen ;

/*  "addinter-n.c.X1" */

void C2F(addinter)(int *descla, int *ptrdescla, int *nvla, char *iname,
		   int *desc, int *ptrdesc, int *nv, char *c_cpp, 
		   int *lib_cpp, int *err);

extern void RemoveInterf  (int Nshared);  
extern void C2F(userlk) (integer *k);  

/*  "bashos-n.c.X1" */

extern int C2F(bashos) (char *ligne, integer *n, integer *nout, integer *ierr, ftnlen ligne_len);  

/*  "basin-n.c.X1" */

extern int C2F(basin) (integer *ierr, integer *lunit, char *string, char *fmt, ftnlen string_len, ftnlen fmt_len);  

/*  "basout-n.c.X1" */
extern int C2F(basout) (integer *io, integer *lunit, char *string, ftnlen string_len);  
extern int C2F(basou1) (integer *lunit, char *string, ftnlen string_len);  
/*  "cgpath-n.c.X1" */
extern void C2F(cgpath) (char *nomfic, integer *ln);  
/*  "cluni0-n.c.X1" */
extern int C2F(cluni0) (char *name, char *nams, integer *ln, ftnlen name_len, ftnlen nams_len);  
/*  "clunit-n.c.X1" */
extern int C2F(clunit) (integer *lunit, char *name, integer *mode, ftnlen name_len);  
/*  "csignal-n.c.X1" */
extern void controlC_handler  (int sig);  
extern int C2F(csignal) (void);  
/*  "ctrlc-n.c.X1" */
extern int C2F(ctrlc) (void);  
/*  "dbasin-n.c.X1" */
extern int C2F(dbasin) (integer *ierr, integer *lunit, char *fmt, double *v, integer *iv, integer *n, ftnlen fmt_len);  
extern int C2F(s2val) (char *str, double *v, integer *iv, integer *n, integer *maxv, integer *ierr, ftnlen str_len);  
extern int C2F(nextv) (char *str, double *v, integer *nv, integer *ir, integer *ierr, ftnlen str_len);  
extern int C2F(s2int) (char *str, integer *nlz, integer *v, integer *ir, integer *ierr, ftnlen str_len);  
/*  "fgetarg-hpux-n.c.X1" */
extern int C2F(fgetarg) (integer *n, char *str, ftnlen str_len);  
/*  "fgetarg-n.c.X1" */
extern int C2F(fgetarg) (integer *n, char *str, ftnlen str_len);  
/*  "flags-n.c.X1" */
extern void set_echo_mode  (int mode);  
extern int get_echo_mode  (void);  
extern void set_is_reading  (int mode);  
extern int get_is_reading  (void);  
/*  "getenvc-n.c.X1" */

void C2F(getenvc) (int *ierr,char *var,char *buf,int *buflen,int *iflag);

/*  "getpidc-n.c.X1" */
extern int C2F(getpidc) (int *id1);  
/*  "getpro-n.c.X1" */
extern void C2F(getpro) (char *ret_val, ftnlen ret_val_len);  

/*  "inffic-n.c.X1" */
extern void C2F(inffic) (integer *iopt, char *name, integer *nc);  
extern void C2F(infficl) (integer *iopt, integer *nc);  
/*  "inibrk-n.c.X1" */
extern int C2F(inibrk) (void);  
extern int C2F(sunieee) (void);  
extern integer C2F(my_handler_) (integer *sig, integer *code, integer *sigcontext, integer *addr);  
extern integer C2F(my_ignore_) (integer *sig, integer *code, integer *sigcontext, integer *addr);  

/*  "plevel-n.c.X1" */
extern int C2F(plevel) (integer *n);  
/*  "sigbas-n.c.X1" */
extern int C2F(sigbas) (integer *n);  
/*  "systemc-n.c.X1" */
extern int C2F(systemc) (char *command, integer *stat);  
/*  "timer-n.c.X1" */
extern int C2F(timer) (double *etime);  
extern int C2F(stimer) (void);  
/*  "tmpdir-n.c.X1" */
extern void C2F(settmpdir) (void);  
extern void C2F(tmpdirc) (void);  
/*  "zzledt-n.c.X1" */
extern void C2F(zzledt) (char *buffer, int *buf_size, int *len_line, int *eof, long int dummy1);  

/* link.c */
extern int LinkStatus  (void);
extern void C2F(isciulink)(integer *i) ;


#endif /* SUNSCI_PROTO */
