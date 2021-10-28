#ifndef NSP_INC_NSPXDR_H
#define NSP_INC_NSPXDR_H

/* prototypes of xdr functions used in nsp  */

extern int xdr_short (XDR *, short *);
extern int xdr_u_short (XDR *, u_short *);
extern int xdr_int (XDR *, int *);
extern int xdr_u_int (XDR *, u_int *);
extern int xdr_long (XDR *, long *);
extern int xdr_u_long (XDR *, u_long *);
extern int xdr_bool (XDR *, int *);
extern int xdr_opaque (XDR *, caddr_t, u_int); 

extern int xdr_char (XDR *, char *); 
/* extern int xdr_u_char (XDR *, unsigned char *); */
extern int xdr_u_char (XDR *, char *);
extern int xdr_vector (XDR *, char *, u_int, u_int, xdrproc_t);
extern int xdr_float (XDR *, float *);
extern int xdr_double (XDR *, double *);

extern int xdr_int8_t (XDR *__xdrs, int8_t *__ip) ;
extern int xdr_uint8_t (XDR *__xdrs, uint8_t *__up) ;
extern int xdr_int16_t (XDR *__xdrs, int16_t *__ip) ;
extern int xdr_uint16_t (XDR *__xdrs, uint16_t *__up) ;
extern int xdr_int32_t (XDR *__xdrs, int32_t *__ip) ;
extern int xdr_uint32_t (XDR *__xdrs, uint32_t *__up) ;
extern int xdr_int64_t (XDR *__xdrs, int64_t *__ip) ;
extern int xdr_uint64_t (XDR *__xdrs, uint64_t *__up) ;

extern void xdrmem_create (XDR *, char *, u_int, enum xdr_op);
extern void xdrstdio_create (XDR *, FILE *, enum xdr_op);
extern void xdrrec_create (XDR *, u_int, u_int, char *, int (*)(char *, char *, int), int (*)(char *, char *, int)); 
extern int xdrrec_endofrecord (XDR *, int); 

#endif /* NSP_INC_XDR_H */

