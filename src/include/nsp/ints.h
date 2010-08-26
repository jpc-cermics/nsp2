#ifndef NSP_INC_INTS
#define NSP_INC_INTS 

/*
 * This Software is GPL (Copyright ENPC 1998-2010) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include <glib.h> 

/* all the integer types */

typedef enum   { nsp_gint, nsp_guint, nsp_gshort, nsp_gushort, nsp_glong , 
		 nsp_gulong, nsp_gint8, nsp_guint8, nsp_gint16,
		 nsp_guint16, nsp_gint32, nsp_guint32, nsp_gint64, 
		 nsp_guint64 } nsp_itype;

typedef union { 
    gint     Gint;
    guint     Guint;
    gshort     Gshort;
    gushort     Gushort;
    glong     Glong;
    gulong     Gulong;
    gint8     Gint8;
    guint8     Guint8;
    gint16     Gint16;
    guint16     Guint16;
    gint32     Gint32;
    guint32     Guint32;
    gint64     Gint64;
    guint64     Guint64;
} nsp_int_union ;

typedef union { 
    void *    Iv;
    gint     *Gint;
    guint     *Guint;
    gshort     *Gshort;
    gushort     *Gushort;
    glong     *Glong;
    gulong     *Gulong;
    gint8     *Gint8;
    guint8     *Guint8;
    gint16     *Gint16;
    guint16     *Guint16;
    gint32     *Gint32;
    guint32     *Guint32;
    gint64     *Gint64;
    guint64     *Guint64;
} nsp_int_union_ptr ;

#endif 

