#ifndef NSP_INC_PA 
#define NSP_INC_PA

/*
 * This Software is GPL (Copyright ENPC 1998-2005) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */

#include <stdio.h>   /* for file declaration **/
#include <portaudio.h>
#include "nsp/sciio.h" 
#include "nsp/object.h"
#include "nsp/xdr.h"

/*
 *
 */

typedef struct _nsppa nsppa;

struct _nsppa {
  PaStream *ostream; 
  int o_device;    /* play on o_device or default device if -1 */
  int err;         /* an error occured */
  int sample_rate; /* sample rate */
  int channels; 
  int refcount;
};

/*
 * NspPa inherits from NspObject 
 */

typedef struct _NspPa NspPa;

typedef struct _NspTypePa { 
  NSP_TYPE_OBJECT__ 
  /*< public >*/
} NspTypePa;

struct _NspPa {
  /*< private >*/
  NspObject father; 
  NspTypePa *type; 
  /*< public >*/
  nsppa *pa;
};

extern int nsp_type_pa_id;
extern NspTypePa *nsp_type_pa;

NspTypePa *new_type_pa(type_mode mode);

NspPa *nsp_new_pa();

/*
 * Object methods redefined for file 
 */

#ifdef Pa_Private 
static int nsp_init_pa(NspPa *ob,NspTypePa *type);
static int nsp_pa_size(NspPa *Mat, int flag);
static char *nsp_pa_type_as_string(void);
static char *nsp_pa_type_short_string(NspObject *v);
static int nsp_pa_eq(NspObject *A, NspObject *B);
static int nsp_pa_neq(NspObject *A, NspObject *B);
static NspMethods *nsp_pa_get_methods(void);
static AttrTab nsp_pa_attrs[];
#endif 

extern NspPa *nsp_pa_create(char *name);
extern NspPa *nsp_pa_copy(NspPa *H);
extern void nsp_pa_destroy(NspPa *H);
extern void nsp_pa_info(NspPa *H, int indent,char *name, int rec_level);
extern int nsp_pa_print(NspPa *H, int indent,char *name, int rec_level);

#define NULLPA (NspPa *) 0

/* PaObj.c */

extern NspPa *nsp_pa_object(NspObject *O);
extern int IsPaObj (Stack stack, int i);
extern NspPa *GetPaCopy (Stack stack, int i);
extern NspPa *GetPa (Stack stack, int i);

extern int nsp_play_file(const char *file,int sync,int device);
extern int nsp_play_data(NspMatrix *M,int sample_rate,int sync,int device);
extern int nsp_play_data_no_cb(NspMatrix *M,int sample_rate, int sync,int device);
extern int nsp_record_data(NspMatrix **M,int seconds,int sample_rate,int channels, 
			   int device,int o_device);

typedef enum { NSP_PA_ACTIVE , NSP_PA_END,  NSP_PA_INACTIVE } nsp_pa_status;


gint timeout_portaudio (void *data);
extern void controlC_handler(int sig);
extern void controlC_handler_portaudio(int sig);
extern void nsp_pa_stop(void);
extern void nsp_finish_pa_thread();
extern void nsp_pa_thread_set_status( nsp_pa_status st);
extern nsp_pa_status nsp_pa_thread_get_status();


#endif 
