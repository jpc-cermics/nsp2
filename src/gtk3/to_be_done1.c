#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <nsp/object.h>
#include <nsp/hash.h>
#include <nsp/matrix.h>

#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>

static int add_constants(void);
extern void nsp_initialize_atk_types(void);
extern void nsp_initialize_cairo_types(void);
extern void nsp_initialize_gdk_types(void);
extern void nsp_initialize_gio_types(void);
extern void nsp_initialize_glib_types(void);
extern void nsp_initialize_gparamspec_types(void);
#ifdef WITH_GI
extern void nsp_initialize_girepository_types(void);
#endif 
extern void nsp_initialize_gtk_types(void);
extern void nsp_initialize_gvalue_types(void);
extern void nsp_initialize_pango_types(void);
extern void nsp_initialize_webkit_types(void);

extern void cairo_add_constants(NspObject *module, const gchar *strip_prefix);
extern void gio_add_constants(NspObject *module, const gchar *strip_prefix);
extern void glib_add_constants(NspObject *module, const gchar *strip_prefix);


/* init nsp gtk object types */

void nsp_init_gtk_types(void)
{
#if GLIB_CHECK_VERSION(2,36,0)
  /* deprecated g_type_init */
#else
  g_type_init();
#endif
  nsp_initialize_atk_types();
  nsp_initialize_cairo_types();
  nsp_initialize_gdk_types();
  nsp_initialize_gio_types();
  nsp_initialize_glib_types();
  nsp_initialize_gparamspec_types();
#ifdef WITH_GI
  nsp_initialize_girepository_types();
#endif
  nsp_initialize_gtk_types();
  /* nsp_initialize_gvalue_types(); */
  nsp_initialize_pango_types();
#ifdef  HAVE_WEBKIT
  nsp_initialize_webkit_types();
#endif
  add_constants();
}

/* more types
 */

void nsp_init_gtk_types_added(void)
{
}

/*
 * registering constants in an Nsp Hash Table
 */

NspHash *nsp_gtk_hash_table = NULL;
NspHash *nsp_gdk_hash_table = NULL;
NspHash *nsp_atk_hash_table = NULL;
NspHash *nsp_pango_hash_table = NULL;
NspHash *nsp_cairo_hash_table = NULL;
NspHash *nsp_gio_hash_table = NULL;
NspHash *nsp_glib_hash_table = NULL;



#define PANGO_ENTER(name,value) \
  if (( nsp_val = (NspObject *) nsp_matrix_create_from_doubles(name,1,1,value))== NULL) return FAIL; \
  if (nsp_hash_enter(nsp_pango_hash_table,nsp_val) == FAIL) return FAIL;

#define ADD_CONSTANTS(name,size,name1,tag)				\
  if ( nsp_##name##_hash_table == NULLHASH )				\
    {									\
      if (( nsp_##name##_hash_table = nsp_hash_create(name1,size))== NULLHASH) return FALSE; \
    }									\
  name##_add_constants(NSP_OBJECT(nsp_##name##_hash_table), tag);

static int add_constants(void)
{
  NspObject *nsp_val;

  ADD_CONSTANTS(gtk,500,"GTK","GTK_");
  ADD_CONSTANTS(gdk,500,"GDK","GDK_");
  ADD_CONSTANTS(atk,500,"ATK", "ATK_");
  ADD_CONSTANTS(pango,500,"PANGO","PANGO_");
  ADD_CONSTANTS(cairo,500,"CAIRO","CAIRO_");
  ADD_CONSTANTS(gio,500,"GIO","G_");
  ADD_CONSTANTS(glib,500,"GLIB","G_");

  PANGO_ENTER( "SCALE_XX_SMALL",PANGO_SCALE_XX_SMALL);
  PANGO_ENTER( "SCALE_X_SMALL",PANGO_SCALE_X_SMALL);
  PANGO_ENTER( "SCALE_SMALL",PANGO_SCALE_SMALL);
  PANGO_ENTER( "SCALE_MEDIUM",PANGO_SCALE_MEDIUM);
  PANGO_ENTER( "SCALE_LARGE",PANGO_SCALE_LARGE);
  PANGO_ENTER( "SCALE_X_LARGE",PANGO_SCALE_X_LARGE);
  PANGO_ENTER( "SCALE_XX_LARGE",PANGO_SCALE_XX_LARGE);

  return TRUE;
}
