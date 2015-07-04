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
extern void nsp_initialize_girepository_types(void);
extern void nsp_initialize_gtk_types(void);
extern void nsp_initialize_gvalue_types(void);
extern void nsp_initialize_pango_types(void);
extern void nsp_initialize_webkit_types(void);

/* init nsp gtk object types */

void nsp_init_gtk_types(void)
{
#if GLIB_CHECK_VERSION(2,36,0)
  /* deprecated g_type_init */
#else
  g_type_init();
#endif
  nsp_initialize_atk_types();
  /* nsp_initialize_cairo_types(); */
  nsp_initialize_gdk_types();
  nsp_initialize_gio_types();
  nsp_initialize_girepository_types();
  nsp_initialize_gtk_types();
  /* nsp_initialize_gvalue_types(); */
  nsp_initialize_pango_types();
  nsp_initialize_webkit_types();

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

#define PANGO_ENTER(name,value) \
  if (( nsp_val = (NspObject *) nsp_matrix_create_from_doubles(name,1,1,value))== NULL) return FAIL; \
  if (nsp_hash_enter(nsp_pango_hash_table,nsp_val) == FAIL) return FAIL; \

static int add_constants(void)
{
  NspObject *nsp_val;
  if ( nsp_gtk_hash_table == NULLHASH )
    {
      /* create and store in the protected frame XXXX  */
      if (( nsp_gtk_hash_table = nsp_hash_create("GTK",500))== NULLHASH) return FALSE;
    }
  gtk_add_constants(NSP_OBJECT(nsp_gtk_hash_table), "GTK_");
  if ( nsp_gdk_hash_table == NULLHASH )
    {
      /* create and store in the protected frame XXXX  */
      if (( nsp_gdk_hash_table = nsp_hash_create("GDK",500))== NULLHASH) return FALSE;
    }
  gdk_add_constants(NSP_OBJECT(nsp_gdk_hash_table), "GDK_");
  if ( nsp_atk_hash_table == NULLHASH )
    {
      /* create and store in the protected frame XXXX  */
      if (( nsp_atk_hash_table = nsp_hash_create("ATK",500))== NULLHASH) return FALSE;
    }
  atk_add_constants(NSP_OBJECT(nsp_atk_hash_table), "ATK_");
  if ( nsp_pango_hash_table == NULLHASH )
    {
      /* create and store in the protected frame XXXX  */
      if (( nsp_pango_hash_table = nsp_hash_create("PANGO",500))== NULLHASH) return FALSE;
    }
  pango_add_constants(NSP_OBJECT(nsp_pango_hash_table),"PANGO_");

  PANGO_ENTER( "SCALE_XX_SMALL",PANGO_SCALE_XX_SMALL);
  PANGO_ENTER( "SCALE_X_SMALL",PANGO_SCALE_X_SMALL);
  PANGO_ENTER( "SCALE_SMALL",PANGO_SCALE_SMALL);
  PANGO_ENTER( "SCALE_MEDIUM",PANGO_SCALE_MEDIUM);
  PANGO_ENTER( "SCALE_LARGE",PANGO_SCALE_LARGE);
  PANGO_ENTER( "SCALE_X_LARGE",PANGO_SCALE_X_LARGE);
  PANGO_ENTER( "SCALE_XX_LARGE",PANGO_SCALE_XX_LARGE);
  return TRUE;
}
