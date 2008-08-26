#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>

extern void * new_type_atkhyperlink(type_mode);
extern void * new_type_atkobject(type_mode);
extern void * new_type_atknoopobject(type_mode);
extern void * new_type_atkobjectfactory(type_mode);
extern void * new_type_atknoopobjectfactory(type_mode);
extern void * new_type_atkregistry(type_mode);
extern void * new_type_atkrelation(type_mode);
extern void * new_type_atkrelationset(type_mode);
extern void * new_type_atkstateset(type_mode);
extern void * new_type_atkutil(type_mode);
extern void * new_type_gdkatom(type_mode);
extern void * new_type_gdkevent(type_mode);
extern void * new_type_gdkfont(type_mode);
extern void * new_type_gdkcolor(type_mode);
extern void * new_type_gdkcursor(type_mode);
extern void * new_type_gdkrectangle(type_mode);
extern void * new_type_gdkcolormap(type_mode);
extern void * new_type_gdkdevice(type_mode);
extern void * new_type_gdkdragcontext(type_mode);
extern void * new_type_gdkdrawable(type_mode);
extern void * new_type_gdkwindow(type_mode);
extern void * new_type_gdkpixmap(type_mode);
extern void * new_type_gdkbitmap(type_mode);
extern void * new_type_gdkgc(type_mode);
extern void * new_type_gdkimage(type_mode);
extern void * new_type_gdkkeymap(type_mode);
extern void * new_type_gdkpixbuf(type_mode);
extern void * new_type_gdkpixbufanimation(type_mode);
extern void * new_type_gdkpixbufanimationiter(type_mode);
extern void * new_type_gdkpixbufloader(type_mode);
extern void * new_type_gdkvisual(type_mode);
extern void * new_type_gdkscreen(type_mode);
extern void * new_type_gdkdisplay(type_mode);
extern void   new_type_gpointer(type_mode);
extern void * new_type_gtkrequisition(type_mode);
extern void * new_type_gtkiconset(type_mode);
extern void * new_type_gtkiconsource(type_mode);
extern void * new_type_gtkselectiondata(type_mode);
extern void * new_type_gtktextattributes(type_mode);
extern void * new_type_gtktextiter(type_mode);
extern void * new_type_gtktreeiter(type_mode);
/* extern void new_type_gtkctreenode(type_mode); */
extern void * new_type_gtkaccelgroup(type_mode);
extern void * new_type_gtkaccessible(type_mode);
extern void * new_type_gtkiconfactory(type_mode);
extern void * new_type_gtkobject(type_mode);
extern void * new_type_gtkitemfactory(type_mode);
extern void * new_type_gtkimcontext(type_mode);
extern void * new_type_gtkimcontextsimple(type_mode);
extern void * new_type_gtkimmulticontext(type_mode);
extern void * new_type_gtkcellrenderer(type_mode);
extern void * new_type_gtkcelllayout(type_mode);
extern void * new_type_gtkcellrenderertoggle(type_mode);
extern void * new_type_gtkcellrenderertext(type_mode);
extern void * new_type_gtkcellrendererpixbuf(type_mode);
extern void * new_type_gtkadjustment(type_mode);
extern void * new_type_gtkrcstyle(type_mode);
extern void * new_type_gtksettings(type_mode);
extern void * new_type_gtksizegroup(type_mode);
extern void * new_type_gtkstyle(type_mode);
extern void * new_type_gtktextbuffer(type_mode);
extern void * new_type_gtktextchildanchor(type_mode);
extern void * new_type_gtktextmark(type_mode);
extern void * new_type_gtktexttag(type_mode);
extern void * new_type_gtktexttagtable(type_mode);
extern void * new_type_gtktooltips(type_mode);
extern void * new_type_gtkliststore(type_mode);
extern void * new_type_gtktreemodelsort(type_mode);
extern void * new_type_gtktreeselection(type_mode);
extern void * new_type_gtktreestore(type_mode);
extern void * new_type_gtktreeviewcolumn(type_mode);
extern void * new_type_gtkwidget(type_mode);
extern void * new_type_gtkseparator(type_mode);
extern void * new_type_gtkvseparator(type_mode);
extern void * new_type_gtkhseparator(type_mode);
extern void * new_type_gtkruler(type_mode);
extern void * new_type_gtkvruler(type_mode);
extern void * new_type_gtkhruler(type_mode);
extern void * new_type_gtkrange(type_mode);
extern void * new_type_gtkscrollbar(type_mode);
extern void * new_type_gtkvscrollbar(type_mode);
extern void * new_type_gtkhscrollbar(type_mode);
extern void * new_type_gtkscale(type_mode);
extern void * new_type_gtkvscale(type_mode);
extern void * new_type_gtkhscale(type_mode);
extern void * new_type_gtkprogress(type_mode);
extern void * new_type_gtkprogressbar(type_mode);
extern void * new_type_gtkpreview(type_mode);
extern void * new_type_gtkoldeditable(type_mode);
extern void * new_type_gtkmisc(type_mode);
/* extern void *new_type_gtkpixmap(type_mode); */
extern void * new_type_gtkarrow(type_mode);
extern void * new_type_gtkimage(type_mode);
extern void * new_type_gtklabel(type_mode);
extern void * new_type_gtkaccellabel(type_mode);
extern void * new_type_gtkinvisible(type_mode);
extern void * new_type_gtkentry(type_mode);
extern void * new_type_gtkspinbutton(type_mode);
extern void * new_type_gtkdrawingarea(type_mode);
extern void * new_type_gtkcurve(type_mode);
extern void * new_type_gtkcontainer(type_mode);
extern void * new_type_gtktreeview(type_mode);
extern void * new_type_gtktoolbar(type_mode);
extern void * new_type_gtktextview(type_mode);
extern void * new_type_gtktable(type_mode);
extern void * new_type_gtksocket(type_mode);
extern void * new_type_gtkpaned(type_mode);
extern void * new_type_gtkvpaned(type_mode);
extern void * new_type_gtkhpaned(type_mode);
extern void * new_type_gtknotebook(type_mode);
extern void * new_type_gtkmenushell(type_mode);
extern void * new_type_gtkmenu(type_mode);
extern void * new_type_gtkmenubar(type_mode);
extern void * new_type_gtklayout(type_mode);
extern void * new_type_gtkfixed(type_mode);
extern void * new_type_gtkbin(type_mode);
extern void * new_type_gtkviewport(type_mode);
extern void * new_type_gtkscrolledwindow(type_mode);
extern void * new_type_gtkitem(type_mode);
extern void * new_type_gtkmenuitem(type_mode);
extern void * new_type_gtktearoffmenuitem(type_mode);
extern void * new_type_gtkseparatormenuitem(type_mode);
extern void * new_type_gtkcheckmenuitem(type_mode);
extern void * new_type_gtkradiomenuitem(type_mode);
extern void * new_type_gtkimagemenuitem(type_mode);
/* extern void *new_type_gtklist(type_mode); */
/* extern void *    new_type_gtklistitem(type_mode); */
extern void * new_type_gtkhandlebox(type_mode);
extern void * new_type_gtkframe(type_mode);
extern void * new_type_gtkaspectframe(type_mode);
extern void * new_type_gtkeventbox(type_mode);
extern void * new_type_gtkalignment(type_mode);
extern void * new_type_gtkbutton(type_mode);
extern void * new_type_gtktogglebutton(type_mode);
extern void * new_type_gtkcheckbutton(type_mode);
extern void * new_type_gtkradiobutton(type_mode);
extern void * new_type_gtkoptionmenu(type_mode);
extern void * new_type_gtkbox(type_mode);
extern void * new_type_gtkvbox(type_mode);
extern void * new_type_gtkcolorselection(type_mode);
extern void * new_type_gtkfontselection(type_mode);
extern void * new_type_gtkgammacurve(type_mode);
extern void * new_type_gtkhbox(type_mode);
extern void * new_type_gtkstatusbar(type_mode);
extern void * new_type_gtkcombo(type_mode);
extern void * new_type_gtkcombobox(type_mode);
extern void * new_type_gtkcomboboxentry(type_mode);
extern void * new_type_gtkbuttonbox(type_mode);
extern void * new_type_gtkvbuttonbox(type_mode);
extern void * new_type_gtkhbuttonbox(type_mode);
/* extern void new_type_gtkclist(type_mode);*/
/* extern void new_type_gtkctree(type_mode);*/
extern void * new_type_gtkcalendar(type_mode);
extern void * new_type_gtkwindow(type_mode);
extern void * new_type_gtkplug(type_mode);
extern void * new_type_gtkdialog(type_mode);
extern void * new_type_gtkmessagedialog(type_mode);
extern void * new_type_gtkinputdialog(type_mode);
extern void * new_type_gtkfontselectiondialog(type_mode);
extern void * new_type_gtkfileselection(type_mode);
extern void * new_type_gtkcolorselectiondialog(type_mode);
extern void * new_type_gtkwindowgroup(type_mode);
extern void * new_type_gtkeditable(type_mode);
extern void * new_type_gtkcelleditable(type_mode);
extern void * new_type_gtktreemodel(type_mode);
extern void * new_type_gtktreepath(type_mode);
extern void * new_type_gtktreedragsource(type_mode);
extern void * new_type_gtktreedragdest(type_mode);
extern void * new_type_gtktreesortable(type_mode);
extern void * new_type_pangoattribute(type_mode);
extern void * new_type_pangoattrlist(type_mode);
extern void * new_type_pangocolor(type_mode);
extern void * new_type_pangofontdescription(type_mode);
extern void * new_type_pangofontmetrics(type_mode);
extern void * new_type_pangoglyphstring(type_mode);
extern void * new_type_pangolanguage(type_mode);
extern void * new_type_pangotabarray(type_mode);
extern void * new_type_pangocontext(type_mode);
extern void * new_type_pangofont(type_mode);
extern void * new_type_pangofontface(type_mode);
extern void * new_type_pangofontfamily(type_mode);
extern void * new_type_pangofontmap(type_mode);
extern void * new_type_pangofontset(type_mode);
extern void * new_type_pangolayout(type_mode);
extern void * new_type_gtkcellview(type_mode);
extern void * new_type_gdate(type_mode);

extern void * new_type_gtkaction(type_mode);
extern void * new_type_gtkactiongroup(type_mode);
extern void * new_type_gtktoggleaction(type_mode);
extern void * new_type_gtkradioaction(type_mode);
extern void * new_type_gtkuimanager(type_mode);

extern void * new_type_gdkdisplaymanager(type_mode);
extern void * new_type_gtkaboutdialog(type_mode);
extern void * new_type_gtkaccelmap(type_mode);
extern void * new_type_gtkborder(type_mode);
extern void * new_type_gtkcellrenderercombo(type_mode);
extern void * new_type_gtkcellrendererprogress(type_mode);
extern void * new_type_gtkcolorbutton(type_mode);
extern void * new_type_gtkentrycompletion(type_mode);
extern void * new_type_gtkexpander(type_mode);
extern void * new_type_gtkfilechooserbutton(type_mode);
extern void * new_type_gtkfilechooserdialog(type_mode);
extern void * new_type_gtkfilechooser(type_mode);
extern void * new_type_gtkfilechooserwidget(type_mode);
extern void * new_type_gtkfilefilter(type_mode);
extern void * new_type_gtkfontbutton(type_mode);
extern void * new_type_gtkiconinfo(type_mode);
extern void * new_type_gtkicontheme(type_mode);
extern void * new_type_gtkiconview(type_mode);
extern void * new_type_gtkmenutoolbutton(type_mode);
extern void * new_type_gtkradiotoolbutton(type_mode);
extern void * new_type_gtkseparatortoolitem(type_mode);
extern void * new_type_gtktoggletoolbutton(type_mode);
extern void * new_type_gtktoolbutton(type_mode);
extern void * new_type_gtktoolitem(type_mode);
extern void * new_type_gtktreemodelfilter(type_mode);
extern void * new_type_gtktreerowreference(type_mode);


static int add_constants(void);

/* init nsp gtk object types */

void nsp_init_gtk_types(void) 
{
  g_type_init();
  new_type_atkhyperlink(T_BASE);
  new_type_atkobject(T_BASE);
  new_type_atknoopobject(T_BASE);
  new_type_atkobjectfactory(T_BASE);
  new_type_atknoopobjectfactory(T_BASE);
  new_type_atkregistry(T_BASE);
  new_type_atkrelation(T_BASE);
  new_type_atkrelationset(T_BASE);
  new_type_atkstateset(T_BASE);
  new_type_atkutil(T_BASE);
  new_type_gboxed(T_BASE);
  new_type_gdkatom(T_BASE);
  new_type_gdkevent(T_BASE);
  new_type_gdkfont(T_BASE);
  new_type_gdkcolor(T_BASE);
  new_type_gdkcursor(T_BASE);
  new_type_gdkrectangle(T_BASE);
  new_type_gdkcolormap(T_BASE);
  new_type_gdkdevice(T_BASE);
  new_type_gdkdragcontext(T_BASE);
  new_type_gdkdrawable(T_BASE);
  new_type_gdkwindow(T_BASE);
  new_type_gdkpixmap(T_BASE);
  new_type_gdkbitmap(T_BASE);
  new_type_gdkgc(T_BASE);
  new_type_gdkimage(T_BASE);
  new_type_gdkkeymap(T_BASE);
  new_type_gdkpixbuf(T_BASE);
  new_type_gdkpixbufanimation(T_BASE);
  new_type_gdkpixbufanimationiter(T_BASE);
  new_type_gdkpixbufloader(T_BASE);
  new_type_gdkvisual(T_BASE);
  new_type_gdkscreen(T_BASE);
  new_type_gdkdisplay(T_BASE);
  new_type_gobject(T_BASE);
  new_type_gpointer(T_BASE);
  new_type_gtkrequisition(T_BASE);
  new_type_gtkiconset(T_BASE);
  new_type_gtkiconsource(T_BASE);
  new_type_gtkselectiondata(T_BASE);
  new_type_gtktextattributes(T_BASE);
  new_type_gtktextiter(T_BASE);
  new_type_gtktreeiter(T_BASE);
  /* new_type_gtkctreenode(T_BASE); */
  new_type_gtkaccelgroup(T_BASE);
  new_type_gtkaccessible(T_BASE);
  new_type_gtkiconfactory(T_BASE);
  new_type_gtkobject(T_BASE);
  new_type_gtkitemfactory(T_BASE);
  new_type_gtkimcontext(T_BASE);
  new_type_gtkimcontextsimple(T_BASE);
  new_type_gtkimmulticontext(T_BASE);
  new_type_gtkcellrenderer(T_BASE);
  new_type_gtkcelllayout(T_BASE);
  new_type_gtkcellrenderertoggle(T_BASE);
  new_type_gtkcellrenderertext(T_BASE);
  new_type_gtkcellrendererpixbuf(T_BASE);
  new_type_gtkadjustment(T_BASE);
  new_type_gtkrcstyle(T_BASE);
  new_type_gtksettings(T_BASE);
  new_type_gtksizegroup(T_BASE);
  new_type_gtkstyle(T_BASE);
  new_type_gtktextbuffer(T_BASE);
  new_type_gtktextchildanchor(T_BASE);
  new_type_gtktextmark(T_BASE);
  new_type_gtktexttag(T_BASE);
  new_type_gtktexttagtable(T_BASE);
  new_type_gtktooltips(T_BASE);
  new_type_gtkliststore(T_BASE);
  new_type_gtktreemodelsort(T_BASE);
  new_type_gtktreeselection(T_BASE);
  new_type_gtktreestore(T_BASE);
  new_type_gtktreeviewcolumn(T_BASE);
  new_type_gtkwidget(T_BASE);
  new_type_gtkseparator(T_BASE);
  new_type_gtkvseparator(T_BASE);
  new_type_gtkhseparator(T_BASE);
  new_type_gtkruler(T_BASE);
  new_type_gtkvruler(T_BASE);
  new_type_gtkhruler(T_BASE);
  new_type_gtkrange(T_BASE);
  new_type_gtkscrollbar(T_BASE);
  new_type_gtkvscrollbar(T_BASE);
  new_type_gtkhscrollbar(T_BASE);
  new_type_gtkscale(T_BASE);
  new_type_gtkvscale(T_BASE);
  new_type_gtkhscale(T_BASE);
  new_type_gtkprogress(T_BASE);
  new_type_gtkprogressbar(T_BASE);
  new_type_gtkpreview(T_BASE);
  new_type_gtkoldeditable(T_BASE);
  new_type_gtkmisc(T_BASE);
  /* new_type_gtkpixmap(T_BASE); */
  new_type_gtkarrow(T_BASE);
  new_type_gtkimage(T_BASE);
  new_type_gtklabel(T_BASE);
  new_type_gtkaccellabel(T_BASE);
  new_type_gtkinvisible(T_BASE);
  new_type_gtkentry(T_BASE);
  new_type_gtkspinbutton(T_BASE);
  new_type_gtkdrawingarea(T_BASE);
  new_type_gtkcurve(T_BASE);
  new_type_gtkcontainer(T_BASE);
  new_type_gtktreeview(T_BASE);
  new_type_gtktoolbar(T_BASE);
  new_type_gtktextview(T_BASE);
  new_type_gtktable(T_BASE);
  new_type_gtksocket(T_BASE);
  new_type_gtkpaned(T_BASE);
  new_type_gtkvpaned(T_BASE);
  new_type_gtkhpaned(T_BASE);
  new_type_gtknotebook(T_BASE);
  new_type_gtkmenushell(T_BASE);
  new_type_gtkmenu(T_BASE);
  new_type_gtkmenubar(T_BASE);
  new_type_gtklayout(T_BASE);
  new_type_gtkfixed(T_BASE);
  new_type_gtkbin(T_BASE);
  new_type_gtkviewport(T_BASE);
  new_type_gtkscrolledwindow(T_BASE);
  new_type_gtkitem(T_BASE);
  new_type_gtkmenuitem(T_BASE);
  new_type_gtktearoffmenuitem(T_BASE);
  new_type_gtkseparatormenuitem(T_BASE);
  new_type_gtkcheckmenuitem(T_BASE);
  new_type_gtkradiomenuitem(T_BASE);
  new_type_gtkimagemenuitem(T_BASE);
  /* new_type_gtklist(T_BASE);
     new_type_gtklistitem(T_BASE);
  */
  new_type_gtkhandlebox(T_BASE);
  new_type_gtkframe(T_BASE);
  new_type_gtkaspectframe(T_BASE);
  new_type_gtkeventbox(T_BASE);
  new_type_gtkalignment(T_BASE);
  new_type_gtkbutton(T_BASE);
  new_type_gtktogglebutton(T_BASE);
  new_type_gtkcheckbutton(T_BASE);
  new_type_gtkradiobutton(T_BASE);
  new_type_gtkoptionmenu(T_BASE);
  new_type_gtkbox(T_BASE);
  new_type_gtkvbox(T_BASE);
  new_type_gtkcolorselection(T_BASE);
  new_type_gtkfontselection(T_BASE);
  new_type_gtkgammacurve(T_BASE);
  new_type_gtkhbox(T_BASE);
  new_type_gtkstatusbar(T_BASE);
  new_type_gtkcombo(T_BASE);
  new_type_gtkcombobox(T_BASE);
  new_type_gtkcomboboxentry(T_BASE);
  new_type_gtkbuttonbox(T_BASE);
  new_type_gtkvbuttonbox(T_BASE);
  new_type_gtkhbuttonbox(T_BASE);
  /* new_type_gtkclist(T_BASE);*/
  /* new_type_gtkctree(T_BASE);*/
  new_type_gtkcalendar(T_BASE);
  new_type_gtkwindow(T_BASE);
  new_type_gtkplug(T_BASE);
  new_type_gtkdialog(T_BASE);
  new_type_gtkmessagedialog(T_BASE);
  new_type_gtkinputdialog(T_BASE);
  new_type_gtkfontselectiondialog(T_BASE);
  new_type_gtkfileselection(T_BASE);
  new_type_gtkcolorselectiondialog(T_BASE);
  new_type_gtkwindowgroup(T_BASE);
  new_type_gtkeditable(T_BASE);
  new_type_gtkcelleditable(T_BASE);
  new_type_gtktreemodel(T_BASE);
  new_type_gtktreepath(T_BASE);
  new_type_gtktreedragsource(T_BASE);
  new_type_gtktreedragdest(T_BASE);
  new_type_gtktreesortable(T_BASE);
  new_type_pangoattribute(T_BASE);
  new_type_pangoattrlist(T_BASE);
  new_type_pangocolor(T_BASE);
  new_type_pangofontdescription(T_BASE);
  new_type_pangofontmetrics(T_BASE);
  new_type_pangoglyphstring(T_BASE);
  new_type_pangolanguage(T_BASE);
  new_type_pangotabarray(T_BASE);
  new_type_pangocontext(T_BASE);
  new_type_pangofont(T_BASE);
  new_type_pangofontface(T_BASE);
  new_type_pangofontfamily(T_BASE);
  new_type_pangofontmap(T_BASE);
  new_type_pangofontset(T_BASE);
  new_type_pangolayout(T_BASE);

#if GTK_CHECK_VERSION(2,6,0)
  new_type_gtkcellview(T_BASE);
#endif 
  add_constants();
}

/* more types 
 */

void nsp_init_gtk_types_added(void)
{
  /* from glib */
  new_type_gdate(T_BASE);
  /* for gtk */
  new_type_gtkaction(T_BASE);
  new_type_gtkactiongroup(T_BASE);
  new_type_gtktoggleaction(T_BASE);  
  new_type_gtkradioaction(T_BASE);
  new_type_gtkuimanager(T_BASE);
  new_type_gdkdisplaymanager(T_BASE);
  new_type_gtkaboutdialog(T_BASE);
  new_type_gtkaccelmap(T_BASE);
  new_type_gtkborder(T_BASE);
  new_type_gtkcellrenderercombo(T_BASE);
  new_type_gtkcellrendererprogress(T_BASE);
  new_type_gtkcolorbutton(T_BASE);
  new_type_gtkentrycompletion(T_BASE);
  new_type_gtkexpander(T_BASE);
  new_type_gtkfilechooserbutton(T_BASE);
  new_type_gtkfilechooserdialog(T_BASE);
  new_type_gtkfilechooser(T_BASE);
  new_type_gtkfilechooserwidget(T_BASE);
  new_type_gtkfilefilter(T_BASE);
  new_type_gtkfontbutton(T_BASE);
  new_type_gtkiconinfo(T_BASE);
  new_type_gtkicontheme(T_BASE);
  new_type_gtkiconview(T_BASE);
  new_type_gtkmenutoolbutton(T_BASE);
  new_type_gtkradiotoolbutton(T_BASE);
  new_type_gtkseparatortoolitem(T_BASE);
  new_type_gtktoggletoolbutton(T_BASE);
  new_type_gtktoolbutton(T_BASE);
  new_type_gtktoolitem(T_BASE);
  new_type_gtktreemodelfilter(T_BASE);
  new_type_gtktreerowreference(T_BASE);
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

