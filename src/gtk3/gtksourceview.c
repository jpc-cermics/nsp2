/* -*- Mode: C -*- */

/* This file is generated, please do not edit */
/* Nsp
 * Copyright (C) 1998-2015 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */





#line 4 "codegen-3.0/gtksourceview.override"
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gtk/gtkx.h>
#include <cairo/cairo.h>
#include <cairo/cairo-gobject.h>

#include <nsp/nsp.h>
#include <nsp/objects.h>
#include <nsp/smatrix.h>
#include <nsp/cells.h>
#include <nsp/plist.h>
#include <nsp/none.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <nsp/interf.h>
#include <nsp/eval.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>
#include <nsp/gtk/gtktexttagtable.h>
#include <nsp/gtk/gtktooltip.h>
#include <nsp/gtk/gerror.h>

#include <nsp/all-glib.h>
#include <nsp/gvariant.h>
#include <nsp/gvarianttype.h>

#include <nsp/gtk/cairo_t.h>
#include <nsp/gtk/cairo_surface_t.h>

#include <gtksourceview/gtksource.h>
#include <gtksourceview/gtksourceview.h>
#include <gtksourceview/gtksourcelanguage.h>
#include <gtksourceview/gtksourcebuffer.h>
#include <gtksourceview/gtksourcelanguagemanager.h>
#include <gtksourceview/gtksourcestyleschememanager.h>
#include <gtksourceview/gtksourceprintcompositor.h>
// #include <gtksourceview/gtksourceiter.h>
#include <gtksourceview/gtksourcegutter.h>

#define GTK_SOURCE_TYPE_BUFFER_INPUT_STREAM GTK_SOURCE_TYPE_BUFFER
#define GTK_SOURCE_TYPE_BUFFER_OUTPUT_STREAM GTK_SOURCE_TYPE_BUFFER

#line 73 "gtksourceview.c"
/* ---------- types from other modules ---------- */
#include <nsp/gtk/gdkatom.h>
#include <nsp/gtk/gdkpixmap.h>
#include <nsp/gtk/gdkwindow.h>
#include <nsp/gtk/gdkgc.h>
#include <nsp/gtk/gdkfont.h>
#include <nsp/gtk/gdkpixbuf.h>
#include <nsp/gtk/gdkbitmap.h>
#include <nsp/gtk/gdkpixbufanimation.h>
#include <nsp/gtk/gdkdragcontext.h>
#include <nsp/gtk/gdkcolormap.h>
#include <nsp/gtk/gdkcolor.h>
#include <nsp/gtk/gdkimage.h>
#include <nsp/gtk/gdkvisual.h>
#include <nsp/gtk/gdkrectangle.h>
#include <nsp/gtk/gdkevent.h>
#include <nsp/gtk/gdkscreen.h>
#include <nsp/gtk/gdkdisplay.h>
#include <nsp/gtk/gdkdevice.h>
#include <nsp/gtk/gdkframeclock.h>
#include <nsp/gtk/gdkrgba.h>
#include <nsp/gtk/gtktreemodel.h>
#include <nsp/gtk/pangolayout.h>
#include <nsp/gtk/pangolanguage.h>
#include <nsp/gtk/pangofontdescription.h>
#include <nsp/gtk/pangofontfamily.h>
#include <nsp/gtk/pangofontface.h>
#include <nsp/gtk/pangoattrlist.h>
#include <nsp/gtk/pangotabarray.h>
#include <nsp/gtk/pangocontext.h>
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/gtksourcebuffer.h>
#include <nsp/gtk/gtksourcecompletion.h>
#include <nsp/gtk/gtksourcecompletioncontext.h>
#include <nsp/gtk/gtksourcecompletioninfo.h>
#include <nsp/gtk/gtksourcecompletionitem.h>
#include <nsp/gtk/gtksourcecompletionproposal.h>
#include <nsp/gtk/gtksourcecompletionprovider.h>
#include <nsp/gtk/gtksourcefile.h>
#include <nsp/gtk/gtksourcefileloader.h>
#include <nsp/gtk/gtksourcefilesaver.h>
#include <nsp/gtk/gtksourcegutter.h>
#include <nsp/gtk/gtksourcegutterrenderer.h>
#include <nsp/gtk/gtksourcegutterrendererpixbuf.h>
#include <nsp/gtk/gtksourcegutterrenderertext.h>
#include <nsp/gtk/gtksourcelanguage.h>
#include <nsp/gtk/gtksourcelanguagemanager.h>
#include <nsp/gtk/gtksourcemark.h>
#include <nsp/gtk/gtksourcemarkattributes.h>
#include <nsp/gtk/gtksourceprintcompositor.h>
#include <nsp/gtk/gtksourcesearchcontext.h>
#include <nsp/gtk/gtksourcesearchsettings.h>
#include <nsp/gtk/gtksourcestylescheme.h>
#include <nsp/gtk/gtksourcestyleschemechooserbutton.h>
#include <nsp/gtk/gtksourcestyleschemechooserwidget.h>
#include <nsp/gtk/gtksourcestyleschememanager.h>
#include <nsp/gtk/gtksourceundomanager.h>
#include <nsp/gtk/gtksourceview.h>
#include <nsp/gtk/gtksourcemap.h>


/* -----------NspGtkSourceBuffer ----------- */


#define  NspGtkSourceBuffer_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcebuffer.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceBuffer inherits from GtkTextBuffer 
 */

int nsp_type_gtksourcebuffer_id=0;
NspTypeGtkSourceBuffer *nsp_type_gtksourcebuffer=NULL;

/*
 * Type object for NspGtkSourceBuffer 
 * all the instance of NspTypeGtkSourceBuffer share the same id. 
 * nsp_type_gtksourcebuffer: is an instance of NspTypeGtkSourceBuffer 
 *    used for objects of NspGtkSourceBuffer type (i.e built with new_gtksourcebuffer) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceBuffer *new_type_gtksourcebuffer(type_mode mode)
{
  NspTypeGtkSourceBuffer *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcebuffer != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcebuffer;
    }
  if (( type =  malloc(sizeof(NspTypeGtkTextBuffer))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtktextbuffer(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcebuffer_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcebuffer_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcebuffer;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcebuffer */ 

  top->s_type =  (s_type_func *) nsp_gtksourcebuffer_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcebuffer_type_short_string;
  /* top->create = (create_func*) int_gtksourcebuffer_create;*/

  /* specific methods for gtksourcebuffer */

  type->init = (init_func *) init_gtksourcebuffer;

  /* 
   * NspGtkSourceBuffer interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcebuffer_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceBuffer called nsp_type_gtksourcebuffer
       */
      type->id =  nsp_type_gtksourcebuffer_id = nsp_new_type_id();
      nsp_type_gtksourcebuffer = type;
      if ( nsp_register_type(nsp_type_gtksourcebuffer) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcebuffer, GTK_SOURCE_TYPE_BUFFER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcebuffer(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcebuffer_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceBuffer instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcebuffer(NspGtkSourceBuffer *Obj,NspTypeGtkSourceBuffer *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceBuffer 
 */

NspGtkSourceBuffer *new_gtksourcebuffer() 
{
  NspGtkSourceBuffer *loc;
  /* type must exists */
  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceBuffer)))== NULLGTKSOURCEBUFFER) return loc;
  /* initialize object */
  if ( init_gtksourcebuffer(loc,nsp_type_gtksourcebuffer) == FAIL) return NULLGTKSOURCEBUFFER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceBuffer 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcebuffer_type_name[]="GtkSourceBuffer";
static char gtksourcebuffer_short_type_name[]="GtkSourceBuffer";

static char *nsp_gtksourcebuffer_type_as_string(void)
{
  return(gtksourcebuffer_type_name);
}

static char *nsp_gtksourcebuffer_type_short_string(NspObject *v)
{
  return(gtksourcebuffer_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceBuffer objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceBuffer   *nsp_gtksourcebuffer_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcebuffer_id)  == TRUE  ) return ((NspGtkSourceBuffer *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcebuffer));
  return NULL;
}

int IsGtkSourceBufferObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcebuffer_id);
}

int IsGtkSourceBuffer(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcebuffer_id);
}

NspGtkSourceBuffer  *GetGtkSourceBufferCopy(Stack stack, int i)
{
  if (  GetGtkSourceBuffer(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceBuffer  *GetGtkSourceBuffer(Stack stack, int i)
{
  NspGtkSourceBuffer *M;
  if (( M = nsp_gtksourcebuffer_object(NthObj(i))) == NULLGTKSOURCEBUFFER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceBuffer *gtksourcebuffer_copy(NspGtkSourceBuffer *self)
{
  /* return gtktextbuffer_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcebuffer);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcebuffer);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceBuffer
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_buffer_new_with_language (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *language;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcelanguage, &language) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_buffer_new_with_language(GTK_SOURCE_LANGUAGE(language->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcebuffer);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gtk_source_buffer_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *table;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtktexttagtable, &table) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_buffer_new(GTK_TEXT_TAG_TABLE(table->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcebuffer);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_buffer_get_highlight_syntax(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_highlight_syntax(GTK_SOURCE_BUFFER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_set_highlight_syntax(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int highlight;
  if ( GetArgs(stack,rhs,opt,T,&highlight) == FAIL) return RET_BUG;
    gtk_source_buffer_set_highlight_syntax(GTK_SOURCE_BUFFER(self->obj),highlight);
  return 0;
}

static int _wrap_gtk_source_buffer_get_highlight_matching_brackets(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_highlight_matching_brackets(GTK_SOURCE_BUFFER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_set_highlight_matching_brackets(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int highlight;
  if ( GetArgs(stack,rhs,opt,T,&highlight) == FAIL) return RET_BUG;
    gtk_source_buffer_set_highlight_matching_brackets(GTK_SOURCE_BUFFER(self->obj),highlight);
  return 0;
}

static int _wrap_gtk_source_buffer_get_max_undo_levels(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_max_undo_levels(GTK_SOURCE_BUFFER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_set_max_undo_levels(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int max_undo_levels;
  if ( GetArgs(stack,rhs,opt,T,&max_undo_levels) == FAIL) return RET_BUG;
    gtk_source_buffer_set_max_undo_levels(GTK_SOURCE_BUFFER(self->obj),max_undo_levels);
  return 0;
}

static int _wrap_gtk_source_buffer_get_language(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceLanguage *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_language(GTK_SOURCE_BUFFER(self->obj));
  nsp_type_gtksourcelanguage = new_type_gtksourcelanguage(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcelanguage))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_buffer_set_language(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *language;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcelanguage, &language) == FAIL) return RET_BUG;
    gtk_source_buffer_set_language(GTK_SOURCE_BUFFER(self->obj),GTK_SOURCE_LANGUAGE(language->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_can_undo(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_can_undo(GTK_SOURCE_BUFFER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_can_redo(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_can_redo(GTK_SOURCE_BUFFER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_get_style_scheme(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceStyleScheme *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_style_scheme(GTK_SOURCE_BUFFER(self->obj));
  nsp_type_gtksourcestylescheme = new_type_gtksourcestylescheme(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcestylescheme))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_buffer_set_style_scheme(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *scheme;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcestylescheme, &scheme) == FAIL) return RET_BUG;
    gtk_source_buffer_set_style_scheme(GTK_SOURCE_BUFFER(self->obj),GTK_SOURCE_STYLE_SCHEME(scheme->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_ensure_highlight(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_buffer_ensure_highlight(GTK_SOURCE_BUFFER(self->obj),start,end);
  return 0;
}

static int _wrap_gtk_source_buffer_undo(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_buffer_undo(GTK_SOURCE_BUFFER(self->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_redo(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_buffer_redo(GTK_SOURCE_BUFFER(self->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_begin_not_undoable_action(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_buffer_begin_not_undoable_action(GTK_SOURCE_BUFFER(self->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_end_not_undoable_action(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_buffer_end_not_undoable_action(GTK_SOURCE_BUFFER(self->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_create_source_mark(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,obj, t_end};
  char *name, *category;
  GtkTextIter *where = NULL;
  NspObject *nsp_where = NULL, *nsp_ret;
  GtkSourceMark *ret;
  if ( GetArgs(stack,rhs,opt,T,&name, &category, &nsp_where) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_where, GTK_TYPE_TEXT_ITER))
      where = nspg_boxed_get(nsp_where, GtkTextIter);
  else {
      Scierror( "Error: where should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_create_source_mark(GTK_SOURCE_BUFFER(self->obj),name,category,where);
  nsp_type_gtksourcemark = new_type_gtksourcemark(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemark))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_buffer_forward_iter_to_source_mark(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,string, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  char *category;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &category) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_forward_iter_to_source_mark(GTK_SOURCE_BUFFER(self->obj),iter,category);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_backward_iter_to_source_mark(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,string, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  char *category;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &category) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_backward_iter_to_source_mark(GTK_SOURCE_BUFFER(self->obj),iter,category);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_get_source_marks_at_iter(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,string, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  char *category;
  GSList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &category) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_get_source_marks_at_iter(GTK_SOURCE_BUFFER(self->obj),iter,category);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

static int _wrap_gtk_source_buffer_get_source_marks_at_line(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string, t_end};
  int line;
  char *category;
  GSList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&line, &category) == FAIL) return RET_BUG;
    ret =gtk_source_buffer_get_source_marks_at_line(GTK_SOURCE_BUFFER(self->obj),line,category);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

static int _wrap_gtk_source_buffer_remove_source_marks(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,string, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL;
  char *category;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end, &category) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_buffer_remove_source_marks(GTK_SOURCE_BUFFER(self->obj),start,end,category);
  return 0;
}

static int _wrap_gtk_source_buffer_iter_has_context_class(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,string, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  char *context_class;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &context_class) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_iter_has_context_class(GTK_SOURCE_BUFFER(self->obj),iter,context_class);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_get_context_classes_at_iter(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL, *nsp_ret;
  gchar **ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_get_context_classes_at_iter(GTK_SOURCE_BUFFER(self->obj),iter);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_buffer_iter_forward_to_context_class_toggle(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,string, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  char *context_class;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &context_class) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_iter_forward_to_context_class_toggle(GTK_SOURCE_BUFFER(self->obj),iter,context_class);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_iter_backward_to_context_class_toggle(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,string, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  char *context_class;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &context_class) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_buffer_iter_backward_to_context_class_toggle(GTK_SOURCE_BUFFER(self->obj),iter,context_class);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_buffer_change_case(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GtkSourceChangeCaseType case_type;
  NspObject *nsp_case_type = NULL, *nsp_start = NULL, *nsp_end = NULL;
  GtkTextIter *start = NULL, *end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_case_type, &nsp_start, &nsp_end) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GTK_SOURCE_TYPE_CHANGE_CASE_TYPE, nsp_case_type, &case_type)== FAIL)
      return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_buffer_change_case(GTK_SOURCE_BUFFER(self->obj),case_type,start,end);
  return 0;
}

static int _wrap_gtk_source_buffer_join_lines(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_buffer_join_lines(GTK_SOURCE_BUFFER(self->obj),start,end);
  return 0;
}

static int _wrap_gtk_source_buffer_sort_lines(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj,s_int, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL, *nsp_flags = NULL;
  GtkSourceSortFlags flags;
  int column;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end, &nsp_flags, &column) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_flags_get_value(GTK_SOURCE_TYPE_SORT_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    gtk_source_buffer_sort_lines(GTK_SOURCE_BUFFER(self->obj),start,end,flags,column);
  return 0;
}

static int _wrap_gtk_source_buffer_get_undo_manager(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceUndoManager *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_undo_manager(GTK_SOURCE_BUFFER(self->obj));
  nsp_type_gtksourceundomanager = new_type_gtksourceundomanager(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceundomanager))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_buffer_set_undo_manager(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *manager;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourceundomanager, &manager) == FAIL) return RET_BUG;
    gtk_source_buffer_set_undo_manager(GTK_SOURCE_BUFFER(self->obj),GTK_SOURCE_UNDO_MANAGER(manager->obj));
  return 0;
}

static int _wrap_gtk_source_buffer_set_implicit_trailing_newline(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int implicit_trailing_newline;
  if ( GetArgs(stack,rhs,opt,T,&implicit_trailing_newline) == FAIL) return RET_BUG;
    gtk_source_buffer_set_implicit_trailing_newline(GTK_SOURCE_BUFFER(self->obj),implicit_trailing_newline);
  return 0;
}

static int _wrap_gtk_source_buffer_get_implicit_trailing_newline(NspGtkSourceBuffer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_buffer_get_implicit_trailing_newline(GTK_SOURCE_BUFFER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcebuffer_methods[] = {
  {"get_highlight_syntax",(nsp_method *) _wrap_gtk_source_buffer_get_highlight_syntax},
  {"set_highlight_syntax",(nsp_method *) _wrap_gtk_source_buffer_set_highlight_syntax},
  {"get_highlight_matching_brackets",(nsp_method *) _wrap_gtk_source_buffer_get_highlight_matching_brackets},
  {"set_highlight_matching_brackets",(nsp_method *) _wrap_gtk_source_buffer_set_highlight_matching_brackets},
  {"get_max_undo_levels",(nsp_method *) _wrap_gtk_source_buffer_get_max_undo_levels},
  {"set_max_undo_levels",(nsp_method *) _wrap_gtk_source_buffer_set_max_undo_levels},
  {"get_language",(nsp_method *) _wrap_gtk_source_buffer_get_language},
  {"set_language",(nsp_method *) _wrap_gtk_source_buffer_set_language},
  {"can_undo",(nsp_method *) _wrap_gtk_source_buffer_can_undo},
  {"can_redo",(nsp_method *) _wrap_gtk_source_buffer_can_redo},
  {"get_style_scheme",(nsp_method *) _wrap_gtk_source_buffer_get_style_scheme},
  {"set_style_scheme",(nsp_method *) _wrap_gtk_source_buffer_set_style_scheme},
  {"ensure_highlight",(nsp_method *) _wrap_gtk_source_buffer_ensure_highlight},
  {"undo",(nsp_method *) _wrap_gtk_source_buffer_undo},
  {"redo",(nsp_method *) _wrap_gtk_source_buffer_redo},
  {"begin_not_undoable_action",(nsp_method *) _wrap_gtk_source_buffer_begin_not_undoable_action},
  {"end_not_undoable_action",(nsp_method *) _wrap_gtk_source_buffer_end_not_undoable_action},
  {"create_source_mark",(nsp_method *) _wrap_gtk_source_buffer_create_source_mark},
  {"forward_iter_to_source_mark",(nsp_method *) _wrap_gtk_source_buffer_forward_iter_to_source_mark},
  {"backward_iter_to_source_mark",(nsp_method *) _wrap_gtk_source_buffer_backward_iter_to_source_mark},
  {"get_source_marks_at_iter",(nsp_method *) _wrap_gtk_source_buffer_get_source_marks_at_iter},
  {"get_source_marks_at_line",(nsp_method *) _wrap_gtk_source_buffer_get_source_marks_at_line},
  {"remove_source_marks",(nsp_method *) _wrap_gtk_source_buffer_remove_source_marks},
  {"iter_has_context_class",(nsp_method *) _wrap_gtk_source_buffer_iter_has_context_class},
  {"get_context_classes_at_iter",(nsp_method *) _wrap_gtk_source_buffer_get_context_classes_at_iter},
  {"iter_forward_to_context_class_toggle",(nsp_method *) _wrap_gtk_source_buffer_iter_forward_to_context_class_toggle},
  {"iter_backward_to_context_class_toggle",(nsp_method *) _wrap_gtk_source_buffer_iter_backward_to_context_class_toggle},
  {"change_case",(nsp_method *) _wrap_gtk_source_buffer_change_case},
  {"join_lines",(nsp_method *) _wrap_gtk_source_buffer_join_lines},
  {"sort_lines",(nsp_method *) _wrap_gtk_source_buffer_sort_lines},
  {"get_undo_manager",(nsp_method *) _wrap_gtk_source_buffer_get_undo_manager},
  {"set_undo_manager",(nsp_method *) _wrap_gtk_source_buffer_set_undo_manager},
  {"set_implicit_trailing_newline",(nsp_method *) _wrap_gtk_source_buffer_set_implicit_trailing_newline},
  {"get_implicit_trailing_newline",(nsp_method *) _wrap_gtk_source_buffer_get_implicit_trailing_newline},
  { NULL, NULL}
};

static NspMethods *gtksourcebuffer_get_methods(void) { return gtksourcebuffer_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcebuffer_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceCompletion ----------- */


#define  NspGtkSourceCompletion_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcecompletion.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceCompletion inherits from GObject 
 */

int nsp_type_gtksourcecompletion_id=0;
NspTypeGtkSourceCompletion *nsp_type_gtksourcecompletion=NULL;

/*
 * Type object for NspGtkSourceCompletion 
 * all the instance of NspTypeGtkSourceCompletion share the same id. 
 * nsp_type_gtksourcecompletion: is an instance of NspTypeGtkSourceCompletion 
 *    used for objects of NspGtkSourceCompletion type (i.e built with new_gtksourcecompletion) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceCompletion *new_type_gtksourcecompletion(type_mode mode)
{
  NspTypeGtkSourceCompletion *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcecompletion != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcecompletion;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcecompletion_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcecompletion_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcecompletion;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcecompletion */ 

  top->s_type =  (s_type_func *) nsp_gtksourcecompletion_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcecompletion_type_short_string;
  /* top->create = (create_func*) int_gtksourcecompletion_create;*/

  /* specific methods for gtksourcecompletion */

  type->init = (init_func *) init_gtksourcecompletion;

  /* 
   * NspGtkSourceCompletion interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcecompletion_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceCompletion called nsp_type_gtksourcecompletion
       */
      type->id =  nsp_type_gtksourcecompletion_id = nsp_new_type_id();
      nsp_type_gtksourcecompletion = type;
      if ( nsp_register_type(nsp_type_gtksourcecompletion) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcecompletion, GTK_SOURCE_TYPE_COMPLETION);
      return ( mode == T_BASE ) ? type : new_type_gtksourcecompletion(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcecompletion_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceCompletion instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcecompletion(NspGtkSourceCompletion *Obj,NspTypeGtkSourceCompletion *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceCompletion 
 */

NspGtkSourceCompletion *new_gtksourcecompletion() 
{
  NspGtkSourceCompletion *loc;
  /* type must exists */
  nsp_type_gtksourcecompletion = new_type_gtksourcecompletion(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceCompletion)))== NULLGTKSOURCECOMPLETION) return loc;
  /* initialize object */
  if ( init_gtksourcecompletion(loc,nsp_type_gtksourcecompletion) == FAIL) return NULLGTKSOURCECOMPLETION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceCompletion 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcecompletion_type_name[]="GtkSourceCompletion";
static char gtksourcecompletion_short_type_name[]="GtkSourceCompletion";

static char *nsp_gtksourcecompletion_type_as_string(void)
{
  return(gtksourcecompletion_type_name);
}

static char *nsp_gtksourcecompletion_type_short_string(NspObject *v)
{
  return(gtksourcecompletion_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceCompletion objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceCompletion   *nsp_gtksourcecompletion_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcecompletion_id)  == TRUE  ) return ((NspGtkSourceCompletion *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcecompletion));
  return NULL;
}

int IsGtkSourceCompletionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcecompletion_id);
}

int IsGtkSourceCompletion(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcecompletion_id);
}

NspGtkSourceCompletion  *GetGtkSourceCompletionCopy(Stack stack, int i)
{
  if (  GetGtkSourceCompletion(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceCompletion  *GetGtkSourceCompletion(Stack stack, int i)
{
  NspGtkSourceCompletion *M;
  if (( M = nsp_gtksourcecompletion_object(NthObj(i))) == NULLGTKSOURCECOMPLETION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceCompletion *gtksourcecompletion_copy(NspGtkSourceCompletion *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletion);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletion);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceCompletion
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_completion_add_provider(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *provider;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionprovider, &provider) == FAIL) return RET_BUG;
    ret =gtk_source_completion_add_provider(GTK_SOURCE_COMPLETION(self->obj),GTK_SOURCE_COMPLETION_PROVIDER(provider->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_remove_provider(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *provider;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionprovider, &provider) == FAIL) return RET_BUG;
    ret =gtk_source_completion_remove_provider(GTK_SOURCE_COMPLETION(self->obj),GTK_SOURCE_COMPLETION_PROVIDER(provider->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_get_providers(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gtk_source_completion_get_providers(GTK_SOURCE_COMPLETION(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_gtk_source_completion_show(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list,obj_check, t_end};
  NspList *nsp_providers;
  GList *providers;
  NspGObject *context;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_providers, &nsp_type_gtksourcecompletioncontext, &context) == FAIL) return RET_BUG;
  providers=nsp_glist_from_nsplist(stack,nsp_providers);
  if (providers== NULL) return RET_BUG;
    ret =gtk_source_completion_show(GTK_SOURCE_COMPLETION(self->obj),providers,GTK_SOURCE_COMPLETION_CONTEXT(context->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_hide(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_completion_hide(GTK_SOURCE_COMPLETION(self->obj));
  return 0;
}

static int _wrap_gtk_source_completion_get_info_window(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceCompletionInfo *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_get_info_window(GTK_SOURCE_COMPLETION(self->obj));
  nsp_type_gtksourcecompletioninfo = new_type_gtksourcecompletioninfo(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcecompletioninfo))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_get_view(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceView *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_get_view(GTK_SOURCE_COMPLETION(self->obj));
  nsp_type_gtksourceview = new_type_gtksourceview(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceview))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_create_context(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkTextIter *position = NULL;
  NspObject *nsp_position = NULL, *nsp_ret;
  GtkSourceCompletionContext *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_position) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_position, GTK_TYPE_TEXT_ITER))
      position = nspg_boxed_get(nsp_position, GtkTextIter);
  else {
      Scierror( "Error: position should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_completion_create_context(GTK_SOURCE_COMPLETION(self->obj),position);
  nsp_type_gtksourcecompletioncontext = new_type_gtksourcecompletioncontext(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcecompletioncontext))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_block_interactive(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_completion_block_interactive(GTK_SOURCE_COMPLETION(self->obj));
  return 0;
}

static int _wrap_gtk_source_completion_unblock_interactive(NspGtkSourceCompletion *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_completion_unblock_interactive(GTK_SOURCE_COMPLETION(self->obj));
  return 0;
}

static NspMethods gtksourcecompletion_methods[] = {
  {"add_provider",(nsp_method *) _wrap_gtk_source_completion_add_provider},
  {"remove_provider",(nsp_method *) _wrap_gtk_source_completion_remove_provider},
  {"get_providers",(nsp_method *) _wrap_gtk_source_completion_get_providers},
  {"show",(nsp_method *) _wrap_gtk_source_completion_show},
  {"hide",(nsp_method *) _wrap_gtk_source_completion_hide},
  {"get_info_window",(nsp_method *) _wrap_gtk_source_completion_get_info_window},
  {"get_view",(nsp_method *) _wrap_gtk_source_completion_get_view},
  {"create_context",(nsp_method *) _wrap_gtk_source_completion_create_context},
  {"block_interactive",(nsp_method *) _wrap_gtk_source_completion_block_interactive},
  {"unblock_interactive",(nsp_method *) _wrap_gtk_source_completion_unblock_interactive},
  { NULL, NULL}
};

static NspMethods *gtksourcecompletion_get_methods(void) { return gtksourcecompletion_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcecompletion_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceCompletionContext ----------- */


#define  NspGtkSourceCompletionContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcecompletioncontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceCompletionContext inherits from GObject 
 */

int nsp_type_gtksourcecompletioncontext_id=0;
NspTypeGtkSourceCompletionContext *nsp_type_gtksourcecompletioncontext=NULL;

/*
 * Type object for NspGtkSourceCompletionContext 
 * all the instance of NspTypeGtkSourceCompletionContext share the same id. 
 * nsp_type_gtksourcecompletioncontext: is an instance of NspTypeGtkSourceCompletionContext 
 *    used for objects of NspGtkSourceCompletionContext type (i.e built with new_gtksourcecompletioncontext) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceCompletionContext *new_type_gtksourcecompletioncontext(type_mode mode)
{
  NspTypeGtkSourceCompletionContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcecompletioncontext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcecompletioncontext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcecompletioncontext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcecompletioncontext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcecompletioncontext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcecompletioncontext */ 

  top->s_type =  (s_type_func *) nsp_gtksourcecompletioncontext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcecompletioncontext_type_short_string;
  /* top->create = (create_func*) int_gtksourcecompletioncontext_create;*/

  /* specific methods for gtksourcecompletioncontext */

  type->init = (init_func *) init_gtksourcecompletioncontext;

  /* 
   * NspGtkSourceCompletionContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcecompletioncontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceCompletionContext called nsp_type_gtksourcecompletioncontext
       */
      type->id =  nsp_type_gtksourcecompletioncontext_id = nsp_new_type_id();
      nsp_type_gtksourcecompletioncontext = type;
      if ( nsp_register_type(nsp_type_gtksourcecompletioncontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcecompletioncontext, GTK_SOURCE_TYPE_COMPLETION_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_gtksourcecompletioncontext(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcecompletioncontext_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceCompletionContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcecompletioncontext(NspGtkSourceCompletionContext *Obj,NspTypeGtkSourceCompletionContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceCompletionContext 
 */

NspGtkSourceCompletionContext *new_gtksourcecompletioncontext() 
{
  NspGtkSourceCompletionContext *loc;
  /* type must exists */
  nsp_type_gtksourcecompletioncontext = new_type_gtksourcecompletioncontext(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceCompletionContext)))== NULLGTKSOURCECOMPLETIONCONTEXT) return loc;
  /* initialize object */
  if ( init_gtksourcecompletioncontext(loc,nsp_type_gtksourcecompletioncontext) == FAIL) return NULLGTKSOURCECOMPLETIONCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceCompletionContext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcecompletioncontext_type_name[]="GtkSourceCompletionContext";
static char gtksourcecompletioncontext_short_type_name[]="GtkSourceCompletionContext";

static char *nsp_gtksourcecompletioncontext_type_as_string(void)
{
  return(gtksourcecompletioncontext_type_name);
}

static char *nsp_gtksourcecompletioncontext_type_short_string(NspObject *v)
{
  return(gtksourcecompletioncontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceCompletionContext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceCompletionContext   *nsp_gtksourcecompletioncontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcecompletioncontext_id)  == TRUE  ) return ((NspGtkSourceCompletionContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcecompletioncontext));
  return NULL;
}

int IsGtkSourceCompletionContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcecompletioncontext_id);
}

int IsGtkSourceCompletionContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcecompletioncontext_id);
}

NspGtkSourceCompletionContext  *GetGtkSourceCompletionContextCopy(Stack stack, int i)
{
  if (  GetGtkSourceCompletionContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceCompletionContext  *GetGtkSourceCompletionContext(Stack stack, int i)
{
  NspGtkSourceCompletionContext *M;
  if (( M = nsp_gtksourcecompletioncontext_object(NthObj(i))) == NULLGTKSOURCECOMPLETIONCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceCompletionContext *gtksourcecompletioncontext_copy(NspGtkSourceCompletionContext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletioncontext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletioncontext);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceCompletionContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_completion_context_add_proposals(NspGtkSourceCompletionContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,list,s_bool, t_end};
  NspGObject *provider;
  NspList *nsp_proposals;
  GList *proposals;
  int finished;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionprovider, &provider, &nsp_proposals, &finished) == FAIL) return RET_BUG;
  proposals=nsp_glist_from_nsplist(stack,nsp_proposals);
  if (proposals== NULL) return RET_BUG;
    gtk_source_completion_context_add_proposals(GTK_SOURCE_COMPLETION_CONTEXT(self->obj),GTK_SOURCE_COMPLETION_PROVIDER(provider->obj),proposals,finished);
  return 0;
}

static int _wrap_gtk_source_completion_context_get_iter(NspGtkSourceCompletionContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_completion_context_get_iter(GTK_SOURCE_COMPLETION_CONTEXT(self->obj),iter);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_context_get_activation(NspGtkSourceCompletionContext *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_context_get_activation(GTK_SOURCE_COMPLETION_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcecompletioncontext_methods[] = {
  {"add_proposals",(nsp_method *) _wrap_gtk_source_completion_context_add_proposals},
  {"get_iter",(nsp_method *) _wrap_gtk_source_completion_context_get_iter},
  {"get_activation",(nsp_method *) _wrap_gtk_source_completion_context_get_activation},
  { NULL, NULL}
};

static NspMethods *gtksourcecompletioncontext_get_methods(void) { return gtksourcecompletioncontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcecompletioncontext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceCompletionInfo ----------- */


#define  NspGtkSourceCompletionInfo_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcecompletioninfo.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceCompletionInfo inherits from GtkWindow 
 */

int nsp_type_gtksourcecompletioninfo_id=0;
NspTypeGtkSourceCompletionInfo *nsp_type_gtksourcecompletioninfo=NULL;

/*
 * Type object for NspGtkSourceCompletionInfo 
 * all the instance of NspTypeGtkSourceCompletionInfo share the same id. 
 * nsp_type_gtksourcecompletioninfo: is an instance of NspTypeGtkSourceCompletionInfo 
 *    used for objects of NspGtkSourceCompletionInfo type (i.e built with new_gtksourcecompletioninfo) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceCompletionInfo *new_type_gtksourcecompletioninfo(type_mode mode)
{
  NspTypeGtkSourceCompletionInfo *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcecompletioninfo != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcecompletioninfo;
    }
  if (( type =  malloc(sizeof(NspTypeGtkWindow))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtkwindow(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcecompletioninfo_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcecompletioninfo_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcecompletioninfo;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcecompletioninfo */ 

  top->s_type =  (s_type_func *) nsp_gtksourcecompletioninfo_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcecompletioninfo_type_short_string;
  /* top->create = (create_func*) int_gtksourcecompletioninfo_create;*/

  /* specific methods for gtksourcecompletioninfo */

  type->init = (init_func *) init_gtksourcecompletioninfo;

  /* 
   * NspGtkSourceCompletionInfo interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcecompletioninfo_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceCompletionInfo called nsp_type_gtksourcecompletioninfo
       */
      type->id =  nsp_type_gtksourcecompletioninfo_id = nsp_new_type_id();
      nsp_type_gtksourcecompletioninfo = type;
      if ( nsp_register_type(nsp_type_gtksourcecompletioninfo) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcecompletioninfo, GTK_SOURCE_TYPE_COMPLETION_INFO);
      return ( mode == T_BASE ) ? type : new_type_gtksourcecompletioninfo(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcecompletioninfo_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceCompletionInfo instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcecompletioninfo(NspGtkSourceCompletionInfo *Obj,NspTypeGtkSourceCompletionInfo *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceCompletionInfo 
 */

NspGtkSourceCompletionInfo *new_gtksourcecompletioninfo() 
{
  NspGtkSourceCompletionInfo *loc;
  /* type must exists */
  nsp_type_gtksourcecompletioninfo = new_type_gtksourcecompletioninfo(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceCompletionInfo)))== NULLGTKSOURCECOMPLETIONINFO) return loc;
  /* initialize object */
  if ( init_gtksourcecompletioninfo(loc,nsp_type_gtksourcecompletioninfo) == FAIL) return NULLGTKSOURCECOMPLETIONINFO;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceCompletionInfo 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcecompletioninfo_type_name[]="GtkSourceCompletionInfo";
static char gtksourcecompletioninfo_short_type_name[]="GtkSourceCompletionInfo";

static char *nsp_gtksourcecompletioninfo_type_as_string(void)
{
  return(gtksourcecompletioninfo_type_name);
}

static char *nsp_gtksourcecompletioninfo_type_short_string(NspObject *v)
{
  return(gtksourcecompletioninfo_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceCompletionInfo objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceCompletionInfo   *nsp_gtksourcecompletioninfo_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcecompletioninfo_id)  == TRUE  ) return ((NspGtkSourceCompletionInfo *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcecompletioninfo));
  return NULL;
}

int IsGtkSourceCompletionInfoObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcecompletioninfo_id);
}

int IsGtkSourceCompletionInfo(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcecompletioninfo_id);
}

NspGtkSourceCompletionInfo  *GetGtkSourceCompletionInfoCopy(Stack stack, int i)
{
  if (  GetGtkSourceCompletionInfo(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceCompletionInfo  *GetGtkSourceCompletionInfo(Stack stack, int i)
{
  NspGtkSourceCompletionInfo *M;
  if (( M = nsp_gtksourcecompletioninfo_object(NthObj(i))) == NULLGTKSOURCECOMPLETIONINFO)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceCompletionInfo *gtksourcecompletioninfo_copy(NspGtkSourceCompletionInfo *self)
{
  /* return gtkwindow_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletioninfo);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletioninfo);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceCompletionInfo
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_completion_info_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_completion_info_new())== NULL) return RET_BUG;

  nsp_type_gtksourcecompletioninfo = new_type_gtksourcecompletioninfo(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcecompletioninfo);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_info_move_to_iter(NspGtkSourceCompletionInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *view;
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtktextview, &view, &nsp_iter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_completion_info_move_to_iter(GTK_SOURCE_COMPLETION_INFO(self->obj),GTK_TEXT_VIEW(view->obj),iter);
  return 0;
}

static NspMethods gtksourcecompletioninfo_methods[] = {
  {"move_to_iter",(nsp_method *) _wrap_gtk_source_completion_info_move_to_iter},
  { NULL, NULL}
};

static NspMethods *gtksourcecompletioninfo_get_methods(void) { return gtksourcecompletioninfo_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcecompletioninfo_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceCompletionItem ----------- */


#define  NspGtkSourceCompletionItem_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcecompletionitem.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceCompletionItem inherits from GObject 
 */

int nsp_type_gtksourcecompletionitem_id=0;
NspTypeGtkSourceCompletionItem *nsp_type_gtksourcecompletionitem=NULL;

/*
 * Type object for NspGtkSourceCompletionItem 
 * all the instance of NspTypeGtkSourceCompletionItem share the same id. 
 * nsp_type_gtksourcecompletionitem: is an instance of NspTypeGtkSourceCompletionItem 
 *    used for objects of NspGtkSourceCompletionItem type (i.e built with new_gtksourcecompletionitem) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceCompletionItem *new_type_gtksourcecompletionitem(type_mode mode)
{
  NspTypeGtkSourceCompletionItem *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcecompletionitem != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcecompletionitem;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcecompletionitem_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcecompletionitem_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcecompletionitem;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcecompletionitem */ 

  top->s_type =  (s_type_func *) nsp_gtksourcecompletionitem_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcecompletionitem_type_short_string;
  /* top->create = (create_func*) int_gtksourcecompletionitem_create;*/

  /* specific methods for gtksourcecompletionitem */

  type->init = (init_func *) init_gtksourcecompletionitem;

  /* 
   * NspGtkSourceCompletionItem interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcecompletionitem_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceCompletionItem called nsp_type_gtksourcecompletionitem
       */
      type->id =  nsp_type_gtksourcecompletionitem_id = nsp_new_type_id();
      nsp_type_gtksourcecompletionitem = type;
      if ( nsp_register_type(nsp_type_gtksourcecompletionitem) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcecompletionitem, GTK_SOURCE_TYPE_COMPLETION_ITEM);
      return ( mode == T_BASE ) ? type : new_type_gtksourcecompletionitem(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcecompletionitem_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceCompletionItem instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcecompletionitem(NspGtkSourceCompletionItem *Obj,NspTypeGtkSourceCompletionItem *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceCompletionItem 
 */

NspGtkSourceCompletionItem *new_gtksourcecompletionitem() 
{
  NspGtkSourceCompletionItem *loc;
  /* type must exists */
  nsp_type_gtksourcecompletionitem = new_type_gtksourcecompletionitem(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceCompletionItem)))== NULLGTKSOURCECOMPLETIONITEM) return loc;
  /* initialize object */
  if ( init_gtksourcecompletionitem(loc,nsp_type_gtksourcecompletionitem) == FAIL) return NULLGTKSOURCECOMPLETIONITEM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceCompletionItem 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcecompletionitem_type_name[]="GtkSourceCompletionItem";
static char gtksourcecompletionitem_short_type_name[]="GtkSourceCompletionItem";

static char *nsp_gtksourcecompletionitem_type_as_string(void)
{
  return(gtksourcecompletionitem_type_name);
}

static char *nsp_gtksourcecompletionitem_type_short_string(NspObject *v)
{
  return(gtksourcecompletionitem_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceCompletionItem objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceCompletionItem   *nsp_gtksourcecompletionitem_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcecompletionitem_id)  == TRUE  ) return ((NspGtkSourceCompletionItem *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcecompletionitem));
  return NULL;
}

int IsGtkSourceCompletionItemObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcecompletionitem_id);
}

int IsGtkSourceCompletionItem(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcecompletionitem_id);
}

NspGtkSourceCompletionItem  *GetGtkSourceCompletionItemCopy(Stack stack, int i)
{
  if (  GetGtkSourceCompletionItem(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceCompletionItem  *GetGtkSourceCompletionItem(Stack stack, int i)
{
  NspGtkSourceCompletionItem *M;
  if (( M = nsp_gtksourcecompletionitem_object(NthObj(i))) == NULLGTKSOURCECOMPLETIONITEM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceCompletionItem *gtksourcecompletionitem_copy(NspGtkSourceCompletionItem *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletionitem);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletionitem);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceCompletionItem
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_completion_item_new_with_markup (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string,obj_check,string, t_end};
  char *markup, *text, *info;
  NspGObject *icon;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&markup, &text, &nsp_type_gdkpixbuf, &icon, &info) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_completion_item_new_with_markup(markup,text,GDK_PIXBUF(icon->obj),info))== NULL) return RET_BUG;

  nsp_type_gtksourcecompletionitem = new_type_gtksourcecompletionitem(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcecompletionitem);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gtk_source_completion_item_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string,obj_check,string, t_end};
  char *label, *text, *info;
  NspGObject *icon;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&label, &text, &nsp_type_gdkpixbuf, &icon, &info) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_completion_item_new(label,text,GDK_PIXBUF(icon->obj),info))== NULL) return RET_BUG;

  nsp_type_gtksourcecompletionitem = new_type_gtksourcecompletionitem(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcecompletionitem);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gtksourcecompletionitem_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcecompletionitem_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceCompletionProposal ----------- */


#define  NspGtkSourceCompletionProposal_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcecompletionproposal.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceCompletionProposal inherits from GObject 
 */

int nsp_type_gtksourcecompletionproposal_id=0;
NspTypeGtkSourceCompletionProposal *nsp_type_gtksourcecompletionproposal=NULL;

/*
 * Type object for NspGtkSourceCompletionProposal 
 * all the instance of NspTypeGtkSourceCompletionProposal share the same id. 
 * nsp_type_gtksourcecompletionproposal: is an instance of NspTypeGtkSourceCompletionProposal 
 *    used for objects of NspGtkSourceCompletionProposal type (i.e built with new_gtksourcecompletionproposal) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceCompletionProposal *new_type_gtksourcecompletionproposal(type_mode mode)
{
  NspTypeGtkSourceCompletionProposal *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcecompletionproposal != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcecompletionproposal;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcecompletionproposal_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcecompletionproposal_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcecompletionproposal;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcecompletionproposal */ 

  top->s_type =  (s_type_func *) nsp_gtksourcecompletionproposal_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcecompletionproposal_type_short_string;
  /* top->create = (create_func*) int_gtksourcecompletionproposal_create;*/

  /* specific methods for gtksourcecompletionproposal */

  type->init = (init_func *) init_gtksourcecompletionproposal;

  /* 
   * NspGtkSourceCompletionProposal interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcecompletionproposal_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceCompletionProposal called nsp_type_gtksourcecompletionproposal
       */
      type->id =  nsp_type_gtksourcecompletionproposal_id = nsp_new_type_id();
      nsp_type_gtksourcecompletionproposal = type;
      if ( nsp_register_type(nsp_type_gtksourcecompletionproposal) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcecompletionproposal, GTK_SOURCE_TYPE_COMPLETION_PROPOSAL);
      return ( mode == T_BASE ) ? type : new_type_gtksourcecompletionproposal(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcecompletionproposal_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceCompletionProposal instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcecompletionproposal(NspGtkSourceCompletionProposal *Obj,NspTypeGtkSourceCompletionProposal *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceCompletionProposal 
 */

NspGtkSourceCompletionProposal *new_gtksourcecompletionproposal() 
{
  NspGtkSourceCompletionProposal *loc;
  /* type must exists */
  nsp_type_gtksourcecompletionproposal = new_type_gtksourcecompletionproposal(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceCompletionProposal)))== NULLGTKSOURCECOMPLETIONPROPOSAL) return loc;
  /* initialize object */
  if ( init_gtksourcecompletionproposal(loc,nsp_type_gtksourcecompletionproposal) == FAIL) return NULLGTKSOURCECOMPLETIONPROPOSAL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceCompletionProposal 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcecompletionproposal_type_name[]="GtkSourceCompletionProposal";
static char gtksourcecompletionproposal_short_type_name[]="GtkSourceCompletionProposal";

static char *nsp_gtksourcecompletionproposal_type_as_string(void)
{
  return(gtksourcecompletionproposal_type_name);
}

static char *nsp_gtksourcecompletionproposal_type_short_string(NspObject *v)
{
  return(gtksourcecompletionproposal_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceCompletionProposal objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceCompletionProposal   *nsp_gtksourcecompletionproposal_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcecompletionproposal_id)  == TRUE  ) return ((NspGtkSourceCompletionProposal *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcecompletionproposal));
  return NULL;
}

int IsGtkSourceCompletionProposalObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcecompletionproposal_id);
}

int IsGtkSourceCompletionProposal(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcecompletionproposal_id);
}

NspGtkSourceCompletionProposal  *GetGtkSourceCompletionProposalCopy(Stack stack, int i)
{
  if (  GetGtkSourceCompletionProposal(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceCompletionProposal  *GetGtkSourceCompletionProposal(Stack stack, int i)
{
  NspGtkSourceCompletionProposal *M;
  if (( M = nsp_gtksourcecompletionproposal_object(NthObj(i))) == NULLGTKSOURCECOMPLETIONPROPOSAL)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceCompletionProposal *gtksourcecompletionproposal_copy(NspGtkSourceCompletionProposal *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletionproposal);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletionproposal);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceCompletionProposal
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_completion_proposal_get_label(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_label(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_completion_proposal_get_markup(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_markup(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_completion_proposal_get_text(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_text(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_completion_proposal_get_icon(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_icon(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_proposal_get_icon_name(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_icon_name(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_proposal_get_gicon(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_gicon(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_proposal_get_info(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_get_info(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_completion_proposal_changed(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_completion_proposal_changed(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  return 0;
}

static int _wrap_gtk_source_completion_proposal_hash(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_proposal_hash(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_proposal_equal(NspGtkSourceCompletionProposal *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *other;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionproposal, &other) == FAIL) return RET_BUG;
    ret =gtk_source_completion_proposal_equal(GTK_SOURCE_COMPLETION_PROPOSAL(self->obj),GTK_SOURCE_COMPLETION_PROPOSAL(other->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcecompletionproposal_methods[] = {
  {"get_label",(nsp_method *) _wrap_gtk_source_completion_proposal_get_label},
  {"get_markup",(nsp_method *) _wrap_gtk_source_completion_proposal_get_markup},
  {"get_text",(nsp_method *) _wrap_gtk_source_completion_proposal_get_text},
  {"get_icon",(nsp_method *) _wrap_gtk_source_completion_proposal_get_icon},
  {"get_icon_name",(nsp_method *) _wrap_gtk_source_completion_proposal_get_icon_name},
  {"get_gicon",(nsp_method *) _wrap_gtk_source_completion_proposal_get_gicon},
  {"get_info",(nsp_method *) _wrap_gtk_source_completion_proposal_get_info},
  {"changed",(nsp_method *) _wrap_gtk_source_completion_proposal_changed},
  {"hash",(nsp_method *) _wrap_gtk_source_completion_proposal_hash},
  {"equal",(nsp_method *) _wrap_gtk_source_completion_proposal_equal},
  { NULL, NULL}
};

static NspMethods *gtksourcecompletionproposal_get_methods(void) { return gtksourcecompletionproposal_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcecompletionproposal_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceCompletionProvider ----------- */


#define  NspGtkSourceCompletionProvider_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcecompletionprovider.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceCompletionProvider inherits from GObject 
 */

int nsp_type_gtksourcecompletionprovider_id=0;
NspTypeGtkSourceCompletionProvider *nsp_type_gtksourcecompletionprovider=NULL;

/*
 * Type object for NspGtkSourceCompletionProvider 
 * all the instance of NspTypeGtkSourceCompletionProvider share the same id. 
 * nsp_type_gtksourcecompletionprovider: is an instance of NspTypeGtkSourceCompletionProvider 
 *    used for objects of NspGtkSourceCompletionProvider type (i.e built with new_gtksourcecompletionprovider) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceCompletionProvider *new_type_gtksourcecompletionprovider(type_mode mode)
{
  NspTypeGtkSourceCompletionProvider *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcecompletionprovider != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcecompletionprovider;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcecompletionprovider_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcecompletionprovider_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcecompletionprovider;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcecompletionprovider */ 

  top->s_type =  (s_type_func *) nsp_gtksourcecompletionprovider_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcecompletionprovider_type_short_string;
  /* top->create = (create_func*) int_gtksourcecompletionprovider_create;*/

  /* specific methods for gtksourcecompletionprovider */

  type->init = (init_func *) init_gtksourcecompletionprovider;

  /* 
   * NspGtkSourceCompletionProvider interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcecompletionprovider_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceCompletionProvider called nsp_type_gtksourcecompletionprovider
       */
      type->id =  nsp_type_gtksourcecompletionprovider_id = nsp_new_type_id();
      nsp_type_gtksourcecompletionprovider = type;
      if ( nsp_register_type(nsp_type_gtksourcecompletionprovider) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcecompletionprovider, GTK_SOURCE_TYPE_COMPLETION_PROVIDER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcecompletionprovider(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcecompletionprovider_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceCompletionProvider instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcecompletionprovider(NspGtkSourceCompletionProvider *Obj,NspTypeGtkSourceCompletionProvider *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceCompletionProvider 
 */

NspGtkSourceCompletionProvider *new_gtksourcecompletionprovider() 
{
  NspGtkSourceCompletionProvider *loc;
  /* type must exists */
  nsp_type_gtksourcecompletionprovider = new_type_gtksourcecompletionprovider(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceCompletionProvider)))== NULLGTKSOURCECOMPLETIONPROVIDER) return loc;
  /* initialize object */
  if ( init_gtksourcecompletionprovider(loc,nsp_type_gtksourcecompletionprovider) == FAIL) return NULLGTKSOURCECOMPLETIONPROVIDER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceCompletionProvider 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcecompletionprovider_type_name[]="GtkSourceCompletionProvider";
static char gtksourcecompletionprovider_short_type_name[]="GtkSourceCompletionProvider";

static char *nsp_gtksourcecompletionprovider_type_as_string(void)
{
  return(gtksourcecompletionprovider_type_name);
}

static char *nsp_gtksourcecompletionprovider_type_short_string(NspObject *v)
{
  return(gtksourcecompletionprovider_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceCompletionProvider objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceCompletionProvider   *nsp_gtksourcecompletionprovider_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcecompletionprovider_id)  == TRUE  ) return ((NspGtkSourceCompletionProvider *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcecompletionprovider));
  return NULL;
}

int IsGtkSourceCompletionProviderObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcecompletionprovider_id);
}

int IsGtkSourceCompletionProvider(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcecompletionprovider_id);
}

NspGtkSourceCompletionProvider  *GetGtkSourceCompletionProviderCopy(Stack stack, int i)
{
  if (  GetGtkSourceCompletionProvider(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceCompletionProvider  *GetGtkSourceCompletionProvider(Stack stack, int i)
{
  NspGtkSourceCompletionProvider *M;
  if (( M = nsp_gtksourcecompletionprovider_object(NthObj(i))) == NULLGTKSOURCECOMPLETIONPROVIDER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceCompletionProvider *gtksourcecompletionprovider_copy(NspGtkSourceCompletionProvider *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletionprovider);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcecompletionprovider);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceCompletionProvider
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_completion_provider_get_name(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_name(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_completion_provider_get_icon(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_icon(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_provider_get_icon_name(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_icon_name(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_provider_get_gicon(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_gicon(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_provider_populate(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *context;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletioncontext, &context) == FAIL) return RET_BUG;
    gtk_source_completion_provider_populate(GTK_SOURCE_COMPLETION_PROVIDER(self->obj),GTK_SOURCE_COMPLETION_CONTEXT(context->obj));
  return 0;
}

static int _wrap_gtk_source_completion_provider_get_activation(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_activation(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_provider_match(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *context;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletioncontext, &context) == FAIL) return RET_BUG;
    ret =gtk_source_completion_provider_match(GTK_SOURCE_COMPLETION_PROVIDER(self->obj),GTK_SOURCE_COMPLETION_CONTEXT(context->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_provider_get_info_widget(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *proposal;
  GtkWidget *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionproposal, &proposal) == FAIL) return RET_BUG;
    ret =gtk_source_completion_provider_get_info_widget(GTK_SOURCE_COMPLETION_PROVIDER(self->obj),GTK_SOURCE_COMPLETION_PROPOSAL(proposal->obj));
  nsp_type_gtkwidget = new_type_gtkwidget(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtkwidget))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_completion_provider_update_info(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *proposal, *info;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionproposal, &proposal, &nsp_type_gtksourcecompletioninfo, &info) == FAIL) return RET_BUG;
    gtk_source_completion_provider_update_info(GTK_SOURCE_COMPLETION_PROVIDER(self->obj),GTK_SOURCE_COMPLETION_PROPOSAL(proposal->obj),GTK_SOURCE_COMPLETION_INFO(info->obj));
  return 0;
}

static int _wrap_gtk_source_completion_provider_get_start_iter(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check,obj, t_end};
  NspGObject *context, *proposal;
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletioncontext, &context, &nsp_type_gtksourcecompletionproposal, &proposal, &nsp_iter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_completion_provider_get_start_iter(GTK_SOURCE_COMPLETION_PROVIDER(self->obj),GTK_SOURCE_COMPLETION_CONTEXT(context->obj),GTK_SOURCE_COMPLETION_PROPOSAL(proposal->obj),iter);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_provider_activate_proposal(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *proposal;
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcecompletionproposal, &proposal, &nsp_iter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_completion_provider_activate_proposal(GTK_SOURCE_COMPLETION_PROVIDER(self->obj),GTK_SOURCE_COMPLETION_PROPOSAL(proposal->obj),iter);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_provider_get_interactive_delay(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_interactive_delay(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_completion_provider_get_priority(NspGtkSourceCompletionProvider *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_completion_provider_get_priority(GTK_SOURCE_COMPLETION_PROVIDER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcecompletionprovider_methods[] = {
  {"get_name",(nsp_method *) _wrap_gtk_source_completion_provider_get_name},
  {"get_icon",(nsp_method *) _wrap_gtk_source_completion_provider_get_icon},
  {"get_icon_name",(nsp_method *) _wrap_gtk_source_completion_provider_get_icon_name},
  {"get_gicon",(nsp_method *) _wrap_gtk_source_completion_provider_get_gicon},
  {"populate",(nsp_method *) _wrap_gtk_source_completion_provider_populate},
  {"get_activation",(nsp_method *) _wrap_gtk_source_completion_provider_get_activation},
  {"match",(nsp_method *) _wrap_gtk_source_completion_provider_match},
  {"get_info_widget",(nsp_method *) _wrap_gtk_source_completion_provider_get_info_widget},
  {"update_info",(nsp_method *) _wrap_gtk_source_completion_provider_update_info},
  {"get_start_iter",(nsp_method *) _wrap_gtk_source_completion_provider_get_start_iter},
  {"activate_proposal",(nsp_method *) _wrap_gtk_source_completion_provider_activate_proposal},
  {"get_interactive_delay",(nsp_method *) _wrap_gtk_source_completion_provider_get_interactive_delay},
  {"get_priority",(nsp_method *) _wrap_gtk_source_completion_provider_get_priority},
  { NULL, NULL}
};

static NspMethods *gtksourcecompletionprovider_get_methods(void) { return gtksourcecompletionprovider_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcecompletionprovider_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceFile ----------- */


#define  NspGtkSourceFile_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcefile.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceFile inherits from GObject 
 */

int nsp_type_gtksourcefile_id=0;
NspTypeGtkSourceFile *nsp_type_gtksourcefile=NULL;

/*
 * Type object for NspGtkSourceFile 
 * all the instance of NspTypeGtkSourceFile share the same id. 
 * nsp_type_gtksourcefile: is an instance of NspTypeGtkSourceFile 
 *    used for objects of NspGtkSourceFile type (i.e built with new_gtksourcefile) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceFile *new_type_gtksourcefile(type_mode mode)
{
  NspTypeGtkSourceFile *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcefile != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcefile;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcefile_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcefile_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcefile;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcefile */ 

  top->s_type =  (s_type_func *) nsp_gtksourcefile_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcefile_type_short_string;
  /* top->create = (create_func*) int_gtksourcefile_create;*/

  /* specific methods for gtksourcefile */

  type->init = (init_func *) init_gtksourcefile;

  /* 
   * NspGtkSourceFile interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcefile_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceFile called nsp_type_gtksourcefile
       */
      type->id =  nsp_type_gtksourcefile_id = nsp_new_type_id();
      nsp_type_gtksourcefile = type;
      if ( nsp_register_type(nsp_type_gtksourcefile) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcefile, GTK_SOURCE_TYPE_FILE);
      return ( mode == T_BASE ) ? type : new_type_gtksourcefile(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcefile_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceFile instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcefile(NspGtkSourceFile *Obj,NspTypeGtkSourceFile *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceFile 
 */

NspGtkSourceFile *new_gtksourcefile() 
{
  NspGtkSourceFile *loc;
  /* type must exists */
  nsp_type_gtksourcefile = new_type_gtksourcefile(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceFile)))== NULLGTKSOURCEFILE) return loc;
  /* initialize object */
  if ( init_gtksourcefile(loc,nsp_type_gtksourcefile) == FAIL) return NULLGTKSOURCEFILE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceFile 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcefile_type_name[]="GtkSourceFile";
static char gtksourcefile_short_type_name[]="GtkSourceFile";

static char *nsp_gtksourcefile_type_as_string(void)
{
  return(gtksourcefile_type_name);
}

static char *nsp_gtksourcefile_type_short_string(NspObject *v)
{
  return(gtksourcefile_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceFile objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceFile   *nsp_gtksourcefile_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcefile_id)  == TRUE  ) return ((NspGtkSourceFile *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcefile));
  return NULL;
}

int IsGtkSourceFileObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcefile_id);
}

int IsGtkSourceFile(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcefile_id);
}

NspGtkSourceFile  *GetGtkSourceFileCopy(Stack stack, int i)
{
  if (  GetGtkSourceFile(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceFile  *GetGtkSourceFile(Stack stack, int i)
{
  NspGtkSourceFile *M;
  if (( M = nsp_gtksourcefile_object(NthObj(i))) == NULLGTKSOURCEFILE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceFile *gtksourcefile_copy(NspGtkSourceFile *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcefile);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcefile);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceFile
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_file_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_file_new())== NULL) return RET_BUG;

  nsp_type_gtksourcefile = new_type_gtksourcefile(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefile);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_get_location(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_get_location(GTK_SOURCE_FILE(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_set_location(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *location;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &location) == FAIL) return RET_BUG;
    gtk_source_file_set_location(GTK_SOURCE_FILE(self->obj),G_FILE(location->obj));
  return 0;
}

static int _wrap_gtk_source_file_get_newline_type(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_get_newline_type(GTK_SOURCE_FILE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_get_compression_type(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_get_compression_type(GTK_SOURCE_FILE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_check_file_on_disk(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_file_check_file_on_disk(GTK_SOURCE_FILE(self->obj));
  return 0;
}

static int _wrap_gtk_source_file_is_local(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_file_is_local(GTK_SOURCE_FILE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_is_externally_modified(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_file_is_externally_modified(GTK_SOURCE_FILE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_is_deleted(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_file_is_deleted(GTK_SOURCE_FILE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_is_readonly(NspGtkSourceFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_file_is_readonly(GTK_SOURCE_FILE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcefile_methods[] = {
  {"get_location",(nsp_method *) _wrap_gtk_source_file_get_location},
  {"set_location",(nsp_method *) _wrap_gtk_source_file_set_location},
  {"get_newline_type",(nsp_method *) _wrap_gtk_source_file_get_newline_type},
  {"get_compression_type",(nsp_method *) _wrap_gtk_source_file_get_compression_type},
  {"check_file_on_disk",(nsp_method *) _wrap_gtk_source_file_check_file_on_disk},
  {"is_local",(nsp_method *) _wrap_gtk_source_file_is_local},
  {"is_externally_modified",(nsp_method *) _wrap_gtk_source_file_is_externally_modified},
  {"is_deleted",(nsp_method *) _wrap_gtk_source_file_is_deleted},
  {"is_readonly",(nsp_method *) _wrap_gtk_source_file_is_readonly},
  { NULL, NULL}
};

static NspMethods *gtksourcefile_get_methods(void) { return gtksourcefile_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcefile_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceFileLoader ----------- */


#define  NspGtkSourceFileLoader_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcefileloader.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceFileLoader inherits from GObject 
 */

int nsp_type_gtksourcefileloader_id=0;
NspTypeGtkSourceFileLoader *nsp_type_gtksourcefileloader=NULL;

/*
 * Type object for NspGtkSourceFileLoader 
 * all the instance of NspTypeGtkSourceFileLoader share the same id. 
 * nsp_type_gtksourcefileloader: is an instance of NspTypeGtkSourceFileLoader 
 *    used for objects of NspGtkSourceFileLoader type (i.e built with new_gtksourcefileloader) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceFileLoader *new_type_gtksourcefileloader(type_mode mode)
{
  NspTypeGtkSourceFileLoader *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcefileloader != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcefileloader;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcefileloader_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcefileloader_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcefileloader;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcefileloader */ 

  top->s_type =  (s_type_func *) nsp_gtksourcefileloader_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcefileloader_type_short_string;
  /* top->create = (create_func*) int_gtksourcefileloader_create;*/

  /* specific methods for gtksourcefileloader */

  type->init = (init_func *) init_gtksourcefileloader;

  /* 
   * NspGtkSourceFileLoader interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcefileloader_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceFileLoader called nsp_type_gtksourcefileloader
       */
      type->id =  nsp_type_gtksourcefileloader_id = nsp_new_type_id();
      nsp_type_gtksourcefileloader = type;
      if ( nsp_register_type(nsp_type_gtksourcefileloader) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcefileloader, GTK_SOURCE_TYPE_FILE_LOADER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcefileloader(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcefileloader_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceFileLoader instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcefileloader(NspGtkSourceFileLoader *Obj,NspTypeGtkSourceFileLoader *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceFileLoader 
 */

NspGtkSourceFileLoader *new_gtksourcefileloader() 
{
  NspGtkSourceFileLoader *loc;
  /* type must exists */
  nsp_type_gtksourcefileloader = new_type_gtksourcefileloader(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceFileLoader)))== NULLGTKSOURCEFILELOADER) return loc;
  /* initialize object */
  if ( init_gtksourcefileloader(loc,nsp_type_gtksourcefileloader) == FAIL) return NULLGTKSOURCEFILELOADER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceFileLoader 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcefileloader_type_name[]="GtkSourceFileLoader";
static char gtksourcefileloader_short_type_name[]="GtkSourceFileLoader";

static char *nsp_gtksourcefileloader_type_as_string(void)
{
  return(gtksourcefileloader_type_name);
}

static char *nsp_gtksourcefileloader_type_short_string(NspObject *v)
{
  return(gtksourcefileloader_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceFileLoader objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceFileLoader   *nsp_gtksourcefileloader_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcefileloader_id)  == TRUE  ) return ((NspGtkSourceFileLoader *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcefileloader));
  return NULL;
}

int IsGtkSourceFileLoaderObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcefileloader_id);
}

int IsGtkSourceFileLoader(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcefileloader_id);
}

NspGtkSourceFileLoader  *GetGtkSourceFileLoaderCopy(Stack stack, int i)
{
  if (  GetGtkSourceFileLoader(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceFileLoader  *GetGtkSourceFileLoader(Stack stack, int i)
{
  NspGtkSourceFileLoader *M;
  if (( M = nsp_gtksourcefileloader_object(NthObj(i))) == NULLGTKSOURCEFILELOADER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceFileLoader *gtksourcefileloader_copy(NspGtkSourceFileLoader *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcefileloader);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcefileloader);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceFileLoader
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_file_loader_new_from_stream (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check,obj_check, t_end};
  NspGObject *buffer, *file, *stream;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer, &nsp_type_gtksourcefile, &file, &nsp_type_ginputstream, &stream) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_file_loader_new_from_stream(GTK_SOURCE_BUFFER(buffer->obj),GTK_SOURCE_FILE(file->obj),G_INPUT_STREAM(stream->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcefileloader = new_type_gtksourcefileloader(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefileloader);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gtk_source_file_loader_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *buffer, *file;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer, &nsp_type_gtksourcefile, &file) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_file_loader_new(GTK_SOURCE_BUFFER(buffer->obj),GTK_SOURCE_FILE(file->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcefileloader = new_type_gtksourcefileloader(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefileloader);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_loader_set_candidate_encodings(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list, t_end};
  NspList *nsp_candidate_encodings;
  GSList *candidate_encodings;
  if ( GetArgs(stack,rhs,opt,T,&nsp_candidate_encodings) == FAIL) return RET_BUG;
  candidate_encodings=nsp_gslist_from_nsplist(stack,nsp_candidate_encodings);
  if (candidate_encodings== NULL) return RET_BUG;
    gtk_source_file_loader_set_candidate_encodings(GTK_SOURCE_FILE_LOADER(self->obj),candidate_encodings);
  return 0;
}

static int _wrap_gtk_source_file_loader_get_buffer(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceBuffer *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_loader_get_buffer(GTK_SOURCE_FILE_LOADER(self->obj));
  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcebuffer))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_loader_get_file(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_loader_get_file(GTK_SOURCE_FILE_LOADER(self->obj));
  nsp_type_gtksourcefile = new_type_gtksourcefile(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_loader_get_location(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_loader_get_location(GTK_SOURCE_FILE_LOADER(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_loader_get_input_stream(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  GInputStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_loader_get_input_stream(GTK_SOURCE_FILE_LOADER(self->obj));
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_loader_load_finish(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =gtk_source_file_loader_load_finish(GTK_SOURCE_FILE_LOADER(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_loader_get_newline_type(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_loader_get_newline_type(GTK_SOURCE_FILE_LOADER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_loader_get_compression_type(NspGtkSourceFileLoader *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_loader_get_compression_type(GTK_SOURCE_FILE_LOADER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcefileloader_methods[] = {
  {"set_candidate_encodings",(nsp_method *) _wrap_gtk_source_file_loader_set_candidate_encodings},
  {"get_buffer",(nsp_method *) _wrap_gtk_source_file_loader_get_buffer},
  {"get_file",(nsp_method *) _wrap_gtk_source_file_loader_get_file},
  {"get_location",(nsp_method *) _wrap_gtk_source_file_loader_get_location},
  {"get_input_stream",(nsp_method *) _wrap_gtk_source_file_loader_get_input_stream},
  {"load_finish",(nsp_method *) _wrap_gtk_source_file_loader_load_finish},
  {"get_newline_type",(nsp_method *) _wrap_gtk_source_file_loader_get_newline_type},
  {"get_compression_type",(nsp_method *) _wrap_gtk_source_file_loader_get_compression_type},
  { NULL, NULL}
};

static NspMethods *gtksourcefileloader_get_methods(void) { return gtksourcefileloader_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcefileloader_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceFileSaver ----------- */


#define  NspGtkSourceFileSaver_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcefilesaver.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceFileSaver inherits from GObject 
 */

int nsp_type_gtksourcefilesaver_id=0;
NspTypeGtkSourceFileSaver *nsp_type_gtksourcefilesaver=NULL;

/*
 * Type object for NspGtkSourceFileSaver 
 * all the instance of NspTypeGtkSourceFileSaver share the same id. 
 * nsp_type_gtksourcefilesaver: is an instance of NspTypeGtkSourceFileSaver 
 *    used for objects of NspGtkSourceFileSaver type (i.e built with new_gtksourcefilesaver) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceFileSaver *new_type_gtksourcefilesaver(type_mode mode)
{
  NspTypeGtkSourceFileSaver *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcefilesaver != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcefilesaver;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcefilesaver_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcefilesaver_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcefilesaver;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcefilesaver */ 

  top->s_type =  (s_type_func *) nsp_gtksourcefilesaver_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcefilesaver_type_short_string;
  /* top->create = (create_func*) int_gtksourcefilesaver_create;*/

  /* specific methods for gtksourcefilesaver */

  type->init = (init_func *) init_gtksourcefilesaver;

  /* 
   * NspGtkSourceFileSaver interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcefilesaver_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceFileSaver called nsp_type_gtksourcefilesaver
       */
      type->id =  nsp_type_gtksourcefilesaver_id = nsp_new_type_id();
      nsp_type_gtksourcefilesaver = type;
      if ( nsp_register_type(nsp_type_gtksourcefilesaver) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcefilesaver, GTK_SOURCE_TYPE_FILE_SAVER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcefilesaver(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcefilesaver_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceFileSaver instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcefilesaver(NspGtkSourceFileSaver *Obj,NspTypeGtkSourceFileSaver *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceFileSaver 
 */

NspGtkSourceFileSaver *new_gtksourcefilesaver() 
{
  NspGtkSourceFileSaver *loc;
  /* type must exists */
  nsp_type_gtksourcefilesaver = new_type_gtksourcefilesaver(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceFileSaver)))== NULLGTKSOURCEFILESAVER) return loc;
  /* initialize object */
  if ( init_gtksourcefilesaver(loc,nsp_type_gtksourcefilesaver) == FAIL) return NULLGTKSOURCEFILESAVER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceFileSaver 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcefilesaver_type_name[]="GtkSourceFileSaver";
static char gtksourcefilesaver_short_type_name[]="GtkSourceFileSaver";

static char *nsp_gtksourcefilesaver_type_as_string(void)
{
  return(gtksourcefilesaver_type_name);
}

static char *nsp_gtksourcefilesaver_type_short_string(NspObject *v)
{
  return(gtksourcefilesaver_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceFileSaver objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceFileSaver   *nsp_gtksourcefilesaver_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcefilesaver_id)  == TRUE  ) return ((NspGtkSourceFileSaver *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcefilesaver));
  return NULL;
}

int IsGtkSourceFileSaverObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcefilesaver_id);
}

int IsGtkSourceFileSaver(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcefilesaver_id);
}

NspGtkSourceFileSaver  *GetGtkSourceFileSaverCopy(Stack stack, int i)
{
  if (  GetGtkSourceFileSaver(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceFileSaver  *GetGtkSourceFileSaver(Stack stack, int i)
{
  NspGtkSourceFileSaver *M;
  if (( M = nsp_gtksourcefilesaver_object(NthObj(i))) == NULLGTKSOURCEFILESAVER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceFileSaver *gtksourcefilesaver_copy(NspGtkSourceFileSaver *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcefilesaver);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcefilesaver);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceFileSaver
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_file_saver_new_with_target (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check,obj_check, t_end};
  NspGObject *buffer, *file, *target_location;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer, &nsp_type_gtksourcefile, &file, &nsp_type_gfile, &target_location) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_file_saver_new_with_target(GTK_SOURCE_BUFFER(buffer->obj),GTK_SOURCE_FILE(file->obj),G_FILE(target_location->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcefilesaver = new_type_gtksourcefilesaver(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefilesaver);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gtk_source_file_saver_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *buffer, *file;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer, &nsp_type_gtksourcefile, &file) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_file_saver_new(GTK_SOURCE_BUFFER(buffer->obj),GTK_SOURCE_FILE(file->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcefilesaver = new_type_gtksourcefilesaver(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefilesaver);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_saver_get_buffer(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceBuffer *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_saver_get_buffer(GTK_SOURCE_FILE_SAVER(self->obj));
  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcebuffer))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_saver_get_file(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_saver_get_file(GTK_SOURCE_FILE_SAVER(self->obj));
  nsp_type_gtksourcefile = new_type_gtksourcefile(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcefile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_saver_get_location(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_file_saver_get_location(GTK_SOURCE_FILE_SAVER(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_file_saver_set_newline_type(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceNewlineType newline_type;
  NspObject *nsp_newline_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_newline_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GTK_SOURCE_TYPE_NEWLINE_TYPE, nsp_newline_type, &newline_type)== FAIL)
      return RET_BUG;
    gtk_source_file_saver_set_newline_type(GTK_SOURCE_FILE_SAVER(self->obj),newline_type);
  return 0;
}

static int _wrap_gtk_source_file_saver_get_newline_type(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_saver_get_newline_type(GTK_SOURCE_FILE_SAVER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_saver_set_compression_type(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceCompressionType compression_type;
  NspObject *nsp_compression_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_compression_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GTK_SOURCE_TYPE_COMPRESSION_TYPE, nsp_compression_type, &compression_type)== FAIL)
      return RET_BUG;
    gtk_source_file_saver_set_compression_type(GTK_SOURCE_FILE_SAVER(self->obj),compression_type);
  return 0;
}

static int _wrap_gtk_source_file_saver_get_compression_type(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_saver_get_compression_type(GTK_SOURCE_FILE_SAVER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_saver_set_flags(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceFileSaverFlags flags;
  NspObject *nsp_flags = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GTK_SOURCE_TYPE_FILE_SAVER_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    gtk_source_file_saver_set_flags(GTK_SOURCE_FILE_SAVER(self->obj),flags);
  return 0;
}

static int _wrap_gtk_source_file_saver_get_flags(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gtk_source_file_saver_get_flags(GTK_SOURCE_FILE_SAVER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_file_saver_save_finish(NspGtkSourceFileSaver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =gtk_source_file_saver_save_finish(GTK_SOURCE_FILE_SAVER(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcefilesaver_methods[] = {
  {"get_buffer",(nsp_method *) _wrap_gtk_source_file_saver_get_buffer},
  {"get_file",(nsp_method *) _wrap_gtk_source_file_saver_get_file},
  {"get_location",(nsp_method *) _wrap_gtk_source_file_saver_get_location},
  {"set_newline_type",(nsp_method *) _wrap_gtk_source_file_saver_set_newline_type},
  {"get_newline_type",(nsp_method *) _wrap_gtk_source_file_saver_get_newline_type},
  {"set_compression_type",(nsp_method *) _wrap_gtk_source_file_saver_set_compression_type},
  {"get_compression_type",(nsp_method *) _wrap_gtk_source_file_saver_get_compression_type},
  {"set_flags",(nsp_method *) _wrap_gtk_source_file_saver_set_flags},
  {"get_flags",(nsp_method *) _wrap_gtk_source_file_saver_get_flags},
  {"save_finish",(nsp_method *) _wrap_gtk_source_file_saver_save_finish},
  { NULL, NULL}
};

static NspMethods *gtksourcefilesaver_get_methods(void) { return gtksourcefilesaver_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcefilesaver_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceGutter ----------- */


#define  NspGtkSourceGutter_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcegutter.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceGutter inherits from GObject 
 */

int nsp_type_gtksourcegutter_id=0;
NspTypeGtkSourceGutter *nsp_type_gtksourcegutter=NULL;

/*
 * Type object for NspGtkSourceGutter 
 * all the instance of NspTypeGtkSourceGutter share the same id. 
 * nsp_type_gtksourcegutter: is an instance of NspTypeGtkSourceGutter 
 *    used for objects of NspGtkSourceGutter type (i.e built with new_gtksourcegutter) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceGutter *new_type_gtksourcegutter(type_mode mode)
{
  NspTypeGtkSourceGutter *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcegutter != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcegutter;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcegutter_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcegutter_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcegutter;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcegutter */ 

  top->s_type =  (s_type_func *) nsp_gtksourcegutter_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcegutter_type_short_string;
  /* top->create = (create_func*) int_gtksourcegutter_create;*/

  /* specific methods for gtksourcegutter */

  type->init = (init_func *) init_gtksourcegutter;

  /* 
   * NspGtkSourceGutter interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcegutter_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceGutter called nsp_type_gtksourcegutter
       */
      type->id =  nsp_type_gtksourcegutter_id = nsp_new_type_id();
      nsp_type_gtksourcegutter = type;
      if ( nsp_register_type(nsp_type_gtksourcegutter) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcegutter, GTK_SOURCE_TYPE_GUTTER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcegutter(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcegutter_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceGutter instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcegutter(NspGtkSourceGutter *Obj,NspTypeGtkSourceGutter *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceGutter 
 */

NspGtkSourceGutter *new_gtksourcegutter() 
{
  NspGtkSourceGutter *loc;
  /* type must exists */
  nsp_type_gtksourcegutter = new_type_gtksourcegutter(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceGutter)))== NULLGTKSOURCEGUTTER) return loc;
  /* initialize object */
  if ( init_gtksourcegutter(loc,nsp_type_gtksourcegutter) == FAIL) return NULLGTKSOURCEGUTTER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceGutter 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcegutter_type_name[]="GtkSourceGutter";
static char gtksourcegutter_short_type_name[]="GtkSourceGutter";

static char *nsp_gtksourcegutter_type_as_string(void)
{
  return(gtksourcegutter_type_name);
}

static char *nsp_gtksourcegutter_type_short_string(NspObject *v)
{
  return(gtksourcegutter_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceGutter objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceGutter   *nsp_gtksourcegutter_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcegutter_id)  == TRUE  ) return ((NspGtkSourceGutter *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcegutter));
  return NULL;
}

int IsGtkSourceGutterObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcegutter_id);
}

int IsGtkSourceGutter(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcegutter_id);
}

NspGtkSourceGutter  *GetGtkSourceGutterCopy(Stack stack, int i)
{
  if (  GetGtkSourceGutter(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceGutter  *GetGtkSourceGutter(Stack stack, int i)
{
  NspGtkSourceGutter *M;
  if (( M = nsp_gtksourcegutter_object(NthObj(i))) == NULLGTKSOURCEGUTTER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceGutter *gtksourcegutter_copy(NspGtkSourceGutter *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutter);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutter);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceGutter
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_gutter_insert(NspGtkSourceGutter *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *renderer;
  int position, ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcegutterrenderer, &renderer, &position) == FAIL) return RET_BUG;
    ret =gtk_source_gutter_insert(GTK_SOURCE_GUTTER(self->obj),GTK_SOURCE_GUTTER_RENDERER(renderer->obj),position);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_reorder(NspGtkSourceGutter *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *renderer;
  int position;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcegutterrenderer, &renderer, &position) == FAIL) return RET_BUG;
    gtk_source_gutter_reorder(GTK_SOURCE_GUTTER(self->obj),GTK_SOURCE_GUTTER_RENDERER(renderer->obj),position);
  return 0;
}

static int _wrap_gtk_source_gutter_remove(NspGtkSourceGutter *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *renderer;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcegutterrenderer, &renderer) == FAIL) return RET_BUG;
    gtk_source_gutter_remove(GTK_SOURCE_GUTTER(self->obj),GTK_SOURCE_GUTTER_RENDERER(renderer->obj));
  return 0;
}

static int _wrap_gtk_source_gutter_queue_draw(NspGtkSourceGutter *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_gutter_queue_draw(GTK_SOURCE_GUTTER(self->obj));
  return 0;
}

static int _wrap_gtk_source_gutter_get_renderer_at_pos(NspGtkSourceGutter *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int x, y;
  GtkSourceGutterRenderer *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&x, &y) == FAIL) return RET_BUG;
    ret =gtk_source_gutter_get_renderer_at_pos(GTK_SOURCE_GUTTER(self->obj),x,y);
  nsp_type_gtksourcegutterrenderer = new_type_gtksourcegutterrenderer(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcegutterrenderer))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gtksourcegutter_methods[] = {
  {"insert",(nsp_method *) _wrap_gtk_source_gutter_insert},
  {"reorder",(nsp_method *) _wrap_gtk_source_gutter_reorder},
  {"remove",(nsp_method *) _wrap_gtk_source_gutter_remove},
  {"queue_draw",(nsp_method *) _wrap_gtk_source_gutter_queue_draw},
  {"get_renderer_at_pos",(nsp_method *) _wrap_gtk_source_gutter_get_renderer_at_pos},
  { NULL, NULL}
};

static NspMethods *gtksourcegutter_get_methods(void) { return gtksourcegutter_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcegutter_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceGutterRenderer ----------- */


#define  NspGtkSourceGutterRenderer_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcegutterrenderer.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceGutterRenderer inherits from GObject 
 */

int nsp_type_gtksourcegutterrenderer_id=0;
NspTypeGtkSourceGutterRenderer *nsp_type_gtksourcegutterrenderer=NULL;

/*
 * Type object for NspGtkSourceGutterRenderer 
 * all the instance of NspTypeGtkSourceGutterRenderer share the same id. 
 * nsp_type_gtksourcegutterrenderer: is an instance of NspTypeGtkSourceGutterRenderer 
 *    used for objects of NspGtkSourceGutterRenderer type (i.e built with new_gtksourcegutterrenderer) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceGutterRenderer *new_type_gtksourcegutterrenderer(type_mode mode)
{
  NspTypeGtkSourceGutterRenderer *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcegutterrenderer != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcegutterrenderer;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcegutterrenderer_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcegutterrenderer_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcegutterrenderer;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcegutterrenderer */ 

  top->s_type =  (s_type_func *) nsp_gtksourcegutterrenderer_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcegutterrenderer_type_short_string;
  /* top->create = (create_func*) int_gtksourcegutterrenderer_create;*/

  /* specific methods for gtksourcegutterrenderer */

  type->init = (init_func *) init_gtksourcegutterrenderer;

  /* 
   * NspGtkSourceGutterRenderer interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcegutterrenderer_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceGutterRenderer called nsp_type_gtksourcegutterrenderer
       */
      type->id =  nsp_type_gtksourcegutterrenderer_id = nsp_new_type_id();
      nsp_type_gtksourcegutterrenderer = type;
      if ( nsp_register_type(nsp_type_gtksourcegutterrenderer) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcegutterrenderer, GTK_SOURCE_TYPE_GUTTER_RENDERER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcegutterrenderer(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcegutterrenderer_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceGutterRenderer instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcegutterrenderer(NspGtkSourceGutterRenderer *Obj,NspTypeGtkSourceGutterRenderer *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceGutterRenderer 
 */

NspGtkSourceGutterRenderer *new_gtksourcegutterrenderer() 
{
  NspGtkSourceGutterRenderer *loc;
  /* type must exists */
  nsp_type_gtksourcegutterrenderer = new_type_gtksourcegutterrenderer(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceGutterRenderer)))== NULLGTKSOURCEGUTTERRENDERER) return loc;
  /* initialize object */
  if ( init_gtksourcegutterrenderer(loc,nsp_type_gtksourcegutterrenderer) == FAIL) return NULLGTKSOURCEGUTTERRENDERER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceGutterRenderer 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcegutterrenderer_type_name[]="GtkSourceGutterRenderer";
static char gtksourcegutterrenderer_short_type_name[]="GtkSourceGutterRenderer";

static char *nsp_gtksourcegutterrenderer_type_as_string(void)
{
  return(gtksourcegutterrenderer_type_name);
}

static char *nsp_gtksourcegutterrenderer_type_short_string(NspObject *v)
{
  return(gtksourcegutterrenderer_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceGutterRenderer objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceGutterRenderer   *nsp_gtksourcegutterrenderer_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcegutterrenderer_id)  == TRUE  ) return ((NspGtkSourceGutterRenderer *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcegutterrenderer));
  return NULL;
}

int IsGtkSourceGutterRendererObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcegutterrenderer_id);
}

int IsGtkSourceGutterRenderer(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcegutterrenderer_id);
}

NspGtkSourceGutterRenderer  *GetGtkSourceGutterRendererCopy(Stack stack, int i)
{
  if (  GetGtkSourceGutterRenderer(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceGutterRenderer  *GetGtkSourceGutterRenderer(Stack stack, int i)
{
  NspGtkSourceGutterRenderer *M;
  if (( M = nsp_gtksourcegutterrenderer_object(NthObj(i))) == NULLGTKSOURCEGUTTERRENDERER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceGutterRenderer *gtksourcegutterrenderer_copy(NspGtkSourceGutterRenderer *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutterrenderer);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutterrenderer);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceGutterRenderer
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_gutter_renderer_begin(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj,obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_background_area, *nsp_cell_area, *nsp_start = NULL, *nsp_end = NULL;
  GdkRectangle background_area = { 0, 0, 0, 0 }, cell_area = { 0, 0, 0, 0 };
  GtkTextIter *start = NULL, *end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_background_area, &nsp_cell_area, &nsp_start, &nsp_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (!nsp_gdk_rectangle_from_object(nsp_background_area, &background_area))
      return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_cell_area, &cell_area))
      return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_gutter_renderer_begin(GTK_SOURCE_GUTTER_RENDERER(self->obj),cr,&background_area,&cell_area,start,end);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_draw(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj,obj,obj,obj, t_end};
  cairo_t *cr = NULL;
  NspObject *nsp_cr = NULL, *nsp_background_area, *nsp_cell_area, *nsp_start = NULL, *nsp_end = NULL, *nsp_state = NULL;
  GdkRectangle background_area = { 0, 0, 0, 0 }, cell_area = { 0, 0, 0, 0 };
  GtkTextIter *start = NULL, *end = NULL;
  GtkSourceGutterRendererState state;
  if ( GetArgs(stack,rhs,opt,T,&nsp_cr, &nsp_background_area, &nsp_cell_area, &nsp_start, &nsp_end, &nsp_state) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_cr, CAIRO_GOBJECT_TYPE_CONTEXT))
      cr = nspg_boxed_get(nsp_cr, cairo_t);
  else {
      Scierror( "Error: cr should be a cairo_t\n");
      return RET_BUG;
  }
  if (!nsp_gdk_rectangle_from_object(nsp_background_area, &background_area))
      return RET_BUG;
  if (!nsp_gdk_rectangle_from_object(nsp_cell_area, &cell_area))
      return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_flags_get_value(GTK_SOURCE_TYPE_GUTTER_RENDERER_STATE, nsp_state, &state)==FAIL)
      return RET_BUG;
    gtk_source_gutter_renderer_draw(GTK_SOURCE_GUTTER_RENDERER(self->obj),cr,&background_area,&cell_area,start,end,state);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_end(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_gutter_renderer_end(GTK_SOURCE_GUTTER_RENDERER(self->obj));
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_get_size(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_get_size(GTK_SOURCE_GUTTER_RENDERER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_set_size(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int size;
  if ( GetArgs(stack,rhs,opt,T,&size) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_set_size(GTK_SOURCE_GUTTER_RENDERER(self->obj),size);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_set_visible(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int visible;
  if ( GetArgs(stack,rhs,opt,T,&visible) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_set_visible(GTK_SOURCE_GUTTER_RENDERER(self->obj),visible);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_get_visible(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_get_visible(GTK_SOURCE_GUTTER_RENDERER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_get_padding(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int xpad, ypad;
  if ( GetArgs(stack,rhs,opt,T,&xpad, &ypad) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_get_padding(GTK_SOURCE_GUTTER_RENDERER(self->obj),&xpad,&ypad);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_set_padding(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int, t_end};
  int xpad, ypad;
  if ( GetArgs(stack,rhs,opt,T,&xpad, &ypad) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_set_padding(GTK_SOURCE_GUTTER_RENDERER(self->obj),xpad,ypad);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_set_alignment(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_double,s_double, t_end};
  double xalign, yalign;
  if ( GetArgs(stack,rhs,opt,T,&xalign, &yalign) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_set_alignment(GTK_SOURCE_GUTTER_RENDERER(self->obj),xalign,yalign);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_set_alignment_mode(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceGutterRendererAlignmentMode mode;
  NspObject *nsp_mode = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GTK_SOURCE_TYPE_GUTTER_RENDERER_ALIGNMENT_MODE, nsp_mode, &mode)== FAIL)
      return RET_BUG;
    gtk_source_gutter_renderer_set_alignment_mode(GTK_SOURCE_GUTTER_RENDERER(self->obj),mode);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_get_view(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkTextView *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_get_view(GTK_SOURCE_GUTTER_RENDERER(self->obj));
  nsp_type_gtktextview = new_type_gtktextview(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtktextview))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_get_alignment_mode(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_get_alignment_mode(GTK_SOURCE_GUTTER_RENDERER(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_get_background(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkRGBA *color = NULL;
  NspObject *nsp_color = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_RGBA))
      color = nspg_boxed_get(nsp_color, GdkRGBA);
  else {
      Scierror( "Error: color should be a GdkRGBA\n");
      return RET_BUG;
  }
    ret =gtk_source_gutter_renderer_get_background(GTK_SOURCE_GUTTER_RENDERER(self->obj),color);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_set_background(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkRGBA *color = NULL;
  NspObject *nsp_color = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_color) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_color, GDK_TYPE_RGBA))
      color = nspg_boxed_get(nsp_color, GdkRGBA);
  else {
      Scierror( "Error: color should be a GdkRGBA\n");
      return RET_BUG;
  }
    gtk_source_gutter_renderer_set_background(GTK_SOURCE_GUTTER_RENDERER(self->obj),color);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_activate(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL, *nsp_area, *nsp_event = NULL;
  GdkRectangle area = { 0, 0, 0, 0 };
  GdkEvent *event = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &nsp_area, &nsp_event) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (!nsp_gdk_rectangle_from_object(nsp_area, &area))
      return RET_BUG;
  if (nspg_boxed_check(nsp_event, GDK_TYPE_EVENT))
      event = nspg_boxed_get(nsp_event, GdkEvent);
  else {
      Scierror( "Error: event should be a GdkEvent\n");
      return RET_BUG;
  }
    gtk_source_gutter_renderer_activate(GTK_SOURCE_GUTTER_RENDERER(self->obj),iter,&area,event);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_query_activatable(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL, *nsp_area, *nsp_event = NULL;
  GdkRectangle area = { 0, 0, 0, 0 };
  GdkEvent *event = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &nsp_area, &nsp_event) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (!nsp_gdk_rectangle_from_object(nsp_area, &area))
      return RET_BUG;
  if (nspg_boxed_check(nsp_event, GDK_TYPE_EVENT))
      event = nspg_boxed_get(nsp_event, GdkEvent);
  else {
      Scierror( "Error: event should be a GdkEvent\n");
      return RET_BUG;
  }
    ret =gtk_source_gutter_renderer_query_activatable(GTK_SOURCE_GUTTER_RENDERER(self->obj),iter,&area,event);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_queue_draw(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_gutter_renderer_queue_draw(GTK_SOURCE_GUTTER_RENDERER(self->obj));
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_query_tooltip(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,s_int,s_int,obj_check, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL, *nsp_area;
  GdkRectangle area = { 0, 0, 0, 0 };
  int x, y, ret;
  NspGObject *tooltip;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &nsp_area, &x, &y, &nsp_type_gtktooltip, &tooltip) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (!nsp_gdk_rectangle_from_object(nsp_area, &area))
      return RET_BUG;
    ret =gtk_source_gutter_renderer_query_tooltip(GTK_SOURCE_GUTTER_RENDERER(self->obj),iter,&area,x,y,GTK_TOOLTIP(tooltip->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_query_data(NspGtkSourceGutterRenderer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL, *nsp_state = NULL;
  GtkSourceGutterRendererState state;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end, &nsp_state) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_flags_get_value(GTK_SOURCE_TYPE_GUTTER_RENDERER_STATE, nsp_state, &state)==FAIL)
      return RET_BUG;
    gtk_source_gutter_renderer_query_data(GTK_SOURCE_GUTTER_RENDERER(self->obj),start,end,state);
  return 0;
}

static NspMethods gtksourcegutterrenderer_methods[] = {
  {"begin",(nsp_method *) _wrap_gtk_source_gutter_renderer_begin},
  {"draw",(nsp_method *) _wrap_gtk_source_gutter_renderer_draw},
  {"end",(nsp_method *) _wrap_gtk_source_gutter_renderer_end},
  {"get_size",(nsp_method *) _wrap_gtk_source_gutter_renderer_get_size},
  {"set_size",(nsp_method *) _wrap_gtk_source_gutter_renderer_set_size},
  {"set_visible",(nsp_method *) _wrap_gtk_source_gutter_renderer_set_visible},
  {"get_visible",(nsp_method *) _wrap_gtk_source_gutter_renderer_get_visible},
  {"get_padding",(nsp_method *) _wrap_gtk_source_gutter_renderer_get_padding},
  {"set_padding",(nsp_method *) _wrap_gtk_source_gutter_renderer_set_padding},
  {"set_alignment",(nsp_method *) _wrap_gtk_source_gutter_renderer_set_alignment},
  {"set_alignment_mode",(nsp_method *) _wrap_gtk_source_gutter_renderer_set_alignment_mode},
  {"get_view",(nsp_method *) _wrap_gtk_source_gutter_renderer_get_view},
  {"get_alignment_mode",(nsp_method *) _wrap_gtk_source_gutter_renderer_get_alignment_mode},
  {"get_background",(nsp_method *) _wrap_gtk_source_gutter_renderer_get_background},
  {"set_background",(nsp_method *) _wrap_gtk_source_gutter_renderer_set_background},
  {"activate",(nsp_method *) _wrap_gtk_source_gutter_renderer_activate},
  {"query_activatable",(nsp_method *) _wrap_gtk_source_gutter_renderer_query_activatable},
  {"queue_draw",(nsp_method *) _wrap_gtk_source_gutter_renderer_queue_draw},
  {"query_tooltip",(nsp_method *) _wrap_gtk_source_gutter_renderer_query_tooltip},
  {"query_data",(nsp_method *) _wrap_gtk_source_gutter_renderer_query_data},
  { NULL, NULL}
};

static NspMethods *gtksourcegutterrenderer_get_methods(void) { return gtksourcegutterrenderer_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcegutterrenderer_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceGutterRendererPixbuf ----------- */


#define  NspGtkSourceGutterRendererPixbuf_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcegutterrendererpixbuf.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceGutterRendererPixbuf inherits from GtkSourceGutterRenderer 
 */

int nsp_type_gtksourcegutterrendererpixbuf_id=0;
NspTypeGtkSourceGutterRendererPixbuf *nsp_type_gtksourcegutterrendererpixbuf=NULL;

/*
 * Type object for NspGtkSourceGutterRendererPixbuf 
 * all the instance of NspTypeGtkSourceGutterRendererPixbuf share the same id. 
 * nsp_type_gtksourcegutterrendererpixbuf: is an instance of NspTypeGtkSourceGutterRendererPixbuf 
 *    used for objects of NspGtkSourceGutterRendererPixbuf type (i.e built with new_gtksourcegutterrendererpixbuf) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceGutterRendererPixbuf *new_type_gtksourcegutterrendererpixbuf(type_mode mode)
{
  NspTypeGtkSourceGutterRendererPixbuf *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcegutterrendererpixbuf != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcegutterrendererpixbuf;
    }
  if (( type =  malloc(sizeof(NspTypeGtkSourceGutterRenderer))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtksourcegutterrenderer(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcegutterrendererpixbuf_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcegutterrendererpixbuf_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcegutterrendererpixbuf;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcegutterrendererpixbuf */ 

  top->s_type =  (s_type_func *) nsp_gtksourcegutterrendererpixbuf_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcegutterrendererpixbuf_type_short_string;
  /* top->create = (create_func*) int_gtksourcegutterrendererpixbuf_create;*/

  /* specific methods for gtksourcegutterrendererpixbuf */

  type->init = (init_func *) init_gtksourcegutterrendererpixbuf;

  /* 
   * NspGtkSourceGutterRendererPixbuf interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcegutterrendererpixbuf_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceGutterRendererPixbuf called nsp_type_gtksourcegutterrendererpixbuf
       */
      type->id =  nsp_type_gtksourcegutterrendererpixbuf_id = nsp_new_type_id();
      nsp_type_gtksourcegutterrendererpixbuf = type;
      if ( nsp_register_type(nsp_type_gtksourcegutterrendererpixbuf) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcegutterrendererpixbuf, GTK_SOURCE_TYPE_GUTTER_RENDERER_PIXBUF);
      return ( mode == T_BASE ) ? type : new_type_gtksourcegutterrendererpixbuf(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcegutterrendererpixbuf_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceGutterRendererPixbuf instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcegutterrendererpixbuf(NspGtkSourceGutterRendererPixbuf *Obj,NspTypeGtkSourceGutterRendererPixbuf *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceGutterRendererPixbuf 
 */

NspGtkSourceGutterRendererPixbuf *new_gtksourcegutterrendererpixbuf() 
{
  NspGtkSourceGutterRendererPixbuf *loc;
  /* type must exists */
  nsp_type_gtksourcegutterrendererpixbuf = new_type_gtksourcegutterrendererpixbuf(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceGutterRendererPixbuf)))== NULLGTKSOURCEGUTTERRENDERERPIXBUF) return loc;
  /* initialize object */
  if ( init_gtksourcegutterrendererpixbuf(loc,nsp_type_gtksourcegutterrendererpixbuf) == FAIL) return NULLGTKSOURCEGUTTERRENDERERPIXBUF;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceGutterRendererPixbuf 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcegutterrendererpixbuf_type_name[]="GtkSourceGutterRendererPixbuf";
static char gtksourcegutterrendererpixbuf_short_type_name[]="GtkSourceGutterRendererPixbuf";

static char *nsp_gtksourcegutterrendererpixbuf_type_as_string(void)
{
  return(gtksourcegutterrendererpixbuf_type_name);
}

static char *nsp_gtksourcegutterrendererpixbuf_type_short_string(NspObject *v)
{
  return(gtksourcegutterrendererpixbuf_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceGutterRendererPixbuf objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceGutterRendererPixbuf   *nsp_gtksourcegutterrendererpixbuf_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcegutterrendererpixbuf_id)  == TRUE  ) return ((NspGtkSourceGutterRendererPixbuf *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcegutterrendererpixbuf));
  return NULL;
}

int IsGtkSourceGutterRendererPixbufObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcegutterrendererpixbuf_id);
}

int IsGtkSourceGutterRendererPixbuf(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcegutterrendererpixbuf_id);
}

NspGtkSourceGutterRendererPixbuf  *GetGtkSourceGutterRendererPixbufCopy(Stack stack, int i)
{
  if (  GetGtkSourceGutterRendererPixbuf(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceGutterRendererPixbuf  *GetGtkSourceGutterRendererPixbuf(Stack stack, int i)
{
  NspGtkSourceGutterRendererPixbuf *M;
  if (( M = nsp_gtksourcegutterrendererpixbuf_object(NthObj(i))) == NULLGTKSOURCEGUTTERRENDERERPIXBUF)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceGutterRendererPixbuf *gtksourcegutterrendererpixbuf_copy(NspGtkSourceGutterRendererPixbuf *self)
{
  /* return gtksourcegutterrenderer_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutterrendererpixbuf);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutterrendererpixbuf);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceGutterRendererPixbuf
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_gutter_renderer_pixbuf_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_gutter_renderer_pixbuf_new())== NULL) return RET_BUG;

  nsp_type_gtksourcegutterrendererpixbuf = new_type_gtksourcegutterrendererpixbuf(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcegutterrendererpixbuf);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_pixbuf_set_pixbuf(NspGtkSourceGutterRendererPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *pixbuf;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdkpixbuf, &pixbuf) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_pixbuf_set_pixbuf(GTK_SOURCE_GUTTER_RENDERER_PIXBUF(self->obj),GDK_PIXBUF(pixbuf->obj));
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_pixbuf_get_pixbuf(NspGtkSourceGutterRendererPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  GdkPixbuf *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_pixbuf_get_pixbuf(GTK_SOURCE_GUTTER_RENDERER_PIXBUF(self->obj));
  nsp_type_gdkpixbuf = new_type_gdkpixbuf(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdkpixbuf))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_pixbuf_set_gicon(NspGtkSourceGutterRendererPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *icon;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gicon, &icon) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_pixbuf_set_gicon(GTK_SOURCE_GUTTER_RENDERER_PIXBUF(self->obj),G_ICON(icon->obj));
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_pixbuf_get_gicon(NspGtkSourceGutterRendererPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_pixbuf_get_gicon(GTK_SOURCE_GUTTER_RENDERER_PIXBUF(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_pixbuf_set_icon_name(NspGtkSourceGutterRendererPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *icon_name;
  if ( GetArgs(stack,rhs,opt,T,&icon_name) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_pixbuf_set_icon_name(GTK_SOURCE_GUTTER_RENDERER_PIXBUF(self->obj),icon_name);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_pixbuf_get_icon_name(NspGtkSourceGutterRendererPixbuf *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_gutter_renderer_pixbuf_get_icon_name(GTK_SOURCE_GUTTER_RENDERER_PIXBUF(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcegutterrendererpixbuf_methods[] = {
  {"set_pixbuf",(nsp_method *) _wrap_gtk_source_gutter_renderer_pixbuf_set_pixbuf},
  {"get_pixbuf",(nsp_method *) _wrap_gtk_source_gutter_renderer_pixbuf_get_pixbuf},
  {"set_gicon",(nsp_method *) _wrap_gtk_source_gutter_renderer_pixbuf_set_gicon},
  {"get_gicon",(nsp_method *) _wrap_gtk_source_gutter_renderer_pixbuf_get_gicon},
  {"set_icon_name",(nsp_method *) _wrap_gtk_source_gutter_renderer_pixbuf_set_icon_name},
  {"get_icon_name",(nsp_method *) _wrap_gtk_source_gutter_renderer_pixbuf_get_icon_name},
  { NULL, NULL}
};

static NspMethods *gtksourcegutterrendererpixbuf_get_methods(void) { return gtksourcegutterrendererpixbuf_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcegutterrendererpixbuf_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceGutterRendererText ----------- */


#define  NspGtkSourceGutterRendererText_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcegutterrenderertext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceGutterRendererText inherits from GtkSourceGutterRenderer 
 */

int nsp_type_gtksourcegutterrenderertext_id=0;
NspTypeGtkSourceGutterRendererText *nsp_type_gtksourcegutterrenderertext=NULL;

/*
 * Type object for NspGtkSourceGutterRendererText 
 * all the instance of NspTypeGtkSourceGutterRendererText share the same id. 
 * nsp_type_gtksourcegutterrenderertext: is an instance of NspTypeGtkSourceGutterRendererText 
 *    used for objects of NspGtkSourceGutterRendererText type (i.e built with new_gtksourcegutterrenderertext) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceGutterRendererText *new_type_gtksourcegutterrenderertext(type_mode mode)
{
  NspTypeGtkSourceGutterRendererText *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcegutterrenderertext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcegutterrenderertext;
    }
  if (( type =  malloc(sizeof(NspTypeGtkSourceGutterRenderer))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtksourcegutterrenderer(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcegutterrenderertext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcegutterrenderertext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcegutterrenderertext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcegutterrenderertext */ 

  top->s_type =  (s_type_func *) nsp_gtksourcegutterrenderertext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcegutterrenderertext_type_short_string;
  /* top->create = (create_func*) int_gtksourcegutterrenderertext_create;*/

  /* specific methods for gtksourcegutterrenderertext */

  type->init = (init_func *) init_gtksourcegutterrenderertext;

  /* 
   * NspGtkSourceGutterRendererText interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcegutterrenderertext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceGutterRendererText called nsp_type_gtksourcegutterrenderertext
       */
      type->id =  nsp_type_gtksourcegutterrenderertext_id = nsp_new_type_id();
      nsp_type_gtksourcegutterrenderertext = type;
      if ( nsp_register_type(nsp_type_gtksourcegutterrenderertext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcegutterrenderertext, GTK_SOURCE_TYPE_GUTTER_RENDERER_TEXT);
      return ( mode == T_BASE ) ? type : new_type_gtksourcegutterrenderertext(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcegutterrenderertext_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceGutterRendererText instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcegutterrenderertext(NspGtkSourceGutterRendererText *Obj,NspTypeGtkSourceGutterRendererText *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceGutterRendererText 
 */

NspGtkSourceGutterRendererText *new_gtksourcegutterrenderertext() 
{
  NspGtkSourceGutterRendererText *loc;
  /* type must exists */
  nsp_type_gtksourcegutterrenderertext = new_type_gtksourcegutterrenderertext(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceGutterRendererText)))== NULLGTKSOURCEGUTTERRENDERERTEXT) return loc;
  /* initialize object */
  if ( init_gtksourcegutterrenderertext(loc,nsp_type_gtksourcegutterrenderertext) == FAIL) return NULLGTKSOURCEGUTTERRENDERERTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceGutterRendererText 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcegutterrenderertext_type_name[]="GtkSourceGutterRendererText";
static char gtksourcegutterrenderertext_short_type_name[]="GtkSourceGutterRendererText";

static char *nsp_gtksourcegutterrenderertext_type_as_string(void)
{
  return(gtksourcegutterrenderertext_type_name);
}

static char *nsp_gtksourcegutterrenderertext_type_short_string(NspObject *v)
{
  return(gtksourcegutterrenderertext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceGutterRendererText objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceGutterRendererText   *nsp_gtksourcegutterrenderertext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcegutterrenderertext_id)  == TRUE  ) return ((NspGtkSourceGutterRendererText *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcegutterrenderertext));
  return NULL;
}

int IsGtkSourceGutterRendererTextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcegutterrenderertext_id);
}

int IsGtkSourceGutterRendererText(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcegutterrenderertext_id);
}

NspGtkSourceGutterRendererText  *GetGtkSourceGutterRendererTextCopy(Stack stack, int i)
{
  if (  GetGtkSourceGutterRendererText(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceGutterRendererText  *GetGtkSourceGutterRendererText(Stack stack, int i)
{
  NspGtkSourceGutterRendererText *M;
  if (( M = nsp_gtksourcegutterrenderertext_object(NthObj(i))) == NULLGTKSOURCEGUTTERRENDERERTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceGutterRendererText *gtksourcegutterrenderertext_copy(NspGtkSourceGutterRendererText *self)
{
  /* return gtksourcegutterrenderer_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutterrenderertext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcegutterrenderertext);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceGutterRendererText
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_gutter_renderer_text_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_gutter_renderer_text_new())== NULL) return RET_BUG;

  nsp_type_gtksourcegutterrenderertext = new_type_gtksourcegutterrenderertext(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcegutterrenderertext);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_gutter_renderer_text_set_markup(NspGtkSourceGutterRendererText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *markup;
  int length;
  if ( GetArgs(stack,rhs,opt,T,&markup, &length) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_text_set_markup(GTK_SOURCE_GUTTER_RENDERER_TEXT(self->obj),markup,length);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_text_set_text(NspGtkSourceGutterRendererText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *text;
  int length;
  if ( GetArgs(stack,rhs,opt,T,&text, &length) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_text_set_text(GTK_SOURCE_GUTTER_RENDERER_TEXT(self->obj),text,length);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_text_measure(NspGtkSourceGutterRendererText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *text;
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&text, &width, &height) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_text_measure(GTK_SOURCE_GUTTER_RENDERER_TEXT(self->obj),text,&width,&height);
  return 0;
}

static int _wrap_gtk_source_gutter_renderer_text_measure_markup(NspGtkSourceGutterRendererText *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,s_int, t_end};
  char *markup;
  int width, height;
  if ( GetArgs(stack,rhs,opt,T,&markup, &width, &height) == FAIL) return RET_BUG;
    gtk_source_gutter_renderer_text_measure_markup(GTK_SOURCE_GUTTER_RENDERER_TEXT(self->obj),markup,&width,&height);
  return 0;
}

static NspMethods gtksourcegutterrenderertext_methods[] = {
  {"set_markup",(nsp_method *) _wrap_gtk_source_gutter_renderer_text_set_markup},
  {"set_text",(nsp_method *) _wrap_gtk_source_gutter_renderer_text_set_text},
  {"measure",(nsp_method *) _wrap_gtk_source_gutter_renderer_text_measure},
  {"measure_markup",(nsp_method *) _wrap_gtk_source_gutter_renderer_text_measure_markup},
  { NULL, NULL}
};

static NspMethods *gtksourcegutterrenderertext_get_methods(void) { return gtksourcegutterrenderertext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcegutterrenderertext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceLanguage ----------- */


#define  NspGtkSourceLanguage_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcelanguage.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceLanguage inherits from GObject 
 */

int nsp_type_gtksourcelanguage_id=0;
NspTypeGtkSourceLanguage *nsp_type_gtksourcelanguage=NULL;

/*
 * Type object for NspGtkSourceLanguage 
 * all the instance of NspTypeGtkSourceLanguage share the same id. 
 * nsp_type_gtksourcelanguage: is an instance of NspTypeGtkSourceLanguage 
 *    used for objects of NspGtkSourceLanguage type (i.e built with new_gtksourcelanguage) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceLanguage *new_type_gtksourcelanguage(type_mode mode)
{
  NspTypeGtkSourceLanguage *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcelanguage != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcelanguage;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcelanguage_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcelanguage_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcelanguage;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcelanguage */ 

  top->s_type =  (s_type_func *) nsp_gtksourcelanguage_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcelanguage_type_short_string;
  /* top->create = (create_func*) int_gtksourcelanguage_create;*/

  /* specific methods for gtksourcelanguage */

  type->init = (init_func *) init_gtksourcelanguage;

  /* 
   * NspGtkSourceLanguage interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcelanguage_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceLanguage called nsp_type_gtksourcelanguage
       */
      type->id =  nsp_type_gtksourcelanguage_id = nsp_new_type_id();
      nsp_type_gtksourcelanguage = type;
      if ( nsp_register_type(nsp_type_gtksourcelanguage) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcelanguage, GTK_SOURCE_TYPE_LANGUAGE);
      return ( mode == T_BASE ) ? type : new_type_gtksourcelanguage(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcelanguage_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceLanguage instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcelanguage(NspGtkSourceLanguage *Obj,NspTypeGtkSourceLanguage *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceLanguage 
 */

NspGtkSourceLanguage *new_gtksourcelanguage() 
{
  NspGtkSourceLanguage *loc;
  /* type must exists */
  nsp_type_gtksourcelanguage = new_type_gtksourcelanguage(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceLanguage)))== NULLGTKSOURCELANGUAGE) return loc;
  /* initialize object */
  if ( init_gtksourcelanguage(loc,nsp_type_gtksourcelanguage) == FAIL) return NULLGTKSOURCELANGUAGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceLanguage 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcelanguage_type_name[]="GtkSourceLanguage";
static char gtksourcelanguage_short_type_name[]="GtkSourceLanguage";

static char *nsp_gtksourcelanguage_type_as_string(void)
{
  return(gtksourcelanguage_type_name);
}

static char *nsp_gtksourcelanguage_type_short_string(NspObject *v)
{
  return(gtksourcelanguage_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceLanguage objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceLanguage   *nsp_gtksourcelanguage_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcelanguage_id)  == TRUE  ) return ((NspGtkSourceLanguage *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcelanguage));
  return NULL;
}

int IsGtkSourceLanguageObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcelanguage_id);
}

int IsGtkSourceLanguage(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcelanguage_id);
}

NspGtkSourceLanguage  *GetGtkSourceLanguageCopy(Stack stack, int i)
{
  if (  GetGtkSourceLanguage(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceLanguage  *GetGtkSourceLanguage(Stack stack, int i)
{
  NspGtkSourceLanguage *M;
  if (( M = nsp_gtksourcelanguage_object(NthObj(i))) == NULLGTKSOURCELANGUAGE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceLanguage *gtksourcelanguage_copy(NspGtkSourceLanguage *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcelanguage);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcelanguage);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceLanguage
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_language_get_id(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_id(GTK_SOURCE_LANGUAGE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_language_get_name(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_name(GTK_SOURCE_LANGUAGE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_language_get_section(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_section(GTK_SOURCE_LANGUAGE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_language_get_hidden(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_hidden(GTK_SOURCE_LANGUAGE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_language_get_metadata(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =gtk_source_language_get_metadata(GTK_SOURCE_LANGUAGE(self->obj),name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_language_get_mime_types(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_mime_types(GTK_SOURCE_LANGUAGE(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_language_get_globs(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_globs(GTK_SOURCE_LANGUAGE(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_language_get_style_ids(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_language_get_style_ids(GTK_SOURCE_LANGUAGE(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_language_get_style_name(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *style_id;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&style_id) == FAIL) return RET_BUG;
    ret =gtk_source_language_get_style_name(GTK_SOURCE_LANGUAGE(self->obj),style_id);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_language_get_style_fallback(NspGtkSourceLanguage *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *style_id;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&style_id) == FAIL) return RET_BUG;
    ret =gtk_source_language_get_style_fallback(GTK_SOURCE_LANGUAGE(self->obj),style_id);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcelanguage_methods[] = {
  {"get_id",(nsp_method *) _wrap_gtk_source_language_get_id},
  {"get_name",(nsp_method *) _wrap_gtk_source_language_get_name},
  {"get_section",(nsp_method *) _wrap_gtk_source_language_get_section},
  {"get_hidden",(nsp_method *) _wrap_gtk_source_language_get_hidden},
  {"get_metadata",(nsp_method *) _wrap_gtk_source_language_get_metadata},
  {"get_mime_types",(nsp_method *) _wrap_gtk_source_language_get_mime_types},
  {"get_globs",(nsp_method *) _wrap_gtk_source_language_get_globs},
  {"get_style_ids",(nsp_method *) _wrap_gtk_source_language_get_style_ids},
  {"get_style_name",(nsp_method *) _wrap_gtk_source_language_get_style_name},
  {"get_style_fallback",(nsp_method *) _wrap_gtk_source_language_get_style_fallback},
  { NULL, NULL}
};

static NspMethods *gtksourcelanguage_get_methods(void) { return gtksourcelanguage_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcelanguage_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceLanguageManager ----------- */


#define  NspGtkSourceLanguageManager_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcelanguagemanager.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceLanguageManager inherits from GObject 
 */

int nsp_type_gtksourcelanguagemanager_id=0;
NspTypeGtkSourceLanguageManager *nsp_type_gtksourcelanguagemanager=NULL;

/*
 * Type object for NspGtkSourceLanguageManager 
 * all the instance of NspTypeGtkSourceLanguageManager share the same id. 
 * nsp_type_gtksourcelanguagemanager: is an instance of NspTypeGtkSourceLanguageManager 
 *    used for objects of NspGtkSourceLanguageManager type (i.e built with new_gtksourcelanguagemanager) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceLanguageManager *new_type_gtksourcelanguagemanager(type_mode mode)
{
  NspTypeGtkSourceLanguageManager *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcelanguagemanager != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcelanguagemanager;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcelanguagemanager_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcelanguagemanager_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcelanguagemanager;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcelanguagemanager */ 

  top->s_type =  (s_type_func *) nsp_gtksourcelanguagemanager_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcelanguagemanager_type_short_string;
  /* top->create = (create_func*) int_gtksourcelanguagemanager_create;*/

  /* specific methods for gtksourcelanguagemanager */

  type->init = (init_func *) init_gtksourcelanguagemanager;

  /* 
   * NspGtkSourceLanguageManager interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcelanguagemanager_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceLanguageManager called nsp_type_gtksourcelanguagemanager
       */
      type->id =  nsp_type_gtksourcelanguagemanager_id = nsp_new_type_id();
      nsp_type_gtksourcelanguagemanager = type;
      if ( nsp_register_type(nsp_type_gtksourcelanguagemanager) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcelanguagemanager, GTK_SOURCE_TYPE_LANGUAGE_MANAGER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcelanguagemanager(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcelanguagemanager_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceLanguageManager instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcelanguagemanager(NspGtkSourceLanguageManager *Obj,NspTypeGtkSourceLanguageManager *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceLanguageManager 
 */

NspGtkSourceLanguageManager *new_gtksourcelanguagemanager() 
{
  NspGtkSourceLanguageManager *loc;
  /* type must exists */
  nsp_type_gtksourcelanguagemanager = new_type_gtksourcelanguagemanager(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceLanguageManager)))== NULLGTKSOURCELANGUAGEMANAGER) return loc;
  /* initialize object */
  if ( init_gtksourcelanguagemanager(loc,nsp_type_gtksourcelanguagemanager) == FAIL) return NULLGTKSOURCELANGUAGEMANAGER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceLanguageManager 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcelanguagemanager_type_name[]="GtkSourceLanguageManager";
static char gtksourcelanguagemanager_short_type_name[]="GtkSourceLanguageManager";

static char *nsp_gtksourcelanguagemanager_type_as_string(void)
{
  return(gtksourcelanguagemanager_type_name);
}

static char *nsp_gtksourcelanguagemanager_type_short_string(NspObject *v)
{
  return(gtksourcelanguagemanager_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceLanguageManager objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceLanguageManager   *nsp_gtksourcelanguagemanager_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcelanguagemanager_id)  == TRUE  ) return ((NspGtkSourceLanguageManager *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcelanguagemanager));
  return NULL;
}

int IsGtkSourceLanguageManagerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcelanguagemanager_id);
}

int IsGtkSourceLanguageManager(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcelanguagemanager_id);
}

NspGtkSourceLanguageManager  *GetGtkSourceLanguageManagerCopy(Stack stack, int i)
{
  if (  GetGtkSourceLanguageManager(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceLanguageManager  *GetGtkSourceLanguageManager(Stack stack, int i)
{
  NspGtkSourceLanguageManager *M;
  if (( M = nsp_gtksourcelanguagemanager_object(NthObj(i))) == NULLGTKSOURCELANGUAGEMANAGER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceLanguageManager *gtksourcelanguagemanager_copy(NspGtkSourceLanguageManager *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcelanguagemanager);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcelanguagemanager);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceLanguageManager
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_language_manager_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_language_manager_new())== NULL) return RET_BUG;

  nsp_type_gtksourcelanguagemanager = new_type_gtksourcelanguagemanager(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcelanguagemanager);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_language_manager_set_search_path(NspGtkSourceLanguageManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  gchar **dirs = NULL;
  NspObject *nsp_dirs = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_dirs) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_dirs))
    { dirs =  ((NspSMatrix *) nsp_dirs)->S;}
  else
    {
      Scierror("Error: dirs should be of type SMat\n");
      return RET_BUG;
    }
    gtk_source_language_manager_set_search_path(GTK_SOURCE_LANGUAGE_MANAGER(self->obj),dirs);
  return 0;
}

static int _wrap_gtk_source_language_manager_get_language(NspGtkSourceLanguageManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *id;
  GtkSourceLanguage *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&id) == FAIL) return RET_BUG;
    ret =gtk_source_language_manager_get_language(GTK_SOURCE_LANGUAGE_MANAGER(self->obj),id);
  nsp_type_gtksourcelanguage = new_type_gtksourcelanguage(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcelanguage))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_language_manager_guess_language(NspGtkSourceLanguageManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *filename, *content_type;
  GtkSourceLanguage *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&filename, &content_type) == FAIL) return RET_BUG;
    ret =gtk_source_language_manager_guess_language(GTK_SOURCE_LANGUAGE_MANAGER(self->obj),filename,content_type);
  nsp_type_gtksourcelanguage = new_type_gtksourcelanguage(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcelanguage))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gtksourcelanguagemanager_methods[] = {
  {"set_search_path",(nsp_method *) _wrap_gtk_source_language_manager_set_search_path},
  {"get_language",(nsp_method *) _wrap_gtk_source_language_manager_get_language},
  {"guess_language",(nsp_method *) _wrap_gtk_source_language_manager_guess_language},
  { NULL, NULL}
};

static NspMethods *gtksourcelanguagemanager_get_methods(void) { return gtksourcelanguagemanager_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcelanguagemanager_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceMark ----------- */


#define  NspGtkSourceMark_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcemark.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceMark inherits from GtkTextMark 
 */

int nsp_type_gtksourcemark_id=0;
NspTypeGtkSourceMark *nsp_type_gtksourcemark=NULL;

/*
 * Type object for NspGtkSourceMark 
 * all the instance of NspTypeGtkSourceMark share the same id. 
 * nsp_type_gtksourcemark: is an instance of NspTypeGtkSourceMark 
 *    used for objects of NspGtkSourceMark type (i.e built with new_gtksourcemark) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceMark *new_type_gtksourcemark(type_mode mode)
{
  NspTypeGtkSourceMark *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcemark != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcemark;
    }
  if (( type =  malloc(sizeof(NspTypeGtkTextMark))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtktextmark(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcemark_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcemark_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcemark;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcemark */ 

  top->s_type =  (s_type_func *) nsp_gtksourcemark_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcemark_type_short_string;
  /* top->create = (create_func*) int_gtksourcemark_create;*/

  /* specific methods for gtksourcemark */

  type->init = (init_func *) init_gtksourcemark;

  /* 
   * NspGtkSourceMark interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcemark_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceMark called nsp_type_gtksourcemark
       */
      type->id =  nsp_type_gtksourcemark_id = nsp_new_type_id();
      nsp_type_gtksourcemark = type;
      if ( nsp_register_type(nsp_type_gtksourcemark) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcemark, GTK_SOURCE_TYPE_MARK);
      return ( mode == T_BASE ) ? type : new_type_gtksourcemark(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcemark_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceMark instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcemark(NspGtkSourceMark *Obj,NspTypeGtkSourceMark *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceMark 
 */

NspGtkSourceMark *new_gtksourcemark() 
{
  NspGtkSourceMark *loc;
  /* type must exists */
  nsp_type_gtksourcemark = new_type_gtksourcemark(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceMark)))== NULLGTKSOURCEMARK) return loc;
  /* initialize object */
  if ( init_gtksourcemark(loc,nsp_type_gtksourcemark) == FAIL) return NULLGTKSOURCEMARK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceMark 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcemark_type_name[]="GtkSourceMark";
static char gtksourcemark_short_type_name[]="GtkSourceMark";

static char *nsp_gtksourcemark_type_as_string(void)
{
  return(gtksourcemark_type_name);
}

static char *nsp_gtksourcemark_type_short_string(NspObject *v)
{
  return(gtksourcemark_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceMark objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceMark   *nsp_gtksourcemark_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcemark_id)  == TRUE  ) return ((NspGtkSourceMark *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcemark));
  return NULL;
}

int IsGtkSourceMarkObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcemark_id);
}

int IsGtkSourceMark(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcemark_id);
}

NspGtkSourceMark  *GetGtkSourceMarkCopy(Stack stack, int i)
{
  if (  GetGtkSourceMark(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceMark  *GetGtkSourceMark(Stack stack, int i)
{
  NspGtkSourceMark *M;
  if (( M = nsp_gtksourcemark_object(NthObj(i))) == NULLGTKSOURCEMARK)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceMark *gtksourcemark_copy(NspGtkSourceMark *self)
{
  /* return gtktextmark_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcemark);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcemark);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceMark
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_mark_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string, t_end};
  char *name, *category;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name, &category) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_mark_new(name,category))== NULL) return RET_BUG;

  nsp_type_gtksourcemark = new_type_gtksourcemark(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemark);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_mark_get_category(NspGtkSourceMark *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_mark_get_category(GTK_SOURCE_MARK(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_mark_next(NspGtkSourceMark *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *category;
  GtkSourceMark *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&category) == FAIL) return RET_BUG;
    ret =gtk_source_mark_next(GTK_SOURCE_MARK(self->obj),category);
  nsp_type_gtksourcemark = new_type_gtksourcemark(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemark))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_mark_prev(NspGtkSourceMark *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *category;
  GtkSourceMark *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&category) == FAIL) return RET_BUG;
    ret =gtk_source_mark_prev(GTK_SOURCE_MARK(self->obj),category);
  nsp_type_gtksourcemark = new_type_gtksourcemark(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemark))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gtksourcemark_methods[] = {
  {"get_category",(nsp_method *) _wrap_gtk_source_mark_get_category},
  {"next",(nsp_method *) _wrap_gtk_source_mark_next},
  {"prev",(nsp_method *) _wrap_gtk_source_mark_prev},
  { NULL, NULL}
};

static NspMethods *gtksourcemark_get_methods(void) { return gtksourcemark_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcemark_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceMarkAttributes ----------- */


#define  NspGtkSourceMarkAttributes_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcemarkattributes.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceMarkAttributes inherits from GObject 
 */

int nsp_type_gtksourcemarkattributes_id=0;
NspTypeGtkSourceMarkAttributes *nsp_type_gtksourcemarkattributes=NULL;

/*
 * Type object for NspGtkSourceMarkAttributes 
 * all the instance of NspTypeGtkSourceMarkAttributes share the same id. 
 * nsp_type_gtksourcemarkattributes: is an instance of NspTypeGtkSourceMarkAttributes 
 *    used for objects of NspGtkSourceMarkAttributes type (i.e built with new_gtksourcemarkattributes) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceMarkAttributes *new_type_gtksourcemarkattributes(type_mode mode)
{
  NspTypeGtkSourceMarkAttributes *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcemarkattributes != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcemarkattributes;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcemarkattributes_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcemarkattributes_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcemarkattributes;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcemarkattributes */ 

  top->s_type =  (s_type_func *) nsp_gtksourcemarkattributes_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcemarkattributes_type_short_string;
  /* top->create = (create_func*) int_gtksourcemarkattributes_create;*/

  /* specific methods for gtksourcemarkattributes */

  type->init = (init_func *) init_gtksourcemarkattributes;

  /* 
   * NspGtkSourceMarkAttributes interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcemarkattributes_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceMarkAttributes called nsp_type_gtksourcemarkattributes
       */
      type->id =  nsp_type_gtksourcemarkattributes_id = nsp_new_type_id();
      nsp_type_gtksourcemarkattributes = type;
      if ( nsp_register_type(nsp_type_gtksourcemarkattributes) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcemarkattributes, GTK_SOURCE_TYPE_MARK_ATTRIBUTES);
      return ( mode == T_BASE ) ? type : new_type_gtksourcemarkattributes(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcemarkattributes_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceMarkAttributes instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcemarkattributes(NspGtkSourceMarkAttributes *Obj,NspTypeGtkSourceMarkAttributes *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceMarkAttributes 
 */

NspGtkSourceMarkAttributes *new_gtksourcemarkattributes() 
{
  NspGtkSourceMarkAttributes *loc;
  /* type must exists */
  nsp_type_gtksourcemarkattributes = new_type_gtksourcemarkattributes(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceMarkAttributes)))== NULLGTKSOURCEMARKATTRIBUTES) return loc;
  /* initialize object */
  if ( init_gtksourcemarkattributes(loc,nsp_type_gtksourcemarkattributes) == FAIL) return NULLGTKSOURCEMARKATTRIBUTES;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceMarkAttributes 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcemarkattributes_type_name[]="GtkSourceMarkAttributes";
static char gtksourcemarkattributes_short_type_name[]="GtkSourceMarkAttributes";

static char *nsp_gtksourcemarkattributes_type_as_string(void)
{
  return(gtksourcemarkattributes_type_name);
}

static char *nsp_gtksourcemarkattributes_type_short_string(NspObject *v)
{
  return(gtksourcemarkattributes_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceMarkAttributes objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceMarkAttributes   *nsp_gtksourcemarkattributes_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcemarkattributes_id)  == TRUE  ) return ((NspGtkSourceMarkAttributes *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcemarkattributes));
  return NULL;
}

int IsGtkSourceMarkAttributesObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcemarkattributes_id);
}

int IsGtkSourceMarkAttributes(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcemarkattributes_id);
}

NspGtkSourceMarkAttributes  *GetGtkSourceMarkAttributesCopy(Stack stack, int i)
{
  if (  GetGtkSourceMarkAttributes(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceMarkAttributes  *GetGtkSourceMarkAttributes(Stack stack, int i)
{
  NspGtkSourceMarkAttributes *M;
  if (( M = nsp_gtksourcemarkattributes_object(NthObj(i))) == NULLGTKSOURCEMARKATTRIBUTES)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceMarkAttributes *gtksourcemarkattributes_copy(NspGtkSourceMarkAttributes *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcemarkattributes);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcemarkattributes);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceMarkAttributes
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_mark_attributes_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_mark_attributes_new())== NULL) return RET_BUG;

  nsp_type_gtksourcemarkattributes = new_type_gtksourcemarkattributes(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemarkattributes);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_mark_attributes_set_background(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkRGBA *background = NULL;
  NspObject *nsp_background = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_background) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_background, GDK_TYPE_RGBA))
      background = nspg_boxed_get(nsp_background, GdkRGBA);
  else {
      Scierror( "Error: background should be a GdkRGBA\n");
      return RET_BUG;
  }
    gtk_source_mark_attributes_set_background(GTK_SOURCE_MARK_ATTRIBUTES(self->obj),background);
  return 0;
}

static int _wrap_gtk_source_mark_attributes_get_background(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GdkRGBA *background = NULL;
  NspObject *nsp_background = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_background) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_background, GDK_TYPE_RGBA))
      background = nspg_boxed_get(nsp_background, GdkRGBA);
  else {
      Scierror( "Error: background should be a GdkRGBA\n");
      return RET_BUG;
  }
    ret =gtk_source_mark_attributes_get_background(GTK_SOURCE_MARK_ATTRIBUTES(self->obj),background);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_mark_attributes_set_icon_name(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *icon_name;
  if ( GetArgs(stack,rhs,opt,T,&icon_name) == FAIL) return RET_BUG;
    gtk_source_mark_attributes_set_icon_name(GTK_SOURCE_MARK_ATTRIBUTES(self->obj),icon_name);
  return 0;
}

static int _wrap_gtk_source_mark_attributes_get_icon_name(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_mark_attributes_get_icon_name(GTK_SOURCE_MARK_ATTRIBUTES(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_mark_attributes_set_gicon(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *gicon;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gicon, &gicon) == FAIL) return RET_BUG;
    gtk_source_mark_attributes_set_gicon(GTK_SOURCE_MARK_ATTRIBUTES(self->obj),G_ICON(gicon->obj));
  return 0;
}

static int _wrap_gtk_source_mark_attributes_get_gicon(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_mark_attributes_get_gicon(GTK_SOURCE_MARK_ATTRIBUTES(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_mark_attributes_get_tooltip_text(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *mark;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcemark, &mark) == FAIL) return RET_BUG;
    ret =gtk_source_mark_attributes_get_tooltip_text(GTK_SOURCE_MARK_ATTRIBUTES(self->obj),GTK_SOURCE_MARK(mark->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_mark_attributes_get_tooltip_markup(NspGtkSourceMarkAttributes *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *mark;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcemark, &mark) == FAIL) return RET_BUG;
    ret =gtk_source_mark_attributes_get_tooltip_markup(GTK_SOURCE_MARK_ATTRIBUTES(self->obj),GTK_SOURCE_MARK(mark->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods gtksourcemarkattributes_methods[] = {
  {"set_background",(nsp_method *) _wrap_gtk_source_mark_attributes_set_background},
  {"get_background",(nsp_method *) _wrap_gtk_source_mark_attributes_get_background},
  {"set_icon_name",(nsp_method *) _wrap_gtk_source_mark_attributes_set_icon_name},
  {"get_icon_name",(nsp_method *) _wrap_gtk_source_mark_attributes_get_icon_name},
  {"set_gicon",(nsp_method *) _wrap_gtk_source_mark_attributes_set_gicon},
  {"get_gicon",(nsp_method *) _wrap_gtk_source_mark_attributes_get_gicon},
  {"get_tooltip_text",(nsp_method *) _wrap_gtk_source_mark_attributes_get_tooltip_text},
  {"get_tooltip_markup",(nsp_method *) _wrap_gtk_source_mark_attributes_get_tooltip_markup},
  { NULL, NULL}
};

static NspMethods *gtksourcemarkattributes_get_methods(void) { return gtksourcemarkattributes_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcemarkattributes_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourcePrintCompositor ----------- */


#define  NspGtkSourcePrintCompositor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourceprintcompositor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourcePrintCompositor inherits from GObject 
 */

int nsp_type_gtksourceprintcompositor_id=0;
NspTypeGtkSourcePrintCompositor *nsp_type_gtksourceprintcompositor=NULL;

/*
 * Type object for NspGtkSourcePrintCompositor 
 * all the instance of NspTypeGtkSourcePrintCompositor share the same id. 
 * nsp_type_gtksourceprintcompositor: is an instance of NspTypeGtkSourcePrintCompositor 
 *    used for objects of NspGtkSourcePrintCompositor type (i.e built with new_gtksourceprintcompositor) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourcePrintCompositor *new_type_gtksourceprintcompositor(type_mode mode)
{
  NspTypeGtkSourcePrintCompositor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourceprintcompositor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourceprintcompositor;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourceprintcompositor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourceprintcompositor_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourceprintcompositor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourceprintcompositor */ 

  top->s_type =  (s_type_func *) nsp_gtksourceprintcompositor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourceprintcompositor_type_short_string;
  /* top->create = (create_func*) int_gtksourceprintcompositor_create;*/

  /* specific methods for gtksourceprintcompositor */

  type->init = (init_func *) init_gtksourceprintcompositor;

  /* 
   * NspGtkSourcePrintCompositor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourceprintcompositor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourcePrintCompositor called nsp_type_gtksourceprintcompositor
       */
      type->id =  nsp_type_gtksourceprintcompositor_id = nsp_new_type_id();
      nsp_type_gtksourceprintcompositor = type;
      if ( nsp_register_type(nsp_type_gtksourceprintcompositor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourceprintcompositor, GTK_SOURCE_TYPE_PRINT_COMPOSITOR);
      return ( mode == T_BASE ) ? type : new_type_gtksourceprintcompositor(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourceprintcompositor_id;
      return type;
    }
}

/*
 * initialize NspGtkSourcePrintCompositor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourceprintcompositor(NspGtkSourcePrintCompositor *Obj,NspTypeGtkSourcePrintCompositor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourcePrintCompositor 
 */

NspGtkSourcePrintCompositor *new_gtksourceprintcompositor() 
{
  NspGtkSourcePrintCompositor *loc;
  /* type must exists */
  nsp_type_gtksourceprintcompositor = new_type_gtksourceprintcompositor(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourcePrintCompositor)))== NULLGTKSOURCEPRINTCOMPOSITOR) return loc;
  /* initialize object */
  if ( init_gtksourceprintcompositor(loc,nsp_type_gtksourceprintcompositor) == FAIL) return NULLGTKSOURCEPRINTCOMPOSITOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourcePrintCompositor 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourceprintcompositor_type_name[]="GtkSourcePrintCompositor";
static char gtksourceprintcompositor_short_type_name[]="GtkSourcePrintCompositor";

static char *nsp_gtksourceprintcompositor_type_as_string(void)
{
  return(gtksourceprintcompositor_type_name);
}

static char *nsp_gtksourceprintcompositor_type_short_string(NspObject *v)
{
  return(gtksourceprintcompositor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourcePrintCompositor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourcePrintCompositor   *nsp_gtksourceprintcompositor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourceprintcompositor_id)  == TRUE  ) return ((NspGtkSourcePrintCompositor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourceprintcompositor));
  return NULL;
}

int IsGtkSourcePrintCompositorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourceprintcompositor_id);
}

int IsGtkSourcePrintCompositor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourceprintcompositor_id);
}

NspGtkSourcePrintCompositor  *GetGtkSourcePrintCompositorCopy(Stack stack, int i)
{
  if (  GetGtkSourcePrintCompositor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourcePrintCompositor  *GetGtkSourcePrintCompositor(Stack stack, int i)
{
  NspGtkSourcePrintCompositor *M;
  if (( M = nsp_gtksourceprintcompositor_object(NthObj(i))) == NULLGTKSOURCEPRINTCOMPOSITOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourcePrintCompositor *gtksourceprintcompositor_copy(NspGtkSourcePrintCompositor *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourceprintcompositor);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourceprintcompositor);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourcePrintCompositor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_print_compositor_new_from_view (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *view;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourceview, &view) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_print_compositor_new_from_view(GTK_SOURCE_VIEW(view->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourceprintcompositor = new_type_gtksourceprintcompositor(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceprintcompositor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gtk_source_print_compositor_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *buffer;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_print_compositor_new(GTK_SOURCE_BUFFER(buffer->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourceprintcompositor = new_type_gtksourceprintcompositor(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceprintcompositor);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_print_compositor_get_buffer(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceBuffer *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_buffer(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcebuffer))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_tab_width(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int width;
  if ( GetArgs(stack,rhs,opt,T,&width) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_tab_width(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),width);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_tab_width(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_tab_width(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_highlight_syntax(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int highlight;
  if ( GetArgs(stack,rhs,opt,T,&highlight) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_highlight_syntax(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),highlight);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_highlight_syntax(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_highlight_syntax(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_print_line_numbers(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int interval;
  if ( GetArgs(stack,rhs,opt,T,&interval) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_print_line_numbers(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),interval);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_print_line_numbers(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_print_line_numbers(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_body_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *font_name;
  if ( GetArgs(stack,rhs,opt,T,&font_name) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_body_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),font_name);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_body_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_body_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_line_numbers_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *font_name;
  if ( GetArgs(stack,rhs,opt,T,&font_name) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_line_numbers_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),font_name);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_line_numbers_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_line_numbers_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_header_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *font_name;
  if ( GetArgs(stack,rhs,opt,T,&font_name) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_header_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),font_name);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_header_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_header_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_footer_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *font_name;
  if ( GetArgs(stack,rhs,opt,T,&font_name) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_footer_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),font_name);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_footer_font_name(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_footer_font_name(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_print_header(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int print;
  if ( GetArgs(stack,rhs,opt,T,&print) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_print_header(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),print);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_print_header(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_print_header(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_print_footer(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int print;
  if ( GetArgs(stack,rhs,opt,T,&print) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_print_footer(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),print);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_print_footer(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_print_footer(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_set_header_format(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,string,string,string, t_end};
  int separator;
  char *left, *center, *right;
  if ( GetArgs(stack,rhs,opt,T,&separator, &left, &center, &right) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_header_format(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),separator,left,center,right);
  return 0;
}

static int _wrap_gtk_source_print_compositor_set_footer_format(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,string,string,string, t_end};
  int separator;
  char *left, *center, *right;
  if ( GetArgs(stack,rhs,opt,T,&separator, &left, &center, &right) == FAIL) return RET_BUG;
    gtk_source_print_compositor_set_footer_format(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),separator,left,center,right);
  return 0;
}

static int _wrap_gtk_source_print_compositor_get_n_pages(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_n_pages(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_paginate(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *context;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtkprintcontext, &context) == FAIL) return RET_BUG;
    ret =gtk_source_print_compositor_paginate(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),GTK_PRINT_CONTEXT(context->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_get_pagination_progress(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  double ret;
  CheckRhs(0,0);
    ret =gtk_source_print_compositor_get_pagination_progress(GTK_SOURCE_PRINT_COMPOSITOR(self->obj));
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_print_compositor_draw_page(NspGtkSourcePrintCompositor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *context;
  int page_nr;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtkprintcontext, &context, &page_nr) == FAIL) return RET_BUG;
    gtk_source_print_compositor_draw_page(GTK_SOURCE_PRINT_COMPOSITOR(self->obj),GTK_PRINT_CONTEXT(context->obj),page_nr);
  return 0;
}

static NspMethods gtksourceprintcompositor_methods[] = {
  {"get_buffer",(nsp_method *) _wrap_gtk_source_print_compositor_get_buffer},
  {"set_tab_width",(nsp_method *) _wrap_gtk_source_print_compositor_set_tab_width},
  {"get_tab_width",(nsp_method *) _wrap_gtk_source_print_compositor_get_tab_width},
  {"set_highlight_syntax",(nsp_method *) _wrap_gtk_source_print_compositor_set_highlight_syntax},
  {"get_highlight_syntax",(nsp_method *) _wrap_gtk_source_print_compositor_get_highlight_syntax},
  {"set_print_line_numbers",(nsp_method *) _wrap_gtk_source_print_compositor_set_print_line_numbers},
  {"get_print_line_numbers",(nsp_method *) _wrap_gtk_source_print_compositor_get_print_line_numbers},
  {"set_body_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_set_body_font_name},
  {"get_body_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_get_body_font_name},
  {"set_line_numbers_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_set_line_numbers_font_name},
  {"get_line_numbers_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_get_line_numbers_font_name},
  {"set_header_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_set_header_font_name},
  {"get_header_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_get_header_font_name},
  {"set_footer_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_set_footer_font_name},
  {"get_footer_font_name",(nsp_method *) _wrap_gtk_source_print_compositor_get_footer_font_name},
  {"set_print_header",(nsp_method *) _wrap_gtk_source_print_compositor_set_print_header},
  {"get_print_header",(nsp_method *) _wrap_gtk_source_print_compositor_get_print_header},
  {"set_print_footer",(nsp_method *) _wrap_gtk_source_print_compositor_set_print_footer},
  {"get_print_footer",(nsp_method *) _wrap_gtk_source_print_compositor_get_print_footer},
  {"set_header_format",(nsp_method *) _wrap_gtk_source_print_compositor_set_header_format},
  {"set_footer_format",(nsp_method *) _wrap_gtk_source_print_compositor_set_footer_format},
  {"get_n_pages",(nsp_method *) _wrap_gtk_source_print_compositor_get_n_pages},
  {"paginate",(nsp_method *) _wrap_gtk_source_print_compositor_paginate},
  {"get_pagination_progress",(nsp_method *) _wrap_gtk_source_print_compositor_get_pagination_progress},
  {"draw_page",(nsp_method *) _wrap_gtk_source_print_compositor_draw_page},
  { NULL, NULL}
};

static NspMethods *gtksourceprintcompositor_get_methods(void) { return gtksourceprintcompositor_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourceprintcompositor_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceSearchContext ----------- */


#define  NspGtkSourceSearchContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcesearchcontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceSearchContext inherits from GObject 
 */

int nsp_type_gtksourcesearchcontext_id=0;
NspTypeGtkSourceSearchContext *nsp_type_gtksourcesearchcontext=NULL;

/*
 * Type object for NspGtkSourceSearchContext 
 * all the instance of NspTypeGtkSourceSearchContext share the same id. 
 * nsp_type_gtksourcesearchcontext: is an instance of NspTypeGtkSourceSearchContext 
 *    used for objects of NspGtkSourceSearchContext type (i.e built with new_gtksourcesearchcontext) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceSearchContext *new_type_gtksourcesearchcontext(type_mode mode)
{
  NspTypeGtkSourceSearchContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcesearchcontext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcesearchcontext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcesearchcontext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcesearchcontext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcesearchcontext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcesearchcontext */ 

  top->s_type =  (s_type_func *) nsp_gtksourcesearchcontext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcesearchcontext_type_short_string;
  /* top->create = (create_func*) int_gtksourcesearchcontext_create;*/

  /* specific methods for gtksourcesearchcontext */

  type->init = (init_func *) init_gtksourcesearchcontext;

  /* 
   * NspGtkSourceSearchContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcesearchcontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceSearchContext called nsp_type_gtksourcesearchcontext
       */
      type->id =  nsp_type_gtksourcesearchcontext_id = nsp_new_type_id();
      nsp_type_gtksourcesearchcontext = type;
      if ( nsp_register_type(nsp_type_gtksourcesearchcontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcesearchcontext, GTK_SOURCE_TYPE_SEARCH_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_gtksourcesearchcontext(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcesearchcontext_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceSearchContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcesearchcontext(NspGtkSourceSearchContext *Obj,NspTypeGtkSourceSearchContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceSearchContext 
 */

NspGtkSourceSearchContext *new_gtksourcesearchcontext() 
{
  NspGtkSourceSearchContext *loc;
  /* type must exists */
  nsp_type_gtksourcesearchcontext = new_type_gtksourcesearchcontext(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceSearchContext)))== NULLGTKSOURCESEARCHCONTEXT) return loc;
  /* initialize object */
  if ( init_gtksourcesearchcontext(loc,nsp_type_gtksourcesearchcontext) == FAIL) return NULLGTKSOURCESEARCHCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceSearchContext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcesearchcontext_type_name[]="GtkSourceSearchContext";
static char gtksourcesearchcontext_short_type_name[]="GtkSourceSearchContext";

static char *nsp_gtksourcesearchcontext_type_as_string(void)
{
  return(gtksourcesearchcontext_type_name);
}

static char *nsp_gtksourcesearchcontext_type_short_string(NspObject *v)
{
  return(gtksourcesearchcontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceSearchContext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceSearchContext   *nsp_gtksourcesearchcontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcesearchcontext_id)  == TRUE  ) return ((NspGtkSourceSearchContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcesearchcontext));
  return NULL;
}

int IsGtkSourceSearchContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcesearchcontext_id);
}

int IsGtkSourceSearchContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcesearchcontext_id);
}

NspGtkSourceSearchContext  *GetGtkSourceSearchContextCopy(Stack stack, int i)
{
  if (  GetGtkSourceSearchContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceSearchContext  *GetGtkSourceSearchContext(Stack stack, int i)
{
  NspGtkSourceSearchContext *M;
  if (( M = nsp_gtksourcesearchcontext_object(NthObj(i))) == NULLGTKSOURCESEARCHCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceSearchContext *gtksourcesearchcontext_copy(NspGtkSourceSearchContext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcesearchcontext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcesearchcontext);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceSearchContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_search_context_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *buffer, *settings;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer, &nsp_type_gtksourcesearchsettings, &settings) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_search_context_new(GTK_SOURCE_BUFFER(buffer->obj),GTK_SOURCE_SEARCH_SETTINGS(settings->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourcesearchcontext = new_type_gtksourcesearchcontext(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcesearchcontext);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_search_context_get_buffer(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceBuffer *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_search_context_get_buffer(GTK_SOURCE_SEARCH_CONTEXT(self->obj));
  nsp_type_gtksourcebuffer = new_type_gtksourcebuffer(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcebuffer))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_search_context_get_settings(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceSearchSettings *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_search_context_get_settings(GTK_SOURCE_SEARCH_CONTEXT(self->obj));
  nsp_type_gtksourcesearchsettings = new_type_gtksourcesearchsettings(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcesearchsettings))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_search_context_set_settings(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *settings;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcesearchsettings, &settings) == FAIL) return RET_BUG;
    gtk_source_search_context_set_settings(GTK_SOURCE_SEARCH_CONTEXT(self->obj),GTK_SOURCE_SEARCH_SETTINGS(settings->obj));
  return 0;
}

static int _wrap_gtk_source_search_context_get_highlight(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_search_context_get_highlight(GTK_SOURCE_SEARCH_CONTEXT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_set_highlight(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int highlight;
  if ( GetArgs(stack,rhs,opt,T,&highlight) == FAIL) return RET_BUG;
    gtk_source_search_context_set_highlight(GTK_SOURCE_SEARCH_CONTEXT(self->obj),highlight);
  return 0;
}

static int _wrap_gtk_source_search_context_get_regex_error(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_search_context_get_regex_error(GTK_SOURCE_SEARCH_CONTEXT(self->obj));
  if ((nsp_ret = (NspObject *) gboxed_create(NVOID,G_TYPE_ERROR, ret, TRUE, TRUE,
                                             (NspTypeBase *) nsp_type_gerror))== NULL)
    return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_search_context_get_occurrences_count(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_search_context_get_occurrences_count(GTK_SOURCE_SEARCH_CONTEXT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_get_occurrence_position(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GtkTextIter *match_start = NULL, *match_end = NULL;
  NspObject *nsp_match_start = NULL, *nsp_match_end = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_match_start, &nsp_match_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_match_start, GTK_TYPE_TEXT_ITER))
      match_start = nspg_boxed_get(nsp_match_start, GtkTextIter);
  else {
      Scierror( "Error: match_start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_end, GTK_TYPE_TEXT_ITER))
      match_end = nspg_boxed_get(nsp_match_end, GtkTextIter);
  else {
      Scierror( "Error: match_end should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_search_context_get_occurrence_position(GTK_SOURCE_SEARCH_CONTEXT(self->obj),match_start,match_end);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_forward(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GtkTextIter *iter = NULL, *match_start = NULL, *match_end = NULL;
  NspObject *nsp_iter = NULL, *nsp_match_start = NULL, *nsp_match_end = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &nsp_match_start, &nsp_match_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_start, GTK_TYPE_TEXT_ITER))
      match_start = nspg_boxed_get(nsp_match_start, GtkTextIter);
  else {
      Scierror( "Error: match_start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_end, GTK_TYPE_TEXT_ITER))
      match_end = nspg_boxed_get(nsp_match_end, GtkTextIter);
  else {
      Scierror( "Error: match_end should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_search_context_forward(GTK_SOURCE_SEARCH_CONTEXT(self->obj),iter,match_start,match_end);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_forward_finish(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,obj, t_end};
  NspGObject *result;
  GtkTextIter *match_start = NULL, *match_end = NULL;
  NspObject *nsp_match_start = NULL, *nsp_match_end = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result, &nsp_match_start, &nsp_match_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_match_start, GTK_TYPE_TEXT_ITER))
      match_start = nspg_boxed_get(nsp_match_start, GtkTextIter);
  else {
      Scierror( "Error: match_start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_end, GTK_TYPE_TEXT_ITER))
      match_end = nspg_boxed_get(nsp_match_end, GtkTextIter);
  else {
      Scierror( "Error: match_end should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_search_context_forward_finish(GTK_SOURCE_SEARCH_CONTEXT(self->obj),G_ASYNC_RESULT(result->obj),match_start,match_end,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_backward(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GtkTextIter *iter = NULL, *match_start = NULL, *match_end = NULL;
  NspObject *nsp_iter = NULL, *nsp_match_start = NULL, *nsp_match_end = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter, &nsp_match_start, &nsp_match_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_start, GTK_TYPE_TEXT_ITER))
      match_start = nspg_boxed_get(nsp_match_start, GtkTextIter);
  else {
      Scierror( "Error: match_start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_end, GTK_TYPE_TEXT_ITER))
      match_end = nspg_boxed_get(nsp_match_end, GtkTextIter);
  else {
      Scierror( "Error: match_end should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_search_context_backward(GTK_SOURCE_SEARCH_CONTEXT(self->obj),iter,match_start,match_end);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_backward_finish(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,obj, t_end};
  NspGObject *result;
  GtkTextIter *match_start = NULL, *match_end = NULL;
  NspObject *nsp_match_start = NULL, *nsp_match_end = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result, &nsp_match_start, &nsp_match_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_match_start, GTK_TYPE_TEXT_ITER))
      match_start = nspg_boxed_get(nsp_match_start, GtkTextIter);
  else {
      Scierror( "Error: match_start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_end, GTK_TYPE_TEXT_ITER))
      match_end = nspg_boxed_get(nsp_match_end, GtkTextIter);
  else {
      Scierror( "Error: match_end should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_search_context_backward_finish(GTK_SOURCE_SEARCH_CONTEXT(self->obj),G_ASYNC_RESULT(result->obj),match_start,match_end,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_replace(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj,string,s_int, t_end};
  GtkTextIter *match_start = NULL, *match_end = NULL;
  NspObject *nsp_match_start = NULL, *nsp_match_end = NULL;
  char *replace;
  int replace_length, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_match_start, &nsp_match_end, &replace, &replace_length) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_match_start, GTK_TYPE_TEXT_ITER))
      match_start = nspg_boxed_get(nsp_match_start, GtkTextIter);
  else {
      Scierror( "Error: match_start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_match_end, GTK_TYPE_TEXT_ITER))
      match_end = nspg_boxed_get(nsp_match_end, GtkTextIter);
  else {
      Scierror( "Error: match_end should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_search_context_replace(GTK_SOURCE_SEARCH_CONTEXT(self->obj),match_start,match_end,replace,replace_length,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_context_replace_all(NspGtkSourceSearchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *replace;
  int replace_length, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&replace, &replace_length) == FAIL) return RET_BUG;
    ret =gtk_source_search_context_replace_all(GTK_SOURCE_SEARCH_CONTEXT(self->obj),replace,replace_length,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n%s\n",NspFname(stack),error->message);
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcesearchcontext_methods[] = {
  {"get_buffer",(nsp_method *) _wrap_gtk_source_search_context_get_buffer},
  {"get_settings",(nsp_method *) _wrap_gtk_source_search_context_get_settings},
  {"set_settings",(nsp_method *) _wrap_gtk_source_search_context_set_settings},
  {"get_highlight",(nsp_method *) _wrap_gtk_source_search_context_get_highlight},
  {"set_highlight",(nsp_method *) _wrap_gtk_source_search_context_set_highlight},
  {"get_regex_error",(nsp_method *) _wrap_gtk_source_search_context_get_regex_error},
  {"get_occurrences_count",(nsp_method *) _wrap_gtk_source_search_context_get_occurrences_count},
  {"get_occurrence_position",(nsp_method *) _wrap_gtk_source_search_context_get_occurrence_position},
  {"forward",(nsp_method *) _wrap_gtk_source_search_context_forward},
  {"forward_finish",(nsp_method *) _wrap_gtk_source_search_context_forward_finish},
  {"backward",(nsp_method *) _wrap_gtk_source_search_context_backward},
  {"backward_finish",(nsp_method *) _wrap_gtk_source_search_context_backward_finish},
  {"replace",(nsp_method *) _wrap_gtk_source_search_context_replace},
  {"replace_all",(nsp_method *) _wrap_gtk_source_search_context_replace_all},
  { NULL, NULL}
};

static NspMethods *gtksourcesearchcontext_get_methods(void) { return gtksourcesearchcontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcesearchcontext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceSearchSettings ----------- */


#define  NspGtkSourceSearchSettings_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcesearchsettings.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceSearchSettings inherits from GObject 
 */

int nsp_type_gtksourcesearchsettings_id=0;
NspTypeGtkSourceSearchSettings *nsp_type_gtksourcesearchsettings=NULL;

/*
 * Type object for NspGtkSourceSearchSettings 
 * all the instance of NspTypeGtkSourceSearchSettings share the same id. 
 * nsp_type_gtksourcesearchsettings: is an instance of NspTypeGtkSourceSearchSettings 
 *    used for objects of NspGtkSourceSearchSettings type (i.e built with new_gtksourcesearchsettings) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceSearchSettings *new_type_gtksourcesearchsettings(type_mode mode)
{
  NspTypeGtkSourceSearchSettings *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcesearchsettings != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcesearchsettings;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcesearchsettings_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcesearchsettings_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcesearchsettings;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcesearchsettings */ 

  top->s_type =  (s_type_func *) nsp_gtksourcesearchsettings_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcesearchsettings_type_short_string;
  /* top->create = (create_func*) int_gtksourcesearchsettings_create;*/

  /* specific methods for gtksourcesearchsettings */

  type->init = (init_func *) init_gtksourcesearchsettings;

  /* 
   * NspGtkSourceSearchSettings interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcesearchsettings_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceSearchSettings called nsp_type_gtksourcesearchsettings
       */
      type->id =  nsp_type_gtksourcesearchsettings_id = nsp_new_type_id();
      nsp_type_gtksourcesearchsettings = type;
      if ( nsp_register_type(nsp_type_gtksourcesearchsettings) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcesearchsettings, GTK_SOURCE_TYPE_SEARCH_SETTINGS);
      return ( mode == T_BASE ) ? type : new_type_gtksourcesearchsettings(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcesearchsettings_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceSearchSettings instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcesearchsettings(NspGtkSourceSearchSettings *Obj,NspTypeGtkSourceSearchSettings *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceSearchSettings 
 */

NspGtkSourceSearchSettings *new_gtksourcesearchsettings() 
{
  NspGtkSourceSearchSettings *loc;
  /* type must exists */
  nsp_type_gtksourcesearchsettings = new_type_gtksourcesearchsettings(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceSearchSettings)))== NULLGTKSOURCESEARCHSETTINGS) return loc;
  /* initialize object */
  if ( init_gtksourcesearchsettings(loc,nsp_type_gtksourcesearchsettings) == FAIL) return NULLGTKSOURCESEARCHSETTINGS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceSearchSettings 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcesearchsettings_type_name[]="GtkSourceSearchSettings";
static char gtksourcesearchsettings_short_type_name[]="GtkSourceSearchSettings";

static char *nsp_gtksourcesearchsettings_type_as_string(void)
{
  return(gtksourcesearchsettings_type_name);
}

static char *nsp_gtksourcesearchsettings_type_short_string(NspObject *v)
{
  return(gtksourcesearchsettings_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceSearchSettings objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceSearchSettings   *nsp_gtksourcesearchsettings_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcesearchsettings_id)  == TRUE  ) return ((NspGtkSourceSearchSettings *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcesearchsettings));
  return NULL;
}

int IsGtkSourceSearchSettingsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcesearchsettings_id);
}

int IsGtkSourceSearchSettings(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcesearchsettings_id);
}

NspGtkSourceSearchSettings  *GetGtkSourceSearchSettingsCopy(Stack stack, int i)
{
  if (  GetGtkSourceSearchSettings(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceSearchSettings  *GetGtkSourceSearchSettings(Stack stack, int i)
{
  NspGtkSourceSearchSettings *M;
  if (( M = nsp_gtksourcesearchsettings_object(NthObj(i))) == NULLGTKSOURCESEARCHSETTINGS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceSearchSettings *gtksourcesearchsettings_copy(NspGtkSourceSearchSettings *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcesearchsettings);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcesearchsettings);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceSearchSettings
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_search_settings_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_search_settings_new())== NULL) return RET_BUG;

  nsp_type_gtksourcesearchsettings = new_type_gtksourcesearchsettings(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcesearchsettings);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_search_settings_set_search_text(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *search_text;
  if ( GetArgs(stack,rhs,opt,T,&search_text) == FAIL) return RET_BUG;
    gtk_source_search_settings_set_search_text(GTK_SOURCE_SEARCH_SETTINGS(self->obj),search_text);
  return 0;
}

static int _wrap_gtk_source_search_settings_get_search_text(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_search_settings_get_search_text(GTK_SOURCE_SEARCH_SETTINGS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_settings_set_case_sensitive(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int case_sensitive;
  if ( GetArgs(stack,rhs,opt,T,&case_sensitive) == FAIL) return RET_BUG;
    gtk_source_search_settings_set_case_sensitive(GTK_SOURCE_SEARCH_SETTINGS(self->obj),case_sensitive);
  return 0;
}

static int _wrap_gtk_source_search_settings_get_case_sensitive(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_search_settings_get_case_sensitive(GTK_SOURCE_SEARCH_SETTINGS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_settings_set_at_word_boundaries(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int at_word_boundaries;
  if ( GetArgs(stack,rhs,opt,T,&at_word_boundaries) == FAIL) return RET_BUG;
    gtk_source_search_settings_set_at_word_boundaries(GTK_SOURCE_SEARCH_SETTINGS(self->obj),at_word_boundaries);
  return 0;
}

static int _wrap_gtk_source_search_settings_get_at_word_boundaries(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_search_settings_get_at_word_boundaries(GTK_SOURCE_SEARCH_SETTINGS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_settings_set_wrap_around(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int wrap_around;
  if ( GetArgs(stack,rhs,opt,T,&wrap_around) == FAIL) return RET_BUG;
    gtk_source_search_settings_set_wrap_around(GTK_SOURCE_SEARCH_SETTINGS(self->obj),wrap_around);
  return 0;
}

static int _wrap_gtk_source_search_settings_get_wrap_around(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_search_settings_get_wrap_around(GTK_SOURCE_SEARCH_SETTINGS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_search_settings_set_regex_enabled(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int regex_enabled;
  if ( GetArgs(stack,rhs,opt,T,&regex_enabled) == FAIL) return RET_BUG;
    gtk_source_search_settings_set_regex_enabled(GTK_SOURCE_SEARCH_SETTINGS(self->obj),regex_enabled);
  return 0;
}

static int _wrap_gtk_source_search_settings_get_regex_enabled(NspGtkSourceSearchSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_search_settings_get_regex_enabled(GTK_SOURCE_SEARCH_SETTINGS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcesearchsettings_methods[] = {
  {"set_search_text",(nsp_method *) _wrap_gtk_source_search_settings_set_search_text},
  {"get_search_text",(nsp_method *) _wrap_gtk_source_search_settings_get_search_text},
  {"set_case_sensitive",(nsp_method *) _wrap_gtk_source_search_settings_set_case_sensitive},
  {"get_case_sensitive",(nsp_method *) _wrap_gtk_source_search_settings_get_case_sensitive},
  {"set_at_word_boundaries",(nsp_method *) _wrap_gtk_source_search_settings_set_at_word_boundaries},
  {"get_at_word_boundaries",(nsp_method *) _wrap_gtk_source_search_settings_get_at_word_boundaries},
  {"set_wrap_around",(nsp_method *) _wrap_gtk_source_search_settings_set_wrap_around},
  {"get_wrap_around",(nsp_method *) _wrap_gtk_source_search_settings_get_wrap_around},
  {"set_regex_enabled",(nsp_method *) _wrap_gtk_source_search_settings_set_regex_enabled},
  {"get_regex_enabled",(nsp_method *) _wrap_gtk_source_search_settings_get_regex_enabled},
  { NULL, NULL}
};

static NspMethods *gtksourcesearchsettings_get_methods(void) { return gtksourcesearchsettings_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcesearchsettings_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceStyleScheme ----------- */


#define  NspGtkSourceStyleScheme_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcestylescheme.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceStyleScheme inherits from GObject 
 */

int nsp_type_gtksourcestylescheme_id=0;
NspTypeGtkSourceStyleScheme *nsp_type_gtksourcestylescheme=NULL;

/*
 * Type object for NspGtkSourceStyleScheme 
 * all the instance of NspTypeGtkSourceStyleScheme share the same id. 
 * nsp_type_gtksourcestylescheme: is an instance of NspTypeGtkSourceStyleScheme 
 *    used for objects of NspGtkSourceStyleScheme type (i.e built with new_gtksourcestylescheme) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceStyleScheme *new_type_gtksourcestylescheme(type_mode mode)
{
  NspTypeGtkSourceStyleScheme *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcestylescheme != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcestylescheme;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcestylescheme_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcestylescheme_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcestylescheme;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcestylescheme */ 

  top->s_type =  (s_type_func *) nsp_gtksourcestylescheme_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcestylescheme_type_short_string;
  /* top->create = (create_func*) int_gtksourcestylescheme_create;*/

  /* specific methods for gtksourcestylescheme */

  type->init = (init_func *) init_gtksourcestylescheme;

  /* 
   * NspGtkSourceStyleScheme interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcestylescheme_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceStyleScheme called nsp_type_gtksourcestylescheme
       */
      type->id =  nsp_type_gtksourcestylescheme_id = nsp_new_type_id();
      nsp_type_gtksourcestylescheme = type;
      if ( nsp_register_type(nsp_type_gtksourcestylescheme) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcestylescheme, GTK_SOURCE_TYPE_STYLE_SCHEME);
      return ( mode == T_BASE ) ? type : new_type_gtksourcestylescheme(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcestylescheme_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceStyleScheme instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcestylescheme(NspGtkSourceStyleScheme *Obj,NspTypeGtkSourceStyleScheme *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceStyleScheme 
 */

NspGtkSourceStyleScheme *new_gtksourcestylescheme() 
{
  NspGtkSourceStyleScheme *loc;
  /* type must exists */
  nsp_type_gtksourcestylescheme = new_type_gtksourcestylescheme(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceStyleScheme)))== NULLGTKSOURCESTYLESCHEME) return loc;
  /* initialize object */
  if ( init_gtksourcestylescheme(loc,nsp_type_gtksourcestylescheme) == FAIL) return NULLGTKSOURCESTYLESCHEME;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceStyleScheme 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcestylescheme_type_name[]="GtkSourceStyleScheme";
static char gtksourcestylescheme_short_type_name[]="GtkSourceStyleScheme";

static char *nsp_gtksourcestylescheme_type_as_string(void)
{
  return(gtksourcestylescheme_type_name);
}

static char *nsp_gtksourcestylescheme_type_short_string(NspObject *v)
{
  return(gtksourcestylescheme_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceStyleScheme objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceStyleScheme   *nsp_gtksourcestylescheme_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcestylescheme_id)  == TRUE  ) return ((NspGtkSourceStyleScheme *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcestylescheme));
  return NULL;
}

int IsGtkSourceStyleSchemeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcestylescheme_id);
}

int IsGtkSourceStyleScheme(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcestylescheme_id);
}

NspGtkSourceStyleScheme  *GetGtkSourceStyleSchemeCopy(Stack stack, int i)
{
  if (  GetGtkSourceStyleScheme(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceStyleScheme  *GetGtkSourceStyleScheme(Stack stack, int i)
{
  NspGtkSourceStyleScheme *M;
  if (( M = nsp_gtksourcestylescheme_object(NthObj(i))) == NULLGTKSOURCESTYLESCHEME)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceStyleScheme *gtksourcestylescheme_copy(NspGtkSourceStyleScheme *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestylescheme);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestylescheme);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceStyleScheme
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_style_scheme_get_id(NspGtkSourceStyleScheme *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_style_scheme_get_id(GTK_SOURCE_STYLE_SCHEME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_style_scheme_get_name(NspGtkSourceStyleScheme *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_style_scheme_get_name(GTK_SOURCE_STYLE_SCHEME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_style_scheme_get_description(NspGtkSourceStyleScheme *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_style_scheme_get_description(GTK_SOURCE_STYLE_SCHEME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_style_scheme_get_filename(NspGtkSourceStyleScheme *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =gtk_source_style_scheme_get_filename(GTK_SOURCE_STYLE_SCHEME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourcestylescheme_methods[] = {
  {"get_id",(nsp_method *) _wrap_gtk_source_style_scheme_get_id},
  {"get_name",(nsp_method *) _wrap_gtk_source_style_scheme_get_name},
  {"get_description",(nsp_method *) _wrap_gtk_source_style_scheme_get_description},
  {"get_filename",(nsp_method *) _wrap_gtk_source_style_scheme_get_filename},
  { NULL, NULL}
};

static NspMethods *gtksourcestylescheme_get_methods(void) { return gtksourcestylescheme_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcestylescheme_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceStyleSchemeChooserButton ----------- */


#define  NspGtkSourceStyleSchemeChooserButton_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcestyleschemechooserbutton.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceStyleSchemeChooserButton inherits from GtkButton 
 */

int nsp_type_gtksourcestyleschemechooserbutton_id=0;
NspTypeGtkSourceStyleSchemeChooserButton *nsp_type_gtksourcestyleschemechooserbutton=NULL;

/*
 * Type object for NspGtkSourceStyleSchemeChooserButton 
 * all the instance of NspTypeGtkSourceStyleSchemeChooserButton share the same id. 
 * nsp_type_gtksourcestyleschemechooserbutton: is an instance of NspTypeGtkSourceStyleSchemeChooserButton 
 *    used for objects of NspGtkSourceStyleSchemeChooserButton type (i.e built with new_gtksourcestyleschemechooserbutton) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceStyleSchemeChooserButton *new_type_gtksourcestyleschemechooserbutton(type_mode mode)
{
  NspTypeGtkSourceStyleSchemeChooserButton *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcestyleschemechooserbutton != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcestyleschemechooserbutton;
    }
  if (( type =  malloc(sizeof(NspTypeGtkButton))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtkbutton(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcestyleschemechooserbutton_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcestyleschemechooserbutton_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcestyleschemechooserbutton;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcestyleschemechooserbutton */ 

  top->s_type =  (s_type_func *) nsp_gtksourcestyleschemechooserbutton_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcestyleschemechooserbutton_type_short_string;
  /* top->create = (create_func*) int_gtksourcestyleschemechooserbutton_create;*/

  /* specific methods for gtksourcestyleschemechooserbutton */

  type->init = (init_func *) init_gtksourcestyleschemechooserbutton;

  /* 
   * NspGtkSourceStyleSchemeChooserButton interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcestyleschemechooserbutton_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceStyleSchemeChooserButton called nsp_type_gtksourcestyleschemechooserbutton
       */
      type->id =  nsp_type_gtksourcestyleschemechooserbutton_id = nsp_new_type_id();
      nsp_type_gtksourcestyleschemechooserbutton = type;
      if ( nsp_register_type(nsp_type_gtksourcestyleschemechooserbutton) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcestyleschemechooserbutton, GTK_SOURCE_TYPE_STYLE_SCHEME_CHOOSER_BUTTON);
      return ( mode == T_BASE ) ? type : new_type_gtksourcestyleschemechooserbutton(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcestyleschemechooserbutton_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceStyleSchemeChooserButton instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcestyleschemechooserbutton(NspGtkSourceStyleSchemeChooserButton *Obj,NspTypeGtkSourceStyleSchemeChooserButton *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceStyleSchemeChooserButton 
 */

NspGtkSourceStyleSchemeChooserButton *new_gtksourcestyleschemechooserbutton() 
{
  NspGtkSourceStyleSchemeChooserButton *loc;
  /* type must exists */
  nsp_type_gtksourcestyleschemechooserbutton = new_type_gtksourcestyleschemechooserbutton(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceStyleSchemeChooserButton)))== NULLGTKSOURCESTYLESCHEMECHOOSERBUTTON) return loc;
  /* initialize object */
  if ( init_gtksourcestyleschemechooserbutton(loc,nsp_type_gtksourcestyleschemechooserbutton) == FAIL) return NULLGTKSOURCESTYLESCHEMECHOOSERBUTTON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceStyleSchemeChooserButton 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcestyleschemechooserbutton_type_name[]="GtkSourceStyleSchemeChooserButton";
static char gtksourcestyleschemechooserbutton_short_type_name[]="GtkSourceStyleSchemeChooserButton";

static char *nsp_gtksourcestyleschemechooserbutton_type_as_string(void)
{
  return(gtksourcestyleschemechooserbutton_type_name);
}

static char *nsp_gtksourcestyleschemechooserbutton_type_short_string(NspObject *v)
{
  return(gtksourcestyleschemechooserbutton_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceStyleSchemeChooserButton objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceStyleSchemeChooserButton   *nsp_gtksourcestyleschemechooserbutton_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcestyleschemechooserbutton_id)  == TRUE  ) return ((NspGtkSourceStyleSchemeChooserButton *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcestyleschemechooserbutton));
  return NULL;
}

int IsGtkSourceStyleSchemeChooserButtonObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcestyleschemechooserbutton_id);
}

int IsGtkSourceStyleSchemeChooserButton(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcestyleschemechooserbutton_id);
}

NspGtkSourceStyleSchemeChooserButton  *GetGtkSourceStyleSchemeChooserButtonCopy(Stack stack, int i)
{
  if (  GetGtkSourceStyleSchemeChooserButton(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceStyleSchemeChooserButton  *GetGtkSourceStyleSchemeChooserButton(Stack stack, int i)
{
  NspGtkSourceStyleSchemeChooserButton *M;
  if (( M = nsp_gtksourcestyleschemechooserbutton_object(NthObj(i))) == NULLGTKSOURCESTYLESCHEMECHOOSERBUTTON)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceStyleSchemeChooserButton *gtksourcestyleschemechooserbutton_copy(NspGtkSourceStyleSchemeChooserButton *self)
{
  /* return gtkbutton_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestyleschemechooserbutton);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestyleschemechooserbutton);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceStyleSchemeChooserButton
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_style_scheme_chooser_button_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_style_scheme_chooser_button_new())== NULL) return RET_BUG;

  nsp_type_gtksourcestyleschemechooserbutton = new_type_gtksourcestyleschemechooserbutton(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcestyleschemechooserbutton);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gtksourcestyleschemechooserbutton_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcestyleschemechooserbutton_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceStyleSchemeChooserWidget ----------- */


#define  NspGtkSourceStyleSchemeChooserWidget_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcestyleschemechooserwidget.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceStyleSchemeChooserWidget inherits from GtkBin 
 */

int nsp_type_gtksourcestyleschemechooserwidget_id=0;
NspTypeGtkSourceStyleSchemeChooserWidget *nsp_type_gtksourcestyleschemechooserwidget=NULL;

/*
 * Type object for NspGtkSourceStyleSchemeChooserWidget 
 * all the instance of NspTypeGtkSourceStyleSchemeChooserWidget share the same id. 
 * nsp_type_gtksourcestyleschemechooserwidget: is an instance of NspTypeGtkSourceStyleSchemeChooserWidget 
 *    used for objects of NspGtkSourceStyleSchemeChooserWidget type (i.e built with new_gtksourcestyleschemechooserwidget) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceStyleSchemeChooserWidget *new_type_gtksourcestyleschemechooserwidget(type_mode mode)
{
  NspTypeGtkSourceStyleSchemeChooserWidget *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcestyleschemechooserwidget != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcestyleschemechooserwidget;
    }
  if (( type =  malloc(sizeof(NspTypeGtkBin))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtkbin(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcestyleschemechooserwidget_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcestyleschemechooserwidget_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcestyleschemechooserwidget;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcestyleschemechooserwidget */ 

  top->s_type =  (s_type_func *) nsp_gtksourcestyleschemechooserwidget_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcestyleschemechooserwidget_type_short_string;
  /* top->create = (create_func*) int_gtksourcestyleschemechooserwidget_create;*/

  /* specific methods for gtksourcestyleschemechooserwidget */

  type->init = (init_func *) init_gtksourcestyleschemechooserwidget;

  /* 
   * NspGtkSourceStyleSchemeChooserWidget interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcestyleschemechooserwidget_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceStyleSchemeChooserWidget called nsp_type_gtksourcestyleschemechooserwidget
       */
      type->id =  nsp_type_gtksourcestyleschemechooserwidget_id = nsp_new_type_id();
      nsp_type_gtksourcestyleschemechooserwidget = type;
      if ( nsp_register_type(nsp_type_gtksourcestyleschemechooserwidget) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcestyleschemechooserwidget, GTK_SOURCE_TYPE_STYLE_SCHEME_CHOOSER_WIDGET);
      return ( mode == T_BASE ) ? type : new_type_gtksourcestyleschemechooserwidget(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcestyleschemechooserwidget_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceStyleSchemeChooserWidget instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcestyleschemechooserwidget(NspGtkSourceStyleSchemeChooserWidget *Obj,NspTypeGtkSourceStyleSchemeChooserWidget *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceStyleSchemeChooserWidget 
 */

NspGtkSourceStyleSchemeChooserWidget *new_gtksourcestyleschemechooserwidget() 
{
  NspGtkSourceStyleSchemeChooserWidget *loc;
  /* type must exists */
  nsp_type_gtksourcestyleschemechooserwidget = new_type_gtksourcestyleschemechooserwidget(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceStyleSchemeChooserWidget)))== NULLGTKSOURCESTYLESCHEMECHOOSERWIDGET) return loc;
  /* initialize object */
  if ( init_gtksourcestyleschemechooserwidget(loc,nsp_type_gtksourcestyleschemechooserwidget) == FAIL) return NULLGTKSOURCESTYLESCHEMECHOOSERWIDGET;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceStyleSchemeChooserWidget 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcestyleschemechooserwidget_type_name[]="GtkSourceStyleSchemeChooserWidget";
static char gtksourcestyleschemechooserwidget_short_type_name[]="GtkSourceStyleSchemeChooserWidget";

static char *nsp_gtksourcestyleschemechooserwidget_type_as_string(void)
{
  return(gtksourcestyleschemechooserwidget_type_name);
}

static char *nsp_gtksourcestyleschemechooserwidget_type_short_string(NspObject *v)
{
  return(gtksourcestyleschemechooserwidget_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceStyleSchemeChooserWidget objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceStyleSchemeChooserWidget   *nsp_gtksourcestyleschemechooserwidget_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcestyleschemechooserwidget_id)  == TRUE  ) return ((NspGtkSourceStyleSchemeChooserWidget *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcestyleschemechooserwidget));
  return NULL;
}

int IsGtkSourceStyleSchemeChooserWidgetObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcestyleschemechooserwidget_id);
}

int IsGtkSourceStyleSchemeChooserWidget(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcestyleschemechooserwidget_id);
}

NspGtkSourceStyleSchemeChooserWidget  *GetGtkSourceStyleSchemeChooserWidgetCopy(Stack stack, int i)
{
  if (  GetGtkSourceStyleSchemeChooserWidget(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceStyleSchemeChooserWidget  *GetGtkSourceStyleSchemeChooserWidget(Stack stack, int i)
{
  NspGtkSourceStyleSchemeChooserWidget *M;
  if (( M = nsp_gtksourcestyleschemechooserwidget_object(NthObj(i))) == NULLGTKSOURCESTYLESCHEMECHOOSERWIDGET)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceStyleSchemeChooserWidget *gtksourcestyleschemechooserwidget_copy(NspGtkSourceStyleSchemeChooserWidget *self)
{
  /* return gtkbin_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestyleschemechooserwidget);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestyleschemechooserwidget);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceStyleSchemeChooserWidget
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_style_scheme_chooser_widget_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_style_scheme_chooser_widget_new())== NULL) return RET_BUG;

  nsp_type_gtksourcestyleschemechooserwidget = new_type_gtksourcestyleschemechooserwidget(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcestyleschemechooserwidget);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gtksourcestyleschemechooserwidget_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcestyleschemechooserwidget_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceStyleSchemeManager ----------- */


#define  NspGtkSourceStyleSchemeManager_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcestyleschememanager.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceStyleSchemeManager inherits from GObject 
 */

int nsp_type_gtksourcestyleschememanager_id=0;
NspTypeGtkSourceStyleSchemeManager *nsp_type_gtksourcestyleschememanager=NULL;

/*
 * Type object for NspGtkSourceStyleSchemeManager 
 * all the instance of NspTypeGtkSourceStyleSchemeManager share the same id. 
 * nsp_type_gtksourcestyleschememanager: is an instance of NspTypeGtkSourceStyleSchemeManager 
 *    used for objects of NspGtkSourceStyleSchemeManager type (i.e built with new_gtksourcestyleschememanager) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceStyleSchemeManager *new_type_gtksourcestyleschememanager(type_mode mode)
{
  NspTypeGtkSourceStyleSchemeManager *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcestyleschememanager != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcestyleschememanager;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcestyleschememanager_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcestyleschememanager_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcestyleschememanager;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcestyleschememanager */ 

  top->s_type =  (s_type_func *) nsp_gtksourcestyleschememanager_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcestyleschememanager_type_short_string;
  /* top->create = (create_func*) int_gtksourcestyleschememanager_create;*/

  /* specific methods for gtksourcestyleschememanager */

  type->init = (init_func *) init_gtksourcestyleschememanager;

  /* 
   * NspGtkSourceStyleSchemeManager interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcestyleschememanager_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceStyleSchemeManager called nsp_type_gtksourcestyleschememanager
       */
      type->id =  nsp_type_gtksourcestyleschememanager_id = nsp_new_type_id();
      nsp_type_gtksourcestyleschememanager = type;
      if ( nsp_register_type(nsp_type_gtksourcestyleschememanager) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcestyleschememanager, GTK_SOURCE_TYPE_STYLE_SCHEME_MANAGER);
      return ( mode == T_BASE ) ? type : new_type_gtksourcestyleschememanager(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcestyleschememanager_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceStyleSchemeManager instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcestyleschememanager(NspGtkSourceStyleSchemeManager *Obj,NspTypeGtkSourceStyleSchemeManager *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceStyleSchemeManager 
 */

NspGtkSourceStyleSchemeManager *new_gtksourcestyleschememanager() 
{
  NspGtkSourceStyleSchemeManager *loc;
  /* type must exists */
  nsp_type_gtksourcestyleschememanager = new_type_gtksourcestyleschememanager(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceStyleSchemeManager)))== NULLGTKSOURCESTYLESCHEMEMANAGER) return loc;
  /* initialize object */
  if ( init_gtksourcestyleschememanager(loc,nsp_type_gtksourcestyleschememanager) == FAIL) return NULLGTKSOURCESTYLESCHEMEMANAGER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceStyleSchemeManager 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcestyleschememanager_type_name[]="GtkSourceStyleSchemeManager";
static char gtksourcestyleschememanager_short_type_name[]="GtkSourceStyleSchemeManager";

static char *nsp_gtksourcestyleschememanager_type_as_string(void)
{
  return(gtksourcestyleschememanager_type_name);
}

static char *nsp_gtksourcestyleschememanager_type_short_string(NspObject *v)
{
  return(gtksourcestyleschememanager_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceStyleSchemeManager objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceStyleSchemeManager   *nsp_gtksourcestyleschememanager_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcestyleschememanager_id)  == TRUE  ) return ((NspGtkSourceStyleSchemeManager *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcestyleschememanager));
  return NULL;
}

int IsGtkSourceStyleSchemeManagerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcestyleschememanager_id);
}

int IsGtkSourceStyleSchemeManager(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcestyleschememanager_id);
}

NspGtkSourceStyleSchemeManager  *GetGtkSourceStyleSchemeManagerCopy(Stack stack, int i)
{
  if (  GetGtkSourceStyleSchemeManager(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceStyleSchemeManager  *GetGtkSourceStyleSchemeManager(Stack stack, int i)
{
  NspGtkSourceStyleSchemeManager *M;
  if (( M = nsp_gtksourcestyleschememanager_object(NthObj(i))) == NULLGTKSOURCESTYLESCHEMEMANAGER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceStyleSchemeManager *gtksourcestyleschememanager_copy(NspGtkSourceStyleSchemeManager *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestyleschememanager);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcestyleschememanager);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceStyleSchemeManager
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_style_scheme_manager_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_style_scheme_manager_new())== NULL) return RET_BUG;

  nsp_type_gtksourcestyleschememanager = new_type_gtksourcestyleschememanager(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcestyleschememanager);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_style_scheme_manager_set_search_path(NspGtkSourceStyleSchemeManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  gchar **path = NULL;
  NspObject *nsp_path = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_path) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_path))
    { path =  ((NspSMatrix *) nsp_path)->S;}
  else
    {
      Scierror("Error: path should be of type SMat\n");
      return RET_BUG;
    }
    gtk_source_style_scheme_manager_set_search_path(GTK_SOURCE_STYLE_SCHEME_MANAGER(self->obj),path);
  return 0;
}

static int _wrap_gtk_source_style_scheme_manager_append_search_path(NspGtkSourceStyleSchemeManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *path;
  if ( GetArgs(stack,rhs,opt,T,&path) == FAIL) return RET_BUG;
    gtk_source_style_scheme_manager_append_search_path(GTK_SOURCE_STYLE_SCHEME_MANAGER(self->obj),path);
  return 0;
}

static int _wrap_gtk_source_style_scheme_manager_prepend_search_path(NspGtkSourceStyleSchemeManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *path;
  if ( GetArgs(stack,rhs,opt,T,&path) == FAIL) return RET_BUG;
    gtk_source_style_scheme_manager_prepend_search_path(GTK_SOURCE_STYLE_SCHEME_MANAGER(self->obj),path);
  return 0;
}

static int _wrap_gtk_source_style_scheme_manager_force_rescan(NspGtkSourceStyleSchemeManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_style_scheme_manager_force_rescan(GTK_SOURCE_STYLE_SCHEME_MANAGER(self->obj));
  return 0;
}

static int _wrap_gtk_source_style_scheme_manager_get_scheme(NspGtkSourceStyleSchemeManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *scheme_id;
  GtkSourceStyleScheme *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&scheme_id) == FAIL) return RET_BUG;
    ret =gtk_source_style_scheme_manager_get_scheme(GTK_SOURCE_STYLE_SCHEME_MANAGER(self->obj),scheme_id);
  nsp_type_gtksourcestylescheme = new_type_gtksourcestylescheme(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcestylescheme))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gtksourcestyleschememanager_methods[] = {
  {"set_search_path",(nsp_method *) _wrap_gtk_source_style_scheme_manager_set_search_path},
  {"append_search_path",(nsp_method *) _wrap_gtk_source_style_scheme_manager_append_search_path},
  {"prepend_search_path",(nsp_method *) _wrap_gtk_source_style_scheme_manager_prepend_search_path},
  {"force_rescan",(nsp_method *) _wrap_gtk_source_style_scheme_manager_force_rescan},
  {"get_scheme",(nsp_method *) _wrap_gtk_source_style_scheme_manager_get_scheme},
  { NULL, NULL}
};

static NspMethods *gtksourcestyleschememanager_get_methods(void) { return gtksourcestyleschememanager_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcestyleschememanager_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceUndoManager ----------- */


#define  NspGtkSourceUndoManager_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourceundomanager.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceUndoManager inherits from GObject 
 */

int nsp_type_gtksourceundomanager_id=0;
NspTypeGtkSourceUndoManager *nsp_type_gtksourceundomanager=NULL;

/*
 * Type object for NspGtkSourceUndoManager 
 * all the instance of NspTypeGtkSourceUndoManager share the same id. 
 * nsp_type_gtksourceundomanager: is an instance of NspTypeGtkSourceUndoManager 
 *    used for objects of NspGtkSourceUndoManager type (i.e built with new_gtksourceundomanager) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceUndoManager *new_type_gtksourceundomanager(type_mode mode)
{
  NspTypeGtkSourceUndoManager *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourceundomanager != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourceundomanager;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourceundomanager_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourceundomanager_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourceundomanager;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourceundomanager */ 

  top->s_type =  (s_type_func *) nsp_gtksourceundomanager_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourceundomanager_type_short_string;
  /* top->create = (create_func*) int_gtksourceundomanager_create;*/

  /* specific methods for gtksourceundomanager */

  type->init = (init_func *) init_gtksourceundomanager;

  /* 
   * NspGtkSourceUndoManager interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourceundomanager_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceUndoManager called nsp_type_gtksourceundomanager
       */
      type->id =  nsp_type_gtksourceundomanager_id = nsp_new_type_id();
      nsp_type_gtksourceundomanager = type;
      if ( nsp_register_type(nsp_type_gtksourceundomanager) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourceundomanager, GTK_SOURCE_TYPE_UNDO_MANAGER);
      return ( mode == T_BASE ) ? type : new_type_gtksourceundomanager(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourceundomanager_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceUndoManager instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourceundomanager(NspGtkSourceUndoManager *Obj,NspTypeGtkSourceUndoManager *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceUndoManager 
 */

NspGtkSourceUndoManager *new_gtksourceundomanager() 
{
  NspGtkSourceUndoManager *loc;
  /* type must exists */
  nsp_type_gtksourceundomanager = new_type_gtksourceundomanager(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceUndoManager)))== NULLGTKSOURCEUNDOMANAGER) return loc;
  /* initialize object */
  if ( init_gtksourceundomanager(loc,nsp_type_gtksourceundomanager) == FAIL) return NULLGTKSOURCEUNDOMANAGER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceUndoManager 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourceundomanager_type_name[]="GtkSourceUndoManager";
static char gtksourceundomanager_short_type_name[]="GtkSourceUndoManager";

static char *nsp_gtksourceundomanager_type_as_string(void)
{
  return(gtksourceundomanager_type_name);
}

static char *nsp_gtksourceundomanager_type_short_string(NspObject *v)
{
  return(gtksourceundomanager_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceUndoManager objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceUndoManager   *nsp_gtksourceundomanager_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourceundomanager_id)  == TRUE  ) return ((NspGtkSourceUndoManager *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourceundomanager));
  return NULL;
}

int IsGtkSourceUndoManagerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourceundomanager_id);
}

int IsGtkSourceUndoManager(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourceundomanager_id);
}

NspGtkSourceUndoManager  *GetGtkSourceUndoManagerCopy(Stack stack, int i)
{
  if (  GetGtkSourceUndoManager(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceUndoManager  *GetGtkSourceUndoManager(Stack stack, int i)
{
  NspGtkSourceUndoManager *M;
  if (( M = nsp_gtksourceundomanager_object(NthObj(i))) == NULLGTKSOURCEUNDOMANAGER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceUndoManager *gtksourceundomanager_copy(NspGtkSourceUndoManager *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourceundomanager);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourceundomanager);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceUndoManager
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_gtk_source_undo_manager_can_undo(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_undo_manager_can_undo(GTK_SOURCE_UNDO_MANAGER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_undo_manager_can_redo(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_undo_manager_can_redo(GTK_SOURCE_UNDO_MANAGER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_undo_manager_undo(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_undo_manager_undo(GTK_SOURCE_UNDO_MANAGER(self->obj));
  return 0;
}

static int _wrap_gtk_source_undo_manager_redo(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_undo_manager_redo(GTK_SOURCE_UNDO_MANAGER(self->obj));
  return 0;
}

static int _wrap_gtk_source_undo_manager_begin_not_undoable_action(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_undo_manager_begin_not_undoable_action(GTK_SOURCE_UNDO_MANAGER(self->obj));
  return 0;
}

static int _wrap_gtk_source_undo_manager_end_not_undoable_action(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_undo_manager_end_not_undoable_action(GTK_SOURCE_UNDO_MANAGER(self->obj));
  return 0;
}

static int _wrap_gtk_source_undo_manager_can_undo_changed(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_undo_manager_can_undo_changed(GTK_SOURCE_UNDO_MANAGER(self->obj));
  return 0;
}

static int _wrap_gtk_source_undo_manager_can_redo_changed(NspGtkSourceUndoManager *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    gtk_source_undo_manager_can_redo_changed(GTK_SOURCE_UNDO_MANAGER(self->obj));
  return 0;
}

static NspMethods gtksourceundomanager_methods[] = {
  {"can_undo",(nsp_method *) _wrap_gtk_source_undo_manager_can_undo},
  {"can_redo",(nsp_method *) _wrap_gtk_source_undo_manager_can_redo},
  {"undo",(nsp_method *) _wrap_gtk_source_undo_manager_undo},
  {"redo",(nsp_method *) _wrap_gtk_source_undo_manager_redo},
  {"begin_not_undoable_action",(nsp_method *) _wrap_gtk_source_undo_manager_begin_not_undoable_action},
  {"end_not_undoable_action",(nsp_method *) _wrap_gtk_source_undo_manager_end_not_undoable_action},
  {"can_undo_changed",(nsp_method *) _wrap_gtk_source_undo_manager_can_undo_changed},
  {"can_redo_changed",(nsp_method *) _wrap_gtk_source_undo_manager_can_redo_changed},
  { NULL, NULL}
};

static NspMethods *gtksourceundomanager_get_methods(void) { return gtksourceundomanager_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourceundomanager_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceView ----------- */


#define  NspGtkSourceView_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourceview.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceView inherits from GtkTextView 
 */

int nsp_type_gtksourceview_id=0;
NspTypeGtkSourceView *nsp_type_gtksourceview=NULL;

/*
 * Type object for NspGtkSourceView 
 * all the instance of NspTypeGtkSourceView share the same id. 
 * nsp_type_gtksourceview: is an instance of NspTypeGtkSourceView 
 *    used for objects of NspGtkSourceView type (i.e built with new_gtksourceview) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceView *new_type_gtksourceview(type_mode mode)
{
  NspTypeGtkSourceView *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourceview != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourceview;
    }
  if (( type =  malloc(sizeof(NspTypeGtkTextView))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtktextview(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourceview_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourceview_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourceview;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourceview */ 

  top->s_type =  (s_type_func *) nsp_gtksourceview_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourceview_type_short_string;
  /* top->create = (create_func*) int_gtksourceview_create;*/

  /* specific methods for gtksourceview */

  type->init = (init_func *) init_gtksourceview;

  /* 
   * NspGtkSourceView interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourceview_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceView called nsp_type_gtksourceview
       */
      type->id =  nsp_type_gtksourceview_id = nsp_new_type_id();
      nsp_type_gtksourceview = type;
      if ( nsp_register_type(nsp_type_gtksourceview) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourceview, GTK_SOURCE_TYPE_VIEW);
      return ( mode == T_BASE ) ? type : new_type_gtksourceview(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourceview_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceView instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourceview(NspGtkSourceView *Obj,NspTypeGtkSourceView *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceView 
 */

NspGtkSourceView *new_gtksourceview() 
{
  NspGtkSourceView *loc;
  /* type must exists */
  nsp_type_gtksourceview = new_type_gtksourceview(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceView)))== NULLGTKSOURCEVIEW) return loc;
  /* initialize object */
  if ( init_gtksourceview(loc,nsp_type_gtksourceview) == FAIL) return NULLGTKSOURCEVIEW;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceView 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourceview_type_name[]="GtkSourceView";
static char gtksourceview_short_type_name[]="GtkSourceView";

static char *nsp_gtksourceview_type_as_string(void)
{
  return(gtksourceview_type_name);
}

static char *nsp_gtksourceview_type_short_string(NspObject *v)
{
  return(gtksourceview_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceView objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceView   *nsp_gtksourceview_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourceview_id)  == TRUE  ) return ((NspGtkSourceView *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourceview));
  return NULL;
}

int IsGtkSourceViewObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourceview_id);
}

int IsGtkSourceView(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourceview_id);
}

NspGtkSourceView  *GetGtkSourceViewCopy(Stack stack, int i)
{
  if (  GetGtkSourceView(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceView  *GetGtkSourceView(Stack stack, int i)
{
  NspGtkSourceView *M;
  if (( M = nsp_gtksourceview_object(NthObj(i))) == NULLGTKSOURCEVIEW)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceView *gtksourceview_copy(NspGtkSourceView *self)
{
  /* return gtktextview_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourceview);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourceview);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceView
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_view_new_with_buffer (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *buffer;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourcebuffer, &buffer) == FAIL) return RET_BUG;
  if ((ret = (GObject *)gtk_source_view_new_with_buffer(GTK_SOURCE_BUFFER(buffer->obj)))== NULL) return RET_BUG;

  nsp_type_gtksourceview = new_type_gtksourceview(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceview);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_gtk_source_view_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_view_new())== NULL) return RET_BUG;

  nsp_type_gtksourceview = new_type_gtksourceview(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceview);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_view_set_show_line_numbers(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int show;
  if ( GetArgs(stack,rhs,opt,T,&show) == FAIL) return RET_BUG;
    gtk_source_view_set_show_line_numbers(GTK_SOURCE_VIEW(self->obj),show);
  return 0;
}

static int _wrap_gtk_source_view_get_show_line_numbers(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_show_line_numbers(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_tab_width(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int width;
  if ( GetArgs(stack,rhs,opt,T,&width) == FAIL) return RET_BUG;
    gtk_source_view_set_tab_width(GTK_SOURCE_VIEW(self->obj),width);
  return 0;
}

static int _wrap_gtk_source_view_get_tab_width(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_tab_width(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_indent_width(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int width;
  if ( GetArgs(stack,rhs,opt,T,&width) == FAIL) return RET_BUG;
    gtk_source_view_set_indent_width(GTK_SOURCE_VIEW(self->obj),width);
  return 0;
}

static int _wrap_gtk_source_view_get_indent_width(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_indent_width(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_auto_indent(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int enable;
  if ( GetArgs(stack,rhs,opt,T,&enable) == FAIL) return RET_BUG;
    gtk_source_view_set_auto_indent(GTK_SOURCE_VIEW(self->obj),enable);
  return 0;
}

static int _wrap_gtk_source_view_get_auto_indent(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_auto_indent(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_insert_spaces_instead_of_tabs(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int enable;
  if ( GetArgs(stack,rhs,opt,T,&enable) == FAIL) return RET_BUG;
    gtk_source_view_set_insert_spaces_instead_of_tabs(GTK_SOURCE_VIEW(self->obj),enable);
  return 0;
}

static int _wrap_gtk_source_view_get_insert_spaces_instead_of_tabs(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_insert_spaces_instead_of_tabs(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_indent_on_tab(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int enable;
  if ( GetArgs(stack,rhs,opt,T,&enable) == FAIL) return RET_BUG;
    gtk_source_view_set_indent_on_tab(GTK_SOURCE_VIEW(self->obj),enable);
  return 0;
}

static int _wrap_gtk_source_view_get_indent_on_tab(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_indent_on_tab(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_indent_lines(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_view_indent_lines(GTK_SOURCE_VIEW(self->obj),start,end);
  return 0;
}

static int _wrap_gtk_source_view_unindent_lines(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj, t_end};
  GtkTextIter *start = NULL, *end = NULL;
  NspObject *nsp_start = NULL, *nsp_end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_start, &nsp_end) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_start, GTK_TYPE_TEXT_ITER))
      start = nspg_boxed_get(nsp_start, GtkTextIter);
  else {
      Scierror( "Error: start should be a GtkTextIter\n");
      return RET_BUG;
  }
  if (nspg_boxed_check(nsp_end, GTK_TYPE_TEXT_ITER))
      end = nspg_boxed_get(nsp_end, GtkTextIter);
  else {
      Scierror( "Error: end should be a GtkTextIter\n");
      return RET_BUG;
  }
    gtk_source_view_unindent_lines(GTK_SOURCE_VIEW(self->obj),start,end);
  return 0;
}

static int _wrap_gtk_source_view_set_highlight_current_line(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int highlight;
  if ( GetArgs(stack,rhs,opt,T,&highlight) == FAIL) return RET_BUG;
    gtk_source_view_set_highlight_current_line(GTK_SOURCE_VIEW(self->obj),highlight);
  return 0;
}

static int _wrap_gtk_source_view_get_highlight_current_line(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_highlight_current_line(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_show_right_margin(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int show;
  if ( GetArgs(stack,rhs,opt,T,&show) == FAIL) return RET_BUG;
    gtk_source_view_set_show_right_margin(GTK_SOURCE_VIEW(self->obj),show);
  return 0;
}

static int _wrap_gtk_source_view_get_show_right_margin(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_show_right_margin(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_right_margin_position(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int pos;
  if ( GetArgs(stack,rhs,opt,T,&pos) == FAIL) return RET_BUG;
    gtk_source_view_set_right_margin_position(GTK_SOURCE_VIEW(self->obj),pos);
  return 0;
}

static int _wrap_gtk_source_view_get_right_margin_position(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_right_margin_position(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_show_line_marks(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int show;
  if ( GetArgs(stack,rhs,opt,T,&show) == FAIL) return RET_BUG;
    gtk_source_view_set_show_line_marks(GTK_SOURCE_VIEW(self->obj),show);
  return 0;
}

static int _wrap_gtk_source_view_get_show_line_marks(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_show_line_marks(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_mark_attributes(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check,s_int, t_end};
  char *category;
  NspGObject *attributes;
  int priority;
  if ( GetArgs(stack,rhs,opt,T,&category, &nsp_type_gtksourcemarkattributes, &attributes, &priority) == FAIL) return RET_BUG;
    gtk_source_view_set_mark_attributes(GTK_SOURCE_VIEW(self->obj),category,GTK_SOURCE_MARK_ATTRIBUTES(attributes->obj),priority);
  return 0;
}

static int _wrap_gtk_source_view_get_mark_attributes(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *category;
  int priority;
  GtkSourceMarkAttributes *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&category, &priority) == FAIL) return RET_BUG;
    ret =gtk_source_view_get_mark_attributes(GTK_SOURCE_VIEW(self->obj),category,&priority);
  nsp_type_gtksourcemarkattributes = new_type_gtksourcemarkattributes(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemarkattributes))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_view_set_smart_backspace(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int smart_backspace;
  if ( GetArgs(stack,rhs,opt,T,&smart_backspace) == FAIL) return RET_BUG;
    gtk_source_view_set_smart_backspace(GTK_SOURCE_VIEW(self->obj),smart_backspace);
  return 0;
}

static int _wrap_gtk_source_view_get_smart_backspace(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_smart_backspace(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_smart_home_end(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceSmartHomeEndType smart_home_end;
  NspObject *nsp_smart_home_end = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_smart_home_end) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GTK_SOURCE_TYPE_SMART_HOME_END_TYPE, nsp_smart_home_end, &smart_home_end)== FAIL)
      return RET_BUG;
    gtk_source_view_set_smart_home_end(GTK_SOURCE_VIEW(self->obj),smart_home_end);
  return 0;
}

static int _wrap_gtk_source_view_get_smart_home_end(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_smart_home_end(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_set_draw_spaces(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceDrawSpacesFlags flags;
  NspObject *nsp_flags = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(GTK_SOURCE_TYPE_DRAW_SPACES_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    gtk_source_view_set_draw_spaces(GTK_SOURCE_VIEW(self->obj),flags);
  return 0;
}

static int _wrap_gtk_source_view_get_draw_spaces(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_draw_spaces(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_get_visual_column(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkTextIter *iter = NULL;
  NspObject *nsp_iter = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_iter) == FAIL) return RET_BUG;
  if (nspg_boxed_check(nsp_iter, GTK_TYPE_TEXT_ITER))
      iter = nspg_boxed_get(nsp_iter, GtkTextIter);
  else {
      Scierror( "Error: iter should be a GtkTextIter\n");
      return RET_BUG;
  }
    ret =gtk_source_view_get_visual_column(GTK_SOURCE_VIEW(self->obj),iter);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_gtk_source_view_get_completion(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceCompletion *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_completion(GTK_SOURCE_VIEW(self->obj));
  nsp_type_gtksourcecompletion = new_type_gtksourcecompletion(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcecompletion))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_view_set_background_pattern(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GtkSourceBackgroundPatternType background_pattern;
  NspObject *nsp_background_pattern = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_background_pattern) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(GTK_SOURCE_TYPE_BACKGROUND_PATTERN_TYPE, nsp_background_pattern, &background_pattern)== FAIL)
      return RET_BUG;
    gtk_source_view_set_background_pattern(GTK_SOURCE_VIEW(self->obj),background_pattern);
  return 0;
}

static int _wrap_gtk_source_view_get_background_pattern(NspGtkSourceView *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =gtk_source_view_get_background_pattern(GTK_SOURCE_VIEW(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtksourceview_methods[] = {
  {"set_show_line_numbers",(nsp_method *) _wrap_gtk_source_view_set_show_line_numbers},
  {"get_show_line_numbers",(nsp_method *) _wrap_gtk_source_view_get_show_line_numbers},
  {"set_tab_width",(nsp_method *) _wrap_gtk_source_view_set_tab_width},
  {"get_tab_width",(nsp_method *) _wrap_gtk_source_view_get_tab_width},
  {"set_indent_width",(nsp_method *) _wrap_gtk_source_view_set_indent_width},
  {"get_indent_width",(nsp_method *) _wrap_gtk_source_view_get_indent_width},
  {"set_auto_indent",(nsp_method *) _wrap_gtk_source_view_set_auto_indent},
  {"get_auto_indent",(nsp_method *) _wrap_gtk_source_view_get_auto_indent},
  {"set_insert_spaces_instead_of_tabs",(nsp_method *) _wrap_gtk_source_view_set_insert_spaces_instead_of_tabs},
  {"get_insert_spaces_instead_of_tabs",(nsp_method *) _wrap_gtk_source_view_get_insert_spaces_instead_of_tabs},
  {"set_indent_on_tab",(nsp_method *) _wrap_gtk_source_view_set_indent_on_tab},
  {"get_indent_on_tab",(nsp_method *) _wrap_gtk_source_view_get_indent_on_tab},
  {"indent_lines",(nsp_method *) _wrap_gtk_source_view_indent_lines},
  {"unindent_lines",(nsp_method *) _wrap_gtk_source_view_unindent_lines},
  {"set_highlight_current_line",(nsp_method *) _wrap_gtk_source_view_set_highlight_current_line},
  {"get_highlight_current_line",(nsp_method *) _wrap_gtk_source_view_get_highlight_current_line},
  {"set_show_right_margin",(nsp_method *) _wrap_gtk_source_view_set_show_right_margin},
  {"get_show_right_margin",(nsp_method *) _wrap_gtk_source_view_get_show_right_margin},
  {"set_right_margin_position",(nsp_method *) _wrap_gtk_source_view_set_right_margin_position},
  {"get_right_margin_position",(nsp_method *) _wrap_gtk_source_view_get_right_margin_position},
  {"set_show_line_marks",(nsp_method *) _wrap_gtk_source_view_set_show_line_marks},
  {"get_show_line_marks",(nsp_method *) _wrap_gtk_source_view_get_show_line_marks},
  {"set_mark_attributes",(nsp_method *) _wrap_gtk_source_view_set_mark_attributes},
  {"get_mark_attributes",(nsp_method *) _wrap_gtk_source_view_get_mark_attributes},
  {"set_smart_backspace",(nsp_method *) _wrap_gtk_source_view_set_smart_backspace},
  {"get_smart_backspace",(nsp_method *) _wrap_gtk_source_view_get_smart_backspace},
  {"set_smart_home_end",(nsp_method *) _wrap_gtk_source_view_set_smart_home_end},
  {"get_smart_home_end",(nsp_method *) _wrap_gtk_source_view_get_smart_home_end},
  {"set_draw_spaces",(nsp_method *) _wrap_gtk_source_view_set_draw_spaces},
  {"get_draw_spaces",(nsp_method *) _wrap_gtk_source_view_get_draw_spaces},
  {"get_visual_column",(nsp_method *) _wrap_gtk_source_view_get_visual_column},
  {"get_completion",(nsp_method *) _wrap_gtk_source_view_get_completion},
  {"set_background_pattern",(nsp_method *) _wrap_gtk_source_view_set_background_pattern},
  {"get_background_pattern",(nsp_method *) _wrap_gtk_source_view_get_background_pattern},
  { NULL, NULL}
};

static NspMethods *gtksourceview_get_methods(void) { return gtksourceview_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourceview_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGtkSourceMap ----------- */


#define  NspGtkSourceMap_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtksourcemap.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGtkSourceMap inherits from GtkSourceView 
 */

int nsp_type_gtksourcemap_id=0;
NspTypeGtkSourceMap *nsp_type_gtksourcemap=NULL;

/*
 * Type object for NspGtkSourceMap 
 * all the instance of NspTypeGtkSourceMap share the same id. 
 * nsp_type_gtksourcemap: is an instance of NspTypeGtkSourceMap 
 *    used for objects of NspGtkSourceMap type (i.e built with new_gtksourcemap) 
 * other instances are used for derived classes 
 */
NspTypeGtkSourceMap *new_type_gtksourcemap(type_mode mode)
{
  NspTypeGtkSourceMap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtksourcemap != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtksourcemap;
    }
  if (( type =  malloc(sizeof(NspTypeGtkSourceView))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtksourceview(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtksourcemap_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtksourcemap_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtksourcemap;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtksourcemap */ 

  top->s_type =  (s_type_func *) nsp_gtksourcemap_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtksourcemap_type_short_string;
  /* top->create = (create_func*) int_gtksourcemap_create;*/

  /* specific methods for gtksourcemap */

  type->init = (init_func *) init_gtksourcemap;

  /* 
   * NspGtkSourceMap interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtksourcemap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGtkSourceMap called nsp_type_gtksourcemap
       */
      type->id =  nsp_type_gtksourcemap_id = nsp_new_type_id();
      nsp_type_gtksourcemap = type;
      if ( nsp_register_type(nsp_type_gtksourcemap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtksourcemap, GTK_SOURCE_TYPE_MAP);
      return ( mode == T_BASE ) ? type : new_type_gtksourcemap(mode);
    }
  else 
    {
      type->id = nsp_type_gtksourcemap_id;
      return type;
    }
}

/*
 * initialize NspGtkSourceMap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtksourcemap(NspGtkSourceMap *Obj,NspTypeGtkSourceMap *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGtkSourceMap 
 */

NspGtkSourceMap *new_gtksourcemap() 
{
  NspGtkSourceMap *loc;
  /* type must exists */
  nsp_type_gtksourcemap = new_type_gtksourcemap(T_BASE);
  if ( (loc = malloc(sizeof(NspGtkSourceMap)))== NULLGTKSOURCEMAP) return loc;
  /* initialize object */
  if ( init_gtksourcemap(loc,nsp_type_gtksourcemap) == FAIL) return NULLGTKSOURCEMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGtkSourceMap 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtksourcemap_type_name[]="GtkSourceMap";
static char gtksourcemap_short_type_name[]="GtkSourceMap";

static char *nsp_gtksourcemap_type_as_string(void)
{
  return(gtksourcemap_type_name);
}

static char *nsp_gtksourcemap_type_short_string(NspObject *v)
{
  return(gtksourcemap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGtkSourceMap objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGtkSourceMap   *nsp_gtksourcemap_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtksourcemap_id)  == TRUE  ) return ((NspGtkSourceMap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtksourcemap));
  return NULL;
}

int IsGtkSourceMapObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtksourcemap_id);
}

int IsGtkSourceMap(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtksourcemap_id);
}

NspGtkSourceMap  *GetGtkSourceMapCopy(Stack stack, int i)
{
  if (  GetGtkSourceMap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGtkSourceMap  *GetGtkSourceMap(Stack stack, int i)
{
  NspGtkSourceMap *M;
  if (( M = nsp_gtksourcemap_object(NthObj(i))) == NULLGTKSOURCEMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGtkSourceMap *gtksourcemap_copy(NspGtkSourceMap *self)
{
  /* return gtksourceview_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcemap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtksourcemap);
}

/*-------------------------------------------------------------------
 * wrappers for the GtkSourceMap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_gtk_source_map_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)gtk_source_map_new())== NULL) return RET_BUG;

  nsp_type_gtksourcemap = new_type_gtksourcemap(T_BASE);
  nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcemap);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_gtk_source_map_set_view(NspGtkSourceMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *view;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtksourceview, &view) == FAIL) return RET_BUG;
    gtk_source_map_set_view(GTK_SOURCE_MAP(self->obj),GTK_SOURCE_VIEW(view->obj));
  return 0;
}

static int _wrap_gtk_source_map_get_view(NspGtkSourceMap *self,Stack stack,int rhs,int opt,int lhs)
{
  GtkSourceView *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_map_get_view(GTK_SOURCE_MAP(self->obj));
  nsp_type_gtksourceview = new_type_gtksourceview(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourceview))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gtksourcemap_methods[] = {
  {"set_view",(nsp_method *) _wrap_gtk_source_map_set_view},
  {"get_view",(nsp_method *) _wrap_gtk_source_map_get_view},
  { NULL, NULL}
};

static NspMethods *gtksourcemap_get_methods(void) { return gtksourcemap_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtksourcemap_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_gtk_source_encoding_get_all(Stack stack, int rhs, int opt, int lhs) /* gtk_source_encoding_get_all */
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gtk_source_encoding_get_all();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

int _wrap_gtk_source_encoding_get_default_candidates(Stack stack, int rhs, int opt, int lhs) /* gtk_source_encoding_get_default_candidates */
{
  GSList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =gtk_source_encoding_get_default_candidates();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_slist_free);

}

int _wrap_gtk_source_language_manager_get_default(Stack stack, int rhs, int opt, int lhs) /* gtk_source_language_manager_get_default */
{
  GtkSourceLanguageManager *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_language_manager_get_default();
  nsp_type_gtksourcelanguagemanager = new_type_gtksourcelanguagemanager(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcelanguagemanager))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gtk_source_style_scheme_manager_get_default(Stack stack, int rhs, int opt, int lhs) /* gtk_source_style_scheme_manager_get_default */
{
  GtkSourceStyleSchemeManager *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =gtk_source_style_scheme_manager_get_default();
  nsp_type_gtksourcestyleschememanager = new_type_gtksourcestyleschememanager(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new_with_possible_type(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtksourcestyleschememanager))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_gtk_source_utils_unescape_search_text(Stack stack, int rhs, int opt, int lhs) /* gtk_source_utils_unescape_search_text */
{
  int_types T[] = {string, t_end};
  char *text;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&text) == FAIL) return RET_BUG;
    ret =gtk_source_utils_unescape_search_text(text);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_gtk_source_utils_escape_search_text(Stack stack, int rhs, int opt, int lhs) /* gtk_source_utils_escape_search_text */
{
  int_types T[] = {string, t_end};
  char *text;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&text) == FAIL) return RET_BUG;
    ret =gtk_source_utils_escape_search_text(text);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gtksourceview_func[]={
  { "gtk_source_buffer_new", _wrap_gtk_source_buffer_new},
  { "gtksourcebuffer_new", _wrap_gtk_source_buffer_new},
  { "gtk_source_buffer_new_with_language", _wrap_gtk_source_buffer_new_with_language},
  { "gtk_source_completion_info_new", _wrap_gtk_source_completion_info_new},
  { "gtksourcecompletioninfo_new", _wrap_gtk_source_completion_info_new},
  { "gtk_source_completion_item_new", _wrap_gtk_source_completion_item_new},
  { "gtksourcecompletionitem_new", _wrap_gtk_source_completion_item_new},
  { "gtk_source_completion_item_new_with_markup", _wrap_gtk_source_completion_item_new_with_markup},
  { "gtk_source_file_new", _wrap_gtk_source_file_new},
  { "gtksourcefile_new", _wrap_gtk_source_file_new},
  { "gtk_source_file_loader_new", _wrap_gtk_source_file_loader_new},
  { "gtksourcefileloader_new", _wrap_gtk_source_file_loader_new},
  { "gtk_source_file_loader_new_from_stream", _wrap_gtk_source_file_loader_new_from_stream},
  { "gtk_source_file_saver_new", _wrap_gtk_source_file_saver_new},
  { "gtksourcefilesaver_new", _wrap_gtk_source_file_saver_new},
  { "gtk_source_file_saver_new_with_target", _wrap_gtk_source_file_saver_new_with_target},
  { "gtk_source_gutter_renderer_pixbuf_new", _wrap_gtk_source_gutter_renderer_pixbuf_new},
  { "gtksourcegutterrendererpixbuf_new", _wrap_gtk_source_gutter_renderer_pixbuf_new},
  { "gtk_source_gutter_renderer_text_new", _wrap_gtk_source_gutter_renderer_text_new},
  { "gtksourcegutterrenderertext_new", _wrap_gtk_source_gutter_renderer_text_new},
  { "gtk_source_language_manager_new", _wrap_gtk_source_language_manager_new},
  { "gtksourcelanguagemanager_new", _wrap_gtk_source_language_manager_new},
  { "gtk_source_mark_new", _wrap_gtk_source_mark_new},
  { "gtksourcemark_new", _wrap_gtk_source_mark_new},
  { "gtk_source_mark_attributes_new", _wrap_gtk_source_mark_attributes_new},
  { "gtksourcemarkattributes_new", _wrap_gtk_source_mark_attributes_new},
  { "gtk_source_print_compositor_new", _wrap_gtk_source_print_compositor_new},
  { "gtksourceprintcompositor_new", _wrap_gtk_source_print_compositor_new},
  { "gtk_source_print_compositor_new_from_view", _wrap_gtk_source_print_compositor_new_from_view},
  { "gtk_source_search_context_new", _wrap_gtk_source_search_context_new},
  { "gtksourcesearchcontext_new", _wrap_gtk_source_search_context_new},
  { "gtk_source_search_settings_new", _wrap_gtk_source_search_settings_new},
  { "gtksourcesearchsettings_new", _wrap_gtk_source_search_settings_new},
  { "gtk_source_style_scheme_chooser_button_new", _wrap_gtk_source_style_scheme_chooser_button_new},
  { "gtksourcestyleschemechooserbutton_new", _wrap_gtk_source_style_scheme_chooser_button_new},
  { "gtk_source_style_scheme_chooser_widget_new", _wrap_gtk_source_style_scheme_chooser_widget_new},
  { "gtksourcestyleschemechooserwidget_new", _wrap_gtk_source_style_scheme_chooser_widget_new},
  { "gtk_source_style_scheme_manager_new", _wrap_gtk_source_style_scheme_manager_new},
  { "gtksourcestyleschememanager_new", _wrap_gtk_source_style_scheme_manager_new},
  { "gtk_source_view_new", _wrap_gtk_source_view_new},
  { "gtksourceview_new", _wrap_gtk_source_view_new},
  { "gtk_source_view_new_with_buffer", _wrap_gtk_source_view_new_with_buffer},
  { "gtk_source_map_new", _wrap_gtk_source_map_new},
  { "gtksourcemap_new", _wrap_gtk_source_map_new},
  { "gtk_source_encoding_get_all", _wrap_gtk_source_encoding_get_all},
  { "gtk_source_encoding_get_default_candidates", _wrap_gtk_source_encoding_get_default_candidates},
  { "gtk_source_language_manager_get_default", _wrap_gtk_source_language_manager_get_default},
  { "gtk_source_style_scheme_manager_get_default", _wrap_gtk_source_style_scheme_manager_get_default},
  { "gtk_source_utils_unescape_search_text", _wrap_gtk_source_utils_unescape_search_text},
  { "gtk_source_utils_escape_search_text", _wrap_gtk_source_utils_escape_search_text},
  { NULL, NULL}
};

/* call ith function in the gtksourceview interface */

int gtksourceview_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,gtksourceview_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(gtksourceview_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gtksourceview_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = gtksourceview_func[i].name;
  *f = gtksourceview_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
gtksourceview_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_BRACKET_MATCH_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_CHANGE_CASE_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GTK_SOURCE_TYPE_SORT_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_COMPLETION_ERROR, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GTK_SOURCE_TYPE_COMPLETION_ACTIVATION, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_FILE_LOADER_ERROR, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_FILE_SAVER_ERROR, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GTK_SOURCE_TYPE_FILE_SAVER_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GTK_SOURCE_TYPE_GUTTER_RENDERER_STATE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_GUTTER_RENDERER_ALIGNMENT_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_NEWLINE_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_COMPRESSION_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_BACKGROUND_PATTERN_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_VIEW_GUTTER_POSITION, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, GTK_SOURCE_TYPE_SMART_HOME_END_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, GTK_SOURCE_TYPE_DRAW_SPACES_FLAGS, strip_prefix);
}

void nsp_initialize_gtksourceview_types(void)
{
  new_type_gtksourcebuffer(T_BASE);
  new_type_gtksourcecompletion(T_BASE);
  new_type_gtksourcecompletioncontext(T_BASE);
  new_type_gtksourcecompletioninfo(T_BASE);
  new_type_gtksourcecompletionitem(T_BASE);
  new_type_gtksourcecompletionproposal(T_BASE);
  new_type_gtksourcecompletionprovider(T_BASE);
  new_type_gtksourcefile(T_BASE);
  new_type_gtksourcefileloader(T_BASE);
  new_type_gtksourcefilesaver(T_BASE);
  new_type_gtksourcegutter(T_BASE);
  new_type_gtksourcegutterrenderer(T_BASE);
  new_type_gtksourcegutterrendererpixbuf(T_BASE);
  new_type_gtksourcegutterrenderertext(T_BASE);
  new_type_gtksourcelanguage(T_BASE);
  new_type_gtksourcelanguagemanager(T_BASE);
  new_type_gtksourcemark(T_BASE);
  new_type_gtksourcemarkattributes(T_BASE);
  new_type_gtksourceprintcompositor(T_BASE);
  new_type_gtksourcesearchcontext(T_BASE);
  new_type_gtksourcesearchsettings(T_BASE);
  new_type_gtksourcestylescheme(T_BASE);
  new_type_gtksourcestyleschemechooserbutton(T_BASE);
  new_type_gtksourcestyleschemechooserwidget(T_BASE);
  new_type_gtksourcestyleschememanager(T_BASE);
  new_type_gtksourceundomanager(T_BASE);
  new_type_gtksourceview(T_BASE);
  new_type_gtksourcemap(T_BASE);
}

#line 9718 "gtksourceview.c"
