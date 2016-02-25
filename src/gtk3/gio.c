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





#line 4 "codegen-3.0/gio.override"
#include <gio/gio.h>
#include <nsp/nsp.h>
#include <nsp/smatrix.h>
#include <nsp/bmatrix.h>
#include <nsp/hash.h>
#include <nsp/plist.h>
#include <nsp/list.h>
#include <nsp/cells.h>
#include <nsp/none.h>
#include <nsp/mpmatrix.h>
#include <nsp/matrix.h>
#include <nsp/file.h>
#include <nsp/type.h>
#include <nsp/hobj.h>
#include <nsp/interf.h>
#include <nsp/gvariant.h>
#include <nsp/gvarianttype.h>
#include <nsp/gtk/gboxed.h>
#include <nsp/gtk/gobject.h>
#include <nsp/gtk/gobject-util.h>

#line 50 "gio.c"
/* ---------- forward type declarations ---------- */
#include <nsp/gtk/gappinfo.h>
#include <nsp/gtk/gapplaunchcontext.h>
#include <nsp/gtk/gapplication.h>
#include <nsp/gtk/gapplicationcommandline.h>
#include <nsp/gtk/gasyncinitable.h>
#include <nsp/gtk/gasyncresult.h>
#include <nsp/gtk/gcancellable.h>
#include <nsp/gtk/gconverter.h>
#include <nsp/gtk/gdbusinterfaceskeleton.h>
#include <nsp/gtk/gdbusobjectmanagerclient.h>
#include <nsp/gtk/gdbusobjectmanagerserver.h>
#include <nsp/gtk/gdbusobjectproxy.h>
#include <nsp/gtk/gdbusobjectskeleton.h>
#include <nsp/gtk/gdbusproxy.h>
#include <nsp/gtk/gdrive.h>
#include <nsp/gtk/gemblemedicon.h>
#include <nsp/gtk/gfile.h>
#include <nsp/gtk/gfileenumerator.h>
#include <nsp/gtk/gfilemonitor.h>
#include <nsp/gtk/giostream.h>
#include <nsp/gtk/gfileiostream.h>
#include <nsp/gtk/gicon.h>
#include <nsp/gtk/ginetaddress.h>
#include <nsp/gtk/ginetaddressmask.h>
#include <nsp/gtk/ginitable.h>
#include <nsp/gtk/ginputstream.h>
#include <nsp/gtk/gfilterinputstream.h>
#include <nsp/gtk/gbufferedinputstream.h>
#include <nsp/gtk/gdatainputstream.h>
#include <nsp/gtk/gconverterinputstream.h>
#include <nsp/gtk/gfileinputstream.h>
#include <nsp/gtk/gloadableicon.h>
#include <nsp/gtk/gmemoryinputstream.h>
#include <nsp/gtk/gmenuattributeiter.h>
#include <nsp/gtk/gmenulinkiter.h>
#include <nsp/gtk/gmenumodel.h>
#include <nsp/gtk/gmenu.h>
#include <nsp/gtk/gmount.h>
#include <nsp/gtk/gmountoperation.h>
#include <nsp/gtk/gnetworkaddress.h>
#include <nsp/gtk/gnetworkservice.h>
#include <nsp/gtk/goutputstream.h>
#include <nsp/gtk/gmemoryoutputstream.h>
#include <nsp/gtk/gfilteroutputstream.h>
#include <nsp/gtk/gbufferedoutputstream.h>
#include <nsp/gtk/gconverteroutputstream.h>
#include <nsp/gtk/gdataoutputstream.h>
#include <nsp/gtk/gfileoutputstream.h>
#include <nsp/gtk/gpermission.h>
#include <nsp/gtk/gresolver.h>
#include <nsp/gtk/gseekable.h>
#include <nsp/gtk/gsettings.h>
#include <nsp/gtk/gsimpleaction.h>
#include <nsp/gtk/gsimpleactiongroup.h>
#include <nsp/gtk/gsimpleproxyresolver.h>
#include <nsp/gtk/gsocket.h>
#include <nsp/gtk/gsocketaddress.h>
#include <nsp/gtk/ginetsocketaddress.h>
#include <nsp/gtk/gproxyaddress.h>
#include <nsp/gtk/gsocketaddressenumerator.h>
#include <nsp/gtk/gproxyaddressenumerator.h>
#include <nsp/gtk/gsocketclient.h>
#include <nsp/gtk/gsocketconnectable.h>
#include <nsp/gtk/gsocketconnection.h>
#include <nsp/gtk/gsocketcontrolmessage.h>
#include <nsp/gtk/gsocketlistener.h>
#include <nsp/gtk/gsocketservice.h>
#include <nsp/gtk/gtcpconnection.h>
#include <nsp/gtk/gtcpwrapperconnection.h>
#include <nsp/gtk/gthreadedsocketservice.h>
#include <nsp/gtk/gtlscertificate.h>
#include <nsp/gtk/gtlsconnection.h>
#include <nsp/gtk/gtlsdatabase.h>
#include <nsp/gtk/gtlsinteraction.h>
#include <nsp/gtk/gtlspassword.h>
#include <nsp/gtk/gvfs.h>
#include <nsp/gtk/gvolume.h>
#include <nsp/gtk/gvolumemonitor.h>
#include <nsp/gtk/gnativevolumemonitor.h>
#include <nsp/gtk/gdbusinterface.h>
#include <nsp/gtk/gdbusobject.h>
#include <nsp/gtk/gaction.h>
#include <nsp/gtk/gactiongroup.h>
#include <nsp/gtk/gactionmap.h>


/* -----------NspGDBusInterface ----------- */


#define  NspGDBusInterface_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusinterface.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusInterface inherits from GObject 
 */

int nsp_type_gdbusinterface_id=0;
NspTypeGDBusInterface *nsp_type_gdbusinterface=NULL;

/*
 * Type object for NspGDBusInterface 
 * all the instance of NspTypeGDBusInterface share the same id. 
 * nsp_type_gdbusinterface: is an instance of NspTypeGDBusInterface 
 *    used for objects of NspGDBusInterface type (i.e built with new_gdbusinterface) 
 * other instances are used for derived classes 
 */
NspTypeGDBusInterface *new_type_gdbusinterface(type_mode mode)
{
  NspTypeGDBusInterface *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusinterface != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusinterface;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusinterface_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusinterface_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusinterface;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusinterface */ 

  top->s_type =  (s_type_func *) nsp_gdbusinterface_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusinterface_type_short_string;
  /* top->create = (create_func*) int_gdbusinterface_create;*/

  /* specific methods for gdbusinterface */

  type->init = (init_func *) init_gdbusinterface;

  /* 
   * NspGDBusInterface interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusinterface_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusInterface called nsp_type_gdbusinterface
       */
      type->id =  nsp_type_gdbusinterface_id = nsp_new_type_id();
      nsp_type_gdbusinterface = type;
      if ( nsp_register_type(nsp_type_gdbusinterface) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusinterface, G_TYPE_DBUS_INTERFACE);
      return ( mode == T_BASE ) ? type : new_type_gdbusinterface(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusinterface_id;
      return type;
    }
}

/*
 * initialize NspGDBusInterface instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusinterface(NspGDBusInterface *Obj,NspTypeGDBusInterface *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusInterface 
 */

NspGDBusInterface *new_gdbusinterface() 
{
  NspGDBusInterface *loc;
  /* type must exists */
  nsp_type_gdbusinterface = new_type_gdbusinterface(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusInterface)))== NULLGDBUSINTERFACE) return loc;
  /* initialize object */
  if ( init_gdbusinterface(loc,nsp_type_gdbusinterface) == FAIL) return NULLGDBUSINTERFACE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusInterface 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusinterface_type_name[]="GDBusInterface";
static char gdbusinterface_short_type_name[]="GDBusInterface";

static char *nsp_gdbusinterface_type_as_string(void)
{
  return(gdbusinterface_type_name);
}

static char *nsp_gdbusinterface_type_short_string(NspObject *v)
{
  return(gdbusinterface_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusInterface objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusInterface   *nsp_gdbusinterface_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_gdbusinterface_id)   ) return ((NspGDBusInterface *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusinterface));
  return NULL;
}

int IsGDBusInterfaceObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_gdbusinterface_id);
}

int IsGDBusInterface(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_gdbusinterface_id);
}

NspGDBusInterface  *GetGDBusInterfaceCopy(Stack stack, int i)
{
  if (  GetGDBusInterface(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusInterface  *GetGDBusInterface(Stack stack, int i)
{
  NspGDBusInterface *M;
  if (( M = nsp_gdbusinterface_object(NthObj(i))) == NULLGDBUSINTERFACE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusInterface *gdbusinterface_copy(NspGDBusInterface *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusinterface);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusinterface);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusInterface
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_interface_get_object(NspGDBusInterface *self,Stack stack,int rhs,int opt,int lhs)
{
  GDBusObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_dbus_interface_get_object(G_DBUS_INTERFACE(self->obj));
  nsp_type_gdbusobject = new_type_gdbusobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdbusobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_dbus_interface_set_object(NspGDBusInterface *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *object;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdbusobject, &object) == FAIL) return RET_BUG;
    g_dbus_interface_set_object(G_DBUS_INTERFACE(self->obj),G_DBUS_OBJECT(object->obj));
  return 0;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_dbus_interface_dup_object(NspGDBusInterface *self,Stack stack,int rhs,int opt,int lhs)
{
  GDBusObject *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_dbus_interface_dup_object(G_DBUS_INTERFACE(self->obj));
  nsp_type_gdbusobject = new_type_gdbusobject(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdbusobject))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_dbus_interface_dup_object(Stack stack, int rhs, int opt, int lhs) /* dup_object */
{
  Scierror("Error: function g_dbus_interface_dup_object not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdbusinterface_methods[] = {
  {"get_object",(nsp_method *) _wrap_g_dbus_interface_get_object},
  {"set_object",(nsp_method *) _wrap_g_dbus_interface_set_object},
  {"dup_object",(nsp_method *) _wrap_g_dbus_interface_dup_object},
  { NULL, NULL}
};

static NspMethods *gdbusinterface_get_methods(void) { return gdbusinterface_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusinterface_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusObject ----------- */


#define  NspGDBusObject_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusobject.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusObject inherits from GObject 
 */

int nsp_type_gdbusobject_id=0;
NspTypeGDBusObject *nsp_type_gdbusobject=NULL;

/*
 * Type object for NspGDBusObject 
 * all the instance of NspTypeGDBusObject share the same id. 
 * nsp_type_gdbusobject: is an instance of NspTypeGDBusObject 
 *    used for objects of NspGDBusObject type (i.e built with new_gdbusobject) 
 * other instances are used for derived classes 
 */
NspTypeGDBusObject *new_type_gdbusobject(type_mode mode)
{
  NspTypeGDBusObject *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusobject != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusobject;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusobject_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusobject_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusobject;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusobject */ 

  top->s_type =  (s_type_func *) nsp_gdbusobject_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusobject_type_short_string;
  /* top->create = (create_func*) int_gdbusobject_create;*/

  /* specific methods for gdbusobject */

  type->init = (init_func *) init_gdbusobject;

  /* 
   * NspGDBusObject interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusobject_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusObject called nsp_type_gdbusobject
       */
      type->id =  nsp_type_gdbusobject_id = nsp_new_type_id();
      nsp_type_gdbusobject = type;
      if ( nsp_register_type(nsp_type_gdbusobject) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusobject, G_TYPE_DBUS_OBJECT);
      return ( mode == T_BASE ) ? type : new_type_gdbusobject(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusobject_id;
      return type;
    }
}

/*
 * initialize NspGDBusObject instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusobject(NspGDBusObject *Obj,NspTypeGDBusObject *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusObject 
 */

NspGDBusObject *new_gdbusobject() 
{
  NspGDBusObject *loc;
  /* type must exists */
  nsp_type_gdbusobject = new_type_gdbusobject(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusObject)))== NULLGDBUSOBJECT) return loc;
  /* initialize object */
  if ( init_gdbusobject(loc,nsp_type_gdbusobject) == FAIL) return NULLGDBUSOBJECT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusObject 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusobject_type_name[]="GDBusObject";
static char gdbusobject_short_type_name[]="GDBusObject";

static char *nsp_gdbusobject_type_as_string(void)
{
  return(gdbusobject_type_name);
}

static char *nsp_gdbusobject_type_short_string(NspObject *v)
{
  return(gdbusobject_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusObject objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusObject   *nsp_gdbusobject_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_gdbusobject_id)   ) return ((NspGDBusObject *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusobject));
  return NULL;
}

int IsGDBusObjectObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_gdbusobject_id);
}

int IsGDBusObject(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_gdbusobject_id);
}

NspGDBusObject  *GetGDBusObjectCopy(Stack stack, int i)
{
  if (  GetGDBusObject(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusObject  *GetGDBusObject(Stack stack, int i)
{
  NspGDBusObject *M;
  if (( M = nsp_gdbusobject_object(NthObj(i))) == NULLGDBUSOBJECT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusObject *gdbusobject_copy(NspGDBusObject *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobject);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobject);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusObject
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_object_get_object_path(NspGDBusObject *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_object_get_object_path(G_DBUS_OBJECT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_object_get_interfaces(NspGDBusObject *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_dbus_object_get_interfaces(G_DBUS_OBJECT(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_dbus_object_get_interface(NspGDBusObject *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *interface_name;
  GDBusInterface *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&interface_name) == FAIL) return RET_BUG;
    ret =g_dbus_object_get_interface(G_DBUS_OBJECT(self->obj),interface_name);
  nsp_type_gdbusinterface = new_type_gdbusinterface(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdbusinterface))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdbusobject_methods[] = {
  {"get_object_path",(nsp_method *) _wrap_g_dbus_object_get_object_path},
  {"get_interfaces",(nsp_method *) _wrap_g_dbus_object_get_interfaces},
  {"get_interface",(nsp_method *) _wrap_g_dbus_object_get_interface},
  { NULL, NULL}
};

static NspMethods *gdbusobject_get_methods(void) { return gdbusobject_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusobject_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGAction ----------- */


#define  NspGAction_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gaction.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGAction inherits from GObject 
 */

int nsp_type_gaction_id=0;
NspTypeGAction *nsp_type_gaction=NULL;

/*
 * Type object for NspGAction 
 * all the instance of NspTypeGAction share the same id. 
 * nsp_type_gaction: is an instance of NspTypeGAction 
 *    used for objects of NspGAction type (i.e built with new_gaction) 
 * other instances are used for derived classes 
 */
NspTypeGAction *new_type_gaction(type_mode mode)
{
  NspTypeGAction *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gaction != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gaction;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gaction_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gaction_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gaction;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gaction */ 

  top->s_type =  (s_type_func *) nsp_gaction_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gaction_type_short_string;
  /* top->create = (create_func*) int_gaction_create;*/

  /* specific methods for gaction */

  type->init = (init_func *) init_gaction;

  /* 
   * NspGAction interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gaction_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGAction called nsp_type_gaction
       */
      type->id =  nsp_type_gaction_id = nsp_new_type_id();
      nsp_type_gaction = type;
      if ( nsp_register_type(nsp_type_gaction) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gaction, G_TYPE_ACTION);
      return ( mode == T_BASE ) ? type : new_type_gaction(mode);
    }
  else 
    {
      type->id = nsp_type_gaction_id;
      return type;
    }
}

/*
 * initialize NspGAction instances 
 * locally and by calling initializer on parent class 
 */

static int init_gaction(NspGAction *Obj,NspTypeGAction *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGAction 
 */

NspGAction *new_gaction() 
{
  NspGAction *loc;
  /* type must exists */
  nsp_type_gaction = new_type_gaction(T_BASE);
  if ( (loc = malloc(sizeof(NspGAction)))== NULLGACTION) return loc;
  /* initialize object */
  if ( init_gaction(loc,nsp_type_gaction) == FAIL) return NULLGACTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGAction 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gaction_type_name[]="GAction";
static char gaction_short_type_name[]="GAction";

static char *nsp_gaction_type_as_string(void)
{
  return(gaction_type_name);
}

static char *nsp_gaction_type_short_string(NspObject *v)
{
  return(gaction_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGAction objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGAction   *nsp_gaction_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_gaction_id)   ) return ((NspGAction *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gaction));
  return NULL;
}

int IsGActionObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_gaction_id);
}

int IsGAction(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_gaction_id);
}

NspGAction  *GetGActionCopy(Stack stack, int i)
{
  if (  GetGAction(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGAction  *GetGAction(Stack stack, int i)
{
  NspGAction *M;
  if (( M = nsp_gaction_object(NthObj(i))) == NULLGACTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGAction *gaction_copy(NspGAction *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gaction);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gaction);
}

/*-------------------------------------------------------------------
 * wrappers for the GAction
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_action_get_name(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_action_get_name(G_ACTION(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_action_get_parameter_type(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariantType *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = (GVariantType *) g_action_get_parameter_type(G_ACTION(self->obj));
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_get_state_type(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariantType *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret = (GVariantType *) g_action_get_state_type(G_ACTION(self->obj));
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_get_state_hint(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariant *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_action_get_state_hint(G_ACTION(self->obj));
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_get_enabled(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_action_get_enabled(G_ACTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_action_get_state(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariant *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_action_get_state(G_ACTION(self->obj));
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_change_state(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GVariant *value = NULL;
  NspObject *nsp_value = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_value))
    { value = ((NspGVariant *) nsp_value)->obj->value;
      if((value = nsp_copy_GVariant(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type GVariant\n");
      return RET_BUG;
    }
    g_action_change_state(G_ACTION(self->obj),value);
  return 0;
}

static int _wrap_g_action_activate(NspGAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {new_opts, t_end};
  nsp_option opts[] = {
	{"parameter",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  GVariant *parameter = NULL;
  NspObject *nsp_parameter = NULL;
  if ( GetArgs(stack,rhs,opt,T,opts, &nsp_parameter) == FAIL) return RET_BUG;
  if (nsp_parameter != NULL)
     {
      if( IsGVariant(nsp_parameter))
        { parameter = ((NspGVariant *) nsp_parameter)->obj->value;}
      else
        {Scierror("Error: parameter should be of type GVariant\n");
         return RET_BUG;
        }
      if((parameter = nsp_copy_GVariant(parameter))==NULL) return RET_BUG;
     }
    g_action_activate(G_ACTION(self->obj),parameter);
  return 0;
}

static NspMethods gaction_methods[] = {
  {"get_name",(nsp_method *) _wrap_g_action_get_name},
  {"get_parameter_type",(nsp_method *) _wrap_g_action_get_parameter_type},
  {"get_state_type",(nsp_method *) _wrap_g_action_get_state_type},
  {"get_state_hint",(nsp_method *) _wrap_g_action_get_state_hint},
  {"get_enabled",(nsp_method *) _wrap_g_action_get_enabled},
  {"get_state",(nsp_method *) _wrap_g_action_get_state},
  {"change_state",(nsp_method *) _wrap_g_action_change_state},
  {"activate",(nsp_method *) _wrap_g_action_activate},
  { NULL, NULL}
};

static NspMethods *gaction_get_methods(void) { return gaction_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gaction_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGActionGroup ----------- */


#define  NspGActionGroup_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gactiongroup.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGActionGroup inherits from GObject 
 */

int nsp_type_gactiongroup_id=0;
NspTypeGActionGroup *nsp_type_gactiongroup=NULL;

/*
 * Type object for NspGActionGroup 
 * all the instance of NspTypeGActionGroup share the same id. 
 * nsp_type_gactiongroup: is an instance of NspTypeGActionGroup 
 *    used for objects of NspGActionGroup type (i.e built with new_gactiongroup) 
 * other instances are used for derived classes 
 */
NspTypeGActionGroup *new_type_gactiongroup(type_mode mode)
{
  NspTypeGActionGroup *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gactiongroup != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gactiongroup;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gactiongroup_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gactiongroup_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gactiongroup;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gactiongroup */ 

  top->s_type =  (s_type_func *) nsp_gactiongroup_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gactiongroup_type_short_string;
  /* top->create = (create_func*) int_gactiongroup_create;*/

  /* specific methods for gactiongroup */

  type->init = (init_func *) init_gactiongroup;

  /* 
   * NspGActionGroup interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gactiongroup_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGActionGroup called nsp_type_gactiongroup
       */
      type->id =  nsp_type_gactiongroup_id = nsp_new_type_id();
      nsp_type_gactiongroup = type;
      if ( nsp_register_type(nsp_type_gactiongroup) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gactiongroup, G_TYPE_ACTION_GROUP);
      return ( mode == T_BASE ) ? type : new_type_gactiongroup(mode);
    }
  else 
    {
      type->id = nsp_type_gactiongroup_id;
      return type;
    }
}

/*
 * initialize NspGActionGroup instances 
 * locally and by calling initializer on parent class 
 */

static int init_gactiongroup(NspGActionGroup *Obj,NspTypeGActionGroup *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGActionGroup 
 */

NspGActionGroup *new_gactiongroup() 
{
  NspGActionGroup *loc;
  /* type must exists */
  nsp_type_gactiongroup = new_type_gactiongroup(T_BASE);
  if ( (loc = malloc(sizeof(NspGActionGroup)))== NULLGACTIONGROUP) return loc;
  /* initialize object */
  if ( init_gactiongroup(loc,nsp_type_gactiongroup) == FAIL) return NULLGACTIONGROUP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGActionGroup 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gactiongroup_type_name[]="GActionGroup";
static char gactiongroup_short_type_name[]="GActionGroup";

static char *nsp_gactiongroup_type_as_string(void)
{
  return(gactiongroup_type_name);
}

static char *nsp_gactiongroup_type_short_string(NspObject *v)
{
  return(gactiongroup_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGActionGroup objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGActionGroup   *nsp_gactiongroup_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_gactiongroup_id)   ) return ((NspGActionGroup *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gactiongroup));
  return NULL;
}

int IsGActionGroupObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_gactiongroup_id);
}

int IsGActionGroup(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_gactiongroup_id);
}

NspGActionGroup  *GetGActionGroupCopy(Stack stack, int i)
{
  if (  GetGActionGroup(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGActionGroup  *GetGActionGroup(Stack stack, int i)
{
  NspGActionGroup *M;
  if (( M = nsp_gactiongroup_object(NthObj(i))) == NULLGACTIONGROUP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGActionGroup *gactiongroup_copy(NspGActionGroup *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gactiongroup);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gactiongroup);
}

/*-------------------------------------------------------------------
 * wrappers for the GActionGroup
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_action_group_has_action(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    ret =g_action_group_has_action(G_ACTION_GROUP(self->obj),action_name);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_action_group_list_actions(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_action_group_list_actions(G_ACTION_GROUP(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_group_get_action_parameter_type(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  GVariantType *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
  ret =(GVariantType*) g_action_group_get_action_parameter_type(G_ACTION_GROUP(self->obj),action_name);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_group_get_action_state_type(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  GVariantType *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
  ret =(GVariantType*) g_action_group_get_action_state_type(G_ACTION_GROUP(self->obj),action_name);
  nsp_type_gvarianttype= new_type_gvarianttype(T_BASE);
  if((ret = nsp_copy_GVariantType(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvarianttype_create(NVOID,ret,(NspTypeBase *) nsp_type_gvarianttype);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_group_get_action_state_hint(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    ret =g_action_group_get_action_state_hint(G_ACTION_GROUP(self->obj),action_name);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_group_get_action_enabled(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    ret =g_action_group_get_action_enabled(G_ACTION_GROUP(self->obj),action_name);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_action_group_get_action_state(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    ret =g_action_group_get_action_state(G_ACTION_GROUP(self->obj),action_name);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_action_group_change_action_state(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj, t_end};
  char *action_name;
  GVariant *value = NULL;
  NspObject *nsp_value = NULL;
  if ( GetArgs(stack,rhs,opt,T,&action_name, &nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_value))
    { value = ((NspGVariant *) nsp_value)->obj->value;
      if((value = nsp_copy_GVariant(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type GVariant\n");
      return RET_BUG;
    }
    g_action_group_change_action_state(G_ACTION_GROUP(self->obj),action_name,value);
  return 0;
}

static int _wrap_g_action_group_activate_action(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,new_opts, t_end};
  nsp_option opts[] = {
	{"parameter",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  char *action_name;
  GVariant *parameter = NULL;
  NspObject *nsp_parameter = NULL;
  if ( GetArgs(stack,rhs,opt,T,&action_name, opts, &nsp_parameter) == FAIL) return RET_BUG;
  if (nsp_parameter != NULL)
     {
      if( IsGVariant(nsp_parameter))
        { parameter = ((NspGVariant *) nsp_parameter)->obj->value;}
      else
        {Scierror("Error: parameter should be of type GVariant\n");
         return RET_BUG;
        }
      if((parameter = nsp_copy_GVariant(parameter))==NULL) return RET_BUG;
     }
    g_action_group_activate_action(G_ACTION_GROUP(self->obj),action_name,parameter);
  return 0;
}

static int _wrap_g_action_group_action_added(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    g_action_group_action_added(G_ACTION_GROUP(self->obj),action_name);
  return 0;
}

static int _wrap_g_action_group_action_removed(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    g_action_group_action_removed(G_ACTION_GROUP(self->obj),action_name);
  return 0;
}

static int _wrap_g_action_group_action_enabled_changed(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_bool, t_end};
  char *action_name;
  int enabled;
  if ( GetArgs(stack,rhs,opt,T,&action_name, &enabled) == FAIL) return RET_BUG;
    g_action_group_action_enabled_changed(G_ACTION_GROUP(self->obj),action_name,enabled);
  return 0;
}

static int _wrap_g_action_group_action_state_changed(NspGActionGroup *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj, t_end};
  char *action_name;
  GVariant *state = NULL;
  NspObject *nsp_state = NULL;
  if ( GetArgs(stack,rhs,opt,T,&action_name, &nsp_state) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_state))
    { state = ((NspGVariant *) nsp_state)->obj->value;
      if((state = nsp_copy_GVariant(state))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: state should be of type GVariant\n");
      return RET_BUG;
    }
    g_action_group_action_state_changed(G_ACTION_GROUP(self->obj),action_name,state);
  return 0;
}

static NspMethods gactiongroup_methods[] = {
  {"has_action",(nsp_method *) _wrap_g_action_group_has_action},
  {"list_actions",(nsp_method *) _wrap_g_action_group_list_actions},
  {"get_action_parameter_type",(nsp_method *) _wrap_g_action_group_get_action_parameter_type},
  {"get_action_state_type",(nsp_method *) _wrap_g_action_group_get_action_state_type},
  {"get_action_state_hint",(nsp_method *) _wrap_g_action_group_get_action_state_hint},
  {"get_action_enabled",(nsp_method *) _wrap_g_action_group_get_action_enabled},
  {"get_action_state",(nsp_method *) _wrap_g_action_group_get_action_state},
  {"change_action_state",(nsp_method *) _wrap_g_action_group_change_action_state},
  {"activate_action",(nsp_method *) _wrap_g_action_group_activate_action},
  {"action_added",(nsp_method *) _wrap_g_action_group_action_added},
  {"action_removed",(nsp_method *) _wrap_g_action_group_action_removed},
  {"action_enabled_changed",(nsp_method *) _wrap_g_action_group_action_enabled_changed},
  {"action_state_changed",(nsp_method *) _wrap_g_action_group_action_state_changed},
  { NULL, NULL}
};

static NspMethods *gactiongroup_get_methods(void) { return gactiongroup_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gactiongroup_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGActionMap ----------- */


#define  NspGActionMap_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gactionmap.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGActionMap inherits from GObject 
 */

int nsp_type_gactionmap_id=0;
NspTypeGActionMap *nsp_type_gactionmap=NULL;

/*
 * Type object for NspGActionMap 
 * all the instance of NspTypeGActionMap share the same id. 
 * nsp_type_gactionmap: is an instance of NspTypeGActionMap 
 *    used for objects of NspGActionMap type (i.e built with new_gactionmap) 
 * other instances are used for derived classes 
 */
NspTypeGActionMap *new_type_gactionmap(type_mode mode)
{
  NspTypeGActionMap *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gactionmap != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gactionmap;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gactionmap_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gactionmap_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gactionmap;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gactionmap */ 

  top->s_type =  (s_type_func *) nsp_gactionmap_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gactionmap_type_short_string;
  /* top->create = (create_func*) int_gactionmap_create;*/

  /* specific methods for gactionmap */

  type->init = (init_func *) init_gactionmap;

  /* 
   * NspGActionMap interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gactionmap_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGActionMap called nsp_type_gactionmap
       */
      type->id =  nsp_type_gactionmap_id = nsp_new_type_id();
      nsp_type_gactionmap = type;
      if ( nsp_register_type(nsp_type_gactionmap) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gactionmap, G_TYPE_ACTION_MAP);
      return ( mode == T_BASE ) ? type : new_type_gactionmap(mode);
    }
  else 
    {
      type->id = nsp_type_gactionmap_id;
      return type;
    }
}

/*
 * initialize NspGActionMap instances 
 * locally and by calling initializer on parent class 
 */

static int init_gactionmap(NspGActionMap *Obj,NspTypeGActionMap *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGActionMap 
 */

NspGActionMap *new_gactionmap() 
{
  NspGActionMap *loc;
  /* type must exists */
  nsp_type_gactionmap = new_type_gactionmap(T_BASE);
  if ( (loc = malloc(sizeof(NspGActionMap)))== NULLGACTIONMAP) return loc;
  /* initialize object */
  if ( init_gactionmap(loc,nsp_type_gactionmap) == FAIL) return NULLGACTIONMAP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGActionMap 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gactionmap_type_name[]="GActionMap";
static char gactionmap_short_type_name[]="GActionMap";

static char *nsp_gactionmap_type_as_string(void)
{
  return(gactionmap_type_name);
}

static char *nsp_gactionmap_type_short_string(NspObject *v)
{
  return(gactionmap_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGActionMap objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGActionMap   *nsp_gactionmap_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_implements (O,nsp_type_gactionmap_id)   ) return ((NspGActionMap *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gactionmap));
  return NULL;
}

int IsGActionMapObj(Stack stack, int i)
{
  return nsp_object_implements(NthObj(i),nsp_type_gactionmap_id);
}

int IsGActionMap(NspObject *O)
{
  return nsp_object_implements(O,nsp_type_gactionmap_id);
}

NspGActionMap  *GetGActionMapCopy(Stack stack, int i)
{
  if (  GetGActionMap(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGActionMap  *GetGActionMap(Stack stack, int i)
{
  NspGActionMap *M;
  if (( M = nsp_gactionmap_object(NthObj(i))) == NULLGACTIONMAP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGActionMap *gactionmap_copy(NspGActionMap *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gactionmap);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gactionmap);
}

/*-------------------------------------------------------------------
 * wrappers for the GActionMap
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#line 132 "codegen-3.0/gio.override"

/* XXX we want to return the most specialized type
 * i.e use nspgobject_new and not gobject_create
 * XXX this should be the generic rule for all methods ?
 */

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_action_map_lookup_action(NspGActionMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  GAction *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    ret =g_action_map_lookup_action(G_ACTION_MAP(self->obj),action_name);
  nsp_type_gaction = new_type_gaction(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *)ret))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_action_map_lookup_action(Stack stack, int rhs, int opt, int lhs) /* lookup_action */
{
  Scierror("Error: function g_action_map_lookup_action not available\n");
  return RET_BUG;
}
#endif

#line 1579 "gio.c"


#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_action_map_add_action(NspGActionMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *action;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gaction, &action) == FAIL) return RET_BUG;
    g_action_map_add_action(G_ACTION_MAP(self->obj),G_ACTION(action->obj));
  return 0;
}

#else
int _wrap_g_action_map_add_action(Stack stack, int rhs, int opt, int lhs) /* add_action */
{
  Scierror("Error: function g_action_map_add_action not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_action_map_remove_action(NspGActionMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *action_name;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    g_action_map_remove_action(G_ACTION_MAP(self->obj),action_name);
  return 0;
}

#else
int _wrap_g_action_map_remove_action(Stack stack, int rhs, int opt, int lhs) /* remove_action */
{
  Scierror("Error: function g_action_map_remove_action not available\n");
  return RET_BUG;
}
#endif
#line 74 "codegen-3.0/gio.override"

static int _wrap_g_action_map_add_action_entries(NspGActionMap *self,Stack stack,int rhs,int opt,int lhs)
{
  int i;
  GActionEntry action_entry={ NULL,NULL,NULL,NULL,NULL};
  int_types T[] = {smat, t_end};
  NspSMatrix *entries;
  CheckLhs(0,0);
  if ( GetArgs(stack,rhs,opt,T,&entries) == FAIL) return RET_BUG;
  if ( entries->n < 3 )
    {
      Scierror("Error: expecting at least a mx3 string matrix\n");
      return RET_BUG;
    }
  for (i=0; i < entries->m; i++)
    {
      action_entry.name = entries->S[i];
      action_entry.parameter_type = (entries->S[i+entries->m][0] != '\0' ) ? entries->S[i+entries->m]: NULL;
      action_entry.state = (entries->S[i+entries->m*2][0] != '\0' ) ? entries->S[i+entries->m*2]: NULL;
      g_action_map_add_action_entries(G_ACTION_MAP(self->obj),&action_entry,1,NULL);
    }
  return 0;
}

#line 1641 "gio.c"


static NspMethods gactionmap_methods[] = {
  {"lookup_action",(nsp_method *) _wrap_g_action_map_lookup_action},
  {"add_action",(nsp_method *) _wrap_g_action_map_add_action},
  {"remove_action",(nsp_method *) _wrap_g_action_map_remove_action},
  {"add_action_entries",(nsp_method *) _wrap_g_action_map_add_action_entries},
  { NULL, NULL}
};

static NspMethods *gactionmap_get_methods(void) { return gactionmap_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gactionmap_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGAppInfo ----------- */


#define  NspGAppInfo_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gappinfo.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGAppInfo inherits from GObject 
 */

int nsp_type_gappinfo_id=0;
NspTypeGAppInfo *nsp_type_gappinfo=NULL;

/*
 * Type object for NspGAppInfo 
 * all the instance of NspTypeGAppInfo share the same id. 
 * nsp_type_gappinfo: is an instance of NspTypeGAppInfo 
 *    used for objects of NspGAppInfo type (i.e built with new_gappinfo) 
 * other instances are used for derived classes 
 */
NspTypeGAppInfo *new_type_gappinfo(type_mode mode)
{
  NspTypeGAppInfo *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gappinfo != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gappinfo;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gappinfo_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gappinfo_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gappinfo;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gappinfo */ 

  top->s_type =  (s_type_func *) nsp_gappinfo_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gappinfo_type_short_string;
  /* top->create = (create_func*) int_gappinfo_create;*/

  /* specific methods for gappinfo */

  type->init = (init_func *) init_gappinfo;

  /* 
   * NspGAppInfo interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gappinfo_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGAppInfo called nsp_type_gappinfo
       */
      type->id =  nsp_type_gappinfo_id = nsp_new_type_id();
      nsp_type_gappinfo = type;
      if ( nsp_register_type(nsp_type_gappinfo) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gappinfo, G_TYPE_APP_INFO);
      return ( mode == T_BASE ) ? type : new_type_gappinfo(mode);
    }
  else 
    {
      type->id = nsp_type_gappinfo_id;
      return type;
    }
}

/*
 * initialize NspGAppInfo instances 
 * locally and by calling initializer on parent class 
 */

static int init_gappinfo(NspGAppInfo *Obj,NspTypeGAppInfo *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGAppInfo 
 */

NspGAppInfo *new_gappinfo() 
{
  NspGAppInfo *loc;
  /* type must exists */
  nsp_type_gappinfo = new_type_gappinfo(T_BASE);
  if ( (loc = malloc(sizeof(NspGAppInfo)))== NULLGAPPINFO) return loc;
  /* initialize object */
  if ( init_gappinfo(loc,nsp_type_gappinfo) == FAIL) return NULLGAPPINFO;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGAppInfo 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gappinfo_type_name[]="GAppInfo";
static char gappinfo_short_type_name[]="GAppInfo";

static char *nsp_gappinfo_type_as_string(void)
{
  return(gappinfo_type_name);
}

static char *nsp_gappinfo_type_short_string(NspObject *v)
{
  return(gappinfo_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGAppInfo objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGAppInfo   *nsp_gappinfo_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gappinfo_id)  == TRUE  ) return ((NspGAppInfo *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gappinfo));
  return NULL;
}

int IsGAppInfoObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gappinfo_id);
}

int IsGAppInfo(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gappinfo_id);
}

NspGAppInfo  *GetGAppInfoCopy(Stack stack, int i)
{
  if (  GetGAppInfo(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGAppInfo  *GetGAppInfo(Stack stack, int i)
{
  NspGAppInfo *M;
  if (( M = nsp_gappinfo_object(NthObj(i))) == NULLGAPPINFO)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGAppInfo *gappinfo_copy(NspGAppInfo *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gappinfo);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gappinfo);
}

/*-------------------------------------------------------------------
 * wrappers for the GAppInfo
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_app_info_dup(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  GAppInfo *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_app_info_dup(G_APP_INFO(self->obj));
  nsp_type_gappinfo = new_type_gappinfo(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gappinfo))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_app_info_equal(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *appinfo2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gappinfo, &appinfo2) == FAIL) return RET_BUG;
    ret =g_app_info_equal(G_APP_INFO(self->obj),G_APP_INFO(appinfo2->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_id(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_app_info_get_id(G_APP_INFO(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_name(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_app_info_get_name(G_APP_INFO(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_display_name(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_app_info_get_display_name(G_APP_INFO(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_description(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_app_info_get_description(G_APP_INFO(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_executable(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_app_info_get_executable(G_APP_INFO(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_commandline(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_app_info_get_commandline(G_APP_INFO(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_get_icon(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_app_info_get_icon(G_APP_INFO(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_app_info_launch(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list,obj_check, t_end};
  NspList *nsp_files;
  GList *files;
  NspGObject *launch_context;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_files, &nsp_type_gapplaunchcontext, &launch_context) == FAIL) return RET_BUG;
  files=nsp_glist_from_nsplist(stack,nsp_files);
  if (files== NULL) return RET_BUG;
    ret =g_app_info_launch(G_APP_INFO(self->obj),files,G_APP_LAUNCH_CONTEXT(launch_context->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_supports_uris(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_app_info_supports_uris(G_APP_INFO(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_supports_files(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_app_info_supports_files(G_APP_INFO(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_launch_uris(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {list,obj_check, t_end};
  NspList *nsp_uris;
  GList *uris;
  NspGObject *launch_context;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_uris, &nsp_type_gapplaunchcontext, &launch_context) == FAIL) return RET_BUG;
  uris=nsp_glist_from_nsplist(stack,nsp_uris);
  if (uris== NULL) return RET_BUG;
    ret =g_app_info_launch_uris(G_APP_INFO(self->obj),uris,G_APP_LAUNCH_CONTEXT(launch_context->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_should_show(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_app_info_should_show(G_APP_INFO(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_set_as_default_for_type(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *content_type;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_set_as_default_for_type(G_APP_INFO(self->obj),content_type,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_set_as_default_for_extension(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *extension;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&extension) == FAIL) return RET_BUG;
    ret =g_app_info_set_as_default_for_extension(G_APP_INFO(self->obj),extension,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_add_supports_type(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *content_type;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_add_supports_type(G_APP_INFO(self->obj),content_type,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_can_remove_supports_type(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_app_info_can_remove_supports_type(G_APP_INFO(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_remove_supports_type(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *content_type;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_remove_supports_type(G_APP_INFO(self->obj),content_type,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_can_delete(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_app_info_can_delete(G_APP_INFO(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_delete(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_app_info_delete(G_APP_INFO(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_app_info_set_as_last_used_for_type(NspGAppInfo *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *content_type;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_set_as_last_used_for_type(G_APP_INFO(self->obj),content_type,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gappinfo_methods[] = {
  {"dup",(nsp_method *) _wrap_g_app_info_dup},
  {"equal",(nsp_method *) _wrap_g_app_info_equal},
  {"get_id",(nsp_method *) _wrap_g_app_info_get_id},
  {"get_name",(nsp_method *) _wrap_g_app_info_get_name},
  {"get_display_name",(nsp_method *) _wrap_g_app_info_get_display_name},
  {"get_description",(nsp_method *) _wrap_g_app_info_get_description},
  {"get_executable",(nsp_method *) _wrap_g_app_info_get_executable},
  {"get_commandline",(nsp_method *) _wrap_g_app_info_get_commandline},
  {"get_icon",(nsp_method *) _wrap_g_app_info_get_icon},
  {"launch",(nsp_method *) _wrap_g_app_info_launch},
  {"supports_uris",(nsp_method *) _wrap_g_app_info_supports_uris},
  {"supports_files",(nsp_method *) _wrap_g_app_info_supports_files},
  {"launch_uris",(nsp_method *) _wrap_g_app_info_launch_uris},
  {"should_show",(nsp_method *) _wrap_g_app_info_should_show},
  {"set_as_default_for_type",(nsp_method *) _wrap_g_app_info_set_as_default_for_type},
  {"set_as_default_for_extension",(nsp_method *) _wrap_g_app_info_set_as_default_for_extension},
  {"add_supports_type",(nsp_method *) _wrap_g_app_info_add_supports_type},
  {"can_remove_supports_type",(nsp_method *) _wrap_g_app_info_can_remove_supports_type},
  {"remove_supports_type",(nsp_method *) _wrap_g_app_info_remove_supports_type},
  {"can_delete",(nsp_method *) _wrap_g_app_info_can_delete},
  {"delete",(nsp_method *) _wrap_g_app_info_delete},
  {"set_as_last_used_for_type",(nsp_method *) _wrap_g_app_info_set_as_last_used_for_type},
  { NULL, NULL}
};

static NspMethods *gappinfo_get_methods(void) { return gappinfo_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gappinfo_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGAppLaunchContext ----------- */


#define  NspGAppLaunchContext_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gapplaunchcontext.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGAppLaunchContext inherits from GObject 
 */

int nsp_type_gapplaunchcontext_id=0;
NspTypeGAppLaunchContext *nsp_type_gapplaunchcontext=NULL;

/*
 * Type object for NspGAppLaunchContext 
 * all the instance of NspTypeGAppLaunchContext share the same id. 
 * nsp_type_gapplaunchcontext: is an instance of NspTypeGAppLaunchContext 
 *    used for objects of NspGAppLaunchContext type (i.e built with new_gapplaunchcontext) 
 * other instances are used for derived classes 
 */
NspTypeGAppLaunchContext *new_type_gapplaunchcontext(type_mode mode)
{
  NspTypeGAppLaunchContext *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gapplaunchcontext != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gapplaunchcontext;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gapplaunchcontext_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gapplaunchcontext_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gapplaunchcontext;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gapplaunchcontext */ 

  top->s_type =  (s_type_func *) nsp_gapplaunchcontext_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gapplaunchcontext_type_short_string;
  /* top->create = (create_func*) int_gapplaunchcontext_create;*/

  /* specific methods for gapplaunchcontext */

  type->init = (init_func *) init_gapplaunchcontext;

  /* 
   * NspGAppLaunchContext interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gapplaunchcontext_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGAppLaunchContext called nsp_type_gapplaunchcontext
       */
      type->id =  nsp_type_gapplaunchcontext_id = nsp_new_type_id();
      nsp_type_gapplaunchcontext = type;
      if ( nsp_register_type(nsp_type_gapplaunchcontext) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gapplaunchcontext, G_TYPE_APP_LAUNCH_CONTEXT);
      return ( mode == T_BASE ) ? type : new_type_gapplaunchcontext(mode);
    }
  else 
    {
      type->id = nsp_type_gapplaunchcontext_id;
      return type;
    }
}

/*
 * initialize NspGAppLaunchContext instances 
 * locally and by calling initializer on parent class 
 */

static int init_gapplaunchcontext(NspGAppLaunchContext *Obj,NspTypeGAppLaunchContext *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGAppLaunchContext 
 */

NspGAppLaunchContext *new_gapplaunchcontext() 
{
  NspGAppLaunchContext *loc;
  /* type must exists */
  nsp_type_gapplaunchcontext = new_type_gapplaunchcontext(T_BASE);
  if ( (loc = malloc(sizeof(NspGAppLaunchContext)))== NULLGAPPLAUNCHCONTEXT) return loc;
  /* initialize object */
  if ( init_gapplaunchcontext(loc,nsp_type_gapplaunchcontext) == FAIL) return NULLGAPPLAUNCHCONTEXT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGAppLaunchContext 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gapplaunchcontext_type_name[]="GAppLaunchContext";
static char gapplaunchcontext_short_type_name[]="GAppLaunchContext";

static char *nsp_gapplaunchcontext_type_as_string(void)
{
  return(gapplaunchcontext_type_name);
}

static char *nsp_gapplaunchcontext_type_short_string(NspObject *v)
{
  return(gapplaunchcontext_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGAppLaunchContext objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGAppLaunchContext   *nsp_gapplaunchcontext_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gapplaunchcontext_id)  == TRUE  ) return ((NspGAppLaunchContext *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gapplaunchcontext));
  return NULL;
}

int IsGAppLaunchContextObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gapplaunchcontext_id);
}

int IsGAppLaunchContext(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gapplaunchcontext_id);
}

NspGAppLaunchContext  *GetGAppLaunchContextCopy(Stack stack, int i)
{
  if (  GetGAppLaunchContext(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGAppLaunchContext  *GetGAppLaunchContext(Stack stack, int i)
{
  NspGAppLaunchContext *M;
  if (( M = nsp_gapplaunchcontext_object(NthObj(i))) == NULLGAPPLAUNCHCONTEXT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGAppLaunchContext *gapplaunchcontext_copy(NspGAppLaunchContext *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gapplaunchcontext);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gapplaunchcontext);
}

/*-------------------------------------------------------------------
 * wrappers for the GAppLaunchContext
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_app_launch_context_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_app_launch_context_new())== NULL) return RET_BUG;

  nsp_type_gapplaunchcontext = new_type_gapplaunchcontext(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gapplaunchcontext );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_app_launch_context_setenv(NspGAppLaunchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *variable, *value;
  if ( GetArgs(stack,rhs,opt,T,&variable, &value) == FAIL) return RET_BUG;
    g_app_launch_context_setenv(G_APP_LAUNCH_CONTEXT(self->obj),variable,value);
  return 0;
}

#else
int _wrap_g_app_launch_context_setenv(Stack stack, int rhs, int opt, int lhs) /* setenv */
{
  Scierror("Error: function g_app_launch_context_setenv not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_app_launch_context_unsetenv(NspGAppLaunchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *variable;
  if ( GetArgs(stack,rhs,opt,T,&variable) == FAIL) return RET_BUG;
    g_app_launch_context_unsetenv(G_APP_LAUNCH_CONTEXT(self->obj),variable);
  return 0;
}

#else
int _wrap_g_app_launch_context_unsetenv(Stack stack, int rhs, int opt, int lhs) /* unsetenv */
{
  Scierror("Error: function g_app_launch_context_unsetenv not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_app_launch_context_get_environment(NspGAppLaunchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_app_launch_context_get_environment(G_APP_LAUNCH_CONTEXT(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_app_launch_context_get_environment(Stack stack, int rhs, int opt, int lhs) /* get_environment */
{
  Scierror("Error: function g_app_launch_context_get_environment not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_app_launch_context_get_display(NspGAppLaunchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,list, t_end};
  NspGObject *info;
  NspList *nsp_files;
  GList *files;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gappinfo, &info, &nsp_files) == FAIL) return RET_BUG;
  files=nsp_glist_from_nsplist(stack,nsp_files);
  if (files== NULL) return RET_BUG;
    ret =g_app_launch_context_get_display(G_APP_LAUNCH_CONTEXT(self->obj),G_APP_INFO(info->obj),files);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_app_launch_context_get_startup_notify_id(NspGAppLaunchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,list, t_end};
  NspGObject *info;
  NspList *nsp_files;
  GList *files;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gappinfo, &info, &nsp_files) == FAIL) return RET_BUG;
  files=nsp_glist_from_nsplist(stack,nsp_files);
  if (files== NULL) return RET_BUG;
    ret =g_app_launch_context_get_startup_notify_id(G_APP_LAUNCH_CONTEXT(self->obj),G_APP_INFO(info->obj),files);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_app_launch_context_launch_failed(NspGAppLaunchContext *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *startup_notify_id;
  if ( GetArgs(stack,rhs,opt,T,&startup_notify_id) == FAIL) return RET_BUG;
    g_app_launch_context_launch_failed(G_APP_LAUNCH_CONTEXT(self->obj),startup_notify_id);
  return 0;
}

static NspMethods gapplaunchcontext_methods[] = {
  {"setenv",(nsp_method *) _wrap_g_app_launch_context_setenv},
  {"unsetenv",(nsp_method *) _wrap_g_app_launch_context_unsetenv},
  {"get_environment",(nsp_method *) _wrap_g_app_launch_context_get_environment},
  {"get_display",(nsp_method *) _wrap_g_app_launch_context_get_display},
  {"get_startup_notify_id",(nsp_method *) _wrap_g_app_launch_context_get_startup_notify_id},
  {"launch_failed",(nsp_method *) _wrap_g_app_launch_context_launch_failed},
  { NULL, NULL}
};

static NspMethods *gapplaunchcontext_get_methods(void) { return gapplaunchcontext_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gapplaunchcontext_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGApplication ----------- */


#define  NspGApplication_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gapplication.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGApplication inherits from GObject 
 * and implements GActionGroup GActionMap
 */

int nsp_type_gapplication_id=0;
NspTypeGApplication *nsp_type_gapplication=NULL;

/*
 * Type object for NspGApplication 
 * all the instance of NspTypeGApplication share the same id. 
 * nsp_type_gapplication: is an instance of NspTypeGApplication 
 *    used for objects of NspGApplication type (i.e built with new_gapplication) 
 * other instances are used for derived classes 
 */
NspTypeGApplication *new_type_gapplication(type_mode mode)
{
  NspTypeGActionGroup *t_gactiongroup;
  NspTypeGActionMap *t_gactionmap;
  NspTypeGApplication *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gapplication != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gapplication;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gapplication_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gapplication_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gapplication;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gapplication */ 

  top->s_type =  (s_type_func *) nsp_gapplication_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gapplication_type_short_string;
  /* top->create = (create_func*) int_gapplication_create;*/

  /* specific methods for gapplication */

  type->init = (init_func *) init_gapplication;

  /* 
   * NspGApplication interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_gactiongroup = new_type_gactiongroup(T_DERIVED);
  type->interface = (NspTypeBase * ) t_gactiongroup;
  t_gactionmap = new_type_gactionmap(T_DERIVED);
  type->interface->interface = (NspTypeBase * ) t_gactionmap;
  if ( nsp_type_gapplication_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGApplication called nsp_type_gapplication
       */
      type->id =  nsp_type_gapplication_id = nsp_new_type_id();
      nsp_type_gapplication = type;
      if ( nsp_register_type(nsp_type_gapplication) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gapplication, G_TYPE_APPLICATION);
      return ( mode == T_BASE ) ? type : new_type_gapplication(mode);
    }
  else 
    {
      type->id = nsp_type_gapplication_id;
      return type;
    }
}

/*
 * initialize NspGApplication instances 
 * locally and by calling initializer on parent class 
 */

static int init_gapplication(NspGApplication *Obj,NspTypeGApplication *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGApplication 
 */

NspGApplication *new_gapplication() 
{
  NspGApplication *loc;
  /* type must exists */
  nsp_type_gapplication = new_type_gapplication(T_BASE);
  if ( (loc = malloc(sizeof(NspGApplication)))== NULLGAPPLICATION) return loc;
  /* initialize object */
  if ( init_gapplication(loc,nsp_type_gapplication) == FAIL) return NULLGAPPLICATION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGApplication 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gapplication_type_name[]="GApplication";
static char gapplication_short_type_name[]="GApplication";

static char *nsp_gapplication_type_as_string(void)
{
  return(gapplication_type_name);
}

static char *nsp_gapplication_type_short_string(NspObject *v)
{
  return(gapplication_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGApplication objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGApplication   *nsp_gapplication_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gapplication_id)  == TRUE  ) return ((NspGApplication *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gapplication));
  return NULL;
}

int IsGApplicationObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gapplication_id);
}

int IsGApplication(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gapplication_id);
}

NspGApplication  *GetGApplicationCopy(Stack stack, int i)
{
  if (  GetGApplication(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGApplication  *GetGApplication(Stack stack, int i)
{
  NspGApplication *M;
  if (( M = nsp_gapplication_object(NthObj(i))) == NULLGAPPLICATION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGApplication *gapplication_copy(NspGApplication *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gapplication);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gapplication);
}

/*-------------------------------------------------------------------
 * wrappers for the GApplication
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_application_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,obj, t_end};
  char *application_id;
  GApplicationFlags flags;
  NspObject *nsp_flags = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&application_id, &nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_APPLICATION_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
  if ((ret = (GObject *)g_application_new(application_id,flags))== NULL) return RET_BUG;

  nsp_type_gapplication = new_type_gapplication(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gapplication );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_application_get_application_id(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_application_get_application_id(G_APPLICATION(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_set_application_id(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *application_id;
  if ( GetArgs(stack,rhs,opt,T,&application_id) == FAIL) return RET_BUG;
    g_application_set_application_id(G_APPLICATION(self->obj),application_id);
  return 0;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_application_get_dbus_object_path(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_application_get_dbus_object_path(G_APPLICATION(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_application_get_dbus_object_path(Stack stack, int rhs, int opt, int lhs) /* get_dbus_object_path */
{
  Scierror("Error: function g_application_get_dbus_object_path not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_application_get_inactivity_timeout(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_application_get_inactivity_timeout(G_APPLICATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_set_inactivity_timeout(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int inactivity_timeout;
  if ( GetArgs(stack,rhs,opt,T,&inactivity_timeout) == FAIL) return RET_BUG;
    g_application_set_inactivity_timeout(G_APPLICATION(self->obj),inactivity_timeout);
  return 0;
}

static int _wrap_g_application_get_flags(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =g_application_get_flags(G_APPLICATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_set_flags(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GApplicationFlags flags;
  NspObject *nsp_flags = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_APPLICATION_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    g_application_set_flags(G_APPLICATION(self->obj),flags);
  return 0;
}

#if GTK_CHECK_VERSION(GLIB,DEPRECATED,0)
int _wrap_g_application_set_action_group(Stack stack, int rhs, int opt, int lhs) /* set_action_group */
{
  Scierror("Error: function g_application_set_action_group is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_g_application_set_action_group(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *action_group;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gactiongroup, &action_group) == FAIL) return RET_BUG;
    g_application_set_action_group(G_APPLICATION(self->obj),G_ACTION_GROUP(action_group->obj));
  return 0;
}

#endif
static int _wrap_g_application_get_is_registered(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_application_get_is_registered(G_APPLICATION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_get_is_remote(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_application_get_is_remote(G_APPLICATION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_register(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_application_register(G_APPLICATION(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_hold(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_hold(G_APPLICATION(self->obj));
  return 0;
}

static int _wrap_g_application_release(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_release(G_APPLICATION(self->obj));
  return 0;
}

static int _wrap_g_application_activate(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_activate(G_APPLICATION(self->obj));
  return 0;
}

static int _wrap_g_application_run(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj, t_end};
  int argc, ret;
  gchar **argv = NULL;
  NspObject *nsp_argv = NULL;
  if ( GetArgs(stack,rhs,opt,T,&argc, &nsp_argv) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_argv))
    { argv =  ((NspSMatrix *) nsp_argv)->S;}
  else
    {
      Scierror("Error: argv should be of type SMat");
      return RET_BUG;
    }
    ret =g_application_run(G_APPLICATION(self->obj),argc,argv);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_application_quit(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_quit(G_APPLICATION(self->obj));
  return 0;
}

#else
int _wrap_g_application_quit(Stack stack, int rhs, int opt, int lhs) /* quit */
{
  Scierror("Error: function g_application_quit not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_application_set_default(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_set_default(G_APPLICATION(self->obj));
  return 0;
}

#else
int _wrap_g_application_set_default(Stack stack, int rhs, int opt, int lhs) /* set_default */
{
  Scierror("Error: function g_application_set_default not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,38,0)
static int _wrap_g_application_mark_busy(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_mark_busy(G_APPLICATION(self->obj));
  return 0;
}

#else
int _wrap_g_application_mark_busy(Stack stack, int rhs, int opt, int lhs) /* mark_busy */
{
  Scierror("Error: function g_application_mark_busy not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,38,0)
static int _wrap_g_application_unmark_busy(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_application_unmark_busy(G_APPLICATION(self->obj));
  return 0;
}

#else
int _wrap_g_application_unmark_busy(Stack stack, int rhs, int opt, int lhs) /* unmark_busy */
{
  Scierror("Error: function g_application_unmark_busy not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,40,0)
static int _wrap_g_application_withdraw_notification(NspGApplication *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *id;
  if ( GetArgs(stack,rhs,opt,T,&id) == FAIL) return RET_BUG;
    g_application_withdraw_notification(G_APPLICATION(self->obj),id);
  return 0;
}

#else
int _wrap_g_application_withdraw_notification(Stack stack, int rhs, int opt, int lhs) /* withdraw_notification */
{
  Scierror("Error: function g_application_withdraw_notification not available\n");
  return RET_BUG;
}
#endif
static NspMethods gapplication_methods[] = {
  {"get_application_id",(nsp_method *) _wrap_g_application_get_application_id},
  {"set_application_id",(nsp_method *) _wrap_g_application_set_application_id},
  {"get_dbus_object_path",(nsp_method *) _wrap_g_application_get_dbus_object_path},
  {"get_inactivity_timeout",(nsp_method *) _wrap_g_application_get_inactivity_timeout},
  {"set_inactivity_timeout",(nsp_method *) _wrap_g_application_set_inactivity_timeout},
  {"get_flags",(nsp_method *) _wrap_g_application_get_flags},
  {"set_flags",(nsp_method *) _wrap_g_application_set_flags},
  {"set_action_group",(nsp_method *) _wrap_g_application_set_action_group},
  {"get_is_registered",(nsp_method *) _wrap_g_application_get_is_registered},
  {"get_is_remote",(nsp_method *) _wrap_g_application_get_is_remote},
  {"register",(nsp_method *) _wrap_g_application_register},
  {"hold",(nsp_method *) _wrap_g_application_hold},
  {"release",(nsp_method *) _wrap_g_application_release},
  {"activate",(nsp_method *) _wrap_g_application_activate},
  {"run",(nsp_method *) _wrap_g_application_run},
  {"quit",(nsp_method *) _wrap_g_application_quit},
  {"set_default",(nsp_method *) _wrap_g_application_set_default},
  {"mark_busy",(nsp_method *) _wrap_g_application_mark_busy},
  {"unmark_busy",(nsp_method *) _wrap_g_application_unmark_busy},
  {"withdraw_notification",(nsp_method *) _wrap_g_application_withdraw_notification},
  { NULL, NULL}
};

static NspMethods *gapplication_get_methods(void) { return gapplication_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gapplication_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGApplicationCommandLine ----------- */


#define  NspGApplicationCommandLine_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gapplicationcommandline.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGApplicationCommandLine inherits from GObject 
 */

int nsp_type_gapplicationcommandline_id=0;
NspTypeGApplicationCommandLine *nsp_type_gapplicationcommandline=NULL;

/*
 * Type object for NspGApplicationCommandLine 
 * all the instance of NspTypeGApplicationCommandLine share the same id. 
 * nsp_type_gapplicationcommandline: is an instance of NspTypeGApplicationCommandLine 
 *    used for objects of NspGApplicationCommandLine type (i.e built with new_gapplicationcommandline) 
 * other instances are used for derived classes 
 */
NspTypeGApplicationCommandLine *new_type_gapplicationcommandline(type_mode mode)
{
  NspTypeGApplicationCommandLine *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gapplicationcommandline != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gapplicationcommandline;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gapplicationcommandline_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gapplicationcommandline_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gapplicationcommandline;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gapplicationcommandline */ 

  top->s_type =  (s_type_func *) nsp_gapplicationcommandline_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gapplicationcommandline_type_short_string;
  /* top->create = (create_func*) int_gapplicationcommandline_create;*/

  /* specific methods for gapplicationcommandline */

  type->init = (init_func *) init_gapplicationcommandline;

  /* 
   * NspGApplicationCommandLine interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gapplicationcommandline_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGApplicationCommandLine called nsp_type_gapplicationcommandline
       */
      type->id =  nsp_type_gapplicationcommandline_id = nsp_new_type_id();
      nsp_type_gapplicationcommandline = type;
      if ( nsp_register_type(nsp_type_gapplicationcommandline) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gapplicationcommandline, G_TYPE_APPLICATION_COMMAND_LINE);
      return ( mode == T_BASE ) ? type : new_type_gapplicationcommandline(mode);
    }
  else 
    {
      type->id = nsp_type_gapplicationcommandline_id;
      return type;
    }
}

/*
 * initialize NspGApplicationCommandLine instances 
 * locally and by calling initializer on parent class 
 */

static int init_gapplicationcommandline(NspGApplicationCommandLine *Obj,NspTypeGApplicationCommandLine *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGApplicationCommandLine 
 */

NspGApplicationCommandLine *new_gapplicationcommandline() 
{
  NspGApplicationCommandLine *loc;
  /* type must exists */
  nsp_type_gapplicationcommandline = new_type_gapplicationcommandline(T_BASE);
  if ( (loc = malloc(sizeof(NspGApplicationCommandLine)))== NULLGAPPLICATIONCOMMANDLINE) return loc;
  /* initialize object */
  if ( init_gapplicationcommandline(loc,nsp_type_gapplicationcommandline) == FAIL) return NULLGAPPLICATIONCOMMANDLINE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGApplicationCommandLine 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gapplicationcommandline_type_name[]="GApplicationCommandLine";
static char gapplicationcommandline_short_type_name[]="GApplicationCommandLine";

static char *nsp_gapplicationcommandline_type_as_string(void)
{
  return(gapplicationcommandline_type_name);
}

static char *nsp_gapplicationcommandline_type_short_string(NspObject *v)
{
  return(gapplicationcommandline_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGApplicationCommandLine objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGApplicationCommandLine   *nsp_gapplicationcommandline_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gapplicationcommandline_id)  == TRUE  ) return ((NspGApplicationCommandLine *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gapplicationcommandline));
  return NULL;
}

int IsGApplicationCommandLineObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gapplicationcommandline_id);
}

int IsGApplicationCommandLine(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gapplicationcommandline_id);
}

NspGApplicationCommandLine  *GetGApplicationCommandLineCopy(Stack stack, int i)
{
  if (  GetGApplicationCommandLine(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGApplicationCommandLine  *GetGApplicationCommandLine(Stack stack, int i)
{
  NspGApplicationCommandLine *M;
  if (( M = nsp_gapplicationcommandline_object(NthObj(i))) == NULLGAPPLICATIONCOMMANDLINE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGApplicationCommandLine *gapplicationcommandline_copy(NspGApplicationCommandLine *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gapplicationcommandline);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gapplicationcommandline);
}

/*-------------------------------------------------------------------
 * wrappers for the GApplicationCommandLine
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_application_command_line_get_arguments(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int argc;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&argc) == FAIL) return RET_BUG;
    ret =g_application_command_line_get_arguments(G_APPLICATION_COMMAND_LINE(self->obj),&argc);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_application_command_line_get_stdin(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  GInputStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_application_command_line_get_stdin(G_APPLICATION_COMMAND_LINE(self->obj));
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_application_command_line_get_stdin(Stack stack, int rhs, int opt, int lhs) /* get_stdin */
{
  Scierror("Error: function g_application_command_line_get_stdin not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_application_command_line_getenv(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  const gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =g_application_command_line_getenv(G_APPLICATION_COMMAND_LINE(self->obj),name);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_command_line_get_cwd(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_application_command_line_get_cwd(G_APPLICATION_COMMAND_LINE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_command_line_get_is_remote(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_application_command_line_get_is_remote(G_APPLICATION_COMMAND_LINE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_command_line_get_exit_status(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_application_command_line_get_exit_status(G_APPLICATION_COMMAND_LINE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_application_command_line_set_exit_status(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int exit_status;
  if ( GetArgs(stack,rhs,opt,T,&exit_status) == FAIL) return RET_BUG;
    g_application_command_line_set_exit_status(G_APPLICATION_COMMAND_LINE(self->obj),exit_status);
  return 0;
}

static int _wrap_g_application_command_line_get_platform_data(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariant *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_application_command_line_get_platform_data(G_APPLICATION_COMMAND_LINE(self->obj));
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_application_command_line_create_file_for_arg(NspGApplicationCommandLine *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *arg;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&arg) == FAIL) return RET_BUG;
    ret =g_application_command_line_create_file_for_arg(G_APPLICATION_COMMAND_LINE(self->obj),arg);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_application_command_line_create_file_for_arg(Stack stack, int rhs, int opt, int lhs) /* create_file_for_arg */
{
  Scierror("Error: function g_application_command_line_create_file_for_arg not available\n");
  return RET_BUG;
}
#endif
static NspMethods gapplicationcommandline_methods[] = {
  {"get_arguments",(nsp_method *) _wrap_g_application_command_line_get_arguments},
  {"get_stdin",(nsp_method *) _wrap_g_application_command_line_get_stdin},
  {"getenv",(nsp_method *) _wrap_g_application_command_line_getenv},
  {"get_cwd",(nsp_method *) _wrap_g_application_command_line_get_cwd},
  {"get_is_remote",(nsp_method *) _wrap_g_application_command_line_get_is_remote},
  {"get_exit_status",(nsp_method *) _wrap_g_application_command_line_get_exit_status},
  {"set_exit_status",(nsp_method *) _wrap_g_application_command_line_set_exit_status},
  {"get_platform_data",(nsp_method *) _wrap_g_application_command_line_get_platform_data},
  {"create_file_for_arg",(nsp_method *) _wrap_g_application_command_line_create_file_for_arg},
  { NULL, NULL}
};

static NspMethods *gapplicationcommandline_get_methods(void) { return gapplicationcommandline_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gapplicationcommandline_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGAsyncInitable ----------- */


#define  NspGAsyncInitable_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gasyncinitable.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGAsyncInitable inherits from GObject 
 */

int nsp_type_gasyncinitable_id=0;
NspTypeGAsyncInitable *nsp_type_gasyncinitable=NULL;

/*
 * Type object for NspGAsyncInitable 
 * all the instance of NspTypeGAsyncInitable share the same id. 
 * nsp_type_gasyncinitable: is an instance of NspTypeGAsyncInitable 
 *    used for objects of NspGAsyncInitable type (i.e built with new_gasyncinitable) 
 * other instances are used for derived classes 
 */
NspTypeGAsyncInitable *new_type_gasyncinitable(type_mode mode)
{
  NspTypeGAsyncInitable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gasyncinitable != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gasyncinitable;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gasyncinitable_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gasyncinitable_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gasyncinitable;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gasyncinitable */ 

  top->s_type =  (s_type_func *) nsp_gasyncinitable_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gasyncinitable_type_short_string;
  /* top->create = (create_func*) int_gasyncinitable_create;*/

  /* specific methods for gasyncinitable */

  type->init = (init_func *) init_gasyncinitable;

  /* 
   * NspGAsyncInitable interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gasyncinitable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGAsyncInitable called nsp_type_gasyncinitable
       */
      type->id =  nsp_type_gasyncinitable_id = nsp_new_type_id();
      nsp_type_gasyncinitable = type;
      if ( nsp_register_type(nsp_type_gasyncinitable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gasyncinitable, G_TYPE_ASYNC_INITABLE);
      return ( mode == T_BASE ) ? type : new_type_gasyncinitable(mode);
    }
  else 
    {
      type->id = nsp_type_gasyncinitable_id;
      return type;
    }
}

/*
 * initialize NspGAsyncInitable instances 
 * locally and by calling initializer on parent class 
 */

static int init_gasyncinitable(NspGAsyncInitable *Obj,NspTypeGAsyncInitable *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGAsyncInitable 
 */

NspGAsyncInitable *new_gasyncinitable() 
{
  NspGAsyncInitable *loc;
  /* type must exists */
  nsp_type_gasyncinitable = new_type_gasyncinitable(T_BASE);
  if ( (loc = malloc(sizeof(NspGAsyncInitable)))== NULLGASYNCINITABLE) return loc;
  /* initialize object */
  if ( init_gasyncinitable(loc,nsp_type_gasyncinitable) == FAIL) return NULLGASYNCINITABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGAsyncInitable 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gasyncinitable_type_name[]="GAsyncInitable";
static char gasyncinitable_short_type_name[]="GAsyncInitable";

static char *nsp_gasyncinitable_type_as_string(void)
{
  return(gasyncinitable_type_name);
}

static char *nsp_gasyncinitable_type_short_string(NspObject *v)
{
  return(gasyncinitable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGAsyncInitable objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGAsyncInitable   *nsp_gasyncinitable_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gasyncinitable_id)  == TRUE  ) return ((NspGAsyncInitable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gasyncinitable));
  return NULL;
}

int IsGAsyncInitableObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gasyncinitable_id);
}

int IsGAsyncInitable(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gasyncinitable_id);
}

NspGAsyncInitable  *GetGAsyncInitableCopy(Stack stack, int i)
{
  if (  GetGAsyncInitable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGAsyncInitable  *GetGAsyncInitable(Stack stack, int i)
{
  NspGAsyncInitable *M;
  if (( M = nsp_gasyncinitable_object(NthObj(i))) == NULLGASYNCINITABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGAsyncInitable *gasyncinitable_copy(NspGAsyncInitable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gasyncinitable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gasyncinitable);
}

/*-------------------------------------------------------------------
 * wrappers for the GAsyncInitable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_async_initable_new_finish (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *initable, *res;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncinitable, &initable, &nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_async_initable_new_finish(G_ASYNC_INITABLE(initable->obj),G_ASYNC_RESULT(res->obj),&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gasyncinitable = new_type_gasyncinitable(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gasyncinitable );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_async_initable_init_finish(NspGAsyncInitable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_async_initable_init_finish(G_ASYNC_INITABLE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gasyncinitable_methods[] = {
  {"init_finish",(nsp_method *) _wrap_g_async_initable_init_finish},
  { NULL, NULL}
};

static NspMethods *gasyncinitable_get_methods(void) { return gasyncinitable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gasyncinitable_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGAsyncResult ----------- */


#define  NspGAsyncResult_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gasyncresult.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGAsyncResult inherits from GObject 
 */

int nsp_type_gasyncresult_id=0;
NspTypeGAsyncResult *nsp_type_gasyncresult=NULL;

/*
 * Type object for NspGAsyncResult 
 * all the instance of NspTypeGAsyncResult share the same id. 
 * nsp_type_gasyncresult: is an instance of NspTypeGAsyncResult 
 *    used for objects of NspGAsyncResult type (i.e built with new_gasyncresult) 
 * other instances are used for derived classes 
 */
NspTypeGAsyncResult *new_type_gasyncresult(type_mode mode)
{
  NspTypeGAsyncResult *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gasyncresult != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gasyncresult;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gasyncresult_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gasyncresult_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gasyncresult;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gasyncresult */ 

  top->s_type =  (s_type_func *) nsp_gasyncresult_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gasyncresult_type_short_string;
  /* top->create = (create_func*) int_gasyncresult_create;*/

  /* specific methods for gasyncresult */

  type->init = (init_func *) init_gasyncresult;

  /* 
   * NspGAsyncResult interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gasyncresult_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGAsyncResult called nsp_type_gasyncresult
       */
      type->id =  nsp_type_gasyncresult_id = nsp_new_type_id();
      nsp_type_gasyncresult = type;
      if ( nsp_register_type(nsp_type_gasyncresult) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gasyncresult, G_TYPE_ASYNC_RESULT);
      return ( mode == T_BASE ) ? type : new_type_gasyncresult(mode);
    }
  else 
    {
      type->id = nsp_type_gasyncresult_id;
      return type;
    }
}

/*
 * initialize NspGAsyncResult instances 
 * locally and by calling initializer on parent class 
 */

static int init_gasyncresult(NspGAsyncResult *Obj,NspTypeGAsyncResult *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGAsyncResult 
 */

NspGAsyncResult *new_gasyncresult() 
{
  NspGAsyncResult *loc;
  /* type must exists */
  nsp_type_gasyncresult = new_type_gasyncresult(T_BASE);
  if ( (loc = malloc(sizeof(NspGAsyncResult)))== NULLGASYNCRESULT) return loc;
  /* initialize object */
  if ( init_gasyncresult(loc,nsp_type_gasyncresult) == FAIL) return NULLGASYNCRESULT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGAsyncResult 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gasyncresult_type_name[]="GAsyncResult";
static char gasyncresult_short_type_name[]="GAsyncResult";

static char *nsp_gasyncresult_type_as_string(void)
{
  return(gasyncresult_type_name);
}

static char *nsp_gasyncresult_type_short_string(NspObject *v)
{
  return(gasyncresult_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGAsyncResult objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGAsyncResult   *nsp_gasyncresult_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gasyncresult_id)  == TRUE  ) return ((NspGAsyncResult *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gasyncresult));
  return NULL;
}

int IsGAsyncResultObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gasyncresult_id);
}

int IsGAsyncResult(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gasyncresult_id);
}

NspGAsyncResult  *GetGAsyncResultCopy(Stack stack, int i)
{
  if (  GetGAsyncResult(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGAsyncResult  *GetGAsyncResult(Stack stack, int i)
{
  NspGAsyncResult *M;
  if (( M = nsp_gasyncresult_object(NthObj(i))) == NULLGASYNCRESULT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGAsyncResult *gasyncresult_copy(NspGAsyncResult *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gasyncresult);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gasyncresult);
}

/*-------------------------------------------------------------------
 * wrappers for the GAsyncResult
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_async_result_get_source_object(NspGAsyncResult *self,Stack stack,int rhs,int opt,int lhs)
{
  GObject *ret;
  CheckRhs(0,0);
    ret =g_async_result_get_source_object(G_ASYNC_RESULT(self->obj));
  if (ret == NULL ) return RET_BUG;
  MoveObj(stack,1,NSP_OBJECT(ret));
  return 1;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_async_result_legacy_propagate_error(NspGAsyncResult *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_async_result_legacy_propagate_error(G_ASYNC_RESULT(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_async_result_legacy_propagate_error(Stack stack, int rhs, int opt, int lhs) /* legacy_propagate_error */
{
  Scierror("Error: function g_async_result_legacy_propagate_error not available\n");
  return RET_BUG;
}
#endif
static NspMethods gasyncresult_methods[] = {
  {"get_source_object",(nsp_method *) _wrap_g_async_result_get_source_object},
  {"legacy_propagate_error",(nsp_method *) _wrap_g_async_result_legacy_propagate_error},
  { NULL, NULL}
};

static NspMethods *gasyncresult_get_methods(void) { return gasyncresult_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gasyncresult_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGCancellable ----------- */


#define  NspGCancellable_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gcancellable.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGCancellable inherits from GObject 
 */

int nsp_type_gcancellable_id=0;
NspTypeGCancellable *nsp_type_gcancellable=NULL;

/*
 * Type object for NspGCancellable 
 * all the instance of NspTypeGCancellable share the same id. 
 * nsp_type_gcancellable: is an instance of NspTypeGCancellable 
 *    used for objects of NspGCancellable type (i.e built with new_gcancellable) 
 * other instances are used for derived classes 
 */
NspTypeGCancellable *new_type_gcancellable(type_mode mode)
{
  NspTypeGCancellable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gcancellable != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gcancellable;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gcancellable_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gcancellable_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gcancellable;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gcancellable */ 

  top->s_type =  (s_type_func *) nsp_gcancellable_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gcancellable_type_short_string;
  /* top->create = (create_func*) int_gcancellable_create;*/

  /* specific methods for gcancellable */

  type->init = (init_func *) init_gcancellable;

  /* 
   * NspGCancellable interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gcancellable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGCancellable called nsp_type_gcancellable
       */
      type->id =  nsp_type_gcancellable_id = nsp_new_type_id();
      nsp_type_gcancellable = type;
      if ( nsp_register_type(nsp_type_gcancellable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gcancellable, G_TYPE_CANCELLABLE);
      return ( mode == T_BASE ) ? type : new_type_gcancellable(mode);
    }
  else 
    {
      type->id = nsp_type_gcancellable_id;
      return type;
    }
}

/*
 * initialize NspGCancellable instances 
 * locally and by calling initializer on parent class 
 */

static int init_gcancellable(NspGCancellable *Obj,NspTypeGCancellable *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGCancellable 
 */

NspGCancellable *new_gcancellable() 
{
  NspGCancellable *loc;
  /* type must exists */
  nsp_type_gcancellable = new_type_gcancellable(T_BASE);
  if ( (loc = malloc(sizeof(NspGCancellable)))== NULLGCANCELLABLE) return loc;
  /* initialize object */
  if ( init_gcancellable(loc,nsp_type_gcancellable) == FAIL) return NULLGCANCELLABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGCancellable 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gcancellable_type_name[]="GCancellable";
static char gcancellable_short_type_name[]="GCancellable";

static char *nsp_gcancellable_type_as_string(void)
{
  return(gcancellable_type_name);
}

static char *nsp_gcancellable_type_short_string(NspObject *v)
{
  return(gcancellable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGCancellable objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGCancellable   *nsp_gcancellable_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gcancellable_id)  == TRUE  ) return ((NspGCancellable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gcancellable));
  return NULL;
}

int IsGCancellableObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gcancellable_id);
}

int IsGCancellable(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gcancellable_id);
}

NspGCancellable  *GetGCancellableCopy(Stack stack, int i)
{
  if (  GetGCancellable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGCancellable  *GetGCancellable(Stack stack, int i)
{
  NspGCancellable *M;
  if (( M = nsp_gcancellable_object(NthObj(i))) == NULLGCANCELLABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGCancellable *gcancellable_copy(NspGCancellable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gcancellable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gcancellable);
}

/*-------------------------------------------------------------------
 * wrappers for the GCancellable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_cancellable_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_cancellable_new())== NULL) return RET_BUG;

  nsp_type_gcancellable = new_type_gcancellable(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gcancellable );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_cancellable_is_cancelled(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_cancellable_is_cancelled(G_CANCELLABLE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_cancellable_set_error_if_cancelled(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_cancellable_set_error_if_cancelled(G_CANCELLABLE(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_cancellable_get_fd(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_cancellable_get_fd(G_CANCELLABLE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_cancellable_release_fd(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_cancellable_release_fd(G_CANCELLABLE(self->obj));
  return 0;
}

static int _wrap_g_cancellable_push_current(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_cancellable_push_current(G_CANCELLABLE(self->obj));
  return 0;
}

static int _wrap_g_cancellable_pop_current(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_cancellable_pop_current(G_CANCELLABLE(self->obj));
  return 0;
}

static int _wrap_g_cancellable_reset(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_cancellable_reset(G_CANCELLABLE(self->obj));
  return 0;
}

static int _wrap_g_cancellable_disconnect(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  gulong handler_id;
  if ( GetArgs(stack,rhs,opt,T,&handler_id) == FAIL) return RET_BUG;
    g_cancellable_disconnect(G_CANCELLABLE(self->obj),handler_id);
  return 0;
}

static int _wrap_g_cancellable_cancel(NspGCancellable *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_cancellable_cancel(G_CANCELLABLE(self->obj));
  return 0;
}

static NspMethods gcancellable_methods[] = {
  {"is_cancelled",(nsp_method *) _wrap_g_cancellable_is_cancelled},
  {"set_error_if_cancelled",(nsp_method *) _wrap_g_cancellable_set_error_if_cancelled},
  {"get_fd",(nsp_method *) _wrap_g_cancellable_get_fd},
  {"release_fd",(nsp_method *) _wrap_g_cancellable_release_fd},
  {"push_current",(nsp_method *) _wrap_g_cancellable_push_current},
  {"pop_current",(nsp_method *) _wrap_g_cancellable_pop_current},
  {"reset",(nsp_method *) _wrap_g_cancellable_reset},
  {"disconnect",(nsp_method *) _wrap_g_cancellable_disconnect},
  {"cancel",(nsp_method *) _wrap_g_cancellable_cancel},
  { NULL, NULL}
};

static NspMethods *gcancellable_get_methods(void) { return gcancellable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gcancellable_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGConverter ----------- */


#define  NspGConverter_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gconverter.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGConverter inherits from GObject 
 */

int nsp_type_gconverter_id=0;
NspTypeGConverter *nsp_type_gconverter=NULL;

/*
 * Type object for NspGConverter 
 * all the instance of NspTypeGConverter share the same id. 
 * nsp_type_gconverter: is an instance of NspTypeGConverter 
 *    used for objects of NspGConverter type (i.e built with new_gconverter) 
 * other instances are used for derived classes 
 */
NspTypeGConverter *new_type_gconverter(type_mode mode)
{
  NspTypeGConverter *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gconverter != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gconverter;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gconverter_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gconverter_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gconverter;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gconverter */ 

  top->s_type =  (s_type_func *) nsp_gconverter_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gconverter_type_short_string;
  /* top->create = (create_func*) int_gconverter_create;*/

  /* specific methods for gconverter */

  type->init = (init_func *) init_gconverter;

  /* 
   * NspGConverter interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gconverter_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGConverter called nsp_type_gconverter
       */
      type->id =  nsp_type_gconverter_id = nsp_new_type_id();
      nsp_type_gconverter = type;
      if ( nsp_register_type(nsp_type_gconverter) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gconverter, G_TYPE_CONVERTER);
      return ( mode == T_BASE ) ? type : new_type_gconverter(mode);
    }
  else 
    {
      type->id = nsp_type_gconverter_id;
      return type;
    }
}

/*
 * initialize NspGConverter instances 
 * locally and by calling initializer on parent class 
 */

static int init_gconverter(NspGConverter *Obj,NspTypeGConverter *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGConverter 
 */

NspGConverter *new_gconverter() 
{
  NspGConverter *loc;
  /* type must exists */
  nsp_type_gconverter = new_type_gconverter(T_BASE);
  if ( (loc = malloc(sizeof(NspGConverter)))== NULLGCONVERTER) return loc;
  /* initialize object */
  if ( init_gconverter(loc,nsp_type_gconverter) == FAIL) return NULLGCONVERTER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGConverter 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gconverter_type_name[]="GConverter";
static char gconverter_short_type_name[]="GConverter";

static char *nsp_gconverter_type_as_string(void)
{
  return(gconverter_type_name);
}

static char *nsp_gconverter_type_short_string(NspObject *v)
{
  return(gconverter_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGConverter objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGConverter   *nsp_gconverter_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gconverter_id)  == TRUE  ) return ((NspGConverter *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gconverter));
  return NULL;
}

int IsGConverterObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gconverter_id);
}

int IsGConverter(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gconverter_id);
}

NspGConverter  *GetGConverterCopy(Stack stack, int i)
{
  if (  GetGConverter(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGConverter  *GetGConverter(Stack stack, int i)
{
  NspGConverter *M;
  if (( M = nsp_gconverter_object(NthObj(i))) == NULLGCONVERTER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGConverter *gconverter_copy(NspGConverter *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gconverter);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gconverter);
}

/*-------------------------------------------------------------------
 * wrappers for the GConverter
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_converter_reset(NspGConverter *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_converter_reset(G_CONVERTER(self->obj));
  return 0;
}

static NspMethods gconverter_methods[] = {
  {"reset",(nsp_method *) _wrap_g_converter_reset},
  { NULL, NULL}
};

static NspMethods *gconverter_get_methods(void) { return gconverter_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gconverter_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusInterfaceSkeleton ----------- */


#define  NspGDBusInterfaceSkeleton_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusinterfaceskeleton.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusInterfaceSkeleton inherits from GObject 
 */

int nsp_type_gdbusinterfaceskeleton_id=0;
NspTypeGDBusInterfaceSkeleton *nsp_type_gdbusinterfaceskeleton=NULL;

/*
 * Type object for NspGDBusInterfaceSkeleton 
 * all the instance of NspTypeGDBusInterfaceSkeleton share the same id. 
 * nsp_type_gdbusinterfaceskeleton: is an instance of NspTypeGDBusInterfaceSkeleton 
 *    used for objects of NspGDBusInterfaceSkeleton type (i.e built with new_gdbusinterfaceskeleton) 
 * other instances are used for derived classes 
 */
NspTypeGDBusInterfaceSkeleton *new_type_gdbusinterfaceskeleton(type_mode mode)
{
  NspTypeGDBusInterfaceSkeleton *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusinterfaceskeleton != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusinterfaceskeleton;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusinterfaceskeleton_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusinterfaceskeleton_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusinterfaceskeleton;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusinterfaceskeleton */ 

  top->s_type =  (s_type_func *) nsp_gdbusinterfaceskeleton_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusinterfaceskeleton_type_short_string;
  /* top->create = (create_func*) int_gdbusinterfaceskeleton_create;*/

  /* specific methods for gdbusinterfaceskeleton */

  type->init = (init_func *) init_gdbusinterfaceskeleton;

  /* 
   * NspGDBusInterfaceSkeleton interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusinterfaceskeleton_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusInterfaceSkeleton called nsp_type_gdbusinterfaceskeleton
       */
      type->id =  nsp_type_gdbusinterfaceskeleton_id = nsp_new_type_id();
      nsp_type_gdbusinterfaceskeleton = type;
      if ( nsp_register_type(nsp_type_gdbusinterfaceskeleton) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusinterfaceskeleton, G_TYPE_DBUS_INTERFACE_SKELETON);
      return ( mode == T_BASE ) ? type : new_type_gdbusinterfaceskeleton(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusinterfaceskeleton_id;
      return type;
    }
}

/*
 * initialize NspGDBusInterfaceSkeleton instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusinterfaceskeleton(NspGDBusInterfaceSkeleton *Obj,NspTypeGDBusInterfaceSkeleton *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusInterfaceSkeleton 
 */

NspGDBusInterfaceSkeleton *new_gdbusinterfaceskeleton() 
{
  NspGDBusInterfaceSkeleton *loc;
  /* type must exists */
  nsp_type_gdbusinterfaceskeleton = new_type_gdbusinterfaceskeleton(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusInterfaceSkeleton)))== NULLGDBUSINTERFACESKELETON) return loc;
  /* initialize object */
  if ( init_gdbusinterfaceskeleton(loc,nsp_type_gdbusinterfaceskeleton) == FAIL) return NULLGDBUSINTERFACESKELETON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusInterfaceSkeleton 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusinterfaceskeleton_type_name[]="GDBusInterfaceSkeleton";
static char gdbusinterfaceskeleton_short_type_name[]="GDBusInterfaceSkeleton";

static char *nsp_gdbusinterfaceskeleton_type_as_string(void)
{
  return(gdbusinterfaceskeleton_type_name);
}

static char *nsp_gdbusinterfaceskeleton_type_short_string(NspObject *v)
{
  return(gdbusinterfaceskeleton_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusInterfaceSkeleton objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusInterfaceSkeleton   *nsp_gdbusinterfaceskeleton_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdbusinterfaceskeleton_id)  == TRUE  ) return ((NspGDBusInterfaceSkeleton *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusinterfaceskeleton));
  return NULL;
}

int IsGDBusInterfaceSkeletonObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdbusinterfaceskeleton_id);
}

int IsGDBusInterfaceSkeleton(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdbusinterfaceskeleton_id);
}

NspGDBusInterfaceSkeleton  *GetGDBusInterfaceSkeletonCopy(Stack stack, int i)
{
  if (  GetGDBusInterfaceSkeleton(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusInterfaceSkeleton  *GetGDBusInterfaceSkeleton(Stack stack, int i)
{
  NspGDBusInterfaceSkeleton *M;
  if (( M = nsp_gdbusinterfaceskeleton_object(NthObj(i))) == NULLGDBUSINTERFACESKELETON)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusInterfaceSkeleton *gdbusinterfaceskeleton_copy(NspGDBusInterfaceSkeleton *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusinterfaceskeleton);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusinterfaceskeleton);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusInterfaceSkeleton
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_interface_skeleton_get_flags(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =g_dbus_interface_skeleton_get_flags(G_DBUS_INTERFACE_SKELETON(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_interface_skeleton_set_flags(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GDBusInterfaceSkeletonFlags flags;
  NspObject *nsp_flags = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_DBUS_INTERFACE_SKELETON_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    g_dbus_interface_skeleton_set_flags(G_DBUS_INTERFACE_SKELETON(self->obj),flags);
  return 0;
}

static int _wrap_g_dbus_interface_skeleton_get_properties(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariant *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_dbus_interface_skeleton_get_properties(G_DBUS_INTERFACE_SKELETON(self->obj));
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_dbus_interface_skeleton_flush(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_dbus_interface_skeleton_flush(G_DBUS_INTERFACE_SKELETON(self->obj));
  return 0;
}

static int _wrap_g_dbus_interface_skeleton_unexport(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_dbus_interface_skeleton_unexport(G_DBUS_INTERFACE_SKELETON(self->obj));
  return 0;
}

static int _wrap_g_dbus_interface_skeleton_get_connections(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_dbus_interface_skeleton_get_connections(G_DBUS_INTERFACE_SKELETON(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_dbus_interface_skeleton_get_object_path(NspGDBusInterfaceSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_interface_skeleton_get_object_path(G_DBUS_INTERFACE_SKELETON(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdbusinterfaceskeleton_methods[] = {
  {"get_flags",(nsp_method *) _wrap_g_dbus_interface_skeleton_get_flags},
  {"set_flags",(nsp_method *) _wrap_g_dbus_interface_skeleton_set_flags},
  {"get_properties",(nsp_method *) _wrap_g_dbus_interface_skeleton_get_properties},
  {"flush",(nsp_method *) _wrap_g_dbus_interface_skeleton_flush},
  {"unexport",(nsp_method *) _wrap_g_dbus_interface_skeleton_unexport},
  {"get_connections",(nsp_method *) _wrap_g_dbus_interface_skeleton_get_connections},
  {"get_object_path",(nsp_method *) _wrap_g_dbus_interface_skeleton_get_object_path},
  { NULL, NULL}
};

static NspMethods *gdbusinterfaceskeleton_get_methods(void) { return gdbusinterfaceskeleton_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusinterfaceskeleton_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusObjectManagerClient ----------- */


#define  NspGDBusObjectManagerClient_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusobjectmanagerclient.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusObjectManagerClient inherits from GObject 
 */

int nsp_type_gdbusobjectmanagerclient_id=0;
NspTypeGDBusObjectManagerClient *nsp_type_gdbusobjectmanagerclient=NULL;

/*
 * Type object for NspGDBusObjectManagerClient 
 * all the instance of NspTypeGDBusObjectManagerClient share the same id. 
 * nsp_type_gdbusobjectmanagerclient: is an instance of NspTypeGDBusObjectManagerClient 
 *    used for objects of NspGDBusObjectManagerClient type (i.e built with new_gdbusobjectmanagerclient) 
 * other instances are used for derived classes 
 */
NspTypeGDBusObjectManagerClient *new_type_gdbusobjectmanagerclient(type_mode mode)
{
  NspTypeGDBusObjectManagerClient *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusobjectmanagerclient != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusobjectmanagerclient;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusobjectmanagerclient_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusobjectmanagerclient_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusobjectmanagerclient;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusobjectmanagerclient */ 

  top->s_type =  (s_type_func *) nsp_gdbusobjectmanagerclient_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusobjectmanagerclient_type_short_string;
  /* top->create = (create_func*) int_gdbusobjectmanagerclient_create;*/

  /* specific methods for gdbusobjectmanagerclient */

  type->init = (init_func *) init_gdbusobjectmanagerclient;

  /* 
   * NspGDBusObjectManagerClient interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusobjectmanagerclient_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusObjectManagerClient called nsp_type_gdbusobjectmanagerclient
       */
      type->id =  nsp_type_gdbusobjectmanagerclient_id = nsp_new_type_id();
      nsp_type_gdbusobjectmanagerclient = type;
      if ( nsp_register_type(nsp_type_gdbusobjectmanagerclient) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusobjectmanagerclient, G_TYPE_DBUS_OBJECT_MANAGER_CLIENT);
      return ( mode == T_BASE ) ? type : new_type_gdbusobjectmanagerclient(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusobjectmanagerclient_id;
      return type;
    }
}

/*
 * initialize NspGDBusObjectManagerClient instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusobjectmanagerclient(NspGDBusObjectManagerClient *Obj,NspTypeGDBusObjectManagerClient *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusObjectManagerClient 
 */

NspGDBusObjectManagerClient *new_gdbusobjectmanagerclient() 
{
  NspGDBusObjectManagerClient *loc;
  /* type must exists */
  nsp_type_gdbusobjectmanagerclient = new_type_gdbusobjectmanagerclient(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusObjectManagerClient)))== NULLGDBUSOBJECTMANAGERCLIENT) return loc;
  /* initialize object */
  if ( init_gdbusobjectmanagerclient(loc,nsp_type_gdbusobjectmanagerclient) == FAIL) return NULLGDBUSOBJECTMANAGERCLIENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusObjectManagerClient 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusobjectmanagerclient_type_name[]="GDBusObjectManagerClient";
static char gdbusobjectmanagerclient_short_type_name[]="GDBusObjectManagerClient";

static char *nsp_gdbusobjectmanagerclient_type_as_string(void)
{
  return(gdbusobjectmanagerclient_type_name);
}

static char *nsp_gdbusobjectmanagerclient_type_short_string(NspObject *v)
{
  return(gdbusobjectmanagerclient_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusObjectManagerClient objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusObjectManagerClient   *nsp_gdbusobjectmanagerclient_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdbusobjectmanagerclient_id)  == TRUE  ) return ((NspGDBusObjectManagerClient *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusobjectmanagerclient));
  return NULL;
}

int IsGDBusObjectManagerClientObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdbusobjectmanagerclient_id);
}

int IsGDBusObjectManagerClient(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdbusobjectmanagerclient_id);
}

NspGDBusObjectManagerClient  *GetGDBusObjectManagerClientCopy(Stack stack, int i)
{
  if (  GetGDBusObjectManagerClient(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusObjectManagerClient  *GetGDBusObjectManagerClient(Stack stack, int i)
{
  NspGDBusObjectManagerClient *M;
  if (( M = nsp_gdbusobjectmanagerclient_object(NthObj(i))) == NULLGDBUSOBJECTMANAGERCLIENT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusObjectManagerClient *gdbusobjectmanagerclient_copy(NspGDBusObjectManagerClient *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectmanagerclient);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectmanagerclient);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusObjectManagerClient
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_object_manager_client_get_flags(NspGDBusObjectManagerClient *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =g_dbus_object_manager_client_get_flags(G_DBUS_OBJECT_MANAGER_CLIENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_object_manager_client_get_name(NspGDBusObjectManagerClient *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_object_manager_client_get_name(G_DBUS_OBJECT_MANAGER_CLIENT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_object_manager_client_get_name_owner(NspGDBusObjectManagerClient *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_object_manager_client_get_name_owner(G_DBUS_OBJECT_MANAGER_CLIENT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods gdbusobjectmanagerclient_methods[] = {
  {"get_flags",(nsp_method *) _wrap_g_dbus_object_manager_client_get_flags},
  {"get_name",(nsp_method *) _wrap_g_dbus_object_manager_client_get_name},
  {"get_name_owner",(nsp_method *) _wrap_g_dbus_object_manager_client_get_name_owner},
  { NULL, NULL}
};

static NspMethods *gdbusobjectmanagerclient_get_methods(void) { return gdbusobjectmanagerclient_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusobjectmanagerclient_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusObjectManagerServer ----------- */


#define  NspGDBusObjectManagerServer_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusobjectmanagerserver.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusObjectManagerServer inherits from GObject 
 */

int nsp_type_gdbusobjectmanagerserver_id=0;
NspTypeGDBusObjectManagerServer *nsp_type_gdbusobjectmanagerserver=NULL;

/*
 * Type object for NspGDBusObjectManagerServer 
 * all the instance of NspTypeGDBusObjectManagerServer share the same id. 
 * nsp_type_gdbusobjectmanagerserver: is an instance of NspTypeGDBusObjectManagerServer 
 *    used for objects of NspGDBusObjectManagerServer type (i.e built with new_gdbusobjectmanagerserver) 
 * other instances are used for derived classes 
 */
NspTypeGDBusObjectManagerServer *new_type_gdbusobjectmanagerserver(type_mode mode)
{
  NspTypeGDBusObjectManagerServer *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusobjectmanagerserver != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusobjectmanagerserver;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusobjectmanagerserver_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusobjectmanagerserver_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusobjectmanagerserver;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusobjectmanagerserver */ 

  top->s_type =  (s_type_func *) nsp_gdbusobjectmanagerserver_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusobjectmanagerserver_type_short_string;
  /* top->create = (create_func*) int_gdbusobjectmanagerserver_create;*/

  /* specific methods for gdbusobjectmanagerserver */

  type->init = (init_func *) init_gdbusobjectmanagerserver;

  /* 
   * NspGDBusObjectManagerServer interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusobjectmanagerserver_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusObjectManagerServer called nsp_type_gdbusobjectmanagerserver
       */
      type->id =  nsp_type_gdbusobjectmanagerserver_id = nsp_new_type_id();
      nsp_type_gdbusobjectmanagerserver = type;
      if ( nsp_register_type(nsp_type_gdbusobjectmanagerserver) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusobjectmanagerserver, G_TYPE_DBUS_OBJECT_MANAGER_SERVER);
      return ( mode == T_BASE ) ? type : new_type_gdbusobjectmanagerserver(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusobjectmanagerserver_id;
      return type;
    }
}

/*
 * initialize NspGDBusObjectManagerServer instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusobjectmanagerserver(NspGDBusObjectManagerServer *Obj,NspTypeGDBusObjectManagerServer *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusObjectManagerServer 
 */

NspGDBusObjectManagerServer *new_gdbusobjectmanagerserver() 
{
  NspGDBusObjectManagerServer *loc;
  /* type must exists */
  nsp_type_gdbusobjectmanagerserver = new_type_gdbusobjectmanagerserver(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusObjectManagerServer)))== NULLGDBUSOBJECTMANAGERSERVER) return loc;
  /* initialize object */
  if ( init_gdbusobjectmanagerserver(loc,nsp_type_gdbusobjectmanagerserver) == FAIL) return NULLGDBUSOBJECTMANAGERSERVER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusObjectManagerServer 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusobjectmanagerserver_type_name[]="GDBusObjectManagerServer";
static char gdbusobjectmanagerserver_short_type_name[]="GDBusObjectManagerServer";

static char *nsp_gdbusobjectmanagerserver_type_as_string(void)
{
  return(gdbusobjectmanagerserver_type_name);
}

static char *nsp_gdbusobjectmanagerserver_type_short_string(NspObject *v)
{
  return(gdbusobjectmanagerserver_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusObjectManagerServer objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusObjectManagerServer   *nsp_gdbusobjectmanagerserver_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdbusobjectmanagerserver_id)  == TRUE  ) return ((NspGDBusObjectManagerServer *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusobjectmanagerserver));
  return NULL;
}

int IsGDBusObjectManagerServerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdbusobjectmanagerserver_id);
}

int IsGDBusObjectManagerServer(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdbusobjectmanagerserver_id);
}

NspGDBusObjectManagerServer  *GetGDBusObjectManagerServerCopy(Stack stack, int i)
{
  if (  GetGDBusObjectManagerServer(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusObjectManagerServer  *GetGDBusObjectManagerServer(Stack stack, int i)
{
  NspGDBusObjectManagerServer *M;
  if (( M = nsp_gdbusobjectmanagerserver_object(NthObj(i))) == NULLGDBUSOBJECTMANAGERSERVER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusObjectManagerServer *gdbusobjectmanagerserver_copy(NspGDBusObjectManagerServer *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectmanagerserver);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectmanagerserver);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusObjectManagerServer
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_object_manager_server_export(NspGDBusObjectManagerServer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *object;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdbusobjectskeleton, &object) == FAIL) return RET_BUG;
    g_dbus_object_manager_server_export(G_DBUS_OBJECT_MANAGER_SERVER(self->obj),G_DBUS_OBJECT_SKELETON(object->obj));
  return 0;
}

static int _wrap_g_dbus_object_manager_server_export_uniquely(NspGDBusObjectManagerServer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *object;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdbusobjectskeleton, &object) == FAIL) return RET_BUG;
    g_dbus_object_manager_server_export_uniquely(G_DBUS_OBJECT_MANAGER_SERVER(self->obj),G_DBUS_OBJECT_SKELETON(object->obj));
  return 0;
}

static int _wrap_g_dbus_object_manager_server_is_exported(NspGDBusObjectManagerServer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *object;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdbusobjectskeleton, &object) == FAIL) return RET_BUG;
    ret =g_dbus_object_manager_server_is_exported(G_DBUS_OBJECT_MANAGER_SERVER(self->obj),G_DBUS_OBJECT_SKELETON(object->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_object_manager_server_unexport(NspGDBusObjectManagerServer *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *object_path;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&object_path) == FAIL) return RET_BUG;
    ret =g_dbus_object_manager_server_unexport(G_DBUS_OBJECT_MANAGER_SERVER(self->obj),object_path);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdbusobjectmanagerserver_methods[] = {
  {"export",(nsp_method *) _wrap_g_dbus_object_manager_server_export},
  {"export_uniquely",(nsp_method *) _wrap_g_dbus_object_manager_server_export_uniquely},
  {"is_exported",(nsp_method *) _wrap_g_dbus_object_manager_server_is_exported},
  {"unexport",(nsp_method *) _wrap_g_dbus_object_manager_server_unexport},
  { NULL, NULL}
};

static NspMethods *gdbusobjectmanagerserver_get_methods(void) { return gdbusobjectmanagerserver_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusobjectmanagerserver_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusObjectProxy ----------- */


#define  NspGDBusObjectProxy_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusobjectproxy.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusObjectProxy inherits from GObject 
 */

int nsp_type_gdbusobjectproxy_id=0;
NspTypeGDBusObjectProxy *nsp_type_gdbusobjectproxy=NULL;

/*
 * Type object for NspGDBusObjectProxy 
 * all the instance of NspTypeGDBusObjectProxy share the same id. 
 * nsp_type_gdbusobjectproxy: is an instance of NspTypeGDBusObjectProxy 
 *    used for objects of NspGDBusObjectProxy type (i.e built with new_gdbusobjectproxy) 
 * other instances are used for derived classes 
 */
NspTypeGDBusObjectProxy *new_type_gdbusobjectproxy(type_mode mode)
{
  NspTypeGDBusObjectProxy *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusobjectproxy != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusobjectproxy;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusobjectproxy_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusobjectproxy_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusobjectproxy;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusobjectproxy */ 

  top->s_type =  (s_type_func *) nsp_gdbusobjectproxy_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusobjectproxy_type_short_string;
  /* top->create = (create_func*) int_gdbusobjectproxy_create;*/

  /* specific methods for gdbusobjectproxy */

  type->init = (init_func *) init_gdbusobjectproxy;

  /* 
   * NspGDBusObjectProxy interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusobjectproxy_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusObjectProxy called nsp_type_gdbusobjectproxy
       */
      type->id =  nsp_type_gdbusobjectproxy_id = nsp_new_type_id();
      nsp_type_gdbusobjectproxy = type;
      if ( nsp_register_type(nsp_type_gdbusobjectproxy) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusobjectproxy, G_TYPE_DBUS_OBJECT_PROXY);
      return ( mode == T_BASE ) ? type : new_type_gdbusobjectproxy(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusobjectproxy_id;
      return type;
    }
}

/*
 * initialize NspGDBusObjectProxy instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusobjectproxy(NspGDBusObjectProxy *Obj,NspTypeGDBusObjectProxy *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusObjectProxy 
 */

NspGDBusObjectProxy *new_gdbusobjectproxy() 
{
  NspGDBusObjectProxy *loc;
  /* type must exists */
  nsp_type_gdbusobjectproxy = new_type_gdbusobjectproxy(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusObjectProxy)))== NULLGDBUSOBJECTPROXY) return loc;
  /* initialize object */
  if ( init_gdbusobjectproxy(loc,nsp_type_gdbusobjectproxy) == FAIL) return NULLGDBUSOBJECTPROXY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusObjectProxy 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusobjectproxy_type_name[]="GDBusObjectProxy";
static char gdbusobjectproxy_short_type_name[]="GDBusObjectProxy";

static char *nsp_gdbusobjectproxy_type_as_string(void)
{
  return(gdbusobjectproxy_type_name);
}

static char *nsp_gdbusobjectproxy_type_short_string(NspObject *v)
{
  return(gdbusobjectproxy_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusObjectProxy objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusObjectProxy   *nsp_gdbusobjectproxy_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdbusobjectproxy_id)  == TRUE  ) return ((NspGDBusObjectProxy *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusobjectproxy));
  return NULL;
}

int IsGDBusObjectProxyObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdbusobjectproxy_id);
}

int IsGDBusObjectProxy(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdbusobjectproxy_id);
}

NspGDBusObjectProxy  *GetGDBusObjectProxyCopy(Stack stack, int i)
{
  if (  GetGDBusObjectProxy(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusObjectProxy  *GetGDBusObjectProxy(Stack stack, int i)
{
  NspGDBusObjectProxy *M;
  if (( M = nsp_gdbusobjectproxy_object(NthObj(i))) == NULLGDBUSOBJECTPROXY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusObjectProxy *gdbusobjectproxy_copy(NspGDBusObjectProxy *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectproxy);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectproxy);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusObjectProxy
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gdbusobjectproxy_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusobjectproxy_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusObjectSkeleton ----------- */


#define  NspGDBusObjectSkeleton_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusobjectskeleton.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusObjectSkeleton inherits from GObject 
 */

int nsp_type_gdbusobjectskeleton_id=0;
NspTypeGDBusObjectSkeleton *nsp_type_gdbusobjectskeleton=NULL;

/*
 * Type object for NspGDBusObjectSkeleton 
 * all the instance of NspTypeGDBusObjectSkeleton share the same id. 
 * nsp_type_gdbusobjectskeleton: is an instance of NspTypeGDBusObjectSkeleton 
 *    used for objects of NspGDBusObjectSkeleton type (i.e built with new_gdbusobjectskeleton) 
 * other instances are used for derived classes 
 */
NspTypeGDBusObjectSkeleton *new_type_gdbusobjectskeleton(type_mode mode)
{
  NspTypeGDBusObjectSkeleton *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusobjectskeleton != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusobjectskeleton;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusobjectskeleton_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusobjectskeleton_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusobjectskeleton;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusobjectskeleton */ 

  top->s_type =  (s_type_func *) nsp_gdbusobjectskeleton_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusobjectskeleton_type_short_string;
  /* top->create = (create_func*) int_gdbusobjectskeleton_create;*/

  /* specific methods for gdbusobjectskeleton */

  type->init = (init_func *) init_gdbusobjectskeleton;

  /* 
   * NspGDBusObjectSkeleton interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusobjectskeleton_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusObjectSkeleton called nsp_type_gdbusobjectskeleton
       */
      type->id =  nsp_type_gdbusobjectskeleton_id = nsp_new_type_id();
      nsp_type_gdbusobjectskeleton = type;
      if ( nsp_register_type(nsp_type_gdbusobjectskeleton) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusobjectskeleton, G_TYPE_DBUS_OBJECT_SKELETON);
      return ( mode == T_BASE ) ? type : new_type_gdbusobjectskeleton(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusobjectskeleton_id;
      return type;
    }
}

/*
 * initialize NspGDBusObjectSkeleton instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusobjectskeleton(NspGDBusObjectSkeleton *Obj,NspTypeGDBusObjectSkeleton *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusObjectSkeleton 
 */

NspGDBusObjectSkeleton *new_gdbusobjectskeleton() 
{
  NspGDBusObjectSkeleton *loc;
  /* type must exists */
  nsp_type_gdbusobjectskeleton = new_type_gdbusobjectskeleton(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusObjectSkeleton)))== NULLGDBUSOBJECTSKELETON) return loc;
  /* initialize object */
  if ( init_gdbusobjectskeleton(loc,nsp_type_gdbusobjectskeleton) == FAIL) return NULLGDBUSOBJECTSKELETON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusObjectSkeleton 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusobjectskeleton_type_name[]="GDBusObjectSkeleton";
static char gdbusobjectskeleton_short_type_name[]="GDBusObjectSkeleton";

static char *nsp_gdbusobjectskeleton_type_as_string(void)
{
  return(gdbusobjectskeleton_type_name);
}

static char *nsp_gdbusobjectskeleton_type_short_string(NspObject *v)
{
  return(gdbusobjectskeleton_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusObjectSkeleton objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusObjectSkeleton   *nsp_gdbusobjectskeleton_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdbusobjectskeleton_id)  == TRUE  ) return ((NspGDBusObjectSkeleton *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusobjectskeleton));
  return NULL;
}

int IsGDBusObjectSkeletonObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdbusobjectskeleton_id);
}

int IsGDBusObjectSkeleton(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdbusobjectskeleton_id);
}

NspGDBusObjectSkeleton  *GetGDBusObjectSkeletonCopy(Stack stack, int i)
{
  if (  GetGDBusObjectSkeleton(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusObjectSkeleton  *GetGDBusObjectSkeleton(Stack stack, int i)
{
  NspGDBusObjectSkeleton *M;
  if (( M = nsp_gdbusobjectskeleton_object(NthObj(i))) == NULLGDBUSOBJECTSKELETON)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusObjectSkeleton *gdbusobjectskeleton_copy(NspGDBusObjectSkeleton *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectskeleton);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusobjectskeleton);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusObjectSkeleton
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_object_skeleton_flush(NspGDBusObjectSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_dbus_object_skeleton_flush(G_DBUS_OBJECT_SKELETON(self->obj));
  return 0;
}

static int _wrap_g_dbus_object_skeleton_add_interface(NspGDBusObjectSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *interface_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdbusinterfaceskeleton, &interface_) == FAIL) return RET_BUG;
    g_dbus_object_skeleton_add_interface(G_DBUS_OBJECT_SKELETON(self->obj),G_DBUS_INTERFACE_SKELETON(interface_->obj));
  return 0;
}

static int _wrap_g_dbus_object_skeleton_remove_interface(NspGDBusObjectSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *interface_;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gdbusinterfaceskeleton, &interface_) == FAIL) return RET_BUG;
    g_dbus_object_skeleton_remove_interface(G_DBUS_OBJECT_SKELETON(self->obj),G_DBUS_INTERFACE_SKELETON(interface_->obj));
  return 0;
}

static int _wrap_g_dbus_object_skeleton_remove_interface_by_name(NspGDBusObjectSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *interface_name;
  if ( GetArgs(stack,rhs,opt,T,&interface_name) == FAIL) return RET_BUG;
    g_dbus_object_skeleton_remove_interface_by_name(G_DBUS_OBJECT_SKELETON(self->obj),interface_name);
  return 0;
}

static int _wrap_g_dbus_object_skeleton_set_object_path(NspGDBusObjectSkeleton *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *object_path;
  if ( GetArgs(stack,rhs,opt,T,&object_path) == FAIL) return RET_BUG;
    g_dbus_object_skeleton_set_object_path(G_DBUS_OBJECT_SKELETON(self->obj),object_path);
  return 0;
}

static NspMethods gdbusobjectskeleton_methods[] = {
  {"flush",(nsp_method *) _wrap_g_dbus_object_skeleton_flush},
  {"add_interface",(nsp_method *) _wrap_g_dbus_object_skeleton_add_interface},
  {"remove_interface",(nsp_method *) _wrap_g_dbus_object_skeleton_remove_interface},
  {"remove_interface_by_name",(nsp_method *) _wrap_g_dbus_object_skeleton_remove_interface_by_name},
  {"set_object_path",(nsp_method *) _wrap_g_dbus_object_skeleton_set_object_path},
  { NULL, NULL}
};

static NspMethods *gdbusobjectskeleton_get_methods(void) { return gdbusobjectskeleton_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusobjectskeleton_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDBusProxy ----------- */


#define  NspGDBusProxy_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdbusproxy.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDBusProxy inherits from GObject 
 */

int nsp_type_gdbusproxy_id=0;
NspTypeGDBusProxy *nsp_type_gdbusproxy=NULL;

/*
 * Type object for NspGDBusProxy 
 * all the instance of NspTypeGDBusProxy share the same id. 
 * nsp_type_gdbusproxy: is an instance of NspTypeGDBusProxy 
 *    used for objects of NspGDBusProxy type (i.e built with new_gdbusproxy) 
 * other instances are used for derived classes 
 */
NspTypeGDBusProxy *new_type_gdbusproxy(type_mode mode)
{
  NspTypeGDBusProxy *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdbusproxy != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdbusproxy;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdbusproxy_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdbusproxy_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdbusproxy;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdbusproxy */ 

  top->s_type =  (s_type_func *) nsp_gdbusproxy_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdbusproxy_type_short_string;
  /* top->create = (create_func*) int_gdbusproxy_create;*/

  /* specific methods for gdbusproxy */

  type->init = (init_func *) init_gdbusproxy;

  /* 
   * NspGDBusProxy interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdbusproxy_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDBusProxy called nsp_type_gdbusproxy
       */
      type->id =  nsp_type_gdbusproxy_id = nsp_new_type_id();
      nsp_type_gdbusproxy = type;
      if ( nsp_register_type(nsp_type_gdbusproxy) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdbusproxy, G_TYPE_DBUS_PROXY);
      return ( mode == T_BASE ) ? type : new_type_gdbusproxy(mode);
    }
  else 
    {
      type->id = nsp_type_gdbusproxy_id;
      return type;
    }
}

/*
 * initialize NspGDBusProxy instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdbusproxy(NspGDBusProxy *Obj,NspTypeGDBusProxy *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDBusProxy 
 */

NspGDBusProxy *new_gdbusproxy() 
{
  NspGDBusProxy *loc;
  /* type must exists */
  nsp_type_gdbusproxy = new_type_gdbusproxy(T_BASE);
  if ( (loc = malloc(sizeof(NspGDBusProxy)))== NULLGDBUSPROXY) return loc;
  /* initialize object */
  if ( init_gdbusproxy(loc,nsp_type_gdbusproxy) == FAIL) return NULLGDBUSPROXY;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDBusProxy 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdbusproxy_type_name[]="GDBusProxy";
static char gdbusproxy_short_type_name[]="GDBusProxy";

static char *nsp_gdbusproxy_type_as_string(void)
{
  return(gdbusproxy_type_name);
}

static char *nsp_gdbusproxy_type_short_string(NspObject *v)
{
  return(gdbusproxy_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDBusProxy objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDBusProxy   *nsp_gdbusproxy_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdbusproxy_id)  == TRUE  ) return ((NspGDBusProxy *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdbusproxy));
  return NULL;
}

int IsGDBusProxyObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdbusproxy_id);
}

int IsGDBusProxy(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdbusproxy_id);
}

NspGDBusProxy  *GetGDBusProxyCopy(Stack stack, int i)
{
  if (  GetGDBusProxy(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDBusProxy  *GetGDBusProxy(Stack stack, int i)
{
  NspGDBusProxy *M;
  if (( M = nsp_gdbusproxy_object(NthObj(i))) == NULLGDBUSPROXY)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDBusProxy *gdbusproxy_copy(NspGDBusProxy *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusproxy);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdbusproxy);
}

/*-------------------------------------------------------------------
 * wrappers for the GDBusProxy
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_dbus_proxy_get_flags(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_flags(G_DBUS_PROXY(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_proxy_get_name(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_name(G_DBUS_PROXY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_proxy_get_name_owner(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_name_owner(G_DBUS_PROXY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_dbus_proxy_get_object_path(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_object_path(G_DBUS_PROXY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_proxy_get_interface_name(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_interface_name(G_DBUS_PROXY(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_proxy_get_default_timeout(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_default_timeout(G_DBUS_PROXY(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_dbus_proxy_set_default_timeout(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int timeout_msec;
  if ( GetArgs(stack,rhs,opt,T,&timeout_msec) == FAIL) return RET_BUG;
    g_dbus_proxy_set_default_timeout(G_DBUS_PROXY(self->obj),timeout_msec);
  return 0;
}

static int _wrap_g_dbus_proxy_get_cached_property(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *property_name;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&property_name) == FAIL) return RET_BUG;
    ret =g_dbus_proxy_get_cached_property(G_DBUS_PROXY(self->obj),property_name);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_dbus_proxy_set_cached_property(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj, t_end};
  char *property_name;
  GVariant *value = NULL;
  NspObject *nsp_value = NULL;
  if ( GetArgs(stack,rhs,opt,T,&property_name, &nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_value))
    { value = ((NspGVariant *) nsp_value)->obj->value;
      if((value = nsp_copy_GVariant(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type GVariant\n");
      return RET_BUG;
    }
    g_dbus_proxy_set_cached_property(G_DBUS_PROXY(self->obj),property_name,value);
  return 0;
}

static int _wrap_g_dbus_proxy_get_cached_property_names(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_dbus_proxy_get_cached_property_names(G_DBUS_PROXY(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_dbus_proxy_call_finish(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_dbus_proxy_call_finish(G_DBUS_PROXY(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_dbus_proxy_call_sync(NspGDBusProxy *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj,obj,s_int,obj_check, t_end};
  char *method_name;
  GVariant *parameters = NULL, *ret;
  NspObject *nsp_parameters = NULL, *nsp_flags = NULL, *nsp_ret;
  GDBusCallFlags flags;
  int timeout_msec;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&method_name, &nsp_parameters, &nsp_flags, &timeout_msec, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_parameters))
    { parameters = ((NspGVariant *) nsp_parameters)->obj->value;
      if((parameters = nsp_copy_GVariant(parameters))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: parameters should be of type GVariant\n");
      return RET_BUG;
    }
  if (nspg_flags_get_value(G_TYPE_DBUS_CALL_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_dbus_proxy_call_sync(G_DBUS_PROXY(self->obj),method_name,parameters,flags,timeout_msec,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gdbusproxy_methods[] = {
  {"get_flags",(nsp_method *) _wrap_g_dbus_proxy_get_flags},
  {"get_name",(nsp_method *) _wrap_g_dbus_proxy_get_name},
  {"get_name_owner",(nsp_method *) _wrap_g_dbus_proxy_get_name_owner},
  {"get_object_path",(nsp_method *) _wrap_g_dbus_proxy_get_object_path},
  {"get_interface_name",(nsp_method *) _wrap_g_dbus_proxy_get_interface_name},
  {"get_default_timeout",(nsp_method *) _wrap_g_dbus_proxy_get_default_timeout},
  {"set_default_timeout",(nsp_method *) _wrap_g_dbus_proxy_set_default_timeout},
  {"get_cached_property",(nsp_method *) _wrap_g_dbus_proxy_get_cached_property},
  {"set_cached_property",(nsp_method *) _wrap_g_dbus_proxy_set_cached_property},
  {"get_cached_property_names",(nsp_method *) _wrap_g_dbus_proxy_get_cached_property_names},
  {"call_finish",(nsp_method *) _wrap_g_dbus_proxy_call_finish},
  {"call_sync",(nsp_method *) _wrap_g_dbus_proxy_call_sync},
  { NULL, NULL}
};

static NspMethods *gdbusproxy_get_methods(void) { return gdbusproxy_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdbusproxy_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDrive ----------- */


#define  NspGDrive_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdrive.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDrive inherits from GObject 
 */

int nsp_type_gdrive_id=0;
NspTypeGDrive *nsp_type_gdrive=NULL;

/*
 * Type object for NspGDrive 
 * all the instance of NspTypeGDrive share the same id. 
 * nsp_type_gdrive: is an instance of NspTypeGDrive 
 *    used for objects of NspGDrive type (i.e built with new_gdrive) 
 * other instances are used for derived classes 
 */
NspTypeGDrive *new_type_gdrive(type_mode mode)
{
  NspTypeGDrive *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdrive != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdrive;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdrive_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdrive_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdrive;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdrive */ 

  top->s_type =  (s_type_func *) nsp_gdrive_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdrive_type_short_string;
  /* top->create = (create_func*) int_gdrive_create;*/

  /* specific methods for gdrive */

  type->init = (init_func *) init_gdrive;

  /* 
   * NspGDrive interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdrive_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDrive called nsp_type_gdrive
       */
      type->id =  nsp_type_gdrive_id = nsp_new_type_id();
      nsp_type_gdrive = type;
      if ( nsp_register_type(nsp_type_gdrive) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdrive, G_TYPE_DRIVE);
      return ( mode == T_BASE ) ? type : new_type_gdrive(mode);
    }
  else 
    {
      type->id = nsp_type_gdrive_id;
      return type;
    }
}

/*
 * initialize NspGDrive instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdrive(NspGDrive *Obj,NspTypeGDrive *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDrive 
 */

NspGDrive *new_gdrive() 
{
  NspGDrive *loc;
  /* type must exists */
  nsp_type_gdrive = new_type_gdrive(T_BASE);
  if ( (loc = malloc(sizeof(NspGDrive)))== NULLGDRIVE) return loc;
  /* initialize object */
  if ( init_gdrive(loc,nsp_type_gdrive) == FAIL) return NULLGDRIVE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDrive 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdrive_type_name[]="GDrive";
static char gdrive_short_type_name[]="GDrive";

static char *nsp_gdrive_type_as_string(void)
{
  return(gdrive_type_name);
}

static char *nsp_gdrive_type_short_string(NspObject *v)
{
  return(gdrive_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDrive objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDrive   *nsp_gdrive_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdrive_id)  == TRUE  ) return ((NspGDrive *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdrive));
  return NULL;
}

int IsGDriveObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdrive_id);
}

int IsGDrive(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdrive_id);
}

NspGDrive  *GetGDriveCopy(Stack stack, int i)
{
  if (  GetGDrive(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDrive  *GetGDrive(Stack stack, int i)
{
  NspGDrive *M;
  if (( M = nsp_gdrive_object(NthObj(i))) == NULLGDRIVE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDrive *gdrive_copy(NspGDrive *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdrive);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdrive);
}

/*-------------------------------------------------------------------
 * wrappers for the GDrive
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_drive_get_name(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_drive_get_name(G_DRIVE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_drive_get_icon(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_drive_get_icon(G_DRIVE(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_drive_get_symbolic_icon(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_drive_get_symbolic_icon(G_DRIVE(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_drive_has_volumes(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_has_volumes(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_get_volumes(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_drive_get_volumes(G_DRIVE(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_drive_is_media_removable(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_is_media_removable(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_has_media(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_has_media(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_is_media_check_automatic(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_is_media_check_automatic(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_can_poll_for_media(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_can_poll_for_media(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_can_eject(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_can_eject(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_poll_for_media_finish(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_drive_poll_for_media_finish(G_DRIVE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_get_identifier(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *kind;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&kind) == FAIL) return RET_BUG;
    ret =g_drive_get_identifier(G_DRIVE(self->obj),kind);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_drive_enumerate_identifiers(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_drive_enumerate_identifiers(G_DRIVE(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_drive_get_start_stop_type(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_drive_get_start_stop_type(G_DRIVE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_can_start(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_can_start(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_can_start_degraded(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_can_start_degraded(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_start_finish(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_drive_start_finish(G_DRIVE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_can_stop(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_drive_can_stop(G_DRIVE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_stop_finish(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_drive_stop_finish(G_DRIVE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_drive_eject_with_operation_finish(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_drive_eject_with_operation_finish(G_DRIVE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_drive_get_sort_key(NspGDrive *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_drive_get_sort_key(G_DRIVE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_drive_get_sort_key(Stack stack, int rhs, int opt, int lhs) /* get_sort_key */
{
  Scierror("Error: function g_drive_get_sort_key not available\n");
  return RET_BUG;
}
#endif
static NspMethods gdrive_methods[] = {
  {"get_name",(nsp_method *) _wrap_g_drive_get_name},
  {"get_icon",(nsp_method *) _wrap_g_drive_get_icon},
  {"get_symbolic_icon",(nsp_method *) _wrap_g_drive_get_symbolic_icon},
  {"has_volumes",(nsp_method *) _wrap_g_drive_has_volumes},
  {"get_volumes",(nsp_method *) _wrap_g_drive_get_volumes},
  {"is_media_removable",(nsp_method *) _wrap_g_drive_is_media_removable},
  {"has_media",(nsp_method *) _wrap_g_drive_has_media},
  {"is_media_check_automatic",(nsp_method *) _wrap_g_drive_is_media_check_automatic},
  {"can_poll_for_media",(nsp_method *) _wrap_g_drive_can_poll_for_media},
  {"can_eject",(nsp_method *) _wrap_g_drive_can_eject},
  {"poll_for_media_finish",(nsp_method *) _wrap_g_drive_poll_for_media_finish},
  {"get_identifier",(nsp_method *) _wrap_g_drive_get_identifier},
  {"enumerate_identifiers",(nsp_method *) _wrap_g_drive_enumerate_identifiers},
  {"get_start_stop_type",(nsp_method *) _wrap_g_drive_get_start_stop_type},
  {"can_start",(nsp_method *) _wrap_g_drive_can_start},
  {"can_start_degraded",(nsp_method *) _wrap_g_drive_can_start_degraded},
  {"start_finish",(nsp_method *) _wrap_g_drive_start_finish},
  {"can_stop",(nsp_method *) _wrap_g_drive_can_stop},
  {"stop_finish",(nsp_method *) _wrap_g_drive_stop_finish},
  {"eject_with_operation_finish",(nsp_method *) _wrap_g_drive_eject_with_operation_finish},
  {"get_sort_key",(nsp_method *) _wrap_g_drive_get_sort_key},
  { NULL, NULL}
};

static NspMethods *gdrive_get_methods(void) { return gdrive_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdrive_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGEmblemedIcon ----------- */


#define  NspGEmblemedIcon_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gemblemedicon.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGEmblemedIcon inherits from GObject 
 */

int nsp_type_gemblemedicon_id=0;
NspTypeGEmblemedIcon *nsp_type_gemblemedicon=NULL;

/*
 * Type object for NspGEmblemedIcon 
 * all the instance of NspTypeGEmblemedIcon share the same id. 
 * nsp_type_gemblemedicon: is an instance of NspTypeGEmblemedIcon 
 *    used for objects of NspGEmblemedIcon type (i.e built with new_gemblemedicon) 
 * other instances are used for derived classes 
 */
NspTypeGEmblemedIcon *new_type_gemblemedicon(type_mode mode)
{
  NspTypeGEmblemedIcon *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gemblemedicon != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gemblemedicon;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gemblemedicon_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gemblemedicon_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gemblemedicon;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gemblemedicon */ 

  top->s_type =  (s_type_func *) nsp_gemblemedicon_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gemblemedicon_type_short_string;
  /* top->create = (create_func*) int_gemblemedicon_create;*/

  /* specific methods for gemblemedicon */

  type->init = (init_func *) init_gemblemedicon;

  /* 
   * NspGEmblemedIcon interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gemblemedicon_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGEmblemedIcon called nsp_type_gemblemedicon
       */
      type->id =  nsp_type_gemblemedicon_id = nsp_new_type_id();
      nsp_type_gemblemedicon = type;
      if ( nsp_register_type(nsp_type_gemblemedicon) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gemblemedicon, G_TYPE_EMBLEMED_ICON);
      return ( mode == T_BASE ) ? type : new_type_gemblemedicon(mode);
    }
  else 
    {
      type->id = nsp_type_gemblemedicon_id;
      return type;
    }
}

/*
 * initialize NspGEmblemedIcon instances 
 * locally and by calling initializer on parent class 
 */

static int init_gemblemedicon(NspGEmblemedIcon *Obj,NspTypeGEmblemedIcon *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGEmblemedIcon 
 */

NspGEmblemedIcon *new_gemblemedicon() 
{
  NspGEmblemedIcon *loc;
  /* type must exists */
  nsp_type_gemblemedicon = new_type_gemblemedicon(T_BASE);
  if ( (loc = malloc(sizeof(NspGEmblemedIcon)))== NULLGEMBLEMEDICON) return loc;
  /* initialize object */
  if ( init_gemblemedicon(loc,nsp_type_gemblemedicon) == FAIL) return NULLGEMBLEMEDICON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGEmblemedIcon 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gemblemedicon_type_name[]="GEmblemedIcon";
static char gemblemedicon_short_type_name[]="GEmblemedIcon";

static char *nsp_gemblemedicon_type_as_string(void)
{
  return(gemblemedicon_type_name);
}

static char *nsp_gemblemedicon_type_short_string(NspObject *v)
{
  return(gemblemedicon_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGEmblemedIcon objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGEmblemedIcon   *nsp_gemblemedicon_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gemblemedicon_id)  == TRUE  ) return ((NspGEmblemedIcon *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gemblemedicon));
  return NULL;
}

int IsGEmblemedIconObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gemblemedicon_id);
}

int IsGEmblemedIcon(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gemblemedicon_id);
}

NspGEmblemedIcon  *GetGEmblemedIconCopy(Stack stack, int i)
{
  if (  GetGEmblemedIcon(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGEmblemedIcon  *GetGEmblemedIcon(Stack stack, int i)
{
  NspGEmblemedIcon *M;
  if (( M = nsp_gemblemedicon_object(NthObj(i))) == NULLGEMBLEMEDICON)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGEmblemedIcon *gemblemedicon_copy(NspGEmblemedIcon *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gemblemedicon);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gemblemedicon);
}

/*-------------------------------------------------------------------
 * wrappers for the GEmblemedIcon
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_emblemed_icon_get_icon(NspGEmblemedIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_emblemed_icon_get_icon(G_EMBLEMED_ICON(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_emblemed_icon_get_emblems(NspGEmblemedIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_emblemed_icon_get_emblems(G_EMBLEMED_ICON(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_emblemed_icon_clear_emblems(NspGEmblemedIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_emblemed_icon_clear_emblems(G_EMBLEMED_ICON(self->obj));
  return 0;
}

static NspMethods gemblemedicon_methods[] = {
  {"get_icon",(nsp_method *) _wrap_g_emblemed_icon_get_icon},
  {"get_emblems",(nsp_method *) _wrap_g_emblemed_icon_get_emblems},
  {"clear_emblems",(nsp_method *) _wrap_g_emblemed_icon_clear_emblems},
  { NULL, NULL}
};

static NspMethods *gemblemedicon_get_methods(void) { return gemblemedicon_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gemblemedicon_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFile ----------- */


#define  NspGFile_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfile.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFile inherits from GObject 
 */

int nsp_type_gfile_id=0;
NspTypeGFile *nsp_type_gfile=NULL;

/*
 * Type object for NspGFile 
 * all the instance of NspTypeGFile share the same id. 
 * nsp_type_gfile: is an instance of NspTypeGFile 
 *    used for objects of NspGFile type (i.e built with new_gfile) 
 * other instances are used for derived classes 
 */
NspTypeGFile *new_type_gfile(type_mode mode)
{
  NspTypeGFile *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfile != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfile;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfile_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfile_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfile;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfile */ 

  top->s_type =  (s_type_func *) nsp_gfile_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfile_type_short_string;
  /* top->create = (create_func*) int_gfile_create;*/

  /* specific methods for gfile */

  type->init = (init_func *) init_gfile;

  /* 
   * NspGFile interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfile_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFile called nsp_type_gfile
       */
      type->id =  nsp_type_gfile_id = nsp_new_type_id();
      nsp_type_gfile = type;
      if ( nsp_register_type(nsp_type_gfile) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfile, G_TYPE_FILE);
      return ( mode == T_BASE ) ? type : new_type_gfile(mode);
    }
  else 
    {
      type->id = nsp_type_gfile_id;
      return type;
    }
}

/*
 * initialize NspGFile instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfile(NspGFile *Obj,NspTypeGFile *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFile 
 */

NspGFile *new_gfile() 
{
  NspGFile *loc;
  /* type must exists */
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ( (loc = malloc(sizeof(NspGFile)))== NULLGFILE) return loc;
  /* initialize object */
  if ( init_gfile(loc,nsp_type_gfile) == FAIL) return NULLGFILE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFile 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfile_type_name[]="GFile";
static char gfile_short_type_name[]="GFile";

static char *nsp_gfile_type_as_string(void)
{
  return(gfile_type_name);
}

static char *nsp_gfile_type_short_string(NspObject *v)
{
  return(gfile_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFile objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFile   *nsp_gfile_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfile_id)  == TRUE  ) return ((NspGFile *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfile));
  return NULL;
}

int IsGFileObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfile_id);
}

int IsGFile(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfile_id);
}

NspGFile  *GetGFileCopy(Stack stack, int i)
{
  if (  GetGFile(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFile  *GetGFile(Stack stack, int i)
{
  NspGFile *M;
  if (( M = nsp_gfile_object(NthObj(i))) == NULLGFILE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFile *gfile_copy(NspGFile *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfile);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfile);
}

/*-------------------------------------------------------------------
 * wrappers for the GFile
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_file_new_for_commandline_arg_and_cwd (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string, t_end};
  char *arg, *cwd;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&arg, &cwd) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_file_new_for_commandline_arg_and_cwd(arg,cwd))== NULL) return RET_BUG;

  nsp_type_gfile = new_type_gfile(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gfile );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_file_new_for_commandline_arg (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *arg;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&arg) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_file_new_for_commandline_arg(arg))== NULL) return RET_BUG;

  nsp_type_gfile = new_type_gfile(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gfile );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_file_new_for_uri (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *uri;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_file_new_for_uri(uri))== NULL) return RET_BUG;

  nsp_type_gfile = new_type_gfile(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gfile );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_file_new_for_path (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *path;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&path) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_file_new_for_path(path))== NULL) return RET_BUG;

  nsp_type_gfile = new_type_gfile(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gfile );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_dup(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_file_dup(G_FILE(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_equal(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *file2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &file2) == FAIL) return RET_BUG;
    ret =g_file_equal(G_FILE(self->obj),G_FILE(file2->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_get_basename(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_get_basename(G_FILE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_file_get_path(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_get_path(G_FILE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_file_get_uri(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_get_uri(G_FILE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_file_get_parse_name(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_get_parse_name(G_FILE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_file_get_parent(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_file_get_parent(G_FILE(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_has_parent(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *parent;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &parent) == FAIL) return RET_BUG;
    ret =g_file_has_parent(G_FILE(self->obj),G_FILE(parent->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_get_child(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =g_file_get_child(G_FILE(self->obj),name);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_get_child_for_display_name(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *display_name;
  GError *error = NULL;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&display_name) == FAIL) return RET_BUG;
    ret =g_file_get_child_for_display_name(G_FILE(self->obj),display_name,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_has_prefix(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *prefix;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &prefix) == FAIL) return RET_BUG;
    ret =g_file_has_prefix(G_FILE(self->obj),G_FILE(prefix->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_get_relative_path(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *descendant;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &descendant) == FAIL) return RET_BUG;
    ret =g_file_get_relative_path(G_FILE(self->obj),G_FILE(descendant->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_file_resolve_relative_path(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *relative_path;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&relative_path) == FAIL) return RET_BUG;
    ret =g_file_resolve_relative_path(G_FILE(self->obj),relative_path);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_is_native(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_file_is_native(G_FILE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_has_uri_scheme(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *uri_scheme;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&uri_scheme) == FAIL) return RET_BUG;
    ret =g_file_has_uri_scheme(G_FILE(self->obj),uri_scheme);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_get_uri_scheme(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_get_uri_scheme(G_FILE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_file_read(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  GFileInputStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_read(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileinputstream = new_type_gfileinputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileinputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_read_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileInputStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_read_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileinputstream = new_type_gfileinputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileinputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_append_to(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileOutputStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_append_to(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileoutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_create(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileOutputStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_create(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileoutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_replace(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_bool,obj,obj_check, t_end};
  char *etag;
  int make_backup;
  GFileCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileOutputStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&etag, &make_backup, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_replace(G_FILE(self->obj),etag,make_backup,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileoutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_append_to_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileOutputStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_append_to_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileoutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_create_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileOutputStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_create_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileoutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_replace_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileOutputStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_replace_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileoutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_open_readwrite(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  GFileIOStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_open_readwrite(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileiostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_open_readwrite_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileIOStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_open_readwrite_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileiostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_create_readwrite(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileIOStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_create_readwrite(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileiostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_create_readwrite_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileIOStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_create_readwrite_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileiostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_replace_readwrite(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_bool,obj,obj_check, t_end};
  char *etag;
  int make_backup;
  GFileCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileIOStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&etag, &make_backup, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_replace_readwrite(G_FILE(self->obj),etag,make_backup,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileiostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_replace_readwrite_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileIOStream *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_replace_readwrite_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileiostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_query_exists(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_query_exists(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_query_file_type(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileQueryInfoFlags flags;
  NspObject *nsp_flags = NULL;
  NspGObject *cancellable;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_query_file_type(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_find_enclosing_mount(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  GMount *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_find_enclosing_mount(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gmount = new_type_gmount(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmount))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_find_enclosing_mount_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GMount *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_find_enclosing_mount_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gmount = new_type_gmount(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmount))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_enumerate_children(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj,obj_check, t_end};
  char *attributes;
  GFileQueryInfoFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileEnumerator *ret;
  if ( GetArgs(stack,rhs,opt,T,&attributes, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_enumerate_children(G_FILE(self->obj),attributes,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileenumerator = new_type_gfileenumerator(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileenumerator))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_enumerate_children_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFileEnumerator *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_enumerate_children_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfileenumerator = new_type_gfileenumerator(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfileenumerator))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_set_display_name(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *display_name;
  NspGObject *cancellable;
  GError *error = NULL;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&display_name, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_set_display_name(G_FILE(self->obj),display_name,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_set_display_name_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_set_display_name_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_delete(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_delete(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_file_delete_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_delete_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_file_delete_finish(Stack stack, int rhs, int opt, int lhs) /* delete_finish */
{
  Scierror("Error: function g_file_delete_finish not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_file_trash(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_trash(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,38,0)
static int _wrap_g_file_trash_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_trash_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_file_trash_finish(Stack stack, int rhs, int opt, int lhs) /* trash_finish */
{
  Scierror("Error: function g_file_trash_finish not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_file_copy_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *res;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res) == FAIL) return RET_BUG;
    ret =g_file_copy_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_make_directory(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_make_directory(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,38,0)
static int _wrap_g_file_make_directory_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_make_directory_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_file_make_directory_finish(Stack stack, int rhs, int opt, int lhs) /* make_directory_finish */
{
  Scierror("Error: function g_file_make_directory_finish not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_file_make_directory_with_parents(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_make_directory_with_parents(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_make_symbolic_link(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *symlink_value;
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&symlink_value, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_make_symbolic_link(G_FILE(self->obj),symlink_value,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_set_attribute_string(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,obj,obj_check, t_end};
  char *attribute, *value;
  GFileQueryInfoFlags flags;
  NspObject *nsp_flags = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&attribute, &value, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_set_attribute_string(G_FILE(self->obj),attribute,value,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_set_attribute_byte_string(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,obj,obj_check, t_end};
  char *attribute, *value;
  GFileQueryInfoFlags flags;
  NspObject *nsp_flags = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&attribute, &value, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_set_attribute_byte_string(G_FILE(self->obj),attribute,value,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_set_attribute_uint32(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,obj,obj_check, t_end};
  char *attribute;
  gulong value;
  GFileQueryInfoFlags flags;
  NspObject *nsp_flags = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&attribute, &value, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_set_attribute_uint32(G_FILE(self->obj),attribute,value,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_set_attribute_int32(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,obj,obj_check, t_end};
  char *attribute;
  int value, ret;
  GFileQueryInfoFlags flags;
  NspObject *nsp_flags = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&attribute, &value, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_QUERY_INFO_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_set_attribute_int32(G_FILE(self->obj),attribute,value,flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_mount_enclosing_volume_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_mount_enclosing_volume_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_mount_mountable_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_mount_mountable_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_unmount_mountable_with_operation_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_unmount_mountable_with_operation_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_eject_mountable_with_operation_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_eject_mountable_with_operation_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_copy_attributes(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,obj_check, t_end};
  NspGObject *destination, *cancellable;
  GFileCopyFlags flags;
  NspObject *nsp_flags = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &destination, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_COPY_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_copy_attributes(G_FILE(self->obj),G_FILE(destination->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_monitor_directory(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileMonitorFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileMonitor *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_MONITOR_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_monitor_directory(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfilemonitor = new_type_gfilemonitor(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfilemonitor))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_monitor_file(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileMonitorFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileMonitor *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_MONITOR_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_monitor_file(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfilemonitor = new_type_gfilemonitor(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfilemonitor))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_monitor(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj,obj_check, t_end};
  GFileMonitorFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GFileMonitor *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_MONITOR_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_file_monitor(G_FILE(self->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gfilemonitor = new_type_gfilemonitor(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfilemonitor))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_start_mountable_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_start_mountable_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_stop_mountable_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_stop_mountable_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_poll_mountable_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_poll_mountable_finish(G_FILE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_query_default_handler(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  GAppInfo *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_query_default_handler(G_FILE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gappinfo = new_type_gappinfo(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gappinfo))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_file_replace_contents(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,string,s_bool,obj,obj,obj_check, t_end};
  char *contents, *etag;
  int length, make_backup, ret;
  GFileCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_new_etag = NULL;
  gchar **new_etag = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&contents, &length, &etag, &make_backup, &nsp_flags, &nsp_new_etag, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_FILE_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
  if ( IsSMat(nsp_new_etag))
    { new_etag =  ((NspSMatrix *) nsp_new_etag)->S;}
  else
    {
      Scierror("Error: new_etag should be of type SMat");
      return RET_BUG;
    }
    ret =g_file_replace_contents(G_FILE(self->obj),contents,length,etag,make_backup,flags,new_etag,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_replace_contents_finish(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *res;
  gchar **new_etag = NULL;
  NspObject *nsp_new_etag = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res, &nsp_new_etag) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_new_etag))
    { new_etag =  ((NspSMatrix *) nsp_new_etag)->S;}
  else
    {
      Scierror("Error: new_etag should be of type SMat");
      return RET_BUG;
    }
    ret =g_file_replace_contents_finish(G_FILE(self->obj),G_ASYNC_RESULT(res->obj),new_etag,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_supports_thread_contexts(NspGFile *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_file_supports_thread_contexts(G_FILE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gfile_methods[] = {
  {"dup",(nsp_method *) _wrap_g_file_dup},
  {"equal",(nsp_method *) _wrap_g_file_equal},
  {"get_basename",(nsp_method *) _wrap_g_file_get_basename},
  {"get_path",(nsp_method *) _wrap_g_file_get_path},
  {"get_uri",(nsp_method *) _wrap_g_file_get_uri},
  {"get_parse_name",(nsp_method *) _wrap_g_file_get_parse_name},
  {"get_parent",(nsp_method *) _wrap_g_file_get_parent},
  {"has_parent",(nsp_method *) _wrap_g_file_has_parent},
  {"get_child",(nsp_method *) _wrap_g_file_get_child},
  {"get_child_for_display_name",(nsp_method *) _wrap_g_file_get_child_for_display_name},
  {"has_prefix",(nsp_method *) _wrap_g_file_has_prefix},
  {"get_relative_path",(nsp_method *) _wrap_g_file_get_relative_path},
  {"resolve_relative_path",(nsp_method *) _wrap_g_file_resolve_relative_path},
  {"is_native",(nsp_method *) _wrap_g_file_is_native},
  {"has_uri_scheme",(nsp_method *) _wrap_g_file_has_uri_scheme},
  {"get_uri_scheme",(nsp_method *) _wrap_g_file_get_uri_scheme},
  {"read",(nsp_method *) _wrap_g_file_read},
  {"read_finish",(nsp_method *) _wrap_g_file_read_finish},
  {"append_to",(nsp_method *) _wrap_g_file_append_to},
  {"create",(nsp_method *) _wrap_g_file_create},
  {"replace",(nsp_method *) _wrap_g_file_replace},
  {"append_to_finish",(nsp_method *) _wrap_g_file_append_to_finish},
  {"create_finish",(nsp_method *) _wrap_g_file_create_finish},
  {"replace_finish",(nsp_method *) _wrap_g_file_replace_finish},
  {"open_readwrite",(nsp_method *) _wrap_g_file_open_readwrite},
  {"open_readwrite_finish",(nsp_method *) _wrap_g_file_open_readwrite_finish},
  {"create_readwrite",(nsp_method *) _wrap_g_file_create_readwrite},
  {"create_readwrite_finish",(nsp_method *) _wrap_g_file_create_readwrite_finish},
  {"replace_readwrite",(nsp_method *) _wrap_g_file_replace_readwrite},
  {"replace_readwrite_finish",(nsp_method *) _wrap_g_file_replace_readwrite_finish},
  {"query_exists",(nsp_method *) _wrap_g_file_query_exists},
  {"query_file_type",(nsp_method *) _wrap_g_file_query_file_type},
  {"find_enclosing_mount",(nsp_method *) _wrap_g_file_find_enclosing_mount},
  {"find_enclosing_mount_finish",(nsp_method *) _wrap_g_file_find_enclosing_mount_finish},
  {"enumerate_children",(nsp_method *) _wrap_g_file_enumerate_children},
  {"enumerate_children_finish",(nsp_method *) _wrap_g_file_enumerate_children_finish},
  {"set_display_name",(nsp_method *) _wrap_g_file_set_display_name},
  {"set_display_name_finish",(nsp_method *) _wrap_g_file_set_display_name_finish},
  {"delete",(nsp_method *) _wrap_g_file_delete},
  {"delete_finish",(nsp_method *) _wrap_g_file_delete_finish},
  {"trash",(nsp_method *) _wrap_g_file_trash},
  {"trash_finish",(nsp_method *) _wrap_g_file_trash_finish},
  {"copy_finish",(nsp_method *) _wrap_g_file_copy_finish},
  {"make_directory",(nsp_method *) _wrap_g_file_make_directory},
  {"make_directory_finish",(nsp_method *) _wrap_g_file_make_directory_finish},
  {"make_directory_with_parents",(nsp_method *) _wrap_g_file_make_directory_with_parents},
  {"make_symbolic_link",(nsp_method *) _wrap_g_file_make_symbolic_link},
  {"set_attribute_string",(nsp_method *) _wrap_g_file_set_attribute_string},
  {"set_attribute_byte_string",(nsp_method *) _wrap_g_file_set_attribute_byte_string},
  {"set_attribute_uint32",(nsp_method *) _wrap_g_file_set_attribute_uint32},
  {"set_attribute_int32",(nsp_method *) _wrap_g_file_set_attribute_int32},
  {"mount_enclosing_volume_finish",(nsp_method *) _wrap_g_file_mount_enclosing_volume_finish},
  {"mount_mountable_finish",(nsp_method *) _wrap_g_file_mount_mountable_finish},
  {"unmount_mountable_with_operation_finish",(nsp_method *) _wrap_g_file_unmount_mountable_with_operation_finish},
  {"eject_mountable_with_operation_finish",(nsp_method *) _wrap_g_file_eject_mountable_with_operation_finish},
  {"copy_attributes",(nsp_method *) _wrap_g_file_copy_attributes},
  {"monitor_directory",(nsp_method *) _wrap_g_file_monitor_directory},
  {"monitor_file",(nsp_method *) _wrap_g_file_monitor_file},
  {"monitor",(nsp_method *) _wrap_g_file_monitor},
  {"start_mountable_finish",(nsp_method *) _wrap_g_file_start_mountable_finish},
  {"stop_mountable_finish",(nsp_method *) _wrap_g_file_stop_mountable_finish},
  {"poll_mountable_finish",(nsp_method *) _wrap_g_file_poll_mountable_finish},
  {"query_default_handler",(nsp_method *) _wrap_g_file_query_default_handler},
  {"replace_contents",(nsp_method *) _wrap_g_file_replace_contents},
  {"replace_contents_finish",(nsp_method *) _wrap_g_file_replace_contents_finish},
  {"supports_thread_contexts",(nsp_method *) _wrap_g_file_supports_thread_contexts},
  { NULL, NULL}
};

static NspMethods *gfile_get_methods(void) { return gfile_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfile_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFileEnumerator ----------- */


#define  NspGFileEnumerator_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfileenumerator.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFileEnumerator inherits from GObject 
 */

int nsp_type_gfileenumerator_id=0;
NspTypeGFileEnumerator *nsp_type_gfileenumerator=NULL;

/*
 * Type object for NspGFileEnumerator 
 * all the instance of NspTypeGFileEnumerator share the same id. 
 * nsp_type_gfileenumerator: is an instance of NspTypeGFileEnumerator 
 *    used for objects of NspGFileEnumerator type (i.e built with new_gfileenumerator) 
 * other instances are used for derived classes 
 */
NspTypeGFileEnumerator *new_type_gfileenumerator(type_mode mode)
{
  NspTypeGFileEnumerator *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfileenumerator != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfileenumerator;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfileenumerator_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfileenumerator_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfileenumerator;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfileenumerator */ 

  top->s_type =  (s_type_func *) nsp_gfileenumerator_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfileenumerator_type_short_string;
  /* top->create = (create_func*) int_gfileenumerator_create;*/

  /* specific methods for gfileenumerator */

  type->init = (init_func *) init_gfileenumerator;

  /* 
   * NspGFileEnumerator interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfileenumerator_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFileEnumerator called nsp_type_gfileenumerator
       */
      type->id =  nsp_type_gfileenumerator_id = nsp_new_type_id();
      nsp_type_gfileenumerator = type;
      if ( nsp_register_type(nsp_type_gfileenumerator) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfileenumerator, G_TYPE_FILE_ENUMERATOR);
      return ( mode == T_BASE ) ? type : new_type_gfileenumerator(mode);
    }
  else 
    {
      type->id = nsp_type_gfileenumerator_id;
      return type;
    }
}

/*
 * initialize NspGFileEnumerator instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfileenumerator(NspGFileEnumerator *Obj,NspTypeGFileEnumerator *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFileEnumerator 
 */

NspGFileEnumerator *new_gfileenumerator() 
{
  NspGFileEnumerator *loc;
  /* type must exists */
  nsp_type_gfileenumerator = new_type_gfileenumerator(T_BASE);
  if ( (loc = malloc(sizeof(NspGFileEnumerator)))== NULLGFILEENUMERATOR) return loc;
  /* initialize object */
  if ( init_gfileenumerator(loc,nsp_type_gfileenumerator) == FAIL) return NULLGFILEENUMERATOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFileEnumerator 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfileenumerator_type_name[]="GFileEnumerator";
static char gfileenumerator_short_type_name[]="GFileEnumerator";

static char *nsp_gfileenumerator_type_as_string(void)
{
  return(gfileenumerator_type_name);
}

static char *nsp_gfileenumerator_type_short_string(NspObject *v)
{
  return(gfileenumerator_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFileEnumerator objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFileEnumerator   *nsp_gfileenumerator_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfileenumerator_id)  == TRUE  ) return ((NspGFileEnumerator *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfileenumerator));
  return NULL;
}

int IsGFileEnumeratorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfileenumerator_id);
}

int IsGFileEnumerator(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfileenumerator_id);
}

NspGFileEnumerator  *GetGFileEnumeratorCopy(Stack stack, int i)
{
  if (  GetGFileEnumerator(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFileEnumerator  *GetGFileEnumerator(Stack stack, int i)
{
  NspGFileEnumerator *M;
  if (( M = nsp_gfileenumerator_object(NthObj(i))) == NULLGFILEENUMERATOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFileEnumerator *gfileenumerator_copy(NspGFileEnumerator *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileenumerator);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileenumerator);
}

/*-------------------------------------------------------------------
 * wrappers for the GFileEnumerator
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_file_enumerator_close(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_file_enumerator_close(G_FILE_ENUMERATOR(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_enumerator_next_files_finish(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_enumerator_next_files_finish(G_FILE_ENUMERATOR(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_file_enumerator_close_finish(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_file_enumerator_close_finish(G_FILE_ENUMERATOR(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_enumerator_is_closed(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_file_enumerator_is_closed(G_FILE_ENUMERATOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_enumerator_has_pending(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_file_enumerator_has_pending(G_FILE_ENUMERATOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_enumerator_set_pending(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int pending;
  if ( GetArgs(stack,rhs,opt,T,&pending) == FAIL) return RET_BUG;
    g_file_enumerator_set_pending(G_FILE_ENUMERATOR(self->obj),pending);
  return 0;
}

static int _wrap_g_file_enumerator_get_container(NspGFileEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_file_enumerator_get_container(G_FILE_ENUMERATOR(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gfileenumerator_methods[] = {
  {"close",(nsp_method *) _wrap_g_file_enumerator_close},
  {"next_files_finish",(nsp_method *) _wrap_g_file_enumerator_next_files_finish},
  {"close_finish",(nsp_method *) _wrap_g_file_enumerator_close_finish},
  {"is_closed",(nsp_method *) _wrap_g_file_enumerator_is_closed},
  {"has_pending",(nsp_method *) _wrap_g_file_enumerator_has_pending},
  {"set_pending",(nsp_method *) _wrap_g_file_enumerator_set_pending},
  {"get_container",(nsp_method *) _wrap_g_file_enumerator_get_container},
  { NULL, NULL}
};

static NspMethods *gfileenumerator_get_methods(void) { return gfileenumerator_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfileenumerator_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFileMonitor ----------- */


#define  NspGFileMonitor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfilemonitor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFileMonitor inherits from GObject 
 */

int nsp_type_gfilemonitor_id=0;
NspTypeGFileMonitor *nsp_type_gfilemonitor=NULL;

/*
 * Type object for NspGFileMonitor 
 * all the instance of NspTypeGFileMonitor share the same id. 
 * nsp_type_gfilemonitor: is an instance of NspTypeGFileMonitor 
 *    used for objects of NspGFileMonitor type (i.e built with new_gfilemonitor) 
 * other instances are used for derived classes 
 */
NspTypeGFileMonitor *new_type_gfilemonitor(type_mode mode)
{
  NspTypeGFileMonitor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfilemonitor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfilemonitor;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfilemonitor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfilemonitor_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfilemonitor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfilemonitor */ 

  top->s_type =  (s_type_func *) nsp_gfilemonitor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfilemonitor_type_short_string;
  /* top->create = (create_func*) int_gfilemonitor_create;*/

  /* specific methods for gfilemonitor */

  type->init = (init_func *) init_gfilemonitor;

  /* 
   * NspGFileMonitor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfilemonitor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFileMonitor called nsp_type_gfilemonitor
       */
      type->id =  nsp_type_gfilemonitor_id = nsp_new_type_id();
      nsp_type_gfilemonitor = type;
      if ( nsp_register_type(nsp_type_gfilemonitor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfilemonitor, G_TYPE_FILE_MONITOR);
      return ( mode == T_BASE ) ? type : new_type_gfilemonitor(mode);
    }
  else 
    {
      type->id = nsp_type_gfilemonitor_id;
      return type;
    }
}

/*
 * initialize NspGFileMonitor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfilemonitor(NspGFileMonitor *Obj,NspTypeGFileMonitor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFileMonitor 
 */

NspGFileMonitor *new_gfilemonitor() 
{
  NspGFileMonitor *loc;
  /* type must exists */
  nsp_type_gfilemonitor = new_type_gfilemonitor(T_BASE);
  if ( (loc = malloc(sizeof(NspGFileMonitor)))== NULLGFILEMONITOR) return loc;
  /* initialize object */
  if ( init_gfilemonitor(loc,nsp_type_gfilemonitor) == FAIL) return NULLGFILEMONITOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFileMonitor 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfilemonitor_type_name[]="GFileMonitor";
static char gfilemonitor_short_type_name[]="GFileMonitor";

static char *nsp_gfilemonitor_type_as_string(void)
{
  return(gfilemonitor_type_name);
}

static char *nsp_gfilemonitor_type_short_string(NspObject *v)
{
  return(gfilemonitor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFileMonitor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFileMonitor   *nsp_gfilemonitor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfilemonitor_id)  == TRUE  ) return ((NspGFileMonitor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfilemonitor));
  return NULL;
}

int IsGFileMonitorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfilemonitor_id);
}

int IsGFileMonitor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfilemonitor_id);
}

NspGFileMonitor  *GetGFileMonitorCopy(Stack stack, int i)
{
  if (  GetGFileMonitor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFileMonitor  *GetGFileMonitor(Stack stack, int i)
{
  NspGFileMonitor *M;
  if (( M = nsp_gfilemonitor_object(NthObj(i))) == NULLGFILEMONITOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFileMonitor *gfilemonitor_copy(NspGFileMonitor *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfilemonitor);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfilemonitor);
}

/*-------------------------------------------------------------------
 * wrappers for the GFileMonitor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_file_monitor_cancel(NspGFileMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_file_monitor_cancel(G_FILE_MONITOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_monitor_is_cancelled(NspGFileMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_file_monitor_is_cancelled(G_FILE_MONITOR(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_file_monitor_set_rate_limit(NspGFileMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int limit_msecs;
  if ( GetArgs(stack,rhs,opt,T,&limit_msecs) == FAIL) return RET_BUG;
    g_file_monitor_set_rate_limit(G_FILE_MONITOR(self->obj),limit_msecs);
  return 0;
}

static int _wrap_g_file_monitor_emit_event(NspGFileMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check,obj, t_end};
  NspGObject *child, *other_file;
  GFileMonitorEvent event_type;
  NspObject *nsp_event_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &child, &nsp_type_gfile, &other_file, &nsp_event_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_FILE_MONITOR_EVENT, nsp_event_type, &event_type)== FAIL)
      return RET_BUG;
    g_file_monitor_emit_event(G_FILE_MONITOR(self->obj),G_FILE(child->obj),G_FILE(other_file->obj),event_type);
  return 0;
}

static NspMethods gfilemonitor_methods[] = {
  {"cancel",(nsp_method *) _wrap_g_file_monitor_cancel},
  {"is_cancelled",(nsp_method *) _wrap_g_file_monitor_is_cancelled},
  {"set_rate_limit",(nsp_method *) _wrap_g_file_monitor_set_rate_limit},
  {"emit_event",(nsp_method *) _wrap_g_file_monitor_emit_event},
  { NULL, NULL}
};

static NspMethods *gfilemonitor_get_methods(void) { return gfilemonitor_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfilemonitor_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGIOStream ----------- */


#define  NspGIOStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/giostream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGIOStream inherits from GObject 
 */

int nsp_type_giostream_id=0;
NspTypeGIOStream *nsp_type_giostream=NULL;

/*
 * Type object for NspGIOStream 
 * all the instance of NspTypeGIOStream share the same id. 
 * nsp_type_giostream: is an instance of NspTypeGIOStream 
 *    used for objects of NspGIOStream type (i.e built with new_giostream) 
 * other instances are used for derived classes 
 */
NspTypeGIOStream *new_type_giostream(type_mode mode)
{
  NspTypeGIOStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_giostream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_giostream;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = giostream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = giostream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_giostream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for giostream */ 

  top->s_type =  (s_type_func *) nsp_giostream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_giostream_type_short_string;
  /* top->create = (create_func*) int_giostream_create;*/

  /* specific methods for giostream */

  type->init = (init_func *) init_giostream;

  /* 
   * NspGIOStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_giostream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGIOStream called nsp_type_giostream
       */
      type->id =  nsp_type_giostream_id = nsp_new_type_id();
      nsp_type_giostream = type;
      if ( nsp_register_type(nsp_type_giostream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_giostream, G_TYPE_IO_STREAM);
      return ( mode == T_BASE ) ? type : new_type_giostream(mode);
    }
  else 
    {
      type->id = nsp_type_giostream_id;
      return type;
    }
}

/*
 * initialize NspGIOStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_giostream(NspGIOStream *Obj,NspTypeGIOStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGIOStream 
 */

NspGIOStream *new_giostream() 
{
  NspGIOStream *loc;
  /* type must exists */
  nsp_type_giostream = new_type_giostream(T_BASE);
  if ( (loc = malloc(sizeof(NspGIOStream)))== NULLGIOSTREAM) return loc;
  /* initialize object */
  if ( init_giostream(loc,nsp_type_giostream) == FAIL) return NULLGIOSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGIOStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char giostream_type_name[]="GIOStream";
static char giostream_short_type_name[]="GIOStream";

static char *nsp_giostream_type_as_string(void)
{
  return(giostream_type_name);
}

static char *nsp_giostream_type_short_string(NspObject *v)
{
  return(giostream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGIOStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGIOStream   *nsp_giostream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_giostream_id)  == TRUE  ) return ((NspGIOStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_giostream));
  return NULL;
}

int IsGIOStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_giostream_id);
}

int IsGIOStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_giostream_id);
}

NspGIOStream  *GetGIOStreamCopy(Stack stack, int i)
{
  if (  GetGIOStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGIOStream  *GetGIOStream(Stack stack, int i)
{
  NspGIOStream *M;
  if (( M = nsp_giostream_object(NthObj(i))) == NULLGIOSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGIOStream *giostream_copy(NspGIOStream *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_giostream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_giostream);
}

/*-------------------------------------------------------------------
 * wrappers for the GIOStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_io_stream_get_input_stream(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GInputStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_io_stream_get_input_stream(G_IO_STREAM(self->obj));
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_io_stream_get_output_stream(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GOutputStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_io_stream_get_output_stream(G_IO_STREAM(self->obj));
  nsp_type_goutputstream = new_type_goutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_goutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_io_stream_close(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_io_stream_close(G_IO_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_io_stream_close_finish(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_io_stream_close_finish(G_IO_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_io_stream_is_closed(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_io_stream_is_closed(G_IO_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_io_stream_has_pending(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_io_stream_has_pending(G_IO_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_io_stream_set_pending(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_io_stream_set_pending(G_IO_STREAM(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_io_stream_clear_pending(NspGIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_io_stream_clear_pending(G_IO_STREAM(self->obj));
  return 0;
}

static NspMethods giostream_methods[] = {
  {"get_input_stream",(nsp_method *) _wrap_g_io_stream_get_input_stream},
  {"get_output_stream",(nsp_method *) _wrap_g_io_stream_get_output_stream},
  {"close",(nsp_method *) _wrap_g_io_stream_close},
  {"close_finish",(nsp_method *) _wrap_g_io_stream_close_finish},
  {"is_closed",(nsp_method *) _wrap_g_io_stream_is_closed},
  {"has_pending",(nsp_method *) _wrap_g_io_stream_has_pending},
  {"set_pending",(nsp_method *) _wrap_g_io_stream_set_pending},
  {"clear_pending",(nsp_method *) _wrap_g_io_stream_clear_pending},
  { NULL, NULL}
};

static NspMethods *giostream_get_methods(void) { return giostream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab giostream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFileIOStream ----------- */


#define  NspGFileIOStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfileiostream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFileIOStream inherits from GIOStream 
 */

int nsp_type_gfileiostream_id=0;
NspTypeGFileIOStream *nsp_type_gfileiostream=NULL;

/*
 * Type object for NspGFileIOStream 
 * all the instance of NspTypeGFileIOStream share the same id. 
 * nsp_type_gfileiostream: is an instance of NspTypeGFileIOStream 
 *    used for objects of NspGFileIOStream type (i.e built with new_gfileiostream) 
 * other instances are used for derived classes 
 */
NspTypeGFileIOStream *new_type_gfileiostream(type_mode mode)
{
  NspTypeGFileIOStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfileiostream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfileiostream;
    }
  if (( type =  malloc(sizeof(NspTypeGIOStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_giostream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfileiostream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfileiostream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfileiostream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfileiostream */ 

  top->s_type =  (s_type_func *) nsp_gfileiostream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfileiostream_type_short_string;
  /* top->create = (create_func*) int_gfileiostream_create;*/

  /* specific methods for gfileiostream */

  type->init = (init_func *) init_gfileiostream;

  /* 
   * NspGFileIOStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfileiostream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFileIOStream called nsp_type_gfileiostream
       */
      type->id =  nsp_type_gfileiostream_id = nsp_new_type_id();
      nsp_type_gfileiostream = type;
      if ( nsp_register_type(nsp_type_gfileiostream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfileiostream, G_TYPE_FILE_IO_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gfileiostream(mode);
    }
  else 
    {
      type->id = nsp_type_gfileiostream_id;
      return type;
    }
}

/*
 * initialize NspGFileIOStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfileiostream(NspGFileIOStream *Obj,NspTypeGFileIOStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFileIOStream 
 */

NspGFileIOStream *new_gfileiostream() 
{
  NspGFileIOStream *loc;
  /* type must exists */
  nsp_type_gfileiostream = new_type_gfileiostream(T_BASE);
  if ( (loc = malloc(sizeof(NspGFileIOStream)))== NULLGFILEIOSTREAM) return loc;
  /* initialize object */
  if ( init_gfileiostream(loc,nsp_type_gfileiostream) == FAIL) return NULLGFILEIOSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFileIOStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfileiostream_type_name[]="GFileIOStream";
static char gfileiostream_short_type_name[]="GFileIOStream";

static char *nsp_gfileiostream_type_as_string(void)
{
  return(gfileiostream_type_name);
}

static char *nsp_gfileiostream_type_short_string(NspObject *v)
{
  return(gfileiostream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFileIOStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFileIOStream   *nsp_gfileiostream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfileiostream_id)  == TRUE  ) return ((NspGFileIOStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfileiostream));
  return NULL;
}

int IsGFileIOStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfileiostream_id);
}

int IsGFileIOStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfileiostream_id);
}

NspGFileIOStream  *GetGFileIOStreamCopy(Stack stack, int i)
{
  if (  GetGFileIOStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFileIOStream  *GetGFileIOStream(Stack stack, int i)
{
  NspGFileIOStream *M;
  if (( M = nsp_gfileiostream_object(NthObj(i))) == NULLGFILEIOSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFileIOStream *gfileiostream_copy(NspGFileIOStream *self)
{
  /* return giostream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileiostream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileiostream);
}

/*-------------------------------------------------------------------
 * wrappers for the GFileIOStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_file_io_stream_get_etag(NspGFileIOStream *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_io_stream_get_etag(G_FILE_IO_STREAM(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods gfileiostream_methods[] = {
  {"get_etag",(nsp_method *) _wrap_g_file_io_stream_get_etag},
  { NULL, NULL}
};

static NspMethods *gfileiostream_get_methods(void) { return gfileiostream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfileiostream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGIcon ----------- */


#define  NspGIcon_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gicon.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGIcon inherits from GObject 
 */

int nsp_type_gicon_id=0;
NspTypeGIcon *nsp_type_gicon=NULL;

/*
 * Type object for NspGIcon 
 * all the instance of NspTypeGIcon share the same id. 
 * nsp_type_gicon: is an instance of NspTypeGIcon 
 *    used for objects of NspGIcon type (i.e built with new_gicon) 
 * other instances are used for derived classes 
 */
NspTypeGIcon *new_type_gicon(type_mode mode)
{
  NspTypeGIcon *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gicon != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gicon;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gicon_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gicon_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gicon;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gicon */ 

  top->s_type =  (s_type_func *) nsp_gicon_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gicon_type_short_string;
  /* top->create = (create_func*) int_gicon_create;*/

  /* specific methods for gicon */

  type->init = (init_func *) init_gicon;

  /* 
   * NspGIcon interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gicon_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGIcon called nsp_type_gicon
       */
      type->id =  nsp_type_gicon_id = nsp_new_type_id();
      nsp_type_gicon = type;
      if ( nsp_register_type(nsp_type_gicon) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gicon, G_TYPE_ICON);
      return ( mode == T_BASE ) ? type : new_type_gicon(mode);
    }
  else 
    {
      type->id = nsp_type_gicon_id;
      return type;
    }
}

/*
 * initialize NspGIcon instances 
 * locally and by calling initializer on parent class 
 */

static int init_gicon(NspGIcon *Obj,NspTypeGIcon *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGIcon 
 */

NspGIcon *new_gicon() 
{
  NspGIcon *loc;
  /* type must exists */
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ( (loc = malloc(sizeof(NspGIcon)))== NULLGICON) return loc;
  /* initialize object */
  if ( init_gicon(loc,nsp_type_gicon) == FAIL) return NULLGICON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGIcon 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gicon_type_name[]="GIcon";
static char gicon_short_type_name[]="GIcon";

static char *nsp_gicon_type_as_string(void)
{
  return(gicon_type_name);
}

static char *nsp_gicon_type_short_string(NspObject *v)
{
  return(gicon_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGIcon objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGIcon   *nsp_gicon_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gicon_id)  == TRUE  ) return ((NspGIcon *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gicon));
  return NULL;
}

int IsGIconObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gicon_id);
}

int IsGIcon(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gicon_id);
}

NspGIcon  *GetGIconCopy(Stack stack, int i)
{
  if (  GetGIcon(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGIcon  *GetGIcon(Stack stack, int i)
{
  NspGIcon *M;
  if (( M = nsp_gicon_object(NthObj(i))) == NULLGICON)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGIcon *gicon_copy(NspGIcon *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gicon);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gicon);
}

/*-------------------------------------------------------------------
 * wrappers for the GIcon
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_icon_new_for_string (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *str;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&str) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_icon_new_for_string(str,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gicon = new_type_gicon(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gicon );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_icon_equal(NspGIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *icon2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gicon, &icon2) == FAIL) return RET_BUG;
    ret =g_icon_equal(G_ICON(self->obj),G_ICON(icon2->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_icon_to_string(NspGIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_icon_to_string(G_ICON(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,38,0)
static int _wrap_g_icon_serialize(NspGIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariant *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_icon_serialize(G_ICON(self->obj));
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_icon_serialize(Stack stack, int rhs, int opt, int lhs) /* serialize */
{
  Scierror("Error: function g_icon_serialize not available\n");
  return RET_BUG;
}
#endif
static NspMethods gicon_methods[] = {
  {"equal",(nsp_method *) _wrap_g_icon_equal},
  {"to_string",(nsp_method *) _wrap_g_icon_to_string},
  {"serialize",(nsp_method *) _wrap_g_icon_serialize},
  { NULL, NULL}
};

static NspMethods *gicon_get_methods(void) { return gicon_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gicon_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGInetAddress ----------- */


#define  NspGInetAddress_Private 
#include <nsp/objects.h>
#include <nsp/gtk/ginetaddress.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGInetAddress inherits from GObject 
 */

int nsp_type_ginetaddress_id=0;
NspTypeGInetAddress *nsp_type_ginetaddress=NULL;

/*
 * Type object for NspGInetAddress 
 * all the instance of NspTypeGInetAddress share the same id. 
 * nsp_type_ginetaddress: is an instance of NspTypeGInetAddress 
 *    used for objects of NspGInetAddress type (i.e built with new_ginetaddress) 
 * other instances are used for derived classes 
 */
NspTypeGInetAddress *new_type_ginetaddress(type_mode mode)
{
  NspTypeGInetAddress *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_ginetaddress != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ginetaddress;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = ginetaddress_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = ginetaddress_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_ginetaddress;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ginetaddress */ 

  top->s_type =  (s_type_func *) nsp_ginetaddress_type_as_string;
  top->sh_type = (sh_type_func *) nsp_ginetaddress_type_short_string;
  /* top->create = (create_func*) int_ginetaddress_create;*/

  /* specific methods for ginetaddress */

  type->init = (init_func *) init_ginetaddress;

  /* 
   * NspGInetAddress interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_ginetaddress_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGInetAddress called nsp_type_ginetaddress
       */
      type->id =  nsp_type_ginetaddress_id = nsp_new_type_id();
      nsp_type_ginetaddress = type;
      if ( nsp_register_type(nsp_type_ginetaddress) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_ginetaddress, G_TYPE_INET_ADDRESS);
      return ( mode == T_BASE ) ? type : new_type_ginetaddress(mode);
    }
  else 
    {
      type->id = nsp_type_ginetaddress_id;
      return type;
    }
}

/*
 * initialize NspGInetAddress instances 
 * locally and by calling initializer on parent class 
 */

static int init_ginetaddress(NspGInetAddress *Obj,NspTypeGInetAddress *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGInetAddress 
 */

NspGInetAddress *new_ginetaddress() 
{
  NspGInetAddress *loc;
  /* type must exists */
  nsp_type_ginetaddress = new_type_ginetaddress(T_BASE);
  if ( (loc = malloc(sizeof(NspGInetAddress)))== NULLGINETADDRESS) return loc;
  /* initialize object */
  if ( init_ginetaddress(loc,nsp_type_ginetaddress) == FAIL) return NULLGINETADDRESS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGInetAddress 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char ginetaddress_type_name[]="GInetAddress";
static char ginetaddress_short_type_name[]="GInetAddress";

static char *nsp_ginetaddress_type_as_string(void)
{
  return(ginetaddress_type_name);
}

static char *nsp_ginetaddress_type_short_string(NspObject *v)
{
  return(ginetaddress_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGInetAddress objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGInetAddress   *nsp_ginetaddress_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_ginetaddress_id)  == TRUE  ) return ((NspGInetAddress *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_ginetaddress));
  return NULL;
}

int IsGInetAddressObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_ginetaddress_id);
}

int IsGInetAddress(NspObject *O)
{
  return nsp_object_type(O,nsp_type_ginetaddress_id);
}

NspGInetAddress  *GetGInetAddressCopy(Stack stack, int i)
{
  if (  GetGInetAddress(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGInetAddress  *GetGInetAddress(Stack stack, int i)
{
  NspGInetAddress *M;
  if (( M = nsp_ginetaddress_object(NthObj(i))) == NULLGINETADDRESS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGInetAddress *ginetaddress_copy(NspGInetAddress *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginetaddress);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginetaddress);
}

/*-------------------------------------------------------------------
 * wrappers for the GInetAddress
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_inet_address_new_any (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, t_end};
  GSocketFamily family;
  NspObject *nsp_family = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_family) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_FAMILY, nsp_family, &family)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)g_inet_address_new_any(family))== NULL) return RET_BUG;

  nsp_type_ginetaddress = new_type_ginetaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_inet_address_new_loopback (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj, t_end};
  GSocketFamily family;
  NspObject *nsp_family = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_family) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_FAMILY, nsp_family, &family)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)g_inet_address_new_loopback(family))== NULL) return RET_BUG;

  nsp_type_ginetaddress = new_type_ginetaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_inet_address_new_from_string (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *string;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_inet_address_new_from_string(string))== NULL) return RET_BUG;

  nsp_type_ginetaddress = new_type_ginetaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_inet_address_equal(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *other_address;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &other_address) == FAIL) return RET_BUG;
    ret =g_inet_address_equal(G_INET_ADDRESS(self->obj),G_INET_ADDRESS(other_address->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_to_string(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_inet_address_to_string(G_INET_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_inet_address_get_native_size(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_native_size(G_INET_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_family(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_family(G_INET_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_any(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_any(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_loopback(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_loopback(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_link_local(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_link_local(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_site_local(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_site_local(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_multicast(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_multicast(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_mc_global(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_mc_global(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_mc_link_local(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_mc_link_local(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_mc_node_local(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_mc_node_local(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_mc_org_local(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_mc_org_local(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_inet_address_get_is_mc_site_local(NspGInetAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_get_is_mc_site_local(G_INET_ADDRESS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods ginetaddress_methods[] = {
  {"equal",(nsp_method *) _wrap_g_inet_address_equal},
  {"to_string",(nsp_method *) _wrap_g_inet_address_to_string},
  {"get_native_size",(nsp_method *) _wrap_g_inet_address_get_native_size},
  {"get_family",(nsp_method *) _wrap_g_inet_address_get_family},
  {"get_is_any",(nsp_method *) _wrap_g_inet_address_get_is_any},
  {"get_is_loopback",(nsp_method *) _wrap_g_inet_address_get_is_loopback},
  {"get_is_link_local",(nsp_method *) _wrap_g_inet_address_get_is_link_local},
  {"get_is_site_local",(nsp_method *) _wrap_g_inet_address_get_is_site_local},
  {"get_is_multicast",(nsp_method *) _wrap_g_inet_address_get_is_multicast},
  {"get_is_mc_global",(nsp_method *) _wrap_g_inet_address_get_is_mc_global},
  {"get_is_mc_link_local",(nsp_method *) _wrap_g_inet_address_get_is_mc_link_local},
  {"get_is_mc_node_local",(nsp_method *) _wrap_g_inet_address_get_is_mc_node_local},
  {"get_is_mc_org_local",(nsp_method *) _wrap_g_inet_address_get_is_mc_org_local},
  {"get_is_mc_site_local",(nsp_method *) _wrap_g_inet_address_get_is_mc_site_local},
  { NULL, NULL}
};

static NspMethods *ginetaddress_get_methods(void) { return ginetaddress_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ginetaddress_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGInetAddressMask ----------- */


#define  NspGInetAddressMask_Private 
#include <nsp/objects.h>
#include <nsp/gtk/ginetaddressmask.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGInetAddressMask inherits from GObject 
 */

int nsp_type_ginetaddressmask_id=0;
NspTypeGInetAddressMask *nsp_type_ginetaddressmask=NULL;

/*
 * Type object for NspGInetAddressMask 
 * all the instance of NspTypeGInetAddressMask share the same id. 
 * nsp_type_ginetaddressmask: is an instance of NspTypeGInetAddressMask 
 *    used for objects of NspGInetAddressMask type (i.e built with new_ginetaddressmask) 
 * other instances are used for derived classes 
 */
NspTypeGInetAddressMask *new_type_ginetaddressmask(type_mode mode)
{
  NspTypeGInetAddressMask *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_ginetaddressmask != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ginetaddressmask;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = ginetaddressmask_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = ginetaddressmask_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_ginetaddressmask;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ginetaddressmask */ 

  top->s_type =  (s_type_func *) nsp_ginetaddressmask_type_as_string;
  top->sh_type = (sh_type_func *) nsp_ginetaddressmask_type_short_string;
  /* top->create = (create_func*) int_ginetaddressmask_create;*/

  /* specific methods for ginetaddressmask */

  type->init = (init_func *) init_ginetaddressmask;

  /* 
   * NspGInetAddressMask interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_ginetaddressmask_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGInetAddressMask called nsp_type_ginetaddressmask
       */
      type->id =  nsp_type_ginetaddressmask_id = nsp_new_type_id();
      nsp_type_ginetaddressmask = type;
      if ( nsp_register_type(nsp_type_ginetaddressmask) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_ginetaddressmask, G_TYPE_INET_ADDRESS_MASK);
      return ( mode == T_BASE ) ? type : new_type_ginetaddressmask(mode);
    }
  else 
    {
      type->id = nsp_type_ginetaddressmask_id;
      return type;
    }
}

/*
 * initialize NspGInetAddressMask instances 
 * locally and by calling initializer on parent class 
 */

static int init_ginetaddressmask(NspGInetAddressMask *Obj,NspTypeGInetAddressMask *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGInetAddressMask 
 */

NspGInetAddressMask *new_ginetaddressmask() 
{
  NspGInetAddressMask *loc;
  /* type must exists */
  nsp_type_ginetaddressmask = new_type_ginetaddressmask(T_BASE);
  if ( (loc = malloc(sizeof(NspGInetAddressMask)))== NULLGINETADDRESSMASK) return loc;
  /* initialize object */
  if ( init_ginetaddressmask(loc,nsp_type_ginetaddressmask) == FAIL) return NULLGINETADDRESSMASK;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGInetAddressMask 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char ginetaddressmask_type_name[]="GInetAddressMask";
static char ginetaddressmask_short_type_name[]="GInetAddressMask";

static char *nsp_ginetaddressmask_type_as_string(void)
{
  return(ginetaddressmask_type_name);
}

static char *nsp_ginetaddressmask_type_short_string(NspObject *v)
{
  return(ginetaddressmask_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGInetAddressMask objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGInetAddressMask   *nsp_ginetaddressmask_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_ginetaddressmask_id)  == TRUE  ) return ((NspGInetAddressMask *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_ginetaddressmask));
  return NULL;
}

int IsGInetAddressMaskObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_ginetaddressmask_id);
}

int IsGInetAddressMask(NspObject *O)
{
  return nsp_object_type(O,nsp_type_ginetaddressmask_id);
}

NspGInetAddressMask  *GetGInetAddressMaskCopy(Stack stack, int i)
{
  if (  GetGInetAddressMask(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGInetAddressMask  *GetGInetAddressMask(Stack stack, int i)
{
  NspGInetAddressMask *M;
  if (( M = nsp_ginetaddressmask_object(NthObj(i))) == NULLGINETADDRESSMASK)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGInetAddressMask *ginetaddressmask_copy(NspGInetAddressMask *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginetaddressmask);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginetaddressmask);
}

/*-------------------------------------------------------------------
 * wrappers for the GInetAddressMask
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_inet_address_mask_new_from_string (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *mask_string;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&mask_string) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_inet_address_mask_new_from_string(mask_string,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_ginetaddressmask = new_type_ginetaddressmask(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetaddressmask );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_inet_address_mask_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *addr;
  int length;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &addr, &length) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_inet_address_mask_new(G_INET_ADDRESS(addr->obj),length,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_ginetaddressmask = new_type_ginetaddressmask(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetaddressmask );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_address_mask_to_string(NspGInetAddressMask *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_inet_address_mask_to_string(G_INET_ADDRESS_MASK(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

#else
int _wrap_g_inet_address_mask_to_string(Stack stack, int rhs, int opt, int lhs) /* to_string */
{
  Scierror("Error: function g_inet_address_mask_to_string not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_address_mask_get_family(NspGInetAddressMask *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_inet_address_mask_get_family(G_INET_ADDRESS_MASK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_inet_address_mask_get_family(Stack stack, int rhs, int opt, int lhs) /* get_family */
{
  Scierror("Error: function g_inet_address_mask_get_family not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_address_mask_get_address(NspGInetAddressMask *self,Stack stack,int rhs,int opt,int lhs)
{
  GInetAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_inet_address_mask_get_address(G_INET_ADDRESS_MASK(self->obj));
  nsp_type_ginetaddress = new_type_ginetaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginetaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_inet_address_mask_get_address(Stack stack, int rhs, int opt, int lhs) /* get_address */
{
  Scierror("Error: function g_inet_address_mask_get_address not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_address_mask_get_length(NspGInetAddressMask *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_address_mask_get_length(G_INET_ADDRESS_MASK(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_inet_address_mask_get_length(Stack stack, int rhs, int opt, int lhs) /* get_length */
{
  Scierror("Error: function g_inet_address_mask_get_length not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_address_mask_matches(NspGInetAddressMask *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *address;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &address) == FAIL) return RET_BUG;
    ret =g_inet_address_mask_matches(G_INET_ADDRESS_MASK(self->obj),G_INET_ADDRESS(address->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_inet_address_mask_matches(Stack stack, int rhs, int opt, int lhs) /* matches */
{
  Scierror("Error: function g_inet_address_mask_matches not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_address_mask_equal(NspGInetAddressMask *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *mask2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddressmask, &mask2) == FAIL) return RET_BUG;
    ret =g_inet_address_mask_equal(G_INET_ADDRESS_MASK(self->obj),G_INET_ADDRESS_MASK(mask2->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_inet_address_mask_equal(Stack stack, int rhs, int opt, int lhs) /* equal */
{
  Scierror("Error: function g_inet_address_mask_equal not available\n");
  return RET_BUG;
}
#endif
static NspMethods ginetaddressmask_methods[] = {
  {"to_string",(nsp_method *) _wrap_g_inet_address_mask_to_string},
  {"get_family",(nsp_method *) _wrap_g_inet_address_mask_get_family},
  {"get_address",(nsp_method *) _wrap_g_inet_address_mask_get_address},
  {"get_length",(nsp_method *) _wrap_g_inet_address_mask_get_length},
  {"matches",(nsp_method *) _wrap_g_inet_address_mask_matches},
  {"equal",(nsp_method *) _wrap_g_inet_address_mask_equal},
  { NULL, NULL}
};

static NspMethods *ginetaddressmask_get_methods(void) { return ginetaddressmask_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ginetaddressmask_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGInitable ----------- */


#define  NspGInitable_Private 
#include <nsp/objects.h>
#include <nsp/gtk/ginitable.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGInitable inherits from GObject 
 */

int nsp_type_ginitable_id=0;
NspTypeGInitable *nsp_type_ginitable=NULL;

/*
 * Type object for NspGInitable 
 * all the instance of NspTypeGInitable share the same id. 
 * nsp_type_ginitable: is an instance of NspTypeGInitable 
 *    used for objects of NspGInitable type (i.e built with new_ginitable) 
 * other instances are used for derived classes 
 */
NspTypeGInitable *new_type_ginitable(type_mode mode)
{
  NspTypeGInitable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_ginitable != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ginitable;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = ginitable_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = ginitable_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_ginitable;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ginitable */ 

  top->s_type =  (s_type_func *) nsp_ginitable_type_as_string;
  top->sh_type = (sh_type_func *) nsp_ginitable_type_short_string;
  /* top->create = (create_func*) int_ginitable_create;*/

  /* specific methods for ginitable */

  type->init = (init_func *) init_ginitable;

  /* 
   * NspGInitable interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_ginitable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGInitable called nsp_type_ginitable
       */
      type->id =  nsp_type_ginitable_id = nsp_new_type_id();
      nsp_type_ginitable = type;
      if ( nsp_register_type(nsp_type_ginitable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_ginitable, G_TYPE_INITABLE);
      return ( mode == T_BASE ) ? type : new_type_ginitable(mode);
    }
  else 
    {
      type->id = nsp_type_ginitable_id;
      return type;
    }
}

/*
 * initialize NspGInitable instances 
 * locally and by calling initializer on parent class 
 */

static int init_ginitable(NspGInitable *Obj,NspTypeGInitable *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGInitable 
 */

NspGInitable *new_ginitable() 
{
  NspGInitable *loc;
  /* type must exists */
  nsp_type_ginitable = new_type_ginitable(T_BASE);
  if ( (loc = malloc(sizeof(NspGInitable)))== NULLGINITABLE) return loc;
  /* initialize object */
  if ( init_ginitable(loc,nsp_type_ginitable) == FAIL) return NULLGINITABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGInitable 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char ginitable_type_name[]="GInitable";
static char ginitable_short_type_name[]="GInitable";

static char *nsp_ginitable_type_as_string(void)
{
  return(ginitable_type_name);
}

static char *nsp_ginitable_type_short_string(NspObject *v)
{
  return(ginitable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGInitable objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGInitable   *nsp_ginitable_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_ginitable_id)  == TRUE  ) return ((NspGInitable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_ginitable));
  return NULL;
}

int IsGInitableObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_ginitable_id);
}

int IsGInitable(NspObject *O)
{
  return nsp_object_type(O,nsp_type_ginitable_id);
}

NspGInitable  *GetGInitableCopy(Stack stack, int i)
{
  if (  GetGInitable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGInitable  *GetGInitable(Stack stack, int i)
{
  NspGInitable *M;
  if (( M = nsp_ginitable_object(NthObj(i))) == NULLGINITABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGInitable *ginitable_copy(NspGInitable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginitable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginitable);
}

/*-------------------------------------------------------------------
 * wrappers for the GInitable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_initable_init(NspGInitable *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_initable_init(G_INITABLE(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods ginitable_methods[] = {
  {"init",(nsp_method *) _wrap_g_initable_init},
  { NULL, NULL}
};

static NspMethods *ginitable_get_methods(void) { return ginitable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ginitable_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGInputStream ----------- */


#define  NspGInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/ginputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGInputStream inherits from GObject 
 */

int nsp_type_ginputstream_id=0;
NspTypeGInputStream *nsp_type_ginputstream=NULL;

/*
 * Type object for NspGInputStream 
 * all the instance of NspTypeGInputStream share the same id. 
 * nsp_type_ginputstream: is an instance of NspTypeGInputStream 
 *    used for objects of NspGInputStream type (i.e built with new_ginputstream) 
 * other instances are used for derived classes 
 */
NspTypeGInputStream *new_type_ginputstream(type_mode mode)
{
  NspTypeGInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_ginputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ginputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = ginputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = ginputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_ginputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ginputstream */ 

  top->s_type =  (s_type_func *) nsp_ginputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_ginputstream_type_short_string;
  /* top->create = (create_func*) int_ginputstream_create;*/

  /* specific methods for ginputstream */

  type->init = (init_func *) init_ginputstream;

  /* 
   * NspGInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_ginputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGInputStream called nsp_type_ginputstream
       */
      type->id =  nsp_type_ginputstream_id = nsp_new_type_id();
      nsp_type_ginputstream = type;
      if ( nsp_register_type(nsp_type_ginputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_ginputstream, G_TYPE_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_ginputstream(mode);
    }
  else 
    {
      type->id = nsp_type_ginputstream_id;
      return type;
    }
}

/*
 * initialize NspGInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_ginputstream(NspGInputStream *Obj,NspTypeGInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGInputStream 
 */

NspGInputStream *new_ginputstream() 
{
  NspGInputStream *loc;
  /* type must exists */
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGInputStream)))== NULLGINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_ginputstream(loc,nsp_type_ginputstream) == FAIL) return NULLGINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char ginputstream_type_name[]="GInputStream";
static char ginputstream_short_type_name[]="GInputStream";

static char *nsp_ginputstream_type_as_string(void)
{
  return(ginputstream_type_name);
}

static char *nsp_ginputstream_type_short_string(NspObject *v)
{
  return(ginputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGInputStream   *nsp_ginputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_ginputstream_id)  == TRUE  ) return ((NspGInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_ginputstream));
  return NULL;
}

int IsGInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_ginputstream_id);
}

int IsGInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_ginputstream_id);
}

NspGInputStream  *GetGInputStreamCopy(Stack stack, int i)
{
  if (  GetGInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGInputStream  *GetGInputStream(Stack stack, int i)
{
  NspGInputStream *M;
  if (( M = nsp_ginputstream_object(NthObj(i))) == NULLGINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGInputStream *ginputstream_copy(NspGInputStream *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_input_stream_skip(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int count, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&count, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_input_stream_skip(G_INPUT_STREAM(self->obj),count,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_close(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_input_stream_close(G_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_read_finish(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_input_stream_read_finish(G_INPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_skip_finish(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_input_stream_skip_finish(G_INPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_close_finish(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_input_stream_close_finish(G_INPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_is_closed(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_input_stream_is_closed(G_INPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_has_pending(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_input_stream_has_pending(G_INPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_set_pending(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_input_stream_set_pending(G_INPUT_STREAM(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_input_stream_clear_pending(NspGInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_input_stream_clear_pending(G_INPUT_STREAM(self->obj));
  return 0;
}

static NspMethods ginputstream_methods[] = {
  {"skip",(nsp_method *) _wrap_g_input_stream_skip},
  {"close",(nsp_method *) _wrap_g_input_stream_close},
  {"read_finish",(nsp_method *) _wrap_g_input_stream_read_finish},
  {"skip_finish",(nsp_method *) _wrap_g_input_stream_skip_finish},
  {"close_finish",(nsp_method *) _wrap_g_input_stream_close_finish},
  {"is_closed",(nsp_method *) _wrap_g_input_stream_is_closed},
  {"has_pending",(nsp_method *) _wrap_g_input_stream_has_pending},
  {"set_pending",(nsp_method *) _wrap_g_input_stream_set_pending},
  {"clear_pending",(nsp_method *) _wrap_g_input_stream_clear_pending},
  { NULL, NULL}
};

static NspMethods *ginputstream_get_methods(void) { return ginputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ginputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFilterInputStream ----------- */


#define  NspGFilterInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfilterinputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFilterInputStream inherits from GInputStream 
 */

int nsp_type_gfilterinputstream_id=0;
NspTypeGFilterInputStream *nsp_type_gfilterinputstream=NULL;

/*
 * Type object for NspGFilterInputStream 
 * all the instance of NspTypeGFilterInputStream share the same id. 
 * nsp_type_gfilterinputstream: is an instance of NspTypeGFilterInputStream 
 *    used for objects of NspGFilterInputStream type (i.e built with new_gfilterinputstream) 
 * other instances are used for derived classes 
 */
NspTypeGFilterInputStream *new_type_gfilterinputstream(type_mode mode)
{
  NspTypeGFilterInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfilterinputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfilterinputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGInputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_ginputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfilterinputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfilterinputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfilterinputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfilterinputstream */ 

  top->s_type =  (s_type_func *) nsp_gfilterinputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfilterinputstream_type_short_string;
  /* top->create = (create_func*) int_gfilterinputstream_create;*/

  /* specific methods for gfilterinputstream */

  type->init = (init_func *) init_gfilterinputstream;

  /* 
   * NspGFilterInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfilterinputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFilterInputStream called nsp_type_gfilterinputstream
       */
      type->id =  nsp_type_gfilterinputstream_id = nsp_new_type_id();
      nsp_type_gfilterinputstream = type;
      if ( nsp_register_type(nsp_type_gfilterinputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfilterinputstream, G_TYPE_FILTER_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gfilterinputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gfilterinputstream_id;
      return type;
    }
}

/*
 * initialize NspGFilterInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfilterinputstream(NspGFilterInputStream *Obj,NspTypeGFilterInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFilterInputStream 
 */

NspGFilterInputStream *new_gfilterinputstream() 
{
  NspGFilterInputStream *loc;
  /* type must exists */
  nsp_type_gfilterinputstream = new_type_gfilterinputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGFilterInputStream)))== NULLGFILTERINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gfilterinputstream(loc,nsp_type_gfilterinputstream) == FAIL) return NULLGFILTERINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFilterInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfilterinputstream_type_name[]="GFilterInputStream";
static char gfilterinputstream_short_type_name[]="GFilterInputStream";

static char *nsp_gfilterinputstream_type_as_string(void)
{
  return(gfilterinputstream_type_name);
}

static char *nsp_gfilterinputstream_type_short_string(NspObject *v)
{
  return(gfilterinputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFilterInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFilterInputStream   *nsp_gfilterinputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfilterinputstream_id)  == TRUE  ) return ((NspGFilterInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfilterinputstream));
  return NULL;
}

int IsGFilterInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfilterinputstream_id);
}

int IsGFilterInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfilterinputstream_id);
}

NspGFilterInputStream  *GetGFilterInputStreamCopy(Stack stack, int i)
{
  if (  GetGFilterInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFilterInputStream  *GetGFilterInputStream(Stack stack, int i)
{
  NspGFilterInputStream *M;
  if (( M = nsp_gfilterinputstream_object(NthObj(i))) == NULLGFILTERINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFilterInputStream *gfilterinputstream_copy(NspGFilterInputStream *self)
{
  /* return ginputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfilterinputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfilterinputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GFilterInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_filter_input_stream_get_base_stream(NspGFilterInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GInputStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_filter_input_stream_get_base_stream(G_FILTER_INPUT_STREAM(self->obj));
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_filter_input_stream_get_close_base_stream(NspGFilterInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_filter_input_stream_get_close_base_stream(G_FILTER_INPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_filter_input_stream_set_close_base_stream(NspGFilterInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int close_base;
  if ( GetArgs(stack,rhs,opt,T,&close_base) == FAIL) return RET_BUG;
    g_filter_input_stream_set_close_base_stream(G_FILTER_INPUT_STREAM(self->obj),close_base);
  return 0;
}

static NspMethods gfilterinputstream_methods[] = {
  {"get_base_stream",(nsp_method *) _wrap_g_filter_input_stream_get_base_stream},
  {"get_close_base_stream",(nsp_method *) _wrap_g_filter_input_stream_get_close_base_stream},
  {"set_close_base_stream",(nsp_method *) _wrap_g_filter_input_stream_set_close_base_stream},
  { NULL, NULL}
};

static NspMethods *gfilterinputstream_get_methods(void) { return gfilterinputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfilterinputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGBufferedInputStream ----------- */


#define  NspGBufferedInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gbufferedinputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGBufferedInputStream inherits from GFilterInputStream 
 */

int nsp_type_gbufferedinputstream_id=0;
NspTypeGBufferedInputStream *nsp_type_gbufferedinputstream=NULL;

/*
 * Type object for NspGBufferedInputStream 
 * all the instance of NspTypeGBufferedInputStream share the same id. 
 * nsp_type_gbufferedinputstream: is an instance of NspTypeGBufferedInputStream 
 *    used for objects of NspGBufferedInputStream type (i.e built with new_gbufferedinputstream) 
 * other instances are used for derived classes 
 */
NspTypeGBufferedInputStream *new_type_gbufferedinputstream(type_mode mode)
{
  NspTypeGBufferedInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gbufferedinputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gbufferedinputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGFilterInputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gfilterinputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gbufferedinputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gbufferedinputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gbufferedinputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gbufferedinputstream */ 

  top->s_type =  (s_type_func *) nsp_gbufferedinputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gbufferedinputstream_type_short_string;
  /* top->create = (create_func*) int_gbufferedinputstream_create;*/

  /* specific methods for gbufferedinputstream */

  type->init = (init_func *) init_gbufferedinputstream;

  /* 
   * NspGBufferedInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gbufferedinputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGBufferedInputStream called nsp_type_gbufferedinputstream
       */
      type->id =  nsp_type_gbufferedinputstream_id = nsp_new_type_id();
      nsp_type_gbufferedinputstream = type;
      if ( nsp_register_type(nsp_type_gbufferedinputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gbufferedinputstream, G_TYPE_BUFFERED_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gbufferedinputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gbufferedinputstream_id;
      return type;
    }
}

/*
 * initialize NspGBufferedInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gbufferedinputstream(NspGBufferedInputStream *Obj,NspTypeGBufferedInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGBufferedInputStream 
 */

NspGBufferedInputStream *new_gbufferedinputstream() 
{
  NspGBufferedInputStream *loc;
  /* type must exists */
  nsp_type_gbufferedinputstream = new_type_gbufferedinputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGBufferedInputStream)))== NULLGBUFFEREDINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gbufferedinputstream(loc,nsp_type_gbufferedinputstream) == FAIL) return NULLGBUFFEREDINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGBufferedInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gbufferedinputstream_type_name[]="GBufferedInputStream";
static char gbufferedinputstream_short_type_name[]="GBufferedInputStream";

static char *nsp_gbufferedinputstream_type_as_string(void)
{
  return(gbufferedinputstream_type_name);
}

static char *nsp_gbufferedinputstream_type_short_string(NspObject *v)
{
  return(gbufferedinputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGBufferedInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGBufferedInputStream   *nsp_gbufferedinputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gbufferedinputstream_id)  == TRUE  ) return ((NspGBufferedInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gbufferedinputstream));
  return NULL;
}

int IsGBufferedInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gbufferedinputstream_id);
}

int IsGBufferedInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gbufferedinputstream_id);
}

NspGBufferedInputStream  *GetGBufferedInputStreamCopy(Stack stack, int i)
{
  if (  GetGBufferedInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGBufferedInputStream  *GetGBufferedInputStream(Stack stack, int i)
{
  NspGBufferedInputStream *M;
  if (( M = nsp_gbufferedinputstream_object(NthObj(i))) == NULLGBUFFEREDINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGBufferedInputStream *gbufferedinputstream_copy(NspGBufferedInputStream *self)
{
  /* return gfilterinputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gbufferedinputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gbufferedinputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GBufferedInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_buffered_input_stream_new_sized (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *base_stream;
  int size;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginputstream, &base_stream, &size) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_buffered_input_stream_new_sized(G_INPUT_STREAM(base_stream->obj),size))== NULL) return RET_BUG;

  nsp_type_gbufferedinputstream = new_type_gbufferedinputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gbufferedinputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_buffered_input_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *base_stream;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginputstream, &base_stream) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_buffered_input_stream_new(G_INPUT_STREAM(base_stream->obj)))== NULL) return RET_BUG;

  nsp_type_gbufferedinputstream = new_type_gbufferedinputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gbufferedinputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_buffered_input_stream_get_buffer_size(NspGBufferedInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_buffered_input_stream_get_buffer_size(G_BUFFERED_INPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_buffered_input_stream_set_buffer_size(NspGBufferedInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int size;
  if ( GetArgs(stack,rhs,opt,T,&size) == FAIL) return RET_BUG;
    g_buffered_input_stream_set_buffer_size(G_BUFFERED_INPUT_STREAM(self->obj),size);
  return 0;
}

static int _wrap_g_buffered_input_stream_get_available(NspGBufferedInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_buffered_input_stream_get_available(G_BUFFERED_INPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_buffered_input_stream_fill(NspGBufferedInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int count, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&count, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_buffered_input_stream_fill(G_BUFFERED_INPUT_STREAM(self->obj),count,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_buffered_input_stream_fill_finish(NspGBufferedInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_buffered_input_stream_fill_finish(G_BUFFERED_INPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_buffered_input_stream_read_byte(NspGBufferedInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_buffered_input_stream_read_byte(G_BUFFERED_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gbufferedinputstream_methods[] = {
  {"get_buffer_size",(nsp_method *) _wrap_g_buffered_input_stream_get_buffer_size},
  {"set_buffer_size",(nsp_method *) _wrap_g_buffered_input_stream_set_buffer_size},
  {"get_available",(nsp_method *) _wrap_g_buffered_input_stream_get_available},
  {"fill",(nsp_method *) _wrap_g_buffered_input_stream_fill},
  {"fill_finish",(nsp_method *) _wrap_g_buffered_input_stream_fill_finish},
  {"read_byte",(nsp_method *) _wrap_g_buffered_input_stream_read_byte},
  { NULL, NULL}
};

static NspMethods *gbufferedinputstream_get_methods(void) { return gbufferedinputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gbufferedinputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDataInputStream ----------- */


#define  NspGDataInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdatainputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDataInputStream inherits from GBufferedInputStream 
 */

int nsp_type_gdatainputstream_id=0;
NspTypeGDataInputStream *nsp_type_gdatainputstream=NULL;

/*
 * Type object for NspGDataInputStream 
 * all the instance of NspTypeGDataInputStream share the same id. 
 * nsp_type_gdatainputstream: is an instance of NspTypeGDataInputStream 
 *    used for objects of NspGDataInputStream type (i.e built with new_gdatainputstream) 
 * other instances are used for derived classes 
 */
NspTypeGDataInputStream *new_type_gdatainputstream(type_mode mode)
{
  NspTypeGDataInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdatainputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdatainputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGBufferedInputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gbufferedinputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdatainputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdatainputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdatainputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdatainputstream */ 

  top->s_type =  (s_type_func *) nsp_gdatainputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdatainputstream_type_short_string;
  /* top->create = (create_func*) int_gdatainputstream_create;*/

  /* specific methods for gdatainputstream */

  type->init = (init_func *) init_gdatainputstream;

  /* 
   * NspGDataInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdatainputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDataInputStream called nsp_type_gdatainputstream
       */
      type->id =  nsp_type_gdatainputstream_id = nsp_new_type_id();
      nsp_type_gdatainputstream = type;
      if ( nsp_register_type(nsp_type_gdatainputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdatainputstream, G_TYPE_DATA_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gdatainputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gdatainputstream_id;
      return type;
    }
}

/*
 * initialize NspGDataInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdatainputstream(NspGDataInputStream *Obj,NspTypeGDataInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDataInputStream 
 */

NspGDataInputStream *new_gdatainputstream() 
{
  NspGDataInputStream *loc;
  /* type must exists */
  nsp_type_gdatainputstream = new_type_gdatainputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGDataInputStream)))== NULLGDATAINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gdatainputstream(loc,nsp_type_gdatainputstream) == FAIL) return NULLGDATAINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDataInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdatainputstream_type_name[]="GDataInputStream";
static char gdatainputstream_short_type_name[]="GDataInputStream";

static char *nsp_gdatainputstream_type_as_string(void)
{
  return(gdatainputstream_type_name);
}

static char *nsp_gdatainputstream_type_short_string(NspObject *v)
{
  return(gdatainputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDataInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDataInputStream   *nsp_gdatainputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdatainputstream_id)  == TRUE  ) return ((NspGDataInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdatainputstream));
  return NULL;
}

int IsGDataInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdatainputstream_id);
}

int IsGDataInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdatainputstream_id);
}

NspGDataInputStream  *GetGDataInputStreamCopy(Stack stack, int i)
{
  if (  GetGDataInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDataInputStream  *GetGDataInputStream(Stack stack, int i)
{
  NspGDataInputStream *M;
  if (( M = nsp_gdatainputstream_object(NthObj(i))) == NULLGDATAINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDataInputStream *gdatainputstream_copy(NspGDataInputStream *self)
{
  /* return gbufferedinputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdatainputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdatainputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GDataInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_data_input_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *base_stream;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginputstream, &base_stream) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_data_input_stream_new(G_INPUT_STREAM(base_stream->obj)))== NULL) return RET_BUG;

  nsp_type_gdatainputstream = new_type_gdatainputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdatainputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_data_input_stream_set_byte_order(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GDataStreamByteOrder order;
  NspObject *nsp_order = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_order) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATA_STREAM_BYTE_ORDER, nsp_order, &order)== FAIL)
      return RET_BUG;
    g_data_input_stream_set_byte_order(G_DATA_INPUT_STREAM(self->obj),order);
  return 0;
}

static int _wrap_g_data_input_stream_get_byte_order(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_data_input_stream_get_byte_order(G_DATA_INPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_input_stream_set_newline_type(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GDataStreamNewlineType type;
  NspObject *nsp_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATA_STREAM_NEWLINE_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
    g_data_input_stream_set_newline_type(G_DATA_INPUT_STREAM(self->obj),type);
  return 0;
}

static int _wrap_g_data_input_stream_get_newline_type(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_data_input_stream_get_newline_type(G_DATA_INPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_input_stream_read_byte(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_input_stream_read_byte(G_DATA_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_input_stream_read_int16(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_input_stream_read_int16(G_DATA_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_input_stream_read_uint16(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_input_stream_read_uint16(G_DATA_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_input_stream_read_int32(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_input_stream_read_int32(G_DATA_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_input_stream_read_uint32(NspGDataInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  gulong ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_input_stream_read_uint32(G_DATA_INPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdatainputstream_methods[] = {
  {"set_byte_order",(nsp_method *) _wrap_g_data_input_stream_set_byte_order},
  {"get_byte_order",(nsp_method *) _wrap_g_data_input_stream_get_byte_order},
  {"set_newline_type",(nsp_method *) _wrap_g_data_input_stream_set_newline_type},
  {"get_newline_type",(nsp_method *) _wrap_g_data_input_stream_get_newline_type},
  {"read_byte",(nsp_method *) _wrap_g_data_input_stream_read_byte},
  {"read_int16",(nsp_method *) _wrap_g_data_input_stream_read_int16},
  {"read_uint16",(nsp_method *) _wrap_g_data_input_stream_read_uint16},
  {"read_int32",(nsp_method *) _wrap_g_data_input_stream_read_int32},
  {"read_uint32",(nsp_method *) _wrap_g_data_input_stream_read_uint32},
  { NULL, NULL}
};

static NspMethods *gdatainputstream_get_methods(void) { return gdatainputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdatainputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGConverterInputStream ----------- */


#define  NspGConverterInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gconverterinputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGConverterInputStream inherits from GFilterInputStream 
 */

int nsp_type_gconverterinputstream_id=0;
NspTypeGConverterInputStream *nsp_type_gconverterinputstream=NULL;

/*
 * Type object for NspGConverterInputStream 
 * all the instance of NspTypeGConverterInputStream share the same id. 
 * nsp_type_gconverterinputstream: is an instance of NspTypeGConverterInputStream 
 *    used for objects of NspGConverterInputStream type (i.e built with new_gconverterinputstream) 
 * other instances are used for derived classes 
 */
NspTypeGConverterInputStream *new_type_gconverterinputstream(type_mode mode)
{
  NspTypeGConverterInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gconverterinputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gconverterinputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGFilterInputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gfilterinputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gconverterinputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gconverterinputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gconverterinputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gconverterinputstream */ 

  top->s_type =  (s_type_func *) nsp_gconverterinputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gconverterinputstream_type_short_string;
  /* top->create = (create_func*) int_gconverterinputstream_create;*/

  /* specific methods for gconverterinputstream */

  type->init = (init_func *) init_gconverterinputstream;

  /* 
   * NspGConverterInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gconverterinputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGConverterInputStream called nsp_type_gconverterinputstream
       */
      type->id =  nsp_type_gconverterinputstream_id = nsp_new_type_id();
      nsp_type_gconverterinputstream = type;
      if ( nsp_register_type(nsp_type_gconverterinputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gconverterinputstream, G_TYPE_CONVERTER_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gconverterinputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gconverterinputstream_id;
      return type;
    }
}

/*
 * initialize NspGConverterInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gconverterinputstream(NspGConverterInputStream *Obj,NspTypeGConverterInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGConverterInputStream 
 */

NspGConverterInputStream *new_gconverterinputstream() 
{
  NspGConverterInputStream *loc;
  /* type must exists */
  nsp_type_gconverterinputstream = new_type_gconverterinputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGConverterInputStream)))== NULLGCONVERTERINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gconverterinputstream(loc,nsp_type_gconverterinputstream) == FAIL) return NULLGCONVERTERINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGConverterInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gconverterinputstream_type_name[]="GConverterInputStream";
static char gconverterinputstream_short_type_name[]="GConverterInputStream";

static char *nsp_gconverterinputstream_type_as_string(void)
{
  return(gconverterinputstream_type_name);
}

static char *nsp_gconverterinputstream_type_short_string(NspObject *v)
{
  return(gconverterinputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGConverterInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGConverterInputStream   *nsp_gconverterinputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gconverterinputstream_id)  == TRUE  ) return ((NspGConverterInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gconverterinputstream));
  return NULL;
}

int IsGConverterInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gconverterinputstream_id);
}

int IsGConverterInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gconverterinputstream_id);
}

NspGConverterInputStream  *GetGConverterInputStreamCopy(Stack stack, int i)
{
  if (  GetGConverterInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGConverterInputStream  *GetGConverterInputStream(Stack stack, int i)
{
  NspGConverterInputStream *M;
  if (( M = nsp_gconverterinputstream_object(NthObj(i))) == NULLGCONVERTERINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGConverterInputStream *gconverterinputstream_copy(NspGConverterInputStream *self)
{
  /* return gfilterinputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gconverterinputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gconverterinputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GConverterInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_converter_input_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *base_stream, *converter;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginputstream, &base_stream, &nsp_type_gconverter, &converter) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_converter_input_stream_new(G_INPUT_STREAM(base_stream->obj),G_CONVERTER(converter->obj)))== NULL) return RET_BUG;

  nsp_type_gconverterinputstream = new_type_gconverterinputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gconverterinputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_converter_input_stream_get_converter(NspGConverterInputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GConverter *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_converter_input_stream_get_converter(G_CONVERTER_INPUT_STREAM(self->obj));
  nsp_type_gconverter = new_type_gconverter(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gconverter))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gconverterinputstream_methods[] = {
  {"get_converter",(nsp_method *) _wrap_g_converter_input_stream_get_converter},
  { NULL, NULL}
};

static NspMethods *gconverterinputstream_get_methods(void) { return gconverterinputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gconverterinputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFileInputStream ----------- */


#define  NspGFileInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfileinputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFileInputStream inherits from GInputStream 
 */

int nsp_type_gfileinputstream_id=0;
NspTypeGFileInputStream *nsp_type_gfileinputstream=NULL;

/*
 * Type object for NspGFileInputStream 
 * all the instance of NspTypeGFileInputStream share the same id. 
 * nsp_type_gfileinputstream: is an instance of NspTypeGFileInputStream 
 *    used for objects of NspGFileInputStream type (i.e built with new_gfileinputstream) 
 * other instances are used for derived classes 
 */
NspTypeGFileInputStream *new_type_gfileinputstream(type_mode mode)
{
  NspTypeGFileInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfileinputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfileinputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGInputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_ginputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfileinputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfileinputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfileinputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfileinputstream */ 

  top->s_type =  (s_type_func *) nsp_gfileinputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfileinputstream_type_short_string;
  /* top->create = (create_func*) int_gfileinputstream_create;*/

  /* specific methods for gfileinputstream */

  type->init = (init_func *) init_gfileinputstream;

  /* 
   * NspGFileInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfileinputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFileInputStream called nsp_type_gfileinputstream
       */
      type->id =  nsp_type_gfileinputstream_id = nsp_new_type_id();
      nsp_type_gfileinputstream = type;
      if ( nsp_register_type(nsp_type_gfileinputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfileinputstream, G_TYPE_FILE_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gfileinputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gfileinputstream_id;
      return type;
    }
}

/*
 * initialize NspGFileInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfileinputstream(NspGFileInputStream *Obj,NspTypeGFileInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFileInputStream 
 */

NspGFileInputStream *new_gfileinputstream() 
{
  NspGFileInputStream *loc;
  /* type must exists */
  nsp_type_gfileinputstream = new_type_gfileinputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGFileInputStream)))== NULLGFILEINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gfileinputstream(loc,nsp_type_gfileinputstream) == FAIL) return NULLGFILEINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFileInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfileinputstream_type_name[]="GFileInputStream";
static char gfileinputstream_short_type_name[]="GFileInputStream";

static char *nsp_gfileinputstream_type_as_string(void)
{
  return(gfileinputstream_type_name);
}

static char *nsp_gfileinputstream_type_short_string(NspObject *v)
{
  return(gfileinputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFileInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFileInputStream   *nsp_gfileinputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfileinputstream_id)  == TRUE  ) return ((NspGFileInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfileinputstream));
  return NULL;
}

int IsGFileInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfileinputstream_id);
}

int IsGFileInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfileinputstream_id);
}

NspGFileInputStream  *GetGFileInputStreamCopy(Stack stack, int i)
{
  if (  GetGFileInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFileInputStream  *GetGFileInputStream(Stack stack, int i)
{
  NspGFileInputStream *M;
  if (( M = nsp_gfileinputstream_object(NthObj(i))) == NULLGFILEINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFileInputStream *gfileinputstream_copy(NspGFileInputStream *self)
{
  /* return ginputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileinputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileinputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GFileInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gfileinputstream_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfileinputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGLoadableIcon ----------- */


#define  NspGLoadableIcon_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gloadableicon.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGLoadableIcon inherits from GObject 
 */

int nsp_type_gloadableicon_id=0;
NspTypeGLoadableIcon *nsp_type_gloadableicon=NULL;

/*
 * Type object for NspGLoadableIcon 
 * all the instance of NspTypeGLoadableIcon share the same id. 
 * nsp_type_gloadableicon: is an instance of NspTypeGLoadableIcon 
 *    used for objects of NspGLoadableIcon type (i.e built with new_gloadableicon) 
 * other instances are used for derived classes 
 */
NspTypeGLoadableIcon *new_type_gloadableicon(type_mode mode)
{
  NspTypeGLoadableIcon *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gloadableicon != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gloadableicon;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gloadableicon_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gloadableicon_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gloadableicon;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gloadableicon */ 

  top->s_type =  (s_type_func *) nsp_gloadableicon_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gloadableicon_type_short_string;
  /* top->create = (create_func*) int_gloadableicon_create;*/

  /* specific methods for gloadableicon */

  type->init = (init_func *) init_gloadableicon;

  /* 
   * NspGLoadableIcon interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gloadableicon_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGLoadableIcon called nsp_type_gloadableicon
       */
      type->id =  nsp_type_gloadableicon_id = nsp_new_type_id();
      nsp_type_gloadableicon = type;
      if ( nsp_register_type(nsp_type_gloadableicon) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gloadableicon, G_TYPE_LOADABLE_ICON);
      return ( mode == T_BASE ) ? type : new_type_gloadableicon(mode);
    }
  else 
    {
      type->id = nsp_type_gloadableicon_id;
      return type;
    }
}

/*
 * initialize NspGLoadableIcon instances 
 * locally and by calling initializer on parent class 
 */

static int init_gloadableicon(NspGLoadableIcon *Obj,NspTypeGLoadableIcon *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGLoadableIcon 
 */

NspGLoadableIcon *new_gloadableicon() 
{
  NspGLoadableIcon *loc;
  /* type must exists */
  nsp_type_gloadableicon = new_type_gloadableicon(T_BASE);
  if ( (loc = malloc(sizeof(NspGLoadableIcon)))== NULLGLOADABLEICON) return loc;
  /* initialize object */
  if ( init_gloadableicon(loc,nsp_type_gloadableicon) == FAIL) return NULLGLOADABLEICON;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGLoadableIcon 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gloadableicon_type_name[]="GLoadableIcon";
static char gloadableicon_short_type_name[]="GLoadableIcon";

static char *nsp_gloadableicon_type_as_string(void)
{
  return(gloadableicon_type_name);
}

static char *nsp_gloadableicon_type_short_string(NspObject *v)
{
  return(gloadableicon_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGLoadableIcon objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGLoadableIcon   *nsp_gloadableicon_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gloadableicon_id)  == TRUE  ) return ((NspGLoadableIcon *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gloadableicon));
  return NULL;
}

int IsGLoadableIconObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gloadableicon_id);
}

int IsGLoadableIcon(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gloadableicon_id);
}

NspGLoadableIcon  *GetGLoadableIconCopy(Stack stack, int i)
{
  if (  GetGLoadableIcon(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGLoadableIcon  *GetGLoadableIcon(Stack stack, int i)
{
  NspGLoadableIcon *M;
  if (( M = nsp_gloadableicon_object(NthObj(i))) == NULLGLOADABLEICON)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGLoadableIcon *gloadableicon_copy(NspGLoadableIcon *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gloadableicon);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gloadableicon);
}

/*-------------------------------------------------------------------
 * wrappers for the GLoadableIcon
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_loadable_icon_load(NspGLoadableIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj,obj_check, t_end};
  int size;
  gchar **type = NULL;
  NspObject *nsp_type = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GInputStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&size, &nsp_type, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_type))
    { type =  ((NspSMatrix *) nsp_type)->S;}
  else
    {
      Scierror("Error: type should be of type SMat");
      return RET_BUG;
    }
    ret =g_loadable_icon_load(G_LOADABLE_ICON(self->obj),size,type,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_loadable_icon_load_finish(NspGLoadableIcon *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *res;
  gchar **type = NULL;
  NspObject *nsp_type = NULL, *nsp_ret;
  GError *error = NULL;
  GInputStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res, &nsp_type) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_type))
    { type =  ((NspSMatrix *) nsp_type)->S;}
  else
    {
      Scierror("Error: type should be of type SMat");
      return RET_BUG;
    }
    ret =g_loadable_icon_load_finish(G_LOADABLE_ICON(self->obj),G_ASYNC_RESULT(res->obj),type,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_ginputstream = new_type_ginputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gloadableicon_methods[] = {
  {"load",(nsp_method *) _wrap_g_loadable_icon_load},
  {"load_finish",(nsp_method *) _wrap_g_loadable_icon_load_finish},
  { NULL, NULL}
};

static NspMethods *gloadableicon_get_methods(void) { return gloadableicon_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gloadableicon_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMemoryInputStream ----------- */


#define  NspGMemoryInputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmemoryinputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMemoryInputStream inherits from GInputStream 
 */

int nsp_type_gmemoryinputstream_id=0;
NspTypeGMemoryInputStream *nsp_type_gmemoryinputstream=NULL;

/*
 * Type object for NspGMemoryInputStream 
 * all the instance of NspTypeGMemoryInputStream share the same id. 
 * nsp_type_gmemoryinputstream: is an instance of NspTypeGMemoryInputStream 
 *    used for objects of NspGMemoryInputStream type (i.e built with new_gmemoryinputstream) 
 * other instances are used for derived classes 
 */
NspTypeGMemoryInputStream *new_type_gmemoryinputstream(type_mode mode)
{
  NspTypeGMemoryInputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmemoryinputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmemoryinputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGInputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_ginputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmemoryinputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmemoryinputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmemoryinputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmemoryinputstream */ 

  top->s_type =  (s_type_func *) nsp_gmemoryinputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmemoryinputstream_type_short_string;
  /* top->create = (create_func*) int_gmemoryinputstream_create;*/

  /* specific methods for gmemoryinputstream */

  type->init = (init_func *) init_gmemoryinputstream;

  /* 
   * NspGMemoryInputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmemoryinputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMemoryInputStream called nsp_type_gmemoryinputstream
       */
      type->id =  nsp_type_gmemoryinputstream_id = nsp_new_type_id();
      nsp_type_gmemoryinputstream = type;
      if ( nsp_register_type(nsp_type_gmemoryinputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmemoryinputstream, G_TYPE_MEMORY_INPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gmemoryinputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gmemoryinputstream_id;
      return type;
    }
}

/*
 * initialize NspGMemoryInputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmemoryinputstream(NspGMemoryInputStream *Obj,NspTypeGMemoryInputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMemoryInputStream 
 */

NspGMemoryInputStream *new_gmemoryinputstream() 
{
  NspGMemoryInputStream *loc;
  /* type must exists */
  nsp_type_gmemoryinputstream = new_type_gmemoryinputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGMemoryInputStream)))== NULLGMEMORYINPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gmemoryinputstream(loc,nsp_type_gmemoryinputstream) == FAIL) return NULLGMEMORYINPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMemoryInputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmemoryinputstream_type_name[]="GMemoryInputStream";
static char gmemoryinputstream_short_type_name[]="GMemoryInputStream";

static char *nsp_gmemoryinputstream_type_as_string(void)
{
  return(gmemoryinputstream_type_name);
}

static char *nsp_gmemoryinputstream_type_short_string(NspObject *v)
{
  return(gmemoryinputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMemoryInputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMemoryInputStream   *nsp_gmemoryinputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmemoryinputstream_id)  == TRUE  ) return ((NspGMemoryInputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmemoryinputstream));
  return NULL;
}

int IsGMemoryInputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmemoryinputstream_id);
}

int IsGMemoryInputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmemoryinputstream_id);
}

NspGMemoryInputStream  *GetGMemoryInputStreamCopy(Stack stack, int i)
{
  if (  GetGMemoryInputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMemoryInputStream  *GetGMemoryInputStream(Stack stack, int i)
{
  NspGMemoryInputStream *M;
  if (( M = nsp_gmemoryinputstream_object(NthObj(i))) == NULLGMEMORYINPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMemoryInputStream *gmemoryinputstream_copy(NspGMemoryInputStream *self)
{
  /* return ginputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmemoryinputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmemoryinputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GMemoryInputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_memory_input_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_memory_input_stream_new())== NULL) return RET_BUG;

  nsp_type_gmemoryinputstream = new_type_gmemoryinputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gmemoryinputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gmemoryinputstream_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmemoryinputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMenuAttributeIter ----------- */


#define  NspGMenuAttributeIter_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmenuattributeiter.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMenuAttributeIter inherits from GObject 
 */

int nsp_type_gmenuattributeiter_id=0;
NspTypeGMenuAttributeIter *nsp_type_gmenuattributeiter=NULL;

/*
 * Type object for NspGMenuAttributeIter 
 * all the instance of NspTypeGMenuAttributeIter share the same id. 
 * nsp_type_gmenuattributeiter: is an instance of NspTypeGMenuAttributeIter 
 *    used for objects of NspGMenuAttributeIter type (i.e built with new_gmenuattributeiter) 
 * other instances are used for derived classes 
 */
NspTypeGMenuAttributeIter *new_type_gmenuattributeiter(type_mode mode)
{
  NspTypeGMenuAttributeIter *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmenuattributeiter != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmenuattributeiter;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmenuattributeiter_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmenuattributeiter_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmenuattributeiter;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmenuattributeiter */ 

  top->s_type =  (s_type_func *) nsp_gmenuattributeiter_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmenuattributeiter_type_short_string;
  /* top->create = (create_func*) int_gmenuattributeiter_create;*/

  /* specific methods for gmenuattributeiter */

  type->init = (init_func *) init_gmenuattributeiter;

  /* 
   * NspGMenuAttributeIter interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmenuattributeiter_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMenuAttributeIter called nsp_type_gmenuattributeiter
       */
      type->id =  nsp_type_gmenuattributeiter_id = nsp_new_type_id();
      nsp_type_gmenuattributeiter = type;
      if ( nsp_register_type(nsp_type_gmenuattributeiter) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmenuattributeiter, G_TYPE_MENU_ATTRIBUTE_ITER);
      return ( mode == T_BASE ) ? type : new_type_gmenuattributeiter(mode);
    }
  else 
    {
      type->id = nsp_type_gmenuattributeiter_id;
      return type;
    }
}

/*
 * initialize NspGMenuAttributeIter instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmenuattributeiter(NspGMenuAttributeIter *Obj,NspTypeGMenuAttributeIter *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMenuAttributeIter 
 */

NspGMenuAttributeIter *new_gmenuattributeiter() 
{
  NspGMenuAttributeIter *loc;
  /* type must exists */
  nsp_type_gmenuattributeiter = new_type_gmenuattributeiter(T_BASE);
  if ( (loc = malloc(sizeof(NspGMenuAttributeIter)))== NULLGMENUATTRIBUTEITER) return loc;
  /* initialize object */
  if ( init_gmenuattributeiter(loc,nsp_type_gmenuattributeiter) == FAIL) return NULLGMENUATTRIBUTEITER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMenuAttributeIter 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmenuattributeiter_type_name[]="GMenuAttributeIter";
static char gmenuattributeiter_short_type_name[]="GMenuAttributeIter";

static char *nsp_gmenuattributeiter_type_as_string(void)
{
  return(gmenuattributeiter_type_name);
}

static char *nsp_gmenuattributeiter_type_short_string(NspObject *v)
{
  return(gmenuattributeiter_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMenuAttributeIter objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMenuAttributeIter   *nsp_gmenuattributeiter_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmenuattributeiter_id)  == TRUE  ) return ((NspGMenuAttributeIter *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmenuattributeiter));
  return NULL;
}

int IsGMenuAttributeIterObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmenuattributeiter_id);
}

int IsGMenuAttributeIter(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmenuattributeiter_id);
}

NspGMenuAttributeIter  *GetGMenuAttributeIterCopy(Stack stack, int i)
{
  if (  GetGMenuAttributeIter(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMenuAttributeIter  *GetGMenuAttributeIter(Stack stack, int i)
{
  NspGMenuAttributeIter *M;
  if (( M = nsp_gmenuattributeiter_object(NthObj(i))) == NULLGMENUATTRIBUTEITER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMenuAttributeIter *gmenuattributeiter_copy(NspGMenuAttributeIter *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenuattributeiter);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenuattributeiter);
}

/*-------------------------------------------------------------------
 * wrappers for the GMenuAttributeIter
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_attribute_iter_next(NspGMenuAttributeIter *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_menu_attribute_iter_next(G_MENU_ATTRIBUTE_ITER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_menu_attribute_iter_next(Stack stack, int rhs, int opt, int lhs) /* next */
{
  Scierror("Error: function g_menu_attribute_iter_next not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_attribute_iter_get_name(NspGMenuAttributeIter *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_menu_attribute_iter_get_name(G_MENU_ATTRIBUTE_ITER(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_menu_attribute_iter_get_name(Stack stack, int rhs, int opt, int lhs) /* get_name */
{
  Scierror("Error: function g_menu_attribute_iter_get_name not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_attribute_iter_get_value(NspGMenuAttributeIter *self,Stack stack,int rhs,int opt,int lhs)
{
  GVariant *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_menu_attribute_iter_get_value(G_MENU_ATTRIBUTE_ITER(self->obj));
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_menu_attribute_iter_get_value(Stack stack, int rhs, int opt, int lhs) /* get_value */
{
  Scierror("Error: function g_menu_attribute_iter_get_value not available\n");
  return RET_BUG;
}
#endif
static NspMethods gmenuattributeiter_methods[] = {
  {"next",(nsp_method *) _wrap_g_menu_attribute_iter_next},
  {"get_name",(nsp_method *) _wrap_g_menu_attribute_iter_get_name},
  {"get_value",(nsp_method *) _wrap_g_menu_attribute_iter_get_value},
  { NULL, NULL}
};

static NspMethods *gmenuattributeiter_get_methods(void) { return gmenuattributeiter_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmenuattributeiter_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMenuLinkIter ----------- */


#define  NspGMenuLinkIter_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmenulinkiter.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMenuLinkIter inherits from GObject 
 */

int nsp_type_gmenulinkiter_id=0;
NspTypeGMenuLinkIter *nsp_type_gmenulinkiter=NULL;

/*
 * Type object for NspGMenuLinkIter 
 * all the instance of NspTypeGMenuLinkIter share the same id. 
 * nsp_type_gmenulinkiter: is an instance of NspTypeGMenuLinkIter 
 *    used for objects of NspGMenuLinkIter type (i.e built with new_gmenulinkiter) 
 * other instances are used for derived classes 
 */
NspTypeGMenuLinkIter *new_type_gmenulinkiter(type_mode mode)
{
  NspTypeGMenuLinkIter *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmenulinkiter != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmenulinkiter;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmenulinkiter_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmenulinkiter_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmenulinkiter;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmenulinkiter */ 

  top->s_type =  (s_type_func *) nsp_gmenulinkiter_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmenulinkiter_type_short_string;
  /* top->create = (create_func*) int_gmenulinkiter_create;*/

  /* specific methods for gmenulinkiter */

  type->init = (init_func *) init_gmenulinkiter;

  /* 
   * NspGMenuLinkIter interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmenulinkiter_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMenuLinkIter called nsp_type_gmenulinkiter
       */
      type->id =  nsp_type_gmenulinkiter_id = nsp_new_type_id();
      nsp_type_gmenulinkiter = type;
      if ( nsp_register_type(nsp_type_gmenulinkiter) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmenulinkiter, G_TYPE_MENU_LINK_ITER);
      return ( mode == T_BASE ) ? type : new_type_gmenulinkiter(mode);
    }
  else 
    {
      type->id = nsp_type_gmenulinkiter_id;
      return type;
    }
}

/*
 * initialize NspGMenuLinkIter instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmenulinkiter(NspGMenuLinkIter *Obj,NspTypeGMenuLinkIter *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMenuLinkIter 
 */

NspGMenuLinkIter *new_gmenulinkiter() 
{
  NspGMenuLinkIter *loc;
  /* type must exists */
  nsp_type_gmenulinkiter = new_type_gmenulinkiter(T_BASE);
  if ( (loc = malloc(sizeof(NspGMenuLinkIter)))== NULLGMENULINKITER) return loc;
  /* initialize object */
  if ( init_gmenulinkiter(loc,nsp_type_gmenulinkiter) == FAIL) return NULLGMENULINKITER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMenuLinkIter 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmenulinkiter_type_name[]="GMenuLinkIter";
static char gmenulinkiter_short_type_name[]="GMenuLinkIter";

static char *nsp_gmenulinkiter_type_as_string(void)
{
  return(gmenulinkiter_type_name);
}

static char *nsp_gmenulinkiter_type_short_string(NspObject *v)
{
  return(gmenulinkiter_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMenuLinkIter objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMenuLinkIter   *nsp_gmenulinkiter_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmenulinkiter_id)  == TRUE  ) return ((NspGMenuLinkIter *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmenulinkiter));
  return NULL;
}

int IsGMenuLinkIterObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmenulinkiter_id);
}

int IsGMenuLinkIter(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmenulinkiter_id);
}

NspGMenuLinkIter  *GetGMenuLinkIterCopy(Stack stack, int i)
{
  if (  GetGMenuLinkIter(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMenuLinkIter  *GetGMenuLinkIter(Stack stack, int i)
{
  NspGMenuLinkIter *M;
  if (( M = nsp_gmenulinkiter_object(NthObj(i))) == NULLGMENULINKITER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMenuLinkIter *gmenulinkiter_copy(NspGMenuLinkIter *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenulinkiter);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenulinkiter);
}

/*-------------------------------------------------------------------
 * wrappers for the GMenuLinkIter
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_link_iter_next(NspGMenuLinkIter *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_menu_link_iter_next(G_MENU_LINK_ITER(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_menu_link_iter_next(Stack stack, int rhs, int opt, int lhs) /* next */
{
  Scierror("Error: function g_menu_link_iter_next not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_link_iter_get_name(NspGMenuLinkIter *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_menu_link_iter_get_name(G_MENU_LINK_ITER(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_menu_link_iter_get_name(Stack stack, int rhs, int opt, int lhs) /* get_name */
{
  Scierror("Error: function g_menu_link_iter_get_name not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_link_iter_get_value(NspGMenuLinkIter *self,Stack stack,int rhs,int opt,int lhs)
{
  GMenuModel *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_menu_link_iter_get_value(G_MENU_LINK_ITER(self->obj));
  nsp_type_gmenumodel = new_type_gmenumodel(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmenumodel))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_menu_link_iter_get_value(Stack stack, int rhs, int opt, int lhs) /* get_value */
{
  Scierror("Error: function g_menu_link_iter_get_value not available\n");
  return RET_BUG;
}
#endif
static NspMethods gmenulinkiter_methods[] = {
  {"next",(nsp_method *) _wrap_g_menu_link_iter_next},
  {"get_name",(nsp_method *) _wrap_g_menu_link_iter_get_name},
  {"get_value",(nsp_method *) _wrap_g_menu_link_iter_get_value},
  { NULL, NULL}
};

static NspMethods *gmenulinkiter_get_methods(void) { return gmenulinkiter_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmenulinkiter_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMenuModel ----------- */


#define  NspGMenuModel_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmenumodel.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMenuModel inherits from GObject 
 */

int nsp_type_gmenumodel_id=0;
NspTypeGMenuModel *nsp_type_gmenumodel=NULL;

/*
 * Type object for NspGMenuModel 
 * all the instance of NspTypeGMenuModel share the same id. 
 * nsp_type_gmenumodel: is an instance of NspTypeGMenuModel 
 *    used for objects of NspGMenuModel type (i.e built with new_gmenumodel) 
 * other instances are used for derived classes 
 */
NspTypeGMenuModel *new_type_gmenumodel(type_mode mode)
{
  NspTypeGMenuModel *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmenumodel != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmenumodel;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmenumodel_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmenumodel_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmenumodel;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmenumodel */ 

  top->s_type =  (s_type_func *) nsp_gmenumodel_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmenumodel_type_short_string;
  /* top->create = (create_func*) int_gmenumodel_create;*/

  /* specific methods for gmenumodel */

  type->init = (init_func *) init_gmenumodel;

  /* 
   * NspGMenuModel interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmenumodel_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMenuModel called nsp_type_gmenumodel
       */
      type->id =  nsp_type_gmenumodel_id = nsp_new_type_id();
      nsp_type_gmenumodel = type;
      if ( nsp_register_type(nsp_type_gmenumodel) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmenumodel, G_TYPE_MENU_MODEL);
      return ( mode == T_BASE ) ? type : new_type_gmenumodel(mode);
    }
  else 
    {
      type->id = nsp_type_gmenumodel_id;
      return type;
    }
}

/*
 * initialize NspGMenuModel instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmenumodel(NspGMenuModel *Obj,NspTypeGMenuModel *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMenuModel 
 */

NspGMenuModel *new_gmenumodel() 
{
  NspGMenuModel *loc;
  /* type must exists */
  nsp_type_gmenumodel = new_type_gmenumodel(T_BASE);
  if ( (loc = malloc(sizeof(NspGMenuModel)))== NULLGMENUMODEL) return loc;
  /* initialize object */
  if ( init_gmenumodel(loc,nsp_type_gmenumodel) == FAIL) return NULLGMENUMODEL;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMenuModel 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmenumodel_type_name[]="GMenuModel";
static char gmenumodel_short_type_name[]="GMenuModel";

static char *nsp_gmenumodel_type_as_string(void)
{
  return(gmenumodel_type_name);
}

static char *nsp_gmenumodel_type_short_string(NspObject *v)
{
  return(gmenumodel_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMenuModel objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMenuModel   *nsp_gmenumodel_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmenumodel_id)  == TRUE  ) return ((NspGMenuModel *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmenumodel));
  return NULL;
}

int IsGMenuModelObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmenumodel_id);
}

int IsGMenuModel(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmenumodel_id);
}

NspGMenuModel  *GetGMenuModelCopy(Stack stack, int i)
{
  if (  GetGMenuModel(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMenuModel  *GetGMenuModel(Stack stack, int i)
{
  NspGMenuModel *M;
  if (( M = nsp_gmenumodel_object(NthObj(i))) == NULLGMENUMODEL)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMenuModel *gmenumodel_copy(NspGMenuModel *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenumodel);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenumodel);
}

/*-------------------------------------------------------------------
 * wrappers for the GMenuModel
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_is_mutable(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_menu_model_is_mutable(G_MENU_MODEL(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_menu_model_is_mutable(Stack stack, int rhs, int opt, int lhs) /* is_mutable */
{
  Scierror("Error: function g_menu_model_is_mutable not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_get_n_items(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_menu_model_get_n_items(G_MENU_MODEL(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_menu_model_get_n_items(Stack stack, int rhs, int opt, int lhs) /* get_n_items */
{
  Scierror("Error: function g_menu_model_get_n_items not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_iterate_item_attributes(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int item_index;
  GMenuAttributeIter *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&item_index) == FAIL) return RET_BUG;
    ret =g_menu_model_iterate_item_attributes(G_MENU_MODEL(self->obj),item_index);
  nsp_type_gmenuattributeiter = new_type_gmenuattributeiter(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmenuattributeiter))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_menu_model_iterate_item_attributes(Stack stack, int rhs, int opt, int lhs) /* iterate_item_attributes */
{
  Scierror("Error: function g_menu_model_iterate_item_attributes not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_get_item_attribute_value(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string,new_opts, t_end};
  nsp_option opts[] = {
	{"expected_type",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  int item_index;
  char *attribute;
  GVariantType *expected_type = NULL;
  NspObject *nsp_expected_type = NULL, *nsp_ret;
  GVariant *ret;
  if ( GetArgs(stack,rhs,opt,T,&item_index, &attribute, opts, &nsp_expected_type) == FAIL) return RET_BUG;
  if (nsp_expected_type != NULL)
     {
      if( IsGVariantType(nsp_expected_type))
        { expected_type = ((NspGVariantType *) nsp_expected_type)->obj->value;}
      else
        {Scierror("Error: expected_type should be of type GVariantType\n");
         return RET_BUG;
        }
      if((expected_type = nsp_copy_GVariantType(expected_type))==NULL) return RET_BUG;
     }
    ret =g_menu_model_get_item_attribute_value(G_MENU_MODEL(self->obj),item_index,attribute,expected_type);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_menu_model_get_item_attribute_value(Stack stack, int rhs, int opt, int lhs) /* get_item_attribute_value */
{
  Scierror("Error: function g_menu_model_get_item_attribute_value not available\n");
  return RET_BUG;
}
#endif
#line 40 "codegen-3.0/gio.override"

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_get_item_attribute(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *rep = NULL;
  int_types T[] = {s_int,string,string, t_end};
  int item_index, ret;
  char *attribute, *format_string;
  CheckLhs(1,2);
  if ( GetArgs(stack,rhs,opt,T,&item_index, &attribute, &format_string) == FAIL) return RET_BUG;
  if ( item_index < 0 || item_index >= g_menu_model_get_n_items (G_MENU_MODEL(self->obj)))
    {
      Scierror("Error: item should be in [0,%d)\n",g_menu_model_get_n_items (G_MENU_MODEL(self->obj)));
      return RET_BUG;
    }
  ret =g_menu_model_get_item_attribute(G_MENU_MODEL(self->obj),item_index,attribute,format_string,&rep);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  if ( lhs == 2 )
    {
      nsp_move_string(stack,2,rep == NULL ? "":rep,-1);
    }
  return Max(lhs,1);
}

#else
int _wrap_g_menu_model_get_item_attribute(Stack stack, int rhs, int opt, int lhs) /* get_item_attribute */
{
  Scierror("Error: function g_menu_model_get_item_attribute not available\n");
  return RET_BUG;
}
#endif

#line 13354 "gio.c"


#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_iterate_item_links(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int item_index;
  GMenuLinkIter *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&item_index) == FAIL) return RET_BUG;
    ret =g_menu_model_iterate_item_links(G_MENU_MODEL(self->obj),item_index);
  nsp_type_gmenulinkiter = new_type_gmenulinkiter(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmenulinkiter))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_menu_model_iterate_item_links(Stack stack, int rhs, int opt, int lhs) /* iterate_item_links */
{
  Scierror("Error: function g_menu_model_iterate_item_links not available\n");
  return RET_BUG;
}
#endif
#line 100 "codegen-3.0/gio.override"

/* XXX we want to return the most specialized type
 * i.e use nspgobject_new and not gobject_create
 * XXX this should be the generic rule for all methods ?
 */

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_get_item_link(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string, t_end};
  int item_index;
  char *link;
  GMenuModel *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&item_index, &link) == FAIL) return RET_BUG;
    ret =g_menu_model_get_item_link(G_MENU_MODEL(self->obj),item_index,link);
  nsp_type_gmenumodel = new_type_gmenumodel(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *)ret))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_menu_model_get_item_link(Stack stack, int rhs, int opt, int lhs) /* get_item_link */
{
  Scierror("Error: function g_menu_model_get_item_link not available\n");
  return RET_BUG;
}
#endif

#line 13410 "gio.c"


#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_model_items_changed(NspGMenuModel *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int, t_end};
  int position, removed, added;
  if ( GetArgs(stack,rhs,opt,T,&position, &removed, &added) == FAIL) return RET_BUG;
    g_menu_model_items_changed(G_MENU_MODEL(self->obj),position,removed,added);
  return 0;
}

#else
int _wrap_g_menu_model_items_changed(Stack stack, int rhs, int opt, int lhs) /* items_changed */
{
  Scierror("Error: function g_menu_model_items_changed not available\n");
  return RET_BUG;
}
#endif
static NspMethods gmenumodel_methods[] = {
  {"is_mutable",(nsp_method *) _wrap_g_menu_model_is_mutable},
  {"get_n_items",(nsp_method *) _wrap_g_menu_model_get_n_items},
  {"iterate_item_attributes",(nsp_method *) _wrap_g_menu_model_iterate_item_attributes},
  {"get_item_attribute_value",(nsp_method *) _wrap_g_menu_model_get_item_attribute_value},
  {"get_item_attribute",(nsp_method *) _wrap_g_menu_model_get_item_attribute},
  {"iterate_item_links",(nsp_method *) _wrap_g_menu_model_iterate_item_links},
  {"get_item_link",(nsp_method *) _wrap_g_menu_model_get_item_link},
  {"items_changed",(nsp_method *) _wrap_g_menu_model_items_changed},
  { NULL, NULL}
};

static NspMethods *gmenumodel_get_methods(void) { return gmenumodel_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmenumodel_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMenu ----------- */


#define  NspGMenu_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmenu.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMenu inherits from GMenuModel 
 */

int nsp_type_gmenu_id=0;
NspTypeGMenu *nsp_type_gmenu=NULL;

/*
 * Type object for NspGMenu 
 * all the instance of NspTypeGMenu share the same id. 
 * nsp_type_gmenu: is an instance of NspTypeGMenu 
 *    used for objects of NspGMenu type (i.e built with new_gmenu) 
 * other instances are used for derived classes 
 */
NspTypeGMenu *new_type_gmenu(type_mode mode)
{
  NspTypeGMenu *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmenu != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmenu;
    }
  if (( type =  malloc(sizeof(NspTypeGMenuModel))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gmenumodel(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmenu_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmenu_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmenu;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmenu */ 

  top->s_type =  (s_type_func *) nsp_gmenu_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmenu_type_short_string;
  /* top->create = (create_func*) int_gmenu_create;*/

  /* specific methods for gmenu */

  type->init = (init_func *) init_gmenu;

  /* 
   * NspGMenu interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmenu_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMenu called nsp_type_gmenu
       */
      type->id =  nsp_type_gmenu_id = nsp_new_type_id();
      nsp_type_gmenu = type;
      if ( nsp_register_type(nsp_type_gmenu) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmenu, G_TYPE_MENU);
      return ( mode == T_BASE ) ? type : new_type_gmenu(mode);
    }
  else 
    {
      type->id = nsp_type_gmenu_id;
      return type;
    }
}

/*
 * initialize NspGMenu instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmenu(NspGMenu *Obj,NspTypeGMenu *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMenu 
 */

NspGMenu *new_gmenu() 
{
  NspGMenu *loc;
  /* type must exists */
  nsp_type_gmenu = new_type_gmenu(T_BASE);
  if ( (loc = malloc(sizeof(NspGMenu)))== NULLGMENU) return loc;
  /* initialize object */
  if ( init_gmenu(loc,nsp_type_gmenu) == FAIL) return NULLGMENU;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMenu 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmenu_type_name[]="GMenu";
static char gmenu_short_type_name[]="GMenu";

static char *nsp_gmenu_type_as_string(void)
{
  return(gmenu_type_name);
}

static char *nsp_gmenu_type_short_string(NspObject *v)
{
  return(gmenu_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMenu objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMenu   *nsp_gmenu_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmenu_id)  == TRUE  ) return ((NspGMenu *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmenu));
  return NULL;
}

int IsGMenuObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmenu_id);
}

int IsGMenu(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmenu_id);
}

NspGMenu  *GetGMenuCopy(Stack stack, int i)
{
  if (  GetGMenu(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMenu  *GetGMenu(Stack stack, int i)
{
  NspGMenu *M;
  if (( M = nsp_gmenu_object(NthObj(i))) == NULLGMENU)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMenu *gmenu_copy(NspGMenu *self)
{
  /* return gmenumodel_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenu);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmenu);
}

/*-------------------------------------------------------------------
 * wrappers for the GMenu
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_menu_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_menu_new())== NULL) return RET_BUG;

  nsp_type_gmenu = new_type_gmenu(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gmenu );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_freeze(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_menu_freeze(G_MENU(self->obj));
  return 0;
}

#else
int _wrap_g_menu_freeze(Stack stack, int rhs, int opt, int lhs) /* freeze */
{
  Scierror("Error: function g_menu_freeze not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_remove(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int position;
  if ( GetArgs(stack,rhs,opt,T,&position) == FAIL) return RET_BUG;
    g_menu_remove(G_MENU(self->obj),position);
  return 0;
}

#else
int _wrap_g_menu_remove(Stack stack, int rhs, int opt, int lhs) /* remove */
{
  Scierror("Error: function g_menu_remove not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,38,0)
static int _wrap_g_menu_remove_all(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_menu_remove_all(G_MENU(self->obj));
  return 0;
}

#else
int _wrap_g_menu_remove_all(Stack stack, int rhs, int opt, int lhs) /* remove_all */
{
  Scierror("Error: function g_menu_remove_all not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_insert(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string,string, t_end};
  int position;
  char *label, *detailed_action;
  if ( GetArgs(stack,rhs,opt,T,&position, &label, &detailed_action) == FAIL) return RET_BUG;
    g_menu_insert(G_MENU(self->obj),position,label,detailed_action);
  return 0;
}

#else
int _wrap_g_menu_insert(Stack stack, int rhs, int opt, int lhs) /* insert */
{
  Scierror("Error: function g_menu_insert not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_prepend(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *label, *detailed_action;
  if ( GetArgs(stack,rhs,opt,T,&label, &detailed_action) == FAIL) return RET_BUG;
    g_menu_prepend(G_MENU(self->obj),label,detailed_action);
  return 0;
}

#else
int _wrap_g_menu_prepend(Stack stack, int rhs, int opt, int lhs) /* prepend */
{
  Scierror("Error: function g_menu_prepend not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_append(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *label, *detailed_action;
  if ( GetArgs(stack,rhs,opt,T,&label, &detailed_action) == FAIL) return RET_BUG;
    g_menu_append(G_MENU(self->obj),label,detailed_action);
  return 0;
}

#else
int _wrap_g_menu_append(Stack stack, int rhs, int opt, int lhs) /* append */
{
  Scierror("Error: function g_menu_append not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_insert_section(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string,obj_check, t_end};
  int position;
  char *label;
  NspGObject *section;
  if ( GetArgs(stack,rhs,opt,T,&position, &label, &nsp_type_gmenumodel, &section) == FAIL) return RET_BUG;
    g_menu_insert_section(G_MENU(self->obj),position,label,G_MENU_MODEL(section->obj));
  return 0;
}

#else
int _wrap_g_menu_insert_section(Stack stack, int rhs, int opt, int lhs) /* insert_section */
{
  Scierror("Error: function g_menu_insert_section not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_prepend_section(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *label;
  NspGObject *section;
  if ( GetArgs(stack,rhs,opt,T,&label, &nsp_type_gmenumodel, &section) == FAIL) return RET_BUG;
    g_menu_prepend_section(G_MENU(self->obj),label,G_MENU_MODEL(section->obj));
  return 0;
}

#else
int _wrap_g_menu_prepend_section(Stack stack, int rhs, int opt, int lhs) /* prepend_section */
{
  Scierror("Error: function g_menu_prepend_section not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_append_section(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *label;
  NspGObject *section;
  if ( GetArgs(stack,rhs,opt,T,&label, &nsp_type_gmenumodel, &section) == FAIL) return RET_BUG;
    g_menu_append_section(G_MENU(self->obj),label,G_MENU_MODEL(section->obj));
  return 0;
}

#else
int _wrap_g_menu_append_section(Stack stack, int rhs, int opt, int lhs) /* append_section */
{
  Scierror("Error: function g_menu_append_section not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_insert_submenu(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,string,obj_check, t_end};
  int position;
  char *label;
  NspGObject *submenu;
  if ( GetArgs(stack,rhs,opt,T,&position, &label, &nsp_type_gmenumodel, &submenu) == FAIL) return RET_BUG;
    g_menu_insert_submenu(G_MENU(self->obj),position,label,G_MENU_MODEL(submenu->obj));
  return 0;
}

#else
int _wrap_g_menu_insert_submenu(Stack stack, int rhs, int opt, int lhs) /* insert_submenu */
{
  Scierror("Error: function g_menu_insert_submenu not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_prepend_submenu(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *label;
  NspGObject *submenu;
  if ( GetArgs(stack,rhs,opt,T,&label, &nsp_type_gmenumodel, &submenu) == FAIL) return RET_BUG;
    g_menu_prepend_submenu(G_MENU(self->obj),label,G_MENU_MODEL(submenu->obj));
  return 0;
}

#else
int _wrap_g_menu_prepend_submenu(Stack stack, int rhs, int opt, int lhs) /* prepend_submenu */
{
  Scierror("Error: function g_menu_prepend_submenu not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_menu_append_submenu(NspGMenu *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *label;
  NspGObject *submenu;
  if ( GetArgs(stack,rhs,opt,T,&label, &nsp_type_gmenumodel, &submenu) == FAIL) return RET_BUG;
    g_menu_append_submenu(G_MENU(self->obj),label,G_MENU_MODEL(submenu->obj));
  return 0;
}

#else
int _wrap_g_menu_append_submenu(Stack stack, int rhs, int opt, int lhs) /* append_submenu */
{
  Scierror("Error: function g_menu_append_submenu not available\n");
  return RET_BUG;
}
#endif
static NspMethods gmenu_methods[] = {
  {"freeze",(nsp_method *) _wrap_g_menu_freeze},
  {"remove",(nsp_method *) _wrap_g_menu_remove},
  {"remove_all",(nsp_method *) _wrap_g_menu_remove_all},
  {"insert",(nsp_method *) _wrap_g_menu_insert},
  {"prepend",(nsp_method *) _wrap_g_menu_prepend},
  {"append",(nsp_method *) _wrap_g_menu_append},
  {"insert_section",(nsp_method *) _wrap_g_menu_insert_section},
  {"prepend_section",(nsp_method *) _wrap_g_menu_prepend_section},
  {"append_section",(nsp_method *) _wrap_g_menu_append_section},
  {"insert_submenu",(nsp_method *) _wrap_g_menu_insert_submenu},
  {"prepend_submenu",(nsp_method *) _wrap_g_menu_prepend_submenu},
  {"append_submenu",(nsp_method *) _wrap_g_menu_append_submenu},
  { NULL, NULL}
};

static NspMethods *gmenu_get_methods(void) { return gmenu_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmenu_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMount ----------- */


#define  NspGMount_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmount.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMount inherits from GObject 
 */

int nsp_type_gmount_id=0;
NspTypeGMount *nsp_type_gmount=NULL;

/*
 * Type object for NspGMount 
 * all the instance of NspTypeGMount share the same id. 
 * nsp_type_gmount: is an instance of NspTypeGMount 
 *    used for objects of NspGMount type (i.e built with new_gmount) 
 * other instances are used for derived classes 
 */
NspTypeGMount *new_type_gmount(type_mode mode)
{
  NspTypeGMount *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmount != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmount;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmount_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmount_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmount;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmount */ 

  top->s_type =  (s_type_func *) nsp_gmount_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmount_type_short_string;
  /* top->create = (create_func*) int_gmount_create;*/

  /* specific methods for gmount */

  type->init = (init_func *) init_gmount;

  /* 
   * NspGMount interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmount_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMount called nsp_type_gmount
       */
      type->id =  nsp_type_gmount_id = nsp_new_type_id();
      nsp_type_gmount = type;
      if ( nsp_register_type(nsp_type_gmount) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmount, G_TYPE_MOUNT);
      return ( mode == T_BASE ) ? type : new_type_gmount(mode);
    }
  else 
    {
      type->id = nsp_type_gmount_id;
      return type;
    }
}

/*
 * initialize NspGMount instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmount(NspGMount *Obj,NspTypeGMount *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMount 
 */

NspGMount *new_gmount() 
{
  NspGMount *loc;
  /* type must exists */
  nsp_type_gmount = new_type_gmount(T_BASE);
  if ( (loc = malloc(sizeof(NspGMount)))== NULLGMOUNT) return loc;
  /* initialize object */
  if ( init_gmount(loc,nsp_type_gmount) == FAIL) return NULLGMOUNT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMount 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmount_type_name[]="GMount";
static char gmount_short_type_name[]="GMount";

static char *nsp_gmount_type_as_string(void)
{
  return(gmount_type_name);
}

static char *nsp_gmount_type_short_string(NspObject *v)
{
  return(gmount_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMount objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMount   *nsp_gmount_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmount_id)  == TRUE  ) return ((NspGMount *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmount));
  return NULL;
}

int IsGMountObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmount_id);
}

int IsGMount(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmount_id);
}

NspGMount  *GetGMountCopy(Stack stack, int i)
{
  if (  GetGMount(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMount  *GetGMount(Stack stack, int i)
{
  NspGMount *M;
  if (( M = nsp_gmount_object(NthObj(i))) == NULLGMOUNT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMount *gmount_copy(NspGMount *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmount);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmount);
}

/*-------------------------------------------------------------------
 * wrappers for the GMount
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_mount_get_root(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_mount_get_root(G_MOUNT(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_get_default_location(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_mount_get_default_location(G_MOUNT(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_get_name(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_mount_get_name(G_MOUNT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_mount_get_icon(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_mount_get_icon(G_MOUNT(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_get_symbolic_icon(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_mount_get_symbolic_icon(G_MOUNT(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_get_uuid(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_mount_get_uuid(G_MOUNT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_mount_get_volume(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  GVolume *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_mount_get_volume(G_MOUNT(self->obj));
  nsp_type_gvolume = new_type_gvolume(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gvolume))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_get_drive(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  GDrive *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_mount_get_drive(G_MOUNT(self->obj));
  nsp_type_gdrive = new_type_gdrive(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdrive))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_can_unmount(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_mount_can_unmount(G_MOUNT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_can_eject(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_mount_can_eject(G_MOUNT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_remount_finish(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_mount_remount_finish(G_MOUNT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_guess_content_type_finish(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_mount_guess_content_type_finish(G_MOUNT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_guess_content_type_sync(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,obj_check, t_end};
  int force_rescan;
  NspGObject *cancellable;
  GError *error = NULL;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&force_rescan, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_mount_guess_content_type_sync(G_MOUNT(self->obj),force_rescan,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_is_shadowed(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_mount_is_shadowed(G_MOUNT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_shadow(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_mount_shadow(G_MOUNT(self->obj));
  return 0;
}

static int _wrap_g_mount_unshadow(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_mount_unshadow(G_MOUNT(self->obj));
  return 0;
}

static int _wrap_g_mount_unmount_with_operation_finish(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_mount_unmount_with_operation_finish(G_MOUNT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_eject_with_operation_finish(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_mount_eject_with_operation_finish(G_MOUNT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_get_sort_key(NspGMount *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_mount_get_sort_key(G_MOUNT(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gmount_methods[] = {
  {"get_root",(nsp_method *) _wrap_g_mount_get_root},
  {"get_default_location",(nsp_method *) _wrap_g_mount_get_default_location},
  {"get_name",(nsp_method *) _wrap_g_mount_get_name},
  {"get_icon",(nsp_method *) _wrap_g_mount_get_icon},
  {"get_symbolic_icon",(nsp_method *) _wrap_g_mount_get_symbolic_icon},
  {"get_uuid",(nsp_method *) _wrap_g_mount_get_uuid},
  {"get_volume",(nsp_method *) _wrap_g_mount_get_volume},
  {"get_drive",(nsp_method *) _wrap_g_mount_get_drive},
  {"can_unmount",(nsp_method *) _wrap_g_mount_can_unmount},
  {"can_eject",(nsp_method *) _wrap_g_mount_can_eject},
  {"remount_finish",(nsp_method *) _wrap_g_mount_remount_finish},
  {"guess_content_type_finish",(nsp_method *) _wrap_g_mount_guess_content_type_finish},
  {"guess_content_type_sync",(nsp_method *) _wrap_g_mount_guess_content_type_sync},
  {"is_shadowed",(nsp_method *) _wrap_g_mount_is_shadowed},
  {"shadow",(nsp_method *) _wrap_g_mount_shadow},
  {"unshadow",(nsp_method *) _wrap_g_mount_unshadow},
  {"unmount_with_operation_finish",(nsp_method *) _wrap_g_mount_unmount_with_operation_finish},
  {"eject_with_operation_finish",(nsp_method *) _wrap_g_mount_eject_with_operation_finish},
  {"get_sort_key",(nsp_method *) _wrap_g_mount_get_sort_key},
  { NULL, NULL}
};

static NspMethods *gmount_get_methods(void) { return gmount_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmount_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMountOperation ----------- */


#define  NspGMountOperation_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmountoperation.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMountOperation inherits from GObject 
 */

int nsp_type_gmountoperation_id=0;
NspTypeGMountOperation *nsp_type_gmountoperation=NULL;

/*
 * Type object for NspGMountOperation 
 * all the instance of NspTypeGMountOperation share the same id. 
 * nsp_type_gmountoperation: is an instance of NspTypeGMountOperation 
 *    used for objects of NspGMountOperation type (i.e built with new_gmountoperation) 
 * other instances are used for derived classes 
 */
NspTypeGMountOperation *new_type_gmountoperation(type_mode mode)
{
  NspTypeGMountOperation *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmountoperation != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmountoperation;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmountoperation_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmountoperation_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmountoperation;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmountoperation */ 

  top->s_type =  (s_type_func *) nsp_gmountoperation_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmountoperation_type_short_string;
  /* top->create = (create_func*) int_gmountoperation_create;*/

  /* specific methods for gmountoperation */

  type->init = (init_func *) init_gmountoperation;

  /* 
   * NspGMountOperation interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmountoperation_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMountOperation called nsp_type_gmountoperation
       */
      type->id =  nsp_type_gmountoperation_id = nsp_new_type_id();
      nsp_type_gmountoperation = type;
      if ( nsp_register_type(nsp_type_gmountoperation) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmountoperation, G_TYPE_MOUNT_OPERATION);
      return ( mode == T_BASE ) ? type : new_type_gmountoperation(mode);
    }
  else 
    {
      type->id = nsp_type_gmountoperation_id;
      return type;
    }
}

/*
 * initialize NspGMountOperation instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmountoperation(NspGMountOperation *Obj,NspTypeGMountOperation *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMountOperation 
 */

NspGMountOperation *new_gmountoperation() 
{
  NspGMountOperation *loc;
  /* type must exists */
  nsp_type_gmountoperation = new_type_gmountoperation(T_BASE);
  if ( (loc = malloc(sizeof(NspGMountOperation)))== NULLGMOUNTOPERATION) return loc;
  /* initialize object */
  if ( init_gmountoperation(loc,nsp_type_gmountoperation) == FAIL) return NULLGMOUNTOPERATION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMountOperation 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmountoperation_type_name[]="GMountOperation";
static char gmountoperation_short_type_name[]="GMountOperation";

static char *nsp_gmountoperation_type_as_string(void)
{
  return(gmountoperation_type_name);
}

static char *nsp_gmountoperation_type_short_string(NspObject *v)
{
  return(gmountoperation_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMountOperation objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMountOperation   *nsp_gmountoperation_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmountoperation_id)  == TRUE  ) return ((NspGMountOperation *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmountoperation));
  return NULL;
}

int IsGMountOperationObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmountoperation_id);
}

int IsGMountOperation(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmountoperation_id);
}

NspGMountOperation  *GetGMountOperationCopy(Stack stack, int i)
{
  if (  GetGMountOperation(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMountOperation  *GetGMountOperation(Stack stack, int i)
{
  NspGMountOperation *M;
  if (( M = nsp_gmountoperation_object(NthObj(i))) == NULLGMOUNTOPERATION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMountOperation *gmountoperation_copy(NspGMountOperation *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmountoperation);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmountoperation);
}

/*-------------------------------------------------------------------
 * wrappers for the GMountOperation
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_mount_operation_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_mount_operation_new())== NULL) return RET_BUG;

  nsp_type_gmountoperation = new_type_gmountoperation(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gmountoperation );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_mount_operation_get_username(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_mount_operation_get_username(G_MOUNT_OPERATION(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_operation_set_username(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *username;
  if ( GetArgs(stack,rhs,opt,T,&username) == FAIL) return RET_BUG;
    g_mount_operation_set_username(G_MOUNT_OPERATION(self->obj),username);
  return 0;
}

static int _wrap_g_mount_operation_get_password(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_mount_operation_get_password(G_MOUNT_OPERATION(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_operation_set_password(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *password;
  if ( GetArgs(stack,rhs,opt,T,&password) == FAIL) return RET_BUG;
    g_mount_operation_set_password(G_MOUNT_OPERATION(self->obj),password);
  return 0;
}

static int _wrap_g_mount_operation_get_anonymous(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_mount_operation_get_anonymous(G_MOUNT_OPERATION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_operation_set_anonymous(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int anonymous;
  if ( GetArgs(stack,rhs,opt,T,&anonymous) == FAIL) return RET_BUG;
    g_mount_operation_set_anonymous(G_MOUNT_OPERATION(self->obj),anonymous);
  return 0;
}

static int _wrap_g_mount_operation_get_domain(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_mount_operation_get_domain(G_MOUNT_OPERATION(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_operation_set_domain(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *domain;
  if ( GetArgs(stack,rhs,opt,T,&domain) == FAIL) return RET_BUG;
    g_mount_operation_set_domain(G_MOUNT_OPERATION(self->obj),domain);
  return 0;
}

static int _wrap_g_mount_operation_get_password_save(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_mount_operation_get_password_save(G_MOUNT_OPERATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_operation_set_password_save(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GPasswordSave save;
  NspObject *nsp_save = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_save) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_PASSWORD_SAVE, nsp_save, &save)== FAIL)
      return RET_BUG;
    g_mount_operation_set_password_save(G_MOUNT_OPERATION(self->obj),save);
  return 0;
}

static int _wrap_g_mount_operation_get_choice(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_mount_operation_get_choice(G_MOUNT_OPERATION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_mount_operation_set_choice(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int choice;
  if ( GetArgs(stack,rhs,opt,T,&choice) == FAIL) return RET_BUG;
    g_mount_operation_set_choice(G_MOUNT_OPERATION(self->obj),choice);
  return 0;
}

static int _wrap_g_mount_operation_reply(NspGMountOperation *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GMountOperationResult result;
  NspObject *nsp_result = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_result) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_MOUNT_OPERATION_RESULT, nsp_result, &result)== FAIL)
      return RET_BUG;
    g_mount_operation_reply(G_MOUNT_OPERATION(self->obj),result);
  return 0;
}

static NspMethods gmountoperation_methods[] = {
  {"get_username",(nsp_method *) _wrap_g_mount_operation_get_username},
  {"set_username",(nsp_method *) _wrap_g_mount_operation_set_username},
  {"get_password",(nsp_method *) _wrap_g_mount_operation_get_password},
  {"set_password",(nsp_method *) _wrap_g_mount_operation_set_password},
  {"get_anonymous",(nsp_method *) _wrap_g_mount_operation_get_anonymous},
  {"set_anonymous",(nsp_method *) _wrap_g_mount_operation_set_anonymous},
  {"get_domain",(nsp_method *) _wrap_g_mount_operation_get_domain},
  {"set_domain",(nsp_method *) _wrap_g_mount_operation_set_domain},
  {"get_password_save",(nsp_method *) _wrap_g_mount_operation_get_password_save},
  {"set_password_save",(nsp_method *) _wrap_g_mount_operation_set_password_save},
  {"get_choice",(nsp_method *) _wrap_g_mount_operation_get_choice},
  {"set_choice",(nsp_method *) _wrap_g_mount_operation_set_choice},
  {"reply",(nsp_method *) _wrap_g_mount_operation_reply},
  { NULL, NULL}
};

static NspMethods *gmountoperation_get_methods(void) { return gmountoperation_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmountoperation_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGNetworkAddress ----------- */


#define  NspGNetworkAddress_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gnetworkaddress.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGNetworkAddress inherits from GObject 
 */

int nsp_type_gnetworkaddress_id=0;
NspTypeGNetworkAddress *nsp_type_gnetworkaddress=NULL;

/*
 * Type object for NspGNetworkAddress 
 * all the instance of NspTypeGNetworkAddress share the same id. 
 * nsp_type_gnetworkaddress: is an instance of NspTypeGNetworkAddress 
 *    used for objects of NspGNetworkAddress type (i.e built with new_gnetworkaddress) 
 * other instances are used for derived classes 
 */
NspTypeGNetworkAddress *new_type_gnetworkaddress(type_mode mode)
{
  NspTypeGNetworkAddress *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gnetworkaddress != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gnetworkaddress;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gnetworkaddress_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gnetworkaddress_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gnetworkaddress;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gnetworkaddress */ 

  top->s_type =  (s_type_func *) nsp_gnetworkaddress_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gnetworkaddress_type_short_string;
  /* top->create = (create_func*) int_gnetworkaddress_create;*/

  /* specific methods for gnetworkaddress */

  type->init = (init_func *) init_gnetworkaddress;

  /* 
   * NspGNetworkAddress interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gnetworkaddress_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGNetworkAddress called nsp_type_gnetworkaddress
       */
      type->id =  nsp_type_gnetworkaddress_id = nsp_new_type_id();
      nsp_type_gnetworkaddress = type;
      if ( nsp_register_type(nsp_type_gnetworkaddress) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gnetworkaddress, G_TYPE_NETWORK_ADDRESS);
      return ( mode == T_BASE ) ? type : new_type_gnetworkaddress(mode);
    }
  else 
    {
      type->id = nsp_type_gnetworkaddress_id;
      return type;
    }
}

/*
 * initialize NspGNetworkAddress instances 
 * locally and by calling initializer on parent class 
 */

static int init_gnetworkaddress(NspGNetworkAddress *Obj,NspTypeGNetworkAddress *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGNetworkAddress 
 */

NspGNetworkAddress *new_gnetworkaddress() 
{
  NspGNetworkAddress *loc;
  /* type must exists */
  nsp_type_gnetworkaddress = new_type_gnetworkaddress(T_BASE);
  if ( (loc = malloc(sizeof(NspGNetworkAddress)))== NULLGNETWORKADDRESS) return loc;
  /* initialize object */
  if ( init_gnetworkaddress(loc,nsp_type_gnetworkaddress) == FAIL) return NULLGNETWORKADDRESS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGNetworkAddress 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gnetworkaddress_type_name[]="GNetworkAddress";
static char gnetworkaddress_short_type_name[]="GNetworkAddress";

static char *nsp_gnetworkaddress_type_as_string(void)
{
  return(gnetworkaddress_type_name);
}

static char *nsp_gnetworkaddress_type_short_string(NspObject *v)
{
  return(gnetworkaddress_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGNetworkAddress objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGNetworkAddress   *nsp_gnetworkaddress_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gnetworkaddress_id)  == TRUE  ) return ((NspGNetworkAddress *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gnetworkaddress));
  return NULL;
}

int IsGNetworkAddressObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gnetworkaddress_id);
}

int IsGNetworkAddress(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gnetworkaddress_id);
}

NspGNetworkAddress  *GetGNetworkAddressCopy(Stack stack, int i)
{
  if (  GetGNetworkAddress(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGNetworkAddress  *GetGNetworkAddress(Stack stack, int i)
{
  NspGNetworkAddress *M;
  if (( M = nsp_gnetworkaddress_object(NthObj(i))) == NULLGNETWORKADDRESS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGNetworkAddress *gnetworkaddress_copy(NspGNetworkAddress *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gnetworkaddress);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gnetworkaddress);
}

/*-------------------------------------------------------------------
 * wrappers for the GNetworkAddress
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_network_address_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *hostname;
  int port;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&hostname, &port) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_network_address_new(hostname,port))== NULL) return RET_BUG;

  nsp_type_gnetworkaddress = new_type_gnetworkaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gnetworkaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_network_address_get_hostname(NspGNetworkAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_network_address_get_hostname(G_NETWORK_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_network_address_get_port(NspGNetworkAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_network_address_get_port(G_NETWORK_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_network_address_get_scheme(NspGNetworkAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_network_address_get_scheme(G_NETWORK_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static NspMethods gnetworkaddress_methods[] = {
  {"get_hostname",(nsp_method *) _wrap_g_network_address_get_hostname},
  {"get_port",(nsp_method *) _wrap_g_network_address_get_port},
  {"get_scheme",(nsp_method *) _wrap_g_network_address_get_scheme},
  { NULL, NULL}
};

static NspMethods *gnetworkaddress_get_methods(void) { return gnetworkaddress_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gnetworkaddress_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGNetworkService ----------- */


#define  NspGNetworkService_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gnetworkservice.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGNetworkService inherits from GObject 
 */

int nsp_type_gnetworkservice_id=0;
NspTypeGNetworkService *nsp_type_gnetworkservice=NULL;

/*
 * Type object for NspGNetworkService 
 * all the instance of NspTypeGNetworkService share the same id. 
 * nsp_type_gnetworkservice: is an instance of NspTypeGNetworkService 
 *    used for objects of NspGNetworkService type (i.e built with new_gnetworkservice) 
 * other instances are used for derived classes 
 */
NspTypeGNetworkService *new_type_gnetworkservice(type_mode mode)
{
  NspTypeGNetworkService *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gnetworkservice != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gnetworkservice;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gnetworkservice_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gnetworkservice_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gnetworkservice;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gnetworkservice */ 

  top->s_type =  (s_type_func *) nsp_gnetworkservice_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gnetworkservice_type_short_string;
  /* top->create = (create_func*) int_gnetworkservice_create;*/

  /* specific methods for gnetworkservice */

  type->init = (init_func *) init_gnetworkservice;

  /* 
   * NspGNetworkService interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gnetworkservice_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGNetworkService called nsp_type_gnetworkservice
       */
      type->id =  nsp_type_gnetworkservice_id = nsp_new_type_id();
      nsp_type_gnetworkservice = type;
      if ( nsp_register_type(nsp_type_gnetworkservice) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gnetworkservice, G_TYPE_NETWORK_SERVICE);
      return ( mode == T_BASE ) ? type : new_type_gnetworkservice(mode);
    }
  else 
    {
      type->id = nsp_type_gnetworkservice_id;
      return type;
    }
}

/*
 * initialize NspGNetworkService instances 
 * locally and by calling initializer on parent class 
 */

static int init_gnetworkservice(NspGNetworkService *Obj,NspTypeGNetworkService *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGNetworkService 
 */

NspGNetworkService *new_gnetworkservice() 
{
  NspGNetworkService *loc;
  /* type must exists */
  nsp_type_gnetworkservice = new_type_gnetworkservice(T_BASE);
  if ( (loc = malloc(sizeof(NspGNetworkService)))== NULLGNETWORKSERVICE) return loc;
  /* initialize object */
  if ( init_gnetworkservice(loc,nsp_type_gnetworkservice) == FAIL) return NULLGNETWORKSERVICE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGNetworkService 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gnetworkservice_type_name[]="GNetworkService";
static char gnetworkservice_short_type_name[]="GNetworkService";

static char *nsp_gnetworkservice_type_as_string(void)
{
  return(gnetworkservice_type_name);
}

static char *nsp_gnetworkservice_type_short_string(NspObject *v)
{
  return(gnetworkservice_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGNetworkService objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGNetworkService   *nsp_gnetworkservice_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gnetworkservice_id)  == TRUE  ) return ((NspGNetworkService *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gnetworkservice));
  return NULL;
}

int IsGNetworkServiceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gnetworkservice_id);
}

int IsGNetworkService(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gnetworkservice_id);
}

NspGNetworkService  *GetGNetworkServiceCopy(Stack stack, int i)
{
  if (  GetGNetworkService(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGNetworkService  *GetGNetworkService(Stack stack, int i)
{
  NspGNetworkService *M;
  if (( M = nsp_gnetworkservice_object(NthObj(i))) == NULLGNETWORKSERVICE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGNetworkService *gnetworkservice_copy(NspGNetworkService *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gnetworkservice);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gnetworkservice);
}

/*-------------------------------------------------------------------
 * wrappers for the GNetworkService
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_network_service_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string,string, t_end};
  char *service, *protocol, *domain;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&service, &protocol, &domain) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_network_service_new(service,protocol,domain))== NULL) return RET_BUG;

  nsp_type_gnetworkservice = new_type_gnetworkservice(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gnetworkservice );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_network_service_get_service(NspGNetworkService *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_network_service_get_service(G_NETWORK_SERVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_network_service_get_protocol(NspGNetworkService *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_network_service_get_protocol(G_NETWORK_SERVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_network_service_get_domain(NspGNetworkService *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_network_service_get_domain(G_NETWORK_SERVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_network_service_get_scheme(NspGNetworkService *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_network_service_get_scheme(G_NETWORK_SERVICE(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_network_service_set_scheme(NspGNetworkService *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *scheme;
  if ( GetArgs(stack,rhs,opt,T,&scheme) == FAIL) return RET_BUG;
    g_network_service_set_scheme(G_NETWORK_SERVICE(self->obj),scheme);
  return 0;
}

static NspMethods gnetworkservice_methods[] = {
  {"get_service",(nsp_method *) _wrap_g_network_service_get_service},
  {"get_protocol",(nsp_method *) _wrap_g_network_service_get_protocol},
  {"get_domain",(nsp_method *) _wrap_g_network_service_get_domain},
  {"get_scheme",(nsp_method *) _wrap_g_network_service_get_scheme},
  {"set_scheme",(nsp_method *) _wrap_g_network_service_set_scheme},
  { NULL, NULL}
};

static NspMethods *gnetworkservice_get_methods(void) { return gnetworkservice_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gnetworkservice_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGOutputStream ----------- */


#define  NspGOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/goutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGOutputStream inherits from GObject 
 */

int nsp_type_goutputstream_id=0;
NspTypeGOutputStream *nsp_type_goutputstream=NULL;

/*
 * Type object for NspGOutputStream 
 * all the instance of NspTypeGOutputStream share the same id. 
 * nsp_type_goutputstream: is an instance of NspTypeGOutputStream 
 *    used for objects of NspGOutputStream type (i.e built with new_goutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGOutputStream *new_type_goutputstream(type_mode mode)
{
  NspTypeGOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_goutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_goutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = goutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = goutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_goutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for goutputstream */ 

  top->s_type =  (s_type_func *) nsp_goutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_goutputstream_type_short_string;
  /* top->create = (create_func*) int_goutputstream_create;*/

  /* specific methods for goutputstream */

  type->init = (init_func *) init_goutputstream;

  /* 
   * NspGOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_goutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGOutputStream called nsp_type_goutputstream
       */
      type->id =  nsp_type_goutputstream_id = nsp_new_type_id();
      nsp_type_goutputstream = type;
      if ( nsp_register_type(nsp_type_goutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_goutputstream, G_TYPE_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_goutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_goutputstream_id;
      return type;
    }
}

/*
 * initialize NspGOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_goutputstream(NspGOutputStream *Obj,NspTypeGOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGOutputStream 
 */

NspGOutputStream *new_goutputstream() 
{
  NspGOutputStream *loc;
  /* type must exists */
  nsp_type_goutputstream = new_type_goutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGOutputStream)))== NULLGOUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_goutputstream(loc,nsp_type_goutputstream) == FAIL) return NULLGOUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char goutputstream_type_name[]="GOutputStream";
static char goutputstream_short_type_name[]="GOutputStream";

static char *nsp_goutputstream_type_as_string(void)
{
  return(goutputstream_type_name);
}

static char *nsp_goutputstream_type_short_string(NspObject *v)
{
  return(goutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGOutputStream   *nsp_goutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_goutputstream_id)  == TRUE  ) return ((NspGOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_goutputstream));
  return NULL;
}

int IsGOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_goutputstream_id);
}

int IsGOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_goutputstream_id);
}

NspGOutputStream  *GetGOutputStreamCopy(Stack stack, int i)
{
  if (  GetGOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGOutputStream  *GetGOutputStream(Stack stack, int i)
{
  NspGOutputStream *M;
  if (( M = nsp_goutputstream_object(NthObj(i))) == NULLGOUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGOutputStream *goutputstream_copy(NspGOutputStream *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_goutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_goutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_output_stream_splice(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,obj_check, t_end};
  NspGObject *source, *cancellable;
  GOutputStreamSpliceFlags flags;
  NspObject *nsp_flags = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginputstream, &source, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_OUTPUT_STREAM_SPLICE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_output_stream_splice(G_OUTPUT_STREAM(self->obj),G_INPUT_STREAM(source->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_flush(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_output_stream_flush(G_OUTPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_close(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_output_stream_close(G_OUTPUT_STREAM(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_write_finish(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_output_stream_write_finish(G_OUTPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_output_stream_write_bytes_finish(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_output_stream_write_bytes_finish(G_OUTPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_output_stream_write_bytes_finish(Stack stack, int rhs, int opt, int lhs) /* write_bytes_finish */
{
  Scierror("Error: function g_output_stream_write_bytes_finish not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_output_stream_splice_finish(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_output_stream_splice_finish(G_OUTPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_flush_finish(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_output_stream_flush_finish(G_OUTPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_close_finish(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_output_stream_close_finish(G_OUTPUT_STREAM(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_is_closed(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_output_stream_is_closed(G_OUTPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_is_closing(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_output_stream_is_closing(G_OUTPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_has_pending(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_output_stream_has_pending(G_OUTPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_set_pending(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_output_stream_set_pending(G_OUTPUT_STREAM(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_output_stream_clear_pending(NspGOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_output_stream_clear_pending(G_OUTPUT_STREAM(self->obj));
  return 0;
}

static NspMethods goutputstream_methods[] = {
  {"splice",(nsp_method *) _wrap_g_output_stream_splice},
  {"flush",(nsp_method *) _wrap_g_output_stream_flush},
  {"close",(nsp_method *) _wrap_g_output_stream_close},
  {"write_finish",(nsp_method *) _wrap_g_output_stream_write_finish},
  {"write_bytes_finish",(nsp_method *) _wrap_g_output_stream_write_bytes_finish},
  {"splice_finish",(nsp_method *) _wrap_g_output_stream_splice_finish},
  {"flush_finish",(nsp_method *) _wrap_g_output_stream_flush_finish},
  {"close_finish",(nsp_method *) _wrap_g_output_stream_close_finish},
  {"is_closed",(nsp_method *) _wrap_g_output_stream_is_closed},
  {"is_closing",(nsp_method *) _wrap_g_output_stream_is_closing},
  {"has_pending",(nsp_method *) _wrap_g_output_stream_has_pending},
  {"set_pending",(nsp_method *) _wrap_g_output_stream_set_pending},
  {"clear_pending",(nsp_method *) _wrap_g_output_stream_clear_pending},
  { NULL, NULL}
};

static NspMethods *goutputstream_get_methods(void) { return goutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab goutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGMemoryOutputStream ----------- */


#define  NspGMemoryOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gmemoryoutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGMemoryOutputStream inherits from GOutputStream 
 */

int nsp_type_gmemoryoutputstream_id=0;
NspTypeGMemoryOutputStream *nsp_type_gmemoryoutputstream=NULL;

/*
 * Type object for NspGMemoryOutputStream 
 * all the instance of NspTypeGMemoryOutputStream share the same id. 
 * nsp_type_gmemoryoutputstream: is an instance of NspTypeGMemoryOutputStream 
 *    used for objects of NspGMemoryOutputStream type (i.e built with new_gmemoryoutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGMemoryOutputStream *new_type_gmemoryoutputstream(type_mode mode)
{
  NspTypeGMemoryOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gmemoryoutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gmemoryoutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGOutputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_goutputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gmemoryoutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gmemoryoutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gmemoryoutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gmemoryoutputstream */ 

  top->s_type =  (s_type_func *) nsp_gmemoryoutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gmemoryoutputstream_type_short_string;
  /* top->create = (create_func*) int_gmemoryoutputstream_create;*/

  /* specific methods for gmemoryoutputstream */

  type->init = (init_func *) init_gmemoryoutputstream;

  /* 
   * NspGMemoryOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gmemoryoutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGMemoryOutputStream called nsp_type_gmemoryoutputstream
       */
      type->id =  nsp_type_gmemoryoutputstream_id = nsp_new_type_id();
      nsp_type_gmemoryoutputstream = type;
      if ( nsp_register_type(nsp_type_gmemoryoutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gmemoryoutputstream, G_TYPE_MEMORY_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gmemoryoutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gmemoryoutputstream_id;
      return type;
    }
}

/*
 * initialize NspGMemoryOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gmemoryoutputstream(NspGMemoryOutputStream *Obj,NspTypeGMemoryOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGMemoryOutputStream 
 */

NspGMemoryOutputStream *new_gmemoryoutputstream() 
{
  NspGMemoryOutputStream *loc;
  /* type must exists */
  nsp_type_gmemoryoutputstream = new_type_gmemoryoutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGMemoryOutputStream)))== NULLGMEMORYOUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gmemoryoutputstream(loc,nsp_type_gmemoryoutputstream) == FAIL) return NULLGMEMORYOUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGMemoryOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gmemoryoutputstream_type_name[]="GMemoryOutputStream";
static char gmemoryoutputstream_short_type_name[]="GMemoryOutputStream";

static char *nsp_gmemoryoutputstream_type_as_string(void)
{
  return(gmemoryoutputstream_type_name);
}

static char *nsp_gmemoryoutputstream_type_short_string(NspObject *v)
{
  return(gmemoryoutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGMemoryOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGMemoryOutputStream   *nsp_gmemoryoutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gmemoryoutputstream_id)  == TRUE  ) return ((NspGMemoryOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gmemoryoutputstream));
  return NULL;
}

int IsGMemoryOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gmemoryoutputstream_id);
}

int IsGMemoryOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gmemoryoutputstream_id);
}

NspGMemoryOutputStream  *GetGMemoryOutputStreamCopy(Stack stack, int i)
{
  if (  GetGMemoryOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGMemoryOutputStream  *GetGMemoryOutputStream(Stack stack, int i)
{
  NspGMemoryOutputStream *M;
  if (( M = nsp_gmemoryoutputstream_object(NthObj(i))) == NULLGMEMORYOUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGMemoryOutputStream *gmemoryoutputstream_copy(NspGMemoryOutputStream *self)
{
  /* return goutputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmemoryoutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gmemoryoutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GMemoryOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_memory_output_stream_new_resizable (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_memory_output_stream_new_resizable())== NULL) return RET_BUG;

  nsp_type_gmemoryoutputstream = new_type_gmemoryoutputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gmemoryoutputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_memory_output_stream_get_size(NspGMemoryOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_memory_output_stream_get_size(G_MEMORY_OUTPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_memory_output_stream_get_data_size(NspGMemoryOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_memory_output_stream_get_data_size(G_MEMORY_OUTPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gmemoryoutputstream_methods[] = {
  {"get_size",(nsp_method *) _wrap_g_memory_output_stream_get_size},
  {"get_data_size",(nsp_method *) _wrap_g_memory_output_stream_get_data_size},
  { NULL, NULL}
};

static NspMethods *gmemoryoutputstream_get_methods(void) { return gmemoryoutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gmemoryoutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFilterOutputStream ----------- */


#define  NspGFilterOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfilteroutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFilterOutputStream inherits from GOutputStream 
 */

int nsp_type_gfilteroutputstream_id=0;
NspTypeGFilterOutputStream *nsp_type_gfilteroutputstream=NULL;

/*
 * Type object for NspGFilterOutputStream 
 * all the instance of NspTypeGFilterOutputStream share the same id. 
 * nsp_type_gfilteroutputstream: is an instance of NspTypeGFilterOutputStream 
 *    used for objects of NspGFilterOutputStream type (i.e built with new_gfilteroutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGFilterOutputStream *new_type_gfilteroutputstream(type_mode mode)
{
  NspTypeGFilterOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfilteroutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfilteroutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGOutputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_goutputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfilteroutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfilteroutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfilteroutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfilteroutputstream */ 

  top->s_type =  (s_type_func *) nsp_gfilteroutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfilteroutputstream_type_short_string;
  /* top->create = (create_func*) int_gfilteroutputstream_create;*/

  /* specific methods for gfilteroutputstream */

  type->init = (init_func *) init_gfilteroutputstream;

  /* 
   * NspGFilterOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfilteroutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFilterOutputStream called nsp_type_gfilteroutputstream
       */
      type->id =  nsp_type_gfilteroutputstream_id = nsp_new_type_id();
      nsp_type_gfilteroutputstream = type;
      if ( nsp_register_type(nsp_type_gfilteroutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfilteroutputstream, G_TYPE_FILTER_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gfilteroutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gfilteroutputstream_id;
      return type;
    }
}

/*
 * initialize NspGFilterOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfilteroutputstream(NspGFilterOutputStream *Obj,NspTypeGFilterOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFilterOutputStream 
 */

NspGFilterOutputStream *new_gfilteroutputstream() 
{
  NspGFilterOutputStream *loc;
  /* type must exists */
  nsp_type_gfilteroutputstream = new_type_gfilteroutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGFilterOutputStream)))== NULLGFILTEROUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gfilteroutputstream(loc,nsp_type_gfilteroutputstream) == FAIL) return NULLGFILTEROUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFilterOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfilteroutputstream_type_name[]="GFilterOutputStream";
static char gfilteroutputstream_short_type_name[]="GFilterOutputStream";

static char *nsp_gfilteroutputstream_type_as_string(void)
{
  return(gfilteroutputstream_type_name);
}

static char *nsp_gfilteroutputstream_type_short_string(NspObject *v)
{
  return(gfilteroutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFilterOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFilterOutputStream   *nsp_gfilteroutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfilteroutputstream_id)  == TRUE  ) return ((NspGFilterOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfilteroutputstream));
  return NULL;
}

int IsGFilterOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfilteroutputstream_id);
}

int IsGFilterOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfilteroutputstream_id);
}

NspGFilterOutputStream  *GetGFilterOutputStreamCopy(Stack stack, int i)
{
  if (  GetGFilterOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFilterOutputStream  *GetGFilterOutputStream(Stack stack, int i)
{
  NspGFilterOutputStream *M;
  if (( M = nsp_gfilteroutputstream_object(NthObj(i))) == NULLGFILTEROUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFilterOutputStream *gfilteroutputstream_copy(NspGFilterOutputStream *self)
{
  /* return goutputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfilteroutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfilteroutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GFilterOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_filter_output_stream_get_base_stream(NspGFilterOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GOutputStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_filter_output_stream_get_base_stream(G_FILTER_OUTPUT_STREAM(self->obj));
  nsp_type_goutputstream = new_type_goutputstream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_goutputstream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_filter_output_stream_get_close_base_stream(NspGFilterOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_filter_output_stream_get_close_base_stream(G_FILTER_OUTPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_filter_output_stream_set_close_base_stream(NspGFilterOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int close_base;
  if ( GetArgs(stack,rhs,opt,T,&close_base) == FAIL) return RET_BUG;
    g_filter_output_stream_set_close_base_stream(G_FILTER_OUTPUT_STREAM(self->obj),close_base);
  return 0;
}

static NspMethods gfilteroutputstream_methods[] = {
  {"get_base_stream",(nsp_method *) _wrap_g_filter_output_stream_get_base_stream},
  {"get_close_base_stream",(nsp_method *) _wrap_g_filter_output_stream_get_close_base_stream},
  {"set_close_base_stream",(nsp_method *) _wrap_g_filter_output_stream_set_close_base_stream},
  { NULL, NULL}
};

static NspMethods *gfilteroutputstream_get_methods(void) { return gfilteroutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfilteroutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGBufferedOutputStream ----------- */


#define  NspGBufferedOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gbufferedoutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGBufferedOutputStream inherits from GFilterOutputStream 
 */

int nsp_type_gbufferedoutputstream_id=0;
NspTypeGBufferedOutputStream *nsp_type_gbufferedoutputstream=NULL;

/*
 * Type object for NspGBufferedOutputStream 
 * all the instance of NspTypeGBufferedOutputStream share the same id. 
 * nsp_type_gbufferedoutputstream: is an instance of NspTypeGBufferedOutputStream 
 *    used for objects of NspGBufferedOutputStream type (i.e built with new_gbufferedoutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGBufferedOutputStream *new_type_gbufferedoutputstream(type_mode mode)
{
  NspTypeGBufferedOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gbufferedoutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gbufferedoutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGFilterOutputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gfilteroutputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gbufferedoutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gbufferedoutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gbufferedoutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gbufferedoutputstream */ 

  top->s_type =  (s_type_func *) nsp_gbufferedoutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gbufferedoutputstream_type_short_string;
  /* top->create = (create_func*) int_gbufferedoutputstream_create;*/

  /* specific methods for gbufferedoutputstream */

  type->init = (init_func *) init_gbufferedoutputstream;

  /* 
   * NspGBufferedOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gbufferedoutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGBufferedOutputStream called nsp_type_gbufferedoutputstream
       */
      type->id =  nsp_type_gbufferedoutputstream_id = nsp_new_type_id();
      nsp_type_gbufferedoutputstream = type;
      if ( nsp_register_type(nsp_type_gbufferedoutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gbufferedoutputstream, G_TYPE_BUFFERED_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gbufferedoutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gbufferedoutputstream_id;
      return type;
    }
}

/*
 * initialize NspGBufferedOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gbufferedoutputstream(NspGBufferedOutputStream *Obj,NspTypeGBufferedOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGBufferedOutputStream 
 */

NspGBufferedOutputStream *new_gbufferedoutputstream() 
{
  NspGBufferedOutputStream *loc;
  /* type must exists */
  nsp_type_gbufferedoutputstream = new_type_gbufferedoutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGBufferedOutputStream)))== NULLGBUFFEREDOUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gbufferedoutputstream(loc,nsp_type_gbufferedoutputstream) == FAIL) return NULLGBUFFEREDOUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGBufferedOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gbufferedoutputstream_type_name[]="GBufferedOutputStream";
static char gbufferedoutputstream_short_type_name[]="GBufferedOutputStream";

static char *nsp_gbufferedoutputstream_type_as_string(void)
{
  return(gbufferedoutputstream_type_name);
}

static char *nsp_gbufferedoutputstream_type_short_string(NspObject *v)
{
  return(gbufferedoutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGBufferedOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGBufferedOutputStream   *nsp_gbufferedoutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gbufferedoutputstream_id)  == TRUE  ) return ((NspGBufferedOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gbufferedoutputstream));
  return NULL;
}

int IsGBufferedOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gbufferedoutputstream_id);
}

int IsGBufferedOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gbufferedoutputstream_id);
}

NspGBufferedOutputStream  *GetGBufferedOutputStreamCopy(Stack stack, int i)
{
  if (  GetGBufferedOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGBufferedOutputStream  *GetGBufferedOutputStream(Stack stack, int i)
{
  NspGBufferedOutputStream *M;
  if (( M = nsp_gbufferedoutputstream_object(NthObj(i))) == NULLGBUFFEREDOUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGBufferedOutputStream *gbufferedoutputstream_copy(NspGBufferedOutputStream *self)
{
  /* return gfilteroutputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gbufferedoutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gbufferedoutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GBufferedOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_buffered_output_stream_new_sized (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *base_stream;
  int size;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_goutputstream, &base_stream, &size) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_buffered_output_stream_new_sized(G_OUTPUT_STREAM(base_stream->obj),size))== NULL) return RET_BUG;

  nsp_type_gbufferedoutputstream = new_type_gbufferedoutputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gbufferedoutputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_buffered_output_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *base_stream;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_goutputstream, &base_stream) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_buffered_output_stream_new(G_OUTPUT_STREAM(base_stream->obj)))== NULL) return RET_BUG;

  nsp_type_gbufferedoutputstream = new_type_gbufferedoutputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gbufferedoutputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_buffered_output_stream_get_buffer_size(NspGBufferedOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_buffered_output_stream_get_buffer_size(G_BUFFERED_OUTPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_buffered_output_stream_set_buffer_size(NspGBufferedOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int size;
  if ( GetArgs(stack,rhs,opt,T,&size) == FAIL) return RET_BUG;
    g_buffered_output_stream_set_buffer_size(G_BUFFERED_OUTPUT_STREAM(self->obj),size);
  return 0;
}

static int _wrap_g_buffered_output_stream_get_auto_grow(NspGBufferedOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_buffered_output_stream_get_auto_grow(G_BUFFERED_OUTPUT_STREAM(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_buffered_output_stream_set_auto_grow(NspGBufferedOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int auto_grow;
  if ( GetArgs(stack,rhs,opt,T,&auto_grow) == FAIL) return RET_BUG;
    g_buffered_output_stream_set_auto_grow(G_BUFFERED_OUTPUT_STREAM(self->obj),auto_grow);
  return 0;
}

static NspMethods gbufferedoutputstream_methods[] = {
  {"get_buffer_size",(nsp_method *) _wrap_g_buffered_output_stream_get_buffer_size},
  {"set_buffer_size",(nsp_method *) _wrap_g_buffered_output_stream_set_buffer_size},
  {"get_auto_grow",(nsp_method *) _wrap_g_buffered_output_stream_get_auto_grow},
  {"set_auto_grow",(nsp_method *) _wrap_g_buffered_output_stream_set_auto_grow},
  { NULL, NULL}
};

static NspMethods *gbufferedoutputstream_get_methods(void) { return gbufferedoutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gbufferedoutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGConverterOutputStream ----------- */


#define  NspGConverterOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gconverteroutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGConverterOutputStream inherits from GFilterOutputStream 
 */

int nsp_type_gconverteroutputstream_id=0;
NspTypeGConverterOutputStream *nsp_type_gconverteroutputstream=NULL;

/*
 * Type object for NspGConverterOutputStream 
 * all the instance of NspTypeGConverterOutputStream share the same id. 
 * nsp_type_gconverteroutputstream: is an instance of NspTypeGConverterOutputStream 
 *    used for objects of NspGConverterOutputStream type (i.e built with new_gconverteroutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGConverterOutputStream *new_type_gconverteroutputstream(type_mode mode)
{
  NspTypeGConverterOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gconverteroutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gconverteroutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGFilterOutputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gfilteroutputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gconverteroutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gconverteroutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gconverteroutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gconverteroutputstream */ 

  top->s_type =  (s_type_func *) nsp_gconverteroutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gconverteroutputstream_type_short_string;
  /* top->create = (create_func*) int_gconverteroutputstream_create;*/

  /* specific methods for gconverteroutputstream */

  type->init = (init_func *) init_gconverteroutputstream;

  /* 
   * NspGConverterOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gconverteroutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGConverterOutputStream called nsp_type_gconverteroutputstream
       */
      type->id =  nsp_type_gconverteroutputstream_id = nsp_new_type_id();
      nsp_type_gconverteroutputstream = type;
      if ( nsp_register_type(nsp_type_gconverteroutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gconverteroutputstream, G_TYPE_CONVERTER_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gconverteroutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gconverteroutputstream_id;
      return type;
    }
}

/*
 * initialize NspGConverterOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gconverteroutputstream(NspGConverterOutputStream *Obj,NspTypeGConverterOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGConverterOutputStream 
 */

NspGConverterOutputStream *new_gconverteroutputstream() 
{
  NspGConverterOutputStream *loc;
  /* type must exists */
  nsp_type_gconverteroutputstream = new_type_gconverteroutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGConverterOutputStream)))== NULLGCONVERTEROUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gconverteroutputstream(loc,nsp_type_gconverteroutputstream) == FAIL) return NULLGCONVERTEROUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGConverterOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gconverteroutputstream_type_name[]="GConverterOutputStream";
static char gconverteroutputstream_short_type_name[]="GConverterOutputStream";

static char *nsp_gconverteroutputstream_type_as_string(void)
{
  return(gconverteroutputstream_type_name);
}

static char *nsp_gconverteroutputstream_type_short_string(NspObject *v)
{
  return(gconverteroutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGConverterOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGConverterOutputStream   *nsp_gconverteroutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gconverteroutputstream_id)  == TRUE  ) return ((NspGConverterOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gconverteroutputstream));
  return NULL;
}

int IsGConverterOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gconverteroutputstream_id);
}

int IsGConverterOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gconverteroutputstream_id);
}

NspGConverterOutputStream  *GetGConverterOutputStreamCopy(Stack stack, int i)
{
  if (  GetGConverterOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGConverterOutputStream  *GetGConverterOutputStream(Stack stack, int i)
{
  NspGConverterOutputStream *M;
  if (( M = nsp_gconverteroutputstream_object(NthObj(i))) == NULLGCONVERTEROUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGConverterOutputStream *gconverteroutputstream_copy(NspGConverterOutputStream *self)
{
  /* return gfilteroutputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gconverteroutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gconverteroutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GConverterOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_converter_output_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *base_stream, *converter;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_goutputstream, &base_stream, &nsp_type_gconverter, &converter) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_converter_output_stream_new(G_OUTPUT_STREAM(base_stream->obj),G_CONVERTER(converter->obj)))== NULL) return RET_BUG;

  nsp_type_gconverteroutputstream = new_type_gconverteroutputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gconverteroutputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_converter_output_stream_get_converter(NspGConverterOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  GConverter *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_converter_output_stream_get_converter(G_CONVERTER_OUTPUT_STREAM(self->obj));
  nsp_type_gconverter = new_type_gconverter(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gconverter))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gconverteroutputstream_methods[] = {
  {"get_converter",(nsp_method *) _wrap_g_converter_output_stream_get_converter},
  { NULL, NULL}
};

static NspMethods *gconverteroutputstream_get_methods(void) { return gconverteroutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gconverteroutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGDataOutputStream ----------- */


#define  NspGDataOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gdataoutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGDataOutputStream inherits from GFilterOutputStream 
 */

int nsp_type_gdataoutputstream_id=0;
NspTypeGDataOutputStream *nsp_type_gdataoutputstream=NULL;

/*
 * Type object for NspGDataOutputStream 
 * all the instance of NspTypeGDataOutputStream share the same id. 
 * nsp_type_gdataoutputstream: is an instance of NspTypeGDataOutputStream 
 *    used for objects of NspGDataOutputStream type (i.e built with new_gdataoutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGDataOutputStream *new_type_gdataoutputstream(type_mode mode)
{
  NspTypeGDataOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gdataoutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gdataoutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGFilterOutputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gfilteroutputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gdataoutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gdataoutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gdataoutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gdataoutputstream */ 

  top->s_type =  (s_type_func *) nsp_gdataoutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gdataoutputstream_type_short_string;
  /* top->create = (create_func*) int_gdataoutputstream_create;*/

  /* specific methods for gdataoutputstream */

  type->init = (init_func *) init_gdataoutputstream;

  /* 
   * NspGDataOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gdataoutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGDataOutputStream called nsp_type_gdataoutputstream
       */
      type->id =  nsp_type_gdataoutputstream_id = nsp_new_type_id();
      nsp_type_gdataoutputstream = type;
      if ( nsp_register_type(nsp_type_gdataoutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gdataoutputstream, G_TYPE_DATA_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gdataoutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gdataoutputstream_id;
      return type;
    }
}

/*
 * initialize NspGDataOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gdataoutputstream(NspGDataOutputStream *Obj,NspTypeGDataOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGDataOutputStream 
 */

NspGDataOutputStream *new_gdataoutputstream() 
{
  NspGDataOutputStream *loc;
  /* type must exists */
  nsp_type_gdataoutputstream = new_type_gdataoutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGDataOutputStream)))== NULLGDATAOUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gdataoutputstream(loc,nsp_type_gdataoutputstream) == FAIL) return NULLGDATAOUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGDataOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gdataoutputstream_type_name[]="GDataOutputStream";
static char gdataoutputstream_short_type_name[]="GDataOutputStream";

static char *nsp_gdataoutputstream_type_as_string(void)
{
  return(gdataoutputstream_type_name);
}

static char *nsp_gdataoutputstream_type_short_string(NspObject *v)
{
  return(gdataoutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGDataOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGDataOutputStream   *nsp_gdataoutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gdataoutputstream_id)  == TRUE  ) return ((NspGDataOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gdataoutputstream));
  return NULL;
}

int IsGDataOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gdataoutputstream_id);
}

int IsGDataOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gdataoutputstream_id);
}

NspGDataOutputStream  *GetGDataOutputStreamCopy(Stack stack, int i)
{
  if (  GetGDataOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGDataOutputStream  *GetGDataOutputStream(Stack stack, int i)
{
  NspGDataOutputStream *M;
  if (( M = nsp_gdataoutputstream_object(NthObj(i))) == NULLGDATAOUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGDataOutputStream *gdataoutputstream_copy(NspGDataOutputStream *self)
{
  /* return gfilteroutputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdataoutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gdataoutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GDataOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_data_output_stream_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *base_stream;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_goutputstream, &base_stream) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_data_output_stream_new(G_OUTPUT_STREAM(base_stream->obj)))== NULL) return RET_BUG;

  nsp_type_gdataoutputstream = new_type_gdataoutputstream(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gdataoutputstream );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_data_output_stream_set_byte_order(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GDataStreamByteOrder order;
  NspObject *nsp_order = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_order) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_DATA_STREAM_BYTE_ORDER, nsp_order, &order)== FAIL)
      return RET_BUG;
    g_data_output_stream_set_byte_order(G_DATA_OUTPUT_STREAM(self->obj),order);
  return 0;
}

static int _wrap_g_data_output_stream_get_byte_order(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_data_output_stream_get_byte_order(G_DATA_OUTPUT_STREAM(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_output_stream_put_byte(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int data, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&data, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_output_stream_put_byte(G_DATA_OUTPUT_STREAM(self->obj),data,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_output_stream_put_int16(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int data, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&data, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_output_stream_put_int16(G_DATA_OUTPUT_STREAM(self->obj),data,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_output_stream_put_uint16(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int data, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&data, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_output_stream_put_uint16(G_DATA_OUTPUT_STREAM(self->obj),data,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_output_stream_put_int32(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int data, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&data, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_output_stream_put_int32(G_DATA_OUTPUT_STREAM(self->obj),data,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_output_stream_put_uint32(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  gulong data;
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&data, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_output_stream_put_uint32(G_DATA_OUTPUT_STREAM(self->obj),data,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_data_output_stream_put_string(NspGDataOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *str;
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&str, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_data_output_stream_put_string(G_DATA_OUTPUT_STREAM(self->obj),str,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gdataoutputstream_methods[] = {
  {"set_byte_order",(nsp_method *) _wrap_g_data_output_stream_set_byte_order},
  {"get_byte_order",(nsp_method *) _wrap_g_data_output_stream_get_byte_order},
  {"put_byte",(nsp_method *) _wrap_g_data_output_stream_put_byte},
  {"put_int16",(nsp_method *) _wrap_g_data_output_stream_put_int16},
  {"put_uint16",(nsp_method *) _wrap_g_data_output_stream_put_uint16},
  {"put_int32",(nsp_method *) _wrap_g_data_output_stream_put_int32},
  {"put_uint32",(nsp_method *) _wrap_g_data_output_stream_put_uint32},
  {"put_string",(nsp_method *) _wrap_g_data_output_stream_put_string},
  { NULL, NULL}
};

static NspMethods *gdataoutputstream_get_methods(void) { return gdataoutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gdataoutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGFileOutputStream ----------- */


#define  NspGFileOutputStream_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gfileoutputstream.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGFileOutputStream inherits from GOutputStream 
 */

int nsp_type_gfileoutputstream_id=0;
NspTypeGFileOutputStream *nsp_type_gfileoutputstream=NULL;

/*
 * Type object for NspGFileOutputStream 
 * all the instance of NspTypeGFileOutputStream share the same id. 
 * nsp_type_gfileoutputstream: is an instance of NspTypeGFileOutputStream 
 *    used for objects of NspGFileOutputStream type (i.e built with new_gfileoutputstream) 
 * other instances are used for derived classes 
 */
NspTypeGFileOutputStream *new_type_gfileoutputstream(type_mode mode)
{
  NspTypeGFileOutputStream *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gfileoutputstream != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gfileoutputstream;
    }
  if (( type =  malloc(sizeof(NspTypeGOutputStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_goutputstream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gfileoutputstream_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gfileoutputstream_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gfileoutputstream;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gfileoutputstream */ 

  top->s_type =  (s_type_func *) nsp_gfileoutputstream_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gfileoutputstream_type_short_string;
  /* top->create = (create_func*) int_gfileoutputstream_create;*/

  /* specific methods for gfileoutputstream */

  type->init = (init_func *) init_gfileoutputstream;

  /* 
   * NspGFileOutputStream interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gfileoutputstream_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGFileOutputStream called nsp_type_gfileoutputstream
       */
      type->id =  nsp_type_gfileoutputstream_id = nsp_new_type_id();
      nsp_type_gfileoutputstream = type;
      if ( nsp_register_type(nsp_type_gfileoutputstream) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gfileoutputstream, G_TYPE_FILE_OUTPUT_STREAM);
      return ( mode == T_BASE ) ? type : new_type_gfileoutputstream(mode);
    }
  else 
    {
      type->id = nsp_type_gfileoutputstream_id;
      return type;
    }
}

/*
 * initialize NspGFileOutputStream instances 
 * locally and by calling initializer on parent class 
 */

static int init_gfileoutputstream(NspGFileOutputStream *Obj,NspTypeGFileOutputStream *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGFileOutputStream 
 */

NspGFileOutputStream *new_gfileoutputstream() 
{
  NspGFileOutputStream *loc;
  /* type must exists */
  nsp_type_gfileoutputstream = new_type_gfileoutputstream(T_BASE);
  if ( (loc = malloc(sizeof(NspGFileOutputStream)))== NULLGFILEOUTPUTSTREAM) return loc;
  /* initialize object */
  if ( init_gfileoutputstream(loc,nsp_type_gfileoutputstream) == FAIL) return NULLGFILEOUTPUTSTREAM;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGFileOutputStream 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gfileoutputstream_type_name[]="GFileOutputStream";
static char gfileoutputstream_short_type_name[]="GFileOutputStream";

static char *nsp_gfileoutputstream_type_as_string(void)
{
  return(gfileoutputstream_type_name);
}

static char *nsp_gfileoutputstream_type_short_string(NspObject *v)
{
  return(gfileoutputstream_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGFileOutputStream objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGFileOutputStream   *nsp_gfileoutputstream_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gfileoutputstream_id)  == TRUE  ) return ((NspGFileOutputStream *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gfileoutputstream));
  return NULL;
}

int IsGFileOutputStreamObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gfileoutputstream_id);
}

int IsGFileOutputStream(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gfileoutputstream_id);
}

NspGFileOutputStream  *GetGFileOutputStreamCopy(Stack stack, int i)
{
  if (  GetGFileOutputStream(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGFileOutputStream  *GetGFileOutputStream(Stack stack, int i)
{
  NspGFileOutputStream *M;
  if (( M = nsp_gfileoutputstream_object(NthObj(i))) == NULLGFILEOUTPUTSTREAM)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGFileOutputStream *gfileoutputstream_copy(NspGFileOutputStream *self)
{
  /* return goutputstream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileoutputstream);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gfileoutputstream);
}

/*-------------------------------------------------------------------
 * wrappers for the GFileOutputStream
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_file_output_stream_get_etag(NspGFileOutputStream *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_file_output_stream_get_etag(G_FILE_OUTPUT_STREAM(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static NspMethods gfileoutputstream_methods[] = {
  {"get_etag",(nsp_method *) _wrap_g_file_output_stream_get_etag},
  { NULL, NULL}
};

static NspMethods *gfileoutputstream_get_methods(void) { return gfileoutputstream_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gfileoutputstream_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGPermission ----------- */


#define  NspGPermission_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gpermission.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGPermission inherits from GObject 
 */

int nsp_type_gpermission_id=0;
NspTypeGPermission *nsp_type_gpermission=NULL;

/*
 * Type object for NspGPermission 
 * all the instance of NspTypeGPermission share the same id. 
 * nsp_type_gpermission: is an instance of NspTypeGPermission 
 *    used for objects of NspGPermission type (i.e built with new_gpermission) 
 * other instances are used for derived classes 
 */
NspTypeGPermission *new_type_gpermission(type_mode mode)
{
  NspTypeGPermission *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gpermission != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gpermission;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gpermission_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gpermission_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gpermission;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gpermission */ 

  top->s_type =  (s_type_func *) nsp_gpermission_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gpermission_type_short_string;
  /* top->create = (create_func*) int_gpermission_create;*/

  /* specific methods for gpermission */

  type->init = (init_func *) init_gpermission;

  /* 
   * NspGPermission interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gpermission_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGPermission called nsp_type_gpermission
       */
      type->id =  nsp_type_gpermission_id = nsp_new_type_id();
      nsp_type_gpermission = type;
      if ( nsp_register_type(nsp_type_gpermission) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gpermission, G_TYPE_PERMISSION);
      return ( mode == T_BASE ) ? type : new_type_gpermission(mode);
    }
  else 
    {
      type->id = nsp_type_gpermission_id;
      return type;
    }
}

/*
 * initialize NspGPermission instances 
 * locally and by calling initializer on parent class 
 */

static int init_gpermission(NspGPermission *Obj,NspTypeGPermission *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGPermission 
 */

NspGPermission *new_gpermission() 
{
  NspGPermission *loc;
  /* type must exists */
  nsp_type_gpermission = new_type_gpermission(T_BASE);
  if ( (loc = malloc(sizeof(NspGPermission)))== NULLGPERMISSION) return loc;
  /* initialize object */
  if ( init_gpermission(loc,nsp_type_gpermission) == FAIL) return NULLGPERMISSION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGPermission 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gpermission_type_name[]="GPermission";
static char gpermission_short_type_name[]="GPermission";

static char *nsp_gpermission_type_as_string(void)
{
  return(gpermission_type_name);
}

static char *nsp_gpermission_type_short_string(NspObject *v)
{
  return(gpermission_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGPermission objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGPermission   *nsp_gpermission_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gpermission_id)  == TRUE  ) return ((NspGPermission *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gpermission));
  return NULL;
}

int IsGPermissionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gpermission_id);
}

int IsGPermission(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gpermission_id);
}

NspGPermission  *GetGPermissionCopy(Stack stack, int i)
{
  if (  GetGPermission(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGPermission  *GetGPermission(Stack stack, int i)
{
  NspGPermission *M;
  if (( M = nsp_gpermission_object(NthObj(i))) == NULLGPERMISSION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGPermission *gpermission_copy(NspGPermission *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gpermission);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gpermission);
}

/*-------------------------------------------------------------------
 * wrappers for the GPermission
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_permission_acquire(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_permission_acquire(G_PERMISSION(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_acquire_finish(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_permission_acquire_finish(G_PERMISSION(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_release(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_permission_release(G_PERMISSION(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_release_finish(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_permission_release_finish(G_PERMISSION(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_get_allowed(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_permission_get_allowed(G_PERMISSION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_get_can_acquire(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_permission_get_can_acquire(G_PERMISSION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_get_can_release(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_permission_get_can_release(G_PERMISSION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_permission_impl_update(NspGPermission *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,s_bool,s_bool, t_end};
  int allowed, can_acquire, can_release;
  if ( GetArgs(stack,rhs,opt,T,&allowed, &can_acquire, &can_release) == FAIL) return RET_BUG;
    g_permission_impl_update(G_PERMISSION(self->obj),allowed,can_acquire,can_release);
  return 0;
}

static NspMethods gpermission_methods[] = {
  {"acquire",(nsp_method *) _wrap_g_permission_acquire},
  {"acquire_finish",(nsp_method *) _wrap_g_permission_acquire_finish},
  {"release",(nsp_method *) _wrap_g_permission_release},
  {"release_finish",(nsp_method *) _wrap_g_permission_release_finish},
  {"get_allowed",(nsp_method *) _wrap_g_permission_get_allowed},
  {"get_can_acquire",(nsp_method *) _wrap_g_permission_get_can_acquire},
  {"get_can_release",(nsp_method *) _wrap_g_permission_get_can_release},
  {"impl_update",(nsp_method *) _wrap_g_permission_impl_update},
  { NULL, NULL}
};

static NspMethods *gpermission_get_methods(void) { return gpermission_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gpermission_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGResolver ----------- */


#define  NspGResolver_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gresolver.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGResolver inherits from GObject 
 */

int nsp_type_gresolver_id=0;
NspTypeGResolver *nsp_type_gresolver=NULL;

/*
 * Type object for NspGResolver 
 * all the instance of NspTypeGResolver share the same id. 
 * nsp_type_gresolver: is an instance of NspTypeGResolver 
 *    used for objects of NspGResolver type (i.e built with new_gresolver) 
 * other instances are used for derived classes 
 */
NspTypeGResolver *new_type_gresolver(type_mode mode)
{
  NspTypeGResolver *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gresolver != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gresolver;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gresolver_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gresolver_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gresolver;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gresolver */ 

  top->s_type =  (s_type_func *) nsp_gresolver_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gresolver_type_short_string;
  /* top->create = (create_func*) int_gresolver_create;*/

  /* specific methods for gresolver */

  type->init = (init_func *) init_gresolver;

  /* 
   * NspGResolver interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gresolver_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGResolver called nsp_type_gresolver
       */
      type->id =  nsp_type_gresolver_id = nsp_new_type_id();
      nsp_type_gresolver = type;
      if ( nsp_register_type(nsp_type_gresolver) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gresolver, G_TYPE_RESOLVER);
      return ( mode == T_BASE ) ? type : new_type_gresolver(mode);
    }
  else 
    {
      type->id = nsp_type_gresolver_id;
      return type;
    }
}

/*
 * initialize NspGResolver instances 
 * locally and by calling initializer on parent class 
 */

static int init_gresolver(NspGResolver *Obj,NspTypeGResolver *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGResolver 
 */

NspGResolver *new_gresolver() 
{
  NspGResolver *loc;
  /* type must exists */
  nsp_type_gresolver = new_type_gresolver(T_BASE);
  if ( (loc = malloc(sizeof(NspGResolver)))== NULLGRESOLVER) return loc;
  /* initialize object */
  if ( init_gresolver(loc,nsp_type_gresolver) == FAIL) return NULLGRESOLVER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGResolver 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gresolver_type_name[]="GResolver";
static char gresolver_short_type_name[]="GResolver";

static char *nsp_gresolver_type_as_string(void)
{
  return(gresolver_type_name);
}

static char *nsp_gresolver_type_short_string(NspObject *v)
{
  return(gresolver_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGResolver objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGResolver   *nsp_gresolver_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gresolver_id)  == TRUE  ) return ((NspGResolver *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gresolver));
  return NULL;
}

int IsGResolverObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gresolver_id);
}

int IsGResolver(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gresolver_id);
}

NspGResolver  *GetGResolverCopy(Stack stack, int i)
{
  if (  GetGResolver(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGResolver  *GetGResolver(Stack stack, int i)
{
  NspGResolver *M;
  if (( M = nsp_gresolver_object(NthObj(i))) == NULLGRESOLVER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGResolver *gresolver_copy(NspGResolver *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gresolver);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gresolver);
}

/*-------------------------------------------------------------------
 * wrappers for the GResolver
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_resolver_set_default(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_resolver_set_default(G_RESOLVER(self->obj));
  return 0;
}

static int _wrap_g_resolver_lookup_by_name(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check, t_end};
  char *hostname;
  NspGObject *cancellable;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&hostname, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_by_name(G_RESOLVER(self->obj),hostname,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_resolver_lookup_by_name_finish(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_by_name_finish(G_RESOLVER(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_resolver_lookup_by_address(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *address, *cancellable;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &address, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_by_address(G_RESOLVER(self->obj),G_INET_ADDRESS(address->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_resolver_lookup_by_address_finish(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_by_address_finish(G_RESOLVER(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_resolver_lookup_service(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,string,obj_check, t_end};
  char *service, *protocol, *domain;
  NspGObject *cancellable;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&service, &protocol, &domain, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_service(G_RESOLVER(self->obj),service,protocol,domain,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_resolver_lookup_service_finish(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_service_finish(G_RESOLVER(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_resolver_lookup_records(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj,obj_check, t_end};
  char *rrname;
  GResolverRecordType record_type;
  NspObject *nsp_record_type = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&rrname, &nsp_record_type, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_RESOLVER_RECORD_TYPE, nsp_record_type, &record_type)== FAIL)
      return RET_BUG;
    ret =g_resolver_lookup_records(G_RESOLVER(self->obj),rrname,record_type,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

#else
int _wrap_g_resolver_lookup_records(Stack stack, int rhs, int opt, int lhs) /* lookup_records */
{
  Scierror("Error: function g_resolver_lookup_records not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_resolver_lookup_records_finish(NspGResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_resolver_lookup_records_finish(G_RESOLVER(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

#else
int _wrap_g_resolver_lookup_records_finish(Stack stack, int rhs, int opt, int lhs) /* lookup_records_finish */
{
  Scierror("Error: function g_resolver_lookup_records_finish not available\n");
  return RET_BUG;
}
#endif
static NspMethods gresolver_methods[] = {
  {"set_default",(nsp_method *) _wrap_g_resolver_set_default},
  {"lookup_by_name",(nsp_method *) _wrap_g_resolver_lookup_by_name},
  {"lookup_by_name_finish",(nsp_method *) _wrap_g_resolver_lookup_by_name_finish},
  {"lookup_by_address",(nsp_method *) _wrap_g_resolver_lookup_by_address},
  {"lookup_by_address_finish",(nsp_method *) _wrap_g_resolver_lookup_by_address_finish},
  {"lookup_service",(nsp_method *) _wrap_g_resolver_lookup_service},
  {"lookup_service_finish",(nsp_method *) _wrap_g_resolver_lookup_service_finish},
  {"lookup_records",(nsp_method *) _wrap_g_resolver_lookup_records},
  {"lookup_records_finish",(nsp_method *) _wrap_g_resolver_lookup_records_finish},
  { NULL, NULL}
};

static NspMethods *gresolver_get_methods(void) { return gresolver_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gresolver_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSeekable ----------- */


#define  NspGSeekable_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gseekable.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSeekable inherits from GObject 
 */

int nsp_type_gseekable_id=0;
NspTypeGSeekable *nsp_type_gseekable=NULL;

/*
 * Type object for NspGSeekable 
 * all the instance of NspTypeGSeekable share the same id. 
 * nsp_type_gseekable: is an instance of NspTypeGSeekable 
 *    used for objects of NspGSeekable type (i.e built with new_gseekable) 
 * other instances are used for derived classes 
 */
NspTypeGSeekable *new_type_gseekable(type_mode mode)
{
  NspTypeGSeekable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gseekable != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gseekable;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gseekable_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gseekable_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gseekable;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gseekable */ 

  top->s_type =  (s_type_func *) nsp_gseekable_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gseekable_type_short_string;
  /* top->create = (create_func*) int_gseekable_create;*/

  /* specific methods for gseekable */

  type->init = (init_func *) init_gseekable;

  /* 
   * NspGSeekable interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gseekable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSeekable called nsp_type_gseekable
       */
      type->id =  nsp_type_gseekable_id = nsp_new_type_id();
      nsp_type_gseekable = type;
      if ( nsp_register_type(nsp_type_gseekable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gseekable, G_TYPE_SEEKABLE);
      return ( mode == T_BASE ) ? type : new_type_gseekable(mode);
    }
  else 
    {
      type->id = nsp_type_gseekable_id;
      return type;
    }
}

/*
 * initialize NspGSeekable instances 
 * locally and by calling initializer on parent class 
 */

static int init_gseekable(NspGSeekable *Obj,NspTypeGSeekable *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSeekable 
 */

NspGSeekable *new_gseekable() 
{
  NspGSeekable *loc;
  /* type must exists */
  nsp_type_gseekable = new_type_gseekable(T_BASE);
  if ( (loc = malloc(sizeof(NspGSeekable)))== NULLGSEEKABLE) return loc;
  /* initialize object */
  if ( init_gseekable(loc,nsp_type_gseekable) == FAIL) return NULLGSEEKABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSeekable 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gseekable_type_name[]="GSeekable";
static char gseekable_short_type_name[]="GSeekable";

static char *nsp_gseekable_type_as_string(void)
{
  return(gseekable_type_name);
}

static char *nsp_gseekable_type_short_string(NspObject *v)
{
  return(gseekable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSeekable objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSeekable   *nsp_gseekable_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gseekable_id)  == TRUE  ) return ((NspGSeekable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gseekable));
  return NULL;
}

int IsGSeekableObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gseekable_id);
}

int IsGSeekable(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gseekable_id);
}

NspGSeekable  *GetGSeekableCopy(Stack stack, int i)
{
  if (  GetGSeekable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSeekable  *GetGSeekable(Stack stack, int i)
{
  NspGSeekable *M;
  if (( M = nsp_gseekable_object(NthObj(i))) == NULLGSEEKABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSeekable *gseekable_copy(NspGSeekable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gseekable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gseekable);
}

/*-------------------------------------------------------------------
 * wrappers for the GSeekable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_seekable_can_seek(NspGSeekable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_seekable_can_seek(G_SEEKABLE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_seekable_can_truncate(NspGSeekable *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_seekable_can_truncate(G_SEEKABLE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gseekable_methods[] = {
  {"can_seek",(nsp_method *) _wrap_g_seekable_can_seek},
  {"can_truncate",(nsp_method *) _wrap_g_seekable_can_truncate},
  { NULL, NULL}
};

static NspMethods *gseekable_get_methods(void) { return gseekable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gseekable_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSettings ----------- */


#define  NspGSettings_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsettings.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSettings inherits from GObject 
 */

int nsp_type_gsettings_id=0;
NspTypeGSettings *nsp_type_gsettings=NULL;

/*
 * Type object for NspGSettings 
 * all the instance of NspTypeGSettings share the same id. 
 * nsp_type_gsettings: is an instance of NspTypeGSettings 
 *    used for objects of NspGSettings type (i.e built with new_gsettings) 
 * other instances are used for derived classes 
 */
NspTypeGSettings *new_type_gsettings(type_mode mode)
{
  NspTypeGSettings *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsettings != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsettings;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsettings_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsettings_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsettings;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsettings */ 

  top->s_type =  (s_type_func *) nsp_gsettings_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsettings_type_short_string;
  /* top->create = (create_func*) int_gsettings_create;*/

  /* specific methods for gsettings */

  type->init = (init_func *) init_gsettings;

  /* 
   * NspGSettings interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsettings_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSettings called nsp_type_gsettings
       */
      type->id =  nsp_type_gsettings_id = nsp_new_type_id();
      nsp_type_gsettings = type;
      if ( nsp_register_type(nsp_type_gsettings) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsettings, G_TYPE_SETTINGS);
      return ( mode == T_BASE ) ? type : new_type_gsettings(mode);
    }
  else 
    {
      type->id = nsp_type_gsettings_id;
      return type;
    }
}

/*
 * initialize NspGSettings instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsettings(NspGSettings *Obj,NspTypeGSettings *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSettings 
 */

NspGSettings *new_gsettings() 
{
  NspGSettings *loc;
  /* type must exists */
  nsp_type_gsettings = new_type_gsettings(T_BASE);
  if ( (loc = malloc(sizeof(NspGSettings)))== NULLGSETTINGS) return loc;
  /* initialize object */
  if ( init_gsettings(loc,nsp_type_gsettings) == FAIL) return NULLGSETTINGS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSettings 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsettings_type_name[]="GSettings";
static char gsettings_short_type_name[]="GSettings";

static char *nsp_gsettings_type_as_string(void)
{
  return(gsettings_type_name);
}

static char *nsp_gsettings_type_short_string(NspObject *v)
{
  return(gsettings_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSettings objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSettings   *nsp_gsettings_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsettings_id)  == TRUE  ) return ((NspGSettings *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsettings));
  return NULL;
}

int IsGSettingsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsettings_id);
}

int IsGSettings(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsettings_id);
}

NspGSettings  *GetGSettingsCopy(Stack stack, int i)
{
  if (  GetGSettings(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSettings  *GetGSettings(Stack stack, int i)
{
  NspGSettings *M;
  if (( M = nsp_gsettings_object(NthObj(i))) == NULLGSETTINGS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSettings *gsettings_copy(NspGSettings *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsettings);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsettings);
}

/*-------------------------------------------------------------------
 * wrappers for the GSettings
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_settings_new_with_path (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string, t_end};
  char *schema_id, *path;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&schema_id, &path) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_settings_new_with_path(schema_id,path))== NULL) return RET_BUG;

  nsp_type_gsettings = new_type_gsettings(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsettings );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_settings_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *schema_id;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&schema_id) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_settings_new(schema_id))== NULL) return RET_BUG;

  nsp_type_gsettings = new_type_gsettings(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsettings );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_settings_list_children(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_settings_list_children(G_SETTINGS(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_settings_list_keys(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_settings_list_keys(G_SETTINGS(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_settings_set_value(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj, t_end};
  char *key;
  GVariant *value = NULL;
  NspObject *nsp_value = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_value))
    { value = ((NspGVariant *) nsp_value)->obj->value;
      if((value = nsp_copy_GVariant(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type GVariant\n");
      return RET_BUG;
    }
    ret =g_settings_set_value(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get_value(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_value(G_SETTINGS(self->obj),key);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,40,0)
static int _wrap_g_settings_get_user_value(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_user_value(G_SETTINGS(self->obj),key);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_settings_get_user_value(Stack stack, int rhs, int opt, int lhs) /* get_user_value */
{
  Scierror("Error: function g_settings_get_user_value not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,40,0)
static int _wrap_g_settings_get_default_value(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  GVariant *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_default_value(G_SETTINGS(self->obj),key);
  nsp_type_gvariant= new_type_gvariant(T_BASE);
  if((ret = nsp_copy_GVariant(ret))==NULL) return RET_BUG;
  nsp_ret =(NspObject*) nsp_gvariant_create(NVOID,ret,(NspTypeBase *) nsp_type_gvariant);
  if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_settings_get_default_value(Stack stack, int rhs, int opt, int lhs) /* get_default_value */
{
  Scierror("Error: function g_settings_get_default_value not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_settings_set(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *key, *format;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &format) == FAIL) return RET_BUG;
    ret =g_settings_set(G_SETTINGS(self->obj),key,format);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *key, *format;
  if ( GetArgs(stack,rhs,opt,T,&key, &format) == FAIL) return RET_BUG;
    g_settings_get(G_SETTINGS(self->obj),key,format);
  return 0;
}

static int _wrap_g_settings_reset(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    g_settings_reset(G_SETTINGS(self->obj),key);
  return 0;
}

static int _wrap_g_settings_get_int(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_int(G_SETTINGS(self->obj),key);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_set_int(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *key;
  int value, ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_int(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_settings_get_uint(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_uint(G_SETTINGS(self->obj),key);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_settings_get_uint(Stack stack, int rhs, int opt, int lhs) /* get_uint */
{
  Scierror("Error: function g_settings_get_uint not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_settings_set_uint(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *key;
  int value, ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_uint(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_settings_set_uint(Stack stack, int rhs, int opt, int lhs) /* set_uint */
{
  Scierror("Error: function g_settings_set_uint not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_settings_get_string(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_string(G_SETTINGS(self->obj),key);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_settings_set_string(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *key, *value;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_string(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get_boolean(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_boolean(G_SETTINGS(self->obj),key);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_set_boolean(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_bool, t_end};
  char *key;
  int value, ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_boolean(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get_double(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  double ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_double(G_SETTINGS(self->obj),key);
  if ( nsp_move_double(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_set_double(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_double, t_end};
  char *key;
  double value;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_double(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get_strv(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_strv(G_SETTINGS(self->obj),key);
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_settings_get_enum(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_enum(G_SETTINGS(self->obj),key);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_set_enum(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *key;
  int value, ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_enum(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get_flags(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_get_flags(G_SETTINGS(self->obj),key);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_set_flags(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *key;
  int value, ret;
  if ( GetArgs(stack,rhs,opt,T,&key, &value) == FAIL) return RET_BUG;
    ret =g_settings_set_flags(G_SETTINGS(self->obj),key,value);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_get_child(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  GSettings *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =g_settings_get_child(G_SETTINGS(self->obj),name);
  nsp_type_gsettings = new_type_gsettings(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsettings))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_settings_is_writable(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *name;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&name) == FAIL) return RET_BUG;
    ret =g_settings_is_writable(G_SETTINGS(self->obj),name);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_settings_delay(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_settings_delay(G_SETTINGS(self->obj));
  return 0;
}

static int _wrap_g_settings_apply(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_settings_apply(G_SETTINGS(self->obj));
  return 0;
}

static int _wrap_g_settings_revert(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_settings_revert(G_SETTINGS(self->obj));
  return 0;
}

static int _wrap_g_settings_get_has_unapplied(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_settings_get_has_unapplied(G_SETTINGS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_settings_create_action(NspGSettings *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *key;
  GAction *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&key) == FAIL) return RET_BUG;
    ret =g_settings_create_action(G_SETTINGS(self->obj),key);
  nsp_type_gaction = new_type_gaction(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gaction))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_settings_create_action(Stack stack, int rhs, int opt, int lhs) /* create_action */
{
  Scierror("Error: function g_settings_create_action not available\n");
  return RET_BUG;
}
#endif
static NspMethods gsettings_methods[] = {
  {"list_children",(nsp_method *) _wrap_g_settings_list_children},
  {"list_keys",(nsp_method *) _wrap_g_settings_list_keys},
  {"set_value",(nsp_method *) _wrap_g_settings_set_value},
  {"get_value",(nsp_method *) _wrap_g_settings_get_value},
  {"get_user_value",(nsp_method *) _wrap_g_settings_get_user_value},
  {"get_default_value",(nsp_method *) _wrap_g_settings_get_default_value},
  {"set",(nsp_method *) _wrap_g_settings_set},
  {"get",(nsp_method *) _wrap_g_settings_get},
  {"reset",(nsp_method *) _wrap_g_settings_reset},
  {"get_int",(nsp_method *) _wrap_g_settings_get_int},
  {"set_int",(nsp_method *) _wrap_g_settings_set_int},
  {"get_uint",(nsp_method *) _wrap_g_settings_get_uint},
  {"set_uint",(nsp_method *) _wrap_g_settings_set_uint},
  {"get_string",(nsp_method *) _wrap_g_settings_get_string},
  {"set_string",(nsp_method *) _wrap_g_settings_set_string},
  {"get_boolean",(nsp_method *) _wrap_g_settings_get_boolean},
  {"set_boolean",(nsp_method *) _wrap_g_settings_set_boolean},
  {"get_double",(nsp_method *) _wrap_g_settings_get_double},
  {"set_double",(nsp_method *) _wrap_g_settings_set_double},
  {"get_strv",(nsp_method *) _wrap_g_settings_get_strv},
  {"get_enum",(nsp_method *) _wrap_g_settings_get_enum},
  {"set_enum",(nsp_method *) _wrap_g_settings_set_enum},
  {"get_flags",(nsp_method *) _wrap_g_settings_get_flags},
  {"set_flags",(nsp_method *) _wrap_g_settings_set_flags},
  {"get_child",(nsp_method *) _wrap_g_settings_get_child},
  {"is_writable",(nsp_method *) _wrap_g_settings_is_writable},
  {"delay",(nsp_method *) _wrap_g_settings_delay},
  {"apply",(nsp_method *) _wrap_g_settings_apply},
  {"revert",(nsp_method *) _wrap_g_settings_revert},
  {"get_has_unapplied",(nsp_method *) _wrap_g_settings_get_has_unapplied},
  {"create_action",(nsp_method *) _wrap_g_settings_create_action},
  { NULL, NULL}
};

static NspMethods *gsettings_get_methods(void) { return gsettings_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsettings_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSimpleAction ----------- */


#define  NspGSimpleAction_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsimpleaction.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSimpleAction inherits from GObject 
 * and implements GAction
 */

int nsp_type_gsimpleaction_id=0;
NspTypeGSimpleAction *nsp_type_gsimpleaction=NULL;

/*
 * Type object for NspGSimpleAction 
 * all the instance of NspTypeGSimpleAction share the same id. 
 * nsp_type_gsimpleaction: is an instance of NspTypeGSimpleAction 
 *    used for objects of NspGSimpleAction type (i.e built with new_gsimpleaction) 
 * other instances are used for derived classes 
 */
NspTypeGSimpleAction *new_type_gsimpleaction(type_mode mode)
{
  NspTypeGAction *t_gaction;
  NspTypeGSimpleAction *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsimpleaction != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsimpleaction;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsimpleaction_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsimpleaction_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsimpleaction;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsimpleaction */ 

  top->s_type =  (s_type_func *) nsp_gsimpleaction_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsimpleaction_type_short_string;
  /* top->create = (create_func*) int_gsimpleaction_create;*/

  /* specific methods for gsimpleaction */

  type->init = (init_func *) init_gsimpleaction;

  /* 
   * NspGSimpleAction interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_gaction = new_type_gaction(T_DERIVED);
  type->interface = (NspTypeBase * ) t_gaction;
  if ( nsp_type_gsimpleaction_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSimpleAction called nsp_type_gsimpleaction
       */
      type->id =  nsp_type_gsimpleaction_id = nsp_new_type_id();
      nsp_type_gsimpleaction = type;
      if ( nsp_register_type(nsp_type_gsimpleaction) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsimpleaction, G_TYPE_SIMPLE_ACTION);
      return ( mode == T_BASE ) ? type : new_type_gsimpleaction(mode);
    }
  else 
    {
      type->id = nsp_type_gsimpleaction_id;
      return type;
    }
}

/*
 * initialize NspGSimpleAction instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsimpleaction(NspGSimpleAction *Obj,NspTypeGSimpleAction *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSimpleAction 
 */

NspGSimpleAction *new_gsimpleaction() 
{
  NspGSimpleAction *loc;
  /* type must exists */
  nsp_type_gsimpleaction = new_type_gsimpleaction(T_BASE);
  if ( (loc = malloc(sizeof(NspGSimpleAction)))== NULLGSIMPLEACTION) return loc;
  /* initialize object */
  if ( init_gsimpleaction(loc,nsp_type_gsimpleaction) == FAIL) return NULLGSIMPLEACTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSimpleAction 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsimpleaction_type_name[]="GSimpleAction";
static char gsimpleaction_short_type_name[]="GSimpleAction";

static char *nsp_gsimpleaction_type_as_string(void)
{
  return(gsimpleaction_type_name);
}

static char *nsp_gsimpleaction_type_short_string(NspObject *v)
{
  return(gsimpleaction_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSimpleAction objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSimpleAction   *nsp_gsimpleaction_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsimpleaction_id)  == TRUE  ) return ((NspGSimpleAction *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsimpleaction));
  return NULL;
}

int IsGSimpleActionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsimpleaction_id);
}

int IsGSimpleAction(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsimpleaction_id);
}

NspGSimpleAction  *GetGSimpleActionCopy(Stack stack, int i)
{
  if (  GetGSimpleAction(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSimpleAction  *GetGSimpleAction(Stack stack, int i)
{
  NspGSimpleAction *M;
  if (( M = nsp_gsimpleaction_object(NthObj(i))) == NULLGSIMPLEACTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSimpleAction *gsimpleaction_copy(NspGSimpleAction *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsimpleaction);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsimpleaction);
}

/*-------------------------------------------------------------------
 * wrappers for the GSimpleAction
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_simple_action_new_stateful (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,new_opts, t_end};
  nsp_option opts[] = {
	{"parameter_type",obj,NULLOBJ,-1},
	{"state",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  char *name;
  GVariantType *parameter_type = NULL;
  NspObject *nsp_parameter_type = NULL, *nsp_state = NULL;
  GVariant *state = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name, opts, &nsp_parameter_type, &nsp_state) == FAIL) return RET_BUG;
  if (nsp_parameter_type != NULL)
     {
      if( IsGVariantType(nsp_parameter_type))
        { parameter_type = ((NspGVariantType *) nsp_parameter_type)->obj->value;}
      else
        {Scierror("Error: parameter_type should be of type GVariantType\n");
         return RET_BUG;
        }
      if((parameter_type = nsp_copy_GVariantType(parameter_type))==NULL) return RET_BUG;
     }
  if (nsp_state != NULL)
     {
      if( IsGVariant(nsp_state))
        { state = ((NspGVariant *) nsp_state)->obj->value;}
      else
        {Scierror("Error: state should be of type GVariant\n");
         return RET_BUG;
        }
      if((state = nsp_copy_GVariant(state))==NULL) return RET_BUG;
     }
  if ((ret = (GObject *)g_simple_action_new_stateful(name,parameter_type,state))== NULL) return RET_BUG;

  nsp_type_gsimpleaction = new_type_gsimpleaction(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsimpleaction );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_simple_action_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,new_opts, t_end};
  nsp_option opts[] = {
	{"parameter_type",obj,NULLOBJ,-1},
	{NULL,t_end,NULLOBJ,-1} };
  char *name;
  GVariantType *parameter_type = NULL;
  NspObject *nsp_parameter_type = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&name, opts, &nsp_parameter_type) == FAIL) return RET_BUG;
  if (nsp_parameter_type != NULL)
     {
      if( IsGVariantType(nsp_parameter_type))
        { parameter_type = ((NspGVariantType *) nsp_parameter_type)->obj->value;}
      else
        {Scierror("Error: parameter_type should be of type GVariantType\n");
         return RET_BUG;
        }
      if((parameter_type = nsp_copy_GVariantType(parameter_type))==NULL) return RET_BUG;
     }
  if ((ret = (GObject *)g_simple_action_new(name,parameter_type))== NULL) return RET_BUG;

  nsp_type_gsimpleaction = new_type_gsimpleaction(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsimpleaction );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_simple_action_set_enabled(NspGSimpleAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int enabled;
  if ( GetArgs(stack,rhs,opt,T,&enabled) == FAIL) return RET_BUG;
    g_simple_action_set_enabled(G_SIMPLE_ACTION(self->obj),enabled);
  return 0;
}

#if GTK_CHECK_VERSION(2,30,0)
static int _wrap_g_simple_action_set_state(NspGSimpleAction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GVariant *value = NULL;
  NspObject *nsp_value = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_value))
    { value = ((NspGVariant *) nsp_value)->obj->value;
      if((value = nsp_copy_GVariant(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type GVariant\n");
      return RET_BUG;
    }
    g_simple_action_set_state(G_SIMPLE_ACTION(self->obj),value);
  return 0;
}

#else
int _wrap_g_simple_action_set_state(Stack stack, int rhs, int opt, int lhs) /* set_state */
{
  Scierror("Error: function g_simple_action_set_state not available\n");
  return RET_BUG;
}
#endif
static NspMethods gsimpleaction_methods[] = {
  {"set_enabled",(nsp_method *) _wrap_g_simple_action_set_enabled},
  {"set_state",(nsp_method *) _wrap_g_simple_action_set_state},
  { NULL, NULL}
};

static NspMethods *gsimpleaction_get_methods(void) { return gsimpleaction_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsimpleaction_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSimpleActionGroup ----------- */


#define  NspGSimpleActionGroup_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsimpleactiongroup.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSimpleActionGroup inherits from GObject 
 * and implements GActionGroup GActionMap
 */

int nsp_type_gsimpleactiongroup_id=0;
NspTypeGSimpleActionGroup *nsp_type_gsimpleactiongroup=NULL;

/*
 * Type object for NspGSimpleActionGroup 
 * all the instance of NspTypeGSimpleActionGroup share the same id. 
 * nsp_type_gsimpleactiongroup: is an instance of NspTypeGSimpleActionGroup 
 *    used for objects of NspGSimpleActionGroup type (i.e built with new_gsimpleactiongroup) 
 * other instances are used for derived classes 
 */
NspTypeGSimpleActionGroup *new_type_gsimpleactiongroup(type_mode mode)
{
  NspTypeGActionGroup *t_gactiongroup;
  NspTypeGActionMap *t_gactionmap;
  NspTypeGSimpleActionGroup *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsimpleactiongroup != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsimpleactiongroup;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsimpleactiongroup_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsimpleactiongroup_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsimpleactiongroup;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsimpleactiongroup */ 

  top->s_type =  (s_type_func *) nsp_gsimpleactiongroup_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsimpleactiongroup_type_short_string;
  /* top->create = (create_func*) int_gsimpleactiongroup_create;*/

  /* specific methods for gsimpleactiongroup */

  type->init = (init_func *) init_gsimpleactiongroup;

  /* 
   * NspGSimpleActionGroup interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  t_gactiongroup = new_type_gactiongroup(T_DERIVED);
  type->interface = (NspTypeBase * ) t_gactiongroup;
  t_gactionmap = new_type_gactionmap(T_DERIVED);
  type->interface->interface = (NspTypeBase * ) t_gactionmap;
  if ( nsp_type_gsimpleactiongroup_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSimpleActionGroup called nsp_type_gsimpleactiongroup
       */
      type->id =  nsp_type_gsimpleactiongroup_id = nsp_new_type_id();
      nsp_type_gsimpleactiongroup = type;
      if ( nsp_register_type(nsp_type_gsimpleactiongroup) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsimpleactiongroup, G_TYPE_SIMPLE_ACTION_GROUP);
      return ( mode == T_BASE ) ? type : new_type_gsimpleactiongroup(mode);
    }
  else 
    {
      type->id = nsp_type_gsimpleactiongroup_id;
      return type;
    }
}

/*
 * initialize NspGSimpleActionGroup instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsimpleactiongroup(NspGSimpleActionGroup *Obj,NspTypeGSimpleActionGroup *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSimpleActionGroup 
 */

NspGSimpleActionGroup *new_gsimpleactiongroup() 
{
  NspGSimpleActionGroup *loc;
  /* type must exists */
  nsp_type_gsimpleactiongroup = new_type_gsimpleactiongroup(T_BASE);
  if ( (loc = malloc(sizeof(NspGSimpleActionGroup)))== NULLGSIMPLEACTIONGROUP) return loc;
  /* initialize object */
  if ( init_gsimpleactiongroup(loc,nsp_type_gsimpleactiongroup) == FAIL) return NULLGSIMPLEACTIONGROUP;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSimpleActionGroup 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsimpleactiongroup_type_name[]="GSimpleActionGroup";
static char gsimpleactiongroup_short_type_name[]="GSimpleActionGroup";

static char *nsp_gsimpleactiongroup_type_as_string(void)
{
  return(gsimpleactiongroup_type_name);
}

static char *nsp_gsimpleactiongroup_type_short_string(NspObject *v)
{
  return(gsimpleactiongroup_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSimpleActionGroup objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSimpleActionGroup   *nsp_gsimpleactiongroup_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsimpleactiongroup_id)  == TRUE  ) return ((NspGSimpleActionGroup *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsimpleactiongroup));
  return NULL;
}

int IsGSimpleActionGroupObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsimpleactiongroup_id);
}

int IsGSimpleActionGroup(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsimpleactiongroup_id);
}

NspGSimpleActionGroup  *GetGSimpleActionGroupCopy(Stack stack, int i)
{
  if (  GetGSimpleActionGroup(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSimpleActionGroup  *GetGSimpleActionGroup(Stack stack, int i)
{
  NspGSimpleActionGroup *M;
  if (( M = nsp_gsimpleactiongroup_object(NthObj(i))) == NULLGSIMPLEACTIONGROUP)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSimpleActionGroup *gsimpleactiongroup_copy(NspGSimpleActionGroup *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsimpleactiongroup);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsimpleactiongroup);
}

/*-------------------------------------------------------------------
 * wrappers for the GSimpleActionGroup
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_simple_action_group_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_simple_action_group_new())== NULL) return RET_BUG;

  nsp_type_gsimpleactiongroup = new_type_gsimpleactiongroup(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsimpleactiongroup );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gsimpleactiongroup_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsimpleactiongroup_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSimpleProxyResolver ----------- */


#define  NspGSimpleProxyResolver_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsimpleproxyresolver.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSimpleProxyResolver inherits from GObject 
 */

int nsp_type_gsimpleproxyresolver_id=0;
NspTypeGSimpleProxyResolver *nsp_type_gsimpleproxyresolver=NULL;

/*
 * Type object for NspGSimpleProxyResolver 
 * all the instance of NspTypeGSimpleProxyResolver share the same id. 
 * nsp_type_gsimpleproxyresolver: is an instance of NspTypeGSimpleProxyResolver 
 *    used for objects of NspGSimpleProxyResolver type (i.e built with new_gsimpleproxyresolver) 
 * other instances are used for derived classes 
 */
NspTypeGSimpleProxyResolver *new_type_gsimpleproxyresolver(type_mode mode)
{
  NspTypeGSimpleProxyResolver *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsimpleproxyresolver != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsimpleproxyresolver;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsimpleproxyresolver_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsimpleproxyresolver_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsimpleproxyresolver;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsimpleproxyresolver */ 

  top->s_type =  (s_type_func *) nsp_gsimpleproxyresolver_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsimpleproxyresolver_type_short_string;
  /* top->create = (create_func*) int_gsimpleproxyresolver_create;*/

  /* specific methods for gsimpleproxyresolver */

  type->init = (init_func *) init_gsimpleproxyresolver;

  /* 
   * NspGSimpleProxyResolver interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsimpleproxyresolver_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSimpleProxyResolver called nsp_type_gsimpleproxyresolver
       */
      type->id =  nsp_type_gsimpleproxyresolver_id = nsp_new_type_id();
      nsp_type_gsimpleproxyresolver = type;
      if ( nsp_register_type(nsp_type_gsimpleproxyresolver) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsimpleproxyresolver, G_TYPE_SIMPLE_PROXY_RESOLVER);
      return ( mode == T_BASE ) ? type : new_type_gsimpleproxyresolver(mode);
    }
  else 
    {
      type->id = nsp_type_gsimpleproxyresolver_id;
      return type;
    }
}

/*
 * initialize NspGSimpleProxyResolver instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsimpleproxyresolver(NspGSimpleProxyResolver *Obj,NspTypeGSimpleProxyResolver *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSimpleProxyResolver 
 */

NspGSimpleProxyResolver *new_gsimpleproxyresolver() 
{
  NspGSimpleProxyResolver *loc;
  /* type must exists */
  nsp_type_gsimpleproxyresolver = new_type_gsimpleproxyresolver(T_BASE);
  if ( (loc = malloc(sizeof(NspGSimpleProxyResolver)))== NULLGSIMPLEPROXYRESOLVER) return loc;
  /* initialize object */
  if ( init_gsimpleproxyresolver(loc,nsp_type_gsimpleproxyresolver) == FAIL) return NULLGSIMPLEPROXYRESOLVER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSimpleProxyResolver 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsimpleproxyresolver_type_name[]="GSimpleProxyResolver";
static char gsimpleproxyresolver_short_type_name[]="GSimpleProxyResolver";

static char *nsp_gsimpleproxyresolver_type_as_string(void)
{
  return(gsimpleproxyresolver_type_name);
}

static char *nsp_gsimpleproxyresolver_type_short_string(NspObject *v)
{
  return(gsimpleproxyresolver_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSimpleProxyResolver objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSimpleProxyResolver   *nsp_gsimpleproxyresolver_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsimpleproxyresolver_id)  == TRUE  ) return ((NspGSimpleProxyResolver *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsimpleproxyresolver));
  return NULL;
}

int IsGSimpleProxyResolverObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsimpleproxyresolver_id);
}

int IsGSimpleProxyResolver(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsimpleproxyresolver_id);
}

NspGSimpleProxyResolver  *GetGSimpleProxyResolverCopy(Stack stack, int i)
{
  if (  GetGSimpleProxyResolver(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSimpleProxyResolver  *GetGSimpleProxyResolver(Stack stack, int i)
{
  NspGSimpleProxyResolver *M;
  if (( M = nsp_gsimpleproxyresolver_object(NthObj(i))) == NULLGSIMPLEPROXYRESOLVER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSimpleProxyResolver *gsimpleproxyresolver_copy(NspGSimpleProxyResolver *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsimpleproxyresolver);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsimpleproxyresolver);
}

/*-------------------------------------------------------------------
 * wrappers for the GSimpleProxyResolver
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_simple_proxy_resolver_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,obj, t_end};
  char *default_proxy;
  gchar **ignore_hosts = NULL;
  NspObject *nsp_ignore_hosts = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&default_proxy, &nsp_ignore_hosts) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_ignore_hosts))
    { ignore_hosts =  ((NspSMatrix *) nsp_ignore_hosts)->S;}
  else
    {
      Scierror("Error: ignore_hosts should be of type SMat");
      return RET_BUG;
    }
  if ((ret = (GObject *)g_simple_proxy_resolver_new(default_proxy,ignore_hosts))== NULL) return RET_BUG;

  nsp_type_gsimpleproxyresolver = new_type_gsimpleproxyresolver(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsimpleproxyresolver );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_simple_proxy_resolver_set_default_proxy(NspGSimpleProxyResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *default_proxy;
  if ( GetArgs(stack,rhs,opt,T,&default_proxy) == FAIL) return RET_BUG;
    g_simple_proxy_resolver_set_default_proxy(G_SIMPLE_PROXY_RESOLVER(self->obj),default_proxy);
  return 0;
}

#else
int _wrap_g_simple_proxy_resolver_set_default_proxy(Stack stack, int rhs, int opt, int lhs) /* set_default_proxy */
{
  Scierror("Error: function g_simple_proxy_resolver_set_default_proxy not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_simple_proxy_resolver_set_ignore_hosts(NspGSimpleProxyResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  gchar **ignore_hosts = NULL;
  NspObject *nsp_ignore_hosts = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_ignore_hosts) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_ignore_hosts))
    { ignore_hosts =  ((NspSMatrix *) nsp_ignore_hosts)->S;}
  else
    {
      Scierror("Error: ignore_hosts should be of type SMat");
      return RET_BUG;
    }
    g_simple_proxy_resolver_set_ignore_hosts(G_SIMPLE_PROXY_RESOLVER(self->obj),ignore_hosts);
  return 0;
}

#else
int _wrap_g_simple_proxy_resolver_set_ignore_hosts(Stack stack, int rhs, int opt, int lhs) /* set_ignore_hosts */
{
  Scierror("Error: function g_simple_proxy_resolver_set_ignore_hosts not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_simple_proxy_resolver_set_uri_proxy(NspGSimpleProxyResolver *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string, t_end};
  char *uri_scheme, *proxy;
  if ( GetArgs(stack,rhs,opt,T,&uri_scheme, &proxy) == FAIL) return RET_BUG;
    g_simple_proxy_resolver_set_uri_proxy(G_SIMPLE_PROXY_RESOLVER(self->obj),uri_scheme,proxy);
  return 0;
}

#else
int _wrap_g_simple_proxy_resolver_set_uri_proxy(Stack stack, int rhs, int opt, int lhs) /* set_uri_proxy */
{
  Scierror("Error: function g_simple_proxy_resolver_set_uri_proxy not available\n");
  return RET_BUG;
}
#endif
static NspMethods gsimpleproxyresolver_methods[] = {
  {"set_default_proxy",(nsp_method *) _wrap_g_simple_proxy_resolver_set_default_proxy},
  {"set_ignore_hosts",(nsp_method *) _wrap_g_simple_proxy_resolver_set_ignore_hosts},
  {"set_uri_proxy",(nsp_method *) _wrap_g_simple_proxy_resolver_set_uri_proxy},
  { NULL, NULL}
};

static NspMethods *gsimpleproxyresolver_get_methods(void) { return gsimpleproxyresolver_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsimpleproxyresolver_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocket ----------- */


#define  NspGSocket_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocket.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocket inherits from GObject 
 */

int nsp_type_gsocket_id=0;
NspTypeGSocket *nsp_type_gsocket=NULL;

/*
 * Type object for NspGSocket 
 * all the instance of NspTypeGSocket share the same id. 
 * nsp_type_gsocket: is an instance of NspTypeGSocket 
 *    used for objects of NspGSocket type (i.e built with new_gsocket) 
 * other instances are used for derived classes 
 */
NspTypeGSocket *new_type_gsocket(type_mode mode)
{
  NspTypeGSocket *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocket != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocket;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocket_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocket_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocket;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocket */ 

  top->s_type =  (s_type_func *) nsp_gsocket_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocket_type_short_string;
  /* top->create = (create_func*) int_gsocket_create;*/

  /* specific methods for gsocket */

  type->init = (init_func *) init_gsocket;

  /* 
   * NspGSocket interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocket_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocket called nsp_type_gsocket
       */
      type->id =  nsp_type_gsocket_id = nsp_new_type_id();
      nsp_type_gsocket = type;
      if ( nsp_register_type(nsp_type_gsocket) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocket, G_TYPE_SOCKET);
      return ( mode == T_BASE ) ? type : new_type_gsocket(mode);
    }
  else 
    {
      type->id = nsp_type_gsocket_id;
      return type;
    }
}

/*
 * initialize NspGSocket instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocket(NspGSocket *Obj,NspTypeGSocket *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocket 
 */

NspGSocket *new_gsocket() 
{
  NspGSocket *loc;
  /* type must exists */
  nsp_type_gsocket = new_type_gsocket(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocket)))== NULLGSOCKET) return loc;
  /* initialize object */
  if ( init_gsocket(loc,nsp_type_gsocket) == FAIL) return NULLGSOCKET;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocket 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocket_type_name[]="GSocket";
static char gsocket_short_type_name[]="GSocket";

static char *nsp_gsocket_type_as_string(void)
{
  return(gsocket_type_name);
}

static char *nsp_gsocket_type_short_string(NspObject *v)
{
  return(gsocket_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocket objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocket   *nsp_gsocket_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocket_id)  == TRUE  ) return ((NspGSocket *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocket));
  return NULL;
}

int IsGSocketObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocket_id);
}

int IsGSocket(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocket_id);
}

NspGSocket  *GetGSocketCopy(Stack stack, int i)
{
  if (  GetGSocket(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocket  *GetGSocket(Stack stack, int i)
{
  NspGSocket *M;
  if (( M = nsp_gsocket_object(NthObj(i))) == NULLGSOCKET)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocket *gsocket_copy(NspGSocket *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocket);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocket);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocket
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_socket_new_from_fd (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int fd;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&fd) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_socket_new_from_fd(fd,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gsocket = new_type_gsocket(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsocket );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_socket_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj,obj,obj, t_end};
  GSocketFamily family;
  NspObject *nsp_family = NULL, *nsp_type = NULL, *nsp_protocol = NULL;
  GSocketType type;
  GSocketProtocol protocol;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_family, &nsp_type, &nsp_protocol) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_FAMILY, nsp_family, &family)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_PROTOCOL, nsp_protocol, &protocol)== FAIL)
      return RET_BUG;
  if ((ret = (GObject *)g_socket_new(family,type,protocol,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gsocket = new_type_gsocket(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsocket );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_get_fd(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_fd(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_get_family(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_get_family(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_get_socket_type(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_get_socket_type(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_get_protocol(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_get_protocol(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_get_local_address(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  GSocketAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_get_local_address(G_SOCKET(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_get_remote_address(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  GSocketAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_get_remote_address(G_SOCKET(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_set_blocking(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int blocking;
  if ( GetArgs(stack,rhs,opt,T,&blocking) == FAIL) return RET_BUG;
    g_socket_set_blocking(G_SOCKET(self->obj),blocking);
  return 0;
}

static int _wrap_g_socket_get_blocking(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_blocking(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_set_keepalive(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int keepalive;
  if ( GetArgs(stack,rhs,opt,T,&keepalive) == FAIL) return RET_BUG;
    g_socket_set_keepalive(G_SOCKET(self->obj),keepalive);
  return 0;
}

static int _wrap_g_socket_get_keepalive(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_keepalive(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_get_listen_backlog(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_listen_backlog(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_set_listen_backlog(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int backlog;
  if ( GetArgs(stack,rhs,opt,T,&backlog) == FAIL) return RET_BUG;
    g_socket_set_listen_backlog(G_SOCKET(self->obj),backlog);
  return 0;
}

static int _wrap_g_socket_get_timeout(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_timeout(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_set_timeout(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int timeout;
  if ( GetArgs(stack,rhs,opt,T,&timeout) == FAIL) return RET_BUG;
    g_socket_set_timeout(G_SOCKET(self->obj),timeout);
  return 0;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_get_ttl(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_ttl(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_get_ttl(Stack stack, int rhs, int opt, int lhs) /* get_ttl */
{
  Scierror("Error: function g_socket_get_ttl not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_set_ttl(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int ttl;
  if ( GetArgs(stack,rhs,opt,T,&ttl) == FAIL) return RET_BUG;
    g_socket_set_ttl(G_SOCKET(self->obj),ttl);
  return 0;
}

#else
int _wrap_g_socket_set_ttl(Stack stack, int rhs, int opt, int lhs) /* set_ttl */
{
  Scierror("Error: function g_socket_set_ttl not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_get_broadcast(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_broadcast(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_get_broadcast(Stack stack, int rhs, int opt, int lhs) /* get_broadcast */
{
  Scierror("Error: function g_socket_get_broadcast not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_set_broadcast(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int broadcast;
  if ( GetArgs(stack,rhs,opt,T,&broadcast) == FAIL) return RET_BUG;
    g_socket_set_broadcast(G_SOCKET(self->obj),broadcast);
  return 0;
}

#else
int _wrap_g_socket_set_broadcast(Stack stack, int rhs, int opt, int lhs) /* set_broadcast */
{
  Scierror("Error: function g_socket_set_broadcast not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_get_multicast_loopback(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_multicast_loopback(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_get_multicast_loopback(Stack stack, int rhs, int opt, int lhs) /* get_multicast_loopback */
{
  Scierror("Error: function g_socket_get_multicast_loopback not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_set_multicast_loopback(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int loopback;
  if ( GetArgs(stack,rhs,opt,T,&loopback) == FAIL) return RET_BUG;
    g_socket_set_multicast_loopback(G_SOCKET(self->obj),loopback);
  return 0;
}

#else
int _wrap_g_socket_set_multicast_loopback(Stack stack, int rhs, int opt, int lhs) /* set_multicast_loopback */
{
  Scierror("Error: function g_socket_set_multicast_loopback not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_get_multicast_ttl(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_multicast_ttl(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_get_multicast_ttl(Stack stack, int rhs, int opt, int lhs) /* get_multicast_ttl */
{
  Scierror("Error: function g_socket_get_multicast_ttl not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_set_multicast_ttl(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int ttl;
  if ( GetArgs(stack,rhs,opt,T,&ttl) == FAIL) return RET_BUG;
    g_socket_set_multicast_ttl(G_SOCKET(self->obj),ttl);
  return 0;
}

#else
int _wrap_g_socket_set_multicast_ttl(Stack stack, int rhs, int opt, int lhs) /* set_multicast_ttl */
{
  Scierror("Error: function g_socket_set_multicast_ttl not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_socket_is_connected(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_is_connected(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_bind(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_bool, t_end};
  NspGObject *address;
  int allow_reuse, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketaddress, &address, &allow_reuse) == FAIL) return RET_BUG;
    ret =g_socket_bind(G_SOCKET(self->obj),G_SOCKET_ADDRESS(address->obj),allow_reuse,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_join_multicast_group(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_bool,string, t_end};
  NspGObject *group;
  int source_specific, ret;
  char *iface;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &group, &source_specific, &iface) == FAIL) return RET_BUG;
    ret =g_socket_join_multicast_group(G_SOCKET(self->obj),G_INET_ADDRESS(group->obj),source_specific,iface,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_join_multicast_group(Stack stack, int rhs, int opt, int lhs) /* join_multicast_group */
{
  Scierror("Error: function g_socket_join_multicast_group not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_leave_multicast_group(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,s_bool,string, t_end};
  NspGObject *group;
  int source_specific, ret;
  char *iface;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &group, &source_specific, &iface) == FAIL) return RET_BUG;
    ret =g_socket_leave_multicast_group(G_SOCKET(self->obj),G_INET_ADDRESS(group->obj),source_specific,iface,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_leave_multicast_group(Stack stack, int rhs, int opt, int lhs) /* leave_multicast_group */
{
  Scierror("Error: function g_socket_leave_multicast_group not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_socket_connect(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *address, *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketaddress, &address, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_connect(G_SOCKET(self->obj),G_SOCKET_ADDRESS(address->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_check_connect_result(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_socket_check_connect_result(G_SOCKET(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_get_available_bytes(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_get_available_bytes(G_SOCKET(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_accept(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  GSocket *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_accept(G_SOCKET(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocket = new_type_gsocket(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocket))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_listen(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_socket_listen(G_SOCKET(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_receive(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,obj_check, t_end};
  char *buffer;
  int size, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&buffer, &size, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_receive(G_SOCKET(self->obj),buffer,size,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_send(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,obj_check, t_end};
  char *buffer;
  int size, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&buffer, &size, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_send(G_SOCKET(self->obj),buffer,size,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_send_to(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,string,s_int,obj_check, t_end};
  NspGObject *address, *cancellable;
  char *buffer;
  int size, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketaddress, &address, &buffer, &size, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_send_to(G_SOCKET(self->obj),G_SOCKET_ADDRESS(address->obj),buffer,size,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_close(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  int ret;
  CheckRhs(0,0);
    ret =g_socket_close(G_SOCKET(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_shutdown(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool,s_bool, t_end};
  int shutdown_read, shutdown_write, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&shutdown_read, &shutdown_write) == FAIL) return RET_BUG;
    ret =g_socket_shutdown(G_SOCKET(self->obj),shutdown_read,shutdown_write,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_is_closed(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_is_closed(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_speaks_ipv4(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_speaks_ipv4(G_SOCKET(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_receive_with_blocking(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,s_bool,obj_check, t_end};
  char *buffer;
  int size, blocking, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&buffer, &size, &blocking, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_receive_with_blocking(G_SOCKET(self->obj),buffer,size,blocking,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_send_with_blocking(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,s_bool,obj_check, t_end};
  char *buffer;
  int size, blocking, ret;
  NspGObject *cancellable;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&buffer, &size, &blocking, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_send_with_blocking(G_SOCKET(self->obj),buffer,size,blocking,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_socket_get_option(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int, t_end};
  int level, optname, value, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&level, &optname, &value) == FAIL) return RET_BUG;
    ret =g_socket_get_option(G_SOCKET(self->obj),level,optname,&value,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_get_option(Stack stack, int rhs, int opt, int lhs) /* get_option */
{
  Scierror("Error: function g_socket_get_option not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,36,0)
static int _wrap_g_socket_set_option(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,s_int,s_int, t_end};
  int level, optname, value, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&level, &optname, &value) == FAIL) return RET_BUG;
    ret =g_socket_set_option(G_SOCKET(self->obj),level,optname,value,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_set_option(Stack stack, int rhs, int opt, int lhs) /* set_option */
{
  Scierror("Error: function g_socket_set_option not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_socket_connection_factory_create_connection(NspGSocket *self,Stack stack,int rhs,int opt,int lhs)
{
  GSocketConnection *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_connection_factory_create_connection(G_SOCKET(self->obj));
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gsocket_methods[] = {
  {"get_fd",(nsp_method *) _wrap_g_socket_get_fd},
  {"get_family",(nsp_method *) _wrap_g_socket_get_family},
  {"get_socket_type",(nsp_method *) _wrap_g_socket_get_socket_type},
  {"get_protocol",(nsp_method *) _wrap_g_socket_get_protocol},
  {"get_local_address",(nsp_method *) _wrap_g_socket_get_local_address},
  {"get_remote_address",(nsp_method *) _wrap_g_socket_get_remote_address},
  {"set_blocking",(nsp_method *) _wrap_g_socket_set_blocking},
  {"get_blocking",(nsp_method *) _wrap_g_socket_get_blocking},
  {"set_keepalive",(nsp_method *) _wrap_g_socket_set_keepalive},
  {"get_keepalive",(nsp_method *) _wrap_g_socket_get_keepalive},
  {"get_listen_backlog",(nsp_method *) _wrap_g_socket_get_listen_backlog},
  {"set_listen_backlog",(nsp_method *) _wrap_g_socket_set_listen_backlog},
  {"get_timeout",(nsp_method *) _wrap_g_socket_get_timeout},
  {"set_timeout",(nsp_method *) _wrap_g_socket_set_timeout},
  {"get_ttl",(nsp_method *) _wrap_g_socket_get_ttl},
  {"set_ttl",(nsp_method *) _wrap_g_socket_set_ttl},
  {"get_broadcast",(nsp_method *) _wrap_g_socket_get_broadcast},
  {"set_broadcast",(nsp_method *) _wrap_g_socket_set_broadcast},
  {"get_multicast_loopback",(nsp_method *) _wrap_g_socket_get_multicast_loopback},
  {"set_multicast_loopback",(nsp_method *) _wrap_g_socket_set_multicast_loopback},
  {"get_multicast_ttl",(nsp_method *) _wrap_g_socket_get_multicast_ttl},
  {"set_multicast_ttl",(nsp_method *) _wrap_g_socket_set_multicast_ttl},
  {"is_connected",(nsp_method *) _wrap_g_socket_is_connected},
  {"bind",(nsp_method *) _wrap_g_socket_bind},
  {"join_multicast_group",(nsp_method *) _wrap_g_socket_join_multicast_group},
  {"leave_multicast_group",(nsp_method *) _wrap_g_socket_leave_multicast_group},
  {"connect",(nsp_method *) _wrap_g_socket_connect},
  {"check_connect_result",(nsp_method *) _wrap_g_socket_check_connect_result},
  {"get_available_bytes",(nsp_method *) _wrap_g_socket_get_available_bytes},
  {"accept",(nsp_method *) _wrap_g_socket_accept},
  {"listen",(nsp_method *) _wrap_g_socket_listen},
  {"receive",(nsp_method *) _wrap_g_socket_receive},
  {"send",(nsp_method *) _wrap_g_socket_send},
  {"send_to",(nsp_method *) _wrap_g_socket_send_to},
  {"close",(nsp_method *) _wrap_g_socket_close},
  {"shutdown",(nsp_method *) _wrap_g_socket_shutdown},
  {"is_closed",(nsp_method *) _wrap_g_socket_is_closed},
  {"speaks_ipv4",(nsp_method *) _wrap_g_socket_speaks_ipv4},
  {"receive_with_blocking",(nsp_method *) _wrap_g_socket_receive_with_blocking},
  {"send_with_blocking",(nsp_method *) _wrap_g_socket_send_with_blocking},
  {"get_option",(nsp_method *) _wrap_g_socket_get_option},
  {"set_option",(nsp_method *) _wrap_g_socket_set_option},
  {"connection_factory_create_connection",(nsp_method *) _wrap_g_socket_connection_factory_create_connection},
  { NULL, NULL}
};

static NspMethods *gsocket_get_methods(void) { return gsocket_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocket_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketAddress ----------- */


#define  NspGSocketAddress_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketaddress.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketAddress inherits from GObject 
 */

int nsp_type_gsocketaddress_id=0;
NspTypeGSocketAddress *nsp_type_gsocketaddress=NULL;

/*
 * Type object for NspGSocketAddress 
 * all the instance of NspTypeGSocketAddress share the same id. 
 * nsp_type_gsocketaddress: is an instance of NspTypeGSocketAddress 
 *    used for objects of NspGSocketAddress type (i.e built with new_gsocketaddress) 
 * other instances are used for derived classes 
 */
NspTypeGSocketAddress *new_type_gsocketaddress(type_mode mode)
{
  NspTypeGSocketAddress *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketaddress != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketaddress;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketaddress_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketaddress_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketaddress;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketaddress */ 

  top->s_type =  (s_type_func *) nsp_gsocketaddress_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketaddress_type_short_string;
  /* top->create = (create_func*) int_gsocketaddress_create;*/

  /* specific methods for gsocketaddress */

  type->init = (init_func *) init_gsocketaddress;

  /* 
   * NspGSocketAddress interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketaddress_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketAddress called nsp_type_gsocketaddress
       */
      type->id =  nsp_type_gsocketaddress_id = nsp_new_type_id();
      nsp_type_gsocketaddress = type;
      if ( nsp_register_type(nsp_type_gsocketaddress) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketaddress, G_TYPE_SOCKET_ADDRESS);
      return ( mode == T_BASE ) ? type : new_type_gsocketaddress(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketaddress_id;
      return type;
    }
}

/*
 * initialize NspGSocketAddress instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketaddress(NspGSocketAddress *Obj,NspTypeGSocketAddress *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketAddress 
 */

NspGSocketAddress *new_gsocketaddress() 
{
  NspGSocketAddress *loc;
  /* type must exists */
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketAddress)))== NULLGSOCKETADDRESS) return loc;
  /* initialize object */
  if ( init_gsocketaddress(loc,nsp_type_gsocketaddress) == FAIL) return NULLGSOCKETADDRESS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketAddress 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketaddress_type_name[]="GSocketAddress";
static char gsocketaddress_short_type_name[]="GSocketAddress";

static char *nsp_gsocketaddress_type_as_string(void)
{
  return(gsocketaddress_type_name);
}

static char *nsp_gsocketaddress_type_short_string(NspObject *v)
{
  return(gsocketaddress_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketAddress objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketAddress   *nsp_gsocketaddress_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketaddress_id)  == TRUE  ) return ((NspGSocketAddress *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketaddress));
  return NULL;
}

int IsGSocketAddressObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketaddress_id);
}

int IsGSocketAddress(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketaddress_id);
}

NspGSocketAddress  *GetGSocketAddressCopy(Stack stack, int i)
{
  if (  GetGSocketAddress(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketAddress  *GetGSocketAddress(Stack stack, int i)
{
  NspGSocketAddress *M;
  if (( M = nsp_gsocketaddress_object(NthObj(i))) == NULLGSOCKETADDRESS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketAddress *gsocketaddress_copy(NspGSocketAddress *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketaddress);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketaddress);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketAddress
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_socket_address_get_family(NspGSocketAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_address_get_family(G_SOCKET_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_address_get_native_size(NspGSocketAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_address_get_native_size(G_SOCKET_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gsocketaddress_methods[] = {
  {"get_family",(nsp_method *) _wrap_g_socket_address_get_family},
  {"get_native_size",(nsp_method *) _wrap_g_socket_address_get_native_size},
  { NULL, NULL}
};

static NspMethods *gsocketaddress_get_methods(void) { return gsocketaddress_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketaddress_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGInetSocketAddress ----------- */


#define  NspGInetSocketAddress_Private 
#include <nsp/objects.h>
#include <nsp/gtk/ginetsocketaddress.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGInetSocketAddress inherits from GSocketAddress 
 */

int nsp_type_ginetsocketaddress_id=0;
NspTypeGInetSocketAddress *nsp_type_ginetsocketaddress=NULL;

/*
 * Type object for NspGInetSocketAddress 
 * all the instance of NspTypeGInetSocketAddress share the same id. 
 * nsp_type_ginetsocketaddress: is an instance of NspTypeGInetSocketAddress 
 *    used for objects of NspGInetSocketAddress type (i.e built with new_ginetsocketaddress) 
 * other instances are used for derived classes 
 */
NspTypeGInetSocketAddress *new_type_ginetsocketaddress(type_mode mode)
{
  NspTypeGInetSocketAddress *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_ginetsocketaddress != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_ginetsocketaddress;
    }
  if (( type =  malloc(sizeof(NspTypeGSocketAddress))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gsocketaddress(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = ginetsocketaddress_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = ginetsocketaddress_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_ginetsocketaddress;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for ginetsocketaddress */ 

  top->s_type =  (s_type_func *) nsp_ginetsocketaddress_type_as_string;
  top->sh_type = (sh_type_func *) nsp_ginetsocketaddress_type_short_string;
  /* top->create = (create_func*) int_ginetsocketaddress_create;*/

  /* specific methods for ginetsocketaddress */

  type->init = (init_func *) init_ginetsocketaddress;

  /* 
   * NspGInetSocketAddress interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_ginetsocketaddress_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGInetSocketAddress called nsp_type_ginetsocketaddress
       */
      type->id =  nsp_type_ginetsocketaddress_id = nsp_new_type_id();
      nsp_type_ginetsocketaddress = type;
      if ( nsp_register_type(nsp_type_ginetsocketaddress) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_ginetsocketaddress, G_TYPE_INET_SOCKET_ADDRESS);
      return ( mode == T_BASE ) ? type : new_type_ginetsocketaddress(mode);
    }
  else 
    {
      type->id = nsp_type_ginetsocketaddress_id;
      return type;
    }
}

/*
 * initialize NspGInetSocketAddress instances 
 * locally and by calling initializer on parent class 
 */

static int init_ginetsocketaddress(NspGInetSocketAddress *Obj,NspTypeGInetSocketAddress *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGInetSocketAddress 
 */

NspGInetSocketAddress *new_ginetsocketaddress() 
{
  NspGInetSocketAddress *loc;
  /* type must exists */
  nsp_type_ginetsocketaddress = new_type_ginetsocketaddress(T_BASE);
  if ( (loc = malloc(sizeof(NspGInetSocketAddress)))== NULLGINETSOCKETADDRESS) return loc;
  /* initialize object */
  if ( init_ginetsocketaddress(loc,nsp_type_ginetsocketaddress) == FAIL) return NULLGINETSOCKETADDRESS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGInetSocketAddress 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char ginetsocketaddress_type_name[]="GInetSocketAddress";
static char ginetsocketaddress_short_type_name[]="GInetSocketAddress";

static char *nsp_ginetsocketaddress_type_as_string(void)
{
  return(ginetsocketaddress_type_name);
}

static char *nsp_ginetsocketaddress_type_short_string(NspObject *v)
{
  return(ginetsocketaddress_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGInetSocketAddress objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGInetSocketAddress   *nsp_ginetsocketaddress_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_ginetsocketaddress_id)  == TRUE  ) return ((NspGInetSocketAddress *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_ginetsocketaddress));
  return NULL;
}

int IsGInetSocketAddressObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_ginetsocketaddress_id);
}

int IsGInetSocketAddress(NspObject *O)
{
  return nsp_object_type(O,nsp_type_ginetsocketaddress_id);
}

NspGInetSocketAddress  *GetGInetSocketAddressCopy(Stack stack, int i)
{
  if (  GetGInetSocketAddress(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGInetSocketAddress  *GetGInetSocketAddress(Stack stack, int i)
{
  NspGInetSocketAddress *M;
  if (( M = nsp_ginetsocketaddress_object(NthObj(i))) == NULLGINETSOCKETADDRESS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGInetSocketAddress *ginetsocketaddress_copy(NspGInetSocketAddress *self)
{
  /* return gsocketaddress_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginetsocketaddress);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_ginetsocketaddress);
}

/*-------------------------------------------------------------------
 * wrappers for the GInetSocketAddress
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_inet_socket_address_new_from_string (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *address;
  int port;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&address, &port) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_inet_socket_address_new_from_string(address,port))== NULL) return RET_BUG;

  nsp_type_ginetsocketaddress = new_type_ginetsocketaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetsocketaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_inet_socket_address_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,s_int, t_end};
  NspGObject *address;
  int port;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &address, &port) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_inet_socket_address_new(G_INET_ADDRESS(address->obj),port))== NULL) return RET_BUG;

  nsp_type_ginetsocketaddress = new_type_ginetsocketaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_ginetsocketaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_inet_socket_address_get_address(NspGInetSocketAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  GInetAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_inet_socket_address_get_address(G_INET_SOCKET_ADDRESS(self->obj));
  nsp_type_ginetaddress = new_type_ginetaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_ginetaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_inet_socket_address_get_port(NspGInetSocketAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_inet_socket_address_get_port(G_INET_SOCKET_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_socket_address_get_flowinfo(NspGInetSocketAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  gulong ret;
  CheckRhs(0,0);
    ret =g_inet_socket_address_get_flowinfo(G_INET_SOCKET_ADDRESS(self->obj));
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_inet_socket_address_get_flowinfo(Stack stack, int rhs, int opt, int lhs) /* get_flowinfo */
{
  Scierror("Error: function g_inet_socket_address_get_flowinfo not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_inet_socket_address_get_scope_id(NspGInetSocketAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  gulong ret;
  CheckRhs(0,0);
    ret =g_inet_socket_address_get_scope_id(G_INET_SOCKET_ADDRESS(self->obj));
 if (  nsp_move_double(stack,1,(double) ret) == FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_inet_socket_address_get_scope_id(Stack stack, int rhs, int opt, int lhs) /* get_scope_id */
{
  Scierror("Error: function g_inet_socket_address_get_scope_id not available\n");
  return RET_BUG;
}
#endif
static NspMethods ginetsocketaddress_methods[] = {
  {"get_address",(nsp_method *) _wrap_g_inet_socket_address_get_address},
  {"get_port",(nsp_method *) _wrap_g_inet_socket_address_get_port},
  {"get_flowinfo",(nsp_method *) _wrap_g_inet_socket_address_get_flowinfo},
  {"get_scope_id",(nsp_method *) _wrap_g_inet_socket_address_get_scope_id},
  { NULL, NULL}
};

static NspMethods *ginetsocketaddress_get_methods(void) { return ginetsocketaddress_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab ginetsocketaddress_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGProxyAddress ----------- */


#define  NspGProxyAddress_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gproxyaddress.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGProxyAddress inherits from GInetSocketAddress 
 */

int nsp_type_gproxyaddress_id=0;
NspTypeGProxyAddress *nsp_type_gproxyaddress=NULL;

/*
 * Type object for NspGProxyAddress 
 * all the instance of NspTypeGProxyAddress share the same id. 
 * nsp_type_gproxyaddress: is an instance of NspTypeGProxyAddress 
 *    used for objects of NspGProxyAddress type (i.e built with new_gproxyaddress) 
 * other instances are used for derived classes 
 */
NspTypeGProxyAddress *new_type_gproxyaddress(type_mode mode)
{
  NspTypeGProxyAddress *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gproxyaddress != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gproxyaddress;
    }
  if (( type =  malloc(sizeof(NspTypeGInetSocketAddress))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_ginetsocketaddress(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gproxyaddress_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gproxyaddress_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gproxyaddress;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gproxyaddress */ 

  top->s_type =  (s_type_func *) nsp_gproxyaddress_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gproxyaddress_type_short_string;
  /* top->create = (create_func*) int_gproxyaddress_create;*/

  /* specific methods for gproxyaddress */

  type->init = (init_func *) init_gproxyaddress;

  /* 
   * NspGProxyAddress interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gproxyaddress_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGProxyAddress called nsp_type_gproxyaddress
       */
      type->id =  nsp_type_gproxyaddress_id = nsp_new_type_id();
      nsp_type_gproxyaddress = type;
      if ( nsp_register_type(nsp_type_gproxyaddress) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gproxyaddress, G_TYPE_PROXY_ADDRESS);
      return ( mode == T_BASE ) ? type : new_type_gproxyaddress(mode);
    }
  else 
    {
      type->id = nsp_type_gproxyaddress_id;
      return type;
    }
}

/*
 * initialize NspGProxyAddress instances 
 * locally and by calling initializer on parent class 
 */

static int init_gproxyaddress(NspGProxyAddress *Obj,NspTypeGProxyAddress *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGProxyAddress 
 */

NspGProxyAddress *new_gproxyaddress() 
{
  NspGProxyAddress *loc;
  /* type must exists */
  nsp_type_gproxyaddress = new_type_gproxyaddress(T_BASE);
  if ( (loc = malloc(sizeof(NspGProxyAddress)))== NULLGPROXYADDRESS) return loc;
  /* initialize object */
  if ( init_gproxyaddress(loc,nsp_type_gproxyaddress) == FAIL) return NULLGPROXYADDRESS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGProxyAddress 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gproxyaddress_type_name[]="GProxyAddress";
static char gproxyaddress_short_type_name[]="GProxyAddress";

static char *nsp_gproxyaddress_type_as_string(void)
{
  return(gproxyaddress_type_name);
}

static char *nsp_gproxyaddress_type_short_string(NspObject *v)
{
  return(gproxyaddress_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGProxyAddress objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGProxyAddress   *nsp_gproxyaddress_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gproxyaddress_id)  == TRUE  ) return ((NspGProxyAddress *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gproxyaddress));
  return NULL;
}

int IsGProxyAddressObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gproxyaddress_id);
}

int IsGProxyAddress(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gproxyaddress_id);
}

NspGProxyAddress  *GetGProxyAddressCopy(Stack stack, int i)
{
  if (  GetGProxyAddress(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGProxyAddress  *GetGProxyAddress(Stack stack, int i)
{
  NspGProxyAddress *M;
  if (( M = nsp_gproxyaddress_object(NthObj(i))) == NULLGPROXYADDRESS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGProxyAddress *gproxyaddress_copy(NspGProxyAddress *self)
{
  /* return ginetsocketaddress_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gproxyaddress);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gproxyaddress);
}

/*-------------------------------------------------------------------
 * wrappers for the GProxyAddress
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_proxy_address_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,s_int,string,string,s_int,string,string, t_end};
  NspGObject *inetaddr;
  int port, dest_port;
  char *protocol, *dest_hostname, *username, *password;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_ginetaddress, &inetaddr, &port, &protocol, &dest_hostname, &dest_port, &username, &password) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_proxy_address_new(G_INET_ADDRESS(inetaddr->obj),port,protocol,dest_hostname,dest_port,username,password))== NULL) return RET_BUG;

  nsp_type_gproxyaddress = new_type_gproxyaddress(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gproxyaddress );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_proxy_address_get_protocol(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_protocol(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_proxy_address_get_destination_protocol(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_destination_protocol(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_proxy_address_get_destination_protocol(Stack stack, int rhs, int opt, int lhs) /* get_destination_protocol */
{
  Scierror("Error: function g_proxy_address_get_destination_protocol not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_proxy_address_get_destination_hostname(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_destination_hostname(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_proxy_address_get_destination_port(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_destination_port(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_proxy_address_get_username(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_username(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_proxy_address_get_password(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_password(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_proxy_address_get_uri(NspGProxyAddress *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_proxy_address_get_uri(G_PROXY_ADDRESS(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_proxy_address_get_uri(Stack stack, int rhs, int opt, int lhs) /* get_uri */
{
  Scierror("Error: function g_proxy_address_get_uri not available\n");
  return RET_BUG;
}
#endif
static NspMethods gproxyaddress_methods[] = {
  {"get_protocol",(nsp_method *) _wrap_g_proxy_address_get_protocol},
  {"get_destination_protocol",(nsp_method *) _wrap_g_proxy_address_get_destination_protocol},
  {"get_destination_hostname",(nsp_method *) _wrap_g_proxy_address_get_destination_hostname},
  {"get_destination_port",(nsp_method *) _wrap_g_proxy_address_get_destination_port},
  {"get_username",(nsp_method *) _wrap_g_proxy_address_get_username},
  {"get_password",(nsp_method *) _wrap_g_proxy_address_get_password},
  {"get_uri",(nsp_method *) _wrap_g_proxy_address_get_uri},
  { NULL, NULL}
};

static NspMethods *gproxyaddress_get_methods(void) { return gproxyaddress_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gproxyaddress_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketAddressEnumerator ----------- */


#define  NspGSocketAddressEnumerator_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketaddressenumerator.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketAddressEnumerator inherits from GObject 
 */

int nsp_type_gsocketaddressenumerator_id=0;
NspTypeGSocketAddressEnumerator *nsp_type_gsocketaddressenumerator=NULL;

/*
 * Type object for NspGSocketAddressEnumerator 
 * all the instance of NspTypeGSocketAddressEnumerator share the same id. 
 * nsp_type_gsocketaddressenumerator: is an instance of NspTypeGSocketAddressEnumerator 
 *    used for objects of NspGSocketAddressEnumerator type (i.e built with new_gsocketaddressenumerator) 
 * other instances are used for derived classes 
 */
NspTypeGSocketAddressEnumerator *new_type_gsocketaddressenumerator(type_mode mode)
{
  NspTypeGSocketAddressEnumerator *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketaddressenumerator != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketaddressenumerator;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketaddressenumerator_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketaddressenumerator_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketaddressenumerator;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketaddressenumerator */ 

  top->s_type =  (s_type_func *) nsp_gsocketaddressenumerator_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketaddressenumerator_type_short_string;
  /* top->create = (create_func*) int_gsocketaddressenumerator_create;*/

  /* specific methods for gsocketaddressenumerator */

  type->init = (init_func *) init_gsocketaddressenumerator;

  /* 
   * NspGSocketAddressEnumerator interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketaddressenumerator_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketAddressEnumerator called nsp_type_gsocketaddressenumerator
       */
      type->id =  nsp_type_gsocketaddressenumerator_id = nsp_new_type_id();
      nsp_type_gsocketaddressenumerator = type;
      if ( nsp_register_type(nsp_type_gsocketaddressenumerator) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketaddressenumerator, G_TYPE_SOCKET_ADDRESS_ENUMERATOR);
      return ( mode == T_BASE ) ? type : new_type_gsocketaddressenumerator(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketaddressenumerator_id;
      return type;
    }
}

/*
 * initialize NspGSocketAddressEnumerator instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketaddressenumerator(NspGSocketAddressEnumerator *Obj,NspTypeGSocketAddressEnumerator *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketAddressEnumerator 
 */

NspGSocketAddressEnumerator *new_gsocketaddressenumerator() 
{
  NspGSocketAddressEnumerator *loc;
  /* type must exists */
  nsp_type_gsocketaddressenumerator = new_type_gsocketaddressenumerator(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketAddressEnumerator)))== NULLGSOCKETADDRESSENUMERATOR) return loc;
  /* initialize object */
  if ( init_gsocketaddressenumerator(loc,nsp_type_gsocketaddressenumerator) == FAIL) return NULLGSOCKETADDRESSENUMERATOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketAddressEnumerator 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketaddressenumerator_type_name[]="GSocketAddressEnumerator";
static char gsocketaddressenumerator_short_type_name[]="GSocketAddressEnumerator";

static char *nsp_gsocketaddressenumerator_type_as_string(void)
{
  return(gsocketaddressenumerator_type_name);
}

static char *nsp_gsocketaddressenumerator_type_short_string(NspObject *v)
{
  return(gsocketaddressenumerator_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketAddressEnumerator objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketAddressEnumerator   *nsp_gsocketaddressenumerator_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketaddressenumerator_id)  == TRUE  ) return ((NspGSocketAddressEnumerator *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketaddressenumerator));
  return NULL;
}

int IsGSocketAddressEnumeratorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketaddressenumerator_id);
}

int IsGSocketAddressEnumerator(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketaddressenumerator_id);
}

NspGSocketAddressEnumerator  *GetGSocketAddressEnumeratorCopy(Stack stack, int i)
{
  if (  GetGSocketAddressEnumerator(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketAddressEnumerator  *GetGSocketAddressEnumerator(Stack stack, int i)
{
  NspGSocketAddressEnumerator *M;
  if (( M = nsp_gsocketaddressenumerator_object(NthObj(i))) == NULLGSOCKETADDRESSENUMERATOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketAddressEnumerator *gsocketaddressenumerator_copy(NspGSocketAddressEnumerator *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketaddressenumerator);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketaddressenumerator);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketAddressEnumerator
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_socket_address_enumerator_next(NspGSocketAddressEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  GSocketAddress *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_address_enumerator_next(G_SOCKET_ADDRESS_ENUMERATOR(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_address_enumerator_next_finish(NspGSocketAddressEnumerator *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GSocketAddress *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_socket_address_enumerator_next_finish(G_SOCKET_ADDRESS_ENUMERATOR(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gsocketaddressenumerator_methods[] = {
  {"next",(nsp_method *) _wrap_g_socket_address_enumerator_next},
  {"next_finish",(nsp_method *) _wrap_g_socket_address_enumerator_next_finish},
  { NULL, NULL}
};

static NspMethods *gsocketaddressenumerator_get_methods(void) { return gsocketaddressenumerator_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketaddressenumerator_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGProxyAddressEnumerator ----------- */


#define  NspGProxyAddressEnumerator_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gproxyaddressenumerator.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGProxyAddressEnumerator inherits from GSocketAddressEnumerator 
 */

int nsp_type_gproxyaddressenumerator_id=0;
NspTypeGProxyAddressEnumerator *nsp_type_gproxyaddressenumerator=NULL;

/*
 * Type object for NspGProxyAddressEnumerator 
 * all the instance of NspTypeGProxyAddressEnumerator share the same id. 
 * nsp_type_gproxyaddressenumerator: is an instance of NspTypeGProxyAddressEnumerator 
 *    used for objects of NspGProxyAddressEnumerator type (i.e built with new_gproxyaddressenumerator) 
 * other instances are used for derived classes 
 */
NspTypeGProxyAddressEnumerator *new_type_gproxyaddressenumerator(type_mode mode)
{
  NspTypeGProxyAddressEnumerator *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gproxyaddressenumerator != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gproxyaddressenumerator;
    }
  if (( type =  malloc(sizeof(NspTypeGSocketAddressEnumerator))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gsocketaddressenumerator(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gproxyaddressenumerator_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gproxyaddressenumerator_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gproxyaddressenumerator;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gproxyaddressenumerator */ 

  top->s_type =  (s_type_func *) nsp_gproxyaddressenumerator_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gproxyaddressenumerator_type_short_string;
  /* top->create = (create_func*) int_gproxyaddressenumerator_create;*/

  /* specific methods for gproxyaddressenumerator */

  type->init = (init_func *) init_gproxyaddressenumerator;

  /* 
   * NspGProxyAddressEnumerator interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gproxyaddressenumerator_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGProxyAddressEnumerator called nsp_type_gproxyaddressenumerator
       */
      type->id =  nsp_type_gproxyaddressenumerator_id = nsp_new_type_id();
      nsp_type_gproxyaddressenumerator = type;
      if ( nsp_register_type(nsp_type_gproxyaddressenumerator) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gproxyaddressenumerator, G_TYPE_PROXY_ADDRESS_ENUMERATOR);
      return ( mode == T_BASE ) ? type : new_type_gproxyaddressenumerator(mode);
    }
  else 
    {
      type->id = nsp_type_gproxyaddressenumerator_id;
      return type;
    }
}

/*
 * initialize NspGProxyAddressEnumerator instances 
 * locally and by calling initializer on parent class 
 */

static int init_gproxyaddressenumerator(NspGProxyAddressEnumerator *Obj,NspTypeGProxyAddressEnumerator *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGProxyAddressEnumerator 
 */

NspGProxyAddressEnumerator *new_gproxyaddressenumerator() 
{
  NspGProxyAddressEnumerator *loc;
  /* type must exists */
  nsp_type_gproxyaddressenumerator = new_type_gproxyaddressenumerator(T_BASE);
  if ( (loc = malloc(sizeof(NspGProxyAddressEnumerator)))== NULLGPROXYADDRESSENUMERATOR) return loc;
  /* initialize object */
  if ( init_gproxyaddressenumerator(loc,nsp_type_gproxyaddressenumerator) == FAIL) return NULLGPROXYADDRESSENUMERATOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGProxyAddressEnumerator 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gproxyaddressenumerator_type_name[]="GProxyAddressEnumerator";
static char gproxyaddressenumerator_short_type_name[]="GProxyAddressEnumerator";

static char *nsp_gproxyaddressenumerator_type_as_string(void)
{
  return(gproxyaddressenumerator_type_name);
}

static char *nsp_gproxyaddressenumerator_type_short_string(NspObject *v)
{
  return(gproxyaddressenumerator_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGProxyAddressEnumerator objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGProxyAddressEnumerator   *nsp_gproxyaddressenumerator_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gproxyaddressenumerator_id)  == TRUE  ) return ((NspGProxyAddressEnumerator *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gproxyaddressenumerator));
  return NULL;
}

int IsGProxyAddressEnumeratorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gproxyaddressenumerator_id);
}

int IsGProxyAddressEnumerator(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gproxyaddressenumerator_id);
}

NspGProxyAddressEnumerator  *GetGProxyAddressEnumeratorCopy(Stack stack, int i)
{
  if (  GetGProxyAddressEnumerator(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGProxyAddressEnumerator  *GetGProxyAddressEnumerator(Stack stack, int i)
{
  NspGProxyAddressEnumerator *M;
  if (( M = nsp_gproxyaddressenumerator_object(NthObj(i))) == NULLGPROXYADDRESSENUMERATOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGProxyAddressEnumerator *gproxyaddressenumerator_copy(NspGProxyAddressEnumerator *self)
{
  /* return gsocketaddressenumerator_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gproxyaddressenumerator);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gproxyaddressenumerator);
}

/*-------------------------------------------------------------------
 * wrappers for the GProxyAddressEnumerator
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gproxyaddressenumerator_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gproxyaddressenumerator_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketClient ----------- */


#define  NspGSocketClient_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketclient.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketClient inherits from GObject 
 */

int nsp_type_gsocketclient_id=0;
NspTypeGSocketClient *nsp_type_gsocketclient=NULL;

/*
 * Type object for NspGSocketClient 
 * all the instance of NspTypeGSocketClient share the same id. 
 * nsp_type_gsocketclient: is an instance of NspTypeGSocketClient 
 *    used for objects of NspGSocketClient type (i.e built with new_gsocketclient) 
 * other instances are used for derived classes 
 */
NspTypeGSocketClient *new_type_gsocketclient(type_mode mode)
{
  NspTypeGSocketClient *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketclient != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketclient;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketclient_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketclient_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketclient;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketclient */ 

  top->s_type =  (s_type_func *) nsp_gsocketclient_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketclient_type_short_string;
  /* top->create = (create_func*) int_gsocketclient_create;*/

  /* specific methods for gsocketclient */

  type->init = (init_func *) init_gsocketclient;

  /* 
   * NspGSocketClient interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketclient_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketClient called nsp_type_gsocketclient
       */
      type->id =  nsp_type_gsocketclient_id = nsp_new_type_id();
      nsp_type_gsocketclient = type;
      if ( nsp_register_type(nsp_type_gsocketclient) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketclient, G_TYPE_SOCKET_CLIENT);
      return ( mode == T_BASE ) ? type : new_type_gsocketclient(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketclient_id;
      return type;
    }
}

/*
 * initialize NspGSocketClient instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketclient(NspGSocketClient *Obj,NspTypeGSocketClient *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketClient 
 */

NspGSocketClient *new_gsocketclient() 
{
  NspGSocketClient *loc;
  /* type must exists */
  nsp_type_gsocketclient = new_type_gsocketclient(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketClient)))== NULLGSOCKETCLIENT) return loc;
  /* initialize object */
  if ( init_gsocketclient(loc,nsp_type_gsocketclient) == FAIL) return NULLGSOCKETCLIENT;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketClient 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketclient_type_name[]="GSocketClient";
static char gsocketclient_short_type_name[]="GSocketClient";

static char *nsp_gsocketclient_type_as_string(void)
{
  return(gsocketclient_type_name);
}

static char *nsp_gsocketclient_type_short_string(NspObject *v)
{
  return(gsocketclient_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketClient objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketClient   *nsp_gsocketclient_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketclient_id)  == TRUE  ) return ((NspGSocketClient *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketclient));
  return NULL;
}

int IsGSocketClientObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketclient_id);
}

int IsGSocketClient(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketclient_id);
}

NspGSocketClient  *GetGSocketClientCopy(Stack stack, int i)
{
  if (  GetGSocketClient(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketClient  *GetGSocketClient(Stack stack, int i)
{
  NspGSocketClient *M;
  if (( M = nsp_gsocketclient_object(NthObj(i))) == NULLGSOCKETCLIENT)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketClient *gsocketclient_copy(NspGSocketClient *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketclient);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketclient);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketClient
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_socket_client_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_socket_client_new())== NULL) return RET_BUG;

  nsp_type_gsocketclient = new_type_gsocketclient(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsocketclient );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_get_family(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_family(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_client_set_family(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GSocketFamily family;
  NspObject *nsp_family = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_family) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_FAMILY, nsp_family, &family)== FAIL)
      return RET_BUG;
    g_socket_client_set_family(G_SOCKET_CLIENT(self->obj),family);
  return 0;
}

static int _wrap_g_socket_client_get_socket_type(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_socket_type(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_client_set_socket_type(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GSocketType type;
  NspObject *nsp_type = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
    g_socket_client_set_socket_type(G_SOCKET_CLIENT(self->obj),type);
  return 0;
}

static int _wrap_g_socket_client_get_protocol(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_protocol(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_client_set_protocol(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GSocketProtocol protocol;
  NspObject *nsp_protocol = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_protocol) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_PROTOCOL, nsp_protocol, &protocol)== FAIL)
      return RET_BUG;
    g_socket_client_set_protocol(G_SOCKET_CLIENT(self->obj),protocol);
  return 0;
}

static int _wrap_g_socket_client_get_local_address(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  GSocketAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_local_address(G_SOCKET_CLIENT(self->obj));
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_set_local_address(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *address;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketaddress, &address) == FAIL) return RET_BUG;
    g_socket_client_set_local_address(G_SOCKET_CLIENT(self->obj),G_SOCKET_ADDRESS(address->obj));
  return 0;
}

static int _wrap_g_socket_client_get_timeout(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_timeout(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_client_set_timeout(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int timeout;
  if ( GetArgs(stack,rhs,opt,T,&timeout) == FAIL) return RET_BUG;
    g_socket_client_set_timeout(G_SOCKET_CLIENT(self->obj),timeout);
  return 0;
}

static int _wrap_g_socket_client_get_enable_proxy(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_enable_proxy(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_client_set_enable_proxy(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int enable;
  if ( GetArgs(stack,rhs,opt,T,&enable) == FAIL) return RET_BUG;
    g_socket_client_set_enable_proxy(G_SOCKET_CLIENT(self->obj),enable);
  return 0;
}

#if GTK_CHECK_VERSION(2,28,0)
static int _wrap_g_socket_client_get_tls(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_tls(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_client_get_tls(Stack stack, int rhs, int opt, int lhs) /* get_tls */
{
  Scierror("Error: function g_socket_client_get_tls not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,28,0)
static int _wrap_g_socket_client_set_tls(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int tls;
  if ( GetArgs(stack,rhs,opt,T,&tls) == FAIL) return RET_BUG;
    g_socket_client_set_tls(G_SOCKET_CLIENT(self->obj),tls);
  return 0;
}

#else
int _wrap_g_socket_client_set_tls(Stack stack, int rhs, int opt, int lhs) /* set_tls */
{
  Scierror("Error: function g_socket_client_set_tls not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,28,0)
static int _wrap_g_socket_client_get_tls_validation_flags(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =g_socket_client_get_tls_validation_flags(G_SOCKET_CLIENT(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_client_get_tls_validation_flags(Stack stack, int rhs, int opt, int lhs) /* get_tls_validation_flags */
{
  Scierror("Error: function g_socket_client_get_tls_validation_flags not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,28,0)
static int _wrap_g_socket_client_set_tls_validation_flags(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GTlsCertificateFlags flags;
  NspObject *nsp_flags = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_TLS_CERTIFICATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    g_socket_client_set_tls_validation_flags(G_SOCKET_CLIENT(self->obj),flags);
  return 0;
}

#else
int _wrap_g_socket_client_set_tls_validation_flags(Stack stack, int rhs, int opt, int lhs) /* set_tls_validation_flags */
{
  Scierror("Error: function g_socket_client_set_tls_validation_flags not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_socket_client_connect(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *connectable, *cancellable;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketconnectable, &connectable, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_client_connect(G_SOCKET_CLIENT(self->obj),G_SOCKET_CONNECTABLE(connectable->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_connect_to_host(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,obj_check, t_end};
  char *host_and_port;
  int default_port;
  NspGObject *cancellable;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&host_and_port, &default_port, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_to_host(G_SOCKET_CLIENT(self->obj),host_and_port,default_port,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_connect_to_service(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,string,obj_check, t_end};
  char *domain, *service;
  NspGObject *cancellable;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&domain, &service, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_to_service(G_SOCKET_CLIENT(self->obj),domain,service,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#if GTK_CHECK_VERSION(2,26,0)
static int _wrap_g_socket_client_connect_to_uri(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int,obj_check, t_end};
  char *uri;
  int default_port;
  NspGObject *cancellable;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri, &default_port, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_to_uri(G_SOCKET_CLIENT(self->obj),uri,default_port,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

#else
int _wrap_g_socket_client_connect_to_uri(Stack stack, int rhs, int opt, int lhs) /* connect_to_uri */
{
  Scierror("Error: function g_socket_client_connect_to_uri not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_socket_client_connect_finish(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_finish(G_SOCKET_CLIENT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_connect_to_host_finish(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_to_host_finish(G_SOCKET_CLIENT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_connect_to_service_finish(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_to_service_finish(G_SOCKET_CLIENT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_connect_to_uri_finish(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GSocketConnection *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_socket_client_connect_to_uri_finish(G_SOCKET_CLIENT(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnection))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_client_add_application_proxy(NspGSocketClient *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *protocol;
  if ( GetArgs(stack,rhs,opt,T,&protocol) == FAIL) return RET_BUG;
    g_socket_client_add_application_proxy(G_SOCKET_CLIENT(self->obj),protocol);
  return 0;
}

static NspMethods gsocketclient_methods[] = {
  {"get_family",(nsp_method *) _wrap_g_socket_client_get_family},
  {"set_family",(nsp_method *) _wrap_g_socket_client_set_family},
  {"get_socket_type",(nsp_method *) _wrap_g_socket_client_get_socket_type},
  {"set_socket_type",(nsp_method *) _wrap_g_socket_client_set_socket_type},
  {"get_protocol",(nsp_method *) _wrap_g_socket_client_get_protocol},
  {"set_protocol",(nsp_method *) _wrap_g_socket_client_set_protocol},
  {"get_local_address",(nsp_method *) _wrap_g_socket_client_get_local_address},
  {"set_local_address",(nsp_method *) _wrap_g_socket_client_set_local_address},
  {"get_timeout",(nsp_method *) _wrap_g_socket_client_get_timeout},
  {"set_timeout",(nsp_method *) _wrap_g_socket_client_set_timeout},
  {"get_enable_proxy",(nsp_method *) _wrap_g_socket_client_get_enable_proxy},
  {"set_enable_proxy",(nsp_method *) _wrap_g_socket_client_set_enable_proxy},
  {"get_tls",(nsp_method *) _wrap_g_socket_client_get_tls},
  {"set_tls",(nsp_method *) _wrap_g_socket_client_set_tls},
  {"get_tls_validation_flags",(nsp_method *) _wrap_g_socket_client_get_tls_validation_flags},
  {"set_tls_validation_flags",(nsp_method *) _wrap_g_socket_client_set_tls_validation_flags},
  {"connect",(nsp_method *) _wrap_g_socket_client_connect},
  {"connect_to_host",(nsp_method *) _wrap_g_socket_client_connect_to_host},
  {"connect_to_service",(nsp_method *) _wrap_g_socket_client_connect_to_service},
  {"connect_to_uri",(nsp_method *) _wrap_g_socket_client_connect_to_uri},
  {"connect_finish",(nsp_method *) _wrap_g_socket_client_connect_finish},
  {"connect_to_host_finish",(nsp_method *) _wrap_g_socket_client_connect_to_host_finish},
  {"connect_to_service_finish",(nsp_method *) _wrap_g_socket_client_connect_to_service_finish},
  {"connect_to_uri_finish",(nsp_method *) _wrap_g_socket_client_connect_to_uri_finish},
  {"add_application_proxy",(nsp_method *) _wrap_g_socket_client_add_application_proxy},
  { NULL, NULL}
};

static NspMethods *gsocketclient_get_methods(void) { return gsocketclient_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketclient_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketConnectable ----------- */


#define  NspGSocketConnectable_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketconnectable.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketConnectable inherits from GObject 
 */

int nsp_type_gsocketconnectable_id=0;
NspTypeGSocketConnectable *nsp_type_gsocketconnectable=NULL;

/*
 * Type object for NspGSocketConnectable 
 * all the instance of NspTypeGSocketConnectable share the same id. 
 * nsp_type_gsocketconnectable: is an instance of NspTypeGSocketConnectable 
 *    used for objects of NspGSocketConnectable type (i.e built with new_gsocketconnectable) 
 * other instances are used for derived classes 
 */
NspTypeGSocketConnectable *new_type_gsocketconnectable(type_mode mode)
{
  NspTypeGSocketConnectable *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketconnectable != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketconnectable;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketconnectable_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketconnectable_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketconnectable;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketconnectable */ 

  top->s_type =  (s_type_func *) nsp_gsocketconnectable_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketconnectable_type_short_string;
  /* top->create = (create_func*) int_gsocketconnectable_create;*/

  /* specific methods for gsocketconnectable */

  type->init = (init_func *) init_gsocketconnectable;

  /* 
   * NspGSocketConnectable interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketconnectable_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketConnectable called nsp_type_gsocketconnectable
       */
      type->id =  nsp_type_gsocketconnectable_id = nsp_new_type_id();
      nsp_type_gsocketconnectable = type;
      if ( nsp_register_type(nsp_type_gsocketconnectable) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketconnectable, G_TYPE_SOCKET_CONNECTABLE);
      return ( mode == T_BASE ) ? type : new_type_gsocketconnectable(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketconnectable_id;
      return type;
    }
}

/*
 * initialize NspGSocketConnectable instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketconnectable(NspGSocketConnectable *Obj,NspTypeGSocketConnectable *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketConnectable 
 */

NspGSocketConnectable *new_gsocketconnectable() 
{
  NspGSocketConnectable *loc;
  /* type must exists */
  nsp_type_gsocketconnectable = new_type_gsocketconnectable(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketConnectable)))== NULLGSOCKETCONNECTABLE) return loc;
  /* initialize object */
  if ( init_gsocketconnectable(loc,nsp_type_gsocketconnectable) == FAIL) return NULLGSOCKETCONNECTABLE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketConnectable 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketconnectable_type_name[]="GSocketConnectable";
static char gsocketconnectable_short_type_name[]="GSocketConnectable";

static char *nsp_gsocketconnectable_type_as_string(void)
{
  return(gsocketconnectable_type_name);
}

static char *nsp_gsocketconnectable_type_short_string(NspObject *v)
{
  return(gsocketconnectable_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketConnectable objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketConnectable   *nsp_gsocketconnectable_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketconnectable_id)  == TRUE  ) return ((NspGSocketConnectable *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketconnectable));
  return NULL;
}

int IsGSocketConnectableObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketconnectable_id);
}

int IsGSocketConnectable(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketconnectable_id);
}

NspGSocketConnectable  *GetGSocketConnectableCopy(Stack stack, int i)
{
  if (  GetGSocketConnectable(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketConnectable  *GetGSocketConnectable(Stack stack, int i)
{
  NspGSocketConnectable *M;
  if (( M = nsp_gsocketconnectable_object(NthObj(i))) == NULLGSOCKETCONNECTABLE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketConnectable *gsocketconnectable_copy(NspGSocketConnectable *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketconnectable);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketconnectable);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketConnectable
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_socket_connectable_enumerate(NspGSocketConnectable *self,Stack stack,int rhs,int opt,int lhs)
{
  GSocketAddressEnumerator *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_connectable_enumerate(G_SOCKET_CONNECTABLE(self->obj));
  nsp_type_gsocketaddressenumerator = new_type_gsocketaddressenumerator(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddressenumerator))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_connectable_proxy_enumerate(NspGSocketConnectable *self,Stack stack,int rhs,int opt,int lhs)
{
  GSocketAddressEnumerator *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_connectable_proxy_enumerate(G_SOCKET_CONNECTABLE(self->obj));
  nsp_type_gsocketaddressenumerator = new_type_gsocketaddressenumerator(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddressenumerator))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gsocketconnectable_methods[] = {
  {"enumerate",(nsp_method *) _wrap_g_socket_connectable_enumerate},
  {"proxy_enumerate",(nsp_method *) _wrap_g_socket_connectable_proxy_enumerate},
  { NULL, NULL}
};

static NspMethods *gsocketconnectable_get_methods(void) { return gsocketconnectable_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketconnectable_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketConnection ----------- */


#define  NspGSocketConnection_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketconnection.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketConnection inherits from GIOStream 
 */

int nsp_type_gsocketconnection_id=0;
NspTypeGSocketConnection *nsp_type_gsocketconnection=NULL;

/*
 * Type object for NspGSocketConnection 
 * all the instance of NspTypeGSocketConnection share the same id. 
 * nsp_type_gsocketconnection: is an instance of NspTypeGSocketConnection 
 *    used for objects of NspGSocketConnection type (i.e built with new_gsocketconnection) 
 * other instances are used for derived classes 
 */
NspTypeGSocketConnection *new_type_gsocketconnection(type_mode mode)
{
  NspTypeGSocketConnection *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketconnection != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketconnection;
    }
  if (( type =  malloc(sizeof(NspTypeGIOStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_giostream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketconnection_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketconnection_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketconnection;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketconnection */ 

  top->s_type =  (s_type_func *) nsp_gsocketconnection_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketconnection_type_short_string;
  /* top->create = (create_func*) int_gsocketconnection_create;*/

  /* specific methods for gsocketconnection */

  type->init = (init_func *) init_gsocketconnection;

  /* 
   * NspGSocketConnection interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketconnection_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketConnection called nsp_type_gsocketconnection
       */
      type->id =  nsp_type_gsocketconnection_id = nsp_new_type_id();
      nsp_type_gsocketconnection = type;
      if ( nsp_register_type(nsp_type_gsocketconnection) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketconnection, G_TYPE_SOCKET_CONNECTION);
      return ( mode == T_BASE ) ? type : new_type_gsocketconnection(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketconnection_id;
      return type;
    }
}

/*
 * initialize NspGSocketConnection instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketconnection(NspGSocketConnection *Obj,NspTypeGSocketConnection *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketConnection 
 */

NspGSocketConnection *new_gsocketconnection() 
{
  NspGSocketConnection *loc;
  /* type must exists */
  nsp_type_gsocketconnection = new_type_gsocketconnection(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketConnection)))== NULLGSOCKETCONNECTION) return loc;
  /* initialize object */
  if ( init_gsocketconnection(loc,nsp_type_gsocketconnection) == FAIL) return NULLGSOCKETCONNECTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketConnection 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketconnection_type_name[]="GSocketConnection";
static char gsocketconnection_short_type_name[]="GSocketConnection";

static char *nsp_gsocketconnection_type_as_string(void)
{
  return(gsocketconnection_type_name);
}

static char *nsp_gsocketconnection_type_short_string(NspObject *v)
{
  return(gsocketconnection_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketConnection objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketConnection   *nsp_gsocketconnection_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketconnection_id)  == TRUE  ) return ((NspGSocketConnection *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketconnection));
  return NULL;
}

int IsGSocketConnectionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketconnection_id);
}

int IsGSocketConnection(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketconnection_id);
}

NspGSocketConnection  *GetGSocketConnectionCopy(Stack stack, int i)
{
  if (  GetGSocketConnection(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketConnection  *GetGSocketConnection(Stack stack, int i)
{
  NspGSocketConnection *M;
  if (( M = nsp_gsocketconnection_object(NthObj(i))) == NULLGSOCKETCONNECTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketConnection *gsocketconnection_copy(NspGSocketConnection *self)
{
  /* return giostream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketconnection);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketconnection);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketConnection
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_connection_is_connected(NspGSocketConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_connection_is_connected(G_SOCKET_CONNECTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_connection_is_connected(Stack stack, int rhs, int opt, int lhs) /* is_connected */
{
  Scierror("Error: function g_socket_connection_is_connected not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_connection_connect(NspGSocketConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *address, *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketaddress, &address, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_socket_connection_connect(G_SOCKET_CONNECTION(self->obj),G_SOCKET_ADDRESS(address->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_connection_connect(Stack stack, int rhs, int opt, int lhs) /* connect */
{
  Scierror("Error: function g_socket_connection_connect not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_socket_connection_connect_finish(NspGSocketConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_socket_connection_connect_finish(G_SOCKET_CONNECTION(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_socket_connection_connect_finish(Stack stack, int rhs, int opt, int lhs) /* connect_finish */
{
  Scierror("Error: function g_socket_connection_connect_finish not available\n");
  return RET_BUG;
}
#endif
static int _wrap_g_socket_connection_get_socket(NspGSocketConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GSocket *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_connection_get_socket(G_SOCKET_CONNECTION(self->obj));
  nsp_type_gsocket = new_type_gsocket(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocket))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_connection_get_local_address(NspGSocketConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  GSocketAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_connection_get_local_address(G_SOCKET_CONNECTION(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_connection_get_remote_address(NspGSocketConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GError *error = NULL;
  GSocketAddress *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_socket_connection_get_remote_address(G_SOCKET_CONNECTION(self->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketaddress = new_type_gsocketaddress(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketaddress))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gsocketconnection_methods[] = {
  {"is_connected",(nsp_method *) _wrap_g_socket_connection_is_connected},
  {"connect",(nsp_method *) _wrap_g_socket_connection_connect},
  {"connect_finish",(nsp_method *) _wrap_g_socket_connection_connect_finish},
  {"get_socket",(nsp_method *) _wrap_g_socket_connection_get_socket},
  {"get_local_address",(nsp_method *) _wrap_g_socket_connection_get_local_address},
  {"get_remote_address",(nsp_method *) _wrap_g_socket_connection_get_remote_address},
  { NULL, NULL}
};

static NspMethods *gsocketconnection_get_methods(void) { return gsocketconnection_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketconnection_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketControlMessage ----------- */


#define  NspGSocketControlMessage_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketcontrolmessage.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketControlMessage inherits from GObject 
 */

int nsp_type_gsocketcontrolmessage_id=0;
NspTypeGSocketControlMessage *nsp_type_gsocketcontrolmessage=NULL;

/*
 * Type object for NspGSocketControlMessage 
 * all the instance of NspTypeGSocketControlMessage share the same id. 
 * nsp_type_gsocketcontrolmessage: is an instance of NspTypeGSocketControlMessage 
 *    used for objects of NspGSocketControlMessage type (i.e built with new_gsocketcontrolmessage) 
 * other instances are used for derived classes 
 */
NspTypeGSocketControlMessage *new_type_gsocketcontrolmessage(type_mode mode)
{
  NspTypeGSocketControlMessage *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketcontrolmessage != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketcontrolmessage;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketcontrolmessage_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketcontrolmessage_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketcontrolmessage;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketcontrolmessage */ 

  top->s_type =  (s_type_func *) nsp_gsocketcontrolmessage_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketcontrolmessage_type_short_string;
  /* top->create = (create_func*) int_gsocketcontrolmessage_create;*/

  /* specific methods for gsocketcontrolmessage */

  type->init = (init_func *) init_gsocketcontrolmessage;

  /* 
   * NspGSocketControlMessage interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketcontrolmessage_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketControlMessage called nsp_type_gsocketcontrolmessage
       */
      type->id =  nsp_type_gsocketcontrolmessage_id = nsp_new_type_id();
      nsp_type_gsocketcontrolmessage = type;
      if ( nsp_register_type(nsp_type_gsocketcontrolmessage) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketcontrolmessage, G_TYPE_SOCKET_CONTROL_MESSAGE);
      return ( mode == T_BASE ) ? type : new_type_gsocketcontrolmessage(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketcontrolmessage_id;
      return type;
    }
}

/*
 * initialize NspGSocketControlMessage instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketcontrolmessage(NspGSocketControlMessage *Obj,NspTypeGSocketControlMessage *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketControlMessage 
 */

NspGSocketControlMessage *new_gsocketcontrolmessage() 
{
  NspGSocketControlMessage *loc;
  /* type must exists */
  nsp_type_gsocketcontrolmessage = new_type_gsocketcontrolmessage(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketControlMessage)))== NULLGSOCKETCONTROLMESSAGE) return loc;
  /* initialize object */
  if ( init_gsocketcontrolmessage(loc,nsp_type_gsocketcontrolmessage) == FAIL) return NULLGSOCKETCONTROLMESSAGE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketControlMessage 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketcontrolmessage_type_name[]="GSocketControlMessage";
static char gsocketcontrolmessage_short_type_name[]="GSocketControlMessage";

static char *nsp_gsocketcontrolmessage_type_as_string(void)
{
  return(gsocketcontrolmessage_type_name);
}

static char *nsp_gsocketcontrolmessage_type_short_string(NspObject *v)
{
  return(gsocketcontrolmessage_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketControlMessage objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketControlMessage   *nsp_gsocketcontrolmessage_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketcontrolmessage_id)  == TRUE  ) return ((NspGSocketControlMessage *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketcontrolmessage));
  return NULL;
}

int IsGSocketControlMessageObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketcontrolmessage_id);
}

int IsGSocketControlMessage(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketcontrolmessage_id);
}

NspGSocketControlMessage  *GetGSocketControlMessageCopy(Stack stack, int i)
{
  if (  GetGSocketControlMessage(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketControlMessage  *GetGSocketControlMessage(Stack stack, int i)
{
  NspGSocketControlMessage *M;
  if (( M = nsp_gsocketcontrolmessage_object(NthObj(i))) == NULLGSOCKETCONTROLMESSAGE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketControlMessage *gsocketcontrolmessage_copy(NspGSocketControlMessage *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketcontrolmessage);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketcontrolmessage);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketControlMessage
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_socket_control_message_get_size(NspGSocketControlMessage *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_control_message_get_size(G_SOCKET_CONTROL_MESSAGE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_control_message_get_level(NspGSocketControlMessage *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_control_message_get_level(G_SOCKET_CONTROL_MESSAGE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_control_message_get_msg_type(NspGSocketControlMessage *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_control_message_get_msg_type(G_SOCKET_CONTROL_MESSAGE(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gsocketcontrolmessage_methods[] = {
  {"get_size",(nsp_method *) _wrap_g_socket_control_message_get_size},
  {"get_level",(nsp_method *) _wrap_g_socket_control_message_get_level},
  {"get_msg_type",(nsp_method *) _wrap_g_socket_control_message_get_msg_type},
  { NULL, NULL}
};

static NspMethods *gsocketcontrolmessage_get_methods(void) { return gsocketcontrolmessage_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketcontrolmessage_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketListener ----------- */


#define  NspGSocketListener_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketlistener.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketListener inherits from GObject 
 */

int nsp_type_gsocketlistener_id=0;
NspTypeGSocketListener *nsp_type_gsocketlistener=NULL;

/*
 * Type object for NspGSocketListener 
 * all the instance of NspTypeGSocketListener share the same id. 
 * nsp_type_gsocketlistener: is an instance of NspTypeGSocketListener 
 *    used for objects of NspGSocketListener type (i.e built with new_gsocketlistener) 
 * other instances are used for derived classes 
 */
NspTypeGSocketListener *new_type_gsocketlistener(type_mode mode)
{
  NspTypeGSocketListener *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketlistener != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketlistener;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketlistener_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketlistener_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketlistener;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketlistener */ 

  top->s_type =  (s_type_func *) nsp_gsocketlistener_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketlistener_type_short_string;
  /* top->create = (create_func*) int_gsocketlistener_create;*/

  /* specific methods for gsocketlistener */

  type->init = (init_func *) init_gsocketlistener;

  /* 
   * NspGSocketListener interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketlistener_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketListener called nsp_type_gsocketlistener
       */
      type->id =  nsp_type_gsocketlistener_id = nsp_new_type_id();
      nsp_type_gsocketlistener = type;
      if ( nsp_register_type(nsp_type_gsocketlistener) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketlistener, G_TYPE_SOCKET_LISTENER);
      return ( mode == T_BASE ) ? type : new_type_gsocketlistener(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketlistener_id;
      return type;
    }
}

/*
 * initialize NspGSocketListener instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketlistener(NspGSocketListener *Obj,NspTypeGSocketListener *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketListener 
 */

NspGSocketListener *new_gsocketlistener() 
{
  NspGSocketListener *loc;
  /* type must exists */
  nsp_type_gsocketlistener = new_type_gsocketlistener(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketListener)))== NULLGSOCKETLISTENER) return loc;
  /* initialize object */
  if ( init_gsocketlistener(loc,nsp_type_gsocketlistener) == FAIL) return NULLGSOCKETLISTENER;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketListener 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketlistener_type_name[]="GSocketListener";
static char gsocketlistener_short_type_name[]="GSocketListener";

static char *nsp_gsocketlistener_type_as_string(void)
{
  return(gsocketlistener_type_name);
}

static char *nsp_gsocketlistener_type_short_string(NspObject *v)
{
  return(gsocketlistener_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketListener objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketListener   *nsp_gsocketlistener_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketlistener_id)  == TRUE  ) return ((NspGSocketListener *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketlistener));
  return NULL;
}

int IsGSocketListenerObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketlistener_id);
}

int IsGSocketListener(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketlistener_id);
}

NspGSocketListener  *GetGSocketListenerCopy(Stack stack, int i)
{
  if (  GetGSocketListener(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketListener  *GetGSocketListener(Stack stack, int i)
{
  NspGSocketListener *M;
  if (( M = nsp_gsocketlistener_object(NthObj(i))) == NULLGSOCKETLISTENER)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketListener *gsocketlistener_copy(NspGSocketListener *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketlistener);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketlistener);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketListener
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_socket_listener_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_socket_listener_new())== NULL) return RET_BUG;

  nsp_type_gsocketlistener = new_type_gsocketlistener(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsocketlistener );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_listener_set_backlog(NspGSocketListener *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int, t_end};
  int listen_backlog;
  if ( GetArgs(stack,rhs,opt,T,&listen_backlog) == FAIL) return RET_BUG;
    g_socket_listener_set_backlog(G_SOCKET_LISTENER(self->obj),listen_backlog);
  return 0;
}

static int _wrap_g_socket_listener_add_socket(NspGSocketListener *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *socket, *source_object = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocket, &socket, &nsp_type_gobject, &source_object) == FAIL) return RET_BUG;
    ret =g_socket_listener_add_socket(G_SOCKET_LISTENER(self->obj),G_SOCKET(socket->obj),G_OBJECT(source_object->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_listener_add_inet_port(NspGSocketListener *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_int,obj_check, t_end};
  int port, ret;
  NspGObject *source_object = NULL;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&port, &nsp_type_gobject, &source_object) == FAIL) return RET_BUG;
    ret =g_socket_listener_add_inet_port(G_SOCKET_LISTENER(self->obj),port,G_OBJECT(source_object->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_listener_add_any_inet_port(NspGSocketListener *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *source_object = NULL;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gobject, &source_object) == FAIL) return RET_BUG;
    ret =g_socket_listener_add_any_inet_port(G_SOCKET_LISTENER(self->obj),G_OBJECT(source_object->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_socket_listener_close(NspGSocketListener *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_socket_listener_close(G_SOCKET_LISTENER(self->obj));
  return 0;
}

static NspMethods gsocketlistener_methods[] = {
  {"set_backlog",(nsp_method *) _wrap_g_socket_listener_set_backlog},
  {"add_socket",(nsp_method *) _wrap_g_socket_listener_add_socket},
  {"add_inet_port",(nsp_method *) _wrap_g_socket_listener_add_inet_port},
  {"add_any_inet_port",(nsp_method *) _wrap_g_socket_listener_add_any_inet_port},
  {"close",(nsp_method *) _wrap_g_socket_listener_close},
  { NULL, NULL}
};

static NspMethods *gsocketlistener_get_methods(void) { return gsocketlistener_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketlistener_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGSocketService ----------- */


#define  NspGSocketService_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gsocketservice.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGSocketService inherits from GSocketListener 
 */

int nsp_type_gsocketservice_id=0;
NspTypeGSocketService *nsp_type_gsocketservice=NULL;

/*
 * Type object for NspGSocketService 
 * all the instance of NspTypeGSocketService share the same id. 
 * nsp_type_gsocketservice: is an instance of NspTypeGSocketService 
 *    used for objects of NspGSocketService type (i.e built with new_gsocketservice) 
 * other instances are used for derived classes 
 */
NspTypeGSocketService *new_type_gsocketservice(type_mode mode)
{
  NspTypeGSocketService *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gsocketservice != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gsocketservice;
    }
  if (( type =  malloc(sizeof(NspTypeGSocketListener))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gsocketlistener(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gsocketservice_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gsocketservice_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gsocketservice;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gsocketservice */ 

  top->s_type =  (s_type_func *) nsp_gsocketservice_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gsocketservice_type_short_string;
  /* top->create = (create_func*) int_gsocketservice_create;*/

  /* specific methods for gsocketservice */

  type->init = (init_func *) init_gsocketservice;

  /* 
   * NspGSocketService interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gsocketservice_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGSocketService called nsp_type_gsocketservice
       */
      type->id =  nsp_type_gsocketservice_id = nsp_new_type_id();
      nsp_type_gsocketservice = type;
      if ( nsp_register_type(nsp_type_gsocketservice) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gsocketservice, G_TYPE_SOCKET_SERVICE);
      return ( mode == T_BASE ) ? type : new_type_gsocketservice(mode);
    }
  else 
    {
      type->id = nsp_type_gsocketservice_id;
      return type;
    }
}

/*
 * initialize NspGSocketService instances 
 * locally and by calling initializer on parent class 
 */

static int init_gsocketservice(NspGSocketService *Obj,NspTypeGSocketService *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGSocketService 
 */

NspGSocketService *new_gsocketservice() 
{
  NspGSocketService *loc;
  /* type must exists */
  nsp_type_gsocketservice = new_type_gsocketservice(T_BASE);
  if ( (loc = malloc(sizeof(NspGSocketService)))== NULLGSOCKETSERVICE) return loc;
  /* initialize object */
  if ( init_gsocketservice(loc,nsp_type_gsocketservice) == FAIL) return NULLGSOCKETSERVICE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGSocketService 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gsocketservice_type_name[]="GSocketService";
static char gsocketservice_short_type_name[]="GSocketService";

static char *nsp_gsocketservice_type_as_string(void)
{
  return(gsocketservice_type_name);
}

static char *nsp_gsocketservice_type_short_string(NspObject *v)
{
  return(gsocketservice_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGSocketService objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGSocketService   *nsp_gsocketservice_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gsocketservice_id)  == TRUE  ) return ((NspGSocketService *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gsocketservice));
  return NULL;
}

int IsGSocketServiceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gsocketservice_id);
}

int IsGSocketService(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gsocketservice_id);
}

NspGSocketService  *GetGSocketServiceCopy(Stack stack, int i)
{
  if (  GetGSocketService(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGSocketService  *GetGSocketService(Stack stack, int i)
{
  NspGSocketService *M;
  if (( M = nsp_gsocketservice_object(NthObj(i))) == NULLGSOCKETSERVICE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGSocketService *gsocketservice_copy(NspGSocketService *self)
{
  /* return gsocketlistener_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketservice);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gsocketservice);
}

/*-------------------------------------------------------------------
 * wrappers for the GSocketService
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_socket_service_new (Stack stack, int rhs, int opt, int lhs)
{
  GObject *ret; NspObject *nsp_ret;
  CheckRhs(0,0);
  if ((ret = (GObject *)g_socket_service_new())== NULL) return RET_BUG;

  nsp_type_gsocketservice = new_type_gsocketservice(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gsocketservice );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_socket_service_start(NspGSocketService *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_socket_service_start(G_SOCKET_SERVICE(self->obj));
  return 0;
}

static int _wrap_g_socket_service_stop(NspGSocketService *self,Stack stack,int rhs,int opt,int lhs)
{
  CheckRhs(0,0);
    g_socket_service_stop(G_SOCKET_SERVICE(self->obj));
  return 0;
}

static int _wrap_g_socket_service_is_active(NspGSocketService *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_socket_service_is_active(G_SOCKET_SERVICE(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gsocketservice_methods[] = {
  {"start",(nsp_method *) _wrap_g_socket_service_start},
  {"stop",(nsp_method *) _wrap_g_socket_service_stop},
  {"is_active",(nsp_method *) _wrap_g_socket_service_is_active},
  { NULL, NULL}
};

static NspMethods *gsocketservice_get_methods(void) { return gsocketservice_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gsocketservice_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTcpConnection ----------- */


#define  NspGTcpConnection_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtcpconnection.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTcpConnection inherits from GSocketConnection 
 */

int nsp_type_gtcpconnection_id=0;
NspTypeGTcpConnection *nsp_type_gtcpconnection=NULL;

/*
 * Type object for NspGTcpConnection 
 * all the instance of NspTypeGTcpConnection share the same id. 
 * nsp_type_gtcpconnection: is an instance of NspTypeGTcpConnection 
 *    used for objects of NspGTcpConnection type (i.e built with new_gtcpconnection) 
 * other instances are used for derived classes 
 */
NspTypeGTcpConnection *new_type_gtcpconnection(type_mode mode)
{
  NspTypeGTcpConnection *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtcpconnection != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtcpconnection;
    }
  if (( type =  malloc(sizeof(NspTypeGSocketConnection))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gsocketconnection(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtcpconnection_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtcpconnection_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtcpconnection;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtcpconnection */ 

  top->s_type =  (s_type_func *) nsp_gtcpconnection_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtcpconnection_type_short_string;
  /* top->create = (create_func*) int_gtcpconnection_create;*/

  /* specific methods for gtcpconnection */

  type->init = (init_func *) init_gtcpconnection;

  /* 
   * NspGTcpConnection interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtcpconnection_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTcpConnection called nsp_type_gtcpconnection
       */
      type->id =  nsp_type_gtcpconnection_id = nsp_new_type_id();
      nsp_type_gtcpconnection = type;
      if ( nsp_register_type(nsp_type_gtcpconnection) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtcpconnection, G_TYPE_TCP_CONNECTION);
      return ( mode == T_BASE ) ? type : new_type_gtcpconnection(mode);
    }
  else 
    {
      type->id = nsp_type_gtcpconnection_id;
      return type;
    }
}

/*
 * initialize NspGTcpConnection instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtcpconnection(NspGTcpConnection *Obj,NspTypeGTcpConnection *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTcpConnection 
 */

NspGTcpConnection *new_gtcpconnection() 
{
  NspGTcpConnection *loc;
  /* type must exists */
  nsp_type_gtcpconnection = new_type_gtcpconnection(T_BASE);
  if ( (loc = malloc(sizeof(NspGTcpConnection)))== NULLGTCPCONNECTION) return loc;
  /* initialize object */
  if ( init_gtcpconnection(loc,nsp_type_gtcpconnection) == FAIL) return NULLGTCPCONNECTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTcpConnection 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtcpconnection_type_name[]="GTcpConnection";
static char gtcpconnection_short_type_name[]="GTcpConnection";

static char *nsp_gtcpconnection_type_as_string(void)
{
  return(gtcpconnection_type_name);
}

static char *nsp_gtcpconnection_type_short_string(NspObject *v)
{
  return(gtcpconnection_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTcpConnection objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTcpConnection   *nsp_gtcpconnection_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtcpconnection_id)  == TRUE  ) return ((NspGTcpConnection *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtcpconnection));
  return NULL;
}

int IsGTcpConnectionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtcpconnection_id);
}

int IsGTcpConnection(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtcpconnection_id);
}

NspGTcpConnection  *GetGTcpConnectionCopy(Stack stack, int i)
{
  if (  GetGTcpConnection(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTcpConnection  *GetGTcpConnection(Stack stack, int i)
{
  NspGTcpConnection *M;
  if (( M = nsp_gtcpconnection_object(NthObj(i))) == NULLGTCPCONNECTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTcpConnection *gtcpconnection_copy(NspGTcpConnection *self)
{
  /* return gsocketconnection_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtcpconnection);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtcpconnection);
}

/*-------------------------------------------------------------------
 * wrappers for the GTcpConnection
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_tcp_connection_set_graceful_disconnect(NspGTcpConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int graceful_disconnect;
  if ( GetArgs(stack,rhs,opt,T,&graceful_disconnect) == FAIL) return RET_BUG;
    g_tcp_connection_set_graceful_disconnect(G_TCP_CONNECTION(self->obj),graceful_disconnect);
  return 0;
}

static int _wrap_g_tcp_connection_get_graceful_disconnect(NspGTcpConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_tcp_connection_get_graceful_disconnect(G_TCP_CONNECTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtcpconnection_methods[] = {
  {"set_graceful_disconnect",(nsp_method *) _wrap_g_tcp_connection_set_graceful_disconnect},
  {"get_graceful_disconnect",(nsp_method *) _wrap_g_tcp_connection_get_graceful_disconnect},
  { NULL, NULL}
};

static NspMethods *gtcpconnection_get_methods(void) { return gtcpconnection_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtcpconnection_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTcpWrapperConnection ----------- */


#define  NspGTcpWrapperConnection_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtcpwrapperconnection.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTcpWrapperConnection inherits from GTcpConnection 
 */

int nsp_type_gtcpwrapperconnection_id=0;
NspTypeGTcpWrapperConnection *nsp_type_gtcpwrapperconnection=NULL;

/*
 * Type object for NspGTcpWrapperConnection 
 * all the instance of NspTypeGTcpWrapperConnection share the same id. 
 * nsp_type_gtcpwrapperconnection: is an instance of NspTypeGTcpWrapperConnection 
 *    used for objects of NspGTcpWrapperConnection type (i.e built with new_gtcpwrapperconnection) 
 * other instances are used for derived classes 
 */
NspTypeGTcpWrapperConnection *new_type_gtcpwrapperconnection(type_mode mode)
{
  NspTypeGTcpWrapperConnection *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtcpwrapperconnection != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtcpwrapperconnection;
    }
  if (( type =  malloc(sizeof(NspTypeGTcpConnection))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gtcpconnection(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtcpwrapperconnection_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtcpwrapperconnection_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtcpwrapperconnection;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtcpwrapperconnection */ 

  top->s_type =  (s_type_func *) nsp_gtcpwrapperconnection_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtcpwrapperconnection_type_short_string;
  /* top->create = (create_func*) int_gtcpwrapperconnection_create;*/

  /* specific methods for gtcpwrapperconnection */

  type->init = (init_func *) init_gtcpwrapperconnection;

  /* 
   * NspGTcpWrapperConnection interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtcpwrapperconnection_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTcpWrapperConnection called nsp_type_gtcpwrapperconnection
       */
      type->id =  nsp_type_gtcpwrapperconnection_id = nsp_new_type_id();
      nsp_type_gtcpwrapperconnection = type;
      if ( nsp_register_type(nsp_type_gtcpwrapperconnection) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtcpwrapperconnection, G_TYPE_TCP_WRAPPER_CONNECTION);
      return ( mode == T_BASE ) ? type : new_type_gtcpwrapperconnection(mode);
    }
  else 
    {
      type->id = nsp_type_gtcpwrapperconnection_id;
      return type;
    }
}

/*
 * initialize NspGTcpWrapperConnection instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtcpwrapperconnection(NspGTcpWrapperConnection *Obj,NspTypeGTcpWrapperConnection *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTcpWrapperConnection 
 */

NspGTcpWrapperConnection *new_gtcpwrapperconnection() 
{
  NspGTcpWrapperConnection *loc;
  /* type must exists */
  nsp_type_gtcpwrapperconnection = new_type_gtcpwrapperconnection(T_BASE);
  if ( (loc = malloc(sizeof(NspGTcpWrapperConnection)))== NULLGTCPWRAPPERCONNECTION) return loc;
  /* initialize object */
  if ( init_gtcpwrapperconnection(loc,nsp_type_gtcpwrapperconnection) == FAIL) return NULLGTCPWRAPPERCONNECTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTcpWrapperConnection 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtcpwrapperconnection_type_name[]="GTcpWrapperConnection";
static char gtcpwrapperconnection_short_type_name[]="GTcpWrapperConnection";

static char *nsp_gtcpwrapperconnection_type_as_string(void)
{
  return(gtcpwrapperconnection_type_name);
}

static char *nsp_gtcpwrapperconnection_type_short_string(NspObject *v)
{
  return(gtcpwrapperconnection_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTcpWrapperConnection objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTcpWrapperConnection   *nsp_gtcpwrapperconnection_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtcpwrapperconnection_id)  == TRUE  ) return ((NspGTcpWrapperConnection *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtcpwrapperconnection));
  return NULL;
}

int IsGTcpWrapperConnectionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtcpwrapperconnection_id);
}

int IsGTcpWrapperConnection(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtcpwrapperconnection_id);
}

NspGTcpWrapperConnection  *GetGTcpWrapperConnectionCopy(Stack stack, int i)
{
  if (  GetGTcpWrapperConnection(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTcpWrapperConnection  *GetGTcpWrapperConnection(Stack stack, int i)
{
  NspGTcpWrapperConnection *M;
  if (( M = nsp_gtcpwrapperconnection_object(NthObj(i))) == NULLGTCPWRAPPERCONNECTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTcpWrapperConnection *gtcpwrapperconnection_copy(NspGTcpWrapperConnection *self)
{
  /* return gtcpconnection_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtcpwrapperconnection);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtcpwrapperconnection);
}

/*-------------------------------------------------------------------
 * wrappers for the GTcpWrapperConnection
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_tcp_wrapper_connection_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *base_io_stream, *socket;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_giostream, &base_io_stream, &nsp_type_gsocket, &socket) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_tcp_wrapper_connection_new(G_IO_STREAM(base_io_stream->obj),G_SOCKET(socket->obj)))== NULL) return RET_BUG;

  nsp_type_gtcpwrapperconnection = new_type_gtcpwrapperconnection(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gtcpwrapperconnection );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tcp_wrapper_connection_get_base_io_stream(NspGTcpWrapperConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GIOStream *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_tcp_wrapper_connection_get_base_io_stream(G_TCP_WRAPPER_CONNECTION(self->obj));
  nsp_type_giostream = new_type_giostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_giostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gtcpwrapperconnection_methods[] = {
  {"get_base_io_stream",(nsp_method *) _wrap_g_tcp_wrapper_connection_get_base_io_stream},
  { NULL, NULL}
};

static NspMethods *gtcpwrapperconnection_get_methods(void) { return gtcpwrapperconnection_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtcpwrapperconnection_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGThreadedSocketService ----------- */


#define  NspGThreadedSocketService_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gthreadedsocketservice.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGThreadedSocketService inherits from GSocketService 
 */

int nsp_type_gthreadedsocketservice_id=0;
NspTypeGThreadedSocketService *nsp_type_gthreadedsocketservice=NULL;

/*
 * Type object for NspGThreadedSocketService 
 * all the instance of NspTypeGThreadedSocketService share the same id. 
 * nsp_type_gthreadedsocketservice: is an instance of NspTypeGThreadedSocketService 
 *    used for objects of NspGThreadedSocketService type (i.e built with new_gthreadedsocketservice) 
 * other instances are used for derived classes 
 */
NspTypeGThreadedSocketService *new_type_gthreadedsocketservice(type_mode mode)
{
  NspTypeGThreadedSocketService *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gthreadedsocketservice != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gthreadedsocketservice;
    }
  if (( type =  malloc(sizeof(NspTypeGSocketService))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gsocketservice(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gthreadedsocketservice_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gthreadedsocketservice_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gthreadedsocketservice;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gthreadedsocketservice */ 

  top->s_type =  (s_type_func *) nsp_gthreadedsocketservice_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gthreadedsocketservice_type_short_string;
  /* top->create = (create_func*) int_gthreadedsocketservice_create;*/

  /* specific methods for gthreadedsocketservice */

  type->init = (init_func *) init_gthreadedsocketservice;

  /* 
   * NspGThreadedSocketService interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gthreadedsocketservice_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGThreadedSocketService called nsp_type_gthreadedsocketservice
       */
      type->id =  nsp_type_gthreadedsocketservice_id = nsp_new_type_id();
      nsp_type_gthreadedsocketservice = type;
      if ( nsp_register_type(nsp_type_gthreadedsocketservice) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gthreadedsocketservice, G_TYPE_THREADED_SOCKET_SERVICE);
      return ( mode == T_BASE ) ? type : new_type_gthreadedsocketservice(mode);
    }
  else 
    {
      type->id = nsp_type_gthreadedsocketservice_id;
      return type;
    }
}

/*
 * initialize NspGThreadedSocketService instances 
 * locally and by calling initializer on parent class 
 */

static int init_gthreadedsocketservice(NspGThreadedSocketService *Obj,NspTypeGThreadedSocketService *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGThreadedSocketService 
 */

NspGThreadedSocketService *new_gthreadedsocketservice() 
{
  NspGThreadedSocketService *loc;
  /* type must exists */
  nsp_type_gthreadedsocketservice = new_type_gthreadedsocketservice(T_BASE);
  if ( (loc = malloc(sizeof(NspGThreadedSocketService)))== NULLGTHREADEDSOCKETSERVICE) return loc;
  /* initialize object */
  if ( init_gthreadedsocketservice(loc,nsp_type_gthreadedsocketservice) == FAIL) return NULLGTHREADEDSOCKETSERVICE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGThreadedSocketService 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gthreadedsocketservice_type_name[]="GThreadedSocketService";
static char gthreadedsocketservice_short_type_name[]="GThreadedSocketService";

static char *nsp_gthreadedsocketservice_type_as_string(void)
{
  return(gthreadedsocketservice_type_name);
}

static char *nsp_gthreadedsocketservice_type_short_string(NspObject *v)
{
  return(gthreadedsocketservice_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGThreadedSocketService objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGThreadedSocketService   *nsp_gthreadedsocketservice_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gthreadedsocketservice_id)  == TRUE  ) return ((NspGThreadedSocketService *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gthreadedsocketservice));
  return NULL;
}

int IsGThreadedSocketServiceObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gthreadedsocketservice_id);
}

int IsGThreadedSocketService(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gthreadedsocketservice_id);
}

NspGThreadedSocketService  *GetGThreadedSocketServiceCopy(Stack stack, int i)
{
  if (  GetGThreadedSocketService(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGThreadedSocketService  *GetGThreadedSocketService(Stack stack, int i)
{
  NspGThreadedSocketService *M;
  if (( M = nsp_gthreadedsocketservice_object(NthObj(i))) == NULLGTHREADEDSOCKETSERVICE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGThreadedSocketService *gthreadedsocketservice_copy(NspGThreadedSocketService *self)
{
  /* return gsocketservice_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gthreadedsocketservice);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gthreadedsocketservice);
}

/*-------------------------------------------------------------------
 * wrappers for the GThreadedSocketService
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_threaded_socket_service_new (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {s_int, t_end};
  int max_threads;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&max_threads) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_threaded_socket_service_new(max_threads))== NULL) return RET_BUG;

  nsp_type_gthreadedsocketservice = new_type_gthreadedsocketservice(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gthreadedsocketservice );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods *gthreadedsocketservice_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gthreadedsocketservice_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTlsCertificate ----------- */


#define  NspGTlsCertificate_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtlscertificate.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTlsCertificate inherits from GObject 
 */

int nsp_type_gtlscertificate_id=0;
NspTypeGTlsCertificate *nsp_type_gtlscertificate=NULL;

/*
 * Type object for NspGTlsCertificate 
 * all the instance of NspTypeGTlsCertificate share the same id. 
 * nsp_type_gtlscertificate: is an instance of NspTypeGTlsCertificate 
 *    used for objects of NspGTlsCertificate type (i.e built with new_gtlscertificate) 
 * other instances are used for derived classes 
 */
NspTypeGTlsCertificate *new_type_gtlscertificate(type_mode mode)
{
  NspTypeGTlsCertificate *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtlscertificate != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtlscertificate;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtlscertificate_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtlscertificate_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtlscertificate;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtlscertificate */ 

  top->s_type =  (s_type_func *) nsp_gtlscertificate_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtlscertificate_type_short_string;
  /* top->create = (create_func*) int_gtlscertificate_create;*/

  /* specific methods for gtlscertificate */

  type->init = (init_func *) init_gtlscertificate;

  /* 
   * NspGTlsCertificate interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtlscertificate_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTlsCertificate called nsp_type_gtlscertificate
       */
      type->id =  nsp_type_gtlscertificate_id = nsp_new_type_id();
      nsp_type_gtlscertificate = type;
      if ( nsp_register_type(nsp_type_gtlscertificate) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtlscertificate, G_TYPE_TLS_CERTIFICATE);
      return ( mode == T_BASE ) ? type : new_type_gtlscertificate(mode);
    }
  else 
    {
      type->id = nsp_type_gtlscertificate_id;
      return type;
    }
}

/*
 * initialize NspGTlsCertificate instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtlscertificate(NspGTlsCertificate *Obj,NspTypeGTlsCertificate *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTlsCertificate 
 */

NspGTlsCertificate *new_gtlscertificate() 
{
  NspGTlsCertificate *loc;
  /* type must exists */
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ( (loc = malloc(sizeof(NspGTlsCertificate)))== NULLGTLSCERTIFICATE) return loc;
  /* initialize object */
  if ( init_gtlscertificate(loc,nsp_type_gtlscertificate) == FAIL) return NULLGTLSCERTIFICATE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTlsCertificate 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtlscertificate_type_name[]="GTlsCertificate";
static char gtlscertificate_short_type_name[]="GTlsCertificate";

static char *nsp_gtlscertificate_type_as_string(void)
{
  return(gtlscertificate_type_name);
}

static char *nsp_gtlscertificate_type_short_string(NspObject *v)
{
  return(gtlscertificate_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTlsCertificate objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTlsCertificate   *nsp_gtlscertificate_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtlscertificate_id)  == TRUE  ) return ((NspGTlsCertificate *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtlscertificate));
  return NULL;
}

int IsGTlsCertificateObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtlscertificate_id);
}

int IsGTlsCertificate(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtlscertificate_id);
}

NspGTlsCertificate  *GetGTlsCertificateCopy(Stack stack, int i)
{
  if (  GetGTlsCertificate(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTlsCertificate  *GetGTlsCertificate(Stack stack, int i)
{
  NspGTlsCertificate *M;
  if (( M = nsp_gtlscertificate_object(NthObj(i))) == NULLGTLSCERTIFICATE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTlsCertificate *gtlscertificate_copy(NspGTlsCertificate *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlscertificate);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlscertificate);
}

/*-------------------------------------------------------------------
 * wrappers for the GTlsCertificate
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int
_wrap_g_tls_certificate_new_from_files (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,string, t_end};
  char *cert_file, *key_file;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&cert_file, &key_file) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_tls_certificate_new_from_files(cert_file,key_file,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gtlscertificate );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_tls_certificate_new_from_file (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string, t_end};
  char *file;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&file) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_tls_certificate_new_from_file(file,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gtlscertificate );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int
_wrap_g_tls_certificate_new_from_pem (Stack stack, int rhs, int opt, int lhs)
{
  int_types T[] = {string,s_int, t_end};
  char *data;
  int length;
  GError *error = NULL;
  GObject *ret; NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&data, &length) == FAIL) return RET_BUG;
  if ((ret = (GObject *)g_tls_certificate_new_from_pem(data,length,&error))== NULL) return RET_BUG;
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }

  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  nsp_ret = (NspObject *) gobject_create(NVOID,ret,(NspTypeBase *) nsp_type_gtlscertificate );
   if ( nsp_ret == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_certificate_get_issuer(NspGTlsCertificate *self,Stack stack,int rhs,int opt,int lhs)
{
  GTlsCertificate *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_tls_certificate_get_issuer(G_TLS_CERTIFICATE(self->obj));
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_certificate_verify(NspGTlsCertificate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *identity, *trusted_ca;
  guint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gsocketconnectable, &identity, &nsp_type_gtlscertificate, &trusted_ca) == FAIL) return RET_BUG;
    ret =g_tls_certificate_verify(G_TLS_CERTIFICATE(self->obj),G_SOCKET_CONNECTABLE(identity->obj),G_TLS_CERTIFICATE(trusted_ca->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,34,0)
static int _wrap_g_tls_certificate_is_same(NspGTlsCertificate *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cert_two;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlscertificate, &cert_two) == FAIL) return RET_BUG;
    ret =g_tls_certificate_is_same(G_TLS_CERTIFICATE(self->obj),G_TLS_CERTIFICATE(cert_two->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_tls_certificate_is_same(Stack stack, int rhs, int opt, int lhs) /* is_same */
{
  Scierror("Error: function g_tls_certificate_is_same not available\n");
  return RET_BUG;
}
#endif
static NspMethods gtlscertificate_methods[] = {
  {"get_issuer",(nsp_method *) _wrap_g_tls_certificate_get_issuer},
  {"verify",(nsp_method *) _wrap_g_tls_certificate_verify},
  {"is_same",(nsp_method *) _wrap_g_tls_certificate_is_same},
  { NULL, NULL}
};

static NspMethods *gtlscertificate_get_methods(void) { return gtlscertificate_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtlscertificate_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTlsConnection ----------- */


#define  NspGTlsConnection_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtlsconnection.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTlsConnection inherits from GIOStream 
 */

int nsp_type_gtlsconnection_id=0;
NspTypeGTlsConnection *nsp_type_gtlsconnection=NULL;

/*
 * Type object for NspGTlsConnection 
 * all the instance of NspTypeGTlsConnection share the same id. 
 * nsp_type_gtlsconnection: is an instance of NspTypeGTlsConnection 
 *    used for objects of NspGTlsConnection type (i.e built with new_gtlsconnection) 
 * other instances are used for derived classes 
 */
NspTypeGTlsConnection *new_type_gtlsconnection(type_mode mode)
{
  NspTypeGTlsConnection *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtlsconnection != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtlsconnection;
    }
  if (( type =  malloc(sizeof(NspTypeGIOStream))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_giostream(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtlsconnection_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtlsconnection_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtlsconnection;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtlsconnection */ 

  top->s_type =  (s_type_func *) nsp_gtlsconnection_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtlsconnection_type_short_string;
  /* top->create = (create_func*) int_gtlsconnection_create;*/

  /* specific methods for gtlsconnection */

  type->init = (init_func *) init_gtlsconnection;

  /* 
   * NspGTlsConnection interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtlsconnection_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTlsConnection called nsp_type_gtlsconnection
       */
      type->id =  nsp_type_gtlsconnection_id = nsp_new_type_id();
      nsp_type_gtlsconnection = type;
      if ( nsp_register_type(nsp_type_gtlsconnection) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtlsconnection, G_TYPE_TLS_CONNECTION);
      return ( mode == T_BASE ) ? type : new_type_gtlsconnection(mode);
    }
  else 
    {
      type->id = nsp_type_gtlsconnection_id;
      return type;
    }
}

/*
 * initialize NspGTlsConnection instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtlsconnection(NspGTlsConnection *Obj,NspTypeGTlsConnection *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTlsConnection 
 */

NspGTlsConnection *new_gtlsconnection() 
{
  NspGTlsConnection *loc;
  /* type must exists */
  nsp_type_gtlsconnection = new_type_gtlsconnection(T_BASE);
  if ( (loc = malloc(sizeof(NspGTlsConnection)))== NULLGTLSCONNECTION) return loc;
  /* initialize object */
  if ( init_gtlsconnection(loc,nsp_type_gtlsconnection) == FAIL) return NULLGTLSCONNECTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTlsConnection 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtlsconnection_type_name[]="GTlsConnection";
static char gtlsconnection_short_type_name[]="GTlsConnection";

static char *nsp_gtlsconnection_type_as_string(void)
{
  return(gtlsconnection_type_name);
}

static char *nsp_gtlsconnection_type_short_string(NspObject *v)
{
  return(gtlsconnection_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTlsConnection objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTlsConnection   *nsp_gtlsconnection_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtlsconnection_id)  == TRUE  ) return ((NspGTlsConnection *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtlsconnection));
  return NULL;
}

int IsGTlsConnectionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtlsconnection_id);
}

int IsGTlsConnection(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtlsconnection_id);
}

NspGTlsConnection  *GetGTlsConnectionCopy(Stack stack, int i)
{
  if (  GetGTlsConnection(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTlsConnection  *GetGTlsConnection(Stack stack, int i)
{
  NspGTlsConnection *M;
  if (( M = nsp_gtlsconnection_object(NthObj(i))) == NULLGTLSCONNECTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTlsConnection *gtlsconnection_copy(NspGTlsConnection *self)
{
  /* return giostream_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlsconnection);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlsconnection);
}

/*-------------------------------------------------------------------
 * wrappers for the GTlsConnection
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
#if GTK_CHECK_VERSION(GLIB,DEPRECATED,0)
int _wrap_g_tls_connection_set_use_system_certdb(Stack stack, int rhs, int opt, int lhs) /* set_use_system_certdb */
{
  Scierror("Error: function g_tls_connection_set_use_system_certdb is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_g_tls_connection_set_use_system_certdb(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int use_system_certdb;
  if ( GetArgs(stack,rhs,opt,T,&use_system_certdb) == FAIL) return RET_BUG;
    g_tls_connection_set_use_system_certdb(G_TLS_CONNECTION(self->obj),use_system_certdb);
  return 0;
}

#endif
#if GTK_CHECK_VERSION(GLIB,DEPRECATED,0)
int _wrap_g_tls_connection_get_use_system_certdb(Stack stack, int rhs, int opt, int lhs) /* get_use_system_certdb */
{
  Scierror("Error: function g_tls_connection_get_use_system_certdb is deprecated\n");
  return RET_BUG;
}
#else
static int _wrap_g_tls_connection_get_use_system_certdb(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_use_system_certdb(G_TLS_CONNECTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#endif
static int _wrap_g_tls_connection_set_database(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *database;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlsdatabase, &database) == FAIL) return RET_BUG;
    g_tls_connection_set_database(G_TLS_CONNECTION(self->obj),G_TLS_DATABASE(database->obj));
  return 0;
}

static int _wrap_g_tls_connection_get_database(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GTlsDatabase *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_database(G_TLS_CONNECTION(self->obj));
  nsp_type_gtlsdatabase = new_type_gtlsdatabase(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlsdatabase))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_connection_set_certificate(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *certificate;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlscertificate, &certificate) == FAIL) return RET_BUG;
    g_tls_connection_set_certificate(G_TLS_CONNECTION(self->obj),G_TLS_CERTIFICATE(certificate->obj));
  return 0;
}

static int _wrap_g_tls_connection_get_certificate(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GTlsCertificate *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_certificate(G_TLS_CONNECTION(self->obj));
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_connection_set_interaction(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *interaction;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlsinteraction, &interaction) == FAIL) return RET_BUG;
    g_tls_connection_set_interaction(G_TLS_CONNECTION(self->obj),G_TLS_INTERACTION(interaction->obj));
  return 0;
}

static int _wrap_g_tls_connection_get_interaction(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GTlsInteraction *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_interaction(G_TLS_CONNECTION(self->obj));
  nsp_type_gtlsinteraction = new_type_gtlsinteraction(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlsinteraction))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_connection_get_peer_certificate(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  GTlsCertificate *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_peer_certificate(G_TLS_CONNECTION(self->obj));
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_connection_get_peer_certificate_errors(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  guint ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_peer_certificate_errors(G_TLS_CONNECTION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_connection_set_require_close_notify(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {s_bool, t_end};
  int require_close_notify;
  if ( GetArgs(stack,rhs,opt,T,&require_close_notify) == FAIL) return RET_BUG;
    g_tls_connection_set_require_close_notify(G_TLS_CONNECTION(self->obj),require_close_notify);
  return 0;
}

static int _wrap_g_tls_connection_get_require_close_notify(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_require_close_notify(G_TLS_CONNECTION(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_connection_set_rehandshake_mode(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj, t_end};
  GTlsRehandshakeMode mode;
  NspObject *nsp_mode = NULL;
  if ( GetArgs(stack,rhs,opt,T,&nsp_mode) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_TLS_REHANDSHAKE_MODE, nsp_mode, &mode)== FAIL)
      return RET_BUG;
    g_tls_connection_set_rehandshake_mode(G_TLS_CONNECTION(self->obj),mode);
  return 0;
}

static int _wrap_g_tls_connection_get_rehandshake_mode(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  gint ret;
  CheckRhs(0,0);
    ret =g_tls_connection_get_rehandshake_mode(G_TLS_CONNECTION(self->obj));
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_connection_handshake(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *cancellable;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_tls_connection_handshake(G_TLS_CONNECTION(self->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_connection_handshake_finish(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_connection_handshake_finish(G_TLS_CONNECTION(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_connection_emit_accept_certificate(NspGTlsConnection *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *peer_cert;
  GTlsCertificateFlags errors;
  NspObject *nsp_errors = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlscertificate, &peer_cert, &nsp_errors) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_TLS_CERTIFICATE_FLAGS, nsp_errors, &errors)==FAIL)
      return RET_BUG;
    ret =g_tls_connection_emit_accept_certificate(G_TLS_CONNECTION(self->obj),G_TLS_CERTIFICATE(peer_cert->obj),errors);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static NspMethods gtlsconnection_methods[] = {
  {"set_use_system_certdb",(nsp_method *) _wrap_g_tls_connection_set_use_system_certdb},
  {"get_use_system_certdb",(nsp_method *) _wrap_g_tls_connection_get_use_system_certdb},
  {"set_database",(nsp_method *) _wrap_g_tls_connection_set_database},
  {"get_database",(nsp_method *) _wrap_g_tls_connection_get_database},
  {"set_certificate",(nsp_method *) _wrap_g_tls_connection_set_certificate},
  {"get_certificate",(nsp_method *) _wrap_g_tls_connection_get_certificate},
  {"set_interaction",(nsp_method *) _wrap_g_tls_connection_set_interaction},
  {"get_interaction",(nsp_method *) _wrap_g_tls_connection_get_interaction},
  {"get_peer_certificate",(nsp_method *) _wrap_g_tls_connection_get_peer_certificate},
  {"get_peer_certificate_errors",(nsp_method *) _wrap_g_tls_connection_get_peer_certificate_errors},
  {"set_require_close_notify",(nsp_method *) _wrap_g_tls_connection_set_require_close_notify},
  {"get_require_close_notify",(nsp_method *) _wrap_g_tls_connection_get_require_close_notify},
  {"set_rehandshake_mode",(nsp_method *) _wrap_g_tls_connection_set_rehandshake_mode},
  {"get_rehandshake_mode",(nsp_method *) _wrap_g_tls_connection_get_rehandshake_mode},
  {"handshake",(nsp_method *) _wrap_g_tls_connection_handshake},
  {"handshake_finish",(nsp_method *) _wrap_g_tls_connection_handshake_finish},
  {"emit_accept_certificate",(nsp_method *) _wrap_g_tls_connection_emit_accept_certificate},
  { NULL, NULL}
};

static NspMethods *gtlsconnection_get_methods(void) { return gtlsconnection_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtlsconnection_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTlsDatabase ----------- */


#define  NspGTlsDatabase_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtlsdatabase.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTlsDatabase inherits from GObject 
 */

int nsp_type_gtlsdatabase_id=0;
NspTypeGTlsDatabase *nsp_type_gtlsdatabase=NULL;

/*
 * Type object for NspGTlsDatabase 
 * all the instance of NspTypeGTlsDatabase share the same id. 
 * nsp_type_gtlsdatabase: is an instance of NspTypeGTlsDatabase 
 *    used for objects of NspGTlsDatabase type (i.e built with new_gtlsdatabase) 
 * other instances are used for derived classes 
 */
NspTypeGTlsDatabase *new_type_gtlsdatabase(type_mode mode)
{
  NspTypeGTlsDatabase *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtlsdatabase != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtlsdatabase;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtlsdatabase_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtlsdatabase_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtlsdatabase;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtlsdatabase */ 

  top->s_type =  (s_type_func *) nsp_gtlsdatabase_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtlsdatabase_type_short_string;
  /* top->create = (create_func*) int_gtlsdatabase_create;*/

  /* specific methods for gtlsdatabase */

  type->init = (init_func *) init_gtlsdatabase;

  /* 
   * NspGTlsDatabase interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtlsdatabase_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTlsDatabase called nsp_type_gtlsdatabase
       */
      type->id =  nsp_type_gtlsdatabase_id = nsp_new_type_id();
      nsp_type_gtlsdatabase = type;
      if ( nsp_register_type(nsp_type_gtlsdatabase) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtlsdatabase, G_TYPE_TLS_DATABASE);
      return ( mode == T_BASE ) ? type : new_type_gtlsdatabase(mode);
    }
  else 
    {
      type->id = nsp_type_gtlsdatabase_id;
      return type;
    }
}

/*
 * initialize NspGTlsDatabase instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtlsdatabase(NspGTlsDatabase *Obj,NspTypeGTlsDatabase *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTlsDatabase 
 */

NspGTlsDatabase *new_gtlsdatabase() 
{
  NspGTlsDatabase *loc;
  /* type must exists */
  nsp_type_gtlsdatabase = new_type_gtlsdatabase(T_BASE);
  if ( (loc = malloc(sizeof(NspGTlsDatabase)))== NULLGTLSDATABASE) return loc;
  /* initialize object */
  if ( init_gtlsdatabase(loc,nsp_type_gtlsdatabase) == FAIL) return NULLGTLSDATABASE;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTlsDatabase 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtlsdatabase_type_name[]="GTlsDatabase";
static char gtlsdatabase_short_type_name[]="GTlsDatabase";

static char *nsp_gtlsdatabase_type_as_string(void)
{
  return(gtlsdatabase_type_name);
}

static char *nsp_gtlsdatabase_type_short_string(NspObject *v)
{
  return(gtlsdatabase_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTlsDatabase objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTlsDatabase   *nsp_gtlsdatabase_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtlsdatabase_id)  == TRUE  ) return ((NspGTlsDatabase *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtlsdatabase));
  return NULL;
}

int IsGTlsDatabaseObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtlsdatabase_id);
}

int IsGTlsDatabase(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtlsdatabase_id);
}

NspGTlsDatabase  *GetGTlsDatabaseCopy(Stack stack, int i)
{
  if (  GetGTlsDatabase(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTlsDatabase  *GetGTlsDatabase(Stack stack, int i)
{
  NspGTlsDatabase *M;
  if (( M = nsp_gtlsdatabase_object(NthObj(i))) == NULLGTLSDATABASE)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTlsDatabase *gtlsdatabase_copy(NspGTlsDatabase *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlsdatabase);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlsdatabase);
}

/*-------------------------------------------------------------------
 * wrappers for the GTlsDatabase
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_tls_database_verify_chain_finish(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  guint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_database_verify_chain_finish(G_TLS_DATABASE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_database_create_certificate_handle(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *certificate;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlscertificate, &certificate) == FAIL) return RET_BUG;
    ret =g_tls_database_create_certificate_handle(G_TLS_DATABASE(self->obj),G_TLS_CERTIFICATE(certificate->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_tls_database_lookup_certificate_for_handle(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,obj_check,obj,obj_check, t_end};
  char *handle;
  NspGObject *interaction, *cancellable;
  GTlsDatabaseLookupFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  GError *error = NULL;
  GTlsCertificate *ret;
  if ( GetArgs(stack,rhs,opt,T,&handle, &nsp_type_gtlsinteraction, &interaction, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_TLS_DATABASE_LOOKUP_FLAGS, nsp_flags, &flags)== FAIL)
      return RET_BUG;
    ret =g_tls_database_lookup_certificate_for_handle(G_TLS_DATABASE(self->obj),handle,G_TLS_INTERACTION(interaction->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_database_lookup_certificate_for_handle_finish(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GTlsCertificate *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_database_lookup_certificate_for_handle_finish(G_TLS_DATABASE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_database_lookup_certificate_issuer(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check,obj,obj_check, t_end};
  NspGObject *certificate, *interaction, *cancellable;
  GTlsDatabaseLookupFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  GError *error = NULL;
  GTlsCertificate *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlscertificate, &certificate, &nsp_type_gtlsinteraction, &interaction, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_TLS_DATABASE_LOOKUP_FLAGS, nsp_flags, &flags)== FAIL)
      return RET_BUG;
    ret =g_tls_database_lookup_certificate_issuer(G_TLS_DATABASE(self->obj),G_TLS_CERTIFICATE(certificate->obj),G_TLS_INTERACTION(interaction->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_database_lookup_certificate_issuer_finish(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GTlsCertificate *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_database_lookup_certificate_issuer_finish(G_TLS_DATABASE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gtlscertificate = new_type_gtlscertificate(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gtlscertificate))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_tls_database_lookup_certificates_issued_by_finish(NspGTlsDatabase *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_database_lookup_certificates_issued_by_finish(G_TLS_DATABASE(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static NspMethods gtlsdatabase_methods[] = {
  {"verify_chain_finish",(nsp_method *) _wrap_g_tls_database_verify_chain_finish},
  {"create_certificate_handle",(nsp_method *) _wrap_g_tls_database_create_certificate_handle},
  {"lookup_certificate_for_handle",(nsp_method *) _wrap_g_tls_database_lookup_certificate_for_handle},
  {"lookup_certificate_for_handle_finish",(nsp_method *) _wrap_g_tls_database_lookup_certificate_for_handle_finish},
  {"lookup_certificate_issuer",(nsp_method *) _wrap_g_tls_database_lookup_certificate_issuer},
  {"lookup_certificate_issuer_finish",(nsp_method *) _wrap_g_tls_database_lookup_certificate_issuer_finish},
  {"lookup_certificates_issued_by_finish",(nsp_method *) _wrap_g_tls_database_lookup_certificates_issued_by_finish},
  { NULL, NULL}
};

static NspMethods *gtlsdatabase_get_methods(void) { return gtlsdatabase_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtlsdatabase_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTlsInteraction ----------- */


#define  NspGTlsInteraction_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtlsinteraction.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTlsInteraction inherits from GObject 
 */

int nsp_type_gtlsinteraction_id=0;
NspTypeGTlsInteraction *nsp_type_gtlsinteraction=NULL;

/*
 * Type object for NspGTlsInteraction 
 * all the instance of NspTypeGTlsInteraction share the same id. 
 * nsp_type_gtlsinteraction: is an instance of NspTypeGTlsInteraction 
 *    used for objects of NspGTlsInteraction type (i.e built with new_gtlsinteraction) 
 * other instances are used for derived classes 
 */
NspTypeGTlsInteraction *new_type_gtlsinteraction(type_mode mode)
{
  NspTypeGTlsInteraction *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtlsinteraction != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtlsinteraction;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtlsinteraction_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtlsinteraction_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtlsinteraction;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtlsinteraction */ 

  top->s_type =  (s_type_func *) nsp_gtlsinteraction_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtlsinteraction_type_short_string;
  /* top->create = (create_func*) int_gtlsinteraction_create;*/

  /* specific methods for gtlsinteraction */

  type->init = (init_func *) init_gtlsinteraction;

  /* 
   * NspGTlsInteraction interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtlsinteraction_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTlsInteraction called nsp_type_gtlsinteraction
       */
      type->id =  nsp_type_gtlsinteraction_id = nsp_new_type_id();
      nsp_type_gtlsinteraction = type;
      if ( nsp_register_type(nsp_type_gtlsinteraction) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtlsinteraction, G_TYPE_TLS_INTERACTION);
      return ( mode == T_BASE ) ? type : new_type_gtlsinteraction(mode);
    }
  else 
    {
      type->id = nsp_type_gtlsinteraction_id;
      return type;
    }
}

/*
 * initialize NspGTlsInteraction instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtlsinteraction(NspGTlsInteraction *Obj,NspTypeGTlsInteraction *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTlsInteraction 
 */

NspGTlsInteraction *new_gtlsinteraction() 
{
  NspGTlsInteraction *loc;
  /* type must exists */
  nsp_type_gtlsinteraction = new_type_gtlsinteraction(T_BASE);
  if ( (loc = malloc(sizeof(NspGTlsInteraction)))== NULLGTLSINTERACTION) return loc;
  /* initialize object */
  if ( init_gtlsinteraction(loc,nsp_type_gtlsinteraction) == FAIL) return NULLGTLSINTERACTION;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTlsInteraction 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtlsinteraction_type_name[]="GTlsInteraction";
static char gtlsinteraction_short_type_name[]="GTlsInteraction";

static char *nsp_gtlsinteraction_type_as_string(void)
{
  return(gtlsinteraction_type_name);
}

static char *nsp_gtlsinteraction_type_short_string(NspObject *v)
{
  return(gtlsinteraction_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTlsInteraction objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTlsInteraction   *nsp_gtlsinteraction_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtlsinteraction_id)  == TRUE  ) return ((NspGTlsInteraction *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtlsinteraction));
  return NULL;
}

int IsGTlsInteractionObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtlsinteraction_id);
}

int IsGTlsInteraction(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtlsinteraction_id);
}

NspGTlsInteraction  *GetGTlsInteractionCopy(Stack stack, int i)
{
  if (  GetGTlsInteraction(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTlsInteraction  *GetGTlsInteraction(Stack stack, int i)
{
  NspGTlsInteraction *M;
  if (( M = nsp_gtlsinteraction_object(NthObj(i))) == NULLGTLSINTERACTION)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTlsInteraction *gtlsinteraction_copy(NspGTlsInteraction *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlsinteraction);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlsinteraction);
}

/*-------------------------------------------------------------------
 * wrappers for the GTlsInteraction
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_tls_interaction_invoke_ask_password(NspGTlsInteraction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *password, *cancellable;
  GError *error = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlspassword, &password, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_tls_interaction_invoke_ask_password(G_TLS_INTERACTION(self->obj),G_TLS_PASSWORD(password->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_interaction_ask_password(NspGTlsInteraction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj_check, t_end};
  NspGObject *password, *cancellable;
  GError *error = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlspassword, &password, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
    ret =g_tls_interaction_ask_password(G_TLS_INTERACTION(self->obj),G_TLS_PASSWORD(password->obj),G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_interaction_ask_password_finish(NspGTlsInteraction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_interaction_ask_password_finish(G_TLS_INTERACTION(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,40,0)
static int _wrap_g_tls_interaction_invoke_request_certificate(NspGTlsInteraction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,obj_check, t_end};
  NspGObject *connection, *cancellable;
  GTlsCertificateRequestFlags flags;
  NspObject *nsp_flags = NULL;
  GError *error = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlsconnection, &connection, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_TLS_CERTIFICATE_REQUEST_FLAGS, nsp_flags, &flags)== FAIL)
      return RET_BUG;
    ret =g_tls_interaction_invoke_request_certificate(G_TLS_INTERACTION(self->obj),G_TLS_CONNECTION(connection->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_tls_interaction_invoke_request_certificate(Stack stack, int rhs, int opt, int lhs) /* invoke_request_certificate */
{
  Scierror("Error: function g_tls_interaction_invoke_request_certificate not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,40,0)
static int _wrap_g_tls_interaction_request_certificate(NspGTlsInteraction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check,obj,obj_check, t_end};
  NspGObject *connection, *cancellable;
  GTlsCertificateRequestFlags flags;
  NspObject *nsp_flags = NULL;
  GError *error = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gtlsconnection, &connection, &nsp_flags, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_TLS_CERTIFICATE_REQUEST_FLAGS, nsp_flags, &flags)== FAIL)
      return RET_BUG;
    ret =g_tls_interaction_request_certificate(G_TLS_INTERACTION(self->obj),G_TLS_CONNECTION(connection->obj),flags,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_tls_interaction_request_certificate(Stack stack, int rhs, int opt, int lhs) /* request_certificate */
{
  Scierror("Error: function g_tls_interaction_request_certificate not available\n");
  return RET_BUG;
}
#endif
#if GTK_CHECK_VERSION(2,40,0)
static int _wrap_g_tls_interaction_request_certificate_finish(NspGTlsInteraction *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_tls_interaction_request_certificate_finish(G_TLS_INTERACTION(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_tls_interaction_request_certificate_finish(Stack stack, int rhs, int opt, int lhs) /* request_certificate_finish */
{
  Scierror("Error: function g_tls_interaction_request_certificate_finish not available\n");
  return RET_BUG;
}
#endif
static NspMethods gtlsinteraction_methods[] = {
  {"invoke_ask_password",(nsp_method *) _wrap_g_tls_interaction_invoke_ask_password},
  {"ask_password",(nsp_method *) _wrap_g_tls_interaction_ask_password},
  {"ask_password_finish",(nsp_method *) _wrap_g_tls_interaction_ask_password_finish},
  {"invoke_request_certificate",(nsp_method *) _wrap_g_tls_interaction_invoke_request_certificate},
  {"request_certificate",(nsp_method *) _wrap_g_tls_interaction_request_certificate},
  {"request_certificate_finish",(nsp_method *) _wrap_g_tls_interaction_request_certificate_finish},
  { NULL, NULL}
};

static NspMethods *gtlsinteraction_get_methods(void) { return gtlsinteraction_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtlsinteraction_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGTlsPassword ----------- */


#define  NspGTlsPassword_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gtlspassword.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGTlsPassword inherits from GObject 
 */

int nsp_type_gtlspassword_id=0;
NspTypeGTlsPassword *nsp_type_gtlspassword=NULL;

/*
 * Type object for NspGTlsPassword 
 * all the instance of NspTypeGTlsPassword share the same id. 
 * nsp_type_gtlspassword: is an instance of NspTypeGTlsPassword 
 *    used for objects of NspGTlsPassword type (i.e built with new_gtlspassword) 
 * other instances are used for derived classes 
 */
NspTypeGTlsPassword *new_type_gtlspassword(type_mode mode)
{
  NspTypeGTlsPassword *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gtlspassword != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gtlspassword;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gtlspassword_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gtlspassword_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gtlspassword;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gtlspassword */ 

  top->s_type =  (s_type_func *) nsp_gtlspassword_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gtlspassword_type_short_string;
  /* top->create = (create_func*) int_gtlspassword_create;*/

  /* specific methods for gtlspassword */

  type->init = (init_func *) init_gtlspassword;

  /* 
   * NspGTlsPassword interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gtlspassword_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGTlsPassword called nsp_type_gtlspassword
       */
      type->id =  nsp_type_gtlspassword_id = nsp_new_type_id();
      nsp_type_gtlspassword = type;
      if ( nsp_register_type(nsp_type_gtlspassword) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gtlspassword, G_TYPE_TLS_PASSWORD);
      return ( mode == T_BASE ) ? type : new_type_gtlspassword(mode);
    }
  else 
    {
      type->id = nsp_type_gtlspassword_id;
      return type;
    }
}

/*
 * initialize NspGTlsPassword instances 
 * locally and by calling initializer on parent class 
 */

static int init_gtlspassword(NspGTlsPassword *Obj,NspTypeGTlsPassword *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGTlsPassword 
 */

NspGTlsPassword *new_gtlspassword() 
{
  NspGTlsPassword *loc;
  /* type must exists */
  nsp_type_gtlspassword = new_type_gtlspassword(T_BASE);
  if ( (loc = malloc(sizeof(NspGTlsPassword)))== NULLGTLSPASSWORD) return loc;
  /* initialize object */
  if ( init_gtlspassword(loc,nsp_type_gtlspassword) == FAIL) return NULLGTLSPASSWORD;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGTlsPassword 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gtlspassword_type_name[]="GTlsPassword";
static char gtlspassword_short_type_name[]="GTlsPassword";

static char *nsp_gtlspassword_type_as_string(void)
{
  return(gtlspassword_type_name);
}

static char *nsp_gtlspassword_type_short_string(NspObject *v)
{
  return(gtlspassword_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGTlsPassword objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGTlsPassword   *nsp_gtlspassword_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gtlspassword_id)  == TRUE  ) return ((NspGTlsPassword *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gtlspassword));
  return NULL;
}

int IsGTlsPasswordObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gtlspassword_id);
}

int IsGTlsPassword(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gtlspassword_id);
}

NspGTlsPassword  *GetGTlsPasswordCopy(Stack stack, int i)
{
  if (  GetGTlsPassword(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGTlsPassword  *GetGTlsPassword(Stack stack, int i)
{
  NspGTlsPassword *M;
  if (( M = nsp_gtlspassword_object(NthObj(i))) == NULLGTLSPASSWORD)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGTlsPassword *gtlspassword_copy(NspGTlsPassword *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlspassword);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gtlspassword);
}

/*-------------------------------------------------------------------
 * wrappers for the GTlsPassword
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_tls_password_set_value(NspGTlsPassword *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string,s_int, t_end};
  guchar *value;
  int length;
  if ( GetArgs(stack,rhs,opt,T,&value, &length) == FAIL) return RET_BUG;
    g_tls_password_set_value(G_TLS_PASSWORD(self->obj),value,length);
  return 0;
}

static int _wrap_g_tls_password_get_description(NspGTlsPassword *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_tls_password_get_description(G_TLS_PASSWORD(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_password_set_description(NspGTlsPassword *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *description;
  if ( GetArgs(stack,rhs,opt,T,&description) == FAIL) return RET_BUG;
    g_tls_password_set_description(G_TLS_PASSWORD(self->obj),description);
  return 0;
}

static int _wrap_g_tls_password_get_warning(NspGTlsPassword *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_tls_password_get_warning(G_TLS_PASSWORD(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_tls_password_set_warning(NspGTlsPassword *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *warning;
  if ( GetArgs(stack,rhs,opt,T,&warning) == FAIL) return RET_BUG;
    g_tls_password_set_warning(G_TLS_PASSWORD(self->obj),warning);
  return 0;
}

static NspMethods gtlspassword_methods[] = {
  {"set_value",(nsp_method *) _wrap_g_tls_password_set_value},
  {"get_description",(nsp_method *) _wrap_g_tls_password_get_description},
  {"set_description",(nsp_method *) _wrap_g_tls_password_set_description},
  {"get_warning",(nsp_method *) _wrap_g_tls_password_get_warning},
  {"set_warning",(nsp_method *) _wrap_g_tls_password_set_warning},
  { NULL, NULL}
};

static NspMethods *gtlspassword_get_methods(void) { return gtlspassword_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gtlspassword_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGVfs ----------- */


#define  NspGVfs_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gvfs.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGVfs inherits from GObject 
 */

int nsp_type_gvfs_id=0;
NspTypeGVfs *nsp_type_gvfs=NULL;

/*
 * Type object for NspGVfs 
 * all the instance of NspTypeGVfs share the same id. 
 * nsp_type_gvfs: is an instance of NspTypeGVfs 
 *    used for objects of NspGVfs type (i.e built with new_gvfs) 
 * other instances are used for derived classes 
 */
NspTypeGVfs *new_type_gvfs(type_mode mode)
{
  NspTypeGVfs *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gvfs != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gvfs;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gvfs_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gvfs_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gvfs;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gvfs */ 

  top->s_type =  (s_type_func *) nsp_gvfs_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gvfs_type_short_string;
  /* top->create = (create_func*) int_gvfs_create;*/

  /* specific methods for gvfs */

  type->init = (init_func *) init_gvfs;

  /* 
   * NspGVfs interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gvfs_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGVfs called nsp_type_gvfs
       */
      type->id =  nsp_type_gvfs_id = nsp_new_type_id();
      nsp_type_gvfs = type;
      if ( nsp_register_type(nsp_type_gvfs) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gvfs, G_TYPE_VFS);
      return ( mode == T_BASE ) ? type : new_type_gvfs(mode);
    }
  else 
    {
      type->id = nsp_type_gvfs_id;
      return type;
    }
}

/*
 * initialize NspGVfs instances 
 * locally and by calling initializer on parent class 
 */

static int init_gvfs(NspGVfs *Obj,NspTypeGVfs *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGVfs 
 */

NspGVfs *new_gvfs() 
{
  NspGVfs *loc;
  /* type must exists */
  nsp_type_gvfs = new_type_gvfs(T_BASE);
  if ( (loc = malloc(sizeof(NspGVfs)))== NULLGVFS) return loc;
  /* initialize object */
  if ( init_gvfs(loc,nsp_type_gvfs) == FAIL) return NULLGVFS;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGVfs 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gvfs_type_name[]="GVfs";
static char gvfs_short_type_name[]="GVfs";

static char *nsp_gvfs_type_as_string(void)
{
  return(gvfs_type_name);
}

static char *nsp_gvfs_type_short_string(NspObject *v)
{
  return(gvfs_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGVfs objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGVfs   *nsp_gvfs_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gvfs_id)  == TRUE  ) return ((NspGVfs *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gvfs));
  return NULL;
}

int IsGVfsObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gvfs_id);
}

int IsGVfs(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gvfs_id);
}

NspGVfs  *GetGVfsCopy(Stack stack, int i)
{
  if (  GetGVfs(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGVfs  *GetGVfs(Stack stack, int i)
{
  NspGVfs *M;
  if (( M = nsp_gvfs_object(NthObj(i))) == NULLGVFS)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGVfs *gvfs_copy(NspGVfs *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gvfs);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gvfs);
}

/*-------------------------------------------------------------------
 * wrappers for the GVfs
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_vfs_is_active(NspGVfs *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_vfs_is_active(G_VFS(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_vfs_get_file_for_path(NspGVfs *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *path;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&path) == FAIL) return RET_BUG;
    ret =g_vfs_get_file_for_path(G_VFS(self->obj),path);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_vfs_get_file_for_uri(NspGVfs *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *uri;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri) == FAIL) return RET_BUG;
    ret =g_vfs_get_file_for_uri(G_VFS(self->obj),uri);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_vfs_parse_name(NspGVfs *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *parse_name;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&parse_name) == FAIL) return RET_BUG;
    ret =g_vfs_parse_name(G_VFS(self->obj),parse_name);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gvfs_methods[] = {
  {"is_active",(nsp_method *) _wrap_g_vfs_is_active},
  {"get_file_for_path",(nsp_method *) _wrap_g_vfs_get_file_for_path},
  {"get_file_for_uri",(nsp_method *) _wrap_g_vfs_get_file_for_uri},
  {"parse_name",(nsp_method *) _wrap_g_vfs_parse_name},
  { NULL, NULL}
};

static NspMethods *gvfs_get_methods(void) { return gvfs_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gvfs_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGVolume ----------- */


#define  NspGVolume_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gvolume.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGVolume inherits from GObject 
 */

int nsp_type_gvolume_id=0;
NspTypeGVolume *nsp_type_gvolume=NULL;

/*
 * Type object for NspGVolume 
 * all the instance of NspTypeGVolume share the same id. 
 * nsp_type_gvolume: is an instance of NspTypeGVolume 
 *    used for objects of NspGVolume type (i.e built with new_gvolume) 
 * other instances are used for derived classes 
 */
NspTypeGVolume *new_type_gvolume(type_mode mode)
{
  NspTypeGVolume *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gvolume != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gvolume;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gvolume_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gvolume_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gvolume;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gvolume */ 

  top->s_type =  (s_type_func *) nsp_gvolume_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gvolume_type_short_string;
  /* top->create = (create_func*) int_gvolume_create;*/

  /* specific methods for gvolume */

  type->init = (init_func *) init_gvolume;

  /* 
   * NspGVolume interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gvolume_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGVolume called nsp_type_gvolume
       */
      type->id =  nsp_type_gvolume_id = nsp_new_type_id();
      nsp_type_gvolume = type;
      if ( nsp_register_type(nsp_type_gvolume) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gvolume, G_TYPE_VOLUME);
      return ( mode == T_BASE ) ? type : new_type_gvolume(mode);
    }
  else 
    {
      type->id = nsp_type_gvolume_id;
      return type;
    }
}

/*
 * initialize NspGVolume instances 
 * locally and by calling initializer on parent class 
 */

static int init_gvolume(NspGVolume *Obj,NspTypeGVolume *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGVolume 
 */

NspGVolume *new_gvolume() 
{
  NspGVolume *loc;
  /* type must exists */
  nsp_type_gvolume = new_type_gvolume(T_BASE);
  if ( (loc = malloc(sizeof(NspGVolume)))== NULLGVOLUME) return loc;
  /* initialize object */
  if ( init_gvolume(loc,nsp_type_gvolume) == FAIL) return NULLGVOLUME;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGVolume 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gvolume_type_name[]="GVolume";
static char gvolume_short_type_name[]="GVolume";

static char *nsp_gvolume_type_as_string(void)
{
  return(gvolume_type_name);
}

static char *nsp_gvolume_type_short_string(NspObject *v)
{
  return(gvolume_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGVolume objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGVolume   *nsp_gvolume_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gvolume_id)  == TRUE  ) return ((NspGVolume *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gvolume));
  return NULL;
}

int IsGVolumeObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gvolume_id);
}

int IsGVolume(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gvolume_id);
}

NspGVolume  *GetGVolumeCopy(Stack stack, int i)
{
  if (  GetGVolume(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGVolume  *GetGVolume(Stack stack, int i)
{
  NspGVolume *M;
  if (( M = nsp_gvolume_object(NthObj(i))) == NULLGVOLUME)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGVolume *gvolume_copy(NspGVolume *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gvolume);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gvolume);
}

/*-------------------------------------------------------------------
 * wrappers for the GVolume
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_volume_get_name(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_volume_get_name(G_VOLUME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_volume_get_icon(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_get_icon(G_VOLUME(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_get_symbolic_icon(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  GIcon *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_get_symbolic_icon(G_VOLUME(self->obj));
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_get_uuid(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_volume_get_uuid(G_VOLUME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_volume_get_drive(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  GDrive *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_get_drive(G_VOLUME(self->obj));
  nsp_type_gdrive = new_type_gdrive(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gdrive))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_get_mount(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  GMount *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_get_mount(G_VOLUME(self->obj));
  nsp_type_gmount = new_type_gmount(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmount))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_can_mount(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_volume_can_mount(G_VOLUME(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_volume_can_eject(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_volume_can_eject(G_VOLUME(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_volume_should_automount(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  int ret;
  CheckRhs(0,0);
    ret =g_volume_should_automount(G_VOLUME(self->obj));
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_volume_mount_finish(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_volume_mount_finish(G_VOLUME(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

static int _wrap_g_volume_get_identifier(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *kind;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&kind) == FAIL) return RET_BUG;
    ret =g_volume_get_identifier(G_VOLUME(self->obj),kind);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

static int _wrap_g_volume_enumerate_identifiers(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  gchar **ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_enumerate_identifiers(G_VOLUME(self->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_get_activation_root(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  GFile *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_get_activation_root(G_VOLUME(self->obj));
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_eject_with_operation_finish(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_volume_eject_with_operation_finish(G_VOLUME(self->obj),G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#if GTK_CHECK_VERSION(2,32,0)
static int _wrap_g_volume_get_sort_key(NspGVolume *self,Stack stack,int rhs,int opt,int lhs)
{
  const gchar *ret;
  CheckRhs(0,0);
    ret =g_volume_get_sort_key(G_VOLUME(self->obj));
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  return 1;
}

#else
int _wrap_g_volume_get_sort_key(Stack stack, int rhs, int opt, int lhs) /* get_sort_key */
{
  Scierror("Error: function g_volume_get_sort_key not available\n");
  return RET_BUG;
}
#endif
static NspMethods gvolume_methods[] = {
  {"get_name",(nsp_method *) _wrap_g_volume_get_name},
  {"get_icon",(nsp_method *) _wrap_g_volume_get_icon},
  {"get_symbolic_icon",(nsp_method *) _wrap_g_volume_get_symbolic_icon},
  {"get_uuid",(nsp_method *) _wrap_g_volume_get_uuid},
  {"get_drive",(nsp_method *) _wrap_g_volume_get_drive},
  {"get_mount",(nsp_method *) _wrap_g_volume_get_mount},
  {"can_mount",(nsp_method *) _wrap_g_volume_can_mount},
  {"can_eject",(nsp_method *) _wrap_g_volume_can_eject},
  {"should_automount",(nsp_method *) _wrap_g_volume_should_automount},
  {"mount_finish",(nsp_method *) _wrap_g_volume_mount_finish},
  {"get_identifier",(nsp_method *) _wrap_g_volume_get_identifier},
  {"enumerate_identifiers",(nsp_method *) _wrap_g_volume_enumerate_identifiers},
  {"get_activation_root",(nsp_method *) _wrap_g_volume_get_activation_root},
  {"eject_with_operation_finish",(nsp_method *) _wrap_g_volume_eject_with_operation_finish},
  {"get_sort_key",(nsp_method *) _wrap_g_volume_get_sort_key},
  { NULL, NULL}
};

static NspMethods *gvolume_get_methods(void) { return gvolume_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gvolume_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGVolumeMonitor ----------- */


#define  NspGVolumeMonitor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gvolumemonitor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGVolumeMonitor inherits from GObject 
 */

int nsp_type_gvolumemonitor_id=0;
NspTypeGVolumeMonitor *nsp_type_gvolumemonitor=NULL;

/*
 * Type object for NspGVolumeMonitor 
 * all the instance of NspTypeGVolumeMonitor share the same id. 
 * nsp_type_gvolumemonitor: is an instance of NspTypeGVolumeMonitor 
 *    used for objects of NspGVolumeMonitor type (i.e built with new_gvolumemonitor) 
 * other instances are used for derived classes 
 */
NspTypeGVolumeMonitor *new_type_gvolumemonitor(type_mode mode)
{
  NspTypeGVolumeMonitor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gvolumemonitor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gvolumemonitor;
    }
  if (( type =  malloc(sizeof(NspTypeGObject))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gobject(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gvolumemonitor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gvolumemonitor_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gvolumemonitor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gvolumemonitor */ 

  top->s_type =  (s_type_func *) nsp_gvolumemonitor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gvolumemonitor_type_short_string;
  /* top->create = (create_func*) int_gvolumemonitor_create;*/

  /* specific methods for gvolumemonitor */

  type->init = (init_func *) init_gvolumemonitor;

  /* 
   * NspGVolumeMonitor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gvolumemonitor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGVolumeMonitor called nsp_type_gvolumemonitor
       */
      type->id =  nsp_type_gvolumemonitor_id = nsp_new_type_id();
      nsp_type_gvolumemonitor = type;
      if ( nsp_register_type(nsp_type_gvolumemonitor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gvolumemonitor, G_TYPE_VOLUME_MONITOR);
      return ( mode == T_BASE ) ? type : new_type_gvolumemonitor(mode);
    }
  else 
    {
      type->id = nsp_type_gvolumemonitor_id;
      return type;
    }
}

/*
 * initialize NspGVolumeMonitor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gvolumemonitor(NspGVolumeMonitor *Obj,NspTypeGVolumeMonitor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGVolumeMonitor 
 */

NspGVolumeMonitor *new_gvolumemonitor() 
{
  NspGVolumeMonitor *loc;
  /* type must exists */
  nsp_type_gvolumemonitor = new_type_gvolumemonitor(T_BASE);
  if ( (loc = malloc(sizeof(NspGVolumeMonitor)))== NULLGVOLUMEMONITOR) return loc;
  /* initialize object */
  if ( init_gvolumemonitor(loc,nsp_type_gvolumemonitor) == FAIL) return NULLGVOLUMEMONITOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGVolumeMonitor 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gvolumemonitor_type_name[]="GVolumeMonitor";
static char gvolumemonitor_short_type_name[]="GVolumeMonitor";

static char *nsp_gvolumemonitor_type_as_string(void)
{
  return(gvolumemonitor_type_name);
}

static char *nsp_gvolumemonitor_type_short_string(NspObject *v)
{
  return(gvolumemonitor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGVolumeMonitor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGVolumeMonitor   *nsp_gvolumemonitor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gvolumemonitor_id)  == TRUE  ) return ((NspGVolumeMonitor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gvolumemonitor));
  return NULL;
}

int IsGVolumeMonitorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gvolumemonitor_id);
}

int IsGVolumeMonitor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gvolumemonitor_id);
}

NspGVolumeMonitor  *GetGVolumeMonitorCopy(Stack stack, int i)
{
  if (  GetGVolumeMonitor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGVolumeMonitor  *GetGVolumeMonitor(Stack stack, int i)
{
  NspGVolumeMonitor *M;
  if (( M = nsp_gvolumemonitor_object(NthObj(i))) == NULLGVOLUMEMONITOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGVolumeMonitor *gvolumemonitor_copy(NspGVolumeMonitor *self)
{
  /* return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gvolumemonitor);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gvolumemonitor);
}

/*-------------------------------------------------------------------
 * wrappers for the GVolumeMonitor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static int _wrap_g_volume_monitor_get_connected_drives(NspGVolumeMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_volume_monitor_get_connected_drives(G_VOLUME_MONITOR(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_volume_monitor_get_volumes(NspGVolumeMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_volume_monitor_get_volumes(G_VOLUME_MONITOR(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_volume_monitor_get_mounts(NspGVolumeMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_volume_monitor_get_mounts(G_VOLUME_MONITOR(self->obj));
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

static int _wrap_g_volume_monitor_get_volume_for_uuid(NspGVolumeMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *uuid;
  GVolume *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uuid) == FAIL) return RET_BUG;
    ret =g_volume_monitor_get_volume_for_uuid(G_VOLUME_MONITOR(self->obj),uuid);
  nsp_type_gvolume = new_type_gvolume(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gvolume))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static int _wrap_g_volume_monitor_get_mount_for_uuid(NspGVolumeMonitor *self,Stack stack,int rhs,int opt,int lhs)
{
  int_types T[] = {string, t_end};
  char *uuid;
  GMount *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uuid) == FAIL) return RET_BUG;
    ret =g_volume_monitor_get_mount_for_uuid(G_VOLUME_MONITOR(self->obj),uuid);
  nsp_type_gmount = new_type_gmount(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gmount))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

static NspMethods gvolumemonitor_methods[] = {
  {"get_connected_drives",(nsp_method *) _wrap_g_volume_monitor_get_connected_drives},
  {"get_volumes",(nsp_method *) _wrap_g_volume_monitor_get_volumes},
  {"get_mounts",(nsp_method *) _wrap_g_volume_monitor_get_mounts},
  {"get_volume_for_uuid",(nsp_method *) _wrap_g_volume_monitor_get_volume_for_uuid},
  {"get_mount_for_uuid",(nsp_method *) _wrap_g_volume_monitor_get_mount_for_uuid},
  { NULL, NULL}
};

static NspMethods *gvolumemonitor_get_methods(void) { return gvolumemonitor_methods;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gvolumemonitor_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;


/* -----------NspGNativeVolumeMonitor ----------- */


#define  NspGNativeVolumeMonitor_Private 
#include <nsp/objects.h>
#include <nsp/gtk/gnativevolumemonitor.h>
#include <nsp/interf.h>
#include <nsp/nspthreads.h>

/* 
 * NspGNativeVolumeMonitor inherits from GVolumeMonitor 
 */

int nsp_type_gnativevolumemonitor_id=0;
NspTypeGNativeVolumeMonitor *nsp_type_gnativevolumemonitor=NULL;

/*
 * Type object for NspGNativeVolumeMonitor 
 * all the instance of NspTypeGNativeVolumeMonitor share the same id. 
 * nsp_type_gnativevolumemonitor: is an instance of NspTypeGNativeVolumeMonitor 
 *    used for objects of NspGNativeVolumeMonitor type (i.e built with new_gnativevolumemonitor) 
 * other instances are used for derived classes 
 */
NspTypeGNativeVolumeMonitor *new_type_gnativevolumemonitor(type_mode mode)
{
  NspTypeGNativeVolumeMonitor *type= NULL;
  NspTypeObject *top;
  if (  nsp_type_gnativevolumemonitor != 0 && mode == T_BASE )
    {
      /* initialization performed and T_BASE requested */
      return nsp_type_gnativevolumemonitor;
    }
  if (( type =  malloc(sizeof(NspTypeGVolumeMonitor))) == NULL) return NULL;
  type->interface = NULL;
  type->surtype = (NspTypeBase *) new_type_gvolumemonitor(T_DERIVED);
  if ( type->surtype == NULL) return NULL;
  type->attrs = gnativevolumemonitor_attrs;
  type->get_attrs = (attrs_func *) int_get_attribute;
  type->set_attrs = (attrs_func *) int_set_attribute;
  type->methods = gnativevolumemonitor_get_methods;
  type->gtk_methods = TRUE;
  type->new = (new_func *) new_gnativevolumemonitor;


  top = NSP_TYPE_OBJECT(type->surtype);
  while ( top->surtype != NULL ) top= NSP_TYPE_OBJECT(top->surtype);

  /* object methods redefined for gnativevolumemonitor */ 

  top->s_type =  (s_type_func *) nsp_gnativevolumemonitor_type_as_string;
  top->sh_type = (sh_type_func *) nsp_gnativevolumemonitor_type_short_string;
  /* top->create = (create_func*) int_gnativevolumemonitor_create;*/

  /* specific methods for gnativevolumemonitor */

  type->init = (init_func *) init_gnativevolumemonitor;

  /* 
   * NspGNativeVolumeMonitor interfaces can be added here 
   * type->interface = (NspTypeBase *) new_type_b();
   * type->interface->interface = (NspTypeBase *) new_type_C()
   * ....
   */
  if ( nsp_type_gnativevolumemonitor_id == 0 ) 
    {
      /* 
       * the first time we get here we initialize the type id and
       * an instance of NspTypeGNativeVolumeMonitor called nsp_type_gnativevolumemonitor
       */
      type->id =  nsp_type_gnativevolumemonitor_id = nsp_new_type_id();
      nsp_type_gnativevolumemonitor = type;
      if ( nsp_register_type(nsp_type_gnativevolumemonitor) == FALSE) return NULL;
      /* add a ref to nsp_type in the gtype */
      register_nsp_type_in_gtype((NspTypeBase *)nsp_type_gnativevolumemonitor, G_TYPE_NATIVE_VOLUME_MONITOR);
      return ( mode == T_BASE ) ? type : new_type_gnativevolumemonitor(mode);
    }
  else 
    {
      type->id = nsp_type_gnativevolumemonitor_id;
      return type;
    }
}

/*
 * initialize NspGNativeVolumeMonitor instances 
 * locally and by calling initializer on parent class 
 */

static int init_gnativevolumemonitor(NspGNativeVolumeMonitor *Obj,NspTypeGNativeVolumeMonitor *type)
{
  /* initialize the surtype */ 
  if ( type->surtype->init(&Obj->father,type->surtype) == FAIL) return FAIL;
  Obj->type = type;
  NSP_OBJECT(Obj)->basetype = (NspTypeBase *)type;
  /* specific */
 return OK;
}

/*
 * new instance of NspGNativeVolumeMonitor 
 */

NspGNativeVolumeMonitor *new_gnativevolumemonitor() 
{
  NspGNativeVolumeMonitor *loc;
  /* type must exists */
  nsp_type_gnativevolumemonitor = new_type_gnativevolumemonitor(T_BASE);
  if ( (loc = malloc(sizeof(NspGNativeVolumeMonitor)))== NULLGNATIVEVOLUMEMONITOR) return loc;
  /* initialize object */
  if ( init_gnativevolumemonitor(loc,nsp_type_gnativevolumemonitor) == FAIL) return NULLGNATIVEVOLUMEMONITOR;
  return loc;
}

/*----------------------------------------------
 * Object method redefined for NspGNativeVolumeMonitor 
 *-----------------------------------------------*/
/*
 * type as string 
 */

static char gnativevolumemonitor_type_name[]="GNativeVolumeMonitor";
static char gnativevolumemonitor_short_type_name[]="GNativeVolumeMonitor";

static char *nsp_gnativevolumemonitor_type_as_string(void)
{
  return(gnativevolumemonitor_type_name);
}

static char *nsp_gnativevolumemonitor_type_short_string(NspObject *v)
{
  return(gnativevolumemonitor_short_type_name);
}

/*-----------------------------------------------------
 * a set of functions used when writing interfaces 
 * for NspGNativeVolumeMonitor objects 
 * Note that some of these functions could become MACROS
 *-----------------------------------------------------*/

NspGNativeVolumeMonitor   *nsp_gnativevolumemonitor_object(NspObject *O)
{
  /* Follow pointer */
  HOBJ_GET_OBJECT(O,NULL);
  /* Check type */
  if ( check_cast (O,nsp_type_gnativevolumemonitor_id)  == TRUE  ) return ((NspGNativeVolumeMonitor *) O);
  else 
    Scierror("Error:	Argument should be a %s\n",type_get_name(nsp_type_gnativevolumemonitor));
  return NULL;
}

int IsGNativeVolumeMonitorObj(Stack stack, int i)
{
  return nsp_object_type(NthObj(i),nsp_type_gnativevolumemonitor_id);
}

int IsGNativeVolumeMonitor(NspObject *O)
{
  return nsp_object_type(O,nsp_type_gnativevolumemonitor_id);
}

NspGNativeVolumeMonitor  *GetGNativeVolumeMonitorCopy(Stack stack, int i)
{
  if (  GetGNativeVolumeMonitor(stack,i) == NULL ) return NULL;
  return MaybeObjCopy(&NthObj(i));
}

NspGNativeVolumeMonitor  *GetGNativeVolumeMonitor(Stack stack, int i)
{
  NspGNativeVolumeMonitor *M;
  if (( M = nsp_gnativevolumemonitor_object(NthObj(i))) == NULLGNATIVEVOLUMEMONITOR)
     ArgMessage(stack,i);
  return M;
}

/*
 * copy for gobject derived class  
 */

NspGNativeVolumeMonitor *gnativevolumemonitor_copy(NspGNativeVolumeMonitor *self)
{
  /* return gvolumemonitor_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gnativevolumemonitor);*/
  return gobject_create(NVOID,((NspGObject *) self)->obj,(NspTypeBase *) nsp_type_gnativevolumemonitor);
}

/*-------------------------------------------------------------------
 * wrappers for the GNativeVolumeMonitor
 * i.e functions at Nsp level 
 *-------------------------------------------------------------------*/
/*-------------------------------------------
 * Methods
 *-------------------------------------------*/
static NspMethods *gnativevolumemonitor_get_methods(void) { return NULL;};
/*-------------------------------------------
 * Attributes
 *-------------------------------------------*/

static AttrTab gnativevolumemonitor_attrs[]={{NULL,NULL,NULL,NULL,NULL}} ;

/*-------------------------------------------
 * functions 
 *-------------------------------------------*/
int _wrap_g_action_name_is_valid(Stack stack, int rhs, int opt, int lhs) /* g_action_name_is_valid */
{
  int_types T[] = {string, t_end};
  char *action_name;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name) == FAIL) return RET_BUG;
    ret =g_action_name_is_valid(action_name);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_action_print_detailed_name(Stack stack, int rhs, int opt, int lhs) /* g_action_print_detailed_name */
{
  int_types T[] = {string,obj, t_end};
  char *action_name;
  GVariant *target_value = NULL;
  NspObject *nsp_target_value = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&action_name, &nsp_target_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_target_value))
    { target_value = ((NspGVariant *) nsp_target_value)->obj->value;
      if((target_value = nsp_copy_GVariant(target_value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: target_value should be of type GVariant\n");
      return RET_BUG;
    }
    ret =g_action_print_detailed_name(action_name,target_value);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_app_info_create_from_commandline(Stack stack, int rhs, int opt, int lhs) /* g_app_info_create_from_commandline */
{
  int_types T[] = {string,string,obj, t_end};
  char *commandline, *application_name;
  GAppInfoCreateFlags flags;
  NspObject *nsp_flags = NULL, *nsp_ret;
  GError *error = NULL;
  GAppInfo *ret;
  if ( GetArgs(stack,rhs,opt,T,&commandline, &application_name, &nsp_flags) == FAIL) return RET_BUG;
  if (nspg_flags_get_value(G_TYPE_APP_INFO_CREATE_FLAGS, nsp_flags, &flags)==FAIL)
      return RET_BUG;
    ret =g_app_info_create_from_commandline(commandline,application_name,flags,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gappinfo = new_type_gappinfo(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gappinfo))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_app_info_get_all(Stack stack, int rhs, int opt, int lhs) /* g_app_info_get_all */
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_app_info_get_all();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_app_info_get_all_for_type(Stack stack, int rhs, int opt, int lhs) /* g_app_info_get_all_for_type */
{
  int_types T[] = {string, t_end};
  char *content_type;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_get_all_for_type(content_type);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_app_info_get_recommended_for_type(Stack stack, int rhs, int opt, int lhs) /* g_app_info_get_recommended_for_type */
{
  int_types T[] = {string, t_end};
  char *content_type;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_get_recommended_for_type(content_type);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_app_info_get_fallback_for_type(Stack stack, int rhs, int opt, int lhs) /* g_app_info_get_fallback_for_type */
{
  int_types T[] = {string, t_end};
  char *content_type;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    ret =g_app_info_get_fallback_for_type(content_type);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_app_info_reset_type_associations(Stack stack, int rhs, int opt, int lhs) /* g_app_info_reset_type_associations */
{
  int_types T[] = {string, t_end};
  char *content_type;
  if ( GetArgs(stack,rhs,opt,T,&content_type) == FAIL) return RET_BUG;
    g_app_info_reset_type_associations(content_type);
  return 0;
}

int _wrap_g_app_info_get_default_for_type(Stack stack, int rhs, int opt, int lhs) /* g_app_info_get_default_for_type */
{
  int_types T[] = {string,s_bool, t_end};
  char *content_type;
  int must_support_uris;
  GAppInfo *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&content_type, &must_support_uris) == FAIL) return RET_BUG;
    ret =g_app_info_get_default_for_type(content_type,must_support_uris);
  nsp_type_gappinfo = new_type_gappinfo(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gappinfo))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_app_info_get_default_for_uri_scheme(Stack stack, int rhs, int opt, int lhs) /* g_app_info_get_default_for_uri_scheme */
{
  int_types T[] = {string, t_end};
  char *uri_scheme;
  GAppInfo *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri_scheme) == FAIL) return RET_BUG;
    ret =g_app_info_get_default_for_uri_scheme(uri_scheme);
  nsp_type_gappinfo = new_type_gappinfo(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gappinfo))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_app_info_launch_default_for_uri(Stack stack, int rhs, int opt, int lhs) /* g_app_info_launch_default_for_uri */
{
  int_types T[] = {string,obj_check, t_end};
  char *uri;
  NspGObject *launch_context;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&uri, &nsp_type_gapplaunchcontext, &launch_context) == FAIL) return RET_BUG;
    ret =g_app_info_launch_default_for_uri(uri,G_APP_LAUNCH_CONTEXT(launch_context->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_application_id_is_valid(Stack stack, int rhs, int opt, int lhs) /* g_application_id_is_valid */
{
  int_types T[] = {string, t_end};
  char *application_id;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&application_id) == FAIL) return RET_BUG;
    ret =g_application_id_is_valid(application_id);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

#line 163 "codegen-3.0/gio.override"
/* XXX we want to return the most specialized type */

int _wrap_g_application_get_default(Stack stack, int rhs, int opt, int lhs) /* g_application_get_default */
{
  GApplication *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
  ret =g_application_get_default();
  nsp_type_gapplication = new_type_gapplication(T_BASE);
  if ((nsp_ret = (NspObject *) nspgobject_new(NVOID,(GObject *)ret)) == NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}
#line 27309 "gio.c"


int _wrap_g_cancellable_get_current(Stack stack, int rhs, int opt, int lhs) /* g_cancellable_get_current */
{
  GCancellable *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_cancellable_get_current();
  nsp_type_gcancellable = new_type_gcancellable(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gcancellable))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_content_type_equals(Stack stack, int rhs, int opt, int lhs) /* g_content_type_equals */
{
  int_types T[] = {string,string, t_end};
  char *type1, *type2;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&type1, &type2) == FAIL) return RET_BUG;
    ret =g_content_type_equals(type1,type2);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_content_type_is_a(Stack stack, int rhs, int opt, int lhs) /* g_content_type_is_a */
{
  int_types T[] = {string,string, t_end};
  char *type, *supertype;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&type, &supertype) == FAIL) return RET_BUG;
    ret =g_content_type_is_a(type,supertype);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_content_type_is_unknown(Stack stack, int rhs, int opt, int lhs) /* g_content_type_is_unknown */
{
  int_types T[] = {string, t_end};
  char *type;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_is_unknown(type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_content_type_get_description(Stack stack, int rhs, int opt, int lhs) /* g_content_type_get_description */
{
  int_types T[] = {string, t_end};
  char *type;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_get_description(type);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_content_type_get_mime_type(Stack stack, int rhs, int opt, int lhs) /* g_content_type_get_mime_type */
{
  int_types T[] = {string, t_end};
  char *type;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_get_mime_type(type);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_content_type_get_icon(Stack stack, int rhs, int opt, int lhs) /* g_content_type_get_icon */
{
  int_types T[] = {string, t_end};
  char *type;
  GIcon *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_get_icon(type);
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_content_type_get_symbolic_icon(Stack stack, int rhs, int opt, int lhs) /* g_content_type_get_symbolic_icon */
{
  int_types T[] = {string, t_end};
  char *type;
  GIcon *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_get_symbolic_icon(type);
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_content_type_get_generic_icon_name(Stack stack, int rhs, int opt, int lhs) /* g_content_type_get_generic_icon_name */
{
  int_types T[] = {string, t_end};
  char *type;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_get_generic_icon_name(type);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_content_type_can_be_executable(Stack stack, int rhs, int opt, int lhs) /* g_content_type_can_be_executable */
{
  int_types T[] = {string, t_end};
  char *type;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&type) == FAIL) return RET_BUG;
    ret =g_content_type_can_be_executable(type);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_content_type_from_mime_type(Stack stack, int rhs, int opt, int lhs) /* g_content_type_from_mime_type */
{
  int_types T[] = {string, t_end};
  char *mime_type;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&mime_type) == FAIL) return RET_BUG;
    ret =g_content_type_from_mime_type(mime_type);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_content_type_guess_for_tree(Stack stack, int rhs, int opt, int lhs) /* g_content_type_guess_for_tree */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *root;
  gchar **ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gfile, &root) == FAIL) return RET_BUG;
    ret =g_content_type_guess_for_tree(G_FILE(root->obj));
  nsp_ret = (NspObject *) nsp_smatrix_create_from_table(ret);
  if ( nsp_ret == NULL) return RET_BUG;
  g_strfreev(ret);
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_content_types_get_registered(Stack stack, int rhs, int opt, int lhs) /* g_content_types_get_registered */
{
  GList *ret, *tmp;
  NspList *nsp_list;
  CheckRhs(0,0);
    ret =g_content_types_get_registered();
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_dbus_address_escape_value(Stack stack, int rhs, int opt, int lhs) /* g_dbus_address_escape_value */
{
  int_types T[] = {string, t_end};
  char *string;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_address_escape_value(string);
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_dbus_is_address(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_address */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_address(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dbus_is_supported_address(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_supported_address */
{
  int_types T[] = {string, t_end};
  char *string;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_supported_address(string,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dbus_address_get_stream_finish(Stack stack, int rhs, int opt, int lhs) /* g_dbus_address_get_stream_finish */
{
  int_types T[] = {obj_check,obj, t_end};
  NspGObject *res;
  gchar **out_guid = NULL;
  NspObject *nsp_out_guid = NULL, *nsp_ret;
  GError *error = NULL;
  GIOStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &res, &nsp_out_guid) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_out_guid))
    { out_guid =  ((NspSMatrix *) nsp_out_guid)->S;}
  else
    {
      Scierror("Error: out_guid should be of type SMat");
      return RET_BUG;
    }
    ret =g_dbus_address_get_stream_finish(G_ASYNC_RESULT(res->obj),out_guid,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_giostream = new_type_giostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_giostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_dbus_address_get_stream_sync(Stack stack, int rhs, int opt, int lhs) /* g_dbus_address_get_stream_sync */
{
  int_types T[] = {string,obj,obj_check, t_end};
  char *address;
  gchar **out_guid = NULL;
  NspObject *nsp_out_guid = NULL, *nsp_ret;
  NspGObject *cancellable;
  GError *error = NULL;
  GIOStream *ret;
  if ( GetArgs(stack,rhs,opt,T,&address, &nsp_out_guid, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if ( IsSMat(nsp_out_guid))
    { out_guid =  ((NspSMatrix *) nsp_out_guid)->S;}
  else
    {
      Scierror("Error: out_guid should be of type SMat");
      return RET_BUG;
    }
    ret =g_dbus_address_get_stream_sync(address,out_guid,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_giostream = new_type_giostream(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_giostream))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_dbus_address_get_for_bus_sync(Stack stack, int rhs, int opt, int lhs) /* g_dbus_address_get_for_bus_sync */
{
  int_types T[] = {obj,obj_check, t_end};
  GBusType bus_type;
  NspObject *nsp_bus_type = NULL;
  NspGObject *cancellable;
  GError *error = NULL;
  gchar *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_bus_type, &nsp_type_gcancellable, &cancellable) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_BUS_TYPE, nsp_bus_type, &bus_type)== FAIL)
      return RET_BUG;
    ret =g_dbus_address_get_for_bus_sync(bus_type,G_CANCELLABLE(cancellable->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_dbus_message_bytes_needed(Stack stack, int rhs, int opt, int lhs) /* g_dbus_message_bytes_needed */
{
  int_types T[] = {string,s_int, t_end};
  guchar *blob;
  int blob_len, ret;
  GError *error = NULL;
  if ( GetArgs(stack,rhs,opt,T,&blob, &blob_len) == FAIL) return RET_BUG;
    ret =g_dbus_message_bytes_needed(blob,blob_len,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_bus_unown_name(Stack stack, int rhs, int opt, int lhs) /* g_bus_unown_name */
{
  int_types T[] = {s_int, t_end};
  int owner_id;
  if ( GetArgs(stack,rhs,opt,T,&owner_id) == FAIL) return RET_BUG;
    g_bus_unown_name(owner_id);
  return 0;
}

int _wrap_g_bus_unwatch_name(Stack stack, int rhs, int opt, int lhs) /* g_bus_unwatch_name */
{
  int_types T[] = {s_int, t_end};
  int watcher_id;
  if ( GetArgs(stack,rhs,opt,T,&watcher_id) == FAIL) return RET_BUG;
    g_bus_unwatch_name(watcher_id);
  return 0;
}

int _wrap_g_dbus_is_guid(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_guid */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_guid(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dbus_generate_guid(Stack stack, int rhs, int opt, int lhs) /* g_dbus_generate_guid */
{
  gchar *ret;
  CheckRhs(0,0);
    ret =g_dbus_generate_guid();
  if ( nsp_move_string(stack,1,(ret) ? ret: "",-1)== FAIL) return RET_BUG;
  g_free(ret);
  return 1;
}

int _wrap_g_dbus_is_name(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_name */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_name(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dbus_is_unique_name(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_unique_name */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_unique_name(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dbus_is_member_name(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_member_name */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_member_name(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_dbus_is_interface_name(Stack stack, int rhs, int opt, int lhs) /* g_dbus_is_interface_name */
{
  int_types T[] = {string, t_end};
  char *string;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&string) == FAIL) return RET_BUG;
    ret =g_dbus_is_interface_name(string);
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_file_parse_name(Stack stack, int rhs, int opt, int lhs) /* g_file_parse_name */
{
  int_types T[] = {string, t_end};
  char *parse_name;
  GFile *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&parse_name) == FAIL) return RET_BUG;
    ret =g_file_parse_name(parse_name);
  nsp_type_gfile = new_type_gfile(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gfile))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_icon_deserialize(Stack stack, int rhs, int opt, int lhs) /* g_icon_deserialize */
{
  int_types T[] = {obj, t_end};
  GVariant *value = NULL;
  NspObject *nsp_value = NULL, *nsp_ret;
  GIcon *ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_value) == FAIL) return RET_BUG;
  if ( IsGVariant(nsp_value))
    { value = ((NspGVariant *) nsp_value)->obj->value;
      if((value = nsp_copy_GVariant(value))==NULL) return RET_BUG;
    }
  else
    {
      Scierror("Error: value should be of type GVariant\n");
      return RET_BUG;
    }
    ret =g_icon_deserialize(value);
  nsp_type_gicon = new_type_gicon(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gicon))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_io_error_from_errno(Stack stack, int rhs, int opt, int lhs) /* g_io_error_from_errno */
{
  int_types T[] = {s_int, t_end};
  int err_no;
  gint ret;
  if ( GetArgs(stack,rhs,opt,T,&err_no) == FAIL) return RET_BUG;
    ret =g_io_error_from_errno(err_no);
  if ( nsp_move_double(stack,1,(double) ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_io_modules_scan_all_in_directory(Stack stack, int rhs, int opt, int lhs) /* g_io_modules_scan_all_in_directory */
{
  int_types T[] = {string, t_end};
  char *dirname;
  if ( GetArgs(stack,rhs,opt,T,&dirname) == FAIL) return RET_BUG;
    g_io_modules_scan_all_in_directory(dirname);
  return 0;
}

int _wrap_g_io_modules_load_all_in_directory(Stack stack, int rhs, int opt, int lhs) /* g_io_modules_load_all_in_directory */
{
  int_types T[] = {string, t_end};
  char *dirname;
  GList *ret, *tmp;
  NspList *nsp_list;
  if ( GetArgs(stack,rhs,opt,T,&dirname) == FAIL) return RET_BUG;
    ret =g_io_modules_load_all_in_directory(dirname);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_io_stream_splice_finish(Stack stack, int rhs, int opt, int lhs) /* g_io_stream_splice_finish */
{
  int_types T[] = {obj_check, t_end};
  NspGObject *result;
  GError *error = NULL;
  int ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_type_gasyncresult, &result) == FAIL) return RET_BUG;
    ret =g_io_stream_splice_finish(G_ASYNC_RESULT(result->obj),&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  if ( nsp_move_boolean(stack,1,ret)==FAIL) return RET_BUG;
  return 1;
}

int _wrap_g_network_address_parse(Stack stack, int rhs, int opt, int lhs) /* g_network_address_parse */
{
  int_types T[] = {string,s_int, t_end};
  char *host_and_port;
  int default_port;
  GError *error = NULL;
  GSocketConnectable *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&host_and_port, &default_port) == FAIL) return RET_BUG;
    ret =g_network_address_parse(host_and_port,default_port,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnectable = new_type_gsocketconnectable(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnectable))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_network_address_parse_uri(Stack stack, int rhs, int opt, int lhs) /* g_network_address_parse_uri */
{
  int_types T[] = {string,s_int, t_end};
  char *uri;
  int default_port;
  GError *error = NULL;
  GSocketConnectable *ret;
  NspObject *nsp_ret;
  if ( GetArgs(stack,rhs,opt,T,&uri, &default_port) == FAIL) return RET_BUG;
    ret =g_network_address_parse_uri(uri,default_port,&error);
  if ( error != NULL ) {
    Scierror("%s: gtk error\n",NspFname(stack));
    return RET_BUG;
  }
  nsp_type_gsocketconnectable = new_type_gsocketconnectable(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gsocketconnectable))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_resolver_get_default(Stack stack, int rhs, int opt, int lhs) /* g_resolver_get_default */
{
  GResolver *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_resolver_get_default();
  nsp_type_gresolver = new_type_gresolver(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gresolver))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_resolver_free_addresses(Stack stack, int rhs, int opt, int lhs) /* g_resolver_free_addresses */
{
  int_types T[] = {list, t_end};
  NspList *nsp_addresses;
  GList *addresses;
  if ( GetArgs(stack,rhs,opt,T,&nsp_addresses) == FAIL) return RET_BUG;
  addresses=nsp_glist_from_nsplist(stack,nsp_addresses);
  if (addresses== NULL) return RET_BUG;
    g_resolver_free_addresses(addresses);
  return 0;
}

int _wrap_g_resolver_free_targets(Stack stack, int rhs, int opt, int lhs) /* g_resolver_free_targets */
{
  int_types T[] = {list, t_end};
  NspList *nsp_targets;
  GList *targets;
  if ( GetArgs(stack,rhs,opt,T,&nsp_targets) == FAIL) return RET_BUG;
  targets=nsp_glist_from_nsplist(stack,nsp_targets);
  if (targets== NULL) return RET_BUG;
    g_resolver_free_targets(targets);
  return 0;
}

int _wrap_g_settings_sync(Stack stack, int rhs, int opt, int lhs) /* g_settings_sync */
{
  CheckRhs(0,0);
    g_settings_sync();
  return 0;
}

int _wrap_g_socket_connection_factory_register_type(Stack stack, int rhs, int opt, int lhs) /* g_socket_connection_factory_register_type */
{
  int_types T[] = {obj,obj,obj,s_int, t_end};
  GType g_type;
  NspObject *nsp_g_type = NULL, *nsp_family = NULL, *nsp_type = NULL;
  GSocketFamily family;
  GSocketType type;
  int protocol;
  if ( GetArgs(stack,rhs,opt,T,&nsp_g_type, &nsp_family, &nsp_type, &protocol) == FAIL) return RET_BUG;
  if ((g_type = nspg_type_from_object(nsp_g_type)) == FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_FAMILY, nsp_family, &family)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
    g_socket_connection_factory_register_type(g_type,family,type,protocol);
  return 0;
}

int _wrap_g_socket_connection_factory_lookup_type(Stack stack, int rhs, int opt, int lhs) /* g_socket_connection_factory_lookup_type */
{
  int_types T[] = {obj,obj,s_int, t_end};
  GSocketFamily family;
  NspObject *nsp_family = NULL, *nsp_type = NULL;
  GSocketType type;
  int protocol_id;
  GType ret;
  if ( GetArgs(stack,rhs,opt,T,&nsp_family, &nsp_type, &protocol_id) == FAIL) return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_FAMILY, nsp_family, &family)== FAIL)
      return RET_BUG;
  if (nspg_enum_get_value(G_TYPE_SOCKET_TYPE, nsp_type, &type)== FAIL)
      return RET_BUG;
    ret =g_socket_connection_factory_lookup_type(family,type,protocol_id);
  return nspg_type_wrapper_new(ret);
}

int _wrap_g_srv_target_list_sort(Stack stack, int rhs, int opt, int lhs) /* g_srv_target_list_sort */
{
  int_types T[] = {list, t_end};
  NspList *nsp_targets, *nsp_list;
  GList *targets, *ret, *tmp;
  if ( GetArgs(stack,rhs,opt,T,&nsp_targets) == FAIL) return RET_BUG;
  targets=nsp_glist_from_nsplist(stack,nsp_targets);
  if (targets== NULL) return RET_BUG;
    ret =g_srv_target_list_sort(targets);
  NSP_LIST_FROM_GLIST(ret,nspgobject_new("lel",(GObject *)tmp->data),g_list_free);

}

int _wrap_g_test_dbus_unset(Stack stack, int rhs, int opt, int lhs) /* g_test_dbus_unset */
{
  CheckRhs(0,0);
    g_test_dbus_unset();
  return 0;
}

int _wrap_g_vfs_get_default(Stack stack, int rhs, int opt, int lhs) /* g_vfs_get_default */
{
  GVfs *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_vfs_get_default();
  nsp_type_gvfs = new_type_gvfs(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gvfs))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_vfs_get_local(Stack stack, int rhs, int opt, int lhs) /* g_vfs_get_local */
{
  GVfs *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_vfs_get_local();
  nsp_type_gvfs = new_type_gvfs(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gvfs))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

int _wrap_g_volume_monitor_get(Stack stack, int rhs, int opt, int lhs) /* g_volume_monitor_get */
{
  GVolumeMonitor *ret;
  NspObject *nsp_ret;
  CheckRhs(0,0);
    ret =g_volume_monitor_get();
  nsp_type_gvolumemonitor = new_type_gvolumemonitor(T_BASE);
  if ((nsp_ret = (NspObject *) gobject_create(NVOID,(GObject *)ret,(NspTypeBase *) nsp_type_gvolumemonitor))== NULL) return RET_BUG;
  MoveObj(stack,1,nsp_ret);
  return 1;
}

/*----------------------------------------------------
 * Interface 
 * i.e a set of function which are accessible at nsp level
 *----------------------------------------------------*/

static OpTab gio_func[]={
  { "g_app_launch_context_new", _wrap_g_app_launch_context_new},
  { "gapplaunchcontext_new", _wrap_g_app_launch_context_new},
  { "g_application_new", _wrap_g_application_new},
  { "gapplication_new", _wrap_g_application_new},
  { "g_async_initable_new_finish", _wrap_g_async_initable_new_finish},
  { "g_cancellable_new", _wrap_g_cancellable_new},
  { "gcancellable_new", _wrap_g_cancellable_new},
 /* gemblemedicon_new g_emblemed_icon_new */
  { "g_file_new_for_path", _wrap_g_file_new_for_path},
  { "g_file_new_for_uri", _wrap_g_file_new_for_uri},
  { "g_file_new_for_commandline_arg", _wrap_g_file_new_for_commandline_arg},
  { "g_file_new_for_commandline_arg_and_cwd", _wrap_g_file_new_for_commandline_arg_and_cwd},
 /* gfile_new g_file_new_tmp */
  { "g_icon_new_for_string", _wrap_g_icon_new_for_string},
  { "g_inet_address_new_from_string", _wrap_g_inet_address_new_from_string},
 /* ginetaddress_new g_inet_address_new_from_bytes */
  { "g_inet_address_new_loopback", _wrap_g_inet_address_new_loopback},
  { "g_inet_address_new_any", _wrap_g_inet_address_new_any},
  { "g_inet_address_mask_new", _wrap_g_inet_address_mask_new},
  { "ginetaddressmask_new", _wrap_g_inet_address_mask_new},
  { "g_inet_address_mask_new_from_string", _wrap_g_inet_address_mask_new_from_string},
 /* ginitable_new g_initable_new_valist */
  { "g_buffered_input_stream_new", _wrap_g_buffered_input_stream_new},
  { "gbufferedinputstream_new", _wrap_g_buffered_input_stream_new},
  { "g_buffered_input_stream_new_sized", _wrap_g_buffered_input_stream_new_sized},
  { "g_data_input_stream_new", _wrap_g_data_input_stream_new},
  { "gdatainputstream_new", _wrap_g_data_input_stream_new},
  { "g_converter_input_stream_new", _wrap_g_converter_input_stream_new},
  { "gconverterinputstream_new", _wrap_g_converter_input_stream_new},
  { "g_memory_input_stream_new", _wrap_g_memory_input_stream_new},
  { "gmemoryinputstream_new", _wrap_g_memory_input_stream_new},
 /* gmemoryinputstream_new g_memory_input_stream_new_from_data */
 /* gmemoryinputstream_new g_memory_input_stream_new_from_bytes */
  { "g_menu_new", _wrap_g_menu_new},
  { "gmenu_new", _wrap_g_menu_new},
  { "g_mount_operation_new", _wrap_g_mount_operation_new},
  { "gmountoperation_new", _wrap_g_mount_operation_new},
  { "g_network_address_new", _wrap_g_network_address_new},
  { "gnetworkaddress_new", _wrap_g_network_address_new},
  { "g_network_service_new", _wrap_g_network_service_new},
  { "gnetworkservice_new", _wrap_g_network_service_new},
 /* gmemoryoutputstream_new g_memory_output_stream_new */
  { "g_memory_output_stream_new_resizable", _wrap_g_memory_output_stream_new_resizable},
  { "g_buffered_output_stream_new", _wrap_g_buffered_output_stream_new},
  { "gbufferedoutputstream_new", _wrap_g_buffered_output_stream_new},
  { "g_buffered_output_stream_new_sized", _wrap_g_buffered_output_stream_new_sized},
  { "g_converter_output_stream_new", _wrap_g_converter_output_stream_new},
  { "gconverteroutputstream_new", _wrap_g_converter_output_stream_new},
  { "g_data_output_stream_new", _wrap_g_data_output_stream_new},
  { "gdataoutputstream_new", _wrap_g_data_output_stream_new},
  { "g_settings_new", _wrap_g_settings_new},
  { "gsettings_new", _wrap_g_settings_new},
  { "g_settings_new_with_path", _wrap_g_settings_new_with_path},
 /* gsettings_new g_settings_new_full */
  { "g_simple_action_new", _wrap_g_simple_action_new},
  { "gsimpleaction_new", _wrap_g_simple_action_new},
  { "g_simple_action_new_stateful", _wrap_g_simple_action_new_stateful},
  { "g_simple_action_group_new", _wrap_g_simple_action_group_new},
  { "gsimpleactiongroup_new", _wrap_g_simple_action_group_new},
  { "g_simple_proxy_resolver_new", _wrap_g_simple_proxy_resolver_new},
  { "gsimpleproxyresolver_new", _wrap_g_simple_proxy_resolver_new},
  { "g_socket_new", _wrap_g_socket_new},
  { "gsocket_new", _wrap_g_socket_new},
  { "g_socket_new_from_fd", _wrap_g_socket_new_from_fd},
 /* gsocketaddress_new g_socket_address_new_from_native */
  { "g_inet_socket_address_new", _wrap_g_inet_socket_address_new},
  { "ginetsocketaddress_new", _wrap_g_inet_socket_address_new},
  { "g_inet_socket_address_new_from_string", _wrap_g_inet_socket_address_new_from_string},
  { "g_proxy_address_new", _wrap_g_proxy_address_new},
  { "gproxyaddress_new", _wrap_g_proxy_address_new},
  { "g_socket_client_new", _wrap_g_socket_client_new},
  { "gsocketclient_new", _wrap_g_socket_client_new},
  { "g_socket_listener_new", _wrap_g_socket_listener_new},
  { "gsocketlistener_new", _wrap_g_socket_listener_new},
  { "g_socket_service_new", _wrap_g_socket_service_new},
  { "gsocketservice_new", _wrap_g_socket_service_new},
  { "g_tcp_wrapper_connection_new", _wrap_g_tcp_wrapper_connection_new},
  { "gtcpwrapperconnection_new", _wrap_g_tcp_wrapper_connection_new},
  { "g_threaded_socket_service_new", _wrap_g_threaded_socket_service_new},
  { "gthreadedsocketservice_new", _wrap_g_threaded_socket_service_new},
  { "g_tls_certificate_new_from_pem", _wrap_g_tls_certificate_new_from_pem},
  { "g_tls_certificate_new_from_file", _wrap_g_tls_certificate_new_from_file},
  { "g_tls_certificate_new_from_files", _wrap_g_tls_certificate_new_from_files},
 /* gtlspassword_new g_tls_password_new */
  { "g_action_name_is_valid", _wrap_g_action_name_is_valid},
  { "g_action_print_detailed_name", _wrap_g_action_print_detailed_name},
  { "g_app_info_create_from_commandline", _wrap_g_app_info_create_from_commandline},
  { "g_app_info_get_all", _wrap_g_app_info_get_all},
  { "g_app_info_get_all_for_type", _wrap_g_app_info_get_all_for_type},
  { "g_app_info_get_recommended_for_type", _wrap_g_app_info_get_recommended_for_type},
  { "g_app_info_get_fallback_for_type", _wrap_g_app_info_get_fallback_for_type},
  { "g_app_info_reset_type_associations", _wrap_g_app_info_reset_type_associations},
  { "g_app_info_get_default_for_type", _wrap_g_app_info_get_default_for_type},
  { "g_app_info_get_default_for_uri_scheme", _wrap_g_app_info_get_default_for_uri_scheme},
  { "g_app_info_launch_default_for_uri", _wrap_g_app_info_launch_default_for_uri},
  { "g_application_id_is_valid", _wrap_g_application_id_is_valid},
  { "g_application_get_default", _wrap_g_application_get_default},
  { "g_cancellable_get_current", _wrap_g_cancellable_get_current},
  { "g_content_type_equals", _wrap_g_content_type_equals},
  { "g_content_type_is_a", _wrap_g_content_type_is_a},
  { "g_content_type_is_unknown", _wrap_g_content_type_is_unknown},
  { "g_content_type_get_description", _wrap_g_content_type_get_description},
  { "g_content_type_get_mime_type", _wrap_g_content_type_get_mime_type},
  { "g_content_type_get_icon", _wrap_g_content_type_get_icon},
  { "g_content_type_get_symbolic_icon", _wrap_g_content_type_get_symbolic_icon},
  { "g_content_type_get_generic_icon_name", _wrap_g_content_type_get_generic_icon_name},
  { "g_content_type_can_be_executable", _wrap_g_content_type_can_be_executable},
  { "g_content_type_from_mime_type", _wrap_g_content_type_from_mime_type},
  { "g_content_type_guess_for_tree", _wrap_g_content_type_guess_for_tree},
  { "g_content_types_get_registered", _wrap_g_content_types_get_registered},
  { "g_dbus_address_escape_value", _wrap_g_dbus_address_escape_value},
  { "g_dbus_is_address", _wrap_g_dbus_is_address},
  { "g_dbus_is_supported_address", _wrap_g_dbus_is_supported_address},
  { "g_dbus_address_get_stream_finish", _wrap_g_dbus_address_get_stream_finish},
  { "g_dbus_address_get_stream_sync", _wrap_g_dbus_address_get_stream_sync},
  { "g_dbus_address_get_for_bus_sync", _wrap_g_dbus_address_get_for_bus_sync},
  { "g_dbus_message_bytes_needed", _wrap_g_dbus_message_bytes_needed},
  { "g_bus_unown_name", _wrap_g_bus_unown_name},
  { "g_bus_unwatch_name", _wrap_g_bus_unwatch_name},
  { "g_dbus_is_guid", _wrap_g_dbus_is_guid},
  { "g_dbus_generate_guid", _wrap_g_dbus_generate_guid},
  { "g_dbus_is_name", _wrap_g_dbus_is_name},
  { "g_dbus_is_unique_name", _wrap_g_dbus_is_unique_name},
  { "g_dbus_is_member_name", _wrap_g_dbus_is_member_name},
  { "g_dbus_is_interface_name", _wrap_g_dbus_is_interface_name},
  { "g_file_parse_name", _wrap_g_file_parse_name},
  { "g_icon_deserialize", _wrap_g_icon_deserialize},
  { "g_io_error_from_errno", _wrap_g_io_error_from_errno},
  { "g_io_modules_scan_all_in_directory", _wrap_g_io_modules_scan_all_in_directory},
  { "g_io_modules_load_all_in_directory", _wrap_g_io_modules_load_all_in_directory},
  { "g_io_stream_splice_finish", _wrap_g_io_stream_splice_finish},
  { "g_network_address_parse", _wrap_g_network_address_parse},
  { "g_network_address_parse_uri", _wrap_g_network_address_parse_uri},
  { "g_resolver_get_default", _wrap_g_resolver_get_default},
  { "g_resolver_free_addresses", _wrap_g_resolver_free_addresses},
  { "g_resolver_free_targets", _wrap_g_resolver_free_targets},
  { "g_settings_sync", _wrap_g_settings_sync},
  { "g_socket_connection_factory_register_type", _wrap_g_socket_connection_factory_register_type},
  { "g_socket_connection_factory_lookup_type", _wrap_g_socket_connection_factory_lookup_type},
  { "g_srv_target_list_sort", _wrap_g_srv_target_list_sort},
  { "g_test_dbus_unset", _wrap_g_test_dbus_unset},
  { "g_vfs_get_default", _wrap_g_vfs_get_default},
  { "g_vfs_get_local", _wrap_g_vfs_get_local},
  { "g_volume_monitor_get", _wrap_g_volume_monitor_get},
  { NULL, NULL}
};

/* call ith function in the gio interface */

int gio_Interf(int i, Stack stack, int rhs, int opt, int lhs)
{
#ifdef NSP_WITH_MAIN_GTK_THREAD
  return nsp_interface_executed_in_main_thread(i,gio_func[i].fonc,
  					       &stack,rhs,opt,lhs);
#else
  return (*(gio_func[i].fonc))(stack,rhs,opt,lhs);
#endif
}

/* used to walk through the interface table 
    (for adding or removing functions) */

void gio_Interf_Info(int i, char **fname, function ( **f))
{
  *fname = gio_func[i].name;
  *f = gio_func[i].fonc;
}

/* ----------- enums and flags ----------- */

void
gio_add_constants(NspObject *module, const gchar *strip_prefix)
{
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_APP_INFO_CREATE_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_CONVERTER_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_CONVERTER_RESULT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DATA_STREAM_BYTE_ORDER, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DATA_STREAM_NEWLINE_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_FILE_ATTRIBUTE_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_FILE_ATTRIBUTE_INFO_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_FILE_ATTRIBUTE_STATUS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_FILE_QUERY_INFO_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_FILE_CREATE_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_FILE_MEASURE_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_MOUNT_UNMOUNT_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DRIVE_START_STOP_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_FILE_COPY_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_FILE_MONITOR_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_FILE_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_FILESYSTEM_PREVIEW_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_FILE_MONITOR_EVENT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_IO_ERROR_ENUM, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_ASK_PASSWORD_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_PASSWORD_SAVE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_MOUNT_OPERATION_RESULT, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_OUTPUT_STREAM_SPLICE_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_IO_STREAM_SPLICE_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_EMBLEM_ORIGIN, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_RESOLVER_ERROR, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_RESOLVER_RECORD_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_RESOURCE_ERROR, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_RESOURCE_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_SOCKET_FAMILY, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_SOCKET_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_SOCKET_PROTOCOL, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_ZLIB_COMPRESSOR_FORMAT, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_UNIX_SOCKET_ADDRESS_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_BUS_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_BUS_NAME_OWNER_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_BUS_NAME_WATCHER_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_PROXY_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DBUS_ERROR, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_CONNECTION_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_CAPABILITY_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_CALL_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DBUS_MESSAGE_TYPE, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_MESSAGE_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DBUS_MESSAGE_HEADER_FIELD, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_PROPERTY_INFO_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_SUBTREE_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_SERVER_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_SIGNAL_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_SEND_MESSAGE_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_CREDENTIALS_TYPE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_DBUS_MESSAGE_BYTE_ORDER, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_APPLICATION_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_TLS_ERROR, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_TLS_CERTIFICATE_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_TLS_AUTHENTICATION_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_TLS_REHANDSHAKE_MODE, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_TLS_INTERACTION_RESULT, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_INTERFACE_SKELETON_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_DBUS_OBJECT_MANAGER_CLIENT_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_TLS_DATABASE_LOOKUP_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_TLS_CERTIFICATE_REQUEST_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_IO_MODULE_SCOPE_FLAGS, strip_prefix);
  nsp_enum_add_constants((NspHash  * ) module, G_TYPE_SOCKET_CLIENT_EVENT, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_SUBPROCESS_FLAGS, strip_prefix);
  nsp_flags_add_constants((NspHash * ) module, G_TYPE_SETTINGS_BIND_FLAGS, strip_prefix);
}

void nsp_initialize_gio_types(void)
{
  new_type_gdbusinterface(T_BASE);
  new_type_gdbusobject(T_BASE);
  new_type_gaction(T_BASE);
  new_type_gactiongroup(T_BASE);
  new_type_gactionmap(T_BASE);
  new_type_gappinfo(T_BASE);
  new_type_gapplaunchcontext(T_BASE);
  new_type_gapplication(T_BASE);
  new_type_gapplicationcommandline(T_BASE);
  new_type_gasyncinitable(T_BASE);
  new_type_gasyncresult(T_BASE);
  new_type_gcancellable(T_BASE);
  new_type_gconverter(T_BASE);
  new_type_gdbusinterfaceskeleton(T_BASE);
  new_type_gdbusobjectmanagerclient(T_BASE);
  new_type_gdbusobjectmanagerserver(T_BASE);
  new_type_gdbusobjectproxy(T_BASE);
  new_type_gdbusobjectskeleton(T_BASE);
  new_type_gdbusproxy(T_BASE);
  new_type_gdrive(T_BASE);
  new_type_gemblemedicon(T_BASE);
  new_type_gfile(T_BASE);
  new_type_gfileenumerator(T_BASE);
  new_type_gfilemonitor(T_BASE);
  new_type_giostream(T_BASE);
  new_type_gfileiostream(T_BASE);
  new_type_gicon(T_BASE);
  new_type_ginetaddress(T_BASE);
  new_type_ginetaddressmask(T_BASE);
  new_type_ginitable(T_BASE);
  new_type_ginputstream(T_BASE);
  new_type_gfilterinputstream(T_BASE);
  new_type_gbufferedinputstream(T_BASE);
  new_type_gdatainputstream(T_BASE);
  new_type_gconverterinputstream(T_BASE);
  new_type_gfileinputstream(T_BASE);
  new_type_gloadableicon(T_BASE);
  new_type_gmemoryinputstream(T_BASE);
  new_type_gmenuattributeiter(T_BASE);
  new_type_gmenulinkiter(T_BASE);
  new_type_gmenumodel(T_BASE);
  new_type_gmenu(T_BASE);
  new_type_gmount(T_BASE);
  new_type_gmountoperation(T_BASE);
  new_type_gnetworkaddress(T_BASE);
  new_type_gnetworkservice(T_BASE);
  new_type_goutputstream(T_BASE);
  new_type_gmemoryoutputstream(T_BASE);
  new_type_gfilteroutputstream(T_BASE);
  new_type_gbufferedoutputstream(T_BASE);
  new_type_gconverteroutputstream(T_BASE);
  new_type_gdataoutputstream(T_BASE);
  new_type_gfileoutputstream(T_BASE);
  new_type_gpermission(T_BASE);
  new_type_gresolver(T_BASE);
  new_type_gseekable(T_BASE);
  new_type_gsettings(T_BASE);
  new_type_gsimpleaction(T_BASE);
  new_type_gsimpleactiongroup(T_BASE);
  new_type_gsimpleproxyresolver(T_BASE);
  new_type_gsocket(T_BASE);
  new_type_gsocketaddress(T_BASE);
  new_type_ginetsocketaddress(T_BASE);
  new_type_gproxyaddress(T_BASE);
  new_type_gsocketaddressenumerator(T_BASE);
  new_type_gproxyaddressenumerator(T_BASE);
  new_type_gsocketclient(T_BASE);
  new_type_gsocketconnectable(T_BASE);
  new_type_gsocketconnection(T_BASE);
  new_type_gsocketcontrolmessage(T_BASE);
  new_type_gsocketlistener(T_BASE);
  new_type_gsocketservice(T_BASE);
  new_type_gtcpconnection(T_BASE);
  new_type_gtcpwrapperconnection(T_BASE);
  new_type_gthreadedsocketservice(T_BASE);
  new_type_gtlscertificate(T_BASE);
  new_type_gtlsconnection(T_BASE);
  new_type_gtlsdatabase(T_BASE);
  new_type_gtlsinteraction(T_BASE);
  new_type_gtlspassword(T_BASE);
  new_type_gvfs(T_BASE);
  new_type_gvolume(T_BASE);
  new_type_gvolumemonitor(T_BASE);
  new_type_gnativevolumemonitor(T_BASE);
}

#line 28278 "gio.c"
