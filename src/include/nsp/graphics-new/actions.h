#ifndef NSP_ACTIONS_H
#define NSP_ACTIONS_H

/*
 * This Software is GPL (Copyright ENPC 2009-2009) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 *
 * The following functions perform scale changes and then redirect 
 * to the graphics driver.
 */

typedef int  driver_gc_change(BCG *Xgc);
typedef void driver_gc_2dzoom(BCG *Xgc);
typedef void driver_gc_3drot(BCG *Xgc);
typedef void driver_gc_delete(BCG *Xgc);
typedef void driver_gc_erase(BCG *Xgc);
typedef void driver_gc_expose(BCG *Xgc);
typedef int  driver_gc_loadsg(BCG *Xgc,const char *filename);
typedef void driver_gc_raise(BCG *Xgc);
typedef void driver_gc_replay(BCG *Xgc);
typedef void driver_gc_resize(BCG *Xgc);
typedef void driver_gc_resize_pixmap(BCG *Xgc);
typedef int driver_gc_savesg(BCG *Xgc,const char *filename);
typedef void driver_gc_select(BCG *Xgc);
typedef void driver_gc_tops(BCG *Xgc, int colored,const char *bufname,const char *driver,char option);
typedef void driver_gc_unzoom(BCG *Xgc);

typedef struct _nsp_gc_actions Nsp_gc_actions ;

struct _nsp_gc_actions {
  driver_gc_change *change ;
  driver_gc_2dzoom *zoom ;
  driver_gc_3drot  *rotation ;
  driver_gc_delete *delete ;
  driver_gc_erase  *erase ;
  driver_gc_expose *expose ;
  driver_gc_loadsg *loadsg; 
  driver_gc_raise  *raise ;
  driver_gc_replay *replay ;
  driver_gc_resize *resize ;
  driver_gc_resize_pixmap *resize_pixmap ;
  driver_gc_savesg *savesg ;
  driver_gc_select *select ;
  driver_gc_tops   *tops ;
  driver_gc_unzoom *unzoom ;
};

extern Nsp_gc_actions nsp_gc_actions ;

#endif 

#ifdef PERI_ACTION_PRIVATE 

static int  nsp_gc_change(BCG *Xgc);
static void nsp_gc_2dzoom(BCG *Xgc);
static void nsp_gc_3drot(BCG *Xgc);
static void nsp_gc_delete(BCG *Xgc);
static void nsp_gc_erase(BCG *Xgc);
static void nsp_gc_expose(BCG *Xgc);
static int  nsp_gc_loadsg(BCG *Xgc,const char *filename);
static void nsp_gc_raise(BCG *Xgc);
static void nsp_gc_replay(BCG *Xgc);
static void nsp_gc_resize(BCG *Xgc);
static void nsp_gc_resize_pixmap(BCG *Xgc);
static int  nsp_gc_savesg(BCG *Xgc,const char *filename);
static void nsp_gc_sel(BCG *Xgc);
static void nsp_gc_tops(BCG *Xgc, int colored,const char *bufname,const char *driver,char option);
static void nsp_gc_unzoom(BCG *Xgc);

Nsp_gc_actions nsp_gc_actions = {
  nsp_gc_change,
  nsp_gc_2dzoom,
  nsp_gc_3drot,
  nsp_gc_delete,
  nsp_gc_erase,
  nsp_gc_expose,
  nsp_gc_loadsg,
  nsp_gc_raise,
  nsp_gc_replay,
  nsp_gc_resize,
  nsp_gc_resize_pixmap,
  nsp_gc_savesg,
  nsp_gc_sel,
  nsp_gc_tops,
  nsp_gc_unzoom
};

#endif 

