#ifndef NSP_INC_QHULL
#define NSP_INC_QHULL

/*
 * This Software is GPL (Copyright ENPC 1998-2018) 
 * Jean-Philippe Chancelier Enpc/Cermics         
 */
#include <nsp/config.h>

#ifdef HAVE_QHULL_LIBQHULL_H
#include <qhull/libqhull.h>
#include <qhull/qset.h>
#include <qhull/geom.h>
#include <qhull/poly.h>
#include <qhull/io.h>
#else
#ifdef HAVE_LIBQHULL_LIBQHULL_H
#include <libqhull/libqhull.h>
#include <libqhull/qset.h>
#include <libqhull/geom.h>
#include <libqhull/poly.h>
#include <libqhull/io.h>
#endif
#endif


#endif 
