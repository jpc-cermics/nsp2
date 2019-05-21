#ifndef NSP_INC_INTSF
#define NSP_INC_INTSF

/*
 * Copyright (C) 2010-2019 Jean-Philippe Chancelier Enpc/Cermics
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
/*
 * forward declaration fo ints.h 
 */

#ifndef NSP_INC_FWD_ITYPE
#define NSP_INC_FWD_ITYPE
typedef enum { nsp_gint, nsp_guint, nsp_gshort, nsp_gushort, nsp_glong , 
		    nsp_gulong, nsp_gint8, nsp_guint8, nsp_gint16,
		    nsp_guint16, nsp_gint32, nsp_guint32, nsp_gint64, 
		    nsp_guint64 } nsp_itype;

typedef union nsp_int_union_ nsp_int_union;
typedef union nsp_int_union_ptr_ nsp_int_union_ptr;
#endif 

#endif 

