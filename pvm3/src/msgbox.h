
/* $Id$ */

/*
 *         PVM version 3.4:  Parallel Virtual Machine System
 *               University of Tennessee, Knoxville TN.
 *           Oak Ridge National Laboratory, Oak Ridge TN.
 *                   Emory University, Atlanta GA.
 *      Authors:  J. J. Dongarra, G. E. Fagg, M. Fischer
 *          G. A. Geist, J. A. Kohl, R. J. Manchek, P. Mucci,
 *         P. M. Papadopoulos, S. L. Scott, and V. S. Sunderam
 *                   (C) 1997 All Rights Reserved
 *
 *                              NOTICE
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby granted
 * provided that the above copyright notice appear in all copies and
 * that both the copyright notice and this permission notice appear in
 * supporting documentation.
 *
 * Neither the Institutions (Emory University, Oak Ridge National
 * Laboratory, and University of Tennessee) nor the Authors make any
 * representations about the suitability of this software for any
 * purpose.  This software is provided ``as is'' without express or
 * implied warranty.
 *
 * PVM version 3 was funded in part by the U.S. Department of Energy,
 * the National Science Foundation and the State of Tennessee.
 */

/*
 *	msgbox.h
 *
 *	Message mailbox includes.
 *
 * $Log$
 * Revision 1.1.1.1  2003/11/14 13:02:08  stochopt
 * Imported files
 *
 * Revision 1.1.1.1  2003/02/18 09:26:05  stochopt
 * initial
 *
 * Revision 1.1.1.1  2003/02/17 11:25:32  stochopt
 * initial
 *
 * Revision 1.1.1.1  2003/02/03 08:42:12  stochopt
 * imported sources
 *
 * Revision 1.1.1.1  2003/01/06 09:04:12  stochopt
 * Imported sources
 *
 * Revision 1.1.1.1  2002/12/19 17:37:37  stochopt
 * start
 *
 * Revision 1.2  2002/10/14 14:37:49  chanceli
 * update
 *
 * Revision 1.5  1999/07/08 19:00:22  kohl
 * Fixed "Log" keyword placement.
 * 	- indent with " * " for new CVS.
 *
 * Revision 1.4  1997/06/25  22:09:14  pvmsrc
 * Markus adds his frigging name to the author list of
 * 	every file he ever looked at...
 *
 * Revision 1.3  1997/04/10  17:53:55  pvmsrc
 * Externalized me_new()...
 * 	- for WT_RECVINFO usage...
 *
 * Revision 1.2  1997/04/08  19:41:12  pvmsrc
 * Added me_savetid to pvmmentry struct.
 * 	- for keeping track of original owner on persistent mboxes.
 * 	- allows more careful cleanup in a system reset, set me_tid to 0
 * 		but save original tid in me_savetid first...  :-)
 *
 * Revision 1.1  1997/04/08  18:44:27  pvmsrc
 * Initial revision
 *
 *
 */


/*
 *	one of these for each name (class) in the database
 */

struct pvmmclass {
	struct pvmmclass *mc_link;		/* dll of peers */
	struct pvmmclass *mc_rlink;
	char *mc_name;					/* class name or 0 if list master */
	struct pvmmentry *mc_ent;		/* entries in class */
};

/*
 *	one of these for each index (entry) of a class
 */

struct pvmmentry {
	struct pvmmentry *me_link;		/* dll of peers */
	struct pvmmentry *me_rlink;
	int me_ind;						/* index in class or -1 if master */
	int me_tid;						/* tid of owner */
	int me_savetid;					/* previous owner tid, now dead */
	int me_flags;					/* options */
	struct pmsg *me_msg;			/* value (message) */
};


struct pvmmentry *me_new __ProtoGlarp__(( int ));

