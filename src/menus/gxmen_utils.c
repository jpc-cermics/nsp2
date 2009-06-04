/* Nsp
 * Copyright (C) 1998-2009 Jean-Philippe Chancelier Enpc/Cermics
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
 *
 * a set of utility functions.
 *
 *--------------------------------------------------------------------------*/

#include "nsp/menus.h"

void nsp_dialogs_insert_title(const char *title,GtkWidget *vbox)
{
  if ( title[0] != '\0' )
    {
      GtkWidget *hbox = gtk_hbox_new (FALSE, 0);
      GtkWidget *image= gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION,
						  GTK_ICON_SIZE_DIALOG);
      gtk_box_pack_start (GTK_BOX (vbox),hbox, FALSE, FALSE, 5);
      gtk_box_pack_start (GTK_BOX (hbox),image,FALSE, FALSE, 5);
      gtk_misc_set_alignment (GTK_MISC(image), 0.5, 0.0);
      gtk_box_pack_start (GTK_BOX (hbox), gtk_label_new (title), FALSE, FALSE,5);
    }
}


