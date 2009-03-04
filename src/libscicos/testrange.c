#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "gtk/gtk.h"

static double *val = NULL;

void range_value_changed (GtkRange *range,
			  gpointer user_data)
{
  fprintf(stderr,"new value %f\n",gtk_range_get_value (range));
  if ( val != NULL) *val = gtk_range_get_value (range);
}


void create_range_controls(double *ext_val)
{
  static GtkWidget *window = NULL;
  GtkWidget *box1;
  GtkWidget *box2;
  GtkWidget *button;
  GtkWidget *scale;
  GtkWidget *separator;
  GtkObject *adjustment;

  val = ext_val;
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  
  g_signal_connect (window, "destroy",
		    G_CALLBACK (gtk_widget_destroyed),
		    &window);

  gtk_window_set_title (GTK_WINDOW (window), "range controls");
  gtk_container_set_border_width (GTK_CONTAINER (window), 0);
  
  box1 = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), box1);
  gtk_widget_show (box1);
  
  box2 = gtk_vbox_new (FALSE, 10);
  gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
  gtk_box_pack_start (GTK_BOX (box1), box2, TRUE, TRUE, 0);
  gtk_widget_show (box2);

  adjustment = gtk_adjustment_new (0.0, 0.0, 101.0, 0.1, 1.0, 1.0);
  
  scale = gtk_hscale_new (GTK_ADJUSTMENT (adjustment));
  gtk_widget_set_size_request (GTK_WIDGET (scale), 150, -1);
  gtk_range_set_update_policy (GTK_RANGE (scale), GTK_UPDATE_DELAYED);
  gtk_scale_set_digits (GTK_SCALE (scale), 1);
  gtk_scale_set_draw_value (GTK_SCALE (scale), TRUE);
  gtk_box_pack_start (GTK_BOX (box2), scale, TRUE, TRUE, 0);
  g_signal_connect (GTK_RANGE (scale),
		    "value_changed",
		    G_CALLBACK (range_value_changed),
		    NULL);
  gtk_widget_show (scale);
  
  separator = gtk_hseparator_new ();
  gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
  gtk_widget_show (separator);
  
  box2 = gtk_vbox_new (FALSE, 10);
  gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
  gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
  gtk_widget_show (box2);

  button = gtk_button_new_with_label ("close");
  g_signal_connect_swapped (button, "clicked",
			    G_CALLBACK (gtk_widget_destroy),
			    window);
  gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button);
  gtk_widget_show (button);
  gtk_widget_show (window);
}


