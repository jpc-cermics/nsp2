/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include <gtk/gtk.h>
#include "nsp/menus.h"
#include "nsp/gtksci.h"

typedef enum { MES_OK, MES_CANCEL,MES_RESET } state;

static void sci_message_ok(GtkWidget *widget,state *answer)
{
  *answer = MES_OK;
  gtk_main_quit();
}

static void sci_message_cancel(GtkWidget *widget,state *answer)
{
  *answer = MES_CANCEL;
  gtk_main_quit();
}

int nsp_message_gtk1(char *message,char **buttons,int n_buttons)
{
  GtkWidget *window = NULL;
  GtkWidget *box1;
  GtkWidget *box2;
  GtkWidget *button;
  GtkWidget *separator;
  GtkWidget *scrolled_window;
  GtkWidget *label;
  state answer = MES_RESET ;

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_widget_set_name (window, "Scilab message");
  gtk_widget_set_usize (window, 300,300);
  gtk_window_set_position (GTK_WINDOW (window), GTK_WIN_POS_MOUSE);
  gtk_window_set_policy (GTK_WINDOW(window), TRUE, TRUE, FALSE);

  gtk_signal_connect (GTK_OBJECT (window), "destroy",
		      GTK_SIGNAL_FUNC(gtk_widget_destroyed),
		      &window);

  gtk_window_set_title (GTK_WINDOW (window), "Scilab message");
  gtk_container_set_border_width (GTK_CONTAINER (window), 0);

  box1 = gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), box1);
  gtk_widget_show (box1);

  scrolled_window = gtk_scrolled_window_new (NULL, NULL);
  gtk_box_pack_start (GTK_BOX (box1), scrolled_window, TRUE, TRUE, 0);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_window);

  label = gtk_label_new (message);
  gtk_widget_show (label);
  gtk_scrolled_window_add_with_viewport( GTK_SCROLLED_WINDOW (scrolled_window), label);

  separator = gtk_hseparator_new ();
  gtk_box_pack_start (GTK_BOX (box1), separator, FALSE, TRUE, 0);
  gtk_widget_show (separator);

  box2 = gtk_hbox_new (FALSE, 10);
  gtk_container_set_border_width (GTK_CONTAINER (box2), 10);
  gtk_box_pack_start (GTK_BOX (box1), box2, FALSE, TRUE, 0);
  gtk_widget_show (box2);

  button = gtk_button_new_with_label (buttons[0]);
  gtk_signal_connect (GTK_OBJECT (button), "clicked",
		      GTK_SIGNAL_FUNC(sci_message_ok),
		      &answer);

  gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
  GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
  gtk_widget_grab_default (button);
  gtk_widget_show (button);

  if ( n_buttons == 2) 
    {
      button = gtk_button_new_with_label (buttons[1]);
      gtk_signal_connect (GTK_OBJECT (button), "clicked",
			  GTK_SIGNAL_FUNC(sci_message_cancel),
			  &answer);
      gtk_box_pack_start (GTK_BOX (box2), button, TRUE, TRUE, 0);
      gtk_widget_show (button);
    }

  gtk_widget_show (window);
  gtk_main();
  gtk_widget_destroy(window);
  return answer ;
}



/*  
 * message with just an OK button 
 */  

char *sci_convert_to_utf8(char *str, int *alloc)
{
  gchar *msg_utf8 =NULL;
  G_CONST_RETURN char *charset;
  if (g_get_charset (&charset)) 
    {
      *alloc = FALSE; 
      msg_utf8 = str; 
    }
  else 
    {
      msg_utf8= g_locale_to_utf8 (str, -1, NULL, NULL, NULL);
      *alloc = TRUE; 
    }
  return msg_utf8; 
}

int nsp_message_modeless_(char *message)
{
  int alloc;
  GtkWidget *dialog, *window=NULL;
  char *msg_utf8;

  start_sci_gtk(); /* be sure that gtk is started */
  msg_utf8 = sci_convert_to_utf8(message,&alloc);
  
  dialog = gtk_message_dialog_new (GTK_WINDOW (window),
				   GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
				   GTK_MESSAGE_INFO,
				   GTK_BUTTONS_OK,
				   message);
  g_signal_connect (dialog, "response",  G_CALLBACK (gtk_widget_destroy),  NULL);
  gtk_widget_show (dialog);
  if ( alloc == TRUE) g_free (msg_utf8);
  return 1;
}

int nsp_message_(char *message,char **buttons,int n_buttons)
{
  GtkWidget *dialog;
  GtkWidget *hbox;
  GtkWidget *stock;
  GtkWidget *label;
  GtkWidget *window=NULL;
  gint response;
  char *ok_mess, *cancel_mess;
  int alloc;
  char *msg_utf8;

  start_sci_gtk(); /* be sure that gtk is started */
  msg_utf8 = sci_convert_to_utf8(message,&alloc);

  ok_mess = buttons[0];
  if ( strcasecmp(ok_mess,"Ok")==0 ) ok_mess = GTK_STOCK_OK; 

  switch ( n_buttons ) 
    {
    case 0: return 1 ; break;
    case 1 : 
      dialog = gtk_dialog_new_with_buttons ("Scilab Dialog", GTK_WINDOW (window),
					    GTK_DIALOG_MODAL| GTK_DIALOG_DESTROY_WITH_PARENT,
					    ok_mess, GTK_RESPONSE_OK,
					    NULL);
      break;
    case 2:
    default: 
      cancel_mess = buttons[1];
      if ( strcasecmp(cancel_mess,"Cancel")==0 ) cancel_mess = GTK_STOCK_CANCEL; 
      dialog = gtk_dialog_new_with_buttons ("Scilab Dialog",
					    GTK_WINDOW (window),
					    GTK_DIALOG_MODAL| GTK_DIALOG_DESTROY_WITH_PARENT,
					    ok_mess, GTK_RESPONSE_OK,
					    cancel_mess,  GTK_RESPONSE_CANCEL,
					    NULL);
      break;
    }

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_container_set_border_width (GTK_CONTAINER (hbox), 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, FALSE, FALSE, 0);
  gtk_widget_show (hbox);

  if ( n_buttons >= 2) 
    stock = gtk_image_new_from_stock (GTK_STOCK_DIALOG_QUESTION, GTK_ICON_SIZE_DIALOG);
  else
    stock = gtk_image_new_from_stock (GTK_STOCK_DIALOG_INFO, GTK_ICON_SIZE_DIALOG);
  gtk_box_pack_start (GTK_BOX (hbox), stock, FALSE, FALSE, 0);
  gtk_widget_show (stock);
  label = gtk_label_new (msg_utf8);
  gtk_box_pack_start (GTK_BOX (hbox),label, TRUE, TRUE, 0);
  gtk_widget_show (label);
  response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
  if ( alloc == TRUE) g_free (msg_utf8);
  if (response == GTK_RESPONSE_OK)
    return 1; 
  else 
    return 2; 
}
