/*-------------------------------------------------------------------
 * This Software is (Copyright ENPC 1998-2003) 
 * Jean-Philippe Chancelier Enpc/Cermics
 *-------------------------------------------------------------------*/

#include "nsp/menus.h"

typedef enum { pOK, pCANCEL ,MEMERR } state; 

static char **nsp_printer_list(int *n_printers);

/* table of export formats */ 

static int nF=6;   /* number of formats */

static char *nsp_export_formats[] = {
  "Postscript",
  "Postscript No Preamble",
  "Postscript-Latex",
  "Xfig",
  "Gif",
  "PPM",
  NULL
};

/******************************************
 * main function 
 ******************************************/

int nsp_print_dialog(int  *flag,char *printer, int  *colored, int  *orientation, char *file, int  *ok)
{ 
  static char **printers = NULL;
  static int firstentry=0,multiple_call=0, n_printers = 0;
  int rep ;
  if ( firstentry == 0) 
    {
      printers = nsp_printer_list(&n_printers);
      if ( printers == NULL) 
	{
	  Scierror("Running out of memory\n") ;
	  *ok=0;
	  return 0;
	}
      firstentry++;
    }
  /** multiple calls are forbidden **/
  if ( multiple_call == 1 )
    {
      Scierror("Can't raise the print menu: you must quit another raised menu before\n");
      *ok=0;
      return(0);
    }
  else
    {
      multiple_call = 1;
    }
  *ok=1;

  if ( *flag == 1 ) 
    {
      int answer=1;
      rep= nsp_menu_print(colored,orientation,printers,n_printers,&answer);
      if ( rep == TRUE ) 
	{
	  strcpy(printer,printers[answer-1]);
	}
    }
  else
    {
      char *filename=NULL;
      int answer=1;
      rep= nsp_menu_export (colored,orientation,nsp_export_formats,nF,&answer,&filename);
      if ( rep == TRUE ) 
	{
	  strcpy(printer,nsp_export_formats[answer-1]);
	  strcpy(file,filename);FREE(filename);
	}
    }
  *ok = (rep == TRUE ) ? TRUE : FALSE ;
  multiple_call =0;
  return(0);
}

/******************************************
 * Initialize list of printers 
 ******************************************/

static char **nsp_printer_list(int *n_printers)
{
  int n,i,npr,count=0;
  char *str,*p, **printer_list,*buffer;

  /* searching for printers */
  if ( (str=getenv("PRINTERS")) == 0) str="lp";
  n=strlen(str);
  if (n==0) 
    {
      str="lp";n=strlen(str);
    }
  /* counting number of printers */
  npr=1;
  for (i=0 ; str[i] != '\0' ;i++)
    if(str[i]==':' ) npr++;
  printer_list = (char **) MALLOC((npr+1)*sizeof(char *));
  buffer=(char *) MALLOC( (strlen(str)+1)*sizeof(char));

  if ( buffer == (char *) 0 || printer_list == (char **) 0) return NULL;
  strcpy(buffer,str);
  while ( count < npr ) 
    {
      p=(count == 0) ? strtok(buffer,":") : strtok((char *)0,":");
      printer_list[count++]=p;
    }
  printer_list[count]=NULL;
  FREE(buffer);
  return printer_list;
}
