#ifndef SCI_IO_H 
#define SCI_IO_H 

/*********************************************************************
 * This Software is ( Copyright ENPC 1998-2003 )                          *
 * Jean-Philippe Chancelier Enpc/Cermics                            *
 *********************************************************************/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include "nsp/math.h"
#include "nsp/user-prefs.h"
#include "nsp/tokenizer.h"

typedef int (*IOVFun) (char *fmt, va_list ap);
extern IOVFun Scivprintf;
IOVFun SetScilabIO(IOVFun F);
int Sciprintf1(int indent,char *fmt,...);
int Sciprintf(char *fmt,...); 

#ifdef  NSP_TYPE_OBJECT 
NspObject *Sciprint2string_reset();
#endif 

int  Sciprint2string(char *fmt, va_list ap);
FILE * Sciprint_set_diary(FILE *f,int diary_echo);
extern int Sciprint_diary_on(void) ;
extern int Sciprint_diary(char *fmt, va_list ap);
extern int Sciprint_diary_only (char *fmt,...);
extern void Sciprint_file(FILE *f); 
extern int Sciprint2file(char *fmt, va_list ap); 

typedef int iofun (char *fmt,...);
typedef iofun *IOFun;

typedef int (*IOFun1) (int indent,char *fmt,...);

extern iofun Scierror ;

int  ParseError  (char *fmt,...);
int scidebug(int i,char *fmt,...);
typedef int (*IOFun2) (FILE *f,char *fmt,...);
extern IOFun2 Scifprintf ;
typedef int (*SciGetC) (void);
extern SciGetC Scigetchar ;
extern SciGetC SetScilabgetchar (SciGetC F);

extern void SciReadClean (void);

extern void set_echo_mode(int mode);
extern int get_echo_mode(void); 
extern void set_is_reading(int mode);
extern int get_is_reading(void);

extern void nsp_error_message_show(void);
extern void nsp_error_message_to_lasterror(void);
extern void nsp_error_message_clear(void);

extern int nsp_set_echo_input_line(int val);
extern int nsp_get_echo_input_line(void);

extern IOVFun nsp_error_vprintf;

extern void sci_get_screen_size (int *rows, int *cols);

extern void nsp_intialize_reader(void);


#endif 


