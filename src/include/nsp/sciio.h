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

typedef int (*IOVFun) (char *fmt, va_list ap);
extern IOVFun Scivprintf;
IOVFun SetScilabIO(IOVFun F);

int Sciprintf1 SCI_VARARGS_2DEF(int,indent,char *,fmt);
int Sciprintf SCI_VARARGS_DEF(char *,fmt); 

#ifdef  NSP_TYPE_OBJECT 
NspObject *Sciprint2string_reset();
#endif 

int  Sciprint2string(char *fmt, va_list ap);
void Sciprint_set_diary(FILE *f);
int Sciprint_diary(char *fmt, va_list ap);
int Sciprint_diary_only (char *fmt,...);
void Sciprint_file(FILE *f); 
int Sciprint2file(char *fmt, va_list ap); 

typedef int (*IOFun) (char *fmt,...);
typedef int (*IOFun1) (int indent,char *fmt,...);

extern IOFun Scierror ;
typedef void (*MoreFun) (int *n);
extern MoreFun scimore ;
extern void scimore_void(int *n);
MoreFun SetScilabMore(MoreFun F);

extern void C2F(scilines) (int *nl,int *nc);
int  ParseError  SCI_VARARGS_DEF(char *,fmt);
int scidebug SCI_VARARGS_2DEF(int,i,char *,fmt);
typedef int (*IOFun2) (FILE *f,char *fmt,...);
extern IOFun2 Scifprintf ;
typedef int (*SciGetC) (void);
extern SciGetC Scigetchar ;
extern SciGetC SetScilabgetchar (SciGetC F);


typedef void (*SciReadFunction) ( char *prompt,char *buffer, int *buf_size,int *len_line,int *eof);
extern void SciFileReadLine (char *prompt,char *buffer, int *buf_size,int *len_line,int *eof);
extern SciReadFunction SciReadLine1 ;
extern SciReadFunction SetSciReadFunction  (SciReadFunction F);
extern SciReadFunction SetSciReadFunctionDef();
extern FILE *SetSciInputFile ( FILE *file);

char *set_input_str(char *str); 
void SciStringReadLine(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof);

extern void SciReadClean (void);

extern void set_echo_mode(int mode);
extern int get_echo_mode(void); 
extern void set_is_reading(int mode);
extern int get_is_reading(void);

extern void DefSciReadLine(char *prompt, char *buffer, int *buf_size, int *len_line, int *eof);


#endif 


