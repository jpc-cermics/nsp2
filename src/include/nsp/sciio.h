#ifndef NSP_INC_IO_H
#define NSP_INC_IO_H

/*
 * This Software is GPL (Copyright ENPC 1998-2019)
 * Jean-Philippe Chancelier Enpc/Cermics
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <nsp/objectf.h>
#include "nsp/math.h"
#include "nsp/user-prefs.h"
#include "nsp/tokenizer.h"

typedef int (*IOVFun) (const char *fmt, va_list ap);
extern IOVFun SetScilabIO(IOVFun F);
IOVFun nsp_set_error_io_basic();
IOVFun SetScilabErrorIO(IOVFun F);
extern int Sciprintf1(int indent,const char *fmt,...);
extern int Sciprintf2(int indent,const char *str,const char *fmt,...);
extern int Sciprintf(const char *fmt,...);
extern int do_printf_stdout(const FILE *ignore,const char *fmt, ...);
extern NspObject *Sciprint2string_reset();

extern int  Sciprint2string(const char *fmt, va_list ap);
extern FILE * Sciprint_set_diary(FILE *f,int diary_echo_out, int diary_echo_exprs );
extern int Sciprint_diary_on(void) ;
extern int Sciprint_diary(const char *fmt, va_list ap);
extern int Sciprint_diary_only (const char *fmt,...);
extern FILE *Sciprint_file(FILE *f);
extern int Sciprint2file(const char *fmt, va_list ap);

typedef int iofun (const char *fmt,...);
typedef iofun *IOFun;
typedef int (*IOFun1) (int indent,const char *fmt,...);

extern iofun Scierror ;

extern int  ParseError  (char *fmt,...);
extern int scidebug(int indent,char *fmt,...);
typedef int (*IOFun2) (FILE *f,const char *fmt,...);
extern IOFun2 Scifprintf ;
typedef int (*SciGetC) (void);
extern SciGetC Scigetchar ;
extern SciGetC nsp_set_nsp_getchar(SciGetC F);
extern void nsp_read_clean_after_ctrc(void);
extern void nsp_readline_clear_line(void);
extern void set_echo_mode(int mode);
extern int get_echo_mode(void);
extern void set_is_reading(int mode);
extern int get_is_reading(void);
extern void nsp_set_in_text_view(int value);
extern int nsp_get_in_text_view(void);

extern void nsp_error_message_show(void);
extern void nsp_error_message_to_lasterror(void);
extern void nsp_error_message_clear(void);

extern int nsp_set_echo_input_line(int val);
extern int nsp_get_echo_input_line(void);

extern IOVFun nsp_error_vprintf;

extern void sci_get_screen_size (int *rows, int *cols);
extern void nsp_intialize_reader(void);
extern int nsp_from_texmacs(void);

#endif
