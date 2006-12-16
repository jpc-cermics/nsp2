#ifndef SCI_FUNTAB
#define SCI_FUNTAB

/* Nsp
 * Copyright (C) 1998-2006 Jean-Philippe Chancelier Enpc/Cermics
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

extern int nsp_enter_function (const char *str,int Int,int Num);
extern void nsp_delete_function (const char *str);
extern void nsp_print_function_table(void);
extern int nsp_find_function(const char *str, int *Int, int *Num);
extern void nsp_init_function_table(void);
extern void nsp_delete_interface_functions(int Int);

#endif
