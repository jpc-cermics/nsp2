/*------------------------------------------------------------------------
 *    Graphic library
 *    Copyright (C) 1998-2000 Enpc/Jean-Philippe Chancelier
 *    jpc@cereve.enpc.fr 
 --------------------------------------------------------------------------*/

#ifndef _SCI_ECH
#define _SCI_ECH

/*
 * Current scale values. 
 */

#include "perigen.h" 

extern window_scale_list current_scale;

/*
 * Current geometric transformation : from double to pixel 
 */

#define XScale_d(x)    ( current_scale.Wscx1*((x) -current_scale.frect[0]) + current_scale.Wxofset1)
#define XLogScale_d(x) ( current_scale.Wscx1*(log10(x) -current_scale.frect[0]) + current_scale.Wxofset1)
#define YScale_d(y)    ( current_scale.Wscy1*(-(y)+current_scale.frect[3]) + current_scale.Wyofset1)
#define YLogScale_d(y) ( current_scale.Wscy1*(-log10(y)+current_scale.frect[3]) + current_scale.Wyofset1)

#define XScale(x)    inint( XScale_d(x) )
#define XLogScale(x) inint( XLogScale_d(x))
#define YScale(y)    inint( YScale_d(y)   )
#define YLogScale(y) inint( YLogScale_d(y))
#define XDouble2Pixel(x) ((current_scale.logflag[0] == 'n') ? ( XScale(x)) : ( XLogScale(x)))
#define YDouble2Pixel(y) ((current_scale.logflag[1] == 'n') ? ( YScale(y)) : ( YLogScale(y)))

/*
 * Current geometric transformation : from pixel to double 
 */

#define XPi2R(x)  current_scale.frect[0] + (1.0/current_scale.Wscx1)*((x) - current_scale.Wxofset1)
#define YPi2R(y)  current_scale.frect[3] - (1.0/current_scale.Wscy1)*((y) - current_scale.Wyofset1)
#define XPi2LogR(x)  exp10( XPi2R(x))
#define YPi2LogR(y)  exp10( YPi2R(y))
#define XPixel2Double(x)  (( current_scale.logflag[0] == 'l') ? XPi2LogR(x) : XPi2R(x))
#define YPixel2Double(y)  (( current_scale.logflag[1] == 'l') ? YPi2LogR(y) : YPi2R(y))

/*
 * Current geometric transformation : 3D plots 
 */

#define TRX(x1,y1,z1) ( current_scale.m[0][0]*(x1) +current_scale.m[0][1]*(y1) +current_scale.m[0][2]*(z1))
#define TRY(x1,y1,z1) ( current_scale.m[1][0]*(x1) +current_scale.m[1][1]*(y1) +current_scale.m[1][2]*(z1))
#define TRZ(x1,y1,z1) ( current_scale.m[2][0]*(x1) +current_scale.m[2][1]*(y1) +current_scale.m[2][2]*(z1))
#define GEOX(x1,y1,z1)  XScale(TRX(x1,y1,z1))
#define GEOY(x1,y1,z1)  YScale(TRY(x1,y1,z1))

void show_scales(void);

#endif  /* _SCI_ECH */






