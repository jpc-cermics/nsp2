function [y]=intdec(x,lom)
// Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
// Copyright (C) 1987-2016 - C. Bunks (INRIA)
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
  
//y=intdec(x,lom)
//Changes the sampling rate of a 1D or 2D signal by the rates in lom
//  x      :Input sampled signal
//  lom    :For a 1D signal this is a scalar which gives the rate change
//         :For a 2D signal this is a 2-Vector of sampling rate
//         :changes lom=(col rate change,row rate change)
//  y      :Output sampled signal
//  Author: C. Bunks  date: 8 August 1990

//Get dimensions of vectors
 
   xsize=size(x);
   xmin=min(x);xmax=max(x);
   if xsize(1)==1 then lom=[1 lom]; end
   if xsize(2)==1 then lom=[lom 1]; end
 
//compute sampling rate change as ratio of two integers
 
   [l,m]=rat(lom);
 
//Assuming that the signal length is N (=xsize)
//the interpolated signal is N*l and the decimated
//signal is N*l/m.  The resulting output will have
//length int(N*l/m).
 
   xlsize=xsize.*l;
   xmsize=int(xlsize./m);
 
//Since the location of %pi in the frequency domain
//falls on a sample point for N even and between two
//sample points for N odd care must be taken to differentiate
//between the two cases in the following manipulations.
 
   leven=2*(int(xsize/2)-xsize/2)+ones_deprecated(xsize);
   meven=2*(int(xmsize/2)-xmsize/2)+ones_deprecated(xmsize);
 
//The position of %pi for the Fourier transform of the
//original signal is different for odd and even length signals.
//For an even length signal %pi is at the (N/2)+1 sample and
//for an odd length signal %pi is between the (N+1)/2 and the
//(N+1)/2 + 1 samples.
 
   fp=int(xsize/2)+ones_deprecated(xsize);
   fpc=xsize-fp+leven;
 
   fm=int(xmsize/2)+ones_deprecated(xmsize);
   fmc=fm-ones_deprecated(fm)-meven;
 
//If the input is a constant then don't do the work
 
   if xmax==xmin then
      y=xmax*ones_deprecated(xmsize(1),xmsize(2));
   else
 
//For interpolation we, theoretically, would upsample x by putting l-1
//zeroes between each sample and then low pass filter at w=%pi.
//However, this corresponds to cutting the Fourier transform of the
//original signal into two parts at w=%pi and inserting l times the
//length of the signal in zeroes.
//
//For decimation we, theoretically, would low pass filter at the
//Nyquist frequency and then remove m-1 samples out of m of the
//time domain signal.  However, this corresponds to saving the
//N/m points of the signal at the two ends of the Fourier transform
//and then inverse transforming.
 
//Fourier transform the signal
 
      xf=fft(x);
 
//Re-assemble the correct portions of the frequency domain signal
 
      if fm(1)<fp(1) then//reduce row length (decimation)
         xlf=[xf(1:fm(1),:);xf(xsize(1)-fmc(1)+1:xsize(1),:)];
      else
         if xmsize(1)==xsize(1) then//no changes in row length
            xlf=xf;
         else//increase row length (interpolation)
            xlf=[xf(1:fp(1),:);...
                 0*ones(xmsize(1)-fpc(1)-fp(1),xsize(2));...
                 xf(xsize(1)-fpc(1)+1:xsize(1),:)];
         end
      end
      if fm(2)<fp(2) then//decrease column length (decimation)
         xlf=[xlf(:,1:fm(2)),xlf(:,xsize(2)-fmc(2)+1:xsize(2))];
      else
         if xmsize(2)==xsize(2) then//no changes in column length
            xlf=xlf;
         else //increase column length (interpolation)
            xlf=[xlf(:,1:fp(2)),...
                 0*ones(xmsize(1),xmsize(2)-fpc(2)-fp(2)),...
                 xlf(:,xsize(2)-fpc(2)+1:xsize(2))];
         end
      end
 
//recuperate new signal and rescale
 
      y=real(ifft(xlf));
      y=prod(lom)*y;
 
    end
endfunction
