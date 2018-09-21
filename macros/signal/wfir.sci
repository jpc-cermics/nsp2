function [wft,wfm,fr]=wfir(ftype,forder,cfreq,wtype,fpar)
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
  
  //[wft,wfm,fr]=wfir(ftype,forder,cfreq,wtype,fpar)
  //Macro which makes linear-phase, FIR low-pass, band-pass,
  //high-pass, and stop-band filters
  //using the windowing technique.
  //Works interactively if called with no arguments.
  //  ftype  :Filter type ('lp','hp','bp','sb')
  //  forder :Filter order (pos integer)(odd for ftype='hp' or 'sb')
  //  cfreq  :2-vector of cutoff frequencies (0<cfreq(1),cfreq(2)<.5)
  //         :only cfreq(1) is used when ftype='lp' or 'hp'
  //  wtype  :Window type ('re','tr','hm','hn','kr','ch')
  //  fpar   :2-vector of window parameters
  //         :     Kaiser window: fpar(1)>0 fpar(2)=0
  //         :     Chebyshev window: fpar(1)>0 fpar(2)<0 or
  //         :                       fpar(1)<0 0<fpar(2)<.5
  //  wft    :Time domain filter coefficients
  //  wfm    :Frequency domain filter response on the grid fr
  //  fr     :Frequency grid
  // Author: C. Bunks  date: 12 March 1988
  // 
  wft=[];wfm=[];fr=[]
  //check arguments of macro call
  
  
  //if macro called with no arguments query user for values
  
  if nargin<=0 then
    
    //Query user for filter type and filter length
    nc=x_choose(['low pass';
		 'high pass';
		 'band pass';
		 'stop band'],'Choose type of filter to be designed')
    //Select filter type and Query user for cut-off frequencies
    flag=0;
    select nc
      case 1 then //low pass
	  [ok,fl,forder]=getvalue('Input filter characteristics',...
				  ['cut-off frequency (0.<frequ<.5):';
				   'filter length'],...
				  list('vec',1,'vec',1),[' ';' '])
	  ftype='lp'
	  fh=0;
      case 2 then //high pass
	  [ok,fl,forder]=getvalue('Input filter characteristics',...
				  ['cut-off frequency (0.<frequ<.5):';
				   'filter length (odd value)'],...
				  list('vec',1,'vec',1),[' ';' '])
	  fh=0;
	  flag=1;
	  ftype='hp'
      case 3 then //band pass
	  [ok,fl,fh,forder]=getvalue('Input filter characteristics',...
				     ['low cut-off frequency (0.<flow<.5):';
				      'high cut-off frequency (0.<flow<fhi<.5):';
				      'filter length'],...
				     list('vec',1,'vec',1,'vec',1),[' ';' ';' '])
	  ftype='bp'	
      case 4 then //stop band
	  [ok,fl,fh,forder]=getvalue('Input filter characteristics',...
				     ['low cut-off frequency (0.<flow<.5):';
				      'high cut-off frequency (0.<flow<fhi<.5):';
				      'filter length (odd value)'],...
				     list('vec',1,'vec',1,'vec',1),[' ';' ';' '])
	  flag=1;
	  ftype='sb'	
      else
	return
    end
    
    if flag==1 then
      if forder-2*int(forder/2)==0 then
	x_message(['Even length high pass and stop band filters not allowed';
		   '---Filter order is being incremented by 1'])
	forder=forder+1;
      end
    end
    
    //Query user for window type and window parameters
    nc=x_choose(['Kaiser';
		 'Chebyshev';
		 'Rectangular';
		 'Triangular';
		 'Hamming' ],'Input window type')
    select nc
      case 1 then
	wtype='kr'
	[ok,Beta]=getvalue('Input window characteristics',...
			   ['beta>0'],list('vec',1),' ')
	fpar(1)=Beta
	fpar(2)=0;
      case 2 then
	wtype='ch' 
	[ok,name,value]=getvalue(['Input window characteristics:';
				  ' ';
				  'dp (dp>0)   : the maximum value of the window side-lobe height';
				  'df (0<df<.5): the width of the window main lobe'
				  ' ';
				  'only one of this two values is to be defined,'
				  'the other one is automaticaly deduced'],...
				 ['name of specified value';
				  'value'],list('str',-1,'vec',1),['dp','0.3'])
	if part(name,1:2)=='dp' then
	  fpar=[value,-1]
	elseif part(name,1:2)=='df' then
	  fpar=[-1,value]
	else
	  x_message('Incorrect parameter name entered')
	  return
	end
      case 3 then
	wtype='re'
	fpar=[0 0];
      case 4 then
	wtype='tr'
	fpar=[0 0];
      case  5 then
	wtype='hm'
	fpar=[0 0];
      case  6 then
	wtype='hn'
	fpar=[0 0];
      else
	return
    end
  else
    fl=cfreq(1);
    fh=cfreq(2);
  end
  
  //Calculate window coefficients

  [win_l,cwp]=window(wtype,forder,fpar);
  [dummy,forder]=size(win_l);
  
  //Get forder samples of the appropriate filter type
  
  hfilt=ffilt(ftype,forder,fl,fh);
  
  //Multiply window with sinc function
  
  wft=win_l.*hfilt;
  
  //Calculate frequency response of the windowed filter
  
  [wfm,fr]=frmag(wft,256);
endfunction
