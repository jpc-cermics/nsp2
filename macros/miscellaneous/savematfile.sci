function savematfile(varargin)
// Save variables in a Matlab binary or ASCII file format.
// This function has been developped following the 'MAT-File Format' description:
// www.mathworks.com/access/helpdesk/help/pdf_doc/matlab/matfile_format.pdf 
// Copyright INRIA
// Authors: SS, VC
// ported to nsp by jpc 
// zlib compression added by jpc 

  function savematfile_compress_miMatrix(fd,value,ArrayName)
  // save a miMatrix in a compressed way 
  // save first in a string buffer 
  // jpc 
    
    S=sopen(512);
    savematfile_miMatrix(S,value,ArrayName)
    nb = S.tell[];; // bytes to be compressed 
    D1 = S.compress[nb]; // compress nb bytes contained in S;
    nbc=D1.length[];  // bytes after compression;
    fd.put[miCompressed,type=md_i];
    fd.put[nbc,type=md_i];
    c=D1.get[n=nbc,type='c'];
    fd.put[c,type='c'];
  endfunction

  function savematfile_miMatrix(fd,value,ArrayName)
  // Save variables in a Matlab binary file
  // This function has been developped following the 'MAT-File Format' description:
  // www.mathworks.com/access/helpdesk/help/pdf_doc/matlab/matfile_format.pdf 
  // Copyright INRIA
  // Authors: VC
  // Nsp port and changes jpc 
  // 
  //printf("<savematfile_miMatrix %d:\n",fd.tell[]);
    TagSize=8; // 8 bytes
    ArrayFlagSize=8; // 8 bytes
    // Position is saved to come back here after writing
    SavePosBefore=fd.tell[];
    // Save space for TAG : WriteEmptyTag(fd);
    fd.put[zeros(1,TagSize),type='uc'];
    // Position is saved to compute number of written bytes
    NumberOfBytes=fd.tell[];
    // Save space for ARRAY FLAGS 
    fd.put[zeros(1,ArrayFlagSize+TagSize),type='uc'];
    // Compute array dimensions
    if type(value,'short')=='s' then
      if size(value,"*")==1
	// save a string 
	savematfile_dim_array(fd,[1,length(value)]);
      else
	// save string matrix as cell 
	savematfile_dim_array(fd,[1,size(value,'*')]);
      end
    elseif type(value,'short')=='h' then 
      savematfile_dim_array(fd,[1,1]);
    else 
      savematfile_dim_array(fd,size(value));
    end
    // Write variable name
    savematfile_array_name(fd,ArrayName);
    Flags=[0 0 0];

    if type(value,'short')=='m' then 
      // matrices 
      // ------------
      value=matrix(value,1,-1);
      Flags(1)=b2m(~isreal(value));
      Class=DoubleClass;
      NnzMax=0;
      savematfile_simple_elt(fd,real(value),miDOUBLE);
      if Flags(1) then
	savematfile_simple_elt(fd,imag(value),miDOUBLE);
      end
    elseif type(value,'short')=='i' then 
      // matrices 
      // ------------
      value=matrix(value,1,-1);
      Flags(1)=0;
      Class=DoubleClass;
      NnzMax=0;
      itype= value.itype[];
      table=hash_create(int=miINT32,uint=miUINT32,short=miINT16,ushort=miUINT16,long=miINT32,...	
			ulong=miUINT32,int8=miINT8,uint8= miUINT8,int16=miINT16,  ...
			uint16=miUINT16,int32=miINT32,uint32=miUINT32,int64=miINT64, ...       
			uint64=miUINT64);
      // Pbs with 64 types 
      ctable=hash_create(int=Int32Class,uint=Uint32Class,short=Int16Class,ushort=Uint16Class,...
			 long=Int32Class, ulong=Uint32Class,int8=Int8Class,uint8=Uint8Class,...
			 int16=Int16Class, uint16=Uint16Class,int32=Int32Class,uint32=Uint32Class,...
			 int64=DoubleClass, ...       
			 uint64=DoubleClass);
      Class = ctable(itype)
      savematfile_simple_elt(fd,i2m(value),table(itype));
    elseif type(value,'short')=='b' then 
      // boolean matrices 
      // ------------
      value=matrix(value,1,-1);
      Flags(1)=0; // not complex 
      Flags(3)=1; // flag for logical 
      //Class=DoubleClass;
      Class=Uint8Class;
      NnzMax=0;
      savematfile_simple_elt(fd,b2m(value),miUINT8);
    elseif type(value,'short')=='s' then 
      // string matrices 
      //------------------
      if size(value,"*")==1 then
	// 1x1 -> mtlb string 
	value=matrix(ascii(value),1,-1);
	Flags(1)=0;
	Class=CharClass;
	NnzMax=0;
	savematfile_simple_elt(fd,value,miUINT16);
      else
	// mxn -> mtlb mxn cell of strings
	printf("Nsp string matrix is saved as a Matlab Cell\n");
	sz=size(value);
	value=matrix(value,1,-1);
	entries={}
	for k=1:size(value,'*')
	  entries{k}=value(k);
	end
	entries.redim[sz(1),sz(2)];
	value=entries;
	fd.seek[SavePosBefore];
	savematfile_miMatrix(fd,value,ArrayName);
	return
      end
    elseif type(value,'short')=='h' then 
      // hash table to 1x1 struct 
      //------------------------
      Flags(1)=0;
      NnzMax=0;
      Class=StructClass;
      Fnams=value.__keys;
      // + 1 to be sure that Names are null terminated. 
      FieldNameLength=max(length(Fnams))+1;
      if FieldNameLength >= 32 then 
	printf("name length is limited to 31 characters\n");
      end
      // FieldNameLength=32;
      //printf("<WriteFieldNameLength\n");
      savematfile_simple_elt(fd,FieldNameLength,miINT32);
      //printf("  /WriteFieldNameLength>\n");
      NumberOfFields=size(Fnams,'*');
      FieldNames=zeros(FieldNameLength,NumberOfFields);
      for k=1:NumberOfFields
	str =Fnams(k);
	FieldNames(1:length(str),k)=ascii(str)';
      end
      FieldNames.redim[1,-1];
      savematfile_simple_elt(fd,FieldNames,miINT8);
      for k=1:NumberOfFields
	savematfile_miMatrix(fd,value(Fnams(k)),"");
      end
    elseif type(value,'short')=='ce' then 
      // cells 
      //------
      Flags(1)=0;
      NnzMax=0;
      Class=CellClass;
      for k=1:length(value)
	savematfile_miMatrix(fd,value{k},"c");
      end
    elseif type(value,'short')=='sp' then 
      // sparse
      //-------
      Class=SparseClass;
      NnzMax=nnz(value);
      [Ir,Jc,v]=spget_mtlb(value);
      savematfile_simple_elt(fd,Jc,miINT32);
      savematfile_simple_elt(fd,Ir,miINT32);
      Flags(1)=b2m(~isreal(v));
      savematfile_simple_elt(fd,real(v),miDOUBLE);
      if Flags(1) then
	savematfile_simple_elt(fd,imag(v),miDOUBLE);
      end
    else
      // unknown 
      //--------
      printf("savematfile: %s of type ''%s'' is ignored\n",...
	     value.get_name[], type(value,'short'));
      fd.seek[SavePosBefore];
      return;
    end
    
    SavePosAfter=fd.tell[];
    NumberOfBytes=SavePosAfter-NumberOfBytes
    // Update tag
    savematfile_tag(fd,miMatrix,NumberOfBytes);
    fd.seek[SavePosBefore+TagSize+TagSize+ArrayFlagSize];
    // Update array flags
    savematfile_array_flags(fd,Flags,Class,NnzMax);
    fd.seek[SavePosAfter];
    //printf("   /savematfile_miMatrix>%d\n",fd.tell[]);
  endfunction
  
  function endian=savematfile_header(fd)
  // Copyright INRIA
  // Write the mat file header informations
  // VC
    head="MATLAB 5.0 MAT-file, Generated by Nsp"
    head=head+part(" ",1:(124-length(head)));
    fd.put[ascii(head),type='uc'];
    // We are supposed here to use the native endian 
    // of the machine but you can force 'l' or 'b' 
    // just for testing 
    if is_little_endian() then 
      endian='l';
    else 
      endian='b';
    end
    fd.put[0x0100,type='us'+endian];
    // write a tag to fix endian
    // we force little_endian
    ei=ascii(["MI"]);
    fd.put[ishift(ei(1),8)+ei(2),type='us'+endian];
  endfunction

  function savematfile_array_flags(fd,Flags,Class,NnzMax)
  // Copyright INRIA
  // Write an array flag
  // VC
    savematfile_tag(fd,miUINT32,ArrayFlagSize);
    // XXX 
    //  fd.seek[fd.tell[]-ArrayFlagSize];
    xx=fd.tell[];
    fd.seek[xx-ArrayFlagSize];
    Flags=[0 Flags(3:-1:1)];
    B=[0 0 0 0];
    B(3)=Flags* 2.^(0:3)';
    B(4)=Class;
    // just to conform to the read !!
    if endian== 'l' then B=B(4:-1:1);end 
    //printf("<savematfile_array_flags %d:[",fd.tell[]);
    fd.put[B,type="uc"];
    fd.put[NnzMax,type=md_i];
    //printf(" uc=[%d,%d,%d,%d] NnzMax=%d]>:\n",B(1),B(2),B(3),B(4),NnzMax);
  endfunction

  function savematfile_dim_array(fd,dims)
  // Copyright INRIA
  // Write dimensions of an array
  // VC
  //printf("<WriteDimensions %d:[\n",fd.tell[]);
    savematfile_simple_elt(fd,dims,miINT32);
    //printf("  /WriteDimensions>%d:[\n",fd.tell[]);
  endfunction

  function savematfile_array_name(fd,ArrayName)
  // Copyright INRIA
  // Write name of an array
  // VC
  //printf("<savematfile_array_name %d:[\n",fd.tell[]);
    savematfile_simple_elt(fd,ascii(ArrayName),miINT8);
  endfunction

  function savematfile_tag(fd,DataType,NumberOfBytes)
  // Copyright INRIA
  // Write a tag
  // VC 
  // Correction jpc the compressed case was not good 
    SavePos=fd.tell[];
    isCompressed=NumberOfBytes<=4 & NumberOfBytes > 0;
    dt=mtlb_data_type_table(DataType);
    if isempty(dt) then str="unknown" else str=dt{1};end   
    if isCompressed then
      fd.seek[SavePos-NumberOfBytes-TagSize/2];
      //printf("<savematfile_tag %d:[bytes=%d,data=%s,compression=1]",fd.tell[],...
      //NumberOfBytes,str);
      fd.put[ishift(NumberOfBytes,16)+DataType,type=md_i];
      //printf(">%d\n",fd.tell[])
    else
      fd.seek[SavePos-NumberOfBytes-TagSize];
      //printf("<savematfile_tag %d:[bytes=%d,data=%s,compression=0]",fd.tell[],...
      //NumberOfBytes,str);
      fd.put[DataType,type=md_i];
      fd.put[NumberOfBytes,type=md_i];
      //printf(">%d\n",fd.tell[])
    end
    fd.seek[SavePos];
  endfunction

  function savematfile_simple_elt(fd,value,DataType)
  // Copyright INRIA
  // Write an element in file
  // VC
    if DataType==miDOUBLE & and(int(value)==value) then
      minv=min(value);maxv=max(value);
      // If data is of double type but made of integer values 
      // then it is writen in an INT* format to save space
      // we follow the matlan choices 
      if minv>=0 & maxv<=255 then       DataType=miUINT8;
      elseif minv>=-128 & maxv<=127 then   DataType=miINT16;// not miINT8
      elseif minv>=0 & maxv<=65535 then       DataType=miUINT16;
      elseif minv>=-32768 & maxv<=32767 then      DataType=miINT16;
      elseif minv>=0 & maxv<=4294967295 then      DataType=miDOUBLE;// not miUINT32
      elseif minv>=-2147483648 & maxv<=2147483647 then  DataType=miINT32;
      end
      //printf("DataType used %f\n",DataType);
    end

    // write an empty tag. 
    tag_start = fd.tell[];
    fd.put[zeros(1,TagSize),type='uc'];
    
    y=mtlb_data_type_table(DataType)
    if isempty(y) then 
      error("DataType: "+string(DataType)+"is not implemented");
    end
    NumberOfBytes=length(value)*y{2}
    fmt=y{3};
    
    isCompressed=NumberOfBytes<=4 & NumberOfBytes > 0;
    if %t & isCompressed then
      xx=fd.tell[];
      fd.seek[xx-TagSize/2];
    end
    // write the values 
    fd.put[value,type=fmt];
    // go back to write the tag part
    savematfile_tag(fd,DataType,NumberOfBytes);
    // add padding to end on multiple of 8. 
    np=modulo(8-modulo(fd.tell[],8),8);
    fd.put[zeros(1,np),type="uc"] ;
    // end 
    //printf("<savematfile_simple_elt: %d:[size=%d,(number,byte)=%dx%d, type=%s, compressed=%d]%d\n", ...
    // tag_start,NumberOfBytes,length(value),y{2},y{1},miCompressed,fd.tell[]);
  endfunction

  function y=mtlb_data_type_table(DataType)
  // jpc Copyright Enpc 
  // { name, bytesize, format }
    mtlb_datatype={"miINT8",1,"c";
		   "miUINT8",1,"uc";
		   "miINT16",2,md_s;
		   "miUINT16",2,"u"+md_s;
		   "miINT32",4,md_i;
		   "miUINT32",4,"u"+md_i;
		   "miSINGLE",-1,"void";
		   "Reserved",-1,"void";
		   "miDOUBLE",8,md_d;
		   "Reserved",-1,"void";
		   "Reserved",-1,"void";
		   "miINT64",16,"void";
		   "miUINT64",16,"void";
		   "miMATRIX",-1,"void";
		   "miCOMPRESSED",-1,"void";
		   "miUTF8",-1,"void";
		   "miUTF16",-1,"void";
		   "miUTF32",-1,"void"};
    if DataType > size(mtlb_datatype,1) | DataType <= 0 then 
      y={};
    else
      y= mtlb_datatype(DataType,:);
    end
  endfunction

  function loadmatfile_mat_consts()
  //set constants. This function should be exec'ed
  //Copyright INRIA
  //Author Serge Steer  
    miINT8=1
    miUINT8=2
    miINT16=3
    miUINT16=4
    miINT32=5
    miUINT32=6
    miSINGLE=7
    miRESERVE1=8
    miDOUBLE=9
    miRESERVE2=10
    miRESERVE3=11
    miINT64=12
    miUINT64=13
    miMatrix=14
    miCompressed=15
    miUTF8 = 16	
    miUTF16 = 17	
    miUTF32 = 18	
    // 
    CellClass=1
    StructClass=2
    ObjectClass=3
    CharClass=4
    SparseClass=5
    DoubleClass=6
    SingleClass=7
    Int8Class=8
    Uint8Class=9
    Int16Class=10
    Uint16Class=11
    Int32Class=12
    Uint32Class=13
    //
    //--set various reading format
    md_i='i'+endian;md_d='d'+endian;md_s='s'+endian;md_l='l'+endian;md_f='f'+endian;
    //
  endfunction


  
  vars=who('caller',hash=%t);
  for k=1:size(varargin)
    if type(varargin(k),'short')<>'s' then
      error("All inputs must be character strings");
    end
  end
  
  // Verify that all inputs are character strings

  mtlb_opts=[]; // Options for ASCII format
  mtlb_thefile=[]; // Name of file to write
  mtlb_names=[]; // Variable names to save
  version=[]; // MAT-file version (4 or 6, miCOMPRESSED not yet implemented)
  bin=%t; // %t is binary file %f if ASCII file


  // Default format is binary
  if nargin==1 then
    bin=%t;
  end

  // Sort all inputs (Options/Names/Filename)
  k=1
  while k <= length(varargin)
    // All options are converted to lower case
    if part(varargin(k),1)=="-" then
      varargin(k)=tolower(varargin(k));
    end
    
    select varargin(k)
     case "-append"
      printf("Option -append not implemented: IGNORED\n")
      k=k+1
     case "-mat"
      bin=%t
      k=k+1
     case "-l_endian"
      force_endian='l';
      k=k+1
     case "-b_endian"
      force_endian='b';
      k=k+1
     case "-ascii"
      mtlb_opts=[mtlb_opts varargin(k)];
      bin=%f
      k=k+1
     case "-struct"
      k=k+1;
      stname=varargin(k);
      k=k+1;
      // Verify if one or more field name is/are given
      if k<=lstsize(varargin) & part(varargin(k),1)<>"-" & mtlb_thefile<>"" then // struct field
	while k<=lstsize(varargin) & part(varargin(k),1)<>"-"
	  // Add field to variable names
	  mtlb_names=[mtlb_names;varargin(k)]; 
	  execstr(varargin(k)+"="+stname+"(mtlb_names($))");
	  k=k+1;
	end
      else // All vars(1)=[];fields have to be saved
	fields=getfield(1,evstr(stname));
	fields(1:2)=[]
	for kk=fields
	  mtlb_names=[mtlb_names;kk];
	  execstr(kk+"="+stname+"(mtlb_names($))");
	end
      end
     case "-v4"
      version=4;
      bin=%t;
      k=k+1
     case "-v6"
      version=6;
      bin=%t;
      k=k+1
     case "-v7"
      version=7;
      bin=%t;
      k=k+1;
     case "-tabs"
      bin=%f;
      mtlb_opts=[mtlb_opts varargin(k)];
      k=k+1
     case "-double"
      bin=%f;
      mtlb_opts=[mtlb_opts varargin(k)];
      k=k+1
     case "-regexp"
      printf("Option -regexp not implemented: IGNORED\n")
      while k<=lstsize(varargin) & and(varargin(k)<>["-mat","-ascii"])
	k=k+1
      end
    else 
      if isempty(mtlb_thefile) then // Filename
	mtlb_thefile=varargin(k)
	if file('extension',mtlb_thefile)==".mat" && isempty(bin) then 
	  // extension .mat and bin not already fixed by options
	  bin=%t
	end
      else // Variable names
	mtlb_names=[mtlb_names;varargin(k)]
      end
      k=k+1
    end
  end


  if isempty(version) & bin then
    // Default version 6 for binary files
    version=6;
    //printf("Option -v6 added\n");
  end
  
  // If no name given then all workspace saved
  if isempty(mtlb_names) then
    mtlb_names=vars.__keys;
  end

  // If binary format and no extension for filename, .mat is added
  if bin & isempty(strindex(mtlb_thefile,".")) then
    mtlb_thefile=mtlb_thefile+".mat"
  end

  // Do not handle function redefinition
  // funcprot(0);

  // Binary save
  if bin then
    if version==4 then
      // LEVEL 4 MAT-file removed 
      error("Version "+string(version)+" MAT-file not implemented");
    elseif version==6 || version == 7 then
      // Open file for writing
      mtlb_thefile=stripblanks(mtlb_thefile)
      mtlb_fd= fopen(mtlb_thefile,mode='w+b',swap=%f)
      // Write header and get endian 
      endian=savematfile_header(mtlb_fd);
      //--set constants (this will use endian !)
      exec(loadmatfile_mat_consts);
      // Write variables as miMATRIX data type
      for k=1:size(mtlb_names,"*")
	if vars.iskey[mtlb_names(k)] then 
	  if version == 7 then 
	    savematfile_compress_miMatrix(mtlb_fd,vars(mtlb_names(k)), mtlb_names(k));
	  else
	    savematfile_miMatrix(mtlb_fd,vars(mtlb_names(k)), mtlb_names(k));
	  end
	else
	  printf('variable %s is not found and ignored in savematfile\n",mtlb_names(k));
	end
      end
      mtlb_fd.close[];
    else
      // This part should contain miCOMPRESSED data type handling
      error("Version "+string(version)+" MAT-file not implemented");
      return;
    end
    // ASCII save
  else
    // The end of this function has been adapted from mtlb_save.sci 
    // Matlab 5 types are not saved (structs...)
    // for k=size(mtlb_names,"*"):-1:1
    //       execstr("x="+mtlb_names(k))
    //       if and(type(x)<>[1 4 5 6 10]) then
    // 	printf("Variable "+mtlb_names(k)+" cannot be saved in ASCII file: IGNORED\n");
    // 	mtlb_names(k)=[]
    //       end
    //     end
    if ~isempty(mtlb_opts) & ~isempty(strindex("-tabs",mtlb_opts)) then
      sep="\t"
    else
      sep=" "
    end
    if size(mtlb_opts,"*")==1 then //8 digits save
      mtlb_fmt="(2x,1pe14.7"+sep+")"
    else
      mtlb_fmt="(2x,1pe23.15"+sep+")"
    end

    mtlb_fd=fopen(mtlb_thefile,mode="w");
    // Clear variable wich are no more used to avoid name conflicts
    for mtlb_k=1:size(mtlb_names,"*")
      // perform changes on variables
      if vars.iskey[mtlb_names(mtlb_k)] then 
	x = vars(mtlb_names(mtlb_k));
      else
	printf('variable %s is not found and ignored in savematfile\n",mtlb_names(k));
      end
      select type(x,'short')
       case 'm' then
	mtlb_fd.put_matrix[real(x)]// ,"("+string(size(x,2))+mtlb_fmt+")")
       case 'b' then
	mtlb_fd.put_matrix[b2m(x)]// ,"("+string(size(x,2))+mtlb_fmt+")")
       case 'sp' then
	[ij,x]=spget(real(x));x=[ij x];
	mtlb_fd.put_matrix[real(x)]// ,"("+string(size(x,2))+mtlb_fmt+")")
       case 'spbool' then
	[ij,x]=spget(bool2s(x));x=[ij x];
	write(mtlb_fd,real(x),"(2f8.0,1x"+string(size(x,2))+mtlb_fmt+")")
       case 's' then
	x=part(x(:),1:max(length(x)))
	x1=[]
	for l=1:size(x,1)
	  x1=[x1;ascii(x(l))]
	end
	mtlb_fd.put_matrix[x1]; // "("+string(size(x1,2))+mtlb_fmt+")")
      end
    end
    mtlb_fd.close[];
  end
endfunction

