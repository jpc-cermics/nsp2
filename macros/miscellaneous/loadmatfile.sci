function loadmatfile(varargin)
// Loads variables in a Matlab binary or ASCII file into Nsp.
// This function has been developped following the 'MAT-File Format' description:
// www.mathworks.com/access/helpdesk/help/pdf_doc/matlab/matfile_format.pdf 
// Copyright INRIA
// Authors: SS, VC
// ported and modified for Nsp by jpc.
//
// Verify that all inputs are character strings

  function [head,version,swap]=loadmatfile_header(fd)
  //get the mat file header informations
  //Copyright INRIA
  //Author Serge Steer  
    head=ascii(fd.get[n=124,type='uc']);
    version=fd.get[n=2,type='uc'];
    //Magic number endian coding
    IM_MI=fd.get[n=2,type='uc'];
    if and(IM_MI==[73,77]) then // little endian file
      swap='l'
    elseif and(IM_MI==[77,73]) then // big endian file
      swap='b'
    else
      fd.close[];
      // This line has to be mofified according to message in 'loadmatfile' function
      error('Invalid level 5 binary MAT-file!') 
    end
  endfunction

  
  function [value,ArrayName]=loadmatfile_mimatrix(fd)
  // Read a variable in a Matlab binary file
  // This function has been developped following the 'MAT-File Format' description:
  // www.mathworks.com/access/helpdesk/help/pdf_doc/matlab/matfile_format.pdf 
  // Copyright INRIA
  // Authors: SS, VC
  // Uncompress data on the fly: jpc 2009.

    [DataType,NumberOfBytes,Compressed]=loadmatfile_tag(fd);
    if fd.eof[] then value=[],ArrayName="",return,end
    
    if DataType == miCompressed then 
      // Data is miCompressed 
      // we decompress the header first to obtain
      // the required size for decompression.
      // 
      nc = NumberOfBytes;
      c = fd.get[n=nc,type='c'];
      D = sopen(nc);
      D.put[c,type='c'];
      // uncompress with a buffer large enough for 
      // the header only. Note that the uncompression 
      // fails but we have the proper first bytes.
      D1=D.uncompress[24];
      fd=D1;
      [DataType,NumberOfBytes,Compressed]=loadmatfile_tag(fd);
      // A new buffer for decompression 
      // we have now the correct size to be used 
      D = sopen(nc);
      D.put[c,type='c'];
      D1=D.uncompress[NumberOfBytes+8];
      fd= D1;
      [DataType,NumberOfBytes,Compressed]=loadmatfile_tag(fd);
    end
    
    if DataType<>miMatrix then 
      error('Found Datatype='+string(DataType)+', expecting '+ ...
	    string(miMatrix))
    end
    if NumberOfBytes==0 then value=[],return,end
    [Flags,Class,NnzMax]=loadmatfile_array_flags(fd);
    // Flags = [type,global, logical]
    DimensionArray=loadmatfile_dim_array(fd);
    ArrayName=loadmatfile_array_name(fd)
    select Class
     case DoubleClass
      value=double_void(loadmatfile_simple_elt(fd,prod(DimensionArray),Class))
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case SingleClass
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class)
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case Int8Class
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class);
      value=m2i(value,'int8');
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case Uint8Class
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class);
      if Flags(3) then 
	value = m2b(value);
      else
	value=m2i(value,'uint8');
      end 
      if Flags(1) then 
	cvalue= loadmatfile_simple_elt(fd,prod(DimensionArray));
	if Flags(3) then 
	  cvalue = m2b(cvalue);
	else
	  cvalue=m2i(cvalue,'uint8');
	end 
	value= value+%i*cvalue;
      end
      value=matrix(value,DimensionArray)
     case Int16Class
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class);
      value=m2i(value,'int16');
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case Uint16Class
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class);
      value=m2i(value,'uint16');
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case Int32Class
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class);
      value=m2i(value,'int32');
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case Uint32Class
      value=loadmatfile_simple_elt(fd,prod(DimensionArray),Class);
      value=m2i(value,'uint32');
      if Flags(1) then 
	value=double_void(value)+%i*double_void(loadmatfile_simple_elt(fd,prod(DimensionArray)))
      end
      value=matrix(value,DimensionArray)
     case CellClass 
      value={}
      for k=1:prod(DimensionArray)
	value{k}=loadmatfile_mimatrix(fd)
      end
      if length(DimensionArray)<>2 then 
	error('Only 2 dimensional cells are accepted\n");
	return;
      end
      value.redim[DimensionArray(1),DimensionArray(2)];
     case CharClass 
      value=matrix(loadmatfile_simple_elt(fd,prod(DimensionArray)),DimensionArray(1),-1)
      t=[];for v=value',t=[t;stripblanks(ascii(double_void(v)))];end
      value=t
     case StructClass
      FieldNameLength=double_void(loadmatfile_simple_elt(fd,1))
      FieldNames=matrix(loadmatfile_simple_elt(fd),FieldNameLength,-1)
      NumberOfFields=size(FieldNames,2)
      Fnams=[];Fields=list();
      for k=1:NumberOfFields
	//l=find(FieldNames(:,k)==0,1)-1;
	Fnams=[Fnams,ascii(FieldNames(:,k))];
	Fields(k)=list();
      end
      
      if prod(DimensionArray)==1 then
	value=hash_create(NumberOfFields);
	for k=1:NumberOfFields
	  value(Fnams(k))=loadmatfile_mimatrix(fd);
	end
      else
	Fk=list();for i=1:size(DimensionArray,'*'),Fk(i)=[];end
	for k=1:NumberOfFields,Fields(k)=Fk,end
	for i=1:prod(DimensionArray)
	  for k=1:NumberOfFields
	    Fields(k)(i)=loadmatfile_mimatrix(fd);
	  end
	end
	//Form Scilab representation
	value=mlist(['st' 'dims' Fnams],DimensionArray,Fields(:))
      end
     case ObjectClass 
      ClassName=stripblanks(ascii(double_void(loadmatfile_simple_elt(fd))))
      FieldNameLength=double_void(loadmatfile_simple_elt(fd,1))
      FieldNames=matrix(loadmatfile_simple_elt(fd),FieldNameLength,-1)
      NumberOfFields=size(FieldNames,2)
      Fields=list();Fnams=[]
      for k=1:NumberOfFields
	l=find(FieldNames(:,k)==0,1)-1
	Fnams=[Fnams,stripblanks(ascii(double_void(FieldNames(1:l,k))))]
	Fields(k)=loadmatfile_mimatrix(fd)
      end
      //Form Scilab representation
      value=tlist([ClassName, Fnams],Fields(:))
      select ClassName
       case 'inline' then
	value=Object2Inline(value)
       case 'ss' then
	value=Object2SS(value)
       case 'tf' then
	value=Object2tf(value)
      end
     case SparseClass then
      Ir=double_void(loadmatfile_simple_elt(fd,NnzMax));
      Jc=double_void(loadmatfile_simple_elt(fd,DimensionArray(2)+1))
      value=double_void(loadmatfile_simple_elt(fd))
      if Flags(1) then 
	value=value+%i*double_void(loadmatfile_simple_elt(fd))
      end
      value=spfrommtlb(Jc,Ir,value,DimensionArray);
    else
      error(sprintf('Unknown Class: %d',Class));
    end
  endfunction

  function [DataType,NumberOfBytes,Compressed]=loadmatfile_tag(fd)
  //--TAG
  //Copyright INRIA   
  //Author Serge Steer  
    p1=fd.tell[] 
    t=fd.get[n=2,type=md_s];
    if isempty(t) then //EOF
      DataType=0;NumberOfBytes=0,Compressed=%f
    else
      if endian=='l' then 
	Compressed=t(2)<>0;
      else 
	Compressed=t(1)<>0;
      end
      if Compressed then // compressed data element format
	// data was writen as  fd.put[ishift(NumberOfBytes,16)+DataType,type=md_i];
	// thus we have to check endian to reread 
	fd.seek[p1];
	if endian == 'l' then 
	  DataType=fd.get[n=1,type=md_s];
	  NumberOfBytes=fd.get[n=1,type=md_s];
	else 
	  NumberOfBytes=fd.get[n=1,type=md_s];
	  DataType=fd.get[n=1,type=md_s];
	end
      else
	fd.seek[p1]
	DataType=fd.get[n=1,type=md_i];
	NumberOfBytes=fd.get[n=1,type=md_i];
      end 
    end
  endfunction

  function [Flags,Class,NnzMax]=loadmatfile_array_flags(fd)
  //Copyright INRIA
  //Author Serge Steer    
    [DataType,NumberOfBytes,Compressed]=loadmatfile_tag(fd) 
    B=fd.get[n=4,type='uc'];
    if endian=='l' then B=B([4 3 2 1]),end
    Class=B(4)
    Flags=byte2bits(B(3));Flags=Flags(4:-1:2)
    NnzMax=fd.get[n=1,type=md_i]
  endfunction

  function dims=loadmatfile_dim_array(fd)
  //Copyright INRIA  
  //Author Serge Steer    
    dims=loadmatfile_simple_elt(fd);
  endfunction

  function ArrayName=loadmatfile_array_name(fd)
  //Copyright INRIA
  //Author Serge Steer    
    ArrayName=ascii(double_void(loadmatfile_simple_elt(fd)))
  endfunction

  function value=loadmatfile_simple_elt(fd,NumberOfValues,Class)
  //Copyright INRIA  
  //Author Serge Steer  
    pse=fd.tell[]
    [DataType,NumberOfBytes,Compressed]=loadmatfile_tag(fd) 
    // printf("In ReadSimple %d %d %d\n",DataType,NumberOfBytes,Compressed);
    select DataType
     case miDOUBLE
      if nargin==1 then NumberOfValues=NumberOfBytes/8,end
      value=fd.get[n=NumberOfValues,type=md_d]
     case miSINGLE
      if nargin==1 then NumberOfValues=NumberOfBytes/4,end
      value=fd.get[n=NumberOfValues,type=md_f]
     case miINT8
      if nargin==1 then NumberOfValues=NumberOfBytes,end
      //     value=fd.geti[n=NumberOfValues,type="c"]
      value=fd.get[n=NumberOfValues,type="c"]
     case miUINT8
      if nargin==1 then NumberOfValues=NumberOfBytes,end
      //value=fd.geti[n=NumberOfValues,type="uc"]
      value=fd.get[n=NumberOfValues,type="uc"]
     case miINT16
      if nargin==1 then NumberOfValues=NumberOfBytes/2,end
      //value=fd.geti[n=NumberOfValues,type=md_s]
      value=fd.get[n=NumberOfValues,type=md_s]
     case miUINT16
      if nargin==1 then NumberOfValues=NumberOfBytes/2,end
      value=fd.get[n=NumberOfValues,type="u"+md_s]
     case miUINT32
      if nargin==1 then NumberOfValues=NumberOfBytes/4,end
      //value=fd.geti[n=NumberOfValues,type="u"+md_i]
      value=fd.get[n=NumberOfValues,type="u"+md_i]
     case miINT32
      if nargin==1 then NumberOfValues=NumberOfBytes/4,end
      //value=fd.geti[n=NumberOfValues,type=md_i]
      value=fd.get[n=NumberOfValues,type=md_i]
     case miUINT64
      if nargin==1 then NumberOfValues=NumberOfBytes/8,end
      value=fd.get[n=NumberOfValues,type="u"+md_l]
     case miINT64
      if nargin==1 then NumberOfValues=NumberOfBytes/8,end
      value=fd.get[n=NumberOfValues,type=md_l]
     case miMatrix
      fd.seek[pse]
      [value,ArrayName]=loadmatfile_mimatrix(fd)
     case miUTF8
      if nargin==1 then NumberOfValues=NumberOfBytes,end
      value=fd.get[n=NumberOfValues,type="uc"+md_i];
     case miUTF16
      error("UTF16: Not implemented");
     case miUTF32
      error("UTF32: Not implemented");
    else
      error("Not implemented DataType: "+string(DataType));
      return;
    end
    //  printf("value %e\n",value);
    loadmatfile_padding()

  endfunction
  

  function loadmatfile_padding()
  // skip padding data 
  //----------------------------------------------
  //Copyright INRIA
  //Author Serge Steer  
  //data fields are aligned on double words 
    np=modulo(8-modulo(fd.tell[],8),8)
    if np>0 then fd.get[n=np,type='uc'],end
  endfunction

  function loadmatfile_showbin(n,pi)
  //for debugging purpose
  //----------------------------------------------
  //Copyright INRIA
  //Author Serge Steer  
    p=fd.tell[]
    if nargin==2 then fd.seek[pi],end
    x=string(matrix(fd.geti[n=8*n,type='uc'],8,-1)')
    t=emptystr(n,1)+'|'
    for k=1:4
      t=t+part(x(:,k),1:max(length(x(:,k)))+1)
    end
    t=t+'|'
    for k=5:8
      t=t+part(x(:,k),1:max(length(x(:,k)))+1)
    end
    t=t+'|'
    write(%io(2),t,'(a)')
    fd.seek[p]
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

  function value=Object2Inline(value)
  //convert inline object to scilab function
  //Copyright INRIA
  //Author Serge Steer  
    
    deff('ans=value('+strcat(stripblanks(value.args),',')+')',value.expr,'n')
    comp(value,1);code=macr2lst(value)
    // load SCI/macros/m2sci/lib
    killed=[];quote='''';dquote="""";batch=%f
    [value,trad]=m2sci(code,'value',%f,%f)
    value($)='endfunction'
    //define the final version
    execstr(value)
  endfunction

  function res=Object2SS(res)
  //convert ss object to scilab 'lss' 
  //Copyright INRIA
  //Author Serge Steer  
    A=res.a;if type(A)==17 then A=A.entries(1),end
    B=res.b;if type(B)==17 then B=B.entries(1),end
    C=res.c;if type(C)==17 then C=C.entries(1),end
    D=res.d;if type(D)==17 then D=D.entries(1),end
    E=res.e;if type(E)==17 then E=E.entries(1),end
    st_nam=res.StateName
    props=res.lti
    dt=props.Ts;if dt==0 then dt='c',end
    res=syslin(dt,A,B,C,D)
    res($+1)=props
    res(1)($+1)='Properties'
  endfunction

  function res=Object2tf(res)
  //convert tf object to scilab 'r' 
  //Copyright INRIA 
  //Author Serge Steer  
    v=res.Variable
    dims=double_void(res.num.dims) //res.num.dims may be an integer array
    props=res.lti
    num=[];den=[];
    for k=1:prod(dims)
      num=[num;poly(res.num.entries(k)($:-1:1),v,'c')];
      den=[den;poly(res.den.entries(k)($:-1:1),v,'c')];
    end
    num=matrix(num,dims)
    den=matrix(den,dims)
    dt=props.Ts;if dt==0 then dt='c',end
    res=syslin(dt,num,den)
    res(1)($+1)='Properties'
    res($+1)=props
  endfunction

  function fd=loadmatfile_open(fil)
  //Copyright INRIA   
  //Author Serge Steer  
    fil=stripblanks(fil)
    fd=fopen(fil,mode='rb',swap=%f)
  endfunction

  function y=double_void(x)
    y=x;
  endfunction


  function b=byte2bits(i)
  //Copyright INRIA, Author Serge Steer 
  //jpc for nsp 
  //  b=(iconvert(i,11)&iconvert(2^(0:3),11))<>uint8(0)
    b = m2b(iand(i*ones(1,4),[1,2,4,8]));
  endfunction

  // start of code 
  
  
  
  for k=1:size(varargin)
    if type(varargin(k),'short')<>'s' then
      error("loadmatfile: expecting only string matrices as argument");
    end
  end
  
  fil=[]
  bin=%t;
  varnames=[]
  
  // Try to find type binary or ASCII ?
  // Filename is the first parameter: loadmatfile(filename[,opts])
  // or the second parameter: loadmatfile(filetype,filename[,opts]) 
  // with filetype equal to -ascii or -mat
  // filetype can also be included in opts
  nv = length(varargin);
  opts_id=bmat_create(nv,1);
  for k=1:nv 
    if part(varargin(k),1)<>'-' then 
      opts_id(k)=%f;
    end
  end
  vars=varargin.sublist[find(~opts_id)];
  opts=varargin.sublist[find(opts_id)];
  if length(vars) < 1 then 
    error('filename not given\n");
  else
    fil = vars(1);
    vars.remove_first[];
    // svars is a hash table which contains strings  to be saved 
    svars=hash_create(length(vars));
    for k=1:length(vars)
      svars(vars(k))=%t;
    end
  end
  get_all = length(svars)==0;
  for k=1:length(opts)
    select opts(k)
     case "-mat"
      bin=%t
     case "-ascii"
      bin=%f
     case "-regexp"
      printf("Option regexp is not implemented and ignored\n")
    end
  end
  if file('extension',fil)==".mat" then 
    // force bin 
    bin=%t
  end
  
  if file('extension',fil)=="" then
    fil=fil+".mat"
  end
  

  if bin then 
    // Try to read file as a level 5 binary file
    // -----------------------------------------
    // File opening
    fd=loadmatfile_open(fil)
    // Reads the file header
    ok=execstr("[head,version,endian]=loadmatfile_header(fd)",errcatch=%t)
    if ~ok then
      errmsg=lasterror();
      testmsg = errmsg(1);
      // This line has to be mofified according to error message in 'matfile_header' function
      if stripblanks(testmsg)=="Invalid level 5 binary MAT-file!\n" then 
	// Level 4 binary file ?
	level=4;
      else
	// Wrong file
	pause 
	error('error:"+strcat(errmsg));
	//error("error in loadmatfile");
      end
    else
      // Level 5 binary file
      level=5;
    end
    // --- LEVEL 5 BINARY FILE (This part already existed in old version) ---
    if level==5 then
      //--set constants
      exec(loadmatfile_mat_consts);
      //--loop on the stored variables
      Name='';Names=[];Matrices=list()
      while %t 
	//read next variable
	ok=execstr('[Matrix,Name]=loadmatfile_mimatrix(fd)',errcatch=%t) 
	if ~ok then 
	  fd.close[],print(lasterror()),
	  return,
	end
	if fd.eof[] then  
	  break,
	end 
	if get_all then 
	  // resume current variable 
	  str = sprintf("resume(%s=Matrix);",Name);
	  execstr(str);
	else
	  // resume current variable if in the list of requested vars 
	  if svars.iskey[Name] then 
	    str = sprintf("resume(%s=Matrix);",Name);
	    execstr(str);
	    svars.delete[Name];
	  end
	end
      end
      //--file closing
      fd.close[];
      // checks if some variable are missing 
      if length(svars)<>0 then 
	missing=svars.__keys;
	for i=1:length(svars);
	  printf("Warning: %s not found in file %s\n",missing(i),fil);
	end
      end
      return;
    else
      error("Unknown Matlab binary file format");
    end
  else
    // --- ASCII FILE ----
    ke=strindex(fil,'.')
    if isempty(ke) then ke=length(fil),else ke=ke($)-1,end
    kp=strindex(fil,['/','\'])
    if isempty(kp) then kp=1,else kp=kp($)+1,end
    name=part(fil,kp:ke)
    mat=evstr(mgetl(fil))
    execstr(name+'= resume(mat)')
  end
endfunction


