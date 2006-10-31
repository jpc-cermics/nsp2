function demo_xchoices(flag)
  l1=list('combo','combo title',1,['choice 1','choice 2','choice 3']);
  l2=list('entry','entry title',0,['initial']); // 0 is unused 
  l3=list('matrix','enter matrix',10,string(rand(6,2))); // l(3) is for entry size
  l3b=list('matrix','enter matrix',10,['A','B';'C','D']); // l(3) is for entry size
  l4=list('colors','colors choice 4',29,['']);
  l5=list('save','file save',1,['foo.sav']); // initial value active must
                                             // be set to 1 to use
                                             // default value.
  l6=list('open','file open',0,['foo.rep','*.eps','*.pdf']); // answer, filter 
  l7=list('folder','choose a folder',1,['']);// answer, filter unused 
  // spin=[value,lower,upper,step_increment,page_increment,page_size,climb_rate,digits] 
  v8=[5,0,100,0.5,10,20,3,3];    
  l8=list('spin','double with spin',0,v8);// l(3) is unused
  l9=list('range','double with range',0,v8);// l(3) is unused
  L= list(l1,l2,l3,l3b,l4,l5,l6,l7,l8,l9);
  [Lres,L1,rep]=x_choices('Toggle Menu',L,flag);
  // here L1==L can be used to detect which menu changed 
  // L1 can be used in a subsequent call since it already contains 
  // the changes. 
  // Lres contains the results stored in list 
  // rep contain the relevant results for the subset of gtk items 
  // which are given by numbers i.e combo,colors,spin and ranges. 
endfunction

function demo_xchoose()
  n=x_choose(['item1';'item2';'item3'],['that is a title';'for xchoose'])
  n=x_choose(['item1';'item2';'item3'],['that is a title';'for xchoose'],"gtk-close")
endfunction

function demo_dialog()
  gain=evstr(x_dialog('value of gain ?','0.235'))
  x_dialog(['Method';'enter sampling period'],'1')
  m=evstr(x_dialog('enter a  3x3 matrix ',['[0 0 0';'0 0 0';'0 0 0]']))
endfunction

function demo_getfile()
  xgetfile()
  xgetfile(dir="/tmp",masks=['foo','*'],title=['title'],save=%t)
  // ici action n'est pas actif 
  xgetfile(dir="/tmp",masks=['foo','*'],title=['title'],open=%t)
  xgetfile(dir="/tmp",masks=['foo','*'],title=['title'],action=%t);
endfunction

function demo_xmdialog()
  rep=x_mdialog(['System Simulation';'with PI regulator'],...
		['P gain';'I gain '],[' ';' '])

  n=5;m=4;mat=rand(n,m);
  row='row';labelv=row(ones(1,n))+string(1:n)
  col='col';labelh=col(ones(1,m))+string(1:m)
  smat=m2s(mat,"%5.2f");
  x_mdialog('Matrix to edit',labelv,labelh,smat);
  x_mdialog('Matrix to edit',string([]),string([]),smat);
  x_mdialog('Matrix to edit',labelv,labelh,smat,entry=%t);
  x_mdialog('Matrix to edit',string([]),string([]),smat,entry=%t);
endfunction;

function demo_message()
  x_message('foo')
  x_message('foo',"gtk-close");
endfunction

