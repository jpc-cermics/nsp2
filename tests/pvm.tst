// -*- Mode: scilab -*- 
//clean remaining tasks

//pvm_halt();
//be sure that SCI is an absolute  pathname 
//since tests are run from scilex 

ok=pvm_start();
[task_id,numt] = pvm_spawn(task='SCI/tests/pvm_resend.sce',ntask=1,nowindow=%t);
if numt<>1 then pause,end
if size(task_id,'*')<>1 then pause,end
// pvm_config 
//----------
res=pvm_config();
if type(res,'short')<>'l' then pause,end
if size(res)<>7 then pause,end
if res(1)<>1|res(2)<>1 then pause,end
if type(res(3),'short')<>'m'|size(res(1),'*')<>1 then pause,end
hostid=res(3);
if type(res(4),'short')<>'s'|type(res(5),'short')<>'s' then pause,end
if size(res(4),'*')<>1|size(res(5),'*')<>1 then pause,end
hostname=res(4);
if type(res(6),'short')<>'m'|type(res(7),'short')<>'m' then pause,end
if size(res(6),'*')<>1|size(res(7),'*')<>1 then pause,end
// pvm_tasks 
//----------
rest=pvm_tasks();
if type(rest,'short')<>'l' then pause,end
if size(rest)<>7 then pause,end

if type(rest(1),'short')<>'m' then pause,end
n=size(rest(1),'*');
if size(rest(1),'*')<>n then pause,end
if rest(1)($)<>task_id then pause,end

if type(rest(2),'short')<>'m' then pause,end
if size(rest(2),'*')<>n then pause,end
if rest(2)($)<>rest(1)($-1) then pause,end

if type(rest(3),'short')<>'m' then pause,end
if size(rest(3),'*')<>n then pause,end

if type(rest(4),'short')<>'m' then pause,end
if size(rest(4),'*')<>n then pause,end

if type(rest(5),'short')<>'s' then pause,end
if size(rest(5),'*')<>n then pause,end
if rest(5)(n)<>'scilab' then pause,end
if or(rest(5)(1:n-1)<>"") then pause,end

if rest(6)<>n then pause,end
if rest(7)<>0 then pause,end

// pvm_tidtohost
if hostid<>pvm_tidtohost(task_id) then pause,end
// pvm_addhosts
err=pvm_addhosts(hostname);
if err<>-28 then pause,end
if pvm_error(err)<>'Duplicate host' then pause,end

// timers 
// FIXME : same as timer()
pvm_set_timer();
t=pvm_get_timer();if t<0 then pause,end

// pvm_joingroup
inum=pvm_joingroup('test');
if inum<>0 then pause,end

// pvm_mytid
tid=pvm_mytid();
if tid<>rest(2)($) then pause,end
// pvm_getinst
if pvm_getinst('test',tid)<>inum then pause,end
// pvm_parent
if pvm_parent()<>-23 then pause,end
// pvm_gettid 
if pvm_gettid('test',inum)<>tid  then pause,end
// pvm_gsize 
if pvm_gsize('test')<>1 then pause,end
// pvm_lvgroup
if pvm_lvgroup('test')<>0 then pause,end

if pvm_gsize('test')<>-19  then pause,end
inum=pvm_joingroup('test');
// pvm_reduce 
[buff, info] = pvm_reduce("Max", [1 2 3], 1,'test',0);
if info < 0 then pause,end
if buff<>[1,2,3] then pause,end

// send to spawned process
a=[];
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=[1 2 3];
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=1;
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=a+%i;
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=rand(7,10);
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=rand(100,10)+%i*rand(100,10);
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=rand(100,200);
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end

// strings
a='';
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=string(rand(5,10));
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=strcat(string(rand(10,10)));
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a='1';
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end
a=string(rand(5,5));
pvm_send(task_id,a,0);a1=pvm_recv(task_id,0);if ~and(a==a1) then pause,end

// stop the slave 
pvm_send(task_id,'exit',0)
// kill the slave
pvm_kill(task_id)

args=['-f',getenv('SCI')+'/tests/pvm_resend.sce','-nw'];
[tids, numt] = pvm_spawn_independent('scilab',ntask=1,args=args);
if numt<>1 then pause,end
if size(tids,'*')<>1 then pause,end
pvm_send(tids,'exit',0)
// pvm_kill 
// pvm_kill(tids);
// halt pvm
pvm_halt( );

