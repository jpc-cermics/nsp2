// -*- Mode: scilab -*- 

str = "x=[5,6]+[5;6]";
// this first call will produce an error 
// execstr(str);
// we check this in a try/catch/finally

err=%f
try 
  execstr(str);
catch
  // printf('Jump to catch:\n');
  // msg= catenate(lasterror()));
  err=%t;
finally 
  // check that error was reported
  if ~err then pause;end 
end

// protect the call with execstr 
// -----------------------------
ok=execstr(str,errcatch=%t);
err=lasterror();
if isempty(err) then pause;end 
if ok then pause;end 

// two recursives execstr 
// the inner one uses errcatch
// should return %t and no message 
//--------------------------------
ok=execstr('execstr('''+str+''',errcatch=%t);lasterror();');
err=lasterror();
if ~ok then pause;end 
if ~isempty(err) then pause;end 

// two recursives execstr 
// the inner one uses errcatch
// should return %t but also the uncleared message 
// since it was not cleared by the internal execstr
ok=execstr('execstr('''+str+''',errcatch=%t);');
err=lasterror();
if ~ok then pause;end 
if isempty(err) then pause;end 

// two recursives execstr 
// the outer one uses errcatch
// should return %f and full error in err
ok=execstr('execstr('''+str+''')',errcatch=%t);
err=lasterror();
if ok == %t then pause;end 
if isempty(err) then pause;end 
if size(err,'*')<>9 then pause;end 

// The same with execution of a function 
// =======================================

function y=f(); y=[5,6]+[5;6]; endfunction ;

err=%f
try 
  execf(f);
catch
  // printf('Jump to catch:\n');
  // msg= catenate(lasterror()));
  err=%t;
finally 
  // check that error was reported
  if ~err then pause;end 
end;

// protect the call with execstr 
// -----------------------------
ok=execf(f,errcatch=%t);
err=lasterror();
if isempty(err) then pause;end 
if ok then pause;end 

// two recursives execstr 
// the inner one uses errcatch
// should return %t and no message 
//--------------------------------
ok=execstr('execf(f,errcatch=%t);lasterror();');
err=lasterror();
if ~ok then pause;end 
if ~isempty(err) then pause;end 

// two recursives execstr 
// the inner one uses errcatch
// should return %t but also the uncleared message 
// since it was not cleared by the internal execstr
ok=execstr('execf(f,errcatch=%t);');
err=lasterror();
if ~ok then pause;end 
if isempty(err) then pause;end 

// two recursives execstr 
// the outer one uses errcatch
// should return %f and full error in err
ok=execstr('execf(f)',errcatch=%t);
err=lasterror();
if ok == %t then pause;end 
if isempty(err) then pause;end 
if size(err,'*')<>9 then pause;end 

// The same with exec of a file 
// =======================================

err=%f
try 
  exec("errcatch.sce");
catch
  // printf('Jump to catch:\n');
  // msg= catenate(lasterror()));
  err=%t;
finally 
  // check that error was reported
  if ~err then pause;end 
end;

// protect the call with execstr 
// -----------------------------
ok=exec("errcatch.sce",errcatch=%t);
err=lasterror();
if isempty(err) then pause;end 
if ok then pause;end 

// two recursives execstr 
// the inner one uses errcatch
// should return %t and no message 
//--------------------------------
ok=execstr('exec('"errcatch.sce'",errcatch=%t);lasterror();');
err=lasterror();
if ~ok then pause;end 
if ~isempty(err) then pause;end 

// two recursives execstr 
// the inner one uses errcatch
// should return %t but also the uncleared message 
// since it was not cleared by the internal execstr
ok=execstr('exec('"errcatch.sce'",errcatch=%t);');
err=lasterror();
if ~ok then pause;end 
if isempty(err) then pause;end 

// two recursives execstr 
// the outer one uses errcatch
// should return %f and full error in err
ok=execstr('exec('"errcatch.sce'")',errcatch=%t);
err=lasterror();
if ok == %t then pause;end 
if isempty(err) then pause;end 
if size(err,'*')<>9 then pause;end 




