

str = "x=[5,6]+[5;6]";

execstr(str); // enter error 

ok=execstr(str,errcatch=%t);
err=lasterror();

// should return %t and no message 
ok=execstr('execstr('''+str+''',errcatch=%t);lasterror();');
err=lasterror();


// should return %t but also the uncleared message 
// since it was not cleared by the internal execstr
ok=execstr('execstr('''+str+''',errcatch=%t);');
err=lasterror();

// Attention il reste ici a repropager 
// le message interne 

// should return %f and full error in err
ok=execstr('execstr('''+str+''')',errcatch=%t);
err=lasterror();

