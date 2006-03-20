// -*- Mode: scilab -*- 

[a,b]=regexp('pipopopopa','p(i[po]+)(pa)')
if ~a then pause;end
if b<>[1,10;2,8;9,10] then pause;end 
rep=regsub(['pipopopopa';'pipopa';'pi'],'p(i[po]+)(pa)','(\\1)(\\2)');
if rep<>["(ipopopo)(pa)";"(ipo)(pa)";"pi"] then pause;end

