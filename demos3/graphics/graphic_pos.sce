// must be run in a gtk nsp 

opengl=%f;

exec('graphic_list.sce');

// draw and save in postscript all graphics 

for i=1:size(graphic_demos_all,'*')
  L=graphic_demos_all(i)(4);
  for j=1:size(L,'*')
    x=L(j)(3);
    execstr(x+'()');
    xg2ps(0,'Pos/'+ x+'_pos.eps',mode='d');
    xset('default');
    // xclick();
    xclear();
  end
end

// produce a latex file 

F=fopen('Pos/all.tex',mode='w');
for i=1:size(graphic_demos_all,'*')
  L=graphic_demos_all(i)(4);
  for j=1:size(L,'*')
    x=L(j)(3);
    F.printf["\section{%s}\n ",strsubst(x,'_',' ')];
    F.printf["\\fbox{\includegraphics{%s_pos.eps}}\n",x];
  end
end
F.close[];
