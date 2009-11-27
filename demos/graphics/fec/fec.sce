

if ~new_graphics() then 
  switch_graphics()
end

// define a mini triangulation (4 vertices, 2 triangles)
x = [0 1 0 -1];
y = [0 0 1  1];
T = [1 1 2 3 1;
     2 3 4 1 1];
// values of the function at each vertices of the 
// triangulation;

z = [0 1 0 -1];  

xclear();
subplot(1,2,1)
xset("colormap",jetcolormap(64))
fec(x,y,T,z,strf="040",mesh=%t)
xtitle("fec example (with the mesh)")
colorbar(-1,1)

subplot(1,2,2)
fec(x,y,T,z,strf="040")  // rmq: mesh=%f by default
xtitle("fec example (without the mesh)")
colorbar(-1,1)
xselect()

// this example shows the effect of zminmax and uses the
// previous example datas (you have to execute the it before)
xclick(); 
xbasc()
xset("colormap",jetcolormap(64))
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], mesh=%t)
xtitle("fec example : using zminmax argument")
colorbar(-0.5,0.5)  // be careful colorbar must be set by hands !
xselect()

// this example shows the effect of zminmax and colout. It uses
// also the datas of the first example (you have to execute the it before)
xclick();xbasc()
xset("colormap",jetcolormap(64))
subplot(2,2,1)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[0 0], mesh=%t)
xtitle("fec example : using zminmax and colout =[0 0]")
colorbar(-0.5,0.5)


subplot(2,2,2)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[67 67], mesh=%t)
xtitle("fec example : using zminmax and colout =[67 67]")
colorbar(-0.5,0.5)

subplot(2,2,3)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[-1 0], mesh=%t)
xtitle("fec example : using zminmax and colout =[-1 0]")
colorbar(-0.5,0.5)

subplot(2,2,4)
fec(x,y,T,z,strf="040", zminmax=[-0.5 0.5], colout=[0 -1], mesh=%t)
xtitle("fec example : using zminmax and colout =[0 -1]")
colorbar(-0.5,0.5)
xselect()


// this example shows a feature from colminmax:
// playing with 2 colormaps for 2 subplots. It
// uses also the data of the first example.
xclick(); xbasc()
xset("colormap",[hotcolormap(64);greencolormap(64)])
subplot(1,2,1)
fec(x,y,T,z,strf="040", colminmax=[1 64], mesh=%t)
xtitle("fec using the hot colormap")
colorbar(-1,1,[1 64])

subplot(1,2,2)
fec(x,y,T,z,strf="040", colminmax=[65 128], mesh=%t)
xtitle("fec using the greencolormap")
colorbar(-1,1,[65 128])

xselect()
  
