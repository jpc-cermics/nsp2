SHELL = /bin/sh

include ../../Makefile.incl
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.a

CFLAGS = $(CC_OPTIONS) -Wno-parentheses -Wno-unused-but-set-variable -Wno-maybe-uninitialized
FFLAGS = $(FC_OPTIONS)

IO = bb01ad.o bb02ad.o bd01ad.o bd02ad.o  md03bf.o \
	ud01md.o ud01mz.o ud01nd.o ud01dd.o \
	ud01cd.o ud01bd.o

OBJSC = Slicot-IN.o \
	ab13md.o \
	ma02ad.o \
	ma02ed.o \
	mb01ru.o \
	mb01rx.o \
	mb01ry.o \
	mb01ud.o \
	sb02mr.o \
	sb02mv.o \
	sb02od.o \
	sb02ou.o \
	sb02ov.o \
	sb02ow.o \
	sb02oy.o \
	sb02rd.o \
	sb02sd.o \
	sb03mx.o \
	sb03sx.o \
	sb03sy.o \
	sb10dd.o \
	sb10fd.o \
	sb10pd.o \
	sb10qd.o \
	sb10rd.o \
	select.o \
	mb01sd.o \
	mb02pd.o \
	sb02ms.o \
	sb02mw.o \
	sb02qd.o \
	sb02ru.o \
	sb03mv.o \
	sb04px.o \
	sb03my.o \
	sb03qx.o \
	sb03qy.o \
	sb03mw.o \
	ab01od.o \
	ab01nd.o \
	mb01pd.o \
	mb03oy.o \
	mb01qd.o \
	ib01cd.o \
	ib01ad.o \
	ib01bd.o \
	tb01wd.o \
	ib01rd.o \
	ib01qd.o \
	ib01md.o \
	ib01nd.o \
	ib01od.o \
	ib01pd.o \
	sb02mt.o \
	sb02nd.o \
	mb01td.o \
	mb04od.o \
	mb04oy.o \
	mb02ud.o \
	mb03ud.o \
	mb04id.o \
	mb03od.o \
	mb04iy.o \
	ib01my.o \
	ib01oy.o \
	ib01py.o \
	mb02qy.o \
	ib01px.o \
	mb04kd.o \
	ma02fd.o \
	mb01vd.o \
	sb04pd.o \
	sb04md.o \
	sb04qd.o \
	sb04nd.o \
	sb04py.o \
	sb04rd.o \
	sb04my.o \
	sb04mu.o \
	sb04qy.o \
	sb04qu.o \
	sb04nw.o \
	sb04ny.o \
	sb04nv.o \
	sb04nx.o \
	sb04rw.o \
	sb04ry.o \
	sb04rx.o \
	sb04mw.o \
	sb04mr.o \
	sb04qr.o \
	sb03md.o \
	sb03od.o \
	sb04rv.o \
	mb01rd.o \
	sb03ou.o \
	sb03ot.o \
	sb03oy.o \
	sb03or.o \
	sb03ov.o \
	mb04nd.o \
	mb04ny.o \

OBJSC_ALL = \
    ab01md.o ab01nd.o  ab04md.o ab05md.o ab05nd.o ab05od.o \
    ab05pd.o ab05qd.o ab05rd.o ab05sd.o ab07md.o ab07nd.o ab08md.o \
    ab08mz.o ab08nd.o ab08nx.o ab08nz.o ab09ad.o ab09ax.o ab09bd.o \
    ab09bx.o ab09cd.o ab09cx.o ab09dd.o ab09ed.o ab09fd.o ab09gd.o \
    ab09hd.o ab09hx.o ab09hy.o ab09id.o ab09ix.o ab09iy.o ab09jd.o \
    ab09jv.o ab09jw.o ab09jx.o ab09kd.o ab09kx.o ab09md.o ab09nd.o \
    ab13ad.o ab13ax.o ab13bd.o ab13cd.o ab13dd.o ab13dx.o ab13ed.o \
    ab13fd.o ab13md.o ab8nxz.o ag07bd.o ag08bd.o ag08by.o ag08bz.o \
    ag8byz.o bb03ad.o bb04ad.o  \
    de01od.o de01pd.o delctg.o df01md.o dg01md.o dg01nd.o dg01ny.o \
    dg01od.o dk01md.o fb01qd.o fb01rd.o fb01sd.o fb01td.o fb01vd.o \
    fd01ad.o ib01ad.o ib01bd.o ib01cd.o ib01md.o ib01my.o ib01nd.o \
    ib01od.o ib01pd.o ib01px.o ib01py.o ib01qd.o ib01rd.o \
    ib03ad.o ib03bd.o ma01ad.o ma02ad.o ma02bd.o ma02bz.o ma02cd.o \
    ma02cz.o ma02dd.o ma02ed.o ma02fd.o ma02gd.o ma02hd.o ma02id.o \
    ma02jd.o mb01md.o mb01nd.o mb01pd.o mb01qd.o mb01rd.o mb01ru.o \
    mb01rw.o mb01rx.o mb01ry.o mb01sd.o mb01td.o mb01ud.o mb01uw.o \
    mb01ux.o mb01vd.o mb01wd.o mb01xd.o mb01xy.o mb01yd.o mb01zd.o \
    mb02cd.o mb02cu.o mb02cv.o mb02cx.o mb02cy.o mb02dd.o mb02ed.o \
    mb02fd.o mb02gd.o mb02hd.o mb02id.o mb02jd.o mb02jx.o mb02kd.o \
    mb02md.o mb02nd.o mb02ny.o mb02od.o mb02pd.o mb02qd.o mb02qy.o \
    mb02rd.o mb02rz.o mb02sd.o mb02sz.o mb02td.o mb02tz.o mb02ud.o \
    mb02uu.o mb02uv.o mb02vd.o mb02wd.o mb02xd.o mb02yd.o mb03md.o \
    mb03my.o mb03nd.o mb03ny.o mb03od.o mb03oy.o mb03pd.o mb03py.o \
    mb03qd.o mb03qx.o mb03qy.o mb03rd.o mb03rx.o mb03ry.o mb03sd.o \
    mb03td.o mb03ts.o mb03ud.o mb03vd.o mb03vy.o mb03wa.o mb03wd.o \
    mb03wx.o mb03xd.o mb03xu.o mb03ya.o mb03yd.o mb03yt.o \
    mb03za.o mb03zd.o mb04dd.o mb04di.o mb04ds.o mb04dy.o mb04gd.o \
    mb04id.o mb04iy.o mb04iz.o mb04jd.o mb04kd.o mb04ld.o mb04md.o \
    mb04nd.o mb04ny.o mb04od.o mb04ow.o mb04ox.o mb04oy.o mb04pa.o \
    mb04pb.o mb04pu.o mb04py.o  mb04qc.o mb04qf.o mb04qu.o \
    mb04ts.o mb04tt.o mb04tu.o mb04tv.o mb04tw.o mb04tx.o \
    mb04ty.o mb04ud.o mb04vd.o mb04vx.o mb04wp.o mb04wr.o \
    mb04wu.o mb04xd.o mb04xy.o mb04yd.o mb04yw.o mb04zd.o mb05md.o \
    mb05my.o mb05nd.o mb05od.o mb05oy.o mb3oyz.o mb3pyz.o mc01md.o \
    mc01nd.o mc01od.o mc01pd.o mc01py.o mc01qd.o mc01rd.o mc01sd.o \
    mc01sw.o mc01sx.o mc01sy.o mc01td.o mc01vd.o mc01wd.o mc03md.o \
    mc03nd.o mc03nx.o mc03ny.o md03ad.o md03ba.o md03bb.o md03bd.o \
    md03bx.o md03by.o nf01ad.o nf01ay.o \
    nf01bd.o nf01bp.o nf01bq.o nf01br.o nf01bs.o \
    nf01bu.o nf01bv.o nf01bw.o nf01bx.o nf01by.o sb01bd.o sb01bx.o \
    sb01by.o sb01dd.o sb01fy.o sb01md.o sb02cx.o sb02md.o sb02mr.o \
    sb02ms.o sb02mt.o sb02mu.o sb02mv.o sb02mw.o sb02nd.o sb02od.o \
    sb02ou.o sb02ov.o sb02ow.o sb02ox.o sb02oy.o sb02pd.o sb02qd.o \
    sb02rd.o sb02ru.o sb02sd.o sb03md.o sb03mu.o sb03mv.o sb03mw.o \
    sb03mx.o sb03my.o sb03od.o sb03or.o sb03ot.o sb03ou.o sb03ov.o \
    sb03oy.o sb03pd.o sb03qd.o sb03qx.o sb03qy.o sb03rd.o sb03sd.o \
    sb03sx.o sb03sy.o sb03td.o sb03ud.o sb04md.o sb04mr.o sb04mu.o \
    sb04mw.o sb04my.o sb04nd.o sb04nv.o sb04nw.o sb04nx.o sb04ny.o \
    sb04od.o sb04ow.o sb04pd.o sb04px.o sb04py.o sb04qd.o sb04qr.o \
    sb04qu.o sb04qy.o sb04rd.o sb04rv.o sb04rw.o sb04rx.o sb04ry.o \
    sb06nd.o sb08cd.o sb08dd.o sb08ed.o sb08fd.o sb08gd.o sb08hd.o \
    sb08md.o sb08my.o sb08nd.o sb08ny.o sb09md.o sb10ad.o sb10dd.o \
    sb10ed.o sb10fd.o sb10hd.o sb10id.o sb10jd.o sb10kd.o sb10ld.o \
    sb10md.o sb10pd.o sb10qd.o sb10rd.o sb10sd.o sb10td.o sb10ud.o \
    sb10vd.o sb10wd.o sb10yd.o sb10zd.o sb10zp.o sb16ad.o sb16ay.o \
    sb16bd.o sb16cd.o sb16cy.o select.o sg02ad.o sg03ad.o sg03ax.o \
    sg03ay.o sg03bd.o sg03bu.o sg03bv.o sg03bw.o sg03bx.o sg03by.o \
    tb01id.o tb01iz.o tb01kd.o tb01ld.o tb01md.o tb01nd.o tb01pd.o \
    tb01td.o tb01ty.o tb01ud.o tb01vd.o tb01vy.o tb01wd.o tb01xd.o \
    tb01xz.o tb01yd.o tb01zd.o tb03ad.o tb03ay.o tb04ad.o tb04ay.o \
    tb04bd.o tb04bv.o tb04bw.o tb04bx.o tb04cd.o tb05ad.o tc01od.o \
    tc04ad.o tc05ad.o td03ad.o td03ay.o td04ad.o td05ad.o tf01md.o \
    tf01mx.o tf01my.o tf01nd.o tf01od.o tf01pd.o tf01qd.o tf01rd.o \
    tg01ad.o tg01az.o tg01bd.o tg01cd.o tg01dd.o tg01ed.o tg01fd.o \
    tg01fz.o tg01hd.o tg01hx.o tg01id.o tg01jd.o tg01wd.o  \
    ue01md.o mb04qb.o mb03xp.o mb04tb.o mb04wd.o \
    nf01ba.o nf01bb.o nf01be.o nf01bf.o ib01oy.o

OBJSF =  

include ../Make.lib

all:: Makefile.mak

Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

SRC = $(patsubst %.o,%.c,$(OBJSC))

job	:
	mv $(SRC) src/

# according to compiler, do not optimize the following files

