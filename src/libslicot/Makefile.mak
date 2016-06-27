#-----------------------------
# generated from Makefile: DO NOT EDIT
# -----------------------------
SHELL = /bin/sh

include ../../Makefile.incl.mak
SCIDIR=../..
SCIDIR1=..\..

LIBRARY = nsp.lib

CFLAGS = $(CC_OPTIONS) -Wno-parentheses -Wno-unused-but-set-variable -Wno-maybe-uninitialized
FFLAGS = $(FC_OPTIONS)

IO = bb01ad.obj bb02ad.obj bd01ad.obj bd02ad.obj  md03bf.obj \
	ud01md.obj ud01mz.obj ud01nd.obj ud01dd.obj \
	ud01cd.obj ud01bd.obj

OBJSC = Slicot-IN.obj \
	ab13md.obj \
	ma02ad.obj \
	ma02ed.obj \
	mb01ru.obj \
	mb01rx.obj \
	mb01ry.obj \
	mb01ud.obj \
	sb02mr.obj \
	sb02mv.obj \
	sb02od.obj \
	sb02ou.obj \
	sb02ov.obj \
	sb02ow.obj \
	sb02oy.obj \
	sb02rd.obj \
	sb02sd.obj \
	sb03mx.obj \
	sb03sx.obj \
	sb03sy.obj \
	sb10dd.obj \
	sb10fd.obj \
	sb10pd.obj \
	sb10qd.obj \
	sb10rd.obj \
	select.obj \
	mb01sd.obj \
	mb02pd.obj \
	sb02ms.obj \
	sb02mw.obj \
	sb02qd.obj \
	sb02ru.obj \
	sb03mv.obj \
	sb04px.obj \
	sb03my.obj \
	sb03qx.obj \
	sb03qy.obj \
	sb03mw.obj \
	ab01od.obj \
	ab01nd.obj \
	mb01pd.obj \
	mb03oy.obj \
	mb01qd.obj \
	ib01cd.obj \
	ib01ad.obj \
	ib01bd.obj \
	tb01wd.obj \
	ib01rd.obj \
	ib01qd.obj \
	ib01md.obj \
	ib01nd.obj \
	ib01od.obj \
	ib01pd.obj \
	sb02mt.obj \
	sb02nd.obj \
	mb01td.obj \
	mb04od.obj \
	mb04oy.obj \
	mb02ud.obj \
	mb03ud.obj \
	mb04id.obj \
	mb03od.obj \
	mb04iy.obj \
	ib01my.obj \
	ib01oy.obj \
	ib01py.obj \
	mb02qy.obj \
	ib01px.obj \
	mb04kd.obj \
	ma02fd.obj \
	mb01vd.obj \
	sb04pd.obj \
	sb04md.obj \
	sb04qd.obj \
	sb04nd.obj \
	sb04py.obj \
	sb04rd.obj \
	sb04my.obj \
	sb04mu.obj \
	sb04qy.obj \
	sb04qu.obj \
	sb04nw.obj \
	sb04ny.obj \
	sb04nv.obj \
	sb04nx.obj \
	sb04rw.obj \
	sb04ry.obj \
	sb04rx.obj \
	sb04mw.obj \
	sb04mr.obj \
	sb04qr.obj \
	sb03md.obj \
	sb03od.obj \
	sb04rv.obj \
	mb01rd.obj \
	sb03ou.obj \
	sb03ot.obj \
	sb03oy.obj \
	sb03or.obj \
	sb03ov.obj \
	mb04nd.obj \
	mb04ny.obj \

OBJSC_ALL = \
    ab01md.obj ab01nd.obj  ab04md.obj ab05md.obj ab05nd.obj ab05od.obj \
    ab05pd.obj ab05qd.obj ab05rd.obj ab05sd.obj ab07md.obj ab07nd.obj ab08md.obj \
    ab08mz.obj ab08nd.obj ab08nx.obj ab08nz.obj ab09ad.obj ab09ax.obj ab09bd.obj \
    ab09bx.obj ab09cd.obj ab09cx.obj ab09dd.obj ab09ed.obj ab09fd.obj ab09gd.obj \
    ab09hd.obj ab09hx.obj ab09hy.obj ab09id.obj ab09ix.obj ab09iy.obj ab09jd.obj \
    ab09jv.obj ab09jw.obj ab09jx.obj ab09kd.obj ab09kx.obj ab09md.obj ab09nd.obj \
    ab13ad.obj ab13ax.obj ab13bd.obj ab13cd.obj ab13dd.obj ab13dx.obj ab13ed.obj \
    ab13fd.obj ab13md.obj ab8nxz.obj ag07bd.obj ag08bd.obj ag08by.obj ag08bz.obj \
    ag8byz.obj bb03ad.obj bb04ad.obj  \
    de01od.obj de01pd.obj delctg.obj df01md.obj dg01md.obj dg01nd.obj dg01ny.obj \
    dg01od.obj dk01md.obj fb01qd.obj fb01rd.obj fb01sd.obj fb01td.obj fb01vd.obj \
    fd01ad.obj ib01ad.obj ib01bd.obj ib01cd.obj ib01md.obj ib01my.obj ib01nd.obj \
    ib01od.obj ib01pd.obj ib01px.obj ib01py.obj ib01qd.obj ib01rd.obj \
    ib03ad.obj ib03bd.obj ma01ad.obj ma02ad.obj ma02bd.obj ma02bz.obj ma02cd.obj \
    ma02cz.obj ma02dd.obj ma02ed.obj ma02fd.obj ma02gd.obj ma02hd.obj ma02id.obj \
    ma02jd.obj mb01md.obj mb01nd.obj mb01pd.obj mb01qd.obj mb01rd.obj mb01ru.obj \
    mb01rw.obj mb01rx.obj mb01ry.obj mb01sd.obj mb01td.obj mb01ud.obj mb01uw.obj \
    mb01ux.obj mb01vd.obj mb01wd.obj mb01xd.obj mb01xy.obj mb01yd.obj mb01zd.obj \
    mb02cd.obj mb02cu.obj mb02cv.obj mb02cx.obj mb02cy.obj mb02dd.obj mb02ed.obj \
    mb02fd.obj mb02gd.obj mb02hd.obj mb02id.obj mb02jd.obj mb02jx.obj mb02kd.obj \
    mb02md.obj mb02nd.obj mb02ny.obj mb02od.obj mb02pd.obj mb02qd.obj mb02qy.obj \
    mb02rd.obj mb02rz.obj mb02sd.obj mb02sz.obj mb02td.obj mb02tz.obj mb02ud.obj \
    mb02uu.obj mb02uv.obj mb02vd.obj mb02wd.obj mb02xd.obj mb02yd.obj mb03md.obj \
    mb03my.obj mb03nd.obj mb03ny.obj mb03od.obj mb03oy.obj mb03pd.obj mb03py.obj \
    mb03qd.obj mb03qx.obj mb03qy.obj mb03rd.obj mb03rx.obj mb03ry.obj mb03sd.obj \
    mb03td.obj mb03ts.obj mb03ud.obj mb03vd.obj mb03vy.obj mb03wa.obj mb03wd.obj \
    mb03wx.obj mb03xd.obj mb03xu.obj mb03ya.obj mb03yd.obj mb03yt.obj \
    mb03za.obj mb03zd.obj mb04dd.obj mb04di.obj mb04ds.obj mb04dy.obj mb04gd.obj \
    mb04id.obj mb04iy.obj mb04iz.obj mb04jd.obj mb04kd.obj mb04ld.obj mb04md.obj \
    mb04nd.obj mb04ny.obj mb04od.obj mb04ow.obj mb04ox.obj mb04oy.obj mb04pa.obj \
    mb04pb.obj mb04pu.obj mb04py.obj  mb04qc.obj mb04qf.obj mb04qu.obj \
    mb04ts.obj mb04tt.obj mb04tu.obj mb04tv.obj mb04tw.obj mb04tx.obj \
    mb04ty.obj mb04ud.obj mb04vd.obj mb04vx.obj mb04wp.obj mb04wr.obj \
    mb04wu.obj mb04xd.obj mb04xy.obj mb04yd.obj mb04yw.obj mb04zd.obj mb05md.obj \
    mb05my.obj mb05nd.obj mb05od.obj mb05oy.obj mb3oyz.obj mb3pyz.obj mc01md.obj \
    mc01nd.obj mc01od.obj mc01pd.obj mc01py.obj mc01qd.obj mc01rd.obj mc01sd.obj \
    mc01sw.obj mc01sx.obj mc01sy.obj mc01td.obj mc01vd.obj mc01wd.obj mc03md.obj \
    mc03nd.obj mc03nx.obj mc03ny.obj md03ad.obj md03ba.obj md03bb.obj md03bd.obj \
    md03bx.obj md03by.obj nf01ad.obj nf01ay.obj \
    nf01bd.obj nf01bp.obj nf01bq.obj nf01br.obj nf01bs.obj \
    nf01bu.obj nf01bv.obj nf01bw.obj nf01bx.obj nf01by.obj sb01bd.obj sb01bx.obj \
    sb01by.obj sb01dd.obj sb01fy.obj sb01md.obj sb02cx.obj sb02md.obj sb02mr.obj \
    sb02ms.obj sb02mt.obj sb02mu.obj sb02mv.obj sb02mw.obj sb02nd.obj sb02od.obj \
    sb02ou.obj sb02ov.obj sb02ow.obj sb02ox.obj sb02oy.obj sb02pd.obj sb02qd.obj \
    sb02rd.obj sb02ru.obj sb02sd.obj sb03md.obj sb03mu.obj sb03mv.obj sb03mw.obj \
    sb03mx.obj sb03my.obj sb03od.obj sb03or.obj sb03ot.obj sb03ou.obj sb03ov.obj \
    sb03oy.obj sb03pd.obj sb03qd.obj sb03qx.obj sb03qy.obj sb03rd.obj sb03sd.obj \
    sb03sx.obj sb03sy.obj sb03td.obj sb03ud.obj sb04md.obj sb04mr.obj sb04mu.obj \
    sb04mw.obj sb04my.obj sb04nd.obj sb04nv.obj sb04nw.obj sb04nx.obj sb04ny.obj \
    sb04od.obj sb04ow.obj sb04pd.obj sb04px.obj sb04py.obj sb04qd.obj sb04qr.obj \
    sb04qu.obj sb04qy.obj sb04rd.obj sb04rv.obj sb04rw.obj sb04rx.obj sb04ry.obj \
    sb06nd.obj sb08cd.obj sb08dd.obj sb08ed.obj sb08fd.obj sb08gd.obj sb08hd.obj \
    sb08md.obj sb08my.obj sb08nd.obj sb08ny.obj sb09md.obj sb10ad.obj sb10dd.obj \
    sb10ed.obj sb10fd.obj sb10hd.obj sb10id.obj sb10jd.obj sb10kd.obj sb10ld.obj \
    sb10md.obj sb10pd.obj sb10qd.obj sb10rd.obj sb10sd.obj sb10td.obj sb10ud.obj \
    sb10vd.obj sb10wd.obj sb10yd.obj sb10zd.obj sb10zp.obj sb16ad.obj sb16ay.obj \
    sb16bd.obj sb16cd.obj sb16cy.obj select.obj sg02ad.obj sg03ad.obj sg03ax.obj \
    sg03ay.obj sg03bd.obj sg03bu.obj sg03bv.obj sg03bw.obj sg03bx.obj sg03by.obj \
    tb01id.obj tb01iz.obj tb01kd.obj tb01ld.obj tb01md.obj tb01nd.obj tb01pd.obj \
    tb01td.obj tb01ty.obj tb01ud.obj tb01vd.obj tb01vy.obj tb01wd.obj tb01xd.obj \
    tb01xz.obj tb01yd.obj tb01zd.obj tb03ad.obj tb03ay.obj tb04ad.obj tb04ay.obj \
    tb04bd.obj tb04bv.obj tb04bw.obj tb04bx.obj tb04cd.obj tb05ad.obj tc01od.obj \
    tc04ad.obj tc05ad.obj td03ad.obj td03ay.obj td04ad.obj td05ad.obj tf01md.obj \
    tf01mx.obj tf01my.obj tf01nd.obj tf01od.obj tf01pd.obj tf01qd.obj tf01rd.obj \
    tg01ad.obj tg01az.obj tg01bd.obj tg01cd.obj tg01dd.obj tg01ed.obj tg01fd.obj \
    tg01fz.obj tg01hd.obj tg01hx.obj tg01id.obj tg01jd.obj tg01wd.obj  \
    ue01md.obj mb04qb.obj mb03xp.obj mb04tb.obj mb04wd.obj \
    nf01ba.obj nf01bb.obj nf01be.obj nf01bf.obj ib01oy.obj

OBJSF =  

include ../Make.lib.mak



Makefile.mak	: Makefile
	$(SCIDIR)/scripts/Mak2VCMak Makefile

SRC = $(patsubst %.obj,%.c,$(OBJSC))

job	:
	mv $(SRC) src/

# according to compiler, do not optimize the following files

