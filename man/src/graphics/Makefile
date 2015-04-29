SHELL=/bin/csh

FIG=$(wildcard *.fig)

all :: xsetechfig.tex 

clean	::
	\rm -f $(patsubst %.fig,%.eps,$(wildcard *.fig)) 
	\rm -f $(patsubst %.fig,%.tex,$(wildcard *.fig)) 	

xsetechfig.tex : xsetechfig.fig 
	make -f Make.fig MAG=1.0 $@

