CFLAGS=`pkg-config webkit2gtk-3.0 --cflags ` -Wno-deprecated-declarations

OBJS= main.o \
	BrowserCellRendererVariant.o \
	BrowserWindow.o \
	BrowserSearchBar.o \
	BrowserSettingsDialog.o \
	BrowserDownloadsBar.o \
	BrowserMarshal.o \


all : BrowserMarshal.c BrowserMarshal.h $(OBJS)
	gcc -o Browser $(OBJS) `pkg-config webkit2gtk-3.0 --libs`

BrowserMarshal.c BrowserMarshal.h: browser-marshal.list
	glib-genmarshal --prefix=browser_marshal ./browser-marshal.list --body > BrowserMarshal.c
	glib-genmarshal --prefix=browser_marshal ./browser-marshal.list --header > BrowserMarshal.h

clean:
	$(RM) -f $(OBJS)
