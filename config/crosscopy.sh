#!/bin/sh
# copy files from cross compiler directory 
# $1 can be i686-w64-mingw32 or /usr/x86_64-w64-mingw32
# $2 should give the cross compiler version
# glib and gtk

if [ "x$1" = "x" ]; then 
    dist="i686-w64-mingw32"
else
    dist=$1
fi

if [ "x$2" = "x" ]; then 
    version="4.8"
else
    version=$2
fi


if [ -d "/usr/$dist" ]; then
    echo populate bin with dlls from cross compiler 
    cp -f /usr/$dist/bin/icudata53.dll bin/
    cp -f /usr/$dist/bin/icui18n53.dll bin/
    cp -f /usr/$dist/bin/icuuc53.dll bin/
    cp -f /usr/$dist/bin/libamd.dll bin/
    cp -f /usr/$dist/bin/libatk-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libblas.dll bin/
    cp -f /usr/$dist/bin/libcairo-2.dll bin/
    cp -f /usr/$dist/bin/libcdt-5.dll bin/
    cp -f /usr/$dist/bin/libcgraph-6.dll bin/
    cp -f /usr/$dist/bin/libcholmod.dll bin/
    cp -f /usr/$dist/bin/libcolamd.dll bin/
    cp -f /usr/$dist/bin/libcroco-0.6-3.dll bin/
    cp -f /usr/$dist/bin/libenchant.dll bin/
    cp -f /usr/$dist/bin/libexpat-1.dll bin/
    cp -f /usr/$dist/bin/libffi-6.dll bin/
    cp -f /usr/$dist/bin/libfftw3-3.dll bin/
    cp -f /usr/$dist/bin/libFLAC-8.dll bin/
    cp -f /usr/$dist/bin/libfontconfig-1.dll bin/
    cp -f /usr/$dist/bin/libfreetype-6.dll bin/
    cp -f /usr/$dist/bin/libgailutil-18.dll bin/
    cp -f /usr/$dist/bin/libgcc_s_sjlj-1.dll bin/
    cp -f /usr/$dist/bin/libgdkglext-win32-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libgdk_pixbuf-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgdk-win32-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgeoclue-0.dll bin/
    cp -f /usr/$dist/bin/libgio-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libglib-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libglpk.dll bin/
    cp -f /usr/$dist/bin/libgmodule-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgmp-10.dll bin/
    cp -f /usr/$dist/bin/libgnutls-28.dll bin/
    cp -f /usr/$dist/bin/libgobject-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgstapp-0.10-0.dll bin/
    cp -f /usr/$dist/bin/libgstbase-0.10-0.dll bin/
    cp -f /usr/$dist/bin/libgstinterfaces-0.10-0.dll bin/
    cp -f /usr/$dist/bin/libgstpbutils-0.10-0.dll bin/
    cp -f /usr/$dist/bin/libgstreamer-0.10-0.dll bin/
    cp -f /usr/$dist/bin/libgstvideo-0.10-0.dll bin/
    cp -f /usr/$dist/bin/libgtkglext-win32-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libgtksourceview-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgtk-win32-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgvc-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_core-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_dot_layout-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_gdk-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_gtk-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_neato_layout-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_pango-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_poppler-6.dll bin/
    cp -f /usr/$dist/bin/libgvplugin_rsvg-6.dll bin/
    cp -f /usr/$dist/bin/libharfbuzz-0.dll bin/
    cp -f /usr/$dist/bin/libhogweed-2-4.dll bin/
    cp -f /usr/$dist/bin/libintl-8.dll bin/
    cp -f /usr/$dist/bin/libjasper-1.dll bin/
    cp -f /usr/$dist/bin/libjavascriptcoregtk-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libjpeg-8.dll bin/
    cp -f /usr/$dist/bin/liblapack.dll bin/
    cp -f /usr/$dist/bin/liblcms2-2.dll bin/
    cp -f /usr/$dist/bin/liblzma-5.dll bin/
    cp -f /usr/$dist/bin/libnettle-4-6.dll bin/
    cp -f /usr/$dist/bin/libogg-0.dll bin/
    cp -f /usr/$dist/bin/libopenjpeg-1.dll bin/
    cp -f /usr/$dist/bin/libp11-kit-0.dll bin/
    cp -f /usr/$dist/bin/libpango-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpangocairo-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpangoft2-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpangowin32-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpathplan-4.dll bin/
    cp -f /usr/$dist/bin/libpixman-1-0.dll bin/
    cp -f /usr/$dist/bin/libpng15-15.dll bin/
    cp -f /usr/$dist/bin/libpng16-16.dll bin/
    cp -f /usr/$dist/bin/libpoppler-47.dll bin/
    cp -f /usr/$dist/bin/libpoppler-glib-8.dll bin/
    cp -f /usr/$dist/bin/libportaudio-2.dll bin/
    cp -f /usr/$dist/bin/libreadline6.dll bin/
    cp -f /usr/$dist/bin/librsvg-2-2.dll bin/
    cp -f /usr/$dist/bin/libsndfile-1.dll bin/
    cp -f /usr/$dist/bin/libsoup-2.4-1.dll bin/
    cp -f /usr/$dist/bin/libsqlite3-0.dll bin/
    cp -f /usr/$dist/bin/libstdc++-6.dll bin/
    cp -f /usr/$dist/bin/libtasn1-6.dll bin/
    cp -f /usr/$dist/bin/libtermcap-0.dll bin/
    cp -f /usr/$dist/bin/libtiff-5.dll bin/
    cp -f /usr/$dist/bin/libumfpack.dll bin/
    cp -f /usr/$dist/bin/libvorbis-0.dll bin/
    cp -f /usr/$dist/bin/libvorbisenc-2.dll bin/
    cp -f /usr/$dist/bin/libwebkitgtk-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libwinpthread-1.dll bin/
    cp -f /usr/$dist/bin/libxml2-2.dll bin/
    cp -f /usr/$dist/bin/libxslt-1.dll bin/
    cp -f /usr/$dist/bin/zlib1.dll bin/
    cp -f /usr/$dist/bin/zlib1.dll bin/
    cp -f /usr/$dist/bin/libdbus-1-3.dll bin/ 
    cp -f /usr/$dist/bin/libdbus-glib-1-2.dll bin/

    subdirs="lib/gtk-2.0 lib/glib-2.0 lib/gio lib/gdk-pixbuf-2.0 lib/pkcs11 etc share" 
    echo populate directories: lib, etc, share.
    for i in $subdirs ;
    do 
	\rm -fr $i
	\cp -R /usr/$dist/$i $i 
    done
    # clean unused stuffs 
    subdirs="bash-completion cmake common-lisp doc GConf gdb graphite2 gtk-2.0 gtk-doc info inkscape man midori"
    for i in $subdirs ;
    do 
	\rm -fr share/$i
    done
else
    echo directory /usr/$dist does not exists
fi

# copy dlls from cross compiler 

if [ -d "/usr/lib/gcc/$dist/$version" ]; then
    cp -f /usr/lib/gcc/$dist/$version/libgfortran-3.dll bin
    cp -f /usr/lib/gcc/$dist/$version/libquadmath-0.dll bin
else
    echo directory /usr/lib/gcc/$dist/$version  does not exists
fi
