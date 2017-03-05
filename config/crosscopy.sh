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
    if [ $dist = "i686-w64-mingw32" ]; then
      cp -f /usr/$dist/bin/gspawn-win32*.exe bin/
    else
      cp -f /usr/$dist/bin/gspawn-win64*.exe bin/
    fi
    cp -f /usr/$dist/bin/icudata56.dll bin/
    cp -f /usr/$dist/bin/icui18n56.dll bin/
    cp -f /usr/$dist/bin/icuuc56.dll bin/
    cp -f /usr/$dist/bin/libatk-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libblas.dll bin/
    cp -f /usr/$dist/bin/libcairo-2.dll bin/
    cp -f /usr/$dist/bin/libcdt-5.dll bin/
    cp -f /usr/$dist/bin/libcgraph-6.dll bin/
    cp -f /usr/$dist/bin/libcroco-0.6-3.dll bin/
    cp -f /usr/$dist/bin/libenchant.dll bin/
    cp -f /usr/$dist/bin/libexpat-1.dll bin/
    cp -f /usr/$dist/bin/libffi-6.dll bin/
    cp -f /usr/$dist/bin/libfftw3-3.dll bin/
    cp -f /usr/$dist/bin/libFLAC-8.dll bin/
    cp -f /usr/$dist/bin/libfontconfig-1.dll bin/
    cp -f /usr/$dist/bin/libfreetype-6.dll bin/
    cp -f /usr/$dist/bin/libgailutil-18.dll bin/
    cp -f /usr/$dist/bin/libgdkglext-win32-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libgdk_pixbuf-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgdk-win32-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgeoclue-0.dll bin/
    cp -f /usr/$dist/bin/libgio-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libglib-2.0-0.dll bin/
    cp -f /usr/$dist/bin/*glpk*.dll bin/
    cp -f /usr/$dist/bin/libgmodule-2.0-0.dll bin/
    cp -f /usr/$dist/bin/libgmp-10.dll bin/
    cp -f /usr/$dist/bin/libgnutls-30.dll bin/
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
    cp -f /usr/$dist/bin/libhogweed-4-2.dll bin/
    cp -f /usr/$dist/bin/libintl-8.dll bin/
    cp -f /usr/$dist/bin/libjasper-4.dll bin/
    cp -f /usr/$dist/bin/libjavascriptcoregtk-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libjpeg-8.dll bin/
    cp -f /usr/$dist/bin/liblapack.dll bin/
    cp -f /usr/$dist/bin/liblcms2-2.dll bin/
    cp -f /usr/$dist/bin/liblzma-5.dll bin/
    cp -f /usr/$dist/bin/libnettle-6-2.dll bin/
    cp -f /usr/$dist/bin/libogg-0.dll bin/
    cp -f /usr/$dist/bin/libopenjpeg-1.dll bin/
    cp -f /usr/$dist/bin/libp11-kit-0.dll bin/
    cp -f /usr/$dist/bin/libpango-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpangocairo-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpangoft2-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpangowin32-1.0-0.dll bin/
    cp -f /usr/$dist/bin/libpathplan-4.dll bin/
    cp -f /usr/$dist/bin/libpixman-1-0.dll bin/
    cp -f /usr/$dist/bin/libpng16-16.dll bin/
    cp -f /usr/$dist/bin/libpoppler-56.dll bin/
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
    cp -f /usr/$dist/bin/libamd.2.4.5.dll bin
    cp -f /usr/$dist/bin/libbtf.1.2.5.dll bin
    cp -f /usr/$dist/bin/libcamd.2.4.5.dll bin
    cp -f /usr/$dist/bin/libccolamd.2.9.5.dll bin
    cp -f /usr/$dist/bin/libcholmod.3.0.10.dll bin
    cp -f /usr/$dist/bin/libcolamd.2.9.5.dll bin
    cp -f /usr/$dist/bin/libcxsparse.3.1.8.dll bin
    cp -f /usr/$dist/bin/libklu.1.3.7.dll bin
    cp -f /usr/$dist/bin/libldl.2.2.5.dll bin
    cp -f /usr/$dist/bin/libmetis.5.1.0.dll bin
    cp -f /usr/$dist/bin/librbio.2.2.5.dll bin
    cp -f /usr/$dist/bin/libspqr.2.0.6.dll bin
    cp -f /usr/$dist/bin/libsuitesparseconfig.4.5.2.dll bin
    cp -f /usr/$dist/bin/libumfpack.5.7.5.dll bin
    if [ -f /usr/$dist/bin/libgcc_s_sjlj-1.dll ]; then 
      cp -f /usr/$dist/bin/libgcc_s_sjlj-1.dll bin
    fi
    
    subdirs="lib/gtk-2.0 lib/glib-2.0 lib/gio lib/gdk-pixbuf-2.0 lib/pkcs11 lib/p11-kit"
    echo populate directories: $subdirs
    for i in $subdirs ;
    do 
	\rm -fr $i
	\cp -R /usr/$dist/$i $i 
    done
    subdirs="etc share" 
    echo populate directories: $subdirs
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
    # clean unused icon themes
    subdirs="gnome oxygen Tango"
    for i in $subdirs ;
    do
	if [ -f share/icons/$1 ]; then 
	    \rm -fr share/icons/$i
	fi
    done
else
    echo directory /usr/$dist does not exists
fi

# copy dlls from cross compiler 

if [ -d "/usr/lib/gcc/$dist/$version" ]; then
    cp -f /usr/lib/gcc/$dist/$version/libgfortran-3.dll bin
    cp -f /usr/lib/gcc/$dist/$version/libquadmath-0.dll bin
    if [ -f "/usr/lib/gcc/$dist/$version/libgcc_s_seh-1.dll" ]; then 
	cp -f /usr/lib/gcc/$dist/$version/libgcc_s_seh-1.dll bin/
    fi
    if [ -f "/usr/lib/gcc/$dist/$version/libgcc_s_sjlj-1.dll" ]; then 
	cp -f /usr/lib/gcc/$dist/$version/libgcc_s_sjlj-1.dll bin/
    fi
    cp -f /usr/lib/gcc/$dist/$version/libgomp-1.dll bin/
else
    echo directory /usr/lib/gcc/$dist/$version  does not exists
fi

chmod +x bin/*.dll


# change the pathes in pixbuf loaders

loaders_cache=lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
if [ -f "$loaders_cache" ]; then
    echo "modify loaders.cache"
    sed -e "s+Z:/usr/$dist+..+" /usr/$dist/$loaders_cache > $loaders_cache
fi
