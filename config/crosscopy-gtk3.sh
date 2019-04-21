#!/bin/sh
# copy files from cross compiler directory 
# $1 can be i686-w64-mingw32 or /usr/x86_64-w64-mingw32
# $2 should give the cross compiler version
# glib and gtk

if [ "x$1" = "x" ]; then 
    dist="i686-w64-mingw32"
    dist="mingw32"
    usrdist="/mingw32";
else
    dist=$1
    dist="mingw64"
    usrdist="/mingw64";
fi

if [ "x$2" = "x" ]; then 
    version="4.8"
else
    version=$2
fi

if [ -d "$usrdist" ]; then
    echo populate bin with dlls from cross compiler
    if [ $dist = "mingw32" ]; then
      cp -f $usrdist/bin/gspawn-win32*.exe bin/
    else
      cp -f $usrdist/bin/gspawn-win64*.exe bin/
    fi
    cp -f $usrdist/bin/libamd.dll bin/
    cp -f $usrdist/bin/libatk-1.0-0.dll bin/
    cp -f $usrdist/bin/libblas.dll bin/
    cp -f $usrdist/bin/libbz2-1.dll bin/
    cp -f $usrdist/bin/libcairo-2.dll bin/
    cp -f $usrdist/bin/libcairo-gobject-2.dll bin/
    cp -f $usrdist/bin/libcamd.dll bin/
    cp -f $usrdist/bin/libccolamd.dll bin/
    cp -f $usrdist/bin/libcdt-5.dll bin/
    cp -f $usrdist/bin/libcgraph-6.dll bin/
    cp -f $usrdist/bin/libcholmod.dll bin/
    cp -f $usrdist/bin/libcolamd.dll bin/
    cp -f $usrdist/bin/libcroco-0.6-3.dll bin/
    cp -f $usrdist/bin/libdatrie-1.dll bin/
    cp -f $usrdist/bin/libdbus-1-3.dll bin/
    cp -f $usrdist/bin/libdbus-glib-1-2.dll bin/
    cp -f $usrdist/bin/libenchant-2.dll bin/
    cp -f $usrdist/bin/libepoxy-0.dll bin/
    cp -f $usrdist/bin/libexpat-1.dll bin/
    cp -f $usrdist/bin/libffi-6.dll bin/
    cp -f $usrdist/bin/libfftw3-3.dll bin/
    cp -f $usrdist/bin/libFLAC-8.dll bin/
    cp -f $usrdist/bin/libfontconfig-1.dll bin/
    cp -f $usrdist/bin/libfreetype-6.dll bin/
    cp -f $usrdist/bin/libfribidi-0.dll bin/
    cp -f $usrdist/bin/libgailutil-3-0.dll bin/
    cp -f $usrdist/bin/libgif-7.dll bin/
    cp -f $usrdist/bin/libgcc_s_seh-1.dll bin/
    cp -f $usrdist/bin/libgdk_pixbuf-2.0-0.dll bin/
    cp -f $usrdist/bin/libgdk-3-0.dll bin/
    cp -f $usrdist/bin/libgdk-win32-2.0-0.dll bin/
    cp -f $usrdist/bin/libgeoclue-0.dll bin/
    cp -f $usrdist/bin/libgfortran-5.dll bin/
    cp -f $usrdist/bin/libgio-2.0-0.dll bin/
    cp -f $usrdist/bin/libglib-2.0-0.dll bin/
    cp -f $usrdist/bin/libglpk-40.dll bin/
    cp -f $usrdist/bin/libgmodule-2.0-0.dll bin/
    cp -f $usrdist/bin/libgmp-10.dll bin/
    cp -f $usrdist/bin/libgnutls-30.dll bin/
    cp -f $usrdist/bin/libgobject-2.0-0.dll bin/
    cp -f $usrdist/bin/libgomp-1.dll bin/
    cp -f $usrdist/bin/libgraphite2.dll bin/
    cp -f $usrdist/bin/libgstapp-1.0-0.dll bin/
    cp -f $usrdist/bin/libgstaudio-1.0-0.dll bin/
    cp -f $usrdist/bin/libgstbase-1.0-0.dll bin/
    cp -f $usrdist/bin/libgstfft-1.0-0.dll bin/
    cp -f $usrdist/bin/libgstpbutils-1.0-0.dll bin/
    cp -f $usrdist/bin/libgstreamer-1.0-0.dll bin/
    cp -f $usrdist/bin/libgsttag-1.0-0.dll bin/
    cp -f $usrdist/bin/libgstvideo-1.0-0.dll bin/
    cp -f $usrdist/bin/libgtk-3-0.dll bin/
    cp -f $usrdist/bin/libgtksourceview-3.0-1.dll bin/
    cp -f $usrdist/bin/libgtk-win32-2.0-0.dll bin/
    cp -f $usrdist/bin/libgvc-6.dll bin/
    cp -f $usrdist/bin/libgvplugin_*.dll bin/
    cp -f $usrdist/bin/libharfbuzz-0.dll bin/
    cp -f $usrdist/bin/libharfbuzz-icu-0.dll bin/
    cp -f $usrdist/bin/libiconv-2.dll bin/
    cp -f $usrdist/bin/libicudt6*.dll bin/
    cp -f $usrdist/bin/libicuin6*.dll bin/
    cp -f $usrdist/bin/libicuio6*.dll bin/
    cp -f $usrdist/bin/libicutest6*.dll bin/
    cp -f $usrdist/bin/libicutu6*.dll bin/
    cp -f $usrdist/bin/libicuuc6*.dll bin/
    cp -f $usrdist/bin/libidn2-0.dll bin/
    cp -f $usrdist/bin/libintl-8.dll bin/
    cp -f $usrdist/bin/libjasper-4.dll bin/
    cp -f $usrdist/bin/libjavascriptcoregtk-3.0-0.dll bin/
    cp -f $usrdist/bin/libjpeg-8.dll bin/
    cp -f $usrdist/bin/liblapack.dll bin/
    cp -f $usrdist/bin/liblcms2-2.dll bin/
    cp -f $usrdist/bin/libltdl-7.dll bin/
    cp -f $usrdist/bin/liblzma-5.dll bin/
    cp -f $usrdist/bin/libmetis.dll bin/
    cp -f $usrdist/bin/libogg-0.dll bin/
    cp -f $usrdist/bin/libopenblas.dll bin/
    cp -f $usrdist/bin/liborc-0.4-0.dll bin/
    cp -f $usrdist/bin/libp11-kit-0.dll bin/
    cp -f $usrdist/bin/libpango-1.0-0.dll bin/
    cp -f $usrdist/bin/libpangocairo-1.0-0.dll bin/
    cp -f $usrdist/bin/libpangoft2-1.0-0.dll bin/
    cp -f $usrdist/bin/libpangowin32-1.0-0.dll bin/
    cp -f $usrdist/bin/libpathplan-4.dll bin/
    cp -f $usrdist/bin/libpcre-1.dll bin/
    cp -f $usrdist/bin/libpixman-1-0.dll bin/
    cp -f $usrdist/bin/libpng16-16.dll bin/
    cp -f $usrdist/bin/libpoppler-glib-8.dll bin/
    cp -f $usrdist/bin/libportaudio-2.dll bin/
    cp -f $usrdist/bin/libpsl-5.dll bin/
    cp -f $usrdist/bin/libqhull.dll bin/
    cp -f $usrdist/bin/libquadmath-0.dll bin/
    cp -f $usrdist/bin/libreadline8.dll bin/
    cp -f $usrdist/bin/librsvg-2-2.dll bin/
    cp -f $usrdist/bin/libsndfile-1.dll bin/
    cp -f $usrdist/bin/libsoup-2.4-1.dll bin/
    cp -f $usrdist/bin/libspeex-1.dll bin/
    cp -f $usrdist/bin/libsqlite3-0.dll bin/
    cp -f $usrdist/bin/libstdc++-6.dll bin/
    cp -f $usrdist/bin/libsuitesparseconfig.dll bin/
    cp -f $usrdist/bin/libsystre-0.dll bin/
    cp -f $usrdist/bin/libtasn1-6.dll bin/
    cp -f $usrdist/bin/libtermcap-0.dll bin/
    cp -f $usrdist/bin/libthai-0.dll bin/
    cp -f $usrdist/bin/libtiff-5.dll bin/
    cp -f $usrdist/bin/libtre-5.dll bin/
    cp -f $usrdist/bin/libumfpack.dll bin/
    cp -f $usrdist/bin/libunistring-2.dll bin/
    cp -f $usrdist/bin/libvorbis-0.dll bin/
    cp -f $usrdist/bin/libvorbisenc-2.dll bin/
    cp -f $usrdist/bin/libwebkitgtk-3.0-0.dll bin/
    cp -f $usrdist/bin/libwebp-7.dll bin/
    cp -f $usrdist/bin/libwinpthread-1.dll bin/
    cp -f $usrdist/bin/libxml2-2.dll bin/
    cp -f $usrdist/bin/libxslt-1.dll bin/
    cp -f $usrdist/bin/zlib1.dll bin/
    cp -f $usrdist/bin/libxdot-4.dll bin/
    cp -f $usrdist/bin/libpoppler-8*.dll bin/
    cp -f $usrdist/bin/libzstd.dll bin/
    cp -f $usrdist/bin/libproxy-1.dll bin/
    cp -f $usrdist/bin/libgailutil-18.dll bin/
    cp -f $usrdist/bin/libgd.dll bin/
    cp -f $usrdist/bin/libIL.dll bin/
    cp -f $usrdist/bin/libILU.dll bin/
    cp -f $usrdist/bin/libXpm-noX4.dll bin/
    cp -f $usrdist/bin/libHalf-2_3.dll bin/
    cp -f $usrdist/bin/libIlmImf-2_3.dll bin/
    cp -f $usrdist/bin/libsquish.dll bin/
    cp -f $usrdist/bin/libIex-2_3.dll bin/
    cp -f $usrdist/bin/libIlmThread-2_3.dll bin/
    cp -f $usrdist/bin/libImath-2_3.dll bin/
    cp -f $usrdist/bin/libgts-0-7-5.dll bin/
    cp -f $usrdist/bin/libnspr4.dll bin/
    cp -f $usrdist/bin/libhogweed-4.dll bin/
    cp -f $usrdist/bin/libnettle-6.dll bin/
    cp -f $usrdist/bin/libcurl-4.dll bin/
    cp -f $usrdist/bin/libopenjp2-7.dll bin/
    cp -f $usrdist/bin/nss3.dll bin/
    cp -f $usrdist/bin/smime3.dll bin/
    cp -f $usrdist/bin/libplc4.dll  bin/
    cp -f $usrdist/bin/libplds4.dll bin/
    cp -f $usrdist/bin/nssutil3.dll bin/
    cp -f $usrdist/bin/libbrotlidec.dll bin/
    cp -f $usrdist/bin/libcrypto-1_1-x64.dll bin/
    cp -f $usrdist/bin/libnghttp2-14.dll bin/
    cp -f $usrdist/bin/libssl-1_1-x64.dll bin/
    cp -f $usrdist/bin/libbrotlicommon.dll bin/
    cp -f $usrdist/bin/libspqr.dll bin/
    if [ -f "$usrdist/bin/libbson-1.0.dll" ]; then 
	cp -f $usrdist/bin/libbson-1.0.dll bin/
    fi
    if [ -f "$usrdist/bin/libmongoc-1.0.dll" ]; then 
	cp -f $usrdist/bin/libmongoc-1.0.dll bin/
    fi
    # copy and change the name 
    cp -f $usrdist/bin/libgdiplus-0.dll bin/gdiplus.dll
    subdirs="lib/gtk-3.0 lib/glib-2.0 lib/gio lib/gdk-pixbuf-2.0 lib/pkcs11 lib/p11-kit"
    echo populate directories: $subdirs
    for i in $subdirs ;
    do 
    	\rm -fr $i
    	\cp -R $usrdist/$i lib/
    done
    subdirs="etc share" 
    echo populate directories: $subdirs
    for i in $subdirs ;
    do 
    	\rm -fr $i
    	\cp -R $usrdist/$i $i
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
	if [ -d share/icons/$i ]; then
	    \rm -fr share/icons/$i
	fi
    done
else
    echo directory $usrdist does not exists
fi

# this is not used with msys2 the dlls are already in bin
# copy dlls from cross compiler 
# take care that dll from cross compiler can be older than
# some opensuze dlls ex libgcc_s_sjlj-1.dll or libstdc++-6.dll
# because cross compiler is 7.3 and opensuze is 8.2

if [ -d "/usr/lib/gcc/$dist/$version" ]; then
    cp -f /usr/lib/gcc/$dist/$version/libgfortran-4.dll bin
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

# for "i686-w64-mingw32" we must use libgcc_s_sjlj-1.dll from opensuze
# to avoid a message about missing symbol when starting help in nsp
# maybe we should do the same for x86_64 version ?

if [ $dist = "mingw32" ]; then
    if [ -f $usrdist/bin/libgcc_s_sjlj-1.dll ]; then 
      cp -f $usrdist/bin/libgcc_s_sjlj-1.dll bin
    fi
fi

chmod +x bin/*.dll

# change the pathes in pixbuf loaders

# loaders_cache=lib/gdk-pixbuf-2.0/2.10.0/loaders.cache
# if [ -f "$loaders_cache" ]; then
#     echo "modify loaders.cache"
#     sed -e "s+Z:$usrdist+..+" $usrdist/$loaders_cache > $loaders_cache
# fi


