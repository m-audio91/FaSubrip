#! /usr/bin/bash
# first compile for every target and then run this scipt to get the zip file.
# note: this script uses relative paths. so it is necessary to open the Terminal in the same folder as this script.

name="fasubrip"
namec="FaSubrip"
ver="1.2.3"
archive="$name""_v$ver""_cross_platform"
hashfile="./$ver/$archive.hash.txt"
lin32bin="$name-i386-linux"
lin64bin="$name-x86_64-linux"
lin64qt5bin="$name-x86_64-linux-Qt5"
winbin="$name-i386-win32.exe"
macosbin="$name-x86_64-darwin"
arm7bin="$name-arm-linux"
qt5note="qt5 runtime dependency note.txt"
macosnote="macos unidentified developer note.txt"

echo start
rm -r ./$ver
mkdir ./$ver
touch $hashfile
echo $archive >> $hashfile
echo SHA256 valuse are calculated using sha256sum utility in default mode >> $hashfile
echo "" >> $hashfile

#gui
if [ -f "../$lin32bin" ]; then 
  mkdir -p ./$ver/linux32/icon
  cp ../$lin32bin ./$ver/linux32/$name
  cp ../extra/icon/*.png ./$ver/linux32/icon
  sha256sum ./$ver/linux32/$name >> $hashfile
fi
if [ -f "../$lin64bin" ]; then 
  mkdir -p ./$ver/linux64/icon
  cp ../$lin64bin ./$ver/linux64/$name
  cp ../extra/icon/*.png ./$ver/linux64/icon
  sha256sum ./$ver/linux64/$name >> $hashfile
fi
if [ -f "../$lin64qt5bin" ]; then 
  mkdir -p ./$ver/linux64-qt5/icon
  cp ../$lin64qt5bin ./$ver/linux64-qt5/$name
  cp ../extra/icon/*.png ./$ver/linux64-qt5/icon
  cp "./$qt5note" "./$ver/linux64-qt5/$qt5note" 
  sha256sum ./$ver/linux64-qt5/$name >> $hashfile
fi
if [ -f "../$winbin" ]; then 
  mkdir -p ./$ver/windows
  cp ../$winbin ./$ver/windows/$namec.exe
  sha256sum ./$ver/windows/$namec.exe >> $hashfile
fi
if [ -f "../$macosbin" ]; then 
  mkdir -p ./$ver/macos
  cp -r ../$namec.app ./$ver/macos/$namec.app
  cp ../$macosbin ./$ver/macos/$namec.app/Contents/MacOS/$namec
  cp "./$macosnote" "./$ver/macos/$macosnote"
  sha256sum ./$ver/macos/$namec.app/Contents/MacOS/$namec >> $hashfile
fi
if [ -f "../$arm7bin" ]; then 
  mkdir -p ./$ver/arm7/icon
  cp ../$arm7bin ./$ver/arm7/$name
  cp ../extra/icon/*.png ./$ver/arm7/icon
  sha256sum ./$ver/arm7/$name >> $hashfile
fi

#final output
cd ./$ver
zip -r ./$archive.zip ./linux32 ./linux64 ./linux64-qt5 ./windows ./macos ./arm7
rm -r ./linux32 ./linux64 ./linux64-qt5 ./windows ./macos ./arm7

echo done

