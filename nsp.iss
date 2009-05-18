; Copyright (C) 2002-2004 Todd Kulesza <todd@dropline.net>
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

; HOWTO CREATE THE INSTALLER
;
; First, you'll need My Inno Setup Extensions, the program used to
; generate the installation executable.  It can be freely downloaded from
; http://www.wintax.nl/isx/.  Once that's installed, you can use it to
; open this file, make any changed you feel are necessary, and hit "compile"
; to create a new installation file.  This .iss file must be placed in the
; root of the GTK+ library hierarchy, i.e. if you have C:\Src\GTK+ with all of
; the GTK+ .dll files in it, and sub-directories for bin, lib, etc, and share,
; then this file must also be in C:\src\GTK+.  The documentation for My Inno
; Setup Extensions is very complete and clearly explains what all of the
; options listed below do.

[Setup]
AppName=Nsp
AppVerName=Nsp 1.0
AppVersion=1.0-2
AppPublisherURL=http://www.gtk.org
AppSupportURL=http://www.gtk.org
AppUpdatesURL=http://www.gtk.org
CreateAppDir=yes
; pf for program files
DefaultDirName={pf}\nsp
DefaultGroupName=Nsp
; icon for uninstall
UninstallDisplayIcon={app}\nsp-win.exe
LicenseFile=COPYING
;AdminPrivilegesRequired=yes
;DisableDirPage=yes
;DisableProgramGroupPage=yes
;DisableStartupPrompt=yes
WindowShowCaption=yes
;BackColor=$FF8200
BackColor=clPurple
BackColor2=clBlack
Compression=bzip/9
OutputDir=..
OutputBaseFilename=nsp2-2009-05-18

;;[Registry]
;;Root: HKLM; Subkey: "Software\Nsp"; Flags: uninsdeletekeyifempty
;;Root: HKLM; Subkey: "Software\Nsp\1.0"; Flags: uninsdeletekey
;;Root: HKLM; Subkey: "Software\Nsp\1.0"; ValueType: string; ValueName: "Path"; ValueData: "{app}"; Flags: uninsdeletekey
;;Root: HKCU; Subkey: "Software\Nsp\1.0"; Flags: uninsdeletekey
;;Root: HKCU; Subkey: "Software\Nsp\1.0"; ValueType: string; ValueName: "Path"; ValueData: "{app}"; Flags: uninsdeletekey

[Files]
Source: "bin\*.*"; DestDir: "{app}\bin"; Flags: recursesubdirs
Source: "src\include\*.*"; DestDir: "{app}\include"; Flags: recursesubdirs
Source: "config\*.*"; DestDir: "{app}\config"; Flags: recursesubdirs
Source: "lib\*.*"; DestDir: "{app}\lib"; Flags: recursesubdirs
Source: "etc\*.*"; DestDir: "{app}\etc"; Flags: recursesubdirs
Source: "man\html\generated\*.*"; DestDir: "{app}\man\html\generated"; Flags: recursesubdirs
Source: "macros\*.*"; DestDir: "{app}\macros\"; Flags: recursesubdirs
Source: "tests\*.*"; DestDir: "{app}\tests\"; Flags: recursesubdirs
Source: "demos\*.*"; DestDir: "{app}\demos\"; Flags: recursesubdirs
Source: "libs\NperiPos.ps"; DestDir: "{app}\libs\"; Flags: recursesubdirs
Source: "..\nsp2-jpc\scicos_work\*.*"; DestDir: "{app}\demos\scicos_work"; Flags: recursesubdirs

[Icons]
Name: "{commondesktop}\Nsp"; Filename: "{app}\bin\nsp-win.exe"
Name: "{commonprograms}\Nsp\Nsp"; Filename: "{app}\bin\nsp-win.exe"
Name: "{commonprograms}\Nsp\Uninstall Nsp"; Filename: "{uninstallexe}"


