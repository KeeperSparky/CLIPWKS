@ECHO Off
@ECHO Compiling the source code files...
xpp clipsnum /w /n /m /l /dll
xpp cw_core  /w /n /m /l /dll
xpp export   /w /n /m /l /dll
xpp frm2wks  /w /n /m /l /dll
xpp import   /w /n /m /l /dll
xpp miscwks  /w /n /m /l /dll
xpp printwks /w /n /m /l /dll
xpp readwks  /w /n /m /l /dll
xpp sortwks  /w /n /m /l /dll
xpp writewks /w /n /m /l /dll
cls
@ECHO Creating the def file
xppfilt clipsnum.obj cw_core.obj export.obj frm2wks.obj import.obj miscwks.obj printwks.obj readwks.obj sortwks.obj writewks.obj  /f:clipwks.def
@ECHO Creating the import file
aimplib clipwks.def
@ECHO Creating the DLL and LIB files
alink/dll clipsnum.obj cw_core.obj export.obj frm2wks.obj import.obj miscwks.obj printwks.obj readwks.obj sortwks.obj writewks.obj clipwks.exp /out:clipwks.dll

lib clpwks52 +clipsnum +cw_core +export +frm2wks +import +miscwks +printwks +readwks +sortwks +writewks +treal +ieereal +bit
