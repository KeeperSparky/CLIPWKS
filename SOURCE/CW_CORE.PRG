* Program.......: Cw_core.prg
* Authors.......: Joseph D. Booth
* Copyright.....: 1998 Joseph D. Booth, All rights reserved.
* Application...: CLIPWKS++
* Version.......: 1.1
* Compile.......: xpp Cw_core /l /m /w /n
*                 Lc(nRow,nColumn)
*                 Lclose(aWks)
*                 Lr(nTrow,nTcolumn,nBrow,nBcolumn)
*                 Liwks(cFilename|nHandle)
*                 Lversion(cFilename|nHandle)
*                 Lmakerange(cCell_range)
*                 Lcell(cCell)
*                 Lrelative(nCoord1,nCoord2)
*                 Lstring(cString,nLength)
*                 Bread(nBytes,nHandle)
*                 Bwrite(cString,aWks,lFlush)
*
* Special Thanks: Ira Emus -
****************************************************************************
#include "CLIPWKS.CH"
#include "FILEIO.CH"

#define  FLUSH      .T.


STATIC   aErased   := {}







*****************************************************************************

* Function:   Lc()
* Purpose:    To convert row and column coordinates to spreadsheet notation
* Syntax:     Lc( nRow,nColumn [,nSheet] )
* Arguments:  nRow     - Row number
*             nColumn  - Column number
*             nSheet   - Worksheet number, Lotus 3.x only
*
* Notes:      Lc() is used to convert from row and column
*             notation into spreadsheet cell addresses.  It is
*             most often used in loops, as shown in the example below:
*
*             for k=1 to 10
*                goto k              // Goto record 'k'
*                for j=1 to 5
*                   Lput( aWks,fieldget(j),lc(k,j) )
*                next
*             next
*
*             The row may be an integer between 1 and 8192 and
*             the column an integer between 1 and 256.
* Returns:    cCell_address
* See Also:   lr()
********************************
function lc(nRow,nColumn,nSheet)

LOCAL retval:="", temp, remd

DEFAULT nSheet TO 0

if valtype(nRow)=="N" .and. valtype(nColumn)=="N"  // Both parameters numeric?
   if (nRow >=1 .and. nRow<=MAX_ROWS) .and. ;      // Are parameters with
      (nColumn>=1 .and. nColumn<=MAX_COLS)         // range of spreadsheet?

      if nColumn <= 26                             // Column 26 or less ?
         retval := chr(nColumn+64)

      else
         temp  := int(nColumn/26)                   // Compute first letter
         remd  := nColumn % 26                      // and second letter and
         retval:= if(remd=0,chr(temp+63)+"Z",;
                    chr(temp+64)+chr(remd+64))
      endif

      retval += ltrim(trim(str(int(nRow),4)))       // Put in row number

      if pcount() = 3

         if nSheet > 0 .and. nSheet <= 26              // Lotus 3, spreadsheet
            retval := chr(nSheet+64)+":"+retval        // number
         else
            temp   := int(nSheet/26)                   // Compute first letter
            remd   := nSheet % 26                      // and second letter and
            retval := if(remd=0,chr(temp+63)+"Z",;
                       chr(temp+64)+chr(remd+64))+":"+retval
         endif
      endif

   endif
endif
return retval
*****************************************************************************
*±±± End of Lc ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

                    

* Function:   Lclose()
* Purpose:    To write the EOF op code and close the file handle
* Syntax:     Lclose( aSpreadsheet )
* Arguments:  aSpreadsheet     - Spreadsheet handle array
* Notes:      After any spreadsheet is opened, either through
*             Lcreate() or Lread(), you must close it to release
*             its buffer and its handle.  If you do not close a
*             newly created spreadsheet, there is a good chance
*             that some data will not be written out to the
*             disk. Be sure to close all spreadsheets after you
*             are done with them.
* Returns:    lSuccess
*
*********************
function lclose(aWks)

LOCAL retval:=ERR_INVALID_PARAMETERS

if valtype(aWks) == "A" .and. !empty(aWks)       // Valid parameter ?

   if empty(aErased)
      if valtype(WKS_TYPE) == "C"                // If new file, then
         if substr(WKS_VERSION,1,1)=="E"
            Bwrite(E2_EOF_OPCODE,aWks,FLUSH)     // add an EOF opcode
         else
            Bwrite(EOF_OPCODE,aWks,FLUSH)        // add an EOF opcode
         endif
      endif
   else
                                                 // Re-write the spreadsheet
   endif

   fclose(WKS_HANDLE)                            // Close the worksheet
   retval := if(Ferror()=0,ALL_OK,ERR_IO_ERROR)  // Update return status
   aWks   := {}                                  // Clear out the array
endif
return retval
*****************************************************************************
*±±± End of Lclose ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lext()
* Purpose:    To determine the file extension for the spreadsheet version
* Syntax:     Lext( cVersion )
* Arguments:  cVersion  - L1  Lotus 1.x
*                       - L2  Lotus 2.x
*                       - L3  Lotus 3.x
*                       - QU  Quattro
*                       - QP  Quattro Pro
*                       - E2  Excel
*                       - E3  Excel for Windows
* Returns:    cExtension
**************************
function lext( cVersion )
LOCAL cExt := ".WK1"
do case
case cVersion == "QP"
  cExt := ".WQ1"
case cVersion == "QU"
  cExt := ".WKQ"
case cVersion == "L1"
  cExt := ".WKS"
case cVersion == "L3"
  cExt := ".WK3"
case cVersion == "L4"
  cExt := ".WK4"
case cVersion == "E2" .or. cVersion == "E3" .or. cVersion == "E4"
  cExt := ".XLS"
otherwise
  cExt := ".WK1"
endcase
return cExt
*****************************************************************************
*±±± End of Lext ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lcformat()
* Purpose:    To convert a cell format into an integer byte value
* Syntax:     Lcformat( cString )
* Arguments:  cString  - D1  yy-mmm-dd
*                      - D2  dd-mmm
*                      - D3  mmm-yy
*                      - Fx  Fixed, x decimal places
*                      - Sx  Sci Notation, x decimal places
*                      - Cx  Currency, x decimal places
*                      - Px  Percent, x decimal places
*                      - ,x  Comma, x decimal places
* Returns:    nValue
*********************************

function lcformat( cString,aWks,lProtect )
LOCAL nValue   := 0
LOCAL cType    := substr(cString,1,1)
LOCAL nPlaces  := val(substr(cString,2))

if WKS_VERSION  == "E3" .or. WKS_VERSION == "E4"
   do case
   case cString == "D2"
      nValue :=20
   case cString == "D3"
      nValue :=21
   case cString == "P0"
      nValue :=22
   case substr(cString,1,1) == "P"
      nValue :=23
   case cString == ",0"
      nValue :=24
   case substr(cString,1,1) == ","
      nValue :=25
   case cString == "C0"
      nValue :=26
   case substr(cString,1,1) == "C"
      nValue :=27
   case substr(cString,1,1) == "S"
      nValue = 28
   case cString == "F0"
      nValue :=29
   case substr(cString,1,1) == "F"
      nValue :=30
   endcase
elseif WKS_VERSION == "E2"
   do case
   case cString == "D1"
      nValue := 13
   case cString == "D2"
      nValue := 15
   case cString == "D3"
      nValue := 16
   case cString == "P0"
      nValue := 10
   case substr(cString,1,1) == "P"
      nValue := 11
   case cString == ",0"
      nValue := 4
   case substr(cString,1,1) == ","
      nValue := 5
   case cString == "C0"
      nValue := 6
   case substr(cString,1,1) == "C"
      nValue := 7
   case substr(cString,1,1) == "S"
      nValue = 12
   case cString == "F0"
      nValue := 2
   case substr(cString,1,1) == "F"
      nValue := 3
   endcase
else
   if lProtect == NIL
      if (WKS_PROTECTION=="P")              // If protection has been enabled
         nValue +=128
      endif
   else
      if lProtect
         nValue += 128
      else
         if nValue > 127
            nValue -= 128
         endif
      endif
   endif

   if cType == "D"                            // A date format cell
      nValue += (113 + nPlaces )
   else
      nValue += at(cType,"SCP,")*16+nPlaces   // Other cell formats, defaults
   endif                                      // to FIXED
endif
return nValue
*****************************************************************************
*±±± End of LcFormat ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lr()
* Purpose:    To convert row and column coordinates to spreadsheet range
* Syntax:     Lr( nTopRow,nTopColumn,nBottomRow,nBottomColumn[,nSheet] )
* Arguments:  nTopRow       - Row number
*             nTopColumn    - Column number
*             nBottomRow    - Bottom row number
*             nBottomColumn - Bottom column number
*             nSheet        - Spreadsheet number, Lotus 3.x only
* Notes:      Lr() takes four coordinates and converts them into
*             a cell address range.  It is used to allow the
*             program to work in terms of numeric row and
*             columns and let CLIPWKS handle the translation
*             into spreadsheet syntax.
* Returns:    cRange_address
* See Also:   Lc()
*************************************************
function lr(nTrow,nTcolumn,nBrow,nBcolumn,nSheet)

LOCAL part_1,part_2
DEFAULT nSheet TO 0
if pcount() = 5
   part_1 := lc(nTrow,nTcolumn,nSheet)              // Pass coordinates to
   part_2 := lc(nBrow,nBcolumn,nSheet)              // lc() function call
else
   part_1 := lc(nTrow,nTcolumn)                     // Pass coordinates to
   part_2 := lc(nBrow,nBcolumn)                     // lc() function call
endif

if ! empty(part_1) .and. ! empty(part_2)         // If both are valid
   return part_1 + ".." + part_2                 // concatenate with ..
endif
return ""
*****************************************************************************
*±±± End of Lr ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Liswks()
* Purpose:    To test if a file is a valid spreadsheet
* Syntax:     Liswks( cFile|nHandle )
* Arguments:  cFile         - File name
*             nHandle       - Handle file is opened on
* Notes:      Liswks() tests the beginning and ending bytes of
*             the file to see if the file appears to be a valid
*             spreadsheet file.  It is useful to confirm the
*             validity of a file if your application allows a
*             user to type in a spreadsheet name.
* Returns:    lSuccess
* See Also:   Lversion()
**************************
function liswks(xFilename)
return !empty(lVersion(xFileName))
*****************************************************************************
*±±± End of Liswks ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Lversion()
* Purpose:    To determine the version of the spreadsheet
* Syntax:     Lversion( cFile|nHandle )
* Arguments:  cFile         - File name
*             nHandle       - Handle file is opened on
* Notes:      If the file|handle appears to be a valid
*             spreadsheet, this function will return a two
*             character code indicating the version of
*             spreadsheet it appears to be.
*
* Returns:    cVersion      - L1  Lotus 1.x
*                           - L2  Lotus 2.x
*                           - L3  Lotus 3.x
*                           - QU  Quattro
*                           - QP  Quattro Pro
*                           - E2  Excel
*                           - E3  Excel for Windows
* See Also:   Liswks()
****************************
function lversion(xFilename)

LOCAL retval:="", wkshandle:=0, nWhere, opcode, length, close_it:=.T., ver

if valtype(xFilename) == "C"

   nWhere    := at(".",xFilename)
   xFilename := if( nWhere=0,trim(xFilename)+"."+DEFAULT_EXTENSION,xFilename )
   if file( xFilename )
      wkshandle := Fopen( xFilename,FO_READ )  // Open file read-only
   endif

else

   wkshandle := xFilename                      // Numeric parameter is a file
   close_it  := .F.                            // handle which shouldn't be
endif                                          // closed when done.

if wkshandle >=0

   Fseek(wkshandle,0,FS_SET)                   // Go to top of file
   Bread(,wkshandle)                           // Initialize the buffer
   opcode:=bin2w(bread(2,wkshandle))           // Read operation code
   length:=bin2w(bread(2,wkshandle))           // and data length

   ver   :=bin2w(bread(2,wkshandle))
   do case
   case ver == 1028                             // Lotus 1A
      retval := "L1"
   case ver == 1029                             // Symphony 1.0
      retval := "SY"
   case ver == 1030                             // Lotus 2.0a-2.2
      retval := "L2"
   case ver == 4096                             // Lotus 3.0
      retval := "L3"
   case ver == 4098                             // Lotus 4.0
      retval := "L4"
   case ver == 1536                             // Lotus 123/J
      retval := "LJ"
   case ver == 2571                             // Quattro
      retval := "QU"
   case ver == 20768                            // Quattro Pro
      retval := "QP"
   case opcode = 2057 .and. ver = 1280          // Excel 5.x
      retval := "E5"
   case ver == 0 .and. opcode = 1033            // Excel 4.x
      retval := "E4"
   case ver == 0 .and. opcode =  521            // Excel 3.x
      retval := "E3"
   case ver == 2
      retval := "E2"
   endcase

   Bread(,-1)

   if close_it
      fclose(wkshandle)
   endif
endif
return retval
*****************************************************************************
*±±± End of Lversion ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Lmakerange()
* Purpose:    To convert a cell range to it internal format
* Syntax:     Lmakerange( cRange,cVersion )
* Arguments:  cRange      - range of cells ( address .. address )
*             cVersion    - type of spreadsheet
* Notes:      Lmakerange() is used internally to convert
*             spreadsheet range syntax into its internal format
*             for use by other programs.  It should never be
*             called directly by your application.  It is
*             included here in case you need to modify CLIPWKS.
* Returns:    cInternal   - Internal format of the range
* Class:      Internal
************************************
function lmakerange(cell_range,cVer)

LOCAL _starting:="", _ending:="", k, which:="S", temp, retval:=""
LOCAL cSheet1:="",cSheet2:="",cMiddle := if(substr(cVer,1,1)=="E",":","..")
cell_range := strtran(cell_range,"$")
if cVer == "L3" .or. cVer == "L4"
   k := at(":",cell_range)
   if k > 0
      cSheet1 := substr(cell_range,1,k)
      cell_range := substr(cell_range,k+1)
      k := at(":",cell_range)
      if k > 0
         cSheet2 := substr(cell_range,k-1,2)
         cell_range := substr(cell_range,1,k-2)+substr(cell_range,k+1)
      endif
   endif
endif

for k=1 to len(cell_range)
   temp := upper(substr(cell_range,k,1))

   if (temp>="A" .and. temp<="Z") .or. temp$"0123456789"

      if which == "S"
         _starting += temp
      else
         _ending   += temp
      endif

   else

      which := "E"                  // .. found, so we start the end part of
                                    // the range
   endif
next

if !empty(_starting) .and. !empty(_ending)
   retval := Lcell(cSheet1+_starting,cVer)+Lcell(cSheet2+_ending,cVer)
endif

return retval
*****************************************************************************
*±±± End of Lmakerange ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Lcell()
* Purpose:    To convert a cell address to it internal format
* Syntax:     Lcell( cAddress,cVersion )
* Arguments:  cAddress    - Address of cell
*             cVersion    - type of spreadsheet
* Notes:      Lcell() is used to convert a cell in spreadsheet
*             notation to its internal representation in the
*             file. For the most part, you should never need to
*             call Lcell() directly.
* Returns:    cInternal   - Internal format of the range
* Class:      Internal
**************************
function lcell(_cell,cVer)

LOCAL _cellstring:="", _col:="", _row:="", _bit, _coln, _rown
LOCAL nSheet :=0,x

_cell := upper(_cell)

if ( x:= at(":",_cell) ) > 0
    if x = 2
       nSheet := asc(substr(_cell,1)) -65
    else
       nSheet := (asc(substr(_cell,1))-64)*26+;
                 (asc(substr(_cell,2,1))-65)
    endif
    _cell  := substr(_cell,x+1)
endif

while !empty(_cell)

   _bit := substr(_cell,1,1)

   if _bit >="A" .and. _bit <="Z"
      _col += _bit
   elseif _bit >="0" .and. _bit <="9"
      _row += _bit
   endif

   _cell := substr(_cell,2)

enddo
_rown     := val(_row) -1
_coln     := if(Len(_col)=1,asc(_col)-65,(asc(_col)-64)*26+(asc(substr(_col,2,1))-65) )

if _rown <=MAX_ROWS -1 .and. _coln<=MAX_COLS -1
   if substr(cVer,1,1)=="E"
      _cellstring := i2bin(_rown)+i2bin(_coln)
   else
      _cellstring := if(cVer=="L3" .or. cVer=="L4",;
                        i2bin(_rown)+chr(nSheet)+chr(_coln),;
                        i2bin(_coln)+i2bin(_rown)  )
   endif

endif

return _cellstring
*****************************************************************************
*±±± End of Lcell ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lrelative()
* Purpose:    To convert cell coordinates to relative values
* Syntax:     Lrelative( nRefer,nCurrent )
* Arguments:  nRefer       - Row|column referred to
*             nCurrent     - Current row|column
* Notes:      Lrelative() is used to compute relative
*             coordinates between two cell address.
*             Spreadsheets store formula cell references are
*             relative positions from the current cell.  This
*             allows copying of formulas to automatically adjust
*             cell reference.
* Returns:    nRelative
* Class:      Internal
**********************************************
function lrelative(_p1,_p2)
LOCAL diff:=(_p1 - _p2)
return if(diff=0,32768,if(diff<0,49152+diff,32768+diff))
*****************************************************************************
*±±± End of Lrelative ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lstring()
* Purpose:    To convert a character string to spreadsheet format
* Syntax:     Lstring( cString, nSize )
* Arguments:  cString      - String to write to spreadsheet
*             nSize        - Size string should be
* Notes:      Spreadsheet character strings are stored with a
*             trailing NULL.  This function takes a string and
*             places a CHR(0) on the end so it can be properly
*             written to the spreadsheet.
* Returns:    cString
* Class:      Internal
*******************************
function lstring(cString,nSize)
return substr(cString+replicate(chr(0),nSize),1,nSize-1)+chr(0)
*****************************************************************************
*±±± End of Lstring ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Bread()
* Purpose:    To read the requested number of bytes from a data file buffer
* Syntax:     Bread( nBytes, nHandle )
* Arguments:  nBytes       - number of bytes to be read
*             nHandle      - Handle the file is opened on
* Notes:      Bread() is used to perform buffered reading of a
*             spreadsheet file. The 2048 bytes in the buffer are
*             used first to satisfy each read request, allowing
*             the functions to operate more quickly.  Bread()
*             should never be called directly by your
*             application.
* Returns:    cString      - data buffer requested
* Class:      Internal
******************************
function bread(nBytes,nHandle)               // Thanks - Ira Emus

STATIC buffer,pointer,offset
LOCAL temp, retval

if nBytes == NIL

   if nHandle = -1                           // Release buffer
      buffer := ""
      return offset+pointer                  // Where I am in the file
   else
      pointer := 1                           // Initialize buffer
      offset  := fseek(nHandle,0,1)          // Save the file offset
      buffer  := space(BUFFSIZE)
      fread(nHandle,@buffer,BUFFSIZE)
   endif
   return NIL

endif

if nBytes+pointer > BUFFSIZE+1
   temp    := substr(buffer,pointer)
   nBytes  := pointer + nBytes - (BUFFSIZE+1)
   pointer := 1
   offset  := fseek(nHandle,0,1)             // Save the file offset
   fread(nHandle,@buffer,BUFFSIZE)
   retval  := temp+substr(buffer,1,nBytes)
else
   retval  := substr(buffer,pointer,nBytes)
endif
pointer += nBytes
return retval
*****************************************************************************
*±±± End of Bread ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Bwrite()
* Purpose:    To write the requested string to a spreadsheet buffer
* Syntax:     Bwrite( cString,aSpreadsheet,lFlush )
* Arguments:  cString      - String to write to the buffer
*             aSpreadsheet - Array returned when spreadsheet was opened
*             lFlush       - Force buffer to be written to disk?
* Notes:      Bwrite() is used to write to the buffer rather
*             than a disk file.  It allows for better
*             performance when creating spreadsheets.  You
*             should never call Bwrite() directly in your
*             application code.
* Returns:    NIL
* Class:      Internal
**************************
function bwrite(cString,aWks,lFlush)

DEFAULT lFlush TO .F.
WKS_BUFFER += cString                      // Add string to write buffer

if len(WKS_BUFFER) > BUFFSIZE .or. lFlush  // If past buffer size, write out

   Ldirty(aWks)                            // Check for parameters to be written
   Fwrite(WKS_HANDLE,WKS_BUFFER)           // Write the actual buffer
   WKS_BUFFER := ""                        // and clear it out

endif
return NIL
*****************************************************************************
*±±± End of Bwrite ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Treal()
* Purpose:    To convert a real number to/from Lotus 3.x format
* Syntax:     treal( nValue | cValue )
* Arguments:  nValue  - real number to convert to IEEE real long 10 byte
*             cValue  - IEEE real long 10 byte string to convert to numeric
* Notes:      Treal() is used to read/write Lotus 3.x 10 byte IEEE real
*             numbers.
* Returns:    cResult | nResult
* Class:      Internal
************************
function treal( xValue,lSpecial)

LOCAL cTmp:="", nSign, nExp, nDec:=0, k, retval, nDiv, tmp

if valtype(xValue) == "N"
   return l_dtot( xValue )          // Thanks Rob Hannah
endif
if( substr(xValue,9,2)==chr(255)+chr(255) )
   lSpecial := asc(substr(xValue,8,1))==224
   return 0
endif
return l_ttod(xValue)               // Thanks Rob
*****************************************************************************
*±±± End of Treal ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Shortnum()
* Purpose:    To convert a Lotus 3.x integer to/from a Clipper numeric
* Syntax:     Shortnum( nValue|cValue )
* Arguments:  nValue   - Integer to convert to short number
*             cValue   - Character string to convert to integer
* Notes:      Shortnum() is used to read/write Lotus 3.x short number
*             cells.
* Returns:    cResult | nResult
* Class:      Internal
****************************
function shortnum( xValue )

LOCAL retval:=""

if valtype(xValue) == "N"
   retval := if( xValue>=0, i2bin(xValue*2), i2bin(65536-(abs(xValue)*2)) )
elseif valtype(xValue) == "C"
   retval := l_snum( xValue )         // Thanks Rob Hannah
endif

return retval
*****************************************************************************
*±±± End of Shortnum() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Real2iee()
* Purpose:    To convert a real number from Lotus/Quattro format
* Syntax:     Real2iee( cValue )
* Arguments:  cValue  - IEEE real long 8 byte string to convert to numeric
* Returns:    nResult
* Class:      Internal
***************************
function real2iee( cValue,lSpecial )

LOCAL x:=asc(substr(cValue,8,1)),y:=asc(substr(cValue,7,1))
if y == 240 .and. (x == 127 .or. x == 255)     // Check for ERR/NA cell
   lSpecial := substr(cValue,1,6) <> replicate(chr(0),6)
   return 0
endif
return realiee( cValue )               // Thanks, Mike Taylor
*****************************************************************************
*±±± End of Real2iee() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
