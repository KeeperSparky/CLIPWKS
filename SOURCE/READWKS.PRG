***************************************************************
* Program.......: READWKS.PRG
* Authors.......: Joseph D. Booth
* Copyright.....: 1998 Joseph D. Booth, All rights reserved.
* Application...: CLIPWKS++
* Version.......: 1.1
* Compile.......: Xpp readwks -l -m
*                 Lfindfirst( aWks [,cColumn] )  - Get first cell
*                 Lfindnext( aWks )              - Get next cell
*                 Lread(cFilename,lUpdate)       - Read a spreadsheet
*
* Special Thanks: Ira Emus
*                 Rob Hannah
*                 John Kaster
*
****************************************************************************
#include "CLIPWKS.CH"
#include "FILEIO.CH"

* Function:   Lfindfirst()
* Purpose:    To return the first cell in the spreadsheet
* Syntax:     Lfindfirst( aWks [,xColumn] )
*
* Arguments:  aWks      - Spreadsheet handle array
*             xColumn   - Column letter to find first cell of
*             nColumn   - Column number to find first cell of
* Returns:    aCell     - Array of cell information
*
* Notes:      Lfindfirst() positions the spreadsheet to the first cell
*             in the spreadsheet or in the specified column.  It also
*             returns an array of cell information.  Subsequent calls
*             to Lfindnext() will obtain the next cell.
* See Also:   Lfindnext()  Lgetcell()
**********************************************

function lfindfirst( aWks,xColumn )
LOCAL retval , ln

if valtype(aWks) == "A" .and. ! empty(aWks)
   //
   // No column specified, go to first cell in spreadsheet
   //
   if xColumn == NIL

      // Fill buffer,pointer,offset

      aWks[10]   := { Lfileread(WKS_HANDLE,WKS_DATA_OFFSET,BUFFSIZE), ;
                      1,WKS_DATA_OFFSET }
      ln         := bin2w(substr(WKS_READ_BUFFER,3,2))
      retval     := limget(aWks,substr(WKS_READ_BUFFER,1,ln+4))

      WKS_READ_POINTER := ln+5

   elseif substr(WKS_VERSION,1,1) =="E" .and. valtype(xColumn)=="N"

   else

      if valtype(xColumn) == "C"
         xColumn := if(Len(xColumn)=1,asc(xColumn)-65,;
                    (asc(xColumn)-64)*26+(asc(substr(xColumn,2,1))-65) )
      endif
      ln     := bin2w(substr(WKS_CPI,xColumn*4+1,2))
      retval := Lgetcell(aWks,lc(ln+1,xColumn+1))
   endif
endif
return retval
*****************************************************************************



* Function:   Lfindnext()
* Purpose:    To return subsequent cells from either Lfindfirst() or Lget()
* Syntax:     Lfindnext( aWks )
*
* Arguments:  aWks      - Spreadsheet handle array
* Returns:    aCell     - Array of cell information
*
* Notes:      Lfindnext() reads the next cell in the spreadsheet. It
*             may start as a result of either a call to Lfindfirst()
*             or to Lgetcell().  If a cell is found, an array of cell
*             information is returned.  If no cell is found, an empty
*             array is returned.
* See Also:   Lfindfirst()  Lgetcell()
**********************************************
function lfindnext( aWks )
LOCAL retval:={} , buf , ln, op, cTmp, x
if valtype(aWks) == "A" .and. ! empty(aWks)

   while .T.

      if WKS_READ_POINTER > ( BUFFSIZE-5 )           // Not enough room in buffer\

         WKS_READ_OFFSET += WKS_READ_POINTER        // to determine length
         buf := space( BUFFSIZE )                   // memory
         Fread( WKS_HANDLE,@buf,BUFFSIZE )
         WKS_READ_BUFFER  := substr(WKS_READ_BUFFER,WKS_READ_POINTER)+buf
         WKS_READ_POINTER := 1
      endif

      op       := bin2w(substr(WKS_READ_BUFFER,WKS_READ_POINTER,2))
      ln       := bin2w(substr(WKS_READ_BUFFER,WKS_READ_POINTER+2,2))

      if WKS_READ_POINTER+4+ln > len(WKS_READ_BUFFER)

         WKS_READ_OFFSET += WKS_READ_POINTER
         buf := space( BUFFSIZE )
         Fread( WKS_HANDLE,@buf,BUFFSIZE )
         WKS_READ_BUFFER  := substr(WKS_READ_BUFFER,WKS_READ_POINTER)+buf
         WKS_READ_POINTER := 1

      endif
      retval     := limget(aWks,substr(WKS_READ_BUFFER,WKS_READ_POINTER,ln+4))
      WKS_READ_POINTER += ln+4
      if substr(WKS_VERSION,1,1)=="E"
         if op == 10 .and. ln == 0
            retval := {}
            exit
         endif
         if !empty(retval) .and. ;
            retval[1] = NIL .and. !empty(WKS_READ_BUFFER)
            loop
         endif
      endif
      exit
   enddo
endif
return retval

*****************************************************************************


* Function:   Lgetrange()
* Purpose:    To return an array of spreadsheet cells
* Syntax:     Lgetrange( aWks,cRange )
*
* Arguments:  aWks      - Spreadsheet handle array
*             cRange    - Range specifier or range name
* Returns:    aContents - Contents of all cells within range
*
**********************************************


function lgetrange( aWks,cRange )
LOCAL arr_:={}, aCell:={}, nTrow, nTcol, nBrow, nBcol ,cTmp ,jj ,x
LOCAL xx,yy,nSheet
if valtype(aWks) == "A" .and. ! empty(aWks) .and. ! empty(cRange)
   if at("..",cRange)==0      // Looking for a named range
      //
      for jj=1 to len(WKS_RANGES)
         if trim(cRange)== trim(WKS_RANGES[jj,1])
            cRange := WKS_RANGES[jj,2]
            jj     := 99999
         endif
      next
   endif
   if (x:=at("..",cRange))<>0                      // Found a range to extract
      // Determine number of cells with range
      cTmp      := Lmakerange(cRange,WKS_VERSION)
      if !empty(cTmp)
         if WKS_VERSION == "L3" .or. WKS_VERSION == "L4"
            nTrow := bin2w(substr(cTmp,1,2))
            nTcol := asc(substr(cTmp,4,1))
            nSheet:= asc(substr(cTmp,3,1))
            nBrow := bin2w(substr(cTmp,5,2))
            nBcol := asc(substr(cTmp,8,1))
         elseif substr(WKS_VERSION,1,1)=="E"
            nSheet:= -1
            nTrow := bin2w(substr(cTmp,1,2))
            nTcol := bin2w(substr(cTmp,3,2))
            nBrow := bin2w(substr(cTmp,5,2))
            nBcol := bin2w(substr(cTmp,7,2))
         else
            nSheet:= -1
            nTrow := bin2w(substr(cTmp,3,2))
            nTcol := bin2w(substr(cTmp,1,2))
            nBrow := bin2w(substr(cTmp,7,2))
            nBcol := bin2w(substr(cTmp,5,2))
         endif
         arr_      := array(nBrow-nTrow+1,nBcol-nTcol+1)
         if WKS_VERSION <> "QP"
            for xx = nTrow+1 to nBrow+1
               for yy = nTcol+1 to nBcol+1
                  aCell     := Lgetcell(aWks,lc(xx,yy,nSheet+1))
                  if !empty(aCell)
                     xx = nBrow+3
                     yy = nBcol+3
                  endif
               next
            next
         else
            for xx = nTcol+1 to nBcol+1
               for yy = nTrow+1 to nBrow+1
                  aCell     := Lgetcell(aWks,lc(yy,xx,nSheet))
                  if !empty(aCell)
                     xx = nBcol+3
                     yy = nBrow+3
                  endif
               next
            next
         endif
         while !empty(aCell)
            if aCell[5] > (nBrow+1) .and. aCell[6] > (nBcol+1)
               exit
            endif
            aCell[5] -= nTrow
            aCell[6] -= nTcol
            if aCell[5] <= len(arr_) .and. aCell[6] <= len(arr_[1]) ;
               .and. aCell[5] >0 .and. aCell[6] >0  .and. arr_[ aCell[5],aCell[6] ] == NIL
                arr_[ aCell[5],aCell[6] ] := aCell[1]
            endif
            aCell := Lfindnext(aWks)
         enddo
      endif
   endif
endif
return arr_
*****************************************************************************




* Function:   Lreplace()
* Purpose:    To replace an existing cell's contents
* Syntax:     Lreplace( aWks,cCell,xData )
* Arguments:  aWks      - Spreadsheet handle array
*             cCell     - Cell address to change
* Returns:    nStatus   - Status of operation
*
**********************************************
function lreplace( aWks,xData,cCell )
LOCAL retval  := -6,arr_:={},tmp,x,cell_spot
LOCAL rewrite := .F.
LOCAL cHold
if valtype(aWks)=="A" .and. !empty(aWks)
   if aWks[4]
      arr_ := if(valtype(cCell)=="A" , arr_, Lgetcell(aWks,cCell) )
      if !empty(arr_)
         if arr_[2] == valtype(xData)
            tmp := lfileread(WKS_HANDLE,arr_[7],512)
            do case
************** Character Data ************************************************
            case (arr_[2] == "C" .and. WKS_VERSION == "QP") .and. len(xData) <= arr_[3]
               tmp := stuff(tmp,12,arr_[3],Padr(xData,arr_[3]))
            case (arr_[2] == "C" .and. WKS_VERSION $"L3|L4") .and. len(xData) <= arr_[3]
               tmp := stuff(tmp,10,arr_[3],Lstring(xData,arr_[3]))
            case arr_[2] == "C" .and. WKS_VERSION $ "E2|E3|E4" .and. len(xData) <= arr_[3]
               tmp := stuff(tmp,13,arr_[3],padr(xData,arr_[3]))
            case arr_[2] == "C" .and. len(xData) <= arr_[3]
               tmp := stuff(tmp,11,arr_[3],Lstring(xData,arr_[3]))
            case arr_[2] == "C"
               rewrite := .T.
************** Numeric / Date ************************************************
            case arr_[2] $ "DN" .and. WKS_VERSION $"L3|L4"
               x := if(arr_[2]=="D",abs(xData-ctod("12/30/1899")),xData)
               if arr_[1] <> x      // Don't replace same value
                  if arr_[3] == 2
                     if x >= -16384 .and. x <= 16383 .and. x == int(x)
                        tmp := stuff(tmp,9,2,shortnum(x))
                     else
                        rewrite := .T.
                     endif
                  elseif arr_[3] == 10
                     tmp := stuff(tmp,9,10,treal(x))
                  endif
               endif
            case arr_[2] $ "DN"
               if arr_[2] == "D" .and. arr_[3] == 2
                  if xData - ctod("12/30/1899") > 32767
                     rewrite := .T.
                  endif
               endif
               x := if(arr_[2]=="D",abs(xData-ctod("12/30/1899")),xData)
               do case
               case arr_[3] == 2
                  if x >= -32767 .and. x <= 32768 .and. x == int(x)
                     tmp := stuff(tmp,10,2,i2bin(x))
                  else
                     rewrite := .T.
                  endif
               case arr_[3] == 8
                  tmp := stuff(tmp,10,8,ieereal(x))
               case arr_[3] == 4 .and. left(WKS_VERSION,1)=="E"
                  cHold := ieereal(xData)
                  cHold := substr(cHold,5,4)
                  bit(cHold,8,.F.)
                  bit(cHold,7,.F.)
                  tmp   := stuff(tmp,11,4,cHold)
               endcase
************** Logical *******************************************************
            case arr_[2] == "L" .and. WKS_VERSION == "QU"
               if arr_[1] <> xData     // Don't replace if same value
                  if xData             // .T.
                     tmp := stuff(tmp,10,8,ieereal(1))
                     tmp := stuff(tmp,20,1,chr(114))
                  else
                     tmp := stuff(tmp,10,8,ieereal(0))
                     tmp := stuff(tmp,20,1,chr(113))
                  endif
               endif
            case arr_[2] == "L" .and. WKS_VERSION $ "L3|L4"
               if arr_[1] <> xData     // Don't replace if same value
                  if xData             // .T.
                     tmp := stuff(tmp,9,10,treal(1))
                     tmp := stuff(tmp,19,1,chr(52))
                  else
                     tmp := stuff(tmp,9,10,treal(0))
                     tmp := stuff(tmp,19,1,chr(51))
                  endif
               endif
            case arr_[2] == "L"
               x := if(WKS_VERSION=="QP",24,20)
               if arr_[1] <> xData     // Don't replace if same value
                  if xData             // .T.
                     tmp := stuff(tmp,10,8,ieereal(1))
                     tmp := stuff(tmp, x,1,chr(52))
                  else
                     tmp := stuff(tmp,10,8,ieereal(0))
                     tmp := stuff(tmp, x,1,chr(51))
                  endif
               endif
            endcase
            if !rewrite
               fseek(WKS_HANDLE,arr_[7],FS_SET)
               retval := if(fwrite(WKS_HANDLE,tmp,len(tmp))==len(tmp),0,-2)
            endif
         else
            rewrite := .T.
         endif
      else
         rewrite := .T.
      endif
      if rewrite
         fseek(WKS_HANDLE,-4,FS_END)
         tmp := Lput(aWks,xData,cCell,,.F.)
         if left(WKS_VERSION,1) <> "E"
            fwrite(WKS_HANDLE,tmp+EOF_OPCODE)
         else
            fwrite(WKS_HANDLE,tmp+E2_EOF_OPCODE)
         endif
         aWks[5,2] := aWks[5,2] +len(tmp)
         retval := 0
      endif
   else
      retval := -12
   endif
endif
return retval
*****************************************************************************




* Function:   Lrows()
* Purpose:    To determine number of rows in spreadsheet
* Syntax:     Lrows( aWks )
* Arguments:  aWks      - Spreadsheet handle array
* Returns:    Number of rows
*             -6       - Invalid spreadsheet array
* See Also:   Lcols()
**********************************************


function lrows( aWks )
LOCAL retval :=-6
if valtype(aWks) == "A" .and. ! empty(aWks)
   retval := WKS_BOTTOM_ROW +1      // Rows in spreadsheet
endif
return retval
*****************************************************************************


* Function:   Lcols()
* Purpose:    To determine number of columns in spreadsheet
* Syntax:     Lcols( aWks )
* Arguments:  aWks      - Spreadsheet handle array
* Returns:    Number of columns
*             -6       - Invalid spreadsheet array
* See Also:   LRows()
**********************************************
function lcols(aWks)
LOCAL retval :=-6
if valtype(aWks) == "A" .and. ! empty(aWks)
   retval := WKS_BOTTOM_COL +1      // Columns in spreadsheet
endif
return retval
*****************************************************************************



* Function:   Lread()
* Purpose:    To open a spreadsheet for reading
* Syntax:     Lread( cFilename,lUpdate,lIndex,lDates )
* Arguments:  cFilename - Name of spreadsheet to read
*             lUpdate   - Whether CLIPWKS can update the spreadsheet
* Returns:    NIL
**********************************************
function lread(cFilename,lUpdate,lDates)

LOCAL aWks:={},nHandle,cVersion,opcode:=0,nLength:=0,cTmp:="",k,nRow
LOCAL tmp:="",y,subtype,cSubform,cPattern,cFormat,colptr,do_it:=.F.,jj,xTmp
LOCAL nWhere := 0,ok:=.T.,x,z
DEFAULT lUpdate  TO  .F.
DEFAULT lDates   TO  .T.
nHandle  := Fopen(cFilename,if(lUpdate,FO_READWRITE,FO_READ))
cVersion := Lversion(nHandle)               // Make sure it is a Lotus/Quattro file
if !empty(cVersion)
   aWks := { cFilename, ;                   // Spreadsheet file name
             cVersion, ;                    // Version
             nHandle, ;                     // Handle file opened on
             lUpdate, ;                     // Allowed to update?
             {0,0}, ;                       // Data offset/end
             {0,0,0,0,9},;                  // Coordinates, global width
             "", ;                          // CPI information
             {}, ;                          // Date bitmap for Lotus 3.x
             {}, ;                          // Ranges
             {"",0,0}, ;                    // Buffer, pointer, offset
             "", ;                          // Global format for Lotus 3.x
             replicate(chr(9),256),  ;      // Cell width's
             {},{},0 }                      // Excel extra elements

   if substr(cVersion,1,1)=="E"
      EXCEL_DATE_FORMAT := ctod("12/30/1899")     // Date offset
    endif

   WKS_FILE_SIZE := fseek(nHandle,0,FS_END) // Get file size
   fseek(nHandle,0,FS_SET)                  // Top of file
   bread(,nHandle)                          // Initialize the buffer
   while !lWksEof(opcode,cVersion)
      opcode :=bin2w(bread(2,nHandle))      // Read operation code
      nLength:=bin2w(bread(2,nHandle))      // and data length
      if nLength > 0                        // If data is present, then
         cTmp := bread(nLength,nHandle)     // extract the data
      endif
      if cVersion == "L3" .or. cVersion == "L4"
         do case
         // Check for start of data section
         case opcode = 0
            WKS_TOP_ROW      := bin2w(substr(cTmp,5,2))
            WKS_TOP_COL      := asc(substr(cTmp,8,1))
            WKS_BOTTOM_ROW   := bin2w(substr(cTmp,9,2))
            WKS_BOTTOM_COL   := asc(substr(cTmp,12,1))
         case opcode = 6
            WKS_GLOBAL_WIDTH := asc(substr(cTmp,5,1))
            aWks[12]         := strtran(aWks[12],chr(9),chr(WKS_GLOBAL_WIDTH))
         case opcode >= 20 .and. opcode <=26
            WKS_DATA_OFFSET  := bread(,-1) - nLength-5       // Compute offset in file
            fseek(nHandle,WKS_DATA_OFFSET,FS_SET)            // Postion the file and
            exit
         case opcode == 31
            if empty( WKS_CPI )
               tmp     := i2bin(0)+i2bin(0)
               WKS_CPI := replicate(tmp,256)
            endif
            if asc(cTmp)==0
               y:= asc(substr(cTmp,2,1))
               WKS_CPI := stuff(WKS_CPI,y*4+1,4,;
                                        substr(cTmp,5,2)+substr(cTmp,-2,2) )
            endif
         case opcode = 19 .and. lDates
            subtype := asc(substr(cTmp,2,1))
            do case
            case subtype == 0
               colptr  := 0
               k       := bin2w( substr(cTmp,3,2) )
               cFormat := substr(cTmp,5,nLength-4)
               cPattern:= str(k+1,6)+replicate(chr(0),32)
               while !empty(cFormat)
                  k :=1
                  if bit(cFormat,25)
                     cTmp    :=substr(cFormat,1,5)
                     cFormat :=substr(cFormat,6)
                     k       :=asc(substr(cTmp,5,1))+1
                  else
                     cTmp    :=substr(cFormat,1,4)
                     cFormat :=substr(cFormat,5)
                  endif
                  // See if cell is a date
                  if bit(cTmp,2) .and. bit(cTmp,3) .and. bit(cTmp,4)
                     csubform:=if(bit(cTmp,5),"1","0")+;
                               if(bit(cTmp,6),"1","0")+;
                               if(bit(cTmp,7),"1","0")+;
                               if(bit(cTmp,8),"1","0")
                     if cSubform == "0010" .or. cSubform == "0011" .or. ;
                        cSubform == "0100" .or. cSubform == "1001" .or. ;
                        cSubform == "1010"
                        for jj=1 to k
                           colptr++
                           bit(cPattern,colptr+48,.T.)
                        next
                        do_it :=.T.
                     else
                        colptr+=k
                     endif
                  else
                     colptr+=k
                  endif
               enddo
               if do_it
                  Aadd( WKS_DATE_BITMAP,cPattern)
               endif

            case subtype == 1
               WK3_GLOBAL_FORMAT := asc(substr(cTmp,5,1))

            case subtype == 2
               nRow := bin2w( substr(cTmp,3,2) )+1
               k := bin2w(substr(cTmp,7,2)) +1
               jj:= ascan(WKS_DATE_BITMAP,{|y|val(substr(y,1,6))==k})
               if jj >0
                  cPattern := str(nRow,6)+substr(aWks[8,jj],7,32)
                  Aadd(WKS_DATE_BITMAP,cPattern)
               endif

            endcase
         case opcode == 9                         // Named ranges
            if bin2w(substr(cTmp,1,2)) == 0
               y := substr(cTmp,3,16)
               k := at(chr(0),y)
               if k > 0
                 y := padr(substr(y,1,k-1),16)
               endif
               xtmp := lc(bin2w(substr(cTmp,19,2))+1,asc(substr(cTmp,22,1))+1,;
                          asc(substr(cTmp,21,1))+1)+".."
               xTmp += lc(bin2w(substr(cTmp,23,2))+1,;
                        asc(substr(cTmp,26,1))+1,asc(substr(cTmp,25,1))+1)
               aadd( aWks[9],{y,xTmp} )
            endif
         case opcode == 7       // Cell width
            if asc(substr(cTmp,1,1))==0
               for k= 5 to len(cTmp) step 2
                  y :=asc(substr(cTmp,k,1))
                  aWks[12]:= stuff(aWks[12],y+1,1,substr(cTmp,k+1,1))
               next
            endif
         endcase

      elseif substr(cVersion,1,1)=="E"

         do case
         case opcode == 30 .or. opcode == 1055
            Aadd( EXCEL_FORMATS,if(opcode==30,substr(ctmp,2),substr(ctmp,4)) )
         case opcode = 0 .or. opcode = 512
            WKS_TOP_ROW       := bin2w(substr(cTmp,1,2))
            WKS_TOP_COL       := bin2w(substr(cTmp,5,2))
            WKS_BOTTOM_ROW    := bin2w(substr(cTmp,3,2))
            WKS_BOTTOM_COL    := bin2w(substr(cTmp,7,2))
       case opcode == 11 .or. opcode == 523
            x := if(opcode==11,9,13)
            EXCEL_FIRST_ROW := bin2w(substr(cTmp,5,2))
            for y := x to len(cTmp) step 4
               Aadd(EXCEL_INDEX_RECORD,bin2l(substr(cTmp,y,4)) )
            next

        case (opcode == 8 .or. opcode == 520) .and. WKS_DATA_OFFSET < 1
            WKS_DATA_OFFSET := nWhere +20 + if(opcode==8,bin2w(substr(cTmp,12,2)),bin2w(substr(cTmp,11,2)))
            x  := len(EXCEL_INDEX_RECORD)
            y  := EXCEL_INDEX_RECORD[x]
            fseek(nHandle,y,FS_SET)                  // Next data cell
            bread(,nHandle)                          // Initialize the buffer
            loop

         case opcode == 34 .and. bin2w(cTmp)== 1
            EXCEL_DATE_FORMAT := ctod("01/01/1904")     // Date offset

         case opcode = 67
            Aadd(EXCEL_XF_RECORDS,cTmp)
         case opcode = 579
            Aadd(EXCEL_XF_RECORDS,cTmp)
         case opcode = 1091
            Aadd(EXCEL_XF_RECORDS,cTmp)
         case opcode == 85
            WKS_GLOBAL_WIDTH := bin2w(substr(cTmp,1,2))
            aWks[12]         := strtran(aWks[12],chr(9),chr(WKS_GLOBAL_WIDTH))
         case opcode == 36
            y                := asc(substr(cTmp,1,1))
            z                := asc(substr(cTmp,2,1))
            k                := int(bin2w(substr(cTmp,3,2))/256)
            for x := y to z
               aWks[12]:= stuff(aWks[12],y+1,1,chr(k))
            next
         case opcode == 125
            y                := bin2w(substr(cTmp,1,2))
            z                := bin2w(substr(cTmp,3,2))
            k                := int(bin2w(substr(cTmp,5,2))/256)
            for x := y to z
               aWks[12]:= stuff(aWks[12],y+1,1,chr(k))
            next
         case opcode == 24
         case opcode == 536       // Range name
            x := asc(substr(cTmp,4,1))
            y := bin2w(substr(cTmp,5,2))
            if asc(substr(cTmp,7+x,1))==45 .and. x > 1       // Found a range
               xTmp := lr( bin2w(substr(cTmp,8+x,2))+1, ;
                           asc(substr(cTmp,12+x,1))+1,  ;
                           bin2w(substr(cTmp,10+x,2))+1,;
                           asc(substr(cTmp,13+x,1))+1 )
               aadd( aWks[9],{substr(cTmp,7,x),xTmp} )
            endif
         endcase
      else
         do case
         // Check for start of data section
         case opcode >= 12 .and. opcode <=16
            WKS_DATA_OFFSET := bread(,-1) - nLength-5    // Compute offset in file
            fseek(nHandle,WKS_DATA_OFFSET,FS_SET)        // Postion the file and
            exit
         case opcode == 6                        // Range record
            WKS_TOP_ROW      := bin2w(substr(cTmp,3,2))
            WKS_TOP_COL      := bin2w(substr(cTmp,1,2))
            WKS_BOTTOM_ROW   := bin2w(substr(cTmp,7,2))
            WKS_BOTTOM_COL   := bin2w(substr(cTmp,5,2))
         case opcode == 11 .or. opcode == 71      // Named ranges
            if WKS_VERSION == "QP"
               y := substr(cTmp,2,asc(cTmp))
            else
               y := substr(cTmp,1,16)
            endif
            y := padr( strtran(y,chr(0)," "),16)
            aadd(aWks[9],{y,lr( bin2w(substr(cTmp,19,2))+1,;
                                bin2w(substr(cTmp,17,2))+1,;
                                bin2w(substr(cTmp,23,2))+1,;
                                bin2w(substr(cTmp,21,2))+1 ) } )
         case opcode == 150
            tmp := i2bin(0)+i2bin(0)
            WKS_CPI := replicate(tmp,256)
            for k=1 to len(cTmp) step 6
               y :=bin2w(substr(cTmp,k,2))
               WKS_CPI :=stuff(WKS_CPI,y*4+1,4,substr(cTmp,k+2,4))
            next
         case opcode == 8       // Cell width
            y := bin2w(substr(cTmp,1,2))
            aWks[12]:= stuff(aWks[12],y+1,1,substr(cTmp,3,1))
         case opcode == 7       // Global width
            WKS_GLOBAL_WIDTH := bin2w(substr(cTmp,7,2))
            aWks[12]         := strtran(aWks[12],chr(9),chr(WKS_GLOBAL_WIDTH))
         endcase
      endif
      nWhere += (4+nLength)
   enddo
endif
return aWks
*****************************************************************************
*±±± End of Lread ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



STATIC function lWksEof(nOpcode,cVer)
if substr(cVer,1,1)=="E"
   return nOpCode == 10
endif
return nOpCode == 1
*****************************************************************************
*±±± End of Lwkseof() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Purpose:    To return the contents of a cell
* Syntax:     Lget( aSpreadsheet,cCell )
* Arguments:  aSpreadsheet    - Spreadsheet handle array
*             cCell           - Cell address to look for
* Returns:    xCell_contents  - "" if cell not found
* See Also:   Ltype()
**********************************************
function lget(aWks,cCell)
LOCAL arr_:=Lgetcell(aWks,cCell)
return if(empty(arr_),"",arr_[1])
*****************************************************************************
*±±± End of Lgetcell() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
* Purpose:    To return the type of a cell
* Syntax:     Ltype( aSpreadsheet,cCell )
* Arguments:  aSpreadsheet    - Spreadsheet handle array
*             cCell           - Cell address to look for
* Returns:    xCell_type      - "" if cell not found
*                                C - Character
*                                D - Date
*                                L - Logical
*                                N - Numeric
*                                F - Formula
* See Also:   Lget()
**********************************************
function ltype(aWks,cCell)
LOCAL arr_:=Lgetcell(aWks,cCell)
return if(empty(arr_),"",arr_[2])
*****************************************************************************
*±±± End of Lgetcell() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
* Purpose:    To return an array of cell attributes
* Syntax:     Lgetcell( aSpreadsheet,cCell )
* Arguments:  aSpreadsheet    - Spreadsheet handle array
*             cCell           - Cell address to look for
* Returns:    aCell_info      - 1 = Contents
*                               2 = Cell type
*                               3 = Cell length
*                               4 = Cell address
*                               5 = Row number
*                               6 = Column number
*                               7 = File offset
* See Also:   Lget() Ltype()
**********************************************
function lgetcell(aWks,cCell)
LOCAL cCoords := Lcell(cCell,WKS_VERSION),nRow,nCol,nSheet,cRetval:=NIL
LOCAL nStart,nAvg,ok:=.T.,arr_:={},nEnd:=aWks[5,2],tmp,k,x,op,ln,nEarly,cAddr
LOCAL nFirstCol:=0,nLastCol:=-1,nWhere,cTmp
LOCAL nSize

if substr(WKS_VERSION,1,1)=="E"
   nRow   := bin2w(substr(cCoords,1,2))             // Row
   nCol   := bin2w(substr(cCoords,3,2))             // Column
elseif WKS_VERSION == "L3" .or. WKS_VERSION == "L4"
   nRow   := bin2w(substr(cCoords,1,2))             // Row
   nSheet := asc(substr(cCoords,3,1))               // Spreadsheet
   nCol   := asc(substr(cCoords,4,1))               // Column
else
   nRow   := bin2w(substr(cCoords,3,2))             // Row
   nCol   := bin2w(substr(cCoords,1,2))             // Column
endif

if substr(WKS_VERSION,1,1) == "E"                   // Special treatment for Excel
   //
   // Determine where the row records are stored, by taking the row number
   // and looking it up in the EXCEL_INDEX_RECORD.  Go to this offset and
   // start reading ROW records until you find one where
   //
   if !empty(EXCEL_INDEX_RECORD)
      x      := int((nRow-EXCEL_FIRST_ROW)/32)+1
      nWhere := EXCEL_INDEX_RECORD[x]
      fseek(WKS_HANDLE,nWhere,FS_SET)
      bread(,WKS_HANDLE)
      op := bin2w(bread(2,WKS_HANDLE))
      ln := bin2w(bread(2,WKS_HANDLE))
      x := 0
      nStart := nWhere +20
      while (op == 8 .and. ln == 18) .or. (op==520 .and. ln=16) .and. nLastcol < 0
         cTmp := bread(ln,WKS_HANDLE)
         x         := if(op==8,bin2w(substr(cTmp,12,2)),bin2w(substr(cTmp,11,2)))
         nStart    += x
         if bin2w(substr(cTmp,1,2)) == nRow
            nFirstCol := bin2w(substr(cTmp,3,2))
            nLastCol  := bin2w(substr(cTmp,5,2))
         endif
         nWhere += (4+ln)
         op := bin2w(bread(2,WKS_HANDLE))
         ln := bin2w(bread(2,WKS_HANDLE))
         if nLastcol > 0 .and. (op==8 .or. op==520)
            cTmp := bread(ln,WKS_HANDLE)
            x         := if(op==8,bin2w(substr(cTmp,12,2)),bin2w(substr(cTmp,11,2)))
            nEnd := nStart +x -1
         endif
      enddo
   else
      nStart    := WKS_DATA_OFFSET
      nEnd      := aWks[5,2]
      nFirstCol := WKS_TOP_COL
      nLastCol  := WKS_BOTTOM_COL
   endif

   if nCol >= nFirstCol .and. nCol <= nLastCol
      x :=0
      while empty(x)
         x := llocate(aWks,cCoords,nStart,nEnd)
         if x > 0
            // Read previous few bytes to see if legitimate cell
            // If ok, exit the loop
            x -= 4
            x   := max(x,WKS_DATA_OFFSET)
            tmp := lfileread( WKS_HANDLE,x,BUFFSIZE )
            op  := bin2w(substr(tmp,1,2))
            ln  := bin2w(substr(tmp,3,2))
            if op > 4096
               tmp := substr(tmp,3)
               op  := bin2w(substr(tmp,1,2))
               ln  := bin2w(substr(tmp,3,2))
            endif
            if bin2w(substr(tmp,5,2)) <> nRow .or. ;
               bin2w(substr(tmp,7,2)) <> nCol .or. ;
               ! is_cell(WKS_VERSION,op,ln)
               nStart := x+9
               x      :=0
               if nStart >= nEnd        // Cell was not found
                  return arr_
               endif
               loop
            else
               exit
            endif
         endif
      enddo
      //
      if x > 0
         WKS_READ_BUFFER   := tmp
         WKS_READ_POINTER  := 1
         WKS_READ_OFFSET   := x
         arr_              := Limget(aWks,substr(WKS_READ_BUFFER,1,ln+4))
         WKS_READ_POINTER  := ln+5
      endif
      //
   endif
   return arr_
endif


if nRow >= WKS_TOP_ROW .and. nRow <= WKS_BOTTOM_ROW     // Is requested cell
   if nCol >= WKS_TOP_COL .and. nCol <= WKS_BOTTOM_COL  // within spreadsheet range?

      if len(WKS_CPI)==1024 .and. nSheet == 0
         ok := nRow>=bin2w(substr(WKS_CPI,nCol*4+1,2))  .and. ;
               nRow<=bin2w(substr(WKS_CPI,nCol*4+3,2))

      endif

      if ok           // Cell should be in the spreadsheet
        // See if the cell is in the buffer
        if !empty(WKS_READ_BUFFER)
           k     :=1
           nSize := len(WKS_READ_BUFFER)
           while k < nSize
              op    := bin2w(substr(WKS_READ_BUFFER,k,2))
              ln    := bin2w(substr(WKS_READ_BUFFER,k+2,2))
              if k+2+ln > nSize
                 exit
              endif
              cAddr := substr(WKS_READ_BUFFER,k+4,4)
              if cAddr == cCoords .and. is_cell(WKS_VERSION,op,ln)
                 WKS_READ_POINTER := k
                 return Lfindnext(aWks)
              endif
              k += (ln+4)
              if k+4 > nSize    // Not enough room in buffer to read op,ln
                 exit
              endif
           enddo
        endif

        // Attempt to make a reasonable first guess as to where
        nAvg  := (aWks[5,2]-WKS_DATA_OFFSET)/((WKS_BOTTOM_COL-WKS_TOP_COL)*(WKS_BOTTOM_ROW-WKS_TOP_ROW))
        nStart:= max(nRow*nCol*nAvg+WKS_DATA_OFFSET-512,WKS_DATA_OFFSET)
        nEarly:= nStart
        k     := 0

        while k == 0
            k := llocate(aWks,cCoords,nStart,nEnd) // From current spot to END
            if k > 0
               // Read previous few bytes to see if legitimate cell
               // If ok, exit the loop
               if ! (WKS_VERSION $"L3|L4")
                  k -= 5
                  k   := max(k,WKS_DATA_OFFSET)
                  tmp := lfileread( WKS_HANDLE,k,BUFFSIZE )
                  op  := bin2w(substr(tmp,1,2))
                  ln  := bin2w(substr(tmp,3,2))
                  if .not. is_cell(WKS_VERSION,op,ln)
                     nStart := k+9
                     k      :=0
                  endif
                else
                  k -=4
                  k   := max(k,WKS_DATA_OFFSET)
                  tmp := lfileread( WKS_HANDLE,k,BUFFSIZE)
                  op  := bin2w(substr(tmp,1,2))
                  ln  := bin2w(substr(tmp,3,2))
                  if .not. is_cell(WKS_VERSION,op,ln)
                     nStart := k+9
                     k      :=0
                  endif
                endif
            endif
            if k < 0
               if nStart == WKS_DATA_OFFSET .and. nEnd < WKS_FILE_SIZE
                  exit
               endif
               if WKS_DATA_OFFSET < nStart
                  nEnd   := nEarly-1
                  nStart := WKS_DATA_OFFSET
                  k      := 0
               endif
            endif
         enddo
         if k > 0          // Found the cell
            aWks[10] := { tmp,1,k }
            arr_     := Limget(aWks,substr(WKS_READ_BUFFER,1,ln+4))
            WKS_READ_POINTER := ln+5
            if  arr_[2] == "S"
               arr_  := Lfindnext(aWks)
            endif
         endif
      endif
   endif
endif
return arr_
*****************************************************************************
*±±± End of Lgetcell ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

STATIC function Is_cell(cVers,op,ln)
do case
case cVers == "L3" .or. cVers == "L4"
   return (op=20 .and. ln==4) .or. (op=21 .and. ln=4) .or. ;
          (op=22 .and. ln<519) .or. (op=23 .and. ln=14) .or. ;
          (op=24 .and. ln==6) .or. (op=25 .and. ln<2049) .or. ;
          (op=26 .and. ln<519)
case cVers == "E3" .or. cVers == "E4"
   return (op==638 .and. ln=10) .or. (op==519) .or. (op==1030) .or. ;
          (op==513 .and. ln==6) .or. (op==515 .and. ln==14) .or. ;
          (op==516) .or. (op==517 .and. ln==8) .or. (op==518)
case cVers == "E2"
   return (op==1 .and. ln==7) .or. (op==2 .and. ln==9) .or. ;
          (op==3 .and. ln==15) .or. (op==4) .or. (op==5 .and. ln==9) .or. ;
          (op==6) .or. (op==7)
otherwise
   return (op==12 .and. ln=5)  .or. (op=13 .and. ln=7) .or. ;
          (op==14 .and. ln=13) .or. (op=15 .and. ln<256) .or. ;
          (op==16 .and. ln<2065) .or. (op=51 .and. ln<256)
endcase
return .F.
*****************************************************************************
*±±± End of Is_cell ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Llocate()
* Purpose:    To locate a cell within the spreadsheet
* Syntax:     llocate( aSpreadsheet,cCell )
* Arguments:  aSpreadsheet - spreadsheet handle array
*             cCell        - Cell coordinates to locate
* Returns:    nOffset      - Offset in file where cell found
* Class:      Internal
*************************************************
STATIC function llocate( aWks,cText,nStart,nEnd )
LOCAL cBuffer  := space(BUFFSIZE), nSavePos, nPointer := 0
LOCAL cOverlap := '', nOverlap:=len(cText), tmp, nLen := 1
LOCAL nBytes   := BUFFSIZE

cText     :=  cText

Fseek(WKS_HANDLE,nStart,FS_SET)                        // Position file pointer
while nBytes == BUFFSIZE .and. nPointer == 0
   nBytes  := fread( WKS_HANDLE, @cBuffer, BUFFSIZE )
   if nBytes <> BUFFSIZE                       // At last buffer
     exit
   endif
   nPointer := at( cText,cOverlap+substr(cBuffer,1,nBytes) )
   if nPointer == 0  // Text wasn't found yet - assign new cOverlap
      cOverlap := right(substr(cBuffer,1,nBytes),nOverlap)
   endif
   if nPointer == 0 .and. ( fseek(WKS_HANDLE,0,FS_RELATIVE) > nEnd )
      exit
   endif
enddo

if nPointer == 0     // Look for text in last buffer
  nPointer := at( cText, cOverlap + substr( cBuffer, 1, nBytes ) )
endif

if nPointer > 0      // The text pointer was found
  nPointer -= ( len( cOverlap ) + nBytes + 1 )  // get amount to move back
  if fseek( WKS_HANDLE, nPointer, 1 )  <=nEnd             // Move back
     return fseek( WKS_HANDLE, 0, 1 )
  endif
endif
return -1
*****************************************************************************
*±±± End of Llocate() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Lfileread()
* Purpose:    To read a portion of an open file
* Syntax:     lfileread( nHandle,nStart,nBytes )
* Arguments:  nHandle      - file handle to read from
*             nStart       - starting offset into the file
*             nBytes       - number of bytes to read
* Returns:    cData        - Data read from the file
* Class:      Internal
*************************************************
STATIC function lfileread( nFh, nStart, nSize )
LOCAL cBuf:=space(nSize),nBytes
fseek( nFh, nStart )
nBytes  := fread( nFh, @cBuf, nSize )
return substr( cBuf, 1, nBytes )
*****************************************************************************
*±±± End of Lfileread ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

*   Function:  Limget()
*    Purpose:  To return an array of cell information
*     Syntax:  limget( aSpreadsheet,cBuffer )
*  Arguments:  aSpreadsheet - spreadsheet handle array
*              cBuffer      - data from the current cell
*    Returns:  aCell_info   - array of cell information
*************************************************
STATIC function limget(aWks,cBuffer)
LOCAL retval     := {},fmtbyte,fm,tst,is_spec,is_date:=.F.,nRow,nCol,cAddr,nSheet
LOCAL nOper      := bin2w(substr(cBuffer,1,2))
LOCAL nLength    := bin2w(substr(cBuffer,3,2))
LOCAL xData      := substr(cBuffer,5,nLength),nAttr,nX,lInt,lDiv,cTmp,x,y,z
LOCAL is_logical := .F.
//
// Test for Lotus 3.x
//
///////////////////////
do case
case WKS_VERSION $ "L3|L4"
   nCol    := asc(substr(xData,4,1))+1
   nRow    := bin2w(substr(xData,1,2))+1
   nSheet  := asc(substr(xData,3,1))+1
   cAddr   := Lc(nRow,nCol,nSheet)

   //
   // Need to somehow check if a date is flagged
   //
   ///////////////////////////
   if !empty(WKS_DATE_BITMAP)
      fm   := ascan(WKS_DATE_BITMAP[1],nRow )
      if fm > 0
         is_date := bit( WKS_DATE_BITMAP[2,fm],nCol )
      endif
   endif
   do case
   case nOper == 20                        // Error cell
      retval := { "ERR","E",3 }
   case nOper == 21                        // Not applicable cell
      retval := { "N/A","NA",3}
   case nOper == 22                        // Label
      retval := { strtran(substr(xData,6,nLength),chr(0),chr(32)),"C",nLength-5 }
   case nOper == 23                        // Number cell
      retval :=  treal(substr(xData,5,10))
      if is_date
         retval := { ctod("12/30/1899")+ retval,"D",10  }
      else
         retval := { retval,"N",10  }
      endif
   case nOper == 24                        // Small number cell
      retval :=  shortnum(substr(xData,5,2))
      if is_date
         retval := { ctod("12/30/1899")+ retval,"D",2  }
      else
         retval := { retval,"N",2  }
      endif
   case nOper == 25                        // Formula cell
      retval := treal(substr(xData,5,10))
      if substr(xData,15,2)==chr(51)+chr(03)
         retval := {.F.,"L",10 }
      elseif substr(xData,15,2)==chr(52)+chr(03)
         retval := {.T.,"L",10 }
      elseif substr(xData,15,2)==chr(55)+chr(03)
         retval := {ctod("12/30/1899")+ retval,"F",10 }
      elseif substr(xData,-2,2)==chr(54)+chr(03)
         retval := {ctod("12/30/1899")+ retval,"F",10 }
      elseif is_date
         retval := {ctod("12/30/1899")+ retval,"F",10 }
      else
         retval := {treal(substr(xData,5,10)),"F",10 }
      endif
   case nOper == 26
      retval := { strtran(substr(xData,5,nLength),chr(0),chr(32)),"C",nLength-4 }
   endcase
//
// All versions of Excel
//
//////////////////////////////////////////////////////////
case WKS_VERSION == "E2" .or. WKS_VERSION == "E3" .or. WKS_VERSION == "E4"
   nCol    := bin2w(substr(xData,3,2))+1
   nRow    := bin2w(substr(xData,1,2))+1
   nAttr   := 1
   if WKS_VERSION == "E2"
      nAttr := asc(substr(xData,6,1))
      if nAttr > 128 ; nAttr -=128 ; endif
      if nAttr > 64  ; nAttr -=64  ; endif
      nAttr++
      is_date := nAttr>=13 .and. nAttr<=16 .or. nAttr == 21
   else
      x       := bin2w(substr(xData,5,2))    // Pointer to extended format
      z       := ""
      if x >=0 .and. x < len(EXCEL_XF_RECORDS)
         y       := asc(substr(EXCEL_XF_RECORDS[x+1],2,1))
         if y >= 0 .and. y < len(EXCEL_FORMATS)
            z       := upper( EXCEL_FORMATS[y+1] )
         endif
      endif
      is_date := "D" $ z .and. "/" $ z
   endif

   cAddr   := Lc(nRow,nCol)
   do case
   case nOper == 1 .or. nOper == 513                // Blank cell
      retval := { "","B",0 }
   case nOper == 2                                  // Integer
      if is_date
         retval := { EXCEL_DATE_FORMAT+bin2w(substr(xData,8,2)),"D",2 }
      else
         retval := { bin2w(substr(xData,8,2)),"N",2 }
      endif
   case nOper == 3                                  // Floating point number
      if is_date
         retval := { EXCEL_DATE_FORMAT+real2iee(substr(xData,8,8)),"D",8 }
      else
         retval := { real2iee(substr(xData,8,8)),"N",8 }
      endif
   case nOper == 4                                  // Label (text) cell
      retval := { substr(xData,9,asc(substr(xData,8,1))),"C",;
                                 asc(substr(xData,8,1)) }
   case nOper == 5
      if asc(substr(xData,8,1))==0
         retval := { asc(substr(xData,9,1))==1,"L",1 }
      elseif asc(substr(xData,8,1))==1
         retval := { "ERR","E",1 }
      endif
   case nOper == 6
      if substr(xData,14,2)== HEX_FFFF   // Special value
         if asc(substr(xData,8,1))==0             // String
         elseif asc(substr(xData,8,1))==1         // Boolean
            retval := { asc(substr(xData,10,1))=1,"L",8 }
         elseif asc(substr(xData,8,1))==2         // Error
            retval := { asc(substr(xData,10,1)),"E",8 }
         endif
      else
         if is_date
            retval := { EXCEL_DATE_FORMAT+real2iee(substr(xData,8,8)),"F",8 }
         else
            retval := { real2iee(substr(xData,8,8)),"F",8 }
         endif
      endif
   case nOper == 515                                // Floating point number
      if is_date
         retval := { EXCEL_DATE_FORMAT+real2iee(substr(xData,7,8)),"D",8 }
      else
         retval := { real2iee(substr(xData,7,8)),"N",8 }
      endif
   case nOper == 516                                // Label (text) cell
      retval := { substr(xData,9,bin2w(substr(xData,7,2))),"C",;
                                 bin2w(substr(xData,7,2)) }
   case nOper == 517
      if asc(substr(xData,7,1))==0
         retval := { asc(substr(xData,8,1))==1,"L",1 }
      elseif asc(substr(xData,7,1))==1
         retval := { "ERR","E",1 }
      endif
   case nOper == 518 .or. nOper == 1030
      if substr(xData,13,2)== HEX_FFFF   // Special value
         if asc(substr(xData,7,1))==0             // String
         elseif asc(substr(xData,7,1))==1         // Boolean
            retval := { asc(substr(xData,9,1))=1,"L",8 }
         elseif asc(substr(xData,7,1))==2         // Error
            retval := { asc(substr(xData,9,1)),"E",8 }
         endif
      else
         if is_date
            retval := { EXCEL_DATE_FORMAT+real2iee(substr(xData,7,8)),"F",8 }
         else
            retval := { real2iee(substr(xData,7,8)),"F",8 }
         endif
      endif
   case nOper == 638                                // RK number
      cTmp := substr(xData,7,4)
      lDiv := bit(cTmp,8)                           // Divide by 100
      lInt := bit(cTmp,7)                           // Integer bit
      bit(cTmp,8,.F.)
      bit(cTmp,7,.F.)
      nX   := if(lInt,bin2l(cTmp),;
                  real2iee(chr(0)+chr(0)+chr(0)+chr(0)+cTmp) )
      if lInt
         nX /= 4
      endif
      if lDiv
         nX /= 100
      endif
      if is_date
        retval := { EXCEL_DATE_FORMAT+nX,"D",4 }
      else
        retval := { nX,"N",4 }
      endif
   otherwise
      retval := {NIL,NIL,NIL}
   endcase
//
// Lotus 2.x, Quattro, and all versions of Quattro Pro
//
//////////////////////////////////////////////////////////
otherwise
   fmtbyte := substr(xData,1,1)                // Format byte
   nCol    := bin2w(substr(xData,2,2))+1
   nRow    := bin2w(substr(xData,4,2))+1
   cAddr   := Lc(nRow,nCol)
   is_spec := ( bit(fmtbyte,2) .and. bit(fmtbyte,3) .and. bit(fmtbyte,4) )
   if is_spec
      tst     := if(bit(fmtbyte,6),4,0)+if(bit(fmtbyte,7),2,0)+if(bit(fmtbyte,8),1,0)
      is_date := tst>1 .and. tst<5   // We've got a date
   endif
   do case
   case nOper == 10
      retval := {}
   case nOper == 12                                 // Blank cell
      retval := { "","B",0 }
   case nOper == 13                                 // Integer cell
      if is_date
         retval := { ctod("12/30/1899")+bin2i(substr(xData,6,2)),"D",2 }
      else
         retval := { bin2i(substr(xData,6,2)),"N",2 }
      endif
   case nOper == 14                                 // Real number
      retval  := { real2iee(substr(xData,6,8)),"N",8 }
      if is_date
         retval := { ctod("12/30/1899")+real2iee(substr(xData,6,8)),"D",8 }
      endif
   case nOper == 16                                 // Formula
      retval  := real2iee(substr(xData,6,8))
      fm      := asc(substr(xData,nLength-1,1))
      if left(WKS_VERSION,1)=="Q"
         if (fm==113 .or. fm==114)  // TRUE/FALSE
            retval := { (retval == 1),"L",8 }
            is_logical := .T.
         elseif (fm==115 .or. fm==118 .or. ;
                 fm==206 .or. fm=142)                   // Date formulas
             is_date :=.T.
         endif

      else
         if (fm == 51 .or. fm == 52)  // TRUE/FALSE
            retval := { (retval == 1),"L",8 }
            is_logical := .T.
         elseif (fm == 55 .or. fm == 54 )    // Date formulas
             is_date :=.T.
         endif
      endif
      if is_date
         retval := { ctod("12/30/1899")+ retval,"D",8 }
      elseif .not.  is_logical
         retval := { retval,"F",8 }
      endif
   case nOper == 15      // Label
      if WKS_VERSION <> "QP"
         retval := substr(xData,7,nLength)
         retval := { substr(retval,1,len(retval)-1),"C",nLength-6 }
      else
         tst    := asc(substr(xData,6,1))
         retval := { substr(xData,8,tst),"C",nLength-7 }
      endif
   case nOper == 51                // String value of formula
      if WKS_VERSION <> "QP"
         retval := substr(xData,6,nLength)
         retval := { substr(retval,1,len(retval)-1),"C",nLength-5 }
      else
         tst    := asc(substr(xData,6,1))
         retval := { substr(xData,8,tst),"C",nLength-7 }
      endif
   endcase
endcase
//
// If we found a value, fill the array
//
/////////////////
if !empty(retval)
   Aadd(retval,cAddr)                                 // Cell address
   Aadd(retval,nRow)                                  // Row of spreadsheet
   Aadd(retval,nCol)                                  // Column
   Aadd(retval,WKS_READ_OFFSET+WKS_READ_POINTER-1)    // Physical file offset
endif
return retval
*****************************************************************************
*±±± End of Limget ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************










