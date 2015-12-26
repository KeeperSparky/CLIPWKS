*  Program.....: Writewks.prg
*  Authors.....: Joseph D. Booth
*  Copyright...: 1998 Joseph D. Booth, All rights reserved.
*  Application.: CLIPWKS++
*  Version.....: 1.1
*  Compile.....: xpp writewks /l /m /n /w
*                Lblank(aWks,cCell [,cFormat])
*                Lcreate(cFilename,cType)
*                Lfunc(aWks,cFormula,cCell [,cFormat])
*                Lput(aWks,xData,cCell [,cFormat])
*                Lwidth(aWks,cColumn|nColumn,nWidth)
*                Lparse()
*
****************************************************************************
#include "CLIPWKS.CH"

STATIC  nHighest_sheet  :=1


#define QP_FORMULAS     1
#define QP_CELLS        2
#define QP_RANGES       3

* Function:    Lblank()
* Purpose:     Write a blank cell into the worksheet
* Syntax:      Lblank( aSpreadsheet, cCell, cFormat )
* Arguments:   aSpreadsheet - spreadsheet handle array
*              cCell        - Cell address to write blank into
*              cFormat      - Format byte to write in cell
* Notes:       Lblank() allows you to write a blank cell into a
*              spreadsheet.  Unless the cell is referred to by a
*              formula or has a different format, blank cells are
*              normally not saved within a spreadsheet.
* Returns:     nStatus    - 0  All ok
*                         - 4  Invalid cell
*                         - 6  Invalid parameters
* See Also:    Lput()
***********************************
function lblank(aWks,cCell,cFormat)

LOCAL retval := ERR_INVALID_PARAMETERS, cell_spot, nFormat

if cFormat <> NIL
  nFormat := lcformat(cFormat,aWks)
else
  nFormat := DEFAULT_FORMAT
endif

if valtype(aWks) == "A" .and. !empty(aWks) .and. valtype(cCell) == "C"

   cell_spot := Lcell(cCell,WKS_VERSION)                // Cell location

   if !empty(cell_spot)                                 // If a valid cell

      do case
      case WKS_VERSION == "L3" .or. WKS_VERSION == "L4"
      case WKS_VERSION == "E2"
         Bwrite(E2_BLANK+cell_spot+i2bin(nFormat)+chr(0),aWks)
      case WKS_VERSION == "E3" .or. WKS_VERSION == "E4"
         Bwrite(E3_BLANK+cell_spot+i2bin(nFormat),aWks)
      otherwise
         bwrite(BLANK_OPCODE+chr(nFormat)+cell_spot,aWks)  // Write blank op
      endcase
      retval := ALL_OK                                  // and cell location

   else
      retval := ERR_INVALID_CELL

   endif
endif
return retval
*****************************************************************************
*±±± End of Lblank ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:    Lcreate()
* Purpose:     Creates a new spreadsheet
* Syntax:      Lcreate( cFilename, cVersion )
* Arguments:   cFilename      - Name of new spreadsheet to create
*              cVersion       - Spreadsheet version
*
*              Valid spreadsheet versions are:
*
*              QP  - Quattro Pro     L1  - Lotus 1.x
*              QU  - Quattro         L2  - Lotus 2.x
*                                    L3  - Lotus 3.x
*                                    L4  - Lotus 4.x
*              E2  - Excel 2.x
*              E3  - Excel 3.x
*              E4  - Excel 4.x
*
* Notes:       The Summer '87 version of Lcreate() requires a
*              third paramter, which is a 33 element array.  This
*              array must be declared prior to calling Lcreate().
*              In the Summer '87 version, Lcreate() returns a
*              logical value indicating whether or not the
*              spreadsheet was created.  In Clipper 5, an array
*              is returned.
*
* Returns:     aSpreadsheet
************************************
function lcreate(cFilename,cVersion)

LOCAL nWhere := at(".",cFilename), cExt, nHandle, arr_:={}
LOCAL nTop    := 3
LOCAL nLeft   := 3
LOCAL nPage   := 66
LOCAL nRight  := 78
LOCAL nBottom := 3
DEFAULT cVersion TO "L2"
if substr(cVersion,1,1)=="E"
   nTop    := 1
   nLeft   := 1
   nPage   := 66
   nRight  := 1
   nBottom := 1
endif
cExt      := Lext(cVersion)
cFilename := if( nWhere=0,trim(cFilename)+cExt,cFilename )
nHandle   := fcreate( cFilename )

if nHandle > 0


   arr_ :={  cFilename,;                               // Spreadsheet name
             cVersion,;                                // Version code
             nHandle,;                                 // Handle file opened on
             .T.,;                                     // Updates allowed?
             "NEW",;                                   // New / old file
             { 1,1 },;                                 // Rows/columns
             {"L","A","N",;
                 if(substr(cVersion,1,1)=="E",100,1),;
                 "P",114},;                             // Miscellaneous parameters
             .F.,;                                      // Data written flag
             "",;                                       // Data buffer
             { "","","","","F",nLeft,nRight,nPage,nTop,nBottom,{"",""}},;
             { "",{"",0},{"",0} },;                     // Sort array
             {},;                                       // Named ranges
             {},;                                       // Column widths
             "A1" }                                     // First cell
   if cVersion $"L3|L4" .or. substr(cVersion,1,1)=="E"  // Formats, L3 and
      Aadd(arr_,{} )                                    // Excel, L3 only
   endif
endif
return arr_
*****************************************************************************
*±±± End of Lcreate ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


*
* Function:    Lfunc()
* Purpose:     Write an @function into the spreadsheet
* Syntax:      Lfunc( aSpreadsheet, cFormula, cCell )
* Arguments:   aSpreadsheet - spreadsheet handle array
*              cFormula     - Formula to write in cell
*              cCell        - Cell address
*
* Notes:       Lfunc() is used mainly for compatibility with
*              earlier CLIPWKS releases.  Lput() handles formula
*              much more powerfully than Lfunc() can.  If you are
*              writing new formulas to the spreadsheet, use
*              Lput() instead.
* Returns:     nStatus
* See Also:    Lput()
**************************************************
function lfunc(aWks,cFormula,cCell,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)

LOCAL parms:="",arr_:={ arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9 },k

for k=1 to len(arr_)

   if valtype( arr_[k] ) == "C"
      parms += arr_[k]+","
   elseif valtype( arr_[k] ) == "N"
      parms += str(arr_[k])+","
   endif

next
parms := if(substr(parms,len(parms),1)==",",substr(parms,1,len(parms)-1),parms)
// Build a valid lput() call
return lput(aWks,"@"+cFormula+"("+parms+")",cCell)
******************************************************************************
*±±± End of Lfunc ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************


* Function:    Lput()
* Purpose:     Writes data to the spreadsheet
* Syntax:      Lput( aSpreadsheet, xData, cCell_address ,cFormat )
* Arguments:   aSpreadsheet   - spreadsheet handle array
*              xData          - Data to write
*              cCell_address  - Cell address to write data at
*              cFormat        - Format byte for the cell
* Notes:       The type of data determines what gets written in
*              the cell.  Numeric data is written out directly,
*              logical data is converted to either the @TRUE or
*              @FALSE function, and dates are converted to the
*              @DATE function.  Character data is intrepretted by
*              the first byte.  If the first byte is a plus sign
*              (+) or an ampersand (@), then it is assume to be a
*              formula and the formula will be written out.  Any
*              other first character will result in the data
*              being written directly to the spreadsheet.
*
*              For example:
*
*              Lput(aWks,"+A5*@SUM(B2..B6)","A1")
*              Lput(aWks,date(),"A2")
*              Lput(aWks,PAYROLL->salary,"A3")
*
* Returns:     nStatus
* See Also:    Lblank() Lfunc()
**************************************************
function lput(aWks,_data,_cell,cFormat,lWrite)

LOCAL retval:=0,_string:="",temp:="",places,cell_spot:=Lcell(_cell,WKS_VERSION)
LOCAL lalign:=chr(94),lversion,temp_out:="",f_h,_row,_col,p_:={},k,r1,c1,nFormat
LOCAL aQpro:={ "","","" },arr_,x,y,nXf := 0
LOCAL lProtect



if valtype(_data)=="D" .and. (WKS_VERSION == "E3" .or. WKS_VERSION=="E4")
   nXf := 19
elseif valtype(_data)=="D" .and. WKS_VERSION == "E2"
   nXf := 13
endif

if lWrite==NIL
   lWrite := .T.
endif

if cFormat <> NIL
  if substr(cFormat,1,1)$"+-"        // If protection was specified
     lProtect := substr(cFormat,1,1)=="+"
     cFormat  := substr(cFormat,2)
  endif
  if !empty(cFormat)
     if substr(WKS_VERSION,1,1)=="E"
        nXf := Lcformat(cFormat,aWks,lProtect)
     else
        nFormat := lcformat(cFormat,aWks,lProtect)
     endif
  else
     nFormat := DEFAULT_FORMAT
     if nFormat > 127 .and. !lProtect
        nFormat -= 128
     elseif nFormat < 128 .and. lProtect
        nFormat += 128
     endif
  endif
else
  nFormat := DEFAULT_FORMAT
endif
if lWrite
   retval   := if(WKS_TYPE=="NEW",ALL_OK,ERR_INVALID_PARAMETERS)
endif

if retval==ALL_OK

   // Since this is only appropiate for character cells, move it to there

   if lWrite
      lalign    := if(WKS_ALIGNMENT=='R',chr(34),if(WKS_ALIGNMENT=='C',chr(94),chr(39)))
   endif

    // Probably can convert all lversion to WKS_VERSION without any ills

   lversion  := WKS_VERSION
   _row      := bin2i(substr(cell_spot,1,2))
   if lversion $ "L3|L4"
      _col      := asc(substr(cell_spot,4,1))
      if asc(substr(cell_spot,3,1)) > nHighest_sheet
         nHighest_sheet := asc(substr(cell_spot,3,1))
      endif
   else
      _col      := bin2i(substr(cell_spot,3,2))
   endif

   do case
   case valtype(_data)=="C"                   // for characters
      if substr(_data,1,1)=="@"
         _data := "+"+_data
      endif
      if substr(_data,1,1) $ "-+@="           // If first character is a +
         if substr(_data,1,1) $ "+-"
            p_   := Parse(_data)             // or @, assume a formula
            if !empty(p_)
               if WKS_VERSION <> "QP" .and. substr(WKS_VERSION,1,1)<>"E"
                  for k=len(p_) to 1 step -1
                     if substr(p_[k],1,1)=="@"
                        temp_out += makeformula( p_[k],lversion,_row,_col )
                     elseif val(p_[k]) == 0
                        y := len(p_[k])
                        if p_[k] <> replicate("0",y)
                           temp_out += to_123( p_[k],lversion,_row,_col )
                        else
                           temp_out += to_123( val(p_[k]),lversion,_row,_col )
                        endif
                     else
                        temp_out += to_123( val(p_[k]),lversion,_row,_col )
                     endif
                  next
                  temp_out += to_123("ENDMARK",lversion)
               elseif substr(WKS_VERSION,1,1)=="E"
                  for k=len(p_) to 1 step -1
                     if substr(p_[k],1,1)=="="
                        temp_out += makeformula( p_[k],lversion,_row,_col )
                     elseif val(p_[k]) == 0
                        y := len(p_[k])
                        if p_[k] <> replicate("0",y)
                           temp_out += to_123( p_[k],lversion,_row,_col )
                        else
                           temp_out += to_123( val(p_[k]),lversion,_row,_col )
                        endif
                     else
                        temp_out += to_123( val(p_[k]),lversion,_row,_col )
                     endif
                  next
               else
                  // Quattro Pro formula
                  for k=len(p_) to 1 step -1
                     if substr(p_[k],1,1)=="@"
                        arr_ := makeformula( p_[k],lversion,_row,_col )
                        aQpro[QP_FORMULAS] += arr_[QP_FORMULAS]
                        aQpro[QP_RANGES]   += arr_[QP_RANGES]
                        aQpro[QP_CELLS]    += arr_[QP_CELLS]
                     elseif val(p_[k]) == 0
                        if ".." $ p_[k]
                           aQpro[QP_FORMULAS] += To_123("RANGEMARK","QP",_Row,_Col)
                           aQpro[QP_RANGES] += to_123( p_[k],lversion,_row,_col )
                        elseif len(p_[k]) > 1
                           aQpro[QP_FORMULAS] += To_123("CELL_ADDR","QP",_Row,_Col)
                           aQpro[QP_CELLS] += to_123( p_[k],lversion,_row,_col )
                        else
                           aQpro[QP_FORMULAS] += to_123( p_[k],lversion,_row,_col )
                        endif
                     else
                        aQpro[QP_FORMULAS] += to_123( val(p_[k]),lversion,_row,_col )
                     endif
                  next
                  aQpro[QP_FORMULAS] += to_123("ENDMARK",lversion)
                  temp_out+=i2bin(len(aQpro[QP_FORMULAS])+4)+;
                            i2bin(len(aQpro[QP_FORMULAS])+4+len(aQpro[QP_CELLS]))+;
                            aQpro[QP_FORMULAS]+aQpro[QP_CELLS]+aQpro[QP_RANGES]
               endif
            endif
         else
            if WKS_VERSION $"L3|L4"
               r1       := bin2w( substr(cell_spot,1,2) )
               c1       := asc( substr(cell_spot,4,1) )
            else
               r1       := bin2w( substr(cell_spot,1,2) )
               c1       := bin2w( substr(cell_spot,3,2) )
            endif
            if WKS_VERSION <> "QP"
               if substr(WKS_VERSION,1,1)<>"E"
                  temp_out := Makeformula(_data,WKS_VERSION,r1,c1)
                  temp_out += to_123("ENDMARK",lversion)
               else
                  temp_out := Makeformula(_data,WKS_VERSION,r1,c1)
               endif
            else
               // Quattro Pro formula
               aQpro := Makeformula(_data,WKS_VERSION,r1,c1)
               aQpro[QP_FORMULAS] += to_123("ENDMARK",lversion)
               temp_out+=i2bin(len(aQpro[QP_FORMULAS])+4)+;
                         i2bin(len(aQpro[QP_FORMULAS])+4+len(aQpro[QP_CELLS]))+;
                         aQpro[QP_FORMULAS]+aQpro[QP_CELLS]+aQpro[QP_RANGES]
            endif
         endif
         if !empty(temp_out)
            if WKS_VERSION $ "L3|L4"
               _string := WK3_FORMULACELL+i2bin(14+len(temp_out))+;
                          cell_spot+treal(0)+temp_out
           elseif WKS_VERSION == "E2"
              _string := E2_FORMULA+i2bin(17+len(temp_out))+;
                         cell_spot+chr(0)+chr(nXf)+chr(0)+ieereal(0)+chr(1)+chr(len(temp_out))+;
                         temp_out
           elseif WKS_VERSION == "E3"
              _string := E3_FORMULA+i2bin(18+len(temp_out))+;
                         cell_spot+i2bin(nXf)+ieereal(0)+i2bin(1)+i2bin(len(temp_out))+;
                         temp_out
           elseif WKS_VERSION == "E4"
              _string := E4_FORMULA+i2bin(18+len(temp_out))+;
                         cell_spot+i2bin(nXf)+ieereal(0)+i2bin(1)+i2bin(len(temp_out))+;
                         temp_out
           else
               _string := FORMULA_OPCODE+i2bin(15+len(temp_out))+chr(nFormat)+;
                          cell_spot+ieereal(0)+i2bin(len(temp_out))+temp_out
            endif
         endif

      elseif substr(_data,1,1)$chr(34)+"\'^|"

         if WKS_VERSION == "QP"

            _data  :=if(len(_data)<=240,_data,substr(_data,1,240))
            _string:=LABEL_OPCODE+i2bin(Len(_data)+7)+chr(nFormat)+cell_spot+;
                     substr(_data,1,1)+chr(len(_data)-1)+substr(_data,2)+chr(1)

         elseif WKS_VERSION $ "L3|L4"

            _data  :=if(len(_data)<=512,_data,substr(_data,1,512))
            _string:=WK3_LABELCELL+i2bin(Len(_data)+5)+cell_spot+_data+chr(0)

         elseif WKS_VERSION == "E2"
            _data  :=if(len(_data)<=255,_data,substr(_data,1,255))
            _string:=E2_LABEL+i2bin(Len(_data)+7)+cell_spot+chr(0)+i2bin(0)+chr(len(_data)-1)+substr(_data,2)

         elseif WKS_VERSION == "E3" .or. WKS_VERSION == "E4"
            x := at( substr(_data,1,1),['^"\] )
            x := 0
            _data  :=if(len(_data)<=255,_data,substr(_data,1,255))
            _string:=E3_LABEL+i2bin(Len(_data)+7)+cell_spot+;
                     i2bin(x)+i2bin(len(_data)-1)+substr(_data,2)

         else
            _data  :=if(len(_data)<=241,_data,substr(_data,1,241))
            _string:=LABEL_OPCODE+i2bin(Len(_data)+6)+chr(nFormat)+cell_spot+_data+chr(0)
         endif

      else
         if WKS_VERSION == "QP"

            _data  :=if(len(_data)<=240,_data,substr(_data,1,240))
            _string:=LABEL_OPCODE+i2bin(Len(_data)+8)+chr(nFormat)+cell_spot+lalign+;
                     chr(len(_data))+_data +chr(1)

         elseif WKS_VERSION $ "L3|L4"

            _data  :=if(len(_data)<=512,_data,substr(_data,1,512))
            _string:=WK3_LABELCELL+i2bin(Len(_data)+6)+cell_spot+"'"+_data+chr(0)

         elseif WKS_VERSION == "E2"
            _data  :=if(len(_data)<=255,_data,substr(_data,1,255))
            _string:=E2_LABEL+i2bin(Len(_data)+8)+cell_spot+chr(0)+i2bin(0)+chr(len(_data))+_data
         elseif WKS_VERSION == "E3" .or. WKS_VERSION == "E4"
            _data  :=if(len(_data)<=255,_data,substr(_data,1,255))
            _string:=E3_LABEL+i2bin(Len(_data)+8)+cell_spot+i2bin(0)+i2bin(len(_data))+_data

         else

            _data  :=if(len(_data)<=240,_data,substr(_data,1,240))
            _string:=LABEL_OPCODE+i2bin(Len(_data)+7)+chr(nFormat)+cell_spot+lalign+_data+chr(0)
         endif
      endif

   case valtype(_data)=="L"        // Logicals ******************************

      if WKS_VERSION == "E2"
         _string := E2_BOOLERR+cell_spot+chr(0)+chr(nXf)+chr(0)+;
                    chr( if(_data,1,0) )+chr(0)
      elseif WKS_VERSION == "E3"
         _string := E3_FORMULA+i2bin(20)+cell_spot+i2bin(0)+;
                    ieereal( if(_data,1,0) )+i2bin(0)+;
                    i2bin(2)+chr(29)+chr(if(_data,1,0))
      elseif WKS_VERSION == "E4"
         _string := E4_FORMULA+i2bin(20)+cell_spot+i2bin(0)+;
                    ieereal( if(_data,1,0) )+i2bin(0)+;
                    i2bin(2)+chr(29)+chr(if(_data,1,0))
      else
         if _data
            return lput(aWks,"@TRUE",_cell)
         else
            return lput(aWks,"@FALSE",_cell)
         endif
      endif

   case valtype(_data)=="D"        // Dates *********************************

       if WKS_VERSION == "E2"
          _string := E2_NUMBER+cell_spot+chr(0)+chr(nXf)+chr(0)+ieereal(_data-ctod("12/31/1899") )
       elseif WKS_VERSION == "E3" .or. WKS_VERSION == "E4"
         _string := E3_NUMBER+cell_spot+i2bin(nXf)+ieereal(_data-ctod("12/30/1899") )
       else
          cFormat := if( cFormat==NIL,"D1",cFormat )
          if !empty(_data)
             return lput(aWks,"@DATE("+alltrim(str(year(_data)-1900,4))+","+;
                                       str(month(_data),2)+","+;
                                       str(day(_data),2)+")",_cell,cFormat)
          else
             return lput(aWks,"@DATE(-1,-1,-1)",_cell,cFormat)
          endif
       endif

   case valtype(_data)=="N"        // Numeric data
      temp   := str(_data)
      places := if(at(".",temp)=0,0,Len(temp)-at(".",temp))
      if WKS_VERSION $ "L3|L4"
         if _data >= -16384 .and. _data <= 16383 .and. places==0
            _string := WK3_SMALLNUMCELL+cell_spot+shortnum( _data )
         else
            _string := WK3_NUMBERCELL+cell_spot+treal( _data )
         endif
      elseif WKS_VERSION == "E2"
         _string := E2_NUMBER+cell_spot+chr(0)+chr(nXf)+chr(0)+ieereal(_data)
      elseif WKS_VERSION=="E3" .or. WKS_VERSION=="E4"
         _string := E3_NUMBER+cell_spot+i2bin(nXf)+ieereal(_data)

      else
         if nFormat == DEFAULT_FORMAT
            if places <>0
               _string:=REAL_OPCODE+chr(128+places)+cell_spot+ieereal(_data)
            else
               _string:=REAL_OPCODE+chr(192)+cell_spot+ieereal(_data)
            endif
         else
            _string:=REAL_OPCODE+chr(nFormat)+cell_spot+ieereal(_data)
         endif
      endif
   endcase
   if lWrite
      Bwrite(_string,aWks)
   else
      return _string
   endif
endif
return retval
*****************************************************************************
*±±± End of Lput ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:    Lwidth()
* Purpose:     Set the column to the specified width
* Syntax:      Lwidth( aSpreadsheet, cColumn|nColumn, nWidth )
* Arguments:   aSpreadsheet - Spreadsheet handle array
*              cColumn      - Column letter
*              nColumn      - Numeric letter
*              nWidth       - Width to set
*              nSheet       - Sheet to set width for
* Notes:       Lwidth() is used to set a column width.  You may
*              specify the column using letter notation or by
*              refering to the column number.  Widths should be
*              set prior to any data being written to the
*              spreadsheet.
* Returns:     nStatus
**************************************************
function lwidth(aWks,xColumn,nWidth,nSheet)

LOCAL retval := if(valtype(WKS_TYPE)=="C",ALL_OK,ERR_INVALID_PARAMETERS)
LOCAL _hold
LOCAL x

DEFAULT nWidth  TO  DEFAULT_WIDTH
DEFAULT nSheet  TO  0

if xColumn == NIL
   xColumn := 0
endif

if retval == ALL_OK
   // Convert alphabetic column references to numeric
   if valtype(xColumn)=="C"
      _hold := upper(trim(xColumn))
      if ":" $ _hold .and. WKS_VERSION $"L3|L4"
          x      := at(":",_hold)
          if x = 2
             nSheet := asc(_hold)-65
             _hold  := substr(_hold,3)
          else
             nSheet := (asc(_hold)-64)*26 + (asc(substr(_hold,2,1))-65)
             _hold  := substr(_hold,4)
          endif
      endif
      if len(_hold)=1 .and. _hold$"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         xColumn := asc(_hold)-64
      elseif len(_hold)=2 .and. ;
         substr(_hold,1,1)$"ABCDEFGHI" .and. ;
         substr(_hold,2,1)$"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
         xColumn := (asc(substr(_hold,1,1))-64)*26+;
                    (asc(substr(_hold,2,1))-64)
      endif
   endif
   if valtype(xColumn)="N"
      if xColumn = 0
         if ! ( WKS_VERSION $"L3|L4" )
            Aadd( WKS_WIDTHS,{ 9999,nWidth } )
         endif
      else
         if WKS_VERSION $ "L3|L4"
            if nSheet > nHighest_sheet
               nHighest_sheet := nSheet
            endif
            Aadd( WKS_WIDTHS,str(nSheet-1,3)+chr(xColumn-1)+chr(nWidth) )
         else
            Aadd( WKS_WIDTHS,{ xColumn-1,nWidth } )
         endif
      endif
      retval := ALL_OK
   endif
endif
return retval
*****************************************************************************
*±±± End of Lwidth ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:    Ldirty()
* Purpose:     Writes the global parameters to disk before data is written
* Syntax:      Ldirty( aSpreadsheet )
* Arguments:   aSpreadsheet - Spreadsheet handle array
* Notes:       CLIPWKS maintains all global parameters in the
*              spreadsheet array until you request an Lput()
*              function.  Once you call Lput(), the global
*              information is formatted and written to the
*              spreadsheet file.  You should never need to call
*              Ldirty() directly.
* Returns:     NIL
* Class:       Internal
**************************************************
function ldirty(aWks)

LOCAL buffer   := "",jj,nCol,nWide,cName,cRange,tmp,outs,x,ct,nRow,nSheet,zz
LOCAL cExAlign := "ð"

if !WKS_DATA_WRITTEN                        // Parameter data not yet saved
******************************************************************************
* Place a BOF op code into the file ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************
   do case
   case WKS_VERSION == "L1"
      buffer := BOF_OPCODE + i2bin( 1028 )
   case WKS_VERSION == "L2"
      buffer := BOF_OPCODE + i2bin( 1030 )
   case WKS_VERSION == "L3"
      buffer = WK3_BOF+i2bin(0)+i2bin( 0)+i2bin(WKS_ROWS-1)+;
               chr(nHighest_sheet-1)+chr(WKS_COLUMNS-1)+i2bin(1)+i2bin( 0)+;
               chr(1)+chr(2)+replicate(chr(0),8)
   case WKS_VERSION == "L4"
      buffer = WK4_BOF+i2bin(0)+i2bin( 0)+i2bin(WKS_ROWS-1)+;
               chr(nHighest_sheet)+chr(WKS_COLUMNS-1)+i2bin(1)+i2bin( 0)+;
               chr(1)+chr(2)+replicate(chr(0),8)
   case WKS_VERSION == "QU"
      buffer := BOF_OPCODE + i2bin( 2571 )
   case WKS_VERSION == "QP"
      buffer := BOF_OPCODE + i2bin( 20768 )
   case WKS_VERSION == "E2"
      buffer := E2_BOF_OPCODE
   case WKS_VERSION == "E3"
      buffer := E3_BOF_OPCODE
   case WKS_VERSION == "E4"
      buffer := E4_BOF_OPCODE
   endcase
******************************************************************************
* Excel 2.x ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************
   if WKS_VERSION == "E2"
      buffer += E2_CODEPAGE        +i2bin(32769)
      buffer += E2_CALCCOUNT       +i2bin( WKS_ITERATION )
      buffer += E2_CALCMODE        +i2bin(if(WKS_CALCMODE=="M",0,if(WKS_CALCMODE=="T",-1,1)))

      buffer += E2_PRECISION       +i2bin(1)
      buffer += E2_REFMODE         +i2bin(1)
      buffer += E2_DELTA           +"ü©ñÒMbP?"
      buffer += E2_ITERATION       +i2bin(0)
      buffer += E2_1904            +i2bin(0)
      buffer += E2_PRINT_HEADERS   +i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E2_PRINT_GRIDLINES +i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E2_PROTECT         +i2bin(if(WKS_PROTECTION=="P",1,0))
      if empty(WKS_HEADER)
         buffer += E2_HEADER+i2bin( 3)+chr(2)+"&f"
      else
         buffer += E2_HEADER+i2bin( 1+len(WKS_HEADER) )+chr(len(WKS_HEADER))+WKS_HEADER
      endif
      if empty(WKS_FOOTER)
         buffer += E2_FOOTER+i2bin( 8)+chr(7)+"Page &p"
      else
         buffer += E2_FOOTER+i2bin( 1+len(WKS_FOOTER) )+chr(len(WKS_FOOTER))+WKS_FOOTER
      endif
      buffer += E2_LEFT_MARGIN     +ieereal(WKS_LEFT)
      buffer += E2_RIGHT_MARGIN    +ieereal(WKS_RIGHT)
      buffer += E2_TOP_MARGIN      +ieereal(WKS_TOP)
      buffer += E2_BOTTOM_MARGIN   +ieereal(WKS_BOTTOM)
      buffer += E2_BACKUP          +i2bin(0)
      buffer += E3_FORMAT+i2bin( 8)+chr( 7)+"General"
      buffer += E3_FORMAT+i2bin( 2)+chr( 1)+"0"
      buffer += E3_FORMAT+i2bin( 5)+chr( 4)+"0.00"
      buffer += E3_FORMAT+i2bin( 6)+chr( 5)+"#,##0"
      buffer += E3_FORMAT+i2bin( 9)+chr( 8)+"#,##0.00"
      buffer += E3_FORMAT+i2bin(22)+chr(21)+'"$"#,##0;\("$"#,##0\)'
      buffer += E3_FORMAT+i2bin(27)+chr(26)+'"$"#,##0;[Red]\("$"#,##0\)'
      buffer += E3_FORMAT+i2bin(28)+chr(27)+'"$"#,##0.00;\("$"#,##0.00\)'
      buffer += E3_FORMAT+i2bin(33)+chr(32)+'"$"#,##0.00;[Red]\("$"#,##0.00\)'
      buffer += E3_FORMAT+i2bin( 3)+chr( 2)+"0%"
      buffer += E3_FORMAT+i2bin( 6)+chr( 5)+"0.00%"
      buffer += E3_FORMAT+i2bin( 9)+chr( 8)+"0.00E+00"
      buffer += E3_FORMAT+i2bin( 7)+chr( 6)+"m/d/yy"
      buffer += E3_FORMAT+i2bin(11)+chr(10)+"d\-mmm\-yy"
      buffer += E3_FORMAT+i2bin( 7)+chr( 6)+"d\-mmm"
      buffer += E3_FORMAT+i2bin( 8)+chr( 7)+"mmm\-yy"
      buffer += E3_FORMAT+i2bin(12)+chr(11)+"h:mm\ AM/PM"
      buffer += E3_FORMAT+i2bin(15)+chr(14)+"h:mm:ss\ AM/PM"
      buffer += E3_FORMAT+i2bin( 5)+chr( 4)+"h:mm"
      buffer += E3_FORMAT+i2bin( 8)+chr( 7)+"h:mm:ss"
      buffer += E3_FORMAT+i2bin(13)+chr(12)+"m/d/yy\ h:mm"
      buffer += E2_DEFAULTWIDTH+i2bin(8)
      for x=1 to len(WKS_WIDTHS)
         if WKS_WIDTHS[x,1] < 9999
            buffer += E2_COLWIDTH+chr(WKS_WIDTHS[x,1])+chr(WKS_WIDTHS[x,1])+;
                                  i2bin(WKS_WIDTHS[x,2]*256)
         endif
      next
      buffer += E2_DIMENSIONS      +i2bin(0)+i2bin( WKS_ROWS+1 )+;
                                    i2bin(0)+i2bin( WKS_COLUMNS+1 )


******************************************************************************
* Excel 3.x and 4.x ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************
   elseif WKS_VERSION == "E3" .or. WKS_VERSION == "E4"
      buffer += E3_WRITEACCESS+chr(15)+padr("CLIPWKS Ver 1.5",31)
      if WKS_VERSION == "E3"
         buffer += E2_CODEPAGE  +i2bin(32769)
      else
         buffer += E2_CODEPAGE  +i2bin( 1252)
      endif
      buffer += E2_CALCCOUNT+i2bin( WKS_ITERATION )
      buffer += E2_CALCMODE+i2bin(if(WKS_CALCMODE=="M",0,if(WKS_CALCMODE=="T",-1,1)))
      buffer += E2_PRECISION+i2bin(1)
      buffer += E2_REFMODE+i2bin(1)
      buffer += E2_DELTA+"ü©ñÒMbP?"
      buffer += E2_ITERATION+i2bin(0)
      buffer += E2_1904+i2bin(0)
      buffer += E2_PRINT_HEADERS+i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E2_PRINT_GRIDLINES+i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E3_SAVERECALC+i2bin(0)
      buffer += E3_GRIDSET+i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E3_HCENTER+i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E3_VCENTER+i2bin(if(WKS_PRINT=="F",1,0))
      buffer += E3_GUTS+i2bin(0)+i2bin(0)
      buffer += E3_DEFAULT_HEIGHT+i2bin(0)+i2bin(255)
      buffer += E3_COUNTRY+i2bin(1)+i2bin(1)
      buffer += E3_HIDEOBJ+i2bin(0)
      buffer += E3_WSBOOL+chr(193)+chr(4)
      for x= 0 to 3
         buffer += E3_FONT+i2bin(20)+i2bin(200)+i2bin(x)+chr(255)+chr(127)+chr(13)+"MS Sans Serif"
      next
      buffer += E3_FONT+i2bin(20)+i2bin(200)+i2bin(0)+chr(255)+chr(127)+chr(13)+"MS Sans Serif"

      if empty(WKS_HEADER)
         buffer += E2_HEADER+i2bin( 3)+chr(2)+"&f"
      else
         buffer += E2_HEADER+i2bin( 1+len(WKS_HEADER) )+chr(len(WKS_HEADER))+WKS_HEADER
      endif
      if empty(WKS_FOOTER)
         buffer += E2_FOOTER+i2bin( 8)+chr(7)+"Page &p"
      else
         buffer += E2_FOOTER+i2bin( 1+len(WKS_FOOTER) )+chr(len(WKS_FOOTER))+WKS_FOOTER
      endif
      buffer += E2_LEFT_MARGIN+ieereal(WKS_LEFT)
      buffer += E2_RIGHT_MARGIN+ieereal(WKS_RIGHT)
      buffer += E2_TOP_MARGIN+ieereal(WKS_TOP)
      buffer += E2_BOTTOM_MARGIN+ieereal(WKS_BOTTOM)
      buffer += E2_BACKUP+i2bin(0)
      buffer += E2_BUILTINFORMAT+i2bin(23)



      buffer += E3_FORMAT+i2bin( 8)+chr( 7)+"General"
      buffer += E3_FORMAT+i2bin( 2)+chr( 1)+"0"
      buffer += E3_FORMAT+i2bin( 5)+chr( 4)+"0.00"
      buffer += E3_FORMAT+i2bin( 6)+chr( 5)+"#,##0"
      buffer += E3_FORMAT+i2bin( 9)+chr( 8)+"#,##0.00"
      buffer += E3_FORMAT+i2bin(24)+chr(23)+'"$"#,##0_);\("$"#,##0\)'
      buffer += E3_FORMAT+i2bin(29)+chr(28)+'"$"#,##0_);[Red]\("$"#,##0\)'
      buffer += E3_FORMAT+i2bin(30)+chr(29)+'"$"#,##0.00_);\("$"#,##0.00\)'
      buffer += E3_FORMAT+i2bin(35)+chr(34)+'"$"#,##0.00_);[Red]\("$"#,##0.00\)'
      buffer += E3_FORMAT+i2bin( 3)+chr( 2)+"0%"
      buffer += E3_FORMAT+i2bin( 6)+chr( 5)+"0.00%"
      buffer += E3_FORMAT+i2bin( 9)+chr( 8)+"0.00E+00"
      buffer += E3_FORMAT+i2bin( 7)+chr( 6)+"#\ ?/?"
      buffer += E3_FORMAT+i2bin( 9)+chr( 8)+"#\ ??/??"
      buffer += E3_FORMAT+i2bin( 7)+chr( 6)+"m/d/yy"
      buffer += E3_FORMAT+i2bin(11)+chr(10)+"d\-mmm\-yy"
      buffer += E3_FORMAT+i2bin( 7)+chr( 6)+"d\-mmm"
      buffer += E3_FORMAT+i2bin( 8)+chr( 7)+"mmm\-yy"
      buffer += E3_FORMAT+i2bin(12)+chr(11)+"h:mm\ AM/PM"
      buffer += E3_FORMAT+i2bin(15)+chr(14)+"h:mm:ss\ AM/PM"
      buffer += E3_FORMAT+i2bin( 5)+chr( 4)+"h:mm"
      buffer += E3_FORMAT+i2bin( 8)+chr( 7)+"h:mm:ss"
      buffer += E3_FORMAT+i2bin(13)+chr(12)+"m/d/yy\ h:mm"

      buffer += E3_NAME+i2bin(7)+i2bin(32)+chr(47)+chr(1)+i2bin(0)+chr(5)
      buffer += E3_NAME+i2bin(7)+i2bin(32)+chr(47)+chr(1)+i2bin(0)+chr(4)
      buffer += E3_NAME+i2bin(7)+i2bin(32)+chr(47)+chr(1)+i2bin(0)+chr(9)
      buffer += E3_NAME+i2bin(7)+i2bin(32)+chr(47)+chr(1)+i2bin(0)+chr(6)
      buffer += E3_NAME+i2bin(7)+i2bin(32)+chr(47)+chr(1)+i2bin(0)+chr(7)
      buffer += E3_NAME+i2bin(7)+i2bin(32)+chr(47)+chr(1)+i2bin(0)+chr(8)

      for x := 1 to len(EXCEL_NAMED_RANGES)
         zz := len(alltrim(EXCEL_NAMED_RANGES[x,1]))
         buffer += E3_NAME+i2bin( 13+zz )+i2bin(0)+"/"+;
                   chr(zz)+i2bin(7)+EXCEL_NAMED_RANGES[x,1]+chr(45)+;
                   left(EXCEL_NAMED_RANGES[x,2],2)+;
                   substr(EXCEL_NAMED_RANGES[x,2],5,2)+;
                   substr(EXCEL_NAMED_RANGES[x,2],3,1)+;
                   substr(EXCEL_NAMED_RANGES[x,2],7,1)
      next

      buffer += E2_PROTECT+i2bin(if(WKS_PROTECTION=="P",1,0))
      buffer += i2bin(25)+i2bin(2)+i2bin(0)
      buffer += i2bin(99)+i2bin(2)+i2bin(0)
      buffer += i2bin(19)+i2bin(2)+i2bin(0)

      if WKS_ALIGNMENT = "R"
         cExAlign := chr(243)
      endif
      if WKS_ALIGNMENT = "C"
         cExAlign := chr(242)
      endif

      buffer += XF_OPCODE+i2bin(0)+i2bin(5)+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+i2bin(1)+chr(5)+"ô"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+i2bin(1)+chr(5)+"ô"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+i2bin(2)+chr(5)+"ô"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+i2bin(2)+chr(5)+"ô"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      for x=1 to 10
         buffer += XF_OPCODE+i2bin(0)+chr(5)+"ô"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      next
      buffer += XF_OPCODE+i2bin(0)+i2bin(1)+i2bin(0)+chr(0)+chr(206)+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(5)+chr(4)+chr(5)+"ø"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(5)+chr(8)+chr(5)+"ø"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(5)+chr(9)+chr(5)+"ø"+cExAlign+"ÿ"+chr(0)+"Î"+i2bin(0)+i2bin(0)
      //
      // Special XF records for D1,D2,D3,Percent,comma,currency,sci,fix
      //
      buffer += XF_OPCODE+chr(0)+chr(14)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr(17)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr(16)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 9)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr(10)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 3)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 4)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 5)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 7)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr(11)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 1)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)
      buffer += XF_OPCODE+chr(0)+chr( 2)+chr(1)+chr(4)+i2bin(0)+chr(0)+"Î"+i2bin(0)+i2bin(0)

      buffer += E3_STYLE+chr(16)+chr(128)+chr(3)+chr(255)
      buffer += E3_STYLE+chr(17)+chr(128)+chr(4)+chr(255)
      buffer += E3_STYLE+chr( 0)+chr(128)+chr(0)+chr(255)
      buffer += E3_STYLE+chr(18)+chr(128)+chr(5)+chr(255)
      buffer += E2_DEFAULTWIDTH+i2bin(8)
      for x=1 to len(WKS_WIDTHS)
         if WKS_WIDTHS[x,1] < 9999
            buffer += E3_COLINFO+i2bin(WKS_WIDTHS[x,1])+i2bin(WKS_WIDTHS[x,1])+;
                                 i2bin(WKS_WIDTHS[x,2]*256)+i2bin(0)+i2bin(0)+chr(0)
         endif
      next


      buffer += E3_DIMENSIONS+i2bin(0)+i2bin( WKS_ROWS )+i2bin(0)+i2bin(WKS_COLUMNS)+i2bin(0)

******************************************************************************
* Other spreadsheets ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************
   elseif ! ( WKS_VERSION $"L3|L4" )
      // RANGE op code
      buffer += RANGE_OPCODE+i2bin( WKS_ROWS-1)+i2bin( WKS_COLUMNS-1 )

      // Iteration
      buffer += ITERATE_OPCODE+chr( WKS_ITERATION )

      // Calc mode
      buffer += MODE_OPCODE+if(WKS_CALCMODE=="A",chr(255),;
                            if(WKS_CALCMODE=="B",chr(1),chr(0)))
      // Calc order
      buffer += ORDER_OPCODE+if(WKS_CALCORDER=="N",chr(0),;
                             if(WKS_CALCORDER=="C",chr(1),chr(255)))

      //
      zz := ascan( WKS_WIDTHS, { |z|z[1]=9999 } )
      if zz > 0
         zz := WKS_WIDTHS[zz,2]
      else
         zz := 9
      endif
      if WKS_VERSION == "QP"
         buffer += WINDOW1_OPCODE+i2bin(38)+Lcell(WKS_FIRST_CELL,"QP")+;
                   chr(241)+chr(0)+i2bin(zz)+i2bin(8)+i2bin(20)+;
                   Lcell("A1","QP")+i2bin(0)+i2bin(0)+Lcell("A1","QP")+;
                   i2bin(4)+i2bin(1)+i2bin(72)+chr(0)+chr(0)+i2bin(20)+;
                   i2bin(0)+chr(1)+chr(0)
      elseif WKS_FIRST_CELL <> "A1"
         buffer += SPLIT_OPCODE+chr(0)
         buffer += SYNC_OPCODE+chr(255)
         if WKS_VERSION <> "L1"
            buffer += WINDOW1_OPCODE+i2bin(32)+Lcell(WKS_FIRST_CELL,WKS_VERSION)+;
                      chr(113)+chr(0)+i2bin(zz)+i2bin(8)+i2bin(20)+;
                      Lcell("A1","L2")+i2bin(0)+i2bin(0)+Lcell("A1","L2")+;
                      chr(4)+chr(0)+chr(4)+chr(0)
            buffer += HIDVEC1_OPCODE+replicate(chr(0),32)
         else
            buffer += WINDOW1_OPCODE+i2bin(31)+Lcell(WKS_FIRST_CELL,WKS_VERSION)+;
                      chr(113)+chr(0)+i2bin(zz)+i2bin(8)+i2bin(20)+;
                      Lcell("A1","L2")+i2bin(0)+i2bin(0)+Lcell("A1","L2")+;
                      chr(4)+chr(0)+chr(0)
         endif
      else
         if WKS_VERSION <> "L1"
            buffer += WINDOW1_OPCODE+i2bin(32)+Lcell(WKS_FIRST_CELL,WKS_VERSION)+;
                      chr(113)+chr(0)+i2bin(zz)+i2bin(8)+i2bin(20)+;
                      Lcell("A1","L2")+i2bin(0)+i2bin(0)+Lcell("A1","L2")+;
                      chr(4)+chr(0)+chr(4)+chr(0)
            buffer += HIDVEC1_OPCODE+replicate(chr(0),32)
         else
            buffer += WINDOW1_OPCODE+i2bin(31)+Lcell(WKS_FIRST_CELL,WKS_VERSION)+;
                      chr(113)+chr(0)+i2bin(zz)+i2bin(8)+i2bin(20)+;
                      Lcell("A1","L2")+i2bin(0)+i2bin(0)+Lcell("A1","L2")+;
                      chr(4)+chr(0)+chr(0)
         endif

      endif

      // Column widths
      for jj = 1 to len( WKS_WIDTHS )
         nCol  := WKS_WIDTHS[jj][1]
         if nCol <> 9999
            nWide := WKS_WIDTHS[jj][2]
            buffer += WIDTH_OPCODE+i2bin(nCol)+chr(nWide)
         endif
      next
      // Named ranges
      //
      // Need to be sorted, thanks David Kelly for pointing this out
      //
      asort( WKS_NAMED_RANGES,,, { |x,y| x[1] < y[1] } )

      for jj = 1 to len( WKS_NAMED_RANGES )
         cName   := WKS_NAMED_RANGES[jj][1]
         cRange  := WKS_NAMED_RANGES[jj][2]
         if WKS_VERSION == "QP"
            buffer  += NAMED_RANGE_OPCODE+chr(len(trim(cName)))+;
                       padr(trim(cname),15)+cRange
         else
            buffer  += NAMED_RANGE_OPCODE+Lstring(cName,16)+cRange
         endif
      next
      if !empty( WKS_RANGE )
         buffer += PRINT_RANGE_OPCODE + WKS_RANGE
      endif
      // Print format
      buffer += FORMAT_OPCODE+chr(if(WKS_PRINT=="F",0,1))
      // Sort ranges
      if !empty( WKS_SORT_RANGE)
         buffer += SORT_RANGE+WKS_SORT_RANGE
          // Sort key ranges
         if !empty( WKS_SORTKEY1 )
            buffer += SORT_KEY_OP + WKS_SORTKEY1 + chr( WKS_SORTKEY1_DIR )
         endif
         if !empty( WKS_SORTKEY2 )
            buffer += SORT_KEY2_OP + WKS_SORTKEY2 + chr( WKS_SORTKEY2_DIR )
         endif
      endif
      // Protection
      buffer += PROTECT_OPCODE+chr(if(WKS_PROTECTION="P",1,0))
      // Footer
      if WKS_VERSION <> "QP"
         buffer += FOOTER_OPCODE+Lstring( WKS_FOOTER,242 )
      else
         buffer += QP_FOOTER_OPCODE+trim(WKS_FOOTER)
      endif
      // Header
      if WKS_VERSION <> "QP"
         buffer += HEADER_OPCODE+Lstring( WKS_HEADER,242)
      else
         buffer += QP_HEADER_OPCODE+trim(WKS_HEADER)
      endif
      // Setup
      if WKS_VERSION <> "QP"
         buffer += SETUP_OPCODE +Lstring( WKS_SETUP,40)
      else
         buffer += QP_SETUP_OPCODE+trim(WKS_SETUP)
      endif
      // Margins
      if WKS_VERSION <> "QP"
         buffer += MARGINS_OPCODE+i2bin(WKS_LEFT)+i2bin(WKS_RIGHT)+;
                   i2bin(WKS_LPP)+i2bin(WKS_TOP)+i2bin(WKS_BOTTOM)
      else
         buffer += MARGINS_OPCODE+i2bin(WKS_LEFT)+i2bin(WKS_RIGHT)+;
                   i2bin(WKS_TOP)+i2bin(WKS_BOTTOM)+i2bin(WKS_LPP)
      endif
      // Label format
      buffer += ALIGN_OPCODE+if(WKS_ALIGNMENT=="C",chr(94),;
                             if(WKS_ALIGNMENT=="R",chr(34),chr(39)))

      // Print borders
      if !empty( WKS_BORDERS[1] ) .and. !empty( WKS_BORDERS[2] )
         buffer += BORDERS_OPCODE + WKS_BORDERS[1] + WKS_BORDERS[2]
      endif

   else

******************************************************************************
* Lotus 3.x ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************
      // Special handling for Lotus 3.x spreadsheets
      // Calcset

      buffer += WK3_CALCSET+if(WKS_CALCMODE=="A",i2bin(1),i2bin(0))+;
                if(WKS_CALCORDER=="N",i2bin(0),if(WKS_CALCORDER=="C",i2bin(1),i2bin(2)))+;
                i2bin( WKS_ITERATION )

      // Windowset
      buffer += WK3_WINDOWSET+chr(1)+chr(1)+chr(1)+chr(0)+i2bin(0)+;
                chr( 8)+chr(20)+chr(72)+chr( 4)+chr( 4)+chr( 0)+;
                i2bin(0)+replicate(chr(0),6)+i2bin(0)+replicate(chr(0),6)

      // SheetCellPtr
      tmp := Lcell(WKS_FIRST_CELL,WKS_VERSION)
      buffer += WK3_SHEETCELLPTR+substr(tmp,3,1)+chr(0)+i2bin(0)+;
                substr(tmp,1,2)+substr(tmp,4,1)+chr(0)+i2bin(0)+i2bin(0)+;
                chr(0)+chr(0)+i2bin(0)

      // Column widths
      if len(WKS_WIDTHS) > 0
         asort( WKS_WIDTHS )
         x   := val(substr(WKS_WIDTHS[1],1,3))
         tmp :=""
         for jj = 1 to len( WKS_WIDTHS )
            do while jj <= len( WKS_WIDTHS ) .and. val( substr(WKS_WIDTHS[jj],1,3) ) = x
               tmp   += substr(WKS_WIDTHS[jj],4,2)
               if jj < len( WKS_WIDTHS ) .and. val(substr(WKS_WIDTHS[jj+1],1,3) ) = x
                  jj++
               else
                  exit
               endif
            enddo
            if !empty(tmp)
               buffer += WK3_WIDTH+chr(x)+chr(0)+i2bin(0)+tmp
               tmp := ""
               if jj <= len( WKS_WIDTHS)
                  tmp   := substr(WKS_WIDTHS[jj],4,2)
               endif
            endif
            x++
         next
      endif

      // Range names - user ************************************************

      for jj = 1 to len( WKS_NAMED_RANGES )
         cName   := WKS_NAMED_RANGES[jj][1]
         cRange  := WKS_NAMED_RANGES[jj][2]
         buffer  += WK3_USERRANGE+i2bin(0)+Lstring(cName,16)+cRange
      next

      // Range names - system **********************************************

      if !empty( WKS_SORT_RANGE )
         cName   := chr(25)+"DS0"
         buffer  += WK3_SYSTEMRANGE+i2bin(0)+Lstring(cName,16)+WKS_SORT_RANGE
      endif
      if !empty( WKS_SORTKEY1 )
         cName   := chr(25)+chr(1)+"XS"
         buffer  += WK3_SYSTEMRANGE+i2bin(0)+Lstring(cName,16)+WKS_SORTKEY1
      endif
      if !empty( WKS_SORTKEY2 )
         cName   := chr(25)+chr(2)+"XS"
         buffer  += WK3_SYSTEMRANGE+i2bin(0)+Lstring(cName,16)+WKS_SORTKEY2
      endif
      if !empty( WKS_RANGE )
         cName   := chr(25)+chr(1)+"P3"
         buffer  += WK3_SYSTEMRANGE+i2bin(0)+Lstring(cName,16)+WKS_RANGE
      endif
      if !empty( WKS_BORDERS[1] )
         cName   := chr(25)+chr(1)+"RLP"
         buffer  += WK3_SYSTEMRANGE+i2bin(0)+Lstring(cName,16)+WKS_BORDERS[1]
      endif
      if !empty( WKS_BORDERS[2] )
         cName   := chr(25)+chr(1)+"RMP"
         buffer  += WK3_SYSTEMRANGE+i2bin(0)+Lstring(cName,16)+WKS_BORDERS[2]
      endif
      // Sort information **************************************************
      if !empty( WKS_SORTKEY1 ) .or. !empty( WKS_SORTKEY2 )
         buffer += WK3_SORTKEYDIR +chr( if(WKS_SORTKEY1_DIR=0,1,0) )+chr(0)+;
                                   chr( if(WKS_SORTKEY2_DIR=0,1,0) )+chr(0)
      endif

      // Print information *************************************************

      buffer += WK3_PRINTMAIN+chr(1)+Lstring("",16)+Lstring("",16)+;
                Lstring("",16)+chr(0)+chr(if(WKS_PRINT=="F",1,0))+;
                replicate(chr(0),19)+i2bin(WKS_LEFT)+;
                i2bin(WKS_RIGHT)+i2bin(WKS_TOP)+i2bin(WKS_BOTTOM)+i2bin(1)+;
                i2bin(1)+i2bin(WKS_LPP)+i2bin(0)

      if !empty(WKS_HEADER)
         buffer += WK3_PRINTSTRING+chr(1)+chr(0)+Lstring( WKS_HEADER,242 )
      endif
      if !empty(WKS_FOOTER)
         buffer += WK3_PRINTSTRING+chr(1)+chr(1)+Lstring( WKS_FOOTER,242 )
      endif
      if !empty(WKS_SETUP)
         buffer += WK3_PRINTSTRING+chr(1)+chr(2)+Lstring( WKS_SETUP,242 )
      endif

      // Format information
      if !empty(WKS_FORMATS)
         // Load formats into the buffer string
         for jj=1 to len(WKS_FORMATS)
            nSheet := val(substr(WKS_FORMATS[jj],1,2))
            nRow   := val(substr(WKS_FORMATS[jj],3,4))
            x      := len(WKS_FORMATS[jj])
            while substr(WKS_FORMATS[jj],x,1)==chr(192)
               x--
            enddo
            WKS_FORMATS[jj] := substr(WKS_FORMATS[jj],1,x)
            outs := ""
            ct   :=  1
            x    :=  7
            while x <= len(WKS_FORMATS[jj])
                tmp  := substr(WKS_FORMATS[jj],x++,1)
                if tmp == chr(224)
                   tmp := chr(255)
                endif
                outs += Wk3_format(tmp,1)
            enddo
            buffer += WK3_FORMAT+i2bin(len(outs)+4)+chr(nSheet)+chr(0)+;
                      i2bin(nRow-1)+outs
            fwrite(WKS_HANDLE,buffer,len(buffer))
            buffer := ""
         next
      endif

      buffer += WK3_GBLFMT+chr(0)+chr(1)+i2bin(0)+;
             chr(if(WKS_PROTECTION="P",241,113))+chr(0)+chr(0)+chr(0)+;
             if(WKS_ALIGNMENT=="C",i2bin(94),if(WKS_ALIGNMENT=="R",i2bin(34),i2bin(39)))+;
             i2bin(0)
   endif
******************************************************************************
* Write the buffer created to the disk file ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
******************************************************************************
   fwrite(WKS_HANDLE,buffer,len(buffer))
   buffer := ""
   WKS_DATA_WRITTEN := .T.
endif
return NIL
*********************


* Function:    Wk3_format()
* Purpose:     Returns a Lotus 3.x format string
* Syntax:      wk3_format( cFormat,nCtr )
* Arguments:   cFormat      - Cell format
*              nCtr         - Number of times format repeats
* Returns:     cString      - Lotus 3.x format string
* Class:       Internal
*************************************
STATIC function wk3_format(cForm,nCt)
LOCAL temp
if nCt==1
   temp := cForm+chr(0)+chr(0)+chr(0)
else
   temp := cForm+chr(0)+chr(0)+chr(128)+chr(nCt)
endif
return temp
*****************************************************************************
*±±± End of Ldirty ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:    Makeformula()
* Purpose:     Takes the formula and converts it into a spreadsheet format
* Syntax:      makeformula( cFormula,cVersion,nRow,nColumn )
* Arguments:   cFormula
*              cVersion
*              nRow
*              nColumn
* Notes:       Makeformula() is used internally to convert a
*              character string containg an @ formula to its
*              internal format.  It is a STATIC function that
*              is only called by Lput() when Lput() is parsing a
*              character string containing a formula.
* Returns:     cToken      - data in spreadsheet tokenized format
* Class:       Internal
**************************************************
STATIC function makeformula( cFormula,cVersion,nRow,nCol )
*
* Take <cFormula> and returns internal representation of that formula
*
*
* Formula := @xxx(   <param>   )
*
*
*                  ---------------------  Function code
*                 |      ---------------  Lotus 1-2-3 operation code
*                 |      |     ---------  Quattro/Quattro Pro operation code
*                 |      |   |
*                 |      |   |
*                 |      |   |
*                 |      |   |
*                 V      V   V
*
STATIC form_ := { ;
              { "SUM"  , 80, 29,  4,99}, ;         // Varying # of parameters
              { "AVG"  , 81, 32,  5,99}, ;
              { "COUNT", 82, 33,  0,99}, ;
              { "MIN"  , 83, 34,  6,99}, ;
              { "MAX"  , 84, 35,  7,99}, ;
              { "STD"  , 88, 31, 12,99}, ;
              { "VAR"  , 87, 30, 46,99}, ;
              { "CHOSE", 48, 36, 16,99}, ;
              { "NA"   , 31,109, 10, 0}, ;         // No parameter functions
              { "ERR"  , 32,110, 84, 0}, ;
              { "RAND" , 53,111, 63, 0,"S"}, ;
              { "PI"   , 38,112, 19, 0}, ;
              { "FALSE", 51,113, 35, 0}, ;
              { "TRUE" , 52,114, 34, 0}, ;
              { "NOW"  , 55,115, 74, 0,"S"}, ;
              { "TODAY", 55,118,221, 0,"S"}, ;
              { "ABS"  , 33,124, 24, 1}, ;         // One parameter functions
              { "SQRT" , 35,126, 20, 1}, ;
              { "LOG"  , 36,127,109, 1}, ;
              { "INT"  , 34,125, 25, 1}, ;
              { "LN"   , 37,128, 22, 1}, ;
              { "SIN"  , 39,129, 15, 1}, ;
              { "COS"  , 40,130, 16, 1}, ;
              { "TAN"  , 41,131, 17, 1}, ;
              { "ATAN" , 43,132, 18, 1}, ;
              { "ASIN" , 44,133, 98, 1}, ;
              { "ACOS" , 45,134, 99, 1}, ;
              { "EXP"  , 46,135, 21, 1}, ;
              { "ISNA" , 49,136,  2, 1}, ;
              { "ISERR", 50,137,  3, 1}, ;
              { "DAY"  , 60,138, 67, 1}, ;
              { "MONTH", 61,139, 68, 1}, ;
              { "YEAR" , 62,140, 69, 1}, ;
              { "ISNO" , 68,141,128, 1}, ;
              { "ISLAB", 69,142,127, 1}, ;
              { "COLS" , 99,145, 77, 1}, ;
              { "ROWS" ,100,146, 76, 1}, ;
              { "CHAR" , 74,147,111, 1}, ;
              { "CODE" , 75,148,121, 1}, ;
              { "LEN"  , 70,149,211, 1}, ;
              { "LOWER",103,150,112, 1}, ;
              { "PROPR",107,152,114, 1}, ;
              { "TRIM" ,109,154,118, 1}, ;
              { "UPPER",102,155,113, 1}, ;
              { "VALUE", 71,156, 33, 1}, ;
              { "DVAL" , 77,157,140, 1}, ;
              { "TVAL" , 78,158,141, 1}, ;
              { "HOURS", 65,159, 71, 1}, ;
              { "MINS" , 66,160, 72, 1}, ;
              { "SECS" , 67,161, 73, 1}, ;
              { "DEG"  ,  0,163, -1, 1}, ;
              { "RAD"  ,  0,164, -1, 1}, ;
              { "HEX2N",  0,165, -1, 1}, ;
              { "NUM2H",  0,166, -1, 1}, ;
              { "FILE" ,  0,167, -1, 1}, ;
              { "CLEAN",  0,168, -1, 1}, ;
              { "MOD"  , 47,187, -1, 2}, ;         // Two parameter functions
              { "ATAN2", 42,188, 18, 2}, ;
              { "ROUND", 63,189, 27, 2}, ;
              { "NPV"  , 86,190, 11, 2} }


LOCAL retval:="",x,lookfor,params:=NIL,needs:=0,nFormcode,cPar,vctr:=0
LOCAL aQPro := {"","",""}
LOCAL nOffset := 2
if len(form_) < 70
   Aadd( form_,{  "IRR"  , 89,191, 62,2} )
   Aadd( form_,{  "CELL" ,108,192, 94,2} )
   Aadd( form_,{  "EXACT",113,193,117,2} )
   Aadd( form_,{  "LEFT" ,104,194,115,2} )
   Aadd( form_,{  "RIGHT",105,196,116,2} )
   Aadd( form_,{  "STRNG", 72,197, -1,2} )
   Aadd( form_,{  "DATE" , 54,206, 65,3} )         // Three parameter functions
   Aadd( form_,{  "PMT"  , 56,207, 59,3} )
   Aadd( form_,{  "PV"   , 57,208, 56,3} )
   Aadd( form_,{  "FV"   , 58,209, 57,3} )
   Aadd( form_,{  "IF"   , 59, 59,  0,3} )
   Aadd( form_,{  "VLOOK", 85,211,102,3} )
   Aadd( form_,{  "HLOOK", 90,212,101,3} )
   Aadd( form_,{  "INDEX", 98,213, -1,3} )
   Aadd( form_,{  "FIND" , 76,214,124,3} )
   Aadd( form_,{  "MID"  , 73,215, -1,3} )
   Aadd( form_,{  "TIME" , 64,216, 66,3} )
   Aadd( form_,{  "CTERM",118,217, -1,3} )
   Aadd( form_,{  "RATE" ,116,218, 60,3} )
   Aadd( form_,{  "SLN"  ,119,219,142,3} )
   Aadd( form_,{  "DSTD" , 97,220, 45,3} )
   Aadd( form_,{  "DVAR" , 96,221, 47,3} )
   Aadd( form_,{  "DSUM" , 91,222, 41,3} )
   Aadd( form_,{  "DAVG" , 92,223, 43,3} )
   Aadd( form_,{  "DCNT" , 93,224, 40,3} )
   Aadd( form_,{  "DMIN" , 94,225, 43,3} )
   Aadd( form_,{  "DMAX" , 95,226, 44,3} )
   Aadd( form_,{  "TERM" ,117,227, -1,3} )
   Aadd( form_,{  "REPL" ,106,232,119,4} )         // Four parameter functions
   Aadd( form_,{  "DDB"  ,121,233,144,4} )
   Aadd( form_,{  "SYD"  ,120,234,143,4} )
endif

cVersion :=if(cVersion=NIL,"L2",cVersion)
if cVersion == "QU"
   nOffset := 3
elseif substr(cVersion,1,1) == "E"
   nOffset := 4
endif


if cFormula <> NIL .and. valtype(cFormula)=="C"
   // Remove leading @ from formula string
   cFormula := if(substr(cFormula,1,1)=="@",substr(cFormula,2),cFormula)
   cFormula := if(substr(cFormula,1,1)=="=" .and. substr(cVersion,1,1)=="E",substr(cFormula,2),cFormula)
   x := at("(",cFormula)
   if x > 0
      cFormula := substr(cFormula,1,len(cFormula)-1)   // Remove final )
      lookfor := substr(cFormula,1,x-1)                // Extract formula
      params  := substr(cFormula,x+1)                  // and parameters
   else
      lookfor := cFormula                              // No parameters
   endif
   x := ascan( form_,{ |y| y[1]==upper(lookfor)} )     // Look for match
   if x > 0
      // Quattro or Lotus formula number ?
      nFormcode := form_[x,nOffset]
      needs     := form_[x,5]                          // Number of parameters
      if needs == 0                                    // No parameters needed
         if cVersion <> "QP"
            if substr(cVersion,1,1) == "E"
               retval   := chr(65)+if(cVersion=="E4",i2bin(nFormcode),;
                                                     chr(nFormcode) )
            else
               retval   := chr(nFormcode)
            endif
         else
            aQpro[QP_FORMULAS] := chr(nFormcode)
         endif
      elseif params <> NIL                             // Parameters found
         while !empty(params)
            x := at(",",params)
            vctr++
            if x > 0
               cPar  := substr(params,1,x-1)
               params:= substr(params,x+1)
            else
               cPar  := params
               params:=""
            endif
            if val(cPar)==0 .or. (nFormcode==59 .and. vctr=1)
               if cVersion <> "QP"
                  retval += To_123( cPar,cVersion,nRow,nCol,vctr=1 .and. nFormcode=59 )
               else
                  if ".." $ cPar
                     aQpro[QP_FORMULAS] += To_123("RANGEMARK","QP",nRow,nCol)
                     aQpro[QP_RANGES] += To_123( cPar,cVersion,nRow,nCol )
                  elseif len(cPar) > 1
                     aQpro[QP_FORMULAS] += To_123("CELL_ADDR","QP",nRow,nCol)
                     aQpro[QP_CELLS]  += To_123( cPar,cVersion,nRow,nCol )
                  else
                     aQpro[QP_FORMULAS]  += To_123( cPar,cVersion,nRow,nCol,vctr=1 .and. nFormcode=59 )
                  endif
               endif
            else
               if cVersion <> "QP"
                  retval += To_123( val(cPar),cVersion,nRow,nCol )
               else
                  aQpro[QP_FORMULAS] += To_123( val(cPar),cVersion,nRow,nCol )
               endif
            endif
         enddo
         if !empty(retval) .or. cVersion == "QP"
            if cVersion <> "QP"
               if substr(cVersion,1,1)=="E"
                  if cVersion <> "E4"
                     retval += if(needs==99,chr(66)+chr(vCtr),chr(65))+chr(nFormcode)
                  else
                     retval += if(needs==99,chr(66)+chr(vCtr),chr(65))+i2bin(nFormcode)
                  endif
               else
                  retval += chr(nFormcode)+if(needs==99,chr(vCtr),"")
               endif
            else
               aQpro[QP_FORMULAS] += chr(nFormcode)+if(needs==99,chr(vCtr),"")
            endif
         endif
      endif
   endif
endif
return if(cVersion<>"QP",retval,aQpro)
***************************


* Function:    To_123()
* Purpose:     Takes the data and converts it into a spreadsheet token
* Syntax:      To_123( xData,cVersion,nRow,nColumn )
* Arguments:   xData
*              cVersion
*              nRow
*              nColumn
* Notes:       This function is used by Lput() to convert data of
*              any type to the internal format used by
*              spreadsheets for storage.  It should never be
*              called directly.
*
*              Columns
*              -------
*                 2        Lotus 2.x
*                 3        Quattro
*                 4        Excel
*                 5        Lotus 3.x
*
* Returns:     cToken      - data in spreadsheet tokenized format
* Class:       Internal
**************************************************
STATIC function to_123(xData,cVersion,nRow,nCol,lIsExp)
STATIC markers_:={ ;
                  {"ENDMARK"  , 3, 25, 0, 3 },;
                  {"RANGEMARK", 2, 17,37, 2 },;
                  {"INTEGER"  , 5, 18,30, 5 },;
                  {"REAL"     , 0, 19,31, 0 },;
                  {"CELL_ADDR", 1, 20,68, 1 },;
                  {"LABEL"    , 6, 21, 0, 6 },;
                  {"+"        , 9,  3, 3,15 },;
                  {"-"        ,10,  4, 4,16 },;
                  {"!"        , 8,  4, 4,14 },;
                  {"*"        ,11,  5, 5,17 },;
                  {"/"        ,12,  6, 6,18 },;
                  {"<="       ,16, 16,10,22 },;
                  {">="       ,17, 17,12,23 },;
                  {"<>"       ,15, 15,15,21 },;
                  {"^"        ,13,  8, 7,19 },;
                  {"="        ,14, 14,11,20 },;
                  {"<"        ,18, 18, 9,24 },;
                  {">"        ,19, 19,13,25 },;
                  {")"        ,04, 23,21,04 }  }

LOCAL retval:="",nElt,x:=0,tmp
LOCAL r1,c1,r2,c2,nTest,tmp1,tmp2,s1:=0,s2:=0
LOCAL nSize := 1,arr_ := { xData }
LOCAL xx,zz,p_
LOCAL k,temp_out,y


cVersion := if(cVersion==NIL,"L2",substr(cVersion,1,2))
nElt     := if(cVersion=="QU",3,;
                   if(substr(cVersion,1,1)=="E",4,if(cVersion="L2",2,5)))
lisExp   := if(lIsExp=NIL,.F.,lIsExp)
if lIsExp
   for zz := 12 to 19
      xx := at( markers_[zz,1],xData )
      if xx > 0
         asize(arr_,3)
         arr_[1] := substr(xData,1,xx-1)
         arr_[2] := substr(xData,xx+len(markers_[zz,1]))
         arr_[3] := markers_[zz,1]
         if val(arr_[1]) > 0
            arr_[1] := val(arr_[1])
         endif
         if val(arr_[2]) > 0
            arr_[2] := val(arr_[2])
         endif
         nSize   := 3
         zz      := 99
      endif
   next
endif


for xx := 1 to nSize
   xData := arr_[xx]
   if valtype(xData)="C" .and. replicate("0",len(xData))=xData
      xData := 0
   endif
if valtype(xData)=="C"
   do case
   case len(xData) <= 1 .and. ;
         (x:= ascan(markers_, { |y| y[1] == xData },7,13))>0
        if cVersion $ "L3|L4"
           retval += chr( markers_[x,nElt] )
        else
           retval += chr( markers_[x,nElt] )
        endif

   case len(xData) == 2 .and. ;
         (x:= ascan(markers_, { |y| y[1] == xData },12,3))>0
        if cVersion $ "L3|L4"
           retval += chr( markers_[x,nElt] )
        else
           retval += chr( markers_[x,nElt] )
        endif

   case ".." $ xData .or. (":" $xData .and. substr(cVersion,1,1)=="E")
      tmp := Lmakerange(xData,cVersion)   // Convert range to internal format
      if cVersion $ "L3|L4"
         r1  := bin2w(substr(tmp,1,2))
         s1  := asc(substr(tmp,3,1))
         c1  := asc(substr(tmp,4,1))
         r2  := bin2w(substr(tmp,5,2))
         s2  := asc(substr(tmp,7,1))
         c2  := asc(substr(tmp,8,1))
      else
         r1  := bin2w(substr(tmp,1,2))
         c1  := bin2w(substr(tmp,3,2))
         r2  := bin2w(substr(tmp,5,2))
         c2  := bin2w(substr(tmp,7,2))
      endif
      if cVersion $ "L3|L4"
         // Lotus 3 range markes
         if "$" $ xData
            retval += chr( markers_[2,nElt] )+chr(0)+;
                      i2bin(r1)+chr(s1)+chr(c1)+i2bin(r2)+chr(s2)+chr(c2)
         else
            retval += chr( markers_[2,nElt] )+chr(255)+;
                      i2bin(r1)+chr(s1)+chr(c1)+i2bin(r2)+chr(s2)+chr(c2)
         endif
      elseif cVersion == "QP"
         if "$" $ xData
            retval += i2bin(r1)+i2bin(c1)+i2bin(r2)+i2bin(c2)
         else
            retval += i2bin(Lrelative(r1,nRow))+i2bin(Lrelative(c1,nCol))+;
                      i2bin(Lrelative(r2,nRow))+i2bin(Lrelative(c2,nCol))
         endif
      elseif substr(cVersion,1,1)=="E"
         if "$" $ xData
            retval += chr( markers_[2,nElt] )+i2bin(r1)+i2bin(r2)+chr(c1)+chr(c2)
         else
            tmp1 := i2bin(r1)
            tmp2 := i2bin(r2)
            bit(tmp1,10,.T.)
            bit(tmp1, 9,.T.)
            bit(tmp2,10,.T.)
            bit(tmp2, 9,.T.)
            retval := chr( markers_[2,nElt] )+tmp1+tmp2+chr(c1)+chr(c2)
         endif
      else
         if "$" $ xData
            retval += chr( markers_[2,nElt] )+;
                      i2bin(r1)+i2bin(c1)+i2bin(r2)+i2bin(c2)
         else
            retval += chr( markers_[2,nElt] )+;
                      i2bin(Lrelative(r1,nRow))+i2bin(Lrelative(c1,nCol))+;
                      i2bin(Lrelative(r2,nRow))+i2bin(Lrelative(c2,nCol))
         endif
      endif
   otherwise
      x:= ascan(markers_, { |y| upper(y[1]) == upper(xData) } )
      do case
      case x > 0
         retval += chr( markers_[x,nElt] )
      case left(xData,1)$"@="      // Some formula
         retval += makeformula( xData,cVersion,nRow,nCol )
      case left(xData,1)$"+-"       // Some expression
         p_       := Parse(xData)
         temp_out := ""
         if !empty(p_)
            if cVersion <> "QP" .and. left(cVersion,1)<>"E"
               for k=len(p_) to 1 step -1
                  if substr(p_[k],1,1)=="@"
                     temp_out += makeformula( p_[k],cVersion,nRow,nCol )
                  elseif val(p_[k]) == 0
                     y := len(p_[k])
                     if p_[k] <> replicate("0",y)
                        temp_out += to_123( p_[k],cVersion,nRow,nCol )
                     else
                        temp_out += to_123( val(p_[k]),cVersion,nRow,nCol )
                     endif
                  else
                     temp_out += to_123( val(p_[k]),cVersion,nRow,nCol )
                  endif
               next
            elseif left(cVersion,1)=="E"
               for k=len(p_) to 1 step -1
                  if substr(p_[k],1,1)=="="
                     temp_out += makeformula( p_[k],cVersion,nRow,nCol )
                  elseif val(p_[k]) == 0
                     y := len(p_[k])
                     if p_[k] <> replicate("0",y)
                        temp_out += to_123( p_[k],cVersion,nRow,nCol )
                     else
                        temp_out += to_123( val(p_[k]),cVersion,nRow,nCol )
                     endif
                  else
                     temp_out += to_123( val(p_[k]),cVersion,nRow,nCol )
                  endif
               next
            endif
         endif
         retval += temp_out
      otherwise                    // Assumed to be a cell address
         tmp  := Lcell(xData,cVersion)
         r1   := bin2w(substr(tmp,1,2))
         if cVersion $ "L3|L4"
            c1   := asc(substr(tmp,4,1))
            s1   := asc(substr(tmp,3,1))
         else
            c1   := bin2w(substr(tmp,3,2))
         endif
         if cVersion $ "L3|L4"
            if "$" $ xData
               retval += chr( markers_[5,nElt] )+chr(0)+;
                         i2bin(r1)+chr(s1)+chr(c1)
            else
               retval += chr( markers_[5,nElt] )+chr(07)+;
                         i2bin(r1)+chr(s1)+chr(c1)
            endif
         elseif substr(cVersion,1,1) == "E"
            if "$" $ xData
               retval += chr( markers_[5,nElt] )+i2bin(r1)+ chr(c1)
            else
               tmp1   := i2bin(r1)
               bit(tmp1,10,.T.)
               bit(tmp1, 9,.T.)
               retval := chr( markers_[5,nElt] )+tmp1+chr(c1)
            endif
         elseif cVersion <> "QP"
            if "$" $ xData
               retval += chr( markers_[5,nElt] )+;
                         i2bin(r1)+i2bin(c1)
            else
               retval += chr( markers_[5,nElt] )+;
                         i2bin(Lrelative(r1,nRow))+;
                         i2bin(Lrelative(c1,nCol))
            endif
         else
            if "$" $ xData
               retval += i2bin(r1)+i2bin(c1)
            else
               retval += i2bin(Lrelative(r1,nRow))+;
                         i2bin(Lrelative(c1,nCol))
            endif
         endif
      endcase
   endcase
elseif valtype(xData)=="N"
   tmp := str(xData)
   x   := if(at(".",tmp)==0,0,len(tmp)-at(".",tmp))
   if ! ( cVersion$"L3|L4" )
      if x > 0
         retval += chr( markers_[4,nElt] ) + ieereal( xData )
      else
         retval += chr( markers_[3,nElt] ) + i2bin( xData )
      endif
   else
      if xData >= -16384 .and. xData <= 16383 .and. x==0
         retval += chr( markers_[3,nElt] ) + shortnum( xData )
      else
         retval += chr( markers_[4,nElt] ) + treal( xData )
      endif
   endif
endif
next
return retval
*************************************



* Function:    Parse()
* Purpose:     Takes an expression and returns a RPN stack in an array
* Syntax:      Parse( cExpression )
* Arguments:   cExpression - Formula to convert to RPN
* Returns:     aPolish    - Reverse polish notation array of tokens
* Notes:       Parse() is used to break a formula string down
*              into an array of reverse polish notation.  It is
*              used internally by Lput() and probably should not
*              be called directly by your application.
* Class:       Internal
**************************************************
STATIC function parse(cExp)           // Thanks Mike Taylor
#define EOT ''         // end-of-expression symbol
#define NEGATE '!'      // unary '-' negation stack symbol
#define IS_NUM(x)      (x $ "0123456789")
#define IS_OP(x)       (x $ "()+-*/^" + EOT)
#define OPERAND  0      // parsing engine expects an operand mode
#define OPERATOR 1      // parsing enging expects an operator mode
#define ERR_PARENS 1    // mis-matched parens
#define ERR_SYNTAX 2    // next token did not match state

LOCAL cToken      := ""
LOCAL cWorkingExp := StrTran(cExp, " ", "") + EOT   // remove ALL spaces
LOCAL aOperators  := {}
LOCAL aRevPolish  := {}
LOCAL nState      := OPERAND
LOCAL lError      := .f.,lECode:=0,c:="",i:=0

while !Empty(cWorkingExp)
   cToken := GetToken(@cWorkingExp, nState)
   if ( !Empty(cToken) )
      do case
      case cToken == EOT                    // last item in expression
         while .T.
            c := Pop(aOperators)            // clear out remaining operators
            if ( c != NIL )
               Push(aRevPolish, c)
            else
               exit
            endif
         enddo
      case cToken == "-" .and. nState == OPERAND      // unary -
         Push(aOperators, NEGATE)
         nState := OPERAND
      case cToken == "+" .and. nState == OPERAND      // unary +
         nState := OPERAND                            //  ignore it
      case cToken == "("                              // left paren
         Push(aOperators, cToken)                     //  push
         nState := OPERAND
      case cToken == ")"                              // if token == right paren
         while .t.                                    //  pop operators until a
            c := Pop(aOperators)                      //  left paren is found
            do case
            case c == NIL
               lError := .t.
               lECode := ERR_PARENS
               exit
            case c == "("
               exit
            otherwise
               Push(aRevPolish, c)
            endcase
         enddo
         Push(aRevPolish, cToken)        // push the right paren
         nState := OPERATOR
      otherwise
         if ( nState == OPERAND )
            Push(aRevPolish, cToken)
            nState := OPERATOR
         else
            OpCopy(aOperators, aRevPolish, cToken)
            nState := OPERAND
         endif
      endcase
   endif
enddo
return (aRevPolish)
*******************


* Function:    Opcopy()
* Purpose:     Place token on the appropiate stack
* Syntax:      Opcopy( aOperators, aPolish, cToken )
* Arguments:   aOperators - Operations stack
*              aPolish    - Data stack
*              cToken     - Token being processed
* Returns:     NIL
* Notes:       Takes cToken and if determines which stack to push it onto.
*              If the operators stack has a token that is of lower precedence,
*              then they are switched otherwise it is pushed onto the stack.
* Class:       Internal
**************************************************
STATIC function opcopy(aOperators, aRevPolish, cToken)
LOCAL aTable := { "+", "-", "*", "/", "^" }
LOCAL aPVal  := {   1,   1,   2,   2,   3 }
LOCAL nTokenPrecedence := 0         // current token's precedence value
LOCAL nTopPrecedence   := 0         // top operators stack's precedence
LOCAL cTemp            := ""

    // are there any operators on the stack?

if ( Len(aOperators) == 0 )
    Push(aOperators, cToken)        // no, push current token
else
    // scan operator table for current token
    if ( ( nTokenPrecedence := AScan(aTable, cToken) ) != 0 )
       // get current tokens precedence value
       nTokenPrecedence := aPVal[nTokenPrecedence]
       nTopPrecedence   := AScan(aTable, Top(aOperators))
       nTopPrecedence   := if(nTopPrecedence != 0, aPVal[nTopPrecedence], 0)
       // check to see if current token's precedence value is
       //  less than or equal to the top token in the stack
       if ( nTokenPrecedence <= nTopPrecedence )
          // if it is, then switch the top token with the current token
          Push(aRevPolish, Pop(aOperators))
          Push(aOperators, cToken)
       else
          Push(aOperators, cToken)
       endif
    else
       Push(aOperators, cToken)
    endif
endif
return NIL
*********************************


* Function:    Gettoken()
* Purpose:     Extracts from the expression, the next available token
* Syntax:      Gettoken( cExpression,nState )
* Arguments:   cExpression - Expression being processed, must be passed
*                            by reference
*              nState      - 0 = Operands    1 = Operators
* Notes:       Gettoken() is used internally by the PARSE()
*              function.  It is used to extract tokens from the
*              formula string to place on the stack.  It is a
*              STATIC function in the WRITEWKS.PRG and should
*              never be called directly by your application.
* Returns:     cToken      - Extracted token from expression
* Class:       Internal
**************************************
STATIC function gettoken(cExp, nState)
LOCAL cCurrChar := "",cToken:="",lDone:=.F.,p:=0

while !(lDone)
    cCurrChar := SubStr(cExp, 1, 1)
    if ( nState == OPERATOR )
        if ( IS_OP(cCurrChar) )
            cToken += cCurrChar
        endif
        lDone := .t.
        cExp  := SubStr(cExp, 2)
    else
        if ( cCurrChar == "@" ) .or. ( cCurrChar == "=" )
            p := At(")", cExp)
            if ( p > 0 )
                cToken += SubStr(cExp, 1, p)
                cExp := SubStr(cExp, p + 1)
                lDone := .t.
            else
                cToken += cCurrChar
            endif
        else
            if ( cCurrChar $ "()-+" + EOT )
                if ( Empty(cToken) )
                    cToken += cCurrChar
                    cExp   := SubStr(cExp, 2)
                endif
                lDone := .t.
            else
                if ( !IS_OP(cCurrChar) )
                    cToken += cCurrChar
                    cExp   := SubStr(cExp, 2)
                else
                    lDone := .t.
                endif
            endif
        endif
    endif
enddo
return (cToken)
**********************


* Function:    Push()
* Purpose:     Pushes the requested element onto the requested stack
* Syntax:      Push( aStack,xElement )
* Arguments:   aStack   - the stack being processed
*              xElement - the data being added to the stack
* Notes:       Pushes the element onto either the operator or
*              operand stack.  This function is a STATIC function
*              used only by the Parse() function.  You should
*              never need to call Push() directly.
* Returns:     NIL
* Class:       Internal
**************************************************

STATIC function push(aStack, xElement)
if ( xElement != NIL )

    if (( Len(aStack) == 0 ) .or. ( aStack[Len(aStack)] != NIL ))
        ASize(aStack, Len(aStack) + 1)
    endif
    AIns(aStack, 1)
    aStack[1] := xElement

endif
return NIL
*****************************************************************************
*±±± End of Push ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:    Pop()
* Purpose:     Pops the top element from the stack
* Syntax:      Pop( aStack )
* Arguments:   aStack - the stack being processed
* Returns:     xData  - First item on the stack, NIL if stack is empty
* Notes:       The Pop() function removes the top element from
*              the stack and adjusts the stack size.  If there
*              are no elements in the stack, the function returns
*              NIL.  It is a STATIC function used by the Parse()
*              function and should never need to be called
*              directly by your application.
* Class:       Internal
**************************************************

STATIC function pop(aStack)
LOCAL xElement := NIL
if ( Len(aStack) > 0 )
    xElement := aStack[1]
    ADel(aStack, 1)
    ASize(aStack, Len(aStack) - 1)
endif
return (xElement)
*****************************************************************************
*±±± End of Pop ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:    Top()
* Purpose:     Returns the top element from the stack without removing it
* Syntax:      Top( aStack )
* Arguments:   aStack - the stack being processed
* Returns:     xData  - First item on the stack, NIL if stack is empty
* Notes:       Top() reads the operator stack and returns the
*              value of the first element, without removing it.
*              It is a STATIC function used by Lput() to write
*              formulas to the disk.  You should never need to
*              call Lput() directly.
* Class:       Internal
**************************************************

STATIC function top(aStack)
return (if(Len(aStack) == 0, NIL, aStack[1]))
*****************************************************************************
*±±± End of Top ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
