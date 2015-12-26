*  Program.....: Miscwks.prg
*  Authors.....: Joseph D. Booth
*  Copyright...: 1998 Joseph D. Booth, All rights reserved.
*  Application.: CLIPWKS++
*  Version.....: 1.1
*  Compile.....: xpp miscwks /l /m /n /w
*  Functions...: Lalign(aWks,cAlignment)
*                Lcalcmode(aWks,cMethod)
*                Lcalcorder(aWks,cMethod)
*                Lfirstcell(aWks,cCell)
*                Lformat(aWks,cRange,cMethod)
*                Literate(aWks,nCount)
*                Lname(aWks,cName,cRange)
*                Lprotect(aWks,cMethod)
*                Lrange(aWks,nRows,nColumns)
*
***************************************************************************
#include "CLIPWKS.CH"


* Function:   Lalign()
* Purpose:    Set label alignment, (L)eft,(c)enter,(r)ight
* Syntax:     Lalign( aSpreadsheet,cAlignment )
* Arguments:  aSpreadsheet - Spreadsheet array handle
*             cAlignment   - Alignment method
* Notes:      Text in a cell may be justified left, right, or
*             centered.  The Lalign() function is used to set
*             the global alignment to used for text being
*             written to cells.
* Returns:    nStatus    - 0  All ok
*                        - 6  Invalid parameters
*****************************
function lalign(aWks,cMethod)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT cMethod TO "L"

if valtype(aWks)=="A" .and. !empty(aWks) .and. valtype(cMethod)=="C"

   WKS_ALIGNMENT := substr(cMethod,1,1)
   retval        := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lalign ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lcalcmode()
* Purpose:    Set calculation mode
* Syntax:     Lcalcmode( aSpreadsheet,cMode )
* Arguments:  aSpreadsheet - Spreadsheet handle array
*             cMode        - Calculation mode
* Notes:      The calculation mode determines how often a
*             spreadsheet's formula are recomputed.  In you
*             choice (A)utomatic, the spreadsheet will update
*             all formulas as soon as a cell value affecting the
*             formula is changed.  If you choice (M)anual, you
*             need to press a key, (usually F9), to recalculate
*             the spreadsheet.  (B)ackground allows the
*             spreadsheet to be recalculated while it is waiting
*             for keystrokes from the end-user.  If you create a
*             spreadsheet using CLIPWKS and set the calcmode to
*             (M)anual, be sure to have your end-user press F9
*             as soon they read in the spreadsheet.  CLIPWKS
*             does not calculate the values from the formulas it
*             writes to the spreadsheet.
* Returns:    nStatus
* See Also:   Lcalcorder()
********************************************
function lcalcmode(aWks,cMethod)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT cMethod TO "A"

if valtype(aWks)=="A" .and. !empty(aWks) .and. valtype(cMethod)=="C"

   WKS_CALCMODE := substr(cMethod,1,1)
   retval       := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lcalcmode ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lcalcorder()
* Purpose:    Set calculation order
* Syntax:     Lcalcorder( aSpreadsheet,cOrder )
* Arguments:  aSpreadsheet - Array of spreadsheet parameters
*             cOrder       - Calculation order,(R)owwise,(C)olumn,(N)atural
* Notes:      Calculation order determines when dependent cells
*             are recalculated in order to resolve any formulas.
*             Natural calculation order means that before a
*             formula is computed, each cell it refers to is
*             calculated first to ensure accurary.  In column
*             recalculation order, calculation starts in the
*             first column and proceeds down.  It ignores
*             formulas in other columns.  Rowwise works
*             similarly, calculating rows at a time.  The
*             preferred method is (N)atural.
* Returns:    nStatus
* See Also:   Lcalcmode()
*********************************
function lcalcorder(aWks,cMethod)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT cMethod TO "N"                 // Default to (N)atural

if valtype(aWks)=="A" .and. !empty(aWks) .and. valtype(cMethod)=="C"

   WKS_CALCORDER := substr(cMethod,1,1)
   retval        := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lcalcorder ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Lfirstcell()
* Purpose:    Set first cell address
* Syntax:     Lfirstcell( aSpreadsheet,cAddress )
* Arguments:  aSpreadsheet - Spreadsheet array handle
*             cAddress     - First cell address
* Notes:      Setting the first cell allows you to have an automatic
*             macro startup when the user invokes your spreadsheet.
* Returns:    nStatus    - 0  All ok
*                        - 6  Invalid parameters
*****************************
function lfirstcell(aWks,cCell)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT cCell TO lc(1,1)

if valtype(aWks)=="A" .and. !empty(aWks) .and. ;
   valtype(cCell)=="C" .and. .not. empty(cCell)

   WKS_FIRST_CELL  := cCell
   retval          := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lfirstcell ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Lformat()
* Purpose:    Set the global date format
* Syntax:     Lformat( aSpreadsheet, cRange,cFormat )
* Arguments:  aSpreadsheet - Spreadsheet array handle
*             cRange       - Range of cells
*             cFormat      - Format to set range to
*
* Returns:    nStatus
******************************
function lformat(aWks,cRange,cFormat)

LOCAL retval   := ERR_INVALID_PARAMETERS
LOCAL r1, r2, c1, c2, tmp, jj, x, tt, nFormat, s1, s2
LOCAL lProtect := (WKS_PROTECTION == "P")

if valtype(aWks) == "A" .and. !empty(aWks)

   if WKS_VERSION $ "L3|L4"                   // Lotus 3.x only

      if substr(cFormat,1,1)$"+-"        // If protection was specified
         lProtect := substr(cFormat,1,1)=="+"
         cFormat  := substr(cFormat,2)
      endif
    
      nFormat := lcFormat(cFormat,aWks,lProtect)   // Break cRange into rows/column
      tmp     := Lmakerange( cRange,WKS_VERSION )

      if !empty(tmp)
         r1  := bin2w(substr(tmp,1,2))+1
         c1  := asc(substr(tmp,4,1))
         s1  := asc(substr(tmp,3,1))
         r2  := bin2w(substr(tmp,5,2))+1
         c2  := asc(substr(tmp,8,1))
         s2  := asc(substr(tmp,7,1))
         for jj=r1 to r2
            x   := ascan( WKS_FORMATS,{ |z|val(substr(z,1,6))=s1*10000+jj} )
            if x = 0
               Aadd( WKS_FORMATS,str(s1*10000+jj,6)+replicate(chr(224),256) )
               x := len(WKS_FORMATS )
            endif
            for tt=c1 to c2
               WKS_FORMATS[x] := stuff(WKS_FORMATS[x],7+tt,1,chr(nFormat))
            next
         next
         retval := ALL_OK
      else
         retval := ERR_INVALID_RANGE
      endif
   endif
endif
return retval
*****************************************************************************
*±±± End of Lformat ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

* Function:   Literate()
* Purpose:    Set the formula recalculation iteration flag
* Syntax:     Literate( aSpreadsheet, nCount )
* Arguments:  aSpreadsheet - Array of spreadsheet parameters
*             nCount       - Iteration count
* Notes:      On some complex formulas, multiple iterations are
*             needed to get an acceptable degree of accuracy.
*             You may specify the number of iterations to
*             perform on formula calculation using the
*             Literate() function.  The default value is one
*             iteration.
* Returns:    nStatus
* See Also:   Lcalcmode() Lcalcorder()
******************************
function literate(aWks,nCount)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT nCount TO 1

if valtype(aWks)=="A" .and. !empty(aWks) .and. valtype(nCount)=="N"

   WKS_ITERATION := nCount
   retval        := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Literate ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lname()
* Purpose:    Write a named range into the spreadsheet
* Syntax:     Lname( aSpreadsheet, cName, cRange )
* Arguments:  aSpreadsheet - Array of spreadsheet parameters
*             cName        - Name for specified range
*             cRange       - Range of cells
* Notes:      A spreadsheet allows a range of cells to have a
*             name assigned to them.  This assists in
*             documenting function calls.  The Lname() function
*             writes an range name out to the spreadsheet.  If a
*             formula references the range, it will display the
*             range name when the formula is view.
* Returns:    nStatus
*********************************
function lname(aWks,cName,cRange)

LOCAL retval := ERR_INVALID_PARAMETERS, _rngexp

if valtype(aWks) =="A" .and. !empty(aWks) .and. ;
   valtype(cName) =="C" .and. valtype(cRange) == "C"

   _rngexp := Lmakerange(cRange,WKS_VERSION)

   if !empty(_rngexp) .and. !empty(cName)
      Aadd( WKS_NAMED_RANGES, { cName,_rngexp } )
      retval := ALL_OK
   elseif empty(_rngexp)
      retval := ERR_INVALID_RANGE
   else
      retval := ERR_MISSING_PARAMETER
   endif

endif
return retval
*****************************************************************************
*±±± End of Lname ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lprotect()
* Purpose:    Set the global protection flag in the spreadsheet
* Syntax:     Lprotect( aSpreadsheet,cSetting | lSetting )
* Arguments:  aSpreadsheet - Array of spreadsheet parameters
*             cSetting     - ON  | P  - Enable global protection
*                            OFF | U  - Disable global protection
*             lSetting     - TRUE - Enable  FALSE - disable
* Notes:      Lprotect is used to enable or disable global
*             protection.  When protection is enabled, protected
*             cells may not be modified.
* Returns:    nStatus
********************************************
function lprotect(aWks,cMethod)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT cMethod TO "P"

if valtype(aWks)=="A" .and. !empty(aWks) .and. valtype(cMethod) $ "CL"

   cMethod        := if(valtype(cMethod)=="L",if(cMethod,"P","U"),cMethod)
   WKS_PROTECTION := substr(cMethod,1,1)
   retval         := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lprotect ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lrange()
* Purpose:    Write number of rows/columns into spreadsheet
* Syntax:     Lrange( aSpreadsheet,nRows,nColumns )
* Arguments:  aSpreadsheet - Array of spreadsheet parameters
*             nRows        - Number of rows in spreadsheet
*             nColumns     - Number of columns in spreadsheet
* Notes:      The Lrange() command is used to specify how many
*             active rows and columns the spreadsheet has.
* Returns:    nStatus
********************************************
function lrange(aWks,nRows,nCols)

LOCAL retval:=ERR_INVALID_PARAMETERS
DEFAULT nRows TO 1
DEFAULT nCols TO 1

if valtype(aWks)=="A" .and. !empty(aWks) .and. ;
   valtype(nRows)=="N" .and. valtype(nCols) == "N"

   WKS_ROWS    := nRows
   WKS_COLUMNS := nCols
   retval      := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lrange ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
