* Program.....: Printwks.prg
* Authors.....: Joseph D. Booth
* Copyright...: 1998 Joseph D. Booth, All rights reserved.
* Application.: CLIPWKS++
* Version.....: 1.1
* Compile.....: xpp printwks /l /m /n /w
*               Lborders(aWks,cTop,cLeft)
*               Lfooter(aWks,cFooter)
*               Lheader(aWks,cHeader)
*               Lmargins(aWks
*               Lprint(aWks,(F)ormatted|(U)nformatted)
*               Lprtrange(aWks,cRange)
*               Lsetup(aWks,cSetup)
*
****************************************************************************
#include "CLIPWKS.CH"

#define  LEFT_MARGIN          3
#define  RIGHT_MARGIN        78
#define  TOP_MARGIN           3
#define  BOTTOM_MARGIN        3
#define  LINES_PER_PAGE      66



* Function:   Lborders()
* Purpose:    Write print borders into the worksheet
* Syntax:     Lborders( aSpreadsheet,cTop_range,cLeft_range )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cTop_range     - Range of cells for top border
*             cLeft_range    - Range of cells for left border
* Notes:      Borders are ranges of cells that are printed along
*             the left margin and the top row of a printed
*             spreadsheet.  Be sure to keep the borders in mind
*             when you set the print range of the spreadsheet.
* Returns:    nStatus
* See Also:   Lprtrange()
********************************************************
function lborders(aWks,cTop,cLeft)

LOCAL retval:=ERR_INVALID_PARAMETERS,_rowexp,_colexp
// ---- Validate parameters

if valtype(aWks)=="A" .and. valtype(cTop)=="C" .and. valtype(cLeft)=="C"

   _rowexp := Lmakerange(cTop,WKS_VERSION)     // Convert each range to its
   _colexp := Lmakerange(cLeft,WKS_VERSION)    // internal form

   if !empty(_rowexp) .and. !empty(_colexp)
      WKS_BORDERS := { _rowexp,_colexp }
      retval      := ALL_OK
   else
      retval      := ERR_INVALID_RANGE
   endif

endif
return retval
*****************************************************************************
*±±± End of Lborders ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lfooter()
* Purpose:    Write footer string into the spreadsheet
* Syntax:     Lfooter( aSpreadsheet,cFooting )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cFooting       - Footer string
* Notes:      The footer string appears on the bottom of each
*             page whenever the spreadsheet is printed.  It may
*             be up to 240 characters in length.  Lfooter() is
*             used to specify the footing string.
* Returns:    nStatus
* See Also:   Lheader()
********************************************************
function lfooter( aWks,cFooting )
LOCAL retval := ERR_INVALID_PARAMETERS
DEFAULT cFooting TO ""
if valtype( aWks ) == "A" .and. !empty(aWks)

   WKS_FOOTER := cFooting
   retval     := ALL_OK

endif
return retval
****************************************************************************
*±±± End of Lfooter ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
****************************************************************************



* Function:   Lheader()
* Purpose:    Write header string into the spreadsheet
* Syntax:     Lheader( aSpreadsheet,cHeading )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cHeading       - Heading string
* Notes:      The header string gets printed at the top of each
*             page whenever the spreadsheet is printed.  The
*             Lheader() function is used to specify this string.
*             It may be up to 240 characters long.
* Returns:    nStatus
* See Also:   Lfooter(),Lmargins()
********************************************************
function lheader( aWks,cHeading )
LOCAL retval := ERR_INVALID_PARAMETERS
DEFAULT cHeading TO ""
if valtype( aWks ) == "A" .and. !empty(aWks)
   WKS_HEADER := cHeading
   retval     := ALL_OK
endif
return retval
*****************************************************************************
*±±± End of Lheader ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lmargins()
* Purpose:    Write printer margin information into the spreadsheet
* Syntax:     Lmargins( aSpreadsheet,nLeft,nRight,nPage,nTop,nBottom )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             nLeft          - Left margin in columns
*             nRight         - Right margin in columns
*             nPage          - Lines per page
*             nTop           - Top margin in rows
*             nBottom        - Bottom margin in rows
* Notes:      Lmargins() is used to specify the margins and page
*             length.  The left and right margin can be between
*             zero and 254 characters. The top and bottom margin
*             can be between zero and 32 lines.  The page length
*             can be between one and 100 lines.
*
*             Default values for various margins are listed below:
*
*             Left       3
*             Right     78
*             Top        3
*             Bottom     3
*             Lines     66
* Returns:    nStatus
* See Also:   Lsetup()
********************************************************
function lmargins(aWks,nLeft,nRight,nPage,nTop,nBottom)
LOCAL retval := ERR_INVALID_PARAMETERS
if valtype(aWks) == "A" .and. !empty(aWks)

   WKS_LEFT   := if(valtype(nLeft)  <>"N" , LEFT_MARGIN    ,nLeft)
   WKS_RIGHT  := if(valtype(nRight) <>"N" , RIGHT_MARGIN   ,nRight)
   WKS_LPP    := if(valtype(nPage)  <>"N" , LINES_PER_PAGE ,nPage)
   WKS_TOP    := if(valtype(nTop)   <>"N" , TOP_MARGIN     ,nTop)
   WKS_BOTTOM := if(valtype(nBottom)<>"N" , BOTTOM_MARGIN  ,nBottom)
   retval     := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lmargins ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************

*
* Function:   Lprint()
* Purpose:    Write select formatted or unformatted spreadsheet printing
* Syntax:     Lprint( aSpreadsheet,cMethod )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cMethod        - (F)ormatted,(U)nformatted
* Notes:      Spreadsheets are normally printed in formatted
*             mode, showing both row and column headers.  You
*             can instruct the spreadsheet to print either
*             formatted or unformatted using the Lprint()
*             function.
* Returns:    nStatus
********************************************************
function lprint(aWks,cMethod)

LOCAL retval := ERR_INVALID_PARAMETERS
DEFAULT cMethod TO "F"

if valtype( aWks ) == "A" .and. !empty(aWks)

   WKS_PRINT := cMethod
   retval    := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lprint ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lprtrange()
* Purpose:    Write print range of cells to spreadsheet
* Syntax:     Lprtrange( aSpreadsheet,cRange )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cRange         - Range of cells to print
* Notes:      The print range determines what cells will be
*             printed when the user prints the spreadsheet.  Be
*             sure to keep in mind the value of the Lborders()
*             settings when specifying the print range.
* Returns:    nStatus
* See Also:   Lborders()
********************************************************
function lprtrange(aWks,cRange)

LOCAL retval:=ERR_INVALID_PARAMETERS,cRangebytes

if valtype(aWks) =="A" .and. valtype(cRange) == "C"
   cRangebytes := Lmakerange(cRange,WKS_VERSION)
   if !empty(cRangebytes)
      WKS_RANGE := cRangebytes
      retval    := ALL_OK
   else
      retval    := ERR_INVALID_RANGE
   endif
endif
return retval
*****************************************************************************
*±±± End of Lprtrange ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lsetup()
* Purpose:    Write printer setup string into the spreadsheet
* Syntax:     Lsetup( aSpreadsheet,cSetup )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cSetup         - Printer setup string
* Notes:      The printer setup string can include imbedded
*             ASCII codes by using the backward slash followed
*             by the three digit ASCII code.  For example, \015
*             would be used to set condensed mode on most Epson
*             printers.  If multiple characters are needed, no
*             space should be used to separate them.
* Returns:    nStatus
* See Also:   Lmargins()
********************************************************
function lsetup(aWks,cSetup)

LOCAL retval := ERR_INVALID_PARAMETERS
DEFAULT cSetup TO ""

if valtype( aWks ) == "A" .and. !empty(aWks) .and. valtype(cSetup)=="C"

   WKS_SETUP := cSetup
   retval    := ALL_OK

endif
return retval
*****************************************************************************
*±±± End of Lsetup ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
