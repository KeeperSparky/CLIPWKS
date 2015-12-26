*  Program.....: Sortwks.prg
*  Author......: Joseph D. Booth
*  Copyright...: 1998 Joseph D. Booth Consulting, Inc., All rights reserved.
*  Application.: CLIPWKS++
*  Version.....: 1.1
*  Compile.....: Xpp Sortwks /l /m /n /w
*
*                Lsortkey(cWks,cRange,cDirection)
*                Lsortkey2(cWks,cRange,cDirection)
*                Lsortrange(cWks,cRange)
*
****************************************************************************
#include "CLIPWKS.CH"
#define   ASCENDING      255
#define   DESCENDING       0

* Function:   Lsortkey()
* Purpose:    Writes the primary sort key out to the spreadsheet
* Syntax:     Lsortkey( aWks,cRange,cDirection )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cRange         - Range of cells
*             cDirection     - (A)scending or (D)escending
* Notes:      Lsortkey() defines the primary key that your spreadsheet
*             should be sorted on.  The range may be sorted either
*             (A)scending or (D)escending depending upon the
*             needs of your application.
* Returns:    nStatus  -   0   = All ok
*                         -5   = Invalid range
*                         -6   = Invalid parameters
* See Also:   Lsortkey2() Lsortrange()
*****************************************************
function lsortkey(aWks,cRange,cDirection)

LOCAL retval:= ERR_INVALID_PARAMETERS
LOCAL cRangebytes

DEFAULT cDirection TO "A"

// ----- Check parameters for validity

if valtype(aWks) == "A" .and. !empty(aWks) .and. valtype(cRange) == "C"

   cRangebytes := Lmakerange(cRange,WKS_VERSION)

   if !empty(cRangebytes)
      WKS_SORTKEY1     := cRangebytes
      WKS_SORTKEY1_DIR := if(cDirection=="D",DESCENDING,ASCENDING)
      retval           := ALL_OK
   else
      retval           := ERR_INVALID_RANGE
   endif

endif
return retval
*****************************************************************************
*±±± End of Lsortkey ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************


* Function:   Lsortkey2()
* Purpose:    Writes the secondary sort key out to the spreadsheet
* Syntax:     Lsortkey2( aSpreadsheet,cRange,cDirection )
* Arguments:  aSpreadsheet   - Spreadsheet handle array
*             cRange         - Range of cells
*             cDirection     - (A)scending or (D)escending
* Notes:      Lsortkey2() defines the secondary key that your spreadsheet
*             should be sorted on.  The range may be sorted either
*             (A)scending or (D)escending depending upon the
*             needs of your application.
* Returns:    nStatus  -   0   = All ok
*                         -5   = Invalid range
*                         -6   = Invalid parameters
* See Also:   Lsortkey() Lsortrange()
******************************************
function lsortkey2(aWks,cRange,cDirection)

LOCAL retval:=ERR_INVALID_PARAMETERS
LOCAL cRangebytes

DEFAULT cDirection TO "A"

// ----- Check parameters for validity

if valtype(aWks) == "A" .and. !empty(aWks) .and. valtype(cRange) == "C"

   cRangebytes := Lmakerange(cRange,WKS_VERSION)

   if !empty(cRangebytes)
      WKS_SORTKEY2     := cRangebytes
      WKS_SORTKEY2_DIR := if(cDirection=="D",DESCENDING,ASCENDING)
      retval           := ALL_OK
   else
      retval           := ERR_INVALID_RANGE
   endif

endif
return retval
*****************************************************************************
*±±± End of Lsortkey2 ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************



* Function:   Lsortrange()
* Purpose:    Writes the sort range out to the spreadsheet
* Syntax:     Lsortrange( aSpreadsheet,cRange )
* Arguments:  aSpreadsheet    - Spreadsheet handle array
*             cRange          - Range of cells
* Notes:      The sort range determines which cells are sorted when the
*             /DS command is executed.  It is normally all active cells
*             within the spreadsheet.
* Returns:    nStatus  -   0   = All ok
*                         -5   = Invalid range
*                         -6   = Invalid parameters
* See Also:   Lsortkey() Lsortkey2()
*****************************************************
function lsortrange(aWks,cRange)

LOCAL retval:=ERR_INVALID_PARAMETERS
LOCAL cRangebytes

// ----- Check parameters for validity

if valtype(aWks) == "A" .and. !empty(aWks) .and. valtype(cRange) == "C"

   cRangebytes := Lmakerange(cRange,WKS_VERSION)
   if !empty(cRangebytes)
      WKS_SORT_RANGE  := cRangebytes
      retval          := ALL_OK
   else
      retval          := ERR_INVALID_RANGE
   endif

endif
return retval
*****************************************************************************
*±±± End of Lsortrange ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
