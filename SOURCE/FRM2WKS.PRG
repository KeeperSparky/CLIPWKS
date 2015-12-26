*  Program.....: Frm2twks.prg
*  Authors.....: Joseph D. Booth
*  Copyright...: 1998 Joseph D. Booth, All rights reserved.
*  Application.: CLIPWKS++
*  Version.....: 1.1
*  Compile.....: xpp frm2wks /w /m /l
*  Functions...: Frmtowks(cFrm_file,cWks_file,cType)
*  Purpose.....: This functions takes an existing form file and creates a
*                spreadsheet file in accordance with the FRM file contents.
*  Calls.......: Lclose()
*                Lcreate()
*                Lput()
*                Lput()
*                Lrange()
*                Lwidth()
*
****************************************************************************
#include "CLIPWKS.CH"
#include "FILEIO.CH"


#define  FORM_FILE_MISSING       -1
#define  FORM_FILE_IO_ERROR      -2
#define  INVALID_FORM_FILE       -3
#define  COULD_NOT_OPEN_FILE     -4
#define  NO_WORK_AREA            -5

#define  FORM_FILE_SIZE        1990
#define  TOTAL_COLUMN          substr(col_stuff[k],4,1)
#define  COLUMN_SIZE           val(substr(col_stuff[k],5,3))



STATIC fbuf, col_stuff, no_col, aWks

* Function:   Frmtowks()
* Purpose:    To convert a report form file to a spreadsheet
* Syntax:     Frmtowks( cReport_form,cSpreadsheet,cVersion )
* Arguments:  cReport_form - name of the report form file to use
*             cSpreadsheet - name of the spreadsheet to create
*             cVersion     - type of spreadsheet to produce
* Notes:      FRMTOWKS() is used to read standard report form
*             files and create spreadsheets from their contents.
*             This allows you to give the user the option of
*             printing to the screen/printer/ or a spreadsheet.
*
*             The valid spreadsheet types are:
*
*             L1  - Lotus 1.x         QU  - Quattro
*             L2  - Lotus 2.x         QP  - Quattro Pro
*             L3  - Lotus 3.x         SY  - Symphony
*             E2  - Excel 2.x         E3  - Excel 3.x for Windows
*             E4  - Excel 4.x
*
* Returns:    nStatus  -   -1 Form file is missing
*                          -2 I/O error reading form file
*                          -3 Invalid form file
*                          -4 Error creating spreadsheet
*                          -5 No current work area
**********************************************
function frmtowks(cFrm_file,cWks_file,cStype)

LOCAL retval := FORM_FILE_MISSING, frm_h, fsize, col_hdr := {}
LOCAL k ,totit, places, espot, hspot, srow := 5 ,j ,x1
LOCAL gp_start, gp_sum, gp_test ,grp ,ptr ,lversion, tmp


/***** Initialize STATIC variables *****/

 col_stuff :={}
 fbuf      :=""


/*** Set defaults and form file name if needed ***/

cStype    := if( valtype(cStype) <> "C" , "L2" , cStype )
cFrm_file := if( at(".",cFrm_file) == 0, trim(cFrm_file)+".FRM", cFrm_file)
lversion  := if( substr(cStype,1,1) =="Q", "QUATTRO", "LOTUS")

/*** Create a default spreadsheet name if none passed ***/

if valtype(cWks_file) <> "C"
   k         := at( "." ,cFrm_file )
   cWks_file := substr(cFrm_file,1,k-1) + "." + Lext( cStype )
endif


if file(cFrm_file)                        // If file exists

   frm_h  := fopen(cFrm_file,FO_READ)     // and can be opened
   retval := FORM_FILE_IO_ERROR           // Form file couldn't be opened

   if frm_h > 0                           // File succesfully open
      retval := INVALID_FORM_FILE         // Invalid form file ?
      fbuf   := space(FORM_FILE_SIZE)
      fsize  := fread(frm_h,@fbuf,FORM_FILE_SIZE)
      fclose(frm_h)                     // Close the FRM file

      if fsize == FORM_FILE_SIZE        // Size of form file

         /*********************************************
          *** Check first 2 bytes and last 2 bytes, ***
          *** both should equal 2 for a valid FRM   ***
          *********************************************/

         if bin2i(substr(fbuf,1,2))==2 .and. bin2i(substr(fbuf,1989,2))==2

            no_col := bin2w(substr(fbuf,1983,2))   // Number of columns
            ptr    := bin2w(substr(fbuf,1967,2))   // Ptr to group info
            grp    := if( ptr>=0 .and. ptr<=55, trim(frmgetexp(ptr)), "")
            *************************************************
            *±±± Build arrays to hold column information ±±±*
            *************************************************
            for k = 1 to no_col
               Aadd(col_stuff, str(bin2w(substr(fbuf,1665+(k*12),2))+1,3))

               if val(col_stuff[k]) == 0
                  no_col--                         // Last column read
               else
                  ********************************************************
                  *±±± Extract column information about put in arrays ±±±*
                  ********************************************************
                  totit        := substr(fbuf, 1670+(k*12) ,1)
                  places       := str( bin2w( substr( fbuf,1671+(k*12),2)),3)
                  espot        := bin2w( substr( fbuf,1673+(k*12),2) )
                  col_stuff[k] += totit + places + frmgetexp(espot)
                  hspot        := bin2w( substr( fbuf,1675+(k*12),2) )

                  Aadd(col_hdr, frmgetexp(hspot) )

               endif
            next

            ********************************
            *±±± Create the spreadsheet ±±±*
            ********************************

            aWks  := Lcreate(cWks_file,cStype)

            if !empty(aWks)
               for k=1 to no_col
                  if len(col_stuff[k]) > 7      // If an expression is present
                     ***********************************************
                     *±±± Set the column widths per definitions ±±±*
                     ***********************************************
                     Lwidth(aWks,k,val(substr(col_stuff[k],1,3)))
                     j := 0
                     while !empty(col_hdr[k])
                        x1 := at(";",col_hdr[k])
                        if x1 == 0
                           x1 := len(col_hdr[k])+1
                        endif
                        j++
                        if x1 > 1
                           Lput(aWks,substr(col_hdr[k],1,x1-1),lc(j+1,k))
                        endif
                        col_hdr[k] := substr(col_hdr[k],x1+1)
                     enddo
                  endif
               next
               ******************************************
               *±±± If a work area has been selected ±±±*
               ******************************************
               if "" <> alias()
                  **************************************
                  *±±± Set up group starting values ±±±*
                  **************************************
                  gp_start := srow
                  gp_sum   := ""
                  if !empty(grp)
                     gp_test := &grp.
                  endif

                  ************************************
                  *±±± Loop through the work area ±±±*
                  ************************************
                  while !eof()
                     if !empty(grp)
                        if &grp. <> gp_test
                           srow     += frmtotals(gp_start+1,srow)
                           gp_test  := &grp.
                           gp_start := srow
                           gp_sum   += str(srow-1,3)
                        endif
                     endif
                     srow++
                     for k=1 to no_col
                        tmp := trim(substr(col_stuff[k],8))
                        if len(tmp) > 0
                           Lput(aWks,&tmp.,lc(srow,k))
                        endif
                     next
                     skip +1
                  enddo
                  if !empty(grp)          // Final subtotal
                     srow     += frmtotals(gp_start+1,srow)
                     gp_sum   += str(srow-1,3)
                  endif
                  if !empty(gp_sum)
                     srow += frmgtot(gp_sum,srow)
                     gp_sum := ""
                  else
                     *******************************************
                     *±±± Grand total, in event of no group ±±±*
                     *******************************************
                     srow += Frmtotals(gp_start+1,srow)
                  endif
                  retval := ALL_OK
               else
                  retval := COULD_NOT_OPEN_FILE
               endif
               Lclose(aWks)
            else
               retval := NO_WORK_AREA
            endif
         endif
      endif
   endif
endif
return retval
*****************************
*±±± End of main program ±±±*
*****************************

* Function:   Frmtotals()
* Purpose:    To write column totals formulas into the spreadsheet
* Syntax:     Frmtotals( nStarting,nFormula_row )
* Arguments:  nStarting      - First row in range
*             nFormula_row   - Row to write formula to
* Notes:      The Frmtotals() function is used to write @SUM
*             records into the spreadsheet.  It is a STATIC
*             function called only in the FRM2WKS.PRG.  Your
*             application program should never need to call this
*             function.  It is documented in case you wish to
*             modify the CLIPWKS source code.
*
* Returns:    nLines
* Class:      Internal
**********************************************
STATIC function frmtotals(start_at,srow)
LOCAL k
for k=1 to no_col
   if TOTAL_COLUMN == "Y"                // Should this column be totaled?
      Lput(aWks,"\=",lc(srow+1,k))       // Put in line of underscores
      Lput( aWks,"@SUM("+Lr(start_at,k,srow,k)+")",Lc(srow+2,k),"F"+alltrim(str(COLUMN_SIZE)) )
   endif
next
return 3
**************************
*±±± End of Frmtotals ±±±*
**************************

* Function:   Frmgtot()
* Purpose:    To write subtotals into the spreadsheet
* Syntax:     Frmgtot( nStarting,nFormula_row )
* Arguments:  nStarting      - First row in range
*             nFormula_row   - Row to write formula to
* Notes:      Frmgtot() is used to write an @SUM() record into a
*             spreadsheet during the FRMTOWKS() function.  It is
*             a STATIC function only available in the FRM2WKS
*             program.  It should never be directly called by
*             your application.
*
* Returns:    nLines
* Class:      Internal
**********************************************
STATIC function frmgtot(rows_to_do,srow)

LOCAL k,j,tmp_str:=""

for k=1 to no_col
   if TOTAL_COLUMN=="Y"                  // Should this column be totaled?
      tmp_str := ""
      Lput(aWks,"\-",lc(srow+1,k))       // Put in line of underscores
      for j=1 to len(rows_to_do) step 3
         tmp_str += lc(val(substr(rows_to_do,j,3)),k)+","
      next
      j       := len(tmp_str)
      Lput( aWks,;
            "@SUM("+substr(tmp_str,1,j-1)+")",;
             Lc(srow+2,k),;
             "F"+alltrim(str(COLUMN_SIZE)) )
   endif

next
return 3
************************
*±±± End of Frmgtot ±±±*
************************

* Function:   Frmgetexp()
* Purpose:    To extract an expression from the form file
* Syntax:     Frmgetexp( nOffset )
* Arguments:  nOffset - Address in report form file where token is
* Returns:    cToken  - Expression from report form file
* Notes:      Frmgetexp() is a STATIC function only called by
*             the FRMTOWKS() function.  You should never call
*             this function directly in your own code.
*
* Class:      Internal
**********************************
STATIC function frmgetexp(nOffset)

LOCAL nStart,nSize
nOffset := ( nOffset*2 ) + 1                       // Adjust offset
nStart  := bin2w( substr( fbuf, 114+nOffset, 2))   // Get token offset
nSize   := bin2w( substr( fbuf, 4+nOffset, 2))     // Get token size
return  substr(fbuf, nStart+225, nSize-1)          // Extract the token
**************************
*±±± End of Frmgetexp ±±±*
**************************
*****************************************************************************
*±±± End of Frmtowks ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
