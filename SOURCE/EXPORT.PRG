* Program.....: Export.prg
* Authors.....: Joseph D. Booth
* Copyright...: 1998 Joseph D. Booth, All rights reserved.
* Application.: CLIPWKS++
* Version.....: 1.1
* Compile.....: xpp export -l -m -n -w
*
****************************************************************************
#include "CLIPWKS.CH"
//
// Taken from DBSTRUCT.CH
//
#define  DBS_NAME      1  // Field name
#define  DBS_TYPE      2  // Field type
#define  DBS_LEN       3  // Field length
#define  DBS_DEC       4  // Number of decimal places


* Function:   Lexport()
* Purpose:    To export a .DBF file to a spreadsheet
* Syntax:     Lexport( cSpreadsheet,aFld_exp,aHdg_exp,cRecords,cVersion )
* Arguments:  cSpreadsheet - name of the spreadsheet to create
*             aFld_exp     - array of fields to export
*             aHdg_exp     - array of heading expressions
*             cRecords     - which records to write to spreadsheet
*             cVersion     - type of spreadsheet to produce
* Notes:      Lexport() is a high-level function use to transfer
*             records from a .DBF file into a spreadsheet.  A
*             great degree of flexibility is provided, including
*             control of the fields and headings, and the
*             selection of records to write to the spreadsheet.
*
*             cWksfile is the name of the spreadsheet to create
*             and it defaults to the work area name with a .WK1
*             extension.
*
*             aFields is either an array or a semi-colon
*             delimited list of fields/functions to place in
*             each column of the spreadsheet.
*
*             aHeaders is a corresponding array or semi-colon
*             delimited list of header strings.  Each element
*             contains the header for the parallel entry in the
*             aFields array.  If aHeaders is a logical .F., no
*             headers will be written to the spreadsheet.
*
*             cRecs is a character string indicating which
*             records to transfer into the spreadsheet.
*
*             cVersion may be
*
*             L2  - Lotus 2.x
*             L3  - Lotus 3.x
*             QU  - Quattro
*             QP  - Quattro Pro
* Returns:    nStatus
**********************************************
function lexport(cFilename,fldlist,hdglist,which_recs,cVersion)

LOCAL temp,k,fc,headers:=.T.,x,aWks,rc,sk_fact,fw
LOCAL _seekit,ret_val:=0,l3dates:={},_cond,bBlock

DEFAULT cVersion TO "L2"             // Default to Lotus 2.x spreadsheets

if "" = alias()                      // The work area is empty
   return -1                         // so return an error code
endif
***********************************
*±±± Validate file name ±±±±±±±±±±*
***********************************
if valtype(cFilename) <> "C"
   cFilename := trim(alias())
endif
/********************************************************
*±±± Convert semi-colon delimited string to an array ±±±*
********************************************************/
if valtype(fldlist)=="C"                // if a semi-colon delimited string
   if !empty(fldlist)                   // is passed, convert it to an array
      fldlist := to_arr(fldlist)
   endif
endif
***************************************************
*±±± If not passed, build default fldlist file ±±±*
***************************************************
if valtype(fldlist) <> "A"
   fldlist :={}
   temp    := dbstruct()
   for k   := 1 to len(temp)
      if temp[k,DBS_TYPE]="D"
         temp[k,DBS_LEN]++
         Aadd(l3dates,k)
      endif
      Aadd(fldlist,Trim(temp[k,DBS_NAME])+"|"+;
                        str(temp[k,DBS_LEN],3)+"."+;
                        ltrim(trim(str(temp[k,DBS_DEC],2))))
   next
endif
fc := Len(fldlist)
*********************************************************
*±±± Convert semi-colon delimited string to an array ±±±*
*********************************************************

if valtype(hdglist)=="C"                // if a semi-colon delimited string
   if !empty(hdglist)                   // is passed, convert it to an array
      hdglist := to_arr(hdglist)
   endif
elseif valtype(hdglist)=="L"            // Logical .F. disables headers
   headers := hdglist
endif

if headers .and. valtype(hdglist)<>"A"  // Headers needed, but not passed
   hdglist:={}
   for k:=1 to fc
      x := at("|",fldlist[k])
      Aadd(hdglist,if(empty(x),fldlist[k],substr(fldlist[k],1,x-1)))
   next
endif

if valtype(which_recs)<>"A"
   which_recs := if(valtype(which_recs)=="C",which_recs,"ALL")
endif

****************************************************************************
*±±± Attempt to create new file ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
****************************************************************************

aWks       := Lcreate( cFilename,cVersion ) // Create a spreadsheet file

if !empty(aWks)                             // If a valid handle returned
   *
   do case                                  // Try to determine number of
   case valtype(which_recs)=="A"            // records to write to lotus
      rc := len(which_recs)

   case which_recs == "ALL"
      rc := reccount()                      // Number of records

   case substr(which_recs,1,5) = "EVERY"

      sk_fact  := val(substr(which_recs,7)) // Extract number of records
      sk_fact  := if(sk_fact=0,2,sk_fact)   // to skip.  Default to 2
      rc       := if(headers,2,0) + (reccount()/sk_fact)+1

   case Substr(which_recs,1,3) = "TAG"
      rc := len(trim(which_recs))-len(strtran(trim(which_recs),"û",""))+1

   otherwise
      rc       := if(headers,2,0) +reccount()

   endcase
   *
   Lrange( aWks,rc+if(headers,2,0),fc )     // Write record range to file
   *
   for k:=1 to fc
      fw := Lgetwide(fldlist[k])
      Lwidth(aWks,k,fw+1)                   // Set width of column
   next
   *
   if cVersion $ "L3|L4"                    // Format Lotus 3.x + dates
      for k:=1 to len(l3dates)
         Lformat(aWks,lr(if(headers,3,1),l3dates[k],if(headers,rc+2,rc),l3dates[k]),"D1")
      next
   endif
   *
   if headers                              // Check for headers
      for k:=1 to fc                       // Write headings into file
         Lput(aWks,hdglist[k],lc(1,k))     // Field name
         Lput(aWks,"\=",lc(2,k))           // Underline the field name
      next
   endif
*********************************************************
*±±± Transfer each record into the lotus spreadsheet ±±±*
*********************************************************
   k:=if(headers,0,-2)
   do case
   case valtype(which_recs)=="A"
      for x:=1 to rc
         goto which_recs[x]
         Lputout(++k,fldlist,aWks)
      next
   case which_recs == "ALL"
      go top
      while !eof()
         Lputout(++k,fldlist,aWks)
         skip +1
      enddo
   case substr(which_recs,1,5) == "EVERY"
      go top
      while !eof()
         Lputout(++k,fldlist,aWks)
         skip sk_fact
         rc++
      enddo
   case substr(which_recs,1,3) == "TAG"
      for x=1 to len(which_recs)+4
         goto x
         if Substr(which_recs,x+4,1)="û"
            Lputout(++k,fldlist,aWks)
         endif
      next
   case Substr(which_recs,1,3) == "FOR"
      _cond := substr(which_recs,5)
      bBlock := &( "{ || "+alltrim(_cond)+" }" )
      go top
      if eval(bBlock) <> "L"
         ret_val := -3
      else
         while !eof()
            if eval(bBlock)
               Lputout(++k,fldlist,aWks)
               rc ++
            endif
            skip +1
         enddo
      endif

   case Substr(which_recs,1,5) == "WHILE"
      which_recs := Substr(which_recs,7)
      x          := at(",",which_recs)
      _seekit    := Substr(which_recs,1,x-1)
      _cond      := Substr(which_recs,x+1)
      bBlock     := &( "{ || "+alltrim(_cond)+" }" )
      go top
      seek _seekit
      if eof()
         ret_val := -4
      elseif eval(bBlock) <> "L"
         ret_val := -3
      else
         while !eof() .and. eval(bBlock)
            Lputout(++k,fldlist,aWks)
            rc++
            skip +1
         enddo
      endif
   endcase
*****************************
*±±± Close the worksheet ±±±*
*****************************
   Lclose(aWks)
else
   ret_val := -2
endif
return ret_val
* *************************




* Function:    Lputout()
* Purpose:     To write a row of data into a spreadsheet
* Syntax:      Lputout( nRow,aField_list,aSpreadsheet )
* Arguments:   nRow         - Row number to put data into
*              aField_list  - Array of field expressions to write
*              aSpreadsheet - array of spreadsheet parameters
* Notes:       Lputout() is a STATIC function used by Lexport()
*              to write data into a cell.  It should never be
*              called directly by your application.  It is used
*              to write all the fields/functions from the current
*              record into the spreadsheet.
* Returns:     1
* Class:       Internal
**********************************************
STATIC function lputout(nRow,flist,aWks)
LOCAL j,jj:=0,x,tmp,bBlock

for j=1 to len(flist)
   x       := at("|",flist[j])
   tmp     := if( x>0,substr(flist[j],1,x-1),flist[j] )
   bBlock  := &( "{ || "+alltrim(tmp)+" }" )
   jj++
   Lput(aWks,eval(bBlock),lc(nRow+2,jj) )
next
return 1
*****************************************************************************
*±±± End of Lputout() ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
* Purpose:     To determine the cell width for a given string
* Syntax:      Lgetwide( cString )
* Arguments:   cString      - Field or expression to output
* Notes:       Lgetwide() is used to extract a width portion from
*              a string passed to Lexport().  It is a STATIC
*              function only used in EXPORT.PRG and should not be
*              called from your application.
* Returns:     nWidth       - Number of characters wide
* Class:       Internal
**********************************************
STATIC function lgetwide(cStr)
LOCAL x1:=at("|",cStr),sarr:={}
if x1 == 0
   sarr := dbstruct()
   x1   := fieldpos( cStr )
   return if(x1==0,DEFAULT_WIDTH,sarr[x1,DBS_LEN] )
endif
return val(substr(cStr,x1+1))
*****************************************************************************
* Purpose:     To create an array from a semi-colon delimited string
* Syntax:      to_arr( cString )
* Arguments:   cString      - semi-colon delimited string
* Notes:       To_arr is a STATIC function used in Lexport() to
*              convert a semi-colon delimited string into an
*              array. The Summer '87 version requires a second
*              parameter, the array to fill.  This function is
*              used externally and should never be called by your
*              application.
* Returns:     aString      - Array of strings
* Class:       Internal
**********************************************
STATIC function to_arr(cStr)
LOCAL farr:={},x
while !empty(cStr)
   x := at(";",cStr)
   if !empty(x)
      Aadd(farr,Substr(cStr,1,x-1))
      cStr := Substr(cStr,x+1)
   else
      Aadd(farr,cStr)
      cStr :=""
   endif
enddo
return farr
*****************************************************************************
*±±± End of Lexport ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*****************************************************************************
