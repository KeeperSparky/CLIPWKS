*******************************************************************************
* Program.....: Import.prg                                                    *
* Authors.....: Joseph D. Booth                                               *
* Copyright...: 1998 Joseph D. Booth, All rights reserved.                    *
* Application.: CLIPWKS++                                                     *
* Version.....: 1.1                                                           *
* Compile.....: Xpp import -l -m -n -w                                        *
*               Limport(cWks_file,cDbf_file,cRange,cDeftype)                  *
*                                                                             *
*******************************************************************************

#include "CLIPWKS.CH"
#include "FILEIO.CH"

#define  MISSING_SPREADSHEET        -1
#define  COULD_NOT_OPEN_FILE        -2
#define  INVALID_RANGE              -3
#define  INDETERMINATE_FIELD_TYPE   -4
#define  DBF_CREATE_PROBLEM         -5

#define  CELL_CONTENTS              arr_[1]
#define  CELL_TYPE                  arr_[2]
#define  CELL_ROW                   arr_[5]
#define  CELL_COLUMN                arr_[6]

#define  DEFAULT_TYPE               "N"


* Function:   Limport()
* Purpose:    To import a spreadsheet into a .DBF file
* Syntax:     Limport( cSpreadsheet,cDBF_file [,cRange] [,cDefType] )
*
* Arguments:  cSpreadsheet - name of the spreadsheet to read
*             cDBF_file    - name of .DBF file to import into
*             cRange       - optional range of cells to import
*             cDefType     - optional default field type
*
* Notes:      If the <cDBF_file> exists, it's structure is used
*             and data is appended into the file.  If you want
*             a new file created, be sure to ZAP the file before
*             Limport().
*
*             If the <cDBF_file> does not exist, then Limport()
*             uses the first row in the range of cells to
*             determine the types and sizes of the fields in
*             the .DBF file.  Be sure to specify a cell range
*             that does not include headers, since this would
*             result in all fields being character type, which
*             might not be what you want.  For example:
*
*                    A                 B                C
*             1     Name            Hire date        Salary
*             2   George Bush       01-JAN-89        120,000.00
*             3   Dan Quayle        01-JAN-89         80,000.00
*
*             Specify range A2..C3 rather than A1..C3.
*
* Returns:    nStatus      0 = Ok
*                         -1 = Missing spreadsheet
*                         -2 = Problem opening the file
*                         -3 = Invalid range specified
*                         -4 = Field type unknown in header
*                         -5 = Problem creating the .DBF file
*
******************************************************************************

function limport(cWks_file,cDbf_file,cRange,cDefType)

LOCAL x ,aWks , flist := {}, arr_
LOCAL cc, tmp, nSize, nDec, nFirstRec :=0,cType, ok := .T.

if valtype(cWks_file) <> "C"          // Missing spreadsheet name, return
   return MISSING_SPREADSHEET         // an error since we cannot process
endif                                 // the function call.

if valtype(cDbf_file) <> "C"          // If the .DBF file is not specified
   x := at(".",cWks_file)             // we will use the same file name as
   x := if(x=0,len(cWks_file),x-1)    // as the spreadsheet file

   cDbf_file := substr(cWks_file,1,x)+".DBF"
endif

if cDefType == NIL                    // If you do not specify a cell type,
   cDefType := DEFAULT_TYPE           // the default will be numeric
endif


aWks := Lread( cWks_file )            // Attempt to open for reading

if !empty(aWks)                      // If opened ok,
   *
   if cRange == NIL
      cRange := Lr( WKS_TOP_ROW+1,WKS_TOP_COL+1,WKS_BOTTOM_ROW,WKS_BOTTOM_COL )
   endif
   arr_ := Lgetrange( aWks,cRange)   // Get the select range of cells into an
   if empty(arr_)                    // array.  Range can be a named range or
      Lclose(aWks)                   // two cells marking the top and bottom.
      return INVALID_RANGE
   endif
   *                                 // If the file does not exist, we need
   if !file(cDbf_file)               // to create it using the first row as
      for x := 1 to len(arr_[1])     // the template structure.
         tmp     := arr_[1,x]
         cType   := if(tmp==NIL,cDeftype,valtype(tmp))
         nSize   := 1
         nDec    := 0
         if cType == "D"
            nSize := 8
         elseif cType == "N"
            nDec  := 2
            nSize := 12
         elseif cType == "C"
            nSize := asc(substr(WKS_CELL_WIDTHS,x,1))
         endif
         if x < 27
            Aadd(flist, {"COL_"+chr(64+x),cType,nSize,nDec} )
         else
            cc := int(x / 26)
            Aadd(flist, {"COL_"+chr(64+cc)+chr(65+(x%26)),cType,nSize,nDec} )
         endif
      next

      if len(flist) > 0

         dbcreate( cDbf_file,flist )
         use (cDbf_file) new
         for x := 1 to len(arr_)
            append blank
         next
      else
         return DBF_CREATE_PROBLEM
      endif
   else
      use (cDbf_file) new
      nFirstRec := reccount()
      for x := 1 to len(arr_)
         append blank
      next
      flist := Dbstruct()
   endif
   //
   // At this point, the array has been retrieved and the .DBF structure is
   // known, so we place elements into the appropiate fields and records.
   //
   if ok
      for x:=1 to len(arr_)
         goto nFirstRec+x
         for cc:=1 to len(arr_[x])
            //
            // If the data returned from the spreadsheet agrees with the field
            // type, just place it directly into the .DBF file.
            //
            if flist[cc,2] == valtype(arr_[x,cc])
              fieldput(cc,arr_[x,cc])
            else
               //
               // Otherwise, call MakeType() to convert the spreadsheet data
               // to the proper field type.
               //
               tmp := MakeType(flist[cc,2],arr_[x,cc])
               if tmp <> NIL
                  fieldput(cc,tmp)
               endif
            endif
         next
      next
      use
   endif
   *
   Lclose(aWks)
else
   return COULD_NOT_OPEN_FILE
endif
return 0
*******************************************************************************

//   Function:  MakeType
//    Purpose:  This function attempts to convert the spreadsheet data into
//              the type of data the .DBF file is expecting in this field
//
//
//
//     Thanks:  John F. Kaster
////////////////////////////////////////////////////////////
STATIC function MakeType( cToType, cField )
LOCAL cFromType := valtype( cField )
if cToType == NIL .or. cField == NIL
  return NIL
endif
   
if cToType $ 'CM'                // Convert "From" type field to character
   if cFromType == 'N'
      return alltrim(str(cField))
   elseif cFromType == 'L'
      return iif(cField,'Y','N')
   elseif cFromType == 'D'
      return dtoc(cField)
   else
      return cField
   endif
elseif cToType == 'N'            // Convert "From" type field to numeric
   if cFromType $ 'CM'
      return val(cField)
   elseif cFromType == 'L'
      return iif(cField,1,0)
   elseif cFromType == 'D'
      return cField - ctod("12/31/1899")
   else
      return cField
   endif
elseif cToType == 'D'            // Convert "From" type field to date
   if cFromType $ 'CM'
      return if(ctod(cField) <> "  /  /  ",ctod(cField),NIL)
   elseif cFromType == 'L' // Ain't no conversion
      return NIL
   elseif cFromType == 'N'
      return  ctod("12/31/1899") + cField
   else
      return cField
   endif
elseif cToType == 'L'            // Convert "From" type field to logical
   if cFromType $ 'CM'
      return substr(upper(cField),1,1)$'YT'
   elseif cFromType == 'D' // Ain't no conversion
      return NIL
   elseif cFromType == 'N'
      return cField <>  0
   else
     return cField
   endif
endif
return NIL
*******************************************************************************
*±±± End of Limport ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±*
*******************************************************************************
