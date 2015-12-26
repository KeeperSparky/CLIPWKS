/* Header file....: CLIPWKS.CH
   Author.........: Joseph D. Booth

***************************************************************************/
#xcommand DEFAULT <param> TO <value> => ;
         <param> := if( <param> == NIL, <value>, <param> )

#define HEX_FFFF     chr(255)+chr(255)


/******************************************* Error return codes **********/
#define ALL_OK                      0
#define ERR_BAD_HANDLE             -1
#define ERR_IO_ERROR               -2
#define ERR_WRONG_HANDLE           -3
#define ERR_INVALID_CELL           -4
#define ERR_INVALID_RANGE          -5
#define ERR_INVALID_PARAMETERS     -6
#define ERR_MISSING_PARAMETER      -7
#define ERR_NOT_SUPPORTED          -8
#define ERR_INVALID_COLUMN         -9
#define ERR_INVALID_WIDTH         -10
#define ERR_CELL_TYPE_MISMATCH    -11
#define ERR_READONLY_WKS          -12

/****************************************** Spreadsheet op codes **********/

#define  BOF_OPCODE             i2bin(  0)+i2bin(  2)
#define  EOF_OPCODE             i2bin(  1)+i2bin(  0)
#define  MODE_OPCODE            i2bin(  2)+i2bin(  1)
#define  ORDER_OPCODE           i2bin(  3)+i2bin(  1)
#define  SPLIT_OPCODE           i2bin(  4)+i2bin(  1)
#define  SYNC_OPCODE            i2bin(  5)+i2bin(  1)
#define  RANGE_OPCODE           i2bin(  6)+i2bin(  8)+i2bin(0)+i2bin(0)
#define  WINDOW1_OPCODE         i2bin(  7)
#define  WIDTH_OPCODE           i2bin(  8)+i2bin(  3)
#define  NAMED_RANGE_OPCODE     i2bin( 11)+i2bin( 24)
#define  BLANK_OPCODE           i2bin( 12)+i2bin(  5)
#define  REAL_OPCODE            i2bin( 14)+i2bin( 13)
#define  LABEL_OPCODE           i2bin( 15)
#define  FORMULA_OPCODE         i2bin( 16)
#define  VERSION_OPCODE         i2bin( 19)+i2bin(  1)
#define  PRINT_RANGE_OPCODE     i2bin( 26)+i2bin(  8)
#define  SORT_RANGE             i2bin( 27)+i2bin(  8)
#define  SORT_KEY_OP            i2bin( 29)+i2bin(  9)
#define  SORT_KEY2_OP           i2bin( 35)+i2bin(  9)
#define  PROTECT_OPCODE         i2bin( 36)+i2bin(  1)
#define  FOOTER_OPCODE          i2bin( 37)+i2bin(242)
#define  HEADER_OPCODE          i2bin( 38)+i2bin(242)
#define  SETUP_OPCODE           i2bin( 39)+i2bin( 40)
#define  QP_FOOTER_OPCODE       i2bin( 37)+i2bin(len(trim(aWks[10,3]))+1)+chr(len(trim(aWks[10,3])))
#define  QP_HEADER_OPCODE       i2bin( 38)+i2bin(len(trim(aWks[10,2]))+1)+chr(len(trim(aWks[10,2])))
#define  QP_SETUP_OPCODE        i2bin( 39)+i2bin(len(trim(aWks[10,1]))+1)+chr(len(trim(aWks[10,1])))
#define  MARGINS_OPCODE         i2bin( 40)+i2bin( 10)
#define  ALIGN_OPCODE           i2bin( 41)+i2bin(  1)
#define  BORDERS_OPCODE         i2bin( 42)+i2bin( 16)
#define  ITERATE_OPCODE         i2bin( 47)+i2bin(  1)
#define  FORMAT_OPCODE          i2bin( 48)+i2bin(  1)
#define  HIDVEC1_OPCODE         i2bin(100)+i2bin( 32)

/******************************************** Lotus 3.x op codes **********/

#define  WK3_BOF                i2bin( 0)+i2bin( 26)+i2bin(4096)+i2bin(4)
#define  WK4_BOF                i2bin( 0)+i2bin( 26)+i2bin(4098)+i2bin(4)
#define  WK3_CALCSET            i2bin( 3)+i2bin(  6)
#define  WK3_WINDOWSET          i2bin( 4)+i2bin( 28)
#define  WK3_SHEETCELLPTR       i2bin( 5)+i2bin( 16)
#define  WK3_WIDTH              i2bin( 7)+i2bin(len(tmp)+4)
#define  WK3_USERRANGE          i2bin( 9)+i2bin( 26)
#define  WK3_SYSTEMRANGE        i2bin(10)+i2bin( 26)
#define  WK3_SORTKEYDIR         i2bin(12)+i2bin(  4)
#define  WK3_PRINTMAIN          i2bin(15)+i2bin( 86)
#define  WK3_PRINTSTRING        i2bin(16)+i2bin(244)
#define  WK3_FORMAT             i2bin(19)
#define  WK3_GBLFMT             i2bin(19)+i2bin( 12)
#define  WK3_LABELCELL          i2bin(22)
#define  WK3_NUMBERCELL         i2bin(23)+i2bin( 14)
#define  WK3_SMALLNUMCELL       i2bin(24)+i2bin(  6)
#define  WK3_FORMULACELL        i2bin(25)


/************************************************ Excel op codes *************/

#define  E2_BOF_OPCODE          i2bin(   9)+i2bin(  4)+i2bin(2)+i2bin(16)
#define  E3_BOF_OPCODE          i2bin( 521)+i2bin(  6)+i2bin(0)+i2bin(16)+i2bin(0)
#define  E4_BOF_OPCODE          i2bin(1033)+i2bin(  6)+i2bin(0)+i2bin(16)+i2bin(0)
#define  E2_EOF_OPCODE          i2bin(  10)+i2bin(  0)
#define  E2_DIMENSIONS          i2bin(   0)+i2bin(  8)
#define  E3_DIMENSIONS          i2bin( 512)+i2bin( 10)
/***************************************************** Excel data opcodes ****/
#define  E2_BLANK               i2bin(  1)+i2bin( 7)
#define  E3_BLANK               i2bin(513)+i2bin( 6)
#define  E2_BOOLERR             i2bin(  5)+i2bin( 9)
#define  E3_BOOLERR             i2bin(517)+i2bin( 8)
#define  E2_NUMBER              i2bin(  3)+i2bin(15)
#define  E3_NUMBER              i2bin(515)+i2bin(14)
#define  E2_FORMULA             i2bin(  6)
#define  E3_FORMULA             i2bin(518)
#define  E4_FORMULA             i2bin(1030)
#define  E2_LABEL               i2bin(  4)
#define  E3_LABEL               i2bin(516)



#define  XF_OPCODE              i2bin(579)+i2bin(12)
#define  E2_DEFAULTWIDTH        i2bin(85)+i2bin(2)
#define  E2_COLWIDTH            i2bin(36)+i2bin(4)


#define  E2_CALCCOUNT           i2bin( 12)+i2bin( 2)
#define  E2_CALCMODE            i2bin( 13)+i2bin( 2)
#define  E2_PRECISION           i2bin( 14)+i2bin( 2)
#define  E2_REFMODE             i2bin( 15)+i2bin( 2)
#define  E2_DELTA               i2bin( 16)+i2bin( 8)
#define  E2_ITERATION           i2bin( 17)+i2bin( 2)
#define  E3_FORMAT              i2bin( 30)
#define  E2_1904                i2bin( 34)+i2bin( 2)
#define  E2_PRINT_HEADERS       i2bin( 42)+i2bin( 2)
#define  E2_PRINT_GRIDLINES     i2bin( 43)+i2bin( 2)
#define  E2_CODEPAGE            i2bin( 66)+i2bin( 2)
#define  E3_WRITEACCESS         i2bin( 92)+i2bin(32)
#define  E3_UNCALCED            i2bin( 94)+i2bin( 2)
#define  E3_SAVERECALC          i2bin( 95)+i2bin( 2)
#define  E3_GRIDSET             i2bin(130)+i2bin( 2)
#define  E3_HCENTER             i2bin(131)+i2bin( 2)
#define  E3_VCENTER             i2bin(132)+i2bin( 2)
#define  E3_COUNTRY             i2bin(140)+i2bin( 4)
#define  E3_GUTS                i2bin(128)+i2bin( 4)
#define  E3_DEFAULT_HEIGHT      i2bin(549)+i2bin( 4)
#define  E3_HIDEOBJ             i2bin(141)+i2bin( 2)
#define  E3_FONT                i2bin(561)
#define  E2_HEADER              i2bin( 20)
#define  E2_FOOTER              i2bin( 21)
#define  E2_LEFT_MARGIN         i2bin( 38)+i2bin(  8)
#define  E2_RIGHT_MARGIN        i2bin( 39)+i2bin(  8)
#define  E2_TOP_MARGIN          i2bin( 40)+i2bin(  8)
#define  E2_BOTTOM_MARGIN       i2bin( 41)+i2bin(  8)
#define  E2_PROTECT             i2bin( 18)+i2bin(  2)
#define  E3_WSBOOL              i2bin(129)+i2bin(  2)
#define  E2_BACKUP              i2bin( 64)+i2bin(  2)
#define  E2_BUILTINFORMAT       i2bin( 86)+i2bin(  2)
#define  E3_NAME                i2bin(536)
#define  E3_STYLE               i2bin(659)+i2bin( 4)
#define  E3_COLINFO             i2bin(125)+i2bin(11)

/*****************************************************************************/

#define  WKS_NAME            aWks[1]
#define  WKS_VERSION         aWks[2]
#define  WKS_HANDLE          aWks[3]
#define  WKS_UPDATE          aWks[4]
#define  WKS_TYPE            aWks[5]
#define  WKS_ROWS            aWks[6,1]
#define  WKS_COLUMNS         aWks[6,2]
#define  WKS_ALIGNMENT       aWks[7,1]
#define  WKS_CALCMODE        aWks[7,2]
#define  WKS_CALCORDER       aWks[7,3]
#define  WKS_ITERATION       aWks[7,4]
#define  WKS_PROTECTION      aWks[7,5]
#define  WKS_DATE_FORMAT     aWks[7,6]
#define  WKS_BUFFER          aWks[9]

/******* Used for existing spreadsheets only ***********/

#define  WKS_DATA_OFFSET     aWks[5,1]
#define  WKS_FILE_SIZE       aWks[5,2]
#define  WKS_TOP_ROW         aWks[6,1]
#define  WKS_TOP_COL         aWks[6,2]
#define  WKS_BOTTOM_ROW      aWks[6,3]
#define  WKS_BOTTOM_COL      aWks[6,4]
#define  WKS_GLOBAL_WIDTH    aWks[6,5]
#define  WKS_CPI             aWks[7]
#define  WKS_DATE_BITMAP     aWks[8]
#define  WKS_RANGES          aWks[9]
#define  WKS_READ_BUFFER     aWks[10,1]
#define  WKS_READ_POINTER    aWks[10,2]
#define  WKS_READ_OFFSET     aWks[10,3]
#define  WK3_GLOBAL_FORMAT   aWks[11]
#define  WKS_CELL_WIDTHS     aWks[12]

/******* Used for new spreadsheets only ***********/

#define  WKS_DATA_WRITTEN    aWks[8]
#define  WKS_SETUP           aWks[10,1]
#define  WKS_HEADER          aWks[10,2]
#define  WKS_FOOTER          aWks[10,3]
#define  WKS_RANGE           aWks[10,4]
#define  WKS_PRINT           aWks[10,5]
#define  WKS_LEFT            aWks[10,6]
#define  WKS_RIGHT           aWks[10,7]
#define  WKS_LPP             aWks[10,8]
#define  WKS_TOP             aWks[10,9]
#define  WKS_BOTTOM          aWks[10,10]
#define  WKS_BORDERS         aWks[10,11]
#define  WKS_SORT_RANGE      aWks[11,1]
#define  WKS_SORTKEY1        aWks[11,2,1]
#define  WKS_SORTKEY1_DIR    aWks[11,2,2]
#define  WKS_SORTKEY2        aWks[11,3,1]
#define  WKS_SORTKEY2_DIR    aWks[11,3,2]
#define  WKS_NAMED_RANGES    aWks[12]
#define  WKS_WIDTHS          aWks[13]
#define  WKS_FIRST_CELL      aWks[14]
#define  WKS_FORMATS         aWks[15]

#define  EXCEL_DATE_FORMAT   aWks[ 7]
#define  EXCEL_FORMATS       aWks[13]
#define  EXCEL_XF_RECORDS    aWks[ 8]
#define  EXCEL_INDEX_RECORD  aWks[14]
#define  EXCEL_FIRST_ROW     aWks[15]
#define  EXCEL_NAMED_RANGES  aWks[12]



/*** Miscellaneous default values ***/

#define DEFAULT_EXTENSION       "WK1"
#define MAX_ROWS                8192
#define MAX_COLS                 256
#define DEFAULT_WIDTH             10
#define DEFAULT_FORMAT           255
#define BUFFSIZE                4096
