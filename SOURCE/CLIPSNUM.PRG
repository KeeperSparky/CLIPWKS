*****************************************************************************
*   Program:  CLIPSNUM.PRG                                                  *
*    Author:  Joseph D. Booth                                               *
*   Purpose:  Xbase++ code for Lotus short numbers                          *
*****************************************************************************
STATIC aTable := { 5000,500,.05,.005,.0005,.00005,.0625,.015625 }

//
//   Function:  L_Snum()
//    Purpose:  Convert a Lotus short number string to a numeric value
//
//////////////////////
function L_Snum(cStr)
LOCAL x,y,z

if ! bit(cStr,8)                 // Bit 8 determines how number is stored
   x := bin2i(cStr)/2
else
   y := if(bit(cStr,7),1,0)+;
           if(bit(cStr,6),2,0)+;
              if(bit(cStr,5),4,0)
   z := aTable[y+1]
   x := z * int(bin2i(cStr)/16)
endif
return x
