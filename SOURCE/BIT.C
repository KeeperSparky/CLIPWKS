/*******************************************************************************
* Program Id: bit.c
*    Version: 1.00
********************************************************************************
*
* Purpose: Sets the given bit in a passed bit string.  Returns the previous
*          value.  Be sure to pass the string by reference.  NOTE.  In order
*          to stay as fast as possible, minimal parameter checking is
*          performed.  It is up to the user to not be too stupid.
*
* Syntax:  bit( <OptC String>, <OptN (1...n) Offset> [, <OptL Set/Clear>] )
*
********************************************************************************
#include <extend.h>

CLIPPER bit( void )
{
   unsigned char   mask,
                  *ptr;
   unsigned int    loc,
                   offset = _parni( 2 ) - 1,
                   res    = 0;

   loc = offset / 8;
   if ( loc < _parclen( 1 ) )
   {
      ptr = _parc( 1 ) + loc;
      loc = offset % 8;
      res = *ptr << loc & 0x80;

      if ( PCOUNT > 2 )
      {
         mask = (unsigned char ) 0x80 >> loc;
         if ( _parl( 3 ) )
            *ptr = *ptr | mask;
         else
            *ptr = *ptr & ~mask;
      }
   }
   _retl( res );
}
