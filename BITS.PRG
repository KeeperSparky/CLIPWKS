* ========================================================================= *
* BitOn   - Is the nth bit of a byte a 1?                                   *
*                                                                           *
* Parameters: <ExpC1> - byte to check                                       *
*             <ExpN1> - number of bit to check                              *
*                                                                           *
* Returns   : <ExpL>  - Is the nth bit on?                                  *
* ------------------------------------------------------------------------- *
FUNCTION BitOn(cByte, nPosn)
LOCAL nVal := INT(ASC(cByte) / (2^(8-nPosn)))
RETURN(MOD(nVal, 2) == 1)
* ========================================================================= *
* BitOff  - Is the nth bit of a byte a 0?                                   *
*                                                                           *
* Parameters: <ExpC1> - byte to check                                       *
*             <ExpN1> - number of bit to check                              *
*                                                                           *
* Returns   : <ExpL>  - Is the nth bit off?                                 *
* ------------------------------------------------------------------------- *
FUNCTION BitOff(cByte, nPosn)
RETURN(! BitOn(cByte, nPosn))
* ========================================================================= *
* BitTurnOn - Turn on the nth bit of a byte                                 *
*                                                                           *
* Parameters: <ExpC1> - byte to manipulate                                  *
*             <ExpN1> - the number of the bit to turn on                    *
*                                                                           *
* Returns   : <ExpC>  - The manipulated byte                                *
* ------------------------------------------------------------------------- *
FUNCTION BitTurnOn(cByte, nPosn)
RETURN(ByteOr(cByte, CHR(2^(8-nPosn))))
* ========================================================================= *
* BitTurnOff - Turn off the nth bit of a byte                               *
*                                                                           *
* Parameters: <ExpC1> - byte to manipulate                                  *
*             <ExpN1> - the number of the bit to turn off                   *
*                                                                           *
* Returns   : <ExpC>  - The manipulated byte                                *
* ------------------------------------------------------------------------- *
FUNCTION BitTurnOff(cByte, nPosn)
RETURN(ByteAnd(cByte, CHR(255 - (2^(8-nPosn)))))
* ========================================================================= *

* ========================================================================= *
* ByteAnd  - AND two bytes together.                                        *
*                                                                           *
* Parameters: <ExpC1> - first byte.                                         *
*             <ExpC2> - second byte.                                        *
*                                                                           *
* Returns   : <ExpC>  - the result of the AND.                              *
* ------------------------------------------------------------------------- *
STATIC function ByteAnd(cByte1, cByte2)
    LOCAL nCntr, nRetVal
    nRetVal := 0
    FOR nCntr := 1 TO 8
        nRetVal *= 2
        IF BitOn(cByte1, nCntr) .AND. BitOn(cByte2, nCntr)
            nRetVal += 1
        ENDIF
    NEXT
RETURN(CHR(nRetVal))
* ========================================================================= *
* ByteOr   - OR two bytes together.                                         *
*                                                                           *
* Parameters: <ExpC1> - first byte.                                         *
*             <ExpC2> - second byte.                                        *
*                                                                           *
* Returns   : <ExpC>  - the result of the OR.                               *
* ------------------------------------------------------------------------- *
STATIC function ByteOr(cByte1, cByte2)
    LOCAL nCntr, nRetVal
    nRetVal := 0
    FOR nCntr := 1 TO 8
        nRetVal *= 2
        IF BitOn(cByte1, nCntr) .OR. BitOn(cByte2, nCntr)
            nRetVal += 1
        ENDIF
    NEXT
RETURN(CHR(nRetVal))
