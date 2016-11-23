      SUBROUTINE TWOIN1(IN,IOUT1,IOUT2)
*
*   The 4byte integer IN  is split up in the two I*2 integers IOUT1,2
*         IOUT1 is the higher 16 bits, IOUT2 the lower 16 bits
*               IOUT1,IOUT2    IOUT3,IOUT4   IOUT5,IOUT6
*                  IN1               IN2        IN3
*     (corresponds to the standard in JADE software, e.g. the HEAD bank)
*
*         J.Olsson   13.12.2005
*
      INTEGER IUPP,ILOW
      DATA IUPP,ILOW /'FFFF0000'X,'0000FFFF'X/
*
* ---------  CODE  --------------
*
      IOUT1 = IAND(IN,IUPP)
      IOUT2 = IAND(IN,ILOW)
*
      IOUT1 = ISHFT(IOUT1,-16)
*
      RETURN
      END
