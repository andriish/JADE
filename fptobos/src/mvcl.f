      SUBROUTINE MVCL(A,NA,B,NB,M)
*
*  MVCL is a DESYLIB routine, originally coded in IBM assembler
*  MVCL is here ported to Fortran, using the CERN-lib routine CBYT 
*  CBYT is contained in the package BITBYT (M421)
*
*  MVCL calling sequence is
*   CALL MVCL (A,      NA,     B,      NB,      M)
*              Target  start   origin  start    Bytes to be copied
*      Start is the last byte BEFORE the range to be copied!
*
*   Example:   CALL MVCL(A,56,B,0,40)  copies B(1:10) into A(15:24)
*
*  The CERN routine CBYT (package M421, BITBYT) has the calling sequence
*   CALL CBYT (IA,      JA,         IX,        J,         NBITS)
*              origin   Byte LSB    Target     Byte LSB   bits/byte
*
*   Example:   CALL CBYT(IA,9,IX,17,16) copies 2 middle bytes of word IA
*                                       into 2 upper bytes of word IX.
*
*   Author:  J. Olsson   28.12.97
*
*****************************
      INTEGER A(1),B(1)
      DIMENSION IST(4)
      DATA IST /1,25,17,9/
      DATA MERRO /0/, MERRP /0/, MERRQ/0/
****************************
      IF(M.LE.0) THEN 
        MERRO = MERRO + 1
        IF(MERRO.LT.6) THEN
          WRITE(6,1001) M
1001      FORMAT(' MVCL bad argument !!   Nr. of bytes ',I9)
        ENDIF
        GO TO 9999
      ENDIF 
      IF(NA.LT.0) THEN 
        MERRP = MERRP + 1
        IF(MERRP.LT.6) THEN
          WRITE(6,1002) NA
1002      FORMAT(' MVCL bad argument !!   Target address ',I9)
        ENDIF
        GO TO 9999
      ENDIF
      IF(NB.LT.0) THEN 
        MERRQ = MERRQ + 1
        IF(MERRQ.LT.6) THEN
          WRITE(6,1003) NB
1003      FORMAT(' MVCL bad argument !!   Source address ',I9)
        ENDIF
        GO TO 9999
      ENDIF
**
      KA = NA
      DO 100 KB = NB+1,NB+M
       KA = KA + 1 
       JAW = (KA-1)/4 + 1
       JBW = (KB-1)/4 + 1
       IAB = IST(MOD(KA,4) +1)
       IBB = IST(MOD(KB,4) +1)
       CALL CBYT(B(JBW),IBB,A(JAW),IAB,8)
 100  CONTINUE
*
9999  CONTINUE
      RETURN
      END
       
