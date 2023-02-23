************************************************************************
* BIBY
* ====
*
*     Emulation of IBM/370 FORTRAN intrinsic functions and of former 
*     DESYLIB bit and byte manipulation routines.
*
*     08/01/98  P.A. Movilla Fernandez
*
*     09/06/99  PMF     HLAND, HLOR, HINT, MACHINE added
*                       MVCL, SETSL modified
*     25/06/99  PMF     MVCL2, MVC2 added
*     09/12/99  PMF     LCOMPL  added
*
*
*     The original routines are ported to Fortran using CERNLIB routines
*     contained in the package BITBYT (M421). 
*     
*     original   present
*     procedure  release     type    CERNLIB routines used
*     --------   -------     ----    ---------------------
*     LAND       LAND(I,J)    I*4    MBYTET
*               HLAND(I,J)    I*2    MBYTET
*     LOR         LOR(I,J)    I*4    MBYTOR
*                HLOR(I,J)    I*2    MBYTOR
*     LXOR       LXOR(I,J)    I*4    SBIT0,SBIT1,JBIT
*     AND         AND(I,J)    R*4    MBYTET
*     OR           OR(I,J)    R*4    MBYTOR
*     LCOMPL   LCOMPL(I,J)    I*4
*     BITON    IBITON(I,J)    I*4    MSBIT0
*     BITOFF  IBITOFF(I,J)    I*4    MSBIT1
*     TBIT       TBIT(I,J)    L*4    JBIT
*     SHFTL    ISHFTL(I,J)    I*4    CBYT
*     SHFTR    ISHFTR(I,J)    I*4    CBYT
* 
*     MVB         MVB                CBYT
*     MVCL        MVCL               CBYT
*                 MVCL2              CBYT,UCOPY
*     MVC         MVC                ->MVCL
*                 MVC2               ->MVCL2
*     SETSL       SETSL              CBYT
*     SETS        SETS               ->SETSL
*
*     Help routines:
*     HINT(I)                 I*2
*     MACHINE
* 
*************************************************
      INTEGER FUNCTION LAND(IW,JW)
      IMPLICIT NONE
*
*  Usage: IX = LAND(IW,JW)
*         
*         IW,JW: INTEGER*4
*         IX = logical AND of IW and JW
*
      INTEGER IW,JW,MBYTET
C
      LAND = MBYTET(IW,JW,1,32)
C
      RETURN
      END
*************************************************
      INTEGER FUNCTION LOR(IW,JW)
      IMPLICIT NONE
*
*  Usage: IX = LOR(IW,JW)
*          
*         IW,JW:  INTEGER*4
*         IX = logical OR of IW and JW
*
      INTEGER IW,JW,MBYTOR
C
      LOR = MBYTOR(IW,JW,1,32)
C
      RETURN
      END
*************************************************
      INTEGER FUNCTION LXOR(IW,JW)
      IMPLICIT NONE
*
*  Usage: IX = LXOR(IW,JW)
*         
*         IW,JW: INTEGER*4
*         IX = logical EXCLUSIVE OR of IW and JW
*
      INTEGER IW,JW,I
      INTEGER JBIT 
C
      DO 10 I = 1, 32
         IF (   (JBIT(IW,I).EQ.1).AND.(JBIT(JW,I).EQ.0) 
     +      .OR.(JBIT(IW,I).EQ.0).AND.(JBIT(JW,I).EQ.1) ) THEN
            CALL SBIT1(LXOR,I)
         ELSE
            CALL SBIT0(LXOR,I)
         ENDIF
 10   CONTINUE
C
      RETURN
      END
************************************************
      INTEGER FUNCTION LCOMPL(IW)
      IMPLICIT NONE
*
* Usage: IX = LCOMPL(IW)
*
*        IW: INTEGER*4
*        IX = binary 1-complement of IW
*
      INTEGER IW,IZ
      DATA IZ/ZFFFFFFFF/
C
      LCOMPL=IZ-IW
C
      RETURN
      END
************************************************
      REAL FUNCTION AND(IW,JW)
      IMPLICIT NONE
*
*  Usage: X = AND(A,B)
*         
*         A,B: INTEGER*4, REAL*4
*
*     X = logical AND of A and B
*
*
      INTEGER IW,JW,MBYTET
      INTEGER JAND
      REAL RAND
      EQUIVALENCE(JAND,RAND)
C
      JAND=MBYTET(IW,JW,1,32)
      AND=RAND
C
      RETURN
      END
************************************************
      REAL FUNCTION OR(IW,JW)
      IMPLICIT NONE
*
*  Usage: X = OR(A,B)
*         
*         A,B: INTEGER*4, REAL*4
*         X = logical OR of A and B
*
      INTEGER IW,JW,MBYTOR
      INTEGER JOR
      REAL ROR
      EQUIVALENCE(JOR,ROR)
C
      JOR=MBYTOR(IW,JW,1,32)
      OR=ROR
C
      RETURN
      END
************************************************
      INTEGER*2 FUNCTION HLAND(I,J)
      IMPLICIT NONE
*
*  Usage: HX = HLAND(I,J)
*
*         I,J: INTEGER*2
*         HX = logical AND of I and J
*
*  HLAND uses S/R MACHINE to check the endian convention.
*
      INTEGER MBYTET
      INTEGER*2 I,J
      INTEGER*2 II(2),JJ(2)
      DATA II(1)/0/,JJ(1)/0/
C Byteorder from subroutine MACHINE:
      INTEGER ITYPE,IST
      COMMON / CIST / ITYPE,IST(4)
C
      IF( ITYPE.EQ.0 ) CALL MACHINE
      II(ITYPE)=I
      JJ(ITYPE)=J
      HLAND=MBYTET(II,JJ,1,32)
C
      RETURN
      END
************************************************
      INTEGER*2 FUNCTION HLOR(I,J)
      IMPLICIT NONE
*
*  Usage: HX = HLOR(I,J)
*
*         I,J: INTEGER*2
*         HX = logical OR of I and J
*
*  HLOR uses S/R MACHINE to check the endian convention.
*
      INTEGER MBYTOR
      INTEGER*2 I,J
      INTEGER*2 II(2),JJ(2)
      DATA II(1)/0/,JJ(1)/0/
C Byteorder from subroutine MACHINE:
      INTEGER ITYPE,IST
      COMMON / CIST / ITYPE,IST(4)
C
      IF( ITYPE.EQ.0 ) CALL MACHINE
      II(ITYPE)=I
      JJ(ITYPE)=J
      HLOR=MBYTOR(II,JJ,1,32)
C
      RETURN
      END
************************************************
      INTEGER FUNCTION IBITOFF(IW,J)
      IMPLICIT NONE
*
*  Usage: IX = IBITOFF(IW,J)
*
*         IW,J: INTEGER*4
*         
*     Returns IW in IX with bit J set to 0.    
*     IW is also modified.
*     J = 0 ... 31
* NB: IBM Convention
*     J=0  : most significant bit
*     J=31 : least significant bit
*
      INTEGER IW,J,MSBIT0
C
      IW = MSBIT0(IW,32-J)
      IBITOFF = IW
C
      RETURN
      END
*************************************************
      INTEGER FUNCTION IBITON(IW,J)
      IMPLICIT NONE
*
*  Usage: IX = IBITON(IW,J)
*
*         IW,J: INTEGER*4
*         
*     Returns IW in IX with bit J set to 1    
*     IW is also modified.
*     J = 0 ... 31
* NB: IBM Convention
*     J=0  : most significant bit
*     J=31 : least significant bit
*
      INTEGER IW,J,MSBIT1
C
      IW = MSBIT1(IW,32-J)
      IBITON = IW
C
      RETURN
      END
************************************************
      LOGICAL FUNCTION TBIT(IW,J)
      IMPLICIT NONE
*
*  Usage: LX = TBIT(IW,J)
*
*         IW, J: INTEGER
*         
*         LX = .TRUE.  if bit J is set in IW 
*         LX = .FALSE. if bit J is not set in IW 
*         J = 0 ... 31
* NB: IBM Convention
*     J=0  : most significant bit
*     J=31 : least significant bit
*
      INTEGER IW,J,JBIT
C
      IF (JBIT(IW,32-J).EQ.1) TBIT = .TRUE.
      IF (JBIT(IW,32-J).EQ.0) TBIT = .FALSE.
C
      RETURN
      END
************************************************
      INTEGER FUNCTION ISHFTLXXX(IW,K)
      IMPLICIT NONE
*
*  Usage: IX = ISHFTL(IW,K)
*
*     IW,K: INTEGER
*     Returns IW in IX with the bits in IW 
*     shifted left by K units. The upper bits 
*     are lost, the lower bits are filled with 0.
*
      INTEGER IW,K
C
      IF (K.LT.0.OR.K.GT.32) THEN
         WRITE(6,1001) K
 1001    FORMAT(' ISHFTL bad argument !!   Nr. of shifts ',I9)
         STOP
      ENDIF
C
      ISHFTL=0
C      K=MOD(K,64)
      CALL CBYT(IW,1,ISHFTL,K+1,32-K)
C
      RETURN
      END
************************************************
      INTEGER FUNCTION ISHFTRXXX(IW,K)
      IMPLICIT NONE
*
*  Usage: IX = ISHFTR(IW,K)
*
*     IW,K: INTEGER
*     Returns IW in IX with the bits in IW
*     shifted right by K units. The lower bits
*     are lost, the upper bits are filled with 0.
*
      INTEGER IW,K
C
      IF (K.LT.0.OR.K.GT.32) THEN
         WRITE(6,1001) K
 1001    FORMAT(' ISHFTR bad argument !!   Nr. of shifts ',I9)
         STOP
      ENDIF
C
      ISHFTR=0
C      K=MOD(K,64)
      CALL CBYT(IW,K+1,ISHFTR,1,32-K)
C
      RETURN
      END
************************************************
      SUBROUTINE MVB(A,NA,B,NB,M)
      IMPLICIT NONE
*     
*  MVB calling sequence is
*    CALL MVB (A,      NA,     B,      NB,      M)
*              Target  start   origin  start    Bits to be copied
*      Start is the first bit of the range to be copied!
*
*      NA, NB = 0 ... 31
*
      INTEGER A,B,NA,NB,M
C
      IF (M.LE.0.OR.M.GT.32) THEN
         WRITE(6,1001) M
 1001    FORMAT(' MVB bad argument !!   Nr. of bits ',I9)
         STOP
      ENDIF
C
      CALL CBYT(B,NB+1,A,NA+1,M)
C
      RETURN
      END
************************************************
      SUBROUTINE MVC(A,NA,B,NB,M)
      IMPLICIT NONE
*
*  MVC calling sequence is
*   CALL MVC (A,      NA,     B,      NB,      M)
*              Target  start   origin  start    Bytes to be copied
*      Start is the last byte BEFORE the range to be copied!
*
      INTEGER A(1),B(1),NA,NB,M
C
      CALL MVCL(A,NA,B,NB,M)
C
      RETURN
      END
************************************************
      SUBROUTINE MVC2(A,NA,B,NB,M)
      IMPLICIT NONE
*
*  MVC calling sequence is
*   CALL MVC2 (A,      NA,     B,      NB,      M)
*              Target  start   origin  start    Bytes to be copied
*      Start is the last byte BEFORE the range to be copied!
*
      INTEGER*2 A(1),B(1)
      INTEGER NA,NB,M
C
      CALL MVCL2(A,NA,B,NB,M)
C
      RETURN
      END
************************************************
      SUBROUTINE MVCL(A,NA,B,NB,M)
      IMPLICIT NONE
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
*            PMF         25.06.99 last modified
*            PMF         16.05.00 last mod
*
      INTEGER A(1),B(1),NA,NB,M
      INTEGER KA,KB,JAW,JBW,IAB,IBB
C Byteorder from subroutine MACHINE:
      INTEGER IST,ITYPE
      COMMON / CIST / ITYPE,IST(4)
C
      IF( ITYPE.EQ.0 ) CALL MACHINE
C
      IF(M.LE.0) THEN 
         WRITE(6,1001) M
 1001    FORMAT(' MVCL bad argument !!   Nr. of bytes ',I9
     +         ,', will not copy.' )
         GOTO 9999
      ENDIF
      IF(NA.LT.0) THEN 
         WRITE(6,1002) NA
 1002    FORMAT(' MVCL bad argument !!   Target address ',I9)
         STOP
      ENDIF
      IF(NB.LT.0) THEN 
         WRITE(6,1003) NB
 1003    FORMAT(' MVCL bad argument !!   Source address ',I9)
         STOP
      ENDIF
C
      KA = NA	
      DO 100 KB = NB+1,NB+M
         KA = KA + 1 
         JAW = (KA-1)/4 + 1
         JBW = (KB-1)/4 + 1
         IAB = IST(MOD(KA-1,4) +1)
         IBB = IST(MOD(KB-1,4) +1)
         CALL CBYT(B(JBW),IBB,A(JAW),IAB,8)
 100  CONTINUE
C
 9999 RETURN
      END
************************************************
      SUBROUTINE MVCL2(A,NA,B,NB,M)
      IMPLICIT NONE
*
*  MVCL2 calling sequence is
*   CALL MVCL2 (A,      NA,     B,      NB,      M)
*              Target  start   origin  start    Bytes to be copied
*      Start is the last byte BEFORE the range to be copied!
*
* This is a modified version of MVCL working also with *misaligned* 
* data arrays arguments, as may be sometimes the case. Use MVCL2 instead
* of MVCL if one array argument is a half integer array with even index!
*
* The adressed part of the original data arrays A, B are
* put into the extra arrays HA, HB with the desired working memory
* alignment. The copy procedure is then performed with these arrays, 
* with the results transferred to respective part of the original 
* target array. 
*
* MVCL2 handles both integer and half integer arguments correctly, 
* but the faster routine MVCL is recommended in the normal case.
*
* -----
* NB.: It is not possible to relate the arrays IA, IB used as arguments 
* for CBYT to HA, HB via the EQUIVALENCE statement.
* -----
      INTEGER*2 A(1),B(1)
      INTEGER NA,NB,M,I
      INTEGER*2 HA((NA+M+1)/2),HB((NB+M+1)/2)
      INTEGER IA((NA+M+3)/4),IB((NB+M+3)/4)
C     
      INTEGER KA,KB,JAW,JBW,IAB,IBB
C Byteorder from subroutine MACHINE:
      INTEGER IST,ITYPE
      COMMON / CIST / ITYPE,IST(4)
C
      IF( ITYPE.EQ.0 ) CALL MACHINE
C
      IF(M.LE.0) THEN 
         WRITE(6,1001) M
 1001    FORMAT(' MVCL bad argument !!   Nr. of bytes ',I9)
         STOP
      ENDIF
      IF(NA.LT.0) THEN 
         WRITE(6,1002) NA
 1002    FORMAT(' MVCL bad argument !!   Target address ',I9)
         STOP
      ENDIF
      IF(NB.LT.0) THEN 
         WRITE(6,1003) NB
 1003    FORMAT(' MVCL bad argument !!   Source address ',I9)
         STOP
      ENDIF
C
      DO 40 I=1,INT((NB+M+1)/2)
 40      HB(I)=B(I)
      DO 50 I=1,INT((NA+M+1)/2)
 50      HA(I)=A(I)
      CALL UCOPY(HB,IB,(NB+M+3)/4)
      CALL UCOPY(HA,IA,(NA+M+3)/4)
C
      KA = NA   
      DO 100 KB = NB+1,NB+M
         KA = KA + 1 
         JAW = (KA-1)/4 + 1
         JBW = (KB-1)/4 + 1
         IAB = IST(MOD(KA-1,4) +1)
         IBB = IST(MOD(KB-1,4) +1)
         CALL CBYT(IB(JBW),IBB,IA(JAW),IAB,8)
 100  CONTINUE
C
      CALL UCOPY(IA,HA,(NA+M+3)/4)
      DO 150 I=1,INT((NA+M+1)/2)
 150     A(I)=HA(I)
C
      RETURN
      END
************************************************
      SUBROUTINE SETS(A,NA,M,C)
      IMPLICIT NONE
*
*  SETS calling sequence is
*   CALL SETS (A,      NA,     M,                  C)
*              target  start   Bytes in target     origin     
*                              to be overwritten 
*
*      Start is the last byte BEFORE the range to be copied!
*
*   Example:   CALL SETS(A,56,12,C)  copies C(1) into A(15),A(16),A(17)
*
      INTEGER A,C,NA,M
C
      CALL SETSL(A,NA,M,C)
C
      RETURN
      END
************************************************
      SUBROUTINE SETSL(A,NA,M,C)
      IMPLICIT NONE
*
*  SETSL calling sequence is
*   CALL SETSL (A,      NA,     M,                  C)
*               target  start   Bytes in target     origin     
*                               to be overwritten 
*
*      Start is the last byte BEFORE the range to be copied!
*
*   Example:   CALL SETSL(A,56,12,C)  copies C(1) into A(15),A(16),A(17)
*
      INTEGER   A(1),NA,M
      INTEGER*1 C(1)
      INTEGER KA,JAW,IAB
C Byteorder from subroutine MACHINE:
      INTEGER IST,ITYPE
      COMMON / CIST / ITYPE,IST(4)
C
      IF( ITYPE.EQ.0 ) CALL MACHINE
C
      IF(M.LE.0) THEN 
         WRITE(6,1001) M
 1001    FORMAT(' SETSL bad argument !!   Nr. of bytes ',I9)
         STOP
      ENDIF 
      IF(NA.LT.0) THEN 
         WRITE(6,1002) NA
 1002    FORMAT(' SETSL bad argument !!   Target address ',I9)
         STOP
      ENDIF
C
      DO 100 KA = NA+1 ,NA+M 
         JAW = (KA-1)/4 + 1
         IAB = IST(MOD(KA-1,4) +1)
         CALL CBYT(C(1),1,A(JAW),IAB,8)
 100  CONTINUE
C
      RETURN
      END
************************************************
      INTEGER*2 FUNCTION HINT(I)
      IMPLICIT NONE
*
* Usage: H = HINT(I)
*
*        I: INTEGER*4
*        H: INTEGER*2
*
*     Performs a conversion of an 
*     INTEGER*4 to an INTEGER*2
*        
*
      INTEGER I
C
      HINT=I
C
      RETURN
      END
************************************************
      SUBROUTINE MACHINE
      IMPLICIT NONE
*
* Usage: CALL MACHINE
*
*     Returns in ITYPE the present type of byte order 
*     in an INTEGER*4 word
*     COMMON / CIST / IST(4) contains the byte order
*     realised on the present platform;
*     IST(1...4) = bit numbers of 4 consecutive bytes
*                  in a word (lsb/msb)
*
*      ITYPE = 1: IBM-like byte order
*      ITYPE = 2: DEC/ALPHA-like byte order
*      
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      SAVE FIRST
C
      INTEGER IBYTE
      INTEGER*2 HBYTE(2)
      EQUIVALENCE (HBYTE(1),IBYTE)
      DATA IBYTE/1/
C
      INTEGER IST,ITYPE
      COMMON / CIST / ITYPE,IST(4)
C
      IF(HBYTE(1).EQ.0) THEN
         ITYPE=2
         IST(1) = 25
         IST(2) = 17
         IST(3) = 9
         IST(4) = 1
         IF( FIRST ) THEN  
            PRINT *,'* MACHINE: Big endian (IBM like), ITYPE=',ITYPE
            FIRST = .FALSE.
         ENDIF
      ELSEIF(HBYTE(2).EQ.0) THEN
         ITYPE=1
         IST(1) = 1
         IST(2) = 9
         IST(3) = 17
         IST(4) = 25
         IF( FIRST ) THEN
            PRINT *,'* MACHINE: Little endian, ITYPE=',ITYPE
            FIRST = .FALSE.
         ENDIF
      ELSE
         ITYPE=-1
         PRINT *,'* MACHINE: Unknown type, ITYPE=',ITYPE
         STOP
      ENDIF
C
      RETURN
      END
      BLOCK DATA CISTBLK
      IMPLICIT NONE
      INTEGER ITYPE,IST
      COMMON / CIST / ITYPE,IST(4)
      DATA ITYPE /0/
      END
***********************************************
