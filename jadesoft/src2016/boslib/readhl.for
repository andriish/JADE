C   07/06/96 606071907  MEMBER NAME  READHL   (S4)          FORTRAN
      SUBROUTINE READHL(LC)
C     BOS SUBPROGRAM =3.2=
      COMMON/CONTEX/LABL,NSL,NF,FR(36),NB,LBT(72)
      CHARACTER*1 LC(80),LBT,LLAB(4),LH(4),LBLK/1H /
      INTEGER NH/0/,BLK/4H    /,LABL,NBT(18)
      REAL*8 ZZ,TEN/1.0D1/
      EQUIVALENCE (LABL,LLAB(1)),(NH,LH(1)),(NBT(1),LBT(1)),
     1   (IZL,FZL)
C
C     PURPOSE
C        CONVERT TEXT INTO NUMBERS
C
C     INPUT
C        LC( )  = ARRAY OF 72 CHARACTERS (A1)
C
C     RESULT
C        LABL   = LABEL, I.E. FIRST FOUR NONNUMERIC NONBLANK
C                 CHARACTERS OF LC( )
C        NSL    = 1, IF SLASH (/) AFTER FIRST NUMBER, OTHERWISE 0
C        NF     = NUMBER OF ENTRIES IN ARRAY FR( )
C        FR( )  = ARRAY OF NUMBERS (FLOATING POINT OR INTEGER)
C        NBT    = NUMBER OF BYTES IN ARRAY LBT( )
C        LBT( ) = ARRAY OF BYTES WITH TEXT AFTER APOSTROPH ('),
C                 FIRST AND LAST ARE NOT EQUAL BLANK
C
C     EXAMPLE
C        LC ( ) = NAME 1 / 3.1 4.9E01 'COMMENT
C     WILL RESULT IN
C        LABL   = 'NAME'
C        NSL    = 1
C        NF     = 3
C        FR(1)  = 1
C        FR(2)  = 3.1
C        FR(3)  = 49.0
C        NBT    = 7
C        LC(1)  = 'C'      ETC    LC(7)  = 'T'
C
      LABL=BLK
      DO 2 I=1,18
    2 NBT(I)=BLK
      NF =0
      NB =0
      NSL=0
      NZ=0
      LAB=0
      LJ=0
      LZL=0
      NV=0
      NI=0
C
      DO 40 I=1,73
      J=1
      IF(I.EQ.73) GOTO 20
C     UNPACK CHARACTER LC(I) INTO NH
      LH(4)=LC(I)
C     NUMBER
      IF(NH.GE.240.AND.NH.LE.249) J=2
C     DOT
      IF(NH.EQ. 75) J=3
C     PLUS - SIGN
      IF(NH.EQ. 78) J=4
C     MINUS - SIGN
      IF(NH.EQ. 96) J=5
C     ' (APOSTROPH)
      IF(NH.EQ.125) J=6
C     / (SLASH)
      IF(NH.EQ. 97) J=7
C     E (EXPONENT)
      IF(NH.EQ.197) J=8
C     BLANK
      IF(NH.EQ.64)  J=9
C
   10 GOTO(12,14,18,20,20,20,12,12,20),J
C     LABEL
   12 IF(LAB.GE.4) GOTO 20
      LAB=LAB+1
      LLAB(LAB)=LC(I)
      GOTO 40
C     NUMBER
   14 IF(NZ.NE.0) GOTO 16
      LAB=4
      ZZ=0.0D0
      NE=0
      NP=0
      IF(LJ.EQ.3) NP=1
   16 NZ=NZ+1
      ZZ=ZZ*TEN+DFLOAT(NH-240)
      NE=NE+NP
      GOTO 40
C     DOT
   18 IF(NZ.EQ.0.OR.NP.EQ.1) GOTO 20
      NP=1
      GOTO 40
C
   20 IF(NZ.EQ.0) GOTO 30
      IF(NP.EQ.0) GOTO 22
C     FLOATING POINT NUMBER
      FZL=ZZ/TEN**NE
      IF(NV.EQ.5) FZL=-FZL
      LZL=1
      GOTO 26
C     INTEGER NUMBER
   22 IZL=ZZ+DBLE(0.5)
      IF(NV.EQ.5) IZL=-IZL
      IF(NI.NE.8.OR.LZL.NE.1) GOTO 24
      FR(NF)=FR(NF)*TEN**IZL
      LZL=0
      GOTO 28
C     STORE NUMBER
   24 LZL=0
   26 NF=NF+1
      FR(NF)=FZL
   28 NZ=0
      NV=0
      NI=0
C
   30 GOTO (34,40,40,32,32,50,34,34,40),J
   32 NV=J
      GOTO 40
   34 NV=0
      NI=NI*10+J
      IF(NF.EQ.1.AND.NI.EQ.7) NSL=1
   40 LJ=J
      GOTO 100
   50 IA=I+1
      DO 52 I=IA,72
      IF(LC(I).NE.LBLK) GOTO 54
   52 CONTINUE
      GOTO 100
   54 IA=I
      J=0
      DO 56 I=IA,72
      J=J+1
      IF(LC(I).NE.LBLK) NB=J
      LBT(J)=LC(I)
   56 CONTINUE
C
  100 RETURN
      END
