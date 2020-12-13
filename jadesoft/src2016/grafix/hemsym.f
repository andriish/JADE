C   24/11/78 C9032101   MEMBER NAME  HEMSYM   (JADEGS)      FORTRAN
      SUBROUTINE HEMSYM(XPOS,YPOS,AGT,HBUF,KCNT,THETA)
C
C     ******************************************************************
C     *   THIS SUBROUTINE WILL DRAW A STRING OF CHARACTERS STORED      *
C     *   IN HBUF STARTING AT XPOS,YPOS.  THE CHARACTERS ARE AGT UNITS *
C     *   HIGH AND THETA SPECIFIES THE ANGLE IN RADIANS.               *
C     *   WRITTEN BY H.E.MILLS  LAST UPDATED ON 12-AUG-78 AT 16.10.    *
C     ******************************************************************
C
      IMPLICIT INTEGER*2 (H)
      DIMENSION HBUF(1),HKLM(2)
      DIMENSION HPTRS(40),HCORDS(167),HCORD1(94),HCORD2(73)
      DIMENSION HLETS(40)
      EQUIVALENCE(HCORDS(1),HCORD1(1))
      EQUIVALENCE(HCORDS(95),HCORD2(1))
C---
C---     THIS IS GIMMICKRY TO MAKE THE IBM SHIFT WORK ON INTEGER*2
C---     L. O'NEILL. 16.8.78.
C---
      EQUIVALENCE(HKLM(1),KLM)
C
      DATA HCORD1/2393,25445,24399,17731,18864,
     -            2393,4439,20144,
     -            1615,24422,25923,16737,-20480,
     -            585,22882,25428,26215,18352,
     -            6495,17251,-20480,
     -            585,22882,25436,17479,26544,
     -            844,23651,25177,18754,17999,24422,-20480,
     -            2407,18352,
     -            3164,25442,22857,16963,19525,17999,24422,25948,-20480,
     -            585,22882,26207,20294,17740,23653,-20480,
     -            343,24844,23728,
     -            327,22366,23892,17428,23386,20801,-22528,
     -            8793,18754,17999,24422,-20480,
     -            327,22373,25425,16816,
     -            6465,18271,5188,-22528,
     -            327,24340,17576,
     -            4963,25177,18754,17999,24422,-20480,
     -            327,1124,10081,-20480,
     -            337,2383,1879,-24576,
     -            585,20826,24488/
      DATA HCORD2/327,10051,3169,-20480,
     -            1857,22952,
     -            327,21607,25008,
     -            327,24935,-20480,
     -            2370,17999,24422,25177,18864,
     -            327,22366,23892,17584,
     -            2370,17999,24422,25177,18706,25008,
     -            327,22366,23892,17420,22960,
     -            585,20826,23380,19525,17999,22366,-20480,
     -            4439,1895,-20480,
     -            1858,18777,25191,-20480,
     -            1873,26544,
     -            1865,21593,26544,
     -            359,1889,-20480,
     -            4436,18215,21680,
     -            1895,16737,-20480,
     -            4694,1124,-20480,
     -            1124,-20480,
     -            -23552,
     -            1615,22366,23884,19283,4689,18762,21168/
      DATA HLETS/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,
     -           1HA,1HB,1HC,1HD,1HE,1HF,1HG,1HH,1HI,1HJ,
     -           1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR,1HS,1HT,
     -           1HU,1HV,1HW,1HX,1HY,1HZ,1H+,1H-,1H ,1H?/
      DATA HPTRS/1,6,9,14,19,22,27,34,36,45,
     -           52,55,62,67,71,75,78,84,88,92,
     -           95,99,101,104,107,112,116,122,127,134,
     -           137,141,143,146,149,152,155,158,160,161/
C
C     **** SET UP SCALES ETC ****
C
CAV
      character*8 MY
      character*1 AMYSTR(16)
      character*8 MYL
      REAL  ta
      integer le
C
      AMYSTR='                '
      
      le=0
      DO 500 I=1,KCNT
      HJ=HBUF(I)
      DO 100 K=1,40
      IF(HLETS(K).EQ.HBUF(I)) then 
      le=le+1
      call UHTOC(HLETS(K),1,AMYSTR(le),1)
      end if
 100  continue
 500  continue
CAV
      ta=tan(THETA)
      call  igset('TANG',THETA)
CAV#if defined(JEXTERNISCERNLIB)      
      call ITX(XPOS,YPOS,AMYSTR)
CAV#endif
CAV#if defined(JEXTERNISPICO)      
C      call ITXN(XPOS,YPOS,AMYSTR(1:le),le)
CAV#endif
      
      return
CAV      
      KLM=0
      SCAL=AGT/7.0
      SCAL=AGT/2.0 !AV
      IXB=0
      C=COS(THETA)
      S=SIN(THETA)
C
C     **** LOOP KCNT TIMES - PUT CHARACTER IN J ****
C
      DO 50 I=1,KCNT
      HJ=HBUF(I)
      DO 10 K=1,40
      IF(HLETS(K).EQ.HJ) GOTO 12
   10 CONTINUE
      K=40
   12 IFLAG=0
        call UHTOC(HLETS(K),1,MY,1)
      IP=HPTRS(K)
C
C     **** GET COORDINATES ****
C
   15 HKLM(2)=HCORDS(IP)
      IF(IFLAG.EQ.0) KLM=ISHFTR(KLM,8)
      H64=64
      HND2=HLAND(HKLM(2),H64)   !PMF 13/08/99 IAND2 -> HLAND
      IX=ISHFTR(IAND(KLM,56),3)
      X=IX + IXB
      Y=IAND(KLM,7)
      XX=X*C - Y*S
      YY=X*S + Y*C
      X=XPOS+SCAL*XX
      Y=YPOS+SCAL*YY
      IF(HND2.EQ.0) GOTO 20
      CALL DRAWA(X,Y)
      GOTO 30
C
   20 CALL MOVEA(X,Y)
   30 IF(IAND(KLM,128).NE.0) GOTO 40
      IF(IFLAG.NE.0) GOTO 35
      IFLAG=1
      GOTO 15
C
   35 IP=IP+1
      IFLAG=0
      GOTO 15
C
   40 IXB=IXB+IX
   50 CONTINUE
      RETURN
      END
