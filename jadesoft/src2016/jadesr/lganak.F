C   12/02/82 202141706  MEMBER NAME  LGANAK   (S)           FORTRAN
C   11/07/79 101072247  MEMBER NAME  LGANAL   (SOURCE)      FORTRAN
      SUBROUTINE LGANAK
C
C     S.YAMADA  31-10-78  14:35
C     LAST MODIFICATION    01-08-79  24:00  IZ=0&31 KILLED
C     LAST MODIFICATION    07-01-79  24:00  NEG ENERGY TO 20000
C                                 Y.WATANABE
C     MODIFIED AT LICEPP  08-10-79 14:25  S.YAMADA  (GO TO 80>>GO TO 85)
C     MODIFIED AT LICEPP  19-11-79 09:30  S.KOMAMIYA
C---- MAIN SUBROUTINE FOR THE 1-ST STEP L-GLASS ANALYSIS.
C---- *******  MODIFIED FOR BOS *******
C
      IMPLICIT INTEGER *2 (H)
C
#include "cdata.for"
C
#include "clgwork1.for"
C------------------------------- ADDED BY S.K. --- 07-11-79 ------------
      COMMON /MAXCLS/EIG1,EIG2,EIGSUM,EIGDIV,MAXHIT,ECMAX,MAXCL
      COMMON /EIGAB/COSD(2),COSA(2),COSB(2),
     &          EIGA1,EIGA2,EIGB1,EIGB2,ENEA,ENEB,NHITA,NHITB,DNORM
C     COMMON /CLVEC/V11,V12,V21,V22
C-----------------------------------------------------------------------
C---- NCLST = NO.OF THE LOCATED CLUSTERS.
C---- MAPCL(J)=THE INDEX OF THE FIRST MEMBER OF THE J-TH CLUSTER
C               IN THE HLGADC. (THE FIRST 51 WORDS OF CLMPRP)
C
      COMMON /CLGPRM/ ITH,MAXCLS,IRLTHD,IRLTH2,IRLTH3,ZVRTX,DZ
C
      COMMON /CLGMSB/ MSGVAL
C
      DATA NMLGCL/4HLGCL/,  LGANVR/1/,  MESS/0/
C
      IF(MESS) 1001,1000,1001
 1000 CALL LGMESG( 2, 0)
      MESS = 1
C
C
C---- FILL HEADER AND CONSTANTS IN THE GENERAL LG-INF. OF THE EVENT.
 1001 IDENT(1) = LGANVR
      IDENT(2) = IDATTM(DUM)
C
C---- CLEAR CLUSTER RESULTS ARRAY
      CALL SETSL(NCLST,0,5200,0)
C
C---- SET NO.OF WORDS/CLUSTER
      NWPCL = 15
C---- SET THE FLAG OF LG-ANALYSIS STEP-1.
      IFLAG(2) = 1
C
C*******************************************************************
C
C       LEAD GLASS CLUSTER FINDING
C
CC*   LG-ADC DATA IS MOVED FROM 'ALGN' TO /CWORK/.
CC*   CALL LGDCPY( NP, IFLAG(1))
C     LG-ADC DATA IS MOVED FROM 'ALGL' TO /CWORK/.CALIBRATED BY *5.
      CALL LGDCPK( NP, IFLAG(1))
C
C
C---- CHECK IF THERE IS ANY LG-DATA.
      IF(NP.LE.0) GO TO 100
C---- IF DATA FORMAT ERROR IS FOUND,SKIP ANALYSIS AND MAKE AN EMPTY RES.
C     IFLAG(1)=1,IF NOT CALIBRATED.
C     IFLAG(1)=2,IF NOT ALL DATA ARE COPIED DUE TO THE BUFFER SIZE.
C     IFLAG(1)=1024,IF FORMAT CONVRSION IS NOT DONE.
C
      IF(IFLAG(1).GE.1024) GO TO 100
C
C#####KILL IZ=0&31 RINGS BECAUSE THEY ARE NOT REAL.
      NHIT=LNG-3
      IF(NHIT.LE.0) GO TO 85
      DO 80 I=1,NHIT
C**********
      HPH=HLGADC(2,I)
      IF(HPH.LT.0) HLGADC(2,I)=20000
C**********
      IZ=HLGADC(1,I)
      IF(IZ.GT.2687) GO TO 80
      IZ=MOD(IZ,32)
      IF(IZ.EQ.0.OR.IZ.EQ.31) HLGADC(2,I)=0
C          PROTECTION HERE TO CORRECT NEGATIVE ENRGY DUE TO OVERFLOW.
80    CONTINUE
85    CONTINUE
C######
C---- CLUSTER FINDING    (NCLST,NCLBEC(1-3) ARE FILLED. MAP IS MADE.)
      CALL LGCCTK(IFLAG(1))
C
C---- CLUSTER POSITIONS AND ENERGIES ARE CALCULATED FOR THE BARREL AND
C     END CAPS.
      IF(NCLBEC(1)) 2,2,1
    1 CALL LGCLBK
    2 IF(NCLBEC(2)+NCLBEC(3)) 4,4,3
    3 CALL LGCLPC
C
C---- TOTAL ENERGY IS CALCULATED.
    4 ETOT(1) = ETOT(2)+ETOT(3)+ETOT(4)
C
  100 RETURN
      END
