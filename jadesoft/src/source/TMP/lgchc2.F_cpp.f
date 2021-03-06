C   25/07/79 006070613  MEMBER NAME  LGCHC2   (SOURCE)      FORTRAN
      SUBROUTINE LGCHC2
C
C     Y.WATANABE 25-07-79  03:50    VERSION 2,  COPIED FROM LGCHC2
C     LAST MODIFICATION 20-09-79  16:10 CXDCHG DIM=30 TO 100
C
C---- CHARGED PARTICLE CLUSTER IN THE LEAD GLASS COUNTER IS GENEATED
C     TO CONNECT THE INNER TRACK AND THE LG CLUSTER.
C     USED IN THE 2-ND STEP LG ANALYSIS.
C
C---- VERSION 2 INCLUDES THE EFFECT OF MULTIPLE SCATTERING IN THE COIL.
C     EXAMINES ALL THE INNER TRACKS.THE TRACKS WHICH DO NOT HIT THE
C     LEAD GLASS COUNTER ARE REGISTERED WITH EMPTY CLUSTERS.
C
      IMPLICIT INTEGER *2 (H)
C
      COMMON /CLGCHG/ NCHIND,NSTEP,CXDCHG(9,100)
      DIMENSION JBCCHG(9,100)
      EQUIVALENCE (CXDCHG(1,1),JBCCHG(1,1))
C---- CXDCHG  CONTAINS INNER TRACK INFORMATION
C     JBCCHG(1,N)     HITTING PART 0=BARREL, +/-1=+/-Z END CAP
C     CXDCHG(2,N)     CHARGE
C     CXDCHG(3-5,N)   HITTING POSITION ON THE COIL OR ON THE END CAP
C     CXDCHG(6-8,N)   DIRECTION COSIGNS
C     CXDCHG(9,N)     ABSOLUTE MOMENTUM IN GEV/C
C
C     USED IN LGCDIR PART OF ANALYSIS
C     INCREASE UP TO 100 TRACKS ON 20/9/79
      COMMON /CWORK/ NCHCLS,NPOINT,MAPCCL(101),HCLADR(1600),
     $               NCHCL2,HCLIST(4,100),  NCLST2,HCLLSO(4,80)
      DATA NPMAX/1600/
C---- /CWORK/ STORES CLUSTERS EXPECTED FOR THE CHARGED TRACKS.
C     NCHCLS=NCHIND, MAPCCL(J)=START INDEX OF CLUSTER DATA IN HCLADR
C                              FOR THE J-TH CHARGED TRACK
C
      COMMON /CLGDMS/ X0,RADIUS(6),RADSX0(6),THX0(4),
     1                ZEND(2),ZENDX0(2),ZWID(2),ZGAP(2),PHWID(2),
     2                ZECAP(4),ZECAPX(4),THECPX(2),
     3                ECXLST(24), EXYLST(24)
C
      DATA INIT/0/
      DIMENSION XZ(3,2),RAD2(2),DXY(2),XY(2,4)
C
      COMMON/ CLGVRN/ NVRSN(20)
      DATA NVCODE/379072623/
C
      IF(INIT.GT.0) GO TO 10
      NVRSN(11) = NVCODE
      INIT=1
      RAD2(1)=RADIUS(3)**2
      RAD2(2)=RADIUS(4)**2
      DEL=(RADIUS(1)+RADIUS(2))/2.
      DEL=(RADIUS(3)-DEL)*0.04
      BDR2=(RADIUS(4)-RADIUS(3))**2
      EDR2=THECPX(1)**2
      PHIDEV=RADIUS(3)*6.283184/84.
      DLBMIN=ZWID(1)
      DLEMIN=140.
      ZDEV=(ZEND(2)-ZEND(1)+ZGAP(1))/32.
      ZMEND=ZEND(1)-ZGAP(1)/2.
C     WRITE(6,590) DLBMIN,DLEMIN,DEL
C590  FORMAT('0 LGCDIR/LGCHC2. WINDOW SIZE FOR BARREL AND END CAP',
C    1  ' (CHGD TRACK TO LG) IN MM',5F12.4)
10    CONTINUE
C
      NPOINT = 0
      NCHCLS = NCHIND
      IF(NCHIND.EQ.0) RETURN
C
      IF(NCHIND.GT.100) NCHIND = 100
      DO 300 N=1,NCHIND
C
C---- MAP FOR THE CHARGE CLUSTERS
      MAPCCL(N) = NPOINT+1
C
      IBE=JBCCHG(1,N)
C     WRITE(6,600) N,IBE,(CXDCHG(J,N),J=2,9)
C600  FORMAT(' LGCHC2',2I5,10F10.4)
      IF(IABS(IBE).GT.1) GO TO 300
      IF(IBE) 200,100,200
C
C
C--------------   THE BARREL PART  ---------------------------
C----   TRACE TO THE FRONT SURFACE OF THE LEAD GLASS.
100   CONTINUE
      A=CXDCHG(6,N)**2+CXDCHG(7,N)**2
      B=CXDCHG(3,N)*CXDCHG(6,N)+CXDCHG(4,N)*CXDCHG(7,N)
      C=CXDCHG(3,N)**2+CXDCHG(4,N)**2
      B2=B*B
      DO 110 I=1,2
      XX=B2-A*(C-RAD2(I))
      IF(XX.LT.0.) XX=0.
      XX=SQRT(XX)
      TI=(XX-B)/A
      IF(XX.EQ.0.) TI=0.
      DO 110 J=1,3
      XZ(J,I)=CXDCHG(J+2,N)+TI*CXDCHG(J+5,N)
110   CONTINUE
C     WRITE(6,610) XZ
C610  FORMAT(' XZ',10F10.4)
C
C     UPDATE CXDCHG
      DO 112 J=1,3
112   CXDCHG(J+2,N)=(XZ(J,1)+XZ(J,2))/2.
      NSTEP=2
C     CHANGE TO PHI AND Z
      DO 120 I=1,2
      PHI=ATAN2(XZ(2,I),XZ(1,I))+6.283184
C     AVOID USING NEGATIVE NUMBERS
      XZ(1,I)=PHI
      XZ(2,I)=XZ(3,I)
120   CONTINUE
C     BE CAREFUL WITH THE PHI=0 BOUNDARY
      IF(ABS(XZ(1,1)-XZ(1,2)).LT.3.14159266) GO TO 130
      IF(XZ(1,1)-XZ(1,2)) 122,122,124
C     WORK AT LARGE POSITIVE PHI
122   XZ(1,1)=XZ(1,1)+6.283184
      GO TO 130
124   XZ(1,2)=XZ(1,2)+6.283184
130   DO 140 I=1,2
      XZ(1,I)=XZ(1,I)*RADIUS(3)
      XZ(2,I)=XZ(2,I)-ZMEND
140   CONTINUE
C     Z IS COUNTED FROM -ZEND
C
C     NOW CALCULATE THE POSITION UNCERTAINTY
C     DLT=0.04/PABS*SQRT(1X0)*(Z-ZCOIL)
      PABS=CXDCHG(9,N)
      IF(PABS.GT.16.) PABS=16.
      IF(PABS.LT.0.1) PABS=0.1
      DS=DEL/PABS
      IF(DS.LT.DLBMIN) DS=DLBMIN
      DR2=BDR2
      GO TO 220
C
C-------------   END CAP PART  ----------------------
C
200   CONTINUE
      NSTEP=2
      DZ=THECPX(1)/CXDCHG(8,N)
      DO 210 I=1,3
      XZ(I,1)=CXDCHG(I+2,N)
      XZ(I,2)=XZ(I,1)+DZ*CXDCHG(I+5,N)
210   CONTINUE
      DO 215 I=1,3
215   CXDCHG(I+2,N)=(XZ(I,1)+XZ(I,2))/2.
C     WRITE(6,610) XZ
      DS=DLEMIN
      DR2=EDR2
C
C--------------- COMMON TO BARREL AND END CAP ---------
C
220   CONTINUE
      DX=XZ(1,2)-XZ(1,1)
      IF(DX.GE.0.) GO TO 240
C     INTERCHANGE SO THAT DX>0
      DO 230 J=1,2
      DY=XZ(J,2)
      XZ(J,2)=XZ(J,1)
      XZ(J,1)=DY
230   CONTINUE
      DX=-DX
240   DY=XZ(2,2)-XZ(2,1)
      DSQ=DX*DX+DY*DY
      DL=DS*SQRT(1.+DSQ/DR2)
C
C     THE ERROR CIRCLE BECOMES AN ELLIPSE BY PROJECTION
C     DL AND DS ARE THE LONGER AND SHORTER WIDTH.
      DSQ=SQRT(DSQ)
      IF(DSQ.LT.1.E-15) DSQ=1.E-15
      DX=DX/DSQ
      DY=DY/DSQ
      IF(DSQ.LE.1.E-15) DX=1.
C     APPROXIMATE THE BOUNDARY BY A RECTANGLE
      DXY(1)=DL*DX
      DXY(2)=DL*DY
      DO 242 J=1,2
      XZ(J,1)=XZ(J,1)-DXY(J)
      XZ(J,2)=XZ(J,2)+DXY(J)
242   CONTINUE
      DXY(1)=-DS*DY
      DXY(2)=DS*DX
      IF(DXY(1).GT.0.) GO TO 244
      DXY(1)=-DXY(1)
      DXY(2)=-DXY(2)
244   CONTINUE
C     THIS IS TO DEFINE SUCH THAT X1<X2, AND X3<X4
C     WRITE(6,610) XZ
C     WRITE(6,615) DS,DL,DLMIN,DX,DY,DXY
C615  FORMAT(' DS,DL,..',10F10.3)
      DO 250 J=1,2
      XY(J,1)=XZ(J,1)-DXY(J)
      XY(J,2)=XZ(J,1)+DXY(J)
      XY(J,3)=XZ(J,2)-DXY(J)
      XY(J,4)=XZ(J,2)+DXY(J)
250   CONTINUE
C     XY ARE THE FOUR CORNERS OF THE ERROR RECTANGLE
C     WRITE(6,620) XY
C620  FORMAT(' XY',10F10.4)
      IF(IBE) 280,260,280
260   CONTINUE
C
C     BARREL PART AGAIN
C     SCALE THE COORDINATE INTO THE COUNTER SIZE
      DO 270 I=1,4
      XY(1,I)=XY(1,I)/PHIDEV
      XY(2,I)=XY(2,I)/ZDEV
270   CONTINUE
C     WRITE(6,620) XY
      CALL LGBHIT(XY,*400)
      GO TO 300
C
C     END CAP AGAIN
280   CONTINUE
      DO 290 I=1,4
      DO 290 J=1,2
290   XY(J,I)=XY(J,I)/140.
C     SCALE THEM TO THE COUNTER SIZE.
C     WRITE(6,620) XY
      CALL LGEHIT(XY,IBE,*400)
300   CONTINUE
      MAPCCL(NCHIND+1)=NPOINT+1
      RETURN
C
400   CALL LGMESG( 9, 1)
      NPOINT=NPMAX
      NCHIND=N
      NCHCLS=NCHIND
      MAPCCL(N+1)=NPOINT+1
      RETURN
      END
