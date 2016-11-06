C   27/02/84 707281253  MEMBER NAME  ZEXTRA   (ZS)          FORTRAN
      SUBROUTINE ZEXTRA(IPT,IER)
C ----------------------------------------------------------------------
C     EXTRAPOLATES PATR TRACK TO Z-CHAMBER
C
C     INPUT     IPT   POINTER TO TRACK IN PATR BANK
C     OUTPUT    IER   ERROR CODE
C                     =0    NORMAL
C                     =1    NO INTERSECTION IN XY
C                     =2    Z OUT OF RANGE
C                     =3    IN DEADSPACE (FRAMES, GAS INLET) INNER WIRE
C                     =4    ''     "        ''     "    "    OUTER WIRE
C                     =5    ''     "        ''     "    "    BOTH WIRES
C                     =6    LAST PT ON TRK HAS R<750 MM
C
C               FILLS COMMON/CZEXT/
C ----------------------------------------------------------------------
C%MACRO CBCS
C%MACRO CZGEO
C ----------------------------------------------- BOS COMMON
      COMMON/BCS/IW(1)
      REAL RW(1)
      INTEGER*2 HW(1)
      EQUIVALENCE(IW(1),RW(1),HW(1))
C ----------------------------------------------- END BOS DECLARATIONS
C ----------------------------------------------- Z-CHAMBER CONSTANTS
       COMMON /CZGEO/ RZCHI,RZCHA,NZRPHI,NZZ,Z1ZCH,Z2ZCH,
     *              ZCHA, ZCHB, ZCHSS, ZCHDL, ZCHDLL,
     *              DLZZ, DLZPMI, DLZW1, DLZW2
C ----------------------------------------------- EXTR. TRACK COMMON
      COMMON/CZEXT/XT, YT, ZT,XI1,YI1,ZI1,XI2,YI2,ZI2
C                  XT, YT, ZT    COORDS OF LAST MEASURED PT ON TRK
C                  XI1,YI1,ZI1   INTERSECTION WITH INNER WIRE
C                  XI2,YI2,ZI2   INTERSECTION WITH OUTER WIRE
C ----------------------------------------------- END /CZEXT/
      COMMON/CZBUG/KRUN,KREC,II,JJ,X0,Y0,R0,RR
      COMMON/CZEXT2/X1,Y1,X2,Y2,XA,YA,XB,YB
      DATA PI/3.14159/,TWOPI/6.28318/
C
                                                                  JJ=1
      XI1=0.
      YI1=0.
      ZI1=0.
      XI2=0.
      YI2=0.
      ZI2=0.
C ----------------------------------------------- FIND PARAMS OF CIRCLE
C                              FORMED BY TRK IN XY (USE LAST PT OF TRK)
      IER= 6
      XT = RW(IPT+12)
      YT = RW(IPT+13)
C *********************************************** CHECK R>750MM
C                                                 ADDED 13/7/86, SLC
CCC   IF(SQRT(XT**2+YT**2).LT.750.)    GOTO 99
C
      ZT = RW(IPT+14)
      DXT= RW(IPT+15)
      DYT= RW(IPT+16)
      IER= 1
      IF(DXT.EQ.0. .OR. DYT.EQ.0. .OR. RW(IPT+28).EQ.0.)
     *                                 WRITE(6,9000) KRUN,KREC,II,
     *                                 XT,YT,ZT,DXT,DYT,RW(IPT+28)
      IF(DXT.EQ.0. .OR. DYT.EQ.0. .OR. RW(IPT+28).EQ.0.)
     *                                 GOTO 99
 9000 FORMAT(' -------- ZEXTRA --- EVENT ',I6,I6,' TRACK ',I3,
     *       ' ZERO CURVATURE OR DIRECTION VECTOR',
     *      /'          XT,YT,ZT,DXT,DYT,CURV: ',6E12.4)
      PHI= ATAN2(DYT,DXT)
      R  = 1./RW(IPT+28)

      X0 = XT + R*SIN(PHI)
      Y0 = YT - R*COS(PHI)
      R0 = ABS(R)
C ----------------------------------------------- FIND INTERSECTION
C                    WITH UPPER AND LOWER BOUNDS OF Z-CH WIRE RADII
                                                                  JJ=2
      RZ1  = 869.5
      RZ1A = 877.0
      RZ2  = 887.1
      RZ1SQ= RZ1*RZ1
      RZ2SQ= RZ2*RZ2
      R0SQ = R0*R0
      RRSQ = X0*X0 + Y0*Y0
      RR   = SQRT(RRSQ)

      IF(RRSQ.EQ.0.)                              WRITE(6,9001)X0,Y0
 9001 FORMAT(' -------- ZEXTRA --- PROBLEMS: X0,Y0: ',2E12.4)
C     YY1SQ= R0SQ - 0.25*RRSQ*(1.+(R0SQ-RZ1SQ)/RRSQ)**2
C ----------------------------------------------- RE-EXPRESS TO REDUCE
C                                                      ROUNDING ERRORS
      YYS0 = 0.25*(2.*R0SQ - RRSQ - R0SQ**2/RRSQ)
      YYS1 = 0.5 * RZ1SQ * (1. + R0SQ/RRSQ*(1. - 0.5*RZ1SQ/R0SQ))
      YY1SQ= YYS0+YYS1
C ----------------------------------------------- NO INTERSECTION?
      IF(YY1SQ.LT.0.)                             GOTO 99
      YY1  = SQRT(YY1SQ)

C     YY2SQ= R0SQ - 0.25*RRSQ*(1.+(R0SQ-RZ2SQ)/RRSQ)**2
      YYS2 = 0.5 * RZ2SQ * (1. + R0SQ/RRSQ*(1. - 0.5*RZ2SQ/R0SQ))
      YY2SQ= YYS0+YYS2
C ----------------------------------------------- NO INTERSECTION?
      IF(YY2SQ.LT.0.)                             GOTO 99
      IF(YY1SQ.EQ.YY2SQ)                          CALL ZBUG
                                                                  JJ=3
      YY2  = SQRT(YY2SQ)

      XX1  = (RRSQ-R0SQ+RZ1SQ)/RRSQ
      XX2  = (RRSQ-R0SQ+RZ2SQ)/RRSQ

      X1A  = 0.5*X0*XX1 + YY1*Y0/RR
      Y1A  = 0.5*Y0*XX1 - YY1*X0/RR
      X1B  = 0.5*X0*XX1 - YY1*Y0/RR
      Y1B  = 0.5*Y0*XX1 + YY1*X0/RR

      X2A  = 0.5*X0*XX2 + YY2*Y0/RR
      Y2A  = 0.5*Y0*XX2 - YY2*X0/RR
      X2B  = 0.5*X0*XX2 - YY2*Y0/RR
      Y2B  = 0.5*Y0*XX2 + YY2*X0/RR
C ----------------------------------------------- X1A/B,Y1A/B ARE THE
C   TWO INTERSECTION PTS WITH THE INNER Z-CH BOUND, X2 ETC WITH OUTER
C ----------------------------------------------- NOW PICK NEARER TO TRK
      DRASQ= (X1A-XT)**2 + (Y1A-YT)**2
      DRBSQ= (X1B-XT)**2 + (Y1B-YT)**2
                                                                  JJ=4

      X1   = X1A
      Y1   = Y1A
      X2   = X2A
      Y2   = Y2A
      IF(DRASQ.LT.DRBSQ)                           GOTO 10
         X1= X1B
         Y1= Y1B
         X2= X2B
         Y2= Y2B
 10   CONTINUE
C ----------------------------------------------- OK. NOW INTERPOLATE
C     TO ACTUAL Z-CH SHAPE (24-GON) BY APPROXIMATING TRK AS STR. LINE
                                                                  JJ=5
      IER=2
C ----------------------------------------------- FIRST FIND PHI-SECTOR
      DPHI = TWOPI/NZRPHI
C ----------------------------------------------- Z-CH PHI MEASURED
C                                 FROM Y-AXIS NOT X-AXIS (BLAST IT)
      PHI1Z= ATAN2(-X1,Y1)
      IF(PHI1Z.LT.0.)             PHI1Z=PHI1Z+TWOPI
      NPHI1= PHI1Z/DPHI + 1

      PHI2Z= ATAN2(-X2,Y2)
      IF(PHI2Z.LT.0.)             PHI2Z=PHI2Z+TWOPI
      NPHI2= PHI2Z/DPHI + 1
C ----------------------------------------------- NOW FIND ENDPOINTS
      PHIA = (NPHI1-1)*DPHI
      PHIB = PHIA+DPHI

      XA   =-RZ1A*SIN(PHIA)
      YA   = RZ1A*COS(PHIA)
      XB   =-RZ1A*SIN(PHIB)
      YB   = RZ1A*COS(PHIB)
C=======================================================================
C
C  REPLACED BELOW TO AVOID DIVIDE CHECK    J.SPITZER 24/7/87
C
C ----------------------------------------------- GET EQN OF WIRE
C     GRAB = (YB-YA)/(XB-XA)
C     CAB  = YA - GRAB*XA
C ----------------------------------------------- AND TRACK
C     GRT  = (Y2-Y1)/(X2-X1)
C     CT   = Y1 - GRT*X1
C ----------------------------------------------- FINALLY INTERCEPT
C     IF(GRAB.EQ.GRT)                             WRITE(6,9002) GRAB,GRT
C9002 FORMAT(' -------- ZEXTRA --- PROBLEMS: GRAB,GRT: ',2E12.4)
C     XI1  = (CT-CAB)/(GRAB-GRT)
C     YI1  = (CT*GRAB-GRT*CAB)/(GRAB-GRT)
C=======================================================================
      DETA1=(Y2-Y1)*(XA-XB)-(YB-YA)*(X1-X2)
      IF(ABS(DETA1).LT.1.E-8)                     WRITE(6,9002) DETA1
 9002 FORMAT(' -------- ZEXTRA --- PROBLEMS: DETA1: ',E12.4)
      XI1=((Y1*X2-X1*Y2)*(XB-XA)-(YA*XB-XA*YB)*(X2-X1))/DETA1
      YI1=((Y1*X2-X1*Y2)*(YB-YA)-(YA*XB-XA*YB)*(Y2-Y1))/DETA1
C --- END REPLACEMENT ---
C=======================================================================
C ----------------------------------------------- AND Z USING TRK FIT
      RI1  = SQRT(XI1**2+YI1**2)
      ZI1  = RW(IPT+30)*RI1 + RW(IPT+31)
C ----------------------------------------------- Z OUT OF RANGE?
      IF(ZI1.LT.Z1ZCH .OR. ZI1.GT.Z2ZCH)          GOTO 99
C ----------------------------------------------- DEAD SPACE?
      IER3=0
      IF(ABS(XI1).LE.ZCHDL)                       IER3=3
C     IF(ABS(YI1).LE.ZCHDLL)                      IER3=3
C
C ----------------------------------------------- INNER WIRE DONE.
C ----------------------------------------------- NOW OUTER WIRE
C                REDO PHIA,PHIB ON OFFCHANCE THAT NPHI1.NE.NPHI2
                                                                  JJ=6
      PHIA = (NPHI2-1)*DPHI
      PHIB = PHIA+DPHI
      XA   =-RZ2*SIN(PHIA)
      YA   = RZ2*COS(PHIA)
      XB   =-RZ2*SIN(PHIB)
      YB   = RZ2*COS(PHIB)
C=======================================================================
C
C  REPLACED BELOW TO AVOID DIVIDE CHECK     J.SPITZER  24/7/87
C
C     GRAB = (YB-YA)/(XB-XA)
C     CAB  = YA - GRAB*XA
C     IF(GRAB.EQ.GRT)                             WRITE(6,9002) GRAB,GRT
C     XI2  = (CT-CAB)/(GRAB-GRT)
C     YI2  = (CT*GRAB-GRT*CAB)/(GRAB-GRT)
C=======================================================================
      DETA1=(Y2-Y1)*(XA-XB)-(YB-YA)*(X1-X2)
      IF(ABS(DETA1).LT.1.E-8)                     WRITE(6,9002) DETA1
      XI2=((Y1*X2-X1*Y2)*(XB-XA)-(YA*XB-XA*YB)*(X2-X1))/DETA1
      YI2=((Y1*X2-X1*Y2)*(YB-YA)-(YA*XB-XA*YB)*(Y2-Y1))/DETA1
C --- END REPLACEMENT ---
C=======================================================================
      RI2  = SQRT(XI2**2+YI2**2)
      ZI2  = RW(IPT+30)*RI2 + RW(IPT+31)
      IF(ABS(XI2).LE.ZCHDL)                       IER=4
C     IF(ABS(YI2).LE.ZCHDLL)                      IER=4
C ----------------------------------------------- SET UP IER AND RETURN
      IF(IER.NE.4)                                IER=IER3
      IF(IER.EQ.4)                                IER=IER+IER3/3
 99   CONTINUE
      RETURN
      END
