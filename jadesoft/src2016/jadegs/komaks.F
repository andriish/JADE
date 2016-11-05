C   11/03/87 705211105  MEMBER NAME  KOMAKS   (JADEGS)      FORTRAN
C-----------------------------------------------------------------------
      SUBROUTINE KOMAKS(IPRINT,IKOMA,ITRYK,IERRK,IEERJ)
C-----------------------------------------------------------------------
C
C     THIS SUBROUTINE IS BASED ON THE PROGRAM KST1.. BY S.KOMAMIYA
C     IT CALLS ALL THE RELEVANT SUBROUTINES FOR K0S SEARCH AND VERTEX
C     OPTIMIZATION.
C
C     THE FLAG IKOMA = 0 FOR NORMAL RETURN
C                      1 FOR NO PATR BANK
C                      2 FOR <3 PATR TRACKS
C
C     THE WORD ITRYK GIVES THE NUMBER OF COMBINATIONS TRIED, IN LOOP 10
C
C    THE ARRAY IERRK CONTAINS A CODE FOR EACH OF THE 1ST 50 COMBS TRIED:
C     1: ARRAY IEEFG IS NONZERO:
C         VALUE 1: TRACK IS PART OF E+E- CONVERSION  (GIVEN BY VTXEE)
C         VALUE 2: NOT USED
C         VALUE 3: SIGSCT=R1I*R2I < 0   (GIVEN BY XYSCAT)
C                  TRACK FROM MAIN VERTEX, WITHIN SCATTERING ERRORS
C         VALUE 4: RMIN > ARLIM
C         VALUE 5: ZMIN > 350.
C         VALUE 6: TRACK IS JUDGED BAD BY VTXPRE
C     2: LESS THAN MNHIT HITS
C     3: RI*RJ PRODUCT, KTRCK(20,...
C     4: IFLAG,JFLAG SET FROM XYVRTX
C     5: KFLAG SET FROM XYVOPT
C     6: LFLAG SET FROM ZRVOPT
C     7: MOMENTA LESS THAN PCUTT
C     8: ERROR CODE FROM ELOSS
C     9: CANDIDATE FOR E+E- CONVERSION
C    10: DVL < DVLMIN
C    11: D > DK0MAX
C    12: SAME CHARGE   (DETERMINED FROM SIGNED CURVATURE IN XYVRTX)
C
C
C
C
C    IMPLEMENTATION BY J. OLSSON
C                 28.04.1986  VALUE OF CHI**2 (MAX) IN ZRVOPT CHANGED
C                             FROM 3600 TO 2500
C                 29.04.1986  ADD ARRAY IEERJ(50,2) TO PASS IEEFG
C                             CODES FOR EACH TRACK IN A COMB TO O/P
C
C--------------------------------------------------------------------
C    THE PROGRAM CAN BE STEERED BY THE FOLLOWING VARIABLES
C    STDEV: NR OF SIGMAS EXCLUDED IN THE SCATTERING ERROR OF TRACKORIGIN
C    ARLIM: MAXIMUM RMIN DISTANCE OF TRACK
C    IZRFIT: 0 NO OPTIMIZATION IN ZR
C            1    OPTIMIZATION IN ZR
C    MNHIT:  MINIMUM NR OF HITS ON A TRACK
C    PCUTT:  MINIMUM MOMENTUM OF A TRACK
C    DVLMIN:  MINIMUM DVL, RFI DECAY LENGTH OF V0 CANDIDATE
C    DK0MAX:  MAXIMUM OF MIN.DISTANCE MAIN VERTEX - V0 FLIGHT PATH
C    IEE3FL  0 NO REJECTION ON IEEFG(..)=3
C            1 REJECTION ON IEEFG(..)=3
C--------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
#include "cdata.for"
C
      COMMON /CKOMAK/NMAS,PCOMB1(4,50),PCOMB2(4,50),HTR1(50),HTR2(50),
     +            VMAS(50),DMAS(50),DVLK0(50),DK0(50),XYZK0(3,50),
     +            STDEV,ARLIM,IZRFIT,MNHIT,PCUTT,DVLMIN,DK0MAX,IEE3FL
C
#include "cgeo1.for"
#include "cgeov.for"
C
      COMMON/MAG/ BC
C
C                    DITTMANN'S VERTEX ROUTINE WORK AREA
C
      COMMON /CWORK1/NTDITT,TDITT(2000),NVDITT,VDITT(200)
      DIMENSION ITDITT(2000),IVDITT(200)
      DIMENSION IERRK(50)
      EQUIVALENCE (TDITT(1),ITDITT(1)),(VDITT(1),IVDITT(1))
C
      COMMON /VTR/ VTRK(20,100)
      DIMENSION KTRK(20,100)
      EQUIVALENCE (KTRK(1,1),VTRK(1,1))
C
      DIMENSION IEEFG(100),V(6),RMIN0(100),IEERJ(50,2)
C
C                            ORIGINAL VALUES, KEEP FOR POSTERITY.
C
C     DATA STDEV /3./
C     DATA ARLIM /4./
C     DATA IZRFIT /1/
C     DATA MNHIT /24/
C     DATA PCUTT /0.100/
C     DATA DVLMIN /10./
C     DATA DK0MAX /10./
C
C
      DATA PIM /0.13957/
      DATA VMKS /0.4977/
      DATA ICALLS /0/
C
C -------------------------- INITIALISATION
C
      IPRO = IPRINT
      ICALLS = ICALLS + 1
 1    IF(ICALLS.NE.1)                   GOTO 100
         WRITE(6,1000)
         WRITE(6,1010) STDEV,ARLIM,MNHIT,PCUTT,DVLMIN,DK0MAX,IZRFIT
         WRITE(6,1020) IEE3FL
 1000    FORMAT(' KOMAKS CALLED, VERSION FROM 10.03.1987   ')
 1010    FORMAT(' STDEV,ARLIM,MNHIT,PCUTT,DVLMIN,DK0MAX,IZRFIT',/,
     +          2F5.2,I5,3X,3(1XF5.2),I5)
 1020    FORMAT(' FLAG FOR IEEFG=3 REJECTION                 :',I5)
C
 100  CONTINUE
      NMAS  = 0
      IKOMA = 0
      ITRYK = 0
      CALL VZERO(IERRK,50)
      ZPOS  = 10000.
      BC    = ABS(BKGAUS*0.3E-4)
      RMAT  = RBPIPE(X0BTWN)
      DO 101 I=1,50
         DO 102 J=1,2
            IEERJ(I,J) = 0
102      CONTINUE
101   CONTINUE
C ------------------------------------- FILL WORK ARRAY VTRK,KTRK
      INHD  = IDATA(IBLN('HEAD'))
      NRUN  = HDATA( INHD   +  INHD  + 10)
      NEVT  = HDATA( INHD   +  INHD  + 11)
      INPA  = IDATA(IBLN('PATR'))
      IF(INPA.LE.0)                     GOTO 990
      NTR   = IDATA(INPA+2)
C
      CALL PATTRK(INPA,*980,*980)
C ------------------------------------- DITTMANN'S VERTEX ROUTINE
      CALL VTXPRE(INHD,INPA)
C ------------------------------------- IEEFG ARRAY MARKS BAD TRACKS
      CALL VZERO(IEEFG,100)
      DO 200 ITR=1,NTR
         I40 = (ITR-1)*40+1
         IF(ITDITT(I40).NE.0)           GOTO 210
C --------- TRACK IS JUDGED BAD BY THE VERTEX PROGRAM
            IEEFG(ITR) = 6
            GOTO 200
  210    CONTINUE
         CALL XYSCAT(NRUN,ITR,STDEV,R1I,R2I,RMINTR,MFLAG)
C ----------------------------------------------------------------------
C        R1I AND R2I ARE MEASURES OF DISTANCE FROM MAIN (RUN) VERTEX
C        IF PRODUCT < 0, RUN VERTEX IN POSSIBLE ORIGIN
C        KTRK(20,..) TELLS IF TRACK ORIGINATES BEFORE OR AFTER VERTEX
C ----------------------------------------------------------------------
         SIGSCT = R1I*R2I
         IF(IEE3FL.NE.0 .AND. SIGSCT.LE.0.)  IEEFG(ITR)   = 3
         IF(R1I.GT.0.   .AND. R2I.GT.0.)     KTRK(20,ITR) = 1
         IF(R1I.LT.0.   .AND. R2I.LT.0.)     KTRK(20,ITR) =-1
         IF(RMINTR.LT.ARLIM)                 IEEFG(ITR)   = 4
C
         ZMIN = ABS(VTRK(19,ITR))
         IF(ZMIN.GT.350.)                    IEEFG(ITR)   = 5
C
         RMIN0(ITR) = RMINTR
 200  CONTINUE
C ------------------------------------- E+E- VERTEX REJECTION
      NVDITT = 0
      CALL VTXEE
      IF(NVDITT.EQ.0)                   GOTO 300
C
      DO 310 ID=1,NVDITT
         JD  = 0
         K1D = 0
         DO 320 KD=1,NTDITT
            IF(ITDITT(JD+14).EQ.ID.AND.K1D.NE.0)  K2D = KD
            IF(ITDITT(JD+14).EQ.ID.AND.K1D.EQ.0)  K1D = KD
            JD = JD+40
 320     CONTINUE
         IEEFG(K1D) = 1
         IEEFG(K2D) = 1
 310  CONTINUE
C
 300  CONTINUE
      IF(IPRO.GT.1)                WRITE(6,1300) NVDITT
      IF(IPRO.GT.0)                WRITE(6,1310) NTR,(IEEFG(I),I=1,NTR)
1300  FORMAT(' KOMAKS,  NVDITT = ',I3)
1310  FORMAT(' KOMAKS,  NTR,IEEFG = ',I3,2X,20I3)
C
C ------------------------------------- TRACK LOOP
C
      DO 400 I=2,NTR
         II=I-1
         DO 500 J=1,II
            ITRYK = ITRYK + 1
C
C           REJECT TRACKS WHICH HAVE BEEN MARKED BAD IN IEEFG
C
            IF(IEEFG(I).NE.0 .OR. IEEFG(J).NE.0)  GOTO 900
            IF(IPRO.GT.1)
     *         WRITE(6,1500) I,J,KTRK(17,I),KTRK(17,J),
     *                           KTRK(20,I),KTRK(20,J)
 1500          FORMAT(' KOMAKS,  I J KTRK17 ',4I5,' KTRK20 ',2I4)
 901        IF( KTRK(17,I).LT.MNHIT)    GOTO 905
            IF( KTRK(17,J).LT.MNHIT)    GOTO 905
            RI = VTRK(10,I)
            RJ = VTRK(10,J)
            IF(IPRO.GT.1)               WRITE(6,1510) I,J,RI,RJ
 1510       FORMAT(' KOMAKS,  I J ',2I3,'  RI RJ ',2E14.6)
C
C                            CANDIDATES SHOULD HAVE THE SAME SIDE ORIGIN
C
            IF(IEE3FL.NE.0 .AND. IER3FL.EQ.0 .AND. RI*RJ.LT.0. .AND.
     +         KTRK(20,I)*KTRK(20,J).LE.0.)      GOTO 910
C
            CALL XYVRTX(I,J,V,DVL,D,JFLAG,IFLAG)
            D0 = D
            IF(IPRO.GT.1)               WRITE(6,1520) I,J,IFLAG,JFLAG
 1520       FORMAT(' KOMAKS,  I J ',2I3,'  I-JFLAG ',2I7)
C --------- JFLAG=100 FLAGS SAME CHARGE
            IF(JFLAG.EQ.100)            GOTO 955
            IF(JFLAG.NE.0)              GOTO 915
            IF(IFLAG.NE.0)              GOTO 915
C
C                            THIS ROUTINE SHOULD PERFORM THE VERTEX
C                            OPTIMISATION HOWEVER THE RESULTS ARE
C                            BY PASSED WITHIN THE ROUTINE SO IT ONLY
C                            SELECTS ON KFLAG VALUE.
C
C
            CALL XYVOPT(I,J,V,DVL,D,CHI1,ALPH12,PHUV,KFLAG)
C
            IF(IPRO.GT.1)               WRITE(6,1530) I,J,KFLAG
 1530       FORMAT(' KOMAKS,  I J ',2I3,'  KFLAG ',I4)
C
            IF(KFLAG.NE.0 .AND. KFLAG.NE.1)     GOTO 920
C --------- KFLAG=1 MEANS V0-VERTEX > 600MM FROM ORIGIN
C
C --------- EXCLUDE BEAMPIPE
CC          IF(DVL.GT.RMAT-30.AND.DVL.LT.RMAT+50.)     GOTO 500
C --------- MAKE CUT ON DECAY LENGTH DVL
            IF(DVL.LT.DVLMIN)                          GOTO 945
C --------- MAKE CUT ON DISTANCE D BETWEEN V0 FLIGHT PATH AND MAIN VX
            IF(D.GT.DK0MAX)                            GOTO 950
C
            ALPH12 = ALPH12 * 180./3.14159265
            VO     = SQRT(V(1)**2 + V(2)**2)
            R1I    = SQRT(VTRK(1,I)**2 + VTRK(2,I)**2)
            R1J    = SQRT(VTRK(1,J)**2 + VTRK(2,J)**2)
            ZVI    = VTRK(3,I) +
     *               (V0-R1I) * VTRK(6,I)/SQRT(1.-VTRK(6,I)**2)
            ZVJ    = VTRK(3,J) +
     *               (V0-R1J) * VTRK(6,J)/SQRT(1.-VTRK(6,J)**2)
            ZIJ    = ABS(ZVI-ZVJ)
            NRPATR = IDATA(INPA-2)
            DZI0   = VTRK(6,I)
            DZJ0   = VTRK(6,J)
C ------------------------------------- R-Z FIT
            IF(IZRFIT.EQ.0)             GOTO 510
               CALL ZRVOPT(NRPATR,I,J,V,DZI,DZJ,CHI21,CHI22,LFLAG,ZPOS)
               IF(IPRO.GT.1)            WRITE(6,1540) I,J,LFLAG
 1540          FORMAT(' KOMAKS,  I J ',2I3,'  LFLAG ',I4)
               IF(LFLAG.NE.0)           GOTO 925
               GOTO 520
 510        CONTINUE
C --------- NO R-Z FIT, TAKE COS THETA FROM PATR DIR.COS.
               DZI = VTRK(6,I)
               DZJ = VTRK(6,J)
 520        CONTINUE
            IF(DZI.GT. 0.99999)         DZI = 0.99999
            IF(DZI.LT.-0.99999)         DZI =-0.99999
            IF(DZJ.GT. 0.99999)         DZJ = 0.99999
            IF(DZJ.LT.-0.99999)         DZJ =-0.99999
C ---------------------------------------------------------------------
C --------- V(3-6) CONTAIN X,Y COMPONENTS OF RADII OF CURVATURE
            PXI = V(3)*BC
            PYI = V(4)*BC
            PXJ = V(5)*BC
            PYJ = V(6)*BC
            PX  = PXI+PXJ
            PY  = PYI+PYJ
            PTI = SQRT(V(3)**2 + V(4)**2) * BC
            PTJ = SQRT(V(5)**2 + V(6)**2) * BC
            PZI = PTI * DZI/SQRT(1.-DZI**2)
            PZJ = PTJ * DZJ/SQRT(1.-DZJ**2)
            PZ  = PZI+PZJ
C
            SPI = PTI**2+PZI**2
            SPJ = PTJ**2+PZJ**2
            PI  = SQRT(SPI)
            PJ  = SQRT(SPJ)
C
            IF(IPRO.GT.1)               WRITE(6,1550) I,J,PI,PJ
 1550       FORMAT(' KOMAKS,  I J ',2I3,'  PI PJ ',2E12.4)
C --------- MOMENTUM CUT
            IF(PI.LT.PCUTT .OR. PJ.LT.PCUTT) GOTO 930
C
            EINI = SQRT(PI**2+PIM**2)
            EINJ = SQRT(PJ**2+PIM**2)
            CALL ELOSS(PXI,PYI,PZI,IERRI)
            CALL ELOSS(PXJ,PYJ,PZJ,IERRJ)
            IF(IPRO.GT.1)               WRITE(6,1560) I,J,IERRI,IERRJ
 1560       FORMAT(' KOMAKS,  I J ',2I3,'  IERRI,J ',2I4)
            IF(IERRI.NE.0 .OR. IERRJ.NE.0)   GOTO 935
C
            PI    = SQRT(PXI**2 + PYI**2 + PZI**2)
            PJ    = SQRT(PXJ**2 + PYJ**2 + PZJ**2)
            EI    = SQRT(PI**2  + PIM**2)
            EJ    = SQRT(PJ**2  + PIM**2)
            ELOSI = EI-EINI
            ELOSJ = EJ-EINJ
C
            COSIJ = (PXI*PXJ + PYI*PYJ + PZI*PZJ)/(PI*PJ)
C
            PX    = PXI+PXJ
            PY    = PYI+PYJ
            PZ    = PZI+PZJ
            E     = EI+EJ
            PLTH  = PX*THX0 + PY*THY0 + PZ*THZ0
            P     = SQRT(PX**2 + PY**2 + PZ**2)
            PTTH  = SQRT(P**2  - PLTH**2)
            VMASS = SQRT(E**2  - P**2)
            X     = V(1)
            Y     = V(2)
            Z     = SQRT(X**2 + Y**2) * PZ/SQRT(PX**2 + PY**2)
            RVO   = SQRT(X**2 + Y**2 + Z**2)
C --------- MASS CUT TO REJECT PHOTON CONVERSION CANDIDATES
            PHMASS= SQRT(ABS((PI+PJ)**2 - P**2))
            IF(IPRO.GT.1)               WRITE(6,1570) I,J,PHMASS
 1570       FORMAT(' KOMAKS,  I J ',2I3,'  PHMASS ',E12.4)
            IF(PHMASS.LT.0.10)          GOTO 940
C
            TL    = DVL/(P * SQRT(1.-(PZ/P)**2)) * VMKS/26.75
            ALPH0 = (PX*X + PY*Y + PZ*Z)/(P*RVO)
            IF(ALPH0.GT.1.)             ALPH0 = 1.
            ALPH0 = ARCOS(ALPH0)
            ALPHA = ALPH0*180./3.14159265
C
            DM    = 0.
C
            IF(IPRO.GT.1)          WRITE(6,1580) I,J,D,DVL,COSIJ,CHI1
 1580       FORMAT(' KOMAKS, I J ',2I3,
     *             ' D DVL ',2E12.4,' COSIJ CHI1 ',2E12.4)
C
C *** SLC 12/3/86 ********************* NEXT 2 IF'S COMMENTED OUT
CC          IF(COSIJ.LT.-0.7)           GOTO 530
CC          IF(CHI1.GT.5.)              GOTO 530
C ------------------------------------- INCORPORATE RESOLUTION
               SIGPI = 0.033 * PI**2
               SIGPJ = 0.033 * PJ**2
               IF(PI.LT.2.)             SIGPI = 0.05*PI
               IF(PJ.LT.2.)             SIGPJ = 0.05*PJ
               DI    = (EJ*PI/EI - PJ*COSIJ) * SIGPI
               DJ    = (EI*PJ/EJ - PI*COSIJ) * SIGPJ
               DMP1  = SQRT(DI**2 + DJ**2)
               DMP   = DMP1/VMKS
               SINIJ = SQRT(1.-COSIJ**2)
               DMT   = PI*PJ * SINIJ * 0.01/VMKS
               DSCATI= 2.*1.41421356 * SCATT(PI,PZI/PI)
               DSCATJ= 2.*1.41421356 * SCATT(PJ,PZJ/PJ)
               DSCATT= SQRT(DSCATI**2 + DSCATJ**2)
               DMTS  = PI*PJ * SINIJ * DSCATT/VMKS
               DM    = SQRT(DMTS**2  + DMP**2)
               VMMIN = VMASS - 2.*DM
               VMMAX = VMASS + 2.*DM
C ------------------------------------- INVESTIGATE RESOLUTION
C                                       THIS HAS BEEN ADDED BY SLC
C                                       AND COMMENTED OUT (PVH)
C
C
C              PARAM(1) = VMASS
C              PARAM(2) = EI+EJ
C              PARAM(3) = DMP
C              PARAM(4) = DMTS
C              CALL DEKB(0)
C              CALL NTUPEL(901,PARAM)
 530        CONTINUE
C ------------------------------------- FILL RESULT COMMON/KOMAK/
            IF(NMAS.GE.50)              GOTO 540
               NMAS           = NMAS + 1
               PCOMB1(1,NMAS) = PXI
               PCOMB1(2,NMAS) = PYI
               PCOMB1(3,NMAS) = PZI
               PCOMB1(4,NMAS) = EI
               PCOMB2(1,NMAS) = PXJ
               PCOMB2(2,NMAS) = PYJ
               PCOMB2(3,NMAS) = PZJ
               PCOMB2(4,NMAS) = EJ
               XYZK0(1,NMAS)  = V(1)
               XYZK0(2,NMAS)  = V(2)
               XYZK0(3,NMAS)  = ZPOS
               VMAS(NMAS)     = VMASS
               DMAS(NMAS)     = DM
               DVLK0(NMAS)    = DVL
               DK0(NMAS)      = D
               HTR1(NMAS)     = I
               HTR2(NMAS)     = J
 540        CONTINUE
            IF(IPRO.LE.1)               GOTO 550
               WRITE(6,1600)NRUN,NEVT
               WRITE(6,1610)I,J,V(1),V(2),DVL,D,DD,ALPHA,
     *                      VMASS,COSIJ,P,PX,PY,PZ,
     *                     PI,ELOSI,PXI,PYI,PZI,
     *                      PJ,ELOSJ,PXJ,PYJ,PZJ,
     *                      CHI1,ALPH12,CHI22,TL,RMIN0(I),RMIN0(J),
     *                      DZI,DZJ,DZI0,DZJ0,VMMIN,VMMAX,ZPOS
C
 1600          FORMAT(/' KOMAKS: *****  RUN# =',I6,' -',I6)
 1610          FORMAT(' # KOMAKS # (I,J)=(',2I6,')  V(',2F9.3,')',
     *         ' DVL=',F10.3,' D,DD=',2F9.3,' ALPHA=',F8.3/
     *         ' #########  VMASS=',F7.3,'  COSIJ=',F7.3,'   P= ',F7.3,
     *         '  P(',3F10.3,')    '/
     *         '   #######  PI= ',F7.3,'  ELOSI=',F7.3,
     *         '   PI=(',3F10.3,')'/
     *         '     #####  PJ= ',F7.3,'  ELOSJ=',F7.3,
     *         '   PJ=(',3F10.3,')'/
     *         '       ### CHI**2=',F10.3,2X,F10.3,3X,F12.3,
     *         ' TL=',F10.3,'   RMIN(I,J)=',2F10.3/
     *         '         #  ',
     *         '      DZ(I,J)=(',2F10.3,')  DZ0(I,J)=(',2F10.3,') '/
     *         '  (VMMIN,VMMAX)=',2F9.3,' ZPOS ',E12.4)
C
 550        CONTINUE
            GOTO 500
C ------------------------------------- THE REJECTION SLIPS
 900        IERRKK = 1
            GOTO 960
 905        IERRKK = 2
            GOTO 960
 910        IERRKK = 3
            GOTO 960
 915        IERRKK = 4
            GOTO 960
 920        IERRKK = 5
            GOTO 960
 925        IERRKK = 100+LFLAG
            GOTO 960
 930        IERRKK = 7
            GOTO 960
 935        IERRKK = 8
            GOTO 960
 940        IERRKK = 9
            GOTO 960
 945        IERRKK = 10
            GOTO 960
 950        IERRKK = 11
            GOTO 960
 955        IERRKK = 12
 960        IF(ITRYK.GT.50) GOTO 500
               IERRK(ITRYK) = IERRKK
               IF(IERRKK .NE. 1) GOTO 500
                 IEERJ(ITRYK,1) = IEEFG(I)
                 IEERJ(ITRYK,2) = IEEFG(J)
C
 500     CONTINUE
 400  CONTINUE
C ------------------------------------- END OF TRACK LOOP
      RETURN
C
 980  CONTINUE
         IKOMA = 2
      RETURN
C
 990  CONTINUE
         IKOMA = 1
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION DET(A)
C-----------------------------------------------------------------------
C
C     FUNCTION TO CALCULATE DETERMINANT OF 3*3 MATRIX
C     S.KOMAMIYA      20.03.1980    22:55
C     MODIFIED        20.03.1980    22:55
C
C
C
C     COPIED FROM F22KOM.KS(DET)    27.11.1985
C
C
      DIMENSION A(3,3)
      DET=A(1,1)*A(2,2)*A(3,3)
     *   +A(1,2)*A(2,3)*A(3,1)
     *   +A(1,3)*A(2,1)*A(3,2)
     *   -A(1,1)*A(2,3)*A(3,2)
     *   -A(1,3)*A(2,2)*A(3,1)
     *   -A(1,2)*A(2,1)*A(3,3)
         RETURN
         END
C
C-----------------------------------------------------------------------
      SUBROUTINE ELOSS(PX,PY,PZ,IELOS)
C-----------------------------------------------------------------------
C
C     SUBROUTINE TO CALCULATE ENERGY LOSS IN BEAM PIPE AND PIPE COUNTERS
C     S.KOMAMIYA      19.03.1980    05:20
C     MODIFIED        14.03.1980    05:20
C    COPIED FROM F22KOM.KS(ELOSS)    27.11.1985
C     MODIFIED TO ACCOUNT FOR CHANGING GEOMETRY FROM 5/1984
C                     28.11.1985    J.OLSSON
C  TEMPORARY MODIFICATION TO CALL JFGAIN INSTEAD OF JEGAIN. THIS IS TO
C  AVOID CONFUSION WITH THE OLD JEGAIN THAT STILL SITS ON JADEGS/GL
C  WHICH USES /CJJONI/ INSTEAD OF /CJIONI/       3.12  1985
C
C   INPUT PARAMETERS
C     P   :MOMENTUM OF PROJECTILE PARTICLE
C     DZ  :COS(AZIMUTH ANGLE)
C
C       PROJECTILE PARTICLE IS ASSUMED TO BE PION
C
C   MATERIAL IN OLD GEOMETRY
C       AL 4.0+7.0 (MM)
C       SC 10.0(MM)
C   MATERIAL IN NEW GEOMETRY    Z/A*RHO  GIVEN IN COMMON /CIJONV/
C       AL 3.0 + 1.0 + 7.0 (MM)
C       VERTEX CHAMBER GAS
      IMPLICIT INTEGER*2 (H)
C- - - - - - - - -   CIJONV  - - - - - - - - - - - - - - - - - - - - - -
C
      COMMON / CIJONV / POTVXC, ZAROVC,
     *                  POTVGA, ZAROVG
C
C                            GIVES AVERAGE IONISATION POTENTIAL
C                            AND VALUE FOR Z*RHO/A IN
C                            BETHE BLOCH FORMULA FOR :
C                               1) BEAMPIPE AND OUTER VERTEX CHAMBER
C                                  WALL (ALUMINIUM)
C                               2) CHAMBER GAS
C
C     DATA POTVXC,ZAROVC             / 160.3, 1.3      /
C     DATA POTVGA,ZAROVG             / 284.4, 0.00080  /
C
#include "cdata.for"
C
      DIMENSION P(6)
      DATA DAL0  /11.0/
      DATA DSC0  /10.0/
      DATA RHOAL /2.70/
      DATA RHOSC /1.03/
      DATA EIAL  /160.3/
      DATA EISC  /80.00/
      DATA ZAL   /13.0/
      DATA AAL   /26.0/
      DATA ZSC   /6.0/
      DATA ASC   /12.0/
      DATA DVTXG /65.0/
      DATA PIMASS/0.13975/
C     DATA ELEMAS/511.0/
C
C  USE DATE IN HEAD BANK TO DETERMINE IF OLD OR NEW GEOMETRY
C
       IPH2 = 2*IDATA(IBLN('HEAD'))
       IMAA = HDATA(IPH2+7)
       IAAR = HDATA(IPH2+8)
       IOLD = 0
       IF(IAAR.GT.1984) IOLD = 1
       IF(IAAR.EQ.1984.AND.IMAA.GT.4) IOLD = 1
C
      PTOT=SQRT(PX**2+PY**2+PZ**2)
      IF(PTOT .LE. 0.) GO TO 100
      E=SQRT(PTOT**2+PIMASS**2)
      P(1)=PX
      P(2)=PY
      P(3)=PZ
      P(4)=E
      P(5)=PIMASS
      P(6)=PTOT
      DZ=P(3)/PTOT
      SIN=SQRT(1.-DZ*DZ)
      IF(SIN.EQ.0.) GO TO 100
C
      DAL=DAL0/SIN
      DSC=DSC0/SIN
      IF(IOLD.EQ.1) DSC = DVTXG/SIN
      ZAROAL=ZAL/AAL*RHOAL
      ZAROSC=ZSC/ASC*RHOSC
      IF(IOLD.EQ.1) ZAROAL = ZAROVC
      IF(IOLD.EQ.1) ZAROSC = ZAROVG
      EISC1 = EISC
      IF(IOLD.EQ.1) EISC1 = POTVGA
C     CALL JEGAIN(P,DAL,EIAL,ZAROAL,*100)
C     CALL JEGAIN(P,DSC,EISC1,ZAROSC,*100)
      CALL JFGAIN(P,DAL,EIAL,ZAROAL,*100)
      CALL JFGAIN(P,DSC,EISC1,ZAROSC,*100)
      PX=P(1)
      PY=P(2)
      PZ=P(3)
      RETURN
100   IELOS=1
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE SOL(A,B,X,IFLAG)
C-----------------------------------------------------------------------
C     SUBROUTINE TO CALCULATE SOLUTION OF 3-VARIABLE LINEAR EQUATION
C     S.KOMAMIYA      20.03.1980    23:15
C     MODIFIED        20.03.1980    23:15
C
C       AX=B
C
C        A(I,J)    I=1,3  J=1,3
C        B(J)
C        X(I)
C
C    IDENTIC WITH THE MEMBER SOL  ON F22KOM.KS
C
      DIMENSION A(3,3),B(3),X(3),C1(3,3),C2(3,3),C3(3,3)
      IFLAG=1
      DA=DET(A)
        IF(DA.NE.0.) IFLAG=0
        IF(IFLAG.NE.0) RETURN
C
      DO 10 I=1,3
      DO 20 J=1,3
         C1(I,J)=A(I,J)
         C2(I,J)=A(I,J)
         C3(I,J)=A(I,J)
 20   CONTINUE
 10   CONTINUE
C
      DO 1 I=1,3
         C1(I,1)=B(I)
 1    CONTINUE
C
      DO 2 I=1,3
         C2(I,2)=B(I)
 2    CONTINUE
C
      DO 3 I=1,3
         C3(I,3)=B(I)
 3    CONTINUE
      X(1)=DET(C1)/DA
      X(2)=DET(C2)/DA
      X(3)=DET(C3)/DA
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE JFGAIN( P, STRAC, POT, ZARO, * )
C-----------------------------------------------------------------------
C
C     VERSION OF 21/11/77
C     FINDS ENERGY LOSS IN ABSORBER WITH POT AND ZARO
C     AND INCREASES ENERGY.
C     P IS CHANGED .
C                      COPIED FROM F22ELS   21/05/1980
C
C
      DIMENSION P(6)
C
      DATA D / .1535E-4 /
C
      IF(P(6).LT.0.) RETURN1
C  FIND REDUCED VARIABLES
      A = 2. * D * ZARO * STRAC / P(5)
      B = 1.022E6 / POT
      XLOW = P(4) / P(5)
      IF( XLOW * XLOW .GT. 16.9205 ) GO TO 400
C
C  INITIALIZE GAUSS METHOD
      ITER = 0
      INCREM = 0
      DXOLD = 1.E13
      X = XLOW
C
  100 ITER = ITER + 1
      IF( ITER .GT. 15 ) RETURN1
      X2 = X * X
      X21 = X2 -1.
      FACLOG = ALOG( B * X21 )
      DX = ( XLOW - X + A * ( X2 / X21 * FACLOG - 1. ) ) /
     *     ( 1. + 2.*A/(X21*X21) * ( FACLOG - X2 ) )
      ADX = ABS( DX )
      INCREM = INCREM + 1
      IF( ADX .LE. DXOLD ) INCREM = 0
      IF( INCREM .GE. 9 ) RETURN1
      DXOLD = ADX
      X = X + DX
      IF( ADX .GT. 0.001 * ABS(X) ) GO TO 100
C
  200 P(4) = X * P(5)
      PTOT = SQRT( P(4)*P(4) - P(5)*P(5) )
      FACT = PTOT / P(6)
      P(6) = PTOT
      DO 300 I = 1,3
  300 P(I) = P(I) * FACT
      RETURN
C
  400 XLOW2 = XLOW * XLOW
      X = XLOW + A*(XLOW2/(XLOW2-1.)*ALOG(B*(XLOW2-1.)) - 1. )
      GO TO 200
C
      END
C
C
C----------------------------------------------------------------------
       SUBROUTINE PATTRK(INDPR,*,*)
C----------------------------------------------------------------------
C            08/03/1980  11:40   CODED BY    S.KOMAMIYA
C            17/03/1980  01:30   MODIFIED BY S.KOMAMIYA
C                  CENTER OF THE CIRCLE
C            03/04/1980  06:10   MODIFIED BY S.KOMAMIYA
C                  ZMIN IN VTRK(19,I)
C            24/06/1980  02:15   MODIFIED BY S.KOMAMIYA
C
C    COPIED FROM F22KOM.KS(PATTRK1)     26.11.1985
C
C
C CONTENTS OF VTRK / KTRK :
C
C  VTRK(1,..):  X OF FIRST MEASURED POINT
C  VTRK(2,..):  Y OF FIRST MEASURED POINT
C  VTRK(3,..):  Z OF FIRST MEASURED POINT
C  VTRK(4,..):  DIRECTION COSINE AT FIRST MEASURED POINT
C  VTRK(5,..):  DIRECTION COSINE AT FIRST MEASURED POINT
C  VTRK(6,..):  DIRECTION COSINE AT FIRST MEASURED POINT
C  VTRK(7,..):  AVERAGE CURVATURE OF TRACK (SIGNED)
C  VTRK(8,..):  ERROR ON AVERAGE CURVATURE
C  VTRK(9,..):  CURVATURE OF TRACK (SIGNED) AT FIRST MEASURED POINT
C  VTRK(10,..): RADIUS OF CURVATURE (SIGNED) AT FIRST MEASURED POINT
C  VTRK(11,..): RADIUS OF CURVATURE (SIGNED) FOR AVERAGE
C  VTRK(12,..): X COORDINATE OF CENTER OF CURVATURE
C  VTRK(13,..): Y COORDINATE OF CENTER OF CURVATURE
C  VTRK(14,..): NOT USED
C  KTRK(15,..): TYPE OF RFI FIT:  1 CIRCLE, 2 PARABOLA
C  KTRK(16,..): NOT USED
C  KTRK(17,..): NR OF HITS IN THE RFI FIT
C  VTRK(18,..): RESIDUAL OF ZR FIT
C  VTRK(19,..): Z INTERCEPT OF ZR FIT
C  KTRK(20,..): MEASURE OF ORIGIN ABOVE OR BELOW VERTEX, SET IN KOMAKS
C
C
C
C
       COMMON/BCS/ IDATA(35000)
       DIMENSION ADATA(35000)
       INTEGER *2 HDATA(70000)
       EQUIVALENCE (IDATA(1),ADATA(1)),(HDATA(1),IDATA(1))
C
       COMMON/VTR/VTRK(20,100)
       DIMENSION KTRK(20,100)
       EQUIVALENCE (VTRK(1,1),KTRK(1,1))
            DO 10 I= 1,20
            DO 20 J= 1,100
            VTRK(I,J)= 0.0
 20         CONTINUE
 10         CONTINUE
C
       L0  =IDATA(INDPR+1)
       NTR =IDATA(INDPR+2)
       LTR =IDATA(INDPR+3)
C
       IF(NTR.LE.0) RETURN1
       IF(NTR.GT.100) RETURN2
C
C===== CHARGED TRACK LOOP
C
      DO 30 I=1,NTR
      LI=INDPR+L0+(I-1)*LTR
      VTRK(1,I)=ADATA(LI+5)
      VTRK(2,I)=ADATA(LI+6)
      VTRK(3,I)=ADATA(LI+7)
      VTRK(4,I)=ADATA(LI+8)
      VTRK(5,I)=ADATA(LI+9)
      VTRK(6,I)=ADATA(LI+10)
      VTRK(7,I)=ADATA(LI+25)
      VTRK(8,I)=ADATA(LI+26)
      VTRK(9,I)=ADATA(LI+27)
C
         VTRK(10,I)=10000000.
      IF(VTRK(9,I).NE.0.) VTRK(10,I)=1./VTRK(9,I)
         VTRK(11,I)=10000000.
      IF(VTRK(7,I).NE.0.) VTRK(11,I)=1./VTRK(7,I)
C
C
C
C     IF(VTRK(6,I).GT.1) GO TO 30
C        SNTH=SQRT(1.-VTRK(6,I)**2)
C        SNPH=VTRK(5,I)/SNTH
C        CNPH=VTRK(4,I)/SNTH
C
C     VTRK(12,I)=VTRK(1,I)+VTRK(10,I)*SNPH
C     VTRK(13,I)=VTRK(2,I)-VTRK(10,I)*CNPH
C
      KTRK(15,I)=IDATA(LI+18)
      IF(KTRK(15,I).EQ.2) GO TO 5
C CIRCLE FIT HERE
      D0=ABS(ADATA(LI+20)+1./ADATA(LI+19))
      VTRK(12,I)=D0*COS(ADATA(LI+21))
      VTRK(13,I)=D0*SIN(ADATA(LI+21))
      GO TO 50
 5    CONTINUE
C PARABOLA FIT HERE
      VTRK(12,I)=ADATA(LI+20)+VTRK(10,I)*SIN(ADATA(LI+19))
      VTRK(13,I)=ADATA(LI+21)-VTRK(10,I)*COS(ADATA(LI+19))
C
C
C
 50   CONTINUE
      KTRK(17,I)=IDATA(LI+24)
      VTRK(18,I)=ADATA(LI+32)
C
       ZMIN=ADATA(LI+30)
       VTRK(19,I)=ZMIN
C      WRITE(6,1000)I,ZMIN,(VTRK(J,I),J=10,13)
C1000  FORMAT(' /PATTRK/ I=',I3,' ZMIN,VTRK',5F11.3)
C
C
C
 30   CONTINUE
C
C
C
        RETURN
        END
C-----------------------------------------------------------------------
      FUNCTION RBPIPE(X0BTWN)
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C
C RETURN THE CURRENT BEAMPIPE RADIUS, BASED ON DATE IN HEAD BANK
C ARGUMENT X0BTWN GIVES AMOUNT OF RADIATION LENGTH BETWEEN PIPE AND TANK
C
#include "cdata.for"
#include "cgeo1.for"
#include "cgeov.for"
C
      IH2 = 2*IDATA(IBLN('HEAD'))
C
      IDAG = HDATA(IH2+7)
      IAAR = HDATA(IH2+8)
      RBPIPE = RPIP
      X0BTWN = 0.16
      IF(IAAR.LT.1984) GO TO 3200
      IF(IAAR.EQ.1984.AND.IDAG.LT.5) GO TO 3200
C NEW GEOMETRY
      RBPIPE = RPIPV
      X0BTWN = 0.1312
3200  CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE RFDRPZ( NR, NTRK, IER )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN      1/11/79 :  CONVERT COORDS OF NTRK TO R-PHI-Z
C
C        MOD: E. ELSEN     19/09/80 :
C        MOD: P. STEFFEN   16/12/83 :  GET NEW RECALIBRATED HITS (R-PHI)
C   LAST MOD: C. BOWDERY   16/12/83 :  RECOMMENTING
C
C        CONVERT COORDINATES OF TRACK NTRK TO R-PHI-Z-COORDINATES
C        RESULT IS STORED IN BANK RFWK, 1.
C        BASIS FOR CONVERSION IS 'JHTL', NR.
C
C        RW(1) = RADIUS OF POINT
C        RW(2) = PHI VALUE OF POINT
C        RW(3) = Z
C        HW(7) = Z ERROR FLAG   ( 0 FOR GOOD Z COORDINATE )
C        HW(8) = NUMBER OF HIT
C
C        IER   = 4,   NOT ENOUGH SPACE IN BCS
C              = 3,   JHTL,NR OR PATR,NR NOT PRESENT
C              = 2,   JETC,0 NOT PRESENT
C              = 1,   TRACK NOT FOUND
C              = 0,   ELSE
C
C        NEW VERSION TO PROVIDE VARIABLES FOR PEARCE'S VERSION OF
C        HITXYZ.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1)), (RW(1),IW(1))
C
#include "cdsmax.for"
C                                           COMMON FOR OLD-DATA
      COMMON / CRFDAT / NTRKOL, DATOLD, IDOLD, NLIOLD, IEROLD
C
      COMMON /CWRK/ WRK(2000)
                    DIMENSION IWRK(2000)
                    EQUIVALENCE(IWRK(1),WRK(1))
C
      DIMENSION RLIST(150), ILIST(150)
C
      DIMENSION RESUL(17),IRESUL(17)
      EQUIVALENCE (IRESUL(1),RESUL(1))
      DIMENSION HELP(2), BUFF(4)
      EQUIVALENCE (IHELP,HELP(1))
C
      DATA LEN / 400/
      DATA LBINIT / 0/
C
C-----------------------------------------------------------------------
C
C                                           INITIALIZATION
      IF(LBINIT.NE.0) GOTO 10
         LBINIT = 1
         IQJETC = IBLN('JETC')
         IQPATR = IBLN('PATR')
         IQJHTL = IBLN('JHTL')
         IQRFWK = IBLN('RFWK')
       WRITE(6,9992)
9992  FORMAT(' RFDRPZ CALLED, NEW VERSION FROM TPSOURCE..')
   10 CONTINUE
C                                           PATR
      IER = 3
      IPPATR = IW(IQPATR)
   12 CONTINUE
         IF(IPPATR.LE.0) GOTO 1500
         IF(IW(IPPATR-2) .EQ. NR) GOTO 14
         IPPATR = IW(IPPATR-1)
         GOTO 12
   14 CONTINUE
C                                           BUFFER OLD ID AND DATE
      IER = 1
      IF( NTRK .GT. IW(IPPATR+2) ) GO TO 1500
      IPTR   = IPPATR + IW(IPPATR+1) +(NTRK-1)*IW(IPPATR+3)
      NTRKOL = NTRK
      NLIOLD = IW(IPPATR+7)
      IDOLD  = IW(IPTR+2)
      DATOLD = RW(IPTR+3)
      IEROLD = IW(IPTR+48)
C     I0 = IPTR + 1
C     I9 = IPTR + 48
C     PRINT 2005, (IW(I1),I1=I0,I9)
C2005 FORMAT(1H0,2I3,I8,2(I4,3F6.1,3F6.3),
C    ,     /,14X,I3,4E13.5,F6.2,I3,4E13.5,
C    ,     /,14X,I3,2F8.3,F6.1,I3,10X,6I3,8I6,2X,Z4)
C                                           JHTL
      IER = 3
      IPJHTL = IW(IQJHTL)
   16 CONTINUE
         IF(IPJHTL.LE.0) GOTO 1500
         IF(IW(IPJHTL-2) .EQ. NR) GOTO 18
         IPJHTL = IW(IPJHTL-1)
         GOTO 16
   18 CONTINUE
C                                           JETC
      IER = 2
      IPJETC = IW(IQJETC)
      IF(IPJETC .LE. 1 ) GO TO 1500
C
C
C                                     FETCH HITS, CALCULATE COORDINATES,
C                                     FILL ARRAY IN /CWORK/
      IPCO0  = 1
      LHIT   = 14
      INDFET = 1
      CALL JFETCH(IPTR,IPJHTL,WRK(1),LHIT,IPRES,INDFET)
      IPCO9  = IPRES - 1
      NHIT = IPRES / LHIT
C     PRINT 2002, NHIT,(WRK(I),I=IPCO0,IPCO9)
C2002 FORMAT('0TRACK:',I6,/,(1X,3I6,4F8.3,I4,F8.3,2I4,F8.3,I6,F8.2))
C
C                                       > 0 HITS ?
      IER = 1
      IF(NHIT .LT. 1) GO TO 1500
C
C
C                                       LOOP OVER FETCH-HITS
C                                       AND STORE RADII FOR ORDERING
      NH1 = 0
      RR0 = 0.
      LBORD = 0
      DO 120 IPCO = IPCO0,IPCO9,LHIT
        IF(IWRK(IPCO+10).GT.0) GOTO 120
          NH1 = NH1 + 1
          ILIST(NH1) = IPCO
          RLIST(NH1) = WRK(IPCO+6)
          IF(WRK(IPCO+6).LT.RR0) LBORD = 1
          RR0        = WRK(IPCO+6)
  120 CONTINUE
C
C
C                                       > 0 HITS ?
      IF(NH1  .LE. 1) GO TO 1500
C
C                                       CHECK IF ORDERING NECESSAIRY
      IF(LBORD.EQ.0) GOTO 190
C                                       ORDER RADII
        M = NH1
  130   M = M / 2
        IF(M.EQ.0) GOTO 160
        K = NH1 - M
        DO 150 J=1,K
           I = J
  140      IF(I.LT.1) GOTO 150
           IF(RLIST(I+M).GE.RLIST(I)) GOTO 150
              RZW        = RLIST(I+M)
              RLIST(I+M) = RLIST(I  )
              RLIST(I  ) = RZW
              IZW        = ILIST(I+M)
              ILIST(I+M) = ILIST(I  )
              ILIST(I  ) = IZW
              I = I - M
           GOTO 140
  150   CONTINUE
        GOTO 130
  160 CONTINUE
C     PRINT 2008, LBORD,(I1,RLIST(I1),ILIST(I1),I1=1,NH1)
C2008 FORMAT('1ORDERING:',I6,/,(5(I8,F8.0,I4)))
C
  190 CONTINUE
C
C                                           RFWK POINTERS
C     CREATE RFWK-BANK IF NOT JET EXISTING
      IF(IW(IQRFWK).GT.0) GOTO 20
         CALL BCRE( NPRFWK, 'RFWK', 1, LEN, *8100, IERR )
         CALL BSAT( 1, 'RFWK' )
   20 CONTINUE
C                                           ADJUST LENGTH OF RFWK
C     CALL BPRS('RFWK',1)
      IER = 0
      NPRFWK = IW(IQRFWK)
      J = NH1*4 - IW(NPRFWK)
      IF(J.NE.0) CALL BCHM( NPRFWK, J, IERR )
      NPRFWK = IW(IQRFWK)
      IRFWK = NPRFWK
C
C     CALL BPRS('RFWK',1)
C
C
      DO 1000 IH1 = 1,NH1
C                                           FIND COORDINATES
          IPCO = ILIST(IH1)
          RW( IRFWK   + 1 ) = WRK(IPCO+6)
          PHI = ATAN2(WRK(IPCO+4),WRK(IPCO+3))
          RW( IRFWK   + 2 ) = PHI
          RW( IRFWK   + 3 ) = WRK(IPCO+5)
          HW( IRFWK*2 + 7 ) = IWRK(IPCO+ 7)
          HW( IRFWK*2 + 8 ) = (IWRK(IPCO+1) - IPJETC*2 - 97) / 2 + 1
C         I1 = IRFWK + 1
C         RA  =  RW(I1  )
C         FI  =  RW(I1+1)
C         ZZ  =  RW(I1+2)
C         IZF =  HW(I1*2+5)
C         IPH =  HW(I1*2+6)
C         PRINT 2001, I2,I1,RA,FI,ZZ,IZF,IPH
C2001 FORMAT(' HIT',I3,I6,3F9.3,I4,I6)
          IRFWK = IRFWK + 4
 1000 CONTINUE
C
 1500 CONTINUE
C     PRINT 2003, IER
C2003 FORMAT('0RFDRPZ9:',I3)
C     I0 = NPRFWK + 1
C     I9 = IRFWK
C     I2 = 0
C     DO 90 I1=I0,I9,4
C        I2 = I2 + 1
C        RA  =  RW(I1  )
C        FI  =  RW(I1+1)
C        ZZ  =  RW(I1+2)
C        IZF =  HW(I1*2+5)
C        IPH =  HW(I1*2+6)
C        PRINT 2001, I2,I1,RA,FI,ZZ,IZF,IPH
C  90 CONTINUE
C
      RETURN
C
C                                           ERROR PART
 8100 WRITE(6,9101)
 9101 FORMAT(/' ++++ RFDRPZ ++++      NOT ENOUGH SPACE TO ACCOMODATE ALL
     * HITS IN BANK RFWK,1 '//)
      IER = 4
      RETURN
      END
C-----------------------------------------------------------------------
      FUNCTION SCATT(P,DZ)
C-----------------------------------------------------------------------
C
C     S.KOMAMIYA      15.03.1980    00:10
C     MODIFIED        17.03.1980    15:30
C     MODIFIED        01.04.1980    00:30   SQRT(SQRT(1.-DZ**2))
C
C   COPIED FROM F22KOM.KS(SCATT)    26.11.1985
C
      DATA PIMASS/0.13957/
      E=SQRT(P**2+PIMASS**2)
      SCATT=0.006/(P**2/E)/SQRT(SQRT(1.-DZ**2))
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE TWOCIR(A1,B1,R1,A2,B2,R2,V1,V2,DIST,IFLAG)
C-----------------------------------------------------------------------
C     SUBROUTINE TO CALCULATE INTERSECTION POINTS OF THE TWO CIRCLE
C     S.KOMAMIYA      07.03.1980    13:45
C     MODIFIED        14.03.1980    14:45
C
C    COPIED FROM F22KOM.KS(TWOCIR)   26.11.1985
C
C                          DIRECTION VECTORS AT VERTEX POINTS
C
C
C   INPUT PARAMETERS
C     A1  :X-COORD. OF CENTER OF CIRCLE1
C     B1  :Y-COORD. OF CENTER OF CIRCLE1
C     R1  :RADIUS OF CIRCLE1      (SIGNED!)
C     A2  :X-COORD. OF CENTER OF CIRCLE2
C     B2  :Y-COORD. OF CENTER OF CIRCLE2
C     R2  :RADIUS OF CIRCLE2
C
C
C   INTERNAL PARAMETERS
C
C
C   OUTPUT  PARAMETERS
C
C     V1(1) : X-COOR. OF VERTEX1
C     V1(2) : Y-COOR. OF VERTEX1
C     V1(3) : X-COMPONENT OF MOMENTUM DIRECTION AT VERTEX1 (PARTICLE 1)
C     V1(4) : Y-COMPONENT OF MOMENTUM DIRECTION AT VERTEX1 (PARTICLE 1)
C     V1(5) : X-COMPONENT OF MOMENTUM DIRECTION AT VERTEX1 (PARTICLE 2)
C     V1(6) : Y-COMPONENT OF MOMENTUM DIRECTION AT VERTEX1 (PARTICLE 2)
C     V2(1) : X-COOR. OF VERTEX2
C     V2(2) : Y-COOR. OF VERTEX2
C     V2(3) : X-COMPONENT OF MOMENTUM DIRECTION AT VERTEX2 (PARTICLE 1)
C     V2(4) : Y-COMPONENT OF MOMENTUM DIRECTION AT VERTEX2 (PARTICLE 1)
C     V2(5) : X-COMPONENT OF MOMENTUM DIRECTION AT VERTEX2 (PARTICLE 2)
C     V2(6) : Y-COMPONENT OF MOMENTUM DIRECTION AT VERTEX2 (PARTICLE 2)
C
C     DIST(1) : X OF THE NEAREST POINT ON LINE V1-V2 FROM THE ORIGIN
C     DIST(2) : Y OF THE NEAREST POINT ON LINE V1-V2 FROM THE ORIGIN
C
C
C    IFLAG = 0     : NORMAL TWO INTERSECTION POINTS
C          = 1     : ONLY ONE INTERSECTION POINT
C          = 10    : TWO CIRCLES DO NOT INTERSECT
C          = 100   :
C          = 1000  :
C
C
C
      DIMENSION V1(6)
      DIMENSION V2(6)
      DIMENSION DIST(2)
C===== INITIALIZATION
      DO 100 K=1,6
         V1(K)=0.
         V2(K)=0.
 100     CONTINUE
         IFLAG=-1000
C===== CALCULATE INTERSECTION POINTS
         AA =2.*(A2-A1)
         BB =2.*(B2-B1)
         CC1=A1**2+B1**2-R1**2
         CC2=A2**2+B2**2-R2**2
         CC=CC1-CC2
      IF(BB.NE.0.) GO TO 10
       IF(AA.NE.0.) GO TO 2
         IFLAG=100
         RETURN
C    IDENTICAL CIRCLES
 2       V1(1)=-CC/AA
         V2(1)=V1(1)
         D=R1**2-(CC/AA+A1)**2
         IF(D) 3,4,5
 3       IFLAG=100
         RETURN
C
 4       IFLAG=1
         GO TO 6
C
 5       IFLAG=0
 6       V1(2)=B1+SQRT(D)
         V2(2)=B1-SQRT(D)
         GO TO 200
C
 10      CONTINUE
         A=1.+(AA/BB)**2
         B=-2.*A1+2.*AA/BB*(CC/BB+B1)
         C=A1**2+(CC/BB+B1)**2-R1**2
         D=B**2-4.*A*C
         IF(D)11,12,13
 11      IFLAG=10
         RETURN
 12      IFLAG=1
         GO TO 14
 13      IFLAG=0
 14      V1(1)=(-B+SQRT(D))/2./A
         V2(1)=(-B-SQRT(D))/2./A
         V1(2)=-(AA*V1(1)+CC)/BB
         V2(2)=-(AA*V2(1)+CC)/BB
C
C===== GET DIRECTION OF THE MOMENTUM AT VERTICES
C   ESSENTIALLY X,Y COMPONENTS OF RADIUS OF CURVATURE
C   SWAP DIRECTION ACCORDING TO CHARGE (SIGN OF RADIUS CURV.)
C
 200     CONTINUE
C===== VERTEX-1
         V1(3)=B1-V1(2)
         V1(4)=V1(1)-A1
      IF(R1) 22,300,21
 21      V1(3)=-V1(3)
         V1(4)=-V1(4)
 22     CONTINUE
         V1(5)=B2-V1(2)
         V1(6)=V1(1)-A2
      IF(R2) 24,300,23
 23      V1(5)=-V1(5)
         V1(6)=-V1(6)
 24     CONTINUE
C===== VERTEX-2
         V2(3)=B1-V2(2)
         V2(4)=V2(1)-A1
      IF(R1) 26,300,25
 25      V2(3)=-V2(3)
         V2(4)=-V2(4)
 26     CONTINUE
         V2(5)=B2-V2(2)
         V2(6)=V2(1)-A2
      IF(R2) 28,300,27
 27      V2(5)=-V2(5)
         V2(6)=-V2(6)
 28     CONTINUE
         DIST(1)=-CC*AA/(AA**2+BB**2)
         DIST(2)=-CC*BB/(AA**2+BB**2)
 300  CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE VTX(NRUN,X,Y)
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2(H)
C     THIS ROUTINE RETURNS THE X AND Y COORDS OF THE RUN VERTEX.
C     FROM COMMON CALIBR (ON PATRECSR....THIS ALSO CONTAINS THE
C     ARRAY DECLARATIONS AND EQUIVILENCES.
C     NRUN IS A DUMMY VARIABLE TO SATISFY KOMAMIYAS CALL VTX
C
C
C                    WRITTEN 9.04.86
C
#include "calibr.for"
C
C                              FIND POINTER IN CALIB ARRAY
C
      IPVTX = ICALIB(10)
C
C                              NOW GET COORDS
C
      X     = ACALIB( IPVTX + 1)
      Y     = ACALIB( IPVTX + 3)
C
C
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE XYSCAT(NRUN,I,STDEV,R1,R2,RMINI,MFLAG)
C-----------------------------------------------------------------------
C
C     S.KOMAMIYA      12.05.1980    00:30
C     MODIFIED        13.05.1980    22:30     MAIN XY-VERTEX
C     COPIED FROM F22KOM.KS(XYSCAT6)     26.11.1985
C     MODIFIED TO TAKE ACCOUTN OF VARYING BEAMPIPE GEOMETRY  J.OLSSON
C     MODIFIED        07.11.1986    FOR WERTEX USE NEED 40 ENTRIES
C                                   IN CWORK1 FOR EACH TRACK  PH
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C  R1,R2 ARE MEASURES OF SPREAD OF TRACK ORIGIN, DUE TO SCATTERING
C  IF R1*R2 < 0, THE RUN VERTEX IS COMPATIBLE WITH BEING THE TRACKORIGIN
C   THE VARIABLE STDEV MAGNIFIES THIS MEASURE, I.E. THE LARGER STDEV,
C   THE CLEARER THE V0 CANDIDATES MUST BE SEPARATED FROM THE MAIN VERTEX
C
      COMMON/VTR/VTRK(20,100)
      DIMENSION KTRK(20,100)
      EQUIVALENCE (VTRK(1,1),KTRK(1,1))
C--- USE DITTMANN'S VERTEX --------------- 16/07/1980 ------------------
C-----------   FILL /CWORK1/ -------------------------------------------
      COMMON /CWORK1/NT,T(2000)
C-----------------------------------------------------------------------
C
      DIMENSION VI1(6),VI2(6),VI(6)
      DIMENSION DISTI(2)
      COMMON/MAG/BC
C     DATA RMAT/152.4/
      RMAT = RBPIPE(X0BTWN)
      R1=0.
      R2=0.
C     R3=0.
C     R4=0.
C  RUN VERTEX   (CALLS VRTPOS)
      CALL VTX(NRUN,X0,Y0)
C
C
C
       AI=VTRK(12,I)
       BI=VTRK(13,I)
       RI=VTRK(10,I)
C------------------------------------- 29/05/1980 ----------------------
       RMINI=ABS(SQRT((AI-X0)**2+(BI-Y0)**2)-ABS(RI))
C-----------------------------------------------------------------------
       CALL TWOCIR(AI,BI,RI,0.,0.,RMAT,VI1,VI2,DISTI,IFLAGI)
       IF(IFLAGI.GT.0) GO TO 6002
       SIGVI1=VI1(1)*VI1(3)+VI1(2)*VI1(4)
       SIGVI2=VI2(1)*VI2(3)+VI2(2)*VI2(4)
       IF(SIGVI1)20,20,10
 10    DO 11 K=1,6
       VI(K)=VI1(K)
 11    CONTINUE
       GO TO 30
 20    IF(SIGVI2)6003,6003,21
 21    DO 22 K=1,6
       VI(K)=VI2(K)
 22    CONTINUE
 30    CONTINUE
C
C
C
      PTI=ABS(VTRK(10,I))*BC
      DZI=VTRK(6,I)
      COSZI=SQRT(1.-DZI**2)
      IF(COSZI .LT. 0.000000001) GO TO 6002
      PI=PTI/COSZI
      SPHIS=SCATT(PI,DZI)
C------------------------------------------- 29/01/1981 ----------------
      IDITT=(I-1)*40
      SFITI=T(IDITT+8)
      SPHI1=SQRT(SPHIS**2+SFITI**2)
      SPHI=SQRT(SPHIS**2+SFITI**2)*STDEV
C     WRITE(6,1000) I,SFITI,SPHIS,SPHI1,SPHI
C1000 FORMAT(' I=',I3,'  (FIT,SCATT,SIGPHI)=',3F10.4,'  *STD',F10.4)
C-----------------------------------------------------------------------
      COS1=COS(SPHI)
      SIN1=SIN(SPHI)
C     COS2=COS(2.*SPHI)
C     SIN2=SIN(2.*SPHI)
      AI1P= COS1*(AI-VI(1))+SIN1*(BI-VI(2))+VI(1)
      BI1P=-SIN1*(AI-VI(1))+COS1*(BI-VI(2))+VI(2)
      AI1M= COS1*(AI-VI(1))-SIN1*(BI-VI(2))+VI(1)
      BI1M= SIN1*(AI-VI(1))+COS1*(BI-VI(2))+VI(2)
C     AI2P= COS2*(AI-VI(1))+SIN2*(BI-VI(2))+VI(1)
C     BI2P=-SIN2*(AI-VI(1))+COS2*(BI-VI(2))+VI(2)
C     AI2M= COS2*(AI-VI(1))-SIN2*(BI-VI(2))+VI(1)
C     BI2M= SIN2*(AI-VI(1))+COS2*(BI-VI(2))+VI(2)
      R1=SQRT((AI1P-X0)**2+(BI1P-Y0)**2)-ABS(RI)
      R2=SQRT((AI1M-X0)**2+(BI1M-Y0)**2)-ABS(RI)
C     R3=SQRT((AI2P-X0)**2+(BI2P-Y0)**2)-ABS(RI)
C     R4=SQRT((AI2M-X0)**2+(BI2M-Y0)**2)-ABS(RI)
CCCCCCCCCCCCCCCC ONLY FOR TEST
C     WRITE(6,1000)I,PI,SPHI,R1,R2,R3,R4,BC
C1000 FORMAT('/XYSCAT/ I=',I3,' PI=',F7.3,' SPHI=',F7.3,' R(4)=',5F10.3)
CCCCCCCCCCCCCCCC
      GO TO 5000
C
 6001 MFLAG=201
      R1=0.
      R2=0.
C     R3=0.
C     R4=0.
      RETURN
 6002 MFLAG=202
      RETURN
 6003 MFLAG=203
      RETURN
C===
 5000 MFLAG=0
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE XYVOPT(I,J,V,DVL,D,CHI1,ALPH12,PHUV,KFLAG)
C-----------------------------------------------------------------------
C
C     S.KOMAMIYA      15.03.1980    00:10
C     MODIFIED        17.03.1980    15:30
C     MODIFIED        30.12.1980    17:30
C     SUBROUTINE TO OPTIMIZE VERTEX X,Y-POSITION OF TWO TRACKS
C     ---------- VERTEX IBIRI -----------
C
C
C    KFLAG = 0     : NORMAL END
C          = 1     :
C          = **    :
C          = 100   :
C          = 201   :   OUTSIDE OF BEAM PIPE
C
C
C    COPIED FROM F22KOM.KS(XYVOPT6)     27.11.1985
C    UPDATED FOR WERTEX USE             07.11.86    PAUL HILL
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      COMMON/VTR/VTRK(20,100)
      DIMENSION KTRK(20,100)
      EQUIVALENCE (VTRK(1,1),KTRK(1,1))
C
      DIMENSION VI1(6),VI2(6),VI(6),VJ1(6),VJ2(6),VJ(6)
      DIMENSION DISTI(2),DISTJ(2),DIST(2),V1(6),V2(6),V(6)
C----------
      DIMENSION U1(6),U2(6)
      DIMENSION DIST0(2)
C----------
      DIMENSION VBY(6)
C
      COMMON /CWORK1/ NT,T(2000)
C-----------------------------------------------------------------------
      COMMON/MAG/BC
C      DATA RMAT/152.4/
C
       RMAT = RBPIPE(X0BTWN)
C---------------------------- 29/04/1980 ------- BY PASS ---------------
      DO 9999 LLL=1,6
      VBY(LLL)=V(LLL)
 9999 CONTINUE
       D0=D
       DVL0=DVL
C-----------------------------------------------------------------------
C   NOTE:  D AND DVL ARE NOT UPDATED IN THIS ROUTINE, ALTHOUGH THEY
C          COULD HAVE BEEN!
C-----------------------------------------------------------------------
       KFLAG=-1000
       VO=SQRT(V(1)**2+V(2)**2)
       IF(VO.GE.RMAT) GO TO 6001
C
C
C
        AI=VTRK(12,I)
        BI=VTRK(13,I)
        RI=VTRK(10,I)
        AJ=VTRK(12,J)
        BJ=VTRK(13,J)
        RJ=VTRK(10,J)
       CALL TWOCIR(AI,BI,RI,AJ,BJ,RJ,U1,U2,DIST0,IFLAG0)
       IF(IFLAG0.GT.0) GO TO 6002
       CALL TWOCIR(AI,BI,RI,0.,0.,RMAT,VI1,VI2,DISTI,IFLAGI)
       IF(IFLAGI.GT.0) GO TO 6002
       SIGVI1=VI1(1)*VI1(3)+VI1(2)*VI1(4)
       SIGVI2=VI2(1)*VI2(3)+VI2(2)*VI2(4)
       IF(SIGVI1)20,20,10
 10    DO 11 K=1,6
       VI(K)=VI1(K)
 11    CONTINUE
       GO TO 30
 20    IF(SIGVI2)6003,6003,21
 21    DO 22 K=1,6
       VI(K)=VI2(K)
 22    CONTINUE
 30    CONTINUE
       CALL TWOCIR(AJ,BJ,RJ,0.,0.,RMAT,VJ1,VJ2,DISTJ,IFLAGJ)
       IF(IFLAGJ.GT.0) GO TO 6004
       SIGVJ1=VJ1(1)*VJ1(3)+VJ1(2)*VJ1(4)
       SIGVJ2=VJ2(1)*VJ2(3)+VJ2(2)*VJ2(4)
       IF(SIGVJ1)40,40,31
 31    DO 32 K=1,6
       VJ(K)=VJ1(K)
 32    CONTINUE
       GO TO 50
 40    IF(SIGVJ2)6005,6005,41
 41    DO 42 K=1,6
       VJ(K)=VJ2(K)
 42    CONTINUE
 50    CONTINUE
C
C
C
      PTI=ABS(VTRK(10,I))*BC
      PTJ=ABS(VTRK(10,J))*BC
C
      DZI=VTRK(6,I)
      DZJ=VTRK(6,J)
C
      PI=PTI/SQRT(1.-DZI**2)
      PJ=PTJ/SQRT(1.-DZJ**2)
C
      SPHI1=SCATT(PI,DZI)
      SPHJ1=SCATT(PJ,DZJ)
C------------------------------------------- 16/07/1980 ----------------
      IDITT=(I-1)*40
      CALL VTXPNT(IDITT,VI(1),VI(2),XI,YI,ZI,QXI,QYI,QZI,PHI,DPHI,STI)
      SPHI=SPHI1
C     IF(IFLAG.NE.0) GO TO 61
C   T(IDITT+8) CONTAINS THE ERROR IN FI FROM VTXPRE
      SPHI=SQRT(SPHI1**2+T(IDITT+8)**2)
C61    CONTINUE
C-----------------------------------------------------------------------
C------------------------------------------- 16/07/1980 ----------------
      JDITT=(J-1)*40
      CALL VTXPNT(JDITT,VJ(1),VJ(2),XJ,YJ,ZJ,QXJ,QYJ,QZJ,PHJ,DPHJ,STJ)
      SPHJ=SPHJ1
C     IF(JFLAG.NE.0) GO TO 62
      SPHJ=SQRT(SPHJ1**2+T(JDITT+8)**2)
C62   CONTINUE
C-----------------------------------------------------------------------
C
      SINPHI=VI(4)/SQRT(VI(3)**2+VI(4)**2)
      SINPHJ=VJ(4)/SQRT(VJ(3)**2+VJ(4)**2)
C
      COSPHI=VI(3)/SQRT(VI(3)**2+VI(4)**2)
      COSPHJ=VJ(3)/SQRT(VJ(3)**2+VJ(4)**2)
C
      AA1=(VJ(1)*COSPHJ+VJ(2)*SINPHJ)*RJ
      BB1=-(VI(1)*COSPHI+VI(2)*SINPHI)*RI
      CCI1=(VI(1)*SINPHI-VI(2)*COSPHI)*RI
      CCJ1=(VJ(1)*SINPHJ-VJ(2)*COSPHJ)*RJ
      CC1=CCJ1-CCI1
      IF(AA1.EQ.0. .OR. BB1.EQ.0.) GO TO 6006
      DPHJ1=-AA1*CC1/(BB1**2/SPHJ**2+AA1**2/SPHI**2)/SPHI**2
      DPHI1=-(AA1*DPHJ1+CC1)/BB1
C     AI=VI(1)+RI*(SINPHI*COS(DPHI)+COSPHI*SIN(DPHI))
C     BI=VI(2)-RI*(COSPHI*COS(DPHI)-SINPHI*SIN(DPHI))
C     AJ=VJ(1)+RJ*(SINPHJ*COS(DPHJ)+COSPHJ*SIN(DPHJ))
C     BJ=VJ(2)-RJ*(COSPHJ*COS(DPHJ)-SINPHJ*SIN(DPHJ))
      AAI1=VI(1)+RI*(SINPHI+COSPHI*DPHI1)
      BBI1=VI(2)-RI*(COSPHI-SINPHI*DPHI1)
      AAJ1=VJ(1)+RJ*(SINPHJ+COSPHJ*DPHJ1)
      BBJ1=VJ(2)-RJ*(COSPHJ-SINPHJ*DPHJ1)
C
      CHI1=(DPHI1/SPHI)**2+(DPHJ1/SPHJ)**2
C
      AA2=(VI(1)*COSPHI+VI(2)*SINPHI)*RI
      BB2=-(VJ(1)*COSPHJ+VJ(2)*SINPHJ)*RJ
      CCI2=(VI(1)*SINPHI-VI(2)*COSPHI)*RI
      CCJ2=(VJ(1)*SINPHJ-VJ(2)*COSPHJ)*RJ
      CC2=CCI2-CCJ2
      IF(AA2.EQ.0. .OR. BB2.EQ.0.) GO TO 6006
      DPHI2=-AA2*CC2/(BB2**2/SPHI**2+AA2**2/SPHJ**2)/SPHJ**2
      DPHJ2=-(AA2*DPHI2+CC2)/BB2
C     AI=VI(1)+RI*(SINPHI*COS(DPHI)+COSPHI*SIN(DPHI))
C     BI=VI(2)-RI*(COSPHI*COS(DPHI)-SINPHI*SIN(DPHI))
C     AJ=VJ(1)+RJ*(SINPHJ*COS(DPHJ)+COSPHJ*SIN(DPHJ))
C     BJ=VJ(2)-RJ*(COSPHJ*COS(DPHJ)-SINPHJ*SIN(DPHJ))
      AAI2=VI(1)+RI*(SINPHI+COSPHI*DPHI2)
      BBI2=VI(2)-RI*(COSPHI-SINPHI*DPHI2)
      AAJ2=VJ(1)+RJ*(SINPHJ+COSPHJ*DPHJ2)
      BBJ2=VJ(2)-RJ*(COSPHJ-SINPHJ*DPHJ2)
C
      CHI2=(DPHI2/SPHI)**2+(DPHJ2/SPHJ)**2
C
C
C
C
      VO=0.
      CALL TWOCIR(AAI1,BBI1,RI,AAJ1,BBJ1,RJ,V1,V2,DIST,IFLAGK)
       IF(IFLAGK.NE.0) GO TO 6000
C--------------------------------------
       X120=U1(1)-U2(1)
       Y120=U1(2)-U2(2)
       X12 =V1(1)-V2(1)
       Y12 =V1(2)-V2(2)
       SS0=SQRT(X120**2+Y120**2)
       SS=SQRT(X12**2+Y12**2)
       COS12=(X120*X12+Y120*Y12)/SS0/SS
       COS12=ABS(COS12)
       ALPH12=ARCOS(COS12)
C-----------------------------------------
        PV1X=V1(3)+V1(5)
        PV1Y=V1(4)+V1(6)
        PV2X=V2(3)+V2(5)
        PV2Y=V2(4)+V2(6)
C
       SIGV1=V1(1)*PV1X+V1(2)*PV1Y
       SIGV2=V2(1)*PV2X+V2(2)*PV2Y
C
       R1STI=SQRT(VTRK(1,I)**2+VTRK(2,I)**2)
       R1STJ=SQRT(VTRK(1,J)**2+VTRK(2,J)**2)
       R1ST=AMIN1(R1STI,R1STJ)
C
       VO1=SQRT(V1(1)**2+V1(2)**2)
       VO2=SQRT(V2(1)**2+V2(2)**2)
C
C
      IF(SIGV1.GE.0. .OR. SIGV2.GE.0) GO TO 200
      IF(VO1.GT.VO2) GO TO 110
      IF(VO1.LT.10.) GO TO 1000
      GO TO 3001
 110  IF(VO2.LT.10.) GO TO 2000
      GO TO 3002
C
 200  IF(SIGV1.LE.0. .OR. SIGV2.GE.0) GO TO 250
      IF(VO2.GT.20.) GO TO 210
      IF(VO1.LT.R1ST) GO TO 1000
      IF(VO1.GT.R1ST) GO TO 2000
 210  CONTINUE
      IF(VO1.LE.R1ST) GO TO 1000
      GO TO 3003
C
 250  IF(SIGV2.LE.0. .OR. SIGV1.GE.0) GO TO 300
      IF(VO1.GT.20.) GO TO 260
      IF(VO2.LT.R1ST) GO TO 2000
      IF(VO2.GT.R1ST) GO TO 1000
 260  CONTINUE
      IF(VO2.LE.R1ST) GO TO 2000
      GO TO 3004
C
 300  IF(SIGV1.LE.0. .OR. SIGV1.LE.0) GO TO 400
      IF(VO1.GE.VO2) GO TO 350
      IF(VO1.LE.R1ST) GO TO 1000
      GO TO 3005
 350  IF(VO2.GE.VO1) GO TO 400
      IF(VO2.LE.R1ST) GO TO 2000
      GO TO 3006
C
 400  IF(VO1.EQ.0.) GO TO 1000
      IF(VO2.EQ.0.) GO TO 2000
      GO TO 3007
C=== V1-QUE
 1000 DO 1100 K=1,6
      V(K)=V1(K)
 1100 CONTINUE
      VO=VO1
      SIGV=SIGV1
      GO TO 5000
C=== V2-QUE
 2000 DO 2100 K=1,6
      V(K)=V2(K)
 2100 CONTINUE
      VO=VO2
      SIGV=SIGV2
      GO TO 5000
C=== JUNK-VRETEX QUE
 3001 KFLAG=11
      RETURN
 3002 KFLAG=12
      RETURN
 3003 KFLAG=13
      RETURN
 3004 KFLAG=14
      RETURN
 3005 KFLAG=15
      RETURN
 3006 KFLAG=16
      RETURN
 3007 KFLAG=17
      RETURN
 4000 KFLAG=100
      RETURN
 6001 KFLAG=201
C---------------------------- 29/04/1980 ------- BY PASS ---------------
C     DO 9998 LLL=1,6
C     V(LLL)=VBY(LLL)
C9998 CONTINUE
C-----------------------------------------------------------------------
      CHI2=0.0
      CHI1=0.0
      RETURN
 6002 KFLAG=202
      RETURN
 6003 KFLAG=203
      RETURN
 6004 KFLAG=204
      RETURN
 6005 KFLAG=205
      RETURN
 6006 KFLAG=206
      RETURN
 6000 KFLAG=200
      RETURN
C=== SARANARU VERTEX-IBIRI
 5000 KFLAG=0
C
      UX=U1(1)-U2(1)
      UY=U1(2)-U2(2)
      UR=SQRT(UX**2+UY**2)
      UX=UX/UR
      UY=UY/UR
      VX=V1(1)-V2(1)
      VY=V1(2)-V2(2)
      VR=SQRT(VX**2+VY**2)
      VX=VX/VR
      VY=VY/VR
      SINUV=ABS(UX*VY-UY*VX)
      PHUV =ARSIN(SINUV)
C---------------------------- 29/04/1980 ------- BY PASS ---------------
      DO 9997 LLL=1,6
      V(LLL)=VBY(LLL)
 9997 CONTINUE
C-----------------------------------------------------------------------
      IF(VO.GT.600.) KFLAG=1
      DVL=DVL0
      D=D0
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE XYVRTX(I,J,V,DVL,D,JFLAG,IFLAG)
C-----------------------------------------------------------------------
C
C     S.KOMAMIYA      08.03.1980    12:30
C     MODIFIED        10.03.1980    01:30
C     MODIFIED        17.07.1980    23:30
C     SUBROUTINE TO CALCULATE VERTEX POSITION OF TWO TRACKS
C
C    IFLAG = 0     : NORMAL TWO INTERSECTION POINTS
C          = 1     : ONLY ONE INTERSECTION POINT
C          = 10    : TWO CIRCLES DO NOT INTERSECT
C
C    JFLAG = 0     : NORMAL END
C          = 1     : MOMENTUM DIRECTION AT VERTEX IS INCONSISTENT
C          = 10    : VERTEX IS TOO FAR FROM THE ORIGIN
C          = 100   : TWO PARTICLES :I,J HAVE SAME SIGN OF CHARGE
C
C
C    COPIED FROM F22KOM.KS(XYVRT6)     27.11.1985
C
      COMMON/VTR/VTRK(20,100)
      DIMENSION KTRK(20,100)
      EQUIVALENCE (VTRK(1,1),KTRK(1,1))
C
      DIMENSION V1(6),V2(6),V(6),DIST(2)
C===== INITIALIZATION
      DO 1 K=1,6
         V(K)=0.
 1       CONTINUE
         JFLAG=-1000
C=====
        AI=VTRK(12,I)
        BI=VTRK(13,I)
        RI=VTRK(10,I)
        AJ=VTRK(12,J)
        BJ=VTRK(13,J)
        RJ=VTRK(10,J)
C
C A,B,R IS X,Y OF CENTER OF CURVATURE AND RADIUS OF CURVATURE
C RADIUS OF CURVATURE IS SIGNED, USED TO REJECT EQUAL SIGN COMBINATIONS
C
       IF(RI*RJ.GT.0.) GO TO 4000
C
       CALL TWOCIR(AI,BI,RI,AJ,BJ,RJ,V1,V2,DIST,IFLAG)
       IF(IFLAG.NE.0) RETURN
        PV1X=V1(3)+V1(5)
        PV1Y=V1(4)+V1(6)
        PV2X=V2(3)+V2(5)
        PV2Y=V2(4)+V2(6)
C
       SIGV1=V1(1)*PV1X+V1(2)*PV1Y
       SIGV2=V2(1)*PV2X+V2(2)*PV2Y
C
       R1STI=SQRT(VTRK(1,I)**2+VTRK(2,I)**2)
       R1STJ=SQRT(VTRK(1,J)**2+VTRK(2,J)**2)
       R1ST=AMIN1(R1STI,R1STJ)
C
       VO1=SQRT(V1(1)**2+V1(2)**2)
       VO2=SQRT(V2(1)**2+V2(2)**2)
C
C
      IF(SIGV1.GE.0. .OR. SIGV2.GE.0) GO TO 200
      IF(VO1.GT.VO2) GO TO 110
      IF(VO1.LT.10.) GO TO 1000
      GO TO 3001
 110  IF(VO2.LT.10.) GO TO 2000
      GO TO 3002
C
 200  IF(SIGV1.LE.0. .OR. SIGV2.GE.0) GO TO 250
C--------------------------------------------------- 25/12/1980
C     IF(VO1.GT.R1STI .OR. VO1.GT.R1STJ) GO TO 1000
C     GO TO 3101
      GO TO 1000
C
 250  IF(SIGV2.LE.0. .OR. SIGV1.GE.0) GO TO 300
C--------------------------------------------------- 25/12/1980
C     IF(VO2.GT.R1STI .OR. VO2.GT.R1STJ) GO TO 2000
C     GO TO 3102
      GO TO 2000
C
 300  IF(SIGV1.LE.0. .OR. SIGV2.LE.0) GO TO 400
      IF(VO1.GT.VO2) GO TO 350
C     IF(VO2.LT.R1STI .OR. VO2.LT.R1STJ) GO TO 2000
C     THE UPPER LINE IS CHANGED TO THE LOWER LINE       25/12/1980
      IF(VO2.LT.R1STI .AND. VO2.LT.R1STJ) GO TO 2000
CCCCC IF(VO1.GT.R1STI .AND. VO1.GT.R1STJ) GO TO 400
      GO TO 1000
C350  IF(VO1.LT.R1STI .OR. VO1.LT.R1STJ) GO TO 1000
C     THE UPPER LINE IS CHANGED TO THE LOWER LINE       25/12/1980
 350  IF(VO1.LT.R1STI .AND. VO1.LT.R1STJ) GO TO 1000
CCCCC IF(VO2.GT.R1STI .AND. VO2.GT.R1STJ) GO TO 400
      GO TO 2000
C
 400  IF(VO1.EQ.0.) GO TO 1000
      IF(VO2.EQ.0.) GO TO 2000
      GO TO 3007
C=== V1-QUE
 1000 DO 1100 K=1,6
      V(K)=V1(K)
 1100 CONTINUE
      VO=VO1
      SIGV=SIGV1
      GO TO 5000
C=== V2-QUE
 2000 DO 2100 K=1,6
      V(K)=V2(K)
 2100 CONTINUE
      VO=VO2
      SIGV=SIGV2
      GO TO 5000
C=== JUNK-VERTEX QUE
 3001 JFLAG=11
      RETURN
 3002 JFLAG=12
      RETURN
 3007 JFLAG=17
      RETURN
C3101 JFLAG=101
C     RETURN
C3102 JFLAG=102
C     RETURN
 4000 JFLAG=100
      RETURN
C=== SARANARU VERTEX-IBIRI
 5000 JFLAG=0
      IF(VO.GT.600.) JFLAG=1
      DVL=SQRT((V(1)-DIST(1))**2+(V(2)-DIST(2))**2)
      DVL=DVL*SIGN(1.0,SIGV)
      D=SQRT(DIST(1)**2+DIST(2)**2)
C     IF(VO.GT.R1STI .AND. VO.GT.R1STJ) JFLAG=500
C-----------------------------------------------------------------------
C      WRITE(6,9000) IFLAG,JFLAG,R1STI,R1STJ,VO,VO1,VO2
C9000  FORMAT(' FLAG(I,J)=',2I5,'   R1ST(I,J)=',2F10.3,'    VO=',3F10.3)
C-----------------------------------------------------------------------
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE ZRVOPT(NPATR,I,J,V,DZIOPT,DZJOPT,CHI1,CHI2,LFLAG,ZPOS)
C-----------------------------------------------------------------------
C
C     S.KOMAMIYA      21/03/1980    18:10
C     MODIFIED        22/03/1980    22:30
C     MODIFIED        08/04/1980    18:30
C     SUBROUTINE TO OPTIMIZE COSTH
C     ---------- THETA IBIRI -----------
C
C    LFLAG = 0     : NORMAL END
C          < 0     : ABNORMAL END
C
C
C
C     COPIED FROM F22KOM.KS(ZRTST)     27.11.1985
C     MODIFIED TO CALCULATE Z-COORDINATE OF SECONDARY VERTEX  2.12.85
C     LAST MOD. 28.04.86 LOWER HIT NUMBER LIMIT CHANGED 20-->12
C                        AS MORE SUITABLE FOR GAMMA GAMMA EVENTS.
C
C
C             LAST MOD:  26.05.86 RESET CHI2 TO 3600.
C
C
      COMMON/BCS/ IDATA(35000)
      DIMENSION ADATA(35000)
      INTEGER *2 HDATA(70000)
      EQUIVALENCE (IDATA(1),ADATA(1)),(HDATA(1),IDATA(1))
C
#include "cgeo1.for"
C
      COMMON/VTR/VTRK(20,100)
      DIMENSION KTRK(20,100)
      EQUIVALENCE (VTRK(1,1),KTRK(1,1))
C
      DIMENSION V(6)
      DIMENSION RI(100) ,ZI(100) ,RJ(100) ,ZJ(100)
C     DIMENSION RIM(100),ZIM(100),RJM(100),ZJM(100)
      DIMENSION IZFT(100) ,JZFT(100) ,FZI(100) ,FZJ(100)
C     DIMENSION IZFTM(100),JZFTM(100),FZIM(100),FZJM(100)
      DIMENSION A(3,3),B(3),X(4)
      COMMON/MAG/BC
C     DATA RMAT/152.4/        IS NOT USED IN THIS ROUTINE
C
C------------------------------------------------
       LFLAG=-1000
       R0=SQRT(V(1)**2+V(2)**2)
C
       DZIOPT=0.
       DZJOPT=0.
C
      DO 50 LL=1,100
       ZI(LL)=0.
       RI(LL)=0.
       FZI(LL)=0.0
       IZFT(LL)=0
       ZJ(LL)=0.
       RJ(LL)=0.
       FZJ(LL)=0.0
       JZFT(LL)=0
 50   CONTINUE
C
      IHTREJ=0
      JHTREJ=0
      IERI=0
      IERJ=0
      CHMAX=0.0
      CHIMAX=0.0
      CHJMAX=0.0
C
C
C===== FILL R-Z HIT MAP OF I-TH TRACK
       CALL RFDRPZ(NPATR,I,IERI)
       IF(IERI) 1001,100,1001
 100   CALL BLOC(NRFWK,'RFWK',1,*1002)
       ILENG=IDATA(NRFWK)
       NHITI=ILENG/4
C
       NHITIB=0
       DO 11 KI0=1,NHITI
       KI4=(KI0-1)*4
       RI(KI0)=ADATA(NRFWK+KI4+1)
       IF(RI(KI0) .LT. R0)  NHITIB=NHITIB+1
       ZI(KI0)=ADATA(NRFWK+KI4+3)
       IZFT(KI0)=HDATA((NRFWK+KI4)*2+7)
 11    CONTINUE
C
C===== FILL R-Z HIT MAP OF J-TH TRACK
      CALL RFDRPZ(NPATR,J,IERJ)
        IF(IERJ) 1003,200,1003
 200  CALL BLOC(NRFWK,'RFWK',1,*1004)
        JLENG=IDATA(NRFWK)
        NHITJ=JLENG/4
C
      NHITJB=0
      DO 21 KJ0=1,NHITJ
       KJ4=(KJ0-1)*4
       RJ(KJ0)=ADATA(NRFWK+KJ4+1)
       IF(RJ(KJ0) .LT. R0)  NHITJB=NHITJB+1
       ZJ(KJ0)=ADATA(NRFWK+KJ4+3)
       JZFT(KJ0)=HDATA((NRFWK+KJ4)*2+7)
 21   CONTINUE
C
       NHITB=NHITIB+NHITJB
C      IF(NHITB.GE.1) GO TO 1010
       IF(NHITIB.GT.2  .OR. NHITJB.GT.2) LFLAG=500
C
       CHI1=0.0
       CHI10=0.0
       DZI1=0.0
       DZJ1=0.0
C
C
C
      DO 1100 NN=1,50
        SI=0.
        SRI=0.
        SZI=0.
        SSRI=0.
        SSZI=0.
        SRZI=0.
C
C
      DO 10 KI1=1,NHITI
       CHI10=CHI1
       IF(IZFT(KI1).NE.0) GO TO 10
       SI=SI+1.
       SRI=SRI+RI(KI1)
       SZI=SZI+ZI(KI1)
       SSRI=SSRI+RI(KI1)**2
       SSZI=SSZI+ZI(KI1)**2
       SRZI=SRZI+RI(KI1)*ZI(KI1)
 10   CONTINUE
C
C
        SJ=0.
        SRJ=0.
        SZJ=0.
        SSRJ=0.
        SSZJ=0.
        SRZJ=0.
C
C
      DO 20 KJ1=1,NHITJ
       IF(JZFT(KJ1).NE.0) GO TO 20
       SJ=SJ+1.
       SRJ=SRJ+RJ(KJ1)
       SZJ=SZJ+ZJ(KJ1)
       SSRJ=SSRJ+RJ(KJ1)**2
       SSZJ=SSZJ+ZJ(KJ1)**2
       SRZJ=SRZJ+RJ(KJ1)*ZJ(KJ1)
 20   CONTINUE
C                           UNHANGED!!!!!!!!!!!
C
      A(1,1)=SSRI+SJ*R0
      A(1,2)=SRJ*R0-SJ*R0**2
      A(1,3)=SJ*R0
      A(2,1)=SRJ*R0-SJ*R0**2
      A(2,2)=SSRJ-2.*SRJ*R0+SJ*R0**2
      A(2,3)=SRJ-SJ*R0
      A(3,1)=SRI+SJ*R0
      A(3,2)=SRJ-SJ*R0
      A(3,3)=SI+SJ
C
C
C
      B(1)=SRZI+SZJ*R0
      B(2)=SRZJ-SZJ*R0
      B(3)=SZI+SZJ
C
C
C
      CALL SOL(A,B,X,LFLAG1)
      IF(LFLAG1) 1005,300,1005
 300  IF(X(1).NE.0.) GO TO 1
      DZI=0.
      GO TO 2
 1    CONTINUE
      DZI=X(1)/SQRT(1.+X(1)**2)
 2    CONTINUE
      IF(X(2).NE.0.) GO TO 3
      DZJ=0.
      GO TO 4
 3    CONTINUE
      DZJ=X(2)/SQRT(1.+X(2)**2)
 4    CONTINUE
C
      X(4)=X(1)*R0-X(2)*R0+X(3)
C
C
C
      CHI1=X(1)**2*SSRI+X(3)**2*SI+SSZI
     *    +2.*(X(1)*X(3)*SRI-X(3)*SZI-X(1)*SRZI)
     *    +X(2)**2*SSRJ+X(4)**2*SJ+SSZJ
     *    +2.*(X(2)*X(4)*SRJ-X(4)*SZJ-X(2)*SRZJ)
      SIJ=SI+SJ-3.
      CHI1=CHI1/SIJ
      IF(NN.EQ.1) CHIMIN=CHI1
      IF(CHI1.GT.CHIMIN) GO TO 25
      CHIMIN=CHI1
      DZIOPT=DZI
      DZJOPT=DZJ
 25   CONTINUE
C---------------------------- 08/04/1980 -------------------------------
C
      DO 18 KI2=1,NHITI
      FZI(KI2)=0.
 18   CONTINUE
C
       CHIMAX=0.0
       KIMAX=0
      DO 30 KI=1,NHITI
       IF(IZFT(KI).NE.0) GO TO 16
       FZI(KI)=(ZI(KI)-X(1)*RI(KI)-X(3))**2
 16    CONTINUE
C-----------------------------------------------------------------------
C      WRITE(6,9100)KI,R0,RI(KI),ZI(KI),IZFT(KI),FZI(KI),KIMAX
C9100  FORMAT(' KI=',I3,'   R0=',F10.3,'   RI=',F10.5,'    ZI=',F10.3,
C    *      '     IZFT=',I5,'     FZI=',F10.3,'  KIMAX=',I3)
C-----------------------------------------------------------------------
       IF(CHIMAX.GE.FZI(KI)) GO TO 30
       KIMAX=KI
       CHIMAX=FZI(KI)
 30   CONTINUE
C
C
      DO 19 KJ2=1,NHITJ
      FZJ(KJ2)=0.
 19   CONTINUE
C
       CHJMAX=0.0
       KJMAX=0
      DO 40 KJ=1,NHITJ
       IF(JZFT(KJ).NE.0) GO TO 17
       FZJ(KJ)=(ZJ(KJ)-X(2)*RJ(KJ)-X(4))**2
 17    CONTINUE
C-----------------------------------------------------------------------
C      WRITE(6,9200)KJ,R0,RJ(KJ),ZJ(KJ),JZFT(KJ),FZJ(KJ),KJMAX
C9200  FORMAT(' KJ=',I3,'   R0=',F10.3,'   RJ=',F10.5,'    ZJ=',F10.3,
C    *      '     JZFT=',I5,'     FZJ=',F10.3,'  KJMAX=',I3)
C-----------------------------------------------------------------------
       IF(CHJMAX.GT.FZJ(KJ)) GO TO 40
       KJMAX=KJ
       CHJMAX=FZJ(KJ)
 40   CONTINUE
C
C-----------------------------------------------------------------------
C     WRITE(6,9000)I,J,SIJ,CHI1,CHI10,DZI,DZJ,DZIOPT,DZJOPT,CHMAX,CHIMIN
C9000 FORMAT(' (I,J)=',2I5,' SIJ=',F5.1,
C    *      ' CHI1(',2F10.2,') DZ=((',2F6.3,' ))',
C    *      ' DZOPT=(',2F6.3,') CHMAX,CHIMIN=',2F10.1/)
C-----------------------------------------------------------------------
      IF(CHIMAX-CHJMAX) 23,22,22
 22   IZFT(KIMAX)=100
      IHTREJ=IHTREJ+1
      GO TO 24
 23   JZFT(KJMAX)=100
      JHTREJ=JHTREJ+1
 24   CONTINUE
      IHTREM=NHITI-IHTREJ
      JHTREM=NHITJ-JHTREJ
      IF(IHTREM.LE.20 .OR. JHTREM.LE.20) GO TO 1200
      CHMAX=AMAX1(CHIMAX,CHJMAX)
      IF(CHMAX.LT.3600.0) GO TO 1200
 1100 CONTINUE
 1200 CONTINUE
C
C-----------------------------------------------------------------------
      PTI=ABS(VTRK(10,I))*BC
      PTJ=ABS(VTRK(10,J))*BC
C
      DZI0=VTRK(6,I)
      DZJ0=VTRK(6,J)
C
      PI=PTI/SQRT(1.-DZI0**2)
      PJ=PTJ/SQRT(1.-DZJ0**2)
C
      STHI=SCATT(PI,DZI0)
      STHJ=SCATT(PJ,DZJ0)
C
      THI=ARCOS(DZI)
      THJ=ARCOS(DZJ)
      THI0=ARCOS(DZI0)
      THJ0=ARCOS(DZJ0)
C
      CHI2=((THI-THI0)/STHI)**2+((THJ-THJ0)/STHJ)**2
C     LFLAG=0
      IF(LFLAG.NE.500)  LFLAG=0
C-----------------------------------------------------------------------
C     WRITE(6,9300) DZI0,DZJ0,DZI,DZJ,DZIOPT,DZJOPT,SIJ,PI,PJ,
C    *              THI0,THJ0,THI,THJ,CHI1,
C    *              CHI2,CHMAX,(X(LL),LL=1,4)
C9300 FORMAT(' ########## DZ0=(',2F7.3,')  DZ=(',2F7.3,')  ',
C    *      '  DZ1=(',2F7.3,')    SIJ=',F5.1/
C    *      '  PI,PJ=',2F7.3,' TH0=',2F8.3,' TH=',2F8.3/
C    *      '  CHI1,CHI2=',2F10.4,'    CHMAX=',F10.3,' X(4)',4F9.3/)
C-----------------------------------------------------------------------
C
C1010 LFLAG=500
C-----------------------------------------------------------------------
C     IF(LFLAG.NE.500) GO TO 1011
C     WRITE(6,9400) I,J,DZI0,DZJ0,DZI,DZJ,DZIOPT,DZJOPT,SIJ,PI,PJ,
C    *              THI0,THJ0,THI,THJ,CHI1,
C    *              CHIMIN,CHMAX,(X(LL),LL=1,4),NHITIB,NHITJB,LFLAG
C9400 FORMAT(' (I,J)=',2I5,'  DZ0=(',2F6.3,') DZ=(',2F6.3,') ',
C    *      ' DZ1=(',2F6.3,') SIJ=',F5.1/
C    *      '  PI,PJ=',2F7.3,' TH0=',2F8.3,' TH=',2F8.3/
C    *      '  CHI1,CHIMIN=',2F10.4,'    CHMAX=',F10.3,' X(4)',4F9.3,
C    *      '  NHITIB,NHITJB=',2I3,'  LFLAG=',I4/)
C1011  CONTINUE
C-----------------------------------------------------------------------
C   CALCULATE Z-COORDINATE OF THE SECONDARY VERTEX
C
      ZPOS = X(1)*R0 + X(3)
C
      RETURN
C
C=== JUNK-VRETEX QUE
 1001 LFLAG=10+IERI
      RETURN
 1002 LFLAG=20
      RETURN
 1003 LFLAG=30+IERJ
      RETURN
 1004 LFLAG=40
      RETURN
 1005 LFLAG=50
      RETURN
C  LFLAG=16 IS NOT USED
C1006 LFLAG=16
C     RETURN
C     RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE BKKOKS
C-----------------------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
C
C WRITE RESULT OF KOMAKS OUT IN A BOS BANK   "KOKS"
C
C     LAST CHANGE 27.02.1986  TAKE OUT COMMONS CGRAPH AND CHEADR
C  +++++++++++++++++++++++++++++++++++++++++++++++
C     COMMON /CHEADR/ HEAD(108)
C     EQUIVALENCE (HEAD(18),HRUN),(HEAD(19),HEVENT)
C%MACRO 'F11GOD.PATRECSR(CGRAPH)'
C  +++++++++++++++++++++++++++++++++++++++++++++++
C
#include "cdata.for"
C
      COMMON /CKOMAK/NMAS,PCOMB1(4,50),PCOMB2(4,50),HTR1(50),HTR2(50),
     $               VMAS(50),DMAS(50),DVLK0(50),DK0(50),XYZK0(3,50),
     $         STDEV,ARLIM,IZRFIT,MNHIT,PCUTT,DVLMIN,DK0MAX,IEE3FL
C
      DATA IERK /0/, IVERSK/1/
C//////////////////////////////////////////////////////////////////////
C
C.....   DELETE BANK KOKS IF ALREADY EXISTING
C
      IPHT = IDATA(IBLN('KOKS'))
      IF(IPHT.LE.0) GO TO 230
      NBNK = IDATA(IPHT-2)
      CALL BDLS('KOKS',NBNK)
      CALL BGAR(DUMK)
C
C @@@@@@@   RESULT BANK   K O K S  @@@@@@@
C
230   NWRES = 8
      IF(NMAS.LE.0) GO TO 600
      NWRES = NWRES + NMAS*16
600   CONTINUE
      IPPATR = IDATA(IBLN('PATR'))
      NBNK = IDATA(IPPATR-2)
      CALL CCRE(IPHT,'KOKS',NBNK,NWRES,IERR)
      IF(IERR.NE.0) GO TO 9998
      CALL BSAW(1,'KOKS')
      IPHT2 = 2*IPHT
      HDATA(IPHT2+1) = IVERSK
      HDATA(IPHT2+2) = NMAS
      ADATA(IPHT+2) = STDEV
      ADATA(IPHT+3) = ARLIM
      ADATA(IPHT+4) = PCUTT
      ADATA(IPHT+5) = DVLMIN
      ADATA(IPHT+6) = DK0MAX
      HDATA(IPHT2+13) = IZRFIT
      HDATA(IPHT2+14) = MNHIT
      HDATA(IPHT2+15) = IEE3FL
      IF(NMAS.LE.0) GO TO 8000
C
      ITRX = 7
      DO 9980  IMA = 1,NMAS
      ITRX = ITRX + 1
9980  ADATA(IPHT+ITRX) = VMAS(IMA)
      DO 9981  IMA = 1,NMAS
      ITRX = ITRX + 1
9981  ADATA(IPHT+ITRX) = DMAS(IMA)
      DO 9979  IMA = 1,NMAS
      ITRX = ITRX + 1
9979  ADATA(IPHT+ITRX) = DVLK0(IMA)
      DO 9978  IMA = 1,NMAS
      ITRX = ITRX + 1
9978  ADATA(IPHT+ITRX) = DK0(IMA)
C
      DO 9982  IMA = 1,NMAS
      DO 9983  I = 1,4
      ITRX = ITRX + 1
9983  ADATA(IPHT+ITRX) = PCOMB1(I,IMA)
9982  CONTINUE
C
      DO 9984  IMA = 1,NMAS
      DO 9985  I = 1,4
      ITRX = ITRX + 1
9985  ADATA(IPHT+ITRX) = PCOMB2(I,IMA)
9984  CONTINUE
C
      DO 9986  IMA = 1,NMAS
      DO 9987  I = 1,3
      ITRX = ITRX + 1
9987  ADATA(IPHT+ITRX) = XYZK0(I,IMA)
9986  CONTINUE
C
      ITRX2 = 2*ITRX
C
      DO 9988  IHT = 1,NMAS
      ITRX2 = ITRX2 + 1
9988  HDATA(IPHT2+ITRX2) = HTR1(IHT)
      DO 9989  IHT = 1,NMAS
      ITRX2 = ITRX2 + 1
9989  HDATA(IPHT2+ITRX2) = HTR2(IHT)
C
C                              END BANK
C
      GO TO 8000
C
9998  CONTINUE
      IERK = IERK + 1
      IF(IERK.LT.11) WRITE(6,7666) IERR,NBNK
7666  FORMAT(' ####   WARNING ######      ERROR ',I4,
     $  ' IN CREATING BANK KOKS WITH NUMBER ',I5)
C
8000  CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE KOKSRE(IERR)
C-----------------------------------------------------------------------
C     UNPACKS THE KOKS BANK INTO THE CKOMAKS COMMON.
C     IERR = 0 FOR NORMAL RETURN
C     IERR = 1 MEANS KOKS BANK DOES NOT EXIST
C
C             WRITTEN 26.02.86
C          LAST MOD.  15.04.86 SECOUND  ARGUMENT (IVERSK) TAKEN OUT
C
      IMPLICIT INTEGER*2 (H)
      COMMON / CKOMAK /NMAS,PCOMB1(4,50),PCOMB2(4,50),HTR1(50),HTR2(50),
     +              VMAS(50),DMAS(50),DVLK0(50),DK0(50),XYZK0(3,50),
     +              STDEV,ARLIM,IZRFIT,MNHIT,PCUTT,DVLMIN,DK0MAX,IEE3FL
      COMMON / BCS    / IDATA(40000)
C
      INTEGER*2 HDATA(80000)
      REAL ADATA(40000)
      EQUIVALENCE (IDATA(1),HDATA(1),ADATA(1))
C
      IERR = 0
C
C                             GET POINTER TO KOKS BANK
C
      IPKOKS = IDATA(IBLN('KOKS'))
      IF( IPKOKS .LE. 0 ) IERR = 1
      IF( IERR   .EQ. 1 ) RETURN
      IPKOK2 = IPKOKS * 2
C
C                               BEGIN FILLING THE COMMON
C
C     IVERSK = HDATA( IPKOK2 + 1 )
      IVERSK = 2
      NMAS   = HDATA( IPKOK2 + 2 )
      STDEV  = ADATA( IPKOKS + 2 )
      ARLIM  = ADATA( IPKOKS + 3 )
      PCUTT  = ADATA( IPKOKS + 4 )
      DVLMIN = ADATA( IPKOKS + 5 )
      DK0MAX = ADATA( IPKOKS + 6 )
      IZRFIT = HDATA( IPKOK2 + 13)
      MNHIT  = HDATA( IPKOK2 + 14)
      IEE3FL = HDATA( IPKOK2 + 15)
C
C                             IF NO CANDIDATES FOUND THEN RETURN
C
      IF ( NMAS .LE. 0) RETURN
      NSTART  = 8
      III = IPKOKS + NSTART
C
C                             FILL THE CANDIDATE INFORMATION.
C
      DO 10  IMA = 1,NMAS
         III = III + 1
         VMAS(IMA) = ADATA( III )
10    CONTINUE
C
      DO 20    IMA = 1,NMAS
        III = III + 1
        DMAS(IMA) = ADATA( III )
20    CONTINUE
C
      DO 30    IMA = 1,NMAS
         III = III + 1
         DVLK0(IMA) = ADATA( III )
30    CONTINUE
C
      DO 40    IMA = 1,NMAS
         III = III + 1
         DK0(IMA) = ADATA( III )
40    CONTINUE
C
      DO 50    IMA = 1,NMAS
         DO 51    I = 1,4
            III = III + 1
            PCOMB1(I,IMA) = ADATA( III )
51       CONTINUE
50    CONTINUE
C
      DO 60    IMA = 1,NMAS
         DO 61    I = 1,4
            III = III + 1
            PCOMB2(I,IMA) = ADATA( III )
61       CONTINUE
60    CONTINUE
C
      DO 70    IMA = 1,NMAS
         DO 71    I = 1,3
            III = III + 1
            XYZK0(I,IMA) = ADATA( III )
71       CONTINUE
70    CONTINUE
C
      III2 = 2*III
C
      DO 80    IHT = 1,NMAS
         III2 = III2 + 1
         HTR1(IHT) = HDATA( III2 )
80    CONTINUE
C
      DO 90    IHT = 1,NMAS
         III2 = III2 + 1
         HTR2(IHT) = HDATA( III2 )
90    CONTINUE
C
      RETURN
      END
