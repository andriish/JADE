      PROGRAM K0S
C-----------------------------------------------------------------------
C Example analysis of reconstructed JADE events in Fortran: search for
C K0S -> pi+ pi-.
C
C The program reads the BOS file written by superv (real data or
C simulation), takes the tracks of the PATR bank (the track parameters of
C the inner detector) and combines each pair of tracks with opposite
C charges whose helices are close to each other:
C   - the projections of the helices on the (x,y) plane are circles; the
C     distance between the two circles (zero if they intersect) must be
C     smaller than DXYMAX;
C   - the point of the closest approach (an intersection point of the
C     circles, or the point on the line of the centres between the
C     circles) is the candidate decay vertex; it must be displaced from the
C     beam by more than RVMIN (the K0S flies a few cm) and the tracks must
C     have the z coordinates that differ at the vertex by less than DZMAX;
C   - the momenta of the tracks at the vertex are calculated (as in the
C     routine CRDOCA, the field is taken from the HEAD bank) and the
C     invariant mass of the pair is calculated with the pion mass.
C The mass is filled into a histogram (printed, and written to the file
C given as the second argument: bin centre, contents, error).
C
C usage: k0s <superv BOS file> <output text file> [<file for the distances>]
C The byte order of the BOS file is set for the Fortran runtime as for the
C other programs (e.g. GFORTRAN_CONVERT_UNIT='big_endian;native:2').
C Exit code 0 if the file was read and at least one pair of tracks was
C combined.
C-----------------------------------------------------------------------
      IMPLICIT NONE
C BOS memory
      INTEGER MBOS
      PARAMETER (MBOS=80000)
      INTEGER IW(MBOS)
      INTEGER*2 HW(MBOS*2)
      REAL RW(MBOS)
      COMMON /BCS/ IW
      EQUIVALENCE (HW(1),IW(1),RW(1))
      INTEGER IBLN
C Cuts
      REAL DXYMAX, RVMIN, RVMAX, DZMAX, PMIN, MPI
      INTEGER NHMIN
      REAL D0MIN
      PARAMETER (DXYMAX=3.0, RVMIN=10.0, RVMAX=300.0, DZMAX=60.0)
      PARAMETER (D0MIN=5.0)
      PARAMETER (PMIN=0.1, MPI=0.13957, NHMIN=10)
C Histogram
      INTEGER NB
      REAL XLO, XHI
      PARAMETER (NB=40, XLO=0.3, XHI=0.7)
      REAL HIST(NB), BW
      INTEGER NC
      PARAMETER (NC=40)
      REAL HGAP(NC), HRV(NC), HDZ(NC)
      CHARACTER*256 DNAME
      INTEGER IC
C Tracks
      INTEGER MAXT
      PARAMETER (MAXT=200)
      REAL CAPT(MAXT), RMT(MAXT), PHIT(MAXT), SIGT(MAXT), DZT(MAXT)
     +    ,Z0T(MAXT), CX(MAXT), CY(MAXT), RAD(MAXT), CHG(MAXT)
      INTEGER NT, I, J, K, NPAIR, NEVT, NREC, NPHEAD, NPATR, NTR, NWORD
     +       ,IPSTRT, IP, IOS, IB, NH, NTRIAL
      REAL BKGAUS, EBEAM, CAPV, RMV, PHIV, SIGV
      REAL DX, DY, D, GAP, A, H, PX, PY, XV(2), YV(2), XVM, YVM
      REAL RV, ZI, ZJ, DZ, P1(3), P2(3), E1, E2, PS(3), XM, COSPT, PTT
      REAL CPMIN
      PARAMETER (CPMIN=0.98)
      REAL BEST, DZB, XB, YB
      INTEGER NV, IV, IBEST
      CHARACTER*256 FNAME, ONAME
      INTEGER IHWSW

      CALL GET_COMMAND_ARGUMENT(1,FNAME)
      CALL GET_COMMAND_ARGUMENT(2,ONAME)
      CALL GET_COMMAND_ARGUMENT(3,DNAME)
      IF (FNAME.EQ.' ' .OR. ONAME.EQ.' ') THEN
        WRITE(*,*) 'usage: k0s <superv BOS file> <output text file>',
     +    ' [<file for the distances>]'
        STOP 2
      ENDIF
      BW=(XHI-XLO)/NB
      DO 1 I=1,NB
        HIST(I)=0.
    1 CONTINUE
      DO 2 I=1,NC
        HGAP(I)=0.
        HRV(I)=0.
        HDZ(I)=0.
    2 CONTINUE
      CALL BINT(MBOS,MBOS/2,MBOS/8,0)
      OPEN(2,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'k0s: cannot open ',FNAME(1:LEN_TRIM(FNAME))
        STOP 2
      ENDIF
      NEVT=0
      NPAIR=0
      NTRIAL=0
C Event loop
   10 CONTINUE
      CALL BREAD(2,*900,*900)
      NPHEAD=IW(IBLN('HEAD'))
      IF (NPHEAD.LE.0) GO TO 10
      NEVT=NEVT+1
C Beam energy (half-word 29 of HEAD, MeV) and field (half-word 30, kG);
C the order of the half-words is that of the machine that wrote the bank
C (see jtuple/src/jtevnt.F): use the neighbouring half-word if the first
C one is not a beam energy
      IHWSW=0
      IF (.NOT.(HW(NPHEAD*2+29).GE.500 .AND. HW(NPHEAD*2+29).LE.30000))
     +  IHWSW=1
      IF (IHWSW.EQ.0) THEN
        EBEAM=HW(NPHEAD*2+29)/1000.
        BKGAUS=HW(NPHEAD*2+30)/1000.
      ELSE
        EBEAM=HW(NPHEAD*2+30)/1000.
        BKGAUS=HW(NPHEAD*2+29)/1000.
      ENDIF
      IF (BKGAUS.EQ.0.) BKGAUS=-4.5
      NPATR=IW(IBLN('PATR'))
      IF (NPATR.LE.0) GO TO 10
      NTR=IW(NPATR+2)
      IF (NTR.LE.1) GO TO 10
      NWORD=IW(NPATR+3)
      IPSTRT=IW(NPATR+1)+NPATR
C Tracks: circle in (x,y), dz/ds, z0, charge
      NT=0
      DO 20 K=1,NTR
        IP=IPSTRT+(K-1)*NWORD
        NH=IW(IP+24)
        IF (NH.LT.NHMIN) GO TO 20
        IF (NT.GE.MAXT) GO TO 20
        CALL PRTOCI(IP,CAPV,RMV,PHIV,SIGV)
        IF (CAPV.LT.1.E-8) GO TO 20
        NT=NT+1
        CAPT(NT)=CAPV
        RMT(NT)=RMV
        PHIT(NT)=PHIV
        SIGT(NT)=SIGV
        DZT(NT)=RW(IP+30)
        Z0T(NT)=RW(IP+31)
        RAD(NT)=1./CAPV
        CX(NT)=(RMV+RAD(NT))*COS(PHIV)
        CY(NT)=(RMV+RAD(NT))*SIN(PHIV)
        CHG(NT)=SIGV
        IF (BKGAUS.LT.0.) CHG(NT)=-CHG(NT)
   20 CONTINUE
C Pairs of tracks with opposite charges
      DO 40 I=1,NT-1
      DO 30 J=I+1,NT
        IF (CHG(I)*CHG(J).GE.0.) GO TO 30
        IF (ABS(RMT(I)).LT.D0MIN .OR. ABS(RMT(J)).LT.D0MIN) GO TO 30
        DX=CX(J)-CX(I)
        DY=CY(J)-CY(I)
        D=SQRT(DX*DX+DY*DY)
        IF (D.LT.1.E-6) GO TO 30
C Distance between the circles in (x,y)
        IF (D.GT.RAD(I)+RAD(J)) THEN
          GAP=D-RAD(I)-RAD(J)
        ELSEIF (D.LT.ABS(RAD(I)-RAD(J))) THEN
          GAP=ABS(RAD(I)-RAD(J))-D
        ELSE
          GAP=0.
        ENDIF
C Distance between the helices (the circles): histogram before the cut, 0 - 20 mm
        IC=INT(GAP/0.5)+1
        IF (IC.GE.1 .AND. IC.LE.NC) HGAP(IC)=HGAP(IC)+1.
        IF (GAP.GT.DXYMAX) GO TO 30
        NTRIAL=NTRIAL+1
C Candidate vertices: the intersection points (or the point between
C the circles on the line of the centres)
        NV=0
        IF (GAP.GT.0.) THEN
          NV=1
          IF (D.GT.RAD(I)+RAD(J)) THEN
            A=RAD(I)+0.5*GAP
          ELSE
            IF (RAD(I).GE.RAD(J)) THEN
              A=RAD(I)-0.5*GAP
            ELSE
              A=RAD(I)+0.5*GAP
            ENDIF
          ENDIF
          XV(1)=CX(I)+A*DX/D
          YV(1)=CY(I)+A*DY/D
        ELSE
          A=(D*D+RAD(I)**2-RAD(J)**2)/(2.*D)
          H=RAD(I)**2-A*A
          IF (H.LT.0.) H=0.
          H=SQRT(H)
          XVM=CX(I)+A*DX/D
          YVM=CY(I)+A*DY/D
          NV=2
          XV(1)=XVM+H*DY/D
          YV(1)=YVM-H*DX/D
          XV(2)=XVM-H*DY/D
          YV(2)=YVM+H*DX/D
        ENDIF
C Choose the candidate with the smallest difference of z
        IBEST=0
        BEST=1.E10
        DO 25 IV=1,NV
          RV=SQRT(XV(IV)**2+YV(IV)**2)
          ZI=Z0T(I)+DZT(I)*RV
          ZJ=Z0T(J)+DZT(J)*RV
          DZ=ABS(ZI-ZJ)
          IF (DZ.LT.BEST) THEN
            BEST=DZ
            IBEST=IV
          ENDIF
   25   CONTINUE
        XB=XV(IBEST)
        YB=YV(IBEST)
        RV=SQRT(XB*XB+YB*YB)
C Distance of the vertex from the beam (0 - 400 mm) and difference of z (0 - 200 mm)
        IC=INT(RV/10.)+1
        IF (IC.GE.1 .AND. IC.LE.NC) HRV(IC)=HRV(IC)+1.
        IF (RV.LT.RVMIN .OR. RV.GT.RVMAX) GO TO 30
        IC=INT(BEST/5.)+1
        IF (IC.GE.1 .AND. IC.LE.NC) HDZ(IC)=HDZ(IC)+1.
        IF (BEST.GT.DZMAX) GO TO 30
C Momenta of the two tracks at the vertex (as in CRDOCA)
        CALL TRMOM(CX(I),CY(I),RAD(I),SIGT(I),DZT(I),BKGAUS,XB,YB,P1)
        CALL TRMOM(CX(J),CY(J),RAD(J),SIGT(J),DZT(J),BKGAUS,XB,YB,P2)
        IF (SQRT(P1(1)**2+P1(2)**2+P1(3)**2).LT.PMIN) GO TO 30
        IF (SQRT(P2(1)**2+P2(2)**2+P2(3)**2).LT.PMIN) GO TO 30
        E1=SQRT(P1(1)**2+P1(2)**2+P1(3)**2+MPI**2)
        E2=SQRT(P2(1)**2+P2(2)**2+P2(3)**2+MPI**2)
        PS(1)=P1(1)+P2(1)
        PS(2)=P1(2)+P2(2)
        PS(3)=P1(3)+P2(3)
C The momentum of the pair must point away from the beam (to the vertex)
        PTT=SQRT(PS(1)**2+PS(2)**2)
        IF (PTT.LT.1.E-6) GO TO 30
        COSPT=(PS(1)*XB+PS(2)*YB)/(PTT*RV)
        IF (COSPT.LT.CPMIN) GO TO 30
        XM=(E1+E2)**2-PS(1)**2-PS(2)**2-PS(3)**2
        IF (XM.LT.0.) GO TO 30
        XM=SQRT(XM)
        NPAIR=NPAIR+1
        IF (XM.GE.XLO .AND. XM.LT.XHI) THEN
          IB=INT((XM-XLO)/BW)+1
          IF (IB.GE.1 .AND. IB.LE.NB) HIST(IB)=HIST(IB)+1.
        ENDIF
   30 CONTINUE
   40 CONTINUE
      GO TO 10
C End of file
  900 CONTINUE
      CLOSE(2)
      CALL PRINTH(HIST,NB,XLO,BW,NEVT,NTRIAL,NPAIR,ONAME)
      IF (DNAME.NE.' ') THEN
C Distributions of the distances (variables of the cuts): G the distance of
C the helices in (x,y), R the distance of the vertex from the beam and
C Z the difference of z, bin centre in mm and contents
        OPEN(12,FILE=DNAME,STATUS='REPLACE')
        WRITE(12,'(A,3F8.1)') '# cuts (mm): G R-min R-max Z',DXYMAX,
     +    RVMIN,RVMAX
        WRITE(12,'(A,F8.1)') '# cut Z (mm):',DZMAX
        DO 3 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'G',(I-0.5)*0.5,HGAP(I)
    3   CONTINUE
        DO 4 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'R',(I-0.5)*10.,HRV(I)
    4   CONTINUE
        DO 5 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'Z',(I-0.5)*5.,HDZ(I)
    5   CONTINUE
        CLOSE(12)
      ENDIF
      IF (NEVT.EQ.0) THEN
        WRITE(*,*) 'FAIL: no events read'
        STOP 1
      ENDIF
      IF (NPAIR.EQ.0) THEN
        WRITE(*,*) 'FAIL: no pair of tracks combined'
        STOP 1
      ENDIF
      WRITE(*,*) 'PASS'
      END

      SUBROUTINE TRMOM(CX,CY,RHO,SIG,DZDS,BKGAUS,X,Y,P)
C Momentum of a track (circle with the centre (CX,CY), the radius RHO,
C the sense SIG and dz/ds DZDS) at the point (X,Y) of the circle
      IMPLICIT NONE
      REAL CX,CY,RHO,SIG,DZDS,BKGAUS,X,Y,P(3)
      REAL RX,RY,RT,PT
      RX=X-CX
      RY=Y-CY
      RT=SQRT(RX*RX+RY*RY)
      IF (RT.LT.1.E-6) RT=1.E-6
C Transverse momentum: .3E-4 * |B(kG)| * radius(mm)
      PT=.3E-4*ABS(BKGAUS)*RHO
      P(1)=PT*RY/RT*SIG
      P(2)=-PT*RX/RT*SIG
      P(3)=PT*DZDS
      RETURN
      END

      SUBROUTINE PRINTH(HIST,NB,XLO,BW,NEVT,NTRIAL,NPAIR,ONAME)
C Print the histogram and write it to a file
      IMPLICIT NONE
      INTEGER NB,NEVT,NTRIAL,NPAIR
      REAL HIST(NB),XLO,BW
      CHARACTER*(*) ONAME
      INTEGER I,L,N
      REAL HM,SW,SB,XC
      CHARACTER*60 BAR
      WRITE(*,'(A,I8,A,I8,A,I8)') ' events:',NEVT,
     +  '  close pairs:',NTRIAL,'  pairs with a vertex:',NPAIR
      HM=0.
      DO 1 I=1,NB
        IF (HIST(I).GT.HM) HM=HIST(I)
    1 CONTINUE
C Pairs in the window of the K0S mass (0.47 - 0.52 GeV) and in the two sidebands (together twice as wide)
      SW=0.
      SB=0.
      DO 4 I=1,NB
        XC=XLO+(I-0.5)*BW
        IF (XC.GT.0.47 .AND. XC.LT.0.52) SW=SW+HIST(I)
        IF ((XC.GT.0.40 .AND. XC.LT.0.45) .OR.
     +      (XC.GT.0.55 .AND. XC.LT.0.60)) SB=SB+HIST(I)
    4 CONTINUE
      WRITE(*,'(A,F8.0,A,F8.0)') ' pairs with 0.47 < m < 0.52 GeV:',SW,
     +  '   in the sidebands (0.40-0.45 and 0.55-0.60 GeV):',SB
      WRITE(*,*) 'Invariant mass of the pi+ pi- pairs, GeV'
      OPEN(11,FILE=ONAME,STATUS='REPLACE')
      DO 2 I=1,NB
        BAR=' '
        N=0
        IF (HM.GT.0.) N=NINT(50.*HIST(I)/HM)
        DO 3 L=1,N
          BAR(L:L)='#'
    3   CONTINUE
        WRITE(*,'(F7.4,F8.0,2X,A)') XLO+(I-0.5)*BW,HIST(I),BAR(1:50)
        WRITE(11,'(F8.4,2F10.2)') XLO+(I-0.5)*BW,HIST(I),
     +    SQRT(MAX(HIST(I),1.))
    2 CONTINUE
      CLOSE(11)
      RETURN
      END
