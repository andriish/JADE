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
C usage: k0s <superv BOS file> <output text file> [<file for the distances>
C            [<card with the cuts>]]
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
      REAL DXYMAX, RVMIN, RVMAX, DZMAX, PMIN, MPI, D0MIN, CPMIN, PKMIN
      REAL ZPMAX, FITV, SIGXY, SIGZ, CHIMAX
      INTEGER NHMIN, NTMIN
      PARAMETER (MPI=0.13957)
C The cuts; the values are those of the card (see RDCARD), these are
C the defaults if there is no card or a key is missing
      COMMON /CUTS/ DXYMAX, RVMIN, RVMAX, DZMAX, PMIN, D0MIN, CPMIN,
     +              PKMIN, ZPMAX, FITV, SIGXY, SIGZ, CHIMAX,
     +              NHMIN, NTMIN
C Histogram
      INTEGER NB
      REAL XLO, XHI
      PARAMETER (NB=40, XLO=0.3, XHI=0.7)
      REAL HIST(NB), BW
      INTEGER NC
      PARAMETER (NC=40)
      REAL HGAP(NC), HRV(NC), HDZ(NC), HPV(NC), HCHI(NC), HNT(NC)
      REAL CHI2
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
      INTEGER NTRTOT, NTSEL, NEV2, NEVPT, NEV5, NZHIT
      COMMON /STAT/ NTRTOT, NTSEL, NEV2, NEVPT, NEV5, NZHIT
      INTEGER NFLOW(11)
      COMMON /FLOW/ NFLOW
      REAL DX, DY, D, GAP, A, H, PX, PY, XV(2), YV(2), XVM, YVM
      REAL RV, ZI, ZJ, DZ, P1(3), P2(3), E1, E2, PS(3), XM, COSPT, PTT
      REAL BEST, DZB, XB, YB, ZP, ZVB, RV3, DV(3), PSM, DVM
      INTEGER NPZV, NPVF, NPVOK, NEVPV, NEVNT
      INTEGER NV, IV, IBEST
      CHARACTER*256 FNAME, ONAME, CNAME
      INTEGER IHWSW

      CALL GET_COMMAND_ARGUMENT(1,FNAME)
      CALL GET_COMMAND_ARGUMENT(2,ONAME)
      CALL GET_COMMAND_ARGUMENT(3,DNAME)
      CALL GET_COMMAND_ARGUMENT(4,CNAME)
      CALL RDCARD(CNAME)
      IF (FNAME.EQ.' ' .OR. ONAME.EQ.' ') THEN
        WRITE(*,*) 'usage: k0s <superv BOS file> <output text file>',
     +    ' [<file for the distances> [<card>]]'
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
        HPV(I)=0.
        HCHI(I)=0.
        HNT(I)=0.
    2 CONTINUE
      CALL BINT(MBOS,MBOS/2,MBOS/8,0)
      OPEN(2,FILE=FNAME,STATUS='OLD',FORM='UNFORMATTED',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'k0s: cannot open ',FNAME(1:LEN_TRIM(FNAME))
        STOP 2
      ENDIF
      NEVT=0
      NPAIR=0
      NTRTOT=0
      NTSEL=0
      NEV2=0
      NEVPT=0
      NEV5=0
      NZHIT=0
      NPVF=0
      NEVPV=0
      NEVNT=0
      DO 6 I=1,11
        NFLOW(I)=0
    6 CONTINUE
      NTRIAL=0
C Event loop
   10 CONTINUE
C Drop the banks of the previous event, else /BCS/ fills up and the
C reading stops early
      CALL BDLG
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
C Primary vertex: z from the bank ZVTX (word 1, valid if the flag in word
C 6 is positive), x and y are the position of the beam (0, 0)
      ZP=0.
      NPZV=IW(IBLN('ZVTX'))
      IF (NPZV.GT.0) THEN
        IF (IW(NPZV+6).GT.0) THEN
          ZP=RW(NPZV+1)
          NPVF=NPVF+1
C z of the primary vertex: histogram -800 - 800 mm
          IC=INT((ZP+800.)/40.)+1
          IF (IC.GE.1 .AND. IC.LE.NC) HPV(IC)=HPV(IC)+1.
        ENDIF
      ENDIF
C Cut on the z of the primary vertex: an event without a vertex is rejected
      NPVOK=0
      IF (NPZV.GT.0) THEN
        IF (IW(NPZV+6).GT.0 .AND. ABS(ZP).LT.ZPMAX) NPVOK=1
      ENDIF
      IF (NPVOK.EQ.0) GO TO 10
      NEVPV=NEVPV+1
      NPATR=IW(IBLN('PATR'))
      IF (NPATR.LE.0) GO TO 10
      NTR=IW(NPATR+2)
      NEVPT=NEVPT+1
      NTRTOT=NTRTOT+MAX(NTR,0)
      IF (NTR.LE.1) THEN
C events with fewer than two tracks in the bank: in the histogram of the
C number of tracks (0 or 1 track), no pairs
        IC=MAX(NTR,0)+1
        IF (IC.GE.1 .AND. IC.LE.NC) HNT(IC)=HNT(IC)+1.
        GO TO 10
      ENDIF
      NWORD=IW(NPATR+3)
      IPSTRT=IW(NPATR+1)+NPATR
C Tracks: circle in (x,y), dz/ds, z0, charge
      NT=0
      DO 20 K=1,NTR
        IP=IPSTRT+(K-1)*NWORD
        NH=IW(IP+24)
        IF (NH.LT.NHMIN) GO TO 20
        IF (NT.GE.MAXT) GO TO 20
        IF (IW(IP+33).GE.12) NZHIT=NZHIT+1
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
      NTSEL=NTSEL+NT
C Number of selected tracks of the event: histogram 0 - 40
      IC=NT+1
      IF (IC.GE.1 .AND. IC.LE.NC) HNT(IC)=HNT(IC)+1.
C Cut on the number of selected tracks of the event
      IF (NT.LT.NTMIN) GO TO 10
      NEVNT=NEVNT+1
      IF (NT.GE.2) NEV2=NEV2+1
      IF (NT.GE.5) NEV5=NEV5+1
C Pairs of tracks with opposite charges
      DO 40 I=1,NT-1
      DO 30 J=I+1,NT
        IF (CHG(I)*CHG(J).GE.0.) GO TO 30
        NFLOW(1)=NFLOW(1)+1
        IF (ABS(RMT(I)).LT.D0MIN .OR. ABS(RMT(J)).LT.D0MIN) GO TO 30
        DX=CX(J)-CX(I)
        DY=CY(J)-CY(I)
        D=SQRT(DX*DX+DY*DY)
        IF (D.LT.1.E-6) GO TO 30
        NFLOW(2)=NFLOW(2)+1
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
        NFLOW(3)=NFLOW(3)+1
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
C A candidate vertex must be on the part of the helices that the tracks
C pass when they go away from the beam: the momentum of each track at the
C vertex has a positive component along the direction from the beam to the
C vertex (the other intersection of the circles is usually on the side that
C a track coming from the beam does not reach).
C Choose the candidate with the smallest difference of z
        IBEST=0
        BEST=1.E10
        DO 25 IV=1,NV
          RV=SQRT(XV(IV)**2+YV(IV)**2)
          CALL TRMOM(CX(I),CY(I),RAD(I),SIGT(I),DZT(I),BKGAUS,
     +      XV(IV),YV(IV),P1)
          CALL TRMOM(CX(J),CY(J),RAD(J),SIGT(J),DZT(J),BKGAUS,
     +      XV(IV),YV(IV),P2)
          IF (P1(1)*XV(IV)+P1(2)*YV(IV).LE.0.) GO TO 25
          IF (P2(1)*XV(IV)+P2(2)*YV(IV).LE.0.) GO TO 25
          ZI=Z0T(I)+DZT(I)*RV
          ZJ=Z0T(J)+DZT(J)*RV
          DZ=ABS(ZI-ZJ)
          IF (DZ.LT.BEST) THEN
            BEST=DZ
            IBEST=IV
            ZVB=0.5*(ZI+ZJ)
          ENDIF
   25   CONTINUE
        IF (IBEST.EQ.0) GO TO 30
        NFLOW(4)=NFLOW(4)+1
        XB=XV(IBEST)
        YB=YV(IBEST)
C Fit of the vertex: the two tracks together (least squares, see VFIT)
        IF (FITV.GT.0.5) THEN
          CALL VFIT(CX(I),CY(I),RAD(I),Z0T(I),DZT(I),CX(J),CY(J),
     +      RAD(J),Z0T(J),DZT(J),SIGXY,SIGZ,XB,YB,ZVB,CHI2)
          IC=INT(CHI2/0.5)+1
          IF (IC.GE.1 .AND. IC.LE.NC) HCHI(IC)=HCHI(IC)+1.
          IF (CHI2.GT.CHIMAX) GO TO 30
        ENDIF
        NFLOW(5)=NFLOW(5)+1
        RV=SQRT(XB*XB+YB*YB)
C The vertex relative to the primary vertex: the distance in 3 dimensions
C (the beam is at x=y=0, the primary vertex has the z of the bank ZVTX)
        DV(1)=XB
        DV(2)=YB
        DV(3)=ZVB-ZP
        DVM=SQRT(DV(1)**2+DV(2)**2+DV(3)**2)
        RV=DVM
C Distance of the vertex from the primary vertex (0 - 400 mm) and difference of z (0 - 200 mm)
        IC=INT(RV/10.)+1
        IF (IC.GE.1 .AND. IC.LE.NC) HRV(IC)=HRV(IC)+1.
        IF (RV.LT.RVMIN .OR. RV.GT.RVMAX) GO TO 30
        NFLOW(6)=NFLOW(6)+1
        IC=INT(BEST/5.)+1
        IF (IC.GE.1 .AND. IC.LE.NC) HDZ(IC)=HDZ(IC)+1.
        IF (BEST.GT.DZMAX) GO TO 30
        NFLOW(7)=NFLOW(7)+1
C Momenta of the two tracks at the vertex (as in CRDOCA)
        CALL TRMOM(CX(I),CY(I),RAD(I),SIGT(I),DZT(I),BKGAUS,XB,YB,P1)
        CALL TRMOM(CX(J),CY(J),RAD(J),SIGT(J),DZT(J),BKGAUS,XB,YB,P2)
        IF (SQRT(P1(1)**2+P1(2)**2+P1(3)**2).LT.PMIN) GO TO 30
        IF (SQRT(P2(1)**2+P2(2)**2+P2(3)**2).LT.PMIN) GO TO 30
        NFLOW(8)=NFLOW(8)+1
        E1=SQRT(P1(1)**2+P1(2)**2+P1(3)**2+MPI**2)
        E2=SQRT(P2(1)**2+P2(2)**2+P2(3)**2+MPI**2)
        PS(1)=P1(1)+P2(1)
        PS(2)=P1(2)+P2(2)
        PS(3)=P1(3)+P2(3)
C The momentum of the pair must point away from the primary vertex (to the
C vertex) in 3 dimensions
        PTT=SQRT(PS(1)**2+PS(2)**2)
        IF (PTT.LT.1.E-6) GO TO 30
        PSM=SQRT(PS(1)**2+PS(2)**2+PS(3)**2)
        COSPT=(PS(1)*DV(1)+PS(2)*DV(2)+PS(3)*DV(3))/(PSM*DVM)
        IF (COSPT.LT.CPMIN) GO TO 30
        NFLOW(9)=NFLOW(9)+1
        IF (SQRT(PS(1)**2+PS(2)**2+PS(3)**2).LT.PKMIN) GO TO 30
        NFLOW(10)=NFLOW(10)+1
        XM=(E1+E2)**2-PS(1)**2-PS(2)**2-PS(3)**2
        IF (XM.LT.0.) GO TO 30
        XM=SQRT(XM)
        NPAIR=NPAIR+1
        IF (XM.GE.XLO .AND. XM.LT.XHI) NFLOW(11)=NFLOW(11)+1
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
      WRITE(*,'(A,I8)') ' events with a primary vertex (ZVTX):',NPVF
      WRITE(*,'(A,I8)') ' events with |z| of the vertex < ZPMAX:',NEVPV
      WRITE(*,'(A,I8)') ' events with at least NTMIN selected tracks:',
     +  NEVNT
      CALL PRINTH(HIST,NB,XLO,BW,NEVT,NTRIAL,NPAIR,ONAME)
      IF (DNAME.NE.' ') THEN
C Distributions of the distances (variables of the cuts): G the distance of
C the helices in (x,y), R the distance of the vertex from the beam and
C Z the difference of z, T the number of selected tracks of the event,
C C the chi2 of the fit of the vertex, P the z of the primary vertex (bank ZVTX, the
C cut ZPMAX is written in the first line), bin
C centre in mm and contents
        OPEN(12,FILE=DNAME,STATUS='REPLACE')
        WRITE(12,'(A,3ES12.4)') '# cuts (mm): G R-min R-max Z',DXYMAX,
     +    RVMIN,RVMAX
        WRITE(12,'(A,ES12.4)') '# cut Z (mm):',DZMAX
        WRITE(12,'(A,ES12.4)') '# cut ZP (mm):',ZPMAX
        WRITE(12,'(A,I6)') '# cut NT (tracks):',NTMIN
        DO 3 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'G',(I-0.5)*0.5,HGAP(I)
    3   CONTINUE
        DO 4 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'R',(I-0.5)*10.,HRV(I)
    4   CONTINUE
        DO 5 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'Z',(I-0.5)*5.,HDZ(I)
    5   CONTINUE
        DO 7 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'P',-800.+(I-0.5)*40.,HPV(I)
    7   CONTINUE
        WRITE(12,'(A,ES12.4)') '# cut CHI (chi2):',CHIMAX
        DO 8 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'C',(I-0.5)*0.5,HCHI(I)
    8   CONTINUE
        DO 9 I=1,NC
          WRITE(12,'(A,F8.2,F10.0)') 'T',I-0.5,HNT(I)
    9   CONTINUE
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
      INTEGER NTRTOT, NTSEL, NEV2, NEVPT, NEV5, NZHIT
      COMMON /STAT/ NTRTOT, NTSEL, NEV2, NEVPT, NEV5, NZHIT
      INTEGER NFLOW(11)
      COMMON /FLOW/ NFLOW
      REAL HIST(NB),XLO,BW
      CHARACTER*(*) ONAME
      INTEGER I,L,N
      REAL HM,SW,SB,XC
      CHARACTER*60 BAR
      WRITE(*,*) 'Number of pairs of tracks after each cut:'
      WRITE(*,'(A,I8)') '  opposite charges, D0MIN of the tracks:',
     +  NFLOW(1)
      WRITE(*,'(A,I8)') '  circles with different centres        :',
     +  NFLOW(2)
      WRITE(*,'(A,I8)') '  distance of the helices, DXYMAX      :',
     +  NFLOW(3)
      WRITE(*,'(A,I8)') '  tracks go away from the beam at vertex:',
     +  NFLOW(4)
      WRITE(*,'(A,I8)') '  vertex fit, chi2 < CHIMAX            :',
     +  NFLOW(5)
      WRITE(*,'(A,I8)') '  vertex distance, RVMIN - RVMAX       :',
     +  NFLOW(6)
      WRITE(*,'(A,I8)') '  difference of z, DZMAX               :',
     +  NFLOW(7)
      WRITE(*,'(A,I8)') '  momentum of the tracks, PMIN         :',
     +  NFLOW(8)
      WRITE(*,'(A,I8)') '  pointing to the vertex, CPMIN        :',
     +  NFLOW(9)
      WRITE(*,'(A,I8)') '  momentum of the pair, PKMIN          :',
     +  NFLOW(10)
      WRITE(*,'(A,I8)') '  mass in the histogram, 0.3 - 0.7 GeV :',
     +  NFLOW(11)
      WRITE(*,'(A,I8,A,I8,A,I8,A,I8)') ' events with a PATR bank:',
     +  NEVPT,'  tracks in PATR:',NTRTOT,'  selected tracks:',NTSEL,
     +  '  events with 2 or more selected tracks:',NEV2
      WRITE(*,'(A,I8,A,I8)') ' events with 5 or more selected tracks:',
     +  NEV5,'  selected tracks with 12 or more hits in z:',NZHIT
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

      SUBROUTINE RDCARD(CNAME)
C Read the cuts from a card file: the lines "KEY value", the lines
C that start with "C " or * are comments. The keys:
C   NHMIN  minimum number of hits in the r-phi plane of a track
C   NTMIN  minimum number of selected tracks of an event
C   D0MIN  minimum distance of the track to the beam (mm)
C   PMIN   minimum momentum at the vertex (GeV)
C   DXYMAX maximum distance of the helices in (x,y) (mm)
C   RVMIN, RVMAX  range of the distance of the vertex from the beam (mm)
C   DZMAX  maximum difference of z of the tracks at the vertex (mm)
C   CPMIN  minimum cosine of the angle between the momentum of the pair
C          and the vertex in (x,y)
C   PKMIN  minimum momentum of the pair (the K0S candidate) (GeV)
C   FITVTX 1: refit the two tracks together to a vertex (see VFIT)
C   SIGXY, SIGZ  assumed errors of the circle position in (x,y) and of z
C          (mm) in the fit of the vertex
C   CHIMAX maximum chi2 of the fit of the vertex
C   ZPMAX  maximum |z| of the primary vertex (mm); an event without a
C          primary vertex (bank ZVTX) is rejected
      IMPLICIT NONE
      CHARACTER*(*) CNAME
      REAL DXYMAX, RVMIN, RVMAX, DZMAX, PMIN, D0MIN, CPMIN, PKMIN
      REAL ZPMAX, FITV, SIGXY, SIGZ, CHIMAX
      INTEGER NHMIN, NTMIN
      COMMON /CUTS/ DXYMAX, RVMIN, RVMAX, DZMAX, PMIN, D0MIN, CPMIN,
     +              PKMIN, ZPMAX, FITV, SIGXY, SIGZ, CHIMAX,
     +              NHMIN, NTMIN
      CHARACTER*200 LINE, KEY
      INTEGER IOS, L
      REAL V
C Defaults
      DXYMAX=3.0
      RVMIN=10.0
      RVMAX=300.0
      DZMAX=60.0
      PMIN=0.1
      D0MIN=5.0
      CPMIN=0.98
      PKMIN=0.
      ZPMAX=1.E6
      FITV=0.
      SIGXY=2.0
      SIGZ=20.0
      CHIMAX=1.E6
      NHMIN=10
      NTMIN=2
      IF (CNAME.EQ.' ') RETURN
      OPEN(13,FILE=CNAME,STATUS='OLD',IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'k0s: cannot open the card ',CNAME(1:LEN_TRIM(CNAME))
        STOP 2
      ENDIF
   10 READ(13,'(A)',END=90) LINE
C A comment: a line that starts with 'C ' (or only 'C') or with '*'; a key
C can start with C (CPMIN, CHIMAX)
      IF (LINE.EQ.' ' .OR. LINE(1:2).EQ.'C ' .OR. LINE(1:1).EQ.'*')
     +  GO TO 10
      L=INDEX(LINE,' ')
      IF (L.LE.1) GO TO 10
      KEY=LINE(1:L-1)
      IF (KEY.EQ.'STOP') GO TO 90
      READ(LINE(L:),*,IOSTAT=IOS) V
      IF (IOS.NE.0) THEN
        WRITE(*,*) 'k0s: bad value in the card line ',LINE
        STOP 2
      ENDIF
      IF (KEY.EQ.'NHMIN') THEN
        NHMIN=NINT(V)
      ELSEIF (KEY.EQ.'NTMIN') THEN
        NTMIN=NINT(V)
      ELSEIF (KEY.EQ.'D0MIN') THEN
        D0MIN=V
      ELSEIF (KEY.EQ.'PMIN') THEN
        PMIN=V
      ELSEIF (KEY.EQ.'DXYMAX') THEN
        DXYMAX=V
      ELSEIF (KEY.EQ.'RVMIN') THEN
        RVMIN=V
      ELSEIF (KEY.EQ.'RVMAX') THEN
        RVMAX=V
      ELSEIF (KEY.EQ.'DZMAX') THEN
        DZMAX=V
      ELSEIF (KEY.EQ.'CPMIN') THEN
        CPMIN=V
      ELSEIF (KEY.EQ.'PKMIN') THEN
        PKMIN=V
      ELSEIF (KEY.EQ.'ZPMAX') THEN
        ZPMAX=V
      ELSEIF (KEY.EQ.'FITVTX') THEN
        FITV=V
      ELSEIF (KEY.EQ.'SIGXY') THEN
        SIGXY=V
      ELSEIF (KEY.EQ.'SIGZ') THEN
        SIGZ=V
      ELSEIF (KEY.EQ.'CHIMAX') THEN
        CHIMAX=V
      ELSE
        WRITE(*,*) 'k0s: unknown key in the card: ',KEY(1:L-1)
        STOP 2
      ENDIF
      GO TO 10
   90 CLOSE(13)
      WRITE(*,'(A,I4)') ' cut NHMIN  ',NHMIN
      WRITE(*,'(A,I4)') ' cut NTMIN  ',NTMIN
      WRITE(*,'(A,F9.3)') ' cut D0MIN  ',D0MIN
      WRITE(*,'(A,F9.3)') ' cut PMIN   ',PMIN
      WRITE(*,'(A,F9.3)') ' cut DXYMAX ',DXYMAX
      WRITE(*,'(A,F9.3)') ' cut RVMIN  ',RVMIN
      WRITE(*,'(A,F9.3)') ' cut RVMAX  ',RVMAX
      WRITE(*,'(A,F9.3)') ' cut DZMAX  ',DZMAX
      WRITE(*,'(A,F9.3)') ' cut CPMIN  ',CPMIN
      WRITE(*,'(A,F9.3)') ' cut PKMIN  ',PKMIN
      WRITE(*,'(A,F9.3)') ' cut ZPMAX  ',ZPMAX
      WRITE(*,'(A,F9.3)') ' FITVTX     ',FITV
      WRITE(*,'(A,F9.3)') ' SIGXY      ',SIGXY
      WRITE(*,'(A,F9.3)') ' SIGZ       ',SIGZ
      WRITE(*,'(A,ES10.3)') ' cut CHIMAX ',CHIMAX
      RETURN
      END

      SUBROUTINE VFIT(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,
     +                X,Y,Z,CHI2)
C Fit of the common vertex of two tracks (a least squares fit with the
C assumed errors SXY and SZ; the covariance matrices of the tracks are not
C used). The unknowns are the position (x,y) of the vertex and its z, which
C is the mean of the z of the two tracks at the vertex (z = Z0 + dz/ds * r
C as in the rest of the program, r the distance from the beam). The
C residuals are
C   (distance of the vertex from the centre - radius)/SXY  for each circle
C   (z of the track 1 - z of the track 2)/(SZ*sqrt(2))
C i.e. 3 residuals and 2 parameters (one degree of freedom). The start
C values of X and Y are the point found from the intersection of the
C circles; the fit is a Gauss-Newton iteration with damping and numerical
C derivatives. On return X,Y,Z is the fitted vertex and CHI2 the sum of
C the squares of the residuals.
      IMPLICIT NONE
      REAL CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y,Z,CHI2
      REAL RES(3), RP(3), RM(3), A(2,2), B(2), JAC(3,2), H
      REAL CH, CHN, XN, YN, DET, DX, DY, XLAM
      INTEGER IT, I, J, K
      PARAMETER (H=0.05)
      XLAM=1.E-3
      CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y,RES)
      CH=RES(1)**2+RES(2)**2+RES(3)**2
      DO 100 IT=1,20
C Jacobian
        CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X+H,Y,
     +    RP)
        CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X-H,Y,
     +    RM)
        DO 10 K=1,3
          JAC(K,1)=(RP(K)-RM(K))/(2.*H)
   10   CONTINUE
        CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y+H,
     +    RP)
        CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y-H,
     +    RM)
        DO 20 K=1,3
          JAC(K,2)=(RP(K)-RM(K))/(2.*H)
   20   CONTINUE
        DO 40 I=1,2
          B(I)=0.
          DO 30 J=1,2
            A(I,J)=0.
   30     CONTINUE
   40   CONTINUE
        DO 60 K=1,3
          DO 50 I=1,2
            B(I)=B(I)-JAC(K,I)*RES(K)
            DO 45 J=1,2
              A(I,J)=A(I,J)+JAC(K,I)*JAC(K,J)
   45       CONTINUE
   50     CONTINUE
   60   CONTINUE
C Damped step, increase the damping until the chi2 does not rise
   70   CONTINUE
        A(1,1)=A(1,1)*(1.+XLAM)+1.E-9
        A(2,2)=A(2,2)*(1.+XLAM)+1.E-9
        DET=A(1,1)*A(2,2)-A(1,2)*A(2,1)
        IF (ABS(DET).LT.1.E-20) GO TO 110
        DX=(B(1)*A(2,2)-B(2)*A(1,2))/DET
        DY=(A(1,1)*B(2)-A(2,1)*B(1))/DET
        XN=X+DX
        YN=Y+DY
        CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,XN,YN,
     +    RES)
        CHN=RES(1)**2+RES(2)**2+RES(3)**2
        IF (CHN.LE.CH) THEN
          X=XN
          Y=YN
          IF (ABS(CH-CHN).LT.1.E-4) THEN
            CH=CHN
            GO TO 110
          ENDIF
          CH=CHN
          XLAM=XLAM*0.3
        ELSE
          XLAM=XLAM*10.
          IF (XLAM.GT.1.E6) GO TO 110
C restore the residuals of the old point and the matrix
          CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y,
     +      RES)
          GO TO 100
        ENDIF
  100 CONTINUE
  110 CONTINUE
      CALL VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y,RES)
      CHI2=RES(1)**2+RES(2)**2+RES(3)**2
      Z=0.5*((Z01+DZ1*SQRT(X*X+Y*Y))+(Z02+DZ2*SQRT(X*X+Y*Y)))
      RETURN
      END

      SUBROUTINE VRES(CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y,
     +                RES)
C Residuals of the fit of the vertex (see VFIT)
      IMPLICIT NONE
      REAL CX1,CY1,R1,Z01,DZ1,CX2,CY2,R2,Z02,DZ2,SXY,SZ,X,Y,RES(3)
      REAL D1,D2,RV
      D1=SQRT((X-CX1)**2+(Y-CY1)**2)
      D2=SQRT((X-CX2)**2+(Y-CY2)**2)
      RV=SQRT(X*X+Y*Y)
      RES(1)=(D1-R1)/SXY
      RES(2)=(D2-R2)/SXY
      RES(3)=((Z01+DZ1*RV)-(Z02+DZ2*RV))/(SZ*1.41421356)
      RETURN
      END
