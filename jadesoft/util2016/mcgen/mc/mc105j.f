CDECK  ID>, EVWRTJ.
      SUBROUTINE EVWRTJ ( NTRAK,ITKDM,MXTRK,PTRAK,IDTRAK,IHSTRY
     +                 ,IDAUGH,CHRG,VERT,ECMASS,IFLAV,CNAM
     +                 ,VTX,JRUN,IEVT,IZRN,LUN,COPTN,IERR )
      IMPLICIT NONE
**********************************************************************
*.
*...EVWRTJ
*.
*. Routine to write an event record to a file in the JADE 'CPROD' Format
*
*. INPUT     : NTRAK   The number of entries in the event record
*. INPUT     : ITKDM   The first dimension of PTRAK
*. INPUT     : MXTRK   The maximum number of entries in PTRAK
*. INPUT     : PTRAK   The 5-momenta array of the event record (Px,Py,Pz,E,M)
*. INPUT     : IDTRAK  The IDs of the particles in the event record
*. INPUT     : IHSTRY  The parent numbers of the particles in the event record
*. INPUT     : IDAUGH  The daughter numbers of the particles in the event record
*. INPUT     : CHRG    The charges of the particles in the event record
*. INPUT     : VERT    The production vertices of the particles in the event record
*. INPUT     : VTX     The vertex position for the event (for all particles)
*. INPUT     : ECMASS  The center-of-mass energy of the event
*. INPUT     : IFLAV   The event flavour
*. INPUT     : CNAM    The names of the particles (only for tests)
*. INPUT     : IRUN    The run number, to label the record
*. INPUT     : IEVT    The event number, to label the record
*. INPUT     : LUN     The logical unit for read or for write
*. INPUT     : COPTN   = 'WB' for binary file option.
*.                     = 'WN' for HBOOK NTuple file option (not yet implemented!!!).
*.                     = 'WA' for ascii file option (only for test purposes).
*. OUTPUT    : IERR    = -1 if there is a problem
*.                     =  0 if all is OK
*. CALLED    : By User
*.
*. AUTHOR    : P.A. Movilla Fernandez
*. CREATED   : 15-May-00
*. LAST MOD  : 06-Jun-01
*.
**********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      INTEGER  IDTRAK (*),IHSTRY (*),IDAUGH (*)
      INTEGER  NTRAK,ITKDM,LUN,IERR,JRUN,IEVT,MXTRK,IZRN,IFLAV
      REAL  PTRAK (ITKDM,*),VTX (*), VERT (5,*), CHRG(*), ECMASS
      REAL XDUMMY(10),SVEC(3,3), ANG
      CHARACTER*(*) COPTN
      CHARACTER*(*) CNAM(*)
      REAL RADDEG
      PARAMETER( RADDEG=57.2958 )
C
      INTEGER I,KF,AKF,ICALL,JADECO,IER2
      DATA ICALL /0/
      SAVE ICALL
Code:
      IERR = 0
C
      ICALL=ICALL+1
      IF( ICALL.EQ.1 )
     +     WRITE(*,'(/,A,/)') 'EVWRTJ called instead of EVWRIT !'
C
C  Fill common /CPROD/:
C
C  Initialization
      CALL INCPRD
C  Get some general info
      NEV=IEVT
      BEAM=ECMASS/2.0
C  Get event flavour in Z0/gamma
      IFLAVR=IFLAV
C  Total number of 'all' particles
      NP=NTRAK
C  Check /CPROD/ limits
      IF( NP.GT.500 ) THEN
        WRITE(*,'(A)') ' EVWRTJ: Too many particles for /CPROD/ !'
        WRITE(*,'(T10,A4,I4,A)') 'NP=',NP,' ... skip event'
        IERR=-1
        GOTO 990
      ENDIF
C
C  Particle loop
      DO I=1,NP
         KF=IDTRAK(I)
         AKF=ABS(KF)
C
C--- List of 'all' (final + intermediate)  particles
C     Number/charge of particles in the 'all' list
         IF( ABS(CHRG(I)).LT.1E-6 ) THEN
            NN=NN+1
            JCH(I)=0
         ELSE
            NC=NC+1
            JCH(I)=CHRG(I)
         ENDIF
C     Particle type (JADE convention)
         JTP(I)=JADECO(KF)
C     Parent and first daughter of the particle
         JP(I,1)=IHSTRY(I)
         JP(I,2)=IDAUGH(I)
C     Mass of the particle
         XM(I)=PTRAK(5,I)
C     4-vectors
         PP(1,I)=PTRAK(1,I)
         PP(2,I)=PTRAK(2,I)
         PP(3,I)=PTRAK(3,I)
         PP(4,I)=PTRAK(4,I)
C     Particle name
         CP(I)=CNAM(I)(:)
C--- List of 'final' (stable)  particles excepting neutrinos
         IF( IDAUGH(I).EQ.0
     +        .AND.AKF.NE.12.AND.AKF.NE.14.AND.AKF.NE.16
     +        .AND.AKF.GT.10
     +        .AND.AKF.NE.21.AND.AKF.NE.23.AND.AKF.NE.24) THEN
C        Number of charged/neutral particles
            NF=NF+1
C        Check /CPROD/ limits
            IF(NF.GT.300) THEN
               WRITE(*,'(A)')
     +          ' EVWRTJ: Too many final particles for /CPROD/ !'
               WRITE(*,'(T10,A4,I4,A)') 'NF=',NF,' ... skip event'
               IERR=-1
               GOTO 990
            ENDIF
C        Number of charged/neutral particles in the 'final' list
            ICF(NF)=JCH(I)
            IF(ICF(NF).EQ.0) THEN
               NNF=NNF+1
            ELSE
               NCF=NCF+1
            ENDIF
C        Type/Mass/charge/4-vector
            ITF(NF)=JTP(I)
            XMF(NF)=XM(I)
            ICF(NF)=JCH(I)
            PF(1,NF)=PP(1,I)
            PF(2,NF)=PP(2,I)
            PF(3,NF)=PP(3,I)
            PF(4,NF)=PP(4,I)
C        Origin of the particles in [mm]
            PSTRT(1,NF)=VERT(I,1)
            PSTRT(2,NF)=VERT(I,2)
            PSTRT(3,NF)=VERT(I,3)
C        Name
            CF(NF)=CNAM(I)
C        Check Particle code
            IF(ITF(NF).LT.1.OR.ITF(NF).GT.6) THEN
              WRITE(*,'(A)')
     +         ' EVWRTJ: Illegal final state particle code'
     +             //'for /CPROD/ common !'
              WRITE(*,'(T10,A12,I4,I8,A)') 'NF,ITF(NF)=',NF,ITF(NF)
              IERR = -1
              GOTO 990
            ENDIF
         ENDIF
      ENDDO
C  Pt/Phi/Theta of event
      PT=0.
      PHI=0.
      THETA=0.
      CALL PXJSP3(NF,4,PF,XDUMMY,SVEC,IER2)
      IF( IER2.EQ.0 ) THEN
         THETA=ACOS(SVEC(3,3))*RADDEG
         CALL PXANXY(SVEC(1,3),SVEC(3,3),ANG,IER2)
         IF( IER2.EQ.0 ) THEN
            PHI=ANG*RADDEG
         ELSE
            PHI=0.
         ENDIF
         DO I=1,NF
            PT=PT+SQRT(PF(1,I)*PF(1,I)+PF(2,I)*PF(2,I))
         ENDDO
      ENDIF
C
C     IF( ICALL.LE.NRTOPR) CALL PRCPRD(6,IERR)
C
C Write out common /CPROD/
C
      IF (COPTN(1:1).EQ.'W') THEN
C
C- Binary format (JADE 'CPROD' format)
         IF(LEN(COPTN).GE.2 .AND. COPTN(2:2).EQ.'B') THEN
            CALL WRCPRD(LUN,IERR)
C
C- Special Ntuple format
         ELSE IF(LEN(COPTN).GE.2 .AND. COPTN(2:2).EQ.'N') THEN
            CALL NTCPRD(LUN,IERR)
C
C- Readable ASCII format 
         ELSE IF(LEN(COPTN).GE.2 .AND. COPTN(2:2).EQ.'A') THEN
            CALL PRCPRD(LUN,IERR)
         ELSE
            WRITE (*,FMT='('' COPTN ='',A6,
     +           '' option unknown'')')  COPTN
            IERR = -1
            GO TO 990
         ENDIF
C
C- Something else ?
      ELSE
          WRITE (*,FMT='('' COPTN ='',A6,
     +          '' option unknown'')')  COPTN
          IERR = -1
          GO TO 990
      END IF
C 
 990  RETURN
1000  WRITE (LUN,FMT='('' Error writing LUN '',I6)') LUN
      IERR = -1
      RETURN
      END
CDECK  ID>, JADECO.
      INTEGER FUNCTION JADECO(PDGCODE)
      IMPLICIT NONE
********************************************************************
*.
*.  Convert PDG particle code to JADE particle code ( see JCN 10 )
*.
********************************************************************
      INTEGER IC,ISIG,PDGCODE
      INTEGER SIGN
Code:

      ISIG=SIGN(1,PDGCODE)
      IC=ABS(PDGCODE)
      IF (IC.EQ.22)  THEN
         JADECO=1               !gamma
      ELSEIF(IC.EQ.11) THEN
         JADECO=2               !e
      ELSEIF(IC.EQ.13) THEN
         JADECO=3               !mu
      ELSEIF(IC.EQ.211) THEN
         JADECO=4               !Pi
      ELSEIF(IC.EQ.111) THEN
         JADECO=40              !Pi0
      ELSEIF(IC.EQ.321.OR.IC.EQ.311.OR.IC.EQ.130) THEN
         JADECO=5               !K+-0long
      ELSEIF(IC.EQ.310) THEN
         JADECO=50              !K0short
      ELSEIF(IC.EQ.2112.OR.IC.EQ.2212) THEN
         JADECO=6               !Nukleon
      ELSEIF(IC.GE.1.AND.IC.LE.6) THEN
         JADECO=IC+500          !Quarks
      ELSEIF(IC.EQ.21) THEN
         JADECO=500             !Gluons
C----- JADE codes for unstable particles from JCN 10 -------
C      Not recommended, use PDG code + offset. 
C
C     ELSEIF(IC.EQ.333) THEN
C        JADECO=7               !Phi
C     ELSEIF(IC.EQ.221) THEN
C        JADECO=8               !Eta
C     ELSEIF(IC.EQ.331) THEN
C        JADECO=9               !Eta'
C     ELSEIF(IC.EQ.223) THEN
C        JADECO=10              !Omega
C     ELSEIF(IC.EQ.323.OR.IC.EQ.313) THEN
C        JADECO=11              !K*(890)+-0
C     ELSEIF(IC.EQ.113.OR.IC.EQ.213) THEN
C        JADECO=12              !Rho+-0
C     ELSEIF(IC.EQ.443) THEN
C        JADECO=13              !J/Psi
C     ELSEIF(IC.EQ.441) THEN
C        JADECO=15              !Eta_c
C     ELSEIF(IC.EQ.411.OR.IC.EQ.421) THEN
C        JADECO=16              !D+-0
C     ELSEIF(IC.EQ.413.OR.IC.EQ.423) THEN
C        JADECO=17              !D*+-0
C     ELSEIF(IC.EQ.431) THEN
C        JADECO=18              !Ds+ (F+)
C     ELSEIF(IC.EQ.433) THEN
C        JADECO=19              !Ds+* (F+*)
      ELSE
         JADECO=IC+100000
      ENDIF
C     JADECO=JADECO*ISIG   
      RETURN
      END
CDECK  ID>, WRCPRD.
      SUBROUTINE WRCPRD(LUN,IERR)
      IMPLICIT NONE
*********************************************************************
*.
*.  Write out 4-vector information in a binary format 
*.  (JADE 'CPROD' format, see JADE Computer Note 69)
*.
*********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      INTEGER I1,I2,I3,I4,I5,N,N2
      INTEGER LUN,IERR
Code:
      WRITE(UNIT=LUN,ERR=10,IOSTAT=IERR) 
     *     NEV, BEAM, PT, PHI, THETA, IFLAVR,
     *     NP, NC, NN, ( (PP(I4,N), I4=1,4), XM(N), JCH(N), JTP(N),
     *     (JP(N,I2), I2=1,2), N=1,NP),
     *     NF, NCF, NNF, ( (PF(I4,N2),I4=1,4), XMF(N2), ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
      RETURN
 10   WRITE(LUN,'(A,2I7)') ' WRCPRD: WRITE ERROR, NEV, IOSTAT=',NEV,IERR
      RETURN
      END
C
CDECK  ID>, PRCPRD.
      SUBROUTINE PRCPRD(LUN,IERR)
      IMPLICIT NONE
*********************************************************************
*.
*.  Print out 4-vector information in common /CPROD/ 
*.  in a readable format.
*.
*********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      INTEGER I1,I2,I3,I4,I5,N,N2
      INTEGER LUN,IERR
Code:
      WRITE(LUN,'(/A,I8)') ' PRCPRD: Content of common /CPROD/ in event'
     *     ,NEV
      WRITE(LUN,'(/A6,I7,A6,F6.2,A8,I2)') 
     *     ' NEV=',NEV,' BEAM=',BEAM,' IFLAVR=',IFLAVR
      WRITE(LUN,'(A14,3F8.3)') ' PT/PHI/THETA=',PT, PHI, THETA
      WRITE(LUN,'(/A15,A10,3I4)') ' All particles:'
     *     ,' NP/NC/NN=',NP, NC, NN
      WRITE(LUN,'(T21,5A9,A5,A8,A6)') 
     *     'PP(1,I)','PP(2,I)','PP(3,I)','PP(4,I)','PP(5,I)'
     *     ,'JCH','JTP','JP'
      WRITE(LUN,'(I3,A17,5F9.3,I5,I8,2I4)')
     *     ( N,CP(N),(PP(I4,N), I4=1,4), XM(N), JCH(N), JTP(N),
     *     (JP(N,I2), I2=1,2), N=1,NP)
      WRITE(LUN,'(/A23,A13,3I4)') ' Final state particles:'
     *     ,' NF/NCF/NNF=',NF, NCF, NNF
      WRITE(LUN,'(T21,5A9,2A5,A21)') 
     *     'PF(1,I)','PF(2,I)','PF(3,I)','PF(4,I)','PF(5,I)'
     *     ,'ICF','ITF','PSTRT-X -Y -Z'
      WRITE(LUN,'(I3,A17,5F9.3,2I5,3F9.3)')
     *     (N2,CF(N2),(PF(I4,N2),I4=1,4), XMF(N2), ICF(N2),ITF(N2),
     *     (PSTRT(I3,N2),I3=1,3),N2=1,NF)
C     
      RETURN
      END
C
CDECK  ID>, NTCPRD.
      SUBROUTINE NTCPRD(LUN,IERR)
      IMPLICIT NONE
*********************************************************************
*.
*.  Write out 4-vector information in a NTuple format 
*.  (still under construction!)
*.
*********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      INTEGER LUN,IERR
Code:
      RETURN
      END
C
CDECK  ID>, INCPRD.
      SUBROUTINE INCPRD
      IMPLICIT NONE
*********************************************************************
*.
*.  Initialization of common /CPROD/ 
*.
*********************************************************************
C  4-vector format for the JADE simulation program MCJADE
C  (see JADE Computer Note 69)
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
Code:
      PT=0.
      PHI=0.
      THETA=0.
      NP=0
      NC=0
      NN=0
      NF=0
      NCF=0
      NNF=0
      CALL VZERO(PP,4*500)
      CALL VZERO(XM,500)
      CALL VZERO(JCH,500)
      CALL VZERO(JTP,500)
      CALL VZERO(JP,2*500)
      CALL VZERO(PF,4*300)
      CALL VZERO(XMF,300)
      CALL VZERO(ICF,300)
      CALL VZERO(ITF,300)
      CALL VZERO(PSTRT,3*300)
      CALL VBLANK(CP,4)
      CALL VBLANK(CF,4)
      RETURN
      END
CDECK  ID>, MYTYPE.
      SUBROUTINE MYTYPE(ITYPE)
      IMPLICIT NONE
*. WWTYPE   Determines event type for WW or Z/gamma events.
*.
*. INPUT     :
*. OUTPUT    :  ITYPE = 1  Z  --> hadrons
*.                      2  Z  --> leptons
*.                      3  WW --> jjjj
*.                      4  WW --> jjll
*.                      5  WW --> llll
*.                      6  ZZ --> jjjj
*.                      7  ZZ --> jjll
*.                      8  ZZ --> llll
*.                      0  anything else
*.
*.**********************************************************************
*.
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /GUJETS/
      INTEGER ITYPE,NW,NZ,NQQ,ITREE,IID,NPAR,IPAR(2)
*
      ITYPE = 0
      NPAR  = 0
      IPAR(1) = 0
      IPAR(2) = 0
*
*--- Find number of Ws and Zs, and determine if they decay to quarks
*--- or leptons..
*
      NW = 0
      NZ = 0
      NQQ = 0
      DO 10 ITREE = 1,N
        IID = ABS(K(ITREE,2))
        IF(K(ITREE,3).NE.0) GO TO 10
        IF(IID.LT.23 .OR. IID.GT.24) GO TO 10
        NPAR = NPAR + 1
        IPAR(NPAR) = ITREE
        IF(IID.EQ.23) THEN
          NZ = NZ + 1
        ELSE
          NW = NW + 1
        ENDIF
   10 CONTINUE
      DO 20 ITREE = 1,N
        IID = ABS(K(ITREE,2))
        IF(K(ITREE,3).EQ.0) GO TO 20
        IF(K(ITREE,3).NE.IPAR(1) .AND. K(ITREE,3).NE.IPAR(2)) GO TO 20
        IF(IID.GE.1.AND.IID.LE.8) NQQ = NQQ + 1
   20 CONTINUE
      IF(NW.EQ.0 .AND. NZ.EQ.0) GO TO 999
*
*--- Classify event.
*
      IF(NW.EQ.2) THEN
        IF(NQQ.EQ.4) THEN
          ITYPE = 3
        ELSEIF(NQQ.EQ.2) THEN
          ITYPE = 4
        ELSEIF(NQQ.EQ.0) THEN
          ITYPE = 5
        ELSE
          ITYPE = 0
        ENDIF
      ELSEIF(NZ.EQ.2) THEN
        IF(NQQ.EQ.4) THEN
          ITYPE = 6
        ELSEIF(NQQ.EQ.2) THEN
          ITYPE = 7
        ELSEIF(NQQ.EQ.0) THEN
          ITYPE = 8
        ELSE
          ITYPE = 0
        ENDIF
      ELSEIF(NZ.EQ.1) THEN
        IF(NQQ.EQ.2) THEN
          ITYPE = 1
        ELSEIF(NQQ.EQ.0) THEN
          ITYPE = 2
        ELSE
          ITYPE = 0
        ENDIF
      ENDIF
*
  999 RETURN
      END
CDECK  ID>, MCSTBL.
      SUBROUTINE MCSTBL
      IMPLICIT NONE
C  Routine to set hadrons with PDG IDs in list IDSTBL stable within JETSET.
C  Code taken from L74LON and moved into subroutine to allow consistent
C  use with other MCs.
C  Stefan Kluth, 26.1.96
C  Modifications:
C     P. Movilla Fernandez, 06.07.00:
C                  Extend list of stable particles (suppression of some weak decays)
C     PMF 27.07.0: Get list of PDG IDs from FFREAD
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
      INTEGER IP,NSTBL,LUCOMP
      PARAMETER( NSTBL= 7+12 +31 )
      INTEGER IDSTBL(NSTBL)
      CHARACTER*8 CNAMS(NSTBL)
      CHARACTER*16 CHAU
      DATA IDSTBL / 3112,3122,3222,3312,3322,3334,310 
     >     ,411,421,431                           ! PMF: add some c hadrons
     >     ,511,513,521,523,531,533,541,543,5122  ! PMF: add some b hadrons
     >     ,31*0 /                                ! PMF: Array fixed now to dim. 50
      SAVE IDSTBL
C  PMF: get PDG IDs from FFREAD input
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
C
C  Overwrite IDSTBL with IDSTAB
      IF ( IDSTAB(1).NE. 0 ) CALL UCOPY(IDSTAB,IDSTBL,50)
C
C  Loop over list, get particle names and set stable:
      DO IP=1,NSTBL
        IF ( IDSTBL(IP).EQ.0 ) GO TO 10
        CALL LUNAME(IDSTBL(IP),CHAU)
        CNAMS(IP)= CHAU
        MDCY(LUCOMP(IDSTBL(IP)),1)= 0
      ENDDO
C
C  Print information:
 10   PRINT*,'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      PRINT*,'MCSTBL:'
      PRINT*,'The following particles (and their anti-particles) have'
      PRINT*,'been declared stable for JETSET'
      CALL PXPRCV('CNAMS',IP-1,CNAMS)
      PRINT*,'All other particles will decay by Jetset7.4'
      PRINT*,'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
C
C  The End:
      RETURN
      END
CDECK  ID>, MCST63.
      SUBROUTINE MCST63
      IMPLICIT NONE
C
C  Routine to set hadrons with PDG IDs in list IDSTBL stable within JETSET6.3.
C
C  28.05.01 P.A.Movilla Fernandez: JETSET6.3 version of MCSTAB
C
      INTEGER IDB63,KDP63
      REAL DPAR63,CBR63
      COMMON/LUDAT3/DPAR63(20),IDB63(120),CBR63(400),KDP63(1600)
      SAVE /LUDAT3/
      INTEGER IP,NSTBL
      PARAMETER( NSTBL= 7+12+31 )
      INTEGER IDSTBL(NSTBL),IDST63(NSTBL),NST63,IDPDG,IDJT63
      CHARACTER*8 CNAMS(NSTBL)
      CHARACTER*8 CHAU
C Use Jetset7.4 codes!
      DATA IDSTBL / 3112,3122,3222,3312,3322,3334,310 
     >     ,411,421,431
     >     ,511,513,521,523,531,533,541,543,5122
     >     ,31*0 /                                
C  Fetch JETSET7.4 IDs from FFREAD input and
C  try to convert into JETSET6.3 convention
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
C     
C  Overwrite IDSTBL with IDSTAB
      IF ( IDSTAB(1).NE. 0 ) CALL UCOPY(IDSTAB,IDSTBL,50)
C
C  Perform conversion
      NST63=0
      DO IP=1,NSTBL
         IF( IDSTBL(IP).EQ.0 ) GO TO 5
         DO IDJT63=-392,392
            CALL PXJPD2(IDJT63,IDPDG)
            IF( IDPDG.NE.0 .AND. IDPDG.EQ.IDSTBL(IP) ) THEN
               NST63=NST63+1
               IDST63(NST63)=IDJT63
               WRITE(*,'(A,I6,A,I6,A)')
     >              'MCST63: conversion of JETSET7.4 code',IDSTBL(IP)
     >              ,' to JETSET6.3 code',IDST63(IP)
     >              ,' succeeded.'
               GOTO 2
            ENDIF
         ENDDO
         WRITE(*,'(A,I6,A)')
     >        'MCST63: conversion of JETSET7.4 code',IDSTBL(IP)
     >        ,' FAILED!!!.'
 2       CONTINUE
      ENDDO 
C
C  Loop over list, get particle names and set stable:
 5    DO IP=1,MAX(NST63,1)
        IF ( IDST63(IP).EQ.0 ) GO TO 10
        CALL LUNAME( IDST63(IP) ,CHAU)
        CNAMS(IP)= CHAU
        IDB63( ABS(IDST63(IP)) ) = -ABS( IDB63( ABS(IDST63(IP)) ) )
      ENDDO
C
C  Print information:
 10   PRINT*,'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      PRINT*,'MCST63:'
      PRINT*,'The following particles (and their anti-particles) have'
      PRINT*,'been declared stable for JETSET'
      CALL PXPRCV('CNAMS',IP-1,CNAMS)
      PRINT*,'All other particles will decay by Jetset6.3'
      PRINT*,'+++++++++++++++++++++++++++++++++++++++++++++++++++++++++'
C
C  The End:
      RETURN
      END
CDECK  ID>, JATUNE.
      SUBROUTINE JATUNE
      IMPLICIT NONE
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
C
      PRINT *,' ****  SPECIAL JADE TUNE **** '
      PRINT *,' ****  SPECIAL JADE TUNE **** '
      PRINT *,' ****  SPECIAL JADE TUNE **** '
*
*  fragmentation parameters (Peterson + Lund symmetric)
*
      PRINT *,' ORIGINAL FF PARAMETERS'
      CALL LUGIVE ('PARJ(21)=0.3')
      CALL LUGIVE ('PARJ(41)=0.5')
      CALL LUGIVE ('PARJ(42)=0.9')
      CALL LUGIVE ('PARJ(81)=0.4')
      CALL LUGIVE ('PARJ(82)=1.0')
      CALL LUGIVE ('MSTJ(11)=3')
      CALL LUGIVE ('PARJ(54)=-0.05')
      CALL LUGIVE ('PARJ(55)=-0.01')
      IF( JADETUN.EQ.2 ) THEN
      PRINT *,' ORIGINAL SUPPRESSION FACTORS'
*
*  di-quark suppression factor(D=0.10)
*
      CALL LUGIVE ('PARJ(1)=0.1')
*
*  strange di-quark suppression factor(D=0.40)
*
      CALL LUGIVE ('PARJ(3)=0.4')
*
*  spin 1 di-quark suppression factor(D=0.05)
*
      CALL LUGIVE ('PARJ(4)=0.05')
*
*  strange quark suppression factor(D=0.30)
*
      CALL LUGIVE ('PARJ(2)=0.3')
*
*  prob that a non-strange meson has spin 1(D=0.50)
*
      CALL LUGIVE ('PARJ(11)=0.5')
*
*  prob that a strange meson has spin 1(D=0.60)
*
      CALL LUGIVE ('PARJ(12)=0.6')
*
*  prob that a charm meson has spin 1(D=0.75)
*
      CALL LUGIVE ('PARJ(13)=0.75')
*
*  allow the L=1 supermultiplet for mesons(D=4*0.0)
*
      PRINT *,' DISABLING L=1 MULTIPLETTS'
      CALL LUGIVE ('PARJ(14)=0.0')
      CALL LUGIVE ('PARJ(15)=0.0')
      CALL LUGIVE ('PARJ(16)=0.0')
      CALL LUGIVE ('PARJ(17)=0.0')
      ENDIF
      PRINT *,' ****  SPECIAL JADE TUNE **** '
      PRINT *,' ****  SPECIAL JADE TUNE **** '
      PRINT *,' ****  SPECIAL JADE TUNE **** '
C
C The End:
      RETURN
      END
CDECK  ID>, JTCONV.
      SUBROUTINE JTCONV(IREP,IERR)
      IMPLICIT NONE
********************************************************************************
*.
*.  Performs conversion of Jetset 6.3 common block /LUJETS/
*.  into Jetset 7.4 style /GUJETS/
*.
*.  30.05.01   P.A.Movilla Fernandez
*.
********************************************************************************
C
C Jetset7.4 common blocks
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /GUJETS/
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
C
C Jetset6.3 common blocks
      INTEGER N63,K63
      REAL P63
      COMMON /LUJETS/ N63,K63(2000,2),P63(2000,5)
      SAVE /LUJETS/
      INTEGER MST63
      REAL PAR63
      COMMON/LUDAT1/MST63(40),PAR63(80)
      SAVE /LUDAT1/
C
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
C
      INTEGER IREP,IERR,IP,KS63,KH63,IDJT63,IDPDG,I,J
      LOGICAL LFIRST
      CHARACTER CHAU*8
Code:
      IERR=0
C
      IF( N.GT.MST63(30) ) THEN
         WRITE(*,'(A)') ' JTCONV: no more space in /LUJETS/'
         IERR=1
         GOTO 999
      ENDIF
C number of lines in event record
      N=N63
      DO I=1,N63
         KS63=INT(K63(I,1)/10000)
         KH63=MOD(K63(I,1),10000)
         IDJT63=K63(I,2)
C Status codes.
C If there are no JETSET7.4 equivalents, set status code =0 ( i.e. 'empty line')
C 
C     -translatable Jetset6.3 status codes
         IF( KS63.EQ.0 ) THEN
            K(I,1)=1              ! an undecayed particle / unfragmented jet (single or last)
         ELSE IF( KS63.EQ.1 ) THEN
            K(I,1)=2              ! an unfragmented jet, followed by more jets in the same colour singlet jet system
         ELSE IF( KS63.EQ.2 ) THEN
            K(I,1)=11             ! a decayed particle or fragmented jet (single or last)
         ELSE IF( KS63.EQ.3 ) THEN
            K(I,1)=12             ! a fragmented jet, followed by more jets in the same colour singlet jet system
C     -untranslatable Jetset 6.3 status codes
         ELSE IF( KS63.EQ.4 ) THEN
            K(I,1)=0          ! original beam or target particle (for documentation purposes), otherwise inactive
         ELSE IF( KS63.EQ.5 ) THEN
            K(I,1)=0          ! a virtually exchanged particle of the hard interaction, (for documentation purposes), otherwise inactive
         ELSE IF( KS63.EQ.6 ) THEN
            K(I,1)=0          ! continuation line, used to store extra info about hadron jets
         ELSE IF( KS63.EQ.7 .OR. KS63.EQ.8 ) THEN
            K(I,1)=0          ! reserved for further continuation lines
         ELSE
            K(I,1)=0          ! used temporarily for internal administration
         ENDIF
         IF( K(I,1).LE.0 ) THEN
            WRITE(*,'(A,I3,A,I8)') ' JTCONV: WARNIG, K(',I,',1)=',K(I,1)
            IERR=-2
         ENDIF
C flavour codes
         CALL PXJPDG(IDJT63,IDPDG)
         IF( IDPDG.EQ.0 ) THEN
            WRITE(*,'(A)')
     >           ' JTCONV: WARNING, particle code conversion failed!'
            IERR=2
            GOTO 999
         ENDIF
         K(I,2)=IDPDG
C history code (1)
C     -line number of parent particle or jet, otherwise 0
         K(I,3)=KH63
C 4 momentum + mass
         DO J=1,5
            IF( KS63.LT.6 ) THEN
               P(I,J)=P63(I,J)
            ELSE
               P(I,J)=0.
               IERR=-3
            ENDIF
         ENDDO
C vertex + production time (may be simulated here additionally)
         DO J=1,5
            V(I,J)=0.
         ENDDO
      ENDDO
C history code (2)
C     -line numbers of first and last daughter
      DO I=1,N
         IF( K(I,1).NE.3 .AND. K(I,1).NE.13 .AND. K(I,1).NE.14 ) THEN
            K(I,4)=0
            K(I,5)=0
            LFIRST=.TRUE.
            IF( I.LT.N ) THEN
               DO J=I+1,N
                  IF( K(J,3).EQ.I ) THEN
C     -line number of the first daughter, 0 for an undecayed particle or an unfragmented jet
                     IF( LFIRST ) THEN
                        LFIRST=.FALSE.
                        K(I,4)=J
                     ENDIF
C     -line number of the last daughter, 0 for an undecayed particle or an unfragmented jet
                     K(I,5)=J
                  ENDIF
               ENDDO
C       In Jetset7.4 event listing, all partons are the parent of a string,
C       and the string is the parent of the primary hadrons. In Jetset6.3 there
C       is no string in the event listing, and only the first and the last parton
C       has (unphysically)  a corresponding first+last daughter. Force here that
C       all further partons have also the same first+last daughters like the first parton.
               IF( I.GT.1 .AND.  K(I,3).EQ.0 .AND. K(I,4).EQ.0 ) THEN
                  K(I,4)=K(I-1,4)
                  K(I,5)=K(I-1,5)
               ENDIF
            ENDIF
         ELSE
            WRITE(*,'(A)') 'JTCONV: K(I,1)=',K(I,1),' not implemented.'
            IERR=-1
         ENDIF
      ENDDO
C Print some info
      IF( IREP.NE.0 ) THEN
         WRITE(*,'(/A//,T13,A,T69,A/)')
     >        ' JTCONV: Conversion of /LUJETS/ into JETSET7.4 style:'
     >        ,' /LUJETS/ in JETSET6.3:','/GUJETS/ in  JETSET7.4:'
         WRITE(*,'(A3,A8,1X,2A7,5A8,A2,5A7,5A8,5A8)')
     >        'I','NAME'
     >        ,'K(I,1)','K(I,2)'
     >        ,'P(I,1)','P(I,2)','P(I,3)','P(I,4)','P(I,5)','|'
     >        ,'K(I,1)','K(I,2)','K(I,3)','K(I,4)','K(I,5)'
     >        ,'P(I,1)','P(I,2)','P(I,3)','P(I,4)','P(I,5)'
     >        ,'V(I,1)','V(I,2)','V(I,3)','V(I,4)','V(I,5)'
         DO I=1,N
            CALl LUNAME(K63(I,2),CHAU)
            WRITE(*,'(I3,A9,2I7,5F8.3,A2,5I7,5F8.3,5E8.1)')
     >           I,CHAU,(K63(I,J),J=1,2),(P63(I,J),J=1,5),'|'
     >           ,(K(I,J),J=1,5),(P(I,J),J=1,5),(V(I,J),J=1,5)
         ENDDO

      ENDIF
C
 999  RETURN
      END
CDECK  ID>, MCINIT.
      SUBROUTINE MCINIT
      IMPLICIT NONE
C  Routine reads MC FFREAD cards and calls user FFREAD routine:
C  Stefan Kluth, 22.1.96
C  Modifications:
C  17.07.96, STK: Replace VZERO by VBLANK for hollerith arrays
c  07.07.00, PMF: Force initialization of common blocks
C  27.07.00, PMF: Read in also Peterson FF parameters PARJ(54),PARJ(55)
C                 Read in PDG IDs for particles to be set stable 
C                 in subroutine MCSTBL
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
      INTEGER HEVFILE(NWONAM)
      EXTERNAL MCEVBL,MCPOBL
c
C
C  Get FFREAD info:
      PRINT*,'MCINIT: read FFREAD cards'
      CALL FFINIT(0)
      CALL FFSET('SIZE',8)
C
C  General:
      IRUN= 6666
      CALL FFKEY('MCIRUN',IRUN,1,'INTEGER')
      NEVNT= 1000
      CALL FFKEY('MCNEVT',NEVNT,1,'INTEGER')
      ECMASS= 91.173
      CALL FFKEY('MCECMS',ECMASS,1,'REAL')
      IMCRDM= 13121965
      CALL FFKEY('MCRDM',IMCRDM,1,'INTEGER')
      NRTOPR= 3
      CALL FFKEY('MCNPRI',NRTOPR,1,'INTEGER')

      LTAB=.FALSE.
      CALL FFKEY('MCTAB',LTAB,1,'LOGI')

      LSTBL= .FALSE.
      CALL FFKEY('MCSTBL',LSTBL,1,'LOGI')
      LUDSC= .FALSE.
      CALL FFKEY('MCUDSC',LUDSC,1,'LOGI')
      LBQRK= .FALSE.
      CALL FFKEY('MCBQRK',LBQRK,1,'LOGI')
      SFLAV= 0
      CALL FFKEY('MCFLAV',SFLAV,1,'INTEGER')
      LISR= .FALSE.
      CALL FFKEY('MCISR',LISR,1,'LOGI')
      LFSR= .TRUE.
      CALL FFKEY('MCFSR',LFSR,1,'LOGI')
      LSTART= .FALSE.
      CALL FFKEY('MCSTART',LSTART,1,'LOGI')
      L4VEC= .FALSE.
      CALL FFKEY('MC4VEC',L4VEC,1,'LOGI')
      CALL VBLANK(HEVFILE,NWONAM)
      CALL FFKEY('MCEVFILE',HEVFILE,NWONAM,'MIXED')
C
C  JETSET related:
      JTB= 0.52
      CALL FFKEY('JTPB',JTB,1,'REAL')
      JTSQ= 0.40
      CALL FFKEY('JTPSQ',JTSQ,1,'REAL')
      JTQ0= 1.90
      CALL FFKEY('JTPQ0',JTQ0,1,'REAL')

      JTEC= -0.031
      CALL FFKEY('JTPEC',JTEC,1,'REAL')
      JTEB= -0.0038
      CALL FFKEY('JTPEB',JTEB,1,'REAL')

      JTA= 0.11
      CALL FFKEY('JTPA',JTA,1,'REAL')
      JTLAMB= 0.25
      CALL FFKEY('JTPLAMB',JTLAMB,1,'REAL')
C
C  List of PDG IDs needed in subroutine MCSTBL if MCSTBL=ON
      CALL VZERO(IDSTAB,50)
      CALL FFKEY('MCIDST',IDSTAB,50,'INTE')
C
C  User cards:
      CALL MCUSFF
C
C  Ok, lets go:
      CALL FFGO
C
C  Convert hollerith to CHARACTER:
      CALL UHTOC(HEVFILE,NBYTEW,CHEVFILE,NAMLEN)
C
C     The End:
      WRITE(*,*) 'Event generation at ecm: ',ECMASS
      RETURN
      END
CDECK  ID>, MCUSFF.
CAV      SUBROUTINE MCUSFF
CAV      IMPLICIT NONE
CAVC  Dummy routine
CAV      INTEGER ICALL
CAV      DATA ICALL /0/
CAV      SAVE ICALL
CAV      ICALL= ICALL+1
CAV      IF( ICALL.EQ.1 ) THEN
CAV        PRINT*,' '
CAV        PRINT*,'Dummy routine MCUSFF called'
CAV        PRINT*,' '
CAV      ENDIF
CAV      RETURN
CAV      END
CDECK  ID>, MCWRIT.
      SUBROUTINE MCWRIT(CHMODE,IERR)
*********************************************************************
*.
*...  MCWRIT Write a Monte Carlo event out
*.
*...  Pack the JETSET event record and write the event to disk    
*...  Copy code from J74PAK 
*.
*.  Input:  CHMODE    Switch, I= initialise, F= finish, W= write
*.  Output: IERR      Error flag
*.  
*.    Stan bentvelsen   17-04-96
*.
*. Modification log
*. 03-Apr-90  J.W.Gary     Fix bug in parton history      
*. 17-Jun-91  J.W.Gary     Update for Jetset73             
*. 22-Apr-95  A.S.Turcot   Change KF codes for P-wave   
*.                         baryons to standard PDG values  
*.                         Affects LUND KF/KC codes 41-51 
*. 06-Sep-95  A.S.Turcot   Fix Bug in PDG values for P-wave 
*.                         anti-baryons (sigh...)
*. 01-Mar-96  A.S.Turcot   Update for compatability with SUSYGEN 
*.                         Avoid sparticle - P-wave confusion
*. 19.09.96, STK: Copied from herwig59.car, renamed, 
*.                routine handles random numbers and event counting
*.
*. 16.05.00, PMF: Introduce a format option for the 4-vector output file
*.********************************************************************
      IMPLICIT NONE
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /GUJETS/
      INTEGER N63,K63
      REAL P63
      COMMON /LUJETS/ N63,K63(2000,2),P63(2000,5)
      SAVE /LUJETS/
      COMMON/GUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /GUDAT2/
**JWG  add psi'(3685) as KC code 80, needed for Eurodec interface
*      --- ---------- -- -- ---- --- ------ --- ------- ---------
      INTEGER  IDPSIP
      PARAMETER  (IDPSIP=80)
      COMMON/GUDATR/MRLU(6),RRLU(100)
      INTEGER  MRLU
      REAL  RRLU
      SAVE /GUDATR/
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
      CHARACTER*1 CHMODE
      INTEGER IERR,IFEVT,NEVT,IENUM
      SAVE IFEVT,NEVT
      INTEGER LWRIT,LRAN,LRAN2,LENOCC
      PARAMETER( LWRIT=33, LRAN=34, LRAN2=35 )
      REAL PLTRK(NPLBUF,NLUPDM)
      REAL VTX(3)
      SAVE VTX
      INTEGER  IP,IX,IPA
      LOGICAL  QPWAV, QFRST
      INTEGER  KFPWAV(11)
      DATA VTX /3*0.0/
      DATA KFPWAV / 13122,  3124, 13114, 13214, 13224, 13314, 13324, 
     &              12112, 12212,  1214,  2124 /
      DATA QFRST /.TRUE./
      DATA QPWAV /.FALSE./
      SAVE QFRST, QPWAV
C
C  MCJADE related
      REAL CHRG(NLUPDM),PLU,ULMASS
      CHARACTER*16 CNAM(NLUPDM)
      INTEGER INDEX,IFLAV,IDUM
      CHARACTER CFORM*12,COPT*2,CH8*8,CH16*16
      DATA COPT /'WB'/
      SAVE COPT
C
C  Check for P-wave baryons, key off mass in BDB decay table:
      IF( QFRST ) THEN
        QFRST= .FALSE.
        IF( PMAS(41,1).EQ.1.407 ) QPWAV= .TRUE.
      ENDIF    
C
C  Do special things for initialisation and finishing:
      IF( CHMODE.EQ.'I' ) THEN
C       Open 4-vector file:
        PRINT*,'MCWRIT: Output of 4-vectors to: '
     &        ,CHEVFILE(1:LENOCC(CHEVFILE))
C
C PMF 16/05/00: Introduce a format option for the 4-vector file,
C               depending on the extension of the filename:
C               extension = .cprod: binary
C               extension = .nt:    binary (HBOOK NTuple) (still not supported)
C               other:              ascii
C     
        IF(       INDEX(CHEVFILE,'.cprod').NE.0
     +       .OR. INDEX(CHEVFILE,'.CPROD').NE.0 ) THEN
           CFORM='UNFORMATTED'
           COPT='WB'
        ELSE
           CFORM='FORMATTED'
           COPT='WA'
        ENDIF
        PRINT *,'MCWRIT: Output file opened as '//CFORM//' file.'
        OPEN(UNIT=LWRIT,STATUS='NEW',FILE=CHEVFILE,FORM=CFORM)
C       Random number stuff:
        IF( LSTART ) THEN
C         Initialise random number files:
          IENUM= 0
          CALL MCRNST(LRAN,LRAN2,IENUM,'I',IERR)
          IF( IERR.NE.0 ) STOP
        ENDIF
C       Read initial random number sequence and event number from disk:
        CALL MCRNST(LRAN,LRAN2,IENUM,'R',IERR)
        IF( IERR.NE.0 ) STOP
        PRINT*,'MCWRIT: Initial seeds'
        WRITE(6,FMT='('' MRLU ='',6(I12))') MRLU
        CALL PXPRRV('RRLU',100,RRLU)
        IFEVT= IENUM
        PRINT*,'MCWRIT: The run number will be ',IRUN
        PRINT*,'MCWRIT: The first event number will be ',IFEVT+1
C       Set event counter for this run to zero:
        NEVT= 0
        RETURN
      ELSEIF( CHMODE.EQ.'F' ) THEN
C       Give some summary info:
        PRINT*,'MCWRIT: First and last event written in this run: '
     &        ,IFEVT+1,IFEVT+NEVT
        PRINT*,'MCWRIT: Final seeds'
        WRITE(6,FMT='('' MRLU ='',6I12)') MRLU
        CALL PXPRRV('RRLU',100,RRLU)
        IENUM= IFEVT+NEVT
C       Save random seeds on disk:
        CALL MCRNST(LRAN,LRAN2,IENUM,'W',IERR)
        CLOSE(UNIT=LWRIT)
        RETURN
      ENDIF
C
C  Make copy of event record:
      IF(  INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
         CALL LUEDIT(-1)
      ELSE
         CALL LUEDIT(21)
      ENDIF
C
C  Modify event record a bit:
      DO IP=1,N
C       Convert P-wave baryon codes:
        IF( QPWAV ) THEN
          IPA= ABS(K(IP,2))
          IF( IPA.GE.41 .AND. IPA.LE.51 ) THEN
            IF( K(IP,2).LT.0 ) THEN
              K(IP,2)= -KFPWAV(IPA-40)
            ELSE
              K(IP,2)= KFPWAV(IPA-40)
            ENDIF
          ENDIF
        ENDIF
C       Reverse order of P array:
        DO IX=1,5
          PLTRK(IX,IP)= P(IP,IX)
        ENDDO
C    Extract HERE all further info needed for /CPROD/ 4-vector format
C     - Charge of the particle:
C       (meaning of the PLU arguments is the same for Jetset6.3 and Jetset7.4)
        CHRG(IP)=PLU(IP,6)
C     - Name of the particle (only for debugging purposes):
        IF(  INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
           CALL LUNAME(K63(IP,2),CH8)
           CNAM(IP)=CH8
        ELSE
           CALL LUNAME(K(IP,2),CH16)
           CNAM(IP)=CH16
C         - Needed for detailed comparison with 4-vector production before Jun.2001
Comment    IDUM=ULMASS( K(IP,2) )
        ENDIF
      ENDDO
C     - Event flavour
      CALL MCGETF(IFLAV)
C     
C  Write event record to disk:
      NEVT= NEVT+1
C
C PMF 13.05.00:
C     Let patchy decide which write out subroutine should be used
C     - for OPAL format, use subroutine EVWRIT
C     - for JADE format, use subroutine EVWRTJ
      IF( LSTBL ) THEN
         IF( NEVT.LT.10 )
     +   WRITE(*,'(A)')
     >        ' MCWRIT: Call of EVWRTJ ignored because MCSTBL=ON !'
      ELSE
         CALL EVWRTJ(N,NPLBUF,NLUPDM,PLTRK,K(1,2),K(1,3)
     &           ,K(1,4),CHRG,V,ECMASS,IFLAV,CNAM
     &           ,VTX,IRUN,IFEVT+NEVT,0,LWRIT,COPT,IERR)
         IF( IERR.NE.0 ) THEN
            WRITE(*,'(A)') ' MCWRIT: Will stop now!' 
            STOP
         ENDIF
      ENDIF
C
C  Restore event record:
      IF(  INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
         CALL LUEDIT(-2)
      ELSE
         CALL LUEDIT(22)
      ENDIF
C
C  The End:
      RETURN
      END
CDECK  ID>, MCRNST.
      SUBROUTINE MCRNST(ILUN,ILCT,IENUM,MODE,IERR)
*.********************************************************************
*. Read or write random seeds for Herwig from or to disk
*.                                                              
*. INPUT     : ILUN     Logical unit number for random sequence
*. INPUT     : ILCT     Logical unit number for pointer to last sequence
*. INPUT     : IENUM    Event number corresponding to the random numbers
*. INPUT     : MODE     = 'R' for read
*.                      = 'W' for write
*.                      = 'I' for initial call (to create file)
*. OUTPUT    : IERR     = 0 if all is OK
*. 
*. AUTHOR    :  J.W.Gary                                                    
*. CREATED   :  26-Apr-89
*. LAST MOD  :  19.09.96
*. Modifications:
*. 19.09.96, STK: Copied from herwig59.car, renamed, cosmetic mods
*.  
********************************************************************          
      COMMON/GUDATR/MRLU(6),RRLU(100)
      INTEGER  MRLU
      REAL  RRLU
      SAVE /GUDATR/
      INTEGER IDUM(6),IRRLU(100),IRDUM(100)
      INTEGER ILUN,IPOS,I1,I2,INXT,ISKIP,IENUM,IERR,ILCT,IEDUM
      CHARACTER*1 MODE
      CHARACTER*11 FILNAM,FILCNT              
      EQUIVALENCE( IRRLU(1),RRLU(1) )
      DATA FILNAM / 'mcrnset.dat' / 
      DATA FILCNT / 'mcrncnt.dat' /
      SAVE FILNAM,FILCNT
C
C  Check input sensibility:
      IF ( .NOT.(MODE.EQ.'I'.OR.MODE.EQ.'R'.OR.MODE.EQ.'W') ) THEN
         WRITE (6,FMT='('' MCRNST:  Error, MODE '',A5,
     &     '' is unknown'')')
         RETURN
      ENDIF
C
C  Open files:
      IF( MODE.EQ.'I' ) THEN
C       Create new files and write initial entries: 
        OPEN(UNIT=ILUN,STATUS='NEW',FILE=FILNAM)
        OPEN(UNIT=ILCT,STATUS='NEW',FILE=FILCNT)
        IPOS= 1                    
        WRITE(ILCT,1060) IPOS
        WRITE(ILUN,1080) (MRLU(I1),I1=1,6),IENUM,(IRRLU(I2),I2=1,100)
        CLOSE(ILUN)
        CLOSE(ILCT)
      ELSE 
C       Find last position, read in or write out:
        OPEN(UNIT=ILCT,STATUS='OLD',FILE=FILCNT)
        OPEN(UNIT=ILUN,STATUS='OLD',FILE=FILNAM)
        READ(ILCT,1060) IPOS
        IF( MODE.EQ.'W' ) THEN
          INXT= IPOS + 1
          REWIND(ILCT)
          WRITE(ILCT,1060) INXT
        ENDIF
        CLOSE(ILCT)
        DO ISKIP=1,IPOS
          READ(ILUN,1080) (IDUM (I1),I1=1,6),IEDUM,(IRDUM (I2),I2=1,100)
        ENDDO
        IF( MODE.EQ.'W' ) THEN
          WRITE(ILUN,1080) (MRLU(I1),I1=1,6),IENUM,(IRRLU(I2),I2=1,100)
        ELSEIF( MODE.EQ.'R' ) THEN
          IENUM= IEDUM
          CALL PXCOPV(6,IDUM,MRLU)
          CALL PXCOPV(100,IRDUM,IRRLU)
        ENDIF
        CLOSE(ILUN)
      ENDIF
      RETURN
 1060 FORMAT(I12)
 1080 FORMAT(7I11,20(/5I16))
      END
CDECK  ID>, MCGETP.
      SUBROUTINE MCGETP(IMODE,IDIM,NTRAK,PTRAK,ILIN,IID,IPAR)
      IMPLICIT NONE
C  Select "stable" QCD partons and optionally FSR photons from JETSET
C  event record
C  Input:  IMODE   =1 select FSR photons too, =0 don't 
C          IDIM    1st dimension of array PTRAK
C  Output: NTRAK   Number of selected partons
C          PTRAK() Array of 5-momenta of selected partons (px,py,pz,E,P)
C          ILIN()  Line numbers in JETSET record
C          IID()   Particle IDs
C          IPAR()  Parent line numbers
C  Author: Stefan Kluth
C  Created: 06.11.96
C  Modifications: 25.05.00, PMF: Put charge info into PTRAK(6,NTRAK)
C                 01.06.01, PMF: Adapt code for use with Jetset6.3
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /GUJETS/
      INTEGER N63,K63
      REAL P63
      COMMON /LUJETS/ N63,K63(2000,2),P63(2000,5)
      SAVE /LUJETS/
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
      LOGICAL LKEEP
      INTEGER IDIM,IMODE,NTRAK,I,II,J,LUCHGE
      INTEGER ILIN(*),IID(*),IPAR(*)
      REAL PTRAK(IDIM,*)
C
C         If it has no parent it can't come from a shower:
C         except for COJETS

      NTRAK= 0
      DO I=1,N
        II= ABS(K(I,2))
        LKEEP= .FALSE.
C       First see if there is a candidate worth looking at, we
C       want a quark, gluon or photon with a parent:
        IF( K(I,3).GT.0 .AND. K(I,3).LE.N ) THEN
           IF( II.GE.1 .AND. II.LE.6 ) LKEEP= .TRUE.
           IF( II.EQ.21 ) LKEEP= .TRUE.
           IF( IMODE.EQ.1 .AND. II.EQ.22 .AND. 
     &        ABS(K(K(I,3),2)).GE.1 .AND. 
     &        ABS(K(K(I,3),2)).LE.6 ) LKEEP= .TRUE.
        ELSEIF( K(I,3).EQ.0 .AND. INDEX(CHMCTYP,'COJET').GT.0 ) THEN
           LKEEP= .TRUE.
        ELSEIF( K(I,3).EQ.0 .AND. INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
           IF( II.GE.1 .AND. II.LE.6 ) LKEEP= .TRUE.
           IF( II.EQ.21 ) LKEEP= .TRUE.
        ENDIF
        IF( LKEEP ) THEN
C         Check if there are any parton or CMshower daughters in the record:
          DO J=I+1,N
            IF( K(J,3).EQ.I .AND. (ABS(K(J,2)).LE.6 .OR.
     &          ABS(K(J,2)).EQ.21 .OR. ABS(K(J,2)).EQ.94) )
     &        LKEEP= .FALSE.
          ENDDO
C         For ARIADNE check that selected quarks point to a string:
          IF( INDEX(CHMCTYP,'ARIADNE').GT.0 .AND. II.LE.6 .AND.
     &        K(I,4).GT.0 .AND. K(I,4).LE.N ) THEN
            IF( K(K(I,4),2).NE.92 ) LKEEP= .FALSE.
          ENDIF
        ENDIF
C       If all is ok we keep it:
        IF( LKEEP ) THEN
          NTRAK= NTRAK+1
          ILIN(NTRAK)= I
          IID(NTRAK)=  K(I,2)
          IPAR(NTRAK)= K(I,3)
          PTRAK(1,NTRAK)= P(I,1)
          PTRAK(2,NTRAK)= P(I,2)
          PTRAK(3,NTRAK)= P(I,3)
          PTRAK(4,NTRAK)= P(I,4)
          PTRAK(5,NTRAK)= SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2)
          IF( IDIM.GE.6 ) THEN
             IF(  INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
                PTRAK(6,NTRAK)= REAL(LUCHGE(K63(I,2)))
             ELSE
                PTRAK(6,NTRAK)= REAL(LUCHGE(K(I,2)))
             ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C  The End:
      RETURN
      END
CDECK  ID>, MCGETF.
      SUBROUTINE MCGETF(JFLAV)
      IMPLICIT NONE
C  Get event flavour in Z/gamma events
C  Output:  JFLAV  flavour code, 1,..,6 = u,d,s,c,b,t
C                                =0: could not determine flavour
C  Author: Stefan Kluth
C  Created: 06.11.96
C  Modifications:
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /GUJETS/
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
      INTEGER JFLAV,I
C
      JFLAV= 0
      IF( INDEX(CHMCTYP,'PYTHIA').GT.0 .OR. 
     &    INDEX(CHMCTYP,'HERWIG').GT.0 .OR. 
     &    INDEX(CHMCTYP,'ARIADNEP').GT.0 ) THEN
C       Try the obvious and trace partons back to Z/gamma:
        DO I=1,N
          IF( ABS(K(I,2)).GE.1 .AND. ABS(K(I,2)).LE.6 .AND.
     &        K(K(I,3),2).EQ.23 ) THEN
            JFLAV= ABS(K(I,2))
            GOTO 10
          ENDIF
        ENDDO
   10   CONTINUE
      ELSE
C       Try first quark with no parent:
C       (Jetset6.3 and Cojets)
        DO I=1,N
          IF( ABS(K(I,2)).GE.1 .AND. ABS(K(I,2)).LE.6 .AND.
     &        K(I,3).EQ.0 ) THEN
            JFLAV= ABS(K(I,2))
            GOTO 20
          ENDIF
        ENDDO
   20   CONTINUE
      ENDIF
C
C  The End:
      RETURN
      END
CDECK  ID>, MCGETH.
      SUBROUTINE MCGETH(IDIM,NTRAK,PTRAK)
      IMPLICIT NONE
C  Select "stable" hadrons from JETSET event record,
C  fill some histograms
C  Input:  IDIM    1st dimension of PTRAK array, if IDIM.gt.6 the
C                  charge will returned in PTRAK(,6) 
C  Output: NTRAK   Number of selected hadrons
C          PTRAK() 5-vectors of selected hadrons, optionally charge
C  Author: Stefan Kluth (based on code from David Ward)
C  Created: 06.11.96
C  Modifications: 01.06.01, PMF: Adapt code for use with Jetset6.3
      INTEGER  NLUPDM,NPLBUF
      PARAMETER  (NLUPDM=4000,NPLBUF=5)
      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
      INTEGER  N,K
      REAL  P,V
      SAVE /GUJETS/
      INTEGER N63,K63
      REAL P63
      COMMON /LUJETS/ N63,K63(2000,2),P63(2000,5)
      SAVE /LUJETS/
C  Stuff for HBOOK:
      INTEGER LIMIT
      PARAMETER( LIMIT=200000 )
      REAL HMEM
      COMMON /PAWC/ HMEM(LIMIT)
      SAVE /PAWC/
C  Stuff for FFREAD:
      INTEGER NAMLEN,NBYTEW,NWONAM
      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
      COMMON /MCFFR/ 
     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,IDSTAB added
     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LTAB,KEVT added
     &     ,JADETUN              ! PMF 10/02/01
     &     ,CHMCTYP,CHEVFILE
      SAVE /MCFFR/
      INTEGER IDIM,NTRAK,I,II,III,LUCHGE,LUCOMP
      REAL PTRAK(IDIM,*)
      INTEGER IERR,IREP,ICALL /0/
      SAVE ICALL
C
C  Edit JETSET event record to leave only "stable" particles:
      IF( INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
         CALL LUEDIT(1)
         ICALL=ICALL+1
         IREP=0
         IF( ICALL.LE.NRTOPR .OR. MOD(ICALL,5000).EQ.0 ) IREP=1
         CALL JTCONV(IREP,IERR)
         IF( IERR.NE.0 ) STOP
      ELSE
         CALL LUEDIT(5)
      ENDIF
C
C  Get hadrons:
      NTRAK= 0
      DO I=1,N
        II= ABS(K(I,2))
        IF( II.GT.6 .AND. II.NE.21 ) THEN

C  N.B.: original JETSET7.4 routine 'LUCOMP' is also used in a JETSET6.3 run 
          CALL HFILL(36,REAL(LUCOMP(K(I,2))),0.0,1.0)

          NTRAK= NTRAK+1
          PTRAK(1,NTRAK)= P(I,1)
          PTRAK(2,NTRAK)= P(I,2)
          PTRAK(3,NTRAK)= P(I,3)
          PTRAK(4,NTRAK)= P(I,4)
          PTRAK(5,NTRAK)= SQRT(P(I,1)**2+P(I,2)**2+P(I,3)**2)
          IF( IDIM.GE.6 ) THEN
             IF(  INDEX(CHMCTYP,'LUND63').GT.0 ) THEN
                PTRAK(6,NTRAK)= REAL(LUCHGE(K63(I,2)))                
             ELSE
                PTRAK(6,NTRAK)= REAL(LUCHGE(K(I,2)))
             ENDIF
          ENDIF
          III= 35
          IF( II.EQ.22 )  III= 21
          IF( II.EQ.12 .OR. II.EQ.14 .OR. II.EQ.16) III= 22
          IF( II.EQ.11 )  III= 23
          IF( II.EQ.13 )  III= 24
          IF( II.EQ.211 ) III= 25
          IF( II.EQ.111 ) III= 26
          IF( II.EQ.321 ) III= 27
          IF( II.EQ.130 ) III= 28
          IF( II.EQ.2212 ) III= 29
          IF( II.EQ.2112 ) III= 30
          IF( II.EQ.15 )  III= 31
          IF( II.EQ.310) III= 32
          IF( II.EQ.311) III= 33
          IF( II.EQ.3122 .OR. II.EQ.3222 .OR. II.EQ.3212 .OR.
     &        II.EQ.3112 .OR. II.EQ.3312 .OR. II.EQ.3322 .OR.
     &        II.EQ.3332 ) III= 34
          IF( PTRAK(5,NTRAK).GT.0 )
     &      CALL HFILL(III,LOG10(PTRAK(5,NTRAK)),0.0,1.0)
          IF( PTRAK(5,NTRAK).LE.0 )
     &      PRINT *,II,' mom =',PTRAK(5,NTRAK)
C          IF( III.EQ.35 ) PRINT*, 'MCGETH: Mysterious particle code:',II
        ENDIF
      ENDDO
C
C  The End:
      RETURN
      END
CDECK  ID>, PXJPD2.
      SUBROUTINE PXJPD2 (IDJT63,IDPDG)
*.*********************************************************
*.
*. PXJPDG version w/o error message
*.
*. 01.06.01 P.A.Movilla Fernandez
*.
*. ------
*. PXJPDG
*. ------
*. SOURCE:  S.L.Lloyd
*. Convert a JETSET63 ID code to a JETSET73 ID Code
*. (A JETSET73 ID code is almost, but not quite the same as
*. a Particle Data Group ID code, see the routine PXJ7PD
*. for the particles which are different)
*. Usage     :
*.
*.      INTEGER  IDJT63,IDPDG
*.      CALL PXJPDG (IDJT63,IDPDG)
*.
*. INPUT     : IDJT63  Particle ID code, JETSET63 format
*. OUTPUT    : IDPDG   Particle ID code, JETSET73 format
*.
*. Modification Log.
*. 06-Jun-89  Adapt to PX environment, add (u,d,s) diquarks J.W.Gary
*.
*.*********************************************************
      IMPLICIT NONE
      INTEGER  ICODE,JSIGN,IDJT63,IDPDG
      INTEGER IPDGJ(500:508),IPDG1(90),IPDG2(9),IPDG3(9),IPDG4(34),
     +        IPDG5(16),IPDG6(36),IPDDQ1(511:513),IPDDQ2(521:523),
     +        IPDDQ3(531:533)
      DATA IPDGJ/  21, 2, 1, 3, 4, 5, 6, 7, 8 /
      DATA IPDG1/  22,  23,  24,  25,   0,   0,  11,  12,  13,  14,
     +             15,  16,  17,  18,   0,   0, 211, 321, 311, 421,
     +            411, 431, 111, 221, 331, 441, 213, 323, 313, 423,
     +            413, 433, 113, 223, 333, 443, 310, 130,   0,   0,
     +           2212,2112,3222,3212,3112,3322,3312,4222,4212,4112,
     +           4322,4312,4332,4422,4412,4432,3122,4122,4232,4132,
     +           2224,2214,2114,1114,3224,3214,3114,3324,3314,3334,
     +           4224,4214,4114,4324,4314,4334,4424,4414,4434,4444,
     +              0,   0, 551, 661,   0,   0, 553, 663,   0,   0/
      DATA IPDG2/-521,-511,-531,-541, 621, 611, 631, 641, 651/
      DATA IPDG3/-523,-513,-533,-543, 623, 613, 633, 643, 653/
      DATA IPDG4/5222,5212,5112,5322,5312,5332,5422,5412,5432,5442,
     +           5522,5512,5532,5542,6222,6212,6112,6322,6312,6332,
     +           6422,6412,6432,6442,6522,6512,6532,6542,6552,6622,
     +           6612,6632,6642,6652/
      DATA IPDG5/5122,5232,5132,5422,5412,5432,6122,6232,6132,6422,
     +           6412,6432,6522,6512,6532,6542/
      DATA IPDG6/5224,5214,5114,5324,5314,5334,5424,5414,5434,5444,
     +           5524,5514,5534,5544,5554,6224,6214,6114,6324,6314,
     +           6334,6424,6414,6434,6444,6524,6514,6534,6544,6554,
     +           6624,6614,6634,6644,6654,6664/
      DATA IPDDQ1/2203,2101,3201/
      DATA IPDDQ2/2103,1103,3101/
      DATA IPDDQ3/3203,3103,3303/

      ICODE = ABS (IDJT63)
      JSIGN = SIGN (1,IDJT63)
      IF (ICODE.GE.1.AND.ICODE.LE.90) THEN
         IDPDG = JSIGN * IPDG1 (ICODE)
      ELSE IF (ICODE.GE.101.AND.ICODE.LE.109) THEN
         IDPDG = JSIGN * IPDG2 (ICODE-100)
      ELSE IF (ICODE.GE.123.AND.ICODE.LE.131) THEN
         IDPDG = JSIGN * IPDG3 (ICODE-122)
      ELSE IF (ICODE.GE.145.AND.ICODE.LE.178) THEN
         IDPDG = JSIGN * IPDG4(ICODE-144)
      ELSE IF (ICODE.GE.241.AND.ICODE.LE.256) THEN
         IDPDG = JSIGN * IPDG5 (ICODE-240)
      ELSE IF (ICODE.GE.293.AND.ICODE.LE.328) THEN
         IDPDG = JSIGN * IPDG6 (ICODE-292)
      ELSE IF (ICODE.GE.500.AND.ICODE.LE.508) THEN
         IDPDG = JSIGN * IPDGJ (ICODE)
      ELSE IF (ICODE.GE.511.AND.ICODE.LE.513) THEN
         IDPDG = JSIGN * IPDDQ1 (ICODE)
      ELSE IF (ICODE.GE.521.AND.ICODE.LE.523) THEN
         IDPDG = JSIGN * IPDDQ2 (ICODE)
      ELSE IF (ICODE.GE.531.AND.ICODE.LE.533) THEN
         IDPDG = JSIGN * IPDDQ3 (ICODE)
      ELSE
c         WRITE (6,FMT='('' PXJPDG: Unknown Jetset particle code '',
c     +     I8)') IDJT63
         IDPDG = 0
      ENDIF

      RETURN
      END
CDECK  ID>, PXEECB.
      SUBROUTINE PXEECB (NTRAK,ITKDM,PTRAK,ENORM,NBINS,
     +           EEC,EECER,EECA,EECAER,CHAR,IERR,ILEV)
*.*********************************************************
*.
*.  ... PXEECB
*.
*.  25/05/2000 Pedro Movilla Fernandez
*.
*.  This version of the PX library subroutine PXEEC4 allow 
*.  a calculation of the energy-energy correlation 
*.  simultaneously at different levels of an event loop job,
*.  e.g. parton and hadron level calculation during a Monte Carlo
*.  generator run.
*.   
*.  The input/output parameters for PXEEC4 are the same as for PXEECB.
*.  In addition PXEECB has a further INTEGER argument 'ILEV' indexing
*.  the current level considered in the calculation. Furthermore, 
*.  the sign of 'ILEV' decides if the calculation has to be done
*.  in theta bins (ILEV>0) or in cost(theta) bins (ILEV<0).
*.
*.  Example: Calculate energy-energy correlation at parton and 
*.           at hadron level during a MC job.
*.
*.      DO IEVT=1,NEVT
*.       ...
*.C      Generate Event
*.       ...
*.C      Set e.g. ILEV=1 to calculate the energy-energy correlation
*.C      from the array PTRAK1 containing 4-vectors of the partons
*.
*.       CALL PXEECB (ntrak,itkdm,PTRAK1,enorm
*.     +          ,nbins,eec,eecer,eeca,eecaer,' ',ierr,1)
*.
*.C      Set e.g. ILEV=2 to calculate the energy-energy correlation
*.C      from the array PTRAK2 containing 4-vectors of the partons
*.
*.       CALL PXEECB (ntrak,itkdm,PTRAK2,enorm
*.     +          ,nbins,eec,eecer,eeca,eecaer,' ',ierr,2)
*.
*.       ...
*.      ENDDO
*.      ...
*.C     End of job:
*.C     Normalize EEC, calculate EECA
*.C     - at parton level (ILEV=1)
*.        CALL PXEEC4 (ntrak,itkdm,ptrak1,enorm,nbins,
*.     +               eec,eecer,eeca,eecaer,'N',ierr,1)
*.C     - at hadron level (ILEV=2)
*.        CALL PXEEC4 (ntrak,itkdm,ptrak2,enorm,nbins,
*.     +               eec,eecer,eeca,eecaer,'N',ierr,2)
*.
*.
*. INPUT: see description of PX subroutine PXEEC4
*. ADDITIONAL INPUT:  ILEV
*.                    ABS(ILEV) Index which indicates the level to be 
*.                              considered in the calculation.
*.                             (currently 1.le.abs(ILEV).le.40)
*.                    ILEV>0    Calculate energy-energy correlation
*.                              in theta bins:
*.                              d EEC(theta)/d theta, theta=0..pi
*.                    ILEV<0    Calculate energy-energy correlation
*.                              in cost(theta) bins:
*.                              d EEC(theta)/d cos(theta)
*.
*.                 theta=pi means back to back region
*.
*.*********************************************************
      IMPLICIT NONE
      INTEGER  NXEECB ,LEV , MLEV, ILEV
      PARAMETER  (NXEECB=500, MLEV=40)
      REAL  PIEEC
      PARAMETER  (PIEEC=3.141593)
      INTEGER  IERR,IECNT,NTRAK,ITKDM,NBINS,IB,IP1,IP2,IEND
      INTEGER  NEEC(MLEV)
      REAL  PTRAK (ITKDM,*),EEC (*),EECER (*),EECA (*),EECAER (*)
      REAL  ENORM,BINMID,COST,THET
      REAL  XBINSZ(MLEV)
      DOUBLE PRECISION  WEIGHT,WEIGH2,DEEC (MLEV,NXEECB),
     +                  DEECER (MLEV,NXEECB),
     +                  DENOM,EECSUM(MLEV),XFACT
      CHARACTER*1 CHAR
      LOGICAL  INIEEC(MLEV)
      SAVE  NEEC,XBINSZ,INIEEC,DEEC,DEECER,EECSUM
      DATA INIEEC /40*.TRUE./
Code:
      IERR = 0
*  perform some input checks
*  ------- ---- ----- ------
      LEV=ABS(ILEV)
      IF(LEV.LT.1.OR.LEV.GT.MLEV) THEN
         WRITE (6,FMT='('' PXEECB: Error, LEV ='',I10,'//
     +        ' '' out of range'')') LEV
         IERR = -1
         GO TO 990
      ENDIF
      IF (CHAR.EQ.'N') GO TO 200
      IF (MOD (NBINS,2).NE.0) THEN
          WRITE (6,FMT='('' PXEECB: Error, NBINS ='',I10,'//
     +     ' '' not an even number'')') NBINS
          IERR = -1
          GO TO 990
      END IF
      IF (NBINS.GT.NXEECB) THEN
          WRITE (6,FMT='('' PXEECB: Error, NBINS ='',I10,'//
     +    '  '' must be smaller than NXEECB ='',I10)') NBINS,NXEECB
          IERR = -1
          GO TO 990
      END IF
      IF (ENORM.EQ.0) THEN
          WRITE (6,FMT='('' PXEECB: Error, ENORM ='',E12.4)') ENORM
          IERR = -1
          GO TO 990
      END IF
*  initialize
*  ----------
      IF (INIEEC(LEV)) THEN 
          INIEEC(LEV) = .FALSE.
          NEEC(LEV) = 0
          IF(ILEV.GT.0) THEN
             XBINSZ(LEV) = (PIEEC / FLOAT (NBINS))
          ELSE
             XBINSZ(LEV) = (2*ABS(COS(PIEEC)) / FLOAT (NBINS))
          ENDIF
          DO 105 IB = 1,NBINS
             DEEC (LEV,IB) = 0D0
             DEECER (LEV,IB) = 0D0
 105      CONTINUE
          EECSUM(LEV) = 0D0
      END IF
      NEEC(LEV) = NEEC(LEV) + 1
*  set up self-correlation or no self-correlation
*  --- -- ---- ----------- -- -- ---- -----------
      IF (CHAR.EQ.'S') THEN
          IEND = NTRAK
          XFACT = 1D0
      ELSE
          XFACT = 2D0
      END IF
*  calculation of EEC
*  ----------- -- ---
      DO 180 IP1=1,NTRAK
          IF (XFACT.GT.1.5) IEND = IP1-1
          DO 160 IP2=1,IEND
              CALL PXANG3 (PTRAK (1,IP1),PTRAK (1,IP2),COST,THET,IERR)
              IF (IERR.EQ.-1) GO TO 990
              IF(ILEV.GT.0) THEN
                 IB = (THET/XBINSZ(LEV)) + 1
              ELSE
                 IB = (COST/XBINSZ(LEV)) + 1 + NBINS/2
              ENDIF
              IF (IB.LT.1.OR.IB.GT.NBINS) THEN
                 IB = NBINS
                 IF (IB.LT.1) IB = 1
              END IF
              WEIGHT = (PTRAK (4,IP2) * PTRAK (4,IP1)) / ENORM**2
              WEIGH2 = WEIGHT * WEIGHT
              DEEC (LEV,IB) = DEEC(LEV,IB) + XFACT * WEIGHT
              DEECER (LEV,IB) = DEECER(LEV,IB) + XFACT * WEIGH2
              EECSUM(LEV) = EECSUM(LEV) + XFACT * WEIGHT
 160      CONTINUE
 180  CONTINUE
      GO TO 990
*  normalization and calculation of asymmetry
*  ------------- --- ----------- -- ---------
 200  CONTINUE
C      WRITE (6,FMT='('' PXEEC4: Normalize EEC, EECA'')')
      IF (NEEC(LEV).EQ.0) THEN
          IERR = -1
          WRITE (6,FMT='('' PXEECB: Error, NEEC ='',I10)') NEEC(LEV)
          GO TO 990
      END IF
      DENOM = FLOAT (NEEC(LEV)) * XBINSZ(LEV)
C      WRITE (6,FMT='(''   EECSUM, EECSUM ='',2E12.4)')
C     +   EECSUM(LEV),EECSUM(LEV)/FLOAT (NEEC(LEV))
      DO 220  IB=1,NBINS
          DEEC (LEV,IB) = DEEC (LEV,IB) / DENOM
          DEECER (LEV,IB) = DSQRT (DEECER (LEV,IB)) / DENOM
          EEC (IB) = DEEC (LEV,IB)
          EECER (IB) = DEECER (LEV,IB)
 220  CONTINUE
      DO 240 IB=1,NBINS/2
          EECA (IB) = DEEC (LEV,NBINS+1-IB) - DEEC (LEV,IB)
          EECAER(IB)= DSQRT(DEECER(LEV,NBINS+1-IB)**2+DEECER(LEV,IB)**2)
 240  CONTINUE
      INIEEC(LEV) = .TRUE.
      NEEC(LEV) = 0
 
 990  CONTINUE
C
      RETURN
      END
