CDECK  ID>, MCJ623.
      PROGRAM MCJ623
******************************************************************
*
*     ADAPTED THE COJET 6.23 PACKAGE FOR THE MC101 JOB
*     STAN BENTVELSEN, 26-11-1996
*
******************************************************************
C     **************
C
C-- Unit number 1 for file COJTABLE (ASMD or ASMD2 from YPATCHY)
C-- Unit number 2 fake file, used for output in /HEPEVT/:  + all particles,
C--               - stable particles only, unit number ignored
C-- Unit number 5 for input cards (defaults to input stream is unassigned)
C-- Unit number 6 for output list (defaults to output stream is unassigned)
C

      OPEN (UNIT=55,FILE='cj623tab.dat',STATUS='OLD')
      OPEN (UNIT=56,FILE='cj623inp.dat',STATUS='OLD')
*
*  event generation
*  ----- ----------
C      CALL COJETS(-55,2,-56,6)
      CALL COJETS(-55,2,5,6)
 
      STOP
      END
CDECK  ID>, CJUSER.
      SUBROUTINE CJUSER (ACCEPT)
C     *************************
C...Shell of user's routine called:
C...  i) Before starting generation of events, with NEVHEP=-1
C... ii) After finishing generation of events, with NEVHEP=-2
C...iii) After each event generation, with NEVHEP > 0 ; if ACCEPT = .FALSE.
C...     is returned, the event is counted as rejected (the next event
C...     will have the same NEVHEP)
C
      INTEGER  ITKDM,MXTRK,MXJET
      PARAMETER  (ITKDM=20,MXTRK=200,MXJET=10)
      INTEGER NMXHEP
      PARAMETER (NMXHEP=2000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      INTEGER  NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      DOUBLE PRECISION PHEP,VHEP
      SAVE /HEPEVT/
      COMMON/CJCONT/
     & ECM   ,PTMIN ,PTMAX ,BOMSMN,BOMSMX
     &,ALAMBD,CUTJET,PTCUT ,PT2INT,QZEVOL
     &,SIGINL,FACTK ,TOPMAS,ZWTHRS,GMCZW
     &,EMCUT ,BRATIO,TLEAVE
     &,UNITS
     &,IEVTYP
     &,KPBAR ,IFUNCX,NFLAV ,KPRHEV,KFRFLA
     &,KPTWGT
     &,KYEVOL,KYTQCD,KYWQED,KYTQED,KEEQED
     &,KREPET,NREPET
     &,NOFRGM,NODECY,NOUNST,NOBROA,NOPDG
     &,NLEAVE,NSIGMA,NPRESH,NDUMP,NJUMP
     &,NEVGEN,NHARD
      REAL
     & ECM   ,PTMIN ,PTMAX ,BOMSMN,BOMSMX
     &,ALAMBD,CUTJET,PTCUT ,PT2INT,QZEVOL
     &,SIGINL,FACTK ,TOPMAS,ZWTHRS,GMCZW
     &,EMCUT ,BRATIO,TLEAVE
     &,UNITS
      REAL QZ
      EQUIVALENCE (CUTJET,QZ)
      COMMON/GUDATR/MRLU(6),RRLU(100)
      INTEGER  MRLU
      REAL  RRLU
      SAVE /GUDATR/
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
      COMMON/GUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /GUDAT2/
**JWG  add psi'(3685) as KC code 80, needed for Eurodec interface
*      --- ---------- -- -- ---- --- ------ --- ------- ---------
      INTEGER  IDPSIP
      PARAMETER  (IDPSIP=80)
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
      INTEGER  LWRIT,LRAN,LRAN2
      PARAMETER  (LWRIT=33,LRAN=34,LRAN2=35)
      INTEGER  ITYPE, NTOPR,NREVT
      PARAMETER  (NTOPR=2,NREVT=1500)
      INTEGER  IHIST (NMXHEP),IRRLU (25)
      INTEGER  IID,ICHG,NPART,IERR,IENUM,IZRN,IFLAV,IEVX

      SAVE  ENDWR,IEVX,IENUM,ICNT,VTX,IZRN,FST,NWRITE

      EXTERNAL LUDATA

      IF (NEVHEP.EQ.-1) THEN
C
C  Initialisation of the analysis routine:
        CALL MCINIT
        CALL MCUSIN
        CHMCTYP= 'COJET623'
*     
*     initialization
*     --------------
         IF(L4VEC) THEN
           CALL MCWRIT('I',IERR)
         ENDIF
C     
C     INITIALISE NUMBERS
C
         IEVX = 0

         GO TO 990
      ELSE IF (NEVHEP.EQ.-2) THEN
C     
C     Run termination:
        IF(L4VEC) THEN
          CALL MCWRIT('F',IERR)
        ENDIF
        CALL MCUSFI
        GO TO 990
      END IF
C     
C     FILL THE JETSET COMMON BLOCKS
C
      CALL LUHEPC(2)
C     
C     PRINT THE FIRST FEW EVENTS
C
      IF(IEVX.LE.5) THEN
        WRITE(*,*) 'PUT IN JETSET COMMON FORMAT'
        CALL LULIST(2)
      ENDIF
C     
C     THROW AWAY THE NON-HADRONIC DECAYS
C
      IF(ABS(K(1,2)).LE.6) THEN

        IF (IEVX.LE.50.OR.MOD (IEVX,500).EQ.1)
     +    WRITE (6,FMT='('' NEVHEP,IEVX,IENUM ='',4I6)')
     +    NEVHEP,IEVX,IENUM
        
        IENUM  = IENUM + 1
        IEVX   = IEVX + 1
C     
C     Write 4-vectors if demanded:
        IF( L4VEC ) CALL MCWRIT('W',IERR)
C     
C     User event analysis:
C     
        CALL MCUSEV
      ELSE
        IF( ICNT.LE.50 )
     &    PRINT*,'MCJ623: Not a qqbar, regenerate'
      ENDIF



 990  RETURN
      END
CDECK  ID>, MCEVBL.
      BLOCK DATA MCEVBL
C
C PMF 12/05/00: Introduce this BLOCK DATA subroutine to
C               initialize COMMON /MCEVSH/
C     25/07/00  Initialize with JADE-like binning
C     Stuff for event shape analysis:
C     nbmax: number of bins for the histograms
C     ndimm: dimension of the XBINS array
C
      INTEGER NVAR,NBMAX,NDIMM
      PARAMETER( NBMAX=100, NDIMM=50, NVAR=6 )
      INTEGER NBMAX2
      PARAMETER( NBMAX2=1000 )  ! PMF: needed for finer binning of LINEAR y23 (D) distribution
C EEC special stuff
      INTEGER NBEEC
      REAL PIEEC
      PARAMETER( NBEEC=100, PIEEC=3.1415927 )
      REAL EEC(NBEEC),EECER(NBEEC),EECA(NBEEC/2),EECAER(NBEEC/2)
C
      INTEGER NBIN(NVAR)
      REAL XBINS(NDIMM,NVAR)
      REAL TH,MH,BT,BW,CP,Y23
      REAL TMA, TMI, OBL, SP, AC
      CHARACTER*2 CHVAR(NVAR)
      COMMON /MCEVSH/ NBIN
     &               ,TH,MH,BT,BW,CP,Y23,XBINS
     &               ,CHVAR
      SAVE /MCEVSH/
**      DATA NBIN / 15, 14, 15, 14, 12, 13 / ! 14 GeV
      DATA NBIN / 15, 14, 15, 14, 14, 14 /
      DATA XBINS/
     >     0.00, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18,
     >     0.20, 0.23, 0.27, 0.32, 0.40, 0.50,         34*0.0, ! 1-T 
     >     0.00, 0.06, 0.10, 0.14, 0.18, 0.22, 0.26, 0.30, 0.34, 0.38,
     >     0.42, 0.46, 0.50, 0.55, 0.60,                     35*0.0, ! MH
     >     0.00, 0.03, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.20,
     >     0.22, 0.24, 0.27, 0.30, 0.34, 0.38,               34*0.0, ! BT
     >     0.00, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18,
     >     0.20, 0.23, 0.26, 0.30, 0.35,                     35*0.0, ! BW
**     >     0.00, 0.16, 0.28, 0.34, 0.40,
**     >     0.46, 0.52, 0.58, 0.64, 0.72, 0.80, 0.88, 1.00,    37*0.0, ! CP 14 GeV
     >     0.00, 0.10, 0.16, 0.22, 0.28, 0.34, 0.40,
     >     0.46, 0.52, 0.58, 0.64, 0.72, 0.80, 0.88, 1.00,    35*0.0, ! CP
**     >     0.00, .002, .004, .006, 0.01, 0.014, 0.02, 0.03, 0.04,
**     >     0.06, 0.10, 0.14, 0.2, 0.3,          36*0.0 /    ! D2 14 GeV
     >     0.00, .001, .002, .004, .006, 0.01, 0.014, 0.02, 0.03, 0.04,
     >     0.06, 0.10, 0.14, 0.2, 0.3,          35*0.0 /    ! D2
      DATA CHVAR / 'TH', 'MH', 'BT', 'BW', 'CP', 'D2' /
      END
CDECK  ID>, MCPOBL.
      BLOCK DATA MCPOBL
C
C PMF 12/05/00: Introduce this BLOCK DATA subroutine to
C               initialize COMMON /NPOWER/
      INTEGER NPOW
      PARAMETER (NPOW=12)
      CHARACTER*3 CHSHAP(NPOW)
      DOUBLE PRECISION DSHAP1(NPOW),DSHAP2(NPOW),DSHAP3(NPOW)
      INTEGER  NSHAP
C PMF 12.05.00: Change order of variables in order to avoid misalignement
C      COMMON / NPOWER / CHSHAP,DSHAP1,DSHAP2,DSHAP3,NSHAP
      COMMON / NPOWER / DSHAP1,DSHAP2,DSHAP3,NSHAP,CHSHAP
C
C..   KEEP RUNNING SUM OF THE EVENT-SHAPES Y, FOR Y, Y**2, Y**3
C..   WHERE Y IS DEFINED IN MCUSIN DATA STATEMENT
C
      DATA CHSHAP /'TH ','TMA','MH ','TMI','BT ','OBL'
     +     ,'BW ','SP ','CP ','AC ','Y23','1TH' /
      END
CDECK  ID>, MCUSFF.
      SUBROUTINE MCUSFF
      IMPLICIT NONE
C  Routine for user FFREAD cards:
C  Stefan Kluth, 16.07.96
C  Modifications:
C  17.07.96, STK: Replace VZERO by VBLANK for hollerith array
C  18.09.96, STK: Introduce switch for ISR removal, change default for FSRRM
C  13.02.97, SB : Introduce the 'JETRATE' card, calculation jet rates with cone
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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
      LFSRRM= .FALSE.
      CALL FFKEY('FSRRM',LFSRRM,1,'LOGI')
      LISRRM= .FALSE.
      CALL FFKEY('ISRRM',LISRRM,1,'LOGI')
      LBOOS= .FALSE.
      CALL FFKEY('MCBOOS',LBOOS,1,'LOGI') ! PMF 13/05/00: BOOST->MCBOOS
      LCONE= .FALSE.
      CALL FFKEY('MCCONE',LCONE,1,'LOGI')
      LDURH= .FALSE.
      CALL FFKEY('MCDURH',LDURH,1,'LOGI')
      LJDE0= .FALSE.
      CALL FFKEY('MCJDE0',LJDE0,1,'LOGI')
      NJTEV= -1
      CALL FFKEY('MCJTEV',NJTEV,1,'INTEGER')
      LCAMJ = .FALSE.
      CALL FFKEY('MCCAMJ',LCAMJ,1,'LOGI')
      LCKRN = .FALSE.
      CALL FFKEY('MCCKRN',LCKRN,1,'LOGI')
      CALL VBLANK(HOUTFILE,NWONAM)
      CALL FFKEY('OUTFILE',HOUTFILE,NWONAM,'MIXED')
C
      JADETUN=0
      CALL FFKEY('JADETUNE',JADETUN,1,'INTE')
C
C  The End:
      RETURN
      END
CDECK  ID>, MCUSIN.
      SUBROUTINE MCUSIN
      IMPLICIT NONE
C  Routine for user initialisation:
C  Stefan Kluth, 22.1.96
C  Modifications:
C  19.09.96, STK: Put bin edges into DATA statements
C  13.02.97, SB : Put in jetrates histograms
C  13.02.97, SB : Extend the jet-resolution to 10^-6
C  26.05.00. PMF: - Add energy-energy correlation
C                   in theta and cos(theta) bins
C                 - Add pTin, pTout, xp, yp, xi 
C                   for charged and neutral particles
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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
C  Stuff for HBOOK:
      INTEGER LIMIT
      PARAMETER( LIMIT=200000 )
      REAL HMEM
      COMMON /PAWC/ HMEM(LIMIT)
      SAVE /PAWC/
C     Stuff for event shape analysis:
C     nbmax: number of bins for the histograms
C     ndimm: dimension of the XBINS array
C
      INTEGER NVAR,NBMAX,NDIMM
      PARAMETER( NBMAX=100, NDIMM=50, NVAR=6 )
      INTEGER NBMAX2
      PARAMETER( NBMAX2=1000 )  ! PMF: needed for finer binning of LINEAR y23 (D) distribution
C EEC special stuff
      INTEGER NBEEC
      REAL PIEEC
      PARAMETER( NBEEC=100, PIEEC=3.1415927 )
      REAL EEC(NBEEC),EECER(NBEEC),EECA(NBEEC/2),EECAER(NBEEC/2)
C
      INTEGER NBIN(NVAR)
      REAL XBINS(NDIMM,NVAR)
      REAL TH,MH,BT,BW,CP,Y23
      REAL TMA, TMI, OBL, SP, AC
      CHARACTER*2 CHVAR(NVAR)
      COMMON /MCEVSH/ NBIN
     &               ,TH,MH,BT,BW,CP,Y23,XBINS
     &               ,CHVAR
      SAVE /MCEVSH/
      INTEGER NPOW
      PARAMETER (NPOW=12)
      CHARACTER*3 CHSHAP(NPOW)
      DOUBLE PRECISION DSHAP1(NPOW),DSHAP2(NPOW),DSHAP3(NPOW)
      INTEGER  NSHAP
C PMF 12.05.00: Change order of variables in order to avoid misalignement
C      COMMON / NPOWER / CHSHAP,DSHAP1,DSHAP2,DSHAP3,NSHAP
      COMMON / NPOWER / DSHAP1,DSHAP2,DSHAP3,NSHAP,CHSHAP
C
C..   KEEP RUNNING SUM OF THE EVENT-SHAPES Y, FOR Y, Y**2, Y**3
C..   WHERE Y IS DEFINED IN MCUSIN DATA STATEMENT
C
      INTEGER I,IVAR,NBE,IOFF,NBMM
      REAL XLO,XHI,XD2(NBMAX+1)
      CHARACTER*6 CNA(7)
      CHARACTER*14 PH(2)
C
C PMF 12/05/00: Initialization of /MCEVSH/ is now done
C               in the BLOCK DATA subroutine 'MCEVBL'
C      DATA NBIN / 13, 12, 14, 13, 12, 15 /
C      DATA XBINS / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12
C     &            ,0.15, 0.22, 0.30, 0.40, 0.50, 36*0.0
C     &            ,0.00, 0.06 ,0.075,0.09, 0.11, 0.14, 0.17, 0.20, 0.25
C     &            ,0.30, 0.35, 0.45, 0.60, 37*0.0
C     &            ,0.00, 0.03, 0.04, 0.05, 0.06, 0.075,0.09, 0.11, 0.13
C     &            ,0.16, 0.20, 0.25, 0.30, 0.35, 0.40, 35*0.0
C     &            ,0.00, 0.02, 0.03, 0.04, 0.05, 0.065,0.08, 0.10, 0.15
C     &            ,0.20, 0.25, 0.30, 0.35, 0.40, 36*0.0
C     &            ,0.00, 0.05, 0.08, 0.11, 0.14, 0.18, 0.22, 0.30, 0.4
C     &            ,0.50, 0.60, 0.75, 1.00, 37*0.0
C     &            ,0.00, 0.3E-03, 0.75E-03, 0.13E-02, 0.23E-02, 0.4E-02
C     &            ,0.7E-02, 0.012, 0.0225, 0.04, 0.07, 0.13, 0.235
C     &  ,0.40, 0.70, 1.00, 34*0.0 /
C
C      DATA CHVAR / 'TH', 'MH', 'BT', 'BW', 'CP', 'D2' /
C

      DATA CNA /' d',' u',' s',' c',' b','  ','(cum.)'/
      SAVE CNA

C PMF 12/05/00: Initialization of CHSHAP is now done 
C               in the BLOCK DATA subroutine 'MCPOBL'
C      DATA CHSHAP /'TH ','TMA','MH ','TMI','BT ','OBL'
C     +     ,'BW ','SP ','CP ','AC ','Y23','1TH' /

      NSHAP = 0
      DO I=1,NPOW
         DSHAP1(I) = 0D0
         DSHAP2(I) = 0D0
         DSHAP3(I) = 0D0
      ENDDO
C
C  Logarithmic bins for D2 distribution:
      XD2(1)= 0.0
      XD2(2)= LOG10(1.0E-6)
      XD2(NBMAX+1)= LOG10(1.0)
      DO I=1,NBMAX-1
        XD2(I+2)= XD2(2)+(XD2(NBMAX+1)-XD2(2))/REAL(NBMAX-1)*REAL(I)
      ENDDO
      DO I=2,NBMAX+1
        XD2(I)= 10.0**XD2(I)
      ENDDO
C
C  Initialise HBOOK and book histos:
      CALL HLIMIT(LIMIT)
      CALL HCDIR('//PAWC',' ')
      CALL HSTAF('YES')
      CALL HBOOK1(10,'Evis partons incl. FSR',195,5.,200.,0.)
      CALL HBOOK1(11,'Evis partons excl. FSR',195,5.,200.,0.)
      CALL HBOOK1(12,'Evis hadrons',195,5.,200.,0.)
      CALL HBOOK1(13,'Flavour',10,-0.5,9.5,0.)
      CALL HBOOK1(21,'log10(p) photons',100,-3.,2.,0.)
      CALL HBOOK1(22,'log10(p) neutrinos',100,-3.,2.,0.)
      CALL HBOOK1(23,'log10(p) electrons',100,-3.,2.,0.)
      CALL HBOOK1(24,'log10(p) muons',100,-3.,2.,0.)
      CALL HBOOK1(25,'log10(p) pi+-',100,-3.,2.,0.)
      CALL HBOOK1(26,'log10(p) pi0',100,-3.,2.,0.)
      CALL HBOOK1(27,'log10(p) K+-',100,-3.,2.,0.)
      CALL HBOOK1(28,'log10(p) K0L (130)',100,-3.,2.,0.)
      CALL HBOOK1(29,'log10(p) protons',100,-3.,2.,0.)
      CALL HBOOK1(30,'log10(p) neutrons',100,-3.,2.,0.)
      CALL HBOOK1(31,'log10(p) tau+-  ',100,-3.,2.,0.)
      CALL HBOOK1(32,'log10(p) K0S (310)',100,-3.,2.,0.)
      CALL HBOOK1(33,'log10(p) K0 (311)',100,-3.,2.,0.)
      CALL HBOOK1(34,'log10(p) Hyperon',100,-3.,2.,0.)
      CALL HBOOK1(35,'log10(p) Unknown',100,-3.,2.,0.)
      CALL HBOOK1(36,'KC particle codes',501,-0.5,500.5,0.0)
      CALL HBOOK1(37,'c*tau of stable hadrons',100,0.0,20.0,0.0)

      CALL HBOOK1(50,'PTin  partons (charged)',100,0.0,10.0,0.0)
      CALL HBOOK1(60,'PTin  hadrons (charged)',100,0.0,10.0,0.0)
      CALL HBOOK1(51,'PTout partons (charged)',100,0.0,5.0,0.0)
      CALL HBOOK1(61,'PTout hadrons (charged)',100,0.0,5.0,0.0)
      CALL HBOOK1(52,'xp    partons (charged)',100,0.0,1.0,0.0)
      CALL HBOOK1(62,'xp    hadrons (charged)',100,0.0,1.0,0.0)
      CALL HBOOK1(53,'yp    partons (charged)',100,0.0,8.0,0.0)
      CALL HBOOK1(63,'yp    hadrons (charged)',100,0.0,8.0,0.0)
      CALL HBOOK1(54,'xi    partons (charged)',100,0.0,10.0,0.0)
      CALL HBOOK1(64,'xi    hadrons (charged)',100,0.0,10.0,0.0)

      CALL HBOOK1(55,'PTin  partons (all)',100,0.0,10.0,0.0)
      CALL HBOOK1(65,'PTin  hadrons (all)',100,0.0,10.0,0.0)
      CALL HBOOK1(56,'PTout partons (all)',100,0.0,5.0,0.0)
      CALL HBOOK1(66,'PTout hadrons (all)',100,0.0,5.0,0.0)
      CALL HBOOK1(57,'xp    partons (all)',100,0.0,1.0,0.0)
      CALL HBOOK1(67,'xp    hadrons (all)',100,0.0,1.0,0.0)
      CALL HBOOK1(58,'yp    partons (all)',100,0.0,8.0,0.0)
      CALL HBOOK1(68,'yp    hadrons (all)',100,0.0,8.0,0.0)
      CALL HBOOK1(59,'xi    partons (all)',100,0.0,10.0,0.0)
      CALL HBOOK1(69,'xi    hadrons (all)',100,0.0,10.0,0.0)
      DO I=1,6
        CALL HBOOK1(100+I,'N partons'//CNA(I),41,-0.5,40.5,0.)
        CALL HBOOK1(200+I,'N hadrons'//CNA(I),151,-0.5,150.5,0.)
        CALL HBOOK1(210+I,'N ch. hadrons'//CNA(I),101,-0.5,100.5,0.)
      ENDDO
      CALL HBOOK1(218,'N CH HADRONS',50,1.0,101.,0.)

*     
*     book nbmax bins
*
      Do I=1,7
C Energy-energy correlation in theta bins
        CALL HBOOK1(I+220,'EEC (theta bins) parton level'//CNA(I)
     +       ,NBEEC,0.,PIEEC,0.)
        CALL HBOOK1(I+230,'EEC (theta bins) hadron level'//CNA(I)
     +       ,NBEEC,0.,PIEEC,0.)
        CALL HBOOK1(I+240,'AEEC (theta bins) parton level'//CNA(I)
     +       ,NBEEC/2,0.,PIEEC/2.,0.)
        CALL HBOOK1(I+250,'AEEC (theta bins) hadron level'//CNA(I)
     +       ,NBEEC/2,0.,PIEEC/2.,0.)
C Energy-energy correlation in cos(theta) bins
        CALL HBOOK1(I+260,'EEC (cos(theta) bins) parton level'//CNA(I)
     +       ,NBEEC,-1.0,1.0,0.)
        CALL HBOOK1(I+270,'EEC (cos(theta) bins) hadron level'//CNA(I)
     +       ,NBEEC,-1.0,1.0,0.)
        CALL HBOOK1(I+280,'AEEC (cos(theta) bins) parton level'//CNA(I)
     +       ,NBEEC/2,-1.,.0,0.)
        CALL HBOOK1(I+290,'AEEC (cos(theta) bins) hadron level'//CNA(I)
     +       ,NBEEC/2,-1.,.0,0.)
C
C                                                                   PMF: - some histogram ranges changed!
C                                                                        - NBMAX2 bins for linear y23 (D)
C                                                                           id = 900 ..
        CALL HBOOK1(I+300,'1-T parton level'//CNA(I),NBMAX,0.0,0.5,0.) ! 1-T:
        CALL HBOOK1(I+310,'1-T hadron level'//CNA(I),NBMAX,0.0,0.5,0.)
        CALL HBOOK1(I+320,'cosT parton level'//CNA(I),NBMAX,0.0,1.0,0.) ! PMF 01/11/00: added
        CALL HBOOK1(I+330,'cosT hadron level'//CNA(I),NBMAX,0.0,1.0,0.) ! PMF 01/11/00: added
        CALL HBOOK1(I+350,'TMA parton level'//CNA(I),NBMAX,0.0,1.0,0.) ! TMA: 0.0 ... 0.6
        CALL HBOOK1(I+360,'TMA hadron level'//CNA(I),NBMAX,0.0,1.0,0.) !      0.0 ... 0.6
        CALL HBOOK1(I+400,'MH  parton level'//CNA(I),NBMAX,0.0,1.0,0.) ! MHT: 0.0 ... 0.6
        CALL HBOOK1(I+410,'MH  hadron level'//CNA(I),NBMAX,0.0,1.0,0.) !      0.0 ... 0.6
        CALL HBOOK1(I+420,'MH2 parton level'//CNA(I),NBMAX,0.0,1.0,0.) ! PMF 10/03/01: added
        CALL HBOOK1(I+430,'MH2 hadron level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+450,'TMI parton level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+460,'TMI hadron level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+500,'BT  parton level'//CNA(I),NBMAX,0.0,0.5,0.) ! BT: 0.0 ... 0.4
        CALL HBOOK1(I+510,'BT  hadron level'//CNA(I),NBMAX,0.0,0.5,0.) !     0.0 ... 0.4
        CALL HBOOK1(I+550,'OBL parton level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+560,'OBL hadron level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+600,'BW  parton level'//CNA(I),NBMAX,0.0,0.5,0.) ! BW: 0.0 ... 0.4
        CALL HBOOK1(I+610,'BW  hadron level'//CNA(I),NBMAX,0.0,0.5,0.) !     0.0 ... 0.4
        CALL HBOOK1(I+650,'SP  parton level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+660,'SP  hadron level'//CNA(I),NBMAX,0.0,1.0,0.)
        CALL HBOOK1(I+700,'CP  parton level'//CNA(I),NBMAX,0.0,1.0,0.) ! CP: 
        CALL HBOOK1(I+710,'CP  hadron level'//CNA(I),NBMAX,0.0,1.0,0.) !
        CALL HBOOK1(I+750,'AC  parton level'//CNA(I),NBMAX,0.0,0.5,0.) ! AC: 0.0 ... 0.3
        CALL HBOOK1(I+760,'AC  hadron level'//CNA(I),NBMAX,0.0,0.5,0.) !     0.0 ... 0.3
        CALL HBOOK1(I+850,'T   parton level'//CNA(I),NBMAX,0.5,1.0,0.)
        CALL HBOOK1(I+860,'T   hadron level'//CNA(I),NBMAX,0.5,1.0,0.)
        CALL HBOOK1(I+900,'y23 (D) parton level'//CNA(I),NBMAX2,0.0,    ! D2:(lin.) ! PMF: NBMAX->NBMAX2
     +    1.0,0.)
        CALL HBOOK1(I+910,'y23 (D) hadron level'//CNA(I),NBMAX2,0.0,    ! ! PMF: NBMAX->NBMAX2
     +    1.0,0.)
        CALL HBOOK1(I+920,'mean jet rate (D) parton level'//CNA(I),     
     +       NBMAX2,0.0,1.0,0.) ! PMF: NBMAX->NBMAX2
        CALL HBOOK1(I+930,'mean jet rate (D) hadron level'//CNA(I),
     +       NBMAX2,0.0, 1.0,0.)! PMF: NBMAX->NBMAX2
        CALL HBOOKB(I+800,'y23 (D) parton level'//CNA(I),NBMAX,XD2,0.0)  ! D2:(log.)
        CALL HBOOKB(I+820,'mean jet rate (D) parton level'//CNA(I),
     +       NBMAX,XD2,0.0)
        CALL HIDOPT(I+800,'STAT')
        CALL HIDOPT(I+820,'STAT')
        CALL HBOOKB(I+810,'y23 (D) hadron level'//CNA(I),NBMAX,XD2,0.0) ! D2:(log.)
        CALL HBOOKB(I+830,'mean jet rate (D) hadron level'//CNA(I),
     +       NBMAX,XD2,0.0)
        CALL HIDOPT(I+810,'STAT')
        CALL HIDOPT(I+830,'STAT')
      ENDDO
      DO IVAR=1,NVAR
        CALL HBOOKB(1000+(IVAR-1)*10+1,CHVAR(IVAR)//' parton level'
     &             ,NBIN(IVAR),XBINS(1,IVAR),0.0)
        CALL HIDOPT(1000+(IVAR-1)*10+1,'STAT')
        CALL HBOOKB(1000+(IVAR-1)*10+2,CHVAR(IVAR)//' hadron level'
     &             ,NBIN(IVAR),XBINS(1,IVAR),0.0)
        CALL HIDOPT(1000+(IVAR-1)*10+2,'STAT')
        DO I=1,6                ! PMF 01/11/00: Introduce flavour dependent hadr. matrices
        CALL HBOOK2(1100+(IVAR-1)*10+I
     &             ,CHVAR(IVAR)//' hadronisation matrix'//CNA(I)
     &             ,NBIN(IVAR),0.5,REAL(NBIN(IVAR))+0.5
     &             ,NBIN(IVAR),0.5,REAL(NBIN(IVAR))+0.5,0.)
        ENDDO
      ENDDO
C
C..   JETRATES HISTOGRAMS
C
      PH(1) =' PARTON LEVEL'
      PH(2) =' HADRON LEVEL'
      DO IOFF=0,1
         IF(LJDE0) THEN
C
C     E0 FINDING, LINEAR SCALE
C 
          CALL HBOOK1(1201+10*IOFF,'R1 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
          CALL HBOOK1(1202+10*IOFF,'R2 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
          CALL HBOOK1(1203+10*IOFF,'R3 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
          CALL HBOOK1(1204+10*IOFF,'R4 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
          CALL HBOOK1(1205+10*IOFF,'R5 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
          CALL HBOOK1(1206+10*IOFF,'R6 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
          CALL HBOOK1(1208+10*IOFF,'MN E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)

         CALL HBOOK1(1221+10*IOFF,'Y12 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
         CALL HBOOK1(1222+10*IOFF,'Y23 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
         CALL HBOOK1(1223+10*IOFF,'Y34 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
         CALL HBOOK1(1224+10*IOFF,'Y45 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
         CALL HBOOK1(1225+10*IOFF,'Y56 E0'//PH(IOFF+1),NBMAX,0.,0.15,0.)
         ENDIF
         IF(LDURH) THEN
C
C     DURHAM FINDING, LOG SCALE
C 
           NBMM = 48
           CALL HBOOK1(1301+10*IOFF,'R1 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1302+10*IOFF,'R2 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1303+10*IOFF,'R3 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1304+10*IOFF,'R4 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1305+10*IOFF,'R5 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1306+10*IOFF,'R6 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1308+10*IOFF,'MN KT'//PH(IOFF+1),NBMM,-6.,0.,0.)

           CALL HBOOK1(1321+10*IOFF,'Y12 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1322+10*IOFF,'Y23 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1323+10*IOFF,'Y34 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1324+10*IOFF,'Y45 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1325+10*IOFF,'Y56 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
         ENDIF
         IF(LCAMJ) THEN
C
C     CAMBRIDGE FINDING, LOG SCALE
C 
            NBMM = 24
            CALL HBOOK1(1401+10*IOFF,'R1 CM'//PH(IOFF+1),NBMM,-6.,0.,0.)
            CALL HBOOK1(1402+10*IOFF,'R2 CM'//PH(IOFF+1),NBMM,-6.,0.,0.)
            CALL HBOOK1(1403+10*IOFF,'R3 CM'//PH(IOFF+1),NBMM,-6.,0.,0.)
            CALL HBOOK1(1404+10*IOFF,'R4 CM'//PH(IOFF+1),NBMM,-6.,0.,0.)
            CALL HBOOK1(1405+10*IOFF,'R5 CM'//PH(IOFF+1),NBMM,-6.,0.,0.)
            CALL HBOOK1(1406+10*IOFF,'R6 CM'//PH(IOFF+1),NBMM,-6.,0.,0.)
            CALL HBOOK1(1408+10*IOFF,'MN CM'//PH(IOFF+1),NBMM,-6.,0.,0.)

           CALL HBOOK1(1421+10*IOFF,'Y12 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1422+10*IOFF,'Y23 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1423+10*IOFF,'Y34 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1424+10*IOFF,'Y45 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
           CALL HBOOK1(1425+10*IOFF,'Y56 KT'//PH(IOFF+1),NBMM,-6.,0.,0.)
         ENDIF

         IF(LCONE) THEN
           CALL HBOOK1(1501+10*IOFF,'R1 CR'//PH(IOFF+1),13,0.25,1.55,0.)
           CALL HBOOK1(1502+10*IOFF,'R2 CR'//PH(IOFF+1),13,0.25,1.55,0.)
           CALL HBOOK1(1503+10*IOFF,'R3 CR'//PH(IOFF+1),13,0.25,1.55,0.)
           CALL HBOOK1(1504+10*IOFF,'R4 CR'//PH(IOFF+1),13,0.25,1.55,0.)
           CALL HBOOK1(1505+10*IOFF,'R5 CR'//PH(IOFF+1),13,0.25,1.55,0.)
           CALL HBOOK1(1506+10*IOFF,'R6 CR'//PH(IOFF+1),13,0.25,1.55,0.)
            
            CALL HBOOK1(1521+10*IOFF,'R1 CE'//PH(IOFF+1),13,1.,27.,0.)
            CALL HBOOK1(1522+10*IOFF,'R2 CE'//PH(IOFF+1),13,1.,27.,0.)
            CALL HBOOK1(1523+10*IOFF,'R3 CE'//PH(IOFF+1),13,1.,27.,0.)
            CALL HBOOK1(1524+10*IOFF,'R4 CE'//PH(IOFF+1),13,1.,27.,0.)
            CALL HBOOK1(1525+10*IOFF,'R5 CE'//PH(IOFF+1),13,1.,27.,0.)
            CALL HBOOK1(1526+10*IOFF,'R6 CE'//PH(IOFF+1),13,1.,27.,0.)
         ENDIF
      ENDDO
C     
C..   POWERS OF EVENT SHAPES
C     
      CALL HBOOK1(191,'MEAN',12,0.5,12.5,0.)
      CALL HBOOK1(192,'WIDT',12,0.5,12.5,0.)
      CALL HBOOK1(193,'SKEW',12,0.5,12.5,0.)
      CALL HBOOK1(194,'Y   ',12,0.5,12.5,0.)
      CALL HBOOK1(195,'Y**2',12,0.5,12.5,0.)
      CALL HBOOK1(196,'Y**3',12,0.5,12.5,0.)
C
C   Tab file with event shapes
C
      IF( LTAB )
     >OPEN(UNIT=71,FILE='shapes.tab',STATUS='UNKNOWN',FORM='FORMATTED')

C
C  The End:
      RETURN
      END
CDECK  ID>, MCUSEV.
      SUBROUTINE MCUSEV
      IMPLICIT NONE
C  Routine gets stable partons and hadrons from JETSET event record,
C  packs momenta into arrays and calls analysis routine.
C  Created: 03.07.96
C  Author: Stefan Kluth (based on code from David Ward)
C  Modifications:
C  16.07.96, STK: New parton selection algorithm,
C                 mods for user FFREAD cards
C  25.07.96, STK: New (simpler) algorithms for ISR treatment
C  18.09.96, STK: Switch ISRRM for ISR removal from event record
C  30.10.96, STK: Fix flavour determination for JETSET
C  06.11.96, STK: Reorganise, pack selections into subroutines
C  25.07.00, PMF: Add calculation of particle statistics by LUTABU
C  01.08.00, PMF: Do not fill neither parton level nor hadron level
C                 distributions if LUDSC.EQ..TRUE.
C  01.06.01, PMF: Adapt code for use with Jetset6.3
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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
C  Stuff for HBOOK:
      INTEGER LIMIT
      PARAMETER( LIMIT=200000 )
      REAL HMEM
      COMMON /PAWC/ HMEM(LIMIT)
      SAVE /PAWC/
      LOGICAL LKEEP
      INTEGER I,J,L,II,III,NTRAKP,NTRAKH,JFLAV,LUCHGE,LUCOMP,IERR
      INTEGER IDIM,MAXTRK,ICALL,IRM,IPA,NSTBL
      PARAMETER( IDIM=6, MAXTRK=1000, NSTBL=7 )
      REAL PTRAKP(IDIM,MAXTRK),PTRAKH(IDIM,MAXTRK),PSUM(4),ESUM
      INTEGER IPAR(MAXTRK),IID(MAXTRK),ILIN(MAXTRK),IDSTBL(NSTBL)
      DATA ICALL /0/
      SAVE ICALL
C  sigma-, Lambda, sigma+, xi-, xi0, omega-, K0s:
      DATA IDSTBL / 3112,3122,3222,3312,3322,3334,310 /
      SAVE IDSTBL
C      DATA MASS / 1197.436, 1115.684, 1189.37, 1321.32, 1314.9, 1672.45
C     &           ,497.672
C      DATA CTAU / 4.434,    7.89,     2.396,   4.91,    8.71,   2.46
C     &           ,2.676 /
C
C  Print the occasional progress message:
      ICALL= ICALL+1
      IF( MOD(ICALL,500).EQ.0 ) THEN
        PRINT*,'MCUSEV: called this many times: ',ICALL
      ENDIF
C
C  Needed for MCFIMA
      KEVT=ICALL
C
C  Analyse parton/particle content of current event
      CALL LUTABU(21)
C
C  Print event record if requested:
      IF( ICALL.LE.NRTOPR  .OR. MOD(ICALL,5000).EQ.0 ) THEN
        PRINT*,' '
        WRITE(*,'(80(''*'')/,A,I7)') 'MCUSEV: Event record:',ICALL
        CALL LULIST(2)
        CALL LUEDIT(21)
      ENDIF
C
C  Get stable QCD partons and FSR photons after termination of parton shower:
      CALL MCGETP(1,IDIM,NTRAKP,PTRAKP,ILIN,IID,IPAR)
C
C  Get event flavour in Z0/gamma:
      CALL MCGETF(JFLAV)
C
C  Print selected partons for first few events:
      CALL VZERO(PSUM,4)
      DO I=1,NTRAKP
        DO J=1,4
          PSUM(J)= PSUM(J)+PTRAKP(J,I)
        ENDDO
      ENDDO
      CALL HFILL(10,PSUM(4),0.0,1.0)
      IF( ICALL.LE.NRTOPR .OR. MOD(ICALL,5000).EQ.0 ) THEN
        PRINT*,'MCUSEV: Event flavour: ',JFLAV
        PRINT*,'MCUSEV: Selected partons:'
        PRINT*,'#    ILIN  IID  IPAR   PX      PY      PZ      E'
        DO I=1,NTRAKP
          PRINT 30,I,ILIN(I),IID(I),IPAR(I)
     &            ,PTRAKP(1,I),PTRAKP(2,I),PTRAKP(3,I),PTRAKP(4,I)
        ENDDO
        WRITE(*,'(1X,''sums:'',16X,4(F8.3))')
     &    PSUM(1),PSUM(2),PSUM(3),PSUM(4)
      ENDIF
   30 FORMAT(1X,I3,1X,I4,1X,I5,1X,I4,2X,4(F8.3))
C
C  Get partons w/o FSR if demanded:
      IF( LFSRRM ) THEN
        CALL MCGETP(0,IDIM,NTRAKP,PTRAKP,ILIN,IID,IPAR)
        ESUM= 0.0
        DO I=1,NTRAKP
          ESUM= ESUM+PTRAKP(4,I)
        ENDDO
        IF( PSUM(4).GT.ESUM ) CALL HFILL(15,PSUM(4)-ESUM,0.0,1.0)
C       Print partons again:
        IF( ICALL.LE.NRTOPR  .OR. MOD(ICALL,5000).EQ.0 ) THEN
          PRINT*,'MCUSEV: partons after FSR removal:'
          PRINT*,'#    ILIN  IID  IPAR'
          DO I=1,NTRAKP
            PRINT 50, I,ILIN(I),IID(I),IPAR(I)
          ENDDO
        ENDIF
      ENDIF
   50 FORMAT(1X,I3,1X,I4,1X,I5,1X,I4)
C
C  Fill "decay length" of long-lived hadrons in histo:
      DO I=1,N
        DO J=1,NSTBL
          IF( ABS(K(I,2)).EQ.IDSTBL(J) ) THEN
            CALL HFILL(37,(V(I,4)+V(I,5))/1000.0,0.0,1.0)
          ENDIF
        ENDDO
      ENDDO
C
C  Mark photons with no parents for removal if demanded, they are ISR:
      IF( LISRRM ) THEN
        IF( ICALL.LE.NRTOPR  .OR. MOD(ICALL,5000).EQ.0 ) THEN
          CALL LUEDIT(5)
          PRINT*,' '
          PRINT*,'MCUSEV: Event record before ISR removal:'
          CALL LULIST(2)
          CALL LUEDIT(22)
        ENDIF
        DO I=1,N
          IF( K(I,2).EQ.22 .AND. K(I,3).EQ.0 ) THEN
            K(I,1)= 21
            CALL HFILL(14,P(I,4),0.0,1.0)
          ENDIF
        ENDDO
      ENDIF
C
C  Get "stable" hadrons after hadronisation and decays:
      CALL MCGETH(IDIM,NTRAKH,PTRAKH)
      IF( ICALL.LE.NRTOPR  .OR. MOD(ICALL,5000).EQ.0 ) THEN
        PRINT*,' '
        PRINT*,'MCUSEV: Stable particles in event record after MCGETH:'
        CALL LULIST(2)
      ENDIF
C
C  Call analysis routine:

      IF(NTRAKH.GE.2) THEN
         IF(ICALL.LE.15) THEN
            WRITE(*,'(A,I4)') 'MCUSEV: FLAVOUR:           ',JFLAV
            WRITE(*,'(A,I4)') 'MCUSEV: NUMBER OF PARTONS: ',NTRAKP
            WRITE(*,'(A,I4)') 'MCUSEV: NUMBER OF HADRONS: ',NTRAKH
            write(*,'(A,A)')  'MCUSEV: GENERATOR        : ',CHMCTYP
         ENDIF
         IF(LBQRK) THEN
            IF(JFLAV.EQ.5) THEN
               CALL MCANAL(JFLAV,NTRAKP,NTRAKH,IDIM,PTRAKP,PTRAKH)
            ENDIF
         ELSEIF(LUDSC) THEN
            IF(JFLAV.LT.5) THEN
               CALL MCANAL(JFLAV,NTRAKP,NTRAKH,IDIM,PTRAKP,PTRAKH)
            ENDIF
         ELSE
            IF(SFLAV.EQ.0) THEN
               CALL MCANAL(JFLAV,NTRAKP,NTRAKH,IDIM,PTRAKP,PTRAKH)
            ELSEIF(SFLAV.EQ.JFLAV) THEN
               CALL MCANAL(JFLAV,NTRAKP,NTRAKH,IDIM,PTRAKP,PTRAKH)
            ENDIF
         ENDIF
      ELSE
         WRITE(*,*) 'SELECTED HADRONS: ',NTRAKH
      ENDIF
C
C  The End:
      RETURN
      END
CDECK  ID>, MCUSFI.
      SUBROUTINE MCUSFI
      IMPLICIT NONE
C  User finish routine, writes histos filled with MC info to file:
C  Stefan Kluth, 03.07.96
C  Modifications:
C  11.07.96, STK: fix filename problem on unix systems, use
C                 external file definition now
C  16.07.96, STK: mods for user FFREAD cards
C  17.07.96, STK: back to FFREAD file defintion, works now on unix
C  25.05.00, PMF: add final calculations for energy-energy correlations
C  25.07.00, PMF: add printout of event statistics via LUTABU
C  02.08.00, PMF: perform flavour dependend final calculations only
C                 if permitted by LUDSC and LBQRK
C  01.06.01, PMF: adapt code for use with Jetset6.3
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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
      INTEGER NPOW
      PARAMETER (NPOW=12)
      CHARACTER*3 CHSHAP(NPOW)
      DOUBLE PRECISION DSHAP1(NPOW),DSHAP2(NPOW),DSHAP3(NPOW)
      INTEGER  NSHAP
C PMF 12.05.00: Change order of variables in order to avoid misalignement
C      COMMON / NPOWER / CHSHAP,DSHAP1,DSHAP2,DSHAP3,NSHAP
      COMMON / NPOWER / DSHAP1,DSHAP2,DSHAP3,NSHAP,CHSHAP
C
C..   KEEP RUNNING SUM OF THE EVENT-SHAPES Y, FOR Y, Y**2, Y**3
C..   WHERE Y IS DEFINED IN MCUSIN DATA STATEMENT
C
C     Stuff for event shape analysis:
C     nbmax: number of bins for the histograms
C     ndimm: dimension of the XBINS array
C
      INTEGER NVAR,NBMAX,NDIMM
      PARAMETER( NBMAX=100, NDIMM=50, NVAR=6 )
      INTEGER NBMAX2
      PARAMETER( NBMAX2=1000 )  ! PMF: needed for finer binning of LINEAR y23 (D) distribution
C EEC special stuff
      INTEGER NBEEC
      REAL PIEEC
      PARAMETER( NBEEC=100, PIEEC=3.1415927 )
      REAL EEC(NBEEC),EECER(NBEEC),EECA(NBEEC/2),EECAER(NBEEC/2)
C
      INTEGER NBIN(NVAR)
      REAL XBINS(NDIMM,NVAR)
      REAL TH,MH,BT,BW,CP,Y23
      REAL TMA, TMI, OBL, SP, AC
      CHARACTER*2 CHVAR(NVAR)
      COMMON /MCEVSH/ NBIN
     &               ,TH,MH,BT,BW,CP,Y23,XBINS
     &               ,CHVAR
      SAVE /MCEVSH/
      INTEGER I,LENOCC,IERR
      INTEGER NENT, IFLAV, IOFF
      REAL  RMEAN,RWIDTH,RSKEW
      CHARACTER*(NAMLEN) CHOUTFILE
C
C PMF 12/05/00: Initialization of /MCEVSH/ is now done
C               in the BLOCK DATA subroutine 'MCEVBL'
C      DATA NBIN / 13, 12, 14, 13, 12, 15 /
C      DATA XBINS / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12
C     &            ,0.15, 0.22, 0.30, 0.40, 0.50, 36*0.0
C     &            ,0.00, 0.06 ,0.075,0.09, 0.11, 0.14, 0.17, 0.20, 0.25
C     &            ,0.30, 0.35, 0.45, 0.60, 37*0.0
C     &            ,0.00, 0.03, 0.04, 0.05, 0.06, 0.075,0.09, 0.11, 0.13
C     &            ,0.16, 0.20, 0.25, 0.30, 0.35, 0.40, 35*0.0
C     &            ,0.00, 0.02, 0.03, 0.04, 0.05, 0.065,0.08, 0.10, 0.15
C     &            ,0.20, 0.25, 0.30, 0.35, 0.40, 36*0.0
C     &            ,0.00, 0.05, 0.08, 0.11, 0.14, 0.18, 0.22, 0.30, 0.4
C     &            ,0.50, 0.60, 0.75, 1.00, 37*0.0
C     &            ,0.00, 0.3E-03, 0.75E-03, 0.13E-02, 0.23E-02, 0.4E-02
C     &            ,0.7E-02, 0.012, 0.0225, 0.04, 0.07, 0.13, 0.235
C     &  ,0.40, 0.70, 1.00, 34*0.0 /
C
C      DATA CHVAR / 'TH', 'MH', 'BT', 'BW', 'CP', 'D2' /
C
C
      REAL EECSUM,EECASUM,EECCUM(NBEEC),EECACUM(NBEEC/2)
C
C..   PRINT THE POWERS
C
      DO I=1,12
         CALL FIN3OUT(DSHAP1(I),DSHAP2(I),DSHAP3(I),NSHAP,
     +        RMEAN,RWIDTH,RSKEW)
         WRITE(*,'(3A,3F8.4,3F12.1)') 'SHAPES ',CHSHAP(I),
     +        ' M W S X X2 X3: ',
     +        RMEAN,RWIDTH,RSKEW,DSHAP1(I),DSHAP2(I),DSHAP3(I)
         CALL HFILL(191,REAL(I),0.,RMEAN)
         CALL HFILL(192,REAL(I),0.,RWIDTH)
         CALL HFILL(193,REAL(I),0.,RSKEW)
         CALL HFILL(194,REAL(I),0.,REAL(DSHAP1(I)))
         CALL HFILL(195,REAL(I),0.,REAL(DSHAP2(I)))
         CALL HFILL(196,REAL(I),0.,REAL(DSHAP3(I)))
      ENDDO
      WRITE(*,*) 'DONE WITH EVENTS: ',NSHAP

      DO IFLAV=1,6
        IF( .NOT.(   (IFLAV.EQ.5.AND.LUDSC)
     +           .OR.(IFLAV.LT.5.AND.LBQRK) )) THEN
         DO IOFF=0,10,10
            CALL HNOENT(920+IFLAV+IOFF,NENT)
            CALL HOPERA(920+IFLAV+IOFF,'+',920+IFLAV+IOFF,
     +           920+IFLAV+IOFF,0.,REAL(NBMAX2)/REAL(NENT))! PMF: NBMAX->NBMAX2
            CALL HNOENT(820+IFLAV+IOFF,NENT)
            CALL HOPERA(820+IFLAV+IOFF,'+',820+IFLAV+IOFF,
     +           820+IFLAV+IOFF,0.,REAL(NBMAX)/REAL(NENT))
C
C     Final calculations for energy-energy correlation

C     --- 1. Normalize and calculate asymmetry in theta bins
            CALL PXEECB(0,0,0.,0.,NBEEC,EEC,EECER,EECA,EECAER
     +           ,'N',IERR,IOFF+IFLAV)
            CALL HPAK(220+IFLAV+IOFF,EEC)
            CALL HPAKE(220+IFLAV+IOFF,EECER)
            CALL HPAK(240+IFLAV+IOFF,EECA)
            CALL HPAKE(240+IFLAV+IOFF,EECAER)
ccc            PRINT *,'=============',IFLAV+IOFF
ccc            CALL PXPRRV ('EEC',NBEEC,EEC)
ccc            CALL PXPRRV ('EECER',NBEEC,EECER)
ccc            CALL PXPRRV ('EECA',(NBEEC/2),EECA)
ccc            CALL PXPRRV ('EECAER',(NBEEC/2),EECAER)

C     --- 2. Fill cumulative distributions in theta bins
            IF( IFLAV.EQ.6) THEN
               EECSUM=0.
               EECASUM=0.
               DO I=1,NBEEC
                  EECSUM=EECSUM+EEC(I)
                  EECCUM(I)=EECSUM
               ENDDO
               CALL HPAK(227+IOFF,EECCUM)
ccc               CALL PXPRRV ('EECCUM',NBEEC,EECCUM)
               DO I=1,NBEEC/2
                  EECASUM=EECASUM+EECA(I)
                  EECACUM(I)=EECASUM
               ENDDO
               CALL HPAK(247+IOFF,EECACUM)
ccc               CALL PXPRRV ('EECACUM',NBEEC/2,EECACUM)
            ENDIF

C     --- 3. Normalize and calculate asymmetry in cos(theta) bins
            CALL PXEECB(0,0,0.,0.,NBEEC,EEC,EECER,EECA,EECAER
     +           ,'N',IERR,-(IOFF+IFLAV+20) )
            CALL HPAK(260+IFLAV+IOFF,EEC)
            CALL HPAKE(260+IFLAV+IOFF,EECER)
            CALL HPAK(280+IFLAV+IOFF,EECA)
            CALL HPAKE(280+IFLAV+IOFF,EECAER)
ccc            PRINT *,'=============',-(IFLAV+IOFF+20)
ccc            CALL PXPRRV ('EEC',NBEEC,EEC)
ccc            CALL PXPRRV ('EECER',NBEEC,EECER)
ccc            CALL PXPRRV ('EECA',(NBEEC/2),EECA)
ccc            CALL PXPRRV ('EECAER',(NBEEC/2),EECAER)

C     --- 4. Fill cumulative distributions in cos(theta) bins
            IF( IFLAV.EQ.6) THEN
               EECSUM=0.
               EECASUM=0.
               DO I=1,NBEEC
                  EECSUM=EECSUM+EEC(I)
                  EECCUM(I)=EECSUM
               ENDDO
               CALL HPAK(267+IOFF,EECCUM)
ccc               CALL PXPRRV ('EECCUM',NBEEC,EECCUM)
               DO I=1,NBEEC/2
                  EECASUM=EECASUM+EECA(I)
                  EECACUM(I)=EECASUM
               ENDDO
               CALL HPAK(287+IOFF,EECACUM)
ccc               CALL PXPRRV ('EECACUM',NBEEC/2,EECACUM)
            ENDIF
         ENDDO
       ENDIF
      ENDDO
C
C     Print event statistics
      CALL LUTABU(22)
C
      CALL UHTOC(HOUTFILE,NBYTEW,CHOUTFILE,NAMLEN)
      PRINT*,' '
      PRINT*,'MCUSFI: Output histos to: ',
     +     CHOUTFILE(1:LENOCC(CHOUTFILE))
      CALL HCDIR('//PAWC',' ')
      CALL HRPUT(0,CHOUTFILE(1:LENOCC(CHOUTFILE)),'N')
C
C Close tab file
C
      IF( LTAB ) CLOSE(UNIT=71)
C Print MC Parameters
C
      RETURN
      END
CDECK  ID>, FIN3OUT.
      SUBROUTINE FIN3OUT(D1,D2,D3,N,RMEAN,RWIDTH,RSKEW)
      DOUBLE PRECISION DMOM(3),DMOMN(3)
      DOUBLE PRECISION D1,D2,D3
      INTEGER N,I
      REAL RMEAN,RWIDTH,RSKEW
* DMOM is sum(x_i),sum((x_i)^2),sum((x_i)^3) for i=1..N
* N is the number of events
* rmean, rwidth, rskew are real mean, width and skewness.
      RMEAN = 0.
      RSKEW = 0.
      RWIDTH = 0.
      IF(N.NE.0) THEN
         DMOMN(1) = D1/N
         DMOMN(2) = D2/N
         DMOMN(3) = D3/N
      ELSE
         RETURN
      ENDIF

      RMEAN=REAL(DMOMN(1))
      RWIDTH=REAL(SQRT(DMOMN(2)-DMOMN(1)**2))
      IF(DMOMN(2).NE.DMOMN(1)**2) THEN
      RSKEW=REAL((DMOMN(3)-3*DMOMN(1)*DMOMN(2)+2*DMOMN(1)**3)/
     +               ((SQRT(DMOMN(2)-DMOMN(1)**2))**3))
      ENDIF
      END

CDECK  ID>, MCFILL.
      SUBROUTINE MCFILL(NTRAK,IDIM,PTRAK,JFLAV,IOFF)
      IMPLICIT NONE
C  Routine to compute event shapes from PTRAK array and fill histos
C  Input:   NTRAK   Number of entries in PTRAK
C           PTRAK() Array with 5-momenta of particles
C           IDIM    1st dimension of PTRAK array
C           JFLAV   quark flavour identification, 1=u,...,5=b
C           IOFF    Offset for histo ids
C  Stefan Kluth, 19.1.96
C  Modifications:
C     22-10-96: include track based quantities SB
C     25-05-00, PMF: - include charged+neutral particles based quantities
C                    - include energy-energy correlations
C  Stuff for HBOOK:
      INTEGER LIMIT
      PARAMETER( LIMIT=200000 )
      REAL HMEM
      COMMON /PAWC/ HMEM(LIMIT)
      SAVE /PAWC/
C     Stuff for event shape analysis:
C     nbmax: number of bins for the histograms
C     ndimm: dimension of the XBINS array
C
      INTEGER NVAR,NBMAX,NDIMM
      PARAMETER( NBMAX=100, NDIMM=50, NVAR=6 )
      INTEGER NBMAX2
      PARAMETER( NBMAX2=1000 )  ! PMF: needed for finer binning of LINEAR y23 (D) distribution
C EEC special stuff
      INTEGER NBEEC
      REAL PIEEC
      PARAMETER( NBEEC=100, PIEEC=3.1415927 )
      REAL EEC(NBEEC),EECER(NBEEC),EECA(NBEEC/2),EECAER(NBEEC/2)
C
      INTEGER NBIN(NVAR)
      REAL XBINS(NDIMM,NVAR)
      REAL TH,MH,BT,BW,CP,Y23
      REAL TMA, TMI, OBL, SP, AC
      CHARACTER*2 CHVAR(NVAR)
      COMMON /MCEVSH/ NBIN
     &               ,TH,MH,BT,BW,CP,Y23,XBINS
     &               ,CHVAR
      SAVE /MCEVSH/
      INTEGER NPOW
      PARAMETER (NPOW=12)
      CHARACTER*3 CHSHAP(NPOW)
      DOUBLE PRECISION DSHAP1(NPOW),DSHAP2(NPOW),DSHAP3(NPOW)
      INTEGER  NSHAP
C PMF 12.05.00: Change order of variables in order to avoid misalignement
C      COMMON / NPOWER / CHSHAP,DSHAP1,DSHAP2,DSHAP3,NSHAP
      COMMON / NPOWER / DSHAP1,DSHAP2,DSHAP3,NSHAP,CHSHAP
C
C..   KEEP RUNNING SUM OF THE EVENT-SHAPES Y, FOR Y, Y**2, Y**3
C..   WHERE Y IS DEFINED IN MCUSIN DATA STATEMENT
C

      INTEGER NTRAK,JFLAV,IDIM,IOFF,IERR,ICALL,I,NFAIL
      REAL PTRAK(IDIM,*)
      REAL XP(1000),YP(1000),PTIN(1000),PTOUT(1000)
      REAL XI
      REAL THRUST,AMH,AML,Y34,Y12
     &    ,THRUS0(3),EVAL(3),EVEC(3,3)
      REAL TVEC0(3,3),TVEC(3)
      REAL CST
      REAL         YY
      INTEGER      NJET
      EQUIVALENCE (THRUST,THRUS0(3))
      EQUIVALENCE (TVEC(1),TVEC0(1,3))
      REAL VY(NBMAX),EX(NBMAX),EY(NBMAX)
      REAL VY2(NBMAX2),EX2(NBMAX2),EY2(NBMAX2) ! PMF: added array with dimension NBMAX2
      REAL XTH(NBMAX),XMH(NBMAX),XBT(NBMAX),XBW(NBMAX),XCP(NBMAX)
     &  ,XD2(NBMAX2),XD2LOG(NBMAX),XTMA(NBMAX),XTMI(NBMAX),XOBL(NBMAX)! PMF: NBMAX->NBMAX2
     &  ,XSP(NBMAX),XAC(NBMAX),XMH2(NBMAX)
      SAVE XTH,XMH,XBT,XBW,XCP,XD2,XD2LOG,XMH2
C  EEC stuff
      REAL EVISIB
      REAL EECVY(NBEEC),EECEX(NBEEC),EECEY(NBEEC)
      REAL AEECVY(NBEEC/2),AEECEX(NBEEC/2),AEECEY(NBEEC/2)
      REAL XEEC1(NBEEC),XAEEC1(NBEEC/2),XEEC2(NBEEC),XAEEC2(NBEEC/2)
      SAVE XEEC1,XAEEC1,XEEC2,XAEEC2
C
C
C PMF 12/05/00: Initialization of /MCEVSH/ is now done
C               in the BLOCK DATA subroutine 'MCEVBL'
C      DATA NBIN / 13, 12, 14, 13, 12, 15 /
C      DATA XBINS / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12
C     &            ,0.15, 0.22, 0.30, 0.40, 0.50, 36*0.0
C     &            ,0.00, 0.06 ,0.075,0.09, 0.11, 0.14, 0.17, 0.20, 0.25
C     &            ,0.30, 0.35, 0.45, 0.60, 37*0.0
C     &            ,0.00, 0.03, 0.04, 0.05, 0.06, 0.075,0.09, 0.11, 0.13
C     &            ,0.16, 0.20, 0.25, 0.30, 0.35, 0.40, 35*0.0
C     &            ,0.00, 0.02, 0.03, 0.04, 0.05, 0.065,0.08, 0.10, 0.15
C     &            ,0.20, 0.25, 0.30, 0.35, 0.40, 36*0.0
C     &            ,0.00, 0.05, 0.08, 0.11, 0.14, 0.18, 0.22, 0.30, 0.4
C     &            ,0.50, 0.60, 0.75, 1.00, 37*0.0
C     &            ,0.00, 0.3E-03, 0.75E-03, 0.13E-02, 0.23E-02, 0.4E-02
C     &            ,0.7E-02, 0.012, 0.0225, 0.04, 0.07, 0.13, 0.235
C     &  ,0.40, 0.70, 1.00, 34*0.0 /
C
C      DATA CHVAR / 'TH', 'MH', 'BT', 'BW', 'CP', 'D2' /
C
      DATA ICALL /0/, NFAIL /0/
      SAVE ICALL,NFAIL
C
C  Fix to fill cumulative distributions:
      ICALL= ICALL+1
      IF( ICALL.EQ.1 ) THEN

        CALL HREBIN(227+IOFF,XEEC1,EECVY,EECEX,EECEY,NBEEC,1,NBEEC)
        CALL HREBIN(247+IOFF,XAEEC1,EECVY,EECEX,EECEY,NBEEC/2,1,NBEEC/2)
        CALL HREBIN(267+IOFF,XEEC2,EECVY,EECEX,EECEY,NBEEC,1,NBEEC)
        CALL HREBIN(287+IOFF,XAEEC2,EECVY,EECEX,EECEY,NBEEC/2,1,NBEEC/2)

        CALL HREBIN(307+IOFF,XTH,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(357+IOFF,XTMA,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(407+IOFF,XMH,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(427+IOFF,XMH2,VY,EX,EY,NBMAX,1,NBMAX) ! PMF 10/03/01: added for MH**2
        CALL HREBIN(457+IOFF,XTMI,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(507+IOFF,XBT,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(557+IOFF,XOBL,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(607+IOFF,XBW,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(657+IOFF,XSP,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(707+IOFF,XCP,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(757+IOFF,XAC,VY,EX,EY,NBMAX,1,NBMAX)
        CALL HREBIN(907+IOFF,XD2,VY2,EX2,EY2,NBMAX2,1,NBMAX2)! PMF: NBMAX->NBMAX2
        CALL HREBIN(807+IOFF,XD2LOG,VY,EX,EY,NBMAX,1,NBMAX)
      ENDIF
C
C  Compute event shapes:
      CALL PXLTH4(NTRAK,IDIM,PTRAK,THRUS0,TVEC0,IERR)
      CALL PXMMBB(NTRAK,IDIM,PTRAK,TVEC,AMH,AML,BT,BW,IERR)
      CALL PXLSP3(NTRAK,IDIM,PTRAK,EVAL,EVEC,IERR)
      CP   = 3.0*( EVAL(1)*EVAL(2) + EVAL(2)*EVAL(3) +
     &          EVAL(3)*EVAL(1) )
      TH   = 1.-THRUST
      MH   = AMH
      TMA = THRUS0(2)
      TMI = THRUS0(1)
      OBL = TMA - TMI
      CST = TVEC0(3,3)          ! PMF 01/11/00: added
      CALL PXJSP3(NTRAK,IDIM,PTRAK,EVAL,EVEC,IERR)
      SP = (3./2.)*(EVAL(1)+EVAL(2))
      AC = (3./2.)*EVAL(1)
C  PMF: Force some negative quantities to be 0.
      IF( CP.LT.0. ) CP=0.
      IF( SP.LT.0. ) SP=0.
      IF( AC.LT.0. ) AC=0.
C
C  PMF: Compute energy-energy correlations.
C  Use modified version of PXEEC4 for simultaneous calculation
C  at hadron and parton level
      EVISIB=0.
      DO I=1,NTRAK
         EVISIB=EVISIB+PTRAK(4,I)
      ENDDO
C     --- in theta bins
      CALL PXEECB(NTRAK,IDIM,PTRAK,EVISIB,NBEEC,
     +     EEC,EECER,EECA,EECAER,'S',IERR,IOFF+JFLAV)
      CALL PXEECB(NTRAK,IDIM,PTRAK,EVISIB,NBEEC,
     +     EEC,EECER,EECA,EECAER,'S',IERR,IOFF+6)
C     --- in cos(theta) bins
      CALL PXEECB(NTRAK,IDIM,PTRAK,EVISIB,NBEEC,
     +     EEC,EECER,EECA,EECAER,'S',IERR, -(IOFF+JFLAV+20) )
      CALL PXEECB(NTRAK,IDIM,PTRAK,EVISIB,NBEEC,
     +     EEC,EECER,EECA,EECAER,'S',IERR, -(IOFF+6+20) )
C  Jet finding with Durham jet algorithm, IMODE=5. Set y23=0 when there
C  are only two partons in the MC event:
      CALL YKERN(5,NTRAK,IDIM,PTRAK,IERR)
      IF( NTRAK.GE.3 ) THEN
        CALL YYJET(2,Y23,Y12,IERR)
      ELSE
        Y23= 0.0
      ENDIF
C
C..   SAVE POWERS OF EVENT SHAPES FOR HADRON LEVEL
C
      IF(IOFF.EQ.10) THEN
         NSHAP     = NSHAP + 1
         DSHAP1(1) = DSHAP1(1) + DBLE(TH)
         DSHAP2(1) = DSHAP2(1) + DBLE(TH)**2
         DSHAP3(1) = DSHAP3(1) + DBLE(TH)**3
         
         DSHAP1(2) = DSHAP1(2) + DBLE(TMA)
         DSHAP2(2) = DSHAP2(2) + DBLE(TMA)**2
         DSHAP3(2) = DSHAP3(2) + DBLE(TMA)**3
         
         DSHAP1(3) = DSHAP1(3) + DBLE(MH)
         DSHAP2(3) = DSHAP2(3) + DBLE(MH)**2
         DSHAP3(3) = DSHAP3(3) + DBLE(MH)**3
         
         DSHAP1(4) = DSHAP1(4) + DBLE(TMI)
         DSHAP2(4) = DSHAP2(4) + DBLE(TMI)**2
         DSHAP3(4) = DSHAP3(4) + DBLE(TMI)**3
         
         DSHAP1(5) = DSHAP1(5) + DBLE(BT)
         DSHAP2(5) = DSHAP2(5) + DBLE(BT)**2
         DSHAP3(5) = DSHAP3(5) + DBLE(BT)**3
         
         DSHAP1(6) = DSHAP1(6) + DBLE(OBL)
         DSHAP2(6) = DSHAP2(6) + DBLE(OBL)**2
         DSHAP3(6) = DSHAP3(6) + DBLE(OBL)**3
         
         DSHAP1(7) = DSHAP1(7) + DBLE(BW)
         DSHAP2(7) = DSHAP2(7) + DBLE(BW)**2
         DSHAP3(7) = DSHAP3(7) + DBLE(BW)**3
         
         DSHAP1(8) = DSHAP1(8) + DBLE(SP)
         DSHAP2(8) = DSHAP2(8) + DBLE(SP)**2
         DSHAP3(8) = DSHAP3(8) + DBLE(SP)**3
         
         DSHAP1(9) = DSHAP1(9) + DBLE(CP)
         DSHAP2(9) = DSHAP2(9) + DBLE(CP)**2
         DSHAP3(9) = DSHAP3(9) + DBLE(CP)**3
         
         DSHAP1(10) = DSHAP1(10) + DBLE(AC)
         DSHAP2(10) = DSHAP2(10) + DBLE(AC)**2
         DSHAP3(10) = DSHAP3(10) + DBLE(AC)**3
         
         DSHAP1(11) = DSHAP1(11) + DBLE(Y23)
         DSHAP2(11) = DSHAP2(11) + DBLE(Y23)**2
         DSHAP3(11) = DSHAP3(11) + DBLE(Y23)**3
         
         DSHAP1(12) = DSHAP1(12) +  1D0-DBLE(TH)
         DSHAP2(12) = DSHAP2(12) + (1D0-DBLE(TH))**2
         DSHAP3(12) = DSHAP3(12) + (1D0-DBLE(TH))**3
      ENDIF
C
C  Fill histos:
      CALL HFILL(300+IOFF+JFLAV,TH,0.,1.)
      CALL HFILL(320+IOFF+JFLAV,CST,0.,1.)  ! PMF 01/11/00: added
      CALL HFILL(350+IOFF+JFLAV,TMA,0.,1.)
      CALL HFILL(400+IOFF+JFLAV,MH,0.,1.)
      CALL HFILL(420+IOFF+JFLAV,MH*MH,0.,1.) ! PMF 10/03/01: added MH**2
      CALL HFILL(450+IOFF+JFLAV,TMI,0.,1.)
      CALL HFILL(500+IOFF+JFLAV,BT,0.,1.)
      CALL HFILL(550+IOFF+JFLAV,OBL,0.,1.)
      CALL HFILL(600+IOFF+JFLAV,BW,0.,1.)
      CALL HFILL(650+IOFF+JFLAV,SP,0.,1.)
      CALL HFILL(700+IOFF+JFLAV,CP,0.,1.)
      CALL HFILL(750+IOFF+JFLAV,AC,0.,1.)
      CALL HFILL(800+IOFF+JFLAV,Y23,0.,1.)
      CALL HFILL(900+IOFF+JFLAV,Y23,0.,1.)
      CALL HFILL(306+IOFF,TH,0.,1.)
      CALL HFILL(326+IOFF,CST,0.,1.)  ! PMF 01/11/00: added
      CALL HFILL(356+IOFF,TMA,0.,1.)
      CALL HFILL(406+IOFF,MH,0.,1.)
      CALL HFILL(426+IOFF,MH*MH,0.,1.) ! PMF 10/03/01: added MH**2
      CALL HFILL(456+IOFF,TMI,0.,1.)
      CALL HFILL(506+IOFF,BT,0.,1.)
      CALL HFILL(556+IOFF,OBL,0.,1.)
      CALL HFILL(606+IOFF,BW,0.,1.)
      CALL HFILL(656+IOFF,SP,0.,1.)
      CALL HFILL(706+IOFF,CP,0.,1.)
      CALL HFILL(756+IOFF,AC,0.,1.)
      CALL HFILL(806+IOFF,Y23,0.,1.)
      CALL HFILL(856+IOFF,1.-TH,0.,1.)
      CALL HFILL(906+IOFF,Y23,0.,1.)
C
C     FILL THE MEAN JET RATE HISTOGRAMS
C
      DO I=1,NBMAX
         YY = XD2LOG(I)
         CALL YNJET(YY,NJET,IERR)
         CALL HFILL(820+IOFF+JFLAV,YY,0.,REAL(NJET))
         CALL HFILL(826+IOFF,YY,0.,REAL(NJET))
      ENDDO
      DO I=1,NBMAX2! PMF: NBMAX->NBMAX2
         YY = XD2(I)
         CALL YNJET(YY,NJET,IERR)
         CALL HFILL(920+IOFF+JFLAV,YY,0.,REAL(NJET))
         CALL HFILL(926+IOFF,YY,0.,REAL(NJET))
      ENDDO



C     Fill cumulative distributions:
c      write(*,'(2I4,7F10.3)') ntrak,jflav,th,tma,mh,tmi,bt,obl,bw
      DO I=1,NBMAX
        IF( TH.LE.XTH(I)   ) CALL HFILL(307+IOFF,XTH(I)  ,0.,1.)
        IF( TMA.LE.XTMA(I) ) CALL HFILL(357+IOFF,XTMA(I),0.,1.)
        IF( MH.LE.XMH(I)   ) CALL HFILL(407+IOFF,XMH(I)  ,0.,1.)
        IF( MH*MH.LE.XMH2(I) ) CALL HFILL(427+IOFF,XMH2(I)  ,0.,1.) ! 10/03/01: added for MH**2
        IF( TMI.LE.XTMI(I) ) CALL HFILL(457+IOFF,XTMI(I),0.,1.)
        IF( BT.LE.XBT(I)   ) CALL HFILL(507+IOFF,XBT(I)  ,0.,1.)
        IF( OBL.LE.XOBL(I) ) CALL HFILL(557+IOFF,XOBL(I),0.,1.)
        IF( BW.LE.XBW(I)   ) CALL HFILL(607+IOFF,XBW(I)  ,0.,1.)
        IF( SP.LE.XSP(I)   ) CALL HFILL(657+IOFF,XOBL(I),0.,1.)
        IF( CP.LE.XCP(I)   ) CALL HFILL(707+IOFF,XCP(I)  ,0.,1.)
        IF( AC.LE.XAC(I)   ) CALL HFILL(757+IOFF,XOBL(I),0.,1.)
        IF( Y23.LE.XD2LOG(I)) CALL HFILL(807+IOFF,XD2LOG(I),0.,1.)
      ENDDO
      DO I=1,NBMAX2
        IF( Y23.LE.XD2(I)  ) CALL HFILL(907+IOFF,XD2(I)  ,0.,1.)! PMF: NBMAX->NBMAX2
      ENDDO

C
C..   COMPUTE TRACK BASED QUANTITIES
C
      CALL QQCTRK(IDIM,NTRAK,PTRAK,XP,YP,PTIN,PTOUT)
      DO I=1,NTRAK
C     ...only charged particles
        IF(PTRAK(6,I).NE.0.) THEN
          CALL HFILL(50+IOFF,ABS(PTIN(I)) ,0.,1.)
          CALL HFILL(51+IOFF,ABS(PTOUT(I)),0.,1.)
          CALL HFILL(52+IOFF,ABS(XP(I))   ,0.,1.)
          CALL HFILL(53+IOFF,ABS(YP(I))   ,0.,1.)
          IF(XP(I).NE.0.) THEN
            XI = LOG(1/ABS(XP(I)))
          ELSE
            XI = 0.
          ENDIF
          CALL HFILL(54+IOFF,XI      ,0.,1.)
        ENDIF
C     ... all particles
        CALL HFILL(55+IOFF,ABS(PTIN(I)) ,0.,1.)
        CALL HFILL(56+IOFF,ABS(PTOUT(I)),0.,1.)
        CALL HFILL(57+IOFF,ABS(XP(I))   ,0.,1.)
        CALL HFILL(58+IOFF,ABS(YP(I))   ,0.,1.)
        IF(XP(I).NE.0.) THEN
           XI = LOG(1/ABS(XP(I)))
        ELSE
           XI = 0.
        ENDIF
        CALL HFILL(59+IOFF,XI      ,0.,1.)
      ENDDO
C
C  The End:
      RETURN
      END
CDECK  ID>, QQBOOS.
      SUBROUTINE QQBOOS(PVEC,IDIM,NTRAK,PTRAK)
      IMPLICIT NONE
C  Boost PTRAK array into restframe of 5-vector PVEC.
C  Input:  PVEC()   5-vector defining restframe, PVEC(5)= MASS
C          IDIM     1st dimension of PTRAK array
C          NTRAK    Number of entries in PTRAK array
C          PTRAK()  Array of 5-vectors to be boosted
C  Output: PTRAK()        "            after boost
C  Author: Stefan Kluth
C  Date: 28.11.95
C  Modifications:
C  13.3.96: Introduce variable first dimension for PTRAK array
      INTEGER I,J,IDIM,NTRAK
      REAL PTRAK(IDIM,*),PVEC(5),PTMP(5)
      DO I=1,NTRAK
        CALL PXLZF5(PVEC,PTRAK(1,I),PTMP)
        DO J=1,5
          PTRAK(J,I)= PTMP(J)
        ENDDO
      ENDDO
      RETURN
      END

CDECK  ID>, QQCTRK.
      SUBROUTINE QQCTRK(IDIM,NTRAK,PTRAK,XP,YP,PTIN,PTOUT)
      IMPLICIT NONE
C  Routine to compute particle based quantities.
C  Input:  IDIM    1st dimension of array PTRAK
C          NTRAK   number of entries in PTRAK
C          PTRAK() array of 5-momenta (px,py,pz,E,m)
C  Output: XP()    array with xp= p/E_beam
C          YP()    array with rapidity w.r.t. thrust axis
C          PTIN()  array with momentum components in event plane
C          PTOUT() array with momentum components out of event plane
C  Author: Stefan Kluth
C  Date: 28.3.96
C  Modifications:
C  Trap NTRAK.LE.1, 12.6.96, Stefan Kluth
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
      INTEGER IDIM, NTRAK
      REAL PTRAK(IDIM,*),XP(*),YP(*),PTIN(*),PTOUT(*)
      INTEGER I,IERR
      REAL XDUMMY(10),TVEC(3,3),SVEC(3,3)
      IF( NTRAK.GT.1 ) THEN
        CALL PXLTH4(NTRAK,IDIM,PTRAK,XDUMMY,TVEC,IERR)
        CALL PXJSP3(NTRAK,IDIM,PTRAK,XDUMMY,SVEC,IERR)
        DO I=1,NTRAK
          XP(I)= SQRT(PTRAK(1,I)**2+PTRAK(2,I)**2+PTRAK(3,I)**2)/
     +      (ECMASS/2)
          CALL PXRAP4(PTRAK(1,I),TVEC(1,3),YP(I),IERR)
          CALL PXPLT3(PTRAK(1,I),SVEC(1,2),PTIN(I),XDUMMY,IERR)
          CALL PXPLT3(PTRAK(1,I),SVEC(1,1),PTOUT(I),XDUMMY,IERR)
        ENDDO
      ELSE
        XP(1)= 0.0
        YP(1)= 0.0
        PTIN(1)= 0.0
        PTOUT(1)= 0.0
      ENDIF
C
C  The End:
      RETURN
      END

CDECK  ID>, MCFIMA.
      SUBROUTINE MCFIMA(VP,VH,KFLAV)
      IMPLICIT NONE
C  Routine to fill hadronisation matrices.
C  Input:   VP()    Array with observables at parton-level
C           VH()    Array with observables at hadron-level
C  Stefan Kluth, 19.1.96
C  Modifications:
C  P. Movilla Fernandez, 01.11.00: Write out arrays into a file 
C                                  Introduce flavour dependent histograming (KFLAV)
      INTEGER IP,IH,IVAR,I,KFLAV
      REAL VP(*),VH(*)
*- stuff needed for writing out into tab file
      CHARACTER CFMT*20
      LOGICAL FIRST /.TRUE./
      SAVE CFMT,FIRST
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
*-
C  Stuff for HBOOK:
      INTEGER LIMIT
      PARAMETER( LIMIT=200000 )
      REAL HMEM
      COMMON /PAWC/ HMEM(LIMIT)
      SAVE /PAWC/
C     Stuff for event shape analysis:
C     nbmax: number of bins for the histograms
C     ndimm: dimension of the XBINS array
C
      INTEGER NVAR,NBMAX,NDIMM
      PARAMETER( NBMAX=100, NDIMM=50, NVAR=6 )
      INTEGER NBMAX2
      PARAMETER( NBMAX2=1000 )  ! PMF: needed for finer binning of LINEAR y23 (D) distribution
C EEC special stuff
      INTEGER NBEEC
      REAL PIEEC
      PARAMETER( NBEEC=100, PIEEC=3.1415927 )
      REAL EEC(NBEEC),EECER(NBEEC),EECA(NBEEC/2),EECAER(NBEEC/2)
C
      INTEGER NBIN(NVAR)
      REAL XBINS(NDIMM,NVAR)
      REAL TH,MH,BT,BW,CP,Y23
      REAL TMA, TMI, OBL, SP, AC
      CHARACTER*2 CHVAR(NVAR)
      COMMON /MCEVSH/ NBIN
     &               ,TH,MH,BT,BW,CP,Y23,XBINS
     &               ,CHVAR
      SAVE /MCEVSH/
C
C PMF 12/05/00: Initialization of /MCEVSH/ is now done
C               in the BLOCK DATA subroutine 'MCEVBL'
C      DATA NBIN / 13, 12, 14, 13, 12, 15 /
C      DATA XBINS / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12
C     &            ,0.15, 0.22, 0.30, 0.40, 0.50, 36*0.0
C     &            ,0.00, 0.06 ,0.075,0.09, 0.11, 0.14, 0.17, 0.20, 0.25
C     &            ,0.30, 0.35, 0.45, 0.60, 37*0.0
C     &            ,0.00, 0.03, 0.04, 0.05, 0.06, 0.075,0.09, 0.11, 0.13
C     &            ,0.16, 0.20, 0.25, 0.30, 0.35, 0.40, 35*0.0
C     &            ,0.00, 0.02, 0.03, 0.04, 0.05, 0.065,0.08, 0.10, 0.15
C     &            ,0.20, 0.25, 0.30, 0.35, 0.40, 36*0.0
C     &            ,0.00, 0.05, 0.08, 0.11, 0.14, 0.18, 0.22, 0.30, 0.4
C     &            ,0.50, 0.60, 0.75, 1.00, 37*0.0
C     &            ,0.00, 0.3E-03, 0.75E-03, 0.13E-02, 0.23E-02, 0.4E-02
C     &            ,0.7E-02, 0.012, 0.0225, 0.04, 0.07, 0.13, 0.235
C     &  ,0.40, 0.70, 1.00, 34*0.0 /
C
C      DATA CHVAR / 'TH', 'MH', 'BT', 'BW', 'CP', 'D2' /
C
C
C  Fill hadronisation matrices:
      DO IVAR=1,NVAR
        IP= 0
        IH= 0
        DO I=1,NBIN(IVAR)
          IF( VP(IVAR).GE.XBINS(I,IVAR) .AND.
     &        VP(IVAR).LT.XBINS(I+1,IVAR) ) IP= I
          IF( VH(IVAR).GE.XBINS(I,IVAR) .AND.
     &        VH(IVAR).LT.XBINS(I+1,IVAR) ) IH= I
        ENDDO
        IF( IP.GT.0 .AND.          IH.GT.0 .AND.
     &      IP.LE.NBIN(IVAR) .AND. IH.LE.NBIN(IVAR)  ) THEN
          CALL HFILL(1000+(IVAR-1)*10+1,VP(IVAR),0.0,1.0)
          CALL HFILL(1000+(IVAR-1)*10+2,VH(IVAR),0.0,1.0)
* PMF 01/11/00: Introduce flavour dependent histograming for hadr. matrices
          CALL HFILL(1100+(IVAR-1)*10+6,REAL(IP),REAL(IH),1.0)
          CALL HFILL(1100+(IVAR-1)*10+KFLAV,REAL(IP),REAL(IH),1.0)
        ENDIF
      ENDDO
C
C Write out into tab file
      IF( LTAB ) THEN
        IF( FIRST ) THEN
            WRITE(CFMT,'(A,I2,A)') '(I6,1X,I1,1X,',2*NVAR,'F5.4)'
            FIRST=.FALSE.
        ENDIF
        WRITE(71,FMT=CFMT) KEVT,KFLAV,(VP(I),I=1,NVAR),(VH(I),I=1,NVAR)
      ENDIF
C
C  The End:
      RETURN
      END
CDECK  ID>, MCANAL.
      SUBROUTINE MCANAL(JFLAV,NTRAKP,NTRAKH,IDIM,PTRAKP,PTRAKH)
      IMPLICIT NONE
C  Routine to do analysis of MC 4-vectors:
C  Input: JFLAV    Quark flavour code
C         NTRAKP   Number of entries in array PTRAKP
C         NTRAKH   Number of entries in array PTRAKPH
C         IDIM     1st dimension of arrays PTRAKP,PTRAKH
C         PTRAKP() Array with parton 4-momenta
C         PTRAKH() Array with hadron 4-momenta
C  Stefan Kluth, 22.1.96
C  Modifications:
C     INTRODUCE BOOST OF THE HADRONIC SYSTEM 25.10.96  SB

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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
C     Stuff for event shape analysis:
C     nbmax: number of bins for the histograms
C     ndimm: dimension of the XBINS array
C
      INTEGER NVAR,NBMAX,NDIMM
      PARAMETER( NBMAX=100, NDIMM=50, NVAR=6 )
      INTEGER NBMAX2
      PARAMETER( NBMAX2=1000 )  ! PMF: needed for finer binning of LINEAR y23 (D) distribution
C EEC special stuff
      INTEGER NBEEC
      REAL PIEEC
      PARAMETER( NBEEC=100, PIEEC=3.1415927 )
      REAL EEC(NBEEC),EECER(NBEEC),EECA(NBEEC/2),EECAER(NBEEC/2)
C
      INTEGER NBIN(NVAR)
      REAL XBINS(NDIMM,NVAR)
      REAL TH,MH,BT,BW,CP,Y23
      REAL TMA, TMI, OBL, SP, AC
      CHARACTER*2 CHVAR(NVAR)
      COMMON /MCEVSH/ NBIN
     &               ,TH,MH,BT,BW,CP,Y23,XBINS
     &               ,CHVAR
      SAVE /MCEVSH/
      LOGICAL FIRST
      INTEGER JFLAV,NTRAKP,NTRAKH,IDIM
      REAL PTRAKP(IDIM,*),PTRAKH(IDIM,*)
      INTEGER I,IVAR,ITRAK,NTRKCH
      REAL EVIS,VP(NVAR),VH(NVAR)
      REAL PEV(5)
C
C PMF 12/05/00: Initialization of /MCEVSH/ is now done
C               in the BLOCK DATA subroutine 'MCEVBL'
C      DATA NBIN / 13, 12, 14, 13, 12, 15 /
C      DATA XBINS / 0.00, 0.01, 0.02, 0.03, 0.04, 0.05, 0.07, 0.09, 0.12
C     &            ,0.15, 0.22, 0.30, 0.40, 0.50, 36*0.0
C     &            ,0.00, 0.06 ,0.075,0.09, 0.11, 0.14, 0.17, 0.20, 0.25
C     &            ,0.30, 0.35, 0.45, 0.60, 37*0.0
C     &            ,0.00, 0.03, 0.04, 0.05, 0.06, 0.075,0.09, 0.11, 0.13
C     &            ,0.16, 0.20, 0.25, 0.30, 0.35, 0.40, 35*0.0
C     &            ,0.00, 0.02, 0.03, 0.04, 0.05, 0.065,0.08, 0.10, 0.15
C     &            ,0.20, 0.25, 0.30, 0.35, 0.40, 36*0.0
C     &            ,0.00, 0.05, 0.08, 0.11, 0.14, 0.18, 0.22, 0.30, 0.4
C     &            ,0.50, 0.60, 0.75, 1.00, 37*0.0
C     &            ,0.00, 0.3E-03, 0.75E-03, 0.13E-02, 0.23E-02, 0.4E-02
C     &            ,0.7E-02, 0.012, 0.0225, 0.04, 0.07, 0.13, 0.235
C     &  ,0.40, 0.70, 1.00, 34*0.0 /
C
C      DATA CHVAR / 'TH', 'MH', 'BT', 'BW', 'CP', 'D2' /
C
      DATA FIRST /.TRUE. /
C
C  Fill general histos:
      CALL HFILL(13,REAL(JFLAV),0.0,1.0)
      CALL HFILL(100+JFLAV,REAL(NTRAKP),0.0,1.0)
      CALL HFILL(200+JFLAV,REAL(NTRAKH),0.0,1.0)
      CALL HFILL(106,REAL(NTRAKP),0.0,1.0)
      CALL HFILL(206,REAL(NTRAKH),0.0,1.0)
C
C  Get number of charged hadrons if charge info supplied in PTRAK(6,):
      IF( IDIM.GE.6 ) THEN
        NTRKCH= 0
        DO I=1,NTRAKH
          IF( PTRAKH(6,I).NE.0.0 ) NTRKCH= NTRKCH+1
        ENDDO
        CALL HFILL(210+JFLAV,REAL(NTRKCH),0.0,1.0)
        CALL HFILL(216,REAL(NTRKCH),0.0,1.0)
        CALL HFILL(220,REAL(NTRKCH),0.0,1.0)
      ENDIF
C
C  Fill histos and store event shape results in array for partons,
C  optionally only for "light" partons:
      IF( LUDSC .AND. JFLAV.EQ.5 ) THEN
        CALL VZERO(VP,6)
      ELSE
        EVIS= 0.0
        DO ITRAK=1,NTRAKP
          EVIS= EVIS+PTRAKP(4,ITRAK)
        ENDDO
        CALL HFILL(11,EVIS,0.,1.)
        CALL MCFILL(NTRAKP,IDIM,PTRAKP,JFLAV,0)
        CALL JTFILL(0,NTRAKP,IDIM,PTRAKP)
c        CALL CTEST (0,NTRAKP,IDIM,PTRAKP)
c        CALL YTEST (0,NTRAKP,IDIM,PTRAKP)
        VP(1)= TH
        VP(2)= MH
        VP(3)= BT
        VP(4)= BW
        VP(5)= CP
        VP(6)= Y23
      ENDIF
C
C  Fill histos and store event shape results for hadrons:
      EVIS= 0.0
      DO ITRAK=1,NTRAKH
        EVIS= EVIS+PTRAKH(4,ITRAK)
      ENDDO
C     
C     BOOST TO THE CMS OF THE HADRONS
C
      IF(LBOOS) THEN
        IF(FIRST) THEN
          WRITE(*,*) '********************************'
          WRITE(*,*) '*** BOOST OF HADRONIC SYSTEM ***'
          WRITE(*,*) '********************************'
        ENDIF
        CALL VZERO(PEV,5)
        DO ITRAK=1,NTRAKH
          PEV(1)=PEV(1)+PTRAKH(1,ITRAK)
          PEV(2)=PEV(2)+PTRAKH(2,ITRAK)
          PEV(3)=PEV(3)+PTRAKH(3,ITRAK)
          PEV(4)=PEV(4)+PTRAKH(4,ITRAK)
        ENDDO
        PEV(5) = PEV(4)**2-(PEV(1)**2+PEV(2)**2+PEV(3)**2)
        IF(PEV(5).GT.0.) THEN
          PEV(5) = SQRT(PEV(5))
          CALL QQBOOS(PEV,IDIM,NTRAKH,PTRAKH)
        ELSE
          PEV(5) = 0.
          WRITE(*,*) 'NOT BOOSTED, MASS<= 0.'
        ENDIF
      ENDIF
      CALL HFILL(12,EVIS,0.0,1.0)
      CALL MCFILL(NTRAKH,IDIM,PTRAKH,JFLAV,10)
      CALL JTFILL(10,NTRAKH,IDIM,PTRAKH)
c      CALL CTEST(10,NTRAKH,IDIM,PTRAKH)
c      CALL YTEST(10,NTRAKH,IDIM,PTRAKH)
      VH(1)= TH
      VH(2)= MH
      VH(3)= BT
      VH(4)= BW
      VH(5)= CP
      VH(6)= Y23

C
C  Fill hadronisation matrices:
      CALL MCFIMA(VP,VH,JFLAV)
C
C     The End:
      FIRST = .FALSE.
      RETURN
      END
*-------------------------------------------------------------------
CDECK  ID>, JTFILL.
      SUBROUTINE JTFILL(IOFF,NTRAK,IDIM,PTRAK)
*
*     Routine to fill histograms with Jet information
*     Jet finding included for JADE E0, Durham, Cambridge
*     and Cone(R) and Cone(e)
*
*     S Bentvelsen, oct 1997
*
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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
      INTEGER MAXBIN
      PARAMETER (MAXBIN=200)

      INTEGER IOFF,NTRAK,IDIM,NWT,K,ID,LOC
      INTEGER NJTOT
      REAL PTRAK(IDIM,*)
      REAL HXMI,HXMA,HYMI,HYMA
      INTEGER NNY,NBIN,IERR,NJET
      INTEGER NPRINT
      CHARACTER*80 CHTOT
      REAL HX(MAXBIN),HY(MAXBIN),HEX(MAXBIN),
     &     HEY(MAXBIN)
C      REAL PJET(5,10)
      INTEGER IPASS(250),IJMUL(10)
      LOGICAL FIRST
      INTEGER IANAL

      REAL Y12,Y23,Y34,Y45,Y56
      INTEGER IJ(500)
      REAL    PJ(4,500)

      REAL PJET(10,10,10)
c      REAL YREC(10),
c      REAL YARR(20)
c      INTEGER HISTOR(2,250)
c      COMMON /YCL/YREC,PJET,HISTOR

      DATA IANAL  / 0 /
      DATA FIRST  / .TRUE. /
      DATA NPRINT / 0 /
      SAVE IANAL
      SAVE NPRINT
      SAVE FIRST

      IANAL = IANAL + 1
      IF(NJTEV.NE.-1.AND.IANAL.GT.NJTEV) RETURN

      IF(FIRST) THEN
         WRITE(*,*) '**********************************'
         WRITE(*,*) '*****    JTFILL              *****'
         WRITE(*,*) '**********************************'
         IF(NJTEV.EQ.-1) THEN
            WRITE(*,*) 'JETFINDING FOR ALL EVENTS'
         ELSE
            WRITE(*,*) 'JETFINDING FOR ',NJTEV,' EVENTS'
         ENDIF
         WRITE(*,*) 
      ENDIF



      IF(LJDE0) THEN
         ID = 1200
         CALL HGIVE(ID+1,CHTOT,NBIN,HXMI,HXMA,NNY,
     &        HYMI,HYMA,NWT,LOC)
         CALL HREBIN(ID+1,HX,HY,HEX,HEY,NBIN,1,NBIN)

         CALL YKERN(1,NTRAK,IDIM,PTRAK,IERR)
         IF(IERR.NE.0) THEN
            WRITE(*,*) 'ERROR IN JADE E0 FINDING: ',IERR
            RETURN
         ENDIF
         CALL MEANJET(IOFF,1)
         DO K=1,NBIN
            CALL YNJET(HX(K),NJET,IERR)
            IF(FIRST) WRITE(*,'(A,F10.6,I4)')
     &           'BINNING E0: ',hx(k),njet
            IF (NJET.EQ.1) NJET=2
            CALL HFILL(ID+8+IOFF,HX(K),0.,FLOAT(NJET))
            IF (NJET.GT.6) NJET=6
            IF (IERR.EQ.0) THEN
               CALL HFILL(ID+IOFF+NJET,HX(K),0.,1.)
            ELSE
               WRITE(6,*) 'ERROR IN HNJET!'
            ENDIF
            Y56 = 0.
            Y45 = 0.
            Y34 = 0.
            Y23 = 0.
            Y12 = 0.
            IF(NTRAK.GE.2) CALL YYJET(2,Y23,Y12,IERR)
            IF(NTRAK.GE.3) CALL YYJET(3,Y34,Y23,IERR)
            IF(NTRAK.GE.4) CALL YYJET(4,Y45,Y34,IERR)
            IF(NTRAK.GE.5) CALL YYJET(5,Y56,Y45,IERR)
            CALL HFILL(ID+IOFF+21,Y12,0.,1.)
            CALL HFILL(ID+IOFF+22,Y23,0.,1.)
            CALL HFILL(ID+IOFF+23,Y34,0.,1.)
            CALL HFILL(ID+IOFF+24,Y45,0.,1.)
            CALL HFILL(ID+IOFF+25,Y56,0.,1.)
         ENDDO
      ENDIF
      IF(LDURH) THEN
         ID = 1300
         CALL HGIVE(ID+1,CHTOT,NBIN,HXMI,HXMA,NNY,
     &        HYMI,HYMA,NWT,LOC)
         CALL HREBIN(ID+1,HX,HY,HEX,HEY,NBIN,1,NBIN)

         CALL YKERN(5,NTRAK,IDIM,PTRAK,IERR)
         IF(IERR.NE.0) THEN
            WRITE(*,*) 'ERROR IN JADE E0 FINDING: ',IERR
            RETURN
         ENDIF
         CALL MEANJET(IOFF,5)
         DO K=1,NBIN
            CALL YNJET(10**HX(K),NJET,IERR)
            IF(FIRST) WRITE(*,'(A,F10.6,I4)')
     &           'BINNING KT: ',hx(k),njet
            IF (NJET.EQ.1) NJET=2
            CALL HFILL(ID+8+IOFF,HX(K),0.,FLOAT(NJET))
            IF (NJET.GT.6) NJET=6
            IF (IERR.EQ.0) THEN
               CALL HFILL(ID+IOFF+NJET,HX(K),0.,1.)
            ELSE
               WRITE(6,*) 'ERROR IN HNJET!'
            ENDIF
            Y56 = 0.
            Y45 = 0.
            Y34 = 0.
            Y23 = 0.
            Y12 = 0.
            IF(NTRAK.GE.2) CALL YYJET(2,Y23,Y12,IERR)
            IF(NTRAK.GE.3) CALL YYJET(3,Y34,Y23,IERR)
            IF(NTRAK.GE.4) CALL YYJET(4,Y45,Y34,IERR)
            IF(NTRAK.GE.5) CALL YYJET(5,Y56,Y45,IERR)
            CALL HFILL(ID+IOFF+21,Y12,0.,1.)
            CALL HFILL(ID+IOFF+22,Y23,0.,1.)
            CALL HFILL(ID+IOFF+23,Y34,0.,1.)
            CALL HFILL(ID+IOFF+24,Y45,0.,1.)
            CALL HFILL(ID+IOFF+25,Y56,0.,1.)
         ENDDO
      ENDIF
      IF(LCAMJ) THEN
         ID = 1400
         CALL HGIVE(ID+1,CHTOT,NBIN,HXMI,HXMA,NNY,
     &        HYMI,HYMA,NWT,LOC)
         CALL HREBIN(ID+1,HX,HY,HEX,HEY,NBIN,1,NBIN)

         NJTOT = MIN(NTRAK,30)
         CALL CKERN(IDIM,NTRAK,PTRAK,NJTOT,IERR)
         IF(IERR.NE.0) THEN
            WRITE(*,*) 'ERROR IN CAMBRIDGE CLUSTERING: ',IERR
            RETURN
         ENDIF

         CALL MEANJET(IOFF,7)
         DO K=1,NBIN
            IF(10**HX(K).GT.1E-4) THEN
               
               CALL CNJET(10**HX(K),NJET,IERR)
C--   CALL PXCAMJ(IDIM,NTRAK,PTRAK,10**HX(K),NJET,IJ,PJ,IERR)
               IF(FIRST) WRITE(*,'(A,F10.6,I4)')
     &              'BINNING CAMJ: ',hx(k),njet
               IF(IERR.NE.0) THEN
                  IF(NPRINT.LE.50) THEN
                     WRITE(*,*) 'ERROR IN CAMBRIDGE CNJET: '
     &                    ,IERR,10**HX(K)
                     NPRINT = NPRINT + 1
                  ENDIF
                  RETURN
               ENDIF
               IF (NJET.EQ.1) NJET=2
               CALL HFILL(ID+8+IOFF,HX(K),0.,FLOAT(NJET))
               IF (NJET.GT.6) NJET=6
               IF (IERR.EQ.0) THEN
                  CALL HFILL(ID+IOFF+NJET,HX(K),0.,1.)
               ELSE
                  WRITE(6,*) 'ERROR IN HNJET!'
               ENDIF
               Y56 = 0.
               Y45 = 0.
               Y34 = 0.
               Y23 = 0.
               Y12 = 0.
               CALL HFILL(ID+IOFF+21,Y12,0.,1.)
               CALL HFILL(ID+IOFF+22,Y23,0.,1.)
               CALL HFILL(ID+IOFF+23,Y34,0.,1.)
               CALL HFILL(ID+IOFF+24,Y45,0.,1.)
               CALL HFILL(ID+IOFF+25,Y56,0.,1.)
            ENDIF
         ENDDO
      ENDIF
      IF(LCONE) THEN
         ID = 1500
         CALL HGIVE(ID+1,CHTOT,NBIN,HXMI,HXMA,NNY,
     &        HYMI,HYMA,NWT,LOC)
         CALL HREBIN(ID+1,HX,HY,HEX,HEY,NBIN,1,NBIN)

         DO K=1,NBIN
            CALL PXCONE(NTRAK,IDIM,PTRAK,HX(K),7.,10,
     &              NJET,PJET,IPASS,IJMUL,IERR)
            IF(FIRST) WRITE(*,'(A,F10.6,I4)')
     &           'BINNING CONE(R): ',hx(k),njet
            IF(IERR.NE.0) THEN
               WRITE(*,*) 'ERROR IN CONE FINDING: ',IERR
               RETURN
            ENDIF
            IF (NJET.EQ.1) NJET=2
            CALL HFILL(ID+8+IOFF,HX(K),0.,FLOAT(NJET))
            IF (NJET.GT.6) NJET=6
            IF (IERR.EQ.0) THEN
               CALL HFILL(ID+IOFF+NJET,HX(K),0.,1.)
            ELSE
               WRITE(6,*) 'ERROR IN HNJET!'
            ENDIF
         ENDDO

         ID = 1520
         CALL HGIVE(ID+1,CHTOT,NBIN,HXMI,HXMA,NNY,
     &        HYMI,HYMA,NWT,LOC)
         CALL HREBIN(ID+1,HX,HY,HEX,HEY,NBIN,1,NBIN)
         DO K=1,NBIN
            CALL PXCONE(NTRAK,IDIM,PTRAK,0.7,HX(K),10,
     &           NJET,PJET,IPASS,IJMUL,IERR)
            IF(FIRST) WRITE(*,'(A,F10.6,I4)') 
     &           'BINNING CONE(E): ',hx(k),njet
            IF(IERR.NE.0) THEN
               WRITE(*,*) 'ERROR IN CONE FINDING: ',IERR
               RETURN
            ENDIF
            IF (NJET.EQ.1) NJET=2
            CALL HFILL(ID+8+IOFF,HX(K),0.,FLOAT(NJET))
            IF (NJET.GT.6) NJET=6
            IF (IERR.EQ.0) THEN
               CALL HFILL(ID+IOFF+NJET,HX(K),0.,1.)
            ELSE
               WRITE(6,*) 'ERROR IN HNJET!'
            ENDIF
         ENDDO
      ENDIF


      FIRST = .FALSE.
      END

CDECK  ID>, YTEST.
      SUBROUTINE YTEST(IOFF,NTRAK,IDIM,PTRAK)
*
*     TESTING OF THE YKERN LIBRARY,
*     IN PARTICULAR THE PROBLEM OF NON-EXISTING JET MULTIPLICITIES IN THE
*     DURHAM FINDER
*
*     S Bentvelsen, apr 1998
*
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
C  FFREAD user variables:
      LOGICAL LFSRRM,LISRRM,LBOOS,LCONE,LDURH,LJDE0,LCAMJ,LCKRN
      INTEGER HOUTFILE(NWONAM)
      INTEGER NJTEV
      COMMON /MCUFFR/ LFSRRM,LISRRM,LBOOS, LCONE,LDURH,LJDE0,LCAMJ
     &                ,LCKRN,NJTEV,HOUTFILE
      SAVE /MCUFFR/
      INTEGER MAXBIN
      PARAMETER (MAXBIN=200)

      INTEGER IOFF,NTRAK,IDIM,IERR,NJET
      REAL PTRAK(IDIM,*)
      REAL YL, YH
      INTEGER K,KRET

      INTEGER   NCH

      DATA      NCH / 0 /
      SAVE      NCH

      IF(NCH.EQ.0) THEN
         CALL HBOOK1(3000,'kt ALL' ,50,0.,50.,0.)
         CALL HBOOK1(3001,'kt ER1' ,50,0.,50.,0.)
         CALL HBOOK1(3002,'kt ER2' ,50,0.,50.,0.)
         CALL HBOOK1(3003,'kt NE ' ,50,0.,50.,0.)

         CALL HBOOK1(4000,'jd ALL' ,50,0.,50.,0.)
         CALL HBOOK1(4001,'jd ER1' ,50,0.,50.,0.)
         CALL HBOOK1(4002,'jd ER2' ,50,0.,50.,0.)
         CALL HBOOK1(4003,'jd NE ' ,50,0.,50.,0.)
      ENDIF

      if(ioff.eq.10) then
c
c..   durham
c
         CALL YKERN(5,NTRAK,IDIM,PTRAK,IERR)
         DO K=2,MIN(NTRAK,10)
            CALL HFILL(3000,K*1.,0.,1.)
            IERR = 0
            CALL YYJET(K,YL,YH,IERR)
            IF(IERR.NE.0) CALL HFILL(3001,K*1.,0.,1.)
            IERR = 0
            CALL YNJET(YL,KRET,IERR)
            IF(IERR.NE.0) CALL HFILL(3002,KRET*1.,0.,1.)
            IF(K.NE.KRET) then
               WRITE(*,*) 'K AND KRET: ',K,KRET
               CALL HFILL(3003,K*1.,0.,1.)
            ENDIF
         ENDDO
c
c..   jade
c
         CALL YKERN(2,NTRAK,IDIM,PTRAK,IERR)
         DO K=2,MIN(NTRAK,10)
            CALL HFILL(4000,K*1.,0.,1.)
            IERR = 0
            CALL YYJET(K,YL,YH,IERR)
            IF(IERR.NE.0) CALL HFILL(4001,K*1.,0.,1.)
            IERR = 0
            CALL YNJET(YL,KRET,IERR)
            IF(IERR.NE.0) CALL HFILL(4002,KRET*1.,0.,1.)
            IF(K.NE.KRET) CALL HFILL(4003,K*1.,0.,1.)
         ENDDO
      endif
      NCH = NCH + 1
      RETURN
      END


C--------------------------------------------------------------------------
CDECK  ID>, MEANJET.
      SUBROUTINE MEANJET(IOFF,JETFINDER)
C--------------------------------------------------------------------------
C
C     MEANJET: STORE THE MEANJET VALUES FOR A LIMITED NUMBER OF EVENTS
C              IN ORDER TO DETERMINE THE STATISTICAL UNCERTAINTY ON ALPHA_S
C              FOR FITTING ALPHA_S OVER A RANGE IN MEANJET.
C              IN THIS WAY THE CORRELATIONS BETWEEN THE POINTS OF <NJET>
C              ARE TAKEN INTO ACCOUNT.
C
C     THE ROUTINE ASSUMES THAT THE JETFINDER HAS BEEN CALLED.
C
C     JETFINDER: VERSION OF THE JETFINDER
C     JETALG   : ARRAY WITH TOTAL NUMBER OF EVENTS FOR THE JETFINDER
C
C     AUGUST 10, 1998, STAN BENTVELSEN
C
C--------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IOFF, JETFINDER

      CHARACTER  TIT*31
      INTEGER    NSAMPLE
      PARAMETER (NSAMPLE = 2000)
      INTEGER    JETALG(10)
      DATA       JETALG / 10*0 /
      INTEGER    HBID
      DATA       HBID   / 60000 /
      LOGICAL    FIRST
      DATA       FIRST / .TRUE. /

      INTEGER    NBIN
      INTEGER    LBIN(20)
      DATA       NBIN / 19 /
      INTEGER    I, J, ID, ID1, IERR, NJET

      REAL       XX(20),XM(19)

      INTEGER    ICURR
      DATA       ICURR / 0 /


      SAVE       HBID, ICURR, FIRST

      IF(FIRST) THEN
         FIRST = .FALSE.
         XX( 1) = .7000000E-05   
         XX( 2) = .1300000E-04   
         XX( 3) = .2256600E-04   
         XX( 4) = .4068000E-04   
         XX( 5) = .7178800E-04   
         XX( 6) = .1282120E-03   
         XX( 7) = .2274480E-03   
         XX( 8) = .4050120E-03   
         XX( 9) = .7196681E-03   
         XX(10) = .1280330E-02   
         XX(11) = .2276270E-02   
         XX(12) = .4048330E-02   
         XX(13) = .7198470E-02   
         XX(14) = .1280150E-01   
         XX(15) = .2276450E-01   
         XX(16) = .4048150E-01   
         XX(17) = .7198650E-01   
         XX(18) = .1280140       
         XX(19) = .2276460       
         XX(20) = .4048140       
         
         XM( 1) = .1000000E-04 
         XM( 2) = .1778300E-04 
         XM( 3) = .3162300E-04 
         XM( 4) = .5623400E-04 
         XM( 5) = .1000000E-03 
         XM( 6) = .1778300E-03 
         XM( 7) = .3162300E-03 
         XM( 8) = .5623400E-03 
         XM( 9) = .9999990E-03 
         XM(10) = .1778300E-02 
         XM(11) = .3162300E-02 
         XM(12) = .5623400E-02 
         XM(13) = .9999985E-02 
         XM(14) = .1778300E-01 
         XM(15) = .3162300E-01 
         XM(16) = .5623400E-01 
         XM(17) = .1000002     
         XM(18) = .1778300     
         XM(19) = .3162300     
      ENDIF

      IF(JETFINDER.LE.9.AND.JETFINDER.GT.0) THEN
C
C..   THIS ROUTINE IS CALLED FOR THE PARTON AND HADRON LEVEL
C..   CONSEQUETIVELY; ONLY COUNT THE 'PARTON LEVEL' EVENTS
C
         IF(IOFF.EQ.0) JETALG(JETFINDER) = JETALG(JETFINDER) + 1

         ID1 = HBID-10000+JETFINDER
         IF(JETALG(JETFINDER).EQ.1) THEN
C
C..   BOOK THE GLOBAL HISTOGRAMS
C
            TIT = 'TOTSAMPLE FOR JETFINDER XX'
            WRITE(TIT(25:26),'(I2)') JETFINDER+IOFF
            CALL HBOOKB(ID1   ,TIT//'PARTON NJET',NBIN,XX,0.)
            CALL HBOOKB(ID1+10,TIT//'PARTON NORM',NBIN,XX,0.)
            CALL HBOOKB(ID1+50,TIT//'HADRON NJET',NBIN,XX,0.)
            CALL HBOOKB(ID1+60,TIT//'HADRON NORM',NBIN,XX,0.)
C
C..   BOOK THE HISTOGRAMS TO CALCULATE THE CORRELATION
C
            CALL HBOOK2(ID1+100,'COV(I,J)  ',19,0.5,19.5,19,0.5,19.5,0.)
            CALL HBOOK2(ID1+101,'MEAN(I)   ',19,0.5,19.5,19,0.5,19.5,0.)
            CALL HBOOK2(ID1+102,'MEAN(J)   ',19,0.5,19.5,19,0.5,19.5,0.)
            CALL HBOOK2(ID1+111,'MEAN(I)**2',19,0.5,19.5,19,0.5,19.5,0.)
            CALL HBOOK2(ID1+112,'MEAN(J)**2',19,0.5,19.5,19,0.5,19.5,0.)
            CALL HBOOK2(ID1+110,'NTOT      ',19,0.5,19.5,19,0.5,19.5,0.)
            PRINT*,TIT
         ENDIF
         IF((JETALG(JETFINDER)/NSAMPLE).LE.100) THEN
C
C..   MAKE SUBSAMPLE HISTOGRAMS FOR THE FIRST 100 SUBSAMPLES.
C
            ID = HBID+100*(JETALG(JETFINDER)/NSAMPLE) +JETFINDER
            IF(MOD(JETALG(JETFINDER),NSAMPLE).EQ.1) THEN
C
C..   BOOK THE SUBSAMPLE HISTOGRAMS
C
               TIT = 'SUBSAMPLE XXX FOR JETFINDER XX'
               WRITE(TIT(11:13),'(I3)') JETALG(JETFINDER)/NSAMPLE
               WRITE(TIT(29:30),'(I2)') JETFINDER+IOFF
               CALL HBOOKB(ID   ,TIT//'PARTON NJET',NBIN,XX,0.)
               CALL HBOOKB(ID+10,TIT//'PARTON NORM',NBIN,XX,0.)
               CALL HBOOKB(ID+50,TIT//'HADRON NJET',NBIN,XX,0.)
               CALL HBOOKB(ID+60,TIT//'HADRON NORM',NBIN,XX,0.)
               PRINT*,TIT
            ENDIF
C
C..   FILL THE SUBSAMPLE HISTOGRAMS
C
            DO I=1,NBIN
               IF(JETFINDER.EQ.7) THEN
                  CALL CNJET(XM(I),NJET,IERR)
               ELSE
                  CALL YNJET(XM(I),NJET,IERR)
               ENDIF
               CALL HFILL(ID+5*IOFF,XM(I),0.,1.*NJET)
               IF(NJET.NE.0) THEN
                  CALL HFILL(ID+5*IOFF+10,XM(I),0.,1.)
               ENDIF
            ENDDO
         ENDIF
C
C..   FILL THE TOTSAMPLE HISTOGRAMS
C
         DO I=1,NBIN
            IF(JETFINDER.EQ.7) THEN
               CALL CNJET(XM(I),NJET,IERR)
            ELSE
               CALL YNJET(XM(I),NJET,IERR)
            ENDIF
            CALL HFILL(ID1+5*IOFF,XM(I),0.,1.*NJET)
            IF(NJET.NE.0) THEN
               CALL HFILL(ID1+5*IOFF+10,XM(I),0.,1.)
            ENDIF
            LBIN(I) = NJET
         ENDDO
C
C..   ATTEMPT TO CALCULATE THE CORRELATIONS BETWEEN THE NJET
C
         IF(IOFF.EQ.10) THEN
            DO I=1,NBIN
               DO J=1,NBIN
                  CALL HFILL(ID1+100,1.*I,1.*J,1.*LBIN(I)*LBIN(J))
                  CALL HFILL(ID1+101,1.*I,1.*J,1.*LBIN(I)        )
                  CALL HFILL(ID1+102,1.*I,1.*J,1.*        LBIN(J))
                  CALL HFILL(ID1+111,1.*I,1.*J,1.*LBIN(I)*LBIN(I))
                  CALL HFILL(ID1+112,1.*I,1.*J,1.*LBIN(J)*LBIN(J))
                  
                  CALL HFILL(ID1+110,1.*I,1.*J,1.)
               ENDDO
            ENDDO
         ENDIF
      ELSE
         PRINT*,'ERROR IN JETFINDER: ',JETFINDER
      ENDIF

      RETURN
      END
CDECK  ID>, PYMAXI.
      SUBROUTINE PYMAXI
C      IMPLICIT NONE 
C...Finds optimal set of coefficients for kinematical variable selection
C...and the maximum of the part of the differential cross-section used
C...in the event weighting.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/GUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /GUDAT2/
**JWG  add psi'(3685) as KC code 80, needed for Eurodec interface
*      --- ---------- -- -- ---- --- ------ --- ------- ---------
      INTEGER  IDPSIP
      PARAMETER  (IDPSIP=80)
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      INTEGER  MSEL,MSUB,KFIN
      REAL  CKIN
      SAVE /PYSUBS/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      INTEGER  ISET,KFPR,ICOL
      REAL  COEF
      SAVE /PYINT2/
      COMMON/PYINT3/XSFX(2,-40:40),ISIG(1000,3),SIGH(1000)
      INTEGER  ISIG
      REAL  XSFX,SIGH
      SAVE /PYINT3/
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      REAL  WIDP,WIDE,WIDS
      SAVE /PYINT4/
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYINT6/PROC(0:200)
      CHARACTER PROC*28
      SAVE /PYINT6/
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      REAL  SIGT
      SAVE /PYINT7/
      CHARACTER CVAR(4)*4
      DIMENSION NPTS(4),MVARPT(500,4),VINTPT(500,30),SIGSPT(500),
     &NAREL(7),WTREL(7),WTMAT(7,7),WTRELN(7),COEFU(7),COEFO(7),
     &IACCMX(4),SIGSMX(4),SIGSSM(3)
      DATA CVAR/'tau ','tau''','y*  ','cth '/
      DATA SIGSSM/3*0./
 
C...Select subprocess to study: skip cases not applicable.
      NPOSI=0
      VINT(143)=1.
      VINT(144)=1.
      XSEC(0,1)=0.
      DO 440 ISUB=1,200
      MINT(51)=0
      IF(ISET(ISUB).EQ.11) THEN
        XSEC(ISUB,1)=1.00001*COEF(ISUB,1)
        NPOSI=NPOSI+1
        GOTO 430
      ELSEIF(ISUB.GE.91.AND.ISUB.LE.95) THEN
        XSEC(ISUB,1)=SIGT(0,0,ISUB-90)
        IF(MSUB(ISUB).NE.1) GOTO 440
        NPOSI=NPOSI+1
        GOTO 430
      ELSEIF(ISUB.EQ.96) THEN
        IF(MINT(50).EQ.0) GOTO 440
        IF(MSUB(95).NE.1.AND.MSTP(81).LE.0.AND.MSTP(131).LE.0) GOTO 440
        IF(MINT(49).EQ.0.AND.MSTP(131).EQ.0) GOTO 440
      ELSEIF(ISUB.EQ.11.OR.ISUB.EQ.12.OR.ISUB.EQ.13.OR.ISUB.EQ.28.OR.
     &ISUB.EQ.53.OR.ISUB.EQ.68) THEN
        IF(MSUB(ISUB).NE.1.OR.MSUB(95).EQ.1) GOTO 440
      ELSE
        IF(MSUB(ISUB).NE.1) GOTO 440
      ENDIF
      MINT(1)=ISUB
      ISTSB=ISET(ISUB)
      IF(ISUB.EQ.96) ISTSB=2
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5000) ISUB
      MWTXS=0
      IF(MSTP(142).GE.1.AND.ISUB.NE.96.AND.MSUB(91)+MSUB(92)+MSUB(93)+
     &MSUB(94)+MSUB(95).EQ.0) MWTXS=1
 
C...Find resonances (explicit or implicit in cross-section).
      MINT(72)=0
      KFR1=0
      IF(ISTSB.EQ.1.OR.ISTSB.EQ.3.OR.ISTSB.EQ.5) THEN
        KFR1=KFPR(ISUB,1)
      ELSEIF(ISUB.EQ.24.OR.ISUB.EQ.25.OR.ISUB.EQ.110.OR.ISUB.EQ.165.OR.
     &ISUB.EQ.171.OR.ISUB.EQ.176) THEN
        KFR1=23
      ELSEIF(ISUB.EQ.23.OR.ISUB.EQ.26.OR.ISUB.EQ.166.OR.ISUB.EQ.172.OR.
     &ISUB.EQ.177) THEN
        KFR1=24
      ELSEIF(ISUB.GE.71.AND.ISUB.LE.77) THEN
        KFR1=25
        IF(MSTP(46).EQ.5) THEN
          KFR1=30
          PMAS(30,1)=PARP(45)
          PMAS(30,2)=PARP(45)**3/(96.*PARU(1)*PARP(47)**2)
        ENDIF
      ENDIF
      CKMX=CKIN(2)
      IF(CKMX.LE.0.) CKMX=VINT(1)
      IF(KFR1.NE.0) THEN
        IF(CKIN(1).GT.PMAS(KFR1,1)+20.*PMAS(KFR1,2).OR.
     &  CKMX.LT.PMAS(KFR1,1)-20.*PMAS(KFR1,2)) KFR1=0
      ENDIF
      IF(KFR1.NE.0) THEN
        TAUR1=PMAS(KFR1,1)**2/VINT(2)
        GAMR1=PMAS(KFR1,1)*PMAS(KFR1,2)/VINT(2)
        MINT(72)=1
        MINT(73)=KFR1
        VINT(73)=TAUR1
        VINT(74)=GAMR1
      ENDIF
      IF(ISUB.EQ.141) THEN
        KFR2=23
        TAUR2=PMAS(KFR2,1)**2/VINT(2)
        GAMR2=PMAS(KFR2,1)*PMAS(KFR2,2)/VINT(2)
        IF(CKIN(1).GT.PMAS(KFR2,1)+20.*PMAS(KFR2,2).OR.
     &  CKMX.LT.PMAS(KFR2,1)-20.*PMAS(KFR2,2)) KFR2=0
        IF(KFR2.NE.0.AND.KFR1.NE.0) THEN
          MINT(72)=2
          MINT(74)=KFR2
          VINT(75)=TAUR2
          VINT(76)=GAMR2
        ELSEIF(KFR2.NE.0) THEN
          KFR1=KFR2
          TAUR1=TAUR2
          GAMR1=GAMR2
          MINT(72)=1
          MINT(73)=KFR1
          VINT(73)=TAUR1
          VINT(74)=GAMR1
        ENDIF
      ENDIF
 
C...Find product masses and minimum pT of process.
      SQM3=0.
      SQM4=0.
      MINT(71)=0
      VINT(71)=CKIN(3)
      VINT(80)=1.
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4) THEN
        NBW=0
        DO 100 I=1,2
        IF(KFPR(ISUB,I).EQ.0) THEN
        ELSEIF(MSTP(42).LE.0.OR.PMAS(LUCOMP(KFPR(ISUB,I)),2).LT.
     &  PARP(41)) THEN
          IF(I.EQ.1) SQM3=PMAS(LUCOMP(KFPR(ISUB,I)),1)**2
          IF(I.EQ.2) SQM4=PMAS(LUCOMP(KFPR(ISUB,I)),1)**2
        ELSE
          NBW=NBW+1
        ENDIF
  100   CONTINUE
        IF(NBW.GE.1) THEN
          CALL PYOFSH(3,0,KFPR(ISUB,1),KFPR(ISUB,2),0.,PQM3,PQM4)
          IF(MINT(51).EQ.1) THEN
            WRITE(MSTU(11),5100) ISUB
            MSUB(ISUB)=0
            GOTO 440
          ENDIF
          SQM3=PQM3**2
          SQM4=PQM4**2
        ENDIF
        IF(MIN(SQM3,SQM4).LT.CKIN(6)**2) MINT(71)=1
        IF(MINT(71).EQ.1) VINT(71)=MAX(CKIN(3),CKIN(5))
        IF(ISUB.EQ.96.AND.MSTP(82).LE.1) VINT(71)=PARP(81)
        IF(ISUB.EQ.96.AND.MSTP(82).GE.2) VINT(71)=0.08*PARP(82)
      ELSEIF(ISTSB.EQ.6) THEN
        CALL PYOFSH(5,0,KFPR(ISUB,1),KFPR(ISUB,2),0.,PQM3,PQM4)
        IF(MINT(51).EQ.1) THEN
          WRITE(MSTU(11),5100) ISUB
          MSUB(ISUB)=0
          GOTO 440
        ENDIF
        SQM3=PQM3**2
        SQM4=PQM4**2
      ENDIF
      VINT(63)=SQM3
      VINT(64)=SQM4
 
C...Prepare for additional variable choices in 2 -> 3.
      IF(ISTSB.EQ.5) THEN
        VINT(201)=0.
        IF(KFPR(ISUB,2).GT.0) VINT(201)=PMAS(KFPR(ISUB,2),1)
        VINT(206)=VINT(201)
        VINT(204)=PMAS(23,1)
        IF(ISUB.EQ.124) VINT(204)=PMAS(24,1)
        IF(ISUB.EQ.121.OR.ISUB.EQ.122.OR.ISUB.EQ.181.OR.ISUB.EQ.182.OR.
     &  ISUB.EQ.186.OR.ISUB.EQ.187) VINT(204)=VINT(201)
        VINT(209)=VINT(204)
      ENDIF
 
C...Number of points for each variable: tau, tau', y*, cos(theta-hat).
      NPTS(1)=2+2*MINT(72)
      IF(MINT(47).EQ.1) THEN
        IF(ISTSB.EQ.1.OR.ISTSB.EQ.2.OR.ISTSB.EQ.6) NPTS(1)=1
      ELSEIF(MINT(47).EQ.5) THEN
        IF(ISTSB.LE.2.OR.ISTSB.GE.6) NPTS(1)=NPTS(1)+1
      ENDIF
      NPTS(2)=1
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) THEN
        IF(MINT(47).GE.2) NPTS(2)=2
        IF(MINT(47).EQ.5) NPTS(2)=3
      ENDIF
      NPTS(3)=1
      IF(MINT(47).GE.4) NPTS(3)=3
      IF(MINT(45).EQ.3) NPTS(3)=NPTS(3)+1
      IF(MINT(46).EQ.3) NPTS(3)=NPTS(3)+1
      NPTS(4)=1
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) NPTS(4)=5
      NTRY=NPTS(1)*NPTS(2)*NPTS(3)*NPTS(4)
 
C...Reset coefficients of cross-section weighting.
      DO 110 J=1,20
      COEF(ISUB,J)=0.
  110 CONTINUE
      COEF(ISUB,1)=1.
      COEF(ISUB,8)=0.5
      COEF(ISUB,9)=0.5
      COEF(ISUB,13)=1.
      COEF(ISUB,18)=1.
      MCTH=0
      MTAUP=0
      METAUP=0
      VINT(23)=0.
      VINT(26)=0.
      SIGSAM=0.
 
C...Find limits and select tau, y*, cos(theta-hat) and tau' values,
C...in grid of phase space points.
      CALL PYKLIM(1)
      METAU=MINT(51)
      NACC=0
      DO 140 ITRY=1,NTRY
      MINT(51)=0
      IF(METAU.EQ.1) GOTO 140
      IF(MOD(ITRY-1,NPTS(2)*NPTS(3)*NPTS(4)).EQ.0) THEN
        MTAU=1+(ITRY-1)/(NPTS(2)*NPTS(3)*NPTS(4))
        IF(MTAU.GT.2+2*MINT(72)) MTAU=7
        CALL PYKMAP(1,MTAU,0.5)
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) CALL PYKLIM(4)
        METAUP=MINT(51)
      ENDIF
      IF(METAUP.EQ.1) GOTO 140
      IF(ISTSB.GE.3.AND.ISTSB.LE.5.AND.MOD(ITRY-1,NPTS(3)*NPTS(4))
     &.EQ.0) THEN
        MTAUP=1+MOD((ITRY-1)/(NPTS(3)*NPTS(4)),NPTS(2))
        CALL PYKMAP(4,MTAUP,0.5)
      ENDIF
      IF(MOD(ITRY-1,NPTS(3)*NPTS(4)).EQ.0) THEN
        CALL PYKLIM(2)
        MEYST=MINT(51)
      ENDIF
      IF(MEYST.EQ.1) GOTO 140
      IF(MOD(ITRY-1,NPTS(4)).EQ.0) THEN
        MYST=1+MOD((ITRY-1)/NPTS(4),NPTS(3))
        IF(MYST.EQ.4.AND.MINT(45).NE.3) MYST=5
        CALL PYKMAP(2,MYST,0.5)
        CALL PYKLIM(3)
        MECTH=MINT(51)
      ENDIF
      IF(MECTH.EQ.1) GOTO 140
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) THEN
        MCTH=1+MOD(ITRY-1,NPTS(4))
        CALL PYKMAP(3,MCTH,0.5)
      ENDIF
      IF(ISUB.EQ.96) VINT(25)=VINT(21)*(1.-VINT(23)**2)
 
C...Store position and limits.
      MINT(51)=0
      CALL PYKLIM(0)
      IF(MINT(51).EQ.1) GOTO 140
      NACC=NACC+1
      MVARPT(NACC,1)=MTAU
      MVARPT(NACC,2)=MTAUP
      MVARPT(NACC,3)=MYST
      MVARPT(NACC,4)=MCTH
      DO 120 J=1,30
      VINTPT(NACC,J)=VINT(10+J)
  120 CONTINUE
 
C...Normal case: calculate cross-section.
      IF(ISTSB.NE.5) THEN
        CALL PYSIGH(NCHN,SIGS)
        IF(MWTXS.EQ.1) THEN
          CALL PYEVWT(WTXS)
          SIGS=WTXS*SIGS
        ENDIF
 
C..2 -> 3: find highest value out of a number of tries.
      ELSE
        SIGS=0.
        DO 130 IKIN3=1,MSTP(129)
        CALL PYKMAP(5,0,0.)
        IF(MINT(51).EQ.1) GOTO 130
        CALL PYSIGH(NCHN,SIGTMP)
        IF(MWTXS.EQ.1) THEN
          CALL PYEVWT(WTXS)
          SIGTMP=WTXS*SIGTMP
        ENDIF
        IF(SIGTMP.GT.SIGS) SIGS=SIGTMP
  130   CONTINUE
      ENDIF
 
C...Store cross-section.
      SIGSPT(NACC)=SIGS
      IF(SIGS.GT.SIGSAM) SIGSAM=SIGS
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5200) MTAU,MYST,MCTH,MTAUP,
     &VINT(21),VINT(22),VINT(23),VINT(26),SIGS
  140 CONTINUE
      IF(NACC.EQ.0) THEN
        WRITE(MSTU(11),5100) ISUB
        MSUB(ISUB)=0
        GOTO 440
      ELSEIF(SIGSAM.EQ.0.) THEN
        WRITE(MSTU(11),5300) ISUB
        MSUB(ISUB)=0
        GOTO 440
      ENDIF
      IF(ISUB.NE.96) NPOSI=NPOSI+1
 
C...Calculate integrals in tau over maximal phase space limits.
      TAUMIN=VINT(11)
      TAUMAX=VINT(31)
      ATAU1=LOG(TAUMAX/TAUMIN)
      IF(NPTS(1).GE.2) THEN
        ATAU2=(TAUMAX-TAUMIN)/(TAUMAX*TAUMIN)
      ENDIF
      IF(NPTS(1).GE.4) THEN
        ATAU3=LOG(TAUMAX/TAUMIN*(TAUMIN+TAUR1)/(TAUMAX+TAUR1))/TAUR1
        ATAU4=(ATAN((TAUMAX-TAUR1)/GAMR1)-ATAN((TAUMIN-TAUR1)/GAMR1))/
     &  GAMR1
      ENDIF
      IF(NPTS(1).GE.6) THEN
        ATAU5=LOG(TAUMAX/TAUMIN*(TAUMIN+TAUR2)/(TAUMAX+TAUR2))/TAUR2
        ATAU6=(ATAN((TAUMAX-TAUR2)/GAMR2)-ATAN((TAUMIN-TAUR2)/GAMR2))/
     &  GAMR2
      ENDIF
      IF(NPTS(1).GT.2+2*MINT(72)) THEN
        ATAU7=LOG(MAX(2E-6,1.-TAUMIN)/MAX(2E-6,1.-TAUMAX))
      ENDIF
 
C...Reset. Sum up cross-sections in points calculated.
      DO 300 IVAR=1,4
      IF(NPTS(IVAR).EQ.1) GOTO 300
      IF(ISUB.EQ.96.AND.IVAR.EQ.4) GOTO 300
      NBIN=NPTS(IVAR)
      DO 160 J1=1,NBIN
      NAREL(J1)=0
      WTREL(J1)=0.
      COEFU(J1)=0.
      DO 150 J2=1,NBIN
      WTMAT(J1,J2)=0.
  150 CONTINUE
  160 CONTINUE
      DO 170 IACC=1,NACC
      IBIN=MVARPT(IACC,IVAR)
      IF(IVAR.EQ.1.AND.IBIN.EQ.7) IBIN=3+2*MINT(72)
      IF(IVAR.EQ.3.AND.IBIN.EQ.5.AND.MINT(45).NE.3) IBIN=4
      NAREL(IBIN)=NAREL(IBIN)+1
      WTREL(IBIN)=WTREL(IBIN)+SIGSPT(IACC)
 
C...Sum up tau cross-section pieces in points used.
      IF(IVAR.EQ.1) THEN
        TAU=VINTPT(IACC,11)
        WTMAT(IBIN,1)=WTMAT(IBIN,1)+1.
        WTMAT(IBIN,2)=WTMAT(IBIN,2)+(ATAU1/ATAU2)/TAU
        IF(NBIN.GE.4) THEN
          WTMAT(IBIN,3)=WTMAT(IBIN,3)+(ATAU1/ATAU3)/(TAU+TAUR1)
          WTMAT(IBIN,4)=WTMAT(IBIN,4)+(ATAU1/ATAU4)*TAU/
     &    ((TAU-TAUR1)**2+GAMR1**2)
        ENDIF
        IF(NBIN.GE.6) THEN
          WTMAT(IBIN,5)=WTMAT(IBIN,5)+(ATAU1/ATAU5)/(TAU+TAUR2)
          WTMAT(IBIN,6)=WTMAT(IBIN,6)+(ATAU1/ATAU6)*TAU/
     &    ((TAU-TAUR2)**2+GAMR2**2)
        ENDIF
        IF(NBIN.GT.2+2*MINT(72)) THEN
          WTMAT(IBIN,NBIN)=WTMAT(IBIN,NBIN)+(ATAU1/ATAU7)*
     &    TAU/MAX(2E-6,1.-TAU)
        ENDIF
 
C...Sum up tau' cross-section pieces in points used.
      ELSEIF(IVAR.EQ.2) THEN
        TAU=VINTPT(IACC,11)
        TAUP=VINTPT(IACC,16)
        TAUPMN=VINTPT(IACC,6)
        TAUPMX=VINTPT(IACC,26)
        ATAUP1=LOG(TAUPMX/TAUPMN)
        ATAUP2=((1.-TAU/TAUPMX)**4-(1.-TAU/TAUPMN)**4)/(4.*TAU)
        WTMAT(IBIN,1)=WTMAT(IBIN,1)+1.
        WTMAT(IBIN,2)=WTMAT(IBIN,2)+(ATAUP1/ATAUP2)*(1.-TAU/TAUP)**3/
     &  TAUP
        IF(NBIN.GE.3) THEN
          ATAUP3=LOG(MAX(2E-6,1.-TAUPMN)/MAX(2E-6,1.-TAUPMX))
          WTMAT(IBIN,3)=WTMAT(IBIN,3)+(ATAUP1/ATAUP3)*
     &    TAUP/MAX(2E-6,1.-TAUP)
        ENDIF
 
C...Sum up y* cross-section pieces in points used.
      ELSEIF(IVAR.EQ.3) THEN
        YST=VINTPT(IACC,12)
        YSTMIN=VINTPT(IACC,2)
        YSTMAX=VINTPT(IACC,22)
        AYST0=YSTMAX-YSTMIN
        AYST1=0.5*(YSTMAX-YSTMIN)**2
        AYST2=AYST1
        AYST3=2.*(ATAN(EXP(YSTMAX))-ATAN(EXP(YSTMIN)))
        WTMAT(IBIN,1)=WTMAT(IBIN,1)+(AYST0/AYST1)*(YST-YSTMIN)
        WTMAT(IBIN,2)=WTMAT(IBIN,2)+(AYST0/AYST2)*(YSTMAX-YST)
        WTMAT(IBIN,3)=WTMAT(IBIN,3)+(AYST0/AYST3)/COSH(YST)
        IF(MINT(45).EQ.3) THEN
          TAUE=VINTPT(IACC,11)
          IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=VINTPT(IACC,16)
          YST0=-0.5*LOG(TAUE)
          AYST4=LOG(MAX(1E-6,EXP(YST0-YSTMIN)-1.)/
     &    MAX(1E-6,EXP(YST0-YSTMAX)-1.))
          WTMAT(IBIN,4)=WTMAT(IBIN,4)+(AYST0/AYST4)/
     &    MAX(1E-6,1.-EXP(YST-YST0))
        ENDIF
        IF(MINT(46).EQ.3) THEN
          TAUE=VINTPT(IACC,11)
          IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=VINTPT(IACC,16)
          YST0=-0.5*LOG(TAUE)
          AYST5=LOG(MAX(1E-6,EXP(YST0+YSTMAX)-1.)/
     &    MAX(1E-6,EXP(YST0+YSTMIN)-1.))
          WTMAT(IBIN,NBIN)=WTMAT(IBIN,NBIN)+(AYST0/AYST5)/
     &    MAX(1E-6,1.-EXP(-YST-YST0))
        ENDIF
 
C...Sum up cos(theta-hat) cross-section pieces in points used.
      ELSE
        RM34=MAX(1E-20,2.*SQM3*SQM4/(VINTPT(IACC,11)*VINT(2))**2)
        RSQM=1.+RM34
        CTHMAX=SQRT(1.-4.*VINT(71)**2/(TAUMAX*VINT(2)))
        CTHMIN=-CTHMAX
        IF(CTHMAX.GT.0.9999) RM34=MAX(RM34,2.*VINT(71)**2/
     &  (TAUMAX*VINT(2)))
        ACTH1=CTHMAX-CTHMIN
        ACTH2=LOG(MAX(RM34,RSQM-CTHMIN)/MAX(RM34,RSQM-CTHMAX))
        ACTH3=LOG(MAX(RM34,RSQM+CTHMAX)/MAX(RM34,RSQM+CTHMIN))
        ACTH4=1./MAX(RM34,RSQM-CTHMAX)-1./MAX(RM34,RSQM-CTHMIN)
        ACTH5=1./MAX(RM34,RSQM+CTHMIN)-1./MAX(RM34,RSQM+CTHMAX)
        CTH=VINTPT(IACC,13)
        WTMAT(IBIN,1)=WTMAT(IBIN,1)+1.
        WTMAT(IBIN,2)=WTMAT(IBIN,2)+(ACTH1/ACTH2)/MAX(RM34,RSQM-CTH)
        WTMAT(IBIN,3)=WTMAT(IBIN,3)+(ACTH1/ACTH3)/MAX(RM34,RSQM+CTH)
        WTMAT(IBIN,4)=WTMAT(IBIN,4)+(ACTH1/ACTH4)/MAX(RM34,RSQM-CTH)**2
        WTMAT(IBIN,5)=WTMAT(IBIN,5)+(ACTH1/ACTH5)/MAX(RM34,RSQM+CTH)**2
      ENDIF
  170 CONTINUE
 
C...Check that equation system solvable; else trivial way out.
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5400) CVAR(IVAR)
      MSOLV=1
      WTRELS=0.
      DO 180 IBIN=1,NBIN
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5500) (WTMAT(IBIN,IRED),
     &IRED=1,NBIN),WTREL(IBIN)
      IF(NAREL(IBIN).EQ.0) MSOLV=0
      WTRELS=WTRELS+WTREL(IBIN)
  180 CONTINUE
      IF(MSOLV.EQ.0) THEN
        DO 190 IBIN=1,NBIN
        COEFU(IBIN)=1.
        WTRELN(IBIN)=0.1
        IF(WTRELS.GT.0.) WTRELN(IBIN)=MAX(0.1,WTREL(IBIN)/WTRELS)
  190   CONTINUE
 
C...Solve to find relative importance of cross-section pieces.
      ELSE
        DO 200 IBIN=1,NBIN
        WTRELN(IBIN)=MAX(0.1,WTREL(IBIN)/WTRELS)
  200   CONTINUE
        DO 230 IRED=1,NBIN-1
        DO 220 IBIN=IRED+1,NBIN
        RQT=WTMAT(IBIN,IRED)/WTMAT(IRED,IRED)
        WTREL(IBIN)=WTREL(IBIN)-RQT*WTREL(IRED)
        DO 210 ICOE=IRED,NBIN
        WTMAT(IBIN,ICOE)=WTMAT(IBIN,ICOE)-RQT*WTMAT(IRED,ICOE)
  210   CONTINUE
  220   CONTINUE
  230   CONTINUE
        DO 250 IRED=NBIN,1,-1
        DO 240 ICOE=IRED+1,NBIN
        WTREL(IRED)=WTREL(IRED)-WTMAT(IRED,ICOE)*COEFU(ICOE)
  240   CONTINUE
        COEFU(IRED)=WTREL(IRED)/WTMAT(IRED,IRED)
  250   CONTINUE
      ENDIF
 
C...Normalize coefficients, with piece shared democratically.
      COEFSU=0.
      WTRELS=0.
      DO 260 IBIN=1,NBIN
      COEFU(IBIN)=MAX(0.,COEFU(IBIN))
      COEFSU=COEFSU+COEFU(IBIN)
      WTRELS=WTRELS+WTRELN(IBIN)
  260 CONTINUE
      IF(COEFSU.GT.0.) THEN
        DO 270 IBIN=1,NBIN
        COEFO(IBIN)=PARP(122)/NBIN+(1.-PARP(122))*0.5*
     &  (COEFU(IBIN)/COEFSU+WTRELN(IBIN)/WTRELS)
  270   CONTINUE
      ELSE
        DO 280 IBIN=1,NBIN
        COEFO(IBIN)=1./NBIN
  280   CONTINUE
      ENDIF
      IF(IVAR.EQ.1) IOFF=0
      IF(IVAR.EQ.2) IOFF=17
      IF(IVAR.EQ.3) IOFF=7
      IF(IVAR.EQ.4) IOFF=12
      DO 290 IBIN=1,NBIN
      ICOF=IOFF+IBIN
      IF(IVAR.EQ.1.AND.IBIN.GT.2+2*MINT(72)) ICOF=7
      IF(IVAR.EQ.3.AND.IBIN.EQ.4.AND.MINT(45).NE.3) ICOF=ICOF+1
      COEF(ISUB,ICOF)=COEFO(IBIN)
  290 CONTINUE
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5600) CVAR(IVAR),
     &(COEFO(IBIN),IBIN=1,NBIN)
  300 CONTINUE
 
C...Find two most promising maxima among points previously determined.
      DO 310 J=1,4
      IACCMX(J)=0
      SIGSMX(J)=0.
  310 CONTINUE
      NMAX=0
      DO 370 IACC=1,NACC
      DO 320 J=1,30
      VINT(10+J)=VINTPT(IACC,J)
  320 CONTINUE
      IF(ISTSB.NE.5) THEN
        CALL PYSIGH(NCHN,SIGS)
        IF(MWTXS.EQ.1) THEN
          CALL PYEVWT(WTXS)
          SIGS=WTXS*SIGS
        ENDIF
      ELSE
        SIGS=0.
        DO 330 IKIN3=1,MSTP(129)
        CALL PYKMAP(5,0,0.)
        IF(MINT(51).EQ.1) GOTO 330
        CALL PYSIGH(NCHN,SIGTMP)
        IF(MWTXS.EQ.1) THEN
          CALL PYEVWT(WTXS)
          SIGTMP=WTXS*SIGTMP
        ENDIF
        IF(SIGTMP.GT.SIGS) SIGS=SIGTMP
  330   CONTINUE
      ENDIF
      IEQ=0
      DO 340 IMV=1,NMAX
      IF(ABS(SIGS-SIGSMX(IMV)).LT.1E-4*(SIGS+SIGSMX(IMV))) IEQ=IMV
  340 CONTINUE
      IF(IEQ.EQ.0) THEN
        DO 350 IMV=NMAX,1,-1
        IIN=IMV+1
        IF(SIGS.LE.SIGSMX(IMV)) GOTO 360
        IACCMX(IMV+1)=IACCMX(IMV)
        SIGSMX(IMV+1)=SIGSMX(IMV)
  350   CONTINUE
        IIN=1
  360   IACCMX(IIN)=IACC
        SIGSMX(IIN)=SIGS
        IF(NMAX.LE.1) NMAX=NMAX+1
      ENDIF
  370 CONTINUE
 
C...Read out starting position for search.
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5700)
      SIGSAM=SIGSMX(1)
      DO 420 IMAX=1,NMAX
      IACC=IACCMX(IMAX)
      MTAU=MVARPT(IACC,1)
      MTAUP=MVARPT(IACC,2)
      MYST=MVARPT(IACC,3)
      MCTH=MVARPT(IACC,4)
      VTAU=0.5
      VYST=0.5
      VCTH=0.5
      VTAUP=0.5
 
C...Starting point and step size in parameter space.
      DO 410 IRPT=1,2
      DO 400 IVAR=1,4
      IF(NPTS(IVAR).EQ.1) GOTO 400
      IF(IVAR.EQ.1) VVAR=VTAU
      IF(IVAR.EQ.2) VVAR=VTAUP
      IF(IVAR.EQ.3) VVAR=VYST
      IF(IVAR.EQ.4) VVAR=VCTH
      IF(IVAR.EQ.1) MVAR=MTAU
      IF(IVAR.EQ.2) MVAR=MTAUP
      IF(IVAR.EQ.3) MVAR=MYST
      IF(IVAR.EQ.4) MVAR=MCTH
      IF(IRPT.EQ.1) VDEL=0.1
      IF(IRPT.EQ.2) VDEL=MAX(0.01,MIN(0.05,VVAR-0.02,0.98-VVAR))
      IF(IRPT.EQ.1) VMAR=0.02
      IF(IRPT.EQ.2) VMAR=0.002
      IMOV0=1
      IF(IRPT.EQ.1.AND.IVAR.EQ.1) IMOV0=0
      DO 390 IMOV=IMOV0,8
 
C...Define new point in parameter space.
      IF(IMOV.EQ.0) THEN
        INEW=2
        VNEW=VVAR
      ELSEIF(IMOV.EQ.1) THEN
        INEW=3
        VNEW=VVAR+VDEL
      ELSEIF(IMOV.EQ.2) THEN
        INEW=1
        VNEW=VVAR-VDEL
      ELSEIF(SIGSSM(3).GE.MAX(SIGSSM(1),SIGSSM(2)).AND.
     &VVAR+2.*VDEL.LT.1.-VMAR) THEN
        VVAR=VVAR+VDEL
        SIGSSM(1)=SIGSSM(2)
        SIGSSM(2)=SIGSSM(3)
        INEW=3
        VNEW=VVAR+VDEL
      ELSEIF(SIGSSM(1).GE.MAX(SIGSSM(2),SIGSSM(3)).AND.
     &VVAR-2.*VDEL.GT.VMAR) THEN
        VVAR=VVAR-VDEL
        SIGSSM(3)=SIGSSM(2)
        SIGSSM(2)=SIGSSM(1)
        INEW=1
        VNEW=VVAR-VDEL
      ELSEIF(SIGSSM(3).GE.SIGSSM(1)) THEN
        VDEL=0.5*VDEL
        VVAR=VVAR+VDEL
        SIGSSM(1)=SIGSSM(2)
        INEW=2
        VNEW=VVAR
      ELSE
        VDEL=0.5*VDEL
        VVAR=VVAR-VDEL
        SIGSSM(3)=SIGSSM(2)
        INEW=2
        VNEW=VVAR
      ENDIF
 
C...Convert to relevant variables and find derived new limits.
      IF(IVAR.EQ.1) THEN
        VTAU=VNEW
        CALL PYKMAP(1,MTAU,VTAU)
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) CALL PYKLIM(4)
      ENDIF
      IF(IVAR.LE.2.AND.ISTSB.GE.3.AND.ISTSB.LE.5) THEN
        IF(IVAR.EQ.2) VTAUP=VNEW
        CALL PYKMAP(4,MTAUP,VTAUP)
      ENDIF
      IF(IVAR.LE.2) CALL PYKLIM(2)
      IF(IVAR.LE.3) THEN
        IF(IVAR.EQ.3) VYST=VNEW
        CALL PYKMAP(2,MYST,VYST)
        CALL PYKLIM(3)
      ENDIF
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) THEN
        IF(IVAR.EQ.4) VCTH=VNEW
        CALL PYKMAP(3,MCTH,VCTH)
      ENDIF
      IF(ISUB.EQ.96) VINT(25)=VINT(21)*(1.-VINT(23)**2)
 
C...Evaluate cross-section. Save new maximum. Final maximum.
      IF(ISTSB.NE.5) THEN
        CALL PYSIGH(NCHN,SIGS)
        IF(MWTXS.EQ.1) THEN
          CALL PYEVWT(WTXS)
          SIGS=WTXS*SIGS
        ENDIF
      ELSE
        SIGS=0.
        DO 380 IKIN3=1,MSTP(129)
        CALL PYKMAP(5,0,0.)
        IF(MINT(51).EQ.1) GOTO 380
        CALL PYSIGH(NCHN,SIGTMP)
        IF(MWTXS.EQ.1) THEN
          CALL PYEVWT(WTXS)
          SIGTMP=WTXS*SIGTMP
        ENDIF
        IF(SIGTMP.GT.SIGS) SIGS=SIGTMP
  380   CONTINUE
      ENDIF
      SIGSSM(INEW)=SIGS
      IF(SIGS.GT.SIGSAM) SIGSAM=SIGS
      IF(MSTP(122).GE.2) WRITE(MSTU(11),5800) IMAX,IVAR,MVAR,IMOV,
     &VNEW,VINT(21),VINT(22),VINT(23),VINT(26),SIGS
  390 CONTINUE
  400 CONTINUE
  410 CONTINUE
  420 CONTINUE
      IF(MSTP(121).EQ.1) SIGSAM=PARP(121)*SIGSAM
      XSEC(ISUB,1)=1.05*SIGSAM
  430 CONTINUE
      IF(MSTP(173).EQ.1.AND.ISUB.NE.96) XSEC(ISUB,1)=
     &PARP(174)*XSEC(ISUB,1)
      IF(ISUB.NE.96) XSEC(0,1)=XSEC(0,1)+XSEC(ISUB,1)
  440 CONTINUE
      MINT(51)=0
 
C...Print summary table.
      IF(NPOSI.EQ.0) THEN
        WRITE(MSTU(11),5900)
        STOP
      ENDIF
      IF(MSTP(122).GE.1) THEN
        WRITE(MSTU(11),6000)
        WRITE(MSTU(11),6100)
        DO 450 ISUB=1,200
        IF(MSUB(ISUB).NE.1.AND.ISUB.NE.96) GOTO 450
        IF(ISUB.EQ.96.AND.MINT(50).EQ.0) GOTO 450
        IF(ISUB.EQ.96.AND.MSUB(95).NE.1.AND.MSTP(81).LE.0) GOTO 450
        IF(ISUB.EQ.96.AND.MINT(49).EQ.0.AND.MSTP(131).EQ.0) GOTO 450
        IF(MSUB(95).EQ.1.AND.(ISUB.EQ.11.OR.ISUB.EQ.12.OR.ISUB.EQ.13.OR.
     &  ISUB.EQ.28.OR.ISUB.EQ.53.OR.ISUB.EQ.68)) GOTO 450
        WRITE(MSTU(11),6200) ISUB,PROC(ISUB),XSEC(ISUB,1)
  450   CONTINUE
        WRITE(MSTU(11),6300)
      ENDIF
 
C...Format statements for maximization results.
 5000 FORMAT(/1X,'Coefficient optimization and maximum search for ',
     &'subprocess no',I4/1X,'Coefficient modes     tau',10X,'y*',9X,
     &'cth',9X,'tau''',7X,'sigma')
 5100 FORMAT(1X,'Warning: requested subprocess ',I3,' has no allowed ',
     &'phase space.'/1X,'Process switched off!')
 5200 FORMAT(1X,4I4,F12.8,F12.6,F12.7,F12.8,1P,E12.4)
 5300 FORMAT(1X,'Warning: requested subprocess ',I3,' has vanishing ',
     &'cross-section.'/1X,'Process switched off!')
 5400 FORMAT(1X,'Coefficients of equation system to be solved for ',A4)
 5500 FORMAT(1X,1P,8E11.3)
 5600 FORMAT(1X,'Result for ',A4,':',7F9.4)
 5700 FORMAT(1X,'Maximum search for given coefficients'/2X,'MAX VAR ',
     &'MOD MOV   VNEW',7X,'tau',7X,'y*',8X,'cth',7X,'tau''',7X,'sigma')
 5800 FORMAT(1X,4I4,F8.4,F11.7,F9.3,F11.6,F11.7,1P,E12.4)
 5900 FORMAT(1X,'Error: no requested process has non-vanishing ',
     &'cross-section.'/1X,'Execution stopped!')
 6000 FORMAT(/1X,8('*'),1X,'PYMAXI: summary of differential ',
     &'cross-section maximum search',1X,8('*'))
 6100 FORMAT(/11X,58('=')/11X,'I',38X,'I',17X,'I'/11X,'I  ISUB  ',
     &'Subprocess name',15X,'I  Maximum value  I'/11X,'I',38X,'I',
     &17X,'I'/11X,58('=')/11X,'I',38X,'I',17X,'I')
 6200 FORMAT(11X,'I',2X,I3,3X,A28,2X,'I',2X,1P,E12.4,3X,'I')
 6300 FORMAT(11X,'I',38X,'I',17X,'I'/11X,58('='))
 
      RETURN
      END
