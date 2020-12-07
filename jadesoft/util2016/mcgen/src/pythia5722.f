CDECK  ID>, P57PAK.
      SUBROUTINE P57PAK (N,NLUPDM,P,K,NPLBUF,PLUTRK)
*.
*...P57PAK Reformat the Pythia57 event record for Gopal.
*.
*. INPUT     :  N       Number lines in input event record
*.           :  NLUPDM  First dimension of arrays P, K
*.           :  P       As P in LUJETS
*.           :  K       As K in LUJETS
*.           :  NPLBUF  First dimension of array PLUTRK
*. OUTPUT    :  N       Number of lines in modified event record
*.           :  P       Modified P array
*.           :  K       Modified K array
*.           :  PLUTRK  As P with inverted order
*.
*. AUTHOR    :  C.P.Ward
*. CREATED   :  12-May-94
*. LAST MOD  :   6-Jul-95
*.
*. Modification log
*.  6-Jul-95 D.Charlton  Change algorithm to handle funny
*.                         processes better...
*. 27-Jun-95 N.K.Watson  Use J74PAK code to avoid duplication.
*.  8-Jul-94 D.Hochman   Remove duplicate entries for H,A,etc.
*.
*.********************************************************************
      INTEGER  N,NLUPDM,NPLBUF,IP,IX,ITRY,IS,NB,NHR,ILINE,I,J,IPAR
      INTEGER  K (NLUPDM,*)
      REAL  P (NLUPDM,*),PLUTRK (NPLBUF,*)
      INTEGER IFBOS,IUCOMP,IORG
      INTEGER NUMBOS
      PARAMETER ( NUMBOS = 9 )
      INTEGER IDBOS (NUMBOS)
      SAVE IDBOS
      DATA  IDBOS / 23,24,25,32,33,34,35,36,37 /
*
*--- Count number of history records
*
      NHR=0
      DO 10 I=1,N
         IF(K(I,1).EQ.21) NHR = NHR + 1
 10   CONTINUE
*
*--- Reformat for interface to Gopal
*
*-- Remove following lines from record, resetting origin flags:
*--  a) first 6 lines (inital particles before/after i.s. radiation)
*--  b) any other particles in history part of record, except quarks and VBs
*--  c) second copies of VB records
*
      ILINE=1
      IORG=1
 100  IF (ILINE.GT.N) GOTO 200
*
*--- Keep this line if history record which is a VB or a quark
*
      IFBOS=IUCOMP(IABS(K(ILINE,2)),IDBOS,NUMBOS)
      IF(IORG.LE.NHR .AND. (IFBOS.GT.0.OR.IABS(K(ILINE,2)).LT.6)) THEN
        ILINE = ILINE + 1
        IORG=IORG+1
        GO TO 100
      ENDIF
*
*--- Keep this line if not a history record and not a VB
*
      IF (IORG.GT.NHR.AND.IFBOS.EQ.0) THEN
         ILINE=ILINE+1
         IORG=IORG+1
         GOTO 100
      ENDIF
*
*--- Remove this line
*
      N = N - 1
      IPAR = K(ILINE,3)
*
      DO 120 I=ILINE,N
         K(I,1) = K(I+1,1)
         K(I,2) = K(I+1,2)
         IF(K(I+1,3).LT.ILINE) THEN
            K(I,3) = K(I+1,3)
         ELSEIF(K(I+1,3).EQ.ILINE) THEN
            K(I,3) = IPAR
         ELSEIF(K(I+1,3).GT.ILINE) THEN
            K(I,3) = K(I+1,3) - 1
         ENDIF
         DO 110 J=1,5
            P(I,J) = P(I+1,J)
C     V(I,J) = V(I+1,J)
 110     CONTINUE
 120  CONTINUE
*
      IORG=IORG+1
      GOTO 100
*
 200  CONTINUE
*
      CALL J74PAK(N,NLUPDM,P,K,NPLBUF,PLUTRK)
 
 990  RETURN
      END
CDECK  ID>, PYKERN.
C*********************************************************************
C*********************************************************************
C*                                                                  **
C*                                                 December 1993    **
C*                                                                  **
C*           The Lund Monte Carlo for Hadronic Processes            **
C*                                                                  **
C*                        PYTHIA version 5.7                        **
C*                                                                  **
C*                        Torbjorn Sjostrand                        **
C*                Department of theoretical physics 2               **
C*                        University of Lund                        **
C*               Solvegatan 14A, S-223 62 Lund, Sweden              **
C*                    E-mail torbjorn@thep.lu.se                    **
C*                    phone +46 - 46 - 222 48 16                    ** 
C*                                                                  **
C*         Several parts are written by Hans-Uno Bengtsson          **
C*     CTEQ 2 parton distributions are by the CTEQ collaboration    **
C*   SaS photon parton distributions together with Gerhard Schuler  **
C*    g + g -> Z + b + bbar matrix element code by Ronald Kleiss    **
C*     g + g and q + qbar -> t + tbar + H code by Zoltan Kunszt     **
C*                                                                  **
C*   The latest program version and documentation is found on WWW   **
C*         http://thep.lu.se/tf2/staff/torbjorn/Welcome.html        **
C*                                                                  **
C*        Copyright Torbjorn Sjostrand and CERN, Geneva 1993        **
C*                                                                  **
C*********************************************************************
C*********************************************************************
C                                                                    *
C  List of subprograms in order of appearance, with main purpose     *
C  (S = subroutine, F = function, B = block data)                    *
C                                                                    *
C  S   PYINIT   to administer the initialization procedure           *
C  S   PYEVNT   to administer the generation of an event             *
C  S   PYSTAT   to print cross-section and other information         *
C  S   PYINRE   to initialize treatment of resonances                *
C  S   PYINBM   to read in beam, target and frame choices            *
C  S   PYINKI   to initialize kinematics of incoming particles       *
C  S   PYINPR   to set up the selection of included processes        *
C  S   PYXTOT   to give total, elastic and diffractive cross-sect.   *
C  S   PYMAXI   to find differential cross-section maxima            *
C  S   PYPILE   to select multiplicity of pileup events              *
C  S   PYSAVE   to save alternatives for gamma-p and gamma-gamma     *
C  S   PYRAND   to select subprocess and kinematics for event        *
C  S   PYSCAT   to set up kinematics and colour flow of event        *
C  S   PYSSPA   to simulate initial state spacelike showers          *
C  S   PYRESD   to perform resonance decays                          *
C  S   PYMULT   to generate multiple interactions                    *
C  S   PYREMN   to add on target remnants                            *
C  S   PYDIFF   to set up kinematics for diffractive events          *
C  S   PYDOCU   to compute cross-sections and handle documentation   *
C  S   PYFRAM   to perform boosts between different frames           *
C  S   PYWIDT   to calculate full and partial widths of resonances   *
C  S   PYOFSH   to calculate partial width into off-shell channels   *
C  S   PYKLIM   to calculate borders of allowed kinematical region   *
C  S   PYKMAP   to construct value of kinematical variable           *
C  S   PYSIGH   to calculate differential cross-sections             *
C  S   PYSTFU   to evaluate structure functions                      *
C  S   PYSTFL   to evaluate structure functions at low x and Q^2     *
C  S   PYSTEL   to evaluate electron structure function              *
C  S   PYSTGA   to evaluate photon structure function (generic)      *
C  S   PYGGAM   to evaluate photon structure function (SaS sets)     *
C  S   PYGVMD   to evaluate VMD part of photon structure functions   *
C  S   PYGANO   to evaluate anomalous part of photon str. func.      *
C  S   PYGBEH   to evaluate Bethe-Heitler part of photon str. func.  *
C  S   PYGDIR   to evaluate direct contribution to photon str. func. *
C  S   PYSTPI   to evaluate pion structure function                  *
C  S   PYSTPR   to evaluate proton structure function                *
C  F   PYCTQ2   to evaluate the CTEQ 2 proton structure function     *
C  F   PYHFTH   to evaluate threshold factor for heavy flavour       *
C  S   PYSPLI   to find flavours left in hadron when one removed     *
C  F   PYGAMM   to evaluate ordinary Gamma function Gamma(x)         *
C  S   PYWAUX   to evaluate auxiliary functions W1(s) and W2(s)      *
C  S   PYI3AU   to evaluate auxiliary function I3(s,t,u,v)           *
C  F   PYSPEN   to evaluate Spence (dilogarithm) function Sp(x)      *
C  S   PYQQBH   to evaluate matrix element for g + g -> Q + Q~ + H   *
C  S   PYTEST   to test the proper functioning of the package        *
C  B   PYDATA   to contain all default values                        *
C  S   PYKCUT   to provide dummy routine for user kinematical cuts   *
C  S   PYEVWT   to provide dummy routine for weighting events        *
C  S   PYUPIN   to initialize a user process                         *
C  S   PYUPEV   to generate a user process event (dummy routine)     *
C  S   PDFSET   dummy routine to be removed when using PDFLIB        *
C  S   STRUCTM  dummy routine to be removed when using PDFLIB        *
C                                                                    *
C*********************************************************************
 
      SUBROUTINE PYINIT(FRAME,BEAM,TARGET,WIN)
 
C...Initializes the generation procedure; finds maxima of the
C...differential cross-sections to be used for weighting.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
      COMMON/GUDAT4/CHAF(500)
      CHARACTER CHAF*8
      SAVE /GUDAT4/
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
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYINT9/DXSEC(0:200)
      DOUBLE PRECISION DXSEC
      SAVE /PYINT9/
      DIMENSION ALAMIN(20),NFIN(20)
      CHARACTER*(*) FRAME,BEAM,TARGET
      CHARACTER CHFRAM*8,CHBEAM*8,CHTARG*8,CHLH(2)*6
 
C...Interface to PDFLIB.
      COMMON/W50512/QCDL4,QCDL5
      SAVE /W50512/
      DOUBLE PRECISION VALUE(20),QCDL4,QCDL5
      CHARACTER*20 PARM(20)
      DATA VALUE/20*0D0/,PARM/20*' '/
 
C...Data:Lambda and n_f values for structure functions; months.
      DATA ALAMIN/0.20,0.29,0.20,0.40,0.213,0.208,0.208,0.322,
     &0.190,0.235,10*0.2/,NFIN/20*4/
      DATA CHLH/'lepton','hadron'/
 
C...Reset MINT and VINT arrays. Write headers.
      DO 100 J=1,400
      MINT(J)=0
      VINT(J)=0.
  100 CONTINUE
      IF(MSTU(12).GE.1) CALL LULIST(0)
      IF(MSTP(122).GE.1) WRITE(MSTU(11),5100)
 
C...Maximum 4 generations; set maximum number of allowed flavours.
      MSTP(1)=MIN(4,MSTP(1))
      MSTU(114)=MIN(MSTU(114),2*MSTP(1))
      MSTP(58)=MIN(MSTP(58),2*MSTP(1))
 
C...Sum up Cabibbo-Kobayashi-Maskawa factors for each quark/lepton.
      DO 120 I=-20,20
      VINT(180+I)=0.
      IA=IABS(I)
      IF(IA.GE.1.AND.IA.LE.2*MSTP(1)) THEN
        DO 110 J=1,MSTP(1)
        IB=2*J-1+MOD(IA,2)
        IPM=(5-ISIGN(1,I))/2
        IDC=J+MDCY(IA,2)+2
        IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.IPM) VINT(180+I)=
     &  VINT(180+I)+VCKM((IA+1)/2,(IB+1)/2)
  110   CONTINUE
      ELSEIF(IA.GE.11.AND.IA.LE.10+2*MSTP(1)) THEN
        VINT(180+I)=1.
      ENDIF
  120 CONTINUE
 
C...Initialize structure functions: PDFLIB.
      IF(MSTP(52).EQ.2) THEN
        PARM(1)='NPTYPE'
        VALUE(1)=1
        PARM(2)='NGROUP'
        VALUE(2)=MSTP(51)/1000
        PARM(3)='NSET'
        VALUE(3)=MOD(MSTP(51),1000)
        PARM(4)='TMAS'
        VALUE(4)=PMAS(6,1)
        CALL PDFSET(PARM,VALUE)
        MINT(93)=1000000+MSTP(51)
      ENDIF
 
C...Choose Lambda value to use in alpha-strong.
      MSTU(111)=MSTP(2)
      IF(MSTP(3).GE.2) THEN
        ALAM=0.2
        NF=4
        IF(MSTP(52).EQ.1.AND.MSTP(51).GE.1.AND.MSTP(51).LE.10) THEN
          ALAM=ALAMIN(MSTP(51))
          NF=NFIN(MSTP(51))
        ELSEIF(MSTP(52).EQ.2) THEN
          ALAM=QCDL4
          NF=4
        ENDIF
        PARP(1)=ALAM
        PARP(61)=ALAM
        PARP(72)=ALAM
        PARU(112)=ALAM
        MSTU(112)=NF
        IF(MSTP(3).EQ.3) PARJ(81)=ALAM      
      ENDIF
 
C...Initialize widths and partial widths for resonances.
      CALL PYINRE
 
C...Identify beam and target particles and frame of process.
      CHFRAM=FRAME//' '
      CHBEAM=BEAM//' '
      CHTARG=TARGET//' '
      CALL PYINBM(CHFRAM,CHBEAM,CHTARG,WIN)
      IF(MINT(65).EQ.1) GOTO 170
 
C...For gamma-p or gamma-gamma allow many (3 or 6) alternatives.
C...For e-gamma allow 2 alternatives.
      MINT(121)=1
      MINT(123)=MSTP(14)
      IF(MSTP(14).EQ.10.AND.(MSEL.EQ.1.OR.MSEL.EQ.2)) THEN
        IF((MINT(11).EQ.22.OR.MINT(12).EQ.22).AND.
     &  (IABS(MINT(11)).GE.28.OR.IABS(MINT(12)).GE.28)) MINT(121)=3
        IF(MINT(11).EQ.22.AND.MINT(12).EQ.22) MINT(121)=6
        IF((MINT(11).EQ.22.OR.MINT(12).EQ.22).AND.
     &  (IABS(MINT(11)).EQ.11.OR.IABS(MINT(12)).EQ.11)) MINT(121)=2
      ENDIF
 
C...Set up kinematics of process.
      CALL PYINKI(0)
 
C...Loop over gamma-p or gamma-gamma alternatives.
      DO 160 IGA=1,MINT(121)
      MINT(122)=IGA
 
C...Select partonic subprocesses to be included in the simulation.
      CALL PYINPR
 
C...Count number of subprocesses on.
      MINT(48)=0
      DO 130 ISUB=1,200
      IF(MINT(50).EQ.0.AND.ISUB.GE.91.AND.ISUB.LE.96.AND.
     &MSUB(ISUB).EQ.1) THEN
        WRITE(MSTU(11),5200) ISUB,CHLH(MINT(41)),CHLH(MINT(42))
        STOP
      ELSEIF(MSUB(ISUB).EQ.1.AND.ISET(ISUB).EQ.-1) THEN
        WRITE(MSTU(11),5300) ISUB
        STOP
      ELSEIF(MSUB(ISUB).EQ.1.AND.ISET(ISUB).LE.-2) THEN
        WRITE(MSTU(11),5400) ISUB
        STOP
      ELSEIF(MSUB(ISUB).EQ.1) THEN
        MINT(48)=MINT(48)+1
      ENDIF
  130 CONTINUE
      IF(MINT(48).EQ.0) THEN
        WRITE(MSTU(11),5500)
        STOP
      ENDIF
      MINT(49)=MINT(48)-MSUB(91)-MSUB(92)-MSUB(93)-MSUB(94)
 
C...Reset variables for cross-section calculation.
      DO 150 I=0,200
      DO 140 J=1,3
      NGEN(I,J)=0
      XSEC(I,J)=0.
  140 CONTINUE
      DXSEC(I)=0D0
  150 CONTINUE
 
C...Find parametrized total cross-sections.
      CALL PYXTOT
 
C...Maxima of differential cross-sections.
      IF(MSTP(121).LE.1) CALL PYMAXI
 
C...Initialize possibility of pileup events.
      IF(MINT(121).GT.1) MSTP(131)=0
      IF(MSTP(131).NE.0) CALL PYPILE(1)
 
C...Initialize multiple interactions with variable impact parameter.
      IF(MINT(50).EQ.1.AND.(MINT(49).NE.0.OR.MSTP(131).NE.0).AND.
     &MSTP(82).GE.2) CALL PYMULT(1)
 
C...Save results for gamma-p and gamma-gamma alternatives.
      IF(MINT(121).GT.1) CALL PYSAVE(1,IGA)
  160 CONTINUE
 
C...Initialization finished.
  170 IF(MSTP(122).GE.1) WRITE(MSTU(11),5600)
 
C...Formats for initialization information.
 5100 FORMAT('1',18('*'),1X,'PYINIT: initialization of PYTHIA ',
     &'routines',1X,17('*'))
 5200 FORMAT(1X,'Error: process number ',I3,' not meaningful for ',A6,
     &'-',A6,' interactions.'/1X,'Execution stopped!')
 5300 FORMAT(1X,'Error: requested subprocess',I4,' not implemented.'/
     &1X,'Execution stopped!')
 5400 FORMAT(1X,'Error: requested subprocess',I4,' not existing.'/
     &1X,'Execution stopped!')
 5500 FORMAT(1X,'Error: no subprocess switched on.'/
     &1X,'Execution stopped.')
 5600 FORMAT(/1X,22('*'),1X,'PYINIT: initialization completed',1X,
     &22('*'))
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYEVNT
 
C...Administers the generation of a high-pT event via calls to
C...a number of subroutines.
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
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYUPPR/NUP,KUP(20,7),PUP(20,5),NFUP,IFUP(10,2),Q2UP(0:10)
      INTEGER  NUP,KUP,NFUP,IFUP
      REAL  PUP,Q2UP
      SAVE /PYUPPR/
      DIMENSION VTX(4)
 
C...Initial values for some counters.
      N=0
      MINT(5)=MINT(5)+1
      MINT(7)=0
      MINT(8)=0
      MINT(83)=0
      MINT(84)=MSTP(126)
      MSTU(24)=0
      MSTU70=0
      MSTJ14=MSTJ(14)
 
C...If variable energies: redo incoming kinematics and cross-section.
      MSTI(61)=0
      IF(MSTP(171).EQ.1) THEN
        CALL PYINKI(1)
        IF(MSTI(61).EQ.1) THEN
          MINT(5)=MINT(5)-1
          RETURN
        ENDIF
        IF(MINT(121).GT.1) CALL PYSAVE(3,1)
        CALL PYXTOT
      ENDIF
 
C...Loop over number of pileup events; check space left.
      IF(MSTP(131).LE.0) THEN
        NPILE=1
      ELSE
        CALL PYPILE(2)
        NPILE=MINT(81)
      ENDIF
      DO 250 IPILE=1,NPILE
      IF(MINT(84)+100.GE.MSTU(4)) THEN
        CALL LUERRM(11,
     &  '(PYEVNT:) no more space in LUJETS for pileup events')
        IF(MSTU(21).GE.1) GOTO 260
      ENDIF
      MINT(82)=IPILE
 
C...Generate variables of hard scattering.
      MINT(51)=0
      MSTI(52)=0
  100 CONTINUE
      IF(MINT(51).NE.0.OR.MSTU(24).NE.0) MSTI(52)=MSTI(52)+1
      MINT(31)=0
      MINT(51)=0
      MINT(57)=0
      CALL PYRAND
*DGC
*1/4/96 Added next 3 lines following two-photon loop trap
      IF (MSTI(191).GE.1000) THEN
         RETURN
      ENDIF
*DGC End
      IF(MSTI(61).EQ.1) THEN
        MINT(5)=MINT(5)-1
        RETURN
      ENDIF
      IF(MINT(51).EQ.2) RETURN
      ISUB=MINT(1)
      IF(MSTP(111).EQ.-1) GOTO 240
 
      IF(ISUB.LE.90.OR.ISUB.GE.95) THEN
C...Hard scattering (including low-pT):
C...reconstruct kinematics and colour flow of hard scattering.
  110   MINT(51)=0
        CALL PYSCAT
        IF(MINT(51).EQ.1) GOTO 100
        IPU1=MINT(84)+1
        IPU2=MINT(84)+2
        IF(ISUB.EQ.95) GOTO 130
 
C...Showering of initial state partons (optional).
        ALAMSV=PARJ(81)
        PARJ(81)=PARP(72)
        IF(MSTP(61).GE.1.AND.MINT(47).GE.2) CALL PYSSPA(IPU1,IPU2)
        PARJ(81)=ALAMSV
        IF(MINT(51).EQ.1) GOTO 100
 
C...Showering of final state partons (optional).
        ALAMSV=PARJ(81)
        PARJ(81)=PARP(72)
        IF(MSTP(71).GE.1.AND.ISET(ISUB).GE.2.AND.ISET(ISUB).LE.10) THEN
          IPU3=MINT(84)+3
          IPU4=MINT(84)+4
          IF(ISET(ISUB).EQ.5.OR.ISET(ISUB).EQ.6) IPU4=-3
          QMAX=VINT(55)
          IF(ISET(ISUB).EQ.2) QMAX=SQRT(PARP(71))*VINT(55)
          CALL LUSHOW(IPU3,IPU4,QMAX)
        ELSEIF(MSTP(71).GE.1.AND.ISET(ISUB).EQ.11.AND.NFUP.GE.1) THEN
          DO 120 IUP=1,NFUP
          IPU3=IFUP(IUP,1)+MINT(84)
          IPU4=IFUP(IUP,2)+MINT(84)
          QMAX=SQRT(MAX(0.,Q2UP(IUP)))
          CALL LUSHOW(IPU3,IPU4,QMAX)
  120     CONTINUE
        ENDIF
        PARJ(81)=ALAMSV
 
C...Decay of final state resonances.
        IF(MSTP(41).GE.1.AND.ISET(ISUB).LE.10) CALL PYRESD
        IF(MINT(51).EQ.1) GOTO 100
        MINT(52)=N
 
C...Multiple interactions.
        IF(MSTP(81).GE.1.AND.MINT(50).EQ.1) CALL PYMULT(6)
        MINT(53)=N
 
C...Hadron remnants and primordial kT.
  130   CALL PYREMN(IPU1,IPU2)
        IF(MINT(51).EQ.1.AND.MINT(57).GE.1.AND.MINT(57).LE.5) GOTO 110
        IF(MINT(51).EQ.1) GOTO 100
 
      ELSE
C...Diffractive and elastic scattering.
        CALL PYDIFF
      ENDIF
 
C...Recalculate energies from momenta and masses (if desired).
      IF(MSTP(113).GE.1) THEN
        DO 140 I=MINT(83)+1,N
        IF(K(I,1).GT.0.AND.K(I,1).LE.10) P(I,4)=SQRT(P(I,1)**2+
     &  P(I,2)**2+P(I,3)**2+P(I,5)**2)
  140   CONTINUE
        NRECAL=N
      ENDIF
 
C...Rearrange partons along strings, check invariant mass cuts.
      MSTU(28)=0
      IF(MSTP(111).LE.0) MSTJ(14)=-1
      CALL LUPREP(MINT(84)+1)
      MSTJ(14)=MSTJ14
      IF(MSTP(112).EQ.1.AND.MSTU(28).EQ.3) GOTO 100
      IF(MSTP(125).EQ.0.OR.MSTP(125).EQ.1) THEN
        DO 170 I=MINT(84)+1,N
        IF(K(I,2).EQ.94) THEN
          DO 160 I1=I+1,MIN(N,I+3)
          IF(K(I1,3).EQ.I) THEN
            K(I1,3)=MOD(K(I1,4)/MSTU(5),MSTU(5))
            IF(K(I1,3).EQ.0) THEN
              DO 150 II=MINT(84)+1,I-1
              IF(K(II,2).EQ.K(I1,2)) THEN
                IF(MOD(K(II,4),MSTU(5)).EQ.I1.OR.
     &          MOD(K(II,5),MSTU(5)).EQ.I1) K(I1,3)=II
              ENDIF
  150         CONTINUE
              IF(K(I+1,3).EQ.0) K(I+1,3)=K(I,3)
            ENDIF
          ENDIF
  160     CONTINUE
        ENDIF
  170   CONTINUE
        CALL LUEDIT(12)
        CALL LUEDIT(14)
        IF(MSTP(125).EQ.0) CALL LUEDIT(15)
        IF(MSTP(125).EQ.0) MINT(4)=0
        DO 190 I=MINT(83)+1,N
        IF(K(I,1).EQ.11.AND.K(I,4).EQ.0.AND.K(I,5).EQ.0) THEN
          DO 180 I1=I+1,N
          IF(K(I1,3).EQ.I.AND.K(I,4).EQ.0) K(I,4)=I1
          IF(K(I1,3).EQ.I) K(I,5)=I1
  180     CONTINUE
        ENDIF
  190   CONTINUE
      ENDIF
 
C...Introduce separators between sections in LULIST event listing.
      IF(IPILE.EQ.1.AND.MSTP(125).LE.0) THEN
        MSTU70=1
        MSTU(71)=N
      ELSEIF(IPILE.EQ.1) THEN
        MSTU70=3
        MSTU(71)=2
        MSTU(72)=MINT(4)
        MSTU(73)=N
      ENDIF
 
C...Go back to lab frame (needed for vertices, also in fragmentation).
      CALL PYFRAM(1)
 
C...Set nonvanishing production vertex (optional).
      IF(MSTP(151).EQ.1) THEN
        DO 200 J=1,4
        VTX(J)=PARP(150+J)*SQRT(-2.*LOG(MAX(1E-10,RLU(0))))*
     &  SIN(PARU(2)*RLU(0))
  200   CONTINUE
        DO 220 I=MINT(83)+1,N
        DO 210 J=1,4
        V(I,J)=V(I,J)+VTX(J)
  210   CONTINUE
  220   CONTINUE
      ENDIF
 
C...Perform hadronization (if desired).
      IF(MSTP(111).GE.1) THEN
        CALL LUEXEC
        IF(MSTU(24).NE.0) GOTO 100
      ENDIF
      IF(MSTP(113).GE.1) THEN
        DO 230 I=NRECAL,N
        IF(P(I,5).GT.0.) P(I,4)=SQRT(P(I,1)**2+
     &  P(I,2)**2+P(I,3)**2+P(I,5)**2)
  230   CONTINUE
      ENDIF
      IF(MSTP(125).EQ.0.OR.MSTP(125).EQ.1) CALL LUEDIT(14)
 
C...Store event information and calculate Monte Carlo estimates of
C...subprocess cross-sections.
  240 IF(IPILE.EQ.1) CALL PYDOCU
 
C...Set counters for current pileup event and loop to next one.
      MSTI(41)=IPILE
      IF(IPILE.GE.2.AND.IPILE.LE.10) MSTI(40+IPILE)=ISUB
      IF(MSTU70.LT.10) THEN
        MSTU70=MSTU70+1
        MSTU(70+MSTU70)=N
      ENDIF
      MINT(83)=N
      MINT(84)=N+MSTP(126)
      IF(IPILE.LT.NPILE) CALL PYFRAM(2)
  250 CONTINUE
 
C...Generic information on pileup events. Reconstruct missing history.
      IF(MSTP(131).EQ.1.AND.MSTP(133).GE.1) THEN
        PARI(91)=VINT(132)
        PARI(92)=VINT(133)
        PARI(93)=VINT(134)
        IF(MSTP(133).GE.2) PARI(93)=PARI(93)*XSEC(0,3)/VINT(131)
      ENDIF
      CALL LUEDIT(16)
 
C...Transform to the desired coordinate frame.
  260 CALL PYFRAM(MSTP(124))
      MSTU(70)=MSTU70
      PARU(21)=VINT(1)
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYSTAT(MSTAT)
 
C...Prints out information about cross-sections, decay widths, branching
C...ratios, kinematical limits, status codes and parameter values.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
      CHARACTER PROGA(6)*28,CHAU*16,CHPA(-100:100)*9,CHIN(2)*12,
     &STATE(-1:5)*4,CHKIN(21)*18,DISGA(2)*28
      DATA PROGA/
     &'VMD/hadron * VMD            ','VMD/hadron * direct         ',
     &'VMD/hadron * anomalous      ','direct * direct             ',
     &'direct * anomalous          ','anomalous * anomalous       '/
      DATA DISGA/'e * VMD','e * anomalous'/
      DATA STATE/'----','off ','on  ','on/+','on/-','on/1','on/2'/,
     &CHKIN/' m_hard (GeV/c^2) ',' p_T_hard (GeV/c) ',
     &'m_finite (GeV/c^2)','   y*_subsystem   ','     y*_large     ',
     &'     y*_small     ','    eta*_large    ','    eta*_small    ',
     &'cos(theta*)_large ','cos(theta*)_small ','       x_1        ',
     &'       x_2        ','       x_F        ',' cos(theta_hard)  ',
     &'m''_hard (GeV/c^2) ','       tau        ','        y*        ',
     &'cos(theta_hard^-) ','cos(theta_hard^+) ','      x_T^2       ',
     &'       tau''       '/
 
C...Cross-sections.
      IF(MSTAT.LE.1) THEN
        IF(MINT(121).GT.1) CALL PYSAVE(5,0)
        WRITE(MSTU(11),5000)
        WRITE(MSTU(11),5100)
        WRITE(MSTU(11),5200) 0,PROC(0),NGEN(0,3),NGEN(0,1),XSEC(0,3)
        DO 100 I=1,200
        IF(MSUB(I).NE.1) GOTO 100
        WRITE(MSTU(11),5200) I,PROC(I),NGEN(I,3),NGEN(I,1),XSEC(I,3)
  100   CONTINUE
        IF(MINT(121).GT.1) THEN
          WRITE(MSTU(11),5300)
          DO 110 IGA=1,MINT(121)
          CALL PYSAVE(3,IGA)
          IF(MINT(121).EQ.2) THEN
            WRITE(MSTU(11),5200) IGA,DISGA(IGA),NGEN(0,3),NGEN(0,1),
     &      XSEC(0,3)
          ELSE
            WRITE(MSTU(11),5200) IGA,PROGA(IGA),NGEN(0,3),NGEN(0,1),
     &      XSEC(0,3)
          ENDIF
  110     CONTINUE
          CALL PYSAVE(5,0)
        ENDIF
        WRITE(MSTU(11),5400) 1.-FLOAT(NGEN(0,3))/
     &  MAX(1.,FLOAT(NGEN(0,2)))
 
C...Decay widths and branching ratios.
      ELSEIF(MSTAT.EQ.2) THEN
        DO 120 KF=-100,100
        CALL LUNAME(KF,CHAU)
        CHPA(KF)=CHAU(1:9)
  120   CONTINUE
        WRITE(MSTU(11),5500)
        WRITE(MSTU(11),5600)
        DO 150 KC=1,40
        KCL=KC
        IF(KC.GE.6.AND.KC.LE.8) KCL=KC+20
        IF(KC.EQ.17.OR.KC.EQ.18) KCL=KC+12
        IF(MSTP(6).NE.1) THEN
          IF(KC.GT.2*MSTP(1).AND.KC.LE.10) GOTO 150
          IF(KC.GT.10+2*MSTP(1).AND.KC.LE.20) GOTO 150
        ELSE
          IF(KC.GT.8.AND.KC.LE.10) GOTO 150
          IF(KC.GT.18.AND.KC.LE.20) GOTO 150
        ENDIF
        IF((KC.GE.26.AND.KC.LE.31).OR.KC.EQ.33) GOTO 150
        IOFF=0
        IF(KC.LE.22) IOFF=1
        IF(KC.EQ.6.AND.MSTP(48).GE.1) IOFF=0
        IF((KC.EQ.7.OR.KC.EQ.8.OR.KC.EQ.17.OR.KC.EQ.18).AND.
     &  (MSTP(6).EQ.1.OR.MSTP(49).GE.1)) IOFF=0
        IF(KC.EQ.18.AND.PMAS(18,1).LT.1.) IOFF=1
C...Off-shell branchings.
        IF(IOFF.EQ.1) THEN
          NGP=0
          IF(KC.LE.20) NGP=(MOD(KC,10)+1)/2
          IF(NGP.LE.MSTP(1)) WRITE(MSTU(11),5700) KC,CHPA(KC),
     &    PMAS(KC,1),0.,0.,STATE(MDCY(KC,1)),0.
          DO 130 J=1,MDCY(KC,3)
          IDC=J+MDCY(KC,2)-1
          NGP1=0
          IF(IABS(KFDP(IDC,1)).LE.20) NGP1=
     &    (MOD(IABS(KFDP(IDC,1)),10)+1)/2
          NGP2=0
          IF(IABS(KFDP(IDC,2)).LE.20) NGP2=
     &    (MOD(IABS(KFDP(IDC,2)),10)+1)/2
          IF(MDME(IDC,2).EQ.102.AND.NGP1.LE.MSTP(1).AND.NGP2.LE.MSTP(1))
     &    WRITE(MSTU(11),5800) IDC,CHPA(KFDP(IDC,1)),CHPA(KFDP(IDC,2)),
     &    0.,0.,STATE(MDME(IDC,1)),0.
  130     CONTINUE
C...On-shell decays.
        ELSE
          BRFIN=1.
          IF(WIDE(KCL,0).LE.0.) BRFIN=0.
          WRITE(MSTU(11),5700) KC,CHPA(KC),PMAS(KC,1),WIDP(KCL,0),1.,
     &    STATE(MDCY(KC,1)),BRFIN
          DO 140 J=1,MDCY(KC,3)
          IDC=J+MDCY(KC,2)-1
          NGP1=0
          IF(IABS(KFDP(IDC,1)).LE.20) NGP1=
     &    (MOD(IABS(KFDP(IDC,1)),10)+1)/2
          NGP2=0
          IF(IABS(KFDP(IDC,2)).LE.20) NGP2=
     &    (MOD(IABS(KFDP(IDC,2)),10)+1)/2
          BRFIN=0.
          IF(WIDE(KCL,0).GT.0.) BRFIN=WIDE(KCL,J)/WIDE(KCL,0)
          IF(NGP1.LE.MSTP(1).AND.NGP2.LE.MSTP(1)) WRITE(MSTU(11),5800)
     &    IDC,CHPA(KFDP(IDC,1)),CHPA(KFDP(IDC,2)),WIDP(KCL,J),
     &    WIDP(KCL,J)/WIDP(KCL,0),STATE(MDME(IDC,1)),BRFIN
  140     CONTINUE
        ENDIF
  150   CONTINUE
        WRITE(MSTU(11),5900)
 
C...Allowed incoming partons/particles at hard interaction.
      ELSEIF(MSTAT.EQ.3) THEN
        WRITE(MSTU(11),6000)
        CALL LUNAME(MINT(11),CHAU)
        CHIN(1)=CHAU(1:12)
        CALL LUNAME(MINT(12),CHAU)
        CHIN(2)=CHAU(1:12)
        WRITE(MSTU(11),6100) CHIN(1),CHIN(2)
        DO 160 KF=-40,40
        CALL LUNAME(KF,CHAU)
        CHPA(KF)=CHAU(1:9)
  160   CONTINUE
        DO 170 I=-20,22
        IF(I.EQ.0) GOTO 170
        IA=IABS(I)
        IF(IA.GT.MSTP(58).AND.IA.LE.10) GOTO 170
        IF(IA.GT.10+2*MSTP(1).AND.IA.LE.20) GOTO 170
        WRITE(MSTU(11),6200) CHPA(I),STATE(KFIN(1,I)),CHPA(I),
     &  STATE(KFIN(2,I))
  170   CONTINUE
        WRITE(MSTU(11),6300)
 
C...User-defined limits on kinematical variables.
      ELSEIF(MSTAT.EQ.4) THEN
        WRITE(MSTU(11),6400)
        WRITE(MSTU(11),6500)
        SHRMAX=CKIN(2)
        IF(SHRMAX.LT.0.) SHRMAX=VINT(1)
        WRITE(MSTU(11),6600) CKIN(1),CHKIN(1),SHRMAX
        PTHMIN=MAX(CKIN(3),CKIN(5))
        PTHMAX=CKIN(4)
        IF(PTHMAX.LT.0.) PTHMAX=0.5*SHRMAX
        WRITE(MSTU(11),6700) CKIN(3),PTHMIN,CHKIN(2),PTHMAX
        WRITE(MSTU(11),6800) CHKIN(3),CKIN(6)
        DO 180 I=4,14
        WRITE(MSTU(11),6600) CKIN(2*I-1),CHKIN(I),CKIN(2*I)
  180   CONTINUE
        SPRMAX=CKIN(32)
        IF(SPRMAX.LT.0.) SPRMAX=VINT(1)
        WRITE(MSTU(11),6600) CKIN(31),CHKIN(15),SPRMAX
        WRITE(MSTU(11),6900)
 
C...Status codes and parameter values.
      ELSEIF(MSTAT.EQ.5) THEN
        WRITE(MSTU(11),7000)
        WRITE(MSTU(11),7100)
        DO 190 I=1,100
        WRITE(MSTU(11),7200) I,MSTP(I),PARP(I),100+I,MSTP(100+I),
     &  PARP(100+I)
  190   CONTINUE
      ENDIF
 
C...Formats for printouts.
 5000 FORMAT('1',9('*'),1X,'PYSTAT:  Statistics on Number of ',
     &'Events and Cross-sections',1X,9('*'))
 5100 FORMAT(/1X,78('=')/1X,'I',34X,'I',28X,'I',12X,'I'/1X,'I',12X,
     &'Subprocess',12X,'I',6X,'Number of points',6X,'I',4X,'Sigma',3X,
     &'I'/1X,'I',34X,'I',28X,'I',12X,'I'/1X,'I',34('-'),'I',28('-'),
     &'I',4X,'(mb)',4X,'I'/1X,'I',34X,'I',28X,'I',12X,'I'/1X,'I',1X,
     &'N:o',1X,'Type',25X,'I',4X,'Generated',9X,'Tried',1X,'I',12X,
     &'I'/1X,'I',34X,'I',28X,'I',12X,'I'/1X,78('=')/1X,'I',34X,'I',28X,
     &'I',12X,'I')
 5200 FORMAT(1X,'I',1X,I3,1X,A28,1X,'I',1X,I12,1X,I13,1X,'I',1X,1P,
     &E10.3,1X,'I')
 5300 FORMAT(1X,'I',34X,'I',28X,'I',12X,'I'/1X,78('=')/
     &1X,'I',34X,'I',28X,'I',12X,'I')
 5400 FORMAT(1X,'I',34X,'I',28X,'I',12X,'I'/1X,78('=')//
     &1X,'********* Fraction of events that fail fragmentation ',
     &'cuts =',1X,F8.5,' *********'/)
 5500 FORMAT('1',17('*'),1X,'PYSTAT:  Decay Widths and Branching ',
     &'Ratios',1X,17('*'))
 5600 FORMAT(/1X,78('=')/1X,'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/
     &1X,'I',1X,'Branching/Decay Channel',5X,'I',1X,'Width (GeV)',1X,
     &'I',7X,'B.R.',1X,'I',1X,'Stat',1X,'I',2X,'Eff. B.R.',1X,'I'/1X,
     &'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/1X,78('='))
 5700 FORMAT(1X,'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/1X,'I',1X,
     &I4,1X,A9,'(',1P,E8.2,0P,')',1X,'->',1X,'I',2X,1P,E10.3,0P,1X,
     &'I',1X,1P,E10.3,0P,1X,'I',1X,A4,1X,'I',1X,1P,E10.3,0P,1X,'I')
 5800 FORMAT(1X,'I',1X,I4,1X,A9,1X,'+',1X,A9,2X,'I',2X,1P,E10.3,0P,
     &1X,'I',1X,1P,E10.3,0P,1X,'I',1X,A4,1X,'I',1X,1P,E10.3,0P,1X,'I')
 5900 FORMAT(1X,'I',29X,'I',13X,'I',12X,'I',6X,'I',12X,'I'/1X,78('='))
 6000 FORMAT('1',7('*'),1X,'PYSTAT: Allowed Incoming Partons/',
     &'Particles at Hard Interaction',1X,7('*'))
 6100 FORMAT(/1X,78('=')/1X,'I',38X,'I',37X,'I'/1X,'I',1X,
     &'Beam particle:',1X,A12,10X,'I',1X,'Target particle:',1X,A12,7X,
     &'I'/1X,'I',38X,'I',37X,'I'/1X,'I',1X,'Content',6X,'State',19X,
     &'I',1X,'Content',6X,'State',18X,'I'/1X,'I',38X,'I',37X,'I'/1X,
     &78('=')/1X,'I',38X,'I',37X,'I')
 6200 FORMAT(1X,'I',1X,A9,5X,A4,19X,'I',1X,A9,5X,A4,18X,'I')
 6300 FORMAT(1X,'I',38X,'I',37X,'I'/1X,78('='))
 6400 FORMAT('1',12('*'),1X,'PYSTAT: User-Defined Limits on ',
     &'Kinematical Variables',1X,12('*'))
 6500 FORMAT(/1X,78('=')/1X,'I',76X,'I')
 6600 FORMAT(1X,'I',16X,1P,E10.3,0P,1X,'<',1X,A,1X,'<',1X,1P,E10.3,0P,
     &16X,'I')
 6700 FORMAT(1X,'I',3X,1P,E10.3,0P,1X,'(',1P,E10.3,0P,')',1X,'<',1X,A,
     &1X,'<',1X,1P,E10.3,0P,16X,'I')
 6800 FORMAT(1X,'I',29X,A,1X,'=',1X,1P,E10.3,0P,16X,'I')
 6900 FORMAT(1X,'I',76X,'I'/1X,78('='))
 7000 FORMAT('1',12('*'),1X,'PYSTAT: Summary of Status Codes and ',
     &'Parameter Values',1X,12('*'))
 7100 FORMAT(/3X,'I',4X,'MSTP(I)',9X,'PARP(I)',20X,'I',4X,'MSTP(I)',9X,
     &'PARP(I)'/)
 7200 FORMAT(1X,I3,5X,I6,6X,1P,E10.3,0P,18X,I3,5X,I6,6X,1P,E10.3)
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYINRE
 
C...Calculates full and effective widths of gauge bosons, stores
C...masses and widths, rescales coefficients to be used for
C...resonance production generation.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
      COMMON/GUDAT4/CHAF(500)
      CHARACTER CHAF*8
      SAVE /GUDAT4/
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
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      REAL  WIDP,WIDE,WIDS
      SAVE /PYINT4/
      COMMON/PYINT6/PROC(0:200)
      CHARACTER PROC*28
      SAVE /PYINT6/
      DIMENSION WDTP(0:40),WDTE(0:40,0:5),WDTPM(0:40),WDTEM(0:40,0:5)
      DIMENSION KCINP(16),KCORD(16),PMORD(16)
      DATA KCINP/23,24,25,6,7,8,17,18,32,34,35,36,37,38,39,40/
 
C...Born level couplings in MSSM Higgs doublet sector.
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2) XW=1.-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1.-XW
      IF(MSTP(4).EQ.2) THEN
        TANBE=PARU(141)
        RATBE=((1.-TANBE**2)/(1.+TANBE**2))**2
        SQMZ=PMAS(23,1)**2
        SQMW=PMAS(24,1)**2
        SQMH=PMAS(25,1)**2
        SQMA=SQMH*(SQMZ-SQMH)/(SQMZ*RATBE-SQMH)
        SQMHP=0.5*(SQMA+SQMZ+SQRT((SQMA+SQMZ)**2-4.*SQMA*SQMZ*RATBE))
        SQMHC=SQMA+SQMW
        IF(SQMH.GE.SQMZ.OR.MIN(SQMA,SQMHP,SQMHC).LE.0.) THEN
          WRITE(MSTU(11),5000)
          STOP
        ENDIF
        PMAS(35,1)=SQRT(SQMHP)
        PMAS(36,1)=SQRT(SQMA)
        PMAS(37,1)=SQRT(SQMHC)
        ALSU=0.5*ATAN(2.*TANBE*(SQMA+SQMZ)/((1.-TANBE**2)*
     &  (SQMA-SQMZ)))
        BESU=ATAN(TANBE)
        PARU(142)=1.
        PARU(143)=1.
        PARU(161)=-SIN(ALSU)/COS(BESU)
        PARU(162)=COS(ALSU)/SIN(BESU)
        PARU(163)=PARU(161)
        PARU(164)=SIN(BESU-ALSU)
        PARU(165)=PARU(164)
        PARU(168)=SIN(BESU-ALSU)+0.5*COS(2.*BESU)*SIN(BESU+ALSU)/XW
        PARU(171)=COS(ALSU)/COS(BESU)
        PARU(172)=SIN(ALSU)/SIN(BESU)
        PARU(173)=PARU(171)
        PARU(174)=COS(BESU-ALSU)
        PARU(175)=PARU(174)
        PARU(176)=COS(2.*ALSU)*COS(BESU+ALSU)-2.*SIN(2.*ALSU)*
     &  SIN(BESU+ALSU)
        PARU(177)=COS(2.*BESU)*COS(BESU+ALSU)
        PARU(178)=COS(BESU-ALSU)-0.5*COS(2.*BESU)*COS(BESU+ALSU)/XW
        PARU(181)=TANBE
        PARU(182)=1./TANBE
        PARU(183)=PARU(181)
        PARU(184)=0.
        PARU(185)=PARU(184)
        PARU(186)=COS(BESU-ALSU)
        PARU(187)=SIN(BESU-ALSU)
        PARU(188)=PARU(186)
        PARU(189)=PARU(187)
        PARU(190)=0.
        PARU(195)=COS(BESU-ALSU)
      ENDIF
 
C...Change matrix element codes when top and 4th generation
C...decay before fragmentation.
      IF(MSTP(48).GE.1) THEN
        IOFF=MDCY(6,2)-1
        DO 100 I=4,7
        MDME(IOFF+I,2)=0
  100   CONTINUE
        MDME(IOFF+9,2)=0
      ENDIF
      IF(MSTP(6).EQ.1) THEN
        IOFF=MDCY(7,2)-1
        DO 110 I=1,4
        MDME(IOFF+I,2)=0
  110   CONTINUE
        IOFF=MDCY(8,2)-1
        DO 120 I=1,4
        MDME(IOFF+I,2)=0
  120   CONTINUE
        IOFF=MDCY(17,2)-1
        MDME(IOFF+2,2)=0
        MDME(IOFF+3,2)=0
        MDME(IOFF+4,2)=0
        IOFF=MDCY(18,2)-1
        MDME(IOFF+1,2)=0
        MDME(IOFF+2,2)=0
      ELSEIF(MSTP(49).GE.1) THEN
        IOFF=MDCY(7,2)-1
        DO 130 I=4,7
        MDME(IOFF+I,2)=0
  130   CONTINUE
        MDME(IOFF+9,2)=0
        MDME(IOFF+10,2)=0
        IOFF=MDCY(8,2)-1
        DO 140 I=4,7
        MDME(IOFF+I,2)=0
  140   CONTINUE
        MDME(IOFF+9,2)=0
        MDME(IOFF+10,2)=0
        IOFF=MDCY(17,2)-1
        MDME(IOFF+4,2)=0
        MDME(IOFF+6,2)=0
        IOFF=MDCY(18,2)-1
        MDME(IOFF+2,2)=0
        MDME(IOFF+3,2)=0
      ENDIF
 
C...Reset full and effective widths of gauge bosons.
      DO 160 I=21,40
      DO 150 J=0,40
      WIDP(I,J)=0.
      WIDE(I,J)=0.
  150 CONTINUE
      WIDS(I,1)=1.
      WIDS(I,2)=1.
      WIDS(I,3)=1.
  160 CONTINUE
 
C...Order resonances by increasing mass (except Z0 and W+/-).
      DO 170 I=1,3
      KCORD(I)=KCINP(I)
      PMORD(I)=PMAS(KCORD(I),1)
  170 CONTINUE
      DO 200 I=4,16
      KCIN=KCINP(I)
      PMIN=PMAS(KCIN,1)
      DO 180 I1=I-1,3,-1
      IF(PMIN.GE.PMORD(I1)) GOTO 190
      KCORD(I1+1)=KCORD(I1)
      PMORD(I1+1)=PMORD(I1)
  180 CONTINUE
  190 KCORD(I1+1)=KCIN
      PMORD(I1+1)=PMIN
  200 CONTINUE
 
C...Loop over possible resonances.
      DO 250 I=1,16
      KC=KCORD(I)
      IF(KC.EQ.6.AND.MSTP(48).LE.0) GOTO 250
      IF(KC.EQ.7.OR.KC.EQ.8.OR.KC.EQ.17.OR.KC.EQ.18) THEN
        IF(MSTP(6).NE.1.AND.(MSTP(49).LE.0.OR.MSTP(1).LE.3)) GOTO 250
        IF(KC.EQ.18.AND.PMORD(I).LT.1.) GOTO 250
      ENDIF
      KCL=KC
      IF(KC.GE.6.AND.KC.LE.8) KCL=KC+20
      IF(KC.EQ.17.OR.KC.EQ.18) KCL=KC+12
 
C...Change decay modes for q* and l*.
      IF(MSTP(6).EQ.1.AND.(KC.EQ.7.OR.KC.EQ.8.OR.KC.EQ.17.OR.
     &KC.EQ.18)) THEN
        DO 210 J=1,MDCY(KC,3)
        IDC=J+MDCY(KC,2)-1
        KF2=KFDP(IDC,2)
        IF(KF2.EQ.7.OR.KF2.EQ.8.OR.KF2.EQ.17.OR.KF2.EQ.18)
     &  KFDP(IDC,2)=KF2-6
  210   CONTINUE
      ENDIF
 
C...Check that no fourth generation channels on by mistake.
      IF(MSTP(1).LE.3) THEN
        DO 220 J=1,MDCY(KC,3)
        IDC=J+MDCY(KC,2)-1
        KFA1=IABS(KFDP(IDC,1))
        KFA2=IABS(KFDP(IDC,2))
        IF(KFA1.EQ.7.OR.KFA1.EQ.8.OR.KFA1.EQ.17.OR.KFA1.EQ.18.OR.KFA2
     &  .EQ.7.OR.KFA2.EQ.8.OR.KFA2.EQ.17.OR.KFA2.EQ.18) MDME(IDC,1)=-1
  220   CONTINUE
      ENDIF
 
C...Find mass and evaluate width.
      PMR=PMAS(KC,1)
      IF(KC.EQ.25.OR.KC.EQ.35.OR.KC.EQ.36) MINT(62)=1
      CALL PYWIDT(KC,PMR**2,WDTP,WDTE)
      IF(KC.EQ.6.OR.KC.EQ.7.OR.KC.EQ.8.OR.KC.EQ.17.OR.KC.EQ.18)
     &CALL PYWIDT(-KC,PMR**2,WDTPM,WDTEM)
      MINT(51)=0
 
C...Evaluate suppression factors due to non-simulated channels.
      IF(KCHG(KC,3).EQ.0) THEN
        WIDS(KCL,1)=((WDTE(0,1)+WDTE(0,2))**2+
     &  2.*(WDTE(0,1)+WDTE(0,2))*(WDTE(0,4)+WDTE(0,5))+
     &  2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
        WIDS(KCL,2)=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
        WIDS(KCL,3)=0.
      ELSEIF(KC.EQ.6.OR.KC.EQ.7.OR.KC.EQ.8.OR.KC.EQ.17.OR.KC.EQ.18) THEN
        WIDS(KCL,1)=((WDTE(0,1)+WDTE(0,2))*(WDTEM(0,1)+WDTEM(0,3))+
     &  (WDTE(0,1)+WDTE(0,2))*(WDTEM(0,4)+WDTEM(0,5))+
     &  (WDTE(0,4)+WDTE(0,5))*(WDTEM(0,1)+WDTEM(0,3))+
     &  WDTE(0,4)*WDTEM(0,5)+WDTE(0,5)*WDTEM(0,4))/WDTP(0)**2
        WIDS(KCL,2)=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
        WIDS(KCL,3)=(WDTEM(0,1)+WDTEM(0,3)+WDTEM(0,4))/WDTP(0)
      ELSE
        WIDS(KCL,1)=((WDTE(0,1)+WDTE(0,2))*(WDTE(0,1)+WDTE(0,3))+
     &  (WDTE(0,1)+WDTE(0,2)+WDTE(0,1)+WDTE(0,3))*(WDTE(0,4)+WDTE(0,5))+
     &  2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
        WIDS(KCL,2)=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))/WDTP(0)
        WIDS(KCL,3)=(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))/WDTP(0)
        IF(KC.EQ.24) THEN
          VINT(91)=((WDTE(0,1)+WDTE(0,2))**2+2.*(WDTE(0,1)+WDTE(0,2))*
     &    (WDTE(0,4)+WDTE(0,5))+2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
          VINT(92)=((WDTE(0,1)+WDTE(0,3))**2+2.*(WDTE(0,1)+WDTE(0,3))*
     &    (WDTE(0,4)+WDTE(0,5))+2.*WDTE(0,4)*WDTE(0,5))/WDTP(0)**2
        ENDIF
      ENDIF
 
C...Find factors to give widths in GeV.
      AEM=ULALEM(PMR**2)
      IF(MSTP(8).GE.1) AEM=SQRT(2.)*PARU(105)*PMAS(24,1)**2*XW/PARU(1)
      IF(KC.LE.20) THEN
        FAC=PMR
      ELSEIF(KC.EQ.23.OR.KC.EQ.32) THEN
        FAC=AEM/(48.*XW*XW1)*PMR
      ELSEIF(KC.EQ.24.OR.KC.EQ.34) THEN
        FAC=AEM/(24.*XW)*PMR
      ELSEIF(KC.EQ.25.OR.KC.EQ.35.OR.KC.EQ.36.OR.KC.EQ.37) THEN
        FAC=AEM/(8.*XW)*(PMR/PMAS(24,1))**2*PMR
      ELSEIF(KC.EQ.38) THEN
        FAC=PMR
      ELSEIF(KC.EQ.39) THEN
        FAC=AEM/4.*PMR
      ELSEIF(KC.EQ.40) THEN
        FAC=AEM/(12.*XW)*PMR
      ENDIF
 
C...Translate widths into GeV and save them.
      DO 230 J=0,40
      WIDP(KCL,J)=FAC*WDTP(J)
      WIDE(KCL,J)=FAC*WDTE(J,0)
  230 CONTINUE
 
C...Set resonance widths and branching ratios in JETSET;
C...also on/off switch for decays in PYTHIA/JETSET.
      PMAS(KC,2)=WIDP(KCL,0)
      PMAS(KC,3)=MIN(0.9*PMAS(KC,1),10.*PMAS(KC,2))
      MDCY(KC,1)=MSTP(41)
      DO 240 J=1,MDCY(KC,3)
      IDC=J+MDCY(KC,2)-1
      BRAT(IDC)=0.
      IF(WIDE(KCL,0).GT.0.) BRAT(IDC)=WIDE(KCL,J)/WIDE(KCL,0)
  240 CONTINUE
  250 CONTINUE
 
C...Flavours of leptoquark: redefine charge and name.
      KFLQQ=KFDP(MDCY(39,2),1)
      KFLQL=KFDP(MDCY(39,2),2)
      KCHG(39,1)=KCHG(IABS(KFLQQ),1)*ISIGN(1,KFLQQ)+
     &KCHG(IABS(KFLQL),1)*ISIGN(1,KFLQL)
      CHAF(39)(4:4)=CHAF(IABS(KFLQQ))(1:1)
      CHAF(39)(5:7)=CHAF(IABS(KFLQL))(1:3)
 
C...Scenario with q* and l*: redefine names.
      IF(MSTP(6).EQ.1) THEN
        CHAF(7)='d*'
        CHAF(8)='u*'
        CHAF(17)='e*'
        CHAF(18)='nu*_e'
      ENDIF
 
C...Special cases in treatment of gamma*/Z0: redefine process name.
      IF(MSTP(43).EQ.1) THEN
        PROC(1)='f + f~ -> gamma*'
        PROC(15)='f + f~ -> g + gamma*'
        PROC(19)='f + f~ -> gamma + gamma*'
        PROC(30)='f + g -> f + gamma*'
        PROC(35)='f + gamma -> f + gamma*'
      ELSEIF(MSTP(43).EQ.2) THEN
        PROC(1)='f + f~ -> Z0'
        PROC(15)='f + f~ -> g + Z0'
        PROC(19)='f + f~ -> gamma + Z0'
        PROC(30)='f + g -> f + Z0'
        PROC(35)='f + gamma -> f + Z0'
      ELSEIF(MSTP(43).EQ.3) THEN
        PROC(1)='f + f~ -> gamma*/Z0'
        PROC(15)='f + f~ -> g + gamma*/Z0'
        PROC(19)='f + f~ -> gamma + gamma*/Z0'
        PROC(30)='f + g -> f + gamma*/Z0'
        PROC(35)='f + gamma -> f + gamma*/Z0'
      ENDIF
 
C...Special cases in treatment of gamma*/Z0/Z'0: redefine process name.
      IF(MSTP(44).EQ.1) THEN
        PROC(141)='f + f~ -> gamma*'
      ELSEIF(MSTP(44).EQ.2) THEN
        PROC(141)='f + f~ -> Z0'
      ELSEIF(MSTP(44).EQ.3) THEN
        PROC(141)='f + f~ -> Z''0'
      ELSEIF(MSTP(44).EQ.4) THEN
        PROC(141)='f + f~ -> gamma*/Z0'
      ELSEIF(MSTP(44).EQ.5) THEN
        PROC(141)='f + f~ -> gamma*/Z''0'
      ELSEIF(MSTP(44).EQ.6) THEN
        PROC(141)='f + f~ -> Z0/Z''0'
      ELSEIF(MSTP(44).EQ.7) THEN
        PROC(141)='f + f~ -> gamma*/Z0/Z''0'
      ENDIF
 
C...Special cases in treatment of WW -> WW: redefine process name.
      IF(MSTP(45).EQ.1) THEN
        PROC(77)='W+ + W+ -> W+ + W+'
      ELSEIF(MSTP(45).EQ.2) THEN
        PROC(77)='W+ + W- -> W+ + W-'
      ELSEIF(MSTP(45).EQ.3) THEN
        PROC(77)='W+/- + W+/- -> W+/- + W+/-'
      ENDIF
 
C...Format for error information.
 5000 FORMAT(1X,'Error: unphysical input tan^2(beta) and m_H ',
     &'combination'/1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYINBM(CHFRAM,CHBEAM,CHTARG,WIN)
 
C...Identifies the two incoming particles and the choice of frame.
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
      CHARACTER CHFRAM*8,CHBEAM*8,CHTARG*8,CHCOM(3)*8,CHALP(2)*26,
     &CHIDNT(3)*8,CHTEMP*8,CHCDE(29)*8,CHINIT*76
      DIMENSION LEN(3),KCDE(29),PM(2)
      DATA CHALP/'abcdefghijklmnopqrstuvwxyz',
     &'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA CHCDE/'e-      ','e+      ','nu_e    ','nu_e~   ',
     &'mu-     ','mu+     ','nu_mu   ','nu_mu~  ','tau-    ',
     &'tau+    ','nu_tau  ','nu_tau~ ','pi+     ','pi-     ',
     &'n0      ','n~0     ','p+      ','p~-     ','gamma   ',
     &'lambda0 ','sigma-  ','sigma0  ','sigma+  ','xi-     ',
     &'xi0     ','omega-  ','pi0     ','reggeon ','pomeron '/
      DATA KCDE/11,-11,12,-12,13,-13,14,-14,15,-15,16,-16,
     &211,-211,2112,-2112,2212,-2212,22,3122,3112,3212,3222,
     &3312,3322,3334,111,28,29/
 
C...Store initial energy. Default frame.
      VINT(290)=WIN
      MINT(111)=0
 
C...Convert character variables to lowercase and find their length.
      CHCOM(1)=CHFRAM
      CHCOM(2)=CHBEAM
      CHCOM(3)=CHTARG
      DO 130 I=1,3
      LEN(I)=8
      DO 110 LL=8,1,-1
      IF(LEN(I).EQ.LL.AND.CHCOM(I)(LL:LL).EQ.' ') LEN(I)=LL-1
      DO 100 LA=1,26
      IF(CHCOM(I)(LL:LL).EQ.CHALP(2)(LA:LA)) CHCOM(I)(LL:LL)=
     &CHALP(1)(LA:LA)
  100 CONTINUE
  110 CONTINUE
      CHIDNT(I)=CHCOM(I)
 
C...Fix up bar, underscore and charge in particle name (if needed).
      DO 120 LL=1,6
      IF(CHIDNT(I)(LL:LL+2).EQ.'bar') THEN
        CHTEMP=CHIDNT(I)
        CHIDNT(I)=CHTEMP(1:LL-1)//'~'//CHTEMP(LL+3:8)//'  '
      ENDIF
  120 CONTINUE
      IF(CHIDNT(I)(1:2).EQ.'nu'.AND.CHIDNT(I)(3:3).NE.'_') THEN
        CHTEMP=CHIDNT(I)
        CHIDNT(I)='nu_'//CHTEMP(3:7)
      ELSEIF(CHIDNT(I)(1:2).EQ.'n ') THEN
        CHIDNT(I)(1:3)='n0 '
      ELSEIF(CHIDNT(I)(1:2).EQ.'n~') THEN
        CHIDNT(I)(1:3)='n~0'
      ELSEIF(CHIDNT(I)(1:2).EQ.'p ') THEN
        CHIDNT(I)(1:3)='p+ '
      ELSEIF(CHIDNT(I)(1:2).EQ.'p~'.OR.CHIDNT(I)(1:2).EQ.'p-') THEN
        CHIDNT(I)(1:3)='p~-'
      ELSEIF(CHIDNT(I)(1:6).EQ.'lambda') THEN
        CHIDNT(I)(7:7)='0'
      ELSEIF(CHIDNT(I)(1:3).EQ.'reg') THEN
        CHIDNT(I)(1:7)='reggeon'
      ELSEIF(CHIDNT(I)(1:3).EQ.'pom') THEN
        CHIDNT(I)(1:7)='pomeron'
      ENDIF
  130 CONTINUE
 
C...Identify free initialization.
      IF(CHCOM(1)(1:2).EQ.'no') THEN
        MINT(65)=1
        RETURN
      ENDIF
 
C...Identify incoming beam and target particles.
      DO 150 I=1,2
      DO 140 J=1,29
      IF(CHIDNT(I+1).EQ.CHCDE(J)) MINT(10+I)=KCDE(J)
  140 CONTINUE
      PM(I)=ULMASS(MINT(10+I))
      VINT(2+I)=PM(I)
  150 CONTINUE
      IF(MINT(11).EQ.0) WRITE(MSTU(11),5000) CHBEAM(1:LEN(2))
      IF(MINT(12).EQ.0) WRITE(MSTU(11),5100) CHTARG(1:LEN(3))
      IF(MINT(11).EQ.0.OR.MINT(12).EQ.0) STOP
 
C...Identify choice of frame and input energies.
      CHINIT=' '
 
C...Events defined in the CM frame.
      IF(CHCOM(1)(1:2).EQ.'cm') THEN
        MINT(111)=1
        S=WIN**2
        IF(MSTP(122).GE.1) THEN
          IF(CHCOM(2)(1:1).NE.'e') THEN
            LOFFS=(31-(LEN(2)+LEN(3)))/2
            CHINIT(LOFFS+1:76)='PYTHIA will be initialized for a '//
     &      CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &      ' collider'//' '
          ELSE
            LOFFS=(30-(LEN(2)+LEN(3)))/2
            CHINIT(LOFFS+1:76)='PYTHIA will be initialized for an '//
     &      CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &      ' collider'//' '
          ENDIF
          WRITE(MSTU(11),5200) CHINIT
          WRITE(MSTU(11),5300) WIN
        ENDIF
 
C...Events defined in fixed target frame.
      ELSEIF(CHCOM(1)(1:3).EQ.'fix') THEN
        MINT(111)=2
        S=PM(1)**2+PM(2)**2+2.*PM(2)*SQRT(PM(1)**2+WIN**2)
        IF(MSTP(122).GE.1) THEN
          LOFFS=(29-(LEN(2)+LEN(3)))/2
          CHINIT(LOFFS+1:76)='PYTHIA will be initialized for '//
     &    CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &    ' fixed target'//' '
          WRITE(MSTU(11),5200) CHINIT
          WRITE(MSTU(11),5400) WIN
          WRITE(MSTU(11),5500) SQRT(S)
        ENDIF
 
C...Frame defined by user three-vectors.
      ELSEIF(CHCOM(1)(1:3).EQ.'use') THEN
        MINT(111)=3
        P(1,5)=PM(1)
        P(2,5)=PM(2)
        P(1,4)=SQRT(P(1,1)**2+P(1,2)**2+P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,1)**2+P(2,2)**2+P(2,3)**2+P(2,5)**2)
        S=(P(1,4)+P(2,4))**2-(P(1,1)+P(2,1))**2-(P(1,2)+P(2,2))**2-
     &  (P(1,3)+P(2,3))**2
        IF(MSTP(122).GE.1) THEN
          LOFFS=(12-(LEN(2)+LEN(3)))/2
          CHINIT(LOFFS+1:76)='PYTHIA will be initialized for '//
     &    CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &    ' user-specified configuration'//' '
          WRITE(MSTU(11),5200) CHINIT
          WRITE(MSTU(11),5600)
          WRITE(MSTU(11),5700) CHCOM(2),P(1,1),P(1,2),P(1,3),P(1,4)
          WRITE(MSTU(11),5700) CHCOM(3),P(2,1),P(2,2),P(2,3),P(2,4)
          WRITE(MSTU(11),5500) SQRT(MAX(0.,S))
        ENDIF
 
C...Frame defined by user four-vectors.
      ELSEIF(CHCOM(1)(1:4).EQ.'four') THEN
        MINT(111)=4
        PMS1=P(1,4)**2-P(1,1)**2-P(1,2)**2-P(1,3)**2
        P(1,5)=SIGN(SQRT(ABS(PMS1)),PMS1)
        PMS2=P(2,4)**2-P(2,1)**2-P(2,2)**2-P(2,3)**2
        P(2,5)=SIGN(SQRT(ABS(PMS2)),PMS2)
        S=(P(1,4)+P(2,4))**2-(P(1,1)+P(2,1))**2-(P(1,2)+P(2,2))**2-
     &  (P(1,3)+P(2,3))**2
        IF(MSTP(122).GE.1) THEN
          LOFFS=(12-(LEN(2)+LEN(3)))/2
          CHINIT(LOFFS+1:76)='PYTHIA will be initialized for '//
     &    CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &    ' user-specified configuration'//' '
          WRITE(MSTU(11),5200) CHINIT
          WRITE(MSTU(11),5600)
          WRITE(MSTU(11),5700) CHCOM(2),P(1,1),P(1,2),P(1,3),P(1,4)
          WRITE(MSTU(11),5700) CHCOM(3),P(2,1),P(2,2),P(2,3),P(2,4)
          WRITE(MSTU(11),5500) SQRT(MAX(0.,S))
        ENDIF
 
C...Frame defined by user five-vectors.
      ELSEIF(CHCOM(1)(1:4).EQ.'five') THEN
        MINT(111)=5
        S=(P(1,4)+P(2,4))**2-(P(1,1)+P(2,1))**2-(P(1,2)+P(2,2))**2-
     &  (P(1,3)+P(2,3))**2
        IF(MSTP(122).GE.1) THEN
          LOFFS=(12-(LEN(2)+LEN(3)))/2
          CHINIT(LOFFS+1:76)='PYTHIA will be initialized for '//
     &    CHCOM(2)(1:LEN(2))//' on '//CHCOM(3)(1:LEN(3))//
     &    ' user-specified configuration'//' '
          WRITE(MSTU(11),5200) CHINIT
          WRITE(MSTU(11),5600)
          WRITE(MSTU(11),5700) CHCOM(2),P(1,1),P(1,2),P(1,3),P(1,4)
          WRITE(MSTU(11),5700) CHCOM(3),P(2,1),P(2,2),P(2,3),P(2,4)
          WRITE(MSTU(11),5500) SQRT(MAX(0.,S))
        ENDIF
 
C...Unknown frame. Error for too low CM energy.
      ELSE
        WRITE(MSTU(11),5800) CHFRAM(1:LEN(1))
        STOP
      ENDIF
      IF(S.LT.PARP(2)**2) THEN
        WRITE(MSTU(11),5900) SQRT(S)
        STOP
      ENDIF
 
C...Formats for initialization and error information.
 5000 FORMAT(1X,'Error: unrecognized beam particle ''',A,'''.'/
     &1X,'Execution stopped!')
 5100 FORMAT(1X,'Error: unrecognized target particle ''',A,'''.'/
     &1X,'Execution stopped!')
 5200 FORMAT(/1X,78('=')/1X,'I',76X,'I'/1X,'I',A76,'I')
 5300 FORMAT(1X,'I',18X,'at',1X,F10.3,1X,'GeV center-of-mass energy',
     &19X,'I'/1X,'I',76X,'I'/1X,78('='))
 5400 FORMAT(1X,'I',22X,'at',1X,F10.3,1X,'GeV/c lab-momentum',22X,'I')
 5500 FORMAT(1X,'I',76X,'I'/1X,'I',11X,'corresponding to',1X,F10.3,1X,
     &'GeV center-of-mass energy',12X,'I'/1X,'I',76X,'I'/1X,78('='))
 5600 FORMAT(1X,'I',76X,'I'/1X,'I',18X,'px (GeV/c)',3X,'py (GeV/c)',3X,
     &'pz (GeV/c)',6X,'E (GeV)',9X,'I')
 5700 FORMAT(1X,'I',8X,A8,4(2X,F10.3,1X),8X,'I')
 5800 FORMAT(1X,'Error: unrecognized coordinate frame ''',A,'''.'/
     &1X,'Execution stopped!')
 5900 FORMAT(1X,'Error: too low CM energy,',F8.3,' GeV for event ',
     &'generation.'/1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYINKI(MODKI)
 
C...Sets up kinematics, including rotations and boosts to/from CM frame.
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
C...Set initial flavour state.
      N=2
      DO 100 I=1,2
      K(I,1)=1
      K(I,2)=MINT(10+I)
  100 CONTINUE
 
C...Reset boost. Do kinematics for various cases.
      DO 110 J=6,10
      VINT(J)=0.
  110 CONTINUE
 
C...Set up kinematics for events defined in CM frame.
      IF(MINT(111).EQ.1) THEN
        WIN=VINT(290)
        IF(MODKI.EQ.1) WIN=PARP(171)*VINT(290)
        S=WIN**2
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        P(1,1)=0.
        P(1,2)=0.
        P(2,1)=0.
        P(2,2)=0.
        P(1,3)=SQRT(((S-P(1,5)**2-P(2,5)**2)**2-(2.*P(1,5)*P(2,5))**2)/
     &  (4.*S))
        P(2,3)=-P(1,3)
        P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,3)**2+P(2,5)**2)
 
C...Set up kinematics for fixed target events.
      ELSEIF(MINT(111).EQ.2) THEN
        WIN=VINT(290)
        IF(MODKI.EQ.1) WIN=PARP(171)*VINT(290)
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        P(1,1)=0.
        P(1,2)=0.
        P(2,1)=0.
        P(2,2)=0.
        P(1,3)=WIN
        P(1,4)=SQRT(P(1,3)**2+P(1,5)**2)
        P(2,3)=0.
        P(2,4)=P(2,5)
        S=P(1,5)**2+P(2,5)**2+2.*P(2,4)*P(1,4)
        VINT(10)=P(1,3)/(P(1,4)+P(2,4))
        CALL LUROBO(0.,0.,0.,0.,-VINT(10))
 
C...Set up kinematics for events in user-defined frame.
      ELSEIF(MINT(111).EQ.3) THEN
        P(1,5)=VINT(3)
        P(2,5)=VINT(4)
        P(1,4)=SQRT(P(1,1)**2+P(1,2)**2+P(1,3)**2+P(1,5)**2)
        P(2,4)=SQRT(P(2,1)**2+P(2,2)**2+P(2,3)**2+P(2,5)**2)
        DO 120 J=1,3
        VINT(7+J)=(DBLE(P(1,J))+DBLE(P(2,J)))/DBLE(P(1,4)+P(2,4))
  120   CONTINUE
        CALL LUROBO(0.,0.,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=ULANGL(P(1,1),P(1,2))
        CALL LUROBO(0.,-VINT(7),0.,0.,0.)
        VINT(6)=ULANGL(P(1,3),P(1,1))
        CALL LUROBO(-VINT(6),0.,0.,0.,0.)
        S=P(1,5)**2+P(2,5)**2+2.*(P(1,4)*P(2,4)-P(1,3)*P(2,3))
 
C...Set up kinematics for events with user-defined four-vectors.
      ELSEIF(MINT(111).EQ.4) THEN
        PMS1=P(1,4)**2-P(1,1)**2-P(1,2)**2-P(1,3)**2
        P(1,5)=SIGN(SQRT(ABS(PMS1)),PMS1)
        PMS2=P(2,4)**2-P(2,1)**2-P(2,2)**2-P(2,3)**2
        P(2,5)=SIGN(SQRT(ABS(PMS2)),PMS2)
        DO 130 J=1,3
        VINT(7+J)=(DBLE(P(1,J))+DBLE(P(2,J)))/DBLE(P(1,4)+P(2,4))
  130   CONTINUE
        CALL LUROBO(0.,0.,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=ULANGL(P(1,1),P(1,2))
        CALL LUROBO(0.,-VINT(7),0.,0.,0.)
        VINT(6)=ULANGL(P(1,3),P(1,1))
        CALL LUROBO(-VINT(6),0.,0.,0.,0.)
        S=(P(1,4)+P(2,4))**2
 
C...Set up kinematics for events with user-defined five-vectors.
      ELSEIF(MINT(111).EQ.5) THEN
        DO 140 J=1,3
        VINT(7+J)=(DBLE(P(1,J))+DBLE(P(2,J)))/DBLE(P(1,4)+P(2,4))
  140   CONTINUE
        CALL LUROBO(0.,0.,-VINT(8),-VINT(9),-VINT(10))
        VINT(7)=ULANGL(P(1,1),P(1,2))
        CALL LUROBO(0.,-VINT(7),0.,0.,0.)
        VINT(6)=ULANGL(P(1,3),P(1,1))
        CALL LUROBO(-VINT(6),0.,0.,0.,0.)
        S=(P(1,4)+P(2,4))**2
      ENDIF
 
C...Return or error for too low CM energy.
      IF(MODKI.EQ.1.AND.S.LT.PARP(2)**2) THEN
        IF(MSTP(172).LE.1) THEN
          CALL LUERRM(23,
     &    '(PYINKI:) too low invariant mass in this event')
        ELSE
          MSTI(61)=1
          RETURN
        ENDIF
      ENDIF
 
C...Save information on incoming particles.
      VINT(1)=SQRT(S)
      VINT(2)=S
      IF(MINT(111).GE.4) VINT(3)=P(1,5)
      IF(MINT(111).GE.4) VINT(4)=P(2,5)
      VINT(5)=P(1,3)
      IF(MODKI.EQ.0) VINT(289)=S
      DO 150 J=1,5
      V(1,J)=0.
      V(2,J)=0.
      VINT(290+J)=P(1,J)
      VINT(295+J)=P(2,J)
  150 CONTINUE
 
C...Store pT cut-off and related constants to be used in generation.
      IF(MODKI.EQ.0) VINT(285)=CKIN(3)
      IF(MSTP(82).LE.1) THEN
        IF(MINT(121).GT.1) PARP(81)=1.30+0.15*LOG(VINT(1)/200.)/
     &  LOG(900./200.)
        PTMN=PARP(81)
      ELSE
        IF(MINT(121).GT.1) PARP(82)=1.25+0.15*LOG(VINT(1)/200.)/
     &  LOG(900./200.)
        PTMN=PARP(82)
      ENDIF
      VINT(149)=4.*PTMN**2/S
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYINPR
 
C...Selects partonic subprocesses to be included in the simulation.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
C...Reset processes to be included.
      IF(MSEL.NE.0) THEN
        DO 100 I=1,200
        MSUB(I)=0
  100   CONTINUE
      ENDIF
 
C...For e-gamma witn MSTP(14)=10 allow mixture of VMD and anomalous.
      IF(MINT(121).EQ.2) THEN
        MSUB(10)=1
        MINT(123)=MINT(122)+1
 
C...For gamma-p or gamma-gamma with MSTP(14)=10 allow mixture.
C...Here also set a few parameters otherwise normally not touched.
      ELSEIF(MINT(121).GT.1) THEN
 
C...Structure functions dampened at small Q2; go to low energies,
C...alpha_s <1; no minimum pT cut-off a priori.
        MSTP(57)=3
        MSTP(85)=0
        PARP(2)=2.
        PARU(115)=1.
        CKIN(5)=0.2
        CKIN(6)=0.2
 
C...Define pT cut-off parameters and whether run involves low-pT.
        IF(MSTP(82).LE.1) THEN
          PTMVMD=1.30+0.15*LOG(VINT(1)/200.)/LOG(900./200.)
        ELSE
          PTMVMD=1.25+0.15*LOG(VINT(1)/200.)/LOG(900./200.)
        ENDIF
        PTMDIR=PARP(15)
        PTMANO=PTMVMD
        IF(MSTP(15).EQ.5) PTMANO=0.60+0.125*LOG(1.+0.1*VINT(1))**2
        IPTL=1
        IF(VINT(285).GT.MAX(PTMVMD,PTMDIR,PTMANO)) IPTL=0
        IF(MSEL.EQ.2) IPTL=1
 
C...Set up for p/VMD * VMD.
        IF(MINT(122).EQ.1) THEN
          MINT(123)=2
          MSUB(11)=1
          MSUB(12)=1
          MSUB(13)=1
          MSUB(28)=1
          MSUB(53)=1
          MSUB(68)=1
          IF(IPTL.EQ.1) MSUB(95)=1
          IF(MSEL.EQ.2) THEN
            MSUB(91)=1
            MSUB(92)=1
            MSUB(93)=1
            MSUB(94)=1
          ENDIF
          PARP(81)=PTMVMD
          PARP(82)=PTMVMD
          IF(IPTL.EQ.1) CKIN(3)=0.
 
C...Set up for p/VMD * direct gamma.
        ELSEIF(MINT(122).EQ.2) THEN
          MINT(123)=0
          IF(MINT(121).EQ.6) MINT(123)=5
          MSUB(33)=1
          MSUB(54)=1
          IF(IPTL.EQ.1) CKIN(3)=PTMDIR
 
C...Set up for p/VMD * anomalous gamma.
        ELSEIF(MINT(122).EQ.3) THEN
          MINT(123)=3
          IF(MINT(121).EQ.6) MINT(123)=7
          MSUB(11)=1
          MSUB(12)=1
          MSUB(13)=1
          MSUB(28)=1
          MSUB(53)=1
          MSUB(68)=1
          IF(MSTP(82).GE.2) MSTP(85)=1
          IF(IPTL.EQ.1) CKIN(3)=PTMANO
 
C...Set up for direct * direct gamma (switch off leptons).
        ELSEIF(MINT(122).EQ.4) THEN
          MINT(123)=0
          MSUB(58)=1
*DGC
*1/4/96 Copied following modification from two-photon code
C MSTP(191)=1 => NO LEPTON PAIR PRODUCTION (SSR 3/2/96)
          IF (MSTP(191).EQ.1) THEN
          DO 110 II=MDCY(22,2),MDCY(22,2)+MDCY(22,3)-1
          IF(IABS(KFDP(II,1)).GE.10) MDME(II,1)=MIN(0,MDME(II,1))
  110     CONTINUE
          ENDIF
C END SSR
*DGC End
          IF(IPTL.EQ.1) CKIN(3)=PTMDIR
 
C...Set up for direct * anomalous gamma.
        ELSEIF(MINT(122).EQ.5) THEN
          MINT(123)=6
          MSUB(33)=1
          MSUB(54)=1
          IF(IPTL.EQ.1) CKIN(3)=PTMANO
 
C...Set up for anomalous * anomalous gamma.
        ELSEIF(MINT(122).EQ.6) THEN
          MINT(123)=3
          MSUB(11)=1
          MSUB(12)=1
          MSUB(13)=1
          MSUB(28)=1
          MSUB(53)=1
          MSUB(68)=1
          IF(MSTP(82).GE.2) MSTP(85)=1
          IF(IPTL.EQ.1) CKIN(3)=PTMANO
        ENDIF
 
C...End of special set up for gamma-p and gamma-gamma.
        CKIN(1)=2.*CKIN(3)
      ENDIF
 
C...Flavour information for individual beams.
      DO 120 I=1,2
      MINT(40+I)=1
      IF(MINT(123).GE.1.AND.MINT(10+I).EQ.22) MINT(40+I)=2
      IF(IABS(MINT(10+I)).GT.100) MINT(40+I)=2
      IF(MINT(10+I).EQ.28.OR.MINT(10+I).EQ.29) MINT(40+I)=2
      MINT(44+I)=MINT(40+I)
      IF(MSTP(11).GE.1.AND.IABS(MINT(10+I)).EQ.11) MINT(44+I)=3
  120 CONTINUE
 
C...If two gammas, whereof one direct, pick the first.
      IF(MINT(11).EQ.22.AND.MINT(12).EQ.22) THEN
        IF(MINT(123).GE.4.AND.MINT(123).LE.6) THEN
          MINT(41)=1
          MINT(45)=1
        ENDIF
      ELSEIF(MINT(11).EQ.22.OR.MINT(12).EQ.22) THEN
        IF(MINT(123).GE.4) CALL LUERRM(26,
     &  '(PYINPR:) unallowed MSTP(14) code for single photon')
      ENDIF
 
C...Flavour information on combination of incoming particles.
      MINT(43)=2*MINT(41)+MINT(42)-2
      MINT(44)=MINT(43)
      IF(MINT(123).LE.0) THEN
        IF(MINT(11).EQ.22) MINT(43)=MINT(43)+2
        IF(MINT(12).EQ.22) MINT(43)=MINT(43)+1
      ELSEIF(MINT(123).LE.3) THEN
        IF(MINT(11).EQ.22) MINT(44)=MINT(44)-2
        IF(MINT(12).EQ.22) MINT(44)=MINT(44)-1
      ELSEIF(MINT(11).EQ.22.AND.MINT(12).EQ.22) THEN
        MINT(43)=4
        MINT(44)=1
      ENDIF
      MINT(47)=2*MIN(2,MINT(45))+MIN(2,MINT(46))-2
      IF(MIN(MINT(45),MINT(46)).EQ.3) MINT(47)=5
      MINT(50)=0
      IF(MINT(41).EQ.2.AND.MINT(42).EQ.2) MINT(50)=1
      IF((MINT(11).EQ.22.OR.MINT(12).EQ.22).AND.MINT(123).GE.3)
     &MINT(50)=0
      MINT(107)=0
      IF(MINT(11).EQ.22) THEN
        MINT(107)=MINT(123)
        IF(MINT(123).GE.4) MINT(107)=0
        IF(MINT(123).EQ.7) MINT(107)=2
      ENDIF
      MINT(108)=0
      IF(MINT(12).EQ.22) THEN
        MINT(108)=MINT(123)
        IF(MINT(123).GE.4) MINT(108)=MINT(123)-3
        IF(MINT(123).EQ.7) MINT(108)=3
      ENDIF
 
C...Select default processes according to incoming beams
C...(already done for gamma-p and gamma-gamma with MSTP(14)=10).
      IF(MINT(121).GT.1) THEN
      ELSEIF(MSEL.EQ.1.OR.MSEL.EQ.2) THEN
 
        IF(MINT(43).EQ.1) THEN
C...Lepton + lepton -> gamma/Z0 or W.
          IF(MINT(11)+MINT(12).EQ.0) MSUB(1)=1
          IF(MINT(11)+MINT(12).NE.0) MSUB(2)=1
 
        ELSEIF(MINT(43).LE.3.AND.MINT(123).EQ.0.AND.
     &  (MINT(11).EQ.22.OR.MINT(12).EQ.22)) THEN
C...Unresolved photon + lepton: Compton scattering.
          MSUB(34)=1
 
        ELSEIF(MINT(43).LE.3) THEN
C...Lepton + hadron: deep inelastic scattering.
          MSUB(10)=1
 
        ELSEIF(MINT(123).EQ.0.AND.MINT(11).EQ.22.AND.
     &  MINT(12).EQ.22) THEN
C...Two unresolved photons: fermion pair production.
          MSUB(58)=1
 
        ELSEIF((MINT(123).EQ.0.AND.(MINT(11).EQ.22.OR.MINT(12).EQ.22))
     &  .OR.(MINT(123).GE.4.AND.MINT(123).LE.6.AND.MINT(11).EQ.22.AND.
     &   MINT(12).EQ.22)) THEN
C...Unresolved photon + hadron: photon-parton scattering.
          MSUB(33)=1
          MSUB(34)=1
          MSUB(54)=1
 
        ELSEIF(MSEL.EQ.1) THEN
C...High-pT QCD processes:
          MSUB(11)=1
          MSUB(12)=1
          MSUB(13)=1
          MSUB(28)=1
          MSUB(53)=1
          MSUB(68)=1
          IF(MSTP(82).LE.1.AND.CKIN(3).LT.PARP(81)) MSUB(95)=1
          IF(MSTP(82).GE.2.AND.CKIN(3).LT.PARP(82)) MSUB(95)=1
          IF(MSUB(95).EQ.1.AND.MINT(50).EQ.0) MSUB(95)=0
 
        ELSE
C...All QCD processes:
          MSUB(11)=1
          MSUB(12)=1
          MSUB(13)=1
          MSUB(28)=1
          MSUB(53)=1
          MSUB(68)=1
          MSUB(91)=1
          MSUB(92)=1
          MSUB(93)=1
          MSUB(94)=1
          MSUB(95)=1
        ENDIF
 
      ELSEIF(MSEL.GE.4.AND.MSEL.LE.8) THEN
C...Heavy quark production.
        MSUB(81)=1
        MSUB(82)=1
        MSUB(84)=1
        DO 130 J=1,MIN(8,MDCY(21,3))
        MDME(MDCY(21,2)+J-1,1)=0
  130   CONTINUE
        MDME(MDCY(21,2)+MSEL-1,1)=1
        MSUB(85)=1
        DO 140 J=1,MIN(12,MDCY(22,3))
        MDME(MDCY(22,2)+J-1,1)=0
  140   CONTINUE
        MDME(MDCY(22,2)+MSEL-1,1)=1
 
      ELSEIF(MSEL.EQ.10) THEN
C...Prompt photon production:
        MSUB(14)=1
        MSUB(18)=1
        MSUB(29)=1
 
      ELSEIF(MSEL.EQ.11) THEN
C...Z0/gamma* production:
        MSUB(1)=1
 
      ELSEIF(MSEL.EQ.12) THEN
C...W+/- production:
        MSUB(2)=1
 
      ELSEIF(MSEL.EQ.13) THEN
C...Z0 + jet:
        MSUB(15)=1
        MSUB(30)=1
 
      ELSEIF(MSEL.EQ.14) THEN
C...W+/- + jet:
        MSUB(16)=1
        MSUB(31)=1
 
      ELSEIF(MSEL.EQ.15) THEN
C...Z0 & W+/- pair production:
        MSUB(19)=1
        MSUB(20)=1
        MSUB(22)=1
        MSUB(23)=1
        MSUB(25)=1
 
      ELSEIF(MSEL.EQ.16) THEN
C...H0 production:
        MSUB(3)=1
        MSUB(102)=1
        MSUB(103)=1
        MSUB(123)=1
        MSUB(124)=1
 
      ELSEIF(MSEL.EQ.17) THEN
C...H0 & Z0 or W+/- pair production:
        MSUB(24)=1
        MSUB(26)=1
 
      ELSEIF(MSEL.EQ.18) THEN
C...H0 production; interesting processes in e+e-.
        MSUB(24)=1
        MSUB(103)=1
        MSUB(123)=1
        MSUB(124)=1
 
      ELSEIF(MSEL.EQ.19) THEN
C...H0, H'0 and A0 production; interesting processes in e+e-.
        MSUB(24)=1
        MSUB(103)=1
        MSUB(123)=1
        MSUB(124)=1
        MSUB(153)=1
        MSUB(171)=1
        MSUB(173)=1
        MSUB(174)=1
        MSUB(158)=1
        MSUB(176)=1
        MSUB(178)=1
        MSUB(179)=1
 
      ELSEIF(MSEL.EQ.21) THEN
C...Z'0 production:
        MSUB(141)=1
 
      ELSEIF(MSEL.EQ.22) THEN
C...W'+/- production:
        MSUB(142)=1
 
      ELSEIF(MSEL.EQ.23) THEN
C...H+/- production:
        MSUB(143)=1
 
      ELSEIF(MSEL.EQ.24) THEN
C...R production:
        MSUB(144)=1
 
      ELSEIF(MSEL.EQ.25) THEN
C...LQ (leptoquark) production.
        MSUB(145)=1
        MSUB(162)=1
        MSUB(163)=1
        MSUB(164)=1
 
      ELSEIF(MSEL.GE.35.AND.MSEL.LE.38) THEN
C...Production of one heavy quark (W exchange):
        MSUB(83)=1
        DO 150 J=1,MIN(8,MDCY(21,3))
        MDME(MDCY(21,2)+J-1,1)=0
  150   CONTINUE
        MDME(MDCY(21,2)+MSEL-31,1)=1
      ENDIF
 
C...Find heaviest new quark flavour allowed in processes 81-84.
      KFLQM=1
      DO 160 I=1,MIN(8,MDCY(21,3))
      IDC=I+MDCY(21,2)-1
      IF(MDME(IDC,1).LE.0) GOTO 160
      KFLQM=I
  160 CONTINUE
      IF(MSTP(7).GE.1.AND.MSTP(7).LE.8.AND.(MSEL.LE.3.OR.MSEL.GE.9))
     &KFLQM=MSTP(7)
      MINT(55)=KFLQM
      KFPR(81,1)=KFLQM
      KFPR(81,2)=KFLQM
      KFPR(82,1)=KFLQM
      KFPR(82,2)=KFLQM
      KFPR(83,1)=KFLQM
      KFPR(84,1)=KFLQM
      KFPR(84,2)=KFLQM
 
C...Find heaviest new fermion flavour allowed in process 85.
      KFLFM=1
      DO 170 I=1,MIN(12,MDCY(22,3))
      IDC=I+MDCY(22,2)-1
      IF(MDME(IDC,1).LE.0) GOTO 170
      KFLFM=KFDP(IDC,1)
  170 CONTINUE
      IF(((MSTP(7).GE.1.AND.MSTP(7).LE.8).OR.(MSTP(7).GE.11.AND.
     &MSTP(7).LE.18)).AND.(MSEL.LE.3.OR.MSEL.GE.9)) KFLFM=MSTP(7)
      MINT(56)=KFLFM
      KFPR(85,1)=KFLFM
      KFPR(85,2)=KFLFM
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYXTOT
 
C...Parametrizes total, elastic and diffractive cross-sections
C...for different energies and beams. Donnachie-Landshoff for
C...total and Schuler-Sjostrand for elastic and diffractive.
C...Process code IPROC:
C...=  1 : p + p;
C...=  2 : pbar + p;
C...=  3 : pi+ + p;
C...=  4 : pi- + p;
C...=  5 : pi0 + p;
C...=  6 : phi + p;
C...=  7 : J/psi + p;
C...= 11 : rho + rho;
C...= 12 : rho + phi;
C...= 13 : rho + J/psi;
C...= 14 : phi + phi;
C...= 15 : phi + J/psi;
C...= 16 : J/psi + J/psi;
C...= 21 : gamma + p (DL);
C...= 22 : gamma + p (VDM).
C...= 23 : gamma + pi (DL);
C...= 24 : gamma + pi (VDM);
C...= 25 : gamma + gamma (DL);
C...= 26 : gamma + gamma (VDM).
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      REAL  SIGT
      SAVE /PYINT7/
      DIMENSION NPROC(30),XPAR(30),YPAR(30),IHADA(20),IHADB(20),
     &PMHAD(4),BHAD(4),BETP(4),IFITSD(20),IFITDD(20),CEFFS(10,10),
     &CEFFD(10,10),SIGTMP(6,0:5)
 
C...Common constants.
      DATA EPS/0.0808/, ETA/-0.4525/, ALP/0.25/, CRES/2./, PMRC/1.062/,
     &SMP/0.880/, FACEL/0.0511/, FACSD/0.0336/, FACDD/0.0084/
 
C...Number of multiple processes to be evaluated (= 0 : undefined).
      DATA NPROC/7*1,3*0,6*1,4*0,4*3,2*6,4*0/
C...X and Y parameters of sigmatot = X * s**epsilon + Y * s**(-eta).
      DATA XPAR/2*21.70,3*13.63,10.01,0.970,3*0.,
     &8.56,6.29,0.609,4.62,0.447,0.0434,4*0.,
     &0.0677,0.0534,0.0425,0.0335,2.11E-4,1.31E-4,4*0./
      DATA YPAR/56.08,98.39,27.56,36.02,31.79,-1.51,-0.146,3*0.,
     &13.08,-0.62,-0.060,0.030,-0.0028,0.00028,4*0.,
     &0.129,0.115,0.081,0.072,2.15E-4,1.70E-4,4*0./
 
C...Beam and target hadron class:
C...= 1 : p/n ; = 2 : pi/rho/omega; = 3 : phi; = 4 : J/psi.
      DATA IHADA/2*1,3*2,3,4,3*0,3*2,2*3,4,4*0/
      DATA IHADB/7*1,3*0,2,3,4,3,2*4,4*0/
C...Characteristic class masses, slope parameters, beta = sqrt(X).
      DATA PMHAD/0.938,0.770,1.020,3.097/
      DATA BHAD/2.3,1.4,1.4,0.23/
      DATA BETP/4.658,2.926,2.149,0.208/
 
C...Fitting constants used in parametrizations of diffractive results.
      DATA IFITSD/2*1,3*2,3,4,3*0,5,6,7,8,9,10,4*0/
      DATA IFITDD/2*1,3*2,3,4,3*0,5,6,7,8,9,10,4*0/
      DATA ((CEFFS(J1,J2),J2=1,10),J1=1,10)/
     & 0.213, 0.0, -0.47, 150., 0.213, 0.0, -0.47, 150., 0., 0.,
     & 0.213, 0.0, -0.47, 150., 0.267, 0.0, -0.47, 100., 0., 0.,
     & 0.213, 0.0, -0.47, 150., 0.232, 0.0, -0.47, 110., 0., 0.,
     & 0.213, 7.0, -0.55, 800., 0.115, 0.0, -0.47, 110., 0., 0.,
     & 0.267, 0.0, -0.46,  75., 0.267, 0.0, -0.46,  75., 0., 0.,
     & 0.232, 0.0, -0.46,  85., 0.267, 0.0, -0.48, 100., 0., 0.,
     & 0.115, 0.0, -0.50,  90., 0.267, 6.0, -0.56, 420., 0., 0.,
     & 0.232, 0.0, -0.48, 110., 0.232, 0.0, -0.48, 110., 0., 0.,
     & 0.115, 0.0, -0.52, 120., 0.232, 6.0, -0.56, 470., 0., 0.,
     & 0.115, 5.5, -0.58, 570., 0.115, 5.5, -0.58, 570., 0., 0./
      DATA ((CEFFD(J1,J2),J2=1,10),J1=1,10)/
     & 3.11, -7.34,  9.71, 0.068, -0.42, 1.31, -1.37,  35.0,  118., 0.,
     & 3.11, -7.10,  10.6, 0.073, -0.41, 1.17, -1.41,  31.6,   95., 0.,
     & 3.12, -7.43,  9.21, 0.067, -0.44, 1.41, -1.35,  36.5,  132., 0.,
     & 3.13, -8.18, -4.20, 0.056, -0.71, 3.12, -1.12,  55.2, 1298., 0.,
     & 3.11, -6.90,  11.4, 0.078, -0.40, 1.05, -1.40,  28.4,   78., 0.,
     & 3.11, -7.13,  10.0, 0.071, -0.41, 1.23, -1.34,  33.1,  105., 0.,
     & 3.12, -7.90, -1.49, 0.054, -0.64, 2.72, -1.13,  53.1,  995., 0.,
     & 3.11, -7.39,  8.22, 0.065, -0.44, 1.45, -1.36,  38.1,  148., 0.,
     & 3.18, -8.95, -3.37, 0.057, -0.76, 3.32, -1.12,  55.6, 1472., 0.,
     & 4.18, -29.2,  56.2, 0.074, -1.36, 6.67, -1.14, 116.2, 6532., 0./
 
C...Parameters. Combinations of the energy.
      AEM=PARU(101)
      PMTH=PARP(102)
      S=VINT(2)
      SRT=VINT(1)
      SEPS=S**EPS
      SETA=S**ETA
      SLOG=LOG(S)
 
C...Ratio of gamma/pi (for rescaling in structure functions).
      VINT(281)=(XPAR(22)*SEPS+YPAR(22)*SETA)/
     &(XPAR(5)*SEPS+YPAR(5)*SETA)
      IF(MINT(50).NE.1) RETURN
 
C...Order flavours of incoming particles: KF1 < KF2.
      IF(IABS(MINT(11)).LE.IABS(MINT(12))) THEN
        KF1=IABS(MINT(11))
        KF2=IABS(MINT(12))
        IORD=1
      ELSE
        KF1=IABS(MINT(12))
        KF2=IABS(MINT(11))
        IORD=2
      ENDIF
      ISGN12=ISIGN(1,MINT(11)*MINT(12))
 
C...Find process number (for lookup tables).
      IF(KF1.GT.1000) THEN
        IPROC=1
        IF(ISGN12.LT.0) IPROC=2
      ELSEIF(KF1.GT.100.AND.KF2.GT.1000) THEN
        IPROC=3
        IF(ISGN12.LT.0) IPROC=4
        IF(KF1.EQ.111) IPROC=5
      ELSEIF(KF1.GT.100) THEN
        IPROC=11
      ELSEIF(KF2.GT.1000) THEN
        IPROC=21
        IF(MINT(123).EQ.2) IPROC=22
      ELSEIF(KF2.GT.100) THEN
        IPROC=23
        IF(MINT(123).EQ.2) IPROC=24
      ELSE
        IPROC=25
        IF(MINT(123).EQ.2) IPROC=26
      ENDIF
 
C... Number of multiple processes to be stored; beam/target side.
      NPR=NPROC(IPROC)
      MINT(101)=1
      MINT(102)=1
      IF(NPR.EQ.3) THEN
        MINT(100+IORD)=4
      ELSEIF(NPR.EQ.6) THEN
        MINT(101)=4
        MINT(102)=4
      ENDIF
      N1=0
      IF(MINT(101).EQ.4) N1=4
      N2=0
      IF(MINT(102).EQ.4) N2=4
 
C...Do not do any more for user-set or undefined cross-sections.
      IF(MSTP(31).LE.0) RETURN
      IF(NPR.EQ.0) CALL LUERRM(26,
     &'(PYXTOT:) cross section for this process not yet implemented')
 
C...Parameters. Combinations of the energy.
      AEM=PARU(101)
      PMTH=PARP(102)
      S=VINT(2)
      SRT=VINT(1)
      SEPS=S**EPS
      SETA=S**ETA
      SLOG=LOG(S)
 
C...Loop over multiple processes (for VDM).
      DO 110 I=1,NPR
      IF(NPR.EQ.1) THEN
        IPR=IPROC
      ELSEIF(NPR.EQ.3) THEN
        IPR=I+4
        IF(KF2.LT.1000) IPR=I+10
      ELSEIF(NPR.EQ.6) THEN
        IPR=I+10
      ENDIF
 
C...Evaluate hadron species, mass, slope contribution and fit number.
      IHA=IHADA(IPR)
      IHB=IHADB(IPR)
      PMA=PMHAD(IHA)
      PMB=PMHAD(IHB)
      BHA=BHAD(IHA)
      BHB=BHAD(IHB)
      ISD=IFITSD(IPR)
      IDD=IFITDD(IPR)
 
C...Skip if energy too low relative to masses.
      DO 100 J=0,5
      SIGTMP(I,J)=0.
  100 CONTINUE
      IF(SRT.LT.1.5*(PMA+PMB)) GOTO 110
 
C...Total cross-section. Elastic slope parameter and cross-section.
      SIGTMP(I,0)=XPAR(IPR)*SEPS+YPAR(IPR)*SETA
      BEL=2.*BHA+2.*BHB+4.*SEPS-4.2
      SIGTMP(I,1)=FACEL*SIGTMP(I,0)**2/BEL
 
C...Diffractive scattering A + B -> X + B.
      BSD=2.*BHB
      SQML=(PMA+PMTH)**2
      SQMU=S*CEFFS(ISD,1)+CEFFS(ISD,2)
      SUM1=LOG((BSD+2.*ALP*LOG(S/SQML))/
     &(BSD+2.*ALP*LOG(S/SQMU)))/(2.*ALP)
      BXB=CEFFS(ISD,3)+CEFFS(ISD,4)/S
      SUM2=CRES*LOG(1.+((PMA+PMRC)/(PMA+PMTH))**2)/
     &(BSD+2.*ALP*LOG(S/((PMA+PMTH)*(PMA+PMRC)))+BXB)
      SIGTMP(I,2)=FACSD*XPAR(IPR)*BETP(IHB)*MAX(0.,SUM1+SUM2)
 
C...Diffractive scattering A + B -> A + X.
      BSD=2.*BHA
      SQML=(PMB+PMTH)**2
      SQMU=S*CEFFS(ISD,5)+CEFFS(ISD,6)
      SUM1=LOG((BSD+2.*ALP*LOG(S/SQML))/
     &(BSD+2.*ALP*LOG(S/SQMU)))/(2.*ALP)
      BAX=CEFFS(ISD,7)+CEFFS(ISD,8)/S
      SUM2=CRES*LOG(1.+((PMB+PMRC)/(PMB+PMTH))**2)/
     &(BSD+2.*ALP*LOG(S/((PMB+PMTH)*(PMB+PMRC)))+BAX)
      SIGTMP(I,3)=FACSD*XPAR(IPR)*BETP(IHA)*MAX(0.,SUM1+SUM2)
 
C...Order single diffractive correctly.
      IF(IORD.EQ.2) THEN
        SIGSAV=SIGTMP(I,2)
        SIGTMP(I,2)=SIGTMP(I,3)
        SIGTMP(I,3)=SIGSAV
      ENDIF
 
C...Double diffractive scattering A + B -> X1 + X2.
      YEFF=LOG(S*SMP/((PMA+PMTH)*(PMB+PMTH))**2)
      DEFF=CEFFD(IDD,1)+CEFFD(IDD,2)/SLOG+CEFFD(IDD,3)/SLOG**2
      SUM1=DEFF+YEFF*(LOG(MAX(1E-10,YEFF/DEFF))-1.)/(2.*ALP)
      IF(YEFF.LE.0) SUM1=0.
      SQMU=S*(CEFFD(IDD,4)+CEFFD(IDD,5)/SLOG+CEFFD(IDD,6)/SLOG**2)
      SLUP=LOG(MAX(1.1,S/(ALP*(PMA+PMTH)**2*(PMB+PMTH)*(PMB+PMRC))))
      SLDN=LOG(MAX(1.1,S/(ALP*SQMU*(PMB+PMTH)*(PMB+PMRC))))
      SUM2=CRES*LOG(1.+((PMB+PMRC)/(PMB+PMTH))**2)*LOG(SLUP/SLDN)/
     &(2.*ALP)
      SLUP=LOG(MAX(1.1,S/(ALP*(PMB+PMTH)**2*(PMA+PMTH)*(PMA+PMRC))))
      SLDN=LOG(MAX(1.1,S/(ALP*SQMU*(PMA+PMTH)*(PMA+PMRC))))
      SUM3=CRES*LOG(1.+((PMA+PMRC)/(PMA+PMTH))**2)*LOG(SLUP/SLDN)/
     &(2.*ALP)
      BXX=CEFFD(IDD,7)+CEFFD(IDD,8)/SRT+CEFFD(IDD,9)/S
      SLRR=LOG(S/(ALP*(PMA+PMTH)*(PMA+PMRC)*(PMB+PMTH)*(PMB*PMRC)))
      SUM4=CRES**2*LOG(1.+((PMA+PMRC)/(PMA+PMTH))**2)*
     &LOG(1.+((PMB+PMRC)/(PMB+PMTH))**2)/MAX(0.1,2.*ALP*SLRR+BXX)
      SIGTMP(I,4)=FACDD*XPAR(IPR)*MAX(0.,SUM1+SUM2+SUM3+SUM4)
 
C...Non-diffractive by unitarity.
      SIGTMP(I,5)=SIGTMP(I,0)-SIGTMP(I,1)-SIGTMP(I,2)-SIGTMP(I,3)-
     &SIGTMP(I,4)
  110 CONTINUE
 
C...Put temporary results in output array: only one process.
      IF(MINT(101).EQ.1.AND.MINT(102).EQ.1) THEN
        DO 120 J=0,5
        SIGT(0,0,J)=SIGTMP(1,J)
  120   CONTINUE
 
C...Beam multiple processes.
      ELSEIF(MINT(101).EQ.4.AND.MINT(102).EQ.1) THEN
        DO 140 I=1,4
        CONV=AEM/PARP(160+I)
        I1=MAX(1,I-1)
        DO 130 J=0,5
        SIGT(I,0,J)=CONV*SIGTMP(I1,J)
  130   CONTINUE
  140   CONTINUE
        DO 150 J=0,5
        SIGT(0,0,J)=SIGT(1,0,J)+SIGT(2,0,J)+SIGT(3,0,J)+SIGT(4,0,J)
  150   CONTINUE
 
C...Target multiple processes.
      ELSEIF(MINT(101).EQ.1.AND.MINT(102).EQ.4) THEN
        DO 170 I=1,4
        CONV=AEM/PARP(160+I)
        IV=MAX(1,I-1)
        DO 160 J=0,5
        SIGT(0,I,J)=CONV*SIGTMP(IV,J)
  160   CONTINUE
  170   CONTINUE
        DO 180 J=0,5
        SIGT(0,0,J)=SIGT(0,1,J)+SIGT(0,2,J)+SIGT(0,3,J)+SIGT(0,4,J)
  180   CONTINUE
 
C...Both beam and target multiple processes.
      ELSE
        DO 210 I1=1,4
        DO 200 I2=1,4
        CONV=AEM**2/(PARP(160+I1)*PARP(160+I2))
        IF(I1.LE.2) THEN
          IV=MAX(1,I2-1)
        ELSEIF(I2.LE.2) THEN
          IV=MAX(1,I1-1)
        ELSEIF(I1.EQ.I2) THEN
          IV=2*I1-2
        ELSE
          IV=5
        ENDIF
        DO 190 J=0,5
        JV=J
        IF(I2.GT.I1.AND.(J.EQ.2.OR.J.EQ.3)) JV=5-J
        SIGT(I1,I2,J)=CONV*SIGTMP(IV,JV)
  190   CONTINUE
  200   CONTINUE
  210   CONTINUE
        DO 230 J=0,5
        DO 220 I=1,4
        SIGT(I,0,J)=SIGT(I,1,J)+SIGT(I,2,J)+SIGT(I,3,J)+SIGT(I,4,J)
        SIGT(0,I,J)=SIGT(1,I,J)+SIGT(2,I,J)+SIGT(3,I,J)+SIGT(4,I,J)
  220   CONTINUE
        SIGT(0,0,J)=SIGT(1,0,J)+SIGT(2,0,J)+SIGT(3,0,J)+SIGT(4,0,J)
  230   CONTINUE
      ENDIF
 
C...Scale up uniformly for Donnachie-Landshoff parametrization.
      IF(IPROC.EQ.21.OR.IPROC.EQ.23.OR.IPROC.EQ.25) THEN
        RFAC=(XPAR(IPROC)*SEPS+YPAR(IPROC)*SETA)/SIGT(0,0,0)
        DO 260 I1=0,N1
        DO 250 I2=0,N2
        DO 240 J=0,5
        SIGT(I1,I2,J)=RFAC*SIGT(I1,I2,J)
  240   CONTINUE
  250   CONTINUE
  260   CONTINUE
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
CAV      SUBROUTINE PYMAXI
      SUBROUTINE PYMAXICAV
 
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
 
C*********************************************************************
 
      SUBROUTINE PYPILE(MPILE)
 
C...Initializes multiplicity distribution and selects mutliplicity
C...of pileup events, i.e. several events occuring at the same
C...beam crossing.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      REAL  SIGT
      SAVE /PYINT7/
      DIMENSION WTI(0:200)
      SAVE IMIN,IMAX,WTI,WTS
C...Sum of allowed cross-sections for pileup events.
      IF(MPILE.EQ.1) THEN
        VINT(131)=SIGT(0,0,5)
        IF(MSTP(132).GE.2) VINT(131)=VINT(131)+SIGT(0,0,4)
        IF(MSTP(132).GE.3) VINT(131)=VINT(131)+SIGT(0,0,2)+SIGT(0,0,3)
        IF(MSTP(132).GE.4) VINT(131)=VINT(131)+SIGT(0,0,1)
        IF(MSTP(133).LE.0) RETURN
 
C...Initialize multiplicity distribution at maximum.
        XNAVE=VINT(131)*PARP(131)
        IF(XNAVE.GT.120.) WRITE(MSTU(11),5000) XNAVE
        INAVE=MAX(1,MIN(200,NINT(XNAVE)))
        WTI(INAVE)=1.
        WTS=WTI(INAVE)
        WTN=WTI(INAVE)*INAVE
 
C...Find shape of multiplicity distribution below maximum.
        IMIN=INAVE
        DO 100 I=INAVE-1,1,-1
        IF(MSTP(133).EQ.1) WTI(I)=WTI(I+1)*(I+1)/XNAVE
        IF(MSTP(133).GE.2) WTI(I)=WTI(I+1)*I/XNAVE
        IF(WTI(I).LT.1E-6) GOTO 110
        WTS=WTS+WTI(I)
        WTN=WTN+WTI(I)*I
        IMIN=I
  100   CONTINUE
 
C...Find shape of multiplicity distribution above maximum.
  110   IMAX=INAVE
        DO 120 I=INAVE+1,200
        IF(MSTP(133).EQ.1) WTI(I)=WTI(I-1)*XNAVE/I
        IF(MSTP(133).GE.2) WTI(I)=WTI(I-1)*XNAVE/(I-1)
        IF(WTI(I).LT.1E-6) GOTO 130
        WTS=WTS+WTI(I)
        WTN=WTN+WTI(I)*I
        IMAX=I
  120   CONTINUE
  130   VINT(132)=XNAVE
        VINT(133)=WTN/WTS
        IF(MSTP(133).EQ.1.AND.IMIN.EQ.1) VINT(134)=
     &  WTS/(WTS+WTI(1)/XNAVE)
        IF(MSTP(133).EQ.1.AND.IMIN.GT.1) VINT(134)=1.
        IF(MSTP(133).GE.2) VINT(134)=XNAVE
 
C...Pick multiplicity of pileup events.
      ELSE
        IF(MSTP(133).LE.0) THEN
          MINT(81)=MAX(1,MSTP(134))
        ELSE
          WTR=WTS*RLU(0)
          DO 140 I=IMIN,IMAX
          MINT(81)=I
          WTR=WTR-WTI(I)
          IF(WTR.LE.0.) GOTO 150
  140     CONTINUE
  150     CONTINUE
        ENDIF
      ENDIF
 
C...Format statement for error message.
 5000 FORMAT(1X,'Warning: requested average number of events per bunch',
     &'crossing too large, ',1P,E12.4)
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSAVE(ISAVE,IGA)
 
C...Saves and restores parameter and cross section values for the
C...3 gamma-p and 6 gamma-gamma alnternatives. Also makes random
C...choice between alternatives.
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
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYINT9/DXSEC(0:200)
      DOUBLE PRECISION DXSEC
      SAVE /PYINT9/
      DIMENSION NCP(10),NSUBCP(10,20),MSUBCP(10,20),COEFCP(10,20,20),
     &NGENCP(10,0:20,3),XSECCP(10,0:20,3),INTCP(10,20),RECP(10,20)
      DOUBLE PRECISION DXSECC(10,0:20)
      SAVE NCP,NSUBCP,MSUBCP,COEFCP,NGENCP,XSECCP,INTCP,RECP,DXSECC
C...Save list of subprocesses and cross-section information.
      IF(ISAVE.EQ.1) THEN
        ICP=0
        DO 120 I=1,200
        IF(MSUB(I).EQ.0.AND.I.NE.96.AND.I.NE.97) GOTO 120
        ICP=ICP+1
        NSUBCP(IGA,ICP)=I
        MSUBCP(IGA,ICP)=MSUB(I)
        DO 100 J=1,20
        COEFCP(IGA,ICP,J)=COEF(I,J)
  100   CONTINUE
        DO 110 J=1,3
        NGENCP(IGA,ICP,J)=NGEN(I,J)
        XSECCP(IGA,ICP,J)=XSEC(I,J)
  110   CONTINUE
        DXSECC(IGA,ICP)=DXSEC(I)
  120   CONTINUE
        NCP(IGA)=ICP
        DO 130 J=1,3
        NGENCP(IGA,0,J)=NGEN(0,J)
        XSECCP(IGA,0,J)=XSEC(0,J)
  130   CONTINUE
        DXSECC(IGA,0)=DXSEC(0)
C...Save various common process variables.
        DO 140 J=1,10
        INTCP(IGA,J)=MINT(40+J)
  140   CONTINUE
        INTCP(IGA,11)=MINT(101)
        INTCP(IGA,12)=MINT(102)
        INTCP(IGA,13)=MINT(107)
        INTCP(IGA,14)=MINT(108)
        INTCP(IGA,15)=MINT(123)
        RECP(IGA,1)=CKIN(3)
 
C...Save cross-section information only.
      ELSEIF(ISAVE.EQ.2) THEN
        DO 160 ICP=1,NCP(IGA)
        I=NSUBCP(IGA,ICP)
        DO 150 J=1,3
        NGENCP(IGA,ICP,J)=NGEN(I,J)
        XSECCP(IGA,ICP,J)=XSEC(I,J)
  150   CONTINUE
        DXSECC(IGA,ICP)=DXSEC(I)
  160   CONTINUE
        DO 170 J=1,3
        NGENCP(IGA,0,J)=NGEN(0,J)
        XSECCP(IGA,0,J)=XSEC(0,J)
  170   CONTINUE
        DXSECC(IGA,0)=DXSEC(0)
 
C...Choose between allowed alternatives.
      ELSEIF(ISAVE.EQ.3.OR.ISAVE.EQ.4) THEN
        IF(ISAVE.EQ.4) THEN
          XSUMCP=0.
          DO 180 IG=1,MINT(121)
          XSUMCP=XSUMCP+XSECCP(IG,0,1)
  180     CONTINUE
          XSUMCP=XSUMCP*RLU(0)
          DO 190 IG=1,MINT(121)
          IGA=IG
          XSUMCP=XSUMCP-XSECCP(IG,0,1)
          IF(XSUMCP.LE.0.) GOTO 200
  190     CONTINUE
  200     CONTINUE
        ENDIF
 
C...Restore cross-section information.
        DO 210 I=1,200
        MSUB(I)=0
  210   CONTINUE
        DO 240 ICP=1,NCP(IGA)
        I=NSUBCP(IGA,ICP)
        MSUB(I)=MSUBCP(IGA,ICP)
        DO 220 J=1,20
        COEF(I,J)=COEFCP(IGA,ICP,J)
  220   CONTINUE
        DO 230 J=1,3
        NGEN(I,J)=NGENCP(IGA,ICP,J)
        XSEC(I,J)=XSECCP(IGA,ICP,J)
  230   CONTINUE
        DXSEC(I)=DXSECC(IGA,ICP)
  240   CONTINUE
        DO 250 J=1,3
        NGEN(0,J)=NGENCP(IGA,0,J)
        XSEC(0,J)=XSECCP(IGA,0,J)
  250   CONTINUE
        DXSEC(0)=DXSECC(IGA,0)
 
C...Restore various common process variables.
        DO 260 J=1,10
        MINT(40+J)=INTCP(IGA,J)
  260   CONTINUE
        MINT(101)=INTCP(IGA,11)
        MINT(102)=INTCP(IGA,12)
        MINT(107)=INTCP(IGA,13)
        MINT(108)=INTCP(IGA,14)
        MINT(123)=INTCP(IGA,15)
        CKIN(3)=RECP(IGA,1)
        CKIN(1)=2.*CKIN(3)
 
C...Sum up cross-section info (for PYSTAT).
      ELSEIF(ISAVE.EQ.5) THEN
        DO 270 I=1,200
        MSUB(I)=0
        NGEN(I,1)=0
        NGEN(I,3)=0
        XSEC(I,3)=0.
  270   CONTINUE
        NGEN(0,1)=0
        NGEN(0,2)=0
        NGEN(0,3)=0
        XSEC(0,3)=0
        DO 290 IG=1,MINT(121)
        DO 280 ICP=1,NCP(IG)
        I=NSUBCP(IG,ICP)
        IF(MSUBCP(IG,ICP).EQ.1) MSUB(I)=1
        NGEN(I,1)=NGEN(I,1)+NGENCP(IG,ICP,1)
        NGEN(I,3)=NGEN(I,3)+NGENCP(IG,ICP,3)
        XSEC(I,3)=XSEC(I,3)+XSECCP(IG,ICP,3)
  280   CONTINUE
        NGEN(0,1)=NGEN(0,1)+NGENCP(IG,0,1)
        NGEN(0,2)=NGEN(0,2)+NGENCP(IG,0,2)
        NGEN(0,3)=NGEN(0,3)+NGENCP(IG,0,3)
        XSEC(0,3)=XSEC(0,3)+XSECCP(IG,0,3)
  290   CONTINUE
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYRAND
 
C...Generates quantities characterizing the high-pT scattering at the
C...parton level according to the matrix elements. Chooses incoming,
C...reacting partons, their momentum fractions and one of the possible
C...subprocesses.
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
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      REAL  SIGT
      SAVE /PYINT7/
      COMMON/PYINT9/DXSEC(0:200)
      DOUBLE PRECISION DXSEC
      SAVE /PYINT9/
      COMMON/PYUPPR/NUP,KUP(20,7),PUP(20,5),NFUP,IFUP(10,2),Q2UP(0:10)
      INTEGER  NUP,KUP,NFUP,IFUP
      REAL  PUP,Q2UP
      SAVE /PYUPPR/
      DIMENSION XPQ(-25:25),PMM(2),PDIF(4),BHAD(4)
*DGC
*1/4/96 Added next line following two-photon loop trap
      MSTI(191)=0
*DGC End
 
C...Parameters and data used in elastic/diffractive treatment.
      DATA EPS/0.0808/, ALP/0.25/, CRES/2./, PMRC/1.062/, SMP/0.880/
      DATA BHAD/2.3,1.4,1.4,0.23/
 
C...Initial values, specifically for (first) semihard interaction.
      MINT(10)=0
      MINT(17)=0
      MINT(18)=0
      VINT(143)=1.
      VINT(144)=1.
      MFAIL=0
      IF(MSTP(171).EQ.1.AND.MSTP(172).EQ.2) MFAIL=1
      ISUB=0
      LOOP=0
  100 LOOP=LOOP+1
      MINT(51)=0
 
C...Choice of process type - first event of pileup.
      IF(MINT(82).EQ.1.AND.(ISUB.LE.90.OR.ISUB.GT.96)) THEN
 
C...For gamma-p or gamma-gamma first pick between alternatives.
        IF(MINT(121).GT.1) CALL PYSAVE(4,IGA)
        MINT(122)=IGA
 
C...For gamma + gamma with different nature, flip at random.
        IF(MINT(11).EQ.22.AND.MINT(12).EQ.22.AND.MINT(123).GE.4.AND.
     &  RLU(0).GT.0.5) THEN
          MINTSV=MINT(41)
          MINT(41)=MINT(42)
          MINT(42)=MINTSV
          MINTSV=MINT(45)
          MINT(45)=MINT(46)
          MINT(46)=MINTSV
          MINTSV=MINT(107)
          MINT(107)=MINT(108)
          MINT(108)=MINTSV
          IF(MINT(47).EQ.2.OR.MINT(47).EQ.3) MINT(47)=5-MINT(47)
        ENDIF
 
C...Pick process type.
        RSUB=XSEC(0,1)*RLU(0)
        DO 110 I=1,200
        IF(MSUB(I).NE.1) GOTO 110
        ISUB=I
        RSUB=RSUB-XSEC(I,1)
        IF(RSUB.LE.0.) GOTO 120
  110   CONTINUE
  120   IF(ISUB.EQ.95) ISUB=96
        IF(ISUB.EQ.96) CALL PYMULT(2)
 
C...Choice of inclusive process type - pileup events.
      ELSEIF(MINT(82).GE.2.AND.ISUB.EQ.0) THEN
        RSUB=VINT(131)*RLU(0)
        ISUB=96
        IF(RSUB.GT.SIGT(0,0,5)) ISUB=94
        IF(RSUB.GT.SIGT(0,0,5)+SIGT(0,0,4)) ISUB=93
        IF(RSUB.GT.SIGT(0,0,5)+SIGT(0,0,4)+SIGT(0,0,3)) ISUB=92
        IF(RSUB.GT.SIGT(0,0,5)+SIGT(0,0,4)+SIGT(0,0,3)+SIGT(0,0,2))
     &  ISUB=91
        IF(ISUB.EQ.96) CALL PYMULT(2)
      ENDIF
      IF(MINT(82).EQ.1) NGEN(0,1)=NGEN(0,1)+1
      IF(MINT(82).EQ.1) NGEN(ISUB,1)=NGEN(ISUB,1)+1
      IF(ISUB.EQ.96.AND.LOOP.EQ.1.AND.MINT(82).EQ.1)
     &NGEN(97,1)=NGEN(97,1)+1
      MINT(1)=ISUB
      ISTSB=ISET(ISUB)
 
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
 
C...Find product masses and minimum pT of process,
C...optionally with broadening according to a truncated Breit-Wigner.
      VINT(63)=0.
      VINT(64)=0.
      MINT(71)=0
      VINT(71)=CKIN(3)
      IF(MINT(82).GE.2) VINT(71)=0.
      VINT(80)=1.
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4) THEN
        NBW=0
        DO 130 I=1,2
        IF(KFPR(ISUB,I).EQ.0) THEN
        ELSEIF(MSTP(42).LE.0.OR.PMAS(LUCOMP(KFPR(ISUB,I)),2).LT.
     &  PARP(41)) THEN
          VINT(62+I)=PMAS(LUCOMP(KFPR(ISUB,I)),1)**2
        ELSE
          NBW=NBW+1
        ENDIF
  130   CONTINUE
        IF(NBW.GE.1) THEN
          CALL PYOFSH(4,0,KFPR(ISUB,1),KFPR(ISUB,2),0.,PQM3,PQM4)
          IF(MINT(51).EQ.1) THEN
            IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
            IF(MFAIL.EQ.1) THEN
              MSTI(61)=1
              RETURN
            ENDIF
            GOTO 100
          ENDIF
          VINT(63)=PQM3**2
          VINT(64)=PQM4**2
        ENDIF
        IF(MIN(VINT(63),VINT(64)).LT.CKIN(6)**2) MINT(71)=1
        IF(MINT(71).EQ.1) VINT(71)=MAX(CKIN(3),CKIN(5))
      ELSEIF(ISTSB.EQ.6) THEN
        CALL PYOFSH(6,0,KFPR(ISUB,1),KFPR(ISUB,2),0.,PQM3,PQM4)
        IF(MINT(51).EQ.1) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
        VINT(63)=PQM3**2
        VINT(64)=PQM4**2
      ENDIF
 
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
 
C...Select incoming VDM particle (rho/omega/phi/J/psi).
      IF(ISTSB.NE.0.AND.(MINT(101).GE.2.OR.MINT(102).GE.2).AND.
     &(MINT(123).EQ.2.OR.MINT(123).EQ.5.OR.MINT(123).EQ.7)) THEN
        VRN=RLU(0)*SIGT(0,0,5)
        IF(MINT(101).LE.1) THEN
          I1MN=0
          I1MX=0
        ELSE
          I1MN=1
          I1MX=MINT(101)
        ENDIF
        IF(MINT(102).LE.1) THEN
          I2MN=0
          I2MX=0
        ELSE
          I2MN=1
          I2MX=MINT(102)
        ENDIF
        DO 150 I1=I1MN,I1MX
        KFV1=110*I1+3
        DO 140 I2=I2MN,I2MX
        KFV2=110*I2+3
        VRN=VRN-SIGT(I1,I2,5)
        IF(VRN.LE.0.) GOTO 160
  140   CONTINUE
  150   CONTINUE
  160   IF(MINT(101).GE.2) MINT(103)=KFV1
        IF(MINT(102).GE.2) MINT(104)=KFV2
      ENDIF
 
      IF(ISTSB.EQ.0) THEN
C...Elastic scattering or single or double diffractive scattering.
 
C...Select incoming particle (rho/omega/phi/J/psi for VDM) and mass.
        MINT(103)=MINT(11)
        MINT(104)=MINT(12)
        PMM(1)=VINT(3)
        PMM(2)=VINT(4)
        IF(MINT(101).GE.2.OR.MINT(102).GE.2) THEN
          JJ=ISUB-90
          VRN=RLU(0)*SIGT(0,0,JJ)
          IF(MINT(101).LE.1) THEN
            I1MN=0
            I1MX=0
          ELSE
            I1MN=1
            I1MX=MINT(101)
          ENDIF
          IF(MINT(102).LE.1) THEN
            I2MN=0
            I2MX=0
          ELSE
            I2MN=1
            I2MX=MINT(102)
          ENDIF
          DO 180 I1=I1MN,I1MX
          KFV1=110*I1+3
          DO 170 I2=I2MN,I2MX
          KFV2=110*I2+3
          VRN=VRN-SIGT(I1,I2,JJ)
          IF(VRN.LE.0.) GOTO 190
  170     CONTINUE
  180     CONTINUE
  190     IF(MINT(101).GE.2) THEN
            MINT(103)=KFV1
            PMM(1)=ULMASS(KFV1)
          ENDIF
          IF(MINT(102).GE.2) THEN
            MINT(104)=KFV2
            PMM(2)=ULMASS(KFV2)
          ENDIF
        ENDIF
 
C...Side/sides of diffractive system.
        MINT(17)=0
        MINT(18)=0
        IF(ISUB.EQ.92.OR.ISUB.EQ.94) MINT(17)=1
        IF(ISUB.EQ.93.OR.ISUB.EQ.94) MINT(18)=1
 
C...Find masses of particles and minimal masses of diffractive states.
        DO 200 JT=1,2
        PDIF(JT)=PMM(JT)
        VINT(66+JT)=PDIF(JT)
        IF(MINT(16+JT).EQ.1) PDIF(JT)=PDIF(JT)+PARP(102)
  200   CONTINUE
        SH=VINT(2)
        SQM1=PMM(1)**2
        SQM2=PMM(2)**2
        SQM3=PDIF(1)**2
        SQM4=PDIF(2)**2
        SMRES1=(PMM(1)+PMRC)**2
        SMRES2=(PMM(2)+PMRC)**2
 
C...Find elastic slope and lower limit diffractive slope.
        IHA=MAX(2,IABS(MINT(103))/110)
        IF(IHA.GE.5) IHA=1
        IHB=MAX(2,IABS(MINT(104))/110)
        IF(IHB.GE.5) IHB=1
        IF(ISUB.EQ.91) THEN
          BMN=2.*BHAD(IHA)+2.*BHAD(IHB)+4.*SH**EPS-4.2
        ELSEIF(ISUB.EQ.92) THEN
          BMN=MAX(2.,2.*BHAD(IHB))
        ELSEIF(ISUB.EQ.93) THEN
          BMN=MAX(2.,2.*BHAD(IHA))
        ELSEIF(ISUB.EQ.94) THEN
          BMN=2.*ALP*4.
        ENDIF
 
C...Determine maximum possible t range and coefficient of generation.
        SQLA12=(SH-SQM1-SQM2)**2-4.*SQM1*SQM2
        SQLA34=(SH-SQM3-SQM4)**2-4.*SQM3*SQM4
        THA=SH-(SQM1+SQM2+SQM3+SQM4)+(SQM1-SQM2)*(SQM3-SQM4)/SH
        THB=SQRT(MAX(0.,SQLA12))*SQRT(MAX(0.,SQLA34))/SH
        THC=(SQM3-SQM1)*(SQM4-SQM2)+(SQM1+SQM4-SQM2-SQM3)*
     &  (SQM1*SQM4-SQM2*SQM3)/SH
        THL=-0.5*(THA+THB)
        THU=THC/THL
        THRND=EXP(MAX(-50.,BMN*(THL-THU)))-1.
 
C...Select diffractive mass/masses according to dm^2/m^2.
  210   DO 220 JT=1,2
        IF(MINT(16+JT).EQ.0) THEN
          PDIF(2+JT)=PDIF(JT)
        ELSE
          PMMIN=PDIF(JT)
          PMMAX=MAX(VINT(2+JT),VINT(1)-PDIF(3-JT))
          PDIF(2+JT)=PMMIN*(PMMAX/PMMIN)**RLU(0)
        ENDIF
  220   CONTINUE
        SQM3=PDIF(3)**2
        SQM4=PDIF(4)**2

*DGC
*1/4/96 Added next 5 lines following two-photon loop trap 
        MSTI(191)=MSTI(191)+1
        IF(MSTI(191).GE.1000) THEN
          WRITE(6,*) 'EVENT HAD MORE THAN 1000 ITERATIONS'
         RETURN
        ENDIF
*DGC End
C..Additional mass factors, including resonance enhancement.
        IF(PDIF(3)+PDIF(4).GE.VINT(1)) GOTO 210
        IF(ISUB.EQ.92) THEN
          FSD=(1.-SQM3/SH)*(1.+CRES*SMRES1/(SMRES1+SQM3))
          IF(FSD.LT.RLU(0)*(1.+CRES)) GOTO 210
        ELSEIF(ISUB.EQ.93) THEN
          FSD=(1.-SQM4/SH)*(1.+CRES*SMRES2/(SMRES2+SQM4))
          IF(FSD.LT.RLU(0)*(1.+CRES)) GOTO 210
        ELSEIF(ISUB.EQ.94) THEN
          FDD=(1.-(PDIF(3)+PDIF(4))**2/SH)*(SH*SMP/(SH*SMP+SQM3*SQM4))*
     &    (1.+CRES*SMRES1/(SMRES1+SQM3))*(1.+CRES*SMRES2/(SMRES2+SQM4))
          IF(FDD.LT.RLU(0)*(1.+CRES)**2) GOTO 210
        ENDIF
 
C...Select t according to exp(Bmn*t) and correct to right slope.
        TH=THU+LOG(1.+THRND*RLU(0))/BMN
        IF(ISUB.GE.92) THEN
          IF(ISUB.EQ.92) THEN
            BADD=2.*ALP*LOG(SH/SQM3)
            IF(BHAD(IHB).LT.1.) BADD=MAX(0.,BADD+2.*BHAD(IHB)-2.)
          ELSEIF(ISUB.EQ.93) THEN
            BADD=2.*ALP*LOG(SH/SQM4)
            IF(BHAD(IHA).LT.1.) BADD=MAX(0.,BADD+2.*BHAD(IHA)-2.)
          ELSEIF(ISUB.EQ.94) THEN
            BADD=2.*ALP*(LOG(EXP(4.)+SH/(ALP*SQM3*SQM4))-4.)
          ENDIF
          IF(EXP(MAX(-50.,BADD*(TH-THU))).LT.RLU(0)) GOTO 210
        ENDIF
 
C...Check whether m^2 and t choices are consistent.
        SQLA34=(SH-SQM3-SQM4)**2-4.*SQM3*SQM4
        THA=SH-(SQM1+SQM2+SQM3+SQM4)+(SQM1-SQM2)*(SQM3-SQM4)/SH
        THB=SQRT(MAX(0.,SQLA12))*SQRT(MAX(0.,SQLA34))/SH
        IF(THB.LE.1E-8) GOTO 210
        THC=(SQM3-SQM1)*(SQM4-SQM2)+(SQM1+SQM4-SQM2-SQM3)*
     &  (SQM1*SQM4-SQM2*SQM3)/SH
        THLM=-0.5*(THA+THB)
        THUM=THC/THLM
        IF(TH.LT.THLM.OR.TH.GT.THUM) GOTO 210
 
C...Information to output.
        VINT(21)=1.
        VINT(22)=0.
        VINT(23)=MIN(1.,MAX(-1.,(THA+2.*TH)/THB))
        VINT(45)=TH
        VINT(59)=2.*SQRT(MAX(0.,-(THC+THA*TH+TH**2)))/THB
        VINT(63)=PDIF(3)**2
        VINT(64)=PDIF(4)**2
 
C...Note: in the following, by In is meant the integral over the
C...quantity multiplying coefficient cn.
C...Choose tau according to h1(tau)/tau, where
C...h1(tau) = c1 + I1/I2*c2*1/tau + I1/I3*c3*1/(tau+tau_R) +
C...I1/I4*c4*tau/((s*tau-m^2)^2+(m*Gamma)^2) +
C...I1/I5*c5*1/(tau+tau_R') +
C...I1/I6*c6*tau/((s*tau-m'^2)^2+(m'*Gamma')^2) +
C...I1/I7*c7*tau/(1.-tau), and
C...c1 + c2 + c3 + c4 + c5 + c6 + c7 = 1.
      ELSEIF(ISTSB.GE.1.AND.ISTSB.LE.6) THEN
        CALL PYKLIM(1)
        IF(MINT(51).NE.0) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
        RTAU=RLU(0)
        MTAU=1
        IF(RTAU.GT.COEF(ISUB,1)) MTAU=2
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)) MTAU=3
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)) MTAU=4
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)+COEF(ISUB,4))
     &  MTAU=5
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)+COEF(ISUB,4)+
     &  COEF(ISUB,5)) MTAU=6
        IF(RTAU.GT.COEF(ISUB,1)+COEF(ISUB,2)+COEF(ISUB,3)+COEF(ISUB,4)+
     &  COEF(ISUB,5)+COEF(ISUB,6)) MTAU=7
        CALL PYKMAP(1,MTAU,RLU(0))
 
C...2 -> 3, 4 processes:
C...Choose tau' according to h4(tau,tau')/tau', where
C...h4(tau,tau') = c1 + I1/I2*c2*(1 - tau/tau')^3/tau' +
C...I1/I3*c3*1/(1 - tau'), and c1 + c2 + c3 = 1.
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) THEN
          CALL PYKLIM(4)
          IF(MINT(51).NE.0) THEN
            IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
            IF(MFAIL.EQ.1) THEN
              MSTI(61)=1
              RETURN
            ENDIF
            GOTO 100
          ENDIF
          RTAUP=RLU(0)
          MTAUP=1
          IF(RTAUP.GT.COEF(ISUB,18)) MTAUP=2
          IF(RTAUP.GT.COEF(ISUB,18)+COEF(ISUB,19)) MTAUP=3
          CALL PYKMAP(4,MTAUP,RLU(0))
        ENDIF
 
C...Choose y* according to h2(y*), where
C...h2(y*) = I0/I1*c1*(y*-y*min) + I0/I2*c2*(y*max-y*) +
C...I0/I3*c3*1/cosh(y*) + I0/I4*c4*1/(1-exp(y*-y*max)) +
C...I0/I5*c5*1/(1-exp(-y*-y*min)), I0 = y*max-y*min,
C...and c1 + c2 + c3 + c4 + c5 = 1.
        CALL PYKLIM(2)
        IF(MINT(51).NE.0) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
        RYST=RLU(0)
        MYST=1
        IF(RYST.GT.COEF(ISUB,8)) MYST=2
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)) MYST=3
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)+COEF(ISUB,10)) MYST=4
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)+COEF(ISUB,10)+
     &  COEF(ISUB,11)) MYST=5
        CALL PYKMAP(2,MYST,RLU(0))
 
C...2 -> 2 processes:
C...Choose cos(theta-hat) (cth) according to h3(cth), where
C...h3(cth) = c0 + I0/I1*c1*1/(A - cth) + I0/I2*c2*1/(A + cth) +
C...I0/I3*c3*1/(A - cth)^2 + I0/I4*c4*1/(A + cth)^2,
C...A = 1 + 2*(m3*m4/sh)^2 (= 1 for massless products),
C...and c0 + c1 + c2 + c3 + c4 = 1.
        CALL PYKLIM(3)
        IF(MINT(51).NE.0) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
        IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) THEN
          RCTH=RLU(0)
          MCTH=1
          IF(RCTH.GT.COEF(ISUB,13)) MCTH=2
          IF(RCTH.GT.COEF(ISUB,13)+COEF(ISUB,14)) MCTH=3
          IF(RCTH.GT.COEF(ISUB,13)+COEF(ISUB,14)+COEF(ISUB,15)) MCTH=4
          IF(RCTH.GT.COEF(ISUB,13)+COEF(ISUB,14)+COEF(ISUB,15)+
     &    COEF(ISUB,16)) MCTH=5
          CALL PYKMAP(3,MCTH,RLU(0))
        ENDIF
 
C...2 -> 3 : select pT1, phi1, pT2, phi2, y3 for 3 outgoing.
        IF(ISTSB.EQ.5) THEN
          CALL PYKMAP(5,0,0.)
          IF(MINT(51).NE.0) THEN
            IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
            IF(MFAIL.EQ.1) THEN
              MSTI(61)=1
              RETURN
            ENDIF
            GOTO 100
          ENDIF
        ENDIF
 
C...Low-pT or multiple interactions (first semihard interaction).
      ELSEIF(ISTSB.EQ.9) THEN
        CALL PYMULT(3)
        ISUB=MINT(1)
 
C...Generate user-defined process: kinematics plus weight.
      ELSEIF(ISTSB.EQ.11) THEN
        MSTI(51)=0
        CALL PYUPEV(ISUB,SIGS)
        IF(NUP.LE.0) THEN
          MINT(51)=2
          MSTI(51)=1
          IF(MINT(82).EQ.1) THEN
            NGEN(0,1)=NGEN(0,1)-1
            NGEN(0,2)=NGEN(0,2)-1
            NGEN(ISUB,1)=NGEN(ISUB,1)-1
          ENDIF
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          RETURN
        ENDIF
 
C...Construct 'trivial' kinematical variables needed.
        KFL1=KUP(1,2)
        KFL2=KUP(2,2)
        VINT(41)=2.*PUP(1,4)/VINT(1)
        VINT(42)=2.*PUP(2,4)/VINT(1)
        VINT(21)=VINT(41)*VINT(42)
        VINT(22)=0.5*LOG(VINT(41)/VINT(42))
        VINT(44)=VINT(21)*VINT(2)
        VINT(43)=SQRT(MAX(0.,VINT(44)))
        VINT(56)=Q2UP(0)
        VINT(55)=SQRT(MAX(0.,VINT(56)))
 
C...Construct other kinematical variables needed (approximately).
        VINT(23)=0.
        VINT(26)=VINT(21)
        VINT(45)=-0.5*VINT(44)
        VINT(46)=-0.5*VINT(44)
        VINT(49)=VINT(43)
        VINT(50)=VINT(44)
        VINT(51)=VINT(55)
        VINT(52)=VINT(56)
        VINT(53)=VINT(55)
        VINT(54)=VINT(56)
        VINT(25)=0.
        VINT(48)=0.
        DO 230 IUP=3,NUP
        IF(KUP(IUP,1).EQ.1) VINT(25)=VINT(25)+2.*(PUP(IUP,5)**2+
     &  PUP(IUP,1)**2+PUP(IUP,2)**2)/VINT(1)
        IF(KUP(IUP,1).EQ.1) VINT(48)=VINT(48)+0.5*(PUP(IUP,1)**2+
     &  PUP(IUP,2)**2)
  230   CONTINUE
        VINT(47)=SQRT(VINT(48))
 
C...Calculate structure function weights.
        IF(MINT(47).GE.2) THEN
          DO 250 I=3-MIN(2,MINT(45)),MIN(2,MINT(46))
          MINT(105)=MINT(102+I)
          MINT(109)=MINT(106+I)
          IF(MSTP(57).LE.1) THEN
            CALL PYSTFU(MINT(10+I),VINT(40+I),Q2UP(0),XPQ)
          ELSE
            CALL PYSTFL(MINT(10+I),VINT(40+I),Q2UP(0),XPQ)
          ENDIF
          DO 240 KFL=-25,25
          XSFX(I,KFL)=XPQ(KFL)
  240     CONTINUE
  250     CONTINUE
        ENDIF
      ENDIF
 
C...Choose azimuthal angle.
      VINT(24)=PARU(2)*RLU(0)
 
C...Check against user cuts on kinematics at parton level.
      MINT(51)=0
      IF((ISUB.LE.90.OR.ISUB.GT.100).AND.ISTSB.LE.10) CALL PYKLIM(0)
      IF(MINT(51).NE.0) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
      IF(MINT(82).EQ.1.AND.MSTP(141).GE.1.AND.ISTSB.LE.10) THEN
        MCUT=0
        IF(MSUB(91)+MSUB(92)+MSUB(93)+MSUB(94)+MSUB(95).EQ.0)
     &  CALL PYKCUT(MCUT)
        IF(MCUT.NE.0) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
      ENDIF
 
C...Calculate differential cross-section for different subprocesses.
      IF(ISTSB.LE.10) CALL PYSIGH(NCHN,SIGS)
      SIGSOR=SIGS
      SIGLPT=SIGT(0,0,5)
 
C...Multiply cross-section by user-defined weights.
      IF(MSTP(173).EQ.1) THEN
        SIGS=PARP(173)*SIGS
        DO 260 ICHN=1,NCHN
        SIGH(ICHN)=PARP(173)*SIGH(ICHN)
  260   CONTINUE
        SIGLPT=PARP(173)*SIGLPT
      ENDIF
      WTXS=1.
      SIGSWT=SIGS
      VINT(99)=1.
      VINT(100)=1.
      IF(MINT(82).EQ.1.AND.MSTP(142).GE.1) THEN
        IF(ISUB.NE.96.AND.MSUB(91)+MSUB(92)+MSUB(93)+MSUB(94)+
     &  MSUB(95).EQ.0) CALL PYEVWT(WTXS)
        SIGSWT=WTXS*SIGS
        VINT(99)=WTXS
        IF(MSTP(142).EQ.1) VINT(100)=1./WTXS
      ENDIF
 
C...Calculations for Monte Carlo estimate of all cross-sections.
      IF(MINT(82).EQ.1.AND.ISUB.LE.90.OR.ISUB.GE.96) THEN
        IF(MSTP(142).LE.1) THEN 
          XSEC(ISUB,2)=XSEC(ISUB,2)+SIGS
          DXSEC(ISUB)=DXSEC(ISUB)+SIGS
        ELSE
          XSEC(ISUB,2)=XSEC(ISUB,2)+SIGSWT
          DXSEC(ISUB)=DXSEC(ISUB)+SIGSWT
        ENDIF
      ELSEIF(MINT(82).EQ.1) THEN
        XSEC(ISUB,2)=XSEC(ISUB,2)+SIGS
        DXSEC(ISUB)=DXSEC(ISUB)+SIGS
      ENDIF
      IF((ISUB.EQ.95.OR.ISUB.EQ.96).AND.LOOP.EQ.1.AND.MINT(82).EQ.1)
     &THEN
        XSEC(97,2)=XSEC(97,2)+SIGLPT
        DXSEC(97)=DXSEC(97)+SIGLPT
      ENDIF

C...Multiple interactions: store results of cross-section calculation.
      IF(MINT(50).EQ.1.AND.MSTP(82).GE.3) THEN
        VINT(153)=SIGSOR
        CALL PYMULT(4)
      ENDIF
 
C...Check that weight not negative.
      VIOL=SIGSWT/XSEC(ISUB,1)
      IF(ISUB.EQ.96.AND.MSTP(173).EQ.1) VIOL=VIOL/PARP(174)
      IF(MSTP(123).LE.0) THEN
        IF(VIOL.LT.-1E-3) THEN
          WRITE(MSTU(11),5000) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
          STOP
        ENDIF
      ELSE
        IF(VIOL.LT.MIN(-1E-3,VINT(109))) THEN
          VINT(109)=VIOL
          WRITE(MSTU(11),5200) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
        ENDIF
      ENDIF
 
C...Weighting using estimate of maximum of differential cross-section.
      IF(MFAIL.EQ.0) THEN
        IF(VIOL.LT.RLU(0)) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          GOTO 100
        ENDIF
      ELSEIF(ISUB.NE.95.AND.ISUB.NE.96) THEN
        IF(VIOL.LT.RLU(0)) THEN
          MSTI(61)=1
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          RETURN
        ENDIF
      ELSE
        RATND=SIGLPT/XSEC(95,1)
        IF(LOOP.EQ.1.AND.RATND.LT.RLU(0)) THEN
          MSTI(61)=1
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          RETURN
        ENDIF
        VIOL=VIOL/RATND
        IF(VIOL.LT.RLU(0)) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          GOTO 100
        ENDIF
      ENDIF
 
C...Check for possible violation of estimated maximum of differential
C...cross-section used in weighting.
      IF(MSTP(123).LE.0) THEN
        IF(VIOL.GT.1.) THEN
          WRITE(MSTU(11),5300) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
          STOP
        ENDIF
      ELSEIF(MSTP(123).EQ.1) THEN
        IF(VIOL.GT.VINT(108)) THEN
          VINT(108)=VIOL
          IF(VIOL.GT.1.) THEN
            MINT(10)=1
            WRITE(MSTU(11),5400) VIOL,NGEN(0,3)+1
            WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),
     &      VINT(26)
          ENDIF
        ENDIF
      ELSEIF(VIOL.GT.VINT(108)) THEN
        VINT(108)=VIOL
        IF(VIOL.GT.1.) THEN
          MINT(10)=1
          XDIF=XSEC(ISUB,1)*(VIOL-1.)
          XSEC(ISUB,1)=XSEC(ISUB,1)+XDIF
          IF(MSUB(ISUB).EQ.1.AND.(ISUB.LE.90.OR.ISUB.GT.96))
     &    XSEC(0,1)=XSEC(0,1)+XDIF
          WRITE(MSTU(11),5400) VIOL,NGEN(0,3)+1
          WRITE(MSTU(11),5100) ISUB,VINT(21),VINT(22),VINT(23),VINT(26)
          IF(ISUB.LE.9) THEN
            WRITE(MSTU(11),5500) ISUB,XSEC(ISUB,1)
          ELSEIF(ISUB.LE.99) THEN
            WRITE(MSTU(11),5600) ISUB,XSEC(ISUB,1)
          ELSE
            WRITE(MSTU(11),5700) ISUB,XSEC(ISUB,1)
          ENDIF
          VINT(108)=1.
        ENDIF
      ENDIF
 
C...Multiple interactions: choose impact parameter.
      VINT(148)=1.
      IF(MINT(50).EQ.1.AND.(ISUB.LE.90.OR.ISUB.GE.96).AND.MSTP(82).GE.3)
     &THEN
        CALL PYMULT(5)
        IF(VINT(150).LT.RLU(0)) THEN
          IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
          IF(MFAIL.EQ.1) THEN
            MSTI(61)=1
            RETURN
          ENDIF
          GOTO 100
        ENDIF
      ENDIF
      IF(MINT(82).EQ.1) NGEN(0,2)=NGEN(0,2)+1
      IF(MINT(82).EQ.1.AND.MSUB(95).EQ.1) THEN
        IF(ISUB.LE.90.OR.ISUB.GE.95) NGEN(95,1)=NGEN(95,1)+1
        IF(ISUB.LE.90.OR.ISUB.GE.96) NGEN(96,2)=NGEN(96,2)+1
      ENDIF
      IF(ISUB.LE.90.OR.ISUB.GE.96) MINT(31)=MINT(31)+1
 
C...Choose flavour of reacting partons (and subprocess).
      IF(ISTSB.GE.11) GOTO 280
      RSIGS=SIGS*RLU(0)
      QT2=VINT(48)
      RQQBAR=PARP(87)*(1.-(QT2/(QT2+(PARP(88)*PARP(82))**2))**2)
      IF(ISUB.NE.95.AND.(ISUB.NE.96.OR.MSTP(82).LE.1.OR.
     &RLU(0).GT.RQQBAR)) THEN
        DO 270 ICHN=1,NCHN
        KFL1=ISIG(ICHN,1)
        KFL2=ISIG(ICHN,2)
        MINT(2)=ISIG(ICHN,3)
        RSIGS=RSIGS-SIGH(ICHN)
        IF(RSIGS.LE.0.) GOTO 280
  270   CONTINUE
 
C...Multiple interactions: choose qq~ preferentially at small pT.
      ELSEIF(ISUB.EQ.96) THEN
        MINT(105)=MINT(103)
        MINT(109)=MINT(107)
        CALL PYSPLI(MINT(11),21,KFL1,KFLDUM)
        MINT(105)=MINT(104)
        MINT(109)=MINT(108)
        CALL PYSPLI(MINT(12),21,KFL2,KFLDUM)
        MINT(1)=11
        MINT(2)=1
        IF(KFL1.EQ.KFL2.AND.RLU(0).LT.0.5) MINT(2)=2
 
C...Low-pT: choose string drawing configuration.
      ELSE
        KFL1=21
        KFL2=21
        RSIGS=6.*RLU(0)
        MINT(2)=1
        IF(RSIGS.GT.1.) MINT(2)=2
        IF(RSIGS.GT.2.) MINT(2)=3
      ENDIF
 
C...Reassign QCD process. Partons before initial state radiation.
  280 IF(MINT(2).GT.10) THEN
        MINT(1)=MINT(2)/10
        MINT(2)=MOD(MINT(2),10)
      ENDIF
      IF(MINT(82).EQ.1.AND.MSTP(111).GE.0) NGEN(MINT(1),2)=
     &NGEN(MINT(1),2)+1
      MINT(15)=KFL1
      MINT(16)=KFL2
      MINT(13)=MINT(15)
      MINT(14)=MINT(16)
      VINT(141)=VINT(41)
      VINT(142)=VINT(42)
      VINT(151)=0.
      VINT(152)=0.
 
C...Calculate x value of photon for parton inside photon inside e.
      DO 310 JT=1,2
      MINT(18+JT)=0
      VINT(154+JT)=0.
      MSPLI=0
      IF(JT.EQ.1.AND.MINT(43).LE.2) MSPLI=1
      IF(JT.EQ.2.AND.MOD(MINT(43),2).EQ.1) MSPLI=1
      IF(IABS(MINT(14+JT)).LE.8.OR.MINT(14+JT).EQ.21) MSPLI=MSPLI+1
      IF(MSPLI.EQ.2) THEN
        KFLH=MINT(14+JT)
        XHRD=VINT(140+JT)
        Q2HRD=VINT(54)
        MINT(105)=MINT(102+JT)
        MINT(109)=MINT(106+JT)
        IF(MSTP(57).LE.1) THEN
          CALL PYSTFU(22,XHRD,Q2HRD,XPQ)
        ELSE
          CALL PYSTFL(22,XHRD,Q2HRD,XPQ)
        ENDIF
        WTMX=4.*XPQ(KFLH)
        IF(MSTP(13).EQ.2) THEN
          Q2PMS=Q2HRD/PMAS(11,1)**2
          WTMX=WTMX*LOG(MAX(2.,Q2PMS*(1.-XHRD)/XHRD**2))
        ENDIF
  290   XE=XHRD**RLU(0)
        XG=MIN(0.999999,XHRD/XE)
        IF(MSTP(57).LE.1) THEN
          CALL PYSTFU(22,XG,Q2HRD,XPQ)
        ELSE
          CALL PYSTFL(22,XG,Q2HRD,XPQ)
        ENDIF
        WT=(1.+(1.-XE)**2)*XPQ(KFLH)
        IF(MSTP(13).EQ.2) WT=WT*LOG(MAX(2.,Q2PMS*(1.-XE)/XE**2))
        IF(WT.LT.RLU(0)*WTMX) GOTO 290
        MINT(18+JT)=1
        VINT(154+JT)=XE
        DO 300 KFLS=-25,25
        XSFX(JT,KFLS)=XPQ(KFLS)
  300   CONTINUE
      ENDIF
  310 CONTINUE
 
C...Pick scale where photon is resolved.
      IF(MINT(107).EQ.3) VINT(283)=PARP(15)**2*
     &(VINT(54)/PARP(15)**2)**RLU(0)
      IF(MINT(108).EQ.3) VINT(284)=PARP(15)**2*
     &(VINT(54)/PARP(15)**2)**RLU(0)
      IF(MINT(121).GT.1) CALL PYSAVE(2,IGA)
 
C...Format statements for differential cross-section maximum violations.
 5000 FORMAT(1X,'Error: negative cross-section fraction',1P,E11.3,1X,
     &'in event',1X,I7,'.'/1X,'Execution stopped!')
 5100 FORMAT(1X,'ISUB = ',I3,'; Point of violation:'/1X,'tau =',1P,
     &E11.3,', y* =',E11.3,', cthe = ',0P,F11.7,', tau'' =',1P,E11.3)
 5200 FORMAT(1X,'Warning: negative cross-section fraction',1P,E11.3,1X,
     &'in event',1X,I7)
 5300 FORMAT(1X,'Error: maximum violated by',1P,E11.3,1X,
     &'in event',1X,I7,'.'/1X,'Execution stopped!')
 5400 FORMAT(1X,'Warning: maximum violated by',1P,E11.3,1X,
     &'in event',1X,I7)
 5500 FORMAT(1X,'XSEC(',I1,',1) increased to',1P,E11.3)
 5600 FORMAT(1X,'XSEC(',I2,',1) increased to',1P,E11.3)
 5700 FORMAT(1X,'XSEC(',I3,',1) increased to',1P,E11.3)
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSCAT
 
C...Finds outgoing flavours and event type; sets up the kinematics
C...and colour flow of the hard scattering.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
      COMMON/PYUPPR/NUP,KUP(20,7),PUP(20,5),NFUP,IFUP(10,2),Q2UP(0:10)
      INTEGER  NUP,KUP,NFUP,IFUP
      REAL  PUP,Q2UP
      SAVE /PYUPPR/
      DIMENSION WDTP(0:40),WDTE(0:40,0:5),PMQ(2),Z(2),CTHE(2),PHI(2),
     &KUPPO(20),VINTSV(41:66)
      SAVE VINTSV
C...Read out process.
      ISUB=MINT(1)
      ISUBSV=ISUB
 
C...Restore information for low-pT processes.
      IF(ISUB.EQ.95.AND.MINT(57).GE.1) THEN
        DO 100 J=41,66
  100   VINT(J)=VINTSV(J)
      ENDIF
 
C...Convert H' or A process into equivalent H one.
      IHIGG=1
      KFHIGG=25
      IF((ISUB.GE.151.AND.ISUB.LE.160).OR.(ISUB.GE.171.AND.
     &ISUB.LE.190)) THEN
        IHIGG=2
        IF(MOD(ISUB-1,10).GE.5) IHIGG=3
        KFHIGG=33+IHIGG
        IF(ISUB.EQ.151.OR.ISUB.EQ.156) ISUB=3
        IF(ISUB.EQ.152.OR.ISUB.EQ.157) ISUB=102
        IF(ISUB.EQ.153.OR.ISUB.EQ.158) ISUB=103
        IF(ISUB.EQ.171.OR.ISUB.EQ.176) ISUB=24
        IF(ISUB.EQ.172.OR.ISUB.EQ.177) ISUB=26
        IF(ISUB.EQ.173.OR.ISUB.EQ.178) ISUB=123
        IF(ISUB.EQ.174.OR.ISUB.EQ.179) ISUB=124
        IF(ISUB.EQ.181.OR.ISUB.EQ.186) ISUB=121
        IF(ISUB.EQ.182.OR.ISUB.EQ.187) ISUB=122
      ENDIF
 
C...Choice of subprocess, number of documentation lines.
      IDOC=6+ISET(ISUB)
      IF(ISUB.EQ.95) IDOC=8
      IF(ISET(ISUB).EQ.5.OR.ISET(ISUB).EQ.6) IDOC=9
      IF(ISET(ISUB).EQ.11) IDOC=4+NUP
      MINT(3)=IDOC-6
      IF(IDOC.GE.9.AND.ISET(ISUB).LE.4) IDOC=IDOC+2
      MINT(4)=IDOC
      IPU1=MINT(84)+1
      IPU2=MINT(84)+2
      IPU3=MINT(84)+3
      IPU4=MINT(84)+4
      IPU5=MINT(84)+5
      IPU6=MINT(84)+6
 
C...Reset K, P and V vectors. Store incoming particles.
      DO 120 JT=1,MSTP(126)+20
      I=MINT(83)+JT
      DO 110 J=1,5
      K(I,J)=0
      P(I,J)=0.
      V(I,J)=0.
  110 CONTINUE
  120 CONTINUE
      DO 140 JT=1,2
      I=MINT(83)+JT
      K(I,1)=21
      K(I,2)=MINT(10+JT)
      DO 130 J=1,5
      P(I,J)=VINT(285+5*JT+J)
  130 CONTINUE
  140 CONTINUE
      MINT(6)=2
      KFRES=0
 
C...Store incoming partons in their CM-frame.
      SH=VINT(44)
      SHR=SQRT(SH)
      SHP=VINT(26)*VINT(2)
      SHPR=SQRT(SHP)
      SHUSER=SHR
      IF(ISET(ISUB).GE.3.AND.ISET(ISUB).LE.5) SHUSER=SHPR
      DO 150 JT=1,2
      I=MINT(84)+JT
      K(I,1)=14
      K(I,2)=MINT(14+JT)
      K(I,3)=MINT(83)+2+JT
      P(I,3)=0.5*SHUSER*(-1.)**(JT-1)
      P(I,4)=0.5*SHUSER
  150 CONTINUE
 
C...Copy incoming partons to documentation lines.
      DO 170 JT=1,2
      I1=MINT(83)+4+JT
      I2=MINT(84)+JT
      K(I1,1)=21
      K(I1,2)=K(I2,2)
      K(I1,3)=I1-2
      DO 160 J=1,5
      P(I1,J)=P(I2,J)
  160 CONTINUE
  170 CONTINUE
 
C...Choose new quark/lepton flavour for relevant annihilation graphs.
      IF(ISUB.EQ.12.OR.ISUB.EQ.53.OR.ISUB.EQ.54.OR.ISUB.EQ.58) THEN
        IGLGA=21
        IF(ISUB.EQ.58) IGLGA=22
        CALL PYWIDT(IGLGA,SH,WDTP,WDTE)
  180   RKFL=(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))*RLU(0)
        DO 190 I=1,MDCY(IGLGA,3)
        KFLF=KFDP(I+MDCY(IGLGA,2)-1,1)
        RKFL=RKFL-(WDTE(I,1)+WDTE(I,2)+WDTE(I,4))
        IF(RKFL.LE.0.) GOTO 200
  190   CONTINUE
  200   CONTINUE
        IF(ISUB.EQ.12.AND.MSTP(5).EQ.1.AND.IABS(MINT(15)).LE.2.AND.
     &  IABS(KFLF).GE.3) THEN
          FACQQB=VINT(58)**2*4./9.*(VINT(45)**2+VINT(46)**2)/
     &    VINT(44)**2
          FACCIB=VINT(46)**2/PARU(155)**4
          IF(FACQQB/(FACQQB+FACCIB).LT.RLU(0)) GOTO 180
        ELSEIF(ISUB.EQ.54) THEN
          IF((KCHG(IABS(KFLF),1)/2.)**2.LT.RLU(0)) GOTO 180
        ELSEIF(ISUB.EQ.58) THEN
          IF((KCHG(IABS(KFLF),1)/3.)**2.LT.RLU(0)) GOTO 180
        ENDIF
      ENDIF
 
C...Final state flavours and colour flow: default values.
      JS=1
      MINT(21)=MINT(15)
      MINT(22)=MINT(16)
      MINT(23)=0
      MINT(24)=0
      KCC=20
      KCS=ISIGN(1,MINT(15))
 
      IF(ISET(ISUB).EQ.11) THEN
C...User-defined processes: find products.
        IRUP=0
        DO 210 IUP=3,NUP
        IF(KUP(IUP,1).NE.1) THEN
        ELSEIF(IRUP.LE.5) THEN
          IRUP=IRUP+1
          MINT(20+IRUP)=KUP(IUP,2)
        ENDIF
  210   CONTINUE
 
      ELSEIF(ISUB.LE.10) THEN
      IF(ISUB.EQ.1) THEN
C...f + f~ -> gamma*/Z0.
        KFRES=23
 
      ELSEIF(ISUB.EQ.2) THEN
C...f + f~' -> W+/- .
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        KFRES=ISIGN(24,KCH1+KCH2)
 
      ELSEIF(ISUB.EQ.3) THEN
C...f + f~ -> H0 (or H'0, or A0).
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.4) THEN
C...gamma + W+/- -> W+/-.
 
      ELSEIF(ISUB.EQ.5) THEN
C...Z0 + Z0 -> H0.
        XH=SH/SHP
        MINT(21)=MINT(15)
        MINT(22)=MINT(16)
        PMQ(1)=ULMASS(MINT(21))
        PMQ(2)=ULMASS(MINT(22))
  220   JT=INT(1.5+RLU(0))
        ZMIN=2.*PMQ(JT)/SHPR
        ZMAX=1.-PMQ(3-JT)/SHPR-(SH-PMQ(JT)**2)/(SHPR*(SHPR-PMQ(3-JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        Z(JT)=ZMIN+(ZMAX-ZMIN)*RLU(0)
        IF(-1.+(1.+XH)/(1.-Z(JT))-XH/(1.-Z(JT))**2.LT.
     &  (1.-XH)**2/(4.*XH)*RLU(0)) GOTO 220
        SQC1=1.-4.*PMQ(JT)**2/(Z(JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 220
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(23,1)**2-PMQ(JT)**2)/(Z(JT)*SHP)
        CTHE(JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(JT)=MIN(1.,MAX(-1.,CTHE(JT)))
        Z(3-JT)=1.-XH/(1.-Z(JT))
        SQC1=1.-4.*PMQ(3-JT)**2/(Z(3-JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 220
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(23,1)**2-PMQ(3-JT)**2)/(Z(3-JT)*SHP)
        CTHE(3-JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(3-JT)=MIN(1.,MAX(-1.,CTHE(3-JT)))
        PHIR=PARU(2)*RLU(0)
        CPHI=COS(PHIR)
        ANG=CTHE(1)*CTHE(2)-SQRT(1.-CTHE(1)**2)*SQRT(1.-CTHE(2)**2)*CPHI
        Z1=2.-Z(JT)
        Z2=ANG*SQRT(Z(JT)**2-4.*PMQ(JT)**2/SHP)
        Z3=1.-Z(JT)-XH+(PMQ(1)**2+PMQ(2)**2)/SHP
        Z(3-JT)=2./(Z1**2-Z2**2)*(Z1*Z3+Z2*SQRT(Z3**2-(Z1**2-Z2**2)*
     &  PMQ(3-JT)**2/SHP))
        ZMIN=2.*PMQ(3-JT)/SHPR
        ZMAX=1.-PMQ(JT)/SHPR-(SH-PMQ(3-JT)**2)/(SHPR*(SHPR-PMQ(JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(Z(3-JT).LT.ZMIN.OR.Z(3-JT).GT.ZMAX) GOTO 220
        KCC=22
        KFRES=25
 
      ELSEIF(ISUB.EQ.6) THEN
C...Z0 + W+/- -> W+/-.
 
      ELSEIF(ISUB.EQ.7) THEN
C...W+ + W- -> Z0.
 
      ELSEIF(ISUB.EQ.8) THEN
C...W+ + W- -> H0.
        XH=SH/SHP
  230   DO 260 JT=1,2
        I=MINT(14+JT)
        IA=IABS(I)
        IF(IA.LE.10) THEN
          RVCKM=VINT(180+I)*RLU(0)
          DO 240 J=1,MSTP(1)
          IB=2*J-1+MOD(IA,2)
          IPM=(5-ISIGN(1,I))/2
          IDC=J+MDCY(IA,2)+2
          IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 240
          MINT(20+JT)=ISIGN(IB,I)
          RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
          IF(RVCKM.LE.0.) GOTO 250
  240     CONTINUE
        ELSE
          IB=2*((IA+1)/2)-1+MOD(IA,2)
          MINT(20+JT)=ISIGN(IB,I)
        ENDIF
  250   PMQ(JT)=ULMASS(MINT(20+JT))
  260   CONTINUE
        JT=INT(1.5+RLU(0))
        ZMIN=2.*PMQ(JT)/SHPR
        ZMAX=1.-PMQ(3-JT)/SHPR-(SH-PMQ(JT)**2)/(SHPR*(SHPR-PMQ(3-JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(ZMIN.GE.ZMAX) GOTO 230
        Z(JT)=ZMIN+(ZMAX-ZMIN)*RLU(0)
        IF(-1.+(1.+XH)/(1.-Z(JT))-XH/(1.-Z(JT))**2.LT.
     &  (1.-XH)**2/(4.*XH)*RLU(0)) GOTO 230
        SQC1=1.-4.*PMQ(JT)**2/(Z(JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 230
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(24,1)**2-PMQ(JT)**2)/(Z(JT)*SHP)
        CTHE(JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(JT)=MIN(1.,MAX(-1.,CTHE(JT)))
        Z(3-JT)=1.-XH/(1.-Z(JT))
        SQC1=1.-4.*PMQ(3-JT)**2/(Z(3-JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 230
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(24,1)**2-PMQ(3-JT)**2)/(Z(3-JT)*SHP)
        CTHE(3-JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(3-JT)=MIN(1.,MAX(-1.,CTHE(3-JT)))
        PHIR=PARU(2)*RLU(0)
        CPHI=COS(PHIR)
        ANG=CTHE(1)*CTHE(2)-SQRT(1.-CTHE(1)**2)*SQRT(1.-CTHE(2)**2)*CPHI
        Z1=2.-Z(JT)
        Z2=ANG*SQRT(Z(JT)**2-4.*PMQ(JT)**2/SHP)
        Z3=1.-Z(JT)-XH+(PMQ(1)**2+PMQ(2)**2)/SHP
        Z(3-JT)=2./(Z1**2-Z2**2)*(Z1*Z3+Z2*SQRT(Z3**2-(Z1**2-Z2**2)*
     &  PMQ(3-JT)**2/SHP))
        ZMIN=2.*PMQ(3-JT)/SHPR
        ZMAX=1.-PMQ(JT)/SHPR-(SH-PMQ(3-JT)**2)/(SHPR*(SHPR-PMQ(JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(Z(3-JT).LT.ZMIN.OR.Z(3-JT).GT.ZMAX) GOTO 230
        KCC=22
        KFRES=25
 
      ELSEIF(ISUB.EQ.10) THEN
C...f + f' -> f + f' (gamma/Z/W exchange); th = (p(f)-p(f))**2.
        IF(MINT(2).EQ.1) THEN
          KCC=22
        ELSE
C...W exchange: need to mix flavours according to CKM matrix.
          DO 280 JT=1,2
          I=MINT(14+JT)
          IA=IABS(I)
          IF(IA.LE.10) THEN
            RVCKM=VINT(180+I)*RLU(0)
            DO 270 J=1,MSTP(1)
            IB=2*J-1+MOD(IA,2)
            IPM=(5-ISIGN(1,I))/2
            IDC=J+MDCY(IA,2)+2
            IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 270
            MINT(20+JT)=ISIGN(IB,I)
            RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
            IF(RVCKM.LE.0.) GOTO 280
  270       CONTINUE
          ELSE
            IB=2*((IA+1)/2)-1+MOD(IA,2)
            MINT(20+JT)=ISIGN(IB,I)
          ENDIF
  280     CONTINUE
          KCC=22
        ENDIF
      ENDIF
 
      ELSEIF(ISUB.LE.20) THEN
      IF(ISUB.EQ.11) THEN
C...f + f' -> f + f' (g exchange); th = (p(f)-p(f))**2.
        KCC=MINT(2)
        IF(MINT(15)*MINT(16).LT.0) KCC=KCC+2
 
      ELSEIF(ISUB.EQ.12) THEN
C...f + f~ -> f' + f~'; th = (p(f)-p(f'))**2.
        MINT(21)=ISIGN(KFLF,MINT(15))
        MINT(22)=-MINT(21)
        KCC=4
 
      ELSEIF(ISUB.EQ.13) THEN
C...f + f~ -> g + g; th arbitrary.
        MINT(21)=21
        MINT(22)=21
        KCC=MINT(2)+4
 
      ELSEIF(ISUB.EQ.14) THEN
C...f + f~ -> g + gamma; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=21
        MINT(23-JS)=22
        KCC=17+JS
 
      ELSEIF(ISUB.EQ.15) THEN
C...f + f~ -> g + Z0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=21
        MINT(23-JS)=23
        KCC=17+JS
 
      ELSEIF(ISUB.EQ.16) THEN
C...f + f~' -> g + W+/-; th = (p(f)-p(W-))**2 or (p(f~')-p(W+))**2.
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        IF(MINT(15)*(KCH1+KCH2).LT.0) JS=2
        MINT(20+JS)=21
        MINT(23-JS)=ISIGN(24,KCH1+KCH2)
        KCC=17+JS
 
      ELSEIF(ISUB.EQ.17) THEN
C...f + f~ -> g + H0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=21
        MINT(23-JS)=25
        KCC=17+JS
 
      ELSEIF(ISUB.EQ.18) THEN
C...f + f~ -> gamma + gamma; th arbitrary.
        MINT(21)=22
        MINT(22)=22
 
      ELSEIF(ISUB.EQ.19) THEN
C...f + f~ -> gamma + Z0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=22
        MINT(23-JS)=23
 
      ELSEIF(ISUB.EQ.20) THEN
C...f + f~' -> gamma + W+/-; th = (p(f)-p(W-))**2 or (p(f~')-p(W+))**2.
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        IF(MINT(15)*(KCH1+KCH2).LT.0) JS=2
        MINT(20+JS)=22
        MINT(23-JS)=ISIGN(24,KCH1+KCH2)
      ENDIF
 
      ELSEIF(ISUB.LE.30) THEN
      IF(ISUB.EQ.21) THEN
C...f + f~ -> gamma + H0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=22
        MINT(23-JS)=25
 
      ELSEIF(ISUB.EQ.22) THEN
C...f + f~ -> Z0 + Z0; th arbitrary.
        MINT(21)=23
        MINT(22)=23
 
      ELSEIF(ISUB.EQ.23) THEN
C...f + f~' -> Z0 + W+/-; th = (p(f)-p(W-))**2 or (p(f~')-p(W+))**2.
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        IF(MINT(15)*(KCH1+KCH2).LT.0) JS=2
        MINT(20+JS)=23
        MINT(23-JS)=ISIGN(24,KCH1+KCH2)
 
      ELSEIF(ISUB.EQ.24) THEN
C...f + f~ -> Z0 + H0 (or H'0, or A0); th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=23
        MINT(23-JS)=KFHIGG
 
      ELSEIF(ISUB.EQ.25) THEN
C...f + f~ -> W+ + W-; th = (p(f)-p(W-))**2.
        MINT(21)=-ISIGN(24,MINT(15))
        MINT(22)=-MINT(21)
 
      ELSEIF(ISUB.EQ.26) THEN
C...f + f~' -> W+/- + H0 (or H'0, or A0);
C...th = (p(f)-p(W-))**2 or (p(f~')-p(W+))**2.
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        IF(MINT(15)*(KCH1+KCH2).GT.0) JS=2
        MINT(20+JS)=ISIGN(24,KCH1+KCH2)
        MINT(23-JS)=KFHIGG
 
      ELSEIF(ISUB.EQ.27) THEN
C...f + f~ -> H0 + H0.
 
      ELSEIF(ISUB.EQ.28) THEN
C...f + g -> f + g; th = (p(f)-p(f))**2.
        KCC=MINT(2)+6
        IF(MINT(15).EQ.21) KCC=KCC+2
        IF(MINT(15).NE.21) KCS=ISIGN(1,MINT(15))
        IF(MINT(16).NE.21) KCS=ISIGN(1,MINT(16))
 
      ELSEIF(ISUB.EQ.29) THEN
C...f + g -> f + gamma; th = (p(f)-p(f))**2.
        IF(MINT(15).EQ.21) JS=2
        MINT(23-JS)=22
        KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.30) THEN
C...f + g -> f + Z0; th = (p(f)-p(f))**2.
        IF(MINT(15).EQ.21) JS=2
        MINT(23-JS)=23
        KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
      ENDIF
 
      ELSEIF(ISUB.LE.40) THEN
      IF(ISUB.EQ.31) THEN
C...f + g -> f' + W+/-; th = (p(f)-p(f'))**2; choose flavour f'.
        IF(MINT(15).EQ.21) JS=2
        I=MINT(14+JS)
        IA=IABS(I)
        MINT(23-JS)=ISIGN(24,KCHG(IA,1)*I)
        RVCKM=VINT(180+I)*RLU(0)
        DO 290 J=1,MSTP(1)
        IB=2*J-1+MOD(IA,2)
        IPM=(5-ISIGN(1,I))/2
        IDC=J+MDCY(IA,2)+2
        IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 290
        MINT(20+JS)=ISIGN(IB,I)
        RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
        IF(RVCKM.LE.0.) GOTO 300
  290   CONTINUE
  300   KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.32) THEN
C...f + g -> f + H0; th = (p(f)-p(f))**2.
        IF(MINT(15).EQ.21) JS=2
        MINT(23-JS)=25
        KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.33) THEN
C...f + gamma -> f + g; th=(p(f)-p(f))**2.
        IF(MINT(15).EQ.22) JS=2
        MINT(23-JS)=21
        KCC=24+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.34) THEN
C...f + gamma -> f + gamma; th=(p(f)-p(f))**2.
        IF(MINT(15).EQ.22) JS=2
        KCC=22
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.35) THEN
C...f + gamma -> f + Z0; th=(p(f)-p(f))**2.
        IF(MINT(15).EQ.22) JS=2
        MINT(23-JS)=23
        KCC=22
 
      ELSEIF(ISUB.EQ.36) THEN
C...f + gamma -> f' + W+/-; th=(p(f)-p(f'))**2.
        IF(MINT(15).EQ.22) JS=2
        I=MINT(14+JS)
        IA=IABS(I)
        MINT(23-JS)=ISIGN(24,KCHG(IA,1)*I)
        IF(IA.LE.10) THEN
          RVCKM=VINT(180+I)*RLU(0)
          DO 310 J=1,MSTP(1)
          IB=2*J-1+MOD(IA,2)
          IPM=(5-ISIGN(1,I))/2
          IDC=J+MDCY(IA,2)+2
          IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 310
          MINT(20+JS)=ISIGN(IB,I)
          RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
          IF(RVCKM.LE.0.) GOTO 320
  310     CONTINUE
        ELSE
          IB=2*((IA+1)/2)-1+MOD(IA,2)
          MINT(20+JS)=ISIGN(IB,I)
        ENDIF
  320   KCC=22
 
      ELSEIF(ISUB.EQ.37) THEN
C...f + gamma -> f + H0.
 
      ELSEIF(ISUB.EQ.38) THEN
C...f + Z0 -> f + g.
 
      ELSEIF(ISUB.EQ.39) THEN
C...f + Z0 -> f + gamma.
 
      ELSEIF(ISUB.EQ.40) THEN
C...f + Z0 -> f + Z0.
      ENDIF
 
      ELSEIF(ISUB.LE.50) THEN
      IF(ISUB.EQ.41) THEN
C...f + Z0 -> f' + W+/-.
 
      ELSEIF(ISUB.EQ.42) THEN
C...f + Z0 -> f + H0.
 
      ELSEIF(ISUB.EQ.43) THEN
C...f + W+/- -> f' + g.
 
      ELSEIF(ISUB.EQ.44) THEN
C...f + W+/- -> f' + gamma.
 
      ELSEIF(ISUB.EQ.45) THEN
C...f + W+/- -> f' + Z0.
 
      ELSEIF(ISUB.EQ.46) THEN
C...f + W+/- -> f' + W+/-.
 
      ELSEIF(ISUB.EQ.47) THEN
C...f + W+/- -> f' + H0.
 
      ELSEIF(ISUB.EQ.48) THEN
C...f + H0 -> f + g.
 
      ELSEIF(ISUB.EQ.49) THEN
C...f + H0 -> f + gamma.
 
      ELSEIF(ISUB.EQ.50) THEN
C...f + H0 -> f + Z0.
      ENDIF
 
      ELSEIF(ISUB.LE.60) THEN
      IF(ISUB.EQ.51) THEN
C...f + H0 -> f' + W+/-.
 
      ELSEIF(ISUB.EQ.52) THEN
C...f + H0 -> f + H0.
 
      ELSEIF(ISUB.EQ.53) THEN
C...g + g -> f + f~; th arbitrary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(KFLF,KCS)
        MINT(22)=-MINT(21)
        KCC=MINT(2)+10
 
      ELSEIF(ISUB.EQ.54) THEN
C...g + gamma -> f + f~; th arbitrary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(KFLF,KCS)
        MINT(22)=-MINT(21)
        KCC=27
        IF(MINT(16).EQ.21) KCC=28
 
      ELSEIF(ISUB.EQ.55) THEN
C...g + Z0 -> f + f~.
 
      ELSEIF(ISUB.EQ.56) THEN
C...g + W+/- -> f + f~'.
 
      ELSEIF(ISUB.EQ.57) THEN
C...g + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.58) THEN
C...gamma + gamma -> f + f~; th arbitrary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(KFLF,KCS)
        MINT(22)=-MINT(21)
        KCC=21
 
      ELSEIF(ISUB.EQ.59) THEN
C...gamma + Z0 -> f + f~.
 
      ELSEIF(ISUB.EQ.60) THEN
C...gamma + W+/- -> f + f~'.
      ENDIF
 
      ELSEIF(ISUB.LE.70) THEN
      IF(ISUB.EQ.61) THEN
C...gamma + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.62) THEN
C...Z0 + Z0 -> f + f~.
 
      ELSEIF(ISUB.EQ.63) THEN
C...Z0 + W+/- -> f + f~'.
 
      ELSEIF(ISUB.EQ.64) THEN
C...Z0 + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.65) THEN
C...W+ + W- -> f + f~.
 
      ELSEIF(ISUB.EQ.66) THEN
C...W+/- + H0 -> f + f~'.
 
      ELSEIF(ISUB.EQ.67) THEN
C...H0 + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.68) THEN
C...g + g -> g + g; th arbitrary.
        KCC=MINT(2)+12
        KCS=(-1)**INT(1.5+RLU(0))
 
      ELSEIF(ISUB.EQ.69) THEN
C...gamma + gamma -> W+ + W-; th arbitrary.
        MINT(21)=24
        MINT(22)=-24
        KCC=21
 
      ELSEIF(ISUB.EQ.70) THEN
C...gamma + W+/- -> Z0 + W+/-; th=(p(W)-p(W))**2.
        IF(MINT(15).EQ.22) MINT(21)=23
        IF(MINT(16).EQ.22) MINT(22)=23
        KCC=21
      ENDIF
 
      ELSEIF(ISUB.LE.80) THEN
      IF(ISUB.EQ.71.OR.ISUB.EQ.72) THEN
C...Z0 + Z0 -> Z0 + Z0; Z0 + Z0 -> W+ + W-.
        XH=SH/SHP
        MINT(21)=MINT(15)
        MINT(22)=MINT(16)
        PMQ(1)=ULMASS(MINT(21))
        PMQ(2)=ULMASS(MINT(22))
  330   JT=INT(1.5+RLU(0))
        ZMIN=2.*PMQ(JT)/SHPR
        ZMAX=1.-PMQ(3-JT)/SHPR-(SH-PMQ(JT)**2)/(SHPR*(SHPR-PMQ(3-JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        Z(JT)=ZMIN+(ZMAX-ZMIN)*RLU(0)
        IF(-1.+(1.+XH)/(1.-Z(JT))-XH/(1.-Z(JT))**2.LT.
     &  (1.-XH)**2/(4.*XH)*RLU(0)) GOTO 330
        SQC1=1.-4.*PMQ(JT)**2/(Z(JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 330
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(23,1)**2-PMQ(JT)**2)/(Z(JT)*SHP)
        CTHE(JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(JT)=MIN(1.,MAX(-1.,CTHE(JT)))
        Z(3-JT)=1.-XH/(1.-Z(JT))
        SQC1=1.-4.*PMQ(3-JT)**2/(Z(3-JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 330
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(23,1)**2-PMQ(3-JT)**2)/(Z(3-JT)*SHP)
        CTHE(3-JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(3-JT)=MIN(1.,MAX(-1.,CTHE(3-JT)))
        PHIR=PARU(2)*RLU(0)
        CPHI=COS(PHIR)
        ANG=CTHE(1)*CTHE(2)-SQRT(1.-CTHE(1)**2)*SQRT(1.-CTHE(2)**2)*CPHI
        Z1=2.-Z(JT)
        Z2=ANG*SQRT(Z(JT)**2-4.*PMQ(JT)**2/SHP)
        Z3=1.-Z(JT)-XH+(PMQ(1)**2+PMQ(2)**2)/SHP
        Z(3-JT)=2./(Z1**2-Z2**2)*(Z1*Z3+Z2*SQRT(Z3**2-(Z1**2-Z2**2)*
     &  PMQ(3-JT)**2/SHP))
        ZMIN=2.*PMQ(3-JT)/SHPR
        ZMAX=1.-PMQ(JT)/SHPR-(SH-PMQ(3-JT)**2)/(SHPR*(SHPR-PMQ(JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(Z(3-JT).LT.ZMIN.OR.Z(3-JT).GT.ZMAX) GOTO 330
        KCC=22
 
      ELSEIF(ISUB.EQ.73) THEN
C...Z0 + W+/- -> Z0 + W+/-.
        JS=MINT(2)
        XH=SH/SHP
  340   JT=3-MINT(2)
        I=MINT(14+JT)
        IA=IABS(I)
        IF(IA.LE.10) THEN
          RVCKM=VINT(180+I)*RLU(0)
          DO 350 J=1,MSTP(1)
          IB=2*J-1+MOD(IA,2)
          IPM=(5-ISIGN(1,I))/2
          IDC=J+MDCY(IA,2)+2
          IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 350
          MINT(20+JT)=ISIGN(IB,I)
          RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
          IF(RVCKM.LE.0.) GOTO 360
  350     CONTINUE
        ELSE
          IB=2*((IA+1)/2)-1+MOD(IA,2)
          MINT(20+JT)=ISIGN(IB,I)
        ENDIF
  360   PMQ(JT)=ULMASS(MINT(20+JT))
        MINT(23-JT)=MINT(17-JT)
        PMQ(3-JT)=ULMASS(MINT(23-JT))
        JT=INT(1.5+RLU(0))
        ZMIN=2.*PMQ(JT)/SHPR
        ZMAX=1.-PMQ(3-JT)/SHPR-(SH-PMQ(JT)**2)/(SHPR*(SHPR-PMQ(3-JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(ZMIN.GE.ZMAX) GOTO 340
        Z(JT)=ZMIN+(ZMAX-ZMIN)*RLU(0)
        IF(-1.+(1.+XH)/(1.-Z(JT))-XH/(1.-Z(JT))**2.LT.
     &  (1.-XH)**2/(4.*XH)*RLU(0)) GOTO 340
        SQC1=1.-4.*PMQ(JT)**2/(Z(JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 340
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(23,1)**2-PMQ(JT)**2)/(Z(JT)*SHP)
        CTHE(JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(JT)=MIN(1.,MAX(-1.,CTHE(JT)))
        Z(3-JT)=1.-XH/(1.-Z(JT))
        SQC1=1.-4.*PMQ(3-JT)**2/(Z(3-JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 340
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(23,1)**2-PMQ(3-JT)**2)/(Z(3-JT)*SHP)
        CTHE(3-JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(3-JT)=MIN(1.,MAX(-1.,CTHE(3-JT)))
        PHIR=PARU(2)*RLU(0)
        CPHI=COS(PHIR)
        ANG=CTHE(1)*CTHE(2)-SQRT(1.-CTHE(1)**2)*SQRT(1.-CTHE(2)**2)*CPHI
        Z1=2.-Z(JT)
        Z2=ANG*SQRT(Z(JT)**2-4.*PMQ(JT)**2/SHP)
        Z3=1.-Z(JT)-XH+(PMQ(1)**2+PMQ(2)**2)/SHP
        Z(3-JT)=2./(Z1**2-Z2**2)*(Z1*Z3+Z2*SQRT(Z3**2-(Z1**2-Z2**2)*
     &  PMQ(3-JT)**2/SHP))
        ZMIN=2.*PMQ(3-JT)/SHPR
        ZMAX=1.-PMQ(JT)/SHPR-(SH-PMQ(3-JT)**2)/(SHPR*(SHPR-PMQ(JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(Z(3-JT).LT.ZMIN.OR.Z(3-JT).GT.ZMAX) GOTO 340
        KCC=22
 
      ELSEIF(ISUB.EQ.74) THEN
C...Z0 + H0 -> Z0 + H0.
 
      ELSEIF(ISUB.EQ.75) THEN
C...W+ + W- -> gamma + gamma.
 
      ELSEIF(ISUB.EQ.76.OR.ISUB.EQ.77) THEN
C...W+ + W- -> Z0 + Z0; W+ + W- -> W+ + W-.
        XH=SH/SHP
  370   DO 400 JT=1,2
        I=MINT(14+JT)
        IA=IABS(I)
        IF(IA.LE.10) THEN
          RVCKM=VINT(180+I)*RLU(0)
          DO 380 J=1,MSTP(1)
          IB=2*J-1+MOD(IA,2)
          IPM=(5-ISIGN(1,I))/2
          IDC=J+MDCY(IA,2)+2
          IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 380
          MINT(20+JT)=ISIGN(IB,I)
          RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
          IF(RVCKM.LE.0.) GOTO 390
  380     CONTINUE
        ELSE
          IB=2*((IA+1)/2)-1+MOD(IA,2)
          MINT(20+JT)=ISIGN(IB,I)
        ENDIF
  390   PMQ(JT)=ULMASS(MINT(20+JT))
  400   CONTINUE
        JT=INT(1.5+RLU(0))
        ZMIN=2.*PMQ(JT)/SHPR
        ZMAX=1.-PMQ(3-JT)/SHPR-(SH-PMQ(JT)**2)/(SHPR*(SHPR-PMQ(3-JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(ZMIN.GE.ZMAX) GOTO 370
        Z(JT)=ZMIN+(ZMAX-ZMIN)*RLU(0)
        IF(-1.+(1.+XH)/(1.-Z(JT))-XH/(1.-Z(JT))**2.LT.
     &  (1.-XH)**2/(4.*XH)*RLU(0)) GOTO 370
        SQC1=1.-4.*PMQ(JT)**2/(Z(JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 370
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(24,1)**2-PMQ(JT)**2)/(Z(JT)*SHP)
        CTHE(JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(JT)=MIN(1.,MAX(-1.,CTHE(JT)))
        Z(3-JT)=1.-XH/(1.-Z(JT))
        SQC1=1.-4.*PMQ(3-JT)**2/(Z(3-JT)**2*SHP)
        IF(SQC1.LT.1.E-8) GOTO 370
        C1=SQRT(SQC1)
        C2=1.+2.*(PMAS(24,1)**2-PMQ(3-JT)**2)/(Z(3-JT)*SHP)
        CTHE(3-JT)=(C2-(C2**2-C1**2)/(C2+(2.*RLU(0)-1.)*C1))/C1
        CTHE(3-JT)=MIN(1.,MAX(-1.,CTHE(3-JT)))
        PHIR=PARU(2)*RLU(0)
        CPHI=COS(PHIR)
        ANG=CTHE(1)*CTHE(2)-SQRT(1.-CTHE(1)**2)*SQRT(1.-CTHE(2)**2)*CPHI
        Z1=2.-Z(JT)
        Z2=ANG*SQRT(Z(JT)**2-4.*PMQ(JT)**2/SHP)
        Z3=1.-Z(JT)-XH+(PMQ(1)**2+PMQ(2)**2)/SHP
        Z(3-JT)=2./(Z1**2-Z2**2)*(Z1*Z3+Z2*SQRT(Z3**2-(Z1**2-Z2**2)*
     &  PMQ(3-JT)**2/SHP))
        ZMIN=2.*PMQ(3-JT)/SHPR
        ZMAX=1.-PMQ(JT)/SHPR-(SH-PMQ(3-JT)**2)/(SHPR*(SHPR-PMQ(JT)))
        ZMAX=MIN(1.-XH,ZMAX)
        IF(Z(3-JT).LT.ZMIN.OR.Z(3-JT).GT.ZMAX) GOTO 370
        KCC=22
 
      ELSEIF(ISUB.EQ.78) THEN
C...W+/- + H0 -> W+/- + H0.
 
      ELSEIF(ISUB.EQ.79) THEN
C...H0 + H0 -> H0 + H0.
 
      ELSEIF(ISUB.EQ.80) THEN
C...q + gamma -> q' + pi+/-; th=(p(q)-p(q'))**2.
        IF(MINT(15).EQ.22) JS=2
        I=MINT(14+JS)
        IA=IABS(I)
        MINT(23-JS)=ISIGN(211,KCHG(IA,1)*I)
        IB=3-IA
        MINT(20+JS)=ISIGN(IB,I)
        KCC=22
      ENDIF
 
      ELSEIF(ISUB.LE.90) THEN
      IF(ISUB.EQ.81) THEN
C...q + q~ -> Q + Q~; th = (p(q)-p(Q))**2.
        MINT(21)=ISIGN(MINT(55),MINT(15))
        MINT(22)=-MINT(21)
        KCC=4
 
      ELSEIF(ISUB.EQ.82) THEN
C...g + g -> Q + Q~; th arbitrary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(MINT(55),KCS)
        MINT(22)=-MINT(21)
        KCC=MINT(2)+10
 
      ELSEIF(ISUB.EQ.83) THEN
C...f + q -> f' + Q; th = (p(f) - p(f'))**2.
        KFOLD=MINT(16)
        IF(MINT(2).EQ.2) KFOLD=MINT(15)
        KFAOLD=IABS(KFOLD)
        IF(KFAOLD.GT.10) THEN
          KFANEW=KFAOLD+2*MOD(KFAOLD,2)-1
        ELSE
          RCKM=VINT(180+KFOLD)*RLU(0)
          IPM=(5-ISIGN(1,KFOLD))/2
          KFANEW=-MOD(KFAOLD+1,2)
  410     KFANEW=KFANEW+2
          IDC=MDCY(KFAOLD,2)+(KFANEW+1)/2+2
          IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.IPM) THEN
            IF(MOD(KFAOLD,2).EQ.0) RCKM=RCKM-VCKM(KFAOLD/2,(KFANEW+1)/2)
            IF(MOD(KFAOLD,2).EQ.1) RCKM=RCKM-VCKM(KFANEW/2,(KFAOLD+1)/2)
          ENDIF
          IF(KFANEW.LE.6.AND.RCKM.GT.0.) GOTO 410
        ENDIF
        IF(MINT(2).EQ.1) THEN
          MINT(21)=ISIGN(MINT(55),MINT(15))
          MINT(22)=ISIGN(KFANEW,MINT(16))
        ELSE
          MINT(21)=ISIGN(KFANEW,MINT(15))
          MINT(22)=ISIGN(MINT(55),MINT(16))
          JS=2
        ENDIF
        KCC=22
 
      ELSEIF(ISUB.EQ.84) THEN
C...g + gamma -> Q + Q~; th arbitary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(MINT(55),KCS)
        MINT(22)=-MINT(21)
        KCC=27
        IF(MINT(16).EQ.21) KCC=28
 
      ELSEIF(ISUB.EQ.85) THEN
C...gamma + gamma -> F + F~; th arbitary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(MINT(56),KCS)
        MINT(22)=-MINT(21)
        KCC=21
 
      ELSEIF(ISUB.GE.86.AND.ISUB.LE.89) THEN
C...g + g -> (J/Psi, chi_0c, chi_1c or chi_2c) + g
        MINT(21)=KFPR(ISUB,1)
        MINT(22)=KFPR(ISUB,2)
        KCC=24
        KCS=(-1)**INT(1.5+RLU(0))
      ENDIF
 
      ELSEIF(ISUB.LE.100) THEN
      IF(ISUB.EQ.95) THEN
C...Low-pT ( = energyless g + g -> g + g).
        KCC=MINT(2)+12
        KCS=(-1)**INT(1.5+RLU(0))
 
      ELSEIF(ISUB.EQ.96) THEN
C...Multiple interactions (should be reassigned to QCD process).
      ENDIF
 
      ELSEIF(ISUB.LE.110) THEN
      IF(ISUB.EQ.101) THEN
C...g + g -> gamma*/Z0.
        KCC=21
        KFRES=22
 
      ELSEIF(ISUB.EQ.102) THEN
C...g + g -> H0 (or H'0, or A0).
        KCC=21
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.103) THEN
C...gamma + gamma -> H0 (or H'0, or A0).
        KCC=21
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.110) THEN
C...f + f~ -> gamma + H0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=22
        MINT(23-JS)=KFHIGG
      ENDIF
 
      ELSEIF(ISUB.LE.120) THEN
      IF(ISUB.EQ.111) THEN
C...f + f~ -> g + H0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(20+JS)=21
        MINT(23-JS)=25
        KCC=17+JS
 
      ELSEIF(ISUB.EQ.112) THEN
C...f + g -> f + H0; th = (p(f) - p(f))**2.
        IF(MINT(15).EQ.21) JS=2
        MINT(23-JS)=25
        KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.113) THEN
C...g + g -> g + H0; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(23-JS)=25
        KCC=22+JS
        KCS=(-1)**INT(1.5+RLU(0))
 
      ELSEIF(ISUB.EQ.114) THEN
C...g + g -> gamma + gamma; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(21)=22
        MINT(22)=22
        KCC=21
 
      ELSEIF(ISUB.EQ.115) THEN
C...g + g -> g + gamma; th arbitrary.
        IF(RLU(0).GT.0.5) JS=2
        MINT(23-JS)=22
        KCC=22+JS
        KCS=(-1)**INT(1.5+RLU(0))
 
      ELSEIF(ISUB.EQ.116) THEN
C...g + g -> gamma + Z0.
 
      ELSEIF(ISUB.EQ.117) THEN
C...g + g -> Z0 + Z0.
 
      ELSEIF(ISUB.EQ.118) THEN
C...g + g -> W+ + W-.
      ENDIF
 
      ELSEIF(ISUB.LE.140) THEN
      IF(ISUB.EQ.121) THEN
C...g + g -> Q + Q~ + H0.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(KFPR(ISUBSV,2),KCS)
        MINT(22)=-MINT(21)
        KCC=11+INT(0.5+RLU(0))
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.122) THEN
C...q + q~ -> Q + Q~ + H0.
        MINT(21)=ISIGN(KFPR(ISUBSV,2),MINT(15))
        MINT(22)=-MINT(21)
        KCC=4
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.123) THEN
C...f + f' -> f + f' + H0 (or H'0, or A0) (Z0 + Z0 -> H0 as
C...inner process).
        KCC=22
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.124) THEN
C...f + f' -> f" + f"' + H0 (or H'0, or A) (W+ + W- -> H0 as
C...inner process).
        DO 430 JT=1,2
        I=MINT(14+JT)
        IA=IABS(I)
        IF(IA.LE.10) THEN
          RVCKM=VINT(180+I)*RLU(0)
          DO 420 J=1,MSTP(1)
          IB=2*J-1+MOD(IA,2)
          IPM=(5-ISIGN(1,I))/2
          IDC=J+MDCY(IA,2)+2
          IF(MDME(IDC,1).NE.1.AND.MDME(IDC,1).NE.IPM) GOTO 420
          MINT(20+JT)=ISIGN(IB,I)
          RVCKM=RVCKM-VCKM((IA+1)/2,(IB+1)/2)
          IF(RVCKM.LE.0.) GOTO 430
  420     CONTINUE
        ELSE
          IB=2*((IA+1)/2)-1+MOD(IA,2)
          MINT(20+JT)=ISIGN(IB,I)
        ENDIF
  430   CONTINUE
        KCC=22
        KFRES=KFHIGG
 
      ELSEIF(ISUB.EQ.131) THEN
C...g + g -> Z0 + q + q~.
        MINT(21)=KFPR(131,1)
        MINT(22)=KFPR(131,2)
        MINT(23)=-MINT(22)
        KCC=MINT(2)+10
        KCS=1
      ENDIF
 
      ELSEIF(ISUB.LE.160) THEN
      IF(ISUB.EQ.141) THEN
C...f + f~ -> gamma*/Z0/Z'0.
        KFRES=32
 
      ELSEIF(ISUB.EQ.142) THEN
C...f + f~' -> W'+/- .
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        KFRES=ISIGN(34,KCH1+KCH2)
 
      ELSEIF(ISUB.EQ.143) THEN
C...f + f~' -> H+/-.
        KCH1=KCHG(IABS(MINT(15)),1)*ISIGN(1,MINT(15))
        KCH2=KCHG(IABS(MINT(16)),1)*ISIGN(1,MINT(16))
        KFRES=ISIGN(37,KCH1+KCH2)
 
      ELSEIF(ISUB.EQ.144) THEN
C...f + f~' -> R.
        KFRES=ISIGN(40,MINT(15)+MINT(16))
 
      ELSEIF(ISUB.EQ.145) THEN
C...q + l -> LQ (leptoquark).
        IF(IABS(MINT(16)).LE.8) JS=2
        KFRES=ISIGN(39,MINT(14+JS))
        KCC=28+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.147.OR.ISUB.EQ.148) THEN
C...q + g -> q* (excited quark).
        IF(MINT(15).EQ.21) JS=2
        KFRES=MINT(14+JS)+ISIGN(6,MINT(14+JS))
        KCC=30+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.149) THEN
C...g + g -> eta_techni.
        KFRES=38
        KCC=23
        KCS=(-1)**INT(1.5+RLU(0))
      ENDIF
 
      ELSE
      IF(ISUB.EQ.161) THEN
C...f + g -> f' + H+/-; th = (p(f)-p(f'))**2.
        IF(MINT(15).EQ.21) JS=2
        I=MINT(14+JS)
        IA=IABS(I)
        MINT(23-JS)=ISIGN(37,KCHG(IA,1)*I)
        IB=IA+MOD(IA,2)-MOD(IA+1,2)
        MINT(20+JS)=ISIGN(IB,I)
        KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.162) THEN
C...q + g -> LQ + l~; LQ=leptoquark; th=(p(q)-p(LQ))^2.
        IF(MINT(15).EQ.21) JS=2
        MINT(20+JS)=ISIGN(39,MINT(14+JS))
        KFLQL=KFDP(MDCY(39,2),2)
        MINT(23-JS)=-ISIGN(KFLQL,MINT(14+JS))
        KCC=15+JS
        KCS=ISIGN(1,MINT(14+JS))
 
      ELSEIF(ISUB.EQ.163) THEN
C...g + g -> LQ + LQ~; LQ=leptoquark; th arbitrary.
        KCS=(-1)**INT(1.5+RLU(0))
        MINT(21)=ISIGN(39,KCS)
        MINT(22)=-MINT(21)
        KCC=MINT(2)+10
 
      ELSEIF(ISUB.EQ.164) THEN
C...q + q~ -> LQ + LQ~; LQ=leptoquark; th=(p(q)-p(LQ))**2.
        MINT(21)=ISIGN(39,MINT(15))
        MINT(22)=-MINT(21)
        KCC=4
 
      ELSEIF(ISUB.EQ.165) THEN
C...q + q~ -> l- + l+; th=(p(q)-p(l-))**2.
        MINT(21)=ISIGN(KFPR(ISUB,1),MINT(15))
        MINT(22)=-MINT(21)
 
      ELSEIF(ISUB.EQ.166) THEN
C...q + q~' -> l + nu; th=(p(u)-p(nu))**2 or (p(u~)-p(nu~))**2.
        IF(MOD(MINT(15),2).EQ.0) THEN
          MINT(21)=ISIGN(KFPR(ISUB,1)+1,MINT(15))
          MINT(22)=ISIGN(KFPR(ISUB,1),MINT(16))
        ELSE
          MINT(21)=ISIGN(KFPR(ISUB,1),MINT(15))
          MINT(22)=ISIGN(KFPR(ISUB,1)+1,MINT(16))
        ENDIF
 
      ELSEIF(ISUB.EQ.167.OR.ISUB.EQ.168) THEN
C...q + q' -> q" + q* (excited quark).
        KFQEXC=ISUB-166
        KFQSTR=ISUB-160
        JS=MINT(2)
        MINT(20+JS)=ISIGN(KFQSTR,MINT(14+JS))
        IF(IABS(MINT(15)).NE.KFQEXC.AND.IABS(MINT(16)).NE.KFQEXC)
     &  MINT(23-JS)=ISIGN(KFQEXC,MINT(17-JS))
        KCC=22
      ENDIF
      ENDIF
 
      IF(ISET(ISUB).EQ.11) THEN
C...Store documentation for user-defined processes.
        BEZUP=(PUP(1,4)-PUP(2,4))/(PUP(1,4)+PUP(2,4))
        KUPPO(1)=MINT(83)+5
        KUPPO(2)=MINT(83)+6
        I=MINT(83)+6
        DO 450 IUP=3,NUP
        KUPPO(IUP)=0
        IF(MSTP(128).GE.2.AND.KUP(IUP,3).NE.0) THEN
          IDOC=IDOC-1
          MINT(4)=MINT(4)-1
          GOTO 450
        ENDIF
        I=I+1
        KUPPO(IUP)=I
        K(I,1)=21
        K(I,2)=KUP(IUP,2)
        K(I,3)=0
        IF(KUP(IUP,3).NE.0) K(I,3)=KUPPO(KUP(IUP,3))
        K(I,4)=0
        K(I,5)=0
        DO 440 J=1,5
        P(I,J)=PUP(IUP,J)
  440   CONTINUE
  450   CONTINUE
        CALL LUDBRB(MINT(83)+7,MINT(83)+4+NUP,0.,VINT(24),0D0,0D0,
     &  -DBLE(BEZUP))
 
C...Store final state partons for user-defined processes.
        N=IPU2
        DO 470 IUP=3,NUP
        N=N+1
        K(N,1)=1
        IF(KUP(IUP,1).NE.1) K(N,1)=11
        K(N,2)=KUP(IUP,2)
        IF(MSTP(128).LE.0.OR.KUP(IUP,3).EQ.0) THEN
          K(N,3)=KUPPO(IUP)
        ELSE
          K(N,3)=MINT(84)+KUP(IUP,3)
        ENDIF
        K(N,4)=0
        K(N,5)=0
        DO 460 J=1,5
        P(N,J)=PUP(IUP,J)
  460   CONTINUE
  470   CONTINUE
        CALL LUDBRB(IPU3,N,0.,VINT(24),0D0,0D0,-DBLE(BEZUP))
 
C...Arrange colour flow for user-defined processes.
        N=MINT(84)
        DO 480 IUP=1,NUP
        N=N+1
        IF(KCHG(LUCOMP(K(N,2)),2).EQ.0) GOTO 480
        IF(K(N,1).EQ.1) K(N,1)=3
        IF(K(N,1).EQ.11) K(N,1)=14
        IF(KUP(IUP,4).NE.0) K(N,4)=K(N,4)+MSTU(5)*(KUP(IUP,4)+MINT(84))
        IF(KUP(IUP,5).NE.0) K(N,5)=K(N,5)+MSTU(5)*(KUP(IUP,5)+MINT(84))
        IF(KUP(IUP,6).NE.0) K(N,4)=K(N,4)+KUP(IUP,6)+MINT(84)
        IF(KUP(IUP,7).NE.0) K(N,5)=K(N,5)+KUP(IUP,7)+MINT(84)
  480   CONTINUE
 
      ELSEIF(IDOC.EQ.7) THEN
C...Resonance not decaying; store kinematics.
        I=MINT(83)+7
        K(IPU3,1)=1
        K(IPU3,2)=KFRES
        K(IPU3,3)=I
        P(IPU3,4)=SHUSER
        P(IPU3,5)=SHUSER
        K(I,1)=21
        K(I,2)=KFRES
        P(I,4)=SHUSER
        P(I,5)=SHUSER
        N=IPU3
        MINT(21)=KFRES
        MINT(22)=0
 
C...Special cases: colour flow in g + g -> eta_techni, q + l -> LQ
C...and q + g -> q*.
        IF(KFRES.EQ.38.OR.IABS(KFRES).EQ.39.OR.(MSTP(6).EQ.1.AND.
     &  (IABS(KFRES).EQ.7.OR.IABS(KFRES).EQ.8))) THEN
          K(IPU3,1)=3
          DO 490 J=1,2
          JC=J
          IF(KCS.EQ.-1) JC=3-J
          IF(ICOL(KCC,1,JC).NE.0.AND.K(IPU1,1).EQ.14) K(IPU1,J+3)=
     &    MINT(84)+ICOL(KCC,1,JC)
          IF(ICOL(KCC,2,JC).NE.0.AND.K(IPU2,1).EQ.14) K(IPU2,J+3)=
     &    MINT(84)+ICOL(KCC,2,JC)
          IF(ICOL(KCC,3,JC).NE.0.AND.K(IPU3,1).EQ.3) K(IPU3,J+3)=
     &    MSTU(5)*(MINT(84)+ICOL(KCC,3,JC))
  490     CONTINUE
        ELSE
          K(IPU1,4)=IPU2
          K(IPU1,5)=IPU2
          K(IPU2,4)=IPU1
          K(IPU2,5)=IPU1
        ENDIF
 
      ELSEIF(IDOC.EQ.8) THEN
C...2 -> 2 processes: store outgoing partons in their CM-frame.
        DO 500 JT=1,2
        I=MINT(84)+2+JT
        K(I,1)=1
        IF(IABS(MINT(20+JT)).LE.100) THEN
          IF(KCHG(IABS(MINT(20+JT)),2).NE.0) K(I,1)=3
        ENDIF
        K(I,2)=MINT(20+JT)
        K(I,3)=MINT(83)+IDOC+JT-2
        KFAA=IABS(K(I,2))
        IF(KFAA.GE.23.OR.(KFAA.EQ.6.AND.KFPR(ISUBSV,1).NE.0.AND.
     &  MSTP(48).GE.1).OR.((KFAA.EQ.7.OR.KFAA.EQ.8.OR.KFAA.EQ.17.OR.
     &  KFAA.EQ.18).AND.KFPR(ISUBSV,1).NE.0.AND.MSTP(49).GE.1)) THEN
          P(I,5)=SQRT(VINT(63+MOD(JS+JT,2)))
        ELSEIF((KFAA.EQ.7.OR.KFAA.EQ.8).AND.MSTP(6).EQ.1.AND.
     &  KFPR(ISUBSV,2).NE.0) THEN
          P(I,5)=SQRT(VINT(64))
        ELSE
          P(I,5)=ULMASS(K(I,2))
        ENDIF
  500   CONTINUE
        IF(P(IPU3,5)+P(IPU4,5).GE.SHR) THEN
          KFA1=IABS(MINT(21))
          KFA2=IABS(MINT(22))
          IF((KFA1.GT.3.AND.KFA1.NE.21).OR.(KFA2.GT.3.AND.KFA2.NE.21))
     &    THEN
            MINT(51)=1
            RETURN
          ENDIF
          P(IPU3,5)=0.
          P(IPU4,5)=0.
        ENDIF
        P(IPU3,4)=0.5*(SHR+(P(IPU3,5)**2-P(IPU4,5)**2)/SHR)
        P(IPU3,3)=SQRT(MAX(0.,P(IPU3,4)**2-P(IPU3,5)**2))
        P(IPU4,4)=SHR-P(IPU3,4)
        P(IPU4,3)=-P(IPU3,3)
        N=IPU4
        MINT(7)=MINT(83)+7
        MINT(8)=MINT(83)+8
 
C...Rotate outgoing partons using cos(theta)=(th-uh)/lam(sh,sqm3,sqm4).
        CALL LUDBRB(IPU3,IPU4,ACOS(VINT(23)),VINT(24),0D0,0D0,0D0)
 
      ELSEIF(IDOC.EQ.9.AND.ISET(ISUB).EQ.5) THEN
C...2 -> 3 processes (alt 1): store outgoing partons in their CM frame.
        DO 510 JT=1,2
        I=MINT(84)+2+JT
        K(I,1)=1
        IF(IABS(MINT(20+JT)).LE.100) THEN
          IF(KCHG(IABS(MINT(20+JT)),2).NE.0) K(I,1)=3
        ENDIF
        K(I,2)=MINT(20+JT)
        K(I,3)=MINT(83)+IDOC+JT-3
        IF(IABS(K(I,2)).LE.22) THEN
          P(I,5)=ULMASS(K(I,2))
        ELSE
          P(I,5)=SQRT(VINT(63+MOD(JS+JT,2)))
        ENDIF
        PT=SQRT(MAX(0.,VINT(197+5*JT)-P(I,5)**2+VINT(196+5*JT)**2))
        P(I,1)=PT*COS(VINT(198+5*JT))
        P(I,2)=PT*SIN(VINT(198+5*JT))
  510   CONTINUE
        K(IPU5,1)=1
        K(IPU5,2)=KFRES
        K(IPU5,3)=MINT(83)+IDOC
        P(IPU5,5)=SHR
        P(IPU5,1)=-P(IPU3,1)-P(IPU4,1)
        P(IPU5,2)=-P(IPU3,2)-P(IPU4,2)
        PMS1=P(IPU3,5)**2+P(IPU3,1)**2+P(IPU3,2)**2
        PMS2=P(IPU4,5)**2+P(IPU4,1)**2+P(IPU4,2)**2
        PMS3=P(IPU5,5)**2+P(IPU5,1)**2+P(IPU5,2)**2
        PMT3=SQRT(PMS3)
        P(IPU5,3)=PMT3*SINH(VINT(211))
        P(IPU5,4)=PMT3*COSH(VINT(211))
        PMS12=(SHPR-P(IPU5,4))**2-P(IPU5,3)**2
        SQL12=(PMS12-PMS1-PMS2)**2-4.*PMS1*PMS2
        IF(SQL12.LE.0.) THEN
          MINT(51)=1
          RETURN
        ENDIF
        P(IPU3,3)=(-P(IPU5,3)*(PMS12+PMS1-PMS2)+
     &  VINT(213)*(SHPR-P(IPU5,4))*SQRT(SQL12))/(2.*PMS12)
        P(IPU4,3)=-P(IPU3,3)-P(IPU5,3)
        P(IPU3,4)=SQRT(PMS1+P(IPU3,3)**2)
        P(IPU4,4)=SQRT(PMS2+P(IPU4,3)**2)
        MINT(23)=KFRES
        N=IPU5
        MINT(7)=MINT(83)+7
        MINT(8)=MINT(83)+8
 
      ELSEIF(IDOC.EQ.9) THEN
C...2 -> 3 processes: store outgoing partons in their CM frame.
        DO 520 JT=1,3
        I=MINT(84)+2+JT
        K(I,1)=1
        IF(IABS(MINT(20+JT)).LE.10.OR.MINT(20+JT).EQ.21) K(I,1)=3
        K(I,2)=MINT(20+JT)
        K(I,3)=MINT(83)+IDOC+JT-3
        IF(JT.EQ.1) THEN
          P(I,5)=SQRT(VINT(63))
        ELSE
          P(I,5)=PMAS(KFPR(ISUB,2),1)
        ENDIF
  520   CONTINUE
        P(IPU3,4)=0.5*(SHR+(VINT(63)-VINT(64))/SHR)
        P(IPU3,3)=SQRT(MAX(0.,P(IPU3,4)**2-P(IPU3,5)**2))
        P(IPU4,4)=0.5*SQRT(VINT(64))
        P(IPU4,3)=SQRT(MAX(0.,P(IPU4,4)**2-P(IPU4,5)**2))
        P(IPU5,4)=P(IPU4,4)
        P(IPU5,3)=-P(IPU4,3)
        N=IPU5
        MINT(7)=MINT(83)+7
        MINT(8)=MINT(83)+9
 
C...Rotate and boost outgoing partons.
        CALL LUDBRB(IPU4,IPU5,ACOS(VINT(83)),VINT(84),0D0,0D0,0D0)
        CALL LUDBRB(IPU4,IPU5,0.,0.,0D0,0D0,
     &  -DBLE(P(IPU3,3)/(SHR-P(IPU3,4))))
        CALL LUDBRB(IPU3,IPU5,ACOS(VINT(23)),VINT(24),0D0,0D0,0D0)
 
      ELSEIF(IDOC.EQ.11) THEN
C...Z0 + Z0 -> H0, W+ + W- -> H0: store Higgs and outgoing partons.
        PHI(1)=PARU(2)*RLU(0)
        PHI(2)=PHI(1)-PHIR
        DO 530 JT=1,2
        I=MINT(84)+2+JT
        K(I,1)=1
        IF(IABS(MINT(20+JT)).LE.10.OR.MINT(20+JT).EQ.21) K(I,1)=3
        K(I,2)=MINT(20+JT)
        K(I,3)=MINT(83)+IDOC+JT-2
        P(I,5)=ULMASS(K(I,2))
        IF(0.5*SHPR*Z(JT).LE.P(I,5)) P(I,5)=0.
        PABS=SQRT(MAX(0.,(0.5*SHPR*Z(JT))**2-P(I,5)**2))
        PTABS=PABS*SQRT(MAX(0.,1.-CTHE(JT)**2))
        P(I,1)=PTABS*COS(PHI(JT))
        P(I,2)=PTABS*SIN(PHI(JT))
        P(I,3)=PABS*CTHE(JT)*(-1)**(JT+1)
        P(I,4)=0.5*SHPR*Z(JT)
        IZW=MINT(83)+6+JT
        K(IZW,1)=21
        K(IZW,2)=23
        IF(ISUB.EQ.8) K(IZW,2)=ISIGN(24,LUCHGE(MINT(14+JT)))
        K(IZW,3)=IZW-2
        P(IZW,1)=-P(I,1)
        P(IZW,2)=-P(I,2)
        P(IZW,3)=(0.5*SHPR-PABS*CTHE(JT))*(-1)**(JT+1)
        P(IZW,4)=0.5*SHPR*(1.-Z(JT))
        P(IZW,5)=-SQRT(MAX(0.,P(IZW,3)**2+PTABS**2-P(IZW,4)**2))
  530   CONTINUE
        I=MINT(83)+9
        K(IPU5,1)=1
        K(IPU5,2)=KFRES
        K(IPU5,3)=I
        P(IPU5,5)=SHR
        P(IPU5,1)=-P(IPU3,1)-P(IPU4,1)
        P(IPU5,2)=-P(IPU3,2)-P(IPU4,2)
        P(IPU5,3)=-P(IPU3,3)-P(IPU4,3)
        P(IPU5,4)=SHPR-P(IPU3,4)-P(IPU4,4)
        K(I,1)=21
        K(I,2)=KFRES
        DO 540 J=1,5
        P(I,J)=P(IPU5,J)
  540   CONTINUE
        N=IPU5
        MINT(23)=KFRES
 
      ELSEIF(IDOC.EQ.12) THEN
C...Z0 and W+/- scattering: store bosons and outgoing partons.
        PHI(1)=PARU(2)*RLU(0)
        PHI(2)=PHI(1)-PHIR
        JTRAN=INT(1.5+RLU(0))
        DO 550 JT=1,2
        I=MINT(84)+2+JT
        K(I,1)=1
        IF(IABS(MINT(20+JT)).LE.10.OR.MINT(20+JT).EQ.21) K(I,1)=3
        K(I,2)=MINT(20+JT)
        K(I,3)=MINT(83)+IDOC+JT-2
        P(I,5)=ULMASS(K(I,2))
        IF(0.5*SHPR*Z(JT).LE.P(I,5)) P(I,5)=0.
        PABS=SQRT(MAX(0.,(0.5*SHPR*Z(JT))**2-P(I,5)**2))
        PTABS=PABS*SQRT(MAX(0.,1.-CTHE(JT)**2))
        P(I,1)=PTABS*COS(PHI(JT))
        P(I,2)=PTABS*SIN(PHI(JT))
        P(I,3)=PABS*CTHE(JT)*(-1)**(JT+1)
        P(I,4)=0.5*SHPR*Z(JT)
        IZW=MINT(83)+6+JT
        K(IZW,1)=21
        IF(MINT(14+JT).EQ.MINT(20+JT)) THEN
          K(IZW,2)=23
        ELSE
          K(IZW,2)=ISIGN(24,LUCHGE(MINT(14+JT))-LUCHGE(MINT(20+JT)))
        ENDIF
        K(IZW,3)=IZW-2
        P(IZW,1)=-P(I,1)
        P(IZW,2)=-P(I,2)
        P(IZW,3)=(0.5*SHPR-PABS*CTHE(JT))*(-1)**(JT+1)
        P(IZW,4)=0.5*SHPR*(1.-Z(JT))
        P(IZW,5)=-SQRT(MAX(0.,P(IZW,3)**2+PTABS**2-P(IZW,4)**2))
        IPU=MINT(84)+4+JT
        K(IPU,1)=3
        K(IPU,2)=KFPR(ISUB,JT)
        IF(ISUB.EQ.72.AND.JT.EQ.JTRAN) K(IPU,2)=-K(IPU,2)
        IF(ISUB.EQ.73.OR.ISUB.EQ.77) K(IPU,2)=K(IZW,2)
        K(IPU,3)=MINT(83)+8+JT
        IF(IABS(K(IPU,2)).LE.10.OR.K(IPU,2).EQ.21) THEN
          P(IPU,5)=ULMASS(K(IPU,2))
        ELSE
          P(IPU,5)=SQRT(VINT(63+MOD(JS+JT,2)))
        ENDIF
        MINT(22+JT)=K(IPU,2)
  550   CONTINUE
C...Find rotation and boost for hard scattering subsystem.
        I1=MINT(83)+7
        I2=MINT(83)+8
        BEXCM=(P(I1,1)+P(I2,1))/(P(I1,4)+P(I2,4))
        BEYCM=(P(I1,2)+P(I2,2))/(P(I1,4)+P(I2,4))
        BEZCM=(P(I1,3)+P(I2,3))/(P(I1,4)+P(I2,4))
        GAMCM=(P(I1,4)+P(I2,4))/SHR
        BEPCM=BEXCM*P(I1,1)+BEYCM*P(I1,2)+BEZCM*P(I1,3)
        PX=P(I1,1)+GAMCM*(GAMCM/(1.+GAMCM)*BEPCM-P(I1,4))*BEXCM
        PY=P(I1,2)+GAMCM*(GAMCM/(1.+GAMCM)*BEPCM-P(I1,4))*BEYCM
        PZ=P(I1,3)+GAMCM*(GAMCM/(1.+GAMCM)*BEPCM-P(I1,4))*BEZCM
        THECM=ULANGL(PZ,SQRT(PX**2+PY**2))
        PHICM=ULANGL(PX,PY)
C...Store hard scattering subsystem. Rotate and boost it.
        SQLAM=(SH-P(IPU5,5)**2-P(IPU6,5)**2)**2-4.*P(IPU5,5)**2*
     &  P(IPU6,5)**2
        PABS=SQRT(MAX(0.,SQLAM/(4.*SH)))
        CTHWZ=VINT(23)
        STHWZ=SQRT(MAX(0.,1.-CTHWZ**2))
        PHIWZ=VINT(24)-PHICM
        P(IPU5,1)=PABS*STHWZ*COS(PHIWZ)
        P(IPU5,2)=PABS*STHWZ*SIN(PHIWZ)
        P(IPU5,3)=PABS*CTHWZ
        P(IPU5,4)=SQRT(PABS**2+P(IPU5,5)**2)
        P(IPU6,1)=-P(IPU5,1)
        P(IPU6,2)=-P(IPU5,2)
        P(IPU6,3)=-P(IPU5,3)
        P(IPU6,4)=SQRT(PABS**2+P(IPU6,5)**2)
        CALL LUDBRB(IPU5,IPU6,THECM,PHICM,DBLE(BEXCM),DBLE(BEYCM),
     &  DBLE(BEZCM))
        DO 570 JT=1,2
        I1=MINT(83)+8+JT
        I2=MINT(84)+4+JT
        K(I1,1)=21
        K(I1,2)=K(I2,2)
        DO 560 J=1,5
        P(I1,J)=P(I2,J)
  560   CONTINUE
  570   CONTINUE
        N=IPU6
        MINT(7)=MINT(83)+9
        MINT(8)=MINT(83)+10
      ENDIF
 
      IF(ISET(ISUB).EQ.11) THEN
      ELSEIF(IDOC.GE.8.AND.ISET(ISUB).NE.6) THEN
C...Store colour connection indices.
        DO 580 J=1,2
        JC=J
        IF(KCS.EQ.-1) JC=3-J
        IF(ICOL(KCC,1,JC).NE.0.AND.K(IPU1,1).EQ.14) K(IPU1,J+3)=
     &  K(IPU1,J+3)+MINT(84)+ICOL(KCC,1,JC)
        IF(ICOL(KCC,2,JC).NE.0.AND.K(IPU2,1).EQ.14) K(IPU2,J+3)=
     &  K(IPU2,J+3)+MINT(84)+ICOL(KCC,2,JC)
        IF(ICOL(KCC,3,JC).NE.0.AND.K(IPU3,1).EQ.3) K(IPU3,J+3)=
     &  MSTU(5)*(MINT(84)+ICOL(KCC,3,JC))
        IF(ICOL(KCC,4,JC).NE.0.AND.K(IPU4,1).EQ.3) K(IPU4,J+3)=
     &  MSTU(5)*(MINT(84)+ICOL(KCC,4,JC))
  580   CONTINUE
 
C...Copy outgoing partons to documentation lines.
        IMAX=2
        IF(IDOC.EQ.9) IMAX=3
        DO 600 I=1,IMAX
        I1=MINT(83)+IDOC-IMAX+I
        I2=MINT(84)+2+I
        K(I1,1)=21
        K(I1,2)=K(I2,2)
        IF(IDOC.LE.9) K(I1,3)=0
        IF(IDOC.GE.11) K(I1,3)=MINT(83)+2+I
        DO 590 J=1,5
        P(I1,J)=P(I2,J)
  590   CONTINUE
  600   CONTINUE
 
      ELSEIF(IDOC.EQ.9) THEN
C...Store colour connection indices.
        DO 610 J=1,2
        JC=J
        IF(KCS.EQ.-1) JC=3-J
        IF(ICOL(KCC,1,JC).NE.0.AND.K(IPU1,1).EQ.14) K(IPU1,J+3)=
     &  K(IPU1,J+3)+MINT(84)+ICOL(KCC,1,JC)+
     &  MAX(0,MIN(1,ICOL(KCC,1,JC)-2))
        IF(ICOL(KCC,2,JC).NE.0.AND.K(IPU2,1).EQ.14) K(IPU2,J+3)=
     &  K(IPU2,J+3)+MINT(84)+ICOL(KCC,2,JC)+
     &  MAX(0,MIN(1,ICOL(KCC,2,JC)-2))
        IF(ICOL(KCC,3,JC).NE.0.AND.K(IPU4,1).EQ.3) K(IPU4,J+3)=
     &  MSTU(5)*(MINT(84)+ICOL(KCC,3,JC))
        IF(ICOL(KCC,4,JC).NE.0.AND.K(IPU5,1).EQ.3) K(IPU5,J+3)=
     &  MSTU(5)*(MINT(84)+ICOL(KCC,4,JC))
  610   CONTINUE
 
C...Copy outgoing partons to documentation lines.
        DO 630 I=1,3
        I1=MINT(83)+IDOC-3+I
        I2=MINT(84)+2+I
        K(I1,1)=21
        K(I1,2)=K(I2,2)
        K(I1,3)=0
        DO 620 J=1,5
        P(I1,J)=P(I2,J)
  620   CONTINUE
  630   CONTINUE
      ENDIF
 
C...Low-pT events: remove gluons used for string drawing purposes.
      IF(ISUB.EQ.95) THEN
        K(IPU3,1)=K(IPU3,1)+10
        K(IPU4,1)=K(IPU4,1)+10
        DO 640 J=41,66
        VINTSV(J)=VINT(J)
        VINT(J)=0.
  640   CONTINUE
        DO 660 I=MINT(83)+5,MINT(83)+8
        DO 650 J=1,5
        P(I,J)=0.
  650   CONTINUE
  660   CONTINUE
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSSPA(IPU1,IPU2)
 
C...Generates spacelike parton showers.
      IMPLICIT DOUBLE PRECISION(D)
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
      DIMENSION KFLS(4),IS(2),XS(2),ZS(2),Q2S(2),TEVCSV(2),TEVESV(2),
     &XFS(2,-25:25),XFA(-25:25),XFB(-25:25),XFN(-25:25),WTAPC(-25:25),
     &WTAPE(-25:25),WTSF(-25:25),THE2(2),ALAM(2),DQ2(3),DPC(3),DPD(4),
     &DPB(4),ROBO(5),MORE(2),KFBEAM(2),Q2MNCS(2),KCFI(2),NFIS(2),
     &THEFIS(2,2),ISFI(2)
      DATA IS/2*0/
 
C...Read out basic information; set global Q^2 scale.
      IPUS1=IPU1
      IPUS2=IPU2
      ISUB=MINT(1)
      Q2MX=VINT(56)
      IF(ISET(ISUB).EQ.2) Q2MX=PARP(67)*VINT(56)
 
C...Initialize QCD evolution and check phase space.
      Q2MNC=PARP(62)**2
      Q2MNCS(1)=Q2MNC
      IF(MSTP(66).EQ.1.AND.MINT(107).EQ.3)
     &Q2MNCS(1)=MAX(Q2MNC,VINT(283))
      Q2MNCS(2)=Q2MNC
      IF(MSTP(66).EQ.1.AND.MINT(108).EQ.3)
     &Q2MNCS(2)=MAX(Q2MNC,VINT(284))
      MCEV=0
      XEC0=2.*PARP(65)/VINT(1)
      ALAMS=PARU(112)
      PARU(112)=PARP(61)
      FQ2C=1.
      TCMX=0.
      IF(MINT(47).GE.2.AND.(MINT(47).NE.5.OR.MSTP(12).GE.1)) THEN
        MCEV=1
        IF(MSTP(64).EQ.1) FQ2C=PARP(63)
        IF(MSTP(64).EQ.2) FQ2C=PARP(64)
        TCMX=LOG(FQ2C*Q2MX/PARP(61)**2)
        IF(Q2MX.LT.MAX(Q2MNC,2.*PARP(61)**2).OR.TCMX.LT.0.2)
     &  MCEV=0
      ENDIF
 
C...Initialize QED evolution and check phase space.
      Q2MNE=PARP(68)**2
      MEEV=0
      XEE=1E-6
      SPME=PMAS(11,1)**2
      TEMX=0.
      FWTE=10.
      IF(MINT(45).EQ.3.OR.MINT(46).EQ.3) THEN
        MEEV=1
        TEMX=LOG(Q2MX/SPME)
        IF(Q2MX.LE.Q2MNE.OR.TEMX.LT.0.2) MEEV=0
      ENDIF
      IF(MCEV.EQ.0.AND.MEEV.EQ.0) RETURN
 
C...Initial values: flavours, momenta, virtualities.
      NS=N
  100 N=NS
      DO 120 JT=1,2
      MORE(JT)=1
      KFBEAM(JT)=MINT(10+JT)
      IF(MINT(18+JT).EQ.1)KFBEAM(JT)=22
      KFLS(JT)=MINT(14+JT)
      KFLS(JT+2)=KFLS(JT)
      XS(JT)=VINT(40+JT)
      IF(MINT(18+JT).EQ.1) XS(JT)=VINT(40+JT)/VINT(154+JT)
      ZS(JT)=1.
      Q2S(JT)=Q2MX
      TEVCSV(JT)=TCMX
      ALAM(JT)=PARP(61)
      THE2(JT)=100.
      TEVESV(JT)=TEMX
      DO 110 KFL=-25,25
      XFS(JT,KFL)=XSFX(JT,KFL)
  110 CONTINUE
  120 CONTINUE
      DSH=VINT(44)
      IF(ISET(ISUB).GE.3.AND.ISET(ISUB).LE.5) DSH=VINT(26)*VINT(2)
 
C...Find if interference with final state partons.
      MFIS=0
      IF(MSTP(67).GE.1.AND.MSTP(67).LE.3) MFIS=MSTP(67)
      IF(MFIS.NE.0) THEN
        DO 140 I=1,2
        KCFI(I)=0
        KCA=LUCOMP(IABS(KFLS(I)))
        IF(KCA.NE.0) KCFI(I)=KCHG(KCA,2)*ISIGN(1,KFLS(I))
        NFIS(I)=0
        IF(KCFI(I).NE.0) THEN
          IF(I.EQ.1) IPFS=IPUS1
          IF(I.EQ.2) IPFS=IPUS2
          DO 130 J=1,2
          ICSI=MOD(K(IPFS,3+J),MSTU(5))
          IF(ICSI.GT.0.AND.ICSI.NE.IPUS1.AND.ICSI.NE.IPUS2.AND.
     &    (KCFI(I).EQ.(-1)**(J+1).OR.KCFI(I).EQ.2)) THEN
            NFIS(I)=NFIS(I)+1
            THEFIS(I,NFIS(I))=ULANGL(P(ICSI,3),SQRT(P(ICSI,1)**2+
     &      P(ICSI,2)**2))
            IF(I.EQ.2) THEFIS(I,NFIS(I))=PARU(1)-THEFIS(I,NFIS(I))
          ENDIF
  130     CONTINUE
        ENDIF
  140   CONTINUE
        IF(NFIS(1)+NFIS(2).EQ.0) MFIS=0
      ENDIF
 
C...Pick up leg with highest virtuality.
  150 N=N+1
      JT=1
      IF(N.GT.NS+1.AND.Q2S(2).GT.Q2S(1)) JT=2
      IF(MORE(JT).EQ.0) JT=3-JT
      KFLB=KFLS(JT)
      XB=XS(JT)
      DO 160 KFL=-25,25
      XFB(KFL)=XFS(JT,KFL)
  160 CONTINUE
      DSHR=2D0*SQRT(DSH)
      DSHZ=DSH/DBLE(ZS(JT))
 
C...Check if allowed to branch.
      MCEV=0
      IF(IABS(KFLB).LE.10.OR.KFLB.EQ.21) THEN
        MCEV=1
        XEC=MAX(XEC0,XB*(1./(1.-PARP(66))-1.))
        IF(XB.GE.1.-2.*XEC) MCEV=0
      ENDIF
      MEEV=0
      IF(MINT(44+JT).EQ.3) THEN
        MEEV=1
        IF(XB.GE.1.-2.*XEE) MEEV=0
        IF((IABS(KFLB).LE.10.OR.KFLB.EQ.21).AND.XB.GE.1.-2.*XEC) MEEV=0
C***Currently kill QED shower for resolved photoproduction.
        IF(MINT(18+JT).EQ.1) MEEV=0
C***Currently kill shower for W inside electron.
        IF(IABS(KFLB).EQ.24) THEN
          MCEV=0
          MEEV=0
        ENDIF
      ENDIF
      IF(MCEV.EQ.0.AND.MEEV.EQ.0) THEN
        Q2B=0.
        GOTO 250
      ENDIF
 
C...Maximum Q2 with or without Q2 ordering. Effective Lambda and n_f.
      Q2B=Q2S(JT)
      TEVCB=TEVCSV(JT)
      TEVEB=TEVESV(JT)
      IF(MSTP(62).LE.1) THEN
        IF(ZS(JT).GT.0.99999) THEN
          Q2B=Q2S(JT)
        ELSE
          Q2B=0.5*(1./ZS(JT)+1.)*Q2S(JT)+0.5*(1./ZS(JT)-1.)*(Q2S(3-JT)-
     &    SNGL(DSH)+SQRT((SNGL(DSH)+Q2S(1)+Q2S(2))**2+8.*Q2S(1)*Q2S(2)*
     &    ZS(JT)/(1.-ZS(JT))))
        ENDIF
        IF(MCEV.EQ.1) TEVCB=LOG(FQ2C*Q2B/ALAM(JT)**2)
        IF(MEEV.EQ.1) TEVEB=LOG(Q2B/SPME)
      ENDIF
      IF(MCEV.EQ.1) THEN
        ALSDUM=ULALPS(FQ2C*Q2B)
        TEVCB=TEVCB+2.*LOG(ALAM(JT)/PARU(117))
        ALAM(JT)=PARU(117)
        B0=(33.-2.*MSTU(118))/6.
      ENDIF
      TEVCBS=TEVCB
      TEVEBS=TEVEB
 
C...Select side for interference with final state partons.
      IF(MFIS.GE.1.AND.N.LE.NS+2) THEN
        IFI=N-NS
        ISFI(IFI)=0
        IF(IABS(KCFI(IFI)).EQ.1.AND.NFIS(IFI).EQ.1) THEN
          ISFI(IFI)=1
        ELSEIF(KCFI(IFI).EQ.2.AND.NFIS(IFI).EQ.1) THEN
          IF(RLU(0).GT.0.5) ISFI(IFI)=1
        ELSEIF(KCFI(IFI).EQ.2.AND.NFIS(IFI).EQ.2) THEN
          ISFI(IFI)=1
          IF(RLU(0).GT.0.5) ISFI(IFI)=2
        ENDIF
      ENDIF
 
C...Calculate Altarelli-Parisi weights.
      DO 170 KFL=-25,25
      WTAPC(KFL)=0.
      WTAPE(KFL)=0.
      WTSF(KFL)=0.
  170 CONTINUE
C...q -> q, g -> q.
      IF(IABS(KFLB).LE.10) THEN
        WTAPC(KFLB)=(8./3.)*LOG((1.-XEC-XB)*(XB+XEC)/(XEC*(1.-XEC)))
        WTAPC(21)=0.5*(XB/(XB+XEC)-XB/(1.-XEC))
C...f -> f, gamma -> f.
      ELSEIF(IABS(KFLB).LE.20) THEN
        WTAPF1=LOG((1.-XEE-XB)*(XB+XEE)/(XEE*(1.-XEE)))
        WTAPF2=LOG((1.-XEE-XB)*(1.-XEE)/(XEE*(XB+XEE)))
        WTAPE(KFLB)=2.*(WTAPF1+WTAPF2)
        IF(MSTP(12).GE.1) WTAPE(22)=XB/(XB+XEE)-XB/(1.-XEE)
C...f -> g, g -> g.
      ELSEIF(KFLB.EQ.21) THEN
        WTAPQ=(16./3.)*(SQRT((1.-XEC)/XB)-SQRT((XB+XEC)/XB))
        DO 180 KFL=1,MSTP(58)
        WTAPC(KFL)=WTAPQ
        WTAPC(-KFL)=WTAPQ
  180   CONTINUE
        WTAPC(21)=6.*LOG((1.-XEC-XB)/XEC)
C...f -> gamma, W+, W-.
      ELSEIF(KFLB.EQ.22) THEN
        WTAPF=LOG((1.-XEE-XB)*(1.-XEE)/(XEE*(XB+XEE)))/XB
        WTAPE(11)=WTAPF
        WTAPE(-11)=WTAPF
      ELSEIF(KFLB.EQ.24) THEN
        WTAPE(-11)=1./(4.*PARU(102))*LOG((1.-XEE-XB)*(1.-XEE)/
     &  (XEE*(XB+XEE)))/XB
      ELSEIF(KFLB.EQ.-24) THEN
        WTAPE(11)=1./(4.*PARU(102))*LOG((1.-XEE-XB)*(1.-XEE)/
     &  (XEE*(XB+XEE)))/XB
      ENDIF
 
C...Calculate structure function weights and sum.
      NTRY=0
  190 NTRY=NTRY+1
      IF(NTRY.GT.500) THEN
        MINT(51)=1
        RETURN
      ENDIF
      WTSUMC=0.
      WTSUME=0.
      XFBO=MAX(1E-10,XFB(KFLB))
      DO 200 KFL=-25,25
      WTSF(KFL)=XFB(KFL)/XFBO
      WTSUMC=WTSUMC+WTAPC(KFL)*WTSF(KFL)
      WTSUME=WTSUME+WTAPE(KFL)*WTSF(KFL)
  200 CONTINUE
      WTSUMC=MAX(0.0001,WTSUMC)
      WTSUME=MAX(0.0001/FWTE,WTSUME)
 
C...Choose new t: fix alpha_s, alpha_s(Q^2), alpha_s(k_T^2).
      NTRY2=0
  210 NTRY2=NTRY2+1
      IF(NTRY2.GT.500) THEN
        MINT(51)=1
        RETURN
      ENDIF
      IF(MCEV.EQ.1) THEN
        IF(MSTP(64).LE.0) THEN
          TEVCB=TEVCB+LOG(RLU(0))*PARU(2)/(PARU(111)*WTSUMC)
        ELSEIF(MSTP(64).EQ.1) THEN
          TEVCB=TEVCB*EXP(MAX(-50.,LOG(RLU(0))*B0/WTSUMC))
        ELSE
          TEVCB=TEVCB*EXP(MAX(-50.,LOG(RLU(0))*B0/(5.*WTSUMC)))
        ENDIF
      ENDIF
      IF(MEEV.EQ.1) THEN
        TEVEB=TEVEB*EXP(MAX(-50.,LOG(RLU(0))*PARU(2)/
     &  (PARU(101)*FWTE*WTSUME*TEMX)))
      ENDIF
 
C...Translate t into Q2 scale; choose between QCD and QED evolution.
  220 IF(MCEV.EQ.1) Q2CB=ALAM(JT)**2*EXP(MAX(-50.,TEVCB))/FQ2C
      IF(MEEV.EQ.1) Q2EB=SPME*EXP(MAX(-50.,TEVEB))
      MCE=0
      IF(MCEV.EQ.0.AND.MEEV.EQ.0) THEN
      ELSEIF(MCEV.EQ.1.AND.MEEV.EQ.0) THEN
        IF(Q2CB.GT.Q2MNCS(JT)) MCE=1
      ELSEIF(MCEV.EQ.0.AND.MEEV.EQ.1) THEN
        IF(Q2EB.GT.Q2MNE) MCE=2
      ELSEIF(Q2MNCS(JT).GT.Q2MNE) THEN
        MCE=1
        IF(Q2EB.GT.Q2CB.OR.Q2CB.LE.Q2MNCS(JT)) MCE=2
        IF(MCE.EQ.2.AND.Q2EB.LE.Q2MNE) MCE=0
      ELSE
        MCE=2
        IF(Q2CB.GT.Q2EB.OR.Q2EB.LE.Q2MNE) MCE=1
        IF(MCE.EQ.1.AND.Q2CB.LE.Q2MNCS(JT)) MCE=0
      ENDIF
 
C...Evolution possibly ended. Update t values.
      IF(MCE.EQ.0) THEN
        Q2B=0.
        GOTO 250
      ELSEIF(MCE.EQ.1) THEN
        Q2B=Q2CB
        Q2REF=FQ2C*Q2B
        IF(MEEV.EQ.1) TEVEB=LOG(Q2B/SPME)
      ELSE
        Q2B=Q2EB
        Q2REF=Q2B
        IF(MCEV.EQ.1) TEVCB=LOG(FQ2C*Q2B/ALAM(JT)**2)
      ENDIF
 
C...Select flavour for branching parton.
      IF(MCE.EQ.1) WTRAN=RLU(0)*WTSUMC
      IF(MCE.EQ.2) WTRAN=RLU(0)*WTSUME
      KFLA=-25
  230 KFLA=KFLA+1
      IF(MCE.EQ.1) WTRAN=WTRAN-WTAPC(KFLA)*WTSF(KFLA)
      IF(MCE.EQ.2) WTRAN=WTRAN-WTAPE(KFLA)*WTSF(KFLA)
      IF(KFLA.LE.24.AND.WTRAN.GT.0.) GOTO 230
      IF(KFLA.EQ.25) THEN
        Q2B=0.
        GOTO 250
      ENDIF
 
C...Choose z value and corrective weight.
      WTZ=0.
C...q -> q + g.
      IF(IABS(KFLA).LE.10.AND.IABS(KFLB).LE.10) THEN
        Z=1.-((1.-XB-XEC)/(1.-XEC))*
     &  (XEC*(1.-XEC)/((XB+XEC)*(1.-XB-XEC)))**RLU(0)
        WTZ=0.5*(1.+Z**2)
C...q -> g + q.
      ELSEIF(IABS(KFLA).LE.10.AND.KFLB.EQ.21) THEN
        Z=XB/(SQRT(XB+XEC)+RLU(0)*(SQRT(1.-XEC)-SQRT(XB+XEC)))**2
        WTZ=0.5*(1.+(1.-Z)**2)*SQRT(Z)
C...f -> f + gamma.
      ELSEIF(IABS(KFLA).LE.20.AND.IABS(KFLB).LE.20) THEN
        IF(WTAPF1.GT.RLU(0)*(WTAPF1+WTAPF2)) THEN
          Z=1.-((1.-XB-XEE)/(1.-XEE))*
     &    (XEE*(1.-XEE)/((XB+XEE)*(1.-XB-XEE)))**RLU(0)
        ELSE
          Z=XB+XB*(XEE/(1.-XEE))*
     &    ((1.-XB-XEE)*(1.-XEE)/(XEE*(XB+XEE)))**RLU(0)
        ENDIF
        WTZ=0.5*(1.+Z**2)*(Z-XB)/(1.-XB)
C...f -> gamma + f.
      ELSEIF(IABS(KFLA).LE.20.AND.KFLB.EQ.22) THEN
        Z=XB+XB*(XEE/(1.-XEE))*
     &  ((1.-XB-XEE)*(1.-XEE)/(XEE*(XB+XEE)))**RLU(0)
        WTZ=0.5*(1.+(1.-Z)**2)*XB*(Z-XB)/Z
C...f -> W+- + f'.
      ELSEIF(IABS(KFLA).LE.20.AND.IABS(KFLB).EQ.24) THEN
        Z=XB+XB*(XEE/(1.-XEE))*
     &  ((1.-XB-XEE)*(1.-XEE)/(XEE*(XB+XEE)))**RLU(0)
        WTZ=0.5*(1.+(1.-Z)**2)*(XB*(Z-XB)/Z)*(Q2B/(Q2B+PMAS(24,1)**2))
C...g -> q + q~.
      ELSEIF(KFLA.EQ.21.AND.IABS(KFLB).LE.10) THEN
        Z=XB/(1.-XEC)+RLU(0)*(XB/(XB+XEC)-XB/(1.-XEC))
        WTZ=1.-2.*Z*(1.-Z)
C...g -> g + g.
      ELSEIF(KFLA.EQ.21.AND.KFLB.EQ.21) THEN
        Z=1./(1.+((1.-XEC-XB)/XB)*(XEC/(1.-XEC-XB))**RLU(0))
        WTZ=(1.-Z*(1.-Z))**2
C...gamma -> f + f~.
      ELSEIF(KFLA.EQ.22.AND.IABS(KFLB).LE.20) THEN
        Z=XB/(1.-XEE)+RLU(0)*(XB/(XB+XEE)-XB/(1.-XEE))
        WTZ=1.-2.*Z*(1.-Z)
      ENDIF
      IF(MCE.EQ.2) WTZ=(WTZ/FWTE)*(TEVEB/TEMX)
 
C...Option with resummation of soft gluon emission as effective z shift.
      IF(MCE.EQ.1) THEN
        IF(MSTP(65).GE.1) THEN
          RSOFT=6.
          IF(KFLB.NE.21) RSOFT=8./3.
          Z=Z*(TEVCB/TEVCSV(JT))**(RSOFT*XEC/((XB+XEC)*B0))
          IF(Z.LE.XB) GOTO 210
        ENDIF
 
C...Option with alpha_s(k_T^2): demand k_T^2 > cutoff, reweight.
        IF(MSTP(64).GE.2) THEN
          IF((1.-Z)*Q2B.LT.Q2MNCS(JT)) GOTO 210
          ALPRAT=TEVCB/(TEVCB+LOG(1.-Z))
          IF(ALPRAT.LT.5.*RLU(0)) GOTO 210
          IF(ALPRAT.GT.5.) WTZ=WTZ*ALPRAT/5.
        ENDIF
 
C...Impose angular constraint in first branching from interference
C...with final state partons.
        IF(MFIS.GE.1.AND.N.LE.NS+2.AND.NTRY2.LT.200) THEN
          THE2D=(4.*Q2B)/(DSH*(1.-Z))
          IF(N.EQ.NS+1.AND.ISFI(1).GE.1) THEN
            IF(THE2D.GT.THEFIS(1,ISFI(1))**2) GOTO 210
          ELSEIF(N.EQ.NS+2.AND.ISFI(2).GE.1) THEN
            IF(THE2D.GT.THEFIS(2,ISFI(2))**2) GOTO 210
          ENDIF
        ENDIF
 
C...Option with angular ordering requirement.
        IF(MSTP(62).GE.3.AND.NTRY2.LT.200) THEN
          THE2T=(4.*Z**2*Q2B)/(VINT(2)*(1.-Z)*XB**2)
          IF(THE2T.GT.THE2(JT)) GOTO 210
        ENDIF
      ENDIF
 
C...Weighting with new structure functions.
      MINT(105)=MINT(102+JT)
      MINT(109)=MINT(106+JT)
      IF(MSTP(57).LE.1) THEN
        CALL PYSTFU(KFBEAM(JT),XB,Q2REF,XFN)
      ELSE
        CALL PYSTFL(KFBEAM(JT),XB,Q2REF,XFN)
      ENDIF
      XFBN=XFN(KFLB)
      IF(XFBN.LT.1E-20) THEN
        IF(KFLA.EQ.KFLB) THEN
          TEVCB=TEVCBS
          TEVEB=TEVEBS
          WTAPC(KFLB)=0.
          WTAPE(KFLB)=0.
          GOTO 190
        ELSEIF(MCE.EQ.1.AND.TEVCBS-TEVCB.GT.0.2) THEN
          TEVCB=0.5*(TEVCBS+TEVCB)
          GOTO 220
        ELSEIF(MCE.EQ.2.AND.TEVEBS-TEVEB.GT.0.2) THEN
          TEVEB=0.5*(TEVEBS+TEVEB)
          GOTO 220
        ELSE
          XFBN=1E-10
          XFN(KFLB)=XFBN
        ENDIF
      ENDIF
      DO 240 KFL=-25,25
      XFB(KFL)=XFN(KFL)
  240 CONTINUE
      XA=XB/Z
      IF(MSTP(57).LE.1) THEN
        CALL PYSTFU(KFBEAM(JT),XA,Q2REF,XFA)
      ELSE
        CALL PYSTFL(KFBEAM(JT),XA,Q2REF,XFA)
      ENDIF
      XFAN=XFA(KFLA)
      IF(XFAN.LT.1E-20) GOTO 190
      WTSFA=WTSF(KFLA)
      IF(WTZ*XFAN/XFBN.LT.RLU(0)*WTSFA) GOTO 190
 
C...Define two hard scatterers in their CM-frame.
  250 IF(N.EQ.NS+2) THEN
        DQ2(JT)=Q2B
        DPLCM=SQRT((DSH+DQ2(1)+DQ2(2))**2-4D0*DQ2(1)*DQ2(2))/DSHR
        DO 270 JR=1,2
        I=NS+JR
        IF(JR.EQ.1) IPO=IPUS1
        IF(JR.EQ.2) IPO=IPUS2
        DO 260 J=1,5
        K(I,J)=0
        P(I,J)=0.
        V(I,J)=0.
  260   CONTINUE
        K(I,1)=14
        K(I,2)=KFLS(JR+2)
        K(I,4)=IPO
        K(I,5)=IPO
        P(I,3)=DPLCM*(-1)**(JR+1)
        P(I,4)=(DSH+DQ2(3-JR)-DQ2(JR))/DSHR
        P(I,5)=-SQRT(SNGL(DQ2(JR)))
        K(IPO,1)=14
        K(IPO,3)=I
        K(IPO,4)=MOD(K(IPO,4),MSTU(5))+MSTU(5)*I
        K(IPO,5)=MOD(K(IPO,5),MSTU(5))+MSTU(5)*I
  270   CONTINUE
 
C...Find maximum allowed mass of timelike parton.
      ELSEIF(N.GT.NS+2) THEN
        JR=3-JT
        DQ2(3)=Q2B
        DPC(1)=P(IS(1),4)
        DPC(2)=P(IS(2),4)
        DPC(3)=0.5*(ABS(P(IS(1),3))+ABS(P(IS(2),3)))
        DPD(1)=DSH+DQ2(JR)+DQ2(JT)
        DPD(2)=DSHZ+DQ2(JR)+DQ2(3)
        DPD(3)=SQRT(DPD(1)**2-4D0*DQ2(JR)*DQ2(JT))
        DPD(4)=SQRT(DPD(2)**2-4D0*DQ2(JR)*DQ2(3))
        IKIN=0
        IF(Q2S(JR).GE.0.25*Q2MNC.AND.DPD(1)-DPD(3).GE.
     &  1D-10*DPD(1)) IKIN=1
        IF(IKIN.EQ.0) DMSMA=(DQ2(JT)/DBLE(ZS(JT))-DQ2(3))*(DSH/
     &  (DSH+DQ2(JT))-DSH/(DSHZ+DQ2(3)))
        IF(IKIN.EQ.1) DMSMA=(DPD(1)*DPD(2)-DPD(3)*DPD(4))/(2.*
     &  DQ2(JR))-DQ2(JT)-DQ2(3)
 
C...Generate timelike parton shower (if required).
        IT=N
        DO 280 J=1,5
        K(IT,J)=0
        P(IT,J)=0.
        V(IT,J)=0.
  280   CONTINUE
        K(IT,1)=3
C...f -> f + g (gamma).
        IF(IABS(KFLB).LE.20.AND.IABS(KFLS(JT+2)).LE.20) THEN
          K(IT,2)=21
          IF(IABS(KFLB).GE.11) K(IT,2)=22
C...f -> g (gamma, W+-) + f.
        ELSEIF(IABS(KFLB).LE.20.AND.IABS(KFLS(JT+2)).GT.20) THEN
          K(IT,2)=KFLB
          IF(KFLS(JT+2).EQ.24) THEN
            K(IT,2)=-12
          ELSEIF(KFLS(JT+2).EQ.-24) THEN
            K(IT,2)=12
          ENDIF
C...g (gamma) -> f + f~, g + g.
        ELSE
          K(IT,2)=-KFLS(JT+2)
          IF(KFLS(JT+2).GT.20) K(IT,2)=KFLS(JT+2)
        ENDIF
        P(IT,5)=ULMASS(K(IT,2))
        IF(SNGL(DMSMA).LE.P(IT,5)**2) GOTO 100
        IF(MSTP(63).GE.1.AND.MCE.EQ.1) THEN
          MSTJ48=MSTJ(48)
          PARJ85=PARJ(85)
          P(IT,4)=(DSHZ-DSH-P(IT,5)**2)/DSHR
          P(IT,3)=SQRT(P(IT,4)**2-P(IT,5)**2)
          IF(MSTP(63).EQ.1) THEN
            Q2TIM=DMSMA
          ELSEIF(MSTP(63).EQ.2) THEN
            Q2TIM=MIN(SNGL(DMSMA),PARP(71)*Q2S(JT))
          ELSE
            Q2TIM=DMSMA
            MSTJ(48)=1
            IF(IKIN.EQ.0) DPT2=DMSMA*(DSHZ+DQ2(3))/(DSH+DQ2(JT))
            IF(IKIN.EQ.1) DPT2=DMSMA*(0.5*DPD(1)*DPD(2)+0.5*DPD(3)*
     &      DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)))/(4.*DSH*DPC(3)**2)
            PARJ(85)=SQRT(MAX(0.,SNGL(DPT2)))*
     &      (1./P(IT,4)+1./P(IS(JT),4))
          ENDIF
          CALL LUSHOW(IT,0,SQRT(Q2TIM))
          MSTJ(48)=MSTJ48
          PARJ(85)=PARJ85
          IF(N.GE.IT+1) P(IT,5)=P(IT+1,5)
        ENDIF
 
C...Reconstruct kinematics of branching: timelike parton shower.
        DMS=P(IT,5)**2
        IF(IKIN.EQ.0) DPT2=(DMSMA-DMS)*(DSHZ+DQ2(3))/(DSH+DQ2(JT))
        IF(IKIN.EQ.1) DPT2=(DMSMA-DMS)*(0.5*DPD(1)*DPD(2)+0.5*DPD(3)*
     &  DPD(4)-DQ2(JR)*(DQ2(JT)+DQ2(3)+DMS))/(4.*DSH*DPC(3)**2)
        IF(DPT2.LT.0.) GOTO 100
        DPB(1)=(0.5*DPD(2)-DPC(JR)*(DSHZ+DQ2(JR)-DQ2(JT)-DMS)/
     &  DSHR)/DPC(3)-DPC(3)
        P(IT,1)=SQRT(SNGL(DPT2))
        P(IT,3)=DPB(1)*(-1)**(JT+1)
        P(IT,4)=SQRT(DPT2+DPB(1)**2+DMS)
        IF(N.GE.IT+1) THEN
          DPB(1)=SQRT(DPB(1)**2+DPT2)
          DPB(2)=SQRT(DPB(1)**2+DMS)
          DPB(3)=P(IT+1,3)
          DPB(4)=SQRT(DPB(3)**2+DMS)
          DBEZ=(DPB(4)*DPB(1)-DPB(3)*DPB(2))/(DPB(4)*DPB(2)-DPB(3)*
     &    DPB(1))
          CALL LUDBRB(IT+1,N,0.,0.,0D0,0D0,DBEZ)
          THE=ULANGL(P(IT,3),P(IT,1))
          CALL LUDBRB(IT+1,N,THE,0.,0D0,0D0,0D0)
        ENDIF
 
C...Reconstruct kinematics of branching: spacelike parton.
        DO 290 J=1,5
        K(N+1,J)=0
        P(N+1,J)=0.
        V(N+1,J)=0.
  290   CONTINUE
        K(N+1,1)=14
        K(N+1,2)=KFLB
        P(N+1,1)=P(IT,1)
        P(N+1,3)=P(IT,3)+P(IS(JT),3)
        P(N+1,4)=P(IT,4)+P(IS(JT),4)
        P(N+1,5)=-SQRT(SNGL(DQ2(3)))
 
C...Define colour flow of branching.
        K(IS(JT),3)=N+1
        K(IT,3)=N+1
        IM1=N+1
        IM2=N+1
C...f -> f + gamma (Z, W).
        IF(IABS(K(IT,2)).GE.22) THEN
          K(IT,1)=1
          ID1=IS(JT)
          ID2=IS(JT)
C...f -> gamma (Z, W) + f.
        ELSEIF(IABS(K(IS(JT),2)).GE.22) THEN
          ID1=IT
          ID2=IT
C...gamma -> q + q~, g + g.
        ELSEIF(K(N+1,2).EQ.22) THEN
          ID1=IS(JT)
          ID2=IT
          IM1=ID2
          IM2=ID1
C...q -> q + g.
        ELSEIF(K(N+1,2).GT.0.AND.K(N+1,2).NE.21.AND.K(IT,2).EQ.21) THEN
          ID1=IT
          ID2=IS(JT)
C...q -> g + q.
        ELSEIF(K(N+1,2).GT.0.AND.K(N+1,2).NE.21) THEN
          ID1=IS(JT)
          ID2=IT
C...q~ -> q~ + g.
        ELSEIF(K(N+1,2).LT.0.AND.K(IT,2).EQ.21) THEN
          ID1=IS(JT)
          ID2=IT
C...q~ -> g + q~.
        ELSEIF(K(N+1,2).LT.0) THEN
          ID1=IT
          ID2=IS(JT)
C...g -> g + g; g -> q + q~.
        ELSEIF((K(IT,2).EQ.21.AND.RLU(0).GT.0.5).OR.K(IT,2).LT.0) THEN
          ID1=IS(JT)
          ID2=IT
        ELSE
          ID1=IT
          ID2=IS(JT)
        ENDIF
        IF(IM1.EQ.N+1) K(IM1,4)=K(IM1,4)+ID1
        IF(IM2.EQ.N+1) K(IM2,5)=K(IM2,5)+ID2
        K(ID1,4)=K(ID1,4)+MSTU(5)*IM1
        K(ID2,5)=K(ID2,5)+MSTU(5)*IM2
        IF(ID1.NE.ID2) THEN
          K(ID1,5)=K(ID1,5)+MSTU(5)*ID2
          K(ID2,4)=K(ID2,4)+MSTU(5)*ID1
        ENDIF
        N=N+1
 
C...Boost to new CM-frame.
        DBSVX=DBLE((P(N,1)+P(IS(JR),1))/(P(N,4)+P(IS(JR),4)))
        DBSVZ=DBLE((P(N,3)+P(IS(JR),3))/(P(N,4)+P(IS(JR),4)))
        IF(DBSVX**2+DBSVZ**2.GE.1D0) GOTO 100
        CALL LUDBRB(NS+1,N,0.,0.,-DBSVX,0D0,-DBSVZ)
        IR=N+(JT-1)*(IS(1)-N)
        CALL LUDBRB(NS+1,N,-ULANGL(P(IR,3),P(IR,1)),PARU(2)*RLU(0),
     &  0D0,0D0,0D0)
      ENDIF
 
C...Update kinematics variables.
      IS(JT)=N
      DQ2(JT)=Q2B
      IF(MSTP(62).GE.3) THE2(JT)=THE2T
      DSH=DSHZ
 
C...Save quantities; loop back.
      Q2S(JT)=Q2B
      IF((MCEV.EQ.1.AND.Q2B.GE.0.25*Q2MNC).OR.
     &(MEEV.EQ.1.AND.Q2B.GE.Q2MNE)) THEN
        KFLS(JT+2)=KFLS(JT)
        KFLS(JT)=KFLA
        XS(JT)=XA
        ZS(JT)=Z
        DO 300 KFL=-25,25
        XFS(JT,KFL)=XFA(KFL)
  300   CONTINUE
        TEVCSV(JT)=TEVCB
        TEVESV(JT)=TEVEB
      ELSE
        MORE(JT)=0
        IF(JT.EQ.1) IPU1=N
        IF(JT.EQ.2) IPU2=N
      ENDIF
      IF(N.GT.MSTU(4)-MSTU(32)-10) THEN
        CALL LUERRM(11,'(PYSSPA:) no more memory left in LUJETS')
        IF(MSTU(21).GE.1) N=NS
        IF(MSTU(21).GE.1) RETURN
      ENDIF
      IF(MORE(1).EQ.1.OR.MORE(2).EQ.1) GOTO 150
 
C...Boost hard scattering partons to frame of shower initiators.
      DO 310 J=1,3
      ROBO(J+2)=(P(NS+1,J)+P(NS+2,J))/(P(NS+1,4)+P(NS+2,4))
  310 CONTINUE
      K(N+2,1)=1
      DO 320 J=1,5
      P(N+2,J)=P(NS+1,J)
  320 CONTINUE
      ROBOT=ROBO(3)**2+ROBO(4)**2+ROBO(5)**2
      IF(ROBOT.GE.0.999999) THEN
        ROBOT=1.00001*SQRT(ROBOT)
        ROBO(3)=ROBO(3)/ROBOT
        ROBO(4)=ROBO(4)/ROBOT
        ROBO(5)=ROBO(5)/ROBOT
      ENDIF
      CALL LUDBRB(N+2,N+2,0.,0.,-DBLE(ROBO(3)),-DBLE(ROBO(4)),
     &-DBLE(ROBO(5)))
      ROBO(2)=ULANGL(P(N+2,1),P(N+2,2))
      ROBO(1)=ULANGL(P(N+2,3),SQRT(P(N+2,1)**2+P(N+2,2)**2))
      CALL LUDBRB(MINT(83)+5,NS,ROBO(1),ROBO(2),DBLE(ROBO(3)),
     &DBLE(ROBO(4)),DBLE(ROBO(5)))
 
C...Store user information. Reset Lambda value.
      K(IPU1,3)=MINT(83)+3
      K(IPU2,3)=MINT(83)+4
      DO 330 JT=1,2
      MINT(12+JT)=KFLS(JT)
      VINT(140+JT)=XS(JT)
      IF(MINT(18+JT).EQ.1) VINT(140+JT)=VINT(154+JT)*XS(JT)
  330 CONTINUE
      PARU(112)=ALAMS
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYRESD
 
C...Allows resonances to decay (including parton showers for hadronic
C...channels).
      IMPLICIT DOUBLE PRECISION(D)
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      REAL  WIDP,WIDE,WIDS
      SAVE /PYINT4/
      DIMENSION IREF(20,8),KDCY(3),KFL1(3),KFL2(3),KEQL(3),NSD(3),
     &ILIN(6),HGZ(3,3),COUP(6,4),CORL(2,2,2),PK(6,4),PKK(6,6),
     &CTHE(3),PHI(3),WDTP(0:40),WDTE(0:40,0:5),DBEZQQ(3),DPMO(5)
      COMPLEX FGK,HA(6,6),HC(6,6)
 
C...The F, Xi and Xj functions of Gunion and Kunszt
C...(Phys. Rev. D33, 665, plus errata from the authors).
      FGK(I1,I2,I3,I4,I5,I6)=4.*HA(I1,I3)*HC(I2,I6)*(HA(I1,I5)*
     &HC(I1,I4)+HA(I3,I5)*HC(I3,I4))
      DIGK(DT,DU)=-4.*D34*D56+DT*(3.*DT+4.*DU)+DT**2*(DT*DU/(D34*D56)-
     &2.*(1./D34+1./D56)*(DT+DU)+2.*(D34/D56+D56/D34))
      DJGK(DT,DU)=8.*(D34+D56)**2-8.*(D34+D56)*(DT+DU)-6.*DT*DU-
     &2.*DT*DU*(DT*DU/(D34*D56)-2.*(1./D34+1./D56)*(DT+DU)+
     &2.*(D34/D56+D56/D34))
 
C...Some general constants.
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2) XW=1.-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1.-XW
      SQMZ=PMAS(23,1)**2
      SQMW=PMAS(24,1)**2
      SH=VINT(44)
 
C...Define initial one, two or three objects.
      ISUB=MINT(1)
      DO 100 JT=1,8
      IREF(1,JT)=0
  100 CONTINUE
      IF(ISET(ISUB).EQ.1.OR.ISET(ISUB).EQ.3) THEN
        IREF(1,1)=MINT(84)+2+ISET(ISUB)
        IREF(1,4)=MINT(83)+6+ISET(ISUB)
      ELSEIF(ISET(ISUB).EQ.2.OR.ISET(ISUB).EQ.4) THEN
        IREF(1,1)=MINT(84)+1+ISET(ISUB)
        IREF(1,2)=MINT(84)+2+ISET(ISUB)
        IREF(1,4)=MINT(83)+5+ISET(ISUB)
        IREF(1,5)=MINT(83)+6+ISET(ISUB)
      ELSEIF(ISET(ISUB).EQ.5) THEN
        IREF(1,1)=MINT(84)+3
        IREF(1,2)=MINT(84)+4
        IREF(1,3)=MINT(84)+5
        IREF(1,4)=MINT(83)+7
        IREF(1,5)=MINT(83)+8
        IREF(1,6)=MINT(83)+9
      ELSEIF(ISET(ISUB).EQ.6) THEN
        IREF(1,1)=MINT(84)+4
        IREF(1,2)=MINT(84)+5
        IREF(1,3)=MINT(84)+3
        IREF(1,4)=MINT(83)+8
        IREF(1,5)=MINT(83)+9
        IREF(1,6)=MINT(83)+7
      ENDIF
 
C...Check if initial resonance has been moved (in resonance + jet).
      DO 120 JT=1,3
      IF(IREF(1,JT).GT.0) THEN
        IF(K(IREF(1,JT),1).GT.10) THEN
          KFA=IABS(K(IREF(1,JT),2))
          IF(KFA.EQ.6.OR.KFA.EQ.7.OR.KFA.EQ.8.OR.KFA.EQ.39) THEN
            DO 110 I=IREF(1,JT)+1,N
            IF(K(I,1).LE.10.AND.K(I,2).EQ.K(IREF(1,JT),2)) IREF(1,JT)=I
  110       CONTINUE
          ELSE
            KDA=MOD(K(IREF(1,JT),4),MSTU(4))
            IF(KFA.GE.23.AND.KFA.LE.40.AND.KDA.GT.1) IREF(1,JT)=KDA
          ENDIF
        ENDIF
      ENDIF
  120 CONTINUE
 
C...Loop over decay history.
      NP=1
      IP=0
  130 IP=IP+1
      NINH=0
      JTMAX=2
      IF(IP.EQ.1.AND.IREF(1,2).EQ.0) JTMAX=1
      IF(IP.EQ.1.AND.IREF(1,3).NE.0) JTMAX=3
      ITLH=0
      NSAV=N
 
C...Start treatment of one or two resonances in parallel.
  140 N=NSAV
      DO 170 JT=1,JTMAX
      ID=IREF(IP,JT)
      KDCY(JT)=0
      KFL1(JT)=0
      KFL2(JT)=0
      KEQL(JT)=0
      NSD(JT)=ID
      IF(ID.EQ.0) GOTO 160
      KFA=IABS(K(ID,2))
      IF((KFA.LT.23.OR.KFA.GT.40).AND.KFA.NE.6.AND.KFA.NE.7.AND.
     &KFA.NE.8.AND.KFA.NE.17.AND.KFA.NE.18) GOTO 160
      IF(MSTP(48).LE.0.AND.KFA.EQ.6) GOTO 160
      IF(MSTP(6).NE.1.AND.MSTP(49).LE.0.AND.(KFA.EQ.7.OR.KFA.EQ.8.OR.
     &KFA.EQ.17.OR.KFA.EQ.18)) GOTO 160
      IF(K(ID,1).GT.10.OR.MDCY(KFA,1).EQ.0) GOTO 160
      IF(KFA.EQ.6.OR.(MSTP(6).NE.1.AND.(KFA.EQ.7.OR.KFA.EQ.8.OR.
     &KFA.EQ.17.OR.KFA.EQ.18))) ITLH=ITLH+1
      K(ID,4)=MSTU(5)*(K(ID,4)/MSTU(5))
      K(ID,5)=MSTU(5)*(K(ID,5)/MSTU(5))
 
C...Select decay channel.
      KFB=0
      IF(ISET(ISUB).NE.6.OR.JT.NE.3) THEN
        IF(ISUB.EQ.1.OR.ISUB.EQ.15.OR.ISUB.EQ.19.OR.ISUB.EQ.22.OR.
     &  ISUB.EQ.30.OR.ISUB.EQ.35.OR.ISUB.EQ.141) MINT(61)=1
        CALL PYWIDT(KFA,P(ID,5)**2,WDTP,WDTE)
        IF(KCHG(KFA,3).EQ.0) THEN
          IPM=2
        ELSE
          IPM=(5-ISIGN(1,K(ID,2)))/2
        ENDIF
        IF(JTMAX.GE.2.AND.JT.LE.2) KFB=IABS(K(IREF(IP,3-JT),2))
        WDTE0S=WDTE(0,1)+WDTE(0,IPM)+WDTE(0,4)
        IF(KFB.EQ.KFA) WDTE0S=WDTE0S+WDTE(0,5)
        IF(WDTE0S.LE.0.) THEN
          IF(KFA.EQ.6.OR.KFA.EQ.7.OR.KFA.EQ.8.OR.KFA.EQ.17.OR.
     &    KFA.EQ.18) THEN
            MINT(51)=1
            RETURN
          ELSE
            GOTO 160
          ENDIF
        ENDIF
        RKFL=WDTE0S*RLU(0)
        IDL=0
  150   IDL=IDL+1
        IDC=IDL+MDCY(KFA,2)-1
        RKFL=RKFL-(WDTE(IDL,1)+WDTE(IDL,IPM)+WDTE(IDL,4))
        IF(KFB.EQ.KFA) RKFL=RKFL-WDTE(IDL,5)
        IF(IDL.LT.MDCY(KFA,3).AND.RKFL.GT.0.) GOTO 150
      ELSE
        IDC=MINT(35)
      ENDIF
 
C...Read out and classify decay channel chosen.
      KFL1(JT)=KFDP(IDC,1)*ISIGN(1,K(ID,2))
      KFC1A=IABS(KFL1(JT))
      IF(KFC1A.GT.100) KFC1A=LUCOMP(KFC1A)
      IF(KCHG(KFC1A,3).EQ.0) KFL1(JT)=IABS(KFL1(JT))
      KFL2(JT)=KFDP(IDC,2)*ISIGN(1,K(ID,2))
      KFC2A=IABS(KFL2(JT))
      IF(KFC2A.GT.100) KFC2A=LUCOMP(KFC2A)
      IF(KCHG(KFC2A,3).EQ.0) KFL2(JT)=IABS(KFL2(JT))
      KDCY(JT)=2
      IF(IABS(KFL1(JT)).LE.10.OR.KFL1(JT).EQ.21) KDCY(JT)=1
      IF(IABS(KFL1(JT)).GE.23.AND.IABS(KFL1(JT)).LE.40) KDCY(JT)=3
      IF(KFB.EQ.KFA) KEQL(JT)=MDME(IDC,1)
      NSD(JT)=N
      HGZ(JT,1)=VINT(111)
      HGZ(JT,2)=VINT(112)
      HGZ(JT,3)=VINT(114)
 
C...Select masses and check that mass sum not too large.
      IF(MSTP(42).LE.0.OR.(PMAS(KFC1A,2).LT.PARP(41).AND.
     &PMAS(KFC2A,2).LT.PARP(41))) THEN
        P(N+1,5)=PMAS(KFC1A,1)
        P(N+2,5)=PMAS(KFC2A,1)
        IF(P(N+1,5)+P(N+2,5)+PARJ(64).GT.P(ID,5)) THEN
          CALL LUERRM(13,'(PYRESD:) daughter masses too large')
          MINT(51)=1
          RETURN
        ENDIF
      ELSEIF(IP.EQ.1) THEN
        CALL PYOFSH(2,KFA,KFL1(JT),KFL2(JT),P(ID,5),P(N+1,5),P(N+2,5))
        IF(MINT(51).EQ.1) RETURN
      ELSE
        CALL PYOFSH(7,KFA,KFL1(JT),KFL2(JT),P(ID,5),P(N+1,5),P(N+2,5))
        IF(MINT(51).EQ.1) RETURN
      ENDIF
 
C...Fill decay products, prepared for parton showers for quarks.
C...Special cases, done by hand, for techni-eta, t, leptoquark and q*.
      MSTU(10)=1
      IF(KFA.EQ.38.OR.KFA.EQ.39.OR.((MSTP(6).EQ.1.OR.MSTP(49).GE.1).AND.
     &(KFA.EQ.7.OR.KFA.EQ.8)).OR.KFA.EQ.6) THEN
        MSTU(19)=1
        CALL LU2ENT(N+1,KFL1(JT),KFL2(JT),P(ID,5))
        ISID=4
        IF(K(ID,2).LT.0) ISID=5
        IF(KFA.EQ.38) THEN
          IF(KFC1A.EQ.21.AND.RLU(0).GT.0.5) ISID=9-ISID
          K(N-1,1)=3
          K(N,1)=3
          K(ID,ISID)=K(ID,ISID)+(N-1)
          K(ID,9-ISID)=K(ID,9-ISID)+N
          K(N-1,ISID)=MSTU(5)*ID
          K(N-1,9-ISID)=MSTU(5)*N
          K(N,ISID)=MSTU(5)*(N-1)
          K(N,9-ISID)=MSTU(5)*ID
        ELSEIF(KFA.EQ.6.OR.(MSTP(6).NE.1.AND.(KFA.EQ.7.OR.KFA.EQ.8)))
     &  THEN
          K(N-1,1)=1
          K(N,1)=3
          K(ID,ISID)=K(ID,ISID)+N
          K(N,ISID)=MSTU(5)*ID
        ELSEIF(KFA.EQ.39) THEN
          K(N-1,1)=3
          K(N,1)=1
          K(ID,ISID)=K(ID,ISID)+(N-1)
          K(N-1,ISID)=MSTU(5)*ID
        ELSEIF(KFL1(JT).NE.21) THEN
          K(N-1,1)=1
          K(N,1)=3
          K(ID,ISID)=K(ID,ISID)+N
          K(N,ISID)=MSTU(5)*ID
        ELSE
          K(N-1,1)=3
          K(N,1)=3
          K(ID,ISID)=K(ID,ISID)+(N-1)
          K(N-1,ISID)=MSTU(5)*ID
          K(N-1,9-ISID)=MSTU(5)*N
          K(N,ISID)=MSTU(5)*(N-1)
        ENDIF
      ELSEIF(KDCY(JT).EQ.1) THEN
        CALL LU2ENT(-(N+1),KFL1(JT),KFL2(JT),P(ID,5))
      ELSE
        CALL LU2ENT(N+1,KFL1(JT),KFL2(JT),P(ID,5))
      ENDIF
      MSTU(10)=2
  160 IF(KFA.GE.23.AND.KFA.LE.40.AND.KFL1(JT).EQ.0) NINH=NINH+1
  170 CONTINUE
 
C...Check for allowed combinations. Skip if no decays.
      IF(JTMAX.GE.2) THEN
        IF(KEQL(1).EQ.4.AND.KEQL(2).EQ.4) GOTO 140
        IF(KEQL(1).EQ.5.AND.KEQL(2).EQ.5) GOTO 140
      ENDIF
      IF(JTMAX.EQ.1.AND.KDCY(1).EQ.0) GOTO 480
      IF(JTMAX.EQ.2.AND.KDCY(1).EQ.0.AND.KDCY(2).EQ.0) GOTO 480
      IF(JTMAX.EQ.3.AND.KDCY(1).EQ.0.AND.KDCY(2).EQ.0.AND.
     &KDCY(3).EQ.0) GOTO 480
 
C...Order incoming partons and outgoing resonances.
      IF(JTMAX.EQ.2.AND.MSTP(47).GE.1.AND.NINH.EQ.0) THEN
        ILIN(1)=MINT(84)+1
        IF(K(MINT(84)+1,2).GT.0) ILIN(1)=MINT(84)+2
        IF(K(ILIN(1),2).EQ.21) ILIN(1)=2*MINT(84)+3-ILIN(1)
        ILIN(2)=2*MINT(84)+3-ILIN(1)
        IMIN=1
        IF(IREF(IP,7).EQ.25.OR.IREF(IP,7).EQ.35.OR.IREF(IP,7)
     &  .EQ.36) IMIN=3
        IMAX=2
        IORD=1
        IF(K(IREF(IP,1),2).EQ.23) IORD=2
        IF(K(IREF(IP,1),2).EQ.24.AND.K(IREF(IP,2),2).EQ.-24) IORD=2
        IAKIPD=IABS(K(IREF(IP,IORD),2))
        IF(IAKIPD.EQ.25.OR.IAKIPD.EQ.35.OR.IAKIPD.EQ.36) IORD=3-IORD
        IF(KDCY(IORD).EQ.0) IORD=3-IORD
 
C...Order decay products of resonances.
        DO 180 JT=IORD,3-IORD,3-2*IORD
        IF(KDCY(JT).EQ.0) THEN
          ILIN(IMAX+1)=NSD(JT)
          IMAX=IMAX+1
        ELSEIF(K(NSD(JT)+1,2).GT.0) THEN
          ILIN(IMAX+1)=N+2*JT-1
          ILIN(IMAX+2)=N+2*JT
          IMAX=IMAX+2
          K(N+2*JT-1,2)=K(NSD(JT)+1,2)
          K(N+2*JT,2)=K(NSD(JT)+2,2)
        ELSE
          ILIN(IMAX+1)=N+2*JT
          ILIN(IMAX+2)=N+2*JT-1
          IMAX=IMAX+2
          K(N+2*JT-1,2)=K(NSD(JT)+1,2)
          K(N+2*JT,2)=K(NSD(JT)+2,2)
        ENDIF
  180   CONTINUE
 
C...Find charge, isospin, left- and righthanded couplings.
        DO 200 I=IMIN,IMAX
        DO 190 J=1,4
        COUP(I,J)=0.
  190   CONTINUE
        KFA=IABS(K(ILIN(I),2))
        IF(KFA.EQ.0.OR.KFA.GT.20) GOTO 200
        COUP(I,1)=KCHG(KFA,1)/3.
        COUP(I,2)=(-1)**MOD(KFA,2)
        COUP(I,4)=-2.*COUP(I,1)*XWV
        COUP(I,3)=COUP(I,2)+COUP(I,4)
  200   CONTINUE
 
C...Full propagator dependence and flavour correlations for 2 gamma*/Z.
        IF(ISUB.EQ.22) THEN
          DO 230 I=3,5,2
          I1=IORD
          IF(I.EQ.5) I1=3-IORD
          DO 220 J1=1,2
          DO 210 J2=1,2
          CORL(I/2,J1,J2)=COUP(1,1)**2*HGZ(I1,1)*COUP(I,1)**2/16.+
     &    COUP(1,1)*COUP(1,J1+2)*HGZ(I1,2)*COUP(I,1)*COUP(I,J2+2)/4.+
     &    COUP(1,J1+2)**2*HGZ(I1,3)*COUP(I,J2+2)**2
  210     CONTINUE
  220     CONTINUE
  230     CONTINUE
          COWT12=(CORL(1,1,1)+CORL(1,1,2))*(CORL(2,1,1)+CORL(2,1,2))+
     &    (CORL(1,2,1)+CORL(1,2,2))*(CORL(2,2,1)+CORL(2,2,2))
          COMX12=(CORL(1,1,1)+CORL(1,1,2)+CORL(1,2,1)+CORL(1,2,2))*
     &    (CORL(2,1,1)+CORL(2,1,2)+CORL(2,2,1)+CORL(2,2,2))
          IF(COWT12.LT.RLU(0)*COMX12) GOTO 140
        ENDIF
      ENDIF
 
C...Select angular orientation type - Z'/W' only.
      MZPWP=0
      IF(ISUB.EQ.141) THEN
        IF(RLU(0).LT.PARU(130)) MZPWP=1
        IF(IP.EQ.2) THEN
          IF(IABS(K(IREF(2,1),2)).EQ.37) MZPWP=2
          IAKIR=IABS(K(IREF(2,2),2))
          IF(IAKIR.EQ.25.OR.IAKIR.EQ.35.OR.IAKIR.EQ.36) MZPWP=2
        ENDIF
        IF(IP.GE.3) MZPWP=2
      ELSEIF(ISUB.EQ.142) THEN
        IF(RLU(0).LT.PARU(136)) MZPWP=1
        IF(IP.EQ.2) THEN
          IAKIR=IABS(K(IREF(2,2),2))
          IF(IAKIR.EQ.25.OR.IAKIR.EQ.35.OR.IAKIR.EQ.36) MZPWP=2
        ENDIF
        IF(IP.GE.3) MZPWP=2
      ENDIF
 
C...Select random angles (begin of weighting procedure).
  240 DO 250 JT=1,JTMAX
      IF(KDCY(JT).EQ.0) GOTO 250
      IF(ISET(ISUB).EQ.6.AND.JT.EQ.3) GOTO 250
      IF(JTMAX.EQ.1) THEN
        CTHE(JT)=VINT(13)+(VINT(33)-VINT(13)+VINT(34)-VINT(14))*RLU(0)
        IF(CTHE(JT).GT.VINT(33)) CTHE(JT)=CTHE(JT)+VINT(14)-VINT(33)
        PHI(JT)=VINT(24)
      ELSE
        CTHE(JT)=2.*RLU(0)-1.
        PHI(JT)=PARU(2)*RLU(0)
      ENDIF
  250 CONTINUE
 
      IF(JTMAX.EQ.2.AND.MSTP(47).GE.1.AND.NINH.EQ.0) THEN
C...Construct massless four-vectors.
        DO 270 I=N+1,N+4
        K(I,1)=1
        DO 260 J=1,5
        P(I,J)=0.
        V(I,J)=0.
  260   CONTINUE
  270   CONTINUE
        DO 280 JT=1,JTMAX
        IF(KDCY(JT).EQ.0) GOTO 280
        ID=IREF(IP,JT)
        P(N+2*JT-1,3)=0.5*P(ID,5)
        P(N+2*JT-1,4)=0.5*P(ID,5)
        P(N+2*JT,3)=-0.5*P(ID,5)
        P(N+2*JT,4)=0.5*P(ID,5)
        CALL LUDBRB(N+2*JT-1,N+2*JT,ACOS(CTHE(JT)),PHI(JT),DBLE(P(ID,1)/
     &  P(ID,4)),DBLE(P(ID,2)/P(ID,4)),DBLE(P(ID,3)/P(ID,4)))
  280   CONTINUE
 
C...Store incoming and outgoing momenta, with random rotation to
C...avoid accidental zeroes in HA expressions.
        DO 300 I=1,IMAX
        K(N+4+I,1)=1
        P(N+4+I,4)=SQRT(P(ILIN(I),1)**2+P(ILIN(I),2)**2+P(ILIN(I),3)**2+
     &  P(ILIN(I),5)**2)
        P(N+4+I,5)=P(ILIN(I),5)
        DO 290 J=1,3
        P(N+4+I,J)=P(ILIN(I),J)
  290   CONTINUE
  300   CONTINUE
  310   THERR=ACOS(2.*RLU(0)-1.)
        PHIRR=PARU(2)*RLU(0)
        CALL LUDBRB(N+5,N+4+IMAX,THERR,PHIRR,0D0,0D0,0D0)
        DO 330 I=1,IMAX
        IF(P(N+4+I,1)**2+P(N+4+I,2)**2.LT.1E-4*P(N+4+I,4)**2) GOTO 310
        DO 320 J=1,4
        PK(I,J)=P(N+4+I,J)
  320   CONTINUE
  330   CONTINUE
 
C...Calculate internal products.
        IF(ISUB.EQ.22.OR.ISUB.EQ.23.OR.ISUB.EQ.25.OR.ISUB.EQ.141.OR.
     &  ISUB.EQ.142) THEN
          DO 350 I1=IMIN,IMAX-1
          DO 340 I2=I1+1,IMAX
          HA(I1,I2)=SQRT((PK(I1,4)-PK(I1,3))*(PK(I2,4)+PK(I2,3))/
     &    (1E-20+PK(I1,1)**2+PK(I1,2)**2))*CMPLX(PK(I1,1),PK(I1,2))-
     &    SQRT((PK(I1,4)+PK(I1,3))*(PK(I2,4)-PK(I2,3))/
     &    (1E-20+PK(I2,1)**2+PK(I2,2)**2))*CMPLX(PK(I2,1),PK(I2,2))
          HC(I1,I2)=CONJG(HA(I1,I2))
          IF(I1.LE.2) HA(I1,I2)=CMPLX(0.,1.)*HA(I1,I2)
          IF(I1.LE.2) HC(I1,I2)=CMPLX(0.,1.)*HC(I1,I2)
          HA(I2,I1)=-HA(I1,I2)
          HC(I2,I1)=-HC(I1,I2)
  340     CONTINUE
  350     CONTINUE
        ENDIF
        DO 370 I=1,2
        DO 360 J=1,4
        PK(I,J)=-PK(I,J)
  360   CONTINUE
  370   CONTINUE
        DO 390 I1=IMIN,IMAX-1
        DO 380 I2=I1+1,IMAX
        PKK(I1,I2)=2.*(PK(I1,4)*PK(I2,4)-PK(I1,1)*PK(I2,1)-
     &  PK(I1,2)*PK(I2,2)-PK(I1,3)*PK(I2,3))
        PKK(I2,I1)=PKK(I1,I2)
  380   CONTINUE
  390   CONTINUE
      ENDIF
 
      KFAGM=IABS(IREF(IP,7))
      IF(MSTP(47).LE.0.OR.NINH.NE.0) THEN
C...Isotropic decay selected by user.
        WT=1.
        WTMAX=1.
 
      ELSEIF(ITLH.GE.1) THEN
C... Isotropic decay t -> b + W etc for 4th generation q and l.
        WT=1.
        WTMAX=1.
 
      ELSEIF(IREF(IP,7).EQ.25.OR.IREF(IP,7).EQ.35.OR.
     &IREF(IP,7).EQ.36) THEN
C...Angular weight for H0 -> Z0 + Z0 or W+ + W- -> 4 quarks/leptons.
        WT=16.*PKK(3,5)*PKK(4,6)
        IF(IP.EQ.1) WTMAX=SH**2
        IF(IP.GE.2) WTMAX=P(IREF(IP,8),5)**4
        KFA=IABS(K(IREF(IP,1),2))
        IF(KFA.NE.23.AND.KFA.NE.24) WT=WTMAX
 
      ELSEIF((KFAGM.EQ.6.OR.(MSTP(6).NE.1.AND.(KFAGM.EQ.7.OR.
     &KFAGM.EQ.8.OR.KFAGM.EQ.17.OR.KFAGM.EQ.18))).AND.
     &IABS(K(IREF(IP,1),2)).EQ.24) THEN
C...Angular correlation in f -> f' + W -> f' + 2 quarks/leptons.
        I1=IREF(IP,8)
        IF(MOD(KFAGM,2).EQ.0) THEN
          I2=N+1
          I3=N+2
        ELSE
          I2=N+2
          I3=N+1
        ENDIF
        I4=IREF(IP,2)
        WT=(P(I1,4)*P(I2,4)-P(I1,1)*P(I2,1)-P(I1,2)*P(I2,2)-
     &  P(I1,3)*P(I2,3))*(P(I3,4)*P(I4,4)-P(I3,1)*P(I4,1)-
     &  P(I3,2)*P(I4,2)-P(I3,3)*P(I4,3))
        WTMAX=(P(I1,5)**4-P(IREF(IP,1),5)**4)/8.
        IF(KFAGM.EQ.6.AND.MSTP(48).LE.1) WT=WTMAX
        IF(KFAGM.NE.6.AND.MSTP(49).LE.1) WT=WTMAX
 
      ELSEIF(ISUB.EQ.1) THEN
C...Angular weight for gamma*/Z0 -> 2 quarks/leptons.
        EI=KCHG(IABS(MINT(15)),1)/3.
        AI=SIGN(1.,EI+0.1)
        VI=AI-4.*EI*XWV
        EF=KCHG(IABS(KFL1(1)),1)/3.
        AF=SIGN(1.,EF+0.1)
        VF=AF-4.*EF*XWV
        ASYM=2.*(EI*AI*VINT(112)*EF*AF+4.*VI*AI*VINT(114)*VF*AF)/
     &  (EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*EF*VF+
     &  (VI**2+AI**2)*VINT(114)*(VF**2+AF**2))
        WT=1.+ASYM*CTHE(1)*ISIGN(1,MINT(15)*KFL1(1))+CTHE(1)**2
        WTMAX=2.+ABS(ASYM)
 
      ELSEIF(ISUB.EQ.2) THEN
C...Angular weight for W+/- -> 2 quarks/leptons.
        WT=(1.+CTHE(1)*ISIGN(1,MINT(15)*KFL1(1)))**2
        WTMAX=4.
 
      ELSEIF(ISUB.EQ.15.OR.ISUB.EQ.19) THEN
C...Angular weight for f + f~ -> gluon/gamma + (gamma*/Z0) ->
C...-> gluon/gamma + 2 quarks/leptons.
        CLILF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,3)*HGZ(2,2)*COUP(3,1)*COUP(3,3)/4.+
     &  COUP(1,3)**2*HGZ(2,3)*COUP(3,3)**2
        CLIRF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,3)*HGZ(2,2)*COUP(3,1)*COUP(3,4)/4.+
     &  COUP(1,3)**2*HGZ(2,3)*COUP(3,4)**2
        CRILF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,4)*HGZ(2,2)*COUP(3,1)*COUP(3,3)/4.+
     &  COUP(1,4)**2*HGZ(2,3)*COUP(3,3)**2
        CRIRF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,4)*HGZ(2,2)*COUP(3,1)*COUP(3,4)/4.+
     &  COUP(1,4)**2*HGZ(2,3)*COUP(3,4)**2
        WT=(CLILF+CRIRF)*(PKK(1,3)**2+PKK(2,4)**2)+
     &  (CLIRF+CRILF)*(PKK(1,4)**2+PKK(2,3)**2)
        WTMAX=(CLILF+CLIRF+CRILF+CRIRF)*
     &  ((PKK(1,3)+PKK(1,4))**2+(PKK(2,3)+PKK(2,4))**2)
 
      ELSEIF(ISUB.EQ.16.OR.ISUB.EQ.20) THEN
C...Angular weight for f + f~' -> gluon/gamma + W+/- ->
C...-> gluon/gamma + 2 quarks/leptons.
        WT=PKK(1,3)**2+PKK(2,4)**2
        WTMAX=(PKK(1,3)+PKK(1,4))**2+(PKK(2,3)+PKK(2,4))**2
 
      ELSEIF(ISUB.EQ.22) THEN
C...Angular weight for f + f~ -> Z0 + Z0 -> 4 quarks/leptons.
        S34=P(IREF(IP,IORD),5)**2
        S56=P(IREF(IP,3-IORD),5)**2
        TI=PKK(1,3)+PKK(1,4)+S34
        UI=PKK(1,5)+PKK(1,6)+S56
        FGK135=ABS(FGK(1,2,3,4,5,6)/TI+FGK(1,2,5,6,3,4)/UI)**2
        FGK145=ABS(FGK(1,2,4,3,5,6)/TI+FGK(1,2,5,6,4,3)/UI)**2
        FGK136=ABS(FGK(1,2,3,4,6,5)/TI+FGK(1,2,6,5,3,4)/UI)**2
        FGK146=ABS(FGK(1,2,4,3,6,5)/TI+FGK(1,2,6,5,4,3)/UI)**2
        FGK253=ABS(FGK(2,1,5,6,3,4)/TI+FGK(2,1,3,4,5,6)/UI)**2
        FGK263=ABS(FGK(2,1,6,5,3,4)/TI+FGK(2,1,3,4,6,5)/UI)**2
        FGK254=ABS(FGK(2,1,5,6,4,3)/TI+FGK(2,1,4,3,5,6)/UI)**2
        FGK264=ABS(FGK(2,1,6,5,4,3)/TI+FGK(2,1,4,3,6,5)/UI)**2
        WT=
     &  CORL(1,1,1)*CORL(2,1,1)*FGK135+CORL(1,1,2)*CORL(2,1,1)*FGK145+
     &  CORL(1,1,1)*CORL(2,1,2)*FGK136+CORL(1,1,2)*CORL(2,1,2)*FGK146+
     &  CORL(1,2,1)*CORL(2,2,1)*FGK253+CORL(1,2,2)*CORL(2,2,1)*FGK263+
     &  CORL(1,2,1)*CORL(2,2,2)*FGK254+CORL(1,2,2)*CORL(2,2,2)*FGK264
        WTMAX=16.*((CORL(1,1,1)+CORL(1,1,2))*(CORL(2,1,1)+CORL(2,1,2))+
     &  (CORL(1,2,1)+CORL(1,2,2))*(CORL(2,2,1)+CORL(2,2,2)))*S34*S56*
     &  ((TI**2+UI**2+2.*SH*(S34+S56))/(TI*UI)-S34*S56*(1./TI**2+
     &  1./UI**2))
 
      ELSEIF(ISUB.EQ.23) THEN
C...Angular weight for f + f~' -> Z0 + W+/- -> 4 quarks/leptons.
        D34=P(IREF(IP,IORD),5)**2
        D56=P(IREF(IP,3-IORD),5)**2
        DT=PKK(1,3)+PKK(1,4)+D34
        DU=PKK(1,5)+PKK(1,6)+D56
        FACBW=1./((SH-SQMW)**2+SQMW*PMAS(24,2)**2)
        CAWZ=COUP(2,3)/SNGL(DT)-2.*XW1*COUP(1,2)*(SH-SQMW)*FACBW
        CBWZ=COUP(1,3)/SNGL(DU)+2.*XW1*COUP(1,2)*(SH-SQMW)*FACBW
        FGK135=ABS(CAWZ*FGK(1,2,3,4,5,6)+CBWZ*FGK(1,2,5,6,3,4))
        FGK136=ABS(CAWZ*FGK(1,2,3,4,6,5)+CBWZ*FGK(1,2,6,5,3,4))
        WT=(COUP(5,3)*FGK135)**2+(COUP(5,4)*FGK136)**2
        WTMAX=4.*D34*D56*(COUP(5,3)**2+COUP(5,4)**2)*(CAWZ**2*
     &  DIGK(DT,DU)+CBWZ**2*DIGK(DU,DT)+CAWZ*CBWZ*DJGK(DT,DU))
 
      ELSEIF(ISUB.EQ.24.OR.ISUB.EQ.171.OR.ISUB.EQ.176) THEN
C...Angular weight for f + f~ -> Z0 + H0 -> 2 quarks/leptons + H0
C...(or H'0, or A0).
        WT=((COUP(1,3)*COUP(3,3))**2+(COUP(1,4)*COUP(3,4))**2)*
     &  PKK(1,3)*PKK(2,4)+((COUP(1,3)*COUP(3,4))**2+(COUP(1,4)*
     &  COUP(3,3))**2)*PKK(1,4)*PKK(2,3)
        WTMAX=(COUP(1,3)**2+COUP(1,4)**2)*(COUP(3,3)**2+COUP(3,4)**2)*
     &  (PKK(1,3)+PKK(1,4))*(PKK(2,3)+PKK(2,4))
 
      ELSEIF(ISUB.EQ.25) THEN
C...Angular weight for f + f~ -> W+ + W- -> 4 quarks/leptons.
        D34=P(IREF(IP,IORD),5)**2
        D56=P(IREF(IP,3-IORD),5)**2
        DT=PKK(1,3)+PKK(1,4)+D34
        DU=PKK(1,5)+PKK(1,6)+D56
        FACBW=1./((SH-SQMZ)**2+SQMZ*PMAS(23,2)**2)
        CDWW=(COUP(1,3)*SQMZ*(SH-SQMZ)*FACBW+COUP(1,2))/SH
        CAWW=CDWW+0.5*(COUP(1,2)+1.)/SNGL(DT)
        CBWW=CDWW+0.5*(COUP(1,2)-1.)/SNGL(DU)
        CCWW=COUP(1,4)*SQMZ*(SH-SQMZ)*FACBW/SH
        FGK135=ABS(CAWW*FGK(1,2,3,4,5,6)-CBWW*FGK(1,2,5,6,3,4))
        FGK253=ABS(FGK(2,1,5,6,3,4)-FGK(2,1,3,4,5,6))
        WT=FGK135**2+(CCWW*FGK253)**2
        WTMAX=4.*D34*D56*(CAWW**2*DIGK(DT,DU)+CBWW**2*DIGK(DU,DT)-CAWW*
     &  CBWW*DJGK(DT,DU)+CCWW**2*(DIGK(DT,DU)+DIGK(DU,DT)-DJGK(DT,DU)))
 
      ELSEIF(ISUB.EQ.26.OR.ISUB.EQ.172.OR.ISUB.EQ.177) THEN
C...Angular weight for f + f~' -> W+/- + H0 -> 2 quarks/leptons + H0
C...(or H'0, or A0).
        WT=PKK(1,3)*PKK(2,4)
        WTMAX=(PKK(1,3)+PKK(1,4))*(PKK(2,3)+PKK(2,4))
 
      ELSEIF(ISUB.EQ.30.OR.ISUB.EQ.35) THEN
C...Angular weight for f + g/gamma -> f + (gamma*/Z0)
C...-> f + 2 quarks/leptons.
        CLILF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,3)*HGZ(2,2)*COUP(3,1)*COUP(3,3)/4.+
     &  COUP(1,3)**2*HGZ(2,3)*COUP(3,3)**2
        CLIRF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,3)*HGZ(2,2)*COUP(3,1)*COUP(3,4)/4.+
     &  COUP(1,3)**2*HGZ(2,3)*COUP(3,4)**2
        CRILF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,4)*HGZ(2,2)*COUP(3,1)*COUP(3,3)/4.+
     &  COUP(1,4)**2*HGZ(2,3)*COUP(3,3)**2
        CRIRF=COUP(1,1)**2*HGZ(2,1)*COUP(3,1)**2/16.+
     &  COUP(1,1)*COUP(1,4)*HGZ(2,2)*COUP(3,1)*COUP(3,4)/4.+
     &  COUP(1,4)**2*HGZ(2,3)*COUP(3,4)**2
        IF(K(ILIN(1),2).GT.0) WT=(CLILF+CRIRF)*(PKK(1,4)**2+
     &  PKK(3,5)**2)+(CLIRF+CRILF)*(PKK(1,3)**2+PKK(4,5)**2)
        IF(K(ILIN(1),2).LT.0) WT=(CLILF+CRIRF)*(PKK(1,3)**2+
     &  PKK(4,5)**2)+(CLIRF+CRILF)*(PKK(1,4)**2+PKK(3,5)**2)
        WTMAX=(CLILF+CLIRF+CRILF+CRIRF)*
     &  ((PKK(1,3)+PKK(1,4))**2+(PKK(3,5)+PKK(4,5))**2)
 
      ELSEIF(ISUB.EQ.31) THEN
C...Angular weight for f + g -> f' + W+/- -> f' + 2 quarks/leptons.
        IF(K(ILIN(1),2).GT.0) WT=PKK(1,4)**2+PKK(3,5)**2
        IF(K(ILIN(1),2).LT.0) WT=PKK(1,3)**2+PKK(4,5)**2
        WTMAX=(PKK(1,3)+PKK(1,4))**2+(PKK(3,5)+PKK(4,5))**2
 
      ELSEIF(ISUB.EQ.71.OR.ISUB.EQ.72.OR.ISUB.EQ.73.OR.ISUB.EQ.76.OR.
     &ISUB.EQ.77) THEN
C...Angular weight for V_L1 + V_L2 -> V_L3 + V_L4 (V = Z/W).
        WT=16.*PKK(3,5)*PKK(4,6)
        WTMAX=SH**2
 
      ELSEIF(ISUB.EQ.110) THEN
C...Angular weight for f + f~ -> gamma + H0 -> gamma + X is isotropic.
        WT=1.
        WTMAX=1.
 
      ELSEIF(ISUB.EQ.141) THEN
        IF(IP.EQ.1.AND.(KDCY(1).EQ.1.OR.KDCY(1).EQ.2)) THEN
C...Angular weight for f + f~ -> gamma*/Z0/Z'0 -> 2 quarks/leptons.
C...Couplings of incoming flavour.
          KFAI=IABS(MINT(15))
          EI=KCHG(KFAI,1)/3.
          AI=SIGN(1.,EI+0.1)
          VI=AI-4.*EI*XWV
          KFAIC=1
          IF(KFAI.LE.10.AND.MOD(KFAI,2).EQ.0) KFAIC=2
          IF(KFAI.GT.10.AND.MOD(KFAI,2).NE.0) KFAIC=3
          IF(KFAI.GT.10.AND.MOD(KFAI,2).EQ.0) KFAIC=4
          VPI=PARU(119+2*KFAIC)
          API=PARU(120+2*KFAIC)
C...Couplings of final flavour.
          KFAF=IABS(KFL1(1))
          EF=KCHG(KFAF,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
          KFAFC=1
          IF(KFAF.LE.10.AND.MOD(KFAF,2).EQ.0) KFAFC=2
          IF(KFAF.GT.10.AND.MOD(KFAF,2).NE.0) KFAFC=3
          IF(KFAF.GT.10.AND.MOD(KFAF,2).EQ.0) KFAFC=4
          VPF=PARU(119+2*KFAFC)
          APF=PARU(120+2*KFAFC)
C...Asymmetry and weight.
          ASYM=2.*(EI*AI*VINT(112)*EF*AF+EI*API*VINT(113)*EF*APF+
     &    4.*VI*AI*VINT(114)*VF*AF+(VI*API+VPI*AI)*VINT(115)*
     &    (VF*APF+VPF*AF)+4.*VPI*API*VINT(116)*VPF*APF)/
     &    (EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*EF*VF+
     &    EI*VPI*VINT(113)*EF*VPF+(VI**2+AI**2)*VINT(114)*
     &    (VF**2+AF**2)+(VI*VPI+AI*API)*VINT(115)*(VF*VPF+AF*APF)+
     &    (VPI**2+API**2)*VINT(116)*(VPF**2+APF**2))
          WT=1.+ASYM*CTHE(1)*ISIGN(1,MINT(15)*KFL1(1))+CTHE(1)**2
          WTMAX=2.+ABS(ASYM)
        ELSEIF(IP.EQ.1.AND.IABS(KFL1(1)).EQ.24) THEN
C...Angular weight for f + f~ -> Z' -> W+ + W-.
          RM1=P(NSD(1)+1,5)**2/SH
          RM2=P(NSD(1)+2,5)**2/SH
          CCOS2=-(1./16.)*((1.-RM1-RM2)**2-4.*RM1*RM2)*
     &    (1.-2.*RM1-2.*RM2+RM1**2+RM2**2+10.*RM1*RM2)
          CFLAT=-CCOS2+0.5*(RM1+RM2)*(1.-2.*RM1-2.*RM2+(RM2-RM1)**2)
          WT=CFLAT+CCOS2*CTHE(1)**2
          WTMAX=CFLAT+MAX(0.,CCOS2)
        ELSEIF(IP.EQ.1.AND.(KFL1(1).EQ.25.OR.KFL1(1).EQ.35.OR.
     &  IABS(KFL1(1)).EQ.37)) THEN
C...Angular weight for f + f~ -> Z' -> H0 + A0, H'0 + A0, H+ + H-.
          WT=1.-CTHE(1)**2
          WTMAX=1.
        ELSEIF(IP.EQ.1.AND.KDCY(1).EQ.3) THEN
C...Angular weight for f + f~ -> Z' -> Z0 + H0.
          RM1=P(NSD(1)+1,5)**2/SH
          RM2=P(NSD(1)+2,5)**2/SH
          FLAM2=MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2)
          WT=1.+FLAM2*(1.-CTHE(1)**2)/(8.*RM1)
          WTMAX=1.+FLAM2/(8.*RM1)
        ELSEIF(MZPWP.EQ.0) THEN
C...Angular weight for f + f~ -> Z' -> W+ + W- -> 4 quarks/leptons
C...(W:s like if intermediate Z).
          D34=P(IREF(IP,IORD),5)**2
          D56=P(IREF(IP,3-IORD),5)**2
          DT=PKK(1,3)+PKK(1,4)+D34
          DU=PKK(1,5)+PKK(1,6)+D56
          FGK135=ABS(FGK(1,2,3,4,5,6)-FGK(1,2,5,6,3,4))
          FGK253=ABS(FGK(2,1,5,6,3,4)-FGK(2,1,3,4,5,6))
          WT=(COUP(1,3)*FGK135)**2+(COUP(1,4)*FGK253)**2
          WTMAX=4.*D34*D56*(COUP(1,3)**2+COUP(1,4)**2)*
     &    (DIGK(DT,DU)+DIGK(DU,DT)-DJGK(DT,DU))
        ELSEIF(MZPWP.EQ.1) THEN
C...Angular weight for f + f~ -> Z' -> W+ + W- -> 4 quarks/leptons
C...(W:s approximately longitudinal, like if intermediate H).
          WT=16.*PKK(3,5)*PKK(4,6)
          WTMAX=SH**2
        ELSE
C...Angular weight for f + f~ -> Z' -> H+ + H-, Z0 + H0, H0 + A0,
C...H'0 + A0 -> 4 quarks/leptons.
          WT=1.
          WTMAX=1.
        ENDIF
 
      ELSEIF(ISUB.EQ.142) THEN
        IF(IP.EQ.1.AND.(KDCY(1).EQ.1.OR.KDCY(1).EQ.2)) THEN
C...Angular weight for f + f~' -> W'+/- -> 2 quarks/leptons.
          KFAI=IABS(MINT(15))
          KFAIC=1
          IF(KFAI.GT.10) KFAIC=2
          VI=PARU(129+2*KFAIC)
          AI=PARU(130+2*KFAIC)
          KFAF=IABS(KFL1(1))
          KFAFC=1
          IF(KFAF.GT.10) KFAFC=2
          VF=PARU(129+2*KFAFC)
          AF=PARU(130+2*KFAFC)
          ASYM=8.*VI*AI*VF*AF/((VI**2+AI**2)*(VF**2+AF**2))
          WT=1.+ASYM*CTHE(1)*ISIGN(1,MINT(15)*KFL1(1))+CTHE(1)**2
          WTMAX=2.+ABS(ASYM)
        ELSEIF(IP.EQ.1.AND.IABS(KFL2(1)).EQ.23) THEN
C...Angular weight for f + f~' -> W'+/- -> W+/- + Z0.
          RM1=P(NSD(1)+1,5)**2/SH
          RM2=P(NSD(1)+2,5)**2/SH
          CCOS2=-(1./16.)*((1.-RM1-RM2)**2-4.*RM1*RM2)*
     &    (1.-2.*RM1-2.*RM2+RM1**2+RM2**2+10.*RM1*RM2)
          CFLAT=-CCOS2+0.5*(RM1+RM2)*(1.-2.*RM1-2.*RM2+(RM2-RM1)**2)
          WT=CFLAT+CCOS2*CTHE(1)**2
          WTMAX=CFLAT+MAX(0.,CCOS2)
        ELSEIF(IP.EQ.1.AND.KDCY(1).EQ.3) THEN
C...Angular weight for f + f~ -> W'+/- -> W+/- + H0.
          RM1=P(NSD(1)+1,5)**2/SH
          RM2=P(NSD(1)+2,5)**2/SH
          FLAM2=MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2)
          WT=1.+FLAM2*(1.-CTHE(1)**2)/(8.*RM1)
          WTMAX=1.+FLAM2/(8.*RM1)
        ELSEIF(MZPWP.EQ.0) THEN
C...Angular weight for f + f~' -> W' -> W + Z0 -> 4 quarks/leptons
C...(W/Z like if intermediate W).
          D34=P(IREF(IP,IORD),5)**2
          D56=P(IREF(IP,3-IORD),5)**2
          DT=PKK(1,3)+PKK(1,4)+D34
          DU=PKK(1,5)+PKK(1,6)+D56
          FGK135=ABS(FGK(1,2,3,4,5,6)-FGK(1,2,5,6,3,4))
          FGK136=ABS(FGK(1,2,3,4,6,5)-FGK(1,2,6,5,3,4))
          WT=(COUP(5,3)*FGK135)**2+(COUP(5,4)*FGK136)**2
          WTMAX=4.*D34*D56*(COUP(5,3)**2+COUP(5,4)**2)*
     &    (DIGK(DT,DU)+DIGK(DU,DT)-DJGK(DT,DU))
        ELSEIF(MZPWP.EQ.1) THEN
C...Angular weight for f + f~' -> W' -> W + Z0 -> 4 quarks/leptons
C...(W/Z approximately longitudinal, like if intermediate H).
          WT=16.*PKK(3,5)*PKK(4,6)
          WTMAX=SH**2
        ELSE
C...Angular weight for f + f~ -> W' -> W + H0 -> whatever.
          WT=1.
          WTMAX=1.
        ENDIF
 
      ELSEIF(ISUB.EQ.145.OR.ISUB.EQ.162.OR.ISUB.EQ.163.OR.ISUB.EQ.164)
     &THEN
C...Isotropic decay of leptoquarks (assumed spin 0).
        WT=1.
        WTMAX=1.
 
      ELSEIF(ISUB.EQ.147.OR.ISUB.EQ.148) THEN
C...Decays of (spin 1/2) q* -> q + (g,gamma) or (Z0,W+-).
        SIDE=1.
        IF(MINT(16).EQ.21) SIDE=-1.
        IF(IP.EQ.1.AND.(KFL1(1).EQ.21.OR.KFL1(1).EQ.22)) THEN
          WT=1.+SIDE*CTHE(1)
          WTMAX=2.
        ELSEIF(IP.EQ.1) THEN
          RM1=P(NSD(1)+1,5)**2/SH
          WT=1.+SIDE*CTHE(1)*(1.-0.5*RM1)/(1.+0.5*RM1)
          WTMAX=1.+(1.-0.5*RM1)/(1.+0.5*RM1)
        ELSE
C...W/Z decay assumed isotropic, since not known.
          WT=1.
          WTMAX=1.
        ENDIF
 
      ELSEIF(ISUB.EQ.149) THEN
C...Isotropic decay of techni-eta.
        WT=1.
        WTMAX=1.
 
C...Obtain correct angular distribution by rejection techniques.
      ELSE
        WT=1.
        WTMAX=1.
      ENDIF
      IF(WT.LT.RLU(0)*WTMAX) GOTO 240
 
C...Construct massive four-vectors using angles chosen. Mark decayed
C...resonances, add documentation lines. Shower evolution.
  400 DO 470 JT=1,JTMAX
      IF(KDCY(JT).EQ.0) GOTO 470
      ID=IREF(IP,JT)
      IF(ISET(ISUB).NE.6.OR.JT.NE.3) THEN
        DO 410 J=1,5
        DPMO(J)=P(ID,J)
  410   CONTINUE
        DPMO(4)=SQRT(DPMO(1)**2+DPMO(2)**2+DPMO(3)**2+DPMO(5)**2)
        CALL LUDBRB(NSD(JT)+1,NSD(JT)+2,ACOS(CTHE(JT)),PHI(JT),
     &  DPMO(1)/DPMO(4),DPMO(2)/DPMO(4),DPMO(3)/DPMO(4))
      ELSE
C...Z + q + q~ : angles already known, in rest frame of system.
        DO 420 J=1,3
        DBEZQQ(J)=(P(ID,J)+P(ID+1,J)+P(ID+2,J))/(P(ID,4)+P(ID+1,4)+
     &  P(ID+2,4))
  420   CONTINUE
        K(N+1,1)=1
        DO 430 J=1,5
        P(N+1,J)=P(ID,J)
  430   CONTINUE
        CALL LUDBRB(N+1,N+1,0.,0.,-DBEZQQ(1),-DBEZQQ(2),-DBEZQQ(3))
        PHIZQQ=ULANGL(P(N+1,1),P(N+1,2))
        THEZQQ=ULANGL(P(N+1,3),SQRT(P(N+1,1)**2+P(N+1,2)**2))
        CALL LUDBRB(NSD(JT)+1,NSD(JT)+2,ACOS(VINT(81)),VINT(82),
     &  0D0,0D0,DBLE(SQRT(P(N+1,1)**2+P(N+1,2)**2+P(N+1,3)**2)/
     &  P(N+1,4)))
        CALL LUDBRB(NSD(JT)+1,NSD(JT)+2,THEZQQ,PHIZQQ,DBEZQQ(1),
     &  DBEZQQ(2),DBEZQQ(3))
      ENDIF
      K(ID,1)=K(ID,1)+10
      KFA=IABS(K(ID,2))
      IF(KFA.EQ.38.OR.KFA.EQ.39.OR.((MSTP(6).EQ.1.OR.MSTP(49).GE.1).AND.
     &(KFA.EQ.7.OR.KFA.EQ.8)).OR.(MSTP(48).GE.1.AND.KFA.EQ.6)) THEN
C...Do not kill colour flow through techni-eta, t, leptoquark or q*!
      ELSE
        K(ID,4)=NSD(JT)+1
        K(ID,5)=NSD(JT)+2
      ENDIF
      IDOC=MINT(83)+MINT(4)
      DO 450 I=NSD(JT)+1,NSD(JT)+2
      I1=MINT(83)+MINT(4)+1
      K(I,3)=I1
      IF(MSTP(128).GE.1) K(I,3)=ID
      IF(MSTP(128).LE.1.AND.MINT(4).LT.MSTP(126)) THEN
        MINT(4)=MINT(4)+1
        K(I1,1)=21
        K(I1,2)=K(I,2)
        K(I1,3)=IREF(IP,JT+3)
        DO 440 J=1,5
        P(I1,J)=P(I,J)
  440   CONTINUE
      ENDIF
  450 CONTINUE
C...Shower - top currently special case.
      NSHBEF=N
      IF(MSTP(71).GE.1.AND.(KDCY(JT).LE.2.OR.KFA.EQ.6.OR.KFA.EQ.7.OR.
     &KFA.EQ.8)) CALL LUSHOW(NSD(JT)+1,NSD(JT)+2,P(ID,5))
      NSHAFT=N
 
C...Check if new resonances were produced.
      KNSDA1=IABS(K(NSD(JT)+1,2))
      KNSDA2=IABS(K(NSD(JT)+2,2))
      IF(KNSDA1.EQ.6.OR.KNSDA2.EQ.6.OR.KNSDA1.EQ.7.OR.KNSDA2.EQ.7.OR.
     &KNSDA1.EQ.8.OR.KNSDA2.EQ.8) THEN
        NSD1=0
        NSD2=0
        DO 460 I=NSD(JT)+1,N
        IF(K(I,1).LT.10.AND.K(I,2).EQ.K(NSD(JT)+1,2)) NSD1=I
        IF(K(I,1).LT.10.AND.K(I,2).EQ.K(NSD(JT)+2,2)) NSD2=I
  460   CONTINUE
        IF(NSD1.NE.0.AND.NSD2.NE.0) THEN
          NP=NP+1
          IREF(NP,1)=NSD1
          IREF(NP,2)=NSD2
          IREF(NP,3)=0
          IREF(NP,4)=IDOC+1
          IREF(NP,5)=IDOC+2
          IREF(NP,6)=0
          IREF(NP,7)=K(IREF(IP,JT),2)
          IREF(NP,8)=IREF(IP,JT)
        ENDIF
      ELSEIF(KDCY(JT).EQ.3.OR.KFA.EQ.6.OR.KFA.EQ.7.OR.KFA.EQ.8) THEN
        NP=NP+1
        IREF(NP,1)=NSD(JT)+1
        IREF(NP,2)=NSD(JT)+2
        IF(NSHAFT-NSHBEF.GT.0) THEN
          IREF(NP,1)=NSHBEF+2
          IREF(NP,2)=NSHBEF+3
        ENDIF
        IREF(NP,3)=0
        IREF(NP,4)=IDOC+1
        IREF(NP,5)=IDOC+2
        IREF(NP,6)=0
        IREF(NP,7)=K(IREF(IP,JT),2)
        IREF(NP,8)=IREF(IP,JT)
      ENDIF
  470 CONTINUE
 
C...Fill information for 2 -> 1 -> 2. Loop back if needed.
      IF(JTMAX.EQ.1.AND.KDCY(1).NE.0) THEN
        MINT(7)=MINT(83)+6+2*ISET(ISUB)
        MINT(8)=MINT(83)+7+2*ISET(ISUB)
        MINT(25)=KFL1(1)
        MINT(26)=KFL2(1)
        VINT(23)=CTHE(1)
        RM3=P(N-1,5)**2/SH
        RM4=P(N,5)**2/SH
        BE34=SQRT(MAX(0.,(1.-RM3-RM4)**2-4.*RM3*RM4))
        VINT(45)=-0.5*SH*(1.-RM3-RM4-BE34*CTHE(1))
        VINT(46)=-0.5*SH*(1.-RM3-RM4+BE34*CTHE(1))
        VINT(48)=0.25*SH*BE34**2*MAX(0.,1.-CTHE(1)**2)
        VINT(47)=SQRT(VINT(48))
      ENDIF
  480 IF(IP.LT.NP) GOTO 130
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYMULT(MMUL)
 
C...Initializes treatment of multiple interactions, selects kinematics
C...of hardest interaction if low-pT physics included in run, and
C...generates all non-hardest interactions.
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
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      REAL  SIGT
      SAVE /PYINT7/
      DIMENSION NMUL(20),SIGM(20),KSTR(500,2),VINTSV(80)
      SAVE XT2,XT2FAC,XC2,XTS,IRBIN,RBIN,NMUL,SIGM
C...Initialization of multiple interaction treatment.
      IF(MMUL.EQ.1) THEN
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5000) MSTP(82)
        ISUB=96
        MINT(1)=96
        VINT(63)=0.
        VINT(64)=0.
        VINT(143)=1.
        VINT(144)=1.
 
C...Loop over phase space points: xT2 choice in 20 bins.
  100   SIGSUM=0.
        DO 120 IXT2=1,20
        NMUL(IXT2)=MSTP(83)
        SIGM(IXT2)=0.
        DO 110 ITRY=1,MSTP(83)
        RSCA=0.05*((21-IXT2)-RLU(0))
        XT2=VINT(149)*(1.+VINT(149))/(VINT(149)+RSCA)-VINT(149)
        XT2=MAX(0.01*VINT(149),XT2)
        VINT(25)=XT2
 
C...Choose tau and y*. Calculate cos(theta-hat).
        IF(RLU(0).LE.COEF(ISUB,1)) THEN
          TAUT=(2.*(1.+SQRT(1.-XT2))/XT2-1.)**RLU(0)
          TAU=XT2*(1.+TAUT)**2/(4.*TAUT)
        ELSE
          TAU=XT2*(1.+TAN(RLU(0)*ATAN(SQRT(1./XT2-1.)))**2)
        ENDIF
        VINT(21)=TAU
        CALL PYKLIM(2)
        RYST=RLU(0)
        MYST=1
        IF(RYST.GT.COEF(ISUB,8)) MYST=2
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)) MYST=3
        CALL PYKMAP(2,MYST,RLU(0))
        VINT(23)=SQRT(MAX(0.,1.-XT2/TAU))*(-1)**INT(1.5+RLU(0))
 
C...Calculate differential cross-section.
        VINT(71)=0.5*VINT(1)*SQRT(XT2)
        CALL PYSIGH(NCHN,SIGS)
        SIGM(IXT2)=SIGM(IXT2)+SIGS
  110   CONTINUE
        SIGSUM=SIGSUM+SIGM(IXT2)
  120   CONTINUE
        SIGSUM=SIGSUM/(20.*MSTP(83))
 
C...Reject result if sigma(parton-parton) is smaller than hadronic one.
        IF(SIGSUM.LT.1.1*SIGT(0,0,5)) THEN
          IF(MSTP(122).GE.1) WRITE(MSTU(11),5100) PARP(82),SIGSUM
          PARP(82)=0.9*PARP(82)
          VINT(149)=4.*PARP(82)**2/VINT(2)
          GOTO 100
        ENDIF
        IF(MSTP(122).GE.1) WRITE(MSTU(11),5200) PARP(82), SIGSUM
 
C...Start iteration to find k factor.
        YKE=SIGSUM/SIGT(0,0,5)
        SO=0.5
        XI=0.
        YI=0.
        XF=0.
        YF=0.
        XK=0.5
        IIT=0
  130   IF(IIT.EQ.0) THEN
          XK=2.*XK
        ELSEIF(IIT.EQ.1) THEN
          XK=0.5*XK
        ELSE
          XK=XI+(YKE-YI)*(XF-XI)/(YF-YI)
        ENDIF
 
C...Evaluate overlap integrals.
        IF(MSTP(82).EQ.2) THEN
          SP=0.5*PARU(1)*(1.-EXP(-XK))
          SOP=SP/PARU(1)
        ELSE
          IF(MSTP(82).EQ.3) DELTAB=0.02
          IF(MSTP(82).EQ.4) DELTAB=MIN(0.01,0.05*PARP(84))
          SP=0.
          SOP=0.
          B=-0.5*DELTAB
  140     B=B+DELTAB
          IF(MSTP(82).EQ.3) THEN
            OV=EXP(-B**2)/PARU(2)
          ELSE
            CQ2=PARP(84)**2
            OV=((1.-PARP(83))**2*EXP(-MIN(50.,B**2))+2.*PARP(83)*
     &      (1.-PARP(83))*2./(1.+CQ2)*EXP(-MIN(50.,B**2*2./(1.+CQ2)))+
     &      PARP(83)**2/CQ2*EXP(-MIN(50.,B**2/CQ2)))/PARU(2)
          ENDIF
          PACC=1.-EXP(-MIN(50.,PARU(1)*XK*OV))
          SP=SP+PARU(2)*B*DELTAB*PACC
          SOP=SOP+PARU(2)*B*DELTAB*OV*PACC
          IF(B.LT.1..OR.B*PACC.GT.1E-6) GOTO 140
        ENDIF
        YK=PARU(1)*XK*SO/SP
 
C...Continue iteration until convergence.
        IF(YK.LT.YKE) THEN
          XI=XK
          YI=YK
          IF(IIT.EQ.1) IIT=2
        ELSE
          XF=XK
          YF=YK
          IF(IIT.EQ.0) IIT=1
        ENDIF
        IF(ABS(YK-YKE).GE.1E-5*YKE) GOTO 130
 
C...Store some results for subsequent use.
        VINT(145)=SIGSUM
        VINT(146)=SOP/SO
        VINT(147)=SOP/SP
 
C...Initialize iteration in xT2 for hardest interaction.
      ELSEIF(MMUL.EQ.2) THEN
        IF(MSTP(82).LE.0) THEN
        ELSEIF(MSTP(82).EQ.1) THEN
          XT2=1.
          XT2FAC=XSEC(96,1)/SIGT(0,0,5)*VINT(149)/(1.-VINT(149))
        ELSEIF(MSTP(82).EQ.2) THEN
          XT2=1.
          XT2FAC=VINT(146)*XSEC(96,1)/SIGT(0,0,5)*VINT(149)*
     &    (1.+VINT(149))
        ELSE
          XC2=4.*CKIN(3)**2/VINT(2)
          IF(CKIN(3).LE.CKIN(5).OR.MINT(82).GE.2) XC2=0.
        ENDIF
 
      ELSEIF(MMUL.EQ.3) THEN
C...Low-pT or multiple interactions (first semihard interaction):
C...choose xT2 according to dpT2/pT2**2*exp(-(sigma above pT2)/norm)
C...or (MSTP(82)>=2) dpT2/(pT2+pT0**2)**2*exp(-....).
        ISUB=MINT(1)
        IF(MSTP(82).LE.0) THEN
          XT2=0.
        ELSEIF(MSTP(82).EQ.1) THEN
          XT2=XT2FAC*XT2/(XT2FAC-XT2*LOG(RLU(0)))
        ELSEIF(MSTP(82).EQ.2) THEN
          IF(XT2.LT.1..AND.EXP(-XT2FAC*XT2/(VINT(149)*(XT2+
     &    VINT(149)))).GT.RLU(0)) XT2=1.
          IF(XT2.GE.1.) THEN
            XT2=(1.+VINT(149))*XT2FAC/(XT2FAC-(1.+VINT(149))*LOG(1.-
     &      RLU(0)*(1.-EXP(-XT2FAC/(VINT(149)*(1.+VINT(149)))))))-
     &      VINT(149)
          ELSE
            XT2=-XT2FAC/LOG(EXP(-XT2FAC/(XT2+VINT(149)))+RLU(0)*
     &      (EXP(-XT2FAC/VINT(149))-EXP(-XT2FAC/(XT2+VINT(149)))))-
     &      VINT(149)
          ENDIF
          XT2=MAX(0.01*VINT(149),XT2)
        ELSE
          XT2=(XC2+VINT(149))*(1.+VINT(149))/(1.+VINT(149)-
     &    RLU(0)*(1.-XC2))-VINT(149)
          XT2=MAX(0.01*VINT(149),XT2)
        ENDIF
        VINT(25)=XT2
 
C...Low-pT: choose xT2, tau, y* and cos(theta-hat) fixed.
        IF(MSTP(82).LE.1.AND.XT2.LT.VINT(149)) THEN
          IF(MINT(82).EQ.1) NGEN(0,1)=NGEN(0,1)-1
          IF(MINT(82).EQ.1) NGEN(ISUB,1)=NGEN(ISUB,1)-1
          ISUB=95
          MINT(1)=ISUB
          VINT(21)=0.01*VINT(149)
          VINT(22)=0.
          VINT(23)=0.
          VINT(25)=0.01*VINT(149)
 
        ELSE
C...Multiple interactions (first semihard interaction).
C...Choose tau and y*. Calculate cos(theta-hat).
          IF(RLU(0).LE.COEF(ISUB,1)) THEN
            TAUT=(2.*(1.+SQRT(1.-XT2))/XT2-1.)**RLU(0)
            TAU=XT2*(1.+TAUT)**2/(4.*TAUT)
          ELSE
            TAU=XT2*(1.+TAN(RLU(0)*ATAN(SQRT(1./XT2-1.)))**2)
          ENDIF
          VINT(21)=TAU
          CALL PYKLIM(2)
          RYST=RLU(0)
          MYST=1
          IF(RYST.GT.COEF(ISUB,8)) MYST=2
          IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)) MYST=3
          CALL PYKMAP(2,MYST,RLU(0))
          VINT(23)=SQRT(MAX(0.,1.-XT2/TAU))*(-1)**INT(1.5+RLU(0))
        ENDIF
        VINT(71)=0.5*VINT(1)*SQRT(VINT(25))
 
C...Store results of cross-section calculation.
      ELSEIF(MMUL.EQ.4) THEN
        ISUB=MINT(1)
        XTS=VINT(25)
        IF(ISET(ISUB).EQ.1) XTS=VINT(21)
        IF(ISET(ISUB).EQ.2.OR.ISET(ISUB).EQ.6)
     &  XTS=(4.*VINT(48)+2.*VINT(63)+2.*VINT(64))/VINT(2)
        IF(ISET(ISUB).GE.3.AND.ISET(ISUB).LE.5) XTS=VINT(26)
        RBIN=MAX(0.000001,MIN(0.999999,XTS*(1.+VINT(149))/
     &  (XTS+VINT(149))))
        IRBIN=INT(1.+20.*RBIN)
        IF(ISUB.EQ.96.AND.MSTP(171).EQ.0) THEN
          NMUL(IRBIN)=NMUL(IRBIN)+1
          SIGM(IRBIN)=SIGM(IRBIN)+VINT(153)
        ENDIF
 
C...Choose impact parameter.
      ELSEIF(MMUL.EQ.5) THEN
        IF(MSTP(82).EQ.3) THEN
          VINT(148)=RLU(0)/(PARU(2)*VINT(147))
        ELSE
          RTYPE=RLU(0)
          CQ2=PARP(84)**2
          IF(RTYPE.LT.(1.-PARP(83))**2) THEN
            B2=-LOG(RLU(0))
          ELSEIF(RTYPE.LT.1.-PARP(83)**2) THEN
            B2=-0.5*(1.+CQ2)*LOG(RLU(0))
          ELSE
            B2=-CQ2*LOG(RLU(0))
          ENDIF
          VINT(148)=((1.-PARP(83))**2*EXP(-MIN(50.,B2))+2.*PARP(83)*
     &    (1.-PARP(83))*2./(1.+CQ2)*EXP(-MIN(50.,B2*2./(1.+CQ2)))+
     &    PARP(83)**2/CQ2*EXP(-MIN(50.,B2/CQ2)))/(PARU(2)*VINT(147))
        ENDIF
 
C...Multiple interactions (variable impact parameter) : reject with
C...probability exp(-overlap*cross-section above pT/normalization).
        RNCOR=(IRBIN-20.*RBIN)*NMUL(IRBIN)
        SIGCOR=(IRBIN-20.*RBIN)*SIGM(IRBIN)
        DO 150 IBIN=IRBIN+1,20
        RNCOR=RNCOR+NMUL(IBIN)
        SIGCOR=SIGCOR+SIGM(IBIN)
  150   CONTINUE
        SIGABV=(SIGCOR/RNCOR)*VINT(149)*(1.-XTS)/(XTS+VINT(149))
        IF(MSTP(171).EQ.1) SIGABV=SIGABV*VINT(2)/VINT(289)
        VINT(150)=EXP(-MIN(50.,VINT(146)*VINT(148)*
     &  SIGABV/SIGT(0,0,5)))
 
C...Generate additional multiple semihard interactions.
      ELSEIF(MMUL.EQ.6) THEN
        ISUBSV=MINT(1)
        DO 160 J=11,80
        VINTSV(J)=VINT(J)
  160   CONTINUE
        ISUB=96
        MINT(1)=96
 
C...Reconstruct strings in hard scattering.
        NMAX=MINT(84)+4
        IF(ISET(ISUBSV).EQ.1) NMAX=MINT(84)+2
        IF(ISET(ISUBSV).EQ.11) NMAX=MINT(84)+2+MINT(3)
        NSTR=0
        DO 180 I=MINT(84)+1,NMAX
        KCS=KCHG(LUCOMP(K(I,2)),2)*ISIGN(1,K(I,2))
        IF(KCS.EQ.0) GOTO 180
 
        DO 170 J=1,4
        IF(KCS.EQ.1.AND.(J.EQ.2.OR.J.EQ.4)) GOTO 170
        IF(KCS.EQ.-1.AND.(J.EQ.1.OR.J.EQ.3)) GOTO 170
        IF(J.LE.2) THEN
          IST=MOD(K(I,J+3)/MSTU(5),MSTU(5))
        ELSE
          IST=MOD(K(I,J+1),MSTU(5))
        ENDIF
        IF(IST.LT.MINT(84).OR.IST.GT.I) GOTO 170
        IF(KCHG(LUCOMP(K(IST,2)),2).EQ.0) GOTO 170
        NSTR=NSTR+1
        IF(J.EQ.1.OR.J.EQ.4) THEN
          KSTR(NSTR,1)=I
          KSTR(NSTR,2)=IST
        ELSE
          KSTR(NSTR,1)=IST
          KSTR(NSTR,2)=I
        ENDIF
  170   CONTINUE
  180   CONTINUE
 
C...Set up starting values for iteration in xT2.
        XT2=VINT(25)
        IF(ISET(ISUBSV).EQ.1) XT2=VINT(21)
        IF(ISET(ISUBSV).EQ.2.OR.ISET(ISUBSV).EQ.6)
     &  XT2=(4.*VINT(48)+2.*VINT(63)+2.*VINT(64))/VINT(2)
        IF(ISET(ISUBSV).GE.3.AND.ISET(ISUBSV).LE.5) XT2=VINT(26)
        IF(MSTP(82).LE.1) THEN
          XT2FAC=XSEC(ISUB,1)*VINT(149)/((1.-VINT(149))*SIGT(0,0,5))
        ELSE
          XT2FAC=VINT(146)*VINT(148)*XSEC(ISUB,1)/SIGT(0,0,5)*
     &    VINT(149)*(1.+VINT(149))
        ENDIF
        VINT(63)=0.
        VINT(64)=0.
        VINT(143)=1.-VINT(141)
        VINT(144)=1.-VINT(142)
 
C...Iterate downwards in xT2.
  190   IF(MSTP(82).LE.1) THEN
          XT2=XT2FAC*XT2/(XT2FAC-XT2*LOG(RLU(0)))
          IF(XT2.LT.VINT(149)) GOTO 240
        ELSE
          IF(XT2.LE.0.01*VINT(149)) GOTO 240
          XT2=XT2FAC*(XT2+VINT(149))/(XT2FAC-(XT2+VINT(149))*
     &    LOG(RLU(0)))-VINT(149)
          IF(XT2.LE.0.) GOTO 240
          XT2=MAX(0.01*VINT(149),XT2)
        ENDIF
        VINT(25)=XT2
 
C...Choose tau and y*. Calculate cos(theta-hat).
        IF(RLU(0).LE.COEF(ISUB,1)) THEN
          TAUT=(2.*(1.+SQRT(1.-XT2))/XT2-1.)**RLU(0)
          TAU=XT2*(1.+TAUT)**2/(4.*TAUT)
        ELSE
          TAU=XT2*(1.+TAN(RLU(0)*ATAN(SQRT(1./XT2-1.)))**2)
        ENDIF
        VINT(21)=TAU
        CALL PYKLIM(2)
        RYST=RLU(0)
        MYST=1
        IF(RYST.GT.COEF(ISUB,8)) MYST=2
        IF(RYST.GT.COEF(ISUB,8)+COEF(ISUB,9)) MYST=3
        CALL PYKMAP(2,MYST,RLU(0))
        VINT(23)=SQRT(MAX(0.,1.-XT2/TAU))*(-1)**INT(1.5+RLU(0))
 
C...Check that x not used up. Accept or reject kinematical variables.
        X1M=SQRT(TAU)*EXP(VINT(22))
        X2M=SQRT(TAU)*EXP(-VINT(22))
        IF(VINT(143)-X1M.LT.0.01.OR.VINT(144)-X2M.LT.0.01) GOTO 190
        VINT(71)=0.5*VINT(1)*SQRT(XT2)
        CALL PYSIGH(NCHN,SIGS)
        IF(SIGS.LT.XSEC(ISUB,1)*RLU(0)) GOTO 190
 
C...Reset K, P and V vectors. Select some variables.
        DO 210 I=N+1,N+2
        DO 200 J=1,5
        K(I,J)=0
        P(I,J)=0.
        V(I,J)=0.
  200   CONTINUE
  210   CONTINUE
        RFLAV=RLU(0)
        PT=0.5*VINT(1)*SQRT(XT2)
        PHI=PARU(2)*RLU(0)
        CTH=VINT(23)
 
C...Add first parton to event record.
        K(N+1,1)=3
        K(N+1,2)=21
        IF(RFLAV.GE.MAX(PARP(85),PARP(86))) K(N+1,2)=
     &  1+INT((2.+PARJ(2))*RLU(0))
        P(N+1,1)=PT*COS(PHI)
        P(N+1,2)=PT*SIN(PHI)
        P(N+1,3)=0.25*VINT(1)*(VINT(41)*(1.+CTH)-VINT(42)*(1.-CTH))
        P(N+1,4)=0.25*VINT(1)*(VINT(41)*(1.+CTH)+VINT(42)*(1.-CTH))
        P(N+1,5)=0.
 
C...Add second parton to event record.
        K(N+2,1)=3
        K(N+2,2)=21
        IF(K(N+1,2).NE.21) K(N+2,2)=-K(N+1,2)
        P(N+2,1)=-P(N+1,1)
        P(N+2,2)=-P(N+1,2)
        P(N+2,3)=0.25*VINT(1)*(VINT(41)*(1.-CTH)-VINT(42)*(1.+CTH))
        P(N+2,4)=0.25*VINT(1)*(VINT(41)*(1.-CTH)+VINT(42)*(1.+CTH))
        P(N+2,5)=0.
 
        IF(RFLAV.LT.PARP(85).AND.NSTR.GE.1) THEN
C....Choose relevant string pieces to place gluons on.
          DO 230 I=N+1,N+2
          DMIN=1E8
          DO 220 ISTR=1,NSTR
          I1=KSTR(ISTR,1)
          I2=KSTR(ISTR,2)
          DIST=(P(I,4)*P(I1,4)-P(I,1)*P(I1,1)-P(I,2)*P(I1,2)-
     &    P(I,3)*P(I1,3))*(P(I,4)*P(I2,4)-P(I,1)*P(I2,1)-
     &    P(I,2)*P(I2,2)-P(I,3)*P(I2,3))/MAX(1.,P(I1,4)*P(I2,4)-
     &    P(I1,1)*P(I2,1)-P(I1,2)*P(I2,2)-P(I1,3)*P(I2,3))
          IF(ISTR.EQ.1.OR.DIST.LT.DMIN) THEN
            DMIN=DIST
            IST1=I1
            IST2=I2
            ISTM=ISTR
          ENDIF
  220     CONTINUE
 
C....Colour flow adjustments, new string pieces.
          IF(K(IST1,4)/MSTU(5).EQ.IST2) K(IST1,4)=MSTU(5)*I+
     &    MOD(K(IST1,4),MSTU(5))
          IF(MOD(K(IST1,5),MSTU(5)).EQ.IST2) K(IST1,5)=
     &    MSTU(5)*(K(IST1,5)/MSTU(5))+I
          K(I,5)=MSTU(5)*IST1
          K(I,4)=MSTU(5)*IST2
          IF(K(IST2,5)/MSTU(5).EQ.IST1) K(IST2,5)=MSTU(5)*I+
     &    MOD(K(IST2,5),MSTU(5))
          IF(MOD(K(IST2,4),MSTU(5)).EQ.IST1) K(IST2,4)=
     &    MSTU(5)*(K(IST2,4)/MSTU(5))+I
          KSTR(ISTM,2)=I
          KSTR(NSTR+1,1)=I
          KSTR(NSTR+1,2)=IST2
          NSTR=NSTR+1
  230     CONTINUE
 
C...String drawing and colour flow for gluon loop.
        ELSEIF(K(N+1,2).EQ.21) THEN
          K(N+1,4)=MSTU(5)*(N+2)
          K(N+1,5)=MSTU(5)*(N+2)
          K(N+2,4)=MSTU(5)*(N+1)
          K(N+2,5)=MSTU(5)*(N+1)
          KSTR(NSTR+1,1)=N+1
          KSTR(NSTR+1,2)=N+2
          KSTR(NSTR+2,1)=N+2
          KSTR(NSTR+2,2)=N+1
          NSTR=NSTR+2
 
C...String drawing and colour flow for qq~ pair.
        ELSE
          K(N+1,4)=MSTU(5)*(N+2)
          K(N+2,5)=MSTU(5)*(N+1)
          KSTR(NSTR+1,1)=N+1
          KSTR(NSTR+1,2)=N+2
          NSTR=NSTR+1
        ENDIF
 
C...Update remaining energy; iterate.
        N=N+2
        IF(N.GT.MSTU(4)-MSTU(32)-10) THEN
          CALL LUERRM(11,'(PYMULT:) no more memory left in LUJETS')
          IF(MSTU(21).GE.1) RETURN
        ENDIF
        MINT(31)=MINT(31)+1
        VINT(151)=VINT(151)+VINT(41)
        VINT(152)=VINT(152)+VINT(42)
        VINT(143)=VINT(143)-VINT(41)
        VINT(144)=VINT(144)-VINT(42)
        IF(MINT(31).LT.240) GOTO 190
  240   CONTINUE
        MINT(1)=ISUBSV
        DO 250 J=11,80
        VINT(J)=VINTSV(J)
  250   CONTINUE
      ENDIF
 
C...Format statements for printout.
 5000 FORMAT(/1X,'****** PYMULT: initialization of multiple inter',
     &'actions for MSTP(82) =',I2,' ******')
 5100 FORMAT(8X,'pT0 =',F5.2,' GeV gives sigma(parton-parton) =',1P,
     &E9.2,' mb: rejected')
 5200 FORMAT(8X,'pT0 =',F5.2,' GeV gives sigma(parton-parton) =',1P,
     &E9.2,' mb: accepted')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYREMN(IPU1,IPU2)
 
C...Adds on target remnants (one or two from each side) and
C...includes primordial kT for hadron beams.
      IMPLICIT DOUBLE PRECISION(D)
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION KFLCH(2),KFLSP(2),CHI(2),PMS(0:6),IS(2),ISN(2),ROBO(5),
     &PSYS(0:2,5),PMIN(0:2),QOLD(4),QNEW(4),DBE(3),PSUM(4)
 
C...Find event type and remaining energy.
      ISUB=MINT(1)
      NS=N
      IF(MINT(50).EQ.0.OR.MSTP(81).LE.0) THEN
        VINT(143)=1.-VINT(141)
        VINT(144)=1.-VINT(142)
      ENDIF
 
C...Define initial partons.
      NTRY=0
  100 NTRY=NTRY+1
      DO 130 JT=1,2
      I=MINT(83)+JT+2
      IF(JT.EQ.1) IPU=IPU1
      IF(JT.EQ.2) IPU=IPU2
      K(I,1)=21
      K(I,2)=K(IPU,2)
      K(I,3)=I-2
      PMS(JT)=0.
      VINT(156+JT)=0.
      VINT(158+JT)=0.
      IF(MINT(47).EQ.1) THEN
        DO 110 J=1,5
        P(I,J)=P(I-2,J)
  110   CONTINUE
      ELSEIF(ISUB.EQ.95) THEN
        K(I,2)=21
      ELSE
        P(I,5)=P(IPU,5)
 
C...No primordial kT, or chosen according to truncated Gaussian or
C...exponential, or (for photon) predetermined or power law.
  120   IF(MINT(40+JT).EQ.2.AND.MINT(10+JT).NE.22) THEN
          IF(MSTP(91).LE.0) THEN
            PT=0.
          ELSEIF(MSTP(91).EQ.1) THEN
            PT=PARP(91)*SQRT(-LOG(RLU(0)))
          ELSE
            RPT1=RLU(0)
            RPT2=RLU(0)
            PT=-PARP(92)*LOG(RPT1*RPT2)
          ENDIF
          IF(PT.GT.PARP(93)) GOTO 120
        ELSEIF(MINT(106+JT).EQ.3) THEN
          PT=SQRT(VINT(282+JT))
          PT=PT*0.8**MINT(57)
          IF(NTRY.GT.10) PT=PT*0.8**(NTRY-10)
        ELSEIF(IABS(MINT(14+JT)).LE.8.OR.MINT(14+JT).EQ.21) THEN
          IF(MSTP(93).LE.0) THEN
            PT=0.
          ELSEIF(MSTP(93).EQ.1) THEN
            PT=PARP(99)*SQRT(-LOG(RLU(0)))
          ELSEIF(MSTP(93).EQ.2) THEN
            RPT1=RLU(0)
            RPT2=RLU(0)
            PT=-PARP(99)*LOG(RPT1*RPT2)
          ELSEIF(MSTP(93).EQ.3) THEN
            HA=PARP(99)**2
            HB=PARP(100)**2
            PT=SQRT(MAX(0.,HA*(HA+HB)/(HA+HB-RLU(0)*HB)-HA))
          ELSE
            HA=PARP(99)**2
            HB=PARP(100)**2
            IF(MSTP(93).EQ.5) HB=MIN(VINT(48),PARP(100)**2)
            PT=SQRT(MAX(0.,HA*((HA+HB)/HA)**RLU(0)-HA))
          ENDIF
          IF(PT.GT.PARP(100)) GOTO 120
        ELSE
          PT=0.
        ENDIF
        VINT(156+JT)=PT
        PHI=PARU(2)*RLU(0)
        P(I,1)=PT*COS(PHI)
        P(I,2)=PT*SIN(PHI)
        PMS(JT)=P(I,5)**2+P(I,1)**2+P(I,2)**2
      ENDIF
  130 CONTINUE
      IF(MINT(47).EQ.1) RETURN
 
C...Kinematics construction for initial partons.
      I1=MINT(83)+3
      I2=MINT(83)+4
      IF(ISUB.EQ.95) THEN
        SHS=0.
        SHR=0.
      ELSE
        SHS=VINT(141)*VINT(142)*VINT(2)+(P(I1,1)+P(I2,1))**2+
     &  (P(I1,2)+P(I2,2))**2
        SHR=SQRT(MAX(0.,SHS))
        IF((SHS-PMS(1)-PMS(2))**2-4.*PMS(1)*PMS(2).LE.0.) GOTO 100
        P(I1,4)=0.5*(SHR+(PMS(1)-PMS(2))/SHR)
        P(I1,3)=SQRT(MAX(0.,P(I1,4)**2-PMS(1)))
        P(I2,4)=SHR-P(I1,4)
        P(I2,3)=-P(I1,3)
 
C...Transform partons to overall CM-frame.
        ROBO(3)=(P(I1,1)+P(I2,1))/SHR
        ROBO(4)=(P(I1,2)+P(I2,2))/SHR
        CALL LUDBRB(I1,I2,0.,0.,-DBLE(ROBO(3)),-DBLE(ROBO(4)),0D0)
        ROBO(2)=ULANGL(P(I1,1),P(I1,2))
        CALL LUDBRB(I1,I2,0.,-ROBO(2),0D0,0D0,0D0)
        ROBO(1)=ULANGL(P(I1,3),P(I1,1))
        CALL LUDBRB(I1,I2,-ROBO(1),0.,0D0,0D0,0D0)
        CALL LUDBRB(I1,MINT(52),ROBO(1),ROBO(2),DBLE(ROBO(3)),
     &  DBLE(ROBO(4)),0D0)
        ROBO(5)=MAX(-0.999999,MIN(0.999999,(VINT(141)-VINT(142))/
     &  (VINT(141)+VINT(142))))
        CALL LUDBRB(I1,MINT(52),0.,0.,0D0,0D0,DBLE(ROBO(5)))
      ENDIF
 
C...Optionally fix up x and Q2 definitions for leptoproduction.
      IDISXQ=0
      IF((MINT(43).EQ.2.OR.MINT(43).EQ.3).AND.((ISUB.EQ.10.AND.
     &MSTP(23).GE.1).OR.(ISUB.EQ.83.AND.MSTP(23).GE.2))) IDISXQ=1
      IF(IDISXQ.EQ.1) THEN
 
C...Find where incoming and outgoing leptons/partons are sitting.
        LESD=1
        IF(MINT(42).EQ.1) LESD=2
        LPIN=MINT(83)+3-LESD
        LEIN=MINT(84)+LESD
        LQIN=MINT(84)+3-LESD
        LEOUT=MINT(84)+2+LESD
        LQOUT=MINT(84)+5-LESD
        IF(K(LEIN,3).GT.LEIN) LEIN=K(LEIN,3)
        IF(K(LQIN,3).GT.LQIN) LQIN=K(LQIN,3)
        LSCMS=0
        DO 140 I=MINT(84)+5,N
        IF(K(I,2).EQ.94) THEN
          LSCMS=I
          LEOUT=I+LESD
          LQOUT=I+3-LESD
        ENDIF
  140   CONTINUE
        LQBG=IPU1
        IF(LESD.EQ.1) LQBG=IPU2
 
C...Calculate actual and wanted momentum transfer.
        XNOM=VINT(43-LESD)
        Q2NOM=-VINT(45)
        HPK=2.*(P(LPIN,4)*P(LEIN,4)-P(LPIN,1)*P(LEIN,1)-
     &  P(LPIN,2)*P(LEIN,2)-P(LPIN,3)*P(LEIN,3))*
     &  (P(MINT(83)+LESD,4)*VINT(40+LESD)/P(LEIN,4))
        HPT2=MAX(0.,Q2NOM*(1.-Q2NOM/(XNOM*HPK)))
        FAC=SQRT(HPT2/(P(LEOUT,1)**2+P(LEOUT,2)**2))
        P(N+1,1)=FAC*P(LEOUT,1)
        P(N+1,2)=FAC*P(LEOUT,2)
        P(N+1,3)=0.25*((HPK-Q2NOM/XNOM)/P(LPIN,4)-
     &  Q2NOM/(P(MINT(83)+LESD,4)*VINT(40+LESD)))*(-1)**(LESD+1)
        P(N+1,4)=SQRT(P(LEOUT,5)**2+P(N+1,1)**2+P(N+1,2)**2+
     &  P(N+1,3)**2)
        DO 150 J=1,4
        QOLD(J)=P(LEIN,J)-P(LEOUT,J)
        QNEW(J)=P(LEIN,J)-P(N+1,J)
  150   CONTINUE
 
C...Boost outgoing electron and daughters.
        IF(LSCMS.EQ.0) THEN
          DO 160 J=1,4
          P(LEOUT,J)=P(N+1,J)
  160     CONTINUE
        ELSE
          DO 170 J=1,3
          P(N+2,J)=(P(N+1,J)-P(LEOUT,J))/(P(N+1,4)+P(LEOUT,4))
  170     CONTINUE
          PINV=2./(1.+P(N+2,1)**2+P(N+2,2)**2+P(N+2,3)**2)
          DO 180 J=1,3
          DBE(J)=PINV*P(N+2,J)
  180     CONTINUE
          DO 200 I=LSCMS+1,N
          IORIG=I
  190     IORIG=K(IORIG,3)
          IF(IORIG.GT.LEOUT) GOTO 190
          IF(I.EQ.LEOUT.OR.IORIG.EQ.LEOUT)
     &    CALL LUDBRB(I,I,0.,0.,DBE(1),DBE(2),DBE(3))
  200     CONTINUE
        ENDIF
 
C...Copy shower initiator and all outgoing partons.
        NCOP=N+1
        K(NCOP,3)=LQBG
        DO 210 J=1,5
        P(NCOP,J)=P(LQBG,J)
  210   CONTINUE
        DO 240 I=MINT(84)+1,N
        ICOP=0
        IF(K(I,1).GT.10) GOTO 240
        IF(I.EQ.LQBG.OR.I.EQ.LQOUT) THEN
          ICOP=I
        ELSE
          IORIG=I
  220     IORIG=K(IORIG,3)
          IF(IORIG.EQ.LQBG.OR.IORIG.EQ.LQOUT) THEN
            ICOP=IORIG
          ELSEIF(IORIG.GT.MINT(84).AND.IORIG.LE.N) THEN
            GOTO 220
          ENDIF
        ENDIF
        IF(ICOP.NE.0) THEN
          NCOP=NCOP+1
          K(NCOP,3)=I
          DO 230 J=1,5
          P(NCOP,J)=P(I,J)
  230     CONTINUE
        ENDIF
  240   CONTINUE
 
C...Calculate relative rescaling factors.
        SLC=3-2*LESD
        PLCSUM=0.
        DO 250 I=N+2,NCOP
        PLCSUM=PLCSUM+(P(I,4)+SLC*P(I,3))
  250   CONTINUE
        DO 260 I=N+2,NCOP
        V(I,1)=(P(I,4)+SLC*P(I,3))/PLCSUM
  260   CONTINUE
 
C...Transfer extra three-momentum of current.
        DO 280 I=N+2,NCOP
        DO 270 J=1,3
        P(I,J)=P(I,J)+V(I,1)*(QNEW(J)-QOLD(J))
  270   CONTINUE
        P(I,4)=SQRT(P(I,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2)
  280   CONTINUE
 
C...Iterate change of initiator momentum to get energy right.
        ITER=0
  290   ITER=ITER+1
        PEEX=-P(N+1,4)-QNEW(4)
        PEMV=-P(N+1,3)/P(N+1,4)
        DO 300 I=N+2,NCOP
        PEEX=PEEX+P(I,4)
        PEMV=PEMV+V(I,1)*P(I,3)/P(I,4)
  300   CONTINUE
        IF(ABS(PEMV).LT.1E-10) THEN
          MINT(51)=1
          MINT(57)=MINT(57)+1
          RETURN
        ENDIF
        PZCH=-PEEX/PEMV
        P(N+1,3)=P(N+1,3)+PZCH
        P(N+1,4)=SQRT(P(N+1,5)**2+P(N+1,1)**2+P(N+1,2)**2+P(N+1,3)**2)
        DO 310 I=N+2,NCOP
        P(I,3)=P(I,3)+V(I,1)*PZCH
        P(I,4)=SQRT(P(I,5)**2+P(I,1)**2+P(I,2)**2+P(I,3)**2)
  310   CONTINUE
        IF(ITER.LT.10.AND.ABS(PEEX).GT.1E-6*P(N+1,4)) GOTO 290
 
C...Modify momenta in event record.
        HBE=2.*(P(N+1,4)+P(LQBG,4))*(P(N+1,3)-P(LQBG,3))/
     &  ((P(N+1,4)+P(LQBG,4))**2+(P(N+1,3)-P(LQBG,3))**2)
        IF(ABS(HBE).GT.0.999999) THEN
          MINT(51)=1
          MINT(57)=MINT(57)+1
          RETURN
        ENDIF
        I=MINT(83)+5-LESD
        CALL LUDBRB(I,I,0.,0.,0D0,0D0,DBLE(HBE))
        DO 330 I=N+1,NCOP
        ICOP=K(I,3)
        DO 320 J=1,4
        P(ICOP,J)=P(I,J)
  320   CONTINUE
  330   CONTINUE
      ENDIF
 
C...Check minimum invariant mass of remnant system(s).
      PSYS(0,4)=P(I1,4)+P(I2,4)+0.5*VINT(1)*(VINT(151)+VINT(152))
      PSYS(0,3)=P(I1,3)+P(I2,3)+0.5*VINT(1)*(VINT(151)-VINT(152))
      PMS(0)=MAX(0.,PSYS(0,4)**2-PSYS(0,3)**2)
      PMIN(0)=SQRT(PMS(0))
      DO 340 JT=1,2
      PSYS(JT,4)=0.5*VINT(1)*VINT(142+JT)
      PSYS(JT,3)=PSYS(JT,4)*(-1)**(JT-1)
      PMIN(JT)=0.
      IF(MINT(44+JT).EQ.1) GOTO 340
      MINT(105)=MINT(102+JT)
      MINT(109)=MINT(106+JT)
      CALL PYSPLI(MINT(10+JT),MINT(12+JT),KFLCH(JT),KFLSP(JT))
      IF(KFLCH(JT).NE.0) PMIN(JT)=PMIN(JT)+ULMASS(KFLCH(JT))
      IF(KFLSP(JT).NE.0) PMIN(JT)=PMIN(JT)+ULMASS(KFLSP(JT))
      IF(KFLCH(JT)*KFLSP(JT).NE.0) PMIN(JT)=PMIN(JT)+0.5*PARP(111)
      PMIN(JT)=SQRT(PMIN(JT)**2+P(MINT(83)+JT+2,1)**2+
     &P(MINT(83)+JT+2,2)**2)
  340 CONTINUE
      IF(PMIN(0)+PMIN(1)+PMIN(2).GT.VINT(1).OR.(MINT(45).GE.2.AND.
     &PMIN(1).GT.PSYS(1,4)).OR.(MINT(46).GE.2.AND.PMIN(2).GT.
     &PSYS(2,4))) THEN
        MINT(51)=1
        MINT(57)=MINT(57)+1
        RETURN
      ENDIF
 
C...Loop over two remnants; skip if none there.
      I=NS
      DO 410 JT=1,2
      ISN(JT)=0
      IF(MINT(44+JT).EQ.1) GOTO 410
      IF(JT.EQ.1) IPU=IPU1
      IF(JT.EQ.2) IPU=IPU2
 
C...Store first remnant parton.
      I=I+1
      IS(JT)=I
      ISN(JT)=1
      DO 350 J=1,5
      K(I,J)=0
      P(I,J)=0.
      V(I,J)=0.
  350 CONTINUE
      K(I,1)=1
      K(I,2)=KFLSP(JT)
      K(I,3)=MINT(83)+JT
      P(I,5)=ULMASS(K(I,2))
 
C...First parton colour connections and kinematics.
      KCOL=KCHG(LUCOMP(KFLSP(JT)),2)
      IF(KCOL.EQ.2) THEN
        K(I,1)=3
        K(I,4)=MSTU(5)*IPU+IPU
        K(I,5)=MSTU(5)*IPU+IPU
        K(IPU,4)=MOD(K(IPU,4),MSTU(5))+MSTU(5)*I
        K(IPU,5)=MOD(K(IPU,5),MSTU(5))+MSTU(5)*I
      ELSEIF(KCOL.NE.0) THEN
        K(I,1)=3
        KFLS=(3-KCOL*ISIGN(1,KFLSP(JT)))/2
        K(I,KFLS+3)=IPU
        K(IPU,6-KFLS)=MOD(K(IPU,6-KFLS),MSTU(5))+MSTU(5)*I
      ENDIF
      IF(KFLCH(JT).EQ.0) THEN
        P(I,1)=-P(MINT(83)+JT+2,1)
        P(I,2)=-P(MINT(83)+JT+2,2)
        PMS(JT)=P(I,5)**2+P(I,1)**2+P(I,2)**2
        PSYS(JT,3)=SQRT(MAX(0.,PSYS(JT,4)**2-PMS(JT)))*(-1)**(JT-1)
        P(I,3)=PSYS(JT,3)
        P(I,4)=PSYS(JT,4)
 
C...When extra remnant parton or hadron: store extra remnant.
      ELSE
        I=I+1
        ISN(JT)=2
        DO 360 J=1,5
        K(I,J)=0
        P(I,J)=0.
        V(I,J)=0.
  360   CONTINUE
        K(I,1)=1
        K(I,2)=KFLCH(JT)
        K(I,3)=MINT(83)+JT
        P(I,5)=ULMASS(K(I,2))
 
C...Find parton colour connections of extra remnant.
        KCOL=KCHG(LUCOMP(KFLCH(JT)),2)
        IF(KCOL.EQ.2) THEN
          K(I,1)=3
          K(I,4)=MSTU(5)*IPU+IPU
          K(I,5)=MSTU(5)*IPU+IPU
          K(IPU,4)=MOD(K(IPU,4),MSTU(5))+MSTU(5)*I
          K(IPU,5)=MOD(K(IPU,5),MSTU(5))+MSTU(5)*I
        ELSEIF(KCOL.NE.0) THEN
          K(I,1)=3
          KFLS=(3-KCOL*ISIGN(1,KFLCH(JT)))/2
          K(I,KFLS+3)=IPU
          K(IPU,6-KFLS)=MOD(K(IPU,6-KFLS),MSTU(5))+MSTU(5)*I
        ENDIF
 
C...Relative transverse momentum when two remnants.
        LOOP=0
  370   LOOP=LOOP+1
        CALL LUPTDI(1,P(I-1,1),P(I-1,2))
        IF(IABS(MINT(10+JT)).LT.20) THEN
          P(I-1,1)=0.
          P(I-1,2)=0.
        ENDIF
        PMS(JT+2)=P(I-1,5)**2+P(I-1,1)**2+P(I-1,2)**2
        P(I,1)=-P(MINT(83)+JT+2,1)-P(I-1,1)
        P(I,2)=-P(MINT(83)+JT+2,2)-P(I-1,2)
        PMS(JT+4)=P(I,5)**2+P(I,1)**2+P(I,2)**2
 
C...Meson or baryon; photon as meson. For splitup below.
        IMB=1
        IF(MOD(MINT(10+JT)/1000,10).NE.0) IMB=2
 
C***Relative distribution for electron into two electrons. Temporary!
        IF(IABS(MINT(10+JT)).LT.20.AND.MINT(14+JT).EQ.-MINT(10+JT))
     &  THEN
          CHI(JT)=RLU(0)
 
C...Relative distribution of electron energy into electron plus parton.
        ELSEIF(IABS(MINT(10+JT)).LT.20) THEN
          XHRD=VINT(140+JT)
          XE=VINT(154+JT)
          CHI(JT)=(XE-XHRD)/(1.-XHRD)
 
C...Relative distribution of energy for particle into two jets.
        ELSEIF(IABS(KFLCH(JT)).LE.10.OR.KFLCH(JT).EQ.21) THEN
          CHIK=PARP(92+2*IMB)
          IF(MSTP(92).LE.1) THEN
            IF(IMB.EQ.1) CHI(JT)=RLU(0)
            IF(IMB.EQ.2) CHI(JT)=1.-SQRT(RLU(0))
          ELSEIF(MSTP(92).EQ.2) THEN
            CHI(JT)=1.-RLU(0)**(1./(1.+CHIK))
          ELSEIF(MSTP(92).EQ.3) THEN
            CUT=2.*0.3/VINT(1)
  380       CHI(JT)=RLU(0)**2
            IF((CHI(JT)**2/(CHI(JT)**2+CUT**2))**0.25*(1.-CHI(JT))**CHIK
     &      .LT.RLU(0)) GOTO 380
          ELSEIF(MSTP(92).EQ.4) THEN
            CUT=2.*0.3/VINT(1)
            CUTR=(1.+SQRT(1.+CUT**2))/CUT
  390       CHIR=CUT*CUTR**RLU(0)
            CHI(JT)=(CHIR**2-CUT**2)/(2.*CHIR)
            IF((1.-CHI(JT))**CHIK.LT.RLU(0)) GOTO 390
          ELSE
            CUT=2.*0.3/VINT(1)
            CUTA=CUT**(1.-PARP(98))
            CUTB=(1.+CUT)**(1.-PARP(98))
  400       CHI(JT)=(CUTA+RLU(0)*(CUTB-CUTA))**(1./(1.-PARP(98)))
            IF(((CHI(JT)+CUT)**2/(2.*(CHI(JT)**2+CUT**2)))**
     &      (0.5*PARP(98))*(1.-CHI(JT))**CHIK.LT.RLU(0)) GOTO 400
          ENDIF
 
C...Relative distribution of energy for particle into jet plus particle.
        ELSE
          IF(MSTP(94).LE.1) THEN
            IF(IMB.EQ.1) CHI(JT)=RLU(0)
            IF(IMB.EQ.2) CHI(JT)=1.-SQRT(RLU(0))
            IF(MOD(KFLCH(JT)/1000,10).NE.0) CHI(JT)=1.-CHI(JT)
          ELSEIF(MSTP(94).EQ.2) THEN
            CHI(JT)=1.-RLU(0)**(1./(1.+PARP(93+2*IMB)))
            IF(MOD(KFLCH(JT)/1000,10).NE.0) CHI(JT)=1.-CHI(JT)
          ELSEIF(MSTP(94).EQ.3) THEN
            CALL LUZDIS(1,0,PMS(JT+4),ZZ)
            CHI(JT)=ZZ
          ELSE
            CALL LUZDIS(1000,0,PMS(JT+4),ZZ)
            CHI(JT)=ZZ
          ENDIF
        ENDIF
 
C...Construct total transverse mass; reject if too large.
        PMS(JT)=PMS(JT+4)/CHI(JT)+PMS(JT+2)/(1.-CHI(JT))
        IF(PMS(JT).GT.PSYS(JT,4)**2) THEN
          IF(LOOP.LT.10) THEN
            GOTO 370
          ELSE
            MINT(51)=1
            MINT(57)=MINT(57)+1
            RETURN
          ENDIF
        ENDIF
        PSYS(JT,3)=SQRT(MAX(0.,PSYS(JT,4)**2-PMS(JT)))*(-1)**(JT-1)
        VINT(158+JT)=CHI(JT)
 
C...Subdivide longitudinal momentum according to value selected above.
        PW1=CHI(JT)*(PSYS(JT,4)+ABS(PSYS(JT,3)))
        P(IS(JT)+1,4)=0.5*(PW1+PMS(JT+4)/PW1)
        P(IS(JT)+1,3)=0.5*(PW1-PMS(JT+4)/PW1)*(-1)**(JT-1)
        P(IS(JT),4)=PSYS(JT,4)-P(IS(JT)+1,4)
        P(IS(JT),3)=PSYS(JT,3)-P(IS(JT)+1,3)
      ENDIF
  410 CONTINUE
      N=I
 
C...Check if longitudinal boosts needed - if so pick two systems.
      PDEV=ABS(PSYS(0,4)+PSYS(1,4)+PSYS(2,4)-VINT(1))+
     &ABS(PSYS(0,3)+PSYS(1,3)+PSYS(2,3))
      IF(PDEV.LE.1E-6*VINT(1)) RETURN
      IF(ISN(1).EQ.0) THEN
        IR=0
        IL=2
      ELSEIF(ISN(2).EQ.0) THEN
        IR=1
        IL=0
      ELSEIF(VINT(143).GT.0.2.AND.VINT(144).GT.0.2) THEN
        IR=1
        IL=2
      ELSEIF(VINT(143).GT.0.2) THEN
        IR=1
        IL=0
      ELSEIF(VINT(144).GT.0.2) THEN
        IR=0
        IL=2
      ELSEIF(PMS(1)/PSYS(1,4)**2.GT.PMS(2)/PSYS(2,4)**2) THEN
        IR=1
        IL=0
      ELSE
        IR=0
        IL=2
      ENDIF
      IG=3-IR-IL
 
C...E+-pL wanted for system to be modified.
      IF((IG.EQ.1.AND.ISN(1).EQ.0).OR.(IG.EQ.2.AND.ISN(2).EQ.0)) THEN
        PPB=VINT(1)
        PNB=VINT(1)
      ELSE
        PPB=VINT(1)-(PSYS(IG,4)+PSYS(IG,3))
        PNB=VINT(1)-(PSYS(IG,4)-PSYS(IG,3))
      ENDIF
 
C...To keep x and Q2 in leptoproduction: do not count scattered lepton.
      IF(IDISXQ.EQ.1.AND.IG.NE.0) THEN
        PMTB=PPB*PNB
        PMTR=PMS(IR)
        PMTL=PMS(IL)
        SQLAM=SQRT(MAX(0.,(PMTB-PMTR-PMTL)**2-4.*PMTR*PMTL))
        SQSGN=SIGN(1.,PSYS(IR,3)*PSYS(IL,4)-PSYS(IL,3)*PSYS(IR,4))
        RKR=(PMTB+PMTR-PMTL+SQLAM*SQSGN)/(2.*(PSYS(IR,4)+PSYS(IR,3))
     &  *PNB)
        RKL=(PMTB+PMTL-PMTR+SQLAM*SQSGN)/(2.*(PSYS(IL,4)-PSYS(IL,3))
     &  *PPB)
        BER=(RKR**2-1.)/(RKR**2+1.)
        BEL=-(RKL**2-1.)/(RKL**2+1.)
        PPB=PPB-(PSYS(0,4)+PSYS(0,3))
        PNB=PNB-(PSYS(0,4)-PSYS(0,3))
        DO 420 J=1,4
        PSYS(0,J)=0.
  420   CONTINUE
        DO 450 I=MINT(84)+1,NS
        IF(K(I,1).GT.10) GOTO 450
        INCL=0
        IORIG=I
  430   IF(IORIG.EQ.LQOUT.OR.IORIG.EQ.LPIN+2) INCL=1
        IORIG=K(IORIG,3)
        IF(IORIG.GT.LPIN) GOTO 430
        IF(INCL.EQ.0) GOTO 450
        DO 440 J=1,4
        PSYS(0,J)=PSYS(0,J)+P(I,J)
  440   CONTINUE
  450   CONTINUE
        PMS(0)=MAX(0.,PSYS(0,4)**2-PSYS(0,3)**2)
        PPB=PPB+(PSYS(0,4)+PSYS(0,3))
        PNB=PNB+(PSYS(0,4)-PSYS(0,3))
      ENDIF
 
C...Construct longitudinal boosts.
      DPMTB=PPB*PNB
      DPMTR=PMS(IR)
      DPMTL=PMS(IL)
      DSQLAM=SQRT(MAX(0D0,(DPMTB-DPMTR-DPMTL)**2-4D0*DPMTR*DPMTL))
      IF(DSQLAM.LE.1D-6*DPMTB) THEN
        MINT(51)=1
        MINT(57)=MINT(57)+1
        RETURN
      ENDIF
      DSQSGN=SIGN(1D0,DBLE(PSYS(IR,3)*PSYS(IL,4)-PSYS(IL,3)*PSYS(IR,4)))
      DRKR=(DPMTB+DPMTR-DPMTL+DSQLAM*DSQSGN)/
     &(2.*(PSYS(IR,4)+PSYS(IR,3))*PNB)
      DRKL=(DPMTB+DPMTL-DPMTR+DSQLAM*DSQSGN)/
     &(2.*(PSYS(IL,4)-PSYS(IL,3))*PPB)
      DBER=(DRKR**2-1D0)/(DRKR**2+1D0)
      DBEL=-(DRKL**2-1D0)/(DRKL**2+1D0)
 
C...Perform longitudinal boosts.
      IF(IR.EQ.1.AND.ISN(1).EQ.1.AND.DBER.LE.-0.99999999D0) THEN
        P(IS(1),3)=0.
        P(IS(1),4)=SQRT(P(IS(1),5)**2+P(IS(1),1)**2+P(IS(1),2)**2)
      ELSEIF(IR.EQ.1) THEN
        CALL LUDBRB(IS(1),IS(1)+ISN(1)-1,0.,0.,0D0,0D0,DBER)
      ELSEIF(IDISXQ.EQ.1) THEN
        DO 470 I=I1,NS
        INCL=0
        IORIG=I
  460   IF(IORIG.EQ.LQOUT.OR.IORIG.EQ.LPIN+2) INCL=1
        IORIG=K(IORIG,3)
        IF(IORIG.GT.LPIN) GOTO 460
        IF(INCL.EQ.1) CALL LUDBRB(I,I,0.,0.,0D0,0D0,DBER)
  470   CONTINUE
      ELSE
        CALL LUDBRB(I1,NS,0.,0.,0D0,0D0,DBER)
      ENDIF
      IF(IL.EQ.2.AND.ISN(2).EQ.1.AND.DBEL.GE.0.99999999D0) THEN
        P(IS(2),3)=0.
        P(IS(2),4)=SQRT(P(IS(2),5)**2+P(IS(2),1)**2+P(IS(2),2)**2)
      ELSEIF(IL.EQ.2) THEN
        CALL LUDBRB(IS(2),IS(2)+ISN(2)-1,0.,0.,0D0,0D0,DBEL)
      ELSEIF(IDISXQ.EQ.1) THEN
        DO 490 I=I1,NS
        INCL=0
        IORIG=I
  480   IF(IORIG.EQ.LQOUT.OR.IORIG.EQ.LPIN+2) INCL=1
        IORIG=K(IORIG,3)
        IF(IORIG.GT.LPIN) GOTO 480
        IF(INCL.EQ.1) CALL LUDBRB(I,I,0.,0.,0D0,0D0,DBEL)
  490   CONTINUE
      ELSE
        CALL LUDBRB(I1,NS,0.,0.,0D0,0D0,DBEL)
      ENDIF
 
C...Final check that energy-momentum conservation worked.
      PESUM=0.
      PZSUM=0.
      DO 500 I=MINT(84)+1,N
      IF(K(I,1).GT.10) GOTO 500
      PESUM=PESUM+P(I,4)
      PZSUM=PZSUM+P(I,3)
  500 CONTINUE
      PDEV=ABS(PESUM-VINT(1))+ABS(PZSUM)
      IF(PDEV.GT.1E-4*VINT(1)) THEN
        MINT(51)=1
        MINT(57)=MINT(57)+1
        RETURN
      ENDIF
 
C...Calculate rotation and boost from overall CM frame to
C...hadronic CM frame in leptoproduction.
      MINT(91)=0
      IF(MINT(82).EQ.1.AND.(MINT(43).EQ.2.OR.MINT(43).EQ.3)) THEN
        MINT(91)=1
        LESD=1
        IF(MINT(42).EQ.1) LESD=2
        LPIN=MINT(83)+3-LESD
 
C...Sum upp momenta of everything not lepton or photon to define boost.
        DO 510 J=1,4
        PSUM(J)=0.
  510   CONTINUE
        DO 530 I=1,N
        IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 530
        IF(IABS(K(I,2)).GE.11.AND.IABS(K(I,2)).LE.20) GOTO 530
        IF(K(I,2).EQ.22) GOTO 530
        DO 520 J=1,4
        PSUM(J)=PSUM(J)+P(I,J)
  520   CONTINUE
  530   CONTINUE
        VINT(223)=-PSUM(1)/PSUM(4)
        VINT(224)=-PSUM(2)/PSUM(4)
        VINT(225)=-PSUM(3)/PSUM(4)
 
C...Boost incoming hadron to hadronic CM frame to determine rotations.
        K(N+1,1)=1
        DO 540 J=1,5
        P(N+1,J)=P(LPIN,J)
        V(N+1,J)=V(LPIN,J)
  540   CONTINUE
        CALL LUDBRB(N+1,N+1,0.,0.,DBLE(VINT(223)),DBLE(VINT(224)),
     &  DBLE(VINT(225)))
        VINT(222)=-ULANGL(P(N+1,1),P(N+1,2))
        CALL LUDBRB(N+1,N+1,0.,VINT(222),0D0,0D0,0D0)
        IF(LESD.EQ.2) THEN
          VINT(221)=-ULANGL(P(N+1,3),P(N+1,1))
        ELSE
          VINT(221)=ULANGL(-P(N+1,3),P(N+1,1))
        ENDIF
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYDIFF
 
C...Handles diffractive and elastic scattering.
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DOUBLE PRECISION DBETAZ
 
C...Reset K, P and V vectors. Store incoming particles.
      DO 110 JT=1,MSTP(126)+10
      I=MINT(83)+JT
      DO 100 J=1,5
      K(I,J)=0
      P(I,J)=0.
      V(I,J)=0.
  100 CONTINUE
  110 CONTINUE
      N=MINT(84)
      MINT(3)=0
      MINT(21)=0
      MINT(22)=0
      MINT(23)=0
      MINT(24)=0
      MINT(4)=4
      DO 130 JT=1,2
      I=MINT(83)+JT
      K(I,1)=21
      K(I,2)=MINT(10+JT)
      DO 120 J=1,5
      P(I,J)=VINT(285+5*JT+J)
  120 CONTINUE
  130 CONTINUE
      MINT(6)=2
 
C...Subprocess; kinematics.
      SQLAM=(VINT(2)-VINT(63)-VINT(64))**2-4.*VINT(63)*VINT(64)
      PZ=SQRT(SQLAM)/(2.*VINT(1))
      DO 200 JT=1,2
      I=MINT(83)+JT
      PE=(VINT(2)+VINT(62+JT)-VINT(65-JT))/(2.*VINT(1))
      KFH=MINT(102+JT)
 
C...Elastically scattered particle.
      IF(MINT(16+JT).LE.0) THEN
        N=N+1
        K(N,1)=1
        K(N,2)=KFH
        K(N,3)=I+2
        P(N,3)=PZ*(-1)**(JT+1)
        P(N,4)=PE
        P(N,5)=SQRT(VINT(62+JT))
 
C...Decay rho from elastic scattering of gamma with sin**2(theta)
C...distribution of decay products (in rho rest frame).
        IF(KFH.EQ.113.AND.MINT(10+JT).EQ.22.AND.MSTP(102).EQ.1) THEN
          NSAV=N
          DBETAZ=DBLE(P(N,3))/SQRT(DBLE(P(N,3))**2+DBLE(P(N,5))**2)
          P(N,3)=0.
          P(N,4)=P(N,5)
          CALL LUDECY(NSAV)
          IF(N.EQ.NSAV+2.AND.IABS(K(NSAV+1,2)).EQ.211) THEN
            PHI=ULANGL(P(NSAV+1,1),P(NSAV+1,2))
            CALL LUDBRB(NSAV+1,NSAV+2,0.,-PHI,0D0,0D0,0D0)
            THE=ULANGL(P(NSAV+1,3),P(NSAV+1,1))
            CALL LUDBRB(NSAV+1,NSAV+2,-THE,0.,0D0,0D0,0D0)
  140       CTHE=2.*RLU(0)-1.
            IF(1.-CTHE**2.LT.RLU(0)) GOTO 140
            CALL LUDBRB(NSAV+1,NSAV+2,ACOS(CTHE),PHI,0D0,0D0,0D0)
          ENDIF
          CALL LUDBRB(NSAV,NSAV+2,0.,0.,0D0,0D0,DBETAZ)
        ENDIF
 
C...Diffracted particle: low-mass system to two particles.
      ELSEIF(VINT(62+JT).LT.(VINT(66+JT)+PARP(103))**2) THEN
        N=N+2
        K(N-1,1)=1
        K(N,1)=1
        K(N-1,3)=I+2
        K(N,3)=I+2
        PMMAS=SQRT(VINT(62+JT))
        NTRY=0
  150   NTRY=NTRY+1
        IF(NTRY.LT.20) THEN
          MINT(105)=MINT(102+JT)
          MINT(109)=MINT(106+JT)
          CALL PYSPLI(KFH,21,KFL1,KFL2)
          CALL LUKFDI(KFL1,0,KFL3,KF1)
          IF(KF1.EQ.0) GOTO 150
          CALL LUKFDI(KFL2,-KFL3,KFLDUM,KF2)
          IF(KF2.EQ.0) GOTO 150
        ELSE
          KF1=KFH
          KF2=111
        ENDIF
        PM1=ULMASS(KF1)
        PM2=ULMASS(KF2)
        IF(PM1+PM2+PARJ(64).GT.PMMAS) GOTO 150
        K(N-1,2)=KF1
        K(N,2)=KF2
        P(N-1,5)=PM1
        P(N,5)=PM2
        PZP=SQRT(MAX(0.,(PMMAS**2-PM1**2-PM2**2)**2-4.*PM1**2*PM2**2))/
     &  (2.*PMMAS)
        P(N-1,3)=PZP
        P(N,3)=-PZP
        P(N-1,4)=SQRT(PM1**2+PZP**2)
        P(N,4)=SQRT(PM2**2+PZP**2)
        CALL LUDBRB(N-1,N,ACOS(2.*RLU(0)-1.),PARU(2)*RLU(0),0D0,0D0,0D0)
        DBETAZ=DBLE(PZ)*(-1)**(JT+1)/SQRT(DBLE(PZ)**2+DBLE(PMMAS)**2)
        CALL LUDBRB(N-1,N,0.,0.,0D0,0D0,DBETAZ)
 
C...Diffracted particle: valence quark kicked out.
      ELSEIF(MSTP(101).EQ.1.OR.(MSTP(101).EQ.3.AND.RLU(0).LT.PARP(101)))
     &THEN
        N=N+2
        K(N-1,1)=2
        K(N,1)=1
        K(N-1,3)=I+2
        K(N,3)=I+2
        MINT(105)=MINT(102+JT)
        MINT(109)=MINT(106+JT)
        CALL PYSPLI(KFH,21,K(N,2),K(N-1,2))
        P(N-1,5)=ULMASS(K(N-1,2))
        P(N,5)=ULMASS(K(N,2))
        SQLAM=(VINT(62+JT)-P(N-1,5)**2-P(N,5)**2)**2-
     &  4.*P(N-1,5)**2*P(N,5)**2
        P(N-1,3)=(PE*SQRT(SQLAM)+PZ*(VINT(62+JT)+P(N-1,5)**2-
     &  P(N,5)**2))/(2.*VINT(62+JT))*(-1)**(JT+1)
        P(N-1,4)=SQRT(P(N-1,3)**2+P(N-1,5)**2)
        P(N,3)=PZ*(-1)**(JT+1)-P(N-1,3)
        P(N,4)=SQRT(P(N,3)**2+P(N,5)**2)
 
C...Diffracted particle: gluon kicked out.
      ELSE
        N=N+3
        K(N-2,1)=2
        K(N-1,1)=2
        K(N,1)=1
        K(N-2,3)=I+2
        K(N-1,3)=I+2
        K(N,3)=I+2
        MINT(105)=MINT(102+JT)
        MINT(109)=MINT(106+JT)
        CALL PYSPLI(KFH,21,K(N,2),K(N-2,2))
        K(N-1,2)=21
        P(N-2,5)=ULMASS(K(N-2,2))
        P(N-1,5)=0.
        P(N,5)=ULMASS(K(N,2))
C...Energy distribution for particle into two jets.
  160   IMB=1
        IF(MOD(KFH/1000,10).NE.0) IMB=2
        CHIK=PARP(92+2*IMB)
        IF(MSTP(92).LE.1) THEN
          IF(IMB.EQ.1) CHI=RLU(0)
          IF(IMB.EQ.2) CHI=1.-SQRT(RLU(0))
        ELSEIF(MSTP(92).EQ.2) THEN
          CHI=1.-RLU(0)**(1./(1.+CHIK))
        ELSEIF(MSTP(92).EQ.3) THEN
          CUT=2.*0.3/VINT(1)
  170     CHI=RLU(0)**2
          IF((CHI**2/(CHI**2+CUT**2))**0.25*(1.-CHI)**CHIK.LT.
     &    RLU(0)) GOTO 170
        ELSEIF(MSTP(92).EQ.4) THEN
          CUT=2.*0.3/VINT(1)
          CUTR=(1.+SQRT(1.+CUT**2))/CUT
  180     CHIR=CUT*CUTR**RLU(0)
          CHI=(CHIR**2-CUT**2)/(2.*CHIR)
          IF((1.-CHI)**CHIK.LT.RLU(0)) GOTO 180
        ELSE
          CUT=2.*0.3/VINT(1)
          CUTA=CUT**(1.-PARP(98))
          CUTB=(1.+CUT)**(1.-PARP(98))
  190     CHI=(CUTA+RLU(0)*(CUTB-CUTA))**(1./(1.-PARP(98)))
          IF(((CHI+CUT)**2/(2.*(CHI**2+CUT**2)))**
     &    (0.5*PARP(98))*(1.-CHI)**CHIK.LT.RLU(0)) GOTO 190
        ENDIF
        IF(CHI.LT.P(N,5)**2/VINT(62+JT).OR.CHI.GT.1.-P(N-2,5)**2/
     &  VINT(62+JT)) GOTO 160
        SQM=P(N-2,5)**2/(1.-CHI)+P(N,5)**2/CHI
        IF((SQRT(SQM)+PARJ(32))**2.GE.VINT(62+JT)) GOTO 160
        PZI=(PE*(VINT(62+JT)-SQM)+PZ*(VINT(62+JT)+SQM))/
     &  (2.*VINT(62+JT))
        PEI=SQRT(PZI**2+SQM)
        PQQP=(1.-CHI)*(PEI+PZI)
        P(N-2,3)=0.5*(PQQP-P(N-2,5)**2/PQQP)*(-1)**(JT+1)
        P(N-2,4)=SQRT(P(N-2,3)**2+P(N-2,5)**2)
        P(N-1,4)=0.5*(VINT(62+JT)-SQM)/(PEI+PZI)
        P(N-1,3)=P(N-1,4)*(-1)**JT
        P(N,3)=PZI*(-1)**(JT+1)-P(N-2,3)
        P(N,4)=SQRT(P(N,3)**2+P(N,5)**2)
      ENDIF
 
C...Documentation lines.
      K(I+2,1)=21
      IF(MINT(16+JT).EQ.0) K(I+2,2)=KFH
      IF(MINT(16+JT).NE.0) K(I+2,2)=10*(KFH/10)
      K(I+2,3)=I
      P(I+2,3)=PZ*(-1)**(JT+1)
      P(I+2,4)=PE
      P(I+2,5)=SQRT(VINT(62+JT))
  200 CONTINUE
 
C...Rotate outgoing partons/particles using cos(theta).
      IF(VINT(23).LT.0.9) THEN
        CALL LUDBRB(MINT(83)+3,N,ACOS(VINT(23)),VINT(24),0D0,0D0,0D0)
      ELSE
        CALL LUDBRB(MINT(83)+3,N,ASIN(VINT(59)),VINT(24),0D0,0D0,0D0)
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYDOCU
 
C...Handles the decumentation of the process in MSTI and PARI,
C...and also computes cross-sections based on accumulated statistics.
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      INTEGER  MSEL,MSUB,KFIN
      REAL  CKIN
      SAVE /PYSUBS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      INTEGER  ISET,KFPR,ICOL
      REAL  COEF
      SAVE /PYINT2/
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      COMMON/PYINT9/DXSEC(0:200)
      DOUBLE PRECISION DXSEC
      SAVE /PYINT9/
C...Calculate Monte Carlo estimates of cross-sections.
      ISUB=MINT(1)
      IF(MSTP(111).NE.-1) NGEN(ISUB,3)=NGEN(ISUB,3)+1
      NGEN(0,3)=NGEN(0,3)+1
      XSEC(0,3)=0.
      DO 100 I=1,200
      IF(I.EQ.96.OR.I.EQ.97) THEN
        XSEC(I,3)=0.
      ELSEIF(MSUB(95).EQ.1.AND.(I.EQ.11.OR.I.EQ.12.OR.I.EQ.13.OR.
     &I.EQ.28.OR.I.EQ.53.OR.I.EQ.68)) THEN
        XSEC(I,3)=DXSEC(96)*NGEN(I,3)/MAX(1.,FLOAT(NGEN(96,1))*
     &  FLOAT(NGEN(96,2)))
      ELSEIF(MSUB(I).EQ.0.OR.NGEN(I,1).EQ.0) THEN
        XSEC(I,3)=0.
      ELSEIF(NGEN(I,2).EQ.0) THEN
        XSEC(I,3)=DXSEC(I)*NGEN(0,3)/(FLOAT(NGEN(I,1))*
     &  FLOAT(NGEN(0,2)))
      ELSE
        XSEC(I,3)=DXSEC(I)*NGEN(I,3)/(FLOAT(NGEN(I,1))*
     &  FLOAT(NGEN(I,2)))
      ENDIF
      XSEC(0,3)=XSEC(0,3)+XSEC(I,3)
  100 CONTINUE
 
C...Rescale to known low-pT cross-section for standard QCD processes.
      IF(MSUB(95).EQ.1) THEN
        XSECH=XSEC(11,3)+XSEC(12,3)+XSEC(13,3)+XSEC(28,3)+XSEC(53,3)+
     &  XSEC(68,3)+XSEC(95,3)
        XSECW=DXSEC(97)/MAX(1.,FLOAT(NGEN(97,1)))
        IF(XSECH.GT.1E-10.AND.XSECW.GT.1E-10) THEN
          FAC=XSECW/XSECH
          XSEC(11,3)=FAC*XSEC(11,3)
          XSEC(12,3)=FAC*XSEC(12,3)
          XSEC(13,3)=FAC*XSEC(13,3)
          XSEC(28,3)=FAC*XSEC(28,3)
          XSEC(53,3)=FAC*XSEC(53,3)
          XSEC(68,3)=FAC*XSEC(68,3)
          XSEC(95,3)=FAC*XSEC(95,3)
          XSEC(0,3)=XSEC(0,3)-XSECH+XSECW
        ENDIF
      ENDIF
 
C...Save information for gamma-p and gamma-gamma.
      IF(MINT(121).GT.1) THEN
        IGA=MINT(122)
        CALL PYSAVE(2,IGA)
        CALL PYSAVE(5,0)
      ENDIF
 
C...Reset information on hard interaction.
      DO 110 J=1,200
      MSTI(J)=0
      PARI(J)=0.
  110 CONTINUE
 
C...Copy integer valued information from MINT into MSTI.
      DO 120 J=1,31
      MSTI(J)=MINT(J)
  120 CONTINUE
      IF(MINT(121).GT.1) MSTI(9)=MINT(122)
 
C...Store cross-section variables in PARI.
      PARI(1)=XSEC(0,3)
      PARI(2)=XSEC(0,3)/MINT(5)
      PARI(9)=VINT(99)
      PARI(10)=VINT(100)
      VINT(98)=VINT(98)+VINT(100)
      IF(MSTP(142).EQ.1) PARI(2)=XSEC(0,3)/VINT(98)
 
C...Store kinematics variables in PARI.
      PARI(11)=VINT(1)
      PARI(12)=VINT(2)
      IF(ISUB.NE.95) THEN
        DO 130 J=13,26
        PARI(J)=VINT(30+J)
  130   CONTINUE
        PARI(31)=VINT(141)
        PARI(32)=VINT(142)
        PARI(33)=VINT(41)
        PARI(34)=VINT(42)
        PARI(35)=PARI(33)-PARI(34)
        PARI(36)=VINT(21)
        PARI(37)=VINT(22)
        PARI(38)=VINT(26)
        PARI(39)=VINT(157)
        PARI(40)=VINT(158)
        PARI(41)=VINT(23)
        PARI(42)=2.*VINT(47)/VINT(1)
      ENDIF
 
C...Store information on scattered partons in PARI.
      IF(ISUB.NE.95.AND.MINT(7)*MINT(8).NE.0) THEN
        DO 140 IS=7,8
        I=MINT(IS)
        PARI(36+IS)=P(I,3)/VINT(1)
        PARI(38+IS)=P(I,4)/VINT(1)
        PR=MAX(1E-20,P(I,5)**2+P(I,1)**2+P(I,2)**2)
        PARI(40+IS)=SIGN(LOG(MIN((SQRT(PR+P(I,3)**2)+ABS(P(I,3)))/
     &  SQRT(PR),1E20)),P(I,3))
        PR=MAX(1E-20,P(I,1)**2+P(I,2)**2)
        PARI(42+IS)=SIGN(LOG(MIN((SQRT(PR+P(I,3)**2)+ABS(P(I,3)))/
     &  SQRT(PR),1E20)),P(I,3))
        PARI(44+IS)=P(I,3)/SQRT(1E-20+P(I,1)**2+P(I,2)**2+P(I,3)**2)
        PARI(46+IS)=ULANGL(P(I,3),SQRT(P(I,1)**2+P(I,2)**2))
        PARI(48+IS)=ULANGL(P(I,1),P(I,2))
  140   CONTINUE
      ENDIF
 
C...Store sum up transverse and longitudinal momenta.
      PARI(65)=2.*PARI(17)
      IF(ISUB.LE.90.OR.ISUB.GE.95) THEN
        DO 150 I=MSTP(126)+1,N
        IF(K(I,1).LE.0.OR.K(I,1).GT.10) GOTO 150
        PT=SQRT(P(I,1)**2+P(I,2)**2)
        PARI(69)=PARI(69)+PT
        IF(I.LE.MINT(52)) PARI(66)=PARI(66)+PT
        IF(I.GT.MINT(52).AND.I.LE.MINT(53)) PARI(68)=PARI(68)+PT
  150   CONTINUE
        PARI(67)=PARI(68)
        PARI(71)=VINT(151)
        PARI(72)=VINT(152)
        PARI(73)=VINT(151)
        PARI(74)=VINT(152)
      ELSE
        PARI(66)=PARI(65)
        PARI(69)=PARI(65)
      ENDIF
 
C...Store various other pieces of information into PARI.
      PARI(61)=VINT(148)
      PARI(75)=VINT(155)
      PARI(76)=VINT(156)
      PARI(77)=VINT(159)
      PARI(78)=VINT(160)
      PARI(81)=VINT(138)
 
C...Set information for LUTABU.
      IF(ISET(ISUB).EQ.1.OR.ISET(ISUB).EQ.3) THEN
        MSTU(161)=MINT(21)
        MSTU(162)=0
      ELSEIF(ISET(ISUB).EQ.5) THEN
        MSTU(161)=MINT(23)
        MSTU(162)=0
      ELSE
        MSTU(161)=MINT(21)
        MSTU(162)=MINT(22)
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYFRAM(IFRAME)
 
C...Performs transformations between different coordinate frames.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
C...Check that transformation can and should be done.
      IF(IFRAME.EQ.1.OR.IFRAME.EQ.2.OR.(IFRAME.EQ.3.AND.
     &MINT(91).EQ.1)) THEN
        IF(IFRAME.EQ.MINT(6)) RETURN
      ELSE
        WRITE(MSTU(11),5000) IFRAME,MINT(6)
        RETURN
      ENDIF
 
      IF(MINT(6).EQ.1) THEN
C...Transform from fixed target or user specified frame to
C...overall CM frame.
        CALL LUROBO(0.,0.,-VINT(8),-VINT(9),-VINT(10))
        CALL LUROBO(0.,-VINT(7),0.,0.,0.)
        CALL LUROBO(-VINT(6),0.,0.,0.,0.)
      ELSEIF(MINT(6).EQ.3) THEN
C...Transform from hadronic CM frame in DIS to overall CM frame.
        CALL LUROBO(-VINT(221),-VINT(222),-VINT(223),-VINT(224),
     &  -VINT(225))
      ENDIF
 
      IF(IFRAME.EQ.1) THEN
C...Transform from overall CM frame to fixed target or user specified
C...frame.
        CALL LUROBO(VINT(6),VINT(7),VINT(8),VINT(9),VINT(10))
      ELSEIF(IFRAME.EQ.3) THEN
C...Transform from overall CM frame to hadronic CM frame in DIS.
        CALL LUROBO(0.,0.,VINT(223),VINT(224),VINT(225))
        CALL LUROBO(0.,VINT(222),0.,0.,0.)
        CALL LUROBO(VINT(221),0.,0.,0.,0.)
      ENDIF
 
C...Set information about new frame.
      MINT(6)=IFRAME
      MSTI(6)=IFRAME
 
 5000 FORMAT(1X,'Error: illegal values in subroutine PYFRAM.',1X,
     &'No transformation performed.'/1X,'IFRAME =',1X,I5,'; MINT(6) =',
     &1X,I5)
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYWIDT(KFLR,SH,WDTP,WDTE)
 
C...Calculates full and partial widths of resonances.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
      COMMON/PYINT4/WIDP(21:40,0:40),WIDE(21:40,0:40),WIDS(21:40,3)
      REAL  WIDP,WIDE,WIDS
      SAVE /PYINT4/
      DIMENSION WDTP(0:40),WDTE(0:40,0:5),MOFSV(3,2),WIDWSV(3,2),
     &WID2SV(3,2)
      SAVE MOFSV,WIDWSV,WID2SV
      DATA MOFSV/6*0/,WIDWSV/6*0./,WID2SV/6*0./
 
C...Some common constants.
      KFLA=IABS(KFLR)
      KFHIGG=25
      IHIGG=1
      IF(KFLA.EQ.35.OR.KFLA.EQ.36) THEN
        KFHIGG=KFLA
        IHIGG=KFLA-33
      ENDIF
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2) XW=1.-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1.-XW
      AEM=ULALEM(SH)
      IF(MSTP(8).GE.1) AEM=SQRT(2.)*PARU(105)*PMAS(24,1)**2*XW/PARU(1)
      AS=ULALPS(SH)
      RADC=1.+AS/PARU(1)
 
C...Reset width information.
      DO 110 I=0,40
      WDTP(I)=0.
      DO 100 J=0,5
      WDTE(I,J)=0.
  100 CONTINUE
  110 CONTINUE
 
      IF(KFLA.EQ.6) THEN
C...t quark.
        DO 120 I=1,MDCY(6,3)
        IDC=I+MDCY(6,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 120
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 120
        IF(I.GE.4.AND.I.LE.7) THEN
C...t -> W + q.
          WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*VCKM(3,I-3)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &    ((1.-RM2)**2+(1.+RM2)*RM1-2.*RM1**2)
          IF(KFLR.GT.0) THEN
            WID2=WIDS(24,2)
            IF(I.EQ.7.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,2)
          ELSE
            WID2=WIDS(24,3)
            IF(I.EQ.7.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,3)
          ENDIF
        ELSEIF(I.EQ.9) THEN
C...t -> H + b.
          WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &    ((1.+RM2-RM1)*(RM2*PARU(141)**2+1./PARU(141)**2)+4.*RM2)
          WID2=WIDS(37,2)
          IF(KFLR.LT.0) WID2=WIDS(37,3)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  120   CONTINUE
 
      ELSEIF(KFLA.EQ.7) THEN
C...l or d* (masked as particle code 7).
        DO 130 I=1,MDCY(7,3)
        IDC=I+MDCY(7,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 130
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 130
        IF(MSTP(6).NE.1) THEN
          IF(I.GE.4.AND.I.LE.7) THEN
C...l -> W + q.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*VCKM(I-3,4)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.-RM2)**2+(1.+RM2)*RM1-2.*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,3)
              IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WID2*WIDS(26,2)
              IF(I.EQ.7.AND.MSTP(49).GE.1) WID2=WID2*WIDS(28,2)
            ELSE
              WID2=WIDS(24,2)
              IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WID2*WIDS(26,3)
              IF(I.EQ.7.AND.MSTP(49).GE.1) WID2=WID2*WIDS(28,3)
            ENDIF
            WID2=WIDS(24,3)
            IF(KFLR.LT.0) WID2=WIDS(24,2)
          ELSEIF(I.EQ.9.OR.I.EQ.10) THEN
C...l -> H + q.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.+RM2-RM1)*(PARU(141)**2+RM2/PARU(141)**2)+4.*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,3)
              IF(I.EQ.10.AND.MSTP(48).GE.1) WID2=WID2*WIDS(26,2)
            ELSE
              WID2=WIDS(37,2)
              IF(I.EQ.10.AND.MSTP(48).GE.1) WID2=WID2*WIDS(26,3)
            ENDIF
          ENDIF
        ELSE
          IF(I.EQ.1) THEN
C...d* -> g + d.
            WDTP(I)=AS*PARU(159)**2*SH/(3.*PARU(155)**2)
            WID2=1.
          ELSEIF(I.EQ.2) THEN
C...d* -> gamma + d.
            QF=-PARU(157)/2.+PARU(158)/6.
            WDTP(I)=AEM*QF**2*SH/(4.*PARU(155)**2)
            WID2=1.
          ELSEIF(I.EQ.3) THEN
C...d* -> Z0 + d.
            QF=-PARU(157)*XW1/2.-PARU(158)*XW/6.
            WDTP(I)=AEM*QF**2*SH/(8.*XW*XW1*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.4) THEN
C...d* -> W- + u.
            WDTP(I)=AEM*PARU(157)**2*SH/(16.*XW*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,3)
            IF(KFLR.LT.0) WID2=WIDS(24,2)
          ENDIF
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  130   CONTINUE
 
      ELSEIF(KFLA.EQ.8) THEN
C...h or u* (masked as particle code 8).
        DO 140 I=1,MDCY(8,3)
        IDC=I+MDCY(8,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 140
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 140
        IF(MSTP(6).NE.1) THEN
          IF(I.GE.4.AND.I.LE.7) THEN
C...h -> W + q.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*VCKM(4,I-3)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.-RM2)**2+(1.+RM2)*RM1-2.*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)
              IF(I.EQ.7.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,2)
            ELSE
              WID2=WIDS(24,3)
              IF(I.EQ.7.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,3)
            ENDIF
          ELSEIF(I.EQ.9.OR.I.EQ.10) THEN
C...h -> H + q.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.+RM2-RM1)*(RM2*PARU(141)**2+1./PARU(141)**2)+4.*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,2)
              IF(I.EQ.10.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,2)
            ELSE
              WID2=WIDS(37,3)
              IF(I.EQ.10.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,3)
            ENDIF
          ENDIF
        ELSE
          IF(I.EQ.1) THEN
C...u* -> g + u.
            WDTP(I)=AS*PARU(159)**2*SH/(3.*PARU(155)**2)
            WID2=1.
          ELSEIF(I.EQ.2) THEN
C...u* -> gamma + u.
            QF=PARU(157)/2.+PARU(158)/6.
            WDTP(I)=AEM*QF**2*SH/(4.*PARU(155)**2)
            WID2=1.
          ELSEIF(I.EQ.3) THEN
C...u* -> Z0 + u.
            QF=PARU(157)*XW1/2.-PARU(158)*XW/6.
            WDTP(I)=AEM*QF**2*SH/(8.*XW*XW1*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.4) THEN
C...u* -> W+ + d.
            WDTP(I)=AEM*PARU(157)**2*SH/(16.*XW*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)
          ENDIF
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  140   CONTINUE
 
      ELSEIF(KFLA.EQ.17) THEN
C...chi or e* (masked as particle code 17).
        DO 150 I=1,MDCY(17,3)
        IDC=I+MDCY(17,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 150
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 150
        IF(MSTP(6).NE.1) THEN
          IF(I.EQ.4) THEN
C...chi -> W + nu_chi.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.-RM2)**2+(1.+RM2)*RM1-2.*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,3)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(30,2)
            ELSE
              WID2=WIDS(24,2)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(30,3)
            ENDIF
          ELSEIF(I.EQ.6) THEN
C...chi -> H + nu_chi.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.+RM2-RM1)*(PARU(141)**2+RM2/PARU(141)**2)+4.*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,3)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(30,2)
            ELSE
              WID2=WIDS(37,2)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(30,3)
            ENDIF
          ENDIF
        ELSE
          IF(I.EQ.2) THEN
C...e* -> gamma + e.
            QF=-PARU(157)/2.-PARU(158)/2.
            WDTP(I)=AEM*QF**2*SH/(4.*PARU(155)**2)
            WID2=1.
          ELSEIF(I.EQ.3) THEN
C...e* -> Z0 + e.
            QF=-PARU(157)*XW1/2.+PARU(158)*XW/2.
            WDTP(I)=AEM*QF**2*SH/(8.*XW*XW1*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.4) THEN
C...e* -> W- + nu.
            WDTP(I)=AEM*PARU(157)**2*SH/(16.*XW*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,3)
            IF(KFLR.LT.0) WID2=WIDS(24,2)
          ENDIF
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  150   CONTINUE
 
      ELSEIF(KFLA.EQ.18) THEN
C...nu_chi or nu*_e (masked as particle code 18).
        DO 160 I=1,MDCY(18,3)
        IDC=I+MDCY(18,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 160
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 160
        IF(MSTP(6).NE.1) THEN
          IF(I.EQ.2) THEN
C...nu_chi -> W + chi.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.-RM2)**2+(1.+RM2)*RM1-2.*RM1**2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(24,2)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(29,2)
            ELSE
              WID2=WIDS(24,3)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(29,3)
            ENDIF
          ELSEIF(I.EQ.3) THEN
C...nu_chi -> H + chi.
            WDTP(I)=AEM*SH/(16.*PMAS(24,1)**2*XW)*
     &      SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))*
     &      ((1.+RM2-RM1)*(RM2*PARU(141)**2+1./PARU(141)**2)+4.*RM2)
            IF(KFLR.GT.0) THEN
              WID2=WIDS(37,2)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(29,2)
            ELSE
              WID2=WIDS(37,3)
              IF(MSTP(49).GE.1) WID2=WID2*WIDS(29,3)
            ENDIF
          ENDIF
        ELSE
          IF(I.EQ.1) THEN
C...nu*_e -> Z0 + nu*_e.
            QF=PARU(157)*XW1/2.+PARU(158)*XW/2.
            WDTP(I)=AEM*QF**2*SH/(8.*XW*XW1*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            WID2=WIDS(23,2)
          ELSEIF(I.EQ.2) THEN
C...nu*_e -> W+ + e.
            WDTP(I)=AEM*PARU(157)**2*SH/(16.*XW*PARU(155)**2)*
     &      (1.-RM1)**2*(2.+RM1)
            IF(KFLR.GT.0) WID2=WIDS(24,2)
            IF(KFLR.LT.0) WID2=WIDS(24,3)
          ENDIF
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  160   CONTINUE
 
      ELSEIF(KFLA.EQ.21) THEN
C...QCD:
        DO 170 I=1,MDCY(21,3)
        IDC=I+MDCY(21,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 170
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 170
        WID2=1.
        IF(I.LE.8) THEN
C...QCD -> q + q~
          WDTP(I)=(1.+2.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
          IF((I.EQ.7.OR.I.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(20+I,1)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  170   CONTINUE
 
      ELSEIF(KFLA.EQ.22) THEN
C...QED photon.
        DO 180 I=1,MDCY(22,3)
        IDC=I+MDCY(22,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 180
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 180
        WID2=1.
        IF(I.LE.8) THEN
C...QED -> q + q~.
          EF=KCHG(I,1)/3.
          FCOF=3.*RADC
          IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*PYHFTH(SH,SH*RM1,1.)
          WDTP(I)=FCOF*EF**2*(1.+2.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
          IF((I.EQ.7.OR.I.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(20+I,1)
        ELSEIF(I.LE.12) THEN
C...QED -> l+ + l-.
          EF=KCHG(9+2*(I-8),1)/3.
          WDTP(I)=EF**2*(1.+2.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(I.EQ.12.AND.MSTP(49).GE.1) WID2=WIDS(29,1)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  180   CONTINUE
 
      ELSEIF(KFLA.EQ.23) THEN
C...Z0:
        ICASE=1
        XWC=1./(16.*XW*XW1)
        FACH=AEM/3.*XWC*SH
  190   CONTINUE
        IF(MINT(61).GE.1.AND.ICASE.EQ.2) THEN
          VINT(111)=0.
          VINT(112)=0.
          VINT(114)=0.
        ENDIF
        IF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          EI=KCHG(IABS(MINT(15)),1)/3.
          AI=SIGN(1.,EI)
          VI=AI-4.*EI*XWV
          SQMZ=PMAS(23,1)**2
          HZ=FACH*WDTP(0)
          IF(MSTP(43).EQ.1.OR.MSTP(43).EQ.3) VINT(111)=1.
          IF(MSTP(43).EQ.3) VINT(112)=
     &    2.*XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(43).EQ.2.OR.MSTP(43).EQ.3) VINT(114)=
     &    XWC**2*SH**2/((SH-SQMZ)**2+HZ**2)
        ENDIF
        DO 200 I=1,MDCY(23,3)
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 200
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 200
        WID2=1.
        IF(I.LE.8) THEN
C...Z0 -> q + q~
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
          FCOF=3.*RADC
          IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*PYHFTH(SH,SH*RM1,1.)
          IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
          IF((I.EQ.7.OR.I.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(20+I,1)
        ELSEIF(I.LE.16) THEN
C...Z0 -> l+ + l-, nu + nu~
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
          FCOF=1.
          IF((I.EQ.15.OR.I.EQ.16).AND.MSTP(49).GE.1) WID2=WIDS(14+I,1)
        ENDIF
        BE34=SQRT(MAX(0.,1.-4.*RM1))
        IF(ICASE.EQ.1) THEN
          WDTP(I)=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          WDTP(I)=FCOF*((EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*
     &    EF*VF+(VI**2+AI**2)*VINT(114)*VF**2)*(1.+2.*RM1)+
     &    (VI**2+AI**2)*VINT(114)*AF**2*(1.-4.*RM1))*BE34
        ELSEIF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
          FGGF=FCOF*EF**2*(1.+2.*RM1)*BE34
          FGZF=FCOF*EF*VF*(1.+2.*RM1)*BE34
          FZZF=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ENDIF
        IF(ICASE.EQ.1) WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          IF((ICASE.EQ.1.AND.MINT(61).NE.1).OR.
     &    (ICASE.EQ.2.AND.MINT(61).EQ.1)) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
          IF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
            IF(MSTP(43).EQ.1.OR.MSTP(43).EQ.3) VINT(111)=
     &      VINT(111)+FGGF*WID2
            IF(MSTP(43).EQ.3) VINT(112)=VINT(112)+FGZF*WID2
            IF(MSTP(43).EQ.2.OR.MSTP(43).EQ.3) VINT(114)=
     &      VINT(114)+FZZF*WID2
          ENDIF
        ENDIF
  200   CONTINUE
        IF(MINT(61).GE.1) ICASE=3-ICASE
        IF(ICASE.EQ.2) GOTO 190
 
      ELSEIF(KFLA.EQ.24) THEN
C...W+/-:
        DO 210 I=1,MDCY(24,3)
        IDC=I+MDCY(24,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 210
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 210
        WID2=1.
        IF(I.LE.16) THEN
C...W+/- -> q + q~'
          FCOF=3.*RADC*VCKM((I-1)/4+1,MOD(I-1,4)+1)
          IF(KFLR.GT.0) THEN
            IF(MOD(I,4).EQ.3.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
            IF(MOD(I,4).EQ.0.AND.MSTP(49).GE.1) WID2=WIDS(28,2)
            IF(I.GE.13.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,3)
          ELSE
            IF(MOD(I,4).EQ.3.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
            IF(MOD(I,4).EQ.0.AND.MSTP(49).GE.1) WID2=WIDS(28,3)
            IF(I.GE.13.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,2)
          ENDIF
        ELSEIF(I.LE.20) THEN
C...W+/- -> l+/- + nu
          FCOF=1.
          IF(KFLR.GT.0) THEN
            IF(I.EQ.20.AND.MSTP(49).GE.1) WID2=WIDS(29,3)*WIDS(30,2)
          ELSE
            IF(I.EQ.20.AND.MSTP(49).GE.1) WID2=WIDS(29,2)*WIDS(30,3)
          ENDIF
        ENDIF
        WDTP(I)=FCOF*(2.-RM1-RM2-(RM1-RM2)**2)*
     &  SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  210   CONTINUE
 
      ELSEIF(KFLA.EQ.25.OR.KFLA.EQ.35.OR.KFLA.EQ.36) THEN
C...H0 (or H'0, or A0):
        DO 250 I=1,MDCY(KFHIGG,3)
        IDC=I+MDCY(KFHIGG,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 250
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(I.NE.16.AND.I.NE.17.AND.SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 250
        WID2=1.
 
        IF(I.LE.8) THEN
C...H0 -> q + q~
          WDTP(I)=3.*RM1*(1.-4.*RM1)*SQRT(MAX(0.,1.-4.*RM1))*RADC
          IF(MSTP(37).EQ.1.AND.MSTP(2).GE.1) WDTP(I)=WDTP(I)*
     &    (LOG(MAX(4.,PARP(37)**2*RM1*SH/PARU(117)**2))/
     &    LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
            IF(MOD(I,2).EQ.1) WDTP(I)=WDTP(I)*PARU(151+10*IHIGG)**2
            IF(MOD(I,2).EQ.0) WDTP(I)=WDTP(I)*PARU(152+10*IHIGG)**2
          ENDIF
          IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
          IF((I.EQ.7.OR.I.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(20+I,1)
 
        ELSEIF(I.LE.12) THEN
C...H0 -> l+ + l-
          WDTP(I)=RM1*(1.-4.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) WDTP(I)=WDTP(I)*
     &    PARU(153+10*IHIGG)**2
          IF(I.EQ.12.AND.MSTP(49).GE.1) WID2=WIDS(29,1)
 
        ELSEIF(I.EQ.13) THEN
C...H0 -> g + g; quark loop contribution only
          ETARE=0.
          ETAIM=0.
          DO 220 J=1,2*MSTP(1)
          EPS=(2.*PMAS(J,1))**2/SH
C...Loop integral; function of eps=4m^2/shat; different for A0.
          IF(EPS.LE.1.) THEN
            IF(EPS.GT.1.E-4) THEN
              ROOT=SQRT(1.-EPS)
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPS-2.)
            ENDIF
            PHIRE=-0.25*(RLN**2-PARU(1)**2)
            PHIIM=0.5*PARU(1)*RLN
          ELSE
            PHIRE=(ASIN(1./SQRT(EPS)))**2
            PHIIM=0.
          ENDIF
          IF(IHIGG.LE.2) THEN
            ETAREJ=-0.5*EPS*(1.+(1.-EPS)*PHIRE)
            ETAIMJ=-0.5*EPS*(1.-EPS)*PHIIM
          ELSE
            ETAREJ=-0.5*EPS*PHIRE
            ETAIMJ=-0.5*EPS*PHIIM
          ENDIF
C...Couplings (=1 for standard model Higgs).
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
            IF(MOD(J,2).EQ.1) THEN
              ETAREJ=ETAREJ*PARU(151+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(151+10*IHIGG)
            ELSE
              ETAREJ=ETAREJ*PARU(152+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(152+10*IHIGG)
            ENDIF
          ENDIF
          ETARE=ETARE+ETAREJ
          ETAIM=ETAIM+ETAIMJ
  220     CONTINUE
          ETA2=ETARE**2+ETAIM**2
          WDTP(I)=(AS/PARU(1))**2*ETA2
 
        ELSEIF(I.EQ.14) THEN
C...H0 -> gamma + gamma; quark, lepton, W+- and H+- loop contributions
          ETARE=0.
          ETAIM=0.
          JMAX=3*MSTP(1)+1
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) JMAX=JMAX+1
          DO 230 J=1,JMAX
          IF(J.LE.2*MSTP(1)) THEN
            EJ=KCHG(J,1)/3.
            EPS=(2.*PMAS(J,1))**2/SH
          ELSEIF(J.LE.3*MSTP(1)) THEN
            JL=2*(J-2*MSTP(1))-1
            EJ=KCHG(10+JL,1)/3.
            EPS=(2.*PMAS(10+JL,1))**2/SH
          ELSEIF(J.EQ.3*MSTP(1)+1) THEN
            EPS=(2.*PMAS(24,1))**2/SH
          ELSE
            EPS=(2.*PMAS(37,1))**2/SH
          ENDIF
C...Loop integral; function of eps=4m^2/shat.
          IF(EPS.LE.1.) THEN
            IF(EPS.GT.1.E-4) THEN
              ROOT=SQRT(1.-EPS)
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPS-2.)
            ENDIF
            PHIRE=-0.25*(RLN**2-PARU(1)**2)
            PHIIM=0.5*PARU(1)*RLN
          ELSE
            PHIRE=(ASIN(1./SQRT(EPS)))**2
            PHIIM=0.
          ENDIF
          IF(J.LE.3*MSTP(1)) THEN
C...Fermion loops: loop integral different for A0; charges.
            IF(IHIGG.LE.2) THEN
              PHIPRE=-0.5*EPS*(1.+(1.-EPS)*PHIRE)
              PHIPIM=-0.5*EPS*(1.-EPS)*PHIIM
            ELSE
              PHIPRE=-0.5*EPS*PHIRE
              PHIPIM=-0.5*EPS*PHIIM
            ENDIF
            IF(J.LE.2*MSTP(1).AND.MOD(J,2).EQ.1) THEN
              EJC=3.*EJ**2
              EJH=PARU(151+10*IHIGG)
            ELSEIF(J.LE.2*MSTP(1)) THEN
              EJC=3.*EJ**2
              EJH=PARU(152+10*IHIGG)
            ELSE
              EJC=EJ**2
              EJH=PARU(153+10*IHIGG)
            ENDIF
            IF(MSTP(4).EQ.0.AND.IHIGG.EQ.1) EJH=1.
            ETAREJ=EJC*EJH*PHIPRE
            ETAIMJ=EJC*EJH*PHIPIM
          ELSEIF(J.EQ.3*MSTP(1)+1) THEN
C...W loops: loop integral and charges.
            ETAREJ=0.5+0.75*EPS*(1.+(2.-EPS)*PHIRE)
            ETAIMJ=0.75*EPS*(2.-EPS)*PHIIM
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(155+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(155+10*IHIGG)
            ENDIF
          ELSE
C...Charged H loops: loop integral and charges.
            FACHHH=(PMAS(24,1)/PMAS(37,1))**2*
     &      PARU(158+10*IHIGG+2*(IHIGG/3))
            ETAREJ=EPS*(1.-EPS*PHIRE)*FACHHH
            ETAIMJ=-EPS**2*PHIIM*FACHHH
          ENDIF
          ETARE=ETARE+ETAREJ
          ETAIM=ETAIM+ETAIMJ
  230     CONTINUE
          ETA2=ETARE**2+ETAIM**2
          WDTP(I)=(AEM/PARU(1))**2*0.5*ETA2
 
        ELSEIF(I.EQ.15) THEN
C...H0 -> gamma + Z0; quark, lepton, W and H+- loop contributions
          ETARE=0.
          ETAIM=0.
          JMAX=3*MSTP(1)+1
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) JMAX=JMAX+1
          DO 240 J=1,JMAX
          IF(J.LE.2*MSTP(1)) THEN
            EJ=KCHG(J,1)/3.
            AJ=SIGN(1.,EJ+0.1)
            VJ=AJ-4.*EJ*XWV
            EPS=(2.*PMAS(J,1))**2/SH
            EPSP=(2.*PMAS(J,1)/PMAS(23,1))**2
          ELSEIF(J.LE.3*MSTP(1)) THEN
            JL=2*(J-2*MSTP(1))-1
            EJ=KCHG(10+JL,1)/3.
            AJ=SIGN(1.,EJ+0.1)
            VJ=AJ-4.*EJ*XWV
            EPS=(2.*PMAS(10+JL,1))**2/SH
            EPSP=(2.*PMAS(10+JL,1)/PMAS(23,1))**2
          ELSE
            EPS=(2.*PMAS(24,1))**2/SH
            EPSP=(2.*PMAS(24,1)/PMAS(23,1))**2
          ENDIF
C...Loop integrals; functions of eps=4m^2/shat and eps'=4m^2/m_Z^2.
          IF(EPS.LE.1.) THEN
            ROOT=SQRT(1.-EPS)
            IF(EPS.GT.1.E-4) THEN
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPS-2.)
            ENDIF
            PHIRE=-0.25*(RLN**2-PARU(1)**2)
            PHIIM=0.5*PARU(1)*RLN
            PSIRE=0.5*ROOT*RLN
            PSIIM=-0.5*ROOT*PARU(1)
          ELSE
            PHIRE=(ASIN(1./SQRT(EPS)))**2
            PHIIM=0.
            PSIRE=SQRT(EPS-1.)*ASIN(1./SQRT(EPS))
            PSIIM=0.
          ENDIF
          IF(EPSP.LE.1.) THEN
            ROOT=SQRT(1.-EPSP)
            IF(EPSP.GT.1.E-4) THEN
              RLN=LOG((1.+ROOT)/(1.-ROOT))
            ELSE
              RLN=LOG(4./EPSP-2.)
            ENDIF
            PHIREP=-0.25*(RLN**2-PARU(1)**2)
            PHIIMP=0.5*PARU(1)*RLN
            PSIREP=0.5*ROOT*RLN
            PSIIMP=-0.5*ROOT*PARU(1)
          ELSE
            PHIREP=(ASIN(1./SQRT(EPSP)))**2
            PHIIMP=0.
            PSIREP=SQRT(EPSP-1.)*ASIN(1./SQRT(EPSP))
            PSIIMP=0.
          ENDIF
          FXYRE=EPS*EPSP/(8.*(EPS-EPSP))*(1.+EPS*EPSP/(EPS-EPSP)*(PHIRE-
     &    PHIREP)+2.*EPS/(EPS-EPSP)*(PSIRE-PSIREP))
          FXYIM=EPS**2*EPSP/(8.*(EPS-EPSP)**2)*(EPSP*(PHIIM-PHIIMP)+
     &    2.*(PSIIM-PSIIMP))
          F1RE=-EPS*EPSP/(2.*(EPS-EPSP))*(PHIRE-PHIREP)
          F1IM=-EPS*EPSP/(2.*(EPS-EPSP))*(PHIIM-PHIIMP)
          IF(J.LE.3*MSTP(1)) THEN
C...Fermion loops: loop integral different for A0; charges.
            IF(IHIGG.EQ.3) FXYRE=0.
            IF(IHIGG.EQ.3) FXYIM=0.
            IF(J.LE.2*MSTP(1).AND.MOD(J,2).EQ.1) THEN
              EJC=-3.*EJ*VJ
              EJH=PARU(151+10*IHIGG)
            ELSEIF(J.LE.2*MSTP(1)) THEN
              EJC=-3.*EJ*VJ
              EJH=PARU(152+10*IHIGG)
            ELSE
              EJC=-EJ*VJ
              EJH=PARU(153+10*IHIGG)
            ENDIF
            IF(MSTP(4).EQ.0.AND.IHIGG.EQ.1) EJH=1.
            ETAREJ=EJC*EJH*(FXYRE-0.25*F1RE)
            ETAIMJ=EJC*EJH*(FXYIM-0.25*F1IM)
          ELSEIF(J.EQ.3*MSTP(1)+1) THEN
C...W loops: loop integral and charges.
            HEPS=(1.+2./EPS)*XW/XW1-(5.+2./EPS)
            ETAREJ=-XW1*((3.-XW/XW1)*F1RE+HEPS*FXYRE)
            ETAIMJ=-XW1*((3.-XW/XW1)*F1IM+HEPS*FXYIM)
            IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
              ETAREJ=ETAREJ*PARU(155+10*IHIGG)
              ETAIMJ=ETAIMJ*PARU(155+10*IHIGG)
            ENDIF
          ELSE
C...Charged H loops: loop integral and charges.
            FACHHH=(PMAS(24,1)/PMAS(37,1))**2*(1.-2.*XW)*
     &      PARU(158+10*IHIGG+2*(IHIGG/3))
            ETAREJ=FACHHH*FXYRE
            ETAIMJ=FACHHH*FXYIM
          ENDIF
          ETARE=ETARE+ETAREJ
          ETAIM=ETAIM+ETAIMJ
  240     CONTINUE
          ETA2=(ETARE**2+ETAIM**2)/(XW*XW1)
          WDTP(I)=(AEM/PARU(1))**2*(1.-PMAS(23,1)**2/SH)**3*ETA2
          WID2=WIDS(23,2)
 
        ELSEIF(I.LE.17) THEN
C...H0 -> Z0 + Z0, W+ + W-
          PM1=PMAS(IABS(KFDP(IDC,1)),1)
          PG1=PMAS(IABS(KFDP(IDC,1)),2)
          IF(MINT(62).GE.1) THEN
            IF(MSTP(42).EQ.0.OR.(4.*(PM1+10.*PG1)**2.LT.SH.AND.
     &      CKIN(46).LT.CKIN(45).AND.CKIN(48).LT.CKIN(47).AND.
     &      MAX(CKIN(45),CKIN(47)).LT.PM1-10.*PG1)) THEN
              MOFSV(IHIGG,I-15)=0
              WIDW=(1.-4.*RM1+12.*RM1**2)*SQRT(MAX(0.,1.-4.*RM1))
              WID2=1.
            ELSE
              MOFSV(IHIGG,I-15)=1
              RMAS=SQRT(MAX(0.,SH))
              CALL PYOFSH(1,KFLA,KFDP(IDC,1),KFDP(IDC,2),RMAS,WIDW,WID2)
              WIDWSV(IHIGG,I-15)=WIDW
              WID2SV(IHIGG,I-15)=WID2
            ENDIF
          ELSE
            IF(MOFSV(IHIGG,I-15).EQ.0) THEN
              WIDW=(1.-4.*RM1+12.*RM1**2)*SQRT(MAX(0.,1.-4.*RM1))
              WID2=1.
            ELSE
              WIDW=WIDWSV(IHIGG,I-15)
              WID2=WID2SV(IHIGG,I-15)
            ENDIF
          ENDIF
          WDTP(I)=WIDW/(2.*(18-I))
          IF(MSTP(4).GE.1.OR.IHIGG.GE.2) WDTP(I)=WDTP(I)*
     &    PARU(138+I+10*IHIGG)**2
          WID2=WID2*WIDS(7+I,1)
 
        ELSEIF(I.EQ.18.AND.KFLA.EQ.35) THEN
C***H'0 -> Z0 + H0 (not yet implemented).
 
        ELSEIF(I.EQ.19.AND.KFLA.EQ.35) THEN
C...H'0 -> H0 + H0.
          WDTP(I)=PARU(176)**2*0.25*PMAS(23,1)**4/SH**2*
     &    SQRT(MAX(0.,1.-4.*RM1))
          WID2=WIDS(25,2)**2
 
        ELSEIF(I.EQ.20.AND.KFLA.EQ.35) THEN
C...H'0 -> A0 + A0.
          WDTP(I)=PARU(177)**2*0.25*PMAS(23,1)**4/SH**2*
     &    SQRT(MAX(0.,1.-4.*RM1))
          WID2=WIDS(36,2)**2
 
        ELSEIF(I.EQ.18.AND.KFLA.EQ.36) THEN
C...A0 -> Z0 + H0.
          WDTP(I)=PARU(186)**2*0.5*SQRT(MAX(0.,(1.-RM1-RM2)**2-
     &    4.*RM1*RM2))**3
          WID2=WIDS(23,2)*WIDS(25,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  250   CONTINUE
 
      ELSEIF(KFLA.EQ.32) THEN
C...Z'0:
        ICASE=1
        XWC=1./(16.*XW*XW1)
        FACH=AEM/3.*XWC*SH
        VINT(117)=0.
  260   CONTINUE
        IF(MINT(61).GE.1.AND.ICASE.EQ.2) THEN
          VINT(111)=0.
          VINT(112)=0.
          VINT(113)=0.
          VINT(114)=0.
          VINT(115)=0.
          VINT(116)=0.
        ENDIF
        IF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
          KFAI=IABS(MINT(15))
          EI=KCHG(KFAI,1)/3.
          AI=SIGN(1.,EI+0.1)
          VI=AI-4.*EI*XWV
          KFAIC=1
          IF(KFAI.LE.10.AND.MOD(KFAI,2).EQ.0) KFAIC=2
          IF(KFAI.GT.10.AND.MOD(KFAI,2).NE.0) KFAIC=3
          IF(KFAI.GT.10.AND.MOD(KFAI,2).EQ.0) KFAIC=4
          VPI=PARU(119+2*KFAIC)
          API=PARU(120+2*KFAIC)
          SQMZ=PMAS(23,1)**2
          HZ=FACH*VINT(117)
          SQMZP=PMAS(32,1)**2
          HZP=FACH*WDTP(0)
          IF(MSTP(44).EQ.1.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.5.OR.
     &    MSTP(44).EQ.7) VINT(111)=1.
          IF(MSTP(44).EQ.4.OR.MSTP(44).EQ.7) VINT(112)=
     &    2.*XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(44).EQ.5.OR.MSTP(44).EQ.7) VINT(113)=
     &    2.*XWC*SH*(SH-SQMZP)/((SH-SQMZP)**2+HZP**2)
          IF(MSTP(44).EQ.2.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.6.OR.
     &    MSTP(44).EQ.7) VINT(114)=XWC**2*SH**2/((SH-SQMZ)**2+HZ**2)
          IF(MSTP(44).EQ.6.OR.MSTP(44).EQ.7) VINT(115)=
     &    2.*XWC**2*SH**2*((SH-SQMZ)*(SH-SQMZP)+HZ*HZP)/
     &    (((SH-SQMZ)**2+HZ**2)*((SH-SQMZP)**2+HZP**2))
          IF(MSTP(44).EQ.3.OR.MSTP(44).EQ.5.OR.MSTP(44).EQ.6.OR.
     &    MSTP(44).EQ.7) VINT(116)=XWC**2*SH**2/((SH-SQMZP)**2+HZP**2)
        ENDIF
        DO 270 I=1,MDCY(32,3)
        IDC=I+MDCY(32,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 270
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1..OR.MDME(IDC,1).LT.0) GOTO 270
        WID2=1.
        IF(I.LE.16) THEN
          IF(I.LE.8) THEN
C...Z'0 -> q + q~
            EF=KCHG(I,1)/3.
            AF=SIGN(1.,EF+0.1)
            VF=AF-4.*EF*XWV
            VPF=PARU(123-2*MOD(I,2))
            APF=PARU(124-2*MOD(I,2))
            FCOF=3.*RADC
            IF(I.GE.6.AND.MSTP(35).GE.1) FCOF=FCOF*PYHFTH(SH,SH*RM1,1.)
            IF(I.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
            IF((I.EQ.7.OR.I.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(20+I,1)
          ELSEIF(I.LE.16) THEN
C...Z'0 -> l+ + l-, nu + nu~
            EF=KCHG(I+2,1)/3.
            AF=SIGN(1.,EF+0.1)
            VF=AF-4.*EF*XWV
            VPF=PARU(127-2*MOD(I,2))
            APF=PARU(128-2*MOD(I,2))
            FCOF=1.
            IF((I.EQ.15.OR.I.EQ.16).AND.MSTP(49).GE.1) WID2=WIDS(14+I,1)
          ENDIF
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(ICASE.EQ.1) THEN
            WDTPZ=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
            WDTP(I)=FCOF*(VPF**2*(1.+2.*RM1)+APF**2*(1.-4.*RM1))*BE34
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=FCOF*((EI**2*VINT(111)*EF**2+EI*VI*VINT(112)*
     &      EF*VF+EI*VPI*VINT(113)*EF*VPF+(VI**2+AI**2)*VINT(114)*
     &      VF**2+(VI*VPI+AI*API)*VINT(115)*VF*VPF+(VPI**2+API**2)*
     &      VINT(116)*VPF**2)*(1.+2.*RM1)+((VI**2+AI**2)*VINT(114)*
     &      AF**2+(VI*VPI+AI*API)*VINT(115)*AF*APF+(VPI**2+API**2)*
     &      VINT(116)*APF**2)*(1.-4.*RM1))*BE34
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=FCOF*EF**2*(1.+2.*RM1)*BE34
            FGZF=FCOF*EF*VF*(1.+2.*RM1)*BE34
            FGZPF=FCOF*EF*VPF*(1.+2.*RM1)*BE34
            FZZF=FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
            FZZPF=FCOF*(VF*VPF*(1.+2.*RM1)+AF*APF*(1.-4.*RM1))*BE34
            FZPZPF=FCOF*(VPF**2*(1.+2.*RM1)+APF**2*(1.-4.*RM1))*BE34
          ENDIF
        ELSEIF(I.EQ.17) THEN
C...Z'0 -> W+ + W-
          WDTPZP=PARU(129)**2*XW1**2*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3*
     &    (1.+10.*RM1+10.*RM2+RM1**2+RM2**2+10.*RM1*RM2)
          IF(ICASE.EQ.1) THEN
            WDTPZ=0.
            WDTP(I)=WDTPZP
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=(VPI**2+API**2)*VINT(116)*WDTPZP
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.
            FGZF=0.
            FGZPF=0.
            FZZF=0.
            FZZPF=0.
            FZPZPF=WDTPZP
          ENDIF
          WID2=WIDS(24,1)
        ELSEIF(I.EQ.18) THEN
C...Z'0 -> H+ + H-
          CZC=2.*(1.-2.*XW)
          BE34C=(1.-4.*RM1)*SQRT(MAX(0.,1.-4.*RM1))
          IF(ICASE.EQ.1) THEN
            WDTPZ=0.25*PARU(142)**2*CZC**2*BE34C
            WDTP(I)=0.25*PARU(143)**2*CZC**2*BE34C
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=0.25*(EI**2*VINT(111)+PARU(142)*EI*VI*VINT(112)*
     &      CZC+PARU(143)*EI*VPI*VINT(113)*CZC+PARU(142)**2*
     &      (VI**2+AI**2)*VINT(114)*CZC**2+PARU(142)*PARU(143)*
     &      (VI*VPI+AI*API)*VINT(115)*CZC**2+PARU(143)**2*
     &      (VPI**2+API**2)*VINT(116)*CZC**2)*BE34C
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.25*BE34C
            FGZF=0.25*PARU(142)*CZC*BE34C
            FGZPF=0.25*PARU(143)*CZC*BE34C
            FZZF=0.25*PARU(142)**2*CZC**2*BE34C
            FZZPF=0.25*PARU(142)*PARU(143)*CZC**2*BE34C
            FZPZPF=0.25*PARU(143)**2*CZC**2*BE34C
          ENDIF
          WID2=WIDS(37,1)
        ELSEIF(I.EQ.19) THEN
C...Z'0 -> Z0 + gamma.
        ELSEIF(I.EQ.20) THEN
C...Z'0 -> Z0 + H0
          FLAM=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WDTPZP=PARU(145)**2*4.*ABS(1.-2.*XW)*(3.*RM1+0.25*FLAM**2)*
     &    FLAM
          IF(ICASE.EQ.1) THEN
            WDTPZ=0.
            WDTP(I)=WDTPZP
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=(VPI**2+API**2)*VINT(116)*WDTPZP
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.
            FGZF=0.
            FGZPF=0.
            FZZF=0.
            FZZPF=0.
            FZPZPF=WDTPZP
          ENDIF
          WID2=WIDS(23,2)*WIDS(25,2)
        ELSEIF(I.EQ.21.OR.I.EQ.22) THEN
C...Z' -> H0 + A0 or H'0 + A0.
          BE34C=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3
          IF(I.EQ.21) THEN
            CZAH=PARU(186)
            CZPAH=PARU(188)
          ELSE
            CZAH=PARU(187)
            CZPAH=PARU(189)
          ENDIF
          IF(ICASE.EQ.1) THEN
            WDTPZ=CZAH**2*BE34C
            WDTP(I)=CZPAH**2*BE34C
          ELSEIF(MINT(61).EQ.1.AND.ICASE.EQ.2) THEN
            WDTP(I)=(CZAH**2*(VI**2+AI**2)*VINT(114)+CZAH*CZPAH*
     &      (VI*VPI+AI*API)*VINT(115)+CZPAH**2*(VPI**2+API**2)*
     &      VINT(116))*BE34C
          ELSEIF(MINT(61).EQ.2) THEN
            FGGF=0.
            FGZF=0.
            FGZPF=0.
            FZZF=CZAH**2*BE34C
            FZZPF=CZAH*CZPAH*BE34C
            FZPZPF=CZPAH**2*BE34C
          ENDIF
          IF(I.EQ.21) WID2=WIDS(25,2)*WIDS(36,2)
          IF(I.EQ.22) WID2=WIDS(35,2)*WIDS(36,2)
        ENDIF
        IF(ICASE.EQ.1) THEN
          VINT(117)=VINT(117)+WDTPZ
          WDTP(0)=WDTP(0)+WDTP(I)
        ENDIF
        IF(MDME(IDC,1).GT.0) THEN
          IF((ICASE.EQ.1.AND.MINT(61).NE.1).OR.
     &    (ICASE.EQ.2.AND.MINT(61).EQ.1)) THEN
            WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
            WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
            WDTE(I,0)=WDTE(I,MDME(IDC,1))
            WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
          ENDIF
          IF(MINT(61).EQ.2.AND.ICASE.EQ.2) THEN
            IF(MSTP(44).EQ.1.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.5.OR.
     &      MSTP(44).EQ.7) VINT(111)=VINT(111)+FGGF*WID2
            IF(MSTP(44).EQ.4.OR.MSTP(44).EQ.7) VINT(112)=VINT(112)+
     &      FGZF*WID2
            IF(MSTP(44).EQ.5.OR.MSTP(44).EQ.7) VINT(113)=VINT(113)+
     &      FGZPF*WID2
            IF(MSTP(44).EQ.2.OR.MSTP(44).EQ.4.OR.MSTP(44).EQ.6.OR.
     &      MSTP(44).EQ.7) VINT(114)=VINT(114)+FZZF*WID2
            IF(MSTP(44).EQ.6.OR.MSTP(44).EQ.7) VINT(115)=VINT(115)+
     &      FZZPF*WID2
            IF(MSTP(44).EQ.3.OR.MSTP(44).EQ.5.OR.MSTP(44).EQ.6.OR.
     &      MSTP(44).EQ.7) VINT(116)=VINT(116)+FZPZPF*WID2
          ENDIF
        ENDIF
  270   CONTINUE
        IF(MINT(61).GE.1) ICASE=3-ICASE
        IF(ICASE.EQ.2) GOTO 260
 
      ELSEIF(KFLA.EQ.34) THEN
C...W'+/-:
        DO 280 I=1,MDCY(34,3)
        IDC=I+MDCY(34,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 280
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 280
        WID2=1.
        IF(I.LE.20) THEN
          IF(I.LE.16) THEN
C...W'+/- -> q + q~'
            FCOF=3.*RADC*(PARU(131)**2+PARU(132)**2)*
     &      VCKM((I-1)/4+1,MOD(I-1,4)+1)
            IF(KFLR.GT.0) THEN
              IF(MOD(I,4).EQ.3.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
              IF(MOD(I,4).EQ.0.AND.MSTP(49).GE.1) WID2=WIDS(28,2)
              IF(I.GE.13.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,3)
            ELSE
              IF(MOD(I,4).EQ.3.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
              IF(MOD(I,4).EQ.0.AND.MSTP(49).GE.1) WID2=WIDS(28,3)
              IF(I.GE.13.AND.MSTP(49).GE.1) WID2=WID2*WIDS(27,2)
            ENDIF
          ELSEIF(I.LE.20) THEN
C...W'+/- -> l+/- + nu
            FCOF=PARU(133)**2+PARU(134)**2
            IF(KFLR.GT.0) THEN
              IF(I.EQ.20.AND.MSTP(49).GE.1) WID2=WIDS(29,3)*WIDS(30,2)
            ELSE
              IF(I.EQ.20.AND.MSTP(49).GE.1) WID2=WIDS(29,2)*WIDS(30,3)
            ENDIF
          ENDIF
          WDTP(I)=FCOF*0.5*(2.-RM1-RM2-(RM1-RM2)**2)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
        ELSEIF(I.EQ.21) THEN
C...W'+/- -> W+/- + Z0
          WDTP(I)=PARU(135)**2*0.5*XW1*(RM1/RM2)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3*
     &    (1.+10.*RM1+10.*RM2+RM1**2+RM2**2+10.*RM1*RM2)
          IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(23,2)
          IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(23,2)
        ELSEIF(I.EQ.23) THEN
C...W'+/- -> W+/- + H0
          FLAM=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          WDTP(I)=PARU(146)**2*2.*(3.*RM1+0.25*FLAM**2)*FLAM
          IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(25,2)
          IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(25,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  280   CONTINUE
 
      ELSEIF(KFLA.EQ.37) THEN
C...H+/-:
        DO 290 I=1,MDCY(37,3)
        IDC=I+MDCY(37,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 290
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 290
        WID2=1.
        IF(I.LE.4) THEN
C...H+/- -> q + q~'
          RM1R=RM1
          IF(MSTP(37).EQ.1.AND.MSTP(2).GE.1) RM1R=RM1*
     &    (LOG(MAX(4.,PARP(37)**2*RM1*SH/PARU(117)**2))/
     &    LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
          WDTP(I)=3.*RADC*((RM1R*PARU(141)**2+RM2/PARU(141)**2)*
     &    (1.-RM1R-RM2)-4.*RM1R*RM2)*
     &    SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          IF(KFLR.GT.0) THEN
            IF(I.EQ.3.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
            IF(I.EQ.4.AND.MSTP(49).GE.1) WID2=WIDS(27,3)*WIDS(28,2)
          ELSE
            IF(I.EQ.3.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
            IF(I.EQ.4.AND.MSTP(49).GE.1) WID2=WIDS(27,2)*WIDS(28,3)
          ENDIF
        ELSEIF(I.LE.8) THEN
C...H+/- -> l+/- + nu
          WDTP(I)=((RM1*PARU(141)**2+RM2/PARU(141)**2)*(1.-RM1-RM2)-
     &    4.*RM1*RM2)*SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
          IF(KFLR.GT.0) THEN
            IF(I.EQ.8.AND.MSTP(49).GE.1) WID2=WIDS(29,3)*WIDS(30,2)
          ELSE
            IF(I.EQ.8.AND.MSTP(49).GE.1) WID2=WIDS(29,2)*WIDS(30,3)
          ENDIF
        ELSEIF(I.EQ.9) THEN
C...H+/- -> W+/- + H0.
          WDTP(I)=PARU(195)**2*0.5*SQRT(MAX(0.,(1.-RM1-RM2)**2-
     &    4.*RM1*RM2))**3
          IF(KFLR.GT.0) WID2=WIDS(24,2)*WIDS(25,2)
          IF(KFLR.LT.0) WID2=WIDS(24,3)*WIDS(25,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  290   CONTINUE
 
      ELSEIF(KFLA.EQ.38) THEN
C...Techni-eta.
        DO 300 I=1,MDCY(38,3)
        IDC=I+MDCY(38,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 300
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 300
        WID2=1.
        IF(I.LE.2) THEN
          WDTP(I)=RM1*SH*SQRT(MAX(0.,1.-4.*RM1))/
     &    (4.*PARU(1)*PARP(46)**2)
          IF(I.EQ.2.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        ELSE
          WDTP(I)=5.*AS**2*SH/(96.*PARU(1)**3*PARP(46)**2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  300   CONTINUE
 
      ELSEIF(KFLA.EQ.39) THEN
C...LQ (leptoquark).
        DO 310 I=1,MDCY(39,3)
        IDC=I+MDCY(39,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 310
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 310
        WDTP(I)=PARU(151)*SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))**3
        WID2=1.
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  310   CONTINUE
 
      ELSEIF(KFLA.EQ.40) THEN
C...R:
        DO 320 I=1,MDCY(40,3)
        IDC=I+MDCY(40,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 320
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SH
        RM2=PMAS(IABS(KFDP(IDC,2)),1)**2/SH
        IF(SQRT(RM1)+SQRT(RM2).GT.1.) GOTO 320
        WID2=1.
        IF(I.LE.6) THEN
C...R -> q + q~'
          FCOF=3.*RADC
        ELSEIF(I.LE.9) THEN
C...R -> l+ + l'-
          FCOF=1.
        ENDIF
        WDTP(I)=FCOF*(2.-RM1-RM2-(RM1-RM2)**2)*
     &  SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
        IF(KFLR.GT.0) THEN
          IF(I.EQ.4.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
          IF(I.EQ.5.AND.MSTP(49).GE.1) WID2=WIDS(27,3)
          IF(I.EQ.6.AND.MSTP(49).GE.1) WID2=WIDS(26,2)*WIDS(28,3)
          IF(I.EQ.9.AND.MSTP(49).GE.1) WID2=WIDS(29,3)
        ELSE
          IF(I.EQ.4.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
          IF(I.EQ.5.AND.MSTP(49).GE.1) WID2=WIDS(27,2)
          IF(I.EQ.6.AND.MSTP(49).GE.1) WID2=WIDS(26,3)*WIDS(28,2)
          IF(I.EQ.9.AND.MSTP(49).GE.1) WID2=WIDS(29,2)
        ENDIF
        WDTP(0)=WDTP(0)+WDTP(I)
        IF(MDME(IDC,1).GT.0) THEN
          WDTE(I,MDME(IDC,1))=WDTP(I)*WID2
          WDTE(0,MDME(IDC,1))=WDTE(0,MDME(IDC,1))+WDTE(I,MDME(IDC,1))
          WDTE(I,0)=WDTE(I,MDME(IDC,1))
          WDTE(0,0)=WDTE(0,0)+WDTE(I,0)
        ENDIF
  320   CONTINUE
 
      ENDIF
      MINT(61)=0
      MINT(62)=0
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYOFSH(MOFSH,KFMO,KFD1,KFD2,PMMO,RET1,RET2)
 
C...Calculates partial width and differential cross-section maxima
C...of channels/processes not allowed on mass-shell, and selects
C...masses in such channels/processes.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
      COMMON/PYINT5/NGEN(0:200,3),XSEC(0:200,3)
      INTEGER  NGEN
      REAL  XSEC
      SAVE /PYINT5/
      DIMENSION KFD(2),MBW(2),PMD(2),PGD(2),PMG(2),PML(2),PMU(2),
     &PMH(2),ATL(2),ATU(2),ATH(2),RMG(2),INX1(100),XPT1(100),
     &FPT1(100),INX2(100),XPT2(100),FPT2(100),WDTP(0:40),
     &WDTE(0:40,0:5)
 
C...Find if particles equal, maximum mass, matrix elements, etc.
      MINT(51)=0
      ISUB=MINT(1)
      KFD(1)=IABS(KFD1)
      KFD(2)=IABS(KFD2)
      MEQL=0
      IF(KFD(1).EQ.KFD(2)) MEQL=1
      MLM=0
      IF(MOFSH.GE.2.AND.MEQL.EQ.1) MLM=INT(1.5+RLU(0))
      IF(MOFSH.LE.2.OR.MOFSH.EQ.7) THEN
        NOFF=44
        PMMX=PMMO
      ELSE
        NOFF=40
        PMMX=VINT(1)
        IF(CKIN(2).GT.CKIN(1)) PMMX=MIN(CKIN(2),VINT(1))
      ENDIF
      MMED=0
      IF((KFMO.EQ.25.OR.KFMO.EQ.35.OR.KFMO.EQ.36).AND.MEQL.EQ.1.AND.
     &(KFD(1).EQ.23.OR.KFD(1).EQ.24)) MMED=1
      IF((KFMO.EQ.32.OR.IABS(KFMO).EQ.34).AND.(KFD(1).EQ.23.OR.
     &KFD(1).EQ.24).AND.(KFD(2).EQ.23.OR.KFD(2).EQ.24)) MMED=2
      IF((KFMO.EQ.32.OR.IABS(KFMO).EQ.34).AND.(KFD(2).EQ.25.OR.
     &KFD(2).EQ.35.OR.KFD(2).EQ.36)) MMED=3
      LOOP=1
 
C...Find where Breit-Wigners are required, else select discrete masses.
  100 DO 110 I=1,2
      KFCA=KFD(I)
      IF(KFCA.GT.100) KFCA=LUCOMP(KFCA)
      IF(KFCA.GT.0) THEN
        PMD(I)=PMAS(KFCA,1)
        PGD(I)=PMAS(KFCA,2)
      ELSE
        PMD(I)=0.
        PGD(I)=0.
      ENDIF
      IF(MSTP(42).LE.0.OR.PGD(I).LT.PARP(41)) THEN
        MBW(I)=0
        PMG(I)=PMD(I)
        RMG(I)=(PMG(I)/PMMX)**2
      ELSE
        MBW(I)=1
      ENDIF
  110 CONTINUE
 
C...Find allowed mass range and Breit-Wigner parameters.
      DO 120 I=1,2
      IF(MOFSH.EQ.1.AND.LOOP.EQ.1.AND.MBW(I).EQ.1) THEN
        PML(I)=PARP(42)
        PMU(I)=PMMX-PARP(42)
        IF(MBW(3-I).EQ.0) PMU(I)=MIN(PMU(I),PMMX-PMD(3-I))
        IF(PMU(I).LT.PML(I)+PARJ(64)) MBW(I)=-1
      ELSEIF((MBW(I).EQ.1.OR.MOFSH.GE.5).AND.MOFSH.NE.7) THEN
        ILM=I
        IF(MLM.EQ.2) ILM=3-I
        PML(I)=MAX(CKIN(NOFF+2*ILM-1),PARP(42))
        IF(MOFSH.GE.5.AND.I.EQ.2) PML(I)=MAX(PML(I),2.*PMAS(KFD2,1))
        PMU(I)=PMMX-MAX(CKIN(NOFF+5-2*ILM),PARP(42))
        IF(MOFSH.GE.5.AND.I.EQ.1) PMU(I)=MIN(PMU(I),PMMX-2.*
     &  PMAS(KFD2,1))
        IF(CKIN(NOFF+2*ILM).GT.CKIN(NOFF+2*ILM-1)) PMU(I)=MIN(PMU(I),
     &  CKIN(NOFF+2*ILM))
        IF(MBW(3-I).EQ.0) PMU(I)=MIN(PMU(I),PMMX-PMD(3-I))
        IF(I.EQ.MLM) PMU(I)=MIN(PMU(I),0.5*PMMX)
        IF(MEQL.EQ.0) PMH(I)=MIN(PMU(I),0.5*PMMX)
        IF(PMU(I).LT.PML(I)+PARJ(64)) MBW(I)=-1
        IF(MBW(I).EQ.1) THEN
          ATL(I)=ATAN((PML(I)**2-PMD(I)**2)/(PMD(I)*PGD(I)))
          ATU(I)=ATAN((PMU(I)**2-PMD(I)**2)/(PMD(I)*PGD(I)))
          IF(MEQL.EQ.0) ATH(I)=ATAN((PMH(I)**2-PMD(I)**2)/(PMD(I)*
     &    PGD(I)))
        ENDIF
      ELSEIF(MBW(I).EQ.1.AND.MOFSH.EQ.7) THEN
        ILM=I
        IF(MLM.EQ.2) ILM=3-I
        PML(I)=PARP(42)
        PMU(I)=PMMX-PARP(42)
        IF(MBW(3-I).EQ.0) PMU(I)=MIN(PMU(I),PMMX-PMD(3-I))
        IF(I.EQ.MLM) PMU(I)=MIN(PMU(I),0.5*PMMX)
        IF(MEQL.EQ.0) PMH(I)=MIN(PMU(I),0.5*PMMX)
        IF(PMU(I).LT.PML(I)+PARJ(64)) MBW(I)=-1
        IF(MBW(I).EQ.1) THEN
          ATL(I)=ATAN((PML(I)**2-PMD(I)**2)/(PMD(I)*PGD(I)))
          ATU(I)=ATAN((PMU(I)**2-PMD(I)**2)/(PMD(I)*PGD(I)))
          IF(MEQL.EQ.0) ATH(I)=ATAN((PMH(I)**2-PMD(I)**2)/(PMD(I)*
     &    PGD(I)))
        ENDIF
      ENDIF
  120 CONTINUE
      IF(MBW(1).LT.0.OR.MBW(2).LT.0.OR.(MBW(1).EQ.0.AND.MBW(2).EQ.0))
     &THEN
        CALL LUERRM(13,'(PYOFSH:) no allowed decay product masses')
        MINT(51)=1
        RETURN
      ENDIF
 
C...Calculation of partial width of resonance.
      IF(MOFSH.EQ.1) THEN
 
C..If only one integration, pick that to be the inner.
        IF(MBW(1).EQ.0) THEN
          PM2=PMD(1)
          PMD(1)=PMD(2)
          PGD(1)=PGD(2)
          PML(1)=PML(2)
          PMU(1)=PMU(2)
        ELSEIF(MBW(2).EQ.0) THEN
          PM2=PMD(2)
        ENDIF
 
C...Start outer loop of integration.
        IF(MBW(1).EQ.1.AND.MBW(2).EQ.1) THEN
          ATL2=ATAN((PML(2)**2-PMD(2)**2)/(PMD(2)*PGD(2)))
          ATU2=ATAN((PMU(2)**2-PMD(2)**2)/(PMD(2)*PGD(2)))
          NPT2=1
          XPT2(1)=1.
          INX2(1)=0
          FMAX2=0.
        ENDIF
  130   IF(MBW(1).EQ.1.AND.MBW(2).EQ.1) THEN
          PM2S=PMD(2)**2+PMD(2)*PGD(2)*TAN(ATL2+XPT2(NPT2)*(ATU2-ATL2))
          PM2=MIN(PMU(2),MAX(PML(2),SQRT(MAX(0.,PM2S))))
        ENDIF
        RM2=(PM2/PMMX)**2
 
C...Start inner loop of integration.
        PML1=PML(1)
        PMU1=MIN(PMU(1),PMMX-PM2)
        IF(MEQL.EQ.1) PMU1=MIN(PMU1,PM2)
        ATL1=ATAN((PML1**2-PMD(1)**2)/(PMD(1)*PGD(1)))
        ATU1=ATAN((PMU1**2-PMD(1)**2)/(PMD(1)*PGD(1)))
        IF(PML1+PARJ(64).GE.PMU1.OR.ATL1+1E-7.GE.ATU1) THEN
          FUNC2=0.
          GOTO 180
        ENDIF
        NPT1=1
        XPT1(1)=1.
        INX1(1)=0
        FMAX1=0.
  140   PM1S=PMD(1)**2+PMD(1)*PGD(1)*TAN(ATL1+XPT1(NPT1)*(ATU1-ATL1))
        PM1=MIN(PMU1,MAX(PML1,SQRT(MAX(0.,PM1S))))
        RM1=(PM1/PMMX)**2
 
C...Evaluate function value - inner loop.
        FUNC1=SQRT(MAX(0.,(1.-RM1-RM2)**2-4.*RM1*RM2))
        IF(MMED.EQ.1) FUNC1=FUNC1*((1.-RM1-RM2)**2+8.*RM1*RM2)
        IF(MMED.EQ.2) FUNC1=FUNC1**3*(1.+10.*RM1+10.*RM2+RM1**2+
     &  RM2**2+10.*RM1*RM2)
        IF(FUNC1.GT.FMAX1) FMAX1=FUNC1
        FPT1(NPT1)=FUNC1
 
C...Go to next position in inner loop.
        IF(NPT1.EQ.1) THEN
          NPT1=NPT1+1
          XPT1(NPT1)=0.
          INX1(NPT1)=1
          GOTO 140
        ELSEIF(NPT1.LE.8) THEN
          NPT1=NPT1+1
          IF(NPT1.LE.4.OR.NPT1.EQ.6) ISH1=1
          ISH1=ISH1+1
          XPT1(NPT1)=0.5*(XPT1(ISH1)+XPT1(INX1(ISH1)))
          INX1(NPT1)=INX1(ISH1)
          INX1(ISH1)=NPT1
          GOTO 140
        ELSEIF(NPT1.LT.100) THEN
          ISN1=ISH1
  150     ISH1=ISH1+1
          IF(ISH1.GT.NPT1) ISH1=2
          IF(ISH1.EQ.ISN1) GOTO 160
          DFPT1=ABS(FPT1(ISH1)-FPT1(INX1(ISH1)))
          IF(DFPT1.LT.PARP(43)*FMAX1) GOTO 150
          NPT1=NPT1+1
          XPT1(NPT1)=0.5*(XPT1(ISH1)+XPT1(INX1(ISH1)))
          INX1(NPT1)=INX1(ISH1)
          INX1(ISH1)=NPT1
          GOTO 140
        ENDIF
 
C...Calculate integral over inner loop.
  160   FSUM1=0.
        DO 170 IPT1=2,NPT1
        FSUM1=FSUM1+0.5*(FPT1(IPT1)+FPT1(INX1(IPT1)))*
     &  (XPT1(INX1(IPT1))-XPT1(IPT1))
  170   CONTINUE
        FUNC2=FSUM1*(ATU1-ATL1)/PARU(1)
  180   IF(MBW(1).EQ.1.AND.MBW(2).EQ.1) THEN
          IF(FUNC2.GT.FMAX2) FMAX2=FUNC2
          FPT2(NPT2)=FUNC2
 
C...Go to next position in outer loop.
          IF(NPT2.EQ.1) THEN
            NPT2=NPT2+1
            XPT2(NPT2)=0.
            INX2(NPT2)=1
            GOTO 130
          ELSEIF(NPT2.LE.8) THEN
            NPT2=NPT2+1
            IF(NPT2.LE.4.OR.NPT2.EQ.6) ISH2=1
            ISH2=ISH2+1
            XPT2(NPT2)=0.5*(XPT2(ISH2)+XPT2(INX2(ISH2)))
            INX2(NPT2)=INX2(ISH2)
            INX2(ISH2)=NPT2
            GOTO 130
          ELSEIF(NPT2.LT.100) THEN
            ISN2=ISH2
  190       ISH2=ISH2+1
            IF(ISH2.GT.NPT2) ISH2=2
            IF(ISH2.EQ.ISN2) GOTO 200
            DFPT2=ABS(FPT2(ISH2)-FPT2(INX2(ISH2)))
            IF(DFPT2.LT.PARP(43)*FMAX2) GOTO 190
            NPT2=NPT2+1
            XPT2(NPT2)=0.5*(XPT2(ISH2)+XPT2(INX2(ISH2)))
            INX2(NPT2)=INX2(ISH2)
            INX2(ISH2)=NPT2
            GOTO 130
          ENDIF
 
C...Calculate integral over outer loop.
  200     FSUM2=0.
          DO 210 IPT2=2,NPT2
          FSUM2=FSUM2+0.5*(FPT2(IPT2)+FPT2(INX2(IPT2)))*
     &    (XPT2(INX2(IPT2))-XPT2(IPT2))
  210     CONTINUE
          FSUM2=FSUM2*(ATU2-ATL2)/PARU(1)
          IF(MEQL.EQ.1) FSUM2=2.*FSUM2
        ELSE
          FSUM2=FUNC2
        ENDIF
 
C...Save result; second integration for user-selected mass range.
        IF(LOOP.EQ.1) WIDW=FSUM2
        WID2=FSUM2
        IF(LOOP.EQ.1.AND.(CKIN(46).GE.CKIN(45).OR.CKIN(48).GE.CKIN(47)
     &  .OR.MAX(CKIN(45),CKIN(47)).GE.1.01*PARP(42))) THEN
          LOOP=2
          GOTO 100
        ENDIF
        RET1=WIDW
        RET2=WID2/WIDW
 
C...Select two decay product masses of a resonance.
      ELSEIF(MOFSH.EQ.2.OR.MOFSH.EQ.7) THEN
  220   DO 230 I=1,2
        IF(MBW(I).EQ.0) GOTO 230
        PMBW=PMD(I)**2+PMD(I)*PGD(I)*TAN(ATL(I)+RLU(0)*(ATU(I)-ATL(I)))
        PMG(I)=MIN(PMU(I),MAX(PML(I),SQRT(MAX(0.,PMBW))))
        RMG(I)=(PMG(I)/PMMX)**2
  230   CONTINUE
        IF((MEQL.EQ.1.AND.PMG(MAX(1,MLM)).GT.PMG(MIN(2,3-MLM))).OR.
     &  PMG(1)+PMG(2)+PARJ(64).GT.PMMX) GOTO 220
 
C...Weight with matrix element (if none known, use beta factor).
        FLAM=SQRT(MAX(0.,(1.-RMG(1)-RMG(2))**2-4.*RMG(1)*RMG(2)))
        IF(MMED.EQ.1) THEN
          WTBE=FLAM*((1.-RMG(1)-RMG(2))**2+8.*RMG(1)*RMG(2))
        ELSEIF(MMED.EQ.2) THEN
          WTBE=FLAM**3*(1.+10.*RMG(1)+10.*RMG(2)+RMG(1)**2+
     &    RMG(2)**2+10.*RMG(1)*RMG(2))
        ELSEIF(MMED.EQ.3) THEN
          WTBE=FLAM*(RMG(1)+FLAM**2/12.)
        ELSE
          WTBE=FLAM
        ENDIF
        IF(WTBE.LT.RLU(0)) GOTO 220
        RET1=PMG(1)
        RET2=PMG(2)
 
C...Find suitable set of masses for initialization of 2 -> 2 processes.
      ELSEIF(MOFSH.EQ.3) THEN
        IF(MBW(1).NE.0.AND.MBW(2).EQ.0) THEN
          PMG(1)=MIN(PMD(1),0.5*(PML(1)+PMU(1)))
          PMG(2)=PMD(2)
        ELSEIF(MBW(2).NE.0.AND.MBW(1).EQ.0) THEN
          PMG(1)=PMD(1)
          PMG(2)=MIN(PMD(2),0.5*(PML(2)+PMU(2)))
        ELSE
          IDIV=-1
  240     IDIV=IDIV+1
          PMG(1)=MIN(PMD(1),0.1*(IDIV*PML(1)+(10-IDIV)*PMU(1)))
          PMG(2)=MIN(PMD(2),0.1*(IDIV*PML(2)+(10-IDIV)*PMU(2)))
          IF(IDIV.LE.9.AND.PMG(1)+PMG(2).GT.0.9*PMMX) GOTO 240
        ENDIF
        RET1=PMG(1)
        RET2=PMG(2)
 
C...Evaluate importance of excluded tails of Breit-Wigners.
        IF(MEQL.EQ.0.AND.MBW(1).EQ.1.AND.MBW(2).EQ.1.AND.PMD(1)+PMD(2).
     &  GT.PMMX.AND.PMH(1).GT.PML(1).AND.PMH(2).GT.PML(2)) MEQL=2
        IF(MEQL.LE.1) THEN
          VINT(80)=1.
          DO 250 I=1,2
          IF(MBW(I).NE.0) VINT(80)=VINT(80)*1.25*(ATU(I)-ATL(I))/PARU(1)
  250     CONTINUE
        ELSE
          VINT(80)=(1.25/PARU(1))**2*MAX((ATU(1)-ATL(1))*
     &    (ATH(2)-ATL(2)),(ATH(1)-ATL(1))*(ATU(2)-ATL(2)))
        ENDIF
        IF((ISUB.EQ.15.OR.ISUB.EQ.19.OR.ISUB.EQ.30.OR.ISUB.EQ.35).AND.
     &  MSTP(43).NE.2) VINT(80)=2.*VINT(80)
        IF(ISUB.EQ.22.AND.MSTP(43).NE.2) VINT(80)=4.*VINT(80)
        IF(MEQL.GE.1) VINT(80)=2.*VINT(80)
 
C...Pick one particle to be the lighter (if improves efficiency).
      ELSEIF(MOFSH.EQ.4) THEN
        IF(MEQL.EQ.0.AND.MBW(1).EQ.1.AND.MBW(2).EQ.1.AND.PMD(1)+PMD(2).
     &  GT.PMMX.AND.PMH(1).GT.PML(1).AND.PMH(2).GT.PML(2)) MEQL=2
  260   IF(MEQL.EQ.2) MLM=INT(1.5+RLU(0))
 
C...Select two masses according to Breit-Wigner + flat in s + 1/s.
        DO 270 I=1,2
        IF(MBW(I).EQ.0) GOTO 270
        PMV=PMU(I)
        IF(MEQL.EQ.2.AND.I.EQ.MLM) PMV=PMH(I)
        ATV=ATU(I)
        IF(MEQL.EQ.2.AND.I.EQ.MLM) ATV=ATH(I)
        RBR=RLU(0)
        IF((ISUB.EQ.15.OR.ISUB.EQ.19.OR.ISUB.EQ.22.OR.ISUB.EQ.30.OR.
     &  ISUB.EQ.35).AND.MSTP(43).NE.2) RBR=2.*RBR
        IF(RBR.LT.0.8) THEN
          PMSR=PMD(I)**2+PMD(I)*PGD(I)*TAN(ATL(I)+RLU(0)*(ATV-ATL(I)))
          PMG(I)=MIN(PMV,MAX(PML(I),SQRT(MAX(0.,PMSR))))
        ELSEIF(RBR.LT.0.9) THEN
          PMG(I)=SQRT(MAX(0.,PML(I)**2+RLU(0)*(PMV**2-PML(I)**2)))
        ELSEIF(RBR.LT.1.5) THEN
          PMG(I)=PML(I)*(PMV/PML(I))**RLU(0)
        ELSE
          PMG(I)=SQRT(MAX(0.,PML(I)**2*PMV**2/(PML(I)**2+RLU(0)*
     &    (PMV**2-PML(I)**2))))
        ENDIF
  270   CONTINUE
        IF((MEQL.GE.1.AND.PMG(MAX(1,MLM)).GT.PMG(MIN(2,3-MLM))).OR.
     &  PMG(1)+PMG(2)+PARJ(64).GT.PMMX) THEN
          IF(MINT(48).EQ.1) THEN
            NGEN(0,1)=NGEN(0,1)+1
            NGEN(MINT(1),1)=NGEN(MINT(1),1)+1
            GOTO 260
          ELSE
            MINT(51)=1
            RETURN
          ENDIF
        ENDIF
        RET1=PMG(1)
        RET2=PMG(2)
 
C...Give weight for selected mass distribution.
        VINT(80)=1.
        DO 280 I=1,2
        IF(MBW(I).EQ.0) GOTO 280
        PMV=PMU(I)
        IF(MEQL.EQ.2.AND.I.EQ.MLM) PMV=PMH(I)
        ATV=ATU(I)
        IF(MEQL.EQ.2.AND.I.EQ.MLM) ATV=ATH(I)
        F0=PMD(I)*PGD(I)/((PMG(I)**2-PMD(I)**2)**2+
     &  (PMD(I)*PGD(I))**2)/PARU(1)
        F1=1.
        F2=1./PMG(I)**2
        F3=1./PMG(I)**4
        FI0=(ATV-ATL(I))/PARU(1)
        FI1=PMV**2-PML(I)**2
        FI2=2.*LOG(PMV/PML(I))
        FI3=1./PML(I)**2-1./PMV**2
        IF((ISUB.EQ.15.OR.ISUB.EQ.19.OR.ISUB.EQ.22.OR.ISUB.EQ.30.OR.
     &  ISUB.EQ.35).AND.MSTP(43).NE.2) THEN
          VINT(80)=VINT(80)*20./(8.+(FI0/F0)*(F1/FI1+6.*F2/FI2+
     &    5.*F3/FI3))
        ELSE
          VINT(80)=VINT(80)*10./(8.+(FI0/F0)*(F1/FI1+F2/FI2))
        ENDIF
        VINT(80)=VINT(80)*FI0
  280   CONTINUE
        IF(MEQL.GE.1) VINT(80)=2.*VINT(80)
 
      ELSEIF(MOFSH.EQ.5) THEN
C...Find suitable set of masses for initialization of 2 -> 3 process.
        IDIV=6
  290   IDIV=IDIV-1
        IF(MBW(1).EQ.0) THEN
          PMG(1)=PMD(1)
        ELSE
          PMSR=PMD(1)**2+PMD(1)*PGD(1)*TAN(ATL(1)+0.1*IDIV*(ATU(1)-
     &    ATL(1)))
          PMG(1)=MIN(PMU(1),MAX(PML(1),SQRT(MAX(0.,PMSR))))
        ENDIF
        PMG(2)=PML(2)*(PMU(2)/PML(2))**(0.1*IDIV)
        IF(IDIV.GE.1.AND.PMG(1)+PMG(2).GT.0.9*PMMX) GOTO 290
        RET1=PMG(1)
        RET2=PMG(2)
 
C...Evaluate size of selected phase space volume.
        VINT(80)=2.*LOG(PMU(2)/PML(2))
        IF(MBW(1).NE.0) VINT(80)=VINT(80)*1.25*(ATU(1)-ATL(1))/PARU(1)
 
C...Pick decay angles.
        VINT(81)=0.
        VINT(82)=0.5*PARU(1)
        VINT(83)=1.
        VINT(84)=0.
 
C...Select flavour of resonance decays.
        KFA=KFPR(ISUB,1)
        CALL PYWIDT(KFA,PMG(1)**2,WDTP,WDTE)
        IF(KCHG(KFA,3).EQ.0) THEN
          IPM=2
        ELSE
          IPM=(5-ISIGN(1,KFA))/2
        ENDIF
        WDTE0S=WDTE(0,1)+WDTE(0,IPM)+WDTE(0,4)
        IF(WDTE0S.LE.0.) THEN
          CALL LUERRM(12,'(PYOFSH:) no allowed resonace decay channel')
          MINT(51)=1
          RETURN
        ENDIF
        WDTEC=0.
        DO 300 IDL=1,MDCY(KFA,3)
        WDTEK=WDTE(IDL,1)+WDTE(IDL,IPM)+WDTE(IDL,4)
        IF(WDTEK.GT.WDTEC) THEN
          IDC=IDL+MDCY(KFA,2)-1
          WDTEC=WDTEK
        ENDIF
  300   CONTINUE
        MINT(35)=IDC
 
C...Compensating factor for all flavours.
        KFL=IABS(KFDP(IDC,1))
        QFL=KCHG(KFL,1)/3.
        AFL=SIGN(1.,QFL+0.1)
        VFL=AFL-4.*PARU(102)*QFL
        WDTEK=VFL**2+AFL**2
        VINT(80)=VINT(80)*WDTE0S/WDTEK
 
      ELSEIF(MOFSH.EQ.6) THEN
C...Select two masses, one basically Breit-Wigner, other dm^2/m^2.
        IF(MBW(1).NE.0) THEN
          RBR=RLU(0)
          IF(RBR.LT.0.8) THEN
            PMSR=PMD(1)**2+PMD(1)*PGD(1)*TAN(ATL(1)+RLU(0)*
     &      (ATU(1)-ATL(1)))
            PMG(1)=MIN(PMU(1),MAX(PML(1),SQRT(MAX(0.,PMSR))))
          ELSEIF(RBR.LT.0.9) THEN
            PMG(1)=SQRT(MAX(0.,PML(1)**2+RLU(0)*(PMU(1)**2-PML(1)**2)))
          ELSE
            PMG(1)=PML(1)*(PMU(1)/PML(1))**RLU(0)
          ENDIF
        ENDIF
        PMG(2)=PML(2)*(PMU(2)/PML(2))**RLU(0)
        IF(SQRT(MAX(0.,1.-(PML(2)/PMG(2))**2)).LT.RLU(0).OR.
     &  PMG(1)+PMG(2)+PARJ(64).GT.PMMX) THEN
          MINT(51)=1
          RETURN
        ENDIF
        RET1=PMG(1)
        RET2=PMG(2)
 
C...Give weight for selected mass distribution.
        VINT(80)=2.*LOG(PMU(2)/PML(2))
        IF(MBW(1).NE.0) THEN
          F0=PMD(1)*PGD(1)/((PMG(1)**2-PMD(1)**2)**2+
     &    (PMD(1)*PGD(1))**2)/PARU(1)
          F1=1.
          F2=1./PMG(1)**2
          FI0=(ATU(1)-ATL(1))/PARU(1)
          FI1=PMU(1)**2-PML(1)**2
          FI2=2.*LOG(PMU(1)/PML(1))
          VINT(80)=VINT(80)*10.*FI0/(8.+(FI0/F0)*(F1/FI1+F2/FI2))
        ENDIF
 
C...Select decay angles.
        VINT(81)=2.*RLU(0)-1.
        VINT(82)=PARU(2)*RLU(0)
        VINT(83)=2.*RLU(0)-1.
        VINT(84)=PARU(2)*RLU(0)
 
C...Select flavour of resonance decays.
        KFA=KFPR(ISUB,1)
        CALL PYWIDT(KFA,PMG(1)**2,WDTP,WDTE)
        IF(KCHG(KFA,3).EQ.0) THEN
          IPM=2
        ELSE
          IPM=(5-ISIGN(1,KFA))/2
        ENDIF
        WDTE0S=WDTE(0,1)+WDTE(0,IPM)+WDTE(0,4)
        IF(WDTE0S.LE.0.) THEN
          CALL LUERRM(12,'(PYOFSH:) no allowed resonace decay channel')
          MINT(51)=1
          RETURN
        ENDIF
        RKFL=WDTE0S*RLU(0)
        IDL=0
  310   IDL=IDL+1
        IDC=IDL+MDCY(KFA,2)-1
        RKFL=RKFL-(WDTE(IDL,1)+WDTE(IDL,IPM)+WDTE(IDL,4))
        IF(IDL.LT.MDCY(KFA,3).AND.RKFL.GT.0.) GOTO 310
        MINT(35)=IDC
 
C...Compensating factor for all flavours.
        KFL=IABS(KFDP(IDC,1))
        QFL=KCHG(KFL,1)/3.
        AFL=SIGN(1.,QFL+0.1)
        VFL=AFL-4.*PARU(102)*QFL
        WDTEK=VFL**2+AFL**2
        VINT(80)=VINT(80)*WDTE0S/WDTEK
      ENDIF
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYKLIM(ILIM)
 
C...Checks generated variables against pre-set kinematical limits;
C...also calculates limits on variables used in generation.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
C...Common kinematical expressions.
      MINT(51)=0
      ISUB=MINT(1)
      ISTSB=ISET(ISUB)
      IF(ISUB.EQ.96) GOTO 110
      SQM3=VINT(63)
      SQM4=VINT(64)
      IF(ILIM.NE.0) THEN
        IF(ABS(SQM3).LT.1E-4.AND.ABS(SQM4).LT.1E-4) THEN
          CKIN09=MAX(CKIN(9),CKIN(13))
          CKIN10=MIN(CKIN(10),CKIN(14))
          CKIN11=MAX(CKIN(11),CKIN(15))
          CKIN12=MIN(CKIN(12),CKIN(16))
        ELSE
          CKIN09=MAX(CKIN(9),MIN(0.,CKIN(13)))
          CKIN10=MIN(CKIN(10),MAX(0.,CKIN(14)))
          CKIN11=MAX(CKIN(11),MIN(0.,CKIN(15)))
          CKIN12=MIN(CKIN(12),MAX(0.,CKIN(16)))
        ENDIF
      ENDIF
      IF(ILIM.NE.1) THEN
        TAU=VINT(21)
        RM3=SQM3/(TAU*VINT(2))
        RM4=SQM4/(TAU*VINT(2))
        BE34=SQRT(MAX(1E-20,(1.-RM3-RM4)**2-4.*RM3*RM4))
      ENDIF
      PTHMIN=CKIN(3)
      IF(MIN(SQM3,SQM4).LT.CKIN(6)**2.AND.ISTSB.NE.1.AND.ISTSB.NE.3)
     &PTHMIN=MAX(CKIN(3),CKIN(5))
 
      IF(ILIM.EQ.0) THEN
C...Check generated values of tau, y*, cos(theta-hat), and tau' against
C...pre-set kinematical limits.
        YST=VINT(22)
        CTH=VINT(23)
        TAUP=VINT(26)
        TAUE=TAU
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=TAUP
        X1=SQRT(TAUE)*EXP(YST)
        X2=SQRT(TAUE)*EXP(-YST)
        XF=X1-X2
        IF(MINT(47).NE.1) THEN
          IF(TAU*VINT(2).LT.CKIN(1)**2) MINT(51)=1
          IF(CKIN(2).GE.0..AND.TAU*VINT(2).GT.CKIN(2)**2) MINT(51)=1
          IF(YST.LT.CKIN(7).OR.YST.GT.CKIN(8)) MINT(51)=1
          IF(XF.LT.CKIN(25).OR.XF.GT.CKIN(26)) MINT(51)=1
        ENDIF
        IF(MINT(45).NE.1) THEN
          IF(X1.LT.CKIN(21).OR.X1.GT.CKIN(22)) MINT(51)=1
        ENDIF
        IF(MINT(46).NE.1) THEN
          IF(X2.LT.CKIN(23).OR.X2.GT.CKIN(24)) MINT(51)=1
        ENDIF
        IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) THEN
          PTH=0.5*BE34*SQRT(TAU*VINT(2)*MAX(0.,1.-CTH**2))
          EXPY3=MAX(1.E-10,(1.+RM3-RM4+BE34*CTH)/
     &    MAX(1.E-10,(1.+RM3-RM4-BE34*CTH)))
          EXPY4=MAX(1.E-10,(1.-RM3+RM4-BE34*CTH)/
     &    MAX(1.E-10,(1.-RM3+RM4+BE34*CTH)))
          Y3=YST+0.5*LOG(EXPY3)
          Y4=YST+0.5*LOG(EXPY4)
          YLARGE=MAX(Y3,Y4)
          YSMALL=MIN(Y3,Y4)
          ETALAR=10.
          ETASMA=-10.
          STH=SQRT(MAX(0.,1.-CTH**2))
          EXSQ3=SQRT(MAX(1E-20,((1.+RM3-RM4)*COSH(YST)+BE34*SINH(YST)*
     &    CTH)**2-4.*RM3))
          EXSQ4=SQRT(MAX(1E-20,((1.-RM3+RM4)*COSH(YST)-BE34*SINH(YST)*
     &    CTH)**2-4.*RM4))
          IF(STH.GE.1.E-6) THEN
            EXPET3=((1.+RM3-RM4)*SINH(YST)+BE34*COSH(YST)*CTH+EXSQ3)/
     &      (BE34*STH)
            EXPET4=((1.-RM3+RM4)*SINH(YST)-BE34*COSH(YST)*CTH+EXSQ4)/
     &      (BE34*STH)
            ETA3=LOG(MIN(1.E10,MAX(1.E-10,EXPET3)))
            ETA4=LOG(MIN(1.E10,MAX(1.E-10,EXPET4)))
            ETALAR=MAX(ETA3,ETA4)
            ETASMA=MIN(ETA3,ETA4)
          ENDIF
          CTS3=((1.+RM3-RM4)*SINH(YST)+BE34*COSH(YST)*CTH)/EXSQ3
          CTS4=((1.-RM3+RM4)*SINH(YST)-BE34*COSH(YST)*CTH)/EXSQ4
          CTSLAR=MIN(1.,MAX(CTS3,CTS4))
          CTSSMA=MAX(-1.,MIN(CTS3,CTS4))
          SH=TAU*VINT(2)
          RPTS=4.*VINT(71)**2/SH
          BE34L=SQRT(MAX(0.,(1.-RM3-RM4)**2-4.*RM3*RM4-RPTS))
          RM34=MAX(1E-20,2.*RM3*RM4)
          IF(2.*VINT(71)**2/(VINT(21)*VINT(2)).LT.0.0001)
     &    RM34=MAX(RM34,2.*VINT(71)**2/(VINT(21)*VINT(2)))
          RTHM=(4.*RM3*RM4+RPTS)/(1.-RM3-RM4+BE34L)
          THA=0.5*SH*MAX(RTHM,1.-RM3-RM4-BE34*CTH)
          UHA=0.5*SH*MAX(RTHM,1.-RM3-RM4+BE34*CTH)
          IF(PTH.LT.PTHMIN) MINT(51)=1
          IF(CKIN(4).GE.0..AND.PTH.GT.CKIN(4)) MINT(51)=1
          IF(YLARGE.LT.CKIN(9).OR.YLARGE.GT.CKIN(10)) MINT(51)=1
          IF(YSMALL.LT.CKIN(11).OR.YSMALL.GT.CKIN(12)) MINT(51)=1
          IF(ETALAR.LT.CKIN(13).OR.ETALAR.GT.CKIN(14)) MINT(51)=1
          IF(ETASMA.LT.CKIN(15).OR.ETASMA.GT.CKIN(16)) MINT(51)=1
          IF(CTSLAR.LT.CKIN(17).OR.CTSLAR.GT.CKIN(18)) MINT(51)=1
          IF(CTSSMA.LT.CKIN(19).OR.CTSSMA.GT.CKIN(20)) MINT(51)=1
          IF(CTH.LT.CKIN(27).OR.CTH.GT.CKIN(28)) MINT(51)=1
          IF(THA.LT.CKIN(35)) MINT(51)=1
          IF(CKIN(36).GE.0..AND.THA.GT.CKIN(36)) MINT(51)=1
          IF(UHA.LT.CKIN(37)) MINT(51)=1
          IF(CKIN(38).GE.0..AND.UHA.GT.CKIN(38)) MINT(51)=1
        ENDIF
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) THEN
          IF(TAUP*VINT(2).LT.CKIN(31)**2) MINT(51)=1
          IF(CKIN(32).GE.0..AND.TAUP*VINT(2).GT.CKIN(32)**2) MINT(51)=1
        ENDIF
 
C...Additional cuts on W2 (approximately) in DIS.
        IF(ISUB.EQ.10) THEN
          XBJ=X2
          IF(IABS(MINT(12)).LT.20) XBJ=X1
          Q2BJ=THA
          W2BJ=Q2BJ*(1.-XBJ)/XBJ
          IF(W2BJ.LT.CKIN(39)) MINT(51)=1
          IF(CKIN(40).GT.0..AND.W2BJ.GT.CKIN(40)) MINT(51)=1
        ENDIF
 
C...Additional p_T cuts on 2 -> 3 process.
        IF(ISTSB.EQ.6) THEN
          KFQ=KFPR(131,2)
          PMQQ=SQRT(VINT(64))
          PMQ=PMAS(KFQ,1)
          PZQ=SQRT(MAX(0.,(0.5*PMQQ)**2-PMQ**2))
          DO 100 I=MINT(84)+1,MINT(84)+2
          K(I,1)=1
          P(I,1)=0.
          P(I,2)=0.
          P(I,3)=PZQ*(-1.)**(I-1)
          P(I,4)=0.5*PMQQ
          P(I,5)=PMQ
  100     CONTINUE
          PEQQ=0.5*SQRT(TAU*VINT(2))*(1.+(VINT(64)-VINT(63))/
     &    (TAU*VINT(2)))
          PZQQ=SQRT(MAX(0.,PEQQ**2-VINT(64)))
          CALL LUDBRB(MINT(84)+1,MINT(84)+2,ACOS(VINT(83)),VINT(84),
     &    0D0,0D0,-DBLE(PZQQ/PEQQ))
          CALL LUDBRB(MINT(84)+1,MINT(84)+2,ACOS(VINT(23)),VINT(24),
     &    0D0,0D0,0D0)
          PTQ2=SQRT(P(MINT(84)+1,1)**2+P(MINT(84)+1,2)**2)
          PTQ3=SQRT(P(MINT(84)+2,1)**2+P(MINT(84)+2,2)**2)
          PTMNQ=MIN(PTQ2,PTQ3)
          PTMXQ=MAX(PTQ2,PTQ3)
          IF(PTMNQ.LT.CKIN(51)) MINT(51)=1
          IF(CKIN(52).GE.0..AND.PTMNQ.GT.CKIN(52)) MINT(51)=1
          IF(PTMXQ.LT.CKIN(53)) MINT(51)=1
          IF(CKIN(54).GE.0..AND.PTMXQ.GT.CKIN(54)) MINT(51)=1
          VINT(85)=PTMNQ
          VINT(86)=PTMXQ
        ENDIF
 
      ELSEIF(ILIM.EQ.1) THEN
C...Calculate limits on tau
C...0) due to definition
        TAUMN0=0.
        TAUMX0=1.
C...1) due to limits on subsystem mass
        TAUMN1=CKIN(1)**2/VINT(2)
        TAUMX1=1.
        IF(CKIN(2).GE.0.) TAUMX1=CKIN(2)**2/VINT(2)
C...2) due to limits on pT-hat (and non-overlapping rapidity intervals)
        TM3=SQRT(SQM3+PTHMIN**2)
        TM4=SQRT(SQM4+PTHMIN**2)
        YDCOSH=1.
        IF(CKIN09.GT.CKIN12) YDCOSH=COSH(CKIN09-CKIN12)
        TAUMN2=(TM3**2+2.*TM3*TM4*YDCOSH+TM4**2)/VINT(2)
        TAUMX2=1.
C...3) due to limits on pT-hat and cos(theta-hat)
        CTH2MN=MIN(CKIN(27)**2,CKIN(28)**2)
        CTH2MX=MAX(CKIN(27)**2,CKIN(28)**2)
        TAUMN3=0.
        IF(CKIN(27)*CKIN(28).GT.0.) TAUMN3=
     &  (SQRT(SQM3+PTHMIN**2/(1.-CTH2MN))+
     &  SQRT(SQM4+PTHMIN**2/(1.-CTH2MN)))**2/VINT(2)
        TAUMX3=1.
        IF(CKIN(4).GE.0..AND.CTH2MX.LT.1.) TAUMX3=
     &  (SQRT(SQM3+CKIN(4)**2/(1.-CTH2MX))+
     &  SQRT(SQM4+CKIN(4)**2/(1.-CTH2MX)))**2/VINT(2)
C...4) due to limits on x1 and x2
        TAUMN4=CKIN(21)*CKIN(23)
        TAUMX4=CKIN(22)*CKIN(24)
C...5) due to limits on xF
        TAUMN5=0.
        TAUMX5=MAX(1.-CKIN(25),1.+CKIN(26))
C...6) due to limits on that and uhat
        TAUMN6=(SQM3+SQM4+CKIN(35)+CKIN(37))/VINT(2)
        TAUMX6=1.
        IF(CKIN(36).GT.0..AND.CKIN(38).GT.0.) TAUMX6=
     &  (SQM3+SQM4+CKIN(36)+CKIN(38))/VINT(2)
 
C...Net effect of all separate limits.
        VINT(11)=MAX(TAUMN0,TAUMN1,TAUMN2,TAUMN3,TAUMN4,TAUMN5,TAUMN6)
        VINT(31)=MIN(TAUMX0,TAUMX1,TAUMX2,TAUMX3,TAUMX4,TAUMX5,TAUMX6)
        IF(MINT(47).EQ.1.AND.(ISTSB.EQ.1.OR.ISTSB.EQ.2.OR.ISTSB.EQ.6))
     &  THEN
          VINT(11)=0.99999
          VINT(31)=1.00001
        ELSEIF(MINT(47).EQ.5) THEN
          VINT(31)=MIN(VINT(31),0.999998)
        ENDIF
        IF(VINT(31).LE.VINT(11)) MINT(51)=1
 
      ELSEIF(ILIM.EQ.2) THEN
C...Calculate limits on y*
        TAUE=TAU
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=VINT(26)
        TAURT=SQRT(TAUE)
C...0) due to kinematics
        YSTMN0=LOG(TAURT)
        YSTMX0=-YSTMN0
C...1) due to explicit limits
        YSTMN1=CKIN(7)
        YSTMX1=CKIN(8)
C...2) due to limits on x1
        YSTMN2=LOG(MAX(TAUE,CKIN(21))/TAURT)
        YSTMX2=LOG(MAX(TAUE,CKIN(22))/TAURT)
C...3) due to limits on x2
        YSTMN3=-LOG(MAX(TAUE,CKIN(24))/TAURT)
        YSTMX3=-LOG(MAX(TAUE,CKIN(23))/TAURT)
C...4) due to limits on xF
        YEPMN4=0.5*ABS(CKIN(25))/TAURT
        YSTMN4=SIGN(LOG(MAX(1E-20,SQRT(1.+YEPMN4**2)+YEPMN4)),CKIN(25))
        YEPMX4=0.5*ABS(CKIN(26))/TAURT
        YSTMX4=SIGN(LOG(MAX(1E-20,SQRT(1.+YEPMX4**2)+YEPMX4)),CKIN(26))
C...5) due to simultaneous limits on y-large and y-small
        YEPSMN=(RM3-RM4)*SINH(CKIN09-CKIN11)
        YEPSMX=(RM3-RM4)*SINH(CKIN10-CKIN12)
        YDIFMN=ABS(LOG(MAX(1E-20,SQRT(1.+YEPSMN**2)-YEPSMN)))
        YDIFMX=ABS(LOG(MAX(1E-20,SQRT(1.+YEPSMX**2)-YEPSMX)))
        YSTMN5=0.5*(CKIN09+CKIN11-YDIFMN)
        YSTMX5=0.5*(CKIN10+CKIN12+YDIFMX)
C...6) due to simultaneous limits on cos(theta-hat) and y-large or
C...   y-small
        CTHLIM=SQRT(MAX(0.,1.-4.*PTHMIN**2/(BE34**2*TAUE*VINT(2))))
        RZMN=BE34*MAX(CKIN(27),-CTHLIM)
        RZMX=BE34*MIN(CKIN(28),CTHLIM)
        YEX3MX=(1.+RM3-RM4+RZMX)/MAX(1E-10,1.+RM3-RM4-RZMX)
        YEX4MX=(1.+RM4-RM3-RZMN)/MAX(1E-10,1.+RM4-RM3+RZMN)
        YEX3MN=MAX(1E-10,1.+RM3-RM4+RZMN)/(1.+RM3-RM4-RZMN)
        YEX4MN=MAX(1E-10,1.+RM4-RM3-RZMX)/(1.+RM4-RM3+RZMX)
        YSTMN6=CKIN09-0.5*LOG(MAX(YEX3MX,YEX4MX))
        YSTMX6=CKIN12-0.5*LOG(MIN(YEX3MN,YEX4MN))
 
C...Net effect of all separate limits.
        VINT(12)=MAX(YSTMN0,YSTMN1,YSTMN2,YSTMN3,YSTMN4,YSTMN5,YSTMN6)
        VINT(32)=MIN(YSTMX0,YSTMX1,YSTMX2,YSTMX3,YSTMX4,YSTMX5,YSTMX6)
        IF(MINT(47).EQ.1) THEN
          VINT(12)=-0.00001
          VINT(32)=0.00001
        ELSEIF(MINT(47).EQ.2) THEN
          VINT(12)=0.99999*YSTMX0
          VINT(32)=1.00001*YSTMX0
        ELSEIF(MINT(47).EQ.3) THEN
          VINT(12)=-1.00001*YSTMX0
          VINT(32)=-0.99999*YSTMX0
        ELSEIF(MINT(47).EQ.5) THEN
          YSTEE=LOG(0.999999/TAURT)
          VINT(12)=MAX(VINT(12),-YSTEE)
          VINT(32)=MIN(VINT(32),YSTEE)
        ENDIF
        IF(VINT(32).LE.VINT(12)) MINT(51)=1
 
      ELSEIF(ILIM.EQ.3) THEN
C...Calculate limits on cos(theta-hat)
        YST=VINT(22)
C...0) due to definition
        CTNMN0=-1.
        CTNMX0=0.
        CTPMN0=0.
        CTPMX0=1.
C...1) due to explicit limits
        CTNMN1=MIN(0.,CKIN(27))
        CTNMX1=MIN(0.,CKIN(28))
        CTPMN1=MAX(0.,CKIN(27))
        CTPMX1=MAX(0.,CKIN(28))
C...2) due to limits on pT-hat
        CTNMN2=-SQRT(MAX(0.,1.-4.*PTHMIN**2/(BE34**2*TAU*VINT(2))))
        CTPMX2=-CTNMN2
        CTNMX2=0.
        CTPMN2=0.
        IF(CKIN(4).GE.0.) THEN
          CTNMX2=-SQRT(MAX(0.,1.-4.*CKIN(4)**2/(BE34**2*TAU*VINT(2))))
          CTPMN2=-CTNMX2
        ENDIF
C...3) due to limits on y-large and y-small
        CTNMN3=MIN(0.,MAX((1.+RM3-RM4)/BE34*TANH(CKIN11-YST),
     &  -(1.-RM3+RM4)/BE34*TANH(CKIN10-YST)))
        CTNMX3=MIN(0.,(1.+RM3-RM4)/BE34*TANH(CKIN12-YST),
     &  -(1.-RM3+RM4)/BE34*TANH(CKIN09-YST))
        CTPMN3=MAX(0.,(1.+RM3-RM4)/BE34*TANH(CKIN09-YST),
     &  -(1.-RM3+RM4)/BE34*TANH(CKIN12-YST))
        CTPMX3=MAX(0.,MIN((1.+RM3-RM4)/BE34*TANH(CKIN10-YST),
     &  -(1.-RM3+RM4)/BE34*TANH(CKIN11-YST)))
C...4) due to limits on that
        CTNMN4=-1.
        CTNMX4=0.
        CTPMN4=0.
        CTPMX4=1.
        SH=TAU*VINT(2)
        IF(CKIN(35).GT.0.) THEN
          CTLIM=(1.-RM3-RM4-2.*CKIN(35)/SH)/BE34
          IF(CTLIM.GT.0.) THEN
            CTPMX4=CTLIM
          ELSE
            CTPMX4=0.
            CTNMX4=CTLIM
          ENDIF
        ENDIF
        IF(CKIN(36).GT.0.) THEN
          CTLIM=(1.-RM3-RM4-2.*CKIN(36)/SH)/BE34
          IF(CTLIM.LT.0.) THEN
            CTNMN4=CTLIM
          ELSE
            CTNMN4=0.
            CTPMN4=CTLIM
          ENDIF
        ENDIF
C...5) due to limits on uhat
        CTNMN5=-1.
        CTNMX5=0.
        CTPMN5=0.
        CTPMX5=1.
        IF(CKIN(37).GT.0.) THEN
          CTLIM=(2.*CKIN(37)/SH-(1.-RM3-RM4))/BE34
          IF(CTLIM.LT.0.) THEN
            CTNMN5=CTLIM
          ELSE
            CTNMN5=0.
            CTPMN5=CTLIM
          ENDIF
        ENDIF
        IF(CKIN(38).GT.0.) THEN
          CTLIM=(2.*CKIN(38)/SH-(1.-RM3-RM4))/BE34
          IF(CTLIM.GT.0.) THEN
            CTPMX5=CTLIM
          ELSE
            CTPMX5=0.
            CTNMX5=CTLIM
          ENDIF
        ENDIF
 
C...Net effect of all separate limits.
        VINT(13)=MAX(CTNMN0,CTNMN1,CTNMN2,CTNMN3,CTNMN4,CTNMN5)
        VINT(33)=MIN(CTNMX0,CTNMX1,CTNMX2,CTNMX3,CTNMX4,CTNMX5)
        VINT(14)=MAX(CTPMN0,CTPMN1,CTPMN2,CTPMN3,CTPMN4,CTPMN5)
        VINT(34)=MIN(CTPMX0,CTPMX1,CTPMX2,CTPMX3,CTPMX4,CTPMX5)
        IF(VINT(33).LE.VINT(13).AND.VINT(34).LE.VINT(14)) MINT(51)=1
 
      ELSEIF(ILIM.EQ.4) THEN
C...Calculate limits on tau'
C...0) due to kinematics
        TAPMN0=TAU
        IF((ISTSB.EQ.5.OR.ISTSB.EQ.6).AND.KFPR(ISUB,2).GT.0) THEN
          PQRAT=2.*PMAS(KFPR(ISUB,2),1)/VINT(1)
          TAPMN0=(SQRT(TAU)+PQRAT)**2
        ENDIF
        TAPMX0=1.
C...1) due to explicit limits
        TAPMN1=CKIN(31)**2/VINT(2)
        TAPMX1=1.
        IF(CKIN(32).GE.0.) TAPMX1=CKIN(32)**2/VINT(2)
 
C...Net effect of all separate limits.
        VINT(16)=MAX(TAPMN0,TAPMN1)
        VINT(36)=MIN(TAPMX0,TAPMX1)
        IF(MINT(47).EQ.1) THEN
          VINT(16)=0.99999
          VINT(36)=1.00001
        ENDIF
        IF(VINT(36).LE.VINT(16)) MINT(51)=1
 
      ENDIF
      RETURN
 
C...Special case for low-pT and multiple interactions:
C...effective kinematical limits for tau, y*, cos(theta-hat).
  110 IF(ILIM.EQ.0) THEN
      ELSEIF(ILIM.EQ.1) THEN
        IF(MSTP(82).LE.1) VINT(11)=4.*PARP(81)**2/VINT(2)
        IF(MSTP(82).GE.2) VINT(11)=PARP(82)**2/VINT(2)
        VINT(31)=1.
      ELSEIF(ILIM.EQ.2) THEN
        VINT(12)=0.5*LOG(VINT(21))
        VINT(32)=-VINT(12)
      ELSEIF(ILIM.EQ.3) THEN
        IF(MSTP(82).LE.1) ST2EFF=4.*PARP(81)**2/(VINT(21)*VINT(2))
        IF(MSTP(82).GE.2) ST2EFF=0.01*PARP(82)**2/(VINT(21)*VINT(2))
        VINT(13)=-SQRT(MAX(0.,1.-ST2EFF))
        VINT(33)=0.
        VINT(14)=0.
        VINT(34)=-VINT(13)
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYKMAP(IVAR,MVAR,VVAR)
 
C...Maps a uniform distribution into a distribution of a kinematical
C...variable according to one of the possibilities allowed. It is
C...assumed that kinematical limits have been set by a PYKLIM call.
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
C...Convert VVAR to tau variable.
      ISUB=MINT(1)
      ISTSB=ISET(ISUB)
      IF(IVAR.EQ.1) THEN
        TAUMIN=VINT(11)
        TAUMAX=VINT(31)
        IF(MVAR.EQ.3.OR.MVAR.EQ.4) THEN
          TAURE=VINT(73)
          GAMRE=VINT(74)
        ELSEIF(MVAR.EQ.5.OR.MVAR.EQ.6) THEN
          TAURE=VINT(75)
          GAMRE=VINT(76)
        ENDIF
        IF(MINT(47).EQ.1.AND.(ISTSB.EQ.1.OR.ISTSB.EQ.2.OR.ISTSB.EQ.6))
     &  THEN
          TAU=1.
        ELSEIF(MVAR.EQ.1) THEN
          TAU=TAUMIN*(TAUMAX/TAUMIN)**VVAR
        ELSEIF(MVAR.EQ.2) THEN
          TAU=TAUMAX*TAUMIN/(TAUMIN+(TAUMAX-TAUMIN)*VVAR)
        ELSEIF(MVAR.EQ.3.OR.MVAR.EQ.5) THEN
          RATGEN=(TAURE+TAUMAX)/(TAURE+TAUMIN)*TAUMIN/TAUMAX
          TAU=TAURE*TAUMIN/((TAURE+TAUMIN)*RATGEN**VVAR-TAUMIN)
        ELSEIF(MVAR.EQ.4.OR.MVAR.EQ.6) THEN
          AUPP=ATAN((TAUMAX-TAURE)/GAMRE)
          ALOW=ATAN((TAUMIN-TAURE)/GAMRE)
          TAU=TAURE+GAMRE*TAN(ALOW+(AUPP-ALOW)*VVAR)
        ELSE
          AUPP=LOG(MAX(2E-6,1.-TAUMAX))
          ALOW=LOG(MAX(2E-6,1.-TAUMIN))
          TAU=1.-EXP(AUPP+VVAR*(ALOW-AUPP))
        ENDIF
        VINT(21)=MIN(TAUMAX,MAX(TAUMIN,TAU))
 
C...Convert VVAR to y* variable.
      ELSEIF(IVAR.EQ.2) THEN
        YSTMIN=VINT(12)
        YSTMAX=VINT(32)
        TAUE=VINT(21)
        IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=VINT(26)
        IF(MINT(47).EQ.1) THEN
          YST=0.
        ELSEIF(MINT(47).EQ.2) THEN
          YST=-0.5*LOG(TAUE)
        ELSEIF(MINT(47).EQ.3) THEN
          YST=0.5*LOG(TAUE)
        ELSEIF(MVAR.EQ.1) THEN
          YST=YSTMIN+(YSTMAX-YSTMIN)*SQRT(VVAR)
        ELSEIF(MVAR.EQ.2) THEN
          YST=YSTMAX-(YSTMAX-YSTMIN)*SQRT(1.-VVAR)
        ELSEIF(MVAR.EQ.3) THEN
          AUPP=ATAN(EXP(YSTMAX))
          ALOW=ATAN(EXP(YSTMIN))
          YST=LOG(TAN(ALOW+(AUPP-ALOW)*VVAR))
        ELSEIF(MVAR.EQ.4) THEN
          YST0=-0.5*LOG(TAUE)
          AUPP=LOG(MAX(1E-6,EXP(YST0-YSTMIN)-1.))
          ALOW=LOG(MAX(1E-6,EXP(YST0-YSTMAX)-1.))
          YST=YST0-LOG(1.+EXP(ALOW+VVAR*(AUPP-ALOW)))
        ELSE
          YST0=-0.5*LOG(TAUE)
          AUPP=LOG(MAX(1E-6,EXP(YST0+YSTMIN)-1.))
          ALOW=LOG(MAX(1E-6,EXP(YST0+YSTMAX)-1.))
          YST=LOG(1.+EXP(AUPP+VVAR*(ALOW-AUPP)))-YST0
        ENDIF
        VINT(22)=MIN(YSTMAX,MAX(YSTMIN,YST))
 
C...Convert VVAR to cos(theta-hat) variable.
      ELSEIF(IVAR.EQ.3) THEN
        RM34=MAX(1E-20,2.*VINT(63)*VINT(64)/(VINT(21)*VINT(2))**2)
        RSQM=1.+RM34
        IF(2.*VINT(71)**2/(VINT(21)*VINT(2)).LT.0.0001) RM34=MAX(RM34,
     &  2.*VINT(71)**2/(VINT(21)*VINT(2)))
        CTNMIN=VINT(13)
        CTNMAX=VINT(33)
        CTPMIN=VINT(14)
        CTPMAX=VINT(34)
        IF(MVAR.EQ.1) THEN
          ANEG=CTNMAX-CTNMIN
          APOS=CTPMAX-CTPMIN
          IF(ANEG.GT.0..AND.VVAR*(ANEG+APOS).LE.ANEG) THEN
            VCTN=VVAR*(ANEG+APOS)/ANEG
            CTH=CTNMIN+(CTNMAX-CTNMIN)*VCTN
          ELSE
            VCTP=(VVAR*(ANEG+APOS)-ANEG)/APOS
            CTH=CTPMIN+(CTPMAX-CTPMIN)*VCTP
          ENDIF
        ELSEIF(MVAR.EQ.2) THEN
          RMNMIN=MAX(RM34,RSQM-CTNMIN)
          RMNMAX=MAX(RM34,RSQM-CTNMAX)
          RMPMIN=MAX(RM34,RSQM-CTPMIN)
          RMPMAX=MAX(RM34,RSQM-CTPMAX)
          ANEG=LOG(RMNMIN/RMNMAX)
          APOS=LOG(RMPMIN/RMPMAX)
          IF(ANEG.GT.0..AND.VVAR*(ANEG+APOS).LE.ANEG) THEN
            VCTN=VVAR*(ANEG+APOS)/ANEG
            CTH=RSQM-RMNMIN*(RMNMAX/RMNMIN)**VCTN
          ELSE
            VCTP=(VVAR*(ANEG+APOS)-ANEG)/APOS
            CTH=RSQM-RMPMIN*(RMPMAX/RMPMIN)**VCTP
          ENDIF
        ELSEIF(MVAR.EQ.3) THEN
          RMNMIN=MAX(RM34,RSQM+CTNMIN)
          RMNMAX=MAX(RM34,RSQM+CTNMAX)
          RMPMIN=MAX(RM34,RSQM+CTPMIN)
          RMPMAX=MAX(RM34,RSQM+CTPMAX)
          ANEG=LOG(RMNMAX/RMNMIN)
          APOS=LOG(RMPMAX/RMPMIN)
          IF(ANEG.GT.0..AND.VVAR*(ANEG+APOS).LE.ANEG) THEN
            VCTN=VVAR*(ANEG+APOS)/ANEG
            CTH=RMNMIN*(RMNMAX/RMNMIN)**VCTN-RSQM
          ELSE
            VCTP=(VVAR*(ANEG+APOS)-ANEG)/APOS
            CTH=RMPMIN*(RMPMAX/RMPMIN)**VCTP-RSQM
          ENDIF
        ELSEIF(MVAR.EQ.4) THEN
          RMNMIN=MAX(RM34,RSQM-CTNMIN)
          RMNMAX=MAX(RM34,RSQM-CTNMAX)
          RMPMIN=MAX(RM34,RSQM-CTPMIN)
          RMPMAX=MAX(RM34,RSQM-CTPMAX)
          ANEG=1./RMNMAX-1./RMNMIN
          APOS=1./RMPMAX-1./RMPMIN
          IF(ANEG.GT.0..AND.VVAR*(ANEG+APOS).LE.ANEG) THEN
            VCTN=VVAR*(ANEG+APOS)/ANEG
            CTH=RSQM-1./(1./RMNMIN+ANEG*VCTN)
          ELSE
            VCTP=(VVAR*(ANEG+APOS)-ANEG)/APOS
            CTH=RSQM-1./(1./RMPMIN+APOS*VCTP)
          ENDIF
        ELSEIF(MVAR.EQ.5) THEN
          RMNMIN=MAX(RM34,RSQM+CTNMIN)
          RMNMAX=MAX(RM34,RSQM+CTNMAX)
          RMPMIN=MAX(RM34,RSQM+CTPMIN)
          RMPMAX=MAX(RM34,RSQM+CTPMAX)
          ANEG=1./RMNMIN-1./RMNMAX
          APOS=1./RMPMIN-1./RMPMAX
          IF(ANEG.GT.0..AND.VVAR*(ANEG+APOS).LE.ANEG) THEN
            VCTN=VVAR*(ANEG+APOS)/ANEG
            CTH=1./(1./RMNMIN-ANEG*VCTN)-RSQM
          ELSE
            VCTP=(VVAR*(ANEG+APOS)-ANEG)/APOS
            CTH=1./(1./RMPMIN-APOS*VCTP)-RSQM
          ENDIF
        ENDIF
        IF(CTH.LT.0.) CTH=MIN(CTNMAX,MAX(CTNMIN,CTH))
        IF(CTH.GT.0.) CTH=MIN(CTPMAX,MAX(CTPMIN,CTH))
        VINT(23)=CTH
 
C...Convert VVAR to tau' variable.
      ELSEIF(IVAR.EQ.4) THEN
        TAU=VINT(21)
        TAUPMN=VINT(16)
        TAUPMX=VINT(36)
        IF(MINT(47).EQ.1) THEN
          TAUP=1.
        ELSEIF(MVAR.EQ.1) THEN
          TAUP=TAUPMN*(TAUPMX/TAUPMN)**VVAR
        ELSEIF(MVAR.EQ.2) THEN
          AUPP=(1.-TAU/TAUPMX)**4
          ALOW=(1.-TAU/TAUPMN)**4
          TAUP=TAU/MAX(1E-7,1.-(ALOW+(AUPP-ALOW)*VVAR)**0.25)
        ELSE
          AUPP=LOG(MAX(2E-6,1.-TAUPMX))
          ALOW=LOG(MAX(2E-6,1.-TAUPMN))
          TAUP=1.-EXP(AUPP+VVAR*(ALOW-AUPP))
        ENDIF
        VINT(26)=MIN(TAUPMX,MAX(TAUPMN,TAUP))
 
C...Selection of extra variables needed in 2 -> 3 process:
C...pT1, pT2, phi1, phi2, y3 for three outgoing particles.
C...Since no options are available, the functions of PYKLIM
C...and PYKMAP are joint for these choices.
      ELSEIF(IVAR.EQ.5) THEN
 
C...Read out total energy and particle masses.
        MINT(51)=0
        MPTPK=1
        IF(ISUB.EQ.123.OR.ISUB.EQ.124.OR.ISUB.EQ.173.OR.ISUB.EQ.174
     &  .OR.ISUB.EQ.178.OR.ISUB.EQ.179) MPTPK=2
        SHP=VINT(26)*VINT(2)
        SHPR=SQRT(SHP)
        PM1=VINT(201)
        PM2=VINT(206)
        PM3=SQRT(VINT(21))*VINT(1)
        IF(PM1+PM2+PM3.GT.0.9999*SHPR) THEN
          MINT(51)=1
          RETURN
        ENDIF
        PMRS1=VINT(204)**2
        PMRS2=VINT(209)**2
 
C...Specify coefficients of pT choice; upper and lower limits.
        IF(MPTPK.EQ.1) THEN
          HWT1=0.4
          HWT2=0.4
        ELSE
          HWT1=0.05
          HWT2=0.05
        ENDIF
        HWT3=1.-HWT1-HWT2
        PTSMX1=((SHP-PM1**2-(PM2+PM3)**2)**2-(2.*PM1*(PM2+PM3))**2)/
     &  (4.*SHP)
        IF(CKIN(52).GT.0.) PTSMX1=MIN(PTSMX1,CKIN(52)**2)
        PTSMN1=CKIN(51)**2
        PTSMX2=((SHP-PM2**2-(PM1+PM3)**2)**2-(2.*PM2*(PM1+PM3))**2)/
     &  (4.*SHP)
        IF(CKIN(54).GT.0.) PTSMX2=MIN(PTSMX2,CKIN(54)**2)
        PTSMN2=CKIN(53)**2
 
C...Select transverse momenta according to
C...dp_T^2 * (a + b/(M^2 + p_T^2) + c/(M^2 + p_T^2)^2).
        HMX=PMRS1+PTSMX1
        HMN=PMRS1+PTSMN1
        IF(HMX.LT.1.0001*HMN) THEN
          MINT(51)=1
          RETURN
        ENDIF
        HDE=PTSMX1-PTSMN1
        RPT=RLU(0)
        IF(RPT.LT.HWT1) THEN
          PTS1=PTSMN1+RLU(0)*HDE
        ELSEIF(RPT.LT.HWT1+HWT2) THEN
          PTS1=MAX(PTSMN1,HMN*(HMX/HMN)**RLU(0)-PMRS1)
        ELSE
          PTS1=MAX(PTSMN1,HMN*HMX/(HMN+RLU(0)*HDE)-PMRS1)
        ENDIF
        WTPTS1=HDE/(HWT1+HWT2*HDE/(LOG(HMX/HMN)*(PMRS1+PTS1))+
     &  HWT3*HMN*HMX/(PMRS1+PTS1)**2)
        HMX=PMRS2+PTSMX2
        HMN=PMRS2+PTSMN2
        IF(HMX.LT.1.0001*HMN) THEN
          MINT(51)=1
          RETURN
        ENDIF
        HDE=PTSMX2-PTSMN2
        RPT=RLU(0)
        IF(RPT.LT.HWT1) THEN
          PTS2=PTSMN2+RLU(0)*HDE
        ELSEIF(RPT.LT.HWT1+HWT2) THEN
          PTS2=MAX(PTSMN2,HMN*(HMX/HMN)**RLU(0)-PMRS2)
        ELSE
          PTS2=MAX(PTSMN2,HMN*HMX/(HMN+RLU(0)*HDE)-PMRS2)
        ENDIF
        WTPTS2=HDE/(HWT1+HWT2*HDE/(LOG(HMX/HMN)*(PMRS2+PTS2))+
     &  HWT3*HMN*HMX/(PMRS2+PTS2)**2)
 
C...Select azimuthal angles and check pT choice.
        PHI1=PARU(2)*RLU(0)
        PHI2=PARU(2)*RLU(0)
        PHIR=PHI2-PHI1
        PTS3=MAX(0.,PTS1+PTS2+2.*SQRT(PTS1*PTS2)*COS(PHIR))
        IF(PTS3.LT.CKIN(55)**2.OR.(CKIN(56).GT.0..AND.PTS3.GT.
     &  CKIN(56)**2)) THEN
          MINT(51)=1
          RETURN
        ENDIF
 
C...Calculate transverse masses and check phase space not closed.
        PMS1=PM1**2+PTS1
        PMS2=PM2**2+PTS2
        PMS3=PM3**2+PTS3
        PMT1=SQRT(PMS1)
        PMT2=SQRT(PMS2)
        PMT3=SQRT(PMS3)
        PM12=(PMT1+PMT2)**2
        IF(PMT1+PMT2+PMT3.GT.0.9999*SHPR) THEN
          MINT(51)=1
          RETURN
        ENDIF
 
C...Select rapidity for particle 3 and check phase space not closed.
        Y3MAX=LOG((SHP+PMS3-PM12+SQRT(MAX(0.,(SHP-PMS3-PM12)**2-
     &  4.*PMS3*PM12)))/(2.*SHPR*PMT3))
        IF(Y3MAX.LT.1E-6) THEN
          MINT(51)=1
          RETURN
        ENDIF
        Y3=(2.*RLU(0)-1.)*0.999999*Y3MAX
        PZ3=PMT3*SINH(Y3)
        PE3=PMT3*COSH(Y3)
 
C...Find momentum transfers in two mirror solutions (in 1-2 frame).
        PZ12=-PZ3
        PE12=SHPR-PE3
        PMS12=PE12**2-PZ12**2
        SQL12=SQRT(MAX(0.,(PMS12-PMS1-PMS2)**2-4.*PMS1*PMS2))
        IF(SQL12.LT.1E-6*SHP) THEN
          MINT(51)=1
          RETURN
        ENDIF
        PMM1=PMS12+PMS1-PMS2
        PMM2=PMS12+PMS2-PMS1
        TFAC=-SHPR/(2.*PMS12)
        T1P=TFAC*(PE12-PZ12)*(PMM1-SQL12)
        T1N=TFAC*(PE12-PZ12)*(PMM1+SQL12)
        T2P=TFAC*(PE12+PZ12)*(PMM2-SQL12)
        T2N=TFAC*(PE12+PZ12)*(PMM2+SQL12)
 
C...Construct relative mirror weights and make choice.
        IF(MPTPK.EQ.1) THEN
          WTPU=1.
          WTNU=1.
        ELSE
          WTPU=1./((T1P-PMRS1)*(T2P-PMRS2))**2
          WTNU=1./((T1N-PMRS1)*(T2N-PMRS2))**2
        ENDIF
        WTP=WTPU/(WTPU+WTNU)
        WTN=WTNU/(WTPU+WTNU)
        EPS=1.
        IF(WTN.GT.RLU(0)) EPS=-1.
 
C...Store result of variable choice and associated weights.
        VINT(202)=PTS1
        VINT(207)=PTS2
        VINT(203)=PHI1
        VINT(208)=PHI2
        VINT(205)=WTPTS1
        VINT(210)=WTPTS2
        VINT(211)=Y3
        VINT(212)=Y3MAX
        VINT(213)=EPS
        IF(EPS.GT.0.) THEN
          VINT(214)=1./WTP
          VINT(215)=T1P
          VINT(216)=T2P
        ELSE
          VINT(214)=1./WTN
          VINT(215)=T1N
          VINT(216)=T2N
        ENDIF
        VINT(217)=-0.5*TFAC*(PE12-PZ12)*(PMM2+EPS*SQL12)
        VINT(218)=-0.5*TFAC*(PE12+PZ12)*(PMM1+EPS*SQL12)
        VINT(219)=0.5*(PMS12-PTS3)
        VINT(220)=SQL12
      ENDIF
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYSIGH(NCHN,SIGS)
 
C...Differential matrix elements for all included subprocesses.
C...Note that what is coded is (disregarding the COMFAC factor)
C...1) for 2 -> 1 processes: s-hat/pi*d(sigma-hat), where,
C...when d(sigma-hat) is given in the zero-width limit, the delta
C...function in tau is replaced by a (modified) Breit-Wigner:
C...1/pi*s*H_res/((s*tau-m_res^2)^2+H_res^2),
C...where H_res = s-hat/m_res*Gamma_res(s-hat);
C...2) for 2 -> 2 processes: (s-hat)**2/pi*d(sigma-hat)/d(t-hat);
C...i.e., dimensionless quantities.
C...3) for 2 -> 3 processes: abs(M)^2, where the total cross-section is
C...Integral abs(M)^2/(2shat') * (prod_(i=1)^3 d^3p_i/((2pi)^3*2E_i)) *
C...(2pi)^4 delta^4(P - sum p_i).
C...COMFAC contains the factor pi/s (or equivalent) and
C...the conversion factor from GeV^-2 to mb.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
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
      COMMON/PYINT7/SIGT(0:6,0:6,0:5)
      REAL  SIGT
      SAVE /PYINT7/
      DIMENSION X(2),XPQ(-25:25),KFAC(2,-40:40),WDTP(0:40),
     &WDTE(0:40,0:5),HGZ(6,3),HL3(3),HR3(3),HL4(3),HR4(3)
      COMPLEX A004,A204,A114,A00U,A20U,A11U
      COMPLEX CIGTOT,CIZTOT,F0ALP,F1ALP,F2ALP,F0BET,F1BET,F2BET,FIF,
     &COULCK,COULCP,COULCD,COULCR,COULCS
 
C...The following gives an interface for process 131, gg -> Zqq,
C...to the matrix element package of Ronald Kleiss.
      COMMON/RKBBVC/RKMQ,RKMZ,RKGZ,RKVQ,RKAQ,RKVL,RKAL
      SAVE /RKBBVC/
      DIMENSION RKG1(0:3),RKG2(0:3),RKQ1(0:3),RKQ2(0:3),RKL1(0:3),
     &RKL2(0:3)
 
C...Reset number of channels and cross-section.
      NCHN=0
      SIGS=0.
 
C...Convert H' or A process into equivalent H one.
      ISUB=MINT(1)
      ISUBSV=ISUB
      IHIGG=1
      KFHIGG=25
      IF((ISUB.GE.151.AND.ISUB.LE.160).OR.(ISUB.GE.171.AND.
     &ISUB.LE.190)) THEN
        IHIGG=2
        IF(MOD(ISUB-1,10).GE.5) IHIGG=3
        KFHIGG=33+IHIGG
        IF(ISUB.EQ.151.OR.ISUB.EQ.156) ISUB=3
        IF(ISUB.EQ.152.OR.ISUB.EQ.157) ISUB=102
        IF(ISUB.EQ.153.OR.ISUB.EQ.158) ISUB=103
        IF(ISUB.EQ.171.OR.ISUB.EQ.176) ISUB=24
        IF(ISUB.EQ.172.OR.ISUB.EQ.177) ISUB=26
        IF(ISUB.EQ.173.OR.ISUB.EQ.178) ISUB=123
        IF(ISUB.EQ.174.OR.ISUB.EQ.179) ISUB=124
        IF(ISUB.EQ.181.OR.ISUB.EQ.186) ISUB=121
        IF(ISUB.EQ.182.OR.ISUB.EQ.187) ISUB=122
      ENDIF
 
C...Read kinematical variables and limits.
      ISTSB=ISET(ISUBSV)
      TAUMIN=VINT(11)
      YSTMIN=VINT(12)
      CTNMIN=VINT(13)
      CTPMIN=VINT(14)
      TAUPMN=VINT(16)
      TAU=VINT(21)
      YST=VINT(22)
      CTH=VINT(23)
      XT2=VINT(25)
      TAUP=VINT(26)
      TAUMAX=VINT(31)
      YSTMAX=VINT(32)
      CTNMAX=VINT(33)
      CTPMAX=VINT(34)
      TAUPMX=VINT(36)
 
C...Derive kinematical quantities.
      TAUE=TAU
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUE=TAUP
      X(1)=SQRT(TAUE)*EXP(YST)
      X(2)=SQRT(TAUE)*EXP(-YST)
      IF(MINT(45).EQ.2.AND.ISTSB.GE.1) THEN
        IF(X(1).GT.0.9999) RETURN
      ELSEIF(MINT(45).EQ.3) THEN
        X(1)=MIN(0.9999989,X(1))
      ENDIF
      IF(MINT(46).EQ.2.AND.ISTSB.GE.1) THEN
        IF(X(2).GT.0.9999) RETURN
      ELSEIF(MINT(46).EQ.3) THEN
        X(2)=MIN(0.9999989,X(2))
      ENDIF
      SH=TAU*VINT(2)
      SQM3=VINT(63)
      SQM4=VINT(64)
      RM3=SQM3/SH
      RM4=SQM4/SH
      BE34=SQRT(MAX(0.,(1.-RM3-RM4)**2-4.*RM3*RM4))
      RPTS=4.*VINT(71)**2/SH
      BE34L=SQRT(MAX(0.,(1.-RM3-RM4)**2-4.*RM3*RM4-RPTS))
      RM34=MAX(1E-20,2.*RM3*RM4)
      RSQM=1.+RM34
      IF(2.*VINT(71)**2/(VINT(21)*VINT(2)).LT.0.0001) RM34=MAX(RM34,
     &2.*VINT(71)**2/(VINT(21)*VINT(2)))
      RTHM=(4.*RM3*RM4+RPTS)/(1.-RM3-RM4+BE34L)
      IF(ISTSB.EQ.0) THEN
        TH=VINT(45)
        UH=-0.5*SH*MAX(RTHM,1.-RM3-RM4+BE34*CTH)
        SQPTH=MAX(VINT(71)**2,0.25*SH*BE34**2*VINT(59)**2)
      ELSE
        TH=-0.5*SH*MAX(RTHM,1.-RM3-RM4-BE34*CTH)
        UH=-0.5*SH*MAX(RTHM,1.-RM3-RM4+BE34*CTH)
        SQPTH=MAX(VINT(71)**2,0.25*SH*BE34**2*(1.-CTH**2))
      ENDIF
      SH2=SH**2
      TH2=TH**2
      UH2=UH**2
 
C...Choice of Q2 scale: hard, structure functions, parton showers.
      IF(ISTSB.EQ.1.OR.ISTSB.EQ.3.OR.ISTSB.EQ.5) THEN
        Q2=SH
      ELSEIF(MOD(ISTSB,2).EQ.0.OR.ISTSB.EQ.9) THEN
        IF(MSTP(32).EQ.1) THEN
          Q2=2.*SH*TH*UH/(SH**2+TH**2+UH**2)
        ELSEIF(MSTP(32).EQ.2) THEN
          Q2=SQPTH+0.5*(SQM3+SQM4)
        ELSEIF(MSTP(32).EQ.3) THEN
          Q2=MIN(-TH,-UH)
        ELSEIF(MSTP(32).EQ.4) THEN
          Q2=SH
        ELSEIF(MSTP(32).EQ.5) THEN
          Q2=-TH
        ENDIF
        IF(ISTSB.EQ.9) Q2=SQPTH
        IF((ISTSB.EQ.9.AND.MSTP(82).GE.2).OR.(ISTSB.NE.9.AND.
     &  MSTP(85).EQ.1)) Q2=Q2+PARP(82)**2
      ENDIF
      Q2SF=Q2
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) THEN
        Q2SF=PMAS(23,1)**2
        IF(ISUB.EQ.8.OR.ISUB.EQ.76.OR.ISUB.EQ.77.OR.ISUB.EQ.124)
     &  Q2SF=PMAS(24,1)**2
        IF(ISUB.EQ.121.OR.ISUB.EQ.122) THEN
          Q2SF=PMAS(KFPR(ISUBSV,2),1)**2
          IF(MSTP(39).EQ.2) Q2SF=Q2SF+MAX(VINT(202),VINT(207))
          IF(MSTP(39).EQ.3) Q2SF=SH
          IF(MSTP(39).EQ.4) Q2SF=VINT(26)*VINT(2)
        ENDIF
      ENDIF
      Q2PS=Q2SF
      Q2SF=Q2SF*PARP(34)
      IF(MSTP(68).GE.2.AND.MINT(47).EQ.5) Q2SF=VINT(2)
      IF(MSTP(22).GE.1.AND.(ISUB.EQ.10.OR.ISUB.EQ.83).AND.
     &(MINT(43).EQ.2.OR.MINT(43).EQ.3)) THEN
        XBJ=X(2)
        IF(MINT(43).EQ.3) XBJ=X(1)
        IF(MSTP(22).EQ.1) THEN
          Q2PS=-TH
        ELSEIF(MSTP(22).EQ.2) THEN
          Q2PS=((1.-XBJ)/XBJ)*(-TH)
        ELSEIF(MSTP(22).EQ.3) THEN
          Q2PS=SQRT((1.-XBJ)/XBJ)*(-TH)
        ELSE
          Q2PS=(1.-XBJ)*MAX(1.,-LOG(XBJ))*(-TH)
        ENDIF
      ENDIF
      IF(MSTP(68).GE.1.AND.MINT(47).EQ.5) Q2PS=VINT(2)
 
C...Store derived kinematical quantities.
      VINT(41)=X(1)
      VINT(42)=X(2)
      VINT(44)=SH
      VINT(43)=SQRT(SH)
      VINT(45)=TH
      VINT(46)=UH
      VINT(48)=SQPTH
      VINT(47)=SQRT(SQPTH)
      VINT(50)=TAUP*VINT(2)
      VINT(49)=SQRT(MAX(0.,VINT(50)))
      VINT(52)=Q2
      VINT(51)=SQRT(Q2)
      VINT(54)=Q2SF
      VINT(53)=SQRT(Q2SF)
      VINT(56)=Q2PS
      VINT(55)=SQRT(Q2PS)
 
C...Calculate parton structure functions.
      IF(ISTSB.LE.0) GOTO 160
      IF(MINT(47).GE.2) THEN
        DO 110 I=3-MIN(2,MINT(45)),MIN(2,MINT(46))
        XSF=X(I)
        IF(ISTSB.EQ.9) XSF=X(I)/VINT(142+I)
        MINT(105)=MINT(102+I)
        MINT(109)=MINT(106+I)
        IF(MSTP(57).LE.1) THEN
          CALL PYSTFU(MINT(10+I),XSF,Q2SF,XPQ)
        ELSE
          CALL PYSTFL(MINT(10+I),XSF,Q2SF,XPQ)
        ENDIF
        DO 100 KFL=-25,25
        XSFX(I,KFL)=XPQ(KFL)
  100   CONTINUE
  110   CONTINUE
      ENDIF
 
C...Calculate alpha_em, alpha_strong and K-factor.
      XW=PARU(102)
      XWV=XW
      IF(MSTP(8).GE.2.OR.(ISUB.GE.71.AND.ISUB.LE.77)) XW=
     &1.-(PMAS(24,1)/PMAS(23,1))**2
      XW1=1.-XW
      XWC=1./(16.*XW*XW1)
      AEM=ULALEM(Q2)
      IF(MSTP(8).GE.1) AEM=SQRT(2.)*PARU(105)*PMAS(24,1)**2*XW/PARU(1)
      IF(MSTP(33).NE.3) AS=ULALPS(PARP(34)*Q2)
      FACK=1.
      FACA=1.
      IF(MSTP(33).EQ.1) THEN
        FACK=PARP(31)
      ELSEIF(MSTP(33).EQ.2) THEN
        FACK=PARP(31)
        FACA=PARP(32)/PARP(31)
      ELSEIF(MSTP(33).EQ.3) THEN
        Q2AS=PARP(33)*Q2
        IF(ISTSB.EQ.9.AND.MSTP(82).GE.2) Q2AS=Q2AS+
     &  PARU(112)*PARP(82)
        AS=ULALPS(Q2AS)
      ENDIF
      VINT(138)=1.
      VINT(57)=AEM
      VINT(58)=AS
 
C...Set flags for allowed reacting partons/leptons.
      DO 140 I=1,2
      DO 120 J=-25,25
      KFAC(I,J)=0
  120 CONTINUE
      IF(MINT(44+I).EQ.1) THEN
        KFAC(I,MINT(10+I))=1
      ELSEIF(MINT(40+I).EQ.1.AND.MSTP(12).EQ.0) THEN
        KFAC(I,MINT(10+I))=1
        KFAC(I,22)=1
        KFAC(I,24)=1
        KFAC(I,-24)=1
      ELSE
        DO 130 J=-25,25
        KFAC(I,J)=KFIN(I,J)
        IF(IABS(J).GT.MSTP(58).AND.IABS(J).LE.10) KFAC(I,J)=0
        IF(XSFX(I,J).LT.1E-10) KFAC(I,J)=0
  130   CONTINUE
      ENDIF
  140 CONTINUE
 
C...Lower and upper limit for fermion flavour loops.
      MMIN1=0
      MMAX1=0
      MMIN2=0
      MMAX2=0
      DO 150 J=-20,20
      IF(KFAC(1,-J).EQ.1) MMIN1=-J
      IF(KFAC(1,J).EQ.1) MMAX1=J
      IF(KFAC(2,-J).EQ.1) MMIN2=-J
      IF(KFAC(2,J).EQ.1) MMAX2=J
  150 CONTINUE
      MMINA=MIN(MMIN1,MMIN2)
      MMAXA=MAX(MMAX1,MMAX2)
 
C...Common conversion factors (including Jacobian) for subprocesses.
      SQMZ=PMAS(23,1)**2
      SQMW=PMAS(24,1)**2
      SQMH=PMAS(KFHIGG,1)**2
      GMMH=PMAS(KFHIGG,1)*PMAS(KFHIGG,2)
      SQMZP=PMAS(32,1)**2
      SQMWP=PMAS(34,1)**2
      SQMHC=PMAS(37,1)**2
      SQMLQ=PMAS(39,1)**2
      SQMR=PMAS(40,1)**2
 
C...Phase space integral in tau.
      COMFAC=PARU(1)*PARU(5)/VINT(2)
      IF(MINT(41).EQ.2.AND.MINT(42).EQ.2) COMFAC=COMFAC*FACK
      IF((MINT(47).GE.2.OR.(ISTSB.GE.3.AND.ISTSB.LE.5)).AND.
     &ISTSB.NE.9) THEN
        ATAU1=LOG(TAUMAX/TAUMIN)
        ATAU2=(TAUMAX-TAUMIN)/(TAUMAX*TAUMIN)
        H1=COEF(ISUBSV,1)+(ATAU1/ATAU2)*COEF(ISUBSV,2)/TAU
        IF(MINT(72).GE.1) THEN
          TAUR1=VINT(73)
          GAMR1=VINT(74)
          ATAUD=LOG(TAUMAX/TAUMIN*(TAUMIN+TAUR1)/(TAUMAX+TAUR1))
          ATAU3=ATAUD/TAUR1
          IF(ATAUD.GT.1E-6) H1=H1+
     &    (ATAU1/ATAU3)*COEF(ISUBSV,3)/(TAU+TAUR1)
          ATAUD=ATAN((TAUMAX-TAUR1)/GAMR1)-ATAN((TAUMIN-TAUR1)/GAMR1)
          ATAU4=ATAUD/GAMR1
          IF(ATAUD.GT.1E-6) H1=H1+
     &    (ATAU1/ATAU4)*COEF(ISUBSV,4)*TAU/((TAU-TAUR1)**2+GAMR1**2)
        ENDIF
        IF(MINT(72).EQ.2) THEN
          TAUR2=VINT(75)
          GAMR2=VINT(76)
          ATAUD=LOG(TAUMAX/TAUMIN*(TAUMIN+TAUR2)/(TAUMAX+TAUR2))
          ATAU5=ATAUD/TAUR2
          IF(ATAUD.GT.1E-6) H1=H1+
     &    (ATAU1/ATAU5)*COEF(ISUBSV,5)/(TAU+TAUR2)
          ATAUD=ATAN((TAUMAX-TAUR2)/GAMR2)-ATAN((TAUMIN-TAUR2)/GAMR2)
          ATAU6=ATAUD/GAMR2
          IF(ATAUD.GT.1E-6) H1=H1+
     &    (ATAU1/ATAU6)*COEF(ISUBSV,6)*TAU/((TAU-TAUR2)**2+GAMR2**2)
        ENDIF
        IF(MINT(47).EQ.5.AND.(ISTSB.LE.2.OR.ISTSB.GE.6)) THEN
          ATAU7=LOG(MAX(2E-6,1.-TAUMIN)/MAX(2E-6,1.-TAUMAX))
          IF(ATAU7.GT.1E-6) H1=H1+(ATAU1/ATAU7)*COEF(ISUBSV,7)*TAU/
     &    MAX(2E-6,1.-TAU)
        ENDIF
        COMFAC=COMFAC*ATAU1/(TAU*H1)
      ENDIF
 
C...Phase space integral in y*.
      IF(MINT(47).GE.4.AND.ISTSB.NE.9) THEN
        AYST0=YSTMAX-YSTMIN
        IF(AYST0.LT.1E-6) THEN
          COMFAC=0.
        ELSE
          AYST1=0.5*(YSTMAX-YSTMIN)**2
          AYST2=AYST1
          AYST3=2.*(ATAN(EXP(YSTMAX))-ATAN(EXP(YSTMIN)))
          H2=(AYST0/AYST1)*COEF(ISUBSV,8)*(YST-YSTMIN)+
     &    (AYST0/AYST2)*COEF(ISUBSV,9)*(YSTMAX-YST)+
     &    (AYST0/AYST3)*COEF(ISUBSV,10)/COSH(YST)
          IF(MINT(45).EQ.3) THEN
            YST0=-0.5*LOG(TAUE)
            AYST4=LOG(MAX(1E-6,EXP(YST0-YSTMIN)-1.)/
     &      MAX(1E-6,EXP(YST0-YSTMAX)-1.))
            IF(AYST4.GT.1E-6) H2=H2+(AYST0/AYST4)*COEF(ISUBSV,11)/
     &      MAX(1E-6,1.-EXP(YST-YST0))
          ENDIF
          IF(MINT(46).EQ.3) THEN
            YST0=-0.5*LOG(TAUE)
            AYST5=LOG(MAX(1E-6,EXP(YST0+YSTMAX)-1.)/
     &      MAX(1E-6,EXP(YST0+YSTMIN)-1.))
            IF(AYST5.GT.1E-6) H2=H2+(AYST0/AYST5)*COEF(ISUBSV,12)/
     &      MAX(1E-6,1.-EXP(-YST-YST0))
          ENDIF
          COMFAC=COMFAC*AYST0/H2
        ENDIF
      ENDIF
 
C...2 -> 1 processes: reduction in angular part of phase space integral
C...for case of decaying resonance.
      ACTH0=CTNMAX-CTNMIN+CTPMAX-CTPMIN
      IF((ISTSB.EQ.1.OR.ISTSB.EQ.3.OR.ISTSB.EQ.5)) THEN
        IF(MDCY(KFPR(ISUBSV,1),1).EQ.1) THEN
          IF(KFPR(ISUB,1).EQ.25.OR.KFPR(ISUB,1).EQ.37.OR.
     &    KFPR(ISUB,1).EQ.39) THEN
            COMFAC=COMFAC*0.5*ACTH0
          ELSE
            COMFAC=COMFAC*0.125*(3.*ACTH0+CTNMAX**3-CTNMIN**3+
     &      CTPMAX**3-CTPMIN**3)
          ENDIF
        ENDIF
 
C...2 -> 2 processes: angular part of phase space integral.
      ELSEIF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) THEN
        ACTH1=LOG((MAX(RM34,RSQM-CTNMIN)*MAX(RM34,RSQM-CTPMIN))/
     &  (MAX(RM34,RSQM-CTNMAX)*MAX(RM34,RSQM-CTPMAX)))
        ACTH2=LOG((MAX(RM34,RSQM+CTNMAX)*MAX(RM34,RSQM+CTPMAX))/
     &  (MAX(RM34,RSQM+CTNMIN)*MAX(RM34,RSQM+CTPMIN)))
        ACTH3=1./MAX(RM34,RSQM-CTNMAX)-1./MAX(RM34,RSQM-CTNMIN)+
     &  1./MAX(RM34,RSQM-CTPMAX)-1./MAX(RM34,RSQM-CTPMIN)
        ACTH4=1./MAX(RM34,RSQM+CTNMIN)-1./MAX(RM34,RSQM+CTNMAX)+
     &  1./MAX(RM34,RSQM+CTPMIN)-1./MAX(RM34,RSQM+CTPMAX)
        H3=COEF(ISUBSV,13)+
     &  (ACTH0/ACTH1)*COEF(ISUBSV,14)/MAX(RM34,RSQM-CTH)+
     &  (ACTH0/ACTH2)*COEF(ISUBSV,15)/MAX(RM34,RSQM+CTH)+
     &  (ACTH0/ACTH3)*COEF(ISUBSV,16)/MAX(RM34,RSQM-CTH)**2+
     &  (ACTH0/ACTH4)*COEF(ISUBSV,17)/MAX(RM34,RSQM+CTH)**2
        COMFAC=COMFAC*ACTH0*0.5*BE34/H3
 
C...2 -> 2 processes: take into account final state Breit-Wigners.
        COMFAC=COMFAC*VINT(80)
      ENDIF
 
C...2 -> 3, 4 processes: phace space integral in tau'.
      IF(MINT(47).GE.2.AND.ISTSB.GE.3.AND.ISTSB.LE.5) THEN
        ATAUP1=LOG(TAUPMX/TAUPMN)
        ATAUP2=((1.-TAU/TAUPMX)**4-(1.-TAU/TAUPMN)**4)/(4.*TAU)
        H4=COEF(ISUBSV,18)+
     &  (ATAUP1/ATAUP2)*COEF(ISUBSV,19)*(1.-TAU/TAUP)**3/TAUP
        IF(MINT(47).EQ.5) THEN
          ATAUP3=LOG(MAX(2E-6,1.-TAUPMN)/MAX(2E-6,1.-TAUPMX))
          H4=H4+(ATAUP1/ATAUP3)*COEF(ISUBSV,20)*TAUP/MAX(2E-6,1.-TAUP)
        ENDIF
        COMFAC=COMFAC*ATAUP1/H4
      ENDIF
 
C...2 -> 3, 4 processes: effective W/Z structure functions.
      IF(ISTSB.EQ.3.OR.ISTSB.EQ.4) THEN
        IF(1.-TAU/TAUP.GT.1.E-4) THEN
          FZW=(1.+TAU/TAUP)*LOG(TAUP/TAU)-2.*(1.-TAU/TAUP)
        ELSE
          FZW=1./6.*(1.-TAU/TAUP)**3*TAU/TAUP
        ENDIF
        COMFAC=COMFAC*FZW
      ENDIF
 
C...2 -> 3 processes: phase space integrals for pT1, pT2, y3, mirror.
      IF(ISTSB.EQ.5) THEN
        COMFAC=COMFAC*VINT(205)*VINT(210)*VINT(212)*VINT(214)/
     &  (128.*PARU(1)**4*VINT(220))*(TAU**2/TAUP)
      ENDIF
 
C...2 -> 2 processes: optional dampening by pT^4/(pT0^2+pT^2)^2.
      IF(MSTP(85).EQ.1.AND.MOD(ISTSB,2).EQ.0) COMFAC=COMFAC*
     &SQPTH**2/(PARP(82)**2+SQPTH)**2
 
C...gamma + gamma: include factor 2 when different nature.
      IF(MINT(11).EQ.22.AND.MINT(12).EQ.22.AND.MINT(123).GE.4)
     &COMFAC=2.*COMFAC
 
C...Phase space integral for low-pT and multiple interactions.
      IF(ISTSB.EQ.9) THEN
        COMFAC=PARU(1)*PARU(5)*FACK*0.5*VINT(2)/SH2
        ATAU1=LOG(2.*(1.+SQRT(1.-XT2))/XT2-1.)
        ATAU2=2.*ATAN(1./XT2-1.)/SQRT(XT2)
        H1=COEF(ISUBSV,1)+(ATAU1/ATAU2)*COEF(ISUBSV,2)/SQRT(TAU)
        COMFAC=COMFAC*ATAU1/H1
        AYST0=YSTMAX-YSTMIN
        AYST1=0.5*(YSTMAX-YSTMIN)**2
        AYST3=2.*(ATAN(EXP(YSTMAX))-ATAN(EXP(YSTMIN)))
        H2=(AYST0/AYST1)*COEF(ISUBSV,8)*(YST-YSTMIN)+
     &  (AYST0/AYST1)*COEF(ISUBSV,9)*(YSTMAX-YST)+
     &  (AYST0/AYST3)*COEF(ISUBSV,10)/COSH(YST)
        COMFAC=COMFAC*AYST0/H2
        IF(MSTP(82).LE.1) COMFAC=COMFAC*XT2**2*(1./VINT(149)-1.)
C...For MSTP(82)>=2 an additional factor (xT2/(xT2+VINT(149))**2 is
C...introduced to make cross-section finite for xT2 -> 0.
        IF(MSTP(82).GE.2) COMFAC=COMFAC*XT2**2/(VINT(149)*
     &  (1.+VINT(149)))
      ENDIF
 
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron.
      IF((MSTP(46).GE.3.AND.MSTP(46).LE.6).AND.(ISUB.EQ.71.OR.ISUB.EQ.
     &72.OR.ISUB.EQ.73.OR.ISUB.EQ.76.OR.ISUB.EQ.77)) THEN
C...Calculate M_R and N_R functions for Higgs-like and QCD-like models.
        IF(MSTP(46).LE.4) THEN
          HDTLH=LOG(PMAS(25,1)/PARP(44))
          HDTMR=(4.5*PARU(1)/SQRT(3.)-74./9.)/8.+HDTLH/12.
          HDTNR=-1./18.+HDTLH/6.
        ELSE
          HDTNM=0.125*(1./(288.*PARU(1)**2)+(PARP(47)/PARP(45))**2)
          HDTLQ=LOG(PARP(45)/PARP(44))
          HDTMR=-(4.*PARU(1))**2*0.5*HDTNM+HDTLQ/12.
          HDTNR=(4.*PARU(1))**2*HDTNM+HDTLQ/6.
        ENDIF
 
C...Calculate lowest and next-to-lowest order partial wave amplitudes.
        HDTV=1./(16.*PARU(1)*PARP(47)**2)
        A00L=HDTV*SH
        A20L=-0.5*A00L
        A11L=A00L/6.
        HDTLS=LOG(SH/PARP(44)**2)
        A004=(HDTV*SH)**2/(4.*PARU(1))*CMPLX((176.*HDTMR+112.*HDTNR)/3.+
     &  11./27.-(50./9.)*HDTLS,4.*PARU(1))
        A204=(HDTV*SH)**2/(4.*PARU(1))*CMPLX(32.*(HDTMR+2.*HDTNR)/3.+
     &  25./54.-(20./9.)*HDTLS,PARU(1))
        A114=(HDTV*SH)**2/(6.*PARU(1))*CMPLX(4.*(-2.*HDTMR+HDTNR)-
     &  1./18.,PARU(1)/6.)
 
C...Unitarize partial wave amplitudes with Pade or K-matrix method.
        IF(MSTP(46).EQ.3.OR.MSTP(46).EQ.5) THEN
          A00U=A00L/(1.-A004/A00L)
          A20U=A20L/(1.-A204/A20L)
          A11U=A11L/(1.-A114/A11L)
        ELSE
          A00U=(A00L+REAL(A004))/(1.-CMPLX(0.,A00L+REAL(A004)))
          A20U=(A20L+REAL(A204))/(1.-CMPLX(0.,A20L+REAL(A204)))
          A11U=(A11L+REAL(A114))/(1.-CMPLX(0.,A11L+REAL(A114)))
        ENDIF
      ENDIF
 
C...A: 2 -> 1, tree diagrams.
 
  160 IF(ISUB.LE.10) THEN
      IF(ISUB.EQ.1) THEN
C...f + f~ -> gamma*/Z0.
        MINT(61)=2
        CALL PYWIDT(23,SH,WDTP,WDTE)
        HP0=AEM/3.*SH
        HP1=AEM/3.*XWC*SH
        HS=HP1*WDTP(0)
        FACZ=4.*COMFAC*3.
        DO 170 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 170
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        HI0=HP0
        IF(IABS(I).LE.10) HI0=HI0*FACA/3.
        HI1=HP1
        IF(IABS(I).LE.10) HI1=HI1*FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZ*(EI**2/SH2*HI0*HP0*VINT(111)+EI*VI*(1.-SQMZ/SH)/
     &  ((SH-SQMZ)**2+HS**2)*(HI0*HP1+HI1*HP0)*VINT(112)+
     &  (VI**2+AI**2)/((SH-SQMZ)**2+HS**2)*HI1*HP1*VINT(114))
  170   CONTINUE
 
      ELSEIF(ISUB.EQ.2) THEN
C...f + f~' -> W+/-.
        CALL PYWIDT(24,SH,WDTP,WDTE)
        HP=AEM/(24.*XW)*SH
        HS=HP*WDTP(0)
        FACBW=4.*COMFAC/((SH-SQMW)**2+HS**2)*3.
        DO 190 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 190
        IA=IABS(I)
        DO 180 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 180
        JA=IABS(J)
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 180
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 180
        KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        HI=HP*2.
        IF(IA.LE.10) HI=HI*VCKM((IA+1)/2,(JA+1)/2)*FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        HF=HP*(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))
        SIGH(NCHN)=HI*FACBW*HF
  180   CONTINUE
  190   CONTINUE
 
      ELSEIF(ISUB.EQ.3) THEN
C...f + f~ -> H0 (or H'0, or A0).
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=4.*COMFAC/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        DO 200 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 200
        IA=IABS(I)
        RMQ=PMAS(IA,1)**2/SH
        HI=HP*RMQ
        IF(IA.LE.10) HI=HP*RMQ*FACA/3.
        IF(IA.LE.10.AND.MSTP(37).EQ.1.AND.MSTP(2).GE.1) HI=HI*
     &  (LOG(MAX(4.,PARP(37)**2*RMQ*SH/PARU(117)**2))/
     &  LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
          IKFI=1
          IF(IA.LE.10.AND.MOD(IA,2).EQ.0) IKFI=2
          IF(IA.GT.10) IKFI=3
          HI=HI*PARU(150+10*IHIGG+IKFI)**2
        ENDIF
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
  200   CONTINUE
 
      ELSEIF(ISUB.EQ.4) THEN
C...gamma + W+/- -> W+/-.
 
      ELSEIF(ISUB.EQ.5) THEN
C...Z0 + Z0 -> H0.
        CALL PYWIDT(25,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=4.*COMFAC/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        HI=HP/4.
        FACI=8./(PARU(1)**2*XW1)*(AEM*XWC)**2
        DO 220 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 220
        DO 210 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 210
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        EJ=KCHG(IABS(J),1)/3.
        AJ=SIGN(1.,EJ)
        VJ=AJ-4.*EJ*XWV
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACI*(VI**2+AI**2)*(VJ**2+AJ**2)*HI*FACBW*HF
  210   CONTINUE
  220   CONTINUE
 
      ELSEIF(ISUB.EQ.6) THEN
C...Z0 + W+/- -> W+/-.
 
      ELSEIF(ISUB.EQ.7) THEN
C...W+ + W- -> Z0.
 
      ELSEIF(ISUB.EQ.8) THEN
C...W+ + W- -> H0.
        CALL PYWIDT(25,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=4.*COMFAC/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        HI=HP/2.
        FACI=1./(4.*PARU(1)**2)*(AEM/XW)**2
        DO 240 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 240
        EI=SIGN(1.,FLOAT(I))*KCHG(IABS(I),1)
        DO 230 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 230
        EJ=SIGN(1.,FLOAT(J))*KCHG(IABS(J),1)
        IF(EI*EJ.GT.0.) GOTO 230
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACI*VINT(180+I)*VINT(180+J)*HI*FACBW*HF
  230   CONTINUE
  240   CONTINUE
 
C...B: 2 -> 2, tree diagrams.
 
      ELSEIF(ISUB.EQ.10) THEN
C...f + f' -> f + f' (gamma/Z/W exchange).
        FACGGF=COMFAC*AEM**2*2.*(SH2+UH2)/TH2
        FACGZF=COMFAC*AEM**2*XWC*4.*SH2/(TH*(TH-SQMZ))
        FACZZF=COMFAC*(AEM*XWC)**2*2.*SH2/(TH-SQMZ)**2
        FACWWF=COMFAC*(0.5*AEM/XW)**2*SH2/(TH-SQMW)**2
        DO 260 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 260
        IA=IABS(I)
        DO 250 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 250
        JA=IABS(J)
C...Electroweak couplings.
        EI=KCHG(IA,1)*ISIGN(1,I)/3.
        AI=SIGN(1.,KCHG(IA,1)+0.5)*ISIGN(1,I)
        VI=AI-4.*EI*XWV
        EJ=KCHG(JA,1)*ISIGN(1,J)/3.
        AJ=SIGN(1.,KCHG(JA,1)+0.5)*ISIGN(1,J)
        VJ=AJ-4.*EJ*XWV
        EPSIJ=ISIGN(1,I*J)
C...gamma/Z exchange, only gamma exchange, or only Z exchange.
        IF(MSTP(21).GE.1.AND.MSTP(21).LE.4) THEN
          IF(MSTP(21).EQ.1.OR.MSTP(21).EQ.4) THEN
            FACNCF=FACGGF*EI**2*EJ**2+FACGZF*EI*EJ*
     &      (VI*VJ*(1.+UH2/SH2)+AI*AJ*EPSIJ*(1.-UH2/SH2))+
     &      FACZZF*((VI**2+AI**2)*(VJ**2+AJ**2)*(1.+UH2/SH2)+
     &      4.*VI*VJ*AI*AJ*EPSIJ*(1.-UH2/SH2))
          ELSEIF(MSTP(21).EQ.2) THEN
            FACNCF=FACGGF*EI**2*EJ**2
          ELSE
            FACNCF=FACZZF*((VI**2+AI**2)*(VJ**2+AJ**2)*(1.+UH2/SH2)+
     &      4.*VI*VJ*AI*AJ*EPSIJ*(1.-UH2/SH2))
          ENDIF
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACNCF
        ENDIF
C...W exchange.
        IF((MSTP(21).EQ.1.OR.MSTP(21).EQ.5).AND.AI*AJ.LT.0.) THEN
          FACCCF=FACWWF*VINT(180+I)*VINT(180+J)
          IF(EPSIJ.LT.0.) FACCCF=FACCCF*UH2/SH2
          IF(IA.GT.10.AND.MOD(IA,2).EQ.0) FACCCF=2.*FACCCF
          IF(JA.GT.10.AND.MOD(JA,2).EQ.0) FACCCF=2.*FACCCF
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACCCF
        ENDIF
  250   CONTINUE
  260   CONTINUE
      ENDIF
 
      ELSEIF(ISUB.LE.20) THEN
      IF(ISUB.EQ.11) THEN
C...f + f' -> f + f' (g exchange).
        FACQQ1=COMFAC*AS**2*4./9.*(SH2+UH2)/TH2
        FACQQB=COMFAC*AS**2*4./9.*((SH2+UH2)/TH2*FACA-
     &  MSTP(34)*2./3.*UH2/(SH*TH))
        FACQQ2=COMFAC*AS**2*4./9.*((SH2+TH2)/UH2-
     &  MSTP(34)*2./3.*SH2/(TH*UH))
        IF(MSTP(5).GE.1) THEN
C...Modifications from contact interactions (compositeness).
          FACCI1=FACQQ1+COMFAC*(SH2/PARU(155)**4)
          FACCIB=FACQQB+COMFAC*(8./9.)*(AS*PARU(156)/PARU(155)**2)*
     &    (UH2/TH+UH2/SH)+COMFAC*(5./3.)*(UH2/PARU(155)**4)
          FACCI2=FACQQ2+COMFAC*(8./9.)*(AS*PARU(156)/PARU(155)**2)*
     &    (SH2/TH+SH2/UH)+COMFAC*(5./3.)*(SH2/PARU(155)**4)
          FACCI3=FACQQ1+COMFAC*(UH2/PARU(155)**4)
        ENDIF
        DO 280 I=MMIN1,MMAX1
        IA=IABS(I)
        IF(I.EQ.0.OR.IA.GT.MSTP(58).OR.KFAC(1,I).EQ.0) GOTO 280
        DO 270 J=MMIN2,MMAX2
        JA=IABS(J)
        IF(J.EQ.0.OR.JA.GT.MSTP(58).OR.KFAC(2,J).EQ.0) GOTO 270
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        IF(MSTP(5).LE.0.OR.(MSTP(5).EQ.1.AND.(IA.GE.3.OR.JA.GE.3)))
     &  THEN
          SIGH(NCHN)=FACQQ1
          IF(I.EQ.-J) SIGH(NCHN)=FACQQB
        ELSE
          SIGH(NCHN)=FACCI1
          IF(I*J.LT.0) SIGH(NCHN)=FACCI3
          IF(I.EQ.-J) SIGH(NCHN)=FACCIB
        ENDIF
        IF(I.EQ.J) THEN
          SIGH(NCHN)=0.5*SIGH(NCHN)
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=2
          IF(MSTP(5).LE.0.OR.(MSTP(5).EQ.1.AND.IA.GE.3)) THEN
            SIGH(NCHN)=0.5*FACQQ2
          ELSE
            SIGH(NCHN)=0.5*FACCI2
          ENDIF
        ENDIF
  270   CONTINUE
  280   CONTINUE
 
      ELSEIF(ISUB.EQ.12) THEN
C...f + f~ -> f' + f~' (q + q~ -> q' + q~' only).
        CALL PYWIDT(21,SH,WDTP,WDTE)
        FACQQB=COMFAC*AS**2*4./9.*(TH2+UH2)/SH2*(WDTE(0,1)+WDTE(0,2)+
     &  WDTE(0,4))
        IF(MSTP(5).EQ.1) THEN
C...Modifications from contact interactions (compositeness).
          FACCIB=FACQQB
          DO 290 I=1,2
          FACCIB=FACCIB+COMFAC*(UH2/PARU(155)**4)*(WDTE(I,1)+WDTE(I,2)+
     &    WDTE(I,4))
  290     CONTINUE
        ELSEIF(MSTP(5).GE.2) THEN
          FACCIB=FACQQB+COMFAC*(UH2/PARU(155)**4)*(WDTE(0,1)+WDTE(0,2)+
     &    WDTE(0,4))
        ENDIF
        DO 300 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 300
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        IF(MSTP(5).LE.0.OR.(MSTP(5).EQ.1.AND.IABS(I).GE.3)) THEN
          SIGH(NCHN)=FACQQB
        ELSE
          SIGH(NCHN)=FACCIB
        ENDIF
  300   CONTINUE
 
      ELSEIF(ISUB.EQ.13) THEN
C...f + f~ -> g + g (q + q~ -> g + g only).
        FACGG1=COMFAC*AS**2*32./27.*(UH/TH-(2.+MSTP(34)*1./4.)*UH2/SH2)
        FACGG2=COMFAC*AS**2*32./27.*(TH/UH-(2.+MSTP(34)*1./4.)*TH2/SH2)
        DO 310 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 310
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=0.5*FACGG1
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=2
        SIGH(NCHN)=0.5*FACGG2
  310   CONTINUE
 
      ELSEIF(ISUB.EQ.14) THEN
C...f + f~ -> g + gamma (q + q~ -> g + gamma only).
        FACGG=COMFAC*AS*AEM*8./9.*(TH2+UH2)/(TH*UH)
        DO 320 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 320
        EI=KCHG(IABS(I),1)/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGG*EI**2
  320   CONTINUE
 
      ELSEIF(ISUB.EQ.15) THEN
C...f + f~ -> g + (gamma*/Z0) (q + q~ -> g + (gamma*/Z0) only).
        FACZG=COMFAC*AS*AEM*(8./9.)*(TH2+UH2+2.*SQM4*SH)/(TH*UH)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs.
        HFGG=0.
        HFGZ=0.
        HFZZ=0.
        HBW4=0.
        RADC4=1.+ULALPS(SQM4)/PARU(1)
        DO 330 I=1,MIN(16,MDCY(23,3))
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 330
        IMDM=0
        IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &  IMDM=1
        IF(I.LE.8) THEN
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ELSEIF(I.LE.16) THEN
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ENDIF
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
        IF(4.*RM1.LT.1.) THEN
          FCOF=1.
          IF(I.LE.8) FCOF=3.*RADC4
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(IMDM.EQ.1) THEN
            HFGG=HFGG+FCOF*EF**2*(1.+2.*RM1)*BE34
            HFGZ=HFGZ+FCOF*EF*VF*(1.+2.*RM1)*BE34
            HFZZ=HFZZ+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
          ENDIF
          HBW4=HBW4+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ENDIF
  330   CONTINUE
C...Propagators: as simulated in PYOFSH and as desired.
        GMMZ=PMAS(23,1)*PMAS(23,2)
        HBW4=HBW4*XWC*SQMZ/((SQM4-SQMZ)**2+GMMZ**2)
        MINT(15)=1
        MINT(61)=1
        CALL PYWIDT(23,SQM4,WDTP,WDTE)
        HFGG=HFGG*VINT(111)/SQM4
        HFGZ=HFGZ*VINT(112)/SQM4
        HFZZ=HFZZ*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure.
        DO 340 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 340
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZG*(EI**2*HFGG+EI*VI*HFGZ+
     &  (VI**2+AI**2)*HFZZ)/HBW4
  340   CONTINUE
 
      ELSEIF(ISUB.EQ.16) THEN
C...f + f~' -> g + W+/- (q + q~' -> g + W+/- only).
        FACWG=COMFAC*AS*AEM/XW*2./9.*(TH2+UH2+2.*SQM4*SH)/(TH*UH)
C...Propagators: as simulated in PYOFSH and as desired.
        GMMW=PMAS(24,1)*PMAS(24,2)
        HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
        CALL PYWIDT(24,SQM4,WDTP,WDTE)
        AEMC=ULALEM(SQM4)
        IF(MSTP(8).GE.1) AEMC=AEM
        GMMWC=SQM4*WDTP(0)*AEMC/(24.*XW)
        HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
        FACWG=FACWG*HBW4C/HBW4
        DO 360 I=MMIN1,MMAX1
        IA=IABS(I)
        IF(I.EQ.0.OR.IA.GT.10.OR.KFAC(1,I).EQ.0) GOTO 360
        DO 350 J=MMIN2,MMAX2
        JA=IABS(J)
        IF(J.EQ.0.OR.JA.GT.10.OR.KFAC(2,J).EQ.0) GOTO 350
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 350
        KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
        FCKM=VCKM((IA+1)/2,(JA+1)/2)
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWG*FCKM*WIDSC
  350   CONTINUE
  360   CONTINUE
 
      ELSEIF(ISUB.EQ.17) THEN
C...f + f~ -> g + H0 (q + q~ -> g + H0 only).
 
      ELSEIF(ISUB.EQ.18) THEN
C...f + f~ -> gamma + gamma.
        FACGG=COMFAC*AEM**2*2.*(TH2+UH2)/(TH*UH)
        DO 370 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 370
        EI=KCHG(IABS(I),1)/3.
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=0.5*FACGG*FCOI*EI**4
  370   CONTINUE
 
      ELSEIF(ISUB.EQ.19) THEN
C...f + f~ -> gamma + (gamma*/Z0).
        FACGZ=COMFAC*2.*AEM**2*(TH2+UH2+2.*SQM4*SH)/(TH*UH)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs.
        HFGG=0.
        HFGZ=0.
        HFZZ=0.
        RADC4=1.+ULALPS(SQM4)/PARU(1)
        DO 380 I=1,MIN(16,MDCY(23,3))
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 380
        IMDM=0
        IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &  IMDM=1
        IF(I.LE.8) THEN
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ELSEIF(I.LE.16) THEN
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ENDIF
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
        IF(4.*RM1.LT.1.) THEN
          FCOF=1.
          IF(I.LE.8) FCOF=3.*RADC4
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(IMDM.EQ.1) THEN
            HFGG=HFGG+FCOF*EF**2*(1.+2.*RM1)*BE34
            HFGZ=HFGZ+FCOF*EF*VF*(1.+2.*RM1)*BE34
            HFZZ=HFZZ+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
          ENDIF
        ENDIF
  380   CONTINUE
C...Propagators: as simulated in PYOFSH and as desired.
        GMMZ=PMAS(23,1)*PMAS(23,2)
        HBW4=(1./PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
        MINT(15)=1
        MINT(61)=1
        CALL PYWIDT(23,SQM4,WDTP,WDTE)
        HFAEM=(PARU(108)/PARU(2))*(2./3.)
        HFGG=HFGG*HFAEM*VINT(111)/SQM4
        HFGZ=HFGZ*HFAEM*VINT(112)/SQM4
        HFZZ=HFZZ*HFAEM*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure.
        DO 390 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 390
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGZ*FCOI*EI**2*(EI**2*HFGG+EI*VI*HFGZ+
     &  (VI**2+AI**2)*HFZZ)/HBW4
  390   CONTINUE
 
      ELSEIF(ISUB.EQ.20) THEN
C...f + f~' -> gamma + W+/-.
        FACGW=COMFAC*0.5*AEM**2/XW
C...Propagators: as simulated in PYOFSH and as desired.
        GMMW=PMAS(24,1)*PMAS(24,2)
        HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
        CALL PYWIDT(24,SQM4,WDTP,WDTE)
        AEMC=ULALEM(SQM4)
        IF(MSTP(8).GE.1) AEMC=AEM
        GMMWC=SQM4*WDTP(0)*AEMC/(24.*XW)
        HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
        FACGW=FACGW*HBW4C/HBW4
C...Anomalous couplings.
        TERM1=(TH2+UH2+2.*SQM4*SH)/(TH*UH)
        TERM2=0.
        TERM3=0.
        IF(MSTP(5).GE.1) THEN
          TERM2=PARU(153)*(TH-UH)/(TH+UH)
          TERM3=0.5*PARU(153)**2*(TH*UH+(TH2+UH2)*SH/
     &    (4.*PMAS(24,1)**2))/(TH+UH)**2
        ENDIF
        DO 410 I=MMIN1,MMAX1
        IA=IABS(I)
        IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 410
        DO 400 J=MMIN2,MMAX2
        JA=IABS(J)
        IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(2,J).EQ.0) GOTO 400
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 400
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 400
        KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
        IF(IA.LE.10) THEN
          FACWR=UH/(TH+UH)-1./3.
          FCKM=VCKM((IA+1)/2,(JA+1)/2)
          FCOI=FACA/3.
        ELSE
          FACWR=-TH/(TH+UH)
          FCKM=1.
          FCOI=1.
        ENDIF
        FACWK=TERM1*FACWR**2+TERM2*FACWR+TERM3
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGW*FACWK*FCOI*FCKM*WIDSC
  400   CONTINUE
  410   CONTINUE
      ENDIF
 
      ELSEIF(ISUB.LE.30) THEN
      IF(ISUB.EQ.21) THEN
C...f + f~ -> gamma + H0.
 
      ELSEIF(ISUB.EQ.22) THEN
C...f + f~ -> (gamma*/Z0) + (gamma*/Z0).
C...Kinematics dependence.
        FACZZ=COMFAC*AEM**2*((TH2+UH2+2.*(SQM3+SQM4)*SH)/(TH*UH)-
     &  SQM3*SQM4*(1./TH2+1./UH2))
C...gamma, gamma/Z interference and Z couplings to final fermion pairs.
        DO 430 I=1,6
        DO 420 J=1,3
        HGZ(I,J)=0.
  420   CONTINUE
  430   CONTINUE
        RADC3=1.+ULALPS(SQM3)/PARU(1)
        RADC4=1.+ULALPS(SQM4)/PARU(1)
        DO 440 I=1,MIN(16,MDCY(23,3))
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 440
        IMDM=0
        IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2) IMDM=1
        IF(MDME(IDC,1).EQ.4.OR.MDME(IDC,1).EQ.5) IMDM=MDME(IDC,1)-2
        IF(I.LE.8) THEN
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ELSEIF(I.LE.16) THEN
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ENDIF
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM3
        IF(4.*RM1.LT.1.) THEN
          FCOF=1.
          IF(I.LE.8) FCOF=3.*RADC3
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(IMDM.GE.1) THEN
            HGZ(1,IMDM)=HGZ(1,IMDM)+FCOF*EF**2*(1.+2.*RM1)*BE34
            HGZ(2,IMDM)=HGZ(2,IMDM)+FCOF*EF*VF*(1.+2.*RM1)*BE34
            HGZ(3,IMDM)=HGZ(3,IMDM)+FCOF*(VF**2*(1.+2.*RM1)+
     &      AF**2*(1.-4.*RM1))*BE34
          ENDIF
        ENDIF
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
        IF(4.*RM1.LT.1.) THEN
          FCOF=1.
          IF(I.LE.8) FCOF=3.*RADC4
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(IMDM.GE.1) THEN
            HGZ(4,IMDM)=HGZ(4,IMDM)+FCOF*EF**2*(1.+2.*RM1)*BE34
            HGZ(5,IMDM)=HGZ(5,IMDM)+FCOF*EF*VF*(1.+2.*RM1)*BE34
            HGZ(6,IMDM)=HGZ(6,IMDM)+FCOF*(VF**2*(1.+2.*RM1)+
     &      AF**2*(1.-4.*RM1))*BE34
          ENDIF
        ENDIF
  440   CONTINUE
C...Propagators: as simulated in PYOFSH and as desired.
        GMMZ=PMAS(23,1)*PMAS(23,2)
        HBW3=(1./PARU(1))*GMMZ/((SQM3-SQMZ)**2+GMMZ**2)
        HBW4=(1./PARU(1))*GMMZ/((SQM4-SQMZ)**2+GMMZ**2)
        MINT(15)=1
        MINT(61)=1
        CALL PYWIDT(23,SQM3,WDTP,WDTE)
        HFAEM=(PARU(108)/PARU(2))*(2./3.)
        DO 450 J=1,3
        HGZ(1,J)=HGZ(1,J)*HFAEM*VINT(111)/SQM3
        HGZ(2,J)=HGZ(2,J)*HFAEM*VINT(112)/SQM3
        HGZ(3,J)=HGZ(3,J)*HFAEM*VINT(114)/SQM3
  450   CONTINUE
        MINT(61)=1
        CALL PYWIDT(23,SQM4,WDTP,WDTE)
        HFAEM=(PARU(108)/PARU(2))*(2./3.)
        DO 460 J=1,3
        HGZ(4,J)=HGZ(4,J)*HFAEM*VINT(111)/SQM4
        HGZ(5,J)=HGZ(5,J)*HFAEM*VINT(112)/SQM4
        HGZ(6,J)=HGZ(6,J)*HFAEM*VINT(114)/SQM4
  460   CONTINUE
C...Loop over flavours; separate left- and right-handed couplings.
        DO 480 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 480
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        VALI=VI-AI
        VARI=VI+AI
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        DO 470 J=1,3
        HL3(J)=EI**2*HGZ(1,J)+EI*VALI*HGZ(2,J)+VALI**2*HGZ(3,J)
        HR3(J)=EI**2*HGZ(1,J)+EI*VARI*HGZ(2,J)+VARI**2*HGZ(3,J)
        HL4(J)=EI**2*HGZ(4,J)+EI*VALI*HGZ(5,J)+VALI**2*HGZ(6,J)
        HR4(J)=EI**2*HGZ(4,J)+EI*VARI*HGZ(5,J)+VARI**2*HGZ(6,J)
  470   CONTINUE
        FACLR=HL3(1)*HL4(1)+HL3(1)*(HL4(2)+HL4(3))+
     &  HL4(1)*(HL3(2)+HL3(3))+HL3(2)*HL4(3)+HL4(2)*HL3(3)+
     &  HR3(1)*HR4(1)+HR3(1)*(HR4(2)+HR4(3))+
     &  HR4(1)*(HR3(2)+HR3(3))+HR3(2)*HR4(3)+HR4(2)*HR3(3)
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=0.5*FACZZ*FCOI*FACLR/(HBW3*HBW4)
  480   CONTINUE
 
      ELSEIF(ISUB.EQ.23) THEN
C...f + f~' -> Z0 + W+/-.
        FACZW=COMFAC*0.5*(AEM/XW)**2
        FACZW=FACZW*WIDS(23,2)
        THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
        FACBW=1./((SH-SQMW)**2+SQMW*PMAS(24,2)**2)
        DO 500 I=MMIN1,MMAX1
        IA=IABS(I)
        IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 500
        DO 490 J=MMIN2,MMAX2
        JA=IABS(J)
        IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(2,J).EQ.0) GOTO 490
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 490
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 490
        KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        EI=KCHG(IA,1)/3.
        AI=SIGN(1.,EI+0.1)
        VI=AI-4.*EI*XWV
        EJ=KCHG(JA,1)/3.
        AJ=SIGN(1.,EJ+0.1)
        VJ=AJ-4.*EJ*XWV
        IF(VI+AI.GT.0) THEN
          VISAV=VI
          AISAV=AI
          VI=VJ
          AI=AJ
          VJ=VISAV
          AJ=AISAV
        ENDIF
        FCKM=1.
        IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
        FCOI=1.
        IF(IA.LE.10) FCOI=FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZW*FCOI*FCKM*(FACBW*((9.-8.*XW)/4.*THUH+
     &  (8.*XW-6.)/4.*SH*(SQM3+SQM4))+(THUH-SH*(SQM3+SQM4))*
     &  (SH-SQMW)*FACBW*0.5*((VJ+AJ)/TH-(VI+AI)/UH)+
     &  THUH/(16.*XW1)*((VJ+AJ)**2/TH2+(VI+AI)**2/UH2)+
     &  SH*(SQM3+SQM4)/(8.*XW1)*(VI+AI)*(VJ+AJ)/(TH*UH))*
     &  WIDS(24,(5-KCHW)/2)
  490   CONTINUE
  500   CONTINUE
 
      ELSEIF(ISUB.EQ.24) THEN
C...f + f~ -> Z0 + H0 (or H'0, or A0).
        THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
        FACHZ=COMFAC*8.*(AEM*XWC)**2*
     &  (THUH+2.*SH*SQM3)/((SH-SQMZ)**2+SQMZ*PMAS(23,2)**2)
        FACHZ=FACHZ*WIDS(23,2)*WIDS(KFHIGG,2)
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACHZ=FACHZ*
     &  PARU(154+10*IHIGG)**2
        DO 510 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 510
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACHZ*FCOI*(VI**2+AI**2)
  510   CONTINUE
 
      ELSEIF(ISUB.EQ.25) THEN
C...f + f~ -> W+ + W-.
C...Propagators: Z0, W+- as simulated in PYOFSH and as desired.
        CALL PYWIDT(23,SH,WDTP,WDTE)
        GMMZC=AEM/(48.*XW*XW1)*SH*WDTP(0)
        HBWZC=SH**2/((SH-SQMZ)**2+GMMZC**2)
        GMMW=PMAS(24,1)*PMAS(24,2)
        HBW3=GMMW/((SQM3-SQMW)**2+GMMW**2)
        AEM3=ULALEM(SQM3)
        IF(MSTP(8).GE.1) AEM3=AEM
        CALL PYWIDT(24,SQM3,WDTP,WDTE)
        GMMW3=AEM3/(24.*XW)*SQM3*WDTP(0)
        HBW3C=GMMW3/((SQM3-SQMW)**2+GMMW3**2)
        HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
        AEM4=ULALEM(SQM4)
        IF(MSTP(8).GE.1) AEM4=AEM
        CALL PYWIDT(24,SQM4,WDTP,WDTE)
        GMMW4=AEM4/(24.*XW)*SQM4*WDTP(0)
        HBW4C=GMMW4/((SQM4-SQMW)**2+GMMW4**2)
C...Kinematical functions.
        THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
        THUH34=(2.*SH*(SQM3+SQM4)+THUH)/(SQM3*SQM4)
        GS=(((SH-SQM3-SQM4)**2-4.*SQM3*SQM4)*THUH34+12.*THUH)/SH2
        GT=THUH34+4.*THUH/TH2
        GST=((SH-SQM3-SQM4)*THUH34+4.*(SH*(SQM3+SQM4)-THUH)/TH)/SH
        GU=THUH34+4.*THUH/UH2
        GSU=((SH-SQM3-SQM4)*THUH34+4.*(SH*(SQM3+SQM4)-THUH)/UH)/SH
C...Common factors and couplings.
        FACWW=COMFAC*(HBW3C/HBW3)*(HBW4C/HBW4)
        FACWW=FACWW*WIDS(24,1)
        CGG=AEM**2/2.
        CGZ=AEM**2/(4.*XW)*HBWZC*(1.-SQMZ/SH)
        CZZ=AEM**2/(32.*XW**2)*HBWZC
        CNG=AEM**2/(4.*XW)
        CNZ=AEM**2/(16.*XW**2)*HBWZC*(1.-SQMZ/SH)
        CNN=AEM**2/(16.*XW**2)
C...Coulomb factor for W+W- pair.
        IF(MSTP(40).GE.1.AND.MSTP(40).LE.3) THEN
          COULE=(SH-4.*SQMW)/(4.*PMAS(24,1))
          COULP=MAX(1E-10,0.5*BE34*SQRT(SH))
          IF(COULE.LT.100.*PMAS(24,2)) THEN
            COULP1=SQRT(0.5*PMAS(24,1)*(SQRT(COULE**2+PMAS(24,2)**2)-
     &      COULE))
          ELSE
            COULP1=SQRT(0.5*PMAS(24,1)*(0.5*PMAS(24,2)**2/COULE))
          ENDIF
          IF(COULE.GT.-100.*PMAS(24,2)) THEN
            COULP2=SQRT(0.5*PMAS(24,1)*(SQRT(COULE**2+PMAS(24,2)**2)+
     &      COULE))
          ELSE
            COULP2=SQRT(0.5*PMAS(24,1)*(0.5*PMAS(24,2)**2/ABS(COULE)))
          ENDIF
          IF(MSTP(40).EQ.1) THEN
            COULDC=PARU(1)-2.*ATAN((COULP1**2+COULP2**2-COULP**2)/
     &      MAX(1E-10,2.*COULP*COULP1)) 
            FACCOU=1.+0.5*PARU(101)*COULDC/MAX(1E-5,BE34)
          ELSEIF(MSTP(40).EQ.2) THEN
            COULCK=CMPLX(COULP1,COULP2)
            COULCP=CMPLX(0.,COULP)
            COULCD=(COULCK+COULCP)/(COULCK-COULCP)
            COULCR=1.+(PARU(101)*SQRT(SH))/(4.*COULCP)*LOG(COULCD)
            COULCS=CMPLX(0.,0.)
            NSTP=100
            DO 515 ISTP=1,NSTP
            COULXX=(ISTP-0.5)/NSTP
            COULCS=COULCS+(1./COULXX)*LOG((1.+COULXX*COULCD)/
     &      (1.+COULXX/COULCD))
  515       CONTINUE
            COULCR=COULCR+(PARU(101)**2*SH)/(16.*COULCP*COULCK)*
     &      (COULCS/NSTP)
            FACCOU=ABS(COULCR)**2
          ELSEIF(MSTP(40).EQ.3) THEN
            COULDC=PARU(1)-2.*(1.-BE34)**2*ATAN((COULP1**2+COULP2**2-
     &      COULP**2)/MAX(1E-10,2.*COULP*COULP1)) 
            FACCOU=1.+0.5*PARU(101)*COULDC/MAX(1E-5,BE34)
          ENDIF
        ELSEIF(MSTP(40).EQ.4) THEN
          FACCOU=1.+0.5*PARU(101)*PARU(1)/MAX(1E-5,BE34)
        ELSE
          FACCOU=1.
        ENDIF
        VINT(95)=FACCOU
        FACWW=FACWW*FACCOU
C...Loop over allowed flavours.
        DO 520 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 520
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI+0.1)
        VI=AI-4.*EI*XWV
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        IF(AI.LT.0.) THEN
          DSIGWW=(CGG*EI**2+CGZ*VI*EI+CZZ*(VI**2+AI**2))*GS+
     &    (CNG*EI+CNZ*(VI+AI))*GST+CNN*GT
        ELSE
          DSIGWW=(CGG*EI**2+CGZ*VI*EI+CZZ*(VI**2+AI**2))*GS-
     &    (CNG*EI+CNZ*(VI+AI))*GSU+CNN*GU
        ENDIF
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWW*FCOI*DSIGWW
  520   CONTINUE
 
      ELSEIF(ISUB.EQ.26) THEN
C...f + f~' -> W+/- + H0 (or H'0, or A0).
        THUH=MAX(TH*UH-SQM3*SQM4,SH*CKIN(3)**2)
        FACHW=COMFAC*0.125*(AEM/XW)**2*(THUH+2.*SH*SQM3)/
     &  ((SH-SQMW)**2+SQMW*PMAS(24,2)**2)
        FACHW=FACHW*WIDS(KFHIGG,2)
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACHW=FACHW*
     &  PARU(155+10*IHIGG)**2
        DO 540 I=MMIN1,MMAX1
        IA=IABS(I)
        IF(I.EQ.0.OR.IA.GT.20.OR.KFAC(1,I).EQ.0) GOTO 540
        DO 530 J=MMIN2,MMAX2
        JA=IABS(J)
        IF(J.EQ.0.OR.JA.GT.20.OR.KFAC(1,J).EQ.0) GOTO 530
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 530
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 530
        KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        FCKM=1.
        IF(IA.LE.10) FCKM=VCKM((IA+1)/2,(JA+1)/2)
        FCOI=1.
        IF(IA.LE.10) FCOI=FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACHW*FCOI*FCKM*WIDS(24,(5-KCHW)/2)
  530   CONTINUE
  540   CONTINUE
 
      ELSEIF(ISUB.EQ.27) THEN
C...f + f~ -> H0 + H0.
 
      ELSEIF(ISUB.EQ.28) THEN
C...f + g -> f + g (q + g -> q + g only).
        FACQG1=COMFAC*AS**2*4./9.*((2.+MSTP(34)*1./4.)*UH2/TH2-UH/SH)*
     &  FACA
        FACQG2=COMFAC*AS**2*4./9.*((2.+MSTP(34)*1./4.)*SH2/TH2-SH/UH)
        DO 560 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.10) GOTO 560
        DO 550 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 550
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 550
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQG1
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=2
        SIGH(NCHN)=FACQG2
  550   CONTINUE
  560   CONTINUE
 
      ELSEIF(ISUB.EQ.29) THEN
C...f + g -> f + gamma (q + g -> q + gamma only).
        FGQ=COMFAC*FACA*AS*AEM*1./3.*(SH2+UH2)/(-SH*UH)
        DO 580 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 580
        EI=KCHG(IABS(I),1)/3.
        FACGQ=FGQ*EI**2
        DO 570 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 570
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 570
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGQ
  570   CONTINUE
  580   CONTINUE
 
      ELSEIF(ISUB.EQ.30) THEN
C...f + g -> f + (gamma*/Z0) (q + g -> q + (gamma*/Z0) only).
        FZQ=COMFAC*FACA*AS*AEM*(1./3.)*(SH2+UH2+2.*SQM4*TH)/(-SH*UH)
C...gamma, gamma/Z interference and Z couplings to final fermion pairs.
        HFGG=0.
        HFGZ=0.
        HFZZ=0.
        HBW4=0.
        RADC4=1.+ULALPS(SQM4)/PARU(1)
        DO 590 I=1,MIN(16,MDCY(23,3))
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 590
        IMDM=0
        IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &  IMDM=1
        IF(I.LE.8) THEN
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ELSEIF(I.LE.16) THEN
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ENDIF
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
        IF(4.*RM1.LT.1.) THEN
          FCOF=1.
          IF(I.LE.8) FCOF=3.*RADC4
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(IMDM.EQ.1) THEN
            HFGG=HFGG+FCOF*EF**2*(1.+2.*RM1)*BE34
            HFGZ=HFGZ+FCOF*EF*VF*(1.+2.*RM1)*BE34
            HFZZ=HFZZ+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
          ENDIF
          HBW4=HBW4+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ENDIF
  590   CONTINUE
C...Propagators: as simulated in PYOFSH and as desired.
        GMMZ=PMAS(23,1)*PMAS(23,2)
        HBW4=HBW4*XWC*SQMZ/((SQM4-SQMZ)**2+GMMZ**2)
        MINT(15)=1
        MINT(61)=1
        CALL PYWIDT(23,SQM4,WDTP,WDTE)
        HFGG=HFGG*VINT(111)/SQM4
        HFGZ=HFGZ*VINT(112)/SQM4
        HFZZ=HFZZ*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure.
        DO 610 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 610
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        FACZQ=FZQ*(EI**2*HFGG+EI*VI*HFGZ+
     &  (VI**2+AI**2)*HFZZ)/HBW4
        DO 600 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 600
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 600
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZQ
  600   CONTINUE
  610   CONTINUE
      ENDIF
 
      ELSEIF(ISUB.LE.40) THEN
      IF(ISUB.EQ.31) THEN
C...f + g -> f' + W+/- (q + g -> q' + W+/- only).
        FACWQ=COMFAC*FACA*AS*AEM/XW*1./12.*
     &  (SH2+UH2+2.*SQM4*TH)/(-SH*UH)
C...Propagators: as simulated in PYOFSH and as desired.
        GMMW=PMAS(24,1)*PMAS(24,2)
        HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
        CALL PYWIDT(24,SQM4,WDTP,WDTE)
        AEMC=ULALEM(SQM4)
        IF(MSTP(8).GE.1) AEMC=AEM
        GMMWC=SQM4*WDTP(0)*AEMC/(24.*XW)
        HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
        FACWQ=FACWQ*HBW4C/HBW4
        DO 630 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 630
        IA=IABS(I)
        KCHW=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
        WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
        DO 620 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 620
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 620
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWQ*VINT(180+I)*WIDSC
  620   CONTINUE
  630   CONTINUE
 
      ELSEIF(ISUB.EQ.32) THEN
C...f + g -> f + H0 (q + g -> q + H0 only).
 
      ELSEIF(ISUB.EQ.33) THEN
C...f + gamma -> f + g (q + gamma -> q + g only).
        FGQ=COMFAC*AS*AEM*8./3.*(SH2+UH2)/(-SH*UH)
        DO 650 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 650
        EI=KCHG(IABS(I),1)/3.
        FACGQ=FGQ*EI**2
        DO 640 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 640
        IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 640
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGQ
  640   CONTINUE
  650   CONTINUE
 
      ELSEIF(ISUB.EQ.34) THEN
C...f + gamma -> f + gamma.
        FGQ=COMFAC*AEM**2*2.*(SH2+UH2)/(-SH*UH)
        DO 670 I=MMINA,MMAXA
        IF(I.EQ.0) GOTO 670
        EI=KCHG(IABS(I),1)/3.
        FACGQ=FGQ*EI**4
        DO 660 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 660
        IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 660
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGQ
  660   CONTINUE
  670   CONTINUE
 
      ELSEIF(ISUB.EQ.35) THEN
C...f + gamma -> f + (gamma*/Z0).
        FZQN=COMFAC*2.*AEM**2*(SH2+UH2+2.*SQM4*TH)
        FZQD=SQPTH*SQM4-SH*UH
C...gamma, gamma/Z interference and Z couplings to final fermion pairs.
        HFGG=0.
        HFGZ=0.
        HFZZ=0.
        HBW4=0.
        RADC4=1.+ULALPS(SQM4)/PARU(1)
        DO 680 I=1,MIN(16,MDCY(23,3))
        IDC=I+MDCY(23,2)-1
        IF(MDME(IDC,1).LT.0) GOTO 680
        IMDM=0
        IF(MDME(IDC,1).EQ.1.OR.MDME(IDC,1).EQ.2.OR.MDME(IDC,1).EQ.4)
     &  IMDM=1
        IF(I.LE.8) THEN
          EF=KCHG(I,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ELSEIF(I.LE.16) THEN
          EF=KCHG(I+2,1)/3.
          AF=SIGN(1.,EF+0.1)
          VF=AF-4.*EF*XWV
        ENDIF
        RM1=PMAS(IABS(KFDP(IDC,1)),1)**2/SQM4
        IF(4.*RM1.LT.1.) THEN
          FCOF=1.
          IF(I.LE.8) FCOF=3.*RADC4
          BE34=SQRT(MAX(0.,1.-4.*RM1))
          IF(IMDM.EQ.1) THEN
            HFGG=HFGG+FCOF*EF**2*(1.+2.*RM1)*BE34
            HFGZ=HFGZ+FCOF*EF*VF*(1.+2.*RM1)*BE34
            HFZZ=HFZZ+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
          ENDIF
          HBW4=HBW4+FCOF*(VF**2*(1.+2.*RM1)+AF**2*(1.-4.*RM1))*BE34
        ENDIF
  680   CONTINUE
C...Propagators: as simulated in PYOFSH and as desired.
        GMMZ=PMAS(23,1)*PMAS(23,2)
        HBW4=HBW4*XWC*SQMZ/((SQM4-SQMZ)**2+GMMZ**2)
        MINT(15)=1
        MINT(61)=1
        CALL PYWIDT(23,SQM4,WDTP,WDTE)
        HFGG=HFGG*VINT(111)/SQM4
        HFGZ=HFGZ*VINT(112)/SQM4
        HFZZ=HFZZ*VINT(114)/SQM4
C...Loop over flavours; consider full gamma/Z structure.
        DO 700 I=MMINA,MMAXA
        IF(I.EQ.0) GOTO 700
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        FACZQ=EI**2*(EI**2*HFGG+EI*VI*HFGZ+
     &  (VI**2+AI**2)*HFZZ)/HBW4
        DO 690 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 690
        IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 690
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZQ*FZQN/MAX(PMAS(IABS(I),1)**2*SQM4,FZQD)
  690   CONTINUE
  700   CONTINUE
 
      ELSEIF(ISUB.EQ.36) THEN
C...f + gamma -> f' + W+/-.
        FWQ=COMFAC*AEM**2/(2.*XW)*
     &  (SH2+UH2+2.*SQM4*TH)/(SQPTH*SQM4-SH*UH)
C...Propagators: as simulated in PYOFSH and as desired.
        GMMW=PMAS(24,1)*PMAS(24,2)
        HBW4=GMMW/((SQM4-SQMW)**2+GMMW**2)
        CALL PYWIDT(24,SQM4,WDTP,WDTE)
        AEMC=ULALEM(SQM4)
        IF(MSTP(8).GE.1) AEMC=AEM
        GMMWC=SQM4*WDTP(0)*AEMC/(24.*XW)
        HBW4C=GMMWC/((SQM4-SQMW)**2+GMMWC**2)
        FWQ=FWQ*HBW4C/HBW4
        DO 720 I=MMINA,MMAXA
        IF(I.EQ.0) GOTO 720
        IA=IABS(I)
        EIA=ABS(KCHG(IABS(I),1)/3.)
        FACWQ=FWQ*(EIA-SH/(SH+UH))**2
        KCHW=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
        WIDSC=(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))/WDTP(0)
        DO 710 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 710
        IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 710
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWQ*VINT(180+I)*WIDSC
  710   CONTINUE
  720   CONTINUE
 
      ELSEIF(ISUB.EQ.37) THEN
C...f + gamma -> f + H0.
 
      ELSEIF(ISUB.EQ.38) THEN
C...f + Z0 -> f + g (q + Z0 -> q + g only).
 
      ELSEIF(ISUB.EQ.39) THEN
C...f + Z0 -> f + gamma.
 
      ELSEIF(ISUB.EQ.40) THEN
C...f + Z0 -> f + Z0.
      ENDIF
 
      ELSEIF(ISUB.LE.50) THEN
      IF(ISUB.EQ.41) THEN
C...f + Z0 -> f' + W+/-.
 
      ELSEIF(ISUB.EQ.42) THEN
C...f + Z0 -> f + H0.
 
      ELSEIF(ISUB.EQ.43) THEN
C...f + W+/- -> f' + g (q + W+/- -> q' + g only).
 
      ELSEIF(ISUB.EQ.44) THEN
C...f + W+/- -> f' + gamma.
 
      ELSEIF(ISUB.EQ.45) THEN
C...f + W+/- -> f' + Z0.
 
      ELSEIF(ISUB.EQ.46) THEN
C...f + W+/- -> f' + W+/-.
 
      ELSEIF(ISUB.EQ.47) THEN
C...f + W+/- -> f' + H0.
 
      ELSEIF(ISUB.EQ.48) THEN
C...f + H0 -> f + g (q + H0 -> q + g only).
 
      ELSEIF(ISUB.EQ.49) THEN
C...f + H0 -> f + gamma.
 
      ELSEIF(ISUB.EQ.50) THEN
C...f + H0 -> f + Z0.
      ENDIF
 
      ELSEIF(ISUB.LE.60) THEN
      IF(ISUB.EQ.51) THEN
C...f + H0 -> f' + W+/-.
 
      ELSEIF(ISUB.EQ.52) THEN
C...f + H0 -> f + H0.
 
      ELSEIF(ISUB.EQ.53) THEN
C...g + g -> f + f~ (g + g -> q + q~ only).
        CALL PYWIDT(21,SH,WDTP,WDTE)
        FACQQ1=COMFAC*AS**2*1./6.*(UH/TH-(2.+MSTP(34)*1./4.)*UH2/SH2)*
     &  (WDTE(0,1)+WDTE(0,2)+WDTE(0,3)+WDTE(0,4))*FACA
        FACQQ2=COMFAC*AS**2*1./6.*(TH/UH-(2.+MSTP(34)*1./4.)*TH2/SH2)*
     &  (WDTE(0,1)+WDTE(0,2)+WDTE(0,3)+WDTE(0,4))*FACA
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 730
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQQ1
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=2
        SIGH(NCHN)=FACQQ2
  730   CONTINUE
 
      ELSEIF(ISUB.EQ.54) THEN
C...g + gamma -> f + f~ (g + gamma -> q + q~ only).
        CALL PYWIDT(21,SH,WDTP,WDTE)
        WDTESU=0.
        DO 740 I=1,MIN(8,MDCY(21,3))
        EF=KCHG(I,1)/3.
        WDTESU=WDTESU+EF**2*(WDTE(I,1)+WDTE(I,2)+WDTE(I,3)+WDTE(I,4))
  740   CONTINUE
        FACQQ=COMFAC*AEM*AS*WDTESU*(TH2+UH2)/(TH*UH)
        IF(KFAC(1,21)*KFAC(2,22).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ
        ENDIF
        IF(KFAC(1,22)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ
        ENDIF
 
      ELSEIF(ISUB.EQ.55) THEN
C...g + Z -> f + f~ (g + Z -> q + q~ only).
 
      ELSEIF(ISUB.EQ.56) THEN
C...g + W -> f + f'~ (g + W -> q + q'~ only).
 
      ELSEIF(ISUB.EQ.57) THEN
C...g + H0 -> f + f~ (g + H0 -> q + q~ only).
 
      ELSEIF(ISUB.EQ.58) THEN
C...gamma + gamma -> f + f~.
        CALL PYWIDT(22,SH,WDTP,WDTE)
        WDTESU=0.
        DO 750 I=1,MIN(12,MDCY(22,3))
        IF(I.LE.8) EF= KCHG(I,1)/3.
        IF(I.GE.9) EF= KCHG(9+2*(I-8),1)/3.
        WDTESU=WDTESU+EF**2*(WDTE(I,1)+WDTE(I,2)+WDTE(I,3)+WDTE(I,4))
  750   CONTINUE
        FACFF=COMFAC*AEM**2*WDTESU*2.*(TH2+UH2)/(TH*UH)
        IF(KFAC(1,22)*KFAC(2,22).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACFF
        ENDIF
 
      ELSEIF(ISUB.EQ.59) THEN
C...gamma + Z0 -> f + f~.
 
      ELSEIF(ISUB.EQ.60) THEN
C...gamma + W+/- -> f + f~'.
      ENDIF
 
      ELSEIF(ISUB.LE.70) THEN
      IF(ISUB.EQ.61) THEN
C...gamma + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.62) THEN
C...Z0 + Z0 -> f + f~.
 
      ELSEIF(ISUB.EQ.63) THEN
C...Z0 + W+/- -> f + f~'.
 
      ELSEIF(ISUB.EQ.64) THEN
C...Z0 + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.65) THEN
C...W+ + W- -> f + f~.
 
      ELSEIF(ISUB.EQ.66) THEN
C...W+/- + H0 -> f + f~'.
 
      ELSEIF(ISUB.EQ.67) THEN
C...H0 + H0 -> f + f~.
 
      ELSEIF(ISUB.EQ.68) THEN
C...g + g -> g + g.
        FACGG1=COMFAC*AS**2*9./4.*(SH2/TH2+2.*SH/TH+3.+2.*TH/SH+
     &  TH2/SH2)*FACA
        FACGG2=COMFAC*AS**2*9./4.*(UH2/SH2+2.*UH/SH+3.+2.*SH/UH+
     &  SH2/UH2)*FACA
        FACGG3=COMFAC*AS**2*9./4.*(TH2/UH2+2.*TH/UH+3.+2.*UH/TH+
     &  UH2/TH2)
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 760
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=0.5*FACGG1
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=2
        SIGH(NCHN)=0.5*FACGG2
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=3
        SIGH(NCHN)=0.5*FACGG3
  760   CONTINUE
 
      ELSEIF(ISUB.EQ.69) THEN
C...gamma + gamma -> W+ + W-.
        SQMWE=MAX(0.5*SQMW,SQRT(SQM3*SQM4))
        FPROP=SH2/((SQMWE-TH)*(SQMWE-UH))
        FACWW=COMFAC*6.*AEM**2*(1.-FPROP*(4./3.+2.*SQMWE/SH)+
     &  FPROP**2*(2./3.+2.*(SQMWE/SH)**2))*WIDS(24,1)
        IF(KFAC(1,22)*KFAC(2,22).EQ.0) GOTO 770
        NCHN=NCHN+1
        ISIG(NCHN,1)=22
        ISIG(NCHN,2)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWW
  770   CONTINUE
 
      ELSEIF(ISUB.EQ.70) THEN
C...gamma + W+/- -> Z0 + W+/-.
        SQMWE=MAX(0.5*SQMW,SQRT(SQM3*SQM4))
        FPROP=(TH-SQMWE)**2/(-SH*(SQMWE-UH))
        FACZW=COMFAC*6.*AEM**2*(XW1/XW)*
     &  (1.-FPROP*(4./3.+2.*SQMWE/(TH-SQMWE))+
     &  FPROP**2*(2./3.+2.*(SQMWE/(TH-SQMWE))**2))*WIDS(23,2)
        DO 790 KCHW=1,-1,-2
        DO 780 ISDE=1,2
        IF(KFAC(ISDE,22)*KFAC(3-ISDE,24*KCHW).EQ.0) GOTO 780
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=22
        ISIG(NCHN,3-ISDE)=24*KCHW
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZW*WIDS(24,(5-KCHW)/2)
  780   CONTINUE
  790   CONTINUE
      ENDIF
 
      ELSEIF(ISUB.LE.80) THEN
      IF(ISUB.EQ.71) THEN
C...Z0 + Z0 -> Z0 + Z0.
        IF(SH.LE.4.01*SQMZ) GOTO 820
 
        IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons.
          BE2=1.-4.*SQMZ/SH
          TH=-0.5*SH*BE2*(1.-CTH)
          UH=-0.5*SH*BE2*(1.+CTH)
          IF(MAX(TH,UH).GT.-1.) GOTO 820
          SHANG=1./XW1*SQMW/SQMZ*(1.+BE2)**2
          ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
          ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
          THANG=1./XW1*SQMW/SQMZ*(BE2-CTH)**2
          ATHRE=(TH-SQMH)/((TH-SQMH)**2+GMMH**2)*THANG
          ATHIM=-GMMH/((TH-SQMH)**2+GMMH**2)*THANG
          UHANG=1./XW1*SQMW/SQMZ*(BE2+CTH)**2
          AUHRE=(UH-SQMH)/((UH-SQMH)**2+GMMH**2)*UHANG
          AUHIM=-GMMH/((UH-SQMH)**2+GMMH**2)*UHANG
          FACZZ=COMFAC*1./(4096.*PARU(1)**2*16.*XW1**2)*
     &    (AEM/XW)**4*(SH/SQMW)**2*(SQMZ/SQMW)*SH2
          IF(MSTP(46).LE.0) FACZZ=FACZZ*(ASHRE**2+ASHIM**2)
          IF(MSTP(46).EQ.1) FACZZ=FACZZ*((ASHRE+ATHRE+AUHRE)**2+
     &    (ASHIM+ATHIM+AUHIM)**2)
          IF(MSTP(46).EQ.2) FACZZ=0.
 
        ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron.
          FACZZ=COMFAC*(AEM/(16.*PARU(1)*XW*XW1))**2*(64./9.)*
     &    ABS(A00U+2.*A20U)**2
        ENDIF
        FACZZ=FACZZ*WIDS(23,1)
 
        DO 810 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 810
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        AVI=AI**2+VI**2
        DO 800 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 800
        EJ=KCHG(IABS(J),1)/3.
        AJ=SIGN(1.,EJ)
        VJ=AJ-4.*EJ*XWV
        AVJ=AJ**2+VJ**2
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=0.5*FACZZ*AVI*AVJ
  800   CONTINUE
  810   CONTINUE
  820   CONTINUE
 
      ELSEIF(ISUB.EQ.72) THEN
C...Z0 + Z0 -> W+ + W-.
        IF(SH.LE.4.01*SQMZ) GOTO 850
 
        IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons.
          BE2=SQRT((1.-4.*SQMW/SH)*(1.-4.*SQMZ/SH))
          CTH2=CTH**2
          TH=-0.5*SH*(1.-2.*(SQMW+SQMZ)/SH-BE2*CTH)
          UH=-0.5*SH*(1.-2.*(SQMW+SQMZ)/SH+BE2*CTH)
          IF(MAX(TH,UH).GT.-1.) GOTO 850
          SHANG=4.*SQRT(SQMW/(SQMZ*XW1))*(1.-2.*SQMW/SH)*
     &    (1.-2.*SQMZ/SH)
          ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
          ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
          ATWRE=XW1/SQMZ*SH/(TH-SQMW)*((CTH-BE2)**2*(3./2.+BE2/2.*
     &    CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4.*((SQMW+SQMZ)/
     &    SH*(1.-3.*CTH2)+8.*SQMW*SQMZ/SH2*(2.*CTH2-1.)+
     &    4.*(SQMW**2+SQMZ**2)/SH2*CTH2+2.*(SQMW+SQMZ)/SH*BE2*CTH))
          ATWIM=0.
          AUWRE=XW1/SQMZ*SH/(UH-SQMW)*((CTH+BE2)**2*(3./2.-BE2/2.*
     &    CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4.*((SQMW+SQMZ)/
     &    SH*(1.-3.*CTH2)+8.*SQMW*SQMZ/SH2*(2.*CTH2-1.)+
     &    4.*(SQMW**2+SQMZ**2)/SH2*CTH2-2.*(SQMW+SQMZ)/SH*BE2*CTH))
          AUWIM=0.
          A4RE=2.*XW1/SQMZ*(3.-CTH2-4.*(SQMW+SQMZ)/SH)
          A4IM=0.
          FACWW=COMFAC*1./(4096.*PARU(1)**2*16.*XW1**2)*
     &    (AEM/XW)**4*(SH/SQMW)**2*(SQMZ/SQMW)*SH2
          IF(MSTP(46).LE.0) FACWW=FACWW*(ASHRE**2+ASHIM**2)
          IF(MSTP(46).EQ.1) FACWW=FACWW*((ASHRE+ATWRE+AUWRE+A4RE)**2+
     &    (ASHIM+ATWIM+AUWIM+A4IM)**2)
          IF(MSTP(46).EQ.2) FACWW=FACWW*((ATWRE+AUWRE+A4RE)**2+
     &    (ATWIM+AUWIM+A4IM)**2)
 
        ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron.
          FACWW=COMFAC*(AEM/(16.*PARU(1)*XW*XW1))**2*(64./9.)*
     &    ABS(A00U-A20U)**2
        ENDIF
        FACWW=FACWW*WIDS(24,1)
 
        DO 840 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 840
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        AVI=AI**2+VI**2
        DO 830 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 830
        EJ=KCHG(IABS(J),1)/3.
        AJ=SIGN(1.,EJ)
        VJ=AJ-4.*EJ*XWV
        AVJ=AJ**2+VJ**2
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWW*AVI*AVJ
  830   CONTINUE
  840   CONTINUE
  850   CONTINUE
 
      ELSEIF(ISUB.EQ.73) THEN
C...Z0 + W+/- -> Z0 + W+/-.
        IF(SH.LE.2.*SQMZ+2.*SQMW) GOTO 880
 
        IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons.
          BE2=1.-2.*(SQMZ+SQMW)/SH+((SQMZ-SQMW)/SH)**2
          EP1=1.-(SQMZ-SQMW)/SH
          EP2=1.+(SQMZ-SQMW)/SH
          TH=-0.5*SH*BE2*(1.-CTH)
          UH=(SQMZ-SQMW)**2/SH-0.5*SH*BE2*(1.+CTH)
          IF(MAX(TH,UH).GT.-1.) GOTO 880
          THANG=(BE2-EP1*CTH)*(BE2-EP2*CTH)
          ATHRE=(TH-SQMH)/((TH-SQMH)**2+GMMH**2)*THANG
          ATHIM=-GMMH/((TH-SQMH)**2+GMMH**2)*THANG
          ASWRE=-XW1/SQMZ*SH/(SH-SQMW)*(-BE2*(EP1+EP2)**4*CTH+
     &    1./4.*(BE2+EP1*EP2)**2*((EP1-EP2)**2-4.*BE2*CTH)+
     &    2.*BE2*(BE2+EP1*EP2)*(EP1+EP2)**2*CTH-
     &    1./16.*SH/SQMW*(EP1**2-EP2**2)**2*(BE2+EP1*EP2)**2)
          ASWIM=0.
          AUWRE=XW1/SQMZ*SH/(UH-SQMW)*(-BE2*(EP2+EP1*CTH)*
     &    (EP1+EP2*CTH)*(BE2+EP1*EP2)+BE2*(EP2+EP1*CTH)*
     &    (BE2+EP1*EP2*CTH)*(2.*EP2-EP2*CTH+EP1)-BE2*(EP2+EP1*CTH)**2*
     &    (BE2-EP2**2*CTH)-1./8.*(BE2+EP1*EP2*CTH)**2*((EP1+EP2)**2+
     &    2.*BE2*(1.-CTH))+1./32.*SH/SQMW*(BE2+EP1*EP2*CTH)**2*
     &    (EP1**2-EP2**2)**2-BE2*(EP1+EP2*CTH)*(EP2+EP1*CTH)*
     &    (BE2+EP1*EP2)+BE2*(EP1+EP2*CTH)*(BE2+EP1*EP2*CTH)*
     &    (2.*EP1-EP1*CTH+EP2)-BE2*(EP1+EP2*CTH)**2*(BE2-EP1**2*CTH)-
     &    1./8.*(BE2+EP1*EP2*CTH)**2*((EP1+EP2)**2+2.*BE2*(1.-CTH))+
     &    1./32.*SH/SQMW*(BE2+EP1*EP2*CTH)**2*(EP1**2-EP2**2)**2)
          AUWIM=0.
          A4RE=XW1/SQMZ*(EP1**2*EP2**2*(CTH**2-1.)-
     &    2.*BE2*(EP1**2+EP2**2+EP1*EP2)*CTH-2.*BE2*EP1*EP2)
          A4IM=0.
          FACZW=COMFAC*1./(4096.*PARU(1)**2*4.*XW1)*(AEM/XW)**4*
     &    (SH/SQMW)**2*SQRT(SQMZ/SQMW)*SH2
          IF(MSTP(46).LE.0) FACZW=0.
          IF(MSTP(46).EQ.1) FACZW=FACZW*((ATHRE+ASWRE+AUWRE+A4RE)**2+
     &    (ATHIM+ASWIM+AUWIM+A4IM)**2)
          IF(MSTP(46).EQ.2) FACZW=FACZW*((ASWRE+AUWRE+A4RE)**2+
     &    (ASWIM+AUWIM+A4IM)**2)
 
        ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron.
          FACZW=COMFAC*AEM**2/(64.*PARU(1)**2*XW**2*XW1)*16.*
     &    ABS(A20U+3.*A11U*CTH)**2
        ENDIF
        FACZW=FACZW*WIDS(23,2)
 
        DO 870 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 870
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        AVI=AI**2+VI**2
        KCHWI=ISIGN(1,KCHG(IABS(I),1)*ISIGN(1,I))
        DO 860 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 860
        EJ=KCHG(IABS(J),1)/3.
        AJ=SIGN(1.,EJ)
        VJ=AI-4.*EJ*XWV
        AVJ=AJ**2+VJ**2
        KCHWJ=ISIGN(1,KCHG(IABS(J),1)*ISIGN(1,J))
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZW*AVI*VINT(180+J)*WIDS(24,(5-KCHWJ)/2)
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=2
        SIGH(NCHN)=FACZW*VINT(180+I)*WIDS(24,(5-KCHWI)/2)*AVJ
  860   CONTINUE
  870   CONTINUE
  880   CONTINUE
 
      ELSEIF(ISUB.EQ.75) THEN
C...W+ + W- -> gamma + gamma.
 
      ELSEIF(ISUB.EQ.76) THEN
C...W+ + W- -> Z0 + Z0.
        IF(SH.LE.4.01*SQMZ) GOTO 910
 
        IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons.
          BE2=SQRT((1.-4.*SQMW/SH)*(1.-4.*SQMZ/SH))
          CTH2=CTH**2
          TH=-0.5*SH*(1.-2.*(SQMW+SQMZ)/SH-BE2*CTH)
          UH=-0.5*SH*(1.-2.*(SQMW+SQMZ)/SH+BE2*CTH)
          IF(MAX(TH,UH).GT.-1.) GOTO 910
          SHANG=4.*SQRT(SQMW/(SQMZ*XW1))*(1.-2.*SQMW/SH)*
     &    (1.-2.*SQMZ/SH)
          ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
          ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
          ATWRE=XW1/SQMZ*SH/(TH-SQMW)*((CTH-BE2)**2*(3./2.+BE2/2.*
     &    CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4.*((SQMW+SQMZ)/
     &    SH*(1.-3.*CTH2)+8.*SQMW*SQMZ/SH2*(2.*CTH2-1.)+
     &    4.*(SQMW**2+SQMZ**2)/SH2*CTH2+2.*(SQMW+SQMZ)/SH*BE2*CTH))
          ATWIM=0.
          AUWRE=XW1/SQMZ*SH/(UH-SQMW)*((CTH+BE2)**2*(3./2.-BE2/2.*
     &    CTH-(SQMW+SQMZ)/SH+(SQMW-SQMZ)**2/(SH*SQMW))+4.*((SQMW+SQMZ)/
     &    SH*(1.-3.*CTH2)+8.*SQMW*SQMZ/SH2*(2.*CTH2-1.)+
     &    4.*(SQMW**2+SQMZ**2)/SH2*CTH2-2.*(SQMW+SQMZ)/SH*BE2*CTH))
          AUWIM=0.
          A4RE=2.*XW1/SQMZ*(3.-CTH2-4.*(SQMW+SQMZ)/SH)
          A4IM=0.
          FACZZ=COMFAC*1./(4096.*PARU(1)**2)*(AEM/XW)**4*
     &    (SH/SQMW)**2*SH2
          IF(MSTP(46).LE.0) FACZZ=FACZZ*(ASHRE**2+ASHIM**2)
          IF(MSTP(46).EQ.1) FACZZ=FACZZ*((ASHRE+ATWRE+AUWRE+A4RE)**2+
     &    (ASHIM+ATWIM+AUWIM+A4IM)**2)
          IF(MSTP(46).EQ.2) FACZZ=FACZZ*((ATWRE+AUWRE+A4RE)**2+
     &    (ATWIM+AUWIM+A4IM)**2)
 
        ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron.
          FACZZ=COMFAC*(AEM/(4.*PARU(1)*XW))**2*(64./9.)*
     &    ABS(A00U-A20U)**2
        ENDIF
        FACZZ=FACZZ*WIDS(23,1)
 
        DO 900 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 900
        EI=SIGN(1.,FLOAT(I))*KCHG(IABS(I),1)
        DO 890 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 890
        EJ=SIGN(1.,FLOAT(J))*KCHG(IABS(J),1)
        IF(EI*EJ.GT.0.) GOTO 890
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=0.5*FACZZ*VINT(180+I)*VINT(180+J)
  890   CONTINUE
  900   CONTINUE
  910   CONTINUE
 
      ELSEIF(ISUB.EQ.77) THEN
C...W+/- + W+/- -> W+/- + W+/-.
        IF(SH.LE.4.01*SQMW) GOTO 940
 
        IF(MSTP(46).LE.2) THEN
C...Exact scattering ME:s for on-mass-shell gauge bosons.
          BE2=1.-4.*SQMW/SH
          BE4=BE2**2
          CTH2=CTH**2
          CTH3=CTH**3
          TH=-0.5*SH*BE2*(1.-CTH)
          UH=-0.5*SH*BE2*(1.+CTH)
          IF(MAX(TH,UH).GT.-1.) GOTO 940
          SHANG=(1.+BE2)**2
          ASHRE=(SH-SQMH)/((SH-SQMH)**2+GMMH**2)*SHANG
          ASHIM=-GMMH/((SH-SQMH)**2+GMMH**2)*SHANG
          THANG=(BE2-CTH)**2
          ATHRE=(TH-SQMH)/((TH-SQMH)**2+GMMH**2)*THANG
          ATHIM=-GMMH/((TH-SQMH)**2+GMMH**2)*THANG
          UHANG=(BE2+CTH)**2
          AUHRE=(UH-SQMH)/((UH-SQMH)**2+GMMH**2)*UHANG
          AUHIM=-GMMH/((UH-SQMH)**2+GMMH**2)*UHANG
          SGZANG=1./SQMW*BE2*(3.-BE2)**2*CTH
          ASGRE=XW*SGZANG
          ASGIM=0.
          ASZRE=XW1*SH/(SH-SQMZ)*SGZANG
          ASZIM=0.
          TGZANG=1./SQMW*(BE2*(4.-2.*BE2+BE4)+BE2*(4.-10.*BE2+BE4)*CTH+
     &    (2.-11.*BE2+10.*BE4)*CTH2+BE2*CTH3)
          ATGRE=0.5*XW*SH/TH*TGZANG
          ATGIM=0.
          ATZRE=0.5*XW1*SH/(TH-SQMZ)*TGZANG
          ATZIM=0.
          UGZANG=1./SQMW*(BE2*(4.-2.*BE2+BE4)-BE2*(4.-10.*BE2+BE4)*CTH+
     &    (2.-11.*BE2+10.*BE4)*CTH2-BE2*CTH3)
          AUGRE=0.5*XW*SH/UH*UGZANG
          AUGIM=0.
          AUZRE=0.5*XW1*SH/(UH-SQMZ)*UGZANG
          AUZIM=0.
          A4ARE=1./SQMW*(1.+2.*BE2-6.*BE2*CTH-CTH2)
          A4AIM=0.
          A4SRE=2./SQMW*(1.+2.*BE2-CTH2)
          A4SIM=0.
          FWW=COMFAC*1./(4096.*PARU(1)**2)*(AEM/XW)**4*(SH/SQMW)**2*SH2
          IF(MSTP(46).LE.0) THEN
            AWWARE=ASHRE
            AWWAIM=ASHIM
            AWWSRE=0.
            AWWSIM=0.
          ELSEIF(MSTP(46).EQ.1) THEN
            AWWARE=ASHRE+ATHRE+ASGRE+ASZRE+ATGRE+ATZRE+A4ARE
            AWWAIM=ASHIM+ATHIM+ASGIM+ASZIM+ATGIM+ATZIM+A4AIM
            AWWSRE=-ATHRE-AUHRE+ATGRE+ATZRE+AUGRE+AUZRE+A4SRE
            AWWSIM=-ATHIM-AUHIM+ATGIM+ATZIM+AUGIM+AUZIM+A4SIM
          ELSE
            AWWARE=ASGRE+ASZRE+ATGRE+ATZRE+A4ARE
            AWWAIM=ASGIM+ASZIM+ATGIM+ATZIM+A4AIM
            AWWSRE=ATGRE+ATZRE+AUGRE+AUZRE+A4SRE
            AWWSIM=ATGIM+ATZIM+AUGIM+AUZIM+A4SIM
          ENDIF
          AWWA2=AWWARE**2+AWWAIM**2
          AWWS2=AWWSRE**2+AWWSIM**2
 
        ELSE
C...Strongly interacting Z_L/W_L model of Dobado, Herrero, Terron.
          FWWA=COMFAC*(AEM/(4.*PARU(1)*XW))**2*(64./9.)*
     &    ABS(A00U+0.5*A20U+4.5*A11U*CTH)**2
          FWWS=COMFAC*(AEM/(4.*PARU(1)*XW))**2*64.*ABS(A20U)**2
        ENDIF
 
        DO 930 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 930
        EI=SIGN(1.,FLOAT(I))*KCHG(IABS(I),1)
        DO 920 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 920
        EJ=SIGN(1.,FLOAT(J))*KCHG(IABS(J),1)
        IF(EI*EJ.LT.0.) THEN
C...W+W-
          IF(MSTP(45).EQ.1) GOTO 920
          IF(MSTP(46).LE.2) FACWW=FWW*AWWA2*WIDS(24,1)
          IF(MSTP(46).GE.3) FACWW=FWWA*WIDS(24,1)
        ELSE
C...W+W+/W-W-
          IF(MSTP(45).EQ.2) GOTO 920
          IF(MSTP(46).LE.2) FACWW=FWW*AWWS2
          IF(MSTP(46).GE.3) FACWW=FWWS
          IF(EI.GT.0.) FACWW=FACWW*VINT(91)
          IF(EI.LT.0.) FACWW=FACWW*VINT(92)
        ENDIF
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACWW*VINT(180+I)*VINT(180+J)
        IF(EI*EJ.GT.0.) SIGH(NCHN)=0.5*SIGH(NCHN)
  920   CONTINUE
  930   CONTINUE
  940   CONTINUE
 
      ELSEIF(ISUB.EQ.78) THEN
C...W+/- + H0 -> W+/- + H0.
 
      ELSEIF(ISUB.EQ.79) THEN
C...H0 + H0 -> H0 + H0.
 
      ELSEIF(ISUB.EQ.80) THEN
C...q + gamma -> q' + pi+/-.
        FQPI=COMFAC*(2.*AEM/9.)*(-SH/TH)*(1./SH2+1./TH2)
        ASSH=ULALPS(MAX(0.5,0.5*SH))
        Q2FPSH=0.55/LOG(MAX(2.,2.*SH))
        DELSH=UH*SQRT(ASSH*Q2FPSH)
        ASUH=ULALPS(MAX(0.5,-0.5*UH))
        Q2FPUH=0.55/LOG(MAX(2.,-2.*UH))
        DELUH=SH*SQRT(ASUH*Q2FPUH)
        DO 960 I=MAX(-2,MMINA),MIN(2,MMAXA)
        IF(I.EQ.0) GOTO 960
        EI=KCHG(IABS(I),1)/3.
        EJ=SIGN(1.-ABS(EI),EI)
        DO 950 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,22).EQ.0) GOTO 950
        IF(ISDE.EQ.2.AND.KFAC(1,22)*KFAC(2,I).EQ.0) GOTO 950
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FQPI*(EI*DELSH+EJ*DELUH)**2
  950   CONTINUE
  960   CONTINUE
 
      ENDIF
 
C...C: 2 -> 2, tree diagrams with masses.
 
      ELSEIF(ISUB.LE.90) THEN
      IF(ISUB.EQ.81) THEN
C...q + q~ -> Q + Q~.
        FACQQB=COMFAC*AS**2*4./9.*(((TH-SQM3)**2+
     &  (UH-SQM3)**2)/SH2+2.*SQM3/SH)
        IF(MSTP(35).GE.1) FACQQB=FACQQB*PYHFTH(SH,SQM3,0.)
        WID2=1.
        IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &  WID2=WIDS(MINT(55)+20,1)
        FACQQB=FACQQB*WID2
        DO 970 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 970
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQQB
  970   CONTINUE
 
      ELSEIF(ISUB.EQ.82) THEN
C...g + g -> Q + Q~.
        IF(MSTP(34).EQ.0) THEN
          FACQQ1=COMFAC*FACA*AS**2*(1./6.)*((UH-SQM3)/(TH-SQM3)-
     &    2.*(UH-SQM3)**2/SH2+4.*(SQM3/SH)*(TH*UH-SQM3**2)/
     &    (TH-SQM3)**2)
          FACQQ2=COMFAC*FACA*AS**2*(1./6.)*((TH-SQM3)/(UH-SQM3)-
     &    2.*(TH-SQM3)**2/SH2+4.*(SQM3/SH)*(TH*UH-SQM3**2)/
     &    (UH-SQM3)**2)
        ELSE
          FACQQ1=COMFAC*FACA*AS**2*(1./6.)*((UH-SQM3)/(TH-SQM3)-
     &    2.25*(UH-SQM3)**2/SH2+4.5*(SQM3/SH)*(TH*UH-SQM3**2)/
     &    (TH-SQM3)**2+0.5*SQM3*TH/(TH-SQM3)**2-SQM3**2/(SH*(TH-SQM3)))
          FACQQ2=COMFAC*FACA*AS**2*(1./6.)*((TH-SQM3)/(UH-SQM3)-
     &    2.25*(TH-SQM3)**2/SH2+4.5*(SQM3/SH)*(TH*UH-SQM3**2)/
     &    (UH-SQM3)**2+0.5*SQM3*UH/(UH-SQM3)**2-SQM3**2/(SH*(UH-SQM3)))
        ENDIF
        IF(MSTP(35).GE.1) THEN
          FATRE=PYHFTH(SH,SQM3,2./7.)
          FACQQ1=FACQQ1*FATRE
          FACQQ2=FACQQ2*FATRE
        ENDIF
        WID2=1.
        IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &  WID2=WIDS(MINT(55)+20,1)
        FACQQ1=FACQQ1*WID2
        FACQQ2=FACQQ2*WID2
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 980
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQQ1
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=2
        SIGH(NCHN)=FACQQ2
  980   CONTINUE
 
      ELSEIF(ISUB.EQ.83) THEN
C...f + q -> f' + Q.
        FACQQS=COMFAC*(0.5*AEM/XW)**2*SH*(SH-SQM3)/(SQMW-TH)**2
        FACQQU=COMFAC*(0.5*AEM/XW)**2*UH*(UH-SQM3)/(SQMW-TH)**2
        DO 1000 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1000
        DO 990 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 990
        IF(I*J.GT.0.AND.MOD(IABS(I+J),2).EQ.0) GOTO 990
        IF(I*J.LT.0.AND.MOD(IABS(I+J),2).EQ.1) GOTO 990
        IF(IABS(I).LT.MINT(55).AND.MOD(IABS(I+MINT(55)),2).EQ.1) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          IF(MOD(MINT(55),2).EQ.0) FACCKM=VCKM(MINT(55)/2,
     &    (IABS(I)+1)/2)*VINT(180+J)
          IF(MOD(MINT(55),2).EQ.1) FACCKM=VCKM(IABS(I)/2,
     &    (MINT(55)+1)/2)*VINT(180+J)
          WID2=1.
          IF(I.GT.0) THEN
            IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
            IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &      WID2=WIDS(MINT(55)+20,2)
          ELSE
            IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
            IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &      WID2=WIDS(MINT(55)+20,3)
          ENDIF
          IF(I*J.GT.0) SIGH(NCHN)=FACQQS*FACCKM*WID2
          IF(I*J.LT.0) SIGH(NCHN)=FACQQU*FACCKM*WID2
        ENDIF
        IF(IABS(J).LT.MINT(55).AND.MOD(IABS(J+MINT(55)),2).EQ.1) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=2
          IF(MOD(MINT(55),2).EQ.0) FACCKM=VCKM(MINT(55)/2,
     &    (IABS(J)+1)/2)*VINT(180+I)
          IF(MOD(MINT(55),2).EQ.1) FACCKM=VCKM(IABS(J)/2,
     &    (MINT(55)+1)/2)*VINT(180+I)
          IF(J.GT.0) THEN
            IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
            IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &      WID2=WIDS(MINT(55)+20,2)
          ELSE
            IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
            IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &      WID2=WIDS(MINT(55)+20,3)
          ENDIF
          IF(I*J.GT.0) SIGH(NCHN)=FACQQS*FACCKM*WID2
          IF(I*J.LT.0) SIGH(NCHN)=FACQQU*FACCKM*WID2
        ENDIF
  990   CONTINUE
 1000   CONTINUE
 
      ELSEIF(ISUB.EQ.84) THEN
C...g + gamma -> Q + Q~.
        FMTU=SQM3/(SQM3-TH)+SQM3/(SQM3-UH)
        FACQQ=COMFAC*AS*AEM*(KCHG(IABS(MINT(55)),1)/3.)**2*
     &  ((SQM3-TH)/(SQM3-UH)+(SQM3-UH)/(SQM3-TH)+4.*FMTU*(1.-FMTU))
        IF(MSTP(35).GE.1) FACQQ=FACQQ*PYHFTH(SH,SQM3,0.)
        WID2=1.
        IF(MINT(55).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((MINT(55).EQ.7.OR.MINT(55).EQ.8).AND.MSTP(49).GE.1)
     &  WID2=WIDS(MINT(55)+20,1)
        FACQQ=FACQQ*WID2
        IF(KFAC(1,21)*KFAC(2,22).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ
        ENDIF
        IF(KFAC(1,22)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQ
        ENDIF
 
      ELSEIF(ISUB.EQ.85) THEN
C...gamma + gamma -> F + F~ (heavy fermion, quark or lepton).
        FMTU=SQM3/(SQM3-TH)+SQM3/(SQM3-UH)
        FACFF=COMFAC*AEM**2*(KCHG(IABS(MINT(56)),1)/3.)**4*2.*
     &  ((SQM3-TH)/(SQM3-UH)+(SQM3-UH)/(SQM3-TH)+4.*FMTU*(1.-FMTU))
        IF(IABS(MINT(56)).LT.10) FACFF=3.*FACFF
        IF(IABS(MINT(56)).LT.10.AND.MSTP(35).GE.1)
     &  FACFF=FACFF*PYHFTH(SH,SQM3,1.)
        WID2=1.
        IF(MINT(56).EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((MINT(56).EQ.7.OR.MINT(56).EQ.8).AND.MSTP(49).GE.1)
     &  WID2=WIDS(MINT(56)+20,1)
        IF(MINT(56).EQ.17.AND.MSTP(49).GE.1) WID2=WIDS(29,1)
        FACFF=FACFF*WID2
        IF(KFAC(1,22)*KFAC(2,22).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=22
          ISIG(NCHN,2)=22
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACFF
        ENDIF
 
      ELSEIF(ISUB.EQ.86) THEN
C...g + g -> J/Psi + g.
        FACQQG=COMFAC*AS**3*(5./9.)*PARP(38)*SQRT(SQM3)*
     &  (((SH*(SH-SQM3))**2+(TH*(TH-SQM3))**2+(UH*(UH-SQM3))**2)/
     &  ((TH-SQM3)*(UH-SQM3))**2)/(SH-SQM3)**2
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
 
      ELSEIF(ISUB.EQ.87) THEN
C...g + g -> chi_0c + g.
        PGTW=(SH*TH+TH*UH+UH*SH)/SH2
        QGTW=(SH*TH*UH)/SH**3
        RGTW=SQM3/SH
        FACQQG=COMFAC*AS**3*4.*(PARP(39)/SQRT(SQM3))*(1./SH)*
     &  (9.*RGTW**2*PGTW**4*(RGTW**4-2.*RGTW**2*PGTW+PGTW**2)-
     &  6.*RGTW*PGTW**3*QGTW*(2.*RGTW**4-5.*RGTW**2*PGTW+PGTW**2)-
     &  PGTW**2*QGTW**2*(RGTW**4+2.*RGTW**2*PGTW-PGTW**2)+
     &  2.*RGTW*PGTW*QGTW**3*(RGTW**2-PGTW)+6.*RGTW**2*QGTW**4)/
     &  (QGTW*(QGTW-RGTW*PGTW)**4)
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
 
      ELSEIF(ISUB.EQ.88) THEN
C...g + g -> chi_1c + g.
        PGTW=(SH*TH+TH*UH+UH*SH)/SH2
        QGTW=(SH*TH*UH)/SH**3
        RGTW=SQM3/SH
        FACQQG=COMFAC*AS**3*12.*(PARP(39)/SQRT(SQM3))*(1./SH)*
     &  PGTW**2*(RGTW*PGTW**2*(RGTW**2-4.*PGTW)+2.*QGTW*(-RGTW**4+
     &  5.*RGTW**2*PGTW+PGTW**2)-15.*RGTW*QGTW**2)/
     &  (QGTW-RGTW*PGTW)**4
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
 
      ELSEIF(ISUB.EQ.89) THEN
C...g + g -> chi_2c + g.
        PGTW=(SH*TH+TH*UH+UH*SH)/SH2
        QGTW=(SH*TH*UH)/SH**3
        RGTW=SQM3/SH
        FACQQG=COMFAC*AS**3*4.*(PARP(39)/SQRT(SQM3))*(1./SH)*
     &  (12.*RGTW**2*PGTW**4*(RGTW**4-2.*RGTW**2*PGTW+PGTW**2)-
     &  3.*RGTW*PGTW**3*QGTW*(8.*RGTW**4-RGTW**2*PGTW+4.*PGTW**2)+
     &  2.*PGTW**2*QGTW**2*(-7.*RGTW**4+43.*RGTW**2*PGTW+PGTW**2)+
     &  RGTW*PGTW*QGTW**3*(16.*RGTW**2-61.*PGTW)+12.*RGTW**2*QGTW**4)/
     &  (QGTW*(QGTW-RGTW*PGTW)**4)
        IF(KFAC(1,21)*KFAC(2,21).NE.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=21
          ISIG(NCHN,2)=21
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQQG
        ENDIF
      ENDIF
 
C...D: Mimimum bias processes.
 
      ELSEIF(ISUB.LE.100) THEN
      IF(ISUB.EQ.91) THEN
C...Elastic scattering.
        SIGS=SIGT(0,0,1)
 
      ELSEIF(ISUB.EQ.92) THEN
C...Single diffractive scattering (first side, i.e. XB).
        SIGS=SIGT(0,0,2)
 
      ELSEIF(ISUB.EQ.93) THEN
C...Single diffractive scattering (second side, i.e. AX).
        SIGS=SIGT(0,0,3)
 
      ELSEIF(ISUB.EQ.94) THEN
C...Double diffractive scattering.
        SIGS=SIGT(0,0,4)
 
      ELSEIF(ISUB.EQ.95) THEN
C...Low-pT scattering.
        SIGS=SIGT(0,0,5)
 
      ELSEIF(ISUB.EQ.96) THEN
C...Multiple interactions: sum of QCD processes.
        CALL PYWIDT(21,SH,WDTP,WDTE)
 
C...q + q' -> q + q'.
        FACQQ1=COMFAC*AS**2*4./9.*(SH2+UH2)/TH2
        FACQQB=COMFAC*AS**2*4./9.*((SH2+UH2)/TH2*FACA-
     &  MSTP(34)*2./3.*UH2/(SH*TH))
        FACQQ2=COMFAC*AS**2*4./9.*((SH2+TH2)/UH2-
     &  MSTP(34)*2./3.*SH2/(TH*UH))
        DO 1020 I=-3,3
        IF(I.EQ.0) GOTO 1020
        DO 1010 J=-3,3
        IF(J.EQ.0) GOTO 1010
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=111
        SIGH(NCHN)=FACQQ1
        IF(I.EQ.-J) SIGH(NCHN)=FACQQB
        IF(I.EQ.J) THEN
          SIGH(NCHN)=0.5*SIGH(NCHN)
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=112
          SIGH(NCHN)=0.5*FACQQ2
        ENDIF
 1010   CONTINUE
 1020   CONTINUE
 
C...q + q~ -> q' + q~' or g + g.
        FACQQB=COMFAC*AS**2*4./9.*(TH2+UH2)/SH2*(WDTE(0,1)+WDTE(0,2)+
     &  WDTE(0,3)+WDTE(0,4))
        FACGG1=COMFAC*AS**2*32./27.*(UH/TH-(2.+MSTP(34)*1./4.)*UH2/SH2)
        FACGG2=COMFAC*AS**2*32./27.*(TH/UH-(2.+MSTP(34)*1./4.)*TH2/SH2)
        DO 1030 I=-3,3
        IF(I.EQ.0) GOTO 1030
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=121
        SIGH(NCHN)=FACQQB
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=131
        SIGH(NCHN)=0.5*FACGG1
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=132
        SIGH(NCHN)=0.5*FACGG2
 1030   CONTINUE
 
C...q + g -> q + g.
        FACQG1=COMFAC*AS**2*4./9.*((2.+MSTP(34)*1./4.)*UH2/TH2-UH/SH)*
     &  FACA
        FACQG2=COMFAC*AS**2*4./9.*((2.+MSTP(34)*1./4.)*SH2/TH2-SH/UH)
        DO 1050 I=-3,3
        IF(I.EQ.0) GOTO 1050
        DO 1040 ISDE=1,2
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=281
        SIGH(NCHN)=FACQG1
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=282
        SIGH(NCHN)=FACQG2
 1040   CONTINUE
 1050   CONTINUE
 
C...g + g -> q + q~ or g + g.
        FACQQ1=COMFAC*AS**2*1./6.*(UH/TH-(2.+MSTP(34)*1./4.)*UH2/SH2)*
     &  (WDTE(0,1)+WDTE(0,2)+WDTE(0,3)+WDTE(0,4))*FACA
        FACQQ2=COMFAC*AS**2*1./6.*(TH/UH-(2.+MSTP(34)*1./4.)*TH2/SH2)*
     &  (WDTE(0,1)+WDTE(0,2)+WDTE(0,3)+WDTE(0,4))*FACA
        FACGG1=COMFAC*AS**2*9./4.*(SH2/TH2+2.*SH/TH+3.+2.*TH/SH+
     &  TH2/SH2)*FACA
        FACGG2=COMFAC*AS**2*9./4.*(UH2/SH2+2.*UH/SH+3.+2.*SH/UH+
     &  SH2/UH2)*FACA
        FACGG3=COMFAC*AS**2*9./4.*(TH2/UH2+2.*TH/UH+3+2.*UH/TH+UH2/TH2)
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=531
        SIGH(NCHN)=FACQQ1
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=532
        SIGH(NCHN)=FACQQ2
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=681
        SIGH(NCHN)=0.5*FACGG1
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=682
        SIGH(NCHN)=0.5*FACGG2
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=683
        SIGH(NCHN)=0.5*FACGG3
      ENDIF
 
C...E: 2 -> 1, loop diagrams.
 
      ELSEIF(ISUB.LE.110) THEN
      IF(ISUB.EQ.101) THEN
C...g + g -> gamma*/Z0.
 
      ELSEIF(ISUB.EQ.102) THEN
C...g + g -> H0 (or H'0, or A0).
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=4.*COMFAC/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        HI=HP*WDTP(13)/32.
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1060
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1060   CONTINUE
 
      ELSEIF(ISUB.EQ.103) THEN
C...gamma + gamma -> H0 (or H'0, or A0).
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=4.*COMFAC/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        HI=HP*WDTP(14)*2.
        IF(KFAC(1,22)*KFAC(2,22).EQ.0) GOTO 1070
        NCHN=NCHN+1
        ISIG(NCHN,1)=22
        ISIG(NCHN,2)=22
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1070   CONTINUE
 
C...F: 2 -> 2, box diagrams.
 
      ELSEIF(ISUB.EQ.110) THEN
C...f + f~ -> gamma + H0.
        THUH=MAX(TH*UH,SH*CKIN(3)**2)
        FACHG=COMFAC*(3.*AEM**4)/(2.*PARU(1)**2*XW*SQMW)*SH*THUH
        FACHG=FACHG*WIDS(KFHIGG,2)
C...Calculate loop contributions for intermediate gamma* and Z0.
        CIGTOT=CMPLX(0.,0.)
        CIZTOT=CMPLX(0.,0.)
        JMAX=3*MSTP(1)+1
        DO 1080 J=1,JMAX
        IF(J.LE.2*MSTP(1)) THEN
          FNC=1.
          EJ=KCHG(J,1)/3.
          AJ=SIGN(1.,EJ+0.1)
          VJ=AJ-4.*EJ*XWV
          BALP=SQM4/(2.*PMAS(J,1))**2
          BBET=SH/(2.*PMAS(J,1))**2
        ELSEIF(J.LE.3*MSTP(1)) THEN
          FNC=3.
          JL=2*(J-2*MSTP(1))-1
          EJ=KCHG(10+JL,1)/3.
          AJ=SIGN(1.,EJ+0.1)
          VJ=AJ-4.*EJ*XWV
          BALP=SQM4/(2.*PMAS(10+JL,1))**2
          BBET=SH/(2.*PMAS(10+JL,1))**2
        ELSE
          BALP=SQM4/(2.*PMAS(24,1))**2
          BBET=SH/(2.*PMAS(24,1))**2
        ENDIF
        BABI=1./(BALP-BBET)
        IF(BALP.LT.1.) THEN
          F0ALP=CMPLX(ASIN(SQRT(BALP)),0.)
          F1ALP=F0ALP**2
        ELSE
          F0ALP=CMPLX(LOG(SQRT(BALP)+SQRT(BALP-1.)),-0.5*PARU(1))
          F1ALP=-F0ALP**2
        ENDIF
        F2ALP=SQRT(ABS(BALP-1.)/BALP)*F0ALP
        IF(BBET.LT.1.) THEN
          F0BET=CMPLX(ASIN(SQRT(BBET)),0.)
          F1BET=F0BET**2
        ELSE
          F0BET=CMPLX(LOG(SQRT(BBET)+SQRT(BBET-1.)),-0.5*PARU(1))
          F1BET=-F0BET**2
        ENDIF
        F2BET=SQRT(ABS(BBET-1.)/BBET)*F0BET
        IF(J.LE.3*MSTP(1)) THEN
          FIF=0.5*BABI+BABI**2*(0.5*(1.-BALP+BBET)*(F1BET-F1ALP)+
     &    BBET*(F2BET-F2ALP))
          CIGTOT=CIGTOT+FNC*EJ**2*FIF
          CIZTOT=CIZTOT+FNC*EJ*VJ*FIF
        ELSE
          TXW=XW/XW1
          CIGTOT=CIGTOT-0.5*(BABI*(1.5+BALP)+BABI**2*((1.5-3.*BALP+
     &    4.*BBET)*(F1BET-F1ALP)+BBET*(2.*BALP+3.)*(F2BET-F2ALP)))
          CIZTOT=CIZTOT-0.5*BABI*XW1*((5.-TXW+2.*BALP*(1.-TXW))*
     &    (1.+2.*BABI*BBET*(F2BET-F2ALP))+BABI*(4.*BBET*(3.-TXW)-
     &    (2.*BALP-1.)*(5.-TXW))*(F1BET-F1ALP))
        ENDIF
 1080   CONTINUE
        GMMZ=PMAS(23,1)*PMAS(23,2)
        CIGTOT=CIGTOT/SH
        CIZTOT=CIZTOT*XWC/CMPLX(SH-SQMZ,GMMZ)
C...Loop over initial flavours.
        DO 1090 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1090
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACHG*FCOI*(ABS(EI*CIGTOT+VI*CIZTOT)**2+
     &  ABS(AI*CIZTOT)**2)
 1090   CONTINUE
 
      ENDIF
 
      ELSEIF(ISUB.LE.120) THEN
      IF(ISUB.EQ.111) THEN
C...f + f~ -> g + H0 (q + q~ -> g + H0 only).
        A5STUR=0.
        A5STUI=0.
        DO 1100 I=1,2*MSTP(1)
        SQMQ=PMAS(I,1)**2
        EPSS=4.*SQMQ/SH
        EPSH=4.*SQMQ/SQMH
        CALL PYWAUX(1,EPSS,W1SR,W1SI)
        CALL PYWAUX(1,EPSH,W1HR,W1HI)
        CALL PYWAUX(2,EPSS,W2SR,W2SI)
        CALL PYWAUX(2,EPSH,W2HR,W2HI)
        A5STUR=A5STUR+EPSH*(1.+SH/(TH+UH)*(W1SR-W1HR)+
     &  (0.25-SQMQ/(TH+UH))*(W2SR-W2HR))
        A5STUI=A5STUI+EPSH*(SH/(TH+UH)*(W1SI-W1HI)+
     &  (0.25-SQMQ/(TH+UH))*(W2SI-W2HI))
 1100   CONTINUE
        FACGH=COMFAC*FACA/(144.*PARU(1)**2)*AEM/XW*AS**3*SQMH/SQMW*
     &  SQMH/SH*(UH**2+TH**2)/(UH+TH)**2*(A5STUR**2+A5STUI**2)
        FACGH=FACGH*WIDS(25,2)
        DO 1110 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1110
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGH
 1110   CONTINUE
 
      ELSEIF(ISUB.EQ.112) THEN
C...f + g -> f + H0 (q + g -> q + H0 only).
        A5TSUR=0.
        A5TSUI=0.
        DO 1120 I=1,2*MSTP(1)
        SQMQ=PMAS(I,1)**2
        EPST=4.*SQMQ/TH
        EPSH=4.*SQMQ/SQMH
        CALL PYWAUX(1,EPST,W1TR,W1TI)
        CALL PYWAUX(1,EPSH,W1HR,W1HI)
        CALL PYWAUX(2,EPST,W2TR,W2TI)
        CALL PYWAUX(2,EPSH,W2HR,W2HI)
        A5TSUR=A5TSUR+EPSH*(1.+TH/(SH+UH)*(W1TR-W1HR)+
     &  (0.25-SQMQ/(SH+UH))*(W2TR-W2HR))
        A5TSUI=A5TSUI+EPSH*(TH/(SH+UH)*(W1TI-W1HI)+
     &  (0.25-SQMQ/(SH+UH))*(W2TI-W2HI))
 1120   CONTINUE
        FACQH=COMFAC*FACA/(384.*PARU(1)**2)*AEM/XW*AS**3*SQMH/SQMW*
     &  SQMH/(-TH)*(UH**2+SH**2)/(UH+SH)**2*(A5TSUR**2+A5TSUI**2)
        FACQH=FACQH*WIDS(25,2)
        DO 1140 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58)) GOTO 1140
        DO 1130 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1130
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 1130
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQH
 1130   CONTINUE
 1140   CONTINUE
 
      ELSEIF(ISUB.EQ.113) THEN
C...g + g -> g + H0.
        A2STUR=0.
        A2STUI=0.
        A2USTR=0.
        A2USTI=0.
        A2TUSR=0.
        A2TUSI=0.
        A4STUR=0.
        A4STUI=0.
        DO 1150 I=1,2*MSTP(1)
        SQMQ=PMAS(I,1)**2
        EPSS=4.*SQMQ/SH
        EPST=4.*SQMQ/TH
        EPSU=4.*SQMQ/UH
        EPSH=4.*SQMQ/SQMH
        IF(EPSH.LT.1.E-6) GOTO 1150
        CALL PYWAUX(1,EPSS,W1SR,W1SI)
        CALL PYWAUX(1,EPST,W1TR,W1TI)
        CALL PYWAUX(1,EPSU,W1UR,W1UI)
        CALL PYWAUX(1,EPSH,W1HR,W1HI)
        CALL PYWAUX(2,EPSS,W2SR,W2SI)
        CALL PYWAUX(2,EPST,W2TR,W2TI)
        CALL PYWAUX(2,EPSU,W2UR,W2UI)
        CALL PYWAUX(2,EPSH,W2HR,W2HI)
        CALL PYI3AU(EPSS,TH/UH,Y3STUR,Y3STUI)
        CALL PYI3AU(EPSS,UH/TH,Y3SUTR,Y3SUTI)
        CALL PYI3AU(EPST,SH/UH,Y3TSUR,Y3TSUI)
        CALL PYI3AU(EPST,UH/SH,Y3TUSR,Y3TUSI)
        CALL PYI3AU(EPSU,SH/TH,Y3USTR,Y3USTI)
        CALL PYI3AU(EPSU,TH/SH,Y3UTSR,Y3UTSI)
        CALL PYI3AU(EPSH,SQMH/SH*TH/UH,YHSTUR,YHSTUI)
        CALL PYI3AU(EPSH,SQMH/SH*UH/TH,YHSUTR,YHSUTI)
        CALL PYI3AU(EPSH,SQMH/TH*SH/UH,YHTSUR,YHTSUI)
        CALL PYI3AU(EPSH,SQMH/TH*UH/SH,YHTUSR,YHTUSI)
        CALL PYI3AU(EPSH,SQMH/UH*SH/TH,YHUSTR,YHUSTI)
        CALL PYI3AU(EPSH,SQMH/UH*TH/SH,YHUTSR,YHUTSI)
        W3STUR=YHSTUR-Y3STUR-Y3UTSR
        W3STUI=YHSTUI-Y3STUI-Y3UTSI
        W3SUTR=YHSUTR-Y3SUTR-Y3TUSR
        W3SUTI=YHSUTI-Y3SUTI-Y3TUSI
        W3TSUR=YHTSUR-Y3TSUR-Y3USTR
        W3TSUI=YHTSUI-Y3TSUI-Y3USTI
        W3TUSR=YHTUSR-Y3TUSR-Y3SUTR
        W3TUSI=YHTUSI-Y3TUSI-Y3SUTI
        W3USTR=YHUSTR-Y3USTR-Y3TSUR
        W3USTI=YHUSTI-Y3USTI-Y3TSUI
        W3UTSR=YHUTSR-Y3UTSR-Y3STUR
        W3UTSI=YHUTSI-Y3UTSI-Y3STUI
        B2STUR=SQMQ/SQMH**2*(SH*(UH-SH)/(SH+UH)+2.*TH*UH*(UH+2.*SH)/
     &  (SH+UH)**2*(W1TR-W1HR)+(SQMQ-SH/4.)*(0.5*W2SR+0.5*W2HR-W2TR+
     &  W3STUR)+SH2*(2.*SQMQ/(SH+UH)**2-0.5/(SH+UH))*(W2TR-W2HR)+
     &  0.5*TH*UH/SH*(W2HR-2.*W2TR)+0.125*(SH-12.*SQMQ-4.*TH*UH/SH)*
     &  W3TSUR)
        B2STUI=SQMQ/SQMH**2*(2.*TH*UH*(UH+2.*SH)/(SH+UH)**2*
     &  (W1TI-W1HI)+(SQMQ-SH/4.)*(0.5*W2SI+0.5*W2HI-W2TI+W3STUI)+
     &  SH2*(2.*SQMQ/(SH+UH)**2-0.5/(SH+UH))*(W2TI-W2HI)+0.5*TH*UH/SH*
     &  (W2HI-2.*W2TI)+0.125*(SH-12.*SQMQ-4.*TH*UH/SH)*W3TSUI)
        B2SUTR=SQMQ/SQMH**2*(SH*(TH-SH)/(SH+TH)+2.*UH*TH*(TH+2.*SH)/
     &  (SH+TH)**2*(W1UR-W1HR)+(SQMQ-SH/4.)*(0.5*W2SR+0.5*W2HR-W2UR+
     &  W3SUTR)+SH2*(2.*SQMQ/(SH+TH)**2-0.5/(SH+TH))*(W2UR-W2HR)+
     &  0.5*UH*TH/SH*(W2HR-2.*W2UR)+0.125*(SH-12.*SQMQ-4.*UH*TH/SH)*
     &  W3USTR)
        B2SUTI=SQMQ/SQMH**2*(2.*UH*TH*(TH+2.*SH)/(SH+TH)**2*
     &  (W1UI-W1HI)+(SQMQ-SH/4.)*(0.5*W2SI+0.5*W2HI-W2UI+W3SUTI)+
     &  SH2*(2.*SQMQ/(SH+TH)**2-0.5/(SH+TH))*(W2UI-W2HI)+0.5*UH*TH/SH*
     &  (W2HI-2.*W2UI)+0.125*(SH-12.*SQMQ-4.*UH*TH/SH)*W3USTI)
        B2TSUR=SQMQ/SQMH**2*(TH*(UH-TH)/(TH+UH)+2.*SH*UH*(UH+2.*TH)/
     &  (TH+UH)**2*(W1SR-W1HR)+(SQMQ-TH/4.)*(0.5*W2TR+0.5*W2HR-W2SR+
     &  W3TSUR)+TH2*(2.*SQMQ/(TH+UH)**2-0.5/(TH+UH))*(W2SR-W2HR)+
     &  0.5*SH*UH/TH*(W2HR-2.*W2SR)+0.125*(TH-12.*SQMQ-4.*SH*UH/TH)*
     &  W3STUR)
        B2TSUI=SQMQ/SQMH**2*(2.*SH*UH*(UH+2.*TH)/(TH+UH)**2*
     &  (W1SI-W1HI)+(SQMQ-TH/4.)*(0.5*W2TI+0.5*W2HI-W2SI+W3TSUI)+
     &  TH2*(2.*SQMQ/(TH+UH)**2-0.5/(TH+UH))*(W2SI-W2HI)+0.5*SH*UH/TH*
     &  (W2HI-2.*W2SI)+0.125*(TH-12.*SQMQ-4.*SH*UH/TH)*W3STUI)
        B2TUSR=SQMQ/SQMH**2*(TH*(SH-TH)/(TH+SH)+2.*UH*SH*(SH+2.*TH)/
     &  (TH+SH)**2*(W1UR-W1HR)+(SQMQ-TH/4.)*(0.5*W2TR+0.5*W2HR-W2UR+
     &  W3TUSR)+TH2*(2.*SQMQ/(TH+SH)**2-0.5/(TH+SH))*(W2UR-W2HR)+
     &  0.5*UH*SH/TH*(W2HR-2.*W2UR)+0.125*(TH-12.*SQMQ-4.*UH*SH/TH)*
     &  W3UTSR)
        B2TUSI=SQMQ/SQMH**2*(2.*UH*SH*(SH+2.*TH)/(TH+SH)**2*
     &  (W1UI-W1HI)+(SQMQ-TH/4.)*(0.5*W2TI+0.5*W2HI-W2UI+W3TUSI)+
     &  TH2*(2.*SQMQ/(TH+SH)**2-0.5/(TH+SH))*(W2UI-W2HI)+0.5*UH*SH/TH*
     &  (W2HI-2.*W2UI)+0.125*(TH-12.*SQMQ-4.*UH*SH/TH)*W3UTSI)
        B2USTR=SQMQ/SQMH**2*(UH*(TH-UH)/(UH+TH)+2.*SH*TH*(TH+2.*UH)/
     &  (UH+TH)**2*(W1SR-W1HR)+(SQMQ-UH/4.)*(0.5*W2UR+0.5*W2HR-W2SR+
     &  W3USTR)+UH2*(2.*SQMQ/(UH+TH)**2-0.5/(UH+TH))*(W2SR-W2HR)+
     &  0.5*SH*TH/UH*(W2HR-2.*W2SR)+0.125*(UH-12.*SQMQ-4.*SH*TH/UH)*
     &  W3SUTR)
        B2USTI=SQMQ/SQMH**2*(2.*SH*TH*(TH+2.*UH)/(UH+TH)**2*
     &  (W1SI-W1HI)+(SQMQ-UH/4.)*(0.5*W2UI+0.5*W2HI-W2SI+W3USTI)+
     &  UH2*(2.*SQMQ/(UH+TH)**2-0.5/(UH+TH))*(W2SI-W2HI)+0.5*SH*TH/UH*
     &  (W2HI-2.*W2SI)+0.125*(UH-12.*SQMQ-4.*SH*TH/UH)*W3SUTI)
        B2UTSR=SQMQ/SQMH**2*(UH*(SH-UH)/(UH+SH)+2.*TH*SH*(SH+2.*UH)/
     &  (UH+SH)**2*(W1TR-W1HR)+(SQMQ-UH/4.)*(0.5*W2UR+0.5*W2HR-W2TR+
     &  W3UTSR)+UH2*(2.*SQMQ/(UH+SH)**2-0.5/(UH+SH))*(W2TR-W2HR)+
     &  0.5*TH*SH/UH*(W2HR-2.*W2TR)+0.125*(UH-12.*SQMQ-4.*TH*SH/UH)*
     &  W3TUSR)
        B2UTSI=SQMQ/SQMH**2*(2.*TH*SH*(SH+2.*UH)/(UH+SH)**2*
     &  (W1TI-W1HI)+(SQMQ-UH/4.)*(0.5*W2UI+0.5*W2HI-W2TI+W3UTSI)+
     &  UH2*(2.*SQMQ/(UH+SH)**2-0.5/(UH+SH))*(W2TI-W2HI)+0.5*TH*SH/UH*
     &  (W2HI-2.*W2TI)+0.125*(UH-12.*SQMQ-4.*TH*SH/UH)*W3TUSI)
        B4STUR=0.25*EPSH*(-2./3.+0.25*(EPSH-1.)*(W2SR-W2HR+W3STUR))
        B4STUI=0.25*EPSH*0.25*(EPSH-1.)*(W2SI-W2HI+W3STUI)
        B4TUSR=0.25*EPSH*(-2./3.+0.25*(EPSH-1.)*(W2TR-W2HR+W3TUSR))
        B4TUSI=0.25*EPSH*0.25*(EPSH-1.)*(W2TI-W2HI+W3TUSI)
        B4USTR=0.25*EPSH*(-2./3.+0.25*(EPSH-1.)*(W2UR-W2HR+W3USTR))
        B4USTI=0.25*EPSH*0.25*(EPSH-1.)*(W2UI-W2HI+W3USTI)
        A2STUR=A2STUR+B2STUR+B2SUTR
        A2STUI=A2STUI+B2STUI+B2SUTI
        A2USTR=A2USTR+B2USTR+B2UTSR
        A2USTI=A2USTI+B2USTI+B2UTSI
        A2TUSR=A2TUSR+B2TUSR+B2TSUR
        A2TUSI=A2TUSI+B2TUSI+B2TSUI
        A4STUR=A4STUR+B4STUR+B4USTR+B4TUSR
        A4STUI=A4STUI+B4STUI+B4USTI+B4TUSI
 1150   CONTINUE
        FACGH=COMFAC*FACA*3./(128.*PARU(1)**2)*AEM/XW*AS**3*
     &  SQMH/SQMW*SQMH**3/(SH*TH*UH)*(A2STUR**2+A2STUI**2+A2USTR**2+
     &  A2USTI**2+A2TUSR**2+A2TUSI**2+A4STUR**2+A4STUI**2)
        FACGH=FACGH*WIDS(25,2)
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1160
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACGH
 1160   CONTINUE
 
      ELSEIF(ISUB.EQ.114.OR.ISUB.EQ.115) THEN
C...g + g -> gamma + gamma or g + g -> g + gamma.
        A0STUR=0.
        A0STUI=0.
        A0TSUR=0.
        A0TSUI=0.
        A0UTSR=0.
        A0UTSI=0.
        A1STUR=0.
        A1STUI=0.
        A2STUR=0.
        A2STUI=0.
        ALST=LOG(-SH/TH)
        ALSU=LOG(-SH/UH)
        ALTU=LOG(TH/UH)
        IMAX=2*MSTP(1)
        IF(MSTP(38).GE.1.AND.MSTP(38).LE.8) IMAX=MSTP(38)
        DO 1170 I=1,IMAX
        EI=KCHG(IABS(I),1)/3.
        EIWT=EI**2
        IF(ISUB.EQ.115) EIWT=EI
        SQMQ=PMAS(I,1)**2
        EPSS=4.*SQMQ/SH
        EPST=4.*SQMQ/TH
        EPSU=4.*SQMQ/UH
        IF((MSTP(38).GE.1.AND.MSTP(38).LE.8).OR.EPSS.LT.1.E-4) THEN
          B0STUR=1.+(TH-UH)/SH*ALTU+0.5*(TH2+UH2)/SH2*(ALTU**2+
     &    PARU(1)**2)
          B0STUI=0.
          B0TSUR=1.+(SH-UH)/TH*ALSU+0.5*(SH2+UH2)/TH2*ALSU**2
          B0TSUI=-PARU(1)*((SH-UH)/TH+(SH2+UH2)/TH2*ALSU)
          B0UTSR=1.+(SH-TH)/UH*ALST+0.5*(SH2+TH2)/UH2*ALST**2
          B0UTSI=-PARU(1)*((SH-TH)/UH+(SH2+TH2)/UH2*ALST)
          B1STUR=-1.
          B1STUI=0.
          B2STUR=-1.
          B2STUI=0.
        ELSE
          CALL PYWAUX(1,EPSS,W1SR,W1SI)
          CALL PYWAUX(1,EPST,W1TR,W1TI)
          CALL PYWAUX(1,EPSU,W1UR,W1UI)
          CALL PYWAUX(2,EPSS,W2SR,W2SI)
          CALL PYWAUX(2,EPST,W2TR,W2TI)
          CALL PYWAUX(2,EPSU,W2UR,W2UI)
          CALL PYI3AU(EPSS,TH/UH,Y3STUR,Y3STUI)
          CALL PYI3AU(EPSS,UH/TH,Y3SUTR,Y3SUTI)
          CALL PYI3AU(EPST,SH/UH,Y3TSUR,Y3TSUI)
          CALL PYI3AU(EPST,UH/SH,Y3TUSR,Y3TUSI)
          CALL PYI3AU(EPSU,SH/TH,Y3USTR,Y3USTI)
          CALL PYI3AU(EPSU,TH/SH,Y3UTSR,Y3UTSI)
          B0STUR=1.+(1.+2.*TH/SH)*W1TR+(1.+2.*UH/SH)*W1UR+
     &    0.5*((TH2+UH2)/SH2-EPSS)*(W2TR+W2UR)-
     &    0.25*EPST*(1.-0.5*EPSS)*(Y3SUTR+Y3TUSR)-
     &    0.25*EPSU*(1.-0.5*EPSS)*(Y3STUR+Y3UTSR)+
     &    0.25*(-2.*(TH2+UH2)/SH2+4.*EPSS+EPST+EPSU+0.5*EPST*EPSU)*
     &    (Y3TSUR+Y3USTR)
          B0STUI=(1.+2.*TH/SH)*W1TI+(1.+2.*UH/SH)*W1UI+
     &    0.5*((TH2+UH2)/SH2-EPSS)*(W2TI+W2UI)-
     &    0.25*EPST*(1.-0.5*EPSS)*(Y3SUTI+Y3TUSI)-
     &    0.25*EPSU*(1.-0.5*EPSS)*(Y3STUI+Y3UTSI)+
     &    0.25*(-2.*(TH2+UH2)/SH2+4.*EPSS+EPST+EPSU+0.5*EPST*EPSU)*
     &    (Y3TSUI+Y3USTI)
          B0TSUR=1.+(1.+2.*SH/TH)*W1SR+(1.+2.*UH/TH)*W1UR+
     &    0.5*((SH2+UH2)/TH2-EPST)*(W2SR+W2UR)-
     &    0.25*EPSS*(1.-0.5*EPST)*(Y3TUSR+Y3SUTR)-
     &    0.25*EPSU*(1.-0.5*EPST)*(Y3TSUR+Y3USTR)+
     &    0.25*(-2.*(SH2+UH2)/TH2+4.*EPST+EPSS+EPSU+0.5*EPSS*EPSU)*
     &    (Y3STUR+Y3UTSR)
          B0TSUI=(1.+2.*SH/TH)*W1SI+(1.+2.*UH/TH)*W1UI+
     &    0.5*((SH2+UH2)/TH2-EPST)*(W2SI+W2UI)-
     &    0.25*EPSS*(1.-0.5*EPST)*(Y3TUSI+Y3SUTI)-
     &    0.25*EPSU*(1.-0.5*EPST)*(Y3TSUI+Y3USTI)+
     &    0.25*(-2.*(SH2+UH2)/TH2+4.*EPST+EPSS+EPSU+0.5*EPSS*EPSU)*
     &    (Y3STUI+Y3UTSI)
          B0UTSR=1.+(1.+2.*TH/UH)*W1TR+(1.+2.*SH/UH)*W1SR+
     &    0.5*((TH2+SH2)/UH2-EPSU)*(W2TR+W2SR)-
     &    0.25*EPST*(1.-0.5*EPSU)*(Y3USTR+Y3TSUR)-
     &    0.25*EPSS*(1.-0.5*EPSU)*(Y3UTSR+Y3STUR)+
     &    0.25*(-2.*(TH2+SH2)/UH2+4.*EPSU+EPST+EPSS+0.5*EPST*EPSS)*
     &    (Y3TUSR+Y3SUTR)
          B0UTSI=(1.+2.*TH/UH)*W1TI+(1.+2.*SH/UH)*W1SI+
     &    0.5*((TH2+SH2)/UH2-EPSU)*(W2TI+W2SI)-
     &    0.25*EPST*(1.-0.5*EPSU)*(Y3USTI+Y3TSUI)-
     &    0.25*EPSS*(1.-0.5*EPSU)*(Y3UTSI+Y3STUI)+
     &    0.25*(-2.*(TH2+SH2)/UH2+4.*EPSU+EPST+EPSS+0.5*EPST*EPSS)*
     &    (Y3TUSI+Y3SUTI)
          B1STUR=-1.-0.25*(EPSS+EPST+EPSU)*(W2SR+W2TR+W2UR)+
     &    0.25*(EPSU+0.5*EPSS*EPST)*(Y3SUTR+Y3TUSR)+
     &    0.25*(EPST+0.5*EPSS*EPSU)*(Y3STUR+Y3UTSR)+
     &    0.25*(EPSS+0.5*EPST*EPSU)*(Y3TSUR+Y3USTR)
          B1STUI=-0.25*(EPSS+EPST+EPSU)*(W2SI+W2TI+W2UI)+
     &    0.25*(EPSU+0.5*EPSS*EPST)*(Y3SUTI+Y3TUSI)+
     &    0.25*(EPST+0.5*EPSS*EPSU)*(Y3STUI+Y3UTSI)+
     &    0.25*(EPSS+0.5*EPST*EPSU)*(Y3TSUI+Y3USTI)
          B2STUR=-1.+0.125*EPSS*EPST*(Y3SUTR+Y3TUSR)+
     &    0.125*EPSS*EPSU*(Y3STUR+Y3UTSR)+
     &    0.125*EPST*EPSU*(Y3TSUR+Y3USTR)
          B2STUI=0.125*EPSS*EPST*(Y3SUTI+Y3TUSI)+
     &    0.125*EPSS*EPSU*(Y3STUI+Y3UTSI)+
     &    0.125*EPST*EPSU*(Y3TSUI+Y3USTI)
        ENDIF
        A0STUR=A0STUR+EIWT*B0STUR
        A0STUI=A0STUI+EIWT*B0STUI
        A0TSUR=A0TSUR+EIWT*B0TSUR
        A0TSUI=A0TSUI+EIWT*B0TSUI
        A0UTSR=A0UTSR+EIWT*B0UTSR
        A0UTSI=A0UTSI+EIWT*B0UTSI
        A1STUR=A1STUR+EIWT*B1STUR
        A1STUI=A1STUI+EIWT*B1STUI
        A2STUR=A2STUR+EIWT*B2STUR
        A2STUI=A2STUI+EIWT*B2STUI
 1170   CONTINUE
        ASQSUM=A0STUR**2+A0STUI**2+A0TSUR**2+A0TSUI**2+A0UTSR**2+
     &  A0UTSI**2+4.*A1STUR**2+4.*A1STUI**2+A2STUR**2+A2STUI**2
        FACGG=COMFAC*FACA/(16.*PARU(1)**2)*AS**2*AEM**2*ASQSUM
        FACGP=COMFAC*FACA*5./(192.*PARU(1)**2)*AS**3*AEM*ASQSUM
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1180
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        IF(ISUB.EQ.114) SIGH(NCHN)=0.5*FACGG
        IF(ISUB.EQ.115) SIGH(NCHN)=FACGP
 1180   CONTINUE
 
      ELSEIF(ISUB.EQ.116) THEN
C...g + g -> gamma + Z0.
 
      ELSEIF(ISUB.EQ.117) THEN
C...g + g -> Z0 + Z0.
 
      ELSEIF(ISUB.EQ.118) THEN
C...g + g -> W+ + W-.
 
      ENDIF
 
C...G: 2 -> 3, tree diagrams.
 
      ELSEIF(ISUB.LE.140) THEN
      IF(ISUB.EQ.121) THEN
C...g + g -> Q + Q~ + H0.
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1190
        IA=KFPR(ISUBSV,2)
        PMF=PMAS(IA,1)
        FACQQH=COMFAC*(4.*PARU(1)*AEM/XW)*(4.*PARU(1)*AS)**2*
     &  (0.5*PMF/PMAS(24,1))**2
        IF(IA.LE.10.AND.MSTP(37).EQ.1.AND.MSTP(2).GE.1) FACQQH=
     &  FACQQH*(LOG(MAX(4.,PARP(37)**2*PMF**2/PARU(117)**2))/
     &  LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        WID2=1.
        IF(IA.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((IA.EQ.7.OR.IA.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(IA+20,1)
        FACQQH=FACQQH*WID2
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
          IKFI=1
          IF(IA.LE.10.AND.MOD(IA,2).EQ.0) IKFI=2
          IF(IA.GT.10) IKFI=3
          FACQQH=FACQQH*PARU(150+10*IHIGG+IKFI)**2
        ENDIF
        CALL PYQQBH(WTQQBH)
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=(1./PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQQH*WTQQBH*FACBW
 1190   CONTINUE
 
      ELSEIF(ISUB.EQ.122) THEN
C...q + q~ -> Q + Q~ + H0.
        IA=KFPR(ISUBSV,2)
        PMF=PMAS(IA,1)
        FACQQH=COMFAC*(4.*PARU(1)*AEM/XW)*(4.*PARU(1)*AS)**2*
     &  (0.5*PMF/PMAS(24,1))**2
        IF(IA.LE.10.AND.MSTP(37).EQ.1.AND.MSTP(2).GE.1) FACQQH=
     &  FACQQH*(LOG(MAX(4.,PARP(37)**2*PMF**2/PARU(117)**2))/
     &  LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        WID2=1.
        IF(IA.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((IA.EQ.7.OR.IA.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(IA+20,1)
        FACQQH=FACQQH*WID2
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) THEN
          IKFI=1
          IF(IA.LE.10.AND.MOD(IA,2).EQ.0) IKFI=2
          IF(IA.GT.10) IKFI=3
          FACQQH=FACQQH*PARU(150+10*IHIGG+IKFI)**2
        ENDIF
        CALL PYQQBH(WTQQBH)
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=(1./PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        DO 1200 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1200
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACQQH*WTQQBH*FACBW
 1200   CONTINUE
 
      ELSEIF(ISUB.EQ.123) THEN
C...f + f' -> f + f' + H0 (or H'0, or A0) (Z0 + Z0 -> H0 as
C...inner process).
        FACNOR=COMFAC*(4.*PARU(1)*AEM/(XW*XW1))**3*SQMZ/32.
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACNOR=FACNOR*
     &  PARU(154+10*IHIGG)**2
        FACPRP=1./((VINT(215)-VINT(204)**2)*(VINT(216)-VINT(209)**2))**2
        FACZZ1=FACNOR*FACPRP*(0.5*TAUP*VINT(2))*VINT(219)
        FACZZ2=FACNOR*FACPRP*VINT(217)*VINT(218)
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=(1./PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        DO 1220 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1220
        IA=IABS(I)
        DO 1210 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1210
        JA=IABS(J)
        EI=KCHG(IA,1)*ISIGN(1,I)/3.
        AI=SIGN(1.,KCHG(IA,1)+0.5)*ISIGN(1,I)
        VI=AI-4.*EI*XWV
        EJ=KCHG(JA,1)*ISIGN(1,J)/3.
        AJ=SIGN(1.,KCHG(JA,1)+0.5)*ISIGN(1,J)
        VJ=AJ-4.*EJ*XWV
        FACLR1=(VI**2+AI**2)*(VJ**2+AJ**2)+4.*VI*AI*VJ*AJ
        FACLR2=(VI**2+AI**2)*(VJ**2+AJ**2)-4.*VI*AI*VJ*AJ
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=(FACLR1*FACZZ1+FACLR2*FACZZ2)*FACBW
 1210   CONTINUE
 1220   CONTINUE
 
      ELSEIF(ISUB.EQ.124) THEN
C...f + f' -> f" + f"' + H0 (or H'0, or A0) (W+ + W- -> H0 as
C...inner process).
        FACNOR=COMFAC*(4.*PARU(1)*AEM/XW)**3*SQMW
        IF(MSTP(4).GE.1.OR.IHIGG.GE.2) FACNOR=FACNOR*
     &  PARU(155+10*IHIGG)**2
        FACPRP=1./((VINT(215)-VINT(204)**2)*(VINT(216)-VINT(209)**2))**2
        FACWW=FACNOR*FACPRP*(0.5*TAUP*VINT(2))*VINT(219)
        CALL PYWIDT(KFHIGG,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        FACBW=(1./PARU(1))*VINT(2)*HF/((SH-SQMH)**2+HS**2)
        IF(ABS(SH-SQMH).GT.100.*HS) FACBW=0.
        DO 1240 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1240
        EI=SIGN(1.,FLOAT(I))*KCHG(IABS(I),1)
        DO 1230 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1230
        EJ=SIGN(1.,FLOAT(J))*KCHG(IABS(J),1)
        IF(EI*EJ.GT.0.) GOTO 1230
        FACLR=VINT(180+I)*VINT(180+J)
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACLR*FACWW*FACBW
 1230   CONTINUE
 1240   CONTINUE
 
      ELSEIF(ISUB.EQ.131) THEN
C...g + g -> Z0 + q + qbar.
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1280
 
C...Read out information on flavours, masses, couplings.
        KFQ=KFPR(131,2)
        KFL=IABS(KFDP(MINT(35),1))
        PMH=SQRT(SH)
        PMQQ=SQRT(VINT(64))
        PMLL=SQRT(VINT(63))
        PMQ=PMAS(KFQ,1)
        QFQ=KCHG(KFQ,1)/3.
        AFQ=SIGN(1.,QFQ+0.1)
        VFQ=AFQ-4.*XWV*QFQ
        QFL=KCHG(KFL,1)/3.
        AFL=SIGN(1.,QFL+0.1)
        VFL=AFL-4.*XWV*QFL
        WID2=1.
        IF(KFQ.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((KFQ.EQ.7.OR.KFQ.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(KFQ+20,1)
 
C...Set line numbers for particles.
        IG1=MINT(84)+1
        IG2=MINT(84)+2
        IQ1=MINT(84)+3
        IQ2=MINT(84)+4
        IL1=MINT(84)+5
        IL2=MINT(84)+6
        IZ=MINT(84)+7
 
C...Reconstruct decay kinematics.
        DO 1260 I=MINT(84)+1,MINT(84)+7
        K(I,1)=1
        DO 1250 J=1,5
        P(I,J)=0.
 1250   CONTINUE
 1260   CONTINUE
        P(IG1,4)=0.5*PMH
        P(IG1,3)=P(IG1,4)
        P(IG2,4)=P(IG1,4)
        P(IG2,3)=-P(IG1,3)
        P(IQ1,5)=PMQ
        P(IQ1,4)=0.5*PMQQ
        P(IQ1,3)=SQRT(MAX(0.,P(IQ1,4)**2-PMQ**2))
        P(IQ2,5)=PMQ
        P(IQ2,4)=P(IQ1,4)
        P(IQ2,3)=-P(IQ1,3)
        P(IL1,4)=0.5*PMLL
        P(IL1,3)=P(IL1,4)
        P(IL2,4)=P(IL1,4)
        P(IL2,3)=-P(IL1,3)
        P(IZ,5)=PMLL
        P(IZ,4)=0.5*(PMH+(PMLL**2-PMQQ**2)/PMH)
        P(IZ,3)=SQRT(MAX(0.,P(IZ,4)**2-PMLL**2))
        CALL LUDBRB(IQ1,IQ2,ACOS(VINT(83)),VINT(84),0D0,0D0,
     &  -DBLE(P(IZ,3)/(PMH-P(IZ,4))))
        CALL LUDBRB(IL1,IL2,ACOS(VINT(81)),VINT(82),0D0,0D0,
     &  DBLE(P(IZ,3)/P(IZ,4)))
        CALL LUDBRB(IQ1,IZ,ACOS(VINT(23)),VINT(24),0D0,0D0,0D0)
 
C...Interface information to program of Ronald Kleiss.
        RKMQ=PMQ
        RKMZ=PMAS(23,1)
        RKGZ=PMAS(23,2)
        RKVQ=VFQ
        RKAQ=AFQ
        RKVL=VFL
        RKAL=AFL
        RKG1(0)=P(IG1,4)
        RKG2(0)=P(IG2,4)
        RKQ1(0)=P(IQ1,4)
        RKQ2(0)=P(IQ2,4)
        RKL1(0)=P(IL1,4)
        RKL2(0)=P(IL2,4)
        DO 1270 J=1,3
        RKG1(J)=P(IG1,J)
        RKG2(J)=P(IG2,J)
        RKQ1(J)=P(IQ1,J)
        RKQ2(J)=P(IQ2,J)
        RKL1(J)=P(IL1,J)
        RKL2(J)=P(IL2,J)
 1270   CONTINUE
        CALL RKBBV(RKG1,RKG2,RKQ1,RKQ2,RKL1,RKL2,1,RKRES)
 
C...Multiply with normalization factors.
        WTMEP=1./(2.*SH*PARU(2)**8)
        WTCOU=AS**2*(4.*PARU(1)*AEM*XWC)**2
        WTZQQ=WTMEP*WTCOU*RKRES
        WTPHS=(PARU(1)/2.)**2*PMQQ**2*
     &  (PARU(1)*((PMLL**2-PMAS(23,1)**2)**2+(PMAS(23,1)*
     &  PMAS(23,2))**2)/(PMAS(23,1)*PMAS(23,2)))*0.5*SH
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=INT(1.5+RLU(0))
        SIGH(NCHN)=COMFAC*WTPHS*WTZQQ*WID2
 1280   CONTINUE
      ENDIF
 
C...H: 2 -> 1, tree diagrams, non-standard model processes.
 
      ELSEIF(ISUB.LE.160) THEN
      IF(ISUB.EQ.141) THEN
C...f + f~ -> gamma*/Z0/Z'0.
        MINT(61)=2
        CALL PYWIDT(32,SH,WDTP,WDTE)
        HP0=AEM/3.*SH
        HP1=AEM/3.*XWC*SH
        HP2=HP1
        HS=HP1*VINT(117)
        HSP=HP2*WDTP(0)
        FACZP=4.*COMFAC*3.
        DO 1290 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1290
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI)
        VI=AI-4.*EI*XWV
        IF(IABS(I).LT.10) THEN
          VPI=PARU(123-2*MOD(IABS(I),2))
          API=PARU(124-2*MOD(IABS(I),2))
        ELSE
          VPI=PARU(127-2*MOD(IABS(I),2))
          API=PARU(128-2*MOD(IABS(I),2))
        ENDIF
        HI0=HP0
        IF(IABS(I).LE.10) HI0=HI0*FACA/3.
        HI1=HP1
        IF(IABS(I).LE.10) HI1=HI1*FACA/3.
        HI2=HP2
        IF(IABS(I).LE.10) HI2=HI2*FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACZP*(EI**2/SH2*HI0*HP0*VINT(111)+EI*VI*
     &  (1.-SQMZ/SH)/((SH-SQMZ)**2+HS**2)*(HI0*HP1+HI1*HP0)*VINT(112)+
     &  EI*VPI*(1.-SQMZP/SH)/((SH-SQMZP)**2+HSP**2)*(HI0*HP2+HI2*HP0)*
     &  VINT(113)+(VI**2+AI**2)/((SH-SQMZ)**2+HS**2)*HI1*HP1*VINT(114)+
     &  (VI*VPI+AI*API)*((SH-SQMZ)*(SH-SQMZP)+HS*HSP)/(((SH-SQMZ)**2+
     &  HS**2)*((SH-SQMZP)**2+HSP**2))*(HI1*HP2+HI2*HP1)*VINT(115)+
     &  (VPI**2+API**2)/((SH-SQMZP)**2+HSP**2)*HI2*HP2*VINT(116))
 1290   CONTINUE
 
      ELSEIF(ISUB.EQ.142) THEN
C...f + f~' -> W'+/-.
        CALL PYWIDT(34,SH,WDTP,WDTE)
        HP=AEM/(24.*XW)*SH
        HS=HP*WDTP(0)
        FACBW=4.*COMFAC/((SH-SQMWP)**2+HS**2)*3.
        DO 1310 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1310
        IA=IABS(I)
        DO 1300 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1300
        JA=IABS(J)
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1300
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 1300
        KCHW=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        HI=HP*(PARU(133)**2+PARU(134)**2)
        IF(IA.LE.10) HI=HP*(PARU(131)**2+PARU(132)**2)*
     &  VCKM((IA+1)/2,(JA+1)/2)*FACA/3.
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        HF=HP*(WDTE(0,1)+WDTE(0,(5-KCHW)/2)+WDTE(0,4))
        SIGH(NCHN)=HI*FACBW*HF
 1300   CONTINUE
 1310   CONTINUE
 
      ELSEIF(ISUB.EQ.143) THEN
C...f + f~' -> H+/-.
        CALL PYWIDT(37,SH,WDTP,WDTE)
        HP=AEM/(8.*XW)*SH/SQMW*SH
        HS=HP*WDTP(0)
        FACBW=4.*COMFAC/((SH-SQMHC)**2+HS**2)
        DO 1330 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1330
        IA=IABS(I)
        IM=(MOD(IA,10)+1)/2
        DO 1320 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1320
        JA=IABS(J)
        JM=(MOD(JA,10)+1)/2
        IF(I*J.GT.0.OR.IA.EQ.JA.OR.IM.NE.JM) GOTO 1320
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 1320
        IF(MOD(IA,2).EQ.0) THEN
          IU=IA
          IL=JA
        ELSE
          IU=JA
          IL=IA
        ENDIF
        RML=PMAS(IL,1)**2/SH
        RMU=PMAS(IU,1)**2/SH
        IF(IL.LE.10.AND.MSTP(37).EQ.1.AND.MSTP(2).GE.1) RML=RML*
     &  (LOG(MAX(4.,PARP(37)**2*RML*SH/PARU(117)**2))/
     &  LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        HI=HP*(RML*PARU(141)**2+RMU/PARU(141)**2)
        IF(IA.LE.10) HI=HI*FACA/3.
        KCHHC=(KCHG(IA,1)*ISIGN(1,I)+KCHG(JA,1)*ISIGN(1,J))/3
        HF=HP*(WDTE(0,1)+WDTE(0,(5-KCHHC)/2)+WDTE(0,4))
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1320   CONTINUE
 1330   CONTINUE
 
      ELSEIF(ISUB.EQ.144) THEN
C...f + f~' -> R.
        CALL PYWIDT(40,SH,WDTP,WDTE)
        HP=AEM/(12.*XW)*SH
        HS=HP*WDTP(0)
        FACBW=4.*COMFAC/((SH-SQMR)**2+HS**2)*3.
        DO 1350 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1350
        IA=IABS(I)
        DO 1340 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1340
        JA=IABS(J)
        IF(I*J.GT.0.OR.IABS(IA-JA).NE.2) GOTO 1340
        HI=HP
        IF(IA.LE.10) HI=HI*FACA/3.
        HF=HP*(WDTE(0,1)+WDTE(0,(10-(I+J))/4)+WDTE(0,4))
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1340   CONTINUE
 1350   CONTINUE
 
      ELSEIF(ISUB.EQ.145) THEN
C...q + l -> LQ (leptoquark).
        CALL PYWIDT(39,SH,WDTP,WDTE)
        HP=AEM/4.*SH
        HS=HP*WDTP(0)
        FACBW=4.*COMFAC/((SH-SQMLQ)**2+HS**2)
        IF(ABS(SH-SQMLQ).GT.100.*HS) FACBW=0.
        KFLQQ=KFDP(MDCY(39,2),1)
        KFLQL=KFDP(MDCY(39,2),2)
        DO 1370 I=MMIN1,MMAX1
        IF(KFAC(1,I).EQ.0) GOTO 1370
        IA=IABS(I)
        IF(IA.NE.KFLQQ.AND.IA.NE.KFLQL) GOTO 1370
        DO 1360 J=MMIN2,MMAX2
        IF(KFAC(2,J).EQ.0) GOTO 1360
        JA=IABS(J)
        IF(JA.NE.KFLQQ.AND.JA.NE.KFLQL) GOTO 1360
        IF(I*J.NE.KFLQQ*KFLQL) GOTO 1360
        IF(IA.EQ.KFLQQ) KCHLQ=ISIGN(1,I)
        IF(JA.EQ.KFLQQ) KCHLQ=ISIGN(1,J)
        HI=HP*PARU(151)
        HF=HP*(WDTE(0,1)+WDTE(0,(5-KCHLQ)/2)+WDTE(0,4))
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1360   CONTINUE
 1370   CONTINUE
 
      ELSEIF(ISUB.EQ.147.OR.ISUB.EQ.148) THEN
C...d + g -> d* and u + g -> u* (excited quarks).
        KFQEXC=ISUB-146
        KFQSTR=ISUB-140
        CALL PYWIDT(KFQSTR,SH,WDTP,WDTE)
        HP=SH
        HS=HP*WDTP(0)
        FACBW=COMFAC/((SH-PMAS(KFQSTR,1)**2)**2+HS**2)
        FACBW=FACBW*AS*PARU(159)**2*SH/(3.*PARU(155)**2)
        IF(ABS(SH-PMAS(KFQSTR,1)**2).GT.100.*HS) FACBW=0.
        DO 1390 I=-KFQEXC,KFQEXC,2*KFQEXC
        DO 1380 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1380
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 1380
        HI=HP
        IF(I.GT.0) HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        IF(I.LT.0) HF=HP*(WDTE(0,1)+WDTE(0,3)+WDTE(0,4))
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1380   CONTINUE
 1390   CONTINUE
 
      ELSEIF(ISUB.EQ.149) THEN
C...g + g -> eta_techni.
        CALL PYWIDT(38,SH,WDTP,WDTE)
        HP=SH
        HS=HP*WDTP(0)
        FACBW=COMFAC*0.5/((SH-PMAS(38,1)**2)**2+HS**2)
        IF(ABS(SH-PMAS(38,1)**2).GT.100.*HS) FACBW=0.
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1400
        HI=HP*WDTP(3)
        HF=HP*(WDTE(0,1)+WDTE(0,2)+WDTE(0,4))
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=HI*FACBW*HF
 1400   CONTINUE
 
      ENDIF
 
C...I: 2 -> 2, tree diagrams, non-standard model processes.
 
      ELSE
      IF(ISUB.EQ.161) THEN
C...f + g -> f' + H+/- (b + g -> t + H+/- only)
C...(choice of only b and t to avoid kinematics problems).
        FHCQ=COMFAC*FACA*AS*AEM/XW*1./24
        DO 1420 I=MMINA,MMAXA
        IA=IABS(I)
        IF(IA.NE.5) GOTO 1420
        SQML=PMAS(IA,1)**2
        IF(IA.LE.10.AND.MSTP(37).EQ.1.AND.MSTP(2).GE.1) SQML=SQML*
     &  (LOG(MAX(4.,PARP(37)**2*SQML/PARU(117)**2))/
     &  LOG(MAX(4.,SH/PARU(117)**2)))**(24./(33.-2.*MSTU(118)))
        IUA=IA+MOD(IA,2)
        SQMQ=PMAS(IUA,1)**2
        FACHCQ=FHCQ*(SQML*PARU(141)**2+SQMQ/PARU(141)**2)/SQMW*
     &  (SH/(SQMQ-UH)+2.*SQMQ*(SQMHC-UH)/(SQMQ-UH)**2+(SQMQ-UH)/SH+
     &  2.*SQMQ/(SQMQ-UH)+2.*(SQMHC-UH)/(SQMQ-UH)*(SQMHC-SQMQ-SH)/SH)
        KCHHC=ISIGN(1,KCHG(IA,1)*ISIGN(1,I))
        DO 1410 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1410
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,1).EQ.0) GOTO 1410
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACHCQ*WIDS(37,(5-KCHHC)/2)
 1410   CONTINUE
 1420   CONTINUE
 
      ELSEIF(ISUB.EQ.162) THEN
C...q + g -> LQ + l~; LQ=leptoquark.
        FACLQ=COMFAC*FACA*PARU(151)*(AS*AEM/6.)*(-TH/SH)*
     &  (UH2+SQMLQ**2)/(UH-SQMLQ)**2
        KFLQQ=KFDP(MDCY(39,2),1)
        DO 1440 I=MMINA,MMAXA
        IF(IABS(I).NE.KFLQQ) GOTO 1440
        KCHLQ=ISIGN(1,I)
        DO 1430 ISDE=1,2
        IF(ISDE.EQ.1.AND.KFAC(1,I)*KFAC(2,21).EQ.0) GOTO 1430
        IF(ISDE.EQ.2.AND.KFAC(1,21)*KFAC(2,I).EQ.0) GOTO 1430
        NCHN=NCHN+1
        ISIG(NCHN,ISDE)=I
        ISIG(NCHN,3-ISDE)=21
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACLQ*WIDS(39,(5-KCHLQ)/2)
 1430   CONTINUE
 1440   CONTINUE
 
      ELSEIF(ISUB.EQ.163) THEN
C...g + g -> LQ + LQ~; LQ=leptoquark.
        FACLQ=COMFAC*FACA*WIDS(39,1)*(AS**2/2.)*
     &  (7./48.+3.*(UH-TH)**2/(16.*SH2))*(1.+2.*SQMLQ*TH/(TH-SQMLQ)**2+
     &  2.*SQMLQ*UH/(UH-SQMLQ)**2+4.*SQMLQ**2/((TH-SQMLQ)*(UH-SQMLQ)))
        IF(KFAC(1,21)*KFAC(2,21).EQ.0) GOTO 1450
        NCHN=NCHN+1
        ISIG(NCHN,1)=21
        ISIG(NCHN,2)=21
C...Since don't know proper colour flow, randomize between alternatives.
        ISIG(NCHN,3)=INT(1.5+RLU(0))
        SIGH(NCHN)=FACLQ
 1450   CONTINUE
 
      ELSEIF(ISUB.EQ.164) THEN
C...q + q~ -> LQ + LQ~; LQ=leptoquark.
        FACLQA=COMFAC*WIDS(39,1)*(AS**2/9.)*
     &  (SH*(SH-4.*SQMLQ)-(UH-TH)**2)/SH2
        FACLQS=COMFAC*WIDS(39,1)*((PARU(151)**2*AEM**2/8.)*
     &  (-SH*TH-(SQMLQ-TH)**2)/TH2+(PARU(151)*AEM*AS/18.)*
     &  ((SQMLQ-TH)*(UH-TH)+SH*(SQMLQ+TH))/(SH*TH))
        KFLQQ=KFDP(MDCY(39,2),1)
        DO 1460 I=MMINA,MMAXA
        IF(I.EQ.0.OR.IABS(I).GT.MSTP(58).OR.
     &  KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1460
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=FACLQA
        IF(IABS(I).EQ.KFLQQ) SIGH(NCHN)=FACLQA+FACLQS
 1460   CONTINUE
 
      ELSEIF(ISUB.EQ.165) THEN
C...q + q~ -> l+ + l- (including contact term for compositeness).
        ZRATR=XWC*SH*(SH-SQMZ)/((SH-SQMZ)**2+SQMZ*PMAS(23,2)**2)
        ZRATI=XWC*SH*PMAS(23,1)*PMAS(23,2)/
     &  ((SH-SQMZ)**2+SQMZ*PMAS(23,2)**2)
        KFF=IABS(KFPR(ISUB,1))
        EF=KCHG(KFF,1)/3.
        AF=SIGN(1.,EF+0.1)
        VF=AF-4.*EF*XWV
        VALF=VF+AF
        VARF=VF-AF
        FCOF=1.
        IF(KFF.LE.10) FCOF=3.
        WID2=1.
        IF(KFF.EQ.6.AND.MSTP(48).GE.1) WID2=WIDS(26,1)
        IF((KFF.EQ.7.OR.KFF.EQ.8).AND.MSTP(49).GE.1) WID2=WIDS(KFF+20,1)
        IF((KFF.EQ.17.OR.KFF.EQ.18).AND.MSTP(49).GE.1) WID2=
     &  WIDS(KFF+12,1)
        DO 1470 I=MMINA,MMAXA
        IF(I.EQ.0.OR.KFAC(1,I)*KFAC(2,-I).EQ.0) GOTO 1470
        EI=KCHG(IABS(I),1)/3.
        AI=SIGN(1.,EI+0.1)
        VI=AI-4.*EI*XWV
        VALI=VI+AI
        VARI=VI-AI
        FCOI=1.
        IF(IABS(I).LE.10) FCOI=FACA/3.
        IF((MSTP(5).EQ.1.AND.IABS(I).LE.2).OR.MSTP(5).EQ.2) THEN
          FGZA=(EI*EF+VALI*VALF*ZRATR+PARU(156)*SH/
     &    (AEM*PARU(155)**2))**2+(VALI*VALF*ZRATI)**2+
     &    (EI*EF+VARI*VARF*ZRATR)**2+(VARI*VARF*ZRATI)**2
        ELSE
          FGZA=(EI*EF+VALI*VALF*ZRATR)**2+(VALI*VALF*ZRATI)**2+
     &    (EI*EF+VARI*VARF*ZRATR)**2+(VARI*VARF*ZRATI)**2
        ENDIF
        FGZB=(EI*EF+VALI*VARF*ZRATR)**2+(VALI*VARF*ZRATI)**2+
     &  (EI*EF+VARI*VALF*ZRATR)**2+(VARI*VALF*ZRATI)**2
        FGZAB=AEM**2*(FGZA*UH2/SH2+FGZB*TH2/SH2)
        IF((MSTP(5).EQ.3.AND.IABS(I).EQ.2).OR.(MSTP(5).EQ.4.AND.
     &  MOD(IABS(I),2).EQ.0)) FGZAB=FGZAB+SH2/(2.*PARU(155)**4)
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=-I
        ISIG(NCHN,3)=1
        SIGH(NCHN)=COMFAC*FCOI*FCOF*FGZAB*WID2
 1470   CONTINUE
 
      ELSEIF(ISUB.EQ.166) THEN
C...q + q'~ -> l + nu_l (including contact term for compositeness).
        WFAC=(1./4.)*(AEM/XW)**2*UH2/((SH-SQMW)**2+SQMW*PMAS(24,2)**2)
        WCIFAC=WFAC+SH2/(4.*PARU(155)**4)
        KFF=IABS(KFPR(ISUB,1))
        FCOF=1.
        IF(KFF.LE.10) FCOF=3.
        DO 1490 I=MMIN1,MMAX1
        IF(I.EQ.0.OR.KFAC(1,I).EQ.0) GOTO 1490
        IA=IABS(I)
        DO 1480 J=MMIN2,MMAX2
        IF(J.EQ.0.OR.KFAC(2,J).EQ.0) GOTO 1480
        JA=IABS(J)
        IF(I*J.GT.0.OR.MOD(IA+JA,2).EQ.0) GOTO 1480
        IF((IA.LE.10.AND.JA.GT.10).OR.(IA.GT.10.AND.JA.LE.10)) GOTO 1480
        FCOI=1.
        IF(IA.LE.10) FCOI=VCKM((IA+1)/2,(JA+1)/2)*FACA/3.
        WID2=1.
        IF((I.GT.0.AND.MOD(I,2).EQ.0).OR.(J.GT.0.AND.MOD(J,2).EQ.0))
     &  THEN
          IF(KFF.EQ.5.AND.MSTP(48).GE.1) WID2=WIDS(26,2)
          IF(KFF.EQ.7.AND.MSTP(49).GE.1) WID2=WIDS(28,2)*WIDS(27,3)
          IF(KFF.EQ.17.AND.MSTP(49).GE.1) WID2=WIDS(30,2)*WIDS(29,3)
        ELSE
          IF(KFF.EQ.5.AND.MSTP(48).GE.1) WID2=WIDS(26,3)
          IF(KFF.EQ.7.AND.MSTP(49).GE.1) WID2=WIDS(28,3)*WIDS(27,2)
          IF(KFF.EQ.17.AND.MSTP(49).GE.1) WID2=WIDS(30,3)*WIDS(29,2)
        ENDIF
        NCHN=NCHN+1
        ISIG(NCHN,1)=I
        ISIG(NCHN,2)=J
        ISIG(NCHN,3)=1
        SIGH(NCHN)=COMFAC*FCOI*FCOF*WFAC*WID2
        IF((MSTP(5).EQ.3.AND.IA.LE.2.AND.JA.LE.2).OR.MSTP(5).EQ.4)
     &  SIGH(NCHN)=COMFAC*FCOI*FCOF*WCIFAC*WID2
 1480   CONTINUE
 1490   CONTINUE
 
      ELSEIF(ISUB.EQ.167.OR.ISUB.EQ.168) THEN
C...d + g -> d* and u + g -> u* (excited quarks).
        KFQEXC=ISUB-166
        KFQSTR=ISUB-160
        FACQSA=COMFAC*(SH/PARU(155)**2)**2*(1.-SQM4/SH)
        FACQSB=COMFAC*0.25*(SH/PARU(155)**2)**2*(1.-SQM4/SH)*
     &  (1.+SQM4/SH)*(1.+CTH)*(1.+((SH-SQM4)/(SH+SQM4))*CTH)
C...Propagators: as simulated in PYOFSH and as desired.
        GMMQ=PMAS(KFQSTR,1)*PMAS(KFQSTR,2)
        HBW4=GMMQ/((SQM4-PMAS(KFQSTR,1)**2)**2+GMMQ**2)
        CALL PYWIDT(KFQSTR,SQM4,WDTP,WDTE)
        GMMQC=SQM4*WDTP(0)
        HBW4C=GMMQC/((SQM4-PMAS(KFQSTR,1)**2)**2+GMMQC**2)
        FACQSA=FACQSA*HBW4C/HBW4
        FACQSB=FACQSB*HBW4C/HBW4
        DO 1510 I=MMIN1,MMAX1
        IA=IABS(I)
        IF(I.EQ.0.OR.IA.GT.6.OR.KFAC(1,I).EQ.0) GOTO 1510
        DO 1500 J=MMIN2,MMAX2
        JA=IABS(J)
        IF(J.EQ.0.OR.JA.GT.6.OR.KFAC(2,J).EQ.0) GOTO 1500
        IF(IA.EQ.KFQEXC.AND.I.EQ.J) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          SIGH(NCHN)=(4./3.)*FACQSA
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=2
          SIGH(NCHN)=(4./3.)*FACQSA
        ELSEIF((IA.EQ.KFQEXC.OR.JA.EQ.KFQEXC).AND.I*J.GT.0) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          IF(JA.EQ.KFQEXC) ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQSA
        ELSEIF(IA.EQ.KFQEXC.AND.I.EQ.-J) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          SIGH(NCHN)=(8./3.)*FACQSB
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=2
          SIGH(NCHN)=(8./3.)*FACQSB
        ELSEIF(I.EQ.-J) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          SIGH(NCHN)=FACQSB
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQSB
        ELSEIF(IA.EQ.KFQEXC.OR.JA.EQ.KFQEXC) THEN
          NCHN=NCHN+1
          ISIG(NCHN,1)=I
          ISIG(NCHN,2)=J
          ISIG(NCHN,3)=1
          IF(JA.EQ.KFQEXC) ISIG(NCHN,3)=2
          SIGH(NCHN)=FACQSB
        ENDIF
 1500   CONTINUE
 1510   CONTINUE
 
      ENDIF
      ENDIF
 
C...Multiply with structure functions.
      IF(ISUB.LE.90.OR.ISUB.GE.96) THEN
        DO 1520 ICHN=1,NCHN
        IF(MINT(45).GE.2) THEN
          KFL1=ISIG(ICHN,1)
          SIGH(ICHN)=SIGH(ICHN)*XSFX(1,KFL1)
        ENDIF
        IF(MINT(46).GE.2) THEN
          KFL2=ISIG(ICHN,2)
          SIGH(ICHN)=SIGH(ICHN)*XSFX(2,KFL2)
        ENDIF
        SIGS=SIGS+SIGH(ICHN)
 1520   CONTINUE
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSTFUAV(KF,X,Q2,XPQ)
 
C...Gives electron, photon, pi+, neutron, proton and hyperon
C...structure functions according to a few different parametrizations.
C...Note that what is coded is x times the probability distribution,
C...i.e. xq(x,Q2) etc.
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT8/XPVMD(-6:6),XPANL(-6:6),XPANH(-6:6),XPBEH(-6:6),
     &XPDIR(-6:6)
      REAL XPVMD,XPANL,XPANH,XPBEH,XPDIR
      SAVE /PYINT8/
      DIMENSION XPQ(-25:25),XPEL(-25:25),XPGA(-6:6),XPPI(-6:6),
     &XPPR(-6:6)
 
C...Interface to PDFLIB.
      COMMON/W50513/XMIN,XMAX,Q2MIN,Q2MAX
      SAVE /W50513/
      DOUBLE PRECISION XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU,
     &VALUE(20),XMIN,XMAX,Q2MIN,Q2MAX
      CHARACTER*20 PARM(20)
      DATA VALUE/20*0D0/,PARM/20*' '/
 
C...Data related to Schuler-Sjostrand photon distributions.
      DATA ALAMGA/0.2/, PMCGA/1.3/, PMBGA/4.6/
 
C...Reset structure functions.
      MINT(92)=0
      DO 100 KFL=-25,25
      XPQ(KFL)=0.
  100 CONTINUE
 
C...Check x and particle species.
      IF(X.LE.0..OR.X.GE.1.) THEN
        WRITE(MSTU(11),5000) X
        RETURN
      ENDIF
      KFA=IABS(KF)
      IF(KFA.NE.11.AND.KFA.NE.22.AND.KFA.NE.211.AND.KFA.NE.2112.AND.
     &KFA.NE.2212.AND.KFA.NE.3122.AND.KFA.NE.3112.AND.KFA.NE.3212
     &.AND.KFA.NE.3222.AND.KFA.NE.3312.AND.KFA.NE.3322.AND.
     &KFA.NE.3334.AND.KFA.NE.111) THEN
        WRITE(MSTU(11),5100) KF
        RETURN
      ENDIF
 
C...Electron structure function call.
      IF(KFA.EQ.11) THEN
        CALL PYSTEL(X,Q2,XPEL)
        DO 110 KFL=-25,25
        XPQ(KFL)=XPEL(KFL)
  110   CONTINUE
 
C...Photon structure function call (VDM+anomalous).
      ELSEIF(KFA.EQ.22.AND.MINT(109).LE.1) THEN
        IF(MSTP(56).EQ.1.AND.MSTP(55).EQ.1) THEN
          CALL PYSTGA(X,Q2,XPGA)
          DO 120 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  120     CONTINUE
        ELSEIF(MSTP(56).EQ.1.AND.MSTP(55).GE.5.AND.MSTP(55).LE.8) THEN
          Q2MX=Q2
          P2MX=0.36
          IF(MSTP(55).GE.7) P2MX=4.0
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGGAM(MSTP(55)-4,X,Q2MX,0.,F2GAM,XPGA)
          DO 130 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  130     CONTINUE
          VINT(231)=P2MX
        ELSEIF(MSTP(56).EQ.1.AND.MSTP(55).GE.9.AND.MSTP(55).LE.12) THEN
          Q2MX=Q2
          P2MX=0.36
          IF(MSTP(55).GE.11) P2MX=4.0
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGGAM(MSTP(55)-8,X,Q2MX,0.,F2GAM,XPGA)
          DO 140 KFL=-6,6
          XPQ(KFL)=XPVMD(KFL)+XPANL(KFL)+XPBEH(KFL)+XPDIR(KFL)
  140     CONTINUE
          VINT(231)=P2MX
        ELSEIF(MSTP(56).EQ.2) THEN
C...Call PDFLIB structure functions.
          PARM(1)='NPTYPE'
          VALUE(1)=3
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(55)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(55),1000)
          IF(MINT(93).NE.3000000+MSTP(55)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=3000000+MSTP(55)
          ENDIF
          XX=X
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          VINT(231)=Q2MIN
          XPQ(0)=GLU
          XPQ(1)=DNV
          XPQ(-1)=DNV
          XPQ(2)=UPV
          XPQ(-2)=UPV
          XPQ(3)=STR
          XPQ(-3)=STR
          XPQ(4)=CHM
          XPQ(-4)=CHM
          XPQ(5)=BOT
          XPQ(-5)=BOT
          XPQ(6)=TOP
          XPQ(-6)=TOP
        ELSE
          WRITE(MSTU(11),5200) KF,MSTP(56),MSTP(55)
        ENDIF
 
C...Pion/gammaVDM structure function call.
      ELSEIF(KFA.EQ.211.OR.KFA.EQ.111.OR.(KFA.EQ.22.AND.
     &MINT(109).EQ.2)) THEN
        IF(KFA.EQ.22.AND.MSTP(56).EQ.1.AND.MSTP(55).GE.5.AND.
     &  MSTP(55).LE.12) THEN
          ISET=1+MOD(MSTP(55)-1,4)
          Q2MX=Q2
          P2MX=0.36
          IF(ISET.GE.3) P2MX=4.0
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGVMD(ISET,2,X,Q2MX,P2MX,ALAMGA,XPGA)
          DO 150 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  150     CONTINUE
          VINT(231)=P2MX
        ELSEIF(MSTP(54).EQ.1.AND.MSTP(53).GE.1.AND.MSTP(53).LE.3) THEN
          CALL PYSTPI(X,Q2,XPPI)
          DO 160 KFL=-6,6
          XPQ(KFL)=XPPI(KFL)
  160     CONTINUE
        ELSEIF(MSTP(54).EQ.2) THEN
C...Call PDFLIB structure functions.
          PARM(1)='NPTYPE'
          VALUE(1)=2
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(53)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(53),1000)
          IF(MINT(93).NE.2000000+MSTP(53)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=2000000+MSTP(53)
          ENDIF
          XX=X
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          VINT(231)=Q2MIN
          XPQ(0)=GLU
          XPQ(1)=DSEA
          XPQ(-1)=UPV+DSEA
          XPQ(2)=UPV+USEA
          XPQ(-2)=USEA
          XPQ(3)=STR
          XPQ(-3)=STR
          XPQ(4)=CHM
          XPQ(-4)=CHM
          XPQ(5)=BOT
          XPQ(-5)=BOT
          XPQ(6)=TOP
          XPQ(-6)=TOP
        ELSE
          WRITE(MSTU(11),5200) KF,MSTP(54),MSTP(53)
        ENDIF
 
C...Anomalous photon structure function call.
      ELSEIF(KFA.EQ.22.AND.MINT(109).EQ.3) THEN
        Q2MX=Q2
        P2MX=PARP(15)**2
        IF(MSTP(56).EQ.1.AND.MSTP(55).LE.8) THEN
          IF(MSTP(55).EQ.5.OR.MSTP(55).EQ.6) P2MX=0.36
          IF(MSTP(55).EQ.7.OR.MSTP(55).EQ.8) P2MX=4.0
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGANO(0,X,Q2MX,P2MX,ALAMGA,XPGA)
          DO 170 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  170     CONTINUE
          VINT(231)=P2MX
        ELSEIF(MSTP(56).EQ.1) THEN
          IF(MSTP(55).EQ.9.OR.MSTP(55).EQ.10) P2MX=0.36
          IF(MSTP(55).EQ.11.OR.MSTP(55).EQ.12) P2MX=4.0
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGGAM(MSTP(55)-8,X,Q2MX,0.,F2GM,XPGA)
          DO 180 KFL=-6,6
          XPQ(KFL)=MAX(0.,XPANL(KFL)+XPBEH(KFL)+XPDIR(KFL))
  180     CONTINUE
          VINT(231)=P2MX
        ELSEIF(MSTP(56).EQ.2) THEN
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGANO(0,X,Q2MX,P2MX,ALAMGA,XPGA)
          DO 185 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  185     CONTINUE
          VINT(231)=P2MX
        ELSEIF(MSTP(55).GE.1.AND.MSTP(55).LE.5) THEN
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGVMD(0,MSTP(55),X,Q2MX,P2MX,PARP(1),XPGA)
          DO 190 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  190     CONTINUE
          VINT(231)=P2MX
        ELSE
  200     RKF=11.*RLU(0)
          KFR=1
          IF(RKF.GT.1.) KFR=2
          IF(RKF.GT.5.) KFR=3
          IF(RKF.GT.6.) KFR=4
          IF(RKF.GT.10.) KFR=5
          IF(KFR.EQ.4.AND.Q2.LT.PMCGA**2) GOTO 200
          IF(KFR.EQ.5.AND.Q2.LT.PMBGA**2) GOTO 200
          IF(MSTP(57).EQ.0) Q2MX=P2MX
          CALL PYGVMD(0,KFR,X,Q2MX,P2MX,PARP(1),XPGA)
          DO 210 KFL=-6,6
          XPQ(KFL)=XPGA(KFL)
  210     CONTINUE
          VINT(231)=P2MX
        ENDIF
 
C...Proton structure function call.
      ELSE
        IF(MSTP(52).EQ.1.AND.MSTP(51).GE.1.AND.MSTP(51).LE.11) THEN
          CALL PYSTPR(X,Q2,XPPR)
          DO 220 KFL=-6,6
          XPQ(KFL)=XPPR(KFL)
  220     CONTINUE
        ELSEIF(MSTP(52).EQ.2) THEN
C...Call PDFLIB structure functions.
          PARM(1)='NPTYPE'
          VALUE(1)=1
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(51)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(51),1000)
          IF(MINT(93).NE.1000000+MSTP(51)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=1000000+MSTP(51)
          ENDIF
          XX=X
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          VINT(231)=Q2MIN
          XPQ(0)=GLU
          XPQ(1)=DNV+DSEA
          XPQ(-1)=DSEA
          XPQ(2)=UPV+USEA
          XPQ(-2)=USEA
          XPQ(3)=STR
          XPQ(-3)=STR
          XPQ(4)=CHM
          XPQ(-4)=CHM
          XPQ(5)=BOT
          XPQ(-5)=BOT
          XPQ(6)=TOP
          XPQ(-6)=TOP
        ELSE
          WRITE(MSTU(11),5200) KF,MSTP(52),MSTP(51)
        ENDIF
      ENDIF
 
C...Isospin average for pi0/gammaVDM.
      IF(KFA.EQ.111.OR.(KFA.EQ.22.AND.MINT(109).EQ.2)) THEN
        IF(KFA.EQ.22.AND.MSTP(55).GE.5.AND.MSTP(55).LE.12) THEN
          XPV=XPQ(2)-XPQ(1)
          XPQ(2)=XPQ(1)
          XPQ(-2)=XPQ(-1)
        ELSE
          XPS=0.5*(XPQ(1)+XPQ(-2))
          XPV=0.5*(XPQ(2)+XPQ(-1))-XPS
          XPQ(2)=XPS
          XPQ(-1)=XPS
        ENDIF
        IF(KFA.EQ.22.AND.MINT(105).LE.223) THEN
          XPQ(1)=XPQ(1)+0.2*XPV
          XPQ(-1)=XPQ(-1)+0.2*XPV
          XPQ(2)=XPQ(2)+0.8*XPV
          XPQ(-2)=XPQ(-2)+0.8*XPV
        ELSEIF(KFA.EQ.22.AND.MINT(105).EQ.333) THEN
          XPQ(3)=XPQ(3)+XPV
          XPQ(-3)=XPQ(-3)+XPV
        ELSEIF(KFA.EQ.22.AND.MINT(105).EQ.443) THEN
          XPQ(4)=XPQ(4)+XPV
          XPQ(-4)=XPQ(-4)+XPV
          IF(MSTP(55).GE.9) THEN
            DO 230 KFL=-6,6
            XPQ(KFL)=0.
  230       CONTINUE
          ENDIF
        ELSE
          XPQ(1)=XPQ(1)+0.5*XPV
          XPQ(-1)=XPQ(-1)+0.5*XPV
          XPQ(2)=XPQ(2)+0.5*XPV
          XPQ(-2)=XPQ(-2)+0.5*XPV
        ENDIF
 
C...Rescale for gammaVDM by effective gamma -> rho coupling.
        IF(KFA.EQ.22.AND.MINT(109).EQ.2) THEN
          DO 240 KFL=-6,6
          XPQ(KFL)=VINT(281)*XPQ(KFL)
  240     CONTINUE
          VINT(232)=VINT(281)*XPV
        ENDIF
 
C...Isospin conjugation for neutron.
      ELSEIF(KFA.EQ.2112) THEN
        XPS=XPQ(1)
        XPQ(1)=XPQ(2)
        XPQ(2)=XPS
        XPS=XPQ(-1)
        XPQ(-1)=XPQ(-2)
        XPQ(-2)=XPS
 
C...Simple recipes for hyperon (average valence structure function).
      ELSEIF(KFA.EQ.3122.OR.KFA.EQ.3112.OR.KFA.EQ.3212.OR.KFA.EQ.3222
     &.OR.KFA.EQ.3312.OR.KFA.EQ.3322.OR.KFA.EQ.3334) THEN
        XPVAL=(XPQ(1)+XPQ(2)-XPQ(-1)-XPQ(-2))/3.
        XPSEA=0.5*(XPQ(-1)+XPQ(-2))
        XPQ(1)=XPSEA
        XPQ(2)=XPSEA
        XPQ(-1)=XPSEA
        XPQ(-2)=XPSEA
        XPQ(KFA/1000)=XPQ(KFA/1000)+XPVAL
        XPQ(MOD(KFA/100,10))=XPQ(MOD(KFA/100,10))+XPVAL
        XPQ(MOD(KFA/10,10))=XPQ(MOD(KFA/10,10))+XPVAL
      ENDIF
 
C...Charge conjugation for antiparticle.
      IF(KF.LT.0) THEN
        DO 250 KFL=1,25
        IF(KFL.EQ.21.OR.KFL.EQ.22.OR.KFL.EQ.23.OR.KFL.EQ.25) GOTO 250
        XPS=XPQ(KFL)
        XPQ(KFL)=XPQ(-KFL)
        XPQ(-KFL)=XPS
  250   CONTINUE
      ENDIF
 
C...Allow gluon also in position 21.
      XPQ(21)=XPQ(0)
 
C...Check positivity and reset above maximum allowed flavour.
      DO 260 KFL=-25,25
      XPQ(KFL)=MAX(0.,XPQ(KFL))
      IF(IABS(KFL).GT.MSTP(58).AND.IABS(KFL).LE.8) XPQ(KFL)=0.
  260 CONTINUE
 
C...Formats for error printouts.
 5000 FORMAT(' Error: x value outside physical range; x =',1P,E12.3)
 5100 FORMAT(' Error: illegal particle code for structure function;',
     &' KF =',I5)
 5200 FORMAT(' Error: unknown structure function; KF, library, set =',
     &3I5)
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSTFL(KF,X,Q2,XPQ)
 
C...Give proton structure function at small x and/or Q^2 according to
C...correct limiting behaviour.
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION XPQ(-25:25),XPA(-25:25),XPB(-25:25),WTSB(-3:3)
      DATA RMR/0.92/,RMP/0.38/,WTSB/0.5,1.,1.,5.,1.,1.,0.5/
 
C...Send everything but protons/neutrons/VMD pions directly to PYSTFU.
      MINT(92)=0
      KFA=IABS(KF)
      IACC=0
      IF((KFA.EQ.2212.OR.KFA.EQ.2112).AND.MSTP(57).GE.2) IACC=1
      IF(KFA.EQ.211.AND.MSTP(57).GE.3) IACC=1
      IF(KFA.EQ.22.AND.MINT(109).EQ.2.AND.MSTP(57).GE.3) IACC=1
      IF(IACC.EQ.0) THEN
        CALL PYSTFU(KF,X,Q2,XPQ)
        RETURN
      ENDIF
 
C...Reset. Check x.
      DO 100 KFL=-25,25
      XPQ(KFL)=0.
  100 CONTINUE
      IF(X.LE.0..OR.X.GE.1.) THEN
        WRITE(MSTU(11),5000) X
        RETURN
      ENDIF
 
C...Define valence content.
      KFC=KF
      NV1=2
      NV2=1
      IF(KF.EQ.2212) THEN
        KFV1=2
        KFV2=1
      ELSEIF(KF.EQ.-2212) THEN
        KFV1=-2
        KFV2=-1
      ELSEIF(KF.EQ.2112) THEN
        KFV1=1
        KFV2=2
      ELSEIF(KF.EQ.-2112) THEN
        KFV1=-1
        KFV2=-2
      ELSEIF(KF.EQ.211) THEN
        NV1=1
        KFV1=2
        KFV2=-1
      ELSEIF(KF.EQ.-211) THEN
        NV1=1
        KFV1=-2
        KFV2=1
      ELSEIF(MINT(105).LE.223) THEN
        KFV1=1
        WTV1=0.2
        KFV2=2
        WTV2=0.8
      ELSEIF(MINT(105).EQ.333) THEN
        KFV1=3
        WTV1=1.0
        KFV2=1
        WTV2=0.0
      ELSEIF(MINT(105).EQ.443) THEN
        KFV1=4
        WTV1=1.0
        KFV2=1
        WTV2=0.0
      ENDIF
 
C...Do naive evaluation and find min Q^2, boundary Q^2 and x_0.
      CALL PYSTFU(KFC,X,Q2,XPA)
      Q2MN=MAX(3.,VINT(231))
      Q2B=2.+0.052**2*EXP(3.56*SQRT(MAX(0.,-LOG(3.*X))))
      XMN=EXP(-(LOG((Q2MN-2.)/0.052**2)/3.56)**2)/3.
 
C...Large Q2 and large x: naive call is enough.
      IF(Q2.GT.Q2MN.AND.Q2.GT.Q2B) THEN
        DO 110 KFL=-25,25
        XPQ(KFL)=XPA(KFL)
  110   CONTINUE
        MINT(92)=1
 
C...Small Q2 and large x: dampen boundary value.
      ELSEIF(X.GT.XMN) THEN
 
C...Evaluate at boundary and define dampening factors.
        CALL PYSTFU(KFC,X,Q2MN,XPA)
        FV=(Q2*(Q2MN+RMR)/(Q2MN*(Q2+RMR)))**(0.55*(1.-X)/(1.-XMN))
        FS=(Q2*(Q2MN+RMP)/(Q2MN*(Q2+RMP)))**1.08
 
C...Separate valence and sea parts of structure function.
        IF(KFA.NE.22) THEN
          XFV1=XPA(KFV1)-XPA(-KFV1)
          XPA(KFV1)=XPA(-KFV1)
          XFV2=XPA(KFV2)-XPA(-KFV2)
          XPA(KFV2)=XPA(-KFV2)
        ELSE
          XPA(KFV1)=XPA(KFV1)-WTV1*VINT(232)
          XPA(-KFV1)=XPA(-KFV1)-WTV1*VINT(232)
          XPA(KFV2)=XPA(KFV2)-WTV2*VINT(232)
          XPA(-KFV2)=XPA(-KFV2)-WTV2*VINT(232)
        ENDIF
 
C...Dampen valence and sea separately. Put back together.
        DO 120 KFL=-25,25
        XPQ(KFL)=FS*XPA(KFL)
  120   CONTINUE
        IF(KFA.NE.22) THEN
          XPQ(KFV1)=XPQ(KFV1)+FV*XFV1
          XPQ(KFV2)=XPQ(KFV2)+FV*XFV2
        ELSE
          XPQ(KFV1)=XPQ(KFV1)+FV*WTV1*VINT(232)
          XPQ(-KFV1)=XPQ(-KFV1)+FV*WTV1*VINT(232)
          XPQ(KFV2)=XPQ(KFV2)+FV*WTV2*VINT(232)
          XPQ(-KFV2)=XPQ(-KFV2)+FV*WTV2*VINT(232)          
        ENDIF
        MINT(92)=2
 
C...Large Q2 and small x: interpolate behaviour.
      ELSEIF(Q2.GT.Q2MN) THEN
 
C...Evaluate at extremes and define coefficients for interpolation.
        CALL PYSTFU(KFC,XMN,Q2MN,XPA)
        VI232A=VINT(232)
        CALL PYSTFU(KFC,X,Q2B,XPB)
        VI232B=VINT(232) 
        FLA=LOG(Q2B/Q2)/LOG(Q2B/Q2MN)
        FVA=(X/XMN)**0.45*FLA
        FSA=(X/XMN)**(-0.08)*FLA
        FB=1.-FLA
 
C...Separate valence and sea parts of structure function.
        IF(KFA.NE.22) THEN
          XFVA1=XPA(KFV1)-XPA(-KFV1)
          XPA(KFV1)=XPA(-KFV1)
          XFVA2=XPA(KFV2)-XPA(-KFV2)
          XPA(KFV2)=XPA(-KFV2)
          XFVB1=XPB(KFV1)-XPB(-KFV1)
          XPB(KFV1)=XPB(-KFV1)
          XFVB2=XPB(KFV2)-XPB(-KFV2)
          XPB(KFV2)=XPB(-KFV2)
        ELSE
          XPA(KFV1)=XPA(KFV1)-WTV1*VI232A
          XPA(-KFV1)=XPA(-KFV1)-WTV1*VI232A
          XPA(KFV2)=XPA(KFV2)-WTV2*VI232A
          XPA(-KFV2)=XPA(-KFV2)-WTV2*VI232A
          XPB(KFV1)=XPB(KFV1)-WTV1*VI232B
          XPB(-KFV1)=XPB(-KFV1)-WTV1*VI232B
          XPB(KFV2)=XPB(KFV2)-WTV2*VI232B
          XPB(-KFV2)=XPB(-KFV2)-WTV2*VI232B
        ENDIF
 
C...Interpolate for valence and sea. Put back together.
        DO 130 KFL=-25,25
        XPQ(KFL)=FSA*XPA(KFL)+FB*XPB(KFL)
  130   CONTINUE
        IF(KFA.NE.22) THEN
          XPQ(KFV1)=XPQ(KFV1)+(FVA*XFVA1+FB*XFVB1)
          XPQ(KFV2)=XPQ(KFV2)+(FVA*XFVA2+FB*XFVB2)
        ELSE
          XPQ(KFV1)=XPQ(KFV1)+WTV1*(FVA*VI232A+FB*VI232B)
          XPQ(-KFV1)=XPQ(-KFV1)+WTV1*(FVA*VI232A+FB*VI232B)
          XPQ(KFV2)=XPQ(KFV2)+WTV2*(FVA*VI232A+FB*VI232B)
          XPQ(-KFV2)=XPQ(-KFV2)+WTV2*(FVA*VI232A+FB*VI232B)          
        ENDIF
        MINT(92)=3
 
C...Small Q2 and small x: dampen boundary value and add term.
      ELSE
 
C...Evaluate at boundary and define dampening factors.
        CALL PYSTFU(KFC,XMN,Q2MN,XPA)
        FB=(XMN-X)*(Q2MN-Q2)/(XMN*Q2MN)
        FA=1.-FB
        FVC=(X/XMN)**0.45*(Q2/(Q2+RMR))**0.55
        FVA=FVC*FA*((Q2MN+RMR)/Q2MN)**0.55
        FVB=FVC*FB*1.10*XMN**0.45*0.11
        FSC=(X/XMN)**(-0.08)*(Q2/(Q2+RMP))**1.08
        FSA=FSC*FA*((Q2MN+RMP)/Q2MN)**1.08
        FSB=FSC*FB*0.21*XMN**(-0.08)*0.21
 
C...Separate valence and sea parts of structure function.
        IF(KFA.NE.22) THEN
          XFV1=XPA(KFV1)-XPA(-KFV1)
          XPA(KFV1)=XPA(-KFV1)
          XFV2=XPA(KFV2)-XPA(-KFV2)
          XPA(KFV2)=XPA(-KFV2)
        ELSE
          XPA(KFV1)=XPA(KFV1)-WTV1*VINT(232)
          XPA(-KFV1)=XPA(-KFV1)-WTV1*VINT(232)
          XPA(KFV2)=XPA(KFV2)-WTV2*VINT(232)
          XPA(-KFV2)=XPA(-KFV2)-WTV2*VINT(232)
        ENDIF
 
C...Dampen valence and sea separately. Add constant terms.
C...Put back together.
        DO 140 KFL=-25,25
        XPQ(KFL)=FSA*XPA(KFL)
  140   CONTINUE
        IF(KFA.NE.22) THEN
          DO 150 KFL=-3,3
          XPQ(KFL)=XPQ(KFL)+FSB*WTSB(KFL)
  150     CONTINUE
          XPQ(KFV1)=XPQ(KFV1)+(FVA*XFV1+FVB*NV1)
          XPQ(KFV2)=XPQ(KFV2)+(FVA*XFV2+FVB*NV2)
        ELSE
          DO 160 KFL=-3,3
          XPQ(KFL)=XPQ(KFL)+VINT(281)*FSB*WTSB(KFL)
  160     CONTINUE
          XPQ(KFV1)=XPQ(KFV1)+WTV1*(FVA*VINT(232)+FVB*VINT(281))
          XPQ(-KFV1)=XPQ(-KFV1)+WTV1*(FVA*VINT(232)+FVB*VINT(281))
          XPQ(KFV2)=XPQ(KFV2)+WTV2*(FVA*VINT(232)+FVB*VINT(281))
          XPQ(-KFV2)=XPQ(-KFV2)+WTV2*(FVA*VINT(232)+FVB*VINT(281))          
        ENDIF
        XPQ(21)=XPQ(0)
        MINT(92)=4
      ENDIF
 
C...Format for error printout.
 5000 FORMAT(' Error: x value outside physical range; x =',1P,E12.3)
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSTEL(X,Q2,XPEL)
 
C...Gives electron structure function.
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION XPEL(-25:25),XPGA(-6:6),SXP(0:6)
 
C...Interface to PDFLIB.
      COMMON/W50513/XMIN,XMAX,Q2MIN,Q2MAX
      SAVE /W50513/
      DOUBLE PRECISION XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU,
     &VALUE(20),XMIN,XMAX,Q2MIN,Q2MAX
      CHARACTER*20 PARM(20)
      DATA VALUE/20*0D0/,PARM/20*' '/
 
C...Some common constants.
      DO 100 KFL=-25,25
      XPEL(KFL)=0.
  100 CONTINUE
      AEM=PARU(101)
      PME=PMAS(11,1)
      XL=LOG(MAX(1E-10,X))
      X1L=LOG(MAX(1E-10,1.-X))
      HLE=LOG(MAX(3.,Q2/PME**2))
      HBE2=(AEM/PARU(1))*(HLE-1.)
 
C...Electron inside electron, see R. Kleiss et al., in Z physics at
C...LEP 1, CERN 89-08, p. 34
      IF(MSTP(59).LE.1) THEN
        HDE=1.+(AEM/PARU(1))*(1.5*HLE+1.289868)+(AEM/PARU(1))**2*
     &  (-2.164868*HLE**2+9.840808*HLE-10.130464)
        HEE=HBE2*(1.-X)**(HBE2-1.)*SQRT(MAX(0.,HDE))-
     &  0.5*HBE2*(1.+X)+HBE2**2/8.*((1.+X)*(-4.*X1L+3.*XL)-
     &  4.*XL/(1.-X)-5.-X)
      ELSE
        HEE=HBE2*(1.-X)**(HBE2-1.)*EXP(0.172784*HBE2)/PYGAMM(1.+HBE2)-
     &  0.5*HBE2*(1.+X)+HBE2**2/8.*((1.+X)*(-4.*X1L+3.*XL)-
     &  4.*XL/(1.-X)-5.-X)        
      ENDIF
      IF(X.GT.0.9999.AND.X.LE.0.999999) THEN
        HEE=HEE*100.**HBE2/(100.**HBE2-1.)
      ELSEIF(X.GT.0.999999) THEN
        HEE=0.
      ENDIF
      XPEL(11)=X*HEE
 
C...Photon and (transverse) W- inside electron.
      AEMP=ULALEM(PME*SQRT(MAX(0.,Q2)))/PARU(2)
      IF(MSTP(13).LE.1) THEN
        HLG=HLE
      ELSE
        HLG=LOG(MAX(1.,(PARP(13)/PME**2)*(1.-X)/X**2))
      ENDIF
      XPEL(22)=AEMP*HLG*(1.+(1.-X)**2)
      HLW=LOG(1.+Q2/PMAS(24,1)**2)/(4.*PARU(102))
      XPEL(-24)=AEMP*HLW*(1.+(1.-X)**2)
 
C...Electron or positron inside photon inside electron.
      IF(MSTP(12).EQ.1) THEN
        XFSEA=0.5*(AEMP*(HLE-1.))**2*(4./3.+X-X**2-4.*X**3/3.+
     &  2.*X*(1.+X)*XL)
        XPEL(11)=XPEL(11)+XFSEA
        XPEL(-11)=XFSEA
 
C...Initialize PDFLIB photon structure functions.
        IF(MSTP(56).EQ.2) THEN
          PARM(1)='NPTYPE'
          VALUE(1)=3
          PARM(2)='NGROUP'
          VALUE(2)=MSTP(55)/1000
          PARM(3)='NSET'
          VALUE(3)=MOD(MSTP(55),1000)
          IF(MINT(93).NE.3000000+MSTP(55)) THEN
            CALL PDFSET(PARM,VALUE)
            MINT(93)=3000000+MSTP(55)
          ENDIF
        ENDIF
 
C...Quarks and gluons inside photon inside electron:
C...numerical convolution required.
        DO 110 KFL=0,6
        SXP(KFL)=0.
  110   CONTINUE
        SUMXPP=0.
        ITER=-1
  120   ITER=ITER+1
        SUMXP=SUMXPP
        NSTP=2**(ITER-1)
        IF(ITER.EQ.0) NSTP=2
        DO 130 KFL=0,6
        SXP(KFL)=0.5*SXP(KFL)
  130   CONTINUE
        WTSTP=0.5/NSTP
        IF(ITER.EQ.0) WTSTP=0.5
C...Pick grid of x_{gamma} values logarithmically even.
        DO 150 ISTP=1,NSTP
        IF(ITER.EQ.0) THEN
          XLE=XL*(ISTP-1)
        ELSE
          XLE=XL*(ISTP-0.5)/NSTP
        ENDIF
        XE=MIN(0.999999,EXP(XLE))
        XG=MIN(0.999999,X/XE)
C...Evaluate photon inside electron structure function for convolution.
        XPGP=1.+(1.-XE)**2
        IF(MSTP(13).LE.1) THEN
          XPGP=XPGP*HLE
        ELSE
          XPGP=XPGP*LOG(MAX(1.,(PARP(13)/PME**2)*(1.-XE)/XE**2))
        ENDIF
C...Evaluate photon structure functions for convolution.
        IF(MSTP(56).EQ.1) THEN
          CALL PYSTGA(XG,Q2,XPGA)
          DO 140 KFL=0,5
          SXP(KFL)=SXP(KFL)+WTSTP*XPGP*XPGA(KFL)
  140     CONTINUE
        ELSEIF(MSTP(56).EQ.2) THEN
C...Call PDFLIB structure functions.
          XX=XG
          QQ=SQRT(MAX(0.,SNGL(Q2MIN),Q2))
          IF(MSTP(57).EQ.0) QQ=SQRT(Q2MIN)
          CALL STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
          SXP(0)=SXP(0)+WTSTP*XPGP*GLU
          SXP(1)=SXP(1)+WTSTP*XPGP*DNV
          SXP(2)=SXP(2)+WTSTP*XPGP*UPV
          SXP(3)=SXP(3)+WTSTP*XPGP*STR
          SXP(4)=SXP(4)+WTSTP*XPGP*CHM
          SXP(5)=SXP(5)+WTSTP*XPGP*BOT
          SXP(6)=SXP(6)+WTSTP*XPGP*TOP
        ENDIF
  150   CONTINUE
        SUMXPP=SXP(0)+2.*SXP(1)+2.*SXP(2)
        IF(ITER.LE.2.OR.(ITER.LE.7.AND.ABS(SUMXPP-SUMXP).GT.
     &  PARP(14)*(SUMXPP+SUMXP))) GOTO 120
 
C...Put convolution into output arrays.
        FCONV=AEMP*(-XL)
        XPEL(0)=FCONV*SXP(0)
        DO 160 KFL=1,6
        XPEL(KFL)=FCONV*SXP(KFL)
        XPEL(-KFL)=XPEL(KFL)
  160   CONTINUE
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSTGA(X,Q2,XPGA)
 
C...Gives photon structure function.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION XPGA(-6:6),DGAG(4,3),DGBG(4,3),DGCG(4,3),DGAN(4,3),
     &DGBN(4,3),DGCN(4,3),DGDN(4,3),DGEN(4,3),DGAS(4,3),DGBS(4,3),
     &DGCS(4,3),DGDS(4,3),DGES(4,3)
 
C...The following data lines are coefficients needed in the
C...Drees and Grassie photon structure function parametrization.
      DATA DGAG/-.207E0,.6158E0,1.074E0,0.E0,.8926E-2,.6594E0,
     &.4766E0,.1975E-1,.03197E0,1.018E0,.2461E0,.2707E-1/
      DATA DGBG/-.1987E0,.6257E0,8.352E0,5.024E0,.5085E-1,.2774E0,
     &-.3906E0,-.3212E0,-.618E-2,.9476E0,-.6094E0,-.1067E-1/
      DATA DGCG/5.119E0,-.2752E0,-6.993E0,2.298E0,-.2313E0,.1382E0,
     &6.542E0,.5162E0,-.1216E0,.9047E0,2.653E0,.2003E-2/
      DATA DGAN/2.285E0,-.1526E-1,1330.E0,4.219E0,-.3711E0,1.061E0,
     &4.758E0,-.1503E-1,15.8E0,-.9464E0,-.5E0,-.2118E0/
      DATA DGBN/6.073E0,-.8132E0,-41.31E0,3.165E0,-.1717E0,.7815E0,
     &1.535E0,.7067E-2,2.742E0,-.7332E0,.7148E0,3.287E0/
      DATA DGCN/-.4202E0,.1778E-1,.9216E0,.18E0,.8766E-1,.2197E-1,
     &.1096E0,.204E0,.2917E-1,.4657E-1,.1785E0,.4811E-1/
      DATA DGDN/-.8083E-1,.6346E0,1.208E0,.203E0,-.8915E0,.2857E0,
     &2.973E0,.1185E0,-.342E-1,.7196E0,.7338E0,.8139E-1/
      DATA DGEN/.5526E-1,1.136E0,.9512E0,.1163E-1,-.1816E0,.5866E0,
     &2.421E0,.4059E0,-.2302E-1,.9229E0,.5873E0,-.79E-4/
      DATA DGAS/16.69E0,-.7916E0,1099.E0,4.428E0,-.1207E0,1.071E0,
     &1.977E0,-.8625E-2,6.734E0,-1.008E0,-.8594E-1,.7625E-1/
      DATA DGBS/.176E0,.4794E-1,1.047E0,.25E-1,25.E0,-1.648E0,
     &-.1563E-1,6.438E0,59.88E0,-2.983E0,4.48E0,.9686E0/
      DATA DGCS/-.208E-1,.3386E-2,4.853E0,.8404E0,-.123E-1,1.162E0,
     &.4824E0,-.11E-1,-.3226E-2,.8432E0,.3616E0,.1383E-2/
      DATA DGDS/-.1685E-1,1.353E0,1.426E0,1.239E0,-.9194E-1,.7912E0,
     &.6397E0,2.327E0,-.3321E-1,.9475E0,-.3198E0,.2132E-1/
      DATA DGES/-.1986E0,1.1E0,1.136E0,-.2779E0,.2015E-1,.9869E0,
     &-.7036E-1,.1694E-1,.1059E0,.6954E0,-.6663E0,.3683E0/
 
C...Photon structure function from Drees and Grassie.
C...Allowed variable range: 1 GeV^2 < Q^2 < 10000 GeV^2.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
      VINT(231)=1.
      IF(MSTP(57).LE.0) THEN
        T=LOG(1./0.16)
      ELSE
        T=LOG(MIN(1E4,MAX(1.,Q2))/0.16)
      ENDIF
      X1=1.-X
      NF=3
      IF(Q2.GT.25.) NF=4
      IF(Q2.GT.300.) NF=5
      NFE=NF-2
      AEM=PARU(101)
 
C...Evaluate gluon content.
      DGA=DGAG(1,NFE)*T**DGAG(2,NFE)+DGAG(3,NFE)*T**(-DGAG(4,NFE))
      DGB=DGBG(1,NFE)*T**DGBG(2,NFE)+DGBG(3,NFE)*T**(-DGBG(4,NFE))
      DGC=DGCG(1,NFE)*T**DGCG(2,NFE)+DGCG(3,NFE)*T**(-DGCG(4,NFE))
      XPGL=DGA*X**DGB*X1**DGC
 
C...Evaluate up- and down-type quark content.
      DGA=DGAN(1,NFE)*T**DGAN(2,NFE)+DGAN(3,NFE)*T**(-DGAN(4,NFE))
      DGB=DGBN(1,NFE)*T**DGBN(2,NFE)+DGBN(3,NFE)*T**(-DGBN(4,NFE))
      DGC=DGCN(1,NFE)*T**DGCN(2,NFE)+DGCN(3,NFE)*T**(-DGCN(4,NFE))
      DGD=DGDN(1,NFE)*T**DGDN(2,NFE)+DGDN(3,NFE)*T**(-DGDN(4,NFE))
      DGE=DGEN(1,NFE)*T**DGEN(2,NFE)+DGEN(3,NFE)*T**(-DGEN(4,NFE))
      XPQN=X*(X**2+X1**2)/(DGA-DGB*LOG(X1))+DGC*X**DGD*X1**DGE
      DGA=DGAS(1,NFE)*T**DGAS(2,NFE)+DGAS(3,NFE)*T**(-DGAS(4,NFE))
      DGB=DGBS(1,NFE)*T**DGBS(2,NFE)+DGBS(3,NFE)*T**(-DGBS(4,NFE))
      DGC=DGCS(1,NFE)*T**DGCS(2,NFE)+DGCS(3,NFE)*T**(-DGCS(4,NFE))
      DGD=DGDS(1,NFE)*T**DGDS(2,NFE)+DGDS(3,NFE)*T**(-DGDS(4,NFE))
      DGE=DGES(1,NFE)*T**DGES(2,NFE)+DGES(3,NFE)*T**(-DGES(4,NFE))
      DGF=9.
      IF(NF.EQ.4) DGF=10.
      IF(NF.EQ.5) DGF=55./6.
      XPQS=DGF*X*(X**2+X1**2)/(DGA-DGB*LOG(X1))+DGC*X**DGD*X1**DGE
      IF(NF.LE.3) THEN
        XPQU=(XPQS+9.*XPQN)/6.
        XPQD=(XPQS-4.5*XPQN)/6.
      ELSEIF(NF.EQ.4) THEN
        XPQU=(XPQS+6.*XPQN)/8.
        XPQD=(XPQS-6.*XPQN)/8.
      ELSE
        XPQU=(XPQS+7.5*XPQN)/10.
        XPQD=(XPQS-5.*XPQN)/10.
      ENDIF
 
C...Put into output arrays.
      XPGA(0)=AEM*XPGL
      XPGA(1)=AEM*XPQD
      XPGA(2)=AEM*XPQU
      XPGA(3)=AEM*XPQD
      IF(NF.GE.4) XPGA(4)=AEM*XPQU
      IF(NF.GE.5) XPGA(5)=AEM*XPQD
      DO 110 KFL=1,6
      XPGA(-KFL)=XPGA(KFL)
  110 CONTINUE
 
      RETURN
      END
 
C*********************************************************************
 
C...The following routines are adapted from
C...SaSgam - parton distributions of the photon
C...by Gerhard A. Schuler and Torbjorn Sjostrand
C...For further information see CERN-TH/95-62.
C...The version found here is NOT suitable for standalone usage.
 
      SUBROUTINE PYGGAM(ISET,X,Q2,P2,F2GM,XPDFGM)
C...Purpose: to construct the F2 and parton distributions of the photon
C...by summing homogeneous (VMD) and inhomogeneous (anomalous) terms.
C...For F2, c and b are included by the Bethe-Heitler formula;
C...in the 'MSbar' scheme additionally a Cgamma term is added.
      DIMENSION XPDFGM(-6:6)
      COMMON/PYINT8/XPVMD(-6:6),XPANL(-6:6),XPANH(-6:6),XPBEH(-6:6),
     &XPDIR(-6:6)
      REAL XPVMD,XPANL,XPANH,XPBEH,XPDIR
      SAVE /PYINT8/
C...Temporary array.
      DIMENSION XPGA(-6:6)
C...Charm and bottom masses (low to compensate for J/psi etc.).
      DATA PMC/1.3/, PMB/4.6/
C...alpha_em and alpha_em/(2*pi).
      DATA AEM/0.007297/, AEM2PI/0.0011614/
C...Lambda value for 4 flavours.
      DATA ALAM/0.20/
C...Mixture u/(u+d), = 0.5 for incoherent and = 0.8 for coherent sum.
      DATA FRACU/0.8/
C...VMD couplings f_V**2/(4*pi).
      DATA FRHO/2.20/, FOMEGA/23.6/, FPHI/18.4/
C...Masses for rho (=omega) and phi.
      DATA PMRHO/0.770/, PMPHI/1.020/
 
C...Reset output.
      F2GM=0.
      DO 100 KFL=-6,6
      XPDFGM(KFL)=0.
      XPVMD(KFL)=0.
      XPANL(KFL)=0.
      XPANH(KFL)=0.
      XPBEH(KFL)=0.
      XPDIR(KFL)=0.
  100 CONTINUE
 
C...Set k0 cut-off parameter as function of set used.
      IF(ISET.LE.2) THEN
        AK0=0.6
      ELSE
        AK0=2.
      ENDIF
 
C...Call VMD parametrization for d quark and use to give rho+omega+ phi.
C...Note scale choice and dipole dampening for off-shell photon.
      P2MX=MAX(P2,AK0**2)
      CALL PYGVMD(ISET,1,X,Q2,P2MX,ALAM,XPGA)
      XFVAL=XPGA(1)-XPGA(2)
      XPGA(1)=XPGA(2)
      XPGA(-1)=XPGA(-2)
      FACUD=AEM*(1./FRHO+1./FOMEGA)*(PMRHO**2/(PMRHO**2+P2))**2
      FACS=AEM*(1./FPHI)*(PMPHI**2/(PMPHI**2+P2))**2
      DO 110 KFL=-5,5
      XPVMD(KFL)=(FACUD+FACS)*XPGA(KFL)
  110 CONTINUE
      XPVMD(1)=XPVMD(1)+(1.-FRACU)*FACUD*XFVAL
      XPVMD(2)=XPVMD(2)+FRACU*FACUD*XFVAL
      XPVMD(3)=XPVMD(3)+FACS*XFVAL
      XPVMD(-1)=XPVMD(-1)+(1.-FRACU)*FACUD*XFVAL
      XPVMD(-2)=XPVMD(-2)+FRACU*FACUD*XFVAL
      XPVMD(-3)=XPVMD(-3)+FACS*XFVAL
 
C...Call anomalous parametrization for d + u + s.
      CALL PYGANO(-3,X,Q2,P2MX,ALAM,XPGA)
      DO 120 KFL=-5,5
      XPANL(KFL)=XPGA(KFL)
  120 CONTINUE
 
C...Call anomalous parametrization for c and b.
      CALL PYGANO(4,X,Q2,P2MX,ALAM,XPGA)
      DO 130 KFL=-5,5
      XPANH(KFL)=XPGA(KFL)
  130 CONTINUE
      CALL PYGANO(5,X,Q2,P2MX,ALAM,XPGA)
      DO 140 KFL=-5,5
      XPANH(KFL)=XPANH(KFL)+XPGA(KFL)
  140 CONTINUE
 
C...Call Bethe-Heitler term expression for charm and bottom.
      CALL PYGBEH(4,X,Q2,P2,PMC**2,XPBH)
      XPBEH(4)=XPBH
      XPBEH(-4)=XPBH
      CALL PYGBEH(5,X,Q2,P2,PMB**2,XPBH)
      XPBEH(5)=XPBH
      XPBEH(-5)=XPBH
 
C...For MSbar subtraction call C^gamma term expression for d, u, s.
      IF(ISET.EQ.2.OR.ISET.EQ.4) THEN
        CALL PYGDIR(X,Q2,P2,AK0,XPGA)
        DO 150 KFL=-5,5
        XPDIR(KFL)=XPGA(KFL)
  150   CONTINUE
      ENDIF
 
C...Store result in output array.
      DO 160 KFL=-5,5
      CHSQ=1./9.
      IF(IABS(KFL).EQ.2.OR.IABS(KFL).EQ.4) CHSQ=4./9.
      XPF2=XPVMD(KFL)+XPANL(KFL)+XPBEH(KFL)+XPDIR(KFL)
      IF(KFL.NE.0) F2GM=F2GM+CHSQ*XPF2
      XPDFGM(KFL)=XPVMD(KFL)+XPANL(KFL)+XPANH(KFL)
  160 CONTINUE
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYGVMD(ISET,KF,X,Q2,P2,ALAM,XPGA)
C...Purpose: to evaluate the VMD parton distributions of a photon,
C...evolved homogeneously from an initial scale P2 to Q2.
C...Does not include dipole suppression factor.
C...ISET is parton distribution set, see above;
C...additionally ISET=0 is used for the evolution of an anomalous photon
C...which branched at a scale P2 and then evolved homogeneously to Q2.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      DIMENSION XPGA(-6:6)
      DATA PMC/1.3/, PMB/4.6/, AEM/0.007297/, AEM2PI/0.0011614/
 
C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
      KFA=IABS(KF)
 
C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      ALAM3=ALAM*(PMC/ALAM)**(2./27.)
      ALAM5=ALAM*(ALAM/PMB)**(2./23.)
      P2EFF=MAX(P2,1.2*ALAM3**2)
      IF(KFA.EQ.4) P2EFF=MAX(P2EFF,PMC**2)
      IF(KFA.EQ.5) P2EFF=MAX(P2EFF,PMB**2)
      Q2EFF=MAX(Q2,P2EFF)
 
C...Find number of flavours at lower and upper scale.
      NFP=4
      IF(P2EFF.LT.PMC**2) NFP=3
      IF(P2EFF.GT.PMB**2) NFP=5
      NFQ=4
      IF(Q2EFF.LT.PMC**2) NFQ=3
      IF(Q2EFF.GT.PMB**2) NFQ=5
 
C...Find s as sum of 3-, 4- and 5-flavour parts.
      S=0.
      IF(NFP.EQ.3) THEN
        Q2DIV=PMC**2
        IF(NFQ.EQ.3) Q2DIV=Q2EFF
        S=S+(6./27.)*LOG(LOG(Q2DIV/ALAM3**2)/LOG(P2EFF/ALAM3**2))
      ENDIF
      IF(NFP.LE.4.AND.NFQ.GE.4) THEN
        P2DIV=P2EFF
        IF(NFP.EQ.3) P2DIV=PMC**2
        Q2DIV=Q2EFF
        IF(NFQ.EQ.5) Q2DIV=PMB**2
        S=S+(6./25.)*LOG(LOG(Q2DIV/ALAM**2)/LOG(P2DIV/ALAM**2))
      ENDIF
      IF(NFQ.EQ.5) THEN
        P2DIV=PMB**2
        IF(NFP.EQ.5) P2DIV=P2EFF
        S=S+(6./23.)*LOG(LOG(Q2EFF/ALAM5**2)/LOG(P2DIV/ALAM5**2))
      ENDIF
 
C...Calculate frequent combinations of x and s.
      X1=1.-X
      XL=-LOG(X)
      S2=S**2
      S3=S**3
      S4=S**4
 
C...Evaluate homogeneous anomalous parton distributions below or
C...above threshold.
      IF(ISET.EQ.0) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = X * 1.5 * (X**2+X1**2)
        XGLU = 0.
        XSEA = 0.
      ELSE
        XVAL = (1.5/(1.-0.197*S+4.33*S2)*X**2 + (1.5+2.10*S)/
     &  (1.+3.29*S)*X1**2 + 5.23*S/(1.+1.17*S+19.9*S3)*X*X1) *
     &  X**(1./(1.+1.5*S)) * (1.-X**2)**(2.667*S)
        XGLU = 4.*S/(1.+4.76*S+15.2*S2+29.3*S4) *
     &  X**(-2.03*S/(1.+2.44*S)) * (X1*XL)**(1.333*S) *
     &  ((4.*X**2+7.*X+4.)*X1/3. - 2.*X*(1.+X)*XL)
        XSEA = S2/(1.+4.54*S+8.19*S2+8.05*S3) *
     &  X**(-1.54*S/(1.+1.29*S)) * X1**(2.667*S) *
     &  ((8.-73.*X+62.*X**2)*X1/9. + (3.-8.*X**2/3.)*X*XL +
     &  (2.*X-1.)*X*XL**2)
      ENDIF
 
C...Evaluate set 1D parton distributions below or above threshold.
      ELSEIF(ISET.EQ.1) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = 1.294 * X**0.80 * X1**0.76
        XGLU = 1.273 * X**0.40 * X1**1.76
        XSEA = 0.100 * X1**3.76
      ELSE
        XVAL = 1.294/(1.+0.252*S+3.079*S2) * X**(0.80-0.13*S) *
     &  X1**(0.76+0.667*S) * XL**(2.*S)
        XGLU = 7.90*S/(1.+5.50*S) * EXP(-5.16*S) *
     &  X**(-1.90*S/(1.+3.60*S)) * X1**1.30 * XL**(0.50+3.*S) +
     &  1.273 * EXP(-10.*S) * X**0.40 * X1**(1.76+3.*S)
        XSEA = (0.1-0.397*S2+1.121*S3)/(1.+5.61*S2+5.26*S3) *
     &  X**(-7.32*S2/(1.+10.3*S2)) *
     &  X1**((3.76+15.*S+12.*S2)/(1.+4.*S))
        XSEA0 = 0.100 * X1**3.76
      ENDIF
 
C...Evaluate set 1M parton distributions below or above threshold.
      ELSEIF(ISET.EQ.2) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = 0.8477 * X**0.51 * X1**1.37
        XGLU = 3.42 * X**0.255 * X1**2.37
        XSEA = 0.
      ELSE
        XVAL = 0.8477/(1.+1.37*S+2.18*S2+3.73*S3) * X**(0.51+0.21*S)
     &  * X1**1.37 * XL**(2.667*S)
        XGLU = 24.*S/(1.+9.6*S+0.92*S2+14.34*S3) * EXP(-5.94*S) *
     &  X**((-0.013-1.80*S)/(1.+3.14*S)) * X1**(2.37+0.4*S) *
     &  XL**(0.32+3.6*S) + 3.42 * EXP(-12.*S) * X**0.255 *
     &  X1**(2.37+3.*S)
        XSEA = 0.842*S/(1.+21.3*S-33.2*S2+229.*S3) *
     &  X**((0.13-2.90*S)/(1.+5.44*S)) * X1**(3.45+0.5*S) *
     &  XL**(2.8*S)
        XSEA0 = 0.
      ENDIF
 
C...Evaluate set 2D parton distributions below or above threshold.
      ELSEIF(ISET.EQ.3) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = X**0.46 * X1**0.64 + 0.76 * X
        XGLU = 1.925 * X1**2
        XSEA = 0.242 * X1**4
      ELSE
        XVAL = (1.+0.186*S)/(1.-0.209*S+1.495*S2) * X**(0.46+0.25*S)
     &  * X1**((0.64+0.14*S+5.*S2)/(1.+S)) * XL**(1.9*S) +
     &  (0.76+0.4*S) * X * X1**(2.667*S)
        XGLU = (1.925+5.55*S+147.*S2)/(1.-3.59*S+3.32*S2) *
     &  EXP(-18.67*S) * X**((-5.81*S-5.34*S2)/(1.+29.*S-4.26*S2))
     &  * X1**((2.-5.9*S)/(1.+1.7*S)) * XL**(9.3*S/(1.+1.7*S))
        XSEA = (0.242-0.252*S+1.19*S2)/(1.-0.607*S+21.95*S2) *
     &  X**(-12.1*S2/(1.+2.62*S+16.7*S2)) * X1**4 * XL**S
        XSEA0 = 0.242 * X1**4
      ENDIF
 
C...Evaluate set 2M parton distributions below or above threshold.
      ELSEIF(ISET.EQ.4) THEN
      IF(Q2.LE.P2.OR.(KFA.EQ.4.AND.Q2.LT.PMC**2).OR.
     &(KFA.EQ.5.AND.Q2.LT.PMB**2)) THEN
        XVAL = 1.168 * X**0.50 * X1**2.60 + 0.965 * X
        XGLU = 1.808 * X1**2
        XSEA = 0.209 * X1**4
      ELSE
        XVAL = (1.168+1.771*S+29.35*S2) * EXP(-5.776*S) *
     &  X**((0.5+0.208*S)/(1.-0.794*S+1.516*S2)) *
     &  X1**((2.6+7.6*S)/(1.+5.*S)) * XL**(5.15*S/(1.+2.*S)) +
     &  (0.965+22.35*S)/(1.+18.4*S) * X * X1**(2.667*S)
        XGLU = (1.808+29.9*S)/(1.+26.4*S) * EXP(-5.28*S) *
     &  X**((-5.35*S-10.11*S2)/(1.+31.71*S)) *
     &  X1**((2.-7.3*S+4.*S2)/(1.+2.5*S)) *
     &  XL**(10.9*S/(1.+2.5*S))
        XSEA = (0.209+0.644*S2)/(1.+0.319*S+17.6*S2) *
     &  X**((-0.373*S-7.71*S2)/(1.+0.815*S+11.0*S2)) *
     &  X1**(4.+S) * XL**(0.45*S)
        XSEA0 = 0.209 * X1**4
      ENDIF
      ENDIF
 
C...Threshold factors for c and b sea.
      SLL=LOG(LOG(Q2EFF/ALAM**2)/LOG(P2EFF/ALAM**2))
      XCHM=0.
      IF(Q2.GT.PMC**2.AND.Q2.GT.1.001*P2EFF) THEN
        SCH=MAX(0.,LOG(LOG(PMC**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
        IF(ISET.EQ.0) THEN
          XCHM=XSEA*(1.-(SCH/SLL)**2)
        ELSE
          XCHM=MAX(0.,XSEA-XSEA0*X1**(2.667*S))*(1.-SCH/SLL)
        ENDIF
      ENDIF
      XBOT=0.
      IF(Q2.GT.PMB**2.AND.Q2.GT.1.001*P2EFF) THEN
        SBT=MAX(0.,LOG(LOG(PMB**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
        IF(ISET.EQ.0) THEN
          XBOT=XSEA*(1.-(SBT/SLL)**2)
        ELSE
          XBOT=MAX(0.,XSEA-XSEA0*X1**(2.667*S))*(1.-SBT/SLL)
        ENDIF
      ENDIF
 
C...Fill parton distributions.
      XPGA(0)=XGLU
      XPGA(1)=XSEA
      XPGA(2)=XSEA
      XPGA(3)=XSEA
      XPGA(4)=XCHM
      XPGA(5)=XBOT
      XPGA(KFA)=XPGA(KFA)+XVAL
      DO 110 KFL=1,5
      XPGA(-KFL)=XPGA(KFL)
  110 CONTINUE
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYGANO(KF,X,Q2,P2,ALAM,XPGA)
C...Purpose: to evaluate the parton distributions of the anomalous
C...photon, inhomogeneously evolved from a scale P2 (where it vanishes)
C...to Q2.
C...KF=0 gives the sum over (up to) 5 flavours,
C...KF<0 limits to flavours up to abs(KF),
C...KF>0 is for flavour KF only.
C...ALAM is the 4-flavour Lambda, which is automatically converted
C...to 3- and 5-flavour equivalents as needed.
      DIMENSION XPGA(-6:6),ALAMSQ(3:5)
      DATA PMC/1.3/, PMB/4.6/, AEM/0.007297/, AEM2PI/0.0011614/
 
C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
      IF(Q2.LE.P2) RETURN
      KFA=IABS(KF)
 
C...Calculate Lambda; protect against unphysical Q2 and P2 input.
      ALAMSQ(3)=(ALAM*(PMC/ALAM)**(2./27.))**2
      ALAMSQ(4)=ALAM**2
      ALAMSQ(5)=(ALAM*(ALAM/PMB)**(2./23.))**2
      P2EFF=MAX(P2,1.2*ALAMSQ(3))
      IF(KF.EQ.4) P2EFF=MAX(P2EFF,PMC**2)
      IF(KF.EQ.5) P2EFF=MAX(P2EFF,PMB**2)
      Q2EFF=MAX(Q2,P2EFF)
      XL=-LOG(X)
 
C...Find number of flavours at lower and upper scale.
      NFP=4
      IF(P2EFF.LT.PMC**2) NFP=3
      IF(P2EFF.GT.PMB**2) NFP=5
      NFQ=4
      IF(Q2EFF.LT.PMC**2) NFQ=3
      IF(Q2EFF.GT.PMB**2) NFQ=5
 
C...Define range of flavour loop.
      IF(KF.EQ.0) THEN
        KFLMN=1
        KFLMX=5
      ELSEIF(KF.LT.0) THEN
        KFLMN=1
        KFLMX=KFA
      ELSE
        KFLMN=KFA
        KFLMX=KFA
      ENDIF
 
C...Loop over flavours the photon can branch into.
      DO 110 KFL=KFLMN,KFLMX
 
C...Light flavours: calculate t range and (approximate) s range.
      IF(KFL.LE.3.AND.(KFL.EQ.1.OR.KFL.EQ.KF)) THEN
        TDIFF=LOG(Q2EFF/P2EFF)
        S=(6./(33.-2.*NFQ))*LOG(LOG(Q2EFF/ALAMSQ(NFQ))/
     &  LOG(P2EFF/ALAMSQ(NFQ)))
        IF(NFQ.GT.NFP) THEN
          Q2DIV=PMB**2
          IF(NFQ.EQ.4) Q2DIV=PMC**2
          SNFQ=(6./(33.-2.*NFQ))*LOG(LOG(Q2DIV/ALAMSQ(NFQ))/
     &    LOG(P2EFF/ALAMSQ(NFQ)))
          SNFP=(6./(33.-2.*(NFQ-1)))*LOG(LOG(Q2DIV/ALAMSQ(NFQ-1))/
     &    LOG(P2EFF/ALAMSQ(NFQ-1)))
          S=S+(LOG(Q2DIV/P2EFF)/LOG(Q2EFF/P2EFF))*(SNFP-SNFQ)
        ENDIF
        IF(NFQ.EQ.5.AND.NFP.EQ.3) THEN
          Q2DIV=PMC**2
          SNF4=(6./(33.-2.*4))*LOG(LOG(Q2DIV/ALAMSQ(4))/
     &    LOG(P2EFF/ALAMSQ(4)))
          SNF3=(6./(33.-2.*3))*LOG(LOG(Q2DIV/ALAMSQ(3))/
     &    LOG(P2EFF/ALAMSQ(3)))
          S=S+(LOG(Q2DIV/P2EFF)/LOG(Q2EFF/P2EFF))*(SNF3-SNF4)
        ENDIF
 
C...u and s quark do not need a separate treatment when d has been done.
      ELSEIF(KFL.EQ.2.OR.KFL.EQ.3) THEN
 
C...Charm: as above, but only include range above c threshold.
      ELSEIF(KFL.EQ.4) THEN
        IF(Q2.LE.PMC**2) GOTO 110
        P2EFF=MAX(P2EFF,PMC**2)
        Q2EFF=MAX(Q2EFF,P2EFF)
        TDIFF=LOG(Q2EFF/P2EFF)
        S=(6./(33.-2.*NFQ))*LOG(LOG(Q2EFF/ALAMSQ(NFQ))/
     &  LOG(P2EFF/ALAMSQ(NFQ)))
        IF(NFQ.EQ.5.AND.NFP.EQ.4) THEN
          Q2DIV=PMB**2
          SNFQ=(6./(33.-2.*NFQ))*LOG(LOG(Q2DIV/ALAMSQ(NFQ))/
     &    LOG(P2EFF/ALAMSQ(NFQ)))
          SNFP=(6./(33.-2.*(NFQ-1)))*LOG(LOG(Q2DIV/ALAMSQ(NFQ-1))/
     &    LOG(P2EFF/ALAMSQ(NFQ-1)))
          S=S+(LOG(Q2DIV/P2EFF)/LOG(Q2EFF/P2EFF))*(SNFP-SNFQ)
        ENDIF
 
C...Bottom: as above, but only include range above b threshold.
      ELSEIF(KFL.EQ.5) THEN
        IF(Q2.LE.PMB**2) GOTO 110
        P2EFF=MAX(P2EFF,PMB**2)
        Q2EFF=MAX(Q2,P2EFF)
        TDIFF=LOG(Q2EFF/P2EFF)
        S=(6./(33.-2.*NFQ))*LOG(LOG(Q2EFF/ALAMSQ(NFQ))/
     &  LOG(P2EFF/ALAMSQ(NFQ)))
      ENDIF
 
C...Evaluate flavour-dependent prefactor (charge^2 etc.).
      CHSQ=1./9.
      IF(KFL.EQ.2.OR.KFL.EQ.4) CHSQ=4./9.
      FAC=AEM2PI*2.*CHSQ*TDIFF
 
C...Evaluate parton distributions (normalized to unit momentum sum).
      IF(KFL.EQ.1.OR.KFL.EQ.4.OR.KFL.EQ.5.OR.KFL.EQ.KF) THEN
        XVAL= ((1.5+2.49*S+26.9*S**2)/(1.+32.3*S**2)*X**2 +
     &  (1.5-0.49*S+7.83*S**2)/(1.+7.68*S**2)*(1.-X)**2 +
     &  1.5*S/(1.-3.2*S+7.*S**2)*X*(1.-X)) *
     &  X**(1./(1.+0.58*S)) * (1.-X**2)**(2.5*S/(1.+10.*S))
        XGLU= 2.*S/(1.+4.*S+7.*S**2) *
     &  X**(-1.67*S/(1.+2.*S)) * (1.-X**2)**(1.2*S) *
     &  ((4.*X**2+7.*X+4.)*(1.-X)/3. - 2.*X*(1.+X)*XL)
        XSEA= 0.333*S**2/(1.+4.90*S+4.69*S**2+21.4*S**3) *
     &  X**(-1.18*S/(1.+1.22*S)) * (1.-X)**(1.2*S) *
     &  ((8.-73.*X+62.*X**2)*(1.-X)/9. + (3.-8.*X**2/3.)*X*XL +
     &  (2.*X-1.)*X*XL**2)
 
C...Threshold factors for c and b sea.
        SLL=LOG(LOG(Q2EFF/ALAM**2)/LOG(P2EFF/ALAM**2))
        XCHM=0.
        IF(Q2.GT.PMC**2.AND.Q2.GT.1.001*P2EFF) THEN
          SCH=MAX(0.,LOG(LOG(PMC**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
          XCHM=XSEA*(1.-(SCH/SLL)**3)
        ENDIF
        XBOT=0.
        IF(Q2.GT.PMB**2.AND.Q2.GT.1.001*P2EFF) THEN
          SBT=MAX(0.,LOG(LOG(PMB**2/ALAM**2)/LOG(P2EFF/ALAM**2)))
          XBOT=XSEA*(1.-(SBT/SLL)**3)
        ENDIF
      ENDIF
 
C...Add contribution of each valence flavour.
      XPGA(0)=XPGA(0)+FAC*XGLU
      XPGA(1)=XPGA(1)+FAC*XSEA
      XPGA(2)=XPGA(2)+FAC*XSEA
      XPGA(3)=XPGA(3)+FAC*XSEA
      XPGA(4)=XPGA(4)+FAC*XCHM
      XPGA(5)=XPGA(5)+FAC*XBOT
      XPGA(KFL)=XPGA(KFL)+FAC*XVAL
  110 CONTINUE
      DO 120 KFL=1,5
      XPGA(-KFL)=XPGA(KFL)
  120 CONTINUE
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYGBEH(KF,X,Q2,P2,PM2,XPBH)
C...Purpose: to evaluate the Bethe-Heitler cross section for
C...heavy flavour production.
      DATA AEM2PI/0.0011614/
 
C...Reset output.
      XPBH=0.
      SIGBH=0.
 
C...Check kinematics limits.
      IF(X.GE.Q2/(4.*PM2+Q2+P2)) RETURN
      W2=Q2*(1.-X)/X-P2
      BETA2=1.-4.*PM2/W2
      IF(BETA2.LT.1E-10) RETURN
      RMQ=4.*PM2/Q2
 
C...Simple case: P2 = 0.
      IF(P2.LT.1E-4) THEN
        BETA=SQRT(BETA2)
        IF(BETA.LT.0.99) THEN
          XBL=LOG((1.+BETA)/(1.-BETA))
        ELSE
          XBL=LOG((1.+BETA)**2*W2/(4.*PM2))
        ENDIF
        SIGBH=BETA*(8.*X*(1.-X)-1.-RMQ*X*(1.-X))+
     &  XBL*(X**2+(1.-X)**2+RMQ*X*(1.-3.*X)-0.5*RMQ**2*X**2)
 
C...Complicated case: P2 > 0, based on approximation of
C...C.T. Hill and G.G. Ross, Nucl. Phys. B148 (1979) 373
      ELSE
        RPQ=1.-4.*X**2*P2/Q2
        IF(RPQ.GT.1E-10) THEN
          RPBE=SQRT(RPQ*BETA2)
          IF(RPBE.LT.0.99) THEN
            XBL=LOG((1.+RPBE)/(1.-RPBE))
            XBI=2.*RPBE/(1.-RPBE**2)
          ELSE
            RPBESN=4.*PM2/W2+(4.*X**2*P2/Q2)*BETA2
            XBL=LOG((1.+RPBE)**2/RPBESN)
            XBI=2.*RPBE/RPBESN
          ENDIF
          SIGBH=BETA*(6.*X*(1.-X)-1.)+
     &    XBL*(X**2+(1.-X)**2+RMQ*X*(1.-3.*X)-0.5*RMQ**2*X**2)+
     &    XBI*(2.*X/Q2)*(PM2*X*(2.-RMQ)-P2*X)
        ENDIF
      ENDIF
 
C...Multiply by charge-squared etc. to get parton distribution.
      CHSQ=1./9.
      IF(IABS(KF).EQ.2.OR.IABS(KF).EQ.4) CHSQ=4./9.
      XPBH=3.*CHSQ*AEM2PI*X*SIGBH
 
      RETURN
      END
 
C*********************************************************************
 
       SUBROUTINE PYGDIR(X,Q2,P2,AK0,XPGA)
C...Purpose: to evaluate the direct contribution, i.e. the C^gamma term,
C...as needed in MSbar parametrizations.
      DIMENSION XPGA(-6:6)
      DATA PMC/1.3/, PMB/4.6/, AEM2PI/0.0011614/
 
C...Reset output.
      DO 100 KFL=-6,6
      XPGA(KFL)=0.
  100 CONTINUE
 
C...Evaluate common x-dependent expression.
      XTMP = (X**2+(1.-X)**2) * (-LOG(X)) - 1.
      CGAM = 3.*AEM2PI*X * (XTMP*(1.+P2/(P2+AK0**2)) + 6.*X*(1.-X))
 
C...d, u, s part by simple charge factor.
      XPGA(1)=(1./9.)*CGAM
      XPGA(2)=(4./9.)*CGAM
      XPGA(3)=(1./9.)*CGAM
 
C...Also fill for antiquarks.
      DO 110 KF=1,5
      XPGA(-KF)=XPGA(KF)
  110 CONTINUE
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSTPI(X,Q2,XPPI)
 
C...Gives pi+ structure function according to two different
C...parametrizations.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION XPPI(-6:6),COW(3,5,4,2),XQ(9),TS(6)
 
C...The following data lines are coefficients needed in the
C...Owens pion structure function parametrizations, see below.
C...Expansion coefficients for up and down valence quark distributions.
      DATA ((COW(IP,IS,1,1),IS=1,5),IP=1,3)/
     1  4.0000E-01,  7.0000E-01,  0.0000E+00,  0.0000E+00,  0.0000E+00,
     2 -6.2120E-02,  6.4780E-01,  0.0000E+00,  0.0000E+00,  0.0000E+00,
     3 -7.1090E-03,  1.3350E-02,  0.0000E+00,  0.0000E+00,  0.0000E+00/
      DATA ((COW(IP,IS,1,2),IS=1,5),IP=1,3)/
     1  4.0000E-01,  6.2800E-01,  0.0000E+00,  0.0000E+00,  0.0000E+00,
     2 -5.9090E-02,  6.4360E-01,  0.0000E+00,  0.0000E+00,  0.0000E+00,
     3 -6.5240E-03,  1.4510E-02,  0.0000E+00,  0.0000E+00,  0.0000E+00/
C...Expansion coefficients for gluon distribution.
      DATA ((COW(IP,IS,2,1),IS=1,5),IP=1,3)/
     1  8.8800E-01,  0.0000E+00,  3.1100E+00,  6.0000E+00,  0.0000E+00,
     2 -1.8020E+00, -1.5760E+00, -1.3170E-01,  2.8010E+00, -1.7280E+01,
     3  1.8120E+00,  1.2000E+00,  5.0680E-01, -1.2160E+01,  2.0490E+01/
      DATA ((COW(IP,IS,2,2),IS=1,5),IP=1,3)/
     1  7.9400E-01,  0.0000E+00,  2.8900E+00,  6.0000E+00,  0.0000E+00,
     2 -9.1440E-01, -1.2370E+00,  5.9660E-01, -3.6710E+00, -8.1910E+00,
     3  5.9660E-01,  6.5820E-01, -2.5500E-01, -2.3040E+00,  7.7580E+00/
C...Expansion coefficients for (up+down+strange) quark sea distribution.
      DATA ((COW(IP,IS,3,1),IS=1,5),IP=1,3)/
     1  9.0000E-01,  0.0000E+00,  5.0000E+00,  0.0000E+00,  0.0000E+00,
     2 -2.4280E-01, -2.1200E-01,  8.6730E-01,  1.2660E+00,  2.3820E+00,
     3  1.3860E-01,  3.6710E-03,  4.7470E-02, -2.2150E+00,  3.4820E-01/
      DATA ((COW(IP,IS,3,2),IS=1,5),IP=1,3)/
     1  9.0000E-01,  0.0000E+00,  5.0000E+00,  0.0000E+00,  0.0000E+00,
     2 -1.4170E-01, -1.6970E-01, -2.4740E+00, -2.5340E+00,  5.6210E-01,
     3 -1.7400E-01, -9.6230E-02,  1.5750E+00,  1.3780E+00, -2.7010E-01/
C...Expansion coefficients for charm quark sea distribution.
      DATA ((COW(IP,IS,4,1),IS=1,5),IP=1,3)/
     1  0.0000E+00, -2.2120E-02,  2.8940E+00,  0.0000E+00,  0.0000E+00,
     2  7.9280E-02, -3.7850E-01,  9.4330E+00,  5.2480E+00,  8.3880E+00,
     3 -6.1340E-02, -1.0880E-01, -1.0852E+01, -7.1870E+00, -1.1610E+01/
      DATA ((COW(IP,IS,4,2),IS=1,5),IP=1,3)/
     1  0.0000E+00, -8.8200E-02,  1.9240E+00,  0.0000E+00,  0.0000E+00,
     2  6.2290E-02, -2.8920E-01,  2.4240E-01, -4.4630E+00, -8.3670E-01,
     3 -4.0990E-02, -1.0820E-01,  2.0360E+00,  5.2090E+00, -4.8400E-02/
 
C...Euler's beta function, requires ordinary Gamma function
      EULBET(X,Y)=PYGAMM(X)*PYGAMM(Y)/PYGAMM(X+Y)
 
C...Reset output array.
      DO 100 KFL=-6,6
      XPPI(KFL)=0.
  100 CONTINUE
 
      IF(MSTP(53).LE.2) THEN
C...Pion structure functions from Owens.
C...Allowed variable range: 4 GeV^2 < Q^2 < approx 2000 GeV^2.
 
C...Determine set, Lambda and s expansion variable.
        NSET=MSTP(53)
        IF(NSET.EQ.1) ALAM=0.2
        IF(NSET.EQ.2) ALAM=0.4
        VINT(231)=4.
        IF(MSTP(57).LE.0) THEN
          SD=0.
        ELSE
          Q2IN=MIN(2E3,MAX(4.,Q2))
          SD=LOG(LOG(Q2IN/ALAM**2)/LOG(4./ALAM**2))
        ENDIF
 
C...Calculate structure functions.
        DO 120 KFL=1,4
        DO 110 IS=1,5
        TS(IS)=COW(1,IS,KFL,NSET)+COW(2,IS,KFL,NSET)*SD+
     &  COW(3,IS,KFL,NSET)*SD**2
  110   CONTINUE
        IF(KFL.EQ.1) THEN
          XQ(KFL)=X**TS(1)*(1.-X)**TS(2)/EULBET(TS(1),TS(2)+1.)
        ELSE
          XQ(KFL)=TS(1)*X**TS(2)*(1.-X)**TS(3)*(1.+TS(4)*X+TS(5)*X**2)
        ENDIF
  120   CONTINUE
 
C...Put into output array.
        XPPI(0)=XQ(2)
        XPPI(1)=XQ(3)/6.
        XPPI(2)=XQ(1)+XQ(3)/6.
        XPPI(3)=XQ(3)/6.
        XPPI(4)=XQ(4)
        XPPI(-1)=XQ(1)+XQ(3)/6.
        XPPI(-2)=XQ(3)/6.
        XPPI(-3)=XQ(3)/6.
        XPPI(-4)=XQ(4)
 
C...Leading order pion structure functions from Gluck, Reya and Vogt.
C...Allowed variable range: 0.25 GeV^2 < Q^2 < 10^8 GeV^2 and
C...10^-5 < x < 1.
      ELSE
 
C...Determine s expansion variable and some x expressions.
        VINT(231)=0.25
        IF(MSTP(57).LE.0) THEN
          SD=0.
        ELSE
          Q2IN=MIN(1E8,MAX(0.25,Q2))
          SD=LOG(LOG(Q2IN/0.232**2)/LOG(0.25/0.232**2))
        ENDIF
        SD2=SD**2
        XL=-LOG(X)
        XS=SQRT(X)
 
C...Evaluate valence, gluon and sea distributions.
        XFVAL=(0.519+0.180*SD-0.011*SD2)*X**(0.499-0.027*SD)*
     &  (1.+(0.381-0.419*SD)*XS)*(1.-X)**(0.367+0.563*SD)
        XFGLU=(X**(0.482+0.341*SQRT(SD))*((0.678+0.877*SD-0.175*SD2)+
     &  (0.338-1.597*SD)*XS+(-0.233*SD+0.406*SD2)*X)+
     &  SD**0.599*EXP(-(0.618+2.070*SD)+SQRT(3.676*SD**1.263*XL)))*
     &  (1.-X)**(0.390+1.053*SD)
        XFSEA=SD**0.55*(1.-0.748*XS+(0.313+0.935*SD)*X)*(1.-X)**3.359*
     &  EXP(-(4.433+1.301*SD)+SQRT((9.30-0.887*SD)*SD**0.56*XL))/
     &  XL**(2.538-0.763*SD)
        IF(SD.LE.0.888) THEN
          XFCHM=0.
        ELSE
          XFCHM=(SD-0.888)**1.02*(1.+1.008*X)*(1.-X)**(1.208+0.771*SD)*
     &    EXP(-(4.40+1.493*SD)+SQRT((2.032+1.901*SD)*SD**0.39*XL))
        ENDIF
        IF(SD.LE.1.351) THEN
          XFBOT=0.
        ELSE
          XFBOT=(SD-1.351)**1.03*(1.-X)**(0.697+0.855*SD)*
     &    EXP(-(4.51+1.490*SD)+SQRT((3.056+1.694*SD)*SD**0.39*XL))
        ENDIF
 
C...Put into output array.
        XPPI(0)=XFGLU
        XPPI(1)=XFSEA
        XPPI(2)=XFSEA
        XPPI(3)=XFSEA
        XPPI(4)=XFCHM
        XPPI(5)=XFBOT
        DO 130 KFL=1,5
        XPPI(-KFL)=XPPI(KFL)
  130   CONTINUE
        XPPI(2)=XPPI(2)+XFVAL
        XPPI(-1)=XPPI(-1)+XFVAL
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSTPR(X,Q2,XPPR)
 
C...Gives proton structure functions according to a few different
C...parametrizations.
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
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION XPPR(-6:6),XQ(9),TX(6),TT(6),TS(6),NEHLQ(8,2),
     &CEHLQ(6,6,2,8,2),CDO(3,6,5,2)
 
 
C...The following data lines are coefficients needed in the
C...Eichten, Hinchliffe, Lane, Quigg proton structure function
C...parametrizations, see below.
C...Powers of 1-x in different cases.
      DATA NEHLQ/3,4,7,5,7,7,7,7,3,4,7,6,7,7,7,7/
C...Expansion coefficients for up valence quark distribution.
      DATA (((CEHLQ(IX,IT,NX,1,1),IX=1,6),IT=1,6),NX=1,2)/
     1 7.677E-01,-2.087E-01,-3.303E-01,-2.517E-02,-1.570E-02,-1.000E-04,
     2-5.326E-01,-2.661E-01, 3.201E-01, 1.192E-01, 2.434E-02, 7.620E-03,
     3 2.162E-01, 1.881E-01,-8.375E-02,-6.515E-02,-1.743E-02,-5.040E-03,
     4-9.211E-02,-9.952E-02, 1.373E-02, 2.506E-02, 8.770E-03, 2.550E-03,
     5 3.670E-02, 4.409E-02, 9.600E-04,-7.960E-03,-3.420E-03,-1.050E-03,
     6-1.549E-02,-2.026E-02,-3.060E-03, 2.220E-03, 1.240E-03, 4.100E-04,
     1 2.395E-01, 2.905E-01, 9.778E-02, 2.149E-02, 3.440E-03, 5.000E-04,
     2 1.751E-02,-6.090E-03,-2.687E-02,-1.916E-02,-7.970E-03,-2.750E-03,
     3-5.760E-03,-5.040E-03, 1.080E-03, 2.490E-03, 1.530E-03, 7.500E-04,
     4 1.740E-03, 1.960E-03, 3.000E-04,-3.400E-04,-2.900E-04,-1.800E-04,
     5-5.300E-04,-6.400E-04,-1.700E-04, 4.000E-05, 6.000E-05, 4.000E-05,
     6 1.700E-04, 2.200E-04, 8.000E-05, 1.000E-05,-1.000E-05,-1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,1,2),IX=1,6),IT=1,6),NX=1,2)/
     1 7.237E-01,-2.189E-01,-2.995E-01,-1.909E-02,-1.477E-02, 2.500E-04,
     2-5.314E-01,-2.425E-01, 3.283E-01, 1.119E-01, 2.223E-02, 7.070E-03,
     3 2.289E-01, 1.890E-01,-9.859E-02,-6.900E-02,-1.747E-02,-5.080E-03,
     4-1.041E-01,-1.084E-01, 2.108E-02, 2.975E-02, 9.830E-03, 2.830E-03,
     5 4.394E-02, 5.116E-02,-1.410E-03,-1.055E-02,-4.230E-03,-1.270E-03,
     6-1.991E-02,-2.539E-02,-2.780E-03, 3.430E-03, 1.720E-03, 5.500E-04,
     1 2.410E-01, 2.884E-01, 9.369E-02, 1.900E-02, 2.530E-03, 2.400E-04,
     2 1.765E-02,-9.220E-03,-3.037E-02,-2.085E-02,-8.440E-03,-2.810E-03,
     3-6.450E-03,-5.260E-03, 1.720E-03, 3.110E-03, 1.830E-03, 8.700E-04,
     4 2.120E-03, 2.320E-03, 2.600E-04,-4.900E-04,-3.900E-04,-2.300E-04,
     5-6.900E-04,-8.200E-04,-2.000E-04, 7.000E-05, 9.000E-05, 6.000E-05,
     6 2.400E-04, 3.100E-04, 1.100E-04, 0.000E+00,-2.000E-05,-2.000E-05/
C...Expansion coefficients for down valence quark distribution.
      DATA (((CEHLQ(IX,IT,NX,2,1),IX=1,6),IT=1,6),NX=1,2)/
     1 3.813E-01,-8.090E-02,-1.634E-01,-2.185E-02,-8.430E-03,-6.200E-04,
     2-2.948E-01,-1.435E-01, 1.665E-01, 6.638E-02, 1.473E-02, 4.080E-03,
     3 1.252E-01, 1.042E-01,-4.722E-02,-3.683E-02,-1.038E-02,-2.860E-03,
     4-5.478E-02,-5.678E-02, 8.900E-03, 1.484E-02, 5.340E-03, 1.520E-03,
     5 2.220E-02, 2.567E-02,-3.000E-05,-4.970E-03,-2.160E-03,-6.500E-04,
     6-9.530E-03,-1.204E-02,-1.510E-03, 1.510E-03, 8.300E-04, 2.700E-04,
     1 1.261E-01, 1.354E-01, 3.958E-02, 8.240E-03, 1.660E-03, 4.500E-04,
     2 3.890E-03,-1.159E-02,-1.625E-02,-9.610E-03,-3.710E-03,-1.260E-03,
     3-1.910E-03,-5.600E-04, 1.590E-03, 1.590E-03, 8.400E-04, 3.900E-04,
     4 6.400E-04, 4.900E-04,-1.500E-04,-2.900E-04,-1.800E-04,-1.000E-04,
     5-2.000E-04,-1.900E-04, 0.000E+00, 6.000E-05, 4.000E-05, 3.000E-05,
     6 7.000E-05, 8.000E-05, 2.000E-05,-1.000E-05,-1.000E-05,-1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,2,2),IX=1,6),IT=1,6),NX=1,2)/
     1 3.578E-01,-8.622E-02,-1.480E-01,-1.840E-02,-7.820E-03,-4.500E-04,
     2-2.925E-01,-1.304E-01, 1.696E-01, 6.243E-02, 1.353E-02, 3.750E-03,
     3 1.318E-01, 1.041E-01,-5.486E-02,-3.872E-02,-1.038E-02,-2.850E-03,
     4-6.162E-02,-6.143E-02, 1.303E-02, 1.740E-02, 5.940E-03, 1.670E-03,
     5 2.643E-02, 2.957E-02,-1.490E-03,-6.450E-03,-2.630E-03,-7.700E-04,
     6-1.218E-02,-1.497E-02,-1.260E-03, 2.240E-03, 1.120E-03, 3.500E-04,
     1 1.263E-01, 1.334E-01, 3.732E-02, 7.070E-03, 1.260E-03, 3.400E-04,
     2 3.660E-03,-1.357E-02,-1.795E-02,-1.031E-02,-3.880E-03,-1.280E-03,
     3-2.100E-03,-3.600E-04, 2.050E-03, 1.920E-03, 9.800E-04, 4.400E-04,
     4 7.700E-04, 5.400E-04,-2.400E-04,-3.900E-04,-2.400E-04,-1.300E-04,
     5-2.600E-04,-2.300E-04, 2.000E-05, 9.000E-05, 6.000E-05, 4.000E-05,
     6 9.000E-05, 1.000E-04, 2.000E-05,-2.000E-05,-2.000E-05,-1.000E-05/
C...Expansion coefficients for up and down sea quark distributions.
      DATA (((CEHLQ(IX,IT,NX,3,1),IX=1,6),IT=1,6),NX=1,2)/
     1 6.870E-02,-6.861E-02, 2.973E-02,-5.400E-03, 3.780E-03,-9.700E-04,
     2-1.802E-02, 1.400E-04, 6.490E-03,-8.540E-03, 1.220E-03,-1.750E-03,
     3-4.650E-03, 1.480E-03,-5.930E-03, 6.000E-04,-1.030E-03,-8.000E-05,
     4 6.440E-03, 2.570E-03, 2.830E-03, 1.150E-03, 7.100E-04, 3.300E-04,
     5-3.930E-03,-2.540E-03,-1.160E-03,-7.700E-04,-3.600E-04,-1.900E-04,
     6 2.340E-03, 1.930E-03, 5.300E-04, 3.700E-04, 1.600E-04, 9.000E-05,
     1 1.014E+00,-1.106E+00, 3.374E-01,-7.444E-02, 8.850E-03,-8.700E-04,
     2 9.233E-01,-1.285E+00, 4.475E-01,-9.786E-02, 1.419E-02,-1.120E-03,
     3 4.888E-02,-1.271E-01, 8.606E-02,-2.608E-02, 4.780E-03,-6.000E-04,
     4-2.691E-02, 4.887E-02,-1.771E-02, 1.620E-03, 2.500E-04,-6.000E-05,
     5 7.040E-03,-1.113E-02, 1.590E-03, 7.000E-04,-2.000E-04, 0.000E+00,
     6-1.710E-03, 2.290E-03, 3.800E-04,-3.500E-04, 4.000E-05, 1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,3,2),IX=1,6),IT=1,6),NX=1,2)/
     1 1.008E-01,-7.100E-02, 1.973E-02,-5.710E-03, 2.930E-03,-9.900E-04,
     2-5.271E-02,-1.823E-02, 1.792E-02,-6.580E-03, 1.750E-03,-1.550E-03,
     3 1.220E-02, 1.763E-02,-8.690E-03,-8.800E-04,-1.160E-03,-2.100E-04,
     4-1.190E-03,-7.180E-03, 2.360E-03, 1.890E-03, 7.700E-04, 4.100E-04,
     5-9.100E-04, 2.040E-03,-3.100E-04,-1.050E-03,-4.000E-04,-2.400E-04,
     6 1.190E-03,-1.700E-04,-2.000E-04, 4.200E-04, 1.700E-04, 1.000E-04,
     1 1.081E+00,-1.189E+00, 3.868E-01,-8.617E-02, 1.115E-02,-1.180E-03,
     2 9.917E-01,-1.396E+00, 4.998E-01,-1.159E-01, 1.674E-02,-1.720E-03,
     3 5.099E-02,-1.338E-01, 9.173E-02,-2.885E-02, 5.890E-03,-6.500E-04,
     4-3.178E-02, 5.703E-02,-2.070E-02, 2.440E-03, 1.100E-04,-9.000E-05,
     5 8.970E-03,-1.392E-02, 2.050E-03, 6.500E-04,-2.300E-04, 2.000E-05,
     6-2.340E-03, 3.010E-03, 5.000E-04,-3.900E-04, 6.000E-05, 1.000E-05/
C...Expansion coefficients for gluon distribution.
      DATA (((CEHLQ(IX,IT,NX,4,1),IX=1,6),IT=1,6),NX=1,2)/
     1 9.482E-01,-9.578E-01, 1.009E-01,-1.051E-01, 3.456E-02,-3.054E-02,
     2-9.627E-01, 5.379E-01, 3.368E-01,-9.525E-02, 1.488E-02,-2.051E-02,
     3 4.300E-01,-8.306E-02,-3.372E-01, 4.902E-02,-9.160E-03, 1.041E-02,
     4-1.925E-01,-1.790E-02, 2.183E-01, 7.490E-03, 4.140E-03,-1.860E-03,
     5 8.183E-02, 1.926E-02,-1.072E-01,-1.944E-02,-2.770E-03,-5.200E-04,
     6-3.884E-02,-1.234E-02, 5.410E-02, 1.879E-02, 3.350E-03, 1.040E-03,
     1 2.948E+01,-3.902E+01, 1.464E+01,-3.335E+00, 5.054E-01,-5.915E-02,
     2 2.559E+01,-3.955E+01, 1.661E+01,-4.299E+00, 6.904E-01,-8.243E-02,
     3-1.663E+00, 1.176E+00, 1.118E+00,-7.099E-01, 1.948E-01,-2.404E-02,
     4-2.168E-01, 8.170E-01,-7.169E-01, 1.851E-01,-1.924E-02,-3.250E-03,
     5 2.088E-01,-4.355E-01, 2.239E-01,-2.446E-02,-3.620E-03, 1.910E-03,
     6-9.097E-02, 1.601E-01,-5.681E-02,-2.500E-03, 2.580E-03,-4.700E-04/
      DATA (((CEHLQ(IX,IT,NX,4,2),IX=1,6),IT=1,6),NX=1,2)/
     1 2.367E+00, 4.453E-01, 3.660E-01, 9.467E-02, 1.341E-01, 1.661E-02,
     2-3.170E+00,-1.795E+00, 3.313E-02,-2.874E-01,-9.827E-02,-7.119E-02,
     3 1.823E+00, 1.457E+00,-2.465E-01, 3.739E-02, 6.090E-03, 1.814E-02,
     4-1.033E+00,-9.827E-01, 2.136E-01, 1.169E-01, 5.001E-02, 1.684E-02,
     5 5.133E-01, 5.259E-01,-1.173E-01,-1.139E-01,-4.988E-02,-2.021E-02,
     6-2.881E-01,-3.145E-01, 5.667E-02, 9.161E-02, 4.568E-02, 1.951E-02,
     1 3.036E+01,-4.062E+01, 1.578E+01,-3.699E+00, 6.020E-01,-7.031E-02,
     2 2.700E+01,-4.167E+01, 1.770E+01,-4.804E+00, 7.862E-01,-1.060E-01,
     3-1.909E+00, 1.357E+00, 1.127E+00,-7.181E-01, 2.232E-01,-2.481E-02,
     4-2.488E-01, 9.781E-01,-8.127E-01, 2.094E-01,-2.997E-02,-4.710E-03,
     5 2.506E-01,-5.427E-01, 2.672E-01,-3.103E-02,-1.800E-03, 2.870E-03,
     6-1.128E-01, 2.087E-01,-6.972E-02,-2.480E-03, 2.630E-03,-8.400E-04/
C...Expansion coefficients for strange sea quark distribution.
      DATA (((CEHLQ(IX,IT,NX,5,1),IX=1,6),IT=1,6),NX=1,2)/
     1 4.968E-02,-4.173E-02, 2.102E-02,-3.270E-03, 3.240E-03,-6.700E-04,
     2-6.150E-03,-1.294E-02, 6.740E-03,-6.890E-03, 9.000E-04,-1.510E-03,
     3-8.580E-03, 5.050E-03,-4.900E-03,-1.600E-04,-9.400E-04,-1.500E-04,
     4 7.840E-03, 1.510E-03, 2.220E-03, 1.400E-03, 7.000E-04, 3.500E-04,
     5-4.410E-03,-2.220E-03,-8.900E-04,-8.500E-04,-3.600E-04,-2.000E-04,
     6 2.520E-03, 1.840E-03, 4.100E-04, 3.900E-04, 1.600E-04, 9.000E-05,
     1 9.235E-01,-1.085E+00, 3.464E-01,-7.210E-02, 9.140E-03,-9.100E-04,
     2 9.315E-01,-1.274E+00, 4.512E-01,-9.775E-02, 1.380E-02,-1.310E-03,
     3 4.739E-02,-1.296E-01, 8.482E-02,-2.642E-02, 4.760E-03,-5.700E-04,
     4-2.653E-02, 4.953E-02,-1.735E-02, 1.750E-03, 2.800E-04,-6.000E-05,
     5 6.940E-03,-1.132E-02, 1.480E-03, 6.500E-04,-2.100E-04, 0.000E+00,
     6-1.680E-03, 2.340E-03, 4.200E-04,-3.400E-04, 5.000E-05, 1.000E-05/
      DATA (((CEHLQ(IX,IT,NX,5,2),IX=1,6),IT=1,6),NX=1,2)/
     1 6.478E-02,-4.537E-02, 1.643E-02,-3.490E-03, 2.710E-03,-6.700E-04,
     2-2.223E-02,-2.126E-02, 1.247E-02,-6.290E-03, 1.120E-03,-1.440E-03,
     3-1.340E-03, 1.362E-02,-6.130E-03,-7.900E-04,-9.000E-04,-2.000E-04,
     4 5.080E-03,-3.610E-03, 1.700E-03, 1.830E-03, 6.800E-04, 4.000E-04,
     5-3.580E-03, 6.000E-05,-2.600E-04,-1.050E-03,-3.800E-04,-2.300E-04,
     6 2.420E-03, 9.300E-04,-1.000E-04, 4.500E-04, 1.700E-04, 1.100E-04,
     1 9.868E-01,-1.171E+00, 3.940E-01,-8.459E-02, 1.124E-02,-1.250E-03,
     2 1.001E+00,-1.383E+00, 5.044E-01,-1.152E-01, 1.658E-02,-1.830E-03,
     3 4.928E-02,-1.368E-01, 9.021E-02,-2.935E-02, 5.800E-03,-6.600E-04,
     4-3.133E-02, 5.785E-02,-2.023E-02, 2.630E-03, 1.600E-04,-8.000E-05,
     5 8.840E-03,-1.416E-02, 1.900E-03, 5.800E-04,-2.500E-04, 1.000E-05,
     6-2.300E-03, 3.080E-03, 5.500E-04,-3.700E-04, 7.000E-05, 1.000E-05/
C...Expansion coefficients for charm sea quark distribution.
      DATA (((CEHLQ(IX,IT,NX,6,1),IX=1,6),IT=1,6),NX=1,2)/
     1 9.270E-03,-1.817E-02, 9.590E-03,-6.390E-03, 1.690E-03,-1.540E-03,
     2 5.710E-03,-1.188E-02, 6.090E-03,-4.650E-03, 1.240E-03,-1.310E-03,
     3-3.960E-03, 7.100E-03,-3.590E-03, 1.840E-03,-3.900E-04, 3.400E-04,
     4 1.120E-03,-1.960E-03, 1.120E-03,-4.800E-04, 1.000E-04,-4.000E-05,
     5 4.000E-05,-3.000E-05,-1.800E-04, 9.000E-05,-5.000E-05,-2.000E-05,
     6-4.200E-04, 7.300E-04,-1.600E-04, 5.000E-05, 5.000E-05, 5.000E-05,
     1 8.098E-01,-1.042E+00, 3.398E-01,-6.824E-02, 8.760E-03,-9.000E-04,
     2 8.961E-01,-1.217E+00, 4.339E-01,-9.287E-02, 1.304E-02,-1.290E-03,
     3 3.058E-02,-1.040E-01, 7.604E-02,-2.415E-02, 4.600E-03,-5.000E-04,
     4-2.451E-02, 4.432E-02,-1.651E-02, 1.430E-03, 1.200E-04,-1.000E-04,
     5 1.122E-02,-1.457E-02, 2.680E-03, 5.800E-04,-1.200E-04, 3.000E-05,
     6-7.730E-03, 7.330E-03,-7.600E-04,-2.400E-04, 1.000E-05, 0.000E+00/
      DATA (((CEHLQ(IX,IT,NX,6,2),IX=1,6),IT=1,6),NX=1,2)/
     1 9.980E-03,-1.945E-02, 1.055E-02,-6.870E-03, 1.860E-03,-1.560E-03,
     2 5.700E-03,-1.203E-02, 6.250E-03,-4.860E-03, 1.310E-03,-1.370E-03,
     3-4.490E-03, 7.990E-03,-4.170E-03, 2.050E-03,-4.400E-04, 3.300E-04,
     4 1.470E-03,-2.480E-03, 1.460E-03,-5.700E-04, 1.200E-04,-1.000E-05,
     5-9.000E-05, 1.500E-04,-3.200E-04, 1.200E-04,-6.000E-05,-4.000E-05,
     6-4.200E-04, 7.600E-04,-1.400E-04, 4.000E-05, 7.000E-05, 5.000E-05,
     1 8.698E-01,-1.131E+00, 3.836E-01,-8.111E-02, 1.048E-02,-1.300E-03,
     2 9.626E-01,-1.321E+00, 4.854E-01,-1.091E-01, 1.583E-02,-1.700E-03,
     3 3.057E-02,-1.088E-01, 8.022E-02,-2.676E-02, 5.590E-03,-5.600E-04,
     4-2.845E-02, 5.164E-02,-1.918E-02, 2.210E-03,-4.000E-05,-1.500E-04,
     5 1.311E-02,-1.751E-02, 3.310E-03, 5.100E-04,-1.200E-04, 5.000E-05,
     6-8.590E-03, 8.380E-03,-9.200E-04,-2.600E-04, 1.000E-05,-1.000E-05/
C...Expansion coefficients for bottom sea quark distribution.
      DATA (((CEHLQ(IX,IT,NX,7,1),IX=1,6),IT=1,6),NX=1,2)/
     1 9.010E-03,-1.401E-02, 7.150E-03,-4.130E-03, 1.260E-03,-1.040E-03,
     2 6.280E-03,-9.320E-03, 4.780E-03,-2.890E-03, 9.100E-04,-8.200E-04,
     3-2.930E-03, 4.090E-03,-1.890E-03, 7.600E-04,-2.300E-04, 1.400E-04,
     4 3.900E-04,-1.200E-03, 4.400E-04,-2.500E-04, 2.000E-05,-2.000E-05,
     5 2.600E-04, 1.400E-04,-8.000E-05, 1.000E-04, 1.000E-05, 1.000E-05,
     6-2.600E-04, 3.200E-04, 1.000E-05,-1.000E-05, 1.000E-05,-1.000E-05,
     1 8.029E-01,-1.075E+00, 3.792E-01,-7.843E-02, 1.007E-02,-1.090E-03,
     2 7.903E-01,-1.099E+00, 4.153E-01,-9.301E-02, 1.317E-02,-1.410E-03,
     3-1.704E-02,-1.130E-02, 2.882E-02,-1.341E-02, 3.040E-03,-3.600E-04,
     4-7.200E-04, 7.230E-03,-5.160E-03, 1.080E-03,-5.000E-05,-4.000E-05,
     5 3.050E-03,-4.610E-03, 1.660E-03,-1.300E-04,-1.000E-05, 1.000E-05,
     6-4.360E-03, 5.230E-03,-1.610E-03, 2.000E-04,-2.000E-05, 0.000E+00/
      DATA (((CEHLQ(IX,IT,NX,7,2),IX=1,6),IT=1,6),NX=1,2)/
     1 8.980E-03,-1.459E-02, 7.510E-03,-4.410E-03, 1.310E-03,-1.070E-03,
     2 5.970E-03,-9.440E-03, 4.800E-03,-3.020E-03, 9.100E-04,-8.500E-04,
     3-3.050E-03, 4.440E-03,-2.100E-03, 8.500E-04,-2.400E-04, 1.400E-04,
     4 5.300E-04,-1.300E-03, 5.600E-04,-2.700E-04, 3.000E-05,-2.000E-05,
     5 2.000E-04, 1.400E-04,-1.100E-04, 1.000E-04, 0.000E+00, 0.000E+00,
     6-2.600E-04, 3.200E-04, 0.000E+00,-3.000E-05, 1.000E-05,-1.000E-05,
     1 8.672E-01,-1.174E+00, 4.265E-01,-9.252E-02, 1.244E-02,-1.460E-03,
     2 8.500E-01,-1.194E+00, 4.630E-01,-1.083E-01, 1.614E-02,-1.830E-03,
     3-2.241E-02,-5.630E-03, 2.815E-02,-1.425E-02, 3.520E-03,-4.300E-04,
     4-7.300E-04, 8.030E-03,-5.780E-03, 1.380E-03,-1.300E-04,-4.000E-05,
     5 3.460E-03,-5.380E-03, 1.960E-03,-2.100E-04, 1.000E-05, 1.000E-05,
     6-4.850E-03, 5.950E-03,-1.890E-03, 2.600E-04,-3.000E-05, 0.000E+00/
C...Expansion coefficients for top sea quark distribution.
      DATA (((CEHLQ(IX,IT,NX,8,1),IX=1,6),IT=1,6),NX=1,2)/
     1 4.410E-03,-7.480E-03, 3.770E-03,-2.580E-03, 7.300E-04,-7.100E-04,
     2 3.840E-03,-6.050E-03, 3.030E-03,-2.030E-03, 5.800E-04,-5.900E-04,
     3-8.800E-04, 1.660E-03,-7.500E-04, 4.700E-04,-1.000E-04, 1.000E-04,
     4-8.000E-05,-1.500E-04, 1.200E-04,-9.000E-05, 3.000E-05, 0.000E+00,
     5 1.300E-04,-2.200E-04,-2.000E-05,-2.000E-05,-2.000E-05,-2.000E-05,
     6-7.000E-05, 1.900E-04,-4.000E-05, 2.000E-05, 0.000E+00, 0.000E+00,
     1 6.623E-01,-9.248E-01, 3.519E-01,-7.930E-02, 1.110E-02,-1.180E-03,
     2 6.380E-01,-9.062E-01, 3.582E-01,-8.479E-02, 1.265E-02,-1.390E-03,
     3-2.581E-02, 2.125E-02, 4.190E-03,-4.980E-03, 1.490E-03,-2.100E-04,
     4 7.100E-04, 5.300E-04,-1.270E-03, 3.900E-04,-5.000E-05,-1.000E-05,
     5 3.850E-03,-5.060E-03, 1.860E-03,-3.500E-04, 4.000E-05, 0.000E+00,
     6-3.530E-03, 4.460E-03,-1.500E-03, 2.700E-04,-3.000E-05, 0.000E+00/
      DATA (((CEHLQ(IX,IT,NX,8,2),IX=1,6),IT=1,6),NX=1,2)/
     1 4.260E-03,-7.530E-03, 3.830E-03,-2.680E-03, 7.600E-04,-7.300E-04,
     2 3.640E-03,-6.050E-03, 3.030E-03,-2.090E-03, 5.900E-04,-6.000E-04,
     3-9.200E-04, 1.710E-03,-8.200E-04, 5.000E-04,-1.200E-04, 1.000E-04,
     4-5.000E-05,-1.600E-04, 1.300E-04,-9.000E-05, 3.000E-05, 0.000E+00,
     5 1.300E-04,-2.100E-04,-1.000E-05,-2.000E-05,-2.000E-05,-1.000E-05,
     6-8.000E-05, 1.800E-04,-5.000E-05, 2.000E-05, 0.000E+00, 0.000E+00,
     1 7.146E-01,-1.007E+00, 3.932E-01,-9.246E-02, 1.366E-02,-1.540E-03,
     2 6.856E-01,-9.828E-01, 3.977E-01,-9.795E-02, 1.540E-02,-1.790E-03,
     3-3.053E-02, 2.758E-02, 2.150E-03,-4.880E-03, 1.640E-03,-2.500E-04,
     4 9.200E-04, 4.200E-04,-1.340E-03, 4.600E-04,-8.000E-05,-1.000E-05,
     5 4.230E-03,-5.660E-03, 2.140E-03,-4.300E-04, 6.000E-05, 0.000E+00,
     6-3.890E-03, 5.000E-03,-1.740E-03, 3.300E-04,-4.000E-05, 0.000E+00/
 
C...The following data lines are coefficients needed in the
C...Duke, Owens proton structure function parametrizations, see below.
C...Expansion coefficients for (up+down) valence quark distribution.
      DATA ((CDO(IP,IS,1,1),IS=1,6),IP=1,3)/
     1 4.190E-01, 3.460E+00, 4.400E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2 4.000E-03, 7.240E-01,-4.860E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     3-7.000E-03,-6.600E-02, 1.330E+00, 0.000E+00, 0.000E+00, 0.000E+00/
      DATA ((CDO(IP,IS,1,2),IS=1,6),IP=1,3)/
     1 3.740E-01, 3.330E+00, 6.030E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2 1.400E-02, 7.530E-01,-6.220E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     3 0.000E+00,-7.600E-02, 1.560E+00, 0.000E+00, 0.000E+00, 0.000E+00/
C...Expansion coefficients for down valence quark distribution.
      DATA ((CDO(IP,IS,2,1),IS=1,6),IP=1,3)/
     1 7.630E-01, 4.000E+00, 0.000E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2-2.370E-01, 6.270E-01,-4.210E-01, 0.000E+00, 0.000E+00, 0.000E+00,
     3 2.600E-02,-1.900E-02, 3.300E-02, 0.000E+00, 0.000E+00, 0.000E+00/
      DATA ((CDO(IP,IS,2,2),IS=1,6),IP=1,3)/
     1 7.610E-01, 3.830E+00, 0.000E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2-2.320E-01, 6.270E-01,-4.180E-01, 0.000E+00, 0.000E+00, 0.000E+00,
     3 2.300E-02,-1.900E-02, 3.600E-02, 0.000E+00, 0.000E+00, 0.000E+00/
C...Expansion coefficients for (up+down+strange) sea quark distribution.
      DATA ((CDO(IP,IS,3,1),IS=1,6),IP=1,3)/
     1 1.265E+00, 0.000E+00, 8.050E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2-1.132E+00,-3.720E-01, 1.590E+00, 6.310E+00,-1.050E+01, 1.470E+01,
     3 2.930E-01,-2.900E-02,-1.530E-01,-2.730E-01,-3.170E+00, 9.800E+00/
      DATA ((CDO(IP,IS,3,2),IS=1,6),IP=1,3)/
     1 1.670E+00, 0.000E+00, 9.150E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2-1.920E+00,-2.730E-01, 5.300E-01, 1.570E+01,-1.010E+02, 2.230E+02,
     3 5.820E-01,-1.640E-01,-7.630E-01,-2.830E+00, 4.470E+01,-1.170E+02/
C...Expansion coefficients for charm sea quark distribution.
      DATA ((CDO(IP,IS,4,1),IS=1,6),IP=1,3)/
     1 0.000E+00,-3.600E-02, 6.350E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2 1.350E-01,-2.220E-01, 3.260E+00,-3.030E+00, 1.740E+01,-1.790E+01,
     3-7.500E-02,-5.800E-02,-9.090E-01, 1.500E+00,-1.130E+01, 1.560E+01/
       DATA ((CDO(IP,IS,4,2),IS=1,6),IP=1,3)/
     1 0.000E+00,-1.200E-01, 3.510E+00, 0.000E+00, 0.000E+00, 0.000E+00,
     2 6.700E-02,-2.330E-01, 3.660E+00,-4.740E-01, 9.500E+00,-1.660E+01,
     3-3.100E-02,-2.300E-02,-4.530E-01, 3.580E-01,-5.430E+00, 1.550E+01/
C...Expansion coefficients for gluon distribution.
      DATA ((CDO(IP,IS,5,1),IS=1,6),IP=1,3)/
     1 1.560E+00, 0.000E+00, 6.000E+00, 9.000E+00, 0.000E+00, 0.000E+00,
     2-1.710E+00,-9.490E-01, 1.440E+00,-7.190E+00,-1.650E+01, 1.530E+01,
     3 6.380E-01, 3.250E-01,-1.050E+00, 2.550E-01, 1.090E+01,-1.010E+01/
      DATA ((CDO(IP,IS,5,2),IS=1,6),IP=1,3)/
     1 8.790E-01, 0.000E+00, 4.000E+00, 9.000E+00, 0.000E+00, 0.000E+00,
     2-9.710E-01,-1.160E+00, 1.230E+00,-5.640E+00,-7.540E+00,-5.960E-01,
     3 4.340E-01, 4.760E-01,-2.540E-01,-8.170E-01, 5.500E+00, 1.260E-01/
 
C...Euler's beta function, requires ordinary Gamma function
      EULBET(X,Y)=PYGAMM(X)*PYGAMM(Y)/PYGAMM(X+Y)
 
C...Reset output array.
      DO 100 KFL=-6,6
      XPPR(KFL)=0.
  100 CONTINUE
 
      IF(MSTP(51).EQ.1.OR.MSTP(51).EQ.2) THEN
C...Proton structure functions from Eichten, Hinchliffe, Lane, Quigg.
C...Allowed variable range: 5 GeV^2 < Q^2 < 1E8 GeV^2; 1E-4 < x < 1
 
C...Determine set, Lambda and x and t expansion variables.
        NSET=MSTP(51)
        IF(NSET.EQ.1) ALAM=0.2
        IF(NSET.EQ.2) ALAM=0.29
        VINT(231)=5.
        TMIN=LOG(5./ALAM**2)
        TMAX=LOG(1E8/ALAM**2)
        IF(MSTP(57).EQ.0) THEN
          T=TMIN
        ELSE
          T=LOG(MAX(1.,Q2/ALAM**2))
        ENDIF
        VT=MAX(-1.,MIN(1.,(2.*T-TMAX-TMIN)/(TMAX-TMIN)))
        NX=1
        IF(X.LE.0.1) NX=2
        IF(NX.EQ.1) VX=(2.*X-1.1)/0.9
        IF(NX.EQ.2) VX=MAX(-1.,(2.*LOG(X)+11.51293)/6.90776)
        CXS=1.
        IF(X.LT.1E-4.AND.ABS(PARP(51)-1.).GT.0.01) CXS=
     &  (1E-4/X)**(PARP(51)-1.)
 
C...Chebyshev polynomials for x and t expansion.
        TX(1)=1.
        TX(2)=VX
        TX(3)=2.*VX**2-1.
        TX(4)=4.*VX**3-3.*VX
        TX(5)=8.*VX**4-8.*VX**2+1.
        TX(6)=16.*VX**5-20.*VX**3+5.*VX
        TT(1)=1.
        TT(2)=VT
        TT(3)=2.*VT**2-1.
        TT(4)=4.*VT**3-3.*VT
        TT(5)=8.*VT**4-8.*VT**2+1.
        TT(6)=16.*VT**5-20.*VT**3+5.*VT
 
C...Calculate structure functions.
        DO 130 KFL=1,6
        XQSUM=0.
        DO 120 IT=1,6
        DO 110 IX=1,6
        XQSUM=XQSUM+CEHLQ(IX,IT,NX,KFL,NSET)*TX(IX)*TT(IT)
  110   CONTINUE
  120   CONTINUE
       XQ(KFL)=XQSUM*(1.-X)**NEHLQ(KFL,NSET)*CXS
  130   CONTINUE
 
C...Put into output array.
        XPPR(0)=XQ(4)
        XPPR(1)=XQ(2)+XQ(3)
        XPPR(2)=XQ(1)+XQ(3)
        XPPR(3)=XQ(5)
        XPPR(4)=XQ(6)
        XPPR(-1)=XQ(3)
        XPPR(-2)=XQ(3)
        XPPR(-3)=XQ(5)
        XPPR(-4)=XQ(6)
 
C...Special expansion for bottom (threshold effects).
        IF(MSTP(58).GE.5) THEN
          IF(NSET.EQ.1) TMIN=8.1905
          IF(NSET.EQ.2) TMIN=7.4474
          IF(T.GT.TMIN) THEN
            VT=MAX(-1.,MIN(1.,(2.*T-TMAX-TMIN)/(TMAX-TMIN)))
            TT(1)=1.
            TT(2)=VT
            TT(3)=2.*VT**2-1.
            TT(4)=4.*VT**3-3.*VT
            TT(5)=8.*VT**4-8.*VT**2+1.
            TT(6)=16.*VT**5-20.*VT**3+5.*VT
            XQSUM=0.
            DO 150 IT=1,6
            DO 140 IX=1,6
            XQSUM=XQSUM+CEHLQ(IX,IT,NX,7,NSET)*TX(IX)*TT(IT)
  140       CONTINUE
  150       CONTINUE
            XPPR(5)=XQSUM*(1.-X)**NEHLQ(7,NSET)*CXS
            XPPR(-5)=XPPR(5)
          ENDIF
        ENDIF
 
C...Special expansion for top (threshold effects).
        IF(MSTP(58).GE.6) THEN
          IF(NSET.EQ.1) TMIN=11.5528
          IF(NSET.EQ.2) TMIN=10.8097
          TMIN=TMIN+2.*LOG(PMAS(6,1)/30.)
          TMAX=TMAX+2.*LOG(PMAS(6,1)/30.)
          IF(T.GT.TMIN) THEN
            VT=MAX(-1.,MIN(1.,(2.*T-TMAX-TMIN)/(TMAX-TMIN)))
            TT(1)=1.
            TT(2)=VT
            TT(3)=2.*VT**2-1.
            TT(4)=4.*VT**3-3.*VT
            TT(5)=8.*VT**4-8.*VT**2+1.
            TT(6)=16.*VT**5-20.*VT**3+5.*VT
            XQSUM=0.
            DO 170 IT=1,6
            DO 160 IX=1,6
            XQSUM=XQSUM+CEHLQ(IX,IT,NX,8,NSET)*TX(IX)*TT(IT)
  160       CONTINUE
  170       CONTINUE
            XPPR(6)=XQSUM*(1.-X)**NEHLQ(8,NSET)*CXS
            XPPR(-6)=XPPR(6)
          ENDIF
        ENDIF
 
      ELSEIF(MSTP(51).EQ.3.OR.MSTP(51).EQ.4) THEN
C...Proton structure functions from Duke, Owens.
C...Allowed variable range: 4 GeV^2 < Q^2 < approx 1E6 GeV^2.
 
C...Determine set, Lambda and s expansion parameter.
        NSET=MSTP(51)-2
        IF(NSET.EQ.1) ALAM=0.2
        IF(NSET.EQ.2) ALAM=0.4
        VINT(231)=4.
        IF(MSTP(57).LE.0) THEN
          SD=0.
        ELSE
          Q2IN=MIN(1E6,MAX(4.,Q2))
          SD=LOG(LOG(Q2IN/ALAM**2)/LOG(4./ALAM**2))
        ENDIF
 
C...Calculate structure functions.
        DO 190 KFL=1,5
        DO 180 IS=1,6
        TS(IS)=CDO(1,IS,KFL,NSET)+CDO(2,IS,KFL,NSET)*SD+
     &  CDO(3,IS,KFL,NSET)*SD**2
  180   CONTINUE
        IF(KFL.LE.2) THEN
          XQ(KFL)=X**TS(1)*(1.-X)**TS(2)*(1.+TS(3)*X)/(EULBET(TS(1),
     &    TS(2)+1.)*(1.+TS(3)*TS(1)/(TS(1)+TS(2)+1.)))
        ELSE
          XQ(KFL)=TS(1)*X**TS(2)*(1.-X)**TS(3)*(1.+TS(4)*X+TS(5)*X**2+
     &    TS(6)*X**3)
        ENDIF
  190   CONTINUE
 
C...Put into output arrays.
        XPPR(0)=XQ(5)
        XPPR(1)=XQ(2)+XQ(3)/6.
        XPPR(2)=3.*XQ(1)-XQ(2)+XQ(3)/6.
        XPPR(3)=XQ(3)/6.
        XPPR(4)=XQ(4)
        XPPR(-1)=XQ(3)/6.
        XPPR(-2)=XQ(3)/6.
        XPPR(-3)=XQ(3)/6.
        XPPR(-4)=XQ(4)
 
      ELSEIF(MSTP(51).GE.5.AND.MSTP(51).LE.10) THEN
C...Interface to the CTEQ 2 structure functions.
        NSET=MSTP(51)-4
        QRT=SQRT(MAX(1.,Q2))
 
C...Loop over flavours; put u and d in right order.
        DO 200 I=-6,2
        KFL=I
        IF(I.EQ.1) KFL=2
        IF(I.EQ.2) KFL=1
        IF(I.EQ.-1) KFL=-2
        IF(I.EQ.-2) KFL=-1
        IF(I.LE.0) THEN
          XPPR(KFL)=PYCTQ2(NSET,I,X,QRT)
          XPPR(-KFL)=XPPR(KFL)
        ELSE
          XPPR(KFL)=PYCTQ2(NSET,I,X,QRT)+XPPR(-KFL)
        ENDIF
  200   CONTINUE
 
C...Leading order proton structure functions from Gluck, Reya and Vogt.
C...Allowed variable range: 0.25 GeV^2 < Q^2 < 10^8 GeV^2 and
C...10^-5 < x < 1.
      ELSE
 
C...Determine s expansion variable and some x expressions.
        VINT(231)=0.25
        IF(MSTP(57).LE.0) THEN
          SD=0.
        ELSE
          Q2IN=MIN(1E8,MAX(0.25,Q2))
          SD=LOG(LOG(Q2IN/0.232**2)/LOG(0.25/0.232**2))
        ENDIF
        SD2=SD**2
        XL=-LOG(X)
        XS=SQRT(X)
 
C...Evaluate valence, gluon and sea distributions.
        XFVUD=(0.663+0.191*SD-0.041*SD2+0.031*SD**3)*X**0.326*
     &  (1.+(-1.97+6.74*SD-1.96*SD2)*XS+(24.4-20.7*SD+4.08*SD2)*X)*
     &  (1.-X)**(2.86+0.70*SD-0.02*SD2)
        XFVDD=(0.579+0.283*SD+0.047*SD2)*X**(0.523-0.015*SD)*
     &  (1.+(2.22-0.59*SD-0.27*SD2)*XS+(5.95-6.19*SD+1.55*SD2)*X)*
     &  (1.-X)**(3.57+0.94*SD-0.16*SD2)
        XFGLU=(X**(1.00-0.17*SD)*((4.879*SD-1.383*SD2)+
     &  (25.92-28.97*SD+5.596*SD2)*X+(-25.69+23.68*SD-1.975*SD2)*X**2)+
     &  SD**0.558*EXP(-(0.595+2.138*SD)+SQRT(4.066*SD**1.218*XL)))*
     &  (1.-X)**(2.537+1.718*SD+0.353*SD2)
        XFSEA=(X**(0.412-0.171*SD)*(0.363-1.196*X+
     &  (1.029+1.785*SD-0.459*SD2)*X**2)*XL**(0.566-0.496*SD)+
     &  SD**1.396*EXP(-(3.838+1.944*SD)+SQRT(2.845*SD**1.331*XL)))*
     &  (1.-X)**(4.696+2.109*SD)
        XFSTR=SD**0.803*(1.+(-3.055+1.024*SD**0.67)*XS+
     &  (27.4-20.0*SD**0.154)*X)*(1.-X)**6.22*
     &  EXP(-(4.33+1.408*SD)+SQRT((8.27-0.437*SD)*SD**0.563*XL))/
     &  XL**(2.082-0.577*SD)
        IF(SD.LE.0.888) THEN
          XFCHM=0.
        ELSE
          XFCHM=(SD-0.888)**1.01*(1.+(4.24-0.804*SD)*X)*
     &    (1.-X)**(3.46+1.076*SD)*EXP(-(4.61+1.49*SD)+
     &    SQRT((2.555+1.961*SD)*SD**0.37*XL))
        ENDIF
        IF(SD.LE.1.351) THEN
          XFBOT=0.
        ELSE
          XFBOT=(SD-1.351)*(1.+1.848*X)*(1.-X)**(2.929+1.396*SD)*
     &    EXP(-(4.71+1.514*SD)+SQRT((4.02+1.239*SD)*SD**0.51*XL))
        ENDIF
 
C...Put into output array.
        XPPR(0)=XFGLU
        XPPR(1)=XFVDD+XFSEA
        XPPR(2)=XFVUD-XFVDD+XFSEA
        XPPR(3)=XFSTR
        XPPR(4)=XFCHM
        XPPR(5)=XFBOT
        XPPR(-1)=XFSEA
        XPPR(-2)=XFSEA
        XPPR(-3)=XFSTR
        XPPR(-4)=XFCHM
        XPPR(-5)=XFBOT
 
      ENDIF
 
      RETURN
      END
 
C*********************************************************************
 
      FUNCTION PYCTQ2 (Iset, Iprt, X, Q)
 
C...This routine gives the CTEQ 2 parton distribution function sets in
C...parametrized form. It is adapted from the revised parametrization
C...with extended range of November 12, 1993.
C...Authors: J. Botts, H.L. Lai, J.G. Morfin, J.F. Owens, J. Qiu,
C...W.K. Tung and H. Weerts.
      COMMON/GUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
      INTEGER  KCHG
      REAL  PMAS,PARF,VCKM
      SAVE /GUDAT2/
**JWG  add psi'(3685) as KC code 80, needed for Eurodec interface
*      --- ---------- -- -- ---- --- ------ --- ------- ---------
      INTEGER  IDPSIP
      PARAMETER  (IDPSIP=80)
C...Data on Lambda values of fits, minimum Q and quark masses.
      DIMENSION Alm(6), Qms(4:6)
      DATA Alm / 0.213, 0.208, 0.208, 0.322, 0.190, 0.235 /
      DATA Qmn / 1.60 /, (Qms(I), I=4,6) / 1.60, 5.00, 180.0 /
      Qms(6) = PMAS(6,1)
 
C....Check flavour thresholds. Set up Qi for SB.
      Ip = IABS(Iprt)
      If (Ip .GE. 4) then
        If (Q .LE. Qms(Ip)) then
          PYCTQ2 = 0.0
          Return
        Endif
        Qi = Qms(ip)
      Else
        Qi = Qmn
      Endif
 
C...Use "standard lambda" of parametrization program for expansion.
      Alam = Alm (Iset)
      SBL = LOG(Q/Alam) / LOG(Qi/Alam)
      SB = LOG (SBL)
      SB2 = SB*SB
      SB3 = SB2*SB
 
C...Expansion for run le26 - CTEQ2M
      If (Iset .EQ. 1) then
      If (Iprt .EQ. 2) then
      A0=Exp( 0.2143E+00+0.8417E+00*SB -0.2451E+01*SB2+0.9875E+00*SB3)
      A1= 0.5209E+00-0.2384E+00*SB +0.5086E+00*SB2-0.2123E+00*SB3
      A2= 0.3178E+01+0.5258E+01*SB -0.8102E+01*SB2+0.3334E+01*SB3
      A3=-0.8537E+00+0.5921E+01*SB -0.1007E+02*SB2+0.4146E+01*SB3
      A4= 0.1821E+01+0.2822E-01*SB +0.1662E+00*SB2-0.1058E+00*SB3
      A5= 0.0000E+00-0.1090E+01*SB +0.3136E+01*SB2-0.1301E+01*SB3
      Elseif (Iprt .EQ. 1) then
      A0=Exp(-0.1314E+01-0.1342E-01*SB +0.1136E+00*SB2-0.1557E+00*SB3)
      A1= 0.2780E+00+0.2558E-01*SB +0.4467E-02*SB2-0.2472E-02*SB3
      A2= 0.3672E+01+0.5324E+00*SB +0.3531E-01*SB2+0.7928E-03*SB3
      A3= 0.2957E+02-0.2000E+02*SB +0.5929E+01*SB2+0.3390E+00*SB3
      A4= 0.8069E+00-0.2877E+00*SB +0.3574E-01*SB2+0.5622E-02*SB3
      A5= 0.0000E+00+0.2287E+00*SB -0.4052E-01*SB2+0.5589E-01*SB3
      Elseif (Iprt .EQ. 0) then
      A0=Exp(-0.1059E+00-0.1461E+01*SB -0.2544E+00*SB2+0.4526E-01*SB3)
      A1=-0.2578E+00+0.1385E+00*SB -0.1383E+00*SB2+0.3811E-01*SB3
      A2= 0.5195E+01+0.9648E+00*SB -0.2103E+00*SB2-0.6701E-01*SB3
      A3= 0.5131E+01+0.2151E+01*SB -0.2880E+01*SB2+0.6608E+00*SB3
      A4= 0.1118E+01+0.2636E+00*SB -0.5140E+00*SB2+0.1613E+00*SB3
      A5= 0.0000E+00+0.2456E+01*SB -0.8741E+00*SB2+0.2136E+00*SB3
      Elseif (Iprt .EQ. -1) then
      A0=Exp(-0.2732E+00-0.3523E+01*SB +0.3657E+01*SB2-0.1415E+01*SB3)
      A1=-0.3807E+00+0.1211E+00*SB -0.1231E+00*SB2+0.3753E-01*SB3
      A2= 0.9698E+01-0.2596E+01*SB +0.2412E+01*SB2-0.9257E+00*SB3
      A3=-0.6165E+00+0.1120E+01*SB -0.1708E+01*SB2+0.6383E+00*SB3
      A4= 0.7292E-01-0.1339E+00*SB +0.2104E+00*SB2-0.7987E-01*SB3
      A5=-0.1370E+01+0.2452E+01*SB -0.1804E+01*SB2+0.6459E+00*SB3
      Elseif (Iprt .EQ. -2) then
      A0=Exp(-0.2319E+01-0.3182E+01*SB +0.3572E+01*SB2-0.1431E+01*SB3)
      A1=-0.2622E+00+0.3085E+00*SB -0.4394E+00*SB2+0.1496E+00*SB3
      A2= 0.9481E+01-0.3627E+01*SB +0.5640E+01*SB2-0.2265E+01*SB3
      A3= 0.5000E+02-0.1851E+02*SB +0.2640E+01*SB2-0.6001E+00*SB3
      A4= 0.1566E+01-0.7375E+00*SB +0.8736E+00*SB2-0.3449E+00*SB3
      A5=-0.7983E-01+0.3236E+01*SB -0.3373E+01*SB2+0.1236E+01*SB3
      Elseif (Iprt .EQ. -3) then
      A0=Exp(-0.1855E+01-0.5302E+01*SB +0.8433E+00*SB2-0.1236E+00*SB3)
      A1=-0.4000E-02-0.1345E+01*SB +0.1192E+01*SB2-0.3039E+00*SB3
      A2= 0.6870E+01+0.1246E+01*SB -0.8968E+00*SB2-0.9791E-01*SB3
      A3= 0.0000E+00+0.4616E+01*SB +0.1026E+02*SB2+0.2844E+02*SB3
      A4= 0.1000E-02+0.4098E+00*SB -0.4250E+00*SB2+0.1100E+00*SB3
      A5= 0.0000E+00-0.2151E+01*SB +0.2991E+01*SB2-0.7717E+00*SB3
      Elseif (Iprt .EQ. -4) then
      A0=SB** 0.7722E+00*Exp(-0.7241E+01-0.7885E-01*SB -0.1124E+01*SB2)
      A1=-0.3971E+00+0.9132E+00*SB -0.1175E+01*SB2+0.3573E+00*SB3
      A2= 0.6367E+01-0.6565E+01*SB +0.8114E+01*SB2-0.2666E+01*SB3
      A3= 0.2878E+02-0.2000E+02*SB +0.7000E+00*SB2+0.3000E+02*SB3
      A4= 0.1010E+00-0.4592E+00*SB +0.5877E+00*SB2-0.1472E+00*SB3
      A5= 0.1749E+00+0.3875E+01*SB -0.3768E+01*SB2+0.1316E+01*SB3
      Elseif (Iprt .EQ. -5) then
      A0=SB** 0.1299E+00*Exp(-0.4868E+01-0.4339E+01*SB +0.7080E+00*SB2)
      A1=-0.1705E+00-0.3381E+00*SB +0.5287E+00*SB2-0.2644E+00*SB3
      A2= 0.5610E+01-0.1365E+01*SB +0.1835E+01*SB2-0.5655E+00*SB3
      A3=-0.1001E+01+0.3044E+01*SB +0.2680E+01*SB2+0.1426E+02*SB3
      A4= 0.3814E-02+0.3430E+00*SB -0.6926E+00*SB2+0.3486E+00*SB3
      A5= 0.1156E+01+0.2016E+01*SB -0.1674E+01*SB2+0.5981E+00*SB3
      Elseif (Iprt .EQ. -6) then
      A0=SB** 0.9819E+00*Exp(-0.7859E+01+0.6819E+00*SB -0.3386E+01*SB2)
      A1=-0.1055E+00-0.1413E+01*SB +0.3451E+01*SB2-0.2466E+01*SB3
      A2= 0.4055E+01+0.8107E+01*SB -0.1576E+02*SB2+0.8094E+01*SB3
      A3= 0.3799E+01+0.9616E+01*SB -0.1984E+02*SB2+0.2641E+02*SB3
      A4= 0.3619E+00-0.8627E+00*SB -0.9390E-01*SB2+0.9196E+00*SB3
      A5= 0.3779E+01-0.6073E+01*SB +0.9999E+01*SB2-0.4304E+01*SB3
      Endif
 
C...Expansion for run sa17 - CTEQ2MS
      Elseif (Iset .EQ. 2) then
      If (Iprt .EQ. 2) then
      A0=Exp( 0.2790E+00+0.7294E+00*SB -0.2202E+01*SB2+0.8599E+00*SB3)
      A1= 0.5380E+00-0.2261E+00*SB +0.4636E+00*SB2-0.1871E+00*SB3
      A2= 0.3259E+01+0.2141E+01*SB -0.2947E+01*SB2+0.1245E+01*SB3
      A3=-0.8390E+00+0.1448E+01*SB -0.2331E+01*SB2+0.8658E+00*SB3
      A4= 0.1847E+01-0.3943E+01*SB +0.5998E+01*SB2-0.2191E+01*SB3
      A5= 0.0000E+00-0.9719E+00*SB +0.2830E+01*SB2-0.1137E+01*SB3
      Elseif (Iprt .EQ. 1) then
      A0=Exp(-0.1318E+01+0.2328E-01*SB +0.5179E-01*SB2-0.1305E+00*SB3)
      A1= 0.2760E+00+0.4429E-01*SB -0.2626E-01*SB2+0.7143E-02*SB3
      A2= 0.3660E+01+0.5232E+00*SB +0.5491E-01*SB2-0.4115E-02*SB3
      A3= 0.2910E+02-0.2000E+02*SB +0.6631E+01*SB2-0.3050E-01*SB3
      A4= 0.8010E+00-0.2688E+00*SB +0.1051E-01*SB2+0.1195E-01*SB3
      A5= 0.0000E+00+0.2887E+00*SB -0.1398E+00*SB2+0.8194E-01*SB3
      Elseif (Iprt .EQ. 0) then
      A0=Exp(-0.1623E+01-0.7232E+00*SB +0.1889E+00*SB2+0.1140E+00*SB3)
      A1=-0.5000E+00+0.8611E-01*SB +0.2203E-01*SB2-0.1401E-01*SB3
      A2= 0.3821E+01+0.8976E+00*SB +0.1400E+00*SB2-0.9163E-01*SB3
      A3= 0.5809E+01-0.5060E+01*SB +0.3808E+00*SB2+0.2519E+00*SB3
      A4= 0.4500E+00-0.5121E+00*SB +0.1979E+00*SB2-0.2705E-01*SB3
      A5= 0.0000E+00+0.1210E+01*SB -0.2921E+00*SB2+0.1240E+00*SB3
      Elseif (Iprt .EQ. -1) then
      A0=Exp(-0.6986E-01-0.5954E+00*SB -0.1582E+01*SB2+0.5104E+00*SB3)
      A1=-0.8461E+00+0.2127E+00*SB +0.9425E-01*SB2-0.5264E-01*SB3
      A2= 0.1200E+02+0.1659E+01*SB -0.5354E+01*SB2+0.1795E+01*SB3
      A3= 0.2958E+02+0.3000E+02*SB +0.3000E+02*SB2-0.1965E+02*SB3
      A4= 0.4000E+01-0.4865E+00*SB +0.9460E+00*SB2+0.3432E+00*SB3
      A5=-0.3378E+01+0.1656E+01*SB +0.1123E+01*SB2-0.4667E+00*SB3
      Elseif (Iprt .EQ. -2) then
      A0=Exp(-0.1929E+01-0.2626E+01*SB +0.2926E+01*SB2-0.1297E+01*SB3)
      A1=-0.6627E+00+0.4561E+00*SB -0.3818E+00*SB2+0.1239E+00*SB3
      A2= 0.9506E+01-0.2724E+01*SB +0.4283E+01*SB2-0.1804E+01*SB3
      A3= 0.1897E+02+0.1642E+01*SB -0.8390E+01*SB2+0.3894E+01*SB3
      A4= 0.1024E+01-0.1786E+00*SB +0.4535E+00*SB2-0.2075E+00*SB3
      A5=-0.1746E+01+0.3572E+01*SB -0.2908E+01*SB2+0.1093E+01*SB3
      Elseif (Iprt .EQ. -3) then
      A0=Exp(-0.4913E+00-0.6866E+01*SB +0.1432E+01*SB2-0.1749E+00*SB3)
      A1=-0.1157E+00-0.1567E+01*SB +0.1439E+01*SB2-0.3724E+00*SB3
      A2= 0.7730E+01+0.9748E+00*SB -0.1157E+01*SB2-0.8358E-02*SB3
      A3=-0.6050E+00+0.1835E+01*SB +0.3788E+01*SB2+0.3000E+02*SB3
      A4= 0.1620E-08+0.4590E+00*SB -0.4070E+00*SB2+0.8900E-01*SB3
      A5=-0.7048E+00-0.2505E+01*SB +0.4000E+01*SB2-0.1161E+01*SB3
      Elseif (Iprt .EQ. -4) then
      A0=SB** 0.7393E+00*Exp(-0.6518E+01-0.3998E+00*SB -0.1111E+01*SB2)
      A1=-0.6482E+00+0.1125E+01*SB -0.1290E+01*SB2+0.3940E+00*SB3
      A2= 0.8487E+01-0.9235E+01*SB +0.9353E+01*SB2-0.2913E+01*SB3
      A3= 0.2265E+02-0.1999E+02*SB +0.4105E+01*SB2+0.2144E+02*SB3
      A4= 0.8990E-01-0.4372E+00*SB +0.5941E+00*SB2-0.1469E+00*SB3
      A5=-0.9690E+00+0.5068E+01*SB -0.4368E+01*SB2+0.1503E+01*SB3
      Elseif (Iprt .EQ. -5) then
      A0=SB** 0.9880E+00*Exp(-0.7180E+01-0.2494E+01*SB +0.3561E-01*SB2)
      A1=-0.4301E+00-0.2611E+00*SB +0.3914E+00*SB2-0.1638E+00*SB3
      A2= 0.5137E+01+0.1506E+01*SB -0.9588E+00*SB2-0.1596E+00*SB3
      A3= 0.1483E+02+0.2998E+02*SB +0.2357E+02*SB2-0.9353E+01*SB3
      A4= 0.2426E+00+0.1371E+00*SB -0.3791E+00*SB2+0.1948E+00*SB3
      A5= 0.1463E+01+0.1907E+00*SB +0.3557E+00*SB2+0.2097E-01*SB3
      Elseif (Iprt .EQ. -6) then
      A0=SB** 0.1005E+01*Exp(-0.5255E+01-0.9866E-01*SB -0.2737E+01*SB2)
      A1=-0.3140E+00-0.2055E+00*SB +0.5594E+00*SB2-0.2960E+00*SB3
      A2= 0.9227E+01-0.4569E+01*SB -0.9724E+01*SB2+0.1026E+02*SB3
      A3= 0.1131E+02-0.1972E+02*SB -0.1107E+02*SB2+0.2311E+02*SB3
      A4= 0.1488E+01+0.1737E+01*SB +0.4323E+01*SB2-0.9925E+01*SB3
      A5= 0.1895E+01-0.7350E+00*SB +0.3780E+01*SB2-0.1408E+01*SB3
      Endif
 
      Elseif (Iset .EQ. 3) then
C...Expansion for run fa06 - CTEQ2MF
      If (Iprt .EQ. 2) then
      A0=Exp(-0.7913E+00-0.2789E+01*SB -0.7289E-01*SB2+0.1770E+00*SB3)
      A1= 0.4942E+00-0.7886E-01*SB +0.9057E-01*SB2-0.5259E-01*SB3
      A2= 0.3727E+01+0.1089E+01*SB -0.1004E+01*SB2+0.4345E+00*SB3
      A3= 0.1944E+01+0.7846E+01*SB +0.7984E+01*SB2+0.5548E+01*SB3
      A4= 0.2940E-02+0.8428E-04*SB +0.1266E+00*SB2-0.3517E-01*SB3
      A5=-0.1060E+00-0.1192E-01*SB +0.1130E+01*SB2-0.4527E+00*SB3
      Elseif (Iprt .EQ. 1) then
      A0=Exp(-0.1344E+01+0.7859E-02*SB +0.4623E-01*SB2-0.1273E+00*SB3)
      A1= 0.2760E+00+0.4201E-01*SB -0.1795E-01*SB2+0.3212E-02*SB3
      A2= 0.3660E+01+0.5247E+00*SB +0.4405E-01*SB2+0.1391E-02*SB3
      A3= 0.2981E+02-0.2000E+02*SB +0.6566E+01*SB2+0.2479E-01*SB3
      A4= 0.7950E+00-0.2732E+00*SB +0.2470E-01*SB2+0.6157E-02*SB3
      A5= 0.0000E+00+0.2793E+00*SB -0.9197E-01*SB2+0.5953E-01*SB3
      Elseif (Iprt .EQ. 0) then
      A0=Exp( 0.9746E+00-0.3252E+01*SB +0.1664E+01*SB2-0.6410E+00*SB3)
      A1=-0.5271E-02-0.3198E+00*SB +0.1279E+00*SB2-0.1256E-02*SB3
      A2= 0.5740E+01-0.3139E+01*SB +0.3841E+01*SB2-0.1415E+01*SB3
      A3= 0.7161E-01-0.4363E+01*SB +0.4925E+01*SB2-0.1614E+01*SB3
      A4= 0.1860E+01+0.1342E+01*SB -0.2234E+01*SB2+0.1047E+01*SB3
      A5= 0.7409E-01+0.2390E+01*SB -0.1457E+01*SB2+0.5853E+00*SB3
      Elseif (Iprt .EQ. -1) then
      A0=Exp(-0.8454E+00-0.3334E+01*SB +0.3591E+01*SB2-0.1485E+01*SB3)
      A1=-0.2826E-02-0.2810E+00*SB -0.3809E-01*SB2+0.6585E-01*SB3
      A2= 0.9139E+01-0.2811E+01*SB +0.4730E+01*SB2-0.2157E+01*SB3
      A3=-0.3120E+00+0.1217E+01*SB -0.1726E+01*SB2+0.6220E+00*SB3
      A4= 0.1793E-01-0.4608E-01*SB +0.5294E-01*SB2-0.1709E-01*SB3
      A5=-0.1471E+00+0.1104E+01*SB -0.1358E+01*SB2+0.7200E+00*SB3
      Elseif (Iprt .EQ. -2) then
      A0=Exp(-0.1398E+01-0.3536E+01*SB +0.3849E+01*SB2-0.1549E+01*SB3)
      A1=-0.1332E-01-0.2155E-01*SB -0.3404E+00*SB2+0.1569E+00*SB3
      A2= 0.9981E+01-0.3499E+01*SB +0.5448E+01*SB2-0.2198E+01*SB3
      A3= 0.3736E+02-0.2000E+02*SB +0.6675E+01*SB2-0.7276E+00*SB3
      A4= 0.1705E+01-0.1013E+01*SB +0.1122E+01*SB2-0.4057E+00*SB3
      A5=-0.1189E-01+0.2698E+01*SB -0.3429E+01*SB2+0.1389E+01*SB3
      Elseif (Iprt .EQ. -3) then
      A0=Exp(-0.2979E+01-0.6085E+01*SB +0.2428E+01*SB2-0.6482E+00*SB3)
      A1=-0.1372E+00-0.1281E+00*SB +0.1587E+00*SB2-0.9637E-01*SB3
      A2= 0.7009E+01-0.1609E+01*SB +0.2765E+01*SB2-0.1177E+01*SB3
      A3= 0.1308E+01+0.9583E+01*SB +0.2360E+02*SB2+0.2999E+02*SB3
      A4= 0.2509E-01+0.2106E+00*SB -0.4405E+00*SB2+0.2075E+00*SB3
      A5=-0.2069E-01+0.1971E+01*SB -0.1615E+01*SB2+0.6039E+00*SB3
      Elseif (Iprt .EQ. -4) then
      A0=SB** 0.8072E+00*Exp(-0.6920E+01-0.5031E+00*SB -0.9965E+00*SB2)
      A1=-0.2118E+00+0.7930E+00*SB -0.1101E+01*SB2+0.3302E+00*SB3
      A2= 0.8039E+01-0.7170E+01*SB +0.8657E+01*SB2-0.2893E+01*SB3
      A3= 0.2926E+02-0.1993E+02*SB +0.1841E+01*SB2+0.2996E+02*SB3
      A4= 0.1339E+00-0.5531E+00*SB +0.6505E+00*SB2-0.1595E+00*SB3
      A5= 0.7439E+00+0.3307E+01*SB -0.3284E+01*SB2+0.1152E+01*SB3
      Elseif (Iprt .EQ. -5) then
      A0=SB** 0.9925E+00*Exp(-0.2190E+01-0.3393E+01*SB -0.8631E+00*SB2)
      A1=-0.1261E+00-0.2368E+00*SB +0.4143E+00*SB2-0.1577E+00*SB3
      A2= 0.4585E+01+0.5227E+01*SB -0.3248E+01*SB2-0.2599E+00*SB3
      A3=-0.1094E+01+0.4927E+00*SB -0.9921E+00*SB2+0.3138E+01*SB3
      A4= 0.1396E+00+0.2562E+00*SB +0.1844E+00*SB2-0.1599E+00*SB3
      A5= 0.8621E+00+0.4715E+00*SB +0.2547E+01*SB2-0.8429E+00*SB3
      Elseif (Iprt .EQ. -6) then
      A0=SB** 0.1016E+01*Exp(-0.5397E+01-0.1979E+01*SB -0.2441E+00*SB2)
      A1=-0.1426E+00-0.2861E+00*SB +0.7434E+00*SB2-0.5214E+00*SB3
      A2= 0.6363E+01+0.4028E+00*SB -0.8356E+01*SB2+0.6814E+01*SB3
      A3=-0.2526E+00+0.2425E+01*SB -0.1407E+02*SB2+0.3000E+02*SB3
      A4= 0.1125E+00-0.1089E+01*SB +0.9977E+01*SB2+0.1000E+02*SB3
      A5= 0.2669E+01-0.6366E+00*SB +0.4355E+01*SB2-0.2919E+01*SB3
      Endif
 
      Elseif (Iset .EQ. 4) then
C...Expansion for run ll25 - CTEQ2ML
      If (Iprt .EQ. 2) then
      A0=Exp( 0.3760E+00+0.5491E+00*SB -0.1845E+01*SB2+0.6803E+00*SB3)
      A1= 0.5650E+00-0.1953E+00*SB +0.3761E+00*SB2-0.1419E+00*SB3
      A2= 0.3464E+01+0.3817E+01*SB -0.5384E+01*SB2+0.2057E+01*SB3
      A3=-0.5850E+00+0.5566E+01*SB -0.9000E+01*SB2+0.3433E+01*SB3
      A4= 0.2322E+01-0.1431E+00*SB +0.3901E+00*SB2-0.1678E+00*SB3
      A5= 0.0000E+00-0.7370E+00*SB +0.2310E+01*SB2-0.8743E+00*SB3
      Elseif (Iprt .EQ. 1) then
      A0=Exp(-0.1324E+01+0.1169E-01*SB +0.1969E-01*SB2-0.7583E-01*SB3)
      A1= 0.2890E+00+0.5832E-01*SB -0.2921E-01*SB2+0.4701E-02*SB3
      A2= 0.3580E+01+0.5291E+00*SB -0.5662E-02*SB2+0.2746E-01*SB3
      A3= 0.3021E+02-0.1999E+02*SB +0.6250E+01*SB2-0.3035E+00*SB3
      A4= 0.7990E+00-0.2531E+00*SB +0.5556E-02*SB2+0.8272E-02*SB3
      A5= 0.0000E+00+0.3674E+00*SB -0.1383E+00*SB2+0.4665E-01*SB3
      Elseif (Iprt .EQ. 0) then
      A0=Exp(-0.1920E+00-0.7015E+00*SB -0.9113E+00*SB2+0.2352E+00*SB3)
      A1=-0.2120E+00+0.1133E-01*SB -0.1553E-01*SB2+0.2822E-02*SB3
      A2= 0.4549E+01+0.1250E+01*SB -0.4647E+00*SB2+0.9617E-01*SB3
      A3= 0.1197E+02-0.4156E+01*SB +0.1413E+00*SB2+0.1607E+00*SB3
      A4= 0.1616E+01+0.1082E+00*SB -0.6651E+00*SB2+0.2356E+00*SB3
      A5= 0.0000E+00+0.1824E+01*SB -0.2063E+00*SB2+0.1148E-01*SB3
      Elseif (Iprt .EQ. -1) then
      A0=Exp(-0.1388E+01-0.7408E+00*SB -0.6454E+00*SB2+0.2373E+00*SB3)
      A1=-0.2928E+00-0.1726E-01*SB +0.4033E-01*SB2-0.2514E-01*SB3
      A2= 0.9975E+01-0.2048E+01*SB -0.6060E+00*SB2+0.5225E+00*SB3
      A3= 0.2687E+02-0.4683E+01*SB -0.1999E+02*SB2+0.1188E+02*SB3
      A4= 0.4000E+01-0.6773E+00*SB +0.4301E+00*SB2+0.4524E+00*SB3
      A5=-0.7164E+00+0.7488E+00*SB +0.5766E+00*SB2-0.2609E+00*SB3
      Elseif (Iprt .EQ. -2) then
      A0=Exp(-0.2272E+01-0.2998E+01*SB +0.3282E+01*SB2-0.1203E+01*SB3)
      A1=-0.2062E+00+0.3320E+00*SB -0.5074E+00*SB2+0.1655E+00*SB3
      A2= 0.9667E+01-0.3497E+01*SB +0.5271E+01*SB2-0.1984E+01*SB3
      A3= 0.4996E+02-0.3241E+01*SB -0.1425E+02*SB2+0.3849E+01*SB3
      A4= 0.1619E+01-0.5354E+00*SB +0.5753E+00*SB2-0.2238E+00*SB3
      A5= 0.8755E-01+0.3195E+01*SB -0.3496E+01*SB2+0.1197E+01*SB3
      Elseif (Iprt .EQ. -3) then
      A0=Exp(-0.1864E+01-0.5258E+01*SB +0.1034E+01*SB2-0.1550E+00*SB3)
      A1= 0.1000E-02-0.1090E+01*SB +0.8345E+00*SB2-0.1887E+00*SB3
      A2= 0.6898E+01-0.4951E+00*SB +0.4279E+00*SB2-0.2727E+00*SB3
      A3= 0.0000E+00+0.4322E+01*SB +0.8181E+01*SB2+0.2309E+02*SB3
      A4= 0.1000E-02+0.3550E+00*SB -0.3220E+00*SB2+0.7294E-01*SB3
      A5= 0.0000E+00-0.1347E+01*SB +0.1896E+01*SB2-0.4491E+00*SB3
      Elseif (Iprt .EQ. -4) then
      A0=SB** 0.7528E+00*Exp(-0.7684E+01+0.6791E-01*SB -0.9094E+00*SB2)
      A1=-0.3732E+00+0.8408E+00*SB -0.1020E+01*SB2+0.3046E+00*SB3
      A2= 0.4984E+01-0.5534E+01*SB +0.6418E+01*SB2-0.1856E+01*SB3
      A3= 0.3761E+02-0.1999E+02*SB -0.3358E+01*SB2+0.2999E+02*SB3
      A4= 0.1161E+00-0.4680E+00*SB +0.5567E+00*SB2-0.1633E+00*SB3
      A5= 0.3028E+00+0.3339E+01*SB -0.3004E+01*SB2+0.9160E+00*SB3
      Elseif (Iprt .EQ. -5) then
      A0=SB** 0.1011E+01*Exp(-0.7217E+01-0.2288E+01*SB +0.3450E+00*SB2)
      A1=-0.1955E+00-0.3371E+00*SB +0.5111E+00*SB2-0.2210E+00*SB3
      A2= 0.4302E+01-0.1214E+01*SB +0.3104E+01*SB2-0.1408E+01*SB3
      A3= 0.1487E+02+0.1549E+02*SB +0.2875E+02*SB2-0.1922E+02*SB3
      A4= 0.8935E-02+0.3571E+00*SB -0.6668E+00*SB2+0.3037E+00*SB3
      A5= 0.1570E+01+0.7105E+00*SB -0.6070E+00*SB2+0.3796E+00*SB3
      Elseif (Iprt .EQ. -6) then
      A0=SB** 0.9986E+00*Exp(-0.5847E+01-0.2798E+00*SB -0.9882E+00*SB2)
      A1=-0.2154E+00-0.8282E-01*SB +0.3611E-01*SB2+0.2623E-01*SB3
      A2= 0.3250E+01+0.9635E+01*SB -0.1274E+02*SB2+0.4453E+01*SB3
      A3=-0.2594E+01+0.9097E+01*SB +0.1581E+02*SB2-0.9123E+01*SB3
      A4= 0.1768E+01-0.2749E+01*SB +0.9999E+01*SB2+0.9995E+01*SB3
      A5= 0.2521E+01-0.1802E-01*SB +0.4820E+00*SB2+0.2004E+00*SB3
      Endif
 
      Elseif (Iset .EQ. 5) then
C...Expansion for run lo24 - CTEQ2L
      If (Iprt .EQ. 2) then
      A0=Exp( 0.7248E-01+0.3941E+00*SB -0.1772E+01*SB2+0.7629E+00*SB3)
      A1= 0.4964E+00-0.1224E+00*SB +0.3646E+00*SB2-0.1685E+00*SB3
      A2= 0.3000E+01+0.2780E+01*SB -0.4028E+01*SB2+0.1816E+01*SB3
      A3=-0.1064E+01+0.3062E+01*SB -0.5927E+01*SB2+0.2785E+01*SB3
      A4= 0.3193E+01+0.1499E+01*SB -0.2765E+01*SB2+0.1019E+01*SB3
      A5= 0.1524E-01-0.4541E+00*SB +0.2281E+01*SB2-0.1033E+01*SB3
      Elseif (Iprt .EQ. 1) then
      A0=Exp(-0.1794E+01-0.2055E+00*SB -0.3350E-01*SB2-0.5084E-01*SB3)
      A1= 0.1748E+00+0.4637E-01*SB -0.2048E-01*SB2+0.2596E-02*SB3
      A2= 0.3321E+01+0.6253E+00*SB +0.2148E-01*SB2+0.1288E-01*SB3
      A3= 0.4355E+02-0.2000E+02*SB +0.5486E+01*SB2+0.1536E+00*SB3
      A4= 0.9586E+00-0.3217E+00*SB +0.4458E-01*SB2-0.1404E-03*SB3
      A5=-0.6595E-02+0.3499E+00*SB -0.7048E-01*SB2+0.2619E-01*SB3
      Elseif (Iprt .EQ. 0) then
      A0=Exp(-0.6194E+00-0.2643E+00*SB -0.1875E+01*SB2+0.6011E+00*SB3)
      A1=-0.2600E+00+0.8704E-01*SB -0.7375E-01*SB2+0.1876E-01*SB3
      A2= 0.4620E+01+0.1578E+01*SB -0.8411E+00*SB2+0.1527E+00*SB3
      A3= 0.1604E+02-0.1230E+02*SB +0.6939E+01*SB2-0.2012E+01*SB3
      A4= 0.1255E+01+0.4769E+00*SB -0.9915E+00*SB2+0.3439E+00*SB3
      A5= 0.1116E-02+0.2409E+01*SB -0.4442E+00*SB2+0.3431E-01*SB3
      Elseif (Iprt .EQ. -1) then
      A0=Exp(-0.1571E+01-0.1905E+00*SB -0.8672E+00*SB2+0.2070E+00*SB3)
      A1=-0.3266E+00+0.6428E-01*SB -0.8694E-01*SB2+0.1778E-01*SB3
      A2= 0.8921E+01-0.5010E+00*SB -0.9658E+00*SB2+0.3893E+00*SB3
      A3= 0.1329E+02+0.4652E+01*SB -0.2000E+02*SB2+0.1001E+02*SB3
      A4= 0.3283E+01-0.3400E+00*SB -0.1957E+00*SB2+0.8063E+00*SB3
      A5=-0.5701E+00+0.4042E+00*SB +0.5239E+00*SB2-0.1665E+00*SB3
      Elseif (Iprt .EQ. -2) then
      A0=Exp(-0.2281E+01-0.2768E+01*SB +0.3137E+01*SB2-0.1278E+01*SB3)
      A1=-0.2624E+00+0.4142E+00*SB -0.5936E+00*SB2+0.1937E+00*SB3
      A2= 0.9438E+01-0.3179E+01*SB +0.5107E+01*SB2-0.2179E+01*SB3
      A3= 0.5000E+02-0.1802E+02*SB -0.7515E+01*SB2+0.2991E+01*SB3
      A4= 0.1809E+01-0.9121E+00*SB +0.8854E+00*SB2-0.3582E+00*SB3
      A5= 0.4056E-01+0.3033E+01*SB -0.3431E+01*SB2+0.1253E+01*SB3
      Elseif (Iprt .EQ. -3) then
      A0=Exp(-0.2318E+01-0.4104E+01*SB -0.1502E+00*SB2+0.1693E+00*SB3)
      A1=-0.2251E-01-0.1101E+01*SB +0.1037E+01*SB2-0.3290E+00*SB3
      A2= 0.6989E+01+0.1794E+01*SB -0.1811E+01*SB2+0.3061E+00*SB3
      A3= 0.7972E+00+0.7806E+01*SB +0.1869E+02*SB2+0.2999E+02*SB3
      A4= 0.4795E-01+0.1622E+00*SB -0.3977E+00*SB2+0.1920E+00*SB3
      A5=-0.5275E-01-0.2616E+01*SB +0.3076E+01*SB2-0.7425E+00*SB3
      Elseif (Iprt .EQ. -4) then
      A0=SB** 0.8431E+00*Exp(-0.6539E+01-0.1875E+00*SB -0.1346E+01*SB2)
      A1=-0.4970E+00+0.9062E+00*SB -0.1169E+01*SB2+0.3703E+00*SB3
      A2= 0.4939E+01-0.2995E+01*SB +0.4483E+01*SB2-0.1704E+01*SB3
      A3= 0.3113E+02-0.1997E+02*SB +0.1540E+01*SB2+0.3000E+02*SB3
      A4= 0.1349E+00-0.5418E+00*SB +0.6142E+00*SB2-0.1360E+00*SB3
      A5=-0.8590E+00+0.3956E+01*SB -0.3612E+01*SB2+0.1401E+01*SB3
      Elseif (Iprt .EQ. -5) then
      A0=SB** 0.2639E-01*Exp(-0.2099E+01-0.2681E+01*SB +0.2925E+00*SB2)
      A1=-0.2243E+00-0.5343E-01*SB -0.1953E-01*SB2+0.1586E-01*SB3
      A2= 0.4294E+01+0.1102E+01*SB -0.1822E+00*SB2-0.2481E+00*SB3
      A3=-0.9998E+00+0.8275E-01*SB +0.5494E+00*SB2-0.1982E+00*SB3
      A4= 0.5904E-04+0.9222E-01*SB -0.9293E-01*SB2+0.9159E-01*SB3
      A5= 0.2657E+00+0.1770E+01*SB -0.7111E+00*SB2+0.2525E+00*SB3
      Elseif (Iprt .EQ. -6) then
      A0=SB** 0.1009E+01*Exp(-0.7032E+01+0.4562E+01*SB -0.9081E+01*SB2)
      A1=-0.1412E+00-0.5076E+00*SB +0.9513E+00*SB2-0.4326E+00*SB3
      A2= 0.5385E+01+0.3023E+01*SB -0.1162E+02*SB2+0.7006E+01*SB3
      A3= 0.4997E+01-0.1600E+02*SB +0.1342E+02*SB2+0.1197E+02*SB3
      A4= 0.5825E+00+0.3994E+00*SB -0.1255E+01*SB2+0.6486E+00*SB3
      A5= 0.3365E+01-0.4026E+01*SB +0.8385E+01*SB2-0.2260E+01*SB3
      Endif
 
      Elseif (Iset .EQ. 6) then
C...Expansion for run da06 - CTEQ2D
      If (Iprt .EQ. 2) then
      A0=Exp( 0.1590E+00+0.5580E+00*SB -0.1838E+01*SB2+0.7018E+00*SB3)
      A1= 0.5110E+00-0.1625E+00*SB +0.3547E+00*SB2-0.1412E+00*SB3
      A2= 0.3158E+01+0.3962E+01*SB -0.5866E+01*SB2+0.2375E+01*SB3
      A3=-0.6000E+00+0.6144E+01*SB -0.1056E+02*SB2+0.4345E+01*SB3
      A4= 0.2306E+01-0.4669E-01*SB +0.2711E+00*SB2-0.1640E+00*SB3
      A5= 0.0000E+00-0.6638E+00*SB +0.2239E+01*SB2-0.8843E+00*SB3
      Elseif (Iprt .EQ. 1) then
      A0=Exp(-0.1182E+01+0.1449E+00*SB +0.2753E-01*SB2-0.1009E+00*SB3)
      A1= 0.2540E+00+0.2686E-01*SB -0.1546E-01*SB2+0.5396E-02*SB3
      A2= 0.3442E+01+0.5576E+00*SB +0.1937E-01*SB2+0.6696E-02*SB3
      A3= 0.2545E+02-0.2000E+02*SB +0.7355E+01*SB2-0.7058E+00*SB3
      A4= 0.9170E+00-0.3090E+00*SB +0.1705E-01*SB2+0.8534E-02*SB3
      A5= 0.0000E+00+0.1449E+00*SB -0.7821E-01*SB2+0.6405E-01*SB3
      Elseif (Iprt .EQ. 0) then
      A0=Exp(-0.3410E+00-0.9613E+00*SB -0.4969E+00*SB2+0.9360E-01*SB3)
      A1=-0.2400E+00+0.1473E+00*SB -0.1593E+00*SB2+0.4538E-01*SB3
      A2= 0.4841E+01+0.9311E+00*SB +0.1601E-03*SB2-0.1331E+00*SB3
      A3= 0.7427E+01-0.1397E+01*SB +0.1489E+00*SB2-0.2848E+00*SB3
      A4= 0.9600E+00+0.3697E+00*SB -0.4246E+00*SB2+0.1032E+00*SB3
      A5= 0.0000E+00+0.2484E+01*SB -0.9908E+00*SB2+0.2568E+00*SB3
      Elseif (Iprt .EQ. -1) then
      A0=Exp( 0.1176E+00-0.3418E+01*SB +0.3529E+01*SB2-0.1367E+01*SB3)
      A1=-0.3654E+00+0.1914E+00*SB -0.2192E+00*SB2+0.6933E-01*SB3
      A2= 0.1099E+02-0.4281E+01*SB +0.3729E+01*SB2-0.1254E+01*SB3
      A3=-0.7514E+00+0.7696E+00*SB -0.1134E+01*SB2+0.4245E+00*SB3
      A4= 0.7690E-01-0.6558E-01*SB +0.8726E-01*SB2-0.3345E-01*SB3
      A5=-0.1447E+01+0.2617E+01*SB -0.2094E+01*SB2+0.7536E+00*SB3
      Elseif (Iprt .EQ. -2) then
      A0=Exp(-0.2412E+01-0.2522E+01*SB +0.3126E+01*SB2-0.1305E+01*SB3)
      A1=-0.2353E+00+0.3118E+00*SB -0.4864E+00*SB2+0.1689E+00*SB3
      A2= 0.9017E+01-0.2437E+01*SB +0.4659E+01*SB2-0.2044E+01*SB3
      A3= 0.5000E+02-0.1158E+02*SB -0.9260E+01*SB2+0.2847E+01*SB3
      A4= 0.1726E+01-0.6849E+00*SB +0.7864E+00*SB2-0.3300E+00*SB3
      A5= 0.5080E-01+0.2858E+01*SB -0.3297E+01*SB2+0.1246E+01*SB3
      Elseif (Iprt .EQ. -3) then
      A0=Exp(-0.1966E+01-0.4405E+01*SB +0.2436E+00*SB2+0.4576E-01*SB3)
      A1=-0.4000E-02-0.1229E+01*SB +0.1118E+01*SB2-0.2988E+00*SB3
      A2= 0.6902E+01+0.1266E+01*SB -0.1068E+01*SB2+0.3062E-01*SB3
      A3= 0.0000E+00+0.3987E+01*SB +0.9389E+01*SB2+0.1881E+02*SB3
      A4= 0.1000E-02+0.3528E+00*SB -0.4201E+00*SB2+0.1248E+00*SB3
      A5= 0.0000E+00-0.2149E+01*SB +0.2925E+01*SB2-0.7609E+00*SB3
      Elseif (Iprt .EQ. -4) then
      A0=SB** 0.7561E+00*Exp(-0.6960E+01+0.5634E-01*SB -0.1170E+01*SB2)
      A1=-0.4232E+00+0.9269E+00*SB -0.1161E+01*SB2+0.3470E+00*SB3
      A2= 0.6057E+01-0.5790E+01*SB +0.7352E+01*SB2-0.2435E+01*SB3
      A3= 0.2941E+02-0.1999E+02*SB -0.8345E+00*SB2+0.3000E+02*SB3
      A4= 0.1069E+00-0.4620E+00*SB +0.5614E+00*SB2-0.1336E+00*SB3
      A5=-0.1865E+00+0.3953E+01*SB -0.3791E+01*SB2+0.1315E+01*SB3
      Elseif (Iprt .EQ. -5) then
      A0=SB** 0.5661E-02*Exp(-0.2123E+01-0.3026E+01*SB +0.1912E+00*SB2)
      A1=-0.2011E+00-0.1338E-01*SB -0.3974E-01*SB2+0.1948E-01*SB3
      A2= 0.4906E+01+0.1740E+01*SB -0.1387E+01*SB2+0.1263E+00*SB3
      A3=-0.1000E+01+0.5767E-01*SB +0.6377E+00*SB2+0.4736E-01*SB3
      A4= 0.5927E-04+0.1039E+00*SB -0.9797E-01*SB2+0.6881E-01*SB3
      A5= 0.4017E+00+0.1981E+01*SB -0.7758E+00*SB2+0.2916E+00*SB3
      Elseif (Iprt .EQ. -6) then
      A0=SB** 0.1008E+01*Exp(-0.7211E+01+0.3273E+01*SB -0.6979E+01*SB2)
      A1=-0.1026E+00-0.4948E+00*SB +0.1188E+01*SB2-0.8016E+00*SB3
      A2= 0.5397E+01+0.2135E+01*SB -0.9531E+01*SB2+0.6115E+01*SB3
      A3= 0.4966E+01-0.1111E+02*SB +0.4732E+01*SB2+0.1568E+02*SB3
      A4= 0.5345E+00-0.1935E+00*SB +0.5816E+00*SB2-0.6794E+00*SB3
      A5= 0.3569E+01-0.3477E+01*SB +0.8756E+01*SB2-0.4139E+01*SB3
      Endif
      Endif
 
C...Calculation of x * f(x, Q).
      PYCTQ2 = MAX(0., A0 *(X**A1) *((1.-X)**A2) *(1.+A3*(X**A4))
     &                 *(log(1.+1./X))**A5 )
 
      RETURN
      END
 
C*********************************************************************
 
      FUNCTION PYHFTH(SH,SQM,FRATT)
 
C...Gives threshold attractive/repulsive factor for heavy flavour
C...production.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
C...Value for alpha_strong.
      IF(MSTP(35).LE.1) THEN
        ALSSG=PARP(35)
      ELSE
        MST115=MSTU(115)
        MSTU(115)=MSTP(36)
        Q2BN=SQRT(MAX(1.,SQM*((SQRT(SH)-2.*SQRT(SQM))**2+PARP(36)**2)))
        ALSSG=ULALPS(Q2BN)
        MSTU(115)=MST115
      ENDIF
 
C...Evaluate attractive and repulsive factors.
      XATTR=4.*PARU(1)*ALSSG/(3.*SQRT(MAX(1E-20,1.-4.*SQM/SH)))
      FATTR=XATTR/(1.-EXP(-MIN(50.,XATTR)))
      XREPU=PARU(1)*ALSSG/(6.*SQRT(MAX(1E-20,1.-4.*SQM/SH)))
      FREPU=XREPU/(EXP(MIN(50.,XREPU))-1.)
      PYHFTH=FRATT*FATTR+(1.-FRATT)*FREPU
      VINT(138)=PYHFTH
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYSPLI(KF,KFLIN,KFLCH,KFLSP)
 
C...In case of a hadron remnant which is more complicated than just a
C...quark or a diquark, split it into two (partons or hadron + parton).
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      DIMENSION KFL(3)
 
C...Preliminaries. Parton composition.
      KFA=IABS(KF)
      KFS=ISIGN(1,KF)
      KFL(1)=MOD(KFA/1000,10)
      KFL(2)=MOD(KFA/100,10)
      KFL(3)=MOD(KFA/10,10)
      IF(KFA.EQ.22.AND.MINT(109).EQ.2) THEN
        KFL(2)=INT(1.5+RLU(0))
        IF(MINT(105).EQ.333) KFL(2)=3
        IF(MINT(105).EQ.443) KFL(2)=4
        KFL(3)=KFL(2)
      ELSEIF((KFA.EQ.111.OR.KFA.EQ.113).AND.RLU(0).GT.0.5) THEN
        KFL(2)=2
        KFL(3)=2
      ELSEIF(KFA.EQ.223.AND.RLU(0).GT.0.5) THEN
        KFL(2)=1
        KFL(3)=1
      ENDIF
      IF(KFLIN.NE.21.AND.KFLIN.NE.22.AND.KFLIN.NE.23) THEN
        KFLR=KFLIN*KFS
      ELSE
        KFLR=KFLIN
      ENDIF
      KFLCH=0
 
C...Subdivide lepton.
      IF(KFA.GE.11.AND.KFA.LE.18) THEN
        IF(KFLR.EQ.KFA) THEN
          KFLSP=KFS*22
        ELSEIF(KFLR.EQ.22) THEN
          KFLSP=KFA
        ELSEIF(KFLR.EQ.-24.AND.MOD(KFA,2).EQ.1) THEN
          KFLSP=KFA+1
        ELSEIF(KFLR.EQ.24.AND.MOD(KFA,2).EQ.0) THEN
          KFLSP=KFA-1
        ELSEIF(KFLR.EQ.21) THEN
          KFLSP=KFA
          KFLCH=KFS*21
        ELSE
          KFLSP=KFA
          KFLCH=-KFLR
        ENDIF
 
C...Subdivide photon.
      ELSEIF(KFA.EQ.22.AND.MINT(109).NE.2) THEN
        IF(KFLR.NE.21) THEN
          KFLSP=-KFLR
        ELSE
          RAGR=0.75*RLU(0)
          KFLSP=1
          IF(RAGR.GT.0.125) KFLSP=2
          IF(RAGR.GT.0.625) KFLSP=3
          IF(RLU(0).GT.0.5) KFLSP=-KFLSP
          KFLCH=-KFLSP
        ENDIF
 
C...Subdivide Reggeon or Pomeron.
      ELSEIF(KFA.EQ.28.OR.KFA.EQ.29) THEN
        IF(KFLIN.EQ.21) THEN
          KFLSP=KFS*21
        ELSE
          KFLSP=-KFLIN
        ENDIF
 
C...Subdivide meson.
      ELSEIF(KFL(1).EQ.0) THEN
        KFL(2)=KFL(2)*(-1)**KFL(2)
        KFL(3)=-KFL(3)*(-1)**IABS(KFL(2))
        IF(KFLR.EQ.KFL(2)) THEN
          KFLSP=KFL(3)
        ELSEIF(KFLR.EQ.KFL(3)) THEN
          KFLSP=KFL(2)
        ELSEIF(KFLR.EQ.21.AND.RLU(0).GT.0.5) THEN
          KFLSP=KFL(2)
          KFLCH=KFL(3)
        ELSEIF(KFLR.EQ.21) THEN
          KFLSP=KFL(3)
          KFLCH=KFL(2)
        ELSEIF(KFLR*KFL(2).GT.0) THEN
          CALL LUKFDI(-KFLR,KFL(2),KFDUMP,KFLCH)
          KFLSP=KFL(3)
        ELSE
          CALL LUKFDI(-KFLR,KFL(3),KFDUMP,KFLCH)
          KFLSP=KFL(2)
        ENDIF
 
C...Subdivide baryon.
      ELSE
        NAGR=0
        DO 100 J=1,3
        IF(KFLR.EQ.KFL(J)) NAGR=NAGR+1
  100   CONTINUE
        IF(NAGR.GE.1) THEN
          RAGR=0.00001+(NAGR-0.00002)*RLU(0)
          IAGR=0
          DO 110 J=1,3
          IF(KFLR.EQ.KFL(J)) RAGR=RAGR-1.
          IF(IAGR.EQ.0.AND.RAGR.LE.0.) IAGR=J
  110     CONTINUE
        ELSE
          IAGR=1.00001+2.99998*RLU(0)
        ENDIF
        ID1=1
        IF(IAGR.EQ.1) ID1=2
        IF(IAGR.EQ.1.AND.KFL(3).GT.KFL(2)) ID1=3
        ID2=6-IAGR-ID1
        KSP=3
        IF(MOD(KFA,10).EQ.2.AND.KFL(1).EQ.KFL(2)) THEN
          IF(IAGR.NE.3.AND.RLU(0).GT.0.25) KSP=1
        ELSEIF(MOD(KFA,10).EQ.2.AND.KFL(2).GE.KFL(3)) THEN
          IF(IAGR.NE.1.AND.RLU(0).GT.0.25) KSP=1
        ELSEIF(MOD(KFA,10).EQ.2) THEN
          IF(IAGR.EQ.1) KSP=1
          IF(IAGR.NE.1.AND.RLU(0).GT.0.75) KSP=1
        ENDIF
        KFLSP=1000*KFL(ID1)+100*KFL(ID2)+KSP
        IF(KFLR.EQ.21) THEN
          KFLCH=KFL(IAGR)
        ELSEIF(NAGR.EQ.0.AND.KFLR.GT.0) THEN
          CALL LUKFDI(-KFLR,KFL(IAGR),KFDUMP,KFLCH)
        ELSEIF(NAGR.EQ.0) THEN
          CALL LUKFDI(10000+KFLSP,-KFLR,KFDUMP,KFLCH)
          KFLSP=KFL(IAGR)
        ENDIF
      ENDIF
 
C...Add on correct sign for result.
      KFLCH=KFLCH*KFS
      KFLSP=KFLSP*KFS
 
      RETURN
      END
 
C*********************************************************************
 
      FUNCTION PYGAMM(X)
 
C...Gives ordinary Gamma function Gamma(x) for positive, real arguments;
C...see M. Abramowitz, I. A. Stegun: Handbook of Mathematical Functions
C...(Dover, 1965) 6.1.36.
      DIMENSION B(8)
      DATA B/-0.577191652,0.988205891,-0.897056937,0.918206857,
     &-0.756704078,0.482199394,-0.193527818,0.035868343/
 
      NX=INT(X)
      DX=X-NX
 
      PYGAMM=1.
      DXP=1.
      DO 100 I=1,8
      DXP=DXP*DX
      PYGAMM=PYGAMM+B(I)*DXP
  100 CONTINUE
      IF(X.LT.1.) THEN
        PYGAMM=PYGAMM/X
      ELSE
        DO 110 IX=1,NX-1
        PYGAMM=(X-IX)*PYGAMM
  110   CONTINUE
      ENDIF
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYWAUX(IAUX,EPS,WRE,WIM)
 
C...Calculates real and imaginary parts of the auxiliary functions W1
C...and W2; see R. K. Ellis, I. Hinchliffe, M. Soldate and J. J. van
C...der Bij, Nucl. Phys. B297 (1988) 221.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      ASINH(X)=LOG(X+SQRT(X**2+1.))
      ACOSH(X)=LOG(X+SQRT(X**2-1.))
 
      IF(EPS.LT.0.) THEN
        IF(IAUX.EQ.1) WRE=2.*SQRT(1.-EPS)*ASINH(SQRT(-1./EPS))
        IF(IAUX.EQ.2) WRE=4.*(ASINH(SQRT(-1./EPS)))**2
        WIM=0.
      ELSEIF(EPS.LT.1.) THEN
        IF(IAUX.EQ.1) WRE=2.*SQRT(1.-EPS)*ACOSH(SQRT(1./EPS))
        IF(IAUX.EQ.2) WRE=4.*(ACOSH(SQRT(1./EPS)))**2-PARU(1)**2
        IF(IAUX.EQ.1) WIM=-PARU(1)*SQRT(1.-EPS)
        IF(IAUX.EQ.2) WIM=-4.*PARU(1)*ACOSH(SQRT(1./EPS))
      ELSE
        IF(IAUX.EQ.1) WRE=2.*SQRT(EPS-1.)*ASIN(SQRT(1./EPS))
        IF(IAUX.EQ.2) WRE=-4.*(ASIN(SQRT(1./EPS)))**2
        WIM=0.
      ENDIF
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYI3AU(EPS,RAT,Y3RE,Y3IM)
 
C...Calculates real and imaginary parts of the auxiliary function I3;
C...see R. K. Ellis, I. Hinchliffe, M. Soldate and J. J. van der Bij,
C...Nucl. Phys. B297 (1988) 221.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      BE=0.5*(1.+SQRT(1.+RAT*EPS))
      IF(EPS.LT.1.) GA=0.5*(1.+SQRT(1.-EPS))
 
      IF(EPS.LT.0.) THEN
        IF(ABS(EPS).LT.1.E-4.AND.ABS(RAT*EPS).LT.1.E-4) THEN
          F3RE=PYSPEN(-0.25*EPS/(1.+0.25*(RAT-1.)*EPS),0.,1)-
     &    PYSPEN((1.-0.25*EPS)/(1.+0.25*(RAT-1.)*EPS),0.,1)+
     &    PYSPEN(0.25*(RAT+1.)*EPS/(1.+0.25*RAT*EPS),0.,1)-
     &    PYSPEN((RAT+1.)/RAT,0.,1)+0.5*(LOG(1.+0.25*RAT*EPS)**2-
     &    LOG(0.25*RAT*EPS)**2)+LOG(1.-0.25*EPS)*
     &    LOG((1.+0.25*(RAT-1.)*EPS)/(1.+0.25*RAT*EPS))+
     &    LOG(-0.25*EPS)*LOG(0.25*RAT*EPS/(1.+0.25*(RAT-1.)*EPS))
        ELSEIF(ABS(EPS).LT.1.E-4.AND.ABS(RAT*EPS).GE.1.E-4) THEN
          F3RE=PYSPEN(-0.25*EPS/(BE-0.25*EPS),0.,1)-
     &    PYSPEN((1.-0.25*EPS)/(BE-0.25*EPS),0.,1)+
     &    PYSPEN((BE-1.+0.25*EPS)/BE,0.,1)-
     &    PYSPEN((BE-1.+0.25*EPS)/(BE-1.),0.,1)+
     &    0.5*(LOG(BE)**2-LOG(BE-1.)**2)+
     &    LOG(1.-0.25*EPS)*LOG((BE-0.25*EPS)/BE)+
     &    LOG(-0.25*EPS)*LOG((BE-1.)/(BE-0.25*EPS))
        ELSEIF(ABS(EPS).GE.1.E-4.AND.ABS(RAT*EPS).LT.1.E-4) THEN
          F3RE=PYSPEN((GA-1.)/(GA+0.25*RAT*EPS),0.,1)-
     &    PYSPEN(GA/(GA+0.25*RAT*EPS),0.,1)+
     &    PYSPEN((1.+0.25*RAT*EPS-GA)/(1.+0.25*RAT*EPS),0.,1)-
     &    PYSPEN((1.+0.25*RAT*EPS-GA)/(0.25*RAT*EPS),0.,1)+
     &    0.5*(LOG(1.+0.25*RAT*EPS)**2-LOG(0.25*RAT*EPS)**2)+
     &    LOG(GA)*LOG((GA+0.25*RAT*EPS)/(1.+0.25*RAT*EPS))+
     &    LOG(GA-1.)*LOG(0.25*RAT*EPS/(GA+0.25*RAT*EPS))
        ELSE
          F3RE=PYSPEN((GA-1.)/(GA+BE-1.),0.,1)-
     &    PYSPEN(GA/(GA+BE-1.),0.,1)+PYSPEN((BE-GA)/BE,0.,1)-
     &    PYSPEN((BE-GA)/(BE-1.),0.,1)+0.5*(LOG(BE)**2-LOG(BE-1.)**2)+
     &    LOG(GA)*LOG((GA+BE-1.)/BE)+LOG(GA-1.)*LOG((BE-1.)/(GA+BE-1.))
        ENDIF
        F3IM=0.
      ELSEIF(EPS.LT.1.) THEN
        IF(ABS(EPS).LT.1.E-4.AND.ABS(RAT*EPS).LT.1.E-4) THEN
          F3RE=PYSPEN(-0.25*EPS/(1.+0.25*(RAT-1.)*EPS),0.,1)-
     &    PYSPEN((1.-0.25*EPS)/(1.+0.25*(RAT-1.)*EPS),0.,1)+
     &    PYSPEN((1.-0.25*EPS)/(-0.25*(RAT+1.)*EPS),0.,1)-
     &    PYSPEN(1./(RAT+1.),0.,1)+LOG((1.-0.25*EPS)/(0.25*EPS))*
     &    LOG((1.+0.25*(RAT-1.)*EPS)/(0.25*(RAT+1.)*EPS))
          F3IM=-PARU(1)*LOG((1.+0.25*(RAT-1.)*EPS)/(0.25*(RAT+1.)*EPS))
        ELSEIF(ABS(EPS).LT.1.E-4.AND.ABS(RAT*EPS).GE.1.E-4) THEN
          F3RE=PYSPEN(-0.25*EPS/(BE-0.25*EPS),0.,1)-
     &    PYSPEN((1.-0.25*EPS)/(BE-0.25*EPS),0.,1)+
     &    PYSPEN((1.-0.25*EPS)/(1.-0.25*EPS-BE),0.,1)-
     &    PYSPEN(-0.25*EPS/(1.-0.25*EPS-BE),0.,1)+
     &    LOG((1.-0.25*EPS)/(0.25*EPS))*
     &    LOG((BE-0.25*EPS)/(BE-1.+0.25*EPS))
          F3IM=-PARU(1)*LOG((BE-0.25*EPS)/(BE-1.+0.25*EPS))
        ELSEIF(ABS(EPS).GE.1.E-4.AND.ABS(RAT*EPS).LT.1.E-4) THEN
          F3RE=PYSPEN((GA-1.)/(GA+0.25*RAT*EPS),0.,1)-
     &    PYSPEN(GA/(GA+0.25*RAT*EPS),0.,1)+
     &    PYSPEN(GA/(GA-1.-0.25*RAT*EPS),0.,1)-
     &    PYSPEN((GA-1.)/(GA-1.-0.25*RAT*EPS),0.,1)+
     &    LOG(GA/(1.-GA))*LOG((GA+0.25*RAT*EPS)/(1.+0.25*RAT*EPS-GA))
          F3IM=-PARU(1)*LOG((GA+0.25*RAT*EPS)/(1.+0.25*RAT*EPS-GA))
        ELSE
          F3RE=PYSPEN((GA-1.)/(GA+BE-1.),0.,1)-
     &    PYSPEN(GA/(GA+BE-1.),0.,1)+PYSPEN(GA/(GA-BE),0.,1)-
     &    PYSPEN((GA-1.)/(GA-BE),0.,1)+LOG(GA/(1.-GA))*
     &    LOG((GA+BE-1.)/(BE-GA))
          F3IM=-PARU(1)*LOG((GA+BE-1.)/(BE-GA))
         ENDIF
      ELSE
        RSQ=EPS/(EPS-1.+(2.*BE-1.)**2)
        RCTHE=RSQ*(1.-2.*BE/EPS)
        RSTHE=SQRT(MAX(0.,RSQ-RCTHE**2))
        RCPHI=RSQ*(1.+2.*(BE-1.)/EPS)
        RSPHI=SQRT(MAX(0.,RSQ-RCPHI**2))
        R=SQRT(RSQ)
        THE=ACOS(MAX(-0.999999,MIN(0.999999,RCTHE/R)))
        PHI=ACOS(MAX(-0.999999,MIN(0.999999,RCPHI/R)))
        F3RE=PYSPEN(RCTHE,RSTHE,1)+PYSPEN(RCTHE,-RSTHE,1)-
     &  PYSPEN(RCPHI,RSPHI,1)-PYSPEN(RCPHI,-RSPHI,1)+
     &  (PHI-THE)*(PHI+THE-PARU(1))
        F3IM=PYSPEN(RCTHE,RSTHE,2)+PYSPEN(RCTHE,-RSTHE,2)-
     &  PYSPEN(RCPHI,RSPHI,2)-PYSPEN(RCPHI,-RSPHI,2)
      ENDIF
 
      Y3RE=2./(2.*BE-1.)*F3RE
      Y3IM=2./(2.*BE-1.)*F3IM
 
      RETURN
      END
 
C***********************************************************************
 
      FUNCTION PYSPEN(XREIN,XIMIN,IREIM)
 
C...Calculates real and imaginary part of Spence function; see
C...G. 't Hooft and M. Veltman, Nucl. Phys. B153 (1979) 365.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      DIMENSION B(0:14)
 
      DATA B/
     & 1.000000E+00,        -5.000000E-01,         1.666667E-01,
     & 0.000000E+00,        -3.333333E-02,         0.000000E+00,
     & 2.380952E-02,         0.000000E+00,        -3.333333E-02,
     & 0.000000E+00,         7.575757E-02,         0.000000E+00,
     &-2.531135E-01,         0.000000E+00,         1.166667E+00/
 
      XRE=XREIN
      XIM=XIMIN
      IF(ABS(1.-XRE).LT.1.E-6.AND.ABS(XIM).LT.1.E-6) THEN
        IF(IREIM.EQ.1) PYSPEN=PARU(1)**2/6.
        IF(IREIM.EQ.2) PYSPEN=0.
        RETURN
      ENDIF
 
      XMOD=SQRT(XRE**2+XIM**2)
      IF(XMOD.LT.1.E-6) THEN
        IF(IREIM.EQ.1) PYSPEN=0.
        IF(IREIM.EQ.2) PYSPEN=0.
        RETURN
      ENDIF
 
      XARG=SIGN(ACOS(XRE/XMOD),XIM)
      SP0RE=0.
      SP0IM=0.
      SGN=1.
      IF(XMOD.GT.1.) THEN
        ALGXRE=LOG(XMOD)
        ALGXIM=XARG-SIGN(PARU(1),XARG)
        SP0RE=-PARU(1)**2/6.-(ALGXRE**2-ALGXIM**2)/2.
        SP0IM=-ALGXRE*ALGXIM
        SGN=-1.
        XMOD=1./XMOD
        XARG=-XARG
        XRE=XMOD*COS(XARG)
        XIM=XMOD*SIN(XARG)
      ENDIF
      IF(XRE.GT.0.5) THEN
        ALGXRE=LOG(XMOD)
        ALGXIM=XARG
        XRE=1.-XRE
        XIM=-XIM
        XMOD=SQRT(XRE**2+XIM**2)
        XARG=SIGN(ACOS(XRE/XMOD),XIM)
        ALGYRE=LOG(XMOD)
        ALGYIM=XARG
        SP0RE=SP0RE+SGN*(PARU(1)**2/6.-(ALGXRE*ALGYRE-ALGXIM*ALGYIM))
        SP0IM=SP0IM-SGN*(ALGXRE*ALGYIM+ALGXIM*ALGYRE)
        SGN=-SGN
      ENDIF
 
      XRE=1.-XRE
      XIM=-XIM
      XMOD=SQRT(XRE**2+XIM**2)
      XARG=SIGN(ACOS(XRE/XMOD),XIM)
      ZRE=-LOG(XMOD)
      ZIM=-XARG
 
      SPRE=0.
      SPIM=0.
      SAVERE=1.
      SAVEIM=0.
      DO 100 I=0,14
      IF(MAX(ABS(SAVERE),ABS(SAVEIM)).LT.1E-30) GOTO 110
      TERMRE=(SAVERE*ZRE-SAVEIM*ZIM)/FLOAT(I+1)
      TERMIM=(SAVERE*ZIM+SAVEIM*ZRE)/FLOAT(I+1)
      SAVERE=TERMRE
      SAVEIM=TERMIM
      SPRE=SPRE+B(I)*TERMRE
      SPIM=SPIM+B(I)*TERMIM
  100 CONTINUE
 
  110 IF(IREIM.EQ.1) PYSPEN=SP0RE+SGN*SPRE
      IF(IREIM.EQ.2) PYSPEN=SP0IM+SGN*SPIM
 
      RETURN
      END
 
C***********************************************************************
 
      SUBROUTINE PYQQBH(WTQQBH)
 
C...Calculates the matrix element for the processes
C...g + g or q + qbar -> Q + Q~ + H (normally with Q = t).
C...REDUCE output and part of the rest courtesy Z. Kunszt, see
C...Z. Kunszt, Nucl. Phys. B247 (1984) 339.
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
      DIMENSION PP(15,4),CLR(8,8),FM(10,10),RM(8,8),DX(8)
      DOT(I,J)=PP(I,4)*PP(J,4)-PP(I,1)*PP(J,1)-PP(I,2)*PP(J,2)-
     &PP(I,3)*PP(J,3)
 
C...Mass parameters.
      WTQQBH=0.
      ISUB=MINT(1)
      SHPR=SQRT(VINT(26))*VINT(1)
      PQ=PMAS(KFPR(ISUB,2),1)
      PH=SQRT(VINT(21))*VINT(1)
      SPQ=PQ**2
      SPH=PH**2
 
C...Set up outgoing kinematics: 1=t, 2=tbar, 3=H.
      DO 100 I=1,2
      PT=SQRT(MAX(0.,VINT(197+5*I)))
      PP(I,1)=PT*COS(VINT(198+5*I))
      PP(I,2)=PT*SIN(VINT(198+5*I))
  100 CONTINUE
      PP(3,1)=-PP(1,1)-PP(2,1)
      PP(3,2)=-PP(1,2)-PP(2,2)
      PMS1=SPQ+PP(1,1)**2+PP(1,2)**2
      PMS2=SPQ+PP(2,1)**2+PP(2,2)**2
      PMS3=SPH+PP(3,1)**2+PP(3,2)**2
      PMT3=SQRT(PMS3)
      PP(3,3)=PMT3*SINH(VINT(211))
      PP(3,4)=PMT3*COSH(VINT(211))
      PMS12=(SHPR-PP(3,4))**2-PP(3,3)**2
      PP(1,3)=(-PP(3,3)*(PMS12+PMS1-PMS2)+
     &VINT(213)*(SHPR-PP(3,4))*VINT(220))/(2.*PMS12)
      PP(2,3)=-PP(1,3)-PP(3,3)
      PP(1,4)=SQRT(PMS1+PP(1,3)**2)
      PP(2,4)=SQRT(PMS2+PP(2,3)**2)
 
C...Set up incoming kinematics and derived momentum combinations.
      DO 110 I=4,5
      PP(I,1)=0.
      PP(I,2)=0.
      PP(I,3)=-0.5*SHPR*(-1)**I
      PP(I,4)=-0.5*SHPR
  110 CONTINUE
      DO 120 J=1,4
      PP(6,J)=PP(1,J)+PP(2,J)
      PP(7,J)=PP(1,J)+PP(3,J)
      PP(8,J)=PP(1,J)+PP(4,J)
      PP(9,J)=PP(1,J)+PP(5,J)
      PP(10,J)=-PP(2,J)-PP(3,J)
      PP(11,J)=-PP(2,J)-PP(4,J)
      PP(12,J)=-PP(2,J)-PP(5,J)
      PP(13,J)=-PP(4,J)-PP(5,J)
  120 CONTINUE
 
C...Derived kinematics invariants.
      X1=DOT(1,2)
      X2=DOT(1,3)
      X3=DOT(1,4)
      X4=DOT(1,5)
      X5=DOT(2,3)
      X6=DOT(2,4)
      X7=DOT(2,5)
      X8=DOT(3,4)
      X9=DOT(3,5)
      X10=DOT(4,5)
 
C...Propagators.
      SS1=DOT(7,7)-SPQ
      SS2=DOT(8,8)-SPQ
      SS3=DOT(9,9)-SPQ
      SS4=DOT(10,10)-SPQ
      SS5=DOT(11,11)-SPQ
      SS6=DOT(12,12)-SPQ
      SS7=DOT(13,13)
      DX(1)=SS1*SS6
      DX(2)=SS2*SS6
      DX(3)=SS2*SS4
      DX(4)=SS1*SS5
      DX(5)=SS3*SS5
      DX(6)=SS3*SS4
      DX(7)=SS7*SS1
      DX(8)=SS7*SS4
 
C...Define colour coefficients for g + g -> Q + Q~ + H.
      IF(ISUB.EQ.121.OR.ISUB.EQ.181.OR.ISUB.EQ.186) THEN
        DO 140 I=1,3
        DO 130 J=1,3
        CLR(I,J)=16./3.
        CLR(I+3,J+3)=16./3.
        CLR(I,J+3)=-2./3.
        CLR(I+3,J)=-2./3.
  130   CONTINUE
  140   CONTINUE
        DO 160 L=1,2
        DO 150 I=1,3
        CLR(I,6+L)=-6.
        CLR(I+3,6+L)=6.
        CLR(6+L,I)=-6.
        CLR(6+L,I+3)=6.
  150   CONTINUE
  160   CONTINUE
        DO 180 K1=1,2
        DO 170 K2=1,2
        CLR(6+K1,6+K2)=12.
  170   CONTINUE
  180   CONTINUE
 
C...Evaluate matrix elements for g + g -> Q + Q~ + H.
      FM(1,1)=64*PQ**6+16*PQ**4*PH**2+32*PQ**4*(X1+2*X2+X4+X9+2*
     & X7+X5)+8*PQ**2*PH**2*(-X1-X4+2*X7)+16*PQ**2*(X2*X9+4*X2*
     & X7+X2*X5-2*X4*X7-2*X9*X7)+8*PH**2*X4*X7-16*X2*X9*X7
      FM(1,2)=16*PQ**6+8*PQ**4*(-2*X1+X2-2*X3-2*X4-4*X10+X9-X8+2
     & *X7-4*X6+X5)+8*PQ**2*(-2*X1*X2-2*X2*X4-2*X2*X10+X2*X7-2*
     & X2*X6-2*X3*X7+2*X4*X7+4*X10*X7-X9*X7-X8*X7)+16*X2*X7*(X4+
     & X10)
      FM(1,3)=16*PQ**6-4*PQ**4*PH**2+8*PQ**4*(-2*X1+2*X2-2*X3-4*
     & X4-8*X10+X9+X8-2*X7-4*X6+2*X5)-(4*PQ**2*PH**2)*(X1+X4+X10
     & +X6)+8*PQ**2*(-2*X1*X2-2*X1*X10+X1*X9+X1*X8-2*X1*X5+X2**2
     & -4*X2*X4-5*X2*X10+X2*X8-X2*X7-3*X2*X6+X2*X5+X3*X9+2*X3*X7
     & -X3*X5+X4*X8+2*X4*X6-3*X4*X5-5*X10*X5+X9*X8+X9*X6+X9*X5+
     & X8*X7-4*X6*X5+X5**2)-(16*X2*X5)*(X1+X4+X10+X6)
      FM(1,4)=16*PQ**6+4*PQ**4*PH**2+16*PQ**4*(-X1+X2-X3-X4+X10-
     & X9-X8+2*X7+2*X6-X5)+4*PQ**2*PH**2*(X1+X3+X4+X10+2*X7+2*X6
     & )+8*PQ**2*(4*X1*X10+4*X1*X7+4*X1*X6+2*X2*X10-X2*X9-X2*X8+
     & 4*X2*X7+4*X2*X6-X2*X5+4*X10*X5+4*X7*X5+4*X6*X5)-(8*PH**2*
     & X1)*(X10+X7+X6)+16*X2*X5*(X10+X7+X6)
      FM(1,5)=8*PQ**4*(-2*X1-2*X4+X10-X9)+4*PQ**2*(4*X1**2-2*X1*
     & X2+8*X1*X3+6*X1*X10-2*X1*X9+4*X1*X8+4*X1*X7+4*X1*X6+2*X1*
     & X5+X2*X10+4*X3*X4-X3*X9+2*X3*X7+3*X4*X8-2*X4*X6+2*X4*X5-4
     & *X10*X7+3*X10*X5-3*X9*X6+3*X8*X7-4*X7**2+4*X7*X5)+8*(X1**
     & 2*X9-X1**2*X8-X1*X2*X7+X1*X2*X6+X1*X3*X9+X1*X3*X5-X1*X4*
     & X8-X1*X4*X5+X1*X10*X9+X1*X9*X7+X1*X9*X6-X1*X8*X7-X2*X3*X7
     & +X2*X4*X6-X2*X10*X7-X2*X7**2+X3*X7*X5-X4*X10*X5-X4*X7*X5-
     & X4*X6*X5)
      FM(1,6)=16*PQ**4*(-4*X1-X4+X9-X7)+4*PQ**2*PH**2*(-2*X1-X4-
     & X7)+16*PQ**2*(-2*X1**2-3*X1*X2-2*X1*X4-3*X1*X9-2*X1*X7-3*
     & X1*X5-2*X2*X4-2*X7*X5)-8*PH**2*X4*X7+8*(-X1*X2*X9-2*X1*X2
     & *X5-X1*X9**2-X1*X9*X5+X2**2*X7-X2*X4*X5+X2*X9*X7-X2*X7*X5
     & +X4*X9*X5+X4*X5**2)
      FM(1,7)=8*PQ**4*(2*X3+X4+3*X10+X9+2*X8+3*X7+6*X6)+2*PQ**2*
     & PH**2*(-2*X3-X4+3*X10+3*X7+6*X6)+4*PQ**2*(4*X1*X10+4*X1*
     & X7+8*X1*X6+6*X2*X10+X2*X9+2*X2*X8+6*X2*X7+12*X2*X6-8*X3*
     & X7+4*X4*X7+4*X4*X6+4*X10*X5+4*X9*X7+4*X9*X6-8*X8*X7+4*X7*
     & X5+8*X6*X5)+4*PH**2*(-X1*X10-X1*X7-2*X1*X6+2*X3*X7-X4*X7-
     & X4*X6)+8*X2*(X10*X5+X9*X7+X9*X6-2*X8*X7+X7*X5+2*X6*X5)
      FM(1,8)=8*PQ**4*(2*X3+X4+3*X10+2*X9+X8+3*X7+6*X6)+2*PQ**2*
     & PH**2*(-2*X3-X4+2*X10+X7+2*X6)+4*PQ**2*(4*X1*X10-2*X1*X9+
     & 2*X1*X8+4*X1*X7+8*X1*X6+5*X2*X10+2*X2*X9+X2*X8+4*X2*X7+8*
     & X2*X6-X3*X9-8*X3*X7+2*X3*X5+2*X4*X9-X4*X8+4*X4*X7+4*X4*X6
     & +4*X4*X5+5*X10*X5+X9**2-X9*X8+2*X9*X7+5*X9*X6+X9*X5-7*X8*
     & X7+2*X8*X5+2*X7*X5+10*X6*X5)+2*PH**2*(-X1*X10+X3*X7-2*X4*
     & X7+X4*X6)+4*(-X1*X9**2+X1*X9*X8-2*X1*X9*X5-X1*X8*X5+2*X2*
     & X10*X5+X2*X9*X7+X2*X9*X6-2*X2*X8*X7+3*X2*X6*X5+X3*X9*X5+
     & X3*X5**2+X4*X9*X5-2*X4*X8*X5+2*X4*X5**2)
      FM(2,2)=16*PQ**6+16*PQ**4*(-X1+X3-X4-X10+X7-X6)+16*PQ**2*(
     & X3*X10+X3*X7+X3*X6+X4*X7+X10*X7)-16*X3*X10*X7
      FM(2,3)=16*PQ**6+8*PQ**4*(-2*X1+X2+2*X3-4*X4-4*X10-X9+X8-2
     & *X7-2*X6+X5)+8*PQ**2*(-2*X1*X5+4*X3*X10-X3*X9-X3*X8-2*X3*
     & X7+2*X3*X6+X3*X5-2*X4*X5-2*X10*X5-2*X6*X5)+16*X3*X5*(X10+
     & X6)
      FM(2,4)=8*PQ**4*(-2*X1-2*X3+X10-X8)+4*PQ**2*(4*X1**2-2*X1*
     & X2+8*X1*X4+6*X1*X10+4*X1*X9-2*X1*X8+4*X1*X7+4*X1*X6+2*X1*
     & X5+X2*X10+4*X3*X4+3*X3*X9-2*X3*X7+2*X3*X5-X4*X8+2*X4*X6-4
     & *X10*X6+3*X10*X5+3*X9*X6-3*X8*X7-4*X6**2+4*X6*X5)+8*(-X1
     & **2*X9+X1**2*X8+X1*X2*X7-X1*X2*X6-X1*X3*X9-X1*X3*X5+X1*X4
     & *X8+X1*X4*X5+X1*X10*X8-X1*X9*X6+X1*X8*X7+X1*X8*X6+X2*X3*
     & X7-X2*X4*X6-X2*X10*X6-X2*X6**2-X3*X10*X5-X3*X7*X5-X3*X6*
     & X5+X4*X6*X5)
      FM(2,5)=16*PQ**4*X10+8*PQ**2*(2*X1**2+2*X1*X3+2*X1*X4+2*X1
     & *X10+2*X1*X7+2*X1*X6+X3*X7+X4*X6)+8*(-2*X1**3-2*X1**2*X3-
     & 2*X1**2*X4-2*X1**2*X10-2*X1**2*X7-2*X1**2*X6-2*X1*X3*X4-
     & X1*X3*X10-2*X1*X3*X6-X1*X4*X10-2*X1*X4*X7-X1*X10**2-X1*
     & X10*X7-X1*X10*X6-2*X1*X7*X6+X3**2*X7-X3*X4*X7-X3*X4*X6+X3
     & *X10*X7+X3*X7**2-X3*X7*X6+X4**2*X6+X4*X10*X6-X4*X7*X6+X4*
     & X6**2)
      FM(2,6)=8*PQ**4*(-2*X1+X10-X9-2*X7)+4*PQ**2*(4*X1**2+2*X1*
     & X2+4*X1*X3+4*X1*X4+6*X1*X10-2*X1*X9+4*X1*X8+8*X1*X6-2*X1*
     & X5+4*X2*X4+3*X2*X10+2*X2*X7-3*X3*X9-2*X3*X7-4*X4**2-4*X4*
     & X10+3*X4*X8+2*X4*X6+X10*X5-X9*X6+3*X8*X7+4*X7*X6)+8*(X1**
     & 2*X9-X1**2*X8-X1*X2*X7+X1*X2*X6+X1*X3*X9+X1*X3*X5+X1*X4*
     & X9-X1*X4*X8-X1*X4*X5+X1*X10*X9+X1*X9*X6-X1*X8*X7-X2*X3*X7
     & -X2*X4*X7+X2*X4*X6-X2*X10*X7+X3*X7*X5-X4**2*X5-X4*X10*X5-
     & X4*X6*X5)
      FM(2,7)=8*PQ**4*(X3+2*X4+3*X10+X7+2*X6)+4*PQ**2*(-4*X1*X3-
     & 2*X1*X4-2*X1*X10+X1*X9-X1*X8-4*X1*X7-2*X1*X6+X2*X3+2*X2*
     & X4+3*X2*X10+X2*X7+2*X2*X6-6*X3*X4-6*X3*X10-2*X3*X9-2*X3*
     & X7-4*X3*X6-X3*X5-6*X4**2-6*X4*X10-3*X4*X9-X4*X8-4*X4*X7-2
     & *X4*X6-2*X4*X5-3*X10*X9-3*X10*X8-6*X10*X7-6*X10*X6+X10*X5
     & +X9*X7-2*X8*X7-2*X8*X6-6*X7*X6+X7*X5-6*X6**2+2*X6*X5)+4*(
     & -X1**2*X9+X1**2*X8-2*X1*X2*X10-3*X1*X2*X7-3*X1*X2*X6+X1*
     & X3*X9-X1*X3*X5+X1*X4*X9+X1*X4*X8+X1*X4*X5+X1*X10*X9+X1*
     & X10*X8-X1*X9*X6+X1*X8*X6+X2*X3*X7-3*X2*X4*X7-X2*X4*X6-3*
     & X2*X10*X7-3*X2*X10*X6-3*X2*X7*X6-3*X2*X6**2-2*X3*X4*X5-X3
     & *X10*X5-X3*X6*X5-X4**2*X5-X4*X10*X5+X4*X6*X5)
      FM(2,8)=8*PQ**4*(X3+2*X4+3*X10+X7+2*X6)+4*PQ**2*(-4*X1*X3-
     & 2*X1*X4-2*X1*X10-X1*X9+X1*X8-4*X1*X7-2*X1*X6+X2*X3+2*X2*
     & X4+X2*X10-X2*X7-2*X2*X6-6*X3*X4-6*X3*X10-2*X3*X9+X3*X8-2*
     & X3*X7-4*X3*X6+X3*X5-6*X4**2-6*X4*X10-2*X4*X9-4*X4*X7-2*X4
     & *X6+2*X4*X5-3*X10*X9-3*X10*X8-6*X10*X7-6*X10*X6+3*X10*X5-
     & X9*X6-2*X8*X7-3*X8*X6-6*X7*X6+X7*X5-6*X6**2+2*X6*X5)+4*(
     & X1**2*X9-X1**2*X8-X1*X2*X7+X1*X2*X6-3*X1*X3*X5+X1*X4*X9-
     & X1*X4*X8-3*X1*X4*X5+X1*X10*X9+X1*X10*X8-2*X1*X10*X5+X1*X9
     & *X6+X1*X8*X7+X1*X8*X6-X2*X4*X7+X2*X4*X6-X2*X10*X7-X2*X10*
     & X6-2*X2*X7*X6-X2*X6**2-3*X3*X4*X5-3*X3*X10*X5+X3*X7*X5-3*
     & X3*X6*X5-3*X4**2*X5-3*X4*X10*X5-X4*X6*X5)
      FM(3,3)=64*PQ**6+16*PQ**4*PH**2+32*PQ**4*(X1+X2+2*X3+X8+X6
     & +2*X5)+8*PQ**2*PH**2*(-X1+2*X3-X6)+16*PQ**2*(X2*X5-2*X3*
     & X8-2*X3*X6+4*X3*X5+X8*X5)+8*PH**2*X3*X6-16*X3*X8*X5
      FM(3,4)=16*PQ**4*(-4*X1-X3+X8-X6)+4*PQ**2*PH**2*(-2*X1-X3-
     & X6)+16*PQ**2*(-2*X1**2-3*X1*X2-2*X1*X3-3*X1*X8-2*X1*X6-3*
     & X1*X5-2*X2*X3-2*X6*X5)-8*PH**2*X3*X6+8*(-X1*X2*X8-2*X1*X2
     & *X5-X1*X8**2-X1*X8*X5+X2**2*X6-X2*X3*X5+X2*X8*X6-X2*X6*X5
     & +X3*X8*X5+X3*X5**2)
      FM(3,5)=8*PQ**4*(-2*X1+X10-X8-2*X6)+4*PQ**2*(4*X1**2+2*X1*
     & X2+4*X1*X3+4*X1*X4+6*X1*X10+4*X1*X9-2*X1*X8+8*X1*X7-2*X1*
     & X5+4*X2*X3+3*X2*X10+2*X2*X6-4*X3**2-4*X3*X10+3*X3*X9+2*X3
     & *X7-3*X4*X8-2*X4*X6+X10*X5+3*X9*X6-X8*X7+4*X7*X6)+8*(-X1
     & **2*X9+X1**2*X8+X1*X2*X7-X1*X2*X6-X1*X3*X9+X1*X3*X8-X1*X3
     & *X5+X1*X4*X8+X1*X4*X5+X1*X10*X8-X1*X9*X6+X1*X8*X7+X2*X3*
     & X7-X2*X3*X6-X2*X4*X6-X2*X10*X6-X3**2*X5-X3*X10*X5-X3*X7*
     & X5+X4*X6*X5)
      FM(3,6)=16*PQ**6+4*PQ**4*PH**2+16*PQ**4*(-X1-X2+2*X3+2*X4+
     & X10-X9-X8-X7-X6+X5)+4*PQ**2*PH**2*(X1+2*X3+2*X4+X10+X7+X6
     & )+8*PQ**2*(4*X1*X3+4*X1*X4+4*X1*X10+4*X2*X3+4*X2*X4+4*X2*
     & X10-X2*X5+4*X3*X5+4*X4*X5+2*X10*X5-X9*X5-X8*X5)-(8*PH**2*
     & X1)*(X3+X4+X10)+16*X2*X5*(X3+X4+X10)
      FM(3,7)=8*PQ**4*(3*X3+6*X4+3*X10+X9+2*X8+2*X7+X6)+2*PQ**2*
     & PH**2*(X3+2*X4+2*X10-2*X7-X6)+4*PQ**2*(4*X1*X3+8*X1*X4+4*
     & X1*X10+2*X1*X9-2*X1*X8+2*X2*X3+10*X2*X4+5*X2*X10+2*X2*X9+
     & X2*X8+2*X2*X7+4*X2*X6-7*X3*X9+2*X3*X8-8*X3*X7+4*X3*X6+4*
     & X3*X5+5*X4*X8+4*X4*X6+8*X4*X5+5*X10*X5-X9*X8-X9*X6+X9*X5+
     & X8**2-X8*X7+2*X8*X6+2*X8*X5)+2*PH**2*(-X1*X10+X3*X7-2*X3*
     & X6+X4*X6)+4*(-X1*X2*X9-2*X1*X2*X8+X1*X9*X8-X1*X8**2+X2**2
     & *X7+2*X2**2*X6+3*X2*X4*X5+2*X2*X10*X5-2*X2*X9*X6+X2*X8*X7
     & +X2*X8*X6-2*X3*X9*X5+X3*X8*X5+X4*X8*X5)
      FM(3,8)=8*PQ**4*(3*X3+6*X4+3*X10+2*X9+X8+2*X7+X6)+2*PQ**2*
     & PH**2*(3*X3+6*X4+3*X10-2*X7-X6)+4*PQ**2*(4*X1*X3+8*X1*X4+
     & 4*X1*X10+4*X2*X3+8*X2*X4+4*X2*X10-8*X3*X9+4*X3*X8-8*X3*X7
     & +4*X3*X6+6*X3*X5+4*X4*X8+4*X4*X6+12*X4*X5+6*X10*X5+2*X9*
     & X5+X8*X5)+4*PH**2*(-X1*X3-2*X1*X4-X1*X10+2*X3*X7-X3*X6-X4
     & *X6)+8*X5*(X2*X3+2*X2*X4+X2*X10-2*X3*X9+X3*X8+X4*X8)
      FM(4,4)=64*PQ**6+16*PQ**4*PH**2+32*PQ**4*(X1+2*X2+X3+X8+2*
     & X6+X5)+8*PQ**2*PH**2*(-X1-X3+2*X6)+16*PQ**2*(X2*X8+4*X2*
     & X6+X2*X5-2*X3*X6-2*X8*X6)+8*PH**2*X3*X6-16*X2*X8*X6
      FM(4,5)=16*PQ**6+8*PQ**4*(-2*X1+X2-2*X3-2*X4-4*X10-X9+X8-4
     & *X7+2*X6+X5)+8*PQ**2*(-2*X1*X2-2*X2*X3-2*X2*X10-2*X2*X7+
     & X2*X6+2*X3*X6-2*X4*X6+4*X10*X6-X9*X6-X8*X6)+16*X2*X6*(X3+
     & X10)
      FM(4,6)=16*PQ**6-4*PQ**4*PH**2+8*PQ**4*(-2*X1+2*X2-4*X3-2*
     & X4-8*X10+X9+X8-4*X7-2*X6+2*X5)-(4*PQ**2*PH**2)*(X1+X3+X10
     & +X7)+8*PQ**2*(-2*X1*X2-2*X1*X10+X1*X9+X1*X8-2*X1*X5+X2**2
     & -4*X2*X3-5*X2*X10+X2*X9-3*X2*X7-X2*X6+X2*X5+X3*X9+2*X3*X7
     & -3*X3*X5+X4*X8+2*X4*X6-X4*X5-5*X10*X5+X9*X8+X9*X6+X8*X7+
     & X8*X5-4*X7*X5+X5**2)-(16*X2*X5)*(X1+X3+X10+X7)
      FM(4,7)=8*PQ**4*(-X3-2*X4-3*X10-2*X9-X8-6*X7-3*X6)+2*PQ**2
     & *PH**2*(X3+2*X4-3*X10-6*X7-3*X6)+4*PQ**2*(-4*X1*X10-8*X1*
     & X7-4*X1*X6-6*X2*X10-2*X2*X9-X2*X8-12*X2*X7-6*X2*X6-4*X3*
     & X7-4*X3*X6+8*X4*X6-4*X10*X5+8*X9*X6-4*X8*X7-4*X8*X6-8*X7*
     & X5-4*X6*X5)+4*PH**2*(X1*X10+2*X1*X7+X1*X6+X3*X7+X3*X6-2*
     & X4*X6)+8*X2*(-X10*X5+2*X9*X6-X8*X7-X8*X6-2*X7*X5-X6*X5)
      FM(4,8)=8*PQ**4*(-X3-2*X4-3*X10-X9-2*X8-6*X7-3*X6)+2*PQ**2
     & *PH**2*(X3+2*X4-2*X10-2*X7-X6)+4*PQ**2*(-4*X1*X10-2*X1*X9
     & +2*X1*X8-8*X1*X7-4*X1*X6-5*X2*X10-X2*X9-2*X2*X8-8*X2*X7-4
     & *X2*X6+X3*X9-2*X3*X8-4*X3*X7-4*X3*X6-4*X3*X5+X4*X8+8*X4*
     & X6-2*X4*X5-5*X10*X5+X9*X8+7*X9*X6-2*X9*X5-X8**2-5*X8*X7-2
     & *X8*X6-X8*X5-10*X7*X5-2*X6*X5)+2*PH**2*(X1*X10-X3*X7+2*X3
     & *X6-X4*X6)+4*(-X1*X9*X8+X1*X9*X5+X1*X8**2+2*X1*X8*X5-2*X2
     & *X10*X5+2*X2*X9*X6-X2*X8*X7-X2*X8*X6-3*X2*X7*X5+2*X3*X9*
     & X5-X3*X8*X5-2*X3*X5**2-X4*X8*X5-X4*X5**2)
      FM(5,5)=16*PQ**6+16*PQ**4*(-X1-X3+X4-X10-X7+X6)+16*PQ**2*(
     & X3*X6+X4*X10+X4*X7+X4*X6+X10*X6)-16*X4*X10*X6
      FM(5,6)=16*PQ**6+8*PQ**4*(-2*X1+X2-4*X3+2*X4-4*X10+X9-X8-2
     & *X7-2*X6+X5)+8*PQ**2*(-2*X1*X5-2*X3*X5+4*X4*X10-X4*X9-X4*
     & X8+2*X4*X7-2*X4*X6+X4*X5-2*X10*X5-2*X7*X5)+16*X4*X5*(X10+
     & X7)
      FM(5,7)=8*PQ**4*(-2*X3-X4-3*X10-2*X7-X6)+4*PQ**2*(2*X1*X3+
     & 4*X1*X4+2*X1*X10+X1*X9-X1*X8+2*X1*X7+4*X1*X6-2*X2*X3-X2*
     & X4-3*X2*X10-2*X2*X7-X2*X6+6*X3**2+6*X3*X4+6*X3*X10+X3*X9+
     & 3*X3*X8+2*X3*X7+4*X3*X6+2*X3*X5+6*X4*X10+2*X4*X8+4*X4*X7+
     & 2*X4*X6+X4*X5+3*X10*X9+3*X10*X8+6*X10*X7+6*X10*X6-X10*X5+
     & 2*X9*X7+2*X9*X6-X8*X6+6*X7**2+6*X7*X6-2*X7*X5-X6*X5)+4*(-
     & X1**2*X9+X1**2*X8+2*X1*X2*X10+3*X1*X2*X7+3*X1*X2*X6-X1*X3
     & *X9-X1*X3*X8-X1*X3*X5-X1*X4*X8+X1*X4*X5-X1*X10*X9-X1*X10*
     & X8-X1*X9*X7+X1*X8*X7+X2*X3*X7+3*X2*X3*X6-X2*X4*X6+3*X2*
     & X10*X7+3*X2*X10*X6+3*X2*X7**2+3*X2*X7*X6+X3**2*X5+2*X3*X4
     & *X5+X3*X10*X5-X3*X7*X5+X4*X10*X5+X4*X7*X5)
      FM(5,8)=8*PQ**4*(-2*X3-X4-3*X10-2*X7-X6)+4*PQ**2*(2*X1*X3+
     & 4*X1*X4+2*X1*X10-X1*X9+X1*X8+2*X1*X7+4*X1*X6-2*X2*X3-X2*
     & X4-X2*X10+2*X2*X7+X2*X6+6*X3**2+6*X3*X4+6*X3*X10+2*X3*X8+
     & 2*X3*X7+4*X3*X6-2*X3*X5+6*X4*X10-X4*X9+2*X4*X8+4*X4*X7+2*
     & X4*X6-X4*X5+3*X10*X9+3*X10*X8+6*X10*X7+6*X10*X6-3*X10*X5+
     & 3*X9*X7+2*X9*X6+X8*X7+6*X7**2+6*X7*X6-2*X7*X5-X6*X5)+4*(
     & X1**2*X9-X1**2*X8-X1*X2*X7+X1*X2*X6+X1*X3*X9-X1*X3*X8+3*
     & X1*X3*X5+3*X1*X4*X5-X1*X10*X9-X1*X10*X8+2*X1*X10*X5-X1*X9
     & *X7-X1*X9*X6-X1*X8*X7-X2*X3*X7+X2*X3*X6+X2*X10*X7+X2*X10*
     & X6+X2*X7**2+2*X2*X7*X6+3*X3**2*X5+3*X3*X4*X5+3*X3*X10*X5+
     & X3*X7*X5+3*X4*X10*X5+3*X4*X7*X5-X4*X6*X5)
      FM(6,6)=64*PQ**6+16*PQ**4*PH**2+32*PQ**4*(X1+X2+2*X4+X9+X7
     & +2*X5)+8*PQ**2*PH**2*(-X1+2*X4-X7)+16*PQ**2*(X2*X5-2*X4*
     & X9-2*X4*X7+4*X4*X5+X9*X5)+8*PH**2*X4*X7-16*X4*X9*X5
      FM(6,7)=8*PQ**4*(-6*X3-3*X4-3*X10-2*X9-X8-X7-2*X6)+2*PQ**2
     & *PH**2*(-2*X3-X4-2*X10+X7+2*X6)+4*PQ**2*(-8*X1*X3-4*X1*X4
     & -4*X1*X10+2*X1*X9-2*X1*X8-10*X2*X3-2*X2*X4-5*X2*X10-X2*X9
     & -2*X2*X8-4*X2*X7-2*X2*X6-5*X3*X9-4*X3*X7-8*X3*X5-2*X4*X9+
     & 7*X4*X8-4*X4*X7+8*X4*X6-4*X4*X5-5*X10*X5-X9**2+X9*X8-2*X9
     & *X7+X9*X6-2*X9*X5+X8*X7-X8*X5)+2*PH**2*(X1*X10-X3*X7+2*X4
     & *X7-X4*X6)+4*(2*X1*X2*X9+X1*X2*X8+X1*X9**2-X1*X9*X8-2*X2
     & **2*X7-X2**2*X6-3*X2*X3*X5-2*X2*X10*X5-X2*X9*X7-X2*X9*X6+
     & 2*X2*X8*X7-X3*X9*X5-X4*X9*X5+2*X4*X8*X5)
      FM(6,8)=8*PQ**4*(-6*X3-3*X4-3*X10-X9-2*X8-X7-2*X6)+2*PQ**2
     & *PH**2*(-6*X3-3*X4-3*X10+X7+2*X6)+4*PQ**2*(-8*X1*X3-4*X1*
     & X4-4*X1*X10-8*X2*X3-4*X2*X4-4*X2*X10-4*X3*X9-4*X3*X7-12*
     & X3*X5-4*X4*X9+8*X4*X8-4*X4*X7+8*X4*X6-6*X4*X5-6*X10*X5-X9
     & *X5-2*X8*X5)+4*PH**2*(2*X1*X3+X1*X4+X1*X10+X3*X7+X4*X7-2*
     & X4*X6)+8*X5*(-2*X2*X3-X2*X4-X2*X10-X3*X9-X4*X9+2*X4*X8)
      FM(7,7)=72*PQ**4*X10+18*PQ**2*PH**2*X10+8*PQ**2*(X1*X10+9*
     & X2*X10+7*X3*X7+2*X3*X6+2*X4*X7+7*X4*X6+X10*X5+2*X9*X7+7*
     & X9*X6+7*X8*X7+2*X8*X6)+2*PH**2*(-X1*X10-7*X3*X7-2*X3*X6-2
     & *X4*X7-7*X4*X6)+4*X2*(X10*X5+2*X9*X7+7*X9*X6+7*X8*X7+2*X8
     & *X6)
      FM(7,8)=72*PQ**4*X10+2*PQ**2*PH**2*X10+4*PQ**2*(2*X1*X10+
     & 10*X2*X10+7*X3*X9+2*X3*X8+14*X3*X7+4*X3*X6+2*X4*X9+7*X4*
     & X8+4*X4*X7+14*X4*X6+10*X10*X5+X9**2+7*X9*X8+2*X9*X7+7*X9*
     & X6+X8**2+7*X8*X7+2*X8*X6)+2*PH**2*(7*X1*X10-7*X3*X7-2*X3*
     & X6-2*X4*X7-7*X4*X6)+2*(-2*X1*X9**2-14*X1*X9*X8-2*X1*X8**2
     & +2*X2*X10*X5+2*X2*X9*X7+7*X2*X9*X6+7*X2*X8*X7+2*X2*X8*X6+
     & 7*X3*X9*X5+2*X3*X8*X5+2*X4*X9*X5+7*X4*X8*X5)
      FM(8,8)=72*PQ**4*X10+18*PQ**2*PH**2*X10+8*PQ**2*(X1*X10+X2
     & *X10+7*X3*X9+2*X3*X8+7*X3*X7+2*X3*X6+2*X4*X9+7*X4*X8+2*X4
     & *X7+7*X4*X6+9*X10*X5)+2*PH**2*(-X1*X10-7*X3*X7-2*X3*X6-2*
     & X4*X7-7*X4*X6)+4*X5*(X2*X10+7*X3*X9+2*X3*X8+2*X4*X9+7*X4*
     & X8)
      FM(9,9)=-4*PQ**4*X10-PQ**2*PH**2*X10+4*PQ**2*(-X1*X10-X2*X10+
     & X3*X7+X4*X6-X10*X5+X9*X6+X8*X7)+PH**2*(X1*X10-X3*X7-X4*X6
     & )+2*X2*(-X10*X5+X9*X6+X8*X7)
      FM(9,10)=-4*PQ**4*X10-PQ**2*PH**2*X10+2*PQ**2*(-2*X1*X10-2*X2*
     & X10+2*X3*X9+2*X3*X7+2*X4*X6-2*X10*X5+X9*X8+2*X8*X7)+PH**2
     & *(X1*X10-X3*X7-X4*X6)+2*(-X1*X9*X8-X2*X10*X5+X2*X8*X7+X3*
     & X9*X5)
      FMXX=-4*PQ**4*X10-PQ**2*PH**2*X10+2*PQ**2*(-2*X1*X10-2*X2*
     & X10+2*X4*X8+2*X4*X6+2*X3*X7-2*X10*X5+X9*X8+2*X9*X6)+PH**2
     & *(X1*X10-X3*X7-X4*X6)+2*(-X1*X9*X8-X2*X10*X5+X2*X9*X6+X4*
     & X8*X5)
      FM(9,10)=0.5*(FMXX+FM(9,10))
      FM(10,10)=-4*PQ**4*X10-PQ**2*PH**2*X10+4*PQ**2*(-X1*X10-X2*X10+
     & X3*X7+X4*X6-X10*X5+X9*X3+X8*X4)+PH**2*(X1*X10-X3*X7-X4*X6
     & )+2*X5*(-X10*X2+X9*X3+X8*X4)
 
C...Repackage matrix elements.
        DO 200 I=1,8
        DO 190 J=1,8
        RM(I,J)=FM(I,J)
  190   CONTINUE
  200   CONTINUE
        RM(7,7)=FM(7,7)-2.*FM(9,9)
        RM(7,8)=FM(7,8)-2.*FM(9,10)
        RM(8,8)=FM(8,8)-2.*FM(10,10)
 
C...Produce final result: matrix elements * colours * propagators.
        DO 220 I=1,8
        DO 210 J=I,8
        FAC=8.
        IF(I.EQ.J)FAC=4.
        WTQQBH=WTQQBH+RM(I,J)*FAC*CLR(I,J)/(DX(I)*DX(J))
  210   CONTINUE
  220   CONTINUE
        WTQQBH=-WTQQBH/256.
 
      ELSE
C...Evaluate matrix elements for q + q~ -> Q + Q~ + H.
        A11=-8.*PQ**4*X10-2.*PQ**2*PH**2*X10-(8.*PQ**2)*(X2*X10+X3
     &   *X7+X4*X6+X9*X6+X8*X7)+2.*PH**2*(X3*X7+X4*X6)-(4.*X2)*(X9
     &   *X6+X8*X7)
        A12=-8.*PQ**4*X10+4.*PQ**2*(-X2*X10-X3*X9-2.*X3*X7-X4*X8-
     &   2.*X4*X6-X10*X5-X9*X8-X9*X6-X8*X7)+2.*PH**2*(-X1*X10+X3*X7
     &   +X4*X6)+2.*(2.*X1*X9*X8-X2*X9*X6-X2*X8*X7-X3*X9*X5-X4*X8*
     &   X5)
        A22=-8.*PQ**4*X10-2.*PQ**2*PH**2*X10-(8.*PQ**2)*(X3*X9+X3*
     &   X7+X4*X8+X4*X6+X10*X5)+2.*PH**2*(X3*X7+X4*X6)-(4.*X5)*(X3
     &   *X9+X4*X8)
 
C...Produce final result: matrix elements * propagators.
        A11=A11/DX(7)**2
        A12=A12/(DX(7)*DX(8))
        A22=A22/DX(8)**2
        WTQQBH=-(A11+A22+2.*A12)/8.
      ENDIF
 
      RETURN
      END
 
C***********************************************************************
 
       SUBROUTINE PYTEST(MTEST)
 
C...Purpose: to provide a simple program (disguised as a subroutine) to
C...run at installation as a check that the program works as intended.
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
      COMMON/GUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      INTEGER  MDCY,MDME,KFDP
      REAL  BRAT
      SAVE /GUDAT3/
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      INTEGER  MSEL,MSUB,KFIN
      REAL  CKIN
      SAVE /PYSUBS/
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      INTEGER  MSTP,MSTI
      REAL  PARP,PARI
      SAVE /PYPARS/
C...Common initial values. Loop over initiating conditions.
      MSTP(122)=MAX(0,MIN(2,MTEST))
      MDCY(LUCOMP(111),1)=0
      NERR=0
      DO 130 IPROC=1,8
 
C...Reset process type, kinematics cuts, and the flags used.
      MSEL=0
      DO 100 ISUB=1,200
      MSUB(ISUB)=0
  100 CONTINUE
      CKIN(1)=2.
      CKIN(3)=0.
      MSTP(2)=1
      MSTP(11)=0
      MSTP(33)=0
      MSTP(81)=1
      MSTP(82)=1
      MSTP(111)=1
      MSTP(131)=0
      MSTP(133)=0
      PARP(131)=0.01
 
C...Prompt photon production at fixed target.
      IF(IPROC.EQ.1) THEN
        PZSUM=300.
        PESUM=SQRT(PZSUM**2+ULMASS(211)**2)+ULMASS(2212)
        PQSUM=2.
        MSEL=10
        CKIN(3)=5.
        CALL PYINIT('FIXT','pi+','p',PZSUM)
 
C...QCD processes at ISR energies.
      ELSEIF(IPROC.EQ.2) THEN
        PESUM=63.
        PZSUM=0.
        PQSUM=2.
        MSEL=1
        CKIN(3)=5.
        CALL PYINIT('CMS','p','p',PESUM)
 
C...W production + multiple interactions at CERN Collider.
      ELSEIF(IPROC.EQ.3) THEN
        PESUM=630.
        PZSUM=0.
        PQSUM=0.
        MSEL=12
        CKIN(1)=20.
        MSTP(82)=4
        MSTP(2)=2
        MSTP(33)=3
        CALL PYINIT('CMS','p','pbar',PESUM)
 
C...W/Z gauge boson pairs + pileup events at the Tevatron.
      ELSEIF(IPROC.EQ.4) THEN
        PESUM=1800.
        PZSUM=0.
        PQSUM=0.
        MSUB(22)=1
        MSUB(23)=1
        MSUB(25)=1
        CKIN(1)=200.
        MSTP(111)=0
        MSTP(131)=1
        MSTP(133)=2
        PARP(131)=0.04
        CALL PYINIT('CMS','p','pbar',PESUM)
 
C...Higgs production at LHC.
      ELSEIF(IPROC.EQ.5) THEN
        PESUM=15400.
        PZSUM=0.
        PQSUM=2.
        MSUB(3)=1
        MSUB(102)=1
        MSUB(123)=1
        MSUB(124)=1
        PMAS(25,1)=300.
        CKIN(1)=200.
        MSTP(81)=0
        MSTP(111)=0
        CALL PYINIT('CMS','p','p',PESUM)
 
C...Z' production at SSC.
      ELSEIF(IPROC.EQ.6) THEN
        PESUM=40000.
        PZSUM=0.
        PQSUM=2.
        MSEL=21
        PMAS(32,1)=600.
        CKIN(1)=400.
        MSTP(81)=0
        MSTP(111)=0
        CALL PYINIT('CMS','p','p',PESUM)
 
C...W pair production at 1 TeV e+e- collider.
      ELSEIF(IPROC.EQ.7) THEN
        PESUM=1000.
        PZSUM=0.
        PQSUM=0.
        MSUB(25)=1
        MSUB(69)=1
        MSTP(11)=1
        CALL PYINIT('CMS','e+','e-',PESUM)
 
C...Deep inelastic scattering at a LEP+LHC ep collider.
      ELSEIF(IPROC.EQ.8) THEN
        P(1,1)=0.
        P(1,2)=0.
        P(1,3)=8000.
        P(2,1)=0.
        P(2,2)=0.
        P(2,3)=-80.
        PESUM=8080.
        PZSUM=7920.
        PQSUM=0.
        MSUB(10)=1
        CKIN(3)=50.
        MSTP(111)=0
        CALL PYINIT('USER','p','e-',PESUM)
      ENDIF
 
C...Generate 20 events of each required type.
      DO 120 IEV=1,20
      CALL PYEVNT
      PESUMM=PESUM
      IF(IPROC.EQ.4) PESUMM=MSTI(41)*PESUM
 
C...Check conservation of energy/momentum/flavour.
      MERR=0
      DEVE=ABS(PLU(0,4)-PESUMM)+ABS(PLU(0,3)-PZSUM)
      DEVT=ABS(PLU(0,1))+ABS(PLU(0,2))
      DEVQ=ABS(PLU(0,6)-PQSUM)
      IF(DEVE.GT.2E-3*PESUM.OR.DEVT.GT.MAX(0.01,1E-4*PESUM).OR.
     &DEVQ.GT.0.1) MERR=1
      IF(MERR.NE.0) WRITE(MSTU(11),5000) IPROC,IEV
 
C...Check that all KF codes are known ones, and that partons/particles
C...satisfy energy-momentum-mass relation.
      DO 110 I=1,N
      IF(K(I,1).GT.20) GOTO 110
      IF(LUCOMP(K(I,2)).EQ.0) THEN
        WRITE(MSTU(11),5100) I
        MERR=MERR+1
      ENDIF
      PD=P(I,4)**2-P(I,1)**2-P(I,2)**2-P(I,3)**2-P(I,5)**2*
     &SIGN(1.,P(I,5))
      IF(ABS(PD).GT.MAX(0.1,0.002*P(I,4)**2,0.002*P(I,5)**2).OR.
     &(P(I,5).GE.0..AND.P(I,4).LT.0.)) THEN
        WRITE(MSTU(11),5200) I
        MERR=MERR+1
      ENDIF
  110 CONTINUE
 
C...Listing of erroneous events, and first event of each type.
      IF(MERR.GE.1) NERR=NERR+1
      IF(NERR.GE.10) THEN
        WRITE(MSTU(11),5300)
        CALL LULIST(1)
        STOP
      ENDIF
      IF(MTEST.GE.1.AND.(MERR.GE.1.OR.IEV.EQ.1)) THEN
        IF(MERR.GE.1) WRITE(MSTU(11),5400)
        CALL LULIST(1)
      ENDIF
  120 CONTINUE
 
C...List statistics for each process type.
      IF(MTEST.GE.1) CALL PYSTAT(1)
  130 CONTINUE
 
C...Summarize result of run.
      IF(NERR.EQ.0) WRITE(MSTU(11),5500)
      IF(NERR.GT.0) WRITE(MSTU(11),5600) NERR
      RETURN
 
C...Formats for information.
 5000 FORMAT(/5X,'Energy/momentum/flavour nonconservation for process',
     &I2,', event',I4)
 5100 FORMAT(/5X,'Entry no.',I4,' in following event not known code')
 5200 FORMAT(/5X,'Entry no.',I4,' in following event has faulty ',
     &'kinematics')
 5300 FORMAT(/5X,'This is the tenth error experienced! Something is ',
     &'wrong.'/5X,'Execution will be stopped after listing of event.')
 5400 FORMAT(5X,'Faulty event follows:')
 5500 FORMAT(//5X,'End result of PYTEST: no errors detected.')
 5600 FORMAT(//5X,'End result of PYTEST:',I2,' errors detected.'/
     &5X,'This should not have happened!')
      END
 
C*********************************************************************
 
      BLOCK DATA PYDATA
 
C...Give sensible default values to all status codes and parameters.
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
C...Default values for allowed processes and kinematics constraints.
      DATA MSEL/1/
      DATA MSUB/200*0/
      DATA ((KFIN(I,J),J=-40,40),I=1,2)/16*0,4*1,4*0,6*1,5*0,5*1,0,
     &5*1,5*0,6*1,4*0,4*1,16*0,16*0,4*1,4*0,6*1,5*0,5*1,0,5*1,5*0,
     &6*1,4*0,4*1,16*0/
      DATA CKIN/
     &   2.0, -1.0,  0.0, -1.0,  1.0,  1.0, -10.,  10., -10.,  10.,
     1  -10.,  10., -10.,  10., -10.,  10., -1.0,  1.0, -1.0,  1.0,
     2   0.0,  1.0,  0.0,  1.0, -1.0,  1.0, -1.0,  1.0,   0.,   0.,
     3   2.0, -1.0,   0.,   0.,  0.0, -1.0,  0.0, -1.0,  4.0, -1.0,
     4  12.0, -1.0, 12.0, -1.0, 12.0, -1.0, 12.0, -1.0,   0.,   0.,
     5   0.0, -1.0,  0.0, -1.0,  0.0, -1.0,   0.,   0.,   0.,   0.,
     6   140*0./
 
C...Default values for main switches and parameters. Reset information.
      DATA (MSTP(I),I=1,100)/
     &     3,    1,    2,    0,    0,    0,    0,    0,    0,    0,
     1     1,    0,    1,    0,    5,    0,    0,    0,    0,    0,
     2     1,    0,    1,    0,    0,    0,    0,    0,    0,    1,
     3     1,    2,    0,    1,    0,    2,    1,    5,    2,    0,
     4     1,    1,    3,    7,    3,    1,    1,    2,    2,    0,
     5     9,    1,    1,    1,    5,    1,    1,    6,    1,    0,
     6     1,    3,    2,    2,    1,    1,    2,    0,    0,    0,
     7     1,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     8     1,    1,  100,    0,    0,    0,    0,    0,    0,    0,
     9     1,    4,    1,    2,    0,    0,    0,    0,    0,    0/
      DATA (MSTP(I),I=101,200)/
     &     3,    1,    0,    0,    0,    0,    0,    0,    0,    0,
     1     1,    1,    1,    0,    0,    0,    0,    0,    0,    0,
     2     0,    1,    2,    1,    1,   20,    0,    0,   10,    0,
     3     0,    4,    0,    1,    0,    0,    0,    0,    0,    0,
     4     0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     5     0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     6     0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     7     0,    2,    0,    0,    0,    0,    0,    0,    0,    0,
     8     5,  722, 1996,   05,   23,  408,    0,    0,    0,    0,
     9     0,    0,    0,    0,    0,    0,    0,    0,    0,    0/
      DATA (PARP(I),I=1,100)/
     &  0.25,  10.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     1    0.,   0.,  1.0, 0.01,  0.6,  1.0,  1.0,   0.,   0.,   0.,
     2    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     3   1.5,  2.0, 0.075, 1.0,  0.2,   0.,  2.0, 0.70, 0.006,  0.,
     4  0.02,  2.0, 0.10, 1000., 2054., 123., 246., 0.,   0.,   0.,
     5   1.0,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     6  0.25,  1.0, 0.25,  1.0,  2.0, 1E-3,  4.0, 1E-3,   0.,   0.,
     7   4.0, 0.25,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     8  1.40, 1.55,  0.5,  0.2, 0.33, 0.66,  0.7,  0.5,   0.,   0.,
     9  0.44, 0.20,  2.0,  1.0,   0.,  3.0,  1.0, 0.75, 0.44,  2.0/
      DATA (PARP(I),I=101,200)/
     &   0.5, 0.28,  1.0,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     1   2.0,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     2   1.0,  0.4,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     3  0.01,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     4    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     5    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     6  2.20, 23.6, 18.4, 11.5,   0.,   0.,   0.,   0.,   0.,   0.,
     7    0.,   0.,   0.,  1.0,   0.,   0.,   0.,   0.,   0.,   0.,
     8    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,
     9    0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0./
      DATA MSTI/200*0/
      DATA PARI/200*0./
      DATA MINT/400*0/
      DATA VINT/400*0./
 
C...Constants for the generation of the various processes.
      DATA (ISET(I),I=1,100)/
     &    1,    1,    1,   -1,    3,   -1,   -1,    3,   -2,    2,
     1    2,    2,    2,    2,    2,    2,   -1,    2,    2,    2,
     2   -1,    2,    2,    2,    2,    2,   -1,    2,    2,    2,
     3    2,   -1,    2,    2,    2,    2,   -1,   -1,   -1,   -1,
     4   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
     5   -1,   -1,    2,    2,   -1,   -1,   -1,    2,   -1,   -1,
     6   -1,   -1,   -1,   -1,   -1,   -1,   -1,    2,    2,    2,
     7    4,    4,    4,   -1,   -1,    4,    4,   -1,   -1,    2,
     8    2,    2,    2,    2,    2,    2,    2,    2,    2,   -2,
     9    0,    0,    0,    0,    0,    9,   -2,   -2,   -2,   -2/
      DATA (ISET(I),I=101,200)/
     &   -1,    1,    1,   -2,   -2,   -2,   -2,   -2,   -2,    2,
     1    2,    2,    2,    2,    2,   -1,   -1,   -1,   -2,   -2,
     2    5,    5,    5,    5,   -2,   -2,   -2,   -2,   -2,   -2,
     3    6,   -2,   -2,   -2,   -2,   -2,   -2,   -2,   -2,   -2,
     4    1,    1,    1,    1,    1,   -2,    1,    1,    1,   -2,
     5    1,    1,    1,   -2,   -2,    1,    1,    1,   -2,   -2,
     6    2,    2,    2,    2,    2,    2,    2,    2,   -2,   -2,
     7    2,    2,    5,    5,   -2,    2,    2,    5,    5,   -2,
     8    5,    5,   -2,   -2,   -2,    5,    5,   -2,   -2,   -2,
     9   -2,   -2,   -2,   -2,   -2,   -2,   -2,   -2,   -2,   -2/
      DATA ((KFPR(I,J),J=1,2),I=1,50)/
     &   23,    0,   24,    0,   25,    0,   24,    0,   25,    0,
     &   24,    0,   23,    0,   25,    0,    0,    0,    0,    0,
     1    0,    0,    0,    0,   21,   21,   21,   22,   21,   23,
     1   21,   24,   21,   25,   22,   22,   22,   23,   22,   24,
     2   22,   25,   23,   23,   23,   24,   23,   25,   24,   24,
     2   24,   25,   25,   25,    0,   21,    0,   22,    0,   23,
     3    0,   24,    0,   25,    0,   21,    0,   22,    0,   23,
     3    0,   24,    0,   25,    0,   21,    0,   22,    0,   23,
     4    0,   24,    0,   25,    0,   21,    0,   22,    0,   23,
     4    0,   24,    0,   25,    0,   21,    0,   22,    0,   23/
      DATA ((KFPR(I,J),J=1,2),I=51,100)/
     5    0,   24,    0,   25,    0,    0,    0,    0,    0,    0,
     5    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     6    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     6    0,    0,    0,    0,   21,   21,   24,   24,   23,   24,
     7   23,   23,   24,   24,   23,   24,   23,   25,   22,   22,
     7   23,   23,   24,   24,   24,   25,   25,   25,    0,  211,
     8    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     8  443,   21,10441,   21,20443,   21,  445,   21,    0,    0,
     9    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     9    0,    0,    0,    0,    0,    0,    0,    0,    0,    0/
      DATA ((KFPR(I,J),J=1,2),I=101,150)/
     &   23,    0,   25,    0,   25,    0,    0,    0,    0,    0,
     &    0,    0,    0,    0,    0,    0,    0,    0,   22,   25,
     1   21,   25,    0,   25,   21,   25,   22,   22,   21,   22,
     1   22,   23,   23,   23,   24,   24,    0,    0,    0,    0,
     2   25,    6,   25,    6,   25,    0,   25,    0,    0,    0,
     2    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     3   23,    5,    0,    0,    0,    0,    0,    0,    0,    0,
     3    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     4   32,    0,   34,    0,   37,    0,   40,    0,   39,    0,
     4    0,    0,    7,    0,    8,    0,   38,    0,    0,    0/
      DATA ((KFPR(I,J),J=1,2),I=151,200)/
     5   35,    0,   35,    0,   35,    0,    0,    0,    0,    0,
     5   36,    0,   36,    0,   36,    0,    0,    0,    0,    0,
     6    6,   37,   39,    0,   39,   39,   39,   39,   11,    0,
     6   11,    0,    0,    7,    0,    8,    0,    0,    0,    0,
     7   23,   35,   24,   35,   35,    0,   35,    0,    0,    0,
     7   23,   36,   24,   36,   36,    0,   36,    0,    0,    0,
     8   35,    6,   35,    6,    0,    0,    0,    0,    0,    0,
     8   36,    6,   36,    6,    0,    0,    0,    0,    0,    0,
     9    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
     9    0,    0,    0,    0,    0,    0,    0,    0,    0,    0/
      DATA COEF/4000*0./
      DATA (((ICOL(I,J,K),K=1,2),J=1,4),I=1,40)/
     1 4,0,3,0,2,0,1,0,3,0,4,0,1,0,2,0,2,0,0,1,4,0,0,3,3,0,0,4,1,0,0,2,
     2 3,0,0,4,1,4,3,2,4,0,0,3,4,2,1,3,2,0,4,1,4,0,2,3,4,0,3,4,2,0,1,2,
     3 3,2,1,0,1,4,3,0,4,3,3,0,2,1,1,0,3,2,1,4,1,0,0,2,2,4,3,1,2,0,0,1,
     4 3,2,1,4,1,4,3,2,4,2,1,3,4,2,1,3,3,4,4,3,1,2,2,1,2,0,3,1,2,0,0,0,
     5 4,2,1,0,0,0,1,0,3,0,0,3,1,2,0,0,4,0,0,4,0,0,1,2,2,0,0,1,4,4,3,3,
     6 2,2,1,1,4,4,3,3,3,3,4,4,1,1,2,2,3,2,1,3,1,2,0,0,4,2,1,4,0,0,1,2,
     7 4,0,0,0,4,0,1,3,0,0,3,0,2,4,3,0,3,4,0,0,1,0,0,1,0,0,3,4,2,0,0,2,
     8 3,0,0,0,1,0,0,0,0,0,3,0,2,0,0,0,2,0,3,1,2,0,0,0,3,2,1,0,1,0,0,0,
     9 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     & 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
 
C...Character constants: name of processes.
      DATA PROC(0)/                    'All included subprocesses   '/
      DATA (PROC(I),I=1,20)/
     1'f + f~ -> gamma*/Z0         ',  'f + f~'' -> W+/-             ',
     2'f + f~ -> H0                ',  'gamma + W+/- -> W+/-        ',
     3'Z0 + Z0 -> H0               ',  'Z0 + W+/- -> W+/-           ',
     4'                            ',  'W+ + W- -> H0               ',
     5'                            ',  'f + f'' -> f + f'' (QFD)      ',
     6'f + f'' -> f + f'' (QCD)      ','f + f~ -> f'' + f~''          ',
     7'f + f~ -> g + g             ',  'f + f~ -> g + gamma         ',
     8'f + f~ -> g + Z0            ',  'f + f~'' -> g + W+/-         ',
     9'f + f~ -> g + H0            ',  'f + f~ -> gamma + gamma     ',
     &'f + f~ -> gamma + Z0        ',  'f + f~'' -> gamma + W+/-     '/
      DATA (PROC(I),I=21,40)/
     1'f + f~ -> gamma + H0        ',  'f + f~ -> Z0 + Z0           ',
     2'f + f~'' -> Z0 + W+/-        ', 'f + f~ -> Z0 + H0           ',
     3'f + f~ -> W+ + W-           ',  'f + f~'' -> W+/- + H0        ',
     4'f + f~ -> H0 + H0           ',  'f + g -> f + g              ',
     5'f + g -> f + gamma          ',  'f + g -> f + Z0             ',
     6'f + g -> f'' + W+/-          ', 'f + g -> f + H0             ',
     7'f + gamma -> f + g          ',  'f + gamma -> f + gamma      ',
     8'f + gamma -> f + Z0         ',  'f + gamma -> f'' + W+/-      ',
     9'f + gamma -> f + H0         ',  'f + Z0 -> f + g             ',
     &'f + Z0 -> f + gamma         ',  'f + Z0 -> f + Z0            '/
      DATA (PROC(I),I=41,60)/
     1'f + Z0 -> f'' + W+/-         ', 'f + Z0 -> f + H0            ',
     2'f + W+/- -> f'' + g          ', 'f + W+/- -> f'' + gamma      ',
     3'f + W+/- -> f'' + Z0         ', 'f + W+/- -> f'' + W+/-       ',
     4'f + W+/- -> f'' + H0         ', 'f + H0 -> f + g             ',
     5'f + H0 -> f + gamma         ',  'f + H0 -> f + Z0            ',
     6'f + H0 -> f'' + W+/-         ', 'f + H0 -> f + H0            ',
     7'g + g -> f + f~             ',  'g + gamma -> f + f~         ',
     8'g + Z0 -> f + f~            ',  'g + W+/- -> f + f~''         ',
     9'g + H0 -> f + f~            ',  'gamma + gamma -> f + f~     ',
     &'gamma + Z0 -> f + f~        ',  'gamma + W+/- -> f + f~''     '/
      DATA (PROC(I),I=61,80)/
     1'gamma + H0 -> f + f~        ',  'Z0 + Z0 -> f + f~           ',
     2'Z0 + W+/- -> f + f~''        ', 'Z0 + H0 -> f + f~           ',
     3'W+ + W- -> f + f~           ',  'W+/- + H0 -> f + f~''        ',
     4'H0 + H0 -> f + f~           ',  'g + g -> g + g              ',
     5'gamma + gamma -> W+ + W-    ',  'gamma + W+/- -> Z0 + W+/-   ',
     6'Z0 + Z0 -> Z0 + Z0          ',  'Z0 + Z0 -> W+ + W-          ',
     7'Z0 + W+/- -> Z0 + W+/-      ',  'Z0 + Z0 -> Z0 + H0          ',
     8'W+ + W- -> gamma + gamma    ',  'W+ + W- -> Z0 + Z0          ',
     9'W+/- + W+/- -> W+/- + W+/-  ',  'W+/- + H0 -> W+/- + H0      ',
     &'H0 + H0 -> H0 + H0          ',  'q + gamma -> q'' + pi+/-     '/
      DATA (PROC(I),I=81,100)/
     1'q + q~ -> Q + Q~, massive   ',  'g + g -> Q + Q~, massive    ',
     2'f + q -> f'' + Q, massive    ', 'g + gamma -> Q + Q~, massive',
     3'gamma + gamma -> F + F~, mas',  'g + g -> J/Psi + g          ',
     4'g + g -> chi_0c + g         ',  'g + g -> chi_1c + g         ',
     5'g + g -> chi_2c + g         ',  '                            ',
     6'Elastic scattering          ',  'Single diffractive (XB)     ',
     7'Single diffractive (AX)     ',  'Double  diffractive         ',
     8'Low-pT scattering           ',  'Semihard QCD 2 -> 2         ',
     9'                            ',  '                            ',
     &'                            ',  '                            '/
      DATA (PROC(I),I=101,120)/
     1'g + g -> gamma*/Z0          ',  'g + g -> H0                 ',
     2'gamma + gamma -> H0         ',  '                            ',
     3'                            ',  '                            ',
     4'                            ',  '                            ',
     5'                            ',  'f + f~ -> gamma + H0        ',
     6'f + f~ -> g + H0            ',  'q + g -> q + H0             ',
     7'g + g -> g + H0             ',  'g + g -> gamma + gamma      ',
     8'g + g -> g + gamma          ',  'g + g -> gamma + Z0         ',
     9'g + g -> Z0 + Z0            ',  'g + g -> W+ + W-            ',
     &'                            ',  '                            '/
      DATA (PROC(I),I=121,140)/
     1'g + g -> Q + Q~ + H0        ',  'q + q~ -> Q + Q~ + H0       ',
     2'f + f'' -> f + f'' + H0       ',
     2'f + f'' -> f" + f"'' + H0     ',
     3'                            ',  '                            ',
     4'                            ',  '                            ',
     5'                            ',  '                            ',
     6'g + g -> Z0 + q + q~        ',  '                            ',
     7'                            ',  '                            ',
     8'                            ',  '                            ',
     9'                            ',  '                            ',
     &'                            ',  '                            '/
      DATA (PROC(I),I=141,160)/
     1'f + f~ -> gamma*/Z0/Z''0     ', 'f + f~'' -> W''+/-            ',
     2'f + f~'' -> H+/-             ', 'f + f~'' -> R                ',
     3'q + l -> LQ                 ',  '                            ',
     4'd + g -> d*                 ',  'u + g -> u*                 ',
     5'g + g -> eta_techni         ',  '                            ',
     6'f + f~ -> H''0               ', 'g + g -> H''0                ',
     7'gamma + gamma -> H''0        ', '                            ',
     8'                            ',  'f + f~ -> A0                ',
     9'g + g -> A0                 ',  'gamma + gamma -> A0         ',
     &'                            ',  '                            '/
      DATA (PROC(I),I=161,180)/
     1'f + g -> f'' + H+/-          ', 'q + g -> LQ + l~            ',
     2'g + g -> LQ + LQ~           ',  'q + q~ -> LQ + LQ~          ',
     3'f + f~ -> f'' + f~'' (gamma/Z)',
     3'f +f~'' -> f" + f~"'' (W)     ',
     4'q + q'' -> q" + d*           ',  'q + q'' -> q" + u*           ',
     5'                            ',  '                            ',
     6'f + f~ -> Z0 + H''0          ', 'f + f~'' -> W+/- + H''0       ',
     7'f + f'' -> f + f'' + H''0      ',
     7'f + f'' -> f" + f"'' + H''0    ',
     8'                            ',  'f + f~ -> Z0 + A0           ',
     9'f + f~'' -> W+/- + A0        ',
     9'f + f'' -> f + f'' + A0       ',
     &'f + f'' -> f" + f"'' + A0     ',
     &'                            '/
      DATA (PROC(I),I=181,200)/
     1'g + g -> Q + Q~ + H''0       ',  'q + q~ -> Q + Q~ + H''0      ',
     2'                            ',  '                            ',
     3'                            ',  'g + g -> Q + Q~ + A0        ',
     4'q + q~ -> Q + Q~ + A0       ',  '                            ',
     5'                            ',  '                            ',
     6'                            ',  '                            ',
     7'                            ',  '                            ',
     8'                            ',  '                            ',
     9'                            ',  '                            ',
     &'                            ',  '                            '/
 
C...Cross sections and slope offsets.
      DATA SIGT/294*0./
 
      END
 
C*********************************************************************
 
      SUBROUTINE RKBBV(AK1,AK2,AP1,AP2,ALEP1,ALEP2,IMC,RESULT)
 
C...The following routines have been written by Ronald Kleiss,
C...to evaluate the matrix element for g + g -> Z + q + qbar,
C...with massive quarks (e.g. q = b).
C...They have been modified, so that all routines and commonblocks
C...have names beginning with RK, and so that some unnecessary
C...initialization information is not printed. Further, COMPLEX*16
C...has been changed to COMPLEX and REAL*8 to DOUBLE PRECISION
C...(in a few cases to REAL), so as to make the program better
C...transportable.
 
* THE CROSS SECTION FOR
* G(K1) + G(K2) ---> Z(QV) + B(P1) + B_BAR(P2)
*                     |
*                     +---> L(LEP1) + LEP_BAR(LEP2)
* THE B QUARKS HAVE TO BE ON-SHELL, THE LEPTONS MASSLESS
* THE OPTION IMC=0 PERFORMS THE STANDARD SPIN SUM
* THE OPTION IMC=1 PERFORMS THE CALCULATION FOR 'NMC' RANDOMLY
* CHOSEN HELICITY STATES WHICH IMPROVES THE
* SPEED BY A FACTOR 32/NMC
      REAL AK1(0:3),AK2(0:3),AP1(0:3),AP2(0:3),ALEP1(0:3),ALEP2(0:3)
      DOUBLE PRECISION K1(0:4),K2(0:4),P1(0:4),P2(0:4),LEP1(0:4),
     &LEP2(0:4)
      REAL RMQ,RMV,RGV,GSTR,VB,AB,VL,AL
      INTEGER INIT
      INTEGER J1,J2,J3,J4,J5
      INTEGER K,IMC,KLOW,KUPP,NMC,OLDIMC
      DOUBLE PRECISION RKRAND,RKDOT,MULT,RMB
C      INTEGER CHKGL1,CHKGL2
      DOUBLE PRECISION QV(0:4),R1(0:4),R2(0:4),Q1(0:4),Q2(0:4)
      DOUBLE PRECISION PP2(0:4)
      DOUBLE PRECISION CROSS
      INTEGER LG1,LG2,LV,L1,L2,HELIX,HELI
      COMPLEX ZFACV,ZFAC1,ZFAC2
      DOUBLE PRECISION ZFACS,ZFACB,ZFACBB,ZFACL
      COMPLEX RKZSF
      COMPLEX ZFAC
      DOUBLE PRECISION VPA,VMA
      DOUBLE PRECISION RR1(0:4),RR2(0:4)
      DOUBLE PRECISION ZD12V,ZD21V,ZD1V2,ZD2V1,ZDV12,ZDV21
      COMPLEX RKZF,ZN12V,ZN21V,ZN1V2,ZN2V1,ZNV12,ZNV21
      COMPLEX ZDIA1,ZDIA2,ZDIA3,ZDIA4,ZDIA5,ZDIA6,ZDIA7,ZDIA8
      COMPLEX ZC12V,ZC21V,ZCV12,ZCV21
      DOUBLE PRECISION S,ZD11,ZD22
      COMPLEX ZABEL,ZNABEL,ZNABEM
      REAL RESULT
      DOUBLE PRECISION THIS1
      COMPLEX ANSS(-1:1,1:4,-1:1,1:4)
      INTEGER DONS(-1:1,1:4,-1:1,1:4)
      COMPLEX ANSF(-1:1,1:4,1:8,-1:1,1:4)
      INTEGER DONF(-1:1,1:4,1:8,-1:1,1:4)
 
C      PARAMETER(CHKGL1=0,CHKGL2=0)
      PARAMETER(NMC=1)
 
      COMMON/RKZSCO/ANSS,DONS
      COMMON/RKZFCO/ANSF,DONF
      COMMON/RKBBVC/RMQ,RMV,RGV,VB,AB,VL,AL
      DATA INIT/0/
 
* CHECK ON EITHER FIRST CALL OR CHANGE IN IMC
      IF(INIT.EQ.0.OR.IMC.NE.OLDIMC) THEN
        OLDIMC=IMC
        INIT=1
* REPRODUCE INPUT DATA
C       WRITE(6,*) ' ----------------------------------------'
C       WRITE(6,*) ' BBV: G G ---> B B_BAR Z, Z ---> L L_BAR'
C       WRITE(6,*) ' B QUARK MASS      = ',RMB,' GEV'
C       WRITE(6,*) ' BOSON MASS        = ',RMV,' GEV'
C       WRITE(6,*) ' BOSON WIDTH       = ',RGV,' GEV'
C       WRITE(6,*) ' B VECTOR C.       = ',VB
C       WRITE(6,*) ' B AXIAL C.        = ',AB
C       WRITE(6,*) ' LEPTON VECTOR C.  = ',VL
C       WRITE(6,*) ' LEPTON AXIAL C.   = ',AL
        RMB=RMQ
* ADJUST STRONG COUPLING SO AS TO GIVE EFFECTIVELY ALPHA_S=1
        GSTR=4D0*DSQRT(DATAN(1D0))
C       WRITE(6,*) ' QCD COUPLING      = ',GSTR
* SEE WETHER GAUGE CHECKS ARE REQUIRED
C        IF(CHKGL1.EQ.1) THEN
C          WRITE(6,*) ' GAUGE CHECK ON GLUON 1'
C        ENDIF
C        IF(CHKGL2.EQ.1) THEN
C          WRITE(6,*) ' GAUGE CHECK ON GLUON 2'
C        ENDIF
* SEE WETHER HELICITY MONTE CARLO IS REQUIRED
        IF(IMC.EQ.0) THEN
          KLOW=1
          KUPP=32
          MULT=1D0
          WRITE(6,*) ' SUM OVER HELICITIES SELECTED'
        ELSEIF(IMC.EQ.1) THEN
          KLOW=1
          KUPP=NMC
          MULT=32D0/(1D0*NMC)
C         WRITE(6,*) ' MONTE CARLO OVER HELICITES SELECTED'
C         WRITE(6,*) ' WITH ',NMC,' HELICITY TRIALS'
C         WRITE(6,*) ' RESULT THEN MULTIPLIED BY ',MULT
        ELSE
          WRITE(6,*) ' ERROR: WRONG OPTION IMC=',IMC
        ENDIF
C       WRITE(6,*) ' THE RESULT IS BASED ON ALPHA_S=1,',
C    .  ' MUST BE MULTIPLIED BY ALPHA_S**2'
C       WRITE(6,*) ' ----------------------------------------'
C       WRITE(6,800)'NO.','LG1','LG2','LV','L1','L2','AMP**2'
C 800   FORMAT(' ',6A4,A10)
      ENDIF
 
* INITIALIZE THE ARRAYS ANSS,DONS
      DO 130 J1=-1,1,2
        DO 120 J2=1,4
          DO 110 J3=-1,1,2
            DO 100 J4=1,4
              ANSS(J1,J2,J3,J4)=(0.,0.)
              DONS(J1,J2,J3,J4)=0
  100       CONTINUE
  110     CONTINUE
  120   CONTINUE
  130 CONTINUE
 
* INITIALIZE THE ARRAYS ANSF,DONF
      DO 180 J1=-1,1,2
        DO 170 J2=1,4
          DO 160 J3=1,8
            DO 150 J4=-1,1,2
              DO 140 J5=1,4
                 ANSF(J1,J2,J3,J4,J5)=(0.,0.)
                 DONF(J1,J2,J3,J4,J5)=0
  140         CONTINUE
  150       CONTINUE
  160     CONTINUE
  170   CONTINUE
  180 CONTINUE
 
* EQUATE THE (0:4) INTERNAL MOMENTA TO THE (0:3) ARGUMENTS MOMENTA
      DO 190 K=0,3
        K1(K)=AK1(K)
        K2(K)=AK2(K)
        P1(K)=AP1(K)
        P2(K)=AP2(K)
        LEP1(K)=ALEP1(K)
        LEP2(K)=ALEP2(K)
  190 CONTINUE
 
* ASSIGN LABELS TO THE MOMENTA FOR RECOGNITION
* THE MOMENTA K1,K2,LEP1,LEP2 (AND R1,R2) CAN OCCUR AS THE MASSLESS
* MOMENTA IN ARGUMENTS NO.2 AND 6 IN ZF, AND NO.2 AND 4 IN RKZSF
* R1,R2 AND Q1,Q2 ARE SOME OF THESE, AND CAN ALSO OCCUR
* AS ARGUMENTS NO.2 AND 6 IN ZF AND NO.2 AND 4 IN RKZSF
        K1(4)=1D0
        K2(4)=2D0
        LEP1(4)=3D0
        LEP2(4)=4D0
* THE OTHER MOMENTA P1,P2 AND THE VARIOUS RR1,RR2 CAN OCCUR ONLY
* AS ARGUMENT NO.3 IN ZF
        P1(4)=1D0
        P2(4)=2D0
 
* THE TOTAL BOSON MOMENTUM
* NO NEED TO ASSIGN 4TH COMPONENT LABEL SINCE IT IS NOT USED
      DO 200 K=0,3
        QV(K)=LEP1(K)+LEP2(K)
  200 CONTINUE
 
 
* DEFINE THE AUXILIARY VECTORS: THE RESULT SHOULD BE THE SAME
* FOR EVERY NON-SINGULAR CHOICE OF THE AUXILIARY VECTORS
* SINGULAR CHOICES ARE R1=K1 OR R2=K2
* THESE ARE OBTAINED BY PUTTING CHKGL1=1 OR CHKGL2=1
 
* AUXILIARY VECTOR FOR GLUON 1
* NEED TO ASSIGN ALSO 4TH COMPONENT LABELS HERE!
C      IF(CHKGL1.EQ.1) THEN
C        DO 210 K=0,4
C          R1(K)=K1(K)
C  210   CONTINUE
C      ELSE
        DO 210 K=0,4
          R1(K)=K2(K)
  210   CONTINUE
C      ENDIF
 
* AUXILIARY VECTOR FOR GLUON 2
C      IF(CHKGL2.EQ.1) THEN
C        DO 230 K=0,4
C          R2(K)=K2(K)
C  230   CONTINUE
C      ELSE
        DO 220 K=0,4
          R2(K)=K1(K)
  220   CONTINUE
C      ENDIF
 
* AUXILIARY VECTOR FOR THE B QUARK
      DO 230 K=0,4
        Q1(K)=LEP1(K)
  230 CONTINUE
 
* AUXILIARY VECTOR FOR THE B_BAR QUARK
      DO 240 K=0,4
        Q2(K)=LEP2(K)
  240 CONTINUE
 
* INITIALIZE THE CROSS SECTION TO ZERO
      CROSS=0D0
 
* SINCE P2 CORRESPONDS TO AN ANTIFERMION WE HAVE TO
* CHANGE ITS SIGN MOMENTARILY: PUT THE OLD RESULT IN PP2(0:3)
* BU MAKE SURE TO KEEP THE LABEL POSITIVE!
      DO 250 K=0,3
        PP2(K)=P2(K)
        P2(K)=-P2(K)
  250 CONTINUE
 
* COMPUTE OVERALL FACTORS: FOR EVERY SLASHED POLARIZATION THERE
* APPEARS A FACTOR OF 2 IN ADDITION TO THE NORMALIZATION
* FOLLOWING FROM THE CHISHOLM IDENTITY
* IN PRINCIPLE THE OVERALL FACTORS ARE DIFFERENT FOR EACH DIFFERENT
* HELICITY COMPBINATION BUT IN THIS CASE WE ARE ONLY INTERESTED IN
* THEIR ABSOLUTE VALUE (NO TRANSVERSE GLUON POLARIZATION ETC.)
* SO WE CAN TAKE THIS OUT OF THE LOOP, EXCEPT FOR THE NONTRIVIAL
* HELICITY DEPENDENCE IN 'ZFACV'
 
* OVERALL FACTOR FOR THE BOSON CURRENT, WITH BREIT-WIGNER
      ZFACV=2./CMPLX(SNGL(RKDOT(QV,QV))-RMV**2,RMV*RGV)
 
* OVERALL FACTOR FOR GLUON 1
C      IF(CHKGL1.EQ.1) THEN
C        ZFAC1=(1.,0.)
C      ELSE
* ORIGINAL FORM: ZFAC1=2D0*LG1/(DSQRT(2D0)*RKZPR(-LG1,K1,R1))
        ZFAC1=DSQRT(2D0)/RKZSF(1,K1,-1,R1)
C      ENDIF
 
* OVERALL FACTOR FOR GLUON 2
C      IF(CHKGL2.EQ.1) THEN
C        ZFAC2=1D0
C      ELSE
* ORIGINAL FORM: ZFAC2=2D0*LG2/(DSQRT(2D0)*RKZPR(-LG2,K2,R2))
        ZFAC2=DSQRT(2D0)/RKZSF(1,K2,-1,R2)
C      ENDIF
 
* OVERALL FACTOR FOR QCD COUPLINGS
      ZFACS=GSTR**2
 
* OVERALL FACTOR FOR THE B QUARK
      ZFACB=1/DSQRT(2D0*RKDOT(P1,Q1))
 
* OVERALL FACTOR FOR THE B_BAR QUARK
      ZFACBB=1D0/DSQRT(2D0*RKDOT(PP2,Q2))
 
* FINAL OVERALL FACTOR
      ZFAC=ZFACV*ZFAC1*ZFAC2*ZFACS*ZFACB*ZFACBB
 
* DO A BIG LOOP OVER ALL HELICITIES OR A RANDOM CHOICE OF HELICITIES
* NB: FUNNY INDENTATION HERE!
* ALSO INITIALIZE COUNTERS FOR RKZSF AND ZF
 
      DO 340 HELIX=KLOW,KUPP
      IF(IMC.EQ.0) THEN
        CALL RKHLPK(HELIX,LG1,LG2,LV,L1,L2)
      ELSE
        HELI=IDINT(32D0*RKRAND(HELIX))+1
        CALL RKHLPK(HELI,LG1,LG2,LV,L1,L2)
      ENDIF
 
* DETERMINE THE 'LEFT-' AND 'RIGHT-'HANDED COUPLINGS OF THE B TO THE Z
      VPA=VB+LV*AB
      VMA=VB-LV*AB
* AND THE LEPTON HELICITY FACTOR
      ZFACL=(VL-LV*AL)
 
* FIRST PART OF THE RESULT: THE ABELIAN TERMS
* COMPUTE THE NUMERATORS (ZN...) USING THE ZF FUNCTION
* AND THE DENOMINATORS (ZD...) THE STANDARD WAY
* THE INTERNAL FERMION MOMENTA ARE DIFFERENT IN EACH DIAGRAM
* AND ARE DENOTED BY RR1 AND RR2
* THE 4TH COMPONENT LABELS ARE NONTRIVIAL HERE: HAVING ALREADY
* P1(4)=1 AND P2(4)=2 WE ALSO DEFINE
* (P1-K1)(4)=3,
* (P1-K1-K2)(4)=(P1-K2-K1)(4)=4
* (P1-K2)(4)=5
* (P1-K1+QV)(4)=6
* (P1-K2+QV)(5)=7
* (P1+QV)(4)=8
* SO THAT IN THE VARIOUS DIAGRAMS WE HAVE
* IN ZN12V: RR1(4)=3, RR2(4)=4
* IN ZN21V: RR1(4)=5, RR2(4)=4
* IN ZN1V2: RR1(4)=3, RR2(4)=6
* IN ZN2V1: RR1(4)=5, RR2(4)=7
* IN ZNV12: RR1(4)=8, RR2(4)=6
* IN ZNV21: RR1(4)=8, RR2(4)=7
 
      DO 260 K=0,3
        RR1(K)=P1(K)-K1(K)
        RR2(K)=RR1(K)-K2(K)
  260 CONTINUE
      RR1(4)=3D0
      RR2(4)=4D0
      ZD12V=(RKDOT(RR1,RR1)-RMB**2)*(RKDOT(RR2,RR2)-RMB**2)
      ZN12V =
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
 
      DO 270 K=0,3
        RR1(K)=P1(K)-K2(K)
        RR2(K)=RR1(K)-K1(K)
  270 CONTINUE
      RR1(4)=5D0
      RR2(4)=4D0
      ZD21V=(RKDOT(RR1,RR1)-RMB**2)*(RKDOT(RR2,RR2)-RMB**2)
      ZN21V =
     .   RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
 
      DO 280 K=0,3
        RR1(K)=P1(K)-K1(K)
        RR2(K)=RR1(K)+QV(K)
  280 CONTINUE
      RR1(4)=3D0
      RR2(4)=6D0
      ZD1V2=(RKDOT(RR1,RR1)-RMB**2)*(RKDOT(RR2,RR2)-RMB**2)
      ZN1V2 =
     .   RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,LG2,R2)  *RKZF(LG2,K2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,-LG2,K2) *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,LG2,R2) *RKZF(LG2,K2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1)     *RKZF(LG1,K1,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,-LG2,K2)*RKZF(-LG2,R2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,LG2,R2)  *RKZF(LG2,K2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,-LG2,K2) *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,LG2,R2) *RKZF(LG2,K2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)    *RKZF(-LG1,R1,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,-LG2,K2)*RKZF(-LG2,R2,P2,RMB,L2,Q2)*VMA
 
      DO 290 K=0,3
        RR1(K)=P1(K)-K2(K)
        RR2(K)=RR1(K)+QV(K)
  290 CONTINUE
      RR1(4)=5D0
      RR2(4)=7D0
      ZD2V1=(RKDOT(RR1,RR1)-RMB**2)*(RKDOT(RR2,RR2)-RMB**2)
      ZN2V1 =
     .   RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,LG1,R1)  *RKZF(LG1,K1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,-LG1,K1) *RKZF(-LG1,R1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,LG1,R1) *RKZF(LG1,K1,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2)     *RKZF(LG2,K2,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,-LG1,K1)*RKZF(-LG1,R1,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,LG1,R1)  *RKZF(LG1,K1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,LV,LEP2)
     .  *RKZF(LV,LEP1,RR2,RMB,-LG1,K1) *RKZF(-LG1,R1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,LG1,R1) *RKZF(LG1,K1,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)    *RKZF(-LG2,R2,RR1,RMB,-LV,LEP1)
     .  *RKZF(-LV,LEP2,RR2,RMB,-LG1,K1)*RKZF(-LG1,R1,P2,RMB,L2,Q2)*VMA
 
      DO 300 K=0,3
        RR1(K)=P1(K)+QV(K)
        RR2(K)=RR1(K)-K1(K)
  300 CONTINUE
      RR1(4)=8D0
      RR2(4)=6D0
      ZDV12=(RKDOT(RR1,RR1)-RMB**2)*(RKDOT(RR2,RR2)-RMB**2)
      ZNV12 =
     .   RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,LG2,R2)  *RKZF(LG2,K2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,-LG2,K2) *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,LG2,R2) *RKZF(LG2,K2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,-LG2,K2)*RKZF(-LG2,R2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,LG2,R2)  *RKZF(LG2,K2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,LG1,R1)
     .  *RKZF(LG1,K1,RR2,RMB,-LG2,K2) *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,LG2,R2) *RKZF(LG2,K2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,-LG1,K1)
     .  *RKZF(-LG1,R1,RR2,RMB,-LG2,K2)*RKZF(-LG2,R2,P2,RMB,L2,Q2)*VMA
 
      DO 310 K=0,3
        RR1(K)=P1(K)+QV(K)
        RR2(K)=RR1(K)-K2(K)
  310 CONTINUE
      RR1(4)=8D0
      RR2(4)=7D0
      ZDV21=(RKDOT(RR1,RR1)-RMB**2)*(RKDOT(RR2,RR2)-RMB**2)
      ZNV21 =
     .   RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,LG1,R1)  *RKZF(LG1,K1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,-LG1,K1) *RKZF(-LG1,R1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,LG1,R1) *RKZF(LG1,K1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2)   *RKZF(LV,LEP1,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,-LG1,K1)*RKZF(-LG1,R1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,LG1,R1)  *RKZF(LG1,K1,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,LG2,R2)
     .  *RKZF(LG2,K2,RR2,RMB,-LG1,K1) *RKZF(-LG1,R1,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,LG1,R1) *RKZF(LG1,K1,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)  *RKZF(-LV,LEP2,RR1,RMB,-LG2,K2)
     .  *RKZF(-LG2,R2,RR2,RMB,-LG1,K1)*RKZF(-LG1,R1,P2,RMB,L2,Q2)*VMA
 
* COMPUTE THE DIAGRAMS SO FAR
      ZDIA1=ZN12V/ZD12V
      ZDIA2=ZN21V/ZD21V
      ZDIA3=ZN1V2/ZD1V2
      ZDIA4=ZN2V1/ZD2V1
      ZDIA5=ZNV12/ZDV12
      ZDIA6=ZNV21/ZDV21
 
* SECOND PART OF THE RESULT: THE NONABELIAN PART.
* THIS IS MADE UP PARTLY FROM THE ABELIAN PART AND PARTLY FROM
* NEW PIECES
* THE ASSIGNMENT OF THE 4TH COMPONENT LABELS IS NOW UNNECESSARY
* FOR RR1 SINCE IT DOES NOT OCCUR IN ANY ZF HERE
 
      S=2D0*RKDOT(K1,K2)
 
      DO 320 K=0,3
        RR1(K)=PP2(K)+QV(K)
  320 CONTINUE
      ZD11=S*(RKDOT(RR1,RR1)-RMB**2)
 
      ZC12V =
     . + RKZF(L1,Q1,P1,RMB,LG1,R1) *RKZSF(LG1,K1,LG2,R2)
     .  *RKZSF(LG2,K2,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1) *RKZSF(LG1,K1,LG2,R2)
     .  *RKZSF(LG2,K2,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1) *RKZSF(LG1,K1,-LG2,K2)
     .  *RKZSF(-LG2,R2,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG1,R1) *RKZSF(LG1,K1,-LG2,K2)
     .  *RKZSF(-LG2,R2,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)*RKZSF(-LG1,R1,LG2,R2)
     .  *RKZSF(LG2,K2,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)*RKZSF(-LG1,R1,LG2,R2)
     .  *RKZSF(LG2,K2,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)*RKZSF(-LG1,R1,-LG2,K2)
     .  *RKZSF(-LG2,R2,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG1,K1)*RKZSF(-LG1,R1,-LG2,K2)
     .  *RKZSF(-LG2,R2,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
 
      ZC21V =
     . + RKZF(L1,Q1,P1,RMB,LG2,R2) *RKZSF(LG2,K2,LG1,R1)
     .  *RKZSF(LG1,K1,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2) *RKZSF(LG2,K2,LG1,R1)
     .  *RKZSF(LG1,K1,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2) *RKZSF(LG2,K2,-LG1,K1)
     .  *RKZSF(-LG1,R1,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LG2,R2) *RKZSF(LG2,K2,-LG1,K1)
     .  *RKZSF(-LG1,R1,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)*RKZSF(-LG2,R2,LG1,R1)
     .  *RKZSF(LG1,K1,LV,LEP2)  *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)*RKZSF(-LG2,R2,LG1,R1)
     .  *RKZSF(LG1,K1,-LV,LEP1) *RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)*RKZSF(-LG2,R2,-LG1,K1)
     .  *RKZSF(-LG1,R1,LV,LEP2) *RKZF(LV,LEP1,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LG2,K2)*RKZSF(-LG2,R2,-LG1,K1)
     .  *RKZSF(-LG1,R1,-LV,LEP1)*RKZF(-LV,LEP2,P2,RMB,L2,Q2)*VMA
      ZDIA7=(-ZN12V+ZN21V)/ZD11-(ZC12V-ZC21V)/(2D0*S)
 
      DO 330 K=0,3
        RR1(K)=P1(K)+QV(K)
  330 CONTINUE
      ZD22=S*(RKDOT(RR1,RR1)-RMB**2)
 
      ZCV12 =
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2) *RKZSF(LV,LEP1,LG1,R1)
     .  *RKZSF(LG1,K1,LG2,R2)    *RKZF(LG2,K2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2) *RKZSF(LV,LEP1,LG1,R1)
     .  *RKZSF(LG1,K1,-LG2,K2)   *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2) *RKZSF(LV,LEP1,-LG1,K1)
     .  *RKZSF(-LG1,R1,LG2,R2)   *RKZF(LG2,K2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,LV,LEP2) *RKZSF(LV,LEP1,-LG1,K1)
     .  *RKZSF(-LG1,R1,-LG2,K2)  *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VPA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)*RKZSF(-LV,LEP2,LG1,R1)
     .  *RKZSF(LG1,K1,LG2,R2)    *RKZF(LG2,K2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)*RKZSF(-LV,LEP2,LG1,R1)
     .  *RKZSF(LG1,K1,-LG2,K2)   *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)*RKZSF(-LV,LEP2,-LG1,K1)
     .  *RKZSF(-LG1,R1,LG2,R2)   *RKZF(LG2,K2,P2,RMB,L2,Q2)*VMA
     . + RKZF(L1,Q1,P1,RMB,-LV,LEP1)*RKZSF(-LV,LEP2,-LG1,K1)
     .  *RKZSF(-LG1,R1,-LG2,K2)  *RKZF(-LG2,R2,P2,RMB,L2,Q2)*VMA
 
* THE FOURTH COMBINATION CAN BE GOTTEN FROM
* THE FIRST THREE USING DIRAC ALGEBRA:
* EPS1*EPS2*EPVS+EPS2*EPS1*EPSV = 2(EPS1.EPS2)*EPSV ETC.
      ZCV21=ZC12V+ZC21V-ZCV12
 
      ZDIA8=(-ZNV12+ZNV21)/ZD22-(ZCV12-ZCV21)/(2D0*S)
 
* CONSTRUCT THE ABELIAN AND NONABELIAN PART
 
      ZABEL= ZDIA1+ZDIA2+ZDIA3+ZDIA4+ZDIA5+ZDIA6
      ZNABEL=ZDIA1-ZDIA2+ZDIA3-ZDIA4+ZDIA5-ZDIA6
      ZNABEM=2D0*ZDIA7+2D0*ZDIA8
      ZNABEL=ZNABEL-ZNABEM
      ZABEL=ZABEL*ZFAC*ZFACL
      ZNABEL=ZNABEL*ZFAC*ZFACL
 
* INCLUDE COLOUR FACTORS:
* (N**2-1)*(N**2-2)/(8*N) = 7/3 FOR THE ABELIAN PART
* N*(N**2-1)/8 = 3 FOR THE NONABELIAN PART
* AND ADD THE RESULT TO THE CROSS SECTION
      THIS1=7D0/3D0*ABS(ZABEL)**2+3D0*ABS(ZNABEL)**2
CC    WRITE(6,801)HELIX,LG1,LG2,LV,L1,L2,THIS1
CC801 FORMAT(' ',6I4,D30.20)
      CROSS=CROSS+THIS1
 
* END OF THE BIG LOOP OVER HELICITIES
  340 CONTINUE
 
* DO NOT FORGET TO PUT P2 BACK TO ITS ORIGINAL VALUE IN PP2!
      DO 350 K=0,3
        P2(K)=PP2(K)
  350 CONTINUE
 
* ADD AVERAGING FACTORS:
* 1/2 FOR EACH GLUON SPIN, 1/8 FOR EACH GLUON COLOUR
      CROSS=CROSS/256D0
 
* TAKE INTO ACCOUNT A POSSIBLE FACTOR FOR THE HELICITY SUM OPTION
* AND RETURN THE FINAL RESULT
      IF(IMC.EQ.1) CROSS=CROSS*MULT
      RESULT=CROSS
      END
 
*==================================================================
 
      FUNCTION RKZF(L1,P1,Q,RMB,L2,P2)
* COMPUTES THE SCALAR STRUCTURE
* U_BAR(L1,P1)(SLASH(Q)+RMB)U(L2,P2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX RKZF,RKZPR,RKZSF
      COMPLEX ANSF(-1:1,1:4,1:8,-1:1,1:4)
      INTEGER DONF(-1:1,1:4,1:8,-1:1,1:4)
      COMMON/RKZFCO/ANSF,DONF
      DIMENSION P1(0:4),P2(0:4),Q(0:4),R(0:4)
* CHECK ON CORRECT LABEL INPUT
      IP1=IDINT(P1(4))
      IQ=IDINT(Q(4))
      IP2=IDINT(P2(4))
      IF(IABS(L1).NE.1.OR.IABS(L2).NE.1.OR.
     . IP1.LT.1.OR.IP1.GT.4            .OR.
     . IQ.LT.1.OR.IQ.GT.8              .OR.
     . IP2.LT.1.OR.IP2.GT.4) THEN
        WRITE(6,*) ' RKZF LABEL ERROR'
        WRITE(6,*) 'L1=',L1,' IP1=',IP1,' IQ=',IQ,
     .             ' L2=',L2,' IP2=',IP2
        STOP
      ENDIF
* CHECK WHETHER THIS ONE HAS BEEN CALCULATED ALREADY
      IF(DONF(L1,IP1,IQ,L2,IP2).EQ.0) THEN
* THIS ONE NOT DONE YET: DO IT AND STORE THE RESULT IN ARRAY 'ANSF'
        IF(L1.EQ.L2) THEN
          A=2D0*RKDOT(Q,P2)
C         IF(DABS(A).LT.(1D-10*P2(0)*Q(0))) THEN
C...The check above is extended to following.
          IF(ABS(A).LT.MAX(1D-8,ABS(1D-10*P2(0)*Q(0)))) THEN
            ANSF(L1,IP1,IQ,L2,IP2)=(0.,0.)
          ELSE
            A=RKDOT(Q,Q)/A
            DO 100 K=0,3
              R(K)=Q(K)-A*P2(K)
  100       CONTINUE
            IF(R(0).GT.0D0) THEN
              C=1D0
            ELSE
              DO 110 K=0,3
                R(K)=-R(K)
  110         CONTINUE
              C=-1D0
            ENDIF
            ANSF(L1,IP1,IQ,L2,IP2)=C*RKZPR(L1,P1,R)*RKZPR(-L1,R,P2)
          ENDIF
        ELSEIF(L1.EQ.-L2) THEN
          ANSF(L1,IP1,IQ,L2,IP2)=RMB*RKZSF(L1,P1,L2,P2)
        ELSE
          WRITE(6,*) ' ERROR IN RKZF: L1=',L1,'  L2=',L2
          STOP
        ENDIF
        RKZF=ANSF(L1,IP1,IQ,L2,IP2)
        DONF(L1,IP1,IQ,L2,IP2)=1
      ELSE
        RKZF=ANSF(L1,IP1,IQ,L2,IP2)
      ENDIF
      END
 
*==================================================================
 
      FUNCTION RKRAND(IDUMMY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DATA INIT/0/
      IF(INIT.EQ.0) THEN
        INIT=1
        X=DMOD(DSQRT(2D0),1D0)
        Y=DMOD(DSQRT(3D0),1D0)
        Z=DMOD(DSQRT(5D0),1D0)
      ELSE
        X=DMOD(X+Y+Z,1D0)
        Y=DMOD(X+Y+Z,1D0)
        Z=DMOD(X+Y+Z,1D0)
      ENDIF
      RKRAND=X
      END
 
*==================================================================
 
      FUNCTION RKDOT(P,Q)
      DOUBLE PRECISION P(0:4),Q(0:4),RKDOT
      RKDOT=P(0)*Q(0)-P(1)*Q(1)-P(2)*Q(2)-P(3)*Q(3)
      END
 
*==================================================================
 
      FUNCTION RKZPR(L,Q1,Q2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX RKZPR
      DIMENSION Q1(0:4),Q2(0:4)
      IF(IABS(L).NE.1) THEN
        WRITE(6,*) ' RKZPR: ERROR   L=',L
        STOP
      ENDIF
C...Introduce cutoff to check that R1 and R2 not zero.
      R1=DSQRT(MAX(1D-10,Q1(0)-Q1(1)))
      R2=DSQRT(MAX(1D-10,Q2(0)-Q2(1)))
      RKZPR=CMPLX(SNGL(Q1(2)),SNGL(Q1(3)))*R2/R1
     .     -CMPLX(SNGL(Q2(2)),SNGL(Q2(3)))*R1/R2
      IF(L.EQ.-1) RKZPR=-CONJG(RKZPR)
      END
 
*==================================================================
 
      FUNCTION RKZSF(L1,P1,L2,P2)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMPLEX RKZSF,RKZPR
      COMPLEX ANSS(-1:1,1:4,-1:1,1:4)
      INTEGER DONS(-1:1,1:4,-1:1,1:4)
      COMMON/RKZSCO/ANSS,DONS
      DIMENSION P1(0:4),P2(0:4)
* CHECK ON CORRECT LABEL INPUT
      IP1=IDINT(P1(4))
      IP2=IDINT(P2(4))
      IF(IABS(L1).NE.1.OR.IABS(L2).NE.1.OR.
     . IP1.LT.1.OR.IP2.GT.4.OR.IP2.LT.1.OR.IP2.GT.4) THEN
       WRITE(6,*)
     .  ' RKZSF: ERROR L1=',L1,' L2=',L2,' IP1=',IP1,' IP2=',IP2
       STOP
      ENDIF
* CHECK WHETER THIS ONE WAS ALREADY COMPUTED
* DONS(,,,)=0: NOT YET COMPUTED, DONS(,,,)=1: ALREADY COMPUTED
* IF NOT YET COMPUTED: COMPUTE IT, AND STORE IN ARRAY 'ANSS'
* IF ALREADY COMPUTED: GET THE RESULT FROM ARRAY 'ANSS'
      IF(DONS(L1,IP1,L2,IP2).EQ.0) THEN
        IF(L1.EQ.L2) THEN
          ANSS(L1,IP1,L2,IP2)=(0.,0.)
        ELSE
          ANSS(L1,IP1,L2,IP2)=RKZPR(L1,P1,P2)
        ENDIF
        DONS(L1,IP1,L2,IP2)=1
      ENDIF
      RKZSF=ANSS(L1,IP1,L2,IP2)
      END
 
*==================================================================
 
      SUBROUTINE RKHLPK(NUM,LGL1,LGL2,LLV,LL1,LL2)
      IMPLICIT INTEGER(A-Z)
      DIMENSION CONFIG(32,6)
      DATA INIT/0/
      IF(INIT.EQ.0) THEN
        INIT=1
        MUM=0
        DO 140 GL1=1,-1,-2
          DO 130 GL2=1,-1,-2
            DO 120 LV=1,-1,-2
              DO 110 L1=1,-1,-2
                DO 100 L2=1,-1,-2
                  MUM=MUM+1
                  CONFIG(MUM,1)=GL1
                  CONFIG(MUM,2)=GL2
                  CONFIG(MUM,3)=LV
                  CONFIG(MUM,4)=L1
                  CONFIG(MUM,5)=L2
  100           CONTINUE
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
  140   CONTINUE
      ENDIF
      LGL1=CONFIG(NUM,1)
      LGL2=CONFIG(NUM,2)
      LLV =CONFIG(NUM,3)
      LL1 =CONFIG(NUM,4)
      LL2 =CONFIG(NUM,5)
      END
 
C*********************************************************************
 
      SUBROUTINE PYKCUT(MCUT)
 
C...Dummy routine, which the user can replace in order to make cuts on
C...the kinematics on the parton level before the matrix elements are
C...evaluated and the event is generated. The cross-section estimates
C...will automatically take these cuts into account, so the given
C...values are for the allowed phase space region only. MCUT=0 means
C...that the event has passed the cuts, MCUT=1 that it has failed.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      INTEGER  ISET,KFPR,ICOL
      REAL  COEF
      SAVE /PYINT2/
C...Set default value (accepting event) for MCUT.
      MCUT=0
 
C...Read out subprocess number.
      ISUB=MINT(1)
      ISTSB=ISET(ISUB)
 
C...Read out tau, y*, cos(theta), tau' (where defined, else =0).
      TAU=VINT(21)
      YST=VINT(22)
      CTH=0.
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) CTH=VINT(23)
      TAUP=0.
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUP=VINT(26)
 
C...Calculate x_1, x_2, x_F.
      IF(ISTSB.LE.2.OR.ISTSB.GE.6) THEN
        X1=SQRT(TAU)*EXP(YST)
        X2=SQRT(TAU)*EXP(-YST)
      ELSE
        X1=SQRT(TAUP)*EXP(YST)
        X2=SQRT(TAUP)*EXP(-YST)
      ENDIF
      XF=X1-X2
 
C...Calculate shat, that, uhat, p_T^2.
      SHAT=TAU*VINT(2)
      SQM3=VINT(63)
      SQM4=VINT(64)
      RM3=SQM3/SHAT
      RM4=SQM4/SHAT
      BE34=SQRT(MAX(0.,(1.-RM3-RM4)**2-4.*RM3*RM4))
      RPTS=4.*VINT(71)**2/SHAT
      BE34L=SQRT(MAX(0.,(1.-RM3-RM4)**2-4.*RM3*RM4-RPTS))
      RM34=2.*RM3*RM4
      RSQM=1.+RM34
      RTHM=(4.*RM3*RM4+RPTS)/(1.-RM3-RM4+BE34L)
      THAT=-0.5*SHAT*MAX(RTHM,1.-RM3-RM4-BE34*CTH)
      UHAT=-0.5*SHAT*MAX(RTHM,1.-RM3-RM4+BE34*CTH)
      PT2=MAX(VINT(71)**2,0.25*SHAT*BE34**2*(1.-CTH**2))
 
C...Decisions by user to be put here.
 
C...Stop program if this routine is ever called.
C...You should not copy these lines to your own routine.
      WRITE(MSTU(11),5000)
      IF(RLU(0).LT.10.) STOP
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link your PYKCUT routine ',
     &'correctly.'/1X,'Dummy routine in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYEVWT(WTXS)
 
C...Dummy routine, which the user can replace in order to multiply the
C...standard PYTHIA differential cross-section by a process- and
C...kinematics-dependent factor WTXS. For MSTP(142)=1 this corresponds
C...to generation of weighted events, with weight 1/WTXS, while for
C...MSTP(142)=2 it corresponds to a modification of the underlying
C...physics.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER  MINT
      REAL  VINT
      SAVE /PYINT1/
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      INTEGER  ISET,KFPR,ICOL
      REAL  COEF
      SAVE /PYINT2/
C...Set default weight for WTXS.
      WTXS=1.
 
C...Read out subprocess number.
      ISUB=MINT(1)
      ISTSB=ISET(ISUB)
 
C...Read out tau, y*, cos(theta), tau' (where defined, else =0).
      TAU=VINT(21)
      YST=VINT(22)
      CTH=0.
      IF(ISTSB.EQ.2.OR.ISTSB.EQ.4.OR.ISTSB.EQ.6) CTH=VINT(23)
      TAUP=0.
      IF(ISTSB.GE.3.AND.ISTSB.LE.5) TAUP=VINT(26)
 
C...Read out x_1, x_2, x_F, shat, that, uhat, p_T^2.
      X1=VINT(41)
      X2=VINT(42)
      XF=X1-X2
      SHAT=VINT(44)
      THAT=VINT(45)
      UHAT=VINT(46)
      PT2=VINT(48)
 
C...Modifications by user to be put here.
 
C...Stop program if this routine is ever called.
C...You should not copy these lines to your own routine.
      WRITE(MSTU(11),5000)
      IF(RLU(0).LT.10.) STOP
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link your PYEVWT routine ',
     &'correctly.'/1X,'Dummy routine in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYUPIN(ISUB,TITLE,SIGMAX)
 
C...Routine to be called by user to set up a user-defined process.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYINT2/ISET(200),KFPR(200,2),COEF(200,20),ICOL(40,4,2)
      INTEGER  ISET,KFPR,ICOL
      REAL  COEF
      SAVE /PYINT2/
      COMMON/PYINT6/PROC(0:200)
      CHARACTER PROC*28
      SAVE /PYINT6/
      CHARACTER*(*) TITLE
 
C...Check that subprocess number free.
      IF(ISUB.LT.1.OR.ISUB.GT.200.OR.ISET(ISUB).GE.0) THEN
        WRITE(MSTU(11),5000) ISUB
        STOP
      ENDIF
 
C...Fill information on new process.
      ISET(ISUB)=11
      COEF(ISUB,1)=SIGMAX
      PROC(ISUB)=TITLE//' '
 
C...Format for error output.
 5000 FORMAT(1X,'Error: user-defined subprocess code ',I4,
     &' not allowed.'//1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PYUPEV(ISUB,SIGEV)
 
C...Dummy routine, to be replaced by user. When called from PYTHIA
C...the subprocess number ISUB will be given, and PYUPEV is supposed
C...to generate an event of this type, to be stored in the PYUPPR
C...commonblock. SIGEV gives the differential cross-section associated
C...with the event, i.e. the acceptance probability of the event is
C...taken to be SIGEV/SIGMAX, where SIGMAX was given in the PYUPIN
C...call.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      COMMON/PYUPPR/NUP,KUP(20,7),PUP(20,5),NFUP,IFUP(10,2),Q2UP(0:10)
      INTEGER  NUP,KUP,NFUP,IFUP
      REAL  PUP,Q2UP
      SAVE /PYUPPR/
C...Stop program if this routine is ever called.
C...You should not copy these lines to your own routine.
      WRITE(MSTU(11),5000)
      IF(RLU(0).LT.10.) STOP
      SIGEV=ISUB
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link your PYUPEV routine ',
     &'correctly.'/1X,'Dummy routine in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE PDFSET(PARM,VALUE)
 
C...Dummy routine, to be removed when PDFLIB is to be linked.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      CHARACTER*20 PARM(20)
      DOUBLE PRECISION VALUE(20)
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      IF(RLU(0).LT.10.) STOP
      PARM(20)=PARM(1)
      VALUE(20)=VALUE(1)
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link PDFLIB correctly.'/
     &1X,'Dummy routine PDFSET in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
 
C*********************************************************************
 
      SUBROUTINE STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
 
C...Dummy routine, to be removed when PDFLIB is to be linked.
      COMMON/GUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      INTEGER  MSTU,MSTJ
      REAL  PARU,PARJ
      SAVE /GUDAT1/
      DOUBLE PRECISION XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      IF(RLU(0).LT.10.) STOP
      UPV=XX+QQ
      DNV=XX+2.*QQ
      USEA=XX+3.*QQ
      DSEA=XX+4.*QQ
      STR=XX+5.*QQ
      CHM=XX+6.*QQ
      BOT=XX+7.*QQ
      TOP=XX+8.*QQ
      GLU=XX+9.*QQ
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link PDFLIB correctly.'/
     &1X,'Dummy routine STRUCTM in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
