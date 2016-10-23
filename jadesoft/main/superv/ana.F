**************************************************
*     
*     SUPERV analyses routines called
*     by SUBROUTINE USER
*
*     07/12/1999  P.A. Movilla Fernandez
*
**************************************************
      SUBROUTINE HSTDEF
      IMPLICIT NONE
**************************************************
*
*     Book histograms
*
**************************************************
* PAW
#include "cana.for"
*
      INTEGER L,ID,I
Code:
      DO L=2,9
C VECT
      IF( L.EQ.2 ) THEN
      CALL HBOOK1(ID('NNEU, VECT',-L)
     +     ,'No of neutr particles, VECT',100,0.,100.,0.)
      CALL HBOOK1(ID('NCH, VECT',-L)
     +     ,'No of chrgd particles, VECT',100,0.,100.,0.)
      ENDIF
C ALGN
      IF( L.EQ.3 ) THEN
      CALL HBOOK1(ID('LG TOT, ALGN',-L)
     +     ,'LG total energy, ALGN',50,0.,50.,0. )
      CALL HBOOK1(ID('LG EC, ALGN',-L)
     +     ,'LG end cap energy, ALGN',50,0.,50.,0. )
      CALL HBOOK1(ID('LG BAR, ALGN',-L)
     +     ,'LG barrel energy, ALGN',50,0.,50.,0. )
      ENDIF
C JETC
      IF( L.EQ.2 ) THEN
      CALL HBOOK1(ID('ID HITS',-L)
     +     ,'Number of I.D. hits, JETC',100,0.,1500.,0.)
      ENDIF
C PATR
      IF( L.EQ.5 .OR. L.EQ.7) THEN
      CALL HBOOK1(ID('PTOT, PATR',-L)
     +     ,'Sum ptot, PATR',50,0.,50.,0.)
      CALL HBOOK1(ID('PT, PATR',-L)
     +     ,'Sum pt, PATR',50,0.,50.,0. )
      CALL HBOOK1(ID('NCH, PATR',-L)
     +     ,'Number of charged particles, PATR',100,0.,100.,0.)
      ENDIF
C ZVTX
      IF( L.EQ.4 ) THEN
      CALL HBOOK1(ID('FAST Z',-L)
     +     ,'Fast z vertex, ZVTX',100,-300.,300.,0.)
      CALL HBOOK1(ID('Z HITS',-L)
     +     ,'Number of hits in z peak, ZVTX',100,0.,800.,0.)
      ENDIF
C LGCL
      IF( L.EQ.6 .OR. L.EQ.7 ) THEN
      CALL HBOOK1(ID('# CLUS TOT',-L)
     +     ,'Number of clusters, total, LGCL',80,0.,80.,0. )
      CALL HBOOK1(ID('# CLUS BAR',-L)
     +     ,'Number of clusters, barrel, LGCL',80,0.,80.,0. )
      CALL HBOOK1(ID('# CLUS +EC',-L)
     +     ,'Number of clusters, +z end cap, LGCL',80,0.,80.,0. )
      CALL HBOOK1(ID('# CLUS -EC',-L)
     +     ,'Number of clusters, -z end cap, LGCL',80,0.,80.,0. )
      
      CALL HBOOK1(ID('LG E TOT',-L)
     +     ,'LG total energy, LGCL',50,0.,50.,0. )
      CALL HBOOK1(ID('LG E BAR',-L)
     +     ,'LG barrel energy, LGCL',50,0.,50.,0. )
      CALL HBOOK1(ID('LG E +EC',-L)
     +     ,'LG +z end cap energy, LGCL',50,0.,50.,0. )
      CALL HBOOK1(ID('LG E -EC',-L)
     +     ,'LG -z end cap energy, LGCL',50,0.,50.,0. )

      CALL HBOOK1(ID('# CLUS, U',-L)
     +     ,'Number of unassociated clusters, LGCL',40,0.,40.,0. )
      CALL HBOOK1(ID('LG E TOT, U',-L)
     +     ,'Energy of unassociated clusters, total, LGCL'
     +     ,50,0.,50.,0. )
      CALL HBOOK1(ID('LG E BAR, U',-L)
     +     ,'Energy of unassociated clusters, barrel, LGCL'
     +     ,50,0.,50.,0. )
      CALL HBOOK1(ID('LG E +EC, U',-L)
     +     ,'Energy of unassociated clusters, +z end cap, LGCL'
     +     ,50,0.,50.,0. )
      CALL HBOOK1(ID('LG E -EC, U',-L)
     +     ,'Energy of unassociated clusters, -z end cap, LGCL'
     +     ,50,0.,50.,0. )
      CALL HBOOK1(ID('LG E MIN',-L)
     +     ,'LG minimum energy',100,0.,.5,0. )
      ENDIF
C
      IF( L.EQ.8 ) THEN 
C LGCL + PATR
      CALL HBOOK1(ID('P+E, PATR LGCL',-L)
     +     ,'Sum Ptot + Etot, PATR LGCL',50,0.,50.,0. )
C ALGN + PATR
      CALL HBOOK1(ID('P+E, PATR ALGN',-L)
     +     ,'Sum Ptot + Etot, PATR ALGN',50,0.,50.,0. )
      ENDIF

      ENDDO
C
      RETURN
      END
C
C
C
      SUBROUTINE HSTFLL(INDX)
      IMPLICIT NONE
**************************************************
*
*     Calculate histograms
*
**************************************************
      INTEGER INDX
* PAW
#include "cana.for"
* BOS
      INTEGER IW
      REAL RW
      INTEGER*2 HW
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1), RW(1), IW(1))
      INTEGER IBLN
*
      INTEGER J,I,IND,IPH,LP,IP0,IP9,NHITS,IL,IH,IC,IEC
      INTEGER IP1,IP2,IP3,NCLST,NWPCL,IB
      REAL BKGAUS,PT,PTOT,PTRANS,ACURV,ETOT,ECAP,LGMIN
*
      INTEGER ID
      LOGICAL LDO,LPATR(9),LALGN(9),LLGCL(9)
      REAL EPATR(9),EALGN(9),ELGCL(9)
Code:
      IF( INDX.LE.2 ) THEN
         CALL VZERO(EPATR,9)
         CALL VZERO(EALGN,9)
         CALL VZERO(ELGCL,9)
         CALL VZERO(LPATR,9)
         CALL VZERO(LALGN,9)
         CALL VZERO(LLGCL,9)
      ENDIF
C VECT
C...  # of input particles
      LDO= INDX.EQ.2
      IND = IW( IBLN('VECT') )
      IF( IND.NE.0 .AND. LDO ) THEN
         CALL HHF1('NNEU, VECT',INDX, REAL( IW( IND +  6 ) ), 1. )
         CALL HHF1('NCH, VECT' ,INDX, REAL( IW( IND +  5 ) ), 1. )
      ENDIF
C PATR
C...  # of PATR tracks
      LDO = INDX.EQ.5 .OR. INDX.EQ.7
      IND = IW( IBLN('PATR') )
      IPH = IW( IBLN('HEAD') )
      IF( IND .NE. 0 .AND. IPH .NE. 0 .AND. LDO) THEN
         CALL HHF1('NCH, PATR',INDX, REAL( IW( IND +  2 ) ), 1. )
C...  Momenta of PATR tracks
         BKGAUS = HW( IPH*2 + 30 ) * .001
         BKGAUS = ABS( BKGAUS )
         PT = 0.
         PTOT = 0.
         LP = IW( IND + 3 )
         IP0 = IND + IW( IND + 1 )
         IP9 = IP0 + ( IW(IND+2) - 1 ) * LP
         DO 1100 J = IP0, IP9, LP
            ACURV = ABS( RW(J+25) )
            PTRANS = .3E-4 * BKGAUS / AMAX1( ACURV, 1.E-6 )
            PT = PT + PTRANS
 1100       PTOT = PTOT + PTRANS * SQRT( 1. + RW(J+30)**2 )
            CALL HHF1('PTOT, PATR',INDX, PTOT ,1. )
            CALL HHF1('PT, PATR',INDX, PT ,1. )
C... save results for later use
            LPATR(INDX)=.TRUE.
            EPATR(INDX)=PTOT
      ENDIF
C JETC
C...  # OF JETC HITS
      LDO=INDX.EQ.2
      IND = IW( IBLN('JETC') )
      IF( IND .NE. 0 .AND. LDO ) THEN
         NHITS = ( HW(IND*2+99) - HW(IND*2+3) ) / 4
 72      CALL HHF1('ID HITS',INDX, REAL(NHITS) ,1. )
      ENDIF
C ZVTX
      LDO=INDX.EQ.4
      IND =  IW( IBLN('ZVTX') )
      IF( IND.NE.0 ) THEN
c         write(*,'(A4,3I)') iw(ind-3),iw(ind-2),iw(ind-1),iw(ind)
c         print *,rw(ind+1),rw(ind+4)
         CALL HHF1('FAST Z',INDX, RW(IND+1), 1. )
         CALL HHF1('Z HITS',INDX, RW(IND+4), 1. )
      ENDIF
C ALGN
C...  LG energy
      LDO=INDX.EQ.3
      IND = IW( IBLN('ALGN') )
      IF( IND .NE. 0 .AND. LDO) THEN
         ETOT = 0.
         ECAP = 0.
         IL = IND*2 + 6
         IH = IL + HW(IL) - 3
         IEC = IL + HW(IND*2+4) - 3
         DO 3010 I = IL,IH, 2
            IF( I .LE. IEC ) GO TO 3010
            ECAP = ECAP + HW(I+2)
 3010       ETOT = ETOT + HW(I+2)
         ECAP = ECAP * .001
         ETOT = ETOT * .001
C
         CALL HHF1('LG TOT, ALGN',INDX, ETOT, 1. )
         CALL HHF1('LG EC, ALGN',INDX, ECAP, 1. )
         CALL HHF1('LG BAR, ALGN',INDX, ETOT - ECAP, 1. )
C ... save for later use
         LALGN(INDX)=.TRUE.
         EALGN(INDX)=ETOT
      ENDIF
C LGCL
      LDO = INDX.EQ.6 .OR. INDX.EQ.7
      IND = IW( IBLN('LGCL') )
      IF( IND .NE.0 .AND. LDO ) THEN
         IP1 = IW( IND+1 )
         IP2 = IW( IND+2 )
         IP3 = IW( IND+3 )
         NCLST= IW( IND + IP1 + 2 )
         NWPCL= IW( IND + IP1 + 20 )
C... No of Clusters
         CALL HHF1('# CLUS TOT',INDX, REAL(IW( IND+IP1+2 )), 1. )
         CALL HHF1('# CLUS BAR',INDX, REAL(IW( IND+IP1+3 )), 1. )
         CALL HHF1('# CLUS -EC',INDX, REAL(IW( IND+IP1+4 )), 1. )
         CALL HHF1('# CLUS +EC',INDX, REAL(IW( IND+IP1+5 )), 1. )
C... Shower Energies
         CALL HHF1('LG E TOT',INDX, RW( IND+IP1+6 ), 1. )
         CALL HHF1('LG E BAR',INDX, RW( IND+IP1+7 ), 1. )
         CALL HHF1('LG E -EC',INDX, RW( IND+IP1+8 ), 1. )
         CALL HHF1('LG E +EC',INDX, RW( IND+IP1+9 ), 1. )
C... No of "Photons" ( unassociated clusters )
         CALL HHF1('# CLUS, U',INDX, REAL(IW( IND+IP1+10 )), 1. )
C... "Photon" Energies
         CALL HHF1('LG E TOT, U',INDX, RW( IND+IP1+11 ), 1. )
         CALL HHF1('LG E BAR, U',INDX, RW( IND+IP1+12 ), 1. )
         CALL HHF1('LG E -EC, U',INDX, RW( IND+IP1+13 ), 1. )
         CALL HHF1('LG E +EC, U',INDX, RW( IND+IP1+14 ), 1. )
C... Loop over clusters
         LGMIN = 1.E9
         DO I=1, NCLST
            IB = IND+IP3+(I-1)*NWPCL-1
C... Minimal shower energy            
            IF( RW(IB+2) .LT. LGMIN ) LGMIN =  RW(IB+2) 
         ENDDO
         CALL HHF1('LG E MIN',INDX, LGMIN, 1. )
C... save for later user
         LLGCL(INDX)=.TRUE.
         ELGCL(INDX)= RW( IND+IP1+6 )
      ENDIF

C TOTAL ENERGY ( TRACKS + CLUSTERS )

      IF( INDX.EQ.8 ) THEN
      IF(LALGN(3).AND.LPATR(5))
     +     CALL HHF1('P+E, PATR ALGN',INDX, EPATR(5) + EALGN(3), 1. )
      IF(LLGCL(6).AND.LPATR(5))
     +     CALL HHF1('P+E, PATR LGCL',INDX, EPATR(5) + ELGCL(6), 1. )
      ENDIF
C
 4000 RETURN
      END
C
C
C
      SUBROUTINE HSTEND(INDX)
      IMPLICIT NONE
**************************************************
*
*     Final calculations
*
**************************************************
      INTEGER INDX
C PAW
#include "cana.for"
Code:
      CALL HCDIR('//PAWC',' ')
      CALL HLDIR('//PAWC',' ')
C
      RETURN
      END
C
C
C
      SUBROUTINE HHF1(CID,IND,X,W)
      IMPLICIT NONE
*******************************************************
*
*  Calls HBOOK histogram fill routine HF1. The 
*  histogram is specified by a mnemonic CID
*  which will be converted here into a unique histo id
*  before calling HF1. The histo id corresponds
*  the mnemonic CID and the supervisor level IND which 
*  is needed in ID to calculate an level specific
*  offset for the histo id.
*
*******************************************************
      INTEGER IND
      CHARACTER CID*(*)
      REAL X,W
      INTEGER IDEN,ID
C
      IDEN=ID(CID,IND)
      IF( IDEN.GT.0 ) CALL HF1(IDEN,X,W)
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION ID(CD,IND)
      IMPLICIT NONE
*******************************************************************
*
*  Administration of histogramm id's and corresponding mnemonics
*
*     INPUT:  CD  ... Character string defining the mnemonic
*             IND ... Supervisor level index with sign
*     OUTPUT: ID  ... Histo id corresponding to CD and ABS(IND)
* 
*     If a mnemonic was not yet defined, then IND must have
*     a negative sign in order to force the definition
*     of the mnemonic, otherwise the programm will abort.
*
*     If the mnemonic CD is already defined, then ID will return
*     an histogram id corresponding to CD and ABS(IND).
*     If the corresponding histogramm does not exist,
*     ID will have the value 0. At least 1 histogram corresponding 
*     to CD and one supervisor level exists, but in general there
*     does not exist respective histograms in ALL supervisor levels.
*
*     If a mnemonic CD is already defined and a histogram
*     exists corresponding to CD and ABS(IND), then it is
*     forbidden to call ID with IND.LT.0 (program will abort).
*
*******************************************************************
      CHARACTER CD*(*)
      INTEGER IND
      CHARACTER CDLIST*20(200)
      INTEGER MAX,I,ICODE
      LOGICAL HEXIST
      CHARACTER CDLIS2*1(4000)
      EQUIVALENCE (CDLIS2,CDLIST(1))
      DATA CDLIS2 /4000*' '/, ICODE/1/
      SAVE CDLIST,ICODE
Code:
      IF( CD.EQ.' '.OR. IND.EQ.1 ) THEN
         WRITE(*,901) CD,ID
 901     FORMAT('*** ERROR IN ID: Illegal arguments: CD ='
     +        ,A,' ID = ',I8)
         ID=0
         GOTO 800
      ENDIF
      DO I=1,ICODE
         IF( CD.EQ.CDLIST(I) ) THEN
            ID=I+1000000*ABS(IND)
            IF( HEXIST(ID) ) THEN
               IF( IND.GT.0 ) GOTO 999
               WRITE(*,903) CD,ID
 903           FORMAT('*** ERROR IN ID: Histogram already exists:'
     +              ,/,' CD = ',A,' ID= ',I9)
               GOTO 800
            ELSE
               IF( IND.LT.0 ) GOTO 999 ! return only id value
               ID=0
               GOTO 999
            ENDIF
         ELSE
            IF( IND.GT.0 .AND. I.EQ.ICODE ) THEN
               WRITE(*,904) CD
 904           FORMAT('*** ERROR IN ID: Histogram corresponding to '
     +              ,'CD = ',A,' is not defined yet.')
               GOTO 800
            ENDIF
         ENDIF
      ENDDO
      IF(ICODE+1.GT.200) THEN
         WRITE(*,902) ICODE+1
 902     FORMAT('*** ERROR IN ID: Maximum number of histograms'
     +        ,' exceeded, ICODE=',I8)
         ID=0
         GOTO 800
      ENDIF
      CDLIST(ICODE)=CD
      ID=ICODE+1000000*ABS(IND)
      ICODE=ICODE+1
      GOTO 999
C      
 800  WRITE(*,'(''... Will stop now!'')')
      STOP
C
 999  RETURN
      END
