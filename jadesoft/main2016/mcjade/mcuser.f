C   14/12/83 401111710  MEMBER NAME  USER     (JADEGS)      FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCUSER(INDX0)
      IMPLICIT NONE
************************************************************************
*
*     User routine for the standard JADE tracking simulation
*     called by subroutine WRTMCB.
*
*     This version: Nov./99  Pedro Movilla Fernandez
*                   28/05/00 PMF last mod
*
************************************************************************
      INTEGER INDX0
C
C--- I/O Parameters
      INTEGER IUNIT,JUNIT,NCALI,KUNITA,LUNITA
      COMMON/CIOUNI/IUNIT,JUNIT,NCALI,KUNITA(10),LUNITA(10)
C
C--- BOS Parameters
      INTEGER IW
      INTEGER*2 HW
      REAL AW
      COMMON /BCS/ IW(40000)
      DIMENSION HW(80000),AW(40000)
      EQUIVALENCE (HW(1),IW(1),AW(1))
C
C--- USER Parameters
C Some special BOS pointers
      INTEGER IHEAD
      INTEGER IBLN
C
C Specify bank contents to be printed
      CHARACTER*100 CBANK(1)
      DATA CBANK(1) 
     +     /'HEAD VECT PALL LATC ATOF HITL ALGN JETC PATR VTXC'/
C
C User I/O
C     JADE convention: LUN=2: Event input  (cprod files)
C                         =3: Event output (bos files)
      INTEGER NEVT
      INTEGER IOVECT,IOBOS,IOBANK
      LOGICAL LBANK,LHIST,LSTART
      COMMON / MYIO / IOVECT,IOBOS,IOBANK,LBANK,LHIST,LSTART,NEVT
C
      INTEGER I,J,K,L,M,N,INDX,ICOUNT
      INTEGER*2 H
      DATA ICOUNT /0/
      SAVE ICOUNT
C
C------------------  C O D E  ------------------------------------------
C
      INDX=1
      ICOUNT=ICOUNT+1
      IF( MOD(ICOUNT,100).EQ.0 )
     >     WRITE(*,'(A,I7,A,I7,A)') 'MCUSER: tracked ',ICOUNT
     >     ,' out of ',NEVT,' events' 
C----------------------
C EVENT SIMULATED
C----------------------
C
 1000 CONTINUE
C
C... Print out JADE bank contents in readable ASCII format
      IF( LBANK ) THEN
         WRITE(IOBANK,1900)
 1900    FORMAT(/'***'/'*** MCUSER:  EVENT SIMULATED'/'***'/)
C Print run, event, ebeam
         IHEAD=IW(IBLN('HEAD'))
         WRITE(IOBANK,'(2X,A,6I8)') 'Date',(HW(IHEAD*2+K),K=3,8)
         WRITE(IOBANK,'(3(2x,A,I8))') 'Run',HW(IHEAD*2+10)
     +        ,' Event',HW(IHEAD*2+11),' Type:',HW(IHEAD*2+12) 
         WRITE(IOBANK,'(2X,A,I8)') 'Ebeam',HW(IHEAD*2+29)
C
C Print out selected banks   
         CALL SHOWB(IOBANK,CBANK(INDX))
      ENDIF
C
C... Do some histograming
      IF( LHIST ) CALL MCFILL
C
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE MCBOOK
      IMPLICIT NONE
C
C Book histograms
C
C PAW
      INTEGER IDNE,IDCH,IDTRK,IDHIT,IDLG
     +     ,IDCAP,IDBAR,IDPTOT,IDPT,IDTOT
      COMMON /USERBK/ IDNE,IDCH,IDTRK,IDHIT,IDLG
     +     ,IDCAP,IDBAR,IDPTOT,IDPT,IDTOT
Code:
      IDNE=101
      IDCH=102
      IDTRK=103
      IDHIT=104
      IDLG=105
      IDCAP=106
      IDBAR=107
      IDPTOT=108
      IDPT=109
      IDTOT=110
      CALL HCDIR('//PAWC',' ')
      CALL HSTAF('YES')
      CALL HBOOK1(IDNE,'N OF NEUTR PARTICLES (INPUT)',100,0.,100.,0.)
      CALL HBOOK1(IDCH,'NO OF CHRGD PARTICLES (INPUT)',100,0.,100.,0.)
      CALL HBOOK1(IDTRK,'NO OF CHRGD PARTICLES (PATR)',100,0.,100.,0.)
      CALL HBOOK1(IDHIT,'NO OF I.D. HITS',100,0.,1500.,0. )
      CALL HBOOK1(IDLG,'LG TOT ENERGY',50,0.,50.,0. )
      CALL HBOOK1(IDCAP,'LG END CAP ENERGY',50,0.,50.,0. )
      CALL HBOOK1(IDBAR,'LG BARREL ENERGY',50,0.,50.,0. )
      CALL HBOOK1(IDPTOT,'SUM PTOT',50,0.,50.,0. )
      CALL HBOOK1(IDPT,'SUM PT',50,0.,50.,0. )
      CALL HBOOK1(IDTOT,'SUM PTOT + LG',50,0.,50.,0. )
C
      RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE MCFILL
      IMPLICIT NONE
C
C Calculate histograms
C

C BOS
      INTEGER IW
      REAL RW
      INTEGER*2 HW
CAV      COMMON / BCS / IW(1)
      COMMON / BCS / IW(40000)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1), RW(1), IW(1))
      INTEGER IBLN
C PAW
      INTEGER NWPAW,ICYCLE,HMEMOR
      PARAMETER (NWPAW=5000000)
      COMMON /PAWC/HMEMOR(NWPAW)
      INTEGER IDNE,IDCH,IDTRK,IDHIT,IDLG
     +     ,IDCAP,IDBAR,IDPTOT,IDPT,IDTOT
      COMMON /USERBK/ IDNE,IDCH,IDTRK,IDHIT,IDLG
     +     ,IDCAP,IDBAR,IDPTOT,IDPT,IDTOT
C
      INTEGER J,I,IND,IPH,LP,IP0,IP9,NHITS,IL,IH,IC,IEC
      REAL BKGAUS,PT,PTOT,PTRANS,ACURV,ETOT,ECAP
C
C Code:
C

C
C...  # OF INPUT PARTICLES
      IND = IW( IBLN('VECT') )
      IF( IND .EQ. 0 ) GOTO 1000
      CALL HF1( IDNE, REAL( IW( IND +  6 ) ), 1. )
      CALL HF1( IDCH, REAL( IW( IND +  5 ) ), 1. )

C
C...  # OF PATR TRACKS
 1000 IND = IW( IBLN('PATR') )
      IF( IND .EQ. 0 ) GO TO 2000
      CALL HF1(IDTRK,REAL( IW( IND +  2 ) ),1.)
C
C...  MOMENTA OF PATR TRACKS
      IPH = IW( IBLN( 'HEAD' ) )
      IF( IPH .EQ. 0 ) GO TO 2000
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
 1100 PTOT = PTOT + PTRANS * SQRT( 1. + RW(J+30)**2 )
      CALL HF1( IDPTOT, PTOT ,1. )
      CALL HF1( IDPT, PT ,1. )
C
C...  # OF JETC HITS
 2000 IND = IW( IBLN('JETC') )
      IF( IND .EQ. 0 ) GO TO 3000
      NHITS = ( HW(IND*2+99) - HW(IND*2+3) ) / 4
      CALL HF1( IDHIT, REAL(NHITS) ,1. )
C
C...  LG ENERGY
 3000 IND = IW( IBLN('ALGN') )
      IF( IND .EQ. 0 ) GO TO 4000
      ETOT = 0.
      ECAP = 0.
C
      IL = IND*2 + 6
      IH = IL + HW(IL) - 3
      IEC = IL + HW(IND*2+4) - 3
C
      DO 3010 I = IL,IH, 2
      IF( I .LE. IEC ) GO TO 3010
      ECAP = ECAP + HW(I+2)
 3010 ETOT = ETOT + HW(I+2)
      ECAP = ECAP * .001
      ETOT = ETOT * .001
C
      CALL HF1( IDLG, ETOT, 1. )
      CALL HF1( IDCAP, ECAP, 1. )
      CALL HF1( IDBAR, ETOT - ECAP, 1. )
C
C... TOTAL ENERGY
      CALL HF1( IDTOT, ETOT + PTOT, 1. )
C
 4000 RETURN
      END
C
C-----------------------------------------------------------------------
      SUBROUTINE MCEND
      IMPLICIT NONE
C
C Save histograms
C
C
      INTEGER NWPAW,ICYCLE,HMEMOR
      PARAMETER (NWPAW=5000000)
      COMMON /PAWC/HMEMOR(NWPAW)
Code:
      CALL HCDIR('//PAWC',' ')
      CALL HLDIR('//PAWC',' ')
C
      RETURN
      END
