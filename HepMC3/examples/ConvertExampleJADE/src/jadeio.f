      SUBROUTINE JFOPEN(FILEN,U,L)
      IMPLICIT NONE
      character*80 FILEN
      INTEGER U, I, L, IERR
      write(*,*)U, FILEN(1:L)
      OPEN (U, FILE=FILEN(1:L), STATUS='REPLACE')
 
      RETURN
      END

      SUBROUTINE JFCLOSE(U)
      IMPLICIT NONE
      INTEGER U
      CLOSE(U)
      RETURN
      END

      SUBROUTINE JFWRITE(U, M)
      IMPLICIT NONE      
      integer U,M, IERR
      
      if ( M. EQ. 0 ) then 
      CALL WRCPRD(U,IERR)
      end if 
      if ( M. EQ. 1 ) then 
      CALL PRCPRD(U,IERR)
      end if 
      if (IERR .NE. 0) then 
      WRITE(*,*)"ERROR IN WRITE ",IERR
      end if
      RETURN
      END

      function LOCCPROD()
      INTEGER LOCCPROD,A,B
      
      REAL PP,PF,XM,XMF,BEAM,PT,THETA,PHI,PSTRT
      INTEGER NEV,NP,NC,NN,JCH,JTP,JP,NF,NCF,NNF,ICF,ITF,IFLAVR
      CHARACTER*16 CP,CF
      COMMON/CPROD/ NEV,BEAM,PT,PHI,THETA,IFLAVR,
     *        NP,NC,NN,PP(4,500),XM(500),JCH(500),JTP(500),JP(500,2),
     *        NF,NCF,NNF,PF(4,300),XMF(300),ICF(300),ITF(300),
     *        PSTRT(3,300)
      COMMON/CHCPRD/ CP(500),CF(300)
      
      
      
      
      
      A=LOC(CP)
      B=LOC(CF)
      LOCCPROD=LOC(NEV)
      
      end


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

      write(*,*) LUN,' np=', NP, ' Nev= ', NEV, BEAM
      
      
      
C            RETURN
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

      write(*,*) LUN,' np=', NP, ' Nev= ', NEV, BEAM
C
      do I1=1,500
      CP(I1)=' add later      '
      end do
      do I1=1,300
      CF(I1)=' add later      '
      end do
C
      WRITE(LUN,'(/A,I8)') ' PRCPRD: Content of common /CPROD/ in event'
     *     ,NEV
      WRITE(LUN,'(/A6,I7,A6,F6.2,A8,I2)')
     *     ' NEV=',NEV,' BEAM=',BEAM,' IFLAVR=',IFLAVR
      WRITE(LUN,'(A14,3F7.4)') ' PT/PHI/THETA=',PT, PHI, THETA
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




c      SUBROUTINE PXJSP3(NTRAK,IPTK,PTRAK,SEVAL,ASEVEC,IERR)
c*.*********************************************************
c*. ------
c*. PXJSP3
c*. ------
c*. Routine to calculate the eigenvectors and eigenvalues of the
c*. momentum tensor. The eigenvectors of the momentum tensor are
c*. the same as the eigenvectors of the Sphericity matrix;
c*. the eigenvalues are related as noted below.
c*. Usage     :
c*.
c*.      INTEGER  IPTK,NTRAK
c*.      PARAMETER  (ITKDM.ge.3,NTRAK.gt.1)
c*.      INTEGER NTRAK,IERR
c*.      REAL  PTRAK (ITKDM,MXTRAK),
c*.     +      SEVEC (3,3.or.more),
c*.     +      SEVAL (3.or.more)
c*.
c*.      NTRAK = 1.to.MXTRAK
c*.      CALL PXJSP3 (NTRAK,ITKDM,PTRAK,SEVAL,ASEVEC,IERR)
c*. INPUT     : NTRAK    Total number of particles
c*. INPUT     : IPTK     First dimension of PTRAK array
c*. INPUT     : PTRAK    Particle 3-momentum array: Px,Py,Pz
c*. OUTPUT    : SEVAL    Sphericity Eigenvalues
c*. OUTPUT    : ASEVEC   Associated Sphericity Eigenvectors;
c*. OUTPUT    : IERR     = 0 if all is OK ;   = -1 otherwise
c*.
c*. Note:
c*. (i)    Sphericity  = (3./2.) * (SEVAL (1) + SEVAL (2))
c*. (ii)   Aplanarity  = (3./2.) *  SEVAL (1)
c*. (iii)  SEVAL (1) < SEVAL (2) < SEVAL (3)
c*. (iv)   ASEVEC (*,3) is the principal sphericity axis
c*.
c*. CALLS     : DSYEV (in the LAPACK library that ships with CERNLIB)
c*. CALLED    : By User
c*.
c*. AUTHOR    :  J.W.Gary
c*. CREATED   :  18-Mar-88
c*. LAST MOD  :  27-May-97
c*.
c*. Modification Log.
c*.
c*. 27-May-97 M.Schroder Preset IERR
c*. 04-Apr-97 : Rewrote to use the LAPACK eigenvalue routine to fix the
c*.             instability when differnece between eigenvalues is large.
c*.             D. Hutchcroft
c*.
c*.*********************************************************
c*
c*
c      INTEGER NTRAK
c      INTEGER IPTK
c      REAL PTRAK(IPTK,*)
c      REAL SEVAL(*)
c      REAL ASEVEC(3,*)
c      INTEGER IERR
c*
c* Integers for the LAPACK routine
c      INTEGER N
c*     order of the matrix A
c      INTEGER LDA
c*     order of the first index of A
c      INTEGER LWORK
c*     no of elements in work (should be 64*N)
c      INTEGER IFAIL
c* returns <0 if inputs wrong, >0 if fails numerically

c* Doubles for the LAPACK routine
c      DOUBLE PRECISION A(3,3)
c*     Matrix to be diagonialised
c      DOUBLE PRECISION W(3)
c*     Array of eigenvalues
c      DOUBLE PRECISION WORK(192)
c*     temp array
c*
c* Character strings for the LAPACK routine
c      CHARACTER*1 JOBZ
c*     'N' if only eigenvalues, 'V' if also calculates the eigenvectors
c      CHARACTER*1 UPLO
c*     'U' if uses upper diagonal, 'L' if lower
c*
c* routine
c      EXTERNAL DSYEV
c*
c* local
c      INTEGER I,J,K
c      DOUBLE PRECISION PSQI
c*
c      IERR = 0
c      IF (NTRAK.LE.1) THEN
c          WRITE (6,FMT='('' PXLAJSP3: Error, NTRAK ='',I4)')
c     +          NTRAK
c          IERR = -1
c          GO TO 990
c      END IF
c* Null the Sphericity Matrix
c      DO 200 I=1,3
c         DO 210 J=1,3
c            A(I,J)=0.D0
c 210     CONTINUE
c 200  CONTINUE
c* Fill sphereicity matrix
c      PSQI=0.D0
c      DO 220 I=1,NTRAK
c         PSQI=PSQI+(PTRAK(1,I)**2+PTRAK(2,I)**2+PTRAK(3,I)**2)
c         DO 230 J=1,3
c            DO 240 K=1,3
c               A(J,K)=A(J,K)+PTRAK(J,I)*PTRAK(K,I)
c 240        CONTINUE
c 230     CONTINUE
c 220  CONTINUE
c*
c      DO 250 I=1,3
c         DO 260 J=1,3
c            A(I,J)=A(I,J)/PSQI
c 260     CONTINUE
c 250  CONTINUE
c*
c* Set up the input for a 3x3 matrix to dsyev
c      N=3
c      LDA=3
c      LWORK=192
c      IFAIL=0
c      JOBZ='V'
c      UPLO='U'

c      CALL DSYEV(JOBZ,UPLO,N,A,LDA,W,WORK,LWORK,IFAIL)
c      IF (IFAIL.LT.0) THEN
c         PRINT *,'ngjps3 ifail = 1 wrong arguments'
c         IERR=1
c         STOP
c      ENDIF
c      IF (IFAIL.GT.0) THEN
c         PRINT *,'Diagonalisation failed numerically in PXLAJSP3'
c         IERR=2
c         GOTO 990
c      ENDIF
c*
c* copy eigenvalues to seval
c      DO 270 I=1,3
c         SEVAL(I)=REAL(W(I))
c 270  CONTINUE
c* copt eigenvectors to asevec
c      DO 280 I=1,3
c         DO 290 J=1,3
c            ASEVEC(I,J)=REAL(A(I,J))
c 290     CONTINUE
c 280  CONTINUE
c 990  CONTINUE

c      END













c      SUBROUTINE MYTYPE(ITYPE)
c      IMPLICIT NONE
c*. WWTYPE   Determines event type for WW or Z/gamma events.
c*.
c*. INPUT     :
c*. OUTPUT    :  ITYPE = 1  Z  --> hadrons
c*.                      2  Z  --> leptons
c*.                      3  WW --> jjjj
c*.                      4  WW --> jjll
c*.                      5  WW --> llll
c*.                      6  ZZ --> jjjj
c*.                      7  ZZ --> jjll
c*.                      8  ZZ --> llll
c*.                      0  anything else
c*.
c*.**********************************************************************
c*.
c      INTEGER  NLUPDM,NPLBUF
c      PARAMETER  (NLUPDM=4000,NPLBUF=5)
c      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
c      INTEGER  N,K
c      REAL  P,V
c      SAVE /GUJETS/
c      INTEGER ITYPE,NW,NZ,NQQ,ITREE,IID,NPAR,IPAR(2)
c*
c      ITYPE = 0
c      NPAR  = 0
c      IPAR(1) = 0
c      IPAR(2) = 0
c*
c*--- Find number of Ws and Zs, and determine if they decay to quarks
c*--- or leptons..
c*
c      NW = 0
c      NZ = 0
c      NQQ = 0
c      DO 10 ITREE = 1,N
c        IID = ABS(K(ITREE,2))
c        IF(K(ITREE,3).NE.0) GO TO 10
c        IF(IID.LT.23 .OR. IID.GT.24) GO TO 10
c        NPAR = NPAR + 1
c        IPAR(NPAR) = ITREE
c        IF(IID.EQ.23) THEN
c          NZ = NZ + 1
c        ELSE
c          NW = NW + 1
c        ENDIF
c   10 CONTINUE
c      DO 20 ITREE = 1,N
c        IID = ABS(K(ITREE,2))
c        IF(K(ITREE,3).EQ.0) GO TO 20
c        IF(K(ITREE,3).NE.IPAR(1) .AND. K(ITREE,3).NE.IPAR(2)) GO TO 20
c        IF(IID.GE.1.AND.IID.LE.8) NQQ = NQQ + 1
c   20 CONTINUE
c      IF(NW.EQ.0 .AND. NZ.EQ.0) GO TO 999
c*
c*--- Classify event.
c*
c      IF(NW.EQ.2) THEN
c        IF(NQQ.EQ.4) THEN
c          ITYPE = 3
c        ELSEIF(NQQ.EQ.2) THEN
c          ITYPE = 4
c        ELSEIF(NQQ.EQ.0) THEN
c          ITYPE = 5
c        ELSE
c          ITYPE = 0
c        ENDIF
c      ELSEIF(NZ.EQ.2) THEN
c        IF(NQQ.EQ.4) THEN
c          ITYPE = 6
c        ELSEIF(NQQ.EQ.2) THEN
c          ITYPE = 7
c        ELSEIF(NQQ.EQ.0) THEN
c          ITYPE = 8
c        ELSE
c          ITYPE = 0
c        ENDIF
c      ELSEIF(NZ.EQ.1) THEN
c        IF(NQQ.EQ.2) THEN
c          ITYPE = 1
c        ELSEIF(NQQ.EQ.0) THEN
c          ITYPE = 2
c        ELSE
c          ITYPE = 0
c        ENDIF
c      ENDIF
c*
c  999 RETURN
c      END




c      SUBROUTINE MCGETF(JFLAV)
c      IMPLICIT NONE
cC  Get event flavour in Z/gamma events
cC  Output:  JFLAV  flavour code, 1,..,6 = u,d,s,c,b,t
cC                                =0: could not determine flavour
cC  Author: Stefan Kluth
cC  Created: 06.11.96
cC  Modifications:
c      INTEGER  NLUPDM,NPLBUF
c      PARAMETER  (NLUPDM=4000,NPLBUF=5)
c      COMMON/GUJETS/N,K(NLUPDM,5),P(NLUPDM,NPLBUF),V(NLUPDM,5)
c      INTEGER  N,K
c      REAL  P,V
c      SAVE /GUJETS/
cC  Stuff for FFREAD:
c      INTEGER NAMLEN,NBYTEW,NWONAM
c      PARAMETER( NAMLEN=80, NBYTEW=4, NWONAM=NAMLEN/NBYTEW )
c      CHARACTER*(NAMLEN) CHMCTYP,CHEVFILE
c      LOGICAL LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB
c      INTEGER NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV,IDSTAB,KEVT,JADETUN
c      REAL ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,JTA,JTLAMB
c      COMMON /MCFFR/
c     &     NEVNT,IMCRDM,NRTOPR,IRUN,SFLAV
c     &     ,ECMASS,JTB,JTSQ,JTQ0,JTEC,JTEB,IDSTAB(50) ! PMF 27/07/00 JTEC,JTEB,I
c     &     ,JTA,JTLAMB           ! PMF 28/05/01 JTA,JTLAMB added
c     &     ,LSTBL,LUDSC,LBQRK,LISR,LFSR,LSTART,L4VEC,LTAB,KEVT ! PMF 01/11/00 LT
c     &     ,JADETUN              ! PMF 10/02/01
c     &     ,CHMCTYP,CHEVFILE
c      SAVE /MCFFR/
c      INTEGER JFLAV,I
cC
c      JFLAV= 0
c      IF( INDEX(CHMCTYP,'PYTHIA').GT.0 .OR.
c     &    INDEX(CHMCTYP,'HERWIG').GT.0 .OR.
c     &    INDEX(CHMCTYP,'ARIADNEP').GT.0 ) THEN
cC       Try the obvious and trace partons back to Z/gamma:
c        DO I=1,N
c          IF( ABS(K(I,2)).GE.1 .AND. ABS(K(I,2)).LE.6 .AND.
c     &        K(K(I,3),2).EQ.23 ) THEN
c            JFLAV= ABS(K(I,2))
c            GOTO 10
c          ENDIF
c        ENDDO
c   10   CONTINUE
c      ELSE
cC       Try first quark with no parent:
cC       (Jetset6.3 and Cojets)
c        DO I=1,N
c          IF( ABS(K(I,2)).GE.1 .AND. ABS(K(I,2)).LE.6 .AND.
c     &        K(I,3).EQ.0 ) THEN
c            JFLAV= ABS(K(I,2))
c            GOTO 20
c          ENDIF
c        ENDDO
c   20   CONTINUE
c      ENDIF
