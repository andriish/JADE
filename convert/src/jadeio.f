      SUBROUTINE JFOPEN(FILEN,U,O,L)
      IMPLICIT NONE
      character*80 FILEN
      INTEGER U, I, L, IERR, O
      write(*,*)U, FILEN(1:L)
      if (O .EQ. 0) then
      OPEN (U, FILE=FILEN(1:L), STATUS='REPLACE',form='unformatted')
      end if
      if (O .EQ. 1) then
      OPEN (U, FILE=FILEN(1:L), STATUS='REPLACE',form='formatted')
      end if

      if (O .EQ. 2) then
      OPEN (U, FILE=FILEN(1:L), CONVERT='LITTLE_ENDIAN', 
     +STATUS='REPLACE',form='unformatted')
      end if

      if (O .EQ. 3) then
      OPEN (U, FILE=FILEN(1:L), CONVERT='BIG_ENDIAN', 
     +STATUS='REPLACE',form='unformatted')
      end if

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
      WRITE(*,*)"M=",M,"U=",U
      if ( M. EQ. 0 ) then 
      CALL WRCPRD(U,IERR)
      CALL PRCPRD(6,IERR)
      end if 
      if ( M. EQ. 1 ) then 
      CALL PRCPRD(U,IERR)
      end if 
      if (IERR .NE. 0) then 
      WRITE(*,*)"ERROR IN WRITE ",IERR
      end if
      RETURN
      END

      function locCHCPRD()
      INTEGER locCHCPRD,A,B



      COMMON/CHCPRD/ CP(500),CF(300)
      CHARACTER*16 CP,CF
       locCHCPRD=loc(CP)
      end 
      
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




      REAL XDUMMY(10),SVEC(3,3), ANG
      INTEGER IER2
      REAL RADDEG
      PARAMETER( RADDEG=57.2958 )
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
         ENDIF

C      write(*,*) LUN,' np=', NP, ' Nev= ', NEV, BEAM
      
      
      
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

C*********************************************************************
C*********************************************************************
C*********************************************************************
 
      FUNCTION LUCHGE(KF)
      COMMON/LUDAT2/KTYP(120),PMAS(120),PWID(60),KFR(80),CFR(40)
 
      KFA=IABS(KF)
      LUCHGE=0
 
C...CALCULATE 3*CHARGE FOR PARTICLES AND PARTONS
      IF(KFA.LE.100) THEN
        KTY=MOD(KTYP(KFA),10)
        IF(KTY.GE.1) LUCHGE=3*KTY-6
 
      ELSEIF(KFA.LT.500) THEN
        CALL LUIFLV(KFA,IFLA,IFLB,IFLC,KSP)
        LUCHGE=3*MOD(KTYP(100+IFLA),10)-16+
     &  (3*MOD(KTYP(100+IABS(IFLB)),10)-16)*ISIGN(1,IFLB)
        IF(IFLC.NE.0) LUCHGE=LUCHGE+3*MOD(KTYP(100+IFLC),10)-16
 
      ELSEIF(KFA.LE.600) THEN
        IF(MOD(KFA,10).NE.0) LUCHGE=3*MOD(KTYP(100+MOD(KFA,10)),10)-16
        IF(KFA.GT.510) LUCHGE=LUCHGE+3*MOD(KTYP(50+KFA/10),10)-16
 
      ELSEIF(KFA.LE.700) THEN
        IF(MOD(KFA,10).NE.0) LUCHGE=3*MOD(KTYP(100+MOD(KFA,10)),10)-16
      ENDIF
 
      LUCHGE=LUCHGE*ISIGN(1,KF)
 
      RETURN
      END
 
      SUBROUTINE LUIFLV(KF,IFLA,IFLB,IFLC,KSP)
      COMMON/LUDAT2/KTYP(120),PMAS(120),PWID(60),KFR(80),CFR(40)
 
      KFA=IABS(KF)
      KFS=ISIGN(1,KF)
      IFLA=0
      IFLB=0
      IFLC=0
 
C...RECONSTRUCT SPIN FOR HADRON
      KSP=-1
      IF((KFA.GE.17.AND.KFA.LE.26).OR.KFA.EQ.37.OR.KFA.EQ.38.OR.
     &(KFA.GE.83.AND.KFA.LE.86).OR.KFA.GE.101) KSP=0
      IF((KFA.GE.27.AND.KFA.LE.36).OR.(KFA.GE.87.AND.KFA.LE.90).OR.
     &KFA.GE.123) KSP=1
      IF((KFA.GE.41.AND.KFA.LE.56).OR.KFA.GE.145) KSP=2
      IF((KFA.GE.57.AND.KFA.LE.60).OR.KFA.GE.241) KSP=3
      IF((KFA.GE.61.AND.KFA.LE.80).OR.KFA.GE.293) KSP=4
      IF(KFA.GE.393) KSP=-1
 
C...RECONSTRUCT FLAVOUR CONTENT FOR MESON
      IF((KFA.GE.23.AND.KFA.LE.26).OR.(KFA.GE.33.AND.KFA.LE.36).OR.
     &(KFA.GE.83.AND.KFA.LE.90)) THEN
        IF(KFA.LE.40) IFLA=KFA-22-10*KSP
        IF(KFA.GE.80) IFLA=KFA-78-4*KSP
        IFLB=-IFLA
      ELSEIF(KFA.EQ.37.OR.KFA.EQ.38) THEN
CAV        IFLA=ISIGN(3,(-1)**INT(RLU(0)+0.5))
        IFLA=ISIGN(3,(-1)**INT(0+0.5))
        IFLB=ISIGN(2,-IFLA)
      ELSEIF(KSP.EQ.0.OR.KSP.EQ.1) THEN
  100   IFLA=IFLA+1
        IF(IFLA.LT.8.AND.KFR(8*KSP+IFLA+1).LT.KFA) GOTO 100
        IFLB=-(KFA-KFR(8*KSP+IFLA))
        IF(IFLA.LE.3) IFLB=-IFLB
        IF(IFLA.LE.3) IFLA=-IFLA
 
C...RECONSTRUCT FLAVOUR CONTENT FOR BARYON
      ELSEIF(KSP.GE.2.AND.KSP.LE.4) THEN
  110   IFLA=IFLA+1
        IF(IFLA.LT.8.AND.KFR(16*KSP+IFLA-15).LT.KFA) GOTO 110
  120   IFLB=IFLB+1
        IF(IFLB.LT.8.AND.KFR(16*KSP+IFLB-7).LT.KFA-KFR(16*KSP+IFLA-16))
     &  GOTO 120
        IFLC=KFA-KFR(16*KSP+IFLA-16)-KFR(16*KSP+IFLB-8)
      ENDIF
 
      IFLA=KFS*IFLA
      IFLB=KFS*IFLB
      IFLC=KFS*IFLC
 
      RETURN
      END
 
 
 
 
      SUBROUTINE LUNAME(KF,CHAU)
      COMMON/LUDAT2/KTYP(120),PMAS(120),PWID(60),KFR(80),CFR(40)
      COMMON/LUDAT4/CHAG(50),CHAF(100)
      CHARACTER CHAU*8,CHAG*4,CHAF*4
      
C...LUDAT2, WITH PARTICLE DATA AND FLAVOUR TREATMENT
      DATA KTYP/0,10,23,3*0,1,2,1,2,1,2,1,2,2*0,2*3,2*2,2*3,4*0,33,43,
     &52,2,2*3,60,70,80,5*0,3,2,3,2,1,2,1,4,3,2,3,2*2,4,2*3,2,2*3,2,94,
     &103,112,121,133,142,151,162,171,1,4,3,2,3,2*2,4,2*3,4,10*0,2,3,
     &8*0,6,2*5,6,5,6,5,6,12*0/
      DATA PMAS/0.,94.,83.,15.,2*0.,.00051,0.,.1057,0.,1.7842,0.,60.,
     &3*0.,.1396,.4937,.4977,1.8646,1.8693,1.9705,.135,.5488,.9576,
     &2.981,.7714,.8921,.8965,2.0072,2.0101,2.11,.7717,.7826,1.0195,
     &3.0969,2*.4977,2*0.,.9383,.9396,1.1894,1.1925,1.1973,1.3149,
     &1.3213,3*2.44,2*2.55,2.74,2*3.63,3.81,1.1156,2.2812,2*2.46,1.23,
     &1.231,1.232,1.233,1.3828,1.3837,1.3872,1.5318,1.535,1.6724,3*2.5,
     &2*2.63,2.8,2*3.69,3.85,4.9,2*0.,9.4,77.99001,118.,397.,9.46,78.,
     &118.,397.,5000.,300.,900.,7*0.,2*.325,.5,1.6,5.,40.,60.,200.,
     &2*0.,.2,.1,0.,.11,.16,.048,4*0./
      DATA PWID/2.8,20.,2.8,20.,.148,.4,.05,.2,.052,.2,.153,.4,.01,.1,
     &.004,.015,.115,.14,.115,.14,.115,.14,.115,.14,.036,.035,.036,
     &.035,.039,.04,.009,.05,.01,.05,26*0./
      DATA KFR/0,16,17,19,100,104,109,115,0,26,27,29,122,126,131,137,
     &0,40,42,47,144,158,178,205,0,1,3,6,10,15,21,28,0,0,56,57,240,
     &246,256,271,0,0,1,3,6,10,15,21,60,61,64,70,292,307,328,356,
     &0,1,3,6,10,15,21,28,16*0/
      DATA CFR/0.5,0.25,0.5,0.25,1.,0.5,0.5,0.,0.5,0.,1.,1.,0.75,0.,
     &0.5,0.,0.,1.,0.1667,0.3333,0.0833,0.6667,0.1667,0.3333,-3.,1.,
     &-2.,-2.,1.,0.,0.,-3.,1.,1.,1.,5*0./
      
C...LUDAT4, CONTAINING CHARACTER STRINGS
      DATA CHAG/' ','HA','LA','TA','BA','CA','SA','DA','UA','G','U','D',
     &'S','C','B','T','L','H','SPEC','QRA','QBRA','JET','B--','B-','B',
     &'B+',' ','-','0','+','++','1','0','*',' ','DFBV','JQ','IQ',
     &' ',' ','STAB','UNST',8*' '/
      DATA CHAF/'GAMM','Z0','W','HIGG','GA/Z',' ','E','NUE','MU',
     &'NUMU','TAU','NUTA','CHI','NUCH','PHAS',' ','PI',2*'K',2*'D','F',
     &'PI0','ETA','ETA''','ETAC','RHO',2*'K*',2*'D*','F*','RHO0',
     &'OMEG','PHI','JPSI','K0S','K0L',2*' ','P','N',3*'SIG',2*'XI',
     &3*'SIC','CSU1','CSD1','CSS1','CCU1','CCD1','CCS1','LAM','LAMC',
     &'CSU0','CSD0',4*'DELT',3*'SIG*',2*'XI*','OME*',3*'SIC*','CSU*',
     &'CSD*','CSS*','CCU*','CCD*','CCS*','CCC*',2*' ','ETAB','ETAT',
     &'ETAL','ETAH','UPSI','PHIT','PHIL','PHIH','R','HIGG','Z''0',
     &7*' '/
 
      CHAU=CHAG(1)//CHAG(1)
      KFA=IABS(KF)
      KFS=ISIGN(1,KF)
 
      IF(KFA.EQ.0) THEN
C...PARTICLE NAMES: ORDINARY AND HEAVY HADRONS
      ELSEIF(KFA.LE.100) THEN
        CHAU=CHAF(KFA)//CHAG(27+KFS*MOD(KTYP(KFA),10))
      ELSEIF(KFA.LT.500) THEN
        CALL LUIFLV(KFA,IFLA,IFLB,IFLC,KSP)
        IF(IFLC.EQ.0) CHAU=CHAG(10+IFLA)(1:1)//CHAG(10-IFLB)(1:2)//
     &  CHAG(35-KSP)(1:1)//CHAG(27+KFS*(LUCHGE(KFA)/3+2))
        IF(IFLC.NE.0) CHAU=CHAG(10+IFLA)(1:1)//CHAG(10+IFLB)(1:1)//
     &  CHAG(10+IFLC)(1:1)//CHAG(30+KSP)(1:1)//CHAG(27+KFS*
     &  (LUCHGE(KFA)/3+2))
 
C...JET NAMES: GLUON, QUARKS AND DIQUARKS; ALSO SPECTATOR AND PHASESPACE
      ELSEIF(KFA.LT.590) THEN
        IFLA=MAX(KFA/10-50,KFA-10*(KFA/10))
        IFLB=MIN(KFA/10-50,KFA-10*(KFA/10))
        IF(IFLB.EQ.0) CHAU=CHAG(10+KFS*IFLA)//CHAG(22)
        KSP=32
        IF(KFA/10-50.LT.IFLA) KSP=33
        IF(IFLB.NE.0) CHAU=CHAG(10+IFLA)(1:1)//CHAG(10+KFS*IFLB)(1:2)//
     &  CHAG(KSP)(1:1)//CHAG(22)
      ELSEIF(KFA.LE.600) THEN
        CHAU=CHAG(KFA-571)//CHAG(22)
 
      ELSEIF(KFA.LT.700) THEN
C...HADRON JETS: J AND I QUARK
        IFLA=ISIGN(KFA/10-60,KF)
        IFLB=ISIGN(MOD(KFA,10),KF)
        IF(IFLA.NE.0) CHAU(1:4)=CHAG(10+IFLA)(1:2)//CHAG(37)(1:2)
        IF(IFLB.NE.0) CHAU(5:8)=CHAG(10+IFLB)(1:2)//CHAG(38)(1:2)
      ENDIF
 
      RETURN
      END
 
C*********************************************************************



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
      CHARACTER*8 CHAU
Code:



      REAL XDUMMY(10),SVEC(3,3), ANG
      INTEGER IER2
      REAL RADDEG
      PARAMETER( RADDEG=57.2958 )
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
         ENDIF


C      write(*,*) LUN,' np=', NP, ' Nev= ', NEV, BEAM
C
      do I1=1,500
C      CP(I1)=' kalet '
      end do
      do I1=1,100
      CALL LUNAME(I1,CHAU)
      write(*,*),I1,CHAU
      end do
      do I1=1,300
      CF(I1)='jjjj ' 
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

      SUBROUTINE PXANXY (XX,YY,ANG,IERR)
*.*********************************************************
*. ------
*. PXANXY
*. ------
*. SOURCE: Jetset7.1 (T. Sjostrand)
*. Reconstruct the azimuthal angle of a vector,
*. given the X and Y components of a vector
*. Usage     :
*.
*.      INTEGER  IERR
*.      REAL  XX,YY,ANG
*.
*.      CALL PXANXY (XX,YY,ANG,IERR)
*.
*. INPUT     : XX      The X component of a vector
*. INPUT     : YY      The Y component of a vector
*. OUTPUT    : ANG     The azimuthal angle
*. OUTPUT    : IERR    = 0 if all is OK ;   = -1 otherwise
*.
*.*********************************************************
      IMPLICIT NONE
      REAL  PIII
      PARAMETER  (PIII=3.1415927)
      INTEGER  IERR
      REAL  XX,YY,ANG
      DOUBLE PRECISION  ULANGL,RRR,XXX,YYY

      IERR = 0
      XXX = XX
      YYY = YY
      RRR = DSQRT (XXX**2 + YYY**2)
      IF (RRR.LT.1E-20) GO TO 990
      IF ((DABS (XXX)/RRR).LT.0.8) THEN
          ULANGL = DSIGN (DACOS (XXX/RRR),YYY)
      ELSE
          ULANGL = DASIN (YYY/RRR)
          IF (XXX.LT.0..AND.ULANGL.GE.0.) THEN
              ULANGL = PIII - ULANGL
          ELSE IF (XXX.LT.0.) THEN
              ULANGL = - PIII - ULANGL
          END IF
      END IF
      ANG = ULANGL

      RETURN
 990  IERR = -1
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




      SUBROUTINE PXJSP3(NTRAK,IPTK,PTRAK,SEVAL,ASEVEC,IERR)
*.*********************************************************
*. ------
*. PXJSP3
*. ------
*. Routine to calculate the eigenvectors and eigenvalues of the
*. momentum tensor. The eigenvectors of the momentum tensor are
*. the same as the eigenvectors of the Sphericity matrix;
*. the eigenvalues are related as noted below.
*. Usage     :
*.
*.      INTEGER  IPTK,NTRAK
*.      PARAMETER  (ITKDM.ge.3,NTRAK.gt.1)
*.      INTEGER NTRAK,IERR
*.      REAL  PTRAK (ITKDM,MXTRAK),
*.     +      SEVEC (3,3.or.more),
*.     +      SEVAL (3.or.more)
*.
*.      NTRAK = 1.to.MXTRAK
*.      CALL PXJSP3 (NTRAK,ITKDM,PTRAK,SEVAL,ASEVEC,IERR)
*. INPUT     : NTRAK    Total number of particles
*. INPUT     : IPTK     First dimension of PTRAK array
*. INPUT     : PTRAK    Particle 3-momentum array: Px,Py,Pz
*. OUTPUT    : SEVAL    Sphericity Eigenvalues
*. OUTPUT    : ASEVEC   Associated Sphericity Eigenvectors;
*. OUTPUT    : IERR     = 0 if all is OK ;   = -1 otherwise
*.
*. Note:
*. (i)    Sphericity  = (3./2.) * (SEVAL (1) + SEVAL (2))
*. (ii)   Aplanarity  = (3./2.) *  SEVAL (1)
*. (iii)  SEVAL (1) < SEVAL (2) < SEVAL (3)
*. (iv)   ASEVEC (*,3) is the principal sphericity axis
*.
*. CALLS     : DSYEV (in the LAPACK library that ships with CERNLIB)
*. CALLED    : By User
*.
*. AUTHOR    :  J.W.Gary
*. CREATED   :  18-Mar-88
*. LAST MOD  :  27-May-97
*.
*. Modification Log.
*.
*. 27-May-97 M.Schroder Preset IERR
*. 04-Apr-97 : Rewrote to use the LAPACK eigenvalue routine to fix the
*.             instability when differnece between eigenvalues is large.
*.             D. Hutchcroft
*.
*.*********************************************************
*
*
      INTEGER NTRAK
      INTEGER IPTK
      REAL PTRAK(IPTK,*)
      REAL SEVAL(*)
      REAL ASEVEC(3,*)
      INTEGER IERR
*
* Integers for the LAPACK routine
      INTEGER N
*     order of the matrix A
      INTEGER LDA
*     order of the first index of A
      INTEGER LWORK
*     no of elements in work (should be 64*N)
      INTEGER IFAIL
* returns <0 if inputs wrong, >0 if fails numerically

* Doubles for the LAPACK routine
      DOUBLE PRECISION A(3,3)
*     Matrix to be diagonialised
      DOUBLE PRECISION W(3)
*     Array of eigenvalues
      DOUBLE PRECISION WORK(192)
*     temp array
*
* Character strings for the LAPACK routine
      CHARACTER*1 JOBZ
*     'N' if only eigenvalues, 'V' if also calculates the eigenvectors
      CHARACTER*1 UPLO
*     'U' if uses upper diagonal, 'L' if lower
*
* routine
      EXTERNAL DSYEV
*
* local
      INTEGER I,J,K
      DOUBLE PRECISION PSQI
*
      IERR = 0
      IF (NTRAK.LE.1) THEN
          WRITE (6,FMT='('' PXLAJSP3: Error, NTRAK ='',I4)')
     +          NTRAK
          IERR = -1
          GO TO 990
      END IF
* Null the Sphericity Matrix
      DO 200 I=1,3
         DO 210 J=1,3
            A(I,J)=0.D0
 210     CONTINUE
 200  CONTINUE
* Fill sphereicity matrix
      PSQI=0.D0
      DO 220 I=1,NTRAK
         PSQI=PSQI+(PTRAK(1,I)**2+PTRAK(2,I)**2+PTRAK(3,I)**2)
         DO 230 J=1,3
            DO 240 K=1,3
               A(J,K)=A(J,K)+PTRAK(J,I)*PTRAK(K,I)
 240        CONTINUE
 230     CONTINUE
 220  CONTINUE
*
      DO 250 I=1,3
         DO 260 J=1,3
            A(I,J)=A(I,J)/PSQI
 260     CONTINUE
 250  CONTINUE
*
* Set up the input for a 3x3 matrix to dsyev
      N=3
      LDA=3
      LWORK=192
      IFAIL=0
      JOBZ='V'
      UPLO='U'

      CALL DSYEV(JOBZ,UPLO,N,A,LDA,W,WORK,LWORK,IFAIL)
      IF (IFAIL.LT.0) THEN
         PRINT *,'ngjps3 ifail = 1 wrong arguments'
         IERR=1
         STOP
      ENDIF
      IF (IFAIL.GT.0) THEN
         PRINT *,'Diagonalisation failed numerically in PXLAJSP3'
         IERR=2
         GOTO 990
      ENDIF
*
* copy eigenvalues to seval
      DO 270 I=1,3
         SEVAL(I)=REAL(W(I))
 270  CONTINUE
* copt eigenvectors to asevec
      DO 280 I=1,3
         DO 290 J=1,3
            ASEVEC(I,J)=REAL(A(I,J))
 290     CONTINUE
 280  CONTINUE
 990  CONTINUE

      END
