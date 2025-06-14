C   24/10/81 206230903  MEMBER NAME  MCGJET   (GRAPHS)      FORTRAN
      SUBROUTINE MCGJET(NJET,Y)
C
C   GRAPHICS VERSION, ALL COMMONS PUT INTO /CWORK/
C                                           J.OLSSON   23.6.82
C
C
C  ********  SUBROUTINE FOR RECONSTRUCTION OF JET AXES IN   *******
C  ************    2-JET , 3-JET , 4-JET EVENTS        ***************
C
C                          ------------  M. C. GODDARD---------
C   VERSION NO. 478263900925
C
C --------------------------------------------------------------
C
C
C  INPUT :              ------------------
C
C       CALLING ARGUMENTS :
C
C           -- NJET --     NUMBER OF JETS TO BE RECONSTRUCTED
C           -- NJET --       (MUST BE GE 2 AND LE 4)
C
C           -- Y(3) --     NORMALIZED VECTOR IN THE DIRECTION OF THE
C           -- Y(3) --     EVENT PLANE NORMAL . CAN BE THE EIGENVECTOR
C           -- Y(3) --     OF THE SECOND LARGEST "Q" EIGENVALUE , OR
C           -- Y(3) --     THE ACCOPLANARITY DIRECTION .
C           -- Y(3) --     NOT NECESSARY FOR TWO-JET RECONSTRUCTION
C
C    *** N B ***  THIS VECTOR MUST HAVE UNIT NORM
C
C
C       COMMON BLOCKS :
C
C       -- /SENSE/ PP(4,100),INUM --  INPUT COMMON BLOCK CONTAINING
C       -- /SENSE/ PP(4,100),INUM --  THE THREE VECTORS OF THE
C       -- /SENSE/ PP(4,100),INUM --  TRACKS TO BE USED IN THE AXES
C       -- /SENSE/ PP(4,100),INUM --  DETERMINATION . INUM IS THE
C       -- /SENSE/ PP(4,100),INUM --  NUMBER OF TRACKS IN THE PP ARRAY .
C       -- /SENSE/ PP(4,100),INUM --  PP(1,J),PP(2,J),PP(3,J) ARE X,Y,Z
C       -- /SENSE/ PP(4,100),INUM --  MOMENTUM COMPONENTS FOR TRACK J .
C       -- /SENSE/ PP(4,100),INUM --  IPP(4,J) IS A TRACK INDEX WHICH
C       -- /SENSE/ PP(4,100),INUM --  IS JUST COPIED INTO THE OUTPUT
C       -- /SENSE/ PP(4,100),INUM --  COMMON BLOCK (SEE BELOW) AND CAN
C       -- /SENSE/ PP(4,100),INUM --  BE USED BY THE USER TO FURTHER
C       -- /SENSE/ PP(4,100),INUM --  ANALYZE A TRACK ASSIGNED TO ANY
C       -- /SENSE/ PP(4,100),INUM --  RECONSTRUCTED JET . THIS DOESNT
C       -- /SENSE/ PP(4,100),INUM --  HAVE TO BE FILLED .
C
C
C       -- /WT/ IWT --  IF IWT IS NON-ZERO , THE JET AXES FOR THREE
C       -- /WT/ IWT --  AND FOUR JET EVENTS WILL BE DETERMINED GIVING
C       -- /WT/ IWT --  A HIGHER WEIGHT TO THE FASTER PARTICLES . THE
C       -- /WT/ IWT --  DEFAULT IS IWT = 0 .
C
C
C  OUTPUT :                    ---------------------------
C
C          COMMON BLOCKS :
C
C         -- /MARKET/ PAR(3,4) --  NORMALIZED VECTORS IN THE DIRECTIONS
C         -- /MARKET/ PAR(3,4) --  OF THE RECONSTRUCTED JET AXES .
C         -- /MARKET/ PAR(3,4) --  PAR(1,J),PAR(2,J),PAR(3,J) ARE X,Y,Z
C         -- /MARKET/ PAR(3,4) --  COMPONENTS FOR JET J .
C
C
C         -- /COLD/ IJ(4),PTH(3,100,4),IPJ(100,4) --
C
C         --  IJ(J) CONTAINS THE NUMBER OF PARTICLES ASSOCIATED WITH
C         --        JET J .
C         --  PTH(3,K,J) CONTAINS THE MOMENTUM VECTOR FOR THE K'TH
C         --             TRACK OF JET J . K SHOULD RUN FROM 1 TO IJ(J) .
C         --  IPJ(K,J) IS THE TRACK INDEX FOR TRACK K OF JET J , JUST
C         --           COPIED FROM THE IPP(4,?) OF THE INPUT /SENSE/
C         --           COMMON BLOCK .
C
C
C         -- /CRUST/ AXIS(3),ATRST --  ANY CALL TO MCGJET CAUSES THE
C         -- /CRUST/ AXIS(3),ATRST --  THRUST AND THRUST AXIS TO BE
C         -- /CRUST/ AXIS(3),ATRST --  CALCULATED . AXIS(3) CONTAINS
C         -- /CRUST/ AXIS(3),ATRST --  THE NORMALIZED THRUST AXIS AND
C         -- /CRUST/ AXIS(3),ATRST --  ATRST THE THRUST VALUE. WHEN
C         -- /CRUST/ AXIS(3),ATRST --  THE CALL IS MADE WITH NJET=2
C         -- /CRUST/ AXIS(3),ATRST --  THE THRUST CALCULATION IS MADE
C         -- /CRUST/ AXIS(3),ATRST --  USING ONLY THE VECTORS IN THE
C         -- /CRUST/ AXIS(3),ATRST --  /SENSE/ COMMON. FOR ANY OTHER
C         -- /CRUST/ AXIS(3),ATRST --  VALUE OF NJET THE THRUST IS
C         -- /CRUST/ AXIS(3),ATRST --  CALCULATED USING A BALANCED SET
C         -- /CRUST/ AXIS(3),ATRST --  OF VECTORS .
C
C --------------------------------------------------------------
CAV      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF
CAV Require same size
      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF,POLD(3,4),TOLD,
     $ TR(4),MJET,IKONF,PMOD(100),YY(3),AXIS(3),ATRST,
     $ IJ(4),PTH(3,100,4),IPJ(100,4),INDX(100,4),THJ,THRUJ(3),
     $ TEMP(3,4),TPTH(3,100,4),ITPJ(100,4),ITJ(4),
     $ ANG(4),COMP(4),TREM(4),T(3)
C
      COMMON /WT/ IWT
C     COMMON /MARKET/ PAR(3,4)
C     COMMON /SENSE/ PP(4,100),INUM
C     COMMON /CWT/ JWT
      DIMENSION Y(3),X(3),Z(3),PSUM(3)
      DIMENSION XDUMMY(3)       !PMF 19.10.99: needed as dummy argument
C
C   FOR TWO JET RECON JWT = 0
C
      JWT=0
      IF(NJET.EQ.2) GO TO 222
C
C   BALANCE MOMENTUM BEFORE CALCULATING THRUST AXIS
C
      IREM=INUM
      IADD=INUM+1
      DO 56 IU=1,3
      PSUM(IU)=0.
      DO 57 IK=1,INUM
  57  PSUM(IU)=PSUM(IU)+PP(IU,IK)
  56  PP(IU,IADD)=-PSUM(IU)
      PPM=PP(1,IADD)**2+PP(2,IADD)**2+PP(3,IADD)**2
      IF(PPM.GT..0001) INUM=IADD
C
C   CALCULATE THRUST AXIS
C
      CALL JETREC(2,Y,XDUMMY,XDUMMY) !PMF 19/10/99: add dummy arguments in the parameter list
C
C   SET JWT TO INPUT VALUE
C
      JWT=IWT
C
C   RESET NUMBER OF TRACKS
C
      INUM=IREM
C
C   INITIALIZE X TO THRUST AXIS
C
      DO 44 IU=1,3
  44  X(IU)=PAR(IU,3)
C
C   Z = X CROSS Y
C
      Z(1)=X(2)*Y(3)-Y(2)*X(3)
      Z(2)=X(3)*Y(1)-Y(3)*X(1)
      Z(3)=X(1)*Y(2)-Y(1)*X(2)
      A=SQRT(Z(1)**2+Z(2)**2+Z(3)**2)
      IF(A.EQ.0.) A=1.
      A=1./A
      DO 28 IU=1,3
  28  Z(IU)=Z(IU)*A
C
C   DO JET AXIS RECONSTRUCTION
C
 222  CALL JETREC(NJET,X,Y,Z)
      RETURN
      END
C
C
C
C  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C
      SUBROUTINE JETREC(NJET,X,Y,Z)
C
C
C
C     MAIN SUBROUTINE FOR RECONSTRUCTION OF JET AXES
C
C
C
C --------------------------------------------------------------
C
C
C  INPUT :              ------------------
C
C       CALLING ARGUMENTS :
C
C           -- NJET --     NUMBER OF JETS TO BE RECONSTRUCTED
C           -- NJET --       (MUST BE GE 2 AND LE 4)
C
C           -- X(3) --     NORMALIZED VECTOR POINTING IN THE
C           -- X(3) --     ESTIMATED DIRECTION OF THE FASTEST JET.
C
C           -- Y(3) --     NORMALIZED VECTOR IN THE DIRECTION OF THE
C           -- Y(3) --     EVENT PLANE NORMAL .
C
C           -- Z(3) --     NORMALIZED VECTOR IN THE EVENT PLANE
C           -- Z(3) --     PERPENDICULAR TO THE X VECTOR .
C
C
C
C
C   INTERNAL COMMON BLOCKS :
C
C   THE /CONF/ COMMON BLOCK CONTAINS THE CONFIGURATION NUMBER
C
C     COMMON /CONF/ ICONF
C
C   THE /OLD/ COMMON BLOCK REMEMBERS THE VALUES OF THE PAR ARRAY
C               FOR EACH CONFIGURATION BEFORE THEY ARE CHANGED BY
C               SUBROUTINE RJXX . THEY ARE THEN USED BY SUBROUTINE
C               MINUS TO GENERATE A NEW CONFIGURATION .
C
C     COMMON /OLD/ POLD(3,4)
C
C   THE /TRIPL/ COMMON BLOCK CONTAINS THE BEST VALUE OF THE GENERALIZED
C                TRIPLICITY OBTAINED SO FAR FOR THIS EVENT .
C
C     COMMON /TRIPL/ TOLD,TR(4)
C
C   THE /LJET/ COMMON BLOCK CONTAINS : MJET -- NUMBER OF JETS TO
C                                           --  BE RECONSTRUCTED
C                                    IKONF -- NUMBER OF CONFIGURATIONS
C                                             (2 FOR 3-JETS ,
C                                                 10 FOR 4-JETS)
C
C     COMMON /LJET/ MJET,IKONF
C
C   THE /PMAG/ COMMON CONTAINS THE MODULUS OF THE VECTORS IN THE
C                 /SENSE/ COMMON
C
C     COMMON /PMAG/ PMOD(100)
C
C   THE /CYYY/ COMMON CONTAINS THE EVENT PLANE NORMAL AS SUPPLIED BY
C                THE USER
C
C     COMMON /CYYY/ YY(3)
CAV Require same size
      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF,POLD(3,4),TOLD,
     $ TR(4),MJET,IKONF,PMOD(100),YY(3),AXIS(3),ATRST,
     $ IJ(4),PTH(3,100,4),IPJ(100,4),INDX(100,4),THJ,THRUJ(3),
     $ TEMP(3,4),TPTH(3,100,4),ITPJ(100,4),ITJ(4),
     $ ANG(4),COMP(4),TREM(4),T(3)
C
C
C ---------------------------------------------------------------------
C
C
C     COMMON /SENSE/ PP(4,100),INUM
C
C
C     COMMON /MARKET/ PAR(3,4)
C
C
C     COMMON /CRUST/ AXIS(3),ATRST
C
C
C     COMMON /COLD/ IJ(4),PTH(3,100,4),IPJ(100,4)
C
      DIMENSION Y(3),Z(3),X(3)
C
      DATA ROOT2/0.707107/
C
C
C
C
C
C
      MJET=NJET
      DO 87 IU=1,4
  87  IJ(IU)=0
C
C  IF IJ(L) IS STILL ZERO FOR L LE MJET THEN ONE JET HAS ZERO PARTICLES
C    ASSOCIATED WITH IT AND THIS EVENT SHOULD PROBABLY BE REJECTED
C    BY THE USER .
C
      DO 42 IU=1,3
  42  YY(IU)=Y(IU)
      IF(MJET.GE.2.AND.MJET.LE.4) GO TO 15
      WRITE(6,876) MJET
 876  FORMAT('0 **** RECONSTRUCTION OF',I5,'  JETS NOT POSSIBLE ******')
      RETURN
  15  CONTINUE
      ITRK=INUM
      IF(ITRK.LE.100) GO TO 16
      ITRK=100
      WRITE(6,908) INUM
  908 FORMAT('0 **** WARNING *****',I7,'   TRACKS INPUT AND THIS IS FAR
     +TOO MANY , SET TO 100')
  16  CONTINUE
      IF(ITRK.GE.MJET) GO TO 17
      WRITE(6,907) ITRK,MJET
 907  FORMAT('0 ***** ONLY',I4,'  TRACKS INPUT AND THIS IS FAR TOO FEW T
     +O RECONSTRUCT',I5,' JETS')
      RETURN
  17  CONTINUE
      IF(MJET.NE.2) GO TO 290
      DO 485 I=1,ITRK
 485  PMOD(I)=SQRT(PP(1,I)**2+PP(2,I)**2+PP(3,I)**2)
 290  CONTINUE
C  INITIALIZE CONFIGURATION NUMBER
      ICONF=0
C  INITIALIZE GENERALIZED TRIPLICITY
      TOLD=0.
      IF(MJET.EQ.3) GO TO 3
      IF(MJET.EQ.2) GO TO 2
C
C
C   -----------    FOUR JET RECONSTRUCTION  -------------------------
C
C  INITIALIZE THE PAR COMMON WITH EACH OF THE 10 CONFIGURATIONS
C      AND THEN CALL RJXX FOR EACH
C
C  SET NUMBER OF CONFIGURATIONS
      IKONF=10
C     CONFIGURATION 1
      DO 45 IU=1,3
      PAR(IU,1)=(X(IU)+Z(IU))*ROOT2
      PAR(IU,2)=(X(IU)-Z(IU))*ROOT2
      PAR(IU,3)=-PAR(IU,1)
      PAR(IU,4)=-PAR(IU,2)
 45   CONTINUE
      CALL RJXX
C   CONFIGURATION 2
      DO 645 IU=1,3
      PAR(IU,1)=POLD(IU,1)
      PAR(IU,2)=POLD(IU,2)
      PAR(IU,3)=(-X(IU)+Y(IU))*ROOT2
      PAR(IU,4)=(-X(IU)-Y(IU))*ROOT2
  645 CONTINUE
      CALL RJXX
C  CONFIGURATION 3
      CALL MINUS
      CALL RJXX
C  CONFIGURATION 4
      DO 675 IU=1,3
      PAR(IU,1)=POLD(IU,1)
      PAR(IU,2)=-POLD(IU,2)
      PAR(IU,3)=(-Z(IU)+Y(IU))*ROOT2
      PAR(IU,4)=(-Z(IU)-Y(IU))*ROOT2
  675 CONTINUE
      CALL RJXX
C  CONFIGURATION 5
      CALL MINUS
      CALL RJXX
C  CONFIGURATION 6
      DO 890 IU=1,3
      PAR(IU,1)=Z(IU)
      PAR(IU,2)=X(IU)
      PAR(IU,3)=-POLD(IU,1)
      PAR(IU,4)=-POLD(IU,2)
 890  CONTINUE
      CALL RJXX
C  CONFIGURATION 7
      DO 276 IU=1,3
      PAR(IU,1)=POLD(IU,1)
      PAR(IU,2)=POLD(IU,2)
      PAR(IU,3)=-(Z(IU)+X(IU))/2.+Y(IU)*ROOT2
      PAR(IU,4)=-(Z(IU)+X(IU))/2.-Y(IU)*ROOT2
 276  CONTINUE
      CALL RJXX
C  CONFIGURATION 8
      CALL MINUS
      CALL RJXX
C  CONFIGURATION 9
      DO 356 IU=1,3
      PAR(IU,1)=-POLD(IU,2)
      PAR(IU,2)=POLD(IU,1)
      PAR(IU,3)=(-Z(IU)+X(IU))/2.+Y(IU)*ROOT2
      PAR(IU,4)=(-Z(IU)+X(IU))/2.-Y(IU)*ROOT2
 356  CONTINUE
      CALL RJXX
C  CONFIGURATION 10
      CALL MINUS
      CALL RJXX
      RETURN
C
C
C    ----------------   THREE JET RECONSTRUCTION   ----------------
C
C  INITIALIZE THE PAR COMMON WITH EACH OF THE 2 CONFIGURATIONS
C      AND THEN CALL RJXX FOR EACH
C
  3   CONTINUE
C  SET NUMBER OF CONFIGURATIONS
      IKONF=2
C     CONFIGURATION 1
      DO 845 IU=1,3
      PAR(IU,1)=X(IU)
      PAR(IU,2)=(-X(IU)-Z(IU))*ROOT2
      PAR(IU,3)=(-X(IU)+Z(IU))*ROOT2
845   CONTINUE
      CALL RJXX
C   CONFIGURATION 2
      CALL MINUS
      CALL RJXX
      RETURN
C
C  ---------------------- TWO JET RECONSTRUCTION --------------------
C
C             SET PAR EQAL TO THE BIGGEST VECTOR IN /SENSE/
C                     AND CALL RJXX
C
  2   CONTINUE
      IMAX=1
      PMAX=PMOD(1)
      DO 621 ITR=2,ITRK
      IF(PMOD(ITR).LT.PMAX) GO TO 621
      IMAX=ITR
      PMAX=PMOD(ITR)
 621  CONTINUE
      DO 892 IU=1,3
      PAR(IU,1)=PP(IU,IMAX)
      PAR(IU,2)=-PP(IU,IMAX)
      PAR(IU,3)=0.
892   CONTINUE
      CALL RJXX
C  DETERMINE THRUST AXIS
      IL=IJ(1)
      DO 434 IK=1,IL
      DO 821 IU=1,3
821   PAR(IU,3)=PTH(IU,IK,1)+PAR(IU,3)
 434  CONTINUE
      IL=IJ(2)
      DO 534 IK=1,IL
      DO 521 IU=1,3
521   PAR(IU,3)=-PTH(IU,IK,2)+PAR(IU,3)
 534  CONTINUE
      PNORM=SQRT(PAR(1,3)**2+PAR(2,3)**2+PAR(3,3)**2)
      IF(PNORM.EQ.0.) GO TO 643
      PNORM=1./PNORM
      DO 759 IU=1,3
      PAR(IU,3)=PAR(IU,3)*PNORM
 759  AXIS(IU)=PAR(IU,3)
 644  CONTINUE
C  DETERMINE THRUST
      PSUM=0.
      PCOMP=0.
      DO 754 IU=1,ITRK
      PSUM=PSUM+PMOD(IU)
      PTEMP=PAR(1,3)*PP(1,IU)+PAR(2,3)*PP(2,IU)+PAR(3,3)*PP(3,IU)
      PCOMP=PCOMP+ABS(PTEMP)
754   CONTINUE
      PTH(1,1,3)=PCOMP/PSUM
      ATRST=PCOMP/PSUM
      RETURN
 643  CONTINUE
      DO 655 IU=1,3
      PAR(IU,3)=PAR(IU,1)
 655  AXIS(IU)=PAR(IU,1)
      GO TO 644
      END
C
C
C
C   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C
      SUBROUTINE RJXX
C
C  ---------------------------------------------------------------
C
C       THIS SUBROUTINE IS CALLED BY MCGJET FOR EACH CONFIGURATION.
C       THE INPUT CONFIGURATION IS USED AS A STARTING VALUE FOR
C       THE JET AXES . BETTER VALUES ARE THEN DETERMINED BY ITERATION .
C        THE INITIAL JET AXES FOR EACH CONFIGURATION ARE STORED
C        IN THE ARRAY POLD AND THE BEST JET AXES SO FAR
C        FOR THIS EVENT ARE IN THE ARRAYS TEMP,TPTH,ITPJ,ITJ .
C        THE VALUE FOR THE TRIPLICITY IS STORED IN THE COMMON /TRIPL/ .
C
C  ----------------------------------------------------------------
C
C     COMMON /SENSE/ PP(4,100),INUM
C     COMMON /MARKET/ PAR(3,4)
C     COMMON /COLD/ IJ(4),PTH(3,100,4),IPJ(100,4)
C
C     COMMON /CONF/ ICONF
C     COMMON /TRIPL/ TOLD,TR(4)
C     COMMON /OLD/ POLD(3,4)
C     COMMON /LJET/ MJET,IKONF
C
C
C  THE /IND/ COMMON IS AN INDEX SET UP JUST BEFORE THE CALL TO SUMTHR .
C    INDX(K,J) IS THE TRACK NUMBER IN THE /SENSE/ COMMON FOR
C    TRACK K IN JET J OF THE /COLD/ COMMON . THIS INDEX ALLOWS
C    THE MODULUS OF EACH TRACK IN THE /SENSE/ COMMON TO BE
C    COMPUTED ONLY ONCE (IN THE MAIN ROUTINE MCGJET) AND STORED
C    IN THE COMMON /PMAG/ FOR REPEATED USE IN THE SUBROUTINE SUMTHR .
C
C     COMMON /IND/ INDX(100,4)
C
C  THE /CJRUST/ COMMON CONTAINS THE NORMALIZED JET DIRECTION IN THE
C   ARRAY THRUJ AND THE VALUE OF THE GENERALIZED TRIPLICITY FOR
C   THIS ONE JET AS COMPUTED IN THE SUBROUTINE SUMTHR .
C
C     COMMON /CJRUST/ THJ,THRUJ(3)
CAV
      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF,POLD(3,4),TOLD,
     $ TR(4),MJET,IKONF,PMOD(100),YY(3),AXIS(3),ATRST,
     $ IJ(4),PTH(3,100,4),IPJ(100,4),INDX(100,4),THJ,THRUJ(3),
     $ TEMP(3,4),TPTH(3,100,4),ITPJ(100,4),ITJ(4),
     $ ANG(4),COMP(4),TREM(4),T(3)
C
      DIMENSION IPP(4,100)
      EQUIVALENCE(PP(1,1),IPP(1,1))
C
C  MAXIMUM NUMBER OF ITERATIONS IS ITMAX
C
      DATA ITMAX/9/
C
C
      ICONF=ICONF+1
C     WRITE(6,909) ICONF
C909  FORMAT('0CONFIGURATION NO ',I10)
C
C  REMEMBER THIS CONFIGURATION
C
      DO 643 IU=1,3
      DO 644 IY=1,4
      POLD(IU,IY)=PAR(IU,IY)
  644 CONTINUE
 643  CONTINUE
      IKOUNT=0
 777  CONTINUE
      IKOUNT=IKOUNT+1
      IIT=0
C
C -------  ITERATION LOOP --------------------------------------
C
 888  CONTINUE
C  IIT IS ITERATION COUNTER
      IIT=IIT+1
      DO 387 IU=1,4
      IJ(IU)=0
 387  CONTINUE
C
C  ---  LOOP OVER TRACKS -----------------------------------------
C
      DO 621 ITR=1,INUM
      PX=PP(1,ITR)
      PY=PP(2,ITR)
      PZ=PP(3,ITR)
C  DETERMINE TO WHICH CURRENT JET AXIS (AS GIVEN IN THE PAR ARRAY)
C   THIS TRACK IS CLOSEST
      DO 287 IU=1,MJET
 287  COMP(IU)=PX*PAR(1,IU)+PY*PAR(2,IU)+PZ*PAR(3,IU)
      JET=1
      AM=COMP(1)
      DO 288 IU=2,MJET
      AM=AMAX1(AM,COMP(IU))
 288  IF(AM.EQ.COMP(IU)) JET=IU
C
C  THIS TRACK IS CLOSEST TO JET NO 'JET'
C
      IF(IJ(JET).GE.100) GO TO 621
C  ASSOCIATE THIS TRACK WITH JET 'JET'
      IJ(JET)=IJ(JET)+1
      IPT=IJ(JET)
      IPJ(IPT,JET)=IPP(4,ITR)
      PTH(1,IPT,JET)=PX
      PTH(2,IPT,JET)=PY
      PTH(3,IPT,JET)=PZ
C  SET INDX ARRAY FOR FASTER COMPUTATION IN SUBROUTINE SUMTHR
      INDX(IPT,JET)=ITR
 621  CONTINUE
C
C  ------  DETERMINE NEW JET DIRECTIONS  ------------------------
C
      TTOT=0.
      DO 478 KU=1,MJET
      JCNT=KU
      CALL SUMTHR(JCNT)
C  ANGLE BETWEEN JET AXIS FROM THIS AND LAST ITERATION
      ANG(KU)=THRUJ(1)*PAR(1,KU)+THRUJ(2)*PAR(2,KU)+THRUJ(3)*PAR(3,KU)
C  STORE NEW JET DIRECTIONS
      DO 188 JL=1,3
      PAR(JL,KU)=THRUJ(JL)
 188   CONTINUE
C  TTOT IS GENERALIZED TRIPLICITY FOR THIS ITERATION
      TR(KU)=THJ
      TTOT=TTOT+THJ
 478  CONTINUE
C  IF CHANGE IN JET DIRECTIONS IS LESS THAN 1 DEGREE OR MAX NUMBER OF
C      OF ITERATIONS HAVE BEEN REACHED , WE WILL FINISH .
      DO 186 IU=1,MJET
 186  IF(ANG(IU).LT.0.99985) GO TO 388
      GO TO 1000
 388  CONTINUE
      IF(IIT.GE.ITMAX) GO TO 1000
C  ANOTHER ITERATION IS NECESSARY
      GO TO 888
C
C  ---------  END OF ITERATION  -------------------------------------
C
1000  CONTINUE
C
C
C     WRITE(6,984)((PAR(IK,IB),IK=1,3),IB=1,4)
C984   FORMAT('0 DIRS AFTER THIS CONF ',4(3F7.4,5X))
C     WRITE(6,898) IJ
C898  FORMAT('  IJ  ',4I10)
C     WRITE(6,245) TTOT
C245  FORMAT(' *****  TTOT = ',F10.5)
C
C  THIS CONF NOT AS GOOD AS ONE BEFORE , GO TO 911
      IF(TTOT.LE.TOLD) GO TO 911
C  THESE ARE THE BEST JET AXES SO FAR , REMEMBER THEM .
      TOLD=TTOT
C  IF TWO JET EVENT RETURN IMMEDIATELY SINCE
C   ONLY ONE CONFIGURATION TO BE TRIED . BEST AXES ARE
C   ALREADY STORED AWAY .
      IF(MJET.EQ.2) RETURN
      DO 87 IE=1,MJET
      IL=IJ(IE)
      IF(IL.EQ.0) GO TO 911
      TREM(IE)=TR(IE)
      DO 89 I=1,3
      TEMP(I,IE)=PAR(I,IE)
 89   CONTINUE
      ITJ(IE)=IJ(IE)
      DO 14 K=1,IL
      ITPJ(K,IE)=IPJ(K,IE)
      DO 14 I=1,3
      TPTH(I,K,IE)=PTH(I,K,IE)
  14  CONTINUE
 87   CONTINUE
  911 CONTINUE
      IF(MJET.EQ.4) GO TO 444
      IF(IKOUNT.GT.3) GO TO 444
      IF(IKOUNT.EQ.1) CALL RESTAR(1,2,3)
      IF(IKOUNT.EQ.2) CALL RESTAR(1,3,2)
      IF(IKOUNT.EQ.3) CALL RESTAR(2,3,1)
      GO TO 777
  444 CONTINUE
C
C  FINISH
C
C  IF MORE CONFIGURATIONS TO COME THEN JUST RETURN
C    BEST PARAMETERS SO FAR WILL BE STORED IN ITJ,ITPJ,TPTH,TEMP
      IF(ICONF.LT.IKONF) RETURN
C  WE HAVE THE LAST CONFIGURATION , SO FILL COMMONS WITH BEST VALUES
      DO 634 IE=1,MJET
      TR(IE)=TREM(IE)
      IL=ITJ(IE)
      IJ(IE)=ITJ(IE)
      DO 164 K=1,IL
      IPJ(K,IE)=ITPJ(K,IE)
      DO 164 I=1,3
      PTH(I,K,IE)=TPTH(I,K,IE)
 164  CONTINUE
      DO 321 I=1,3
      PAR(I,IE)=TEMP(I,IE)
 321  CONTINUE
 634  CONTINUE
C     WRITE(6,721)((PAR(IK,IB),IK=1,3),IB=1,4)
C721  FORMAT('0FINAL AXES ',4(3F7.4,5X))
C     WRITE(6,267) IJ
C267  FORMAT('   IJ    ',4I10)
C     WRITE(6,794) TOLD
C794  FORMAT(' FINAL TTOT  ',F10.5)
      RETURN
      END
C
C
C
C   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C
      SUBROUTINE MINUS
C
C  ---------------------------------------------------------------
C
C     THIS SUBROUTINE MAKES A NEW CONFIGURATION BY SIMPLY
C     REVERSING THE AXES OF THE PREVIOUS CONFIGURATION
C
C  ----------------------------------------------------------------
C
CAV require same size
      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF,POLD(3,4),TOLD,
     $ TR(4),MJET,IKONF,PMOD(100),YY(3),AXIS(3),ATRST,
     $ IJ(4),PTH(3,100,4),IPJ(100,4),INDX(100,4),THJ,THRUJ(3),
     $ TEMP(3,4),TPTH(3,100,4),ITPJ(100,4),ITJ(4),
     $ ANG(4),COMP(4),TREM(4),T(3)
C     COMMON /OLD/ POLD(3,4)
C     COMMON /MARKET/ PAR(3,4)
C
      DO 187 IE=1,4
      DO 87 IU=1,3
      PAR(IU,IE)=-POLD(IU,IE)
 87   CONTINUE
 187  CONTINUE
      RETURN
      END
C
C
C
C   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C
      SUBROUTINE SUMTHR(JIN)
C
C
C
C  --------------------------------------------------------------
C
C    THIS SUBROUTINE CALCULATES NEW JET DIRECTIONS FOR JET "JIN" .
C           IT USES THE MOMENTA FOR JET JIN STORED IN THE COMMON
C          /COLD/ . ALSO CALCULATES VALUE FOR THE GENERALIZED
C          TRIPLICITY . THE NORMALIZED JET DIRECTION FOR JET JIN
C          AND THE CONTRIBUTION TO THE GENERALIZED TRIPLICITY
C          FROM THIS JET ARE STORED IN THE /CJRUST/ COMMON .
C
C ---------------------------------------------------------------
C
      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF,POLD(3,4),TOLD,
     $ TR(4),MJET,IKONF,PMOD(100),YY(3),AXIS(3),ATRST,
     $ IJ(4),PTH(3,100,4),IPJ(100,4),INDX(100,4),THJ,THRUJ(3),
     $ TEMP(3,4),TPTH(3,100,4),ITPJ(100,4),ITJ(4),
     $ ANG(4),COMP(4),TREM(4),T(3)
C     COMMON /CJRUST/ THJ,THRUJ(3)
C     COMMON /MARKET/ PAR(3,4)
C     COMMON /COLD/ IJ(4),PTH(3,100,4),IPJ(100,4)
C     COMMON /PMAG/ PMOD(100)
C     COMMON /IND/ INDX(100,4)
C     COMMON /CWT/ JWT
C
C     DIMENSION T(3)
C
      THJ=0.
      NTRK=IJ(JIN)
      IF(NTRK.LE.0) RETURN
C
C   BRANCH FOR WEIGHTING OF JET AXIS
C
      IF(JWT.NE.0) GO TO 543
C
      DO 101 K=1,3
      T(K)=0.
      DO 100 I=1,NTRK
      T(K)=T(K)+PTH(K,I,JIN)
 100  CONTINUE
 101  CONTINUE
 544  CONTINUE
      TMOD=SQRT(T(1)**2+T(2)**2+T(3)**2)
      RTMOD=1./TMOD
      DO 201 K=1,3
 201  THRUJ(K)=T(K)*RTMOD
C     PSUM=0.
C     DO 102 K=1,NTRK
C     I=INDX(K,JIN)
C     PSUM=PSUM+PMOD(I)
C102  CONTINUE
C     THJ=TMOD/PSUM
      THJ=TMOD
      RETURN
C
C   WEIGHTING
C
 543  CONTINUE
      DO 501 K=1,3
      T(K)=0.
      DO 500 I=1,NTRK
      J=INDX(I,JIN)
      T(K)=T(K)+PTH(K,I,JIN)*PMOD(J)
 500  CONTINUE
 501  CONTINUE
      GO TO 544
      END
C
C
C
C
C  $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
C
C
      SUBROUTINE RESTAR(I,J,K)
C
C
C
C ------------------------------------------------------------------
C
C     THIS SUBROUTINE GENERATES A NEW CONFIGURATION BY FIXING
C         RECONSTRUCTED AXIS K
C
C ------------------------------------------------------------------
C
CAV require same size
      COMMON /CWORK/ PAR(3,4),PP(4,100),INUM,JWT,ICONF,POLD(3,4),TOLD,
     $ TR(4),MJET,IKONF,PMOD(100),YY(3),AXIS(3),ATRST,
     $ IJ(4),PTH(3,100,4),IPJ(100,4),INDX(100,4),THJ,THRUJ(3),
     $ FOOTEMP(3,4),TPTH(3,100,4),ITPJ(100,4),ITJ(4),
     $ ANG(4),COMP(4),TREM(4),T(3)
C     COMMON /OLD/ POLD(3,4)
C     COMMON /MARKET/ PAR(3,4)
C     COMMON /CYYY/ YY(3)
C
      DIMENSION TEMP(3)
C
C   TEMP IS (REC JET AXIS K) CROSS (EVENT PLANE NORMAL)
C
      TEMP(1)=PAR(2,K)*YY(3)-PAR(3,K)*YY(2)
      TEMP(2)=PAR(3,K)*YY(1)-PAR(1,K)*YY(3)
      TEMP(3)=PAR(1,K)*YY(2)-PAR(2,K)*YY(1)
      A=SQRT(TEMP(1)**2+TEMP(2)**2+TEMP(3)**2)
      IF(A.EQ.0.) A=1.0
      A=1./A
      DO 26 IU=1,3
      TEMP(IU)=TEMP(IU)*A
C
C   GENERATE NEW CONFIGURATION
C
      PAR(IU,I)=(TEMP(IU)-PAR(IU,K))*0.707107
      PAR(IU,J)=(-TEMP(IU)-PAR(IU,K))*0.707107
  26  CONTINUE
      RETURN
      END
C
C
C   $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C
      BLOCK DATA BLCKG4 !PMF 03/12/99: add name
C
C   SET DEFAULT VALUE OF IWT
C
      COMMON /WT/ IWT
C
      DATA IWT /0/
C
      END
