CDECK  ID>, CKERN.
      SUBROUTINE CKERN(ITKDM,NT,PT,NJREQ,IERR)
*.-----------------------------------------------------------------------
*.
*.    CKERN: Resolves the clustering, and stores information
*.           in common block CKCOM. 
*.           INPUT: ITKDM      (integer) First dimension of PT array 
*.                                 (>=4 required)
*.                  NT         (integer) Number of tracks
*.                  PT(ITKDM,*)(real)    Four-momenta of tracks
*.                  NJREQ      (integer) Clustering limit. Note that the CPU
*.                                       consumption is roughly proportional to
*.                                       calling JADE/Durham algorithm NJREQ 
*.                                       times!! 
*.                                       1) If interested in ALL possible N-jet
*.                                          configurations, set NJREQ equal to
*.                                          NT. Note the remark on CPU time!
*.                                       2) If interested in first N-jet 
*.                                          configuration (with largest 
*.                                          ycut values), set NJREQ equal 
*.                                          to number of jets N-jet.
*.           OUTPUT: IERR      (integer) Error flag, 0=OK.
*.           CALLS:  CKSORD
*.
*.
*.    Once CKERN is called, all information on the event clustering, 
*.    final state jets and particle association can be accessed 
*.    without additional CPU time using the utility routines.
*.
*.---CAMBRIDGE JET CLUSTERING ALGORITHM
*.   BASED ON YCLUS BY S BETHKE
*.   REF: YU L DOKSHITZER, G D LEDER, S MORETTI, B R WEBBER
*.   CAVENDISH-HEP-97/06 (JUNE 1997)
*.   07/07/97 FIRST RELEASED BY BRW
*.   23/08/97 COMMENTS REVISED BY BRW
*.   23/09/97 IMPLEMENT FOR PX LIBRARY BY STK AND SB
*.
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:  11-02-1998, Z. Troscyani, Add factor epsilon
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER ITKDM,NT,NJ,NJREQ,IERR
      REAL PT(ITKDM,*)
      INTEGER NTRK,NV
      PARAMETER( NTRK=300, NV=NTRK*(NTRK-2)+NTRK-(NTRK-2)*(NTRK-1)/2 )
      LOGICAL IP(NTRK),LCALL
      INTEGER I,II,J,K,L,IMINI,JMINI,IAD,IJ(NTRK),JJ(NTRK)
      DOUBLE PRECISION PP(5),PL(5,NTRK),V(NV),PM,VMINI,YSCA,EVIS
C
      DOUBLE PRECISION EPSILON
      DOUBLE PRECISION YCUR(NTRK)
      INTEGER          ICUR, KIX(NTRK)
C      

      SAVE LCALL
      DATA LCALL / .FALSE. /
      DATA EPSILON / 1D-12 /
      SAVE EPSILON


      print *,'njreq',njreq
C  Welcome message:
      IF( .NOT.LCALL ) THEN
c        PRINT *, ' '
c        PRINT *, 'Cambridge jet finding algorithm, please refer to:'
c        PRINT *, 'Yu.L. Dokshitzer, G.D. Leder, S. Moretti, B.R. Webber'
c        PRINT *, 'CAVENDISH-HEP-97/06 (June 1997)'
        PRINT *, ' '
        PRINT *, ' #################################################'
        PRINT *, ' ###     C K E R N    V E R S I O N    104     ###'
c        PRINT *, ' ###  S. Bentvelsen and I. Meyer, EPXXX, 1998  ###'
        PRINT *, ' #################################################'
        WRITE(*,'(A,I2,A)') 
     +      '  ##### CAMBRIDGE JET CLUSTERING 1 - ',NJREQ,' JETS  #####'
        PRINT *, ' #################################################'
        LCALL= .TRUE.
      ENDIF
C---WARNINGS
      IERR = 0
      EVIS = 0D0
      ICUR = 0
      NTRACK = NT
      NJREQ = MIN(NJREQ,NT)
      IF( NT.GT.NTRK ) THEN
        WRITE(*,'(''CAMJET: More than '',I3,'' input particles: '',I5)')
     &        NTRK,NT
        IERR= 1
        RETURN
      ELSEIF( NT.LT.2 ) THEN
        WRITE(*,'(''CAMJET: Less than 2 input particles: '',I5)') NT
        IERR= 2
        RETURN
      ENDIF
C
C..   RESET ALL VALUES
C
      NJITER = 0
      DO I=1,NMXY
         YTRANS(I) = 0D0
         NTRANS(I) = 0
         DO J=1,NMXP
            ICMJ(J,I)   = 0
         ENDDO
      ENDDO

      DO I=1,NT
         EVIS = EVIS + DBLE(PT(4,I))
      ENDDO
      YSCA   = EVIS*EVIS
C---COPY MOMENTA INTO PL-ARRAY
 2    DO I=1,NT
         YCUR(I) = 0D0
         IP(I)=.TRUE.
         IJ(I)=I
         DO II=1,4
            PL(II,I)= DBLE(PT(II,I))
         ENDDO
         PM=PL(1,I)**2+PL(2,I)**2+PL(3,I)**2
         IF (PM.GT.0D0) THEN
            PL(5,I)=1D0/SQRT(PM)
         ELSE
            PL(5,I)=1D0
         ENDIF
      ENDDO
C---  FILL V-ARRAY: V(I,J) IS V(NT*(I-1)+J-I(I+1)/2)
      IAD = 0
      DO I=1,NT-1
         DO II=1,5
            PP(II)=PL(II,I)
         ENDDO
         DO J=I+1,NT
            IAD = IAD + 1
            V(IAD) = 2D0*(1D0-(PP(1)*PL(1,J) +PP(2)*PL(2,J)
     &               +PP(3)*PL(3,J))*PP(5)*PL(5,J))
         ENDDO
      ENDDO
      NJ=NT
C---START MAIN LOOP.  FIRST LOOK FOR MINIMUM V
    1 VMINI = 1D10
      IMINI = 0
      IAD = 0
      DO I=1,NT-1
         IF (IP(I)) THEN
            DO J=I+1,NT
               IAD = IAD + 1
               IF (IP(J).AND.V(IAD).LT.VMINI) THEN
                  VMINI = V(IAD)
                  IMINI = I
                  JMINI = J
               ENDIF
            ENDDO
         ELSE
            IAD=IAD+NT-I
         ENDIF
      ENDDO
C---  END OF CLUSTER SEARCH FOR VMINI
      IF (IMINI.NE.0) THEN
C---  NOT FINISHED YET
         ICUR       = ICUR + 1
         YCUR(ICUR) = VMINI*MIN(PL(4,IMINI),PL(4,JMINI))**2
         IF (YCUR(ICUR)+EPSILON.GE.YSCA) THEN
C---  SOFT FREEZING HERE
            IF (PL(4,IMINI).LT.PL(4,JMINI)) THEN
               IP(IMINI)=.FALSE.
            ELSE
               IP(JMINI)=.FALSE.
            ENDIF
         ELSE
C---  COMBINE PARTICLES IMINI AND JMINI
            DO II=1,4
               PL(II,IMINI)=PL(II,IMINI)+PL(II,JMINI)
            ENDDO
            PM=PL(1,IMINI)**2+PL(2,IMINI)**2+PL(3,IMINI)**2
            IF (PM.GT.0D0) THEN
               PL(5,IMINI)=1D0/SQRT(PM)
            ELSE
               PL(5,IMINI)=1D0
            ENDIF
C---  FLAG PARTICLE JMINI AS COMBINED
            IP(JMINI)=.FALSE.
            IJ(JMINI)=IMINI
            NJ=NJ-1
C---  CALCULATE RELEVANT NEW V VALUES
            DO I=1,NT
               IF (I.NE.IMINI) THEN
                  IF (IJ(I).EQ.JMINI) IJ(I)=IMINI
                  IF (IP(I)) THEN
                     K = MIN(I,IMINI)
                     L = MAX(I,IMINI)
                     IAD = NT*(K-1) + L - (K*(K+1))/2
                     V(IAD) = 2D0*(1D0-(PL(1,K)*PL(1,L) +PL(2,K)*PL(2,L)
     &                    +PL(3,K)*PL(3,L))*PL(5,K)*PL(5,L))
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
C---  BACK TO START OF LOOP
         GO TO 1
      ELSE
C
C..   ITERATION IN JET-FINDING FINISHED
C
         NJITER = NJITER + 1
         IF(NJITER.LE.NMXY) THEN
C
C..   STORE THE VALUE OF YFLIP AND THE CORRESPONDING NUMBER OF JETS
C
            YTRANS(NJITER) = YSCA
            NTRANS(NJITER) = NJ

            IF(NJITER.LE.NMXP) THEN
C
C..   STORE THE JET-DIRECTIONS AND PARTICLE ASSOCIATION
C
               J=0
               DO I=1,NT
                  IF (IJ(I).EQ.I) THEN
                     J=J+1
                     JJ(I)=J
                     DO II=1,4
                        PCMJ(NJITER,II,J)= PL(II,I)
                     ENDDO
                  ENDIF
               ENDDO
               DO I=1,NT
                  ICMJ(NJITER,I)=JJ(IJ(I))
               ENDDO
            ENDIF
            IF(NJ.LE.NJREQ) THEN
C
C..   ANOTHER ITERATION?
C
               CALL CKSORD(NT,YCUR,KIX,'I')
               DO I=1,NT
C
C..   DETERMINE THE NEXT VALUE FOR YSCA, RESET VARIABLES
C
                  IF(YCUR(KIX(I)).LT.YSCA) THEN
                     YSCA = YCUR(KIX(I))
                     ICUR = 0
                     DO J=1,NT
                        YCUR(J) = 0D0
                     ENDDO
C
C..   YUP, GO FOR THE NEXT ROUND
C
                     IF(YSCA.NE.0) GOTO 2
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
      ENDIF
C
C..   DEVIDE BY ECM**2
C
      NJMAX = -1
      DO I=1,NJITER
         YTRANS(I) = YTRANS(I)/EVIS**2
         IF(NTRANS(I).GT.NJMAX) NJMAX = NTRANS(I)
      ENDDO

      END

CDECK  ID>, CKSORD.
      SUBROUTINE CKSORD (ISZ,ARY,KIX,COPT)
*.*********************************************************
*. ------
*. CKSORV: TAKEN FROM OPAL PX-LIBRARY
*. ------
*. SOURCE: HERWIG (B.Webber,G.Marchesini)
*. Sort a real array into assending order based on
*. the magnitude of its elements; provide an
*. integer "index array" which specifies the ordering
*. of the array.
*. Usage     :
*.
*.      PARAMETER  (NDIM=1.or.more)
*.      REAL  ARY (NDIM)
*.      INTEGER  KIX (NDIM)
*.      INTEGER  ISIZ
*.      CHARACTER*1  COPT
*.
*.      ISIZ = 1.to.NDIM
*.      COPT = 'I'
*.      CALL PXSORV (ISIZ,ARY,KIX,COPT)
*.
*. INPUT     : ISIZ  The dimension of the input array
*. INPUT     : ARY   The input array
*. OUTPUT    : KIX   The index array
*. CONTROL   : COPT  Control of output vector ARY
*.              = ' ' : return sorted ARY and index array KIX
*.              = 'I' : return index array KIX only, don't
*.                      modify ARY
*.
*.mod: S. BENTVELSEN OCT-97
*.     CHANGE CODE TO BE ABLE TO SORT DOUBLE PRECISION
*.     ARRAYS, AND ARRAY IS ORDERED IN DESENDING VALUES.
*.*********************************************************
      IMPLICIT NONE
      INTEGER  MXSZ
      PARAMETER  (MXSZ=500)
      INTEGER  ISZ,IX,JX
      INTEGER  KIX (*),IL (MXSZ),IR (MXSZ)
      double precision  ARY (*),BRY (MXSZ)
      CHARACTER*1  COPT

      do ix=1,isz
         ary(ix)=-ary(ix)
      enddo


      IF (ISZ.GT.MXSZ) THEN
          WRITE (6,FMT='('' PXSORT: Error,'',
     +           '' Max array size ='',I6)') MXSZ
          KIX (1) = -1
          GO TO 990
      END IF
      IL (1) = 0
      IR (1) = 0
      DO 10 IX = 2,ISZ
          IL (IX) = 0
          IR (IX) = 0
          JX = 1
   2      IF (ARY (IX).GT.ARY (JX)) GO TO 5
   3      IF (IL (JX).EQ.0) GO TO 4
          JX = IL (JX)
          GO TO 2
   4      IR (IX) = -JX
          IL (JX) =  IX
          GO TO 10
   5      IF (IR (JX).LE.0) GO TO 6
          JX = IR (JX)
          GO TO 2
   6      IR (IX) = IR (JX)
          IR (JX) = IX
  10  CONTINUE
      IX = 1
      JX = 1
      GO TO 8
  20  JX = IL (JX)
   8  IF (IL (JX).GT.0) GO TO 20
   9  KIX (IX) = JX
      BRY (IX) = ARY (JX)
      IX = IX + 1
      IF (IR (JX)) 12,30,13
  13  JX = IR (JX)
      GO TO 8
  12  JX = -IR (JX)
      GO TO 9
  30  continue
      do ix=1,isz
         ary(ix)=-ary(ix)
      enddo
      IF (COPT.EQ.'I') RETURN
      DO 31 IX = 1,ISZ
  31  ARY (IX) = BRY (IX)
 990  continue
      RETURN
      END


CDECK  ID>, CYJET.
      SUBROUTINE CYJET(NJET,YL,YH,IERR)
*.-----------------------------------------------------------------------
*.
*.    CYJET: Return the largest pair of y-flip values for a required n-jet 
*.           configuration. 
*.           INPUT:   NJET (integer) Number of jets required
*.           OUTPUT:  YL   (real   ) Lower boundary of y interval
*.                    YH   (real   ) Upper boundary of y interval
*.                    IERR (integer) Error flag, 0=OK.
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      REAL    YL,YH
      INTEGER NJET,I,IERR
      INTEGER NPRINT
      DATA    NPRINT / 0 /
      SAVE    NPRINT

      IERR = 0
      YL = -1.
      YH = -1.
      IF(NJET.GT.NJMAX) THEN
         WRITE(*,*) '#################################################'
         WRITE(*,*) '## CYJET: CAMBRIDGE JET FINDER RESOLVED FOR    ##'
         WRITE(*,'(A,I2,A)') ' ##    NUMBER OF JETS BETWEEN 1 AND '
     +        ,NJMAX,' ONLY     ##'
         WRITE(*,*) '#################################################'
         IERR = 2
         RETURN
      ENDIF
      IF(NJET.LT.1) THEN
         WRITE(*,*) 'ERROR IN NJET: ',NJET
         IERR = 3
         RETURN
      ENDIF

      DO I=1,NJITER
         IF(NTRANS(I).EQ.NJET) THEN
            YH = 1.*YTRANS(I)
            IF(I.LT.NJITER) YL = 1.*YTRANS(I+1)
            GOTO 889
         ENDIF
      ENDDO

      IF(NPRINT.LT.10) THEN
         NPRINT = NPRINT + 1
         WRITE(*,*) '#################################################'
         WRITE(*,*) '## CYJET: CAMBRIDGE JET FINDER CANNOT RESOLVE  ##'
         WRITE(*,'(A,I2,A)') ' ##         THIS CONFIGURATION TO A '
     +        ,NJET,' JET      ##'
         WRITE(*,*) '#################################################'
      ENDIF

 889  CONTINUE
      RETURN
      END
CDECK  ID>, CASSO.
      SUBROUTINE CASSO(NJET,PNJ,BL,IERR)
*.-----------------------------------------------------------------------
*.
*.    CASSO: Return the jet four-momenta and jet-particle association for a
*.           required number of jets.
*.           (corresponding to largest ycut values)
*.           INPUT:   NJET (integer) Number of jets required
*.           OUTPUT:  PNJ(4,*)(real) Array of jets 4-vectors
*.                    BL(*)(integer) Particle i belongs to jet BL(i) 
*.                    IERR (integer) Error flag, 0=OK.
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER NJET, IERR
      REAL PNJ(4,*)
      INTEGER I,J,II
      INTEGER BL(*)
      INTEGER NPRINT
      DATA    NPRINT / 0 /
      SAVE    NPRINT

      IERR = 0
      IF(NJET.GT.NJMAX.OR.NJET.LE.0) THEN
         WRITE(*,*) '#################################################'
         WRITE(*,*) '## CASSO: CAMBRIDGE JET FINDER RESOLVED FOR    ##'
         WRITE(*,'(A,I2,A)') ' ##    NUMBER OF JETS BETWEEN 1 AND '
     +        ,NJMAX,' ONLY     ##'
         WRITE(*,*) '#################################################'
         DO I=1,NJMAX
            DO II=1,4
               PNJ(II,I) = 0
            ENDDO
         ENDDO
         IERR = 2
         RETURN
      ENDIF
      DO I=1,NJITER
         IF(NTRANS(I).EQ.NJET) THEN
            DO J=1,NJET
               DO II=1,4
                  PNJ(II,J) = SNGL(PCMJ(I,II,J))
               ENDDO
            ENDDO
            DO J=1,NTRACK
               BL(J) = ICMJ(I,J)
            ENDDO
            GOTO 889
         ENDIF
      ENDDO

      IF(NPRINT.LT.10) THEN
         NPRINT = NPRINT + 1
         WRITE(*,*) '#################################################'
         WRITE(*,*) '## CASSO: CAMBRIDGE JET FINDER CANNOT RESOLVE  ##'
         WRITE(*,'(A,I2,A)') ' ##         THIS CONFIGURATION TO A '
     +        ,NJET,' JET      ##'
         WRITE(*,*) '#################################################'
      ENDIF

 889  CONTINUE
      RETURN
      END

CDECK  ID>, CAXES.
      SUBROUTINE CAXES(NJET,PNJ,IERR)
*.-----------------------------------------------------------------------
*.
*.    CAXES: Returns the jet four-momenta for a required n-jet configuration
*.           (corresponding to largest ycut values)
*.           INPUT:   NJET (integer) Number of jets required
*.           OUTPUT:  PNJ(4,*)(real) Array of jets 4-vectors 
*.                    IERR (integer) Error flag, 0=OK.
*.           CALLS:   CASSO
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      REAL PNJ(4,*)
      INTEGER ITMP(300)
      INTEGER IERR, NJET

      CALL CASSO(NJET,PNJ,ITMP,IERR)

      RETURN
      END

CDECK  ID>, CNJET.
      SUBROUTINE CNJET(YCUT,NJET,IERR)
*.-----------------------------------------------------------------------
*.
*.    CNJET: Returns number of jets for given value of ycut
*.           INPUT:   YCUT    (real) Normalised ycut value
*.           OUTPUT:  NJET (integer) Number of jets
*.                    IERR (integer) Error flag, 0=OK.
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER I,NJET,IERR
      REAL    YCUT
      INTEGER NPRINT
      DATA    NPRINT / 0 /
      SAVE    NPRINT
      
      IERR = 0
      DO I=1,NJITER
         IF(YCUT.GE.(1.*YTRANS(I))) THEN
            NJET = NTRANS(I)
            GOTO 889
         ENDIF
      ENDDO

      IF(NPRINT.LT.10) THEN
         NPRINT = NPRINT + 1
         WRITE(*,*) '#################################################'
         WRITE(*,*) '## CNJET: CAMBRIDGE JET FINDER CANNOT RESOLVE  ##'
         WRITE(*,'(A,I2,A)') ' ##         THIS CONFIGURATION TO A '
     +        ,NJET,' JET      ##'
         WRITE(*,*) '#################################################'
      ENDIF

 889  CONTINUE
      RETURN
      END

CDECK  ID>, CDBLE.
      SUBROUTINE CDBLE(IDUB,NJDUB,NJC)
*.-----------------------------------------------------------------------
*.
*.    CDBLE: Returns information about Njet configurations found at least
*.           two times.
*.           OUTPUT:  IDUB (integer) Number of Njet configurations for 
*.                                   which at least two Njet configurations 
*.                                   have been found
*.                    NJDUB(*)(integer) Array of length IDUB containing the 
*.                                   values for Njet
*.                    NJC(*)(integer) Array of length IDUB containing the
*.                                   number of different Njet regions
*.                                   NB: Entry NJDUB(i) correspond to NJC(i) 
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER IDUB,NJDUB(*),NJC(*)
      INTEGER NJOC(NMXY)
      INTEGER I

      DO I=1,NMXY
         NJOC(I) = 0
      ENDDO

      DO I=1,NJITER
         IF(NTRANS(I).GT.0.AND.NTRANS(I).LT.NMXY) THEN
            NJOC(NTRANS(I)) = NJOC(NTRANS(I)) + 1
         ENDIF
      ENDDO
      IDUB = 0
      DO I=1,NJITER
         IF(NJOC(I).GE.2) THEN
            IDUB = IDUB + 1
            NJDUB(IDUB) = I
            NJC(IDUB)   = NJOC(I)
         ENDIF
      ENDDO

      RETURN
      END
CDECK  ID>, CNONE.
      SUBROUTINE CNONE(INON,NJNON)
*.-----------------------------------------------------------------------
*.
*.    CNONE: Returns information about Njet configurations that could not
*.           be resolved.
*.           OUTPUT: INONE(integer) Number of Njet configurations that
*.                                  could not be resolved
*.                   NJNON(*)(integer) Array of length INON containing the
*.                                  Njet for which the configuration 
*.                                  could not be resolved.
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER INON,NJNON(*)
      INTEGER NJOC(NMXY)
      INTEGER I

      DO I=1,NMXY
         NJOC(I) = 0
      ENDDO

      DO I=1,NJITER
         IF(NTRANS(I).GT.0.AND.NTRANS(I).LT.NMXY) THEN
            NJOC(NTRANS(I)) = NJOC(NTRANS(I)) + 1
         ENDIF
      ENDDO
      INON = 0
      DO I=1,NJMAX
         IF(NJOC(I).EQ.0) THEN
            INON = INON + 1
            NJNON(INON) = I
         ENDIF
      ENDDO

      RETURN
      END
CDECK  ID>, CNCNT.
      INTEGER FUNCTION CNCNT(NJET)
*.-----------------------------------------------------------------------
*.
*.    CNCNT: (INTEGER FUNCTION) Returns the number of different Njet
*.           configurations found for given Njet.
*.           INPUT:  NJET(integer) Number of required jets
*.
*.
*.  CREATED :  08-01-1998, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER NJET, I

      CNCNT = 0
      DO I=1,NJITER
         IF(NTRANS(I).EQ.NJET) THEN
            CNCNT = CNCNT + 1
         ENDIF
      ENDDO
      
      RETURN
      END

CDECK  ID>, CNASS.
      SUBROUTINE CNASS(NJET,ICON,PNJ,BL,YL,YH,IERR)
*.-----------------------------------------------------------------------
*.
*.    CNASS: Like CASSO, but used to retrieve information about each
*.           Njet configuration.
*.           INPUT:   NJET (integer) Number of jets required
*.                    ICON (integer) Equals i for ith Njet configuration
*.                                   (max valid value = CNCNT(NJET))
*.           OUTPUT:  PNJ(4,*)(real) Array of jets 4-vectors
*.                    BL(*)(integer) Particle i belongs to jet BL(i) 
*.                    YL   (real   ) Lower boundary of y interval for ICONth
*.                                   Njet configuration
*.                    YH   (real   ) Upper boundary of y interval for ICONth
*.                                   Njet configuration
*.                    IERR (integer) Error flag, 0=OK.
*.
*.
*.  CREATED :  08-01-1998, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER NJET, ICON, IERR
      REAL PNJ(4,*),YL,YH
      INTEGER BL(*)
      INTEGER CNCNT, IN, I, II, J
      INTEGER NCH, ITMP
      DATA    NCH / 0 /
      SAVE NCH

      IN = CNCNT(NJET)
      IF(IN.EQ.0) THEN
         NCH = NCH + 1
         IF(NCH.LE.10) THEN
            WRITE(*,*) 'NOT RESOLVED FOR NUMBER OF JETS ',NJET
         ENDIF
         IERR = 1
         RETURN
      ELSEIF(ICON.GT.IN) THEN
         NCH = NCH + 1
         IF(NCH.LE.10) THEN
            WRITE(*,*) 'NJET HAS ONLY ',IN,' DISTINCT YCUT RANGES'
         ENDIF
         IERR = 2
         RETURN
      ENDIF
      
      ITMP = 0
      DO I=1,NJITER
         IF(NTRANS(I).EQ.NJET) THEN
            ITMP = ITMP + 1
            IF(ITMP.EQ.ICON) THEN
               YH = 1.*YTRANS(I)
               IF(I.LT.NJITER) YL = 1.*YTRANS(I+1)
               DO J=1,NJET
                  DO II=1,4
                     PNJ(II,J) = SNGL(PCMJ(I,II,J))
                  ENDDO
               ENDDO
               DO J=1,NTRACK
                  BL(J) = ICMJ(I,J)
               ENDDO
               GOTO 889
            ENDIF
         ENDIF
      ENDDO
      
 889  CONTINUE
      RETURN
      END

CDECK  ID>, CINFO.
      SUBROUTINE CINFO
*.----------------------------------------------------------------------
*.
*.    CINFO: Print all found yflip values to standard output
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER I, IDUB,NJDUB(20),NJOC(20),INON,NJNON(20)

      WRITE(*,'(/A)'    ) '*******************************************'
      WRITE(*,'( A)'    ) '* CINFO: Information Cambridge clustering *'
      WRITE(*,'(A,I3,A)') '*   Clustering resolved to ',NJMAX
     +     ,' Jets       *'
      WRITE(*,'(A,2(I3,A))') '* Iterations: ',NJITER
     +                    ,',  Number of tracks: ',NTRACK,' *'
      WRITE(*,'( A)'    ) '*******************************************'
      WRITE(*,'( A)'    ) '* Iteration   Ycut              Njet      *'
      DO I=1,NJITER
         WRITE(*,'(A,I4,G20.8,I8,A)') '* ',I,YTRANS(I),NTRANS(I)
     +        ,'        *'
      ENDDO
      WRITE(*,'( A)'    ) '*******************************************'
      CALL CDBLE(IDUB,NJDUB,NJOC)
      WRITE(*,'(A,I2,A)') '* Number of double regions for NJET: '
     +      ,IDUB,'   *'
      DO I=1,IDUB
         WRITE(*,'(A,3I9,A)') '* ',I,NJDUB(I),NJOC(I),'             *'
      ENDDO
      WRITE(*,'( A)'    ) '*******************************************'
      CALL CNONE(INON,NJNON)
      WRITE(*,'(A,I2,A)') '* Number of impossible NJET states : '
     +      ,INON,'   *'
      DO I=1,INON
         WRITE(*,'(A,2I9,A)') '* ',I,NJNON(I),'                      *'
      ENDDO
      WRITE(*,'(A/)'    ) '*******************************************'
      

      RETURN
      END

CDECK  ID>, CYGET.
      SUBROUTINE CYGET(NJI,YAR,NAR)
*.----------------------------------------------------------------------
*.    CYGET: Get information on all yflip values and corresponding Njet
*.           values
*.           OUTPUT:  NJI (integer) Number of found yflip values (upto
*.                                  first appearance of NJREQ jets)
*.                    YAR(*)(real)  Array containing all yflip values
*.                    NAR(*)(integer) Array containing all Njet values
*.
*.  CREATED :  11-12-1997, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER NJI,NAR(*),I
      REAL    YAR(*)

      NJI = NJITER
      DO I=1,NJITER
         YAR(I) = 1.*YTRANS(I)
         NAR(I) = NTRANS(I)
      ENDDO

      RETURN
      END
CDECK  ID>, CWRIT.
      SUBROUTINE CWRIT(ITKDIM,NT,PT,NDBLE,IERR)
C-----------------------------------------------------------------------
C
C     ROUTINE TO WRITE 4-VECTORS OF THE EVENT IN ASCII FILE 'scam.evdata'
C     AND YFLIP VALUES OF THE EVENT IN ASCII FILE 'scam.yflip'
C     WRITE ONLY WHEN NUMBER OF MULTIPLE REGIONS FOR NJET >= NDBLE
C
C  CREATED :  11-12-1997, STAN BENTVELSEN
C  LAST MOD:
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER ITKDIM, NT, NDBLE, IERR
      INTEGER I,J
      INTEGER IDUB,NJDUB(20),NJOC(20)
      REAL    PT(ITKDIM,*)
      LOGICAL FIRST
      DATA    FIRST / .TRUE. /
      SAVE    FIRST

      IF(FIRST) THEN
         FIRST = .FALSE.
         OPEN(20,FILE='scam.evdata',STATUS='UNKNOWN')
         OPEN(21,FILE='scam.ycut',STATUS='UNKNOWN')
      ENDIF

      CALL CDBLE(IDUB,NJDUB,NJOC)
      IF(IDUB.GE.NDBLE) THEN
         WRITE(20,'(I8)') NT
         DO I=1,NT
            WRITE(20,'(I8,4F12.5)') I,(PT(J,I),J=1,4)
         ENDDO
         WRITE(21,'(I8)') NJITER
         DO I=1,NJITER
            WRITE(21,'(I8,F13.10,I8)') I,YTRANS(I),NTRANS(I)
         ENDDO
      ENDIF

      RETURN
      END
CDECK  ID>, CREAD.
      SUBROUTINE CREAD(ITKDIM,NT,PT,IERR)
C-----------------------------------------------------------------------
C
C  CREATED :  11-12-1997, STAN BENTVELSEN
C  LAST MOD:
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  I,J,K
      INTEGER ITKDIM, NT, IERR
      REAL    PT(ITKDIM,*)

      LOGICAL FIRST
      DATA    FIRST / .TRUE. /
      SAVE    FIRST

      IF(FIRST) THEN
         FIRST = .FALSE.
         OPEN(20,FILE='scam.evdata',STATUS='UNKNOWN')
      ENDIF

      READ(20,'(I8)') NT
      DO I=1,NT
         READ(20,'(I8,4F12.5)') K,(PT(J,I),J=1,4)
      ENDDO

      RETURN
      END
CDECK  ID>, CMIGR.
      SUBROUTINE CMIGR(NJ,ORD,IERR)
C-----------------------------------------------------------------------
C
C     RETURN THE MIGRATION MATRIX OF TRACKS BETWEEN AN NJ JET
C     CONFIGURATION AND A NJ-1 CONFIGURATION.
C
C     INPUT : NJ         : NUMBER OF JETS
C     OUTPUT: MAT(10,10) : MIGRATION MATRIX, NJ X (NJ-1)
C             IERR       : ERROR FLAG
C
C  CREATED :  17-12-1997, STAN BENTVELSEN
C  LAST MOD:
C-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER NJ,IERR
      INTEGER IN(300),IM(300)
      INTEGER I,J,N,M
      REAL    PNJ(4,300), PMJ(4,300)
      INTEGER MAT(10,10), ORD(10,10), SUM(10)
      REAL    PUR(10,10)
      INTEGER KROW(10),KCOL(10)
      REAL*16 MROW(10),MCOL(10)


      IERR = 0
      DO I=1,10
         DO J=1,10
            MAT(I,J) = 0
         ENDDO
      ENDDO
      
      CALL CASSO(NJ  ,PNJ,IN,IERR)
      CALL CASSO(NJ-1,PMJ,IM,IERR)

      DO I=1,NTRACK
         N = IN(I)
         M = IM(I)
         IF(N.NE.0.AND.M.NE.0.AND.N.LE.10.AND.M.LE.10)
     +        MAT(N,M) = MAT(N,M) + 1
      ENDDO

      DO I=1,NJ-1
         SUM(I) = 0
      ENDDO

      DO I=1,NJ-1
         DO J=1,NJ
            SUM(I) = SUM(I) + MAT(J,I)
         ENDDO
      ENDDO

         
      DO I=1,NJ-1
         MCOL(I) = -9999.
         DO J=1,NJ
            PUR(J,I) = 1.*MAT(J,I) / SUM(I)
            IF(PUR(J,I).GT.MCOL(I)) MCOL(I) = PUR(J,I)
         ENDDO
      ENDDO

      DO J=1,NJ
         MROW(J) = -9999.
         DO I=1,NJ-1
            IF(PUR(J,I).GT.MROW(J)) MROW(J) = PUR(J,I)
         ENDDO
      ENDDO
      CALL PXSORV(NJ-1,MCOL,KCOL,' ')
      CALL PXSORV(NJ  ,MROW,KROW,' ')

      DO I=1,NJ-1
         DO J=1,NJ
            ORD(J,I) = MAT(KROW(NJ-J+1),KCOL(NJ-I))
         ENDDO
      ENDDO

      RETURN
      END
