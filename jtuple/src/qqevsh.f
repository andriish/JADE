CDECK  ID>, QQEVSH.
      SUBROUTINE QQEVSH( IDIM, NTRAK, PTRAK
     &          , T, TMA, TMI, MH, ML, BT, BW, CP, DP, S, A, AKP )
      IMPLICIT NONE
C  Routine computes standard LEP 1.5 and 2 event shapes
C  from array PTRAK
C  Input:   IDIM    1st dimension of array PTRAK
C           NTRAK   Number of entries in PTRAK
C           PTRAK() array of 5-momenta
C  Output:  T,TMA,TMI,MH,ML,BT,BW,CP,DP,S,A,AKP: event shapes
C  Author: Stefan Kluth
C  Date: 25.3.96
C  Modifications:
C  28.05.96, STK: Trap non-physical results for T, CP, S and A
C  18.10.96, STK: Remove +SEQ,QQNTUP and pass needed values as call arguments
C  21.04.97, STK: New calling sequence with new event shapes ML and AKP
C  22.04.97, STK: New length of array YD, new call argument NXJD, check
C                 error flags of PX subroutines
C  16.07.97, STK: New Durham algorithms
C  22.09.97, STK: Make array YE variable length
C  23.09.97, STK: Remove "new Durham algorithms", introduce Cambridge jets
C  29.09.97, STK: Check length of local arrays against number of particles
C  07.11.97, STK: Change minimum y_cut from 10^-4 to 10^-6 for E0 and D,
C                 replace CAMJET by PXCAMJ
C  01.12.97, SB : Introduce Yflip values for the Cambrige finder
C                 Remove fixed Y values for the Cambridge finder
C  26.06.02, JRS: include 4 jet event variables 
C  08.10.02, STK: move jet calculations to new routines
      INTEGER IDIM, NTRAK
      REAL PTRAK(IDIM,*)
      REAL T,TMA,TMI,MH,ML,BT,BW,CP,DP,S,A,AKP
      REAL TVAL(3),TVEC(3,3),EVAL(3),EVEC(3,3),AVEC(3)
      INTEGER IERR
C
      IF( NTRAK.GT.2 ) THEN
C       Event shapes:
        CALL PXLTH4( NTRAK, IDIM, PTRAK, TVAL, TVEC, IERR )
        IF( IERR.NE.0 ) THEN
          PRINT *, 'QQEVSH: PXLTH4 error', IERR
          T= -1.0
          TMA= -1.0
          TMI= -1.0
          MH= -1.0
          ML= -1.0
          BT= -1.0
          BW= -1.0
        ELSE
          T= 1.0-TVAL(3)
          IF( T.LT.0.0 ) T= 0.0
          TMA= TVAL(2)
          TMI= TVAL(1)
          CALL PXMMBB( NTRAK, IDIM, PTRAK, TVEC(1,3), 
     &         MH, ML, BT, BW, IERR)
          IF( IERR.NE.0 ) THEN
            PRINT *, 'QQEVSH: PXMMBB error', IERR
            MH= -1.0
            ML= -1.0
            BT= -1.0
            BW= -1.0
          ENDIF
        ENDIF
        CALL PXLSP3( NTRAK, IDIM, PTRAK, EVAL, EVEC, IERR )
        IF( IERR.NE.0 ) THEN
          PRINT *, 'QQEVSH: PXLSP3 error', IERR
          CP= -1.0
          DP= -1.0
        ELSE
          CP= 3.0*(EVAL(1)*EVAL(2)+EVAL(2)*EVAL(3)+EVAL(3)*EVAL(1))
          IF( CP.LT.0.0 ) CP= 0.0
          DP= 27.0*EVAL(1)*EVAL(2)*EVAL(3)
          IF( DP.LT.0.0 ) DP= 0.0
        ENDIF
        CALL PXJSP3( NTRAK, IDIM, PTRAK, EVAL, EVEC, IERR )
        IF( IERR.NE.0 ) THEN
          PRINT *, 'QQEVSH: PXJSP3 error', IERR
          S= -1.0
          A= -1.0
        ELSE
          S= 3.0/2.0*(EVAL(1)+EVAL(2))
          A= 3.0/2.0*EVAL(1)
          IF( S.LT.0.0 ) S= 0.0
          IF( A.LT.0.0 ) A= 0.0
        ENDIF
        CALL PXAKO4( NTRAK, IDIM, NTRAK+1, PTRAK, NTRAK, 
     &       AKP, AVEC, IERR)
        IF( IERR.NE.0 ) THEN
          PRINT *, 'QQEVSH: PXAKO4 error', IERR
          AKP= -1.0
        ELSE
          IF( AKP.LT.0.0 ) AKP= 0.0
        ENDIF
      ELSE
        T= -1.0
        TMA= -1.0
        TMI= -1.0
        MH= -1.0
        ML= -1.0
        BT= -1.0
        BW= -1.0
        CP= -1.0
        DP= -1.0
        S= -1.0
        A= -1.0
        AKP= -1.0
      ENDIF
C
C  The End:
      RETURN
      END
