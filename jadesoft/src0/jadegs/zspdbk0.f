C   16/06/87 706161850  MEMBER NAME  ZSPDBK0  (JADEGS)      FORTRAN77
      SUBROUTINE ZSPDBK( ITRK )
C----------------------------------------------------------
C  Version of 22/04/87      last mod 22/04/87   E Elsen
C  Copy information of z fit into Bank ZSPD, itrk
C  all existing banks of this name are deleted.
C  The words are stored as:
C     (..+1) = IJC(I),       REL POINTER IN JETC
C     (..+2) = S(I),         TRACK LENGTH
C     (..+3) = Z(I),         Z
C     (..+4) = FLAG(I),      0 IF OK
C     (..+5) = 1/SIGMA(I)**2
C----------------------------------------------------------
      IMPLICIT INTEGER*2 (H)
      PARAMETER (NWTR=5)
      COMMON / BCS / IW(1)
      DIMENSION HW(1),RW(1)
      EQUIVALENCE (HW(1),IW(1),RW(1))
C
      COMMON /CWORK/ NHIT,IZB(60),IJC(60),IJH(60),NW(60),LH(60),
     *                LRFL(60),ALU(60),ARU(60),X(60),Y(60),Z(60),S(60),
     *                G(60),DT(60),DL(60),DW(60),FCORR(60)
C
      NPJETC = IW(IBLN('JETC'))

      IF( NHIT .GT.0 .AND. NPJETC.GT.0 .AND.
     *              IW(NPJETC-2).EQ.0 ) THEN
        CALL BCRE( NPZSPD, 'ZSPD', ITRK, NWTR*NHIT+1, &8000, IER )
        CALL BSAW( 1, 'ZSPD' )
        IW(NPZSPD+1) = NWTR
        NPZSP1 = NPZSPD + 1
        DO 100 I=1,NHIT
          IW(NPZSP1+(I-1)*NWTR+1) = IJC(I) - NPJETC*2
          RW(NPZSP1+(I-1)*NWTR+2) = S(I)
          RW(NPZSP1+(I-1)*NWTR+3) = Z(I)
          IF( IZB(I) .EQ. 0 ) THEN
            IW(NPZSP1+(I-1)*NWTR+4) = 16
          ELSE
            IW(NPZSP1+(I-1)*NWTR+4) = 0
          ENDIF
          RW(NPZSP1+(I-1)*NWTR+5) = G(I)
  100   CONTINUE
      ENDIF
 8000 CONTINUE
      END
