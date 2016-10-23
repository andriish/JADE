C   22/01/88 808021742  MEMBER NAME  CALCOR   (SOURCE)      FORTRAN


C        CORRECTION OF SYSTEMATIC EFFECTS IN THE LG-CALIBRATION
C        FOR THE BARREL PART

C        ROUTINE CALLED BY LGANAL

C          WRITTEN BY D.PITZL 22.1.88

C=======================================================================
      SUBROUTINE  CALCOR
C=======================================================================


      IMPLICIT INTEGER *2 (H)

#include "'jadelg.source.for"

C     COMMON /CWORK/ LNG,HNORD,HNORML,HPOINT(4),HLGADC(2,2880),
C    1               LGCL(3),LNGCL,NPNT(4),IDENT(2),NCLST,NCLBEC(3),
C    2               ETOT(4),NNEUT,EGAM(4),IFLAG(5),NWPCL,
C    3               MAPCL(81),  CLPRP(1280)
C     INTEGER *2 HMAPCL(2,81)
C     DIMENSION ICLSPR(16,80),CLSPRP(16,80), CLMPRP(1361)
C     EQUIVALENCE (HMAPCL(1,1),MAPCL(1)), (CLMPRP(1),MAPCL(1))
C     EQUIVALENCE (ICLSPR(1,1),CLPRP(1)),  (CLSPRP(1,1),CLPRP(1))
C
C---       IF ALGN - BANK IS PRESENT IT HAS 3 HEADER WORDS AND ONE
C          WORD FOR EACH BLOCK ( I.E. TWO HALFWORDS WITH BLOCK # AND
C          ENERGY IN MEV )

      DATA  NCALLS / 0 /

      IF ( NCALLS .EQ. 0 ) PRINT 3355
      NCALLS = 1

 3355 FORMAT ( T2,'JADELG.LOAD (CALCOR) IS CALLED FROM LGANAL',
     +   ', PERFORMS LG-CALIBRATION CORRECTION ON ALGN-BANK')


      NHIT = LNG - 3

CCC   PRINT 1144, NHIT
 1144 FORMAT ( /T2,' CALCOR: # OF BLOCKS IN ALGN-BANK = ',I6,
     +  /T2,' BLOCK#  RING       MEV     FCORR       MEV')

      IF ( NHIT .LT. 1 ) GOTO 90

      DO 80 I=1,NHIT
         IBLO  = HLGADC ( 1, I )
         IF ( IBLO .GT. 2687 ) GO TO 80
C---                                         BARREL PART HERE
         MEV   = HLGADC ( 2, I )
         RMEV  = FLOAT ( MEV )
         IRING = MOD ( IBLO , 32 )

         RMEV0 = RMEV
         CALL BBCORR ( IRING, FCORR )
         RMEV = RMEV * FCORR

         MEV  = INT ( RMEV )
         HLGADC ( 2, I ) = MEV

CCC      PRINT 1155, IBLO, IRING, RMEV0, FCORR, RMEV

  80  CONTINUE
  90  CONTINUE

 1155 FORMAT ( T2,2I6, F10.1, F10.3, F10.1 )

C---                           INCREASE HNORML TO INDICATE CALIBRATION
C                              CORRECTION WAS DONE
      HNORML = HNORML + 12000

      RETURN
      END
