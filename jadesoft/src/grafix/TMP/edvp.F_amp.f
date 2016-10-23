C   09/03/84 807251533  MEMBER NAME  EDVP     (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EDVP
C-----------------------------------------------------------------------
C
C   AUTHOR:  J. HAGEMANN   12/02/86 : EDIT VPAT-BANK
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      LOGICAL DSPDTM
      LOGICAL FL18,FL22,FL24
C
#include "cdata.for"
#include "cgraph.for"
#include "cgrscl.for"
C
      COMMON / CGAMOD / MODEGA, GAHEX
      COMMON / CGRAP2 / BCMD, DSPDTM(30), ISTVW, JTVW
      COMMON / CPROJ  / XMINR,XMAXR,YMINR,YMAXR,IPRJC,FL18,FL22,FL24
      COMMON / CVX    / NNPATR, NDUMM, NNJETC, NNVTXC
C
      DIMENSION IDATUM(5)
      DIMENSION IPHT(7), HTEMP(246)
C
      DATA HYES / 'Y ' /
      DATA HBLK / '  ' /
C
C
C------------------  C O D E  ------------------------------------------
C
C                            CHECK IF WE ARE RUNNING IN GA-MODE
      IF( MODEGA .NE. 1 ) GO TO 9000
C                            CHECK IF EDVP IS POSSIBLE FOR THIS VIEW
      IF( LASTVW .NE. 20 ) GO TO 9100
      IF( .NOT. FL22  .OR.
     *    ABS(XMAX-XMIN) .GT. 0.2*ABS(XMAXST(20)-XMINST(20)) )
     *   GO TO 9200
C
      CALL CLEAR
      CALL CLOC( IPVPAT, 'VPAT', NNVTXC )
      IF( IPVPAT .GT. 0 ) GO TO 1002
         WRITE(6,1001) NNVTXC
 1001    FORMAT(' VPAT-bank ',I3,'  not existing!')
         CALL TRMOUT(80,' EDVP stops here!^')
         RETURN
 1002 WRITE(6,1003) NNVTXC
 1003 FORMAT(' VPAT-bank ',I3,'  selected!')
C
      IPVPT2 = IPVPAT*2
      ILVPT2 = IDATA(IPVPAT)*2
      NTRKV  = HDATA(IPVPT2+5)
      ILHD   = HDATA(IPVPT2+1)
      ITDL   = HDATA(IPVPT2+4)
C
      CALL TRMOUT(80,' 1: You can edit existing single tracks.^')
      CALL TRMOUT(80,' 2: You can add a new linked track.^')
      CALL TRMOUT(80,' Choose 1 or 2.  <1>,2^')
 1050 CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, ICHO )
      IF( ICHO .EQ. 0 ) ICHO = 1
      IF( ICHO .GT. 0 .AND. ICHO .LT. 3 ) GO TO 1100
C
         CALL TRMOUT(80,' Don''t be so nervous!  1 or 2?^')
         GO TO 1050
C
 1100 CALL TRMOUT(80,' Are you sure? <Y>,N^')
      CALL TRMIN( 2, HCD )
      IF( HCD .EQ. HBLK ) HCD = HYES
      IF( HCD .NE. HYES ) GO TO 10000
C
         IF( ICHO .EQ. 2 ) GO TO 5000
C
C                                           BRANCH 1
 1200    CALL TRMOUT(80,' Enter number of ID-track to be edited.^')
         CALL TRMIN( 4, ICD )
         CALL GETINT( ICD, ITRK )
         IF( ITRK .GT. 0 .AND. ITRK .LT. 99 ) GO TO 1300
C
            CALL TRMOUT(80,' Illegal track number!  0 < ITRK < 99^')
            GO TO 1200
C
 1300    CALL TRMOUT(80,' Are you sure? <Y>,N^')
         CALL TRMIN( 2, HCD )
         IF( HCD .EQ. HBLK ) HCD = HYES
         IF( HCD .NE. HYES ) GO TO 1200
C
            HFND  = 0
            IPTRI = IPVPT2 + ILHD
            DO 1400 IT = 1, NTRKV
               ITID  = HDATA(IPTRI+1)
               IF( ITID .NE. ITRK ) GO TO 1350
C
                  CALL TRMOUT(80,' Enter total number of linked hits.^')
 1320             CALL TRMIN( 4, ICD )
                  CALL GETINT( ICD, NTOT )
                  IF( NTOT .GT. 0 .AND. NTOT .LT. 8 ) GO TO 1330
C
                     CALL TRMOUT(80,' Wrong input:   0 < NHIT < 8^')
                     GO TO 1320
C
 1330             CALL CLEAR
                  WRITE(6,1331) (HDATA(IPTRI+5+IZ),IZ=1,7)
 1331             FORMAT(' Old hit pointers:',7I6)
                  WRITE(6,1332) NTOT
 1332             FORMAT(' Now enter ',I3,' new hit pointers!')
C
                  HDATA(IPTRI+2) = 0
                  HDATA(IPTRI+3) = 0
                  HDATA(IPTRI+4) = 3
                  HDATA(IPTRI+5) = NTOT
C
                  READ(5,*) (IPHT(I),I=1,NTOT)
C
                  DO 1340 I7 = 1, 7
                     IF( I7 .GT. NTOT ) IPHT(I7) = 0
                     HDATA(IPTRI+5+I7) = IPHT(I7)
 1340             CONTINUE
                  HFND = 1
                  GO TO 1500
C
 1350          IPTRI = IPTRI + ITDL
               IF( IPTRI .GE. IPVPT2+ILVPT2 ) GO TO 1500
 1400       CONTINUE
C
 1500       IF( HFND .EQ. 0 )
     *         CALL TRMOUT(80,' Track not found in bank.^')
C
            CALL TRMOUT(80,' Any other existing track to be edited?
     * <N>,Y^')
            CALL TRMIN( 2, HCD )
            IF( HCD .EQ. HYES ) GO TO 1200
C
      GO TO 11000
C
C                                           BRANCH 2
 5000 IF( NTRKV .LT. 20 ) GO TO 5050
         WRITE(6,5001) NTRKV
 5001    FORMAT(' Buffer full. Number of linked tracks= ',I4,'  > 19!')
         GO TO 10000
C
 5050 NBYT = ILVPT2*2
      CALL MVCL( HTEMP, 0, IDATA, IPVPT2*2, NBYT )
C
      CALL CLEAR
      CALL TRMOUT(80,' Now editing can start!^')
 5100 CALL TRMOUT(80,' Enter ID-track number of selected track you want
     *to add!^')
      CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, ITRK )
      IF( ITRK .GT. 0 .AND. ITRK .LT. 99 ) GO TO 5200
C
         CALL TRMOUT(80,' Illegal track number!  0 < ITRK < 99^')
         GO TO 5200
C
 5200 CALL TRMOUT(80,' Are you sure? <Y>,N^')
      CALL TRMIN( 2, HCD )
      IF( HCD .EQ. HBLK ) HCD = HYES
      IF( HCD .NE. HYES ) GO TO 5100
C
      CALL TRMOUT(80,' Enter total number of linked hits.^')
 5300 CALL TRMIN( 4, ICD )
      CALL GETINT( ICD, NTOT )
      CALL CLEAR
      IF( NTOT .GT. 0 .AND. NTOT .LT. 8 ) GO TO 5400
C
         CALL TRMOUT(80,' Wrong input:   0 < NHIT < 8^')
         GO TO 5300
C
 5400 WRITE(6,5401) NTOT
 5401 FORMAT(' Now enter ',I3,' new hit pointers!')
C
      IPTEMP = 6 + NTRKV*12
C
      HTEMP(IPTEMP+1) = ITRK
      HTEMP(IPTEMP+2) = 0
      HTEMP(IPTEMP+3) = 0
      HTEMP(IPTEMP+4) = 3
      HTEMP(IPTEMP+5) = NTOT
C
      READ(5,*) (IPHT(I), I = 1, NTOT )
C
      DO 5500 I7 = 1, 7
         IF( I7 .GT. NTOT ) IPHT(I7) = 0
            HTEMP(IPTEMP+5+I7) = IPHT(I7)
 5500 CONTINUE
C
      CALL DAY2(IDATUM)
      HTEMP(3) = (IDATUM(1)-85)*1000 + IDATUM(2)
      HTEMP(5) = HTEMP(5) + 1
      HTEMP(6) = HTEMP(6) + 1
C
      NWRD = 3 + (NTRKV+1)*6
      CALL BDLS( 'VPAT', NNVTXC )
      CALL CCRE( IPVPAT, 'VPAT', NNVTXC, NWRD, IERR)
      IF( IERR .NE. 0 ) GO TO 9300
         CALL MVCL( IDATA, IPVPAT*4, HTEMP, 0, NWRD*4 )
C
      GO TO 11000
C
 9000 CALL TRMOUT(80,' Sorry, EDVP only works in GA-mode!^')
      GO TO 10000
C
 9100 CALL TRMOUT(80,' Sorry, EDVP not available for this view!^')
      GO TO 10000
C
 9200 CALL TRMOUT(80,' You have to magnify the VC-view more than 5-times
     * in the interested region!^')
      GO TO 10000
C
 9300 CALL TRMOUT(80,' Error during creation of VPAT-bank!^')
      GO TO 10000
C
10000 CALL TRMOUT(80,' EDVP ended abnormally!^')
      GO TO 15000
C
11000 CALL TRMOUT(80,' EDVP ended normally!^')
C
C
15000 CONTINUE
C
      RETURN
      END
