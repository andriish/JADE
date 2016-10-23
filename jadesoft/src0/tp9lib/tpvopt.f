C   04/02/88 807222047  MEMBER NAME  TPVOPT   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPVOPT( ERROR )
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       4/02/88:  Verify TP options
C
C          mod: C. Bowdery      16/03/88:  Add TRACK options
C          mod: C. Bowdery       7/06/88:  Add VPROCESS option to TRACK
C     Last mod: C. Bowdery      22/07/88:  Set VTXC options.
C
C     Routine to verify TP options in the options table.
C     Errors are printed when found.
C
C-----------------   A R G U M E N T S      ----------------------------
C
C     ERROR     Out      L*4      TRUE if errors found
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

      PARAMETER  ( NP  = 15, NG = 10 )
      PARAMETER  ( N1  = NP-1, N2  = NP-2,  N3  = NP-3,  N4  = NP-4 )
      PARAMETER  ( N5  = NP-5, N6  = NP-6,  N7  = NP-7,  N8  = NP-8 )
      PARAMETER  ( N9  = NP-9, N10 = NP-10, N11 = NP-11, N12 = NP-12)

      CHARACTER   CTPOPT*20, CVALID(NP,NG)*20
      CHARACTER   CREFIT*8,  CSTRNG*20, CWEAK*20

#include "tables.for"

      LOGICAL  ERROR, WEAK, STRONG, TCOPY(NOPTS)

      DATA  CVALID /
     +  'MUON',  'ANALYSE', 'TP', 'DELETE', 'FULL', 'IFNOTDONE', N6*' ',
     +  'TOF',   'ANALYSE','IFNOTDONE','TP','DELETE', N5*' ',
     +  'TAGG',  'ANALYSE','IFNOTDONE','TP','DELETE', N5*' ',
     +  'VTXC',  'ANALYSE','COMFIT', N3*' ',
     +  'LG',    'ANALYSE','IFNOTDONE','TP','DELETE',
     +           'LGCAL','IFNOLGCAL','TRACKMATCH', N8*' ',
     +  'CALCS', 'THRUST','SPHERICITY','REDUCTION', N4*' ',
     +  'REFIT', 'RFI','IFNORFI','WEAKVTX','STRONGVTX','ZS','IFNOZS',
     +           'COMMONZVTX','OLDPATR', N9*' ',
     +  'DEDX',  'ANALYSE','TP', N3*' ',
     +  'TRACK', 'TP', 'DELETE', 'VPROCESS', N4*' ',
     +  'JETC',  'ANALYSE','OLDDEL','RFICAL','IFNORFICAL',
     +           'ZCAL','ZVTX','IFNOTDONE', N8*' '/

      DATA CREFIT, CSTRNG, CWEAK / 'REFIT', 'STRONGVTX', 'WEAKVTX' /

C------------------  C O D E  ------------------------------------------

      ERROR = .FALSE.

C                            Check each option group in turn

      DO  70  K = 1,NOG

C                            Copy row of true/false options

        DO  10  I = 1,NOK

          TCOPY(I) = ATABLE( K, I )

  10    CONTINUE

C                            Check that the specified options
C                            are valid for this option group by checking
C                            off the valid options in TCOPY using CVALID

        DO  30  I = 1,NG

          IF( CVALID(1,I) .EQ. NTABLE(K) ) THEN

            DO  20  J = 2,NP

              CTPOPT = CVALID( J, I )
              IF( CTPOPT .EQ. ' ' ) GO TO 25

C                            Find the table location of this option

              DO  15  MPOS = 1,NOK

                IF( CTPOPT .EQ. OTABLE( MPOS ) ) GO TO 18

  15          CONTINUE

C                            Programmer error! Option not in OTABLE

              CALL TPERRH
              ERROR = .TRUE.
              RETURN

C                            Clear the flag in the temporary table

  18          TCOPY( MPOS ) = .FALSE.

  20        CONTINUE

C                            Any entries not checked off in TCOPY?

  25        DO  28  N = 1,NOK

              IF( TCOPY(N) ) THEN
                ERROR = .TRUE.

C                            Print the illegal option

                WRITE(6,26)  OTABLE(N), NTABLE(K)
  26            FORMAT(/'  ****   TP option error!  Keyword ',A20,
     +                  ' is not valid for option group  ',A8)
              ENDIF

  28        CONTINUE

C                            Terminate loop over CVALID
            GO TO 70

          ENDIF

  30    CONTINUE

C                            If here then option group not in CVALID

        CALL TPERRH
        PRINT *,CVALID(1,I),I,NTABLE(K),K

        ERROR = .TRUE.

  70  CONTINUE

C                            Check for incompatible options

      CALL TPUOPT( CREFIT, CSTRNG, STRONG )
      CALL TPUOPT( CREFIT, CWEAK,  WEAK   )

      IF( STRONG  .AND.  WEAK ) THEN

        ERROR = .TRUE.
        WRITE(6,85)
  85    FORMAT(/'  ****   TP Option error!  STRONGVTX and WEAKVTX are ',
     +          'incompatible options')

      ENDIF

      RETURN
      END
