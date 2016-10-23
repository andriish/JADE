C   18/01/87 708200943  MEMBER NAME  FEHLGEP  (S)           FORTRAN77
      SUBROUTINE FEHLGP( ICODE, *)
C-------------------------------------------------------------------
C GIVES ENTRY IN ERROR COUNTING HISTOGRAMM AND JUMPS BACK TO &****
C-------------------------------------------------------------------
      PARAMETER (MAXLNE=50)
      CHARACTER*40  TEXTAR( MAXLNE ),BLANC
      CHARACTER*(*) TEXT
      DATA BLANC/' '/
      IF( ICODE .GT. MAXLNE ) GOTO 8000
      CALL HIST( 99, FLOAT( ICODE ) )
 8000 RETURN 1
C
      ENTRY FEHLTX( NLINE, TEXT )
C-----------------------------------------------------------------
C  FILLS TEXT ARRAY FOR PRINTOUT
C-----------------------------------------------------------------
C                                      CHECK PARAMETERS
      IF(NLINE .GT. MAXLNE .OR. NLINE .LT. 0) THEN
        WRITE(6,9001)  NLINE
 9001   FORMAT('------  FEHLTX : WRONG PARAMETER ----NLINE = ',I5)
        RETURN
      ENDIF
      IF(TEXTAR(NLINE) .NE. BLANC ) THEN
        WRITE(6,9000) NLINE
 9000   FORMAT(///10X,'!!!!!!!!! WARNING FROM FEHLTX !!!!!!!!!!'/
     &            10X,'ERROR NUMBER ',I5,'  DOUBLY DEFINED  '/)
        RETURN
      ENDIF
C                                      FILL ARRAY
      TEXTAR( NLINE ) = TEXT
      RETURN
      ENTRY FEHLIN
C-----------------------------------------------------------------
C  INIT ARRAYS
C-----------------------------------------------------------------
      DO 1002 I=1,MAXLNE
        TEXTAR(I) = BLANC
 1002 CONTINUE
      RETURN
      ENTRY FEHLPR
C-----------------------------------------------------------------
C  PRINTOUT OF ERROR CODE
C-----------------------------------------------------------------
      WRITE(6,9011)
 9011 FORMAT(///,10X,'MEANING OF ENTRIES IN ERROR HISTOGRAM( 99 )'/
     &          ,10X,'  #                TEXT')
      DO 1003 I=1,MAXLNE
        IF ( TEXTAR(I) .NE. BLANC ) WRITE(6,9002)I,TEXTAR(I)
 9002 FORMAT(10X,I3,2X,A40)
 1003 CONTINUE
      WRITE(6,*)' '
      RETURN
      END
