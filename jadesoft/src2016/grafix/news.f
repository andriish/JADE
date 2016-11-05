C   01/11/84 807251739  MEMBER NAME  NEWS     (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE NEWS
C-----------------------------------------------------------------------
C
C      PRINT NEWS OF GRAPHICS ONTO THE SCREEN
C
C-----------------------------------------------------------------------
C
      CONTINUE !CALL CLRCON PMF 03/12/99
C
      CALL TRMOUT(80,'    ******  NEWS   *****  NEWS   ******
     $  from 20.05.87^')
C
      CALL TRMOUT(80,'Command  ZFIT available for comparison of differen
     $t z-s-fits^')
C
      CALL TRMOUT(80,'    ******  NEWS   *****  NEWS   ******
     $  from 16.04.87^')
C
      CALL TRMOUT(80,'Command  HLP now with trailing nr for steering out
     $put to dest L1,L2,..^')
C
      CALL TRMOUT(80,'    ******  NEWS   *****  NEWS   ******
     $  from 12.03.87^')
C
      CALL TRMOUT(80,'Command  RES available also in FW view: results of
     $ TAGAN analysis^')
C
      CALL TRMOUT(80,'    ******  NEWS   *****  NEWS   ******
     $  from 17.09.86^')
C
      CALL TRMOUT(80,'Command J68K gives a display of the fadc data in t
     $he Jetchamber.^')
      CALL TRMOUT(80,'works only for events with J68K bank included in t
     $he readout.^')
      CALL TRMOUT(80,'    ******  VERY  H O T   NEWS   *****
     $  from 18.11.99^')
C
      CALL TRMOUT(80,'JADE event display is working again ... yeah!^')
C
      RETURN
      END
