C   29/07/79 C9073001   MEMBER NAME  MDALGN   (S)           FORTRAN
      SUBROUTINE MDALGN
C-----------------------------------------------------------
C
C   VERSION OF 29/07/79      LAST MOD    30/07/79       E.ELSEN
C   ERASE NON EXISTING LG RINGS FOR MONTE CARLO DATA
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
      COMMON / BCS / IW(1)
      DIMENSION HW(1)
      EQUIVALENCE ( HW(1), IW(1) )
C
      COMMON / CLGHIT / AMPL(3000)
C
C                                          GET ALGN BANK POINTER
      NPALGN = IW( IBLN('ALGN') )
      IF( NPALGN .EQ. 0 ) RETURN
      NP2 = NPALGN * 2
      IF( HW( NP2 + 3 ) .GE. HW( NP2 + 6 ) ) RETURN
C                                          INITIALISE AMPL
      CALL VZERO( AMPL, 3000 )
C                                          GET INTERNAL POINTERS
      IL = NP2 + HW(NP2+3) + 6
      IH = NP2 + HW(NP2+6) + 4
C                                          COPY FIRED BLOCKS TO HAMPL
C                                          INCREMENT BLOCK NUMBER FOR
C                                          PROPER USE IN STALGN
      DO 1000  I = IL, IH, 2
 1000 AMPL(HW(I)+1) = HW(I+1) + .5
C                                           ERASE EXTREME RINGS
      CALL ORDLG
C                                           CHECK IF BLOCKS SURVIVE
      CALL LHALGN( LNEW )
      IF( LNEW .EQ. 0 ) GO TO 2000
C                                           CHANGE LENGTH OF ALGN
      LD = LNEW - IW(NPALGN)
      IF( LD .GE. 0 ) RETURN
      CALL BCHM( NPALGN, LD, IER )
      CALL STALGN( LNEW, IW(NPALGN+1) )
      RETURN
C
C                                           DELETE BANK
 2000 CALL BDLS( 'ALGN', IW(NPALGN-2) )
      RETURN
      END
