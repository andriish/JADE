C   12/01/87 702231755  MEMBER NAME  ZE4VML   (S)           FORTRAN77
      SUBROUTINE ZE4VMI( NPZIM1 )
C-----------------------------------------------------------
C VERSION OF 23/02/87    LAST MOD. 23/02/87  M.ZIMMER
C
C INITS ADDITIONAL MEIER LG ANALYSIS
C
C CUTS APPLIED :
C       P >= 1GEV
C       E/P FROM LGCL >= 0.5 ( SINCE E-MEIER < E-LGCL )
C
C-----------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / BCS / IW(1)
      DIMENSION RW(1), HW(1)
      EQUIVALENCE (HW(1),RW(1),IW(1))
      LOGICAL FIRST / .TRUE. /

C                                      CUTS USED FOR SELECTION          IDATES
C                                      MIN MOMENTUM
      DATA PMIN / 1. /
C                                      E/P MIN (FROM LGCL )
      DATA EPMIN / .5 /
C
C
      IF( .NOT. FIRST ) GO TO 10
         FIRST = .FALSE.
C
         CALL LGINIT
         CALL FEHLTX( 21,'P - CUT IN ZE4VML NOT PASSED')
         CALL FEHLTX( 22,'E/P - CUT IN ZE4VML NOT PASSED')
         CALL FEHLTX( 23,'TRACK NOT CHARGED IN ZE4VML')
         WRITE(6,9101) PMIN,  EPMIN
 9101    FORMAT(/' ZE4VMI HAS BEEN CALLED WITH FOLLOWING CUTS:'/,
     *           ' P - MIN                 :  ',F10.4,/,
     *           ' MIN E/P FROM LGCL       :  ',F10.4,/)
C
   10 CONTINUE
C
C                                      SAVE PATR-BANK
      NPPATR = IW(IBLN('PATR'))
      LPATR = IW( NPPATR )
      IPATR = IW( NPPATR - 2 )
      NPZIM1 = 0
      CALL BCRE(NPZIM1,'ZIM1',1,LPATR,*8100,IER)
      IF(IER.NE.0) GOTO 8100
      CALL BSAW(1,'ZIM1')
C                                      COPY PATR-BANK TO ZIM1 BANK
      CALL UCOPY( RW(NPPATR + 1), RW(NPZIM1+1), LPATR )
C                                      END OF INIT
      GOTO 8000
      ENTRY ZE4VML( NPSPUR, ESHM )
C----------------------------------------------------------------------
C LAST MOD 23/02/87 MZ/GE
C
C PERFORM MLG-ANALYSIS
C NPSPUR IS POINTER TO TRACK IN ZE4V-BANK
C----------------------------------------------------------------------
      ESHM = 0.
      NPZE4V = IW( IBLN('ZE4V'))
      IF ( NPZE4V .LE. 0 ) GOTO 8000
C                                      LENGTH OF GEN. TRACK PART
      LT = HW( NPZE4V*2 + 5 )
C                                      IS TRACK CHARGED?
      IPTF = HW( NPSPUR*2 + 18 )
      IF( IPTF .NE. 1 ) CALL FEHLER( 23, *8000 )
C                                      MOMENTUM CUT
      PTOT = RW( NPSPUR + 6 )
      IF ( PTOT .LT. PMIN )   CALL FEHLER( 21, *8000 )
C                                      E/P MINIMUM ( FROM LGCL )
      EPLGCL = RW( NPSPUR + LT + 1 ) / PTOT
      IF ( EPLGCL .LT. EPMIN ) CALL FEHLER( 22, *8000 )
C                                      GET TRACK NUMBER OF PATR BANK
      NUMTRK = HW( NPSPUR*2 + LT*2 + 9 )
C                                      ESHM ENERGY FROM BARREL ONLY
      CALL PBEN9( NUMTRK, NCLU, ESHE, ESHM, IPBC, IPBER )
      GOTO 8000
C                                      NORMAL EXIT
      ENTRY ZE4VME (IPPATR )
C----------------------------------------------------------------------
C LAST MOD 23/02/87  MZ/GE
C
C RESTORE ORIGINAL PATR- BANK ( COPIED TO ZIM1 ) IN OLD LOCATION
C----------------------------------------------------------------------
C                                      COPY ZIM1-BANK TO PATR-BANK
      CALL UCOPY( RW(NPZIM1 + 1), RW(NPPATR+1), LPATR )
      CALL BDLS('ZIM1',1)
      IPPATR = NPPATR
 8000 CONTINUE
      RETURN
C                                      ERRORS
C
 8100 WRITE(6,*)' ++++++ ZE4VLG :  ERROR WHILE CREATING ZIM1-BANK !!'
      RETURN
 8200 WRITE(6,*)' ++++++ ZE4VLG :  INVALID PTF-CODE !!!!!!'
      RETURN
      END
