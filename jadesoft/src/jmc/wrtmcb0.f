C   25/08/83 311102220  MEMBER NAME  WRTMCB0  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE WRTMCB(NUNIT)
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     09/05/79 :  OUTPUTS MONTE CARLO BINARY EVENTS
C
C        MOD  E. ELSEN     19/08/80 :  TAGGING BANK 'ATAG' ADDED
C        MOD  E. ELSEN     21/07/82 :
C        MOD  C. BOWDERY   18/08/83 :  NEW 'HITL' BANK ADDED
C        MOD  C. BOWDERY   22/08/83 :  EXPANDED COMMON /CWSET/.
C                                   :  HITAR(16000) I.E. 4000 HITS
C   LAST MOD  C. BOWDERY   10/11/83 :  BSAW CALLED FOR 'MUCH' AND 'MUHC'
C
C   THIS ROUTINE OUTPUTS MT CARLO DATA ONTO UNIT NUNIT USING BOS.
C
C   SPECIAL LIST IS CLEARED AFTER EVERY GARBAGE COLLECTION TO
C   ENSURE THAT JETC BANK IS ALWAYS LAST IN OUTPUT RECORD
C
C   NOTE THAT THE VECT BANK NOW EXISTS ON INPUT
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER * 2 (H)
C
      COMMON / CGEO1  / GEO(65)
      COMMON / CJDRCH / DRCH(34)
      COMMON / CTOF   / IATDC(94)
      COMMON / C4VHIT / H4VHTR(400), H4VHIT(4000)
      COMMON / CWSET  / NHALL, ISTART, HPCELL(98), HITAR(16000)
      COMMON / CMUB   / LMUBMX,LMUB,HMUB(2000)
      COMMON / CJPATR / KPATR, APATR(1448)
      COMMON / BCS    / IW(1)
C
      DIMENSION NAM1(3), HW(1)
C
      EQUIVALENCE ( IW(1) , HW(1) )
C
      DATA NAM1/'MTCO','MJET','MGEO'/
      DATA LGEO,LJET/65,34/
      DATA ICALL/0/
C-----------------------------------------------------------------------
C
      ICALL=ICALL+1
      IF(ICALL.NE.1) GO TO 10
C
C                OUTPUT CONSTANTS ONLY ONCE AT THE FIRST CALL TO THE S/R
C
C                CLEAR SPECIAL-LIST
C
      CALL BSLC
      CALL BCRE( IPMTCO, NAM1(1), 1, 100, *1000, IER )
C
C                                           JET CHAMBER CONST
C
      CALL BCRE( IND, NAM1(2), 2, LJET, *1000, IER )
      CALL BSTR( IND, DRCH, LJET )
      IW(IPMTCO - 4 + 55 ) = IND - IPMTCO + 4
C
C                                           GEOMETRICAL CONST
C
      CALL BCRE( IND, NAM1(3), 3, LGEO, *1000, IER )
      CALL BSTR( IND, GEO, LGEO )
      IW(IPMTCO - 4 + 56 ) = IND - IPMTCO + 4
C
C                                           WRITE OUT
C
      CALL BSAW( 3, NAM1 )
      CALL BSLW
      CALL BWRITE( NUNIT )
      CALL BSLT
      CALL BDLG
      CALL BSLC
C
C                                           MU CHAMBER CONST
C
      CALL MUCONW( 1, NUNIT, 0, HERR )
      CALL BSLC
C
C                   ORGANIZE OUTPUT FOR MT CARLO DATA
C
C                                           SET UP FIRST BANK
C
   10 CALL BCRE( IPHEAD, 'HEAD', 1, 100, *1000, IER )
      CALL BSAW( 1, 'HEAD' )
      CALL STHEAD( LENGTH, IW(IPHEAD+1) )
C
C                                           FILL LATC BANK
C
      CALL LHLATC( LENGTH )
      CALL BCRE( IND, 'LATC', 0, LENGTH, *1000, IER )
      CALL BSAW( 1, 'LATC' )
      CALL STLATC( LENGTH, IW(IND+1) )
C
C                                           SET TOF ADC/TDC BANK
C
      CALL BCRE( IND, 'ATOF', 0, 94, *1000, IER )
      CALL BSAW( 1, 'ATOF' )
      CALL BSTR( IND, IATDC, 94 )
C
C                                           SET BP ADC/TDC BANK
C
      CALL LHATBP( LENGTH )
      CALL BCRE( IND, 'ATBP', 0, LENGTH, *1000, IER )
      CALL BSAW( 1, 'ATBP' )
      CALL STATBP( LENGTH, IW(IND+1) )
C
C                                           SET 4_VECTOR BANK 'VECT'
C
      CALL BLOC( IND, 'VECT', 0, *1000, IER )
      CALL BSAW( 1, 'VECT' )
C
C                                           SET 4_VECTOR BANK 'PALL'
C
      CALL BLOC( IND, 'PALL', 0, *1011, IER )
      CALL BSAW( 1, 'PALL' )
C
C                                           4_VECTOR/JETC HIT LABEL BANK
C
1011  LENGTH = ( NHALL + 2 ) / 2
      CALL BCRE( IND, 'HITL', 8, LENGTH, *1000, IER )
      CALL BSAW( 1, 'HITL' )
      HW(IND*2 + 1) = NHALL
      CALL MVCL( HW, 4*IND + 2, H4VHIT, 0, NHALL * 2 )
C
C                                           LEAD GLASS AMPLITUDES
C
      CALL LHALGN( LENGTH )
      IF( LENGTH.EQ.0 ) GO TO 30
      CALL BCRE( IND, 'ALGN', 1, LENGTH, *1000, IER )
      CALL BSAW( 1, 'ALGN' )
      CALL STALGN( LENGTH, IW(IND+1) )
C
C                                           MU CHAMBER DATA
C
   30 CALL BLOC( IND, 'MUEV', 0, *35, IER )
      CALL BSAW( 1, 'MUEV' )
   35 CALL BLOC( IND, 'MUCH', 0, *36, IER )
      CALL BSAW( 1, 'MUCH' )
   36 CALL BLOC( IND, 'MUHC', 0, *37, IER )
      CALL BSAW( 1, 'MUHC' )
C
C                                           TAGGING DATA
C
   37 CALL LHATAG( LENGTH )
      IF( LENGTH .LE. 0 ) GO TO 40
      CALL BCRE( IND, 'ATAG', 0, LENGTH, *1000, IER )
      CALL BSAW( 1, 'ATAG' )
      CALL STATAG( LENGTH, IW(IND+1) )
C
C                                           TRACK BANK
C
   40 CONTINUE
      IF( KPATR .LT. 10 ) GO TO 50
      CALL BCRE( IND, 'PATR', 12, KPATR, *1000, IER )
      CALL BSAW( 1, 'PATR' )
      CALL BSTR( IND, APATR, KPATR )
C
C                                           Z-CHAMBER
C
   50 CONTINUE
      CALL BSAW( 1, 'ZETC' )
C
C                                           JET CHAMBER DATA AND BANK
C                                           DESCRIPTOR.
C                                           HPCELL(97) IS A SPECIAL WORD
C                                           POINTING TO THE FIRST FREE
C                                           LOCATION IN HITAR.
C
  60  LENGTH = (HPCELL(97) - 1) / 2 + 50
      IF( LENGTH .LE. 50 ) GO TO 90
      CALL BCRE( IND, 'JETC', 8, LENGTH, *1000, IER )
      CALL BSAW( 1, 'JETC' )
      IW(IND+1) = 0
C                                           COPY INFO: HPCELL & HITAR
C
      CALL MVCL( IW, (IND+1)*4, HPCELL, 0, (LENGTH-1)*4 )
C
   90 CALL BSLW
      CALL MCHIST
      CALL BWRITE( NUNIT )
C
      CALL BSLT
      CALL BDLG
      CALL BSLC
      RETURN
C
C                                           ERROR IN RECORD WRITING
C
 1000 WRITE(6,9101) IER
 9101 FORMAT(' BCRE OR BLOC ENDED WITH ERROR IER=',I4,' IN WRTMCB')
      RETURN
      END
