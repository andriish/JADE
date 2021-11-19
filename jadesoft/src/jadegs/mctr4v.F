C   17/01/84 401171711  MEMBER NAME  MCTR4V   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE MCTR4V( IOPT, IERROR)
C-----------------------------------------------------------------------
C
C   AUTHOR    C. BOWDERY   3/10/83 :  CREATE 'TR4V' BANK
C
C        MOD  C. BOWDERY   5/10/83 :  REMOVE BUG WRT 2ND HIGHEST TRACK
C        MOD  C. BOWDERY   6/10/83 :  ADD THE IOPT AND IERROR ARGUMENTS
C        MOD  C. BOWDERY  17/10/83 :  ABSORBED HITS HAVE -VE POINTERS
C        MOD  C. BOWDERY  17/10/83 :  3 HIT ORIGINS ==> 4
C        MOD  C. BOWDERY  19/10/83 :  PRINTING FLAGS ADDED (IEFL1/2)
C   LAST MOD  C. BOWDERY  16/01/84 :  SPEED IMPROVEMENTS
C
C   PROCESSES THE 'HITL' AND 'HTSL' BANKS FOR MC EVENTS ALONG WITH THE
C   'JHTL' BANK FROM PATREC IN ORDER TO DETERMINE  WHICH  PARTICLE  IN
C   'VECT' BANK 0 OR 1 WAS THE CAUSE  OF  EACH  TRACK  IN  THE  'PATR'
C   BANK.THE RESULTS ARE PLACED IN A NEW BANK CALLED 'TR4V'.
C
C-----------------------------------------------------------------------
C
C                  STRUCTURE OF THE 'TR4V' BANK   32 BIT WORDS
C                  ----------------------------
C
C     1ST WORD  =  NUMBER OF 'PATR' TRACKS
C
C     THIS IS FOLLOWED BY GROUPS OF 8 WORDS, ONE GROUP PER 'PATR' TRACK.
C
C  |  1ST GROUP WORD  =  NO. OF ASSOCIATED 4_VECTOR PARTICLES
C  |  2ND   "    "    =  NO. OF ASSOCIATED 'JETC' HITS WITH 'PATR' TRACK
C  |      THIS IS FOLLOWED BY 3 SUBGROUPS OF 2 WORDS
C  |      |   1ST SUBGROUP WORD =  CODED 4_VECTOR PARTICLE ORIGIN
C  |      |   2ND   "    "      =  NO. OF HITS FROM THE ABOVE ORIGIN
C
C       THE FIRST SUBGROUP CORRESPONDS TO THE MOST PROBABLE ORIGIN,
C       THE SECOND SUBGROUP CORRESPONDS TO THE 2ND MOST PROBABLE ORIGIN
C       (BUT MAY BE EMPTY) AND SO ON.
C
C       THE CODE USED IS:   2 * 4_VECTOR_PARTICLE + VECT_BANK_NUMBER
C
C       THE ARGUMENTS
C       -------------
C
C   INPUT:  IOPT:   0 = DO NOTHING IF THE 'TR4V' BANK CORRESPONDING TO
C                       THE LATEST 'PATR' BANK ALREADY EXISTS. OTHERWISE
C                       CREATE THAT 'TR4V' BANK.
C
C                   1 = DELETE EXISTING 'TR4V' BANK CORRESPONDING TO THE
C                       LATEST 'PATR' BANK, IF IT EXISTS, THEN CREATE
C                       A NEW 'TR4V' BANK.
C
C   OUTPUT: IERROR: 0 = NO ERROR OCCURRED.
C                   1 = B O S  BANK ERROR. NO 'TR4V' CREATED.
C                   2 = 'HITL' BANK NOT SAME LENGTH AS 'HTSL' BANK. THIS
C                       COULD BE A SERIOUS ERROR. NO 'TR4V' CREATED.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CWORK  / H4VORG(5000,4), H4VTOT(5000)
      COMMON / CWORK1 / HHTTRK(5000,2)
C
#include "cmubcs.for"
C
      DIMENSION ITR4V(300), ITEMP(4)
C
      DATA MASK7F / Z'7F' / , IEFL1 / 0 / , IEFL2 / 0 /
C
C-----------------------  C O D E  -------------------------------------
C
      IERROR = 0
C
      IPP = IDATA( IBLN( 'PATR' ) )
      IPJ = IDATA( IBLN( 'JHTL' ) )
      IPH = IDATA( IBLN( 'HITL' ) )
      IPS = IDATA( IBLN( 'HTSL' ) )
C
      IF(IPH.LE.0  .OR. IPS.LE.0  .OR. IPP.LE.0 .OR. IPJ.LE.0 ) RETURN
C
C                            ALL 4 BANKS EXIST.
C
      IPH2   = IPH * 2
      IPS2   = IPS * 2
C
C                            NOHITS = NUMBER OF ORIGINAL HITS
C
      NOHITS = HDATA(IPH2+1)
      IF(NOHITS .EQ. HDATA(IPS2+1) ) GO TO 3
C
      IEFL2 = IEFL2 + 1
      IERROR = 2
      IF( IEFL2 .GT. 10 ) RETURN
        WRITE(6,1)
 1      FORMAT(' *** ERROR *** ''HITL'' BANK LENGTH .NE. ''HTSL'' ',
     +                                      'BANK LENGTH IN MCTR4V')
        RETURN
C
C                            PROVIDED THAT  THERE  WERE  SOME  ORIGINAL
C                            UNSMEARED  JETC  HITS,  FIND  THE  CURRENT
C                            'PATR'   BANK   AND   CHECK   WHETHER    A
C                            CORRESPONDING 'TR4V' BANK ALREADY  EXISTS.
C                            PROCEED ACCORDING TO INPUT OPTION IOPT.
C
 3    IF(NOHITS .LT. 1) RETURN
      NBANK  = IDATA(IPP-2)
      CALL CLOC(IPT,'TR4V',NBANK)
      IF(IPT .LE. 0) GO TO 12
        IF(IOPT .EQ. 0) RETURN
        CALL BDLS('TR4V',NBANK)
C
 12   CALL VZERO(H4VORG,12500)
      CALL VZERO(HHTTRK,5000)
C
C                            USE 'HITL' AND 'HTSL' TO
C                            CREATE A TABLE OF HIT ORIGINS.
C
      DO  2  I = 1,NOHITS
        NEWHIT = HDATA(IPS2 + 1 + I)
        IF(NEWHIT .EQ. 0 ) GO TO 2
C
C                            ABSORBED HITS HAVE -VE POINTERS
C
        NEWHIT = IABS( NEWHIT )
        H4VTOT(NEWHIT) = H4VTOT(NEWHIT) + 1
        IF(H4VTOT(NEWHIT) .GT. 4) H4VTOT(NEWHIT) = 4
        H4VORG(NEWHIT,H4VTOT(NEWHIT)) = HDATA(IPH2 + 1 + I)
 2    CONTINUE
C
C                            NOW CONSTRUCT AN  ARRAY  OF  'PATR'  TRACK
C                            ASSOCIATION FOR EVERY HIT IN 'JETC'  USING
C                            THE 'JHTL' BANK.
C
      NHITS = IDATA(IPP + 5)
      NTRKS = IDATA(IPP + 2)
C
      DO  4  I = 1,NHITS
        LABEL       = IDATA(IPJ + 1 + I)
        L1          = ISHFTR(LABEL,17)
        HHTTRK(I,1) = LAND(L1,MASK7F)
C
        L2          = ISHFTR(LABEL,1)
        HHTTRK(I,2) = LAND(L2,MASK7F)
 4    CONTINUE
C
C                            CREATE THE 'TR4V' BANK  WITH  BANK  NUMBER
C                            THE SAME AS THE 'PATR' BANK.
C
      LENGTH = NTRKS*8 + 1
      CALL BCRE(IPT,'TR4V',NBANK,LENGTH,*1000,IER)
      CALL BSAW(1,'TR4V')
      CALL UZERO(IDATA, IPT+1, IPT+LENGTH )
      IDATA(IPT+1) = NTRKS
C
C                            LOOP OVER EVERY 'PATR' TRACK AND PICK  OUT
C                            THE HITS FROM 'JETC'  THAT  BELONG  TO  IT
C                            USING THE HHTTRK ARRAY. FOR EACH TRACK  IN
C                            TURN, HISTOGRAM THE 4 VECTOR ORIGIN(S)  OF
C                            EACH HIT. ENSURE THAT  NO  HIT  ORIGINATES
C                            FROM THE SAME 4  VECTOR  MORE  THAN  ONCE.
C                            DECIDE THE 4 VECTOR ORIGIN OF  EACH  TRACK
C                            FROM THE ARRAY ELEMENT  WITH  THE  LARGEST
C                            NUMBER OF ENTRIES
C
      DO  5  I = 1,NTRKS
        NTOT = 0
        CALL VZERO(ITR4V,300)
C
C                            LOOP OVER ALL HITS
C
        DO  6  J = 1,NHITS
          IF(HHTTRK(J,1) .NE. I .AND. HHTTRK(J,2) .NE. I ) GO TO 6
            NTOT = NTOT + 1
            NUM = H4VTOT(J)
            IF(NUM .EQ. 0) GO TO 6
C
            DO  8  L = 1,NUM
              ITEMP(L) = H4VORG(J,L)
              LH = L - 1
              IF( LH .LE. 0 ) GO TO 88
                DO  7  LL = 1,LH
                  IF( ITEMP(L) .EQ. ITEMP(LL) ) GO TO 8
   7            CONTINUE
  88          ITR4V( ITEMP(L) ) = ITR4V( ITEMP(L) ) + 1
   8        CONTINUE
C
 6      CONTINUE
C
C        DETERMINE THE ORIGIN OF THIS TRACK
C
C              SEARCH THE ITR4V HISTOGRAM TO FIND THE CELLS
C              IE. 4 VECTOR PARTICLES, WITH THE 3 LARGEST NUMBER
C              OF ENTRIES, IE. CONTRIBUTING HITS.
C
C              LARGE1 > LARGE2 > LARGE3   ==> NO. OF HITS
C
C              I4V1  = 4 VECTOR WITH CURRENT HIGHEST NO. OF HITS
C              I4V2  = 4 VECTOR WITH CURRENT 2ND HIGHEST NO. OF HITS
C              I4V3  = 4 VECTOR WITH CURRENT 3RD HIGHEST NO. OF HITS
C
        LARGE1 = 0
        LARGE2 = 0
        LARGE3 = 0
        I4V1   = 0
        I4V2   = 0
C
        DO  9  M = 1,300
          IF(ITR4V(M) .EQ. 0 )     GO TO 9
          IF(ITR4V(M) .LE. LARGE1) GO TO 10
C                                                A NEW HIGHEST NO.
            LARGE3 = LARGE2
            LARGE2 = LARGE1
            I4V3   = I4V2
            I4V2   = I4V1
            LARGE1 = ITR4V(M)
            I4V1   = M
            GO TO 9
  10      IF(ITR4V(M) .LE. LARGE2) GO TO 11
C                                                A NEW 2ND HIGHEST NO.
            LARGE3 = LARGE2
            I4V3   = I4V2
            LARGE2 = ITR4V(M)
            I4V2   = M
            GO TO 9
  11      IF(ITR4V(M) .LE. LARGE3) GO TO 9
C                                                A NEW 3RD HIGHEST NO.
            LARGE3 = ITR4V(M)
            I4V3   = M
 9      CONTINUE
C
C                  NOW FILL IN THE 'TR4V' BANK FOR THIS 'PATR' TRACK.
C
        INDEX = IPT + (I-1)*8 + 1
        IDATA(INDEX + 2) = NTOT
        IF(LARGE1 .LE. 0 ) GO TO 5
          IDATA(INDEX + 3) = I4V1
          IDATA(INDEX + 4) = LARGE1
          IDATA(INDEX + 1) = 1
C                                               INCLUDE A 2ND ORIGIN ?
        IF(.NOT.(           LARGE2 .GE. 8
     +                          .OR.
     +           (LARGE2 .GE. 5 .AND. NTOT .LT. 20) ) ) GO TO 5
C
          IDATA(INDEX + 5) = I4V2
          IDATA(INDEX + 6) = LARGE2
          IDATA(INDEX + 1) = 2
C                                               INCLUDE A 3RD ORIGIN ?
        IF(.NOT.(           LARGE3 .GE. 8
     +                          .OR.
     +           (LARGE3 .GE. 5 .AND. NTOT .LT. 20) ) ) GO TO 5
C
          IDATA(INDEX + 7) = I4V3
          IDATA(INDEX + 8) = LARGE3
          IDATA(INDEX + 1) = 3
C
 5    CONTINUE
      RETURN
C                                      B O S    ERROR
 1000 IERROR = 1
      IEFL1 = IEFL1 + 1
      IF( IEFL1 .GT. 10 ) RETURN
        WRITE(6,1001) IER
 1001   FORMAT(' BCRE ERROR ',I3,' OCCURRED WHILE CREATING ''TR4V'' ')
        RETURN
      END
