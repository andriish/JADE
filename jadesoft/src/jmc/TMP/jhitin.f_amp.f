C   01/10/83 310011924  MEMBER NAME  JHITIN   (S1)          FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE JHITIN ( * )
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     25/09/78 :  ORDERS JET CHAMBER HITS
C
C        MOD  C. BOWDERY   19/08/83 :  EXPANDED COMMON /CWSET/.
C                                   :  HITAR(16000) I.E. 4000 HITS
C   LAST MOD  C. BOWDERY   23/08/83 :  4V/HIT ASSOCIATION INFO SORTED
C
C        ORDERS HITS ACCORDING TO WIRE NUMBERS (AND DRIFT TIMES WHEN A
C        WIRE HAS MULTIPLE HITS). THE HIT INFO FOR THE CURRENT TRACK IS
C        HELD IN HLIST AND H4VHTR. ON EXIT THE HITAR AND H4VHIT ARRAYS
C        CONTAIN ALL THE HITS INCLUDING THOSE OF THE CURRENT TRACK.
C
C           NHITS  = NO. OF HITS FOR CURRENT TRACK
C           INEXT  = POINTER TO FIRST FREE WORD IN HLIST (4 * NHITS + 1)
C           NHALL  = TOTAL NUMBER OF HITS IN HITAR
C           ISTART = POINTER TO FIRST FREE WORD IN HITAR (4 * NHALL + 1)
C
C        RETURN 1  IF ON ADDING THE CURRENT TRACK'S HITS, NHALL WOULD
C                                                         EXCEED 4000
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / C4VHIT / H4VHTR(400), H4VHIT(4000)
      COMMON / CWORK  / NHITS, INEXT, HLIST(1600)
      COMMON / CWSET  / NHALL, ISTART, HPCELL(98), HITAR(16000)
C
      DIMENSION HW(4)
C
C------------------------  C O D E  ------------------------------------
C
C             FIRST ORDER HITS OF THE CURRENT TRACK USING THE SHELLSORT
C             ALGORITHM. THE SORTING IS DONE 'IN SITU'.
C
      IF( NHITS.EQ. 0 ) RETURN
      IF( NHALL + NHITS .GT. 4000) RETURN 1
C
      IL = 1
      M  = MIN0( NHITS, 30 )
C
   10 M = M/2
      IF( M .EQ. 0 ) GO TO 900
        K = ( NHITS - M )*4
        DO 50 J = IL, K, 4
           I = J
   20      IM = I + M*4
           IF( HLIST(IM)   .GT. HLIST(I)   .OR.
     *        (HLIST(IM)   .EQ. HLIST(I)   .AND.
     *         HLIST(IM+3) .LE. HLIST(I+3)) ) GO TO 50
C
C                            INDICES FOR HLIST
C
              ISH  = ( I  - 1 ) * 2
              IMSH = ( IM - 1 ) * 2
              CALL MVC( HW,    0,    HLIST, ISH,  8 )
              CALL MVC( HLIST, ISH,  HLIST, IMSH, 8 )
              CALL MVC( HLIST, IMSH, HW,    0,    8 )
C
C                            INDICES FOR H4VHTR
C
              I4V = ( I  + 3 ) / 4
              M4V = ( IM + 3 ) / 4
              HTEMP         = H4VHTR( I4V )
              H4VHTR( I4V ) = H4VHTR( M4V )
              H4VHTR( M4V ) = HTEMP
C
              I = I - 4*M
              IF( I .LT. 1 ) GO TO 50
              GO TO 20
   50   CONTINUE
        GO TO 10
  900 CONTINUE
C
C-----------------------------------------------------------------------
C
C     COPY FROM HLIST --> HITAR   AND   H4VHTR --> H4VHIT
C
C            IR = POINTS IN HITAR WHERE NEXT 4 HIT WORDS WILL GO
C            IT = POINTS IN HLIST TO GET NEXT 4 NEW HIT WORDS
C            IL = POINTS IN HITAR TO GET NEXT 4 OLD HIT WORDS
C
C     THE HITAR ARRAY IS ORDERED BY SELECTING THE HIT INFORMATION
C     (8 BYTES) IN DESCENDING ORDER FROM HLIST (NEW HITS) AND HITAR
C     (EXISTING HITS), FROM WHICHEVER HAS THE LARGER WIRE NUMBER OR
C     SMALLER DRIFT TIME, AND PLACING IT INTO HITAR POINTED AT BY 'IR'.
C     'IR' INITIALLY POINTS TO THE TOP POSITION IN HITAR FOR STORING
C     ALL THE EXISTING AND NEW HITS. IT IS DECREMENTED EACH TIME A HIT
C     IS STORED. WHEN A HIT IS TAKEN FROM HLIST, THE 'IT' POINTER IS
C     THEN DECREMENTED. SIMILARLY WHEN AN EXISTING HIT IS REPOSITIONED
C     IN HITAR, THE 'IL' POINTER IS DECREMENTED TO POINT AT THE NEXT
C     EXISTING HIT AT THE TOP OF THE OLD LIST.
C     THE SORTING OF THE 4_VECTOR_TRACK/HIT ASSOCIATION INFO IS
C     SIMILAR BUT THE POINTERS ARE SIMPLER (ONLY 1 HALF-WORD MOVES).
C
C-----------------------------------------------------------------------
C
      IR = ISTART + NHITS * 4 - 1
      IT = INEXT - 5
      IL = ISTART - 5
C                                       H4VHTR * H4VHIT POINTERS
      JR = NHALL + NHITS + 1
      JT = NHITS
      JL = NHALL
C
 1000 IR = IR - 4
      JR = JR - 1
      IF( IL .LT. 0 ) GO TO 1100
C
      IF( HLIST(IT+1) .GT. HITAR(IL+1)  .OR.
     *   (HLIST(IT+1) .EQ. HITAR(IL+1)  .AND.
     *    HLIST(IT+4) .LE. HITAR(IL+4)) ) GO TO 1100
C                                                     REPOSITION OLD HIT
         CALL MVC( HITAR, IR*2, HITAR, IL*2, 8 )
         H4VHIT( JR ) = H4VHIT( JL )
         IL = IL - 4
         JL = JL - 1
         GO TO 1000
C                                                     STORE NEW HIT
 1100 CALL MVC( HITAR, IR*2, HLIST, IT*2, 8 )
      H4VHIT( JR ) = H4VHTR( JT )
      JT = JT - 1
      IT = IT - 4
      IF( IT .GE. 0 ) GO TO 1000
C
C
      ISTART = ISTART + NHITS * 4
      NHALL  = NHALL  + NHITS
      RETURN
      END
