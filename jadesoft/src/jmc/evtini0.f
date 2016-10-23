C   24/08/85 511061959  MEMBER NAME  EVTINI0  (S)           FORTRAN
C   23/08/85            MEMBER NAME  EVTINI   (S)           FORTRAN
C   19/08/83 308251801  MEMBER NAME  EVTINI   (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE EVTINI
C-----------------------------------------------------------------------
C
C   AUTHOR:   E. ELSEN     05/03/79 :  INITIALISES A MONTE CARLO EVENT
C
C        MOD  E. ELSEN     05/11/81 :  FORWARD DETECTOR INITIALISED
C        MOD  J. HAGEMANN  17/01/83 :  S/R DAY2 NOW CALLED HERE ONCE PER
C             R  RAMCKE             :  EVENT, NOT FOR EVERY PARTICLE IN
C                                   :  S/R JIPATR. NEW COMMON /CDAY/
C        MOD  C. BOWDERY   19/08/83 :  EXPANDED COMMON /CWSET/.
C                                   :  HITAR(16000) I.E. 4000 HITS
C  LAST  MOD  A.FINCH      24/08/85 :  CALL TO MCTAGI INSERTED TO
C                                      INITIALISE NEW MCTAG SCHEME
C
C
C   THIS ROUTINE INITIALIZES AN EVENT BEFORE MONTE CARLO TRACKING.
C
C   IT HAS TO BE CALLED ONCE FOR EVERY EVENT BEFORE ITS PARTICLES ARE
C   TRACKED THROUGH THE DETECTOR.
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON / CLGHIT / AMPLG(3000)
      COMMON / CWSET  / NHALL, ISTART, HPCELL(98), HITAR(16000)
      COMMON / CJPATR / KPATR, IPATR(1243)
C
      COMMON / CTOFF  / ITOFAR(21)
      COMMON / CBPC   / IBPC(12)
C
      COMMON / CJTCDC / IBANK, IPART
      COMMON / CDAY   / IDAY(5)
C
C
C------------------------  C O D E  ------------------------------------
C
C                  INITIALIZE TO VECT BANK 0 AND PARTICLE 0
C
      IBANK = 0
      IPART = 0
C
C                  INITIALIZE TRACKING THROUGH LEAD GLASS
C
      DO 1 I=1,3000
    1 AMPLG(I)=0.
C
C                  INITIALIZE TRACKING THROUGH CENTRAL DETECTOR
C                            ZERO NUMBER OF HITS; 1ST FREE WORD IS #1
      NHALL  = 0
      ISTART = 1
C
C                  INITIALIZE TRACK ARRAY
C
      KPATR = 8
      CALL DAY2(IDAY)
      DO 10 I=1,8
   10 IPATR(I) = 0
      IPATR(1) = 8
      IPATR(3) = 48
C
C                  INITIALIZE HODOSCOPE COUNTER ARRAYS
C
      DO 20 I=1,21
   20 ITOFAR(I)=0
      DO 21 I=1,12
   21 IBPC(I)=0
      CALL INTTOF
C
C                  INITIALIZE FWD LG BLOCKS
C
      CALL MCTAGI
C
      RETURN
      END
