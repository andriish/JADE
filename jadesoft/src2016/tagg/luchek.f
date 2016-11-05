C   14/03/84            MEMBER NAME  LUCHEK   (S)           FORTRAN
C   06/07/79 C9071001   MEMBER NAME  LUCHEK   (S)           FORTRAN
      SUBROUTINE LUCHEK
C---  CHECK FIRED LUMONITORS
C
C     H.WRIEDT       06.07.79      18:55
C     LAST MODIFICATION     10.07.79     01:20
C
      IMPLICIT INTEGER*2 (H)
C
      COMMON /CWORK/ IWORK1(501),HITRAK(3),HCLST,IWORK2(3),HWORK1,
     +               HPBLAT,IWORK3(793),HTRACK(20,51)
C
      LOGICAL LBSIS(8),LBSOT(8),LABSIS(8),LABSOT(8)
C
      COMMON /CLUMON/ LUMHIT(6,24),HLUPLT(102)
C
C---  EXPLANATION OF INDICES IN LUMHIT
C     2.ND INDEX:  1 FOR 1A,  2 FOR 1B,  3 FOR 1A*1B ETC.,
C                 22 FOR 8A, 23 FOR 8B, 24 FOR 8A*8B
C     1.ST INDEX (IN CASE OF A- OR B-COUNTERS):
C                1: SHOULD HAVE FIRED
C                2: HAS FIRED
C                3: HAS NOT FIRED
C                4: SHOULD NOT HAVE FIRED
C                (IN CASE OF A*B COINCIDENCES):
C                1: SHOULD HAVE FIRED
C                2: HAS FIRED
C                3: HAS NOT FIRED
C                4: NONE HAVE FIRED
C                5: ONLY A HAS FIRED
C                6: ONLY B HAS FIRED
C
      NTRACK = HITRAK(1)
      IF (NTRACK.LE.0) RETURN
        DO 10 I = 1,NTRACK
        LUMAOT = HTRACK(4,I)/256
        LUMBOT = HTRACK(4,I) - LUMAOT*256
        LUMAIS = HTRACK(5,I)/256
        LUMBIS = HTRACK(5,I) - LUMAIS*256
        LPBIS = HPBLAT
          DO 20 J = 1,8
          LBIT = 9 - J
          IAOT = JBIT(LUMAOT,LBIT)
          IBOT = JBIT(LUMBOT,LBIT)
          IAIS = JBIT(LUMAIS,LBIT)
          IBIS = JBIT(LUMBIS,LBIT)
          LSBIT = 17 - J - (J-1)/2
          ISIS = JBIT(LPBIS,LSBIT)
          LBSIS(J) = .FALSE.
          LBSOT(J) = .FALSE.
          LABSIS(J) = .FALSE.
          LABSOT(J) = .FALSE.
C
C---      A - COUNTERS
          INDA = 3*(J-1) + 1
          INDAOT = 4 - 3*IAOT
          INDAIS = 3 - IAIS
          LUMHIT(INDAOT,INDA) = LUMHIT(INDAOT,INDA) + 1
          IF (IAIS.EQ.1) HLUPLT(J) = HLUPLT(J) + 1
          IF (IAOT.NE.1) GOTO 2
          LUMHIT(INDAIS,INDA) = LUMHIT(INDAIS,INDA) + 1
          HLUPLT(J+50) = HLUPLT(J+50) + 1
C
C---      B - COUNTERS
    2     INDB = 3*(J-1) + 2
          INDBOT = 4 - 3*IBOT
          INDBIS = 3 - IBIS
          LUMHIT(INDBOT,INDB) = LUMHIT(INDBOT,INDB) + 1
          IF (IBIS.EQ.1) HLUPLT(J+10) = HLUPLT(J+10) + 1
          IF (IBOT.NE.1) GOTO 3
          LUMHIT(INDBIS,INDB) = LUMHIT(INDBIS,INDB) + 1
          HLUPLT(J+60) = HLUPLT(J+60) + 1
C
C---      B-S - COINCIDENCES
    3     IF (ISIS.NE.1) GOTO 4
          IF (IBIS.EQ.1) LBSIS(J) = .TRUE.
          IF (IBOT.EQ.1) LBSOT(J) = .TRUE.
C
C---      A-B - COINCIDENCES
    4     INDAB = 3*(J-1) + 3
          ICOIN = IAOT*IBOT
          IF (ICOIN.NE.1) GOTO 20
          LUMHIT(1,INDAB) = LUMHIT(1,INDAB) + 1
          HLUPLT(J+70) = HLUPLT(J+70) + 1
          IF (IAIS*IBIS.NE.1) GOTO 1
          LUMHIT(2,INDAB) = LUMHIT(2,INDAB) + 1
          HLUPLT(J+20) = HLUPLT(J+20) + 1
          GOTO 5
    1     LUMHIT(3,INDAB) = LUMHIT(3,INDAB) + 1
          INDABI = 4 + IAIS + 2*IBIS
          LUMHIT(INDABI,INDAB) = LUMHIT(INDABI,INDAB) + 1
          GOTO 6
C
C---      A-B-S - COINCIDENCES
    5     IF (ISIS.NE.1) GOTO 8
          HLUPLT(J+30) = HLUPLT(J+30) + 1
          LABSIS(J) = .TRUE.
          GOTO 7
    6     IF (ISIS.NE.1) GOTO 8
    7     HLUPLT(J+80) = HLUPLT(J+80) + 1
          LABSOT(J) = .TRUE.
C
C---      A-B-S - B-S - COINCIDENCES
    8     IF (J.LE.4) GOTO 20
          IF (LABSIS(J-4).AND.LBSIS(J)) HLUPLT(J+36) = HLUPLT(J+36) + 1
          IF (LABSIS(J).AND.LBSIS(J-4)) HLUPLT(J+40) = HLUPLT(J+40) + 1
          IF (LABSOT(J-4).AND.LBSOT(J)) HLUPLT(J+86) = HLUPLT(J+86) + 1
          IF (LABSOT(J).AND.LBSOT(J-4)) HLUPLT(J+90) = HLUPLT(J+90) + 1
   20     CONTINUE
   10   CONTINUE
      RETURN
      END
