C   12/03/84 406221959  MEMBER NAME  LISTGN   (S)           FORTRAN
C
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
       SUBROUTINE LISTGN
C
C THIS SUBROUTINE GENERATES THE NEIGHBOUR LIST FOR THE 1983 TAGGER
C SOFTWARE NUMBERING SCHEME - MAINLY BECAUSE I CANT BE BOTHERED TO
C TYPE THEM ALL IN TO BLOCK DATA.
C
C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C NLIST - LIST OF CLOSEST NEIGHBOURS TO A BLOCK   (I,1) = TOTAL NUMBER
C         OF NEIGHBOURS
C
#include "comtag.for"
C
C
       LISTOK = 0
       DO 10 I = 1,48,24
          ISTART = I
          IEND   = I + 7
          DO 20 J = ISTART,IEND
             NLIST(J,1,2) = 8
             NLIST(J,2,2) = J - 1
             NLIST(J,3,2) = J + 1
             NLIST(J,4,2) = J + 7
             NLIST(J,5,2) = J + 8
             NLIST(J,6,2) = J + 9
             NLIST(J,7,2) = J + 15
             NLIST(J,8,2) = J + 16
             NLIST(J,9,2) = J + 17
       IF ( NLIST(J,2,2) .LT. ISTART ) NLIST(J,2,2) = IEND
       IF ( NLIST(J,4,2) .LT. (ISTART + 8) )  NLIST(J,4,2) = IEND + 8
       IF ( NLIST(J,7,2) .LT. (ISTART + 16) ) NLIST(J,7,2) = IEND + 16
       IF ( NLIST(J,9,2) .GT. (IEND + 16) )   NLIST(J,9,2) = ISTART + 1600004200
       IF ( NLIST(J,6,2) .GT. (IEND + 8) )    NLIST(J,6,2) = ISTART + 8
       IF ( NLIST(J,3,2) .GT. IEND ) NLIST(J,3,2) = ISTART
  20      CONTINUE
          ISTART = I + 8
          IEND = ISTART + 7
          DO 30 J = ISTART,IEND
             NLIST(J,1,2) = 8
             NLIST(J,2,2) = J - 1
             NLIST(J,3,2) = J + 1
             NLIST(J,4,2) = J + 7
             NLIST(J,5,2) = J + 8
             NLIST(J,6,2) = J + 9
             NLIST(J,7,2) = J - 9
             NLIST(J,8,2) = J - 8
             NLIST(J,9,2) = J - 7
      IF ( NLIST(J,2,2) .LT. ISTART )       NLIST(J,2,2) = IEND
      IF ( NLIST(J,4,2) .LT. (ISTART + 8) ) NLIST(J,4,2) = IEND + 8
      IF ( NLIST(J,7,2) .LT. (ISTART - 8) ) NLIST(J,7,2) = IEND - 8
      IF ( NLIST(J,9,2) .GT. (IEND - 8) )   NLIST(J,9,2) = ISTART - 8
      IF ( NLIST(J,6,2) .GT. (IEND + 8) )   NLIST(J,6,2) = ISTART + 8
      IF ( NLIST(J,3,2) .GT. IEND )         NLIST(J,3,2) = ISTART
  30      CONTINUE
          ISTART = I + 16
          IEND = ISTART + 7
          DO 10 J = ISTART,IEND
             NLIST(J,1,2) = 8
             NLIST(J,2,2) = J - 1
             NLIST(J,3,2) = J + 1
             NLIST(J,4,2) = J - 15
             NLIST(J,5,2) = J - 16
             NLIST(J,6,2) = J - 17
             NLIST(J,7,2) = J - 9
             NLIST(J,8,2) = J - 8
             NLIST(J,9,2) = J - 7
       IF ( NLIST(J,2,2) .LT. ISTART )        NLIST(J,2,2) = IEND
       IF ( NLIST(J,4,2) .GT. (IEND - 16) )   NLIST(J,4,2) = ISTART - 1600007800
       IF ( NLIST(J,7,2) .LT. (ISTART - 8) )  NLIST(J,7,2) = IEND - 8
       IF ( NLIST(J,9,2) .GT. (IEND - 8) )    NLIST(J,9,2) = ISTART - 8
       IF ( NLIST(J,6,2) .LT. (ISTART - 16) ) NLIST(J,6,2) = IEND - 16
       IF ( NLIST(J,3,2) .GT. IEND )          NLIST(J,3,2) = ISTART
  10   CONTINUE
       DO 70 I = 9,16
          XMAP(I) = -1.0 * XMAP(17 - I)
  70      YMAP(I) = YMAP(17 - I)
       DO 80 I = 17,32
          XMAP(I) = XMAP(33 - I)
   80     YMAP(I) = -1.0 * YMAP(33 - I)
       DO 90 I = 1,32
          XMAP(I + 32) = -1.0 * XMAP( I)
 90       YMAP(I + 32) = -1.0 * YMAP( I)
C
C LISTOK = 1 FLAGS THAT LIST HAS BEEN GENERATED TO STOP LISTGN BEING
C             CALLED AGAIN
C
       LISTOK = 1
       RETURN
       END
