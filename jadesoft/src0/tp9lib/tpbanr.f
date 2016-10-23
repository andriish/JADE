C   29/09/86 901092129  MEMBER NAME  TPBANR   (S)        M  FORTRAN77
C
C-----------------------------------------------------------------------
      SUBROUTINE  TPBANR
C-----------------------------------------------------------------------
C
C     Author:   C. Bowdery       2/10/86:  Print TP banner
C
C          mod: C. Bowdery      16/12/87:  Correct FORMAT error
C          mod: C. Bowdery      26/01/88:  Print comment lines as well
C          mod: C. Bowdery      21/07/88:  Use internal DATA
C     Last mod: C. Bowdery       9/01/89:  Different DESY and UK code
C
C     Routine to print the TP banner using internal DATA.
C
C------------------  D E F I N I T I O N S  ----------------------------

      IMPLICIT  INTEGER*2 (H)

C                            COMMON for storing comment lines.
C                            This is BLOCK DATA set in TPBD01.
C                            If MAXCOM changes, adjust FORMAT label 40

      PARAMETER  ( MAXCOM = 4 )
      CHARACTER*80  COMMNT
      COMMON / CTPCMT / COMMNT(MAXCOM)

C                            Array of characters to hold banner

      CHARACTER  C(86)*40

      DATA  C( 1) / '          ///////////////////////|      ' /
      DATA  C( 2) / '      //////////////////|               ' /
      DATA  C( 3) / '         ////////////////////////|      ' /
      DATA  C( 4) / '     ////////////////////|              ' /
      DATA  C( 5) / '         TTTTTTTTTTTTTTTTTTTTTT//|      ' /
      DATA  C( 6) / '    /PPPPPPPPPPPPPPP//////|             ' /
      DATA  C( 7) / '         TTTTTTTTTTTTTTTTTTTTTT//       ' /
      DATA  C( 8) / '    PPPPPPPPPPPPPPPPPP/////|            ' /
      DATA  C( 9) / '         TTTTTTTTTTTTTTTTTTTTTT/        ' /
      DATA  C(10) / '    PPPPPPPPPPPPPPPPPPPP///|            ' /
      DATA  C(11) / '                 TTTTTT//|              ' /
      DATA  C(12) / '    PPPPP//|        PPPPP//|            ' /
      DATA  C(13) / '                 TTTTTT//|              ' /
      DATA  C(14) / '    PPPPP//|        PPPPP//|            ' /
      DATA  C(15) / '                 TTTTTT//|              ' /
      DATA  C(16) / '    PPPPP//|        /PPPP//|            ' /
      DATA  C(17) / '                 TTTTTT//|              ' /
      DATA  C(18) / '    PPPPP///////////PPPPP//             ' /
      DATA  C(19) / '                 TTTTTT//|              ' /
      DATA  C(20) / '    PPPPP///////////PPPPP/              ' /
      DATA  C(21) / '                 TTTTTT//|              ' /
      DATA  C(22) / '    PPPPPPPPPPPPPPPPPPPP/               ' /
      DATA  C(23) / '                 TTTTTT//|              ' /
      DATA  C(24) / '    PPPPPPPPPPPPPPPPPP/                 ' /
      DATA  C(25) / '                 TTTTTT//|              ' /
      DATA  C(26) / '    PPPPPPPPPPPPPPP/                    ' /
      DATA  C(27) / '                 TTTTTT//|              ' /
      DATA  C(28) / '    PPPPP//|                            ' /
      DATA  C(29) / '                 TTTTTT//|              ' /
      DATA  C(30) / '    PPPPP//|                            ' /
      DATA  C(31) / '                 TTTTTT//|              ' /
      DATA  C(32) / '    PPPPP//|                            ' /
      DATA  C(33) / '                 TTTTTT//|              ' /
      DATA  C(34) / '    PPPPP//|                            ' /
      DATA  C(35) / '                 TTTTTT//               ' /
      DATA  C(36) / '    PPPPP//                             ' /
      DATA  C(37) / '                 TTTTTT/                ' /
      DATA  C(38) / '    PPPPP/                              ' /
      DATA  C(39) / '                                        ' /
      DATA  C(40) / '                                        ' /
      DATA  C(41) / '                                        ' /
      DATA  C(42) / '                                        ' /
      DATA  C(43) / '                                        ' /
      DATA  C(44) / '                                        ' /
      DATA  C(45) / '            V                           ' /
      DATA  C(46) / '                    9                   ' /
      DATA  C(47) / '             E                //////////' /
      DATA  C(48) / '//////|                                 ' /
      DATA  C(49) / '              R              ///////////' /
      DATA  C(50) / '////////|         N                     ' /
      DATA  C(51) / '               S           //99999999999' /
      DATA  C(52) / '9999/////|       O                      ' /
      DATA  C(53) / '                I         /9999999999999' /
      DATA  C(54) / '999999///|      I                       ' /
      DATA  C(55) / '                 O        9999//|       ' /
      DATA  C(56) / '   9999//|     S                        ' /
      DATA  C(57) / '                  N       9999//|       ' /
      DATA  C(58) / '   9999//|    R                         ' /
      DATA  C(59) / '                          9999//////////' /
      DATA  C(60) / '///9999//|   E                          ' /
      DATA  C(61) / '                    9     9999//////////' /
      DATA  C(62) / '///9999//|  V                           ' /
      DATA  C(63) / '       VERSION 9           9999999999999' /
      DATA  C(64) / '9999999//|       VERSION 9              ' /
      DATA  C(65) / '                             99999999999' /
      DATA  C(66) / '9999999//|                              ' /
      DATA  C(67) / '       VERSION 9                        ' /
      DATA  C(68) / '   9999//|       VERSION 9              ' /
      DATA  C(69) / '                    9       /////|      ' /
      DATA  C(70) / '   9999//|  V                           ' /
      DATA  C(71) / '                           /////////////' /
      DATA  C(72) / '///9999//    E                          ' /
      DATA  C(73) / '                  N        9999/////////' /
      DATA  C(74) / '///9999/      R                         ' /
      DATA  C(75) / '                 O          999999999999' /
      DATA  C(76) / '999999/        S                        ' /
      DATA  C(77) / '                I            99999999999' /
      DATA  C(78) / '99999/          I                       ' /
      DATA  C(79) / '               S                        ' /
      DATA  C(80) / '                 O                      ' /
      DATA  C(81) / '              R                         ' /
      DATA  C(82) / '                  N                     ' /
      DATA  C(83) / '             E                          ' /
      DATA  C(84) / '                                        ' /
      DATA  C(85) / '            V                           ' /
      DATA  C(86) / '                    9                   ' /

C------------------  C O D E  ------------------------------------------

C                            Output first line of box

      WRITE(6,10)

C                            Different options for DESY and UK

  10  FORMAT('1',8X,58('* '),'*',4(/ 9X,'*',115X,'*') )
C 10  FORMAT('1',8X,58('* '),'*',2(/ 9X,'*',115X,'*') )

      DO  30  J = 1,86,2

        WRITE(6,20) C(J), C(J+1)
  20    FORMAT(9X,'*',22X,2A40,13X,'*')

  30  CONTINUE

      WRITE(6,40) ( COMMNT(KK), KK = 1,MAXCOM )

C                            Different options for DESY and UK

  40  FORMAT(4(9X,'*',115X,'*'/),9X,58('* '),'*'/
     +        (9X,'*',115X,'*'/),
     +       4(9X,'*',20X,A80,15X,'*'/9X,'*',115X,'*'/),
     +         9X,58('* '),'*'//)

C 40  FORMAT(2(9X,'*',115X,'*'/),9X,58('* '),'*'/
C    +        (9X,'*',115X,'*'/),
C    +       4(9X,'*',20X,A80,15X,'*'/9X,'*',115X,'*'/),
C    +         9X,58('* '),'*')

      RETURN
      END
