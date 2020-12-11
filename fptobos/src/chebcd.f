      CHARACTER*4 FUNCTION CHEBCD(IN)
*
*     The INTEGER IN is converted into the character string CHEBCD
*     It is assumed that IN is a 4byte integer word
*     Thus CHEBCD contains 4 characters, e.g. Z48454144  will be 'HEAD'
*
*     In current version only characters 75-255 are defined 
*
*      J.Olsson   13.12.2005
*
      CHARACTER CHAS*256, CHOUT*4
      DATA CHAS(1:40)    /'                                        '/
      DATA CHAS(41:80)   /'                                    .<(+'/
      DATA CHAS(81:120)  /'|&         !$*); -/          %_>?       '/
      DATA CHAS(121:160) /'   :#@`=" abcdefghi       jklmnopqr     '/
      DATA CHAS(161:200) /'  stuvwxyz                      {ABCDEFG'/
      DATA CHAS(201:240) /'HI      }JKLMNOPQR        STUVWXYZ      '/
      DATA CHAS(241:256) /'0123456789      '/
*
CAV USE STANDARD PREFIX SYNTAX
      DATA IBYT1,IBYT2 /Z'FF000000',Z'00FF0000'/
      DATA IBYT3,IBYT4 /Z'0000FF00',Z'000000FF'/
*
* ----------------------  CODE  --------------------
*
      INT1 = IAND(IN,IBYT1)
      INT2 = IAND(IN,IBYT2)
      INT3 = IAND(IN,IBYT3)
      INT4 = IAND(IN,IBYT4) + 1

*
      INT1 = ISHFT(INT1,-24) + 1
      INT2 = ISHFT(INT2,-16) + 1
      INT3 = ISHFT(INT3,-8)  + 1
*      WRITE(6,1001) INT1,INT2,INT3,INT4
* 1001 FORMAT(' CHEBCD:  INT1-4 ',4I15)
*      WRITE(6,1002) CHAS(INT1:INT1),CHAS(INT2:INT2),CHAS(INT3:INT3),
*     $              CHAS(INT4:INT4)
* 1002 FORMAT(' CHEBCD CHAS1-4',4(A1,1X))
*
      CHOUT = CHAS(INT1:INT1)//CHAS(INT2:INT2)//
     $        CHAS(INT3:INT3)//CHAS(INT4:INT4) 
      CHEBCD = CHOUT
*
      RETURN
      END
