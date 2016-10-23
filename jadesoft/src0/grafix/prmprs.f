C   10/02/86 604291407  MEMBER NAME  PRMPRSR  (S)           FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE PRMPRS( IPMPRS, XS, YS , DEL, SIZE )
C-----------------------------------------------------------------------
C
C   AUTHOR:   J. HAGEMANN 29/04/86 :  PRINT BOS BANK MPRS
C
C
C-----------------------------------------------------------------------
C
      IMPLICIT INTEGER*2 (H)
C
#include "cdata.for"
#include "cgraph.for"
C
      COMMON / CWORK1 / HWORK(70)
      COMMON / CWORK  / HDUM(8000),
     +                  HADC(2,42),HTDC(2,42),HTDC1(2,42),HADCF(2,16),
     +                  HTDCF(2,16),HTSPAR(16)
C
      DIMENSION HSY(13,16)
C
      DATA HSY/
     $'UN','EX','PE','CT','ED',' I','NT','ER','RU','PT',' N','-3','  ',
     $'UN','EX','PE','CT','ED',' I','NT','ER','RU','PT',' N','-2','  ',
     $'NO','N ','IN','CR','EA','SI','NG',' W','IR','E-','NU','MB','ER',
     $'IL','LE','GA','L ','HI','T ','CO','UN','TE','R ','  ','  ','  ',
     $'NE','GA','TI','VE',' D','AT','A ','FO','UN','D ','  ','  ','  ',
     $'JE','TC',' B','AN','K ','LO','NG','ER',' T','HA','N ','40','00',
     $'WI','RE',' N','UM','BE','R ','GR','EA','TE','R ','15','36','  ',
     $'NO',' H','IT','S ','IN',' R','IN','G2','  ','  ','  ','  ','  ',
     $'LA','ST',' C','EL','L ','IN',' R','IN','G ','IN','CO','MP','L.',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'T2','  ','RE','JE','CT','  ','  ','  ','  ','  ','  ','  ','  ',
     $'Z-','VE','RT','EX',' R','EJ','EC','T ','  ','  ','  ','  ','  ',
     $'  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ','  ',
     $'AC','TU','AL','LY',' R','EJ','EC','TE','D ','BY',' N','10','  '/
C
C
C------------------  C O D E  ------------------------------------------
C
      LIM2 = IPMPRS
      LH2  = LIM2*2
      YS   = YS - DEL
      CALL CORE(HWORK,46)
      WRITE(JUSCRN,1) HDATA(LH2+10)
    1 FORMAT(' MIPROC EVENT COUNT (=TRIGGER NUMBER) ',I8)
      CALL SYSSYM(XS,YS,SIZE,HWORK,46,0.)
C
      YS = YS - DEL
      CALL CORE(HWORK,59)
      WRITE(JUSCRN,2) HDATA(LH2+4),(HDATA(LH2+K),K=7,9)
    2 FORMAT(' MIPROC ZVTX',I6,' ZVX AND BACKG. PEAKS',2I6,' FLAG ',I2)
      CALL SYSSYM(XS,YS,SIZE,HWORK,59,0.)
C
      YS = YS - DEL
      CALL CORE(HWORK,27)
      WRITE(JUSCRN,3) HDATA(LH2+5)
    3 FORMAT(' MIPROC FOUND ',I3,' R3 TRACKS')
      CALL SYSSYM(XS,YS,SIZE,HWORK,27,0.)
C
      YS = YS - DEL
      CALL CORE(HWORK,37)
      WRITE(JUSCRN,4) HDATA(LH2+3)
    4 FORMAT(' MIPROC REJECTION AND ERROR FLAG ',Z4)
      CALL SYSSYM(XS,YS,SIZE,HWORK,37,0.)
C
      MIPROC = HDATA(LH2+3)
      MIPROC = SHFTL(MIPROC,1)
      DO 7 IBT = 1,16
         MIPROC = SHFTR(MIPROC,1)
         IF( .NOT. TBIT(MIPROC,31) ) GO TO 7
C BIT IBT IS ON, WRITE CORRESPONDING INFORMATION
         DO 5  I = 1,13
            HDUM(I) = HSY(I,IBT)
    5    CONTINUE
         YS = YS - DEL
         CALL CORE(HWORK,41)
         WRITE(JUSCRN,6) (HDUM(I),I=1,13)
    6    FORMAT('  ACTION/ERROR ',13A2)
         CALL SYSSYM(XS,YS,SIZE,HWORK,41,0.)
    7 CONTINUE
      RETURN
      END
