*******************************************************************
*
*     HIGZ colour parameters for graphics action in JADEZ
*
*     This file is included when compiling SETCOL
*
      LOGICAL LCOL,LINIT
      INTEGER NCOL,IFORE,IBACK,IFORE2,IBACK2
      REAL WID
      PARAMETER (NCOL=10)
      REAL RED,GREEN,BLUE
      CHARACTER TCOL*4
      COMMON /PLTCOL/ LCOL,LINIT,IFORE,IBACK,IFORE2,IBACK2
     _     ,RED(NCOL),GREEN(NCOL),BLUE(NCOL),TCOL(NCOL),WID(NCOL)

      INTEGER PLTFLG
      COMMON /CPLTC2/ PLTFLG
*******************************************************************      

