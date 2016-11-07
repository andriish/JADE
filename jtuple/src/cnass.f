
CDECK  ID>, CNASS.
      SUBROUTINE CNASS(NJET,ICON,PNJ,BL,YL,YH,IERR)
*.-----------------------------------------------------------------------
*.
*.    CNASS: Like CASSO, but used to retrieve information about each
*.           Njet configuration.
*.           INPUT:   NJET (integer) Number of jets required
*.                    ICON (integer) Equals i for ith Njet configuration
*.                                   (max valid value = CNCNT(NJET))
*.           OUTPUT:  PNJ(4,*)(real) Array of jets 4-vectors
*.                    BL(*)(integer) Particle i belongs to jet BL(i) 
*.                    YL   (real   ) Lower boundary of y interval for ICONth
*.                                   Njet configuration
*.                    YH   (real   ) Upper boundary of y interval for ICONth
*.                                   Njet configuration
*.                    IERR (integer) Error flag, 0=OK.
*.
*.
*.  CREATED :  08-01-1998, STAN BENTVELSEN
*.  LAST MOD:
*.-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NMXY , NMXP
      PARAMETER (NMXY = 300)
      PARAMETER (NMXP = 30)
      DOUBLE PRECISION YTRANS(NMXY)
      DOUBLE PRECISION PCMJ(NMXP,4,NMXP)
      INTEGER          NTRANS(NMXY), NJITER, NJMAX, NTRACK
      INTEGER          ICMJ(NMXP,NMXY)
      COMMON / CKCOM / YTRANS, PCMJ, NTRANS, ICMJ, NJITER, NJMAX, NTRACK
      INTEGER NJET, ICON, IERR
      REAL PNJ(4,*),YL,YH
      INTEGER BL(*)
      INTEGER CNCNT, IN, I, II, J
      INTEGER NCH, ITMP
      DATA    NCH / 0 /
      SAVE NCH

      IN = CNCNT(NJET)
      IF(IN.EQ.0) THEN
         NCH = NCH + 1
         IF(NCH.LE.10) THEN
            WRITE(*,*) 'NOT RESOLVED FOR NUMBER OF JETS ',NJET
         ENDIF
         IERR = 1
         RETURN
      ELSEIF(ICON.GT.IN) THEN
         NCH = NCH + 1
         IF(NCH.LE.10) THEN
            WRITE(*,*) 'NJET HAS ONLY ',IN,' DISTINCT YCUT RANGES'
         ENDIF
         IERR = 2
         RETURN
      ENDIF
      
      ITMP = 0
      DO I=1,NJITER
         IF(NTRANS(I).EQ.NJET) THEN
            ITMP = ITMP + 1
            IF(ITMP.EQ.ICON) THEN
               YH = 1.*YTRANS(I)
               IF(I.LT.NJITER) YL = 1.*YTRANS(I+1)
               DO J=1,NJET
                  DO II=1,4
                     PNJ(II,J) = SNGL(PCMJ(I,II,J))
                  ENDDO
               ENDDO
               DO J=1,NTRACK
                  BL(J) = ICMJ(I,J)
               ENDDO
               GOTO 889
            ENDIF
         ENDIF
      ENDDO
      
 889  CONTINUE
      RETURN
      END
