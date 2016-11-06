C   12/08/87 708120949  MEMBER NAME  EAZERO   (S)           FORTRAN77
      SUBROUTINE EAZERO
C-----------------------------------------------------------
C  VERSION OF 12/08/87     LAST MOD 12/08/87    E ELSEN
C  INIT VARIABLES USED IN E ANALYSIS
C  ADAPTED FORM F11HEL.NEUFS(ELSE34, ENTRY ZUNULL IN EVENTO)
C-----------------------------------------------------------
C
C
      IMPLICIT INTEGER*2 (H)
C---------------------------------------- COMMONS FOR LEAD GLASS ENERGY
C
      COMMON /LINK/   IBLCHK,IREG,NBLK,NBLE,XI,YI,ZI,XF,YF,ZF,XSTART,
     *                YSTART,ZSTART,PSTART,TRKL(2,50),TRITER,EBITER,
     *                PMAG,NNEW,NENEW,NLIST(40),ENEW,ICHARG(40,20),
     +                NBLO,MEICL(50),NEICL(50),EBIT1,NBN1,EBLO1,NBL1
      COMMON /CLDAT/ NUMBLO,NBLIST(40),ENLIST(40),IPACL,ECLCO,ECLUN,
     *               PIMP1,PIMP2
C-------------------------------------------- GENERAL COMMONS
C
      COMMON / CRELC / SIGP,SIG1EP,CH1EP,CHI1W,CHI2N,PCHI2,ESHM,RFIR,
     +                 CHIRP,CHIRZ,ZINT,RDIS,JKAND
C
      XI=-2000.
      YI=-2000.
      ZI=-2000.
      ZF=-2000.
      PSTART=-20.
      PIMP2=-2000.
      EBITER=-1.
      ENEW=-1.
      RFIR=0.0
      CHIRP=-1.
      CHIRZ=-1.
      ZINT=-1200.
      SIGP=-1.
      SIG1EP=-1.
      CH1EP=-1.
      CHI1W=-1.
      CHI2N=-1.
      PCHI2=-1.
      NUMBLO=-1
      JKAND=0
      END
