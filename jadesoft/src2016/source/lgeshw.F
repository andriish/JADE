C   24/10/79 910312212  MEMBER NAME  LGESHW   (SOURCE)      FORTRAN
      SUBROUTINE LGESHW
C
C     GET REALISTIC SHOWER ENERGY BY SUBTRACTING THE CONTRIBUTION OF
C     CHARGED PARTICLES, USING THE FOLLOWING FORMULA.
C     ESHOWER = (CLUSTER ENERGY - MIN(P,0.35))
C     AND STORE IT TO ADATA(NPCL+3) REPLACING SIG(E) AS A TEMPLALLY
C     MEASURE.
C     LAST MODIFICATION                24-10-79 Y.WATANABE
C
      IMPLICIT INTEGER *2 (H)
C
#include "cdata.for"
#include "clgwork2.for"
      COMMON /CLGMSB/ MSGVAL(5)
      COMMON /CLGCHG/ NCHIND,NSTEP,CXDCHG(9,100)
C---- CXDCHG  CONTAINS INNER TRACK INFORMATION
C     CXDCHG(9,N)     ABSOLUTE MOMENTUM IN GEV/C
C
      IF(NCLST2.LT.1) RETURN
C     SET POINTERS
      CALL BLOC(NPLG,'LGCL',1,*90)
      NPCLS=NPLG+IDATA(NPLG+3)-1
      NPCGI=NPLG+IDATA(NPLG+1)-1
      NWPCL=IDATA(NPCGI+21)
      NPCL=NPCLS-NWPCL
C
      DO 30 J=1,NCLST2
      NPCL=NPCL+NWPCL
      ESH=ADATA(NPCL+2)
      NCH=HCLLSO(1,J)
      IF(NCH.LT.1) GO TO 20
C     GO THROUGH ALL THE CHARGED PARTICLES.
C      TO TAKE CARE OF THE CASE NCH > 3.
C
      DO 10 N=1,NCHCL2
      IF(HCLIST(1,N).NE.1) GO TO 10
C      ASSUME THAT AT MOST ONE CLUSTER IS CONNECTED TO A PARTICLE.
      K=HCLIST(2,N)
      IF(K.NE.J) GO TO 10
C     SUBTRACT THE CONTRIBUTION.
      ESH=ESH-AMIN1(CXDCHG(9,N),0.35)
10    CONTINUE
C
      IF(ESH.LT.0.) ESH=0.
20    CONTINUE
      ADATA(NPCL+3)=ESH
30    CONTINUE
C
      RETURN
90    MSGVAL(3)=NPLG
      CALL LGMESG(1,2)
      RETURN
      END
