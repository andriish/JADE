C   08/08/83 308081857  MEMBER NAME  MUSAFE   (JADEMUS)     FORTRAN
C(************************************************)
C(*                                              *)
C(*  ROUTINE TO DECIDE IF AN ANGULAR COORDINATE  *)
C(*  FALLS IN THAT AREA OF THE MUON FILTER WHERE *)
C(*  IT IS LIKELY NOT TO BE RECOGNISED.          *)
C(*     USED TO SET THE ACCEPTANCE FLAG IN THE   *)
C(*  MUR2 BANK. RETURNS VALUES:-                 *)
C(*         0  -  DEFINITELY IN ACCEPTANCE       *)
C(*         2  -  DEFINITELY OUT OF ACCEPTANCE   *)
C(*                                              *)
C(************************************************)
      INTEGER FUNCTION MUSAFE(COSTHE,PHI)
      IMPLICIT REAL(C,P)
C
C     (* CONSTANTS THAT PARAMETRISE MISSED MUON MAP *)
      DATA COST1, COST2, COST3, COST4/-0.89,-0.82,-0.65,-0.62/
      DATA COST5, COST6, COST7, COST8/-0.58,-0.04,+0.12,+0.53/
      DATA COST9,COST10,COST11,COST12/+0.60,+0.67,+0.80,+0.89/
      DATA PHI1, PHI2, PHI3, PHI4/-135.0,-120.0,-103.0,-103.0/
      DATA PHI5, PHI6, PHI7, PHI8/-74.0, -73.0, -56.0, -43.0/
      DATA PHI9,PHI10,PHI11,PHI12/+33.0, +79.0, +99.0,+145.0/
C
C     (* CONE AROUND BEAM PIPE *)
      IF (COSTHE.LE.COST1 .OR. COSTHE.GE.COST12) GOTO 1
C
C     (* GAP BELOW BEAM PIPE AT EACH ENDWALL *)
      IF ((COSTHE.LE.COST3 .AND. PHI.GE.PHI3 .AND. PHI.LE.PHI5).OR.
     *    (COSTHE.GE.COST10.AND. PHI.GE.PHI4 .AND. PHI.LE.PHI6)) GOTO 1
C
C     (* SIDE EDGES OF PLINTH *)
      IF (COSTHE.GE.COST5 .AND. COSTHE.LE.COST8 .AND.
     *    ((PHI.GE.PHI1 .AND. PHI.LE.PHI2).OR.
     *     (PHI.GE.PHI7 .AND. PHI.LE.PHI8))) GOTO 1
C
C     (* GAPS BETWEEN CHAMBERS IN EACH ARCH *)
      IF (PHI.GE.PHI9 .AND. PHI.LE.PHI12 .AND.
     *    COSTHE.GE.COST6 .AND. COSTHE.LE.COST7) GOTO 1
C
C     (* GAP BETWEEN ARCHES *)
      IF (PHI.GE.PHI10 .AND. PHI.LE.PHI11) GOTO 1
C
C     (* END EDGES OF ROOF *)
      IF (PHI.LE.PHI12 .AND.
     *    PHI.LE.(PHI12-PHI11)/(COST2+1)*(COSTHE+1)+PHI11 .AND.
     *    PHI.GE.(PHI12-PHI10)/(COST4-COST1)*(COSTHE-COST1)+PHI10)GOTO 1
      IF (PHI.GE.PHI9 .AND.
     *    PHI.GE.(PHI9-PHI10)/(COST2+1)*(COSTHE+1)+PHI10 .AND.
     *    PHI.LE.(PHI9-PHI11)/(COST4-COST1)*(COSTHE-COST1)+PHI11) GOTO 1
      IF (PHI.LE.PHI12 .AND.
     *    PHI.LE.(PHI11-PHI12)/(1-COST11)*(COSTHE-1)+PHI11 .AND.
     *    PHI.GE.(PHI10-PHI12)/(COST12-COST9)*(COSTHE-COST12)+PHI10)
     *       GOTO 1
      IF (PHI.GE.PHI9 .AND.
     *    PHI.GE.(PHI10-PHI9)/(1-COST11)*(COSTHE-1)+PHI10 .AND.
     *    PHI.LE.(PHI11-PHI9)/(COST12-COST9)*(COSTHE-COST12)+PHI11)
     *       GOTO 1
C
C     (* IF NONE OF ABOVE CONDITIONS TRUE *)
      MUSAFE=0
      RETURN
C
C     (* IF ANY OF ABOVE CONDITIONS TRUE *)
    1 MUSAFE=2
      RETURN
      END
