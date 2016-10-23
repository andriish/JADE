C-------------------------------
C  MACRO CHEADR .... HEADER BANK
C-------------------------------
      COMMON /CHEADR/ IHEADR(54)
      INTEGER*2 HHEADR(108)
      EQUIVALENCE (IHEADR(1),HHEADR(1))
C --
C --  HHEADR(17) = EXPERIMENT NUMBER
C --  HHEADR(18) = RUN NUMBER
C --  HHEADR(19) = EVENT NUMBER
C --  HHEADR(38) = MAGNETIC FIELD (GAUSS)
C --
C--------- END OF MACRO CHEADR ------------
