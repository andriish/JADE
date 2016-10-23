C   01/11/84 807251742  MEMBER NAME  NOTICE   (S)        M  FORTRAN
C
C-----------------------------------------------------------------------
      SUBROUTINE NOTICE
C-----------------------------------------------------------------------
C
C        SUBROUTINE TO WRITE LATEST NEWS OF INTEREST TO GRAPHICS USERS.
C        NOTICE IS CALLED BY GPHMAIN.
C
C-----------------------------------------------------------------------
C
      CALL TRMOUT(80,'* * * * * * * * * * * * * * * * * * * * * * * * *
     $* * * * * * * * * * * * * * ^')
C
      CALL TRMOUT(80,'*  Command HLP 1, HLP 2 etc.., for steering output
     $ to destination L1,L2,..  * ^')
C
      CALL TRMOUT(80,'*  New Command ZFIT n m available for comparison o
     $f different z-fits...     * ^')
C
      CALL TRMOUT(80,'* * * * * * * * * * * * * * * * * * * * * * * * *
     $* * * * * * * * * * * * * * ^')
C
      RETURN
      END
