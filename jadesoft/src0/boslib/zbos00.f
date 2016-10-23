C   21/11/78 C9071801   MEMBER NAME  ZBOS00   (S)           FORTRAN
      SUBROUTINE BSCOM
C
C               BBBBBB      OOOOO      SSSSS
C               B     B    O     O    S     S
C               B     B    O     O    S
C               BBBBBB     O     O     SSSSS
C               B     B    O     O          S
C               B     B    O     O    S     S
C               BBBBBB      OOOOO      SSSSS
C
C               ======== M A N U A L ========
C
C
C     -----------------------------------------------------
C     BANK ORGANISATION SYSTEM                      - BOS -
C     -----------------------------------------------------
C           DYNAMIC STORAGE ORGANISATION WITH FORTRAN
C                        1979 VERSION
C
C
C
C
C     AUTHOR  -  V.BLOBEL
C                II.INSTITUT FUER EXP.PHYSIK
C                UNIVERSITY OF HAMBURG   AND   DESY
C
C
C
C
C             ABSTRACT
C             ========
C             THE BOS SYSTEM AND ITS SUBSYSTEM SBOS
C             ALLOW THE EFFICIENT MANAGEMENT OF DATA
C             ARRAYS IN MEMORY AND ON MASS STORAGE
C             DEVICES. THE SYSTEMS ARE DEVELOPPED
C             ESPECIALLY FOR THE USE IN THE ANALYSIS
C             OF HIGH ENERGY PHYSICS EXPERIMENTS.
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C
C         TABLE OF CONTENT                             PAGE
C     -----------------------------------------------------
C
C
C
C     1.  INTRODUCTION                                    4
C
C     2.  CREATION OF BANKS                               9
C         2.1 CREATION OF A BANK
C         2.2 CHANGING THE LENGTH OF A BANK
C         2.3 STORAGE OF DATA IN A BANK
C         2.4 RENAMING A BANK
C         2.5 CALL OF THE PREVIOUS VERSION
C
C     3.  LOCATING OF BANKS                              12
C         3.1 LOCATING A BANK
C         3.2 LOCATING ALL BANKS OF THE SAME NAME
C         3.3 FASTER METHODS
C
C     4.  SETS OF BANKS                                  14
C         4.1 THE CURRENT LIST
C         4.2 THE SPECIAL LIST
C
C     5.  PRINTOUT OF BANKS                              18
C         5.1 SINGLE BANKS
C         5.2 SETS OF BANKS
C
C     6.  DELETING BANKS AND GARBAGE COLLECTION          18
C         6.1 SINGLE BANKS
C         6.2 SETS OF BANKS
C         6.3 GARBAGE COLLECTION
C
C     7.  INPUT FROM CARDS                               19
C         7.1 FREE FORMAT
C         7.2 FORMATED
C         7.3 SPECIAL CARDS
C         7.4 EXAMPLE
C
C     8.  UNFORMATED INPUT/OUTPUT OF BANKS               21
C         8.1 STANDARD OUTPUT AND INPUT
C         8.2 OUTPUT MODES AND RECORD TYPES
C         8.3 USER WRITE AND READ STATEMENTS
C         8.4 BUFFER BANKS
C         8.5 STANDARD EVENT READING/WRITING
C
C
C
C
C
C     9.  BANKS ON DIRECT ACCESS DATA SETS               27
C         9.1 INITIALIZATION OF A DADS
C         9.2 STORING AND RETRIEVING BANKS
C         9.3 UNLOADING AND LOADING THE DADS
C         9.4 PROTECTED DADS
C
C     10. PRINTING STATISTIC AND A DUMP                  30
C
C
C     APPENDIX A - THE SBOS SYSTEM                       34
C
C
C         A.1  INITIALIZATION
C         A.2  FUNCTION ILOC
C         A.3  FUNCTION IBLN
C         A.4  SUBROUTINE BMLT
C         A.5  SUBROUTINE BLST
C         A.6  SUBROUTINE BDLM
C         A.7  SUBROUTINE BGAR
C         A.8  SUBROUTINE BSIN
C         A.9  SUBROUTINE BSOUT
C
C
C     APPENDIX B - APPLICATION PROGRAMS                  40
C
C
C         B.1  PRINTOUT AND COMMENTS
C         B.2  COUNT IN A 32768 * 32768 ARRAY
C         B.3  COUNT IN A 256 * 16 ARRAY
C         B.4  HISTOGRAM
C         B.5  CORRELATION PLOT
C         B.6  STORE VECTORS
C         B.7  DETERMINE MEAN VALUES
C         B.8  DETERMINE PEAK VALUES
C         B.9  STORE SINGLE VALUES
C         B.10 BIN DEFINITION
C         B.11 INTERPOLATION OF TABULATED DATA
C         B.12 DETERMINATION OF A LINEAR DEPENDENCE
C         B.13 DETERMINATION OF AN EMPIRICAL FUNCTION
C
C
C
C
C
C
C
C
C
C
C
C
C
C
      RETURN
      END
