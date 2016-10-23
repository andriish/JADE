C   11/01/88 801172315  MEMBER NAME  TABLES   (S)        M  MACROFOR

C                            Common for TP options tables
C                            NTABLE contains the option names like MUON
C                            OTBALE conatins the option keywords like TP
C                            ATABLE conatins the TRUE/FALSE settings
C                            There are NOG entries in NTABLE
C                            and NOK entries in OTABLE when set up.

      PARAMETER  ( NOPTG = 10, NOPTS = 30 )

      CHARACTER  NTABLE( NOPTG )*8,  OTABLE( NOPTS )*20

      LOGICAL    ATABLE( NOPTG, NOPTS )

      INTEGER  NOG, NOK

      COMMON / CTPOP1 / NTABLE, OTABLE
      COMMON / CTPOP2 / ATABLE
      COMMON / CTPOP3 / NOG, NOK


