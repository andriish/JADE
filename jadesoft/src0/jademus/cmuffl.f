C   20/10/81 408081453  MEMBER NAME  CMUFFL   (S)           FORTRAN
C
C LAST CHANGE 15.00 08/08/84 - CKB PWA     - ADD NALLWM,PMXFLG
C      CHANGE 15.00 03/12/83 - HUGH MCCANN - NEW LEAD GLASS PARAMETERS.
C      CHANGE 08.00 12/10/81 - HUGH MCCANN - TO ADD OVLCUT(FOR OVERLAPS)
C      CHANGE 09.33 17/06/81 - JOHN ALLISON - TO ADD RESOLUTIONS.
C
C-------------START OF MACRO CMUFFL-------------------------------------
C
      COMMON / CMUFFL / XYSTEP,RCOILI,RCOILO,PGMID,WMU,WMUSQ,DUMF1(4),
     +                  NSPECI,XPBAR,XPROT,XPION,XKAON,DUMF2(5),
     +                  TBP,GMBP,ABBP,RDBP,DEBP,
     +                  TJETI,GMJETI,ABJETI,RDJETI,DEJETI,
     +                  TJET1,GMJET1,ABJET1,RDJET1,DEJET1,
     +                  TJET2,GMJET2,ABJET2,RDJET2,DEJET2,
     +                  TJET4,GMJET4,ABJET4,RDJET4,DEJET4,
     +                  TJETO,GMJETO,ABJETO,RDJETO,DEJETO,
     +                  TCOIL,GMCOIL,ABCOIL,RDCOIL,DECOIL,
     +                  ZJETE,TJETE,GMJETE,ABJETE,RDJETE,DEJETE,
     +                  ZEPLG,TEPLG,GMEPLG,ABEPLG,RDEPLG,DEEPLG,
     +                  RLG,ZLG,TLG,GMLG,ABLG,RDLG,DELG,
     +                  ZEP,TEP,GMEP,ABEP,RDEP,DEEP,
     +                  GMG(3),ABG(3),RDG(3),DEG(3),
     +                  VDRES,VLRES,
     +                  REST1,REST2,RESTMX,RESL,RESLMX,FACTOR,OVLCUT,
     +                  ZLGHAF,TLGHAF,ZLGC,GMLGC,ABLGC,RDLGC,DELGC,
     +                  NALLWM,PMXFLG
C
C FOR EXPLANATION SEE BLOCK DATA AFTER SUBROUTINE MUFFLE.
C
C--------------END OF MACRO CMUFFL--------------------------------------
