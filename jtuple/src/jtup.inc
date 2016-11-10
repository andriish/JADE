*********************************************************
*                                                       *
* This file was generated by HUWFUN.                    *
*                                                       *
*********************************************************
*
*     Ntuple Id:      10   
*     Ntuple Title:   QQJT200
*     Creation:       07/04/2005 15.04.19
*
*********************************************************
*     
      LOGICAL         CHAIN
      CHARACTER*128   CFILE
      INTEGER         IDNEVT,NCHEVT,ICHEVT
      REAL            OBS(13)
*
      COMMON /PAWIDN/ IDNEVT,OBS
      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
      COMMON /PAWCHC/ CFILE
*
*--   Ntuple Variable Declarations
*
      REAL EBEAM,PGCE(4),TVECTC(3),EB,EE(2),ZVERT,TDTC,TMADTC,TMIDTC
     + ,MHDTC,MLDTC,BTDTC,BWDTC,CPDTC,DPDTC,SDTC,ADTC,ACPDTC,TDT,TMADT
     + ,TMIDT,MHDT,MLDT,BTDT,BWDT,CPDT,DPDT,SDT,ADT,ACPDT,TDC,TMADC
     + ,TMIDC,MHDC,MLDC,BTDC,BWDC,CPDC,DPDC,SDC,ADC,ACPDC,TDMT,TMADMT
     + ,TMIDMT,MHDMT,MLDMT,BTDMT,BWDMT,CPDMT,DPDMT,SDMT,ADMT,ACPDMT
     + ,YDDTC(31),YEDTC(31),YCDTC(31),YDDT(31),YEDT(31),YCDT(31)
     + ,YDDC(31),YEDC(31),YCDC(31),YDDMT(31),YEDMT(31),YCDMT(31)
     + ,DEDX(501),DDED(501),DP(501),PTRK(3,501),Z0(501),D0(501)
     + ,MTSCFT(501),PCLUS(3,501),MTSCFC(501),PISR(4),PRIMF(4,4),TP,TMAP
     + ,TMIP,MHP,MLP,BTP,BWP,CPP,DPP,SP,AP,ACPP,TH,TMAH,TMIH,MHH,MLH,BTH
     + ,BWH,CPH,DPH,SH,AH,ACPH,YDP(31),YEP(31),YCP(31),YDH(31),YEH(31)
     + ,YCH(31),PTRKP(4,50),PTRKH(4,501)
      INTEGER IRUN,IEVNT,ITKMH,IGPMH,ISIST,ICVST,ICJST,ICZST,IEBST,IEEST
     + ,ISTG1,NTKD02,INO,INTTR,ILTR,ITAUCT,IMCRED,NJDCUT(2),NXJDTC,NXJDT
     + ,NXJDC,NXJDMT,NXJETC,NXJET,NXJEC,NXJEMT,NXJCTC,NXJCT,NXJCC,NXJCMT
     + ,NJCEDTC(7),NJCRDTC(7),NJCEDT(7),NJCRDT(7),NJCEDC(7),NJCRDC(7)
     + ,NJCEDMT(7),NJCRDMT(7),NTRK,ID02(501),NHDE(501),ICHG(501)
     + ,NHCJ(501),NMTTRK,IMTTRK(501),NCLUS,NMTCLS,IMTCLS(501),NMTKIL
     + ,IMTKIL(501),IEVTYP,INONR,NPRIMF,IFERID(4),NXJDP,NXJDH,NXJEP
     + ,NXJEH,NXJCP,NXJCH,NJCEP(7),NJCRP(7),NJCEH(7),NJCRH(7),NTRKP
     + ,ILUCP(50),NTRKH,NTRK2,ILUCH(501),ILUC(501),ISTRT(501),ICHGH(501)
*
      COMMON /PAWCR4/ IRUN,IEVNT,ITKMH,IGPMH,ISIST,ICVST,ICJST,ICZST
     + ,IEBST,IEEST,ISTG1,NTKD02,EBEAM,PGCE,TVECTC,INO,INTTR,ILTR,ITAUCT
     + ,IMCRED,NJDCUT,EB,EE,ZVERT,TDTC,TMADTC,TMIDTC,MHDTC,MLDTC,BTDTC
     + ,BWDTC,CPDTC,DPDTC,SDTC,ADTC,ACPDTC,TDT,TMADT,TMIDT,MHDT,MLDT
     + ,BTDT,BWDT,CPDT,DPDT,SDT,ADT,ACPDT,TDC,TMADC,TMIDC,MHDC,MLDC,BTDC
     + ,BWDC,CPDC,DPDC,SDC,ADC,ACPDC,TDMT,TMADMT,TMIDMT,MHDMT,MLDMT
     + ,BTDMT,BWDMT,CPDMT,DPDMT,SDMT,ADMT,ACPDMT,NXJDTC,NXJDT,NXJDC
     + ,NXJDMT,NXJETC,NXJET,NXJEC,NXJEMT,NXJCTC,NXJCT,NXJCC,NXJCMT,YDDTC
     + ,YEDTC,YCDTC,NJCEDTC,NJCRDTC,YDDT,YEDT,YCDT,NJCEDT,NJCRDT,YDDC
     + ,YEDC,YCDC,NJCEDC,NJCRDC,YDDMT,YEDMT,YCDMT,NJCEDMT,NJCRDMT,NTRK
     + ,ID02,DEDX,DDED,NHDE,DP,PTRK,ICHG,NHCJ,Z0,D0,NMTTRK,IMTTRK,MTSCFT
     + ,NCLUS,NMTCLS,IMTCLS,NMTKIL,IMTKIL,PCLUS,MTSCFC,IEVTYP,INONR,PISR
     + ,NPRIMF,IFERID,PRIMF,TP,TMAP,TMIP,MHP,MLP,BTP,BWP,CPP,DPP,SP,AP
     + ,ACPP,TH,TMAH,TMIH,MHH,MLH,BTH,BWH,CPH,DPH,SH,AH,ACPH,NXJDP,NXJDH
     + ,NXJEP,NXJEH,NXJCP,NXJCH,YDP,YEP,YCP,NJCEP,NJCRP,YDH,YEH,YCH
     + ,NJCEH,NJCRH,NTRKP,ILUCP,PTRKP,NTRKH,NTRK2,PTRKH,ILUCH,ILUC,ISTRT
     + ,ICHGH