C -*- mode: fortran -*-
C  Dimension parameters and values for cone and Cambridge jet finder:
      integer maxtrk,maxjet,nbc
      parameter( maxtrk=501, nbc=7, maxjet=31 )
      real xcr(nbc), xce(nbc)
      data xcr / 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5 /
      data xce / 2.0, 6.0, 10.0, 14.0, 18.0, 22.0, 25.5 /
      integer ijade, idurhm, icambr
      parameter( ijade=1, idurhm=5, icambr=7 )
C  Bins for fragmentation function histo. (22 non-equal bins)
c      integer nbff
c      parameter( nbff=22 )
c      real xbinff(nbff+1)
c      data xbinff / 0.0000, 0.0100, 0.0200, 0.0300, 0.0400, 0.0500
c     &     ,0.0600, 0.0700, 0.0800, 0.0900, 0.1000, 0.1200, 0.1400
c     &     ,0.1600, 0.1800, 0.2000, 0.2500, 0.3000, 0.4000, 0.5000
c     &     ,0.6000, 0.8000, 1.000 /
C  Ntuple booking character strings:
      character*512 chdagn, chjdct, chdaev, chdaan, chdajt
     &     , chdact, chdacl
     &     , chmcgn, chmcev, chmcan, chmcjt, chmcpt, chmcct
      common /qqchar/ chdagn, chjdct, chdaev, chdaan, chdajt
     &     , chdact, chdacl
     &     , chmcgn, chmcev, chmcan, chmcjt, chmcpt, chmcct
      save /qqchar/
C  Detector level:
      integer irun,ievnt,itkmh,igpmh,isist,icvst,icjst,iczst,iebst
     &       ,ieest,istg1,ntkd02
      real ebeam,pgce(4),tvectc(3)
      common /qqdagn/
     &      irun,ievnt,itkmh,igpmh,isist,icvst,icjst,iczst,iebst
     &     ,ieest,istg1,ntkd02
     &     ,ebeam,pgce,tvectc
      save /qqdagn/
      integer ino, inttr, iltr, itauct, imcred, njdct(2)
      real eb, ee(2), zvert
      common /jdcuts/ ino, inttr, iltr, itauct, imcred, njdct
     &     , eb, ee, zvert
      save /jdcuts/
      real tdtc,tmadtc,tmidtc,mhdtc,mldtc,btdtc,bwdtc,cpdtc,dpdtc,sdtc
     &    ,adtc,acpdtc
     &    ,tdt,tmadt,tmidt,mhdt,mldt,btdt,bwdt,cpdt,dpdt,sdt,adt,acpdt
     &    ,tdc,tmadc,tmidc,mhdc,mldc,btdc,bwdc,cpdc,dpdc,sdc,adc,acpdc
     &    ,tdmt,tmadmt,tmidmt,mhdmt,mldmt,btdmt,bwdmt,cpdmt,dpdmt,sdmt
     &    ,admt,acpdmt
      common /qqdaev/
     &      tdtc,tmadtc,tmidtc,mhdtc,mldtc,btdtc,bwdtc,cpdtc,dpdtc,sdtc
     &     ,adtc,acpdtc
     &     ,tdt,tmadt,tmidt,mhdt,mldt,btdt,bwdt,cpdt,dpdt,sdt,adt,acpdt
     &     ,tdc,tmadc,tmidc,mhdc,mldc,btdc,bwdc,cpdc,dpdc,sdc,adc,acpdc
     &     ,tdmt,tmadmt,tmidmt,mhdmt,mldmt,btdmt,bwdmt,cpdmt,dpdmt,sdmt
     &     ,admt,acpdmt
      save /qqdaev/
      real  bzdtc,kswdtc,nrdtc,c34dtc
     &     ,bzdt,kswdt,nrdt,c34dt
     &     ,bzdc,kswdc,nrdc,c34dc
     &     ,bzdmt,kswdmt,nrdmt,c34dmt
      common /qqdaan/
     &      bzdtc,kswdtc,nrdtc,c34dtc
     &     ,bzdt,kswdt,nrdt,c34dt
     &     ,bzdc,kswdc,nrdc,c34dc
     &     ,bzdmt,kswdmt,nrdmt,c34dmt
      save /qqdaan/
      integer nxjdtc,nxjdt,nxjdc,nxjdmt
      integer nxjetc,nxjet,nxjec,nxjemt
      integer nxjctc,nxjct,nxjcc,nxjcmt
      real yddtc(maxjet),yddt(maxjet),yddc(maxjet),yddmt(maxjet)
     &    ,yedtc(maxjet),yedt(maxjet),yedc(maxjet),yedmt(maxjet)
     &    ,ycdtc(maxjet),ycdt(maxjet),ycdc(maxjet),ycdmt(maxjet)
      integer njcedtc(nbc),njcrdtc(nbc),njcedt(nbc),njcrdt(nbc)
     &       ,njcedc(nbc),njcrdc(nbc),njcedmt(nbc),njcrdmt(nbc)
      common /qqdajt/
     &      nxjdtc,nxjdt,nxjdc,nxjdmt
     &     ,nxjetc,nxjet,nxjec,nxjemt
     &     ,nxjctc,nxjct,nxjcc,nxjcmt
     &     ,yddtc,yedtc,ycdtc,njcedtc,njcrdtc
     &     ,yddt,yedt,ycdt,njcedt,njcrdt
     &     ,yddc,yedc,ycdc,njcedc,njcrdc
     &     ,yddmt,yedmt,ycdmt,njcedmt,njcrdmt
      save /qqdajt/
      integer ntrk,id02(maxtrk),nhde(maxtrk),nhcj(maxtrk),ichg(maxtrk)
     &     ,nmttrk,imttrk(maxtrk)
      real dedx(maxtrk),dded(maxtrk),dp(maxtrk)
     &    ,ptrk(3,maxtrk),z0(maxtrk),d0(maxtrk),mtscft(maxtrk)
      common /qqdatk/
     &      ntrk,id02,dedx,dded,nhde,dp
     &     ,ptrk,ichg,nhcj,z0,d0
     &     ,nmttrk,imttrk,mtscft
      save /qqdatk/
      integer nclus,nmtcls,imtcls(maxtrk),nmtkil,imtkil(maxtrk)
      real pclus(3,maxtrk),mtscfc(maxtrk)
      common /qqdacl/
     &     nclus,nmtcls,imtcls,nmtkil,imtkil,pclus,mtscfc
      save /qqdacl/
C  MC hadron and parton-level:
      integer maxprt,maxprf
      parameter( maxprt=50, maxprf=4 )
      integer ievtyp,inonr,iferid(maxprf),nprimf
      real pisr(4),primf(4,maxprf)
      common /qqmcgn/
     &     ievtyp,inonr,pisr,nprimf,iferid,primf
      save /qqmcgn/
      real tp,tmap,tmip,mhp,mlp,btp,bwp,cpp,dpp,sp,ap,acpp
     &    ,th,tmah,tmih,mhh,mlh,bth,bwh,cph,dph,sh,ah,acph
      common /qqmcev/
     &     tp,tmap,tmip,mhp,mlp,btp,bwp,cpp,dpp,sp,ap,acpp
     &    ,th,tmah,tmih,mhh,mlh,bth,bwh,cph,dph,sh,ah,acph
      save /qqmcev/
      real bzp,kswp,nrp,c34p,bzh,kswh,nrh,c34h
      common /qqmcan/
     &     bzp,kswp,nrp,c34p,bzh,kswh,nrh,c34h
C WHAT IS THAT?
C      save /qqmvan/
C
      integer nxjdp,nxjdh
      integer nxjep,nxjeh
      integer nxjcp,nxjch
      real ydp(maxjet),yep(maxjet),ydh(maxjet),yeh(maxjet)
      real ycp(maxjet),ych(maxjet)
      integer njcep(nbc),njcrp(nbc),njceh(nbc),njcrh(nbc)
      common /qqmcjt/
     &     nxjdp,nxjdh,nxjep,nxjeh
     &    ,nxjcp,nxjch
     &    ,ydp,yep,ycp,njcep,njcrp
     &    ,ydh,yeh,ych,njceh,njcrh
      save /qqmcjt/
      integer ntrkp,ilucp(maxprt)
      real ptrkp(4,maxprt)
      common /qqpart/ ntrkp,ilucp,ptrkp
      integer ntrkh,ntrk2
      integer iluch(maxtrk),iluc(maxtrk),istrt(maxtrk),ichgh(maxtrk)
      real ptrkh(4,maxtrk)
      common /qqmchd/
     &      ntrkh,ntrk2,ptrkh
     &     ,iluch,iluc,istrt,ichgh
      save /qqmchd/
