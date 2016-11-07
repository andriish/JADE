      subroutine JADEPR
C
C Print out event in readable format
C
#include "xdata.inc"

C General part
      WRITE(*,5900) IXREVN
     +     ,IXRUN,IXEVENT
     +     ,XEBEAM,IXMCR,XEVCH,XEVTOT
     +     ,IXNVER,IXNPAR,IXQFL
      WRITE(*,5910)
     +     'Thrust',XTHR,XTHRX,XTHRY,XTHRZ
     +     ,'Spher1',XSPH1,XSPH1X,XSPH1Y,XSPH1Z
     +     ,'Spher2',XSPH2,XSPH2X,XSPH2Y,XSPH2Z
     +     ,'Spher3',XSPH3,XSPH3X,XSPH3Y,XSPH3Z
     +     ,'Aco',XACO,XACOX,XACOY,XACOZ
      IF( IXNPAR.GT.0 ) THEN
         WRITE(*,5904)
         DO I=1,IXNPAR
            WRITE(*,5905) I,XPPX(I),XPPY(I),XPPZ(I),XPP(I) 
         ENDDO
      ENDIF
      WRITE(*,5902) IXNP
C Particle section
      I1=0
      I2=0
      DO I=1, IXNP
         
         IF( IXPTF(I) .EQ. 1 ) THEN
            I1=I1+1
            IF( I1.EQ.1 ) WRITE(*, 6001)
            WRITE(*,6010)
C    ... general
     +           IXRPN(I)
     +           ,IXPATR(I),IXPALL(I),IXTPTR(I)
     +           ,IXVN(I),IXV2N(I)
     +           ,IXEO(I),IXPC(I),IXPTF(I)
     +           ,XEX(I),XEY(I),XEZ(I),XP(I),XCH(I)
C    ... tracks
     +           ,XECL(I),XECLE(I),XECLC(I)
     +           ,IXDFLG(I),IXCLN(I),IXCTRN(I)
     +           ,INT(IXHITS(I)/100),MOD(IXHITS(I),100),IXMUQ(I)
     +           ,XRMIN(I),XDX(I),XDY(I),XDZ(I)
     +           ,XDEDX(I),XDEDXS(I)
         ELSE IF( IXPTF(I) .EQ. 0 ) THEN
            I2=I2+1
            IF( I2.EQ.1 ) WRITE(*, 6002)
            WRITE(*,6011)
C    ... general
     +           IXRPN(I)
     +           ,IXPATR(I),IXPALL(I),IXTPTR(I)
     +           ,IXVN(I),IXV2N(I)
     +           ,IXEO(I),IXPC(I),IXPTF(I)
     +           ,XEX(I),XEY(I),XEZ(I),XP(I),XCH(I)
C    ... clusters
     +           ,XECL(I),XECLE(I),XECLC(I)
     +           ,IXLGP(I),IXCLN(I),IXNBL1(I),IXNBL2(I)
         ENDIF
      ENDDO
C
 5900 FORMAT ('JADEPR:',/,T2
     +     ,58('#'),'   JADE EVENT RECORD ',I7,'    ',58('#')
     +     ,/,' Run=',I6,' Event=',I6,' Ebeam=',F6.2
     +     ,' MCREDU=',I3,' Evis(chrgd)= ',F5.2,' Evis(tot)= ',F5.2
     +     ,/,T4,'#Vertices=',I3,' #Partons=',I3,' Quarkflavour=',I3)
 5910 FORMAT (T4,A6,'= ',F6.4,4X,'x= ',F6.4,4X,'y= ',F6.4,4X,'z= ',F6.4)
 5904 FORMAT(T2,'Parton#      px      py      pz     |p|')
 5905 FORMAT(T2,I7,4F8.4)
 5902 FORMAT(T2,'#Particles=',I3)
 6001 FORMAT('  # PAT PAL TPT vt1 vt2 eor  pc  pt'
     +     ,'     ex     ey     ez    |p|  Q'
     +     ,'   E(cl)     +-    cor '  
     +     ,' det c# t# rf rz mu'
     +     ,'   Rmin   docx   docy   docz   dEdx     +-')
 6002 FORMAT(T91,' LG c# #b1 #b2')
 6010 FORMAT(T2,I2,8I4,4F7.3,F4.0,3F7.3,I5,4I3,I3,6F7.3)
 6011 FORMAT(T2,I2,8I4,4F7.3,F4.0,3F7.3,I5,3I3)

 6900 FORMAT(' - - - - - ',I2
     +     ,/T5,'General:',' PATR#',I2,' PALL#',I2,' TPTR#',I2
     +     ,' vtx#',I2,' vtx(2nd)#',I2
     +     ,' e-ori:',I3,' p-code:',I2,' p-type:',I2
     +     ,/T13,' ex=',F6.4,' ey=',F6.4,' ez=',F6.4
     +     ,' |p|=',F7.3,' Q=',F4.1)
 6901 FORMAT(T5,'Track:  '
     +     ,' det:',I4,' cl#:',I2,' tr#:',I2
     +     ,' #rphi:',I2,' #rz:',I2,' Mu:',I2
     +     ,' Rmin=',F7.3,' dx=',F7.3,' dy=',F7.3,' dz=',F7.3
     +     ,/T13,' E=',F7.3,' +-',F7.3,' cor=',F7.3
     +     ,' dEdx=',F7.4,' +-',F7.4)
 6902 FORMAT(T5,'Ecal:   '
     +     ,' LG:',I4,' Cl#',I2,' #Bl1:',I2,' #Bl2:',I2
     +     ,' E=',F7.3,' +-',F7.3,' cor=',F7.3)
C      
      RETURN
      END
