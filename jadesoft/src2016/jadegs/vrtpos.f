C   06/11/81 203091007  MEMBER NAME  VRTPOS   (JADEGS)      FORTRAN
      SUBROUTINE VRTPOS(NRUN,VX,VY,WV)
      VX = -.21
      VY = -.30
      WV = 2.20
      IF(NRUN.LT.3060) RETURN
      VX = -1.95
      VY = -.26
      WV = 2.20
      IF(NRUN.LT.3300) RETURN
      VX = .51
      VY = .49
      WV = 2.80
      IF(NRUN.LT.3540) RETURN
      VX = .53
      VY = .65
      WV = 2.40
      IF(NRUN.LT.3750) RETURN
      VX = 1.44
      VY = .43
      WV = 2.60
      IF(NRUN.LT.3960) RETURN
      VX = .17
      VY = .14
      WV = 2.30
      IF(NRUN.LT.4140) RETURN
      VX = -1.40
      VY = .50
      WV = 2.30
      IF(NRUN.LT.4410) RETURN
      VX = .68
      VY = .18
      WV = 2.20
      IF(NRUN.LT.4920) RETURN
      VX = .76
      VY = .92
      WV = 2.40
      IF(NRUN.LT.5310) RETURN
      VX = -.44
      VY = 1.16
      WV = 2.50
      IF(NRUN.LT.5668) RETURN
      VX = -1.80
      VY =  .10
      WV = 2.50
      IF(NRUN.LT.6340) RETURN
      VX = -3.00
      VY = -.10
      WV = 2.50
      IF(NRUN.LT.6500) RETURN
      VX = -2.20
      VY = -.10
      WV = 2.50
      IF(NRUN.LT.6590) RETURN
      VX = -3.00
      VY = -.10
      WV = 2.50
      IF(NRUN.LT.6760) RETURN
      VX = -2.60
      VY = -.10
      WV = 2.50
      IF(NRUN.LT.6820) RETURN
      VX = -3.20
      VY = -.10
      WV = 2.50
      IF(NRUN.LT.7200) RETURN
      VX = -3.60
      VY = -.30
      WV = 2.50
      IF(NRUN.LT.7370) RETURN
      VX = -1.80
      VY = -.300
      WV = 2.50
      IF(NRUN.LT.7592) RETURN
      VX = -1.6
      VY = -.5
      WV = 10.
      IF(NRUN.LT.8897) RETURN
      VX = 0.
      VY = 0.
      WV = 10.
      RETURN
      END
