C--------------START OF MACRO CMUFRCH-----------------------------------
C
C /CMUNIT/,/CMFFIX/,/CMFSUR/,/CMCFIX/,/CMCSUR/,/CMCELE/ AND /CMCSTA/ ARE
C   DESCRIBED ON 'F22ALL.JADEMUS(@MUINFOM)'. THEY CAN BE READ FROM
C   A OS DATA SET (PRESENTLY 'F22ALL.MUSQUEEZ.DATA'), OR FROM
C   THE APPROPRIATE MU CALIBRATION DATA BOS RECORD, E.G.,
C   'F22ALL.MUCALIB.DATA0001'.
C
      COMMON /CMUCDV/NVERSN,DESCRP(15)
C
      COMMON /CMUNIT/HOVALL(6)
C
      COMMON/CMFFIX/HMFFIX(740)
      DIMENSION HFACE(82),HSECT(82),HLAYER(82),HNORM(82),HLONG(82),
     *          HTRANS(82),HAC(82),HAL(82),HUNIT(82)
      EQUIVALENCE (HMFFIX(1),NFRAMS),(HMFFIX(3),HFACE(1)),
     *            (HMFFIX(85),HSECT(1)),(HMFFIX(167),HLAYER(1)),
     *            (HMFFIX(249),HNORM(1)),(HMFFIX(331),HLONG(1)),
     *            (HMFFIX(413),HTRANS(1)),(HMFFIX(495),HAC(1)),
     *            (HMFFIX(577),HAL(1)),(HMFFIX(659),HUNIT(1))
C
      COMMON/CMFSUR/HMFSUR(492)
      DIMENSION HDIST(82),HANG(82),HCLLO(82),HCLHI(82),HCTLO(82),
     *          HCTHI(82)
      EQUIVALENCE (HMFSUR(1),HDIST(1)),(HMFSUR(83),HANG(1)),
     *            (HMFSUR(165),HCLLO(1)),(HMFSUR(247),HCLHI(1)),
     *            (HMFSUR(329),HCTLO(1)),(HMFSUR(411),HCTHI(1))
C
      COMMON/CMCFIX/HMCFIX(636)
      DIMENSION HFR(634)
      EQUIVALENCE (HMCFIX(1),NCHAMS),(HMCFIX(3),HFR(1))
C
      COMMON/CMCSUR/HMCSUR(1268)
      DIMENSION HD1(634),HCTW(634)
      EQUIVALENCE (HMCSUR(1),HCTW(1)),(HMCSUR(635),HD1(1))
C
      COMMON/CMCELE/HMCELE(4440)
      DIMENSION HDTP(634),HLTP(634),HLSF(4,634),HVDRFT(634)
      EQUIVALENCE (HMCELE(1),HVDR),(HMCELE(2),HDTP(1)),
     *            (HMCELE(636),HLTP(1)),(HMCELE(1270),HLSF(1,1)),
     *            (HMCELE(3806),HMCEDM),(HMCELE(3807),HVDRFT(1))
C
      COMMON/CMCSTA/HMCSTA(634)
C
C---------------END OF MACRO CMUFRCH------------------------------------
