      SUBROUTINE CMSGCC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates Module Banks for CC massless gap
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Created :   CMSG, CSHA
C-
C-   Created  20-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:REGION.DEF'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCMSG.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCSHA.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
C
      INTEGER MCMSG(5), MCSHA(5), MCLNK(5), LZFIND, LQCMSG
      EQUIVALENCE (LQCLGA, LQCMSG)
      CHARACTER*4 CHAR4, CHAS4, CHAL4
      INTEGER ICHARR
      CHARACTER*4 CHARR
      EQUIVALENCE (ICHARR, CHARR)
      EQUIVALENCE (CHAR4, MCMSG(1))
      EQUIVALENCE (CHAS4, MCSHA(1))
      EQUIVALENCE (CHAL4, MCLNK(1))
C
      DATA MCMSG / 4HCMSG, 6, 1, 18, 9 /
      DATA MCSHA / 4HCSHA, 0, 0,  6, 9 /
      DATA MCLNK / 4HCLNK, 1, 0,  4, 2 /
      DATA CHAR4 / 'CMSG'/
      DATA CHAS4 / 'CSHA'/
      DATA CHAL4 / 'CLNK'/
C
      LQCREG = LZFIND( IDVSTP, LC(LCGEH-IZCREG), ICCAL, IGREGN)
C
      MCMSG(5) = IOCLGA
C
C     SOUTH CC MASSLESS GAP
C
      CALL MZLIFT(IDVSTP, LQCLNK, LQCREG, -IZCLNK, MCLNK, 0)
      IC(LQCLNK + IGIDEN) = ICCMG
      IC(LQCLNK + IGNSEG) = 1
      IC(LQCLNK + IGJPHI) = 1
      IC(LQCLNK + IGN1) = 0
      CALL MZLIFT(IDVSTP, LQCMSG, LQCREG, -IZCMSG, MCMSG, 0)
      IC(LQCMSG+IGIDEN) = ICCMG        ! massless gap code
      IC(LQCMSG+IGNLAY) = 1
      CHARR = 'CCMG'
      IC(LQCMSG+IGNAM)  = ICHARR 
      IC(LQCMSG+IGCOOR) = 123
      IC(LQCMSG+IGPERP) = 3
      C(LQCMSG +IGDPHI) = TWOPI
      CALL CLGSRC(LQCMSG,'CC_SOUTH_MASSLESS_GAP_VOLUME','NONE')
C
C     PUT IN SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IDVSTP, LQCSHA, LCGEH, -IZCSHA, MCSHA, 0)
      CALL SHPSRC(LQCSHA, 'CC_SOUTH_MASSLESS_GAP_VOLUME')
C
C     CONNECT REFERENCE LINKS
C
      LC(LQCMSG-IXCMAT) = LQCMAT       ! ref link to CMAT
      LC(LQCMSG-IXCSHA) = LQCSHA       ! ref link to CSHA
      LC(LQCLNK-1) = LQCMSG            ! ref link from CLNK
C
C     NORTH CC MASSLESS GAP
C
      CALL MZLIFT(IDVSTP, LQCLNK, LQCREG, -IZCLNK, MCLNK, 0)
      IC(LQCLNK + IGIDEN) = ICCMG
      IC(LQCLNK + IGNSEG) = 1
      IC(LQCLNK + IGJPHI) = 1
      IC(LQCLNK + IGN1) = 0
      CALL MZLIFT(IDVSTP, LQCMSG, LQCREG, -IZCMSG, MCMSG, 0)
      IC(LQCMSG+IGIDEN) = -ICCMG       ! massless gap code
      IC(LQCMSG+IGNLAY) = 1
      CHARR = 'CCMG'
      IC(LQCMSG+IGNAM)  = ICHARR
      IC(LQCMSG+IGCOOR) = 123
      IC(LQCMSG+IGPERP) = 3
      C(LQCMSG +IGDPHI) = TWOPI
      CALL CLGSRC(LQCMSG,'CC_NORTH_MASSLESS_GAP_VOLUME','NONE')
C
C     PUT IN SHAPE INFORMATION
C
      MCSHA(5) = IOCSHA
      CALL MZLIFT(IDVSTP, LQCSHA, LCGEH, -IZCSHA, MCSHA, 0)
      CALL SHPSRC(LQCSHA, 'CC_NORTH_MASSLESS_GAP_VOLUME')
C
C     CONNECT REFERENCE LINKS
C
      LC(LQCMSG-IXCMAT) = LQCMAT       ! ref link to CMAT
      LC(LQCMSG-IXCSHA) = LQCSHA       ! ref link to CSHA
      LC(LQCLNK-1) = LQCMSG            ! ref link from CLNK
C
C----------------------------------------------------------------------
  999 RETURN
      END
