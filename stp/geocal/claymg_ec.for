      SUBROUTINE CLAYMG_EC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creating the Layer Bank for the End
C-       Calorimeter Massless Gap.  The CLAY bank is supported
C-       by the CMSG Massless Gap module bank.  This bank contains
C-       the physical pad segmentation.
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-   Zebra Banks Created:    CLAY, CSHA
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
      INCLUDE 'D0$INC:CLAY.DEF'
      INCLUDE 'D0$PARAMS:CMAT.PARAMS'
      INCLUDE 'D0$LINKS:IZCMAT.LINK'
      INCLUDE 'D0$LINKS:IZCMSG.LINK'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCREG.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
C
      INTEGER LZFIND, LUNIT, LQCMSG, MCLAY(5)
      EQUIVALENCE (LQCMSG, LQCLGA)
C
      CHARACTER*4 CHAR4
      EQUIVALENCE (CHAR4,MCLAY(1))
      DATA MCLAY / 0, 4, 0, 28, 9 /
      DATA CHAR4 / 'CLAY'/
      DATA LUNIT / 0 /
C
      LQCREG = LZFIND(IDVSTP, LC(LCGEH-IZCREG), ISECAL, IGREGN)
      LQCMSG = LZFIND(IDVSTP, LC(LQCREG-IZCMSG), ICMHMG, IGIDEN)
      CALL MZFORM('CLAY','6I9F2I-F',IOCLAY)
      MCLAY(5) = IOCLAY
C
      CALL MZLIFT(IDVSTP, LQCLAY, LQCMSG, -IZCLAY, MCLAY, 0)
      IC(LQCLAY+ILIDEN) = ICMHMG       ! layer ID
      IC(LQCLAY+ILETPH) = 1            ! eta-phi zones on plate
      IC(LQCLAY+ILNPLA) = 1            ! number of plate cells
      IC(LQCLAY+ILCOOR) = 123          ! coordinate code
      IC(LQCLAY+ILPERP) = 3            ! perpendicular coordinate
      C(LQCLAY+ILDPHI) = TWOPI/64      ! phi increment
      CALL FLRSRC(LQCLAY,'NONE','EC_MHG+1','NONE')
      CALL ECPAD(LQCLAY,'MHG_DIVISIONS+Z','MHG+')
      CALL SBIT1(IC(LQCLAY),IBZRFL)
C
      LC(LQCLAY-IZLSHA) = LC(LQCMSG-IXCSHA)      ! link to CSHA -- same
C                                       ! as from CMSG
      LC(LQCLAY-IZLMAT) = LQCMAT        ! link to CMAT
      LC(LQCLAY-IZLLGA) = LQCMSG        ! link to CMSG
      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK        ! link to CLNK
C
      IF( LUNIT .EQ. 0 ) CALL PRCLAY( LUNIT, LQCLAY)
C
      LQCMSG = LZFIND(IDVSTP, LC(LQCREG-IZCMSG), ICOHMG, IGIDEN)
C
      CALL MZLIFT(IDVSTP, LQCLAY, LQCMSG, -IZCLAY, MCLAY, 0)
      IC(LQCLAY+ILIDEN) = ICOHMG       ! layer ID
      IC(LQCLAY+ILETPH) = 1            ! eta-phi zones on plate
      IC(LQCLAY+ILNPLA) = 1            ! number of plate cells
      IC(LQCLAY+ILCOOR) = 123          ! coordinate code
      IC(LQCLAY+ILPERP) = 9            ! perpendicular coordinate
      C(LQCLAY+ILDPHI) = TWOPI/64      ! phi increment
      CALL FLRSRC(LQCLAY,'NONE','EC_OHG+1','NONE')
      CALL ECPAD(LQCLAY,'OHG_DIVISIONS+Z','OHG+')
      CALL SBIT1(IC(LQCLAY),IBZRFL)
C
      LC(LQCLAY-IZLSHA) = LC(LQCMSG-IXCSHA)      ! link to CSHA -- same
C                                       ! as from CMSG
      LQCMAT = LZFIND(IDVSTP,LC(LCGEH-IZCMAT),IC(LQCLAY+ILMATE),1)
      LQCLNK=LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IC(LQCLGA+IGIDEN),IGIDEN)
      LC(LQCLAY-IZLLNK) = LQCLNK        ! link to CLNK
      LC(LQCLAY-IZLMAT) = LQCMAT        ! link to CMAT
      LC(LQCLAY-IZLLGA) = LQCMSG        ! link to CMSG
C
      IF( LUNIT .EQ. 0 ) CALL PRCLAY( LUNIT, LQCLAY)
C----------------------------------------------------------------------
  999 RETURN
      END
