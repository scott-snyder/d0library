      SUBROUTINE CELICD(IETA, IPHI, IDEPTH, IGROUP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To Transfer Cell Info for ICD to CLYR
C-       bank.  Note that LQCLAY points to temporary CLYR bank
C-       formed by CLRRCP and LQCLYR will contain permanent CLYR
C-       bank.
C-
C-   Inputs  :     IETA      Physics ETA
C-                 IPHI      Physics PHI
C-                 IDEPTH    Physics LAYER
C-                 IGROUP    sub-cell index 
C-   Outputs :     in Zebra Banks
C-   Controls: 
C-   Zebra Banks Altered:    CLYR
C-
C-   Created   4-OCT-1989   Stephen Kahn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:CLINKS.INC'
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CETA.PARAMS'
      INCLUDE 'D0$PARAMS:CLYR.PARAMS'
      INCLUDE 'D0$PARAMS:CLGA.PARAMS'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$LINKS:IZCLAY.LINK'
      INCLUDE 'D0$LINKS:IZCLNK.LINK'
      INCLUDE 'D0$INC:CLAY.DEF'
C
      INTEGER IETA, IPHI, IDEPTH, IGROUP, NWDS, IERR, JQCLYR
      INTEGER LZFIND, IDEN, MPHI
      REAL PHITOW, DELPHI, R, PHIONE
C
      IF (IDEPTH .NE. LYICD .OR. IGROUP .NE. 1) RETURN     ! ICD only
C
      MPHI = IC(LQCETA + IGMPHI)       ! max phi's per grouping for eta
      JQCLYR = LC(LQCLAY-IZLLNK)       ! CLYR placed temporarily here
      IDEN = IC(LQCLAY + ILIDEN)       ! layer id is same as module id
      NWDS = IC(JQCLYR+ICNPAR) + 8     ! numb of words minus one
      CALL UCOPY(C(JQCLYR+ICX), C(LQCLYR+ICX), NWDS)  ! copy temporary
C                                       ! CLYR to permanent CLYR
      CALL CALPHI(1, IETA, PHIONE, DELPHI,IERR)       ! PHI for IPHI = 1
      CALL CALPHI(IPHI, IETA, PHITOW, DELPHI,IERR)    ! PHI for IPHI
C
      R = SQRT(C(LQCLYR+ICX)**2+C(LQCLYR+ICY)**2)
      C(LQCLYR+ICX) = R*COS(PHITOW)
      C(LQCLYR+ICY) = R*SIN(PHITOW)
      C(LQCLYR+NWDS) = C(LQCLYR+NWDS) + (PHITOW-PHIONE)/RADIAN  ! phi
C                                       ! begin
      C(LQCLYR+NWDS+1) = C(LQCLYR+NWDS+1) + (PHITOW-PHIONE)/RADIAN     
C                                       ! phi end
      IF( IPHI .EQ.MPHI) THEN
        LQCLNK = LZFIND(IDVSTP,LC(LQCREG-IZCLNK),IDEN,IGIDEN)
        LC(LQCLAY - IZLLNK) = LQCLNK     ! link to CLNK put in proper place
        CALL MZDROP(IXSTP, JQCLYR,' ')   ! drop temporary CLYR bank
                                         ! created in CSCN
C ...        end of patch
      END IF
C----------------------------------------------------------------------
  999 RETURN
      END
