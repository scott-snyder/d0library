      SUBROUTINE DETLV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the LV0 Detector
C-
C-   Inputs  : /D0LOG/ switches
C-   Outputs : NONE
C-
C-   Created   6-DEC-1988   A.M.Jonckheere
C-   Updated  17-JAN-1989   Harrison B. Prosper
C-                          Changed SCAL to SLV0 in SETDET
C-   Updated  19-MAR-1992   Freedy Nang  Add IUSET_LV0_PMT 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:D0LOG.INC/LIST'
      INTEGER ISET,IDET
C----------------------------------------------------------------------
C
C  DEFINE 'LV0' DETECTOR SET 
C
      CALL SLSRCP('SRCP_LV0')
      CALL SETDET('IUSET_LV0_TILES+',SLV0,ISET,IDET)
      CALL SETDET('IUSET_LV0_SUPPORT+',SLV0,ISET,IDET)
      CALL SETDET('IUSET_LV0_TILES-',SLV0,ISET,IDET)
      CALL SETDET('IUSET_LV0_SUPPORT-',SLV0,ISET,IDET)
      CALL SETDET('IUSET_LV0_PMT+',SLV0,ISET,IDET)
      CALL SETDET('IUSET_LV0_PMT-',SLV0,ISET,IDET)
      CALL RSSRCP
C
  999 RETURN
      END
