      SUBROUTINE ENLSTP (DETEC1,IZLINK,SBANK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link from which the bank Sxxx hangs
C-                         below the STPx headers for given detector.
C-
C-   Inputs  : DETEC1      Detector mneumonic
C-                         '/CAL', '/LV0', '/CDC' etc.
C-                         
C-   Outputs : IZLINK      Link from which bank hangs
C-             SBANK       Name of bank
C-             
C-   Controls: 
C-
C-   Created   7-MAR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER IZLINK,L
      CHARACTER*(*) DETEC1
      CHARACTER*4 DETEC,SBANK
      INCLUDE 'D0$LINKS:IZSLV0.LINK'
      INCLUDE 'D0$LINKS:IZSMUO.LINK'
      INCLUDE 'D0$LINKS:IZSCAL.LINK'
      INCLUDE 'D0$LINKS:IZSVTX.LINK'
      INCLUDE 'D0$LINKS:IZSCDC.LINK'
      INCLUDE 'D0$LINKS:IZSFDC.LINK'
      INCLUDE 'D0$LINKS:IZSTRD.LINK'
C----------------------------------------------------------------------
C
      L = LEN(DETEC1)
      DETEC = ' '
      CALL UPCASE (DETEC1(1:L),DETEC)
      IF     ( INDEX(DETEC,'LV0') .GT. 0 ) THEN
        IZLINK = IZSLV0
        SBANK  = 'SLV0'
      ELSEIF ( INDEX(DETEC,'MUO') .GT. 0 ) THEN
        IZLINK = IZSMUO
        SBANK  = 'SMUO'
      ELSEIF ( INDEX(DETEC,'CAL') .GT. 0 ) THEN
        IZLINK = IZSCAL
        SBANK  = 'SCAL'
      ELSEIF ( INDEX(DETEC,'VTX') .GT. 0 ) THEN
        IZLINK = IZSVTX
        SBANK  = 'SVTX'
      ELSEIF ( INDEX(DETEC,'CDC') .GT. 0 ) THEN
        IZLINK = IZSCDC
        SBANK  = 'SCDC'
      ELSEIF ( INDEX(DETEC,'FDC') .GT. 0 ) THEN
        IZLINK = IZSFDC
        SBANK  = 'SFDC'
      ELSEIF ( INDEX(DETEC,'TRD') .GT. 0 ) THEN
        IZLINK = IZSTRD
        SBANK  = 'STRD'
      ELSE
        IZLINK = 0                      ! Error condition
        SBANK  = ' '
      ENDIF
C
  999 RETURN
      END
