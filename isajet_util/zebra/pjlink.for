C DEC/CMS REPLACEMENT HISTORY, Element PJLINK.FOR
C *2    25-JAN-1990 13:38:44 CSTEWART "Chip Stewart: CONTROL PJET TEMP LINK AREA"
C *1    17-JAN-1990 12:25:22 CSTEWART "Chip Stewart: CONTROL PJET TEMP LINK AREA"
C DEC/CMS REPLACEMENT HISTORY, Element PJLINK.FOR
      SUBROUTINE PJLINK (SWITCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Activate/de-activate PJET temporary link
C-   area.
C-
C-   Inputs  : SWITCH   [I]     1 -- Activate link area
C-                              0 -- De-activate link area
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-JAN-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER SWITCH
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LKPJET.INC'
C----------------------------------------------------------------------
      IF ( SWITCH .EQ. 1 ) THEN
        CALL MZLINT (IXMAIN,'LKPJET',JPJET,KPJET(MXPJET),JPJET)
      ELSE
        JPJET(1) = 0                    ! De-activate link area
      ENDIF
  999 RETURN
      END
