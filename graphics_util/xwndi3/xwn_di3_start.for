      SUBROUTINE DI3_START(DEVICE)
C----------------------------------------------------------------------
C-   Purpose and Methods : Open Di3000 window.
C-   Inputs  : DEVICE   [I]     Device number
C-   Created  20-FEB-1991   Harrison B. Prosper, Sharon Hagopian
C-               JUN-1991   Michael Shupe  SGI version
C----------------------------------------------------------------------
      INTEGER DEVICE
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
c        IDEBUG=1
C!!!!GONE        CALL HPLINT(DEVICE)
        CALL JBEGIN
        CALL JDINIT(DEVICE)
        CALL JDEVON(DEVICE)
        FIRST = .FALSE.
      ENDIF
      RETURN
C
      ENTRY DI3_END
      IF ( .NOT. FIRST ) THEN
C!!!!GONE        CALL HPLEND
        CALL JDEVOF(DEVICE)
      ENDIF
  999 RETURN
      END
