      LOGICAL FUNCTION INILV0
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize LV0 detector - done EVERY run
C-
C-   Inputs  : Logical flags
C-   Outputs : None
C-
C-   Created  6-DEC-1988   A.M.Jonckheere
C-   Updated  18-JAN-1989  Harrison B. Prosper, Chip Stewart
C-                         added calls to get SRCP_LV0 bank
C-   Updated   2-JUN-1989   Harrison B. Prosper
C-   Made into a program-builder interface function
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR,LUNGEO
      INCLUDE 'D0$INC:D0LOG.INC'
C----------------------------------------------------------------------
      INILV0 = .TRUE.
      IF ( DLV0 .LE. 0 ) GOTO 999
C
      IF ( DLV0.GT.0 ) THEN
        CALL GTUNIT(4,LUNGEO,IERR)
        CALL ZZOPEN (LUNGEO,'LV0_STPFILE',IERR,'INPUT')
        CALL EZIN   (LUNGEO,'SRCP_LV0')   ! Fetch LV0 SRCP bank from disk
        CALL ZZCLOS (LUNGEO,IERR,'INPUT')
        CALL RLUNIT(4,LUNGEO,IERR)
      ENDIF
C
  999 RETURN
      END
