      SUBROUTINE PSAVE_WRITE
C------------------------------------------------
C-Purpose :Making Scan_save & Write Event active
C-         (hidden) when after Modify Scan,the
C-         command is NEXT EVENT
C-Control :
C-Created on 22-DEC-1992 by Vipin Bhatnagar
C-
C-MODIFIED   13-JAN-1993    Vipin Bhatnagar
C-    PX_WRITE_SCAN flag .TRUE.for writing scanned event
C--------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL FLGVAL
C
C----Checking Flag for Scan active
C
      IF ( FLGVAL('ACTIVE_SCAN') ) THEN
C
C----Saving info in bank
C
        CALL SCAN_SAVE
        CALL FLGSET('ACTIVE_SCAN',.FALSE.)
C
C----Setting Flag for SCANNED write event
C
        CALL FLGSET('PX_WRITE_SCAN',.TRUE.)
      ENDIF
C
      RETURN
      END
