      LOGICAL FUNCTION CAL_EVENT_DUMP
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    Called from CRUNCH: DUMP EVENT
C-
C-    Created by the PROGRAM BUILDER Release V1.10
C-    13-OCT-1989 08:52
C-
C-
      LOGICAL FLGVAL
      EXTERNAL  FLGVAL
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL CALDMP
      EXTERNAL CALDMP
C-
      CAL_EVENT_DUMP = .TRUE.
C-
      IF ((FLGVAL('PBD_CALOR'))
     X    ) THEN
         IF (.NOT. CALDMP()) THEN
            CAL_EVENT_DUMP = .FALSE.
            CALLER = 'CAL_EVENT_DUMP'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
C-
C-    No routine was provided for the package: CAHITS
C-
C-
C-    No routine was provided for the package: CAJETS
C-
C-
C-    No routine was provided for the package: CAPHEL
C-
      RETURN
      END
