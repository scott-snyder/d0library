      LOGICAL FUNCTION CAL_EVENT_RESET
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    Called from CRUNCH: Called BEFORE EVENT is written
C-
C-    Created by the PROGRAM BUILDER Release V1.10
C-    13-OCT-1989 08:52
C-
C-
      LOGICAL FLGVAL
      EXTERNAL  FLGVAL
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL CALEVZ
      EXTERNAL CALEVZ
C-
      CAL_EVENT_RESET = .TRUE.
C-
      IF ((FLGVAL('PBD_CALOR'))
     X    ) THEN
         IF (.NOT. CALEVZ()) THEN
            CAL_EVENT_RESET = .FALSE.
            CALLER = 'CAL_EVENT_RESET'
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
