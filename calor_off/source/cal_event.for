      LOGICAL FUNCTION CAL_EVENT
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    Called from CRUNCH: DO EVENT PROCESSING
C-
C-    Created by the PROGRAM BUILDER Release V1.10
C-    13-OCT-1989 08:52
C-
C-
      LOGICAL FLGVAL
      EXTERNAL  FLGVAL
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL CAHITS
      EXTERNAL CAHITS
      LOGICAL CAJETS
      EXTERNAL CAJETS
      LOGICAL CAPHEL
      EXTERNAL CAPHEL
C-
      CAL_EVENT = .TRUE.
C-
C-
C-    No routine was provided for the package: CALOR
C-
      IF ((FLGVAL('PBD_CAHITS'))
     X    ) THEN
         IF (.NOT. CAHITS()) THEN
            CAL_EVENT = .FALSE.
            CALLER = 'CAL_EVENT'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAJETS'))
     X    ) THEN
         IF (.NOT. CAJETS()) THEN
            CAL_EVENT = .FALSE.
            CALLER = 'CAL_EVENT'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAPHEL'))
     X    ) THEN
         IF (.NOT. CAPHEL()) THEN
            CAL_EVENT = .FALSE.
            CALLER = 'CAL_EVENT'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      RETURN
      END
