      LOGICAL FUNCTION CAL_END
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    Called from CEND: END OF PROCESSING
C-
C-    Created by the PROGRAM BUILDER Release V1.10
C-    13-OCT-1989 08:52
C-
C-
      LOGICAL FLGVAL
      EXTERNAL  FLGVAL
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL CHTFIN
      EXTERNAL CHTFIN
      LOGICAL CJTFIN
      EXTERNAL CJTFIN
      LOGICAL CPHFIN
      EXTERNAL CPHFIN
C-
      CAL_END = .TRUE.
C-
C-
C-    No routine was provided for the package: CALOR
C-
      IF ((FLGVAL('PBD_CAHITS'))
     X    ) THEN
         IF (.NOT. CHTFIN()) THEN
            CAL_END = .FALSE.
            CALLER = 'CAL_END'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAJETS'))
     X    ) THEN
         IF (.NOT. CJTFIN()) THEN
            CAL_END = .FALSE.
            CALLER = 'CAL_END'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAPHEL'))
     X    ) THEN
         IF (.NOT. CPHFIN()) THEN
            CAL_END = .FALSE.
            CALLER = 'CAL_END'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      RETURN
      END
