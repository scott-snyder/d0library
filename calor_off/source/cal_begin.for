      LOGICAL FUNCTION CAL_BEGIN
C-
C-
C-    Purposes and Methods:
C-    --------------------
C-    Called from CBEGIN: INITIALIZE USER PACKAGES
C-
C-    Created by the PROGRAM BUILDER Release V1.10
C-    13-OCT-1989 08:52
C-
C-
      LOGICAL FLGVAL
      EXTERNAL  FLGVAL
      CHARACTER*32 MESSID,CALLER
      CHARACTER*80 MESSAG
      LOGICAL CALOR_INI
      EXTERNAL CALOR_INI
      LOGICAL CHTINI
      EXTERNAL CHTINI
      LOGICAL CJTINI
      EXTERNAL CJTINI
      LOGICAL CPHINI
      EXTERNAL CPHINI
C-
      CAL_BEGIN = .TRUE.
C-
      IF ((FLGVAL('PBD_CALOR'))
     X    ) THEN
         IF (.NOT. CALOR_INI()) THEN
            CAL_BEGIN = .FALSE.
            CALLER = 'CAL_BEGIN'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAHITS'))
     X    ) THEN
         IF (.NOT. CHTINI()) THEN
            CAL_BEGIN = .FALSE.
            CALLER = 'CAL_BEGIN'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAJETS'))
     X    ) THEN
         IF (.NOT. CJTINI()) THEN
            CAL_BEGIN = .FALSE.
            CALLER = 'CAL_BEGIN'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      IF ((FLGVAL('PBD_CAPHEL'))
     X    ) THEN
         IF (.NOT. CPHINI()) THEN
            CAL_BEGIN = .FALSE.
            CALLER = 'CAL_BEGIN'
            MESSAG = 'This error is ignored'
            CALL ERRMSG(MESSID,CALLER,MESSAG,'I')
         END IF
      END IF
      RETURN
      END
