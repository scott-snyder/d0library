      PROGRAM ERRMSG_EXAMPLE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      demonstrate basic ERRMSG call
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  10-JUL-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CALL SETCHK                       ! set up for
      CALL SPLTIT                       ! splilt screen (optional)
C
C...REQUEST UNIT 80 AS LOGGING UNIT, WARNINGS TO SCREEN
      CALL ERRINI(80,.TRUE.)

C
C       EXAMPLE OF BASIC CALLS
C
C...FIRST TO SCREEN ONLY
C
      CALL ERRMSG('FIRST MESSAGE','ERRMSG_EXAMPLE',
     &  'ANY MESSAGE YOU LIKE','S')
C
C...THEN INFORMATIVE MESSAGE, TO LOG ONLY
      CALL ERRMSG('SECOND MESSAGE','ERRMSG_EXAMPLE',
     &  'ANY MESSAGE YOU LIKE','I')
C
C...THEN WARNING MESSAGE, TO SCREEN AND TO LOG
      CALL ERRMSG('THIRD MESSAGE','ERRMSG_EXAMPLE',
     &  'ANY MESSAGE YOU LIKE','W')
C
C...THEN ANOTHER WARNING MESSAGE
      CALL ERRMSG('FOURTH MESSAGE','ERRMSG_EXAMPLE',
     &  'ANY MESSAGE YOU LIKE','W')
C
C...AND REPEAT THIRD MESSAGE AS WARNING
      CALL ERRMSG('THIRD MESSAGE','ERRMSG_EXAMPLE',
     &  'ANY MESSAGE YOU LIKE','W')
C
C...AND FINALLY A SUMMARY
      CALL ERRSUM(90)
      END
