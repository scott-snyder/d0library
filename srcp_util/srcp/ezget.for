      SUBROUTINE EZGET (PARAM1,IVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the value(s) associated with the
C-                         identifier PARAM1 from a pre-selected
C-                         RCP bank.  
C-
C-   Inputs  : PARAM1   [C*]    Name of parameter (CHARACTER string)
C-
C-   Outputs : IVAL(*)          Value(s) pertaining to name ``PARAM1''.
C-
C-             IER      [I]     Error code
C-
C-                               0 --> OK
C-                              -1 --> Variable not found
C-                              -2 --> Bank not found
C-
C-   Controls: None
C-   Calls:    EZGETA
C-
C-   Created  27-NOV-1987   Rajendran Raja
C-   Modified 16-MAY-1988   Harrison B. prosper
C-                          Modified to allow access to different SRCP
C-                          banks depending on detector type.
C-   Modified 18-JUN-1988   Harrison B. prosper
C-                          Can now return REAL,INTEGER,BOOLEAN values
C-                          or arrays thereof.
C-   Modified 27-JUN-1988   Harrison B. prosper
C-                          Convert to standard FORTRAN-77. Use UHTOC
C-                          to convert from INTEGER to CHARACTERS.
C-   Modified 30-SEP-1988   Harrison B. Prosper
C-                          Can now return a variable range of array
C-                          elements
C-   Modified  3-OCT-1988   Harrison B. Prosper
C-                          Integration with new SRCP routines
C-                          EZFILL etc..
C-   Modified 15-OCT-1988   Harrison B. Prosper
C-                          Put in check on value of bank addresses
C-   Modified 15-NOV-1988   Harrison B. Prosper
C-                          MAJOR CHANGE: USES NEW SRCP BANK FORMAT
C-   Updated  10-MAR-1992   Harrison B. Prosper   
C-      Allow return of arrays
C-   Updated   3-Jan-1996   sss - Compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) PARAM1
      INTEGER       IVAL(*)
      INTEGER       IER
C
      LOGICAL GET
      CHARACTER*32  PARAM
      INTEGER KSTART,KEND,KSTEP,LENGIN
      INTEGER I,J,K,L,ID
C
C----------------------------------------------------------------------
      ENTRY     GXSRCP (PARAM1,IVAL,IER)
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 5
C
C ****  Entry point for setting routine
C
      ENTRY SXSRCP(PARAM1,IVAL,IER)
      ENTRY EZSET (PARAM1,IVAL,IER)
      GET = .FALSE.
    5 CONTINUE
C
C ****  Decode parameter name
C
      L = LEN(PARAM1)
      CALL EZZDCD (PARAM1(1:L),PARAM,LENGIN,KSTART,KEND,KSTEP)
C
C ****  Get/Set full array if sub-range not specified.
C
      L = MIN(32,L)
      IF ( PARAM(1:L) .EQ. PARAM1(1:L) ) THEN
        KSTART = 1
        KEND   = 0
        KSTEP  = 1
      ENDIF
C
C ****  Get/Set parameter
C
      IF ( GET ) THEN
        CALL EZGETA (PARAM,KSTART,KEND,KSTEP,IVAL,IER)
      ELSE
        CALL EZSETA (PARAM,KSTART,KEND,KSTEP,IVAL,IER)
      ENDIF
C
  999 RETURN
      END
