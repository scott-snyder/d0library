      FUNCTION PBD_TEMP_FLAG (FLAG,VALUE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To test whether PBD FLAG was temporarily set
C-                         TRUE or FALSE in PBD_TEMP_FLAG_TRUE ENTRY.
C-                         This routine was invented to circumvent the
C-                         way that PBD handles calls to package routines. 
C-                         For a MENU routine the CALL should be made 
C-                         So that the item appears in the menu- BUT
C-                         It appears with a special marker indicating that it
C-                         is not available.
C-
C-   Returned value  : TRUE IF FLAG BOOKED
C-   Inputs  : FLAG [C]
C-   Outputs : VALUE
C-   Controls: NONE
C-
C-   Created  22-JUL-1990   Chip Stewart
C-   Updated  24-MAY-1992   Boaz Klima
C-      Set maximum number of packages/flags to 50
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER I,J,K,N,MAX_FLAG,F1,F2,FL,P1,P2,PL
      PARAMETER (MAX_FLAG=50)
      LOGICAL PBD_TEMP_FLAG_TRUE,PBD_TEMP_FLAG_RESET,OK,ACTIVE,VALUE
      LOGICAL PBD_TEMP_FLAG_ONE_TRUE,PBD_TEMP_FLAG
      LOGICAL PBD_VALUE(MAX_FLAG)
      LOGICAL PBD_GET_NEXT_FLAG,PBD_GET_FLAG,PBD_SET_FLAG
      LOGICAL TEMP
      CHARACTER*32 PBD_FLAG(MAX_FLAG),FLAG_TMP
      CHARACTER*(*) FLAG
      PBD_TEMP_FLAG = .FALSE.
      IF (.NOT. TEMP) THEN
        CALL ERRMSG('PBDTEMPNINIT','PBD_TEMP_FLAG',
     &    ' PBD TEMPORARY FLAGS NOT SETUP ','W')
        VALUE = .FALSE.
        GOTO 999
      END IF
      CALL WORD (FLAG,F1,F2,FL)
      DO I = 1, N
        CALL WORD (PBD_FLAG(I),P1,P2,PL)
        IF ( PBD_FLAG(I)(P1:P2).EQ. FLAG(F1:F2) ) THEN
          PBD_TEMP_FLAG = .TRUE.
          VALUE = PBD_VALUE(I)
        END IF
      END DO
  999 RETURN
C
C ****  ENTRY TO TEMPORARILY SET ALL FLAGS TRUE
C
      ENTRY PBD_TEMP_FLAG_TRUE ()
      IF (TEMP) THEN
        CALL ERRMSG('PBDTEMPDONE','PBD_TEMP_FLAG_TRUE',
     &    ' PBD TEMPORARY FLAGS ALREADY SETUP ','W')
        GOTO 1999
      END IF
      N = 1
      ACTIVE = .TRUE.
      DO WHILE (ACTIVE)
        ACTIVE = PBD_GET_NEXT_FLAG(PBD_FLAG(N),PBD_VALUE(N),N)
        IF (N.GT.MAX_FLAG) ACTIVE = .FALSE.
      END DO
      OK  = .TRUE.
      DO I = 1, N
        IF ( .NOT. PBD_VALUE(I)) THEN
          OK = OK .AND. PBD_SET_FLAG(PBD_FLAG(I),.TRUE.)
        END IF
      END DO
      PBD_TEMP_FLAG_TRUE = OK
      TEMP = .TRUE.
 1999 RETURN
C
C ****  ENTRY TO RESET ALL FLAGS TO ORIGINAL STATE
C
      ENTRY PBD_TEMP_FLAG_RESET ()
      IF (.NOT. TEMP) THEN
        CALL ERRMSG('PBDTEMPNORESET','PBD_TEMP_FLAG_RESET',
     &    ' PBD TEMPORARY FLAGS NOT SETUP ','W')
        GOTO 2999
      END IF

      OK = .TRUE.
      DO I = 1, N
        IF ( .NOT. PBD_VALUE(I)) THEN
          OK = OK .AND. PBD_SET_FLAG(PBD_FLAG(I),.FALSE.)
        END IF
      END DO
      PBD_TEMP_FLAG_RESET = OK
      TEMP = .FALSE.
 2999 RETURN
C
C ****  ENTRY TO TEMPORARILY SET ALL FLAGS FALSE BUT ONE - FLAG
C
      ENTRY PBD_TEMP_FLAG_ONE_TRUE (FLAG,VALUE)
      IF ( TEMP) THEN
        CALL ERRMSG('PBDTEMPNOFALSE','PBD_TEMP_FLAG_ONE_TRUE',
     &    ' PBD TEMPORARY FLAGS ALREADY USED ','W')
        GOTO 3999
      END IF
      N = 1
      ACTIVE = .TRUE.
      DO WHILE (ACTIVE)
        ACTIVE = PBD_GET_NEXT_FLAG(PBD_FLAG(N),PBD_VALUE(N),N)
        IF (N.GT.MAX_FLAG) ACTIVE = .FALSE.
      END DO
      OK  = .TRUE.
      DO I = 1, N
        IF (PBD_VALUE(I) .AND. (PBD_FLAG(I).NE.FLAG) ) THEN
          OK = OK .AND. PBD_SET_FLAG(PBD_FLAG(I),.FALSE.)
        END IF
      END DO
      PBD_TEMP_FLAG_ONE_TRUE = OK
      TEMP = .TRUE.
 3999 RETURN
      END
