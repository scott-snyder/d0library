      SUBROUTINE EZZLOC (BKNAME,LSRCP,ID)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return index into SRCP link area.
C-
C-   Inputs  : BKNAME        Bank name. Up to 32 characters.
C-
C-   Outputs : LSRCP       Address of SRCP bank
C-             ID          Pointer to bank address in SRCP link area
C-                        > 0 --- OK
C-                          0 --- Bank not found.
C-   Controls: None
C-
C-   Created  23-SEP-1988   Harrison B. Prosper
C-   Updated  10-MAY-1990   Harrison B. Prosper
C-      Made compatible with EZDROP
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) BKNAME
      INTEGER ID
C
      INTEGER I,J,L,N
      CHARACTER*32 STRING
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
      ID = 0
      LSRCP = 0
C
      L = LEN (BKNAME)
      CALL WORD (BKNAME(1:L),I,J,N) ! Remove leading and trailing blanks
      STRING = BKNAME(I:J)
      CALL UPCASE (STRING,STRING)
C
C ****  Do simple sequential search
C
      I = 0
      J = 0
      DO WHILE ( (I .LT. NSRCP) .AND. (J .LT. MXSRCP) )
        J = J + 1
        IF ( KSRCP(J) .GT. 0 ) THEN
          I = I + 1
          IF ( STRING .EQ. MSRCP(J) ) THEN
            ID = J
            LSRCP = KSRCP(ID)
            GOTO 999
          ENDIF
        ENDIF
      ENDDO
C
  999 RETURN
      END
