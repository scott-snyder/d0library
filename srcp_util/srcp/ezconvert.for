      SUBROUTINE EZCONVERT(LSTART,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Convert SRCP banks within a STP ZEBRA tree to 
C-                         CRCP banks
C-
C-   Inputs  : LSTART - Address of starting bank 
C-   Outputs : IER    - IER=0 OK
C-   Controls:
C-
C-   Created  22-JUL-1992   Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LSTART,IER
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:STP_ZLINKA.INC'
      INTEGER LSRCP,J,K,LSUP,IL,JL,NL,LBANK,ID,LEVEL
      INTEGER NLINK,NSTART,IER1
      INTEGER MAXL
      PARAMETER (MAXL=500)
      INTEGER LINKS(MAXL)
      CHARACTER BANK*4,BKNAME*80,BANKS(MAXl)*4
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        CALL STP_INZLNK
        CALL STP_GSLINK('EZCONVERT',NSTART)
      END IF
      LEVEL = 0
      IL = 0
      JL = 0
      LSRCP = LSTART
      STP_LSLINK(NSTART) = LSRCP
    1 CALL DHTOC (4,IC(LSRCP-4),BANK)
      IF ( BANK .EQ.'SRCP' ) THEN         !SRCP
        CALL EZSRCP_TO_CRCP(LSRCP,IER1)
        IER = IER1
      END IF
      NL = IC(LSRCP-2)                    !LOOP THROUGH LINKS FOR SRCP
      DO IL = JL, NL
        IF (LC(LSRCP-IL).GT.0) GOTO 2     !FOUND A LINK
      END DO
      JL= LINKS(LEVEL)+1                  !PREVIOUS LINK
      LEVEL  = LEVEL - 1                  !NO LINKS HERE
      IF(LEVEL.LT.0) THEN
        LSTART = STP_LSLINK(NSTART) 
        GOTO 999
      END IF
      LSRCP = STP_LSLINK(NSTART)
      DO J = 1, LEVEL
        LSRCP  = LC(LSRCP-LINKS(J))
        IF (LBANK.LE.0) THEN
          CALL ERRMSG('LOST LINK TREE 3','EZCONVERT','oops','W')
        END IF
      END DO
      GOTO 1
    2 CONTINUE
      JL = 0
      LEVEL = LEVEL + 1
      LINKS(LEVEL) = IL
      LSRCP = LC(LSRCP-IL)
      LBANK = STP_LSLINK(NSTART)
      CALL DHTOC(4,IC(LSRCP-4),BANKS(LEVEL))
      DO J = 1, LEVEL
        LBANK  = LC(LBANK-LINKS(J))
        IF (LBANK.LE.0) THEN
          CALL ERRMSG('LOST LINK TREE 1','EZCONVERT','oops','W')
        END IF
      END DO
      IF(LBANK.NE.LSRCP) THEN
        CALL ERRMSG('LOST LINK TREE 2','EZCONVERT','oops','W')
      ELSE
        GOTO 1
      END IF
  999 RETURN
      END
