      SUBROUTINE ERRDSP(IDSTR, SUB, LABEL, VARSTR, LINE1, LINE2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a formatted error message to the
C-                         indicated device.  Displays one or two
C-                         line message.
C-
C-   Inputs  :
C-             IDSTR       Char string identifies the message
C-             SUB         Char string identifies calling routine
C-             LABEL       Two char identifing severity level
C-             VARSTR      Char string proving additional error
C-                         information
C-
C-   Outputs : LINE1       First line of message to be displayed
C-             LINE2       Second line of error message
C-   Controls: None
C-
C-   Created  15-JUN-1989   Jason McCampbell (MSU)
C-   Updated   5-NOV-1991   Krzysztof L. Genser
C-   Updated  14-APR-1992   James T. Linnemann  add ELN version for ALARM system
C       to handle FATMEN long generic names
C-   Updated  23-MAR-1994   James T. Linnemann   supress run=event=0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) IDSTR, SUB
      CHARACTER*(*) VARSTR
      CHARACTER*(*) LABEL
      CHARACTER*(*) LINE1,LINE2
      CHARACTER*48 RESTR
      CHARACTER*48 ZEROS
      PARAMETER( ZEROS =' Run 0 Event 0'  )
      INTEGER SCRNSZ
      INTEGER I1,J1,N1, I2,J2,N2, I3,J3,N3
      LOGICAL LOGWRN
      INCLUDE 'D0$ERRMSG_UTIL$PARAMS:ERRMSG.PARAMS'
      INCLUDE 'D0$INC:ERRCTL.INC'
      INCLUDE 'D0$INC:ERRCNT.INC'
      INCLUDE 'D0$INC:ERRIDS.INC'
C&IF VAXELN
C&      CHARACTER*6 NODE_NAME
C&      PARAMETER( SCRNSZ = 128)    !size of message passed to ALARM system
C&ELSE
      PARAMETER( SCRNSZ = 79)          ! Set to 78 if INTMSG requires it]
C&ENDIF
C----------------------------------------------------------------------
C Get the run and event number string.  Form all info into error line.
      CALL EVTMSG(RESTR)                ! Get the run/event string
      IF (RESTR.EQ.ZEROS) RESTR = ' '
      CALL SWORDS(RESTR, I1,J1,N1)
      CALL SWORDS(SUB, I2, J2, N2)      ! Cut down subroutine string
      CALL SWORDS(IDSTR, I3, J3, N3)    ! Also clip the id string
C&IF VAXELN
C&      CALL GET_NODE_NAME(NODE_NAME)
C&      WRITE(LINE1, FMT=100) NODE_NAME, SUB(I2:J2), LABEL(1:1),
C&     & RESTR(I1:J1), IDSTR(I3:J3)
C&  100 FORMAT(A,'::'A,'-',A,1X,A,':',A)
C&ELSE
      WRITE(LINE1, FMT=100) SUB(I2:J2), LABEL(1:1),
     + RESTR(I1:J1), IDSTR(I3:J3)
  100 FORMAT(A,'-',A,1X,A,':',A)
C&ENDIF
C Find lengths of both lines
      CALL SWORDS(LINE1, I1, J1, N1)    ! Get size of 1st line
      CALL SWORDS(VARSTR, I2, J2, N2)   ! Pack down variable string
 
C&IF VAXELN
C&C...truncate variable string to fit the alarm message length
C&      IF (N1 + N2 + 1 .GT. SCRNSZ) THEN   !format below adds a character
C&        J2 = (I2 - 1) + (SCRNSZ - N1 - 1) !always enough room for N1; chop N2
C&      ENDIF
C&      WRITE(LINE1(J1+1:), FMT=200) VARSTR(I2:J2)
C&ELSE
C If all info will fit on one line, use one.  Otherwise put VARSTR below.
      IF (N1 + N2 + 1 .LT. SCRNSZ) THEN   !format below adds a character
        WRITE(LINE1(J1+1:), FMT=200) VARSTR(I2:J2)
        LINE2 = ' '
      ELSE
        LINE2 = VARSTR(I2:J2)
      ENDIF
C&ENDIF
C
C *** Formats
C
  200   FORMAT('-',A,130X)
C
  999 RETURN
      END
