      SUBROUTINE PU_GET_EVNT_NUM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : This routine will get the event number from the user
C-   after the user request GO TO EVENT option in PIXIE COMPACK Event display.
C-   The NEXT_EVENT flag is set to TRUE here. The event number EVTCNT is
C-   stored in PXCOMK.INC.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  15-JUN-1990   Lupe Rosas Howell based on the GO TO EVENT code
C-                           in old PXMAIN  by Sharon Hagopian
C-   Updated   7-SEP-1990   Harrison B. Prosper
C-      Removed argument. Use GETPAR.
C-   Updated   2-OCT-1990   Lupe Howell  Removed the setting of NEXT_EVENT
C-                          flag to TRUE (it is done in PUSYSTEM)
C-   Updated  23-SEP-1992   Jasbir Singh Updated for Run Number to select
C-                          a particular RUN and EVENT
C-   Updated   1-OCT-1992   Lupe Howell  
C-      Use GETPAR to get event and run number 
C-   Updated   9-OCT-1992   Jasbir Singh Use GETPAR and VALUES to get Event
C                           and Run number with example                          
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PXCOMK.INC'
C
      INTEGER I,J,K,II,JJ,KK,NUMS
      CHARACTER*30 STRING,STRING2
      CHARACTER*60 RUNEVT
      REAL NUMBER(3)
      INTEGER TYPE(3)        
      CHARACTER*80 MSSG
C
      CHARACTER*(*) MESS1
      PARAMETER (MESS1 = ' GOING TO RUN/EVENT> ')
      CHARACTER*(*) MESS2
      PARAMETER (MESS2 = ' GOING TO EVENT> ')
C----------------------------------------------------------------------
C
C ****  Displaying message and getting RUN and EVENT number
C ****  from the user
C
      CALL OUTMSG('1')                  ! Clear the screen
      CALL GETPAR(1,' Enter [RUN or 0] and EVENT number[45905 197 
     & or 0 197] > ','A',RUNEVT)
C ****  Convert  character string to integer numbers 
        RUNEVT = '1 '//RUNEVT
        CALL VALUES(RUNEVT,NUMBER,TYPE,NUMS)
        RUNCNT = NUMBER(2)
        EVTCNT = NUMBER(3)
C
      WRITE(UNIT=STRING,FMT='(I8)') EVTCNT
      CALL SWORDS(STRING,I,J,K)
C
      WRITE(UNIT=STRING2,FMT='(I8)') RUNCNT
      CALL SWORDS(STRING2,II,JJ,KK)
C
C ****  to display course of action message
C
      CALL OUTMSG('1')                  ! Clear the screen
C
      IF(( EVTCNT .GT. 0 ).AND.(RUNCNT .GT. 0) ) THEN
        MSSG = MESS1//STRING2(II:JJ)//' '//STRING(I:J)
      ELSEIF( EVTCNT .GT. 0 ) THEN
        MSSG = MESS2//STRING(I:J)
      ELSE 
        MSSG = ' '
      ENDIF
C
C ****  Display message
C
      CALL INTMSG(MSSG)
C      CALL OUTMSG('1')                       ! Clear the Screen
  999 RETURN
      END
