      SUBROUTINE PX_START_SEQUENCE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initiate a sequence of views separated by 
C-   some delay.  A sequence command is identified by the '$' prefix.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  24-SEP-1991   Harrison B. Prosper, Lupe Howell
C-   Updated   2-JAN-1992   Lupe Howell  The list of available seuqnce arrays
C-                          is not display if it is only one array 
C-   Updated   7-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated   8-JUL-1992   Lupe Howell  Set the sequence flag at this level 
C-                          for submenus in sequences
C-   Updated   3-NOV-1992   Lupe Howell  Add DISPLAY_ITEMS 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*40 CURRENT_PACKAGE,SEQUENCE_COMMAND,SEQUENCE_NAME(10)
      CHARACTER*40 REMARK,SEQ_REM(10)
      CHARACTER*4 CVAL
      INTEGER ID(10),NID,N,TOTAL_SEQUENCE,I,ITYPE,IVAL,IER
      LOGICAL ID_FOUND
C----------------------------------------------------------------------
C
C ****  Get the ID's for all sequence arrays
C
      TOTAL_SEQUENCE = 0
      NID = 1 
      I = 0
      ID_FOUND = .TRUE.
      DO WHILE ( ID_FOUND ) 
        I = I + 1
        CALL EZGNXT('$',NID,ID(I))
        ID_FOUND = ( ID(I) .NE. 0 )
        IF ( ID_FOUND ) TOTAL_SEQUENCE = TOTAL_SEQUENCE + 1
      ENDDO
C
C ****  Getting Sequence Array Names
C
      DO I = 1, TOTAL_SEQUENCE
        CALL EZGETN(ID(I),SEQUENCE_NAME(I),N)
      ENDDO
      IF ( TOTAL_SEQUENCE .EQ. 0 ) THEN
        CALL INTMSG(' There are NO Sequence Commands available')
        GOTO 999
      ENDIF
C
C ****  Getting Sequence array(s) Remarks
C
      I = 0
      DO WHILE ( I .LT. TOTAL_SEQUENCE ) 
        I = I + 1
        CALL EZ_GET_ARRAY ! Getting remarks of combined views
     &    (SEQUENCE_NAME(I),'%TITLE',1,IVAL,CVAL,ITYPE,REMARK,IER)
        SEQ_REM(I) = REMARK
      ENDDO
C
C ****  Display list of available sequences
C ****  If the list of sequence arrays is equal to 1 do not
C ****  display it for selection select the one automatically.
C
      N = 0
      IF ( TOTAL_SEQUENCE .GT. 1 ) THEN
        CALL DISPLAY_ITEMS
     &  (TOTAL_SEQUENCE,SEQUENCE_NAME,SEQ_REM,'SEQUENCE DISPLAY',N)
      ELSE
        N = 1
      ENDIF
      IF ( N .EQ. 0 ) GOTO 999              ! Exit if no selection made
      SEQUENCE_COMMAND = SEQUENCE_NAME(N)
C
C ****  Load sequence command into first FIFO 
C
      CALL PU_ACTIVE_PACKAGE(CURRENT_PACKAGE)
      CALL FLGSET('SEQUENCE_MODE',.TRUE.)
      CALL PX_COMBINE_VIEWS(CURRENT_PACKAGE,SEQUENCE_COMMAND)
C
C ****  Copy content of COMMAND_FIFO into SEQUENCE-FIFO
C ****  and disable COMMAND_FIFO
C
      CALL PX_ENABLE_SEQUENCE_FIFO
C
C ****  Setup interrupt menu
C
      CALL PX_SETUP_SEQUENCE_INTERRUPT

  999 RETURN
      END
