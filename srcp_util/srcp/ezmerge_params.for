      SUBROUTINE EZMERGE_PARAMS(BANKNAME,PARAM,PAREC,ELEMENTS,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Merges the element(s) of a given paramters 
C-   into the same parameter found in BANKNAME.
C-   If the given parameter and the one in the bank have size one it will 
C-   merge the two values in an array if their values are not different.
C-
C-   Inputs  : BANKNAME  [C*]: Name of bank to do merge
C-             PARAM [C*]: name of the parameter to merge
C-             PAREC    [I ]: Maximum number of records
C-             ELEMENTS[C*(*)]: Record(s) tha belong to the parameter
C-
C-   Outputs : IER       [I ]: 0 If OK
C-
C-   Created  21-NOV-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANKNAME
      CHARACTER*(*) PARAM
      CHARACTER*(*) ELEMENTS(*)
      INTEGER PAREC,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
C
      INTEGER MAXREC
      PARAMETER( MAXREC = 3000  )
C
      INTEGER I,J,K,NREC,L1,L2,II,PARAM_REC
      LOGICAL FOUND,EZCHEK
      CHARACTER*132 BUFFER(MAXREC),TEMP1,TEMP2
C----------------------------------------------------------------------
      PARAM_REC = PAREC
      CALL EZPICK(BANKNAME)
C
C ****  Check if parameter present in bank
C ****  If not exit
C
      FOUND = EZCHEK(PARAM)
      IF ( .NOT. FOUND ) THEN
        IER = EZS_BANK_NOTFOUND
        GOTO 800 
      ENDIF
C
C ****  Get the elements of the parameters in the BANK
C
      CALL EZFETCH(PARAM,MAXREC,NREC,BUFFER,IER)
C
C ****  If the parameters to be merge are NOT arrays i.e.,
C ****  NREC and PARAM_REC are equal to 1 check their element
C ****  If the elements are NOT equal build an array with
C ****  the two elements otherwise leave only one element
C
      IF(( NREC .EQ. 1 ) .AND.( PARAM_REC .EQ. 1 ) ) THEN
        CALL WORD(BUFFER(1),I,J,K)
        CALL WORD(BUFFER(1)(J+1:80),I,K,L1)
        TEMP1 = BUFFER(1)(J+I:J+K)
        CALL WORD(ELEMENTS(1),I,J,K)
        CALL WORD(ELEMENTS(1)(J+1:80),I,K,L2)
        TEMP2 = ELEMENTS(1)(J+I:J+K)
        IF ( TEMP1(1:L1) .NE. TEMP2(1:L2) )THEN
          BUFFER(1) = '\ARRAY '//PARAM
          BUFFER(2) = TEMP1(1:L1)
          BUFFER(3) = TEMP2(1:L2)
          BUFFER(4) = '\END'
          NREC = 4
        ENDIF
C
C ****  If the parameter in the bank has only one element set the 
C ****  buffer up for an array to acomodate the merging.
C
      ELSE
        IF( NREC .EQ. 1 ) THEN
          BUFFER(2) = BUFFER(1)
          BUFFER(1) = '\ARRAY '//PARAM 
          NREC = NREC + 1
C
C ****  If the parameter in the bank has more than one element
C ****  decrease the counter of records to exclude the '\END'
C
        ELSEIF ( NREC .GT. 1 ) THEN
          NREC = NREC - 1 ! Delte the \END record from 1st file
        ENDIF
        II = 0
C
C ****  Merge the elements of the parametes into buffer
C ****  If there is more than one record in file2 skip the
C ****  '\array ' record by adding one to the buffer count
C
        DO WHILE ( II .LT. (PARAM_REC-1) )
          II = II + 1
          IF ( PARAM_REC .GT. 1 ) THEN
            BUFFER(NREC+II) = ELEMENTS(II+1) ! if more than 1 skip \array
          ELSE
            BUFFER(NREC+II) = ELEMENTS(II)
          ENDIF
          IF( (NREC+II) .GE. MAXREC ) THEN
            CALL ERRMSG(' OUT OF RANGE','EZMERGE_PARAMS',
     &      'Merge array out of range','W')
            IER = -1
            GOTO 800 
          ENDIF
        ENDDO
C
C ****  Adding the total of records of both paramters 
C ****  If the number of records in PARAM is grather than 1
C ****  decrease the count by one so the record with '\ARRAY' won't count
C
        IF ( PARAM_REC .GT. 1 )
     &          PARAM_REC = PARAM_REC - 1 ! delete the \ARRAY from count
        NREC = NREC + PARAM_REC
C
C ****  Adding \END to the buffer if PARAM_REC is one
C
        IF ( PARAM_REC .EQ. 1 ) THEN
          NREC = NREC + 1
          BUFFER(NREC) = '\END'
        ENDIF
      ENDIF
C
C ****  After merging the elements into one buffer 
C ****  Modify the array
C
      CALL EZ_MODIFY_ARRAY(PARAM,BUFFER,NREC,IER)
      IF ( IER .NE. EZS_SUCCESS ) THEN
        CALL ERRMSG(' MODIFYING ARRAYS','EZMERGE_PARAMS',
     &    ' Error modifying the bank','W')
      ENDIF

  800 CALL EZRSET
  999 RETURN
      END
