      SUBROUTINE EZ_GET_REM(ARRAY_NAME,ARRAY_ELEMENT,IDX,OCURRENCE,
     &            OUTVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets the remark of an element,
C-   defined by the triplet
C-   (param-name,value,remark) from an array of the following
C-   structure:
C-
C-   \ARRAY array-name
C-      'Param-name'    value   'Remark'
C-              :       :       :
C-   \END
C-
C-   The search for the requested element will begin at the IDX word
C-   of the array ARRAY_NAME and will return the value of the NTH OCCURRENCE
C-   of that element.
C-
C-   Inputs  : ARRAY_NAME    [C*] - Name of the array
C-             ARRAY_ELEMENT [C*] - Name of the element in the array to find
C-             IDX           [I ] - Index in the array where to start the
C-                                  search
C-             OCURRENCE     [I ] - The number of nth occurrence of the element
C-                                  that will be return
C-
C-   Outputs : OUTVAL       [C(*)]- Remark of the element requested
C-             IER           [I ] - If it does not equal to 0 the element was
C-                                  not found
C-
C-   Created  21-OCT-1992   Lupe Howell   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      CHARACTER*(*) OUTVAL
      CHARACTER*(*) NEWVAL
      INTEGER IDX, OCURRENCE,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER COUNT,ID,VAL(20),TOTAL,IPOINT,VALPOINT,REMPOINT
      INTEGER LEN,EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,LREM,I,J,N
C 
      CHARACTER*(NUMCHR) PARNAME
      CHARACTER*(CHRCRD) AVAL,REM
C
      LOGICAL ACTIVE,GET
C----------------------------------------------------------------------
      SAVE REM,AVAL
C----------------------------------------------------------------------
      GET = .TRUE.

      ENTRY EZ_SET_REM(ARRAY_NAME,ARRAY_ELEMENT,IDX,OCURRENCE,
     &      NEWVAL,IER)
      GET = .FALSE.
C
   20 CONTINUE
C
      LARR = LEN(ARRAY_NAME)
      CALL SWORDS(ARRAY_ELEMENT,I,J,LNAM) 
C
C ****  Check if an SRCP bank has been selected
C
      IF ( ISRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTSELECTED
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get the bank address
C
      LSRCP = KSRCP(ISRCP)
      IF ( LSRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTFOUND
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get pointers to data within SRCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get index into RCP bank for the given array-name
C
      CALL EZGETI (ARRAY_NAME(1:LARR),ID,IER)
C
C ****  Get number of values/identifier from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)       ! Pointer to values-list
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)     ! Zero upper word
C
C ****  Make pointers point to absolute
C
      IPTV = IPTV + IPVAL
      IPTT = IPTT + IPVAL

   30 CONTINUE
      IPOINT = IDX
      COUNT  = 0
      IER    = 0
C
C ****  Searching for the Nth element requested
C
      IER = -1                          ! Assume it does not finds it
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 999
C
C ****  Get element of array (param, value, remark) and update pointer
C
        REM = ' '
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
C
C ****  Comparing current parameter name with parameter name requested
C ****  If the counter equals to the number of ocurrence requested get/set
C ****  the parameter
C
        IF ( ARRAY_ELEMENT(1:LNAM) .EQ. PARNAME(1:LNAM) ) THEN
          COUNT = COUNT + 1
          IF( COUNT .EQ. OCURRENCE ) THEN
            IF ( GET ) THEN
C
C ****  If getting a the remark return it in OUTVAL
C
              OUTVAL = REM
              IER = 0
            ELSE
C
C ****  If setting a the remak set the new value taking in 
C ****  account the size of the previous character to avoind 
C ****  overwriting in the bank.
C
              REMPOINT = VALPOINT + ( (ITYP - 10 +3)/4 )
              N = IPOINT - REMPOINT 
              LREM = N*4
C
C ****  Set the new value 
C
              REM = NEWVAL
              CALL DCTOH(LREM,REM,VAL)
              CALL EZSET1(ID,REMPOINT,REMPOINT+N-1,1,VAL,IER)
            ENDIF
C
C ****  Exit if counter equals the number of ocurrence requested
C
            ACTIVE = .FALSE.
          ENDIF
        ENDIF
      ENDDO
  999 RETURN
C
      END
