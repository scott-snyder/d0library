      SUBROUTINE EZ_STORE_NAME(RCP_NAME,NAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Store a name NAME in RCP array given
C-   by RCP_NAME. Create RCP_NAME if it does not exist.
C-   RCP bank is assumed to be picked prior to call
C-
C-   Inputs  : RCP_NAME,NAME
C-   Outputs : IER not zero if error
C-   Controls:
C-
C-   Created  11-FEB-1990   Rajendran Raja
C-   Updated   5-NOV-1991   Krzysztof L. Genser  
C-      to handle FATMEN long generic names
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_NAME,NAME
      INTEGER IER
      LOGICAL EZCHEK
      INTEGER LNAME,HNAME(64),LEN,LWORD,I
      LOGICAL EZERR,IDUM
      CHARACTER*1 TNAME(64)
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      IER = 0
      LNAME = LEN(NAME)
      IF(LNAME.GT.255)LNAME = 255
      IF ( .NOT.EZCHEK(RCP_NAME) ) THEN
C Create the bank and fill it
        LWORD = (LNAME+3)/4
        CALL UCTOH(NAME,HNAME,4,LNAME)
        DO I = 1 , LWORD
          TNAME(I) = 'C'
        ENDDO
        CALL EZFILL(RCP_NAME,'CREATING NEW NAME',HNAME,TNAME,
     &    LWORD)
        IDUM = EZERR(IER)
        CALL EZEND                      ! DO IT RIGHT
      ELSE
        CALL EZSETC(RCP_NAME,1,LNAME,NAME,IER)
      ENDIF
      IF ( .NOT. ((IER.NE.EZS_SUCCESS) .OR.
     &     (IER.NE.EZS_BANK_EXTENDED)) ) THEN
        CALL ERRMSG('RCP','EZ_STORE_NAME',
     &    'ERROR IN STORING NAME ','W')
      ENDIF
  999 RETURN
      END
