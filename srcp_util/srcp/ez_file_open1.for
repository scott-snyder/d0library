      SUBROUTINE EZ_FILE_OPEN1(GTUNIT_ID,RCP_NAME,CHIO,CHAR,
     &  UNIT, FILENAME,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : OPENS a file name given in RCP_FILE
C-
C-   Inputs  : GTUNIT_ID  = ID for GT_UNIT
C-             RCP_NAME   = RCP name containing file name to be opened
C-                          as a counted character string
C-             CHOPT = character option   for D0OPEN
C-                            
C-           'I' input  (VAX status OLD, READONLY)                        
C-           'O' output (VAX status NEW)                                  
C-           'A' append (this option may not be available on all machines)
C-           'F' formatted                                                
C-           'U' unformatted                                              
C-           combinations allowed but some are illegal (i.e. 'UF')        
C-           defaults 'I' and 'F' (i.e. ' ' equivalent to 'IF' or 'FI')   
C-           
C-             CHAR = CHARACTER STRING TO APPEND TO FILE NAME IN RCP
C-             
C-   Outputs : UNIT = Unit number of opened file
C-             FILENAME = name of opened file (obtained from RCP)
C-             IER  = Error code.
C-   Controls: 
C-
C-   Created   9-NOV-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GTUNIT_ID,UNIT,IER,NSTRING,LENGTH
      INTEGER I,J,K,II,JJ,KK,LL,ICHAR
      CHARACTER*(*) RCP_NAME,CHIO,FILENAME,CHAR
      LOGICAL OK
C----------------------------------------------------------------------
      IER = 0
C
      NSTRING = 1
      FILENAME = ' '                   ! Initializing it.
      CALL EZGETS (RCP_NAME , NSTRING , FILENAME , LENGTH, IER)
      CALL SWORDS (CHAR,I,J,K)
      CALL SWORDS (FILENAME,II,JJ,KK)
      LL = LEN(FILENAME)
C
C ****  PICK YOUR FAVORITE MARKER TO STICK CHAR AT
C
      ICHAR = INDEX(FILENAME,'#')
      IF (ICHAR.EQ.0 )THEN
        ICHAR = INDEX(FILENAME,'@')
      END IF
      IF (ICHAR.EQ.0 )THEN
        ICHAR = INDEX(FILENAME,'%')
      END IF
      IF (ICHAR.EQ.0 )THEN
        ICHAR = INDEX(FILENAME,'*')
      END IF
      IF (ICHAR.EQ.0 )THEN
        ICHAR = INDEX(FILENAME,'?')
      END IF
      IF (ICHAR.EQ.0 )THEN
        ICHAR = JJ
      END IF
C
      FILENAME = FILENAME(1:ICHAR-1)//CHAR(I:J)//FILENAME(ICHAR+1:JJ)
      CALL GTUNIT(GTUNIT_ID,UNIT,IER)
      CALL D0OPEN (UNIT,FILENAME,CHIO,OK)
      IF(.NOT.OK)IER = IER +1
C
  999 RETURN
      END
