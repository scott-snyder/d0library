C DEC/CMS REPLACEMENT HISTORY, Element EZ_FILE_OPEN.FOR
C *1    21-MAR-1990 15:31:35 STEWART "opens file specified in rcp file"
C DEC/CMS REPLACEMENT HISTORY, Element EZ_FILE_OPEN.FOR
      SUBROUTINE EZ_FILE_OPEN(GTUNIT_ID,RCP_NAME,CHIO,UNIT,
     &  FILENAME,IER)
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
      CHARACTER*(*) RCP_NAME,CHIO,FILENAME
      LOGICAL OK
C----------------------------------------------------------------------
      IER = 0
C
      NSTRING = 1
      FILENAME = ' '                   ! Initializing it.
      CALL EZGETS (RCP_NAME , NSTRING , FILENAME , LENGTH, IER)
      CALL GTUNIT(GTUNIT_ID,UNIT,IER)
      CALL D0OPEN (UNIT,FILENAME,CHIO,OK)
      IF(.NOT.OK)IER = IER +1
C
  999 RETURN
      END
