       SUBROUTINE PM_INFO(IFIRST,ILAST,NUM_EVENTS,
     &		    NUM_STREAMS,STREAM_NAME,STREAM_TYPE,NEVT_STREAM,
     &              PRG_NAME,IPRG_VERSION,IPRG_PASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :When programs are run under the control of the 
C-            Production Manager, calling PM_INFO periodically will make
C-            status information from the calling program available to
C-            the PM. If it is called when the job is finished, with the 
C-            final status info, then this information will be added to 
C-            the production data base in a standard way. 
C-
C-   Inputs  : 
C
C Variables are as follows:
C
C	IFIRST(1)  : First event processed RUN NUMBER
C	IFIRST(2)  : First event processed EVENT NUMBER
C	ILAST(1)   : Last event  processed RUN NUMBER
C	ILAST(2)   : Last event  processed EVENT NUMBER
C	NUM_EVENTS : Number of events processed
C	NUM_STREAMS: Number of output streams
C	STREAM_NAME: Names of output streams (Ex. MIN,MUO,ELE,QCD)
C	STREAM_TYPE: Data type of output streams (Ex. RAW,STA,DST)
C	NEVT_STREAM: Number of events to each output stream
C	PRG_NAME   : Program name
C	IPRG_VERSION: Program version release number
C	IPRG_PASS   : Program pass release number
C
C-   Outputs : out put file PM_STATUS
C-   Controls: 
C-
C-   Created  22-JUL-1992   Lee Lueking
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
	CHARACTER*(*) STREAM_NAME(*),STREAM_TYPE(*)
	CHARACTER*(*) PRG_NAME
        INTEGER I,INDEX
        INTEGER IPRG_VERSION,IPRG_PASS
	INTEGER IFIRST(2),ILAST(2),NUM_EVENTS,NUM_STREAMS
        INTEGER NEVT_STREAM(*)
        LOGICAL OK,FIRST
        CHARACTER*10  STRING,STRING1,STRING2 !for integer conversions
        CHARACTER*2  STR2 !for 2 place integer conversions
        CHARACTER*80 INFORMATION
        SAVE FIRST
        DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C Open the file
C
        CALL PM_OPEN(OK)
C
C Write the info to an output file using PM_MESSAGE.
C
C
C General info
C
        INFORMATION=' PROGRAM NAME            : '//PRG_NAME
        CALL PM_MESSAGE(INFORMATION,OK)
        WRITE(STRING,'( I7 )')IPRG_VERSION
        INFORMATION=' PROGRAM VERSION NUMBER  : '//STRING
        CALL PM_MESSAGE(INFORMATION,OK)
        WRITE(STRING,'( I7 )')IPRG_PASS
        INFORMATION=' PROGRAM PASS NUMBER     : '//STRING
        CALL PM_MESSAGE(INFORMATION,OK)
        WRITE(STRING,'( I7 )')NUM_EVENTS
        INFORMATION=' EVENTS PROCESSED        : '//STRING
        CALL PM_MESSAGE(INFORMATION,OK)
          WRITE(STRING1,'( I7 )')IFIRST(1)
          WRITE(STRING2,'( I7 )')IFIRST(2)
          INFORMATION=' FIRST EVENT (RUN EVENT) : '//STRING1//
     &                                          ' '//STRING2
          CALL PM_MESSAGE(INFORMATION,OK)
          WRITE(STRING1,'( I7 )')ILAST(1)
          WRITE(STRING2,'( I7 )')ILAST(2)
          INFORMATION=' LAST EVENT  (RUN EVENT) : '//STRING1//
     &                                          ' '//STRING2
          CALL PM_MESSAGE(INFORMATION,OK)
        WRITE(STRING,'( I7 )')NUM_STREAMS
        INFORMATION=' NUMBER OF STREAMS       : '//STRING
        CALL PM_MESSAGE(INFORMATION,OK)
C
C Stream info
C
        DO I=1,NUM_STREAMS
            WRITE(STR2,'( I2.2 )')I
            INFORMATION=' STREAM NAME   '//STR2//
     &                  '        : '//STREAM_NAME(I)
            CALL PM_MESSAGE(INFORMATION,OK)
            INFORMATION=' STREAM TYPE   '//STR2//
     &                  '        : '//STREAM_TYPE(I)
            CALL PM_MESSAGE(INFORMATION,OK)
            WRITE(STRING,'( I7 )')NEVT_STREAM(I)
            INFORMATION=' STREAM EVENTS '//STR2//
     &                  '        : '//STRING
            CALL PM_MESSAGE(INFORMATION,OK)
        ENDDO
C
C Close the file
C
        CALL PM_CLOSE(OK)
C
C
  999 RETURN
      END
