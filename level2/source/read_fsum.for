      SUBROUTINE READ_FSUM(FILENAME,WEIGHT,TIME_OUT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the stored filter summary of one run
C-
C-   Returned value  : TRUE always
C-   Inputs  : FILENAME the file containing the filter summary
C-   Outputs :  WEIGHT  the weight with which this is added
C-              TIME_OUT    the normalization time for this bank
C-              IER = 0 if all went well
C-   Controls: None
C-
C-   Created 3-FEB-1992   James T. Linnemann   
C-   Updated   8-MAR-1994   R. J. Genik II   Added protection against
C-   returning time_out=0.0 An Warning is issued and 9999999. is returned
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FILTER_COM.INC'
      INCLUDE 'D0$PARAMS:L2_TYPE.DEF'
      INCLUDE 'D0$INC:FSUM.INC'
      INTEGER NRUN,RUNNO,N,LUN,FSUM_USER,IER
      LOGICAL GOTIT
      PARAMETER( FSUM_USER = 40909  )
      CHARACTER*64 FILENAME
      REAL    WEIGHT,TIME_OUT
C----------------------------------------------------------------------
      CALL GTUNIT(FSUM_USER,LUN,IER)
      CALL D0OPEN(LUN,FILENAME,'IU',GOTIT)
      TIME = 9999999.
      IF ((IER.NE.0).OR.(.NOT.GOTIT)) THEN
        WEIGHT = 0
        IER = -5
      CALL ERRMSG('GRAND_FSUM 1','READ_FSUM',
     &  'D0OPEN Failed for file '//FILENAME,'W')
      ELSE
        READ(LUN,ERR=100)
     &    NRUN_READ,EVENT_WEIGHT,CROSS_SECT,FILTER_COUNT,
     &    TOT_EVENTS_READ,
     &    FILTER_SET_COUNT,FILTER_WRITE_COUNT,WRITE_UNFILT_COUNT,
     &    FILT_EVENTS,AVGBITS,FNCALL,FAVG,FSIGMA,
     &    SNCALL,SAVG,SSIGMA,TNCALL,TAVG,TSIGMA,TIME
        If (Time.gt.0.0) then
          TIME_OUT = TIME
        Else 
      CALL ERRMSG('GRAND_FSUM 1','READ_FSUM',
     &  'Returned time was Zero or less, set to 9999999.0','W')
        Endif
        CLOSE(LUN)
      ENDIF
  200 CONTINUE
      WEIGHT = EVENT_WEIGHT
      CALL RLUNIT(FSUM_USER,LUN,IER)
  999 RETURN
  100 CONTINUE
      CALL ERRMSG('GRAND_FSUM 1','READ_FSUM',
     &  'Error reading data from file '//FILENAME//' Old format?','W')
      CLOSE(LUN)
      GOTO 200
C
      END
