       SUBROUTINE INIRUN(NSKIP)
C------------------------------------------------------------------
C-                                                                -
C-  Read input record and call analyzing subroutine               -
C-                                                                -
C-   OUTPUT:                                                      -
C-   NSKIP  = number of events to skip                            -
C-                                                                -
C-   ENTRY FRSTRN set flag SAME=.FALSE. to indicate next run      -
C-                is first of a series                            -
C-                                                                -
C-     SDP July,1987                                              -
C-                                                                -
C------------------------------------------------------------------
C
       IMPLICIT NONE
       INTEGER NEVRUN,NEVTS,NSKIP
       INCLUDE 'D0$INC:ZEBCOM.INC'
       LOGICAL OK
       LOGICAL SAME,DOWRIT,YES,GOON,USRPAR,FLGVAL
       INTEGER USNVRN,IRECS,IRECTP
       DATA SAME/.FALSE./
C
       IF(GOON().AND.(.NOT.SAME)) THEN
         NEVRUN=USNVRN()
         IF(NEVRUN.EQ.0) NEVRUN=-2
         CALL STNVRN(NEVRUN)
         SAME=.TRUE.
       ENDIF
C
       IF(.NOT.SAME) CALL DIAL_EVENTS(NSKIP)
C
C
         IRECS=IQ(LHEAD+1)
         IRECTP=MOD(IRECS,1000)   ! find record type
         IF(IRECTP.EQ.1) THEN
C                                  ! check flags if only event records
           CALL OUT_ONLY_EVENTS(.TRUE.) ! are to be written out
           CALL EVTWOS  ! write begin run record
           CALL OUT_ONLY_EVENTS(.FALSE.) ! reset flags
         ENDIF
C
  999  RETURN
C
C
       ENTRY FRSTRN
       SAME=.FALSE.
       RETURN
C
       END                   
