      SUBROUTINE CTRAST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Trap CTRL-C or CTRL-Y in full screen mode to 
C-                         sure cursor is put back on screen. VAX-specific.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 26-SEP-1988   Jan S. Hoftun
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE '($TRMDEF)'
      INTEGER ISTAT,LIBERA,CURONF,SMG$CANCEL_INPUT
      INTEGER SMG$READ_STRING,IMOD
      LOGICAL YN
      CHARACTER*132 OUTTXT
C----------------------------------------------------------------------
      ISTAT=CURONF(0)
      ISTAT=LIBERA(1,1)
      ISTAT=SMG$CANCEL_INPUT(KEYID)
      IMOD=IOR(IOR(TRM$M_TM_TRMNOECHO,TRM$M_TM_NORECALL),TRM$M_TM_PURGE)
      ISTAT=SMG$READ_STRING(KEYID,OUTTXT,,132,IMOD,0,,,,MAINID,)
      CALL GETPAR(1,'Do you REALLY want to terminate program ? (Y/N) >',
     *     'L',YN)
      IF(YN) THEN 
         CALL EXIT(1)
      ELSEIF(CURLEV.GT.0) THEN
         CALL MENDIS(.TRUE.)
      ENDIF
C&ENDIF
      RETURN
      END
