      FUNCTION PTCAEP2(LAYERC,IPHIC,IETAC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate an array of pointers to the level 2 CAEP
C-   bank.  NOTE THAT THE INDICES ARE IN A DIFFERENT (more efficient) order
C-   than PTCAEP
C-
C-******************************************************************************
C-WARNING--this contains PTR2.DEF, which must be modified each time PTCAEP2.DEF
C-is modified.
C-******************************************************************************
C-
C-      This routine also contains some checking as to whether
C-      the requested information is valid
C-
C-   Returned value  : As for PTCAEP, the values are zero if the cell does not
C-      exist, or was zero supressed.
C-   Inputs  : LAYERC,IPHIC,IETAC the offline coordinates of the requested cell
C-   Controls:
C-
C-   Created   1-NOV-1990   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LAYERC,IETAC,IPHIC
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1PHP.PARAMS'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PHTT.INC'
      INCLUDE 'D0$INC:TTDONE.INC'
      INCLUDE 'D0$INC:TTMGETA.INC'
      INCLUDE 'D0$CALOR_FILTER$SOURCE:PTR2.DEF' ! pointer array
      CHARACTER*80 VARMSG               ! error message text
      LOGICAL FIRST                     ! remember initialization
      INTEGER TTETA,TTPHI       ! bounds in TT coords; loop indices
      LOGICAL BTEST
C&IF VAXVMS,VAXELN
C&ELSE
C&      EXTERNAL BTEST
C&ENDIF
C----------------------------------------------------------------------
      PTCAEP2 = 0
C
C...check validity of arguments
      IF ( (IETAC.LT.-NETAL).OR.(IETAC.GT.NETAL).OR.
     &     (IPHIC.LT.1)     .OR.(IPHIC.GT.NPHIL).OR.
     &     (LAYERC.LT.1)    .OR.(LAYERC.GT.NLYRL))      THEN
        WRITE(VARMSG,100)LAYERC,IPHIC,IETAC
  100   FORMAT(' LAYERC = ',I3,' IPHIC = ',I3,' IETAC= ',I4)
        CALL ERRMSG('args out of limits','PTCAEP2',VARMSG,'E')
        GO TO 999
      ENDIF
      IF (IETAC.EQ.0) THEN
C        CALL ERRMSG('ietac = 0','PTCAEP2',' ','W')
C        GO TO 999
      ENDIF
C
C...see if any conversion has been done this event
      IF(CL2CAEP_EVT.NE.IQ(LHEAD+7)) THEN
        WRITE(VARMSG,110)IQ(LHEAD+7),CL2CAEP_EVT
  110   FORMAT(' Current Evt: ',I10,' Valid Evt: ',I10)
        CALL ERRMSG('Pointer Info Invalid for this event',
     &    'PTCAEP2',VARMSG,'E')
        GO TO 999
      ENDIF
C
C...check whether the conversion had been done before, as required
      TTETA = PHTTETA(IETAC)
      TTPHI = PHTTPHI(IPHIC)
      IF ((LAYERC.GE.MNLYMG).AND.(LAYERC.LE.MXLYMG)
     &  .AND.TTMGETA(TTETA)) THEN
C
C... check for massless gap usage: corresponding TTOWER w/ MG/ICD info should
C... have been converted
        IF( .NOT.BTEST(TTDONE(SIGN(NETAL11,TTETA)),TTPHI-1) )THEN
          WRITE(VARMSG,100)LAYERC,IPHIC,IETAC
          CALL ERRMSG('MG/ICD info used before CL2 conversion',
     &          'PTCAEP2',VARMSG,'W')
          GO TO 999
        ENDIF
      ELSE
C
C...not a Massless Gap--be sure this tower has been converted
        IF( .NOT.BTEST(TTDONE(TTETA),TTPHI-1) )THEN
          WRITE(VARMSG,100)LAYERC,IPHIC,IETAC
          CALL ERRMSG('CAL info used before CL2 conversion',
     &          'PTCAEP2',VARMSG,'E')
          GO TO 999
        ENDIF
      ENDIF
C
C...now cough up the information
      PTCAEP2 = PTR2(LAYERC,IPHIC,IETAC)
  999 RETURN
      END
