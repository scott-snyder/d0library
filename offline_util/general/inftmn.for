      SUBROUTINE INFTMN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : TO INITIALIZE FATMEN STORE
C-
C-   Inputs  : RZ UNIT, SERVER UNIT, CATALOG NAME
C-   Outputs : TRUE/FALSE
C-   Controls: FATCOM.INC
C-
C-   Created  14-OCT-1991   Krzysztof L. Genser
C-   Updated   4-NOV-1991   Krzysztof L. Genser
C-    to avoid clushes with other COMMON names
C-   Updated   6-MAR-1992   Krzysztof L. Genser
C-    set logl to default, use D0TRNK
C-   Updated  19-MAY-1994   Krzysztof L. Genser  FATLNK,FATCOM->D0FMLK,D0FMCM
C-    
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:FATCOM.INC/LIST'
      INTEGER  LUNRZ,LUNFZ,IER
      INTEGER IUSER
      PARAMETER( IUSER = 111 )
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN

        FIRST =.FALSE.

        CALL MZSTOR(IFMSTR,'/D0FMCM/',' ',IFMFNC,
     &    LFMEV,RFMVEC(LFMR),RFMVEC(LFMW),
     &    RFMVEC(LFM2),RFMVEC(LFMLAS))
        CALL MZDIV (IFMSTR,IFMDIV,'USERS',NFMDII,LFMLAS,'L')
        CALL MZLINK(IFMSTR,'/D0FMLK/',LFMUS1,LFMUSL,LFMUS1)


        CALL GTUNIT (IUSER,LUNRZ,IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('GTUNIT','INFTMN',
     &      'CANT CONTINUE WITHOUT UNIT FOR FATMEN RZ','F')
        ENDIF

        CALL GTUNIT (IUSER,LUNFZ,IER)
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('GTUNIT','INFTMN',
     &      'CANT CONTINUE WITHOUT UNIT FOR FATMEN FZ','F')
        ENDIF

        CALL FMINIT(IFMSTR,LUNRZ,LUNFZ,D0TRNK,IER)

        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('FMINIT','INFTMN',
     &      'CANT CONTINUE WITHOUT INITIALIZING FATMEN','F')
        ENDIF
C
C ****  combine fatmen updates together
C
        CALL FMUPDT(2000,10,0,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('FMUPDT','INFTMN',
     &      ' Problem seting update params ','F')
        ENDIF

      ENDIF
      RETURN
      END
