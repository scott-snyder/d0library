      SUBROUTINE MU_MAG_POL(ICONT,PWAM,PSAM,IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get magnet polarities from local variables
C-                 -or-                        GLOB bank
C-                 -or-                        DBMON database
C-
C-   Inputs  : ICONT = control word  0 ==> return local values
C-                                   1 ==> update local values   
C-                                   2 ==> update local values, but 
C-                                         avoid data base
C-   Outputs : PWAM = WAMUS magnet polarity (1.,-1.,0.)=(For,Rev,Off)
C-             PSAM = SAMUS magnet polarity (1.,-1.,0.)=(For,Rev,Off)
C-             IERR = Error code, 0=OK
C-                                BTEST(IERR,0)==> error getting WAMUS
C-                                BTEST(IERR,1)==> error getting SAMUS
C-   Controls: 
C-
C-   Created  24-APR-1993   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL PWAM,PSAM,PWAM_LOC,PSAM_LOC
      INTEGER IERR,LGLOB,IPWAM,IPSAM,IERR_DB,IVER_GLOB,ICONT
      LOGICAL CHECK_DB
      INTEGER GZGLOB
C
      SAVE PWAM_LOC,PSAM_LOC
C
      EXTERNAL GZGLOB
C----------------------------------------------------------------------
      IF(ICONT.EQ.0) THEN
        IERR = 0
      ELSE  
C default values
        PWAM_LOC = 1.
        PSAM_LOC = 1.
        IERR = 3
C check GLOB bank
        LGLOB = GZGLOB(0)
        CHECK_DB = .TRUE.
        IPWAM = 2
        IPSAM = 2
C magnet polarities are saved in GLOB bank version 3 or higher
        IF(LGLOB.GT.0) THEN
          IVER_GLOB = IQ(LGLOB+1)
          IF(IVER_GLOB.GE.3) THEN
            IPWAM = IQ(LGLOB+19)
            IPSAM = IQ(LGLOB+20)
            CHECK_DB = .FALSE.
          ENDIF  
        ENDIF  
C check database only if necessary, and never when ICONT=2
        IF(CHECK_DB .AND. ICONT.NE.2) THEN
          CALL MAG_POLARITY(IPWAM,IPSAM,IERR_DB)
          LGLOB = GZGLOB(0)
        ENDIF  
C accept only standard values (-1,1,0) of WAMUS polarity
        IF(ABS(IPWAM).EQ.1 .OR. IPWAM.EQ.0) THEN
          PWAM_LOC = FLOAT(IPWAM)
          IERR = IBCLR(IERR,0)
        ENDIF  
C accept only standard values (-1,1,0) of SAMUS polarity
        IF(ABS(IPSAM).EQ.1 .OR. IPSAM.EQ.0) THEN
          PSAM_LOC = FLOAT(IPSAM)
          IERR = IBCLR(IERR,1)
        ENDIF  
        IF(CHECK_DB  .AND. ICONT.NE.2) IERR = IERR_DB
      ENDIF
C
      PWAM = PWAM_LOC
      PSAM = PSAM_LOC
C          
  999 RETURN
      END
