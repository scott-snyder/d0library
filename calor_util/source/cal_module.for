      FUNCTION CAL_MODULE(IETA,ILYR,NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns module type index given IETA,ILYR
C-                         CCEM = 1  ECEM = 2  CCMG = 3   ICD  = 4
C-                         ECMG = 5  CCFH = 6  ECIH = 7   ECMH = 8
C-                         CCCH = 9  ECOH = 10
C-
C-   Inputs  : IETA, ILYR [I]
C-   Outputs : MODULE NAME [C*4]
C-   Controls: D0$CALOR_OFF:CAL_MODULE.RCP
C-
C-   Created   2-MAR-1992   Harrison B. Prosper, Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) NAME
      INTEGER CAL_MODULE,IETA,ILYR,IER,I,J,K,M,N,LOC,IMOD
      LOGICAL CEXIST,FIRST
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      CHARACTER MODULE(-NETAL:NETAL)*72,CMODULE(10)*4
      CHARACTER HEX*1,FMODULE(1:2)*72
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZLOC('CAL_MODULE_RCP',LOC)
        IF (LOC.EQ.0) THEN
          CALL INRCP('D0$CALOR_OFF:CAL_MODULE.RCP',IER)
        END IF
        CALL EZPICK('CAL_MODULE_RCP')
        CALL EZ_GET_CHARS('MODULE_MAP',N,MODULE(1),IER)
        CALL EZ_GET_CHARS('MODULE_MAP_FIX',N,FMODULE(1),IER)
        DO I = -NETAL,-1
          MODULE(I) = MODULE(-I)
          IF(I.EQ.-12) MODULE(I) = FMODULE(1)
          IF(I.EQ.-14) MODULE(I) = FMODULE(2)
        END DO
        CALL EZ_GET_CHARS('MODULE_NAME',N,CMODULE,IER)
        CALL EZRSET
      ENDIF
      CAL_MODULE = 0
      NAME = ' '
      IER = -1
      IF(.NOT.CEXIST(IETA,1,ILYR)) GOTO 999
      I = 21 + 2*ILYR ! don't mess with this part either
C
C ****  CHECK BOUNDARY
C
      IF(I.LT.23 .or. I.GT.55) GOTO 999
C
C ****  INTERNAL READ MODULE INDEX INTO M AND CMODULE
C
      READ(MODULE(IETA)(I:I),'(Z1)',ERR=999,IOSTAT=IER) M
      CAL_MODULE = M
      NAME = CMODULE(M)
  999 RETURN
      END
