      SUBROUTINE CJETN90(NCELLS,ETJET,PTRCATE,N90)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS NUMBER OF TOWERS COMPRISING 90% OF JET ET
C-
C-
C-   Inputs  : NCELLS:  NUMBER OF CELLS IN JET
C-             ETJET :  ET OF JET
C-             PTR: POINTER TO CATE TOWERS IN JET
C-
C-   Outputs : N90   : NUMBER OF TOWERS WHICH CONSTITUTE 90% OF JET ET
C-
C-   Controls: NONE
C-
C-   Created  11-JAN-1993   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
      INTEGER I,NCH
      INTEGER NCELLS,PTRCATE(2000),IMAP(2000)
      INTEGER PTR(2000),PTOWERS(2000)
      REAL ETTOWER(2000)
      INTEGER POINTER,N90,OLD,NTOWERS,GZCATE,LCATE,NREPCATE
      REAL    ETJET,ESUM,CUT
C----------------------------------------------------------------------
C
      NCH   = NCELLS                  ! RVA
      LCATE = GZCATE()
      NREPCATE = IQ(LCATE+2)
C
      IF(NCELLS.GT.2000) THEN
        CALL ERRMSG('CJETN90','TOO MANY CELLS',
     &      ' TRUNCATED TO 2000','W')
        NCH = 2000
      ENDIF
      CALL UCOPY(PTRCATE,PTR,NCH)
      CALL SRTINT(PTR,NCH,IMAP)
      OLD = 0
      NTOWERS = 0
      DO I = 1,NCH
        IF(PTR(I) .NE. OLD) THEN
          PTOWERS(NTOWERS+1) = PTR(I)
          NTOWERS = NTOWERS+1
          POINTER = LCATE + (NREPCATE*(PTOWERS(NTOWERS)-1)+4)
          ETTOWER(NTOWERS) = Q(POINTER+4)
        ENDIF
        OLD = PTR(I)
      ENDDO
C
      ESUM = 0.
      N90 = 0
      CUT = 0.9*ETJET
      DO WHILE (ESUM .LE. CUT)
        N90 = N90+1
        ESUM = ESUM + ETTOWER(N90)
        IF(N90 .GE. NTOWERS) THEN         !FOR BUGGY EVENTS...
          N90  = NTOWERS
          ESUM = ETJET
        ENDIF
      ENDDO
C
  999 RETURN
      END
