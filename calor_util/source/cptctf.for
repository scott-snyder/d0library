      SUBROUTINE CPTCTF 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Fill pointers for CATE bank if it exists already 
C-
C-   Created   19-APR-1989   Andrzej Zieminski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:PTCATE.INC'
C
      INCLUDE 'D0$LINKS:IZCATE.LINK'
C
      INTEGER I 
      INTEGER LCATE,GZCATE 
      INTEGER IETA,IPHI,IHD,POINTR
      INTEGER NCH,NR,NTOT 
C----------------------------------------------------------------------
C
      IF(.NOT.PTTFLG) GOTO 999    ! PTCATE array has been done --> do nothing
C
      LCATE=GZCATE()
      IF(LCATE.LE.0) THEN        
        CALL ERRMSG(' No CATE bank','CPTCF',
     &      ' Cannot fill PTCATE array','W')
          GOTO 999
      ENDIF
C
C        loop over CATE towers
C
      NCH=IQ(LCATE+3)
      NCH=MOD(NCH,10000)
      NR=IQ(LCATE+2)
      POINTR=LCATE
      IF(NCH.LE.0 .OR. NR.LE.0) GOTO 999
      DO 1 I=1,NCH
        IETA=IQ(POINTR+12)
        IPHI=IQ(POINTR+13)
        IHD =IQ(POINTR+14)
        PTCATE(IETA,IPHI,IHD)=I
        POINTR=POINTR+NR
    1 CONTINUE
C
      PTTFLG=.FALSE.           ! set flag indicating PTCATE is not 0
  999 RETURN
      END
