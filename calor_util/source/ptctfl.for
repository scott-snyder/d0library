      SUBROUTINE PTCTFL(OK)
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
      LOGICAL OK
      INTEGER I 
      INTEGER LCATE,GZCATE 
      INTEGER IETA,IPHI,IHD,POINTR
      INTEGER NCH,NR,NTOT 
C----------------------------------------------------------------------
C
      OK=.TRUE.
      LCATE=GZCATE()
      IF(.NOT.PTTFLG) GOTO 999    ! PTCATE array has been done --> do nothing
C
      OK=.FALSE.
      IF(GZCATE().LE.0) THEN        ! Abort if CATE does not exist 
        CALL ERRMSG('CAHITS',' PTCTFL',
     &    'Trying to fill PTCATE without CATE bank','F')
      ENDIF
C
C        initialize
C
      PTTFLG=.TRUE.
      NTOT=(2*NETAL+1)*NPHIL*2
      CALL UZERO(PTCATE,1,NTOT)
C
C        loop over CAEH towers
C
      NCH=IQ(LCATE+3)
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
      OK=.TRUE.
  999 RETURN
      END
