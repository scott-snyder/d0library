      SUBROUTINE TCORFL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill in the contents of the bank TCOR.
C-
C-   Inputs  :
C-
C-   Outputs :
C-   Controls:
C-
C-   Created  26-FEB-1996   A. ZYLBERSTEJN
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTCOR
      INTEGER I,NP,NPOINT
C      PARAMETER( NPOINT =4500 )
      REAL R1(10)
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      INTEGER GZTCOR
C----------------------------------------------------------------------
      LOGICAL FIRST,OK
      SAVE FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C      IF ( FIRST ) THEN
C do initialization here if necessary.
C        FIRST = .FALSE.
C      ENDIF
C
      IF ( LTCOR .LT. 0 ) THEN
        LTCOR = GZTCOR()    ! GET LINK.
      ENDIF
C
      IF ( LTCOR .EQ. 0 ) THEN
        CALL BKTCOR(LTCOR)
      ENDIF
      IF(LTCOR.LE.0)GO TO 999
      NPOINT=IC(LTCOR-1)
C      print*,' in TCORFL, nb. de pts max',npoint
      C(LTCOR+1) = 0.               ! Bank version
C fill in the rest of the bank here.
      CALL D0OPEN (65,'TRD_COR_TEMP','I',OK)
      IF (.NOT.OK) THEN
        CALL ERRMSG(' error reading TRD_COR_TEMP','TCORFL',
     &       ' file does not exist','w')
        GO TO 8
      END IF
      NP=1
    6 READ(65,1001,END=8,ERR=8)(R1(I),I=1,10)
      IF(NP.GT.NPOINT)THEN
        CALL ERRMSG(' error reading TRD_COR_TEMP','TCORFL',
     &       ' too many input points','w')
C        print*,' npoint,np',npoint,np
        NP=NP-1
        GO TO 8
      END IF
      DO I=1,10,2
        NP=NP+1
        C(LTCOR+NP)=R1(I)
        NP=NP+1
        C(LTCOR+NP)=R1(I+1)
      END DO
      GO TO 6
    8 CONTINUE
      CLOSE(65)
 1001 FORMAT(5(F10.4,F6.3))
C
  999 RETURN
      END
