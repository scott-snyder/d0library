      SUBROUTINE CLDROP(MXCACL,THREN,THRET,THREM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reject clusters failing E, ET or EM cuts
C-
C-   Inputs  : MXCACL, THRET and THREN,THREM
C-   Outputs :
C-   Controls:
C-
C-   Created   7-MAY-1989   Rajendran Raja
C-   Modified 12-OCT-1989       Gerald C. Blazey
C-   Modified 28-Mar-1990    N. A. Graf Added EM ratio cut
C-   Updated  21-AUG-1990   Norman A. Graf  Added EFH to calculate 
C-                          EM_RATIO correctly 
C-   Updated  16-APR-1993   Norman A. Graf  sort on Et of CACL banks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER MXCACL,GZCACL
      REAL THRET,THREN,THREM,ET,E,EFH
      REAL ECLUS_TOT,EM_RATIO
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
      ENDIF
C
      LCACL = GZCACL()                    ! 1st Bank
      IF(LCACL.EQ.0)THEN
        CALL ERRMSG('Calorimeter','CLDROP',
     &    'NO CACL BANKS LEFT TO DROP','W')
        RETURN
      ENDIF
C
C ****  Now to cycle through banks and drop them. Associated CACH banks
C ****  will be automatically dropped.
C
      DO WHILE (LCACL.GT.0)
      E = Q(LCACL+7)
      ET = Q(LCACL+8)
      ECLUS_TOT = Q(LCACL+17)
      EFH = Q(LCACL+19)
C
      IF(ABS(ECLUS_TOT).GT.1.E-8)THEN
        EM_RATIO = (E-EFH)/ECLUS_TOT
C        EM_RATIO = E/ECLUS_TOT
C
C ****  going back to previous cut , till this has been approved by Electron_id
C ****  group. R.Raja 2-Oct-1992
C
      ELSE
        EM_RATIO=0.0
      ENDIF
C
      IF(E.LT.THREN .OR. ET.LT.THRET .OR. EM_RATIO.LT.THREM) THEN
        CALL MZDROP(IXCOM,LCACL,' ')    ! Mark it for dropping
      ENDIF
      LCACL = LQ(LCACL)                 ! Go to next one
      ENDDO
C
C ****  now sort on Et of banks
C
      LCACL = GZCACL()                    ! 1st Bank
      CALL ZSORT(IXCOM,LCACL,8)
      LCACL = GZCACL()                    ! 1st Bank
      CALL ZTOPSY(IXCOM,LCACL)
C
  999 RETURN
C
      END
