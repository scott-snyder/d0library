C
C======================================================================
        SUBROUTINE DI3INT 
C======================================================================
C
C  Description: Initialze DI-3000 system 
C  ============
C
C
C  Input Arguments:
C  ================
C  
C
C  Output Arguments:
C  =================
C  None
C
C  Preconditions necessary before call:
C  ====================================
C  None
C
C  Author:
C  =======
C  SHARON HAGOPIAN
C
C  Revision History:
C  =================
C  MAR 2, 1986 - Original creation
C
C======================================================================

        IMPLICIT NONE
C
C  Include Statements:
C  ===================
C
        INCLUDE 'D0$INC:GRAPHF77.INC'
C
C  Local Declarations:
C  ===================
C
        REAL VX,VY            ! Locator echo point
C
C  External Declarations:
C  ======================

C  Data Statements:
C  ================
C
      LOGICAL IRIGHT
      DATA IRIGHT/.TRUE./
 
C  Executable Code:
C  ================

  
C  Initialize DI-3000
C  ==================
      CALL JBEGIN
      CALL JDINIT(1)
      CALL JDEVON(1)
C
C  Set the Debug value:
C  ====================
C
C  Set-up Default Values for DI-3000:
C  ==================================

      CALL JDJUST(2,2)
      CALL JDSIZE(2.,4.)
      CALL JDCOLR(COLMAG)
      CALL JDDETE(32767)
      CALL JDPINT(0)
      CALL JFRAME(1)
C      
C  Set-up right-handed coordinate system:
C  ======================================
C
      CALL JRIGHT( .TRUE. )
      CALL JVUPNT( 0., 0., 0. )
      CALL JUPVEC( 0., 1., 0. )
      CALL JNORML( 0., 0., -1. )
C
C  Set-up a default window 
C  =======================
      CALL JWINDO(-120.,120.,-120.,120.)
C
      RETURN
      END
