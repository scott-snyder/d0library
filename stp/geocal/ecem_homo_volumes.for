      SUBROUTINE ECEM_HOMO_VOLUMES(NLINES,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Homogeneous level ECEM volumes
C-
C-   Inputs  :  NLINES  Current index in LINE array
C-              LINE    Character array
C-   Outputs :  NLINES  Updated index in LINE array
C-   Controls:  none
C-
C-   Created  26-MAR-1990   Stuart Fuess
C-   Modified 20-MAY-1990   Natalie Roe   add EM3A,B,C and EM4A,B
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER NLINES
      CHARACTER*(*) LINE(*)
C----------------------------------------------------------------------
C  Select the TBM SRCP file
C----------------------------------------------------------------------
      CALL EZPICK ( 'TESTBM' )
C----------------------------------------------------------------------
C  One of a kind volumes
C----------------------------------------------------------------------
      LINE(NLINES+1)  = '''ECEM_HOMO_MODULE_VOLUME'''
      LINE(NLINES+2)  = '''ECEM+Z_SUPPORT_PIPE'''
      LINE(NLINES+3)  = '''ECEM+Z_FLOOR1_VOLUME'''
      LINE(NLINES+4)  = '''ECEM+Z_FLOOR2_VOLUME'''
      LINE(NLINES+5)  = '''ECEM+Z_FLOOR3A_VOLUME'''
      LINE(NLINES+6)  = '''ECEM+Z_FLOOR3B_VOLUME'''
      LINE(NLINES+7)  = '''ECEM+Z_FLOOR3C_VOLUME'''
      LINE(NLINES+8)  = '''ECEM+Z_FLOOR4A_VOLUME'''
      LINE(NLINES+9)  = '''ECEM+Z_FLOOR4B_VOLUME'''
      LINE(NLINES+10) = '''ECEM+Z_X_FLOOR1_VOLUME'''
      LINE(NLINES+11) = '''ECEM+Z_X_FLOOR2_VOLUME'''
      LINE(NLINES+12) = '''ECEM+Z_X_FLOOR3_VOLUME'''
      LINE(NLINES+13) = '''ECEM+Z_X_FLOOR4_VOLUME'''
      LINE(NLINES+14) = '''ECEM+Z_11_ABSORBER'''
      NLINES = NLINES + 14
      RETURN
      END
