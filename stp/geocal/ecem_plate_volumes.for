      SUBROUTINE ECEM_PLATE_VOLUMES(NLINES,LINE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plate level ECEM volumes
C-
C-   Inputs  :  NLINES  Current index in LINE array
C-              LINE    Character array
C-   Outputs :  NLINES  Updated index in LINE array
C-   Controls:  none
C-
C-   Created  21-MAR-1990   Stuart Fuess
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER NLINES
      CHARACTER*(*) LINE(*)
C  Integers
      INTEGER I, J
C  Data
      CHARACTER*8 PLATE_MATERIAL(5)
      DATA PLATE_MATERIAL/'ABSORBER',
     &                    'ARGON_A ',
     &                    'MLB     ',
     &                    'COPPER  ',
     &                    'ARGON_B '/
C----------------------------------------------------------------------
C  One of a kind volumes
C----------------------------------------------------------------------
      LINE(NLINES+1)  = '''ECEM_PLATE_MODULE_VOLUME'''
      LINE(NLINES+2)  = '''ECEM+Z_SUPPORT_PIPE'''
      LINE(NLINES+3)  = '''ECEM+Z_READOUT_12_MLB'''
      LINE(NLINES+4)  = '''ECEM+Z_READOUT_12_COPPER'''
      NLINES = NLINES + 4
C----------------------------------------------------------------------
C  Loop over ECEM elements
C----------------------------------------------------------------------
      DO I=1,19
       DO J=1,5
        IF(I.EQ.19.AND.J.GE.2)GOTO 100
        NLINES = NLINES + 1
        WRITE(LINE(NLINES),1001)I,PLATE_MATERIAL(J)
       ENDDO
      ENDDO
 100  CONTINUE
      RETURN
 1001 FORMAT('''ECEM+Z_',I2.2,'_',A8,'''')
      END
