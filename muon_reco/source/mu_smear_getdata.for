      SUBROUTINE MU_SMEAR_GETDATA(NMOD,TEMPDAT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: NMOD = 0 on first call. After, it is PDT for which getting
C-             smearing data.
C-
C-   Created   5-MAR-1993   Tom Diehl
C-   Modified 23-MAY-1994   Paul Quintas: Make IBM compatable
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INTEGER MAXNUM,NUMVAR
      PARAMETER (MAXNUM=164)
      PARAMETER (NUMVAR=8)
      REAL DATA(MAXNUM,NUMVAR)
      REAL TEMPDAT(NUMVAR)

      INTEGER I,J,IER,NMOD,MODULE,MUNMOD2
      CHARACTER*9 TEMP
      LOGICAL FIRST

      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        IER = 0
        DO I = 1,MAXNUM
          MODULE = MUNMOD2(1,I)
          IF(IER.EQ.0) THEN
            IF(MODULE.LT.100) THEN
              WRITE(TEMP,621),MODULE
            ELSE
              WRITE(TEMP,756),MODULE
            ENDIF
            CALL EZGET(TEMP,TEMPDAT,IER)
            DO J = 1,NUMVAR
              DATA(I,J) = TEMPDAT(J)
            ENDDO
          ELSE
            CALL ERRMSG('Error return in S/R MUSMEAR_GETDATA',
     +                  'MUSMEAR_GETDATA' ,'stop processing.','F')
            STOP
          ENDIF
        ENDDO
      ENDIF

      IF(NMOD.NE.0) THEN
        MODULE = MUNMOD2(2,NMOD)
        DO I = 1,NUMVAR
          TEMPDAT(I) = DATA(MODULE,I)
        ENDDO
      ENDIF

  621 FORMAT('M0',I2,'_DAT')
  756 FORMAT('M',I3,'_DAT')

  999 RETURN
      END
