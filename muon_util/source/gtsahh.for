      SUBROUTINE GTSAHH(MODNO,IMUD1,NRAW)
C=====================================================================
C
C  Description:  Gets hit information from bank SAHH and MUD1 for
C                a half-plane with ID = MODNO
C  ============
C
C  Argument Descriptions:
C  ======================
C  MODNO - Integer - Input - ID number for MAC card that produced data
C  IMUD1 -  I - Output - LOCATION IN MUD1 OF FIRST DATA WORD FOR PLANE
C  NRAW -   I - Output - NUMBER OF RAW HITS IN PLANE
C
C  Author
C  ===============
C  M. Fortner 11/91
C
C-   Updated  20-JAN-1992   Daria Zieminska  eliminate dummy arg. in GZSAHH 
C
C-   Fixed a bug in the pointer JHEADW  03-15-1992  - K. Bazizi
C
C-   Update the word count NRAW to match the change from 16 bits 
C-   to 12 bits only for the word count. see MUD1 description
C                                  11-16-1992 - K. Bazizi and G. Lima
C
C======================================================================
C
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER MODNO,NRAW,IMUD1
      INTEGER LSAHH,GZSAHH,LMUD1,GZMUD1
      INTEGER I,ISTA,JMUD1,JHEADW,B12
	integer ishft,itest
      DATA B12/z'FFF'/
C
      NRAW = 0
      LSAHH = GZSAHH()
      IF (LSAHH.EQ.0) RETURN
      LMUD1 = GZMUD1(0)
      ISTA = MODNO/10 - 40              ! Select station 0 to 5
      IF (ISTA.LT.0.OR.ISTA.GE.6) RETURN
      JMUD1 = IQ(LSAHH+19+ISTA)         ! Pointer to first data in crate
      IMUD1 = JMUD1
C
C               Loop over possible modules in this crate
      DO I = -7,-2
        JHEADW = IQ(LMUD1+JMUD1+I)
        NRAW = IAND(JHEADW,B12)/3
        IF (MODNO.EQ.ISHFT(JHEADW,-16)) RETURN  ! Success
        IMUD1 = IMUD1 + NRAW*3
      END DO
      NRAW = 0                          ! Failure to find module
      RETURN
      END
