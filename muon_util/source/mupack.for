      SUBROUTINE MUPACK(IMOD,IPLN,IWIR,IWADD,IERR)
C=======================================================================
C
C  Description:  Packs the wire address.... to be kept up to date with
C  ============  subroutine MUADD.
C
C  Author:
C  ========
C  Tami Kramer
C
C  Revision History:
C  ==================
C  Original Creation - August 11,1988
C  DH 10/89   new format
C  DH 11/90 use version number
C=========================================================================
C
      IMPLICIT NONE
      INTEGER IMOD,IPLN,IWIR,IWADD,IERR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IFIRST,MC,GZMUD1,L
      DATA MC,IFIRST/0,0/
      IF(IFIRST.EQ.0) THEN
        IFIRST=1
        L=GZMUD1(0)
        IF(IQ(L+4).EQ.1) MC=1      ! MONTE CARLO
      ENDIF
      IF(MC.EQ.1) THEN     ! MONTE CARLO
        IWADD = IMOD*128 + IPLN*32 + IWIR
      ELSE
        IWADD = IMOD*256 + IWIR*4 + IPLN
      ENDIF
      RETURN
      END
