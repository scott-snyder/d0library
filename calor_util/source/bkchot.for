      SUBROUTINE BKCHOT(NTOT,LCHOT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Booking CHOT bank for calorimeter hot channels.
C-
C-   Inputs  : ND - length of bank
C-   Outputs : LCHOT - address of bank
C-   Controls: none
C-
C-   Created  26-JAN-1993   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCHOT.LINK'
      INTEGER NL,NS,ND,NIO,LCHOT,NTOT,NH,NV
      INTEGER GZCHOT,GZCGEV,LCGEV,NR
      PARAMETER( NL = 2 )               ! NUMBER OF LINKS
      PARAMETER( NS = 2 )               ! NUMBER OF STRUCTURAL LINKS
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      LCHOT = 0
      IF (FIRST) THEN
        NH = 5              ! Number of header words
        NV = 1              ! Version number
        CALL MZLINK(IXSTP,'/ZEBSTP/',LCHOT,LCHOT,LCHOT)
        CALL STP_INZLNK
        CALL MZFORM('CHOT','-I',NIO)
        FIRST = .FALSE.
      ENDIF
C
      LCGEV = GZCGEV()
      NR = 0
      IF (LCGEV.EQ.0) CALL BKCGEV(NR,LCGEV)
      LCHOT = GZCHOT()
      IF (LCHOT.EQ.0) THEN
        ND = NH + NTOT
        CALL MZBOOK(IDVSTP,LCHOT,LCGEV,-IZCHOT,'CHOT',NL,NS,ND,NIO,0)
        IC(LCHOT+1) = NH    ! Number of header words
        IC(LCHOT+2) = NV    ! Version number
      ENDIF
C
  999 RETURN
      END
