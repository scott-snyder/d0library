      SUBROUTINE PRFITR( LUNPR, LJFITR, NFITR, CFL, IFL )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the bank FITR
C-
C-   Inputs  : LUNPR  [I] : Unit for output
C-             LKFITR [I] : Pointer on the FITR bank. MANDATORY
C-             NFITR  [I] : Number of the FITR bank ( dummy )
C-             CFL   [C*] : Character flag.   ( dummy )
C-             IFL    [I] : Level of printing ( dummy )
C-   Outputs :
C-
C-   Created  30-JUN-1988   Ghita Rahal-Callot
C-   Updated  30-JAN-1989   Jeffrey Bantly   for the FDC
C-   Updated  20-MAR-1990   Jeffrey Bantly  add degree angles 
C-   Updated   1-MAR-1993   Susan K. Blessing  Set ICALL=0 and then to 1 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      INTEGER LSFITR, NTRAC, I, J, IER
      INTEGER LUNPR, LKFITR, LJFITR, NFITR, IFL, ICALL
      INTEGER GZFITR
      REAL    DEGTHE,DEGPHI
      REAL     XVER,YVER,RVER,ZVER,XIN,YIN,RIN,ZIN,ZMIN,DIR,Z0(2)
      CHARACTER*(*) CFL
      SAVE Z0
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF(ICALL.EQ.0) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('Z0',Z0,IER)
        CALL EZRSET
        ICALL = 1
      ENDIF
      LKFITR = LJFITR
      IF ( LKFITR .LE. 0 ) LKFITR = GZFITR()
      IF ( LKFITR .LE. 0 ) GO TO 999
      NTRAC = IQ ( LKFITR + 1 )
      WRITE ( LUNPR, 12) NTRAC
   12 FORMAT(10X,' Number of ISAJET tracks inside the FDC is ',I4)
      IF ( NTRAC .LE. 0 ) GO TO 999
      WRITE ( LUNPR,13)
   13 FORMAT(/,5X,'X ver',5X,'Y ver',5X,'Z ver',3X,' Phi (r&deg)',
     &         3X,' Theta (deg&r)',2X,'Momentum',5X,'Mass',6X,'Trk N',
     &         1X,'asso track')
      LSFITR = LKFITR + 2
      DO 100 I = 1, NTRAC
        DEGPHI=Q(LSFITR+4)*360./TWOPI
        DEGTHE=Q(LSFITR+5)*360./TWOPI
        WRITE(LUNPR,14) (Q(LSFITR+J),J=1,4),DEGPHI,DEGTHE,
     &             (Q(LSFITR+J),J=5,IQ(LKFITR+2))
   14   FORMAT(3F10.5,F8.2,2F8.1,F8.2,2F10.5,2F10.0)
C
        XVER=Q(LSFITR+1)                ! Track vertex X
        YVER=Q(LSFITR+2)                ! Track vertex Y
        ZVER=Q(LSFITR+3)                ! Track vertex Z
        IF( Q(LSFITR+5) .LE. HALFPI ) DIR = 1.    ! Half 1
        IF( Q(LSFITR+5) .GE. HALFPI ) DIR = -1.   ! Half 0
        ZMIN = ABS(Z0(1))*DIR               ! Front edge of FDC
        ZIN  = ZMIN - ZVER              ! Track length before entry into FDC
        RIN  = ABS(ZIN) * TAN(Q(LSFITR+5))      ! XY radius before entry
        XIN  = ABS(RIN)*COS( Q(LSFITR+4) )      ! X before entry
        YIN  = ABS(RIN)*SIN( Q(LSFITR+4) )      ! Y before entry
C
        LSFITR = LSFITR + IQ (LKFITR+2)
  100 CONTINUE
      GOTO 999
  998 CONTINUE
C-----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
