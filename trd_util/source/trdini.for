      FUNCTION TRDINI
C--------------------------------------------------------------
C
C  Initialization routine for TRD package.
C  Read control parameters.
C
C  Daria Zieminska OCT 1989.
C-   Updated  21-MAR-1991   JFG Includes initialization of EXAMINE
C-   Updated  19-NOV-1991   A. Zylberstejn  Modification to the call to the
C-   booking routines 
C-   Updated  15-NOV-1993   Susan K. Blessing   Add call to INRCPE.
C
C--------------------------------------------------------------
C
      IMPLICIT NONE
      LOGICAL TRDINI,TRDDDF,OK
      LOGICAL TEXAMIN,TTRAKIN
      INTEGER IER,ICALL
      INTEGER DO_HISTO(4)
c      INTEGER DO_HISTO(4),IHBDIR(1)
      CHARACTER*(*) RCPFIL
      CHARACTER*(*) FILRCP
      CHARACTER*3 HBDIR(1)
c      EQUIVALENCE (IHBDIR(1),HBDIR(1))
      PARAMETER( RCPFIL = 'TRD_RCP' )   ! Logical name of control file
      PARAMETER( FILRCP = 'TRDHIT_RCP') ! Logical name of TRDHIT control
                                        ! file
      DATA ICALL/0/
C
      IF (ICALL.NE.0) GO TO 1000
      CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
      OK = IER .EQ. 0
C
C ****  Read in TRD edit rcpfile TRD_RCPE
C
      CALL INRCPE('TRD_RCPE',IER)
      IF ( IER .EQ. 0 ) THEN
        CALL ERRMSG('TRD_RCPE used','TRDINI',
     &    'Default TRD_RCP modified','W')
      ENDIF
C
      CALL EZPICK('TRD_RCP')
      CALL EZGET('TEXAMIN',TEXAMIN,IER) ! Examin hits switch
      OK = (IER .EQ. 0).AND.OK
      CALL EZGET('TTRAKIN',TTRAKIN,IER) ! Tracking switch
      CALL EZRSET
      OK = (IER .EQ. 0).AND.OK
      OK=TRDDDF().AND.OK       !  Get list of banks to dump
      CALL TRDDRP(IER)
      OK = (IER .EQ. 0).AND.OK
      IF (ICALL.EQ.0) THEN
        ICALL=1
      END IF
C Histogram booking put in TRDPAR
c      IF (TEXAMIN) THEN
c        CALL TRDBOK_ON
c      ENDIF
c      IF(TTRAKIN)        CALL TRDBOK
 1000 CONTINUE
      TRDINI=OK
      RETURN
      END
