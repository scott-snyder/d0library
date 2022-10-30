      SUBROUTINE TRREAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads control parameters in TRD_RCP once
C-                         for all. Fills TCNTRL.INC, as well as
C-                         various other commons.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-NOV-1989   J.Fr. Glicenstein
C-   Updated  17-NOV-1989   A. Zylberstejn  :Introduce definition of min.
C-                                           radius and Z
C-   Updated  24-NOV-1989   J.Fr. Glicenstein  Reads all controls parameters
C-   Updated   4-JAN-1990   J.Fr. Glicenstein  Debugging flags
C-   Updated  27-JAN-1993   Alain PLUQUET Change EPICOR and efficiency switches
C-   Updated  23-SEP-1993   J.P. Cussonneau  Reads new epicor array for MC data
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:TRDVOL.INC'
      INCLUDE 'D0$INC:TRDBGU.INC'
      INCLUDE 'D0$INC:GEOMTR.INC'
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:GCONST.INC'
      INCLUDE 'D0$INC:TRINFO.INC'
      INCLUDE 'D0$INC:TRINTR.INC'
      INCLUDE 'D0$INC:TRTOBN.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER IDENT,MASK16,LOUT,NTOTEV,ITRA,TRUNIT,IER,IBC
      INTEGER IOPTRD,ITRANC(14),ITRANI(4),USUNIT
      INTEGER TRD_FIND_VER
      CHARACTER*4 TCAVAL(20)
      LOGICAL DO_CORRECTION,doprint,trd_do_print
      EQUIVALENCE (IOPTRD,OPT),(ITRANI(1),TVALIN(1)),
     &(ITRANC(1),TCAVAL(1))
      REAL STEPPH
      DATA MASK16/65535/
      PRTRD=5
      STEPPH=TWOPI/256.
C ****  Define the limits of the TRD RMIN, ZMIN, RMAX, ZMAX
      RMIN = RADWIN(1)
      RMAX = RADEXT(3)
      ZMIN =ZACT(1)
      ZMAX = ZMIN
      CALL EZPICK('TRD_RCP')
      CALL EZGET_i('TYPPRO',TYPPRO,IER)
      CALL EZGET_i('OPT',IOPTRD,IER)
      CALL EZGET('THMIN',THMIN,IER)
      CALL EZGET('THMAX',THMAX,IER)
      CALL EZGET_i('SWTLYR',JSWITC(1),IER)
      CALL EZGET('ETOTM',ETOTM,IER)
      CALL EZGET_l('COR_EPI',DO_CORRECTION,IER)
      IF (DO_CORRECTION) THEN
        IF (MCDATA.AND.TRD_FIND_VER().EQ.1) THEN
          CALL EZGET('NEW_EPICOR',EPICOR(1),IER)
        ELSE
          CALL EZGET('EPICOR',EPICOR(1),IER)
        ENDIF
      ELSE
        EPICOR(1)=1.
        EPICOR(2)=1.
        EPICOR(3)=1.
      ENDIF
      CALL EZGET_i('CLUSTER_RECONS',ITRANC(7),IER)
      CALL EZGET_i('CALIBRATION',ITRANC(8),IER)
      CALL EZGET_i('ALIGNMENT',ITRANC(9),IER)
      CALL EZGET_i('VOL0_CORRECTION',ITRANC(10),IER)
      CALL EZGET_i('ONE_ANOD_CELL',ITRANC(12),IER)
      CALL EZGET_i('CONVERTED_GAMMAS',ITRANC(13),IER)
*****   Character conversion (very ackward!!)
      DO 5 IBC=1,20
    5 TVALCA(IBC)=TCAVAL(IBC)
*****   If no correction by Epion, no efficiency calculation
      IF (TVALCA(1).EQ.'NO ') THEN
        DO 10 IBC=2,6
   10   TVALCA(IBC) = 'NO '
      ENDIF
*****
      CALL EZGET_i('FADC_DRAW',ITRANI(1),IER)
      CALL EZGET_i('TRK_FADC_DRAW',ITRANI(2),IER)
      CALL EZGET_i('ALL_FADC_DRAW',ITRANI(3),IER)
      IF (ITRANI(3).LT.ITRANI(2)) ITRANI(3) = ITRANI(2)
      CALL EZGET_i('CLUSTER_THRESHOLD',ITRANI(4),IER)
*****
      CALL EZGET_l('TEXAMIN',TEXAMIN,IER)
      CALL EZGET_l('TTRAKIN',TTRAKIN,IER)
      CALL EZGET_l('TRDPEDES',TRDPEDES,IER)
      CALL EZGET_l('TRDGAIN',TRDGAIN,IER)
      CALL EZRSET
      LOUT=TRUNIT()
      IF (TRD_DO_PRINT())THEN
        WRITE (LOUT,*) '****  TRD debugging option selected  ****'
        WRITE (LOUT,*) TNEVDG,'  events will be debugged'
        WRITE (LOUT,*) TNTRDG,'  tracks/events'
        WRITE(LOUT,*)' In TRREAD,epicor',epicor
      ENDIF
  999 RETURN
  666 FORMAT(20A3)
      END
