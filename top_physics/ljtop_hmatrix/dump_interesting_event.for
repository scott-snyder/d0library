      SUBROUTINE DUMP_INTERESTING_EVENT(P6_ELECTRON,NELEC,P5_PHOTON,
     &    NPHO,P5_MUON,NMUO,ELECTRON,PHOTON,MUON)

C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : P6_ELECTRON = 6 VECTOR OF ELECTRON
C-             NELEC = NUMBER OF ELECTRONS
C-             P5_PHOTON = 5 VECTOR OF PHOTON
C-             NPHO = NUMBER OF PHOTONS
C-             P5_MUON = 5 VECTOR OF MUON
C-             NMUO = NUMBER OF MUONS
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      REAL    P6_ELECTRON(6,*),P5_PHOTON(5,*),P5_MUON(5,*)
      LOGICAL ELECTRON,PHOTON,MUON
      INTEGER SSUNIT,IE,IR,K
      INTEGER NELEC,NPHO,NMUO,I,IER
      INTEGER GZPNUT
      INTEGER LOCAL_RUN,LOCAL_EVENT
      REAL    SCALAR_ET,MET_D0,P2_NEUT(2)
      REAL    EXOTIC_ET_CUT
      INTEGER RUNNO,EVONUM
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('HMATRIX_RCP')
        CALL EZGET('EXOTIC_ET_CUT',EXOTIC_ET_CUT,IER)
        CALL EZEND
      ENDIF
C
      LOCAL_RUN = RUNNO()
      LOCAL_EVENT = EVONUM()
C
      IE=IQ(LHEAD+7)                    ! event number
      IR=IQ(LHEAD+8)                    ! run number
C
      IF(ELECTRON.AND.MUON.AND.PHOTON)THEN
        WRITE(SSUNIT(),2)LOCAL_RUN,LOCAL_EVENT,IR,IE
    2   FORMAT(//' **THREE EXOTIC PARTICLES IN SAME EVENT** ',/,
     &    ' LOCAL RUN NUMBER :' I10, ' LOCAL EVENT NUMBER : ',I10,/,
     &    ' RUN NUMBER :' I10, 'EVENT NUMBER : ',I10)
        GO TO 123
      ENDIF
C
      IF ((ELECTRON.AND.MUON).OR.(ELECTRON.AND.PHOTON)
     &  .OR.(PHOTON.AND.MUON ).OR.NELEC.GE.2.OR.NPHO.GE.2
     &  .OR.NMUO.GE.2) THEN
        WRITE(SSUNIT(),1)LOCAL_RUN,LOCAL_EVENT,IR,IE
    1   FORMAT(//' **TWO EXOTIC PARTICLES IN SAME EVENT** ',/,
     &    ' LOCAL RUN NUMBER :' I10, ' LOCAL EVENT NUMBER : ',I10,/,
     &    ' RUN NUMBER :' I10, 'EVENT NUMBER : ',I10)
        GO TO 123
      ENDIF
      RETURN
  123 CONTINUE
      IF ( ELECTRON ) THEN
        WRITE(SSUNIT(),3)((P6_ELECTRON(K,I),K=1,6),I=1,NELEC)
    3   FORMAT(' ELECTRON VECTOR ',(6F12.4/))
        DO I = 1 , NELEC
          IF(P6_ELECTRON(5,I).GT.EXOTIC_ET_CUT)THEN
            WRITE(SSUNIT(),31)P6_ELECTRON(5,I)
   31       FORMAT(' *****EXOTIC_ET_CUT_EXCEEDED ',F12.4)
          ENDIF
        ENDDO
      ENDIF
      IF ( PHOTON ) THEN
        WRITE(SSUNIT(),4)((P5_PHOTON(K,I),K=1,5),I=1,NPHO)
    4   FORMAT(' PHOTON VECTOR ',(5F12.4/))
        DO I = 1 , NPHO
          IF(P5_PHOTON(5,I).GT.EXOTIC_ET_CUT)THEN
            WRITE(SSUNIT(),31)P5_PHOTON(5,I)
          ENDIF
        ENDDO
      ENDIF
      IF ( MUON ) THEN
        WRITE(SSUNIT(),5)((P5_MUON(K,I),K=1,5),I=1,NMUO)
    5   FORMAT(' MUON VECTOR ',(5F12.4/))
        DO I = 1 , NMUO
          IF(P5_MUON(5,I).GT.EXOTIC_ET_CUT)THEN
            WRITE(SSUNIT(),31)P5_MUON(5,I)
          ENDIF
        ENDDO
      ENDIF
C
      LPNUT = GZPNUT(0)
C
      IF(LPNUT.EQ.0)THEN
        CALL ERRMSG('HMATRIX','HMATRIX_SET_TOP_QUAN',
     &    'NO PNUT VECTOR FOR THIS EVENT ','W')
        GO TO 999
      ENDIF
C
      CALL UCOPY(Q(LPNUT+3),P2_NEUT,2)
      SCALAR_ET = Q(LPNUT+14)         ! SCALAR ET FOR EVENT
      MET_D0 = SQRT(P2_NEUT(1)**2 + P2_NEUT(2)**2)
      WRITE(SSUNIT(),6)P2_NEUT,MET_D0,SCALAR_ET
    6 FORMAT(' MISSING ET 2 VECTOR ',2F12.4,/,
     &    ' MISSING ET MAGNITUDE ',F12.4,/,
     &    ' SCALAR ET ',F12.4/)
C
      IF(MET_D0.GT.EXOTIC_ET_CUT)THEN
        WRITE(SSUNIT(),31)MET_D0
      ENDIF
C
  999 RETURN
      END
