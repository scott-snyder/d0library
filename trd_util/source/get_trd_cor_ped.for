      SUBROUTINE GET_TRD_COR_PED (PLANE,WIRE,YFADC,PED,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : PLANE      integer   1,2,3 (anodes) or 4,5,6 (cathodes)
C-             WIRE       integer   in [1,512]
C-             YDATA      FADC FADC raw data
C-   Outputs : PED real      pedestal
C-             ERROR      integer   0 = OK
C-                                  1 = ped not required in TRD.RCP
C-                                  2 = pedestal not found
C-   Controls: TRD.RCP
C-
C-   Created  15-JAN-1993   Alain PLUQUET
C-   Updated   6-JAN-1994   A. Zylberstejn  remove definition of ped_avt,
C-                        ped_apr
C-   Updated  21-MAR-1994   A. Zylberstejn  Skip call to TRGPED if CDD4 is
C-                                          absent
C-   Updated  24-MAR-1995   A. Zylberstejn  : clean-up
C-   Updated  27-APR-1995   A. Zylberstejn  :change number and meaning of input
C-   arguments
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C      INCLUDE 'D0$INC:VARTRD.INC'
      INCLUDE 'D0$INC:zebcom.INC'
C      INCLUDE 'D0$INC:ZEBWRK.INC'
      INTEGER PLANE,WIRE,HIT,ERROR,I,TCHNB
      INTEGER IIK,IER,IMIN,IMX0,TYP_PED
      REAL PED,PEDAV,VMAX,VSUM,PED_DBL,YFADC(128)
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      LOGICAL FIRST,DO_PED
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
        FIRST=.FALSE.
        CALL EZPICK('TRD_RCP')
C        CALL EZGET('COR_PED',DO_PED,IER)
        CALL EZGET('PED_TYPE',TYP_PED,IER)
C        CALL EZGET('NBINS_PHYSICS',NBIN_FADC,IER)
        CALL EZRSET
        DO_PED=.TRUE.
        IF(LQ(LHEAD-IZCDD4).EQ.0)DO_PED=.FALSE.! Return if CDD4 absent
        IF(TYP_PED.NE.-1 .AND.TYP_PED.NE.0 .AND.TYP_PED.NE.1)
     &    TYP_PED=0
C        print*,' lcdd4',lq(lhead-izcdd4),' do_ped',do_ped,' typ_ped',
C     &    typ_ped
        IMIN=10
      ENDIF
      CALL TRGPED(' ',WIRE,PLANE,PED_DBL,IER)
      IF(TYP_PED.EQ.0)THEN
        PED=PED_DBL
        ERROR=0
        RETURN
      END IF
      PED=0.
C      print*,' do_ped',do_ped
      IF (.NOT.DO_PED) GO TO 999
C      print*,' wire,plane',wire,plane,' ped_dbl',ped_dbl,' ier',ier
      IF (TYP_PED.EQ.-1 ) THEN
        PEDAV=0.
        DO IIK=1,10
          IF(ABS(YFADC(IIK)-PED_DBL).GT.4.)THEN
            ERROR=3
            GO TO 999
          ENDIF
          PEDAV=PEDAV+YFADC(IIK)
        END DO
        ERROR=0
        PED=PEDAV/10.
      END IF
C      IF(TYP_PED.EQ.1)THEN
C        IMX0=128-IMIN+1
C        IF(VMAX(yfadc(IMX0),IMIN)-PED_DBL.LT.4.)
C     &          PED=VSUM(yfadc(IMX0),IMIN)/FLOAT(IMIN)
C      END IF
  999 CONTINUE
c      PRINT*,' in get_trd_cor_ped, cped',PED,' error',ERROR,
c     &  ' ped_dbl',PED_DBL
      IF(PED.LE.0. .AND.PED_DBL.GT.4.)PED=PED_DBL
      RETURN
      END
