      SUBROUTINE UDST_GET_TRAK(LZTRK,XDATA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Fill track parameters into array XDATA
C-
C-   Inputs  : LTRAK - link to ZTRK
C-   Outputs :
C-             XDATA( 1) - dE/dx of CDC/FDC
C-             XDATA( 2) - dE/dx of CDC/FDC from ZFIT
C-             XDATA( 3) - xy chi squared from ZFIT
C-             XDATA( 4) - Number of X-Y hits on the track
C-             XDATA( 5) - z chi squared from ZFIT
C-             XDATA( 6) - Number of Z coordinates on the track
C-             XDATA( 7) - phi from ZFIT
C-             XDATA( 8) - theta from ZFIT
C-             XDATA( 9) - x of point on ZFIT track
C-             XDATA(10) - y of point on ZFIT track
C-             XDATA(11) - z of point on ZFIT track
C-             XDATA(12) - error on ZFIT phi
C-             XDATA(13) - error on ZFIT theta
C-             XDATA(14) - error on ZFIT xy
C-             XDATA(15) - error on ZFIT z
C-             XDATA(16) - covariance term between z and theta
C-             XDATA(17) - impact parameter in xy
C-             XDATA(18) - distance to vertex along z axis
C-             XDATA(19) - xy chi squared from DTRK/FDCT
C-             XDATA(20) - z chi squared from DTRK/FDCT
C-             XDATA(21) - Total number of degrees of freedom
C-             XDATA(22) - phi from DTRK/FDCT
C-             XDATA(23) - theta from DTRK/FDCT
C-             XDATA(24) - x of point on DTRK/FTRK track
C-             XDATA(25) - y of point on DTRK/FTRK track
C-             XDATA(26) - z of point on DTRK/FTRK track
C-             XDATA(27) - error on DTRK/FTRK phi
C-             XDATA(28) - error on DTRK/FTRK theta
C-             XDATA(29) - link to VERT bank
C-             XDATA(30) - link to VTXT bank
C-             XDATA(31) - Vertex with which the track is associated 
C-                         
C-   Created   2-DEC-1993   Ian Adam - Code based on original UDST_GET_EMCLUS
C-   Updated  19-DEC-1993   Ulrich Heintz   save chi2 and ndof separately
C-   Updated  21-JAN-1994   Ian Adam - Move VZERO call to handle VTX only
C-                          tracks correctly
C-   Updated  15-OCT-1994   Ulrich Heintz  remove VTXT info 
C-
C---------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE   'D0$INC:ZEBCOM.INC'
      INTEGER   LZTRK,LZFIT,LDTRK,LFDCT,ITRK,UDST_VERT_LINK,I
      INTEGER   KTRAK,KTRAK1,ID_TRAK,NVTXT,UDST_VTXT_LINK
C
      PARAMETER (KTRAK=31)
      CHARACTER*8 TRAK_TAGS(KTRAK)
      DATA TRAK_TAGS/
     &   'DEDX'  ,'DEDXZ' ,'CHI2XT','NHITXT','CHI2ZT','NHITZT','PHIT'
     &  ,'THT'   ,'XT'    ,'YT'    ,'ZT'    ,'DPHIT' ,'DTHT'  ,'DXYT'
     &  ,'DZT'   ,'COVT'  ,'XYIMPT','ZDIST' ,'CHI2XD','CHI2ZD','DOFD'
     &  ,'PHID'  ,'THD'   ,'XD'    ,'YD'    ,'ZD'    ,'DPHID' ,'DTHD'
     &  ,'LVERTT','LVTXTT','LVFITT'/
      REAL  DEDX  ,DEDXZ ,CHI2XT,NHITXT,CHI2ZT,NHITZT,PHIT
     &     ,THT   ,XT    ,YT    ,ZT    ,DPHIT ,DTHT  ,DXYT
     &     ,DZT   ,COVT  ,XYIMPT,ZDIST ,CHI2XD,CHI2ZD,DOFD
     &     ,PHID  ,THD   ,XD    ,YD    ,ZD    ,DPHID ,DTHD
     &     ,LVERTT,LVTXTT,LVFITT
      COMMON/TRAK_OBJ/
     &      DEDX  ,DEDXZ ,CHI2XT,NHITXT,CHI2ZT,NHITZT,PHIT
     &     ,THT   ,XT    ,YT    ,ZT    ,DPHIT ,DTHT  ,DXYT
     &     ,DZT   ,COVT  ,XYIMPT,ZDIST ,CHI2XD,CHI2ZD,DOFD
     &     ,PHID  ,THD   ,XD    ,YD    ,ZD    ,DPHID ,DTHD
     &     ,LVERTT,LVTXTT,LVFITT
      REAL XX(KTRAK),XDATA(KTRAK)
      EQUIVALENCE (XX,DEDX)
C----------------------------------------------------------------------
      CALL VZERO(XX,KTRAK)
      IF(LZTRK.LE.0)THEN
        CALL ERRMSG('NO ZTRK BANK','UDST_GET_TRAK',' ','W')
      ELSE
        LZFIT=LQ(LZTRK-1)               ! link to ZFIT
        IF (LZFIT.LE.0)THEN
          CALL ERRMSG('NO ZFIT BANK','UDST_GET_TRAK',' ','W')
        ELSE
          NHITXT = FLOAT(IQ(LZFIT+6))  ! xy # hits
          NHITZT = FLOAT(IQ(LZFIT+7))  ! z # hits
          CHI2XT = Q(LZFIT+8)           ! xy chi**2
          CHI2ZT = Q(LZFIT+9)           ! z chi**2
          PHIT   = Q(LZFIT+10)          ! phi of track
          XT     = Q(LZFIT+11)          ! x
          YT     = Q(LZFIT+12)          ! y
          THT    = Q(LZFIT+13)          ! theta of track
          ZT     = Q(LZFIT+15)          ! z
          DPHIT  = Q(LZFIT+16)          ! error on phi of track
          DXYT   = Q(LZFIT+17)          ! error on xy
          DTHT   = Q(LZFIT+18)          ! error on theta of track
          DZT    = Q(LZFIT+19)          ! error on z
          DEDXZ  = Q(LZFIT+26)          ! ionisation of CDC/FDC track
          XYIMPT = Q(LZFIT+32)          ! impact parameter in X-Y plane
          ZDIST  = Q(LZFIT+33)          ! distance to VERTEX_Z along Z axis
          COVT   = Q(LZFIT+34)          ! covariance term between Z0 and theta
        ENDIF
        LFDCT=LQ(LZTRK-8)
        LDTRK=LQ(LZTRK-7)
        IF(LDTRK.NE.0)THEN
          PHID   = Q(LDTRK+6)           ! phi of track
          XD     = Q(LDTRK+7)
          YD     = Q(LDTRK+8)
          THD    = Q(LDTRK+9)           ! theta of track
          ZD     = Q(LDTRK+11)
          CHI2XD = Q(LDTRK+12)          ! xy chi**2
          CHI2ZD = Q(LDTRK+13)          ! z chi**2
          DOFD   = FLOAT(IQ(LDTRK+14))  ! Total number of degrees of freedom
          DPHID  = Q(LDTRK+16)          ! error on phi of track
          DTHD   = Q(LDTRK+18)          ! error on theta of track
          DEDX   = Q(LDTRK+20)          ! CDC dE/dx
          LVFITT = FLOAT(IQ(LDTRK+15))  ! VFIT bank number 
        ELSEIF(LFDCT.NE.0)THEN
          XD     = Q(LFDCT+4)
          YD     = Q(LFDCT+5)
          PHID   = Q(LFDCT+6)           ! phi of track
          ITRK   = IQ(LFDCT-5)
          CALL FGETZ0(ITRK,ZD)
          CHI2XD = Q(LFDCT+19)          ! chi**2
          CHI2ZD = 0.
          DEDX   = Q(LFDCT+20)          ! FDC dE/dx
          THD    = Q(LFDCT+22)          ! theta of track
          DPHID  = Q(LFDCT+23)          ! error on phi of track
          DTHD   = Q(LFDCT+24)          ! error on theta of track
          DOFD   = FLOAT(IQ(LFDCT+25))  ! number of points used in track fit
          LVFITT = FLOAT(IQ(LFDCT+31))  ! VFIT bank number 
        ENDIF
        CALL UDST_GET_VERT_LINK(LZTRK,UDST_VERT_LINK)
        LVERTT   = FLOAT(UDST_VERT_LINK)
        CALL UDST_GET_VTXT_LINK(LZTRK,UDST_VTXT_LINK)
        LVTXTT   = FLOAT(UDST_VTXT_LINK)
      ENDIF
      DO I=1,KTRAK
        XDATA(I) = XX(I)
      ENDDO
  999 RETURN
C----------------------------------------------------------------------
C
      ENTRY UDST_TRAK_TAGS(KTRAK1,ID_TRAK)
C----------------------------------------------------------------------
      KTRAK1=KTRAK
      ID_TRAK=15
      CALL UDST_BOOK_GROUP(ID_TRAK,'TRAK',TRAK_TAGS,KTRAK)
C----------------------------------------------------------------------
      RETURN
      END
