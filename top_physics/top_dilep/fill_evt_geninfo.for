      SUBROUTINE FILL_EVT_GENINFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-APR-1995   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:TOP_DILEP_ANALYSIS.INC'
      INTEGER RUNNO,EVONUM,IER,NTAGS
      INTEGER NV,RVERSION,PASS,I,MAXVER
      INTEGER LISAE,GZISAE,VAXTIME(2)
      INTEGER LISV1,ID,LSUP
      PARAMETER( MAXVER = 3 )
      INTEGER WGTVER(MAXVER),METVER(MAXVER),NVERTRK(MAXVER)
      REAL    AGE(2), RAW_LUM(2), DLUM, DTIME, XLUM_INST
      REAL    XDATA(1000),ZVTX_INFO(3, MAXVER),DZVERT
      REAL    PMC(4),XMC,YMC,ZMC
      LOGICAL OKZ
C----------------------------------------------------------------------
      ixrun(1) = runno()
      ixrun(2) = evonum()
      ixrun(3) = iq(lhead+11)
      ixrun(4) = iq(lhead+15)
      ixrun(5) = iq(lhead+16)
      ixrun(6) = iq(lhead+17)
      ixrun(7) = iq(lhead+18)
      ixrun(8)= iq(lhead+7)
      ixrun(9)= iq(lhead+30)
      mcdata = iq(lhead+1).gt.1000
      ixrun(10) = mcdata
      CALL reco_version(rversion,pass)
      ixrun(11) = rversion
      ixrun(12) = pass
      CALL vertex_info(maxver,nv,zvtx_info,okz)
C            only consider the main primary vertex
      zvert = 0.0
      IF ( okz ) THEN
        DO i = 1, min(maxver, nv)
          zvert = zvtx_info(1,i)
          dzvert = zvtx_info(2,i)
          xrun(13+i) = zvert
          xrun(16+i) = dzvert
        end do
        ixrun(13) = nv
C
C ****  store the location of primary vertex for use in other routines
C
        zvert = zvtx_info(1,1)
      ENDIF
      CALL fetch_mbr_z(maxver,nv,zvtx_info,okz)
      IF ( okz ) THEN
        xrun(23)=zvtx_info(1,1)
        xrun(24)=zvtx_info(2,1)
        xrun(25)=zvtx_info(3,1)
        xrun(26)=zvtx_info(1,2)
        xrun(27)=zvtx_info(2,2)
      ENDIF
C
C **** Store additional vertex information
C
      call fill_cd_vertex_info(NV,WGTVER,METVER,NVERTRK)
      do i = 1, min(maxver, nv)
        xrun(27+i)=wgtver(i)
        xrun(30+i)=metver(i)
        xrun(33+i)=nvertrk(i)
      enddo
C
C ****  store the location of primary vertex in ISV1 if Monte Carlo
C
      LISV1=0
      CALL GTISV1(LISV1,LISV1,ID,PMC,XMC,YMC,ZMC)
      xrun(22)=0.0
      IF (LISV1.GT.0) xrun(22) = zmc
C
      lisae = gzisae()
      if (lisae.gt.0) xrun(20) = q(lisae+11)
C
C ****  get instaneous luminosity for event
C
      xlum_inst=0.
      if (.not.mcdata) then
      vaxtime(1) = iq(lhead + 4)
      vaxtime(2) = iq(lhead + 5)
      call getlum(vaxtime,raw_lum,age,ier)
      if (ier.eq.0) then
        dtime = age(2) + age(1)
        dlum = raw_lum(1) - raw_lum(2)
        if (dtime.gt.0.0) then
          xlum_inst = raw_lum(2) + age(2) * (dlum/dtime)
        else
          xlum_inst = raw_lum(1)
        endif
      else
        xlum_inst = -1.0
      endif
      endif
      xrun(21) = xlum_inst
  999 RETURN
C...........................................................................
      ENTRY EVT_GENINFO(NVAR,XDATA)
      nvar = 36
      CALL ucopy(xRUN,xdata,nvar)
      RETURN
C...........................................................................
      ENTRY EVT_GENINFO_TAGS(NTAGS,TRUN)
      ntags=36
      TRUN(1)='RUN:I'
      TRUN(2)='EVENT:I'
      TRUN(3)='L1BIT:I'
      TRUN(4)='L2BIT0:I'
      TRUN(5)='L2BIT1:I'
      TRUN(6)='L2BIT2:I'
      TRUN(7)='L2BIT3:I'
      TRUN(8)='XING:I'
      TRUN(9)='UBLANK:I'
      TRUN(10)='MC:I'
      TRUN(11)='RECOVERS:I'
      TRUN(12)='RECOPASS:I'
      TRUN(13)='NVERTEX:I'
      TRUN(14)='ZVERTEX1'
      TRUN(15)='ZVERTEX2'
      TRUN(16)='ZVERTEX3'
      TRUN(17)='DZVERTEX1'
      TRUN(18)='DZVERTEX2'
      TRUN(19)='DZVERTEX3'
      TRUN(20)='MCXSEC'
      TRUN(21)='INSTLUM'
      TRUN(22)='MCZVERT'
      TRUN(23)='MBZVER1'
      TRUN(24)='MBZVER2'
      TRUN(25)='MBZVER3'
      TRUN(26)='MBZVER4'
      TRUN(27)='MBZVER5'
      TRUN(28)='WGTVER1'
      TRUN(29)='WGTVER2'
      TRUN(30)='WGTVER3'
      TRUN(31)='METVER1'
      TRUN(32)='METVER2'
      TRUN(33)='METVER3'
      TRUN(34)='TRKVER1'
      TRUN(35)='TRKVER2'
      TRUN(36)='TRKVER3'
      RETURN
      END
