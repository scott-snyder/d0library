      SUBROUTINE INTVTX(XYZ)
C-------------------------------------------------------
C-
C-  Subroutine to generate an interaction vertex.
C-
C-  Default is to generate an interaction point with normal distribution
C-  along z given by SIGZ= 25 cm and with x=0,y=0.
C-
C-  If SETVTX is called the interaction point returned in the next
C-  call of INTVTX will be the values given to SETVTX.
C-  SETVTX must be called for every event before event generation
C-  to overwrite the default.
C-
C-  SETVTX_PAR is called to select the vertex means and widths and
C-  override the defaults.
C-
C-  SETBEAM_PAR is called to select the beam slopes in x and y and
C-  override the defaults.
C-
C-  OUTPUT:
C-  XYZ = x,y,z position of vertex 
C-
C-  ENTRY SETVTX(XYZ): 
C-  INPUT:
C-  XYZ=  x,y,z position of vertex
C-
C-  ENTRY SETVTX_PAR(VTX_OFFSET,VTX_SIGMA)
C-  INPUT:
C-  REAL VTX_OFFSET(3) =  x,y,z mean position of vertex
C-  REAL VTX_SIGMA(3)  =  gaussian width of x,y,z position of vertex
C-
C-  ENTRY SETBEAM_PAR(DXDZ,DYDZ)
C-  INPUT:
C-  REAL DXDZ = slope of beam in x  
C-  REAL DYDZ = slope of beam in y
C-
C-    SDP Nov.,1985
C-   Updated   9-MAY-1991   Serban D. Protopopescu   
C-            make sure vertex is random for different processes
C-   Updated  19-JUL-1994   Norman A. Graf  Added entry point SETVTX_PAR 
C-                                          and allowed for offset X, Y
C-                                          as well as beam spot width.
C-   Updated  11-MAR-1997   Norman A. Graf  Added entry point SETBEAM_PAR
C-                                          and allowed for beam slope.
C-                                          Also fixed logic problem in
C-                                          X/YOFF and X/YSIG.
C-
C-------------------------------------------------------
C
      IMPLICIT NONE
      REAL XYZ(3)
      INTEGER NA,I
      REAL A,XSIG,YSIG,ZSIG,XOFF,YOFF,ZOFF,XSET(3),RANF
      REAL VTX_OFFSET(3),VTX_SIGMA(3)
      REAL DXDZ,DYDZ,XSLOPE,YSLOPE
      LOGICAL SETXYZ,FIRST
      SAVE SETXYZ,FIRST,ZSIG
      DATA SETXYZ,FIRST/.FALSE.,.TRUE./
      DATA XSIG /0.0/   !sigma of interaction region in x
      DATA YSIG /0.0/   !sigma of interaction region in y
      DATA ZSIG/25.0/   ! sigma of interaction region in z
      DATA XOFF/0.0/    ! mean of interaction region in X
      DATA YOFF/0.0/    ! mean of interaction region in Y
      DATA ZOFF/0.0/    ! mean of interaction region in Z
      DATA XSLOPE /0.0/ !slope in beam dX/dZ
      DATA YSLOPE /0.0/ !slope in beam dY/dZ
C
      IF(SETXYZ) THEN
       CALL UCOPY(XSET,XYZ,3)
      ELSE
c
        IF(FIRST) THEN      ! attempt to make vertex random for each process
          NA=RANF()*1000000.
          NA=MOD(NA,100)
          DO I=1,NA
            CALL NORRAN(A)
          ENDDO
          FIRST=.FALSE.
        ENDIF
C
        XYZ(1)=0.
        XYZ(2)=0.
        CALL NORRAN(A)
        XYZ(3)=ZSIG*A + ZOFF
C
        IF(XSIG.GT.0) THEN
          CALL NORRAN(A)
          XYZ(1)=XSIG*A + XOFF
        ELSE
          XYZ(1)= XOFF
        ENDIF
C
        IF(YSIG.GT.0) THEN
          CALL NORRAN(A)
          XYZ(2)=YSIG*A + YOFF
        ELSE
          XYZ(2)= YOFF
        ENDIF
C
        IF(XSLOPE.NE.0) THEN
          XYZ(1) = XYZ(1) + XSLOPE*XYZ(3)
        ENDIF
C
        IF(YSLOPE.NE.0) THEN
          XYZ(2) = XYZ(2) + YSLOPE*XYZ(3)
        ENDIF
C
      ENDIF
      SETXYZ=.FALSE.
C
      RETURN
      ENTRY SETVTX(XYZ)
C
C      A call to SETVTX forces vertex to have values XYZ
C
      SETXYZ=.TRUE.
      CALL UCOPY(XYZ,XSET,3)
      RETURN
C
      ENTRY SETVTX_PAR(VTX_OFFSET,VTX_SIGMA)
      XOFF = VTX_OFFSET(1)
      YOFF = VTX_OFFSET(2)
      ZOFF = VTX_OFFSET(3)
      XSIG = VTX_SIGMA(1)
      YSIG = VTX_SIGMA(2)
      ZSIG = VTX_SIGMA(3)
      RETURN
C
      ENTRY SETBEAM_PAR(DXDZ,DYDZ)
      XSLOPE = DXDZ
      YSLOPE = DYDZ
      RETURN
C
      END

