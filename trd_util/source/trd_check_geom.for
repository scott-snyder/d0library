      SUBROUTINE TRD_CHECK_GEOM (LTRDT, GEOMETRY,BAD_SECTOR)
C----------------------------------------------------------------------
C
C   Purpose and Methods : Checks if track associated with trdt crosses the 3
C   layers
C
C   Inputs  :
C      LTRDT          integer         LINK to TRDT
C   Outputs :
C      GEOMETRY       logical(3)   layer 1,2,3.
C   Controls: none
C
C-   Created  12-APR-1995   A. ZYLBERSTEJN
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER IER,LTRDT,LAYER,LZTRK,LZFIT,I,LCACL,GZUDST
      REAL THETA,PHI,ETA,VERSION,VIN(6),VOUT(6),ST,R_LAYER1
      LOGICAL GEOMETRY(3),BAD_SECTOR
      DATA R_LAYER1/26.3/
C-------------------------------------------------------------------------------
C Geometry
C-------------------------------------------------------------------------------
      GEOMETRY(1)=.FALSE.
      GEOMETRY(2)=.FALSE.
      GEOMETRY(3)=.FALSE.
      LZTRK=LQ(LTRDT-4)
      IF (LZTRK.GT.0) THEN
        LZFIT=LQ(LZTRK-1)
        IF (LZFIT.GT.0) THEN
          VIN(1)=Q(LZFIT+11)
          VIN(2)=Q(LZFIT+12)
          VIN(3)=Q(LZFIT+15)
          PHI=Q(LZFIT+10)
          THETA=Q(LZFIT+13)
          ST=SIN(THETA)
          IF (GZUDST().GT.0) THEN
            VIN(4)=ST*COS(PHI)!CX
            VIN(5)=ST*SIN(PHI)!cy
            VIN(6)=COS(THETA) !cz
          ELSE
            VIN(4)=Q(LZFIT+20)
            VIN(5)=Q(LZFIT+22)
            VIN(6)=Q(LZFIT+24)
          ENDIF
C          ETA=-LOG(TAN(THETA/2.))
        ELSE! ZTRK with no ZFIT; should not happen
          CALL ERRMSG
     &      ('bank ZFIT not found ','TRD_CHECK_GEOM',' ','W')
          GO TO 999
        ENDIF
      ELSE ! Check if track is a PPHO
        IF(LQ(LTRDT-5).NE.0)THEN
          LCACL=LQ(LTRDT-5)
          VIN(1)=Q(LCACL+14)
          VIN(2)=Q(LCACL+15)
          VIN(3)=Q(LCACL+16)
          THETA=Q(LCACL+11)
C          ETA=-LOG(TAN(THETA/2.))
          PHI=Q(LCACL+12)
          ST=SIN(THETA)
          VIN(4)=ST*COS(PHI)!CX
          VIN(5)=ST*SIN(PHI)!cy
          VIN(6)=COS(THETA) !cz
        ELSE ! No ZTRK,No PPHO: error
          CALL ERRMSG
     &      ('banks ZTRK,CACL not found ','TRD_CHECK_GEOM',' ','W')
          GO TO 999
        END IF
      END IF
      CALL TRD_INTERSECTION (VIN,THETA,PHI,GEOMETRY)
C  check bad   sectors for non MC data
      IF(GEOMETRY(1) .AND. IQ(LHEAD+1) .LT.1000)THEN
        CALL EXTCYL(VIN,VOUT,R_LAYER1,IER)
C        print*,' in  TRD_CHECK_GEOM,vin',vin,'vout',vout,
C     &    ' ier',ier, vout(2)/vout(1)
        IF(IER.NE.0)THEN
          CALL ERRMSG(' contradiction in geometry',
     &      'TRD_CHECK_GEOM',' ','W')
          GO TO 999
        END IF
        PHI=ATAN2(-VOUT(2),-VOUT(1))+3.1415926
C      PRINT*,' phi',PHI
        CALL TRD_BADSECTOR(PHI,BAD_SECTOR)
C        print*,' bad_sector',bad_sector
      END IF
CC-------------------------------------------------------------------------------
  999 RETURN
      END
