      SUBROUTINE CCCH_CELLS(FLOOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CCCH Cell phi and eta division info for
C-                         a Floor
C-
C-      Set the phi and eta boundary information for the CCCH floor
C-
C-   Inputs  : FLOOR    CCCH Floor number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  16-JAN-1989   Stuart Fuess
C-   Updated  29-JAN-1989   Stuart Fuess  Name changes, add # modules 
C-   Updated   4-FEB-1990   Stuart Fuess  New EZ routines; more general
C-                                        common blocks 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C  Arguments
      INTEGER FLOOR
C  Include files
      INCLUDE 'D0$INC:SCCCH_FLOOR.INC'
      INCLUDE 'D0$INC:WRITE_UNIT.INC'
C  Integers
      INTEGER IER
      INTEGER NUMBER_MODULES
      INTEGER NUMBER_PHI_DIVISIONS
      INTEGER NUMBER_PHI_BOUNDARIES
      INTEGER PHI_BOUNDARY
      INTEGER NUMBER_ETA_DIVISIONS
      INTEGER NUMBER_ETA_BOUNDARIES
      INTEGER ETA_BOUNDARY
      INTEGER LSTRING
      INTEGER PTR
C  Reals
      REAL ANGLE_MODULE
      REAL ANGLE_FLOOR
      REAL ANGLE_CELL
      REAL ANGLE(5)
      REAL ANGULAR_OFFSET
      REAL Z(24)
C  Characters
      CHARACTER*32 NAME
C  Equivalences
      INTEGER IVAL(50)
      REAL RVAL(50)
      EQUIVALENCE ( IVAL, RVAL )
C  Parameters
      REAL CM_PER_INCH
      PARAMETER ( CM_PER_INCH = 2.54 )
      REAL RAD
      PARAMETER (RAD=0.017453293)
C----------------------------------------------------------------------
C  Get the number of modules in this floor
C----------------------------------------------------------------------
      CALL EZGET_i('CCCH_NUMBER_MODULES',NUMBER_MODULES,IER)
C----------------------------------------------------------------------
C  Set the phi angular size of the entire module
C----------------------------------------------------------------------
      ANGLE_MODULE = 360. / NUMBER_MODULES
C----------------------------------------------------------------------
C  Get the number of phi divisions in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1001) FLOOR
      CALL EZGET_i(NAME,NUMBER_PHI_DIVISIONS,IER)
      NUMBER_PHI_BOUNDARIES = NUMBER_PHI_DIVISIONS + 1
C----------------------------------------------------------------------
C  Set the phi angular size of each cell
C----------------------------------------------------------------------
      ANGLE_CELL = ANGLE_MODULE / NUMBER_PHI_DIVISIONS
C----------------------------------------------------------------------
C  Use the Floor lateral boundary to set the phi limits on the first
C  and last cells of the module
C----------------------------------------------------------------------
      ANGLE_FLOOR = 2. * ATAN( 0.5  * (CCCH_FLOOR_INNER_WIDTH  +
     &                                  CCCH_FLOOR_OUTER_WIDTH ) /
     &                                 (CCCH_FLOOR_INNER_RADIUS +
     &                                  CCCH_FLOOR_OUTER_RADIUS ) )/RAD
C----------------------------------------------------------------------
C  Set the phi positions of the cell boundaries.  The edge cells are
C  determined from the Floor boundaries, while the central cells have
C  boundaries at exact phi divisions.
C----------------------------------------------------------------------
      ANGLE(1) = 0.5 * (ANGLE_MODULE - ANGLE_FLOOR)
      ANGLE(NUMBER_PHI_BOUNDARIES) = 0.5 * (ANGLE_MODULE + ANGLE_FLOOR)
      DO PHI_BOUNDARY=2,NUMBER_PHI_BOUNDARIES-1
        ANGLE(PHI_BOUNDARY) = (PHI_BOUNDARY-1) * ANGLE_CELL
      ENDDO
C----------------------------------------------------------------------
C  Get also the phi offset information for the CCCH modules
C----------------------------------------------------------------------
      CALL EZGET('CCCH_ANGULAR_OFFSET',ANGULAR_OFFSET,IER)
C----------------------------------------------------------------------
C  Get the number of eta divisions in this floor
C----------------------------------------------------------------------
      WRITE(NAME,1002) FLOOR
      CALL EZGET_i(NAME,NUMBER_ETA_DIVISIONS,IER)
      NUMBER_ETA_BOUNDARIES = NUMBER_ETA_DIVISIONS / 2 + 1
C----------------------------------------------------------------------
C  Use the Floor Z boundaries to set the Z limits on the end cells of
C  the module
C----------------------------------------------------------------------
      Z(NUMBER_ETA_BOUNDARIES)  = 0.5 * CM_PER_INCH * 
     &                            CCCH_FLOOR_INNER_LENGTH
C----------------------------------------------------------------------
C  Get the remaining eta boundary information for this floor.  Note 
C  that the boundaries are only given for 1/2 of a module, assuming 
C  that it is symmetric about Z=0.
C----------------------------------------------------------------------
      WRITE(NAME,1003) FLOOR
      CALL EZGET_iarr(NAME,IVAL,IER)
      PTR = 1
      DO ETA_BOUNDARY=1,NUMBER_ETA_BOUNDARIES-1
        PTR = PTR + 1
        Z(ETA_BOUNDARY) = CM_PER_INCH * RVAL(PTR)
      ENDDO
C----------------------------------------------------------------------
C  Write the cell division header info
C----------------------------------------------------------------------
      WRITE(OUT_VOL,2001) FLOOR
C----------------------------------------------------------------------
C  Write the cell phi division info
C----------------------------------------------------------------------
      WRITE(OUT_VOL,2002) NUMBER_MODULES
      WRITE(OUT_VOL,2003) ANGULAR_OFFSET
      WRITE(OUT_VOL,2004) NUMBER_PHI_BOUNDARIES
      WRITE(OUT_VOL,2005) (ANGLE(PHI_BOUNDARY),PHI_BOUNDARY=1,
     &                                         NUMBER_PHI_BOUNDARIES)
C----------------------------------------------------------------------
C  Write the cell eta division info
C----------------------------------------------------------------------
      WRITE(OUT_VOL,2006) NUMBER_ETA_BOUNDARIES
      WRITE(OUT_VOL,2007) (Z(ETA_BOUNDARY),ETA_BOUNDARY=1,
     &                                     NUMBER_ETA_BOUNDARIES)
      WRITE(OUT_VOL,2008)
      RETURN
 1001 FORMAT('CCCH_FLOOR',I1,'_PHI_DIVISIONS')
 1002 FORMAT('CCCH_FLOOR',I1,'_ETA_DIVISIONS')
 1003 FORMAT('CCCH_FLOOR',I1,'_ETA_BOUNDARIES')
 2001 FORMAT('\ARRAY CCCH_FLOOR',I1,'_CELLS')
 2002 FORMAT(I8,' ! Number of modules')
 2003 FORMAT(F8.3,' ! Module angular offset')
 2004 FORMAT(I8,' ! Number of phi boundaries',
     &     /,8x,' ! phi boundaries (degrees) follow')
 2005 FORMAT((8X,8(F8.3,:)))
 2006 FORMAT(I8,' ! Number of eta boundaries',
     &     /,8x,' ! eta boundaries (cm) follow')
 2007 FORMAT((8X,8(F8.3,:)))
 2008 FORMAT('\END')
      END
