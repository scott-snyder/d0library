      SUBROUTINE EZRDF (LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read a TEXT file containing parameters and
C-                         save records and decoded values in the SRCP
C-                         bank which hangs below STPH. The Character data
C-                         is ordered to allow the use of a binary search
C-                         to retrieve data from the bank. The SRCP bank
C-                         hanging below STPH (via SCPH) is given the name
C-                         SCPH by default. It can be renamed with EZRNAM.
C-                         The SRCP bank will be automatically expanded to
C-                         accomodate new entries. 
C-
C-   Inputs  : LUN         Logical Unit number of input data stream
C-   Outputs : None
C-
C-                         Error codes. Use EZERR to check for code.
C-                         0 --- OK
C-                         1 --- Bank has been expanded at least once
C-                         2 --- Maximum bank size reached.
C-                        -4 --- FATAL ERROR. IZSCPH link already occupied
C-
C-   Created  26-NOV-1987   Rajendran Raja
C-
C-   Modified  6-MAY-1988   Harrison B. Prosper
C-                       1) Replaced NUMERIC by more general utility
C-                          VALUE. Valid numerical types:
C-                          INTEGER, REAL and REAL in E-format.
C-   Modified 16-MAY-1988   Harrison B. Prosper
C-                       2) The datum names can now contain numbers, e.g.
C-                          MINSIG1.1
C-   Modified 17-JUN-1988   Harrison B. Prosper
C-                       3) Can now read in boolean values TRUE and FALSE.
C-   Modified 18-JUN-1988   Harrison B. Prosper
C-                       4) Can now read in arrays.
C-   Modified 19-JUN-1988   Harrison B. Prosper
C-                       5) Replaced machine-dependent STR$UPCASE by
C-                          call to machine-independent routine UPCASE.
C-   Modified 27-JUN-1988   Harrison B. Prosper
C-                       6) Converting to standard FORTRAN-77. Use UCTOH
C-                          to pack Characters into integer variables.
C-   Modified  3-OCT-1988   Harrison B. Prosper
C-                          Integration with new SRCP routines CRSRCP
C-                          EZFILL etc.
C-   Modified 12-OCT-1988   Harrison B. Prosper
C-                          Call EZZEXT to expand SRCP bank dynamically
C-   Modified 16-OCT-1988   Harrison B. Prosper
C-                          Check if link IZSCPH is free. SCRAP if not
C-                          free.
C-
C-   Modified  9-NOV-1988   Harrison B. Prosper
C-                          Now calls EZREAD which contains the original
C-                          EZRDF  code.
C-   Updated   3-Jan-1996   sss - Compile with g77.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LUN,LSCPH
      INCLUDE 'D0$LINKS:IZSCPH.LINK'
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      ENTRY RDSRCP (LUN)
C----------------------------------------------------------------------
C
C ****  Create Static Run Control Parameters bank hanging off SCPH
C
      LSCPH = LC(LSTPH-IZSCPH)
      IF ( LSCPH .LE. 0 ) CALL BKSCPH (LSCPH)
      CALL EZREAD (LUN,'SCPH',WRDCRD,LSCPH,IZSRCP)
C
  999 RETURN
      END
