$!========================================================================
$!
$! Name      : EVENT_DISPLAY
$!
$! Purpose   : define logicals needed by D0USER event display
$!
$! Arguments : P1=MC define the logicals needed for Montecarlo
$!               = default is data or COSMIC
$!             P2 = DI3000 driver 1 or 
$!                  XEMU if you want to use the XWindow emulator
$!             P3 = DI3000 driver 2 
$!
$! Created  18-APR-1991   Serban D. Protopopescu
$! Modified 20-OCT-1993   Lupe Howell Modified so the Xwindow emulator can be
$!                        use and the setup is done properly
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ input_data = P1
$ IF input_data .eqs. "" 
$ THEN 
$   inquire input_data " data, COSMIC or MC? [data]"
$ ENDIF
$   define CDC_STPFILE D0$STP:CDC_D0STPFILE.DAT
$   define FDC_STPFILE D0$STP:FDC_D0STPFILE.DAT
$   define VTX_STPFILE D0$STP:VTX_D0STPFILE.DAT
$   define CAL_STPFILE D0$STP:CAL_STPFILE.DAT
$   define MUO_STPFILE D0$STP:MUO_STPFILE.DAT
$   define TRD_STPFILE D0$STP:TRD_STPFILE.DAT
$   define SAM_STPFILE D0$STP:SAM_STPFILE.DAT
$ IF input_data .eqs. "MC"
$ THEN
$   define CDC_STPFILE D0$STP:CDC_STPFILE.DAT
$   define FDC_STPFILE D0$STP:FDC_STPFILE.DAT
$   define VTX_STPFILE D0$STP:VTX_STPFILE.DAT
$ ENDIF
$ DR1 = P2
$ DR2 = P3
$ IF DR1 .eqs. "" 
$ THEN 
$   write sys$output "DI3000 Drivers options: GPV, XDW, X11, PST (hardcopy)"
$   write sys$output "Emulator Drivers options: XEMU (Xwindow emulator)"
$   inquire DR1 "Give driver 1 (required) [XDW]"
$   if DR1 .eqs. "" then DR1 = "XDW"
$   inquire DR2 "Give driver 2 (not required) []"
$ ENDIF
$!-------------------------------------------------------
$! Define a dummy driver when Xwindow emulator requested
$!-------------------------------------------------------
$ IF DR1 .eqs. "XEMU"
$ THEN
$     DEFINE/NOLOG YD1DRV DRV_LINK:DRVXDW.EXE
$     DEFINE/NOLOG YD2DRV DRV_LINK:DRVPST.EXE
$ ELSE
$!-------------------------------------------------------
$!  Define drivers using DI3000
$!-------------------------------------------------------
$     SETDRV 'DR1' 'DR2'
$ ENDIF
$EXIT:
$   EXIT
