****************************************************************************
*                                                                          *
*  File:  Hydra.Inc                                    Date:  17-FEB-1994  *
*                                                                          *
*  Description of Module:                                                  *
*  =====================                                                   *
*                                                                          *
*  This file contains offsets to all of the Peripheral Bus Registers.      *
*  These are stored using the assembler .set directive and are 16          *
*  bit values.  These values can be used for register offset addressing,   *
*  if the Base Address of the Peripheral Bus is stored in a register.      *
*  The full 32-bit Base Address of the Peripheral Bus is also defined      *
*  as a constant in this file.                                             *
*                                                                          *
*  This file contains only constants defined with the assembler .set       *
*  directive                                                               *
*                                                                          *
****************************************************************************

****************************************************************************
*   Base address of the Peripheral Bus (32 bit constant)                   *
****************************************************************************

Base_of_Peripheral_Bus      .set    00100000h   ; Base Address of Peripheral
                                                ; Bus

****************************************************************************
*   Offsets to elements in the Peripheral Bus (16 bit constants)           *
****************************************************************************

Disp_to_Global_MICR         .set    0000h   ; Global Mem. Intf. Ctrl. Reg.
Disp_to_Local_MICR          .set    0004h   ; Local Mem. Intf. Ctrl. Reg.

Disp_to_Analysis_Data       .set    0010h   ; Analysis Module Data Register
Disp_to_Analysis_Control    .set    0011h   ; Analysis Module Control Register
Disp_to_Analysis_Status     .set    0012h   ; Analysis Module Status Register
Disp_to_Analysis_Task_ID    .set    0013h   ; Analysis Module Task ID Register

Disp_to_Timer_0_Control     .set    0020h   ; Timer #0 Control Register
Disp_to_Timer_0_Counter     .set    0024h   ; Timer #0 Counter Register
Disp_to_Timer_0_Period      .set    0028h   ; Timer #0 Period Register
Disp_to_Timer_1_Control     .set    0030h   ; Timer #1 Control Register
Disp_to_Timer_1_Counter     .set    0034h   ; Timer #1 Counter Register
Disp_to_Timer_1_Period      .set    0038h   ; Timer #1 Period Register

Disp_to_ComPort_0_Control   .set    0040h   ; Com Port #0 Control Register
Disp_to_ComPort_0_Input     .set    0041h   ; Com Port #0 Input Register
Disp_to_ComPort_0_Output    .set    0042h   ; Com Port #0 Output Register
Disp_to_ComPort_1_Control   .set    0050h   ; Com Port #1 Control Register
Disp_to_ComPort_1_Input     .set    0051h   ; Com Port #1 Input Register
Disp_to_ComPort_1_Output    .set    0052h   ; Com Port #1 Output Register
Disp_to_ComPort_2_Control   .set    0060h   ; Com Port #2 Control Register
Disp_to_ComPort_2_Input     .set    0061h   ; Com Port #2 Input Register
Disp_to_ComPort_2_Output    .set    0062h   ; Com Port #2 Output Register
Disp_to_ComPort_3_Control   .set    0070h   ; Com Port #3 Control Register
Disp_to_ComPort_3_Input     .set    0071h   ; Com Port #3 Input Register
Disp_to_ComPort_3_Output    .set    0072h   ; Com Port #3 Output Register
Disp_to_ComPort_4_Control   .set    0080h   ; Com Port #4 Control Register
Disp_to_ComPort_4_Input     .set    0081h   ; Com Port #4 Input Register
Disp_to_ComPort_4_Output    .set    0082h   ; Com Port #4 Output Register
Disp_to_ComPort_5_Control   .set    0090h   ; Com Port #5 Control Register
Disp_to_ComPort_5_Input     .set    0091h   ; Com Port #5 Input Register
Disp_to_ComPort_5_Output    .set    0092h   ; Com Port #5 Output Register

Disp_to_DMA_0_Control           .set    00a0h   ; DMA Ch #0 Control Register
Disp_to_DMA_0_Source            .set    00a1h   ; DMA Ch #0 Source Address
Disp_to_DMA_0_Source_Index      .set    00a2h   ; DMA Ch #0 Source Address Index
Disp_to_DMA_0_Counter           .set    00a3h   ; DMA Ch #0 Transfer Counter
Disp_to_DMA_0_Destination       .set    00a4h   ; DMA Ch #0 Destination Address
Disp_to_DMA_0_Dest_Index        .set    00a5h   ; DMA Ch #0 Dest. Address Index
Disp_to_DMA_0_Link_Pointer      .set    00a6h   ; DMA Ch #0 Link Pointer
Disp_to_DMA_0_Aux_Counter       .set    00a7h   ; DMA Ch #0 Aux Transfer Counter
Disp_to_DMA_0_Aux_Link_Pointer  .set    00a8h   ; DMA Ch #0 Aux Link Pointer
Disp_to_DMA_1_Control           .set    00b0h   ; DMA Ch #1 Control Register
Disp_to_DMA_1_Source            .set    00b1h   ; DMA Ch #1 Source Address
Disp_to_DMA_1_Source_Index      .set    00b2h   ; DMA Ch #1 Source Address Index
Disp_to_DMA_1_Counter           .set    00b3h   ; DMA Ch #1 Transfer Counter
Disp_to_DMA_1_Destination       .set    00b4h   ; DMA Ch #1 Destination Address
Disp_to_DMA_1_Dest_Index        .set    00b5h   ; DMA Ch #1 Dest. Address Index
Disp_to_DMA_1_Link_Pointer      .set    00b6h   ; DMA Ch #1 Link Pointer
Disp_to_DMA_1_Aux_Counter       .set    00b7h   ; DMA Ch #1 Aux Transfer Counter
Disp_to_DMA_1_Aux_Link_Pointer  .set    00b8h   ; DMA Ch #1 Aux Link Pointer
Disp_to_DMA_2_Control           .set    00c0h   ; DMA Ch #2 Control Register
Disp_to_DMA_2_Source            .set    00c1h   ; DMA Ch #2 Source Address
Disp_to_DMA_2_Source_Index      .set    00c2h   ; DMA Ch #2 Source Address Index
Disp_to_DMA_2_Counter           .set    00c3h   ; DMA Ch #2 Transfer Counter
Disp_to_DMA_2_Destination       .set    00c4h   ; DMA Ch #2 Destination Address
Disp_to_DMA_2_Dest_Index        .set    00c5h   ; DMA Ch #2 Dest. Address Index
Disp_to_DMA_2_Link_Pointer      .set    00c6h   ; DMA Ch #2 Link Pointer
Disp_to_DMA_2_Aux_Counter       .set    00c7h   ; DMA Ch #2 Aux Transfer Counter
Disp_to_DMA_2_Aux_Link_Pointer  .set    00c8h   ; DMA Ch #2 Aux Link Pointer
Disp_to_DMA_3_Control           .set    00d0h   ; DMA Ch #3 Control Register
Disp_to_DMA_3_Source            .set    00d1h   ; DMA Ch #3 Source Address
Disp_to_DMA_3_Source_Index      .set    00d2h   ; DMA Ch #3 Source Address Index
Disp_to_DMA_3_Counter           .set    00d3h   ; DMA Ch #3 Transfer Counter
Disp_to_DMA_3_Destination       .set    00d4h   ; DMA Ch #3 Destination Address
Disp_to_DMA_3_Dest_Index        .set    00d5h   ; DMA Ch #3 Dest. Address Index
Disp_to_DMA_3_Link_Pointer      .set    00d6h   ; DMA Ch #3 Link Pointer
Disp_to_DMA_3_Aux_Counter       .set    00d7h   ; DMA Ch #3 Aux Transfer Counter
Disp_to_DMA_3_Aux_Link_Pointer  .set    00d8h   ; DMA Ch #3 Aux Link Pointer
Disp_to_DMA_4_Control           .set    00e0h   ; DMA Ch #4 Control Register
Disp_to_DMA_4_Source            .set    00e1h   ; DMA Ch #4 Source Address
Disp_to_DMA_4_Source_Index      .set    00e2h   ; DMA Ch #4 Source Address Index
Disp_to_DMA_4_Counter           .set    00e3h   ; DMA Ch #4 Transfer Counter
Disp_to_DMA_4_Destination       .set    00e4h   ; DMA Ch #4 Destination Address
Disp_to_DMA_4_Dest_Index        .set    00e5h   ; DMA Ch #4 Dest. Address Index
Disp_to_DMA_4_Link_Pointer      .set    00e6h   ; DMA Ch #4 Link Pointer
Disp_to_DMA_4_Aux_Counter       .set    00e7h   ; DMA Ch #4 Aux Transfer Counter
Disp_to_DMA_4_Aux_Link_Pointer  .set    00e8h   ; DMA Ch #4 Aux Link Pointer
Disp_to_DMA_5_Control           .set    00f0h   ; DMA Ch #5 Control Register
Disp_to_DMA_5_Source            .set    00f1h   ; DMA Ch #5 Source Address
Disp_to_DMA_5_Source_Index      .set    00f2h   ; DMA Ch #5 Source Address Index
Disp_to_DMA_5_Counter           .set    00f3h   ; DMA Ch #5 Transfer Counter
Disp_to_DMA_5_Destination       .set    00f4h   ; DMA Ch #5 Destination Address
Disp_to_DMA_5_Dest_Index        .set    00f5h   ; DMA Ch #5 Dest. Address Index
Disp_to_DMA_5_Link_Pointer      .set    00f6h   ; DMA Ch #5 Link Pointer
Disp_to_DMA_5_Aux_Counter       .set    00f7h   ; DMA Ch #5 Aux Transfer Counter
Disp_to_DMA_5_Aux_Link_Pointer  .set    00f8h   ; DMA Ch #5 Aux Link Pointer

*****************************************************************************
*   Base Address of the Dual Port Memory                                    *
*****************************************************************************

Base_of_Dual_Port_Memory   .set     80000000h   ; Base of Dual-Port_Memory

*****************************************************************************
*   Displacement to Interrupt Service Routine Vectors in IVT                *
*   (16 bit constants)                                                      *
*****************************************************************************

Disp_to_NMI_Vector              .set    0001h   ; Non-Maskable Interrupt
Disp_to_TINT0_Vector            .set    0002h   ; Timer 0 Interrupt
Disp_to_IIOF0_Vector            .set    0003h   ; IIOF0 Interrupt
Disp_to_IIOF1_Vector            .set    0004h   ; IIOF1 Interrupt
Disp_to_IIOF2_Vector            .set    0005h   ; IIOF2 Interrupt
Disp_to_IIOF3_Vector            .set    0006h   ; IIOF3 Interrupt
Disp_to_ICFULL0_Vector          .set    000dh   ; In Ch. Full C.P. 0 Intrupt
Disp_to_ICRDY0_Vector           .set    000eh   ; In Ch. Ready C.P. 0 Intrupt
Disp_to_OCRDY0_Vector           .set    000fh   ; Out Ch. Ready C.P. 0 Intrupt
Disp_to_OCEMPTY0_Vector         .set    0010h   ; Out Ch. Empty C.P. 0 Intrupt
Disp_to_ICFULL1_Vector          .set    0011h   ; In Ch. Full C.P. 1 Intrupt
Disp_to_ICRDY1_Vector           .set    0012h   ; In Ch. Ready C.P. 1 Intrupt
Disp_to_OCRDY1_Vector           .set    0013h   ; Out Ch. Ready C.P. 1 Intrupt
Disp_to_OCEMPTY1_Vector         .set    0014h   ; Out Ch. Empty C.P. 1 Intrupt
Disp_to_ICFULL2_Vector          .set    0015h   ; In Ch. Full C.P. 2 Intrupt
Disp_to_ICRDY2_Vector           .set    0016h   ; In Ch. Ready C.P. 2 Intrupt
Disp_to_OCRDY2_Vector           .set    0017h   ; Out Ch. Ready C.P. 2 Intrupt
Disp_to_OCEMPTY2_Vector         .set    0018h   ; Out Ch. Empty C.P. 2 Intrupt
Disp_to_ICFULL3_Vector          .set    0019h   ; In Ch. Full C.P. 3 Intrupt
Disp_to_ICRDY3_Vector           .set    001ah   ; In Ch. Ready C.P. 3 Intrupt
Disp_to_OCRDY3_Vector           .set    001bh   ; Out Ch. Ready C.P. 3 Intrupt
Disp_to_OCEMPTY3_Vector         .set    001ch   ; Out Ch. Empty C.P. 3 Intrupt
Disp_to_ICFULL4_Vector          .set    001dh   ; In Ch. Full C.P. 4 Intrupt
Disp_to_ICRDY4_Vector           .set    001eh   ; In Ch. Ready C.P. 4 Intrupt
Disp_to_OCRDY4_Vector           .set    001fh   ; Out Ch. Ready C.P. 4 Intrupt
Disp_to_OCEMPTY4_Vector         .set    0020h   ; Out Ch. Empty C.P. 4 Intrupt
Disp_to_ICFULL5_Vector          .set    0021h   ; In Ch. Full C.P. 5 Intrupt
Disp_to_ICRDY5_Vector           .set    0022h   ; In Ch. Ready C.P. 5 Intrupt
Disp_to_OCRDY5_Vector           .set    0023h   ; Out Ch. Ready C.P. 5 Intrupt
Disp_to_OCEMPTY5_Vector         .set    0024h   ; Out Ch. Empty C.P. 5 Intrupt
Disp_to_DMAINT0_Vector          .set    0025h   ; DMA Ch. 0 Interrupt
Disp_to_DMAINT1_Vector          .set    0026h   ; DMA Ch. 1 Interrupt
Disp_to_DMAINT2_Vector          .set    0027h   ; DMA Ch. 2 Interrupt
Disp_to_DMAINT3_Vector          .set    0028h   ; DMA Ch. 3 Interrupt
Disp_to_DMAINT4_Vector          .set    0029h   ; DMA Ch. 4 Interrupt
Disp_to_DMAINT5_Vector          .set    002ah   ; DMA Ch. 5 Interrupt
Disp_to_TINT1_Vector            .set    002bh   ; Timer 1 Interrupt

*****************************************************************************
*   Address of the VSBTCR and VSBCSR (for DSP #2 only)                      *
*****************************************************************************

VSBTCR_and_VSBCSR_Address  .set     3ffe8000h   ; VSBTCR/VSBCSR (DSP #2)
