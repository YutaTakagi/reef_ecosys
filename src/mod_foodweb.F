
!!!=== Copyright (c) 2012-2025 Takashi NAKAMURA  =====
!!!      modified by Shinya AMANO, Faisal AMRI, Jorge SUAREZ

#include "cppdefs.h"


!!!**** MODULE OF FOOD WEB *******************************************

MODULE mod_foodweb
#if defined FOODWEB

  implicit none

  integer, parameter :: Ndom   = 2    ! Number of Dissolved Organic Matter types
  integer, parameter ::  iLDOM = 1    !  1: Labile DOM
  integer, parameter ::  iRDOM = 2    !  2: Refractory DOM

  integer, parameter :: Npom   = 3    ! Number of Particulate Organic Matter types
  integer, parameter ::  iLPOM = 1    !  1: Labile POM
  integer, parameter ::  iRPOM = 2    !  2: Refractory POM
  integer, parameter ::  iCPOM = 3    !  3: Coarse POM (leaf litter, etc.)

  integer, parameter :: Nphy   = 4    ! Number of Phytoplankton types
  integer, parameter ::  iDnfl = 1    !  1: Dinoflagellate
  integer, parameter ::  iDtm  = 2    !  2: Diatom
  integer, parameter ::  iCcl  = 3    !  3: Coccolithophorids
  integer, parameter ::  iCya  = 4    !  4: Cyanobacteria
   
  integer, parameter :: Nzoo   = 1    ! Number of Zooplankton types
  integer, parameter ::  iGzoo = 1    !  1: General Zooplankton

  integer, parameter :: Npim    = 1   ! Number of Particulate Inorganic Matter types
  integer, parameter ::  iCaCO3 = 1   !  1: Coccolith (CaCO3)

!---- POM sinking parameters           LPOM    LPOM    CPOM    
  real(8), parameter :: rho_POM(3) = [ 1.1d0,  1.1d0,  0.99d0 ]  ! ***** POM density [g/cm3] 
  real(8), parameter :: D_POM(3)   = [ 1.0d-1, 1.0d-1, 10.0d0 ]  ! ***** POM diameter [mm] 

!---- PIM sinking parameters
  real(8), parameter :: rho_PIM(1) = [ 1.4d0  ]  ! ***** PIM density [g/cm3] 
  real(8), parameter :: D_PIM(1)   = [ 1.0d-2 ]  ! ***** PIM diameter [mm] 

CONTAINS

!!! **********************************************************************
!!!  Main program of foodweb model (Modified from Yamamoto et al. under review)
!!! **********************************************************************

  SUBROUTINE foodweb &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tmp            &   ! Temperature (oC)
    , Sal            &   ! Salinity (PSU)
    , DIC            &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
    , TA             &   ! Total alkalinity (TA: umol kg-1)
    , DOx            &   ! Dissolved oxygen (umol L-1)
    , DOC            &   ! DOC(Ndom): Dissolved organic carbon (DOC: umol L-1)
    , POC            &   ! POC(Npom): Particulate organic carbon (POC: umol L-1)
    , PHY            &   ! PHY(Nphy): phytoplankton (umol C L-1)
    , ZOO            &   ! ZOO(Nzoo): zooplankton (umol C L-1)
    , PIC            &   ! PIC(Npim): Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#if defined NUTRIENTS         
    , NO3            &   ! NO3 (umol L-1)
    , NH4            &   ! NH4 (umol L-1)
    , PO4            &   ! PO4 (umol L-1)
    , DON            &   ! Dissolved organic nitrogen (DON: umol L-1)
    , PON            &   ! Particulate organic nitrogen (PON: umol L-1)
    , DOP            &   ! Dissolved organic phosporius (DOP: umol L-1)
    , POP            &   ! Particulate organic phosporius (POP: umol L-1)
#endif
#if defined COT_STARFISH         
    , COTe           &   ! COT starfish egg (umol L-1)
    , COTl           &   ! COT starfish larvae (umol L-1)
#endif
!   output parameters
    , dDIC_dt        &   ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dTA_dt         &   ! dTA/dt   (umol kg-1 s-1) 
    , dDOx_dt        &   ! dDOx/dt  (umol L-1 s-1) 
    , dDOC_dt        &   ! dDOC/dt  (umol L-1 s-1) 
    , dPOC_dt        &   ! dPOC/dt  (umol L-1 s-1) 
    , dPHY_dt        &   ! dPHY1/dt  (umol L-1 s-1)  
    , dZOO_dt        &   ! dZOO/dt  (umol L-1 s-1)  
    , dPIC_dt        &   ! dPIC/dt  (umol L-1 s-1)
#if defined NUTRIENTS
    , dNO3_dt        &   ! dNO3/dt (umol L-1 s-1)
    , dNH4_dt        &   ! dNH4/dt (umol L-1 s-1)
    , dPO4_dt        &   ! dPO4/dt (umol L-1 s-1)
    , dDON_dt        &   ! dDON/dt (umol L-1 s-1)
    , dPON_dt        &   ! dPON/dt (umol L-1 s-1)
    , dDOP_dt        &   ! dDOP/dt (umol L-1 s-1)
    , dPOP_dt        &   ! dPOP/dt (umol L-1 s-1)
#endif                                
#if defined COT_STARFISH         
    , dCOTe_dt       &   ! dCOTe/dt (umol L-1 s-1)
    , dCOTl_dt       &   ! dCOTl/dt (umol L-1 s-1)
#endif
    , pH             &   ! pH
    , Wcal           &   ! Calcite saturation state
    , Warg           &   ! Aragonite saturation state
    )

!-----------------------------------------------------------------------
!
    USE mod_geochem

    implicit none

!        input parameters
    integer, intent(in) :: ng, n, i, j    ! ng: nested grid number; n: coral compartment; i,j: position
    real(8), intent(in) :: dt             ! Time step (sec)
    real(8), intent(in) :: PFD            ! Photon flux density (umol m-2 s-1)
    real(8), intent(in) :: rho_sw         ! Density of seawater (g cm-3)
    real(8), intent(in) :: Tmp            ! Temperature (oC)
    real(8), intent(in) :: Sal            ! Salinity (PSU)
    real(8), intent(in) :: DIC(N_Csp)     ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: TA             ! Total alkalinity (TA: umol kg-1)
    real(8), intent(in) :: DOx            ! Dissolved oxygen (umol L-1)
    real(8), intent(in) :: DOC(N_Csp,Ndom)      ! Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC(N_Csp,Npom)      ! Particulate organic carbon (POC: umol L-1)
    real(8), intent(in) :: PHY(N_Csp,Nphy)      ! phytoplankton1 (umol C L-1), dinoflagellate
    real(8), intent(in) :: ZOO(N_Csp,Nzoo)      ! zooplankton (umol C L-1)
    real(8), intent(in) :: PIC(N_Csp,Npim)      ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#if defined NUTRIENTS         
    real(8), intent(in) :: NO3(N_Nsp)            ! NO3 (umol L-1)
    real(8), intent(in) :: NH4(N_Nsp)            ! NH4 (umol L-1)
    real(8), intent(in) :: PO4(N_Nsp)            ! PO4 (umol L-1)
    real(8), intent(in) :: DON(N_Nsp,Ndom)      ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON(N_Nsp,Npom)      ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP(N_Psp,Ndom)      ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP(N_Psp,Npom)      ! Particulate organic phosporius (POP: umol L-1)
#endif
#if defined COT_STARFISH         
    real(8), intent(in) :: COTe           ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl           ! COT starfish larvae (umol L-1)
#endif
!   output parameters
    real(8), intent(out) :: dDIC_dt(N_Csp)       ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    real(8), intent(out) :: dTA_dt        ! dTA/dt   (umol kg-1 s-1) 
    real(8), intent(out) :: dDOx_dt       ! dDOx/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dDOC_dt(N_Csp,Ndom) ! dDOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt(N_Csp,Npom) ! dPOC/dt  (umol L-1 s-1) 
    real(8), intent(out) :: dPHY_dt(N_Csp,Nphy) ! dPHY1/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dZOO_dt(N_Csp,Nzoo) ! dZOO/dt  (umol L-1 s-1)  
    real(8), intent(out) :: dPIC_dt(N_Csp,Npim) ! dPIC/dt  (umol L-1 s-1) 
#if defined NUTRIENTS
    real(8), intent(out) :: dNO3_dt(N_Nsp)       ! dNO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt(N_Nsp)       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt(N_Psp)       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDON_dt(N_Nsp,Ndom) ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt(N_Nsp,Npom) ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt(N_Psp,Ndom) ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt(N_Psp,Npom) ! dPOP/dt (umol L-1 s-1)
#endif                                
#if defined COT_STARFISH
    real(8), intent(out) :: dCOTe_dt      ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt      ! dCOTl/dt (umol L-1 s-1)
#endif
    real(8), intent(out) :: pH             ! pH
    real(8), intent(out) :: Wcal           ! Calcite saturation state
    real(8), intent(out) :: Warg           ! Aragonite saturation state

!!!------------Set parameters  ----------------------------------

!------- Phytoplankton parameters ------------------------
!                                     Dnfl      Dtm        Ccl      Cya
!  Maximum photosynthetic rate at 0 oC ( s-1) (0.4d0 d-1; Kishi et al., 2007)0.3
    real(8), parameter :: k_Pp(4) = [ 3.5d-6,   1.0d-5,   3.5d-6,   1.0d-6]  ! (s-1)   JS_edit check reference values for all parameters
!  Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)          
    real(8), parameter :: b_Pp(4) = [ 0.0693d0, 0.0693d0, 0.0693d0, 0.0693d0 ]  ! (degC-1) 
!  Optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)     
    real(8), parameter :: Ip(4)   = [ 90.0d0,   90.0d0,   90.0d0,   90.0d0   ] ! (umol m-2 s-1) JS_edit check reference values
!  Respiration rate at 0 oC (Kawamiya et al., 1995)
    real(8), parameter :: k_Rp(4) = [ 3.5d-7,   3.5d-7,   3.5d-7,   3.5d-7   ]    ! (s-1)
!  Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Rp(4) = [ 0.0519d0, 0.0519d0, 0.0519d0, 0.0519d0 ] ! (degC-1)
!  Mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day? 0.0088; Kishi et al., 2001)0.0066     
    real(8), parameter :: k_Mp(4) = [ 3.3d-7,   3.3d-7,   6.5d-7,   6.5d-7   ] ! (L umol-1 s-1)
!  Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
    real(8), parameter :: b_Mp(4) = [ 0.069d0,  0.069d0,  0.069d0,  0.069d0  ] ! (degC-1)   
!  PHY ratio of extracellular excretion to production 0.135(Kawamiya et al., 1995)    
    real(8), parameter :: k_Ep(4) = [ 0.135d0,  0.135d0,  0.135d0,  0.135d0  ] ! (no dim.) 
    real(8), parameter :: rLDOMphy = 0.99d0  ! (Labile DOM)/(Total DOM) of phytoplankton excreted DOM (no dim.) 
#if defined NUTRIENTS
!  PHY Nutrient Assimilation Parameter         
!                                       Dnfl    Dtm     Ccl     Cya
    real(8), parameter :: Kp_NH4(4) = [ 3.0d-1, 5.0d-2, 1.0d-2, 2.0d-1 ]      ! (µmol L-1)       JS_edit   
    real(8), parameter :: Kp_NO3(4) = [ 2.0d-1, 5.0d-1, 2.0d-1, 5.0d0  ]       ! (µmol L-1)       JS_edit 
    real(8), parameter :: Kp_PO4(4) = [ 5.0d-1, 5.0d-2, 6.0d-3, 1.5d-1 ]      ! (µmol L-1)       JS_edit 
    ! (1.5L umolN-1; Kishi et al., 2007)  0.01  0.226  0.5
!                                       Dnfl    Dtm     Ccl     Cya
    real(8), parameter :: psi(4) =    [ 0.5d0,  0.5d0,  0.5d0,  0.5d0  ] !(L umolC-1)   
#endif
!------- Zooplankton parameters ------------------------
!                                            from  Dnfl       Dtm        Ccl         Cya
    real(8), parameter :: k_Gp2z(4,1) = reshape( [ 3.0d-6,    2.0d-6,    3.0d-7 ,    3.0d-7    ], [4,1] ) ! to Gzoo! (s-1)          Maximum grazing rate of PHY1 by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)0.16
    real(8), parameter :: b_Gp2z(4,1) = reshape( [ 0.0693d0,  0.0693d0,  0.0693d0,   0.0693d0  ], [4,1] ) ! to Gzoo! (degC-1)       Temperature coefficient for ZOO grazing (0.063d0degC-1;Kawamiya et al., 1995)
    real(8), parameter :: e_Gp2z(4,1) = reshape( [ 0.250d0,   0.250d0,   0.250d0,    0.250d0   ], [4,1] ) ! to Gzoo! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)

    real(8), parameter :: k_Rz(1) = [ 6.0d-8    ]  ! (s-1)            ZOO respiration rate at 0  oC  																					!!!(Tuning) Check
    real(8), parameter :: b_Rz(1) = [ 0.0693d0  ]  ! (degC-1)         Temperature coefficient for ZOO respiration rate (Kawamiya et al., 1995)
    real(8), parameter :: k_Mz(1) = [ 1.0d-8    ]  ! (L umol-1 s-1)   ZOO mortality rate at 0 oC 3.0d0/86400.0d0(0.0088d0 umol-1 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Mz(1) = [ 0.0693d0  ]  ! (degC-1)         Temperature coefficient for ZOO mortality (Kawamiya et al., 1995)
    real(8), parameter :: lam(1)  = [ 0.211d0   ]  ! ((umol C L-1)-1) zooplankton Ivlev constant for PHY1 (1.4L/umolN.0.211; Kishi et al., 2007)
    real(8), parameter :: t_Gp2z(4,1) = reshape( [ 0.265d0,   0.265d0,   0.265d0,   0.265d0   ], [4,1] ) ! (umol C L-1)       PHY1 threshold value for grazing by ZOO (0.04umolN/L0.265; Kishi et al., 2007)
!------- Microbial loop parameters  -------
!                                            from     LDOM      RDOM
    real(8), parameter :: k_Gdoc2z(2,1) = reshape( [ 1.0d-8,    0.0d0    ], [2,1] ) ! to Gzoo ! (s-1)          Maximum grazing rate of DOC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
!                                            from     LDOM      RDOM
    real(8), parameter :: b_Gdoc2z(2,1) = reshape( [ 0.0693d0,  0.0693d0 ], [2,1] ) ! to Gzoo ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)

!                                            from     LPOM      RPOM      CPOM
    real(8), parameter :: k_Gpoc2z(3,1) = reshape( [ 1.0d-7,   1.0d-7,    1.0d-7   ], [3,1] ) ! to Gzoo ! (s-1)          Maximum grazing rate of POC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
!                                            from     LPOM      RPOM      CPOM
    real(8), parameter :: b_Gpoc2z(3,1) = reshape( [ 0.0693d0, 0.0693d0,  0.0693d0 ], [3,1] ) ! to Gzoo ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
!                                            from     LDOM      RDOM
    real(8), parameter :: e_Gdoc2z(2,1) = reshape( [ 0.010d0,   0.010d0  ], [2,1] )        ! to Gzoo
!                                            from     LPOM      RPOM      CPOM
    real(8), parameter :: e_Gpoc2z(3,1) = reshape( [ 0.10d0,    0.10d0,   0.10d0   ], [3,1] ) ! to Gzoo ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
!------- Decomposition parameters --------------------
!                                        LDOM      RDOM
    real(8), parameter :: k_Ddoc(2) = [ 3.0d-7,   1.0d-11   ]    ! (s-1)    Decomposition rate of DOC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: k_Ddon(2) = [ 2.0d-7,   1.0d-11   ]    ! (s-1)    Decomposition rate of DON at 0 oC (0.02d0 d-1; Kishi et al., 2007)
    real(8), parameter :: k_Ddop(2) = [ 2.0d-7,   1.0d-11   ]    ! (s-1)    Decomposition rate of DOP at 0 oC 
!                                        LPOM      RPOM      CPOM
    real(8), parameter :: k_Dpoc(3) = [ 3.0d-7,   3.0d-7,  1.0d-10   ]    ! (s-1)    Decomposition rate of POC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: k_Dpon(3) = [ 1.0d-7,   1.0d-7,  1.0d-10   ]    ! (s-1)    Decomposition rate of PON at 0 oC (0.1d0 d-1; Kishi et al., 2007)
    real(8), parameter :: k_Dpop(3) = [ 1.0d-7,   1.0d-7,  1.0d-10   ]    ! (s-1)    Decomposition rate of POP at 0 oC 
!                                        LDOM      RDOM
    real(8), parameter :: b_Ddoc(2) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of DOC (Kishi et al., 2001)
    real(8), parameter :: b_Ddon(2) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of DON (Kishi et al., 2007)
    real(8), parameter :: b_Ddop(2) = [ 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of DOP 
!                                        LPOM      RPOM      CPOM
    real(8), parameter :: b_Dpoc(3) = [ 0.0693d0, 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: b_Dpon(3) = [ 0.0693d0, 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of POC (Kishi et al., 2007)
    real(8), parameter :: b_Dpop(3) = [ 0.0693d0, 0.0693d0, 0.0693d0 ]    ! (degC-1) Temperature coefficient for decomposition of POP 
!------- Decomposition parameters (POM -> DOM) --------------------
!                                              from     LPOM      RPOM      CPOM   
    real(8), parameter :: k_Dp2doc(3,2) = reshape( [   1.0d-6,   1.0d-6,   1.0d-9       &      ! to LDOM
                                                    ,  1.0d-9,   1.0d-9,   1.0d-11  ], [3,2] ) ! to RDOM   ! (s-1)          Decomposition rate of POC at 0 oC (0.3d0 d-1; Kishi et al., 2001)
    real(8), parameter :: k_Dp2don(3,2) = reshape( [   1.0d-6,   1.0d-6,   1.0d-9       &      ! to LDOM 
                                                    ,  1.0d-9,   1.0d-9,   1.0d-11  ], [3,2] ) ! to RDOM   ! (s-1)          Decomposition rate of PON at 0 oC (0.02d0 d-1; Kishi et al., 2007)
    real(8), parameter :: k_Dp2dop(3,2) = reshape( [   1.0d-6,   1.0d-6,   1.0d-9       &      ! to LDOM 
                                                    ,  1.0d-9,   1.0d-9,   1.0d-11  ], [3,2] ) ! to RDOM   ! (s-1)          Decomposition rate of POP at 0 oC 
!                                              from     LPOM      RPOM      CPOM   
    real(8), parameter :: b_Dp2doc(3,2) = reshape( [  0.0693d0, 0.0693d0, 0.0693d0     &       ! to LDOM 
                                                    , 0.0693d0, 0.0693d0, 0.0693d0  ], [3,2] ) ! to RDOM   ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: b_Dp2don(3,2) = reshape( [  0.0693d0, 0.0693d0, 0.0693d0     &       ! to LDOM
                                                    , 0.0693d0, 0.0693d0, 0.0693d0  ], [3,2] ) ! to RDOM   ! (degC-1)       Temperature coefficient for decomposition of PON (Kishi et al., 2007)
    real(8), parameter :: b_Dp2dop(3,2) = reshape( [  0.0693d0, 0.0693d0, 0.0693d0     &       ! to LDOM
                                                    , 0.0693d0, 0.0693d0, 0.0693d0  ], [3,2] ) ! to RDOM   ! (degC-1)       Temperature coefficient for decomposition of POP 
!    real(8), parameter :: rLDOMpom = 0.95d0  ! (Labile DOM)/(Total DOM) of DOM decomposed from POM (no dim.) 
!------- Decomposition parameters (CPOM -> Detritus) --------------------
    real(8), parameter :: k_Dcpoc2dtr = 1.0d-9    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpoc2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
    real(8), parameter :: k_Dcpon2dtr = 1.0d-9    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpon2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
    real(8), parameter :: k_Dcpop2dtr = 1.0d-9    ! (s-1)     Decomposition rate of CPOM to Detritus at 0 oC
    real(8), parameter :: b_Dcpop2dtr = 0.0693d0  ! (degC-1)  Temperature coefficient for decomposition of CPOM to Detritus
!------- Physical parameters --------------------
#if defined NUTRIENTS
    real(8), parameter :: k_Nit = 3.0d-7     ! (s-1)      Nitrification rate at 0 oC (0.03d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Nit = 0.0693d0   ! (degC-1)   Temperature coefficient for nitrification (Kishi et al., 2007)
#endif
!------- Physical parameters --------------------
#if defined NUTRIENTS   

! Redfield Ratio (1934) C:N:P -> C:N=6.6  C:P=106    N:P=16
! Martiny et al  (2014) C:N:P -> C:N=7.4  C:P=163    N:P=22
! GLODAP Data           C:N:P -> C:N=9.2  C:P=131.9  N:P=14.4
!                                     Dnfl   Dtm    Ccl    Cya
    real(8), parameter :: rCNp(4) = [ 6.6d0, 6.6d0, 6.6d0, 6.6d0 ]    ! (no dim.) PHY1 C:N ratio    JS_edit Set to Redfield Ratio
    real(8), parameter :: rCPp(4) = [ 106d0, 106d0, 106d0, 106d0 ]    ! (no dim.) PHY1 C:P ratio
!                                     Gzoo
    real(8), parameter :: rCNz(1) = [ 9.2d0   ]    ! (no dim.) ZOO  C:N ratio
    real(8), parameter :: rCPz(1) = [ 131.9d0 ]    ! (no dim.) ZOO  C:P ratio
    real(8), parameter :: rNH4toNO3(4) = 0.3d0            ! (no dim.)  PHY NH4:Nitrogen ratio JS_edit make array to use 4 categories
#endif                                
    real(8), parameter :: rCCp(4) = [ 0.0d0, 0.0d0, 0.5d0, 0.0d0 ]  ! ***** (no dim.) CaCO3:OrgC ratio !!! Need Ref!!!!!!!!!!
!----- for carbon isotope ------------------------
#if defined CARBON_TRACE
!                                        iCt   Ctrac1  Ctrac1  Ctrac1
    real(8), parameter :: a_phot(4) = [ 1.0d0,  1.0d0,  1.0d0,  1.0d0 ] 
    real(8), parameter :: a_calc(4) = [ 1.0d0,  1.0d0,  1.0d0,  1.0d0 ] 
    real(8), parameter :: a_grz (4) = [ 1.0d0,  1.0d0,  1.0d0,  1.0d0 ] 
#else
!                                        iCt          iC13
    real(8), parameter :: a_phot(2) = [ 1.0d0, -20.0d-3 + 1.0d0 ] !!!!! To Be Confirmed
    real(8), parameter :: a_calc(2) = [ 1.0d0,   2.7d-3 + 1.0d0 ] !!!!! To Be Confirmed
    real(8), parameter :: a_grz (2) = [ 1.0d0,   1.0d-3 + 1.0d0 ] !!!!! To Be Confirmed
#endif
    real(8), parameter :: sn =   1.0d-20 ! A small number for avoiding a number divided by 0

!------- Local variables --------------------
    real(8) :: Pp(Nphy), Rp(Nphy), Mp(Nphy), Ep(Nphy), Ap(Nphy)
    real(8) :: Gp2z(Nphy,Nzoo), Rz(Nzoo), Mz(Nzoo)
    real(8) :: Gdoc2z(Ndom,Nzoo), Gpoc2z(Npom,Nzoo)
    real(8) :: Ddoc(Ndom), Dpoc(Npom), Dp2doc(Npom,Ndom)
    real(8) :: Dcpoc2dtr
    real(8) :: CO3, Dissol
#if defined CARBON_ISOTOPE
    real(8) :: rDI13C, rPO13C(Npom), rDO13C(Ndom)
    real(8) :: rPHY13C(Nphy), rZOO13C(Nzoo), rPI13C(Npim)
#endif
#if defined NUTRIENTS
    real(8) :: Ddon(Ndom), Dpon(Npom), Dp2don(Npom,Ndom)
    real(8) :: Ddop(Ndom), Dpop(Npom), Dp2dop(Npom,Ndom)
    real(8) :: Vp_NH4(Nphy), Vp_NO3(Nphy)
    real(8) :: Vp_PO4(Nphy)
    real(8) :: Dcpon2dtr, Dcpop2dtr
    real(8) :: Nit
#endif

    real(8) :: rDIC(N_Csp)     ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8) :: rDOC(N_Csp,Ndom)      ! Dissolved organic carbon (DOC: umol L-1)
    real(8) :: rPOC(N_Csp,Npom)      ! Particulate organic carbon (POC: umol L-1)
    real(8) :: rPHY(N_Csp,Nphy)      ! phytoplankton1 (umol C L-1), dinoflagellate
    real(8) :: rZOO(N_Csp,Nzoo)      ! zooplankton (umol C L-1)
    real(8) :: rPIC(N_Csp,Npim)      ! Particulate inorganic carbon (PIC: umol L-1), coccolith (CaCO3)
#if defined NUTRIENTS         
    real(8) :: rNO3(N_Nsp)            ! NO3 (umol L-1)
    real(8) :: rNH4(N_Nsp)            ! NH4 (umol L-1)
    real(8) :: rPO4(N_Nsp)            ! PO4 (umol L-1)
    real(8) :: rDON(N_Nsp,Ndom)      ! Labile Dissolved organic nitrogen (DON: umol L-1)
    real(8) :: rPON(N_Nsp,Npom)      ! Particulate organic nitrogen (PON: umol L-1)
    real(8) :: rDOP(N_Psp,Ndom)      ! Labile Dissolved organic phosporius (DOP: umol L-1)
    real(8) :: rPOP(N_Psp,Npom)      ! Particulate organic phosporius (POP: umol L-1)
#endif
    
    integer :: k,m,isp
    real(8) :: cf1, cf2, cf3

!===== Initiallize tracer parameters =================================
    dDIC_dt(:) = 0.0d0
    dTA_dt  = 0.0d0
    dDOx_dt = 0.0d0
    dPOC_dt(:,:) = 0.0d0
    dDOC_dt(:,:) = 0.0d0
    dPHY_dt(:,:) = 0.0d0
    dZOO_dt(:,:) = 0.0d0
    dPIC_dt(:,:) = 0.0d0
#if defined NUTRIENTS
    dNO3_dt(:) = 0.0d0
    dNH4_dt(:) = 0.0d0
    dPO4_dt(:) = 0.0d0
    dDON_dt(:,:) = 0.0d0
    dPON_dt(:,:) = 0.0d0
    dDOP_dt(:,:) = 0.0d0
    dPOP_dt(:,:) = 0.0d0
#endif                                
#if defined COT_STARFISH
    dCOTe_dt = 0.0d0
    dCOTl_dt = 0.0d0
#endif

!===== Calculate istope ratio =================================
    DO isp=1,N_Csp
      rDIC(isp) = Iso_ratio( DIC(isp), DIC(iCt) )
    ENDDO
#if defined NUTRIENTS
    DO isp=1,N_Nsp
      rNO3(isp) = Iso_ratio( NO3(isp), NO3(iNt) )
      rNH4(isp) = Iso_ratio( NH4(isp), NH4(iNt) )
    ENDDO
    DO isp=1,N_Psp
      rPO4(isp) = Iso_ratio( PO4(isp), PO4(iPt) )
    ENDDO
#endif
    DO m = 1,Ndom
      DO isp=1,N_Csp
        rDOC(isp,m) = Iso_ratio( DOC(isp,m), DOC(iCt,m) )  
      ENDDO
#if defined NUTRIENTS
      DO isp=1,N_Nsp
        rDON(isp,m) = Iso_ratio( DON(isp,m), DON(iNt,m) )   
      ENDDO
      DO isp=1,N_Psp
        rDOP(isp,m) = Iso_ratio( DOP(isp,m), DOP(iPt,m) )
      ENDDO
#endif
    ENDDO   
    DO m = 1,Npom-1
      DO isp=1,N_Csp
        rPOC(isp,m) = Iso_ratio( POC(isp,m), POC(iCt,m) ) 
      ENDDO
#if defined NUTRIENTS
      DO isp=1,N_Nsp
        rPON(isp,m) = Iso_ratio( PON(isp,m), PON(iNt,m) )   
      ENDDO
      DO isp=1,N_Psp
        rPOP(isp,m) = Iso_ratio( POP(isp,m), POP(iPt,m) )
      ENDDO
#endif
    ENDDO   
    DO m = 1,Nphy
      DO isp=1,N_Csp
        rPHY(isp,m) = Iso_ratio( PHY(isp,m), PHY(iCt,m) ) 
      ENDDO
    ENDDO   
    DO m = 1,Nzoo
      DO isp=1,N_Csp
        rZOO(isp,m) = Iso_ratio( ZOO(isp,m), ZOO(iCt,m) ) 
      ENDDO
    ENDDO   
    DO m = 1,Npim
      DO isp=1,N_Csp
        rPIC(isp,m) = Iso_ratio( PIC(isp,m), PIC(iCt,m) ) 
      ENDDO
    ENDDO   
  

!===== Phytoplankton reaction =================================

    DO k=1, Nphy
!----- Gross photosynthetic rate (umolC L-1 s-1) -----------------

!      Pphy = k_Pphy * exp(b_Pphy*Tmp) * PFD/Iphy*exp(1.0d0-PFD/Iphy) * PHY
      Pp(k) = k_Pp(k) * exp(b_Pp(k)*Tmp) * tanh(PFD/Ip(k)) * MAX(PHY(iCt,k), 0.0d0)
      IF(DIC(iCt) <= 0.0d0) THEN !-----For Error handling
        Pp(k) = 0.0d0
      ENDIF
      Pp(k) = MAX( Pp(k), 0.0d0 )  !!! Error handling

!----- Assimilation rate (umolC L-1 s-1) ----------------- JS_edit find more references about switching NH4 to NO3, etc (nutrient assimilation)

#if defined NUTRIENTS
        cf1 = MAX(NH4(iNt), 0.0d0)  !!! Error handling
        cf2 = MAX(NO3(iNt), 0.0d0)  !!! Error handling
        cf3 = MAX(PO4(iPt), 0.0d0)  !!! Error handling

      IF(k == iDnfl) THEN
        Vp_NH4(k) = cf1*cf1/(cf1*cf1+Kp_NH4(k)*Kp_NH4(k)) * rNH4toNO3(k)
        Vp_NO3(k) = cf2*cf2/(cf2*cf2+Kp_NO3(k)*Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4(k))    
        Vp_PO4(k) = cf3*cf3/(cf3*cf3+Kp_PO4(k)*Kp_PO4(k))
      ELSE IF(k == iDtm) THEN   
        Vp_NH4(k) = cf1*cf1/(cf1*cf1+Kp_NH4(k)*Kp_NH4(k)) * rNH4toNO3(k)
        Vp_NO3(k) = cf2*cf2/(cf2*cf2+Kp_NO3(k)*Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4(k))    
        Vp_PO4(k) = cf3*cf3/(cf3*cf3+Kp_PO4(k)*Kp_PO4(k))
      ELSE IF(k == iCya) THEN   
        Vp_NH4(k) = cf1*cf1/(cf1*cf1+Kp_NH4(k)*Kp_NH4(k)) * rNH4toNO3(k)
        Vp_NO3(k) = cf2*cf2/(cf2*cf2+Kp_NO3(k)*Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4toNO3(k))    
        Vp_PO4(k) = cf3*cf3/(cf3*cf3+Kp_PO4(k)*Kp_PO4(k))
      ELSE
        Vp_NH4(k) = cf1/(cf1+Kp_NH4(k)) * rNH4toNO3(k)
        Vp_NO3(k) = cf2/(cf2+Kp_NO3(k)) * exp(-psi(k) * cf1) * (1.0d0 - rNH4toNO3(k))     
        Vp_PO4(k) = cf3/(cf3+Kp_PO4(k))
      END IF
      Vp_NH4(k) = MAX( Vp_NH4(k), 0.0d0 )  !!! Error handling
      Vp_NO3(k) = MAX( Vp_NO3(k), 0.0d0 )  !!! Error handling
      Vp_PO4(k) = MAX( Vp_PO4(k), 0.0d0 )  !!! Error handling
      
      Ap(k) = MIN( Vp_NH4(k)+Vp_NO3(k), Vp_PO4(k) ) ! Assimilation rate

      Ap(k) = MIN( Ap(k), 1.0d0 )  !!! Error handling

      Ap(k) = Ap(k) * Pp(k)

#else
      Ap(k) = (1.0d0-k_Ep(k)) * Pp(k)

#endif

!     === Mass balance ===
      DO isp=1,N_Csp
        dPHY_dt(isp,k) = dPHY_dt(isp,k) + Ap(k)*a_phot(isp)*rDIC(isp)
        dDIC_dt(isp) = dDIC_dt(isp) - Ap(k) - Ap(k)*rCCp(k)*a_calc(isp)*rDIC(isp)      ! calcification
      ENDDO
      dTA_dt = dTA_dt - Ap(k)*rCCp(k)*2.0d0  ! calcification
      dDOx_dt = dDOx_dt + Ap(k)  !!!!!!**************
#if defined NUTRIENTS
      if(Vp_NO3(k)+Vp_NH4(k) /= 0.0d0) then ! yt_edit prevent div by 0
        DO isp=1,N_Nsp
          dNO3_dt(isp) = dNO3_dt(isp) - Ap(k)*Vp_NO3(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)*rNO3(isp)
          dNH4_dt(isp) = dNH4_dt(isp) - Ap(k)*Vp_NH4(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)*rNH4(isp)
        ENDDO
        dDOx_dt = dDOx_dt + 2.0d0 * Ap(k)*Vp_NO3(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)  !!!*********************
        dTA_dt  = dTA_dt  + 1.0d0 * Ap(k)*Vp_NO3(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)  ! increase of alkalinity by 1 mole when nitrate or nitrite is the N source
        dTA_dt  = dTA_dt  - 1.0d0 * Ap(k)*Vp_NH4(k)/(Vp_NH4(k)+Vp_NO3(k))/rCNp(k)  ! decrease of alkalinity by 1 mole when ammonia is used
      endif
      DO isp=1,N_Nsp
        dPO4_dt(isp) = dPO4_dt(isp) - Ap(k)/rCPp(k)*rPO4(isp)
      ENDDO
      dTA_dt    = dTA_dt  + 2.0d0 * Ap(k)/rCPp(k)  ! increases alkalinity by 2 mole per mole HPO42-
#endif

!----- Excretion rate (umolC L-1 s-1) -----------------

#if defined NUTRIENTS
      Ep(k) = (Pp(k) - Ap(k))*k_Ep(k) 
#else
      Ep(k) = k_Ep(k) * Pp(k)
#endif

!     === Mass balance ===

      dDOx_dt = dDOx_dt + Ep(k)  !!!!!!**************
      DO isp=1,N_Csp
        dDIC_dt(isp) = dDIC_dt(isp) - Ep(k) *a_phot(isp)*rDIC(isp)
        dDOC_dt(isp,iLDOM) = dDOC_dt(isp,iLDOM) + Ep(k)*rLDOMphy *a_phot(isp)*rDIC(isp)  !!!!!!**************
        dDOC_dt(isp,iRDOM) = dDOC_dt(isp,iRDOM) + Ep(k)*(1.0d0-rLDOMphy*a_phot(isp))*rDIC(isp)  !!!!!!**************
      ENDDO
#if defined NUTRIENTS
!      dDON_dt(iLDOM) = dDON_dt(iLDOM) + Ep(k)*rLDOMphy/rCNp(k)  !!!!!!**************
!      dDOP_dt(iLDOM) = dDOP_dt(iLDOM) + Ep(k)*rLDOMphy/rCPp(k)  !!!!!!**************
!      dDON_dt(iRDOM) = dDON_dt(iRDOM) + Ep(k)*(1.0d0-rLDOMphy)/rCNp(k)  !!!!!!**************
!      dDOP_dt(iRDOM) = dDOP_dt(iRDOM) + Ep(k)*(1.0d0-rLDOMphy)/rCPp(k)  !!!!!!**************
#endif

!----- Respiration rate (umolC L-1 s-1) -----------------

      Rp(k) = k_Rp(k) * exp(b_Rp(k)*Tmp) * MAX(PHY(iCt,k), 0.0d0)
      ! IF(DOx <= 0.0d0) THEN !-----For Error handling
      !   Rp(k) = 0.0d0
      ! ENDIF
      Rp(k) = MAX( Rp(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      dDOx_dt = dDOx_dt - Rp(k)
      DO isp=1,N_Csp
        dPHY_dt(isp,k) = dPHY_dt(isp,k) - Rp(k) *rPHY(isp,k)
        dDIC_dt(isp) = dDIC_dt(isp) + Rp(k) *rPHY(isp,k)
        dPIC_dt(isp,iCaCO3) = dPIC_dt(isp,iCaCO3) + Rp(k)*rCCp(k)*rPIC(isp,k)  !!!??????????????? Check needed
      ENDDO
#if defined NUTRIENTS
      DO isp=1,N_Nsp
        dNH4_dt(isp) = dNH4_dt(isp) + Rp(k)/rCNp(k) *rPHY(isp,k)  !!!??????????????? Check needed
      ENDDO
      DO isp=1,N_Psp
        dPO4_dt(isp) = dPO4_dt(isp) + Rp(k)/rCPp(k) *rPHY(isp,k)  !!!??????????????? Check needed
      ENDDO
      dTA_dt = dTA_dt + 1.0d0*Rp(k)/rCNp(k) - 2.0d0*Rp(k)/rCPp(k)
#endif

!----- Mortality (umolC L-1 s-1) -----------------

      Mp(k) = k_Mp(k) * exp(b_Mp(k)*Tmp) * MAX(PHY(iCt,k),0.0d0)*MAX(PHY(iCt,k),0.0d0)
!      Mphy = k_Mphy * exp(b_Mphy*Tmp) * PHY
      Mp(k) = MAX( Mp(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      DO isp=1,N_Csp
        dPHY_dt(isp,k) = dPHY_dt(isp,k) - Mp(k) *rPHY(isp,k)
        dPOC_dt(isp,iLPOM) = dPOC_dt(isp,iLPOM) + Mp(k) *rPHY(isp,k)
        dPIC_dt(isp,iCaCO3) = dPIC_dt(isp,iCaCO3) + Mp(k)*rCCp(k) *a_calc(isp)/a_phot(isp)*rPHY(isp,k)  !!!!!
      ENDDO
#if defined NUTRIENTS
      DO isp=1,N_Nsp
        dPON_dt(isp,iLPOM) = dPON_dt(isp,iLPOM) + Mp(k)/rCNp(k) *rPHY(isp,k)  !!!!!!**************
      ENDDO
      DO isp=1,N_Psp
        dPOP_dt(isp,iLPOM) = dPOP_dt(isp,iLPOM) + Mp(k)/rCPp(k) *rPHY(isp,k)  !!!!!!**************
      ENDDO
#endif
    END DO

!===== Zooplankton reaction ======================================

    DO k=1, Nzoo

!----- Grazing rate of PHY by ZOO (umolC L-1 s-1) ----------------- JS_edit Difficult part: Mixotrophy from Dinoflagellates

      DO m=1, Nphy
        Gp2z(m,k) = k_Gp2z(m,k) * exp(b_Gp2z(m,k)*Tmp)    &
          * (1-exp(lam(k)*(t_Gp2z(m,k) - PHY(iCt,m)))) * ZOO(iCt,k)  !!!!!!!!!!!!!!!!!!!
        IF(Gp2z(m,k) <= 0.0d0) THEN !-----For Error handling
          Gp2z(m,k) = 0.0d0
        ENDIF
        Gp2z(m,k) = MAX( Gp2z(m,k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

        DO isp=1,N_Csp
          dPHY_dt(isp,m) = dPHY_dt(isp,m) - Gp2z(m,k) *rPHY(isp,m)
          dZOO_dt(isp,k) = dZOO_dt(isp,k) + Gp2z(m,k)*e_Gp2z(m,k) *a_grz(isp) *rPHY(isp,m)
          dPOC_dt(isp,iLPOM) = dPOC_dt(isp,iLPOM) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k)*a_grz(isp)) *rPHY(isp,m) !!!!****Need check
          dPIC_dt(isp,iCaCO3) = dPIC_dt(isp,iCaCO3) + Gp2z(m,k)*rCCp(m)*rPIC(isp,m) !!!!****Need check
        ENDDO
#if defined NUTRIENTS
        DO isp=1,N_Nsp
          dPON_dt(isp,iLPOM) = dPON_dt(isp,iLPOM) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))/rCNp(m)*rPHY(isp,m) !!!!****Need check
        ENDDO
        DO isp=1,N_Psp
          dPOP_dt(isp,iLPOM) = dPOP_dt(isp,iLPOM) + Gp2z(m,k)*(1.0d0-e_Gp2z(m,k))/rCPp(m)*rPHY(isp,m) !!!!****Need check
        ENDDO
#endif
      END DO

!!!------- Microbial loop (implicitly assumed) ---------------

!----- Grazing rate of DOC(iLDOM) by ZOO (umolC L-1 s-1) -----------------

      DO m=1, Ndom
        cf1 = DOC(iCt,m)
#if defined NUTRIENTS
        cf1 =  MIN( MIN(DOC(iCt,m),DON(iNt,m)*rCNz(k)) , DOP(iPt,m)*rCPz(k) )
#endif
        Gdoc2z(m,k) = k_Gdoc2z(m,k) * exp(b_Gdoc2z(m,k)*Tmp) * cf1 * ZOO(iCt,k) ! yt_edit consider making this michaelis-menten dependent on ZOO to not exceed exceed 100% uptake
        Gdoc2z(m,k) = MAX( Gdoc2z(m,k), 0.0d0 )  !!! Error handling

!     === Mass balance ===
        DO isp=1,N_Csp
          dZOO_dt(isp,k) = dZOO_dt(isp,k) + Gdoc2z(m,k)*e_Gdoc2z(m,k)*a_grz(isp) *rDOC(isp,m)
          dDOC_dt(isp,m) = dDOC_dt(isp,m) - Gdoc2z(m,k) *rDOC(isp,m)
          dDOC_dt(isp,iLDOM) = dDOC_dt(isp,iLDOM) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k)*a_grz(isp)) *rDOC(isp,m)
        ENDDO
#if defined NUTRIENTS
        DO isp=1,N_Nsp
          dDON_dt(isp,m) = dDON_dt(isp,m) - Gdoc2z(m,k)/rCNz(k) *rDON(isp,m)
          dDON_dt(isp,iLDOM) = dDON_dt(isp,iLDOM) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))/rCNz(k) *rZOO(isp,k)  !!! ********** Check needed
        ENDDO
        DO isp=1,N_Psp
          dDOP_dt(isp,m) = dDOP_dt(isp,m) - Gdoc2z(m,k)/rCPz(k) *rDOP(isp,m)
          dDOP_dt(isp,iLDOM) = dDOP_dt(isp,iLDOM) + Gdoc2z(m,k)*(1.0d0-e_Gdoc2z(m,k))/rCPz(k)*rZOO(isp,k)
        ENDDO
#endif
      END DO

!----- Grazing rate of POC by ZOO (umolC L-1 s-1) -----------------

      DO m=1, Npom
        cf1 = POC(iCt,m)
#if defined NUTRIENTS
        cf1 =  MIN( MIN(POC(iCt,m),PON(iNt,m)*rCNz(k)) , POP(iPt,m)*rCPz(k) )
#endif
        Gpoc2z(m,k) = k_Gpoc2z(m,k) * exp(b_Gpoc2z(m,k)*Tmp) * cf1 * ZOO(iCt,k)
        Gpoc2z(m,k) = MAX( Gpoc2z(m,k), 0.0d0 )  !!! Error handling

!     === Mass balance ===
        DO isp=1,N_Csp
          dZOO_dt(isp,k) = dZOO_dt(isp,k) + Gpoc2z(m,k)*e_Gpoc2z(m,k)*a_grz(isp)*rPOC(isp,m) !!!!****Need check
          dPOC_dt(isp,m) = dPOC_dt(isp,m) - Gpoc2z(m,k)*rPOC(isp,m)
          dPOC_dt(isp,iLPOM) = dPOC_dt(isp,iLPOM) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k)*a_grz(isp))*rPOC(isp,m) !!!!****Need check
        ENDDO
#if defined NUTRIENTS
        DO isp=1,N_Nsp
          dPON_dt(isp,m) = dPON_dt(isp,m) - Gpoc2z(m,k)/rCNz(k) *rPON(isp,m)
          dPON_dt(isp,iLPOM) = dPON_dt(isp,iLPOM) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))/rCNz(k) *rPON(isp,m)
        ENDDO
        DO isp=1,N_Psp
          dPOP_dt(isp,m) = dPOP_dt(isp,m) - Gpoc2z(m,k)/rCPz(k) *rPOP(isp,m)
          dPOP_dt(isp,iLPOM) = dPOP_dt(isp,iLPOM) + Gpoc2z(m,k)*(1.0d0-e_Gpoc2z(m,k))/rCPz(k) *rPOP(isp,m)
        ENDDO
#endif
      END DO
  
!----- Respiration rate (umolC L-1 s-1) -----------------

      Rz(k) = k_Rz(k) * exp(b_Rz(k)*Tmp) * ZOO(iCt,k)
      ! IF(DOx <= 0.0d0) THEN !-----For Error handling
      !   Rz(k) = 0.0d0
      ! ENDIF
      Rz(k) = MAX( Rz(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      dDOx_dt = dDOx_dt - Rz(k)
      DO isp=1,N_Csp
        dZOO_dt(isp,k) = dZOO_dt(isp,k) - Rz(k)*rZOO(isp,k)
        dDIC_dt(isp) = dDIC_dt(isp) + Rz(k)*rZOO(isp,k)
      ENDDO
#if defined NUTRIENTS
      DO isp=1,N_Nsp
        dNH4_dt(isp) = dNH4_dt(isp) + Rz(k)/rCNz(k)*rZOO(isp,k) !!!??????????????????????????????????
      ENDDO
      DO isp=1,N_Psp
        dPO4_dt(isp) = dPO4_dt(isp) + Rz(k)/rCPz(k)*rZOO(isp,k) !!!??????????????????????????????????
      ENDDO
      dTA_dt = dTA_dt + 1.0d0*Rz(k)/rCNz(k) - 2.0d0*Rz(k)/rCPz(k)
#endif
  
!----- Mortality (umolC L-1 s-1) -----------------

      Mz(k) = k_Mz(k) * exp(b_Mz(k)*Tmp) * ZOO(iCt,k)*ZOO(iCt,k)
      Mz(k) = MAX( Mz(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      DO isp=1,N_Csp
        dZOO_dt(isp,k) = dZOO_dt(isp,k) - Mz(k)*rZOO(isp,k)
        dPOC_dt(isp,iLPOM) = dPOC_dt(isp,iLPOM) + Mz(k)*rZOO(isp,k)
      ENDDO
#if defined NUTRIENTS
      DO isp=1,N_Nsp
        dPON_dt(isp,iLPOM) = dPON_dt(isp,iLPOM) + Mz(k)/rCNz(k)*rZOO(isp,k) !!!??????????????????????????????????
      ENDDO
      DO isp=1,N_Psp
        dPOP_dt(isp,iLPOM) = dPOP_dt(isp,iLPOM) + Mz(k)/rCPz(k)*rZOO(isp,k) !!!??????????????????????????????????
      ENDDO
#endif
    END DO

!===== Decomposition ==================================================

!----- Decomposition rate of DOM (umol L-1 s-1) -----------------

    DO k=1, Ndom
      Ddoc(k) = k_Ddoc(k) * exp(b_Ddoc(k)*Tmp) * DOC(iCt,k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddoc(k) = 0.0d0
      ENDIF
      Ddoc(k) = MAX( Ddoc(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      dDOx_dt = dDOx_dt - Ddoc(k)
      DO isp=1,N_Csp
        dDOC_dt(isp,k) = dDOC_dt(isp,k) - Ddoc(k)*rDOC(isp,k)
        dDIC_dt(isp)   = dDIC_dt(isp)   + Ddoc(k)*rDOC(isp,k)
      ENDDO
#if defined NUTRIENTS
      Ddon(k) = k_Ddon(k) * exp(b_Ddon(k)*Tmp) * DON(iNt,k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddon(k) = 0.0d0
      ENDIF
      Ddon(k) = MAX( Ddon(k), 0.0d0 )  !!! Error handling

      Ddop(k) = k_Ddop(k) * exp(b_Ddop(k)*Tmp) * DOP(iNt,k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Ddop(k) = 0.0d0
      ENDIF
      Ddop(k) = MAX( Ddop(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      DO isp=1,N_Nsp
        dDON_dt(isp,k) = dDON_dt(isp,k) - Ddon(k)*rDON(isp,k)
        dNH4_dt(isp)   = dNH4_dt(isp)   + Ddon(k)*rDON(isp,k)
      ENDDO
      DO isp=1,N_Psp
        dDOP_dt(isp,k) = dDOP_dt(isp,k) - Ddop(k)*rDOP(isp,k)
        dPO4_dt(isp)   = dPO4_dt(isp)   + Ddop(k)*rDOP(isp,k)
      ENDDO
      dTA_dt = dTA_dt + 1.0d0*Ddon(k) - 2.0d0*Ddop(k)
#endif
    END DO

!----- Decomposition rate of POM (umol L-1 s-1) -----------------

    DO k=1, Npom
      Dpoc(k) = k_Dpoc(k) * exp(b_Dpoc(k)*Tmp) * POC(iCt,k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpoc(k) = 0.0d0
      ENDIF
      Dpoc(k) = MAX( Dpoc(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      dDOx_dt = dDOx_dt - Dpoc(k)
      DO isp=1,N_Csp
        dPOC_dt(isp,k) = dPOC_dt(isp,k) - Dpoc(k)*rPOC(isp,k)
        dDIC_dt(isp)   = dDIC_dt(isp)   + Dpoc(k)*rPOC(isp,k)
      ENDDO

#if defined NUTRIENTS
      Dpon(k) = k_Dpon(k) * exp(b_Dpon(k)*Tmp) * PON(iNt,k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpon(k) = 0.0d0
      ENDIF
      Dpon(k) = MAX( Dpon(k), 0.0d0 )  !!! Error handling

      Dpop(k) = k_Dpop(k) * exp(b_Dpop(k)*Tmp) * POP(iPt,k)
      IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpop(k) = 0.0d0
      ENDIF
      Dpop(k) = MAX( Dpop(k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

      DO isp=1,N_Nsp
        dPON_dt(isp,k) = dPON_dt(isp,k) - Dpon(k)*rPON(isp,k)
        dNH4_dt(isp)   = dNH4_dt(isp)   + Dpon(k)*rPON(isp,k)
      ENDDO
      DO isp=1,N_Psp
        dPOP_dt(isp,k) = dPOP_dt(isp,k) - Dpop(k)*rPOP(isp,k)
        dPO4_dt(isp)   = dPO4_dt(isp)   + Dpop(k)*rPOP(isp,k)
      ENDDO
      dTA_dt = dTA_dt + 1.0d0*Dpon(k) - 2.0d0*Dpop(k)
#endif
    END DO

!----- Decomposition rate from POM to DOM (umol L-1 s-1) -----------------

    DO k=1, Ndom
      DO m=1, Npom
        Dp2doc(m,k) = k_Dp2doc(m,k) * exp(b_Dp2doc(m,k)*Tmp) * MAX(POC(iCt,m),0.0d0)
        IF(DOx <= 0.0d0) THEN !-----For Error handling
          Dp2doc(m,k) = 0.0d0
        ENDIF
        Dp2doc(m,k) = MAX( Dp2doc(m,k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

        DO isp=1,N_Csp
          dDOC_dt(isp,k) = dDOC_dt(isp,k) + Dp2doc(m,k)*rPOC(isp,m)
          dPOC_dt(isp,m) = dPOC_dt(isp,m) - Dp2doc(m,k)*rPOC(isp,m)
        ENDDO
#if defined NUTRIENTS
        Dp2don(m,k) = k_Dp2don(m,k) * exp(b_Dp2don(m,k)*Tmp) * MAX(PON(iNt,m),0.0d0)
        IF(DOx <= 0.0d0) THEN !-----For Error handling
          Dp2don(m,k) = 0.0d0
        ENDIF
        Dp2don(m,k) = MAX( Dp2don(m,k), 0.0d0 )  !!! Error handling

        Dp2dop(m,k) = k_Dp2dop(m,k) * exp(b_Dp2dop(m,k)*Tmp) * MAX(POP(iPt,m),0.0d0)
        IF(DOx <= 0.0d0) THEN !-----For Error handling
          Dp2dop(m,k) = 0.0d0
        ENDIF
        Dp2dop(m,k) = MAX( Dp2dop(m,k), 0.0d0 )  !!! Error handling

!     === Mass balance ===

        DO isp=1,N_Nsp
          dDON_dt(isp,k) = dDON_dt(isp,k) + Dp2don(m,k)*rPON(isp,m)
          dPON_dt(isp,m) = dPON_dt(isp,m) - Dp2don(m,k)*rPON(isp,m)
        ENDDO
        DO isp=1,N_Psp
          dDOP_dt(isp,k) = dDOP_dt(isp,k) + Dp2dop(m,k)*rPOP(isp,m)
          dPOP_dt(isp,m) = dPOP_dt(isp,m) - Dp2dop(m,k)*rPOP(isp,m)
        ENDDO

#endif
      END DO
    END DO

!----- Decomposition rate from CPOM to Detritus (umol L-1 s-1) -----------------

    Dcpoc2dtr = k_Dcpoc2dtr * exp(b_Dcpoc2dtr*Tmp) * POC(iCt,iCPOM)
    Dcpoc2dtr = MAX( Dcpoc2dtr, 0.0d0 )  !!! Error handling

  !     === Mass balance ===
  
    DO isp=1,N_Csp
      dPOC_dt(isp,iCPOM) = dPOC_dt(isp,iCPOM) - Dcpoc2dtr*rPOC(isp,iCPOM)
      dPOC_dt(isp,iLPOM) = dPOC_dt(isp,iLPOM) + Dcpoc2dtr*rPOC(isp,iCPOM)
    ENDDO
#if defined NUTRIENTS
    Dcpon2dtr = k_Dcpon2dtr * exp(b_Dcpon2dtr*Tmp) * PON(iNt,iCPOM)
    Dcpon2dtr = MAX( Dcpon2dtr, 0.0d0 )  !!! Error handling

    Dcpop2dtr = k_Dcpop2dtr * exp(b_Dcpop2dtr*Tmp) * POP(iPt,iCPOM)
    Dcpop2dtr = MAX( Dcpop2dtr, 0.0d0 )  !!! Error handling
  
  !     === Mass balance ===
  
    DO isp=1,N_Nsp
      dPON_dt(isp,iCPOM) = dPON_dt(isp,iCPOM) - Dcpon2dtr*rPON(isp,iCPOM)
      dPON_dt(isp,iLPOM) = dPON_dt(isp,iLPOM) + Dcpon2dtr*rPON(isp,iCPOM)
    ENDDO
    DO isp=1,N_Psp
      dPOP_dt(isp,iCPOM) = dPOP_dt(isp,iCPOM) - Dcpop2dtr*rPOP(isp,iCPOM)
      dPOP_dt(isp,iLPOM) = dPOP_dt(isp,iLPOM) + Dcpop2dtr*rPOP(isp,iCPOM)
    ENDDO
#endif

  
!===== Nitrification ==================================================== JS_edit find reference

#if defined NUTRIENTS
    Nit = k_Nit * exp(b_Nit*Tmp) * MAX( NH4(iNt), 0.0d0 )**2
    IF(DOx <= 0.0d0) THEN !-----For Error handling
        Nit = 0.0d0
    ENDIF
    Nit = MAX( Nit, 0.0d0 )  !!! Error handling

!     === Mass balance ===

    DO isp=1,N_Nsp
      dNO3_dt(isp) = dNO3_dt(isp) + Nit*rNH4(isp)
      dNH4_dt(isp) = dNH4_dt(isp) - Nit*rNH4(isp)
    ENDDO
    dDOx_dt = dDOx_dt - Nit*2.0d0
    dTA_dt = dTA_dt - Nit*2.0d0
#endif

!===== Carbonate dissolution ==============================================

!!!!!! Need pressure correction (not impremented) !!!!!!
    pH     = pH_fromATCT( TA, DIC(iCt), Tmp+273.15d0, Sal )   ! pH
    CO3   = cCO3_fromCTpH( DIC(iCt), pH, Tmp+273.15d0, Sal )  ! CO32- concentration
    Wcal   = Wcal_fromcCO3( CO3, Tmp+273.15d0, Sal )  ! Calcite saturation state
    Warg   = Warg_fromcCO3( CO3, Tmp+273.15d0, Sal )  ! Calcite saturation state
    IF ( Wcal < 1.0d0 ) THEN !!! CHECK: Keir 1979 !!!!!!
      Dissol = 1.3d7*(1.d0-Wcal)**2.8d0 * PIC(iCt,iCaCO3)  !! [umol L-1 s-1] Low-Mg Calcite dissolution rate (Walter and Morse, 1985)
    ELSE
      Dissol = 0.0d0
    END IF
    Dissol = MAX( Dissol, 0.0d0 )  !!! Error handling

!     === Mass balance ===

    DO isp=1,N_Csp
      dPIC_dt(isp,iCaCO3) = dPIC_dt(isp,iCaCO3) - Dissol*rPIC(isp,iCaCO3)
      dDIC_dt(isp) = dDIC_dt(isp) + Dissol*rPIC(isp,iCaCO3)
    ENDDO
    dTA_dt = dTA_dt + Dissol*2.0d0

!===== Unit adjustment ===========================================

    dDIC_dt(:) = dDIC_dt(:)/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)
    dTA_dt  = dTA_dt/rho_sw !(umol L-1 s-1) -> (umol kg-1 s-1)

      
    RETURN

  END SUBROUTINE foodweb
#endif
END MODULE mod_foodweb


