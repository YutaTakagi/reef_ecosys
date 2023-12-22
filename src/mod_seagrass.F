
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====

#include "cppdefs.h"


! #ifdef SEAGRASS

! !!!************************************************************************
! !!!** This code is old and preserved only for backwards copatibility     **
! !!!** The updated seagrass model uses the CPP flag YT_SEAGRASS           **
! !!!** Replace SEAGRASS with YT_SEAGRASS in the project header file       **
! !!!** to use the newer module                                            **
! !!!** The corresponding code is found starting at Line: 250              **
! !!!************************************************************************

! !!!*************** SEAGRASS ***********************************************


! MODULE mod_seagrass
!   implicit none
!   integer, parameter :: Nsg = 1    !! Number of seagrass groups
!   TYPE T_SGRASS
!     real(8), pointer :: Pg(:,:,:) 
!     real(8), pointer :: R (:,:,:) 
!     real(8), pointer :: QC(:,:,:)
! #if defined CARBON_ISOTOPE
! !  13C isotope
!     real(8), pointer :: Q13C(:,:,:)
! #endif
! #if defined NUTRIENTS         
!     real(8), pointer :: QN(:,:,:)
!     real(8), pointer :: QP(:,:,:)
! #endif
!   END TYPE T_SGRASS

!   TYPE (T_SGRASS), allocatable :: SGRASS(:)

! CONTAINS

! !!! **********************************************************************
! !!!  set initial conditions for coral polyp model (Nakamura et al. 2013)
! !!! **********************************************************************

!   subroutine initialize_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj)

!     USE mod_geochem
    
!     implicit none
! ! input parameters
!     integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj
!     real(8)  R13C
!     integer i,j,n

!     IF (ng.eq.1) allocate ( SGRASS(Ngrids) )

!     allocate( SGRASS(ng)%Pg(Nsg,LBi:UBi,LBj:UBj)     )
!     allocate( SGRASS(ng)%R (Nsg,LBi:UBi,LBj:UBj)     )
!     allocate( SGRASS(ng)%QC(Nsg,LBi:UBi,LBj:UBj)     )
! #if defined CARBON_ISOTOPE
!     allocate( SGRASS(ng)%Q13C(Nsg,LBi:UBi,LBj:UBj)   )
! #endif
! #if defined NUTRIENTS         
!     allocate( SGRASS(ng)%QN(Nsg,LBi:UBi,LBj:UBj)     )
!     allocate( SGRASS(ng)%QP(Nsg,LBi:UBi,LBj:UBj)     )
! #endif

! !------------------------------------------
! !  Set initial conditions
!     do j=LBj,UBj
!       do i=LBi,UBi
!         do n=1,Nsg
! !      seagrass internal conditions
!           SGRASS(ng)%Pg(n,i,j) = 0.0d0
!           SGRASS(ng)%R (n,i,j) = 0.0d0
!           SGRASS(ng)%QC(n,i,j) = 15.0d0  !!!�Ă��Ƃ�
! #if defined CARBON_ISOTOPE
!           R13C = R13C_fromd13C(-15.0d0)
! !        c13CH2O (n,i,j)=R13C/(1.+R13C)*CH2O(n,i,j)
!           SGRASS(ng)%Q13C(n,i,j) = R13C * SGRASS(ng)%QC(n,i,j)
! #endif
! #if defined NUTRIENTS         
!           SGRASS(ng)%QN(n,i,j) = 1.5d0  !!!�Ă��Ƃ�
!           SGRASS(ng)%QP(n,i,j) = 0.1d0  !!!�Ă��Ƃ�
! #endif
!         enddo
!       enddo
!     enddo
    
!     RETURN
    
!   END SUBROUTINE initialize_seagrass

! !!! **********************************************************************
! !!!  Main program of seagrass model (Watanabe et al. 2012)
! !!! **********************************************************************

!   SUBROUTINE seagrass           &
! !   input parameters
!     ( ng, n, i, j    &   ! ng: nested grid number; n: seagrass compartment; i,j: position
!     , PFD            &   ! Photon flux density (umol m-2 s-1)
!     , rho_sw         &   ! Density of seawater (g cm-3)
!     , DICamb         &   ! DIC (umol kg-1)
!     , DOamb          &   ! DO (umol L-1)
! #if defined NUTRIENTS         
!     , NH4amb         &   ! NH4 concentration (umol L-1)
! #endif
! #if defined CARBON_ISOTOPE
!     , DI13Camb       &   ! 13C of DIC (umol kg-1)
! #endif
! !   output parameters
!     , DICuptake      &   ! DIC uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
!     , DOuptake       &   ! DO  uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
! #if defined NUTRIENTS         
!     , NO3uptake      &   ! NO3 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
!     , NH4uptake      &   ! NH4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
!     , PO4uptake      &   ! PO4 uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
! #endif
! #if defined CARBON_ISOTOPE
!     , DI13Cuptake    &   ! DI13C uptake rate (mmol m-2 s-1)  * direction of water column to coral is positive
! #endif
!     )

! !-----------------------------------------------------------------------
!     USE mod_geochem
    
!     implicit none

! ! input parameters
!     integer, intent(in) :: ng, n, i, j    ! ng: nested grid number; n: seagrass compartment; i,j: position
!     real(8), intent(in) :: PFD
!     real(8), intent(in) :: rho_sw   
!     real(8), intent(in) :: DICamb
!     real(8), intent(in) :: DOamb
! #if defined NUTRIENTS         
!     real(8), intent(in) :: NH4amb
! #endif
! #if defined CARBON_ISOTOPE
!     real(8), intent(in) :: DI13Camb
! #endif
! ! output parameters
!     real(8), intent(out) :: DICuptake
!     real(8), intent(out) :: DOuptake
! #if defined NUTRIENTS         
!     real(8), intent(out) :: NO3uptake
!     real(8), intent(out) :: NH4uptake
!     real(8), intent(out) :: PO4uptake
! #endif
! #if defined CARBON_ISOTOPE
!     real(8), intent(out) :: DI13Cuptake
! #endif

! ! --- C:N:P ratio of seagrass ---
!     real(8), parameter :: nc=27./599.d0 !M.J.Atkinson and SV Smith(1983)
!     real(8), parameter :: pc=1./599.d0
! ! --- Photosynthesis  Parameters ---
! !    real(8), parameter :: pmax =  51.3d0 ! Watanabe et al. 2013
! !    real(8), parameter :: pIk  = 589.65d0
! !    real(8), parameter :: p0   =  15.05d0
!     real(8), parameter :: pmax =  55.81d0  ! Nakamura & Nakamori 2009
!     real(8), parameter :: pIk  = 671.8d0   !  Model skill = 0.990
!     real(8), parameter :: p0   =  21.62d0  !
! #if defined NUTRIENTS         
!     real(8) npref
!     real(8) ldocn,ldocd
! #endif
! #if defined CARBON_ISOTOPE
!     real(8), parameter :: a_phot  = -7.d-3+1.d0  ! -5 to -10 per mill (Dr. Miyajima, pers. comn.)
!     real(8), parameter :: a_resp  =  0.d-3+1.d0  !
!     real(8) R13C_DIC, R13C_QC
! #endif

! ! --- Organic and Inorganic Production Rate -----------------

!     SGRASS(ng)%Pg(n,i,j)= pmax*tanh(PFD/pIk)/3600.d0   !Light response curve [mmolC/m2/s]
!     SGRASS(ng)%R (n,i,j)= p0/3600.d0   !Constant [mmolC/m2/s]
    
!     IF(DICamb<=0.d0) THEN !-----For Error handling
!       SGRASS(ng)%Pg(n,i,j) = 0.d0
!     ENDIF
!     IF(DOamb<=0.d0) THEN !-----For Error handling
!       SGRASS(ng)%R (n,i,j) = 0.d0
!     ENDIF
    
!     DICuptake= SGRASS(ng)%Pg(n,i,j)-SGRASS(ng)%R (n,i,j)
!     DOuptake = SGRASS(ng)%R (n,i,j)-SGRASS(ng)%Pg(n,i,j)

! !!! ----- Isotope calculation ----------------------------------------------------
! #if defined CARBON_ISOTOPE
!     R13C_DIC  = DI13Camb/DICamb
!     R13C_QC = SGRASS(ng)%Q13C(n,i,j) / SGRASS(ng)%QC(n,i,j)
    
!     IF(DI13Camb<=0.d0) THEN !-----For Error handling
!       R13C_DIC =0.d0
!     ENDIF

!     DI13Cuptake=SGRASS(ng)%Pg(n,i,j)*R13C_DIC*a_phot        &
!                 - SGRASS(ng)%R (n,i,j)*R13C_QC*a_resp
! #endif
      
! ! --- Nutrient fluxes between water column and coral -----
! #if defined NUTRIENTS         
! !!! Under developing
!     NO3uptake = 0.0d0
!     NH4uptake = 0.0d0
!     PO4uptake = 0.0d0
! #endif
!     RETURN
    
!   END SUBROUTINE seagrass

! END MODULE mod_seagrass

! #endif



































#ifdef YT_SEAGRASS

!!! ==================================================================================================
!!! **************************************************************************************************
!!! **                                                                                              **
!!! **                                       YT_SEAGRASS MODEL                                      **
!!! **                                                                                              **
!!! ** -------------------------------------------------------------------------------------------- **
!!! **                                                                                              **
!!! **    This is the updated seagrass module devloped by Yuta A. Takagi (2023)                     **
!!! **    It replaces the older SEAGRASS model                                                      **
!!! **    Use CPP flag YT_SEAGRASS in your header file to enable                                    **
!!! **    Disable CPP flag SEAGRASS (it is a full replacement, not an add-on)                       **
!!! **                                                                                              **
!!! **************************************************************************************************
!!! ==================================================================================================

MODULE mod_seagrass

!!! ==================================================================================================
!!! **************************************************************************************************
!!!                          CREATE YT_SEAGRASS MODEL PRESERVED VARIABLES
!!! **************************************************************************************************
!!! ==================================================================================================

  implicit none
  integer, parameter :: Nsg = 1   ! yt_edit fix this later???

  TYPE T_SGRASS ! create a class for all seagrass values (for a given nested grid)
    !                                                         (c s i j l)                               the columns correspond to indexes for: c = chemical species; s = species_id_number; i = i-position; j = j-position; l = sediment layer
! >>> Housekeeping Vars <<< !
    integer, pointer :: Day                                     (  :,:)   ! date                        Simulation whole day (integer round down)
! >>> Saved Constants <<< !
    ! model params
    real(8), pointer :: GridArea                                (  :,:)   ! [m2.ground]                 Total ground area in grid cell
    ! biophysical params
    real(8), pointer :: C_N_P_ratios                          (:,:    )   !                             C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
    real(8), pointer :: SgNperC                                 (:    )   ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    real(8), pointer :: SgPperC                                 (:    )   ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    real(8), pointer :: SgMperC                                 (:    )   ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    real(8), pointer :: f_above                                 (:    )   ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction
    real(8), pointer :: rho_lf                                  (:    )   ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    real(8), pointer :: k_proj                                  (:    )   ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: root_mass_profile                       (:,    :) ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
    ! photoynthesis: light-limit
    real(8), pointer :: P_max                                   (:    )   ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate  
    real(8), pointer :: I_k                                     (:    )   ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    real(8), pointer :: Lf_DIC_uptake_max                       (:    )   ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    real(8), pointer :: k_lf_DIC                                (:    )   ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: Rt_DIC_flux_max                         (:    )   ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    real(8), pointer :: k_rt_DIC                                (:    )   ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8), pointer :: Lf_PO4_uptake_max                       (:    )   ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    real(8), pointer :: k_lf_PO4                                (:    )   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: Rt_PO4_flux_max                         (:    )   ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    real(8), pointer :: k_rt_PO4                                (:    )   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    real(8), pointer :: Lf_DO_uptake_max                        (:    )   ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    real(8), pointer :: k_lf_DO                                 (:    )   ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
    ! respiration: required
    real(8), pointer :: R_basal                                 (:    )   ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    ! dieoff when respiration is insufficient
    real(8), pointer :: max_dieoff_rate                         (:    )   ! [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    real(8), pointer :: trans_rate                              (:    )   ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    real(8), pointer :: opt_sg_bed_density                      (:    )   ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Saved Values <<< !
    ! Update at end of main interval
    real(8), pointer :: SgCBm                                   (:,:,:)   ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! Update at end of growth interval
    ! update total carbon-biomass
    real(8), pointer :: TotSgCBm                                (:,:,:)   ! [mol.C.tot]                 Total seagrass carbon-biomass in grid
    ! allocate growth
    real(8), pointer :: TotLfCBm                                (:,:,:)   ! [mol.C.lf]                  Total seagrass above ground carbon-biomass in grid
    real(8), pointer :: TotRtCBm                                (:,:,:)   ! [mol.C.rt]                  Total seagrass below ground carbon-biomass in grid
    ! update bed-density/habitat area
    real(8), pointer :: HabArea                                 (:,:,:)   ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    real(8), pointer :: LfCBm                                   (:,:,:)   ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    real(8), pointer :: RtCBm                                   (:,:,:)   ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    real(8), pointer :: TotSgDryBm                              (:,:,:)   ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    real(8), pointer :: TotLfDryBm                              (:,:,:)   ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    real(8), pointer :: TotRtDryBm                              (:,:,:)   ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    real(8), pointer :: SgDryBm                                 (:,:,:)   ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer :: LfDryBm                                 (:,:,:)   ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer :: RtDryBm                                 (:,:,:)   ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    real(8), pointer :: LAI                                     (:,:,:)   ! [m2.lf m-2.sg.hab]          Leaf area index: one-sided green leaf area per unit ground surface area 

  END TYPE T_SGRASS

  TYPE (T_SGRASS), allocatable :: SGRASS(:) ! create an array of T_SGRASS for all nested grids
  
  CONTAINS
  

!!! ==================================================================================================
!!! **************************************************************************************************
!!!                             INITIALIZATION OF YT_SEAGRASS MODEL
!!! **************************************************************************************************
!!! ==================================================================================================
  
! ====================================================================================================
! Seagrass initialization subroutine in/out interface
! ====================================================================================================
  subroutine initialize_seagrass(ng, Ngrids, LBi, UBi, LBj, UBj, dXI, dETA, Nspecies, p_sgrass)

! ====================================================================================================
! Module dependencies
! ====================================================================================================
    USE mod_geochem
# if defined YT_SEDIMENT_ECOSYS
    USE mod_sedecosys,    ONLY : Nsed
# endif

! ====================================================================================================
! Variable declarations
! ====================================================================================================
    implicit none
! input parameters
    integer, intent(in) :: ng                     !                         ng is the nested grid number
    integer, intent(in) :: Ngrids                 !                         Ngrids is the total number of nested grids
    integer, intent(in) :: LBi, UBi, LBj, UBj     !                         lower and upper bounds i & j
    real(8), intent(in) :: dXI (LBi:UBi,LBj:UBj)  ! [m]                     grid cell widths in XI direction
    real(8), intent(in) :: dETA(LBi:UBi,LBj:UBj)  ! [m]                     grid cell widths in ETA direction
    integer, intent(in) :: Nspecies               !                         Number of seagrass species
    real(8), intent(in) :: p_sgrass(Nspecies)     ! [m2.sg.hab m-2.ground]  Seagrass coverage
! indexing variables
    integer species,i,j
  
! ====================================================================================================
! Variable memory allocaation
! ====================================================================================================
! if initializaing the first nested grid, allocate the SGRASS array
    IF (ng.eq.1) THEN
      allocate ( SGRASS(Ngrids) ) ! array of T_SGRASS which holds all the values for each nested grid
    ENDIF
! >>> Housekeeping Vars <<< !
    allocate( SGRASS(ng)%Day                                                 (LBi:UBi,LBj:UBj)  ) !                             Simulation whole day (integer round down)
! >>> Saved Constants <<< !
    ! model params
    allocate( SGRASS(ng)%GridArea                                            (LBi:UBi,LBj:UBj)  ) ! [m2.ground]                 Total ground area in grid cell
    ! biophysical params
    allocate( SGRASS(ng)%C_N_P_ratios                            (3, Nspecies)                  ) !                             C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]
    allocate( SGRASS(ng)%SgNperC                                    (Nspecies)                  ) ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    allocate( SGRASS(ng)%SgPperC                                    (Nspecies)                  ) ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    allocate( SGRASS(ng)%SgMperC                                    (Nspecies)                  ) ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    allocate( SGRASS(ng)%f_above                                    (Nspecies)                  ) ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction
    allocate( SGRASS(ng)%rho_lf                                     (Nspecies)                  ) ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    allocate( SGRASS(ng)%k_proj                                     (Nspecies)                  ) ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient  
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%root_mass_profile                          (Nspecies, Nsed)            ) ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif    ! photoynthesis: light-limit
    allocate( SGRASS(ng)%P_max                                      (Nspecies)                  ) ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate  
    allocate( SGRASS(ng)%I_k                                        (Nspecies)                  ) ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    allocate( SGRASS(ng)%Lf_DIC_uptake_max                          (Nspecies)                  ) ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    allocate( SGRASS(ng)%k_lf_DIC                                   (Nspecies)                  ) ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    allocate( SGRASS(ng)%Rt_DIC_flux_max                            (Nspecies)                  ) ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    allocate( SGRASS(ng)%k_rt_DIC                                   (Nspecies)                  ) ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%Lf_PO4_uptake_max                          (Nspecies)                  ) ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    allocate( SGRASS(ng)%k_lf_PO4                                   (Nspecies)                  ) ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    allocate( SGRASS(ng)%Rt_PO4_flux_max                            (Nspecies)                  ) ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    allocate( SGRASS(ng)%k_rt_PO4                                   (Nspecies)                  ) ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    allocate( SGRASS(ng)%Lf_DO_uptake_max                           (Nspecies)                  ) ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    allocate( SGRASS(ng)%k_lf_DO                                    (Nspecies)                  ) ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
    ! respiration: required
    allocate( SGRASS(ng)%R_basal                                    (Nspecies)                  ) ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    ! dieoff when respiration is insufficient
    allocate( SGRASS(ng)%max_dieoff_rate                            (Nspecies)                  ) ! [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Growth interval
    allocate( SGRASS(ng)%trans_rate                                 (Nspecies)                  ) ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    allocate( SGRASS(ng)%opt_sg_bed_density                         (Nspecies)                  ) ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)
! >>> Saved Values <<< !
    ! Update at end of main interval
    allocate( SGRASS(ng)%SgCBm                                      (Nspecies,LBi:UBi,LBj:UBj)  ) ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! Update at end of growth interval
    ! update total carbon-biomass
    allocate( SGRASS(ng)%TotSgCBm                                   (Nspecies,LBi:UBi,LBj:UBj)  ) ! [mol.C.tot]                 Total seagrass carbon-biomass in grid
    ! allocate growth
    allocate( SGRASS(ng)%TotLfCBm                                   (Nspecies,LBi:UBi,LBj:UBj)  ) ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid
    allocate( SGRASS(ng)%TotRtCBm                                   (Nspecies,LBi:UBi,LBj:UBj)  ) ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid
    ! update bed-density/habitat area
    allocate( SGRASS(ng)%HabArea                                    (Nspecies,LBi:UBi,LBj:UBj)  ) ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell such that seagrass coverage (p_sgrass) times the grid area equals the habitat area
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    allocate( SGRASS(ng)%LfCBm                                      (Nspecies,LBi:UBi,LBj:UBj)  ) ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%RtCBm                                      (Nspecies,LBi:UBi,LBj:UBj)  ) ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    allocate( SGRASS(ng)%TotSgDryBm                                 (Nspecies,LBi:UBi,LBj:UBj)  ) ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
		allocate( SGRASS(ng)%TotLfDryBm                                 (Nspecies,LBi:UBi,LBj:UBj)  ) ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    allocate( SGRASS(ng)%TotRtDryBm                                 (Nspecies,LBi:UBi,LBj:UBj)  ) ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    allocate( SGRASS(ng)%SgDryBm                                    (Nspecies,LBi:UBi,LBj:UBj)  ) ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%LfDryBm                                    (Nspecies,LBi:UBi,LBj:UBj)  ) ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    allocate( SGRASS(ng)%RtDryBm                                    (Nspecies,LBi:UBi,LBj:UBj)  ) ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    allocate( SGRASS(ng)%LAI                                        (Nspecies,LBi:UBi,LBj:UBj)  ) ! [m2.lf m-2.sg.hab]          Leaf area index: one-sided green leaf area per unit ground surface area

! ====================================================================================================
! Variable initializations
! ====================================================================================================
! >>> Saved Constants <<< !
! Set constant values  yt_edit make this user definable!!!
    ! biophysical params
    SGRASS(ng)%C_N_P_ratios                         = reshape([ 545d0, 35d0, 1d0 ],[3, Nspecies])     ! C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]     ! Martins and Bandeira (2001) measured C:N:P = 545:35:1 for T hemprichii
    SGRASS(ng)%f_above                              = [ 0.21d0   ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! 0.21 Mallombasi et al. (2020)
    ! SGRASS(ng)%f_above                            = [ 1.0d0    ]                                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction                              ! yt_debug testing
    SGRASS(ng)%rho_lf                               = [ 0.069d0  ]                                    ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density                                                                  ! 0.069 kg/m2 Calculated from Zulfikar et al. (2020) 
    SGRASS(ng)%k_proj                               = [ 0.8d0    ]                                    ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient                                                                  ! 0.8 is a guess
    ! SGRASS(ng)%k_proj                             = [ 1.0d0    ]                                    ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient                                                                
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SGRASS(ng)%root_mass_profile                    = reshape([0.2d0, 0.2d0, 0.1d0, 0.1d0, 0.1d0, 0.1d0, 0.1d0, 0.1d0, 0d0, 0d0 &
                                                               , 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0               &
                                                               , 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0               &
                                                               , 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0               &
                                                               , 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0               &
                                                               ],[Nsed, Nspecies])                    ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif

    ! photoynthesis: light-limit
    ! SGRASS(ng)%P_max                              = [ 3.45d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! 50 µmol/kg/s Calculated from net photosynthesis data from Bjo ̈rk et al. (1997) or 3.45 µmol.C m-2.leaf s-1 (see master's thesis section 6.3.2c)
    SGRASS(ng)%P_max                                = [ 73.8d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! tuning: when R_basal = 2.5265d0 (half-life in complete darkness is ~7.0 days), biomass reaches equilibrium at 2 kg.drywt/m2.hab under max-photosynthesis.
    ! SGRASS(ng)%P_max                              = [ 34.5d0   ]                                    ! [umol.C m-2.leaf s-1]       Species specific maximum photoynthesis rate                                                                   ! yt_debug testing
    SGRASS(ng)%I_k                                  = [ 180d0    ]                                    ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve                    ! Calculated from rETR data from Liu et al. (2005), rETR to ETR conversion using Aquation (2020), ETR to carbon assimilation rate using Miyazawa and Yahata (2006), and a leaf bend angle of 10◦ from James et al. (2021) (refer to master's thesi Section 6.3.2b)

    ! photoynthesis: DIC-limit
    SGRASS(ng)%Lf_DIC_uptake_max                    = [ 73.8d0   ]                                    ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves                                                                      ! tuning set equal to P_max
    ! SGRASS(ng)%k_lf_DIC                             = [ 4500d0   ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth                                             ! 4500 µmol L-1 from Bjo ̈rk et al. (1997) (master's thesis section 6.3.2b)
    SGRASS(ng)%k_lf_DIC                             = [ 3000.0d0 ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth                                               ! yt_edit Tuning
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    SGRASS(ng)%Rt_DIC_flux_max                      = [ 100.0d0  ]                                    ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots                                                                       ! yt_edit Tuning
    SGRASS(ng)%k_rt_DIC                             = [ 3000.0d0 ]                                    ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth                                            ! yt_edit Tuning
# endif

    ! photoynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    SGRASS(ng)%Lf_PO4_uptake_max                    = [ 200.0d0/545.0d0  ]                            ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves                                                                      ! yt_edit tuning  set equal to max photosynthesis
    SGRASS(ng)%k_lf_PO4                             = [ 11.2d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth                                               ! Stapel et al. (1996) (master's thesis Section 6.3.5b)
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    SGRASS(ng)%Rt_PO4_flux_max                      = [ 200.0d0/545.0d0  ]                            ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots                                                                       ! yt_edit tuning, same as leaves
    SGRASS(ng)%k_rt_PO4                             = [ 11.2d0   ]                                    ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth                                            ! yt_edit tuning, same as leaves
# endif
    
    ! respiration: DO-limit
    ! SGRASS(ng)%Lf_DO_uptake_max                     = [ 2.075d0  ]                                  ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves                                                                       ! 2.075 umol.C/m2.leaf/s calculated from Rasmusson et al. (2020) (refer to master's thesis 6.3.3a)
    SGRASS(ng)%Lf_DO_uptake_max                     = [ 40d0     ]                                    ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves                                                                       ! tuning
    SGRASS(ng)%k_lf_DO                              = [ 43d0     ]                                    ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration                                           ! 43 umol.O2/L Loosely calculated from Rasmusson et al. (2020) (Section 6.3.3b)
    ! respiration: required
    SGRASS(ng)%R_basal                              = [ 2.5265d0 ]                                    ! [umol.C m-2.leaf s-1]       Maximum respiration rate                                                                                      ! tuning: half-life in complete darkness is ~7.0 days.
    ! dieoff when respiration is insufficient
    SGRASS(ng)%max_dieoff_rate                      = [ 0.5d0    ]                                    ! [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates ! tuning

    ! Growth interval
    SGRASS(ng)%trans_rate                           = [ 0.05d0   ]                                    ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day                                         ! yt_edit tuning: translocation 10% per day
    SGRASS(ng)%opt_sg_bed_density                   = [ 500.0d0  ]                                    ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)                                         ! yt_edit tuning


! Set grid-wise initial conditions
    do i=LBi,UBi
      do j=LBj,UBj
        ! grid areas
        SGRASS(ng)%GridArea(i,j) = dXI(i,j)*dETA(i,j)
        SGRASS(ng)%Day(i,j) = -1  ! start at negative so first day triggers calculation
      enddo
    enddo

! Set species specific initial conditions
    do species=1,Nspecies

      ! SgNperC                     [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
      ! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
      SGRASS(ng)%SgNperC = SGRASS(ng)%C_N_P_ratios(2, species) / SGRASS(ng)%C_N_P_ratios(1, species)

      ! SgPperC                     [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
      ! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
      SGRASS(ng)%SgPperC = SGRASS(ng)%C_N_P_ratios(3, species) / SGRASS(ng)%C_N_P_ratios(1, species)

      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! C_N_P_ratios(chem,sp)                                   C-N-P ratios: (CH2O)x(NH3)y(H3PO4)z  groups of three per species in list: order is [ sp1-C, sp1-N, sp1-P, sp2-C, sp2-N, sp2-P, etc. ]    
      ! (CH2O)x(NH3)y(H3PO4)z / x
      SGRASS(ng)%SgMperC(species) = (                                                                                             &
        ! H = (1.0078) (x*2 + y*3 +z*3)            
        1.0078d0*(2*SGRASS(ng)%C_N_P_ratios(1,species)+3*SGRASS(ng)%C_N_P_ratios(2,species)+3*SGRASS(ng)%C_N_P_ratios(3,species))+&
        ! C = (12.011) (x)
        12.011d0*(  SGRASS(ng)%C_N_P_ratios(1,species)                                                                          )+&
        ! N = (14.007) (y)
        14.007d0*(                                       SGRASS(ng)%C_N_P_ratios(2,species)                                     )+&
        ! O = (15.999) (x + z*4)
        15.999d0*(  SGRASS(ng)%C_N_P_ratios(1,species)                                     +4*SGRASS(ng)%C_N_P_ratios(3,species))+&
        ! P = (30.974) (z)
        30.974d0*(                                                                            SGRASS(ng)%C_N_P_ratios(3,species)) &
        ! (sum of HCNOP) / x
        ) / SGRASS(ng)%C_N_P_ratios(1,species)

    enddo

! analytical start
    do species=1,Nspecies
      do i=LBi,UBi
        do j=LBj,UBj
          ! when initializaing from start in a new run when only coverage is known, calculate biomass values assuming an ELAP of 90% in the coverage area and an ideal ratio of above to belowground biomass

          ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
          ! GridArea                    [m2.ground]                 Total ground area in grid cell
          ! p_sgrass                    [m2.sg.hab m-2.ground]      Seagrass coverage
          ! LAI                         [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
          ! ELAP                        [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
          ! k_proj                      [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
          ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
          ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
          ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
          ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
          ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
          ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
          ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
          ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
          ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
          ! TotSgDryBm                  [g.drywt]                   Total seagrass dry weight biomass in grid cell
          ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
          ! TotRtDryBm                  [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
          ! SgDryBm                     [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
          ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
          ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)

          ! [m2.sg.hab]                   = [m2.ground]              [m2.sg.hab m-2.ground]
          SGRASS(ng)%HabArea(species,i,j) = SGRASS(ng)%GridArea(i,j) * p_sgrass(species)
          
          SGRASS(ng)%LAI(species,i,j) = (-1*log(1.0-0.85)/SGRASS(ng)%k_proj(species))

          ! [umol.C/m2.hab]              
          SGRASS(ng)%LfCBm(species,i,j)                                                                                           &
          ! = [m2.lf/m2.hab]              [kg.drywt/m2.lf]             [mol.C/g.drywt]                 (1000.g.drywt/kg.drywt) (1000000umol/mol)
            = SGRASS(ng)%LAI(species,i,j) * SGRASS(ng)%rho_lf(species) / SGRASS(ng)%SgMperC(species) * 1000000000d0

          ! [umol.C.tot/m2.hab]         = [umol.C.lf/m2.hab]            [mol.C.tot/mol.C.lf]
          SGRASS(ng)%SgCBm(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) / SGRASS(ng)%f_above(species)

          ! [umol.C.rt/m2.hab]          = [umol.C.tot/m2.hab]           [mol.C.rt/mol.C.tot]
          SGRASS(ng)%RtCBm(species,i,j) = SGRASS(ng)%SgCBm(species,i,j) * (1.0d0 - SGRASS(ng)%f_above(species))

          ! [mol.C]                        = [umol.C/m2.hab]               [m2.hab]                          (mol/1000000umol)
          SGRASS(ng)%TotSgCBm(species,i,j) = SGRASS(ng)%SgCBm(species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0

          ! [mol.C]                        = [umol.C/m2.hab]               [m2.hab]                          (mol/1000000umol)
          SGRASS(ng)%TotLfCBm(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0

          ! [mol.C]                        = [umol.C/m2.hab]               [m2.hab]                          (mol/1000000umol)
          SGRASS(ng)%TotRtCBm(species,i,j) = SGRASS(ng)%RtCBm(species,i,j) * SGRASS(ng)%HabArea(species,i,j) * 0.000001d0
          
          ! [g.drywt]                        = [mol.C]                          [g.drywt/mol.C]
          SGRASS(ng)%TotSgDryBm(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j) * SGRASS(ng)%SgMperC(species)

          ! [g.drywt]                        = [mol.C]                          [g.drywt/mol.C]
          SGRASS(ng)%TotLfDryBm(species,i,j) = SGRASS(ng)%TotLfCBm(species,i,j) * SGRASS(ng)%SgMperC(species)

          ! [g.drywt]                        = [mol.C]                          [g.drywt/mol.C]
          SGRASS(ng)%TotRtDryBm(species,i,j) = SGRASS(ng)%TotRtCBm(species,i,j) * SGRASS(ng)%SgMperC(species)

          ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
          SGRASS(ng)%SgDryBm(species,i,j) = SGRASS(ng)%SgCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

          ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
          SGRASS(ng)%LfDryBm(species,i,j) = SGRASS(ng)%LfCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0

          ! [g.drywt/m2.hab]              = [umol.C/m2.hab]               [g.drywt/mol.C]               (mol.C/1000000umol.C)
          SGRASS(ng)%RtDryBm(species,i,j) = SGRASS(ng)%RtCBm(species,i,j) * SGRASS(ng)%SgMperC(species) * 0.000001d0


          ! ! yt_edit testing
          ! SGRASS(ng)%TotLfCBm(species,i,j) = 1d0
          ! SGRASS(ng)%TotRtCBm(species,i,j) = 1d0
          ! SGRASS(ng)%TotSgCBm(species,i,j) = SGRASS(ng)%TotLfCBm(species,i,j)+SGRASS(ng)%TotRtCBm(species,i,j)
          ! SGRASS(ng)%LfCBm(species,i,j) = SGRASS(ng)%TotLfCBm(species,i,j) / SGRASS(ng)%HabArea(species,i,j) * 1000000d0
          ! SGRASS(ng)%RtCBm(species,i,j) = SGRASS(ng)%TotRtCBm(species,i,j) / SGRASS(ng)%HabArea(species,i,j) * 1000000d0
          ! SGRASS(ng)%SgCBm(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j) / SGRASS(ng)%HabArea(species,i,j) * 1000000d0
          ! SGRASS(ng)%TotLfDryBm(species,i,j) = SGRASS(ng)%TotLfCBm(species,i,j) * SGRASS(ng)%SgMperC(species)
          ! SGRASS(ng)%TotRtDryBm(species,i,j) = SGRASS(ng)%TotRtCBm(species,i,j) * SGRASS(ng)%SgMperC(species)
          ! SGRASS(ng)%TotSgDryBm(species,i,j) = SGRASS(ng)%TotSgCBm(species,i,j) * SGRASS(ng)%SgMperC(species)
          ! SGRASS(ng)%LAI(species,i,j) = SGRASS(ng)%TotLfCBm(species,i,j) * SGRASS(ng)%SgMperC(species) / &
          !   SGRASS(ng)%rho_lf(Nspecies) / SGRASS(ng)%HabArea(species,i,j)  * 0.001d0

        enddo
      enddo
    enddo
    
    RETURN
    
  END SUBROUTINE initialize_seagrass



!!! ==================================================================================================
!!! **************************************************************************************************
!!!                                MAIN LOOP OF YT_SEAGRASS MODEL
!!! **************************************************************************************************
!!! ==================================================================================================

! ====================================================================================================
! Seagrass main subroutine in/out interface
! ====================================================================================================
  SUBROUTINE seagrass             &
!   input parameters
    ( ng                          &   !                           nested grid number; 
    , species, Nspecies           &   !                           seagrass species id; total number of species
    , i, j                        &   !                           i,j: position
    , dz                          &   ! [m]                       vertical layer thickness
    , dt                          &   ! [s]                       time step
    , p_sgrass                    &   ! [m2.sg.hab m-2.ground]    Seagrass coverage (every species)
    , PFD                         &   ! [umol m-2 s-1]            Photon flux density 
    , rho_sw                      &   ! [g cm-3]                  Density of seawater 
    , ocean_DIC_conc_in           &   ! [umol.DIC kg-1]           DIC concentration of ocean water (input only, do not use in calculations)
    , ocean_DO__conc_in           &   ! [umol.DO  L-1]            DO  concentration of ocean water (input only, do not use in calculations)
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    , ocean_NO3_conc_in           &   ! [umol.NO3 L-1]            NO3 concentration of ocean water (input only, do not use in calculations)
    , ocean_NH4_conc_in           &   ! [umol.NH4 L-1]            NH4 concentration of ocean water (input only, do not use in calculations)
    , ocean_PO4_conc_in           &   ! [umol.PO4 L-1]            PO4 concentration of ocean water (input only, do not use in calculations)
# endif
!   output parameters
    , DIC_uptake_rate_from_ocean  &   ! [mmol.DIC m-2.sg.hab s-1] DIC uptake rate  * direction of ocean to seagrass is positive
    , DO__uptake_rate_from_ocean  &   ! [mmol.DO  m-2.sg.hab s-1] DO  uptake rate  * direction of ocean to seagrass is positive
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    , NO3_uptake_rate_from_ocean  &   ! [mmol.NO3 m-2.sg.hab s-1] NO3 uptake rate  * direction of ocean to seagrass is positive
    , NH4_uptake_rate_from_ocean  &   ! [mmol.NH4 m-2.sg.hab s-1] NH4 uptake rate  * direction of ocean to seagrass is positive
    , PO4_uptake_rate_from_ocean  &   ! [mmol.PO4 m-2.sg.hab s-1] PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
    )

! ====================================================================================================
! Module dependencies
! ====================================================================================================
    USE mod_geochem
    USE mod_param,        ONLY : time
    ! USE dateclock_mod  ! yt_edit look into using this module
# if defined YT_SEDIMENT_ECOSYS
    USE mod_sedecosys,    ONLY : Nsed, SEDECO
# endif

! ====================================================================================================
! Variable declarations
! ====================================================================================================
    implicit none
! >>> Inputs <<< !
    integer, intent(in)   :: ng                         !                             nested grid number
    integer, intent(in)   :: species, Nspecies          !                             seagrass species id; total number of species
    integer, intent(in)   :: i, j                       !                             i,j: position
    real(8), intent(in)   :: dz                         ! [m]                         vertical layer thickness
    real(8), intent(in)   :: dt                         ! [s]                         Time step interval duration
    real(8), intent(inout):: p_sgrass(Nspecies)         ! []                          Seagrass coverage (every species)
    real(8), intent(in)   :: PFD                        ! [umol m-2 s-1]              Photon flux density 
    real(8), intent(in)   :: rho_sw                     ! [g cm-3]                    Density of seawater 
    real(8), intent(in)   :: ocean_DIC_conc_in          ! [umol.DIC kg-1]             DIC concentration of ocean water (input only, do not use in calculations)
    real(8), intent(in)   :: ocean_DO__conc_in          ! [umol.DO L-1]               DO  concentration of ocean water (input only, do not use in calculations)
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8), intent(in)   :: ocean_NO3_conc_in          ! [umol.NO3 L-1]              NO3 concentration of ocean water (input only, do not use in calculations)
    real(8), intent(in)   :: ocean_NH4_conc_in          ! [umol.NH4 L-1]              NH4 concentration of ocean water (input only, do not use in calculations)
    real(8), intent(in)   :: ocean_PO4_conc_in          ! [umol.PO4 L-1]              PO4 concentration of ocean water (input only, do not use in calculations)
# endif
! >>> Error-Corrected Inputs <<< !      rewrite input variables to new variables for edgecase handling and safety
    real(8)               :: ocean_DIC_conc             ! [umol.DIC L-1]              DIC concentration of ocean water
    real(8)               :: ocean_DO__conc             ! [umol.DO  L-1]              DO  concentration of ocean water
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8)               :: ocean_NO3_conc             ! [umol.NO3 L-1]              NO3 concentration of ocean water
    real(8)               :: ocean_NH4_conc             ! [umol.NH4 L-1]              NH4 concentration of ocean water
    real(8)               :: ocean_PO4_conc             ! [umol.PO4 L-1]              PO4 concentration of ocean water
# endif
! >>> Outputs <<< !
    real(8), intent(out)  :: DIC_uptake_rate_from_ocean ! [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: DO__uptake_rate_from_ocean ! [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE         
    real(8), intent(out)  :: NO3_uptake_rate_from_ocean ! [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: NH4_uptake_rate_from_ocean ! [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    real(8), intent(out)  :: PO4_uptake_rate_from_ocean ! [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif
! >>> Housekeeping Vars <<< !
    integer, pointer      :: day                        !                             Simulation whole day (integer round down)
    logical               :: is_growth_interval         !                             Boolean flag for if the current interval is a growth interval
! >>> Saved Constants <<< !
    ! model params
    real(8)               :: GridArea                   ! [m2.ground]                 Area of grid cell
    ! biophysical params
    real(8)               :: SgNperC                    ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    real(8)               :: SgPperC                    ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    real(8)               :: SgMperC                    ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    real(8)               :: f_above                    ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
    real(8)               :: rho_lf                     ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    real(8)               :: k_proj                     ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: root_mass_profile(Nsed)    ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
    ! photosynthesis: light-limit
    real(8)               :: P_max                      ! [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    real(8)               :: I_k                        ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photosynthesis: DIC-limit
    real(8)               :: Lf_DIC_uptake_max          ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    real(8)               :: k_lf_DIC                   ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: Rt_DIC_flux_max            ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    real(8)               :: k_rt_DIC                   ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photosynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8)               :: Lf_PO4_uptake_max          ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    real(8)               :: k_lf_PO4                   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: Rt_PO4_flux_max            ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    real(8)               :: k_rt_PO4                   ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    real(8)               :: Lf_DO_uptake_max           ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    real(8)               :: k_lf_DO                    ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
    ! respiration: required
    real(8)               :: R_basal                    ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    ! dieoff when respiration is insufficient
    real(8)               :: max_dieoff_rate            ! [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    real(8)               :: trans_rate                 ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    real(8)               :: opt_sg_bed_density         ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Values Read From Sediment Module <<< !
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! real(8)               :: Nsed                     !                             Number of sediment layers
    real(8)               :: sed_layer_thickness(Nsed)  ! [mm]                        Thickness of each sediment layer
# endif
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: sed_DIC(Nsed)              ! [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    real(8)               :: sed_DO (Nsed)              ! [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: sed_NO3(Nsed)              ! [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    real(8)               :: sed_NH4(Nsed)              ! [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    real(8)               :: sed_PO4(Nsed)              ! [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
# endif

! >>> Saved Values <<< !
    ! Update at end of main interval
    real(8), pointer      :: SgCBm                      ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! Update at end of growth interval
    ! update total carbon-biomass
    real(8), pointer      :: TotSgCBm                   ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! allocate growth
    real(8), pointer      :: TotLfCBm                   ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    real(8), pointer      :: TotRtCBm                   ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! update bed-density/habitat area
    real(8), pointer      :: HabArea                    ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    real(8), pointer      :: LfCBm                      ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    real(8), pointer      :: RtCBm                      ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    real(8), pointer      :: TotSgDryBm                 ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    real(8), pointer      :: TotLfDryBm                 ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    real(8), pointer      :: TotRtDryBm                 ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    real(8), pointer      :: SgDryBm                    ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer      :: LfDryBm                    ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    real(8), pointer      :: RtDryBm                    ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    real(8), pointer      :: LAI                        ! [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area

! >>> Delta Values <<< !      amount to add to stored variables at end of the calculation time step to update values: positive adds, negative subtracts (erased each time step)
    real(8)               :: delta_SgCBm                ! [umol.C m-2.sg.hab]         Change in SgCBm this time step
    ! values to write to sediment module
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8), pointer :: delta_sed_DIC(:)                ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_DO (:)                ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8), pointer :: delta_sed_NO3(:)                ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_NH4(:)                ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    real(8), pointer :: delta_sed_PO4(:)                ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
# endif


! >>> Intermediate Calculation Variables <<< !      (erased each time step)
    ! looping index vars
    integer               :: sedlayer                   !                             Sediment layer index (suface layer = 1, bottom layer = Nsed)
    ! Photosynthesis/Growth
    real(8)               :: Growth_rate                ! [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! photosynthesis: light-limit
    real(8)               :: Growth_limit_light         ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    real(8)               :: ELAP                       ! [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! photosynthesis: DIC-limit
    real(8)               :: Growth_limit_DIC           ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: DIC_uptake_possible(0:Nsed)! [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# else
    real(8)               :: DIC_uptake_possible(0:0)   ! 
# endif
    ! photosynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: Growth_limit_PO4           ! [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
# endif
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE && defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: NO3_uptake_possible(0:Nsed)! [umol.NO3 m-2.sg.hab s-1]   NO3 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    real(8)               :: NH4_uptake_possible(0:Nsed)! [umol.NH4 m-2.sg.hab s-1]   NH4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    real(8)               :: PO4_uptake_possible(0:Nsed)! [umol.PO4 m-2.sg.hab s-1]   PO4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# elif defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    real(8)               :: NO3_uptake_possible(0:0)   ! 
    real(8)               :: NH4_uptake_possible(0:0)   ! 
    real(8)               :: PO4_uptake_possible(0:0)   ! 
# elif defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    real(8)               :: NO3_uptake_possible(1:Nsed)! 
    real(8)               :: NH4_uptake_possible(1:Nsed)! 
    real(8)               :: PO4_uptake_possible(1:Nsed)! 
# endif
    ! Respiration/Un-growth
    real(8)               :: Resp_rate                  ! [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! respiration: DO-limit
    real(8)               :: Resp_limit_DO              ! [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    real(8)               :: DO_uptake_possible(0:Nsed) ! [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
# else
    real(8)               :: DO_uptake_possible(0:0)    !
# endif
    ! respiration: possible
    real(8)               :: Resp_pos                   ! [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    ! respiration: required
    real(8)               :: Resp_req                   ! [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
    ! respiration: dieoff
    real(8)               :: dieoff                     ! [umol.C m-2.sg.hab]         Seagrass dieoff this time step
    ! Update at end of growth interval
    ! update total carbon-biomass
    real(8)               :: oldTotSgCBm                ! [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
    real(8)               :: oldTotLfCBm                ! [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
    real(8)               :: oldTotRtCBm                ! [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
    ! allocate growth
    real(8)               :: mass_to_allocate           ! [mol.C]                     mass to allocate in growth or death to either leaves or roots
    ! update bed-density/habitat area
    real(8)               :: desiredHabArea             ! [m2.hab]                    The desired habitat area given the biomass and optimal density
    real(8)               :: translocatableArea         ! [m2.hab]                    Area that can be translocated (shrink or expand) as limited by the translocation rate
    ! leaf-root biomass translocation
    real(8)               :: translocatable_mass        ! [mol.C]                     Biomass that can be translocated between leaf and root as limited by the translocation rate

! ====================================================================================================
! Variable initializations
! ====================================================================================================

! >>> Error-Corrected Inputs <<< !      rewrite input variables to new variables for edgecase handling and safety (set to zero if below zero)
    if (ocean_DIC_conc_in .lt. 0d0) then                                    ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ocean_DIC_conc = 0d0                                                  ! [umol.DIC L-1]              DIC concentration of ocean water
    else                                                                    ! """                         """
      ! [umol/L]     = [umol/kg]         [g/cm3]  (kg/1000g)(1000cm3/L)     ! """                         """
      ocean_DIC_conc = ocean_DIC_conc_in * rho_sw                           ! """                         """
    endif                                                                   ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (ocean_DO__conc_in .lt. 0d0) then                                    ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ocean_DO__conc = 0d0                                                  ! [umol.O2 L-1]               DO  concentration of ocean water
    else                                                                    ! """                         """
      ocean_DO__conc = ocean_DO__conc_in                                    ! """                         """
    endif                                                                   ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE        
    if (ocean_NO3_conc_in .lt. 0d0) then                                    ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ocean_NO3_conc = 0d0                                                  ! [umol.NO3 L-1]              NO3  concentration of ocean water
    else                                                                    ! """                         """
      ocean_NO3_conc = ocean_NO3_conc_in                                    ! """                         """
    endif                                                                   ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (ocean_NH4_conc_in .lt. 0d0) then                                    ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ocean_NH4_conc = 0d0                                                  ! [umol.NH4 L-1]              NH4  concentration of ocean water
    else                                                                    ! """                         """
      ocean_NH4_conc = ocean_NH4_conc_in                                    ! """                         """
    endif                                                                   ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (ocean_PO4_conc_in .lt. 0d0) then                                    ! vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      ocean_PO4_conc = 0d0                                                  ! [umol.PO4 L-1]              PO4  concentration of ocean water
    else                                                                    ! """                         """
      ocean_PO4_conc = ocean_PO4_conc_in                                    ! """                         """
    endif                                                                   ! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# endif

! >>> Outputs <<< !
    ! clear old values from output variables
    DIC_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.DIC m-2.sg.hab s-1]   DIC uptake rate  * direction of ocean to seagrass is positive
    DO__uptake_rate_from_ocean = 0.0d0                                      ! [mmol.DO  m-2.sg.hab s-1]   DO  uptake rate  * direction of ocean to seagrass is positive
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE        
    NO3_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.NO3 m-2.sg.hab s-1]   NO3 uptake rate  * direction of ocean to seagrass is positive
    NH4_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.NH4 m-2.sg.hab s-1]   NH4 uptake rate  * direction of ocean to seagrass is positive
    PO4_uptake_rate_from_ocean = 0.0d0                                      ! [mmol.PO4 m-2.sg.hab s-1]   PO4 uptake rate  * direction of ocean to seagrass is positive
# endif


! >>> Housekeeping Vars <<< !
    ! update at start of every loop before everything else
    Day                 =>  SGRASS(ng)%Day                            (i,j) !                             Simulation whole day (integer round down)
    is_growth_interval  =   .false.                                         !                             Boolean flag for if the current interval is a growth interval

! >>> Saved Constants <<< !
    ! model params
    GridArea            =   SGRASS(ng)%GridArea                       (i,j) ! [m2]                        Area of grid cells
    ! biophysical params
    SgNperC             =   SGRASS(ng)%SgNperC                (species)     ! [mol.N mol-1.C]             Species specific seagrass Nitrogen to Carbon ratio
    SgPperC             =   SGRASS(ng)%SgPperC                (species)     ! [mol.P mol-1.C]             Species specific seagrass Phosphorus to Carbon ratio
    SgMperC             =   SGRASS(ng)%SgMperC                (species)     ! [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    f_above             =   SGRASS(ng)%f_above                (species)     ! [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
    rho_lf              =   SGRASS(ng)%rho_lf                 (species)     ! [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    k_proj              =   SGRASS(ng)%k_proj                 (species)     ! [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    root_mass_profile   =   SGRASS(ng)%root_mass_profile      (species, :)  ! []                          Mass ratio of roots in each layer (total must sum to 1.0)
# endif
    ! photoynthesis: light-limit
    P_max               =   SGRASS(ng)%P_max                  (species)     ! [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    I_k                 =   SGRASS(ng)%I_k                    (species)     ! [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve
    ! photoynthesis: DIC-limit
    Lf_DIC_uptake_max   =   SGRASS(ng)%Lf_DIC_uptake_max      (species)     ! [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    k_lf_DIC            =   SGRASS(ng)%k_lf_DIC               (species)     ! [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    Rt_DIC_flux_max     =   SGRASS(ng)%Rt_DIC_flux_max        (species)     ! [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    k_rt_DIC            =   SGRASS(ng)%k_rt_DIC               (species)     ! [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
# endif
    ! photoynthesis: nutrient-limit
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    Lf_PO4_uptake_max   =   SGRASS(ng)%Lf_PO4_uptake_max      (species)     ! [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    k_lf_PO4            =   SGRASS(ng)%k_lf_PO4               (species)     ! [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Rt_PO4_flux_max     =   SGRASS(ng)%Rt_PO4_flux_max        (species)     ! [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    k_rt_PO4            =   SGRASS(ng)%k_rt_PO4               (species)     ! [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
# endif
    ! respiration: DO-limit
    Lf_DO_uptake_max    =   SGRASS(ng)%Lf_DO_uptake_max       (species)     ! [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    k_lf_DO             =   SGRASS(ng)%k_lf_DO                (species)     ! [umol.DO L-1]               Half saturation consentration of DO in water column for respiration
    ! respiration: required
    R_basal             =   SGRASS(ng)%R_basal                (species)     ! [umol.C m-2.leaf s-1]       Maximum respiration rate
    ! dieoff when respiration is insufficient
    max_dieoff_rate     =   SGRASS(ng)%max_dieoff_rate        (species)     ! [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
    ! Update at end of growth interval
    ! allocate growth
    ! update bed-density/habitat area
    trans_rate          =   SGRASS(ng)%trans_rate             (species)     ! [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
    opt_sg_bed_density  =   SGRASS(ng)%opt_sg_bed_density     (species)     ! [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)

! >>> Values Read From Sediment Module <<< !
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! Nsed                                                    Number of sediment layers
    sed_layer_thickness(:) = SEDECO(ng)%thickness(:)                        ! [mm]                        Thickness of each sediment layer
# endif
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    sed_DIC(:)     =   SEDECO(ng)%DIC(i,j,:)                                ! [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    where (sed_DIC .le. 0.0d0) sed_DIC = 0.0d0                              !                             error-correct negative values to become zero
    sed_DO (:)     =   SEDECO(ng)%DOx(i,j,:)                                ! [umol.DO L-1]               DO  consentration of sediment (in each sediment layer)
    where (sed_DO .le. 0.0d0) sed_DO = 0.0d0                                !                             error-correct negative values to become zero
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    sed_NO3(:)     =   SEDECO(ng)%NO3(i,j,:)                                ! [umol.NO3 L-1]              NO3 consentration of sediment (in each sediment layer)
    where (sed_NO3 .le. 0.0d0) sed_NO3 = 0.0d0                              !                             error-correct negative values to become zero
    sed_NH4(:)     =   SEDECO(ng)%NH4(i,j,:)                                ! [umol.NH4 L-1]              NH4 consentration of sediment (in each sediment layer)
    where (sed_NH4 .le. 0.0d0) sed_NH4 = 0.0d0                              !                             error-correct negative values to become zero
    sed_PO4(:)     =   SEDECO(ng)%PO4(i,j,:)                                ! [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
    where (sed_PO4 .le. 0.0d0) sed_PO4 = 0.0d0                              !                             error-correct negative values to become zero
# endif

! >>> Saved Values <<< !
    ! Update at end of main interval
    SgCBm               =>  SGRASS(ng)%SgCBm                  (species,i,j) ! [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! Update at end of growth interval
    ! update total carbon-biomass
    TotSgCBm            =>  SGRASS(ng)%TotSgCBm               (species,i,j) ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
    ! allocate growth
    TotLfCBm            =>  SGRASS(ng)%TotLfCBm               (species,i,j) ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
    TotRtCBm            =>  SGRASS(ng)%TotRtCBm               (species,i,j) ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    ! update bed-density/habitat area
    HabArea             =>  SGRASS(ng)%HabArea                (species,i,j) ! [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! leaf-root biomass translocation
    ! update carbon-biomass per area
    LfCBm               =>  SGRASS(ng)%LfCBm                  (species,i,j) ! [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
    RtCBm               =>  SGRASS(ng)%RtCBm                  (species,i,j) ! [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
    ! update dry-weight biomass
    TotSgDryBm          =>  SGRASS(ng)%TotSgDryBm             (species,i,j) ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
    TotLfDryBm          =>  SGRASS(ng)%TotLfDryBm             (species,i,j) ! [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
    TotRtDryBm          =>  SGRASS(ng)%TotRtDryBm             (species,i,j) ! [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
    SgDryBm             =>  SGRASS(ng)%SgDryBm                (species,i,j) ! [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
    LfDryBm             =>  SGRASS(ng)%LfDryBm                (species,i,j) ! [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
    RtDryBm             =>  SGRASS(ng)%RtDryBm                (species,i,j) ! [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! update leaf area index
    LAI                 =>  SGRASS(ng)%LAI                    (species,i,j) ! []                          Leaf area index: one-sided green leaf area per unit ground surface area

! >>> Delta Values <<< !      amount to add to stored variables at end of the calculation time step to update values: positive adds, negative subtracts (erased each time step)
    ! set delta variables to zero
    delta_SgCBm         =   0.0d0                                           ! [umol.C m-2.sg.hab]         Change in SgCBm this time step
    ! values to write to sediment module
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    delta_sed_DIC       =>  SEDECO(ng)%sg_dDIC_dt(i, j, :)                  ! [umol.DIC L-1 s-1]          Rate of change in DIC due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_DIC       =   0.0d0                                           ! 
    delta_sed_DO        =>  SEDECO(ng)%sg_dDOx_dt(i, j, :)                  ! [umol.DO L-1 s-1]           Rate of change in DO due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_DO        =   0.0d0                                           !
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    delta_sed_NO3       =>  SEDECO(ng)%sg_dNO3_dt(i, j, :)                  ! [umol.NO3 L-1 s-1]          Rate of change in NO3 due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_NO3       =   0.0d0                                           !
    delta_sed_NH4       =>  SEDECO(ng)%sg_dNH4_dt(i, j, :)                  ! [umol.NH4 L-1 s-1]          Rate of change in NH4 due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_NH4       =   0.0d0                                           ! 
    delta_sed_PO4       =>  SEDECO(ng)%sg_dPO4_dt(i, j, :)                  ! [umol.PO4 L-1 s-1]          Rate of change in PO4 due to flux from roots to sediment (positive means seagrass to sediment)
    delta_sed_PO4       =   0.0d0                                           ! 
# endif


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                  HOUSE KEEPING AT START OF LOOP
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

    if (int(time) .gt. Day) then
      is_growth_interval = .true.
      Day = int(time)
      ! write(*,*) "yt_debug: Day = ", Day
    endif


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                    MAIN INTERVAL CALCULATIONS
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================


! ====================================================================================================
!
! Photosynthesis/Growth
!
!   if x:y:z is the C:N:P ratio
!   (x/x)*CO2 + (y/x)*(NO3+NH4) + (z/y)*PO4 -> (1/x)*(CH2O)x(NH3)y(H3PO4)z + (some)*O2 +/- (some)*H+
!
!   CO2    +   H2O  -> CH2O  +   O2      ! for every mol of CO2  consumed in growth: 1 mol of O2 is produced
!   NO3-   +   H3O+ -> NH3   + 2*O2      ! for every mol of NO3- consumed in growth: 2 mol of O2 is produced
!   NH4+   +   H2O  -> NH3   +   H3O+                          
!   PO4^3- + 3*H3O+ -> H3PO4 + 3*H2O     
!
! ====================================================================================================

! Effective leaf area projection ---------------------------------------------------------------------

    ! ELAP                      [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! k_proj                    [m2.sg.hab m-2.leaf]        Species specific leaf projection coefficient
    ELAP = 1.0d0-exp(-1d0*k_proj*LAI)

! Photosynthesis rate light limit ---------------------------------------------------------------------

    ! Growth_limit_light        [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! P_max                     [umol.C m-2.leaf s-1]       Maximum photoynthesis rate
    ! ELAP                      [m2.lf.proj m-2.sg.hab]     Effective leaf area projection on ground (only within seagrass habitat (covered area))
    ! PFD                       [umol.photon m-2 s-1]       Photon flux density
    ! I_k                       [umol.photon m-2 s-1]       Irradiance where the initial slope intersects P_max on the photosynthesis-irradiance curve

    ! [umol.C/m2.hab s] = [umol.C/m2.lf s] [m2.lf/m2.hab] [             ]
    Growth_limit_light  = P_max            * ELAP         * tanh(PFD/I_k)

! Photosynthesis rate DIC limit -----------------------------------------------------------------------

    ! DIC_uptake_possible(:)    [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_DIC_uptake_max         [umol.DIC m-2.lf s-1]       Maximum uptake rate of DIC in the leaves
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! ocean_DIC_conc            [umol.DIC L-1]              DIC concentration of ocean water 
    ! k_lf_DIC                  [umol.DIC L-1]              Half saturation consentration of DIC in water column for growth

    ! [umol.DIC/m2.hab s]  = [umol.DIC/m2.lf s] [m2.lf/m2.hab] ( Michaelis-Menten term                      )
    DIC_uptake_possible(0) = Lf_DIC_uptake_max  * LAI          * ocean_DIC_conc / (k_lf_DIC + ocean_DIC_conc)

# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_DIC_flux_max             [umol.DIC g-1.rt.drywt s-1] Maximum uptake rate of DIC in the roots 
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_DIC(Nsed)               [umol.DIC L-1]              DIC consentration of sediment (in each sediment layer)
    ! k_rt_DIC                    [umol.DIC L-1]              Half saturation consentration of DIC in soil pore water for growth
    do sedlayer = 1, Nsed
      ! [umol.DIC/m2.hab s]         = [umol.DIC/g.rt s] []                            [g.rt/m2.hab]
      DIC_uptake_possible(sedlayer) = Rt_DIC_flux_max   * root_mass_profile(sedlayer) * RtDryBm     &
        * ( sed_DIC(sedlayer) / (k_rt_DIC + sed_DIC(sedlayer)) )
    enddo
# endif

    ! Growth_limit_DIC          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
    ! DIC_uptake_possible(:)    [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers

    ! [umol.C/m2.hab s] = [mol.C/mol.DIC] [umol.DIC/m2.hab s]
    Growth_limit_DIC    =                 sum(DIC_uptake_possible)


! Growth rate PO4 limit -------------------------------------------------------------------------------

# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE
    ! PO4_uptake_possible(:)    [umol.PO4 m-2.sg.hab s-1]   PO4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_PO4_uptake_max         [umol.PO4 m-2.lf s-1]       Maximum uptake rate of PO4 in the leaves
    ! LAI                       [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! ocean_PO4_conc            [umol.PO4 L-1]              PO4 concentration of ocean water 
    ! k_lf_PO4                  [umol.PO4 L-1]              Half saturation consentration of PO4 in water column for growth

    ! [umol.PO4/m2.hab s]  = [umol.PO4/m2.lf s] [m2.lf/m2.hab] ( Michaelis-Menten term                      )
    PO4_uptake_possible(0) = Lf_PO4_uptake_max * LAI           * ocean_PO4_conc / (k_lf_PO4 + ocean_PO4_conc)
# endif
# if defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! PO4_uptake_possible(:)      [umol.PO4 m-2.sg.hab s-1]   PO4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Rt_PO4_flux_max             [umol.PO4 g-1.rt.drywt s-1] Maximum uptake rate of PO4 in the roots 
    ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
    ! sed_PO4(Nsed)               [umol.PO4 L-1]              PO4 consentration of sediment (in each sediment layer)
    ! k_rt_PO4                    [umol.PO4 L-1]              Half saturation consentration of PO4 in soil pore water for growth
    do sedlayer = 1, Nsed
      ! [umol.PO4/m2.hab s]         = [umol.PO4/g.rt s] []                            [g.rt/m2.hab]
      PO4_uptake_possible(sedlayer) = Rt_PO4_flux_max   * root_mass_profile(sedlayer) * RtDryBm     &
        * ( sed_PO4(sedlayer) / (k_rt_PO4 + sed_PO4(sedlayer)) )
    enddo
# endif
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    ! Growth_limit_PO4          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
    ! SgPperC                   [mol.PO4 mol-1.C]           Species specific seagrass Phosphorus to Carbon ratio
    ! PO4_uptake_possible(:)    [umol.PO4 m-2.sg.hab s-1]   PO4 uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers

    ! [umol.C/m2.hab s] = [mol.C/mol.PO4] [umol.PO4/m2.hab s]
    Growth_limit_PO4    = 1d0 / SgPperC   * sum(PO4_uptake_possible)
# endif

! Gross Photosythesis/Growth rate ---------------------------------------------------------------------

    ! Growth_rate               [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! Growth_limit_light        [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by light
    ! Growth_limit_DIC          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by DIC uptake
    ! Growth_limit_PO4          [umol.C m-2.sg.hab s-1]     Photosynthesis/growth rate possible as limited by PO4 uptake
# if defined YT_SEAGRASS_LEAF_NUTRIENT_UPTAKE || defined YT_SEAGRASS_ROOT_NUTRIENT_UPTAKE
    Growth_rate = min(Growth_limit_light, Growth_limit_DIC, Growth_limit_PO4)
# else
    Growth_rate = min(Growth_limit_light, Growth_limit_DIC)
# endif

    if (Growth_rate .lt. 0d0) then ! error handling
      write(*,*) "yt_debug: negative value error in photosynthesis/growth. Time =", &
        time, "Position =", i, j, "Growth_rate = ", Growth_rate
      Growth_rate = 0.0d0
    endif

! Mass balance ---------------------------------------------------------------------------------------
    
    ! photosynthesis produces biomass
    ! delta_SgCBm                 [umol.C m-2.sg.hab]         Change in SgCBm this time step
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! dt                          [s]                         Time step interval duration
    delta_SgCBm = delta_SgCBm + Growth_rate * dt

    ! yt_edit (growth of biomass consumes N and P) need to implement

# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! DIC_uptake_possible(:)      [umol.DIC m-2.sg.hab s-1]   DIC uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers

    ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [umol.C/m2.hab s]                                               (mmol/1000umol)
    DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean &
      + Growth_rate * (DIC_uptake_possible(0)/sum(DIC_uptake_possible)) * 0.001d0
    DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean &
      - Growth_rate * (TotLfCBm/TotSgCBm) * 0.001d0

    ! delta_sed_CHEM(Nsed)        [umol.CHEM L-1 s-1]         Rate of change in CHEM due to flux from roots to sediment (positive means seagrass to sediment)
    ! root_mass_profile(sed)      []                          Mass ratio of roots in each layer (total must sum to 1.0)
    ! sed_layer_thickness(Nsed)   [mm]                        Thickness of each sediment layer
    do sedlayer = 1, Nsed
      ! [umol.CHEM/L s]       = [umol.CHEM/L s]         - [umol.C/m2.hab s]                                                      [1/mm]                          (1000mm/m)(m3/1000L)
      delta_sed_DIC(sedlayer) = delta_sed_DIC(sedlayer) &
        - Growth_rate * (DIC_uptake_possible(sedlayer)/sum(DIC_uptake_possible)) / sed_layer_thickness(sedlayer)
      delta_sed_DO(sedlayer)  = delta_sed_DO(sedlayer)  &
        + Growth_rate * (root_mass_profile(sedlayer) * TotRtCBm/TotSgCBm ) / sed_layer_thickness(sedlayer)
    enddo
# else
    ! CHEM_uptake_rate_from_ocean [mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
    ! Growth_rate                 [umol.C m-2.sg.hab s-1]     Gross photosynthesis/growth rate
    ! [mmol.CHEM m-2.sg.hab s-1] = [mmol.CHEM m-2.sg.hab s-1] + [umol.C m-2.sg.hab s-1] (mmol/1000umol)
    DIC_uptake_rate_from_ocean   = DIC_uptake_rate_from_ocean + Growth_rate             * 0.001d0
    DO__uptake_rate_from_ocean   = DO__uptake_rate_from_ocean - Growth_rate             * 0.001d0
# endif
! yt_edit add in O2 generation from NO3 consumption

! write(*,*) 'yt_debug: Growth_rate =', Growth_rate
! write(*,*) 'yt_debug: delta_sed_DIC =', delta_sed_DIC

! ====================================================================================================
!
! Respiration/Un-Growth
!
!   if x:y:z is the C:N:P ratio
!   (1/x)*(CH2O)x(NH3)y(H3PO4)z + (some)*O2 +/- (some)*H+ -> (x/x)*CO2 + (y/x)*(NO3+NH4) + (z/y)*PO4
!
!   CH2O  +   O2   -> CO2    +   H2O     ! for every mol of respiration: 1 mol of O2 is consumed
!   NH3   +   H3O+ -> NH4+   +   H2O                          
!   H3PO4 + 3*H2O  -> PO4^3- + 3*H3O+   
!
! ====================================================================================================

! Respiration rate DO limit --------------------------------------------------------------------------

    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    ! Lf_DO_uptake_max            [umol.DO m-2.lf s-1]        Maximum uptake rate of DO in the leaves
    ! LAI                         [m2.leaf m-2.sg.hab]        Leaf area index: one-sided green leaf area per unit ground surface area 
    ! ocean_DO__conc              [umol.DO L-1]               DIC concentration of ocean water 
    ! k_lf_DO                     [umol.DO L-1]               Half saturation consentration of DO in water column for respiration

    ! [umol.DO/m2.hab s]  = [umol.DO/m2.lf s] [m2.lf/m2.hab] ( Michaelis-Menten term                     )
    DO_uptake_possible(0) = Lf_DO_uptake_max  * LAI          * ocean_DO__conc / (k_lf_DO + ocean_DO__conc)
# if defined YT_SEAGRASS_ROOT_CARBON_OXYGEN_EXCHANGE
    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers
    do sedlayer = 1, Nsed
      ! [umol.DO/m2.hab s]
      DO_uptake_possible(sedlayer) = 0d0 !yt_edit finish this!!
    enddo
# endif

    ! Resp_limit_DO               [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake
    ! DO_uptake_possible(:)       [umol.DO m-2.sg.hab s-1]    DO uptake rate possible in each layer; 0 = water column, 1-Nsed = sediment layers

    ! [umol.C/m2.hab s] = [mol.C/mol.DO] [umol.DO/m2.hab s]
    Resp_limit_DO       =                sum(DO_uptake_possible)

    ! yt_edit implement anaerobic respiraation???


! Respiration possible -------------------------------------------------------------------------------

    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    ! Resp_limit_DO               [umol.C m-2.sg.hab s-1]     Respiration/un-growth rate possible as limited by DO uptake

    Resp_pos = Resp_limit_DO

! Respiration required -------------------------------------------------------------------------------

    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
    ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! R_basal                     [umol.C m-2.leaf s-1]       Maximum respiration rate
    ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    ! dt                          [s]                         time step duration

    ! respFrac = R_basal/rho_lf*SgMperC*dt*0.000000001        The fraction of mass required for respiration this time step! [umol.C m-2.leaf s-1] [m2.leaf kg-1.drywt ] [g.drywt mol-1.C] [s] (kg/1000g)(mol/1000000umol) = 0.000000001 []
    ! mass_respired = SgCBm(respFrac)/(1+respFrac)/dt         With a 1:1 ratio of carbon-biomass to respiration we get:
    
    Resp_req = SgCBm * (R_basal/rho_lf*SgMperC*dt*0.000000001)/(1+R_basal/rho_lf*SgMperC*dt*0.000000001) / dt

! Respiration/Un-growth rate -------------------------------------------------------------------------

    ! Resp_rate                   [umol.C m-2.sg.hab s-1]     Gross respiration/un-growth rate
    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
 
    Resp_rate = min(Resp_pos, Resp_req)

    if (Resp_rate .lt. 0d0) then ! error handling
      write(*,*) "yt_debug: negative value error in respiration. Time =", &
        time, "Position =", i, j, "Resp_rate = ", Resp_rate, '   Resp_pos =', Resp_pos, '   Resp_req =', Resp_req
      write (*,*) 'yt_debug: DO_uptake_possible =', DO_uptake_possible
      Resp_rate = 0.0d0
    endif

    ! write(*,*) 'yt_debug: Growth_rate = ', Growth_rate, '   Resp_rate = ', Resp_rate


! Dieoff when respiration is insufficient ------------------------------------------------------------

    ! Resp_req                    [umol.C m-2.sg.hab s-1]     Respiration rate required for maintenance in this time step
    ! Resp_pos                    [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
    if (Resp_req .gt. Resp_pos) then
      ! dieoff                    [umol.C m-2.sg.hab]         Seagrass dieoff this time step
      ! Resp_pos                  [umol.C m-2.sg.hab s-1]     Respiration rate possible in this time step
      ! HabArea                   [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! R_basal                   [umol.C m-2.leaf s-1]       Maximum respiration rate
      ! rho_lf                    [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
      ! SgMperC                   [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! SgCBm                     [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)

      ! respiration possible           = maintenance repiration rate                                                          * biomass maintainable
      ! [umol.resp / s               ] = [umol.resp / umol.C s                                                              ] [umol.C                      ]
      ! [umol.resp/m2.hab s] [m2.hab ] = [umol.resp/m2.lf s] [m2.lf/kg.drywt] [g.drywt/mol.C] (kg/1000g)(mol.C/1000000umol.C) [umol.C/m2.hab     ] [m2.hab ]
      ! Resp_pos             * HabArea = R_basal             / rho_lf         * SgMperC       * 0.000000001d0                 * SgCBm_maintainable * HabArea

      ! solve for SgCBm_maintainable:
      ! SgCBm_maintainable = Resp_pos / (R_basal             / rho_lf         * SgMperC       * 0.000000001d0)

      ! dieoff = SgCBm - SgCBm_maintainable

      dieoff = SgCBm - (Resp_pos / (R_basal             / rho_lf         * SgMperC       * 0.000000001d0))

      ! limit dieoff rate (especially for when DO intake is zero)
      ! max_dieoff_rate             [mol.C mol-1.C day-1]       maximum dieoff in fraction of mass per day, represents atypical modes of survival such as anaerobic respiration and cellular canibalism which limits extreme death rates
      ! [mol.C mol-1.C]     = [mol.C mol-1.C day-1] [s]  (day/86400s)
      ! max_dieoff_per_step = max_dieoff_rate       * dt / 86400
      if (dieoff/SgCBm .gt. max_dieoff_rate*dt/86400d0) then
        dieoff = SgCBm * max_dieoff_rate*dt/86400d0
      endif

      ! delta_SgCBm                 [umol.C m-2.sg.hab]         Change in SgCBm this time step
      ! dieoff                      [umol.C m-2.sg.hab]         Seagrass dieoff this time step
      delta_SgCBm = delta_SgCBm - dieoff

      ! ! CHEM_uptake_rate_from_ocea[mmol.CHEM m-2.sg.hab s-1]  direction of ocean to seagrass is positive
      ! ! dieoff_rate               [umol.C m-2.sg.hab s-1]     Rate of seagrass dieoff
      ! ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [umol.C/m2.hab s] (mmol/1000umol)
      ! DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean - dieoff_rate       * 0.001d0
      ! DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean + dieoff_rate       * 0.001d0

      ! write(*,*) 'yt_debug: dieoff_rate =', dieoff/dt, 'max_dieoff_rate =', SgCBm*max_dieoff_rate/86400d0

    else

! Mass balance ---------------------------------------------------------------------------------------

      ! respiration consumes CH2O in biomass
      delta_SgCBm = delta_SgCBm - Resp_rate * dt

      ! yt_edit (breakdown of biomass releases N and P) need to implement
      
      ! CHEM_uptake_rate from_ocean [mmol m-2 s-1]              direction of ocean to seagrass is positive
      ! Resp                        [mmol.C m-2.ground s-1]     Gross respiration rate
      ! [mmol.CHEM/m2.hab s]     = [mmol.CHEM/m2.hab s]       + [umol.C/m2.hab s] (mmol/1000umol)
      DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean - Resp_rate         * 0.001d0
      DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean + Resp_rate         * 0.001d0
    
    endif


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                                UPDATE AT END OF MAIN INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================

    ! mass balance of seagrass growth/dieoff

    ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
    ! delta_SgCBm                 [umol.C m-2.sg.hab]         Change in SgCBm this time step
    ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
    if (SgCBm + delta_SgCBm .lt. 1.0d0/SgMperC) then  ! if seagrass dry weight biomass would become less than 1.0 ug per m2, then instead set to zero
      delta_SgCBm = -1d0 * SgCBm
      SgCBm = 0d0
    else
      SgCBm = SgCBm + delta_SgCBm
    endif

    ! if (is_growth_interval) then 
		! 	if (delta_SgCBm .lt. 0d0) then
		! 			write(*,'(*(g0:,""))') "yt_debug: day=", Day, "  ELAP=", ELAP, "  ELAP/TotSgDryBm=", &
		! 			ELAP/TotSgDryBm, "  delta-mass: -   ", delta_SgCBm*SgMperC, "  TotSgDryBm=", TotSgDryBm
		! 	else
		! 			write(*,'(*(g0:,""))') "yt_debug: day=", Day, "  ELAP=", ELAP, "  ELAP/TotSgDryBm=", &
		! 			ELAP/TotSgDryBm, "  delta-mass:   + ", delta_SgCBm*SgMperC, "  TotSgDryBm=", TotSgDryBm
		! 	endif
    ! endif

# ifdef ORGANIC_MATTER
    ! yt_edit insert mass balance if organic matter is enabled
# else
    ! ! CHEM_uptake_rate_from_o   [mmol m-2 s-1]              direction of ocean to seagrass is positive
    ! ! HabArea                   [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
    ! ! [mmol.DIC/m2 s]          = [mmol.DIC/m2 s]            + [mol.DIC/mol.C] [mol.C]        [1/m2]    [1/s] (1000mmol/mol)
    ! DIC_uptake_rate_from_ocean = DIC_uptake_rate_from_ocean +                 delta_TotSgCBm / HabArea / dt  * 1000d0
    ! ! SgO2perC                  []                          Species specific seagrass O2 consumed per carbon-biomass decomposed         !  for every mol of carbon-biomass decomposed, 1 + 2y/x mol of O2 is consumed (assuming primarily NO3- is produced at ocean pH levels)
    ! ! [mmol.O2/m2 s]           = [mmol.O2/m2 s]             - [mol.O2/mol.C] [mol.c]          [1/m2]    [1/s] (1000mmol/mol)
    ! DO__uptake_rate_from_ocean = DO__uptake_rate_from_ocean - SgO2perC       * delta_TotSgCBm / HabArea / dt * 1000d0  
# endif


! ====================================================================================================
! ----------------------------------------------------------------------------------------------------
!                               UPDATE AT END OF GROWTH INTERVAL
! ----------------------------------------------------------------------------------------------------
! ====================================================================================================
    
    if (is_growth_interval) then ! yt_edit change this to use the dateclock_mod module in COAWST      

! update total carbon-biomass ------------------------------------------------------------------------

      write(*,*) 'yt_debug: TotSgCBm = ', TotSgCBm, ' TotLfCBm = ', TotLfCBm,  'TotRtCBm = ', TotRtCBm

      ! oldTotSgCBm                 [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
      ! oldTotLfCBm                 [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
      ! oldTotRtCBm                 [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
      oldTotSgCBm = TotSgCBm
      oldTotLfCBm = TotLfCBm
      oldTotRtCBm = TotRtCBm

      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)

      ! [mol.C] = [umol.C/m2.hab] [m2.hab]  (mol/1000000umol)
      TotSgCBm  = SgCBm           * HabArea * 0.000001d0


! allocate growth ------------------------------------------------------------------------------------

      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
      ! oldTotSgCBm                 [mol.C.tot]                 Old total seagrass carbon-biomass in grid cell
      ! oldTotLfCBm                 [mol.C.lf]                  Old total seagrass leaf carbon-biomass in grid cell
      ! oldTotRtCBm                 [mol.C.rt]                  Old total seagrass root carbon-biomass in grid cell
      ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  
      ! mass_to_allocate            [mol.C]                     mass to allocate in growth or death to either leaves or roots
      mass_to_allocate = TotSgCBm - oldTotSgCBm
      if (mass_to_allocate .ge. 0d0) then                                 ! if growth is positive 
        if ((oldTotLfCBm + mass_to_allocate)/TotSgCBm .lt. f_above) then  !   if adding all growth to the leaves and f_above is still below ideal, 
          TotLfCBm = oldTotLfCBm + mass_to_allocate                       !     add all growth to leaves
        elseif ((oldTotLfCBm)/TotSgCBm .gt. f_above) then                 !   elif adding all growth to the roots and f_above is still above ideal, 
          TotRtCBm = oldTotRtCBm + mass_to_allocate                       !     add all growth to roots
        else                                                              !   else
          TotLfCBm = f_above * TotSgCBm                                   !     add growth to leaves and roots to achieve ideal ratio 
          TotRtCBm = TotSgCBm - TotLfCBm
        endif
      else                                                                ! if growth is negative (death)
        if ((oldTotLfCBm + mass_to_allocate)/TotSgCBm .gt. f_above) then  !   if subtracting all death from the leaves and f_above is still above ideal, 
          TotLfCBm = oldTotLfCBm + mass_to_allocate                       !     subtract all death from leaves
        elseif ((oldTotLfCBm)/TotSgCBm .lt. f_above) then                 !   elif subtracting all death from the roots and f_above is still below ideal, 
          TotRtCBm = oldTotRtCBm + mass_to_allocate                       !     subtract all death from roots
        else                                                              !   else
          TotLfCBm = f_above * TotSgCBm                                   !     subtract death from leaves and roots to achieve ideal ratio 
          TotRtCBm = TotSgCBm - TotLfCBm
        endif
      endif

      

      ! ! Allocate biomass (grow or cull) proportional to current proportions (leaf or root)
      ! ! f_above_actual = oldTotLfCBm/oldTotSgCBm
      ! ! f_below_actual = oldTotRtCBm/oldTotSgCBm
      ! ! newTotLfCBm = f_above_actual * newTotSgCBm
      ! ! newTotRtCBm = f_below_actual * newTotSgCBm

      ! TotLfCBm = oldTotLfCBm/oldTotSgCBm * TotSgCBm
      ! TotRtCBm = oldTotRtCBm/oldTotSgCBm * TotSgCBm


! update bed-density/habitat area --------------------------------------------------------------------

      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! desiredHabArea              [m2.hab]                    The desired habitat area given the biomass and optimal density
      ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
      ! opt_sg_bed_density          [g.drywt m-2.hab]           Ideal seagrass bed density (represents guerilla vs phallanx strategy)
      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! translocatableArea          [m2.hab]                    Area that can be translocated (shrink or expand) as limited by the translocation rate
      ! trans_rate                  [mol.C mol-1.C day-1]       Maximum translocation rate as fraction of mass translocatable per day
      ! GridArea                    [m2.ground]                 Area of grid cell

      ! [mol.C/m2.hab]       = [g.drywt/m2.hab]   [mol.C/g.drywt]
      ! opt_sg_bed_C_density = opt_sg_bed_density / SgMperC

      ! TotSgCBm / desiredHabArea = opt_sg_bed_C_density
      ! desiredHabArea = TotSgCBm / opt_sg_bed_C_density

      desiredHabArea = TotSgCBm / (opt_sg_bed_density/SgMperC)

      if ( (TotSgCBm / HabArea) .lt. (opt_sg_bed_density/SgMperC) ) then  ! if density is too low, then shrink hab area
        ! [m2]               = [mol.C]             [m2/mol.C]
        ! translocatableArea = translocatable_mass     / current_density
        translocatableArea   = (TotSgCBm * trans_rate) / (TotSgCBm / HabArea)
        if (HabArea - translocatableArea .lt. desiredHabArea) then
          HabArea = desiredHabArea
        else
          HabArea = HabArea - translocatableArea
        endif
        if (HabArea .lt. 0d0) then 
          HabArea = 0d0
        endif
      else                                                        ! if density is too high, then expand hab area
        ! [m2]               = [mol.C]             [m2/mol.C]
        ! translocatableArea = translocatable_mass     / optimal_density
        translocatableArea   = (TotSgCBm * trans_rate) / (opt_sg_bed_density / SgMperC)
        if (HabArea + translocatableArea .gt. desiredHabArea) then
          HabArea = desiredHabArea
        else
          HabArea = HabArea + translocatableArea
        endif
        if (HabArea .gt. GridArea) then ! yt_edit need to fix this to interact with other habitat types
          HabArea = GridArea
        endif
      endif

      ! update seagrass fraction 
      ! p_sgrass(Nspecies)          [] (0.0~1.0)                Seagrass coverage (every species)
      p_sgrass(species) = HabArea / GridArea ! yt_edit need to fix this to interact with other habitat types


! leaf-root biomass translocation --------------------------------------------------------------------

      ! Translocate biomass
      ! yt_edit fix this to include lateral spreading
      
      ! translocatable_mass         [mol.C]                     Biomass that can be translocated between leaf and root as limited by the translocation rate
      ! trans_rate                  [mol.C/mol.C day-1]         Maximum translocation rate as fraction of mass translocatable per day
      ! TotSgCBm                    [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      ! f_above                     [g.lf.drywt g-1.tot.drywt]  also [mol.C.lf mol-1.C.tot] Ideal above ground biomass to total biomass fraction  

      translocatable_mass = TotSgCBm * trans_rate

      ! write(*,*) 'yt_debug: f_actual =', TotLfCBm / TotSgCBm, '   f_above =', f_above

      if (TotLfCBm / TotSgCBm .lt. f_above) then                              ! translocate from root to leaf if leaf ratio is below optimum
        if ( (TotLfCBm + translocatable_mass) / TotSgCBm .lt. f_above) then   ! if leaf needs more than translocatable amount
          ! write(*,*) 'yt_debug: LEAF needs MORE than translocatable amount'
          TotLfCBm = TotLfCBm + translocatable_mass
        else                                                                  ! if leaf needs less than translocatable amount
          ! write(*,*) 'yt_debug: LEAF needs LESS than translocatable amount'
          TotLfCBm = TotSgCBm * f_above
        endif
        TotRtCBm = TotSgCBm - TotLfCBm
      else                                                                    ! translocate from leef to root if leaf ratio is above optimum
        if ( (TotLfCBm - translocatable_mass) / TotSgCBm .gt. f_above) then   ! if root needs more than translocatable amount
          ! write(*,*) 'yt_debug: ROOT needs MORE than translocatable amount'
          TotRtCBm = TotRtCBm + translocatable_mass
        else                                                                  ! if root needs less than translocatable amount
          ! write(*,*) 'yt_debug: ROOT needs LESS than translocatable amount'
          TotRtCBm = TotSgCBm * (1.0d0 - f_above)
        endif
        TotLfCBm = TotSgCBm - TotRtCBm
      endif

      ! write(*,*) 'yt_debug: f_actual =', TotLfCBm / TotSgCBm, '   f_above =', f_above
      ! write(*,*) '  '


! update carbon-biomass per area ---------------------------------------------------------------------

      ! adjust SgCBm according to new HabArea
      ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
      ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
      ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! [umol.C/m2.hab] = [mol.C]  / [m2.hab] (1000000umol/mol)
      SgCBm             = TotSgCBm / HabArea  * 1000000d0
      LfCBm             = TotLfCBm / HabArea  * 1000000d0
      RtCBm             = TotRtCBm / HabArea  * 1000000d0


! update dry-weight biomass --------------------------------------------------------------------------

      ! TotSgDryBm                  [g.drywt]                   Total seagrass dry weight biomass in grid cell
      ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
      ! TotRtDryBm                  [g.rt.drywt]                Total seagrass root dry weight biomass in grid cell
      ! TotSgCBm                    [mol.C]                     Total seagrass carbon-biomass in grid cell
      ! TotLfCBm                    [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
      ! TotRtCBm                    [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      TotSgDryBm = TotSgCBm * SgMperC
      TotLfDryBm = TotLfCBm * SgMperC
      TotRtDryBm = TotRtCBm * SgMperC

      ! SgDryBm                     [g.drywt m-2.sg.hab]        Seagrass dry weight biomass per unit ground area (in seagrass habitat area)
      ! LfDryBm                     [g.lf.drywt m-2.sg.hab]     Seagrass leaf dry weight biomass per unit ground area (in seagrass habitat area)
      ! RtDryBm                     [g.rt.drywt m-2.sg.hab]     Seagrass root dry weight biomass per unit ground area (in seagrass habitat area)
      ! SgCBm                       [umol.C m-2.sg.hab]         Seagrass leaf+root carbon-biomass per unit ground area (in seagrass habitat area)
      ! LfCBm                       [umol.lf.C m-2.sg.hab]      Seagrass leaf carbon-biomass per unit ground area (in seagrass habitat area)
      ! RtCBm                       [umol.rt.C m-2.sg.hab]      Seagrass root carbon-biomass per unit ground area (in seagrass habitat area)
      ! SgMperC                     [g.drywt mol-1.C]           Species specific biomass mass per unit carbon based on C-N-P ratio according to (CH2O)x(NH3)y(H3PO4)z as the chemical formula for biomass
      ! [g/m2.hab] = [umol.C/m2.hab] [g/mol.C] (mol/1000000umol)
      SgDryBm      = SgCBm           * SgMperC * 0.000001d0
      LfDryBm      = LfCBm           * SgMperC * 0.000001d0
      RtDryBm      = RtCBm           * SgMperC * 0.000001d0


! update leaf area index -----------------------------------------------------------------------------

      ! LAI                         []                          Leaf area index: one-sided green leaf area per unit ground surface area
      ! TotLfDryBm                  [g.lf.drywt]                Total seagrass leaf dry weight biomass in grid cell
      ! rho_lf                      [kg.drywt m-2.leaf]         Rho_leaf: Species specific leaf mass density
      ! HabArea                     [m2.sg.hab]                 Seagrass habitat ground area in grid cell (such that seagrass coverage (p_sgrass) times the grid area equals the habitat area)
      ! [m2.lf/m2.hab] = [g.lf]     [m2.lf/kg.lf] [1/m2.hab] (kg.lf/1000g.lf)
      LAI              = TotLfDryBm / rho_lf      / HabArea  * 0.001d0
    endif


! ====================================================================================================
! End
! ====================================================================================================

    RETURN
    
  END SUBROUTINE seagrass

  ! SUBROUTINE get_seagrass_data(ng, species, i, j, TotSgDryBm, TotSgCBm,TotLfCBm,TotRtCBm)
  !   implicit none
  !   integer, intent(in)    :: ng, species, i, j !                             nested grid number ID, species ID number, i & j positions
  !   real(8), intent(out)   :: TotSgDryBm        ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
  !   real(8), intent(out)   :: TotSgCBm          ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
  !   real(8), intent(out)   :: TotLfCBm          ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
  !   real(8), intent(out)   :: TotRtCBm          ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell

  !   write(*,*) "yt_debug: yo"

  !   TotSgDryBm  = SGRASS(ng)%TotSgDryBm                     (species,i,j)  ! [g.drywt]                   Total seagrass dry weight biomass in grid cell
  !   TotSgCBm    = SGRASS(ng)%TotSgCBm                       (species,i,j)  ! [mol.C.tot]                 Total seagrass carbon-biomass in grid cell
  !   TotLfCBm    = SGRASS(ng)%TotLfCBm                       (species,i,j)  ! [mol.C.lf]                  Total seagrass leaf carbon-biomass in grid cell
  !   TotRtCBm    = SGRASS(ng)%TotRtCBm                       (species,i,j)  ! [mol.C.rt]                  Total seagrass root carbon-biomass in grid cell
    
  !   RETURN
  ! END SUBROUTINE get_seagrass_data

END MODULE mod_seagrass  


#endif
