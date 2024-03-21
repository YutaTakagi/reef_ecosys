!!!=== Copyright (c) 2013-2020 Takashi NAKAMURA  =====!!!
!!!         - Tokyo Institute of Technology -         !!!
!!!                                                   !!!
!!!          Sediment Biogeochemical Module           !!!
!!!            Developed by Masa Muto 2023-           !!!
!!!===================================================!!!
 
#include "cppdefs.h"

#ifdef SEDIMENT_ECOSYS


MODULE mod_sedecosys
  implicit none

! Variables Declaration
  integer, parameter :: Nsed = 101      !! Number of sediment layers

  TYPE T_SEDECO
! general parameters
    real(8), pointer :: poro(:,:,:)   !! Porosity (cm_w3 cm_s-3)
    ! real(8), pointer :: rhos_d(:,:,:) !! dry soil density (g　cm_s-3)
    ! real(8), pointer :: rhos_w(:,:,:) !! wet soil density (g cm_w-3)
    real(8), pointer :: dz(:)         !! layer thickness (cm)
    real(8), pointer :: z(:)          !! sediment depth  (cm)

    real(8), pointer :: Tmp(:,:,:)    !! Temperature    (ºC)
    real(8), pointer :: Sal(:,:,:)    !! Salinity       (PSU)

    real(8), pointer :: O2 (:,:,:)    !! Dissolved Oxygen    (µmol l-1)
    real(8), pointer :: CO2(:,:,:)    !! Carbon dyoxide      (µmol l-1)
    real(8), pointer :: N2 (:,:,:)    !! Nitrogen            (µmol l-1)

! Organic matters
# if defined ORGANIC_MATTER
    real(8), pointer :: DOCf(:,:,:)    !! Dissolved organic carbon   (fast :Labile)       (µmol l-1)
    real(8), pointer :: DOCs(:,:,:)    !! Dissolved organic carbon   (slow :Refractory)   (µmol l-1)
    real(8), pointer :: POCf(:,:,:)    !! Particulate organic carbon (fast :Labile)       (nmol g-1)
    real(8), pointer :: POCs(:,:,:)    !! Particulate organic carbon (slow :Refractory)   (nmol g-1)
    real(8), pointer :: POCn(:,:,:)    !! Particulate organic carbon (slow :Refractory)   (nmol g-1)
# endif

!  Nutrients dynamics
# if defined NUTRIENTS
    real(8), pointer :: NO3(:,:,:)     !! Nitrate     (µmol l-1)
    real(8), pointer :: NH4(:,:,:)     !! Ammonium    (µmol l-1)
    real(8), pointer :: PO4(:,:,:)     !! Phosphate   (µmol l-1)
! #  if defined ORGANIC_MATTER
    ! real(8), pointer :: DON(:,:,:)     !! Dissolved organic nitrogen       (µmol l-1)
    ! real(8), pointer :: PON(:,:,:)     !! Particulate organic nitrogen     (µmol l-1)
    ! real(8), pointer :: DOP(:,:,:)     !! Dissolved organic phosphorus     (µmol l-1)
    ! real(8), pointer :: POP(:,:,:)     !! Particulate organic phosphorus   (µmol l-1)
! #  endif
# endif

! Sulfur dynamics
# if defined SULFATE
    real(8), pointer :: Mn2(:,:,:)    !! Manganese ion        (µmol l-1)
    real(8), pointer :: MnO2(:,:,:)   !! Manganese dioxide    (nmol g-1)

    real(8), pointer :: Fe2(:,:,:)       !! iron(II)          (µmol l-1)
    real(8), pointer :: FeS(:,:,:)       !! iron sulfide      (nmol g-1)
    real(8), pointer :: FeS2(:,:,:)      !! pyrite            (nmol g-1)
    real(8), pointer :: FeOOH(:,:,:)     !! iron hydroxide    (nmol g-1)
    real(8), pointer :: FeOOH_PO4(:,:,:) !! iron-bound phosphate (FeOOH=PO43-)   (nmol g-1)

    real(8), pointer :: H2S(:,:,:)       !! hydrogen sulfide         (µmol l-1)
    real(8), pointer :: SO4(:,:,:)      !! sulfate   (µmol l-1)
    real(8), pointer :: S0(:,:,:)       !! sulfur    (nmol g-1)
# endif
  END TYPE T_SEDECO
  TYPE (T_SEDECO), allocatable :: SEDECO(:)

CONTAINS

!!! **********************************************************************
!!!  Initial conditions for sediment ecosystem model
!!! **********************************************************************

  SUBROUTINE initialize_sedecosys(ng, Ngrids, LBi, UBi, LBj, UBj)

    USE mod_geochem ! another module in a same folder (function list)
    ! USE mod_readinput
    
    implicit none

    ! input parameters
    integer, intent(in) :: ng, Ngrids, LBi, UBi, LBj, UBj  ! numbers for grid
    integer i,j,k ! symbols for loop calculations
    !! depth : Diffusive Boundary Layer (DBL)
    real(8), parameter :: dbl = 0.03d0 ! DBL:difussive boundary layer (cm)
    !! porosity   ( poro = a + b * exp(c * z)   (fossing et al 2004) )
    !! Kasih 2009
    ! real(8), parameter :: coefa_poro = 0.88d0
    ! real(8), parameter :: coefa_poro = 0.90d0
    ! real(8), parameter :: coefb_poro = 0.04d0
    ! real(8), parameter :: coefc_poro = -0.4d0
    !! Fossing 2004
    real(8), parameter :: coefa_poro = 0.763d0
    real(8), parameter :: coefb_poro = 0.086d0
    real(8), parameter :: coefc_poro = -0.216d0

    ! allocate sizes of each parameters for (LBi:UBi, LBj:UBj)
    IF (ng.eq.1) allocate( SEDECO(Ngrids) )
    !! General
    allocate( SEDECO(ng)%poro(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%dz  (Nsed) )
    allocate( SEDECO(ng)%z   (Nsed) )
    allocate( SEDECO(ng)%Tmp (LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%Sal (LBi:UBi,LBj:UBj,Nsed) )
    
    allocate( SEDECO(ng)%O2  (LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%CO2 (LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%N2  (LBi:UBi,LBj:UBj,Nsed) )
    !! Organic Matter(Carbon)
# if defined ORGANIC_MATTER
    allocate( SEDECO(ng)%DOCf(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%DOCs(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%POCf(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%POCs(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%POCn(LBi:UBi,LBj:UBj,Nsed) )
# endif
    !! Nutrient dynamics
# if defined NUTRIENTS
    allocate( SEDECO(ng)%NO3(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%NH4(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%PO4(LBi:UBi,LBj:UBj,Nsed) )
! #  if defined ORGANIC_MATTER
    ! allocate( SEDECO(ng)%DON(LBi:UBi,LBj:UBj,Nsed) )
    ! allocate( SEDECO(ng)%PON(LBi:UBi,LBj:UBj,Nsed) )
    ! allocate( SEDECO(ng)%DOP(LBi:UBi,LBj:UBj,Nsed) )
    ! allocate( SEDECO(ng)%POP(LBi:UBi,LBj:UBj,Nsed) )
! #  endif
# endif     
    !! Sulfur dynamics
# if defined SULFATE
    allocate( SEDECO(ng)%Mn2(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%MnO2(LBi:UBi,LBj:UBj,Nsed) )

    allocate( SEDECO(ng)%Fe2(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%FeS(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%FeS2(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%FeOOH(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%FeOOH_PO4(LBi:UBi,LBj:UBj,Nsed) )

    allocate( SEDECO(ng)%H2S(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%SO4(LBi:UBi,LBj:UBj,Nsed) )
    allocate( SEDECO(ng)%S0(LBi:UBi,LBj:UBj,Nsed) )
# endif

! - - - - - - - - - - - - - - - - - - - - - - - -
    !  Set initial conditions

    !!! layer thickness and depth
    SEDECO(ng)%dz(1) = 0.03d0      ! thickness of surface layer (cm)
    SEDECO(ng)%z(1)  = 0.03d0      ! depth at bottom surface (cm)

    do k = 2,41                                    ! dz is constantly 0.03 until z=1.2cm
        SEDECO(ng)%dz(k) = SEDECO(ng)%dz(1);
        SEDECO(ng)%z(k)  = SEDECO(ng)%z(k-1) + SEDECO(ng)%dz(k)
    enddo
    do k = 42,Nsed                               ! dz exponentially increases until z=20cm (Nsed=100)
        SEDECO(ng)%dz(k) = SEDECO(ng)%dz(2) * exp(0.06265 * (k -41))   ! 0.06265 is a tuned constant
        SEDECO(ng)%z(k)  = SEDECO(ng)%z(k-1) + SEDECO(ng)%dz(k)
    enddo

    !!! porosity
    ! SEDECO(ng)%poro(:,:,1) = 1.0d0
    do k = 1,Nsed
        ! SEDECO(ng)%poro(:,:,k) = coefa_poro + coefb_poro * exp(coefc_poro * SEDECO(ng)%z(k))
        !! --- Shizugawa --- !!
        SEDECO(ng)%poro(:,:,k) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%z(k))
        !! --- Shiraho --- !!
        ! SEDECO(ng)%poro(:,:,k) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%z(k))
        !! --- Fukido --- !!
        ! SEDECO(ng)%poro(:,:,k) = 0.6007d0 - 0.006d0 * log(SEDECO(ng)%z(k))
    enddo

# if defined initialmode
    !!! ------------------- For determinating initial concentration ----------------------- !!!
        ! General
        SEDECO(ng)%Tmp(:,:,:)  = 0.0d0     
        SEDECO(ng)%Sal(:,:,:)  = 0.0d0

        SEDECO(ng)%O2 (:,:,:)  = 300.0d0        !DO  (µmol L-1)
        SEDECO(ng)%CO2(:,:,:)  = 0.0d0        !CO2  (µmol L-1)
        SEDECO(ng)%N2 (:,:,:)  = 0.0d0        !N2  (µmol L-1)
        ! OM
#  if defined ORGANIC_MATTER
        SEDECO(ng)%DOCf(:,:,:)  = 0.0d0       !DOCf  (µmol L-1) 
        SEDECO(ng)%DOCs(:,:,:)  = 2000.0d0       !DOCs  (µmol L-1) 
        SEDECO(ng)%POCf(:,:,:)  = 0.0d0       !DOCf  (µmol L-1) 
        ! SEDECO(ng)%POCf(:,:,:)  = 1.0d6      !POCf  (nmol g-1)          2.0d6
        ! SEDECO(ng)%POCf(:,:,:)  = 3.0d6      !POCf  (nmol g-1)
        ! SEDECO(ng)%POCs(:,:,:)  = 1.1d6      !DOCf  (µmol L-1) 
        ! SEDECO(ng)%POCs(:,:,:)  = 1.0d6      !POCs  (nmol g-1)          1.0d6
        ! SEDECO(ng)%POCs(:,:,:)  = 8.0d5      !POCs  (nmol g-1)
        SEDECO(ng)%POCs(:,:,:)  = 8.5d5       !POCs  (nmol g-1)
        ! SEDECO(ng)%POCn(:,:,:)  = 1.0d6       !DOCf  (µmol L-1)
        SEDECO(ng)%POCn(:,:,:)  = 8.5d5      !DOCf  (µmol L-1)
        ! SEDECO(ng)%POCn(:,:,:)  = 4.0d5      !DOCf  (µmol L-1)
#  endif
        ! Nutrient dynamics
#  if defined NUTRIENTS
        SEDECO(ng)%NO3(:,:,:) = 5.0d0        !NO3  (µmol L-1) 
        SEDECO(ng)%NH4(:,:,:) = 50.0d0        !NH4  (µmol L-1) 
        SEDECO(ng)%PO4(:,:,:) = 10.0d0        !PO4  (µmol L-1) 
! #  if defined ORGANIC_MATTER                 
!         SEDECO(ng)%DON(:,:,:) = 0.0d0   !DON  (µmol L-1) 
!         SEDECO(ng)%PON(:,:,:) = 0.0d0   !PON  (µmol L-1) 
!         SEDECO(ng)%DOP(:,:,:) = 0.0d0   !DOP  (µmol L-1) 
!         SEDECO(ng)%POP(:,:,:) = 0.0d0   !POP  (µmol L-1) 
! #  endif
#  endif
        ! Sulfur dynamics
#  if defined SULFATE
        SEDECO(ng)%Mn2(:,:,:) = 60.0d0    !Mn2  (µmol L-1) 
        SEDECO(ng)%MnO2(:,:,1:50) = 1.0d4   !MnO2  (nmol g-1) 
        SEDECO(ng)%MnO2(:,:,51:Nsed) = 0.0d0   !MnO2  (nmol g-1) 

        SEDECO(ng)%Fe2(:,:,:) = 0.0d0          !Fe2  (µmol L-1) 
        SEDECO(ng)%FeS(:,:,:) = 0.0d0          !FeS  (nmol g-1) 
        SEDECO(ng)%FeS2(:,:,:) = 1.0d4         !FeS2  (nmol g-1) 
        SEDECO(ng)%FeOOH(:,:,1:50) = 1.0d5        !FeOOH  (nmol g-1)
        SEDECO(ng)%FeOOH(:,:,51:Nsed) = 0.0d0        !FeOOH  (nmol g-1)
        SEDECO(ng)%FeOOH_PO4(:,:,:) = 0.0d0    !FeOOH=PO4  (nmol g-1) 

        SEDECO(ng)%H2S(:,:,:) = 100.0d0     !H2S  (µmol L-1)
        SEDECO(ng)%SO4(:,:,:) = 28.0d3     !SO4  (µmol L-1)
        SEDECO(ng)%S0(:,:,:)  = 0.0d0     !S0  (nmol g-1)
#  endif
       !!    --------------------   !!
!         SEDECO(ng)%Tmp(:,:,:)  = 0.0d0     
!         SEDECO(ng)%Sal(:,:,:)  = 0.0d0

!         SEDECO(ng)%O2 (:,:,:)  = 0.0d0        !DO  (µmol L-1)
!         SEDECO(ng)%CO2(:,:,:)  = 0.0d0        !CO2  (µmol L-1)
!         SEDECO(ng)%N2 (:,:,:)  = 0.0d0        !N2  (µmol L-1)
! ! OM
! #  if defined ORGANIC_MATTER
!         SEDECO(ng)%DOCf(:,:,:)  = 0.0d0       !DOCf  (µmol L-1) 
!         SEDECO(ng)%DOCs(:,:,:)  = 0.0d0       !DOCs  (µmol L-1) 
!         SEDECO(ng)%POCf(:,:,:)  = 0.0d0       !DOCf  (µmol L-1) 
!         ! SEDECO(ng)%POCf(:,:,:)  = 1.0d6      !POCf  (nmol g-1)          2.0d6
!         ! SEDECO(ng)%POCf(:,:,:)  = 3.0d6      !POCf  (nmol g-1)
!         ! SEDECO(ng)%POCs(:,:,:)  = 1.1d6      !DOCf  (µmol L-1) 
!         ! SEDECO(ng)%POCs(:,:,:)  = 1.0d6      !POCs  (nmol g-1)          1.0d6
!         SEDECO(ng)%POCs(:,:,:)  = 0.0d0      !POCs  (nmol g-1)
!         ! SEDECO(ng)%POCs(:,:,:)  = 1.3d6       !POCs  (nmol g-1)
!         SEDECO(ng)%POCn(:,:,:)  = 8.0d5       !DOCf  (µmol L-1)
!         ! SEDECO(ng)%POCn(:,:,:)  = 1.0d6      !DOCf  (µmol L-1)
!         ! SEDECO(ng)%POCn(:,:,:)  = 4.0d5      !DOCf  (µmol L-1)
! #  endif
!     ! Nutrient dynamics
! #  if defined NUTRIENTS
!         SEDECO(ng)%NO3(:,:,:) = 0.0d0        !NO3  (µmol L-1) 
!         SEDECO(ng)%NH4(:,:,:) = 0.0d0        !NH4  (µmol L-1) 
!         SEDECO(ng)%PO4(:,:,:) = 0.0d0        !PO4  (µmol L-1) 
!     ! #  if defined ORGANIC_MATTER                 
!     !         SEDECO(ng)%DON(:,:,:) = 0.0d0   !DON  (µmol L-1) 
!     !         SEDECO(ng)%PON(:,:,:) = 0.0d0   !PON  (µmol L-1) 
!     !         SEDECO(ng)%DOP(:,:,:) = 0.0d0   !DOP  (µmol L-1) 
!     !         SEDECO(ng)%POP(:,:,:) = 0.0d0   !POP  (µmol L-1) 
!     ! #  endif
! #  endif
! ! Sulfur dynamics
! #  if defined SULFATE
!         SEDECO(ng)%Mn2(:,:,:) = 0.0d0    !Mn2  (µmol L-1) 
!         SEDECO(ng)%MnO2(:,:,:) = 0.0d0   !MnO2  (nmol g-1) 

!         SEDECO(ng)%Fe2(:,:,:) = 0.0d0          !Fe2  (µmol L-1) 
!         SEDECO(ng)%FeS(:,:,:) = 0.0d0          !FeS  (nmol g-1) 
!         SEDECO(ng)%FeS2(:,:,:) = 0.0d0         !FeS2  (nmol g-1) 
!         SEDECO(ng)%FeOOH(:,:,:) = 0.0d0        !FeOOH  (nmol g-1)
!         SEDECO(ng)%FeOOH_PO4(:,:,:) = 0.0d0    !FeOOH=PO4  (nmol g-1) 

!         SEDECO(ng)%H2S(:,:,:) = 0.0d0     !H2S  (µmol L-1)
!         SEDECO(ng)%SO4(:,:,:) = 0.0d0     !SO4  (µmol L-1)
!         SEDECO(ng)%S0(:,:,:)  = 0.0d0     !S0  (nmol g-1)
! #  endif

# else
    !!! -------------- initial concentration from sed initial concentration ----------------- !!!
        ! ! General
        open(199, file='input/sedinitial_DO.txt', status='old')
        read(199,*) SEDECO(ng)%O2(:,:,:)
        close(199)

        open(200, file='input/sedinitial_DOCf.txt', status='old')
        read(200,*) SEDECO(ng)%DOCf(:,:,:)
        close(200)

        open(201, file='input/sedinitial_DOCs.txt', status='old')
        read(201,*) SEDECO(ng)%DOCs(:,:,:)
        close(201)

        open(202, file='input/sedinitial_POCf.txt', status='old')
        read(202,*) SEDECO(ng)%POCf(:,:,:)
        close(202)

        open(203, file='input/sedinitial_POCs.txt', status='old')
        read(203,*) SEDECO(ng)%POCs(:,:,:)
        close(203)

        open(204, file='input/sedinitial_POCn.txt', status='old')
        read(204,*) SEDECO(ng)%POCn(:,:,:)
        close(204)

        open(205, file='input/sedinitial_NO3.txt', status='old')
        read(205,*) SEDECO(ng)%NO3(:,:,:)
        close(205)

        open(206, file='input/sedinitial_NH4.txt', status='old')
        read(206,*) SEDECO(ng)%NH4(:,:,:)
        close(206)

        open(207, file='input/sedinitial_PO4.txt', status='old')
        read(207,*) SEDECO(ng)%PO4(:,:,:)
        close(207)
        
        open(208, file='input/sedinitial_Mn2.txt', status='old')
        read(208,*) SEDECO(ng)%Mn2(:,:,:)
        close(208)

        open(209, file='input/sedinitial_MnO2.txt', status='old')
        read(209,*) SEDECO(ng)%MnO2(:,:,:)
        close(209)

        open(210, file='input/sedinitial_Fe2.txt', status='old')
        read(210,*) SEDECO(ng)%Fe2(:,:,:)
        close(210)

        open(211, file='input/sedinitial_FeOOH.txt', status='old')
        read(211,*) SEDECO(ng)%FeOOH(:,:,:)
        close(211)

        open(212, file='input/sedinitial_FeS.txt', status='old')
        read(212,*) SEDECO(ng)%FeS(:,:,:)
        close(212)

        open(213, file='input/sedinitial_FeS2.txt', status='old')
        read(213,*) SEDECO(ng)%FeS2(:,:,:)
        close(213)

        open(214, file='input/sedinitial_FeOOH_PO4.txt', status='old')
        read(214,*) SEDECO(ng)%FeOOH_PO4(:,:,:)
        close(214)

        open(215, file='input/sedinitial_H2S.txt', status='old')
        read(215,*) SEDECO(ng)%H2S(:,:,:)
        close(215)

        open(216, file='input/sedinitial_S0.txt', status='old')
        read(216,*) SEDECO(ng)%S0(:,:,:)
        close(216)

        open(217, file='input/sedinitial_SO4.txt', status='old')
        read(217,*) SEDECO(ng)%SO4(:,:,:)
        close(217)

# endif
    !! for debug
    ! write(*,*) SEDECO(ng)%SO4(1,1,:)


    RETURN

  END SUBROUTINE initialize_sedecosys
 ! ****************************************************************** !     



!!! **********************************************************************
!!!  Main program of sediment ecosystem model
!!! **********************************************************************
! amb:ambient = bottom grid of water column = Just above seafloor
! amb will be set as value(1) in mod_reef_ecosys.F

  SUBROUTINE sedecosys           &
!   input parameters
    ( ng, i, j       &   ! ng: nested grid number; i,j: position
    , dt             &   ! Time step (sec)
    , ymax           &   ! calculation year
    , Tamb           &   ! Tmp: Temperature (oC)
    , Samb           &   ! Sal: Salinity (PSU)

    , O2amb          &   ! O2: Dissolved oxygen (umol L-1)
    , CO2amb         &   ! CO2: carbon dioxide  (µmol L-1)
    , N2amb          &   ! N2: Nitrogen         (µmol L-1)
# if defined ORGANIC_MATTER
    , DOCfamb         &   ! DOCf: Dissolved organic carbon (umol L-1)
    , DOCsamb         &   ! DOCs: Dissolved organic carbon (umol L-1)
    , POCfamb         &   ! POCf: Particulate organic carbon (nmol g-1)
    , POCsamb         &   ! POCs: Particulate organic carbon (nmol g-1)
# endif
# if defined NUTRIENTS
    , NO3amb         &   ! NO3: NO3 (umol L-1)
    , NH4amb         &   ! NH4: NH4 (umol L-1)
    , PO4amb         &   ! PO4: PO4 (umol L-1)
! #  if defined ORGANIC_MATTER
!     , DONamb         &   ! DON: Dissolved organic nitrogen (DON: umol L-1)
!     , PONamb         &   ! PON: Particulate organic nitrogen (PON: umol L-1)
!     , DOPamb         &   ! DOP: Dissolved organic phosporius (DOP: umol L-1)
!     , POPamb         &   ! POP: Particulate organic phosporius (POP: umol L-1)
! #  endif
# endif
# if defined SULFATE
    , Mn2amb         &  ! Mn2 (µmol L-1)
    , Fe2amb         &  ! Fe2 (µmol L-1)
    , H2Samb         &  ! H2S (µmol L-1)
    , SO4amb         &  ! SO4 (µmol L-1)
    , Fdep_MnO2      &  ! MnO2 deposition flux (nmol cm-2 s-1)
    , Fdep_FeOOH     &  ! FeOOH depositoin flux (nmol cm-2 s-1)
# endif
# if defined ORGANIC_MATTER
    , Fdep_POC       &   ! POC deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  if defined NUTRIENTS
    , Fdep_PON       &   ! PON deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Fdep_POP       &   ! POP deposition flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
#  endif
# endif
!   output parameters
    , Flux_Tmp       &   ! Temperature flux (K cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_Sal       &   ! Salinity flux (cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_DO        &   ! DO  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_CO2sed       &   ! CO2 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_N2        &   ! N2  flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
# if defined ORGANIC_MATTER
    , Flux_DOCf       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_DOCs       &   ! DOC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POCf       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_POCs       &   ! POC flux (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
# if defined NUTRIENTS
    , Flux_NO3       &   ! NO3 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_NH4       &   ! NH4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
    , Flux_PO4       &   ! PO4 flux (nmol cm-2 s-1)  * direction of water column to sediment is positive
! #  if defined ORGANIC_MATTER
!     , Flux_DON       &   ! DON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
!     , Flux_PON       &   ! PON flux (nmol cm-2 s-1) * direction of water column to sediment is positive
!     , Flux_DOP       &   ! DOP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
!     , Flux_POP       &   ! POP flux (nmol cm-2 s-1) * direction of water column to sediment is positive
! #  endif
# endif
# if defined SULFATE
    , Flux_Mn2       &   ! Mn2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_MnO2      &   ! MnO2 flux  (nmol cm-2 s-1) * direction of water column to sediment is positive

    , Flux_Fe2       &   ! Fe2 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_FeS       &   ! FeS flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_FeS2      &   ! FeS2 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_FeOOH     &   ! FeOOH flux  (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_FeOOH_PO4 &   ! FeOOH=PO43 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive

    , Flux_H2S       &   ! H2S flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_S0        &   ! S0 flux    (nmol cm-2 s-1) * direction of water column to sediment is positive
    , Flux_SO4       &   ! SO4 flux   (nmol cm-2 s-1) * direction of water column to sediment is positive
# endif
    )

    USE mod_geochem
    
    implicit none

! ***********************************************************************************
!!! 宣言文 declarative statement
!! These variable names are different from those in mod_reef_ecosys.F

    !! General
    integer, intent(in) :: ng, i, j
    real(8), intent(in) :: dt
    real(8), intent(in) :: ymax
    real(8), intent(in) :: Tamb     
    real(8), intent(in) :: Samb     

    real(8), intent(in) :: O2amb
    real(8), intent(in) :: CO2amb
    real(8), intent(in) :: N2amb
    !! Organic matters (Carbon)
# if defined ORGANIC_MATTER
    real(8), intent(in) :: DOCfamb
    real(8), intent(in) :: DOCsamb
    real(8), intent(in) :: POCfamb 
    real(8), intent(in) :: POCsamb
# endif
    !! Nutrient
# if defined NUTRIENTS
    real(8), intent(in) :: NO3amb   
    real(8), intent(in) :: NH4amb   
    real(8), intent(in) :: PO4amb   
! #  if defined ORGANIC_MATTER
!     real(8), intent(in) :: DONamb   
!     real(8), intent(in) :: PONamb   
!     real(8), intent(in) :: DOPamb   
!     real(8), intent(in) :: POPamb   
! #  endif
# endif
    !! Sulfate
# if defined SULFATE
    real(8), intent(in) :: Mn2amb
    real(8), intent(in) :: Fe2amb
    real(8), intent(in) :: H2Samb
    real(8), intent(in) :: SO4amb 
# endif

# if defined ORGANIC_MATTER
    real(8), intent(in) :: Fdep_POC   
#  if defined NUTRIENTS
    real(8), intent(in) :: Fdep_PON   
    real(8), intent(in) :: Fdep_POP   
#  endif
# endif

!! output parameters (Flux)
    real(8), intent(out) :: Flux_Tmp
    real(8), intent(out) :: Flux_Sal

    real(8), intent(out) :: Flux_DO
    real(8), intent(out) :: Flux_CO2sed
    real(8), intent(out) :: Flux_N2 
# if defined ORGANIC_MATTER
    real(8), intent(out) :: Flux_DOCf
    real(8), intent(out) :: Flux_DOCs
    real(8), intent(out) :: Flux_POCf
    real(8), intent(out) :: Flux_POCs
# endif
# if defined NUTRIENTS
    real(8), intent(out) :: Flux_NO3
    real(8), intent(out) :: Flux_NH4
    real(8), intent(out) :: Flux_PO4
! #  if defined ORGANIC_MATTER
!     real(8), intent(out) :: Flux_DON
!     real(8), intent(out) :: Flux_PON
!     real(8), intent(out) :: Flux_DOP
!     real(8), intent(out) :: Flux_POP
! #  endif
# endif
# if defined SULFATE
    real(8), intent(out) :: Flux_Mn2
    real(8), intent(out) :: Flux_MnO2

    real(8), intent(out) :: Flux_Fe2
    real(8), intent(out) :: Flux_FeS
    real(8), intent(out) :: Flux_FeS2
    real(8), intent(out) :: Flux_FeOOH
    real(8), intent(out) :: Flux_FeOOH_PO4

    real(8), intent(out) :: Flux_H2S
    real(8), intent(out) :: Flux_S0
    real(8), intent(out) :: Flux_SO4
# endif


!!! Set parameters 
    integer :: k       ! symbol for loop cal

!! -- Physical properties of sediment ------------------------------
    real(8) :: z(Nsed)         ! depth(cm)
    real(8) :: dz(Nsed)        ! layer thickness(cm)
    real(8), parameter :: dbl = 0.03d0 ! DBL:difussive boundary layer (cm)
    real(8) :: rho_sw(0:Nsed)    ! seawater density (g cm-3)
    real(8) :: rhos_d(Nsed)      ! soil density (g cm_s-3)

    
!! -- Diffusion coefficient -----------------------------------------
    !! General diffusion in pure seawater  (cm2 s-1)
    real(8) :: Dw_DOC(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: Dw_O2(Nsed),Dw_CO2(Nsed),Dw_N2(Nsed)
# if defined NUTRIENTS
    real(8) :: Dw_NO3(Nsed), Dw_NH4(Nsed), Dw_PO4(Nsed)
# endif
# if defined SULFATE
    real(8) :: Dw_Mn2(Nsed), Dw_Fe2(Nsed), Dw_SO4(Nsed), Dw_H2S(Nsed)
# endif
    !! Molecular Diffusion coefficients (cm2 s-1)
    real(8) :: Ds_DOC(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: Ds_O2(Nsed)
# if defined NUTRIENTS
    real(8) :: Ds_NO3(Nsed), Ds_NH4(Nsed), Ds_PO4(Nsed)
! #  if defined ORGANIC_MATTER
!     real(8) :: Ds_DON(0:Nsed), Ds_DOP(0:Nsed)
! #  endif
# endif
# if defined SULFATE
    real(8) :: Ds_Mn2(Nsed), Ds_Fe2(Nsed), Ds_H2S(Nsed), Ds_SO4(Nsed)!, Ds_S2(Nsed)
# endif
    !! Biodifuusivity　(cm2 s-1)
    real(8) :: DBd(Nsed), DBp(Nsed)    !(cm2 s-1)

    !! Adsorption constant K (cm3 g-1)
    real(8),parameter :: Kad_NO3 = 5.4d0   !(cm3 g-1)
    real(8),parameter :: Kad_NH4 = 2.2d0   !(cm3 g-1)
    real(8),parameter :: Kad_PO4 = 2.0d0   !(cm3 g-1)
    real(8),parameter :: Kad_Mn2 = 13.0d0  !(cm3 g-1)
    real(8),parameter :: Kad_Fe2 = 500.0d0 !(cm3 g-1)

    !! Sedimentation rate (cm/sec)
# if defined Burial
    ! real(8),parameter :: u = 0.064d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Fossing 2004
    ! real(8),parameter :: u = 0.38d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Irie 2018
    real(8),parameter :: u = 0.5d0 / (365d0*24d0*60d0*60d0) ! (cm/sec) ! Mochida 2023
# endif

    !! Deposition Flux (test) Kasih 2009 -------- 
    real(8) :: Fdep_MnO2, Fdep_FeOOH
    

    !! Diffusion Flux  ----------------------------------------------
       !(Fd :: molecular diffusion flux, Fb :: biodiffusive flux)
       !! unit: (nmol cm-2 s-1)
    real(8) :: Fd_Tmp(Nsed), Fb_Tmp(Nsed)
    real(8) :: Fd_Sal(Nsed), Fb_sal(Nsed)

    real(8) :: Fd_O2 (Nsed), Fb_O2(Nsed)
    real(8) :: Fd_CO2(Nsed), Fb_CO2(Nsed)
    real(8) :: Fd_N2 (Nsed), Fb_N2(Nsed)
# if defined ORGANIC_MATTER-d
    real(8) :: Fd_DOC(Nsed,2),Fb_DOC(Nsed,2)    ! 1:labile, 2:refractory
    real(8) ::                Fb_POC(Nsed,3)   ! 1:labile, 2:refractory, 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: Fd_NO3(Nsed), Fb_NO3(Nsed)
    real(8) :: Fd_NH4(Nsed), Fb_Nh4(Nsed)
    real(8) :: Fd_PO4(Nsed), Fb_PO4(Nsed)
! #  if defined ORGANIC_MATTER
!     real(8) :: Fd_DON(Nsed), Fb_DON(Nsed)
!     real(8) ::                 Fb_PON(Nsed)
!     real(8) :: Fd_DOP(Nsed), Fb_DOP(Nsed)
!     real(8) ::                 Fb_POP(Nsed)
! #  endif
# endif
# if defined SULFATE
    real(8) :: Fd_Mn2(Nsed), Fb_Mn2(Nsed)
    real(8) ::               Fb_MnO2(Nsed)
    real(8) :: Fd_Fe2(Nsed), Fb_Fe2(Nsed)
    real(8) ::               Fb_FeS(Nsed)
    real(8) ::               Fb_FeS2(Nsed)
    real(8) ::               Fb_FeOOH(Nsed)
    real(8) ::               Fb_FeOOH_PO4(Nsed)
    real(8) :: Fd_H2S(Nsed), Fb_H2S(Nsed)
    real(8) ::               Fb_S0(Nsed)
    real(8) :: Fd_SO4(Nsed), Fb_SO4(Nsed)
# endif

    !! Brial Flux (Fc) ----------------------------------- 
    !! unit: (nmol cm-2 s-1)
# if defined Burial
    real(8) :: Fc_Tmp(Nsed)
    real(8) :: Fc_sal(Nsed)

    real(8) :: Fc_O2(Nsed)
    real(8) :: Fc_CO2(Nsed)
    real(8) :: Fc_N2(Nsed)
#  if defined ORGANIC_MATTER
    real(8) :: Fc_DOC(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: Fc_POC(Nsed,3)    ! 1:labile, 2:refractory 3:non-degratable
#  endif
#  if defined NUTRIENTS
    real(8) :: Fc_NO3(Nsed)
    real(8) :: Fc_NH4(Nsed)
    real(8) :: Fc_PO4(Nsed)
! #   if defined ORGANIC_MATTER
!     real(8) :: Fc_DON(Nsed)
!     real(8) :: Fc_PON(Nsed)
!     real(8) :: Fc_DOP(Nsed)
!     real(8) :: Fc_POP(Nsed)
! #   endif
#  endif
#  if defined SULFATE
    real(8) :: Fc_Mn2(Nsed)
    real(8) :: Fc_MnO2(Nsed)
    real(8) :: Fc_Fe2(Nsed)
    real(8) :: Fc_FeS(Nsed)
    real(8) :: Fc_FeS2(Nsed)
    real(8) :: Fc_FeOOH(Nsed)
    real(8) :: Fc_FeOOH_PO4(Nsed)
    real(8) :: Fc_H2S(Nsed)
    real(8) :: Fc_S0(Nsed)
    real(8) :: Fc_SO4(Nsed)
#  endif
# endif

    !! Total Flux (:: FT) ---------------------------------- 
    !! unit: (nmol cm-2 s-1)
    real(8) :: FT_Tmp(Nsed+1)
    real(8) :: FT_Sal(Nsed+1)
    real(8) :: FT_O2 (Nsed+1)
    real(8) :: FT_CO2(Nsed+1)
    real(8) :: FT_N2(Nsed+1)
# if defined ORGANIC_MATTER
    real(8) :: FT_DOC(Nsed+1,2)    ! 1:labile, 2:refractory
    real(8) :: FT_POC(Nsed+1,3)    ! 1:labile, 2:refractory 3:non-degratable
# endif
# if defined NUTRIENTS
    real(8) :: FT_NO3(Nsed+1)
    real(8) :: FT_NH4(Nsed+1)
    real(8) :: FT_PO4(Nsed+1)
! #  if defined ORGANIC_MATTER
!     real(8) :: FT_DON(Nsed)
!     real(8) :: FT_PON(Nsed)
!     real(8) :: FT_DOP(Nsed)
!     real(8) :: FT_POP(Nsed)
! #  endif
# endif
# if defined SULFATE
    real(8) :: FT_Mn2(Nsed+1)
    real(8) :: FT_MnO2(Nsed+1)
    real(8) :: FT_Fe2(Nsed+1)
    real(8) :: FT_FeS(Nsed+1)
    real(8) :: FT_FeS2(Nsed+1)
    real(8) :: FT_FeOOH(Nsed+1)
    real(8) :: FT_FeOOH_PO4(Nsed+1)
    real(8) :: FT_H2S(Nsed+1)
    real(8) :: FT_S0(Nsed+1)
    real(8) :: FT_SO4(Nsed+1)
# endif

!! -- Reaction parameters -----------------------------------------
!!!! ============================================================== !!!
    !! ORGANIC MATTER DECOMPOSITION
        ! POMf --> DOMf + DOMs
        ! POMs --> DOMf + DOMs
    !! PRIMARY REACTIONS  (CH2O :: Organic Matters(DOMf,s))
        ! R1:   O2 + CH2O --> CO2 + H2O
        ! R2:   4NO3- + 5CH2O + 4H+ --> N2 + 5CO2 + 7H2O
        ! R3:   2MnO2 + CH2O + 4H+ --> 2Mn2+ + CO2 + 3H2O
        ! R4:   4FeOOH + CH2O + 4H+ --> 4Fe2+ + CO2 + 7H2O
        ! R5:   SO42- + 2CH2O + 2H+ --> H2S + 2CO2 + 2H2O
    !! SECONDARY REACTIONS
        ! R6:   NH4+ + 2O2 --> NO3- + H2O + 2H+
        ! R7:   FeOOH + PO43- --> FeOOH=PO43-
        ! R8:   2Fe2+ + MnO2 + 2H2O --> 2FeOOH + Mn2+ + 2H+
        ! R9:   2Mn2+ + O2 + 2H2O --> 2MnO2 + 4H+
        ! R10a: H2S + 2FeOOH=PO43- + 4H+ --> S0 + 2Fe2+ + 4H2O + 2PO43-
        ! R10b: H2S + 2FeOOH + 4H+ --> S0 + 2Fe2+ + 4H2O
        ! R11:  4Fe2+ + O2 + 6H2O --> 4FeOOH + 8H+
        ! R12:  H2S + MnO2 + 4H+ --> S0 + Mn2+ + 2H2O
        ! R13:  H2S + Fe2+ --> FeS + 2H+
        ! R14:  FeS + S0 --> FeS2
        ! R15:  SO42- + 3H2S + 4FeS + 2H+ --> 4FeS2 + 4H2O
        ! R16:  H2S + 2O2 --> SO42- + 2H+
        ! R17:  FeS + 2O2 --> Fe2+ + SO42-
        ! R18:  2FeS2 + 7O2 + 2H2O --> 2Fe2+ + 4SO42- + 4H+
        ! R19:  4S0 + 4H2O --> 3H2S + SO42- + 2H+
!!!! ============================================================== !!!

    !! Reaction rate constants
    real(8),parameter :: k6  = 2.5d-6  ! For R6  (µM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k6  = 2.5d-7  ! For R6  (µM-1 s-1)           Berg(2003),Kasih2004
    real(8),parameter :: k7  = 5.0d-14 ! For R7  (s-1)                Kasih2009
    ! real(8),parameter :: k7  = 5.0d-11 ! For R7  (s-1)                Fossing2004
    ! real(8),parameter :: k8  = 7.5d-11 ! For R8  (µM-1 s-1)           Berg2003
    real(8),parameter :: k8  = 1.7d-9 ! For R8  (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k9  = 1.5d-5  ! For R9  (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k10 = 2.0d-7  ! For R10 (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k11 = 5.0d-4  ! For R11 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k12 = 3.0d-9  ! For R12 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k13 = 3.75d-5  ! For R13 (µM-1 s-1)           Kasih(2009)
    real(8),parameter :: k14 = 3.0d-10 ! For R14 (cm3 s-1 nmol-1)     Fossing(2004)
    real(8),parameter :: k15 = 7.5d-12 ! For R15 (s-1)                Kasih(2009)
    real(8),parameter :: k16 = 5.0d-5  ! For R16 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k17 = 6.0d-7  ! For R17 (µM-1 s-1)           Fossing(2004)
    ! real(8),parameter :: k18 = 1.6d-8  ! For R18 (µM-1 s-1)           Fossing(2004)
    real(8),parameter :: k18 = 3.0d-10  ! For R18 (µM-1 s-1)           Berg2003, Kasih2009
    real(8),parameter :: k19 = 7.0d-7  ! For R19 (s-1)               Fossing(2004)
    !! non-degratable
    real(8),parameter :: k20 = 1.3d-9     !For R20 (MnO2_ded --> MnO2_undeg) (s-1)    Fossing2004
    real(8),parameter :: k21 = 9.0d-10    !For R21 (FeOOH_ded --> FeOOH_undeg) (s-1)  Fossing2004


    !! ----------- Decomposition rate constant for organic matter ------------- !!
    !! DOM
    ! real(8),parameter :: kdomf = 1.0d-3   ! For  domf (s-1)      Kasih 2009, Irie2018
    real(8),parameter :: kdomf = 1.0d-4   ! For  domf (s-1)           
    ! real(8),parameter :: kdomf = 2.0d-3   ! For  domf (s-1)
    ! real(8),parameter :: kdomf = 1.2d-1   ! For  domf (s-1)

    ! real(8),parameter :: kdoms = 5.0d-9   ! For  doms (s-1)      Kasih 2009
    ! real(8),parameter :: kdoms = 5.0d-8   ! For  doms (s-1)      Irie(2018)
    ! real(8),parameter :: kdoms = 1.0d-8
    ! real(8),parameter :: kdoms = 2.5d-9   ! For  doms (s-1)      
    ! real(8),parameter :: kdoms = 1.0d-7   ! For  doms (s-1)   
    real(8),parameter :: kdoms = 1.4d-10   ! For  doms (s-1)
    ! real(8),parameter :: kdoms = 1.4d-12   ! For  doms (s-1)            

    !! POM
    ! real(8),parameter :: kpomf = 2.5d-6   ! For  pomf (s-1)      Kasih2009, Irie2018
    real(8),parameter :: kpomf = 2.5d-7   ! For  pomf (s-1)
    ! real(8),parameter :: kpomf = 2.5d-8   ! For  pomf (s-1)          
    ! real(8),parameter :: kpomf = 2.0d-9   ! For  pomf (s-1)            
    ! real(8),parameter :: kpomf = 5.0d-10   ! For  pomf (s-1)            

    ! real(8),parameter :: kpoms = 1.2d-10   ! For  poms (s-1)     Kasih2009
    ! real(8),parameter :: kpoms = 6.0d-10   ! For  poms (s-1)     Irie(2018)
    ! real(8),parameter :: kpoms = 6.0d-11   ! For  poms (s-1)      
    ! real(8),parameter :: kpoms = 1.2d-9   ! For  poms (s-1)     
    real(8),parameter :: kpoms = 1.2d-11   ! For  poms (s-1) 
    ! real(8),parameter :: kpoms = 7.0d-12   ! For  poms (s-1)

    !! ------------------------------------------------------------------------- !!


    !! Temperature coefficient Q10
    real(8),parameter :: Q10_pri = 3.8d0   !  For primary reactions               Fossing2004
    real(8),parameter :: Q10_sec = 2.0d0   !  For secondary reactions             Fossing2004

    !! Reaction rate constants considering temperature dependency
    real(8) :: k6_Q10(Nsed),k7_Q10(Nsed),k8_Q10(Nsed),k9_Q10(Nsed),k10_Q10(Nsed)
    real(8) :: k11_Q10(Nsed),k12_Q10(Nsed),k13_Q10(Nsed),k14_Q10(Nsed),k15_Q10(Nsed)
    real(8) :: k16_Q10(Nsed),k17_Q10(Nsed),k18_Q10(Nsed),k19_Q10(Nsed),k20_Q10(Nsed),k21_Q10(Nsed) 
    real(8) :: kdomf_Q10(Nsed),kdoms_Q10(Nsed),kpomf_Q10(Nsed),kpoms_Q10(Nsed)

    !! C:N:P ratio
    ! real(8), parameter :: rCNph = 106.0d0/16.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)
    real(8), parameter :: rCPph = 106.0d0/1.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)
    real(8), parameter :: rCNph = 11.0d0   ! (no dim.) algal C:N ratio (Redfield ratio)        from analysis
    ! real(8), parameter :: rCPph = 80.0d0    ! (no dim.) algal C:P ratio (Redfield ratio)     Fossing2004

    !! OM ratio
    real(8), parameter :: OMFratio = 0.4d0    ! (no dim.) OMf/OM ratio       Irie 0.57 / Kasih0.4 / Fossing 0.42
    real(8), parameter :: OMSratio = 0.3d0    ! (no dim.) OMs/OM ratio   
    real(8), parameter :: OMNratio = 0.3d0    ! (no dim.) OMn/OM ratio       Irie 0.2 / Kasih0.2 / Fossing 0.08
   ! real(8), parameter :: 
 
!! Reaction Rate  -------------------------------------------------------------
    !! unit: (nmol cm-3 s-1)
    real(8) :: vdom(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: vpom(Nsed,2)    ! 1:labile, 2:refractory
    real(8) :: vOMf(Nsed),vOMs(Nsed),vOM(Nsed)
    real(8) :: v1(Nsed),v2(Nsed),v3(Nsed),v4(Nsed),v5(Nsed),v6(Nsed),v7(Nsed),v8(Nsed),v9(Nsed),v10a(Nsed),v10b(Nsed)
    real(8) :: v11(Nsed),v12(Nsed),v13(Nsed),v14(Nsed),v15(Nsed),v16(Nsed),v17(Nsed),v18(Nsed),v19(Nsed),v20(Nsed),v21(Nsed)

    !! Inhibiting concentration of R19
    real(8),parameter :: H2Sstop = 10d0  ! a certain limit of [H2S] in R19 (µM) (fossing et al. 2004)

    !! Concentration limit
    real(8),parameter :: O2lim    = 20.0d0    !(µmol l-1)     Fossing2004
    real(8),parameter :: NO3lim   = 5.0d0     !(µmol l-1)     Fossing2004
    real(8),parameter :: MnO2lim  = 50000d0   !(nmol g-1)     Fossing2004
    real(8),parameter :: FeOOHlim = 100000d0  !(nmol g-1)     Fossing2004


!! Net Production and Consumption  -------------------------------------------
    !! unit: (nmol cm-3 s-1)

    real(8) :: R_O2 (Nsed), R_CO2(Nsed), R_N2(Nsed)
# if defined ORGANIC_MATTER
    real(8) :: R_DOC(Nsed,2), R_POC(Nsed,2)    ! 1:labile, 2:refractory
# endif
# if defined NUTRIENTS
    real(8) :: R_NO3(Nsed),R_NH4(Nsed),R_PO4(Nsed)
! #  if defined ORGANIC_MATTER
!     real(8) :: R_DON(Nsed)
!     real(8) :: R_PON(Nsed)
!     real(8) :: R_DOP(Nsed)
!     real(8) :: R_POP(Nsed)
! #  endif
# endif
# if defined SULFATE
    real(8) :: R_Mn2(Nsed), R_MnO2(Nsed)
    real(8) :: R_Fe2(Nsed), R_FeS(Nsed), R_FeS2(Nsed), R_FeOOH(Nsed), R_FeOOH_PO4(Nsed)
    real(8) :: R_H2S(Nsed), R_S0(Nsed), R_SO4(Nsed)
# endif


!!  Output ------------------------------------------------------------------
# if defined SEDIMENT_TESTMODE
    real(8), parameter :: OUTPUT_INTERVAL = 1440.0d0 !*365     ! Output interval (min)
    real(8), save :: time = 0.d0 !sec
    real(8), save :: dsec = 0.d0 !sec
    real(8), save :: year = 0.0d0 
    real(8) :: pHamb 
    real(8) :: redoxlevel(Nsed)    ! for debug / 1:O2, 2:NO3, 3:MnO2, 4:FeOOH, 5:SO4
# endif

! ============================================================================================================== !
!  STRUCTURE OF SEDIMENT COLUMN
!
!                 |   Layer     |    Layer    |       Depth       |      Concentration        |    Flux
!                 |   number    |  thickness  |                   |                           |
!
!                    --------------------------------------------------------------------------------------------
!                       k             dz                z              SEDECO(ng)%C(i,j,k)         FT_C
!                                    (cm)              (cm)           (µmol L-1),(nmol g-1)    (nmol cm-2 s-1)
!        (i,j)       --------------------------------------------------------------------------------------------
!       ___|______     DBL                               0                     Camb             ↓ FT(1)     ←diffusion in DBL  (Dw)
!     /____|____ /| 
!     |         | |
!     |   sed   | |     1           dz(1)               0.03                   C(1)              
!     |_________|/|                                                                        ---   ↓ FT(2)     ←diffusion in sediment  (Ds)
!     |         | |
!     |   sed   | |     2           dz(2)         z(2)=z(1)+dz(2)              C(2)              
!     |_________|/                                                                         ---   ↓ FT(3)     ←diffusion in sediment (Ds)   
!          ...
!      _________ /| 
!     |         | |
!     |   sed   | |    k-1         dz(k-1)       z(k-1)=z(k-2)+dz(k-1)        C(k-1)
!     |_________|/|                                                                        ---   ↓ FT(k)   
!     |         | |
!     |   sed   | |     k           dz(k)         z(k)=z(k-1)+dz(k)            C(k)
!     |_________|/|                                                                        ---   ↓ FT(k+1)   
!     |         | |
!     |   sed   | |    k+1         dz(k+1)       z(k+1)=z(k)+dz(k+1)          C(k+1)
!     |_________|/                                                                         ---   ↓ FT(k+2)   
!          ...
!      _________ /| 
!     |         | |
!     |   sed   | |   Nsed-1      dz(Nsed-1)                                  C(Nsed-1)
!     |_________|/|                                                                        ---   ↓ FT(Nsed)   (Fd, Fb == 0 (closed) / Fc ~= 0 (open))
!     |         | |
!     |   sed   | |     Nsed        dz(Nsed)     z(Nsed)=z(Nsed-1)+dz(Nsed)    C(Nsed)
!     |_________|/                                                                         ---   
!
!                                                                                   [ ***  Flux of layer(k)  *** ]
!                                                                                   [     Flux IN :  FT(K)       ]
!                                                                                   [     Flux OUT:  FT(k+1)     ]
!                                                                                   [  FT: Flux Total (Fd,Fb,Fc) ]
! ============================================================================================================== !


! ***********************************************************************************
!!! 実行文 executable statement

! ***************************** !
!!! physical properties of sediment layers 
! ***************************** !

    dz = SEDECO(ng)%dz(:)
    z  = SEDECO(ng)%z(:)

    !! soil density (This must be given as an input data actually)
    rhos_d(:) = 2.5d0 ! (g/cm3)

! ***************************** !
!!! initial Temperature and Salinity 
! ***************************** !
    !! set the same temp and sal for all of layers as water column

    SEDECO(ng)%Tmp(i,j,:) = Tamb
    SEDECO(ng)%Sal(i,j,:) = Samb

! ***************************** !


! ***************************** !
!!!  Boundary condition
! ***************************** !
!!!  Upper boundary is defined as constant concentrations in water column in main.F
!!!  Bottom boundary (nmol cm-2 s-1) --------------------------
!    Closed boundary condition 
# if defined sedBC_closed
!! set boundary-1 to boundary and calculate below vvv in ficks law to get zero diffusion
    SEDECO(ng)%Tmp(i,j,Nsed) = SEDECO(ng)%Tmp(i,j,Nsed-1) 
    SEDECO(ng)%Sal(i,j,Nsed) = SEDECO(ng)%Sal(i,j,Nsed-1)

    SEDECO(ng)%O2(i,j,Nsed) = SEDECO(ng)%O2(i,j,Nsed-1)
    SEDECO(ng)%CO2(i,j,Nsed) = SEDECO(ng)%CO2(i,j,Nsed-1)
    SEDECO(ng)%N2(i,j,Nsed) = SEDECO(ng)%N2(i,j,Nsed-1)
#  if defined ORGANIC_MATTER
    SEDECO(ng)%DOCf(i,j,Nsed) = SEDECO(ng)%DOCf(i,j,Nsed-1)
    SEDECO(ng)%DOCs(i,j,Nsed) = SEDECO(ng)%DOCs(i,j,Nsed-1)
    SEDECO(ng)%POCf(i,j,Nsed) = SEDECO(ng)%POCf(i,j,Nsed-1)
    SEDECO(ng)%POCs(i,j,Nsed) = SEDECO(ng)%POCs(i,j,Nsed-1)
    SEDECO(ng)%POCn(i,j,Nsed) = SEDECO(ng)%POCn(i,j,Nsed-1)
#  endif
#  if defined NUTRIENTS
    SEDECO(ng)%NO3(i,j,Nsed)  = SEDECO(ng)%NO3(i,j,Nsed-1)
    SEDECO(ng)%NH4(i,j,Nsed) = SEDECO(ng)%NH4(i,j,Nsed-1)
    SEDECO(ng)%PO4(i,j,Nsed) = SEDECO(ng)%PO4(i,j,Nsed-1)
! #   if defined ORGANIC_MATTER
!     SEDECO(ng)%DON(i,j,Nsed) = SEDECO(ng)%DON(i,j,Nsed-1)
!     SEDECO(ng)%PON(i,j,Nsed) = SEDECO(ng)%PON(i,j,Nsed-1)
!     SEDECO(ng)%DOP(i,j,Nsed) = SEDECO(ng)%DOP(i,j,Nsed-1)
!     SEDECO(ng)%POP(i,j,Nsed) = SEDECO(ng)%POP(i,j,Nsed-1)
! #   endif
#  endif
#  if defined SULFATE
    SEDECO(ng)%Mn2(i,j,Nsed)  = SEDECO(ng)%Mn2(i,j,Nsed-1)
    SEDECO(ng)%MnO2(i,j,Nsed) = SEDECO(ng)%MnO2(i,j,Nsed-1)

    SEDECO(ng)%Fe2(i,j,Nsed)       = SEDECO(ng)%Fe2(i,j,Nsed-1)
    SEDECO(ng)%FeS(i,j,Nsed)       = SEDECO(ng)%FeS(i,j,Nsed-1)
    SEDECO(ng)%FeS2(i,j,Nsed)      = SEDECO(ng)%FeS2(i,j,Nsed-1)
    SEDECO(ng)%FeOOH(i,j,Nsed)     = SEDECO(ng)%FeOOH(i,j,Nsed-1)
    SEDECO(ng)%FeOOH_PO4(i,j,Nsed) = SEDECO(ng)%FeOOH_PO4(i,j,Nsed-1)

    SEDECO(ng)%H2S(i,j,Nsed) = SEDECO(ng)%H2S(i,j,Nsed-1)
    SEDECO(ng)%S0(i,j,Nsed)  = SEDECO(ng)%S0(i,j,Nsed-1)
    SEDECO(ng)%SO4(i,j,Nsed) = SEDECO(ng)%SO4(i,j,Nsed-1)
#  endif
# endif    


! ***************************** !
!          Diffusion
! ***************************** !

    !! ----- for sediment column ----- !!
    DO k = 1,Nsed
        Dw_O2(k)   = (11.7d0 + 0.344d0*SEDECO(ng)%Tmp(i,j,k) + 0.00505d0*SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6)
# if defined ORGANIC_MATTER
        Dw_DOC(k,1) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k)) *(1.0d-7) ! DOCf
        Dw_DOC(k,2) = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k)) *(1.0d-9) ! DOCs
# endif
# if defined NUTRIENTS
        Dw_NO3(k)  = (9.72d0 + 0.365d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_NH4(k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_PO4(k)  = (9.76d0 + 0.398d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
! #  if defined ORGANIC_MATTER
        ! Dw_DON(k) = 
        ! Dw_DOP(k) = 
! #  endif 
# endif
# if defined SULFATE
        Dw_Mn2(k)  = (3.04d0 + 0.153d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_Fe2(k)  = (3.36d0 + 0.148d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_SO4(k)  = (4.96d0 + 0.226d0*SEDECO(ng)%Tmp(i,j,k))  *(1.0d-6)
        Dw_H2S(k)  = (8.74d0 + 0.264d0*SEDECO(ng)%Tmp(i,j,k) + 0.004d0*SEDECO(ng)%Tmp(i,j,k)**2d0) *(1.0d-6)
# endif
    ENDDO

    !! --- Diffusion coefficient in sediment (function Dsed is defined in mod_geochem.F) --- !!
    DO k = 2,Nsed
        Ds_O2(k)   = Dsed(Dw_O2(k),SEDECO(ng)%poro(i,j,k))
# if defined ORGANIC_MATTER
        Ds_DOC(k,1) = Dsed(Dw_DOC(k,1),SEDECO(ng)%poro(i,j,k)) ! DOCf
        Ds_DOC(k,2) = Dsed(Dw_DOC(k,2),SEDECO(ng)%poro(i,j,k)) ! DOCs
# endif
# if defined NUTRIENTS
        Ds_NO3(k)  = Dsed(Dw_NO3(k),SEDECO(ng)%poro(i,j,k))
        Ds_NH4(k)  = Dsed(Dw_NH4(k),SEDECO(ng)%poro(i,j,k))
        Ds_PO4(k)  = Dsed(Dw_PO4(k),SEDECO(ng)%poro(i,j,k))
! #  if defined ORGANIC_MATTER
!         Ds_DON(k)  = Dsed(Dw_DOC(k,1),SEDECO(ng)%poro(i,j,k))
!         Ds_DOP(k)  = Dsed(Dw_DOC(k,1),SEDECO(ng)%poro(i,j,k))
! #  endif
# endif
# if defined SULFATE
        Ds_Mn2(k)  = Dsed(Dw_Mn2(k),SEDECO(ng)%poro(i,j,k))
        Ds_Fe2(k)  = Dsed(Dw_Fe2(k),SEDECO(ng)%poro(i,j,k))
        Ds_SO4(k)  = Dsed(Dw_SO4(k),SEDECO(ng)%poro(i,j,k))
        Ds_H2S(k)  = Dsed(Dw_H2S(k),SEDECO(ng)%poro(i,j,k))
# endif
    ENDDO

!!!  (0) DBL: Water-sediment fluxes (nmol cm-2 s-1) -------------------------
    ! (0) --> (1)

    rho_sw(1) = densSW( Tamb,Samb )  ! Density of seawater [g/cm3]     ! function densSW is defined in mod_geochem
!     Fd_Tmp(1) = Diff*( Tamb  - SEDECO(ng)%Tmp(i,j,1) )/t_dbl
!     Fd_Sal(1) = Diff*( Samb  - SEDECO(ng)%Sal(i,j,1) )/t_dbl
    Fd_O2(1) = Ficks(Dw_O2(1),O2amb,SEDECO(ng)%O2(i,j,1),dbl)
# if defined ORGANIC_MATTER
    Fd_DOC(1,1) = Ficks(Dw_DOC(1,1),DOCfamb,SEDECO(ng)%DOCf(i,j,1),dbl)
    Fd_DOC(1,2) = Ficks(Dw_DOC(1,2),DOCsamb,SEDECO(ng)%DOCs(i,j,1),dbl)
# endif
# if defined NUTRIENTS
    Fd_NO3(1) = Ficks(Dw_NO3(1),NO3amb,SEDECO(ng)%NO3(i,j,1),dbl)
    Fd_NH4(1) = Ficks(Dw_NH4(1),NH4amb,SEDECO(ng)%NH4(i,j,1),dbl)
    Fd_PO4(1) = Ficks(Dw_PO4(1),PO4amb,SEDECO(ng)%PO4(i,j,1),dbl)
! #  if defined ORGANIC_MATTER
!     Fd_DON(1) = Ficks(Dw_DOC(1,1),DONamb,SEDECO(ng)%DON(i,j,1),dbl)
!     Fd_DOP(1) = Ficks(Dw_DOC(1,1),DOPamb,SEDECO(ng)%DOP(i,j,1),dbl)
! #  endif
# endif
# if defined SULFATE
    Fd_Mn2(1) = Ficks(Dw_Mn2(1),Mn2amb,SEDECO(ng)%Mn2(i,j,1),dbl)
    Fd_Fe2(1) = Ficks(Dw_Fe2(1),Fe2amb,SEDECO(ng)%Fe2(i,j,1),dbl)
    Fd_H2S(1) = Ficks(Dw_H2S(1),H2Samb,SEDECO(ng)%H2S(i,j,1),dbl)
    Fd_SO4(1) = Ficks(Dw_SO4(1),SO4amb,SEDECO(ng)%SO4(i,j,1),dbl)
# endif


    !! (1): Molecular Diffusion Flux calculation (Only for solutes) ------------
    DO k = 2,Nsed
        ! function densSW is defined in mod_geochem
        rho_sw(k) = densSW( SEDECO(ng)%Tmp(i,j,k),SEDECO(ng)%Sal(i,j,k) )  ! Density of seawater [g/cm3]   
        ! Fd_Tmp(k) = Diff*( Tamb  - SEDECO(ng)%Tmp(i,j,1) )/t_dbl
        ! Fd_Sal(k) = Diff*( Samb  - SEDECO(ng)%Sal(i,j,1) )/t_dbl
        Fd_O2(k)    = Ficks_sedd(Ds_O2(k),SEDECO(ng)%O2(i,j,k-1),SEDECO(ng)%O2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# if defined ORGANIC_MATTER
        Fd_DOC(k,1) = Ficks_sedd(Ds_DOC(k,1),SEDECO(ng)%DOCf(i,j,k-1),SEDECO(ng)%DOCf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOCf
        Fd_DOC(k,2) = Ficks_sedd(Ds_DOC(k,2),SEDECO(ng)%DOCs(i,j,k-1),SEDECO(ng)%DOCs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOCs
# endif
# if defined NUTRIENTS
        Fd_NO3(k)   = Ficks_sedd(Ds_NO3(k),SEDECO(ng)%NO3(i,j,k-1),SEDECO(ng)%NO3(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_NH4(k)   = Ficks_sedd(Ds_NH4(k),SEDECO(ng)%NH4(i,j,k-1),SEDECO(ng)%NH4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_PO4(k)   = Ficks_sedd(Ds_PO4(k),SEDECO(ng)%PO4(i,j,k-1),SEDECO(ng)%PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
! #  if defined ORGANIC_MATTER
!         Fd_DON(k)   = Ficks_sedd(Ds_DON(k),SEDECO(ng)%DON(i,j,k-1),SEDECO(ng)%DON(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DON
!         Fd_DON(k)   = Ficks_sedd(Ds_DOP(k),SEDECO(ng)%DOP(i,j,k-1),SEDECO(ng)%DOP(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k)) ! DOP
! #  endif
# endif
# if defined SULFATE
        Fd_Mn2(k)   = Ficks_sedd(Ds_Mn2(k),SEDECO(ng)%Mn2(i,j,k-1),SEDECO(ng)%Mn2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_Fe2(k)   = Ficks_sedd(Ds_Fe2(k),SEDECO(ng)%Fe2(i,j,k-1),SEDECO(ng)%Fe2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_H2S(k)   = Ficks_sedd(Ds_H2S(k),SEDECO(ng)%H2S(i,j,k-1),SEDECO(ng)%H2S(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fd_SO4(k)   = Ficks_sedd(Ds_SO4(k),SEDECO(ng)%SO4(i,j,k-1),SEDECO(ng)%SO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# endif
     ENDDO

!!!  (2): Bio-diffusion flux in sediment column (nmol cm-2 s-1) (For solutes and solids) ------------
     DO k = 1,Nsed
        !! determine DBd and DBp depending on z
        if (z(k) <= 11.8d0 )then
            DBd(k) = 3.51d-6
        else
            DBd(k) = 3.51d-6 * exp(-0.378d0*(z(k)-11.8d0))
        endif
        DBp(k) = DBd(k)/9.3d0
     ENDDO

     DO k = 2,Nsed
        ! Fb_Tmp(k) = Diff*( Tamb  - SEDECO(ng)%Tmp(i,j,1) )/t_dbl
        ! Fb_Sal(k) = Diff*( Samb  - SEDECO(ng)%Sal(i,j,1) )/t_dbl
        Fb_O2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%O2(i,j,k-1),SEDECO(ng)%O2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# if defined ORGANIC_MATTER
        Fb_DOC(k,1) = Ficks_sedd(DBd(k),SEDECO(ng)%DOCf(i,j,k-1),SEDECO(ng)%DOCf(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_DOC(k,2) = Ficks_sedd(DBd(k),SEDECO(ng)%DOCs(i,j,k-1),SEDECO(ng)%DOCs(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_POC(k,1) = Ficks_sedp(DBp(k),SEDECO(ng)%POCf(i,j,k-1),SEDECO(ng)%POCf(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_POC(k,2) = Ficks_sedp(DBp(k),SEDECO(ng)%POCs(i,j,k-1),SEDECO(ng)%POCs(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_POC(k,3) = Ficks_sedp(DBp(k),SEDECO(ng)%POCn(i,j,k-1),SEDECO(ng)%POCn(i,j,k),dz(k),&
                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
# endif
# if defined NUTRIENTS
        Fb_NO3(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%NO3(i,j,k-1),SEDECO(ng)%NO3(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_NH4(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%NH4(i,j,k-1),SEDECO(ng)%NH4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_PO4(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%PO4(i,j,k-1),SEDECO(ng)%PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
! #  if defined ORGANIC_MATTER
!         Fb_DON(k) = Ficks_sedd(DBd,SEDECO(ng)%DON(i,j,k-1),SEDECO(ng)%DON(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
!         Fb_PON(k) = Ficks_sedp(DBp,SEDECO(ng)%PON(i,j,k-1),SEDECO(ng)%PON(i,j,k),dz(k),&
!                                SEDECO(ng)%poro(i,j,k-1),rhos_d(k))
!         Fb_DOP(k) = Ficks_sedd(DBd,SEDECO(ng)%DOP(i,j,k-1),SEDECO(ng)%DOP(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
!         Fb_POP(k) = Ficks_sedp(DBp,SEDECO(ng)%POP(i,j,k-1),SEDECO(ng)%POP(i,j,k),dz(k),&
!                                SEDECO(ng)%poro(i,j,k),rhos_d(k))
! #  endif
# endif
# if defined SULFATE
        Fb_Mn2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%Mn2(i,j,k-1),SEDECO(ng)%Mn2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_MnO2(k)  = Ficks_sedp(DBp(k),SEDECO(ng)%MnO2(i,j,k-1),SEDECO(ng)%MnO2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))

        Fb_Fe2(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%Fe2(i,j,k-1),SEDECO(ng)%Fe2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_FeS(k)   = Ficks_sedp(DBp(k),SEDECO(ng)%FeS(i,j,k-1),SEDECO(ng)%FeS(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_FeS2(k)  = Ficks_sedp(DBp(k),SEDECO(ng)%FeS2(i,j,k-1),SEDECO(ng)%FeS2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_FeOOH(k) = Ficks_sedp(DBp(k),SEDECO(ng)%FeOOH(i,j,k-1),SEDECO(ng)%FeOOH(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_FeOOH_PO4(k) &
          = Ficks_sedp(DBp(k),SEDECO(ng)%FeOOH_PO4(i,j,k-1),SEDECO(ng)%FeOOH_PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))

        Fb_H2S(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%H2S(i,j,k-1),SEDECO(ng)%H2S(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
        Fb_S0(k)    = Ficks_sedp(DBp(k),SEDECO(ng)%S0(i,j,k-1),SEDECO(ng)%S0(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_SO4(k)   = Ficks_sedd(DBd(k),SEDECO(ng)%SO4(i,j,k-1),SEDECO(ng)%SO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k))
# endif
     ENDDO

!!! (3) Adsorption (nmol cm-2 s-1) ------------------------------------------
# if defined adsorption
     DO k = 2,Nsed
#  if defined NUTRIENTS
        Fb_NO3(k)   = Fb_NO3(k) + Ficks_sedp(DBp(k) * Kad_NO3,SEDECO(ng)%NO3(i,j,k-1), &
                                            SEDECO(ng)%NO3(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_NH4(k)   = Fb_NH4(k) + Ficks_sedp(DBp(k) * Kad_NH4,SEDECO(ng)%NH4(i,j,k-1), &
                                            SEDECO(ng)%NH4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_PO4(k)   = Fb_PO4(k) + Ficks_sedp(DBp(k) * Kad_PO4,SEDECO(ng)%PO4(i,j,k-1), &
                                            SEDECO(ng)%PO4(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
#  endif
#  if defined SULFATE
        Fb_Mn2(k)   = Fb_Mn2(k) + Ficks_sedp(DBp(k) * Kad_Mn2,SEDECO(ng)%Mn2(i,j,k-1), & 
                                            SEDECO(ng)%Mn2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
        Fb_Fe2(k)   = Fb_Fe2(k) + Ficks_sedp(DBp(k) * Kad_Fe2,SEDECO(ng)%Fe2(i,j,k-1),& 
                                            SEDECO(ng)%Fe2(i,j,k),dz(k),SEDECO(ng)%poro(i,j,k),rhos_d(k))
#  endif
     ENDDO
# endif



! ***************************** !
!!!     Burial term
! ***************************** !
# if defined Burial
    !! Burial Flux calculation 
    DO k = 2,Nsed
        ! Fc_Tmp(k) = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%O2(i,j,k))
        ! Fc_Sal(k) = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%O2(i,j,k))
        Fc_O2(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%O2(i,j,k))
#  if defined ORGANIC_MATTER
        Fc_DOC(k,1)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOCf(i,j,k))
        Fc_DOC(k,2)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOCs(i,j,k))
        Fc_POC(k,1)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POCf(i,j,k),rhos_d(k))
        Fc_POC(k,2)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POCs(i,j,k),rhos_d(k))
        Fc_POC(k,3)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POCn(i,j,k),rhos_d(k))
#  endif
#  if defined NUTRIENTS
        Fc_NO3(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%NO3(i,j,k))
        Fc_NH4(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%NH4(i,j,k))
        Fc_PO4(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%PO4(i,j,k))
! #   if defined ORGANIC_MATTER
!         Fc_DON(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DON(i,j,k))
!         Fc_PON(k)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%PON(i,j,k),rhos_d(k))
!         Fc_DOP(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%DOP(i,j,k))
!         Fc_POP(k)   = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%POP(i,j,k),rhos_d(k))
! #   endif
#  endif
#  if defined SULFATE
        Fc_Mn2(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%Mn2(i,j,k))
        Fc_MnO2(k)  = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%MnO2(i,j,k),rhos_d(k))

        Fc_Fe2(k)       = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%Fe2(i,j,k))
        Fc_FeS(k)       = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeS(i,j,k),rhos_d(k))
        Fc_FeS2(k)      = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeS2(i,j,k),rhos_d(k))
        Fc_FeOOH(k)     = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeOOH(i,j,k),rhos_d(k))
        Fc_FeOOH_PO4(k) = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%FeOOH_PO4(i,j,k),rhos_d(k))

        Fc_H2S(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%H2S(i,j,k))
        Fc_S0(k)    = FCp(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%S0(i,j,k),rhos_d(k))
        Fc_SO4(k)   = FCd(SEDECO(ng)%poro(i,j,k),u,SEDECO(ng)%SO4(i,j,k))
#  endif
#  if defined adsorption
        Fc_NO3(k) = Fc_NO3(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_NO3),SEDECO(ng)%NO3(i,j,k),rhos_d(k))
        Fc_NH4(k) = Fc_NH4(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_NH4),SEDECO(ng)%NH4(i,j,k),rhos_d(k))
        Fc_PO4(k) = Fc_PO4(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_PO4),SEDECO(ng)%PO4(i,j,k),rhos_d(k))
        Fc_Mn2(k) = Fc_Mn2(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_Mn2),SEDECO(ng)%Mn2(i,j,k),rhos_d(k))
        Fc_Fe2(k) = Fc_Fe2(k) + FCp(SEDECO(ng)%poro(i,j,k),(u * Kad_Fe2),SEDECO(ng)%Fe2(i,j,k),rhos_d(k))
#  endif
    ENDDO
# endif



! ***************************** !
!!!    Reaction term
! ***************************** !

    !! Reaction rate constants depending on temperature
    DO k = 1,Nsed
        k6_Q10(k)  = k6  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k7_Q10(k)  = k7  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Berg2003)      (s-1)
        k8_Q10(k)  = k8  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)
        k9_Q10(k)  = k9  * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k10_Q10(k) = k10 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)
        k11_Q10(k) = k11 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k12_Q10(k) = k12 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k13_Q10(k) = k13 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)
        k14_Q10(k) = k14 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k15_Q10(k) = k15 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (G.A.Anggra2009)(s-1)
        k16_Q10(k) = k16 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k17_Q10(k) = k17 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k18_Q10(k) = k18 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)
        k19_Q10(k) = k19 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)   (s-1)
        k20_Q10(k) = k20 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)   (s-1)
        k21_Q10(k) = k21 * (Q10_sec**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))     ! (Fossing2004)   (s-1)
        
        kdomf_Q10(k) = kdomf * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! fast  
        kdoms_Q10(k) = kdoms * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! slow  
        kpomf_Q10(k) = kpomf * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! fast  
        kpoms_Q10(k) = kpoms * (Q10_pri**((SEDECO(ng)%Tmp(i,j,k)-9d0)/10d0))  ! slow  
    ENDDO
    

    !! Reaction rate calculatoin (functions Vdd, Vsd, Vss are defined in mod_geochem.F) -- !!
        ! Vdd : reaction with solute * solute (R6, ...)
        ! Vsd : reaction with solid * solute  (R7, ...)
        ! Vss : reaction with solid * solid   (R14)
    !! ----------------------------------------------------------------------------------- !!
    DO k = 1,Nsed
        vpom(k,1) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpomf_Q10(k) * SEDECO(ng)%POCf(i,j,k)  ! For POC_fast                               ! For DOC_slow
        vpom(k,2) = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * kpoms_Q10(k) * SEDECO(ng)%POCs(i,j,k)  ! For POC_slow
        vdom(k,1) = SEDECO(ng)%poro(i,j,k) * kdomf_Q10(k) * SEDECO(ng)%DOCf(i,j,k)                                ! For DOC_fast
        vdom(k,2) = SEDECO(ng)%poro(i,j,k) * kdoms_Q10(k) * SEDECO(ng)%DOCs(i,j,k)                                ! For DOC_slow
        vOM(k)   = vdom(k,1) + vdom(k,2)
        v1(k)   = vOM(k)
        v2(k)   = 0d0
        v3(k)   = 0d0
        v4(k)   = 0d0
        v5(k)   = 0d0
        v6(k)   = Vdd(k6_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%NH4(i,j,k),      SEDECO(ng)%O2(i,j,k)            )
        v7(k)   = Vsd(k7_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%FeOOH(i,j,k),    SEDECO(ng)%PO4(i,j,k), rhos_d(k))
        v8(k)   = Vsd(k8_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%MnO2(i,j,k),     SEDECO(ng)%Fe2(i,j,k), rhos_d(k))
        v9(k)   = Vdd(k9_Q10(k),SEDECO(ng)%poro(i,j,k),  SEDECO(ng)%O2(i,j,k),       SEDECO(ng)%Mn2(i,j,k)           )
        v10a(k) = Vsd(k10_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeOOH_PO4(i,j,k),SEDECO(ng)%H2S(i,j,k), rhos_d(k))
        v10b(k) = Vsd(k10_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeOOH(i,j,k),    SEDECO(ng)%H2S(i,j,k), rhos_d(k))
        v11(k)  = Vdd(k11_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%Fe2(i,j,k),      SEDECO(ng)%O2(i,j,k)            )
        v12(k)  = Vsd(k12_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%MnO2(i,j,k),     SEDECO(ng)%H2S(i,j,k), rhos_d(k))
        v13(k)  = Vdd(k13_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%Fe2(i,j,k),      SEDECO(ng)%H2S(i,j,k)           )
        v14(k)  = Vss(k14_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS(i,j,k),      SEDECO(ng)%S0(i,j,k),  rhos_d(k))
        v15(k)  = Vsd(k15_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS(i,j,k),      SEDECO(ng)%H2S(i,j,k), rhos_d(k)) 
        v16(k)  = Vdd(k16_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%H2S(i,j,k),      SEDECO(ng)%O2(i,j,k)            )
        v17(k)  = Vsd(k17_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS(i,j,k),      SEDECO(ng)%O2(i,j,k),  rhos_d(k))
        v18(k)  = Vsd(k18_Q10(k),SEDECO(ng)%poro(i,j,k), SEDECO(ng)%FeS2(i,j,k),     SEDECO(ng)%O2(i,j,k),  rhos_d(k))
        if (SEDECO(ng)%H2S(i,j,k) <= H2Sstop) then  
           v19(k)  = Vsd(k19_Q10(k),SEDECO(ng)%poro(i,j,k),SEDECO(ng)%S0(i,j,k),(1d0 - (SEDECO(ng)%H2S(i,j,k)/H2Sstop)),rhos_d(k))
        else
           v19(k)  = 0d0
        endif
        v20(k)  = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * k20_Q10(k) * SEDECO(ng)%MnO2(i,j,k) 
        v21(k)  = rhos_d(k) * (1d0 - SEDECO(ng)%poro(i,j,k)) * k21_Q10(k) * SEDECO(ng)%FeOOH(i,j,k) 
        

# if defined sedORP
        !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
        !! redox stages change depending on ORP input
        !! create condition of it here
        !! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !!
            !! ギブスエネルギーで反応が変化するようにするべきだ
# else

        !! Redox concentration limit in organic matter degradation (Berg et al.2003)
        !! -- Denitrification (NO3) (µmol L-1)--!!
        redoxlevel(k) = 1d0 !!for debug

        if (SEDECO(ng)%O2(i,j,k)  < O2lim .and. &
            SEDECO(ng)%NO3(i,j,k) > NO3lim) then
            v1(k) = vOM(k) * (SEDECO(ng)%O2(i,j,k) / O2lim)
            v2(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim)
            redoxlevel(k) = 2d0 !!for debug
        endif
        !! -- Manganese reduction (MnO2) (nmol g-1)--!!
        if (SEDECO(ng)%O2(i,j,k)  < O2lim .and. &
            SEDECO(ng)%NO3(i,j,k) < NO3lim .and. &
            SEDECO(ng)%MnO2(i,j,k) > MnO2lim)  then
            v1(k) = vOM(k) * (SEDECO(ng)%O2(i,j,k) / O2lim)
            v2(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                             * (SEDECO(ng)%NO3(i,j,k) / NO3lim)
            v3(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                             * (1d0 - SEDECO(ng)%NO3(i,j,k) / NO3lim) &
                             * (SEDECO(ng)%MnO2(i,j,k) / MnO2lim)
            redoxlevel(k) = 3d0 !!for debug
        endif
        !! -- Iron reduction (FeOOH) (nmol g-1)--!!
        if (SEDECO(ng)%O2(i,j,k)  < O2lim .and. &
            SEDECO(ng)%NO3(i,j,k) < NO3lim .and. &
            SEDECO(ng)%MnO2(i,j,k) < MnO2lim .and. &
            SEDECO(ng)%FeOOH(i,j,k) > FeOOHlim)  then
            v1(k) = vOM(k) * (SEDECO(ng)%O2(i,j,k) / O2lim)
            v2(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                           * (SEDECO(ng)%NO3(i,j,k) / NO3lim)
            v3(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                           * (1d0 - SEDECO(ng)%NO3(i,j,k) / NO3lim) &
                           * (SEDECO(ng)%MnO2(i,j,k) / MnO2lim)
            v4(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                           * (1d0 - SEDECO(ng)%NO3(i,j,k) / NO3lim) &
                           * (1d0 - SEDECO(ng)%MnO2(i,j,k) / MnO2lim) &
                           * (SEDECO(ng)%FeOOH(i,j,k) / FeOOHlim)
            redoxlevel(k) = 4d0 !!for debug
        endif
        !! -- Sulfate reduction (SO4) (µmol L-1)--!!
        if (SEDECO(ng)%O2(i,j,k)  < O2lim .and. &
            SEDECO(ng)%NO3(i,j,k) < NO3lim .and. &
            SEDECO(ng)%MnO2(i,j,k) < MnO2lim .and. &
            SEDECO(ng)%FeOOH(i,j,k) < FeOOHlim )  then
            v1(k) = vOM(k) * (SEDECO(ng)%O2(i,j,k) / O2lim)
            v2(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                           * (SEDECO(ng)%NO3(i,j,k) / NO3lim)
            v3(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                           * (1d0 - SEDECO(ng)%NO3(i,j,k) / NO3lim) &
                           * (SEDECO(ng)%MnO2(i,j,k) / MnO2lim)
            v4(k) = vOM(k) * (1d0 - SEDECO(ng)%O2(i,j,k) / O2lim) &
                           * (1d0 - SEDECO(ng)%NO3(i,j,k) / NO3lim) &
                           * (1d0 - SEDECO(ng)%MnO2(i,j,k) / MnO2lim) &
                           * (SEDECO(ng)%FeOOH(i,j,k) / FeOOHlim)
            v5(k) = vOM(k) - v1(k) -v2(k) -v3(k) - v4(k)
            redoxlevel(k) = 5d0 !!for debug
        endif
        !! stop OM decomposition if SEDECO(ng)%SO4(i,j,k) == 0
        if (SEDECO(ng)%SO4(i,j,k) <= 0d0 )then
            v5(k) = 0d0
        endif
# endif
    ENDDO


    !! Net Production and Consumption from Reactions
    R_O2 (:) = -v1(:) -2d0*v6(:) -v9(:) -v11(:) -2d0*v16(:) -2d0*v17(:) -7d0*v18(:)
    R_CO2(:) = v1(:) + v2(:) +v3(:) +v4(:) + v5(:)
    R_N2(:)  = 0.2d0*v2(:)
# if defined ORGANIC_MATTER
    R_DOC(:,1) = -vdom(:,1) + OMFratio*vpom(:,1) + OMSratio*vpom(:,2)! DOCf
    R_DOC(:,2) = -vdom(:,2) + OMFratio*vpom(:,1) + OMSratio*vpom(:,2) ! DOCs
    R_POC(:,1) = -vpom(:,1) ! POCf
    R_POC(:,2) = -vpom(:,2) ! POCs
# endif
# if defined NUTRIENTS
    R_NO3(:) = -0.8d0*v2(:) + v6(:)
    R_NH4(:) = (1d0/rCNph)*vOM(:) -v6(:)
    R_PO4(:) = (1d0/rCPph)*vOM(:) -v7(:) +2d0*v10a(:)
! #  if defined ORGANIC_MATTER
!     R_DON(:) = 1d0/rCNph*R_DOC(:,1)
!     R_PON(:) = 1d0/rCNph*R_POC(:,1)
!     R_DOP(:) = 1d0/rCPph*R_DOC(:,1)
!     R_POP(:) = 1d0/rCPph*R_POC(:,1)
! #  endif
# endif
# if defined SULFATE
    R_Mn2(:)  = 2d0*v3(:) +v8(:) -2d0*v9(:) +v12(:)
    R_MnO2(:) = -2d0*v3(:) -v8(:) +2d0*v9(:) -v12(:) -v20(:)
    R_Fe2(:)       = 4d0*v4(:) -2d0*v8(:) +2d0*v10a(:) -4d0*v11(:) +2d0*v10b(:) -v13(:) +v17(:) +2d0*v18(:) 
    R_FeS(:)       = v13(:) -v14(:) -4d0*v15(:) -v17(:)
    R_FeS2(:)      = v14(:) +4d0*v15(:) -2d0*v18(:)
    R_FeOOH(:)     = -4d0*v4(:) -v7(:) +2d0*v8(:) -2d0*v10b(:) +4d0*v11(:) -v21(:)
    R_FeOOH_PO4(:) = v7(:) -2d0*v10a(:)
    R_H2S(:) = 0.5d0*v5(:) -v10a(:) -v10b(:) -v12(:) -v13(:) -3d0*v15(:) -v16(:) +3d0*v19(:)
    R_S0(:)  =  v10a(:) +v10b(:) -v14(:) +v12(:) -4d0*v19(:) 
    R_SO4(:) = -0.5d0*v5(:) -v15(:) +v16(:) +v17(:) +4d0*v18(:) +v19(:)
# endif
    

! ***************************** !
!!!   Total Flux calculation
! ***************************** !
    !! Total Flux : (nmol cm-2 s-1)
    !! DBL Flux (Nsed = 0) (Only diffusion is considered. Deposition added in POM.)
    FT_O2(1)  = Fd_O2(1)
    FT_CO2(1) = Fd_CO2(1)
    FT_N2(1)  = Fd_N2(1)
# if defined ORGANIC_MATTER
    FT_DOC(1,1) = Fd_DOC(1,1)
    FT_DOC(1,2) = Fd_DOC(1,2)
    FT_POC(1,1) = Fdep_POC * OMFratio   ! Deposition of POCf
    FT_POC(1,2) = Fdep_POC * OMSratio   ! Deposition of POCs
    FT_POC(1,3) = Fdep_POC * OMNratio   ! Deposition of POCn
# endif
# if defined NUTRIENTS
    FT_NO3(1)  = Fd_NO3(1)
    FT_NH4(1)  = Fd_NH4(1)
    FT_PO4(1)  = Fd_PO4(1)
! #  if defined ORGANIC_MATTER
!     FT_DON(1) = Fd_DON(1)
!     FT_PON(1) = Fdep_PON   ! Deposition
!     FT_DOP(1) = Fd_DOP(1)
!     FT_POP(1) = Fdep_POP   ! Deposition
! #  endif
# endif
# if defined SULFATE
    FT_Mn2(1) = Fd_Mn2(1)
    FT_Fe2(1) = Fd_Fe2(1)
    FT_H2S(1) = Fd_H2S(1)
    FT_SO4(1) = Fd_SO4(1)

    !! if those sedimentations were included
    FT_MnO2(1) = Fdep_MnO2
    FT_FeOOH(1) = Fdep_FeOOH

# endif

    !! sediment column Flux (Nsed = 1:Nsed)
    !! -- with Burial -- !!
# if defined Burial
    DO k = 2,Nsed
        FT_O2(k)  = Fd_O2(k) + Fb_O2(k) + Fc_O2(k)
        FT_CO2(k) = Fd_CO2(k) + Fb_CO2(k) + Fc_CO2(k)
        FT_N2(k)  = Fd_N2(k) + Fb_N2(k) + Fc_N2(k)
#  if defined ORGANIC_MATTER
        FT_DOC(k,1) = Fd_DOC(k,1) + Fb_DOC(k,1) + Fc_DOC(k,1)
        FT_DOC(k,2) = Fd_DOC(k,2) + Fb_DOC(k,2) + Fc_DOC(k,2)
        FT_POC(k,1) = Fb_POC(k,1) + Fc_POC(k,1)
        FT_POC(k,2) = Fb_POC(k,2) + Fc_POC(k,2)
        FT_POC(k,3) = Fb_POC(k,3) + Fc_POC(k,3)
#  endif
#  if defined NUTRIENTS
        FT_NO3(k)  = Fd_NO3(k) + Fb_NO3(k) + Fc_NO3(k)
        FT_NH4(k)  = Fd_NH4(k) + Fb_NH4(k) + Fc_NH4(k)
        FT_PO4(k)  = Fd_PO4(k) + Fb_PO4(k) + Fc_PO4(k)
! #   if defined ORGANIC_MATTER
!         FT_DON(k) = Fd_DON(k) + Fb_DON(k) + Fc_DON(k)
!         FT_PON(k) = Fb_PON(k) + Fc_PON(k)
!         FT_DOP(k) = Fd_DOP(k) + Fb_DOP(k) + Fc_DOP(k)
!         FT_POP(k) = Fb_POP(k) + Fc_POP(k)
! #   endif
#  endif
#  if defined SULFATE
        FT_Mn2(k)  = Fd_Mn2(k) + Fb_Mn2(k) + Fc_Mn2(k)
        FT_MnO2(k) = Fb_MnO2(k) + Fc_MnO2(k)
        FT_Fe2(k)       = Fd_Fe2(k) + Fb_Fe2(k) + Fc_Fe2(k)
        FT_FeS(k)       = Fb_FeS(k) + Fc_FeS(k)
        FT_FeS2(k)      = Fb_FeS2(k) + Fc_FeS2(k) 
        FT_FeOOH(k)     = Fb_FeOOH(k) + Fc_FeOOH(k) 
        FT_FeOOH_PO4(k) = Fb_FeOOH_PO4(k) + Fc_FeOOH_PO4(k) 
        FT_H2S(k) = Fd_H2S(k) + Fb_H2S(k) + Fc_H2S(k) 
        FT_S0(k)  = Fb_S0(k) + Fc_S0(k) 
        FT_SO4(k) = Fd_SO4(k) + Fb_SO4(k) + Fc_SO4(k) 
#  endif
    ENDDO

# else
    !! -- without Burial -- !!
    DO k = 2,Nsed
        FT_O2(k)  = Fd_O2(k) + Fb_O2(k)
        FT_CO2(k) = Fd_CO2(k) + Fb_CO2(k)
        FT_N2(k)  = Fd_N2(k) + Fb_N2(k)
#  if defined ORGANIC_MATTER
        FT_DOC(k,1) = Fd_DOC(k,1) + Fb_DOC(k,1)
        FT_DOC(k,2) = Fd_DOC(k,2) + Fb_DOC(k,2)
        FT_POC(k,1) = Fb_POC(k,1)
        FT_POC(k,2) = Fb_POC(k,2)
        FT_POC(k,3) = Fb_POC(k,3)
#  endif
#  if defined NUTRIENTS
        FT_NO3(k)  = Fd_NO3(k) + Fb_NO3(k)
        FT_NH4(k)  = Fd_NH4(k) + Fb_NH4(k)
        FT_PO4(k)  = Fd_PO4(k) + Fb_PO4(k)
! #   if defined ORGANIC_MATTER
!         FT_DON(k) = Fd_DON(k) + Fb_DON(k)
!         FT_PON(k) = Fb_PON(k)
!         FT_DOP(k) = Fd_DOP(k) + Fb_DOP(k)
!         FT_POP(k) = Fb_POP(k)
! #   endif
#  endif
#  if defined SULFATE
        FT_Mn2(k)  = Fd_Mn2(k) + Fb_Mn2(k)
        FT_MnO2(k) = Fb_MnO2(k)
        FT_Fe2(k)       = Fd_Fe2(k) + Fb_Fe2(k)
        FT_FeS(k)       = Fb_FeS(k)
        FT_FeS2(k)      = Fb_FeS2(k)
        FT_FeOOH(k)     = Fb_FeOOH(k)
        FT_FeOOH_PO4(k) = Fb_FeOOH_PO4(k)
        FT_H2S(k) = Fd_H2S(k) + Fb_H2S(k)
        FT_S0(k)  = Fb_S0(k)
        FT_SO4(k) = Fd_SO4(k) + Fb_SO4(k)
#  endif
    ENDDO
# endif


! ***************************** !
!!!   Mass balance equations
! ***************************** !

    !! Mass Balance cal (Concentration at each layer in sed column)
    DO k = 1,Nsed-1
        ! -- O2 -- !
        SEDECO(ng)%O2(i,j,k)  = massbalance_sed_d(SEDECO(ng)%O2(i,j,k),FT_O2(k),FT_O2(k+1),R_O2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)
        ! -- CO2 -- !
        SEDECO(ng)%CO2(i,j,k) = massbalance_sed_d(SEDECO(ng)%CO2(i,j,k),FT_CO2(k),FT_CO2(k+1),R_CO2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)
        ! -- N2 -- !
        SEDECO(ng)%N2(i,j,k)  = massbalance_sed_d(SEDECO(ng)%N2(i,j,k),FT_N2(k),FT_N2(k+1),R_N2(k), & 
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)
# if defined ORGANIC_MATTER
        ! -- DOC -- !
        SEDECO(ng)%DOCf(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOCf(i,j,k),FT_DOC(k,1),FT_DOC(k+1,1),R_DOC(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)  ! DOCf
        SEDECO(ng)%DOCs(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOCs(i,j,k),FT_DOC(k,2),FT_DOC(k+1,2),R_DOC(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)  ! DOCs
        ! -- POC -- !
        SEDECO(ng)%POCf(i,j,k) = massbalance_sed_p(SEDECO(ng)%POCf(i,j,k),FT_POC(k,1),FT_POC(k+1,1),R_POC(k,1), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k))  ! POCf
        SEDECO(ng)%POCs(i,j,k) = massbalance_sed_p(SEDECO(ng)%POCs(i,j,k),FT_POC(k,2),FT_POC(k+1,2),R_POC(k,2), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k))  ! POCs
        SEDECO(ng)%POCn(i,j,k) = massbalance_sed_p(SEDECO(ng)%POCn(i,j,k),FT_POC(k,3),FT_POC(k+1,3),0d0, &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k))  ! POCs
# endif
# if defined NUTRIENTS
#  if defined adsorption
        ! -- NO3 -- !
        SEDECO(ng)%NO3(i,j,k) = massbalance_sed_d(SEDECO(ng)%NO3(i,j,k),FT_NO3(k),FT_NO3(k+1),R_NO3(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_NO3),dt) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(i,j,k) = massbalance_sed_d(SEDECO(ng)%NH4(i,j,k),FT_NH4(k),FT_NH4(k+1),R_NH4(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_NH4),dt) 
        ! -- PO4 -- !
        SEDECO(ng)%PO4(i,j,k) = massbalance_sed_d(SEDECO(ng)%PO4(i,j,k),FT_PO4(k),FT_PO4(k+1),R_PO4(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_PO4),dt)
#  else
            ! -- NO3 -- !
        SEDECO(ng)%NO3(i,j,k) = massbalance_sed_d(SEDECO(ng)%NO3(i,j,k),FT_NO3(k),FT_NO3(k+1),R_NO3(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt) 
        ! -- NH4 -- !
        SEDECO(ng)%NH4(i,j,k) = massbalance_sed_d(SEDECO(ng)%NH4(i,j,k),FT_NH4(k),FT_NH4(k+1),R_NH4(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt) 
        ! -- PO4 -- !
        SEDECO(ng)%PO4(i,j,k) = massbalance_sed_d(SEDECO(ng)%PO4(i,j,k),FT_PO4(k),FT_PO4(k+1),R_PO4(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)
#  endif
! #  if defined ORGANIC_MATTER
!         ! -- DON -- !
!         SEDECO(ng)%DON(i,j,k) = massbalance_sed_d(SEDECO(ng)%DON(i,j,k),FT_DON(k),FT_DON(k+1),R_DON(k), &
!                                                   dz(k),SEDECO(ng)%poro(i,j,k),dt) 
!         ! -- PON -- !
!         SEDECO(ng)%PON(i,j,k) = massbalance_sed_p(SEDECO(ng)%PON(i,j,k),FT_PON(k),FT_PON(k+1),R_PON(k), &
!                                                   dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k))
!         ! -- DOP -- !
!         SEDECO(ng)%DOP(i,j,k) = massbalance_sed_d(SEDECO(ng)%DOP(i,j,k),FT_DOP(k),FT_DOP(k+1),R_DOP(k), &
!                                                   dz(k),SEDECO(ng)%poro(i,j,k),dt) 
!         ! -- POP -- !
!         SEDECO(ng)%POP(i,j,k) = massbalance_sed_p(SEDECO(ng)%POP(i,j,k),FT_POP(k),FT_POP(k+1),R_POP(k), &
!                                                   dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
! #  endif
# endif
# if defined SULFATE
#  if defined adsorption
        ! -- Mn2 -- !
        SEDECO(ng)%Mn2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Mn2(i,j,k),FT_Mn2(k),FT_Mn2(k+1),R_Mn2(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_Mn2),dt) 
        ! -- Fe2 -- !
        SEDECO(ng)%Fe2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Fe2(i,j,k),FT_Fe2(k),FT_Fe2(k+1),R_Fe2(k), &
                                            dz(k),(SEDECO(ng)%poro(i,j,k)+rhos_d(k)*(1d0 - SEDECO(ng)%poro(i,j,k))*Kad_Fe2),dt) 
#  else
        ! -- Mn2 -- !
        SEDECO(ng)%Mn2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Mn2(i,j,k),FT_Mn2(k),FT_Mn2(k+1),R_Mn2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt) 
        ! -- Fe2 -- !
        SEDECO(ng)%Fe2(i,j,k) = massbalance_sed_d(SEDECO(ng)%Fe2(i,j,k),FT_Fe2(k),FT_Fe2(k+1),R_Fe2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt) 
#  endif
        ! -- MnO2 -- !
        SEDECO(ng)%MnO2(i,j,k) = massbalance_sed_p(SEDECO(ng)%MnO2(i,j,k),FT_MnO2(k),FT_MnO2(k+1),R_MnO2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
        ! -- FeS -- !
        SEDECO(ng)%FeS(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeS(i,j,k),FT_FeS(k),FT_FeS(k+1),R_FeS(k),  &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
        ! -- FeS2 -- !
        SEDECO(ng)%FeS2(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeS2(i,j,k),FT_FeS2(k),FT_FeS2(k+1),R_FeS2(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
        ! -- FeOOH -- !
        SEDECO(ng)%FeOOH(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeOOH(i,j,k),FT_FeOOH(k),FT_FeOOH(k+1),R_FeOOH(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
        ! -- FeOOH=PO4 -- !
        SEDECO(ng)%FeOOH_PO4(i,j,k) = massbalance_sed_p(SEDECO(ng)%FeOOH_PO4(i,j,k),FT_FeOOH_PO4(k),FT_FeOOH_PO4(k+1), & 
                                                  R_FeOOH_PO4(k),dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
        ! -- H2S -- !
        SEDECO(ng)%H2S(i,j,k) = massbalance_sed_d(SEDECO(ng)%H2S(i,j,k),FT_H2S(k),FT_H2S(k+1),R_H2S(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt) 
        ! -- S0 -- !
        SEDECO(ng)%S0(i,j,k) = massbalance_sed_p(SEDECO(ng)%S0(i,j,k),FT_S0(k),FT_S0(k+1),R_S0(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt,rhos_d(k)) 
        ! -- SO4 -- !
        SEDECO(ng)%SO4(i,j,k) = massbalance_sed_d(SEDECO(ng)%SO4(i,j,k),FT_SO4(k),FT_SO4(k+1),R_SO4(k), &
                                                  dz(k),SEDECO(ng)%poro(i,j,k),dt)
# endif
    ENDDO


! ***************************** !
!!!   set output parameters
! ***************************** !

!! Return total flux of water --> DBL
    ! Flux_Tmp = Fd_Tmp(0)
    ! Flux_Sal = Fd_Sal(0)
    Flux_DO  = FT_O2(1) 
    Flux_CO2sed = FT_CO2(1)
    Flux_N2  = FT_N2(1)
# if defined ORGANIC_MATTER
    Flux_DOCf = FT_DOC(1,1)
    Flux_DOCs = FT_DOC(1,2)
    Flux_POCf = FT_POC(1,1)
    Flux_POCs = FT_POC(1,2)
# endif
# if defined NUTRIENTS
    Flux_NO3 = FT_NO3(1)
    Flux_NH4 = FT_NH4(1)
    Flux_PO4 = FT_PO4(1)
! #  if defined ORGANIC_MATTER
!     Flux_DON = FT_DON(1)
!     Flux_PON = FT_PON(1)
!     Flux_DOP = FT_DOP(1)
!     Flux_POP = FT_POP(1)
! #  endif
# endif
# if defined SULFATE
    Flux_Mn2  = FT_Mn2(1)
    Flux_MnO2 = FT_MnO2(1)
    Flux_Fe2       = FT_Fe2(1)
    Flux_FeS       = FT_FeS(1)
    Flux_FeS2      = FT_FeS2(1)
    Flux_FeOOH     = FT_FeOOH(1)
    Flux_FeOOH_PO4 = FT_FeOOH_PO4(1)
    Flux_H2S = FT_H2S(1)
    Flux_S0  = FT_S0(1)
    Flux_SO4 = FT_SO4(1)
# endif

! ***************************** !
!!!  Print section
! ***************************** !
# if defined SEDIMENT_TESTMODE

    time = time +dt  ! sec
    year = time/86400d0/365d0 

    if(time.ge.dsec) then

        ! if(year.ge.(ymax-1.0d0)) then
            ! write(*,*) dsec, year

        write(58,*) time/86400.d0, O2amb, SEDECO(ng)%O2(i,j,:)
        write(80,*) time/86400.d0, CO2amb, SEDECO(ng)%CO2(i,j,:)
        write(81,*) time/86400.d0, N2amb, SEDECO(ng)%N2(i,j,:)

#  if defined ORGANIC_MATTER
        write(65,*) time/86400.d0, DOCfamb, SEDECO(ng)%DOCf(i,j,:)
        write(82,*) time/86400.d0, DOCsamb, SEDECO(ng)%DOCs(i,j,:)
        write(66,*) time/86400.d0, POCfamb, SEDECO(ng)%POCf(i,j,:)
        write(83,*) time/86400.d0, POCsamb, SEDECO(ng)%POCs(i,j,:)
        write(100,*) time/86400.d0, 0d0, SEDECO(ng)%POCn(i,j,:)
#  endif
#  if defined NUTRIENTS
        write(61,*) time/86400.d0, NH4amb, SEDECO(ng)%NH4(i,j,:)
        write(63,*) time/86400.d0, NO3amb, SEDECO(ng)%NO3(i,j,:)
        write(64,*) time/86400.d0, PO4amb, SEDECO(ng)%PO4(i,j,:)
    ! #   if defined ORGANIC_MATTER
    !       write(67,*) time/86400.d0, DONamb, SEDECO(ng)%DON(i,j,:)
    !       write(68,*) time/86400.d0, PONamb, SEDECO(ng)%PON(i,j,:)
    !       write(69,*) time/86400.d0, DOPamb, SEDECO(ng)%DOP(i,j,:)
    !       write(70,*) time/86400.d0, POPamb, SEDECO(ng)%POP(i,j,:)
    ! #   endif
#  endif
# if defined SULFATE
        write(84,*) time/86400.d0, Mn2amb, SEDECO(ng)%Mn2(i,j,:)
        write(85,*) time/86400.d0, 0d0,    SEDECO(ng)%MnO2(i,j,:)
        write(86,*) time/86400.d0, Fe2amb, SEDECO(ng)%Fe2(i,j,:)
        write(87,*) time/86400.d0, 0d0,    SEDECO(ng)%FeS(i,j,:)
        write(88,*) time/86400.d0, 0d0,    SEDECO(ng)%FeS2(i,j,:)
        write(89,*) time/86400.d0, 0d0,    SEDECO(ng)%FeOOH(i,j,:)
        write(90,*) time/86400.d0, 0d0,    SEDECO(ng)%FeOOH_PO4(i,j,:)
        write(91,*) time/86400.d0, H2Samb, SEDECO(ng)%H2S(i,j,:)
        write(93,*) time/86400.d0, 0d0,    SEDECO(ng)%S0(i,j,:)
        write(94,*) time/86400.d0, SO4amb, SEDECO(ng)%SO4(i,j,:)
# endif

            !! for debug !!
        !  write(96,'(*(g0:,", "))') time/86400.d0, Tamb, Samb, O2amb   &
        !                         ,Dw_O2(0), 0d0, Fd_O2(0), 0d0, 0d0, FT_O2(0), 0d0, 0d0
        !  write(97,'(*(g0:,", "))') time/86400.d0, SEDECO(ng)%Tmp(i,j,1), SEDECO(ng)%Sal(i,j,1), SEDECO(ng)%O2(i,j,1)    &
        !                         ,Dw_O2(1), Ds_O2(1), Fd_O2(1), Fb_O2(1), Fc_O2(1), FT_O2(1), v1(1), R_O2(1)

        ! write(96,'(*(g0:,", "))') time/86400.d0, SEDECO(ng)%Tmp(i,j,1), SEDECO(ng)%Sal(i,j,1), SEDECO(ng)%DOCf(i,j,1)    &
        !                         ,Dw_DOC(1,1), 0d0, Fd_DOC(1,1), 0d0, 0d0, FT_DOC(1,1), 0d0, 0d0
        ! write(97,'(*(g0:,", "))') time/86400.d0, SEDECO(ng)%Tmp(i,j,2), SEDECO(ng)%Sal(i,j,2), SEDECO(ng)%DOCf(i,j,2)    &
        !                         ,Dw_DOC(2,1), Ds_DOC(2,1), Fd_DOC(2,1), Fb_DOC(2,1), Fc_DOC(2,1), FT_DOC(2,1), vdom(2,1), R_DOC(2,1)

        ! write(97,'(*(g0:,", "))') time/86400.d0, vOM(10), v1(10),v2(10),v3(10),v4(10),v5(10)

        write(98,*) time/86400d0, redoxlevel(:) !! for debug

        write(99,*) dz(:), z(:), SEDECO(ng)%poro(i,j,:)
        write(101,*) time/86400.d0, v1(:)
        write(102,*) time/86400.d0, v2(:)
        write(103,*) time/86400.d0, v3(:)
        write(104,*) time/86400.d0, v4(:)
        write(105,*) time/86400.d0, v5(:)
        write(106,*) time/86400.d0, vOM(:)
        write(107,*) time/86400.d0, R_O2(:)   !! for debug
        write(108,*) time/86400.d0, Flux_DO   !! for debug

        ! endif
        write(*,*) 'Time (day): ', time/86400d0 

        dsec=dsec+OUTPUT_INTERVAL*60.
        !  time = time +dt  ! sec

    endif
# endif

    RETURN

  END SUBROUTINE sedecosys
END MODULE mod_sedecosys

#endif
