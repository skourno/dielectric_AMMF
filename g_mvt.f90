!-----------------------------------------------------------------
!                        fluids.f90  -  description
!                     ------------------------------
!          begin    : Mon 02.10.2019
!          email    : sk1916@ic.ac.uk, skournopoulos@msn.com
!-----------------------------------------------------------------
!
! This module contains the necessary routines to invoke the first 
! mean value theorem of calculus to compute radial integrals over
! a SW or a Sutherland potential.
!
! SW  fluid : A. Gil-Villegas et al, J. Chem. Phys. 106, 4168 (1997).
! Mie fluid : T. Lafitte et al, J. Chem. Phys. 139, 154504 (2013)
!
!-----------------------------------------------------------------
!
! 10.08.2016      Created           Skou
! 19.08.2016      added LJ fluid    Skou
! 22.08.2016      added CP method   Skou
!
!-----------------------------------------------------------------

Module g_mvt 
    Use nrtype

contains

    !-----------------------------------------------------------------
    ! This subroutine can be called to calculate the g at contact at an 
    ! effective packing fraction "eta_eff". The mean value theorem of
    ! calculus is invoked to compute an integral of the SW or the 
    ! Sutherland potential
    !
    !
    ! eta : fluid packing fraction ( eta = rho * pi*sigma^3*(1/6) )
    ! IFl : 1-SW, 2-Sutherland
    ! r   : range_of_interaction lambda (dimensionless)    
    !
    !-----------------------------------------------------------------
    Function g_atContact(eta, IFl, r) 
        Implicit None
        Integer(I4B)          :: IFl
        Real(DP)              :: eta, r
        Real(DP)              :: g_atContact
        Real(DP)              :: eta_eff
        Real(DP), Allocatable :: c(:)
        Real(DP)              :: eta2, eta3, eta4, aux

        Intent(IN)            :: eta, IFl, r

        Allocate(c(4)) 

        Select Case (IFl)
            Case(1) ; STOP "ERROR - Function 'g_atContact' : SW fluid is not supported"
            Case(2) ; call Calculate_Constants_MVT (IFl, r, c)
        End Select

        ! powers of eta
        eta2        = eta*eta
        eta3        = eta*eta2
        eta4        = eta*eta3

        eta_eff     =  c(1)*eta + c(2)*eta2 + c(3)*eta3 + c(4)*eta4

        aux         =  (ONE - eta_eff)*(ONE - eta_eff)*(ONE - eta_eff)
        g_atContact =  (ONE - HALF*eta_eff) / aux

    End Function g_atContact

    !-----------------------------------------------------------------
    ! This subroutine can be called to calculate the mean value theorem 
    ! parametrization constant values for two different types of fluids.
    ! The references, where these two parametrizations are presented are:
    ! SW  fluid : A. Gil-Villegas et al, J. Chem. Phys. 106, 4168 (1997).
    ! Mie fluid : T. Lafitte et al, J. Chem. Phys. 139, 154504 (2013)
    !
    !
    ! IFl : 1-SW, 2-Sutherland
    ! r   : range_of_interaction lambda (dimensionless)
    ! c   : output array that will contain the constant values (should be properly allocated)
    !-----------------------------------------------------------------
    Subroutine Calculate_Constants_MVT (IFl, r, c)
        Implicit None
        Integer(I4B)          :: IFl
        Real(DP)              :: r
        Real(DP), Allocatable :: c(:)
        Intent(IN)            :: IFl, r
        Intent(INOUT)         :: c

        ! actual constant calculations happen below here
        Select Case (IFl)
            Case(1) ! Square-Well

                c(1) = +  2.25855d0 -  1.50349d0*r + 0.249434d0*r*r
                c(2) = -  0.66927d0 +  1.40049d0*r - 0.827739d0*r*r
                c(3) = + 10.15760d0 - 15.04270d0*r + 5.308270d0*r*r
                c(4) =   ZERO ! does not exist and not used

            Case(2) ! Lennard-Jones

                c(1) = + 0.81096d0 +  1.7888d0*(ONE/r) -  37.578d0*(ONE/r**2) +  92.284d0*(ONE/r**3)
                c(2) = + 1.02050d0 - 19.2410d0*(ONE/r) + 151.260d0*(ONE/r**2) - 463.500d0*(ONE/r**3)
                c(3) = - 1.90570d0 + 22.8450d0*(ONE/r) - 228.140d0*(ONE/r**2) + 973.920d0*(ONE/r**3)
                c(4) = + 1.08850d0 -  6.1962d0*(ONE/r) + 106.980d0*(ONE/r**2) - 677.640d0*(ONE/r**3)

            Case Default ; STOP 'Calculate_Constants: ERROR - wrong IFluid #'
        End Select
    End Subroutine Calculate_Constants_MVT

End Module