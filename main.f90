Program main
	Use nrtype
	Use phys_consts
	Use diel_models
	Implicit None

	Real(DP)            :: mu2, rho
	Real(DP)            :: diel, G, sigBH, temp, scale, y
	Integer(I4B)        :: i 


	rho    = 0.70     ! dimensionless
	temp   = 1.45
	mu2    = 0.00001  ! dimensionless with eps
	scale  = 1.30


    	OPEN(UNIT=23, FILE='./out.dat', STATUS='UNKNOWN', FORM='FORMATTED')
	
     	do while (mu2 <= 5)
!     	do while (rho < 1.0)
!    	do while (temp <= 2.51)
                y = (FOUR/9)*PI*rho*mu2*(1/temp)
		call compute_dielecric_SM(rho, mu2, temp, diel, G, sigBH, scale)
		write( *,111) temp, rho, mu2, sigBH, diel, G, y ! mu*mu output in epsilon units
		write(23,111) temp, rho, mu2, sigBH, diel, G, y ! mu*mu output in epsilon units
       		!temp = temp + 0.02
		mu2 = mu2 + 0.05 			
        enddo


111 FORMAT ( F10.2, 2X, F10.3, 2X, F14.6, 2X, F10.4, 2X, F10.2, 2X, E10.2, 2X, E16.6)

End program main
