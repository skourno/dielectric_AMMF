MODULE Phys_consts
    Use nrtype
    !Frequently used mathematical constants (with precision to spare):
    REAL(DP), PARAMETER :: k_B         = 1.38064852D-23  ! Boltzman constant (SI units)
    REAL(DP), PARAMETER :: AvogNu      = 6.02214086D+23  ! Avogadro's number
    REAL(DP), PARAMETER :: perm0       = 8.85418782D-12  ! permittivity of free space (SI units)
    REAL(DP), PARAMETER :: perm0x4pi   = 1.11265006D-10  ! permittivity of free space times 4pi (SI units)

    REAL(DP), PARAMETER :: Debye_to_Cm = 3.33564D-30     ! convert Debye units to Coulomb times meter
    REAL(DP), PARAMETER :: Ang_to_m    = 1.D-10          ! convert Angstrom units to meters
END MODULE Phys_consts
