find_dofs ( )

if problem.mode == &static then
    K = assemble ( )
    F = construct_forces ( )
    Kc = zero_constrained (K)
    Fc = zero_constrained (F)
    d = solve_displacements (Kc, Fc)

else if problem.mode == &transient then
    K = assemble (M, C)
    D = integrate_hyperbolic (K, M, C)

else if problem.mode == &modal then
    K = assemble (M, C)
    Kc = remove_constrained (K)
    Mc = remove_constrained (M)
    Cc = remove_constrained (C)
    l = compute_modes (K, M, X)

else if problem.mode == &static_thermal then
    K = assemble ( )
    F = construct_forces ( )
    Kc = zero_constrained (K)
    Fc = zero_constrained (F)
    d = solve_displacements (Kc, Fc)

else if problem.mode == &transient_thermal then
    K = assemble (M)
    D = integrate_parabolic (K, M)
end
