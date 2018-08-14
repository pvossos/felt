f = require 'felt'

function solve(flt)
   f.felt(flt)
   f.find_dofs()
   local mode = f.set_analysis_mode()
   
   if mode == "Transient" then
      K, M, C = f.construct_dynamic()
      dtable = f.integrate_hyperbolic(K, M, C)

   elseif mode == "TransientThermal" then
      f.analysis.dofs = {1}
      K, M = f.construct_dynamic()
      dtable = f.integrate_parabolic(K, M)
      
   elseif mode == "Static" then
      K = f.construct_stiffness()
      F = f.construct_forces()
      Kc, Fc = f.zero_constrained(K, F)
      d = f.solve_displacements(Kc, Fc)
      f.element_stresses()
      R = f.solve_reactions(K, d)

   elseif mode == "StaticLoadCases" or mode == "StaticLoadRange" then
      K = f.construct_stiffness()
      F = f.construct_forces()
      Kc, Fc = f.zero_constrained(K, F)
      if mode == "StaticLoadCases" then
         dtable = f.solve_static_load_cases(Kc, Fc)
      else
         dtable = f.solve_static_load_range(Kc, Fc)
      end

   elseif mode == "StaticSubstitutionLoadRange" or mode == "StaticIncrementalLoadRange" then
      K = f.create_nonlinear_stiffness()
      F = f.construct_forces()
      dtable = f.solve_nonlinear_load_range(K, F)
      
   elseif mode == "StaticSubstitution" or mode == "StaticIncremental" then
      K = f.construct_nonlinear_stiffness()
      F = f.construct_forces()
      d = f.static_nonlinear_displacements(K, F)
      
   elseif mode == "StaticThermal" then
      K = f.construct_stiffness()
      F = f.construct_forces()
      Kc, Fc = f.zero_constrained(K, F)
      d = f.solve_displacements(Kc, Fc)
      
   elseif mode == "Modal" then
      K, M, C = f.construct_dynamic()
      Kc, Mc, Cc = f.remove_constrained(K, M, C)
      l, r = f.compute_modes(Kc, Mc)
      r = f.normalize_by_first(r)
      
   elseif mode == "Spectral" then
      k, m, c = f.construct_dynamic()
      kc = f.zero_constrained(k)
      mc = f.zero_constrained(m)
      cc = f.zero_constrained(c)
      forced = f.find_forced_dof()
      H = f.compute_transfer_functions(mc, cc, kc, forced)
      S = f.compute_output_spectra(H, forced)

   end
   
end
   


