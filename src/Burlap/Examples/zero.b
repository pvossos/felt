function ZeroRowCol ( )
    global K, nodes, dof_numbers, dof_positions

    for n in nodes do
	b = nd * (n.number - 1) + 1
	for d in dof_numbers do
	    if n.constraint.dx (d) then
		a = b + dof_positions (d) - 1
		K (:, a) = zeros (rols (K))
		K (a, :) = zeros (cols (K))
		K (a, a) = 1
	    end
	end
    end
end
