function truss_equivalent_forces (e, T, L)
    if e.num_loads == 1 and e.loads (1).nvalues == 2 then
	v1 = e.loads (1).value (1)
	v2 = e.loads (1).value (2)
	if v1.node == 1 and v2.node == 2 then
	    wa = v1.magnitude
	    wb = v2.magnitude
	else if v1.node == 2 and v2.node == 1 then
	    wa = v2.magnitude
	    wb = v1.magnitude
	end

	if fabs (wa) > fabs (wb) then
	    equiv = [wb * L/2 + (wa - wb) * L/3; wb * L/2 + (wa - wb) * L/6]
	else
	    equiv = [wa * L/2 + (wb - wa) * L/6; wa * L/2 + (wb - wa) * L/3]
	end

	equiv = T' * equiv
	e.node (1).eq_force (1:3) = e.node (1).eq_force (1:3) + equiv (1:3)'
	e.node (2).eq_force (1:3) = e.node (2).eq_force (1:3) + equiv (4:6)'
    end
end


function truss_set_up (e)
    L = length (e)
    AEonL = e.material.A * e.material.E / L
    K = [AEonL, -AEonL; -AEonL, AEonL]

    cx = (e.node (2).x - e.node (1).x) / L
    cy = (e.node (2).y - e.node (1).y) / L
    cz = (e.node (2).z - e.node (1).z) / L

    T = [cx, cy, cz, 0, 0, 0; 0, 0, 0, cx, cy, cz]
    e.K = T'*K*T

    truss_equivalent_forces (e, T, L)
    return 0
end


function truss_stress (e)
    L = length (e)
    EonL = e.material.E / L
    c = zeros (6, 1)

    c(1) = (e.node (2).x - e.node (1).x) / L
    c(2) = (e.node (2).y - e.node (1).y) / L
    c(3) = (e.node (2).z - e.node (1).z) / L

    stress = EonL * (e.node(2).dx - e.node(1).dx) * c

    e.ninteg = 1
    e.stress (1).x = (e.node(1).x + e.node(2).x) / 2
    e.stress (1).y = (e.node(1).y + e.node(2).y) / 2
    e.stress (1).values (1) = stress

    return 0
end
