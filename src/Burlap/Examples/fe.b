function clear_nodes ( )
    global problem

    for n in problem.nodes do
	nodes (i).dx = zeros (1, 6)
	nodes (i).eq_force = zeros (1, 6)
    end
end


function construct_forces ( )
    global problem

    length = problem.num_dofs - 1
    indices = problem.dofs_num
    first_dof = problem.dofs_num (1)

    f = zeros (1, problem.num_dofs * problem.num_nodes)

    for n in problem.nodes do
	dof = global_dof (n, first_dof)
	force = n.eq_force (indices)
	if not null? (n.force) then
	    force = force + n.force.force (indices)
	end
	f (dof : dof + length) = f (dof: dof + length) + force
    end

    return f'
end


function find_dofs ( )
    global problem

    count = 0
    active = zeros (1, 6)
    positions = zeros (1, 6)

    for e in problem.elements do
	for dof in 1 : e.definition.num_dofs do
	    active (e.definition.dofs (dof)) = 1
	end
    end

    for dof in 1 : 6 do
	if active (dof) then
	    count = count + 1
	    positions (dof) = count
	end
    end

    problem.dofs_pos = positions
    return problem.num_dofs
end


function set_up (e, s)
    return e.definition.set_up (e, s)
end


function solve_displacements (K, f)
    global problem

    length = problem.num_dofs - 1
    indices = problem.dofs_num
    first_dof = problem.dofs_num (1)

    d = K \ f

    for n in nodes do
	dof = global_dof (n, first_dof)
	n.dx = zeros (1, 6)
	n.dx (indices) = d (dof : dof + length)
    end

    return d
end
